//go:build amd64 && !noasm

#include "textflag.h"

// Data section with constants for YCbCr -> RGB conversion.
// These are the standard JFIF conversion formula constants, scaled by 256 for 24.8 fixed-point integer arithmetic.

// Constants for VPMULLD must be DWords (4 bytes).
DATA ·c_359<>+0(SB)/4, $359       // R from Cr (1.402 * 256)
DATA ·c_454<>+0(SB)/4, $454       // B from Cb (1.772 * 256)
DATA ·c_88<>+0(SB)/4, $88         // G from Cb (0.344136 * 256)
DATA ·c_183<>+0(SB)/4, $183       // G from Cr (0.714136 * 256)

// Constants for biasing and rounding.
DATA ·c_128_w<>+0(SB)/2, $128     // Bias for Cb/Cr (-128) as 16-bit words for packed subtraction.
DATA ·c_128_d<>+0(SB)/4, $128     // Rounding bias (+128) as 32-bit dwords before shifting.

GLOBL ·c_359<>(SB), (RODATA + NOPTR), $4
GLOBL ·c_454<>(SB), (RODATA + NOPTR), $4
GLOBL ·c_88<>(SB), (RODATA + NOPTR), $4
GLOBL ·c_183<>(SB), (RODATA + NOPTR), $4
GLOBL ·c_128_w<>(SB), (RODATA + NOPTR), $2
GLOBL ·c_128_d<>(SB), (RODATA + NOPTR), $4

// func ycbcrToRGBAAVX2(dst, y, cb, cr []byte)
// AVX2 implementation of YCbCr to RGBA conversion. Processes 16 pixels per iteration.
TEXT ·ycbcrToRGBAAVX2(SB), NOSPLIT, $0-96
	// Load arguments (pointers and length)
	MOVQ  dst_base+0(FP), DI
	MOVQ  y_base+24(FP), R8
	MOVQ  cb_base+48(FP), R9
	MOVQ  cr_base+72(FP), R10
	MOVQ  y_len+32(FP), CX // Loop counter (number of pixels)

	// Broadcast constants. Y7-Y12 and Y15 must be preserved across the loop (safe from VEX zeroing).
	VPBROADCASTD ·c_359<>(SB), Y7       // 359 (R)
	VPBROADCASTD ·c_454<>(SB), Y8       // 454 (B)
	VPBROADCASTD ·c_88<>(SB),  Y9       // 88 (G)
	VPBROADCASTD ·c_183<>(SB), Y10      // 183 (G)
	VPBROADCASTW ·c_128_w<>(SB), Y11    // 128w (Bias)
	VPBROADCASTD ·c_128_d<>(SB), Y12    // 128d (Round)
	VPCMPEQB Y15, Y15, Y15              // Alpha mask (0xFF)

loop_ycbcr:
	// Check if at least 16 pixels remaining.
	CMPQ CX, $16
	JL   done_ycbcr

	// Load 16 bytes of Y, Cb, Cr. X0-X2 are temporary holders.
	VMOVDQU (R8),  X0 // Y
	VMOVDQU (R9),  X1 // Cb
	VMOVDQU (R10), X2 // Cr

	// Unpack bytes to words (16-bit). Y3=Y, Y4=Cb, Y5=Cr.
	VPMOVZXBW X0, Y3
	VPMOVZXBW X1, Y4
	VPMOVZXBW X2, Y5

	// Subtract bias (128). Cb' = Cb - 128, Cr' = Cr - 128.
	VPSUBW Y11, Y4, Y4
	VPSUBW Y11, Y5, Y5

	// --- Setup inputs for 32-bit arithmetic ---

	// Unpack Y to dwords and calculate Y' = Y << 8.
    // Use X0, X1 as temporary destinations for VEXTRACTF128 (safe registers).
	VEXTRACTF128 $0, Y3, X0; VPMOVZXWD X0, Y0 // Y'_low (P0-7)
	VEXTRACTF128 $1, Y3, X1; VPMOVZXWD X1, Y1 // Y'_high (P8-15)
	VPSLLD $8, Y0, Y0
	VPSLLD $8, Y1, Y1

	// Unpack Cb', Cr' to dwords (sign extend).
    // Use X2-X5 as temporary destinations.
	VEXTRACTF128 $0, Y4, X2; VPMOVSXWD X2, Y2 // Cb'_low
	VEXTRACTF128 $1, Y4, X3; VPMOVSXWD X3, Y3 // Cb'_high
	VEXTRACTF128 $0, Y5, X4; VPMOVSXWD X4, Y4 // Cr'_low
	VEXTRACTF128 $1, Y5, X5; VPMOVSXWD X5, Y5 // Cr'_high

    // Inputs: Y'(Y0/Y1), Cb'(Y2/Y3), Cr'(Y4/Y5).
    // Temps/Accumulators: Y6, Y13, Y14.

	// --- Compute G = (Y' - 88*Cb' - 183*Cr' + 128) >> 8 ---
    // G_low (Y13), G_high (Y14). Temp Y6.
	VMOVDQA  Y0, Y13
	VPMULLD  Y9, Y2, Y6; VPSUBD Y6, Y13, Y13 // G_low -= 88*Cb'
	VPMULLD  Y10, Y4, Y6; VPSUBD Y6, Y13, Y13 // G_low -= 183*Cr'

	VMOVDQA  Y1, Y14
	VPMULLD  Y9, Y3, Y6; VPSUBD Y6, Y14, Y14 // G_high -= 88*Cb'
	VPMULLD  Y10, Y5, Y6; VPSUBD Y6, Y14, Y14 // G_high -= 183*Cr'

	// Rounding (+128) and shifting (>> 8).
	VPADDD   Y12, Y13, Y13; VPSRAD $8, Y13, Y13
	VPADDD   Y12, Y14, Y14; VPSRAD $8, Y14, Y14

	// Pack G dwords to words (signed saturation). Results are scrambled due to in-lane packing.
	VPACKSSDW Y14, Y13, Y13 // G words in Y13. Y14 is free.

	// --- Compute R = (Y' + 359*Cr' + 128) >> 8 ---
    // R_low (Y14), R_high (Y6).
	VPMULLD  Y7, Y4, Y14    // R_low = 359*Cr'_low
	VPADDD   Y0, Y14, Y14   // R_low += Y'_low

	VPMULLD  Y7, Y5, Y6     // R_high = 359*Cr'_high
	VPADDD   Y1, Y6, Y6     // R_high += Y'_high

	// Rounding and shifting.
	VPADDD   Y12, Y14, Y14; VPSRAD $8, Y14, Y14
	VPADDD   Y12, Y6, Y6;   VPSRAD $8, Y6, Y6

	// Pack R dwords to words (scrambled).
	VPACKSSDW Y6, Y14, Y14 // R words in Y14. Y6 is free.

	// --- Compute B = (Y' + 454*Cb' + 128) >> 8 ---
    // B_low (Y6), B_high (Y4). Cr' registers (Y4/Y5) are free.
	VPMULLD  Y8, Y2, Y6     // B_low = 454*Cb'_low
	VPADDD   Y0, Y6, Y6     // B_low += Y'_low

	VPMULLD  Y8, Y3, Y4     // B_high = 454*Cb'_high (reusing Y4)
	VPADDD   Y1, Y4, Y4     // B_high += Y'_high

	// Rounding and shifting.
	VPADDD   Y12, Y6, Y6;   VPSRAD $8, Y6, Y6
	VPADDD   Y12, Y4, Y4;   VPSRAD $8, Y4, Y4

	// Pack B dwords to words (scrambled).
	VPACKSSDW Y4, Y6, Y6 // B words in Y6. Y4 is free.

    // G(Y13), R(Y14), B(Y6) are packed words, scrambled.

    // Unscramble using VPERMQ. Mask 0xD8 (11011000) reorders the quadwords correctly.
    VPERMQ $0xD8, Y13, Y13 // G words unscrambled.
    VPERMQ $0xD8, Y14, Y14 // R words unscrambled.
    VPERMQ $0xD8, Y6, Y6   // B words unscrambled.

    // --- Packing words to bytes (saturation clamping) ---
    // Use safe registers Y0-Y5 for destinations to avoid VEX zeroing constants (Y7-Y12, Y15).

    // Pack G. Use X0 (temp) and X1 (dest).
    // Extract high lane, then pack low lane (X13) and high lane (X0) using 128-bit VPACKUSWB.
    VEXTRACTF128 $1, Y13, X0; VPACKUSWB X0, X13, X1 // G bytes -> X1.

    // Pack R. Use X2 (temp) and X3 (dest).
    VEXTRACTF128 $1, Y14, X2; VPACKUSWB X2, X14, X3 // R bytes -> X3.

    // Pack B. Use X4 (temp) and X5 (dest).
    VEXTRACTF128 $1, Y6, X4; VPACKUSWB X4, X6, X5 // B bytes -> X5.

    // --- Interleaving RGBA ---
    // R=X3, G=X1, B=X5. A=X15.
    // Use safe registers Y0, Y2, Y4, Y6, Y13, Y14 for intermediates.

    // Prepare Alpha A=X0.
    VMOVDQA X15, X0

    // Interleave R and G. Go asm: S2, S1, D -> D=Interleave(S1, S2).
	VPUNPCKLBW X1, X3, X2 // RG low -> X2.
	VPUNPCKHBW X1, X3, X4 // RG high -> X4.

	// Interleave B and A.
	VPUNPCKLBW X0, X5, X6 // BA low -> X6.
	VPUNPCKHBW X0, X5, X13 // BA high -> X13.

    // Interleave RG and BA words. Store results in X0-X3.
	VPUNPCKLWD X6, X2, X0 // RGBA 0-3 -> X0.
	VPUNPCKHWD X6, X2, X1 // RGBA 4-7 -> X1.
	VPUNPCKLWD X13, X4, X2 // RGBA 8-11 -> X2.
	VPUNPCKHWD X13, X4, X3 // RGBA 12-15 -> X3.

	// Store 64 bytes (16 pixels).
	VMOVDQU X0, (DI)
	VMOVDQU X1, 16(DI)
	VMOVDQU X2, 32(DI)
	VMOVDQU X3, 48(DI)

	// Advance pointers and decrement counter.
	ADDQ $16, R8; ADDQ $16, R9; ADDQ $16, R10
	ADDQ $64, DI
	SUBQ $16, CX
	JMP  loop_ycbcr

done_ycbcr:
    // Clear upper bits of YMM registers before returning to Go.
	VZEROUPPER
	RET

// func rgbToRGBAAVX2(dst, r, g, b []byte)
TEXT ·rgbToRGBAAVX2(SB), NOSPLIT, $0-96
	MOVQ dst_base+0(FP), DI
	MOVQ r_base+24(FP), R8
	MOVQ g_base+48(FP), R9
	MOVQ b_base+72(FP), R10
	MOVQ r_len+32(FP), CX

	VPCMPEQB Y15, Y15, Y15 // Alpha mask (0xFF)

rgb_to_rgba_loop:
    // Process 32 pixels per iteration.
	CMPQ CX, $32
	JL   done_rgb

    // Load 32 bytes of R, G, B.
	VMOVDQU (R8),  Y0 // R
	VMOVDQU (R9),  Y1 // G
	VMOVDQU (R10), Y2 // B
	VMOVDQA Y15,   Y3 // A

    // Interleave R and G (in-lane).
	VPUNPCKLBW Y1, Y0, Y4
	VPUNPCKHBW Y1, Y0, Y5
    // Interleave B and A (in-lane).
	VPUNPCKLBW Y3, Y2, Y6
	VPUNPCKHBW Y3, Y2, Y7

    // Interleave RG and BA words (in-lane).
	VPUNPCKLWD Y6, Y4, Y8
	VPUNPCKHWD Y6, Y4, Y9
	VPUNPCKLWD Y7, Y5, Y10
	VPUNPCKHWD Y7, Y5, Y11

    // Permute 128-bit lanes to arrange pixels contiguously.
    // VPERM2F128 Imm, Src2, Src1, Dest.
    // 0x20 selects Lane 0 from Src1 and Lane 0 from Src2.
    // 0x31 selects Lane 1 from Src1 and Lane 1 from Src2.
	VPERM2F128 $0x20, Y9, Y8, Y12   // Pixels 0-7
	VPERM2F128 $0x20, Y11, Y10, Y13 // Pixels 8-15
	VPERM2F128 $0x31, Y9, Y8, Y14   // Pixels 16-23
	VPERM2F128 $0x31, Y11, Y10, Y3  // Pixels 24-31 (reusing Y3)

    // Store 128 bytes (32 pixels).
	VMOVDQU Y12, (DI)
	VMOVDQU Y13, 32(DI)
	VMOVDQU Y14, 64(DI)
	VMOVDQU Y3, 96(DI)

	ADDQ $32, R8; ADDQ $32, R9; ADDQ $32, R10
	ADDQ $128, DI
	SUBQ $32, CX
	JMP  rgb_to_rgba_loop

done_rgb:
	VZEROUPPER
	RET

// func grayToRGBAAVX2(dst, gray []byte)
TEXT ·grayToRGBAAVX2(SB), NOSPLIT, $0-48
	MOVQ dst_base+0(FP), DI
	MOVQ gray_base+24(FP), R8
	MOVQ gray_len+32(FP), CX

	VPCMPEQB Y15, Y15, Y15 // Alpha mask (0xFF)

gray_to_rgba_loop:
    // Process 32 pixels per iteration.
	CMPQ CX, $32
	JL   done_gray

    // Load 32 bytes of Gray (Y).
	VMOVDQU (R8), Y0 // Y
	VMOVDQA Y15, Y3  // A

    // Interleave Y and Y (R=Y, G=Y).
	VPUNPCKLBW Y0, Y0, Y4
	VPUNPCKHBW Y0, Y0, Y5
    // Interleave Y and A (B=Y, A=255).
	VPUNPCKLBW Y3, Y0, Y6
	VPUNPCKHBW Y3, Y0, Y7

    // Interleave RG and BA words.
	VPUNPCKLWD Y6, Y4, Y8
	VPUNPCKHWD Y6, Y4, Y9
	VPUNPCKLWD Y7, Y5, Y10
	VPUNPCKHWD Y7, Y5, Y11

    // Permute lanes (same logic as rgbToRGBA).
	VPERM2F128 $0x20, Y9, Y8, Y12
	VPERM2F128 $0x20, Y11, Y10, Y13
	VPERM2F128 $0x31, Y9, Y8, Y14
	VPERM2F128 $0x31, Y11, Y10, Y3

    // Store 128 bytes (32 pixels).
	VMOVDQU Y12, (DI)
	VMOVDQU Y13, 32(DI)
	VMOVDQU Y14, 64(DI)
	VMOVDQU Y3, 96(DI)

	ADDQ $32, R8
	ADDQ $128, DI
	SUBQ $32, CX
	JMP  gray_to_rgba_loop

done_gray:
	VZEROUPPER
	RET
