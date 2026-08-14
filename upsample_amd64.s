//go:build amd64 && !noasm

#include "textflag.h"

// upsampleNearestNeighborAVX2 performs a 2x2 nearest neighbor upsampling using AVX2.
TEXT ·upsampleNearestNeighborAVX2(SB), NOSPLIT, $0-48
	MOVQ src+0(FP), SI
	MOVQ dst+8(FP), DI
	MOVQ srcW+16(FP), R8
	MOVQ srcH+24(FP), R9
	MOVQ srcS+32(FP), R10
	MOVQ dstS+40(FP), R11

y_loop:
	MOVQ SI, R13
	MOVQ DI, R14
	MOVQ R8, CX

x_loop:
	CMPQ CX, $16
	JL   x_rem

	VMOVDQU (R13), X0

	// Horizontal expansion: Duplicate bytes by interleaving X0 with itself.
	// This efficiently doubles the pixels for a 2x upscale.
	VPUNPCKLBW X0, X0, X1
	VPUNPCKHBW X0, X0, X2

	VMOVDQU X1, (R14)
	VMOVDQU X2, 16(R14)

	ADDQ $16, R13
	ADDQ $32, R14
	SUBQ $16, CX
	JMP  x_loop

x_rem:
	TESTQ CX, CX
	JZ    row_done

x_rem_loop:
	MOVB (R13), AL
	MOVB AL, (R14)
	MOVB AL, 1(R14)
	INCQ R13
	ADDQ $2, R14
	DECQ CX
	JNZ  x_rem_loop

row_done:
	// Vertical expansion: Copy the expanded row to the next line.
	MOVQ DI, R13
	MOVQ DI, R14
	ADDQ R11, R14
	MOVQ R8, CX
	SHLQ $1, CX   // CX = Destination width

copy_loop:
	CMPQ CX, $32
	JL   copy_rem

	VMOVDQU (R13), Y0
	VMOVDQU Y0, (R14)

	ADDQ $32, R13
	ADDQ $32, R14
	SUBQ $32, CX
	JMP  copy_loop

copy_rem:
	TESTQ CX, CX
	JZ    after_copy

copy_rem_loop:
	MOVB (R13), AL
	MOVB AL, (R14)
	INCQ R13
	INCQ R14
	DECQ CX
	JNZ  copy_rem_loop

after_copy:
	ADDQ R10, SI
	ADDQ R11, DI
	ADDQ R11, DI

	DECQ R9
	JNZ  y_loop

	VZEROUPPER
	RET

// Coefficients for the 4-tap filter: A=-9, B=111, C=29, D=-3.
// These are arranged as 16-bit signed words for use with VPMADDWD, which
// computes (p_i * coeff_i) + (p_{i+1} * coeff_{i+1}). The memory layout is little-endian.

// For O1: A*p0 + B*p1. Coeffs: [A, B]. Hex: 0x006FFFF7.
DATA ·const_catmull_rom_AB+0(SB)/8, $0x006FFFF7006FFFF7
DATA ·const_catmull_rom_AB+8(SB)/8, $0x006FFFF7006FFFF7
DATA ·const_catmull_rom_AB+16(SB)/8, $0x006FFFF7006FFFF7
DATA ·const_catmull_rom_AB+24(SB)/8, $0x006FFFF7006FFFF7
GLOBL ·const_catmull_rom_AB(SB), RODATA, $32

// For O1: C*p2 + D*p3. Coeffs: [C, D]. Hex: 0xFFFD001D.
DATA ·const_catmull_rom_CD+0(SB)/8, $0xFFFD001DFFFD001D
DATA ·const_catmull_rom_CD+8(SB)/8, $0xFFFD001DFFFD001D
DATA ·const_catmull_rom_CD+16(SB)/8, $0xFFFD001DFFFD001D
DATA ·const_catmull_rom_CD+24(SB)/8, $0xFFFD001DFFFD001D
GLOBL ·const_catmull_rom_CD(SB), RODATA, $32

// For O2: D*p0 + C*p1. Coeffs: [D, C]. Hex: 0x001DFFFD.
DATA ·const_catmull_rom_DC+0(SB)/8, $0x001DFFFD001DFFFD
DATA ·const_catmull_rom_DC+8(SB)/8, $0x001DFFFD001DFFFD
DATA ·const_catmull_rom_DC+16(SB)/8, $0x001DFFFD001DFFFD
DATA ·const_catmull_rom_DC+24(SB)/8, $0x001DFFFD001DFFFD
GLOBL ·const_catmull_rom_DC(SB), RODATA, $32

// For O2: B*p2 + A*p3. Coeffs: [B, A]. Hex: 0xFFF7006F.
DATA ·const_catmull_rom_BA+0(SB)/8, $0xFFF7006FFFF7006F
DATA ·const_catmull_rom_BA+8(SB)/8, $0xFFF7006FFFF7006F
DATA ·const_catmull_rom_BA+16(SB)/8, $0xFFF7006FFFF7006F
DATA ·const_catmull_rom_BA+24(SB)/8, $0xFFF7006FFFF7006F
GLOBL ·const_catmull_rom_BA(SB), RODATA, $32

// Constant vector of 32-bit dwords, all set to 64, for rounding ((sum + 64) >> 7).
DATA ·const_64+0(SB)/8, $0x0000004000000040
DATA ·const_64+8(SB)/8, $0x0000004000000040
DATA ·const_64+16(SB)/8, $0x0000004000000040
DATA ·const_64+24(SB)/8, $0x0000004000000040
GLOBL ·const_64(SB), RODATA, $32

// func upsampleHAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)
// Performs 2x horizontal Catmull-Rom upsampling using AVX2.
TEXT ·upsampleHAVX2(SB), NOSPLIT, $0-48
	MOVQ dst+0(FP), DI
	MOVQ src+8(FP), SI
	MOVQ w+16(FP), R8
	MOVQ h+24(FP), R9
	MOVQ dstStride+32(FP), R10
	MOVQ srcStride+40(FP), R11

	// Load constants into YMM registers for the main loop.
	VMOVDQU ·const_catmull_rom_AB(SB), Y13 // Y13: [A, B] for p0, p1
	VMOVDQU ·const_catmull_rom_CD(SB), Y12 // Y12: [C, D] for p2, p3
	VMOVDQU ·const_catmull_rom_DC(SB), Y11 // Y11: [D, C] for p0, p1
	VMOVDQU ·const_catmull_rom_BA(SB), Y10 // Y10: [B, A] for p2, p3
	VMOVDQU ·const_64(SB), Y14             // Y14: Rounding constant 64

y_loop_h:
	MOVQ DI, R12 // R12: Dst row pointer
	MOVQ SI, R13 // R13: Src row pointer

	// Handle the first 3 output pixels using special boundary condition coefficients from the pure Go implementation.
	XORQ R14, R14; MOVB 0(R13), R14B
	XORQ R15, R15; MOVB 1(R13), R15B
	XORQ AX, AX; MOVB 2(R13), AL

	// out[0] = cf(139*p0 - 11*p1)
	MOVQ R14, DX; IMULQ $139, DX
	MOVQ R15, BX; IMULQ $-11, BX; ADDQ BX, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE clip0_h; XORQ DX, DX

clip0_h:
	CMPQ DX, $255; JLE clip1_h; MOVQ $255, DX

clip1_h:
	MOVB DL, 0(R12)

	// out[1] = cf(104*p0 + 27*p1 - 3*p2)
	MOVQ R14, DX; IMULQ $104, DX
	MOVQ R15, BX; IMULQ $27, BX; ADDQ BX, DX
	MOVQ AX, BX; IMULQ $-3, BX; ADDQ BX, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE clip2_h; XORQ DX, DX

clip2_h:
	CMPQ DX, $255; JLE clip3_h; MOVQ $255, DX

clip3_h:
	MOVB DL, 1(R12)

	// out[2] = cf(28*p0 + 109*p1 - 9*p2)
	MOVQ R14, DX; IMULQ $28, DX
	MOVQ R15, BX; IMULQ $109, BX; ADDQ BX, DX
	MOVQ AX, BX; IMULQ $-9, BX; ADDQ BX, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE clip4_h; XORQ DX, DX

clip4_h:
	CMPQ DX, $255; JLE clip5_h; MOVQ $255, DX

clip5_h:
	MOVB DL, 2(R12)

	// Set up pointers for the vectorized main loop.
	ADDQ $3, R12             // Dst pointer starts at index 3.
	MOVQ R8, CX; SUBQ $3, CX // Loop counter = W-3

x_loop_h:
	CMPQ CX, $16
	JL   remainder_h

	// Load 16 pixels from 4 overlapping positions to get p0, p1, p2, p3 for each operation.
	VMOVDQU 0(R13), X0
	VMOVDQU 1(R13), X1
	VMOVDQU 2(R13), X2
	VMOVDQU 3(R13), X3

	// Unpack 8-bit pixels into 16-bit words for arithmetic.
	VPMOVZXBW X0, Y0; VPMOVZXBW X1, Y1; VPMOVZXBW X2, Y2; VPMOVZXBW X3, Y3

	// Interleave pixels to create pairs [p0, p1] and [p2, p3] for VPMADDWD.
	VPUNPCKLWD Y1, Y0, Y4; VPUNPCKHWD Y1, Y0, Y5 // Y4/Y5 = [p0, p1] pairs
	VPUNPCKLWD Y3, Y2, Y6; VPUNPCKHWD Y3, Y2, Y7 // Y6/Y7 = [p2, p3] pairs

	// Calculate O1 = cf(A*p0 + B*p1 + C*p2 + D*p3)
	// Multiply-add pairs of 16-bit words, producing 32-bit dword results.
	VPMADDWD Y13, Y4, Y8; VPMADDWD Y13, Y5, Y9 // (A*p0 + B*p1)
	VPMADDWD Y12, Y6, Y2; VPMADDWD Y12, Y7, Y3 // (C*p2 + D*p3)
	VPADDD   Y2, Y8, Y8; VPADDD Y3, Y9, Y9     // Sum into O1 results

	// Calculate O2 = cf(D*p0 + C*p1 + B*p2 + A*p3)
	VPMADDWD Y11, Y4, Y4; VPMADDWD Y11, Y5, Y5 // (D*p0 + C*p1)
	VPMADDWD Y10, Y6, Y6; VPMADDWD Y10, Y7, Y7 // (B*p2 + A*p3)
	VPADDD   Y6, Y4, Y4; VPADDD Y7, Y5, Y5     // Sum into O2 results

	// Finalize: round, shift, pack, and store
	// Add 64 for rounding, then shift right by 7.
	VPADDD Y14, Y8, Y8; VPADDD Y14, Y9, Y9; VPADDD Y14, Y4, Y4; VPADDD Y14, Y5, Y5
	VPSRAD $7, Y8, Y8; VPSRAD $7, Y9, Y9; VPSRAD $7, Y4, Y4; VPSRAD $7, Y5, Y5

	// Pack 32-bit dwords to 16-bit signed words.
	VPACKSSDW Y9, Y8, Y8 // O1 results
	VPACKSSDW Y5, Y4, Y4 // O2 results

	// Interleave O1 and O2 results to get the final output order: [O1, O2, O1, O2...].
	VPUNPCKLWD Y4, Y8, Y0; VPUNPCKHWD Y4, Y8, Y1

	// Pack 16-bit signed words to 8-bit unsigned bytes with saturation (clipping to 0-255).
	VPACKUSWB Y1, Y0, Y0
	VMOVDQU   Y0, (R12)  // Store 32 resulting bytes.

	// Advance pointers and counter for the next iteration.
	ADDQ $16, R13; ADDQ $32, R12; SUBQ $16, CX
	JMP  x_loop_h

remainder_h:
	// Process any remaining pixels that didn't fit into a 16-pixel AVX block.
rem_loop_h:
	CMPQ CX, $0
	JLE  done_row_h

	XORQ AX, AX; MOVB 0(R13), AL
	XORQ BX, BX; MOVB 1(R13), BL
	XORQ R14, R14; MOVB 2(R13), R14B
	XORQ R15, R15; MOVB 3(R13), R15B

	// Calculate O1 = cf(A*p0 + B*p1 + C*p2 + D*p3)
	MOVQ AX, DX; IMULQ $-9, DX
	MOVQ BX, BP; IMULQ $111, BP; ADDQ BP, DX
	MOVQ R14, BP; IMULQ $29, BP; ADDQ BP, DX
	MOVQ R15, BP; IMULQ $-3, BP; ADDQ BP, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE clip6_h; XORQ DX, DX

clip6_h:
	CMPQ DX, $255; JLE clip7_h; MOVQ $255, DX

clip7_h:
	MOVB DL, 0(R12)

	// Calculate O2 = cf(D*p0 + C*p1 + B*p2 + A*p3)
	MOVQ AX, DX; IMULQ $-3, DX
	MOVQ BX, BP; IMULQ $29, BP; ADDQ BP, DX
	MOVQ R14, BP; IMULQ $111, BP; ADDQ BP, DX
	MOVQ R15, BP; IMULQ $-9, BP; ADDQ BP, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE clip8_h; XORQ DX, DX

clip8_h:
	CMPQ DX, $255; JLE clip9_h; MOVQ $255, DX

clip9_h:
	MOVB DL, 1(R12)

	ADDQ $1, R13
	ADDQ $2, R12
	DECQ CX
	JMP  rem_loop_h

done_row_h:
	// Handle the last 3 output pixels using mirrored boundary conditions.
	XORQ DX, DX; MOVB 2(R13), DL
	XORQ BX, BX; MOVB 1(R13), BL
	XORQ R14, R14; MOVB 0(R13), R14B

	// out[2W-3] (mirrors out[2])
	MOVQ DX, R15; IMULQ $28, R15
	MOVQ BX, AX; IMULQ $109, AX; ADDQ AX, R15
	MOVQ R14, AX; IMULQ $-9, AX; ADDQ AX, R15
	ADDQ $64, R15; SARQ $7, R15
	CMPQ R15, $0; JGE clipA_h; XORQ R15, R15

clipA_h:
	CMPQ R15, $255; JLE clipB_h; MOVQ $255, R15

clipB_h:
	MOVB R15B, 0(R12)

	// out[2W-2] (mirrors out[1])
	MOVQ DX, R15; IMULQ $104, R15
	MOVQ BX, AX; IMULQ $27, AX; ADDQ AX, R15
	MOVQ R14, AX; IMULQ $-3, AX; ADDQ AX, R15
	ADDQ $64, R15; SARQ $7, R15
	CMPQ R15, $0; JGE clipC_h; XORQ R15, R15

clipC_h:
	CMPQ R15, $255; JLE clipD_h; MOVQ $255, R15

clipD_h:
	MOVB R15B, 1(R12)

	// out[2W-1] (mirrors out[0])
	MOVQ DX, R15; IMULQ $139, R15
	MOVQ BX, AX; IMULQ $-11, AX; ADDQ AX, R15
	ADDQ $64, R15; SARQ $7, R15
	CMPQ R15, $0; JGE clipE_h; XORQ R15, R15

clipE_h:
	CMPQ R15, $255; JLE clipF_h; MOVQ $255, R15

clipF_h:
	MOVB R15B, 2(R12)

	// Advance base pointers to the next row.
	ADDQ R11, SI
	ADDQ R10, DI
	DECQ R9
	JNZ  y_loop_h

	VZEROUPPER
	RET

// func upsampleVAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)
// Performs 2x vertical Catmull-Rom upsampling using AVX2.
TEXT ·upsampleVAVX2(SB), NOSPLIT, $0-48
	MOVQ dst+0(FP), DI
	MOVQ src+8(FP), SI
	MOVQ w+16(FP), R8
	MOVQ h+24(FP), R9
	MOVQ dstStride+32(FP), R10
	MOVQ srcStride+40(FP), R11

	// Load constants into YMM registers.
	VMOVDQU ·const_catmull_rom_AB(SB), Y13
	VMOVDQU ·const_catmull_rom_CD(SB), Y12
	VMOVDQU ·const_catmull_rom_DC(SB), Y11
	VMOVDQU ·const_catmull_rom_BA(SB), Y10
	VMOVDQU ·const_64(SB), Y14

	// Initialize pointers for the top edge processing.
	MOVQ SI, R12 // R12: Src column pointer
	MOVQ DI, R13 // R13: Dst column pointer
	MOVQ R8, CX  // Width counter

	// Process the first three output rows (0, 1, 2) for each column.
v_top_edge_loop:
	TESTQ CX, CX
	JZ    v_main_loop_start

	// Load p0, p1, p2 vertically from three different source rows.
	XORQ AX, AX; MOVB (R12), AL
	XORQ BX, BX; MOVB (R12)(R11*1), BL
	XORQ DX, DX; MOVB (R12)(R11*2), DL

	// O0 = cf(139*p0 - 11*p1)
	MOVQ AX, R14; IMULQ $139, R14
	MOVQ BX, R15; IMULQ $-11, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clip0; XORQ R14, R14

v_clip0:
	CMPQ R14, $255; JLE v_clip1; MOVQ $255, R14

v_clip1:
	MOVB R14B, (R13)

	// O1 = cf(104*p0 + 27*p1 - 3*p2)
	MOVQ AX, R14; IMULQ $104, R14
	MOVQ BX, R15; IMULQ $27, R15; ADDQ R15, R14
	MOVQ DX, R15; IMULQ $-3, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clip2; XORQ R14, R14

v_clip2:
	CMPQ R14, $255; JLE v_clip3; MOVQ $255, R14

v_clip3:
	MOVB R14B, (R13)(R10*1)

	// O2 = cf(28*p0 + 109*p1 - 9*p2)
	MOVQ AX, R14; IMULQ $28, R14
	MOVQ BX, R15; IMULQ $109, R15; ADDQ R15, R14
	MOVQ DX, R15; IMULQ $-9, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clip4; XORQ R14, R14

v_clip4:
	CMPQ R14, $255; JLE v_clip5; MOVQ $255, R14

v_clip5:
	MOVB R14B, (R13)(R10*2)

	INCQ R12; INCQ R13; DECQ CX
	JMP  v_top_edge_loop

v_main_loop_start:
	MOVQ  R9, R15; SUBQ $3, R15
	TESTQ R15, R15
	JZ    v_bottom_edge_start

	// Source pointer for the main loop starts at the beginning (row 0).
	MOVQ SI, R12

	// Destination pointer starts at output row 3.
	MOVQ DI, R13
	LEAQ (R13)(R10*2), R13
	ADDQ R10, R13

	VPXOR Y15, Y15, Y15 // Zero Y15 for use in packing.

v_y_loop:
	// Pointers to the start of the 16-pixel block in each of the 4 source rows.
	MOVQ R12, AX
	LEAQ (R12)(R11*1), BX
	LEAQ (R12)(R11*2), CX
	LEAQ (CX)(R11*1), DX  // p3_ptr = R12 + 3*srcStride

	// Pointers to the 2 destination rows.
	MOVQ R13, R14         // o1_ptr
	LEAQ (R13)(R10*1), BP // o2_ptr

	MOVQ R8, R9 // Use R9 as width counter for the x-loop.

v_x_loop:
	CMPQ R9, $16
	JL   v_x_rem

	// Load 16 pixels from each of the 4 consecutive source rows.
	VMOVDQU (AX), X0; VMOVDQU (BX), X1; VMOVDQU (CX), X2; VMOVDQU (DX), X3

	// Unpack, interleave, and apply filter (same logic as horizontal).
	VPMOVZXBW  X0, Y0; VPMOVZXBW X1, Y1; VPMOVZXBW X2, Y2; VPMOVZXBW X3, Y3
	VPUNPCKLWD Y1, Y0, Y4; VPUNPCKHWD Y1, Y0, Y5
	VPUNPCKLWD Y3, Y2, Y6; VPUNPCKHWD Y3, Y2, Y7

	// Calculate O1.
	VPMADDWD Y13, Y4, Y8; VPMADDWD Y13, Y5, Y9
	VPMADDWD Y12, Y6, Y2; VPMADDWD Y12, Y7, Y3
	VPADDD   Y2, Y8, Y8; VPADDD Y3, Y9, Y9

	// Calculate O2.
	VPMADDWD Y11, Y4, Y4; VPMADDWD Y11, Y5, Y5
	VPMADDWD Y10, Y6, Y6; VPMADDWD Y10, Y7, Y7
	VPADDD   Y6, Y4, Y4; VPADDD Y7, Y5, Y5

	// Round and Shift.
	VPADDD Y14, Y8, Y8; VPADDD Y14, Y9, Y9; VPADDD Y14, Y4, Y4; VPADDD Y14, Y5, Y5
	VPSRAD $7, Y8, Y8; VPSRAD $7, Y9, Y9; VPSRAD $7, Y4, Y4; VPSRAD $7, Y5, Y5

	// Pack 32-bit dwords to 16-bit signed words.
	VPACKSSDW Y9, Y8, Y8 // O1 words (Y8)
	VPACKSSDW Y5, Y4, Y4 // O2 words (Y4)

	// Pack 256-bit vector of 16-bit words into a 128-bit vector of 8-bit bytes
	// This is a standard shuffle pattern to combine results from both lanes of a YMM register.

	// Pack words to bytes in-lane.
	VPACKUSWB Y15, Y8, Y8
	VPACKUSWB Y15, Y4, Y4

	// Extract the high 128-bit lane.
	VEXTRACTF128 $1, Y8, X9
	VEXTRACTF128 $1, Y4, X5

	// Interleave the low and high parts to form a contiguous 16-byte result.
	VPUNPCKLQDQ X9, X8, X8
	VPUNPCKLQDQ X5, X4, X4

	// Store the 16-byte results to the two destination rows.
	VMOVDQU X8, (R14)
	VMOVDQU X4, (BP)

	// Advance pointers for the next block of 16 pixels.
	ADDQ $16, AX; ADDQ $16, BX; ADDQ $16, CX; ADDQ $16, DX
	ADDQ $16, R14; ADDQ $16, BP
	SUBQ $16, R9
	JMP  v_x_loop

v_x_rem:
	TESTQ R9, R9
	JZ    v_y_loop_end
	MOVQ  AX, SI

v_x_rem_loop:
	LEAQ (SI)(R11*2), CX
	ADDQ R11, CX

	// Calculate O1 = cf(A*p0 + B*p1 + C*p2 + D*p3)
	XORQ BX, BX; MOVB (SI), BL; IMULQ $-9, BX; MOVQ BX, DX
	XORQ BX, BX; MOVB (SI)(R11*1), BL; IMULQ $111, BX; ADDQ BX, DX
	XORQ BX, BX; MOVB (SI)(R11*2), BL; IMULQ $29, BX; ADDQ BX, DX
	XORQ BX, BX; MOVB (CX), BL; IMULQ $-3, BX; ADDQ BX, DX
	ADDQ $64, DX; SARQ $7, DX
	CMPQ DX, $0; JGE v_clip6; XORQ DX, DX

v_clip6:
	CMPQ DX, $255; JLE v_clip7; MOVQ $255, DX

v_clip7:
	MOVB DL, (R14)

	// Calculate O2 = cf(D*p0 + C*p1 + B*p2 + A*p3)
	XORQ BX, BX; MOVB (SI), BL; IMULQ $-3, BX; MOVQ BX, AX
	XORQ BX, BX; MOVB (SI)(R11*1), BL; IMULQ $29, BX; ADDQ BX, AX
	XORQ BX, BX; MOVB (SI)(R11*2), BL; IMULQ $111, BX; ADDQ BX, AX
	XORQ BX, BX; MOVB (CX), BL; IMULQ $-9, BX; ADDQ BX, AX
	ADDQ $64, AX; SARQ $7, AX
	CMPQ AX, $0; JGE v_clip8; XORQ AX, AX

v_clip8:
	CMPQ AX, $255; JLE v_clip9; MOVQ $255, AX

v_clip9:
	MOVB AL, (BP)

	INCQ SI; INCQ R14; INCQ BP
	DECQ R9
	JNZ  v_x_rem_loop

v_y_loop_end:
	// Advance the main loop pointers to the next set of rows.
	ADDQ R11, R12 // src_row_ptr += srcStride
	ADDQ R10, R13 // dst_row_ptr += 2 * dstStride
	ADDQ R10, R13

	DECQ R15
	JNZ  v_y_loop

v_bottom_edge_start:
	// Pointers R12 and R13 are already positioned correctly by the main loop.
	MOVQ R8, CX // Width counter

v_bottom_edge_loop:
	TESTQ CX, CX
	JZ    v_done

	// Load pixels in reverse order for the symmetric filter.
	XORQ DX, DX; MOVB (R12)(R11*2), DL // p0B = p(H-1)
	XORQ BX, BX; MOVB (R12)(R11*1), BL // p1B = p(H-2)
	XORQ AX, AX; MOVB (R12), AL        // p2B = p(H-3)

	// O(2H-3) (mirrors O2)
	MOVQ DX, R14; IMULQ $28, R14
	MOVQ BX, R15; IMULQ $109, R15; ADDQ R15, R14
	MOVQ AX, R15; IMULQ $-9, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clipA; XORQ R14, R14

v_clipA:
	CMPQ R14, $255; JLE v_clipB; MOVQ $255, R14

v_clipB:
	MOVB R14B, (R13)

	// O(2H-2) (mirrors O1)
	MOVQ DX, R14; IMULQ $104, R14
	MOVQ BX, R15; IMULQ $27, R15; ADDQ R15, R14
	MOVQ AX, R15; IMULQ $-3, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clipC; XORQ R14, R14

v_clipC:
	CMPQ R14, $255; JLE v_clipD; MOVQ $255, R14

v_clipD:
	MOVB R14B, (R13)(R10*1)

	// O(2H-1) (mirrors O0)
	MOVQ DX, R14; IMULQ $139, R14
	MOVQ BX, R15; IMULQ $-11, R15; ADDQ R15, R14
	ADDQ $64, R14; SARQ $7, R14
	CMPQ R14, $0; JGE v_clipE; XORQ R14, R14

v_clipE:
	CMPQ R14, $255; JLE v_clipF; MOVQ $255, R14

v_clipF:
	MOVB R14B, (R13)(R10*2)

	INCQ R12; INCQ R13; DECQ CX
	JMP  v_bottom_edge_loop

v_done:
	VZEROUPPER
	RET

// Clamp REG to [0,255] using TMP.
#define CLAMP(reg, tmp) \
	XORQ    tmp, tmp;   \
	CMPQ    reg, tmp;   \
	CMOVQLT tmp, reg;   \
	MOVQ    $255, tmp;  \
	CMPQ    reg, tmp;   \
	CMOVQGT tmp, reg

// ACC = clamp((C0*P0 + C1*P1 + 64) >> 7).
#define TAP2(p0, p1, c0, c1, acc, tmp) \
	MOVQ  p0, acc;  \
	IMULQ c0, acc;  \
	MOVQ  p1, tmp;  \
	IMULQ c1, tmp;  \
	ADDQ  tmp, acc; \
	ADDQ  $64, acc; \
	SARQ  $7, acc;  \
	CLAMP(acc, tmp)

// ACC = clamp((C0*P0 + C1*P1 + C2*P2 + 64) >> 7).
#define TAP3(p0, p1, p2, c0, c1, c2, acc, tmp) \
	MOVQ  p0, acc;  \
	IMULQ c0, acc;  \
	MOVQ  p1, tmp;  \
	IMULQ c1, tmp;  \
	ADDQ  tmp, acc; \
	MOVQ  p2, tmp;  \
	IMULQ c2, tmp;  \
	ADDQ  tmp, acc; \
	ADDQ  $64, acc; \
	SARQ  $7, acc;  \
	CLAMP(acc, tmp)

// ACC = clamp((C0*P0 + C1*P1 + C2*P2 + C3*P3 + 64) >> 7).
#define TAP4(p0, p1, p2, p3, c0, c1, c2, c3, acc, tmp) \
	MOVQ  p0, acc;  \
	IMULQ c0, acc;  \
	MOVQ  p1, tmp;  \
	IMULQ c1, tmp;  \
	ADDQ  tmp, acc; \
	MOVQ  p2, tmp;  \
	IMULQ c2, tmp;  \
	ADDQ  tmp, acc; \
	MOVQ  p3, tmp;  \
	IMULQ c3, tmp;  \
	ADDQ  tmp, acc; \
	ADDQ  $64, acc; \
	SARQ  $7, acc;  \
	CLAMP(acc, tmp)

// Filter eight pixels held as [p0,p1] pairs in X4/X5 and [p2,p3] pairs in X6/X7
// into O1 words in X8 and O2 words in X4. Clobbers X0, X1, X5, X6, X7, X9.
#define CATMULL8() \
	MOVOU    X4, X8;      \
	PMADDWL  X10, X8;     \
	MOVOU    X6, X0;      \
	PMADDWL  X11, X0;     \
	PADDL    X0, X8;      \
	MOVOU    X5, X9;      \
	PMADDWL  X10, X9;     \
	MOVOU    X7, X1;      \
	PMADDWL  X11, X1;     \
	PADDL    X1, X9;      \
	PMADDWL  X12, X4;     \
	PMADDWL  X13, X6;     \
	PADDL    X6, X4;      \
	PMADDWL  X12, X5;     \
	PMADDWL  X13, X7;     \
	PADDL    X7, X5;      \
	PADDL    X14, X8;     \
	PADDL    X14, X9;     \
	PADDL    X14, X4;     \
	PADDL    X14, X5;     \
	PSRAL    $7, X8;      \
	PSRAL    $7, X9;      \
	PSRAL    $7, X4;      \
	PSRAL    $7, X5;      \
	PACKSSLW X9, X8;      \
	PACKSSLW X5, X4

// upsampleNearestNeighborSSE performs a 2x2 nearest neighbor upsampling.
TEXT ·upsampleNearestNeighborSSE(SB), NOSPLIT, $0-48
	MOVQ src+0(FP), SI
	MOVQ dst+8(FP), DI
	MOVQ srcW+16(FP), R8
	MOVQ srcH+24(FP), R9
	MOVQ srcS+32(FP), R10
	MOVQ dstS+40(FP), R11

sse_y_loop:
	MOVQ SI, R13
	MOVQ DI, R14
	MOVQ R8, CX

sse_x_loop:
	CMPQ CX, $16
	JL   sse_x_rem

	MOVOU (R13), X0

	MOVOU     X0, X1
	PUNPCKLBW X0, X1
	MOVOU     X0, X2
	PUNPCKHBW X0, X2

	MOVOU X1, (R14)
	MOVOU X2, 16(R14)

	ADDQ $16, R13
	ADDQ $32, R14
	SUBQ $16, CX
	JMP  sse_x_loop

sse_x_rem:
	TESTQ CX, CX
	JZ    sse_row_done

sse_x_rem_loop:
	MOVB (R13), AL
	MOVB AL, (R14)
	MOVB AL, 1(R14)
	INCQ R13
	ADDQ $2, R14
	DECQ CX
	JNZ  sse_x_rem_loop

sse_row_done:
	MOVQ DI, R13
	MOVQ DI, R14
	ADDQ R11, R14
	MOVQ R8, CX
	SHLQ $1, CX

sse_copy_loop:
	CMPQ CX, $16
	JL   sse_copy_rem

	MOVOU (R13), X0
	MOVOU X0, (R14)

	ADDQ $16, R13
	ADDQ $16, R14
	SUBQ $16, CX
	JMP  sse_copy_loop

sse_copy_rem:
	TESTQ CX, CX
	JZ    sse_after_copy

sse_copy_rem_loop:
	MOVB (R13), AL
	MOVB AL, (R14)
	INCQ R13
	INCQ R14
	DECQ CX
	JNZ  sse_copy_rem_loop

sse_after_copy:
	ADDQ R10, SI
	ADDQ R11, DI
	ADDQ R11, DI

	DECQ R9
	JNZ  sse_y_loop

	RET

// func upsampleHSSE(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)
TEXT ·upsampleHSSE(SB), NOSPLIT, $0-48
	MOVQ dst+0(FP), DI
	MOVQ src+8(FP), SI
	MOVQ w+16(FP), R8
	MOVQ h+24(FP), R9
	MOVQ dstStride+32(FP), R10
	MOVQ srcStride+40(FP), R11

	MOVOU ·const_catmull_rom_AB(SB), X10
	MOVOU ·const_catmull_rom_CD(SB), X11
	MOVOU ·const_catmull_rom_DC(SB), X12
	MOVOU ·const_catmull_rom_BA(SB), X13
	MOVOU ·const_64(SB), X14

sse_y_loop_h:
	MOVQ DI, R12
	MOVQ SI, R13

	XORQ R14, R14
	MOVB 0(R13), R14B
	XORQ R15, R15
	MOVB 1(R13), R15B
	XORQ AX, AX
	MOVB 2(R13), AL

	TAP2(R14, R15, $139, $-11, DX, BX)
	MOVB DL, 0(R12)
	TAP3(R14, R15, AX, $104, $27, $-3, DX, BX)
	MOVB DL, 1(R12)
	TAP3(R14, R15, AX, $28, $109, $-9, DX, BX)
	MOVB DL, 2(R12)

	ADDQ $3, R12
	MOVQ R8, CX
	SUBQ $3, CX

sse_x_loop_h:
	CMPQ CX, $8
	JL   sse_remainder_h

	PMOVZXBW 0(R13), X0
	PMOVZXBW 1(R13), X1
	PMOVZXBW 2(R13), X2
	PMOVZXBW 3(R13), X3

	MOVOU     X0, X4
	PUNPCKLWL X1, X4
	MOVOU     X0, X5
	PUNPCKHWL X1, X5
	MOVOU     X2, X6
	PUNPCKLWL X3, X6
	MOVOU     X2, X7
	PUNPCKHWL X3, X7

	CATMULL8()

	MOVOU     X8, X0
	PUNPCKLWL X4, X0
	PUNPCKHWL X4, X8
	PACKUSWB  X8, X0
	MOVOU     X0, (R12)

	ADDQ $8, R13
	ADDQ $16, R12
	SUBQ $8, CX
	JMP  sse_x_loop_h

sse_remainder_h:
	CMPQ CX, $0
	JLE  sse_done_row_h

	XORQ AX, AX
	MOVB 0(R13), AL
	XORQ BX, BX
	MOVB 1(R13), BL
	XORQ R14, R14
	MOVB 2(R13), R14B
	XORQ R15, R15
	MOVB 3(R13), R15B

	TAP4(AX, BX, R14, R15, $-9, $111, $29, $-3, DX, BP)
	MOVB DL, 0(R12)
	TAP4(AX, BX, R14, R15, $-3, $29, $111, $-9, DX, BP)
	MOVB DL, 1(R12)

	ADDQ $1, R13
	ADDQ $2, R12
	DECQ CX
	JMP  sse_remainder_h

sse_done_row_h:
	XORQ DX, DX
	MOVB 2(R13), DL
	XORQ BX, BX
	MOVB 1(R13), BL
	XORQ R14, R14
	MOVB 0(R13), R14B

	TAP3(DX, BX, R14, $28, $109, $-9, R15, AX)
	MOVB R15B, 0(R12)
	TAP3(DX, BX, R14, $104, $27, $-3, R15, AX)
	MOVB R15B, 1(R12)
	TAP2(DX, BX, $139, $-11, R15, AX)
	MOVB R15B, 2(R12)

	ADDQ R11, SI
	ADDQ R10, DI
	DECQ R9
	JNZ  sse_y_loop_h

	RET

// func upsampleVSSE(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)
TEXT ·upsampleVSSE(SB), NOSPLIT, $0-48
	MOVQ dst+0(FP), DI
	MOVQ src+8(FP), SI
	MOVQ w+16(FP), R8
	MOVQ h+24(FP), R9
	MOVQ dstStride+32(FP), R10
	MOVQ srcStride+40(FP), R11

	MOVOU ·const_catmull_rom_AB(SB), X10
	MOVOU ·const_catmull_rom_CD(SB), X11
	MOVOU ·const_catmull_rom_DC(SB), X12
	MOVOU ·const_catmull_rom_BA(SB), X13
	MOVOU ·const_64(SB), X14

	MOVQ SI, R12
	MOVQ DI, R13
	MOVQ R8, CX

sse_v_top_edge_loop:
	TESTQ CX, CX
	JZ    sse_v_main_loop_start

	XORQ AX, AX
	MOVB (R12), AL
	XORQ BX, BX
	MOVB (R12)(R11*1), BL
	XORQ DX, DX
	MOVB (R12)(R11*2), DL

	TAP2(AX, BX, $139, $-11, R14, R15)
	MOVB R14B, (R13)
	TAP3(AX, BX, DX, $104, $27, $-3, R14, R15)
	MOVB R14B, (R13)(R10*1)
	TAP3(AX, BX, DX, $28, $109, $-9, R14, R15)
	MOVB R14B, (R13)(R10*2)

	INCQ R12
	INCQ R13
	DECQ CX
	JMP  sse_v_top_edge_loop

sse_v_main_loop_start:
	MOVQ  R9, R15
	SUBQ  $3, R15
	TESTQ R15, R15
	JZ    sse_v_bottom_edge_start

	MOVQ SI, R12

	MOVQ DI, R13
	LEAQ (R13)(R10*2), R13
	ADDQ R10, R13

sse_v_y_loop:
	MOVQ R12, AX
	LEAQ (R12)(R11*1), BX
	LEAQ (R12)(R11*2), CX
	LEAQ (CX)(R11*1), DX

	MOVQ R13, R14
	LEAQ (R13)(R10*1), BP

	MOVQ R8, R9

sse_v_x_loop:
	CMPQ R9, $8
	JL   sse_v_x_rem

	PMOVZXBW (AX), X0
	PMOVZXBW (BX), X1
	PMOVZXBW (CX), X2
	PMOVZXBW (DX), X3

	MOVOU     X0, X4
	PUNPCKLWL X1, X4
	MOVOU     X0, X5
	PUNPCKHWL X1, X5
	MOVOU     X2, X6
	PUNPCKLWL X3, X6
	MOVOU     X2, X7
	PUNPCKHWL X3, X7

	CATMULL8()

	PACKUSWB X8, X8
	PACKUSWB X4, X4
	MOVQ     X8, (R14)
	MOVQ     X4, (BP)

	ADDQ $8, AX
	ADDQ $8, BX
	ADDQ $8, CX
	ADDQ $8, DX
	ADDQ $8, R14
	ADDQ $8, BP
	SUBQ $8, R9
	JMP  sse_v_x_loop

sse_v_x_rem:
	TESTQ R9, R9
	JZ    sse_v_y_loop_end
	MOVQ  AX, SI

sse_v_x_rem_loop:
	LEAQ (SI)(R11*2), CX
	ADDQ R11, CX

	XORQ  BX, BX
	MOVB  (SI), BL
	IMULQ $-9, BX
	MOVQ  BX, DX
	XORQ  BX, BX
	MOVB  (SI)(R11*1), BL
	IMULQ $111, BX
	ADDQ  BX, DX
	XORQ  BX, BX
	MOVB  (SI)(R11*2), BL
	IMULQ $29, BX
	ADDQ  BX, DX
	XORQ  BX, BX
	MOVB  (CX), BL
	IMULQ $-3, BX
	ADDQ  BX, DX
	ADDQ  $64, DX
	SARQ  $7, DX
	CLAMP(DX, BX)
	MOVB  DL, (R14)

	XORQ  BX, BX
	MOVB  (SI), BL
	IMULQ $-3, BX
	MOVQ  BX, AX
	XORQ  BX, BX
	MOVB  (SI)(R11*1), BL
	IMULQ $29, BX
	ADDQ  BX, AX
	XORQ  BX, BX
	MOVB  (SI)(R11*2), BL
	IMULQ $111, BX
	ADDQ  BX, AX
	XORQ  BX, BX
	MOVB  (CX), BL
	IMULQ $-9, BX
	ADDQ  BX, AX
	ADDQ  $64, AX
	SARQ  $7, AX
	CLAMP(AX, BX)
	MOVB  AL, (BP)

	INCQ SI
	INCQ R14
	INCQ BP
	DECQ R9
	JNZ  sse_v_x_rem_loop

sse_v_y_loop_end:
	ADDQ R11, R12
	ADDQ R10, R13
	ADDQ R10, R13

	DECQ R15
	JNZ  sse_v_y_loop

sse_v_bottom_edge_start:
	MOVQ R8, CX

sse_v_bottom_edge_loop:
	TESTQ CX, CX
	JZ    sse_v_done

	XORQ DX, DX
	MOVB (R12)(R11*2), DL
	XORQ BX, BX
	MOVB (R12)(R11*1), BL
	XORQ AX, AX
	MOVB (R12), AL

	TAP3(DX, BX, AX, $28, $109, $-9, R14, R15)
	MOVB R14B, (R13)
	TAP3(DX, BX, AX, $104, $27, $-3, R14, R15)
	MOVB R14B, (R13)(R10*1)
	TAP2(DX, BX, $139, $-11, R14, R15)
	MOVB R14B, (R13)(R10*2)

	INCQ R12
	INCQ R13
	DECQ CX
	JMP  sse_v_bottom_edge_loop

sse_v_done:
	RET
