//go:build amd64 && !noasm

#include "textflag.h"

// This file provides an optimized AVX2 implementation of the 8x8 Inverse Discrete Cosine Transform (IDCT).
// The implementation uses the transposition method, applying the AAN fast IDCT algorithm.
//
// The algorithm proceeds in the following stages:
// 1.  Load & Check: Load the 8x8 block (as rows) and check for the DC-only case.
// 2.  T1 (Transpose 1): Transpose the input from rows to columns (Structure of Arrays format).
// 3.  P1 (Pass 1): Apply the 1D IDCT logic (SoA).
// 4.  T2 (Transpose 2): Transpose the intermediate results back (Array of Structures format).
// 5.  P2 (Pass 2): Apply the 1D IDCT logic (AoS).
// 6.  Pack, Clip, Reorder & Store: The results from P2 are in row-major order.
//     The packing stage clips values to the [0, 255] range, reorders the rows
//     within the YMM registers, and stores the final 8x8 byte block.

// Macro to define a 256-bit YMM constant by broadcasting a 32-bit value.
#define CONST_YMM(name, val) \
DATA name<>+0(SB)/4, $val; \
DATA name<>+4(SB)/4, $val; \
DATA name<>+8(SB)/4, $val; \
DATA name<>+12(SB)/4, $val; \
DATA name<>+16(SB)/4, $val; \
DATA name<>+20(SB)/4, $val; \
DATA name<>+24(SB)/4, $val; \
DATA name<>+28(SB)/4, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $32

// AAN fast IDCT algorithm constants (scaled by 2^11).
CONST_YMM(const_w3, 2408)
CONST_YMM(const_w6, 1108)
CONST_YMM(const_w7, 565)

// Pre-computed combined constants for efficiency.
CONST_YMM(const_w1mw7, 2276) // w1-w7
CONST_YMM(const_w1pw7, 3406) // w1+w7
CONST_YMM(const_w3mw5, 799)  // w3-w5
CONST_YMM(const_w3pw5, 4017) // w3+w5
CONST_YMM(const_w2mw6, 1568) // w2-w6
CONST_YMM(const_w2pw6, 3784) // w2+w6

// Other constants for scaling, rounding, and level-shifting.
CONST_YMM(const_181, 181)
CONST_YMM(const_128, 128)
CONST_YMM(const_4, 4)              // P2 intermediate rounding (for >> 3)
CONST_YMM(const_round_pass1, 128)  // P1 (rowIdct) rounding for >> 8
CONST_YMM(const_round_pass2, 8192) // P2 (colIdct) rounding for >> 14
CONST_YMM(const_round_col, 32)     // For DC-only path
CONST_YMM(const_level_shift, 128)
CONST_YMM(const_zero, 0)

// Mask to isolate AC coefficients by zeroing out the DC coefficient.
DATA mask_ac_coeffs<>+0(SB)/4, $0x00000000
DATA mask_ac_coeffs<>+4(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+8(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+12(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+16(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+20(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+24(SB)/4, $0xffffffff
DATA mask_ac_coeffs<>+28(SB)/4, $0xffffffff
GLOBL mask_ac_coeffs<>(SB), RODATA|NOPTR, $32

// Macro for Transpose Stage 1 (32-bit element interleaving).
#define TRANSPOSE_32BIT_S1() \
	/* Interleave low dwords */                     \
	VPUNPCKLDQ Y1, Y0, Y8; VPUNPCKLDQ Y3, Y2, Y10;  \
	VPUNPCKLDQ Y5, Y4, Y12; VPUNPCKLDQ Y7, Y6, Y14; \
	/* Interleave high dwords */                    \
	VPUNPCKHDQ Y1, Y0, Y9; VPUNPCKHDQ Y3, Y2, Y11;  \
	VPUNPCKHDQ Y5, Y4, Y13; VPUNPCKHDQ Y7, Y6, Y15

// func idctAVX2(in *[64]int32, out []byte, offset int, stride int)
TEXT ·idctAVX2(SB), NOSPLIT, $16-48
	MOVQ in+0(FP), SI       // SI = input block pointer
	MOVQ out_data+8(FP), DI // DI = output slice data pointer
	MOVQ offset+32(FP), R8  // R8 = output offset
	MOVQ stride+40(FP), CX  // CX = output stride
	ADDQ R8, DI             // DI = effective output start address

	// Load the 8x8 block (Rows) into Y0-Y7.
	VMOVDQU 0(SI), Y0; VMOVDQU 32(SI), Y1
	VMOVDQU 64(SI), Y2; VMOVDQU 96(SI), Y3
	VMOVDQU 128(SI), Y4; VMOVDQU 160(SI), Y5
	VMOVDQU 192(SI), Y6; VMOVDQU 224(SI), Y7

	// DC-Only Optimization Check
	VMOVDQU mask_ac_coeffs<>(SB), Y15
	VPAND   Y15, Y0, Y15              // Mask out DC coefficient

	// OR all rows together.
	VPOR Y1, Y15, Y15; VPOR Y2, Y15, Y15; VPOR Y3, Y15, Y15
	VPOR Y4, Y15, Y15; VPOR Y5, Y15, Y15; VPOR Y6, Y15, Y15; VPOR Y7, Y15, Y15

	// VPTEST checks if the result is zero (ZF=1 if Y15 is all zeros).
	VPTEST Y15, Y15
	JZ     dc_only_path

	// Transpose 1 (T1: Rows -> Columns/SoA Frequencies)
	// Stage 1: 32-bit interleave.
	TRANSPOSE_32BIT_S1()

	// Stage 2: 64-bit interleave. Input Y8-15. Output Y0-7.
	VPUNPCKLQDQ Y10, Y8, Y0; VPUNPCKHQDQ Y10, Y8, Y1
	VPUNPCKLQDQ Y11, Y9, Y2; VPUNPCKHQDQ Y11, Y9, Y3
	VPUNPCKLQDQ Y14, Y12, Y4; VPUNPCKHQDQ Y14, Y12, Y5
	VPUNPCKLQDQ Y15, Y13, Y6; VPUNPCKHQDQ Y15, Y13, Y7

	// Stage 3: 128-bit interleave. Final T1 result (Columns) in Y8-Y15.
	// VPERM2F128 permutes 128-bit lanes. $0x20 selects Src1_L0 and Src2_L0.
	VPERM2F128 $0x20, Y4, Y0, Y8; VPERM2F128  $0x20, Y5, Y1, Y9
	VPERM2F128 $0x20, Y6, Y2, Y10; VPERM2F128  $0x20, Y7, Y3, Y11

	// $0x31 selects Src1_L1 and Src2_L1.
	VPERM2F128 $0x31, Y4, Y0, Y12; VPERM2F128  $0x31, Y5, Y1, Y13
	VPERM2F128 $0x31, Y6, Y2, Y14; VPERM2F128  $0x31, Y7, Y3, Y15

	// Pass 1 (IDCT on Columns, using rowIdct logic)
	// Register map (AAN algorithm variables mapped to permuted Frequencies):
	// x0=Y8, x1=Y12, x2=Y14, x3=Y10, x4=Y9, x5=Y15, x6=Y13, x7=Y11.
	// Setup: Scaling (<< 11) and rounding (+128).
	VPSLLD $11, Y12, Y12                   // x1 << 11
	VPSLLD $11, Y8, Y8                     // x0 << 11
	VPADDD const_round_pass1<>(SB), Y8, Y8 // x0 + 128

	// Stage 1:
	VPADDD  Y15, Y9, Y0; VPADDD  Y11, Y13, Y1
	VPMULLD const_w7<>(SB), Y0, Y2; VPMULLD const_w3<>(SB), Y1, Y3
	VPMULLD const_w1mw7<>(SB), Y9, Y9; VPADDD  Y2, Y9, Y9
	VPMULLD const_w1pw7<>(SB), Y15, Y15; VPSUBD  Y15, Y2, Y15
	VPMULLD const_w3mw5<>(SB), Y13, Y13; VPSUBD  Y13, Y3, Y13
	VPMULLD const_w3pw5<>(SB), Y11, Y11; VPSUBD  Y11, Y3, Y11

	// Stage 2:
	VPADDD  Y12, Y8, Y0; VPSUBD  Y12, Y8, Y8
	VPADDD  Y14, Y10, Y1; VPMULLD const_w6<>(SB), Y1, Y1
	VPMULLD const_w2pw6<>(SB), Y14, Y14; VPSUBD Y14, Y1, Y14
	VPMULLD const_w2mw6<>(SB), Y10, Y10; VPADDD Y1, Y10, Y10

	// Stage 3 & 4:
	VPADDD Y13, Y9, Y1; VPSUBD  Y13, Y9, Y2
	VPADDD Y11, Y15, Y3; VPSUBD  Y11, Y15, Y4
	VPADDD Y10, Y0, Y5; VPSUBD  Y10, Y0, Y6
	VPADDD Y14, Y8, Y7; VPSUBD  Y14, Y8, Y8

	// Rotation stage
	VPADDD Y4, Y2, Y9; VPSUBD  Y4, Y2, Y15

	// (x * 181 + 128) >> 8
	VPMULLD const_181<>(SB), Y9, Y9; VPADDD const_128<>(SB), Y9, Y9; VPSRAD $8, Y9, Y9
	VPMULLD const_181<>(SB), Y15, Y15; VPADDD const_128<>(SB), Y15, Y15; VPSRAD $8, Y15, Y15

	// Final calculation for Pass 1 (>> 8).
	// Inputs: x1=Y1, x6=Y3, x7=Y5, x8=Y6, x3=Y7, x0=Y8, x2=Y9, x4=Y15.
	// We save Y5 to Y12 to prevent it from being clobbered.
	VMOVDQU Y1, Y10 // Save x1 in Y10
	VMOVDQU Y3, Y11 // Save x6 in Y11
	VMOVDQU Y5, Y12 // Save x7 (Y5) to Y12 before it's overwritten.

	// Calculate sums into Y0-Y3.
	VPADDD Y12, Y10, Y0 // Y0 = x7+x1 (use saved Y12)
	VPADDD Y7, Y9, Y1   // Y1 = x3+x2
	VPADDD Y8, Y15, Y2  // Y2 = x0+x4
	VPADDD Y6, Y11, Y3  // Y3 = x8+x6

	// Calculate differences into Y4-Y7.
	VPSUBD Y11, Y6, Y4  // Y4 = x8-x6
	VPSUBD Y15, Y8, Y5  // Y5 = x0-x4 (This overwrites original Y5)
	VPSUBD Y9, Y7, Y6   // Y6 = x3-x2
	VPSUBD Y10, Y12, Y7 // <<< FIX: Y7 = x7-x1 (use saved Y12)

	// Shift all results (>> 8).
	VPSRAD $8, Y0, Y0
	VPSRAD $8, Y1, Y1
	VPSRAD $8, Y2, Y2
	VPSRAD $8, Y3, Y3
	VPSRAD $8, Y4, Y4
	VPSRAD $8, Y5, Y5
	VPSRAD $8, Y6, Y6
	VPSRAD $8, Y7, Y7

	// Transpose 2 (T2: Intermediate SoA -> AoS Rows)
	// Transpose Y0-Y7 into Y8-Y15.
	TRANSPOSE_32BIT_S1()

	// Stage 2: 64-bit interleave.
	VPUNPCKLQDQ Y10, Y8, Y0; VPUNPCKHQDQ Y10, Y8, Y1
	VPUNPCKLQDQ Y11, Y9, Y2; VPUNPCKHQDQ Y11, Y9, Y3
	VPUNPCKLQDQ Y14, Y12, Y4; VPUNPCKHQDQ Y14, Y12, Y5
	VPUNPCKLQDQ Y15, Y13, Y6; VPUNPCKHQDQ Y15, Y13, Y7

	// Stage 3: 128-bit interleave. Final T2 result (Rows) in Y8-Y15.
	VPERM2F128 $0x20, Y4, Y0, Y8; VPERM2F128  $0x20, Y5, Y1, Y9
	VPERM2F128 $0x20, Y6, Y2, Y10; VPERM2F128  $0x20, Y7, Y3, Y11
	VPERM2F128 $0x31, Y4, Y0, Y12; VPERM2F128  $0x31, Y5, Y1, Y13
	VPERM2F128 $0x31, Y6, Y2, Y14; VPERM2F128  $0x31, Y7, Y3, Y15

	// Pass 2 (IDCT on Rows, using colIdct logic)
	// Input Y8-Y15. Register map is the same as Pass 1.
	// Setup: Scaling (<< 8) and rounding (+8192).
	VPSLLD $8, Y12, Y12; VPSLLD $8, Y8, Y8
	VPADDD const_round_pass2<>(SB), Y8, Y8

	// Stage 1: (with intermediate rounding +4 and shift >> 3).
	VPADDD Y15, Y9, Y0; VPADDD  Y11, Y13, Y1

	// x8 = w7*(x4+x5) + 4
	VPMULLD const_w7<>(SB), Y0, Y2; VPADDD const_4<>(SB), Y2, Y2

	// x8 = w3*(x6+x7) + 4
	VPMULLD const_w3<>(SB), Y1, Y3; VPADDD const_4<>(SB), Y3, Y3

	// (x8 + (w1-w7)*x4) >> 3
	VPMULLD const_w1mw7<>(SB), Y9, Y9; VPADDD Y2, Y9, Y9; VPSRAD $3, Y9, Y9

	// (x8 - (w1+w7)*x5) >> 3
	VPMULLD const_w1pw7<>(SB), Y15, Y15; VPSUBD Y15, Y2, Y15; VPSRAD $3, Y15, Y15

	// (x8 - (w3-w5)*x6) >> 3
	VPMULLD const_w3mw5<>(SB), Y13, Y13; VPSUBD Y13, Y3, Y13; VPSRAD $3, Y13, Y13

	// (x8 - (w3+w5)*x7) >> 3
	VPMULLD const_w3pw5<>(SB), Y11, Y11; VPSUBD Y11, Y3, Y11; VPSRAD $3, Y11, Y11

	// Stage 2:
	VPADDD Y12, Y8, Y0; VPSUBD Y12, Y8, Y8

	// x1 = w6*(x3+x2) + 4
	VPADDD Y14, Y10, Y1; VPMULLD const_w6<>(SB), Y1, Y1; VPADDD const_4<>(SB), Y1, Y1

	// (x1 - (w2+w6)*x2) >> 3
	VPMULLD const_w2pw6<>(SB), Y14, Y14; VPSUBD Y14, Y1, Y14; VPSRAD $3, Y14, Y14

	// (x1 + (w2-w6)*x3) >> 3
	VPMULLD const_w2mw6<>(SB), Y10, Y10; VPADDD Y1, Y10, Y10; VPSRAD $3, Y10, Y10

	// Stage 3 & 4
	VPADDD Y13, Y9, Y1; VPSUBD  Y13, Y9, Y2
	VPADDD Y11, Y15, Y3; VPSUBD  Y11, Y15, Y4
	VPADDD Y10, Y0, Y5; VPSUBD  Y10, Y0, Y6
	VPADDD Y14, Y8, Y7; VPSUBD  Y14, Y8, Y8

	// Rotation stage
	VPADDD Y4, Y2, Y9; VPSUBD  Y4, Y2, Y15

	// (181*(x4+x5) + 128) >> 8
	VPMULLD const_181<>(SB), Y9, Y9; VPADDD const_128<>(SB), Y9, Y9; VPSRAD $8, Y9, Y9

	// (181*(x4-x5) + 128) >> 8
	VPMULLD const_181<>(SB), Y15, Y15; VPADDD const_128<>(SB), Y15, Y15; VPSRAD $8, Y15, Y15

	// Final calculation for Pass 2 (>> 14) and level shift (+128).
	// We save x7 (Y5) to Y13 to prevent it from being overwritten.
	// Inputs: x1=Y1, x6=Y3, x7=Y5, x8=Y6, x3=Y7, x0=Y8, x2=Y9, x4=Y15.
	VMOVDQU Y1, Y10                      // Save x1 in Y10
	VMOVDQU Y3, Y12                      // Save x6 in Y12
	VMOVDQU Y5, Y13                      // Save x7 (Y5) to Y13.
	VMOVDQU const_level_shift<>(SB), Y11 // Load level shift constant

	// Calculate sums into Y0-Y3.
	VPADDD Y13, Y10, Y0 // Y0 = x7+x1 (use saved Y13)
	VPADDD Y7, Y9, Y1   // Y1 = x3+x2
	VPADDD Y8, Y15, Y2  // Y2 = x0+x4
	VPADDD Y6, Y12, Y3  // Y3 = x8+x6

	// Calculate differences into Y4-Y7.
	VPSUBD Y12, Y6, Y4  // Y4 = x8-x6
	VPSUBD Y15, Y8, Y5  // Y5 = x0-x4 (This overwrites original Y5)
	VPSUBD Y9, Y7, Y6   // Y6 = x3-x2
	VPSUBD Y10, Y13, Y7 // Y7 = x7-x1 (use saved Y13)

	// Shift (>> 14) and level shift (+128).
	VPSRAD $14, Y0, Y0; VPADDD Y11, Y0, Y0
	VPSRAD $14, Y1, Y1; VPADDD Y11, Y1, Y1
	VPSRAD $14, Y2, Y2; VPADDD Y11, Y2, Y2
	VPSRAD $14, Y3, Y3; VPADDD Y11, Y3, Y3
	VPSRAD $14, Y4, Y4; VPADDD Y11, Y4, Y4
	VPSRAD $14, Y5, Y5; VPADDD Y11, Y5, Y5
	VPSRAD $14, Y6, Y6; VPADDD Y11, Y6, Y6
	VPSRAD $14, Y7, Y7; VPADDD Y11, Y7, Y7

	// Packing, Reordering and Storing
	// Input: Y0-Y7 hold the final 8 rows (R0-R7) as 32-bit integers.
	// R0L denotes the low 4 elements (0-3), R0H the high 4 elements (4-7).

	// Pack 32-bit signed integers to 16-bit signed integers with saturation.
	// AVX2 VPACKSSDW operates in-lane. It packs (Src2, Src1) -> Dest.
	// Y0 = [L1: R1H|R0H | L0: R1L|R0L] (16-bit elements)
	VPACKSSDW Y1, Y0, Y0
	VPACKSSDW Y3, Y2, Y1 // Y1 = [L1: R3H|R2H | L0: R3L|R2L]
	VPACKSSDW Y5, Y4, Y2 // Y2 = [L1: R5H|R4H | L0: R5L|R4L]
	VPACKSSDW Y7, Y6, Y3 // Y3 = [L1: R7H|R6H | L0: R7L|R6L]

	// Pack 16-bit signed integers to 8-bit unsigned bytes with saturation (clipping).
	// AVX2 VPACKUSWB also operates in-lane.
	// Y0 = [L1: R3H|R2H|R1H|R0H | L0: R3L|R2L|R1L|R0L] (8-bit elements)
	VPACKUSWB Y1, Y0, Y0

	// Y1 = [L1: R7H|R6H|R5H|R4H | L0: R7L|R6L|R5L|R4L]
	VPACKUSWB Y3, Y2, Y1

	// We must reorganize the data from the in-lane packing format to contiguous rows.

	// Step 1: Use VPERMQ to group H/L parts together within the same 128-bit lane.
	// Mask 0xD8 (11011000b). Permutation order (LSB first): Q0, Q2, Q1, Q3.
	// Input Y0 (by QW): [Q3:R3H,R2H | Q2:R1H,R0H | Q1:R3L,R2L | Q0:R1L,R0L]
	// Output Y0: [R3H,R2H | R3L,R2L || R1H,R0H | R1L,R0L].
	VPERMQ $0xD8, Y0, Y0
	VPERMQ $0xD8, Y1, Y1

	// Step 2: Use VPSHUFD (in-lane) to interleave H/L parts (32-bit chunks) within each lane.
	// Mask 0xD8. Permutation order (LSB first): D0, D2, D1, D3.
	// Lane 0 input: [R1H,R0H | R1L,R0L].
	// Lane 0 output: [R1H, R1L, R0H, R0L] = [R1, R0].
	// Final Y0: [R3, R2 | R1, R0].
	VPSHUFD $0xD8, Y0, Y0
	VPSHUFD $0xD8, Y1, Y1

	// Store the final 64 bytes to the output slice.
	CMPQ CX, $8
	JE   store_stride8

store_strided:
	// Handle non-contiguous output (stride != 8).
	// The strategy is to store each 128-bit XMM register (containing 2 rows)
	// to the stack, and then load each 64-bit (8-byte) row into a GPR for storing to the output.
	VEXTRACTF128 $0, Y0, X2 // X2 = [Row 1, Row 0]
	VEXTRACTF128 $1, Y0, X3 // X3 = [Row 3, Row 2]
	VEXTRACTF128 $0, Y1, X4 // X4 = [Row 5, Row 4]
	VEXTRACTF128 $1, Y1, X5 // X5 = [Row 7, Row 6]

	// Store and write rows 0 & 1
	VMOVDQU X2, 0(SP) // Store [R1, R0] to the stack
	MOVQ    0(SP), R9 // Load R0 into R9
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI
	MOVQ    8(SP), R9 // Load R1 into R9
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI

	// Store and write rows 2 & 3
	VMOVDQU X3, 0(SP) // Store [R3, R2] to the stack
	MOVQ    0(SP), R9 // Load R2
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI
	MOVQ    8(SP), R9 // Load R3
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI

	// Store and write rows 4 & 5
	VMOVDQU X4, 0(SP) // Store [R5, R4] to the stack
	MOVQ    0(SP), R9 // Load R4
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI
	MOVQ    8(SP), R9 // Load R5
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI

	// Store and write rows 6 & 7
	VMOVDQU X5, 0(SP) // Store [R7, R6] to the stack
	MOVQ    0(SP), R9 // Load R6
	MOVQ    R9, 0(DI)
	ADDQ    CX, DI
	MOVQ    8(SP), R9 // Load R7
	MOVQ    R9, 0(DI)
	JMP     done

store_stride8:
	// Handle contiguous output (stride == 8) with two 256-bit moves.
	// Y0 = [R3, R2, R1, R0]. Y1 = [R7, R6, R5, R4].
	VMOVDQU Y0, 0(DI)  // Store R0-R3
	VMOVDQU Y1, 32(DI) // Store R4-R7
	JMP     done

dc_only_path:
	// Calculation: clip(((in[0] << 3) + 32) >> 6 + 128).
	// Load the DC coefficient (first element).
	MOVD   (SI), X0
	VPSLLD $3, X0, X0
	VPADDD const_round_col<>(SB), X0, X0
	VPSRAD $6, X0, X0
	VPADDD const_level_shift<>(SB), X0, X0

	// Pack to a single byte with saturation.
	VMOVDQU   const_zero<>(SB), X1
	VPACKSSDW X1, X0, X0
	VPACKUSWB X1, X0, X0

	// Broadcast the resulting byte to all 64 positions.
	// Move the byte to memory (stack) first, then broadcast from memory.
	MOVD         X0, 8(SP)
	VPBROADCASTB 8(SP), Y0

	// Store the resulting block.
	CMPQ CX, $8
	JNE  dc_store_strided

dc_store_stride8:
	// Store Y0 twice to fill the 64-byte block efficiently.
	VMOVDQU Y0, 0(DI)
	VMOVDQU Y0, 32(DI)
	JMP     done

dc_store_strided:
	// Extract the 8-byte pattern since all bytes are the same.
	VEXTRACTF128 $0, Y0, X0
	VMOVQ        X0, 0(SP)  // Store the 8-byte pattern from X0 to the stack
	MOVQ         0(SP), R9  // Load the pattern into R9

	// Store the pattern 8 times, handling the stride.
	MOVQ R9, 0(DI); ADDQ CX, DI; MOVQ R9, 0(DI); ADDQ CX, DI
	MOVQ R9, 0(DI); ADDQ CX, DI; MOVQ R9, 0(DI); ADDQ CX, DI
	MOVQ R9, 0(DI); ADDQ CX, DI; MOVQ R9, 0(DI); ADDQ CX, DI
	MOVQ R9, 0(DI); ADDQ CX, DI; MOVQ R9, 0(DI)

done:
	// Clear the upper 128 bits of all YMM registers before returning (AVX/SSE transition penalty avoidance).
	VZEROUPPER
	RET
