//go:build arm64 && !noasm

#include "textflag.h"

#define MUL4S(m, n, d) WORD $(0x4EA09C00 | ((m) << 16) | ((n) << 5) | (d))
#define SQXTUN2_16B(n, d) WORD $(0x6E212800 | ((n) << 5) | (d))
#define SQXTUN8B(n, d) WORD $(0x2E212800 | ((n) << 5) | (d))
#define SSHR4S(k, n, d) WORD $(0x4F000400 | ((64 - (k)) << 16) | ((n) << 5) | (d))
#define UQXTN2_8H(n, d) WORD $(0x6E614800 | ((n) << 5) | (d))
#define UQXTN4H(n, d) WORD $(0x2E614800 | ((n) << 5) | (d))

// Optimized ARM64 NEON 8x8 IDCT (AAN algorithm + transposition)
// This implementation uses a two-pass approach with a matrix transpose in between.
//
// Algorithm overview:
// 1. Load 8x8 block and check for a DC-only fast path.
// 2. Transpose 1: Convert the 8x8 block from row-major to column-major. This allows applying the 1D IDCT to 8 columns in parallel using NEON vectors.
// 3. Pass 1 (Columns): Perform a 1D IDCT on the 8 columns simultaneously. This is logically equivalent to the pure Go `rowIdct` function.
// 4. Transpose 2: Transpose the intermediate results back to row-major order.
// 5. Pass 2 (Rows): Perform a 1D IDCT on the 8 rows simultaneously. This is logically equivalent to the pure Go `colIdct` function.
// 6. Pack & Store: The final 32-bit integer results are saturated and packed into 8-bit unsigned bytes, clipped to the [0, 255] range, and stored to memory.
//
// Unsupported vector instructions (VMUL, VSSHR, SQXTN/SQXTN2, SQXTUN/SQXTUN2) are emitted directly using the WORD directive with their pre-calculated opcodes.
// These opcodes are for instructions available on ARMv8-A but not directly supported by the Go assembler syntax.

// CONST32 defines a 32-bit constant in the read-only data section.
#define CONST32(name, val) \
DATA name<>+0(SB)/4, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $4

// AAN fixed-point constants (scaled by 2^11).
CONST32(const_w1, 2841); CONST32(const_w2, 2676); CONST32(const_w3, 2408)
CONST32(const_w5, 1609); CONST32(const_w6, 1108); CONST32(const_w7, 565)

// Pre-calculated combined constants to reduce instruction count.
CONST32(const_w1mw7, 2276) // w1 - w7
CONST32(const_w1pw7, 3406) // w1 + w7
CONST32(const_w3mw5, 799)  // w3 - w5
CONST32(const_w3pw5, 4017) // w3 + w5
CONST32(const_w2mw6, 1568) // w2 - w6
CONST32(const_w2pw6, 3784) // w2 + w6

// Constants for rounding, rotation, and level-shifting.
CONST32(const_rot, 181)
CONST32(const_128, 128)
CONST32(const_4, 4)
CONST32(const_8192, 8192)

// LOAD_CONST_VEC loads a 32-bit constant and broadcasts it across a 128-bit NEON vector.
#define LOAD_CONST_VEC(name, Vd) \
	MOVD  $name<>(SB), R6; \
	MOVWU (R6), R7;        \
	VDUP  R7, Vd.S4

// func idctNEON(in *[64]int32, out []byte, offset int, stride int)
TEXT ·idctNEON(SB), NOSPLIT, $0-48
	// Load function arguments from the stack into general-purpose registers.
	MOVD in+0(FP), R0       // R0 = input block pointer (*[64]int32)
	MOVD out_base+8(FP), R1 // R1 = output slice data pointer (&out[0])
	MOVD offset+32(FP), R2  // R2 = output offset
	MOVD stride+40(FP), R3  // R3 = output stride
	ADD  R2, R1, R1         // R1 = &out[0] + offset (effective start address)

	// Load the entire 8x8 block of int32 coefficients into NEON registers V0-V15.
	MOVD   R0, R8
	VLD1.P 64(R8), [V0.B16, V1.B16, V2.B16, V3.B16]
	VLD1.P 64(R8), [V4.B16, V5.B16, V6.B16, V7.B16]
	VLD1.P 64(R8), [V8.B16, V9.B16, V10.B16, V11.B16]
	VLD1.P 64(R8), [V12.B16, V13.B16, V14.B16, V15.B16]

	// DC-only fast path optimization.
	VMOV V0.S[0], R4                                               // Save DC coefficient (in[0]) to R4 (W4).
	SXTW R4, R4                                                    // SIGN-EXTEND W4 -> X4 to preserve negative DC values for 64-bit arithmetic.
	MOVD $0, R5
	VMOV R5, V0.S[0]                                               // Temporarily zero the DC coefficient in V0.
	VORR V0.B16, V1.B16, V16.B16
	VORR V2.B16, V16.B16, V16.B16; VORR V3.B16, V16.B16, V16.B16
	VORR V4.B16, V16.B16, V16.B16; VORR V5.B16, V16.B16, V16.B16
	VORR V6.B16, V16.B16, V16.B16; VORR V7.B16, V16.B16, V16.B16
	VORR V8.B16, V16.B16, V16.B16; VORR V9.B16, V16.B16, V16.B16
	VORR V10.B16, V16.B16, V16.B16; VORR V11.B16, V16.B16, V16.B16
	VORR V12.B16, V16.B16, V16.B16; VORR V13.B16, V16.B16, V16.B16
	VORR V14.B16, V16.B16, V16.B16; VORR V15.B16, V16.B16, V16.B16
	VMOV V16.D[0], R5
	VMOV V16.D[1], R6
	ORR  R6, R5, R5
	CBZ  R5, dc_only_path                                          // If zero, branch to the simplified DC-only logic.

	// Restore the original DC coefficient if not taking the fast path.
	VMOV R4, V0.S[0]

	// ===== Transpose 1 (Rows -> Columns) =====
	VTRN1 V2.S4, V0.S4, V16.S4; VTRN2 V2.S4, V0.S4, V17.S4
	VTRN1 V6.S4, V4.S4, V18.S4; VTRN2 V6.S4, V4.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V0.D2; VTRN2 V18.D2, V16.D2, V2.D2
	VTRN1 V19.D2, V17.D2, V4.D2; VTRN2 V19.D2, V17.D2, V6.D2
	VTRN1 V3.S4, V1.S4, V16.S4; VTRN2 V3.S4, V1.S4, V17.S4
	VTRN1 V7.S4, V5.S4, V18.S4; VTRN2 V7.S4, V5.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V1.D2; VTRN2 V18.D2, V16.D2, V3.D2
	VTRN1 V19.D2, V17.D2, V5.D2; VTRN2 V19.D2, V17.D2, V7.D2
	VTRN1 V10.S4, V8.S4, V16.S4; VTRN2 V10.S4, V8.S4, V17.S4
	VTRN1 V14.S4, V12.S4, V18.S4; VTRN2 V14.S4, V12.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V8.D2; VTRN2 V18.D2, V16.D2, V10.D2
	VTRN1 V19.D2, V17.D2, V12.D2; VTRN2 V19.D2, V17.D2, V14.D2
	VTRN1 V11.S4, V9.S4, V16.S4; VTRN2 V11.S4, V9.S4, V17.S4
	VTRN1 V15.S4, V13.S4, V18.S4; VTRN2 V15.S4, V13.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V9.D2; VTRN2 V18.D2, V16.D2, V11.D2
	VTRN1 V19.D2, V17.D2, V13.D2; VTRN2 V19.D2, V17.D2, V15.D2

	// After Transpose 1: C0:(V0,V8), C1:(V4,V12), C2:(V2,V10), C3:(V6,V14), C4:(V1,V9), C5:(V5,V13), C6:(V3,V11), C7:(V7,V15)

	// ===== Pass 1 (Columns) =====
	LOAD_CONST_VEC(const_w1mw7, V16); LOAD_CONST_VEC(const_w1pw7, V17)
	LOAD_CONST_VEC(const_w3mw5, V18); LOAD_CONST_VEC(const_w3pw5, V19)
	LOAD_CONST_VEC(const_w2mw6, V20); LOAD_CONST_VEC(const_w2pw6, V21)
	LOAD_CONST_VEC(const_128, V22);   LOAD_CONST_VEC(const_rot, V23)
	LOAD_CONST_VEC(const_w7, V24);    LOAD_CONST_VEC(const_w3, V25)
	LOAD_CONST_VEC(const_w6, V26)

	VSHL $11, V0.S4, V0.S4; VSHL $11, V8.S4, V8.S4
	VADD V22.S4, V0.S4, V0.S4; VADD V22.S4, V8.S4, V8.S4
	VSHL $11, V1.S4, V1.S4; VSHL $11, V9.S4, V9.S4

	// Stage 1
	VADD V7.S4, V4.S4, V27.S4; VADD V15.S4, V12.S4, V28.S4
	MUL4S(24, 27, 27); MUL4S(24, 28, 28)
	MUL4S(16, 4, 29); MUL4S(16, 12, 30)
	VADD V29.S4, V27.S4, V4.S4; VADD V30.S4, V28.S4, V12.S4
	MUL4S(17, 7, 29); MUL4S(17, 15, 30)
	VSUB V29.S4, V27.S4, V7.S4; VSUB V30.S4, V28.S4, V15.S4
	VADD V6.S4, V5.S4, V27.S4; VADD V14.S4, V13.S4, V28.S4
	MUL4S(25, 27, 27); MUL4S(25, 28, 28)
	MUL4S(18, 5, 29); MUL4S(18, 13, 30)
	VSUB V29.S4, V27.S4, V5.S4; VSUB V30.S4, V28.S4, V13.S4
	MUL4S(19, 6, 29); MUL4S(19, 14, 30)
	VSUB V29.S4, V27.S4, V6.S4; VSUB V30.S4, V28.S4, V14.S4

	// Stage 2
	VADD V1.S4, V0.S4, V27.S4; VADD V9.S4, V8.S4, V28.S4
	VSUB V1.S4, V0.S4, V0.S4; VSUB V9.S4, V8.S4, V8.S4
	VADD V3.S4, V2.S4, V29.S4; VADD V11.S4, V10.S4, V30.S4
	MUL4S(26, 29, 29); MUL4S(26, 30, 30)
	MUL4S(21, 3, 31); MUL4S(21, 11, 16)
	VSUB V31.S4, V29.S4, V3.S4; VSUB V16.S4, V30.S4, V11.S4
	MUL4S(20, 2, 31); MUL4S(20, 10, 16)
	VADD V31.S4, V29.S4, V2.S4; VADD V16.S4, V30.S4, V10.S4

	// Stage 3
	VADD V5.S4, V4.S4, V1.S4; VADD V13.S4, V12.S4, V9.S4
	VSUB V5.S4, V4.S4, V4.S4; VSUB V13.S4, V12.S4, V12.S4
	VADD V6.S4, V7.S4, V5.S4; VADD V14.S4, V15.S4, V13.S4
	VSUB V6.S4, V7.S4, V7.S4; VSUB V14.S4, V15.S4, V15.S4

	// Stage 4
	VADD V2.S4, V27.S4, V6.S4; VADD V10.S4, V28.S4, V14.S4
	VSUB V2.S4, V27.S4, V27.S4; VSUB V10.S4, V28.S4, V28.S4
	VADD V3.S4, V0.S4, V2.S4; VADD V11.S4, V8.S4, V10.S4
	VSUB V3.S4, V0.S4, V0.S4; VSUB V11.S4, V8.S4, V8.S4

	// Rotation stage
	VADD V7.S4, V4.S4, V29.S4; VADD V15.S4, V12.S4, V30.S4
	MUL4S(23, 29, 29); MUL4S(23, 30, 30)
	VADD V22.S4, V29.S4, V3.S4; VADD V22.S4, V30.S4, V11.S4
	SSHR4S(8, 3, 3); SSHR4S(8, 11, 11)
	VSUB V7.S4, V4.S4, V31.S4; VSUB V15.S4, V12.S4, V16.S4
	MUL4S(23, 31, 31); MUL4S(23, 16, 16)
	VADD V22.S4, V31.S4, V4.S4; VADD V22.S4, V16.S4, V12.S4
	SSHR4S(8, 4, 4); SSHR4S(8, 12, 12)

	// Save variables, then final sums/diffs for Pass 1
	VMOV V6.B16, V16.B16; VMOV V14.B16, V17.B16
	VMOV V27.B16, V18.B16; VMOV V28.B16, V19.B16
	VMOV V5.B16, V20.B16; VMOV V13.B16, V21.B16
	VMOV V3.B16, V22.B16; VMOV V11.B16, V23.B16
	VMOV V2.B16, V24.B16; VMOV V10.B16, V25.B16
	VMOV V1.B16, V26.B16; VMOV V9.B16, V27.B16
	VMOV V4.B16, V28.B16; VMOV V12.B16, V29.B16
	VMOV V0.B16, V30.B16; VMOV V8.B16, V31.B16

	VADD V16.S4, V26.S4, V0.S4; VADD V17.S4, V27.S4, V1.S4
	VADD V24.S4, V22.S4, V2.S4; VADD V25.S4, V23.S4, V3.S4
	VADD V30.S4, V28.S4, V4.S4; VADD V31.S4, V29.S4, V5.S4
	VADD V18.S4, V20.S4, V6.S4; VADD V19.S4, V21.S4, V7.S4
	VSUB V20.S4, V18.S4, V8.S4; VSUB V21.S4, V19.S4, V9.S4
	VSUB V28.S4, V30.S4, V10.S4; VSUB V29.S4, V31.S4, V11.S4
	VSUB V22.S4, V24.S4, V12.S4; VSUB V23.S4, V25.S4, V13.S4
	VSUB V26.S4, V16.S4, V14.S4; VSUB V27.S4, V17.S4, V15.S4

	SSHR4S(8, 0, 0); SSHR4S(8, 1, 1); SSHR4S(8, 2, 2); SSHR4S(8, 3, 3)
	SSHR4S(8, 4, 4); SSHR4S(8, 5, 5); SSHR4S(8, 6, 6); SSHR4S(8, 7, 7)
	SSHR4S(8, 8, 8); SSHR4S(8, 9, 9); SSHR4S(8, 10, 10); SSHR4S(8, 11, 11)
	SSHR4S(8, 12, 12); SSHR4S(8, 13, 13); SSHR4S(8, 14, 14); SSHR4S(8, 15, 15)

	// ===== Transpose 2 =====
	VTRN1 V2.S4, V0.S4, V16.S4; VTRN2 V2.S4, V0.S4, V17.S4
	VTRN1 V6.S4, V4.S4, V18.S4; VTRN2 V6.S4, V4.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V0.D2; VTRN2 V18.D2, V16.D2, V2.D2
	VTRN1 V19.D2, V17.D2, V4.D2; VTRN2 V19.D2, V17.D2, V6.D2
	VTRN1 V3.S4, V1.S4, V16.S4; VTRN2 V3.S4, V1.S4, V17.S4
	VTRN1 V7.S4, V5.S4, V18.S4; VTRN2 V7.S4, V5.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V1.D2; VTRN2 V18.D2, V16.D2, V3.D2
	VTRN1 V19.D2, V17.D2, V5.D2; VTRN2 V19.D2, V17.D2, V7.D2
	VTRN1 V10.S4, V8.S4, V16.S4; VTRN2 V10.S4, V8.S4, V17.S4
	VTRN1 V14.S4, V12.S4, V18.S4; VTRN2 V14.S4, V12.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V8.D2; VTRN2 V18.D2, V16.D2, V10.D2
	VTRN1 V19.D2, V17.D2, V12.D2; VTRN2 V19.D2, V17.D2, V14.D2
	VTRN1 V11.S4, V9.S4, V16.S4; VTRN2 V11.S4, V9.S4, V17.S4
	VTRN1 V15.S4, V13.S4, V18.S4; VTRN2 V15.S4, V13.S4, V19.S4
	VTRN1 V18.D2, V16.D2, V9.D2; VTRN2 V18.D2, V16.D2, V11.D2
	VTRN1 V19.D2, V17.D2, V13.D2; VTRN2 V19.D2, V17.D2, V15.D2

	// ===== Pass 2 (Rows) =====
	LOAD_CONST_VEC(const_w1mw7, V16); LOAD_CONST_VEC(const_w1pw7, V17)
	LOAD_CONST_VEC(const_w3mw5, V18); LOAD_CONST_VEC(const_w3pw5, V19)
	LOAD_CONST_VEC(const_w2mw6, V20); LOAD_CONST_VEC(const_w2pw6, V21)
	LOAD_CONST_VEC(const_128, V22);   LOAD_CONST_VEC(const_rot, V23)
	LOAD_CONST_VEC(const_w7, V24);    LOAD_CONST_VEC(const_w3, V25); LOAD_CONST_VEC(const_w6, V26)
	LOAD_CONST_VEC(const_4, V31);     LOAD_CONST_VEC(const_8192, V28)

	VSHL $8, V0.S4, V0.S4; VSHL $8, V8.S4, V8.S4
	VADD V28.S4, V0.S4, V0.S4; VADD V28.S4, V8.S4, V8.S4
	VSHL $8, V1.S4, V1.S4; VSHL $8, V9.S4, V9.S4

	// Stage 1 (rounding >>3 inside)
	VADD V7.S4, V4.S4, V29.S4; VADD V15.S4, V12.S4, V30.S4
	MUL4S(24, 29, 27); MUL4S(24, 30, 28)
	VADD V31.S4, V27.S4, V27.S4; VADD V31.S4, V28.S4, V28.S4
	MUL4S(16, 4, 29); MUL4S(16, 12, 30) // (w1-w7)*x4
	VADD V29.S4, V27.S4, V4.S4; VADD V30.S4, V28.S4, V12.S4
	SSHR4S(3, 4, 4); SSHR4S(3, 12, 12)
	MUL4S(17, 7, 29); MUL4S(17, 15, 30) // (w1+w7)*x5
	VSUB V29.S4, V27.S4, V7.S4; VSUB V30.S4, V28.S4, V15.S4
	SSHR4S(3, 7, 7); SSHR4S(3, 15, 15)

	VADD V6.S4, V5.S4, V29.S4; VADD V14.S4, V13.S4, V30.S4
	MUL4S(25, 29, 27); MUL4S(25, 30, 28) // w3*(x6+x7)
	VADD V31.S4, V27.S4, V27.S4; VADD V31.S4, V28.S4, V28.S4
	MUL4S(18, 5, 29); MUL4S(18, 13, 30) // (w3-w5)*x6
	VSUB V29.S4, V27.S4, V5.S4; VSUB V30.S4, V28.S4, V13.S4
	SSHR4S(3, 5, 5); SSHR4S(3, 13, 13)
	MUL4S(19, 6, 29); MUL4S(19, 14, 30) // (w3+w5)*x7
	VSUB V29.S4, V27.S4, V6.S4; VSUB V30.S4, V28.S4, V14.S4
	SSHR4S(3, 6, 6); SSHR4S(3, 14, 14)

	// Stage 2
	VADD V1.S4, V0.S4, V27.S4; VADD V9.S4, V8.S4, V28.S4
	VSUB V1.S4, V0.S4, V0.S4; VSUB V9.S4, V8.S4, V8.S4
	VADD V3.S4, V2.S4, V29.S4; VADD V11.S4, V10.S4, V30.S4
	MUL4S(26, 29, 29); MUL4S(26, 30, 30) // w6*(x3+x2)
	VADD V31.S4, V29.S4, V29.S4; VADD V31.S4, V30.S4, V30.S4
	MUL4S(21, 3, 31); MUL4S(21, 11, 16) // (w2+w6)*x2
	VSUB V31.S4, V29.S4, V3.S4; VSUB V16.S4, V30.S4, V11.S4
	SSHR4S(3, 3, 3); SSHR4S(3, 11, 11)
	MUL4S(20, 2, 31); MUL4S(20, 10, 16) // (w2-w6)*x3
	VADD V31.S4, V29.S4, V2.S4; VADD V16.S4, V30.S4, V10.S4
	SSHR4S(3, 2, 2); SSHR4S(3, 10, 10)

	// Stages 3 & 4
	VADD V5.S4, V4.S4, V1.S4; VADD V13.S4, V12.S4, V9.S4
	VSUB V5.S4, V4.S4, V4.S4; VSUB V13.S4, V12.S4, V12.S4
	VADD V6.S4, V7.S4, V5.S4; VADD V14.S4, V15.S4, V13.S4
	VSUB V6.S4, V7.S4, V7.S4; VSUB V14.S4, V15.S4, V15.S4
	VADD V2.S4, V27.S4, V6.S4; VADD V10.S4, V28.S4, V14.S4
	VSUB V2.S4, V27.S4, V27.S4; VSUB V10.S4, V28.S4, V28.S4
	VADD V3.S4, V0.S4, V2.S4; VADD V11.S4, V8.S4, V10.S4
	VSUB V3.S4, V0.S4, V0.S4; VSUB V11.S4, V8.S4, V8.S4

	// Rotation
	VADD V7.S4, V4.S4, V29.S4; VADD V15.S4, V12.S4, V30.S4
	MUL4S(23, 29, 29); MUL4S(23, 30, 30) // 181*(x4+x5)
	VADD V22.S4, V29.S4, V3.S4; VADD V22.S4, V30.S4, V11.S4
	SSHR4S(8, 3, 3); SSHR4S(8, 11, 11) // >>8
	VSUB V7.S4, V4.S4, V31.S4; VSUB V15.S4, V12.S4, V16.S4
	MUL4S(23, 31, 31); MUL4S(23, 16, 16) // 181*(x4-x5)
	VADD V22.S4, V31.S4, V4.S4; VADD V22.S4, V16.S4, V12.S4
	SSHR4S(8, 4, 4); SSHR4S(8, 12, 12) // >>8

	// Save variables, then final sums/diffs for Pass 2
	VMOV V6.B16, V16.B16; VMOV V14.B16, V17.B16
	VMOV V27.B16, V18.B16; VMOV V28.B16, V19.B16
	VMOV V5.B16, V20.B16; VMOV V13.B16, V21.B16
	VMOV V3.B16, V22.B16; VMOV V11.B16, V23.B16
	VMOV V2.B16, V24.B16; VMOV V10.B16, V25.B16
	VMOV V1.B16, V26.B16; VMOV V9.B16, V27.B16
	VMOV V4.B16, V28.B16; VMOV V12.B16, V29.B16
	VMOV V0.B16, V30.B16; VMOV V8.B16, V31.B16

	VADD V16.S4, V26.S4, V0.S4; VADD V17.S4, V27.S4, V8.S4
	VADD V24.S4, V22.S4, V1.S4; VADD V25.S4, V23.S4, V9.S4
	VADD V30.S4, V28.S4, V2.S4; VADD V31.S4, V29.S4, V10.S4
	VADD V18.S4, V20.S4, V3.S4; VADD V19.S4, V21.S4, V11.S4
	VSUB V20.S4, V18.S4, V4.S4; VSUB V21.S4, V19.S4, V12.S4
	VSUB V22.S4, V24.S4, V6.S4; VSUB V23.S4, V25.S4, V14.S4
	VSUB V26.S4, V16.S4, V7.S4; VSUB V27.S4, V17.S4, V15.S4
	VSUB V28.S4, V30.S4, V5.S4; VSUB V29.S4, V31.S4, V13.S4

	// Final shift (>> 14).
	SSHR4S(14, 0, 0); SSHR4S(14, 1, 1); SSHR4S(14, 2, 2); SSHR4S(14, 3, 3)
	SSHR4S(14, 4, 4); SSHR4S(14, 5, 5); SSHR4S(14, 6, 6); SSHR4S(14, 7, 7)
	SSHR4S(14, 8, 8); SSHR4S(14, 9, 9); SSHR4S(14, 10, 10); SSHR4S(14, 11, 11)
	SSHR4S(14, 12, 12); SSHR4S(14, 13, 13); SSHR4S(14, 14, 14); SSHR4S(14, 15, 15)

	// Add level shift (+128) to all results.
	LOAD_CONST_VEC(const_128, V17)
	VADD V17.S4, V0.S4, V0.S4; VADD V17.S4, V1.S4, V1.S4
	VADD V17.S4, V2.S4, V2.S4; VADD V17.S4, V3.S4, V3.S4
	VADD V17.S4, V4.S4, V4.S4; VADD V17.S4, V5.S4, V5.S4
	VADD V17.S4, V6.S4, V6.S4; VADD V17.S4, V7.S4, V7.S4
	VADD V17.S4, V8.S4, V8.S4; VADD V17.S4, V9.S4, V9.S4
	VADD V17.S4, V10.S4, V10.S4; VADD V17.S4, V11.S4, V11.S4
	VADD V17.S4, V12.S4, V12.S4; VADD V17.S4, V13.S4, V13.S4
	VADD V17.S4, V14.S4, V14.S4; VADD V17.S4, V15.S4, V15.S4

	// --- Pack and Store ---
	// Step 1: Pack 32-bit signed int -> 16-bit unsigned int (with saturation).
	// We use UQXTN instead of SQXTN to handle negative value saturation correctly in the next step.
	UQXTN4H(0, 16); UQXTN2_8H(8, 16)
	UQXTN4H(4, 17); UQXTN2_8H(12, 17)
	UQXTN4H(2, 18); UQXTN2_8H(10, 18)
	UQXTN4H(6, 19); UQXTN2_8H(14, 19)
	UQXTN4H(1, 20); UQXTN2_8H(9, 20)
	UQXTN4H(5, 21); UQXTN2_8H(13, 21)
	UQXTN4H(3, 22); UQXTN2_8H(11, 22)
	UQXTN4H(7, 23); UQXTN2_8H(15, 23)

	// Step 2: Pack 16-bit signed int -> 8-bit unsigned byte (with saturation).
	// V0 = [R1, R0], V1 = [R3, R2], V2 = [R5, R4], V3 = [R7, R6]
	SQXTUN8B(16, 0); SQXTUN2_16B(20, 0)
	SQXTUN8B(18, 1); SQXTUN2_16B(22, 1)
	SQXTUN8B(17, 2); SQXTUN2_16B(21, 2)
	SQXTUN8B(19, 3); SQXTUN2_16B(23, 3)

	// Step 3: Store
	CMP $8, R3
	BEQ store_stride8

store_strided:
	VMOV V0.D[0], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 0
	VMOV V0.D[1], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 1
	VMOV V1.D[0], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 2
	VMOV V1.D[1], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 3
	VMOV V2.D[0], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 4
	VMOV V2.D[1], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 5
	VMOV V3.D[0], R5; MOVD R5, (R1); ADD R3, R1, R1 // Row 6
	VMOV V3.D[1], R5; MOVD R5, (R1)                 // Row 7
	JMP  done

store_stride8:
	VMOV V0.D[0], R5; VMOV V0.D[1], R6; MOVD R5, (R1); MOVD R6, 8(R1)    // R0, R1
	VMOV V1.D[0], R5; VMOV V1.D[1], R6; MOVD R5, 16(R1); MOVD R6, 24(R1) // R2, R3
	VMOV V2.D[0], R5; VMOV V2.D[1], R6; MOVD R5, 32(R1); MOVD R6, 40(R1) // R4, R5
	VMOV V3.D[0], R5; VMOV V3.D[1], R6; MOVD R5, 48(R1); MOVD R6, 56(R1) // R6, R7
	JMP  done

dc_only_path:
	// Simplified calculation for DC-only blocks: clip(((in[0] << 3) + 32) >> 6 + 128).
	LSL $3, R4, R4
	ADD $32, R4, R4
	ASR $6, R4, R4
	ADD $128, R4, R4

	// Clip to [0, 255].
	MOVD $0, R5
	MOVD $255, R6
	CMP  $0, R4
	CSEL LT, R5, R4, R4
	CMP  $255, R4
	CSEL GT, R6, R4, R4

	// Broadcast and store
	VDUP R4, V0.B16
	CMP  $8, R3
	BNE  dc_store_strided

dc_store_stride8:
	MOVD   R1, R8
	VST1.P [V0.B16], 16(R8)
	VST1.P [V0.B16], 16(R8)
	VST1.P [V0.B16], 16(R8)
	VST1   [V0.B16], (R8)
	JMP    done

dc_store_strided:
	VMOV V0.D[0], R5
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1); ADD R3, R1, R1
	MOVD R5, (R1)

done:
	RET
