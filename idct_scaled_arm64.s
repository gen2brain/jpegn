//go:build arm64 && !noasm

#include "textflag.h"

// NEON 4-point inverse DCT for 1/2 scaling, bit-identical to idct8x8To4x4.
// Integer multiply, the rounding shift and the narrowing saturations are all
// missing from the assembler, so the macros below encode them.

#define MUL4S(m, n, d)   WORD $(0x4EA09C00 | ((m) << 16) | ((n) << 5) | (d))
#define SSHR4S(k, n, d)  WORD $(0x4F000400 | ((64 - (k)) << 16) | ((n) << 5) | (d))
#define SQXTN4H(n, d)    WORD $(0x0E614800 | ((n) << 5) | (d))
#define SQXTUN8B(n, d)   WORD $(0x2E212800 | ((n) << 5) | (d))

DATA sk<>+0(SB)/4, $10703
DATA sk<>+4(SB)/4, $4433
GLOBL sk<>(SB), RODATA|NOPTR, $8

// Transpose the four dword rows V0-V3 into V16-V19, clobbering V4-V7.
#define TRANSPOSE4() \
	VTRN1 V1.S4, V0.S4, V4.S4;    \
	VTRN2 V1.S4, V0.S4, V5.S4;    \
	VTRN1 V3.S4, V2.S4, V6.S4;    \
	VTRN2 V3.S4, V2.S4, V7.S4;    \
	VZIP1 V6.D2, V4.D2, V16.D2;   \
	VZIP1 V7.D2, V5.D2, V17.D2;   \
	VZIP2 V6.D2, V4.D2, V18.D2;   \
	VZIP2 V7.D2, V5.D2, V19.D2

// One 4-point inverse DCT pass from V16-V19 into V0-V3. The rounding term is
// added before the shift, not folded into a rounding shift, so the int32
// wraparound at extreme coefficients matches the pure Go reference exactly.
#define PASS(rnd, sh) \
	VADD V18.S4, V16.S4, V8.S4;   \
	VSUB V18.S4, V16.S4, V9.S4;   \
	VSHL $13, V8.S4, V8.S4;       \
	VSHL $13, V9.S4, V9.S4;       \
	MUL4S(14, 17, 10);            \
	MUL4S(15, 19, 11);            \
	VADD V11.S4, V10.S4, V10.S4;  \
	MUL4S(15, 17, 12);            \
	MUL4S(14, 19, 13);            \
	VSUB V13.S4, V12.S4, V12.S4;  \
	VADD V10.S4, V8.S4, V0.S4;    \
	VADD V12.S4, V9.S4, V1.S4;    \
	VSUB V12.S4, V9.S4, V2.S4;    \
	VSUB V10.S4, V8.S4, V3.S4;    \
	VADD rnd.S4, V0.S4, V0.S4;    \
	VADD rnd.S4, V1.S4, V1.S4;    \
	VADD rnd.S4, V2.S4, V2.S4;    \
	VADD rnd.S4, V3.S4, V3.S4;    \
	SSHR4S(sh, 0, 0);             \
	SSHR4S(sh, 1, 1);             \
	SSHR4S(sh, 2, 2);             \
	SSHR4S(sh, 3, 3)

// Clamp one row to bytes and store it.
#define STOREROW(r, n) \
	VADD V20.S4, r.S4, r.S4; \
	SQXTN4H(n, n);           \
	SQXTUN8B(n, n);          \
	VMOV r.S[0], R5;         \
	MOVW R5, (R1)

// func idct4x4NEON(blk *[64]int32, out *byte, stride int)
TEXT ·idct4x4NEON(SB), NOSPLIT, $0-24
	MOVD blk+0(FP), R0
	MOVD out+8(FP), R1
	MOVD stride+16(FP), R2

	MOVD $sk<>(SB), R3
	VLD1 (R3), [V28.S2]
	VDUP V28.S[0], V14.S4
	VDUP V28.S[1], V15.S4

	MOVD $128, R5
	VDUP R5, V20.S4
	MOVD $1048576, R5
	VDUP R5, V21.S4

	VLD1.P 16(R0), [V0.S4]
	ADD    $16, R0, R0
	VLD1.P 16(R0), [V1.S4]
	ADD    $16, R0, R0
	VLD1.P 16(R0), [V2.S4]
	ADD    $16, R0, R0
	VLD1.P 16(R0), [V3.S4]

	TRANSPOSE4()
	PASS(V20, 8)
	TRANSPOSE4()
	PASS(V21, 21)

	STOREROW(V0, 0)
	ADD R2, R1, R1
	STOREROW(V1, 1)
	ADD R2, R1, R1
	STOREROW(V2, 2)
	ADD R2, R1, R1
	STOREROW(V3, 3)

	RET
