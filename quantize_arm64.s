//go:build arm64 && !noasm

#include "textflag.h"

// NEON quantization, bit-identical to quantizeBlockScalar. SSHR and SMULL are
// missing from the assembler, so the macros below encode them.

#define SSHR4S(k, n, d)    WORD $(0x4F000400 | ((64 - (k)) << 16) | ((n) << 5) | (d))
#define SMULL2D(m, n, d)   WORD $(0x0EA0C000 | ((m) << 16) | ((n) << 5) | (d))
#define SMULL2_2D(m, n, d) WORD $(0x4EA0C000 | ((m) << 16) | ((n) << 5) | (d))

// func quantizeNEON(dst, src, recip, half *[64]int32)
TEXT ·quantizeNEON(SB), NOSPLIT, $0-32
	MOVD dst+0(FP), R0
	MOVD src+8(FP), R1
	MOVD recip+16(FP), R2
	MOVD half+24(FP), R3
	MOVD $1023, R5
	VDUP R5, V30.S4
	MOVD $8, R4

loop:
	VLD1.P 16(R1), [V0.S4]
	SSHR4S(31, 0, 1)
	VEOR V1.B16, V0.B16, V2.B16
	VSUB V1.S4, V2.S4, V2.S4
	VLD1.P 16(R3), [V3.S4]
	VADD V3.S4, V2.S4, V2.S4
	VLD1.P 16(R2), [V3.S4]
	SMULL2D(3, 2, 4)
	SMULL2_2D(3, 2, 5)
	VUSHR $31, V4.D2, V4.D2
	VUSHR $31, V5.D2, V5.D2
	VUZP1 V5.S4, V4.S4, V6.S4
	VUMIN V30.S4, V6.S4, V6.S4
	VEOR V1.B16, V6.B16, V6.B16
	VSUB V1.S4, V6.S4, V6.S4
	VST1.P [V6.S4], 16(R0)
	VLD1.P 16(R1), [V7.S4]
	SSHR4S(31, 7, 8)
	VEOR V8.B16, V7.B16, V9.B16
	VSUB V8.S4, V9.S4, V9.S4
	VLD1.P 16(R3), [V10.S4]
	VADD V10.S4, V9.S4, V9.S4
	VLD1.P 16(R2), [V10.S4]
	SMULL2D(10, 9, 11)
	SMULL2_2D(10, 9, 12)
	VUSHR $31, V11.D2, V11.D2
	VUSHR $31, V12.D2, V12.D2
	VUZP1 V12.S4, V11.S4, V13.S4
	VUMIN V30.S4, V13.S4, V13.S4
	VEOR V8.B16, V13.B16, V13.B16
	VSUB V8.S4, V13.S4, V13.S4
	VST1.P [V13.S4], 16(R0)

	SUB $1, R4, R4
	CBNZ R4, loop

	RET
