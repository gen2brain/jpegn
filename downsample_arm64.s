//go:build arm64 && !noasm

#include "textflag.h"

// NEON 2x2 chroma box filter, bit-identical to downsampleRow2x2Scalar. UADDLP
// sums adjacent byte pairs into 16-bit lanes; Go's arm64 assembler lacks it.

#define UADDLP8H(n, d) WORD $(0x6E202800 | ((n) << 5) | (d))

// func downsampleRow2x2NEON(dst, src0, src1 *byte, n int)
TEXT ·downsampleRow2x2NEON(SB), NOSPLIT, $0-32
	MOVD dst+0(FP), R0
	MOVD src0+8(FP), R1
	MOVD src1+16(FP), R2
	MOVD n+24(FP), R3

	LSR  $4, R3, R3
	CBZ  R3, done

	MOVD $2, R4
	VDUP R4, V20.H8

loop:
	VLD1.P 32(R1), [V0.B16, V1.B16]
	VLD1.P 32(R2), [V2.B16, V3.B16]

	UADDLP8H(0, 0)
	UADDLP8H(1, 1)
	UADDLP8H(2, 2)
	UADDLP8H(3, 3)

	VADD V2.H8, V0.H8, V0.H8
	VADD V3.H8, V1.H8, V1.H8
	VADD V20.H8, V0.H8, V0.H8
	VADD V20.H8, V1.H8, V1.H8

	VUSHR $2, V0.H8, V0.H8
	VUSHR $2, V1.H8, V1.H8

	VUZP1  V1.B16, V0.B16, V0.B16
	VST1.P [V0.B16], 16(R0)

	SUB  $1, R3, R3
	CBNZ R3, loop

done:
	RET
