//go:build arm64 && !noasm

#include "textflag.h"

// NEON pre-erosion row, bit-identical to preErosionRowScalar.

#define SSHLL4S(n, d)     WORD $(0x0F10A400 | ((n) << 5) | (d))
#define SSHLL2_4S(n, d)   WORD $(0x4F10A400 | ((n) << 5) | (d))
#define SCVTF4S(n, d)     WORD $(0x4E21D800 | ((n) << 5) | (d))
#define FMUL4S(m, n, d)   WORD $(0x6E20DC00 | ((m) << 16) | ((n) << 5) | (d))
#define FADD4S(m, n, d)   WORD $(0x4E20D400 | ((m) << 16) | ((n) << 5) | (d))
#define FMIN4S(m, n, d)   WORD $(0x4EA0F400 | ((m) << 16) | ((n) << 5) | (d))
#define FSQRT4S(n, d)     WORD $(0x6EA1F800 | ((n) << 5) | (d))

// LUT loads one gamma weight per lane, since NEON has no gather.
#define GAMMA(off, lane, d)   \
	MOVBU off(R1), R9;        \
	MOVW  (R4)(R9<<2), R10;   \
	VMOV  R10, d.S[lane]

// func preErosionNEON(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)
TEXT ·preErosionNEON(SB), NOSPLIT, $0-52
	MOVD dst+0(FP), R0
	MOVD row+8(FP), R1
	MOVD rowT+16(FP), R2
	MOVD rowB+24(FP), R3
	MOVD lut+32(FP), R4
	MOVD n+40(FP), R5
	MOVWU acc+48(FP), R6

	SUB $1, R1, R7
	ADD $1, R1, R8

	MOVD $0x3e4ccccd, R11
	VDUP R11, V10.S4
	MOVD $0x480e0641, R11
	VDUP R11, V11.S4
	MOVD $0x41e00000, R11
	VDUP R11, V12.S4
	MOVD $0x3e800000, R11
	VDUP R11, V13.S4
	VDUP R6, V14.S4

loop:
	VLD1 (R1), [V0.B8]
	VLD1 (R7), [V1.B8]
	VLD1 (R8), [V2.B8]
	VLD1 (R2), [V3.B8]
	VLD1 (R3), [V4.B8]

	VUXTL V0.B8, V0.H8
	VUXTL V1.B8, V1.H8
	VUXTL V2.B8, V2.H8
	VUXTL V3.B8, V3.H8
	VUXTL V4.B8, V4.H8

	VSHL $2, V0.H8, V5.H8
	VSUB V1.H8, V5.H8, V5.H8
	VSUB V2.H8, V5.H8, V5.H8
	VSUB V3.H8, V5.H8, V5.H8
	VSUB V4.H8, V5.H8, V5.H8

	SSHLL4S(5, 6)
	SSHLL2_4S(5, 7)
	SCVTF4S(6, 6)
	SCVTF4S(7, 7)

	GAMMA(0, 0, V8)
	GAMMA(1, 1, V8)
	GAMMA(2, 2, V8)
	GAMMA(3, 3, V8)
	GAMMA(4, 0, V9)
	GAMMA(5, 1, V9)
	GAMMA(6, 2, V9)
	GAMMA(7, 3, V9)

	FMUL4S(8, 6, 6)
	FMUL4S(6, 6, 6)
	FMIN4S(10, 6, 6)
	FMUL4S(11, 6, 6)
	FADD4S(12, 6, 6)
	FSQRT4S(6, 6)
	FMUL4S(13, 6, 6)

	FMUL4S(9, 7, 7)
	FMUL4S(7, 7, 7)
	FMIN4S(10, 7, 7)
	FMUL4S(11, 7, 7)
	FADD4S(12, 7, 7)
	FSQRT4S(7, 7)
	FMUL4S(13, 7, 7)

	VLD1 (R0), [V15.S4, V16.S4]
	VAND V14.B16, V15.B16, V15.B16
	VAND V14.B16, V16.B16, V16.B16
	FADD4S(15, 6, 6)
	FADD4S(16, 7, 7)

	VST1 [V6.S4, V7.S4], (R0)

	ADD $8, R1
	ADD $8, R2
	ADD $8, R3
	ADD $8, R7
	ADD $8, R8
	ADD $32, R0
	SUB $8, R5
	CBNZ R5, loop

	RET
