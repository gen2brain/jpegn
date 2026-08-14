//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 pre-erosion row, bit-identical to preErosionRowScalar.

DATA pelimit<>+0(SB)/4, $0x3e4ccccd  // 0.2
GLOBL pelimit<>(SB), RODATA|NOPTR, $4

DATA pemul<>+0(SB)/4, $0x480e0641    // 145433.015625
GLOBL pemul<>(SB), RODATA|NOPTR, $4

DATA peoff<>+0(SB)/4, $0x41e00000    // 28
GLOBL peoff<>(SB), RODATA|NOPTR, $4

DATA pequarter<>+0(SB)/4, $0x3e800000 // 0.25
GLOBL pequarter<>(SB), RODATA|NOPTR, $4

// func preErosionAVX2(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)
TEXT ·preErosionAVX2(SB), NOSPLIT, $0-52
	MOVQ dst+0(FP), DI
	MOVQ row+8(FP), SI
	MOVQ rowT+16(FP), DX
	MOVQ rowB+24(FP), BX
	MOVQ lut+32(FP), R8
	MOVQ n+40(FP), CX

	VBROADCASTSS pelimit<>(SB), Y10
	VBROADCASTSS pemul<>(SB), Y11
	VBROADCASTSS peoff<>(SB), Y12
	VBROADCASTSS pequarter<>(SB), Y13
	VBROADCASTSS acc+48(FP), Y14

loop:
	VPMOVZXBD (SI), Y0
	VPMOVZXBD -1(SI), Y1
	VPMOVZXBD 1(SI), Y2
	VPMOVZXBD (DX), Y3
	VPMOVZXBD (BX), Y4

	VPSLLD $2, Y0, Y5
	VPSUBD Y1, Y5, Y5
	VPSUBD Y2, Y5, Y5
	VPSUBD Y3, Y5, Y5
	VPSUBD Y4, Y5, Y5

	VCVTDQ2PS Y5, Y5

	VPCMPEQD    Y6, Y6, Y6
	VGATHERDPS  Y6, (R8)(Y0*4), Y7

	VMULPS Y7, Y5, Y5
	VMULPS Y5, Y5, Y5
	VMINPS Y10, Y5, Y5
	VMULPS Y11, Y5, Y5
	VADDPS Y12, Y5, Y5
	VSQRTPS Y5, Y5
	VMULPS Y13, Y5, Y5

	VMOVUPS (DI), Y8
	VANDPS  Y14, Y8, Y8
	VADDPS  Y8, Y5, Y5

	VMOVUPS Y5, (DI)

	ADDQ $8, SI
	ADDQ $8, DX
	ADDQ $8, BX
	ADDQ $32, DI
	SUBQ $8, CX
	JNZ  loop

	VZEROUPPER
	RET
