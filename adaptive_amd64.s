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

// Load lut[row[OFF]] into lane LANE of X7. Clobbers AX.
#define GAMMA(off, lane) \
	MOVBLZX off(SI), AX; \
	PINSRD  lane, (R8)(AX*4), X7

// func preErosionSSE(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)
TEXT ·preErosionSSE(SB), NOSPLIT, $0-52
	MOVQ dst+0(FP), DI
	MOVQ row+8(FP), SI
	MOVQ rowT+16(FP), DX
	MOVQ rowB+24(FP), BX
	MOVQ lut+32(FP), R8
	MOVQ n+40(FP), CX

	MOVL   pelimit<>(SB), X10
	PSHUFD $0, X10, X10
	MOVL   pemul<>(SB), X11
	PSHUFD $0, X11, X11
	MOVL   peoff<>(SB), X12
	PSHUFD $0, X12, X12
	MOVL   pequarter<>(SB), X13
	PSHUFD $0, X13, X13
	MOVL   acc+48(FP), X14
	PSHUFD $0, X14, X14

sse_loop:
	PMOVZXBD (SI), X0
	PMOVZXBD -1(SI), X1
	PMOVZXBD 1(SI), X2
	PMOVZXBD (DX), X3
	PMOVZXBD (BX), X4

	MOVOU X0, X5
	PSLLL $2, X5
	PSUBL X1, X5
	PSUBL X2, X5
	PSUBL X3, X5
	PSUBL X4, X5

	CVTPL2PS X5, X5

	MOVBLZX 0(SI), AX
	MOVL    (R8)(AX*4), X7
	GAMMA(1, $1)
	GAMMA(2, $2)
	GAMMA(3, $3)

	MULPS  X7, X5
	MULPS  X5, X5
	MINPS  X10, X5
	MULPS  X11, X5
	ADDPS  X12, X5
	SQRTPS X5, X5
	MULPS  X13, X5

	MOVUPS (DI), X8
	ANDPS  X14, X8
	ADDPS  X8, X5

	MOVUPS X5, (DI)

	ADDQ $4, SI
	ADDQ $4, DX
	ADDQ $4, BX
	ADDQ $16, DI
	SUBQ $4, CX
	JNZ  sse_loop

	RET
