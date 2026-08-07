//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 quantization, bit-identical to quantizeBlockScalar. VPMULDQ multiplies
// only the even 32-bit lanes, so the odd lanes are shifted down, multiplied,
// and blended back.

DATA qmax<>+0(SB)/4, $1023
DATA qmax<>+4(SB)/4, $1023
DATA qmax<>+8(SB)/4, $1023
DATA qmax<>+12(SB)/4, $1023
DATA qmax<>+16(SB)/4, $1023
DATA qmax<>+20(SB)/4, $1023
DATA qmax<>+24(SB)/4, $1023
DATA qmax<>+28(SB)/4, $1023
GLOBL qmax<>(SB), RODATA|NOPTR, $32

// func quantizeAVX2(dst, src, recip, half *[64]int32) uint64
TEXT ·quantizeAVX2(SB), NOSPLIT, $0-40
	MOVQ dst+0(FP), DI
	MOVQ src+8(FP), SI
	MOVQ recip+16(FP), DX
	MOVQ half+24(FP), BX

	VMOVDQU qmax<>(SB), Y7
	VPXOR   Y8, Y8, Y8
	XORQ    R8, R8
	XORQ    R9, R9
	MOVQ    $8, R11

loop:
	VMOVDQU (SI), Y0
	VPSRAD  $31, Y0, Y1
	VPABSD  Y0, Y2
	VPADDD  (BX), Y2, Y2

	VMOVDQU (DX), Y3
	VPMULDQ Y3, Y2, Y4

	VPSRLQ  $32, Y2, Y5
	VPSRLQ  $32, Y3, Y6
	VPMULDQ Y6, Y5, Y5

	VPSRLQ   $31, Y4, Y4
	VPSRLQ   $31, Y5, Y5
	VPSLLQ   $32, Y5, Y5
	VPBLENDD $0xAA, Y5, Y4, Y4

	VPMINSD Y7, Y4, Y4
	VPXOR   Y1, Y4, Y4
	VPSUBD  Y1, Y4, Y4

	VMOVDQU Y4, (DI)

	// Record which lanes are non-zero.
	VPCMPEQD  Y8, Y4, Y9
	VMOVMSKPS Y9, AX
	NOTL      AX
	ANDL      $0xFF, AX
	MOVQ      AX, R10
	MOVQ      R9, CX
	SHLQ      CL, R10
	ORQ       R10, R8
	ADDQ      $8, R9

	ADDQ $32, SI
	ADDQ $32, DX
	ADDQ $32, BX
	ADDQ $32, DI
	DECQ R11
	JNZ  loop

	MOVQ R8, ret+32(FP)

	VZEROUPPER
	RET
