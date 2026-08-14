//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 2x2 chroma box filter, bit-identical to downsampleRow2x2Scalar.
// VPMADDUBSW against ones sums adjacent byte pairs into 16-bit lanes.

DATA dsone<>+0(SB)/8, $0x0101010101010101
DATA dsone<>+8(SB)/8, $0x0101010101010101
DATA dsone<>+16(SB)/8, $0x0101010101010101
DATA dsone<>+24(SB)/8, $0x0101010101010101
GLOBL dsone<>(SB), RODATA|NOPTR, $32

DATA dstwo<>+0(SB)/8, $0x0002000200020002
DATA dstwo<>+8(SB)/8, $0x0002000200020002
DATA dstwo<>+16(SB)/8, $0x0002000200020002
DATA dstwo<>+24(SB)/8, $0x0002000200020002
GLOBL dstwo<>(SB), RODATA|NOPTR, $32

// func downsampleRow2x2AVX2(dst, src0, src1 *byte, n int)
TEXT ·downsampleRow2x2AVX2(SB), NOSPLIT, $0-32
	MOVQ dst+0(FP), DI
	MOVQ src0+8(FP), SI
	MOVQ src1+16(FP), DX
	MOVQ n+24(FP), CX

	SHRQ $4, CX
	JZ   done

	VMOVDQU dsone<>(SB), Y2
	VMOVDQU dstwo<>(SB), Y3

loop:
	VMOVDQU (SI), Y0
	VMOVDQU (DX), Y1

	VPMADDUBSW Y2, Y0, Y0
	VPMADDUBSW Y2, Y1, Y1

	VPADDW Y1, Y0, Y0
	VPADDW Y3, Y0, Y0
	VPSRLW $2, Y0, Y0

	VPACKUSWB Y0, Y0, Y0
	VPERMQ    $0xD8, Y0, Y0
	VMOVDQU   X0, (DI)

	ADDQ $32, SI
	ADDQ $32, DX
	ADDQ $16, DI
	DECQ CX
	JNZ  loop

	VZEROUPPER

done:
	RET
