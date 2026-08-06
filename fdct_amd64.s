//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 8x8 FDCT, one row per YMM register, bit-identical to fdctScalar.

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

CONST_YMM(fc2446, 2446)
CONST_YMM(fc3196, 3196)
CONST_YMM(fc4433, 4433)
CONST_YMM(fc6270, 6270)
CONST_YMM(fc7373, 7373)
CONST_YMM(fc9633, 9633)
CONST_YMM(fc12299, 12299)
CONST_YMM(fc15137, 15137)
CONST_YMM(fc16069, 16069)
CONST_YMM(fc16819, 16819)
CONST_YMM(fc20995, 20995)
CONST_YMM(fc25172, 25172)

CONST_YMM(fc128, 128)
CONST_YMM(fcr2, 2)
CONST_YMM(fcr1024, 1024)
CONST_YMM(fcr16384, 16384)

// Transpose an 8x8 block of dwords from Y0-Y7 into Y8-Y15, clobbering Y0-Y7.
#define TRANSPOSE_8X8() \
	VPUNPCKLDQ Y1, Y0, Y8; VPUNPCKLDQ Y3, Y2, Y10;            \
	VPUNPCKLDQ Y5, Y4, Y12; VPUNPCKLDQ Y7, Y6, Y14;           \
	VPUNPCKHDQ Y1, Y0, Y9; VPUNPCKHDQ Y3, Y2, Y11;            \
	VPUNPCKHDQ Y5, Y4, Y13; VPUNPCKHDQ Y7, Y6, Y15;           \
	VPUNPCKLQDQ Y10, Y8, Y0; VPUNPCKHQDQ Y10, Y8, Y1;         \
	VPUNPCKLQDQ Y11, Y9, Y2; VPUNPCKHQDQ Y11, Y9, Y3;         \
	VPUNPCKLQDQ Y14, Y12, Y4; VPUNPCKHQDQ Y14, Y12, Y5;       \
	VPUNPCKLQDQ Y15, Y13, Y6; VPUNPCKHQDQ Y15, Y13, Y7;       \
	VPERM2F128 $0x20, Y4, Y0, Y8; VPERM2F128 $0x20, Y5, Y1, Y9;    \
	VPERM2F128 $0x20, Y6, Y2, Y10; VPERM2F128 $0x20, Y7, Y3, Y11;  \
	VPERM2F128 $0x31, Y4, Y0, Y12; VPERM2F128 $0x31, Y5, Y1, Y13;  \
	VPERM2F128 $0x31, Y6, Y2, Y14; VPERM2F128 $0x31, Y7, Y3, Y15

// Odd part. In Y8-Y15 = b0-b7. Out Y1/Y3/Y5/Y7 = out1/3/5/7, Y8-Y11 = tmp10/13/11/12.
#define FDCT_ODD(RND, SH) \
	VPADDD Y15, Y8, Y0;                     \
	VPSUBD Y15, Y8, Y7;                     \
	VPADDD Y14, Y9, Y1;                     \
	VPSUBD Y14, Y9, Y6;                     \
	VPADDD Y13, Y10, Y2;                    \
	VPSUBD Y13, Y10, Y5;                    \
	VPADDD Y12, Y11, Y3;                    \
	VPSUBD Y12, Y11, Y4;                    \
	VPADDD Y3, Y0, Y8;                      \
	VPSUBD Y3, Y0, Y9;                      \
	VPADDD Y2, Y1, Y10;                     \
	VPSUBD Y2, Y1, Y11;                     \
	VPADDD Y7, Y4, Y12;                     \
	VPADDD Y6, Y5, Y13;                     \
	VPADDD Y6, Y4, Y14;                     \
	VPADDD Y7, Y5, Y15;                     \
	VPADDD Y15, Y14, Y0;                    \
	VPMULLD fc9633<>(SB), Y0, Y0;           \
	VPMULLD fc2446<>(SB), Y4, Y4;           \
	VPMULLD fc16819<>(SB), Y5, Y5;          \
	VPMULLD fc25172<>(SB), Y6, Y6;          \
	VPMULLD fc12299<>(SB), Y7, Y7;          \
	VPMULLD fc7373<>(SB), Y12, Y12;         \
	VPMULLD fc20995<>(SB), Y13, Y13;        \
	VPMULLD fc16069<>(SB), Y14, Y14;        \
	VPMULLD fc3196<>(SB), Y15, Y15;         \
	VPSUBD Y14, Y0, Y14;                    \
	VPSUBD Y15, Y0, Y15;                    \
	VPSUBD Y12, Y4, Y4; VPADDD Y14, Y4, Y4; \
	VPSUBD Y13, Y5, Y5; VPADDD Y15, Y5, Y5; \
	VPSUBD Y13, Y6, Y6; VPADDD Y14, Y6, Y6; \
	VPSUBD Y12, Y7, Y7; VPADDD Y15, Y7, Y7; \
	VPADDD RND(SB), Y4, Y4; VPSRAD $SH, Y4, Y4; \
	VPADDD RND(SB), Y5, Y5; VPSRAD $SH, Y5, Y5; \
	VPADDD RND(SB), Y6, Y6; VPSRAD $SH, Y6, Y6; \
	VPADDD RND(SB), Y7, Y7; VPSRAD $SH, Y7, Y7; \
	VMOVDQA Y7, Y1;                         \
	VMOVDQA Y6, Y3;                         \
	VMOVDQA Y4, Y7

// Even part. In Y8-Y11 = tmp10/13/11/12. Out Y0/Y2/Y4/Y6 = out0/2/4/6.
#define FDCT_EVEN(RND, SH) \
	VPADDD Y10, Y8, Y0;                     \
	VPSUBD Y10, Y8, Y4;                     \
	VPADDD Y9, Y11, Y12;                    \
	VPMULLD fc4433<>(SB), Y12, Y12;         \
	VPMULLD fc6270<>(SB), Y9, Y2;           \
	VPADDD Y12, Y2, Y2;                     \
	VPADDD RND(SB), Y2, Y2; VPSRAD $SH, Y2, Y2; \
	VPMULLD fc15137<>(SB), Y11, Y6;         \
	VPSUBD Y6, Y12, Y6;                     \
	VPADDD RND(SB), Y6, Y6; VPSRAD $SH, Y6, Y6

// func fdctAVX2(blk *[64]int32)
TEXT ·fdctAVX2(SB), NOSPLIT, $0-24
	MOVQ blk+0(FP), SI
	MOVQ src+8(FP), DI
	MOVQ stride+16(FP), CX

	// Load bytes, zero extend to dwords and level shift.
	VMOVDQU fc128<>(SB), Y8
	VPMOVZXBD (DI), Y0
	VPSUBD Y8, Y0, Y0
	ADDQ CX, DI
	VPMOVZXBD (DI), Y1
	VPSUBD Y8, Y1, Y1
	ADDQ CX, DI
	VPMOVZXBD (DI), Y2
	VPSUBD Y8, Y2, Y2
	ADDQ CX, DI
	VPMOVZXBD (DI), Y3
	VPSUBD Y8, Y3, Y3
	ADDQ CX, DI
	VPMOVZXBD (DI), Y4
	VPSUBD Y8, Y4, Y4
	ADDQ CX, DI
	VPMOVZXBD (DI), Y5
	VPSUBD Y8, Y5, Y5
	ADDQ CX, DI
	VPMOVZXBD (DI), Y6
	VPSUBD Y8, Y6, Y6
	ADDQ CX, DI
	VPMOVZXBD (DI), Y7
	VPSUBD Y8, Y7, Y7

	TRANSPOSE_8X8()

	// Pass 1: the even outputs are scaled up by PASS1_BITS instead of descaled.
	FDCT_ODD(fcr1024<>, 11)
	FDCT_EVEN(fcr1024<>, 11)
	VPSLLD $2, Y0, Y0
	VPSLLD $2, Y4, Y4

	TRANSPOSE_8X8()

	// Pass 2 removes the PASS1_BITS scaling.
	FDCT_ODD(fcr16384<>, 15)
	FDCT_EVEN(fcr16384<>, 15)
	VPADDD fcr2<>(SB), Y0, Y0
	VPSRAD $2, Y0, Y0
	VPADDD fcr2<>(SB), Y4, Y4
	VPSRAD $2, Y4, Y4

	VMOVDQU Y0, 0(SI)
	VMOVDQU Y1, 32(SI)
	VMOVDQU Y2, 64(SI)
	VMOVDQU Y3, 96(SI)
	VMOVDQU Y4, 128(SI)
	VMOVDQU Y5, 160(SI)
	VMOVDQU Y6, 192(SI)
	VMOVDQU Y7, 224(SI)

	VZEROUPPER
	RET
