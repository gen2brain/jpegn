//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 8x8 FDCT, one row per YMM register, bit-identical to fdctScalar.
// Every intermediate fits int16, so pairs are packed into 32-bit lanes and the
// rotations become VPMADDWD with folded coefficients instead of VPMULLD.

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

CONST_YMM(fc128, 128)
CONST_YMM(fcr2, 2)
CONST_YMM(fcr1024, 1024)
CONST_YMM(fcr16384, 16384)

#define CONST_PAIR(name, val) \
DATA name<>+0(SB)/8, $val; \
DATA name<>+8(SB)/8, $val; \
DATA name<>+16(SB)/8, $val; \
DATA name<>+24(SB)/8, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $32

CONST_PAIR(fp2_1213, 0x29CF115129CF1151) // 4433, 10703
CONST_PAIR(fp6_1213, 0x1151D6301151D630) // -10704, 4433
CONST_PAIR(fp1_45, 0x192508D4192508D4) // 2260, 6437
CONST_PAIR(fp1_67, 0x2C6325A12C6325A1) // 9633, 11363
CONST_PAIR(fp3_45, 0xD39EE6DCD39EE6DC) // -6436, -11362
CONST_PAIR(fp3_67, 0x25A1F72D25A1F72D) // -2259, 9633
CONST_PAIR(fp5_45, 0x08D525A108D525A1) // 9633, 2261
CONST_PAIR(fp5_67, 0x1925D39E1925D39E) // -11362, 6437
CONST_PAIR(fp7_45, 0x25A1D39D25A1D39D) // -11363, 9633
CONST_PAIR(fp7_67, 0x08D4E6DC08D4E6DC) // -6436, 2260

// Transpose an 8x8 block of dwords from Y0-Y7 into Y8-Y15, clobbering Y0-Y7.
#define TRANSPOSE_8X8() \
	VPUNPCKLDQ Y1, Y0, Y8; VPUNPCKLDQ Y3, Y2, Y10; \
	VPUNPCKLDQ Y5, Y4, Y12; VPUNPCKLDQ Y7, Y6, Y14; \
	VPUNPCKHDQ Y1, Y0, Y9; VPUNPCKHDQ Y3, Y2, Y11; \
	VPUNPCKHDQ Y5, Y4, Y13; VPUNPCKHDQ Y7, Y6, Y15; \
	VPUNPCKLQDQ Y10, Y8, Y0; VPUNPCKHQDQ Y10, Y8, Y1; \
	VPUNPCKLQDQ Y11, Y9, Y2; VPUNPCKHQDQ Y11, Y9, Y3; \
	VPUNPCKLQDQ Y14, Y12, Y4; VPUNPCKHQDQ Y14, Y12, Y5; \
	VPUNPCKLQDQ Y15, Y13, Y6; VPUNPCKHQDQ Y15, Y13, Y7; \
	VPERM2F128 $0x20, Y4, Y0, Y8; VPERM2F128 $0x20, Y5, Y1, Y9; \
	VPERM2F128 $0x20, Y6, Y2, Y10; VPERM2F128 $0x20, Y7, Y3, Y11; \
	VPERM2F128 $0x31, Y4, Y0, Y12; VPERM2F128 $0x31, Y5, Y1, Y13; \
	VPERM2F128 $0x31, Y6, Y2, Y14; VPERM2F128 $0x31, Y7, Y3, Y15

// func fdctAVX2(blk *[64]int32, src *byte, stride int)
TEXT ·fdctAVX2(SB), NOSPLIT, $0-24
	MOVQ blk+0(FP), SI
	MOVQ src+8(FP), DI
	MOVQ stride+16(FP), CX

	// load bytes, zero extend to dwords and level shift
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

	// butterflies
	VPADDD Y15, Y8, Y0
	VPSUBD Y15, Y8, Y7
	VPADDD Y14, Y9, Y1
	VPSUBD Y14, Y9, Y6
	VPADDD Y13, Y10, Y2
	VPSUBD Y13, Y10, Y5
	VPADDD Y12, Y11, Y3
	VPSUBD Y12, Y11, Y4
	VPADDD Y3, Y0, Y8
	VPSUBD Y3, Y0, Y9
	VPADDD Y2, Y1, Y10
	VPSUBD Y2, Y1, Y11

	// pack tmp pairs into 32-bit lanes for VPMADDWD
	VPSLLD $16, Y5, Y12
	VPBLENDW $0x55, Y4, Y12, Y12
	VPSLLD $16, Y7, Y13
	VPBLENDW $0x55, Y6, Y13, Y13
	VPSLLD $16, Y9, Y14
	VPBLENDW $0x55, Y11, Y14, Y14

	// even part
	VPADDD Y10, Y8, Y0
	VPSUBD Y10, Y8, Y4
	VPSLLD $2, Y0, Y0
	VPSLLD $2, Y4, Y4
	VPMADDWD fp2_1213<>(SB), Y14, Y2
	VPADDD fcr1024<>(SB), Y2, Y2
	VPSRAD $11, Y2, Y2
	VPMADDWD fp6_1213<>(SB), Y14, Y6
	VPADDD fcr1024<>(SB), Y6, Y6
	VPSRAD $11, Y6, Y6

	// odd part
	VPMADDWD fp1_45<>(SB), Y12, Y1
	VPMADDWD fp1_67<>(SB), Y13, Y15
	VPADDD Y15, Y1, Y1
	VPADDD fcr1024<>(SB), Y1, Y1
	VPSRAD $11, Y1, Y1
	VPMADDWD fp3_45<>(SB), Y12, Y3
	VPMADDWD fp3_67<>(SB), Y13, Y15
	VPADDD Y15, Y3, Y3
	VPADDD fcr1024<>(SB), Y3, Y3
	VPSRAD $11, Y3, Y3
	VPMADDWD fp5_45<>(SB), Y12, Y5
	VPMADDWD fp5_67<>(SB), Y13, Y15
	VPADDD Y15, Y5, Y5
	VPADDD fcr1024<>(SB), Y5, Y5
	VPSRAD $11, Y5, Y5
	VPMADDWD fp7_45<>(SB), Y12, Y7
	VPMADDWD fp7_67<>(SB), Y13, Y15
	VPADDD Y15, Y7, Y7
	VPADDD fcr1024<>(SB), Y7, Y7
	VPSRAD $11, Y7, Y7

	TRANSPOSE_8X8()

	// butterflies
	VPADDD Y15, Y8, Y0
	VPSUBD Y15, Y8, Y7
	VPADDD Y14, Y9, Y1
	VPSUBD Y14, Y9, Y6
	VPADDD Y13, Y10, Y2
	VPSUBD Y13, Y10, Y5
	VPADDD Y12, Y11, Y3
	VPSUBD Y12, Y11, Y4
	VPADDD Y3, Y0, Y8
	VPSUBD Y3, Y0, Y9
	VPADDD Y2, Y1, Y10
	VPSUBD Y2, Y1, Y11

	// pack tmp pairs into 32-bit lanes for VPMADDWD
	VPSLLD $16, Y5, Y12
	VPBLENDW $0x55, Y4, Y12, Y12
	VPSLLD $16, Y7, Y13
	VPBLENDW $0x55, Y6, Y13, Y13
	VPSLLD $16, Y9, Y14
	VPBLENDW $0x55, Y11, Y14, Y14

	// even part
	VPADDD Y10, Y8, Y0
	VPSUBD Y10, Y8, Y4
	VPADDD fcr2<>(SB), Y0, Y0
	VPSRAD $2, Y0, Y0
	VPADDD fcr2<>(SB), Y4, Y4
	VPSRAD $2, Y4, Y4
	VPMADDWD fp2_1213<>(SB), Y14, Y2
	VPADDD fcr16384<>(SB), Y2, Y2
	VPSRAD $15, Y2, Y2
	VPMADDWD fp6_1213<>(SB), Y14, Y6
	VPADDD fcr16384<>(SB), Y6, Y6
	VPSRAD $15, Y6, Y6

	// odd part
	VPMADDWD fp1_45<>(SB), Y12, Y1
	VPMADDWD fp1_67<>(SB), Y13, Y15
	VPADDD Y15, Y1, Y1
	VPADDD fcr16384<>(SB), Y1, Y1
	VPSRAD $15, Y1, Y1
	VPMADDWD fp3_45<>(SB), Y12, Y3
	VPMADDWD fp3_67<>(SB), Y13, Y15
	VPADDD Y15, Y3, Y3
	VPADDD fcr16384<>(SB), Y3, Y3
	VPSRAD $15, Y3, Y3
	VPMADDWD fp5_45<>(SB), Y12, Y5
	VPMADDWD fp5_67<>(SB), Y13, Y15
	VPADDD Y15, Y5, Y5
	VPADDD fcr16384<>(SB), Y5, Y5
	VPSRAD $15, Y5, Y5
	VPMADDWD fp7_45<>(SB), Y12, Y7
	VPMADDWD fp7_67<>(SB), Y13, Y15
	VPADDD Y15, Y7, Y7
	VPADDD fcr16384<>(SB), Y7, Y7
	VPSRAD $15, Y7, Y7

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

// Transpose a 4x4 block of dwords held in A-D, clobbering T0-T3.
#define TRANSPOSE4(a, b, c, d, t0, t1, t2, t3) \
	MOVOU      a, t0;   \
	PUNPCKLLQ  b, t0;   \
	MOVOU      c, t1;   \
	PUNPCKLLQ  d, t1;   \
	MOVOU      a, t2;   \
	PUNPCKHLQ  b, t2;   \
	MOVOU      c, t3;   \
	PUNPCKHLQ  d, t3;   \
	MOVOU      t0, a;   \
	PUNPCKLQDQ t1, a;   \
	MOVOU      t0, b;   \
	PUNPCKHQDQ t1, b;   \
	MOVOU      t2, c;   \
	PUNPCKLQDQ t3, c;   \
	MOVOU      t2, d;   \
	PUNPCKHQDQ t3, d

// Butterfly X0-X7 into t10 (X12), t11 (X13) and the coefficient pairs
// p45 (X10), p67 (X11) and p1213 (X14). Clobbers X8, X9.
#define FDCT_BUTTERFLY() \
	MOVOU   X0, X8;         \
	PADDL   X7, X8;         \
	PSUBL   X7, X0;         \
	MOVOU   X1, X9;         \
	PADDL   X6, X9;         \
	PSUBL   X6, X1;         \
	MOVOU   X2, X10;        \
	PADDL   X5, X10;        \
	PSUBL   X5, X2;         \
	MOVOU   X3, X11;        \
	PADDL   X4, X11;        \
	PSUBL   X4, X3;         \
	MOVOU   X8, X12;        \
	PADDL   X11, X12;       \
	PSUBL   X11, X8;        \
	MOVOU   X9, X13;        \
	PADDL   X10, X13;       \
	PSUBL   X10, X9;        \
	MOVOU   X2, X10;        \
	PSLLL   $16, X10;       \
	PBLENDW $0x55, X3, X10; \
	MOVOU   X0, X11;        \
	PSLLL   $16, X11;       \
	PBLENDW $0x55, X1, X11; \
	MOVOU   X8, X14;        \
	PSLLL   $16, X14;       \
	PBLENDW $0x55, X9, X14

// Rotate the packed pairs into the odd and non-DC even outputs. Clobbers X13.
#define FDCT_ODD(rnd, sh) \
	MOVOU   X14, X2;                 \
	PMADDWL fp2_1213<>(SB), X2;      \
	PADDL   rnd, X2;                 \
	PSRAL   sh, X2;                  \
	MOVOU   X14, X6;                 \
	PMADDWL fp6_1213<>(SB), X6;      \
	PADDL   rnd, X6;                 \
	PSRAL   sh, X6;                  \
	MOVOU   X10, X1;                 \
	PMADDWL fp1_45<>(SB), X1;        \
	MOVOU   X11, X13;                \
	PMADDWL fp1_67<>(SB), X13;       \
	PADDL   X13, X1;                 \
	PADDL   rnd, X1;                 \
	PSRAL   sh, X1;                  \
	MOVOU   X10, X3;                 \
	PMADDWL fp3_45<>(SB), X3;        \
	MOVOU   X11, X13;                \
	PMADDWL fp3_67<>(SB), X13;       \
	PADDL   X13, X3;                 \
	PADDL   rnd, X3;                 \
	PSRAL   sh, X3;                  \
	MOVOU   X10, X5;                 \
	PMADDWL fp5_45<>(SB), X5;        \
	MOVOU   X11, X13;                \
	PMADDWL fp5_67<>(SB), X13;       \
	PADDL   X13, X5;                 \
	PADDL   rnd, X5;                 \
	PSRAL   sh, X5;                  \
	MOVOU   X10, X7;                 \
	PMADDWL fp7_45<>(SB), X7;        \
	MOVOU   X11, X13;                \
	PMADDWL fp7_67<>(SB), X13;       \
	PADDL   X13, X7;                 \
	PADDL   rnd, X7;                 \
	PSRAL   sh, X7

// Load four rows at DI into X0-X3 (columns 0-3) and X4-X7 (columns 4-7).
#define FDCT_LOAD4() \
	PMOVZXBD 0(DI), X0;         \
	PSUBL    fc128<>(SB), X0;   \
	PMOVZXBD 4(DI), X4;         \
	PSUBL    fc128<>(SB), X4;   \
	ADDQ     CX, DI;            \
	PMOVZXBD 0(DI), X1;         \
	PSUBL    fc128<>(SB), X1;   \
	PMOVZXBD 4(DI), X5;         \
	PSUBL    fc128<>(SB), X5;   \
	ADDQ     CX, DI;            \
	PMOVZXBD 0(DI), X2;         \
	PSUBL    fc128<>(SB), X2;   \
	PMOVZXBD 4(DI), X6;         \
	PSUBL    fc128<>(SB), X6;   \
	ADDQ     CX, DI;            \
	PMOVZXBD 0(DI), X3;         \
	PSUBL    fc128<>(SB), X3;   \
	PMOVZXBD 4(DI), X7;         \
	PSUBL    fc128<>(SB), X7;   \
	ADDQ     CX, DI;            \
	TRANSPOSE4(X0, X1, X2, X3, X8, X9, X10, X11); \
	TRANSPOSE4(X4, X5, X6, X7, X8, X9, X10, X11)

// Row pass over the four rows held in X0-X7, storing row-major at OFF(SI).
#define FDCT_ROWS(off) \
	FDCT_BUTTERFLY();                             \
	MOVOU X12, X0;                                \
	PADDL X13, X0;                                \
	PSLLL $2, X0;                                 \
	MOVOU X12, X4;                                \
	PSUBL X13, X4;                                \
	PSLLL $2, X4;                                 \
	MOVOU fcr1024<>(SB), X15;                     \
	FDCT_ODD(X15, $11);                           \
	TRANSPOSE4(X0, X1, X2, X3, X8, X9, X10, X11); \
	TRANSPOSE4(X4, X5, X6, X7, X8, X9, X10, X11); \
	MOVOU X0, (off+0)(SI);                        \
	MOVOU X4, (off+16)(SI);                       \
	MOVOU X1, (off+32)(SI);                       \
	MOVOU X5, (off+48)(SI);                       \
	MOVOU X2, (off+64)(SI);                       \
	MOVOU X6, (off+80)(SI);                       \
	MOVOU X3, (off+96)(SI);                       \
	MOVOU X7, (off+112)(SI)

// Column pass over the four columns at OFF(SI), stored back in place.
#define FDCT_COLS(off) \
	MOVOU (off+0)(SI), X0;      \
	MOVOU (off+32)(SI), X1;     \
	MOVOU (off+64)(SI), X2;     \
	MOVOU (off+96)(SI), X3;     \
	MOVOU (off+128)(SI), X4;    \
	MOVOU (off+160)(SI), X5;    \
	MOVOU (off+192)(SI), X6;    \
	MOVOU (off+224)(SI), X7;    \
	FDCT_BUTTERFLY();           \
	MOVOU X12, X0;              \
	PADDL X13, X0;              \
	PADDL fcr2<>(SB), X0;       \
	PSRAL $2, X0;               \
	MOVOU X12, X4;              \
	PSUBL X13, X4;              \
	PADDL fcr2<>(SB), X4;       \
	PSRAL $2, X4;               \
	MOVOU fcr16384<>(SB), X15;  \
	FDCT_ODD(X15, $15);         \
	MOVOU X0, (off+0)(SI);      \
	MOVOU X1, (off+32)(SI);     \
	MOVOU X2, (off+64)(SI);     \
	MOVOU X3, (off+96)(SI);     \
	MOVOU X4, (off+128)(SI);    \
	MOVOU X5, (off+160)(SI);    \
	MOVOU X6, (off+192)(SI);    \
	MOVOU X7, (off+224)(SI)

// func fdctSSE(blk *[64]int32, src *byte, stride int)
TEXT ·fdctSSE(SB), NOSPLIT, $0-24
	MOVQ blk+0(FP), SI
	MOVQ src+8(FP), DI
	MOVQ stride+16(FP), CX

	FDCT_LOAD4()
	FDCT_ROWS(0)

	FDCT_LOAD4()
	FDCT_ROWS(128)

	FDCT_COLS(0)
	FDCT_COLS(16)

	RET
