//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 4-point inverse DCT for 1/2 scaling, bit-identical to idct8x8To4x4.
// Both passes run four lanes at a time, with a 4x4 transpose between them.

#define CONST4(name, val) \
DATA name<>+0(SB)/4, $val; \
DATA name<>+4(SB)/4, $val; \
DATA name<>+8(SB)/4, $val; \
DATA name<>+12(SB)/4, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $16

CONST4(sk1, 10703)
CONST4(sk3, 4433)
CONST4(srnd1, 128)
CONST4(srnd2, 1048576)

// Transpose the four dword rows a0-a3 into d0-d3, clobbering t0 and t1.
#define TRANSPOSE4(a0, a1, a2, a3, t0, t1, d0, d1, d2, d3) \
	VPUNPCKLDQ a1, a0, t0;  \
	VPUNPCKHDQ a1, a0, t1;  \
	VPUNPCKLDQ a3, a2, d2;  \
	VPUNPCKHDQ a3, a2, d3;  \
	VPUNPCKLQDQ d2, t0, d0; \
	VPUNPCKHQDQ d2, t0, d1; \
	VPUNPCKLQDQ d3, t1, t0; \
	VPUNPCKHQDQ d3, t1, d3; \
	VMOVDQA t0, d2

// One 4-point inverse DCT pass over the four lanes of a0-a3.
#define PASS(a0, a1, a2, a3, rnd, sh, o0, o1, o2, o3) \
	VPADDD a2, a0, X8;      \
	VPSUBD a2, a0, X9;      \
	VPSLLD $13, X8, X8;     \
	VPSLLD $13, X9, X9;     \
	VPADDD rnd, X8, X8;     \
	VPADDD rnd, X9, X9;     \
	VPMULLD X14, a1, X10;   \
	VPMULLD X15, a3, X11;   \
	VPADDD X11, X10, X10;   \
	VPMULLD X15, a1, X12;   \
	VPMULLD X14, a3, X13;   \
	VPSUBD X13, X12, X12;   \
	VPADDD X10, X8, o0;     \
	VPADDD X12, X9, o1;     \
	VPSUBD X12, X9, o2;     \
	VPSUBD X10, X8, o3;     \
	VPSRAD sh, o0, o0;      \
	VPSRAD sh, o1, o1;      \
	VPSRAD sh, o2, o2;      \
	VPSRAD sh, o3, o3

// Clamp one row to bytes and store it.
#define STOREROW(r, off) \
	VPADDD srnd1<>(SB), r, r; \
	VPACKSSDW r, r, r;        \
	VPACKUSWB r, r, r;        \
	VMOVD r, off

// func idct4x4AVX2(blk *[64]int32, out *byte, stride int)
TEXT ·idct4x4AVX2(SB), NOSPLIT, $0-24
	MOVQ blk+0(FP), SI
	MOVQ out+8(FP), DI
	MOVQ stride+16(FP), CX

	VMOVDQU sk1<>(SB), X14
	VMOVDQU sk3<>(SB), X15

	VMOVDQU 0(SI), X0
	VMOVDQU 32(SI), X1
	VMOVDQU 64(SI), X2
	VMOVDQU 96(SI), X3

	TRANSPOSE4(X0, X1, X2, X3, X8, X9, X4, X5, X6, X7)
	PASS(X4, X5, X6, X7, srnd1<>(SB), $8, X0, X1, X2, X3)
	TRANSPOSE4(X0, X1, X2, X3, X8, X9, X4, X5, X6, X7)
	PASS(X4, X5, X6, X7, srnd2<>(SB), $21, X0, X1, X2, X3)

	STOREROW(X0, (DI))
	ADDQ CX, DI
	STOREROW(X1, (DI))
	ADDQ CX, DI
	STOREROW(X2, (DI))
	ADDQ CX, DI
	STOREROW(X3, (DI))

	VZEROUPPER
	RET

// Transpose the four dword rows A-D in place, clobbering T0-T3.
#define TRANSPOSE4S(a, b, c, d, t0, t1, t2, t3) \
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

// One 4-point inverse DCT pass over the four lanes of A0-A3.
#define PASSS(a0, a1, a2, a3, rnd, sh, o0, o1, o2, o3) \
	MOVOU  a0, X8;    \
	PADDL  a2, X8;    \
	MOVOU  a0, X9;    \
	PSUBL  a2, X9;    \
	PSLLL  $13, X8;   \
	PSLLL  $13, X9;   \
	PADDL  rnd, X8;   \
	PADDL  rnd, X9;   \
	MOVOU  a1, X10;   \
	PMULLD X14, X10;  \
	MOVOU  a3, X11;   \
	PMULLD X15, X11;  \
	PADDL  X11, X10;  \
	MOVOU  a1, X12;   \
	PMULLD X15, X12;  \
	MOVOU  a3, X13;   \
	PMULLD X14, X13;  \
	PSUBL  X13, X12;  \
	MOVOU  X8, o0;    \
	PADDL  X10, o0;   \
	MOVOU  X9, o1;    \
	PADDL  X12, o1;   \
	MOVOU  X9, o2;    \
	PSUBL  X12, o2;   \
	MOVOU  X8, o3;    \
	PSUBL  X10, o3;   \
	PSRAL  sh, o0;    \
	PSRAL  sh, o1;    \
	PSRAL  sh, o2;    \
	PSRAL  sh, o3

// Clamp one row to bytes and store it.
#define STOREROWS(r, off) \
	PADDL    srnd1<>(SB), r; \
	PACKSSLW r, r;           \
	PACKUSWB r, r;           \
	MOVL     r, off

// func idct4x4SSE(blk *[64]int32, out *byte, stride int)
TEXT ·idct4x4SSE(SB), NOSPLIT, $0-24
	MOVQ blk+0(FP), SI
	MOVQ out+8(FP), DI
	MOVQ stride+16(FP), CX

	MOVOU sk1<>(SB), X14
	MOVOU sk3<>(SB), X15

	MOVOU 0(SI), X0
	MOVOU 32(SI), X1
	MOVOU 64(SI), X2
	MOVOU 96(SI), X3

	TRANSPOSE4S(X0, X1, X2, X3, X8, X9, X10, X11)
	PASSS(X0, X1, X2, X3, srnd1<>(SB), $8, X4, X5, X6, X7)
	TRANSPOSE4S(X4, X5, X6, X7, X8, X9, X10, X11)
	PASSS(X4, X5, X6, X7, srnd2<>(SB), $21, X0, X1, X2, X3)

	STOREROWS(X0, (DI))
	ADDQ CX, DI
	STOREROWS(X1, (DI))
	ADDQ CX, DI
	STOREROWS(X2, (DI))
	ADDQ CX, DI
	STOREROWS(X3, (DI))

	RET
