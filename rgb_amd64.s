//go:build amd64 && !noasm

#include "textflag.h"

// AVX2 packed RGB to YCbCr planes, bit-identical to rgbToYCbCr. Green 38470 and
// the 32768 terms exceed VPMADDWD's signed 16-bit range: green splits as
// 22086+16384, the 32768 terms become shifts.

// Byte selectors, applied per 128-bit lane to four pixels.
DATA mrg<>+0(SB)/8, $0x8005800480018000
DATA mrg<>+8(SB)/8, $0x800D800C80098008
DATA mrg<>+16(SB)/8, $0x8005800480018000
DATA mrg<>+24(SB)/8, $0x800D800C80098008
GLOBL mrg<>(SB), RODATA|NOPTR, $32

DATA mbg<>+0(SB)/8, $0x8005800680018002
DATA mbg<>+8(SB)/8, $0x800D800E8009800A
DATA mbg<>+16(SB)/8, $0x8005800680018002
DATA mbg<>+24(SB)/8, $0x800D800E8009800A
GLOBL mbg<>(SB), RODATA|NOPTR, $32

DATA mr<>+0(SB)/8, $0x8080800480808000
DATA mr<>+8(SB)/8, $0x8080800C80808008
DATA mr<>+16(SB)/8, $0x8080800480808000
DATA mr<>+24(SB)/8, $0x8080800C80808008
GLOBL mr<>(SB), RODATA|NOPTR, $32

DATA mb<>+0(SB)/8, $0x8080800680808002
DATA mb<>+8(SB)/8, $0x8080800E8080800A
DATA mb<>+16(SB)/8, $0x8080800680808002
DATA mb<>+24(SB)/8, $0x8080800E8080800A
GLOBL mb<>(SB), RODATA|NOPTR, $32

#define CONST_PAIR(name, val) \
DATA name<>+0(SB)/8, $val; \
DATA name<>+8(SB)/8, $val; \
DATA name<>+16(SB)/8, $val; \
DATA name<>+24(SB)/8, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $32

CONST_PAIR(cyrg, 0x56464C8B56464C8B) // 19595, 22086
CONST_PAIR(cybg, 0x40001D2F40001D2F) // 7471, 16384
CONST_PAIR(cbrg, 0xAB30D4D0AB30D4D0) // -11056, -21712
CONST_PAIR(crbg, 0x94D0EB3094D0EB30) // -5328, -27440

#define CONST_DWORD(name, val) \
DATA name<>+0(SB)/4, $val; \
DATA name<>+4(SB)/4, $val; \
DATA name<>+8(SB)/4, $val; \
DATA name<>+12(SB)/4, $val; \
DATA name<>+16(SB)/4, $val; \
DATA name<>+20(SB)/4, $val; \
DATA name<>+24(SB)/4, $val; \
DATA name<>+28(SB)/4, $val; \
GLOBL name<>(SB), RODATA|NOPTR, $32

CONST_DWORD(rndy, 32768)
CONST_DWORD(rndc, 8421376)

// Convert the eight pixels in SRC into dwords YO, CBO and CRO. SRC is clobbered.
#define CONVERT8(SRC, YO, CBO, CRO, T0, T1, T2, T3) \
	VPSHUFB mrg<>(SB), SRC, T0;    \
	VPSHUFB mbg<>(SB), SRC, T1;    \
	VPSHUFB mr<>(SB), SRC, T2;     \
	VPSHUFB mb<>(SB), SRC, T3;     \
	VPMADDWD cyrg<>(SB), T0, YO;   \
	VPMADDWD cybg<>(SB), T1, SRC;  \
	VPADDD SRC, YO, YO;            \
	VPADDD rndy<>(SB), YO, YO;     \
	VPSRAD $16, YO, YO;            \
	VPMADDWD cbrg<>(SB), T0, CBO;  \
	VPSLLD $15, T3, T3;            \
	VPADDD T3, CBO, CBO;           \
	VPADDD rndc<>(SB), CBO, CBO;   \
	VPSRAD $16, CBO, CBO;          \
	VPMADDWD crbg<>(SB), T1, CRO;  \
	VPSLLD $15, T2, T2;            \
	VPADDD T2, CRO, CRO;           \
	VPADDD rndc<>(SB), CRO, CRO;   \
	VPSRAD $16, CRO, CRO

// Pack two dword vectors into sixteen saturated bytes in the low half of DST.
#define PACK16(LO, HI, DST) \
	VPACKSSDW HI, LO, DST;   \
	VPACKUSWB DST, DST, DST; \
	VPERMQ $0xD8, DST, DST;  \
	VPSHUFD $0xD8, DST, DST

// func rgbToYCbCrAVX2(dstY, dstCb, dstCr, src []byte, n int)
TEXT ·rgbToYCbCrAVX2(SB), NOSPLIT, $0-104
	MOVQ dstY_base+0(FP), DI
	MOVQ dstCb_base+24(FP), SI
	MOVQ dstCr_base+48(FP), DX
	MOVQ src_base+72(FP), BX
	MOVQ n+96(FP), CX

loop:
	CMPQ CX, $16
	JL   done

	VMOVDQU 0(BX), Y0
	VMOVDQU 32(BX), Y8

	CONVERT8(Y0, Y5, Y6, Y7, Y1, Y2, Y3, Y4)
	CONVERT8(Y8, Y9, Y10, Y11, Y1, Y2, Y3, Y4)

	PACK16(Y5, Y9, Y12)
	VMOVDQU X12, 0(DI)
	PACK16(Y6, Y10, Y12)
	VMOVDQU X12, 0(SI)
	PACK16(Y7, Y11, Y12)
	VMOVDQU X12, 0(DX)

	ADDQ $64, BX
	ADDQ $16, DI
	ADDQ $16, SI
	ADDQ $16, DX
	SUBQ $16, CX
	JMP  loop

done:
	VZEROUPPER
	RET

// Convert the four pixels in SRC into dwords YO, CBO and CRO. SRC is clobbered.
#define CONVERT4(SRC, YO, CBO, CRO, T0, T1, T2, T3) \
	MOVOU SRC, T0;            \
	MOVOU SRC, T1;            \
	MOVOU SRC, T2;            \
	MOVOU SRC, T3;            \
	PSHUFB mrg<>(SB), T0;     \
	PSHUFB mbg<>(SB), T1;     \
	PSHUFB mr<>(SB), T2;      \
	PSHUFB mb<>(SB), T3;      \
	MOVOU T0, YO;             \
	MOVOU T1, SRC;            \
	PMADDWL cyrg<>(SB), YO;   \
	PMADDWL cybg<>(SB), SRC;  \
	PADDL SRC, YO;            \
	PADDL rndy<>(SB), YO;     \
	PSRAL $16, YO;            \
	MOVOU T0, CBO;            \
	PMADDWL cbrg<>(SB), CBO;  \
	PSLLL $15, T3;            \
	PADDL T3, CBO;            \
	PADDL rndc<>(SB), CBO;    \
	PSRAL $16, CBO;           \
	MOVOU T1, CRO;            \
	PMADDWL crbg<>(SB), CRO;  \
	PSLLL $15, T2;            \
	PADDL T2, CRO;            \
	PADDL rndc<>(SB), CRO;    \
	PSRAL $16, CRO

// Pack two dword vectors into eight saturated bytes in the low half of DST.
#define PACK8(LO, HI, DST) \
	MOVOU LO, DST;         \
	PACKSSLW HI, DST;      \
	PACKUSWB DST, DST

// func rgbToYCbCrSSE(dstY, dstCb, dstCr, src []byte, n int)
TEXT ·rgbToYCbCrSSE(SB), NOSPLIT, $0-104
	MOVQ dstY_base+0(FP), DI
	MOVQ dstCb_base+24(FP), SI
	MOVQ dstCr_base+48(FP), DX
	MOVQ src_base+72(FP), BX
	MOVQ n+96(FP), CX

sse_loop:
	CMPQ CX, $8
	JL   sse_done

	MOVOU 0(BX), X0
	MOVOU 16(BX), X8

	CONVERT4(X0, X5, X6, X7, X1, X2, X3, X4)
	CONVERT4(X8, X9, X10, X11, X1, X2, X3, X4)

	PACK8(X5, X9, X12)
	MOVQ X12, 0(DI)
	PACK8(X6, X10, X12)
	MOVQ X12, 0(SI)
	PACK8(X7, X11, X12)
	MOVQ X12, 0(DX)

	ADDQ $32, BX
	ADDQ $8, DI
	ADDQ $8, SI
	ADDQ $8, DX
	SUBQ $8, CX
	JMP  sse_loop

sse_done:
	RET
