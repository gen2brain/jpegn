//go:build riscv64 && riscv64.rva23u64 && !noasm

#include "textflag.h"

// RVV 4-point inverse DCT for 1/2 scaling, bit-identical to idct8x8To4x4. The
// coefficient columns are gathered with strided loads so both passes run
// lane-wise; the transpose between them goes through the caller's scratch.

#define PASS(a0, a1, a2, a3, rnd, sh, o0, o1, o2, o3) \
	VADDVV a2, a0, V8;   \
	VSUBVV a2, a0, V9;   \
	VSLLVI $13, V8, V8;  \
	VSLLVI $13, V9, V9;  \
	VMULVX X16, a1, V10; \
	VMULVX X17, a3, V11; \
	VADDVV V11, V10, V10; \
	VMULVX X17, a1, V12; \
	VMULVX X16, a3, V13; \
	VSUBVV V13, V12, V12; \
	VADDVV V10, V8, o0;  \
	VADDVV V12, V9, o1;  \
	VSUBVV V12, V9, o2;  \
	VSUBVV V10, V8, o3;  \
	VADDVX rnd, o0, o0;  \
	VADDVX rnd, o1, o1;  \
	VADDVX rnd, o2, o2;  \
	VADDVX rnd, o3, o3;  \
	VSRAVI sh, o0, o0;   \
	VSRAVI sh, o1, o1;   \
	VSRAVI sh, o2, o2;   \
	VSRAVI sh, o3, o3

// func idct4x4RVV(blk *[64]int32, out *byte, stride int, scratch *[16]int32)
TEXT ·idct4x4RVV(SB), NOSPLIT, $0-32
	MOV blk+0(FP), X10
	MOV out+8(FP), X11
	MOV stride+16(FP), X12
	MOV scratch+24(FP), X14

	MOV $10703, X16
	MOV $4433, X17
	MOV $128, X18
	MOV $1048576, X19
	MOV $32, X20
	MOV $4, X21
	MOV $255, X23
	MOV $16, X24

	VSETVLI X21, E32, M1, TA, MA, X22

	VLSE32V (X10), X20, V0
	ADD     $4, X10, X13
	VLSE32V (X13), X20, V1
	ADD     $8, X10, X13
	VLSE32V (X13), X20, V2
	ADD     $12, X10, X13
	VLSE32V (X13), X20, V3

	PASS(V0, V1, V2, V3, X18, $8, V4, V5, V6, V7)

	VSSE32V V4, X24, (X14)
	ADD     $4, X14, X13
	VSSE32V V5, X24, (X13)
	ADD     $8, X14, X13
	VSSE32V V6, X24, (X13)
	ADD     $12, X14, X13
	VSSE32V V7, X24, (X13)

	VLE32V (X14), V0
	ADD    $16, X14, X13
	VLE32V (X13), V1
	ADD    $32, X14, X13
	VLE32V (X13), V2
	ADD    $48, X14, X13
	VLE32V (X13), V3

	PASS(V0, V1, V2, V3, X19, $21, V4, V5, V6, V7)

	VADDVX X18, V4, V4
	VADDVX X18, V5, V5
	VADDVX X18, V6, V6
	VADDVX X18, V7, V7

	VMAXVX X0, V4, V4
	VMAXVX X0, V5, V5
	VMAXVX X0, V6, V6
	VMAXVX X0, V7, V7
	VMINVX X23, V4, V4
	VMINVX X23, V5, V5
	VMINVX X23, V6, V6
	VMINVX X23, V7, V7

	VSETVLI X21, E16, MF2, TA, MA, X22
	VNSRLWI $0, V4, V0
	VNSRLWI $0, V5, V1
	VNSRLWI $0, V6, V2
	VNSRLWI $0, V7, V3

	VSETVLI X21, E8, MF4, TA, MA, X22
	VNSRLWI $0, V0, V4
	VNSRLWI $0, V1, V5
	VNSRLWI $0, V2, V6
	VNSRLWI $0, V3, V7

	VSE8V V4, (X11)
	ADD   X12, X11, X11
	VSE8V V5, (X11)
	ADD   X12, X11, X11
	VSE8V V6, (X11)
	ADD   X12, X11, X11
	VSE8V V7, (X11)

	RET
