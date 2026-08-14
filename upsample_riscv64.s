//go:build riscv64 && riscv64.rva23u64 && !noasm

#include "textflag.h"

// RVV 4-tap Catmull-Rom upsampling, bit-identical to the pure Go middles. The
// vector length clamps itself to the run, so there is no tail.

#define TAPS(p0, p1, p2, p3, acc) \
	VMVVX   X21, acc;         \
	VMACCVX p0, X16, acc;     \
	VMACCVX p1, X17, acc;     \
	VMACCVX p2, X18, acc;     \
	VMACCVX p3, X19, acc;     \
	VSRAVI  $7, acc, acc;     \
	VMAXVX  X0, acc, acc;     \
	VMINVX  X22, acc, acc

// func upsampleVMiddleRVV(dst1, dst2, src *byte, stride, n int)
TEXT ·upsampleVMiddleRVV(SB), NOSPLIT, $0-40
	MOV dst1+0(FP), X10
	MOV dst2+8(FP), X11
	MOV src+16(FP), X12
	MOV stride+24(FP), X13
	MOV n+32(FP), X14

	MOV $-9, X16
	MOV $111, X17
	MOV $29, X18
	MOV $-3, X19
	MOV $64, X21
	MOV $255, X22

	ADD X13, X12, X23
	ADD X13, X23, X24
	ADD X13, X24, X25

vloop:
	VSETVLI X14, E8, M1, TA, MA, X15
	VLE8V   (X12), V0
	VLE8V   (X23), V1
	VLE8V   (X24), V2
	VLE8V   (X25), V3

	VSETVLI  X14, E32, M4, TA, MA, X15
	VZEXTVF4 V0, V8
	VZEXTVF4 V1, V12
	VZEXTVF4 V2, V16
	VZEXTVF4 V3, V20

	TAPS(V8, V12, V16, V20, V24)
	TAPS(V20, V16, V12, V8, V28)

	VSETVLI X14, E16, M2, TA, MA, X15
	VNSRLWI $0, V24, V4
	VNSRLWI $0, V28, V6

	VSETVLI X14, E8, M1, TA, MA, X15
	VNSRLWI $0, V4, V0
	VSE8V   V0, (X10)
	VNSRLWI $0, V6, V1
	VSE8V   V1, (X11)

	ADD X15, X10, X10
	ADD X15, X11, X11
	ADD X15, X12, X12
	ADD X15, X23, X23
	ADD X15, X24, X24
	ADD X15, X25, X25
	SUB X15, X14, X14
	BNE X0, X14, vloop

	RET

// func upsampleHMiddleRVV(dst, src *byte, n int)
TEXT ·upsampleHMiddleRVV(SB), NOSPLIT, $0-24
	MOV dst+0(FP), X10
	MOV src+8(FP), X12
	MOV n+16(FP), X14

	MOV $-9, X16
	MOV $111, X17
	MOV $29, X18
	MOV $-3, X19
	MOV $64, X21
	MOV $255, X22

	ADD $1, X12, X23
	ADD $2, X12, X24
	ADD $3, X12, X25

hloop:
	VSETVLI X14, E8, M1, TA, MA, X15
	VLE8V   (X12), V0
	VLE8V   (X23), V1
	VLE8V   (X24), V2
	VLE8V   (X25), V3

	VSETVLI  X14, E32, M4, TA, MA, X15
	VZEXTVF4 V0, V8
	VZEXTVF4 V1, V12
	VZEXTVF4 V2, V16
	VZEXTVF4 V3, V20

	TAPS(V8, V12, V16, V20, V24)
	TAPS(V20, V16, V12, V8, V28)

	VSETVLI X14, E16, M2, TA, MA, X15
	VNSRLWI $0, V24, V4
	VNSRLWI $0, V28, V6

	VSETVLI   X14, E8, M1, TA, MA, X15
	VNSRLWI   $0, V4, V0
	VNSRLWI   $0, V6, V1
	VSSEG2E8V V0, (X10)

	SLLI $1, X15, X26
	ADD  X26, X10, X10
	ADD  X15, X12, X12
	ADD  X15, X23, X23
	ADD  X15, X24, X24
	ADD  X15, X25, X25
	SUB  X15, X14, X14
	BNE  X0, X14, hloop

	RET
