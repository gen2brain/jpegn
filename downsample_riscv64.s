//go:build riscv64 && riscv64.rva23u64 && !noasm

#include "textflag.h"

// RVV 2x2 chroma box filter, bit-identical to downsampleRow2x2Scalar. The
// segment load deinterleaves the column pairs and the widening add sums them,
// and the vector length clamps itself to the row, so there is no tail.

// func downsampleRow2x2RVV(dst, src0, src1 *byte, n int)
TEXT ·downsampleRow2x2RVV(SB), NOSPLIT, $0-32
	MOV dst+0(FP), X10
	MOV src0+8(FP), X11
	MOV src1+16(FP), X12
	MOV n+24(FP), X13

loop:
	VSETVLI   X13, E8, M2, TA, MA, X14
	VLSEG2E8V (X11), V0
	VWADDUVV  V0, V2, V8
	VLSEG2E8V (X12), V4
	VWADDUVV  V4, V6, V12

	VSETVLI X13, E16, M4, TA, MA, X14
	VADDVV  V12, V8, V8
	VADDVI  $2, V8, V8
	VSRLVI  $2, V8, V8

	VSETVLI X13, E8, M2, TA, MA, X14
	VNSRLWI $0, V8, V16
	VSE8V   V16, (X10)

	ADD  X14, X10, X10
	SLLI $1, X14, X15
	ADD  X15, X11, X11
	ADD  X15, X12, X12
	SUB  X14, X13, X13
	BNE  X0, X13, loop

	RET
