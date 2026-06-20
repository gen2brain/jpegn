//go:build arm64 && !noasm

#include "textflag.h"

// upsampleNearestNeighborNEON performs a 2x2 nearest neighbor upsampling using NEON.
// It processes 16 source bytes (one NEON register) per iteration.
//
// func upsampleNearestNeighborNEON(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)
TEXT ·upsampleNearestNeighborNEON(SB), NOSPLIT, $0-48
    // Load arguments from the stack into registers.
    MOVD src+0(FP), R0      // R0 = source pixel data pointer
    MOVD dst+8(FP), R1      // R1 = destination pixel data pointer
    MOVD srcW+16(FP), R2    // R2 = source width
    MOVD srcH+24(FP), R3    // R3 = source height
    MOVD srcS+32(FP), R4    // R4 = source stride (bytes per row)
    MOVD dstS+40(FP), R5    // R5 = destination stride (bytes per row)

y_loop:
    // Initialize per-row pointers and the width counter for the inner loop.
    MOVD R0, R10            // R10 = current source row pointer
    MOVD R1, R11            // R11 = current destination row pointer (for row 2*y)
    MOVD R2, R12            // R12 = counter for remaining pixels in the row

    // --- Horizontal 2x Expansion Loop ---
x_loop:
    // If fewer than 16 bytes remain, jump to the scalar remainder handling.
    CMP  $16, R12
    BLO  x_rem

    // Load 16 bytes from the source. The .P modifier post-increments the pointer R10 by 16.
    VLD1.P 16(R10), [V0.B16]

    // The core of the 2x horizontal upsampling. We duplicate each byte.
    // For a source vector [b0, b1, ..., b15], we want to produce
    // [b0, b0, b1, b1, ..., b15, b15]. This is done in two steps for the
    // low and high 8 bytes of the source vector.

    // VZIP1 V0, V0 interleaves the low 8 bytes of V0 with itself.
    // Input low half: [b0, b1, b2, b3, b4, b5, b6, b7]
    // Output V1:      [b0, b0, b1, b1, b2, b2, b3, b3, b4, b4, b5, b5, b6, b6, b7, b7]
    VZIP1 V0.B16, V0.B16, V1.B16

    // VZIP2 V0, V0 does the same for the high 8 bytes of V0.
    // Input high half: [b8, b9, ..., b15]
    // Output V2:       [b8, b8, b9, b9, ..., b15, b15]
    VZIP2 V0.B16, V0.B16, V2.B16

    // Store the two resulting 16-byte vectors. The .P modifier post-increments R11 by 16 each time.
    VST1.P [V1.B16], 16(R11)
    VST1.P [V2.B16], 16(R11)

    // Decrement the width counter and loop.
    SUBS $16, R12, R12
    BNE  x_loop
    B    row_done

x_rem:
    // Handle the remaining 1-15 bytes using scalar instructions.
    CBZ R12, row_done

x_rem_loop:
    MOVBU (R10), R13        // Load one byte from source
    ADD   $1, R10           // src++
    MOVB  R13, (R11)        // Store it to dst
    MOVB  R13, 1(R11)       // Store it again to dst+1
    ADD   $2, R11           // dst+=2
    SUBS  $1, R12, R12      // remaining--
    BNE   x_rem_loop

row_done:
    // --- Vertical 2x Expansion ---
    // Copy the newly created expanded row (at 2*y) to the next row (2*y + 1).
    MOVD R1, R10            // R10 = source for copy (the row we just wrote)
    ADD  R5, R1, R11        // R11 = destination for copy (the next row)
    MOVD R2, R12            // R12 = original source width
    LSL  $1, R12, R12       // R12 = destination width to copy (srcW * 2)

copy_loop:
    // If fewer than 16 bytes remain, jump to scalar copy.
    CMP  $16, R12
    BLO  copy_rem

    // Load 16 bytes and store them to the next row. .P increments pointers.
    VLD1.P 16(R10), [V0.B16]
    VST1.P [V0.B16], 16(R11)

    SUBS $16, R12, R12
    BNE  copy_loop
    B    after_copy

copy_rem:
    // Handle remaining bytes for the copy.
    CBZ R12, after_copy

copy_rem_loop:
    MOVBU (R10), R13
    ADD   $1, R10
    MOVB  R13, (R11)
    ADD   $1, R11
    SUBS  $1, R12, R12
    BNE   copy_rem_loop

after_copy:
    // Move pointers to the next source row and the next pair of destination rows.
    ADD R4, R0              // src_ptr += src_stride
    ADD R5, R1              // dst_ptr += dst_stride
    ADD R5, R1              // dst_ptr += dst_stride (total of 2 * dst_stride)

    // Decrement height counter and loop for the next row.
    SUBS $1, R3, R3
    BNE  y_loop

    RET
    
// Catmull-Rom 2x upsampling (NEON middle-loop kernels).
//
// These kernels accelerate only the vectorizable interior of each row (H) or
// column strip (V); the three-sample boundaries and the sub-block tail are
// handled by the shared scalar helpers in upsample_noasm.go. Each invocation
// processes 8 samples per iteration with the 4-tap filter
//   O1 = (A*p0 + B*p1 + C*p2 + D*p3 + 64) >> 7
//   O2 = (D*p0 + C*p1 + B*p2 + A*p3 + 64) >> 7   (A=-9 B=111 C=29 D=-3)
// in 32-bit lanes, then clamps to [0,255] via unsigned-saturating narrowing.
//
// Coefficients live in V16(A) V17(B) V18(C) V19(D) V20(64). The 32-bit-lane
// VMUL, signed SSHR and UQXTN/SQXTUN narrowing are emitted as WORD directives
// (the Go assembler does not accept these mnemonics).

// FILTER8 consumes p0..p3 byte vectors in V0..V3 and produces O1 in V5.8B and
// O2 in V6.8B. Clobbers V4, V7, V8-V15, V21-V25.
#define FILTER8() \
	VUXTL V0.B8, V4.H8; VUXTL V4.H4, V8.S4; VUXTL2 V4.H8, V9.S4;    \
	VUXTL V1.B8, V4.H8; VUXTL V4.H4, V10.S4; VUXTL2 V4.H8, V11.S4;  \
	VUXTL V2.B8, V4.H8; VUXTL V4.H4, V12.S4; VUXTL2 V4.H8, V13.S4;  \
	VUXTL V3.B8, V4.H8; VUXTL V4.H4, V14.S4; VUXTL2 V4.H8, V15.S4;  \
	WORD $0x4EB09D15; WORD $0x4EB19D56; VADD V22.S4, V21.S4, V21.S4; \
	WORD $0x4EB29D96; VADD V22.S4, V21.S4, V21.S4;                  \
	WORD $0x4EB39DD6; VADD V22.S4, V21.S4, V21.S4; VADD V20.S4, V21.S4, V21.S4; \
	WORD $0x4F3906B5;                                               \
	WORD $0x4EB09D36; WORD $0x4EB19D77; VADD V23.S4, V22.S4, V22.S4; \
	WORD $0x4EB29DB7; VADD V23.S4, V22.S4, V22.S4;                  \
	WORD $0x4EB39DF7; VADD V23.S4, V22.S4, V22.S4; VADD V20.S4, V22.S4, V22.S4; \
	WORD $0x4F3906D6;                                               \
	WORD $0x2E614AA7; WORD $0x6E614AC7; WORD $0x2E2128E5;           \
	WORD $0x4EB39D17; WORD $0x4EB29D58; VADD V24.S4, V23.S4, V23.S4; \
	WORD $0x4EB19D98; VADD V24.S4, V23.S4, V23.S4;                  \
	WORD $0x4EB09DD8; VADD V24.S4, V23.S4, V23.S4; VADD V20.S4, V23.S4, V23.S4; \
	WORD $0x4F3906F7;                                               \
	WORD $0x4EB39D38; WORD $0x4EB29D79; VADD V25.S4, V24.S4, V24.S4; \
	WORD $0x4EB19DB9; VADD V25.S4, V24.S4, V24.S4;                  \
	WORD $0x4EB09DF9; VADD V25.S4, V24.S4, V24.S4; VADD V20.S4, V24.S4, V24.S4; \
	WORD $0x4F390718;                                               \
	WORD $0x2E614AE7; WORD $0x6E614B07; WORD $0x2E2128E6

// LOAD_COEFFS broadcasts the filter coefficients into V16-V20.
#define LOAD_COEFFS() \
	MOVD $-9, R3;  VDUP R3, V16.S4; \
	MOVD $111, R3; VDUP R3, V17.S4; \
	MOVD $29, R3;  VDUP R3, V18.S4; \
	MOVD $-3, R3;  VDUP R3, V19.S4; \
	MOVD $64, R3;  VDUP R3, V20.S4

// func upsampleHMiddleNEON(dst, src unsafe.Pointer, n int)
// Writes 2*n interleaved interior samples. n is a multiple of 8. For input
// position i, p0..p3 are src[i..i+3]; outputs O1,O2 are stored interleaved.
TEXT ·upsampleHMiddleNEON(SB), NOSPLIT, $0-24
	MOVD dst+0(FP), R0
	MOVD src+8(FP), R1
	MOVD n+16(FP), R2
	LOAD_COEFFS()

h_mid_loop:
	CMP $8, R2
	BLT h_mid_done

	VLD1 (R1), [V0.B8]
	ADD  $1, R1, R4; VLD1 (R4), [V1.B8]
	ADD  $2, R1, R4; VLD1 (R4), [V2.B8]
	ADD  $3, R1, R4; VLD1 (R4), [V3.B8]

	FILTER8()

	VST2.P [V5.B8, V6.B8], 16(R0)

	ADD $8, R1
	SUB $8, R2
	B   h_mid_loop

h_mid_done:
	RET

// func upsampleVMiddleNEON(dst1, dst2, src unsafe.Pointer, stride, n int)
// Writes n samples to each of two output rows. n is a multiple of 8. p0..p3 are
// the four source rows src, src+stride, src+2*stride, src+3*stride.
TEXT ·upsampleVMiddleNEON(SB), NOSPLIT, $0-40
	MOVD dst1+0(FP), R0
	MOVD dst2+8(FP), R1
	MOVD src+16(FP), R2
	MOVD stride+24(FP), R7
	MOVD n+32(FP), R6
	LOAD_COEFFS()

v_mid_loop:
	CMP $8, R6
	BLT v_mid_done

	VLD1 (R2), [V0.B8]
	ADD  R7, R2, R5; VLD1 (R5), [V1.B8]
	ADD  R7, R5, R5; VLD1 (R5), [V2.B8]
	ADD  R7, R5, R5; VLD1 (R5), [V3.B8]

	FILTER8()

	VST1.P [V5.B8], 8(R0)
	VST1.P [V6.B8], 8(R1)

	ADD $8, R2
	SUB $8, R6
	B   v_mid_loop

v_mid_done:
	RET
