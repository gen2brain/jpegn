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
    