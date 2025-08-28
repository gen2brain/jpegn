package jpegn

// Inverse Discrete Cosine Transform (Pure Go Implementation)

// Constants for the AAN fast IDCT algorithm (scaled by 2^11).
const (
	w1 = 2841 // 2048*sqrt(2)*cos(1*pi/16)
	w2 = 2676 // 2048*sqrt(2)*cos(2*pi/16)
	w3 = 2408 // 2048*sqrt(2)*cos(3*pi/16)
	w5 = 1609 // 2048*sqrt(2)*cos(5*pi/16)
	w6 = 1108 // 2048*sqrt(2)*cos(6*pi/16)
	w7 = 565  // 2048*sqrt(2)*cos(7*pi/16)
)

// rowIdct performs a 1D IDCT on a single 8-element row.
func rowIdct(blk *[64]int32, offset int) {
	// Operate on the specific row starting at offset
	b := blk[offset : offset+8]

	// Optimization: Explicitly assert the length of the slice to eliminate bounds checks (BCE).
	_ = b[7]

	var x0, x1, x2, x3, x4, x5, x6, x7, x8 int32

	// Check if AC coefficients are zero (optimization)
	x1 = b[4] << 11
	x2 = b[6]
	x3 = b[2]
	x4 = b[1]
	x5 = b[7]
	x6 = b[5]
	x7 = b[3]

	if (x1 | x2 | x3 | x4 | x5 | x6 | x7) == 0 {
		val := b[0] << 3
		// Optimization: Unroll loop for a DC only case
		b[0] = val
		b[1] = val
		b[2] = val
		b[3] = val
		b[4] = val
		b[5] = val
		b[6] = val
		b[7] = val

		return
	}

	// Load and setup
	x0 = (b[0] << 11) + 128

	// Stage 1
	x8 = w7 * (x4 + x5)
	x4 = x8 + (w1-w7)*x4
	x5 = x8 - (w1+w7)*x5
	x8 = w3 * (x6 + x7)
	x6 = x8 - (w3-w5)*x6
	x7 = x8 - (w3+w5)*x7

	// Stage 2
	x8 = x0 + x1
	x0 -= x1
	x1 = w6 * (x3 + x2)
	x2 = x1 - (w2+w6)*x2
	x3 = x1 + (w2-w6)*x3

	// Stage 3
	x1 = x4 + x6
	x4 -= x6
	x6 = x5 + x7
	x5 -= x7

	// Stage 4
	x7 = x8 + x3
	x8 -= x3
	x3 = x0 + x2
	x0 -= x2

	// Rotation stage
	x2 = (181*(x4+x5) + 128) >> 8
	x4 = (181*(x4-x5) + 128) >> 8

	// Final stage: store the results back into the block
	b[0] = (x7 + x1) >> 8
	b[1] = (x3 + x2) >> 8
	b[2] = (x0 + x4) >> 8
	b[3] = (x8 + x6) >> 8
	b[4] = (x8 - x6) >> 8
	b[5] = (x0 - x4) >> 8
	b[6] = (x3 - x2) >> 8
	b[7] = (x7 - x1) >> 8
}

// colIdct performs a 1D IDCT on a single 8-element column.
func colIdct(blk *[64]int32, offset int, out []byte, outOffset int, stride int) {
	// Optimization: Slice 'out' starting from outOffset to help BCE and simplify indexing.
	// This assumes outOffset is valid (the slice operation will panic if not, which is acceptable).
	if len(out) == 0 {
		return
	}
	out = out[outOffset:]

	var x0, x1, x2, x3, x4, x5, x6, x7, x8 int32

	// Check for an optimization case
	x1 = blk[offset+8*4] << 8
	x2 = blk[offset+8*6]
	x3 = blk[offset+8*2]
	x4 = blk[offset+8*1]
	x5 = blk[offset+8*7]
	x6 = blk[offset+8*5]
	x7 = blk[offset+8*3]

	if (x1 | x2 | x3 | x4 | x5 | x6 | x7) == 0 {
		// DC-only case
		// Hint BCE. We access up to index 7*stride.
		_ = out[7*stride]

		x1 = int32(clip(((blk[offset+8*0] + 32) >> 6) + 128))
		b := byte(x1)

		// Unroll the loop for faster execution.
		currentOutOffset := 0
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b
		currentOutOffset += stride
		out[currentOutOffset] = b

		return
	}

	// Full transform
	x0 = (blk[offset+8*0] << 8) + 8192

	// Stage 1
	x8 = w7*(x4+x5) + 4
	x4 = (x8 + (w1-w7)*x4) >> 3
	x5 = (x8 - (w1+w7)*x5) >> 3
	x8 = w3*(x6+x7) + 4
	x6 = (x8 - (w3-w5)*x6) >> 3
	x7 = (x8 - (w3+w5)*x7) >> 3

	// Stage 2
	x8 = x0 + x1
	x0 -= x1
	x1 = w6*(x3+x2) + 4
	x2 = (x1 - (w2+w6)*x2) >> 3
	x3 = (x1 + (w2-w6)*x3) >> 3

	// Stage 3
	x1 = x4 + x6
	x4 -= x6
	x6 = x5 + x7
	x5 -= x7

	// Stage 4
	x7 = x8 + x3
	x8 -= x3
	x3 = x0 + x2
	x0 -= x2

	// Rotation stage
	x2 = (181*(x4+x5) + 128) >> 8
	x4 = (181*(x4-x5) + 128) >> 8

	// Final stage: store results with proper clipping and level shift
	// Hint BCE.
	_ = out[7*stride]

	// Unroll the loop.
	currentOutOffset := 0
	out[currentOutOffset] = clip(((x7 + x1) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x3 + x2) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x0 + x4) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x8 + x6) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x8 - x6) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x0 - x4) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x3 - x2) >> 14) + 128)
	currentOutOffset += stride
	out[currentOutOffset] = clip(((x7 - x1) >> 14) + 128)
}
