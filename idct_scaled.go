package jpegn

// idctScaled dispatches to the appropriate IDCT function based on the scale denominator.
// scaleDenom must be 1, 2, 4, or 8.
// When scaleDenom = 1, this calls the existing idct() function which uses optimized assembly implementations on amd64/arm64.
func idctScaled(blk *[64]int32, out []byte, outOffset int, stride int, scaleDenom int) {
	switch scaleDenom {
	case 1:
		// Use existing full 8x8 IDCT (with assembly optimizations on amd64/arm64)
		idct(blk, out, outOffset, stride)
	case 2:
		// 1/2 scaling: 4x4 output
		idct8x8To4x4(blk, out, outOffset, stride)
	case 4:
		// 1/4 scaling: 2x2 output
		idct8x8To2x2(blk, out, outOffset, stride)
	case 8:
		// 1/8 scaling: 1x1 output
		idct8x8To1x1(blk, out, outOffset, stride)
	default:
		// Fallback to full IDCT for invalid scale (shouldn't happen with validation)
		idct(blk, out, outOffset, stride)
	}
}
