//go:build amd64 && !noasm

package jpegn

//go:noescape
func idct4x4AVX2(blk *[64]int32, out *byte, stride int)

// idctScaled dispatches on the scale denominator, which must be 1, 2, 4 or 8.
func idctScaled(blk *[64]int32, out []byte, outOffset int, stride int, scaleDenom int) {
	switch scaleDenom {
	case 2:
		if isAVX2 {
			idct4x4AVX2(blk, &out[outOffset], stride)

			return
		}

		idct8x8To4x4(blk, out, outOffset, stride)
	case 4:
		idct8x8To2x2(blk, out, outOffset, stride)
	case 8:
		idct8x8To1x1(blk, out, outOffset, stride)
	default:
		idct(blk, out, outOffset, stride)
	}
}
