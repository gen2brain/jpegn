//go:build arm64 && !noasm

package jpegn

//go:noescape
func quantizeNEON(dst, src, recip, half *[64]int32) uint64

// quantizeBlock divides a natural-order block by the reciprocal table and
// returns a mask of the non-zero positions.
func quantizeBlock(dst, src *[64]int32, recip, half *[64]int32) uint64 {
	return quantizeNEON(dst, src, recip, half)
}
