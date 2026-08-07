//go:build (!amd64 && !arm64) || noasm

package jpegn

// quantizeBlock divides a natural-order block by the reciprocal table and
// returns a mask of the non-zero positions.
func quantizeBlock(dst, src *[64]int32, recip, half *[64]int32) uint64 {
	return quantizeBlockScalar(dst, src, recip, half)
}
