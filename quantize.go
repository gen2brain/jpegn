//go:build (!amd64 && !arm64) || noasm

package jpegn

// quantizeBlock divides a natural-order block by the reciprocal table.
func quantizeBlock(dst, src *[64]int32, recip, half *[64]int32) {
	quantizeBlockScalar(dst, src, recip, half)
}
