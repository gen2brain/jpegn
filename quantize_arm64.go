//go:build arm64 && !noasm

package jpegn

//go:noescape
func quantizeNEON(dst, src, recip, half *[64]int32)

// quantizeBlock divides a natural-order block by the reciprocal table.
func quantizeBlock(dst, src *[64]int32, recip, half *[64]int32) {
	quantizeNEON(dst, src, recip, half)
}
