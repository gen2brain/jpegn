//go:build amd64 && !noasm

package jpegn

//go:noescape
func quantizeAVX2(dst, src, recip, half *[64]int32)

// quantizeBlock divides a natural-order block by the reciprocal table.
func quantizeBlock(dst, src *[64]int32, recip, half *[64]int32) {
	if isAVX2 {
		quantizeAVX2(dst, src, recip, half)

		return
	}

	quantizeBlockScalar(dst, src, recip, half)
}
