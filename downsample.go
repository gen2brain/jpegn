//go:build (!amd64 && !arm64) || noasm

package jpegn

// downsampleRow2x2 box-filters 2x2 sample groups from two source rows (fallback).
func downsampleRow2x2(dst, src0, src1 []byte, n int) {
	downsampleRow2x2Scalar(dst, src0, src1, n)
}
