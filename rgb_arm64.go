//go:build arm64 && !noasm

package jpegn

// rgbToYCbCrRow converts n pixels with a four byte stride to YCbCr planes.
func rgbToYCbCrRow(dstY, dstCb, dstCr, src []byte, n int) {
	rgbToYCbCrRowScalar(dstY, dstCb, dstCr, src, n)
}
