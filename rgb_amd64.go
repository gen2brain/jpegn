//go:build amd64 && !noasm

package jpegn

//go:noescape
func rgbToYCbCrAVX2(dstY, dstCb, dstCr, src []byte, n int)

// rgbToYCbCrRow converts n pixels with a four byte stride to YCbCr planes.
func rgbToYCbCrRow(dstY, dstCb, dstCr, src []byte, n int) {
	if isAVX2 {
		if v := n &^ 15; v > 0 {
			rgbToYCbCrAVX2(dstY, dstCb, dstCr, src, v)

			dstY, dstCb, dstCr = dstY[v:], dstCb[v:], dstCr[v:]
			src = src[v*4:]
			n -= v
		}
	}

	rgbToYCbCrRowScalar(dstY, dstCb, dstCr, src, n)
}
