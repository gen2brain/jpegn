package jpegn

// rgbToYCbCrRowScalar converts n pixels with a four byte stride to YCbCr planes.
func rgbToYCbCrRowScalar(dstY, dstCb, dstCr, src []byte, n int) {
	for i, o := 0, 0; i < n; i, o = i+1, o+4 {
		dstY[i], dstCb[i], dstCr[i] = rgbToYCbCr(src[o], src[o+1], src[o+2])
	}
}
