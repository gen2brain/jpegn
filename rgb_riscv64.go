//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func rgbToYCbCrRowRVV(dstY, dstCb, dstCr, src *byte, n int)

// rgbToYCbCrRow converts n pixels with a four byte stride to YCbCr planes.
func rgbToYCbCrRow(dstY, dstCb, dstCr, src []byte, n int) {
	if n == 0 {
		return
	}

	rgbToYCbCrRowRVV(&dstY[0], &dstCb[0], &dstCr[0], &src[0], n)
}
