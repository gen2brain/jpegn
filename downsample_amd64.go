//go:build amd64 && !noasm

package jpegn

//go:noescape
func downsampleRow2x2AVX2(dst, src0, src1 *byte, n int)

//go:noescape
func downsampleRow2x2SSE(dst, src0, src1 *byte, n int)

// downsampleRow2x2 box-filters 2x2 sample groups from two source rows.
func downsampleRow2x2(dst, src0, src1 []byte, n int) {
	if hasAVX2 && n >= 16 {
		k := n &^ 15
		downsampleRow2x2AVX2(&dst[0], &src0[0], &src1[0], k)

		if k < n {
			downsampleRow2x2Scalar(dst[k:], src0[k*2:], src1[k*2:], n-k)
		}

		return
	}

	if hasSSE4 && n >= 8 {
		k := n &^ 7
		downsampleRow2x2SSE(&dst[0], &src0[0], &src1[0], k)

		if k < n {
			downsampleRow2x2Scalar(dst[k:], src0[k*2:], src1[k*2:], n-k)
		}

		return
	}

	downsampleRow2x2Scalar(dst, src0, src1, n)
}
