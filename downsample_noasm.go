package jpegn

// downsampleRow2x2Scalar box-filters 2x2 sample groups into n samples.
func downsampleRow2x2Scalar(dst, src0, src1 []byte, n int) {
	d := dst[:n]
	s0, s1 := src0[:n*2], src1[:n*2]

	for i := range d {
		x := i * 2
		d[i] = byte((int32(s0[x]) + int32(s0[x+1]) + int32(s1[x]) + int32(s1[x+1]) + 2) >> 2)
	}
}
