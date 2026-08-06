package jpegn

// quantizeBlockScalar divides a natural-order block by the reciprocal table.
func quantizeBlockScalar(dst, src *[64]int32, recip, half *[64]int32) {
	for i := 0; i < 64; i++ {
		v := src[i]
		sign := v >> 31
		a := (v ^ sign) - sign

		q := int32((int64(a+half[i]) * int64(recip[i])) >> quantShift)
		if q > 1023 {
			q = 1023
		}

		dst[i] = (q ^ sign) - sign
	}
}
