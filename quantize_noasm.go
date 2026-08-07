package jpegn

// quantizeBlockScalar divides a natural-order block by the reciprocal table and
// returns a mask of the non-zero positions.
func quantizeBlockScalar(dst, src *[64]int32, recip, half *[64]int32) uint64 {
	nz := uint64(0)

	for i := 0; i < 64; i++ {
		v := src[i]
		sign := v >> 31
		a := (v ^ sign) - sign

		q := int32((int64(a+half[i]) * int64(recip[i])) >> quantShift)
		if q > 1023 {
			q = 1023
		}

		v = (q ^ sign) - sign
		dst[i] = v

		if v != 0 {
			nz |= 1 << uint(i)
		}
	}

	return nz
}

// nonZeroMask returns a bit per non-zero coefficient of a block.
func nonZeroMask(blk *[64]int32) uint64 {
	nz := uint64(0)

	for i := 0; i < 64; i++ {
		v := blk[i]
		nz |= uint64(uint32((v|-v)>>31)&1) << uint(i)
	}

	return nz
}
