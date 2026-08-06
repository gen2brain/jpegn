package jpegn

// Forward Discrete Cosine Transform (Pure Go Implementation)

// Constants for the integer FDCT (Loeffler, Ligtenberg and Moschytz).
const (
	fdctConstBits = 13
	fdctPass1Bits = 2

	f0298631336 = 2446
	f0390180644 = 3196
	f0541196100 = 4433
	f0765366865 = 6270
	f0899976223 = 7373
	f1175875602 = 9633
	f1501321110 = 12299
	f1847759065 = 15137
	f1961570560 = 16069
	f2053119869 = 16819
	f2562915447 = 20995
	f3072711026 = 25172
)

// descale rounds x down by n fractional bits.
func descale(x int32, n uint) int32 {
	return (x + 1<<(n-1)) >> n
}

// fdctScalar loads samples, level shifts them and transforms, scaled up by 8.
func fdctScalar(blk *[64]int32, src []byte, stride int) {
	off := 0

	for y := 0; y < 64; y += 8 {
		row := src[off : off+8 : off+8]
		b := blk[y : y+8 : y+8]

		b[0] = int32(row[0]) - 128
		b[1] = int32(row[1]) - 128
		b[2] = int32(row[2]) - 128
		b[3] = int32(row[3]) - 128
		b[4] = int32(row[4]) - 128
		b[5] = int32(row[5]) - 128
		b[6] = int32(row[6]) - 128
		b[7] = int32(row[7]) - 128

		off += stride
	}

	for i := 0; i < 64; i += 8 {
		b := blk[i : i+8 : i+8]
		_ = b[7]

		tmp0 := b[0] + b[7]
		tmp7 := b[0] - b[7]
		tmp1 := b[1] + b[6]
		tmp6 := b[1] - b[6]
		tmp2 := b[2] + b[5]
		tmp5 := b[2] - b[5]
		tmp3 := b[3] + b[4]
		tmp4 := b[3] - b[4]

		tmp10 := tmp0 + tmp3
		tmp13 := tmp0 - tmp3
		tmp11 := tmp1 + tmp2
		tmp12 := tmp1 - tmp2

		b[0] = (tmp10 + tmp11) << fdctPass1Bits
		b[4] = (tmp10 - tmp11) << fdctPass1Bits

		z1 := (tmp12 + tmp13) * f0541196100
		b[2] = descale(z1+tmp13*f0765366865, fdctConstBits-fdctPass1Bits)
		b[6] = descale(z1-tmp12*f1847759065, fdctConstBits-fdctPass1Bits)

		z1 = tmp4 + tmp7
		z2 := tmp5 + tmp6
		z3 := tmp4 + tmp6
		z4 := tmp5 + tmp7
		z5 := (z3 + z4) * f1175875602

		tmp4 *= f0298631336
		tmp5 *= f2053119869
		tmp6 *= f3072711026
		tmp7 *= f1501321110
		z1 *= -f0899976223
		z2 *= -f2562915447
		z3 = z5 - z3*f1961570560
		z4 = z5 - z4*f0390180644

		b[7] = descale(tmp4+z1+z3, fdctConstBits-fdctPass1Bits)
		b[5] = descale(tmp5+z2+z4, fdctConstBits-fdctPass1Bits)
		b[3] = descale(tmp6+z2+z3, fdctConstBits-fdctPass1Bits)
		b[1] = descale(tmp7+z1+z4, fdctConstBits-fdctPass1Bits)
	}

	for i := 0; i < 8; i++ {
		b := blk[i:]
		_ = b[56]

		tmp0 := b[0] + b[56]
		tmp7 := b[0] - b[56]
		tmp1 := b[8] + b[48]
		tmp6 := b[8] - b[48]
		tmp2 := b[16] + b[40]
		tmp5 := b[16] - b[40]
		tmp3 := b[24] + b[32]
		tmp4 := b[24] - b[32]

		tmp10 := tmp0 + tmp3
		tmp13 := tmp0 - tmp3
		tmp11 := tmp1 + tmp2
		tmp12 := tmp1 - tmp2

		b[0] = descale(tmp10+tmp11, fdctPass1Bits)
		b[32] = descale(tmp10-tmp11, fdctPass1Bits)

		z1 := (tmp12 + tmp13) * f0541196100
		b[16] = descale(z1+tmp13*f0765366865, fdctConstBits+fdctPass1Bits)
		b[48] = descale(z1-tmp12*f1847759065, fdctConstBits+fdctPass1Bits)

		z1 = tmp4 + tmp7
		z2 := tmp5 + tmp6
		z3 := tmp4 + tmp6
		z4 := tmp5 + tmp7
		z5 := (z3 + z4) * f1175875602

		tmp4 *= f0298631336
		tmp5 *= f2053119869
		tmp6 *= f3072711026
		tmp7 *= f1501321110
		z1 *= -f0899976223
		z2 *= -f2562915447
		z3 = z5 - z3*f1961570560
		z4 = z5 - z4*f0390180644

		b[56] = descale(tmp4+z1+z3, fdctConstBits+fdctPass1Bits)
		b[40] = descale(tmp5+z2+z4, fdctConstBits+fdctPass1Bits)
		b[24] = descale(tmp6+z2+z3, fdctConstBits+fdctPass1Bits)
		b[8] = descale(tmp7+z1+z4, fdctConstBits+fdctPass1Bits)
	}
}
