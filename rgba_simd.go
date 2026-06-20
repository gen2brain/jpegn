//go:build (amd64 || arm64) && !noasm

package jpegn

// The SIMD RGBA kernels operate on contiguous runs of pixels and are unaware of
// component strides. Components are not guaranteed to be stride-packed at
// conversion time: a full-resolution component of a non-MCU-aligned image keeps
// a stride rounded up to the block grid (e.g. width 487, stride 496). These
// drivers therefore feed the kernels one row at a time over each component's
// real stride, handling the sub-block tail of every row with the scalar path.

// ycbcrToRGBARows drives a YCbCr->RGBA kernel that converts whole blocks of
// `block` pixels. The kernel signature matches the per-architecture assembly.
func ycbcrToRGBARows(y, cb, cr *component, dst []byte, width, height, block int, kernel func(dst, y, cb, cr []byte)) {
	rem := width % block
	limit := width - rem

	for row := 0; row < height; row++ {
		yo := row * y.stride
		cbo := row * cb.stride
		cro := row * cr.stride
		do := row * width * 4

		if limit > 0 {
			kernel(dst[do:do+limit*4], y.pixels[yo:yo+limit], cb.pixels[cbo:cbo+limit], cr.pixels[cro:cro+limit])
		}

		if rem > 0 {
			yRem := &component{pixels: y.pixels[yo+limit : yo+width], stride: rem}
			cbRem := &component{pixels: cb.pixels[cbo+limit : cbo+width], stride: rem}
			crRem := &component{pixels: cr.pixels[cro+limit : cro+width], stride: rem}
			yCbCrToRGBAScalar(yRem, cbRem, crRem, dst[do+limit*4:do+width*4], rem, 1)
		}
	}
}

// rgbToRGBARows drives an RGB->RGBA kernel that converts whole blocks of `block` pixels.
func rgbToRGBARows(r, g, b *component, dst []byte, width, height, block int, kernel func(dst, r, g, b []byte)) {
	rem := width % block
	limit := width - rem

	for row := 0; row < height; row++ {
		ro := row * r.stride
		go_ := row * g.stride
		bo := row * b.stride
		do := row * width * 4

		if limit > 0 {
			kernel(dst[do:do+limit*4], r.pixels[ro:ro+limit], g.pixels[go_:go_+limit], b.pixels[bo:bo+limit])
		}

		if rem > 0 {
			rRem := &component{pixels: r.pixels[ro+limit : ro+width], stride: rem}
			gRem := &component{pixels: g.pixels[go_+limit : go_+width], stride: rem}
			bRem := &component{pixels: b.pixels[bo+limit : bo+width], stride: rem}
			rgbToRGBAScalar(rRem, gRem, bRem, dst[do+limit*4:do+width*4], rem, 1)
		}
	}
}

// grayToRGBARows drives a grayscale->RGBA kernel that converts whole blocks of `block` pixels.
func grayToRGBARows(c *component, dst []byte, width, height, block int, kernel func(dst, gray []byte)) {
	rem := width % block
	limit := width - rem

	for row := 0; row < height; row++ {
		co := row * c.stride
		do := row * width * 4

		if limit > 0 {
			kernel(dst[do:do+limit*4], c.pixels[co:co+limit])
		}

		if rem > 0 {
			cRem := &component{pixels: c.pixels[co+limit : co+width], stride: rem}
			grayToRGBAScalar(cRem, dst[do+limit*4:do+width*4], rem, 1)
		}
	}
}
