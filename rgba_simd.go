//go:build (amd64 || arm64) && !noasm

package jpegn

// The SIMD RGBA kernels are stride-unaware, but components may have stride > width
// at conversion time (non-MCU-aligned full-res planes). These drivers feed the
// kernels one row at a time, handling each row's sub-block tail with the scalar path.

// ycbcrToRGBARows drives a YCbCr->RGBA kernel over `block`-pixel chunks per row.
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

// rgbToRGBARows drives an RGB->RGBA kernel over `block`-pixel chunks per row.
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

// grayToRGBARows drives a grayscale->RGBA kernel over `block`-pixel chunks per row.
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
