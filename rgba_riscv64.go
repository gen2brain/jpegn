//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func yCbCrToRGBARowRVV(dst, y, cb, cr *byte, n int)

//go:noescape
func rgbToRGBARowRVV(dst, sr, sg, sb *byte, n int)

//go:noescape
func grayToRGBARowRVV(dst, gray *byte, n int)

// yCbCrToRGBA converts a 3-component YCbCr image to a 4-channel RGBA buffer.
func yCbCrToRGBA(y, cb, cr *component, dst []byte, width, height int) {
	if width <= 0 || height <= 0 || len(dst) < width*height*4 {
		yCbCrToRGBAScalar(y, cb, cr, dst, width, height)

		return
	}

	py, pcb, pcr, off := 0, 0, 0, 0

	for i := 0; i < height; i++ {
		yCbCrToRGBARowRVV(&dst[off], &y.pixels[py], &cb.pixels[pcb], &cr.pixels[pcr], width)

		py += y.stride
		pcb += cb.stride
		pcr += cr.stride
		off += width * 4
	}
}

// rgbToRGBA converts a 3-component RGB image to a 4-channel RGBA buffer.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	if width <= 0 || height <= 0 || len(dst) < width*height*4 {
		rgbToRGBAScalar(r, g, b, dst, width, height)

		return
	}

	pr, pg, pb, off := 0, 0, 0, 0

	for i := 0; i < height; i++ {
		rgbToRGBARowRVV(&dst[off], &r.pixels[pr], &g.pixels[pg], &b.pixels[pb], width)

		pr += r.stride
		pg += g.stride
		pb += b.stride
		off += width * 4
	}
}

// grayToRGBA converts a single-component grayscale image to a 4-channel RGBA buffer.
func grayToRGBA(c *component, dst []byte, width, height int) {
	if width <= 0 || height <= 0 || len(dst) < width*height*4 {
		grayToRGBAScalar(c, dst, width, height)

		return
	}

	src, off := 0, 0

	for i := 0; i < height; i++ {
		grayToRGBARowRVV(&dst[off], &c.pixels[src], width)

		src += c.stride
		off += width * 4
	}
}
