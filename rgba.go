package jpegn

// yCbCrToRGBA converts a 3-component YCbCr image to a 4-channel RGBA buffer.
func yCbCrToRGBA(y, cb, cr *component, dst []byte, width, height int) {
	// Pointers to the start of the current row for each component.
	py, pcb, pcr := 0, 0, 0
	// Offset into the destination RGBA buffer.
	rgbaOffset := 0

	for yy := 0; yy < height; yy++ {
		for x := 0; x < width; x++ {
			// YCbCr to RGB conversion with level shift and constants for fast integer math.
			// The y value is shifted to allow for fixed-point arithmetic.
			yVal := int32(y.pixels[py+x]) << 8
			cbVal := int32(cb.pixels[pcb+x]) - 128
			crVal := int32(cr.pixels[pcr+x]) - 128

			// These are the standard JFIF conversion formulas, scaled for integer arithmetic.
			// R = Y + 1.402 * Cr
			// G = Y - 0.344136 * Cb - 0.714136 * Cr
			// B = Y + 1.772 * Cb
			// The constants (359, 88, 183, 454) are scaled approximations.
			r := (yVal + 359*crVal + 128) >> 8
			g := (yVal - 88*cbVal - 183*crVal + 128) >> 8
			b := (yVal + 454*cbVal + 128) >> 8

			// Store the clipped results in the destination buffer.
			dst[rgbaOffset] = clip(r)   // R
			dst[rgbaOffset+1] = clip(g) // G
			dst[rgbaOffset+2] = clip(b) // B
			dst[rgbaOffset+3] = 255     // A (fully opaque)
			rgbaOffset += 4
		}

		// Advance to the next row in each source component.
		py += y.stride
		pcb += cb.stride
		pcr += cr.stride
	}
}

// rgbToRGBA converts a 3-component RGB image to a 4-channel RGBA buffer.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	// Pointers to the start of the current row for each component.
	pr, pg, pb := 0, 0, 0
	// Offset into the destination RGBA buffer.
	rgbaOffset := 0

	for yy := 0; yy < height; yy++ {
		for x := 0; x < width; x++ {
			// Simple copy from separate R, G, B planes to interleaved RGBA.
			dst[rgbaOffset] = r.pixels[pr+x]   // R
			dst[rgbaOffset+1] = g.pixels[pg+x] // G
			dst[rgbaOffset+2] = b.pixels[pb+x] // B
			dst[rgbaOffset+3] = 255            // A (fully opaque)
			rgbaOffset += 4
		}

		// Advance to the next row in each source component.
		pr += r.stride
		pg += g.stride
		pb += b.stride
	}
}

// grayToRGBA converts a single-component grayscale image to a 4-channel RGBA buffer.
func grayToRGBA(c *component, dst []byte, width, height int) {
	// Pointer to the start of the current source row.
	yOffset := 0
	// Offset into the destination RGBA buffer.
	rgbaOffset := 0

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			// For grayscale, R, G, and B are all set to the luminance value.
			lum := c.pixels[yOffset+x]

			dst[rgbaOffset] = lum
			dst[rgbaOffset+1] = lum
			dst[rgbaOffset+2] = lum
			dst[rgbaOffset+3] = 255 // A (fully opaque)
			rgbaOffset += 4
		}

		// Advance to the next source row.
		yOffset += c.stride
	}
}
