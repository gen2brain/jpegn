//go:build arm64 && !noasm

package jpegn

//go:noescape
func ycbcrToRGBANEON(dst, y, cb, cr []byte)

//go:noescape
func rgbToRGBANEON(dst, r, g, b []byte)

//go:noescape
func grayToRGBANEON(dst, gray []byte)

// yCbCrToRGBA converts a 3-component YCbCr image to a 4-channel RGBA buffer.
func yCbCrToRGBA(y, cb, cr *component, dst []byte, width, height int) {
	if width > 0 && height > 0 && len(dst) >= width*height*4 {
		ycbcrToRGBARows(y, cb, cr, dst, width, height, 8, ycbcrToRGBANEON)

		return
	}

	yCbCrToRGBAScalar(y, cb, cr, dst, width, height)
}

// rgbToRGBA converts a 3-component RGB image to a 4-channel RGBA buffer.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	if width > 0 && height > 0 && len(dst) >= width*height*4 {
		rgbToRGBARows(r, g, b, dst, width, height, 16, rgbToRGBANEON)

		return
	}

	rgbToRGBAScalar(r, g, b, dst, width, height)
}

// grayToRGBA converts a single-component grayscale image to a 4-channel RGBA buffer.
func grayToRGBA(c *component, dst []byte, width, height int) {
	if width > 0 && height > 0 && len(dst) >= width*height*4 {
		grayToRGBARows(c, dst, width, height, 16, grayToRGBANEON)

		return
	}

	grayToRGBAScalar(c, dst, width, height)
}
