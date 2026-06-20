//go:build amd64 && !noasm

package jpegn

//go:noescape
func ycbcrToRGBAAVX2(dst, y, cb, cr []byte)

//go:noescape
func rgbToRGBAAVX2(dst, r, g, b []byte)

//go:noescape
func grayToRGBAAVX2(dst, gray []byte)

// yCbCrToRGBA converts a 3-component YCbCr image to a 4-channel RGBA buffer.
func yCbCrToRGBA(y, cb, cr *component, dst []byte, width, height int) {
	if isAVX2 && width > 0 && height > 0 && len(dst) >= width*height*4 {
		ycbcrToRGBARows(y, cb, cr, dst, width, height, 16, ycbcrToRGBAAVX2)

		return
	}

	yCbCrToRGBAScalar(y, cb, cr, dst, width, height)
}

// rgbToRGBA converts a 3-component RGB image to a 4-channel RGBA buffer.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	if isAVX2 && width > 0 && height > 0 && len(dst) >= width*height*4 {
		rgbToRGBARows(r, g, b, dst, width, height, 32, rgbToRGBAAVX2)

		return
	}

	rgbToRGBAScalar(r, g, b, dst, width, height)
}

// grayToRGBA converts a single-component grayscale image to a 4-channel RGBA buffer.
func grayToRGBA(c *component, dst []byte, width, height int) {
	if isAVX2 && width > 0 && height > 0 && len(dst) >= width*height*4 {
		grayToRGBARows(c, dst, width, height, 32, grayToRGBAAVX2)

		return
	}

	grayToRGBAScalar(c, dst, width, height)
}
