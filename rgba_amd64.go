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
	// Check if we can use the AVX2 version.
	// It requires the destination buffer to be correctly sized.
	if isAVX2 && len(dst) >= width*height*4 {
		// Calculate the number of full 16-pixel chunks.
		n := width * height
		rem := n % 16
		limit := n - rem

		if limit > 0 {
			ycbcrToRGBAAVX2(dst[:limit*4], y.pixels[:limit], cb.pixels[:limit], cr.pixels[:limit])
		}

		// Process the remainder using the scalar version.
		if rem > 0 {
			// Create temporary component structs for the remainder.
			yRem := &component{pixels: y.pixels[limit:], stride: rem}
			cbRem := &component{pixels: cb.pixels[limit:], stride: rem}
			crRem := &component{pixels: cr.pixels[limit:], stride: rem}
			yCbCrToRGBAScalar(yRem, cbRem, crRem, dst[limit*4:], rem, 1)
		}

		return
	}

	yCbCrToRGBAScalar(y, cb, cr, dst, width, height)
}

// rgbToRGBA converts a 3-component RGB image to a 4-channel RGBA buffer.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	if isAVX2 && len(dst) >= width*height*4 {
		n := width * height
		rem := n % 32
		limit := n - rem

		if limit > 0 {
			rgbToRGBAAVX2(dst[:limit*4], r.pixels[:limit], g.pixels[:limit], b.pixels[:limit])
		}

		if rem > 0 {
			rRem := &component{pixels: r.pixels[limit:], stride: rem}
			gRem := &component{pixels: g.pixels[limit:], stride: rem}
			bRem := &component{pixels: b.pixels[limit:], stride: rem}
			rgbToRGBAScalar(rRem, gRem, bRem, dst[limit*4:], rem, 1)
		}

		return
	}

	rgbToRGBAScalar(r, g, b, dst, width, height)
}

// grayToRGBA converts a single-component grayscale image to a 4-channel RGBA buffer.
func grayToRGBA(c *component, dst []byte, width, height int) {
	if isAVX2 && len(dst) >= width*height*4 {
		n := width * height
		rem := n % 32
		limit := n - rem

		if limit > 0 {
			grayToRGBAAVX2(dst[:limit*4], c.pixels[:limit])
		}

		if rem > 0 {
			cRem := &component{pixels: c.pixels[limit:], stride: rem}
			grayToRGBAScalar(cRem, dst[limit*4:], rem, 1)
		}

		return
	}

	grayToRGBAScalar(c, dst, width, height)
}
