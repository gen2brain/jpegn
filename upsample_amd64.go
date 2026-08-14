//go:build amd64 && !noasm

package jpegn

import "unsafe"

//go:noescape
func upsampleNearestNeighborAVX2(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

//go:noescape
func upsampleNearestNeighborSSE(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

// upsampleNearestNeighbor uses SIMD for the common 2x2 case, otherwise falls back to Go.
func upsampleNearestNeighbor(c *component, width, height int) {
	var xShift, yShift uint
	tempWidth := c.width
	tempHeight := c.height

	for tempWidth < width {
		tempWidth <<= 1
		xShift++
	}

	for tempHeight < height {
		tempHeight <<= 1
		yShift++
	}

	if tempWidth == c.width && tempHeight == c.height {
		return
	}

	// Use the SIMD path for the common 2x2 (4:2:0) case.
	if (hasAVX2 || hasSSE4) && xShift == 1 && yShift == 1 {
		origPixels := c.pixels
		origStride := c.stride
		origWidth := c.width
		origHeight := c.height

		if origWidth <= 0 || origHeight <= 0 || len(origPixels) == 0 {
			upsampleNearestNeighborScalar(c, width, height)

			return
		}

		out := make([]byte, tempWidth*tempHeight)

		c.pixels = out
		c.width = tempWidth
		c.height = tempHeight
		c.stride = tempWidth

		if hasAVX2 {
			upsampleNearestNeighborAVX2(unsafe.Pointer(&origPixels[0]), unsafe.Pointer(&out[0]), origWidth, origHeight, origStride, c.stride)

			return
		}

		upsampleNearestNeighborSSE(unsafe.Pointer(&origPixels[0]), unsafe.Pointer(&out[0]), origWidth, origHeight, origStride, c.stride)

		return
	}

	upsampleNearestNeighborScalar(c, width, height)
}

//go:noescape
func upsampleHAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleVAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleHSSE(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleVSSE(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

// upsampleCatmullRom dispatches to the SIMD horizontal and vertical resampling
// functions if available, otherwise falls back to the generic Go implementation.
func upsampleCatmullRom(c *component, width, height int) {
	for c.width < width || c.height < height {
		if c.width < width {
			upsampleH(c)
		}

		if c.height < height {
			upsampleV(c)
		}
	}
}

// upsampleHRun doubles the width in place with kernel.
func upsampleHRun(c *component, kernel func(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)) {
	newWidth := c.width << 1
	out := make([]byte, newWidth*c.height)

	kernel(unsafe.Pointer(&out[0]), unsafe.Pointer(&c.pixels[0]), c.width, c.height, newWidth, c.stride)

	c.width = newWidth
	c.stride = newWidth
	c.pixels = out
}

// upsampleVRun doubles the height in place with kernel.
func upsampleVRun(c *component, kernel func(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)) {
	newHeight := c.height << 1
	out := make([]byte, c.width*newHeight)

	kernel(unsafe.Pointer(&out[0]), unsafe.Pointer(&c.pixels[0]), c.width, c.height, c.width, c.stride)

	c.height = newHeight
	c.stride = c.width
	c.pixels = out
}

// upsampleH needs 3 edge pixels plus one full vector block for the main loop to run.
func upsampleH(c *component) {
	switch {
	case hasAVX2 && c.width >= 19:
		upsampleHRun(c, upsampleHAVX2)

		return
	case hasSSE4 && c.width >= 11:
		upsampleHRun(c, upsampleHSSE)

		return
	}

	upsampleHScalar(c)
}

// upsampleV needs one full vector block across and enough rows for the main loop.
func upsampleV(c *component) {
	if c.height >= 16 {
		switch {
		case hasAVX2 && c.width >= 16:
			upsampleVRun(c, upsampleVAVX2)

			return
		case hasSSE4 && c.width >= 8:
			upsampleVRun(c, upsampleVSSE)

			return
		}
	}

	upsampleVScalar(c)
}
