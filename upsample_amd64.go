//go:build amd64 && !noasm

package jpegn

import "unsafe"

//go:noescape
func upsampleNearestNeighborAVX2(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

// upsampleNearestNeighbor uses AVX2 for the common 2x2 case, otherwise falls back to Go.
// The AVX2 implementation is optimized for source widths that are multiples of 16.
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

	// Use AVX2 optimized path for the common 2x2 (4:2:0) case.
	// A minimum width of 16 ensures at least one full XMM register is processed.
	if isAVX2 && xShift == 1 && yShift == 1 {
		origPixels := c.pixels
		origStride := c.stride
		origWidth := c.width
		origHeight := c.height

		out := make([]byte, tempWidth*tempHeight)

		c.pixels = out
		c.width = tempWidth
		c.height = tempHeight
		c.stride = tempWidth

		upsampleNearestNeighborAVX2(unsafe.Pointer(&origPixels[0]), unsafe.Pointer(&out[0]), origWidth, origHeight, origStride, c.stride)

		return
	}

	upsampleNearestNeighborGeneric(c, width, height)
}

//go:noescape
func upsampleHAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleVAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

// upsampleCatmullRom dispatches to AVX2-optimized horizontal and vertical resampling functions
// if available, otherwise falls back to the generic Go implementation.
func upsampleCatmullRom(c *component, width, height int) {
	// The upsampleH and upsampleV functions handle the dispatch logic based on isAVX2 and dimensions.
	for c.width < width || c.height < height {
		if c.width < width {
			upsampleH(c)
		}

		if c.height < height {
			upsampleV(c)
		}
	}
}

func upsampleH(c *component) {
	// A minimum width of 19 (16 iterations + 3 edge pixels) allows the AVX2 main loop to run at least once.
	// This provides a good balance between optimization and overhead.
	if isAVX2 && c.width >= 19 {
		newWidth := c.width << 1
		out := make([]byte, newWidth*c.height)

		upsampleHAVX2(unsafe.Pointer(&out[0]), unsafe.Pointer(&c.pixels[0]), c.width, c.height, newWidth, c.stride)

		c.width = newWidth
		c.stride = newWidth
		c.pixels = out

		return
	}

	// Fallback to Go implementation if AVX2 is not used or width is small.
	upsampleHGeneric(c)
}

func upsampleV(c *component) {
	// For the vertical AVX2 optimization, we need sufficient width (>= 16) to leverage SIMD,
	// and sufficient height (>= 4) for the main loop to run. We use a heuristic of >= 16 for height as well.
	if isAVX2 && c.width >= 16 && c.height >= 16 {
		newHeight := c.height << 1
		out := make([]byte, c.width*newHeight)

		// dstStride is the new stride (c.width), srcStride is the original stride.
		upsampleVAVX2(unsafe.Pointer(&out[0]), unsafe.Pointer(&c.pixels[0]), c.width, c.height, c.width, c.stride)

		c.height = newHeight
		c.stride = c.width
		c.pixels = out

		return
	}

	// Fallback to Go implementation.
	upsampleVGeneric(c)
}
