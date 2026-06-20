//go:build arm64 && !noasm

package jpegn

import (
	"unsafe"
)

//go:noescape
func upsampleNearestNeighborNEON(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

// upsampleNearestNeighbor uses NEON for the common 2x2 case, otherwise falls back to Go.
// The NEON implementation is optimized for source widths that are multiples of 16.
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

	// Use NEON optimized path for the common 2x2 (4:2:0) case.
	if xShift == 1 && yShift == 1 {
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

		upsampleNearestNeighborNEON(unsafe.Pointer(&origPixels[0]), unsafe.Pointer(&out[0]), origWidth, origHeight, origStride, c.stride)

		return
	}

	upsampleNearestNeighborScalar(c, width, height)
}

//go:noescape
func upsampleHMiddleNEON(dst, src unsafe.Pointer, n int)

//go:noescape
func upsampleVMiddleNEON(dst1, dst2, src unsafe.Pointer, stride, n int)

// upsampleCatmullRom repeatedly applies 2x horizontal and vertical Catmull-Rom
// upsampling until the component reaches the target dimensions.
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

// upsampleH performs 2x horizontal Catmull-Rom upsampling, using NEON for the
// interior of each row. A width of at least 11 guarantees one full 8-sample NEON
// block in the interior (width-3 >= 8).
func upsampleH(c *component) {
	if c.width < 11 {
		upsampleHScalar(c)

		return
	}

	newWidth := c.width << 1
	out := make([]byte, newWidth*c.height)
	nbulk := (c.width - 3) &^ 7

	for y := 0; y < c.height; y++ {
		in := c.pixels[y*c.stride:]
		o := out[y*newWidth:]

		upsampleHEdges(in, o, c.width)
		if nbulk > 0 {
			upsampleHMiddleNEON(unsafe.Pointer(&o[3]), unsafe.Pointer(&in[0]), nbulk)
		}
		upsampleHMiddle(in, o, c.width, nbulk)
	}

	c.width = newWidth
	c.stride = newWidth
	c.pixels = out
}

// upsampleV performs 2x vertical Catmull-Rom upsampling, using NEON for the
// interior rows. Requires width >= 8 (one full NEON column block) and height >= 4
// (at least one interior output row pair).
func upsampleV(c *component) {
	if c.width < 8 || c.height < 4 {
		upsampleVScalar(c)

		return
	}

	w := c.width
	stride := c.stride
	newHeight := c.height << 1
	out := make([]byte, w*newHeight)
	nbulk := w &^ 7

	upsampleVTopEdge(c.pixels, out, w, stride)

	for y := 0; y < c.height-3; y++ {
		src := c.pixels[y*stride:]
		out1 := out[(2*y+3)*w:]
		out2 := out[(2*y+4)*w:]

		if nbulk > 0 {
			upsampleVMiddleNEON(unsafe.Pointer(&out1[0]), unsafe.Pointer(&out2[0]), unsafe.Pointer(&src[0]), stride, nbulk)
		}
		upsampleVMiddleRowPair(src, out1, out2, w, stride, nbulk)
	}

	src := c.pixels[(c.height-3)*stride:]
	upsampleVBottomEdge(src, out[(2*c.height-3)*w:], out[(2*c.height-2)*w:], out[(2*c.height-1)*w:], w, stride)

	c.height = newHeight
	c.stride = w
	c.pixels = out
}
