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

// upsampleCatmullRom is a placeholder that falls back to the generic Go implementation.
func upsampleCatmullRom(c *component, width, height int) {
	upsampleCatmullRomScalar(c, width, height)
}

// upsampleH is a placeholder that falls back to the generic Go implementation.
func upsampleH(c *component) {
	upsampleHScalar(c)
}

// upsampleV is a placeholder that falls back to the generic Go implementation.
func upsampleV(c *component) {
	upsampleVScalar(c)
}
