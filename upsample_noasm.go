package jpegn

// Upsampling

// Constants for a 4-tap Catmull-Rom upsampling filter.
const (
	cf4A = -9
	cf4B = 111
	cf4C = 29
	cf4D = -3
	cf3A = 28
	cf3B = 109
	cf3C = -9
	cf3X = 104
	cf3Y = 27
	cf3Z = -3
	cf2A = 139
	cf2B = -11
)

// cf applies the final step of the filter calculation.
func cf(x int32) byte {
	return clamp((x + 64) >> 7)
}

// upsampleCatmullRomScalar performs upsampling by using the 4-tap Catmull-Rom interpolation filter.
func upsampleCatmullRomScalar(c *component, width, height int) {
	for c.width < width || c.height < height {
		if c.width < width {
			upsampleHScalar(c)
		}

		if c.height < height {
			upsampleVScalar(c)
		}
	}
}

// upsampleHEdges writes the three left and three right boundary output samples
// for a single horizontally-upsampled row. in is the source row (length >= width)
// and out is the destination row (length 2*width).
func upsampleHEdges(in, out []byte, width int) {
	p0L := int32(in[0])
	p1L := int32(in[1])
	p2L := int32(in[2])

	out[0] = cf(cf2A*p0L + cf2B*p1L)
	out[1] = cf(cf3X*p0L + cf3Y*p1L + cf3Z*p2L)
	out[2] = cf(cf3A*p0L + cf3B*p1L + cf3C*p2L)

	newWidth := width << 1
	p0R := int32(in[width-1]) // P(W-1)
	p1R := int32(in[width-2]) // P(W-2)
	p2R := int32(in[width-3]) // P(W-3)

	out[newWidth-3] = cf(cf3A*p0R + cf3B*p1R + cf3C*p2R) // mirrors out[2]
	out[newWidth-2] = cf(cf3X*p0R + cf3Y*p1R + cf3Z*p2R) // mirrors out[1]
	out[newWidth-1] = cf(cf2A*p0R + cf2B*p1R)            // mirrors out[0]
}

// upsampleHMiddle computes the interior output samples of a horizontally-upsampled
// row for source positions x in [start, width-3). It writes out[2x+3] and out[2x+4].
func upsampleHMiddle(in, out []byte, width, start int) {
	for x := start; x < width-3; x++ {
		p0 := int32(in[x])
		p1 := int32(in[x+1])
		p2 := int32(in[x+2])
		p3 := int32(in[x+3])

		out[(x<<1)+3] = cf(cf4A*p0 + cf4B*p1 + cf4C*p2 + cf4D*p3)
		out[(x<<1)+4] = cf(cf4D*p0 + cf4C*p1 + cf4B*p2 + cf4A*p3)
	}
}

// upsampleHScalar performs a 2x horizontal upsampling on a component's pixel data
// using a 4-tap Catmull-Rom interpolation filter.
func upsampleHScalar(c *component) {
	newWidth := c.width << 1
	out := make([]byte, newWidth*c.height)

	// The SOF decoder guarantees width >= 3 if upsampling is required.
	for y := 0; y < c.height; y++ {
		in := c.pixels[y*c.stride:]
		o := out[y*newWidth:]

		upsampleHEdges(in, o, c.width)
		upsampleHMiddle(in, o, c.width, 0)
	}

	c.width = newWidth
	c.stride = c.width
	c.pixels = out
}

// upsampleVTopEdge writes the first three output rows (0, 1, 2) of a
// vertically-upsampled component from the first three source rows.
func upsampleVTopEdge(src, out []byte, w, stride int) {
	s1 := stride
	s2 := stride << 1

	for x := 0; x < w; x++ {
		p0 := int32(src[x])
		p1 := int32(src[x+s1])
		p2 := int32(src[x+s2])

		out[x] = cf(cf2A*p0 + cf2B*p1)
		out[w+x] = cf(cf3X*p0 + cf3Y*p1 + cf3Z*p2)
		out[2*w+x] = cf(cf3A*p0 + cf3B*p1 + cf3C*p2)
	}
}

// upsampleVBottomEdge writes the last three output rows of a vertically-upsampled
// component. src points at source row H-3; out1/out2/out3 are output rows
// 2H-3, 2H-2, 2H-1 respectively.
func upsampleVBottomEdge(src, out1, out2, out3 []byte, w, stride int) {
	s1 := stride
	s2 := stride << 1

	for x := 0; x < w; x++ {
		p0 := int32(src[x+s2]) // R(H-1)
		p1 := int32(src[x+s1]) // R(H-2)
		p2 := int32(src[x])    // R(H-3)

		out1[x] = cf(cf3A*p0 + cf3B*p1 + cf3C*p2) // mirrors row 2
		out2[x] = cf(cf3X*p0 + cf3Y*p1 + cf3Z*p2) // mirrors row 1
		out3[x] = cf(cf2A*p0 + cf2B*p1)           // mirrors row 0
	}
}

// upsampleVMiddleRowPair computes the interior output row pair for a source row,
// reading four consecutive source rows starting at src and writing out1 (the even
// output row) and out2 (the odd output row) for columns in [start, w).
func upsampleVMiddleRowPair(src, out1, out2 []byte, w, stride, start int) {
	s1 := stride
	s2 := stride << 1
	s3 := s2 + s1

	for x := start; x < w; x++ {
		p0 := int32(src[x])
		p1 := int32(src[x+s1])
		p2 := int32(src[x+s2])
		p3 := int32(src[x+s3])

		out1[x] = cf(cf4A*p0 + cf4B*p1 + cf4C*p2 + cf4D*p3)
		out2[x] = cf(cf4D*p0 + cf4C*p1 + cf4B*p2 + cf4A*p3)
	}
}

// upsampleVScalar performs a 2x vertical upsampling on a component's pixel data
// using a 4-tap Catmull-Rom filter and symmetric boundary conditions.
func upsampleVScalar(c *component) {
	w := c.width
	stride := c.stride
	newHeight := c.height << 1

	out := make([]byte, w*newHeight)

	// The SOF decoder guarantees height >= 3 if upsampling is required.
	upsampleVTopEdge(c.pixels, out, w, stride)

	for y := 0; y < c.height-3; y++ {
		src := c.pixels[y*stride:]
		out1 := out[(2*y+3)*w:]
		out2 := out[(2*y+4)*w:]

		upsampleVMiddleRowPair(src, out1, out2, w, stride, 0)
	}

	src := c.pixels[(c.height-3)*stride:]
	upsampleVBottomEdge(src, out[(2*c.height-3)*w:], out[(2*c.height-2)*w:], out[(2*c.height-1)*w:], w, stride)

	c.height = newHeight
	c.stride = c.width
	c.pixels = out
}

// upsampleNearestNeighborScalar performs upsampling by integer factors using the nearest-neighbor algorithm.
// This method is fast but produces lower-quality, "blocky" results compared to Catmull-Rom interpolation.
func upsampleNearestNeighborScalar(c *component, width, height int) {
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

	// If no upsampling is needed, return early.
	if tempWidth == c.width && tempHeight == c.height {
		return
	}

	// Specialized implementation for the common 4:2:0 case (2x2 upsampling).
	if xShift == 1 && yShift == 1 {
		origWidth := c.width
		origHeight := c.height
		origStride := c.stride
		origPixels := c.pixels

		c.width = tempWidth
		c.height = tempHeight
		c.stride = tempWidth

		out := make([]byte, c.width*c.height)
		c.pixels = out

		// Optimized 2x2 upsampling (Nearest Neighbor)
		for y := 0; y < origHeight; y++ {
			srcRow := origPixels[y*origStride : y*origStride+origWidth]
			// Calculate destination rows for the current source row (y) and the next one (y+1).
			dstRow1 := out[2*y*c.stride : (2*y+1)*c.stride]
			dstRow2 := out[(2*y+1)*c.stride : (2*y+2)*c.stride]

			// 2x horizontal expansion
			k := 0

			// Ensure bounds checks are eliminated in the inner loop.
			if origWidth > 0 {
				_ = srcRow[origWidth-1]
				// We know dstRow1 has length c.stride (tempWidth), and 2*origWidth <= tempWidth.
				_ = dstRow1[2*origWidth-1]
			}

			for x := 0; x < origWidth; x++ {
				val := srcRow[x]
				dstRow1[k] = val
				dstRow1[k+1] = val
				k += 2
			}

			// 2x vertical expansion (copy the expanded row)
			copy(dstRow2, dstRow1)
		}

		return
	}

	// Generic implementation
	c.width = tempWidth
	c.height = tempHeight

	out := make([]byte, c.width*c.height)

	for y := 0; y < c.height; y++ {
		// Find the source row by right-shifting y
		lin := c.pixels[(y>>yShift)*c.stride:]
		lout := out[y*c.width:]

		for x := 0; x < c.width; x++ {
			// Find the source pixel by right-shifting x
			lout[x] = lin[x>>xShift]
		}
	}

	c.stride = c.width
	c.pixels = out
}
