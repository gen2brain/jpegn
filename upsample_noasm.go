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

// upsampleCatmullRom doubles c with the 4-tap Catmull-Rom filter until it covers width x height.
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

// upsampleH doubles the width of c with the 4-tap Catmull-Rom filter.
func upsampleH(c *component) { upsampleHWith(c, upsampleHRows) }

// upsampleV doubles the height of c with the 4-tap Catmull-Rom filter.
func upsampleV(c *component) { upsampleVWith(c, upsampleVRowPair) }

// upsampleCubic reports whether Catmull-Rom applies to c: its edge taps need
// three samples along each axis it doubles.
func upsampleCubic(c *component, width, height int, method UpsampleMethod) bool {
	return method == CatmullRom && (c.width >= width || c.width >= 3) && (c.height >= height || c.height >= 3)
}

// upsampleHEdges writes the three left and three right boundary samples of a row.
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

// upsampleHMiddle computes interior samples for source positions x in [start, width-3).
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

// upsampleHWith doubles the width of c, filtering its rows with rows.
func upsampleHWith(c *component, rows func(dst, src []byte, w, h, dstStride, srcStride int)) {
	newWidth := c.width << 1
	out := make([]byte, newWidth*c.height)

	rows(out, c.pixels, c.width, c.height, newWidth, c.stride)

	c.width = newWidth
	c.stride = c.width
	c.pixels = out
}

// upsampleVTopEdge writes the first three output rows from the first three source rows.
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

// upsampleVBottomEdge writes the last three output rows from source rows H-3..H-1.
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

// upsampleVMiddleRowPair computes the interior output row pair (cols [start, w))
// from four consecutive source rows starting at src.
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

// upsampleVWith doubles the height of c with symmetric boundary conditions,
// filtering interior row pairs with pair.
func upsampleVWith(c *component, pair func(src, out1, out2 []byte, w, stride int)) {
	w := c.width
	stride := c.stride
	newHeight := c.height << 1

	out := make([]byte, w*newHeight)

	upsampleVTopEdge(c.pixels, out, w, stride)

	for y := 0; y < c.height-3; y++ {
		pair(c.pixels[y*stride:], out[(2*y+3)*w:], out[(2*y+4)*w:], w, stride)
	}

	src := c.pixels[(c.height-3)*stride:]
	upsampleVBottomEdge(src, out[(2*c.height-3)*w:], out[(2*c.height-2)*w:], out[(2*c.height-1)*w:], w, stride)

	c.height = newHeight
	c.stride = c.width
	c.pixels = out
}

// upsampleNearestNeighbor upsamples c by power-of-two factors with sample replication.
func upsampleNearestNeighbor(c *component, width, height int) {
	upsampleNearestNeighborWith(c, width, height, upsampleNearestNeighborRows)
}

// upsampleNearestNeighborWith replicates samples, using rows for the common 2x2 (4:2:0) case.
func upsampleNearestNeighborWith(c *component, width, height int, rows func(dst, src []byte, w, h, dstStride, srcStride int)) {
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

	out := make([]byte, tempWidth*tempHeight)

	if xShift == 1 && yShift == 1 {
		rows(out, c.pixels, c.width, c.height, tempWidth, c.stride)
	} else {
		for y := 0; y < tempHeight; y++ {
			lin := c.pixels[(y>>yShift)*c.stride:]
			lout := out[y*tempWidth:]

			for x := 0; x < tempWidth; x++ {
				lout[x] = lin[x>>xShift]
			}
		}
	}

	c.width = tempWidth
	c.height = tempHeight
	c.stride = tempWidth
	c.pixels = out
}

// upsampleHRowsScalar writes the 2x horizontal Catmull-Rom upsampling of h rows.
func upsampleHRowsScalar(dst, src []byte, w, h, dstStride, srcStride int) {
	for y := 0; y < h; y++ {
		in := src[y*srcStride:]
		o := dst[y*dstStride:]

		upsampleHEdges(in, o, w)
		upsampleHMiddle(in, o, w, 0)
	}
}

// upsampleNearestNeighborRowsScalar writes each of h source rows doubled
// horizontally into two consecutive destination rows.
func upsampleNearestNeighborRowsScalar(dst, src []byte, w, h, dstStride, srcStride int) {
	for y := 0; y < h; y++ {
		srcRow := src[y*srcStride : y*srcStride+w]
		dstRow := dst[2*y*dstStride : 2*y*dstStride+2*w]

		for x, val := range srcRow {
			// A two-byte subslice lets both stores share one bounds check.
			d := dstRow[2*x : 2*x+2]
			d[0], d[1] = val, val
		}

		copy(dst[(2*y+1)*dstStride:], dstRow)
	}
}
