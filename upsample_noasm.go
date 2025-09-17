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

// upsampleCatmullRom performs upsampling by using the 4-tap Catmull-Rom interpolation filter.
func upsampleCatmullRomGeneric(c *component, width, height int) {
	for c.width < width || c.height < height {
		if c.width < width {
			upsampleHGeneric(c)
		}

		if c.height < height {
			upsampleVGeneric(c)
		}
	}
}

// upsampleH performs a 2x horizontal upsampling on a component's pixel data.
// It uses a 4-tap Catmull-Rom interpolation filter.
func upsampleHGeneric(c *component) {
	out := make([]byte, (c.width*c.height)<<1)

	newWidth := c.width << 1
	lin := c.pixels
	lout := out

	// The SOF decoder guarantees width >= 3 if upsampling is required.
	for y := 0; y < c.height; y++ {
		baseIn := y * c.stride
		baseOut := y * newWidth

		// Left-edge boundary conditions (Forward application)
		// Use int32 arithmetic consistently.
		p0L := int32(lin[baseIn+0])
		p1L := int32(lin[baseIn+1])
		p2L := int32(lin[baseIn+2])

		lout[baseOut+0] = cf(cf2A*p0L + cf2B*p1L)
		lout[baseOut+1] = cf(cf3X*p0L + cf3Y*p1L + cf3Z*p2L)
		lout[baseOut+2] = cf(cf3A*p0L + cf3B*p1L + cf3C*p2L)

		// Main loop for the middle part of the image.
		for x := 0; x < c.width-3; x++ {
			p0 := int32(lin[baseIn+x])
			p1 := int32(lin[baseIn+x+1])
			p2 := int32(lin[baseIn+x+2])
			p3 := int32(lin[baseIn+x+3])

			lout[baseOut+(x<<1)+3] = cf(cf4A*p0 + cf4B*p1 + cf4C*p2 + cf4D*p3)
			lout[baseOut+(x<<1)+4] = cf(cf4D*p0 + cf4C*p1 + cf4B*p2 + cf4A*p3)
		}

		// Right-edge boundary conditions (Symmetric/Mirrored application).

		// Pixels in reverse order:
		p0R := int32(lin[baseIn+c.width-1]) // P(W-1)
		p1R := int32(lin[baseIn+c.width-2]) // P(W-2)
		p2R := int32(lin[baseIn+c.width-3]) // P(W-3)

		// lout[-3] mirrors lout[2]
		lout[baseOut+newWidth-3] = cf(cf3A*p0R + cf3B*p1R + cf3C*p2R)
		// lout[-2] mirrors lout[1]
		lout[baseOut+newWidth-2] = cf(cf3X*p0R + cf3Y*p1R + cf3Z*p2R)
		// lout[-1] mirrors lout[0]
		lout[baseOut+newWidth-1] = cf(cf2A*p0R + cf2B*p1R)
	}

	c.width = newWidth
	c.stride = c.width
	c.pixels = out
}

// upsampleV performs a 2x vertical upsampling on a component's pixel data.
// Like upsampleH, it uses a 4-tap Catmull-Rom filter and symmetric boundary conditions.
func upsampleVGeneric(c *component) {
	w := c.width
	s1 := c.stride
	s2 := s1 + s1
	s3 := s2 + s1 // 3 * stride
	newHeight := c.height << 1

	out := make([]byte, w*newHeight)

	// The SOF decoder guarantees height >= 3 if upsampling is required.
	for x := 0; x < w; x++ {
		cin := x
		cout := x

		// Top-edge boundary conditions (Forward application)
		p0T := int32(c.pixels[cin])
		p1T := int32(c.pixels[cin+s1])
		p2T := int32(c.pixels[cin+s2])

		out[cout] = cf(cf2A*p0T + cf2B*p1T)
		cout += w
		out[cout] = cf(cf3X*p0T + cf3Y*p1T + cf3Z*p2T)
		cout += w
		out[cout] = cf(cf3A*p0T + cf3B*p1T + cf3C*p2T)

		// Main loop for the middle part of the image.
		for y := 0; y < c.height-3; y++ {
			p0 := int32(c.pixels[cin])
			p1 := int32(c.pixels[cin+s1])
			p2 := int32(c.pixels[cin+s2])
			p3 := int32(c.pixels[cin+s3])

			cout += w // Advance cout
			out[cout] = cf(cf4A*p0 + cf4B*p1 + cf4C*p2 + cf4D*p3)
			cout += w // Advance cout
			out[cout] = cf(cf4D*p0 + cf4C*p1 + cf4B*p2 + cf4A*p3)

			cin += s1 // Advance cin
		}

		// Bottom-edge boundary conditions (Symmetric/Mirrored application).

		// Pixels in reverse order:
		p0B := int32(c.pixels[cin+s2]) // R(H-1)
		p1B := int32(c.pixels[cin+s1]) // R(H-2)
		p2B := int32(c.pixels[cin])    // R(H-3)

		// Apply coefficients symmetrically.

		// Row 2H-3 (mirrors Row 2)
		cout += w
		out[cout] = cf(cf3A*p0B + cf3B*p1B + cf3C*p2B)
		// Row 2H-2 (mirrors Row 1)
		cout += w
		out[cout] = cf(cf3X*p0B + cf3Y*p1B + cf3Z*p2B)
		// Row 2H-1 (mirrors Row 0)
		cout += w
		out[cout] = cf(cf2A*p0B + cf2B*p1B)
	}

	c.height = newHeight
	c.stride = c.width
	c.pixels = out
}

// upsampleNearestNeighbor performs upsampling by integer factors using the nearest-neighbor algorithm.
// This method is fast but produces lower-quality, "blocky" results compared to Catmull-Rom interpolation.
func upsampleNearestNeighborGeneric(c *component, width, height int) {
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
