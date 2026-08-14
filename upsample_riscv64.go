//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func upsampleVMiddleRVV(dst1, dst2, src *byte, stride, n int)

//go:noescape
func upsampleHMiddleRVV(dst, src *byte, n int)

// upsampleNearestNeighbor doubles a component with sample replication.
func upsampleNearestNeighbor(c *component, width, height int) {
	upsampleNearestNeighborScalar(c, width, height)
}

// upsampleCatmullRom doubles a component with a 4-tap interpolation filter.
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

// upsampleH doubles the width with a 4-tap interpolation filter.
func upsampleH(c *component) {
	if c.width < 11 {
		upsampleHScalar(c)

		return
	}

	newWidth := c.width << 1
	out := make([]byte, newWidth*c.height)
	nbulk := c.width - 3

	for y := 0; y < c.height; y++ {
		in := c.pixels[y*c.stride:]
		o := out[y*newWidth:]

		upsampleHEdges(in, o, c.width)
		upsampleHMiddleRVV(&o[3], &in[0], nbulk)
	}

	c.width = newWidth
	c.stride = c.width
	c.pixels = out
}

// upsampleV doubles the height with a 4-tap interpolation filter.
func upsampleV(c *component) {
	if c.width < 8 || c.height < 4 {
		upsampleVScalar(c)

		return
	}

	w := c.width
	stride := c.stride
	newHeight := c.height << 1
	out := make([]byte, w*newHeight)

	upsampleVTopEdge(c.pixels, out, w, stride)

	for y := 0; y < c.height-3; y++ {
		src := c.pixels[y*stride:]
		out1 := out[(2*y+3)*w:]
		out2 := out[(2*y+4)*w:]

		upsampleVMiddleRVV(&out1[0], &out2[0], &src[0], stride, w)
	}

	last := (c.height - 3) * stride
	upsampleVBottomEdge(c.pixels[last:], out[(newHeight-3)*w:], out[(newHeight-2)*w:], out[(newHeight-1)*w:], w, stride)

	c.height = newHeight
	c.pixels = out
	c.stride = w
}
