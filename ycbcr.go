package jpegn

import (
	"image"
)

// rgbToYCbCr converts one pixel to the JPEG YCbCr color space.
func rgbToYCbCr(r, g, b byte) (byte, byte, byte) {
	r1, g1, b1 := int32(r), int32(g), int32(b)

	yy := (19595*r1 + 38470*g1 + 7471*b1 + 1<<15) >> 16
	cb := (-11056*r1 - 21712*g1 + 32768*b1 + 257<<15) >> 16
	cr := (32768*r1 - 27440*g1 - 5328*b1 + 257<<15) >> 16

	if cb < 0 {
		cb = 0
	} else if cb > 255 {
		cb = 255
	}

	if cr < 0 {
		cr = 0
	} else if cr > 255 {
		cr = 255
	}

	return byte(yy), byte(cb), byte(cr)
}

// rgbToY converts one pixel to a luma sample.
func rgbToY(r, g, b byte) byte {
	return byte((19595*int32(r) + 38470*int32(g) + 7471*int32(b) + 1<<15) >> 16)
}

// readRow returns row y of m as RGB samples and the byte stride between pixels.
func readRow(m image.Image, b image.Rectangle, y int, dst []byte) ([]byte, int) {
	w := b.Dx()

	switch src := m.(type) {
	case *image.RGBA:
		return src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):], 4
	case *image.NRGBA:
		p := src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):]

		for x := 0; x < w; x++ {
			a := uint32(p[x*4+3])
			dst[x*3] = byte(uint32(p[x*4]) * a / 255)
			dst[x*3+1] = byte(uint32(p[x*4+1]) * a / 255)
			dst[x*3+2] = byte(uint32(p[x*4+2]) * a / 255)
		}
	case *image.Gray:
		p := src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):]

		for x := 0; x < w; x++ {
			v := p[x]
			dst[x*3], dst[x*3+1], dst[x*3+2] = v, v, v
		}
	default:
		for x := 0; x < w; x++ {
			r, g, bl, _ := m.At(b.Min.X+x, b.Min.Y+y).RGBA()
			dst[x*3] = byte(r >> 8)
			dst[x*3+1] = byte(g >> 8)
			dst[x*3+2] = byte(bl >> 8)
		}
	}

	return dst, 3
}

// fillRow copies src into dst[:w], replicating the last sample if src is short.
func fillRow(dst, src []byte, w int) {
	n := copy(dst[:w], src)
	if n == 0 {
		return
	}

	for i := n; i < w; i++ {
		dst[i] = dst[n-1]
	}
}

// padPlane replicates the right column and bottom row across the MCU padding.
func padPlane(p []byte, stride, w, h, ph int) {
	for y := 0; y < h; y++ {
		row := p[y*stride : (y+1)*stride]
		v := row[w-1]

		for x := w; x < stride; x++ {
			row[x] = v
		}
	}

	last := p[(h-1)*stride : h*stride]
	for y := h; y < ph; y++ {
		copy(p[y*stride:(y+1)*stride], last)
	}
}

// buildPlanes allocates the padded component planes and fills them from m.
func (e *encoder) buildPlanes(m image.Image) {
	for i := 0; i < e.ncomp; i++ {
		c := &e.comp[i]
		c.stride = e.mcusX * c.ssX * 8
		c.width = (e.width*c.ssX + e.hmax - 1) / e.hmax
		c.height = (e.height*c.ssY + e.vmax - 1) / e.vmax

		need := c.stride * e.mcusY * c.ssY * 8
		if cap(c.plane) < need {
			c.plane = make([]byte, need)
		} else {
			c.plane = c.plane[:need]
		}
	}

	switch {
	case e.ncomp == 1:
		e.fillGray(m)
	case e.matchesYCbCr(m):
		e.fillFromYCbCr(m.(*image.YCbCr))
	default:
		e.fillColor(m)
	}

	for i := 0; i < e.ncomp; i++ {
		c := &e.comp[i]
		padPlane(c.plane, c.stride, c.width, c.height, e.mcusY*c.ssY*8)
	}
}

// matchesYCbCr reports whether m has planes usable without resampling.
func (e *encoder) matchesYCbCr(m image.Image) bool {
	src, ok := m.(*image.YCbCr)
	if !ok {
		return false
	}

	switch src.SubsampleRatio {
	case image.YCbCrSubsampleRatio444:
		return e.hmax == 1 && e.vmax == 1
	case image.YCbCrSubsampleRatio440:
		return e.hmax == 1 && e.vmax == 2
	case image.YCbCrSubsampleRatio422:
		return e.hmax == 2 && e.vmax == 1
	case image.YCbCrSubsampleRatio420:
		return e.hmax == 2 && e.vmax == 2
	}

	return false
}

// fillFromYCbCr copies the source planes directly into the component planes.
func (e *encoder) fillFromYCbCr(src *image.YCbCr) {
	b := src.Bounds()

	y := &e.comp[0]
	for py := 0; py < y.height; py++ {
		off := src.YOffset(b.Min.X, b.Min.Y+py)
		fillRow(y.plane[py*y.stride:], src.Y[off:], y.width)
	}

	cb := &e.comp[1]
	cr := &e.comp[2]

	for py := 0; py < cb.height; py++ {
		off := src.COffset(b.Min.X, b.Min.Y+py*e.vmax)
		fillRow(cb.plane[py*cb.stride:], src.Cb[off:], cb.width)
		fillRow(cr.plane[py*cr.stride:], src.Cr[off:], cr.width)
	}
}

// fillGray fills the single luma plane of a grayscale image.
func (e *encoder) fillGray(m image.Image) {
	b := m.Bounds()
	c := &e.comp[0]

	switch src := m.(type) {
	case *image.Gray:
		for py := 0; py < c.height; py++ {
			off := src.PixOffset(b.Min.X, b.Min.Y+py)
			fillRow(c.plane[py*c.stride:], src.Pix[off:], c.width)
		}

		return
	case *image.YCbCr:
		for py := 0; py < c.height; py++ {
			off := src.YOffset(b.Min.X, b.Min.Y+py)
			fillRow(c.plane[py*c.stride:], src.Y[off:], c.width)
		}

		return
	}

	row := e.scratch(c.width * 3)

	for py := 0; py < c.height; py++ {
		p, ps := readRow(m, b, py, row)
		q := c.plane[py*c.stride:]

		for px, o := 0, 0; px < c.width; px, o = px+1, o+ps {
			q[px] = rgbToY(p[o], p[o+1], p[o+2])
		}
	}
}

// fillColor converts m into the component planes, box-filtering chroma.
func (e *encoder) fillColor(m image.Image) {
	b := m.Bounds()
	w, h := e.width, e.height
	row := e.scratch(w * 3)

	y := &e.comp[0]
	cb := &e.comp[1]
	cr := &e.comp[2]

	shiftX, shiftY := 0, 0
	if e.hmax == 2 {
		shiftX = 1
	}

	if e.vmax == 2 {
		shiftY = 1
	}

	if shiftX == 0 && shiftY == 0 {
		for py := 0; py < h; py++ {
			p, ps := readRow(m, b, py, row)
			yp := y.plane[py*y.stride:]
			cbp := cb.plane[py*cb.stride:]
			crp := cr.plane[py*cr.stride:]

			for px, o := 0, 0; px < w; px, o = px+1, o+ps {
				yp[px], cbp[px], crp[px] = rgbToYCbCr(p[o], p[o+1], p[o+2])
			}
		}

		return
	}

	cw, ch := cb.width, cb.height
	n := cw * ch

	if cap(e.cbAcc) < n {
		e.cbAcc = make([]int32, n)
		e.crAcc = make([]int32, n)
		e.cnt = make([]uint8, n)
	}

	cbAcc, crAcc, cnt := e.cbAcc[:n], e.crAcc[:n], e.cnt[:n]
	clear(cbAcc)
	clear(crAcc)
	clear(cnt)

	for py := 0; py < h; py++ {
		p, ps := readRow(m, b, py, row)
		yp := y.plane[py*y.stride:]
		co := (py >> shiftY) * cw

		for px, o := 0, 0; px < w; px, o = px+1, o+ps {
			yv, cbv, crv := rgbToYCbCr(p[o], p[o+1], p[o+2])
			yp[px] = yv

			i := co + px>>shiftX
			cbAcc[i] += int32(cbv)
			crAcc[i] += int32(crv)
			cnt[i]++
		}
	}

	for cy := 0; cy < ch; cy++ {
		cbp := cb.plane[cy*cb.stride:]
		crp := cr.plane[cy*cr.stride:]
		i := cy * cw

		for cx := 0; cx < cw; cx++ {
			k := int32(cnt[i+cx])
			cbp[cx] = byte((cbAcc[i+cx] + k>>1) / k)
			crp[cx] = byte((crAcc[i+cx] + k>>1) / k)
		}
	}
}

// scratch returns a reusable row buffer of at least n bytes.
func (e *encoder) scratch(n int) []byte {
	if cap(e.rowBuf) < n {
		e.rowBuf = make([]byte, n)
	}

	return e.rowBuf[:n]
}
