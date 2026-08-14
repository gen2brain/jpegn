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

// readRow returns row y of m as RGB samples with a four byte pixel stride.
func readRow(m image.Image, b image.Rectangle, y int, dst []byte) []byte {
	w := b.Dx()

	switch src := m.(type) {
	case *image.RGBA:
		return src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):]
	case *image.NRGBA:
		p := src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):]

		for x, o := 0, 0; x < w; x, o = x+1, o+4 {
			a := uint32(p[o+3])
			dst[o] = byte(uint32(p[o]) * a / 255)
			dst[o+1] = byte(uint32(p[o+1]) * a / 255)
			dst[o+2] = byte(uint32(p[o+2]) * a / 255)
		}
	case *image.Gray:
		p := src.Pix[src.PixOffset(b.Min.X, b.Min.Y+y):]

		for x, o := 0, 0; x < w; x, o = x+1, o+4 {
			v := p[x]
			dst[o], dst[o+1], dst[o+2] = v, v, v
		}
	default:
		for x, o := 0, 0; x < w; x, o = x+1, o+4 {
			r, g, bl, _ := m.At(b.Min.X+x, b.Min.Y+y).RGBA()
			dst[o] = byte(r >> 8)
			dst[o+1] = byte(g >> 8)
			dst[o+2] = byte(bl >> 8)
		}
	}

	return dst
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

	row := e.scratch(c.width * 4)

	for py := 0; py < c.height; py++ {
		p := readRow(m, b, py, row)
		q := c.plane[py*c.stride:]

		for px, o := 0, 0; px < c.width; px, o = px+1, o+4 {
			q[px] = rgbToY(p[o], p[o+1], p[o+2])
		}
	}
}

// fillColor converts m into the component planes, box-filtering chroma.
func (e *encoder) fillColor(m image.Image) {
	b := m.Bounds()
	w, h := e.width, e.height
	row := e.scratch(w * 4)

	y := &e.comp[0]
	cb := &e.comp[1]
	cr := &e.comp[2]

	if e.hmax == 1 && e.vmax == 1 {
		for py := 0; py < h; py++ {
			p := readRow(m, b, py, row)
			rgbToYCbCrRow(y.plane[py*y.stride:], cb.plane[py*cb.stride:], cr.plane[py*cr.stride:], p, w)
		}

		return
	}

	sx, sy := e.hmax, e.vmax
	span := w * 2

	if cap(e.chromaBuf) < span*sy {
		e.chromaBuf = make([]byte, span*sy)
	}

	buf := e.chromaBuf[:span*sy]

	for cy := 0; cy < cb.height; cy++ {
		rows := 0

		for k := 0; k < sy; k++ {
			py := cy*sy + k
			if py >= h {
				break
			}

			p := readRow(m, b, py, row)
			rgbToYCbCrRow(y.plane[py*y.stride:], buf[k*span:], buf[k*span+w:], p, w)
			rows++
		}

		downsampleChroma(cb.plane[cy*cb.stride:], cr.plane[cy*cr.stride:], buf, w, cb.width, sx, rows)
	}
}

// downsampleChroma box-filters sx by rows sample groups into one chroma row.
func downsampleChroma(dstCb, dstCr, buf []byte, w, cw, sx, rows int) {
	span := w * 2
	cb0, cr0 := buf[0:w], buf[w:span]
	cb1, cr1 := cb0, cr0

	if rows > 1 {
		cb1, cr1 = buf[span:span+w], buf[span+w:span*2]
	}

	full := cw
	if full*sx > w {
		full--
	}

	outCb, outCr := dstCb[:full], dstCr[:full]

	switch {
	case sx == 2 && rows == 2:
		downsampleRow2x2(outCb, cb0, cb1, full)
		downsampleRow2x2(outCr, cr0, cr1, full)
	case sx == 2:
		n := full * 2
		sCb0, sCr0 := cb0[:n], cr0[:n]

		for cx := range outCb {
			x := cx * 2
			outCb[cx] = byte((int32(sCb0[x]) + int32(sCb0[x+1]) + 1) >> 1)
			outCr[cx] = byte((int32(sCr0[x]) + int32(sCr0[x+1]) + 1) >> 1)
		}
	case rows == 2:
		sCb0, sCb1 := cb0[:full], cb1[:full]
		sCr0, sCr1 := cr0[:full], cr1[:full]

		for cx := range outCb {
			outCb[cx] = byte((int32(sCb0[cx]) + int32(sCb1[cx]) + 1) >> 1)
			outCr[cx] = byte((int32(sCr0[cx]) + int32(sCr1[cx]) + 1) >> 1)
		}
	default:
		copy(outCb, cb0[:full])
		copy(outCr, cr0[:full])
	}

	for cx := full; cx < cw; cx++ {
		var sb, sr, cnt int32

		for k := 0; k < rows; k++ {
			cbRow, crRow := cb0, cr0
			if k == 1 {
				cbRow, crRow = cb1, cr1
			}

			for dx, x := 0, cx*sx; dx < sx && x < w; dx, x = dx+1, x+1 {
				sb += int32(cbRow[x])
				sr += int32(crRow[x])
				cnt++
			}
		}

		dstCb[cx] = byte((sb + cnt>>1) / cnt)
		dstCr[cx] = byte((sr + cnt>>1) / cnt)
	}
}

// scratch returns a reusable row buffer of at least n bytes.
func (e *encoder) scratch(n int) []byte {
	if cap(e.rowBuf) < n {
		e.rowBuf = make([]byte, n)
	}

	return e.rowBuf[:n]
}
