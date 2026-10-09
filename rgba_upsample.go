package jpegn

// upsampleStripRows is the number of chroma source rows upsampled per strip. It
// keeps the strip buffers cache-resident between upsampling and conversion while
// amortizing the per-call and Catmull-Rom carry-over cost.
const upsampleStripRows = 16

// yCbCrToRGBAUpsampled converts YCbCr with 2x horizontally (4:2:2) or 2x2 (4:2:0)
// subsampled chroma to RGBA, upsampling chroma strip by strip right before
// converting it instead of materializing full-resolution chroma planes. The output
// equals upsampleCatmullRom/upsampleNearestNeighbor followed by yCbCrToRGBA. It
// reports false, without writing, when the layout is not one it handles.
func yCbCrToRGBAUpsampled(y, cb, cr *component, dst []byte, width, height int, method UpsampleMethod) bool {
	if width <= 0 || height <= 0 || len(dst) < width*height*4 {
		return false
	}

	if y.width < width || y.height < height || cb.width != cr.width || cb.height != cr.height {
		return false
	}

	w, h := cb.width, cb.height
	needV := h < height

	if w >= width || 2*w < width || (needV && 2*h < height) {
		return false
	}

	w2 := w << 1
	cubic := upsampleCubic(cb, width, height, method)

	switch {
	case cubic && needV:
		yCbCrToRGBACatmullRom420(y, cb, cr, dst, width, height)
	case cubic:
		yCbCrToRGBARowStrips(y, cb, cr, dst, width, height, w2, 1, upsampleHRows)
	case needV:
		yCbCrToRGBARowStrips(y, cb, cr, dst, width, height, w2, 2, upsampleNearestNeighborRows)
	default:
		// Nearest-neighbour rows come out doubled, so 4:2:2 reads every other one.
		yCbCrToRGBARowStrips(y, cb, cr, dst, width, height, 2*w2, 1, upsampleNearestNeighborRows)
	}

	return true
}

// yCbCrToRGBAStrip converts output rows [r0, r1) with chroma rows taken from
// cbRows/crRows at cStride.
func yCbCrToRGBAStrip(y *component, cbRows, crRows []byte, cStride int, dst []byte, width, r0, r1 int) {
	ys := component{pixels: y.pixels[r0*y.stride:], stride: y.stride}
	cbs := component{pixels: cbRows, stride: cStride}
	crs := component{pixels: crRows, stride: cStride}

	yCbCrToRGBA(&ys, &cbs, &crs, dst[r0*width*4:r1*width*4], width, r1-r0)
}

// yCbCrToRGBARowStrips upsamples strips of chroma rows with rows into buffers
// read back at cStride, each source row covering vScale output rows.
func yCbCrToRGBARowStrips(y, cb, cr *component, dst []byte, width, height, cStride, vScale int, rows func(dst, src []byte, w, h, dstStride, srcStride int)) {
	w2 := cb.width << 1
	stripSize := upsampleStripRows * vScale * cStride
	buf := make([]byte, 2*stripSize)
	cbBuf, crBuf := buf[:stripSize], buf[stripSize:]
	srcRows := (height + vScale - 1) / vScale

	for sy := 0; sy < srcRows; sy += upsampleStripRows {
		n := min(upsampleStripRows, srcRows-sy)

		rows(cbBuf, cb.pixels[sy*cb.stride:], cb.width, n, w2, cb.stride)
		rows(crBuf, cr.pixels[sy*cr.stride:], cr.width, n, w2, cr.stride)
		yCbCrToRGBAStrip(y, cbBuf, crBuf, cStride, dst, width, vScale*sy, min(vScale*(sy+n), height))
	}
}

// catmullRomStrip holds one chroma component's horizontally interpolated source
// rows and the vertically interpolated output rows of the current strip.
type catmullRomStrip struct {
	c     *component
	hRows []byte // source rows [y0, y1+3) after horizontal interpolation
	vRows []byte // output rows [r0, r1)
}

// fill interpolates the strip covering interior row pairs [y0, y1), plus the top
// edge when y0 is 0 and the bottom edge when y1 is the last pair, and returns the
// number of output rows written. The first carry rows of hRows already hold
// source rows y0..y0+carry-1.
func (s *catmullRomStrip) fill(y0, y1, carry, pairs int) int {
	c := s.c
	w2 := c.width << 1

	upsampleHRows(s.hRows[carry*w2:], c.pixels[(y0+carry)*c.stride:], c.width, y1+3-y0-carry, w2, c.stride)

	n := 0
	if y0 == 0 {
		upsampleVTopEdge(s.hRows, s.vRows, w2, w2)
		n = 3
	}

	for py := y0; py < y1; py++ {
		upsampleVRowPair(s.hRows[(py-y0)*w2:], s.vRows[n*w2:], s.vRows[(n+1)*w2:], w2, w2)
		n += 2
	}

	if y1 == pairs {
		src := s.hRows[(pairs-y0)*w2:]
		upsampleVBottomEdge(src, s.vRows[n*w2:], s.vRows[(n+1)*w2:], s.vRows[(n+2)*w2:], w2, w2)
		n += 3
	}

	return n
}

// yCbCrToRGBACatmullRom420 interpolates chroma horizontally then vertically per
// strip. The vertical filter spans four source rows, so the last three
// horizontally interpolated rows of a strip carry over to the next.
func yCbCrToRGBACatmullRom420(y, cb, cr *component, dst []byte, width, height int) {
	w2 := cb.width << 1
	pairs := cb.height - 3
	hSize := (upsampleStripRows + 3) * w2
	vSize := (2*upsampleStripRows + 6) * w2
	buf := make([]byte, 2*(hSize+vSize))

	strips := [2]catmullRomStrip{
		{c: cb, hRows: buf[:hSize], vRows: buf[hSize : hSize+vSize]},
		{c: cr, hRows: buf[hSize+vSize : 2*hSize+vSize], vRows: buf[2*hSize+vSize:]},
	}

	carry := 0
	r0 := 0

	for y0 := 0; ; {
		y1 := min(y0+upsampleStripRows, pairs)

		n := strips[0].fill(y0, y1, carry, pairs)
		strips[1].fill(y0, y1, carry, pairs)

		r1 := min(r0+n, height)
		yCbCrToRGBAStrip(y, strips[0].vRows, strips[1].vRows, w2, dst, width, r0, r1)

		if r1 == height || y1 == pairs {
			return
		}

		for i := range strips {
			copy(strips[i].hRows, strips[i].hRows[(y1-y0)*w2:(y1-y0+3)*w2])
		}

		carry = 3
		r0 = r1
		y0 = y1
	}
}
