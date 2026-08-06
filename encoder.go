package jpegn

import (
	"image"
	"io"
	"math/bits"
	"sync"
)

// JPEG markers emitted by the encoder.
const (
	markerSOI  = 0xD8
	markerEOI  = 0xD9
	markerSOF0 = 0xC0
	markerDHT  = 0xC4
	markerDQT  = 0xDB
	markerDRI  = 0xDD
	markerSOS  = 0xDA
	markerAPP1 = 0xE1
	markerAPP0 = 0xE0
	markerCOM  = 0xFE
	markerRST0 = 0xD0
)

// DefaultQuality is the quality used when EncodeOptions is nil or Quality is zero.
const DefaultQuality = 75

// quantShift is the fixed-point scale of the quantization reciprocals.
const quantShift = 31

// maxSegmentData is the largest payload a marker segment can carry.
const maxSegmentData = 65533

// validSegmentMarker reports whether m may be written as a standalone segment.
func validSegmentMarker(m byte) bool {
	return (m >= markerAPP0 && m <= markerAPP0+15) || m == markerCOM
}

// Subsampling selects the chroma subsampling of the encoded image.
type Subsampling int

const (
	// SubsampleAuto follows the source image, defaulting to 4:2:0.
	SubsampleAuto Subsampling = iota
	// Subsample444 keeps chroma at full resolution.
	Subsample444
	// Subsample440 halves chroma vertically.
	Subsample440
	// Subsample422 halves chroma horizontally.
	Subsample422
	// Subsample420 halves chroma in both directions.
	Subsample420
	// SubsampleGray discards chroma and writes a single-component image.
	SubsampleGray
)

// Segment is an application or comment marker segment to embed in the output.
type Segment struct {
	// Marker is an APPn marker (0xE0 to 0xEF) or the comment marker 0xFE.
	Marker byte
	// Data is the segment payload, at most 65533 bytes.
	Data []byte
}

// EncodeOptions specifies encoding parameters.
type EncodeOptions struct {
	// Quality ranges from 1 (smallest) to 100 (best). Zero selects [DefaultQuality].
	Quality int
	// Subsampling selects the chroma subsampling ratio.
	Subsampling Subsampling
	// OptimizeCoding derives Huffman tables from the coefficient statistics.
	OptimizeCoding bool
	// RestartInterval is the MCU count between restart markers; zero disables them.
	RestartInterval int
	// Exif is a raw APP1 payload from [RawExif]; it replaces the JFIF APP0 segment.
	Exif []byte
	// ResetOrientation rewrites the embedded EXIF orientation tag to 1.
	ResetOrientation bool
	// Segments are additional marker segments written after the header segment.
	Segments []Segment
}

// Quantization tables from the JPEG standard, Annex K.1, in natural order.
var stdLumaQuant = [64]uint16{
	16, 11, 10, 16, 24, 40, 51, 61,
	12, 12, 14, 19, 26, 58, 60, 55,
	14, 13, 16, 24, 40, 57, 69, 56,
	14, 17, 22, 29, 51, 87, 80, 62,
	18, 22, 37, 56, 68, 109, 103, 77,
	24, 35, 55, 64, 81, 104, 113, 92,
	49, 64, 78, 87, 103, 121, 120, 101,
	72, 92, 95, 98, 112, 100, 103, 99,
}

var stdChromaQuant = [64]uint16{
	17, 18, 24, 47, 99, 99, 99, 99,
	18, 21, 26, 66, 99, 99, 99, 99,
	24, 26, 56, 99, 99, 99, 99, 99,
	47, 66, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99,
}

// encComponent is one color component of the encoder.
type encComponent struct {
	id           int
	ssX, ssY     int
	qtSel        int
	dcSel, acSel int
	pred         int32
	plane        []byte
	stride       int
	width        int
	height       int
}

// encoder holds the state of the JPEG encoding process.
type encoder struct {
	out           []byte
	acc           uint32
	nacc          uint
	width, height int
	ncomp         int
	comp          [3]encComponent
	hmax, vmax    int
	mcusX, mcusY  int
	qtab          [2][64]uint16 // Zigzag order, as written to DQT.
	qrecip        [2][64]int64  // Zigzag order, reciprocal of the FDCT-matched divisor.
	qhalf         [2][64]int32  // Zigzag order, half the divisor, for rounding.
	nqtab         int
	dcTab         [2]huffEncTable
	acTab         [2]huffEncTable
	dcBits        [2][17]uint8
	acBits        [2][17]uint8
	dcVals        [2][256]uint8
	acVals        [2][256]uint8
	dcCount       [2]int
	acCount       [2]int
	nhuff         int
	dcFreq        [2][257]int32
	acFreq        [2][257]int32
	rst           int
	exif          []byte
	segments      []Segment
	gather        bool
	blk           [64]int32
	zblk          [64]int32
	rowBuf        []byte
	chromaBuf     []byte
}

// encoderPool is a pool of encoder structs to reduce allocation overhead.
var encoderPool = sync.Pool{
	New: func() interface{} {
		return &encoder{}
	},
}

// Encode writes the image m to w in baseline JPEG format.
func Encode(w io.Writer, m image.Image, opts ...*EncodeOptions) error {
	b := m.Bounds()
	if b.Dx() <= 0 || b.Dy() <= 0 || b.Dx() > 65535 || b.Dy() > 65535 {
		return ErrUnsupported
	}

	quality := DefaultQuality
	sub := SubsampleAuto
	optimize := false
	rst := 0

	var exif []byte
	var segments []Segment

	if len(opts) > 0 && opts[0] != nil {
		exif = opts[0].Exif
		segments = opts[0].Segments

		if len(exif) > maxSegmentData {
			return ErrInvalidSegment
		}

		if len(exif) > 0 && opts[0].ResetOrientation {
			exif = setExifOrientation(exif, 1)
		}

		for _, seg := range segments {
			if !validSegmentMarker(seg.Marker) || len(seg.Data) > maxSegmentData {
				return ErrInvalidSegment
			}
		}

		if opts[0].Quality != 0 {
			quality = opts[0].Quality
		}

		sub = opts[0].Subsampling
		optimize = opts[0].OptimizeCoding

		if opts[0].RestartInterval > 0 {
			rst = min(opts[0].RestartInterval, 65535)
		}
	}

	e := encoderPool.Get().(*encoder)

	defer func() {
		e.out = e.out[:0]
		e.exif = nil
		e.segments = nil
		encoderPool.Put(e)
	}()

	e.exif = exif
	e.segments = segments

	if err := e.encode(m, quality, sub, optimize, rst); err != nil {
		return err
	}

	_, err := w.Write(e.out)

	return err
}

// encode runs the full compression pipeline into e.out.
func (e *encoder) encode(m image.Image, quality int, sub Subsampling, optimize bool, rst int) error {
	b := m.Bounds()

	e.out = e.out[:0]
	e.acc = 0
	e.nacc = 0
	e.width = b.Dx()
	e.height = b.Dy()
	e.rst = rst

	e.setSampling(resolveSampling(m, sub))

	e.mcusX = (e.width + e.hmax*8 - 1) / (e.hmax * 8)
	e.mcusY = (e.height + e.vmax*8 - 1) / (e.vmax * 8)

	e.buildQuant(quality)
	e.buildPlanes(m)

	if optimize {
		e.gather = true
		e.dcFreq = [2][257]int32{}
		e.acFreq = [2][257]int32{}
		e.scan()
		e.gather = false
		e.buildOptimalTables()
	} else {
		e.setStdTables()
	}

	e.writeHeader()
	e.scan()
	e.emitMarker(markerEOI)

	return nil
}

// resolveSampling maps SubsampleAuto onto a concrete ratio for the source image.
func resolveSampling(m image.Image, sub Subsampling) Subsampling {
	if sub != SubsampleAuto {
		return sub
	}

	switch src := m.(type) {
	case *image.Gray:
		return SubsampleGray
	case *image.YCbCr:
		switch src.SubsampleRatio {
		case image.YCbCrSubsampleRatio444:
			return Subsample444
		case image.YCbCrSubsampleRatio440:
			return Subsample440
		case image.YCbCrSubsampleRatio422:
			return Subsample422
		case image.YCbCrSubsampleRatio420:
			return Subsample420
		}
	}

	return Subsample420
}

// setComp configures one component, keeping its allocated plane.
func (e *encoder) setComp(i, id, ssX, ssY, sel int) {
	c := &e.comp[i]
	c.id = id
	c.ssX, c.ssY = ssX, ssY
	c.qtSel, c.dcSel, c.acSel = sel, sel, sel
	c.pred = 0
}

// setSampling configures the component list for a subsampling ratio.
func (e *encoder) setSampling(sub Subsampling) {
	if sub == SubsampleGray {
		e.ncomp = 1
		e.nqtab = 1
		e.nhuff = 1
		e.hmax, e.vmax = 1, 1
		e.setComp(0, 1, 1, 1, 0)

		return
	}

	switch sub {
	case Subsample444:
		e.hmax, e.vmax = 1, 1
	case Subsample440:
		e.hmax, e.vmax = 1, 2
	case Subsample422:
		e.hmax, e.vmax = 2, 1
	default:
		e.hmax, e.vmax = 2, 2
	}

	e.ncomp = 3
	e.nqtab = 2
	e.nhuff = 2
	e.setComp(0, 1, e.hmax, e.vmax, 0)
	e.setComp(1, 2, 1, 1, 1)
	e.setComp(2, 3, 1, 1, 1)
}

// buildQuant scales the standard tables for a quality into zigzag order.
func (e *encoder) buildQuant(quality int) {
	if quality < 1 {
		quality = 1
	}

	if quality > 100 {
		quality = 100
	}

	scale := 200 - quality*2
	if quality < 50 {
		scale = 5000 / quality
	}

	for t := 0; t < e.nqtab; t++ {
		base := &stdLumaQuant
		if t == 1 {
			base = &stdChromaQuant
		}

		for i := 0; i < 64; i++ {
			v := (int(base[zz[i]])*scale + 50) / 100
			if v < 1 {
				v = 1
			}

			if v > 255 {
				v = 255
			}

			d := int32(v) * 8
			e.qtab[t][i] = uint16(v)
			e.qhalf[t][i] = d >> 1
			e.qrecip[t][i] = (1<<quantShift + int64(d) - 1) / int64(d)
		}
	}
}

// setStdTables installs the Huffman tables from JPEG Annex K.
func (e *encoder) setStdTables() {
	for t := 0; t < e.nhuff; t++ {
		dcCounts, dcValues := &defaultDCLumaCounts, defaultDCLumaValues
		acCounts, acValues := &defaultACLumaCounts, defaultACLumaValues

		if t == 1 {
			dcCounts, dcValues = &defaultDCChromaCounts, defaultDCChromaValues
			acCounts, acValues = &defaultACChromaCounts, defaultACChromaValues
		}

		e.dcBits[t] = [17]uint8{}
		e.acBits[t] = [17]uint8{}

		for l := 1; l <= 16; l++ {
			e.dcBits[t][l] = dcCounts[l-1]
			e.acBits[t][l] = acCounts[l-1]
		}

		e.dcCount[t] = copy(e.dcVals[t][:], dcValues)
		e.acCount[t] = copy(e.acVals[t][:], acValues)

		buildHuffEnc(&e.dcTab[t], &e.dcBits[t], e.dcVals[t][:e.dcCount[t]])
		buildHuffEnc(&e.acTab[t], &e.acBits[t], e.acVals[t][:e.acCount[t]])
	}
}

// buildOptimalTables derives Huffman tables from the gathered statistics.
func (e *encoder) buildOptimalTables() {
	e.setStdTables()

	for t := 0; t < e.nhuff; t++ {
		var dcBits [17]uint8
		var dcVals [256]uint8

		if n := genOptimalTable(&e.dcFreq[t], &dcBits, &dcVals); n > 0 {
			e.dcBits[t] = dcBits
			e.dcVals[t] = dcVals
			e.dcCount[t] = n
			buildHuffEnc(&e.dcTab[t], &e.dcBits[t], e.dcVals[t][:n])
		}

		var acBits [17]uint8
		var acVals [256]uint8

		if n := genOptimalTable(&e.acFreq[t], &acBits, &acVals); n > 0 {
			e.acBits[t] = acBits
			e.acVals[t] = acVals
			e.acCount[t] = n
			buildHuffEnc(&e.acTab[t], &e.acBits[t], e.acVals[t][:n])
		}
	}
}

// emitMarker writes a standalone marker.
func (e *encoder) emitMarker(m byte) {
	e.out = append(e.out, 0xFF, m)
}

// emitU16 writes a big-endian 16-bit value.
func (e *encoder) emitU16(v int) {
	e.out = append(e.out, byte(v>>8), byte(v))
}

// emitDHT writes one Huffman table segment.
func (e *encoder) emitDHT(class, id int, tbits *[17]uint8, values []byte) {
	e.emitMarker(markerDHT)
	e.emitU16(2 + 1 + 16 + len(values))
	e.out = append(e.out, byte(class<<4|id))
	e.out = append(e.out, tbits[1:17]...)
	e.out = append(e.out, values...)
}

// writeHeader emits everything from SOI up to and including SOS.
func (e *encoder) writeHeader() {
	e.emitMarker(markerSOI)

	if len(e.exif) > 0 {
		e.emitMarker(markerAPP1)
		e.emitU16(2 + len(e.exif))
		e.out = append(e.out, e.exif...)
	} else {
		e.emitMarker(markerAPP0)
		e.emitU16(16)
		e.out = append(e.out, 'J', 'F', 'I', 'F', 0, 1, 1, 0, 0, 1, 0, 1, 0, 0)
	}

	for _, seg := range e.segments {
		e.emitMarker(seg.Marker)
		e.emitU16(2 + len(seg.Data))
		e.out = append(e.out, seg.Data...)
	}

	for t := 0; t < e.nqtab; t++ {
		e.emitMarker(markerDQT)
		e.emitU16(2 + 1 + 64)
		e.out = append(e.out, byte(t))

		for i := 0; i < 64; i++ {
			e.out = append(e.out, byte(e.qtab[t][i]))
		}
	}

	e.emitMarker(markerSOF0)
	e.emitU16(8 + 3*e.ncomp)
	e.out = append(e.out, 8)
	e.emitU16(e.height)
	e.emitU16(e.width)
	e.out = append(e.out, byte(e.ncomp))

	for i := 0; i < e.ncomp; i++ {
		c := &e.comp[i]
		e.out = append(e.out, byte(c.id), byte(c.ssX<<4|c.ssY), byte(c.qtSel))
	}

	for t := 0; t < e.nhuff; t++ {
		e.emitDHT(0, t, &e.dcBits[t], e.dcVals[t][:e.dcCount[t]])
		e.emitDHT(1, t, &e.acBits[t], e.acVals[t][:e.acCount[t]])
	}

	if e.rst > 0 {
		e.emitMarker(markerDRI)
		e.emitU16(4)
		e.emitU16(e.rst)
	}

	e.emitMarker(markerSOS)
	e.emitU16(6 + 2*e.ncomp)
	e.out = append(e.out, byte(e.ncomp))

	for i := 0; i < e.ncomp; i++ {
		c := &e.comp[i]
		e.out = append(e.out, byte(c.id), byte(c.dcSel<<4|c.acSel))
	}

	e.out = append(e.out, 0, 63, 0)
}

// emitBits appends nbits of code, applying JPEG byte stuffing.
func (e *encoder) emitBits(code uint32, nbits uint8) {
	e.acc = e.acc<<nbits | code
	e.nacc += uint(nbits)

	for e.nacc >= 8 {
		b := byte(e.acc >> (e.nacc - 8))
		e.out = append(e.out, b)

		if b == 0xFF {
			e.out = append(e.out, 0x00)
		}

		e.nacc -= 8
	}
}

// flushBits pads to a byte boundary with one bits.
func (e *encoder) flushBits() {
	if e.nacc > 0 {
		e.emitBits((1<<(8-e.nacc))-1, uint8(8-e.nacc))
	}

	e.acc = 0
	e.nacc = 0
}

// magnitude returns the JPEG size category of v and the coefficient bits.
func magnitude(v int32) (uint8, uint32) {
	if v == 0 {
		return 0, 0
	}

	a := v
	if a < 0 {
		a = -a
		v += 1<<uint(bits.Len32(uint32(a))) - 1
	}

	return uint8(bits.Len32(uint32(a))), uint32(v)
}

// scan walks the MCUs, gathering statistics or emitting coded data.
func (e *encoder) scan() {
	for i := 0; i < e.ncomp; i++ {
		e.comp[i].pred = 0
	}

	rst := 0
	n := 0
	last := e.mcusX*e.mcusY - 1
	mcu := 0

	for my := 0; my < e.mcusY; my++ {
		for mx := 0; mx < e.mcusX; mx++ {
			for ci := 0; ci < e.ncomp; ci++ {
				c := &e.comp[ci]

				for by := 0; by < c.ssY; by++ {
					for bx := 0; bx < c.ssX; bx++ {
						e.encodeBlock(c, (mx*c.ssX+bx)*8, (my*c.ssY+by)*8)
					}
				}
			}

			n++

			if e.rst > 0 && n == e.rst && mcu != last {
				if !e.gather {
					e.flushBits()
					e.emitMarker(byte(markerRST0 + rst))
				}

				rst = (rst + 1) & 7
				n = 0

				for i := 0; i < e.ncomp; i++ {
					e.comp[i].pred = 0
				}
			}

			mcu++
		}
	}

	if !e.gather {
		e.flushBits()
	}
}

// encodeBlock transforms, quantizes and codes the block at (sx, sy).
func (e *encoder) encodeBlock(c *encComponent, sx, sy int) {
	blk := &e.blk
	off := sy*c.stride + sx

	for y := 0; y < 64; y += 8 {
		row := c.plane[off : off+8 : off+8]
		b := blk[y : y+8 : y+8]

		b[0] = int32(row[0]) - 128
		b[1] = int32(row[1]) - 128
		b[2] = int32(row[2]) - 128
		b[3] = int32(row[3]) - 128
		b[4] = int32(row[4]) - 128
		b[5] = int32(row[5]) - 128
		b[6] = int32(row[6]) - 128
		b[7] = int32(row[7]) - 128

		off += c.stride
	}

	fdct(blk)

	recip := &e.qrecip[c.qtSel]
	half := &e.qhalf[c.qtSel]
	zb := &e.zblk

	for i := 0; i < 64; i++ {
		v := blk[zz[i]]
		sign := v >> 31
		a := (v ^ sign) - sign

		q := int32((int64(a+half[i]) * recip[i]) >> quantShift)
		if q > 1023 {
			q = 1023
		}

		zb[i] = (q ^ sign) - sign
	}

	e.encodeCoeffs(zb, c)
}

// encodeCoeffs codes one block of quantized coefficients in zigzag order.
func (e *encoder) encodeCoeffs(zb *[64]int32, c *encComponent) {
	diff := zb[0] - c.pred
	c.pred = zb[0]

	s, b := magnitude(diff)

	if e.gather {
		e.dcFreq[c.dcSel][s]++
	} else {
		t := &e.dcTab[c.dcSel]
		e.emitBits(t.code[s], t.size[s])

		if s > 0 {
			e.emitBits(b, s)
		}
	}

	af := &e.acFreq[c.acSel]
	t := &e.acTab[c.acSel]
	run := 0

	for k := 1; k < 64; k++ {
		v := zb[k]
		if v == 0 {
			run++

			continue
		}

		for run > 15 {
			if e.gather {
				af[0xF0]++
			} else {
				e.emitBits(t.code[0xF0], t.size[0xF0])
			}

			run -= 16
		}

		s, b := magnitude(v)
		sym := run<<4 | int(s)

		if e.gather {
			af[sym]++
		} else {
			e.emitBits(t.code[sym], t.size[sym])
			e.emitBits(b, s)
		}

		run = 0
	}

	if run > 0 {
		if e.gather {
			af[0]++
		} else {
			e.emitBits(t.code[0], t.size[0])
		}
	}
}
