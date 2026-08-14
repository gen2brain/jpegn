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
	markerSOF2 = 0xC2
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

// invZz maps a natural coefficient index to its zigzag position.
var invZz [64]uint8

func init() {
	for k, n := range zz {
		invZz[n] = uint8(k)
	}
}

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
	// Progressive writes a progressive JPEG.
	Progressive bool
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

	coeffs             []int32
	masks              []uint64
	nBlocksX, nBlocksY int
	blocksPerLine      int
	blocksPerCol       int
}

// encoder holds the state of the JPEG encoding process.
type encoder struct {
	out           []byte
	acc           uint64
	nacc          uint
	width, height int
	ncomp         int
	comp          [3]encComponent
	hmax, vmax    int
	mcusX, mcusY  int
	qtab          [2][64]uint16 // Zigzag order, as written to DQT.
	qrecip        [2][64]int32  // Natural order, reciprocal of the FDCT-matched divisor.
	qhalf         [2][64]int32  // Natural order, half the divisor, for rounding.
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
	progressive   bool
	scans         []progScan
	sentBits      [2][2][17]uint8
	sentVals      [2][2][256]uint8
	sentCount     [2][2]int
	eobRun        int
	corr          [maxCorrBits + 88]uint8
	nCorr         int
	be            int
	absBuf        [64]int32
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
	prog := false
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
		prog = opts[0].Progressive

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

	if err := e.encode(m, quality, sub, optimize, prog, rst); err != nil {
		return err
	}

	_, err := w.Write(e.out)

	return err
}

// encode runs the full compression pipeline into e.out.
func (e *encoder) encode(m image.Image, quality int, sub Subsampling, optimize, prog bool, rst int) error {
	b := m.Bounds()

	e.out = e.out[:0]
	e.acc = 0
	e.nacc = 0
	e.width = b.Dx()
	e.height = b.Dy()
	e.rst = rst
	e.progressive = prog

	e.setSampling(resolveSampling(m, sub))

	e.mcusX = (e.width + e.hmax*8 - 1) / (e.hmax * 8)
	e.mcusY = (e.height + e.vmax*8 - 1) / (e.vmax * 8)

	e.buildQuant(quality)
	e.buildPlanes(m)

	if prog {
		e.encodeProgressive()

		return nil
	}

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
			nat := zz[i]

			v := (int(base[nat])*scale + 50) / 100
			if v < 1 {
				v = 1
			}

			if v > 255 {
				v = 255
			}

			d := int32(v) * 8
			e.qtab[t][i] = uint16(v)
			e.qhalf[t][nat] = d >> 1
			e.qrecip[t][nat] = int32((1<<quantShift + int64(d) - 1) / int64(d))
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
	e.writeFrameHeader(markerSOF0)

	for t := 0; t < e.nhuff; t++ {
		e.emitDHT(0, t, &e.dcBits[t], e.dcVals[t][:e.dcCount[t]])
		e.emitDHT(1, t, &e.acBits[t], e.acVals[t][:e.acCount[t]])
	}

	e.writeDRI()

	var comps [4]int
	for i := 0; i < e.ncomp; i++ {
		comps[i] = i
	}

	e.writeSOS(&comps, e.ncomp, 0, 63, 0, 0)
}

// writeFrameHeader emits SOI, the metadata segments, the quantization tables
// and the frame header with the given SOF marker.
func (e *encoder) writeFrameHeader(sof byte) {
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

	e.emitMarker(sof)
	e.emitU16(8 + 3*e.ncomp)
	e.out = append(e.out, 8)
	e.emitU16(e.height)
	e.emitU16(e.width)
	e.out = append(e.out, byte(e.ncomp))

	for i := 0; i < e.ncomp; i++ {
		c := &e.comp[i]
		e.out = append(e.out, byte(c.id), byte(c.ssX<<4|c.ssY), byte(c.qtSel))
	}
}

// writeDRI emits the restart interval segment when restarts are enabled.
func (e *encoder) writeDRI() {
	if e.rst > 0 {
		e.emitMarker(markerDRI)
		e.emitU16(4)
		e.emitU16(e.rst)
	}
}

// writeSOS emits a scan header over the given components and spectral range.
func (e *encoder) writeSOS(comps *[4]int, ncomp, ss, se, ah, al int) {
	e.emitMarker(markerSOS)
	e.emitU16(6 + 2*ncomp)
	e.out = append(e.out, byte(ncomp))

	for i := 0; i < ncomp; i++ {
		c := &e.comp[comps[i]]
		e.out = append(e.out, byte(c.id), byte(c.dcSel<<4|c.acSel))
	}

	e.out = append(e.out, byte(ss), byte(se), byte(ah<<4|al))
}

// emitBits appends nbits of code, applying JPEG byte stuffing.
func (e *encoder) emitBits(code uint32, nbits uint8) {
	e.acc = e.acc<<nbits | uint64(code)
	e.nacc += uint(nbits)

	if e.nacc < 32 {
		return
	}

	e.nacc -= 32
	v := uint32(e.acc >> e.nacc)

	// A set bit marks a 0xFF byte, which needs a stuffed zero after it.
	if (^v-0x01010101)&v&0x80808080 != 0 {
		e.stuff(v)

		return
	}

	e.out = append(e.out, byte(v>>24), byte(v>>16), byte(v>>8), byte(v))
}

// emitPair appends a Huffman symbol and its value bits in one accumulator step.
// Both sizes together never exceed 32, the room the accumulator always has.
func (e *encoder) emitPair(code uint32, nbits uint8, val uint32, vbits uint8) {
	n := nbits + vbits
	e.acc = e.acc<<n | uint64(code)<<vbits | uint64(val)
	e.nacc += uint(n)

	if e.nacc < 32 {
		return
	}

	e.nacc -= 32
	v := uint32(e.acc >> e.nacc)

	if (^v-0x01010101)&v&0x80808080 != 0 {
		e.stuff(v)

		return
	}

	e.out = append(e.out, byte(v>>24), byte(v>>16), byte(v>>8), byte(v))
}

// stuff writes four bytes that contain at least one 0xFF.
func (e *encoder) stuff(v uint32) {
	for s := 24; s >= 0; s -= 8 {
		b := byte(v >> uint(s))
		e.out = append(e.out, b)

		if b == 0xFF {
			e.out = append(e.out, 0x00)
		}
	}
}

// flushBits pads to a byte boundary with one bits.
func (e *encoder) flushBits() {
	for e.nacc >= 8 {
		e.nacc -= 8

		b := byte(e.acc >> e.nacc)
		e.out = append(e.out, b)

		if b == 0xFF {
			e.out = append(e.out, 0x00)
		}
	}

	if e.nacc > 0 {
		pad := 8 - e.nacc

		b := byte(e.acc<<pad) | byte(1<<pad-1)
		e.out = append(e.out, b)

		if b == 0xFF {
			e.out = append(e.out, 0x00)
		}
	}

	e.acc = 0
	e.nacc = 0
}

// magnitude returns the JPEG size category of v and the coefficient bits.
func magnitude(v int32) (uint8, uint32) {
	m := v >> 31
	n := bits.Len32(uint32((v ^ m) - m))

	return uint8(n), uint32(v + (1<<uint(n)-1)&m)
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
						e.encodeBlock(ci, mx*c.ssX+bx, my*c.ssY+by)
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

// encodeBlock transforms, quantizes and codes the block at (bx, by).
func (e *encoder) encodeBlock(ci, bx, by int) {
	c := &e.comp[ci]
	blk := &e.blk
	zb := &e.zblk

	fdct(blk, c.plane[by*8*c.stride+bx*8:], c.stride)

	e.encodeCoeffs(zb, c, quantizeBlock(zb, blk, &e.qrecip[c.qtSel], &e.qhalf[c.qtSel]))
}

// encodeCoeffs codes one block of quantized coefficients. nz marks the non-zero
// natural positions; it is permuted to zigzag order so only those are visited.
func (e *encoder) encodeCoeffs(zb *[64]int32, c *encComponent, nz uint64) {
	diff := zb[0] - c.pred
	c.pred = zb[0]

	s, b := magnitude(diff)

	if e.gather {
		e.dcFreq[c.dcSel][s]++
	} else {
		t := &e.dcTab[c.dcSel]
		e.emitPair(t.code[s], t.size[s], b, s)
	}

	af := &e.acFreq[c.acSel]
	t := &e.acTab[c.acSel]

	zm := uint64(0)

	for m := nz &^ 1; m != 0; m &= m - 1 {
		zm |= 1 << invZz[bits.TrailingZeros64(m)]
	}

	prev := 0

	for zm != 0 {
		k := bits.TrailingZeros64(zm)
		zm &= zm - 1

		run := k - prev - 1
		for run > 15 {
			if e.gather {
				af[0xF0]++
			} else {
				e.emitBits(t.code[0xF0], t.size[0xF0])
			}

			run -= 16
		}

		s, b := magnitude(zb[zz[k]])
		sym := run<<4 | int(s)

		if e.gather {
			af[sym]++
		} else {
			e.emitPair(t.code[sym], t.size[sym], b, s)
		}

		prev = k
	}

	if prev < 63 {
		if e.gather {
			af[0]++
		} else {
			e.emitBits(t.code[0], t.size[0])
		}
	}
}

// progScan is one scan of the progressive script.
type progScan struct {
	ss, se, ah, al int
	band           uint64
	comps          [4]int
	ncomp          int
}

// maxCorrBits bounds the pending refinement correction bits.
const maxCorrBits = 1000

// maskLevels is the number of magnitude masks kept per block: bit k of mask l
// is set when the coefficient at zigzag k survives a point transform by l.
const maskLevels = 4

// buildScanScript fills e.scans with jpegli's progressive level 2 script.
func (e *encoder) buildScanScript() {
	specs := [...]struct {
		ss, se, ah, al int
		interleaved    bool
	}{
		{0, 0, 0, 0, e.hmax == 1 && e.vmax == 1},
		{1, 2, 0, 0, false},
		{3, 63, 0, 2, false},
		{3, 63, 2, 1, false},
		{3, 63, 1, 0, false},
	}

	e.scans = e.scans[:0]

	for _, s := range specs {
		band := ^uint64(0)
		if s.se < 63 {
			band = 1<<uint(s.se+1) - 1
		}

		band &^= 1<<uint(s.ss) - 1

		if s.interleaved {
			sc := progScan{ss: s.ss, se: s.se, ah: s.ah, al: s.al, band: band, ncomp: e.ncomp}
			for i := 0; i < e.ncomp; i++ {
				sc.comps[i] = i
			}

			e.scans = append(e.scans, sc)

			continue
		}

		for i := 0; i < e.ncomp; i++ {
			sc := progScan{ss: s.ss, se: s.se, ah: s.ah, al: s.al, band: band, ncomp: 1}
			sc.comps[0] = i

			e.scans = append(e.scans, sc)
		}
	}
}

// encodeProgressive writes the whole progressive image, two passes per scan.
func (e *encoder) encodeProgressive() {
	e.buildCoeffs()
	e.buildScanScript()
	e.setStdTables()

	e.sentCount = [2][2]int{}
	e.sentBits = [2][2][17]uint8{}
	e.sentVals = [2][2][256]uint8{}

	e.writeFrameHeader(markerSOF2)
	e.writeDRI()

	for i := range e.scans {
		s := &e.scans[i]

		e.gather = true
		e.resetScanFreq(s)
		e.progressiveScan(s)
		e.gather = false

		e.writeScanTables(s)
		e.writeSOS(&s.comps, s.ncomp, s.ss, s.se, s.ah, s.al)
		e.progressiveScan(s)
	}

	e.emitMarker(markerEOI)
}

// buildCoeffs transforms and quantizes every block into zigzag order.
func (e *encoder) buildCoeffs() {
	for ci := 0; ci < e.ncomp; ci++ {
		c := &e.comp[ci]

		c.nBlocksX = e.mcusX * c.ssX
		c.nBlocksY = e.mcusY * c.ssY
		c.blocksPerLine = (c.width + 7) >> 3
		c.blocksPerCol = (c.height + 7) >> 3

		blocks := c.nBlocksX * c.nBlocksY

		if need := blocks * 64; cap(c.coeffs) < need {
			c.coeffs = make([]int32, need)
		} else {
			c.coeffs = c.coeffs[:need]
		}

		if need := blocks * maskLevels; cap(c.masks) < need {
			c.masks = make([]uint64, need)
		} else {
			c.masks = c.masks[:need]
		}

		recip := &e.qrecip[c.qtSel]
		half := &e.qhalf[c.qtSel]

		for by := 0; by < c.nBlocksY; by++ {
			for bx := 0; bx < c.nBlocksX; bx++ {
				off := (by*c.nBlocksX + bx) * 64

				fdct(&e.blk, c.plane[by*8*c.stride+bx*8:], c.stride)

				nz := quantizeBlock(&e.zblk, &e.blk, recip, half)

				dst := c.coeffs[off : off+64 : off+64]
				clear(dst)

				var m0, m1, m2, m3 uint64

				for t := nz; t != 0; t &= t - 1 {
					n := bits.TrailingZeros64(t)
					v := e.zblk[n]
					k := invZz[n]
					dst[k] = v

					if v < 0 {
						v = -v
					}

					b := uint64(1) << k
					m0 |= b

					if v > 1 {
						m1 |= b
					}

					if v > 3 {
						m2 |= b
					}

					if v > 7 {
						m3 |= b
					}
				}

				m := c.masks[off/64*maskLevels:]
				m[0], m[1], m[2], m[3] = m0, m1, m2, m3
			}
		}
	}
}

// resetScanFreq clears the histograms of the tables this scan will use.
func (e *encoder) resetScanFreq(s *progScan) {
	for i := 0; i < s.ncomp; i++ {
		c := &e.comp[s.comps[i]]

		if s.ss == 0 {
			e.dcFreq[c.dcSel] = [257]int32{}
		} else {
			e.acFreq[c.acSel] = [257]int32{}
		}
	}
}

// writeScanTables optimizes and emits the Huffman tables this scan will use.
func (e *encoder) writeScanTables(s *progScan) {
	var seen [2]bool

	for i := 0; i < s.ncomp; i++ {
		c := &e.comp[s.comps[i]]

		class, sel, freq := 0, c.dcSel, &e.dcFreq[c.dcSel]
		if s.ss != 0 {
			class, sel, freq = 1, c.acSel, &e.acFreq[c.acSel]
		}

		if seen[sel] {
			continue
		}

		seen[sel] = true

		bits, vals, count := &e.dcBits[sel], &e.dcVals[sel], &e.dcCount[sel]
		tab := &e.dcTab[sel]

		if class == 1 {
			bits, vals, count = &e.acBits[sel], &e.acVals[sel], &e.acCount[sel]
			tab = &e.acTab[sel]
		}

		var nbits [17]uint8
		var nvals [256]uint8

		if n := genOptimalTable(freq, &nbits, &nvals); n > 0 {
			*bits, *vals, *count = nbits, nvals, n
			buildHuffEnc(tab, bits, vals[:n])
		}

		// A table already in effect does not need repeating.
		if e.sentCount[class][sel] == *count && e.sentBits[class][sel] == *bits &&
			e.sentVals[class][sel] == *vals {
			continue
		}

		e.sentBits[class][sel], e.sentVals[class][sel], e.sentCount[class][sel] = *bits, *vals, *count

		e.emitDHT(class, sel, bits, vals[:*count])
	}
}

// progressiveScan runs one scan over its units, gathering or emitting.
func (e *encoder) progressiveScan(s *progScan) {
	for i := 0; i < s.ncomp; i++ {
		e.comp[s.comps[i]].pred = 0
	}

	e.eobRun = 0
	e.nCorr = 0
	e.be = 0

	c := &e.comp[s.comps[0]]
	sel := c.acSel

	units := c.blocksPerLine * c.blocksPerCol
	if s.ncomp > 1 {
		units = e.mcusX * e.mcusY
	}

	n := 0
	rst := 0
	bx := 0
	row := 0

	for u := 0; u < units; u++ {
		switch {
		case s.ncomp > 1:
			e.encodeMCUDC(s, u)
		case s.ss == 0:
			e.encodeDCFirst(c, row+bx)
		case s.ah == 0:
			e.encodeACFirst(c, row+bx, s)
		default:
			e.encodeACRefine(c, row+bx, s)
		}

		if bx++; bx == c.blocksPerLine {
			bx = 0
			row += c.nBlocksX
		}

		n++

		if e.rst > 0 && n == e.rst && u != units-1 {
			e.flushEOBRun(sel)

			if !e.gather {
				e.flushBits()
				e.emitMarker(byte(markerRST0 + rst))
			}

			rst = (rst + 1) & 7
			n = 0

			for i := 0; i < s.ncomp; i++ {
				e.comp[s.comps[i]].pred = 0
			}
		}
	}

	e.flushEOBRun(sel)

	if !e.gather {
		e.flushBits()
	}
}

// encodeMCUDC codes the DC coefficients of one interleaved MCU.
func (e *encoder) encodeMCUDC(s *progScan, mcu int) {
	mx := mcu % e.mcusX
	my := mcu / e.mcusX

	for i := 0; i < s.ncomp; i++ {
		c := &e.comp[s.comps[i]]

		for by := 0; by < c.ssY; by++ {
			for bx := 0; bx < c.ssX; bx++ {
				e.encodeDCFirst(c, (my*c.ssY+by)*c.nBlocksX+mx*c.ssX+bx)
			}
		}
	}
}

// putAC codes one AC symbol followed by nb bits of value.
func (e *encoder) putAC(sel, sym int, val uint32, nb uint8) {
	if e.gather {
		e.acFreq[sel][sym]++

		return
	}

	t := &e.acTab[sel]
	e.emitPair(t.code[sym], t.size[sym], val, nb)
}

// putBits appends n raw bits, which carry no statistics.
func (e *encoder) putBits(val uint32, n uint8) {
	if !e.gather {
		e.emitBits(val&(1<<n-1), n)
	}
}

// flushEOBRun closes a pending end-of-band run and its correction bits.
func (e *encoder) flushEOBRun(sel int) {
	if e.eobRun == 0 {
		return
	}

	nbits := bits.Len32(uint32(e.eobRun)) - 1
	e.putAC(sel, nbits<<4, 0, 0)

	if nbits > 0 {
		e.putBits(uint32(e.eobRun), uint8(nbits))
	}

	e.eobRun = 0

	e.emitCorr(e.be)

	e.nCorr = copy(e.corr[:], e.corr[e.be:e.nCorr])
	e.be = 0
}

// flushCorr appends the correction bits belonging to the symbol just written.
func (e *encoder) flushCorr() {
	e.emitCorr(e.nCorr)
	e.nCorr = 0
}

// emitCorr writes the first n correction bits, packed into words.
func (e *encoder) emitCorr(n int) {
	if e.gather {
		return
	}

	for i := 0; i < n; {
		k := min(n-i, 24)

		v := uint32(0)
		for _, b := range e.corr[i : i+k] {
			v = v<<1 | uint32(b)
		}

		e.emitBits(v, uint8(k))

		i += k
	}
}

// encodeDCFirst codes the DC difference of one block.
func (e *encoder) encodeDCFirst(c *encComponent, blk int) {
	v := c.coeffs[blk*64]
	diff := v - c.pred
	c.pred = v

	s, b := magnitude(diff)

	if e.gather {
		e.dcFreq[c.dcSel][s]++

		return
	}

	t := &e.dcTab[c.dcSel]
	e.emitPair(t.code[s], t.size[s], b, s)
}

// pointTransform divides by 2^al, rounding towards zero.
func pointTransform(v int32, al uint) int32 {
	m := v >> 31
	a := ((v ^ m) - m) >> al

	return (a ^ m) - m
}

// encodeACFirst codes the first pass of one spectral band.
func (e *encoder) encodeACFirst(c *encComponent, blk int, s *progScan) {
	sel := c.acSel
	sig := c.masks[blk*maskLevels+s.al] & s.band

	if sig == 0 {
		e.eobRun++

		if e.eobRun == 0x7FFF {
			e.flushEOBRun(sel)
		}

		return
	}

	coefs := c.coeffs[blk*64 : blk*64+64 : blk*64+64]
	al := uint(s.al)
	prev := s.ss - 1

	for m := sig; m != 0; m &= m - 1 {
		k := bits.TrailingZeros64(m)
		run := k - prev - 1
		prev = k

		e.flushEOBRun(sel)

		for run > 15 {
			e.putAC(sel, 0xF0, 0, 0)
			run -= 16
		}

		n, b := magnitude(pointTransform(coefs[k], al))
		e.putAC(sel, run<<4|int(n), b, n)
	}

	if prev < s.se {
		e.eobRun++

		if e.eobRun == 0x7FFF {
			e.flushEOBRun(sel)
		}
	}
}

// encodeACRefine codes a refinement pass of one spectral band.
func (e *encoder) encodeACRefine(c *encComponent, blk int, s *progScan) {
	sel := c.acSel
	sig := c.masks[blk*maskLevels+s.al] & s.band

	if sig == 0 {
		e.endRefineBlock(sel, 0, 1)

		return
	}

	big := c.masks[blk*maskLevels+s.al+1] & s.band
	coefs := c.coeffs[blk*64 : blk*64+64 : blk*64+64]
	al := uint(s.al)

	eob := 0
	if n := sig &^ big; n != 0 {
		eob = 63 - bits.LeadingZeros64(n)
	}

	run := 0
	start := e.nCorr
	prev := s.ss - 1

	for m := sig; m != 0; m &= m - 1 {
		k := bits.TrailingZeros64(m)
		run += k - prev - 1
		prev = k

		for run > 15 && k <= eob {
			e.flushEOBRun(sel)
			e.putAC(sel, 0xF0, 0, 0)
			run -= 16
			e.flushCorr()
			start = 0
		}

		v := coefs[k]

		if big&(1<<uint(k)) != 0 {
			m := v >> 31
			e.corr[e.nCorr] = uint8(((v ^ m) - m) >> al & 1)
			e.nCorr++

			continue
		}

		e.flushEOBRun(sel)
		e.putAC(sel, run<<4|1, 0, 0)

		bit := uint32(1)
		if v < 0 {
			bit = 0
		}

		e.putBits(bit, 1)
		e.flushCorr()

		start = 0
		run = 0
	}

	e.endRefineBlock(sel, start, run+s.se-prev)
}

// endRefineBlock counts the end-of-band of a refined block and hands its
// trailing correction bits to the pending run.
func (e *encoder) endRefineBlock(sel, start, run int) {
	if run == 0 && e.nCorr == start {
		return
	}

	e.eobRun++
	e.be = e.nCorr

	if e.eobRun == 0x7FFF || e.be > maxCorrBits-64+1 {
		e.flushEOBRun(sel)
	}
}
