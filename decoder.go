package jpegn

import (
	"errors"
	"fmt"
	"image"
)

// vlcCode represents a single entry in the pre-calculated Huffman lookup table.
// It stores the number of bits for the code and the decoded value.
type vlcCode struct {
	bits, code uint8
}

// component stores information about a single color component (e.g., Y, Cb, or Cr).
type component struct {
	id                 int    // Component identifier (e.g., 1 for Y, 2 for Cb, 3 for Cr).
	ssX, ssY           int    // Subsampling factors for X and Y axes.
	width, height      int    // Dimensions of this component in pixels.
	stride             int    // The number of bytes from one row of pixels to the next.
	qtSel              int    // Quantization table selector.
	acTabSel, dcTabSel int    // Huffman table selectors for AC and DC coefficients.
	dcPred             int    // DC prediction value for differential coding.
	pixels             []byte // Decoded pixel data for this component.
}

// decoder holds the state of the JPEG decoding process.
type decoder struct {
	jpegData          []byte                    // Input buffer containing the entire JPEG file.
	pos               int                       // Current position index in the input buffer.
	size              int                       // Remaining bytes to be processed.
	length            int                       // Length of the current marker segment.
	width, height     int                       // Dimensions of the final image.
	mbWidth, mbHeight int                       // Dimensions of the image in MCU (Minimum Coded Unit) blocks.
	mbSizeX, mbSizeY  int                       // Dimensions of a single MCU in pixels.
	ncomp             int                       // Number of color components (1 for grayscale, 3 for color).
	comp              [3]component              // Array to hold data for each color component.
	qtUsed, qtAvail   int                       // Bitmasks tracking used and available quantization tables.
	qtab              [4]*[64]uint8             // Pointers for pooling.
	vlcTab            [4]*[65536]vlcCode        // Pointers for pooling.
	buf               uint64                    // Use uint64 for fewer refills.
	bufBits           int                       // Number of valid bits in the bit buffer.
	block             [64]int32                 // Temporary storage for a single 8x8 block of DCT coefficients.
	rstInterval       int                       // Restart interval in MCUs, for error resilience.
	pixels            []byte                    // Final decoded pixel data.
	subsampleRatio    image.YCbCrSubsampleRatio // The detected YCbCr subsampling ratio.
	isRGB             bool                      // True if the image is encoded as RGB instead of YCbCr.
	upsampleMethod    UpsampleMethod            // The upsampling method to use.
	toRGBA            bool                      // Whether to convert the final image to RGBA.
	autoRotate        bool                      // Whether to auto-rotate based on EXIF orientation.
	orientation       int                       // EXIF orientation tag (1-8).
}

// errDecode is used for internal panics during the hot decoding path.
type errDecode struct{ error }

// newDecoder creates a new decoder instance and allocates the large tables.
func newDecoder() *decoder {
	d := new(decoder)
	for i := 0; i < 4; i++ {
		d.qtab[i] = new([64]uint8)
		d.vlcTab[i] = new([65536]vlcCode)
	}

	return d
}

// reset clears the decoder state for reuse, preserving the allocated tables.
func (d *decoder) reset() {
	// Save pointers to the tables.
	vlcTmp := d.vlcTab
	qtabTmp := d.qtab

	// Zero the struct. This clears references (jpegData, pixels, etc.) allowing GC, and resets all state variables.
	*d = decoder{}

	// Restore pointers to the tables.
	d.vlcTab = vlcTmp
	d.qtab = qtabTmp
}

// panic triggers an internal panic to signal a decoding error in the hot path.
func (d *decoder) panic(err error) {
	panic(errDecode{err})
}

// zz is the zigzag ordering table. It maps the 1D order of coefficients in the JPEG stream to their 2D position in an 8x8 block.
var zz = [64]int{
	0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18,
	11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
	42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
	38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
}

// clip clamps an int32 value to the valid 8-bit pixel range [0, 255].
func clip(x int32) byte {
	if x < 0 {
		return 0
	}

	if x > 255 {
		return 255
	}

	return byte(x)
}

// Bitstream handling

// showBits reads 'bits' number of bits from the bitstream without consuming them.
// It ensures the internal buffer `d.buf` has enough data, reading from the main
// jpegData slice if necessary. It also handles JPEG byte stuffing (0xFF00).
func (d *decoder) showBits(bits int) int {
	if bits == 0 {
		return 0
	}

	// Loop to fill the buffer until we have enough bits or hit a marker.
fillLoop:
	for d.bufBits < bits {
		if d.size <= 0 {
			// If we run out of data, pad with 0xFF (STOP codes).
			// Check if we already have enough bits before padding.
			if d.bufBits >= bits {
				break
			}

			d.buf = (d.buf << 8) | 0xFF
			d.bufBits += 8

			continue
		}

		b := d.jpegData[d.pos]
		d.pos++
		d.size--

		// Tentatively increase the bit count. We will revert this if 'b' is a marker prefix.
		d.bufBits += 8

		if b == 0xFF {
			if d.size <= 0 {
				// Reached EOF after a 0xFF. Treat 0xFF as data.
			} else {
				b2 := d.jpegData[d.pos] // Peek at the next byte.

				if b2 != 0 {
					// It's a marker (not 0xFF00).
					isRST := (b2 | 7) == 0xD7 // Checks if b2 is 0xD0-0xD7

					if !isRST {
						// It's a non-RST marker (including 0xFFFF).
						// Rewind so the main loop can see the 0xFF marker.
						// Crucially, revert the bit count and DO NOT add 0xFF to the buffer.
						d.pos--
						d.size++
						d.bufBits -= 8

						break fillLoop // Stop filling the buffer.
					}
					// If it's an RST marker, we fall through and treat it as data 0xFF.
				} else {
					// It was 0xFF00 (stuffing byte). Consume the 0x00.
					d.pos++
					d.size--

					// Fall through (treat 0xFF as data).
				}
			}
		}

		// If we reached here, 'b' is data. Add it to the buffer.
		d.buf = (d.buf << 8) | uint64(b)
	}

	// Calculate the result.
	shift := d.bufBits - bits
	var res uint64
	if shift >= 0 {
		res = d.buf >> shift
	} else {
		// This case handles an underfull buffer (e.g., after hitting a marker or EOF padding).
		res = d.buf << (-shift)
		padMask := uint64((1 << (-shift)) - 1)
		res |= padMask // Pad with 1s.
	}

	return int(res & ((1 << bits) - 1))
}

// skipBits consumes 'bits' number of bits from the bitstream.
func (d *decoder) skipBits(bits int) {
	if d.bufBits < bits {
		// We must ensure the buffer is filled (handling byte stuffing) even if we just skip.
		d.showBits(bits)
	}

	if d.bufBits < bits {
		d.bufBits = 0
	} else {
		d.bufBits -= bits
	}
}

// getBits reads and consumes 'bits' number of bits from the bitstream.
func (d *decoder) getBits(bits int) int {
	res := d.showBits(bits)
	d.skipBits(bits)

	return res
}

// byteAlign aligns the bitstream to the next byte boundary.
func (d *decoder) byteAlign() {
	d.bufBits &= ^7 // equivalent to (d.bufBits / 8) * 8
}

// skip advances the current position in the jpegData buffer by 'count' bytes.
func (d *decoder) skip(count int) error {
	d.pos += count
	d.size -= count

	if d.length >= count {
		d.length -= count
	} else {
		d.length = 0
	}

	if d.size < 0 {
		return ErrSyntax
	}

	return nil
}

// decode16 reads a 16-bit big-endian integer from the specified offset.
func (d *decoder) decode16(offset int) int {
	p := d.pos + offset

	return (int(d.jpegData[p]) << 8) | int(d.jpegData[p+1])
}

// decodeLength reads the 16-bit length field of a JPEG marker segment and updates the decoder's internal length counter.
func (d *decoder) decodeLength() error {
	if d.size < 2 {
		return ErrSyntax
	}

	d.length = d.decode16(0)
	if d.length > d.size {
		return ErrSyntax
	}

	if d.length < 2 {
		return ErrSyntax // Length must include its own 2 bytes.
	}

	// Skip the 2 bytes of the length field itself.
	// d.length will now hold the size of the remaining payload.
	return d.skip(2)
}

// skipMarker reads the length of the current marker's payload and skips it.
func (d *decoder) skipMarker() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	return d.skip(d.length)
}

// Marker Decoders

// decodeAPP1 decodes the APP1 marker segment, typically containing EXIF metadata.
// It specifically looks for the orientation tag if auto-rotation is enabled.
func (d *decoder) decodeAPP1() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	// We only parse EXIF if auto-rotation is enabled.
	if !d.autoRotate {
		return d.skip(d.length)
	}

	// Check for "Exif\0\0" signature (6 bytes).
	if d.length >= 6 &&
		d.jpegData[d.pos+0] == 'E' &&
		d.jpegData[d.pos+1] == 'x' &&
		d.jpegData[d.pos+2] == 'i' &&
		d.jpegData[d.pos+3] == 'f' &&
		d.jpegData[d.pos+4] == 0 &&
		d.jpegData[d.pos+5] == 0 {

		// Start parsing TIFF header inside EXIF payload, starting after the 6-byte signature.
		d.parseExif(6)
	}

	// Skip any remaining data in the segment.
	// d.length is updated by d.skip() calls.
	return d.skip(d.length)
}

// parseExif parses the TIFF header and IFD structure within the EXIF payload to find the orientation tag.
// 'offset' is relative to the start of the APP1 payload (d.pos).
func (d *decoder) parseExif(offset int) {
	// Check if there is enough data for the TIFF header (8 bytes).
	if d.length < offset+8 {
		return
	}

	// TIFF Header starts at d.pos + offset.
	tiffHeaderPos := d.pos + offset

	// Check Byte order (MM or II).
	var littleEndian bool
	byteOrder := d.jpegData[tiffHeaderPos : tiffHeaderPos+2]

	if byteOrder[0] == 0x49 && byteOrder[1] == 0x49 { // II (Intel)
		littleEndian = true
	} else if byteOrder[0] == 0x4D && byteOrder[1] == 0x4D { // MM (Motorola)
		littleEndian = false
	} else {
		return // Invalid byte order marker.
	}

	// Helper functions for reading 16/32-bit integers based on endianness.
	// These reads relative to the TIFF header start (tiffHeaderPos).
	read16 := func(relOffset int) uint16 {
		p := tiffHeaderPos + relOffset
		if littleEndian {
			return uint16(d.jpegData[p]) | (uint16(d.jpegData[p+1]) << 8)
		}

		return (uint16(d.jpegData[p]) << 8) | uint16(d.jpegData[p+1])
	}

	read32 := func(relOffset int) uint32 {
		p := tiffHeaderPos + relOffset
		if littleEndian {
			return uint32(d.jpegData[p]) | (uint32(d.jpegData[p+1]) << 8) | (uint32(d.jpegData[p+2]) << 16) | (uint32(d.jpegData[p+3]) << 24)
		}

		return (uint32(d.jpegData[p]) << 24) | (uint32(d.jpegData[p+1]) << 16) | (uint32(d.jpegData[p+2]) << 8) | uint32(d.jpegData[p+3])
	}

	// Check the magic number (42) at offset 2.
	if read16(2) != 42 {
		return
	}

	// Read the offset to the first IFD (Image File Directory) at offset 4.
	ifdOffset := read32(4)

	// Basic validation of IFD offset.
	if ifdOffset < 8 {
		return
	}

	// Calculate the available data size starting from the TIFF header.
	availableDataSize := d.length - offset

	// Ensure the IFD offset is within the segment bounds.
	if uint32(availableDataSize) < ifdOffset {
		return
	}

	// Read the number of entries in the IFD (2 bytes).
	if uint32(availableDataSize) < ifdOffset+2 {
		return
	}
	numEntries := read16(int(ifdOffset))

	// IFD entries are 12 bytes long. Check if all entries fit within the segment.
	// If not, we truncate the number of entries we read (robustness against corrupted files).
	requiredSize := ifdOffset + 2 + uint32(numEntries)*12
	if uint32(availableDataSize) < requiredSize {
		maxEntries := (uint32(availableDataSize) - ifdOffset - 2) / 12
		numEntries = uint16(maxEntries)
	}

	// Iterate through IFD entries to find the Orientation tag (0x0112).
	entryOffset := int(ifdOffset) + 2
	const orientationTag = 0x0112

	for i := 0; i < int(numEntries); i++ {
		tag := read16(entryOffset)
		if tag == orientationTag {
			// Check format (Type 3 = SHORT, 2 bytes).
			format := read16(entryOffset + 2)
			if format != 3 {
				break // Invalid format for Orientation
			}

			// Check count (must be 1).
			count := read32(entryOffset + 4)
			if count != 1 {
				break // Invalid count for Orientation
			}

			// Read the orientation value (stored in the first 2 bytes of the offset field).
			orientation := read16(entryOffset + 8)
			if orientation >= 1 && orientation <= 8 {
				d.orientation = int(orientation)
			}

			break // Found it, stop searching.
		}

		entryOffset += 12
	}
}

// decodeAPP14 decodes the APP14 "Adobe" marker segment, which specifies the color space transformation.
func (d *decoder) decodeAPP14() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	// Check for the "Adobe" signature.
	if d.length >= 12 &&
		d.jpegData[d.pos+0] == 'A' &&
		d.jpegData[d.pos+1] == 'd' &&
		d.jpegData[d.pos+2] == 'o' &&
		d.jpegData[d.pos+3] == 'b' &&
		d.jpegData[d.pos+4] == 'e' {

		// The colorTransform byte is at offset 11.
		// 0: RGB (or Grayscale for 1-component)
		// 1: YCbCr
		// 2: YCCK
		colorTransform := d.jpegData[d.pos+11]
		if colorTransform == 0 {
			d.isRGB = true
		}
	}

	return d.skip(d.length)
}

// decodeSOF decodes the Start of Frame segment. It extracts image dimensions,
// number of components, and component-specific information like subsampling factors.
// If configOnly is true, it doesn't allocate memory for pixel data.
func (d *decoder) decodeSOF(configOnly bool) error {
	ssxMax, ssyMax := 0, 0
	if err := d.decodeLength(); err != nil {
		return err
	}

	if d.length < 9 {
		return ErrSyntax
	}

	if d.jpegData[d.pos] != 8 {
		return ErrUnsupported // Precision must be 8-bit.
	}

	d.height = d.decode16(1)
	d.width = d.decode16(3)
	if d.width == 0 || d.height == 0 {
		return ErrSyntax
	}

	d.ncomp = int(d.jpegData[d.pos+5])
	if err := d.skip(6); err != nil {
		return err
	}

	switch d.ncomp {
	case 1, 3: // Grayscale or YCbCr/RGB
	default:
		return ErrUnsupported
	}

	if d.length < (d.ncomp * 3) {
		return ErrSyntax
	}

	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		c.id = int(d.jpegData[d.pos])

		c.ssX = int(d.jpegData[d.pos+1]) >> 4
		if c.ssX == 0 || (c.ssX&(c.ssX-1)) != 0 {
			return ErrUnsupported // Subsampling factor must be a power of two.
		}

		c.ssY = int(d.jpegData[d.pos+1]) & 15
		if c.ssY == 0 || (c.ssY&(c.ssY-1)) != 0 {
			return ErrUnsupported // Subsampling factor must be a power of two.
		}

		c.qtSel = int(d.jpegData[d.pos+2])
		if (c.qtSel & 0xFC) != 0 {
			return ErrSyntax
		}

		if err := d.skip(3); err != nil {
			return err
		}

		d.qtUsed |= 1 << c.qtSel
		if c.ssX > ssxMax {
			ssxMax = c.ssX
		}

		if c.ssY > ssyMax {
			ssyMax = c.ssY
		}
	}

	if d.ncomp == 1 {
		c := &d.comp[0]
		c.ssX, c.ssY = 1, 1
		ssxMax, ssyMax = 1, 1
		d.subsampleRatio = image.YCbCrSubsampleRatio444
	} else if d.ncomp == 3 {
		// Check for RGB component IDs as a fallback to APP14 marker.
		if d.comp[0].id == 'R' && d.comp[1].id == 'G' && d.comp[2].id == 'B' {
			d.isRGB = true
		}

		if d.isRGB {
			d.subsampleRatio = image.YCbCrSubsampleRatio444 // Treat as 4:4:4
		} else {
			y, cb, cr := &d.comp[0], &d.comp[1], &d.comp[2]
			if cb.ssX == 1 && cb.ssY == 1 && cr.ssX == 1 && cr.ssY == 1 {
				switch {
				case y.ssX == 1 && y.ssY == 1:
					d.subsampleRatio = image.YCbCrSubsampleRatio444
				case y.ssX == 2 && y.ssY == 1:
					d.subsampleRatio = image.YCbCrSubsampleRatio422
				case y.ssX == 2 && y.ssY == 2:
					d.subsampleRatio = image.YCbCrSubsampleRatio420
				case y.ssX == 1 && y.ssY == 2:
					d.subsampleRatio = image.YCbCrSubsampleRatio440
				case y.ssX == 4 && y.ssY == 1:
					d.subsampleRatio = image.YCbCrSubsampleRatio411
				case y.ssX == 4 && y.ssY == 2:
					d.subsampleRatio = image.YCbCrSubsampleRatio410
				default:
					d.subsampleRatio = -1
				}
			} else {
				d.subsampleRatio = -1
			}

			if d.subsampleRatio == -1 {
				errMsg := fmt.Sprintf("unsupported YCbCr subsampling ratio (Y:%dx%d, Cb:%dx%d, Cr:%dx%d)",
					y.ssX, y.ssY, cb.ssX, cb.ssY, cr.ssX, cr.ssY)

				return fmt.Errorf("%s: %w", errMsg, ErrUnsupported)
			}
		}
	}

	// Calculate MCU dimensions and image dimensions in MCUs.
	d.mbSizeX = ssxMax << 3
	d.mbSizeY = ssyMax << 3
	d.mbWidth = (d.width + d.mbSizeX - 1) / d.mbSizeX
	d.mbHeight = (d.height + d.mbSizeY - 1) / d.mbSizeY

	// Calculate component dimensions and allocate memory for pixels.
	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		c.width = (d.width*c.ssX + ssxMax - 1) / ssxMax
		c.height = (d.height*c.ssY + ssyMax - 1) / ssyMax
		c.stride = d.mbWidth * c.ssX << 3

		if ((c.width < 3) && (c.ssX != ssxMax)) || ((c.height < 3) && (c.ssY != ssyMax)) {
			return ErrUnsupported // Smoothing is not supported.
		}

		if !configOnly {
			pixelSize := c.stride * d.mbHeight * c.ssY << 3
			if pixelSize <= 0 && (d.width > 0 && d.height > 0) {
				return ErrOutOfMemory
			}

			if pixelSize > 0 {
				c.pixels = make([]byte, pixelSize)
			}
		}
	}

	// Allocate the final RGBA buffer if we know we will need it (ToRGBA, source is RGB),
	// or if AutoRotate is enabled (as rotation requires an RGBA buffer).
	if !configOnly && (d.toRGBA || d.isRGB || d.autoRotate) {
		rgbaSize := d.width * d.height * 4
		if rgbaSize <= 0 && (d.width > 0 && d.height > 0) {
			return ErrOutOfMemory
		}

		if rgbaSize > 0 {
			d.pixels = make([]byte, rgbaSize)
		}
	}

	if d.length > 0 {
		return d.skip(d.length)
	}

	return nil
}

// decodeDHT decodes the Define Huffman Table segment. It parses Huffman table
// specifications and builds fast lookup tables for entropy decoding.
func (d *decoder) decodeDHT() error {
	var counts [16]uint8
	if err := d.decodeLength(); err != nil {
		return err
	}

	for d.length >= 17 {
		i := int(d.jpegData[d.pos])
		if (i & 0xEC) != 0 {
			return ErrSyntax
		}

		if (i & 0x02) != 0 {
			// This is an early check for progressive JPEGs which are not supported by this decoder.
			// The main check is for the SOF2 marker.
			return ErrUnsupported
		}

		i = (i | (i >> 3)) & 3 // Table index: 0-1 for DC, 2-3 for AC.

		// Read counts of codes for each length (1-16 bits).
		for codeLen := 1; codeLen <= 16; codeLen++ {
			counts[codeLen-1] = d.jpegData[d.pos+codeLen]
		}

		if err := d.skip(17); err != nil {
			return err
		}

		var n int
		for _, num := range counts {
			n += int(num)
		}

		if n > 256 || n > d.length {
			return ErrSyntax
		}

		// Build the lookup table using canonical Huffman codes.
		vlc := d.vlcTab[i]

		// Pooling: Clear the table before filling it.
		*vlc = [65536]vlcCode{}

		var huffCode uint32
		valueIdx := 0

		for codeLen := 1; codeLen <= 16; codeLen++ {
			numCodes := int(counts[codeLen-1])
			for k := 0; k < numCodes; k++ {
				huffVal := d.jpegData[d.pos+valueIdx]
				valueIdx++
				shift := 16 - codeLen
				numEntries := 1 << shift
				baseIndex := huffCode << shift

				for j := 0; j < numEntries; j++ {
					index := baseIndex + uint32(j)
					if index < 65536 {
						vlc[index].bits = uint8(codeLen)
						vlc[index].code = huffVal
					}
				}

				huffCode++
			}

			huffCode <<= 1
		}

		if err := d.skip(n); err != nil {
			return err
		}
	}

	if d.length != 0 {
		return ErrSyntax
	}

	return nil
}

// decodeDQT decodes the Define Quantization Table segment. It parses and stores
// the 8x8 quantization matrices used for dequantizing DCT coefficients.
func (d *decoder) decodeDQT() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	for d.length >= 65 {
		i := int(d.jpegData[d.pos])
		if (i & 0xFC) != 0 {
			return ErrSyntax
		}

		d.qtAvail |= 1 << i
		t := d.qtab[i]

		// Pooling: Clear the table before filling it.
		*t = [64]uint8{}

		for j := 0; j < 64; j++ {
			t[j] = d.jpegData[d.pos+j+1]
		}

		if err := d.skip(65); err != nil {
			return err
		}
	}

	if d.length != 0 {
		return ErrSyntax
	}

	return nil
}

// decodeDRI decodes the Define Restart Interval segment. This specifies how often
// restart markers are embedded in the scan data for error resilience.
func (d *decoder) decodeDRI() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	if d.length < 2 {
		return ErrSyntax
	}

	d.rstInterval = d.decode16(0)

	return d.skip(d.length)
}

// Entropy Decoding

// getVLC decodes a single Variable-Length Code (VLC) from the bitstream using
// the pre-built Huffman tables. It returns the decoded integer value.
// Uses panic for errors, uint64 buffer, and fast path extraction.
func (d *decoder) getVLC(vlc *[65536]vlcCode, code *uint8) int {
	// Peek 16 bits for Huffman lookup. This ensures the buffer is filled.
	value16 := d.showBits(16)

	// Lookup Huffman code details from the pre-built table.
	entry := vlc[value16]
	huffBits := int(entry.bits)
	if huffBits == 0 {
		d.panic(ErrSyntax) // Invalid Huffman code.
	}

	huffCode := entry.code
	if code != nil {
		*code = huffCode
	}

	valBits := int(huffCode & 15)

	// Handle EOB/ZRL (valBits == 0).
	if valBits == 0 {
		d.skipBits(huffBits)

		return 0
	}

	totalBits := huffBits + valBits

	// Fast path: Check if we have enough bits for Huffman code + value in the buffer.
	// With an uint64 buffer, this is highly likely.
	if d.bufBits >= totalBits {
		// Extract the value.
		// d.buf holds bits left-aligned. Shift right to align the end of the value to the LSB.
		shift := d.bufBits - totalBits
		mask := (uint64(1) << valBits) - 1
		value := int((d.buf >> shift) & mask)

		// Consume bits directly from the buffer.
		d.bufBits -= totalBits

		// Sign extension.
		if value < (1 << (valBits - 1)) {
			value += ((-1) << valBits) + 1
		}

		return value
	}

	// Slow path: Not enough bits in the buffer (rare with uint64).

	// Consume Huffman bits.
	d.skipBits(huffBits)

	// Read value bits. This will trigger buffer refill if needed.
	value := d.getBits(valBits)

	// Sign extension.
	if value < (1 << (valBits - 1)) {
		value += ((-1) << valBits) + 1
	}

	return value
}

// decodeBlock decodes a single 8x8 block of a component. This involves
// entropy decoding of DC and AC coefficients, dequantization, and applying the IDCT.
// Uses panic for errors and optimized table access.
func (d *decoder) decodeBlock(c *component, outOffset int) {
	var code uint8
	var value int

	// This clears the array to zeros.
	d.block = [64]int32{}

	// Cache pointers to tables used in the loop.
	qt := d.qtab[c.qtSel]
	dcVLC := d.vlcTab[c.dcTabSel]
	acVLC := d.vlcTab[c.acTabSel+2]

	// Decode DC coefficient.
	value = d.getVLC(dcVLC, nil)

	c.dcPred += value
	d.block[0] = int32(c.dcPred) * int32(qt[0])

	// Decode AC coefficients.
	coef := 1
	for coef <= 63 {
		value = d.getVLC(acVLC, &code)

		if code == 0 { // EOB (End of Block)
			break
		}

		if (code & 0x0F) == 0 {
			if code != 0xF0 { // ZRL (Zero Run Length)
				d.panic(ErrSyntax)
			}

			coef += 16

			continue
		}

		coef += int(code >> 4) // Skip zero coefficients.
		if coef > 63 {
			d.panic(ErrSyntax)
		}

		// Dequantize and store in zigzag order.
		d.block[zz[coef]] = int32(value) * int32(qt[coef])
		coef++
	}

	// Perform 2D IDCT.
	// This calls the unified function which handles both passes,
	// selecting the optimized implementation (e.g., AVX2) if available.
	idct(&d.block, c.pixels, outOffset, c.stride)
}

// decodeScan decodes the image scan data. It iterates through all MCUs in the
// image, decoding each block for each component.
// Handles panics from the hot path.
func (d *decoder) decodeScan() (err error) {
	// Setup recovery for panics in the hot path (getVLC, decodeBlock).
	defer func() {
		if r := recover(); r != nil {
			if de, ok := r.(errDecode); ok {
				err = de.error
			} else {
				// Propagate other panics (e.g., runtime errors like index out of bounds)
				panic(r)
			}
		}
	}()

	rstCount := d.rstInterval
	nextRst := 0

	if err := d.decodeLength(); err != nil {
		return err
	}

	if d.length < (4 + 2*d.ncomp) {
		return ErrSyntax
	}

	if int(d.jpegData[d.pos]) != d.ncomp {
		return ErrUnsupported // Must match component count from SOF.
	}

	if err := d.skip(1); err != nil {
		return err
	}

	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		if int(d.jpegData[d.pos]) != c.id {
			return ErrSyntax
		}

		c.dcTabSel = int(d.jpegData[d.pos+1]) >> 4
		c.acTabSel = int(d.jpegData[d.pos+1]) & 0x0F
		if c.dcTabSel > 3 || c.acTabSel > 3 {
			return ErrSyntax
		}

		if err := d.skip(2); err != nil {
			return err
		}
	}

	// Check for baseline DCT parameters.
	if d.jpegData[d.pos] != 0 || (d.jpegData[d.pos+1] != 63) || d.jpegData[d.pos+2] != 0 {
		return ErrUnsupported
	}

	if err := d.skip(d.length); err != nil {
		return err
	}

	// Reset DC predictors at the start of the scan.
	for k := 0; k < 3; k++ {
		d.comp[k].dcPred = 0
	}

	d.buf = 0
	d.bufBits = 0

	for mby := 0; mby < d.mbHeight; mby++ {
		for mbx := 0; mbx < d.mbWidth; mbx++ {
			for i := 0; i < d.ncomp; i++ {
				c := &d.comp[i]

				for sby := 0; sby < c.ssY; sby++ {
					for sbx := 0; sbx < c.ssX; sbx++ {
						offset := ((mby*c.ssY+sby)*c.stride + mbx*c.ssX + sbx) << 3

						d.decodeBlock(c, offset)
					}
				}
			}

			// Handle restart markers.
			if d.rstInterval != 0 {
				rstCount--
				if rstCount == 0 {
					d.byteAlign()

					i := d.getBits(16)

					if ((i & 0xFFF8) != 0xFFD0) || ((i & 7) != nextRst) {
						d.panic(ErrSyntax)
					}

					nextRst = (nextRst + 1) & 7
					rstCount = d.rstInterval

					for k := 0; k < 3; k++ {
						d.comp[k].dcPred = 0
					}
				}
			}
		}
	}

	return nil
}

// convert handles upsampling of chroma components (if needed) and converts the final YCbCr/RGB pixel data to RGBA format.
func (d *decoder) convert() error {
	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]

		if c.width < d.width || c.height < d.height {
			switch d.upsampleMethod {
			case CatmullRom:
				upsampleCatmullRom(c, d.width, d.height)
			case NearestNeighbor:
				fallthrough
			default:
				upsampleNearestNeighbor(c, d.width, d.height)
			}
		}

		if c.width < d.width || c.height < d.height {
			return ErrInternal
		}
	}

	if d.ncomp == 3 {
		if d.isRGB {
			// RGB to RGBA conversion
			rgbaOffset := 0
			pr, pg, pb := 0, 0, 0

			for yy := 0; yy < d.height; yy++ {
				for x := 0; x < d.width; x++ {
					d.pixels[rgbaOffset] = d.comp[0].pixels[pr+x]   // R
					d.pixels[rgbaOffset+1] = d.comp[1].pixels[pg+x] // G
					d.pixels[rgbaOffset+2] = d.comp[2].pixels[pb+x] // B
					d.pixels[rgbaOffset+3] = 255                    // A
					rgbaOffset += 4
				}

				pr += d.comp[0].stride
				pg += d.comp[1].stride
				pb += d.comp[2].stride
			}
		} else {
			// YCbCr to RGBA conversion
			rgbaOffset := 0
			py, pcb, pcr := 0, 0, 0

			for yy := 0; yy < d.height; yy++ {
				for x := 0; x < d.width; x++ {
					y := int32(d.comp[0].pixels[py+x]) << 8
					cb := int32(d.comp[1].pixels[pcb+x]) - 128
					cr := int32(d.comp[2].pixels[pcr+x]) - 128

					r := (y + 359*cr + 128) >> 8
					g := (y - 88*cb - 183*cr + 128) >> 8
					b := (y + 454*cb + 128) >> 8

					d.pixels[rgbaOffset] = clip(r)   // R
					d.pixels[rgbaOffset+1] = clip(g) // G
					d.pixels[rgbaOffset+2] = clip(b) // B
					d.pixels[rgbaOffset+3] = 255     // A
					rgbaOffset += 4
				}

				py += d.comp[0].stride
				pcb += d.comp[1].stride
				pcr += d.comp[2].stride
			}
		}
	} else if d.ncomp == 1 {
		// Grayscale to RGBA conversion
		rgbaOffset := 0
		yOffset := 0

		for y := 0; y < d.height; y++ {
			for x := 0; x < d.width; x++ {
				lum := d.comp[0].pixels[yOffset+x]

				d.pixels[rgbaOffset] = lum
				d.pixels[rgbaOffset+1] = lum
				d.pixels[rgbaOffset+2] = lum
				d.pixels[rgbaOffset+3] = 255
				rgbaOffset += 4
			}

			yOffset += d.comp[0].stride
		}
	}

	return nil
}

// transform applies rotation and flipping to the decoded RGBA image based on the EXIF orientation tag.
func (d *decoder) transform() {
	srcWidth, srcHeight := d.width, d.height
	src := d.pixels
	srcStride := srcWidth * 4

	dstWidth, dstHeight := srcWidth, srcHeight

	// Orientations 5-8 involve 90/270 degree rotations, swapping width and height.
	if d.orientation >= 5 {
		dstWidth, dstHeight = srcHeight, srcWidth
	}

	// Allocate a new buffer for the transformed image.
	// While some transformations (e.g., 180 rotation) can be done in-place,
	// rotations require a separate buffer if W!=H. For simplicity, we always use a new buffer.
	dst := make([]byte, dstWidth*dstHeight*4)
	dstStride := dstWidth * 4

	// Iterate over the source image dimensions (forward mapping).
	for sy := 0; sy < srcHeight; sy++ {
		for sx := 0; sx < srcWidth; sx++ {
			var dx, dy int

			// Map source coordinates (sx, sy) to destination coordinates (dx, dy).
			switch d.orientation {
			case 2: // Flip horizontal
				dx, dy = srcWidth-1-sx, sy
			case 3: // Rotate 180
				dx, dy = srcWidth-1-sx, srcHeight-1-sy
			case 4: // Flip vertical
				dx, dy = sx, srcHeight-1-sy
			case 5: // Transpose (Flip along TL-BR diagonal)
				dx, dy = sy, sx
			case 6: // Rotate 90 CW
				dx, dy = srcHeight-1-sy, sx
			case 7: // Transverse (Flip along TR-BL diagonal)
				dx, dy = srcHeight-1-sy, srcWidth-1-sx
			case 8: // Rotate 270 CW (90 CCW)
				dx, dy = sy, srcWidth-1-sx
			default:
				// Should not happen as we check orientation > 1 before calling transform.
				continue
			}

			srcOffset := sy*srcStride + sx*4
			dstOffset := dy*dstStride + dx*4

			// Copy RGBA pixel (4 bytes).
			copy(dst[dstOffset:dstOffset+4], src[srcOffset:srcOffset+4])
		}
	}

	// Update decoder state with the transformed image.
	d.pixels = dst
	d.width = dstWidth
	d.height = dstHeight
}

// decode reads the JPEG stream from a byte slice, parses all segments, decodes the scan data, and performs color conversion.
// If configOnly is true, it stops after reading the image metadata (SOF marker).
func (d *decoder) decode(jpegData []byte, configOnly bool) (image.Image, error) {
	d.jpegData = jpegData
	d.pos = 0
	d.size = len(jpegData)
	d.orientation = 1 // Default orientation (Top-Left)

	// Check for SOI (Start of Image) marker.
	if d.size < 2 || d.jpegData[0] != 0xFF || d.jpegData[1] != 0xD8 {
		return nil, ErrNoJPEG
	}

	if err := d.skip(2); err != nil {
		return nil, err
	}

	var sofDecoded bool

markerLoop:
	for {
		if d.size < 2 {
			break markerLoop
		}

		if d.jpegData[d.pos] != 0xFF {
			return nil, ErrSyntax
		}

		marker := d.jpegData[d.pos+1]
		if err := d.skip(2); err != nil {
			return nil, err
		}

		switch marker {
		case 0xC0: // SOF0 (Start of Frame, Baseline DCT)
			if err := d.decodeSOF(configOnly); err != nil {
				return nil, err
			}

			sofDecoded = true
			if configOnly {
				break markerLoop // Found config, we are done.
			}
		case 0xC4: // DHT (Define Huffman Table)
			if err := d.decodeDHT(); err != nil {
				return nil, err
			}
		case 0xDB: // DQT (Define Quantization Table)
			if err := d.decodeDQT(); err != nil {
				return nil, err
			}
		case 0xDD: // DRI (Define Restart Interval)
			if err := d.decodeDRI(); err != nil {
				return nil, err
			}
		case 0xDA: // SOS (Start of Scan)
			if !sofDecoded {
				return nil, ErrSyntax // Scan data found before SOF.
			}

			if err := d.decodeScan(); err != nil {
				// Check if the error was actually a valid EOI marker.
				// This can happen if the entropy decoder reads ahead and hits an EOI marker,
				// potentially causing a syntax error (e.g., invalid Huffman code) caught by panic/recover.
				if errors.Is(err, ErrSyntax) && d.size >= 0 && d.pos < len(d.jpegData)-1 && d.jpegData[d.pos] == 0xFF && d.jpegData[d.pos+1] == 0xD9 {
					break markerLoop
				}

				return nil, err
			}

			break markerLoop
		case 0xFE: // COM (Comment)
			if err := d.skipMarker(); err != nil {
				return nil, err
			}
		case 0xD9: // EOI (End of Image)
			break markerLoop
		default:
			if marker >= 0xE0 && marker <= 0xEF { // APPn markers
				if marker == 0xE1 { // APP1 (EXIF)
					if err := d.decodeAPP1(); err != nil {
						// Errors in APP1 structure (e.g., bad length) are fatal.
						return nil, err
					}
				} else if marker == 0xEE { // APP14 (Adobe)
					if err := d.decodeAPP14(); err != nil {
						return nil, err
					}
				} else { // Other APPn markers, e.g., EXIF, JFIF
					if err := d.skipMarker(); err != nil {
						return nil, err
					}
				}
			} else if marker >= 0xD0 && marker <= 0xD7 {
				// RSTn (Restart markers). Should be handled within the scan, but we ignore them here.
			} else {
				// Unsupported markers include SOF2 (0xC2) for progressive JPEGs.
				return nil, ErrUnsupported
			}
		}
	}

	if !sofDecoded {
		return nil, ErrSyntax // No image configuration found.
	}

	if configOnly {
		return nil, nil // Success for config-only path.
	}

	// Determine if we need to convert to RGBA.
	// This is needed if requested (toRGBA), if the source is RGB, or if we need to rotate (autoRotate && orientation > 1).
	needsRotation := d.autoRotate && d.orientation > 1
	needsRGBA := d.toRGBA || d.isRGB || needsRotation

	if needsRGBA {
		// Ensure the RGBA buffer (d.pixels) is allocated. It should have been allocated in decodeSOF
		// if (d.toRGBA || d.isRGB || d.autoRotate) was true.
		if d.pixels == nil && (d.width > 0 && d.height > 0) {
			// This case should theoretically not happen with the current logic.
			return nil, ErrInternal
		}

		if err := d.convert(); err != nil {
			return nil, err
		}

		// Apply rotation/flip if needed.
		if needsRotation {
			// This updates d.pixels, d.width, and d.height.
			d.transform()
		}

		return &image.RGBA{
			Pix:    d.pixels,
			Stride: d.width * 4,
			Rect:   image.Rect(0, 0, d.width, d.height),
		}, nil
	}

	// Native YCbCr/Gray output (no rotation applied)
	rect := image.Rect(0, 0, d.width, d.height)
	switch d.ncomp {
	case 1:
		return &image.Gray{
			Pix:    d.comp[0].pixels,
			Stride: d.comp[0].stride,
			Rect:   rect,
		}, nil
	case 3:
		return &image.YCbCr{
			Y:              d.comp[0].pixels,
			Cb:             d.comp[1].pixels,
			Cr:             d.comp[2].pixels,
			YStride:        d.comp[0].stride,
			CStride:        d.comp[1].stride, // Cb and Cr strides are the same.
			SubsampleRatio: d.subsampleRatio,
			Rect:           rect,
		}, nil
	default:
		// This path should not be reached with valid JPEGs.
		return nil, ErrInternal
	}
}
