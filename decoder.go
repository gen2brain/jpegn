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
	id                 int     // Component identifier (e.g., 1 for Y, 2 for Cb, 3 for Cr).
	ssX, ssY           int     // Subsampling factors for X and Y axes.
	width, height      int     // Dimensions of this component in pixels.
	stride             int     // The number of bytes from one row of pixels to the next.
	qtSel              int     // Quantization table selector.
	acTabSel, dcTabSel int     // Huffman table selectors for AC and DC coefficients.
	dcPred             int     // DC prediction value for differential coding.
	pixels             []byte  // Decoded pixel data for this component (used in baseline).
	coeffs             []int32 // Stores DCT coefficients for progressive mode (zigzag order).
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
	qtab              [4]*[64]uint8             // Pointers for pooling. Stored in zigzag order.
	dcVlcTab          [4]*[65536]vlcCode        // DC Huffman tables (indices 0-3)
	acVlcTab          [4]*[65536]vlcCode        // AC Huffman tables (indices 0-3)
	buf               uint64                    // Use uint64 for fewer refills.
	bufBits           int                       // Number of valid bits in the bit buffer.
	markerHit         bool                      // True if a marker was hit during bit reading.
	block             [64]int32                 // Temporary storage for a single 8x8 block of DCT coefficients (natural order).
	rstInterval       int                       // Restart interval in MCUs, for error resilience.
	pixels            []byte                    // Final decoded pixel data (RGBA buffer).
	subsampleRatio    image.YCbCrSubsampleRatio // The detected YCbCr subsampling ratio.
	isRGB             bool                      // True if the image is encoded as RGB instead of YCbCr.
	isBaseline        bool                      // True if the image is a baseline JPEG.
	isProgressive     bool                      // True if the image is a progressive JPEG.
	upsampleMethod    UpsampleMethod            // The upsampling method to use.
	toRGBA            bool                      // Whether to convert the final image to RGBA.
	autoRotate        bool                      // Whether to auto-rotate based on EXIF orientation.
	orientation       int                       // EXIF orientation tag (1-8).

	// Progressive scan state
	eobRun int // End-of-Block run counter for progressive AC scans.
}

// errDecode is used for internal panics during the hot decoding path.
type errDecode struct{ error }

// zz is the zigzag ordering table. It maps the 1D order of coefficients in the JPEG stream (zigzag index)
// to their 2D position (natural index) in an 8x8 block.
var zz = [64]int{
	0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18,
	11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
	42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
	38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
}

// clamp clamps an int32 value to the valid 8-bit pixel range [0, 255].
func clamp(x int32) byte {
	return byte(min(max(x, 0), 255))
}

// newDecoder creates a new decoder instance and allocates the large tables.
func newDecoder() *decoder {
	d := new(decoder)
	for i := 0; i < 4; i++ {
		d.qtab[i] = new([64]uint8)
		// Initialize separate DC and AC Huffman tables.
		d.dcVlcTab[i] = new([65536]vlcCode)
		d.acVlcTab[i] = new([65536]vlcCode)
	}

	return d
}

// panic triggers an internal panic to signal a decoding error in the hot path.
func (d *decoder) panic(err error) {
	panic(errDecode{err})
}

// reset clears the decoder state for reuse, preserving the allocated tables.
func (d *decoder) reset() {
	// Save pointers to the tables.
	dcVlcTmp := d.dcVlcTab
	acVlcTmp := d.acVlcTab
	qtabTmp := d.qtab

	// Zero the struct. This clears references (jpegData, pixels, etc.) allowing GC, and resets all state variables.
	*d = decoder{}

	// Restore pointers to the tables.
	d.dcVlcTab = dcVlcTmp
	d.acVlcTab = acVlcTmp
	d.qtab = qtabTmp
}

// Bitstream handling

// showBits reads 'bits' number of bits from the bitstream without consuming them.
// It ensures the internal buffer `d.buf` has enough data, reading from the main
// jpegData slice if necessary. It also handles JPEG byte stuffing (0xFF00).
func (d *decoder) showBits(bits int) int {
	if bits == 0 {
		return 0
	}

	// Reset the marker hit flag at the start of a new read operation.
	d.markerHit = false

	// Loop to fill the buffer until we have enough bits or hit a marker.
fillLoop:
	for d.bufBits < bits {
		if d.size <= 0 {
			// If we run out of data (EOF), we must stop filling the buffer.
			break fillLoop
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
					// It's a marker (including RSTn). Rewind so the main loop can see the 0xFF marker.
					// Crucially, revert the bit count and DO NOT add 0xFF to the buffer.
					d.pos--
					d.size++
					d.bufBits -= 8

					d.markerHit = true // Signal that we stopped due to a marker.
					break fillLoop     // Stop filling the buffer.
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
		// This case handles an underfull buffer (e.g., after hitting a marker or EOF).
		res = d.buf << (-shift)
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
	// Fast path: we already have enough bits in the buffer and there was no marker-driven underflow.
	// This avoids calling showBits (and its marker reset logic) in the common case.
	if bits > 0 && d.bufBits >= bits && !d.markerHit {
		shift := d.bufBits - bits
		res := int((d.buf >> shift) & ((1 << bits) - 1))
		d.bufBits = shift

		return res
	}

	// Slow path: ensure enough bits (and check marker semantics).
	res := d.showBits(bits)

	// If raw bits reading (used for refinement, sign bits, EOB run lengths)
	// hits a marker, the result might be padded.
	// Go stdlib behavior is to stop the scan immediately when a marker is hit
	// during raw bit reading (unlike EOF, where it allows 0-padding).
	if d.markerHit && d.bufBits < bits {
		d.panic(ErrSyntax)
	}

	d.skipBits(bits)

	return res
}

// getBit reads a single bit from the bitstream. Used for successive approximation.
func (d *decoder) getBit() int {
	// Fast path for the most common case in refinement scans.
	if d.bufBits > 0 && !d.markerHit {
		d.bufBits--

		return int((d.buf >> d.bufBits) & 1)
	}

	// Fallback (handles refill and marker semantics).
	return d.getBits(1)
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

	// Calculate component dimensions and allocate memory for pixels or coefficients.
	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		c.width = (d.width*c.ssX + ssxMax - 1) / ssxMax
		c.height = (d.height*c.ssY + ssyMax - 1) / ssyMax
		c.stride = d.mbWidth * c.ssX << 3

		if ((c.width < 3) && (c.ssX != ssxMax)) || ((c.height < 3) && (c.ssY != ssyMax)) {
			return ErrUnsupported // Smoothing is not supported.
		}

		if !configOnly {
			if d.isProgressive {
				// Progressive mode: Allocate coefficient buffer.
				nBlocksX := d.mbWidth * c.ssX
				nBlocksY := d.mbHeight * c.ssY
				// 64 coefficients (int32) per block.
				coeffSize := nBlocksX * nBlocksY * 64

				if coeffSize <= 0 && (d.width > 0 && d.height > 0) {
					return ErrOutOfMemory
				}

				if coeffSize > 0 {
					// initialize to zero, required for progressive decoding.
					c.coeffs = make([]int32, coeffSize)
				}

				// c.pixels will be allocated in postProcessProgressive.
			} else {
				// Baseline mode: Allocate pixel buffer.
				pixelSize := c.stride * d.mbHeight * c.ssY << 3
				if pixelSize <= 0 && (d.width > 0 && d.height > 0) {
					return ErrOutOfMemory
				}

				if pixelSize > 0 {
					c.pixels = make([]byte, pixelSize)
				}
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
		infoByte := int(d.jpegData[d.pos])

		// Parse Tc (Table Class) and Th (Table Destination Identifier).
		tc := infoByte >> 4 // 0=DC, 1=AC
		th := infoByte & 0x0F

		if tc > 1 {
			return ErrSyntax
		}

		// Baseline only allows th=0 or 1, but progressive allows up to 3.
		// We support up to 3 for both modes for simplicity.
		if th > 3 {
			return ErrUnsupported
		}

		// Select the correct VLC table (DC or AC).
		var vlc *[65536]vlcCode
		if tc == 0 {
			vlc = d.dcVlcTab[th]
		} else {
			vlc = d.acVlcTab[th]
		}

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

		// Check precision (Pq). Must be 8-bit (Pq=0).
		if (i >> 4) != 0 {
			return ErrUnsupported // 16-bit quantization tables not supported.
		}

		i &= 0x0F // Tq (Table destination identifier)

		if i > 3 {
			return ErrSyntax
		}

		d.qtAvail |= 1 << i
		t := d.qtab[i]

		// Pooling: Clear the table before filling it.
		*t = [64]uint8{}

		for j := 0; j < 64; j++ {
			// Read value in zigzag order from the stream (j).
			val := d.jpegData[d.pos+j+1]
			// Store in natural order (zz[j]) for faster access during dequantization.
			t[zz[j]] = val
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
			rgbToRGBA(&d.comp[0], &d.comp[1], &d.comp[2], d.pixels, d.width, d.height)
		} else {
			// YCbCr to RGBA conversion
			yCbCrToRGBA(&d.comp[0], &d.comp[1], &d.comp[2], d.pixels, d.width, d.height)
		}
	} else if d.ncomp == 1 {
		// Grayscale to RGBA conversion
		grayToRGBA(&d.comp[0], d.pixels, d.width, d.height)
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
	var scansCompleted int

markerLoop:
	for {
		// Skip any stray non-marker bytes.
		for d.size > 0 && d.jpegData[d.pos] != 0xFF {
			// Skip forward until we find the next 0xFF marker introducer or run out of data.
			d.pos++
			d.size--
		}

		if d.size < 2 {
			break markerLoop
		}

		// At this point we expect a marker introducer 0xFF.
		if d.jpegData[d.pos] != 0xFF {
			// If we still can't find a marker, treat as truncated stream.
			if d.isProgressive && scansCompleted > 0 {
				// Allow truncated progressive image (best effort), but only
				// after at least one scan so we don't accept garbage as a JPEG.
				break markerLoop
			}

			return nil, ErrSyntax
		}

		// Collapse any fill bytes 0xFF,0xFF,... before the actual marker code.
		// (The prior code handled a single 0xFF padding step; now we loop.)
		for d.size >= 2 && d.jpegData[d.pos+1] == 0xFF {
			if err := d.skip(1); err != nil {
				return nil, err
			}
		}

		if d.size < 2 {
			break markerLoop
		}

		marker := d.jpegData[d.pos+1]

		// Consume the 0xFF + marker byte pair.
		if err := d.skip(2); err != nil {
			return nil, err
		}

		switch marker {
		case 0xC0: // SOF0 (Start of Frame, Baseline DCT)
			if sofDecoded {
				return nil, ErrSyntax
			}

			d.isBaseline = true
			d.isProgressive = false

			if err := d.decodeSOF(configOnly); err != nil {
				return nil, err
			}
			sofDecoded = true

			if configOnly {
				break markerLoop
			}
		case 0xC2: // SOF2 (Start of Frame, Progressive DCT)
			if sofDecoded {
				return nil, ErrSyntax
			}

			d.isProgressive = true
			d.isBaseline = false

			if err := d.decodeSOF(configOnly); err != nil {
				return nil, err
			}
			sofDecoded = true

			if configOnly {
				break markerLoop
			}
		case 0xC4: // DHT
			if err := d.decodeDHT(); err != nil {
				return nil, err
			}
		case 0xDB: // DQT
			if err := d.decodeDQT(); err != nil {
				return nil, err
			}
		case 0xDD: // DRI
			if err := d.decodeDRI(); err != nil {
				return nil, err
			}
		case 0xDA: // SOS
			if !sofDecoded {
				return nil, ErrSyntax
			}

			if err := d.decodeScan(); err != nil {
				// We now do a best-effort early exit if:
				//   * We've already completed at least one scan, AND
				//   * The very next two bytes are exactly an EOI marker.
				// Otherwise, we propagate the error so truly malformed streams still fail fast.
				if errors.Is(err, ErrSyntax) &&
					scansCompleted > 0 &&
					d.size >= 2 &&
					d.jpegData[d.pos] == 0xFF &&
					d.jpegData[d.pos+1] == 0xD9 {

					// Treat as graceful end (truncated entropy near EOI).
					break markerLoop
				}

				return nil, err
			}

			scansCompleted++

			if d.isBaseline {
				break markerLoop
			}
			// Progressive: continue accumulating scans.
		case 0xFE: // COM
			if err := d.skipMarker(); err != nil {
				return nil, err
			}
		case 0xD9: // EOI
			break markerLoop
		default:
			if marker >= 0xE0 && marker <= 0xEF { // APPn
				if marker == 0xE1 { // APP1 (EXIF)
					if err := d.decodeAPP1(); err != nil {
						return nil, err
					}
				} else if marker == 0xEE { // APP14 (Adobe)
					if err := d.decodeAPP14(); err != nil {
						return nil, err
					}
				} else {
					if err := d.skipMarker(); err != nil {
						return nil, err
					}
				}
			} else if marker >= 0xD0 && marker <= 0xD7 {
				// RSTn: should be handled inside scan entropy decoding.
			} else {
				return nil, ErrUnsupported
			}
		}

		if d.size <= 0 {
			// End of data: allow truncated progressive images only after at least one scan.
			if d.isProgressive && scansCompleted > 0 {
				break markerLoop
			}

			return nil, ErrSyntax
		}
	}

	if !sofDecoded {
		return nil, ErrSyntax
	}

	if configOnly {
		return nil, nil
	}

	if scansCompleted == 0 {
		return nil, ErrSyntax
	}

	// Post-process progressive coefficients (dequantize + IDCT).
	if d.isProgressive {
		if err := d.postProcessProgressive(); err != nil {
			return nil, err
		}
	}

	needsRotation := d.autoRotate && d.orientation > 1
	needsRGBA := d.toRGBA || d.isRGB || needsRotation

	if needsRGBA {
		if d.pixels == nil && (d.width > 0 && d.height > 0) {
			return nil, ErrInternal
		}

		if err := d.convert(); err != nil {
			return nil, err
		}

		if needsRotation {
			d.transform()
		}

		return &image.RGBA{
			Pix:    d.pixels,
			Stride: d.width * 4,
			Rect:   image.Rect(0, 0, d.width, d.height),
		}, nil
	}

	// Native output
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
			CStride:        d.comp[1].stride,
			SubsampleRatio: d.subsampleRatio,
			Rect:           rect,
		}, nil
	default:
		return nil, ErrInternal
	}
}
