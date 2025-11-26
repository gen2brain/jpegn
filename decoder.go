package jpegn

import (
	"bytes"
	"errors"
	"image"
	"io"
)

// vlcCode represents a single entry in the pre-calculated Huffman lookup table.
// It stores the number of bits for the code and the decoded value.
type vlcCode struct {
	bits, code uint8
}

// vlcCode8 is an 8-bit lookup table entry (like stdlib's approach)
// High 8 bits: decoded value
// Low 8 bits: code length + 1 (or 0 if code longer than 8 bits)
type vlcCode8 uint16

// component stores information about a single color component (e.g., Y, Cb, or Cr).
type component struct {
	id                 int     // Component identifier (e.g., 1 for Y, 2 for Cb, 3 for Cr).
	ssX, ssY           int     // Subsampling factors for X and Y axes.
	width, height      int     // Dimensions of this component in pixels.
	nBlocksX, nBlocksY int     // Dimensions of this component in blocks.
	stride             int     // The number of bytes from one row of pixels to the next.
	qtSel              int     // Quantization table selector.
	acTabSel, dcTabSel int     // Huffman table selectors for AC and DC coefficients.
	dcPred             int     // DC prediction value for differential coding.
	pixels             []byte  // Decoded pixel data for this component (used in baseline).
	coeffs             []int32 // Stores DCT coefficients for progressive mode (natural order).
}

// decoder holds the state of the JPEG decoding process.
type decoder struct {
	jpegData            []byte                    // Input buffer containing the entire JPEG file.
	pos                 int                       // Current position index in the input buffer.
	size                int                       // Remaining bytes to be processed.
	length              int                       // Length of the current marker segment.
	width, height       int                       // Dimensions of the final image.
	mbWidth, mbHeight   int                       // Dimensions of the image in MCU (Minimum Coded Unit) blocks.
	mbSizeX, mbSizeY    int                       // Dimensions of a single MCU in pixels.
	ncomp               int                       // Number of color components (1 for grayscale, 3 for color, 4 for CMYK).
	comp                [4]component              // Array to hold data for each color component.
	qtUsed, qtAvail     int                       // Bitmasks tracking used and available quantization tables.
	qtab                [4]*[64]uint8             // Pointers for pooling. Stored in zigzag order.
	dcVlcTab            [4]*[65536]vlcCode        // DC Huffman tables (indices 0-3) - 16-bit lookup
	acVlcTab            [4]*[65536]vlcCode        // AC Huffman tables (indices 0-3) - 16-bit lookup
	dcVlcTab8           [4]*[256]vlcCode8         // DC Huffman tables - 8-bit lookup (fast path)
	acVlcTab8           [4]*[256]vlcCode8         // AC Huffman tables - 8-bit lookup (fast path)
	buf                 uint64                    // Use uint64 for fewer refills.
	bufBits             int                       // Number of valid bits in the bit buffer.
	markerHit           bool                      // True if a marker was hit during bit reading.
	eobRun              int                       // Global End-of-Block run counter (progressive AC).
	block               [64]int32                 // Temporary storage for a single 8x8 block of DCT coefficients (natural order).
	rstInterval         int                       // Restart interval in MCUs, for error resilience.
	pixels              []byte                    // Final decoded pixel data (RGBA buffer).
	subsampleRatio      image.YCbCrSubsampleRatio // The detected YCbCr subsampling ratio.
	isRGB               bool                      // True if the image is encoded as RGB instead of YCbCr.
	isBaseline          bool                      // True if the image is a baseline JPEG.
	isProgressive       bool                      // True if the image is a progressive JPEG.
	upsampleMethod      UpsampleMethod            // The upsampling method to use.
	toRGBA              bool                      // Whether to convert the final image to RGBA.
	autoRotate          bool                      // Whether to auto-rotate based on EXIF orientation.
	orientation         int                       // EXIF orientation tag (1-8).
	adobeTransformValid bool                      // True if APP14 Adobe marker was found.
	adobeTransform      uint8                     // Adobe color transform: 0=Unknown(RGB/CMYK), 1=YCbCr, 2=YCCK.
	scaleDenom          int                       // IDCT scaling denominator: 1 (no scaling), 2 (1/2), 4 (1/4), 8 (1/8).

	acTouched []bool // reused per scan for stats
}

// errDecode is used for internal panics during the hot decoding path.
type errDecode struct{ error }

// Exif contains metadata extracted from a JPEG image's EXIF data.
type Exif struct {
	// Basic image info
	Orientation int // EXIF orientation (1-8). 1 = normal, values 2-8 indicate rotation/flip.
	Width       int // Image width in pixels (may differ from actual JPEG dimensions if rotated).
	Height      int // Image height in pixels.

	// Camera info
	Make     string // Camera manufacturer (e.g., "Canon").
	Model    string // Camera model (e.g., "Canon EOS 5D Mark III").
	Software string // Software used to process/create the image.

	// Date/Time (format: "YYYY:MM:DD HH:MM:SS")
	DateTime         string // File modification date/time.
	DateTimeOriginal string // Original capture date/time (when photo was taken).

	// Exposure settings
	ExposureTime float64 // Shutter speed in seconds (e.g., 0.004 = 1/250s).
	FNumber      float64 // Aperture f-number (e.g., 5.6 = f/5.6).
	ISOSpeed     int     // ISO speed rating (e.g., 800).
	FocalLength  float64 // Lens focal length in millimeters.
	Flash        int     // Flash mode/status (0 = no flash, non-zero = flash fired).

	// GPS location
	GPSLatitude  float64 // Latitude in decimal degrees (positive = North, negative = South).
	GPSLongitude float64 // Longitude in decimal degrees (positive = East, negative = West).
	GPSAltitude  float64 // Altitude in meters above sea level.

	// Copyright/Author
	Copyright string // Copyright notice.
	Artist    string // Creator/photographer name.
}

// zz is the zigzag ordering table. It maps the 1D order of coefficients in the JPEG stream (zigzag index)
// to their 2D position (natural index) in an 8x8 block.
var zz = [64]int{
	0, 1, 8, 16, 9, 2, 3, 10,
	17, 24, 32, 25, 18, 11, 4, 5,
	12, 19, 26, 33, 40, 48, 41, 34,
	27, 20, 13, 6, 7, 14, 21, 28,
	35, 42, 49, 56, 57, 50, 43, 36,
	29, 22, 15, 23, 30, 37, 44, 51,
	58, 59, 52, 45, 38, 31, 39, 46,
	53, 60, 61, 54, 47, 55, 62, 63,
}

// Default Huffman tables (JPEG standard Annex K).

// Table K.3: DC Luminance (Class 0, Index 0)
var defaultDCLumaCounts = [16]uint8{
	0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
}
var defaultDCLumaValues = []byte{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
}

// Table K.4: DC Chrominance (Class 0, Index 1)
var defaultDCChromaCounts = [16]uint8{
	0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
}
var defaultDCChromaValues = []byte{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
}

// Table K.5: AC Luminance (Class 1, Index 0)
var defaultACLumaCounts = [16]uint8{
	0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 125,
}
var defaultACLumaValues = []byte{
	0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
	0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
	0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
	0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
	0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
	0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa,
}

// Table K.6: AC Chrominance (Class 1, Index 1)
var defaultACChromaCounts = [16]uint8{
	0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 119,
}
var defaultACChromaValues = []byte{
	0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
	0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
	0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34, 0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
	0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
	0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
	0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
	0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
	0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
	0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa,
}

// Pre-built default VLC tables.
var (
	defaultDCLumaVLC   [65536]vlcCode
	defaultDCChromaVLC [65536]vlcCode
	defaultACLumaVLC   [65536]vlcCode
	defaultACChromaVLC [65536]vlcCode

	// 8-bit lookup tables (fast path, like stdlib)
	defaultDCLumaVLC8   [256]vlcCode8
	defaultDCChromaVLC8 [256]vlcCode8
	defaultACLumaVLC8   [256]vlcCode8
	defaultACChromaVLC8 [256]vlcCode8
)

func init() {
	// Build default VLC tables once at startup.
	_ = buildVlcTable(&defaultDCLumaVLC, &defaultDCLumaCounts, defaultDCLumaValues)
	_ = buildVlcTable(&defaultDCChromaVLC, &defaultDCChromaCounts, defaultDCChromaValues)
	_ = buildVlcTable(&defaultACLumaVLC, &defaultACLumaCounts, defaultACLumaValues)
	_ = buildVlcTable(&defaultACChromaVLC, &defaultACChromaCounts, defaultACChromaValues)

	// Build 8-bit lookup tables (fast path)
	_ = buildVlcTable8(&defaultDCLumaVLC8, &defaultDCLumaCounts, defaultDCLumaValues)
	_ = buildVlcTable8(&defaultDCChromaVLC8, &defaultDCChromaCounts, defaultDCChromaValues)
	_ = buildVlcTable8(&defaultACLumaVLC8, &defaultACLumaCounts, defaultACLumaValues)
	_ = buildVlcTable8(&defaultACChromaVLC8, &defaultACChromaCounts, defaultACChromaValues)
}

// clamp clamps an int32 value to the valid 8-bit pixel range [0, 255].
func clamp(x int32) byte {
	return byte(min(max(x, 0), 255))
}

// buildVlcTable builds the fast lookup table from Huffman table definitions (COUNTS and HUFFVAL).
func buildVlcTable(vlc *[65536]vlcCode, counts *[16]uint8, values []byte) error {
	// Clear the table before filling it.
	for i := range vlc {
		vlc[i] = vlcCode{}
	}

	// Validate counts and calculate the total number of symbols.
	nSymbols := 0
	for _, c := range counts {
		nSymbols += int(c)
	}

	if nSymbols > 256 {
		return ErrSyntax
	}
	// Allow values slice to be longer than nSymbols, only use the first nSymbols elements.
	if len(values) < nSymbols {
		return ErrSyntax
	}

	// Structure to hold generated codes temporarily.
	type symbolCode struct {
		code   uint16
		length uint8
		value  uint8
	}

	// We allocate space for all symbols present.
	symbols := make([]symbolCode, nSymbols)

	// Generate canonical Huffman codes.
	k := 0
	// Use uint32 for code generation to prevent overflow when shifting at L15 (code <<= 1).
	// The standard algorithm requires tracking codes up to 2^16.
	code := uint32(0)
	valueIdx := 0

	for length := 1; length <= 16; length++ {
		count := int(counts[length-1])
		for i := 0; i < count; i++ {
			if k >= nSymbols {
				// Should be caught by initial validation.
				return ErrSyntax
			}

			// Ensure the generated code fits in 16 bits before assignment.
			// This check prevents assigning 65536 (which wraps to 0 in uint16) if the table is full.
			if code > 0xFFFF {
				// This should be caught by Kraft's inequality check below if counts are valid.
				return ErrSyntax
			}

			symbols[k].length = uint8(length)
			symbols[k].code = uint16(code)
			symbols[k].value = values[valueIdx]

			k++
			code++
			valueIdx++
		}

		// Check Kraft's inequality (ensures codes do not overflow the space).
		if code > (uint32(1) << length) {
			return ErrSyntax
		}

		// If the tree is full at this level, stop if all symbols consumed.
		if code == (uint32(1) << length) {
			if k == nSymbols {
				break
			}
			// If tree is full but symbols remain for longer lengths, it's an oversubscribed table.
			// If L=16 and tree is full, we must ensure k == nSymbols.
			if length == 16 && k < nSymbols {
				return ErrSyntax
			}
		}

		// Prepare starting code for the next length.
		// If length < 16, shift code. If length == 16, loop terminates.
		if length < 16 {
			code <<= 1
		}
	}

	// Populate the 16-bit lookup table (LUT).
	for i := 0; i < nSymbols; i++ {
		s := symbols[i]
		codeLen := s.length
		huffCode := s.code
		huffVal := s.value

		if codeLen == 0 {
			continue // Should not happen based on generation logic.
		}

		// Calculate LUT indices (MSB-first alignment).
		shift := 16 - int(codeLen)
		numEntries := 1 << shift
		baseIndex := uint32(huffCode) << shift

		// Fill the LUT entries.
		for j := uint32(0); j < uint32(numEntries); j++ {
			index := baseIndex + j
			if index < 65536 {
				// Collision check is unnecessary if counts are valid and generation is correct.
				vlc[index].bits = codeLen
				vlc[index].code = huffVal
			}
		}
	}

	return nil
}

// buildVlcTable8 builds an 8-bit Huffman lookup table (like stdlib's approach).
// This is faster for progressive JPEG because it only requires 8 bits in the buffer.
// High 8 bits of uint16: decoded value
// Low 8 bits of uint16: code length + 1 (or 0 if code is longer than 8 bits)
func buildVlcTable8(vlc *[256]vlcCode8, counts *[16]uint8, values []byte) error {
	// Clear the table
	for i := range vlc {
		vlc[i] = 0
	}

	// Validate counts
	nSymbols := 0
	for _, c := range counts {
		nSymbols += int(c)
	}

	if nSymbols > 256 || len(values) < nSymbols {
		return ErrSyntax
	}

	// Generate canonical Huffman codes
	code := uint32(0)
	valueIdx := 0

	for length := 1; length <= 8; length++ { // Only handle codes up to 8 bits
		count := int(counts[length-1])
		for i := 0; i < count; i++ {
			if valueIdx >= nSymbols {
				return ErrSyntax
			}

			// Calculate the 8-bit index range for this code
			// Codes are MSB-aligned, so a 3-bit code like "101" becomes "10100000"
			base := uint8(code << (8 - length))
			lutValue := vlcCode8(uint16(values[valueIdx])<<8 | uint16(length+1))

			// Fill all matching 8-bit prefixes
			numEntries := 1 << (8 - length)
			for k := 0; k < numEntries; k++ {
				vlc[uint8(base)|uint8(k)] = lutValue
			}

			code++
			valueIdx++
		}

		if code > (1 << length) {
			return ErrSyntax
		}

		if length < 8 {
			code <<= 1
		}
	}

	return nil
}

// newDecoder creates a new decoder instance and allocates the large tables.
func newDecoder() *decoder {
	d := new(decoder)
	for i := 0; i < 4; i++ {
		d.qtab[i] = new([64]uint8)
		// Initialize separate DC and AC Huffman tables.
		d.dcVlcTab[i] = new([65536]vlcCode)
		d.acVlcTab[i] = new([65536]vlcCode)
		d.dcVlcTab8[i] = new([256]vlcCode8)
		d.acVlcTab8[i] = new([256]vlcCode8)
	}

	// Initialize default Huffman tables by copying from pre-built globals.
	*d.dcVlcTab[0] = defaultDCLumaVLC
	*d.dcVlcTab[1] = defaultDCChromaVLC
	*d.acVlcTab[0] = defaultACLumaVLC
	*d.acVlcTab[1] = defaultACChromaVLC
	*d.dcVlcTab8[0] = defaultDCLumaVLC8
	*d.dcVlcTab8[1] = defaultDCChromaVLC8
	*d.acVlcTab8[0] = defaultACLumaVLC8
	*d.acVlcTab8[1] = defaultACChromaVLC8

	return d
}

// panic triggers an internal panic to signal a decoding error in the hot path.
func (d *decoder) panic(err error) {
	panic(errDecode{err})
}

// processRestart handles the detection and processing of restart markers (RSTn) at restart interval boundaries during a scan.
// Returns true if the scan should continue, false if it should terminate (due to error or EOF).
func (d *decoder) processRestart(nextRst *int, rstCount *int, ah int, nCompScan int, scanComp [4]int) bool {
	// This function is called when rstCount reaches 0.

	// Align the bitstream to byte boundary (discard padding bits 0-7).
	d.byteAlign()

	// Calculate number of full bytes buffered.
	// d.byteAlign() ensures d.bufBits is a multiple of 8.
	bufferedBytes := d.bufBits / 8

	// Rewind d.pos by the number of buffered bytes.
	if bufferedBytes > 0 {
		d.pos -= bufferedBytes
		d.size += bufferedBytes
	}

	// Discard the buffer content now that d.pos is synchronized.
	d.buf = 0
	d.bufBits = 0

	// Search for RST marker in the byte stream, skipping 0xFF fill bytes.
	found := false

	for d.size > 0 {
		b := d.jpegData[d.pos]
		if b != 0xFF {
			// Not 0xFF. The RST marker is missing.
			// Important: Per JPEG standard, RST markers should be present at regular intervals.
			// If missing, the stream may be corrupted.

			// Don't try to continue with corrupted state - terminate the scan
			d.markerHit = true

			return false
		}

		// Found 0xFF. Check next byte.
		if d.size < 2 {
			// EOF reached (lone 0xFF). Terminate scan.
			d.markerHit = true

			return false
		}

		b2 := d.jpegData[d.pos+1]

		// Check for RST marker (0xD0-0xD7).
		if (b2 & 0xF8) == 0xD0 {
			// Found RST marker.
			foundRst := int(b2 & 0x07)
			if foundRst == *nextRst {
				// Correct RST marker. Consume it.
				d.pos += 2
				d.size -= 2
				*nextRst = (*nextRst + 1) & 7
				found = true

				break
			}

			// Incorrect RST sequence. Handle like libjpeg, accept it and update expectation.

			d.pos += 2
			d.size -= 2
			*nextRst = (foundRst + 1) & 7
			found = true

			break
		}

		if b2 == 0x00 {
			// Stuffed byte 0xFF00 found where a marker was expected.
			// This indicates the RST is missing; the stream has entropy-coded data instead.

			// Don't try to continue
			d.markerHit = true

			return false
		}

		if b2 != 0xFF {
			// Other marker found (e.g., EOI, SOS). Terminate current scan.
			d.markerHit = true

			return false
		}

		// 0xFFFF - fill byte. Consume the first 0xFF and continue searching.
		d.pos++
		d.size--
	}

	if d.size == 0 && !found {
		// EOF reached without finding the marker. Terminate scan.
		d.markerHit = true

		return false
	}

	if found {
		// Reset state.
		*rstCount = d.rstInterval
		d.eobRun = 0        // Reset EOB run (required for AC scans, harmless for DC).
		d.markerHit = false // We are synchronized and continuing.

		// Reset DC predictors if first pass (Ah=0).
		if ah == 0 {
			for i := 0; i < nCompScan; i++ {
				d.comp[scanComp[i]].dcPred = 0
			}
		}

		return true
	}

	// RST marker not found where expected - this is an error condition
	// Per JPEG standard, when restart interval is set, markers MUST be present

	d.markerHit = true

	return false
}

// resetForConfig clears the decoder state for config-only decoding.
// This skips expensive VLC table initialization since config-only decoding doesn't need Huffman tables.
func (d *decoder) resetForConfig() {
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

	// Clear the quantization tables to prevent state leakage between decodes.
	for i := range d.qtab {
		*d.qtab[i] = [64]uint8{}
	}

	// Since tables are pooled and might have been overwritten by a previous DHT,
	// we must restore the defaults here for the next decode operation.
	*d.dcVlcTab[0] = defaultDCLumaVLC
	*d.dcVlcTab[1] = defaultDCChromaVLC
	*d.acVlcTab[0] = defaultACLumaVLC
	*d.acVlcTab[1] = defaultACChromaVLC

	// Clear non-default tables (Index 2 and 3) for safety.
	for i := range d.dcVlcTab[2] {
		d.dcVlcTab[2][i] = vlcCode{}
	}
	for i := range d.dcVlcTab[3] {
		d.dcVlcTab[3][i] = vlcCode{}
	}
	for i := range d.acVlcTab[2] {
		d.acVlcTab[2][i] = vlcCode{}
	}
	for i := range d.acVlcTab[3] {
		d.acVlcTab[3][i] = vlcCode{}
	}
}

// alignAndRewind aligns the bitstream and synchronizes d.pos with the buffer.
// It ensures that after returning, d.pos points to the start of the next marker segment
// (or EOF), and the bit buffer is cleared, ready for the next scan or marker processing.
func (d *decoder) alignAndRewind() {
	// Marker hit during scan (d.markerHit=true).
	if d.markerHit {
		// showBits guarantees d.pos points at the marker (0xFF) when it sets markerHit.
		// Discard buffer.
		d.buf = 0
		d.bufBits = 0
		d.markerHit = false
		return
	}

	// Scan finished naturally (all MCUs processed) or EOF during scan.
	// We need to find the next marker boundary or confirm EOF.

	// Byte align the stream (discard padding bits within the last byte).
	// This is generally required by the standard before searching for the next marker following an ECS.
	d.byteAlign()

	// We actively consume the stream until a marker is found or EOF is reached.
	for !d.markerHit {
		// Try to ensure at least 16 bits are available. This forces refill attempts.
		// showBits will set markerHit if a marker is encountered during refill.
		d.showBits(16)

		if d.bufBits == 0 {
			// If buffer is empty and no marker hit, it must be EOF.
			break
		}

		// If we have data but no marker yet, we must consume the buffer
		// to allow showBits to read further into the stream (especially if the buffer was full).
		// We consume data byte by byte (8 bits).

		bitsToConsume := 8
		if d.bufBits < 8 {
			bitsToConsume = d.bufBits
		}

		d.skipBits(bitsToConsume)
	}

	// Now, either markerHit is true, or we reached EOF (bufBits=0).

	if d.markerHit {
		// d.pos points at the marker (0xFF). Discard buffer.
		d.buf = 0
		d.bufBits = 0
		d.markerHit = false
	} else {
		// EOF reached. Clear buffer.
		d.buf = 0
		d.bufBits = 0
	}
}

// skip advances the current position in the jpegData buffer by 'count' bytes.
func (d *decoder) skip(count int) error {
	// For resilient decoding, cap skip amount to available data.
	// This handles truncated segments gracefully (e.g., in DecodeConfig with limited buffer).
	if count > d.size {
		count = d.size
	}

	d.pos += count
	d.size -= count

	if d.length >= count {
		d.length -= count
	} else {
		d.length = 0
	}

	// d.size can never be negative now due to the cap above.
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
	// For resilient decoding, allow d.length > d.size (truncated segment).
	// The skip() function will handle this gracefully by capping the skip amount.

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
		if p+1 >= len(d.jpegData) {
			return 0
		}
		if littleEndian {
			return uint16(d.jpegData[p]) | (uint16(d.jpegData[p+1]) << 8)
		}

		return (uint16(d.jpegData[p]) << 8) | uint16(d.jpegData[p+1])
	}

	read32 := func(relOffset int) uint32 {
		p := tiffHeaderPos + relOffset
		if p+3 >= len(d.jpegData) {
			return 0
		}
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
	// If not, we truncate the number of entries we read.
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
		// 0: Unknown/RGB/CMYK
		// 1: YCbCr
		// 2: YCCK
		d.adobeTransformValid = true
		d.adobeTransform = d.jpegData[d.pos+11]

		if d.adobeTransform == 0 {
			d.isRGB = true // For 3-component images, treat as RGB
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
	case 1, 3, 4: // Grayscale, YCbCr/RGB, or CMYK/YCbCrK
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
	} else if d.ncomp == 4 {
		// For 4-component images (CMYK or YCbCrK), we only support two
		// subsampling patterns: [0x11 0x11 0x11 0x11] and [0x22 0x11 0x11 0x22].
		// Check that C and K channels (comp[0] and comp[3]) have the same subsampling,
		// and that M and Y channels (comp[1] and comp[2]) both have 1x1.
		if d.comp[0].ssX != d.comp[3].ssX || d.comp[0].ssY != d.comp[3].ssY {
			return ErrUnsupported
		}
		if d.comp[1].ssX != 1 || d.comp[1].ssY != 1 || d.comp[2].ssX != 1 || d.comp[2].ssY != 1 {
			return ErrUnsupported
		}
		if (d.comp[0].ssX == 1 && d.comp[0].ssY == 1) || (d.comp[0].ssX == 2 && d.comp[0].ssY == 2) {
			// Valid: either all 1x1 or C/K are 2x2
			d.subsampleRatio = image.YCbCrSubsampleRatio444 // Use as placeholder
		} else {
			return ErrUnsupported
		}
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
				// Non-standard subsampling ratio (possibly from a corrupted JPEG).
				// Accept it and use a safe fallback value. We choose 4:4:4 as it's the most general (no subsampling).
				// Force RGBA conversion since YCbCr's At() won't work correctly with non-standard subsampling.
				d.subsampleRatio = image.YCbCrSubsampleRatio444
				d.toRGBA = true
			}
		}
	}

	// Calculate MCU dimensions and image dimensions in MCUs.
	// This MUST be << 3 (shift left by 3 bits = multiply by 8)
	// Each MCU dimension = max_sampling_factor * 8 pixels
	d.mbSizeX = ssxMax << 3
	d.mbSizeY = ssyMax << 3

	// mbWidth/mbHeight represent the number of MCUs in the original image
	d.mbWidth = (d.width + d.mbSizeX - 1) / d.mbSizeX
	d.mbHeight = (d.height + d.mbSizeY - 1) / d.mbSizeY

	// Save original dimensions before scaling
	origWidth := d.width
	origHeight := d.height

	// Apply IDCT scaling to output dimensions (only for full decode, not config-only)
	if !configOnly && d.scaleDenom > 1 {
		d.width = (d.width + d.scaleDenom - 1) / d.scaleDenom
		d.height = (d.height + d.scaleDenom - 1) / d.scaleDenom
	}

	// Calculate output block size based on scaling
	scaleDenom := d.scaleDenom
	if scaleDenom < 1 {
		scaleDenom = 1
	}
	outBlockW := 8 / scaleDenom
	outBlockH := 8 / scaleDenom

	// Calculate component dimensions and allocate memory for pixels or coefficients.
	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]

		// Component dimensions in pixels (scaled if scaleDenom > 1)
		if !configOnly && d.scaleDenom > 1 {
			// Compute scaled component dimensions
			unscaledCompWidth := (origWidth*c.ssX + ssxMax - 1) / ssxMax
			unscaledCompHeight := (origHeight*c.ssY + ssyMax - 1) / ssyMax
			c.width = (unscaledCompWidth + d.scaleDenom - 1) / d.scaleDenom
			c.height = (unscaledCompHeight + d.scaleDenom - 1) / d.scaleDenom
		} else {
			c.width = (d.width*c.ssX + ssxMax - 1) / ssxMax
			c.height = (d.height*c.ssY + ssyMax - 1) / ssyMax
		}

		// Calculate nBlocksX/Y based on MCU dimensions.
		// Both baseline and progressive modes need to allocate enough space for
		// all blocks that the MCU loop will process, including padding blocks at
		// image boundaries for non-MCU-aligned dimensions.
		c.nBlocksX = d.mbWidth * c.ssX
		c.nBlocksY = d.mbHeight * c.ssY

		// The stride must account for the scaled block size
		c.stride = c.nBlocksX * outBlockW

		if ((c.width < 3) && (c.ssX != ssxMax)) || ((c.height < 3) && (c.ssY != ssyMax)) {
			return ErrUnsupported // Smoothing is not supported.
		}

		if !configOnly {
			if d.isProgressive {
				// Progressive: Allocate coefficient buffer based on actual component dimensions
				coeffSize := c.nBlocksX * c.nBlocksY * 64

				if coeffSize <= 0 && (d.width > 0 && d.height > 0) {
					return ErrOutOfMemory
				}

				if coeffSize > 0 {
					// initialize to zero, required for progressive decoding.
					c.coeffs = make([]int32, coeffSize)
				}

				// c.pixels will be allocated in postProcessProgressive.
			} else {
				// Baseline: Allocate pixel buffer using scaled block height
				pixelSize := c.stride * c.nBlocksY * outBlockH
				if pixelSize <= 0 && (d.width > 0 && d.height > 0) {
					// Handle cases where image is not empty but component might be.
				}

				if pixelSize > 0 {
					c.pixels = make([]byte, pixelSize)
				}
			}
		}
	}

	// Allocate the final RGBA buffer if we know we will need it
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

		// Extract values (HUFFVAL). d.pos is now at the start of the values.
		values := d.jpegData[d.pos : d.pos+n]

		// Build the lookup table using canonical Huffman codes.
		// buildVlcTable handles clearing the table (Pooling) and validation.
		if err := buildVlcTable(vlc, &counts, values); err != nil {
			return err
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

	// For resilient decoding: check if we have enough data to actually read the segment.
	// If not (truncated buffer in DecodeConfig), skip it gracefully.
	if d.length > d.size {
		return d.skip(d.size)
	}

	for d.length >= 65 {
		// Bounds check before reading
		if d.pos+65 > len(d.jpegData) {
			// Not enough data, skip what we can
			return d.skip(d.length)
		}

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
	d.pos += 2
	d.size -= 2

	var sofDecoded bool
	var scansCompleted int

markerLoop:
	for {
		// Check if there's enough data for a marker before starting the search.
		if d.size < 2 {
			if d.isProgressive && scansCompleted > 0 {
				break markerLoop // Allow truncated progressive files.
			}

			return nil, ErrSyntax
		}

		// Find the next marker introducer (0xFF).
		idx := bytes.IndexByte(d.jpegData[d.pos:d.pos+d.size], 0xFF)
		if idx == -1 {
			if d.isProgressive && scansCompleted > 0 {
				break markerLoop // No more markers, but we have a partial image.
			}

			return nil, ErrSyntax // No marker found in remaining data.
		}

		// Advance position past any stray data to the 0xFF.
		d.pos += idx
		d.size -= idx

		// We have at least one 0xFF at d.pos. Find the marker code that follows it, skipping any fill bytes (0xFF).
		searchPos := d.pos + 1
		for searchPos < d.pos+d.size && d.jpegData[searchPos] == 0xFF {
			searchPos++
		}

		// Check if we ran out of data while skipping fill bytes.
		if searchPos >= d.pos+d.size {
			if d.isProgressive && scansCompleted > 0 {
				break markerLoop
			}

			return nil, ErrSyntax
		}

		// We found a non-0xFF byte, which is our marker code.
		marker := d.jpegData[searchPos]

		// Advance the main decoder position past all the skipped 0xFFs and the marker code.
		consumedBytes := (searchPos + 1) - d.pos
		d.pos += consumedBytes
		d.size -= consumedBytes

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

		case 0xC9: // SOF9 (Extended sequential DCT, Arithmetic coding)
			// Resilient decoding: treat as baseline and attempt Huffman decoding.
			// Some images marked as arithmetic may still decode with Huffman tables.
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
				// Handle missing Huffman code (padding artifact), common in progressive JPEGs with incomplete tables.
				if errors.Is(err, ErrMissingHuffmanCode) {
					// If we hit a missing Huffman code, it likely means we reached padding bits (all 1s).
					// We must align and synchronize the stream before proceeding to the next marker.
					d.alignAndRewind()

					// Continue the marker loop, effectively terminating the current scan.
					scansCompleted++ // Consider this scan completed (partially or fully).

					if d.isBaseline {
						break markerLoop
					}
					continue // Continue to next marker
				}

				// Best-effort early exit if just before EOI and at least one scan decoded.
				if errors.Is(err, ErrSyntax) &&
					scansCompleted > 0 &&
					d.size >= 2 &&
					d.jpegData[d.pos] == 0xFF &&
					d.jpegData[d.pos+1] == 0xD9 {

					break markerLoop
				}

				// For resilient decoding: if this is baseline mode and we got a syntax error
				// (likely truncated scan data), accept whatever we decoded and break out.
				// This handles corrupted JPEGs with missing EOI markers or truncated scan data.
				if d.isBaseline && errors.Is(err, ErrSyntax) {
					scansCompleted++
					break markerLoop
				}

				return nil, err
			}

			scansCompleted++

			if d.isBaseline {
				// Baseline has a single scan.
				break markerLoop
			}

		case 0xFE: // COM
			if err := d.skipMarker(); err != nil {
				return nil, err
			}

		case 0xD9: // EOI
			break markerLoop

		case 0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7: // RSTn outside scan: ignore
			continue

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
			} else {
				// Handle unknown or unsupported markers.

				// Skip DAC (Define Arithmetic Coding) marker for resilient decoding.
				// We'll attempt Huffman decoding even if the image claims to use arithmetic coding.
				if marker == 0xCC { // DAC
					if err := d.skipMarker(); err != nil {
						return nil, err
					}
					continue
				}

				// Check for unsupported SOF markers (C1, C3, C5-C8, CA-CB, CD-CF).
				// C0, C2, C9 (Supported) and C4 (DHT) are handled above.
				if marker >= 0xC0 && marker <= 0xCF {
					if !sofDecoded {
						// If this is the first SOF and it's unsupported, reject the image.
						return nil, ErrUnsupported
					}
					// If we already saw a SOF, we will skip this one below.
				}

				// Handle standalone markers (no length field).
				if marker == 0x01 { // TEM (Temporary private use)
					continue
				}

				// If we encounter 0x00 here, it usually means the previous scan parsing was incorrect
				// (read a stuffed 0xFF00 as marker 0x00 due to synchronization loss).
				if marker == 0x00 {
					return nil, ErrSyntax
				}

				// Skip unknown markers that have a length field (DNL, EXP, JPGn, etc.).
				if err := d.skipMarker(); err != nil {
					return nil, err
				}
			}
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
	case 4:
		return d.convertToCMYK()
	default:
		return nil, ErrInternal
	}
}

// convertToCMYK converts a 4-component JPEG image to image.CMYK.
// The conversion depends on the Adobe APP14 transform marker:
//   - transform=0 (CMYK): Interleave C, M, Y, K channels with Adobe inversion
//   - transform=2 (YCbCrK): Convert YCbCr to RGB, treat as CMY, apply K channel
//
// Adobe CMYK images are inverted (255 = no ink, 0 = full ink).
func (d *decoder) convertToCMYK() (image.Image, error) {
	if !d.adobeTransformValid {
		return nil, ErrUnsupported
	}

	bounds := image.Rect(0, 0, d.width, d.height)

	// YCbCrK case: Convert YCbCr to RGB, treat RGB as CMY, apply inverted K
	if d.adobeTransform == 2 {
		img := image.NewCMYK(bounds)

		// Convert YCbCr to RGB, which becomes CMY (RGB inversion cancels Adobe inversion)
		for y := 0; y < d.height; y++ {
			for x := 0; x < d.width; x++ {
				// Get YCbCr values
				yy := d.comp[0].pixels[y*d.comp[0].stride+x]
				cb := d.comp[1].pixels[(y/d.comp[1].ssY)*d.comp[1].stride+(x/d.comp[1].ssX)]
				cr := d.comp[2].pixels[(y/d.comp[2].ssY)*d.comp[2].stride+(x/d.comp[2].ssX)]
				kk := d.comp[3].pixels[(y/d.comp[3].ssY)*d.comp[3].stride+(x/d.comp[3].ssX)]

				// YCbCr to RGB conversion
				yy1 := int32(yy)
				cb1 := int32(cb) - 128
				cr1 := int32(cr) - 128

				r := yy1 + (91881*cr1)>>16
				g := yy1 - ((22554*cb1 + 46802*cr1) >> 16)
				b := yy1 + (116130*cb1)>>16

				// Clamp to [0, 255]
				if r < 0 {
					r = 0
				} else if r > 255 {
					r = 255
				}
				if g < 0 {
					g = 0
				} else if g > 255 {
					g = 255
				}
				if b < 0 {
					b = 0
				} else if b > 255 {
					b = 255
				}

				// RGB becomes CMY (no inversion), K is inverted
				offset := y*img.Stride + x*4
				img.Pix[offset+0] = uint8(r) // C
				img.Pix[offset+1] = uint8(g) // M
				img.Pix[offset+2] = uint8(b) // Y
				img.Pix[offset+3] = 255 - kk // K (inverted)
			}
		}
		return img, nil
	}

	// CMYK case (adobeTransform == 0): Interleave channels with inversion
	img := image.NewCMYK(bounds)

	for i := 0; i < 4; i++ {
		comp := &d.comp[i]
		subsample := comp.ssX != 1 || comp.ssY != 1

		for y := 0; y < d.height; y++ {
			sy := y
			if subsample {
				sy /= comp.ssY
			}
			for x := 0; x < d.width; x++ {
				sx := x
				if subsample {
					sx /= comp.ssX
				}
				// Adobe inversion: 255 - value
				img.Pix[y*img.Stride+x*4+i] = 255 - comp.pixels[sy*comp.stride+sx]
			}
		}
	}

	return img, nil
}

// DecodeExif reads EXIF metadata from a JPEG image without decoding the entire image.
// It returns an Exif struct containing common EXIF tags like camera make/model,
// exposure settings, GPS location, and date/time information.
//
// Returns an error if:
//   - The input is not a valid JPEG
//   - No EXIF data is present in the image
//   - EXIF data is corrupted or cannot be parsed
//
// Individual fields in the returned Exif struct may be zero/empty if those specific
// tags are not present in the EXIF data.
func DecodeExif(r io.Reader) (*Exif, error) {
	// Read all data into memory (similar to DecodeConfig approach)
	data, err := readAllData(r)
	if err != nil {
		return nil, err
	}

	// Check for SOI marker
	if len(data) < 2 || data[0] != 0xFF || data[1] != 0xD8 {
		return nil, ErrSyntax
	}

	exif := &Exif{}
	pos := 2
	foundExif := false

	// Scan through JPEG markers looking for APP1 (EXIF)
	for pos+1 < len(data) {
		if data[pos] != 0xFF {
			return nil, ErrSyntax
		}

		marker := data[pos+1]
		pos += 2

		// Check for markers without length field
		if marker == 0xD8 || marker == 0xD9 || (marker >= 0xD0 && marker <= 0xD7) {
			continue
		}

		// Read segment length
		if pos+1 >= len(data) {
			break
		}
		length := int(data[pos])<<8 | int(data[pos+1])
		if length < 2 {
			return nil, ErrSyntax
		}
		length -= 2 // Exclude the length bytes themselves
		pos += 2

		if pos+length > len(data) {
			break
		}

		// Found APP1 marker (EXIF)
		if marker == 0xE1 && length >= 6 {
			// Check for "Exif\0\0" signature
			if data[pos] == 'E' && data[pos+1] == 'x' && data[pos+2] == 'i' &&
				data[pos+3] == 'f' && data[pos+4] == 0 && data[pos+5] == 0 {
				// Parse EXIF data
				if err := parseExifData(data[pos+6:pos+length], exif); err != nil {
					return nil, err
				}
				foundExif = true
				break
			}
		}

		pos += length
	}

	if !foundExif {
		return nil, ErrNoEXIF
	}

	return exif, nil
}
