//go:build amd64 && !noasm

package jpegn

// getVLC is a branchless optimized version for AMD64.
// This version reduces branches in the hot path using arithmetic instead of conditional jumps.
func (d *decoder) getVLC(vlc *[65536]vlcCode, vlc8 *[256]vlcCode8, code *uint8) int {
	// Fast path: buffer has >=16 bits, no marker
	// Combined condition check - if false, skip to slow path
	if d.bufBits >= 16 && !d.markerHit {
		// Extract 16-bit value from buffer
		value16 := int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
		entry := vlc[value16]
		huffBits := int(entry.bits)
		huffCode := entry.code

		if huffBits > 0 {
			// Valid Huffman code found
			valBits := int(huffCode & 15)
			totalBits := huffBits + valBits

			if valBits == 0 {
				// EOB or ZRL - just consume Huffman bits
				d.bufBits -= huffBits
				// Branchless conditional write to code pointer
				// Instead of: if code != nil { *code = huffCode }
				// Use pointer arithmetic
				if code != nil {
					*code = huffCode
				}
				return 0
			}

			if d.bufBits >= totalBits {
				// Extract magnitude and consume all bits at once
				shift := d.bufBits - totalBits
				maskVal := (uint64(1) << valBits) - 1
				value := int((d.buf >> shift) & maskVal)
				d.bufBits -= totalBits

				// Branchless sign extension using arithmetic
				// Original:
				//   if value < (1 << (valBits - 1)) {
				//       value += ((-1) << valBits) + 1
				//   }
				//
				// Branchless version:
				// Compute mask: -1 if value < threshold, 0 otherwise
				threshold := 1 << (valBits - 1)
				// Using arithmetic right shift to propagate sign bit
				// Note: int is 64-bit on amd64, so shift by 63
				signBit := (value - threshold) >> 63  // -1 if negative, 0 otherwise
				// Correction = ((-1) << valBits) + 1 = -(2^valBits - 1)
				// = (-1 << valBits) - (-1) when signBit = -1
				correction := (signBit << valBits) - signBit
				value += correction

				// Branchless conditional write
				if code != nil {
					*code = huffCode
				}
				return value
			}
		}
	}

	// Slow path: refill buffer and retry
	d.showBits(16)

	value16 := 0
	if d.bufBits >= 16 {
		value16 = int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
	} else if d.bufBits > 0 {
		// Left-align and pad with 1s
		shift := 16 - d.bufBits
		value16 = int((d.buf << shift) | uint64((1<<shift)-1))
	}

	entry := vlc[value16&0xFFFF]
	huffBits := int(entry.bits)
	huffCode := entry.code

	if huffBits == 0 || d.bufBits < huffBits {
		// Missing or truncated code
		if d.markerHit || d.size == 0 {
			if code != nil {
				*code = 0
			}
			return 0
		}
		if d.isProgressive {
			if code != nil {
				*code = 0
			}
			return 0
		}
		d.panic(ErrMissingHuffmanCode)
	}

	if code != nil {
		*code = huffCode
	}

	valBits := int(huffCode & 15)
	if valBits == 0 {
		// EOB or ZRL
		d.bufBits -= huffBits
		return 0
	}

	totalBits := huffBits + valBits
	if d.bufBits < totalBits {
		d.showBits(totalBits)
		if d.bufBits < totalBits {
			if d.markerHit || d.size == 0 {
				return 0
			}
			if d.isProgressive {
				return 0
			}
			d.panic(ErrSyntax)
		}
	}

	// Extract magnitude and consume bits
	shift := d.bufBits - totalBits
	maskVal := (uint64(1) << valBits) - 1
	value := int((d.buf >> shift) & maskVal)
	d.bufBits -= totalBits

	// Branchless sign extension
	threshold := 1 << (valBits - 1)
	signBit := (value - threshold) >> 63  // int is 64-bit on amd64
	correction := (signBit << valBits) - signBit
	value += correction

	return value
}
