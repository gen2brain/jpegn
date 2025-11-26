//go:build (!amd64 && !arm64) || noasm

package jpegn

// getVLC decodes a single Variable-Length Code (VLC) from the bitstream using the pre-built Huffman tables.
func (d *decoder) getVLC(vlc *[65536]vlcCode, vlc8 *[256]vlcCode8, code *uint8) int {
	// Optimized fast path: buffer has >=16 bits, no marker
	if d.bufBits >= 16 && !d.markerHit {
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

				// Sign extension
				if value < (1 << (valBits - 1)) {
					value += ((-1) << valBits) + 1
				}

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

	// Sign extension
	if value < (1 << (valBits - 1)) {
		value += ((-1) << valBits) + 1
	}

	return value
}
