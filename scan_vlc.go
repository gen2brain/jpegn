package jpegn

// getVLC decodes a single Variable-Length Code (VLC) from the bitstream using the
// pre-built Huffman table, then reads and sign-extends the magnitude bits.
func (d *decoder) getVLC(t *huffTable, code *uint8) int {
	if d.bufBits < 16 && !d.markerHit {
		d.showBits(16)
	}

	// Peek 16 bits, MSB-aligned, padding any missing low bits with 1s.
	var look uint32
	if d.bufBits >= 16 {
		look = uint32((d.buf >> (d.bufBits - 16)) & 0xFFFF)
	} else if d.bufBits > 0 {
		sh := uint(16 - d.bufBits)
		look = uint32(((d.buf << sh) | ((uint64(1) << sh) - 1)) & 0xFFFF)
	} else {
		if code != nil {
			*code = 0
		}
		if d.markerHit || d.size == 0 || d.isProgressive {
			return 0
		}
		d.panic(ErrMissingHuffmanCode)
	}

	// Decode the Huffman symbol: 8-bit fast path, then the long-code path.
	var huffBits int
	var huffCode uint8
	if e := t.lut[look>>8]; e != 0 {
		huffBits = int(e & 0xFF)
		huffCode = uint8(e >> 8)
	} else {
		var ok bool
		huffBits, huffCode, ok = t.decodeLong(look)
		if !ok {
			if code != nil {
				*code = 0
			}
			if d.markerHit || d.size == 0 || d.isProgressive {
				return 0
			}
			d.panic(ErrMissingHuffmanCode)
		}
	}

	if d.bufBits < huffBits {
		// The code was only matched against padding bits; no real data left.
		if code != nil {
			*code = 0
		}
		if d.markerHit || d.size == 0 || d.isProgressive {
			return 0
		}
		d.panic(ErrMissingHuffmanCode)
	}

	if code != nil {
		*code = huffCode
	}

	valBits := int(huffCode & 15)
	if valBits == 0 {
		// EOB or ZRL - just consume the Huffman bits.
		d.bufBits -= huffBits
		return 0
	}

	totalBits := huffBits + valBits
	if d.bufBits < totalBits {
		d.showBits(totalBits)
		if d.bufBits < totalBits {
			if d.markerHit || d.size == 0 || d.isProgressive {
				return 0
			}
			d.panic(ErrSyntax)
		}
	}

	// Extract magnitude and consume all bits at once.
	shift := d.bufBits - totalBits
	maskVal := (uint64(1) << valBits) - 1
	value := int((d.buf >> shift) & maskVal)
	d.bufBits -= totalBits

	return signExtend(value, valBits)
}
