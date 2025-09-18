package jpegn

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

					break fillLoop // Stop filling the buffer.
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
