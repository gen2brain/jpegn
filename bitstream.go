package jpegn

import "encoding/binary"

// Bitstream handling

// showBits peeks the next bits without consuming them, handling byte stuffing.
func (d *decoder) showBits(bits int) int {
	if bits == 0 {
		return 0
	}

	// Drop already-consumed bits so they cannot shift back into the stream.
	if d.bufBits < 64 {
		mask := (uint64(1) << d.bufBits) - 1
		d.buf &= mask
	}

	if !d.markerHit {
		// Take four bytes at a time while none of them is 0xFF, so the common
		// stuffing-free run costs one load instead of four branches.
		for d.bufBits <= 32 && d.bufBits < bits && d.size >= 4 {
			v := binary.BigEndian.Uint32(d.jpegData[d.pos:])
			if (^v-0x01010101)&v&0x80808080 != 0 {
				break
			}

			d.buf = d.buf<<32 | uint64(v)
			d.bufBits += 32
			d.pos += 4
			d.size -= 4
		}

	fillLoop:
		// Ensure we don't overflow the 64-bit buffer. Stop filling if d.bufBits > 56 (64-8),
		// as we cannot safely shift left by 8 without losing bits.
		for d.bufBits < bits && d.bufBits <= 56 {
			if d.size <= 0 {
				break fillLoop
			}

			b := d.jpegData[d.pos]
			d.pos++
			d.size--

			if b == 0xFF {
				if d.size <= 0 {
					// Treat lone 0xFF as data (EOF after 0xFF).
				} else {
					b2 := d.jpegData[d.pos]

					if b2 == 0x00 {
						// Stuffed 0xFF00: consume 0x00 and treat 0xFF as data.
						d.pos++
						d.size--
					} else {
						// End of the entropy-coded segment: rewind the 0xFF for the marker parser.

						// Marker: rewind and do NOT add this 0xFF.
						// (Covers RSTn, SOS, EOI, and fill bytes 0xFFFF...)
						d.pos--
						d.size++

						d.markerHit = true

						break fillLoop
					}
				}
			}

			// Append the byte to the low bits; older bits move up.
			d.buf = (d.buf << 8) | uint64(b)
			d.bufBits += 8 // Update d.bufBits after successful refill.
		}
	}

	if d.bufBits == 0 {
		return 0
	}

	if d.bufBits >= bits {
		shift := d.bufBits - bits

		// Use uint64 mask to safely handle bits up to 64 (used for late marker detection).
		mask := (uint64(1) << bits) - 1
		if bits == 64 {
			mask = ^uint64(0)
		}

		return int((d.buf >> shift) & mask)
	}

	// Underfilled: only return existing bits right-aligned (no left padding that could synthesize an unintended Huffman code).
	available := d.bufBits

	// Calculate mask using uint64, handling available=64.
	mask := (uint64(1) << available) - 1
	if available == 64 {
		mask = ^uint64(0)
	}

	val := int(d.buf & mask)

	// Right-align into requested width by shifting if caller masks; we simply return val.
	// Caller always ANDs with ((1<<bits)-1) so returning val is safe.
	return val
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
	// Fast path: we already have enough bits in the buffer AND we haven't hit a marker.
	if d.bufBits >= bits && !d.markerHit {
		shift := d.bufBits - bits
		res := int((d.buf >> shift) & ((1 << bits) - 1))
		d.bufBits = shift
		return res
	}

	if bits == 0 {
		return 0
	}

	// Slow path: ensure enough bits (and check marker semantics).
	res := d.showBits(bits)

	// If the buffer is underfilled (d.bufBits < bits).
	if d.bufBits < bits {
		// If we hit a marker (d.markerHit is true, potentially set by showBits), we allow graceful termination.
		// The caller should detect d.markerHit and stop decoding the scan.
		if d.markerHit {
			// The scan is terminating, so leave the bits unconsumed for alignAndRewind.
			return res
		}

		// For progressive scans, running out of data without a marker is normal
		// Just return what we have (possibly 0) without panicking
		if d.isProgressive && d.size == 0 {
			return res
		}

		// If no marker hit and not progressive EOF, it is a fatal syntax error
		d.panic(ErrSyntax)
	}

	d.skipBits(bits)

	return res
}

// getBit reads one bit, returning 0 past a marker or EOF.
func (d *decoder) getBit() int {
	if d.bufBits > 0 && !d.markerHit {
		d.bufBits--

		return int((d.buf >> d.bufBits) & 1)
	}

	return d.getBitSlow()
}

// getBitSlow handles buffer refill and marker/EOF semantics for getBit.
func (d *decoder) getBitSlow() int {
	d.showBits(1)

	if d.bufBits < 1 {
		// Buffer underfilled. Per the JPEG spec for refinement, a marker hit or
		// EOF means the missing bits are treated as 0.
		if d.markerHit || d.size == 0 {
			return 0
		}

		// If no marker hit and not EOF, it is a fatal syntax error.
		d.panic(ErrSyntax)
	}

	d.bufBits--

	return int((d.buf >> d.bufBits) & 1)
}

// byteAlign aligns the bitstream to the next byte boundary.
func (d *decoder) byteAlign() {
	d.bufBits &= ^7 // equivalent to (d.bufBits / 8) * 8
}
