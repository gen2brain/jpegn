package jpegn

// Bitstream handling

// showBits reads 'bits' number of bits from the bitstream without consuming them.
// It ensures the internal buffer `d.buf` has enough data, reading from the main
// jpegData slice if necessary. It also handles JPEG byte stuffing (0xFF00).
func (d *decoder) showBits(bits int) int {
	if bits == 0 {
		return 0
	}

	// Mask d.buf to ensure only valid bits are kept before refilling.
	// This prevents stale (already consumed) bits from being shifted back into the active stream.
	// If d.bufBits < 64, we calculate the mask. If d.bufBits == 0, mask is 0, clearing d.buf.
	// If d.bufBits >= 64 (though typically <= 56 before refill), the entire buffer is valid, no masking needed.
	if d.bufBits < 64 {
		mask := (uint64(1) << d.bufBits) - 1
		d.buf &= mask
	}

	if !d.markerHit {
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
						// Marker or Fill bytes (0xFFxx where xx!=0x00, including 0xFFFF).
						// This indicates the end of the Entropy Coded Segment (ECS).
						// We must stop the scan and rewind the 0xFF so the marker parser can read it.

						// Marker: rewind and do NOT add this 0xFF.
						// (Covers RSTn, SOS, EOI, and fill bytes 0xFFFF...)
						d.pos--
						d.size++

						d.markerHit = true

						break fillLoop
					}
				}
			}

			// Append data byte to the low bits; older bits move up.
			// Since d.bufBits <= 56, the shift is safe.
			// d.buf is already masked above, so the shift operates only on valid bits.
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
			// We do not consume the bits (skipBits), as the scan is terminating.
			// The buffer will be realigned later by alignAndRewind.
			// The returned value 'res' contains the available bits (right-aligned), though it's mostly irrelevant as the scan stops.
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

// getBit reads a single bit from the bitstream. Used for successive approximation.
// Returns 0 or 1. Treats missing bits (due to marker hit or EOF) as 0.
// The fast path is kept small so it inlines into the refinement hot loops.
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
