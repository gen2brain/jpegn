package jpegn

// Entropy Decoding

// getVLC decodes a single Variable-Length Code (VLC) from the bitstream using
// the pre-built Huffman tables. It returns the decoded integer value.
// Uses panic for errors, uint64 buffer, and fast path extraction.
func (d *decoder) getVLC(vlc *[65536]vlcCode, code *uint8) int {
	// Fast path: if we already have >=16 bits in the buffer, avoid showBits call overhead.
	var value16 int
	if d.bufBits >= 16 && !d.markerHit {
		value16 = int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
	} else {
		value16 = d.showBits(16)
	}

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
		// If we hit a marker, we must ensure the Huffman code didn't rely on padded bits,
		// as we won't call getBits() which usually performs this check.
		if d.markerHit && d.bufBits < huffBits {
			d.panic(ErrSyntax)
		}

		d.skipBits(huffBits)

		return 0
	}

	totalBits := huffBits + valBits

	// Fast path: enough bits in buffer, no marker.
	if d.bufBits >= totalBits && !d.markerHit {
		shift := d.bufBits - totalBits
		mask := (uint64(1) << valBits) - 1
		value := int((d.buf >> shift) & mask)
		d.bufBits -= totalBits

		// Sign extension.
		if value < (1 << (valBits - 1)) {
			value += ((-1) << valBits) + 1
		}

		return value
	}

	// Slow path.
	d.skipBits(huffBits)
	value := d.getBits(valBits)
	if value < (1 << (valBits - 1)) {
		value += ((-1) << valBits) + 1
	}

	return value
}

// getHuffSymbol decodes a Huffman symbol from the bitstream without reading subsequent value bits.
// Used for progressive AC decoding where value bits might be handled separately (e.g., refinement).
func (d *decoder) getHuffSymbol(vlc *[65536]vlcCode) int {
	// Fast path: if we already have >=16 bits in the buffer, avoid showBits call overhead.
	var value16 int
	if d.bufBits >= 16 && !d.markerHit {
		value16 = int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
	} else {
		value16 = d.showBits(16)
	}

	entry := vlc[value16]
	huffBits := int(entry.bits)
	if huffBits == 0 {
		d.panic(ErrSyntax)
	}

	// Marker semantics: if this symbol relied on padded bits due to a marker, it's invalid.
	if d.markerHit && d.bufBits < huffBits {
		d.panic(ErrSyntax)
	}

	d.skipBits(huffBits)

	return int(entry.code)
}

// decodeBlock decodes a single 8x8 block of a component (Baseline). This involves
// entropy decoding of DC and AC coefficients, dequantization, and applying the IDCT.
// Uses panic for errors and optimized table access.
func (d *decoder) decodeBlock(c *component, outOffset int) {
	var code uint8
	var value int

	// This clears the array to zeros.
	d.block = [64]int32{}

	// Cache pointers to tables used in the loop.
	// Quantization table is stored in natural order (row-major), not zigzag.
	qt := d.qtab[c.qtSel]
	dcVLC := d.dcVlcTab[c.dcTabSel]
	acVLC := d.acVlcTab[c.acTabSel]

	// Decode DC coefficient.
	value = d.getVLC(dcVLC, nil)
	c.dcPred += value
	d.block[0] = int32(c.dcPred) * int32(qt[0])

	// Decode AC coefficients.
	coef := 1 // coef is the zigzag index.
	for coef <= 63 {
		value = d.getVLC(acVLC, &code)

		if code == 0 { // EOB
			break
		}

		if (code & 0x0F) == 0 {
			if code != 0xF0 { // ZRL
				d.panic(ErrSyntax)
			}
			coef += 16

			continue
		}

		coef += int(code >> 4) // Skip run of zeros.
		if coef > 63 {
			d.panic(ErrSyntax)
		}

		// Map zigzag index to natural index.
		naturalIndex := zz[coef]

		// Dequantize using the natural index.
		d.block[naturalIndex] = int32(value) * int32(qt[naturalIndex])

		coef++
	}

	// Perform IDCT (in-place into the output pixel buffer).
	idct(&d.block, c.pixels, outOffset, c.stride)
}

// decodeScan decodes the image scan data. It dispatches to the baseline or progressive decoder.
// Handles panics from the hot path.
func (d *decoder) decodeScan() (err error) {
	// Setup recovery for panics in the hot path (getVLC, decodeBlock, progressive logic).
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

	// The actual decoding logic is handled by decodeScanInternal which parses the header
	// and calls the appropriate baseline or progressive loop.
	return d.decodeScanInternal()
}

// decodeScanInternal handles the parsing of the SOS header and the main decoding loop.
func (d *decoder) decodeScanInternal() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	// Parse SOS header.
	nCompScan := int(d.jpegData[d.pos])
	if d.length < (4+2*nCompScan) || nCompScan < 1 || nCompScan > 4 {
		return ErrSyntax
	}

	if err := d.skip(1); err != nil {
		return err
	}

	var scanComp [4]int
	for i := 0; i < nCompScan; i++ {
		scanID := int(d.jpegData[d.pos])
		found := false
		for j := 0; j < d.ncomp; j++ {
			if d.comp[j].id == scanID {
				c := &d.comp[j]
				scanComp[i] = j

				c.dcTabSel = int(d.jpegData[d.pos+1]) >> 4
				c.acTabSel = int(d.jpegData[d.pos+1]) & 0x0F

				if c.dcTabSel > 3 || c.acTabSel > 3 {
					return ErrSyntax
				}

				found = true

				break
			}
		}

		if !found {
			return ErrSyntax
		}

		if err := d.skip(2); err != nil {
			return err
		}
	}

	ss := int(d.jpegData[d.pos])
	se := int(d.jpegData[d.pos+1])
	ah := int(d.jpegData[d.pos+2]) >> 4
	al := int(d.jpegData[d.pos+2]) & 0x0F

	if err := d.skip(d.length); err != nil {
		return err
	}

	d.buf = 0
	d.bufBits = 0

	if d.isBaseline {
		if ss != 0 || se != 63 || ah != 0 || al != 0 {
			return ErrUnsupported
		}

		if nCompScan != d.ncomp {
			return ErrUnsupported
		}

		return d.decodeScanBaseline(nCompScan, scanComp)
	}

	if ss > se || se > 63 {
		return ErrSyntax
	}

	isDC := ss == 0
	if isDC {
		if se != 0 {
			return ErrSyntax
		}
	} else {
		if nCompScan != 1 {
			return ErrUnsupported
		}
	}

	return d.decodeScanProgressive(nCompScan, scanComp, ss, se, ah, al)
}

// decodeScanBaseline decodes a baseline JPEG scan.
func (d *decoder) decodeScanBaseline(nCompScan int, scanComp [4]int) error {
	rstCount := d.rstInterval
	nextRst := 0

	// Reset DC predictors at the start of the scan.
	for k := 0; k < d.ncomp; k++ {
		d.comp[k].dcPred = 0
	}

	for mby := 0; mby < d.mbHeight; mby++ {
		for mbx := 0; mbx < d.mbWidth; mbx++ {
			for i := 0; i < nCompScan; i++ {
				compIndex := scanComp[i]
				c := &d.comp[compIndex]

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
					// Per spec, before reading a marker we must be byte-aligned.
					d.byteAlign()
					d.buf = 0
					d.bufBits = 0

					// Read the restart marker directly from the byte stream.
					if d.size < 2 {
						d.panic(ErrSyntax)
					}

					if d.jpegData[d.pos] != 0xFF || (d.jpegData[d.pos+1]&0xF8) != 0xD0 || int(d.jpegData[d.pos+1]&0x07) != nextRst {
						d.panic(ErrSyntax)
					}

					// Consume marker bytes.
					d.pos += 2
					d.size -= 2

					nextRst = (nextRst + 1) & 7
					rstCount = d.rstInterval

					for k := 0; k < d.ncomp; k++ {
						d.comp[k].dcPred = 0
					}
				}
			}
		}
	}

	return nil
}

// Progressive Decoding Functions

// decodeScanProgressive decodes a progressive JPEG scan.
func (d *decoder) decodeScanProgressive(nCompScan int, scanComp [4]int, ss, se, ah, al int) error {
	isDC := ss == 0
	if isDC && ah == 0 {
		for i := 0; i < nCompScan; i++ {
			d.comp[scanComp[i]].dcPred = 0
		}
	}

	d.eobRun = 0
	rstCount := d.rstInterval
	nextRst := 0

	// Fast-path: the overwhelmingly common case is single-component scan.
	if nCompScan == 1 {
		c := &d.comp[scanComp[0]]
		blocksX := d.mbWidth * c.ssX
		blocksY := d.mbHeight * c.ssY
		rowStrideBlocks := blocksX
		rowStride64 := rowStrideBlocks * 64

		for by := 0; by < blocksY; by++ {
			rowBase := by * rowStride64
			for bx := 0; bx < blocksX; bx++ {
				offset := rowBase + bx*64
				if isDC {
					d.decodeBlockDC(c, offset, ah, al)
				} else {
					var eobStarted bool
					if ah == 0 {
						eobStarted = d.decodeBlockACFirst(c, offset, ss, se, al)
					} else {
						eobStarted = d.decodeBlockACRefine(c, offset, ss, se, al)
					}

					if eobStarted {
						// Handle restart if needed on unit boundary.
						if d.rstInterval != 0 {
							rstCount--
							if rstCount == 0 {
								d.eobRun = 0
								d.byteAlign()
								d.buf = 0
								d.bufBits = 0

								if d.size < 2 {
									d.panic(ErrSyntax)
								}

								if d.jpegData[d.pos] != 0xFF || (d.jpegData[d.pos+1]&0xF8) != 0xD0 || int(d.jpegData[d.pos+1]&0x07) != nextRst {
									d.panic(ErrSyntax)
								}

								d.pos += 2
								d.size -= 2
								nextRst = (nextRst + 1) & 7
								rstCount = d.rstInterval
								c.dcPred = 0
							}
						}

						// Continue to next unit.
						continue
					}
				}

				// Restart marker handling per unit (block) in single-component scans.
				if d.rstInterval != 0 {
					rstCount--
					if rstCount == 0 {
						d.eobRun = 0
						d.byteAlign()
						d.buf = 0
						d.bufBits = 0

						if d.size < 2 {
							d.panic(ErrSyntax)
						}

						if d.jpegData[d.pos] != 0xFF || (d.jpegData[d.pos+1]&0xF8) != 0xD0 || int(d.jpegData[d.pos+1]&0x07) != nextRst {
							d.panic(ErrSyntax)
						}

						d.pos += 2
						d.size -= 2
						nextRst = (nextRst + 1) & 7
						rstCount = d.rstInterval
						c.dcPred = 0
					}
				}
			}
		}

		d.byteAlign()
		d.buf = 0
		d.bufBits = 0

		return nil
	}

	// Generic (multi-component / interleaved) path remains as before.
	rstCount = d.rstInterval
	nextRst = 0

	var loopHeight, loopWidth int
	if nCompScan == 1 {
		c := &d.comp[scanComp[0]]
		loopWidth = d.mbWidth * c.ssX
		loopHeight = d.mbHeight * c.ssY
	} else {
		loopWidth = d.mbWidth
		loopHeight = d.mbHeight
	}

	// Pre-calculate block dimensions for single component scans
	var nBlocksX int
	if nCompScan == 1 {
		nBlocksX = d.mbWidth * d.comp[scanComp[0]].ssX
	}

	for mby := 0; mby < loopHeight; mby++ {
		for mbx := 0; mbx < loopWidth; mbx++ {
			for i := 0; i < nCompScan; i++ {
				compIndex := scanComp[i]
				c := &d.comp[compIndex]

				var startX, startY, endX, endY int
				if nCompScan == 1 {
					startX, startY = mbx, mby
					endX, endY = mbx+1, mby+1
				} else {
					startX, startY = mbx*c.ssX, mby*c.ssY
					endX, endY = startX+c.ssX, startY+c.ssY
				}

				// Use pre-calculated nBlocksX for single component scans
				blockRowStride := nBlocksX
				if nCompScan != 1 {
					blockRowStride = d.mbWidth * c.ssX
				}
				blockRowStride64 := blockRowStride * 64

				for by := startY; by < endY; by++ {
					rowBase := by * blockRowStride64
					for bx := startX; bx < endX; bx++ {
						blockOffset := rowBase + bx*64
						if isDC {
							d.decodeBlockDC(c, blockOffset, ah, al)
						} else {
							var eobStarted bool
							if ah == 0 {
								eobStarted = d.decodeBlockACFirst(c, blockOffset, ss, se, al)
							} else {
								eobStarted = d.decodeBlockACRefine(c, blockOffset, ss, se, al)
							}

							if eobStarted {
								goto endOfUnit
							}
						}
					}
				}
			}
		endOfUnit:
			if d.rstInterval != 0 {
				rstCount--
				if rstCount == 0 {
					d.eobRun = 0
					d.byteAlign()
					d.buf = 0
					d.bufBits = 0

					if d.size < 2 {
						d.panic(ErrSyntax)
					}

					if d.jpegData[d.pos] != 0xFF || (d.jpegData[d.pos+1]&0xF8) != 0xD0 || int(d.jpegData[d.pos+1]&0x07) != nextRst {
						d.panic(ErrSyntax)
					}

					d.pos += 2
					d.size -= 2
					nextRst = (nextRst + 1) & 7
					rstCount = d.rstInterval

					for k := 0; k < nCompScan; k++ {
						d.comp[scanComp[k]].dcPred = 0
					}
				}
			}
		}
	}

	d.byteAlign()
	d.buf = 0
	d.bufBits = 0

	return nil
}

// decodeBlockDC decodes the DC coefficient (or refines it) for a progressive scan.
// DC coefficients are always at index 0 (both zigzag and natural).
func (d *decoder) decodeBlockDC(c *component, offset int, ah, al int) {
	if ah == 0 {
		// First DC scan (Ah=0).
		dcVLC := d.dcVlcTab[c.dcTabSel]
		diff := d.getVLC(dcVLC, nil)
		c.dcPred += diff
		// Store the coefficient, shifted by Al.
		c.coeffs[offset] = int32(c.dcPred << al)
	} else {
		// DC refinement scan (Ah>0).
		// Per JPEG standard (G.1.2.3) and Go stdlib implementation, we use bitwise OR.
		if d.getBit() == 1 {
			c.coeffs[offset] |= int32(1 << al)
		}
	}
}

// decodeBlockACFirst handles the first pass (Ah=0) AC coefficient decoding.
func (d *decoder) decodeBlockACFirst(c *component, offset int, ss, se, al int) bool {
	// If an EOB run is active, this entire block has no new ACs in the current band.
	if d.eobRun > 0 {
		d.eobRun--

		return true
	}

	acVLC := d.acVlcTab[c.acTabSel]
	k := ss // k is the zigzag index.

	// Work on a local 64-coefficient window to avoid repeated offset + zz[k].
	coefs := c.coeffs[offset : offset+64]

	for k <= se {
		symbol := d.getHuffSymbol(acVLC)
		R := symbol >> 4
		S := symbol & 0x0F

		if S == 0 {
			if R != 15 {
				// EOB run. The run length is read from the stream.
				runLen := 1 << R
				if R > 0 {
					runLen += d.getBits(R)
				}

				d.eobRun = runLen - 1

				return true
			}

			// ZRL (R=15, S=0). Skip 16 zero coefficients.
			k += 16
		} else {
			// Skip R zero coefficients.
			k += R
			if k > se {
				d.panic(ErrSyntax)
			}

			// Decode the coefficient value (S bits).
			value := d.getBits(S)
			if value < (1 << (S - 1)) {
				value += ((-1) << S) + 1
			}

			coefs[zz[k]] = int32(value << al)
			k++
		}
	}

	return false
}

// decodeBlockACRefine handles the refinement pass (Ah>0) AC coefficient decoding.
func (d *decoder) decodeBlockACRefine(c *component, offset int, ss, se, al int) bool {
	acVLC := d.acVlcTab[c.acTabSel]
	p1 := int32(1 << al)

	coefs := c.coeffs[offset : offset+64]
	k := ss

	refine := func(idx int) {
		if d.getBit() == 1 {
			if coefs[idx] > 0 {
				coefs[idx] += p1
			} else {
				coefs[idx] -= p1
			}
		}
	}

	// If an EOB run is active, just refine existing non-zero coefficients.
	if d.eobRun > 0 {
		for ; k <= se; k++ {
			idx := zz[k]
			if coefs[idx] != 0 {
				refine(idx)
			}
		}

		d.eobRun--

		return true
	}

	for k <= se {
		idx := zz[k]

		// Refine already non-zero coefficient.
		if coefs[idx] != 0 {
			refine(idx)
			k++

			continue
		}

		// Decode (R,S) symbol.
		symbol := d.getHuffSymbol(acVLC)
		R := symbol >> 4
		S := symbol & 0x0F

		if S == 0 {
			if R == 15 {
				// ZRL: skip 16 zero coefficients, refining intervening non-zeros.
				zerosToSkip := 16
				for zerosToSkip > 0 {
					if k > se {
						break
					}

					idx = zz[k]
					if coefs[idx] != 0 {
						refine(idx)
					} else {
						zerosToSkip--
					}

					k++
				}

				continue
			}

			// EOBRUN: read run length, refine remaining non-zeros in this block, then set eobRun.
			runLen := 1 << R
			if R > 0 {
				runLen += d.getBits(R)
			}

			for ; k <= se; k++ {
				idx = zz[k]
				if coefs[idx] != 0 {
					refine(idx)
				}
			}

			d.eobRun = runLen - 1

			return true
		}

		// S must be 1 for refinement scans.
		if S != 1 {
			d.panic(ErrSyntax)
		}

		// Skip R zeros, refining any intervening non-zero coefficients.
		for {
			if k > se {
				d.panic(ErrSyntax)
			}

			idx = zz[k]
			if coefs[idx] != 0 {
				refine(idx)
			} else {
				if R == 0 {
					break
				}

				R--
			}

			k++
		}

		// Insert new coefficient with magnitude 1<<Al and sign from next bit.
		if d.getBit() == 1 {
			coefs[zz[k]] = p1
		} else {
			coefs[zz[k]] = -p1
		}

		k++
	}

	return false
}

// postProcessProgressive performs dequantization and IDCT after all progressive scans are complete.
func (d *decoder) postProcessProgressive() error {
	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		qt := d.qtab[c.qtSel] // qt is in natural order.

		// Allocate pixel buffer if not already allocated.
		pixelSize := c.stride * d.mbHeight * c.ssY << 3
		if pixelSize <= 0 && (d.width > 0 && d.height > 0) {
			// Check if we actually have coefficients to process.
			if len(c.coeffs) > 0 {
				return ErrOutOfMemory
			}

			continue // Empty component
		}

		if pixelSize > 0 && c.pixels == nil {
			c.pixels = make([]byte, pixelSize)
		} else if c.pixels == nil {
			continue
		}

		nBlocksX := d.mbWidth * c.ssX
		nBlocksY := d.mbHeight * c.ssY
		rowStride64 := nBlocksX * 64

		for by := 0; by < nBlocksY; by++ {
			base := by * rowStride64
			for bx := 0; bx < nBlocksX; bx++ {
				offset := base + bx*64

				// Dequantize using local 64-window to eliminate bounds checks.
				coefs := c.coeffs[offset : offset+64]
				for k := 0; k < 64; k++ {
					d.block[k] = coefs[k] * int32(qt[k])
				}

				// IDCT
				outOffset := (by<<3)*c.stride + (bx << 3)
				idct(&d.block, c.pixels, outOffset, c.stride)
			}
		}

		// Free the coefficient buffer.
		c.coeffs = nil
	}

	return nil
}
