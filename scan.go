package jpegn

func (d *decoder) ensureACTouchedSize(nBlocks int) {
	if cap(d.acTouched) < nBlocks {
		d.acTouched = make([]bool, nBlocks)
	} else {
		d.acTouched = d.acTouched[:nBlocks]
	}

	for i := range d.acTouched {
		d.acTouched[i] = false
	}
}

// Entropy Decoding

// getHuffSymbol decodes a Huffman symbol from the bitstream without reading subsequent value bits.
func (d *decoder) getHuffSymbol(vlc *[65536]vlcCode, blockIndex int, compID int) int {
	// Fast path: buffer has at least 16 bits and no marker hit
	if d.bufBits >= 16 && !d.markerHit {
		value16 := int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
		entry := vlc[value16]
		huffBits := int(entry.bits)

		if huffBits > 0 && d.bufBits >= huffBits {
			d.bufBits -= huffBits
			return int(entry.code)
		}
	}

	// Slow path - fill buffer and lookup
	d.showBits(16)

	// Try lookup with current buffer state
	if d.bufBits >= 16 {
		value16 := int((d.buf >> (d.bufBits - 16)) & 0xFFFF)
		entry := vlc[value16]
		if entry.bits > 0 {
			d.bufBits -= int(entry.bits)
			return int(entry.code)
		}
	} else if d.bufBits > 0 {
		// Buffer underfilled - left-align and pad
		shift := 16 - d.bufBits
		value16 := int((d.buf << shift) | uint64((1<<shift)-1))
		entry := vlc[value16&0xFFFF]
		if entry.bits > 0 && int(entry.bits) <= d.bufBits {
			d.bufBits -= int(entry.bits)
			return int(entry.code)
		}
	}

	// Return 0 (EOB) if no valid code found
	return 0
}

// decodeBlock decodes a single 8x8 block of a component (Baseline).
// This involves entropy decoding of DC and AC coefficients, dequantization, and applying the IDCT.
func (d *decoder) decodeBlock(c *component, outOffset int) {
	var code uint8
	var value int

	// This clears the array to zeros.
	d.block = [64]int32{}

	// Cache pointers to tables used in the loop.
	// Quantization table is stored in natural order (row-major), not zigzag.
	qt := d.qtab[c.qtSel]
	dcVLC := d.dcVlcTab[c.dcTabSel]
	dcVLC8 := d.dcVlcTab8[c.dcTabSel]
	acVLC := d.acVlcTab[c.acTabSel]
	acVLC8 := d.acVlcTab8[c.acTabSel]

	// Decode DC coefficient.
	value = d.getVLC(dcVLC, dcVLC8, nil)
	// Note: Baseline scans should generally not hit markers during getVLC unexpectedly.
	// If they do, we might proceed with potentially corrupted data if d.markerHit isn't checked here.

	c.dcPred += value
	d.block[0] = int32(c.dcPred) * int32(qt[0])

	// Decode AC coefficients.
	coef := 1 // coef is the zigzag index.
	for coef <= 63 {
		value = d.getVLC(acVLC, acVLC8, &code)

		if code == 0 { // EOB (also handles graceful termination at EOF)
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
	// Use scaled IDCT if scaling is enabled
	idctScaled(&d.block, c.pixels, outOffset, c.stride, d.scaleDenom)
}

// decodeScan decodes the image scan data. It dispatches to the baseline or progressive decoder.
// Handles panics from the hot path.
func (d *decoder) decodeScan() (err error) {
	// Recovery for panics in the hot path (getVLC, decodeBlock, progressive logic).
	defer func() {
		if r := recover(); r != nil {
			if de, ok := r.(errDecode); ok {
				err = de.error
			} else {
				// Propagate other panics
				panic(r)
			}
		}
	}()

	// decodeScanInternal parses the header and calls the appropriate baseline or progressive loop.
	return d.decodeScanInternal()
}

// decodeScanInternal handles the parsing of the SOS header and the main decoding loop.
func (d *decoder) decodeScanInternal() error {
	if err := d.decodeLength(); err != nil {
		return err
	}

	nCompScan := int(d.jpegData[d.pos])
	if d.length < (4+2*nCompScan) || nCompScan < 1 || nCompScan > 4 {
		return ErrSyntax
	}

	if err := d.skip(1); err != nil {
		return err
	}

	qtNeeded := 0
	var scanComp [4]int
	var compIDs []int

	for i := 0; i < nCompScan; i++ {
		scanID := int(d.jpegData[d.pos])
		compIDs = append(compIDs, scanID)

		found := false

		for j := 0; j < d.ncomp; j++ {
			if d.comp[j].id == scanID {
				c := &d.comp[j]
				scanComp[i] = j
				qtNeeded |= 1 << c.qtSel

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

	if (qtNeeded &^ d.qtAvail) != 0 {
		return ErrSyntax
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
	d.markerHit = false

	// Reset EOB run at the start of each scan, as required by the standard (Annex G.2.3).
	// This prevents excessive EOB runs from carrying over between scans.
	d.eobRun = 0

	if d.isBaseline {
		if ss != 0 || se != 63 || ah != 0 || al != 0 {
			return ErrUnsupported
		}

		if d.ncomp > 1 && nCompScan != d.ncomp {
			return ErrUnsupported
		}

		return d.decodeScanBaseline(nCompScan, scanComp)
	}

	if ss > se || se > 63 {
		return ErrSyntax
	}

	if ah > 13 || al > 13 {
		return ErrSyntax
	}

	isDC := ss == 0
	if isDC {
		if se != 0 {
			return ErrSyntax
		}
	} else if nCompScan > 1 {
		return ErrSyntax
	}

	if isDC {
		return d.decodeScanProgressiveDC(nCompScan, scanComp, ah, al)
	}

	return d.decodeScanProgressiveAC(nCompScan, scanComp, ss, se, ah, al)
}

// decodeScanProgressiveDC handles progressive DC scans
func (d *decoder) decodeScanProgressiveDC(nCompScan int, scanComp [4]int, ah, al int) error {
	// Initialize restart interval counters
	rstCount := d.rstInterval
	nextRst := 0

	// Reset DC predictors for first pass
	if ah == 0 {
		for i := 0; i < nCompScan; i++ {
			d.comp[scanComp[i]].dcPred = 0
		}
	}

	blocksProcessed := 0

	// Single-component (non-interleaved) DC progressive scan path
	if nCompScan == 1 {
		compIndex := scanComp[0]
		c := &d.comp[compIndex]
		totalBlocks := c.nBlocksX * c.nBlocksY

		// Process all blocks
		for blockIndex := 0; blockIndex < totalBlocks; blockIndex++ {
			coeffOffset := blockIndex * 64

			// Bounds check
			if coeffOffset+64 > len(c.coeffs) {
				break
			}

			// Decode DC coefficient
			if ah == 0 {
				// First pass - decode DC coefficient difference
				var diff int

				// Try to decode the DC coefficient
				dcVLC := d.dcVlcTab[c.dcTabSel]
				dcVLC8 := d.dcVlcTab8[c.dcTabSel]
				diff = d.getVLC(dcVLC, dcVLC8, nil)

				// Always update predictor and store coefficient
				c.dcPred += diff
				c.coeffs[coeffOffset] = int32(c.dcPred) << al
				blocksProcessed++
			} else {
				// Refinement pass - try to get a bit, getBit will return 0 if no data
				bit := d.getBit()
				if bit > 0 {
					c.coeffs[coeffOffset] |= int32(1 << al)
				}

				blocksProcessed++
			}

			// Handle restart markers
			if d.rstInterval > 0 && !d.markerHit {
				rstCount--
				if rstCount == 0 {
					if !d.processRestart(&nextRst, &rstCount, ah, 1, scanComp) {
						// Restart processing failed
					}
				}
			}
		}

		d.alignAndRewind()
		return nil
	}

	// Interleaved scan path (multiple components)
	// Process ALL MCUs - critical for progressive mode
	for mby := 0; mby < d.mbHeight; mby++ {
		for mbx := 0; mbx < d.mbWidth; mbx++ {
			// Process all components for this MCU
			for i := 0; i < nCompScan; i++ {
				compIndex := scanComp[i]
				c := &d.comp[compIndex]

				for sby := 0; sby < c.ssY; sby++ {
					for sbx := 0; sbx < c.ssX; sbx++ {
						by := mby*c.ssY + sby
						bx := mbx*c.ssX + sbx

						if by >= c.nBlocksY || bx >= c.nBlocksX {
							continue
						}

						blockIndex := by*c.nBlocksX + bx
						coeffOffset := blockIndex * 64

						if coeffOffset+64 > len(c.coeffs) {
							continue
						}

						// Process the block
						if ah == 0 {
							// First pass - decode DC coefficient difference
							var diff int

							// Check for data exhaustion BEFORE attempting to decode
							if d.markerHit || (d.size == 0 && d.bufBits == 0) {
								// No data left - use diff=0
								diff = 0
							} else {
								// Save current predictor in case decode corrupts it
								oldPred := c.dcPred

								// Try to decode the DC coefficient
								dcVLC := d.dcVlcTab[c.dcTabSel]
								dcVLC8 := d.dcVlcTab8[c.dcTabSel]
								diff = d.getVLC(dcVLC, dcVLC8, nil)

								// If getVLC corrupted the predictor (shouldn't happen but check anyway)
								if c.dcPred != oldPred {
									c.dcPred = oldPred // Restore it
								}
							}

							// Update predictor and store coefficient
							c.dcPred += diff
							c.coeffs[coeffOffset] = int32(c.dcPred) << al
						} else {
							// Refinement pass
							if d.markerHit || (d.size == 0 && d.bufBits == 0) {
								// Skip refinement if no data
								continue
							}

							bit := d.getBit()
							if bit > 0 {
								c.coeffs[coeffOffset] |= int32(1 << al)
							}
						}
					}
				}
			}

			// Handle restart markers
			if d.rstInterval > 0 && !d.markerHit && d.size > 0 {
				rstCount--
				if rstCount == 0 {
					if !d.processRestart(&nextRst, &rstCount, ah, nCompScan, scanComp) {
						// Restart failed
					}
				}
			}
		}
	}

	d.alignAndRewind()

	return nil
}

// decodeBlockDCProgressive decodes the DC coefficient (or refines it) for a progressive scan.
func (d *decoder) decodeBlockDCProgressive(c *component, offset int, ah, al int) (diff int, terminated bool) {
	// Check bounds
	if offset+64 > len(c.coeffs) {
		return 0, true // Terminate on bounds error
	}

	if ah == 0 {
		// First pass - decode DC coefficient difference
		dcVLC := d.dcVlcTab[c.dcTabSel]
		dcVLC8 := d.dcVlcTab8[c.dcTabSel]

		// Check for complete data exhaustion before attempting decode
		if d.size == 0 && d.bufBits == 0 && !d.markerHit {
			// Completely out of data - use zero difference but don't terminate
			// Store with zero difference
			c.coeffs[offset] = int32(c.dcPred) << al
			return 0, false // Don't terminate, let caller continue with remaining blocks
		}

		// Try to decode
		diff = d.getVLC(dcVLC, dcVLC8, nil)

		// Check if marker was hit
		if d.markerHit {
			// Use zero difference for this block
			c.coeffs[offset] = int32(c.dcPred) << al
			return 0, false // Don't terminate immediately, process remaining blocks with zero
		}

		// Check if we got an incomplete code at EOF
		if diff == 0 && d.size == 0 && d.bufBits < 16 && !d.markerHit {
			// Likely incomplete code - use zero difference
			c.coeffs[offset] = int32(c.dcPred) << al
			return 0, false // Continue with remaining blocks
		}

		// Successfully decoded - update predictor and store
		c.dcPred += diff
		c.coeffs[offset] = int32(c.dcPred) << al

		return diff, false
	}

	// Refinement pass
	bit := d.getBit()

	if bit < 0 {
		if d.markerHit {
			return 0, false // Don't terminate, skip refinement for remaining
		}
		// EOF - skip this block's refinement
		return 0, false
	}

	if bit > 0 {
		c.coeffs[offset] |= int32(1 << al)
	}

	return 0, false
}

// decodeScanBaseline decodes a baseline JPEG scan.
func (d *decoder) decodeScanBaseline(nCompScan int, scanComp [4]int) error {
	rstCount := d.rstInterval
	nextRst := 0

	// Reset DC predictors at the start of the scan.
	for k := 0; k < d.ncomp; k++ {
		d.comp[k].dcPred = 0
	}

	// Reset EOB run for baseline.
	d.eobRun = 0

	// Calculate output block dimensions based on scaling
	outBlockW := 8 / d.scaleDenom
	outBlockH := 8 / d.scaleDenom

	for mby := 0; mby < d.mbHeight; mby++ {
		for mbx := 0; mbx < d.mbWidth; mbx++ {
			for i := 0; i < nCompScan; i++ {
				compIndex := scanComp[i]
				c := &d.comp[compIndex]

				for sby := 0; sby < c.ssY; sby++ {
					for sbx := 0; sbx < c.ssX; sbx++ {
						// Compute offset for scaled output blocks
						rowStart := (mby*c.ssY + sby) * outBlockH * c.stride
						colStart := (mbx*c.ssX + sbx) * outBlockW
						offset := rowStart + colStart

						d.decodeBlock(c, offset)
					}
				}
			}

			// Handle restart markers.
			if d.rstInterval != 0 {
				rstCount--
				if rstCount == 0 {
					// Per spec, the bitstream is byte-aligned before a restart marker.
					// We reset the bit buffer; d.pos is already at the correct byte boundary.
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

					// Reset state for the next interval.
					nextRst = (nextRst + 1) & 7
					rstCount = d.rstInterval

					for k := 0; k < d.ncomp; k++ {
						d.comp[k].dcPred = 0
					}
				}
			}
		}
	}

	// After all MCUs are decoded, we must finalize the bitstream synchronization.
	d.alignAndRewind()

	return nil
}

// decodeScanProgressiveAC handles an AC scan in a progressive JPEG.
func (d *decoder) decodeScanProgressiveAC(nCompScan int, scanComp [4]int, ss, se, ah, al int) error {
	// AC scans must be non-interleaved (nCompScan=1).
	if nCompScan != 1 {
		return ErrInternal
	}

	rstCount := d.rstInterval
	nextRst := 0

	var decodeFunc func(*component, int, int, int, int, int, int)
	if ah == 0 {
		decodeFunc = d.decodeBlockACFirst
	} else {
		decodeFunc = d.decodeBlockACRefine
	}

	c := &d.comp[scanComp[0]]
	totalBlocks := c.nBlocksX * c.nBlocksY

	d.ensureACTouchedSize(totalBlocks)

	blockIndex := 0
	blocksExplicitlyProcessed := 0
	eobRunsApplied := 0

	// Process all blocks using actual component dimensions
	for blockIndex < totalBlocks {
		coeffOffset := blockIndex * 64

		// Bounds check for coefficient buffer
		if coeffOffset+64 > len(c.coeffs) {
			break
		}

		// Check if we have an active EOB run
		if d.eobRun > 0 {
			// Apply EOB run - this block gets EOB processing
			if ah > 0 {
				// For refinement passes, we MUST refine existing non-zero coefficients
				// even if scan data is exhausted. getBit() will return 0 when out of data,
				// which is the correct behavior per JPEG standard.
				d.refineBlockEOB(c, coeffOffset, ss, se, al)
			}
			// For first pass (ah == 0), coefficients remain zero (already initialized)

			d.eobRun--
			eobRunsApplied++

			// Do NOT use 'continue' here. We must fall through to the restart marker check.
		} else {
			// No active EOB run - need to decode this block

			// Check if marker was already hit
			if false { // BUG FIX: removed markerHit check to allow decoding with padded bits
				// Marker hit - treat remaining blocks as having EOB
				if ah > 0 {
					// Refinement pass - refine existing non-zero coefficients using zero bits
					d.refineBlockEOB(c, coeffOffset, ss, se, al)
				}
			} else {
				// No marker hit - try to decode the block
				remainingBlocks := totalBlocks - blockIndex

				// Try to decode this block
				decodeFunc(c, coeffOffset, ss, se, al, blockIndex, remainingBlocks)

				blocksExplicitlyProcessed++
			}
		}

		blockIndex++

		// Handle restart markers (checked after every block)
		if d.rstInterval > 0 && !d.markerHit {
			rstCount--
			if rstCount == 0 {
				// AC scans are always single component (nCompScan=1).
				if !d.processRestart(&nextRst, &rstCount, ah, 1, scanComp) {
					// Termination requested (error or EOF).
					// processRestart already set d.markerHit=true.
				}
			}
		}
	}

	// Clear any remaining EOB run for next scan
	if d.eobRun > 0 {
		d.eobRun = 0
	}

	d.alignAndRewind()

	return nil
}

// decodeBlockACFirst handles the first pass (Ah=0) AC coefficient decoding.
func (d *decoder) decodeBlockACFirst(c *component, offset, ss, se, al, blockIndex, remainingBlocks int) {
	// Check bounds before accessing coeffs
	if offset+64 > len(c.coeffs) {
		return
	}

	acVLC8 := d.acVlcTab8[c.acTabSel]
	acVLC := d.acVlcTab[c.acTabSel]
	coefs := c.coeffs[offset : offset+64 : offset+64]

	for k := ss; k <= se; {
		// Ultra-fast path with 8-bit lookup (like stdlib)
		var symbol int

		// Fast path: we have >=8 bits, no marker hit, and 8-bit table exists
		if d.bufBits >= 8 && !d.markerHit && acVLC8 != nil {
			value8 := uint8((d.buf >> (d.bufBits - 8)) & 0xFF)
			entry := (*acVLC8)[value8]

			if entry != 0 {
				// Valid code found in 8-bit table
				codeLen := int(entry&0xFF) - 1
				d.bufBits -= codeLen
				symbol = int(entry >> 8)
			} else {
				// Code longer than 8 bits, use slow path
				symbol = d.getHuffSymbol(acVLC, blockIndex, c.id)
			}
		} else {
			symbol = d.getHuffSymbol(acVLC, blockIndex, c.id)
		}

		s := symbol & 0x0F
		r := symbol >> 4

		if s == 0 {
			if r == 15 { // ZRL (Zero Run Length)
				k += 16
				continue
			}

			// EOB (End of Block) run
			run := 1 << r
			if r > 0 {
				// Read EOB run length bits
				bits := d.getBits(int(r))

				if d.markerHit {
					// If marker hit while reading EOB run length bits, treat as simple EOB
					return
				}

				run += bits
			}

			// Clamp EOB run to the number of remaining blocks. (JPEG standard Annex G.2.3)
			if run > remainingBlocks {
				run = remainingBlocks
			}

			// The EOB run includes the current block
			// If run=819 at block 205, it covers blocks 205-1023 (819 blocks)
			// So we set eobRun to run-1 because the current block is already being processed
			d.eobRun = run - 1

			return
		}

		k += r
		if k > se {
			return
		}

		// Read coefficient value
		val := d.getBits(s)

		if val < (1 << (s - 1)) {
			val += ((-1) << s) + 1
		}

		nat := zz[k]
		coefs[nat] = int32(val << al)

		if d.acTouched != nil && blockIndex < len(d.acTouched) {
			d.acTouched[blockIndex] = true
		}

		k++
	}
}

// decodeBlockACRefine handles the refinement pass (Ah>0) AC coefficient decoding.
func (d *decoder) decodeBlockACRefine(c *component, offset, ss, se, al, blockIndex, remainingBlocks int) {
	// Check bounds before accessing coeffs
	if offset+64 > len(c.coeffs) {
		return
	}

	acVLC := d.acVlcTab[c.acTabSel]
	acVLC8 := d.acVlcTab8[c.acTabSel]
	delta := int32(1 << al)
	coefs := c.coeffs[offset : offset+64 : offset+64]

	if err := d.refineBlock(coefs, acVLC, acVLC8, int32(ss), int32(se), delta, blockIndex, remainingBlocks, false); err != nil {
		// Error handling (e.g., if refineBlock returned an error, though currently it only returns nil)
		return
	}

	if d.acTouched != nil && blockIndex < len(d.acTouched) {
		// Check if any coefficients in the spectral range were modified
		for k := ss; k <= se; k++ {
			if coefs[zz[k]] != 0 {
				d.acTouched[blockIndex] = true
				break
			}
		}
	}
}

// refineBlock implements progressive AC refinement working directly on coefficient slice.
func (d *decoder) refineBlock(coefs []int32, vlc *[65536]vlcCode, vlc8 *[256]vlcCode8, zigStart, zigEnd, delta int32, blockIndex, remainingBlocks int, trace bool) error {
	zig := zigStart

	// This block is not in an EOB run, so decode its coefficient data.
	newCoeffs := 0
	refinements := 0

loop:
	for zig <= zigEnd {
		z := int32(0)

		// Ultra-fast path with 8-bit lookup
		var symbol int

		if d.bufBits >= 8 && !d.markerHit && vlc8 != nil {
			value8 := uint8((d.buf >> (d.bufBits - 8)) & 0xFF)
			entry := (*vlc8)[value8]

			if entry != 0 {
				codeLen := int(entry&0xFF) - 1
				d.bufBits -= codeLen
				symbol = int(entry >> 8)
			} else {
				symbol = d.getHuffSymbol(vlc, blockIndex, -1)
			}
		} else {
			symbol = d.getHuffSymbol(vlc, blockIndex, -1)
		}

		// If getHuffSymbol hit a marker, it might return 0 or a partial symbol. We proceed.

		val0 := int32(symbol >> 4)   // run of zeroes
		val1 := int32(symbol & 0x0f) // category/size

		switch val1 {
		case 0: // EOB or ZRL
			if val0 != 0x0f { // EOB
				eobRun := int32(1) << val0
				if val0 > 0 {
					bits := d.getBits(int(val0))
					if false { // BUG FIX: removed markerHit check to allow decoding with padded bits
						// If marker hit during EOB run bits read, the scan must terminate.
						return nil
					}

					// Use += for standard compliance.
					eobRun += int32(bits)
				}

				maxRun := int32(remainingBlocks)
				if eobRun > maxRun {
					eobRun = maxRun // Clamp EOB run (JPEG standard Annex G.2.3)
				}

				d.eobRun = int(eobRun - 1)

				break loop
			}
			// ZRL - fall through to refine existing coefficients
		case 1: // New non-zero coefficient
			bit := d.getBit()
			z = delta
			if bit == 0 {
				z = -z
			}

			newCoeffs++
		default:
			// Invalid symbol for a refinement scan
			// We continue processing as best effort, though the stream is technically invalid.
			continue loop
		}

		// Process run of zeros and refine existing non-zero coefficients
		// JPEG Spec: R (nz) counts ZERO coefficients to skip.
		// After skipping R zeros, place the new coefficient at the next zero position.
		// We refine non-zero coefficients as we encounter them.
		nz := val0
		for zig <= zigEnd {
			u := zz[zig]
			if coefs[u] == 0 {
				if nz == 0 {
					// Found target zero position - place coefficient here
					break
				}
				nz--
			} else {
				// Refine existing non-zero coefficient
				// getBit() returns 0 gracefully if markerHit or EOF
				bit := d.getBit()
				if bit != 0 {
					if coefs[u] >= 0 {
						coefs[u] += delta
					} else {
						coefs[u] -= delta
					}
					refinements++
				}
			}
			zig++
		}

		if zig > zigEnd {
			break loop
		}

		// Set new coefficient if we have one
		if z != 0 {
			coefs[zz[zig]] = z
		}

		zig++
	}

	// Refine any remaining non-zero coefficients in this block (if loop broke due to EOB run starting)
	// This refinement must happen before the EOB run takes effect for the subsequent blocks.
	for zig <= zigEnd {
		u := zz[zig]
		if coefs[u] != 0 {
			// getBit() returns 0 gracefully if markerHit or EOF
			bit := d.getBit()
			if bit != 0 {
				if coefs[u] >= 0 {
					coefs[u] += delta
				} else {
					coefs[u] -= delta
				}
				refinements++
			}
		}
		zig++
	}

	return nil
}

// refineNonZeroes implements progressive AC refinement for existing nonzero coefficients.
func (d *decoder) refineNonZeroes(coefs []int32, zig, zigEnd, nz, delta int32, blockIndex int, trace bool) (int32, error) {
	for zig <= zigEnd {
		u := zz[zig]
		if coefs[u] == 0 {
			if nz == 0 {
				break
			}
			nz--
		} else {
			bit := d.getBit()

			// Check markerHit after getBit.
			if d.markerHit {
				return zig, nil
			}

			if bit != 0 {
				if coefs[u] >= 0 {
					coefs[u] += delta
				} else {
					coefs[u] -= delta
				}
			}
		}

		zig++
	}

	return zig, nil
}

// refineBlockEOB refines existing non-zero coefficients in a block during EOB run processing
func (d *decoder) refineBlockEOB(c *component, offset, ss, se, al int) {
	if offset+64 > len(c.coeffs) {
		return
	}

	coefs := c.coeffs[offset : offset+64 : offset+64]
	delta := int32(1 << al)

	// Refine existing non-zero coefficients in the spectral range
	// getBit() returns 0 gracefully if markerHit or EOF
	for k := ss; k <= se; k++ {
		nat := zz[k]
		if coefs[nat] != 0 {
			bit := d.getBit()
			if bit != 0 {
				if coefs[nat] >= 0 {
					coefs[nat] += delta
				} else {
					coefs[nat] -= delta
				}
			}
		}
	}
}

// postProcessProgressive performs dequantization and IDCT after all progressive scans are complete.
func (d *decoder) postProcessProgressive() error {
	// Calculate output block dimensions based on scaling
	outBlockW := 8 / d.scaleDenom
	outBlockH := 8 / d.scaleDenom

	for i := 0; i < d.ncomp; i++ {
		c := &d.comp[i]
		qt := d.qtab[c.qtSel]

		// Stride already computed in decodeSOF with scaled block width
		// c.stride = c.nBlocksX * outBlockW

		// Use c.nBlocksY * outBlockH as the padded height for scaled output
		paddedHeight := c.nBlocksY * outBlockH
		pixelSize := c.stride * paddedHeight

		if c.pixels == nil && pixelSize > 0 {
			c.pixels = make([]byte, pixelSize)
		}

		if len(c.coeffs) == 0 {
			continue
		}

		// Process all blocks using 2D iteration to avoid division
		blocksProcessed := 0

		for by := 0; by < c.nBlocksY; by++ {
			pixelY := by * outBlockH

			// Skip entire row if outside component height
			if pixelY >= c.height {
				continue
			}

			for bx := 0; bx < c.nBlocksX; bx++ {
				pixelX := bx * outBlockW

				// Skip blocks outside component width
				if pixelX >= c.width {
					continue
				}

				// Use simple linear coefficient indexing
				blockIndex := by*c.nBlocksX + bx
				coeffOffset := blockIndex * 64

				// Bounds check for coefficient buffer
				if coeffOffset+64 > len(c.coeffs) {
					continue
				}

				// Clear work block and dequantize
				d.block = [64]int32{} // Fast zero initialization
				coefs := c.coeffs[coeffOffset : coeffOffset+64]
				// Unrolled loop for better performance
				for k := 0; k < 64; k += 4 {
					if c0 := coefs[k]; c0 != 0 {
						d.block[k] = c0 * int32(qt[k])
					}
					if c1 := coefs[k+1]; c1 != 0 {
						d.block[k+1] = c1 * int32(qt[k+1])
					}
					if c2 := coefs[k+2]; c2 != 0 {
						d.block[k+2] = c2 * int32(qt[k+2])
					}
					if c3 := coefs[k+3]; c3 != 0 {
						d.block[k+3] = c3 * int32(qt[k+3])
					}
				}

				// Apply IDCT to pixel buffer
				if len(c.pixels) > 0 {
					outOffset := pixelY*c.stride + pixelX

					// Bounds check for pixel buffer
					if outOffset >= 0 && outOffset < len(c.pixels) &&
						pixelY < paddedHeight && pixelX < c.stride { // Use padded bounds for safety
						// Apply scaled IDCT
						idctScaled(&d.block, c.pixels, outOffset, c.stride, d.scaleDenom)
					}
				}

				blocksProcessed++
			}
		}

		// Free coefficients to save memory
		c.coeffs = nil
	}

	return nil
}
