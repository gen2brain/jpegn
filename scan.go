package jpegn

// Entropy Decoding

// getHuffSymbol decodes a Huffman symbol from the bitstream without reading
// subsequent value bits. It returns 0 (EOB) if no valid code is found.
func (d *decoder) getHuffSymbol(t *huffTable) int {
	if d.bufBits < 16 && !d.markerHit {
		d.showBits(16)
	}

	var look uint32
	if d.bufBits >= 16 {
		look = uint32((d.buf >> (d.bufBits - 16)) & 0xFFFF)
	} else if d.bufBits > 0 {
		sh := uint(16 - d.bufBits)
		look = uint32(((d.buf << sh) | ((uint64(1) << sh) - 1)) & 0xFFFF)
	} else {
		return 0
	}

	var huffBits int
	var huffCode uint8
	if e := t.lut[look>>8]; e != 0 {
		huffBits = int(e & 0xFF)
		huffCode = uint8(e >> 8)
	} else {
		var ok bool
		huffBits, huffCode, ok = t.decodeLong(look)
		if !ok {
			return 0
		}
	}

	if d.bufBits < huffBits {
		return 0
	}

	d.bufBits -= huffBits

	return int(huffCode)
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
	dcHuff := d.dcHuff[c.dcTabSel]
	acHuff := d.acHuff[c.acTabSel]

	// Decode DC coefficient.
	value = d.getVLC(dcHuff, nil)
	// Note: Baseline scans should generally not hit markers during getVLC unexpectedly.
	// If they do, we might proceed with potentially corrupted data if d.markerHit isn't checked here.

	c.dcPred += value
	d.block[0] = int32(c.dcPred) * int32(qt[0])

	// Decode AC coefficients.
	coef := 1 // coef is the zigzag index.
	for coef <= 63 {
		value = d.getVLC(acHuff, &code)

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

	for i := 0; i < nCompScan; i++ {
		scanID := int(d.jpegData[d.pos])

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

		// Non-interleaved: iterate the true block raster (no MCU padding),
		// addressing the buffer with the nBlocksX stride.
	dcBlocks:
		for by := 0; by < c.blocksPerCol; by++ {
			for bx := 0; bx < c.blocksPerLine; bx++ {
				coeffOffset := (by*c.nBlocksX + bx) * 64

				// Bounds check
				if coeffOffset+64 > len(c.coeffs) {
					break dcBlocks
				}

				// Decode DC coefficient
				if ah == 0 {
					// First pass - decode DC coefficient difference
					var diff int

					// Try to decode the DC coefficient
					diff = d.getVLC(d.dcHuff[c.dcTabSel], nil)

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
								// Try to decode the DC coefficient
								diff = d.getVLC(d.dcHuff[c.dcTabSel], nil)
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

// decodeScanBaseline decodes a baseline JPEG scan.
func (d *decoder) decodeScanBaseline(nCompScan int, scanComp [4]int) error {
	// Reset DC predictors at the start of the scan.
	for k := 0; k < d.ncomp; k++ {
		d.comp[k].dcPred = 0
	}

	// Reset EOB run for baseline.
	d.eobRun = 0

	// Calculate output block dimensions based on scaling
	outBlockW := 8 / d.scaleDenom
	outBlockH := 8 / d.scaleDenom

	decodeMCU := func(mbx, mby int) {
		for i := 0; i < nCompScan; i++ {
			c := &d.comp[scanComp[i]]

			for sby := 0; sby < c.ssY; sby++ {
				for sbx := 0; sbx < c.ssX; sbx++ {
					rowStart := (mby*c.ssY + sby) * outBlockH * c.stride
					colStart := (mbx*c.ssX + sbx) * outBlockW
					d.decodeBlock(c, rowStart+colStart)
				}
			}
		}
	}

	if d.rstInterval > 0 {
		d.decodeScanBaselineRestart(decodeMCU)
	} else {
		for mby := 0; mby < d.mbHeight; mby++ {
			for mbx := 0; mbx < d.mbWidth; mbx++ {
				decodeMCU(mbx, mby)
			}
		}
	}

	// After all MCUs are decoded, we must finalize the bitstream synchronization.
	d.alignAndRewind()

	return nil
}

// decodeScanBaselineRestart decodes a baseline scan that uses restart intervals.
// A corrupt interval is recovered by resyncing to the next restart marker rather
// than aborting the whole image (libjpeg-style error concealment).
func (d *decoder) decodeScanBaselineRestart(decodeMCU func(mbx, mby int)) {
	total := d.mbWidth * d.mbHeight
	nextRst := 0

	for mcu := 0; mcu < total; {
		end := mcu + d.rstInterval
		if end > total {
			end = total
		}

		mcu = d.decodeBaselineInterval(mcu, end, decodeMCU)
		if mcu >= total {
			break
		}

		if !d.resyncToRestart(&nextRst) {
			break
		}
	}
}

// decodeBaselineInterval decodes MCUs [start, end), recovering from an entropy
// error by abandoning the rest of the interval. It returns the next MCU index.
func (d *decoder) decodeBaselineInterval(start, end int, decodeMCU func(mbx, mby int)) (next int) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(errDecode); ok {
				next = end

				return
			}

			panic(r)
		}
	}()

	for mcu := start; mcu < end; mcu++ {
		decodeMCU(mcu%d.mbWidth, mcu/d.mbWidth)
	}

	return end
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

	// Non-interleaved: iterate the true block raster (no MCU padding). blockIndex
	// is the sequential scan position (for EOB-run clamping); the buffer uses the
	// nBlocksX stride.
	totalBlocks := c.blocksPerLine * c.blocksPerCol

	blockIndex := 0
	blocksExplicitlyProcessed := 0
	eobRunsApplied := 0

acBlocks:
	for by := 0; by < c.blocksPerCol; by++ {
		for bx := 0; bx < c.blocksPerLine; bx++ {
			coeffOffset := (by*c.nBlocksX + bx) * 64

			// Bounds check for coefficient buffer
			if coeffOffset+64 > len(c.coeffs) {
				break acBlocks
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
				// No active EOB run - try to decode the block.
				remainingBlocks := totalBlocks - blockIndex

				decodeFunc(c, coeffOffset, ss, se, al, blockIndex, remainingBlocks)

				blocksExplicitlyProcessed++
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

	acHuff := d.acHuff[c.acTabSel]
	coefs := c.coeffs[offset : offset+64 : offset+64]

	for k := ss; k <= se; {
		// Ultra-fast path: 8-bit look-ahead, then the long-code path.
		var symbol int

		if d.bufBits >= 8 && !d.markerHit {
			value8 := (d.buf >> (d.bufBits - 8)) & 0xFF
			if entry := acHuff.lut[value8]; entry != 0 {
				d.bufBits -= int(entry & 0xFF)
				symbol = int(entry >> 8)
			} else {
				symbol = d.getHuffSymbol(acHuff)
			}
		} else {
			symbol = d.getHuffSymbol(acHuff)
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

		k++
	}
}

// decodeBlockACRefine handles the refinement pass (Ah>0) AC coefficient decoding.
func (d *decoder) decodeBlockACRefine(c *component, offset, ss, se, al, blockIndex, remainingBlocks int) {
	// Check bounds before accessing coeffs
	if offset+64 > len(c.coeffs) {
		return
	}

	acHuff := d.acHuff[c.acTabSel]
	delta := int32(1 << al)
	coefs := c.coeffs[offset : offset+64 : offset+64]

	if err := d.refineBlock(coefs, acHuff, int32(ss), int32(se), delta, blockIndex, remainingBlocks, false); err != nil {
		// Error handling (e.g., if refineBlock returned an error, though currently it only returns nil)
		return
	}
}

// refineBlock implements progressive AC refinement working directly on coefficient slice.
func (d *decoder) refineBlock(coefs []int32, t *huffTable, zigStart, zigEnd, delta int32, blockIndex, remainingBlocks int, trace bool) error {
	zig := zigStart

	// This block is not in an EOB run, so decode its coefficient data.
	newCoeffs := 0
	refinements := 0

loop:
	for zig <= zigEnd {
		z := int32(0)

		// Ultra-fast path with 8-bit lookup
		var symbol int

		if d.bufBits >= 8 && !d.markerHit {
			value8 := (d.buf >> (d.bufBits - 8)) & 0xFF
			if entry := t.lut[value8]; entry != 0 {
				d.bufBits -= int(entry & 0xFF)
				symbol = int(entry >> 8)
			} else {
				symbol = d.getHuffSymbol(t)
			}
		} else {
			symbol = d.getHuffSymbol(t)
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
				// Branchless dequant; writing all 64 also zero-fills, no clear needed.
				coefs := c.coeffs[coeffOffset : coeffOffset+64]
				for k := 0; k < 64; k++ {
					d.block[k] = coefs[k] * int32(qt[k])
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
