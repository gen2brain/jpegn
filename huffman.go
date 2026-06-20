package jpegn

// huffLUTBits is the look-ahead size of the Huffman fast-path table.
const huffLUTBits = 8

// huffTable is a canonical Huffman decoding table. Codes up to huffLUTBits are
// resolved with a single direct lookup in lut; longer codes (9..16 bits) use the
// per-length maxcode/delta arrays. This replaces the 256 KB 16-bit direct table:
// it builds far cheaper (a 256-entry LUT instead of 65536 entries) and the hot
// lookup stays in L1.
type huffTable struct {
	// lut maps the next huffLUTBits bits to a code: high 8 bits = symbol value,
	// low 8 bits = code length (1..8). A zero entry means the code is longer than
	// huffLUTBits (use the maxcode path) or invalid.
	lut [1 << huffLUTBits]uint16

	// For codes of length l in [9,16]: maxcode[l] is the largest l-bit code of
	// that length (-1 if none), and the symbol for an l-bit code c is
	// values[c + delta[l]].
	maxcode [17]int32
	delta   [17]int32

	values [256]uint8
}

// decodeLong resolves a code longer than huffLUTBits from a 16-bit (MSB-aligned)
// look-ahead. It returns the code length, the symbol, and whether a valid code
// was found.
func (t *huffTable) decodeLong(look uint32) (int, uint8, bool) {
	for l := 9; l <= 16; l++ {
		code := int32(look >> uint(16-l))
		if code <= t.maxcode[l] {
			idx := code + t.delta[l]
			if idx < 0 || idx >= 256 {
				return 0, 0, false
			}

			return l, t.values[idx], true
		}
	}

	return 0, 0, false
}

// buildHuff builds a huffTable from the DHT counts (codes per length 1..16) and
// the symbol values in canonical order.
func buildHuff(t *huffTable, counts *[16]uint8, values []byte) error {
	nSymbols := 0
	for _, c := range counts {
		nSymbols += int(c)
	}

	if nSymbols > 256 || len(values) < nSymbols {
		return ErrSyntax
	}

	t.lut = [1 << huffLUTBits]uint16{}
	for l := range t.maxcode {
		t.maxcode[l] = -1
	}
	copy(t.values[:nSymbols], values[:nSymbols])

	code := 0
	k := 0 // running symbol index

	for l := 1; l <= 16; l++ {
		cnt := int(counts[l-1])
		if cnt > 0 {
			// symbol index for code c of length l is c + (k - code).
			t.delta[l] = int32(k) - int32(code)

			if l <= huffLUTBits {
				for i := 0; i < cnt; i++ {
					base := code << (huffLUTBits - l)
					n := 1 << (huffLUTBits - l)
					entry := uint16(values[k])<<8 | uint16(l)
					for j := 0; j < n; j++ {
						t.lut[base+j] = entry
					}
					code++
					k++
				}
			} else {
				code += cnt
				k += cnt
				t.maxcode[l] = int32(code - 1)
			}

			// Kraft inequality: codes must not overflow the available space.
			if code > (1 << l) {
				return ErrSyntax
			}
		}

		code <<= 1
	}

	if k != nSymbols {
		return ErrSyntax
	}

	return nil
}
