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
			// Reject oversubscribed tables (Kraft inequality) before generating
			// any codes, so the LUT fill below cannot index out of range.
			if code+cnt > (1 << l) {
				return ErrSyntax
			}

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
		}

		code <<= 1
	}

	if k != nSymbols {
		return ErrSyntax
	}

	return nil
}

// huffEncTable maps a symbol to its canonical code and length.
type huffEncTable struct {
	code [256]uint32
	size [256]uint8
}

// buildHuffEnc builds an encoding table from DHT counts and values.
func buildHuffEnc(t *huffEncTable, bits *[17]uint8, values []byte) {
	*t = huffEncTable{}

	code := uint32(0)
	k := 0

	for l := 1; l <= 16; l++ {
		for i := 0; i < int(bits[l]); i++ {
			if k >= len(values) {
				return
			}

			sym := values[k]
			t.code[sym] = code
			t.size[sym] = uint8(l)

			code++
			k++
		}

		code <<= 1
	}
}

// maxCodeLen is the working limit before folding back to 16 bits.
const maxCodeLen = 32

// genOptimalTable builds a Huffman table from a histogram per JPEG Annex K.2.
func genOptimalTable(freq *[257]int32, bits *[17]uint8, values *[256]uint8) int {
	var f [257]int32
	copy(f[:], freq[:])
	f[256] = 1

	var others [257]int
	for i := range others {
		others[i] = -1
	}

	var codesize [257]int

	for {
		v1, v2 := -1, -1
		var least1, least2 int32

		for i := 0; i <= 256; i++ {
			if f[i] > 0 && (v1 < 0 || f[i] <= least1) {
				least1 = f[i]
				v1 = i
			}
		}

		for i := 0; i <= 256; i++ {
			if f[i] > 0 && i != v1 && (v2 < 0 || f[i] <= least2) {
				least2 = f[i]
				v2 = i
			}
		}

		if v2 < 0 {
			break
		}

		f[v1] += f[v2]
		f[v2] = 0

		codesize[v1]++
		for others[v1] >= 0 {
			v1 = others[v1]
			codesize[v1]++
		}

		others[v1] = v2

		codesize[v2]++
		for others[v2] >= 0 {
			v2 = others[v2]
			codesize[v2]++
		}
	}

	var count [maxCodeLen + 1]int32
	for i := 0; i <= 256; i++ {
		if l := codesize[i]; l > 0 {
			if l > maxCodeLen {
				l = maxCodeLen
				codesize[i] = l
			}

			count[l]++
		}
	}

	for i := maxCodeLen; i > 16; i-- {
		for count[i] > 0 {
			j := i - 2
			for count[j] == 0 {
				j--
			}

			count[i] -= 2
			count[i-1]++
			count[j+1] += 2
			count[j]--
		}
	}

	i := 16
	for i > 0 && count[i] == 0 {
		i--
	}

	if i == 0 {
		return 0
	}

	count[i]--

	*bits = [17]uint8{}
	for l := 1; l <= 16; l++ {
		bits[l] = uint8(count[l])
	}

	p := 0
	for l := 1; l <= maxCodeLen; l++ {
		for sym := 0; sym < 256; sym++ {
			if codesize[sym] == l {
				values[p] = uint8(sym)
				p++
			}
		}
	}

	return p
}
