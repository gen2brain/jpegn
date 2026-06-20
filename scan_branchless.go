//go:build (amd64 || arm64) && !noasm

package jpegn

// signExtend converts a valBits-wide magnitude into a signed JPEG coefficient.
// This branchless variant avoids a conditional jump in the entropy-decoding hot
// path; amd64 and arm64 both have an efficient arithmetic right shift (and the
// compiler may further lower it to a conditional select).
func signExtend(value, valBits int) int {
	threshold := 1 << (valBits - 1)
	// signBit is -1 when value < threshold, 0 otherwise (int is 64-bit here).
	signBit := (value - threshold) >> 63
	// correction = -(2^valBits - 1) when signBit == -1, else 0.
	return value + (signBit << valBits) - signBit
}
