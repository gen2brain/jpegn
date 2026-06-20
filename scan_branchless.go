//go:build (amd64 || arm64) && !noasm

package jpegn

// signExtend converts a valBits-wide magnitude into a signed JPEG coefficient.
// Branchless variant for amd64/arm64 (int is 64-bit, so the >>63 yields the sign mask).
func signExtend(value, valBits int) int {
	threshold := 1 << (valBits - 1)
	signBit := (value - threshold) >> 63
	return value + (signBit << valBits) - signBit
}
