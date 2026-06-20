//go:build (!amd64 && !arm64) || noasm

package jpegn

// signExtend converts a valBits-wide magnitude into a signed JPEG coefficient.
func signExtend(value, valBits int) int {
	if value < (1 << (valBits - 1)) {
		value += ((-1) << valBits) + 1
	}

	return value
}
