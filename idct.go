//go:build (!amd64 && !arm64) || noasm

package jpegn

// idct performs a full 8x8 2D IDCT using the iterative approach (fallback).
func idct(blk *[64]int32, out []byte, outOffset int, stride int) {
	idctIterative(blk, out, outOffset, stride)
}
