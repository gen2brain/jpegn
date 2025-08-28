//go:build !amd64 || noasm

package jpegn

// idct performs a full 8x8 2D IDCT using the iterative approach (fallback).
func idct(blk *[64]int32, out []byte, outOffset int, stride int) {
	for i := 0; i < 64; i += 8 {
		rowIdct(blk, i)
	}

	for i := 0; i < 8; i++ {
		colIdct(blk, i, out, outOffset+i, stride)
	}
}
