//go:build (!amd64 && !arm64) || noasm

package jpegn

// fdct performs a full 8x8 2D FDCT (fallback).
func fdct(blk *[64]int32) {
	fdctScalar(blk)
}
