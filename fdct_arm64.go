//go:build arm64 && !noasm

package jpegn

//go:noescape
func fdctNEON(blk *[64]int32)

// fdct performs a full 8x8 2D FDCT.
func fdct(blk *[64]int32) {
	fdctNEON(blk)
}
