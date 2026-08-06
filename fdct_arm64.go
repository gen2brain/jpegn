//go:build arm64 && !noasm

package jpegn

//go:noescape
func fdctNEON(blk *[64]int32, src *byte, stride int)

// fdct performs a full 8x8 2D FDCT.
func fdct(blk *[64]int32, src []byte, stride int) {
	fdctNEON(blk, &src[0], stride)
}
