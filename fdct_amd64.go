//go:build amd64 && !noasm

package jpegn

//go:noescape
func fdctAVX2(blk *[64]int32)

// fdct performs a full 8x8 2D FDCT.
func fdct(blk *[64]int32) {
	if isAVX2 {
		fdctAVX2(blk)

		return
	}

	fdctScalar(blk)
}
