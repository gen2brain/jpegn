//go:build amd64 && !noasm

package jpegn

//go:noescape
func fdctAVX2(blk *[64]int32, src *byte, stride int)

//go:noescape
func fdctSSE(blk *[64]int32, src *byte, stride int)

// fdct performs a full 8x8 2D FDCT.
func fdct(blk *[64]int32, src []byte, stride int) {
	switch {
	case hasAVX2:
		fdctAVX2(blk, &src[0], stride)

		return
	case hasSSE4:
		fdctSSE(blk, &src[0], stride)

		return
	}

	fdctScalar(blk, src, stride)
}
