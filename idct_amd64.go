//go:build amd64 && !noasm

package jpegn

//go:noescape
func idctAVX2(in *[64]int32, out []byte, offset int, stride int)

// idct uses the optimized assembly implementation on AMD64.
func idct(blk *[64]int32, out []byte, outOffset int, stride int) {
	// Bounds check for safety before calling assembly. The assembly implementation assumes valid pointers.
	// We need enough space for 8 rows: outOffset + 7*stride + 8 bytes.
	if isAVX2 && len(out) > 0 && outOffset >= 0 && stride > 0 && len(out)-outOffset >= 7*stride+8 {
		idctAVX2(blk, out, outOffset, stride)

		return
	}

	// Fallback to pure Go if bounds check fails (e.g., corrupted JPEG dimensions/offsets).
	idctIterative(blk, out, outOffset, stride)
}

// idctIterative is a helper for the iterative fallback (using pure Go functions).
func idctIterative(blk *[64]int32, out []byte, outOffset int, stride int) {
	for i := 0; i < 64; i += 8 {
		rowIdct(blk, i)
	}

	for i := 0; i < 8; i++ {
		colIdct(blk, i, out, outOffset+i, stride)
	}
}
