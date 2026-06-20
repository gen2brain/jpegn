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
