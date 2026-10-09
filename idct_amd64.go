//go:build amd64 && !noasm

package jpegn

//go:noescape
func idctAVX2(in *[64]int32, out []byte, offset int, stride int) bool

//go:noescape
func idctSSE(in *[64]int32, out []byte, offset int, stride int) bool

// idct uses the optimized assembly implementation on AMD64.
func idct(blk *[64]int32, out []byte, outOffset int, stride int) {
	// Bounds check for safety before calling assembly. The assembly implementation assumes valid pointers.
	// We need enough space for 8 rows: outOffset + 7*stride + 8 bytes.
	if len(out) > 0 && outOffset >= 0 && stride > 0 && len(out)-outOffset >= 7*stride+8 {
		switch {
		case hasAVX2:
			if idctAVX2(blk, out, outOffset, stride) {
				return
			}
		case hasSSE4:
			if idctSSE(blk, out, outOffset, stride) {
				return
			}
		}
	}

	// Fallback to pure Go if bounds check fails (e.g., corrupted JPEG dimensions/offsets)
	// or the kernel rejects a block that could wrap its int32 lanes.
	idctIterative(blk, out, outOffset, stride)
}
