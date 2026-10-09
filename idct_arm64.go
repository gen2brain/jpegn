//go:build arm64 && !noasm

package jpegn

//go:noescape
func idctNEON(in *[64]int32, out []byte, offset int, stride int) bool

// idct uses the optimized NEON assembly implementation on ARM64.
func idct(blk *[64]int32, out []byte, outOffset int, stride int) {
	// Bounds check for safety before calling assembly. The assembly implementation assumes valid pointers and slice capacity.
	// We need enough space for 8 rows: outOffset + 7*stride + 8 bytes.
	if len(out) > 0 && outOffset >= 0 && stride > 0 && len(out)-outOffset >= 7*stride+8 {
		if idctNEON(blk, out, outOffset, stride) {
			return
		}
	}

	// Fallback to pure Go if bounds check fails, the output slice is empty, or
	// the kernel rejects a block that could wrap its int32 lanes.
	idctIterative(blk, out, outOffset, stride)
}
