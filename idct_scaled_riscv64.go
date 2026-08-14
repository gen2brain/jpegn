//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func idct4x4RVV(blk *[64]int32, out *byte, stride int, scratch *[16]int32)

// idctScaled dispatches on the scale denominator, which must be 1, 2, 4 or 8.
func idctScaled(blk *[64]int32, out []byte, outOffset int, stride int, scaleDenom int) {
	switch scaleDenom {
	case 2:
		var scratch [16]int32

		idct4x4RVV(blk, &out[outOffset], stride, &scratch)
	case 4:
		idct8x8To2x2(blk, out, outOffset, stride)
	case 8:
		idct8x8To1x1(blk, out, outOffset, stride)
	default:
		idct(blk, out, outOffset, stride)
	}
}
