//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func upsampleVMiddleRVV(dst1, dst2, src *byte, stride, n int)

//go:noescape
func upsampleHMiddleRVV(dst, src *byte, n int)

// upsampleHRows writes the 2x horizontal Catmull-Rom upsampling of h rows.
func upsampleHRows(dst, src []byte, w, h, dstStride, srcStride int) {
	if w < 11 {
		upsampleHRowsScalar(dst, src, w, h, dstStride, srcStride)

		return
	}

	for y := 0; y < h; y++ {
		in := src[y*srcStride:]
		o := dst[y*dstStride:]

		upsampleHEdges(in, o, w)
		upsampleHMiddleRVV(&o[3], &in[0], w-3)
	}
}

// upsampleVRowPair writes one interior output row pair from the four source rows at src.
func upsampleVRowPair(src, out1, out2 []byte, w, stride int) {
	if w < 8 {
		upsampleVMiddleRowPair(src, out1, out2, w, stride, 0)

		return
	}

	upsampleVMiddleRVV(&out1[0], &out2[0], &src[0], stride, w)
}

// upsampleNearestNeighborRows writes each of h source rows doubled horizontally
// into two consecutive destination rows.
func upsampleNearestNeighborRows(dst, src []byte, w, h, dstStride, srcStride int) {
	upsampleNearestNeighborRowsScalar(dst, src, w, h, dstStride, srcStride)
}
