//go:build noasm || (!amd64 && !arm64 && !(riscv64 && riscv64.rva23u64))

package jpegn

func upsampleHRows(dst, src []byte, w, h, dstStride, srcStride int) {
	upsampleHRowsScalar(dst, src, w, h, dstStride, srcStride)
}

func upsampleVRowPair(src, out1, out2 []byte, w, stride int) {
	upsampleVMiddleRowPair(src, out1, out2, w, stride, 0)
}

func upsampleNearestNeighborRows(dst, src []byte, w, h, dstStride, srcStride int) {
	upsampleNearestNeighborRowsScalar(dst, src, w, h, dstStride, srcStride)
}
