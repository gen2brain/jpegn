//go:build amd64 && !noasm

package jpegn

import "unsafe"

//go:noescape
func upsampleNearestNeighborAVX2(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

//go:noescape
func upsampleNearestNeighborSSE(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

//go:noescape
func upsampleHAVX2(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleHSSE(dst, src unsafe.Pointer, w, h, dstStride, srcStride int)

//go:noescape
func upsampleVMiddleAVX2(dst1, dst2, src unsafe.Pointer, stride, n int)

//go:noescape
func upsampleVMiddleSSE(dst1, dst2, src unsafe.Pointer, stride, n int)

// upsampleHRows writes the 2x horizontal Catmull-Rom upsampling of h rows. The
// kernels need 3 edge samples plus one full vector block.
func upsampleHRows(dst, src []byte, w, h, dstStride, srcStride int) {
	switch {
	case hasAVX2 && w >= 19:
		upsampleHAVX2(unsafe.Pointer(&dst[0]), unsafe.Pointer(&src[0]), w, h, dstStride, srcStride)

		return
	case hasSSE4 && w >= 11:
		upsampleHSSE(unsafe.Pointer(&dst[0]), unsafe.Pointer(&src[0]), w, h, dstStride, srcStride)

		return
	}

	upsampleHRowsScalar(dst, src, w, h, dstStride, srcStride)
}

// upsampleVRowPair writes one interior output row pair from the four source rows at src.
func upsampleVRowPair(src, out1, out2 []byte, w, stride int) {
	nbulk := 0

	switch {
	case hasAVX2 && w >= 16:
		nbulk = w &^ 15
		upsampleVMiddleAVX2(unsafe.Pointer(&out1[0]), unsafe.Pointer(&out2[0]), unsafe.Pointer(&src[0]), stride, nbulk)
	case hasSSE4 && w >= 8:
		nbulk = w &^ 7
		upsampleVMiddleSSE(unsafe.Pointer(&out1[0]), unsafe.Pointer(&out2[0]), unsafe.Pointer(&src[0]), stride, nbulk)
	}

	upsampleVMiddleRowPair(src, out1, out2, w, stride, nbulk)
}

// upsampleNearestNeighborRows writes each of h source rows doubled horizontally
// into two consecutive destination rows.
func upsampleNearestNeighborRows(dst, src []byte, w, h, dstStride, srcStride int) {
	switch {
	case hasAVX2:
		upsampleNearestNeighborAVX2(unsafe.Pointer(&src[0]), unsafe.Pointer(&dst[0]), w, h, srcStride, dstStride)

		return
	case hasSSE4:
		upsampleNearestNeighborSSE(unsafe.Pointer(&src[0]), unsafe.Pointer(&dst[0]), w, h, srcStride, dstStride)

		return
	}

	upsampleNearestNeighborRowsScalar(dst, src, w, h, dstStride, srcStride)
}
