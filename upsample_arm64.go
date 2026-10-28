//go:build arm64 && !noasm

package jpegn

import (
	"unsafe"
)

//go:noescape
func upsampleNearestNeighborNEON(src, dst unsafe.Pointer, srcW, srcH, srcS, dstS int)

//go:noescape
func upsampleHMiddleNEON(dst, src unsafe.Pointer, n int)

//go:noescape
func upsampleVMiddleNEON(dst1, dst2, src unsafe.Pointer, stride, n int)

// upsampleHRows writes the 2x horizontal Catmull-Rom upsampling of h rows.
// Width >= 11 guarantees at least one full 8-sample NEON block (width-3 >= 8).
func upsampleHRows(dst, src []byte, w, h, dstStride, srcStride int) {
	if w < 11 {
		upsampleHRowsScalar(dst, src, w, h, dstStride, srcStride)

		return
	}

	nbulk := (w - 3) &^ 7

	for y := 0; y < h; y++ {
		in := src[y*srcStride:]
		o := dst[y*dstStride:]

		upsampleHEdges(in, o, w)
		upsampleHMiddleNEON(unsafe.Pointer(&o[3]), unsafe.Pointer(&in[0]), nbulk)
		upsampleHMiddle(in, o, w, nbulk)
	}
}

// upsampleVRowPair writes one interior output row pair from the four source rows at src.
func upsampleVRowPair(src, out1, out2 []byte, w, stride int) {
	nbulk := 0
	if w >= 8 {
		nbulk = w &^ 7
		upsampleVMiddleNEON(unsafe.Pointer(&out1[0]), unsafe.Pointer(&out2[0]), unsafe.Pointer(&src[0]), stride, nbulk)
	}

	upsampleVMiddleRowPair(src, out1, out2, w, stride, nbulk)
}

// upsampleNearestNeighborRows writes each of h source rows doubled horizontally
// into two consecutive destination rows.
func upsampleNearestNeighborRows(dst, src []byte, w, h, dstStride, srcStride int) {
	upsampleNearestNeighborNEON(unsafe.Pointer(&src[0]), unsafe.Pointer(&dst[0]), w, h, srcStride, dstStride)
}
