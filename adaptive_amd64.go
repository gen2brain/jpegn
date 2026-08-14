//go:build amd64 && !noasm

package jpegn

//go:noescape
func preErosionAVX2(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)

//go:noescape
func preErosionSSE(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)

// preErosionRow accumulates one row of local pixel differences into dst.
func preErosionRow(dst []float32, row, rowT, rowB []byte, acc bool) {
	n := len(dst)

	mask := uint32(0)
	if acc {
		mask = 0xFFFFFFFF
	}

	var mid int

	switch {
	case hasAVX2 && n >= 18:
		mid = (n - 2) &^ 7
		preErosionAVX2(&dst[1], &row[1], &rowT[1], &rowB[1], &gammaDiffLUT[0], mid, mask)
	case hasSSE4 && n >= 10:
		mid = (n - 2) &^ 3
		preErosionSSE(&dst[1], &row[1], &rowT[1], &rowB[1], &gammaDiffLUT[0], mid, mask)
	default:
		preErosionRowScalar(dst, row, rowT, rowB, acc)

		return
	}

	preErosionRowEdge(dst, row, rowT, rowB, acc, 0, 1)
	preErosionRowEdge(dst, row, rowT, rowB, acc, mid+1, n)
}
