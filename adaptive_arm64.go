//go:build arm64 && !noasm

package jpegn

//go:noescape
func preErosionNEON(dst *float32, row, rowT, rowB *byte, lut *float32, n int, acc uint32)

// preErosionRow accumulates one row of local pixel differences into dst.
func preErosionRow(dst []float32, row, rowT, rowB []byte, acc bool) {
	n := len(dst)

	if n < 18 {
		preErosionRowScalar(dst, row, rowT, rowB, acc)

		return
	}

	mid := (n - 2) &^ 7

	mask := uint32(0)
	if acc {
		mask = 0xFFFFFFFF
	}

	preErosionNEON(&dst[1], &row[1], &rowT[1], &rowB[1], &gammaDiffLUT[0], mid, mask)

	preErosionRowEdge(dst, row, rowT, rowB, acc, 0, 1)
	preErosionRowEdge(dst, row, rowT, rowB, acc, mid+1, n)
}
