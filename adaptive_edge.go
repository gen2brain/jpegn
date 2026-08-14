package jpegn

// preErosionRowEdge covers the clamped columns the kernel leaves.
func preErosionRowEdge(dst []float32, row, rowT, rowB []byte, acc bool, from, to int) {
	n := len(dst)

	for x := from; x < to; x++ {
		xl, xr := x-1, x+1

		if xl < 0 {
			xl = 0
		}

		if xr == n {
			xr = x
		}

		cv := int(row[x])
		d4 := 4*cv - int(row[xr]) - int(row[xl]) - int(rowT[x]) - int(rowB[x])

		diff := gammaDiffLUT[cv] * float32(d4)
		if diff *= diff; diff > aqDiffLimit {
			diff = aqDiffLimit
		}

		diff = aqMaskingSqrt(diff)

		if acc {
			diff += dst[x]
		}

		dst[x] = diff
	}
}
