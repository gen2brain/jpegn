package jpegn

// preErosionRowScalar accumulates one row of local pixel differences into dst.
func preErosionRowScalar(dst []float32, row, rowT, rowB []byte, acc bool) {
	n := len(dst)

	lv, cv := int(row[0]), int(row[0])
	rv := int(row[min(1, n-1)])

	for x := 0; x < n; x++ {
		d4 := 4*cv - rv - lv - int(rowT[x]) - int(rowB[x])

		diff := gammaDiffLUT[cv] * float32(d4)
		if diff *= diff; diff > aqDiffLimit {
			diff = aqDiffLimit
		}

		diff = aqMaskingSqrt(diff)

		if acc {
			diff += dst[x]
		}

		dst[x] = diff

		lv, cv = cv, rv

		if x+2 < n {
			rv = int(row[x+2])
		}
	}
}
