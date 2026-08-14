//go:build riscv64 && riscv64.rva23u64 && !noasm

package jpegn

//go:noescape
func downsampleRow2x2RVV(dst, src0, src1 *byte, n int)

// downsampleRow2x2 box-filters 2x2 sample groups from two source rows.
func downsampleRow2x2(dst, src0, src1 []byte, n int) {
	if n == 0 {
		return
	}

	downsampleRow2x2RVV(&dst[0], &src0[0], &src1[0], n)
}
