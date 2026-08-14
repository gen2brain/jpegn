//go:build noasm || (!amd64 && !arm64 && !(riscv64 && riscv64.rva23u64))

package jpegn

// preErosionRow accumulates one row of local pixel differences into dst.
func preErosionRow(dst []float32, row, rowT, rowB []byte, acc bool) {
	preErosionRowScalar(dst, row, rowT, rowB, acc)
}
