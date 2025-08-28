//go:build amd64 && !noasm

package jpegn

var isAVX2 bool

func init() {
	isAVX2 = hasAVX2()
}

func hasAVX2() bool
