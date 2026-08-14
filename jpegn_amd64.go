//go:build amd64 && !noasm

package jpegn

var (
	hasAVX2 = cpuidAVX2()
	hasSSE4 = cpuidSSE41()
)

func cpuidAVX2() bool

func cpuidSSE41() bool
