//go:build amd64 && !noasm

package jpegn

// simdTiers lists the instruction sets this machine can run, widest last.
func simdTiers() []string {
	tiers := []string{"scalar"}

	if hasSSE4 {
		tiers = append(tiers, "sse")
	}

	if hasAVX2 {
		tiers = append(tiers, "avx2")
	}

	return tiers
}

func setTier(name string) {
	hasAVX2 = name == "avx2"
	hasSSE4 = name == "avx2" || name == "sse"
}
