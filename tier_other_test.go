//go:build !amd64 || noasm

package jpegn

// simdTiers lists the instruction sets this machine can run, widest last.
func simdTiers() []string { return []string{"native"} }

func setTier(string) {}
