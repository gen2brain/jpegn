//go:build (!amd64 && !arm64) || noasm

package jpegn

func upsampleNearestNeighbor(c *component, width, height int) {
	upsampleNearestNeighborScalar(c, width, height)
}

func upsampleCatmullRom(c *component, width, height int) {
	upsampleCatmullRomScalar(c, width, height)
}

func upsampleH(c *component) {
	upsampleHScalar(c)
}

func upsampleV(c *component) {
	upsampleVScalar(c)
}
