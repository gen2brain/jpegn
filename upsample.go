//go:build (!amd64 && !arm64) || noasm

package jpegn

func upsampleNearestNeighbor(c *component, width, height int) {
	upsampleNearestNeighborGeneric(c, width, height)
}

func upsampleCatmullRom(c *component, width, height int) {
	upsampleCatmullRomGeneric(c, width, height)
}

func upsampleH(c *component) {
	upsampleHGeneric(c)
}

func upsampleV(c *component) {
	upsampleVGeneric(c)
}
