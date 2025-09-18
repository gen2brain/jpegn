//go:build !amd64 || noasm

package jpegn

// yCbCrToRGBA is the entry point for non-amd64 builds.
func yCbCrToRGBA(y, cb, cr *component, dst []byte, width, height int) {
	yCbCrToRGBAScalar(y, cb, cr, dst, width, height)
}

// rgbToRGBA is the entry point for non-amd64 builds.
func rgbToRGBA(r, g, b *component, dst []byte, width, height int) {
	rgbToRGBAScalar(r, g, b, dst, width, height)
}

// grayToRGBA is the entry point for non-amd64 builds.
func grayToRGBA(c *component, dst []byte, width, height int) {
	grayToRGBAScalar(c, dst, width, height)
}
