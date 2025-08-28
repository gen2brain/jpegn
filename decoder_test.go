package jpegn

import (
	"bytes"
	_ "embed"
	"image"
	"image/color"
	"image/draw"
	"image/jpeg"
	"testing"
)

//go:embed testdata/test.420.jpg
var test420 []byte

//go:embed testdata/test.422.jpg
var test422 []byte

//go:embed testdata/test.440.jpg
var test440 []byte

//go:embed testdata/test.444.jpg
var test444 []byte

//go:embed testdata/test.cmyk.jpg
var testCMYK []byte

//go:embed testdata/test.gray.jpg
var testGRAY []byte

//go:embed testdata/test.rgb.jpg
var testRGB []byte

//go:embed testdata/test.420p.jpg
var test420p []byte

//go:embed testdata/test.420o.jpg
var test420o []byte

// baselineGray2x2 is a minimal 2x2, 8-bit grayscale, baseline JPEG.
var baselineGray2x2 = []byte{
	// SOI: Start of Image
	0xff, 0xd8,
	// APP0: JFIF segment
	0xff, 0xe0, 0x00, 0x10, 0x4a, 0x46, 0x49, 0x46, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x00, 0x01,
	0x00, 0x00,
	// DQT: Define Quantization Table
	0xff, 0xdb, 0x00, 0x43, 0x00, 0x03, 0x02, 0x02, 0x02, 0x02, 0x02, 0x03, 0x02, 0x02, 0x02, 0x03,
	0x03, 0x03, 0x03, 0x04, 0x06, 0x04, 0x04, 0x04, 0x05, 0x0a, 0x07, 0x07, 0x08, 0x0a, 0x0d, 0x0b,
	0x0d, 0x0c, 0x0c, 0x0b, 0x0b, 0x0c, 0x11, 0x0f, 0x12, 0x10, 0x13, 0x12, 0x11, 0x0f, 0x11, 0x10,
	0x10, 0x14, 0x18, 0x1a, 0x17, 0x14, 0x15, 0x18, 0x10, 0x10, 0x13, 0x1c, 0x15, 0x13, 0x15, 0x16,
	0x19, 0x1c, 0x19, 0x19, 0x19, // Added 2 padding bytes

	// SOF0: Start of Frame (Baseline DCT)
	0xff, 0xc0, 0x00, 0x0b, 0x08, 0x00, 0x02, 0x00, 0x02, 0x01, 0x01, 0x11, 0x00,

	// DHT for DC table 0 (Standard Luminance DC)
	0xff, 0xc4, 0x00, 0x1f, 0x00,
	// Counts (16 bytes)
	0x00, 0x01, 0x05, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	// Values (12 bytes)
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,

	// DHT for AC table 0 (Standard Luminance AC)
	0xff, 0xc4, 0x00, 0xb5, 0x10,
	// Counts (16 bytes)
	0x00, 0x02, 0x01, 0x03, 0x03, 0x02, 0x04, 0x03, 0x05, 0x05, 0x04, 0x04, 0x00, 0x00, 0x01, 0x7d,
	// Values (162 bytes)
	0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
	0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
	0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
	0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
	0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
	0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
	0xf9, 0xfa,

	// SOS: Start of Scan
	0xff, 0xda, // Marker
	0x00, 0x08, // Length 8 (6 + 2*1 component)
	0x01,       // Ns=1 (1 component)
	0x01, 0x00, // Cs=1 (ID 1), Td/Ta=0 (DC/AC table 0)
	0x00, 0x3f, 0x00, // Ss=0, Se=63, Ah/Al=0 (Baseline parameters)

	// Scan data
	0xed, 0x9f, 0x2f, 0x84, 0xa2, 0x8b, 0x1f, 0x22, 0xa2, 0x80, 0x2a, 0x28,
	0xa2, 0x80, 0x2a, 0x28, 0xa2, 0x80, 0x2a, 0x28, 0xa2, 0x80, 0x3f, 0xff,

	// EOI: End of Image
	0xd9,
}

// A small tolerance is needed to account for differences in IDCT implementations.
const defaultTolerance = 2

// isClose checks if two color component values are within the allowed tolerance.
func isClose(a, b, tol uint8) bool {
	if a > b {
		return a-b <= tol
	}

	return b-a <= tol
}

// TestDecode2x2 tests the main Decode function with a valid grayscale baseline JPEG.
// It verifies image dimensions and pixel values.
func TestDecode2x2(t *testing.T) {
	r := bytes.NewReader(baselineGray2x2)
	img, err := Decode(r, &Options{ToRGBA: true})
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	// Check image dimensions
	bounds := img.Bounds()
	if bounds.Dx() != 2 || bounds.Dy() != 2 {
		t.Fatalf("Expected 2x2 image, got %dx%d", bounds.Dx(), bounds.Dy())
	}

	// Expected pixel values (grayscale, so R=G=B).
	// These values are based on the output of a standard reference decoder.
	expectedPixels := []color.RGBA{
		{150, 150, 150, 255}, {150, 150, 150, 255},
		{150, 150, 150, 255}, {150, 150, 150, 255},
	}

	for y := 0; y < 2; y++ {
		for x := 0; x < 2; x++ {
			expected := expectedPixels[y*2+x]
			got := img.At(x, y).(color.RGBA)

			if !isClose(got.R, expected.R, defaultTolerance) ||
				!isClose(got.G, expected.G, defaultTolerance) ||
				!isClose(got.B, expected.B, defaultTolerance) ||
				got.A != expected.A {
				t.Errorf("Pixel at (%d, %d) - got RGBA%v, want close to RGBA%v", x, y, got, expected)
			}
		}
	}
}

// TestDecodeSubsampling tests decoding of baseline JPEGs with different subsampling ratios,
// verifying the default YCbCr output.
func TestDecodeSubsampling(t *testing.T) {
	var testFiles = map[string][]byte{
		"4:2:0": test420,
		"4:2:2": test422,
		"4:4:0": test440,
		"4:4:4": test444,
	}

	for name, data := range testFiles {
		t.Run(name, func(t *testing.T) {
			refImgStd, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("std jpeg.Decode failed: %v", err)
			}

			refImg, ok := refImgStd.(*image.YCbCr)
			if !ok {
				t.Fatalf("std jpeg.Decode did not return a YCbCr image, but %T", refImgStd)
			}
			refBounds := refImg.Bounds()

			img, err := Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("Decode failed: %v", err)
			}
			bounds := img.Bounds()

			if bounds != refBounds {
				t.Fatalf("Bounds mismatch: got %v, want %v", bounds, refBounds)
			}

			myImg, ok := img.(*image.YCbCr)
			if !ok {
				t.Fatalf("jpegn.Decode did not return a YCbCr image, but %T", img)
			}

			// Check a few sample pixel values against the reference.
			pointsToCheck := []image.Point{
				{X: 0, Y: 0},
				{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
				{X: 100, Y: 400},
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
				{X: 42, Y: 42},
			}

			for _, p := range pointsToCheck {
				expected := refImg.YCbCrAt(p.X, p.Y)
				got := myImg.YCbCrAt(p.X, p.Y)

				if !isClose(got.Y, expected.Y, defaultTolerance) ||
					!isClose(got.Cb, expected.Cb, defaultTolerance) ||
					!isClose(got.Cr, expected.Cr, defaultTolerance) {
					t.Errorf("Pixel at %v - got YCbCr %v, want close to YCbCr %v", p, got, expected)
				}
			}
		})
	}
}

// TestDecodeSubsamplingRGBANearestNeighbor tests decoding of baseline JPEGs with different subsampling ratios,
// verifying the RGBA conversion with NearestNeighbor upsampling.
func TestDecodeSubsamplingRGBANearestNeighbor(t *testing.T) {
	var testFiles = map[string][]byte{
		"4:2:0": test420,
		"4:2:2": test422,
		"4:4:0": test440,
		"4:4:4": test444,
	}

	for name, data := range testFiles {
		t.Run(name, func(t *testing.T) {
			refImg, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("std jpeg.Decode failed: %v", err)
			}
			refBounds := refImg.Bounds()

			img, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true, UpsampleMethod: NearestNeighbor})
			if err != nil {
				t.Fatalf("Decode failed: %v", err)
			}
			bounds := img.Bounds()

			if bounds != refBounds {
				t.Fatalf("Bounds mismatch: got %v, want %v", bounds, refBounds)
			}

			pointsToCheck := []image.Point{
				{X: 0, Y: 0},
				{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
				{X: 100, Y: 400},
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
				{X: 42, Y: 42},
			}

			for _, p := range pointsToCheck {
				expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
				got := img.At(p.X, p.Y).(color.RGBA)

				if !isClose(got.R, expected.R, defaultTolerance) ||
					!isClose(got.G, expected.G, defaultTolerance) ||
					!isClose(got.B, expected.B, defaultTolerance) ||
					got.A != expected.A {
					t.Errorf("Pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
				}
			}
		})
	}
}

// TestDecodeSubsamplingRGBACatmullRom tests decoding of baseline JPEGs with different subsampling ratios,
// verifying the RGBA conversion with CatmullRom upsampling.
func TestDecodeSubsamplingRGBACatmullRom(t *testing.T) {
	var testFiles = map[string][]byte{
		"4:2:0": test420,
		"4:2:2": test422,
		"4:4:0": test440,
		"4:4:4": test444,
	}

	// When testing Catmull-Rom upsampling against the standard library reference
	// (which uses the Nearest Neighbor and higher precision color conversion),
	// a larger tolerance is required to accommodate the algorithmic differences.
	const rgbaTolerance = 10

	for name, data := range testFiles {
		t.Run(name, func(t *testing.T) {
			refImg, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("std jpeg.Decode failed: %v", err)
			}
			refBounds := refImg.Bounds()

			img, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true, UpsampleMethod: CatmullRom})
			if err != nil {
				t.Fatalf("Decode failed: %v", err)
			}
			bounds := img.Bounds()

			if bounds != refBounds {
				t.Fatalf("Bounds mismatch: got %v, want %v", bounds, refBounds)
			}

			pointsToCheck := []image.Point{
				{X: 0, Y: 0},
				{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
				{X: 100, Y: 400},
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
				{X: 42, Y: 42},
			}

			for _, p := range pointsToCheck {
				expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
				got := img.At(p.X, p.Y).(color.RGBA)

				if !isClose(got.R, expected.R, rgbaTolerance) ||
					!isClose(got.G, expected.G, rgbaTolerance) ||
					!isClose(got.B, expected.B, rgbaTolerance) ||
					got.A != expected.A {
					t.Errorf("Pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
				}
			}
		})
	}
}

// TestDecodeGray tests decoding of a baseline grayscale JPEG.
func TestDecodeGray(t *testing.T) {
	refImg, err := jpeg.Decode(bytes.NewReader(testGRAY))
	if err != nil {
		t.Fatalf("std jpeg.Decode failed for grayscale image: %v", err)
	}
	refBounds := refImg.Bounds()

	// Note: Grayscale images are converted to RGBA by default in our decoder when ncomp=1.
	// We test this path explicitly.
	img, err := Decode(bytes.NewReader(testGRAY), &Options{ToRGBA: true})
	if err != nil {
		t.Fatalf("Decode failed for grayscale image: %v", err)
	}
	bounds := img.Bounds()

	if bounds != refBounds {
		t.Fatalf("Bounds mismatch: got %v, want %v", bounds, refBounds)
	}

	pointsToCheck := []image.Point{
		{X: 0, Y: 0},
		{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
		{X: refBounds.Dx() / 4, Y: refBounds.Dy() / 4},
		{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
		{X: 15, Y: 85},
	}

	for _, p := range pointsToCheck {
		expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
		got := img.At(p.X, p.Y).(color.RGBA)

		if !isClose(got.R, expected.R, defaultTolerance) ||
			!isClose(got.G, expected.G, defaultTolerance) ||
			!isClose(got.B, expected.B, defaultTolerance) ||
			got.A != expected.A {
			t.Errorf("ERROR: Grayscale pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
		}
	}
}

// TestDecodeRGB tests decoding of a baseline RGB JPEG.
func TestDecodeRGB(t *testing.T) {
	refImg, err := jpeg.Decode(bytes.NewReader(testRGB))
	if err != nil {
		t.Fatalf("std jpeg.Decode failed for RGB image: %v", err)
	}
	refBounds := refImg.Bounds()

	// RGB JPEGs are always converted to RGBA in decoder.
	img, err := Decode(bytes.NewReader(testRGB))
	if err != nil {
		t.Fatalf("Decode failed for RGB image: %v", err)
	}
	bounds := img.Bounds()

	if bounds != refBounds {
		t.Fatalf("Bounds mismatch: got %v, want %v", bounds, refBounds)
	}

	pointsToCheck := []image.Point{
		{X: 0, Y: 0},
		{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
		{X: refBounds.Dx() / 4, Y: refBounds.Dy() / 4},
		{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
		{X: 15, Y: 85},
	}

	for _, p := range pointsToCheck {
		expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
		got := img.At(p.X, p.Y).(color.RGBA)

		if !isClose(got.R, expected.R, defaultTolerance) ||
			!isClose(got.G, expected.G, defaultTolerance) ||
			!isClose(got.B, expected.B, defaultTolerance) ||
			got.A != expected.A {
			t.Errorf("ERROR: RGB pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
		}
	}
}

// TestDecodeProgressive verifies that the decoder correctly falls back to the
// standard library when an unsupported format (like a progressive JPEG) is provided.
func TestDecodeProgressive(t *testing.T) {
	img, err := Decode(bytes.NewReader(test420p))
	if err != nil {
		t.Fatalf("Decode with fallback failed for progressive JPEG: %v", err)
	}

	refImg, err := jpeg.Decode(bytes.NewReader(test420p))
	if err != nil {
		t.Fatalf("std jpeg.Decode failed for progressive image: %v", err)
	}

	if img.Bounds() != refImg.Bounds() {
		t.Fatalf("Bounds mismatch: got %v, want %v", img.Bounds(), refImg.Bounds())
	}

	refBounds := refImg.Bounds()
	pointsToCheck := []image.Point{
		{X: 0, Y: 0},
		{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
		{X: 100, Y: 400},
		{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
	}

	for _, p := range pointsToCheck {
		expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
		got := color.RGBAModel.Convert(img.At(p.X, p.Y)).(color.RGBA)

		if !isClose(got.R, expected.R, defaultTolerance) ||
			!isClose(got.G, expected.G, defaultTolerance) ||
			!isClose(got.B, expected.B, defaultTolerance) {
			t.Errorf("Pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
		}
	}
}

// TestDecodeCMYK verifies that the decoder correctly falls back to the
// standard library when an unsupported format (like a CMYK JPEG) is provided.
func TestDecodeCMYK(t *testing.T) {
	img, err := Decode(bytes.NewReader(testCMYK))
	if err != nil {
		t.Fatalf("Decode with fallback failed for CMYK JPEG: %v", err)
	}

	refImg, err := jpeg.Decode(bytes.NewReader(testCMYK))
	if err != nil {
		t.Fatalf("std jpeg.Decode failed for CMYK image: %v", err)
	}

	if img.Bounds() != refImg.Bounds() {
		t.Fatalf("Bounds mismatch: got %v, want %v", img.Bounds(), refImg.Bounds())
	}

	refBounds := refImg.Bounds()
	pointsToCheck := []image.Point{
		{X: 0, Y: 0},
		{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
		{X: 100, Y: 400},
		{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
	}

	for _, p := range pointsToCheck {
		expected := color.CMYKModel.Convert(refImg.At(p.X, p.Y)).(color.CMYK)
		got := color.CMYKModel.Convert(img.At(p.X, p.Y)).(color.CMYK)

		if !isClose(got.C, expected.C, defaultTolerance) ||
			!isClose(got.M, expected.M, defaultTolerance) ||
			!isClose(got.Y, expected.Y, defaultTolerance) ||
			!isClose(got.K, expected.K, defaultTolerance) {
			t.Errorf("Pixel at %v - got CMYK%v, want close to CMYK%v", p, got, expected)
		}
	}
}

// TestDecodeAutoRotate verifies that the decoder correctly rotates the image based on the EXIF orientation tag.
func TestDecodeAutoRotate(t *testing.T) {
	// test420o.jpg is 384x512 and has EXIF orientation 6 (Rotate 90 CW).

	imgRef, err := Decode(bytes.NewReader(test420o), &Options{ToRGBA: true, AutoRotate: false})
	if err != nil {
		t.Fatalf("Decode reference failed: %v", err)
	}

	boundsRef := imgRef.Bounds()
	widthRef, heightRef := boundsRef.Dx(), boundsRef.Dy()

	if widthRef != 384 || heightRef != 512 {
		t.Fatalf("Reference image dimensions incorrect: got %dx%d, want 384x512", widthRef, heightRef)
	}

	imgRot, err := Decode(bytes.NewReader(test420o), &Options{AutoRotate: true})
	if err != nil {
		t.Fatalf("Decode with AutoRotate failed: %v", err)
	}

	boundsRot := imgRot.Bounds()
	widthRot, heightRot := boundsRot.Dx(), boundsRot.Dy()

	// Check if dimensions are swapped (Orientation 6 requires rotation).
	if widthRot != heightRef || heightRot != widthRef {
		t.Fatalf("Dimensions not swapped correctly. Ref: %dx%d, Rotated: %dx%d", widthRef, heightRef, widthRot, heightRot)
	}

	if _, ok := imgRot.(*image.RGBA); !ok {
		t.Fatalf("AutoRotate did not return an RGBA image, but %T", imgRot)
	}

	// Compare pixel data to verify rotation (90 CW).
	// Mapping for 90 CW (orientation 6): Original(sx, sy) -> Rotated(H_src-1-sy, sx)
	// Where H_src is the height of the original image.

	// Helper function to compare pixels, assuming RGBA input images.
	comparePixels := func(sx, sy, dx, dy int) {
		pRef := imgRef.At(sx, sy).(color.RGBA)
		pRot := imgRot.At(dx, dy).(color.RGBA)

		// The colors should match very closely as rotation is a direct memory copy after conversion.
		if !isClose(pRef.R, pRot.R, defaultTolerance) ||
			!isClose(pRef.G, pRot.G, defaultTolerance) ||
			!isClose(pRef.B, pRot.B, defaultTolerance) {
			t.Errorf("Pixel mismatch at Rotated(%d, %d). Got %v, want close to %v (from Ref (%d, %d))", dx, dy, pRot, pRef, sx, sy)
		}
	}

	// Check corners:

	// Original top-left (0, 0) -> New top-right (H-1, 0).
	comparePixels(0, 0, heightRef-1, 0)

	// Original top-right (W-1, 0) -> New bottom-right (H-1, W-1).
	comparePixels(widthRef-1, 0, heightRef-1, widthRef-1)

	// Original bottom-left (0, H-1) -> New top-left (0, 0).
	comparePixels(0, heightRef-1, 0, 0)

	// Original bottom-right (W-1, H-1) -> New bottom-left (0, W-1).
	comparePixels(widthRef-1, heightRef-1, 0, widthRef-1)

	// Check center pixel.
	sx, sy := widthRef/2, heightRef/2
	dx, dy := heightRef-1-sy, sx
	comparePixels(sx, sy, dx, dy)
}

// BenchmarkDecode420 measures the performance of decoder.
func BenchmarkDecode420(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)

		_, err := Decode(r)
		if err != nil {
			b.Fatalf("Decode failed: %v", err)
		}
	}
}

// BenchmarkDecode420StdLib measures the performance of the standard library's image/jpeg decoder.
func BenchmarkDecode420StdLib(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)

		_, err := jpeg.Decode(r)
		if err != nil {
			b.Fatalf("image.Decode failed: %v", err)
		}
	}
}

// BenchmarkDecodeConfig measures the performance of DecodeConfig.
func BenchmarkDecodeConfig(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)
		_, err := DecodeConfig(r)
		if err != nil {
			b.Fatalf("DecodeConfig failed: %v", err)
		}
	}
}

// BenchmarkDecodeConfigStdLib measures the performance of the standard library's image/jpeg.DecodeConfig.
func BenchmarkDecodeConfigStdLib(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)
		_, err := jpeg.DecodeConfig(r)
		if err != nil {
			b.Fatalf("jpeg.DecodeConfig failed: %v", err)
		}
	}
}

// BenchmarkDecodeToRGBANearestNeighbor measures the performance of decoding to RGBA with NearestNeighbor upsampling.
func BenchmarkDecodeToRGBANearestNeighbor(b *testing.B) {
	opts := &Options{ToRGBA: true, UpsampleMethod: NearestNeighbor}
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)
		_, err := Decode(r, opts)
		if err != nil {
			b.Fatalf("Decode failed: %v", err)
		}
	}
}

// BenchmarkDecodeToRGBACatmullRom measures the performance of decoding to RGBA with CatmullRom upsampling.
func BenchmarkDecodeToRGBACatmullRom(b *testing.B) {
	opts := &Options{ToRGBA: true, UpsampleMethod: CatmullRom}
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)
		_, err := Decode(r, opts)
		if err != nil {
			b.Fatalf("Decode failed: %v", err)
		}
	}
}

// BenchmarkDecodeToRGBAStdLib measures the performance of the standard library decoding and converting to RGBA.
func BenchmarkDecodeToRGBAStdLib(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420)
		img, err := jpeg.Decode(r)
		if err != nil {
			b.Fatalf("jpeg.Decode failed: %v", err)
		}

		// Convert to RGBA
		bounds := img.Bounds()
		rgba := image.NewRGBA(bounds)
		draw.Draw(rgba, rgba.Bounds(), img, bounds.Min, draw.Src)
	}
}
