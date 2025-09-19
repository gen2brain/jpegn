package jpegn

import (
	"bytes"
	_ "embed"
	"image"
	"image/color"
	"image/jpeg"
	"testing"
)

//go:embed testdata/test.420.progressive.jpg
var test420p []byte

//go:embed testdata/test.422.progressive.jpg
var test422p []byte

//go:embed testdata/test.440.progressive.jpg
var test440p []byte

//go:embed testdata/test.444.progressive.jpg
var test444p []byte

//go:embed testdata/test.gray.progressive.jpg
var testGRAYp []byte

//go:embed testdata/test.rgb.progressive.jpg
var testRGBp []byte

// TestDecodeProgressive verifies that the decoder correctly decodes a progressive JPEG.
func TestDecodeProgressive(t *testing.T) {
	// Test both native YCbCr output and RGBA output.
	testCases := []struct {
		name      string
		options   *Options
		tolerance uint8
	}{
		{"Native", nil, defaultTolerance},
		{"RGBA_NearestNeighbor", &Options{ToRGBA: true, UpsampleMethod: NearestNeighbor}, defaultTolerance},
		// CatmullRom requires a higher tolerance due to algorithmic differences with stdlib.
		{"RGBA_CatmullRom", &Options{ToRGBA: true, UpsampleMethod: CatmullRom}, 10},
	}

	refImg, err := jpeg.Decode(bytes.NewReader(test420p))
	if err != nil {
		t.Fatalf("std jpeg.Decode failed for progressive image: %v", err)
	}
	refBounds := refImg.Bounds()

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			img, err := Decode(bytes.NewReader(test420p), tc.options)
			if err != nil {
				t.Fatalf("Decode failed for progressive JPEG: %v", err)
			}

			if img.Bounds() != refBounds {
				t.Fatalf("Bounds mismatch: got %v, want %v", img.Bounds(), refBounds)
			}

			pointsToCheck := []image.Point{
				{X: 0, Y: 0},
				{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
				{X: 100, Y: 400},
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
				{X: 42, Y: 42},
			}

			// Determine the color model to use for comparison.
			isRGBA := tc.options != nil && tc.options.ToRGBA

			for _, p := range pointsToCheck {
				if isRGBA {
					expected := color.RGBAModel.Convert(refImg.At(p.X, p.Y)).(color.RGBA)
					got := color.RGBAModel.Convert(img.At(p.X, p.Y)).(color.RGBA)

					if !isClose(got.R, expected.R, tc.tolerance) ||
						!isClose(got.G, expected.G, tc.tolerance) ||
						!isClose(got.B, expected.B, tc.tolerance) {
						t.Errorf("Pixel at %v - got RGBA%v, want close to RGBA%v", p, got, expected)
					}
				} else {
					// YCbCr comparison.
					myImgYCbCr, ok1 := img.(*image.YCbCr)
					refImgYCbCr, ok2 := refImg.(*image.YCbCr)

					if !ok1 {
						t.Fatalf("jpegn.Decode did not return YCbCr, but %T", img)
					}
					if !ok2 {
						// Standard library usually returns YCbCr for 4:2:0 JPEGs.
						t.Logf("std jpeg.Decode did not return YCbCr, but %T. Skipping YCbCr comparison.", refImg)
						continue
					}

					expected := refImgYCbCr.YCbCrAt(p.X, p.Y)
					got := myImgYCbCr.YCbCrAt(p.X, p.Y)

					if !isClose(got.Y, expected.Y, tc.tolerance) ||
						!isClose(got.Cb, expected.Cb, tc.tolerance) ||
						!isClose(got.Cr, expected.Cr, tc.tolerance) {
						t.Errorf("Pixel at %v - got YCbCr %v, want close to YCbCr %v", p, got, expected)
					}
				}
			}
		})
	}
}

// TestDecodeProgressiveSubsampling tests decoding of progressive JPEGs with different subsampling ratios,
// verifying the default YCbCr output.
func TestDecodeProgressiveSubsampling(t *testing.T) {
	var testFiles = map[string][]byte{
		"4:2:0": test420p,
		"4:2:2": test422p,
		"4:4:0": test440p,
		"4:4:4": test444p,
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

// BenchmarkDecodeProgressive420 measures the performance of the progressive decoder.
func BenchmarkDecodeProgressive420(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420p)

		_, err := Decode(r)
		if err != nil {
			b.Fatalf("Decode failed: %v", err)
		}
	}
}

// BenchmarkDecodeProgressive420StdLib measures the performance of the standard library's progressive decoder.
func BenchmarkDecodeProgressive420StdLib(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r := bytes.NewReader(test420p)

		_, err := jpeg.Decode(r)
		if err != nil {
			b.Fatalf("image.Decode failed: %v", err)
		}
	}
}
