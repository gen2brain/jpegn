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
				{X: 10, Y: 20},
				{X: 42, Y: 42},
				{X: 100, Y: 400},
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2},
				{X: refBounds.Dx() - 1, Y: refBounds.Dy() - 1},
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
					myImgYCbCr, ok1 := img.(*image.YCbCr)
					refImgYCbCr, ok2 := refImg.(*image.YCbCr)

					if !ok1 {
						t.Fatalf("jpegn.Decode did not return YCbCr, but %T", img)
					}
					if !ok2 {
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

// TestDecodeProgressiveSubsampling tests decoding of progressive JPEGs with different subsampling ratios.
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
			// Include many edge and corner pixels to diagnose issues
			// For 4:2:0, chroma blocks are 16x16 pixels in the full image
			pointsToCheck := []image.Point{
				{X: 0, Y: 0},     // Top-left corner
				{X: 10, Y: 20},   // Near top-left
				{X: 42, Y: 42},   // Interior
				{X: 100, Y: 400}, // Interior
				{X: refBounds.Dx() / 2, Y: refBounds.Dy() / 2}, // Center
				// Test edges of different chroma blocks near bottom-right
				{X: 479, Y: 479}, // Block 992 (not last row/col)
				{X: 495, Y: 495}, // Block 1023 (last block)
				{X: 480, Y: 500}, // Block 1022 (last row, not last col)
				{X: 500, Y: 480}, // Block 1019 (last col, not last row)
				{X: 500, Y: 500}, // Block 1023
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

// BenchmarkGetVLC measures the performance of the Huffman decoding (getVLC) function.
// This microbenchmark isolates the Huffman decode operation to measure assembly optimization impact.
func BenchmarkGetVLC(b *testing.B) {
	// Decode a baseline JPEG to get a fully initialized decoder with real Huffman tables
	data, err := readAllData(bytes.NewReader(test420))
	if err != nil {
		b.Fatalf("Failed to read test data: %v", err)
	}

	d := decoderPool.Get().(*decoder)
	d.reset()
	defer func() {
		d.reset()
		decoderPool.Put(d)
	}()

	// Decode just the headers to build Huffman tables
	_, err = d.decode(data, false)
	if err != nil {
		b.Fatalf("Failed to decode: %v", err)
	}

	// Now we have real Huffman tables. Create a realistic bitstream buffer.
	// We'll fill it with actual JPEG scan data from the same image.
	// Find the scan data by decoding again and capturing the state.
	d.reset()
	d.jpegData = data
	d.size = len(data)
	d.pos = 0

	// Parse until we hit the scan data
	if err := d.skipToScan(); err != nil {
		b.Fatalf("Failed to skip to scan: %v", err)
	}

	// Now d.buf should have bitstream data and d.acVlcTab/dcVlcTab should be initialized
	// Fill the bit buffer
	d.showBits(16)

	// Use the AC VLC table (most commonly used in baseline JPEGs)
	vlc := d.acVlcTab[0]
	if vlc == nil {
		b.Fatal("AC VLC table not initialized")
	}

	b.ResetTimer()
	b.ReportAllocs()

	var result int
	var code uint8

	// Benchmark the hot path of getVLC
	for i := 0; i < b.N; i++ {
		// Reset buffer state periodically to avoid running out of bits
		if i%100 == 0 {
			d.showBits(16)
		}

		result = d.getVLC(vlc, nil, &code)
	}

	// Prevent compiler optimization
	if result == -12345 {
		b.Log("Impossible")
	}
}

// Helper function to skip to scan data for benchmarking
func (d *decoder) skipToScan() error {
	// This is a simplified version that just parses markers until we hit SOS
	for {
		if d.size < 2 {
			return ErrSyntax
		}

		if d.jpegData[d.pos] != 0xFF {
			return ErrSyntax
		}

		marker := d.jpegData[d.pos+1]
		d.pos += 2
		d.size -= 2

		if marker == 0xDA { // SOS - Start of Scan
			if err := d.decodeLength(); err != nil {
				return err
			}
			// Skip the SOS header
			if err := d.skip(d.length); err != nil {
				return err
			}
			return nil
		}

		// For other markers, skip them
		if marker == 0xD8 || marker == 0xD9 || (marker >= 0xD0 && marker <= 0xD7) {
			// SOI, EOI, RSTn - no payload
			continue
		}

		// Read length and skip
		if err := d.decodeLength(); err != nil {
			return err
		}
		if err := d.skip(d.length); err != nil {
			return err
		}
	}
}
