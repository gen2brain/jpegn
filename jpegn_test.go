package jpegn

import (
	"bytes"
	"embed"
	"image"
	"image/jpeg"
	"path/filepath"
	"testing"
)

//go:embed testdata/*.jpg
var fuzzCorpus embed.FS

// addFuzzCorpus adds all embedded testdata/*.jpg files to the fuzzing seed corpus.
func addFuzzCorpus(f *testing.F) {
	f.Helper()

	files, err := fuzzCorpus.ReadDir("testdata")
	if err != nil {
		f.Fatalf("failed to read embedded dir: %v", err)
	}

	for _, file := range files {
		path := filepath.Join("testdata", file.Name())
		data, err := fuzzCorpus.ReadFile(path)
		if err != nil {
			f.Fatalf("failed to read embedded file %s: %v", path, err)
		}

		// Add the file content to the seed corpus.
		f.Add(data)
	}
}

// FuzzDecode tests the Decode function for panics with a variety of inputs.
func FuzzDecode(f *testing.F) {
	addFuzzCorpus(f)

	optsNN := &Options{ToRGBA: true, UpsampleMethod: NearestNeighbor}
	optsCR := &Options{ToRGBA: true, UpsampleMethod: CatmullRom}

	f.Fuzz(func(t *testing.T, data []byte) {
		// Test decoding to native format (e.g., YCbCr, Gray).
		_, _ = Decode(bytes.NewReader(data))

		// Test decoding to RGBA with Nearest Neighbor upsampling.
		_, _ = Decode(bytes.NewReader(data), optsNN)

		// Test decoding to RGBA with Catmull-Rom upsampling.
		_, _ = Decode(bytes.NewReader(data), optsCR)
	})
}

// FuzzDecodeConfig tests the DecodeConfig function for panics.
func FuzzDecodeConfig(f *testing.F) {
	addFuzzCorpus(f)

	f.Fuzz(func(t *testing.T, data []byte) {
		_, _ = DecodeConfig(bytes.NewReader(data))
	})
}

// FuzzEncode checks every accepted image produces a decodable stream.
func FuzzEncode(f *testing.F) {
	f.Add(uint8(16), uint8(16), uint8(75), uint8(0), false, uint8(0), []byte{1, 2, 3, 4})
	f.Add(uint8(1), uint8(1), uint8(100), uint8(4), true, uint8(1), []byte{0})
	f.Add(uint8(37), uint8(9), uint8(1), uint8(3), true, uint8(3), []byte{255, 0, 128})

	f.Fuzz(func(t *testing.T, w, h, quality, sub uint8, optimize bool, rst uint8, pix []byte) {
		width := int(w)%97 + 1
		height := int(h)%97 + 1

		src := image.NewRGBA(image.Rect(0, 0, width, height))
		if len(pix) > 0 {
			for i := range src.Pix {
				src.Pix[i] = pix[i%len(pix)]
			}
		}

		for i := 3; i < len(src.Pix); i += 4 {
			src.Pix[i] = 0xFF
		}

		opts := &EncodeOptions{
			Quality:         int(quality)%101 + 1,
			Subsampling:     Subsampling(int(sub) % 6),
			OptimizeCoding:  optimize,
			RestartInterval: int(rst),
		}

		var buf bytes.Buffer
		if err := Encode(&buf, src, opts); err != nil {
			t.Fatalf("Encode: %v", err)
		}

		data := buf.Bytes()

		std, err := jpeg.Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("stdlib decode: %v", err)
		}

		if std.Bounds().Dx() != width || std.Bounds().Dy() != height {
			t.Fatalf("stdlib bounds = %v, want %dx%d", std.Bounds(), width, height)
		}

		got, err := Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("Decode: %v", err)
		}

		if got.Bounds() != std.Bounds() {
			t.Fatalf("bounds = %v, want %v", got.Bounds(), std.Bounds())
		}
	})
}
