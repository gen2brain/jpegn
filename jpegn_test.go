package jpegn

import (
	"bytes"
	"embed"
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

	// Define options to test different code paths.
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
