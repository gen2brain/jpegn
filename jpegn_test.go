package jpegn

import (
	"bytes"
	"embed"
	"image"
	"image/jpeg"
	"os"
	"path"
	"path/filepath"
	"strings"
	"testing"
)

//go:embed testdata/*.jpg
var fuzzCorpus embed.FS

// eachTier runs f once per instruction set the machine supports, so the narrow
// kernels are tested on a wide machine.
func eachTier(t *testing.T, f func(t *testing.T)) {
	t.Helper()

	tiers := simdTiers()
	if len(tiers) == 1 {
		f(t)

		return
	}

	for _, tier := range tiers {
		t.Run(tier, func(t *testing.T) {
			defer setTier(tiers[len(tiers)-1])
			setTier(tier)
			f(t)
		})
	}
}

// eachTierB benchmarks f once per instruction set the machine supports.
func eachTierB(b *testing.B, f func(b *testing.B)) {
	b.Helper()

	tiers := simdTiers()
	if len(tiers) == 1 {
		f(b)

		return
	}

	for _, tier := range tiers {
		b.Run(tier, func(b *testing.B) {
			defer setTier(tiers[len(tiers)-1])
			setTier(tier)
			f(b)
		})
	}
}

// addFuzzCorpus adds all embedded testdata/*.jpg files to the fuzzing seed corpus.
func addFuzzCorpus(f *testing.F) {
	f.Helper()

	files, err := fuzzCorpus.ReadDir("testdata")
	if err != nil {
		f.Fatalf("failed to read embedded dir: %v", err)
	}

	for _, file := range files {
		// embed.FS is always slash separated, so filepath.Join would build a
		// name with backslashes on Windows and never match.
		name := path.Join("testdata", file.Name())

		data, err := fuzzCorpus.ReadFile(name)
		if err != nil {
			f.Fatalf("failed to read embedded file %s: %v", name, err)
		}

		// Add the file content to the seed corpus.
		f.Add(data)
	}

	addConformanceCorpus(f)
}

// addConformanceCorpus seeds from the corpora named by CONFORMANCE_DIR, whose
// crash reproducers are far better fuzz seeds than the embedded testdata.
func addConformanceCorpus(f *testing.F) {
	f.Helper()

	env := os.Getenv("CONFORMANCE_DIR")
	if env == "" {
		return
	}

	for _, dir := range filepath.SplitList(env) {
		_ = filepath.Walk(dir, func(p string, fi os.FileInfo, err error) error {
			if err != nil || fi.IsDir() || fi.Size() > 1<<20 {
				return nil //nolint:nilerr
			}

			switch strings.ToLower(filepath.Ext(p)) {
			case ".jpg", ".jpeg":
				if data, err := os.ReadFile(p); err == nil {
					f.Add(data)
				}
			}

			return nil
		})
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

		_, _ = DecodeExif(bytes.NewReader(data))

		if raw, err := RawExif(bytes.NewReader(data)); err == nil {
			_ = setExifOrientation(raw, 1)
		}
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

		prog := sub/6&1 == 1
		adaptive := sub/12&1 == 1

		opts := &EncodeOptions{
			Quality:         int(quality)%101 + 1,
			Subsampling:     Subsampling(int(sub) % 6),
			OptimizeCoding:  optimize,
			Progressive:     prog,
			RestartInterval: int(rst),

			AdaptiveQuantization: adaptive,
		}

		var buf bytes.Buffer
		if err := Encode(&buf, src, opts); err != nil {
			t.Fatalf("Encode: %v", err)
		}

		data := buf.Bytes()

		got, err := Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("Decode: %v", err)
		}

		if got.Bounds().Dx() != width || got.Bounds().Dy() != height {
			t.Fatalf("bounds = %v, want %dx%d", got.Bounds(), width, height)
		}

		// The stdlib cannot read progressive restarts.
		if prog && rst > 0 {
			return
		}

		std, err := jpeg.Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("stdlib decode: %v", err)
		}

		if got.Bounds() != std.Bounds() {
			t.Fatalf("bounds = %v, want %v", got.Bounds(), std.Bounds())
		}
	})
}
