package jpegn

import (
	"bytes"
	"image"
	"image/color"
	"image/draw"
	"image/jpeg"
	"math"
	"math/rand"
	"sync"
	"testing"
)

// synthImage builds a deterministic gradient and edge pattern.
func synthImage(w, h int) *image.RGBA {
	m := image.NewRGBA(image.Rect(0, 0, w, h))

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			r := uint8(x * 255 / max(w-1, 1))
			g := uint8(y * 255 / max(h-1, 1))
			b := uint8((x ^ y) & 0xFF)

			if (x/16+y/16)&1 == 0 {
				b = 255 - b
			}

			m.Set(x, y, color.RGBA{R: r, G: g, B: b, A: 255})
		}
	}

	return m
}

func encodeToBytes(t *testing.T, m image.Image, opts *EncodeOptions) []byte {
	t.Helper()

	var buf bytes.Buffer
	if err := Encode(&buf, m, opts); err != nil {
		t.Fatalf("Encode: %v", err)
	}

	return buf.Bytes()
}

// TestEncodeRoundTrip checks stdlib decodability and reconstruction error.
func TestEncodeRoundTrip(t *testing.T) {
	sizes := []image.Point{{1, 1}, {7, 3}, {8, 8}, {16, 16}, {17, 9}, {65, 33}, {128, 96}}
	subs := []struct {
		name string
		s    Subsampling
		minP float64
	}{
		{"444", Subsample444, 34},
		{"422", Subsample422, 28},
		{"440", Subsample440, 28},
		{"420", Subsample420, 26},
	}

	for _, sz := range sizes {
		src := synthImage(sz.X, sz.Y)

		for _, sub := range subs {
			for _, q := range []int{30, 75, 95} {
				data := encodeToBytes(t, src, &EncodeOptions{Quality: q, Subsampling: sub.s})

				got, err := jpeg.Decode(bytes.NewReader(data))
				if err != nil {
					t.Fatalf("%dx%d %s q%d: stdlib decode: %v", sz.X, sz.Y, sub.name, q, err)
				}

				if got.Bounds().Dx() != sz.X || got.Bounds().Dy() != sz.Y {
					t.Fatalf("%dx%d %s q%d: got bounds %v", sz.X, sz.Y, sub.name, q, got.Bounds())
				}

				if sz.X < 16 || sz.Y < 16 || q < 90 {
					continue
				}

				if p := psnr(src, got); p < sub.minP {
					t.Errorf("%dx%d %s q%d: psnr %.1f dB below %.1f", sz.X, sz.Y, sub.name, q, p, sub.minP)
				}
			}
		}
	}
}

// TestEncodeOwnDecoderMatchesStdlib checks both decoders agree on our output.
func TestEncodeOwnDecoderMatchesStdlib(t *testing.T) {
	src := synthImage(129, 71)

	for _, sub := range []Subsampling{Subsample444, Subsample422, Subsample440, Subsample420} {
		data := encodeToBytes(t, src, &EncodeOptions{Quality: 85, Subsampling: sub})

		mine, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true})
		if err != nil {
			t.Fatalf("sub %d: Decode: %v", sub, err)
		}

		std, err := jpeg.Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("sub %d: stdlib decode: %v", sub, err)
		}

		if p := psnr(mine, std); p < 40 {
			t.Errorf("sub %d: decoders disagree, psnr %.1f dB", sub, p)
		}
	}
}

func TestEncodeGray(t *testing.T) {
	src := image.NewGray(image.Rect(0, 0, 64, 48))
	for y := 0; y < 48; y++ {
		for x := 0; x < 64; x++ {
			src.SetGray(x, y, color.Gray{Y: uint8((x*4 + y*2) & 0xFF)})
		}
	}

	data := encodeToBytes(t, src, &EncodeOptions{Quality: 90})

	cfg, err := DecodeConfig(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("DecodeConfig: %v", err)
	}

	if cfg.ColorModel != color.GrayModel {
		t.Errorf("color model = %v, want Gray", cfg.ColorModel)
	}

	if cfg.Width != 64 || cfg.Height != 48 {
		t.Errorf("config = %dx%d, want 64x48", cfg.Width, cfg.Height)
	}

	got, err := jpeg.Decode(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("stdlib decode: %v", err)
	}

	if _, ok := got.(*image.Gray); !ok {
		t.Errorf("decoded type = %T, want *image.Gray", got)
	}

	if p := psnr(src, got); p < 38 {
		t.Errorf("psnr %.1f dB too low", p)
	}
}

// TestEncodeAutoSubsampling checks that SubsampleAuto follows the source type.
func TestEncodeAutoSubsampling(t *testing.T) {
	cases := []struct {
		name  string
		data  []byte
		ncomp int
		ratio image.YCbCrSubsampleRatio
	}{
		{"444", test444, 3, image.YCbCrSubsampleRatio444},
		{"422", test422, 3, image.YCbCrSubsampleRatio422},
		{"440", test440, 3, image.YCbCrSubsampleRatio440},
		{"420", test420, 3, image.YCbCrSubsampleRatio420},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			src, err := Decode(bytes.NewReader(tc.data))
			if err != nil {
				t.Fatalf("Decode source: %v", err)
			}

			ycc, ok := src.(*image.YCbCr)
			if !ok {
				t.Fatalf("source type = %T, want *image.YCbCr", src)
			}

			if ycc.SubsampleRatio != tc.ratio {
				t.Fatalf("source ratio = %v, want %v", ycc.SubsampleRatio, tc.ratio)
			}

			data := encodeToBytes(t, src, &EncodeOptions{Quality: 92})

			got, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("stdlib decode: %v", err)
			}

			out, ok := got.(*image.YCbCr)
			if !ok {
				t.Fatalf("decoded type = %T, want *image.YCbCr", got)
			}

			if out.SubsampleRatio != tc.ratio {
				t.Errorf("ratio = %v, want %v", out.SubsampleRatio, tc.ratio)
			}

			if p := psnr(src, got); p < 38 {
				t.Errorf("psnr %.1f dB too low", p)
			}
		})
	}
}

func TestEncodeOptimizeCoding(t *testing.T) {
	src := synthImage(160, 120)

	plain := encodeToBytes(t, src, &EncodeOptions{Quality: 80})
	opt := encodeToBytes(t, src, &EncodeOptions{Quality: 80, OptimizeCoding: true})

	if len(opt) >= len(plain) {
		t.Errorf("optimized size %d not smaller than %d", len(opt), len(plain))
	}

	a, err := jpeg.Decode(bytes.NewReader(plain))
	if err != nil {
		t.Fatalf("stdlib decode plain: %v", err)
	}

	b, err := jpeg.Decode(bytes.NewReader(opt))
	if err != nil {
		t.Fatalf("stdlib decode optimized: %v", err)
	}

	if p := psnr(a, b); !math.IsInf(p, 1) {
		t.Errorf("optimized coding changed the pixels, psnr %.1f dB", p)
	}
}

func TestEncodeRestartInterval(t *testing.T) {
	src := synthImage(96, 64)

	for _, ri := range []int{1, 2, 7, 1000} {
		data := encodeToBytes(t, src, &EncodeOptions{Quality: 85, RestartInterval: ri})

		got, err := jpeg.Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("ri %d: stdlib decode: %v", ri, err)
		}

		ref, err := jpeg.Decode(bytes.NewReader(encodeToBytes(t, src, &EncodeOptions{Quality: 85})))
		if err != nil {
			t.Fatalf("ri %d: stdlib decode ref: %v", ri, err)
		}

		if p := psnr(ref, got); !math.IsInf(p, 1) {
			t.Errorf("ri %d: restart markers changed the pixels, psnr %.1f dB", ri, p)
		}

		mine, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true})
		if err != nil {
			t.Fatalf("ri %d: Decode: %v", ri, err)
		}

		if mine.Bounds() != src.Bounds() {
			t.Errorf("ri %d: bounds = %v, want %v", ri, mine.Bounds(), src.Bounds())
		}
	}
}

// TestEncodeQualityMonotonic checks quality raises both size and accuracy.
func TestEncodeQualityMonotonic(t *testing.T) {
	src := synthImage(128, 128)

	var lastSize int
	var lastPSNR float64

	for _, q := range []int{10, 25, 50, 75, 90, 100} {
		data := encodeToBytes(t, src, &EncodeOptions{Quality: q, Subsampling: Subsample444})

		got, err := jpeg.Decode(bytes.NewReader(data))
		if err != nil {
			t.Fatalf("q%d: stdlib decode: %v", q, err)
		}

		p := psnr(src, got)

		if len(data) <= lastSize {
			t.Errorf("q%d: size %d not larger than %d", q, len(data), lastSize)
		}

		if p <= lastPSNR {
			t.Errorf("q%d: psnr %.1f not better than %.1f", q, p, lastPSNR)
		}

		lastSize, lastPSNR = len(data), p
	}
}

// TestEncodeSubImage checks encoding from a non-zero origin.
func TestEncodeSubImage(t *testing.T) {
	full := synthImage(128, 128)
	sub := full.SubImage(image.Rect(31, 17, 95, 81))

	data := encodeToBytes(t, sub, &EncodeOptions{Quality: 95, Subsampling: Subsample444})

	got, err := jpeg.Decode(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("stdlib decode: %v", err)
	}

	if got.Bounds().Dx() != 64 || got.Bounds().Dy() != 64 {
		t.Fatalf("bounds = %v, want 64x64", got.Bounds())
	}

	if p := psnr(sub, got); p < 34 {
		t.Errorf("psnr %.1f dB too low", p)
	}
}

// TestEncodeInputTypes checks the per-type row readers.
func TestEncodeInputTypes(t *testing.T) {
	base := synthImage(64, 64)

	nrgba := image.NewNRGBA(base.Bounds())
	for y := 0; y < 64; y++ {
		for x := 0; x < 64; x++ {
			c := base.RGBAAt(x, y)
			nrgba.SetNRGBA(x, y, color.NRGBA{R: c.R, G: c.G, B: c.B, A: 255})
		}
	}

	cm := image.NewCMYK(base.Bounds())
	for y := 0; y < 64; y++ {
		for x := 0; x < 64; x++ {
			cm.Set(x, y, base.At(x, y))
		}
	}

	for _, tc := range []struct {
		name string
		img  image.Image
		minP float64
	}{
		{"RGBA", base, 40},
		{"NRGBA", nrgba, 40},
		{"CMYK", cm, 34},
	} {
		t.Run(tc.name, func(t *testing.T) {
			data := encodeToBytes(t, tc.img, &EncodeOptions{Quality: 95, Subsampling: Subsample444})

			got, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("stdlib decode: %v", err)
			}

			if p := psnr(tc.img, got); p < tc.minP {
				t.Errorf("psnr %.1f dB below %.1f", p, tc.minP)
			}
		})
	}
}

func TestEncodeRejectsEmpty(t *testing.T) {
	var buf bytes.Buffer

	if err := Encode(&buf, image.NewRGBA(image.Rect(0, 0, 0, 10))); err == nil {
		t.Error("expected an error for a zero-width image")
	}
}

// TestEncodeSizeVersusStdlib records the size difference against stdlib.
func TestEncodeSizeVersusStdlib(t *testing.T) {
	src := synthImage(256, 256)

	mine := encodeToBytes(t, src, &EncodeOptions{Quality: 75, Subsampling: Subsample420})

	var std bytes.Buffer
	if err := jpeg.Encode(&std, src, &jpeg.Options{Quality: 75}); err != nil {
		t.Fatalf("stdlib encode: %v", err)
	}

	t.Logf("jpegn %d bytes, image/jpeg %d bytes", len(mine), std.Len())

	if float64(len(mine)) > float64(std.Len())*1.15 {
		t.Errorf("output %d bytes is more than 15%% larger than stdlib %d", len(mine), std.Len())
	}
}

// TestEncodeSolid covers DC-only blocks and minimal Huffman histograms.
func TestEncodeSolid(t *testing.T) {
	colors := []color.RGBA{
		{R: 0, G: 0, B: 0, A: 255},
		{R: 255, G: 255, B: 255, A: 255},
		{R: 255, G: 0, B: 0, A: 255},
		{R: 0, G: 0, B: 255, A: 255},
		{R: 128, G: 128, B: 128, A: 255},
	}

	for _, c := range colors {
		for _, sz := range []int{1, 8, 40} {
			src := image.NewRGBA(image.Rect(0, 0, sz, sz))
			for y := 0; y < sz; y++ {
				for x := 0; x < sz; x++ {
					src.SetRGBA(x, y, c)
				}
			}

			for _, opt := range []bool{false, true} {
				for _, q := range []int{1, 75, 100} {
					data := encodeToBytes(t, src, &EncodeOptions{
						Quality:        q,
						Subsampling:    Subsample444,
						OptimizeCoding: opt,
					})

					got, err := jpeg.Decode(bytes.NewReader(data))
					if err != nil {
						t.Fatalf("%v %dx%d opt=%v q%d: stdlib decode: %v", c, sz, sz, opt, q, err)
					}

					if _, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true}); err != nil {
						t.Fatalf("%v %dx%d opt=%v q%d: Decode: %v", c, sz, sz, opt, q, err)
					}

					if q < 100 {
						continue
					}

					r, g, b, _ := got.At(0, 0).RGBA()
					dr := int(r>>8) - int(c.R)
					dg := int(g>>8) - int(c.G)
					db := int(b>>8) - int(c.B)

					if abs(dr) > 2 || abs(dg) > 2 || abs(db) > 2 {
						t.Errorf("%v %dx%d opt=%v q100: got (%d,%d,%d)",
							c, sz, sz, opt, r>>8, g>>8, b>>8)
					}
				}
			}
		}
	}
}

func abs(v int) int {
	if v < 0 {
		return -v
	}

	return v
}

// TestEncodeUnsupportedYCbCrRatio checks the generic conversion fallback.
func TestEncodeUnsupportedYCbCrRatio(t *testing.T) {
	src := image.NewYCbCr(image.Rect(0, 0, 96, 64), image.YCbCrSubsampleRatio411)

	base := synthImage(96, 64)
	for y := 0; y < 64; y++ {
		for x := 0; x < 96; x++ {
			c := base.RGBAAt(x, y)
			yy, cb, cr := color.RGBToYCbCr(c.R, c.G, c.B)
			src.Y[src.YOffset(x, y)] = yy
			src.Cb[src.COffset(x, y)] = cb
			src.Cr[src.COffset(x, y)] = cr
		}
	}

	data := encodeToBytes(t, src, &EncodeOptions{Quality: 92})

	got, err := jpeg.Decode(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("stdlib decode: %v", err)
	}

	if p := psnr(src, got); p < 34 {
		t.Errorf("psnr %.1f dB too low", p)
	}
}

// TestEncodeOptimizeAllSizes exercises the table generator across histograms.
func TestEncodeOptimizeAllSizes(t *testing.T) {
	for _, sz := range []image.Point{{1, 1}, {3, 5}, {8, 8}, {33, 17}, {200, 140}} {
		src := synthImage(sz.X, sz.Y)

		for _, sub := range []Subsampling{Subsample444, Subsample420, SubsampleGray} {
			data := encodeToBytes(t, src, &EncodeOptions{
				Quality:        85,
				Subsampling:    sub,
				OptimizeCoding: true,
			})

			got, err := jpeg.Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("%v sub %d: stdlib decode: %v", sz, sub, err)
			}

			if got.Bounds().Dx() != sz.X || got.Bounds().Dy() != sz.Y {
				t.Errorf("%v sub %d: bounds = %v", sz, sub, got.Bounds())
			}
		}
	}
}

// TestQuantReciprocal checks the reciprocal matches integer division for every
// divisor a quantization table can produce and the full coefficient range.
func TestQuantReciprocal(t *testing.T) {
	const maxCoef = 1 << 14

	for qval := 1; qval <= 255; qval++ {
		d := int32(qval) * 8
		half := d >> 1
		recip := (1<<quantShift + int64(d) - 1) / int64(d)

		for a := int32(0); a <= maxCoef; a++ {
			want := (a + half) / d
			got := int32((int64(a+half) * recip) >> quantShift)

			if got != want {
				t.Fatalf("qval %d coef %d: got %d, want %d", qval, a, got, want)
			}
		}
	}
}

// TestEncodeSizeSweep round-trips every size to catch padding and stride bugs.
func TestEncodeSizeSweep(t *testing.T) {
	for h := 1; h <= 33; h++ {
		for w := 1; w <= 33; w++ {
			src := synthImage(w, h)

			for _, sub := range []Subsampling{Subsample444, SubsampleGray, Subsample420} {
				data := encodeToBytes(t, src, &EncodeOptions{Quality: 95, Subsampling: sub})

				std, err := jpeg.Decode(bytes.NewReader(data))
				if err != nil {
					t.Fatalf("%dx%d sub %d: stdlib decode: %v", w, h, sub, err)
				}

				if std.Bounds().Dx() != w || std.Bounds().Dy() != h {
					t.Fatalf("%dx%d sub %d: bounds = %v", w, h, sub, std.Bounds())
				}

				mine, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true})
				if err != nil {
					t.Fatalf("%dx%d sub %d: Decode: %v", w, h, sub, err)
				}

				if mine.Bounds().Dx() != w || mine.Bounds().Dy() != h {
					t.Fatalf("%dx%d sub %d: bounds = %v", w, h, sub, mine.Bounds())
				}

				if p := psnr(std, mine); p < 35 {
					t.Errorf("%dx%d sub %d: decoders disagree, psnr %.1f dB", w, h, sub, p)
				}
			}
		}
	}
}

// TestEncodeConcurrent exercises the encoder pool from several goroutines.
func TestEncodeConcurrent(t *testing.T) {
	subs := []Subsampling{Subsample444, Subsample422, Subsample440, Subsample420, SubsampleGray}

	var wg sync.WaitGroup

	for i := 0; i < 16; i++ {
		wg.Add(1)

		go func(i int) {
			defer wg.Done()

			src := synthImage(32+i*7, 24+i*3)

			for n := 0; n < 8; n++ {
				var buf bytes.Buffer

				opts := &EncodeOptions{
					Quality:        50 + n*5,
					Subsampling:    subs[(i+n)%len(subs)],
					OptimizeCoding: n%2 == 0,
				}

				if err := Encode(&buf, src, opts); err != nil {
					t.Errorf("goroutine %d: Encode: %v", i, err)

					return
				}

				got, err := jpeg.Decode(bytes.NewReader(buf.Bytes()))
				if err != nil {
					t.Errorf("goroutine %d: decode: %v", i, err)

					return
				}

				if got.Bounds() != src.Bounds() {
					t.Errorf("goroutine %d: bounds = %v, want %v", i, got.Bounds(), src.Bounds())

					return
				}
			}
		}(i)
	}

	wg.Wait()
}

// photoRGBA decodes a corpus image to RGBA.
func photoRGBA(tb testing.TB) *image.RGBA {
	tb.Helper()

	src, err := Decode(bytes.NewReader(test444), &Options{ToRGBA: true})
	if err != nil {
		tb.Fatalf("Decode: %v", err)
	}

	dst := image.NewRGBA(src.Bounds())
	draw.Draw(dst, dst.Bounds(), src, src.Bounds().Min, draw.Src)

	return dst
}

// photoYCbCr decodes a corpus image, keeping its native planes.
func photoYCbCr(tb testing.TB) *image.YCbCr {
	tb.Helper()

	src, err := Decode(bytes.NewReader(test420))
	if err != nil {
		tb.Fatalf("Decode: %v", err)
	}

	return src.(*image.YCbCr)
}

func photoGray(tb testing.TB) *image.Gray {
	tb.Helper()

	src, err := Decode(bytes.NewReader(testGRAY))
	if err != nil {
		tb.Fatalf("Decode: %v", err)
	}

	return src.(*image.Gray)
}

func benchEncode(b *testing.B, m image.Image, opts *EncodeOptions) {
	r := m.Bounds()

	b.ReportAllocs()
	b.SetBytes(int64(r.Dx() * r.Dy()))
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var buf bytes.Buffer
		if err := Encode(&buf, m, opts); err != nil {
			b.Fatal(err)
		}
	}
}

func benchStdlib(b *testing.B, m image.Image, quality int) {
	r := m.Bounds()

	b.ReportAllocs()
	b.SetBytes(int64(r.Dx() * r.Dy()))
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var buf bytes.Buffer
		if err := jpeg.Encode(&buf, m, &jpeg.Options{Quality: quality}); err != nil {
			b.Fatal(err)
		}
	}
}

// Only the 420 rows are comparable; stdlib always writes 4:2:0 for color.

func BenchmarkEncodeRGBA420(b *testing.B) {
	benchEncode(b, photoRGBA(b), &EncodeOptions{Quality: 75, Subsampling: Subsample420})
}

func BenchmarkEncodeRGBA420Stdlib(b *testing.B) {
	benchStdlib(b, photoRGBA(b), 75)
}

func BenchmarkEncodeYCbCr420(b *testing.B) {
	benchEncode(b, photoYCbCr(b), &EncodeOptions{Quality: 75})
}

func BenchmarkEncodeYCbCr420Stdlib(b *testing.B) {
	benchStdlib(b, photoYCbCr(b), 75)
}

func BenchmarkEncodeGray(b *testing.B) {
	benchEncode(b, photoGray(b), &EncodeOptions{Quality: 75})
}

func BenchmarkEncodeGrayStdlib(b *testing.B) {
	benchStdlib(b, photoGray(b), 75)
}

func BenchmarkEncodeRGBA444(b *testing.B) {
	benchEncode(b, photoRGBA(b), &EncodeOptions{Quality: 75, Subsampling: Subsample444})
}

func BenchmarkEncodeRGBA422(b *testing.B) {
	benchEncode(b, photoRGBA(b), &EncodeOptions{Quality: 75, Subsampling: Subsample422})
}

func BenchmarkEncodeRGBA420Optimize(b *testing.B) {
	benchEncode(b, photoRGBA(b), &EncodeOptions{Quality: 75, Subsampling: Subsample420, OptimizeCoding: true})
}

// TestEncodeSizeTable reports compressed size against stdlib.
func TestEncodeSizeTable(t *testing.T) {
	photo := photoRGBA(t)

	t.Logf("%-8s %10s %10s %10s %8s", "quality", "jpegn", "jpegn+opt", "stdlib", "vs std")

	for _, q := range []int{50, 75, 90, 95} {
		mine := len(encodeToBytes(t, photo, &EncodeOptions{Quality: q, Subsampling: Subsample420}))
		opt := len(encodeToBytes(t, photo, &EncodeOptions{
			Quality: q, Subsampling: Subsample420, OptimizeCoding: true,
		}))

		var std bytes.Buffer
		if err := jpeg.Encode(&std, photo, &jpeg.Options{Quality: q}); err != nil {
			t.Fatalf("stdlib encode: %v", err)
		}

		t.Logf("%-8d %10d %10d %10d %7.1f%%",
			q, mine, opt, std.Len(), 100*float64(mine-std.Len())/float64(std.Len()))

		if opt > mine {
			t.Errorf("q%d: optimized %d larger than plain %d", q, opt, mine)
		}
	}
}

// TestEmitBitsStuffing checks the bulk byte-stuffing fast path against a
// reference scan of the entropy-coded segment.
func TestEmitBitsStuffing(t *testing.T) {
	for i := 0; i < 32; i++ {
		if v := uint32(i) * 0x01010101; ((^v-0x01010101)&v&0x80808080 != 0) != false {
			t.Fatalf("false positive for %08x", v)
		}
	}

	for _, pos := range []uint{0, 8, 16, 24} {
		v := uint32(0xFF) << pos
		if (^v-0x01010101)&v&0x80808080 == 0 {
			t.Fatalf("missed 0xFF at bit %d (%08x)", pos, v)
		}
	}

	rng := rand.New(rand.NewSource(31))

	for n := 0; n < 4000; n++ {
		v := rng.Uint32()

		want := false
		for s := 0; s < 32; s += 8 {
			if byte(v>>uint(s)) == 0xFF {
				want = true
			}
		}

		if got := (^v-0x01010101)&v&0x80808080 != 0; got != want {
			t.Fatalf("%08x: got %v, want %v", v, got, want)
		}
	}
}

// TestEncodeScanIsStuffed checks every 0xFF inside the entropy-coded segment is
// followed by a stuffed zero or is a restart marker.
func TestEncodeScanIsStuffed(t *testing.T) {
	srcs := []image.Image{synthImage(97, 61), photoRGBA(t)}

	solid := image.NewRGBA(image.Rect(0, 0, 64, 64))
	for i := range solid.Pix {
		solid.Pix[i] = 0xFF
	}

	srcs = append(srcs, solid)

	for si, src := range srcs {
		for _, q := range []int{1, 25, 75, 95, 100} {
			for _, ri := range []int{0, 3} {
				for _, opt := range []bool{false, true} {
					data := encodeToBytes(t, src, &EncodeOptions{
						Quality: q, Subsampling: Subsample420,
						OptimizeCoding: opt, RestartInterval: ri,
					})

					sos := bytes.Index(data, []byte{0xFF, 0xDA})
					if sos < 0 {
						t.Fatalf("no SOS marker")
					}

					start := sos + 2 + int(data[sos+2])<<8 + int(data[sos+3])

					for i := start; i < len(data)-2; i++ {
						if data[i] != 0xFF {
							continue
						}

						n := data[i+1]
						if n == 0x00 || (n >= 0xD0 && n <= 0xD7) {
							i++

							continue
						}

						if n == 0xD9 && i == len(data)-2 {
							break
						}

						t.Fatalf("src %d q%d ri%d opt=%v: unstuffed FF %02X at %d of %d",
							si, q, ri, opt, n, i, len(data))
					}

					if _, err := jpeg.Decode(bytes.NewReader(data)); err != nil {
						t.Fatalf("src %d q%d ri%d opt=%v: stdlib decode: %v", si, q, ri, opt, err)
					}
				}
			}
		}
	}
}

// TestDownsampleRow2x2MatchesScalar checks the box filter against the reference.
func TestDownsampleRow2x2MatchesScalar(t *testing.T) {
	rng := rand.New(rand.NewSource(11))

	for _, n := range []int{0, 1, 2, 7, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 255, 256} {
		src0 := make([]byte, n*2+2)
		src1 := make([]byte, n*2+2)

		for i := range src0 {
			src0[i] = byte(rng.Intn(256))
			src1[i] = byte(rng.Intn(256))
		}

		want := make([]byte, n+8)
		downsampleRow2x2Scalar(want, src0, src1, n)

		got := make([]byte, n+8)
		downsampleRow2x2(got, src0, src1, n)

		for i := 0; i < n; i++ {
			if got[i] != want[i] {
				t.Fatalf("n=%d sample %d: got %d, want %d (src %d,%d,%d,%d)",
					n, i, got[i], want[i], src0[i*2], src0[i*2+1], src1[i*2], src1[i*2+1])
			}
		}

		for i := n; i < n+8; i++ {
			if got[i] != 0 {
				t.Fatalf("n=%d: wrote past end at %d", n, i)
			}
		}
	}
}

// TestDownsampleRow2x2Exhaustive checks every sample pair in both source rows.
func TestDownsampleRow2x2Exhaustive(t *testing.T) {
	const n = 256

	src0 := make([]byte, n*2)
	src1 := make([]byte, n*2)
	got := make([]byte, n)
	want := make([]byte, n)

	others := []byte{0, 1, 2, 3, 127, 128, 254, 255}

	for _, swap := range []bool{false, true} {
		for a := 0; a < 256; a++ {
			for _, o := range others {
				for i := 0; i < n; i++ {
					lo, hi := &src0, &src1
					if swap {
						lo, hi = &src1, &src0
					}

					(*lo)[i*2] = byte(a)
					(*lo)[i*2+1] = byte(i)
					(*hi)[i*2] = o
					(*hi)[i*2+1] = byte(255 - i)
				}

				downsampleRow2x2Scalar(want, src0, src1, n)
				downsampleRow2x2(got, src0, src1, n)

				for i := 0; i < n; i++ {
					if got[i] != want[i] {
						t.Fatalf("swap=%v a=%d o=%d sample %d: got %d, want %d",
							swap, a, o, i, got[i], want[i])
					}
				}
			}
		}
	}
}

func BenchmarkDownsampleRow2x2(b *testing.B) {
	const n = 512

	src0 := make([]byte, n*2)
	src1 := make([]byte, n*2)
	dst := make([]byte, n)

	for i := range src0 {
		src0[i] = byte(i * 7 % 256)
		src1[i] = byte(i * 13 % 256)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		downsampleRow2x2(dst, src0, src1, n)
	}
}
