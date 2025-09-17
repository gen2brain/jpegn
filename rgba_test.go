package jpegn

import (
	"bytes"
	"image/color"
	"reflect"
	"testing"
)

// TestYCbCrToRGBA verifies the correctness of the YCbCr to RGBA color conversion.
func TestYCbCrToRGBA(t *testing.T) {
	// These test cases are based on the standard JFIF conversion formulas.
	// A small tolerance is used to account for rounding differences in integer arithmetic.
	testCases := []struct {
		name      string
		in        color.YCbCr
		want      color.RGBA
		tolerance uint8
	}{
		{"Black", color.YCbCr{Y: 0, Cb: 128, Cr: 128}, color.RGBA{R: 0, G: 0, B: 0, A: 255}, 1},
		{"White", color.YCbCr{Y: 255, Cb: 128, Cr: 128}, color.RGBA{R: 255, G: 255, B: 255, A: 255}, 1},
		{"Gray", color.YCbCr{Y: 128, Cb: 128, Cr: 128}, color.RGBA{R: 128, G: 128, B: 128, A: 255}, 1},
		{"Red", color.YCbCr{Y: 76, Cb: 84, Cr: 255}, color.RGBA{R: 255, G: 0, B: 0, A: 255}, 2},
		{"Green", color.YCbCr{Y: 149, Cb: 43, Cr: 21}, color.RGBA{R: 0, G: 255, B: 0, A: 255}, 2},
		{"Blue", color.YCbCr{Y: 29, Cb: 255, Cr: 107}, color.RGBA{R: 0, G: 0, B: 255, A: 255}, 2},
		{"Magenta", color.YCbCr{Y: 105, Cb: 212, Cr: 234}, color.RGBA{R: 255, G: 0, B: 255, A: 255}, 2},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			const w, h = 1, 1
			y := &component{pixels: []byte{tc.in.Y}, stride: w}
			cb := &component{pixels: []byte{tc.in.Cb}, stride: w}
			cr := &component{pixels: []byte{tc.in.Cr}, stride: w}
			dst := make([]byte, w*h*4)

			yCbCrToRGBA(y, cb, cr, dst, w, h)

			got := color.RGBA{R: dst[0], G: dst[1], B: dst[2], A: dst[3]}
			if !isClose(got.R, tc.want.R, tc.tolerance) ||
				!isClose(got.G, tc.want.G, tc.tolerance) ||
				!isClose(got.B, tc.want.B, tc.tolerance) ||
				got.A != tc.want.A {
				t.Errorf("yCbCrToRGBA(%v) got RGBA%v, want close to RGBA%v", tc.in, got, tc.want)
			}
		})
	}
}

// TestRGBToRGBA verifies the conversion from separate R, G, B planes to an interleaved RGBA buffer.
func TestRGBToRGBA(t *testing.T) {
	const w, h = 2, 2
	r := &component{pixels: []byte{255, 10, 20, 30}, stride: w}
	g := &component{pixels: []byte{0, 40, 50, 60}, stride: w}
	b := &component{pixels: []byte{128, 70, 80, 90}, stride: w}
	dst := make([]byte, w*h*4)

	want := []byte{
		255, 0, 128, 255, // Row 1, Pixel 1
		10, 40, 70, 255, // Row 1, Pixel 2
		20, 50, 80, 255, // Row 2, Pixel 1
		30, 60, 90, 255, // Row 2, Pixel 2
	}

	rgbToRGBA(r, g, b, dst, w, h)

	if !bytes.Equal(dst, want) {
		t.Errorf("rgbToRGBA failed.\nGot:  %v\nWant: %v", dst, want)
	}
}

// TestGrayToRGBA verifies the conversion from a single grayscale plane to an RGBA buffer.
func TestGrayToRGBA(t *testing.T) {
	const w, h = 2, 2
	c := &component{pixels: []byte{0, 100, 128, 255}, stride: w}
	dst := make([]byte, w*h*4)

	want := []byte{
		0, 0, 0, 255, // Row 1, Pixel 1
		100, 100, 100, 255, // Row 1, Pixel 2
		128, 128, 128, 255, // Row 2, Pixel 1
		255, 255, 255, 255, // Row 2, Pixel 2
	}

	grayToRGBA(c, dst, w, h)

	if !reflect.DeepEqual(dst, want) {
		t.Errorf("grayToRGBA failed.\nGot:  %v\nWant: %v", dst, want)
	}
}

// setupBenchmarkComponents creates component structs with dummy data for benchmarking.
func setupBenchmarkComponents(b *testing.B, w, h int, ncomp int) ([]*component, []byte) {
	b.Helper()
	components := make([]*component, ncomp)
	for i := 0; i < ncomp; i++ {
		// Fill with some arbitrary data.
		pix := make([]byte, w*h)
		for j := range pix {
			pix[j] = byte((j + i*10) % 256)
		}
		components[i] = &component{
			pixels: pix,
			stride: w,
			width:  w,
			height: h,
		}
	}
	dst := make([]byte, w*h*4)
	return components, dst
}

// BenchmarkYCbCrToRGBA measures the performance of the YCbCr to RGBA conversion.
func BenchmarkYCbCrToRGBA(b *testing.B) {
	const w, h = 1920, 1080
	comps, dst := setupBenchmarkComponents(b, w, h, 3)
	y, cb, cr := comps[0], comps[1], comps[2]
	b.ReportAllocs()
	b.SetBytes(int64(w * h * 4))
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		yCbCrToRGBA(y, cb, cr, dst, w, h)
	}
}

// BenchmarkRGBToRGBA measures the performance of converting separate RGB planes to RGBA.
func BenchmarkRGBToRGBA(b *testing.B) {
	const w, h = 1920, 1080
	comps, dst := setupBenchmarkComponents(b, w, h, 3)
	r, g, blue := comps[0], comps[1], comps[2]
	b.ReportAllocs()
	b.SetBytes(int64(w * h * 4))
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		rgbToRGBA(r, g, blue, dst, w, h)
	}
}

// BenchmarkGrayToRGBA measures the performance of converting a grayscale plane to RGBA.
func BenchmarkGrayToRGBA(b *testing.B) {
	const w, h = 1920, 1080
	comps, dst := setupBenchmarkComponents(b, w, h, 1)
	c := comps[0]
	b.ReportAllocs()
	b.SetBytes(int64(w * h * 4))
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		grayToRGBA(c, dst, w, h)
	}
}
