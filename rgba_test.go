package jpegn

import (
	"bytes"
	"fmt"
	"image/color"
	"math/rand"
	"reflect"
	"testing"
)

// Helper functions to generate random image component data for testing.
func makeRandComponent(w, h int) *component {
	pix := make([]byte, w*h)

	// Using a fixed seed for reproducible tests.
	r := rand.New(rand.NewSource(int64(w*h + len(pix))))
	for i := range pix {
		pix[i] = byte(r.Intn(256))
	}

	return &component{pixels: pix, stride: w, width: w, height: h}
}

func makeRandYCbCr(w, h int) (y, cb, cr *component) {
	return makeRandComponent(w, h), makeRandComponent(w, h), makeRandComponent(w, h)
}

func makeRandRGB(w, h int) (r, g, b *component) {
	return makeRandComponent(w, h), makeRandComponent(w, h), makeRandComponent(w, h)
}

func makeRandGray(w, h int) *component {
	return makeRandComponent(w, h)
}

// makeRandComponentStride builds a component with stride > width, padding the
// extra columns with a sentinel so a stride-unaware conversion produces wrong output.
func makeRandComponentStride(w, h, stride int) *component {
	pix := make([]byte, stride*h)
	r := rand.New(rand.NewSource(int64(w*h + stride)))
	for row := 0; row < h; row++ {
		for x := 0; x < stride; x++ {
			if x < w {
				pix[row*stride+x] = byte(r.Intn(256))
			} else {
				pix[row*stride+x] = 0xAA
			}
		}
	}

	return &component{pixels: pix, stride: stride, width: w, height: h}
}

// TestRGBAStrideHandling verifies the SIMD conversions honor stride != width.
func TestRGBAStrideHandling(t *testing.T) {
	cases := []struct{ w, h, stride int }{
		{487, 17, 496}, // luma-like padding, width not a multiple of 16/32
		{17, 5, 32},
		{33, 3, 48},
		{1, 1, 8},
		{100, 8, 128},
	}

	for _, tc := range cases {
		name := fmt.Sprintf("%dx%d_stride%d", tc.w, tc.h, tc.stride)
		t.Run(name, func(t *testing.T) {
			t.Run("YCbCr", func(t *testing.T) {
				y := makeRandComponentStride(tc.w, tc.h, tc.stride)
				cb := makeRandComponentStride(tc.w, tc.h, tc.stride)
				cr := makeRandComponentStride(tc.w, tc.h, tc.stride)
				got := make([]byte, tc.w*tc.h*4)
				want := make([]byte, tc.w*tc.h*4)

				yCbCrToRGBA(y, cb, cr, got, tc.w, tc.h)
				yCbCrToRGBAScalar(y, cb, cr, want, tc.w, tc.h)

				if !bytes.Equal(got, want) {
					t.Fatalf("yCbCrToRGBA mismatch with stride != width\n%s", findFirstDiff(got, want, tc.w))
				}
			})

			t.Run("RGB", func(t *testing.T) {
				r := makeRandComponentStride(tc.w, tc.h, tc.stride)
				g := makeRandComponentStride(tc.w, tc.h, tc.stride)
				b := makeRandComponentStride(tc.w, tc.h, tc.stride)
				got := make([]byte, tc.w*tc.h*4)
				want := make([]byte, tc.w*tc.h*4)

				rgbToRGBA(r, g, b, got, tc.w, tc.h)
				rgbToRGBAScalar(r, g, b, want, tc.w, tc.h)

				if !bytes.Equal(got, want) {
					t.Fatalf("rgbToRGBA mismatch with stride != width\n%s", findFirstDiff(got, want, tc.w))
				}
			})

			t.Run("Gray", func(t *testing.T) {
				c := makeRandComponentStride(tc.w, tc.h, tc.stride)
				got := make([]byte, tc.w*tc.h*4)
				want := make([]byte, tc.w*tc.h*4)

				grayToRGBA(c, got, tc.w, tc.h)
				grayToRGBAScalar(c, want, tc.w, tc.h)

				if !bytes.Equal(got, want) {
					t.Fatalf("grayToRGBA mismatch with stride != width\n%s", findFirstDiff(got, want, tc.w))
				}
			})
		})
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
		{"Cyan", color.YCbCr{Y: 178, Cb: 171, Cr: 0}, color.RGBA{R: 0, G: 255, B: 255, A: 255}, 3},
		{"Yellow", color.YCbCr{Y: 225, Cb: 0, Cr: 149}, color.RGBA{R: 255, G: 255, B: 0, A: 255}, 3},
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

// findFirstDiff finds the first differing pixel between two RGBA buffers and returns a formatted string.
func findFirstDiff(got, want []byte, w int) string {
	for i := 0; i < len(got)/4; i++ {
		gotPixel := got[i*4 : i*4+4]
		wantPixel := want[i*4 : i*4+4]

		if !bytes.Equal(gotPixel, wantPixel) {
			x := i % w
			y := i / w

			return fmt.Sprintf("first difference at pixel index %d (x:%d, y:%d):\nGot:  RGBA%v\nWant: RGBA%v", i, x, y, gotPixel, wantPixel)
		}
	}

	return "buffers are identical (this should not be reached in case of failure)"
}

// TestAssemblyVsScalarImplementation compares the AVX2 implementation against the pure Go scalar version
// for various image sizes to ensure correctness, especially with remainder handling.
func TestAssemblyVsScalarImplementation(t *testing.T) {
	testCases := []struct{ w, h int }{
		{1, 1},
		{15, 1}, // Less than one 16-pixel chunk
		{16, 1}, // Exactly one 16-pixel chunk
		{17, 1}, // One chunk + remainder
		{31, 1}, // Less than one 32-pixel chunk
		{32, 1}, // Exactly one 32-pixel chunk
		{33, 1}, // One chunk + remainder
		{63, 1},
		{64, 1},
		{65, 1},
		{100, 100},
		{127, 99},
	}

	for _, tc := range testCases {
		name := fmt.Sprintf("%dx%d", tc.w, tc.h)
		t.Run(name, func(t *testing.T) {
			// Test YCbCrToRGBA
			t.Run("YCbCr", func(t *testing.T) {
				y, cb, cr := makeRandYCbCr(tc.w, tc.h)
				dstGot := make([]byte, tc.w*tc.h*4)
				dstWant := make([]byte, tc.w*tc.h*4)

				yCbCrToRGBA(y, cb, cr, dstGot, tc.w, tc.h)
				yCbCrToRGBAScalar(y, cb, cr, dstWant, tc.w, tc.h)

				if !bytes.Equal(dstGot, dstWant) {
					t.Fatalf("AVX2 implementation does not match scalar for yCbCrToRGBA\n%s", findFirstDiff(dstGot, dstWant, tc.w))
				}
			})

			// Test RGBToRGBA
			t.Run("RGB", func(t *testing.T) {
				r, g, b := makeRandRGB(tc.w, tc.h)
				dstGot := make([]byte, tc.w*tc.h*4)
				dstWant := make([]byte, tc.w*tc.h*4)

				rgbToRGBA(r, g, b, dstGot, tc.w, tc.h)
				rgbToRGBAScalar(r, g, b, dstWant, tc.w, tc.h)

				if !bytes.Equal(dstGot, dstWant) {
					t.Fatalf("AVX2 implementation does not match scalar for rgbToRGBA\n%s", findFirstDiff(dstGot, dstWant, tc.w))
				}
			})

			// Test GrayToRGBA
			t.Run("Gray", func(t *testing.T) {
				c := makeRandGray(tc.w, tc.h)
				dstGot := make([]byte, tc.w*tc.h*4)
				dstWant := make([]byte, tc.w*tc.h*4)

				grayToRGBA(c, dstGot, tc.w, tc.h)
				grayToRGBAScalar(c, dstWant, tc.w, tc.h)

				if !bytes.Equal(dstGot, dstWant) {
					t.Fatalf("AVX2 implementation does not match scalar for grayToRGBA\n%s", findFirstDiff(dstGot, dstWant, tc.w))
				}
			})
		})
	}
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
