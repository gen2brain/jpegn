package jpegn

import (
	"bytes"
	"image"
	"math"
	"math/rand"
	"os"
	"testing"
)

func TestScaledDecoding(t *testing.T) {
	eachTier(t, testScaledDecoding)
}

func testScaledDecoding(t *testing.T) {
	testCases := []struct {
		file     string
		baseline bool
	}{
		{"testdata/test.420.jpg", true},
		{"testdata/test.420.progressive.jpg", false},
		{"testdata/test.444.jpg", true},
		{"testdata/test.444.progressive.jpg", false},
	}

	for _, tc := range testCases {
		t.Run(tc.file, func(t *testing.T) {
			data, err := os.ReadFile(tc.file)
			if err != nil {
				t.Skipf("Test file not found: %s", tc.file)
			}

			// Decode original image
			origImg, err := Decode(bytes.NewReader(data))
			if err != nil {
				t.Fatalf("Failed to decode original: %v", err)
			}
			origBounds := origImg.Bounds()
			origWidth := origBounds.Dx()
			origHeight := origBounds.Dy()

			t.Logf("Original: %dx%d", origWidth, origHeight)

			// Test each scale factor
			scales := []struct {
				denom          int
				expectedWidth  int
				expectedHeight int
			}{
				{1, origWidth, origHeight},
				{2, (origWidth + 1) / 2, (origHeight + 1) / 2},
				{4, (origWidth + 3) / 4, (origHeight + 3) / 4},
				{8, (origWidth + 7) / 8, (origHeight + 7) / 8},
			}

			for _, scale := range scales {
				t.Run(formatScale(scale.denom), func(t *testing.T) {
					opts := &Options{
						ScaleDenom: scale.denom,
						ToRGBA:     true, // Convert to RGBA for easier comparison
					}
					scaledImg, err := Decode(bytes.NewReader(data), opts)
					if err != nil {
						t.Fatalf("Failed to decode with scale 1/%d: %v", scale.denom, err)
					}

					bounds := scaledImg.Bounds()
					width := bounds.Dx()
					height := bounds.Dy()

					t.Logf("Scale 1/%d: %dx%d", scale.denom, width, height)

					// Verify dimensions
					if width != scale.expectedWidth || height != scale.expectedHeight {
						t.Errorf("Scale 1/%d: got %dx%d, expected %dx%d",
							scale.denom, width, height, scale.expectedWidth, scale.expectedHeight)
					}

					// Verify image is not all black or all white (actually has content)
					rgba, ok := scaledImg.(*image.RGBA)
					if !ok {
						t.Fatalf("Expected *image.RGBA, got %T", scaledImg)
					}

					var sumR, sumG, sumB, sumA uint64
					pixelCount := width * height
					for i := 0; i < len(rgba.Pix); i += 4 {
						sumR += uint64(rgba.Pix[i])
						sumG += uint64(rgba.Pix[i+1])
						sumB += uint64(rgba.Pix[i+2])
						sumA += uint64(rgba.Pix[i+3])
					}

					avgR := sumR / uint64(pixelCount)
					avgG := sumG / uint64(pixelCount)
					avgB := sumB / uint64(pixelCount)
					avgA := sumA / uint64(pixelCount)

					t.Logf("Average color: R=%d G=%d B=%d A=%d", avgR, avgG, avgB, avgA)

					// Image should not be all black (avg < 10) or all white (avg > 245)
					if (avgR < 10 && avgG < 10 && avgB < 10) || (avgR > 245 && avgG > 245 && avgB > 245) {
						t.Errorf("Image appears to be corrupted (all black or all white)")
					}

					// Alpha should be 255
					if avgA != 255 {
						t.Errorf("Alpha channel corrupted: avg=%d, expected 255", avgA)
					}

					// Verify we have some variance (not a solid color)
					var varR, varG, varB uint64
					for i := 0; i < len(rgba.Pix); i += 4 {
						diffR := int64(rgba.Pix[i]) - int64(avgR)
						diffG := int64(rgba.Pix[i+1]) - int64(avgG)
						diffB := int64(rgba.Pix[i+2]) - int64(avgB)
						varR += uint64(diffR * diffR)
						varG += uint64(diffG * diffG)
						varB += uint64(diffB * diffB)
					}
					varR /= uint64(pixelCount)
					varG /= uint64(pixelCount)
					varB /= uint64(pixelCount)

					t.Logf("Variance: R=%d G=%d B=%d", varR, varG, varB)

					// Should have some variance (not a solid color)
					if varR < 10 && varG < 10 && varB < 10 {
						t.Errorf("Image appears to be a solid color (no variance)")
					}
				})
			}
		})
	}
}

// TestScaledVsDownsampled verifies that scaled IDCT produces reasonable results
// compared to full decode + downsample
func TestScaledVsDownsampled(t *testing.T) {
	eachTier(t, testScaledVsDownsampled)
}

func testScaledVsDownsampled(t *testing.T) {
	data, err := os.ReadFile("testdata/test.420.jpg")
	if err != nil {
		t.Skip("Test file not found")
	}

	// Decode full size
	fullImg, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true})
	if err != nil {
		t.Fatalf("Failed to decode full size: %v", err)
	}

	fullRGBA := fullImg.(*image.RGBA)
	fullWidth := fullRGBA.Bounds().Dx()
	fullHeight := fullRGBA.Bounds().Dy()

	// Test 1/2 scaling
	scaledImg, err := Decode(bytes.NewReader(data), &Options{ScaleDenom: 2, ToRGBA: true})
	if err != nil {
		t.Fatalf("Failed to decode with scale 1/2: %v", err)
	}

	scaledRGBA := scaledImg.(*image.RGBA)
	scaledWidth := scaledRGBA.Bounds().Dx()
	scaledHeight := scaledRGBA.Bounds().Dy()

	expectedWidth := (fullWidth + 1) / 2
	expectedHeight := (fullHeight + 1) / 2

	if scaledWidth != expectedWidth || scaledHeight != expectedHeight {
		t.Fatalf("Scaled dimensions wrong: got %dx%d, expected %dx%d",
			scaledWidth, scaledHeight, expectedWidth, expectedHeight)
	}

	// Manually downsample the full image using nearest neighbor for comparison
	downsampled := image.NewRGBA(image.Rect(0, 0, expectedWidth, expectedHeight))
	for y := 0; y < expectedHeight; y++ {
		for x := 0; x < expectedWidth; x++ {
			srcX := x * 2
			srcY := y * 2
			if srcX >= fullWidth {
				srcX = fullWidth - 1
			}
			if srcY >= fullHeight {
				srcY = fullHeight - 1
			}
			c := fullRGBA.RGBAAt(srcX, srcY)
			downsampled.SetRGBA(x, y, c)
		}
	}

	// Compare scaled IDCT result with downsampled result
	// They won't be identical, but should be reasonably similar
	var totalDiff uint64
	var maxDiff uint8
	pixelCount := scaledWidth * scaledHeight

	for y := 0; y < scaledHeight; y++ {
		for x := 0; x < scaledWidth; x++ {
			sc := scaledRGBA.RGBAAt(x, y)
			dc := downsampled.RGBAAt(x, y)

			diffR := absDiff(sc.R, dc.R)
			diffG := absDiff(sc.G, dc.G)
			diffB := absDiff(sc.B, dc.B)

			totalDiff += uint64(diffR) + uint64(diffG) + uint64(diffB)
			if diffR > maxDiff {
				maxDiff = diffR
			}
			if diffG > maxDiff {
				maxDiff = diffG
			}
			if diffB > maxDiff {
				maxDiff = diffB
			}
		}
	}

	avgDiff := float64(totalDiff) / float64(pixelCount*3)
	t.Logf("Average per-channel difference: %.2f", avgDiff)
	t.Logf("Maximum per-channel difference: %d", maxDiff)

	// The scaled IDCT should be reasonably close to downsampled
	// Note: Scaled IDCT is actually BETTER quality than nearest neighbor downsampling
	// so some difference is expected and desired. We just want to verify it's in the ballpark.
	// Allow average difference up to 50 per channel
	if avgDiff > 50 {
		t.Errorf("Scaled IDCT differs too much from downsampled: avg=%.2f (max diff=%d)", avgDiff, maxDiff)
	} else {
		t.Logf("Scaled IDCT quality comparison: PASS (difference is expected - scaled IDCT uses more frequency information)")
	}
}

// TestScaledNativeColorspace verifies scaled decoding works with native colorspaces
func TestScaledNativeColorspace(t *testing.T) {
	eachTier(t, testScaledNativeColorspace)
}

func testScaledNativeColorspace(t *testing.T) {
	data, err := os.ReadFile("testdata/test.420.jpg")
	if err != nil {
		t.Skip("Test file not found")
	}

	// Test with native YCbCr output
	scaledImg, err := Decode(bytes.NewReader(data), &Options{ScaleDenom: 2, ToRGBA: false})
	if err != nil {
		t.Fatalf("Failed to decode: %v", err)
	}

	ycbcr, ok := scaledImg.(*image.YCbCr)
	if !ok {
		t.Fatalf("Expected *image.YCbCr, got %T", scaledImg)
	}

	width := ycbcr.Bounds().Dx()
	height := ycbcr.Bounds().Dy()

	// Verify YCbCr planes are correctly sized
	yStride := ycbcr.YStride
	cStride := ycbcr.CStride

	t.Logf("YCbCr image: %dx%d, YStride=%d, CStride=%d", width, height, yStride, cStride)

	// Check Y plane
	if len(ycbcr.Y) < yStride*height {
		t.Errorf("Y plane too small: got %d bytes, need at least %d", len(ycbcr.Y), yStride*height)
	}

	// Check that image has content
	var sumY uint64
	for _, y := range ycbcr.Y[:yStride*height] {
		sumY += uint64(y)
	}
	avgY := sumY / uint64(yStride*height)
	t.Logf("Average Y: %d", avgY)

	if avgY < 10 || avgY > 245 {
		t.Errorf("Y channel appears corrupted: avg=%d", avgY)
	}
}

func formatScale(denom int) string {
	switch denom {
	case 1:
		return "Scale1/1"
	case 2:
		return "Scale1/2"
	case 4:
		return "Scale1/4"
	case 8:
		return "Scale1/8"
	default:
		return "ScaleUnknown"
	}
}

func absDiff(a, b uint8) uint8 {
	if a > b {
		return a - b
	}
	return b - a
}

func BenchmarkScaledDecoding(b *testing.B) {
	data, err := os.ReadFile("testdata/test.420.jpg")
	if err != nil {
		b.Skip("Test file not found")
	}

	scales := []int{1, 2, 4, 8}

	for _, scale := range scales {
		b.Run(formatScale(scale), func(b *testing.B) {
			opts := &Options{ScaleDenom: scale, ToRGBA: true}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err := Decode(bytes.NewReader(data), opts)
				if err != nil {
					b.Fatalf("Decode failed: %v", err)
				}
			}
		})
	}
}

func BenchmarkScaledVsDownsample(b *testing.B) {
	data, err := os.ReadFile("testdata/test.420.jpg")
	if err != nil {
		b.Skip("Test file not found")
	}

	b.Run("Scaled1/2_IDCT", func(b *testing.B) {
		opts := &Options{ScaleDenom: 2, ToRGBA: true}
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_, err := Decode(bytes.NewReader(data), opts)
			if err != nil {
				b.Fatalf("Decode failed: %v", err)
			}
		}
	})

	b.Run("Full+Downsample", func(b *testing.B) {
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			// Decode full size
			fullImg, err := Decode(bytes.NewReader(data), &Options{ToRGBA: true})
			if err != nil {
				b.Fatalf("Decode failed: %v", err)
			}

			// Downsample to half size using nearest neighbor
			fullRGBA := fullImg.(*image.RGBA)
			fullWidth := fullRGBA.Bounds().Dx()
			fullHeight := fullRGBA.Bounds().Dy()
			halfWidth := (fullWidth + 1) / 2
			halfHeight := (fullHeight + 1) / 2

			downsampled := image.NewRGBA(image.Rect(0, 0, halfWidth, halfHeight))
			for y := 0; y < halfHeight; y++ {
				for x := 0; x < halfWidth; x++ {
					srcX := x * 2
					srcY := y * 2
					if srcX >= fullWidth {
						srcX = fullWidth - 1
					}
					if srcY >= fullHeight {
						srcY = fullHeight - 1
					}
					c := fullRGBA.RGBAAt(srcX, srcY)
					downsampled.SetRGBA(x, y, c)
				}
			}
		}
	})
}

// refReduced evaluates the true n-point inverse DCT over the top-left n by n
// coefficients, the reduction the scaled transforms approximate.
func refReduced(blk *[64]int32, n int) []float64 {
	out := make([]float64, n*n)

	for y := 0; y < n; y++ {
		for x := 0; x < n; x++ {
			sum := 0.0

			for v := 0; v < n; v++ {
				for u := 0; u < n; u++ {
					cu, cv := 1.0, 1.0
					if u == 0 {
						cu = 1 / math.Sqrt2
					}

					if v == 0 {
						cv = 1 / math.Sqrt2
					}

					sum += cu * cv * float64(blk[v*8+u]) *
						math.Cos(float64(2*x+1)*float64(u)*math.Pi/(2*float64(n))) *
						math.Cos(float64(2*y+1)*float64(v)*math.Pi/(2*float64(n)))
				}
			}

			out[y*n+x] = math.Max(0, math.Min(255, sum/4+128))
		}
	}

	return out
}

// TestScaledIdctAgainstReference checks each reduced transform against the
// inverse DCT it stands for, which the end-to-end tests cannot isolate.
func TestScaledIdctAgainstReference(t *testing.T) {
	eachTier(t, testScaledIdctAgainstReference)
}

func testScaledIdctAgainstReference(t *testing.T) {
	rng := rand.New(rand.NewSource(5))

	cases := []struct {
		name string
		n    int
		fn   func(*[64]int32, []byte, int, int)
	}{
		{"4x4", 4, idct8x8To4x4},
		{"2x2", 2, idct8x8To2x2},
		{"1x1", 1, idct8x8To1x1},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var sum, worst float64

			var cnt int

			for iter := 0; iter < 2000; iter++ {
				var blk [64]int32

				blk[0] = int32(rng.Intn(2048)) - 1024
				for i := 1; i < 64; i++ {
					if rng.Intn(3) == 0 {
						blk[i] = int32(rng.Intn(256)) - 128
					}
				}

				out := make([]byte, tc.n*tc.n)
				tc.fn(&blk, out, 0, tc.n)

				ref := refReduced(&blk, tc.n)

				for i := range out {
					d := math.Abs(float64(out[i]) - ref[i])
					sum += d
					cnt++

					if d > worst {
						worst = d
					}
				}
			}

			mean := sum / float64(cnt)

			t.Logf("mean=%.3f max=%.3f", mean, worst)

			if mean > 0.5 || worst > 1.5 {
				t.Errorf("mean %.3f max %.3f, want mean under 0.5 and max under 1.5", mean, worst)
			}
		})
	}
}

// TestScaledIdctMatchesScalar checks the assembly 4x4 transform against the
// pure Go one over the coefficient range the decoder clamps to.
func TestScaledIdctMatchesScalar(t *testing.T) {
	eachTier(t, testScaledIdctMatchesScalar)
}

func testScaledIdctMatchesScalar(t *testing.T) {
	rng := rand.New(rand.NewSource(9))

	const stride = 13

	for iter := 0; iter < 200000; iter++ {
		var blk [64]int32

		switch iter % 4 {
		case 0:
			blk[0] = dequantLimit
		case 1:
			for i := 0; i < 32; i++ {
				blk[i] = -dequantLimit
			}
		case 2:
			for i := range blk {
				blk[i] = int32(rng.Intn(2*dequantLimit+1)) - dequantLimit
			}
		default:
			blk[0] = int32(rng.Intn(2048)) - 1024
			for i := 1; i < 64; i++ {
				if rng.Intn(3) == 0 {
					blk[i] = int32(rng.Intn(256)) - 128
				}
			}
		}

		want := make([]byte, stride*4)
		got := make([]byte, stride*4)

		idct8x8To4x4(&blk, want, 0, stride)
		idctScaled(&blk, got, 0, stride, 2)

		for i := range want {
			if got[i] != want[i] {
				t.Fatalf("iter %d byte %d: got %d, want %d", iter, i, got[i], want[i])
			}
		}
	}
}
