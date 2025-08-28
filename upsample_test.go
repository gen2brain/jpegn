package jpegn

import (
	"bytes"
	"testing"
)

// isEqual is a helper function to compare two byte slices and report differences.
func isEqual(t *testing.T, got, want []byte, context string) {
	t.Helper()

	if !bytes.Equal(got, want) {
		t.Errorf("%s: pixel data mismatch", context)
		t.Logf("Got:  %v", got)
		t.Logf("Want: %v", want)
	}
}

// TestUpsampleNearestNeighbor verifies the nearest-neighbor upsampling implementation.
func TestUpsampleNearestNeighbor(t *testing.T) {
	tests := []struct {
		name      string
		initialW  int
		initialH  int
		initialP  []byte
		initialS  int
		targetW   int
		targetH   int
		expectedW int
		expectedH int
		expectedP []byte
	}{
		{
			name:      "2x2 to 4x4",
			initialW:  2,
			initialH:  2,
			initialP:  []byte{10, 20, 30, 40},
			initialS:  2,
			targetW:   4,
			targetH:   4,
			expectedW: 4,
			expectedH: 4,
			expectedP: []byte{
				10, 10, 20, 20,
				10, 10, 20, 20,
				30, 30, 40, 40,
				30, 30, 40, 40,
			},
		},
		{
			name:      "1x2 to 4x4",
			initialW:  1,
			initialH:  2,
			initialP:  []byte{100, 200},
			initialS:  1,
			targetW:   4,
			targetH:   4,
			expectedW: 4,
			expectedH: 4,
			expectedP: []byte{
				100, 100, 100, 100,
				100, 100, 100, 100,
				200, 200, 200, 200,
				200, 200, 200, 200,
			},
		},
		{
			name:      "No change",
			initialW:  2,
			initialH:  2,
			initialP:  []byte{1, 2, 3, 4},
			initialS:  2,
			targetW:   2,
			targetH:   2,
			expectedW: 2,
			expectedH: 2,
			expectedP: []byte{1, 2, 3, 4},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &component{
				width:  tt.initialW,
				height: tt.initialH,
				pixels: tt.initialP,
				stride: tt.initialS,
			}

			upsampleNearestNeighbor(c, tt.targetW, tt.targetH)

			if c.width != tt.expectedW || c.height != tt.expectedH {
				t.Errorf("Expected dimensions %dx%d, got %dx%d", tt.expectedW, tt.expectedH, c.width, c.height)
			}

			if c.stride != tt.expectedW {
				t.Errorf("Expected stride %d, got %d", tt.expectedW, c.stride)
			}

			isEqual(t, c.pixels, tt.expectedP, tt.name)
		})
	}
}

// TestUpsampleHCatmullRom verifies the horizontal Catmull-Rom upsampling.
// The expected values are pre-calculated based on the filter constants and symmetric boundary conditions.
func TestUpsampleHCatmullRom(t *testing.T) {
	tests := []struct {
		name           string
		inputPixels    []byte
		expectedPixels []byte
	}{
		{
			name:           "8 pixels",
			inputPixels:    []byte{10, 20, 50, 100, 150, 200, 230, 250},
			expectedPixels: []byte{9, 11, 16, 26, 41, 61, 87, 113, 138, 163, 189, 209, 224, 236, 247, 252},
		},
		{
			name:           "5 pixels (boundary test)",
			inputPixels:    []byte{10, 50, 100, 150, 200},
			expectedPixels: []byte{7, 16, 38, 62, 87, 113, 138, 164, 192, 204},
		},
		{
			name:           "3 pixels (minimum size)",
			inputPixels:    []byte{10, 128, 250},
			expectedPixels: []byte{0, 29, 94, 163, 230, 255},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &component{
				width:  len(tt.inputPixels),
				height: 1,
				stride: len(tt.inputPixels),
				pixels: append([]byte(nil), tt.inputPixels...), // Make a copy
			}

			upsampleH(c)

			expectedWidth := len(tt.inputPixels) * 2
			if c.width != expectedWidth {
				t.Errorf("Expected width %d, got %d", expectedWidth, c.width)
			}

			if c.stride != expectedWidth {
				t.Errorf("Expected stride %d, got %d", expectedWidth, c.stride)
			}

			isEqual(t, c.pixels, tt.expectedPixels, tt.name)
		})
	}
}

// TestUpsampleVCatmullRom verifies the vertical Catmull-Rom upsampling.
// The filter logic is identical to horizontal, so the output values should be the same.
func TestUpsampleVCatmullRom(t *testing.T) {
	tests := []struct {
		name           string
		inputPixels    []byte
		expectedPixels []byte
	}{
		{
			name:           "8 pixels",
			inputPixels:    []byte{10, 20, 50, 100, 150, 200, 230, 250},
			expectedPixels: []byte{9, 11, 16, 26, 41, 61, 87, 113, 138, 163, 189, 209, 224, 236, 247, 252},
		},
		{
			name:           "5 pixels (boundary test)",
			inputPixels:    []byte{10, 50, 100, 150, 200},
			expectedPixels: []byte{7, 16, 38, 62, 87, 113, 138, 164, 192, 204},
		},
		{
			name:           "3 pixels (minimum size)",
			inputPixels:    []byte{10, 128, 250},
			expectedPixels: []byte{0, 29, 94, 163, 230, 255},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &component{
				width:  1,
				height: len(tt.inputPixels),
				stride: 1,
				pixels: append([]byte(nil), tt.inputPixels...), // Make a copy
			}

			upsampleV(c)

			expectedHeight := len(tt.inputPixels) * 2
			if c.height != expectedHeight {
				t.Errorf("Expected height %d, got %d", expectedHeight, c.height)
			}

			if c.stride != 1 {
				t.Errorf("Expected stride 1, got %d", c.stride)
			}

			isEqual(t, c.pixels, tt.expectedPixels, tt.name)
		})
	}
}

// BenchmarkUpsampleNearestNeighbor measures the performance of the nearest-neighbor upsampling.
func BenchmarkUpsampleNearestNeighbor(b *testing.B) {
	const (
		initialW = 512
		initialH = 512
		targetW  = 1024
		targetH  = 1024
	)

	// Pre-allocate a sample image outside the loop.
	initialPixels := make([]byte, initialW*initialH)

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Reset the component for each iteration.
		c := &component{
			width:  initialW,
			height: initialH,
			stride: initialW,
			pixels: initialPixels,
		}

		upsampleNearestNeighbor(c, targetW, targetH)
	}
}

// BenchmarkUpsampleCatmullRom measures the performance of the Catmull-Rom upsampling.
func BenchmarkUpsampleCatmullRom(b *testing.B) {
	const (
		initialW = 512
		initialH = 512
		targetW  = 1024
		targetH  = 1024
	)

	// Pre-allocate a sample image outside the loop.
	initialPixels := make([]byte, initialW*initialH)

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Reset the component for each iteration. The upsample functions
		// allocate a new pixel buffer and replace c.pixels, so the
		// original initialPixels is not modified.
		c := &component{
			width:  initialW,
			height: initialH,
			stride: initialW,
			pixels: initialPixels,
		}

		upsampleCatmullRom(c, targetW, targetH)
	}
}
