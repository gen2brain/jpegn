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
		if len(got) != len(want) {
			t.Fatalf("Length mismatch: got %d, want %d", len(got), len(want))
		}

		// Find the first differing byte to give a more helpful error.
		for i := range got {
			if got[i] != want[i] {
				t.Errorf("First difference at index %d: got %d, want %d", i, got[i], want[i])
				// Stop after the first difference to avoid spamming logs.
				// For a full debug, uncomment the Logf lines below.
				return
			}
		}

		// Uncomment for full dump on failure
		// t.Logf("Got:  %v", got)
		// t.Logf("Want: %v", want)
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

// TestUpsampleNearestNeighborAssembly validates the assembly implementation against the generic Go implementation
// across various dimensions to catch boundary and pointer bugs.
func TestUpsampleNearestNeighborAssembly(t *testing.T) {
	testCases := []struct {
		name     string
		initialW int
		initialH int
		initialS int // Stride
	}{
		{
			name:     "Original failing case",
			initialW: 19,
			initialH: 5,
			initialS: 24, // Stride > width
		},
		{
			name:     "SIMD aligned width",
			initialW: 32, // Multiple of 16
			initialH: 4,
			initialS: 32, // Stride == width
		},
		{
			name:     "Scalar only width",
			initialW: 7, // Less than 16
			initialH: 3,
			initialS: 10,
		},
		{
			name:     "Large non-aligned image",
			initialW: 131,
			initialH: 67,
			initialS: 140,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			targetW := tc.initialW * 2
			targetH := tc.initialH * 2

			// Create initial pixel data with a predictable pattern.
			initialP := make([]byte, tc.initialH*tc.initialS)
			for y := 0; y < tc.initialH; y++ {
				for x := 0; x < tc.initialW; x++ {
					initialP[y*tc.initialS+x] = byte((y*tc.initialW + x) % 256)
				}
			}

			// Create two identical components to work on.
			cAsm := &component{
				width:  tc.initialW,
				height: tc.initialH,
				pixels: append([]byte(nil), initialP...),
				stride: tc.initialS,
			}

			cGeneric := &component{
				width:  tc.initialW,
				height: tc.initialH,
				pixels: append([]byte(nil), initialP...),
				stride: tc.initialS,
			}

			// Run both the assembly-optimized and generic versions.
			upsampleNearestNeighbor(cAsm, targetW, targetH)
			upsampleNearestNeighborGeneric(cGeneric, targetW, targetH)

			// The results must be identical.
			if cAsm.width != cGeneric.width || cAsm.height != cGeneric.height {
				t.Errorf("Dimension mismatch: ASM is %dx%d, Generic is %dx%d",
					cAsm.width, cAsm.height, cGeneric.width, cGeneric.height)
			}

			if cAsm.stride != cGeneric.stride {
				t.Errorf("Stride mismatch: ASM is %d, Generic is %d", cAsm.stride, cGeneric.stride)
			}

			context := "Assembly vs Generic"
			isEqual(t, cAsm.pixels, cGeneric.pixels, context)
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
