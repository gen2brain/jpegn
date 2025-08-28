package jpegn

import (
	"bytes"
	"fmt"
	"testing"
)

// idctTestBlock is a set of DCT coefficients used as input for the test.
// This block has a single non-zero DC coefficient (512) and all AC coefficients are zero.
// The IDCT of such a block should result in a flat 8x8 block where every pixel has the same value.
var idctTestBlock = [64]int32{
	512, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
}

// idctTestPixels is the expected 8x8 pixel block output after applying the IDCT.
// The result should be a flat block where every pixel value is 192.
// This is calculated as (512 / 8) + 128 = 64 + 128 = 192.
var idctTestPixels = [64]byte{
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
	192, 192, 192, 192, 192, 192, 192, 192,
}

// idctTestBlockAC is a test block with non-zero AC coefficients to test the main transform logic.
var idctTestBlockAC = [64]int32{
	0, 20, 0, 0, 0, 0, 0, 0,
	-30, 0, 15, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
}

// idctTestPixelsAC is the expected output for idctTestBlockAC.
// These values are pre-calculated from a known-good IDCT implementation.
var idctTestPixelsAC = [64]byte{
	130, 127, 123, 120, 119, 119, 121, 123,
	130, 128, 124, 121, 120, 120, 122, 123,
	130, 129, 126, 124, 122, 122, 123, 124,
	131, 130, 129, 127, 126, 125, 124, 124,
	132, 132, 131, 130, 129, 127, 126, 125,
	132, 133, 134, 134, 132, 130, 127, 126,
	133, 134, 136, 136, 135, 132, 128, 126,
	133, 135, 137, 137, 136, 133, 129, 126,
}

// idctHelper performs a full 8x8 2D IDCT.
func idctHelper(block *[64]int32) [64]byte {
	// We must pass a copy because idct (specifically the Go fallback path) modifies the block in place.
	// The optimized assembly path does not modify the input block.
	b := *block
	var out [64]byte

	// The output stride for a standard 8x8 block is 8.
	idct(&b, out[:], 0, 8)

	return out
}

// printBlock is a helper for formatting an 8x8 block for readable test output.
func printBlock(t *testing.T, block []byte) {
	var buf bytes.Buffer

	for i := 0; i < 64; i++ {
		if i > 0 && i%8 == 0 {
			buf.WriteString("\n")
		}

		buf.WriteString(fmt.Sprintf("%4d", block[i]))
	}

	t.Log("\n" + buf.String())
}

// TestIdctDC verifies the IDCT implementation for the DC-only case.
// This tests the optimization path in both rowIDCT and colIDCT.
func TestIdctDC(t *testing.T) {
	block := idctTestBlock
	pixels := idctHelper(&block)

	for i, want := range idctTestPixels {
		if got := pixels[i]; got != want {
			t.Errorf("IDCT DC mismatch at index %d: got %d, want %d", i, got, want)
			t.Log("Got pixels:")
			printBlock(t, pixels[:])
			t.Log("Want pixels:")
			printBlock(t, idctTestPixels[:])
			t.FailNow()
		}
	}
}

// TestIdctAC verifies the IDCT implementation for a general case with AC coefficients.
// This tests the main logic of the AAN fast IDCT algorithm.
func TestIdctAC(t *testing.T) {
	block := idctTestBlockAC
	pixels := idctHelper(&block)

	for i, want := range idctTestPixelsAC {
		if got := pixels[i]; got != want {
			t.Errorf("IDCT AC mismatch at index %d: got %d, want %d", i, got, want)
			t.Log("Got pixels:")
			printBlock(t, pixels[:])
			t.Log("Want pixels:")
			printBlock(t, idctTestPixelsAC[:])
			t.FailNow()
		}
	}
}

// TestIdctACStrided verifies the IDCT implementation for a case with a non-8 stride.
func TestIdctACStrided(t *testing.T) {
	const stride = 16
	block := idctTestBlockAC
	// The output buffer must be large enough for 8 rows with the given stride.
	out := make([]byte, 7*stride+8)

	// We must pass a copy because the Go fallback path modifies the block.
	b := block
	idct(&b, out, 0, stride)

	// Verify the output by comparing it to the expected pixels, accounting for the stride.
	for r := 0; r < 8; r++ {
		for c := 0; c < 8; c++ {
			want := idctTestPixelsAC[r*8+c]
			got := out[r*stride+c]
			if got != want {
				t.Errorf("IDCT AC (strided) mismatch at row %d, col %d: got %d, want %d", r, c, got, want)

				// De-stride the output for easier comparison printing
				var gotFlat [64]byte
				for r2 := 0; r2 < 8; r2++ {
					for c2 := 0; c2 < 8; c2++ {
						gotFlat[r2*8+c2] = out[r2*stride+c2]
					}
				}

				t.Log("Got pixels (de-strided):")
				printBlock(t, gotFlat[:])
				t.Log("Want pixels:")
				printBlock(t, idctTestPixelsAC[:])
				t.FailNow()
			}
		}
	}
}

// BenchmarkIdct measures the performance of the full 8x8 IDCT process.
func BenchmarkIdct(b *testing.B) {
	// Use the AC test block as a representative input.
	block := idctTestBlockAC
	var out [64]byte

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		currentBlock := block // Clone
		idct(&currentBlock, out[:], 0, 8)
	}
}
