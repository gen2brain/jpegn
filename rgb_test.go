package jpegn

import (
	"math/rand"
	"testing"
)

// TestRGBToYCbCrRowMatchesScalar checks the row kernel against the reference.
func TestRGBToYCbCrRowMatchesScalar(t *testing.T) {
	rng := rand.New(rand.NewSource(7))

	for _, n := range []int{0, 1, 2, 7, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 255, 256} {
		src := make([]byte, (n+1)*4)
		for i := range src {
			src[i] = byte(rng.Intn(256))
		}

		wantY := make([]byte, n+8)
		wantCb := make([]byte, n+8)
		wantCr := make([]byte, n+8)
		rgbToYCbCrRowScalar(wantY, wantCb, wantCr, src, n)

		gotY := make([]byte, n+8)
		gotCb := make([]byte, n+8)
		gotCr := make([]byte, n+8)
		rgbToYCbCrRow(gotY, gotCb, gotCr, src, n)

		for i := 0; i < n; i++ {
			if gotY[i] != wantY[i] || gotCb[i] != wantCb[i] || gotCr[i] != wantCr[i] {
				t.Fatalf("n=%d pixel %d rgb(%d,%d,%d): got (%d,%d,%d), want (%d,%d,%d)",
					n, i, src[i*4], src[i*4+1], src[i*4+2],
					gotY[i], gotCb[i], gotCr[i], wantY[i], wantCb[i], wantCr[i])
			}
		}

		for i := n; i < n+8; i++ {
			if gotY[i] != 0 || gotCb[i] != 0 || gotCr[i] != 0 {
				t.Fatalf("n=%d: wrote past end at %d", n, i)
			}
		}
	}
}

// TestRGBToYCbCrExhaustive checks every RGB triple through the row kernel.
func TestRGBToYCbCrExhaustive(t *testing.T) {
	const n = 256

	src := make([]byte, n*4)
	gotY := make([]byte, n)
	gotCb := make([]byte, n)
	gotCr := make([]byte, n)

	for r := 0; r < 256; r++ {
		for g := 0; g < 256; g++ {
			for b := 0; b < 256; b++ {
				src[b*4] = byte(r)
				src[b*4+1] = byte(g)
				src[b*4+2] = byte(b)
			}

			rgbToYCbCrRow(gotY, gotCb, gotCr, src, n)

			for b := 0; b < 256; b++ {
				wy, wcb, wcr := rgbToYCbCr(byte(r), byte(g), byte(b))
				if gotY[b] != wy || gotCb[b] != wcb || gotCr[b] != wcr {
					t.Fatalf("rgb(%d,%d,%d): got (%d,%d,%d), want (%d,%d,%d)",
						r, g, b, gotY[b], gotCb[b], gotCr[b], wy, wcb, wcr)
				}
			}
		}
	}
}

func BenchmarkRGBToYCbCrRow(b *testing.B) {
	const n = 1024

	src := make([]byte, n*4)
	for i := range src {
		src[i] = byte(i * 7)
	}

	dstY := make([]byte, n)
	dstCb := make([]byte, n)
	dstCr := make([]byte, n)

	b.ResetTimer()
	b.SetBytes(int64(n))

	for i := 0; i < b.N; i++ {
		rgbToYCbCrRow(dstY, dstCb, dstCr, src, n)
	}
}

func BenchmarkRGBToYCbCrRowScalar(b *testing.B) {
	const n = 1024

	src := make([]byte, n*4)
	for i := range src {
		src[i] = byte(i * 7)
	}

	dstY := make([]byte, n)
	dstCb := make([]byte, n)
	dstCr := make([]byte, n)

	b.ResetTimer()
	b.SetBytes(int64(n))

	for i := 0; i < b.N; i++ {
		rgbToYCbCrRowScalar(dstY, dstCb, dstCr, src, n)
	}
}
