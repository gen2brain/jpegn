package jpegn

import (
	"math/rand"
	"testing"
)

// quantTables builds the reciprocal and rounding tables for one quality value.
func quantTables(qval int) (recip, half [64]int32) {
	for i := 0; i < 64; i++ {
		d := int32(qval)*8 + int32(i%7)*8
		if d < 8 {
			d = 8
		}

		half[i] = d >> 1
		recip[i] = int32((1<<quantShift + int64(d) - 1) / int64(d))
	}

	return recip, half
}

// TestQuantizeBlockMatchesScalar checks the assembly path against pure Go.
func TestQuantizeBlockMatchesScalar(t *testing.T) {
	rng := rand.New(rand.NewSource(11))

	edges := []int32{0, 1, -1, 7, -7, 1023, -1023, 1024, -1024,
		8191, -8191, 8192, -8192, 32767, -32768}

	for _, qval := range []int{1, 2, 3, 16, 99, 128, 255} {
		recip, half := quantTables(qval)

		for n := 0; n < 4000; n++ {
			var src [64]int32

			for i := range src {
				switch n % 4 {
				case 0:
					src[i] = edges[rng.Intn(len(edges))]
				case 1:
					src[i] = int32(rng.Intn(16385)) - 8192
				case 2:
					src[i] = 0
				default:
					src[i] = int32(rng.Intn(2049)) - 1024
				}
			}

			var want, got [64]int32
			wantMask := quantizeBlockScalar(&want, &src, &recip, &half)
			gotMask := quantizeBlock(&got, &src, &recip, &half)

			if got != want {
				for i := range got {
					if got[i] != want[i] {
						t.Fatalf("qval %d coef %d = %d: got %d, want %d",
							qval, i, src[i], got[i], want[i])
					}
				}
			}

			if gotMask != wantMask {
				t.Fatalf("qval %d case %d mask: got %064b, want %064b", qval, n, gotMask, wantMask)
			}
		}
	}
}

// TestQuantizeBlockExhaustive sweeps every coefficient magnitude a block can hold.
func TestQuantizeBlockExhaustive(t *testing.T) {
	for _, qval := range []int{1, 5, 37, 255} {
		recip, half := quantTables(qval)

		for base := int32(-8192); base <= 8192; base += 61 {
			var src [64]int32
			for i := range src {
				src[i] = base + int32(i)
			}

			var want, got [64]int32
			wantMask := quantizeBlockScalar(&want, &src, &recip, &half)
			gotMask := quantizeBlock(&got, &src, &recip, &half)

			if got != want {
				t.Fatalf("qval %d base %d: got %v want %v", qval, base, got, want)
			}

			if gotMask != wantMask {
				t.Fatalf("qval %d base %d mask: got %064b, want %064b", qval, base, gotMask, wantMask)
			}
		}
	}
}

// TestQuantizeReciprocalFitsInt32 checks the table type cannot overflow.
func TestQuantizeReciprocalFitsInt32(t *testing.T) {
	for qval := 1; qval <= 255; qval++ {
		d := int64(qval) * 8
		r := (1<<quantShift + d - 1) / d

		if r > 1<<31-1 {
			t.Fatalf("qval %d: reciprocal %d overflows int32", qval, r)
		}
	}
}

func BenchmarkQuantizeBlock(b *testing.B) {
	recip, half := quantTables(16)

	var src, dst [64]int32
	for i := range src {
		src[i] = int32(i*137%16384) - 8192
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		quantizeBlock(&dst, &src, &recip, &half)
	}
}

func BenchmarkQuantizeBlockScalar(b *testing.B) {
	recip, half := quantTables(16)

	var src, dst [64]int32
	for i := range src {
		src[i] = int32(i*137%16384) - 8192
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		quantizeBlockScalar(&dst, &src, &recip, &half)
	}
}
