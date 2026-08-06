package jpegn

import (
	"math"
	"math/rand"
	"testing"
)

// refDCT is a direct evaluation of the DCT-II definition.
func refDCT(in *[64]int32, out *[64]float64) {
	for v := 0; v < 8; v++ {
		for u := 0; u < 8; u++ {
			cu, cv := 1.0, 1.0
			if u == 0 {
				cu = 1 / math.Sqrt2
			}

			if v == 0 {
				cv = 1 / math.Sqrt2
			}

			sum := 0.0
			for y := 0; y < 8; y++ {
				for x := 0; x < 8; x++ {
					sum += float64(in[y*8+x]) *
						math.Cos(float64(2*x+1)*float64(u)*math.Pi/16) *
						math.Cos(float64(2*y+1)*float64(v)*math.Pi/16)
				}
			}

			out[v*8+u] = 0.25 * cu * cv * sum
		}
	}
}

func TestFDCTAgainstReference(t *testing.T) {
	rng := rand.New(rand.NewSource(1))

	cases := []struct {
		name string
		gen  func(i int) int32
	}{
		{"flat", func(int) int32 { return 0 }},
		{"white", func(int) int32 { return 127 }},
		{"black", func(int) int32 { return -128 }},
		{"ramp", func(i int) int32 { return int32(i%8*32) - 128 }},
		{"checker", func(i int) int32 {
			if (i/8+i%8)&1 == 0 {
				return 127
			}

			return -128
		}},
		{"random", func(int) int32 { return int32(rng.Intn(256)) - 128 }},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var in, blk [64]int32
			for i := range in {
				in[i] = tc.gen(i)
			}

			blk = in
			fdct(&blk)

			var want [64]float64
			refDCT(&in, &want)

			for i := range blk {
				got := float64(blk[i])
				exp := want[i] * 8

				if math.Abs(got-exp) > 3 {
					t.Errorf("coefficient %d: got %v, want %v (tolerance 3)", i, got, exp)
				}
			}
		})
	}
}

// TestFDCTRange verifies the magnitude bounds the entropy coder relies on.
func TestFDCTRange(t *testing.T) {
	rng := rand.New(rand.NewSource(2))

	var maxDC, maxAC int32

	for n := 0; n < 20000; n++ {
		var blk [64]int32
		for i := range blk {
			switch n % 3 {
			case 0:
				blk[i] = int32(rng.Intn(256)) - 128
			case 1:
				if (i/8+i%8)&1 == 0 {
					blk[i] = 127
				} else {
					blk[i] = -128
				}
			default:
				if rng.Intn(2) == 0 {
					blk[i] = 127
				} else {
					blk[i] = -128
				}
			}
		}

		fdct(&blk)

		for i, v := range blk {
			if v < 0 {
				v = -v
			}

			if i == 0 {
				if v > maxDC {
					maxDC = v
				}
			} else if v > maxAC {
				maxAC = v
			}
		}
	}

	if maxDC > 8*1024 {
		t.Errorf("DC magnitude %d exceeds 8192", maxDC)
	}

	if maxAC > 8*1023 {
		t.Errorf("AC magnitude %d exceeds 8184", maxAC)
	}
}

func BenchmarkFDCT(b *testing.B) {
	var blk [64]int32
	for i := range blk {
		blk[i] = int32(i*3%256) - 128
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		src := blk
		fdct(&src)
	}
}
