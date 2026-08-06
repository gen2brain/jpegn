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

// plane8 renders a block of level-shifted values as an 8x8 byte plane.
func plane8(in *[64]int32, stride int) []byte {
	p := make([]byte, stride*8)
	for y := 0; y < 8; y++ {
		for x := 0; x < 8; x++ {
			p[y*stride+x] = byte(in[y*8+x] + 128)
		}
	}

	return p
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

			fdct(&blk, plane8(&in, 8), 8)

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
		var src, blk [64]int32
		for i := range src {
			switch n % 3 {
			case 0:
				src[i] = int32(rng.Intn(256)) - 128
			case 1:
				if (i/8+i%8)&1 == 0 {
					src[i] = 127
				} else {
					src[i] = -128
				}
			default:
				if rng.Intn(2) == 0 {
					src[i] = 127
				} else {
					src[i] = -128
				}
			}
		}

		fdct(&blk, plane8(&src, 8), 8)

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

// TestFDCTMatchesScalar checks the assembly path is bit-identical to pure Go.
func TestFDCTMatchesScalar(t *testing.T) {
	rng := rand.New(rand.NewSource(3))

	gen := []func(i int) int32{
		func(int) int32 { return 0 },
		func(int) int32 { return 127 },
		func(int) int32 { return -128 },
		func(i int) int32 { return int32(i%8*32) - 128 },
		func(i int) int32 { return int32(i/8*32) - 128 },
		func(i int) int32 {
			if (i/8+i%8)&1 == 0 {
				return 127
			}

			return -128
		},
		func(int) int32 { return int32(rng.Intn(256)) - 128 },
	}

	for n := 0; n < 100000; n++ {
		var src [64]int32
		g := gen[n%len(gen)]

		for i := range src {
			src[i] = g(i)
		}

		var want, got [64]int32
		fdctScalar(&want, plane8(&src, 11), 11)
		fdct(&got, plane8(&src, 11), 11)

		if got != want {
			t.Fatalf("case %d mismatch\n src  %v\n got  %v\n want %v", n, src, got, want)
		}
	}
}

// TestFDCTDoesNotReadPastBlock checks the transform stays inside its buffers.
func TestFDCTDoesNotReadPastBlock(t *testing.T) {
	var guard [192]int32
	for i := range guard {
		guard[i] = 0x5A5A5A5A
	}

	var src [64]int32
	for i := range src {
		src[i] = int32(i*5%256) - 128
	}

	const stride = 13

	plane := plane8(&src, stride)
	tail := len(plane) - (7*stride + 8)

	for i := len(plane) - tail; i < len(plane); i++ {
		plane[i] = 0xC3
	}

	blk := (*[64]int32)(guard[64:128])
	fdct(blk, plane, stride)

	for i := 0; i < 64; i++ {
		if guard[i] != 0x5A5A5A5A {
			t.Fatalf("wrote before block at %d: %d", i, guard[i])
		}
	}

	for i := 128; i < 192; i++ {
		if guard[i] != 0x5A5A5A5A {
			t.Fatalf("wrote past block at %d: %d", i, guard[i])
		}
	}
}

func BenchmarkFDCT(b *testing.B) {
	var src, blk [64]int32
	for i := range src {
		src[i] = int32(i*3%256) - 128
	}

	p := plane8(&src, 8)

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		fdct(&blk, p, 8)
	}
}

func BenchmarkFDCTScalar(b *testing.B) {
	var src, blk [64]int32
	for i := range src {
		src[i] = int32(i*3%256) - 128
	}

	p := plane8(&src, 8)

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		fdctScalar(&blk, p, 8)
	}
}
