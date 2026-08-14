package jpegn

import (
	"bytes"
	"image"
	"image/color"
	"image/jpeg"
	"math"
	"math/rand"
	"sort"
	"testing"
)

// TestFastLog2 checks the rational approximation against the real logarithm.
// jpegli documents an L1 error near 3.9e-6.
func TestFastLog2(t *testing.T) {
	worst := 0.0

	for x := 1e-4; x < 1e6; x *= 1.0009 {
		if e := math.Abs(float64(fastLog2(float32(x))) - math.Log2(x)); e > worst {
			worst = e
		}
	}

	t.Logf("max absolute error %.3e", worst)

	if worst > 1e-5 {
		t.Errorf("max absolute error %.3e too large", worst)
	}
}

// TestFastPow2 checks the approximation against the real exponent.
func TestFastPow2(t *testing.T) {
	worst := 0.0

	for x := -30.0; x < 30.0; x += 0.0007 {
		want := math.Exp2(x)
		if e := math.Abs(float64(fastPow2(float32(x)))-want) / want; e > worst {
			worst = e
		}
	}

	t.Logf("max relative error %.3e", worst)

	if worst > 1e-5 {
		t.Errorf("max relative error %.3e too large", worst)
	}
}

// TestGammaRatio checks the two returns are reciprocal, and that the ratio
// falls steeply over the dark half of the range before turning back up, which
// is the shape that makes dark areas quantize more finely.
func TestGammaRatio(t *testing.T) {
	for v := float32(0); v <= 300; v += 0.5 {
		r, inv := gammaRatio(v), gammaRatioInv(v)

		if d := math.Abs(float64(r*inv) - 1); d > 1e-5 {
			t.Fatalf("v %.1f: ratio %g and inverse %g are not reciprocal", v, r, inv)
		}

		if r <= 0 || math.IsNaN(float64(r)) {
			t.Fatalf("v %.1f: ratio %g", v, r)
		}
	}

	r0 := gammaRatio(0)
	r50 := gammaRatio(50)
	r100 := gammaRatio(100)
	r200 := gammaRatio(200)
	r300 := gammaRatio(300)

	if !(r0 > r50 && r50 > r100 && r100 < r200 && r200 < r300) {
		t.Errorf("ratio %g %g %g %g %g is not a fall then a rise", r0, r50, r100, r200, r300)
	}

	if r0/r50 < 100 {
		t.Errorf("ratio only falls %.1fx over the dark half", r0/r50)
	}
}

// aqTestImage builds a left half of flat color and a right half of noise.
func aqTestImage(w, h int) *image.Gray {
	m := image.NewGray(image.Rect(0, 0, w, h))
	rng := rand.New(rand.NewSource(9))

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			v := uint8(128)
			if x >= w/2 {
				v = uint8(rng.Intn(256))
			}

			m.SetGray(x, y, color.Gray{Y: v})
		}
	}

	return m
}

// TestQuantFieldStructure checks the field is finite, non-negative, and much
// stronger over noise than over flat area. That direction is the whole point:
// detail masks quantization error, so busy blocks get the wider dead zone and
// flat ones keep their precision.
func TestQuantFieldStructure(t *testing.T) {
	e := &encoder{}
	if err := e.encode(aqTestImage(256, 64), 75, SubsampleGray, true, false, true, 0); err != nil {
		t.Fatalf("encode: %v", err)
	}

	a := &e.aq
	if a.w != 32 || a.h != 8 {
		t.Fatalf("field is %dx%d, want 32x8", a.w, a.h)
	}

	var flat, noisy float64

	for by := 0; by < a.h; by++ {
		for bx := 0; bx < a.w; bx++ {
			v := a.field[by*a.w+bx]

			if math.IsNaN(float64(v)) || math.IsInf(float64(v), 0) || v < 0 {
				t.Fatalf("field[%d,%d] = %g", bx, by, v)
			}

			if bx < a.w/2-1 {
				flat += float64(v)
			} else if bx > a.w/2 {
				noisy += float64(v)
			}
		}
	}

	n := float64(a.h * (a.w/2 - 1))
	t.Logf("mean field: flat %.3f, noisy %.3f", flat/n, noisy/n)

	if noisy <= 4*flat {
		t.Errorf("noisy field %.3f not well above flat %.3f", noisy/n, flat/n)
	}
}

// TestDeadZoneOnlyDrops checks the dead zone never changes a kept coefficient,
// never touches DC, and keeps the non-zero mask in step with the block.
func TestDeadZoneOnlyDrops(t *testing.T) {
	e := &encoder{}
	e.setSampling(Subsample420)
	e.buildQuant(75)
	e.buildZeroBias(75)

	rng := rand.New(rand.NewSource(3))

	for i := 0; i < 2000; i++ {
		var src, plain, dead [64]int32

		for k := range src {
			src[k] = int32(rng.Intn(16384) - 8192)
		}

		ci := i % 3
		aq := float32(rng.Float64() * 4)

		want := quantizeBlock(&plain, &src, &e.qrecip[e.comp[ci].qtSel], &e.qhalf[e.comp[ci].qtSel])
		got := quantizeBlock(&dead, &src, &e.qrecip[e.comp[ci].qtSel], &e.qhalf[e.comp[ci].qtSel])
		got = e.applyDeadZone(&dead, &src, got, ci, aq)

		if got&^want != 0 {
			t.Fatalf("block %d: dead zone added coefficients", i)
		}

		if dead[0] != plain[0] {
			t.Fatalf("block %d: DC changed from %d to %d", i, plain[0], dead[0])
		}

		for k := 0; k < 64; k++ {
			if dead[k] != 0 && dead[k] != plain[k] {
				t.Fatalf("block %d: coefficient %d changed from %d to %d", i, k, plain[k], dead[k])
			}

			if (got>>uint(k))&1 != b2u(dead[k] != 0) {
				t.Fatalf("block %d: mask disagrees with coefficient %d", i, k)
			}
		}
	}
}

func b2u(b bool) uint64 {
	if b {
		return 1
	}

	return 0
}

// TestDeadZoneNeutralAtZero checks a zero threshold reproduces the plain
// quantizer exactly, so the float path cannot drift from the integer one.
func TestDeadZoneNeutralAtZero(t *testing.T) {
	e := &encoder{}
	e.setSampling(Subsample444)
	e.buildQuant(90)
	e.buildZeroBias(90)

	for ci := 0; ci < 3; ci++ {
		e.zeroBiasMul[ci] = [64]float32{}
		e.zeroBiasOff[ci] = [64]float32{}
	}

	rng := rand.New(rand.NewSource(5))

	for i := 0; i < 2000; i++ {
		var src, plain, dead [64]int32

		for k := range src {
			src[k] = int32(rng.Intn(16384) - 8192)
		}

		ci := i % 3
		want := quantizeBlock(&plain, &src, &e.qrecip[e.comp[ci].qtSel], &e.qhalf[e.comp[ci].qtSel])
		got := quantizeBlock(&dead, &src, &e.qrecip[e.comp[ci].qtSel], &e.qhalf[e.comp[ci].qtSel])
		got = e.applyDeadZone(&dead, &src, got, ci, 7)

		if got != want || dead != plain {
			t.Fatalf("block %d: a zero threshold changed the block", i)
		}
	}
}

// TestEncodeAdaptiveRoundTrip checks every mode still produces a readable file.
func TestEncodeAdaptiveRoundTrip(t *testing.T) {
	subs := []Subsampling{Subsample444, Subsample422, Subsample440, Subsample420, SubsampleGray}

	for _, sz := range []image.Point{{1, 1}, {8, 8}, {17, 9}, {129, 71}} {
		src := synthImage(sz.X, sz.Y)

		for _, sub := range subs {
			for _, prog := range []bool{false, true} {
				data := encodeToBytes(t, src, &EncodeOptions{
					Quality: 80, Subsampling: sub, Progressive: prog,
					OptimizeCoding: !prog, AdaptiveQuantization: true,
				})

				if _, err := jpeg.Decode(bytes.NewReader(data)); err != nil {
					t.Fatalf("%v sub %d prog %v: stdlib decode: %v", sz, sub, prog, err)
				}

				if _, err := Decode(bytes.NewReader(data)); err != nil {
					t.Fatalf("%v sub %d prog %v: Decode: %v", sz, sub, prog, err)
				}
			}
		}
	}
}

// TestEncodeAdaptiveSize reports what the dead zone is worth and checks it pays
// on both sources.
func TestEncodeAdaptiveSize(t *testing.T) {
	srcs := []struct {
		name string
		m    image.Image
	}{
		{"graphic", synthImage(769, 512)},
		{"photo", photoRGBA(t)},
	}

	for _, src := range srcs {
		for _, q := range []int{50, 75, 90} {
			plain := len(encodeToBytes(t, src.m, &EncodeOptions{
				Quality: q, Subsampling: Subsample420, OptimizeCoding: true,
			}))
			adaptive := len(encodeToBytes(t, src.m, &EncodeOptions{
				Quality: q, Subsampling: Subsample420, OptimizeCoding: true,
				AdaptiveQuantization: true,
			}))

			delta := 100 * float64(adaptive-plain) / float64(plain)
			t.Logf("%-8s q%-3d plain %8d adaptive %8d %+.1f%%", src.name, q, plain, adaptive, delta)

			if delta > -1 {
				t.Errorf("%s q%d: adaptive quantization only %+.1f%%", src.name, q, delta)
			}
		}
	}
}

// TestSort4 checks the sorting network against a plain sort.
func TestSort4(t *testing.T) {
	rng := rand.New(rand.NewSource(21))

	for i := 0; i < 5000; i++ {
		v := [4]uint32{}
		for k := range v {
			v[k] = math.Float32bits(float32(rng.Intn(20)) + 1)
		}

		a, b, c, d := sort4(v[0], v[1], v[2], v[3])

		want := append([]uint32(nil), v[:]...)
		sort.Slice(want, func(i, j int) bool { return want[i] < want[j] })

		if a != want[0] || b != want[1] || c != want[2] || d != want[3] {
			t.Fatalf("sort4%v = %v %v %v %v, want %v", v, a, b, c, d, want)
		}
	}
}

// TestUpdateMin4 checks the running four smallest against a plain sort.
func TestUpdateMin4(t *testing.T) {
	rng := rand.New(rand.NewSource(22))

	for i := 0; i < 2000; i++ {
		all := make([]uint32, 4+rng.Intn(8))
		for k := range all {
			all[k] = math.Float32bits(float32(rng.Intn(30)) + 1)
		}

		m0, m1, m2, m3 := sort4(all[0], all[1], all[2], all[3])
		for _, v := range all[4:] {
			m0, m1, m2, m3 = updateMin4(v, m0, m1, m2, m3)
		}

		want := append([]uint32(nil), all...)
		sort.Slice(want, func(i, j int) bool { return want[i] < want[j] })

		if m0 != want[0] || m1 != want[1] || m2 != want[2] || m3 != want[3] {
			t.Fatalf("%v: got %v %v %v %v, want %v", all, m0, m1, m2, m3, want[:4])
		}
	}
}

// TestBlockModulations checks both per-block terms against a direct reading of
// jpegli's loops, which the encoder walks fused into one pass.
func TestBlockModulations(t *testing.T) {
	rng := rand.New(rand.NewSource(23))
	block := make([]byte, 8*16)

	for i := range block {
		block[i] = byte(rng.Intn(256))
	}

	wantHf, wantRatio := 0, float32(0)

	for dy := 0; dy < 8; dy++ {
		for dx := 0; dx < 8; dx++ {
			p := int(block[dy*16+dx])

			if dx != 7 {
				wantHf += absInt(p - int(block[dy*16+dx+1]))
			}

			if dy != 7 {
				wantHf += absInt(p - int(block[(dy+1)*16+dx]))
			}
		}
	}

	for dy := 0; dy < 8; dy++ {
		for dx := 0; dx < 8; dx++ {
			wantRatio += gammaRatioInv(float32(block[dy*16+dx]) + aqGammaBias)
		}
	}

	hf, gamma := blockModulations(block, 16)

	if want := float32(wantHf) * aqHfCoeff; hf != want {
		t.Errorf("high frequency term %g, want %g", hf, want)
	}

	if want := aqGammaMul * fastLog2(wantRatio*aqGammaScale); gamma != want {
		t.Errorf("gamma term %g, want %g", gamma, want)
	}

	if hf >= 0 {
		t.Errorf("high frequency term %g should lower the exponent", hf)
	}

	flat := make([]byte, 8*16)
	for i := range flat {
		flat[i] = 200
	}

	if hf, _ := blockModulations(flat, 16); hf != 0 {
		t.Errorf("flat block high frequency term %g, want 0", hf)
	}
}

// TestPreErosionClamped checks the difference limit actually binds: the worst
// possible edge must land on the clamped value and never above it.
func TestPreErosionClamped(t *testing.T) {
	m := image.NewGray(image.Rect(0, 0, 64, 64))
	for y := 0; y < 64; y++ {
		for x := 0; x < 64; x++ {
			if (x+y)&1 == 0 {
				m.SetGray(x, y, color.Gray{Y: 255})
			}
		}
	}

	e := &encoder{}
	if err := e.encode(m, 75, SubsampleGray, true, false, true, 0); err != nil {
		t.Fatalf("encode: %v", err)
	}

	limit := 4 * aqMaskingSqrt(aqDiffLimit)
	worst := float32(0)

	for y := 0; y < e.aq.preH; y++ {
		row := e.aq.preRow(y)
		for x := 0; x < e.aq.preW; x++ {
			if v := math.Float32frombits(row[x+1]); v > worst {
				worst = v
			}
		}
	}

	t.Logf("worst pre-erosion value %.3f, limit %.3f", worst, limit)

	if worst > limit {
		t.Errorf("pre-erosion %.3f above the clamped limit %.3f", worst, limit)
	}

	if worst < limit*0.999 {
		t.Errorf("pre-erosion %.3f never reached the limit %.3f", worst, limit)
	}
}

// TestZeroBiasMix checks the two tables are selected by quality, low quality
// resolving to jpegli's low quality table and high quality to the other.
func TestZeroBiasMix(t *testing.T) {
	e := &encoder{}
	e.setSampling(Subsample420)
	e.buildQuant(20)
	e.buildZeroBias(20)

	for ci := 0; ci < 3; ci++ {
		for k := 1; k < 64; k++ {
			if e.zeroBiasMul[ci][k] != zeroBiasMulLQ[ci*64+k] {
				t.Fatalf("q20 component %d entry %d is %g, want the low quality table %g",
					ci, k, e.zeroBiasMul[ci][k], zeroBiasMulLQ[ci*64+k])
			}
		}
	}

	e.buildQuant(100)
	e.buildZeroBias(100)

	for ci := 0; ci < 3; ci++ {
		for k := 1; k < 64; k++ {
			if e.zeroBiasMul[ci][k] != zeroBiasMulHQ[ci*64+k] {
				t.Fatalf("q100 component %d entry %d is %g, want the high quality table %g",
					ci, k, e.zeroBiasMul[ci][k], zeroBiasMulHQ[ci*64+k])
			}
		}
	}
}

// TestAdaptiveConstants pins the constants derived from jpegli's source, so a
// mis-transcribed one shows up here rather than as a changed output hash.
func TestAdaptiveConstants(t *testing.T) {
	cases := []struct {
		name string
		got  float64
		want float64
	}{
		{"aqHfCoeff", aqHfCoeff, -7.0210760622159963e-05},
		{"aqGammaMul", aqGammaMul, -0.1076241172501486},
		{"aqGammaBias", aqGammaBias, 40.800000000000004},
		{"aqGammaScale", aqGammaScale, 6.1274509803921568e-05},
		{"aqGammaOffset", aqGammaOffset, 4.8449999999999998},
		{"aqRatioNumOffset", aqRatioNumOffset, 650.25000000000011},
		{"aqRatioNumMul", aqRatioNumMul, 119.52369034508325},
		{"aqRatioVOffset", aqRatioVOffset, 1265.7516793962984},
		{"aqRatioDenMul", aqRatioDenMul, 0.0024096049955325922},
		{"aqBaseLevel", aqBaseLevel, 0.40367999999999998},
		{"aqDiffLimit", aqDiffLimit, 0.2},
		{"aqQuant", aqQuant, 0.841},
		{"aqMaskMul", float64(aqMaskMul), 145433.015625},
	}

	// The tolerance admits the rounding path only; any mis-transcription moves
	// these by far more.
	for _, tc := range cases {
		if math.Abs(tc.got-tc.want) > math.Abs(tc.want)*1e-12 {
			t.Errorf("%s = %.17g, want %.17g", tc.name, tc.got, tc.want)
		}
	}

	for q, want := range map[int]float32{100: 0.01, 90: 1.0, 75: 2.35, 30: 6.4, 20: 9.0666666} {
		if got := qualityToDistance(q); math.Abs(float64(got-want)) > 1e-5 {
			t.Errorf("qualityToDistance(%d) = %g, want %g", q, got, want)
		}
	}

	if got := aqMaskingSqrt(aqDiffLimit); math.Abs(float64(got)-42.6575046) > 1e-4 {
		t.Errorf("aqMaskingSqrt(limit) = %g, want 42.6575046", got)
	}

	if got := aqComputeMask(0); math.Abs(float64(got)-7.4933538) > 1e-4 {
		t.Errorf("aqComputeMask(0) = %g, want 7.4933538", got)
	}
}

// TestAQDampen checks the ramp that flattens the field as the luma table
// coarsens, which only engages at low quality.
func TestAQDampen(t *testing.T) {
	cases := []struct {
		q        uint16
		mul, add float32
	}{
		{1, aqQuant, 0},
		{6, aqQuant, 0},
		{9, aqQuant, 0},
		{37, aqQuant * 0.5, 0.5 * aqBaseLevel},
		{65, 0, aqBaseLevel},
		{200, 0, aqBaseLevel},
	}

	for _, tc := range cases {
		mul, add := aqDampen(tc.q)

		if math.Abs(float64(mul-tc.mul)) > 1e-6 || math.Abs(float64(add-tc.add)) > 1e-6 {
			t.Errorf("aqDampen(%d) = %g, %g, want %g, %g", tc.q, mul, add, tc.mul, tc.add)
		}
	}
}

// TestFuzzyErosion checks the eroded value against a direct reading of the
// stage: the four smallest of the 3x3 neighborhood, in weight order.
func TestFuzzyErosion(t *testing.T) {
	a := &aqField{w: 3, h: 2, preW: 6, preH: 4}
	a.stride = a.preW + 2
	a.pre = make([]uint32, a.stride*a.preH)
	a.tmp = make([]float32, a.preW*a.preH)
	a.field = make([]float32, a.w*a.h)

	rng := rand.New(rand.NewSource(31))
	for y := 0; y < a.preH; y++ {
		row := a.preRow(y)
		for x := 0; x < a.preW; x++ {
			row[x+1] = math.Float32bits(float32(rng.Intn(100)) + 1)
		}

		row[0] = row[1]
		row[a.preW+1] = row[a.preW]
	}

	at := func(x, y int) float32 {
		return math.Float32frombits(a.preRow(y)[min(max(x, -1), a.preW)+1])
	}

	a.fuzzyErosion()

	for y := 0; y < a.preH; y++ {
		for x := 0; x < a.preW; x++ {
			var n []float32
			for dy := -1; dy <= 1; dy++ {
				for dx := -1; dx <= 1; dx++ {
					n = append(n, at(x+dx, y+dy))
				}
			}

			sort.Slice(n, func(i, j int) bool { return n[i] < n[j] })

			want := 0.125*n[0] + 0.075*n[1] + 0.06*n[2] + 0.05*n[3]
			if got := a.tmp[y*a.preW+x]; math.Abs(float64(got-want)) > 1e-5 {
				t.Fatalf("(%d,%d): eroded %g, want %g", x, y, got, want)
			}
		}
	}

	for by := 0; by < a.h; by++ {
		for bx := 0; bx < a.w; bx++ {
			want := a.tmp[2*by*a.preW+2*bx] + a.tmp[2*by*a.preW+2*bx+1] +
				a.tmp[(2*by+1)*a.preW+2*bx] + a.tmp[(2*by+1)*a.preW+2*bx+1]

			if got := a.field[by*a.w+bx]; math.Abs(float64(got-want)) > 1e-5 {
				t.Fatalf("block (%d,%d): field %g, want %g", bx, by, got, want)
			}
		}
	}
}
