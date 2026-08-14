package jpegn

import (
	"math"
	"math/bits"
)

// Adaptive quantization, ported from jpegli's adaptive_quantization.cc. The
// field is one value per 8x8 luma block; it does not change the quantization
// table, only how far a coefficient must be from zero to be kept.

const (
	aqEpsilon  = 1e-2
	aqInvLog2e = 0.6931471805599453
	aqLog2e    = 1.442695041

	aqSGMul     = 226.0480446705883
	aqSGMul2    = 1.0 / 73.377132366608819
	aqSGVOffset = 7.14672470003
	aqSGRetMul  = aqSGMul2 * 18.6580932135 * aqInvLog2e

	aqRatioNumOffset = aqEpsilon * 255 * 255
	aqRatioNumMul    = aqSGRetMul * 3 * aqSGMul
	aqRatioVOffset   = (aqSGVOffset*aqInvLog2e + aqEpsilon) * 255
	aqRatioDenMul    = aqInvLog2e * aqSGMul / 255 / 255

	aqGammaOffset = 0.019 * 255
	aqDiffLimit   = 0.2

	aqHfCoeff    = -2.0052193233688884 / 255 / 112
	aqGammaBias  = 0.16 * 255
	aqGammaScale = 1.0 / 255 / 64
	aqGammaMul   = -0.15526878023684174 * aqInvLog2e

	aqQuant     = 0.841
	aqBaseLevel = 0.48 * aqQuant

	aqDampenStart = 9.0
	aqDampenRange = 65.0 - 9.0
)

// aqMaskMul is folded from jpegli's MaskingSqrt, which roots its multiplier.
var aqMaskMul = float32(math.Sqrt(211.50759899638012 * 1e8))

// fastLog2 is jpegli's FastLog2f, a 2,2 rational approximation of log1p.
func fastLog2(x float32) float32 {
	xb := int32(math.Float32bits(x))
	shifted := (xb - 0x3f2aaaab) >> 23
	m := math.Float32frombits(uint32(xb-shifted<<23)) - 1

	p := float32(0.74245873327820566*m) + 1.4287160470083755
	p = float32(p*m) - 1.8503833400518310e-06

	q := float32(0.17409343003366853*m) + 1.0096718572241148
	q = float32(q*m) + 0.99032814277590719

	return p/q + float32(shifted)
}

// fastPow2 is jpegli's FastPow2f.
func fastPow2(x float32) float32 {
	fl := float32(math.Floor(float64(x)))
	exp := math.Float32frombits(uint32(int32(fl)+127) << 23)
	f := x - fl

	num := f + 10.1749063
	num = float32(num*f) + 48.8687798
	num = float32(num*f) + 98.5506591

	den := float32(0.210242958*f) - 0.0222328856
	den = float32(den*f) - 19.4414990
	den = float32(den*f) + 98.5506633

	return num * exp / den
}

// gammaRatio is jpegli's RatioOfDerivativesOfCubicRootToSimpleGamma, and
// gammaRatioInv its inverted form.
func gammaRatio(v float32) float32 {
	num, den := gammaTerms(v)

	return den / num
}

func gammaRatioInv(v float32) float32 {
	num, den := gammaTerms(v)

	return num / den
}

func gammaTerms(v float32) (float32, float32) {
	if v < 0 {
		v = 0
	}

	v2 := v * v

	return float32(aqRatioNumMul*v2) + aqRatioNumOffset,
		float32(aqRatioDenMul*v*v2) + aqRatioVOffset
}

// Both callers evaluate the ratio at a fixed offset from a sample, so the whole
// function collapses to one entry per possible sample value. The difference
// table carries the quarter of the neighbor average with it, which lets the
// average itself stay in integers.
var (
	gammaDiffLUT [256]float32
	gammaModLUT  [256]float32
)

func init() {
	for v := 0; v < 256; v++ {
		gammaDiffLUT[v] = gammaRatio(float32(v)+aqGammaOffset) * 0.25
		gammaModLUT[v] = gammaRatioInv(float32(v) + aqGammaBias)
	}
}

// aqMaskingSqrt is jpegli's MaskingSqrt.
func aqMaskingSqrt(v float32) float32 {
	return float32(0.25 * float32(math.Sqrt(float64(float32(v*aqMaskMul)+28))))
}

// aqComputeMask modulates the exponent by the local masking strength.
func aqComputeMask(v float32) float32 {
	v1 := v * 0.74760422233706747
	if v1 < 1e-3 {
		v1 = 1e-3
	}

	v2 := 1 / (v1 + 305.04035728311436)
	v3 := 1 / (float32(v1*v1) + 2.1925739705298404)
	v4 := 1 / (float32(v1*v1) + 0.25*2.1925739705298404)

	return -0.74174993 + float32(3.2353257320940401*v4) +
		float32(12.906028311180409*v2) + float32(5.0220313103171232*v3)
}

// aqField holds the per-block quantization field and its scratch buffers.
type aqField struct {
	field  []float32
	pre    []uint32
	tmp    []float32
	diff   []float32
	w, h   int
	preW   int
	preH   int
	stride int
}

// preRow returns one pre-erosion row, clamped vertically. Element zero is the
// x = -1 border, so x maps to index x+1.
func (a *aqField) preRow(y int) []uint32 {
	y = min(max(y, 0), a.preH-1)

	return a.pre[y*a.stride : (y+1)*a.stride]
}

// buildQuantField computes the adaptive quantization field from the luma plane.
func (e *encoder) buildQuantField() {
	c := &e.comp[0]
	a := &e.aq

	a.w, a.h = e.mcusX*e.hmax, e.mcusY*e.vmax
	a.preW, a.preH = a.w*2, a.h*2
	a.stride = a.preW + 2

	a.field = growFloat(a.field, a.w*a.h)

	if need := a.stride * a.preH; cap(a.pre) < need {
		a.pre = make([]uint32, need)
	} else {
		a.pre = a.pre[:need]
	}

	a.tmp = growFloat(a.tmp, a.preW*a.preH)
	a.diff = growFloat(a.diff, a.w*8)

	a.computePreErosion(c)
	a.fuzzyErosion()
	a.perBlockModulations(c, e.qtab[0][1])

	for i, v := range a.field {
		if v = 0.6/v - 1; v < 0 {
			v = 0
		}

		a.field[i] = v
	}
}

// growFloat returns a slice of n floats, reusing s when it is big enough.
func growFloat(s []float32, n int) []float32 {
	if cap(s) < n {
		return make([]float32, n)
	}

	return s[:n]
}

// computePreErosion measures local pixel differences, subsampled 4x both ways.
func (a *aqField) computePreErosion(c *encComponent) {
	xsize := a.w * 8
	ysize := a.h * 8

	for y := 0; y < ysize; y++ {
		row := c.plane[y*c.stride : y*c.stride+xsize]
		rowT := c.plane[max(y-1, 0)*c.stride:][:xsize]
		rowB := c.plane[min(y+1, ysize-1)*c.stride:][:xsize]

		preErosionRow(a.diff[:xsize], row, rowT, rowB, y&3 != 0)

		if y&3 != 3 {
			continue
		}

		out := a.preRow(y / 4)
		for x := 0; x < a.preW; x++ {
			v := (a.diff[x*4] + a.diff[x*4+1] + a.diff[x*4+2] + a.diff[x*4+3]) * 0.25
			out[x+1] = math.Float32bits(v)
		}

		out[0] = out[1]
		out[a.preW+1] = out[a.preW]
	}
}

// fuzzyErosion keeps a weighted combination of the four smallest values of each
// 3x3 neighborhood, so one busy spot does not raise its quiet neighbors.
func (a *aqField) fuzzyErosion() {
	for y := 0; y < a.preH; y++ {
		rowT := a.preRow(y - 1)
		rowM := a.preRow(y)
		rowB := a.preRow(y + 1)
		out := a.tmp[y*a.preW:]

		for x := 0; x < a.preW; x++ {
			l, m, r := x, x+1, x+2

			min0, min1, min2, min3 := sort4(rowM[m], rowM[l], rowM[r], rowT[l])
			min0, min1, min2, min3 = updateMin4(rowT[m], min0, min1, min2, min3)
			min0, min1, min2, min3 = updateMin4(rowT[r], min0, min1, min2, min3)
			min0, min1, min2, min3 = updateMin4(rowB[l], min0, min1, min2, min3)
			min0, min1, min2, min3 = updateMin4(rowB[m], min0, min1, min2, min3)
			min0, min1, min2, min3 = updateMin4(rowB[r], min0, min1, min2, min3)

			out[x] = float32(0.125*math.Float32frombits(min0)) +
				float32(0.075*math.Float32frombits(min1)) +
				float32(0.06*math.Float32frombits(min2)) +
				float32(0.05*math.Float32frombits(min3))
		}

		if y&1 != 1 {
			continue
		}

		prev := a.tmp[(y-1)*a.preW:]
		dst := a.field[y/2*a.w:]

		for bx, x := 0, 0; bx < a.w; bx, x = bx+1, x+2 {
			dst[bx] = out[x] + out[x+1] + prev[x] + prev[x+1]
		}
	}
}

// The erosion network runs on bit patterns. Every value it sees is a positive
// finite float, and for those the unsigned ordering is the float ordering, so
// integer min and max give the same answer without the NaN and signed zero
// handling the float builtins carry.

// sort4 orders four values ascending.
func sort4(a, b, c, d uint32) (uint32, uint32, uint32, uint32) {
	t0, t1 := min(a, b), max(a, b)
	t2, t3 := min(c, d), max(c, d)
	t4, t5 := max(t0, t2), min(t1, t3)

	return min(t0, t2), min(t4, t5), max(t4, t5), max(t1, t3)
}

// updateMin4 folds v into an ascending list of the four smallest values.
func updateMin4(v, m0, m1, m2, m3 uint32) (uint32, uint32, uint32, uint32) {
	t0 := max(m0, v)
	t1 := max(m1, t0)
	t2 := max(m2, t1)

	return min(m0, v), min(m1, t0), min(m2, t1), min(m3, t2)
}

// perBlockModulations folds the masking, high frequency and gamma terms of each
// block into the exponent, then converts it to a multiplier.
func (a *aqField) perBlockModulations(c *encComponent, yQuant01 uint16) {
	mul, add := aqDampen(yQuant01)

	for by := 0; by < a.h; by++ {
		for bx := 0; bx < a.w; bx++ {
			block := c.plane[by*8*c.stride+bx*8:]

			hf, gamma := blockModulations(block, c.stride)
			v := aqComputeMask(a.field[by*a.w+bx]) + hf + gamma

			a.field[by*a.w+bx] = float32(fastPow2(v*aqLog2e)*mul) + add
		}
	}
}

// aqDampen ramps the field towards a flat base level as the luma table coarsens.
func aqDampen(yQuant01 uint16) (float32, float32) {
	dampen := float32(1)

	if q := float32(yQuant01); q >= aqDampenStart {
		if dampen = 1 - (q-aqDampenStart)/aqDampenRange; dampen < 0 {
			dampen = 0
		}
	}

	return float32(aqQuant * dampen), float32((1 - dampen) * aqBaseLevel)
}

// blockModulations returns the high frequency and gamma terms of one block,
// which walk the same 64 samples.
func blockModulations(block []byte, stride int) (float32, float32) {
	sum := 0
	ratio := float32(0)

	for dy := 0; dy < 8; dy++ {
		row := block[dy*stride : dy*stride+8]
		next := row

		if dy != 7 {
			next = block[(dy+1)*stride : (dy+1)*stride+8]
		}

		p := int(row[0])
		ratio += gammaModLUT[row[0]]
		sum += absInt(p - int(next[0]))

		for dx := 1; dx < 8; dx++ {
			q := int(row[dx])
			ratio += gammaModLUT[row[dx]]
			sum += absInt(p-q) + absInt(q-int(next[dx]))
			p = q
		}
	}

	return float32(float32(sum) * aqHfCoeff),
		float32(aqGammaMul * fastLog2(ratio*aqGammaScale))
}

// absInt is branchless because it is applied to differences of random sign.
func absInt(v int) int {
	m := v >> (bits.UintSize - 1)

	return (v ^ m) - m
}

// aqStrength returns the field value covering block (bx, by) of component ci.
func (e *encoder) aqStrength(ci, bx, by int) float32 {
	c := &e.comp[ci]

	return e.aq.field[by*c.vf*e.aq.w+bx*c.hf]
}

// applyDeadZone drops the AC coefficients that fall inside the block's dead
// zone. Kept values are untouched, so the SIMD kernel still does the
// arithmetic and only the surviving positions are visited here.
func (e *encoder) applyDeadZone(dst, src *[64]int32, nz uint64, ci int, aq float32) uint64 {
	c := &e.comp[ci]

	qmul := &e.qmulF[c.qtSel]
	off := &e.zeroBiasOff[ci]
	mul := &e.zeroBiasMul[ci]

	for m := nz &^ 1; m != 0; m &= m - 1 {
		k := bits.TrailingZeros64(m)

		s := src[k]
		m := s >> 31

		if float32((s^m)-m)*qmul[k] < off[k]+float32(mul[k]*aq) {
			dst[k] = 0
			nz &^= 1 << uint(k)
		}
	}

	return nz
}

// qualityToDistance is jpegli's mapping from a libjpeg quality to its own
// distance scale, used only to mix the two zero bias tables.
func qualityToDistance(quality int) float32 {
	q := float32(quality)

	switch {
	case quality >= 100:
		return 0.01
	case quality >= 30:
		return 0.1 + float32((100-q)*0.09)
	default:
		return float32(53.0/3000.0*q*q) - float32(23.0/20.0*q) + 25.0
	}
}

// buildZeroBias fills the dead zone tables for the quality in use.
func (e *encoder) buildZeroBias(quality int) {
	for t := 0; t < e.nqtab; t++ {
		for k := 0; k < 64; k++ {
			e.qmulF[t][k] = 1 / float32(e.qhalf[t][k]*2)
		}
	}

	for ci := 0; ci < e.ncomp; ci++ {
		for k := 0; k < 64; k++ {
			e.zeroBiasMul[ci][k] = 0.5
			e.zeroBiasOff[ci][k] = 0.5
		}

		e.zeroBiasMul[ci][0] = 0
		e.zeroBiasOff[ci][0] = 0
	}

	if e.ncomp != 3 {
		return
	}

	mix0 := min(max((qualityToDistance(quality)-1)/2, 0), 1)
	mix1 := 1 - mix0

	for ci := 0; ci < 3; ci++ {
		for k := 0; k < 64; k++ {
			e.zeroBiasMul[ci][k] = float32(mix0*zeroBiasMulLQ[ci*64+k]) +
				float32(mix1*zeroBiasMulHQ[ci*64+k])
			e.zeroBiasOff[ci][k] = zeroBiasOffAC[ci]
		}

		e.zeroBiasOff[ci][0] = 0
	}
}
