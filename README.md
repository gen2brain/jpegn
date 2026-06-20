## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

JPEG decoder with SIMD optimizations.


### Benchmark

Compared to the standard library:
```
BenchmarkDecodeBaseline420-8             	    2250	   1068625 ns/op	  427250 B/op	       9 allocs/op
BenchmarkDecodeBaseline420StdLib-8       	    1243	   1939177 ns/op	  407093 B/op	       5 allocs/op

BenchmarkDecodeProgressive420-8          	     880	   2789336 ns/op	 2001552 B/op	      16 allocs/op
BenchmarkDecodeProgressive420StdLib-8    	     663	   3652539 ns/op	 1980106 B/op	      17 allocs/op

BenchmarkDecodeConfig-8                  	 5247145	       463.5 ns/op	      53 B/op	       1 allocs/op
BenchmarkDecodeConfigStdLib-8            	 1618652	      1543 ns/op	   13616 B/op	       2 allocs/op

BenchmarkDecodeToRGBANearestNeighbor-8   	    1569	   1517478 ns/op	 2002146 B/op	      13 allocs/op
BenchmarkDecodeToRGBACatmullRom-8        	    1472	   1637226 ns/op	 2265237 B/op	      15 allocs/op
BenchmarkDecodeToRGBAStdLib-8            	     805	   2940831 ns/op	 1455734 B/op	       7 allocs/op
```

Difference with assembly optimizations (noasm vs asm):
```
benchmark                                  old ns/op     new ns/op     delta
BenchmarkDecodeBaseline420-8               2056599       1068625       -48.04%
BenchmarkDecodeProgressive420-8            3712737       2789336       -24.87%
BenchmarkDecodeConfig-8                    512.9         463.5         -9.63%
BenchmarkDecodeToRGBANearestNeighbor-8     3361461       1517478       -54.86%
BenchmarkDecodeToRGBACatmullRom-8          4546850       1637226       -63.99%
BenchmarkIdct-8                            166.4         40.54         -75.64%
BenchmarkUpsampleNearestNeighbor-8         348521        113296        -67.49%
BenchmarkUpsampleCatmullRom-8              2905568       338537        -88.35%
```

### Build tags

* `noasm` - do not use SIMD optimizations
