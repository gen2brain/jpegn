## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

JPEG decoder with SIMD optimizations.


### Benchmark

Compared to the standard library:
```
BenchmarkDecodeBaseline420-8             	    3546	   1011092 ns/op	  426775 B/op	       9 allocs/op
BenchmarkDecodeBaseline420StdLib-8       	    1942	   1874464 ns/op	  407088 B/op	       5 allocs/op

BenchmarkDecodeProgressive420-8          	    2221	   2741827 ns/op	 2004426 B/op	      17 allocs/op
BenchmarkDecodeProgressive420StdLib-8    	     950	   3645997 ns/op	 1980096 B/op	      17 allocs/op

BenchmarkDecodeConfig-8                  	11457307	       322.3 ns/op	      52 B/op	       1 allocs/op
BenchmarkDecodeConfigStdLib-8            	 2974725	      1199 ns/op	   13616 B/op	       2 allocs/op

BenchmarkDecodeToRGBANearestNeighbor-8   	    2683	   1405335 ns/op	 2002868 B/op	      13 allocs/op
BenchmarkDecodeToRGBACatmullRom-8        	    2307	   1568539 ns/op	 2271535 B/op	      15 allocs/op
BenchmarkDecodeToRGBAStdLib-8            	    1304	   2837424 ns/op	 1455730 B/op	       7 allocs/op
```

Difference with assembly optimizations (noasm vs asm):
```
benchmark                                  old ns/op     new ns/op     delta
BenchmarkDecodeBaseline420-8               1984046       1011092       -49.04%
BenchmarkDecodeProgressive420-8            3645160       2741827       -24.79%
BenchmarkDecodeConfig-8                    344           322           -6.44%
BenchmarkDecodeToRGBANearestNeighbor-8     3368435       1405335       -58.28%
BenchmarkDecodeToRGBACatmullRom-8          5253731       1568539       -70.14%
BenchmarkIdct-8                            147           37.7          -74.30%
BenchmarkUpsampleNearestNeighbor-8         349901        126524        -63.84%
BenchmarkUpsampleCatmullRom-8              7418931       378112        -94.90%
```

### Build tags

* `noasm` - do not use SIMD optimizations
