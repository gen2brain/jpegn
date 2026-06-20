## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

JPEG decoder with SIMD optimizations.


### Benchmark

Compared to the standard library:
```
BenchmarkDecodeBaseline420-8             	    3124	    742291 ns/op	  426321 B/op	       6 allocs/op
BenchmarkDecodeBaseline420StdLib-8       	    1318	   1819781 ns/op	  407089 B/op	       5 allocs/op

BenchmarkDecodeProgressive420-8          	    1156	   2128195 ns/op	 1999883 B/op	      10 allocs/op
BenchmarkDecodeProgressive420StdLib-8    	     662	   3602531 ns/op	 1980106 B/op	      17 allocs/op

BenchmarkDecodeConfig-8                  	 5923922	       410.4 ns/op	      48 B/op	       1 allocs/op
BenchmarkDecodeConfigStdLib-8            	 1473702	      1474 ns/op	   13616 B/op	       2 allocs/op

BenchmarkDecodeToRGBANearestNeighbor-8   	    2053	   1157808 ns/op	 2000039 B/op	      10 allocs/op
BenchmarkDecodeToRGBACatmullRom-8        	    1915	   1263899 ns/op	 2262274 B/op	      13 allocs/op
BenchmarkDecodeToRGBAStdLib-8            	     849	   2839869 ns/op	 1455735 B/op	       7 allocs/op
```

Difference with assembly optimizations (noasm vs asm):
```
benchmark                                  old ns/op     new ns/op     delta
BenchmarkDecodeBaseline420-8               1584411       742291        -53.15%
BenchmarkDecodeProgressive420-8            2882890       2128195       -26.18%
BenchmarkDecodeConfig-8                    453.7         410.4         -9.54%
BenchmarkDecodeToRGBANearestNeighbor-8     2942280       1157808       -60.65%
BenchmarkDecodeToRGBACatmullRom-8          4016591       1263899       -68.53%
BenchmarkIdct-8                            144.1         36.68         -74.55%
BenchmarkUpsampleNearestNeighbor-8         359914        116759        -67.56%
BenchmarkUpsampleCatmullRom-8              2703968       319568        -88.18%
```

### Build tags

* `noasm` - do not use SIMD optimizations
