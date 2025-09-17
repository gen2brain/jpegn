## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

JPEG decoder with SIMD optimizations. It is based on [NanoJPEG](https://keyj.emphy.de/nanojpeg/).

For unsupported formats, such as CMYK JPEGs, it gracefully falls back to the standard `image/jpeg`.

### Benchmark

Compared to the standard library:
```
BenchmarkDecode420-8                     	    1062	   1222948 ns/op	  426839 B/op	       6 allocs/op
BenchmarkDecode420StdLib-8               	     542	   2156193 ns/op	  407088 B/op	       5 allocs/op

BenchmarkDecodeConfig-8                  	 1442437	       859.4 ns/op	      53 B/op	       1 allocs/op
BenchmarkDecodeConfigStdLib-8            	  758470	      1369 ns/op	   13616 B/op	       2 allocs/op

BenchmarkDecodeToRGBANearestNeighbor-8   	     517	   2353340 ns/op	 1999821 B/op	      10 allocs/op
BenchmarkDecodeToRGBACatmullRom-8        	     546	   2180485 ns/op	 2263200 B/op	      13 allocs/op
BenchmarkDecodeToRGBAStdLib-8            	     414	   2903516 ns/op	 1455734 B/op	       7 allocs/op
```

Difference with assembly optimizations:
```
benchmark                                  old ns/op     new ns/op     delta
BenchmarkDecode420-8                       1621872       1068240       -34.14%
BenchmarkDecodeConfig-8                    824           754           -8.46%
BenchmarkDecodeToRGBANearestNeighbor-8     2726279       2071692       -24.01%
BenchmarkDecodeToRGBACatmullRom-8          4068463       2156728       -46.99%
BenchmarkIdct-8                            135           40.7          -69.93%
BenchmarkUpsampleNearestNeighbor-8         318541        124015        -61.07%
BenchmarkUpsampleCatmullRom-8              3548015       411422        -88.40%
```

### Build tags

* `noasm` - do not use SIMD optimizations
