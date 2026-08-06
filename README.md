## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

JPEG decoder and encoder with SIMD optimizations.

The decoder reads baseline and progressive JPEG, including grayscale, RGB, CMYK and YCCK, and is
resilient to truncated or corrupt scan data. The encoder writes baseline JPEG with a choice of
chroma subsampling, optional optimized Huffman tables, restart intervals and embedded metadata.

Assembly paths are bit-identical to the pure Go ones, so output does not depend on the architecture
or on the `noasm` build tag.

### Encoding

```go
opts := &jpegn.EncodeOptions{
    Quality:        90,
    Subsampling:    jpegn.Subsample420, // or Auto, 444, 440, 422, Gray
    OptimizeCoding: true,               // smaller files, one extra pass
}

err := jpegn.Encode(w, img, opts)
```

`SubsampleAuto` follows the source: an `image.YCbCr` keeps its native ratio and its planes are
used without resampling, an `image.Gray` is written as a single component, anything else becomes 4:2:0.

Metadata is carried through a decode and re-encode with `RawExif`:

```go
raw, _ := jpegn.RawExif(bytes.NewReader(src))

err := jpegn.Encode(w, img, &jpegn.EncodeOptions{
    Quality:          90,
    Exif:             raw,
    ResetOrientation: true, // when the pixels were already rotated
    Segments:         []jpegn.Segment{{Marker: 0xE2, Data: iccProfile}},
})
```

### Benchmark

Decoding, compared to the standard library:
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

Encoding, compared to the standard library:
```
BenchmarkEncodeRGBA420-8           	    1251	   1922799 ns/op	 136.33 MB/s	   33285 B/op	       2 allocs/op
BenchmarkEncodeRGBA420Stdlib-8     	     403	   6046358 ns/op	  43.36 MB/s	   66032 B/op	      12 allocs/op

BenchmarkEncodeYCbCr420-8          	    1630	   1536924 ns/op	 170.56 MB/s	   33173 B/op	       2 allocs/op
BenchmarkEncodeYCbCr420Stdlib-8    	     421	   5650503 ns/op	  46.39 MB/s	   66032 B/op	      12 allocs/op

BenchmarkEncodeGray-8              	    1741	   1364447 ns/op	 192.12 MB/s	   33074 B/op	       2 allocs/op
BenchmarkEncodeGrayStdlib-8        	     655	   3512462 ns/op	  74.63 MB/s	   65936 B/op	      10 allocs/op
```

The standard library always writes 4:2:0 with the standard Huffman tables, so these have no
counterpart:
```
BenchmarkEncodeRGBA444-8           	     986	   2458430 ns/op	 106.63 MB/s	   41999 B/op	       2 allocs/op
BenchmarkEncodeRGBA422-8           	    1051	   2284407 ns/op	 114.75 MB/s	   42352 B/op	       2 allocs/op
BenchmarkEncodeRGBA420Optimize-8   	     771	   3113893 ns/op	  84.19 MB/s	   33567 B/op	       2 allocs/op
```

File size is within 0.2% of the standard library at the same quality and subsampling.
`OptimizeCoding` reduces it by 4% at quality 50, rising to 20% at quality 95.

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

BenchmarkEncodeRGBA420-8                   3821227       1783265       -53.33%
BenchmarkEncodeRGBA444-8                   5646200       2326511       -58.80%
BenchmarkEncodeYCbCr420-8                  2352321       1359983       -42.19%
BenchmarkEncodeGray-8                      1853314       1216213       -34.38%
BenchmarkFDCT-8                            114.1         24.9          -78.16%
BenchmarkRGBToYCbCrRow-8                   4994          404           -91.91%
BenchmarkQuantizeBlock-8                   22.3          16.9          -24.32%
```

### Build tags

* `noasm` - do not use SIMD optimizations
