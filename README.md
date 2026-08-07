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
BenchmarkDecodeBaseline420-8             	    3058	    791961 ns/op	  426324 B/op	       6 allocs/op
BenchmarkDecodeBaseline420StdLib-8       	    1106	   2047137 ns/op	  407090 B/op	       5 allocs/op

BenchmarkDecodeProgressive420-8          	     991	   2303827 ns/op	 1999880 B/op	      10 allocs/op
BenchmarkDecodeProgressive420StdLib-8    	     603	   4017204 ns/op	 1980104 B/op	      17 allocs/op

BenchmarkDecodeConfig-8                  	 4975368	       474.3 ns/op	      48 B/op	       1 allocs/op
BenchmarkDecodeConfigStdLib-8            	 1219048	      2059 ns/op	   13616 B/op	       2 allocs/op

BenchmarkDecodeToRGBANearestNeighbor-8   	    1822	   1317247 ns/op	 2000039 B/op	      10 allocs/op
BenchmarkDecodeToRGBACatmullRom-8        	    1633	   1423599 ns/op	 2262183 B/op	      12 allocs/op
BenchmarkDecodeToRGBAStdLib-8            	     740	   3219690 ns/op	 1455735 B/op	       7 allocs/op
```

Encoding, compared to the standard library:
```
BenchmarkEncodeRGBA420-8                 	    1651	   1407864 ns/op	 186.20 MB/s	   32834 B/op	       2 allocs/op
BenchmarkEncodeRGBA420Stdlib-8           	     456	   5381613 ns/op	  48.71 MB/s	   66032 B/op	      12 allocs/op

BenchmarkEncodeYCbCr420-8                	    2404	    998885 ns/op	 262.44 MB/s	   33062 B/op	       2 allocs/op
BenchmarkEncodeYCbCr420Stdlib-8          	     517	   4809590 ns/op	  54.50 MB/s	   66032 B/op	      12 allocs/op

BenchmarkEncodeGray-8                    	    2581	    931858 ns/op	 281.31 MB/s	   33160 B/op	       2 allocs/op
BenchmarkEncodeGrayStdlib-8              	     750	   3108625 ns/op	  84.33 MB/s	   65936 B/op	      10 allocs/op
```

The standard library always writes 4:2:0 with the standard Huffman tables, so these have no
counterpart:
```
BenchmarkEncodeRGBA444-8                 	    1480	   1624043 ns/op	 161.41 MB/s	   41675 B/op	       2 allocs/op
BenchmarkEncodeRGBA422-8                 	    1438	   1658117 ns/op	 158.10 MB/s	   41514 B/op	       2 allocs/op
BenchmarkEncodeRGBA420Optimize-8         	    1134	   2226588 ns/op	 117.73 MB/s	   33331 B/op	       2 allocs/op
```

File size is within 0.2% of the standard library at the same quality and subsampling, and identical
to libjpeg-turbo at quality 50. `OptimizeCoding` reduces it by 4% at quality 50, rising to 20% at
quality 95.

Difference with assembly optimizations (noasm vs asm):
```
benchmark                                  old ns/op     new ns/op     delta
BenchmarkDecodeBaseline420-8               1703998       898566        -47.27%
BenchmarkDecodeProgressive420-8            2970034       2335332       -21.37%
BenchmarkDecodeToRGBANearestNeighbor-8     2969821       1277749       -56.98%
BenchmarkDecodeToRGBACatmullRom-8          4189137       1423913       -66.01%
BenchmarkIdct-8                            161.3         42.81         -73.46%
BenchmarkUpsampleNearestNeighbor-8         352034        142694        -59.47%
BenchmarkUpsampleCatmullRom-8              2957886       397606        -86.56%

BenchmarkEncodeRGBA420-8                   3987337       1542867       -61.31%
BenchmarkEncodeRGBA444-8                   5651131       1699801       -69.92%
BenchmarkEncodeYCbCr420-8                  2447185       1065880       -56.44%
BenchmarkEncodeGray-8                      1881926       985907        -47.61%
BenchmarkFDCT-8                            113.4         24.62         -78.29%
BenchmarkRGBToYCbCrRow-8                   4841          367           -92.41%
BenchmarkQuantizeBlock-8                   173.2         24.83         -85.66%
```

### Build tags

* `noasm` - do not use SIMD optimizations
