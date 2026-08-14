## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

[JPEG](https://en.wikipedia.org/wiki/JPEG) image decoder and encoder in pure Go.

No CGo, no dependencies.

SIMD support for amd64 (AVX2) and arm64 (NEON). Build with `-tags noasm` for pure Go everywhere.

### Decoding

```go
img, err := jpegn.Decode(r)
```

`jpegn.Decode` returns the image in its native color space, `*image.YCbCr`, `*image.Gray`,
`*image.CMYK` or `*image.RGBA`, and registers itself with `image.RegisterFormat`. `Options`
forces RGBA output, selects the chroma upsampling filter, applies the EXIF orientation, and
scales by 1/2, 1/4 or 1/8 during the inverse transform.

### Encoding

```go
err := jpegn.Encode(w, img, &jpegn.EncodeOptions{Quality: 90})
```

`EncodeOptions` selects the chroma subsampling, optimized Huffman tables and restart intervals.
Subsampling follows the source by default, so an `image.YCbCr` keeps its native ratio and its
planes are encoded without resampling. `RawExif` and `Segments` carry EXIF and anything else,
such as an ICC profile, through a decode and re-encode.

### Supported

The decoder reads baseline and progressive JPEG, 8-bit, grayscale, YCbCr, RGB, CMYK and YCCK,
any power-of-two subsampling, and restart intervals. Truncated or corrupt scan data decodes as
far as it goes rather than failing. Arithmetic coding and 12-bit samples are refused.

The encoder writes baseline JPEG, grayscale or YCbCr, at 4:4:4, 4:4:0, 4:2:2 or 4:2:0. Files are
within 0.2% of the standard library at the same quality and subsampling, and identical to
libjpeg-turbo at quality 50; optimized Huffman tables save ~4% at quality 50, rising to ~20% at
quality 95.

Against the standard library, ~1.7x on progressive and ~2.5x on baseline decode, and ~3.5x on
encode, in two allocations rather than twelve. SIMD is worth ~1.3x to ~3x over `-tags noasm`.

### License

MIT, in [LICENSE](LICENSE).
