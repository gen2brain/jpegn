## jpegn
[![Status](https://github.com/gen2brain/jpegn/actions/workflows/test.yml/badge.svg)](https://github.com/gen2brain/jpegn/actions)
[![Go Reference](https://pkg.go.dev/badge/github.com/gen2brain/jpegn.svg)](https://pkg.go.dev/github.com/gen2brain/jpegn)

[JPEG](https://en.wikipedia.org/wiki/JPEG) image decoder and encoder in pure Go.

No CGo, no dependencies.

SIMD support for amd64 (SSE4.1, AVX2), arm64 (NEON) and riscv64 (RVV, with `GORISCV64=rva23u64`).
Build with `-tags noasm` for pure Go everywhere.

### Decoding

```go
img, err := jpegn.Decode(r)
```

`jpegn.Decode` returns the image in its native color space, `*image.YCbCr`, `*image.Gray`,
`*image.CMYK` or `*image.RGBA`, and registers itself with `image.RegisterFormat`. It can also
force RGBA output, pick the chroma upsampling filter, apply the EXIF orientation, and scale by
1/2, 1/4 or 1/8 inside the inverse transform, which costs less than a full decode.

### Encoding

```go
err := jpegn.Encode(w, img, &jpegn.EncodeOptions{Quality: 90})
```

Subsampling follows the source, so an `image.YCbCr` keeps its native ratio and its planes are
encoded without resampling. Progressive mode, optimized Huffman tables, adaptive quantization
and restart intervals are all opt-in, and EXIF and anything else, such as an ICC profile,
carry through a decode and re-encode.

### Supported

The decoder reads baseline and progressive JPEG, 8-bit, grayscale, YCbCr, RGB, CMYK and YCCK,
any power-of-two subsampling, and restart intervals. Truncated or corrupt scan data decodes as
far as it goes rather than failing. Arithmetic coding and 12-bit samples are refused.

The encoder writes baseline and progressive JPEG, grayscale or YCbCr, at 4:4:4, 4:4:0, 4:2:2 or
4:2:0. Files are within 0.2% of the standard library at the same quality and subsampling, and
identical to libjpeg-turbo at quality 50. Optimized Huffman tables save ~4% at quality 50,
rising to ~20% at quality 95; progressive mode ~9% more on flat color and hard edges, and little
or nothing on detailed photographic content; jpegli's adaptive dead zone ~10% again. All three
together are ~17% below plain baseline at ~1.6x the encode time.

Against the standard library, ~1.9x on progressive and ~3.1x on baseline decode, ~2.9x decoding
straight to RGBA, and ~4.5x to ~6.3x on encode, in two allocations rather than twelve. SIMD is
worth ~1.3x to ~3.7x over `-tags noasm`. On a machine without AVX2 the SSE4.1 kernels still
give ~1.7x to ~2.8x over pure Go.

### License

MIT, in [LICENSE](LICENSE).
