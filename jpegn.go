package jpegn

import (
	"errors"
	"fmt"
	"image"
	"image/color"
	"io"
	"sync"
)

// Standard error types for JPEG decoding.
var (
	ErrNoJPEG             = errors.New("not a JPEG file")
	ErrNoEXIF             = errors.New("no EXIF data found")
	ErrUnsupported        = errors.New("unsupported format")
	ErrOutOfMemory        = errors.New("out of memory")
	ErrInternal           = errors.New("internal error")
	ErrSyntax             = errors.New("syntax error")
	ErrMissingHuffmanCode = errors.New("missing Huffman code")
)

// UpsampleMethod defines the algorithm used for chroma upsampling.
type UpsampleMethod int

const (
	// NearestNeighbor is a fast but low-quality upsampling method.
	NearestNeighbor UpsampleMethod = iota
	// CatmullRom is a higher-quality bicubic upsampling method.
	CatmullRom
)

// Options specifies decoding parameters.
type Options struct {
	// ToRGBA forces the output image to be in the RGBA color space.
	// If false, the image will be returned in its native color space,
	// which is typically YCbCr for color images or Grayscale for monochrome images.
	ToRGBA bool
	// UpsampleMethod defines the algorithm used for chroma upsampling when
	// converting a subsampled YCbCr image to a full-color format.
	// This option is only used when the output is RGBA (either because
	// ToRGBA is true or the source JPEG is in the RGB format).
	UpsampleMethod UpsampleMethod
	// AutoRotate enables automatic image rotation based on the EXIF orientation tag.
	// If true, the decoded image will be rotated/flipped to match the intended viewing orientation.
	// This process forces the output to RGBA if a transformation is applied.
	AutoRotate bool
	// ScaleDenom specifies the IDCT scaling denominator for efficient downscaling.
	// Valid values are 1 (no scaling), 2 (1/2 size), 4 (1/4 size), or 8 (1/8 size).
	// Invalid values will be clamped to the nearest valid value.
	// This produces higher quality and faster decoding than decoding full size and then downsampling.
	ScaleDenom int
}

// Initial buffer size for reading JPEG headers in DecodeConfig.
// We use a larger buffer to handle images with sizeable APP markers (EXIF, Adobe, etc.).
// Many images have EXIF data in the 2-16KB range before the SOF marker.
const initialHeaderSize = 16384 // 16KB should handle most images

// A pool for header-sized buffers to reduce allocations in DecodeConfig.
var headerBufferPool = sync.Pool{
	New: func() interface{} {
		// Allocate initial buffer for reading JPEG headers.
		b := make([]byte, initialHeaderSize)

		return &b
	},
}

// decoderPool is a pool of decoder structs to reduce allocation overhead.
var decoderPool = sync.Pool{
	New: func() interface{} {
		return newDecoder()
	},
}

// Interface to check if a reader knows its remaining length.
type readerWithLen interface {
	Len() int
}

// readAllData reads data from r, pre-allocating if the size is known.
func readAllData(r io.Reader) ([]byte, error) {
	// Pre-allocate buffer if the reader knows its remaining length.
	// This significantly reduces allocations compared to io.ReadAll for large images.
	if rl, ok := r.(readerWithLen); ok {
		size := rl.Len()
		if size > 0 {
			data := make([]byte, size)
			_, err := io.ReadFull(r, data)
			if err != nil {
				return nil, fmt.Errorf("failed to read image data: %w", err)
			}

			return data, nil
		}
	}

	// Fallback for readers that don't implement Len() (e.g., network streams, os.File) or were empty.
	return io.ReadAll(r)
}

// Decode reads a JPEG image from r and returns it as an [image.Image].
// It accepts an optional Options struct to control decoding parameters.
func Decode(r io.Reader, opts ...*Options) (image.Image, error) {
	data, err := readAllData(r)
	if err != nil {
		return nil, err
	}

	// Get a decoder from the pool.
	d := decoderPool.Get().(*decoder)

	d.reset()

	// Ensure the decoder is reset and returned to the pool when finished.
	defer func() {
		d.reset()
		decoderPool.Put(d)
	}()

	// Initialize options.
	d.toRGBA = false
	d.upsampleMethod = NearestNeighbor
	d.autoRotate = false
	d.scaleDenom = 1 // Default: no scaling

	if len(opts) > 0 && opts[0] != nil {
		d.upsampleMethod = opts[0].UpsampleMethod
		d.toRGBA = opts[0].ToRGBA
		d.autoRotate = opts[0].AutoRotate

		// Validate and set scaleDenom - must be 1, 2, 4, or 8
		scaleDenom := opts[0].ScaleDenom
		if scaleDenom <= 0 || scaleDenom == 1 {
			scaleDenom = 1
		} else if scaleDenom <= 2 {
			scaleDenom = 2
		} else if scaleDenom <= 4 {
			scaleDenom = 4
		} else {
			scaleDenom = 8
		}
		d.scaleDenom = scaleDenom
	}

	img, err := d.decode(data, false)
	if err != nil {
		return nil, err
	}

	return img, nil
}

// DecodeConfig returns the color model and dimensions of a JPEG image without decoding the entire image data.
// The dimensions returned are as stored in the file (SOF marker), ignoring any EXIF orientation tags.
func DecodeConfig(r io.Reader) (image.Config, error) {
	// Strategy: read incrementally until we find the SOF marker.
	// Start with a pooled buffer, then read more if needed.
	bufPtr := headerBufferPool.Get().(*[]byte)
	defer headerBufferPool.Put(bufPtr)
	initialBuf := *bufPtr

	// Try to read the initial buffer
	n, err := io.ReadFull(r, initialBuf)
	if err != nil && !errors.Is(err, io.ErrUnexpectedEOF) {
		return image.Config{}, err
	}

	if n == 0 {
		return image.Config{}, ErrNoJPEG
	}

	var data []byte
	// If we read less than the full buffer, the file is small - use what we got
	if n < len(initialBuf) {
		data = initialBuf[:n]
	} else {
		// File is larger than initial buffer - read the rest to ensure we get SOF
		// For efficiency, read everything remaining since we already buffered 16KB
		remaining, err := io.ReadAll(r)
		if err != nil {
			return image.Config{}, err
		}
		// Combine initial buffer with remaining data
		data = make([]byte, n+len(remaining))
		copy(data, initialBuf[:n])
		copy(data[n:], remaining)
	}

	// Use a decoder from our pool.
	d := decoderPool.Get().(*decoder)

	d.resetForConfig()

	defer func() {
		d.resetForConfig()
		decoderPool.Put(d)
	}()

	// We disable autorotation parsing for DecodeConfig as we only need the raw dimensions from SOF.
	d.autoRotate = false
	d.scaleDenom = 1 // Always use full size for DecodeConfig

	if _, err := d.decode(data, true); err != nil {
		return image.Config{}, err
	}

	var cm color.Model
	switch d.ncomp {
	case 1:
		cm = color.GrayModel
	case 3:
		if d.isRGB {
			cm = color.RGBAModel
		} else {
			cm = color.YCbCrModel
		}
	case 4:
		cm = color.CMYKModel
	default:
		return image.Config{}, ErrUnsupported
	}

	return image.Config{
		ColorModel: cm,
		Width:      d.width,
		Height:     d.height,
	}, nil
}

// init registers the JPEG format with the standard library's image package.
// This allows image.Decode to automatically recognize and decode JPEG files using this package.
func init() {
	decodeWrapper := func(r io.Reader) (image.Image, error) {
		return Decode(r)
	}

	image.RegisterFormat("jpeg", "\xff\xd8", decodeWrapper, DecodeConfig)
}
