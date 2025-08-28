package jpegn

import (
	"bytes"
	"errors"
	"fmt"
	"image"
	"image/color"
	"image/jpeg"
	"io"
	"sync"
)

// Standard error types for JPEG decoding.
var (
	ErrNoJPEG      = errors.New("not a JPEG file")
	ErrUnsupported = errors.New("unsupported format")
	ErrOutOfMemory = errors.New("out of memory")
	ErrInternal    = errors.New("internal error")
	ErrSyntax      = errors.New("syntax error")
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
}

// A reasonable upper limit for the size of JPEG headers.
// Most headers are well under this size (64KB).
const maxHeaderSize = 65536

// A pool for header-sized buffers to reduce allocations in DecodeConfig.
var headerBufferPool = sync.Pool{
	New: func() interface{} {
		// Allocate a buffer large enough to hold typical JPEG headers.
		b := make([]byte, maxHeaderSize)

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
// If the JPEG format is unsupported (e.g., progressive, or CMYK), it falls back to the standard library's decoder.
func Decode(r io.Reader, opts ...*Options) (image.Image, error) {
	data, err := readAllData(r)
	if err != nil {
		return nil, err
	}

	// Get a decoder from the pool.
	d := decoderPool.Get().(*decoder)
	// Ensure the decoder is reset and returned to the pool when finished.
	defer func() {
		d.reset()
		decoderPool.Put(d)
	}()

	// Initialize options.
	d.toRGBA = false
	d.upsampleMethod = NearestNeighbor
	d.autoRotate = false

	if len(opts) > 0 && opts[0] != nil {
		d.upsampleMethod = opts[0].UpsampleMethod
		d.toRGBA = opts[0].ToRGBA
		d.autoRotate = opts[0].AutoRotate
	}

	img, err := d.decode(data, false)
	if err != nil {
		// If the format is unsupported, fall back to the standard library.
		if errors.Is(err, ErrUnsupported) {
			// Note: The standard library's jpeg.Decode does not auto-rotate based on EXIF.
			return jpeg.Decode(bytes.NewReader(data))
		}

		return nil, err
	}

	return img, nil
}

// DecodeConfig returns the color model and dimensions of a JPEG image without decoding the entire image data.
// The dimensions returned are as stored in the file (SOF marker), ignoring any EXIF orientation tags.
func DecodeConfig(r io.Reader) (image.Config, error) {
	// Get a buffer from the pool to avoid allocating a large slice on every call.
	bufPtr := headerBufferPool.Get().(*[]byte)
	defer headerBufferPool.Put(bufPtr)
	headerData := *bufPtr

	// Read the start of the file into the pooled buffer. We expect an
	// io.ErrUnexpectedEOF if the file is smaller than our buffer, which is normal.
	n, err := io.ReadFull(r, headerData)
	if err != nil && !errors.Is(err, io.ErrUnexpectedEOF) {
		// A read error or an empty file (n=0, err=io.EOF) is fatal.
		return image.Config{}, err
	}

	if n == 0 {
		return image.Config{}, ErrNoJPEG
	}

	// Use a decoder from our pool.
	d := decoderPool.Get().(*decoder)
	defer func() {
		d.reset()
		decoderPool.Put(d)
	}()

	// We disable autorotation parsing for DecodeConfig as we only need the raw dimensions from SOF.
	d.autoRotate = false

	if _, err := d.decode(headerData[:n], true); err != nil {
		if errors.Is(err, ErrUnsupported) {
			// Fallback for unsupported formats. We must provide the standard library's decoder with the full image stream.
			// We reconstruct it by joining the header data we already read with the rest of the original reader.
			fullReader := io.MultiReader(bytes.NewReader(headerData[:n]), r)

			return jpeg.DecodeConfig(fullReader)
		}

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
