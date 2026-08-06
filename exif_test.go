package jpegn

import (
	"bytes"
	_ "embed"
	"image/jpeg"
	"testing"
)

//go:embed testdata/test.exif.canon.jpg
var testExifCanon []byte

//go:embed testdata/test.exif.gps.jpg
var testExifGPS []byte

//go:embed testdata/test.exif.invalid.jpg
var testExifInvalid []byte

// TestDecodeExifCanon tests EXIF parsing with a Canon camera image containing
// camera make/model, exposure settings, and date/time information.
func TestDecodeExifCanon(t *testing.T) {
	exif, err := DecodeExif(bytes.NewReader(testExifCanon))
	if err != nil {
		t.Fatalf("DecodeExif failed: %v", err)
	}

	// Verify basic camera info
	if exif.Make == "" {
		t.Error("Expected Make to be set")
	}
	if exif.Model == "" {
		t.Error("Expected Model to be set")
	}

	// Log the values for manual inspection
	t.Logf("Make: %s", exif.Make)
	t.Logf("Model: %s", exif.Model)
	t.Logf("DateTime: %s", exif.DateTime)
	t.Logf("DateTimeOriginal: %s", exif.DateTimeOriginal)
	t.Logf("Orientation: %d", exif.Orientation)
	t.Logf("Width: %d, Height: %d", exif.Width, exif.Height)
	t.Logf("ExposureTime: %f", exif.ExposureTime)
	t.Logf("FNumber: %f", exif.FNumber)
	t.Logf("ISOSpeed: %d", exif.ISOSpeed)
	t.Logf("FocalLength: %f", exif.FocalLength)
	t.Logf("Flash: %d", exif.Flash)

	// Verify orientation is reasonable (1-8)
	if exif.Orientation < 0 || exif.Orientation > 8 {
		t.Errorf("Invalid orientation: %d", exif.Orientation)
	}

	// If exposure settings are present, verify they're reasonable
	if exif.ExposureTime > 0 {
		if exif.ExposureTime < 0 || exif.ExposureTime > 60 {
			t.Errorf("Unreasonable exposure time: %f", exif.ExposureTime)
		}
	}
	if exif.FNumber > 0 {
		if exif.FNumber < 1 || exif.FNumber > 64 {
			t.Errorf("Unreasonable f-number: %f", exif.FNumber)
		}
	}
	if exif.ISOSpeed > 0 {
		if exif.ISOSpeed < 50 || exif.ISOSpeed > 102400 {
			t.Errorf("Unreasonable ISO speed: %d", exif.ISOSpeed)
		}
	}
	if exif.FocalLength > 0 {
		if exif.FocalLength < 1 || exif.FocalLength > 1000 {
			t.Errorf("Unreasonable focal length: %f", exif.FocalLength)
		}
	}
}

// TestDecodeExifGPS tests EXIF parsing with GPS location data.
func TestDecodeExifGPS(t *testing.T) {
	exif, err := DecodeExif(bytes.NewReader(testExifGPS))
	if err != nil {
		t.Fatalf("DecodeExif failed: %v", err)
	}

	t.Logf("GPSLatitude: %f", exif.GPSLatitude)
	t.Logf("GPSLongitude: %f", exif.GPSLongitude)
	t.Logf("GPSAltitude: %f", exif.GPSAltitude)

	// GPS coordinates should be within valid ranges
	// Latitude: -90 to +90
	// Longitude: -180 to +180
	if exif.GPSLatitude != 0 {
		if exif.GPSLatitude < -90 || exif.GPSLatitude > 90 {
			t.Errorf("Invalid GPS latitude: %f", exif.GPSLatitude)
		}
	}
	if exif.GPSLongitude != 0 {
		if exif.GPSLongitude < -180 || exif.GPSLongitude > 180 {
			t.Errorf("Invalid GPS longitude: %f", exif.GPSLongitude)
		}
	}
}

// TestDecodeExifInvalid tests error handling with invalid EXIF data.
func TestDecodeExifInvalid(t *testing.T) {
	exif, err := DecodeExif(bytes.NewReader(testExifInvalid))

	// This should either succeed (if EXIF data is present but has some invalid fields)
	// or fail with an error (if EXIF data is completely missing/corrupted)
	if err != nil {
		t.Logf("DecodeExif returned error as expected: %v", err)
		return
	}

	// If it succeeded, log what we got
	t.Logf("DecodeExif succeeded, got exif data:")
	t.Logf("Make: %s", exif.Make)
	t.Logf("Model: %s", exif.Model)
}

// TestDecodeExifNoExif tests that an error is returned when no EXIF data is present.
func TestDecodeExifNoExif(t *testing.T) {
	// Use a test image without EXIF data
	_, err := DecodeExif(bytes.NewReader(test420))
	if err == nil {
		t.Error("Expected error for image without EXIF data, got nil")
	}
	t.Logf("Got expected error: %v", err)
}

// TestDecodeExifInvalidJPEG tests that an error is returned for invalid JPEG data.
func TestDecodeExifInvalidJPEG(t *testing.T) {
	invalidData := []byte{0x00, 0x00, 0x00, 0x00}
	_, err := DecodeExif(bytes.NewReader(invalidData))
	if err == nil {
		t.Error("Expected error for invalid JPEG data, got nil")
	}
	t.Logf("Got expected error: %v", err)
}

// TestRawExifRoundTrip carries metadata through a decode and re-encode.
func TestRawExifRoundTrip(t *testing.T) {
	for _, tc := range []struct {
		name string
		data []byte
	}{
		{"canon", testExifCanon},
		{"gps", testExifGPS},
		{"orientation", test420o},
	} {
		t.Run(tc.name, func(t *testing.T) {
			raw, err := RawExif(bytes.NewReader(tc.data))
			if err != nil {
				t.Fatalf("RawExif: %v", err)
			}

			want, err := DecodeExif(bytes.NewReader(tc.data))
			if err != nil {
				t.Fatalf("DecodeExif source: %v", err)
			}

			src, err := Decode(bytes.NewReader(tc.data))
			if err != nil {
				t.Fatalf("Decode: %v", err)
			}

			var buf bytes.Buffer
			if err := Encode(&buf, src, &EncodeOptions{Quality: 90, Exif: raw}); err != nil {
				t.Fatalf("Encode: %v", err)
			}

			got, err := DecodeExif(bytes.NewReader(buf.Bytes()))
			if err != nil {
				t.Fatalf("DecodeExif output: %v", err)
			}

			if *got != *want {
				t.Errorf("exif changed:\n got %+v\nwant %+v", *got, *want)
			}

			if _, err := jpeg.Decode(bytes.NewReader(buf.Bytes())); err != nil {
				t.Errorf("stdlib decode: %v", err)
			}
		})
	}
}

// TestEncodeResetOrientation checks the orientation tag is normalized.
func TestEncodeResetOrientation(t *testing.T) {
	raw, err := RawExif(bytes.NewReader(test420o))
	if err != nil {
		t.Fatalf("RawExif: %v", err)
	}

	before, err := DecodeExif(bytes.NewReader(test420o))
	if err != nil {
		t.Fatalf("DecodeExif: %v", err)
	}

	if before.Orientation == 1 {
		t.Fatalf("fixture orientation is already 1, nothing to reset")
	}

	src, err := Decode(bytes.NewReader(test420o), &Options{ToRGBA: true, AutoRotate: true})
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}

	var buf bytes.Buffer
	if err := Encode(&buf, src, &EncodeOptions{Quality: 90, Exif: raw, ResetOrientation: true}); err != nil {
		t.Fatalf("Encode: %v", err)
	}

	got, err := DecodeExif(bytes.NewReader(buf.Bytes()))
	if err != nil {
		t.Fatalf("DecodeExif output: %v", err)
	}

	if got.Orientation != 1 {
		t.Errorf("orientation = %d, want 1", got.Orientation)
	}

	if got.Make != before.Make || got.Model != before.Model {
		t.Errorf("resetting orientation disturbed other tags")
	}

	if raw2, _ := RawExif(bytes.NewReader(test420o)); !bytes.Equal(raw, raw2) {
		t.Error("ResetOrientation mutated the caller's slice")
	}
}

// TestEncodeSegments writes extra application and comment segments.
func TestEncodeSegments(t *testing.T) {
	src := synthImage(32, 32)
	comment := []byte("jpegn test comment")
	icc := append([]byte("ICC_PROFILE\x00\x01\x01"), make([]byte, 64)...)

	var buf bytes.Buffer
	err := Encode(&buf, src, &EncodeOptions{
		Quality: 80,
		Segments: []Segment{
			{Marker: 0xFE, Data: comment},
			{Marker: 0xE2, Data: icc},
		},
	})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}

	out := buf.Bytes()
	if !bytes.Contains(out, comment) {
		t.Error("comment segment missing")
	}

	if !bytes.Contains(out, icc) {
		t.Error("ICC segment missing")
	}

	if _, err := jpeg.Decode(bytes.NewReader(out)); err != nil {
		t.Errorf("stdlib decode: %v", err)
	}

	if _, err := Decode(bytes.NewReader(out)); err != nil {
		t.Errorf("Decode: %v", err)
	}
}

// TestEncodeSegmentValidation rejects markers and sizes that cannot be written.
func TestEncodeSegmentValidation(t *testing.T) {
	src := synthImage(16, 16)

	for _, tc := range []struct {
		name string
		opts *EncodeOptions
	}{
		{"SOF marker", &EncodeOptions{Segments: []Segment{{Marker: 0xC0, Data: []byte{1}}}}},
		{"SOS marker", &EncodeOptions{Segments: []Segment{{Marker: 0xDA, Data: []byte{1}}}}},
		{"oversized", &EncodeOptions{Segments: []Segment{{Marker: 0xE3, Data: make([]byte, 65534)}}}},
		{"oversized exif", &EncodeOptions{Exif: make([]byte, 65534)}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			if err := Encode(&buf, src, tc.opts); err == nil {
				t.Error("expected an error")
			}
		})
	}
}

// TestEncodeExifReplacesJFIF checks APP1 precedes the frame and APP0 is dropped.
func TestEncodeExifReplacesJFIF(t *testing.T) {
	raw, err := RawExif(bytes.NewReader(testExifCanon))
	if err != nil {
		t.Fatalf("RawExif: %v", err)
	}

	src := synthImage(32, 32)

	var withExif, plain bytes.Buffer
	if err := Encode(&withExif, src, &EncodeOptions{Quality: 80, Exif: raw}); err != nil {
		t.Fatalf("Encode: %v", err)
	}

	if err := Encode(&plain, src, &EncodeOptions{Quality: 80}); err != nil {
		t.Fatalf("Encode: %v", err)
	}

	if got := withExif.Bytes()[2:4]; got[0] != 0xFF || got[1] != 0xE1 {
		t.Errorf("first marker = %X, want FFE1", got)
	}

	if bytes.Contains(withExif.Bytes()[:64], []byte("JFIF")) {
		t.Error("JFIF APP0 written alongside EXIF APP1")
	}

	if !bytes.Contains(plain.Bytes()[:32], []byte("JFIF")) {
		t.Error("JFIF APP0 missing when no EXIF is supplied")
	}
}

// TestEncodeMetadataNotPooled checks a pooled encoder does not leak metadata.
func TestEncodeMetadataNotPooled(t *testing.T) {
	raw, err := RawExif(bytes.NewReader(testExifCanon))
	if err != nil {
		t.Fatalf("RawExif: %v", err)
	}

	src := synthImage(24, 24)

	var first bytes.Buffer
	if err := Encode(&first, src, &EncodeOptions{Quality: 80, Exif: raw,
		Segments: []Segment{{Marker: 0xFE, Data: []byte("secret")}}}); err != nil {
		t.Fatalf("Encode: %v", err)
	}

	for i := 0; i < 8; i++ {
		var next bytes.Buffer
		if err := Encode(&next, src, &EncodeOptions{Quality: 80}); err != nil {
			t.Fatalf("Encode: %v", err)
		}

		if bytes.Contains(next.Bytes(), []byte("secret")) {
			t.Fatal("comment leaked into a later encode")
		}

		if _, err := DecodeExif(bytes.NewReader(next.Bytes())); err == nil {
			t.Fatal("EXIF leaked into a later encode")
		}
	}
}
