package jpegn

import (
	"bytes"
	_ "embed"
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
