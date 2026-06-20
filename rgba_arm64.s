//go:build arm64 && !noasm

#include "textflag.h"

// NEON implementations of the plane-to-RGBA conversions. The grayscale and RGB
// kernels rely on the NEON VST4 structured store to interleave the planes with a
// constant alpha. The YCbCr kernel performs the JFIF fixed-point conversion in
// 32-bit lanes (matching the scalar reference exactly) before narrowing back to
// bytes with unsigned saturation.
//
// Several 32-bit-lane SIMD instructions (VMUL, signed SSHR, UQXTN/SQXTUN) are
// not recognized by the Go assembler and are emitted via WORD directives, using
// the same approach as idct_arm64.s.

// func grayToRGBANEON(dst, gray []byte)
// Processes 16 pixels per iteration. gray_len is a multiple of 16.
TEXT ·grayToRGBANEON(SB), NOSPLIT, $0-48
	MOVD  dst_base+0(FP), R0
	MOVD  gray_base+24(FP), R1
	MOVD  gray_len+32(FP), R2
	VMOVI $255, V3.B16            // constant alpha channel

gray_loop:
	CMP  $16, R2
	BLT  gray_done
	VLD1.P 16(R1), [V0.B16]      // R = gray
	VMOV   V0.B16, V1.B16        // G = gray
	VMOV   V0.B16, V2.B16        // B = gray
	VST4.P [V0.B16, V1.B16, V2.B16, V3.B16], 64(R0)
	SUB  $16, R2
	B    gray_loop

gray_done:
	RET

// func rgbToRGBANEON(dst, r, g, b []byte)
// Processes 16 pixels per iteration. r_len is a multiple of 16.
TEXT ·rgbToRGBANEON(SB), NOSPLIT, $0-96
	MOVD  dst_base+0(FP), R0
	MOVD  r_base+24(FP), R1
	MOVD  g_base+48(FP), R2
	MOVD  b_base+72(FP), R3
	MOVD  r_len+32(FP), R4
	VMOVI $255, V3.B16            // constant alpha channel

rgb_loop:
	CMP  $16, R4
	BLT  rgb_done
	VLD1.P 16(R1), [V0.B16]      // R
	VLD1.P 16(R2), [V1.B16]      // G
	VLD1.P 16(R3), [V2.B16]      // B
	VST4.P [V0.B16, V1.B16, V2.B16, V3.B16], 64(R0)
	SUB  $16, R4
	B    rgb_loop

rgb_done:
	RET

// func ycbcrToRGBANEON(dst, y, cb, cr []byte)
// Processes 8 pixels per iteration. y_len is a multiple of 8.
TEXT ·ycbcrToRGBANEON(SB), NOSPLIT, $0-96
	MOVD dst_base+0(FP), R0
	MOVD y_base+24(FP), R1
	MOVD cb_base+48(FP), R2
	MOVD cr_base+72(FP), R3
	MOVD y_len+32(FP), R4

	// Broadcast the JFIF fixed-point coefficients (scaled by 256) into 32-bit lanes.
	MOVD  $359, R5; VDUP R5, V16.S4 // R from Cr (1.402 * 256)
	MOVD  $88, R5;  VDUP R5, V17.S4 // G from Cb (0.344136 * 256)
	MOVD  $183, R5; VDUP R5, V18.S4 // G from Cr (0.714136 * 256)
	MOVD  $454, R5; VDUP R5, V19.S4 // B from Cb (1.772 * 256)
	MOVD  $128, R5; VDUP R5, V20.S4 // bias / rounding constant
	VMOVI $255, V21.B16             // constant alpha channel

ycbcr_loop:
	CMP $8, R4
	BLT ycbcr_done

	VLD1.P 8(R1), [V0.B8] // Y
	VLD1.P 8(R2), [V1.B8] // Cb
	VLD1.P 8(R3), [V2.B8] // Cr

	// yVal = Y << 8, widened to 32-bit lanes (V4 = pixels 0-3, V5 = pixels 4-7).
	VUXTL  V0.B8, V6.H8
	VUXTL  V6.H4, V4.S4
	VUXTL2 V6.H8, V5.S4
	VSHL   $8, V4.S4, V4.S4
	VSHL   $8, V5.S4, V5.S4

	// cbVal = Cb - 128 (V7 lo, V8 hi).
	VUXTL  V1.B8, V6.H8
	VUXTL  V6.H4, V7.S4
	VUXTL2 V6.H8, V8.S4
	VSUB   V20.S4, V7.S4, V7.S4
	VSUB   V20.S4, V8.S4, V8.S4

	// crVal = Cr - 128 (V9 lo, V10 hi).
	VUXTL  V2.B8, V6.H8
	VUXTL  V6.H4, V9.S4
	VUXTL2 V6.H8, V10.S4
	VSUB   V20.S4, V9.S4, V9.S4
	VSUB   V20.S4, V10.S4, V10.S4

	// R = (yVal + 359*crVal + 128) >> 8, narrowed with unsigned saturation.
	WORD $0x4EB09D2B            // VMUL V11.4S, V9.4S, V16.4S  (359*crLo)
	WORD $0x4EB09D4C            // VMUL V12.4S, V10.4S, V16.4S (359*crHi)
	VADD V4.S4, V11.S4, V11.S4
	VADD V5.S4, V12.S4, V12.S4
	VADD V20.S4, V11.S4, V11.S4
	VADD V20.S4, V12.S4, V12.S4
	WORD $0x4F38056B            // SSHR V11.4S, V11.4S, #8
	WORD $0x4F38058C            // SSHR V12.4S, V12.4S, #8
	WORD $0x2E61496D            // UQXTN  V13.4H, V11.4S
	WORD $0x6E61498D            // UQXTN2 V13.8H, V12.4S
	WORD $0x2E2129A0            // SQXTUN V0.8B, V13.8H  (R)

	// B = (yVal + 454*cbVal + 128) >> 8.
	WORD $0x4EB39CEB            // VMUL V11.4S, V7.4S, V19.4S  (454*cbLo)
	WORD $0x4EB39D0C            // VMUL V12.4S, V8.4S, V19.4S  (454*cbHi)
	VADD V4.S4, V11.S4, V11.S4
	VADD V5.S4, V12.S4, V12.S4
	VADD V20.S4, V11.S4, V11.S4
	VADD V20.S4, V12.S4, V12.S4
	WORD $0x4F38056B            // SSHR V11.4S, #8
	WORD $0x4F38058C            // SSHR V12.4S, #8
	WORD $0x2E61496D            // UQXTN  V13.4H, V11.4S
	WORD $0x6E61498D            // UQXTN2 V13.8H, V12.4S
	WORD $0x2E2129A2            // SQXTUN V2.8B, V13.8H  (B)

	// G = (yVal - 88*cbVal - 183*crVal + 128) >> 8.
	WORD $0x4EB19CEB            // VMUL V11.4S, V7.4S, V17.4S  (88*cbLo)
	WORD $0x4EB19D0C            // VMUL V12.4S, V8.4S, V17.4S  (88*cbHi)
	WORD $0x4EB29D2D            // VMUL V13.4S, V9.4S, V18.4S  (183*crLo)
	WORD $0x4EB29D4E            // VMUL V14.4S, V10.4S, V18.4S (183*crHi)
	VSUB V11.S4, V4.S4, V15.S4  // gLo = yValLo - 88*cbLo
	VSUB V13.S4, V15.S4, V15.S4 // gLo -= 183*crLo
	VADD V20.S4, V15.S4, V15.S4 // gLo += 128
	VSUB V12.S4, V5.S4, V11.S4  // gHi = yValHi - 88*cbHi
	VSUB V14.S4, V11.S4, V11.S4 // gHi -= 183*crHi
	VADD V20.S4, V11.S4, V11.S4 // gHi += 128
	WORD $0x4F3805EF            // SSHR V15.4S, #8 (gLo)
	WORD $0x4F38056B            // SSHR V11.4S, #8 (gHi)
	WORD $0x2E6149ED            // UQXTN  V13.4H, V15.4S
	WORD $0x6E61496D            // UQXTN2 V13.8H, V11.4S
	WORD $0x2E2129A1            // SQXTUN V1.8B, V13.8H  (G)

	// Interleave R,G,B with constant alpha and store 8 RGBA pixels.
	VMOV   V21.B16, V3.B16
	VST4.P [V0.B8, V1.B8, V2.B8, V3.B8], 32(R0)

	SUB $8, R4
	B   ycbcr_loop

ycbcr_done:
	RET
