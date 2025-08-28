//go:build amd64 && !noasm

#include "textflag.h"

// hasAVX2 returns true if the CPU supports AVX2 instructions and the OS enables their use.
// This function uses CPUID to check hardware support and XGETBV to check OS support.
TEXT ·hasAVX2(SB), NOSPLIT, $0-1
	// First, check if CPUID supports function 7 (maxID >= 7).
	// CPUID with EAX=0 returns the maximum function number in EAX.
	MOVL $0, AX     // Set EAX to 0 for maximum function query.
	CPUID           // Execute CPUID; results in EAX, EBX, ECX, EDX.
	CMPL AX, $7     // Compare max function with 7.
	JL   no_support // If less than 7, jump to no_support (AVX2 requires function 7).

	// Now, query CPUID function 1 to check for OSXSAVE (bit 27 in ECX).
	// OSXSAVE indicates the OS supports saving extended states with XSAVE/XRSTOR.
	MOVL $1, AX     // Set EAX to 1 for basic feature info.
	MOVL $0, CX     // Set ECX to 0 (subleaf).
	CPUID           // Execute CPUID.
	BTL  $27, CX    // Test bit 27 in ECX (OSXSAVE).
	JNC  no_support // If not set (no carry), jump to no_support.

	// If OSXSAVE is supported, use XGETBV to check OS support for AVX.
	// XGETBV with ECX=0 returns the XCR0 register in EAX:EDX.
	// For AVX, we need bits 1 (XMM) and 2 (YMM) set in XCR0.
	MOVL $0, CX     // Set ECX to 0 for XCR0.
	XGETBV          // Execute XGETBV; result in EAX (low) and EDX (high).
	ANDL $6, AX     // Mask bits 1 and 2 (0b110).
	CMPL AX, $6     // Check if both bits are set.
	JNE  no_support // If not equal, jump to no_support.

	// Finally, query CPUID function 7, subleaf 0, for AVX2 support.
	// AVX2 is indicated by bit 5 in EBX.
	MOVL $7, AX     // Set EAX to 7 for extended features.
	MOVL $0, CX     // Set ECX to 0 (subleaf).
	CPUID           // Execute CPUID.
	BTL  $5, BX     // Test bit 5 in EBX (AVX2).
	JNC  no_support // If not set, jump to no_support.

	// If all checks pass, return true.
	MOVB $1, ret+0(FP) // Set return value to 1 (true). Note: bool is 1 byte.
	RET                // Return.

no_support:
	// Return false if any check fails.
	MOVB $0, ret+0(FP) // Set return value to 0 (false).
	RET
