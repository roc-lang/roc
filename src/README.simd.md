# SIMD Optimizations

This document describes the SIMD (Single Instruction, Multiple Data) optimizations and how to configure them.

## Overview

Roc uses SIMD instructions to accelerate performance-critical operations by processing multiple data elements simultaneously. We detect the target CPU's SIMD capabilities and select optimal vector widths, while also providing manual override options for advanced users.

## Current SIMD Implementations

### chompTrivia (Tokenizer)

**Location:** `src/check/parse/tokenize.zig`

**Purpose:** The `chompTrivia` function processes whitespace characters (spaces, tabs, newlines, comments) during tokenization. This is a hot path in the compiler as source files often contain significant amounts of leading whitespace and indentation.

**Why SIMD:** Traditional scalar processing examines one character at a time. SIMD allows processing 8-64 characters simultaneously, providing significant speedups for:
- Files with deep indentation
- Code with extensive whitespace
- Large source files with many comments

**Implementation Details:**
- Processes chunks of uniform whitespace (all spaces or all tabs) using vector operations
- Falls back to scalar processing for mixed content or special characters
- Uses `@splat`, `@reduce`, and `@select` Zig SIMD builtins
- Tracks SIMD usage for debugging and performance analysis

## Build Configuration

### Auto-Detection (Default)

The build system automatically detects optimal SIMD width based on target CPU features:

```bash
# Uses auto-detected optimal SIMD width
zig build
zig build test
zig build roc
```

**Detection Logic:**
- **x86_64/x86:** AVX-512 (64 bytes) → AVX/AVX2 (32 bytes) → SSE2 (16 bytes) → SSE (8 bytes)
- **ARM64:** NEON (16 bytes) → fallback (8 bytes)
- **ARM:** NEON (16 bytes) → fallback (8 bytes)
- **RISC-V:** Vector extension (16 bytes) → fallback (8 bytes)
- **Other architectures:** Conservative 8-byte default

### Manual Configuration

#### Disable SIMD

Use scalar processing only (useful for debugging or compatibility):

```bash
zig build -Dsimd=false
zig build test -Dsimd=false
zig build roc -Dsimd=false
```

#### Override SIMD Width

Force a specific vector width (must be supported by target):

```bash
# Basic SIMD (8 bytes)
zig build -Dsimd-width=8

# SSE2/NEON (16 bytes)
zig build -Dsimd-width=16

# AVX/AVX2 (32 bytes)
zig build -Dsimd-width=32

# AVX-512 (64 bytes)
zig build -Dsimd-width=64
```

#### Cross-Compilation Examples

```bash
# Target modern Intel CPU with AVX2
zig build -Dtarget=x86_64-linux -Dcpu=haswell
# Auto-detects: 32 bytes

# Target older x86_64 with SSE2
zig build -Dtarget=x86_64-linux -Dcpu=x86_64
# Auto-detects: 16 bytes

# Target ARM64 with NEON
zig build -Dtarget=aarch64-linux
# Auto-detects: 16 bytes

# Force specific width on any target (if supported)
zig build -Dtarget=x86_64-linux -Dsimd-width=32
```

## Build Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `-Dsimd` | bool | `true` | Enable/disable all SIMD optimizations |
| `-Dsimd-width` | int | auto-detected | SIMD vector width in bytes (8/16/32/64) |
