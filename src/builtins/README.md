# Builtins

Built-in functions, types, and runtime support for the Roc language.

## Overview

The builtins module provides the core runtime functionality that every Roc program depends on. It includes fundamental operations, data types, and the interface between Roc code and the host platform.

## Purpose

This module provides:
- **Core Data Types**: Built-in types like strings, numbers, and basic collections
- **Runtime Operations**: Fundamental operations like memory allocation, string manipulation, and arithmetic
- **Host Platform Interface**: The bridge between Roc code and the underlying platform (ABI, system calls, etc.)
- **Standard Library**: Essential functions that are always available in Roc programs

The builtins module is essential for both the compiler (during type checking and code generation) and the runtime (during program execution).

## Numeric Oracle Tests

Numeric builtins should have tests whose expected values come from an
independent, known-good computation rather than from the implementation under
test. Prefer generating those expected values at Zig comptime, then storing them
in ordinary test fixture arrays.

The usual pattern is:

1. Write an explicit input corpus that covers ordinary values, sign boundaries,
   representation boundaries, domain boundaries, overflow/underflow edges,
   branch cutoffs, and special values such as zero, infinity, and NaN where the
   type supports them.
2. Build fixture arrays at comptime from that corpus.
3. Compute expected values with Zig language builtins or `std.math` functions
   that are independent of the Roc builtin implementation being tested.
4. Run the Roc builtin implementation at test runtime and compare it to the
   comptime-generated expectation.

For exact operations, compare exact representations. Examples include integer
overflow flags, bit conversions, parsing/formatting cases with exact expected
text, and Dec operations whose semantics define an exact fixed-point result.
For Dec this usually means comparing the raw scaled `i128` payload.

For approximate operations, keep the approximation explicit in the test name and
fixture. Transcendental functions such as `sin`, `cos`, `tan`, `asin`, `atan`,
`log`, and fractional `pow` generally do not have exactly representable Dec
results. Their tests should use comptime oracle values plus a stated error
envelope, not an ambiguous "close enough" check hidden inside the assertion.
For floating-point functions, prefer ULP-aware comparisons that preserve signed
zero and NaN behavior.

Do not use the implementation under test as its own oracle. Also avoid relying
on a backend host import as evidence for a custom builtin implementation unless
that import is exactly the code under test. For example, a wasm host math import
can validate wasm linking and call plumbing, but direct Zig tests are still
needed for a self-contained builtin math port.

When adding a new numeric builtin, add tests at the lowest layer that owns the
algorithm and add integration coverage for the public Roc API or low-level
operation that reaches it. The comptime oracle fixtures establish algorithmic
expectations; eval, CLI, wasm, and backend tests establish that the compiler and
runtime plumbing call the intended builtin.

## Test Maturity TODOs

- TODO: Add property-style List tests for refcounted elements, seamless slices,
  aliasing, concat/drop/replace/reverse chains, and allocation-count invariants.
  The current List tests cover many targeted cases; the follow-up should add a
  systematic corpus that exercises ownership-sensitive operation sequences.
