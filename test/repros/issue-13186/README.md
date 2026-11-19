# Nested expects in x86_64 macOS test units

Issue: https://github.com/roc-lang/roc/issues/13186

## Reproduction

The host-independent regression exercises the actual x64mac expect-site
emitter and constructs its unit-image object:

```sh
zig build run-test-zig-module-backend -j2 \
  -Dtest-filter='issue 13186' -Dtest-filter='backend tests' \
  -Dtest-filter='backend module imports' -- --test-filter 'issue 13186'
```

It crashes with:

```text
Mach-O invariant violated: an abs32s data relocation, which only an x86_64 test unit emits
```

The test asserts successful construction, preservation of the site addend,
and the expect-site-base symbol, not a particular instruction encoding.
It will pass unchanged after the emitter/writer contract is corrected.

The source-level regression is `test/cli/Issue13186NestedExpect.roc`,
registered in the CLI subcommands suite as `issue 13186`. On Intel macOS:

```sh
zig-out/bin/roc test test/cli/Issue13186NestedExpect.roc
```

Expected: `All (2) tests passed`. The Intel CLI command was not run here:
this host is ARM64 macOS without x86_64 execution support.
The exact command above passes on this ARM64 host with the locally built
ReleaseFast compiler, confirming the issue's architecture-specific control.

## Root cause

`src/compile/boxy_unit.zig:822-824` enables relocatable expect sites for
test units. `LirCodeGen.emitRelocatableExpectSite`
(`src/backend/dev/LirCodeGen.zig:22479`) selects `mov r64, imm32` with a
signed `abs32s` relocation for every x86_64 target. That relocation reaches
`ObjectWriter.emitIndexed`, which sends it to the target's format writer.
`src/backend/dev/object/macho.zig:387` explicitly rejects it.

This is a producer/format contract violation, not a large allocation. The
current checkout exposes an invariant failure at this boundary; the issue's
reported compiler surfaced an out-of-memory diagnostic. No old compiler was
used to establish how that version translated the failure.

ARM64 uses an `abs64_field` literal supported by Mach-O. Linux has an ELF
`R_X86_64_32S` encoding. Interpreter, LSS, and optimized test execution do
not use this Boxy test-unit relocation path. Compiling every procedure in
the module explains why an uncalled function containing an expect suffices.

## Recommended fix

Represent an expect site as the unsigned 32-bit id it actually is. Emit
`mov r32, imm32` via `movRegImm32ZeroExtend` on x86_64 and carry an explicit
unsigned absolute 32-bit field relocation, including its in-field addend.
Support that producer-selected kind coherently through `Relocation.zig`,
ELF (`R_X86_64_32`), Mach-O (`X86_64_RELOC_UNSIGNED`, four-byte field), COFF,
and unit-image conversion/loading. Preserve the existing ARM64 literal path.

This avoids both the unsupported signed Mach-O encoding and an unnecessary
signed-id limit. It is smaller than the current sign-extending instruction
and needs neither an extra load nor a full 64-bit immediate. Account for the
optional REX prefix when recording the field offset. Verify ordinary and
extended registers, nonzero addends/base ids, the full u32 range, and
encoded-object/direct-image equivalence. Update the test-unit design contract.
Do not turn a format mismatch into an allocation error or bypass test units.
