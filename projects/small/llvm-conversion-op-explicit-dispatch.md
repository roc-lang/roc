# Explicit Dispatch for LLVM Numeric-Conversion Ops

## Problem

The numeric-conversion `LowLevel` ops (~230 variants: every
`int_to_int`, `to_dec`, `to_frac`, `_wrap`/`_try`/`_trunc` family
member) are handled by explicit exhaustive switch arms in the
interpreter, the dev backend, and the wasm backend — so adding or
renaming one is a compile error there. The LLVM backend is the
exception: its `emitLowLevel` dispatch ends in
`else => try self.emitNumericConversionOrCrash(...)`
(`src/backend/llvm/MonoLlvmCodeGen.zig:2945`), and that function
derives semantics by **parsing the enum tag name**
(`const name = @tagName(op)` at `:3543`, then
`std.mem.endsWith(u8, name, "_to_dec")` at `:3585`,
`endsWith(name, "_try")` at `:3589`,
`std.mem.find(u8, name, "_to_")` / `"_try"` / `"_str"` at
`:3599-3601`), falling back to emitting a runtime crash with the tag
name as the message.

The enum's naming convention is load-bearing codegen logic in exactly
one of four executors. A new conversion op that doesn't fit the
substring heuristics — or a variant renamed for clarity — compiles
cleanly and either misroutes through a heuristic or becomes a runtime
crash in compiled programs only, while the other three backends would
have refused to build. This is both a competing source of truth (the
op semantics, encoded as name substrings) and a violation of the
repo-wide rule that layout/check/eval logic must not dispatch on
strings.

## Background

`src/base/LowLevel.zig` is the deliberate single source for the op
vocabulary, and its value is precisely that Zig switches over it are
exhaustive. The dev backend handles these ops as grouped explicit arms
(`src/backend/dev/LirCodeGen.zig:2287, 2311, 2341`), wasm likewise
(`src/backend/wasm/WasmCodeGen.zig:10717-10805`). `LowLevel.zig`
already hosts per-op metadata tables (`rcEffect`,
`numericParseSpec`), so a conversion-classification table has an
obvious home beside them.

## Evidence

- `src/backend/llvm/MonoLlvmCodeGen.zig:2945` — the `else` prong that
  exempts LLVM from the exhaustiveness guarantee.
- `:3542-3608` — `emitNumericConversionOrCrash`'s substring
  heuristics and the `emitCrashBytes(name)` fallback.
- The three sibling backends' explicit arms (cited above) — proof the
  explicit form is tractable at this op count.

## Solution design

1. **Classify once, in data.** Add a comptime classifier to
   `src/base/LowLevel.zig` (beside `rcEffect`):
   `conversionSpec(op) ?struct { src: NumType, dst: NumType, mode:
   enum { exact, wrap, try_, trunc, to_str } }`. It may be *built* at
   comptime from the tag names (the convention is real and
   comptime-checkable) — the difference from today is that the parse
   happens once, at comptime, with a `@compileError` for any
   conversion-shaped tag it cannot classify, instead of at emission
   time with a crash fallback.
2. **LLVM switches exhaustively.** `emitLowLevel` loses its `else`;
   conversion ops dispatch on `conversionSpec(op)` (a data lookup, no
   strings at runtime), and non-conversion ops get their own arms as
   in the other backends. `emitCrashBytes(name)` as a
   reachable-by-design path is deleted; unsupported ops become a
   compile error in the backend, matching dev/wasm.
3. **Optional convergence.** dev and wasm may later consume the same
   `conversionSpec` to shrink their grouped arms, but that is not
   required for this project — the goal is ending LLVM's silent-drift
   exemption.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -n '@tagName(op)' src/backend/llvm/MonoLlvmCodeGen.zig` finds
  no semantic use (debug messages may keep it).
- `emitLowLevel`'s switch has no `else` prong; adding a `LowLevel`
  variant breaks the LLVM build until handled, exactly as it breaks
  dev/wasm/interpreter today.
- Renaming any conversion op's tag is semantics-neutral (the comptime
  builder either still classifies it or fails the build — never a
  silent behavior change).
- LLVM-path output for every existing conversion op is unchanged
  (existing eval/backend agreement tests stay green).

## How to evaluate the result

### Correctness ideal

All four executors have the same drift profile: unhandled op = build
failure. The naming convention becomes documentation, checked at
comptime, rather than runtime dispatch data.

### Performance ideal

Strictly better: substring scans at emission time are replaced by a
comptime-built table lookup. No generated-code change (verify by
diffing emitted IR for a conversion-heavy fixture).

## Tests to add

- A comptime totality test: every op the other backends classify as a
  conversion has a `conversionSpec`, and vice versa.
- One end-to-end fixture per conversion mode (exact/wrap/try/trunc/
  to_str) compiled via the LLVM path and compared against interpreter
  results — pinning that the de-stringing changed nothing.

## Related projects

- The landed single-source builtin registration
  (`src/builtins/builtin_registry.zig`) — cured the same backend's
  symbol-string drift; same disease, neighboring mechanism.
