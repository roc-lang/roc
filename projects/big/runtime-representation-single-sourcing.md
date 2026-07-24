# Single-Source the Runtime Value Representation

## Problem

The physical contract of Roc's runtime values — RocStr/RocList field
order and offsets, the small-string flag bit, the seamless-slice tag bit,
the refcount word's position and its static-data sentinel, Dec's 10^18
scale — is defined canonically by the Zig structs in `src/builtins/`
(`str.zig:52`, `list.zig:27`, `utils.zig:286`, `dec.zig:60`). But the
backends do not consume those definitions; they re-encode the same facts
as hand-written magic numbers:

- The dev backend hardcodes RocStr/RocList field offsets as raw integer
  literals — `+ 8` for length, `+ 16` for capacity — at
  `src/backend/dev/LirCodeGen.zig:1379, 1397, 1534, 4889`, with
  comments like "length is at offset 8". It also recomputes
  `roc_str_size = 3 * target_ptr_size` and `small_str_max_len`
  (`LirCodeGen.zig:437-438`).
- The LLVM backend comptime-asserts the header *word count*
  (`src/backend/llvm/layout_types.zig:158-159` — good), but hardcodes
  the field offsets (`rocListLenOffset`/`rocListCapacityOffset`/
  `rocStrCapacityOffset`, `MonoLlvmCodeGen.zig:7584-7592`), the slice
  tag as a literal `1` (`:4320-4321, 7345`), and the small-string bit
  and `0x7F` mask open-coded (`:4301-4312, 4350`). `layout_types.zig:288-307`
  re-states "Str/List = 3 words, Box = 1 word" as `ptr_size * 3`
  literals instead of importing `word_count`/`SizeAlign`.
- The wasm backend is fully manual: field offsets 0/4/8 throughout,
  slice bit decoded via `i32_and 1` / `i32_and -2`
  (`src/backend/wasm/WasmCodeGen.zig:2483-2498`), small-string detected
  by the sign bit of the length word (`:2506-2520`), and the refcount
  contract re-implemented inline — data pointer minus 4 to reach the
  refcount word, `i32_eqz` as the "static data, never touch" check
  (`emitDataPtrIncref` and friends, `:2658-2812`) — with no reference
  to `builtins.utils.REFCOUNT_STATIC_DATA`. `WasmLayout.wasmRepr`
  hardcodes `.str => 12` (`src/backend/wasm/WasmLayout.zig:45-47`)
  while the same file's header comment declares the layout store "the
  single source of truth" for concrete sizes.
- The GDB/LLDB pretty-printers re-implement all of it in Python:
  `DEC_ONE = 10**18`, `SEAMLESS_SLICE_TAG = 1`, the small-string bit,
  refcount-zero-means-static, and Dec's 18-digit fractional formatting
  (`src/backend/llvm/debugger/roc_gdb.py:16-43`, `roc_lldb.py`).
- `src/default_platform/roc_str_view.zig` re-implements the struct
  *and* the logic (`isSmallStr`, `isSeamlessSlice`, `decref` with a
  literal `0` instead of `REFCOUNT_STATIC_DATA`). Its struct shape is
  locked by a real test (`src/builtins/str.zig:5026-5045`), but the
  hand-written logic is not.
- Adjacent: `src/backend/dev/CallingConvention.zig:55-164`
  independently hardcodes the SysV/Win64/AAPCS64 C-ABI facts (register
  files, the 16-byte return-by-pointer threshold, Win64's {1,2,4,8}
  by-value rule) that `src/layout/abi/` already encodes as the shared
  classifier every backend uses for Roc/host calls.

The interpreter is the one executor that does this right — it calls the
builtins structs' own methods (`isSeamlessSlice()`, `isSmallStr()`,
imports `REFCOUNT_STATIC_DATA`). That is exactly what makes the drift
dangerous: a representation change keeps the interpreter (and therefore
most of the test suite, which runs through `--opt=dev` eval) correct
while silently miscompiling the llvm and wasm paths.

## Background

The canonical definitions already export everything needed:
`RocStr.word_count`/`small_str_flag`/`SMALL_STR_BIT` (`str.zig:66-79`,
with a comptime assert tying `word_count` to `@sizeOf`),
`RocList.SEAMLESS_SLICE_TAG`/`word_count` (`list.zig:22-46`),
`utils.REFCOUNT_STATIC_DATA` (`utils.zig:286`), and
`RocDec.decimal_places`/`one_point_zero_i128` (`dec.zig:60-68`). Some
consumers already demonstrate the right pattern: the wasm backend
derives `dec_one_i64` from `RocDec.one_point_zero_i128`
(`WasmCodeGen.zig:20-23`); the LLVM header type is comptime-tied to
`word_count`; and the `roc_str_view` shape test
(`str.zig:5026-5045`) shows how to lock a mirror that cannot share
code. Grep confirms no backend imports `SEAMLESS_SLICE_TAG` or
`small_str_flag` today — only the interpreter and the Python debuggers
reference those names.

## Evidence

- `src/backend/dev/LirCodeGen.zig:1379` — "List is a (ptr, len,
  capacity) triple - length is at offset 8", with the literal `8`.
- `src/backend/wasm/WasmCodeGen.zig:2658-2812` — inline refcount
  arithmetic with `-4` offsets and `i32_eqz` static checks, no
  `REFCOUNT_STATIC_DATA` reference.
- `src/backend/llvm/layout_types.zig:288-307` — `ptr_size * 3`
  literals beside the comptime-checked header type at `:158`.
- `src/backend/wasm/WasmLayout.zig:8-11` vs `:45-47` — the file's own
  stated invariant contradicted three lines of code later.
- `src/default_platform/roc_str_view.zig:59-68` — hand-written decref
  with literal `0` sentinel; only the struct shape is test-locked.
- `src/backend/dev/CallingConvention.zig:69-86, 143-164` vs
  `src/layout/abi/x86_64.zig:73-155` — the same ABI thresholds twice.

## Solution design

1. **Constants flow one way.** Every backend imports
   `builtins.list.SEAMLESS_SLICE_TAG`, `builtins.str.small_str_flag`,
   and `builtins.utils.REFCOUNT_STATIC_DATA` instead of re-spelling
   `1`, `0x80`/sign-bit, and `0`. The wasm inline refcount code keeps
   its inlining (that is a codegen strategy, not a competing fact) but
   its offsets and sentinel come from the builtins module.
2. **Offsets are derived, not memorized.** Replace the dev backend's
   `+8`/`+16` and llvm's `rocListLenOffset` family with values computed
   from `@offsetOf(RocList, "length")` etc., scaled by target word
   size for cross-word-size targets (the structs are all-`usize`
   fields, so offset-in-words × target word size is exact). Same for
   `WasmLayout.wasmRepr`'s `12`/`16` literals.
3. **C-ABI facts unify.** Either route dev's builtin-wrapper calls
   through `layout/abi/call.lower`, or extract the register tables and
   thresholds into `layout/abi/` and have `CallingConvention.zig`
   consume them. One authority for "what does the platform C ABI say",
   two consumers.
4. **Cross-language mirrors get generated or locked.** The Python
   debugger constants (`DEC_ONE`, `SEAMLESS_SLICE_TAG`, small-string
   bit, refcount sentinel) are emitted into a small generated
   `roc_debug_constants.py` from the Zig source (a build step), or at
   minimum a test greps the .py files for the expected literal values
   so a Zig-side change fails CI. Extend the `roc_str_view` shape test
   with behavior tests (`isSmallStr`/`len`/`decref` agreement against
   canonical on a fixture set).

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn 'offset 8\|+ 16' src/backend/dev/LirCodeGen.zig` finds no
  RocStr/RocList field-offset literals; the derivation sites name the
  struct field they derive from.
- No file under `src/backend/` contains a hand-written seamless-slice,
  small-string, or refcount-sentinel literal; all reference the
  builtins constants.
- `WasmLayout.wasmRepr` contains no aggregate-size literals.
- One encoding of each platform C ABI's register file and by-value
  thresholds, consumed by both `layout/abi` classification and dev's
  `CallBuilder`.
- The debugger constants are generated or CI-locked.
- Mutation test passes: flip `SEAMLESS_SLICE_TAG` (or reorder RocStr's
  fields) in a scratch build and confirm every backend either fails to
  compile or fails the lock tests — nothing silently miscompiles.

## How to evaluate the result

### Correctness ideal

A representation change to `src/builtins` is impossible to ship
half-applied: each backend either picks it up automatically (derived
offsets/constants) or fails loudly at comptime/test time (locked
mirrors). The interpreter stops being the only executor that tracks
the builtins by construction.

### Performance ideal

Zero runtime cost — `@offsetOf` and imported constants fold at
comptime to the same immediates the literals produce today. Verify by
diffing generated code for a representative program per backend
(dev object bytes, llvm IR, wasm module) before/after: byte-identical
output expected.

## Tests to add

- Behavior lock for `roc_str_view` logic: `isSmallStr`/`len`/
  `allocationPtr`/`decref` agree with canonical `RocStr` over a fixture
  set (small, big, slice, static).
- Comptime asserts at each derivation site (offset-in-words × word
  size == `@offsetOf` on native).
- Debugger-constants lock (generated file or grep test).
- The mutation checks above, kept as documentation if not automated.

## Related projects

- The landed single-source builtin registration
  (`src/builtins/builtin_registry.zig`) — the symbol name/ABI half of
  the same backend-drift disease, now consolidated; this project is
  the data-layout half.
- [../small/silent-drift-guards.md](../small/silent-drift-guards.md) —
  the pattern language for mirrors that cannot share code (the Python
  debuggers are exactly that case).
