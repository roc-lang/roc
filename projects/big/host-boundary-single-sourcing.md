# Single-Source the Host/Platform ABI Boundary

## Problem

`src/builtins/host_abi.zig` is the canonical host ABI, and every
Zig-side consumer inside the compiler correctly imports it. But the ABI
is restated, as text nothing checks, everywhere the import cannot
reach:

1. **The glue code-generator templates.** The `RocOps` vtable (six
   callbacks and their exact C-ABI signatures, `host_abi.zig:108-141`)
   is re-emitted as string literals by `src/glue/src/ZigGlue.roc:1561-1568`,
   `RustGlue.roc:924-956`, and `CGlue.roc:897-914`. The bare
   `extern_host` symbols (`host_abi.zig:62-69`) — whose doc comment
   claims they are "written down in exactly one place" — are re-emitted
   by `ZigGlue.roc:2017-2022` and `RustGlue.roc:2304, 2402`. RocStr and
   RocList struct definitions are re-emitted by all three templates
   (`ZigGlue.roc:1768, 501-613`; `CGlue.roc:890-892`, size-assert
   only; `RustGlue.roc:1283, 1488`). The generated `RocHost` has
   already diverged structurally from canonical `RocOps` (which carries
   `env` and `hosted_fns` fields the templates model differently).
2. **The glue platform's Roc↔Zig struct mirrors.**
   `src/glue/platform/host.zig:352-413` hand-mirrors eight Roc types
   from `src/glue/platform/*.roc` as `extern struct`s, relying on
   comments like "Field order matches …roc as compiled by Roc" — i.e.
   on a human preserving alphabetical field order. A renamed or added
   Roc field silently reorders the ABI and the host reads garbage. The
   `roc_make_glue` entrypoint symbol is spelled in three files
   (`platform/main.roc:37`, `platform/host.zig:423`,
   `glue.zig:337, 479`).
3. **The shim symbol names.** The strings bridging compiled code to the
   shims — `"roc_shim_get_ops"`, `"roc_entrypoint"`,
   `"roc_entrypoint_from_image"`, `"roc_shim_hosted_fns"`,
   `"roc_shim_hosted_count"` — are raw literals on both sides:
   emission in `src/backend/llvm/MonoLlvmCodeGen.zig:469-500, 1594,
   1640, 1645` versus definition in `src/machine_code_shim/main.zig:750-754`,
   `src/interpreter_shim/main.zig:213-231`, and
   `src/shim_host_abi.zig:19-20`. No shared constants module; a typo is
   a link failure at best.
4. **The test-host boilerplate.** The size-tracking allocator scheme
   (prefix-stored size, `rocAllocFn`/`rocDeallocFn`/`rocReallocFn`,
   and the `roc_alloc`… export names) is copy-pasted across ~9-12
   `host.zig` files: `src/glue/platform/host.zig:170-314` plus
   `test/{alloc-count,archive,dylib,int,fx,fx-open,str,http-headers,
   json-decoder,wasm,...}/platform/host.zig`. A fix in one does not
   propagate.

Minor but same-shaped: `src/ipc/SharedMemoryAllocator.zig:229` re-types
the `0x524F4353` header magic as a bare literal instead of the
`HEADER_MAGIC` constant declared at `:45`.

## Background

The Zig-internal side of this boundary is healthy — the shims, eval,
and backends all import `builtins.host_abi`; the dev `RunImage` format
is consumed by the machine-code shim via direct import
(`machine_code_shim/main.zig:31`); and the hot-reload IPC protocol is
single-defined with a `@compileError` size guard
(`src/ipc/hot_reload.zig`). The one properly enforced *mirror* is
`src/default_platform/roc_str_view.zig`, locked by the test at
`src/builtins/str.zig:5026-5045` — the model for every mirror below
that cannot share code. The glue templates target three foreign
languages, so true unification is impossible there; the achievable end
state is that every mirror is either generated or lock-tested, and
every symbol name exists in exactly one Zig constant.

## Evidence

- `src/builtins/host_abi.zig:55-61` — the "exactly one place" comment,
  contradicted by the template emissions cited above.
- `src/glue/platform/host.zig:388-398` — "Field order matches
  src/glue/platform/…roc as compiled by Roc" comments as the only
  enforcement.
- `grep -rn '"roc_shim_get_ops"' src/` — two independent spellings,
  emitter and definer, no shared const.
- `test/alloc-count/platform/host.zig:24-83` vs
  `test/archive/platform/host.zig:26-67` vs
  `test/dylib/platform/host.zig:27-68` — near-identical allocator
  bodies.
- `src/glue/README.md:164` — the only current template check is "the
  generated Zig compiles."

## Solution design

1. **One shim-symbols module.** A leaf `src/builtins/shim_symbols.zig`
   (or a decl block in `host_abi.zig`) declaring every boundary symbol
   name as `pub const`. The LLVM emitter, both shims, and
   `shim_host_abi.zig` consume it. `grep -rn '"roc_shim_\|"roc_entrypoint'`
   outside that module should end up matching nothing. Same treatment
   for `roc_make_glue`. Fix the IPC magic literal while in the area.
2. **Golden compile-and-compare tests for the glue templates.** A test
   step runs glue, compiles the generated Zig platform code, and
   comptime-asserts its `RocHost`/`RocStr`/`RocList` layouts
   (size, alignment, field offsets, callback signatures) against the
   imported `builtins` definitions — the `roc_str_view` pattern
   applied at the template boundary. For C and Rust output, extend the
   existing `ROC_STATIC_ASSERT` emission to field offsets, and add a
   CI check that compiles the generated C against a header derived
   from `host_abi.zig`. Resolve the existing `RocOps`/`RocHost`
   structural divergence as part of writing the first assertion.
3. **Generate the glue-platform mirrors.** The eight `extern struct`
   mirrors in `glue/platform/host.zig` are mechanical functions of the
   `.roc` sources (alphabetical field order, known layout rules).
   Either generate them in the build (the builtin-compiler step
   already parses Roc), or lock them with a test that canonicalizes
   the `.roc` files and asserts field order/types match the Zig
   mirrors. Generation is preferred: it deletes the manual-sync
   contract instead of guarding it.
4. **One test-host allocator.** Extract the tracking allocator and the
   `roc_alloc`… export block into a shared module under
   `src/builtins/` (or a `test/support/` module) that every test
   platform imports; each host keeps only its platform-specific
   entrypoints. Fold the export-name strings into the same module so
   the Finding-2-style symbol duplication on the implementation side
   disappears with it.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Every boundary symbol name (`roc_shim_*`, `roc_entrypoint*`,
  `roc_alloc`…, `roc_make_glue`) has exactly one Zig declaration;
  emitters and definers both reference it.
- The glue golden tests exist and fail when `host_abi.zig` or the
  builtins structs change without a matching template change
  (demonstrated by a mutation run).
- The `host_abi.zig` "exactly one place" comment is true, or amended
  to name the generated/locked mirrors.
- `glue/platform/host.zig`'s struct mirrors are generated or
  lock-tested; the "field order matches" comments are gone or point at
  the enforcing test.
- Test hosts share one allocator implementation; a behavior fix lands
  in one file.

## How to evaluate the result

### Correctness ideal

No hand-synced restatement of the host ABI survives unguarded: every
mirror is generated, imported, or covered by a test that a mutation
provably trips. Renaming a glue-platform Roc field breaks a test, not
a user's generated host.

### Performance ideal

Neutral at runtime — these are build-time/test-time structures. Glue
generation output is byte-identical for unchanged inputs (diff the
generated files for the test platforms before/after).

## Tests to add

- Shim-symbol round-trip: a link test that builds a minimal image and
  resolves every symbol in `shim_symbols.zig` through both shims.
- The glue golden compile-and-compare suite (Zig full layout; C/Rust
  static-assert emission checked by compiling generated output).
- Glue-platform mirror lock (if generation is deferred): Roc field
  order vs Zig `extern struct` order.
- Allocator-module conformance: the alloc-count test platform's
  accounting semantics pinned against the shared module.

## Related projects

- [runtime-representation-single-sourcing.md](runtime-representation-single-sourcing.md)
  — the same builtins structs restated inside the compiler's backends;
  this project covers their restatements outside the compiler.
- [../small/hosted-extern-declared-abi.md](../small/hosted-extern-declared-abi.md)
  — the type-level half of the host contract; this project is the
  symbol/layout half.
