# Next Steps

## Current State

- The snapshot blocker is fixed.
- The docs snapshot panic in `test/snapshots/docs_transitive_modules.md` was caused by `MirToLir` still emitting LIR calls around wrapped bad callees.
- `MirToLir` now resolves wrapped `block` / `lookup` / `struct_access` callees back to `runtime_err_*` or `runtime_err_anno_only` before building an LIR `.call`.
- The interpreter cleanup needed for `minici` is also in:
  - `LirInterpreter.init` is now fallible instead of using `@panic("OOM")`
  - the tag-payload invariant now uses the interpreter crash path instead of `std.debug.panic`

## Verified

- `zig build snapshot -- test/snapshots/docs_transitive_modules.md --threads 1 --verbose` passes
- `zig build snapshot --summary failures` passes
- Focused LIR checks still pass:
  - `zig build test-lir -- --test-filter "canonicalizes field wrapper callable defs"`
  - `zig build test-lir -- --test-filter "RC live cell consumed final use transfers ownership out of cell"`

## What `minici` Shows Now

`zig build minici` now gets past the original snapshot failure and fails later on separate issues:

- baseline repo lint-policy failures in untouched files
- an unrelated layout failure:
  - `src/layout/store_test.zig:1304`
  - `test.fromTypeVar - recursive nominal with Box has no double-boxing (issue #8916)`

Those are not the original snapshot blocker.

## Next Investigation Track

Return to the remaining dev app use-after-free in the platform / hosted-call path.

Known status:

- REPL snapshot regressions were fixed by the RC changes already in progress
- the standalone app repro still crashes in the dev backend
- the remaining issue looks like a separate refcount / ownership problem in the platform-hosted-call path, not in the snapshot/docs/typecheck path

## Suggested Next Commands

```sh
./zig-out/bin/roc --opt=dev tmp_box_list_app.roc
gdb --batch -x tmp_trace_str_helpers.gdb --args ./tmp_box_list_app
```

## Relevant Files

- `src/lir/MirToLir.zig`
- `src/eval/interpreter.zig`
- `src/lir/rc_insert.zig`
- `test/snapshots/docs_transitive_modules.md`
