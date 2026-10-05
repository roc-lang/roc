# Zig 0.17 upgrade validation

This records the source migration and validation on the Zig 0.17 upgrade
branch. Roc requires Zig 0.17.0 and a compatible roc-bootstrap dependency
bundle containing LLVM/Clang/LLD 22.1.8. A published compatible bundle and the
complete native compiler validation are still pending.

## Source and compiler correctness

The following module suites passed in ReleaseSafe on x86_64 Linux. Counts
are from the recorded full-suite runs; the base, parse, and builtins runs
passed, but their counts were not retained.

| Module | Passed | Skipped |
| --- | ---: | ---: |
| collections | 37 | 0 |
| base, parse, builtins | full suites passed | count not retained |
| can | 275 | 0 |
| types | 79 | 0 |
| layout | 98 | 0 |
| check | 1,659 | 0 |
| backend | 547 | 1 |
| lir_core | 27 | 0 |
| lir | 537 | 0 |
| postcheck | 622 | 1 |
| compile | 781 | 0 |

Run these module leaves from the repository root with Zig 0.17.0:

```sh
for module in collections base parse builtins can types layout check backend lir_core lir postcheck compile; do
    zig build "run-test-zig-module-$module" -Doptimize=ReleaseSafe -j2
done
```

The compile suite uses the vendored Zig LLVM IR builder and does not need
LLVM library linkage. The complete eval suite does link LLVM support and
remains pending the compatible bundle. The recorded large compile runs
used the generated standalone `zig test` commands from the module graph;
the module leaves above regenerate the dependency and embedding inputs.

The full check suite covers the allocator fixture lifetime corrections and
allocation-failure sweeps. Those sweeps disable test allocator resize/remap
so Zig 0.17 backing growth, which depends on neighboring allocation state,
does not alter the failure index. The normal SafeAllocator remains the
backing allocator and retains leak and double-free checks.

The full compile suite exposed a production Boxy lifetime bug: evaluating
the destination of a dictionary-result assignment before materialization
held an address into an array that materialization could grow. A GDB
hardware watchpoint confirmed the write through freed storage. Computing
the result first and then indexing the destination fixes the callable-use
and both nested-callable-use stores. All 20 issue 11305 Boxy/LSS fixtures
and the full 781-test compile suite pass with this fix.

Additional focused ReleaseSafe checks:

```sh
# Five tests, including the module's declaration-import test.
zig build run-test-zig-module-compile -Doptimize=ReleaseSafe -j2 \
    -Dtest-filter="source version pins" \
    -Dtest-filter="parse-stage record" \
    -Dtest-filter="checked module cache header"

# Twenty issue fixtures plus the declaration-import test.
zig build run-test-zig-module-compile -Doptimize=ReleaseSafe -j2 \
    -Dtest-filter="issue 11305"

# The scanner fixture references a declared function's returned FnId.
zig build run-test-zig-module-postcheck -Doptimize=ReleaseSafe -j2 \
    -Dtest-filter="call-pattern scans direct call and function reference capture operands"
```

The source-pin regression changes a recognized human nightly version
through actual canonicalized and checked cache hits. It preserves semantic
artifact keys, refreshes the current mismatch warning, matches uncached
diagnostics, and keeps unpinned modules cached. Unrecognized local version
strings normalize to the same skipped pin-validation input across HEAD
changes. Producer-authored metadata records this validation input at both
cache levels.

Other validated source checks include canonical C/Rust glue output byte
equality against the pre-migration compiler, seven target ABI compile
checks for the emitted Zig glue, and Bytebox continuation-count regression
coverage beyond 65,535. Bytebox's public `Val` retains 16-byte size and
alignment. The echo-WASM runner compiles in Debug and its argument contract
was checked outside the checkout; executing the complete echo artifact is
still pending.

After the full suites, the official Zig 0.17 formatter updated 144 tracked
source/test/vendor files. A token-preserving comparison verified only enum
builtin spelling/intCast wrappers, pointer-cast nesting normalization, and
whitespace changed; quoted text and comments stayed intact. The subsequent
real-FnId fixture change passed its two focused ReleaseSafe tests.

```sh
zig build run-check-zig-format --cache-poison=disallowed
```

This public formatting leaf passes. `ReleaseFast` and `ReleaseSafe` remain
accepted CLI values in Zig 0.17; their internal enum tags are now `.fast`
and `.safe`.

## Build graph, caching, and fixture isolation

The following integration scripts exercise the actual Zig build graph:

```sh
python3 ci/test_build_identity.py /path/to/zig-0.17.0
python3 ci/test_build_cache.py /path/to/zig-0.17.0 --work-dir /new/validation/path
python3 ci/test_fixture_isolation.py /path/to/zig-0.17.0
zig test src/build/helpers_test_root.zig -O ReleaseSafe -lc
```

The cache check passes with a Debug host builtin compiler and three
independently executed, byte-identical bakes. Unchanged, HEAD-only,
display-version, docs-only, and dedicated-test edits reuse the compiler and
all bakes. A production edit invalidates them; restoration reuses the
original results. Distinct declared output names ensure the three bakes
cannot collapse into one cached invocation. Checkpoint logs were retained
at `/tmp/roc-build-cache-017-checkpoint` for build commit `002baaaa86`.

The fixture check passes concurrent separate-cache/mode and shared-cache
graphs, selected host overlays, preserved tracked CRT inputs, immutable
cached roots, private runner writes, and reuse after `.pyc`/`.pyo` edits.
Generated hosts are cached outputs. Manual benchmark scripts explicitly
publish the selected hosts with `update-test-fixtures` before running
checkout fixtures. Publication uses the existing `build-test-hosts`
dependency tree and preserves `-Dplatform` filtering.

The helper suite passes 11 ReleaseSafe tests. Mutable header changes behind
an escaped Nix-prefix path change the actual compiler identity instead of
being mistaken for immutable store inputs. Nine source check leaves and
five archive-padding integration cases passed. Linux, Windows GNU, and
aarch64 macOS help graphs configure with `--cache-poison=disallowed`;
these are graph checks, not native execution results.

The benchmark runner help text now names Zig 0.17.0, and profiling recipes
retain debug symbols. The first help check in the upgrade worktree exposed
an existing EXIT trap that attempted cleanup of ignored `bench-main` and
`bench-local` directories; their prior existence is unknown. Baseline
worktrees and `/tmp` outputs were separate. Trap registration now follows
argument parsing. Subsequent help validation used an isolated script copy
with canary artifacts and confirmed both directories remained intact.

For separate caches, use `--cache-dir` for the local build cache and
`ZIG_GLOBAL_CACHE_DIR` for Zig's global cache. Zig 0.17's `zig build` does
not accept the compiler subcommand's `--global-cache-dir` flag.

## Workarounds and native validation

- The four LLVM scaling fixes remain ported to LLVM 22.1.8. The assertions
  harness passes 1,236 targeted lit tests, with 347 unsupported and one
  expected failure; LLVM unit suites pass 1,355 tests with 48 skips. The
  CodeGenPrepare port preserves LLVM 22 attached debug-record insertion
  semantics. The Roc LLVM bridge also passes a C++ syntax probe against
  full LLVM 22.1.8 headers.
- The BSD compiler-rt exclusion was removed after a controlled Debug build
  runner reproduced Zig 0.16 x86_64 FreeBSD/OpenBSD/NetBSD BUS crashes and
  Zig 0.17 passed. Roc extern-builtins plus compiler-rt compile on those
  three BSD targets and x86_64/aarch64 macOS. This is cross-compilation
  evidence; native BSD/macOS execution has not been performed. macOS keeps
  its separate compiler-rt exclusion because final linking uses libSystem.
- The LLVM loop-vectorization workaround remains: its upstream fix targets
  LLVM 23 while this upgrade uses LLVM 22.
- Final archive-padding repair remains because minimal cases passing on
  both Zig versions do not prove the Roc/macOS problem fixed. It now
  writes a declared cached output without mutating producer archives.
- The Linux libc-free host CPU detection workaround remains: the direct
  Zig 0.17 getauxval probe still returns zero. Historical Zig 0.16 source
  and ABI provenance comments retain their original version references.

Pending validation uses the complete roc-bootstrap LLVM 22 musl/libc++
bundle: native Roc linking, the full eval suite, native app and test fixture
execution, and cold/warm compiler and application cache measurements.
Performance runs must use an unstripped ReleaseFast Roc compiler; no
native Roc timing improvement is claimed by the graph cache checks.

```sh
zig build roc -Doptimize=ReleaseFast -Dstrip=false \
    -Droc-deps-path=/path/to/compatible-bundle
zig build run-test-zig-module-eval -Doptimize=ReleaseSafe \
    -Droc-deps-path=/path/to/compatible-bundle
```

Related investigation: [Roc issue 11896](https://github.com/roc-lang/roc/issues/11896).
