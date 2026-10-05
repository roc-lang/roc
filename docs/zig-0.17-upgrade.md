# Zig 0.17 upgrade validation

This records the source migration and validation on the Zig 0.17 upgrade
branch. Roc requires Zig 0.17.0 and a compatible roc-bootstrap dependency
bundle containing LLVM/Clang/LLD 22.1.8. A compatible LLVM 22 musl/libc++
bundle has been built locally and passed its static C++ LLVM/LLD, Binaryen,
zlib, and zstd probe. The full native Debug Roc build passes all 152 build
steps, and the native, generated Zig glue, and small WASM gates below pass.
A published roc-bootstrap release and ReleaseFast performance measurements
are still pending.

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
| eval | 69 | 0 |

Run these module leaves from the repository root with Zig 0.17.0:

```sh
for module in collections base parse builtins can types layout check backend lir_core lir postcheck compile eval; do
    zig build "run-test-zig-module-$module" -Doptimize=ReleaseSafe \
        -Droc-deps-path=/path/to/compatible-bundle -j2
done
```

The compile suite uses the vendored Zig LLVM IR builder and does not need
LLVM library linkage. The complete eval suite links LLVM support and
passes all 69 tests against the locally built compatible bundle, including the native C++ bridge and
LLVM/LLD linkage. Its exact command and logs are retained at
`/tmp/roc-017-full-eval/command-retry-2.txt` and
`/tmp/roc-017-full-eval/run-test-zig-module-eval-retry-2.log`. The recorded
large compile runs used the generated standalone `zig test` commands from the module graph;
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
checks for generated Zig glue, and Bytebox continuation-count
regression coverage beyond 65,535. Bytebox's public `Val` retains 16-byte
size and alignment. The echo-WASM runner compiles in Debug and its argument contract
was checked outside the checkout; executing the complete echo artifact is
still pending.

The seven Zig ABI checks now compile `test/glue/zig_abi_lock.zig` against
the actual `roc_platform_abi.zig` emitted by the newly linked Debug Roc:

```sh
roc glue src/glue/src/ZigGlue.roc /private/output test/fx/platform/main.roc
```

Generation ran in a private fixture copy. Its output passes the ReleaseSafe
compile-only lock for x86_64/aarch64 Linux musl, x86_64/aarch64 macOS,
x86_64/aarch64 Windows MSVC, and wasm32-freestanding-none. Exact generation
and seven `zig build-obj` commands, logs, and results are retained at
`/tmp/roc-017-generated-glue-gates-fikkc8xk/results.json`. These checks cover
generated Zig layout and runtime signatures; they do not claim native
execution on those other targets or the complete generated C/Rust gate.
An earlier extracted-template control rejects a renamed RocStr field
(`/tmp/roc-017-abi-review-results.json`). The complete multi-language gate
can be rerun with:

```sh
zig build run-check-glue-abi -Doptimize=ReleaseSafe \
    -Droc-deps-path=/path/to/compatible-bundle -j2
```

The final CLI classification audit adds the six new Zig OS tags to the
existing filename, platform-support, linker, and fixture-policy branches.
A compile-only probe using the current source declarations validates the
install/fixture classifiers and native host selection. The native CLI
execution checks below use the newly linked Roc binary.

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

The later 14-case cache check also passes for the independent Zig stdlib
and mutable-dependency digest stages (`2e13e65c55`, `9193f93035`), with
results retained at
`/tmp/roc-build-cache-017-complete-stages-final/results.json`. Outer
ReleaseFast, Windows GNU, and Linux GNU configurations reuse the Debug
host compiler, three bakes, and both large-input digests. A production
source edit reruns the final identity, compiler, and bakes while preserving
both cached input trees and digest runs. A mutable header edit reruns only
its dependency copy/digest, preserves the toolchain copy/digest, and
invalidates the final compiler/bakes. Restoring inputs reuses the original
results; a following unchanged run confirms all stages cached. Relocating
an identical stdlib copy preserves identity, while adding a stdlib file
changes identity and only its toolchain digest. These are build graph
correctness checks, not full native cross-target runtime measurements.

Mutable dependency bundles and relocated stdlib copies are identified by
their bytes. Verified immutable Nix dependency bundles use their complete
store path to avoid copying and hashing multi-GiB libraries. A stable store
path reuses identity; assembling identical headers and libraries under a
different store output conservatively changes the compiler identity and
cache namespace, including a metadata-only recipe change.

Deleting an installed host-tool executable restores identical bytes with
its compilation cached. Deleting an individual internal identity Run or
Options output instead fails with `FileNotFound` while its producer remains
cached. Minimal stock Zig 0.17 Run and WriteFiles/InstallFile graphs
reproduce this behavior; discarding their private local cache and rebuilding
regenerates the files. The same minimal declared Run deletion also fails
under Zig 0.16, confirming a preexisting upstream limitation. This is
partial-cache corruption recovery outside the 14-case matrix, rather than
a Roc repair. Exact probes and logs
are retained at `/tmp/roc-017-small-cache-gates-tplz59o0`.

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

## Native CLI and cache execution

The updated Debug Roc at source checkpoint `06d8403a4f` passes the following
private-fixture checks. The host and Bytebox runner used for WASM were
built with ReleaseSafe. These are correctness checks; recorded command
durations are diagnostic logs, not performance measurements.

| Gate | Result | Exact command and log records |
| --- | --- | --- |
| Five fresh cache roots: interpreter, dev run, dev build, execute | 20 commands passed | `/tmp/roc-017-debug-native-fixed-rhw433t4/results.json` |
| Native dev/size/speed build and execute, package check/test, two cached dev build/execute cycles | 12 commands passed; one Roc expect passed | same native results file |
| Existing Zig 0.16 Roc against the same cache root | Cleanup ran; both canaries and all nine current cache artifacts survived | `/tmp/roc-017-debug-native-fixed-rhw433t4/shared-016-cache-results.json` |
| Actual Zig glue generation and seven target ABI locks | 8 commands passed | `/tmp/roc-017-generated-glue-gates-fikkc8xk/results.json` |
| Small WASM dev/size/speed build and explicit-artifact Bytebox execution | 6 commands passed | `/tmp/roc-017-small-wasm-gates-fb5qfsdp/runtime-results.json` |
| Imported Roc module baseline/create/edit/delete/restore/revert with warm caches | 11 commands passed, including two expected missing-module failures | `/tmp/roc-017-imported-module-gates-3crglyjc/results.json` |

Each record includes the exact argv, working directory, cache environment,
exit status, and output log paths. Fixtures and generated artifacts were
copied into private `/tmp` projects, with separate Roc, Zig, XDG, install,
and temporary directories. The imported-module matrix also verifies the
original checkout fixture hashes remain unchanged. Native smoke commands,
run from a private copy of `test/echo`, include:

```sh
roc --opt=interpreter --no-cache platformless_app.roc
roc --opt=dev --no-cache platformless_app.roc
roc build --opt=dev --no-cache --output=/private/native-dev platformless_app.roc
/private/native-dev
roc build --opt=size --no-cache --output=/private/native-size platformless_app.roc
roc build --opt=speed --no-cache --output=/private/native-speed platformless_app.roc
roc check --no-cache platformless_app_with_package.roc
roc test --no-cache platformless_app_with_package.roc
roc build --opt=dev --output=/private/native-cached platformless_app.roc
```

Native run and emitted-binary checks require the exact `Hello, World!`
output. The imported-module matrix creates an imported `Suffix.roc`, edits
its exported suffix from `!` to `?`, and observes `Hello, World?` using the
same warm cache. Deleting the file makes both `roc check` and `roc
--opt=dev` fail instead of replaying cached success. Restoring it restores
the changed output; reverting the importer and deleting the added module
returns `Hello, World!`, including another unchanged warm run.

The first native builds exposed a migration regression in legacy cache
cleanup: the new 64-hex-character compatibility namespace was classified
as a legacy hash directory and deleted while its platform inputs were in
use. Five fresh Zig 0.17 Debug controls failed the first dev build; all
five equivalent retained Zig 0.16 ReleaseFast controls passed
(`/tmp/roc-017-debug-native-gates-mlvnv_am/fresh-baseline-matrix.json`). The
fix removes ambiguous directory-name deletion and retains explicit flat
legacy `.rcache` cleanup. Its actual background-thread filesystem
regression fails before the fix and passes afterward; all seven cleanup
tests pass in ReleaseSafe. Exact standalone module arguments and logs are
retained at `/tmp/roc-017-cache-cleanup-test-args.json` and
`/tmp/roc-017-cache-cleanup-after.log`.

Cache directories now use `compat-<ID>` while semantic compatibility IDs
and artifact key inputs stay unchanged. This prefix also protects current
namespaces from the unchanged Zig 0.16 compiler's legacy cleanup. In the
shared-root execution test, a bare 64-hex-directory negative control is
deleted, proving that old cleanup ran. The actual prefixed namespace's
scratch and artifact canaries retain their contents, and all nine current
cache artifacts retain their hashes. The two cache-directory tests also
pass in ReleaseSafe.

The small WASM checks use the emitted `host.wasm` in an isolated platform
fixture and explicitly pass each final app artifact to the runner:

```sh
roc build --target=wasm32 --opt=dev --no-cache \
    --output=/private/app-dev.wasm test/wasm/app.roc
/private/wasm_runner --wasm-path /private/app-dev.wasm \
    --expected 'Hello from Roc WASM!'
```

The same commands pass with `--opt=size` and `--opt=speed`. Host and runner
compilation commands are retained at
`/tmp/roc-017-small-wasm-gates-fb5qfsdp/helper-results.json`. This executes
the small static-library fixture, not the full echo or REPL WASM suites.

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

Remaining validation includes broader platform, full echo/REPL WASM, and
multi-language glue runtime suites, plus cold/warm compiler and application
cache measurements with the complete LLVM 22 musl/libc++ bundle.
Performance runs must use an unstripped ReleaseFast Roc compiler; no native
Roc timing improvement is claimed by the graph or Debug execution checks.

```sh
zig build roc -Doptimize=ReleaseFast -Dstrip=false \
    -Droc-deps-path=/path/to/compatible-bundle
zig build run-test-zig-module-eval -Doptimize=ReleaseSafe \
    -Droc-deps-path=/path/to/compatible-bundle
```

Related investigation: [Roc issue 11896](https://github.com/roc-lang/roc/issues/11896).
