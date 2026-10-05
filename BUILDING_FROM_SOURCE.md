# Building the new Roc compiler from source

If you run into any problems getting Roc built from source, please ask for help in the `#beginners` channel on [Roc Zulip](https://roc.zulipchat.com) (the fastest way), or create an issue in this repo!

## Recommended way

[Download Zig 0.17.0](https://ziglang.org/download/) and add it to your PATH.
[Search "Setting up PATH"](https://ziglang.org/learn/getting-started/) for more details.

Do a test run with
```
zig build roc
./zig-out/bin/roc version
```

## Using Nix

If you're familiar with nix and like using it, you can build the compiler like this:
```
nix develop ./src
buildcmd
./zig-out/bin/roc version
```

## Local dependency bundles and caching

A compatible roc-bootstrap bundle contains `include/` and `lib/` for the
compiler's target, including LLVM 22, LLD, and Binaryen. Build against a local
bundle with:

```sh
zig build roc -Droc-deps-path=/path/to/bundle
```

This option uses the same LLVM/LLD/Binaryen configuration as a downloaded
bundle. It cannot be combined with `-Dllvm-path` or `-Dsystem-llvm`, which select
legacy LLVM dependency modes. Bundle headers and libraries at mutable paths
participate by content in the cache identity; immutable Nix store paths identify
their contents.

The displayed Git version is separate from application-cache compatibility.
Compatibility tracks compiler/runtime/vendor sources, the build recipe and
pinned dependency manifest, the exact Zig executable and library tree, semantic
options, and each compiler executable's actual mode, target, and CPU features.
Dirty production edits invalidate cached applications without changing `HEAD`.
Ordinary compiler builds preserve other compiler builds' application caches.
Generated compiler embedding assets live in Zig's cache.
The complete Zig library and mutable dependency contents have independent
cached digest stages. Production edits reuse those unchanged large input trees;
the final compatibility identity includes their declared content digests.

Integration tests prepare their fixture trees and generated host libraries in
Zig's cache, then run in private temporary copies. Concurrent build modes and
cache directories therefore do not overwrite the checkout's hosts or each
other's generated test executables. Test runners use absolute paths to their
compiler and prebuilt applications. `build-test-hosts` only builds the cached
hosts. For manual commands against checkout fixtures, publish them explicitly:

```sh
zig build update-test-fixtures
```

Dedicated audited `src/*/test/` directories are excluded from the production
source identity. Inline tests, test helpers outside those directories, vendored
tests, and Zig library tests remain conservatively included: editing them may
invalidate application caches. Changes to `build.zig` also invalidate identity,
even when an edit only affects a build comment or a test leaf.

Runtime filters preserve the compiled test binary and select matching tests:

```sh
zig build run-test-zig-minici -- --test-filter "parseMiniArgs"
```

Use repeatable `-Dtest-filter="pattern"` options when the filter should also
limit test compilation. The build-input identity regression check uses a small
Debug test binary and checks production/test-only edits at unchanged `HEAD`:

```sh
python3 ci/test_build_identity.py /path/to/zig
python3 ci/test_fixture_isolation.py /path/to/zig
python3 ci/test_build_cache.py /path/to/zig
```

The last check builds the Debug builtin compiler in a private source snapshot.
It compares three independently executed bakes, verifies reuse after version,
documentation and dedicated test changes, and verifies invalidation after a
production edit while reusing the Zig library and dependency copies and digests.
A controlled dependency header edit rebuilds only its input stage and invalidates
the compiler and bakes; restoration reuses the original results. The check also
changes the surrounding build mode and target, including a native ABI, while
preserving the Debug host bake graph. Pass `--work-dir /new/path` to preserve its
graph logs and artifact identities for review.

See [the Zig 0.17 migration record](docs/zig-0.17-upgrade.md) for validated
source changes, workaround decisions and remaining platform checks.

## CPU requirements

Builds target the baseline instruction set for their architecture, so the `roc`
binary you build runs on any CPU of that architecture: baseline x86-64 (2003) or
armv8.0-a. You do not need to pick a build for your specific CPU.

To trade that portability for speed on a machine you know, pass `-Dcpu`:

```
zig build build-release -Dcpu=x86_64_v3   # AVX2, BMI2, FMA: Haswell (2013) and later, any AMD Zen
zig build build-release -Dcpu=native      # this exact machine; the result may not run elsewhere
```

`zig build -Dcpu=...` works the same way for non-release builds. `zig build --help`
lists the CPU names Zig accepts.

A binary built for a CPU level above the one it runs on dies with `Illegal
instruction` (SIGILL) at startup, before printing anything.

This is the CPU level of the `roc` binary itself. The CPU level that compiled
Roc *programs* target is a separate setting with its own floor.

## Windows Notes

Due to a [Zig bug](https://github.com/ziglang/zig/issues/17652) related to extracting dependencies from tarball files containing symlinks (which is not allowed by default on Windows), you might encounter permission denial issues. The workaround is to enable the `Developer Mode` option on Windows, which could be found under `Settings > System > Advanced`. If that does not work, please review the aforementioned bug for any additional clues.
