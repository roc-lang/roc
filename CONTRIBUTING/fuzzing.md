# Fuzzing Roc

Roc uses [zig-afl-kit](https://github.com/bhansconnect/zig-afl-kit) for fuzzing, which provides integration with [AFL++](https://aflplus.plus) for coverage-guided fuzzing.

## Prerequisites

### Ubuntu/Debian

Linux fuzz builds pass Zig's LLVM bitcode to `afl-cc`. Its Clang must be at least
as new as Zig's LLVM version: the project's Zig 0.17.0 uses LLVM 22. Older
distribution packages of AFL++ cannot read this bitcode, so build AFL++ against
LLVM 22 using [apt.llvm.org](https://apt.llvm.org/):

```bash
sudo apt update
sudo apt install build-essential git wget gnupg lsb-release software-properties-common

# Install Clang/LLVM 22 and the development files needed to build AFL++.
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 22
sudo apt install llvm-22-dev
```

In a directory outside your Roc checkout, build and install AFL++ following its
[source build instructions](https://github.com/AFLplusplus/AFLplusplus/blob/stable/docs/INSTALL.md):

```bash
git clone --depth 1 --branch stable https://github.com/AFLplusplus/AFLplusplus.git
(
    cd AFLplusplus
    make source-only LLVM_CONFIG=llvm-config-22 NO_PYTHON=1 NO_NYX=1
    sudo make install LLVM_CONFIG=llvm-config-22 NO_PYTHON=1 NO_NYX=1
)
```

The build summary must include `[+] LLVM mode successfully built`. Python and
Nyx support are disabled because Roc's source fuzz targets do not need them.
Keep `LLVM_CONFIG=llvm-config-22` on the install command too, since installation
can rebuild the LLVM instrumentation.

Check which compiler will consume Zig's bitcode:

```bash
zig cc --version
afl-cc --version
```

The Clang version reported by `afl-cc` must be 22 or newer for Zig 0.17.0. If it
reports an older version, use `command -v afl-cc` to check that your `PATH` selects
the new installation (by default `/usr/local/bin/afl-cc`). Recheck these versions
when upgrading Zig.

### macOS

```bash
brew install afl++
```

## Building Fuzz Targets

From your Roc checkout, build the fuzz executables with:

```bash
zig build -j2 -Dfuzz
```

This builds:
- `zig-out/bin/fuzz-*` - Fuzz targets for AFL++
- `zig-out/bin/repro-*` - Reproduction executables for debugging crashes

Available fuzz targets:
- `fuzz-parse` - Parser and formatter stability and comment preservation testing
- `fuzz-format` - Formatter comment preservation and stability testing on generated programs with comments between their tokens
- `fuzz-tokenize` - Tokenizer testing
- `fuzz-canonicalize` - Canonicalization testing
- `fuzz-typecheck` - Type checking generated well-typed Roc programs

## Generating a Corpus

Generate a seed corpus from the snapshot tests:

```bash
mkdir -p /tmp/corpus
zig build -j2 run-snapshot-tool -- --fuzz-corpus /tmp/corpus
```

This extracts source code from all snapshot tests. For REPL snapshots, it strips the `»` delimiters and creates separate corpus files for each expression.

## Running the Fuzzer

Basic single-threaded fuzzing:

```bash
afl-fuzz -i /tmp/corpus -o /tmp/parse-out zig-out/bin/fuzz-parse
```

The typecheck and format fuzzers generate Roc source from arbitrary input
bytes, so they can start from a tiny byte corpus:

```bash
mkdir -p /tmp/typecheck-corpus
printf '\0' > /tmp/typecheck-corpus/seed
afl-fuzz -i /tmp/typecheck-corpus -o /tmp/typecheck-out zig-out/bin/fuzz-typecheck
afl-fuzz -i /tmp/typecheck-corpus -o /tmp/format-out zig-out/bin/fuzz-format
```

You may need to set environment variables to skip certain checks:

```bash
AFL_SKIP_CPUFREQ=1 AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 \
  afl-fuzz -i /tmp/corpus -o /tmp/parse-out zig-out/bin/fuzz-parse
```

The fuzzer will report:
- **edges_found** - Number of code paths discovered
- **stability** - Should be close to 100%
- **saved_crashes** - Crashes found (these are bugs!)

Crashes are saved to `/tmp/parse-out/default/crashes/`.

## Reproducing Crashes

Use the repro executable to debug a crash:

```bash
./zig-out/bin/repro-parse /tmp/parse-out/default/crashes/<crash-file>
```

Use the matching repro executable for each fuzz target, for example:

```bash
./zig-out/bin/repro-typecheck /tmp/typecheck-out/default/crashes/<crash-file>
```

The repro executables include extra debug output and full stack traces.

## Minimizing Test Cases

Create a minimal reproduction case:

```bash
afl-tmin -i /tmp/parse-out/default/crashes/<crash-file> \
         -o /tmp/minimized.roc \
         zig-out/bin/fuzz-parse
```

## Adding Crash Cases to Tests

If you find a crash that reveals a bug:

1. Minimize the test case with `afl-tmin`
2. Fix the bug
3. Add a snapshot test covering the case (especially if complex or interesting)
4. Submit a PR

## Advanced Usage

### Multiple Cores

Run multiple fuzzer instances for better performance. See the [AFL++ documentation on using multiple cores](https://aflplus.plus/docs/fuzzing_in_depth/#c-using-multiple-cores).

### Corpus Management

Keep your corpus between sessions for incremental improvement:

```bash
# First run - generate initial corpus
zig build -j2 run-snapshot-tool -- --fuzz-corpus ~/roc-fuzz-corpus

# Subsequent runs - AFL++ will add new interesting inputs
afl-fuzz -i ~/roc-fuzz-corpus -o /tmp/parse-out zig-out/bin/fuzz-parse
```

### Performance Tips

For better fuzzing performance, you may want to modify `std.mem.backend_can_use_eql_bytes` to return `false` in your Zig stdlib. This allows AFL++ to observe character-by-character string comparisons.

Find the file with:
```bash
zig env  # Look for std_dir, then edit $std_dir/mem.zig
```

**Remember to revert this change when done!**

## Troubleshooting

### Low edge count or 0% stability

If you see very few edges (< 100) or 0% stability, the coverage instrumentation may not be working. Ensure you built with `-Dfuzz` and that `afl-cc` is available on your PATH.

### "Hmm, your system is configured to send core dump notifications..."

Set `AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1` or configure core dumps:

```bash
echo core | sudo tee /proc/sys/kernel/core_pattern
```

### CPU scaling governor warnings

Set `AFL_SKIP_CPUFREQ=1` or configure the CPU governor:

```bash
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
```

## Resources

- [AFL++ Documentation](https://aflplus.plus/docs/)
- [AFL++ Fuzzing in Depth](https://aflplus.plus/docs/fuzzing_in_depth/)
- [zig-afl-kit](https://github.com/bhansconnect/zig-afl-kit)
