# Fuzzing Roc

Roc uses [zig-afl-kit](https://github.com/bhansconnect/zig-afl-kit) for fuzzing, which provides integration with [AFL++](https://aflplus.plus) for coverage-guided fuzzing.

## Prerequisites

Install AFL++ on your system:

```bash
# Ubuntu/Debian
sudo apt install afl++

# macOS
brew install afl++

# Verify installation
afl-fuzz --version
```

## Building Fuzz Targets

Build the fuzz executables with:

```bash
zig build -Dfuzz
```

This builds:
- `zig-out/bin/fuzz-*` - Fuzz targets for AFL++
- `zig-out/bin/repro-*` - Reproduction executables for debugging crashes

Available fuzz targets:
- `fuzz-parse` - Parser and formatter stability testing
- `fuzz-tokenize` - Tokenizer testing
- `fuzz-canonicalize` - Canonicalization testing

## Generating a Corpus

Generate a seed corpus from the snapshot tests:

```bash
mkdir -p /tmp/corpus
zig build snapshot -- --fuzz-corpus /tmp/corpus
```

This extracts source code from all snapshot tests. For REPL snapshots, it strips the `»` delimiters and creates separate corpus files for each expression.

## Running the Fuzzer

Basic single-threaded fuzzing:

```bash
afl-fuzz -i /tmp/corpus -o /tmp/parse-out zig-out/bin/fuzz-parse
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
zig build snapshot -- --fuzz-corpus ~/roc-fuzz-corpus

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
