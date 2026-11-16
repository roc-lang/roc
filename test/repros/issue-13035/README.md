# Issue 13035: inhabited `Try` error row slows an optimized append loop

Issue: <https://github.com/roc-lang/roc/issues/13035> (body read; no comments).
Inherited baseline supplied by the task: omnibus PR #13086,
`ae9f9dbdb7274edf247dc225824d5007309fd859`. No reset or Git diagnostic commands
were used.

`inhabited.roc` preserves the issue's minimal workload, with `0.U64` on the sum
initializer because this baseline rejects the original unsuffixed sum as an
unresolved numeric type. The control is generated
by changing **only** the two `Try(List(U8), [E])` signatures to
`Try(List(U8), [])`. Both must print `1000 * loops + floor(loops / 2)`.
The empty-row control's unreachable `Err` warning is expected.

## Run

From the repository root:

```sh
zig build -j2 roc
python3 test/repros/issue-13035/benchmark.py
```

The runner builds both programs with `--opt=speed`, warms each once, alternates
measurement order, checks every output, and prints all samples plus medians.
Use `--roc PATH`, `--loops N`, or `--samples N` to select the compiler and
measurement duration. Output binaries and generated sources remain in
`zig-out/repros/issue-13035/`.

On Linux with permitted hardware counters, inspect the original reported metric
using the same generated binaries:

```sh
perf stat -e instructions,cycles zig-out/repros/issue-13035/empty.bin 100000
perf stat -e instructions,cycles zig-out/repros/issue-13035/inhabited.bin 100000
```

## Oracle and scope

Correct performance is comparable optimized execution for these two always-`Ok`
programs, not the issue's roughly 2x instruction/time penalty for `[E]`.
This is a runnable benchmark, **not a deterministic failing regression test**:
its exit status asserts output correctness only, and its timings require human
interpretation on an otherwise quiet machine. No arbitrary wall-clock threshold
is installed in CI.

Allocation counts alone do not faithfully capture the minimal report: the stated
extra work is repeated capacity, seamless-slice, and reference-count *checks*,
which need not allocate. A pre-LLVM structural check would not establish the
reported final optimized-loop behavior, and pinning particular assembly opcodes
would make the oracle architecture-dependent. The original report measured Linux
x86_64; this reproduction environment is macOS arm64. No claim about the larger
codec-shaped example is made.

## Local validation

On macOS arm64, using Zig 0.17.1 and the compiler built from the supplied
baseline:

- `zig build -j2 roc`: passed.
- `python3 test/repros/issue-13035/benchmark.py`: passed all output assertions,
  using 1,000,000 outer loops, one warmup per binary, and nine measured samples.
  Every run printed `1000500000`.
- Empty error row: median **0.467983 s**.
- Inhabited error row: median **1.326460 s**, **2.834x** the empty-row median.

Raw seconds, in each variant's measurement order:

```text
empty:     0.442070 0.544822 0.467983 0.421190 0.562918 0.410637 0.505116 0.587342 0.460535
inhabited: 1.158220 1.326460 1.304153 1.174187 1.327638 1.256789 1.748412 1.424225 1.755773
```

This reproduces a substantial slowdown on this machine, not a deterministic
performance-test failure. The shared machine introduces timing noise; no hardware
instruction counters were available, and no Linux x86_64 run was performed.
The issue's precise 2x ratio is not asserted.

The initial `zig build -j2 build-roc` attempt failed because that step does not
exist; `zig build -j2 -l` identified `roc`. An initial source run also exposed
the unresolved sum type described above. Roc returns status 2 for the expected
empty-row warning; the runner permits that status only for the control and
requires a freshly produced binary. Neither setup failure is claimed as the
reported performance bug. No compiler fixes or fix analysis were performed.
