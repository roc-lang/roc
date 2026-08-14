# PR #10043 — Add RWX CI for the Zig compiler (Linux x64 + arm64), running in parallel with GHA

- **Author:** djpenka (Dylan Penka) · **Draft:** yes · **Base:** `main`
- **Size:** +1,852 / −86 across 12 files
- ~934 lines of new CI config (`.rwx/ci.yml` 502, `.rwx/ci-arm64.yml` 233, `.captain/config.yml` 199) plus `build.zig` +200/−40 and the test-runner work

Adds a second, parallel CI system alongside GitHub Actions for Linux x64/arm64,
with content-addressed caching driven by per-task input filters, Captain
per-test reporting, and an opt-in custom Zig test runner.

## Verdict

The PR is unusually well-presented — the author flags their own riskiest change,
offers to split it, and documents the reasoning inline at every non-obvious
decision. The `hostToolsTarget` comment explaining the
builtin_compiler → Builtin.bin → compiled_builtins cache cascade is genuinely
better than most code comments in the repo.

**Three things I'd separate from the "should we adopt RWX" question:**

1. The aarch64 CPU-model change is worse than the author's own warning suggests
   — it affects every Apple Silicon contributor, and probably costs compile
   speed via hardware SHA-256 (#1). **Should be gated.**
2. The test-runner crash-recovery fix is a real bug fix worth ~140 silently
   dropped tests. **Land it now, independently.** (#2)
3. Two `main` bugs the pipeline surfaced are currently documented only in a PR
   description. **File them.** (#3)

And one structural concern about the approach itself (#4) that the maintainers,
not I, have to weigh.

---

## Findings

### 1. (Should gate) `stableHostCpuModel(.aarch64) => .baseline` degrades every local Apple Silicon build

The author already flags this:

> ⚠️ **This is the one maintainer-facing behavior change in the PR — please flag
> if that's not acceptable**

I think it's sharper than the warning conveys. This line

```zig
default_target_query.cpu_model = stableHostCpuModel(builtin.target.cpu.arch);
```

sits in the *default* target used by `standardTargetOptions`, so a plain
`zig build` on an M-series Mac or an arm64 Linux box now compiles for aarch64
**baseline** instead of the native CPU. That means dropping, among others:

- **LSE atomics** — atomic refcount operations fall back to LL/SC retry loops.
  ARC traffic is not a rare path in this compiler.
- **The crypto extension (hardware SHA-256)** — and this compiler hashes
  constantly: cache keys (`SHA256(source)`), content-addressed generated
  identities (`TypeDigest`), evidence digests, the `MODULE_ENV_VERSION_HASH`.
  Software SHA-256 is several times slower than the `sha256h`/`sha256su` path.
- **CRC**, and whatever else `apple_m1`/`apple_m2` enables over `generic+v8a`.

So the plausible outcome is that every macOS contributor's `roc` gets slower,
for a reason that exists entirely to make one CI provider's cache keys stable.
That's exactly the trade the repo's stance on performance argues against.

The author already offers the fix and it's the right one: **put it behind a
flag** (`-Dstable-cpu`, default off, set by both CI systems), or scope it to
`os_tag == .linux`. Note the x86_64 half is a non-issue — `x86_64_v3` was
already the effective default via `getReleaseTargetQuery`, so only aarch64
actually changes.

`hostToolsTarget`'s use of `.baseline` is fine as-is and shouldn't be gated:
those are build-time-only tools whose output is CPU-independent, they're Debug
anyway, and the cascade argument in its doc comment is convincing.

### 2. (Should split and land now) The test-runner crash-recovery fix

> This also **fixes a real pre-existing bug** in the runner: it didn't survive
> Zig's crash-recovery re-spawn … which silently dropped ~140 tests from the
> reported totals whenever a test crashed.

That is a serious correctness problem in the test infrastructure, entirely
independent of RWX: today, a crashing test can cause ~140 *other* tests to
vanish from the count without anything going red. It should not be gated behind
a decision about adopting a CI vendor.

Please split `test/zig_test_runner.zig`'s crash-recovery handling into its own
PR. It'll be small, obviously correct, and reviewable on its own terms — and it
makes the RWX PR that much smaller.

Worth adding there: a test (or at least a documented manual repro) that a
deliberately-crashing test still produces the full expected count, so this can't
silently regress again.

### 3. (Should file) Two `main` bugs are living in a PR description

> the pipeline surfaced two pre-existing `main` issues (a deterministic
> `roc_alloc called` abort in `json_decoder_platform_test` on both arches, and a
> valgrind uninitialised-value hit in `CoreCtx.osCanonicalize`)

File both now, with the reproduction commands. A deterministic abort on both
architectures and a valgrind uninitialised read are the kind of finding that
justifies the whole exercise, and right now they're discoverable only by reading
a draft PR body. If this PR is ever closed, they're lost.

### 4. (Structural, for maintainers) Positive input filters trade a slow-CI failure mode for a wrong-CI failure mode

This is the part I'd want the team to decide with eyes open, because it's the
core of the design rather than a bug.

The claimed benefit is real:

> tasks declare **positive input filters**, so RWX's content-based cache skips
> whatever a change can't affect. A docs-only change cache-hits essentially the
> whole graph

But the filters are a **hand-maintained restatement of the build graph** in
YAML, and the failure mode when they're wrong is silent. If a task's filter
omits a path the task actually reads, RWX serves a cached green result for a
change that would have failed. GHA's "rebuild everything" is slower but cannot
be wrong in that direction.

The config itself shows the maintenance burden concretely:

```yaml
- legal_details # embedded into the roc binary via addAnonymousImport (build.zig:5042)
```

That comment encodes (a) a build.zig implementation detail and (b) a **line
number** that will rot on the next edit to build.zig. Nothing detects it when it
does. Similarly, `zig-build-runner-x64` filters on exactly
`build.zig`, `build.zig.zon`, `src/build`, `src/target` — correct today, and
invalidated the first time the build runner grows a dependency outside those
four paths.

The generic version of this concern: Zig's build system already knows the true
dependency graph. Re-deriving a coarse approximation of it in YAML, by hand,
without a consistency check, means every future build.zig change carries an
invisible obligation to update `.rwx/*.yml`. That obligation will be missed.

Two things that would make this much safer:

- A **periodic no-filter run** (nightly, or on `main`) that ignores the cache
  entirely. If a filtered PR run and the unfiltered run ever disagree, the
  filters are wrong and you find out in a day rather than after a bad merge.
- **Treat filter edits as build-graph edits** in review — i.e. a `build.zig`
  change that adds an input should be expected to touch `.rwx/` too, the same way
  a new `IteratorKind` is expected to touch the cache version.

None of this argues against the PR; it argues for the soak period being used to
measure *correctness* parity (do the two systems ever disagree?) and not just
speed.

### 5. (Structural) Two CI definitions covering the same matrix will drift

`.github/workflows/ci_zig.yml` and `.rwx/ci.yml` + `.rwx/ci-arm64.yml` now both
describe Linux x64/arm64 coverage, and nothing keeps them in sync. Concretely:
PR #10645 adds a `Run dedicated REPL WebAssembly tests` step to the GHA
workflow. Whoever merges that has no signal that `.rwx/ci.yml` needs the same
task.

Worth stating the plan explicitly in the PR body: is the intent to converge on
one system after the soak (and if so, on what timeline and what evidence), or to
run both indefinitely? "Indefinitely" is a legitimate answer, but it should be a
chosen answer, since it doubles the cost of every future CI change.

### 6. (For maintainers, not code) Org-level decisions the PR requires

The PR is upfront about needing an org-wide GitHub App install and an RWX org.
The security posture described is sensible and I'd note it approvingly:

- no secrets in PR CI at all,
- the one vault is write-gated to `main` and read-only for fork PRs — so forks
  get cache hits but can't poison the cache,
- fork PRs behind a manual-start gate,
- public run visibility so contributors don't need accounts.

That's a better default than most third-party CI integrations ship with. The
remaining question is purely organizational (who administers the RWX org, what
happens if the author moves on), and belongs in the PR discussion rather than a
code review.

## Smaller notes

- **The opt-in defaults are right.** `-Droc-test-runner`, `-Droc-test-rwx-out`,
  `-Droc-test-files`, `-Droc-test-nonmodule-only`, `-Droc-test-modules-only` all
  default to off/empty, so a plain local `zig build test` is byte-identical to
  today. That's the correct way to add CI-only machinery, and the inline comments
  explaining each option's role in Captain partitioning are good.
- **`rocTestFileSelected`'s empty-CSV-means-all** convention is the right
  default (a missing partition spec runs everything rather than nothing), and
  it's shared between the module loop and the standalone tests so counts can't
  diverge. Worth a unit test though — an off-by-one in partition selection
  presents as "tests silently didn't run," the same class of failure as #2.
- **`test_runner_exe` install** changed from `b.installArtifact` to an explicit
  `addInstallArtifact` + named step so CI can build it alone. Fine, and the
  comment explains why (the valgrind anchor needs a subset).
- **`getReleaseTargetQuery`'s comment lost detail.** It used to say *why*
  x86_64_v3 ("modern CPU features (AVX2, BMI2, etc.)"); now it says only "pinned
  via stableHostCpuModel for cross-agent cache stability." The AVX-512/Valgrind
  reason survives in `stableHostCpuModel`'s comment, so nothing is lost overall —
  just noting the reader of `getReleaseTargetQuery` now has to follow one hop.
- **`.captain/.gitignore`** (+3) — worth confirming it ignores the run outputs
  (`tmp/captain/*.json`) and not the config, since `.rwx/ci.yml` explicitly
  depends on `.captain` being present as an input.

## Suggested sequencing

Given the above, the cleanest path looks like:

1. **Now, independently:** the `zig_test_runner.zig` crash-recovery fix (#2).
2. **Now, independently:** file the two `main` bugs (#3).
3. **In this PR:** gate the aarch64 CPU-model change behind a flag (#1).
4. **Then:** merge RWX non-required as proposed, with a nightly unfiltered run
   (#4) so the soak measures correctness parity and not just wall-clock.

That gets the two unambiguously-good pieces landed immediately and leaves the
vendor question to be decided on evidence.
