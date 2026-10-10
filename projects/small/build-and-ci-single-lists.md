# One CI Gate List Outside Pull Requests

## Problem

Pull-request CI has one gate list: `src/build/minici.zig`'s `jobs`
array, which `.github/workflows/ci_manager.yml` runs shard by shard and
`--minici-verify-workflow` checks against the workflow. The module
unit-test jobs in it are derived from the module inventory in
`src/build/modules.zig` (`ModuleType.info`), so a new module's tests
reach MiniCI without a second edit.

Two other orchestrators still name MiniCI jobs by hand:

1. **The nightly `zig-tests` job** in `.github/workflows/ci_zig.yml`
   runs on operating systems pull-request CI never uses. On those legs
   (`pr_os` unset) it re-lists four MiniCI jobs as individual workflow
   steps (`run-test-cli`, `run-test-zig-machine-code-shim`,
   `run-test-eval-host-effects`, `run-test-wasm-static-lib`) and then
   runs the `run-test-zig` aggregate. A job added to MiniCI runs on
   those operating systems only if someone also adds a step there, so
   "what the nightly ran on those legs" and "what a PR ran" are two
   lists with nothing comparing them.
2. **The Nix leg** (`ci/zig_nix_ci.sh`, called by
   `.github/workflows/ci_zig_nix.yml`) names `run-check-snapshots` and
   `run-test-zig` itself.

One module's unit tests are deliberately outside MiniCI: `glue` is
marked `.minici = false` in the module inventory, so only the nightly
`run-test-zig` aggregate runs `run-test-zig-module-glue`. Whether that
should stay nightly-only is an open decision; the marker makes it a
visible one.

## Background

`zig build minici` already takes `--minici-shard`, `--minici-from`/
`--minici-to`, and `--minici-skip-build`, and each job carries a
`Placement` saying which hosts its result can differ on. The nightly
legs want exactly "every `every_host` job", which is what a secondary
`Lane` selects.

The Zig toolchain version is pinned in `build.zig.zon`
(`minimum_zig_version`), in each workflow's `setup-zig` step, and in
`src/flake.nix`. `run-check-tidy` compares the workflow and flake pins
against the zon value, so a bump that misses one fails the `source`
lane.

## Solution design

1. Give the nightly's non-PR legs one invocation: `zig build minici`
   restricted to the secondary lane (a shard set for "any other host",
   or a `--minici-lane secondary` selector), replacing the four named
   steps. The steps that are nightly-only by design (ReleaseFast
   `run-test-zig`, `run-check-snapshots -- --debug`, the Lambda Mono
   differential sweep, static-link checks, kcov) stay, because MiniCI
   does not run them.
2. Have `ci/zig_nix_ci.sh` call the same selector instead of naming
   steps, or record in `minici.zig` why the Nix sandbox needs a
   different set.
3. Decide `glue`: either flip `.minici` to true (and rebalance the
   shards from measured timings) or state the reason beside the marker.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -n 'zig build run-test' .github/workflows/ci_zig.yml ci/zig_nix_ci.sh`
  shows only steps MiniCI does not run.
- Adding a job to `minici.zig`'s `jobs` makes it run on every nightly
  leg with no workflow edit.
- The `glue` marker either is gone or carries its reason.

## How to evaluate the result

### Correctness ideal

"What does CI run" has one answer on every host, derivable from
`minici.zig`. A gate that exists on PR hosts but is skipped on
nightly-only hosts is impossible to create silently.

### Performance ideal

Nightly wall-time unchanged or better: MiniCI already parallelizes its
jobs and reuses one `build-ci`. Verify total pipeline time on one
nightly run before and after.

## Tests to add

- A `minici.zig` test that the nightly selector covers every
  `every_host` job exactly once, beside the existing "MiniCI shards
  cover" tests.
- `--minici-verify-workflow` extended to `ci_zig.yml`, so the nightly
  workflow is checked against the selector the way `ci_manager.yml` is
  checked against the shards.
