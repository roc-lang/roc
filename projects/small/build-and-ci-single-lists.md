# One Module List, One CI Step List, One Toolchain Pin

## Problem

Three inventories in the build/CI layer are maintained as parallel
hand-written lists, and two of them have already diverged:

1. **The compiler module list is restated ~7 times in one file, plus
   once more in minici.** In `src/build/modules.zig`: the
   `ModuleType` enum (`:288-324`), the `getDependencies` switch
   (`:327-365`), the `RocModules` struct fields (`:371-406`), the
   `create()` `addModule` calls (`:429-479`), the `all_modules` array
   (`:516-552`), the `addAll` `addImport` calls (`:591-632`), the
   `getModule` switch (`:641-678`), the `test_configs` array
   (`:699-728`), and the `aggregatorFilters` map (`:26-44`). The
   switches are exhaustive (safe); the arrays are not — and
   `test_configs` already omits several modules present in the enum,
   so "modules that exist" and "modules that get a unit-test step"
   have silently diverged. `src/build/minici.zig:55-82` then
   hardcodes the per-module step names again as strings.
2. **The CI gate list exists in three orchestrators.** `build.zig`
   defines the `run-check-*`/`run-test-*` steps; `minici.zig:35-100`
   lists the leaf steps `zig build minici` runs (with a comment
   instructing humans to keep it in sync); `.github/workflows/ci_zig.yml:59-117`
   re-lists them as individual workflow steps; `src/flake.nix:82-102`
   re-lists a subset again. The two CI paths already run different
   granularities (ci_zig.yml uses aggregate `run-test-zig`; minici
   enumerates `run-test-zig-module-*`), so "what CI ran" depends on
   which orchestrator fired. A new gate must be added in up to four
   places or it silently runs in only some paths.
3. **The Zig toolchain version is pinned in ~12 places.**
   `build.zig.zon:4` (`minimum_zig_version`, the only enforced one),
   eight `setup-zig` pins across `.github/workflows/` (`ci_zig.yml:42,
   210, 311, 454, 513`, `ci_cross_compile.yml:32`,
   `ci_zig_nix.yml:33`, `ci_manager.yml:124`), `src/flake.nix:67`,
   and the bootstrap dependency URLs in `build.zig.zon:12-47`. A Zig
   bump that misses a workflow runs that job on the wrong toolchain.

## Background

The build layer also contains the counter-examples: `RocTarget`
(`src/target/mod.zig`) is deliberately importable by both `build.zig`
and the CLI, and the compiler version is assembled once into
`build_options`. The module list is the last large inventory without
that treatment. Note `ci_manager.yml` already runs `zig build minici`
as its whole gate — proof the single-entry-point form works in CI.

## Evidence

- `src/build/modules.zig` — diff the enum against `test_configs` to
  see the existing omissions.
- `minici.zig:36-38` — the keep-in-sync comment.
- `ci_zig.yml:381-385` vs `minici.zig:55-82` — the granularity
  divergence.
- `grep -rn '0.16.0' .github/workflows build.zig.zon src/flake.nix`.

## Solution design

1. **Comptime-drive modules.zig from the enum.** `getDependencies`
   and `getModule` are already exhaustive; convert `all_modules`,
   `create`, `addAll`, and `test_configs` to `inline for
   (std.enums.values(ModuleType))` loops over one comptime table
   (per-module: name, deps, has-tests flag, aggregator filter). A
   module deliberately excluded from testing carries an explicit
   `no_tests` marker in the table, so the current silent omissions
   become visible decisions. The struct-of-fields stays (Zig field
   names), guarded by a comptime assert that its field set equals the
   enum. minici derives the `run-test-zig-module-*` job names from
   the same table (exported via a generated list or a `build.zig`
   query) instead of string literals.
2. **Collapse the CI lists.** `ci_zig.yml`'s `check-once` job becomes
   a single `zig build minici` invocation (as `ci_manager.yml`
   already does), and `flake.nix` calls the same aggregate. The gate
   list then lives in exactly two places with one direction of flow:
   `build.zig` defines steps; `minici.zig` enumerates the gates; CI
   invokes minici. If per-step GitHub-UI granularity is worth
   keeping, instead generate the workflow's step list from
   `minici.zig`'s `jobs` array with a checked-in generated file and a
   CI check that it is current.
3. **One toolchain pin.** Read the version in workflows from
   `build.zig.zon` (setup-zig supports a version-file/zon read; if
   the pinned action version does not, a one-line
   `grep minimum_zig_version` shell step feeding the action input
   does). `flake.nix` and the bootstrap URLs stay manual but get a CI
   check comparing them against the zon value, so a partial bump
   fails fast.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Adding a module = one table entry; `zig build` fails at comptime if
  the struct field, dependency arm, or test decision is missing.
- Every enum module is either tested or explicitly marked `no_tests`;
  the current divergence list is triaged in that change.
- `grep -n 'run-test-zig-module' src/build/minici.zig` shows derived
  names, not literals.
- One CI path: `ci_zig.yml` and `flake.nix` invoke the minici
  aggregate (or consume a generated, checked list).
- A wrong-version workflow pin fails CI via the zon-comparison check.

## How to evaluate the result

### Correctness ideal

"What does CI run" has one answer, derivable from `minici.zig`; "what
modules exist" has one answer, the enum. A skipped gate or untested
module is impossible to create silently.

### Performance ideal

CI wall-time unchanged or better (minici already parallelizes its
jobs; verify total pipeline time on one run before/after). Build
graph construction cost of the comptime loops is negligible (same
work, generated).

## Tests to add

- The comptime field-set/enum assert in modules.zig.
- The zon-vs-workflow/flake version-comparison CI check.
- If the generated-workflow route is chosen: the freshness check.

## Related projects

- [cli-declarative-flags.md](cli-declarative-flags.md) — the same
  declarative-table move for the CLI surface.
