# Surface ARC Certifier Skips

## Problem

The debug borrow certifier can silently decline to verify a procedure, and
nothing tells anyone it happened.

Since PR roc-lang/roc#9746, a procedure whose join-entry-state count exceeds
the certifier's capacity budget (currently 4096 distinct entry-state
summaries per join, checked in the `.jump` handler in
`src/lir/arc_certify.zig`) makes `certifyProc` return
`error.CertifierCapacityExceeded`. `certifyStore` catches that error per
procedure, increments `Diagnostic.skipped_proc_count`, and continues with
the rest of the store. That was the right call for build viability — the
cap is a capacity limit, not a finding, and it must never abort a valid
build (issue roc-lang/roc#9658 was a valid program that tripped the old cap
of 64) — but the result is that the procedure is left **unverified** and
the count is exposed only as a struct field that no caller reads.
`certifyStoreOrPanic`, the production entry point invoked from
`arc.insert` in debug builds, swallows the error variant defensively and
prints nothing.

This matters because the procedures most likely to exceed the cap — many
refcounted mutable locals rebound in nested loops — are exactly the
procedures where the hand-maintained RC inserter (`src/lir/arc.zig`) is
most likely to have a bookkeeping bug. A real refcount leak in such a
procedure would be skipped rather than reported, and nobody would know the
net had a hole until a user hit the leak at runtime.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck IRs
(Monotype → Lifted → Lambda Solved → Lambda Mono) → LIR lowering → ARC
insertion → backends (LLVM / dev x86+aarch64 / wasm / interpreter).

ARC insertion (`src/lir/arc.zig`) emits explicit `incref` / `decref` /
`free` statements from a whole-program borrows-with-lifetimes solution
computed in `src/lir/arc_solve.zig`. Because the inserter's ownership
bookkeeping is hand-maintained per LIR instruction kind, debug builds run
an independent certifier, `src/lir/arc_certify.zig`, which re-checks every
emitted procedure against the ownership rules (every unit released or
transferred exactly once on every path, no use after death, no release of
a borrow). `design.md` at the repo root, sections "ARC Borrow Inference"
and "Debug Borrow Certifier", is the authoritative design reference. The
certifier is compiled away entirely in release builds.

Borrow inference deliberately keeps refcounts at 1 so the runtime list/str
builtins in `src/builtins/` can take `refcount == 1` in-place fast paths —
so a missed leak or double-release is not cosmetic; it changes program
behavior. The certifier is the only automated check standing between an
inserter bug and shipped memory unsafety, which is why a silent gap in its
coverage is worth closing loudly even before the gap itself is removed.

## Evidence

Verified against the current tree:

- `src/lir/arc_certify.zig`:
  - `CertifyError = error{ OutOfMemory, Certification,
    CertifierCapacityExceeded }`, with a doc comment stating the capacity
    variant "must never abort a valid build".
  - The cap: `if (record.scheduled.count() > 4096) return
    error.CertifierCapacityExceeded;` in the `.jump` case of the
    certifier walk.
  - `certifyStore`: catches the error per procedure and runs
    `diag.skipped_proc_count += 1; continue;`.
  - `Diagnostic.skipped_proc_count: usize = 0` — doc comment says
    "Exposed for tests/debugging"; no caller reads it.
  - `certifyStoreOrPanic`: `error.CertifierCapacityExceeded => {}` (a
    defensive no-op; the per-proc catch means it cannot reach here).
  - `LirStore.procDebugName(proc_id)` exists (used by
    `writeFailureContext`) and can name a skipped procedure.
- `src/lir/arc.zig`: `insert` calls `arc_certify.certifyStoreOrPanic`
  under `builtin.mode == .Debug`.
- Existing stats/verbose infrastructure: `BuildStats` in
  `src/compile/compile_build.zig` (`getBuildStats`), rendered by
  `printBuildSuccess` in `src/cli/main.zig` when `args.verbose` is set.
  Issues roc-lang/roc#9787 and roc-lang/roc#9789 discuss a debug stats
  compiler; verify what exists when implementing and wire into it rather
  than inventing a parallel channel.

## Solution design

Three pieces, all small; this is a stopgap that makes the hole visible.
[ARC Certifier Lattice Join](../big/arc-certifier-lattice-join.md) removes
the hole, at which point this surfacing (and the counter) is deleted.

1. **Warn on skip in debug builds.** In `certifyStore`'s
   `error.CertifierCapacityExceeded` catch, record the skipped
   `LirProcSpecId`; after the loop (or in `certifyStoreOrPanic`), if
   `diag.skipped_proc_count > 0`, emit a warning via `std.log.warn`
   naming each skipped procedure — id plus `store.procDebugName(...)`
   when available — or a summary count if the list is long (cap the
   listing at, say, 10 names plus "and N more"). The warning text should
   state plainly what it means: "ARC certifier skipped N procedure(s)
   whose join-state enumeration exceeded capacity; their RC schedules are
   UNVERIFIED."
2. **Surface the count in stats/verbose output.** Extend the diagnostic
   plumbing so the skip count reaches the compiler's existing stats
   surface: verify the current state of the debug stats work
   (issues 9787/9789) and add `arc_certifier_skipped_procs` to whatever
   structure feeds verbose output (`BuildStats` /
   `printBuildSuccess` today). If no per-stage stats channel exists yet,
   the `std.log.warn` from item 1 is the minimum viable surface; do not
   build new infrastructure for this stopgap.
3. **CI zero-skip check.** Add an assertion mode so CI fails the day a
   corpus procedure starts being skipped, rather than letting unverified
   procedures silently accumulate: a build option or environment variable
   (e.g. `ROC_CERTIFIER_FORBID_SKIPS`) checked after `certifyStore`
   returns — when set and `skipped_proc_count > 0`, panic with the same
   listing as item 1. Enable it for the debug test jobs in CI so the
   repo's own test corpus (unit tests, snapshot tests, eval tests — all
   of which run `arc.insert` in debug builds) certifies with zero skips.

What gets deleted later: all three pieces, together with
`skipped_proc_count` and `error.CertifierCapacityExceeded`, when the big
project's dataflow fixpoint removes the skip path entirely.

## What success looks like

- A certifier skip is impossible to miss: it prints a warning naming the
  procedure(s) in any debug build, and it fails CI on the repo's corpus.
- The count appears in verbose/stats output alongside existing build
  stats, so a developer investigating RC behavior can see at a glance
  whether certification was exhaustive.
- No behavior change whatsoever when zero procedures are skipped, and no
  change of any kind in release builds.

## How to evaluate the result

### Correctness ideal

The warning fires exactly when certification was skipped — no false
positives, no misses. Unit-test the plumbing with a synthetic exceedance:
either lower the cap in a test build (make the cap a comptime-known
constant that tests can override, or inject it through the `Certifier`
struct) or construct a Bell-heavy procedure with the `ArcTest` harness in
`src/lir/arc.zig` that legitimately exceeds the real cap. Assert both the
counter value and the warning text, including the procedure name. Assert
the forbid-skips mode panics on the same input and is silent on a normal
corpus.

### Performance ideal

Trivially zero cost when no skips occur: one integer compare after
certification. The certifier is debug-only, so release compile time and
generated code are untouched by construction. The warning path allocates
only when it fires.

## Tests to add

- Synthetic exceedance test (lowered cap or Bell-heavy generated proc)
  asserting `skipped_proc_count`, the warning text, and the procedure
  name appearing in it.
- A test that the forbid-skips mode turns the same situation into a
  hard failure, and that it passes on a small normal store.
- CI: enable the zero-skip assertion on the existing debug test corpus
  (no new test content — the corpus itself is the fixture).

## Related projects

- [ARC Certifier Lattice Join and Centralized Ownership-Transfer Keying](../big/arc-certifier-lattice-join.md)
  — replaces join-state enumeration with a dataflow fixpoint, removing
  the cap and the skip path; this project's surfacing is deleted with it.
