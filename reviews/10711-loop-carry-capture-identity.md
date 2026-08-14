# PR #10711 — Give loop-carry parameters their binder's capture identity

- **Author:** lukewilliamboswell · **Draft:** yes · **Base:** `main` · **Head:** `fix-10698`
- **Size:** +30 / −1 across 3 files
- **Fixes:** #10698

## What it does

`prepareLoopCarries` (`src/postcheck/monotype/lower.zig:44634`) mints one fresh
parameter local per reassigned `var` when a `while`/`for`/`loop` is lowered to a
Monotype `loop_`, then rebinds the checked binder to that parameter for the loop
body (`lower.zig:43948`, `44013`). It created that local with `binder = null`.

The diff passes the binder instead and binds the debug name, so the parameter
inherits the binder's current `CaptureId`.

## Verdict

**Correct, minimal, and consistent with the surrounding convention. Recommend
merge** once the questions in "Open questions" are answered (neither is a
blocker in my reading).

## Correctness analysis

I traced the identity plumbing end to end:

- `BodyDraftStore.addLocal` (`lower.zig:11133-11160`) assigns
  `capture_id = inherited orelse generatedLift(index)` **whenever `binder != null`**,
  and `null` otherwise. So passing the binder is sufficient to guarantee the
  parameter has *some* `CaptureId` — there is no residual path where the fix
  leaves it null. That closes the invariant at `lift.zig:2143`
  (`"pre-lift explicit capture local had no CaptureId"`) by construction, not by
  luck.
- `addLocalWithBinderCell` (`lower.zig:13743-13756`) reads
  `self.binders.get(binder)` to find the *current* binding and inherits its
  `capture_id`. At the point `prepareLoopCarries` runs, that lookup returns
  `initial` — the exact local it just read on the line above (`44638`). So the
  parameter deliberately shares identity with the pre-loop version, which is
  what "a new version of the same binding" is supposed to mean per the doc
  comment on `addFreshLocalWithBinder` (`lower.zig:13758-13761`).
- The fix now also sets `checked_capture_id = CaptureId.fromBinder(binder)`
  (`lower.zig:11141`), which is what `addFnDefEdge` prefers for `declared_id`
  (`lift.zig:2145`). That is the part that actually makes pre-lift capture edges
  line up with the checked declared captures, and it's why the release-mode
  corruption described in the PR body goes away rather than just the debug
  panic. Worth saying explicitly in the PR body — the current text emphasizes
  `capture_id`, but `checked_capture_id` is the load-bearing half.

**The "one outlier" claim checks out.** Grepping `param_local` shows the only
construction site is line 44643, and the two sibling version-minting sites
(`finalCarryPattern` at `44650-44668`, and the branch-merge locals) already pass
their binder plus `bindLocalName`. The new code is a literal copy of that shape,
including the ordering (`addLocalWithBinderCell` then `bindLocalName`). No
divergence.

**No duplicate-binder hazard.** `prepareLoopCarries` iterates the list built by
`collectReassignedBindersInStatement`, which funnels through
`appendUniqueBinder` (`lower.zig:44774`), so two carries can never name the same
binder and mint two parameters that fight over one identity.

**Nested loops behave.** An inner loop's `prepareLoopCarries` runs while the
binder maps to the *outer* loop's parameter, so the inner parameter inherits the
same id transitively. All versions of one `var` in one function converge on one
`CaptureId`, which is the intended invariant.

**Rules compliance.** No fallback, no heuristic, no probe-then-mutate: this is a
data-propagation fix (pass the provenance you already have) rather than a
recovery path, so it's on the right side of AGENTS.md. The added comment states
*why* the binder is required rather than restating *what* the call does, which
matches `.rules`.

## Findings

### 1. (Low) `localProcCaptureBindings` dedupe now silently picks a version

`localProcCaptureBindings` (`lower.zig:26811-26835`) deduplicates a local
procedure's captured context entries by `CaptureId`, first-entry-wins. Before
this change, a loop parameter reaching that walk would hit the
`"local procedure context capture local had no capture identity"` invariant at
line 26827 — loud. After the change it has an id, so if a local proc's context
happens to list *both* the pre-loop local and the loop-parameter local for the
same `var`, they now collide and the **first** entry wins silently.

If `context.entries` is not guaranteed to be in binding order, that could select
the stale pre-loop value. I could not convince myself either way from the code
alone; the entries come from `validateLocalProcContext` and I did not chase
their construction order. This is the same exposure the existing
`finalCarryPattern` locals already have, so it is not *introduced* here — but
this PR widens the set of programs that reach it, and it's the one place where
the change converts a panic into a silent choice.

Suggested: either confirm entries are emitted in binding order (and say so in a
comment at 26807, where the "first entry wins" contract is documented but not
justified), or add a debug assertion that colliding entries agree on the local's
`ty` and dominance.

### 2. (Nit) Test placement — CLI runner vs. eval suite

The regression is wired into the parallel CLI runner's `subcommands` suite,
which spawns a full `roc test --no-cache` per case. The bug lives in
postcheck/monotype lowering + lift, which `zig build run-test-eval` also
exercises at a fraction of the cost. If eval-tests reach the lift pass (they
should — they evaluate lowered programs), an eval test would give the same
coverage much cheaper. Not worth blocking on; the CLI test is genuinely
end-to-end and the repro is the issue's verbatim reproducer, which has its own
value.

### 3. (Nit) Test asserts an exact count string

`.contains = "All (2) tests passed"` pins the count. That's the real output
format (`src/cli/main.zig:10474`), so it's correct today — but it means anyone
adding a third `expect` to the fixture has to remember to update the runner
entry in a different file. Asserting `"tests passed"` plus the existing
`not_contains` on `panic`/`Segmentation fault` would be equally strong and less
brittle. Minor.

### 4. (Nit) Test fixture doesn't isolate the trigger

`Issue10698WhileVarCapture.roc` is the issue reproducer, so it carries some
incidental machinery: `match items.get($index)` with a discarded `Err` arm, a
second `var $total`, and the `?` desugar. The *essential* ingredients are
(a) a `var` reassigned in a `while`, and (b) a closure inside the body capturing
that `var`. A reader debugging a future regression will have to re-derive that.
Consider a one-line comment at the top naming the two ingredients — or trimming
to the minimum, per `.rules` "Minimal Complexity". The PR body says variants
(`for`, nested `while`, capture before *and* inside the loop) were stress-checked
locally; at least the "captured both before and inside the loop" variant is the
one that exercises finding #1 above and would be worth committing.

### 5. (Nit) PR body has a Claude co-author trailer

The body ends with `🤖 Generated with [Claude Code]`. Per the repo's convention
that should come out before merge.

## Things I checked and found fine

- The fixture's module syntax (`Name :: [].{ ... }` with top-level defs after)
  matches the existing convention in `test/cli/EncoderFor*.roc`, including tabs.
- `bindLocalName` is a no-op for non-`assign` patterns and for out-of-range
  regions (`lower.zig:13929-13943`), so adding the call cannot fail or panic on
  synthetic binders.
- `loopStateTypeCell` / `finalCarryPattern` / `continueCarryExpr` all key off
  `carry.ty` and `carry.param_local` and are unaffected by the identity change.
- No snapshot output depends on local debug names, so `bindLocalName` on the
  parameter has no test fallout.
