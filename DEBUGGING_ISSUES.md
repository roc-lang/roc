# Debugging Issues During the LIR Interpreter Migration

This branch is doing a large refactor: evaluation is moving from the old CIR
interpreter to a new interpreter that executes post-RC LIR directly.

That changes the failure mode. A bug that looks like "the interpreter is wrong"
often starts earlier in the pipeline:

1. CIR typing or specialization
2. MIR lowering
3. monotype or layout resolution
4. MIR -> LIR lowering
5. call canonicalization
6. RC insertion
7. only then the LIR interpreter itself

If you debug the wrong stage, you usually lose a lot of time.

## The Most Important Fact First

On this branch there are currently two interpreter implementations in tree:

- `src/eval/cir_interpreter.zig`
- `src/eval/interpreter.zig`

And in `src/eval/mod.zig`, `Interpreter` still aliases `CirInterpreter`, while
the new engine is exported separately as `LirInterpreter`.

So the first question for any failure is not "what is wrong with the
interpreter?" but:

- which caller is failing?
- which interpreter path is that caller using?
- did the failure start before the interpreter ever saw the program?

Do not assume that a panic in a REPL, snapshot, LSP, or comptime path is
already inside the new interpreter.

## Mental Model For This Migration

The shared lowering path for the new execution model lives in
`src/eval/cir_to_lir.zig`.

The useful mental model is:

1. start from canonicalized CIR
2. lower to MIR in `src/mir/Lower.zig`
3. resolve monotypes and layouts
4. lower to LIR in `src/lir/MirToLir.zig`
5. run `src/lir/CallCanonicalize.zig`
6. run `src/lir/rc_insert.zig`
7. canonicalize again
8. execute the result with `src/eval/interpreter.zig`

The practical consequence is simple:

- `test-layout` failures are usually not interpreter bugs
- `test-lir` failures are usually not interpreter bugs
- `test-lsp` failures in `Lower.zig` are usually specialization/layout bugs
- only after those pass should you spend serious time inside the LIR
  interpreter

## Fast Triage Workflow

Use this order. It is the most reliable workflow we have found on this branch.

### 1. Reduce to one failing test or one tiny repro

Start with the smallest target that still fails:

```sh
zig build test-eval -- --test-filter "some failing name"
zig build test-layout -- --test-filter "some failing name"
zig build test-lir -- --test-filter "some failing name"
zig build test-lsp -- --test-filter "some failing name"
zig build snapshot -- test/snapshots/some_file.md --threads 1 --verbose
zig build minici
```

If logs are noisy or interleaved, use `--threads 1`.

If you can reduce the failure to a tiny `.roc` file or one REPL expression, do
that immediately. A standalone repro is usually better than instrumenting a
large snapshot too early.

### 2. Decide which class of bug it is

In practice most failures on this branch have landed in one of these buckets:

- lowering or specialization mismatch
- layout mismatch, especially recursive nominal types or boxed payloads
- RC or ownership mismatch
- host/comptime integration mismatch
- incorrect error semantics
- test helper bypassing the LIR path and hiding the real bug

### 3. Check the earliest stage that diverges

A useful rule:

- if `test-layout` fails, stop there first
- if `test-lir` fails, stop there first
- if both pass and `test-eval` fails, inspect `src/eval/interpreter.zig`
- if eval passes but snapshots or LSP fail, inspect host integration code

### 4. Compare against old behavior

There are three useful references:

1. `src/eval/cir_interpreter.zig`
2. `origin/main`
3. the current LIR path

The in-tree CIR interpreter is the fastest reference when you want to remember
how an older behavior used to work without changing branches.

Use `origin/main` when you need to answer:

- was this already broken before the migration?
- did a helper or caller drift as part of the refactor?
- which file changed semantics versus just moved code around?

## Comparing Against `origin/main`

The branch diff is large enough that broad diffs are often not useful. Prefer
small, path-scoped comparisons.

### Useful commands

```sh
git diff --stat origin/main -- src/eval src/lir src/layout src/mir src/repl src/lsp
git diff origin/main -- src/eval/interpreter.zig
git diff origin/main -- src/eval/cir_to_lir.zig
git diff origin/main -- src/mir/Lower.zig src/layout/store.zig src/lir/rc_insert.zig
git log --oneline origin/main..HEAD -- src/eval/interpreter.zig src/eval/cir_to_lir.zig
git show origin/main:src/eval/interpreter.zig | sed -n '1,220p'
```

These commands answer different questions:

- `git diff --stat ...` shows where the migration actually moved code
- `git diff origin/main -- <file>` is best for semantic comparison
- `git log origin/main..HEAD -- <paths>` shows the branch-local history that
  introduced the behavior
- `git show origin/main:<path>` is useful when you want the old implementation
  side-by-side without checking out another branch

### Use a worktree for behavior comparisons

For real behavior comparison, a worktree is better than mental diffing.

```sh
git worktree add /tmp/roc-origin-main origin/main
cd /tmp/roc-origin-main
zig build test-eval -- --test-filter "issue 8754"
```

Then run the same command in your current branch and compare:

- pass vs fail
- different panic site
- different rendered output
- different refcount behavior

This is especially useful when a failure could be in:

- lowering
- host ABI integration
- RC insertion
- test helper drift

## Best Tools For This Migration

### Focused build targets

These have been the most useful while working on this migration:

```sh
zig build test-layout
zig build test-eval
zig build test-lir
zig build test-lsp
zig build snapshot
zig build minici
```

A practical way to use them:

- use `test-layout` to validate recursive nominal/layout issues
- use `test-lir` for `CallCanonicalize` and `RcInsert`
- use `test-eval` for interpreter semantics
- use `snapshot` for REPL-style integration regressions
- use `test-lsp` when a MIR/lowering change breaks editor queries
- use `minici` as the broad end-to-end gate

### Trace evaluation

The interpreter already supports trace-oriented debugging:

```sh
zig build -Dtrace-eval=true
zig build snapshot -- --trace-eval test/snapshots/repl/some_snapshot.md
```

Use this when you need to confirm:

- which expression is being executed
- whether a branch or pattern match went down the wrong arm
- whether the evaluator is looping instead of making progress

### Trace refcounts

Refcount bugs rarely become obvious from source-level output alone.

```sh
zig build -Dtrace-refcount=true
```

Use this when you suspect:

- a use-after-free
- a leak
- a missing decref
- a hidden ownership transfer bug in a low-level builtin

### Run the cached Zig test binary directly

When `zig build` crashes a test with signal 6 or 11, rerun the exact cached test
binary that Zig prints, but remove `--listen=-`.

Example shape:

```sh
./.zig-cache/o/<hash>/test --seed=0x1234 --cache-dir=./.zig-cache
gdb --batch --ex run --ex bt --args ./.zig-cache/o/<hash>/test --seed=0x1234 --cache-dir=./.zig-cache
```

This is the fastest path from "Zig said something crashed" to "I have a real
backtrace at the failing site".

### Small GDB scripts beat interactive debugging

For refcount and ownership bugs, tiny `.gdb` scripts are usually more useful
than stepping interactively for hours.

Pattern:

```gdb
set pagination off
set debuginfod enabled off
break utils.decrefDataPtrC
commands
  silent
  printf "\ndecref_data regs rdi=%p rsi=%#lx rdx=%#lx rcx=%p\n", $rdi, $rsi, $rdx, $rcx
  bt 6
  continue
end
run
```

This style of script was useful for this migration because it lets you:

- break on one hot helper
- print the raw ABI values
- optionally print a short backtrace
- continue automatically

That is often enough to spot a double decref, wrong caller, or invalid pointer
without needing a full interactive session.

### `strace` for build or platform confusion

If you are not sure which file, binary, cache artifact, or platform object is
actually being used, `strace` is a fast sanity check:

```sh
strace -f -e trace=%file zig build snapshot -- test/snapshots/docs_transitive_modules.md --threads 1
```

This is useful when the bug might be:

- the wrong platform binary
- stale cache state
- a missing temporary file
- unexpected file resolution in a test harness

### `rg` and path-scoped searching

Use `rg` aggressively:

```sh
rg -n "tag_payload_access|cell_load|while_loop|expect|runtime_error" src/eval src/lir src/mir
rg -n "issue 8754|issue 8979|recursive nominal|double-box" src test
rg -n "Str.inspect|wrapInStrInspect|lirInterpreterStr" src/eval/test
```

Migration work usually spans several layers, so text search is often faster than
trying to reason from a single stack trace.

## Things We Have Already Learned The Hard Way

These are the patterns that have repeatedly produced real bugs during this
migration.

### 1. Many "interpreter" failures are really layout failures

Two recurring examples were:

- recursive nominal types with `Box`
- boxed tag-union payloads that had to be normalized to the layout expected by
  pattern matching or payload access

If a value looks structurally correct but the interpreter still panics or
mis-matches on it, inspect:

- `src/layout/store.zig`
- `src/layout/mir_monotype_resolver.zig`
- `src/interpreter_layout/store.zig`

before assuming the value execution logic is wrong.

### 2. Specialization bugs can surface as field access or pattern match panics

A panic in `src/mir/Lower.zig` does not automatically mean the local lowering
code is wrong. We saw failures where the real problem was monotype mismatch or
specialization reuse, and the visible symptom was a later panic like
`lowerDotAccess` seeing a non-record receiver.

If the panic site is in `Lower.zig`, also inspect:

- monotype equality assumptions
- specialization caches
- layout reuse keyed by the wrong notion of equality

### 3. Error semantics matter

The LIR interpreter must preserve behavior, not just produce a value.

Real examples:

- `expect` failure must record through the expect path, not act like a crash
- division by zero and modulo by zero must report the expected runtime message
- compile-time infinite loops must fail, not hang forever

When behavior is wrong, compare not only the final value but also:

- crash path
- expect path
- runtime error message
- whether execution continues after a failure

### 4. Zero-sized values need explicit care

Zero-sized types are an easy place to accidentally write code that is "almost
correct" but still wrong for the runtime representation.

We hit this in list operations where:

- data pointers can legitimately be null
- semantic length still matters
- capacity or ownership metadata may still matter

Any low-level builtin that touches lists, boxes, tags, or strings should be
checked for zero-sized edge cases.

### 5. Test helpers can lie to you

For LIR migration work, helpers that constant-fold or render through the old
path can hide the real bug.

When a test is supposed to validate the new interpreter, prefer a helper that:

1. lowers through the shared CIR -> MIR -> LIR pipeline
2. executes through `LirInterpreter`
3. renders via `Str.inspect` or the LIR value formatter

This is much more trustworthy than a helper that folds values before the LIR
interpreter ever sees them.

### 6. Allocator mismatches can masquerade as runtime corruption

Do not assume every late crash is an ownership bug in the interpreter.
Allocator mismatches in tests or helper code can produce shutdown crashes that
look like evaluator corruption.

When the failure happens during cleanup or deinit, inspect the test helper and
its allocator ownership before changing RC code.

## Where To Look First By Symptom

### Layout assertion or recursive nominal failure

Look at:

- `src/layout/store.zig`
- `src/layout/mir_monotype_resolver.zig`
- `src/interpreter_layout/store.zig`

Typical smell:

- double boxing
- wrong payload layout
- recursive nominal not resolving consistently across code paths

### Panic in `src/mir/Lower.zig`

Look at:

- monotype equality and reuse
- specialization bookkeeping
- tag/field structural equivalence

Do not assume the panicking lowering function is the root cause.

### `test-lir` or RC-related failures

Look at:

- `src/lir/CallCanonicalize.zig`
- `src/lir/rc_insert.zig`
- the corresponding LIR emitted before and after RC insertion

Typical smell:

- cleanup inserted on the wrong path
- ownership transfer not reflected in RC ops
- final use or early-return cleanup emitted incorrectly

### `test-eval` failure with layout tests already passing

Look at:

- `src/eval/interpreter.zig`
- `src/eval/value.zig`
- `src/eval/value_format.zig`
- low-level builtin execution helpers

Typical smell:

- wrong layout used at a boundary
- missing normalization between boxed and unboxed representations
- wrong low-level runtime semantics

### Snapshot or REPL failure

Look at:

- `src/repl`
- `src/eval/comptime_evaluator.zig`
- `src/eval/test/helpers.zig`
- any path that caches or reuses module state across evaluations

Typical smell:

- caller still using old interpreter assumptions
- trace or rendering drift
- helper comparing the wrong backend or wrong formatting path

### Fails only in a dev app, not in focused eval tests

Look at:

- hosted-call ABI boundaries
- builtin wrappers
- RC handoff between generated code and runtime helpers

In these cases, the interpreter may be fine and the real issue may be in the
platform or backend integration path.

## Practical Comparison Strategy

When a bug is stubborn, use this progression:

1. get a minimal repro
2. run the focused test on the current branch
3. run the same repro on `origin/main`
4. inspect the relevant old code in `src/eval/cir_interpreter.zig`
5. decide the first stage where behavior diverges
6. instrument only that stage

This is much faster than adding debug prints to five layers at once.

## Good Habits For This Branch

- Add a focused regression test as soon as you understand the bug.
- Prefer comparing layout ids, symbols, tag ids, and structural state over
  comparing rendered strings inside core compiler code.
- Keep instrumentation narrow and temporary. Convert it to a test or an
  invariant once the bug is understood.
- If you fix a layout or lowering bug, rerun the next wider tier:
  `test-layout` -> `test-lir` -> `test-eval` -> `snapshot` -> `minici`.
- If you think the interpreter is wrong, first prove that the post-RC LIR fed
  into it is actually correct.

## Suggested File Map

When navigating this migration, these are the files worth keeping open together:

- `src/eval/mod.zig`
- `src/eval/cir_interpreter.zig`
- `src/eval/interpreter.zig`
- `src/eval/cir_to_lir.zig`
- `src/eval/comptime_evaluator.zig`
- `src/eval/test/helpers.zig`
- `src/mir/Lower.zig`
- `src/lir/MirToLir.zig`
- `src/lir/CallCanonicalize.zig`
- `src/lir/rc_insert.zig`
- `src/layout/store.zig`
- `src/layout/mir_monotype_resolver.zig`

That set usually makes it possible to answer:

- which interpreter path is active?
- where does this layout come from?
- where did this LIR node come from?
- who inserted this RC operation?
- is this really an interpreter bug or not?
