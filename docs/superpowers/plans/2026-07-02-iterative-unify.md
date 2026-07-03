# Iterative Unify Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace recursive type-checking unification in `src/check/unify.zig` with an explicit iterative work loop.

**Architecture:** Add a typed work stack to `unify.Scratch`; make guarded unification schedule `Frame` values and execute them in `runWorkLoop`. Preserve existing semantic order by scheduling post-child frames before child frames and pushing child frames in reverse execution order.

**Tech Stack:** Zig, Roc compiler `types.Store`, `collections.SafeList`, existing check tests, `jj`.

---

### Task 1: Add Stack-Safety Regression Coverage

**Files:**
- Modify: `src/check/test/unify_test.zig`

- [ ] Add a test that creates two very deep tuple chains with matching leaf types and verifies `env.unify(a, b)` returns `.ok`.
- [ ] Use an iterative test builder so the test itself does not recurse.
- [ ] Run `zig build test-check --summary all --color off`.
- [ ] Commit with `jj describe -m "test: cover deep iterative unification" && jj new`.

### Task 2: Introduce Explicit Work Frames

**Files:**
- Modify: `src/check/unify.zig`

- [ ] Add `Frame`, `GuardFrame`, and small post-child frame payload structs near `Unifier`.
- [ ] Add `unify_work_stack: MkSafeList(Frame)` to `Scratch`.
- [ ] Initialize, reset, and deinit the work stack with the other scratch-owned lists.
- [ ] Compile with `zig build test-check --summary all --color off`.
- [ ] Commit with `jj describe -m "refactor: add unify work stack" && jj new`.

### Task 3: Convert Guarded Pair Entry

**Files:**
- Modify: `src/check/unify.zig`

- [ ] Change `unifyGuarded` to push a guarded-pair frame.
- [ ] Add `runWorkLoop` and `processGuardedPair` to handle equivalence checks, pair guard entry, `unresolved_b` save/restore, and guard cleanup.
- [ ] Keep existing recursive helper bodies temporarily, but route top-level `unify` through `runWorkLoop`.
- [ ] Run `zig build test-check --summary all --color off`.
- [ ] Commit with `jj describe -m "refactor: drive unify through work loop" && jj new`.

### Task 4: Convert Simple Recursive Children

**Files:**
- Modify: `src/check/unify.zig`

- [ ] Convert alias backing, flex-with-constrained-alias, rigid-with-alias, structural-alias, tuple, function, nominal argument, and static-dispatch child unifications to schedule frames.
- [ ] Add post-child merge frames for tuple, function, nominal, and same-identity alias cases.
- [ ] Preserve child execution order by pushing frames in reverse.
- [ ] Run `zig build test-check --summary all --color off`.
- [ ] Commit with `jj describe -m "refactor: make simple unify children iterative" && jj new`.

### Task 5: Convert Records and Tag Unions

**Files:**
- Modify: `src/check/unify.zig`

- [ ] Convert record extension, shared field, tag extension, and shared tag child unifications to explicit frames.
- [ ] Preserve existing mismatch aggregation for shared fields and alias args.
- [ ] Preserve range-copy timing: append merged fields/tags only after nested child work completes.
- [ ] Run `zig build test-check --summary all --color off`.
- [ ] Commit with `jj describe -m "refactor: make row unification iterative" && jj new`.

### Task 6: Remove Recursive Unify Calls and Verify

**Files:**
- Modify: `src/check/unify.zig`
- Modify: `src/check/test/unify_test.zig`

- [ ] Search for remaining `try self.unifyGuarded` and `self.unifyGuarded(... catch` call sites inside helper processing code; replace them with frame scheduling or explicit child-result frames.
- [ ] Run `zig fmt` on modified Zig files.
- [ ] Run `zig build test-check --summary all --color off`.
- [ ] Run `zig build minici --summary all --color off`; if one section fails, rerun and fix that specific section until it passes before returning to full `minici`.
- [ ] Commit with `jj describe -m "refactor: complete iterative unify loop"`.

### Task 7: Publish Draft PR

**Files:**
- No source edits expected.

- [ ] Inspect `jj status` and `jj log -r 'ancestors(@, 12)' --no-graph`.
- [ ] Push this workspace's branch without global `jj` operations.
- [ ] Create a draft GitHub PR assigned to `jaredramirez`.
