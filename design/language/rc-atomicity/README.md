# Reference-count atomicity across tasks: models and prototype

These files back
[Atomic Reference Counts Across Tasks](../concurrency-rc-atomicity.md), the
companion to the
[concurrency RFC](../concurrency-explicit-suspension.md). They model the
design, check it and measure it. None of this is compiler code.

| Directory | Tool | What it establishes |
|---|---|---|
| `maude/` | Maude 3.5.1 | An executable model, exhaustively searched over every interleaving: a small RC language, the analyses, and a runtime that detects count races directly |
| `lean/` | Lean 4.36 (core only) | Machine-checked proofs of Theorem A, the window soundness of forward propagation, and Theorem B, the optimality of the least solution |
| `proto/` | Python 3 | A prototype of every analysis on a richer language (calls, recursion, branches), with a race oracle, a Lean-style marking runtime, borrow inference with lending, a typed program fuzzer, and a cross-check against the Maude model |

The regression tests for all three, and for the
[concurrency prototype](../concurrency-prototype/README.md), are in
`../concurrency-prototype/tests.py`.

## Running

```bash
cd design/language/rc-atomicity/maude && maude -no-banner -batch checks.maude < /dev/null
```

```bash
cd design/language/rc-atomicity/lean && lean RcAtomicity.lean
```

```bash
cd design/language/rc-atomicity/proto && python3 fuzz.py 3000 2
```

```bash
cd design/language/rc-atomicity/proto && python3 fuzz.py 3000 2 --channels
```

```bash
cd design/language/rc-atomicity/proto && python3 scenarios.py
```

```bash
cd design/language/rc-atomicity/proto && python3 bridge.py 120 11 20
```

## The model (`maude/`)

`rc.maude` defines three layers.

- **Syntax.** Construction, copy, projection, drop, in-place field update,
  hosted calls, and fork-join children with captured parameters.
- **Analyses.** Each analysis yields a set of prefix facts `m(local, path)`,
  meaning every path from that prefix down is atomic:
  - `forward` is the proposal;
  - `unify` is the current analysis;
  - `allAtomic` and `noAnalysis` are the two baselines;
  - `forwardHostForward`, `forwardNoConstruct` and `forwardNoAlias` are
    deliberately wrong variants.
- **Runtime.** A plain count update takes two steps, a read and then a
  write; an atomic update takes one. Any access by another party while a
  plain access is in progress is reported as a race, so races are detected
  directly rather than through their symptoms. Use after free, leaks and
  stuck parties are errors too.

`checks.maude` runs every scenario in `scenarios.maude` under each analysis.

| Check | Result |
|---|---|
| `forward` on all six scenarios | Race-free in every interleaving |
| `unify` | Race-free |
| Serial schedules (the `Tasks` setting) with no atomics at all | Race-free (T1) |
| No atomics, parallel | Race found |
| Construction edges omitted | Race found: positions are needed |
| Host seeds propagated forward only | Race found: host sharing needs equality classes |

## The proofs (`lean/`)

`RcAtomicity.lean` checks with no `sorry`; the main theorems depend only on
`propext` and `Quot.sound`.

- **`reach_optimal` (Theorem B).** Forward reachability from the seeds is a
  solution of the constraints, it lies below every solution, and so it
  minimizes every monotone cost.
- **`window_sound` (Theorem A).** Take a window in which parties start from
  one immutable heap and their captures, and allocate only fresh objects of
  their own. Assume every capture that reaches a shared object is seeded,
  and the mode predicate satisfies the constraints of every party's code.
  Then any object two parties both touch is shared, and both touches are
  atomic.
- **`window_sound_least`.** The same theorem, instantiated with the least
  solution.

The window model omits in-place updates. They touch only unique objects,
which no other party can reach; the Maude model covers them.

## The prototype (`proto/`)

| File | Contents |
|---|---|
| `rcproto.py` | The language, an interpreter that records every count event with its series-parallel party, the race oracles, and the analyses listed below. Without channels, the oracle is the Feng–Leiserson criterion plus host visibility. With them, it orders events by happens-before, where a send is ordered before its receive |
| `fuzz.py` | A typed random program generator that composes small constructs. It checks that every analysis except `none` is sound, and that `forward_edge ⊆ forward_pts ⊆ forward_ctx ⊆ forward ⊆ unify` on every event. With `--channels`, some forks send a value from one child to a later one, and the sender keeps using what it holds |
| `scenarios.py` | Programs that each isolate one effect the note claims |
| `bridge.py` | Translates generated programs into Maude, then checks three things: that Python and Maude compute the same forward facts, that `forward` is race-free in every interleaving, and that the Python race oracle agrees with Maude's exhaustive search |

The analyses, each more precise than the one before:

| Analysis | What it adds |
|---|---|
| `unify` | Today's analysis: union-find over all value flow, seeded at every spawn boundary and hosted call |
| `forward` | Spawn seeds only on captures that may alias a sibling's, propagated forward over (local, path) facts. Host seeds keep their equality classes |
| `forward_ctx` | Procedures specialized by their parameters' modes |
| `forward_pts` | Seeds decided by `PointsTo`, a may-alias analysis that keeps call sites apart. It gives each procedure a summary of what its result reaches, in terms of its parameters and fresh objects, which a caller names by its call site |
| `forward_edge` | Edge modes (note, 4.4). Positions covered only by facts truncated at a recursive layout get mode T, and RC statements there test a bit carried by the handle they use |
| `lean` | Run dynamically, for comparison: Lean-style marking |

`Borrow` adds lending (note, 4.6): parameters and child captures are borrowed
unless consumed, and the run that uses it has its own trace and race oracle.

Every analysis treats a sent value as it treats a hosted argument: a seed,
closed under its equality class. This is the sessions RFC's S8
([concurrency-sessions.md](../concurrency-sessions.md), "Reference
counting"). `SEND_SEEDS = False` in `rcproto.py` is its mutation.

## Results

All counts are of count-update events; no timings are measured here.

**Fuzzing: 3,000 programs (seed 2), 212,857 count updates, 59,446 of which
can race.** No sound analysis left a racing update plain, and the precision
order held on every event. A further 20,000 programs (seed 7) gave the same
verdicts.

| Analysis | Atomic | False atomic | Extra costs |
|---|---|---|---|
| `all_atomic` | 209,209 | 149,763 | |
| `unify` (today) | 181,182 | 121,736 | |
| `forward` | 136,816 | 77,370 | |
| `forward_ctx` | 130,414 | 70,968 | |
| `forward_pts` | 128,700 | 69,254 | |
| `forward_edge` | 128,696 | 69,250 | 6 bit tests |
| `lean` (dynamic) | 118,514 | 59,004 | A test on all 213,337 updates, 14,352 objects walked, 160 in-place updates lost |

Of `forward_pts`'s 69,254 false atomics, 62,650 come from host equality
classes, which the proposal leaves as they are, and 6,604 from spawn seeds
(`../concurrency-prototype/bench.py`). The gap to `lean` is therefore almost
all host data.

With lending, the same programs make 148,749 count updates, of which 30,624
can race. That is half as many racing updates as without lending. Random
programs keep shared values in results far more often than real ones, so
this is a pessimistic bound.

**Scenarios: false atomic updates.**

| Scenario | `unify` | `forward` | `forward_ctx` | `forward_pts` | `forward_edge` | `lean` | Racing updates | Racing, lent |
|---|---|---|---|---|---|---|---|---|
| `spreading`: a shared helper does the parent's private work | 2,081 | 1,950 | 2 | 2 | 2 | 3, plus 2,139 tests | 8 | 2 |
| `moves`: each child gets its own tree | 1,532 | 166 | 166 | 0 | 0 | 210, plus 42 walked | 0 | 0 |
| `path_copy_shared` (Theorem C) | 899 | 459 | 70 | 70 | 0, plus 130 tests | 23 | 226 | 144 |
| `shared_read` | 705 | 0 | 0 | 0 | 0 | 23 (permanent marks) | 166 | 0 |
| `host_data` | 242 | 242 | 242 | 242 | 242 | 0 | 52 | 2 |

What the scenario numbers show:

1. **Forward propagation alone is not enough when a shared helper does
   private work** (`spreading`). The helper's body is one context, so the
   parent's private calls are atomic too. Specializing by mode removes
   almost all of it, so mode specialization is necessary, not optional.
2. **Seeds need a may-alias analysis that keeps call sites apart**
   (`moves`). Both trees pass through `insl`, so the union-find merges them,
   and the seed rule fires although nothing is shared. `PointsTo` removes
   every one of these false atomics.
3. **Edge modes close the static limit** (`path_copy_shared`). The 70 false
   atomics left where shared and new nodes meet in one recursive position
   become 0, at the cost of 130 bit tests, where Lean-style marking leaves
   23 and tests all 1,125 updates.
4. **Lending removes contention from read-only sharing** (`shared_read`):
   children that only read a shared tree make no racing updates at all.
   Path copying still retains shared subtrees in new nodes, so 144 racing
   updates remain there.
5. **Host classes are as coarse as today's** (`host_data`). The proposal
   does not change them; that is separate work, and it is now where nearly
   all remaining imprecision lies.

**Cross-check: `bridge.py 120 11 20`.** Of 120 programs, 117 completed and
3 hit the 20-second search limit; 18 of the 117 have races.

| Check | Result |
|---|---|
| Python and Maude computed the same forward facts | 117/117 |
| `forward` race-free in every interleaving Maude explored | 117/117 |
| Python race oracle agrees with Maude's exhaustive search | 117/117 |

The last row validates the oracle that the fuzzer's soundness numbers rely on.

**Channels: `fuzz.py 3000 2 --channels`.** 261,651 count updates, 90,483 of
which can race. With send seeds, no analysis left a racing update plain, and
the precision order held on every event.

| Analysis | Atomic | False atomic | Programs unsound without send seeds |
|---|---|---|---|
| `all_atomic` | 257,083 | 166,600 | |
| `unify` | 230,407 | 139,924 | 127 |
| `forward` | 212,716 | 122,233 | 484 |
| `forward_ctx` | 211,634 | 121,151 | 487 |
| `forward_pts` | 211,290 | 120,807 | 498 |
| `forward_edge` | 211,288 | 120,805 | 498 |
| `lean` (dynamic) | 157,657 | 66,975 | |

The happens-before oracle is checked against the series-parallel one: on
programs without channels, the two report the same racing updates.
