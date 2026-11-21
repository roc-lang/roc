# 5. Reference Count Atomicity

| | |
|---|---|
| Status | Settled, with parts open. The analysis is proved and prototyped; its cost in compiled code, and whether edge modes are needed, are measured only once the compiler implements it |
| Builds on | [Structured tasks](03-structured-tasks.md), [one meaning per build](04-one-meaning-per-build.md) |
| Detail | [Atomic Reference Counts Across Tasks](../../concurrency-rc-atomicity.md) (the whole derivation and proofs); main RFC: [Reference counting across tasks](../../concurrency-explicit-suspension.md#reference-counting-across-tasks); shared-state RFC: [Reference counting](../../concurrency-shared-variables.md#reference-counting); models and prototype: [rc-atomicity/](../../rc-atomicity/README.md) |

When tasks run on several threads, which reference counts must be atomic, and how does the compiler decide that from explicit data instead of guessing?

## The Decision

### What You'll See

Heap-allocated Roc values are [reference-counted](https://en.wikipedia.org/wiki/Reference_counting). An _atomic_ count update stays correct when two threads update the same count at once; a _plain_ one is cheaper but doesn't. Today, values the host can see use atomic counts, and values the compiler proves are confined to Roc code use plain ones.

- **`None` and `Tasks` builds: nothing changes.** A `Tasks` host runs one resume at a time, so no two tasks' count updates can race, even when it moves tasks between threads.
- **`ParallelTasks` builds:** a value that two child tasks able to run at the same time can both reach gets atomic counts, in the code that touches it inside those children. Everything else stays plain:
  - a value moved into one child, however large;
  - code that builds a value before the children start, or uses it after they finish;
  - each child's own new allocations.
- **Children borrow what they capture.** The parent keeps the shared value alive until every child is done, so a child that only reads shared data updates none of its counts.
- **Values stored in a `Shared` cell** always use atomic counts in `ParallelTasks` builds (see [shared state](06-shared-state.md)).
- **Values the host can reach** keep atomic counts, as today.

So a platform author should declare `Tasks` rather than `ParallelTasks` unless the host really resumes tasks in parallel.

### How the Compiler Decides

The analysis runs on the compiler's ownership pass (ARC) in optimized builds, and records the atomicity of every count statement as explicit data that backends follow.

- **Modes.** Each local gets a _mode_, plain or atomic, for each _position_ in its value: the value itself, and each refcounted part reachable through fields, elements and payloads. A recursive type collapses to one position. Atomic is always safe, just slower, so the job is to find the least atomic assignment that's still safe.
- **Seeds** are where atomic starts:
  - at a spawn, an entry's capture that a sibling's capture may alias, decided by a points-to analysis that keeps call sites apart;
  - at the host, hosted arguments and results, spread in both directions as today, because the host may receive a value that earlier aliases still hold;
  - every store into a `Shared` cell.
- **Forward propagation.** Atomic flows only forward: through a copy, from an argument to its parameter, from a return to its result, from a stored value into its field of a new container, and from a field to the local read out of it. Nothing flows back, so a new container holding a shared value isn't itself shared, and parent code before the fork or after the join stays plain.
- **Least solution.** Every rule has the form "if this is atomic, so is that". The smallest assignment satisfying them all exists, sits below every other safe assignment, and is computed by reachability from the seeds, in time linear in the number of (local, position) pairs. No search, no heuristic.
- **Mode specialization.** A procedure is specialized by its parameters' modes, as Roc already specializes by type. In practice that's one copy for code inside children and one for code outside, so a helper the children share doesn't make the parent's private calls atomic.
- **Checking.** A debug-build certifier checks each constraint locally, statement by statement, independently of how the solution was found.

Plain and atomic updates of one allocation may mix as long as they never race. That replaces design.md's stronger rule that every statement touching one allocation agrees on its atomicity, and it's what lets the propagation run one way.

Three theorems back this, proved in the [atomicity document](../../concurrency-rc-atomicity.md#4-solution):

- **A, soundness:** any object two parallel children both touch is atomic at both touches.
- **B, optimality:** the least solution lies below every other safe one, so it's optimal for any monotone cost, and computing it takes linear time.
- **C, the static limit:** when a child path-copies a shared tree, new nodes and shared nodes sit at the same recursive position, so no static scheme can keep the new ones plain. _Edge modes_, one bit in each handle at such positions, close the gap, and are added only if a benchmark shows it matters.

## What We Tried

| Option | What it lost |
|---|---|
| Every count atomic | Measured: always-atomic counts (roc-lang/roc#7718) slowed the previous compiler's benchmarks by up to 62.5% (`n_queens`; `r_b_tree_ck` 34.8%, `deriv` 25.6%, `c_fold` 7.6%) |
| Every count plain, copying at task boundaries | Not evaluated separately in the sources. A spawn would walk and copy the captured data, which the requirement that spawning cost the same at any data size (K2) rules out |
| Today's analysis, seeded at spawns | It joins values into classes in both directions and links every call site of a procedure. One shared `Dict(Str, U64)` makes every dictionary of that type atomic, even in code unrelated to concurrency. It never regains plain counts after the children finish |
| Forward propagation without mode specialization | A helper shared with the children keeps the parent's private work atomic: 1,950 needless atomic updates against 2 with specialization |
| Today's union-find to choose seeds | Two values that merely pass through the same helper share a class, so moved data gets seeded: 166 needless atomic updates against 0 with points-to |
| Per-object marking at run time (Lean 4, Koka) | A sign test on every count update in the program (the previous compiler's per-object bit ran about 4% behind plain counts), a walk at every spawn over moved data too, marks that never go away, and lost in-place updates on marked data |
| Biased reference counting (CPython's PEP 703) | An owner field and a merge protocol on every allocation, to handle unstructured sharing that structured tasks don't have |

Rust makes the same choice statically, per type, as `Rc` or `Arc`, written by the programmer. Mode specialization infers that choice per position instead.

## Why This One

[Structured tasks](03-structured-tasks.md) make every execution a series-parallel order: two events can race only if they're in different children of the same fork. So only data the siblings' captures share can race. Moved data, the parent's own work and the children's new allocations can't. That's why spawn seeds need to flow only forward, while host seeds, which have no join, keep spreading both ways.

The choice meets the requirements the other schemes miss: code that touches no shared data pays nothing, no test and no atomic; spawning never walks the data; and the parent regains plain counts after the join. It follows the compiler's rules too: atomicity is computed by an earlier stage, written into each count statement, checked locally, and followed by backends.

Because the constraints are monotone, the least solution is optimal for any monotone cost, so there's nothing to tune and no search. Solving is plain reachability.

No atomicity choice makes truly contended updates cheap. In a C microbenchmark (noisy to about ±50%), two threads folding one list cost 3 to 25 times a plain fold, against about 1.2 to 2 times for uncontended atomics. Lending is what removes contention from read-only sharing.

## What It Costs

- **New compiler work.** A points-to analysis for the seed decision, forward edges over (local, position) facts, mode specialization, helper plans keyed by layout and mode instead of one flag, and a borrowing variant of the erased-callable convention, whose captures are all owned today.
- **Code growth.** Mode specialization copies procedures, in practice at most two per procedure.
- **Host data stays coarse.** In the fuzzed programs, about 90% of the needless atomic updates left come from host seeds' equality classes, which this design keeps as they are. Narrowing them is separate work.
- **Results stay atomic.** A child's result that holds shared parts makes the parent's later handling of those parts atomic. It's sound, and it's the only imprecision left in parent code.
- **The path-copy limit.** Static modes leave atomic counts on a child's new nodes when it path-copies a shared tree. Edge modes fix it, but need a spare bit in each handle at those positions. Roc already uses the low pointer bits for tag ids, and wasm32 has only two, so that needs a layout decision.
- **Dev builds may pay.** Dev builds and `.boxy` units may stay all-atomic in `ParallelTasks` builds, since the analysis runs only in optimized builds.
- **Contention stays.** Lending removes racing updates when children only read. When they path-copy shared data, some remain: 144 in the prototype's path-copy scenario.

## Evidence

From section 7 of the [atomicity document](../../concurrency-rc-atomicity.md#7-evidence) and the [rc-atomicity results](../../rc-atomicity/README.md#results):

- **Maude.** An executable model whose runtime detects count races directly, searched over every interleaving. The forward analysis is race-free on every scenario. Each deliberately wrong variant races: no atomics, construction edges dropped, and host seeds propagated forward only.
- **Lean.** `window_sound` mechanizes Theorem A for a window and `reach_optimal` mechanizes Theorem B, in Lean 4 core with no `sorry`.
- **Fuzzing.** Over 23,000 generated programs (1.7 million count updates), every analysis is sound, with and without lending, and each refinement makes a subset of the previous one's updates atomic, event by event.
- **Cross-check.** On 117 generated programs, the Python prototype and Maude computed the same forward facts, and the prototype's race oracle agreed with Maude's exhaustive search on every one.

Needless atomic updates, that is, atomic updates that can't race, counted by the prototype:

| Program | Today's analysis | Forward | Forward + mode specialization | Per-object marking |
|---|---|---|---|---|
| A helper shared with children does the parent's private work | 2,081 | 1,950 | 2 | 3, plus a test on all 2,139 updates |
| Children read a shared tree | 705 | 0 | 0 | 23 |
| Children path-copy a shared tree | 899 | 459 | 70 | 23 |
| 3,000 generated programs | 121,736 | 77,370 | 70,968 | 59,004, plus a test on all 213,337 updates and 14,352 objects walked |

Per-object marking's lower total comes almost entirely from host data, which this design leaves alone. With lending, children that only read a shared tree make no racing updates at all.

These are counts of events, not timings. Argued, not measured: the cost in compiled code. Step 6 of the implementation plan runs the roc-lang/roc#7718 benchmarks inside `all!` children in ReleaseFast `ParallelTasks` builds, compares points-to with the union-find on real programs, and adds edge modes only if the path-copy residual measurably costs.
