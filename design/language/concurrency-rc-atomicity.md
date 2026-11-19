# Atomic Reference Counts Across Tasks

| | |
|---|---|
| Companion to | [RFC: Suspendable Effects and Platform-Scheduled Concurrency](concurrency-explicit-suspension.md), "Reference counting across tasks" |
| Status | Draft |

When Roc tasks run in parallel, some reference-count updates must be atomic.
This note does four things:

1. states what any solution must satisfy;
2. chooses the mathematical structures in which the problem is naturally
   expressed;
3. restates the problem in them;
4. solves it.

The solution is a least fixpoint over a two-point lattice. The current
analysis computes a coarser fixpoint of the same kind, and this note pins down
exactly where the extra coarseness comes from.

## 1. Requirements

**Safety.** These are non-negotiable.

| | Requirement |
|---|---|
| S1 | **No count races.** Two updates of one count word by parties not ordered by happens-before are both atomic read-modify-writes (C11). |
| S2 | **Exact release.** Exactly one party sees an allocation's count reach zero, and it frees the allocation after every other party's accesses. |
| S3 | **Sound uniqueness.** An in-place update happens only when no other party can observe the allocation. |
| S4 | **Host boundary.** Everything the host may retain is treated as shared, as today. |

**Cost.** These are the reasons to do anything at all.

| | Requirement |
|---|---|
| K1 | **No overhead without sharing.** Code that touches no shared data pays nothing: no atomic, no test. This covers single-task programs, `Tasks` builds and pure code outside parallel children. |
| K2 | **Spawn cost independent of data size.** Spawning never walks the captured data. |
| K3 | **No permanent penalty.** Once the children that share a value have finished, that value regains plain counts and in-place updates. |
| K4 | **Proportionality.** Atomic updates fall only on updates that may race, up to the precision of the abstraction. |
| K5 | **Minimal contention.** Count updates on really shared data are themselves minimized, since no choice of atomicity makes contended updates cheap. |

**Engineering.** These follow from AGENTS.md and design.md.

| | Requirement |
|---|---|
| E1 | **Explicit data.** Atomicity is LIR data computed by an earlier stage, which backends follow without heuristics or fallbacks. |
| E2 | **Modular and optional.** Dev builds and `.boxy` units may stay all-atomic (always sound). The analysis runs only in optimized builds. |
| E3 | **Certifiable.** A debug certifier can check a solution independently of how it was found. |
| E4 | **Bounded code growth.** |
| E5 | **Wasm.** No atomics on targets without threads. |

As a check, here is how the two existing schemes fare against these
requirements:

- **Lean 4 and Koka** fail K1 (a test on every update), K2 (a walk at every
  spawn) and K3 (marks are permanent, and marked objects lose in-place
  updates).
- **Roc's current analysis, seeded at spawns**, fails K3 and badly fails K4
  (spreading).

## 2. Structures

### 2.1 Executions are series-parallel orders

Structured concurrency makes every execution a series-parallel partial order
(SP order). Call nesting, `;` and resumes compose in series. Each combinator
composes its children in parallel between a fork node and a join node. An SP
order is described by its decomposition tree, whose internal nodes are $S$
(series) or $P$ (parallel) and whose leaves are events.

> **Fact (SP race criterion).** Two events are unordered by happens-before iff
> their lowest common ancestor in the decomposition tree is a $P$ node. This is
> the basis of SP-bags race detection, Feng and Leiserson, SPAA 1997.

A $P$ node is a *window* $W$. Its *parties* are the children; under the
combinator obligation O1 below, the parent does no count updates on
child-reachable data inside the window. In `Tasks` builds (P10), resumes are
totally ordered, so no $P$ node orders anything in parallel and Roc events
never race. Host events lie outside the SP order and may race with anything
the host retains (S4).

> **O1.** Inside a window, a combinator moves each closure into its child and
> touches no count reachable from a child's roots.

### 2.2 Count words are resources; forking splits them

In separation logic, the parallel rule

$$
\frac{\{P_1\}\,c_1\,\{Q_1\} \qquad \{P_2\}\,c_2\,\{Q_2\}}
     {\{P_1 \ast P_2\}\; c_1 \parallel c_2 \;\{Q_1 \ast Q_2\}}
$$

forces the heap to be split between the two sides.

- **Fields are free.** Roc values are immutable, so their fields are
  persistent facts that both sides may hold at once.
- **Count words are the only real resource.** A plain update of $k_a$ needs
  exclusive ownership of $k_a$, and $\mathrm{own}(k_a) \ast \mathrm{own}(k_a)$
  is false.

At each window, every count word therefore goes to one of three places:

| Where $k_a$ goes | Meaning | What that party may do with $k_a$ |
|---|---|---|
| **Exclusive** to one party | A move | Update with plain operations |
| **Shared invariant** | Atomic | Update with atomic operations |
| **Fractions** to each party, while one owned unit stays with the parent | A borrow | Read and borrow, but not update |

The third form is Bornat et al.'s counting permissions. It is the precise
meaning of *lending*: the parent keeps one unit, the children hold fractions,
and the whole is restored at the join. A party that needs to update $k_a$ while
holding only a fraction must first acquire a unit, which is a retain.

### 2.3 Modes: a two-point lattice with subsumption

Let $\mathbf{2} = \{\mathsf{P} \sqsubseteq \mathsf{A}\}$, where $\mathsf{P}$
is plain and $\mathsf{A}$ is atomic. The essential property is that **atomic is
always sound**: an atomic update of an allocation that never races is merely
slower. This gives *subsumption*: a $\mathsf{P}$ value may be handled anywhere
an $\mathsf{A}$ value is expected. So a sound assignment may raise any mode,
and the problem is to find the least one.

**Lemma 0.** Plain and atomic updates may be mixed on one allocation as long
as they never race. *Proof.* C11 forbids only racing non-atomic accesses. ∎

So "every statement touching one allocation agrees on its atomicity", which
design.md requires today, is stronger than safety needs. Section 4 shows that
this extra strength is what forces the analysis to be symmetric.

**Positions.** A refcounted layout $L$ has a graph of *positions*: the root,
and each refcounted sub-layout reachable through fields, elements and
payloads. Recursive layouts form strongly connected components (SCCs), which
collapse to one position. A *mode* for a value of layout $L$ is a map
$\mu : \mathrm{Pos}(L) \to \mathbf{2}$ that is monotone toward contents:
whatever is inside a shared value is shared. Equivalently, it is a set of
positions closed under going deeper. Each SCC gets a single mode. This is where
the static limit of Section 4.4 comes from.

### 2.4 Constraints: inclusion, not unification

A mode assignment is a solution of monotone inequalities over $\mathbf{2}$.
The analysis literature offers two ways to solve value-flow problems:

| | Equality-based (Steensgaard) | Inclusion-based (Andersen) |
|---|---|---|
| Each flow edge $y \to x$ means | $\mu(x) = \mu(y)$ | $\mu(y) \sqsubseteq \mu(x)$ |
| Solved by | union-find | reachability |
| Propagation | both directions | forward only |

The current analysis is equality-based (`computeVisibilityFromFacts`). Over a
two-point lattice, the inclusion-based version is only directed reachability,
so it is no slower.

## 3. The problem, restated

> Given the LIR flow graph, the layout position graphs and the spawn sites,
> find the least assignment $\mu(x, p, \kappa) \in \mathbf{2}$, for each local
> $x$, position $p$ of its layout and context $\kappa$, such that every count
> event that can race (Section 2.1) is assigned $\mathsf{A}$.

Each RC statement names a local, and a helper plan walks positions, so every
event has a well-defined $(x, p, \kappa)$.

## 4. Solution

### 4.1 Constraints

| Statement or site | Constraint |
|---|---|
| Spawn group: capture $c$ of child $i$ whose alias class meets a capture of a sibling $j \neq i$ | $\mu(c, p) = \mathsf{A}$ for every $p$ |
| Host seeds (S4) | Unchanged: equality classes, as today |
| Flow ($x := y$; argument → parameter; return → result; capture → captured) | $\mu(y, p) \sqsubseteq \mu(x, p)$ |
| Construction ($x := C(\ldots, y \text{ at } q, \ldots)$) | $\mu(y, p) \sqsubseteq \mu(x, q\cdot p)$, with no edge back |
| Projection ($x := y.q$) | $\mu(y, q\cdot p) \sqsubseteq \mu(x, p)$ |
| Monotonicity | $\mu(x, p) \sqsubseteq \mu(x, p\cdot q)$ |
| Low-level operations | Directed edges from `RcEffect` sharing masks, argument → result |

Alias classes decide *which captures to seed*. The existing union-find is a
sound may-alias analysis for this, because it joins containers with their
contents: two captures that reach a common allocation end up in one class.
Only the propagation that follows is directed.

### 4.2 Theorem A: forward propagation is sound for windows

If $\mu$ satisfies 4.1, then $\mathsf{at}(e) = \mu(x_e, p_e, \kappa_e)$
satisfies S1 for every pair of racing Roc events.

*Proof.*

1. Let $e_1, e_2$ race on allocation $a$. By the SP race criterion, their
   lowest common ancestor is a window $W$, and they belong to different
   parties $P \neq Q$.
2. During $W$, a party reaches only:
   - what was reachable from its captures at the fork;
   - its own new allocations;
   - host results and static data.
3. Nothing moves a handle between parties inside $W$. By S3, in-place updates
   change only allocations that one party alone can reach. Since $a$ is
   reached by both $P$ and $Q$, it is not a new allocation of either, so $a$ is
   reachable from a capture $c$ of $P$ and from a capture $d$ of $Q$.
4. Containment links put $c$ and $d$ in the same alias class, so both are
   seeded.
5. Every handle $P$ uses to reach $a$ is derived from $c$ by a chain of flow,
   projection, construction and call steps. This includes handles stored into
   $P$'s own new containers and read back out.
6. Each step carries $\mathsf{A}$ forward to the position at which $a$ is
   reached, by its constraint in 4.1. So $e_1$ is assigned $\mathsf{A}$, and
   symmetrically so is $e_2$.
7. Events outside $W$ are ordered with $W$'s events, and Lemma 0 allows them
   to be plain. ∎

The theorem only covers windows. Host sharing has no join, and the host can
receive an allocation that existing aliases still hold, so aliases created
*before* the hosted call need the mode too. That is why host seeds keep
equality propagation. Structured concurrency is what lets spawn seeds avoid it:
every alias a party uses inside a window is created after the fork, from its
captures.

**Corollaries.**

- **Moves are free.** A capture that no sibling can reach is never seeded,
  however large it is.
- **Parent code stays plain.** No backward edge exists, so code that builds a
  value before the fork, or uses it after the join, keeps plain counts. This
  satisfies K3 for the parent.
- **Child results need not be seeds.** The join orders them. Forward
  propagation through a result does make the parent's later handling of
  shared parts $\mathsf{A}$. That is sound, and it is the only K3 imprecision
  left (see 4.5).

### 4.3 Theorem B: the least solution is optimal and linear-time

All constraints in 4.1 are monotone Horn clauses over $\mathbf{2}$ (seeds
$\top \sqsubseteq x$, edges $x \sqsubseteq y$). By Knaster–Tarski, a least
solution exists, and it is pointwise below every other solution. Any cost
function that is monotone in $\mu$ is therefore minimized by the least
solution. **No search or optimization is needed: solving the problem means
computing reachability from the seeds.** That takes
$O(|\text{nodes}| + |\text{edges}|)$, where the nodes are (local, position)
pairs.

For E3, the certifier checks a solution by checking each constraint locally.

### 4.4 Theorem C: the static limit, and how to cross it

Let a statement $s$ in context $\kappa$ touch a recursive layout whose SCC
$\sigma$ holds a shared allocation in some execution and an unshared one in
another (or in the same one). Then every sound assignment that is a function of
$(s, \kappa, \text{position})$ has $\mu(s, \kappa, \sigma) = \mathsf{A}$, and
$s$'s events on the unshared allocations are atomic with no possible race.

*Proof.* Both allocations sit at the same position, and soundness forces that
position to $\mathsf{A}$. ∎

The canonical case is **path copying inside a child**: inserting into a shared
tree creates new nodes along one path, which point to shared subtrees
everywhere else. No static scheme can do better here, at any precision. This is
the only place where per-object information (Lean's marks) is more precise
than the least static solution.

**Edge modes.** Runtime information is needed only at such positions, and it
does not have to be per-object. Extend $\mathbf{2}$ with a third mode
$\mathsf{T}$ (test), used only for mixed SCC positions in window code:

- **Writing the bit.** A handle stored at a $\mathsf{T}$ position carries one
  bit, set when it is stored, from the stored value's mode:
  - from its static mode, when that mode is $\mathsf{P}$ or $\mathsf{A}$;
  - copied from the handle it was read from, when that mode is $\mathsf{T}$.
- **Reading the bit.** RC statements at $\mathsf{T}$ positions test the bit;
  no other statement does.

This meets every requirement Lean's scheme misses:

| Requirement | Why edge modes meet it |
|---|---|
| K1 | Only $\mathsf{T}$ positions test |
| K2 | Nothing is walked: the bit comes from static knowledge at the moment each edge is created. Lean must walk because it has no static knowledge |
| K3 | Code outside windows is compiled with $\mathsf{P}$ for those positions and ignores the bits |

The cost is one spare bit in each handle at $\mathsf{T}$ positions. Roc
already uses the low pointer bits for tag ids (`DATA_PTR_TAG_MASK`), and on
wasm32 only two such bits exist, so this needs a layout decision. Whether
edge modes are worth it is an empirical question: they pay off only for
programs that path-copy shared recursive data inside children.

### 4.5 Contexts: monomorphizing modes

A procedure called with $\mathsf{A}$ arguments in a child and $\mathsf{P}$
arguments elsewhere receives the join of the two through 4.1's argument edges.
In other words, its body is $\mathsf{A}$ everywhere, but because the edges are
forward only, its callers are not affected.

To recover the body, treat procedures as *mode-polymorphic* and specialize
them by their parameters' mode vectors, as Roc already specializes by type.
Mode vectors are monotone maps into a finite lattice, so the set of
specializations is finite, and a worklist over (procedure, mode vector) pairs
computes the specializations needed as part of the same least fixpoint. In
practice a procedure gets at most two copies: one inside windows and one
outside. The same device, applied at joins, would remove the last K3
imprecision of 4.2 by analysing a child's result in the parent's own context.

### 4.6 Contention: lending as counting permissions

S1 makes racing updates atomic, but K5 asks for fewer of them. By 2.2, a child
that holds only a fraction of $k_a$ cannot update it: every update it makes is
a retain followed by a release. Two cases arise:

- **Released before the join.** The pair is balanced within one party.
  The parent's unit keeps $k_a \geq 1$ throughout, so no decision depends on
  the pair, and ARC's borrow machinery removes it.
- **Surviving to the join.** These are the escaping retains: handles to shared
  data that a child keeps in its result. Only these events can still race. They
  are atomic increments, bounded by the number of edges from a child's result
  into shared data.

The parent's paused record already holds the unit (G5, R3). Spawned closures
need a borrowing variant of the erased-callable convention, whose captures
are all owned today.

## 5. Results against the requirements

| | Lean 4 / Koka | Current analysis, seeded at spawns | Least solution (4.1–4.3) | Plus 4.4–4.6 |
|---|---|---|---|---|
| S1–S4 | ✓ | ✓ | ✓ (Theorem A) | ✓ |
| K1 No overhead without sharing | ✗ test everywhere | ✓ outside the class | ✓ | ✓ ($\mathsf{T}$ positions only) |
| K2 Spawn independent of size | ✗ walk | ✓ | ✓ | ✓ |
| K3 No permanent penalty | ✗ sticky, no reuse | ✗ whole lifetime | ✓ parent; results stay $\mathsf{A}$ | ✓ |
| K4 Proportionality | Exact per object, except moved captures | ✗ spreads both ways across specializations | Exact up to context and SCCs | Exact except SCCs without spare bits |
| K5 Minimal contention | ✗ | ✗ | ✗ | ✓ escaping retains only |
| E1–E3 | runtime only | ✓ | ✓ linear, locally checkable | ✓ |
| E4 Code growth | none | none | none | mode specializations |
| E5 Wasm | ✓ via `Tasks` | ✓ via `Tasks` | ✓ via `Tasks` | ✓ via `Tasks` |

## 6. Consequences

1. **Spawn seeds propagate forward only.** Equality propagation stays only for
   host seeds. This needs:
   - helper plans keyed by (layout, mode), where a mode is a set of positions
     closed under going deeper, instead of a single flag;
   - Lemma 0 replacing design.md's requirement that every statement touching
     one allocation agrees.
2. **Specialize by mode** (4.5). The prototype shows this is required, not
   optional. When a helper shared with children does the parent's private
   work, forward propagation alone leaves that work atomic: 1,950 false
   atomics, against 2 with specialization.
3. **Seed only captures that siblings can share.** The may-alias test
   decides seeds, and the existing union-find is too coarse for it: two
   values that pass through one helper share a class, so moved data gets
   seeded. A points-to analysis that keeps call sites apart, used for this
   one decision, removes all of it in the prototype (166 false atomics to 0).
   It summarizes each procedure's result in terms of its parameters and
   fresh objects, which a caller names by its call site.
4. **Lend captures to children** (4.6) so that only escaping retains race.
   With borrow inference in the prototype, children that only read a shared
   tree make no racing updates at all.
5. **Edge modes** (4.4) only if a benchmark that path-copies shared trees in
   children shows atomic counts on its new nodes to be significant. In the
   prototype's path-copy scenario they take the static residual from 70
   false atomics to 0, testing a bit on 130 of 1,125 updates. Per-object
   marking leaves 23 and tests all 1,125. Per-object marking is never needed:
   everything it detects that the static solution misses is covered by edge
   modes, without walks or permanent marks.
6. **Host classes are now the main imprecision.** Of the false atomics left
   in the fuzzed programs, 90% come from host seeds' equality classes, which
   this design keeps as they are. Narrowing them is separate work.

## 7. Evidence

[`rc-atomicity/`](rc-atomicity/README.md) holds a model, proofs and a
prototype of this design:

- **Maude** (`maude/`). An executable model whose runtime detects count races
  directly, searched over every interleaving. The forward analysis is
  race-free on every scenario. Each deliberately wrong variant produces a
  race:
  - no atomics at all;
  - construction edges dropped;
  - host seeds propagated forward only.

  Serial schedules need no atomics (T1).
- **Lean** (`lean/`). `window_sound` mechanizes Theorem A for a window, and
  `reach_optimal` mechanizes Theorem B, in Lean 4 core with no `sorry`.
- **Prototype** (`proto/`). Every analysis, including dynamic Lean-style
  marking, runs on a language with calls, recursion and branches.
  - **Fuzzing.** Over 23,000 generated programs (1.7 million count
    updates), every analysis is sound, with and without lending, and each
    refinement (forward, mode specialization, call-site-sensitive seeds,
    edge modes) is contained in the one before, event by event.
  - **Cross-check.** On 117 generated programs, Python and Maude computed the
    same forward facts, and the race oracle agreed with Maude's exhaustive
    search on every one.

## References

- Feng and Leiserson, *Efficient Detection of Determinacy Races in Cilk Programs*, SPAA 1997
- O'Hearn, *Resources, Concurrency and Local Reasoning*, TCS 2007
- Bornat, Calcagno, O'Hearn and Parkinson, *Permission Accounting in Separation Logic*, POPL 2005
- Steensgaard, *Points-to Analysis in Almost Linear Time*, POPL 1996
- Andersen, *Program Analysis and Specialization for the C Programming Language*, PhD thesis, DIKU 1994
- Ullrich and de Moura, *Counting Immutable Beans*, IFL 2019
- Reinking, Xie, de Moura and Leijen, *Perceus*, PLDI 2021
- Lorenzen, White, Dolan, Eisenberg and Lindley, *Oxidizing OCaml with Modal Memory Management*, ICFP 2024 (modes for locality, uniqueness and linearity)
