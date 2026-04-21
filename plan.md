# Plan: Normal Lambda-Set Callables, Explicit Structural Equality

This file is a migration plan for getting from the current implementation to the target architecture discussed in `design.md`.

It is intentionally strict:

- no workarounds
- no fallbacks
- no heuristics
- no recovery/reconstruction in later stages
- exact callable information must travel through the normal callable representation, not a side channel

## Core Decisions

These are the design choices this plan assumes.

### 1. Exact callables use normal lambda-set machinery

We do **not** want a second long-term callable representation such as:

- `exact_symbol`
- exact-target side tables
- special callable payloads outside the normal solved callable representation

The normal representation is:

- a solved function type
- whose lambda-set contains exactly one member when the callable is exact

That is what `cor/lss` does for ordinary exact callables:

- `infer_fn` creates a `TFn(arg, lset, ret)`
- the `lset` is `LSet(SymbolMap.singleton lambda captures)`

So the target here is:

- exact callable identity must become an ordinary singleton lambda set in `lambdasolved`
- every later stage uses ordinary lambda-set machinery only

### 2. Structural equality is not an ordinary method call

The language still supports:

- user-defined `is_eq` methods on nominals
- implicit structural equality when no user-defined `is_eq` exists and the type supports equality

Those are different semantics.

So after the checker decides which one applies, that distinction must become explicit:

- real user method -> ordinary resolved method call
- implicit structural equality -> explicit `structural_eq` node

We do **not** want:

- `implicit_eq` as a permanent special method-call tag in later lowering stages
- later stages re-deciding whether `is_eq` is structural or method dispatch

### 3. Method resolution stays after solving and inside specialization

We do **not** want post-specialization method lookup.

We do want:

1. checker produces explicit callable information for method calls
2. specialization clones a local solved world
3. specialization unifies that local world with the method-call site facts
4. the refined solved callable fact becomes exact there
5. `lambdamono` reads the exact target directly from the refined solved callable fact

No attached-method index search should survive after that point.

### 4. `lambdamono` specializes from frozen solved worlds

For each specialization:

1. clone the relevant solved graph
2. unify it completely up front
3. freeze it
4. lower executable types and exprs from that frozen world only

That means:

- no `self.unify(...)` during executable lowering
- no cache invalidation because solved vars changed meaning mid-lowering
- no late bridge-as-repair

## Migration Plan

## Phase 1: Fix the current blocker correctly

Current blocker:

- exact callable identity is being lost at the monotype -> lambdasolved handoff
- later `lambdamono` then sees a non-exact method callable and wants to recover/search

The correct fix is **not** to invent a new permanent callable representation.

The correct fix is:

1. identify the earlier explicit exact-callee information that already exists for method calls and exact aliases
2. preserve that explicit information through instantiation
3. materialize it in `lambdasolved` as an ordinary singleton lambda set

Concretely:

- when instantiating a function type that is already known to denote one exact callable, `lambdasolved` must not drop that exactness to `freshUnbd()`
- instead, it must create the ordinary solved lambda-set value containing that one callable

Important constraint:

- this must be driven by explicit earlier-stage information
- not by later receiver-type search or “if it looks exact enough” logic

## Phase 2: Split structural equality from ordinary method calls

This is the first representation cleanup that should happen, because it simplifies everything else.

### Checker

Keep the current semantic policy:

- if a real `is_eq` method exists, resolve to that
- else if the type supports equality, resolve to structural equality
- else report an error

### Typed CIR

Add an explicit structural-equality expression.

Something in spirit like:

- `e_structural_eq { lhs, rhs }`

Then remove the current “implicit eq method call” side table/state:

- no `implicit_eq_method_calls`
- no `methodCallIsImplicitEq`

### Lowering stages

Propagate that explicit split:

- `method_call` remains only for real method dispatch
- `structural_eq` becomes its own node in monotype, monotype_lifted, and lambdasolved

Then `lambdamono` lowers:

- real method calls through ordinary method-call specialization
- structural equality through a dedicated structural-equality lowering path

This removes the current fake “method call, except not really” shape.

## Phase 3: Move exact callable transport onto normal lambda-set machinery

Once structural equality is no longer entangled with method-call lowering, make the callable path normal.

### `lambdasolved`

Make `lambdasolved` the only owner of callable truth after solving.

For exact callables:

- exactness must appear as a singleton lambda set
- not as a side tag on a later AST node
- not as a monotype-only callable payload

This should follow the `cor/lss` shape:

- function inference builds singleton lambda sets for exact defs
- call specialization consumes solved lambda sets directly

### Alias tracking

Port the `exact_callable_aliases` idea from `fix-currying`, but use it only as explicit propagation of exact callable identity into the solved callable world.

Use it for:

- direct value aliases of exact callables
- direct local aliases of exact callables
- transported method callable facts that are already known explicitly earlier

Do **not** use it as a recovery mechanism.

Its job is:

- preserve exact callable identity long enough for `lambdasolved` to materialize the right normal lambda set

## Phase 4: Port whole-wanted-function unification

Port the relevant `fix-currying` call-solving improvement:

- calls unify against one full wanted function type
- not step-by-step reconstructed arg/result chains

Apply that to:

- ordinary calls
- method calls
- type-method calls

The result should be:

- call semantics determined by one explicit callable shape
- fewer transported “step result” helper facts
- less reconstruction later

This should also let us delete:

- curried-step-specific call reconstruction paths
- extra call-shape bookkeeping that exists only because calls are being re-derived piecemeal

## Phase 5: Port N-ary function/call representation

Port the relevant `fix-currying` N-ary representation changes.

Target shape:

- function types use arg spans, not unary chaining
- function defs use arg spans
- calls use arg spans

This should be done through:

- monotype
- monotype_lifted
- lambdasolved
- lambdamono

The goal is:

- later stages stop rebuilding multi-arg calls from curried internal chains
- whole-wanted-function unification becomes the natural representation

Obsolete pieces to delete after this lands:

- curried call reconstruction helpers
- `CurriedFnShape`-style machinery
- stepwise arg/result bookkeeping that only exists because of unary internal representation

## Phase 6: Remove late method-target search

After exact callable identity is preserved properly in normal lambda-set form, remove the remaining late search helpers from `lambdamono`.

Things that should disappear:

- attached-method target search by receiver type
- tag-union-specific method target search
- nominal-backing-based target recovery
- any “find the method target from scraps” logic

What should remain:

- read the refined solved callable fact
- if it is exact, use that target
- if it is a multi-lambda set, branch over those explicit alternatives
- if it is erased, lower the erased path
- if it is none of the above, that is a compiler bug

## Phase 7: Finish the frozen-solved-world specialization model

Make `lambdamono` match the desired `cor/lss` shape fully.

### Specialization

For each specialization:

- fresh cloned solved graph
- fresh instantiation map
- fresh specialization-local type cache
- fresh specialization-local executable lowering cache

Then:

- unify once up front
- freeze
- lower only from that frozen world

### Delete mutable-world executable lowering

Remove:

- `self.unify(...)` during executable lowering
- cache invalidation hooks caused by mid-lowering unification
- any late executable repair path that exists because the solved world was still mutating

## Phase 8: Tighten bridges

Keep explicit bridge nodes, but only for real representation changes planned upstream.

Allowed bridge planning sites:

- nominal/backing conversions
- explicit box/erased boundaries
- aggregate boundaries with known representation differences
- explicit control-flow/function boundaries with known source vs target representation differences

Forbidden:

- bridge as generic “expected != actual, so repair it”
- late IR/LIR mismatch discovery that invents a bridge

`FromIr` should only lower explicit bridge nodes.

## Phase 9: Delete obsolete machinery

After the new architecture is in place, do a full deletion audit.

Delete all remnants of:

- `implicit_eq` as a later-stage method-call special case
- exact method target side channels
- curried call reconstruction
- partial call/result reconstruction helpers
- late method target search
- mutable-solved-world executable lowering
- bridge-as-repair

This phase is complete only when later stages are consuming explicit earlier-stage information directly and nothing else.

## Specific Non-Goals

This plan does **not** include:

- merging `fix-currying` wholesale
- inventing a second permanent exact-callable representation outside normal lambda sets
- leaving `implicit_eq` as a permanent special method-call kind
- any late recovery from missing exact callable information

## Final Validation Criteria

The design is only done when all of the following are true:

1. exact callables are represented through ordinary lambda-set machinery
2. structural equality is an explicit node, not a fake method-call subtype
3. real nominal `is_eq` methods still override structural equality
4. method resolution after solving is just reading refined solved callable information
5. `lambdamono` lowers from frozen specialization-local solved worlds
6. `FromIr` lowers only explicit bridges
7. no late search/recovery/reconstruction remains in later compilation stages

