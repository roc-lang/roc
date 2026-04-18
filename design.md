# Solved-Callable-Driven Specialization

## Summary

The ideal long-term design is:

- `lambdasolved` owns the only semantic truth about callables
- `monotype` is structural only
- `lambdamono` is the only specialization stage
- each specialization gets its own specialization-local solved world
- all unification for that specialization happens up front
- executable lowering runs only after that specialization-local solved world is stable
- later stages consume explicit facts and never reconstruct, repair, guess, infer, or recover missing information

This follows the useful part of `cor/lss`: specialization-local solved graphs, lowered after unification, with later stages kept dumb.

## Core Rule

For a given requested specialization:

1. clone the source function's solved type/body facts into a specialization-local solved graph
2. unify that local graph with the requested solved function type
3. after that, treat the solved graph as frozen for lowering purposes
4. lower executable types, captures, calls, and bridges only from that frozen solved graph

No stage may mix:

- mutable solved vars
- long-lived executable lowering caches
- later unifications
- repair bridges

That combination is architecturally unsound.

## Ownership By Stage

### `lambdasolved`

`lambdasolved` owns the only semantic callable facts:

- function type
- lambda-set membership
- capture types
- method-call function type
- call result type

No later stage may recover or reconstruct any of these from syntax, cached executable types, or partial call-shape bookkeeping.

### `monotype`

`monotype` should be structural only.

It may:

- lower tree shape
- preserve explicit checked facts
- preserve ordinary call and partial-application structure

It must not:

- perform specialization
- rebuild call chains
- choose specialized clones
- infer executable result structure from expected types

### `lambdamono`

`lambdamono` is the only specialization stage.

Its responsibilities are:

- clone specialization-local solved graphs
- unify them with requested solved types
- lower executable callables from those solved graphs
- choose direct vs indirect call form from solved callable facts
- emit explicit bridges where real representation changes are required

It must not:

- reuse executable-type caches across changing solved graphs
- repair wrong executable shapes after the fact
- infer bridges from late layout mismatches

## Specialization-Local Solved Worlds

Every specialization should operate on its own instantiated solved graph.

That means:

- fresh solved type clone for that specialization
- fresh type-instantiation cache for that specialization
- fresh executable-type cache for that specialization

These caches are only valid for the lifetime of that one specialization-local solved world.

This is the key architectural constraint:

**a cache keyed by solved type-variable identity is only sound if that type-variable's meaning is stable for the lifetime of the cache.**

So:

- specialization-local cache: good
- cross-specialization cache by raw solved vars: forbidden
- cache surviving later unifications on the same solved vars: forbidden

## Call Lowering

All call lowering must consume the solved callee type directly.

For a call, the lowering stage should read:

- solved callee type
- solved argument type
- solved result type
- solved lambda-set membership
- solved capture information

And from those explicit facts decide:

- direct call vs indirect call
- exact target specialization
- executable result type

It must not reconstruct call semantics from:

- source-variable chains
- cached executable expr types
- ad hoc call-shape builders
- later layout mismatches

## Method Calls

Method calls follow the same rule as ordinary calls.

The only extra method-specific step allowed is:

- resolve the target symbol explicitly, or
- thread the checked method function type explicitly

After that, method calls use the same ordinary call-lowering machinery.

There must not be separate reduced semantics for method calls.

## Captures

After `lambdasolved`, captures have one source of truth:

- solved lambda-set captures

Later phases must ask only:

> for function symbol `S`, in solved function type `T`, what captures does that lambda member have?

They must not read capture facts from:

- duplicated AST storage
- executable expr caches
- side caches
- reconstructed environment guesses

## Bridges

A bridge is an explicit compiler operation that converts a value from one already-known internal representation to another already-known internal representation.

It is not:

- a guess
- a fallback
- a heuristic
- a repair mechanism

It is just:

- source representation: known
- target representation: known
- conversion step: emitted explicitly

Examples include:

- nominal value to backing representation
- boxed value to unboxed value
- one list representation to another compatible list representation
- one callable representation to another, if the language/compiler explicitly requires that

In the ideal design:

- `lambdamono` decides whether a bridge is needed
- `lambdamono` emits an explicit bridge node
- IR preserves that node
- `FromIr` lowers only explicit bridges

`FromIr` must never discover a representation mismatch and then decide what to do about it.

If a non-bridge call result reaches `FromIr` with the wrong layout, that is a compiler bug.

## Partial Application

Partial application remains ordinary function semantics.

Semantically:

- applying some arguments returns another function

The important point is only this:

- later stages must read that fact from solved callable types
- they must not reinvent it by reconstructing chains from partial executable information

No additional abstraction is required beyond explicit solved callable facts.

## What This Forbids

This design forbids:

- late call reconstruction
- late executable-shape repair
- cache reuse across mutable solved worlds
- bridge inference in IR/LIR
- duplicate capture authorities
- specialization keys weaker than the full requested solved function type

## What This Enables

This design gives us:

- one semantic authority for callable facts
- one specialization stage
- specialization-local cache validity
- explicit bridges only where semantically required
- dumb later stages
- predictable architecture that matches the "consume explicit facts from earlier stages" rule

## One-Sentence Version

The perfect design is:

**for each specialization, build a fresh solved world, finish all unification there up front, then lower from that frozen solved world with specialization-local caches and explicit bridges, while every later stage remains dumb and consumes only those explicit facts.**
