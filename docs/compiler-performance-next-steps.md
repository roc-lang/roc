# Proposed extensions to compiler performance work

## Status

These are design proposals, not implemented optimizations or accepted changes to
the language rules. The final one-flag-removed measurements completed without
compiler source changes; see `compiler-performance-flag-ablations.md`.
The measured current suite and its limitations are documented in
`compiler-performance-rollout.md` and `compiler-performance-measurements.md`.

The next opportunities are more general late nominal unification, replayable
literal obligations, and narrower higher-order cache exclusions. They should be
separate reviewed changes, not reasons to remove current safety checks.

Caching scope is now explicitly the normal specializing pipeline. Boxy
(`--specialize=no`) may always miss for this work; supporting its descriptor
sidecars or shared generic worker cache is deferred. Existing Boxy correctness
fixes for sparse nominal construction are separate from this caching scope.

## 1. Bare tags whose nominal type is discovered later

### What the existing change does not cover

A bare constructor with a direct nominal expectation can use the selected-tag
path. With payload-only contextual guidance, tag projection can avoid copying
unrelated expected alternatives. Neither mechanism generally optimizes a bare
constructor that was inferred before its eventual nominal use.

For example:

```roc
Choice(a) := [None, Some(a), Message(Str)]

make_some = |value| Some(value)

result : Choice(U64)
result = make_some(42)
```

The generic helper initially returns an open structural row, schematically
`[Some(a) | r]`. A particular use can later relate an instance of that row to
`Choice(U64)`. The generic helper must remain usable in other valid contexts;
it must not be globally rewritten to return `Choice`.

Today [unifyTagUnionWithNominal](../src/check/unify.zig#L1926-L2023) opens the nominal's complete backing before
performing ordinary row unification. The open extension can acquire the remaining
variants. That work is not eliminated merely by recognizing the syntax of the
original `Some`.

### Recommendation: lazy nominal instantiation with an exact row complement

Optimize the general structural-row/nominal relation, rather than rely on
constructor syntax surviving until the eventual use.

When the example meets `Choice(U64)`:

1. Check opacity and declaration validity exactly as today.
2. Read the declaration's labels and arities without copying all payload graphs.
3. Instantiate and relate the visible `Some` payload under the application's
   actual parameter bindings.
4. Preserve `r` as the exact remaining row, represented symbolically as
   `Choice(U64) minus {Some}` instead of eagerly copying `None` and `Message`.
5. Publish the nominal at this successful use, without changing the generic
   helper's scheme.

This is not permission to close `r` to empty. Another consumer can observe or
constrain that extension, so the complement must mean exactly the same row the
ordinary unifier would have produced.

The proposed solver-owned representation has:

- opening-instance identity, declaration identity, and actual parameter bindings;
- one owned instantiation/substitution map for that opening;
- an exact row fragment: excluded labels plus the residual extension;
- mutable solver cells for demanded payloads, allocated once per template root
  within that application.

Different variants within one opening that share a formal or private template
variable must see the same instantiated cell. Independent openings must not share
private unknowns merely because their declaration and arguments compare equal.
Recursive reuse within an opening and any reuse across calls need separate
ownership rules. The declaration remains immutable. Existing readonly analysis
views illustrate scoped identity, but their synthetic IDs must not be passed to
ordinary mutable unification.

Row operations read this representation through one explicit interface. Label
partitioning can inspect the complete constructor universe without instantiating
every payload. A later constraint on `Message` demands its payload through the
same application map. Operations requiring all payloads still pay for that work;
it is deferred only when it is semantically unnecessary.

All instantiation obligations that are required today must remain required,
including obligations not discoverable from the visible tag alone. A lazy
payload is not a license to omit constraints or method/evidence requirements.
Generalization, copying, occurs checks, recursive applications, diagnostics,
artifact serialization, hashing, and downstream representation planning need
explicit support for the new representation. A format requiring materialized
rows may use a declared stage boundary; it must not reconstruct missing facts
through a heuristic.

### Materialization contract and review requirements

The governing equivalence must be: demanding a delayed fragment exposes the same
equalities, alternatives, obligations, and checked result as eager instantiation
followed by ordinary row unification. Demand order cannot change those facts.
This is a design obligation, not a proof already supplied by this proposal.

The independent review supports the direction but identifies these decisions
that must precede implementation:

- Initial scope should be explicitly closed nominal backings. An open backing
  `[A | s]` has complement `s`, not empty; extending the lazy rule to it requires
  the full two-open-row algebra.
- Row gathering must retain duplicate-label payload relations, not just operate
  on a set of names. Multiple visible tags and complement-to-complement relations
  must preserve every shared payload equality.
- Semantic traversals must see latent graph edges. Occurs checks, rank propagation,
  and generalization cannot inspect only payloads that have been allocated.
  A cell demanded later must retain its opening's effective rank/region history.
- Scheme instantiation must freshen private/quantified state while preserving
  sharing within the copied instance, including existing associated-reference
  substitutions.
- Speculative rollback must restore lazy maps, demands, obligations, and solver
  cells together. Preserve existing error-recovery behavior; do not assume all
  ordinary failed unifications are globally atomic.
- Diagnostics must inspect complete expected shapes without leaking diagnostic
  demands or mutations into surviving inference state.
- Decide publication explicitly. Keeping lazy nodes checker-internal and
  materializing at the checked-artifact boundary is a simpler first contract,
  but may sacrifice the gain. Retaining them requires a persistent format and
  consumer contract. Measure the complete path before choosing.

### Why not just remember that the row came from `Some`?

Deferred producer provenance could accelerate an additional narrow class of
owned constructions. It does not by itself justify discarding the extension
after the row has been generalized, joined with another alternative, or shared
with another constraint. Lazy exact complements address that general case
without requiring a guess that nobody else uses the tail.

Likewise, caching one mutable full backing per nominal declaration would share
application-private inference state and is not a sound replacement for copying.

### Required acceptance cases

- Bare single tags without annotations, first related to a nominal at a call.
- Generalized helpers used at distinct nominal applications and structural rows.
- Multi-tag joins, shared extension variables, and later tail constraints.
- Closed structural rows that must reject missing or extra variants.
- Generic substitutions, private unknowns, aliases, recursion and scope resets.
- Opacity, arity/payload errors, complete diagnostics, and failed-unification
  state preservation.
- Equivalent checked artifacts and runtime representations.
- Whole-compiler timing, memory, and artifact size on ordinary bare-tag-heavy
  code, not only qualified generated constructors.

The authoritative mutation rule and serialization contract must be declared
before implementing this solver representation. This is broader than a small
addition to the existing constructor fast path.

## 2. Reusing code without skipping custom literal validation

This section is an unimplemented proposal, now subject to a narrower alternative:
reuse already-finalized specialization code and preserve/replay its validation
outcome and diagnostics where checked artifacts do not already retain them.
Rejected literals report errors but compilation proceeds with a runtime crash
path. They do not inherently require converter re-execution on a cache hit.
The current excluded pack shape can be an unevaluated conditional conversion,
not a previously established failed result. See `custom-literal-replay-contract.md`
for that distinction before treating the larger manifest below as necessary.

### Current omission

Lowering a concrete specialization can register a custom literal conversion
that must execute during compilation. An early body-cache hit can skip that
registration. The pack therefore withholds entries whose reachable code carries
such deferred conversions. An imported but unused generic literal must remain
dormant; a later runtime-only call that specializes it must still validate it.

### Recommendation: checked obligation templates and specialization manifests

The checked artifact should publish deferred literal obligations alongside their
owning procedure templates. Each records the literal facts, checked expression
identity, result type, selected conversion dispatch, lexical ownership, and
evidence requirements. Already-closed checked roots keep their existing path.

At specialization reservation, instantiate those templates from the same type,
callable, codec, and dictionary evidence that define the specialization. The
cache hit returns compiled code together with a derived obligation manifest:

```text
specialization
  -> ordered conversion obligations
  -> typed completed-value bindings consumed by the cached code
```

On a hit, the compiler registers the conversion roots without visiting the
entire source body, evaluates them through normal finalization, reports failures,
and binds successful values into the current program. The artifact can then
reuse code that reads those declared bindings. Missing or unclosed evidence is
not inferred from machine code; that class remains ineligible.

The checked artifact is the semantic source of truth. A pack may carry the
derived plan, versioned and validated against that source, but optional pack
availability must not change whether a source literal needs validation.

Keys must distinguish literal bytes/kind, source artifact and expression,
owning specialization, actual result type, conversion implementation, evidence,
nested callable/codec dependencies, compiler version, and target-relevant ABI.
Persistent identities cannot use raw process pointers or unqualified local IDs.
Regions are resolved from the active artifact for diagnostics, not reused from
an old native-code location table.

Initially cache the code and replay the obligations, not their results across
programs. A separate typed CTFE-result cache can later avoid repeated conversion
execution if it preserves successful values, failures, debug replay, dependency
identity and portable callback/data graphs. A successful object hit is not proof
that a previous conversion result is valid here.

### Important implementation boundary

The plan must be closed before accepting an early hit. If literal resolution or
the resulting callable/layout can only be established by later body analysis,
that class needs an explicit earlier producer plan or remains unsupported.
Cached code must have a stable completed-value inlet ABI; a value that changes
specialization decisions cannot be silently substituted beneath a fixed object.

The current compile-time branch-observation gate remains independent. Restoring
literal obligations does not prove that cached code records required branch
observations.

### Required acceptance cases

Successful/rejected conversions cold and warm; dormant generic imports; calls
reachable only at runtime; nested locals and captured evidence; multiple literals;
the same literal at different types/dictionaries; moved source locations; debug
replay; changed converter/dependency invalidation; and callable-containing
conversion results. Preserve the existing `literal_root_rejected` workflow.

The exact completed-value inlet/linking ABI and plan-only instantiation entry
point remain implementation decisions, not claims already established.

## 3. Less restrictive higher-order cache reuse

### Why the existing guard is conservative

The requested arrow type does not identify which callbacks can flow through it.
Different `U64 -> U64` functions can have different code and captures, affecting
specialization. The current early lookup precedes later callable-set and closure
analysis. It therefore excludes function-containing argument/result shapes,
including through nominal backings.

### Recommendation: admit explicit closed callable proofs incrementally

Revised first step: **wait until the specializing pipeline has the complete
callable/specialization identity, then look up the object at that point**. Do not
add a duplicate earlier analysis merely to force an early hit. Keep the existing
early path for ordinary eligible signatures.

The current Direct LIR path computes a full procedure identity but still looks
up the earlier template's specialization key; the early signature guard can
leave higher-order templates without that key. Waiting therefore needs a coherent
later key/publication path, not merely moving an existing lookup unchanged.
The matching pack entries must use the same complete stable identity.

This can skip only work after that identity is established; it cannot avoid
earlier analysis already performed to obtain it. Measure the remaining cost
before deciding whether separating identity analysis from body construction is
worthwhile. The earlier-proof approach below is an optional subsequent extension,
not a prerequisite for higher-order caching.

Start with zero-capture imported/top-level callbacks whose exact target is known
before body lowering. An earlier producer must publish a durable callable
contract, not a consumer guess from a singleton-looking type:

- stable source/template identity for each possible target;
- exact finite target topology, including function-bearing container positions;
- callable ABI and ordered capture layout/representation;
- evidence/codec requirements and nominal provenance;
- linkable dependency closure.

Extend specialization identity with that contract and compare its exact topology
after digest lookup, as existing evidence handling does. Local lambda-set/type
IDs and equal arrow layouts are not persistent identities.

Capture layout and capture values are different. If cached code receives an
environment at runtime, calls with different numeric captures can share its
code when the environment ABI is identical. If values or callback constants are
baked into the artifact, their stable identities and relocatable data must also
be part of the contract. Otherwise withhold the entry.

Closed callable facts remove one semantic exclusion. The pack writer still has
to carry or reconnect the actual dependency closure. Literal validation and
branch observation remain separate checks.

### Broader alternative

This alternative is deferred; it is not in the agreed current caching scope.

Truly erased generic higher-order workers could share code across arbitrary
callbacks through a fixed closure ABI: code/adapter pointer, environment, argument
and result descriptors, evidence, and ownership operations. That needs a portable
descriptor/linking contract and may trade compile-time sharing for runtime
indirection. Existing runtime support does not establish cache portability.
This is a larger architectural change than admitting proven closed callbacks.

Reuse existing later callable-analysis machinery where possible, but extract
an explicit earlier analysis rather than pretending its downstream result
already exists at the early lookup.

### Required acceptance cases

Same arrow/different callbacks; same code/different captures; returned functions;
nominal/container function fields; recursive target sets; reversed caller build
order; cold/warm equivalence; and imported versus application-local dependencies.
Unproved flows remain excluded. No performance gain or universal eligibility
follows from this source-only proposal.

## Suggested order

1. Use the completed final one-flag-removed measurements to choose the rollout
   configuration; do not present neutral components as demonstrated speed wins.
2. Design and test general late nominal unification: it targets the common bare
   tag case rather than another qualified-constructor niche.
3. Publish replayable literal obligations before relaxing the pack restriction.
4. Admit the smallest proven higher-order callback class, then widen only with
   explicit callable and portability contracts.

These extensions do not claim to fix issue #12006's quadratic Lambda Mono
specializations. That requires its own reproduction and specialization-identity
investigation.
