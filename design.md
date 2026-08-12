# Roc Post-Check Compiler Design

This document is the authoritative design for the compiler stages that run after
checking has completed and before backend code generation begins.

The design is deliberately narrow. It keeps the existing checked boundary:

- checked CIR
- the checked type store
- checked module caching
- all user-facing checking and static-dispatch reporting

It also keeps the existing LIR boundary:

- statement-only LIR
- explicit LIR ARC insertion
- backend, interpreter, LirImage, and glue consumers of LIR

Keeping the LIR boundary means preserving the public LIR contract, not keeping
dependencies on a second post-check representation. Hosted ABI metadata,
platform metadata, symbols, literal ids, layout stores, and LIR stores live in
neutral modules owned by their actual consumers.

Everything in between those boundaries is selected by the runtime lowering
strategy for the current compilation. The strategy is a post-check choice and
never changes checked module output.

The lambda-set-specializing strategy is called `.lss` internally. It is the
Cor-style typed IR pipeline:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> optional SpecConstr
  -> Lambda Solved IR
  -> solved inline plan
  -> direct Solved-to-LIR decisions
  -> LIR
  -> ARC insertion
  -> backend, interpreter, or LirImage
```

The boxing strategy is called `.boxy` internally. It skips Monotype,
Monotype Lifted, Lambda Solved, and Lambda Mono:

```text
checked modules
  -> boxy checked-to-LIR lowering
  -> LIR
  -> ARC insertion
  -> backend, interpreter, or LirImage
```

`.boxy` does not construct or consume lambda sets. It lowers checked CIR plus
checked types directly to LIR by representing polymorphic value positions as
ordinary Roc box pointers, representing closures as boxed erased callables, and
passing explicit runtime descriptors and dictionaries as hidden Roc-internal
arguments or captures. The value shapes that can cross the host ABI remain
exactly the source ABI shapes.

There is no separate MIR layer in either strategy. There is no separate stored
layout IR between a strategy-specific lowerer and LIR. Layout selection is owned
by the selected LIR builder. In `.lss`, Lambda Mono is represented by explicit
callable and procedure decision tables consumed by direct LIR lowering, not by a
second stored expression, pattern, and statement tree. In `.boxy`, checked CIR
is consumed directly with explicit boxy representation plans owned by that
lowerer.

## Core Principles

Compiler stages after parsing and error reporting must not use workarounds,
fallbacks, heuristics, or best-effort reconstruction.

Every stage after checking consumes explicit data produced by earlier stages.
If a stage needs checked data that was not produced, the producer is
incomplete. A consumer must not recover missing data from source syntax, names,
body scans, display strings, runtime bytes, object symbols, backend state, or
incidental data structure shape.

All user-facing failures are reported during checking at the latest. Checking is
not complete until type checking, static-dispatch finalization, platform/app
relation output, compile-time constant evaluation, and checked module
output have all completed. After a checked module is output, every
violated assumption is a compiler bug:

```text
debug build: debug-only assertion
release build: unreachable
```

Post-check stages do not return user-facing checking errors. They do not emit
fallback code. They do not silently repair missing data. They do not add
release-build runtime checks for compiler invariants.

Checked identity and runtime encoding are separate data. A stable id, checked
id, symbol, type variable, procedure reference, callable member, or source row
id may identify what a value means. It must not also be treated as the integer
tag, variant slot, byte offset, ABI register class, object symbol, or memory
layout used to represent that value. The stage that commits a runtime encoding
outputs the explicit mapping from checked ids to runtime encodings, and all
later stages consume that mapping.

Identity provenance follows meaning provenance. An identity may be derived
from module content—the module name, the source bytes, and the identities
of imports—only for definitions whose entire meaning is determined by that
content. A binding whose meaning is partly supplied from outside the compiled
program is identified by its role at that outside boundary instead. Hosted
functions receive their implementations from the host, and `provides`
entrypoints are called by the host, both keyed by the symbol strings in the
platform header; their identities are those symbol strings and declaration
slots, never a content hash of a declaring module. Two hosted declarations
are two distinct identities even when their declaring modules are
byte-identical, and no deduplication, specialization, or merging step may
collapse two externally-bound identities into one.

### Dense IDs and structural keys

Compiler-owned identity domains prefer dense, store-local integer IDs. The
producer that owns an append-only table assigns its IDs densely from zero, or
provides an explicit bijection to a dense ordinal when the ID's raw bits encode
multiple disjoint namespaces. An identity with this contract is named `...Id`.
The integer value of an `Id` is meaningful only in the scope of its owning
store; it is not a stable serialization, object-file, or cross-process
identity unless that store and its mapping are carried across the same
boundary.

Per-ID data is a parallel column on the owning store, or a
`collections.DenseMap` when the column is dynamic or scoped. A short-lived
scope over a larger ID domain uses a paged, reusable, epoch-based, or explicitly
remapped dense column so that clearing and iteration remain proportional to the
live scope. `DenseMap`'s sparse pages contain only ID-to-dense-position rows;
its values occupy a compact live-entry column, so opening a sparse page never
initializes a page of potentially large values. The size of the owning ID domain
is not a reason to hash an ID. Direct columns avoid hashing, table growth,
repeated key storage, allocator traffic, and duplicate per-consumer indexing
work.

The suffix `...Key` is reserved for structural or composite identity for which
a dense owner-relative ID cannot preserve the required identity. Examples
include identities which must remain stable before or across serialization,
cache, object-file, or process boundaries where no owning ID table accompanies
them. Prefer assigning or interning a dense `Id` at the producer boundary and
passing that ID downstream. Use a `Key` only when such a dense ID cannot work,
not merely to avoid maintaining the owning table, and never rename an `Id` to
`Key` merely to permit hashing.

The Zig source lint in `ci/zig_lints.zig` rejects a `HashMap` whose first type
argument is named `...Id` (including qualified and multiline type names). This
lint makes the naming contract mechanically useful: hashing an ID hides
unnecessary work and often means that multiple consumers are rebuilding an
index already represented by the producer's dense domain. Replace such a map
with an owning-store column or `collections.DenseMap`. If the key genuinely
requires structural identity, name it `...Key` only after establishing why a
dense ID cannot represent it; changing the name solely to silence the lint
violates this invariant.

Backends do not reason about reference counting. They lower and execute the
explicit LIR `incref`, `decref`, and `free` statements emitted before backend
code generation. Each explicit RC statement carries the concrete RC helper
selected by LIR ARC insertion. Consumers may lazily cache code or interpreter
execution plans for that helper, but they must not select a different helper
from local layout data. Reference-counting policy belongs to LIR ARC insertion.

Recursive walks over post-check types and values must have an explicit
termination argument. A structure reachable after checking can be
self-referential—a recursive nominal's backing, or the fixpoint value of a
recursively-constructed chain (an iterator wrapped around itself a runtime
number of times)—so "this walk terminates" is an assumption, not a property,
unless the walk traverses a proven acyclic structure, detects graph cycles, or
carries explicit proof fuel. Proof fuel is not rewrite evidence. Every
fuelled query returns a typed result that distinguishes `proven`, `disproven`,
and `unknown_budget_exhausted`, and only `proven` may authorize a rewrite.
Exhaustion therefore retains the ordinary exact IR; it is never cached as
`disproven` and never selects a guessed runtime
representation.

Representation finiteness is different from proof-query termination. Monotype
bounds minted iterator identities at the single graph-owned construction choke
point (`generatedIteratorNode` plus graph finalization); crossing that declared
type-universe boundary produces the explicit `forced_dynamic` representation.
SpecConstr's shape, substitution, structural-work, and constructor-size
queries instead use typed proof exhaustion solely to decline an optional
rewrite. Code-growth admission is likewise separate from rewrite-legality proof: a
growth limit may retain the ordinary shared control-flow form after a rewrite
has been proven legal, but it cannot change a proof result or choose a runtime
encoding.

Cycles in a constructor-specialization `Value` graph form through a nominal
value's backing, a static-data candidate's runtime value, or a callable capture
that reaches either edge. The size, substitution, shape, and reusability
queries spend one shared proof-fuel value per query and preserve exhaustion in
their result types. Constructor-size arithmetic also detects overflow instead
of turning it into an apparent exact size. When an inline argument's finite
size cannot be proven, the inliner binds a plain clone of its source expression;
when static matching exhausts its proof fuel, it retains the runtime match. The
value matchers (`bindPatToValue`, `bindPatToMatchValue`, `bindPatToFlowValue`),
the field, item, and tag readers (`fieldFromValue`, `itemFromValue`,
`tagFromValue`, `recordFromValue`, `tupleFromValue`), and `materialize` each
count the pointer edges they follow against a shared strip cap; the matchers
retain a runtime match on exhaustion, while those readers and
`materialize`—which only ever run on values already proven acyclic by the
rules above—treat reaching the cap as a compiler bug. The shape-driven walks (`valueFromShapeArgs`,
`appendExprsFromValue`, `supplyLoopSlotLeaves`, `shapeMatchesValue`, `shapeEql`)
carry no budget: `Shape` trees are finite and acyclic by construction—they are
produced only by the budgeted derivations and a nominal shape's backing is a
fresh allocation, never a back-reference—so those walks terminate on the
structure alone.

## Checking Effects And Const Roots

Checking owns Roc effect validation, compile-time evaluation eligibility, and
compile-time root selection. These are checked-stage responsibilities, not
post-check repairs. The checker must finish with explicit outputs for function
effect kinds, top-level effect errors, effectful `expect` errors, compile-time
diagnostics, and selected compile-time roots. Later stages consume those
outputs directly.

The pre-check CIR producer outputs CIR and checked identity inputs; it does not
own compile-time root selection. Root selection happens during checking because
checking already walks every expression, resolves local identity, computes
types, validates function effects, and receives static-dispatch results. The
checker must not perform a later whole-module expression walk merely to decide
which expressions are roots, and later stages must not recreate those answers.

The question "can this expression be evaluated at compile time?" depends only
on checked data dependency, checked control reachability, and effectfulness. It
does not depend on whether a call was direct or static-dispatch syntax, whether
an expression is a leaf, whether a value was written inline or named at the top
level, or whether the expression contains `crash`, `dbg`, or `expect`. Those
constructs are compile-time observable, and evaluating them at compile time is
required when the surrounding expression has no runtime data dependency, no
runtime control dependency, and no effectful call.

Control reachability is checked data, not source-shape guessing. An expression
can be a standalone compile-time root only when the source meaning evaluates it
unconditionally whenever the root is needed. Branch bodies, match
guards, and match branch values are control-dependent on the enclosing
conditional or match. They may contribute summaries to an enclosing `if` or
`match` root, but they must not add independent selected roots while their
enclosing control decision can be made at runtime. Otherwise an untaken branch
containing `crash`, `dbg`, or `expect` would run during `roc check`, which
would change the program's observable behavior. If the whole enclosing control
expression is compile-time-known and effect-free, the enclosing expression may
be selected as the root and the evaluator follows the same branch choices as
the source program.

Non-local control-transfer expressions such as `return` and `break` are not
standalone value roots and cannot cover child candidates by themselves. Their
payloads may still contribute to an enclosing eligible root or be selected as
ordinary child expressions when they are reached through checked control data.
Making the control-transfer expression itself a root would require an explicit
checked continuation representation; until that exists, selecting it as a
stored constant is a compiler bug, not an optimization choice.

An effectful call is one of:

- a direct call to a checked effectful function
- a call through a function-typed value whose checked function type is effectful
- a static-dispatch call whose selected implementation is checked effectful

Creating a function value is not an effectful call, even when that function's
body is effectful. The effect propagates only when the function value is called.
Negative effect answers are not durable until the relevant slot has finalized;
static dispatch can still turn an apparently pure call site into an effectful
call before checked output is produced.

### Effect Slots

Roc effect propagation is a directed dataflow problem over function bodies and
call sites. It must not be represented by one early boolean that is finalized
before static dispatch has resolved. The checker maintains sparse effect slots
for the places where effectfulness is part of the checked result:

- function and lambda bodies
- top-level value right-hand sides
- `expect` bodies
- compile-time root candidates whose effectfulness may depend on delayed
  dispatch

An effect slot becomes effectful when it contains a direct call to an effectful
function, when a delayed static-dispatch call watched by the slot resolves to an
effectful function, or when it calls another slot that is effectful. Ordinary
calls add directed dependencies from caller slot to callee slot. Static-dispatch
calls add watcher entries from the dispatch function variable to the active
slot. When static-dispatch resolution later proves the selected method is
effectful, the watcher marks or connects the owning slot before any checked
output is finalized.

Effect dependencies are directed. A caller depending on a callee must not be
represented as equality. Strongly connected recursive groups may be condensed
for solving, but unrelated caller and callee slots must remain one-way
dependencies.

Effects are not inferred from source spelling alone. A `!` name contributes to
identifier parsing and annotations, but the checked source of truth is the
resolved function type and dispatch result. A method call whose syntax appears
inside a pure-looking expression can still make that expression effectful after
dispatch resolution. Conversely, `crash`, `dbg`, and `expect` are not real
effectful calls. They must never be used as reasons to reject a compile-time
root.

Effect finalization runs after ordinary type constraints, literal defaulting,
and static-dispatch constraints for the relevant boundary have settled. It
computes final slot effectfulness with directed graph propagation. After
finalization, the checker uses the slot results to select `fn_pure`,
`fn_effectful`, or the equivalent checked function kind, to report invalid pure
annotations, to report effectful top-level values, and to report effectful
`expect` bodies. Checked module output must not contain unresolved effect
kinds.

The effect solver may cache positive effectfulness immediately. It must not
treat an unresolved negative answer as final while dispatch watchers or callee
slots can still change. Recursive groups are solved by directed propagation:
strongly connected groups can be condensed, marked effectful if any member is
effectful, and then propagated to callers. Ordinary caller-to-callee edges must
remain one-way.

Every static-dispatch call that is not resolved when the expression frame
finishes is represented by explicit checked state. The active effect slot owns
the watcher for that dispatch function variable. If the same expression is also
a compile-time root candidate, the root candidate records that it is waiting on
the same slot. When the dispatch result arrives, effect finalization updates the
slot once, and both the function-effect answer and the root-selection answer
consume that finalized slot. Root selection must not infer delayed dispatch by
re-reading syntax or by searching for unresolved method names.

### Root Selection During Checking

Compile-time root selection uses the same checker traversal that already walks
checked CIR expressions. There is no separate root-selection walk over every
expression. While checking an expression, the checker returns a small
transient summary to its parent:

```text
runtime dependency status
control reachability status
effect slot or delayed-effect status when needed
candidate stack interval owned by this expression frame
```

The summary is stack-local for ordinary nested expressions. The checker stores
only data needed after the current expression finishes: summaries for bindings
that later lookups may read, effect slots and dispatch watchers, tentative root
candidates, and final selected roots. It must not allocate a permanent
per-expression table merely to answer root eligibility.

Runtime dependency is computed bottom-up from checked CIR identity. Lambda
arguments, match-bound values, loop-bound values, mutable variables, and
reassignments are runtime-dependent. Immutable local definitions store the
summary of their right-hand side; later local lookups consume that stored
summary. Top-level checked values and imported checked values are checked
binding identities at the use site. Looking up a module-level binding is
compile-time-known as a reference to that checked binding; the initializer's
own evaluation, diagnostics, reachability, and static storage are handled by
the module-level checked outputs, not by replaying the initializer summary into
each lookup expression. Parent expressions combine child summaries directly.

The expression summary is not a second effect system. Effect slots remain the
owner of effectfulness. The expression summary only says whether the expression
is already known effect-free, already known effectful, or waiting on an effect
slot that can still be marked by delayed dispatch or callee propagation.

An expression that already produced a checking problem is poisoned for
compile-time root selection. Poison is not runtime dependency and it is not an
effect. It only prevents an erroneous parent value from becoming a selected
root while preserving the original diagnostic ownership, so a bad child reports
once instead of being hidden by hoisting or reported again by a parent root.
Static-dispatch failures, type errors, and other checker-owned problems must
feed this poison result explicitly through the same expression summary path.
Poison is local to the expression or dependency region that owns the checking
problem. It propagates only through explicit checked dependencies, such as a
lookup of an erroneous local or top-level value. It must never become a module,
package, or program flag. A checked module or checked program may contain
user-facing diagnostics and still produce hoisted roots for every independent
expression whose own dependency region is resolved and otherwise eligible. This
is required for Roc's recover-and-continue behavior: `roc check`, tests, and
program execution must keep doing all valid work that does not depend on the
erroneous code path.
No downstream compile-time-evaluation step may use a CheckedModule's
nonempty diagnostic list as a reason to skip independent roots; it must consume
the explicit root list and the per-root poisoned/dependency state produced by
checking.
Compiler implementation gaps are not poison. Once checking has accepted an
eligible expression, failure to evaluate, store, restore, or emit it correctly
is a compiler bug with a regression test, not a reason to demote the expression
from compile-time evaluation.

Root selection keeps maximal eligible expressions. Each expression frame
records the root-candidate stack length at entry. If the expression finishes as
compile-time-known, unconditionally reachable, and effect-free, it removes
child candidates added inside the frame and adds itself. If the expression is
not eligible because of runtime data dependency or effectfulness, its eligible
unconditionally reached child candidates remain. If the expression is not
eligible because it is control-dependent on a runtime branch or match decision,
children inside that conditional region do not add standalone selected roots;
they are evaluated only if an enclosing eligible control expression is
selected. If the expression has delayed effect sources, the checker stores a
tentative parent over its child candidates; effect finalization later keeps the
parent and drops the children when the parent resolves effect-free, or drops
the parent and keeps the children when the parent resolves effectful. This is
the only parent-child replacement rule. There are no special cases for leaves,
strings, numbers, empty lists, records, loops, or other data-expression shapes.
Control-transfer expressions and conditionally evaluated branch regions are
handled by explicit checked control reachability, not by pruning arbitrary
source shapes.

Delayed parents form intervals over the candidate stack, not source-tree
queries. Nested delayed parents finalize from explicit interval ownership: when
an outer delayed parent is kept, every child candidate in its interval is
removed, including delayed children. When an outer delayed parent is discarded,
the candidates in its interval keep their own finalized results. This preserves
the maximal-root rule without a second walk and without special pruning rules.

Root selection must be independent of how the source was arranged. A named
top-level value, a closed immutable local value, and an equivalent inline
expression must produce equivalent selected roots once checked dependencies,
checked control reachability, and effects are the same. Selecting a parent root
is the only reason to discard an already selected child root from an
unconditionally evaluated region; rejecting a parent for runtime data
dependency or effectfulness must preserve those eligible children. A
runtime-controlled branch body is different: its contents are not
unconditionally evaluated, so they cannot be selected independently without
explicit checked proof that doing so preserves compile-time observables.

### Checker Implementation Contract

The checker has one authoritative state for effect propagation and compile-time
root selection. This state is owned by checking, updated during the existing
`checkExpr` traversal, finalized before checked module output, and exported as
explicit checked data. Canonicalization may produce stable identities and source
structure, but it must not select compile-time roots or decide final
effectfulness. Post-check stages may consume checked roots and evaluated
constants, but they must not repair or reinterpret root eligibility.

Checking a module-level definition as a dependency is not a child expression of
the lookup that forced it. If a forward reference causes a different
module-level definition to be checked while an expression frame is active, the
checker detaches the root-frame and candidate stacks for that definition. The
definition still writes to the module's shared selected-root, delayed-root,
known-binding, effect-slot, and checked-output state, but its transient
expression frames must not bubble runtime dependency, child candidates, or
last-expression metadata into the forcing lookup. This keeps the result
independent of whether an equivalent top-level constant was checked before or
after the use site.

Each expression frame records the current root-candidate stack length when the
frame begins. The frame receives child expression summaries as checking
progresses and returns one transient summary to its parent. The summary records
only the data needed by the parent: runtime data dependency, checked control
reachability, and effect state. Ordinary summaries are stack-local. A summary is
stored past the current expression only when a later checked local lookup needs
it, when an effect slot or dispatch watcher must finalize later, or when a
tentative root candidate has been selected.

Effect propagation uses directed slots and edges:

- checking a function body, top-level value, `expect` body, or delayed root
  candidate creates an effect slot when that boundary needs a checked effect
  answer
- a direct call to a known effectful function marks the active slot effectful
- a direct call to a local function with its own slot adds a caller-to-callee
  edge
- a call through a function-typed value consumes the checked function effect
  kind
- an unresolved static-dispatch call records a watcher from the dispatch
  variable to the active slot
- dispatch resolution updates the watched slot from the selected checked method
  effect

Those edges are dependencies, not equality. Recursive groups may be condensed
while solving, but unrelated caller and callee slots must remain one-way. A
slot whose callees and dispatch watchers have not finalized cannot be reported
as definitely pure. Finalization runs after the relevant ordinary type,
literal-defaulting, and static-dispatch constraints have settled; checked
module output must not contain unresolved effect slots or unresolved root
candidates.

Root selection uses the same expression frames. When a frame finishes as
compile-time-known, unconditionally reached, and effect-free, it replaces the
candidate interval added by its children with the parent candidate. When a
frame finishes as runtime-dependent or effectful, it leaves eligible
unconditionally reached children in place. When a frame is in a branch body,
match guard, or match branch value controlled by a runtime decision, it does
not add selected roots from that conditional region. The enclosing
`if` or `match` may still be selected if the whole control expression becomes
compile-time-known and effect-free.

Delayed root candidates are tied to effect slots. The candidate stores the
owned interval of child candidates and is finalized from the slot result. If
the slot resolves effect-free, the parent candidate is kept and the child
interval is removed. If the slot resolves effectful, the parent candidate is
discarded and finalized children remain. This interval rule is the only
subsumption rule; implementation must not add leaf filters, observable-effect
filters, or source-shape pruning.

Compile-time observables are not effect blockers. `crash`, `dbg`, and `expect`
must be represented as ordinary checked expressions for root selection. When
their enclosing selected root is evaluated during checking, they run and report
their diagnostics. An untaken runtime-controlled branch containing those
constructs must not be independently selected, because that would change source
behavior by running compile-time observables that the program would not
evaluate.

### Type Declaration Template Validity

Checking records type-declaration template validity while generating annotation
nodes. A declaration is locally invalid when its header contains a `.malformed`
annotation node or any annotation node generated for its backing resolves to the
solver's error type. Every local named-type reference generated in a declaration
also records a directed dependency from the referencing declaration to the
referenced declaration. This evidence comes from the normal annotation-generation
traversal; validity must not be reconstructed later by rescanning source syntax or
solved type structure.

After all local type declarations have been generated, checking computes the
transitive closure of invalidity over those recorded dependency edges. This
finalization is linear in the number of declarations plus recorded references,
handles forward and mutually recursive declarations independent of source order,
and first runs before nominal-recursion validation. Nominal-recursion errors add
invalid declarations to the same worklist, which is then propagated incrementally
before value checking without rebuilding the dependency graph. Every invalid
type declaration has its declaration root and backing template poisoned to the
error type, and invalid nominal declarations are also marked invalid in the
declaration table. CheckedModule construction enforces this invariant: a nominal
declaration marked valid always has an error-free checked template, and
encountering malformed template data for one is an invariant violation.
`CheckedTypeStore` construction and public-API dependency collection both omit
nominal declarations that checking explicitly marked invalid.

### Associated Item Lookup Through Aliases

Canonicalization resolves ordinary nominal associated items whose declaration
is already known, but it does not interpret an alias annotation to rediscover
the nominal owner. A lookup through a local or imported alias is explicit CIR
carrying the alias declaration and requested item. Checking follows the solved
alias backing to its terminal nominal, resolves that nominal's owner by module
content identity, and performs the exact owner-and-item method lookup. It then
replaces the source node with the exact target module identity, type node, and
definition. Alias traversal has no source-text reconstruction or fixed hop
limit; invalid and cyclic aliases must already resolve to the checker error
type.

The checker memoizes this resolution by alias declaration type variable and
item, while each use still instantiates the selected method scheme separately.
`CheckedBodyPayloadCopier.copyExprData` treats any unresolved associated lookup
as an invariant violation. For a resolved target reached through a re-export,
checked module data stores the exact imported procedure or imported constant
identity selected by checking. `buildImportedTemplateClosure` and
`collectPublicApiDependencies` receive defining checked module data explicitly
when no lexical import exists.

### Compile-Time Evaluation And Static Storage

Compile-time evaluation must evaluate every checked top-level expression and
every selected compile-time root that can be evaluated without effectful calls
or runtime data. It must run `crash`, `dbg`, and `expect` during that
evaluation and output their diagnostics during `roc check`.

A function-typed top-level binding whose entire checked right-hand side is one
resolved procedure lookup already has its complete checked callable identity.
It has no value computation for compile-time evaluation to perform. Checking
keeps the callable root and binding so ordinary runtime uses can instantiate the
referenced procedure, but it does not add a compile-time request for that root;
the root payload remains pending. This decision consumes the checked expression
kind and its resolved value reference. It must not infer a procedure from source
names, function type alone, body shape below the root expression, or post-check
specialization results. Any wrapper, capture, conditional, call, or other
function-valued computation remains an ordinary compile-time callable root.

Evaluation and static storage are separate checked outputs. Unreachable
top-level values are still evaluated when eligible so their `crash`, `dbg`, and
`expect` behavior is reported, but successfully evaluated unreachable data does
not need to be stored in checked module data or target static data. Reachable
evaluated values that have a static representation should be stored once and
shared. Records that contain static lists should point at shared static list
bytes; equivalent named and inline constants should produce equivalent static
data.

When checking finalization lowers a root that refers to an already-stored,
representation-stable `ConstStore` value, it must preserve that sharing by
emitting an explicit LIR static-data value. It must not recursively rebuild that
value as runtime list, record, tuple, tag, box, string, or callable construction.
Callable-containing const graphs are not representation-stable at this boundary:
post-check specialization may replace callable identities and capture graphs, so
they remain explicit reconstructions. Eligibility is computed by a memoized walk
over explicit `ConstStore` edges, never inferred from runtime bytes or layout
coincidence.

The explicit static-root locator is recursive context. Restoration propagates
it through every transparent and nominal construction layer and into tag,
record, tuple, box, list, string, and callable captures. A wrapper must not drop
that context and thereby turn a representation-stable descendant back into a
runtime construction.

Object emission, native compile-time execution, and interpreter compile-time
execution consume the same target-layout static-data materializer. In-process
evaluators own a relocated immutable data image and index its root addresses
directly by compact `StaticDataId`; callable relocations carry explicit
capture-offset metadata so the interpreter does not reconstruct ABI meaning from
bytes or symbols. A static-data candidate is identified by its stored const node,
checked type, and concrete Monotype type; a checked type id alone cannot identify
its representation across distinct specialization contexts. Raw stage-local
Monotype ids are not representation identity. Candidates for the same stored node
and checked type share one `StaticDataId` only after exact structural
Monotype equality succeeds. All child lowering contexts in one specialization
draft reuse that one closed candidate expression. Direct LIR lowering then reuses
one initializer only for the same identified candidate and committed runtime
layout. This is at least as strict as requiring identical destination const plans
and layouts, preserves distinct specialized and narrowed representations, and
prevents equivalent instantiations from rebuilding or repeatedly walking a large
stored graph.

Compile-time evaluation is allowed to fail with user diagnostics only during
checking. After checking, stored constant data is ordinary checked output. A
target static-data builder may decide which reachable evaluated values have a
target representation, but it consumes the checked roots and evaluated values
directly; it must not scan checked CIR or generated code to rediscover root
eligibility.

If a reachable evaluated value cannot yet be represented as target static data,
the missing representation is a compiler bug. The checked output or static-data
builder must make the missing explicit data assertable and testable; it must
not silently demote the value from compile-time evaluation. No backend may
rediscover or guess root eligibility by scanning source syntax, function
bodies, object symbols, or generated code.

Target static data is produced from explicit, closed LIR initializer procedures.
Monotype restoration builds each initializer from the stored `ConstStore` value,
then closure lifting, lambda solving, layout commitment, structural LIR rewrites,
and ARC run normally. The initializer's returned LIR value is the sole source of
target representation: static materialization freezes its exact bytes,
allocations, procedure relocations, and explicit generated-RC-helper
relocations into readonly target data. It must not
walk `ConstStore` in reverse using a type-derived storage plan, because a
target-independent `ConstStore` node does not preserve the contextual callable
encoding of every nested allocation.

F32 and F64 are distinct representations throughout the post-check pipeline.
An F32 local, immediate, register value, stack slot, call argument, return, and
builtin-wrapper parameter/result must remain IEEE binary32. A backend must not
implement an F32 LIR operation by widening its operands to F64, performing the
F64 operation, and narrowing the result. F64 may enter an F32 value's dataflow
only at an explicit language conversion between F32 and F64. Integer and Dec
conversions to F32 must round directly to binary32 rather than converting to
F64 first, so they cannot double-round.

Floating-point evaluation has an explicit NaN mode. Ordinary runtime execution
uses `preserve`, which permits the target's native f32 and f64 NaN sign, payload,
and signaling-bit result. Any interpreter or backend used to execute a static
initializer must instead use `normalize`. In that mode, every f32 or f64 value
produced by an LIR assignment is canonicalized before it is bound to its local:
all f32 NaNs become bits `0x7fc00000`, and all f64 NaNs become bits
`0x7ff8000000000000`. This producer-side invariant includes results nested into
later aggregates because aggregate construction consumes already-normalized
locals. The `ConstStore` writer and target static-data materializer must freeze
the value they receive and must not inspect, repair, or guess floating-point
behavior. Consequently, the same checked initializer has byte-identical NaNs
whether compile-time evaluation runs through the interpreter or native code,
and regardless of the host used for cross-compilation.

Runtime NaNs need not have identical in-memory bits, but Roc code must not be
able to distinguish their sign or payload. The float `to_bits` operations and
hashing canonicalize every NaN at their public observation boundaries, and
`to_str` renders every NaN as `"nan"` without a sign. Fallible float-to-integer
conversions and fallible f64-to-f32 narrowing reject non-finite inputs before
conversion; wrapping float-to-integer conversions have explicit
target-independent modulo behavior and map every non-finite input to zero.
Encoders may distinguish NaN, positive infinity, and negative infinity, but not
individual NaN representations. A new float operation or observation boundary
must preserve these rules explicitly; a backend must never infer from its use
whether normalization is required.

Finite float bits remain observable through `to_bits`. A transcendental whose
finite result is not fixed by a correctly-rounded IEEE operation must therefore
use one explicit target-independent implementation for each float width across
compile-time evaluation, the interpreter, and every backend. Backends must not
substitute a target libm call or LLVM intrinsic for that implementation. The
implementation must keep F32 operations binary32 and F64 operations binary64,
and exact-bit backend tests pin representative finite results.

Static initializer execution uses target-width symbolic memory rather than host
pointers. Every allocation records its committed target layout, alignment,
reference-count metadata, and relocations. Materialization freezes the graph
reachable from the returned value and may deduplicate only exact target
representations. It must not choose callable variants, reconstruct layouts, or
search for compatible encodings: those decisions already exist explicitly in
the initializer's LIR. In particular, an erased callable's final-drop
relocation carries its exact RC operation and capture-layout identity; a
backend compiles that named atomic helper and never derives it from the
capture layout or a symbol name.
Initializer procedures are materialization-only LIR: they remain available to
the freezer but runtime backends do not emit dead machine code for them.

Release builds of the compiler must never impose artificial resource limits on
compile-time evaluation. In particular, the interpreter's call-depth guard
(`max_call_depth` in `src/eval/interpreter.zig`) is enforced only in Debug
builds of the compiler, where it turns runaway recursion into a deterministic
Roc crash with interpreter context attached. In release builds, evaluation
depth is bounded only by actual native stack memory, and exhaustion is
reported by whoever owns the executing thread: compile-time evaluation runs on
compiler threads covered by the stack overflow guard in `src/base`, while
runtime interpretation runs in the shim/app process, where stack-overflow
reporting belongs to the platform host. An arbitrary depth budget in release
would make a program's compile-time-evaluability depend on a compiler build
constant rather than on the program itself, and would let Debug and release
builds disagree about whether the same program compiles.

## Backend Builtins

Backend builtin linking is part of backend code generation, not a later repair
step. Each backend consumes explicit builtin call symbols emitted from LIR and
uses the representation that matches that backend.

The dev object backend emits native object code directly. Its builtin calls are
ordinary object-symbol references resolved by linking the target's
`roc_builtins.o`. The dev backend keeps using target-specific builtin object
files because it does not produce LLVM bitcode.

The LLVM backend emits application LLVM bitcode. LLVM builds must not link
`roc_builtins.o`. Instead, the compiler selects builtin LLVM bitcode by the
target pointer width, links that builtin module with the application module
before LLVM optimization, and emits the object file from the merged module.
Roc supports only 32-bit and 64-bit target pointers here, so two builtin
bitcode payload families are sufficient: one for 32-bit targets and one for
64-bit targets. Each pointer-width family has a core payload for common
string/list/refcount/debug roots plus lightweight integer parse/format roots,
and a full payload for decimal, float parsing/formatting, wide-integer, and
other heavier roots. The LLVM backend selects the core payload only when every
explicit builtin declaration in the app module is in the core root set;
otherwise it selects the full payload. These pointer-width
payloads must contain Roc builtin definitions only; they must not bundle
compiler-rt or other target-specific runtime code, because that would make the
payload architecture-specific again. The payloads are built as freestanding
LLVM bitcode so compile-time OS and CPU branches cannot bake a native
platform's syscalls, inline assembly, or runtime support into a module that will
later be retargeted. LLVM object emission for targets that are not required to
link a platform C runtime disables target-library assumptions. Targets that
also lack native memory operations lower LLVM memory intrinsics to explicit
loops before target code generation. macOS and Windows keep target library calls
available because their final links include the platform runtime libraries.

Builtin definitions in the merged LLVM module are real definitions. They must
not be marked `available_externally`, because there is no later builtin object
file to provide non-inlined calls. After builtin call symbols are resolved,
builtin aliases and definitions that are not application exports may be made
internal so LLVM dead-code elimination and the final linker can remove unused
builtin code. Before merging, the LLVM backend roots builtin exports at the
explicit builtin declarations emitted by the application module, internalizes
all other builtin definitions, and runs LLVM global dead-code elimination on
the builtin module. After merging, it resolves builtin aliases to their
concrete definitions, internalizes the merged builtin definitions that are not
application exports, and runs LLVM global dead-code elimination again before
object emission. Pre-merge elimination keeps unused builtin IR out of the
expensive optimization and code-generation pipeline. Post-merge elimination
cleans up definitions and aliases whose final reachability is only visible
after app calls have been resolved. Both passes preserve real definitions for
builtin calls that the application can inline.
LLVM object emission must request function and data sections, and the final
target linker must use section garbage collection where the target format
supports it.

Static ownership reasoning lives in exactly one place: LIR ARC insertion.
ARC insertion computes a whole-program borrows-with-lifetimes solution and
emits explicit RC statements from it (see ARC Borrow Inference). No other
stage—checking, post-check lowering, backends, the interpreter, or LirImage—
contains a borrow, lifetime, uniqueness, or parameter-mode model. Everything
the solver computes is ARC-stage-local data; none of it appears in checked
modules, LirImage, or any consumer-visible structure. Runtime mutation still
uses `refcount == 1` to decide whether in-place mutation is allowed.

Roc functions have fixed arity. Roc functions are not automatically curried.
The compiler must not synthesize partial-application closures, curried call
chains, or missing-argument wrappers unless Roc source explicitly constructs a
function value that returns another function.

`Bool` is an ordinary nominal tag union at runtime. Lowering and backends must
not special-case Bool runtime representation. Internal scalar predicate values
may exist only as control-flow implementation details; when a Roc value is
stored, returned, boxed, passed, exported, or refcounted, Bool uses the ordinary
tag-union representation selected by layout lowering.

Implementation data must never use sentinel/default values that can be mistaken
for real results. If a value is valid only after a producer writes it, storage is
initialized to `undefined` or guarded by explicit presence/state metadata. Every
consumer must prove the producer ran before reading the value. A crash or
invariant violation must be reported as the actual crash or invariant violation;
it must never be disguised by reading a convenience default.

This rule applies everywhere: checked data, lowering data, backend data, cache
data, ARC data, LirImage data, and test reporting. If code uses a sentinel
as a placeholder for data that must be produced, stop and redesign the producer
ownership and presence model.

## Source Parsing Boundary

Parsing is a token-first stage. Tokenization produces the only cursor input for
the parser, and the parser walks that token buffer directly. The parser does not
use recursive grammar functions, and it does not keep source substrings as an
implicit parsing cursor. Source text may be consulted only through token
metadata, for diagnostics, literal decoding, and identifier interning.

### Import Targets

The parser records every source import as a structured target. That structure
contains its origin (local or package), local base (importer, parent traversal,
or package root), ordered source-path segments, package qualifier when present,
and ordered nested-type segments. Binding clauses are separate fields and must
never participate in source-file selection.

`/` separates source-path segments. `.` selects a type nested inside the source
module. A bare local target and `./` are relative to the importing file, `../`
traverses toward the package root, and a leading `/` begins at the package root.
A package target consists of a lowercase package alias, one `.`, and its public
module name; any later dotted segments are nested types. Package consumers
cannot name the package's internal source path. A package header may bind a
public module name to one exact internal target with an explicit local import.

Resolution normalizes local targets to one package-root-relative logical path
before graph insertion. That logical path is the module identity, so distinct
source spellings of the same target share one graph node. Resolution derives
exactly one `.roc` path from the parsed target. It does not probe alternate
files, reinterpret dots as directories, inspect `as` or `exposing`, or recover
an import meaning from solved types. Parent traversal beyond the package root,
incorrect directory spelling, and multiple logical targets for one underlying
file are errors. Cache keys and watch inputs consume the normalized identity
and exact resolved path produced here.

The parser is a direct token-dispatch machine. Hot parser code is organized as
grammar kernels that walk the token buffer with local token dispatch and ordinary
lexical control flow. The hot path must not route grammar progress through a
central parser-state interpreter loop, even when the cases contain token
switches, because optimized code can lower that transition pattern to a central
indirect branch.

This mirrors simdjson stage 2 more closely than a generic labeled-state switch.
simdjson's stage-2 parser walks a precomputed structural stream with concrete
JSON grammar labels such as object-begin, object-continue, array-value, and
scope-end. Its depth stack stores only open JSON scope fields (`is_array`,
tape index, item count); it does not store "run this parser state next"
instructions. Roc parser kernels must follow the same split:
tokenization performs linear input discovery, parser kernels inspect the
current token directly, and parser-owned syntax state describes currently open
Roc syntax rather than queued control flow.

Zig has no arbitrary sibling `goto`, so Roc cannot literally copy simdjson's C++
label layout. The Zig equivalent is lexical grammar loops with local token
switches, explicit syntax-depth state for nested constructs, and direct
fallthrough/`continue`/`break` transitions inside the kernel. Where a grammar
transition cannot be expressed lexically without a generic context switch, the
hot alternatives are to duplicate a small token-dispatch block or to split out a
specialized grammar kernel whose body remains stack-safe and assembly-audited.
Using a wide parser-context switch is not accepted for hot expression, pattern,
statement, or type parsing unless ReleaseFast assembly proves that exact slice
has no central indirect branch and is faster than the lexical-kernel shape.

Parsing chunks are not considered structurally done until ReleaseFast assembly
has been checked for this shape: no recursive parser calls for the converted
grammar, no instruction-driver loop, no broad parser-context dispatch ladder,
and no unexpected indirect branch in the hot transition path. The expression
prefix/suffix/binary-operator kernel is the first required audit target because
it is the parse-heavy hot path.

Parsing conversion proceeds by grammar slices that can be assembly-audited.
Before expanding a slice, build a tiny Zig proof of the intended dispatch shape
and compare it with the analogous simdjson stage-2 parser shape: local token
tests, direct branches between parser states, and explicit syntax state only
where nested syntax requires it. After converting the real Roc slice, build the
ReleaseFast compiler with symbols and disassemble the converted parser symbol
directly, for example:

```sh
zig build roc -Doptimize=ReleaseFast -Dstrip=false
xcrun llvm-objdump --macho --disassemble --dis-symname <source-parsing-symbol> zig-out/bin/roc
```

The audit result must be recorded before moving to the next slice. If the
assembly shows a dense jump table, generic context dispatch loop, indirect
branch in the hot parser transition path, or revived recursive grammar call,
the slice is not accepted and must be reshaped before more grammar is converted.

Current parser audit result:

```text
commit: 27165e02fd Fix pattern root parser instantiation
binary: zig-out/bin/roc
version: release-fast-27165e02
build: zig build roc -Doptimize=ReleaseFast -Dstrip=false --summary all --color off
```

The parser entry wrappers for expression, statement, pattern, type annotation,
and associated statement blocks all enter `runExprStatementKernel` with an
explicit root mode. They are API wrappers, not separate recursive grammar
kernels. Static source and symbol checks must not find `OpenSyntaxKind`,
`ParserContext`, `TypeOpenSyntaxStack`, `runTypeAnnoDirect`,
`parseWhereClauseTokens`, or `parseWhereConstraintTokens`.

The current ReleaseFast audit disassembled these parser kernel instantiations:

```text
_Parser.runExprStatementKernel__anon_169991
_Parser.runExprStatementKernel__anon_175153
_Parser.runExprStatementKernel__anon_175404
```

Searching those disassemblies for indirect branch-table dispatch found no
`br xN` instructions. Remaining indirect instructions were `blr x8` allocator
calls in growth/copy paths, not parser-state transitions. This is the accepted
assembly shape for the current unified parser slice.

The optional-record-field parse slice, including flat mixed-chain construction,
was audited separately on 2026-07-22:

```text
change: mlulymlm (optional record field parse phase, including mixed chains)
baseline: 18ef7fc30c0bc4957120e663f0183d296b981d5f
target: native x86_64_v3-apple-macos
build: zig build run-test-zig-module-parse -Doptimize=ReleaseFast --verbose
binary: .zig-cache/o/ed6407b87243805d109b0945a092b421/parse
symbols:
  _Parser.runExprStatementKernel__anon_23955
  _Parser.runExprStatementKernel__anon_32009
  _Parser.runExprStatementKernel__anon_32114
```

On this x86_64 target the unchanged parent already contained 14 indirect jumps
in each kernel instantiation. A first version of the optional-access token
dispatch caused LLVM to add a fifteenth jump table in the suffix path. Grouping
the dot-token enum values by operation and using contiguous range branches
removed it. The final flat-path candidate still has exactly 14 legitimate
indirect-jump sites per instantiation, with the same ordered target-register
signatures as the retained pre-flatten binary. LLVM objdump decodes in-text
jump-table bytes after each function's terminal `retq`; the audit excludes those
false instructions using the Mach-O `JUMP_TABLE32` data-in-code ranges. The new
`.?field`, optional-field-declaration, and path-segment branches add no indirect
branch, central dispatch, or recursive parser call. Each path uses one compact,
source-ordered segment span in dedicated side storage; it does not widen the
parser's node column.

Nested Roc syntax uses explicit open-syntax state, like simdjson's open
container depth. This state records concrete syntax currently being parsed:
open lists, records, strings, blocks, matches, type applications, and similar
constructs. It is not a parser instruction stream and must not store "execute
this parser operation next" entries. When a syntactic construct closes, the
parser inspects the parent open syntax and branches directly to that parent's
lexical continuation inside the current grammar kernel, or returns a completed
result to the caller when the kernel's root syntax closes.

Open-syntax state is stored compactly. The hot state records syntax kind and
indexes into syntax-specific side storage when payload is unavoidable. The
parser must not store wide tagged unions as call records for grammar work, and
must not push generic parser instructions just to decide what token to inspect
next. Leaf token cases that do not open nested syntax must not push state.

The parser owns a small set of result registers. Expression, pattern, type,
statement, associated-item, header, collection, and token-span results are
written to registers as syntax closes. The parent open syntax documents which
register it consumes. Closing nested syntax means jumping to the parent's token
branch, not returning through a Zig call stack and not interpreting a queued
parser action. Leaf helpers may exist for non-grammar work such as token
inspection, literal decoding, declaration indexing, scratch-span construction,
and diagnostic output, but they must not parse nested Roc grammar by calling
another grammar entrypoint.

`NodeStore` is the parser's output builder. The parser may accumulate children
in parser-owned scratch spans while a syntactic collection is open, then commit
the final AST node when its closing token is consumed or when parser recovery
emits a malformed node. Declaration indexing is updated from committed
statements and headers as part of this same iterative walk, so later compiler
stages consume explicit parser output rather than inspecting source syntax.

Error recovery is part of parsing and error reporting. Recovery states are also
iterative token states: they advance to a known delimiter, line boundary, or
collection close token and then jump to the next documented open-syntax branch.
Recovery may use parser-local heuristics because parsing and error reporting are
the only compiler stages allowed to do so. Recovery must still output explicit
malformed AST nodes and diagnostics; later stages must not recover missing
syntax on their own.

The parser implementation must not keep the old recursive-descent or
per-subgrammar instruction-interpreter architecture. Old expression, pattern,
statement, block, and type-annotation parser entrypoints are forbidden
implementation details. Public package functions may continue to expose parsing
capabilities such as parsing a whole file, header, expression, or statement, but
inside the parser they must enter direct token dispatch with an explicit goal
context. Static verification for this invariant is part of parser work:
searches for the old architecture names and recursive parser entrypoint names
must come back empty before Zig is run.

Post-check names should be short and precise. Do not encode whole explanations
into long compound type or function names. Prefer a small local vocabulary such
as `FnSet`, `FnVariant`, `FnTemplate`, and `CaptureSlot`, then define the exact
meaning in the surrounding design or module comments. Longer names are reserved
for cases where two nearby concepts would otherwise be genuinely ambiguous.
Avoid vague compiler jargon when a plain name is available. The words `bridge`
and `projection` are banned in new post-check docs, APIs, modules, type names,
variable names, and comments. They may appear only in this ban. Say the
specific operation instead, such as conversion, field read, tag payload read,
capture slot, or wrapper function.

The terms `readback`, `reification`, `value graph`,
`compile-time value store`, and `representation repair` are also banned in new
post-check docs and code. Compile-time evaluation stores results in
`ConstStore`; later compilations restore cached consts from `ConstStore`.
There is no separate phase or store under the const-related names. If two
representations unexpectedly differ for the same value at the same boundary,
that is an invariant violation; fix the producer instead of adding a later
conversion.

Outside the existing Canonicalization phase, the word `canonical` is banned in
new post-check docs and code. Use the exact term instead: `authoritative` for source-of-truth documents,
`lexicographic order by name` for sorted rows, `payload position order` for tag
payloads, `TypeDef`/`NamedType` for named type definitions, and `TypeDigest` for
structural checked-type digests.

The suffixes `Key` and `Ref` are banned in new type names. Use `Id` for assigned
identities, `Digest` for structural hashes, and a concrete domain noun for
compound identities such as `TypeDef`, `FnDef`, `ProcTemplate`, or
`CheckedModuleId`.

The term `runtime image` is banned in new post-check docs and code. Use
`LirImage` for the contiguous, viewable ARC-inserted LIR image plus layout store
and entrypoint tables.

The word `publish` and vague data-owner terms are banned in new post-check docs
and code, including their common variants. Use `output` for phase output, or
use the exact owner/data name.

The word `physical` is banned in new post-check docs and code. Use `layout`
only for memory shape data such as size, alignment, field offsets, and payload
layout. Use `runtime encoding` for the broader category that includes layouts,
discriminants, callable variant encodings, erased callable code entries, ABI
shape, and runtime schemas.

Vague owner terms are banned in new post-check docs and code. Use the precise
owner instead: `CheckedModule`, `CheckedModuleBuilder`, `checked module cache`,
checked module data, platform relation data, or another exact producer/consumer
name.

The word `semantic` is banned in new post-check docs and code. Use the precise
term instead: checked data, checked type store, source meaning,
checked identity, source row position, `FnDef` identity, `FnSet` context, or
another exact stage-owned name.

The word `executable` is banned in new post-check docs and code except in the
phrases `executable binary` and `executable program`. Use the precise stage or
data name instead: Monotype, Lambda Mono, LIR, direct call target, lowered
value, runtime layout, or checked function template.

The word `obligation` is banned in new post-check docs and code. Use the exact
owner instead: checked dispatch plan, erased callable requirement,
specialization queue entry, or debug assertion.

## Canonicalization Stack Safety

Canonicalization is allowed to be referenced by name here because it is the
existing pre-check phase that produces CIR. This section is about the
implementation of that phase only; it does not change the boundary that
post-check compiler stages must consume Checked Modules rather than CIR.

Canonicalization must be fully stack-safe. Traversal code in `src/canonicalize`
must not use direct recursion, indirect recursion, or mutual recursion to walk
source syntax. Deep source nesting must consume explicit work storage allocated
with `std.heap.stackFallback` and then the general allocator, not the process
call stack. Nesting limits such as maximum parenthesis depth must not be used
to protect the implementation from recursion; the traversal shape must be
iterative.

The main expression, block, and associated-item path should be implemented as a
direct labeled-switch kernel rather than as a generic frame pop loop. The
public entry points can remain small wrappers such as `canonicalizeExpr` and
`canonicalizeExprOrMalformed`, but the internal worker should look like a state
machine:

```zig
const CanLabel = enum {
    expr_start,
    expr_complete,
    seq_next,
    block_next,
    block_finish,
    associated_next,
    associated_finish,
};

fn runExprKernel(self: *Self, root: AST.Expr.Idx) Allocator.Error!?CanExprResult {
    var fallback_state = std.heap.stackFallback(16 * 1024, self.env.gpa);
    const scratch_allocator = fallback_state.get();

    var scratch = CanKernelScratch{};
    defer scratch.deinit(scratch_allocator);
    errdefer scratch.cleanupActive(self);

    var expr_state = ExprState{ .ast = root };
    var last_expr: ?CanExprResult = null;

    can_kernel: switch (CanLabel.expr_start) {
        .expr_start => {
            // Inspect expr_state, schedule child work, and jump directly.
            continue :can_kernel .expr_complete;
        },
        .expr_complete => {
            // Use last_expr as the completed child result.
            return last_expr;
        },
        else => unreachable,
    }
}
```

Completed child results should be carried through typed return registers such
as `last_expr`, `last_pattern`, and `last_type_anno`. Avoid a generic result
stack for every child expression. The kernel should keep hot state in locals
and jump directly between labels with `continue :can_kernel .label`, following
the same performance model as the stack-safe parser.

Do not replace recursion with one large tagged union that stores every possible
continuation payload. That tends to copy the largest payload on every push and
pop. Instead, use typed continuation stacks with compact parent-kind enums. For
example, expression continuations can have a small parent-kind stack plus
specialized payload stacks for the cases that need extra data:

```zig
const ExprParentKind = enum(u16) {
    unary,
    bin_lhs,
    bin_rhs,
    list_item,
    tuple_item,
    apply_arg,
    method_receiver,
    method_arg,
    lambda_body,
    if_condition,
    if_then,
    if_else,
    match_cond,
    match_guard,
    match_body,
    while_cond,
    while_body,
    for_list,
    for_body,
    block_expr_stmt,
    block_final_expr,
    block_decl_body,
    block_var_body,
    block_reassign_body,
    block_expect_body,
    block_return_body,
    associated_decl_body,
};
```

Each payload type should live in the stack that matches its shape. Hot nested
constructs such as blocks, sequences, and associated-item groups should use a
current-plus-spill layout: keep the active item in a local or `current` field,
and move the previous active item to a spill stack only when entering another
item of the same kind. This avoids repeatedly copying large block state while
still supporting arbitrary nesting.

Blocks should have an explicit `BlockState` managed by a current-plus-spill
stack. A block state owns the statement slice, next statement index, saved
scratch tops, saved scope flags, pending result indexes, and any block-specific
bookkeeping needed to restore canonicalization state at block exit. Child
continuations must not carry copies of the whole block state. When a statement
schedules child expression work, it should push only the statement-specific
continuation data needed to resume the current block. The `block_next` label
advances statements one at a time, and `block_finish` performs local
forward-reference classification, constructs the block expression, exits
scopes, and restores saved state.

Lists, tuples, calls, method calls, tags, match branches, and other repeated
child forms should use sequence state instead of nested calls. A sequence state
tracks the source items, the next item index, output scratch ranges, and the
continuation to run when all items are complete. Each child result is appended
to the sequence output as it arrives in `last_expr` or the appropriate typed
return register.

Associated items need explicit ownership boundaries. The current
`enterAssociatedBlockState` and `exitAssociatedBlockState` responsibilities
should remain, but the expression kernel should model associated work as active
state rather than as cleanup hidden in pending generic frames.
`CanKernelScratch.cleanupActive(self)` must unwind every active block scope,
associated scope, type-variable scope, and owned alias sink on errors. Correct
cleanup must not depend on eventually popping a particular continuation frame.

Pattern and type-annotation canonicalization should use the same design. Keep
their public entry points, but implement each traversal as a direct
labeled-switch kernel with its own typed return register and typed continuation
stacks. The expression kernel may call those kernels for lambda arguments, loop
patterns, and type annotations because each call is itself nonrecursive and
stack-safe.

The recommended migration order is:

1. Add the expression kernel scratch state, typed stacks, current-plus-spill
   helpers, and active cleanup path.
2. Port expression leaves and small one-child or two-child forms.
3. Port sequence forms such as lists, tuples, calls, method calls, and tags.
4. Port block handling and remove copies of whole block state from continuation
   payloads.
5. Port associated-item integration and verify error cleanup.
6. Port pattern and type-annotation canonicalization kernels.
7. Audit `src/canonicalize` for direct, indirect, and mutual recursive
   traversal calls.
8. Verify with focused canonicalization tests, `zig build minici`, and
   parser/canonicalization benchmarks.

## Canonicalization Policy Ownership

Canonicalization owns source-name scope policy before checking. Any rule that
decides whether a source type name is inserted, shadows another type, replaces
an auto-imported type, redeclares an existing type, or repeats the same external
type must live in one place. Callers may choose which source operation they are
performing, but they must not duplicate the type-binding collision matrix.

The `Scope.type_bindings` table has one ordinary mutation API for type names.
It accepts the full scope slice, the target scope index, the introduced name,
and the incoming binding:

```zig
const TypeBindingInput = union(enum) {
    local_nominal: CIR.Statement.Idx,
    local_alias: CIR.Statement.Idx,
    associated_nominal: CIR.Statement.Idx,
    external_nominal: Scope.ExternalTypeBinding,
};

const TypeBindingDecision = union(enum) {
    inserted,
    inserted_shadowing_parent: Scope.TypeBinding,
    replaced_current_external: Scope.ExternalTypeBinding,
    idempotent_current,
    rejected_current_conflict: Scope.TypeBinding,
    redeclared_current: Scope.TypeBinding,
};
```

The exact names may change with implementation, but the shape must remain:
one `Scope` function mutates `type_bindings`, and its return value carries the
old binding that caused any warning or error. `Scope` does not push diagnostics,
does not inspect source regions from `ModuleEnv`, and does not update import
display mappings. `Can` maps the returned decision to diagnostics and performs
the import-mapping side effects that are specific to external imports.

Parent-scope shadowing must be computed by the same type-binding API. The
function receives the scope slice and target index directly; it does not use an
untyped callback or a callback without context. The parent walk chooses the
nearest parent binding and returns that binding to the caller. This preserves
regions for both local statements and external imports without reconstructing
them at each call site.

The current-scope collision matrix is:

- A same statement already bound to the same name is idempotent.
- A same external module/original-name pair already bound to the same name is
  idempotent.
- A local declaration replacing an external binding succeeds and returns the
  replaced external binding so `Can` can report the shadowing region.
- An external binding colliding with any different current binding is rejected
  and returns the existing binding.
- A local alias colliding with a current local alias reports alias
  redeclaration.
- A local nominal colliding with a current local nominal or associated nominal
  reports type redeclaration.
- A local declaration colliding with the other local kind reports the diagnostic
  chosen by the existing binding, not by the incoming binding.

Direct writes to `Scope.type_bindings` are allowed only for explicit
initialization paths that prove the binding cannot collide, such as seeding the
compiler-owned builtin scope before source declarations are introduced. If that
proof stops being local and obvious, the initialization path must use the same
type-binding API.

Result suffix desugaring has a separate owner inside `Can`. The suffix forms
`expr?`, `expr ? handler`, and `expr ?? default` all lower to a match over the
same `Try` shape. They must share one concrete builder for the common structure:

- resolve the compiler-owned `Try` nominal target once,
- wrap `Ok` and `Err` tag patterns with that nominal target,
- append the `Ok(#ok) => #ok` branch,
- construct the `Err(...)` tag expression used by early return,
- create the final match with the caller-selected `is_try_suffix` value.

The builder must support both local and external nominal targets because the
Builtin module may refer to its own `Try`, while ordinary modules use the
compiler-owned external builtin. It must not silently fall back to bare `Ok` and
`Err` patterns when `Try` is missing. A missing compiler-owned `Try` target is a
compiler invariant violation after builtin setup, not an alternate
canonicalization mode.

The three suffix callers provide only the distinct error-branch body:

- `expr?` returns the original error payload from the enclosing function, or
  emits `e_expect_err` inside a top-level `expect`.
- `expr ? handler` transforms the error payload and then returns `Err(...)`, or
  emits `e_expect_err` inside a top-level `expect`.
- `expr ?? default` uses the default expression and does not mark the match as a
  try suffix.

Do not introduce a generic desugaring interpreter for these cases. The helper
should be a small set of concrete `Can` functions that emit the same CIR nodes
and scratch spans as the current hand-written paths. This keeps
canonicalization output explicit, keeps diagnostics in `Can`, and keeps release
builds fast: the work runs once per source suffix or type declaration, with no
runtime cost in the compiled program.

## Checked Type Equivalence Classes

The checked type store represents each solved equivalence class with two
independent identities:

- the **storage root** is an internal union-find node chosen only to keep the
  equivalence-class forest shallow;
- the **checked representative** is the type variable whose identity ordinary
  unification says survives the merge.

These identities must not be conflated. Ordinary unification continues to
select its second resolved operand as the checked representative. Expected
return variables, deferred dispatch constraints, generalization pools, checked
type keys, and every other consumer of solved variable identity observe that
checked representative exactly as they did before structural balancing. They
must never observe or infer meaning from the storage root.

The storage root is selected by a separate union-find rank. This structural
rank is not `Descriptor.rank`: descriptor rank is the Hindley-Milner scope level
used to decide generalization, while union-find rank is private data-structure
metadata. On a union, the lower structural rank redirects to the higher one; a
tie keeps the second storage root and increments its structural rank. Path
compression may change storage parents at any time outside a solver savepoint.
Neither operation may change the class descriptor or checked representative.

Each live storage root names one descriptor whose root metadata contains the
checked representative. Structural rank is stored separately on every slot,
including redirects. This lets occurrence-directed error recovery explicitly
re-root a class without coupling its new storage root to the descriptor chosen
by checked unification. When a balanced union keeps the
first operand's storage root but the second operand is the checked survivor,
the retained storage root adopts the second class's descriptor and records the
second checked representative. This preserves the unifier's descriptor merge
destination and externally observable survivor while allowing the storage
tree to remain balanced.

`resolveVar` returns the checked representative and class descriptor;
storage-root traversal is a private store operation. Consequently a checked
representative is not required to be a storage root slot. `is_root` on a
resolved variable means that the queried variable is the checked
representative, which is the only root notion checker consumers may use.

Solver savepoints journal storage-parent, structural-rank, and root-metadata
mutations and restore all three byte-for-byte. Checked-store serialization
includes both metadata stores, so loading a cached store preserves its checked
representatives and balanced structure. A declared solver-mutating redirect
joins whole classes through the same balanced mechanism while explicitly
adopting the redirect destination's descriptor and checked representative; it
must not directly graft one storage root beneath another and recreate an
unbounded chain.

A failed unification is different from a successful class union. Its first
operand can be one checked occurrence already connected to a shared binding;
error recovery must poison that exact occurrence without making the binding or
an incidental storage child of the occurrence erroneous. When the queried
variable is not the checked representative, `poisonOnMismatch` enumerates its
class explicitly, re-roots and flattens the remainder at the checked
representative, isolates the queried occurrence as a rank-zero singleton, and
rank-merges that singleton with the second operand's error class. When the
queried variable is the checked representative, the mismatch belongs to the
class itself and the whole class is rank-merged into the error class. This rule
makes error recovery independent of union-tree shape and path-compression
history; it never guesses which variables are source occurrences from their
storage parents. Error recovery must use this explicit operation rather than
calling ordinary union or writing a raw redirect.

An already-erroneous operand cannot overwrite a solved type or a flex carrying
constraints. Encountering `.err` against either terminates the current
unification successfully for diagnostic recovery, before any enclosing
structure is merged. An unconstrained flex placeholder may adopt `.err`; this
is how an erroneous expression explicitly fills its owning binding or
annotation slot without contaminating an independently constrained producer.
Checker sites that own a reported error use `markErroneous` to poison the owning
solved class directly. No successful ordinary unification propagates an
existing `.err` into a type that already carries information.

A relation an expression merely consults is not a relation it may destroy. A
call checks its callee and its arguments, and a field access checks the record
it reads from, against a shape the consuming expression demands; each operand is
an independently solved producer that other expressions also read. Those sites
unify through `unifyOwnedRelation`, which suppresses mismatch poisoning, records
the diagnostic itself, and marks only the consuming expression erroneous. The
producer keeps the type it was solved to, so a rejected relation neither
cascades into unrelated uses of that producer nor leaves an `.err` on a binding
whose value post-check lowering must still instantiate.

Because `.err` no longer merges, it also no longer relates the operands unified
against it. A checker site that relies on one variable to carry a relation
between several others has to supply that relation itself once the carrier is
erroneous. `match` is the one such site: every branch pattern describes the same
scrutinee value, and that mutual consistency normally travels through the
scrutinee's variable. When the scrutinee is already erroneous, the patterns
unify against a shared fresh variable instead, so a disagreement between two
patterns is still reported at the pattern that disagrees rather than surfacing
later as an unexplained branch-body mismatch. The scrutinee's own error is not
re-reported, the patterns are never related back to it, and the first
disagreement poisons the shared variable so later patterns short-circuit exactly
as they do when the scrutinee carries the relation.

## Type Alias Invariant

Source type aliases are transparent views of their backing type. An alias root
in the checked type store records source spelling and alias arguments. It is not
a nominal type identity, and it is not the authoritative solved representative
for a concrete structure.

When unification relates an alias to a concrete structure, the checker must
unify the concrete structure with the alias backing variable directly. It must
not allocate a replacement alias, redirect the concrete structure to an alias
root, redirect the alias backing through the alias root, or otherwise make alias
preservation depend on union-find representative shape. The alias root may
remain as a transparent checked view whose backing variable carries the solved
structure.

This invariant also covers the degenerate case where the concrete structure
variable is already the alias backing variable. That unification is a no-op
after resolving the backing. Creating a fresh alias representative in that case
would make the alias backing resolve back to the alias itself, which is an
invalid self-referential type-store graph.

Any stage that needs alias spelling, source identity, or user-facing checked
type presentation must consume explicit checked data produced by checking. It
must not infer that presentation from a union-find representative chosen during
structural unification. This keeps the producer responsible for checked
presentation, keeps consumers simple, and keeps release builds fast: no
alias-content cloning, no substitution-map reconstruction, and no cycle-repair
walks are part of normal unification.

For an expression or definition with an explicit type annotation, checking first
proves that the body is compatible with the annotation. After that succeeds,
the checked root for the expression or definition is the annotation root. The
body may have constrained the annotation backing type, underscore variables, or
alias arguments, but references to the annotated value consume the annotation
root. This is how alias spelling from annotations is preserved without making
alias roots union-find representatives for concrete structures.

## Nominal Constructor Backing Relation

An explicit nominal constructor chooses the nominal wrapper itself. Its operand
is checked against the declaration's instantiated backing through the dedicated
`nominal_constructor_backing` root relation, not through unrestricted ordinary
unification.

At that root, the relation cannot be satisfied by implicitly lifting an already
nominal actual value through an anonymous expected backing. For example, if
`Wrap` has backing `{ a : U8, b : U8 }`, then `Wrap.{ ..wrap, a: 1 }` is rejected
when the record update has already lifted to `Wrap`; the constructor requires
its record backing, not another `Wrap`. A constructor whose declared backing is
itself a named type may still receive that exact named type.

Only the constructor's outer backing pair uses this relation. Once that pair is
accepted, component pairs use ordinary unification, so a backing record field or
tag payload may contain an ordinary nominal value and structural values may lift
there according to the normal language rule. An unconstrained backing parameter
may likewise resolve to a nominal type; that is substitution of the declaration
parameter, not an inverse lift through a concrete structural backing.

This rule is implemented inside pure unification as explicit caller-supplied
relation data. The checker must not probe a solved operand and then mutate or
poison the graph separately, and Monotype must not repair an invalid checked
constructor. A rejected root relation produces the existing nominal-constructor
type mismatch diagnostic. The rejected side is pinned by
`test/snapshots/issue/issue_10195_nominal_record_update_rewrapped.md`; accepted
nested-nominal and implicit-record-update controls are pinned in
`src/check/test/type_checking_integration.zig`.

## Module Completion Boundary

The compile coordinator records phase progress separately from user diagnostics.
A source module that reaches checking has no user-error `Failure` outcome. It
must produce its complete `ModuleEnv`, final content identity, and CheckedModule
data required by importers. The Check type-check result carries exactly one of:

- the complete CheckedModule
- the complete checker-owned continuation for platform/app relation
  construction, which waits for both CheckedModule inputs

User diagnostics never select a third outcome and never propagate dependency
failure. Every invalid source construct is represented at its first owning
producer boundary by explicit checked data: a checked runtime-error expression,
a checked-error dispatch plan, a crash constant, or a checked-error platform
requirement. Importers and every post-check stage consume that data normally.
Independent definitions, imports, compile-time roots, and runtime paths remain
available; execution crashes only if it reaches a recorded checked error.

Parsing and error reporting may recover malformed source in order to construct
the explicit malformed/runtime-error nodes that later stages consume. I/O,
allocation failure, unsupported compiler hosts, corrupt serialized CheckedModule
inputs, and compiler invariant violations are operational aborts, not user-error
module outcomes. They must propagate as operation errors rather than being
converted into a user diagnostic or a module `Failure` value.

## Cache Boundary

The checked module cache is the only checked cache boundary in this design.
Checked module cache entries are trusted compiler-produced cache entries, not
adversarial inputs. Cache reads validate only the cache header,
entry-version hash, key, serialized layout, and ordinary binary decoding. They must
not rerun checked validation, reselect hoisted roots, reconstruct checked data,
or walk checked expressions to prove that cached checked data is still complete.
Correctness belongs to the producer path that writes the cache entry, and
invalidation belongs to the cache key and explicit cache/selection format
versions.

`ModuleEnv` contains `CommonEnv.strings`, a `base.StringLiteral.Store`. That
store is part of the checked module cache data. A cache hit materializes it as a
view of its byte buffer and stops. Cache reads must not scan the string entries,
rebuild a string interning table, check every string length header, or check
every entry boundary. This design adds no
store-specific release-build validation; cache reads perform only the existing
cache-entry admission and decode checks before trusting the blob. Once those
pass, the internal string buffer structure is a producer invariant. Debug builds
may assert this invariant while constructing fresh stores and in focused store
tests; optimized cache reads consume the store directly.

String literal deduplication is a build-time concern. The durable
`StringLiteral.Store` owns only portable checked string bytes plus `get` and
iteration by `StringLiteral.Idx`. Each entry is encoded as:

```text
len: u32 little-endian | bytes
```

It has no insert API and no dedup index.
Fresh construction uses `StringLiteral.Builder` state paired with a `Store`.
That state may live in a wrapper or in the build owner that owns the store, but
it is always transient. The builder index is never serialized, never stored in
LirImage, and never rebuilt on a cache hit. If a later phase needs a mutable
string-literal builder, it must request an explicit fresh builder from source
data or another builder-owned input; it must not reopen a cached store on the
normal cache path.

The byte interning algorithm has one owner shared by identifier names, checked
name stores, and string-literal builders. Storage policies own only id encoding,
text lookup, and append layout. For string literals, appending a new entry writes
the portable checked entry layout above and returns the content byte offset as
`StringLiteral.Idx`. Duplicate input
bytes must return the existing content offset. The hash table is an accelerator
only: hash matches must still compare exact byte length and contents before an
existing id is reused. The shared interning algorithm is comptime-policy
specialized, so string literals, identifier names, and checked name stores do
not pay a runtime storage-kind branch.

Runtime static string layout is generated later by the target-specific static
data emitter. The checked cache must not store native pointer-width padding,
static refcount words, allocation headers, or any other runtime `RocStr` layout
bytes.

The string-literal builder must reject impossible `u32` length or content-offset
overflow as a compiler invariant: debug builds assert or panic with the
invariant, and optimized builds mark the path unreachable. It must never silently
truncate a string length or offset.

The checked module cache id is target-independent:

```text
CheckedModuleId =
    source_hash
  + compiler_build_hash
  + module_identity
  + checking_context_identity
  + direct_import_checked_module_ids
```

The cache id is not merely the module's source bytes plus recursive import
ids. Source bytes are only one input. The id also includes the compiler build
hash, the module identity, the checking context identity, and the ordered direct
import checked module ids. The checking context identity includes import-name
hashes, resolved import ids, platform requirement context, platform/app
relation identity, and explicit root requests. Any additional checked tables
stored in the checked module cache must be deterministic output of those
checked inputs and the checked modules they name. Such tables are serialized
data, not new cache-id inputs.

`module_identity` includes the module's name. Canonicalization output is not
a function of source bytes alone—a type module's main type takes its name
from the module's file name—so no key or identity derived from module
content may be computed from source bytes without the module name.

The cache id does not include target ABI, pointer width, layout ids, field offsets,
alignment decisions, backend choice, object format, code-generation options,
post-check lowering strategy, or post-check specialization state.

Module boundaries are cache boundaries only. They must never change the final
runtime behavior or performance of the compiled program, except for debug
information. Compiling one large module and compiling the same code split across
imports must produce the same reachable `.lss` specializations or `.boxy`
descriptors, callable representations, layout decisions, ARC statements, and
backend behavior for the same lowering strategy.

Checked modules store the target-independent lowering visibility selected by
checking. This includes the complete checked module id set needed by later
post-check stages for the module's explicit roots, checked type roots, checked
type schemes, public API dependencies, type-owner dependencies, and
platform/app relation closures. The set is duplicate-free and stable. It is
serialized as relocatable sorted POD slices, with binary-search lookup where a
map is needed; mutable hash maps may be used only while constructing the checked
module and must not be the persisted representation.

On a cache hit, the coordinator consumes this lowering visibility directly to
materialize imported checked module views. It must not rebuild the same set by
walking checked bodies, checked type roots, checked type schemes, public API
dependency lists, or platform/app relation closure data. Recomputing that
visibility during post-check lowering is a producer-boundary bug: the checked
module cache already owns this target-independent checked information.

The compiler does not cache Monotype IR, Monotype Lifted IR, Lambda Solved IR,
Lambda Mono decisions, boxy representation plans, boxy type descriptors, boxy
dictionaries, LIR, or any callable/layout representation derived from them as
part of checked modules. Those structures are target/session products of the
current root compilation.

A post-check specialization cache is a separate boundary named
`SpecializationCacheFile`. It is consumed only after immutable checked modules
and explicit root requests are available. It is not embedded in checked modules,
is not visible to importers as checked data, and does not change the checked
module cache id. Its validity id is computed from exactly the checked modules,
root requests, and Monotype configuration consumed by specialization. A
Monotype-only cache file excludes target ABI, pointer width, layout ids, field
offsets, backend choice, object format, ARC state, and code-generation options.
If a later-stage cache needs those inputs, it must use a separate file format
and a separate validity id.

Monotype IR is target-independent, but it is still post-check and root-specific.
It depends on the roots requested for the current compilation, the reachable
monomorphic specializations, and the static-dispatch and source-loop lowering
performed for that compilation. `ConstStore` entries in checked modules are
therefore checked-stage stored constants, not Monotype nodes.

Boxy lowering is also post-check and root-specific. Its descriptors and
dictionaries depend on the current roots, target pointer width, exact host ABI
roots selected for this compilation, and the checked dispatch plans reachable
from those roots. They are not checked cache data. Any cache that stores lowered
LIR, LirImage data, object code, or executable binary output must include the lowering
strategy and every target/backend input that can change the lowered
representation.

The checked module cache stores checked Roc values only. Roc language values are
target-independent except for pointer-sized Roc values if the language exposes
them to compile-time evaluation. Compiler runtime representation data are not
Roc values and must never enter `ConstStore`: runtime addresses, allocation
identity, layout ids, runtime discriminants, field offsets, LIR proc ids,
backend symbols, object-format details, and backend state are all outside the
checked value domain. Host interaction exists only at runtime, so host handles
and host results cannot be compile-time values. If Roc exposes pointer-sized
values to compile-time evaluation, their checked cache format must be an explicit
checked rule before such values may be output.

When the checker changes what checked data it emits, how hoisted roots are
selected, or how checked compile-time values are serialized, the checked module
cache format or the specific checked-data selection version must be bumped. A
cache hit with a matching key and version is consumed as already-checked output;
the compiler must not pay an extra pass to rediscover whether the cached output
is complete for the checked module.

## Def Checking Order

Type checking processes a module's top-level defs as binding groups: the SCC
condensation of the name-reference graph, in deterministic topological order
(groups ordered by their first member's source position among independent
groups; members within a group in source order). The graph covers every name
reference in a def's expression tree—nested lambda bodies and blocks
included—plus statically-resolvable type-qualified method-call targets. It
is transient checking input computed from canonicalization output when
checking starts and freed with the checker; a checked module never stores it.

Because groups are checked in dependency order, a name reference between
groups always points at an already-checked def. There is no re-entrant
`checkDef` from inside an expression walk, and no code path exists for a name
reference to an unchecked def outside the current group (it is a debug
invariant violation).

Annotated schemes come before any body. A pre-pass declares a standalone
generalized scheme from every eligible annotation (a simple `.assign` binding
whose annotation has no `_` hole): the annotation's type is generated once in
place, deep-copied into disjoint orphan vars, generalized, and the annotation
nodes are reset so the def's own body check generates them again exactly as
always. A reference to an annotated def—by name or by dispatch—before or
while its body checks instantiates this standalone scheme, exactly like a
reference to an imported scheme copy; the def itself still checks with its
annotation generated in its body's frame, sharing vars with the scheme the
checked module outputs, which checked dispatch-evidence resolution relies on.

The recursion rule is the ML binding-group rule. A recursive group gets one
shared rank frame: members' patterns are ranked in it first, an in-group
reference to an unannotated member unifies monomorphically with the member's
in-flight type (call-site constraints flow into the inferred scheme), unannotated
members' top-level lambdas stay in the frame instead of generalizing on their
own, and the whole group generalizes at the frame's boundary. References to
annotated members instantiate the pre-declared scheme, preserving sound
polymorphic recursion for annotated defs. The same rule applies to
block-local `s_decl` functions: each local function decl is a binding group
of one with its own rank frame, and annotated (type-var-free) locals
pre-declare their scheme. There is no deferred post-generalization validation
of recursive references anywhere; the monomorphic rule leaves nothing to
validate afterwards. A consequence is that an unannotated recursive def
used at two incompatible types within its own group is a type error—the
old deferral silently accepted such programs unsoundly.

Static-dispatch dependencies are inherently dynamic (`|a| a.foo()` dispatches
on an inferred type), so they cannot be in the name graph. When a deferred
static-dispatch constraint resolves to an unchecked, unannotated local def
mid-body, the target is only *recorded* (`pending_dispatch_targets`, owned by
the discovering group) and the constraint re-deferred; the constraint's vars
are pinned at the group's boundary rank so no inner lambda can generalize
them first. At the group's generalization boundary—a singleton def's RHS
frame or a recursive group's shared frame, where no mid-body state is pending—
the driver checks each target's group in its own nested frame (together
with any unchecked topological prefix), re-runs dispatch, and interleaves
boundary literal defaulting to a fixpoint before generalizing. Group checks
nest only at such boundaries.

Group suspension and merge need no dedicated machinery: a suspended group's
members are `.processed` with still-live, not-yet-generalized vars, so a
dispatch back-edge from a nested group links to them monomorphically and rank
adjustment keeps the shared structure at the suspended group's rank, where it
generalizes when that group's boundary completes. A group therefore never
generalizes while one of its deferred dispatch constraints into an unchecked
group is outstanding, and a suspended group is never re-entered—both are
asserted in
debug builds (`group_stack`, group states, and the pending-target
stack-suffix discipline).

Deferred early-return / `?` constraints record the lambda that created them,
and each lambda's end drains exactly its own entries; a drain cannot touch
another lambda's constraints, by construction. Rank bookkeeping is therefore
strictly stack-shaped: unification only ever runs while the frame owning its
vars is active, and `addVarToRank`'s debug guard is a regression tripwire
rather than a reachable condition.

## Checked Boundary

Checked CIR is the last source-level representation. It owns:

- source expression and statement shape
- source patterns
- checked expression and pattern types
- checked procedure templates
- resolved value references
- method registries
- normalized static-dispatch plans
- platform/app relation data
- `ConstStore` and dependency summaries
- checked module serialization
- all user-facing diagnostics

Checked CIR may contain source-level forms such as static-dispatch calls,
method equality, type-dispatch calls, and source `for` loops because those are
part of the checked source module.

Those forms do not survive runtime lowering. The `.lss` strategy removes them
while producing Monotype IR. The `.boxy` strategy removes them while producing
LIR directly from checked data.

The checked boundary outputs immutable checked modules. A checked module is
either complete or unavailable to later stages. Later stages may read checked
modules but may not mutate checked source data, perform additional user-facing
checking, or derive missing checked data by scanning source syntax.

During checking finalization, compile-time evaluation may need to lower and run
checked roots before the checked module can be output. That work uses a
`CheckedModuleBuilder`, not a complete checked module. The builder is the only
mutable owner of the in-progress `ConstStore`, dependency summaries, and checked
root payloads.

Compile-time lowering during checking finalization receives a
`CheckingFinalizationView`. That view exposes checked data plus
builder-owned checked result sinks. It is not a `CheckedModule`, and it
is not visible to importers or later post-check stages.

The builder has one transition:

```text
CheckedModuleBuilder.finish() -> CheckedModule
```

After this transition, the checked module is immutable. Post-check runtime lowering,
importers, LirImage construction, and backends only consume
`CheckedModule`. Compile-time lowering during finalization may receive a
builder-owned sink for checked results, but that sink is not an alternate
post-check state and is never visible to importers.

The checked module may store checked-stage constant values in `ConstStore` and
checked procedure templates for promoted callables. It must not store post-check
representation data. In particular, the checked module does not contain runtime
type payloads, value conversion plans, callable-set descriptors, boxy
`TypeDesc` data, boxy dictionaries, erased callable ABI decisions, layout ids,
runtime tag discriminants, or backend encodings.

The checked type store is an interned graph, not a collection of independently
duplicated checked type trees. During construction it maintains an exact
checked-type-digest index for O(1) root lookup. Closed roots (roots with no
reachable flex or rigid identity) with the same checked-type digest are one
`CheckedTypeId`; identity-bearing roots remain distinct when their explicit
identity instance changes checked meaning. Each root stores whether an identity
is reachable, computed by the producer that constructs or projects it.
Consumers use that metadata instead of repeatedly traversing the graph.

Type substitution preserves this graph discipline. A substitution of a closed
root is the original root, because no formal can occur in it. A real
substitution is memoized by its complete source/formal/actual input and interns
its result through the same checked-type-digest index. Dispatch callable
instantiation additionally memoizes the complete target-callable/plan-callable
pair, so equal checked dispatch edges share one result. Checked-type digest
construction is memoized over already-stored child roots; cryptographic hashing
is performed once for a new checked-type root, never as a linear search
mechanism.

This is a checked-boundary rule, not merely a pipeline rule. Any checked
module field outside `ConstStore` whose only purpose is to feed post-check
runtime representation is not part of the checked boundary. If later lowering
needs data, checking must output it as target-independent checked data such as
templates, dispatch plans, method registry entries, platform relation data,
hosted declarations, or `ConstStore` entries. Target-specific runtime
representation data is produced after checking.

The checked module may output checked data that later stages need, such
as:

- checked procedure templates
- `ConstStore` entries for compile-time constants
- checked dispatch plans
- method registries
- platform, hosted, and exposed function declarations
- opaque, nominal, alias, row, and builtin ownership data

Those data must remain target-independent. Outside the explicit Monotype
evidence attached to evaluated `ConstStore` values, they are also
representation-free.

Named checked types carry explicit owner identity. The source-origin module
identity remains the source identity used for `TypeDef`, diagnostics, source
locations, and name-store interning. Alias and nominal checked payloads also
carry the checked module id that owns the declaration or representation
authority. For local named types, that id is the current checked module id; for
imports, checked type copying preserves the imported owner id; for builtins, it
is the builtin checked module id.

Monotype and runtime lowering consume the owner checked module id directly as a
checked module address. If a checked type mentions an owner checked module id
that is not present in lowering visibility, the checked module producer is
incomplete.

### Platform/App Relation

The app↔platform correspondence is assigned once, at check time, and carried
as checked data. When an app root is checked against a platform's requirement
surface, the app's checked module records one requirement solution per
platform requirement, keyed by requires-clause index: the exported app value
that satisfies it as a checked export id (def and checked pattern, never a
name), the requirement type as solved in the app's env, and the solved types
of the requirement's identity variables in identity-slot order—the
first-encounter order the type key digest assigns, shared byte-for-byte
between the solver-var digest and the checked-payload digest.

Finalization and platform-root output are readers of those rows. Building the
platform/app relation resolves each platform requirement declaration to the
recorded app export by requires index; the relation-bearing platform root
output projects the recorded solved roots into its store while preserving each
source root's identity
and pairs the platform requirement payload's identity nodes with the recorded
solutions slot by slot. No stage after check completion resolves an app export
by name, re-checks requirement/provided type compatibility, or re-derives
identity bindings by structurally matching platform types against app types. A
requirement/app mismatch is only ever a check-time diagnostic. Finalization
constructs one total outcome per platform requirement: an exact relation/binding
for a successful solution or an explicit checked-error requirement index for a
failed solution. The relation-bearing platform CheckedModule preserves successful
sibling bindings, and a lookup at a checked-error index lowers to a runtime
crash. There is no relation-less error fallback and no separate flow that
"permits" user errors.

A platform `provides` declaration must name a top-level value defined in the
platform module. It cannot name a value from `requires` directly; a platform
that wants to expose an app-provided value to its host defines an explicit
platform-local entrypoint whose definition uses that requirement.
Canonicalization writes `local_def` into every `ModuleEnv.ProvidesEntry`; it is
non-null exactly when the header target is a top-level definition in the
platform module. Header validation inspects all declarations, while checked
provides output contains only entries with non-null `local_def`. Rejected
declarations also produce source diagnostics.

A platform requirement's for-clause alias is a binder over an app-supplied
type: the requirement's `Model` IS the app's `Model` by the for-clause's own
definition, so identity provenance follows meaning provenance. After the
requirement surface is checked, the checker supplies each platform alias's
explicit `(origin module, source declaration)` key and backing identity root as
source substitutions while copying each requirement into the app store. The
copier resolves every declaration-owned alias occurrence and every recorded
identity slot directly to the app's own type declaration; no copied
platform-owned alias enters the solved app graph. Nothing in the app's checked
output needs the platform root's checked module as a type owner, which is what
lets an app build's platform root defer its checked-module output.

The platform root's checked module is output exactly once per build:
relation-bearing at finalization when an app root is paired (keyed by the
platform/app relation identity), or relation-less at check completion when the
workspace has no app root. While a paired platform root's output is deferred,
its requirement surface installs from its checked env and a requirement context
computed from the same declaration data the output required-declaration table
hashes, so the checker input and the cache identity cannot disagree.
Deferral is enabled only when the Coordinator will construct relation-bearing
checked module data. The retained `Check` owner supplies that construction's
problem store, selected hoisted roots,
requirement context, imported diagnostic environments, and CTFE options. A
checked module cache entry contains both `ModuleEnv` bytes and `CheckedModule`
bytes; `ModuleEnv` bytes alone cannot stand in for the retained `Check` data.

A relation-less platform output preserves its complete `provides` metadata for
glue and interface consumers. If it also declares app requirements, it does not
output provided runtime roots and cannot enter Monotype;
runtime commands reject it until an app relation exists. This decision depends
only on the explicit relation state and requirement surface, never on scanning
provided bodies to guess whether the missing requirements happen to be used.

### Compile-Time Constants and Hoisted Roots

Compile-time constants are checked roots. A compile-time constant root may be an
ordinary top-level constant or a selected top-level-equivalent expression from a
runtime body. A top-level-equivalent expression is an expression whose checked
dependencies are all available without runtime arguments, mutable runtime state,
host interaction, or observable runtime effects. Its value is computed during
checking finalization and stored in `ConstStore`; later lowering restores that
checked value instead of emitting runtime work for the original expression.

Hoisting does not move source syntax. A hoisted root points at the existing
checked expression and its source region. User-facing compile-time diagnostics,
debug information, crash locations, and source maps must report the expression's
original source location. Synthetic root wrappers and ordering metadata are
compiler-internal only.

Compile-time crash diagnostics use checked source regions carried forward by
post-check lowering, not source text reconstruction. Monotype expressions and
statements carry checked regions beside resolved `SourceLoc` values; LIR stores
the checked region for each source-bearing statement; and the interpreter
captures the failed checked region directly. Compiler-owned or builtin frames
whose checked region is `Region.zero()` are explicit transparent implementation
frames and lower to `SourceLoc.none`, so a callee failure crossing such a frame
reports the checked caller site. Finalization must not recover a checked region
from module display names, source filenames, line/column offsets, or broadest
matching checked nodes.

Runtime and debugger provenance is independent of machine procedure
boundaries. Post-check IR interns inline-scope nodes containing the source
procedure identity, the exact caller site, and the enclosing inline scope;
every source-bearing expression and statement carries one scope id beside its
location. Cloning preserves the scope. Inlining extends it with the call site.
Specialization keeps the original source procedure identity. LIR and serialized
LirImage retain the same scope graph. The LIR interpreter captures the failed
statement's innermost scope id so a diagnostic consumer can expand its exact
parent chain. LLVM emits the graph as standard nested `DISubprogram` and
`DILocation.inlinedAt` metadata. Other debuggers and runtime symbolizers must
consume this graph (or a lossless backend encoding of it); they must not infer
source frames from the surviving machine procedures.

The synthetic default platform's crash entrypoint receives such a lossless
encoding directly from LLVM codegen: the current LIR statement's inline-scope
chain is flattened innermost-first into constant source-frame records, with the
statement location for the innermost frame and each exact call site for its
parent. The default platform prints those materialized source frames before
walking the machine stack. It never scans procedure bodies for crashability or
reconstructs an inlined source frame from a machine symbol.

Inlining permission never depends on scanning a body for `crash`, `expect`, a
particular low-level operation, or a transitively reachable failure. Such scans
are necessarily incomplete for indirect calls and future failure forms. A
machine stack frame and a source frame are separate concepts: optimized code may
remove the machine call while its virtual inline frame remains represented for
debugger and crash-report consumers.

Hoistability is computed while checking expressions, as part of the existing
recursive checking work that already determines types, resolved references, and
effect data. Checking may return temporary hoistability data from `checkExpr`
and keep temporary binding data in the active lexical scope, but it must not add
permanent hoistability summaries to every checked expression. The checked module
stores only selected hoisted roots plus sparse lookup indexes needed by later
lowering, such as checked-expression id to hoisted-root id and selected
local-binding id to hoisted-root id.

The hoistability decision must use explicit checked data, not source-name scans
or canonicalization guesses. Allowed dependencies include literals, already
known compile-time constants, selected hoisted constants, imported constants
whose checked modules have stored values, and pure checked callables whose
captures are themselves compile-time-known. Rejected dependencies include
function arguments, runtime pattern binders, mutable locals, runtime control
decisions, effectful calls, host calls, platform requirements whose values are
not available during checking finalization, and any static dispatch whose
checked plan does not identify a pure compile-time-evaluable operation.
Low-level operations may participate only through explicit checked purity and
totality metadata; they must never be allowed by whitelist, name, or backend
knowledge.

Checking errors are dependency-local for hoistability. A malformed expression,
unresolved static-dispatch call, type error, or other checker-owned diagnostic
poisons the expression that owns the error and any expression that explicitly
depends on it. It does not poison sibling definitions, unrelated top-level
values, unrelated imported modules, or the checked program as a whole. A checked
CheckedModule data that carries diagnostics is still a valid input to every independent
compile-time-evaluation decision whose expression/dependency region is
well-checked. If one definition is erroneous and another definition is
independently compile-time-known, the independent definition must still be
evaluated during checking and, when reachable, emitted as static data.
The CheckedModule data must therefore be able to contain both diagnostics and
successful compile-time root requests. The presence of diagnostics is not an
module-level root-selection failure.

The compiler must not create separate hoisted roots inside an ordinary top-level
constant body. The whole top-level constant body is already a compile-time root,
so nested hoisted roots would add metadata and scheduling work without removing
runtime work. However, ordinary top-level constants can still depend on selected
hoisted constants indirectly by calling pure checked functions whose bodies
restore selected hoisted locals. Therefore same-module compile-time roots are
emitted as one dependency-sorted request stream, not as permanently separated
top-level and hoisted groups.

A top-level definition with a destructuring pattern is the exception to the
ordinary-root representation: it has no single source name under which the whole
right-hand side can be stored. After the pattern checks successfully, the
checker selects one pattern-extraction root for each binder, carrying the shared
right-hand side and complete scrutinee pattern. Those roots are emitted in
dependency-first order and later lookups resolve through the selected binder,
never as runtime-local pattern references.

Hoisted-root selection is positional as well as dependency-based. Selection may
fire only in structurally unguarded positions of runtime bodies, and the checker
must carry that position as explicit checking context while computing
hoistability in the normal recursive traversal. Eager child expressions inherit
their parent's position. Branch bodies, match guards, expect bodies, loop bodies,
statements after a prior effect/divergence blocker, block finals after such a
blocker, and conditions reached only after earlier conditional branches are
suppressed: they may still prove top-level-equivalent for enclosing expressions
or warnings, but they must not become independent roots. Ordinary top-level
constant bodies use a stronger compile-time-root context that suppresses nested
root selection and nested eligibility entirely, because the enclosing body is
already evaluated at compile time.

Canonicalization's top-level dependency order remains an input for ordinary
top-level constants, and checking should prefer to emit selected hoisted roots
in dependency-first order as it proves and selects them. The request order is
then computed from explicit checked references across all same-module
compile-time roots: ordinary top-level constants, selected hoisted constants,
callable eval roots, and literal conversion roots. Sorting may build
temporary dependency edges while sorting, but it must discard those edges before
the checked module is finalized. The durable checked module data stores only the
roots, the sorted request stream, stored `ConstStore` payloads, and sparse
lookup indexes.

Canonicalization also outputs the exact strict-demand edges between top-level
definitions. Those edges are durable `ModuleEnv` data, serialized independently
of the transient SCC evaluation order, because `RootRequestTable` scheduling
and compile-time finalization must distinguish eager dependencies from references
inside delayed callable bodies. Those consumers use the serialized relation
directly; they must not infer strictness from checked template references,
rebuild it from CIR, or treat a missing relation as an empty one. This relation
is durable `ModuleEnv` data, not a dependency graph stored in the checked module.

A checked module must not permanently store a hoisted-root dependency graph or
per-expression dependency metadata. The durable checked data is the compile-time
roots, their sorted compile-time request order, their `ConstStore` payloads, and
sparse root lookup indexes.

Checked module caches persist that same sorted selected-root list. On cache
miss, checking computes the list once from explicit checked data while it is
already traversing expressions. On cache hit, the cached list is decoded and
used directly after normal cache header, version, key, payload, and binary-shape
checks. Cache reads must not run a second hoistability analysis or validate
root-set maximality.

The compile-time finalizer consumes sorted root requests and validates that any
referenced same-module constant has already been filled before a root uses it.
That availability check is retained for the generic compile-time pipeline,
which also handles literal conversions, expects, callable roots, imported
constants, and platform-required values. It is not a scheduling graph for
hoisted constants, and it must not require storing dependency edges in the
checked module.

Hoisted-root scheduling is computed after checking has selected the sparse
hoisted roots, because only checked data can distinguish runtime captures,
effects, mutable locals, static dispatch behavior, platform availability,
same-module selected-hoisted const uses, and concrete compile-time types.
A record type is concrete for this purpose only when every field kind is
resolved. An undetermined field-kind cell remains specialization-owned even
when its associated checked type cell is otherwise concrete, so it cannot make
a compile-time root request eligible.

Runtime lowering restores a selected hoisted root by checked expression id. While
lowering the synthetic compile-time wrapper for that same root, lowering must
suppress restoration of the root currently being evaluated so the original
expression is evaluated exactly once by the compile-time finalizer. Nested uses
of other already-sorted compile-time constants may still restore their stored
`ConstStore` values.

Hoisted roots use the same compile-time constant rules as ordinary top-level
constants. A failure produced while evaluating a hoisted root is a checking-time
failure reported at the hoisted expression's original source region. If Roc ever
needs lazy-runtime-preserving hoists, that must be a separate checked root policy
with explicit totality and failure behavior; it must not be implemented as a
best-effort variant of top-level constant hoisting.

Imported checked modules must contain every checked procedure template and checked
body that may be instantiated by an importing root. This includes private helper
templates reachable from exported templates, static-dispatch targets, and
compile-time checked callable leaves. Privacy affects source
name lookup and diagnostics. It must not hide checked bodies from whole-program
post-check specialization.

### Function Definitions

`FnDef` is the checked identity of a callable body. It is used in
Monotype `fn_def` expressions and in compile-time checked callable leaves.

```zig
const FnDef = union(enum) {
    local_checked_template: ProcTemplate,
    imported_checked_template: ImportedProcTemplate,
    nested: NestedFn,
    local_hosted: HostedProcId,
    imported_hosted: ImportedHostedProc,
    checked_compiler_generated: GeneratedFn,
};

const ImportedProcTemplate = struct {
    module: CheckedModuleId,
    template: ProcTemplate,
};

const NestedFn = struct {
    owner: ProcTemplate,
    site: NestedProcSiteId,
    context_fn_key: TypeDigest,
};

const PromotedTemplate = struct {
    promoted: PromotedProc,
    template: CallableProcTemplate,
};

const ImportedPromotedTemplate = struct {
    module: CheckedModuleId,
    promoted: PromotedProc,
    template: CallableProcTemplate,
};

const ImportedHostedProc = struct {
    module: CheckedModuleId,
    hosted: HostedProcId,
};

const GeneratedFn = union(enum) {
    entry_wrapper: EntryWrapperId,
    intrinsic_wrapper: IntrinsicWrapperId,
};

const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
    source_fn_key: TypeDigest,
    mono_fn_ty: MonotypeTypeId,
};
```

A `FnDef` names the checked body/template to instantiate. It is not a
procedure value, LIR proc id, object symbol, erased ABI id, callable-set member,
layout id, or runtime code pointer. If a callable value captures data, the
captures are stored next to the `FnDef` by the value that contains it; the
`FnDef` itself remains capture-free.
`FnTemplate` is the checked callable template used by post-check function-value
flow. It pairs the checked function identity with the checked source function
type. Later stages must carry it forward instead of recovering the checked
function type from generated procedures, runtime layouts, or call sites.

Checked module output assigns a `NestedProcSiteId` to every
expression-position function inside each checked procedure template. A nested
function is identified by `(owner template, nested site, context function
digest)`. The site id is assigned from the checked body traversal before
post-check lowering starts. The context function digest is assigned by Monotype
from the `FnTemplate` whose body currently owns the nested function occurrence.
Monotype lowering carries that checked identity together with the checked source
function type and the lowered monomorphic function type. Post-check stages must
consume those checked data; they must not name nested functions by allocation
order, generated symbols, source display strings, body shape, capture shape,
runtime layout, or LIR procedure ids.

Monotype body lowering tracks two function-context digests:

- the owner function digest for local procedure sites output by the checked
  owner template
- the current function digest for expression-position lambdas and closures
  inside the body currently being lowered

References to local procedures use the owner function digest. That makes
recursive calls and sibling references inside one checked owner point at the
same nested function instance. Lambdas and closures use the current function
digest. That makes a lambda inside a nested local procedure belong to that
nested local procedure, so captures come from the correct body instance.

Function-context identity contains only durable checked identities and type
digests. Draft-local allocation ids are operational binder-to-value mappings;
they are not checked identity and must not affect a nested function or local
procedure digest. Captured runtime values remain separate from `FnDef` identity,
as described above.

When Monotype restores a capturing function from `ConstStore`, it preserves the
stored nested `FnDef`, including its producer-authored context function digest
and local-procedure-context digest. Restoration may install fresh draft locals
for the stored captures, but it must not recompute either digest from those
consumer-side locals. Thus a restored function and the corresponding runtime
successor use the same callable identity whenever their checked identity,
monomorphic type, evidence, and captures agree.

When Monotype has put a nested function in the nested definition table, that
table is the only owner of the function body. Later value occurrences of the
same `FnTemplate` are references to that nested definition; they do not rebuild
the body or recalculate captures from the occurrence site.

`local_checked_template` is checked-module-relative while the owning builder/checked module
is being processed. Importers refer to the same body through
`imported_checked_template` with an explicit checked module id. Complete imported
checked modules must contain the private checked templates reachable through these
references; consumers never recover imported callable bodies from source text,
display names, generated callable shapes, or runtime values.

`checked_compiler_generated` is only for checked-stage generated
templates that are part of the checked module contract. Functions generated
after checking, such as Lambda Mono specializations or adapters, use stage-local
symbols and are not `FnDef` values.

### RECORD FIELDS AND METHODS ARE ABSOLUTELY DISJOINT

RECORD FIELDS AND METHODS ARE DIFFERENT LANGUAGE FEATURES. THEY DO NOT SHARE A
LOOKUP RULE, A CALL RULE, AN AST NODE, AN IR NODE, A RESOLUTION PATH, OR A
FALLBACK. THE COMPILER MUST NEVER REPRESENT THEM AS A GENERALIZED "MEMBER," MUST
NEVER ASK WHETHER ONE COULD BE REINTERPRETED AS THE OTHER, AND MUST NEVER CHOOSE
BETWEEN THEM USING TYPE INFORMATION.

THE SOURCE SYNTAX SELECTS THE OPERATION BEFORE TYPE CHECKING, WITH ZERO
AMBIGUITY:

```roc
field_value = value.field
optional_field_result = value.?optional_field
field_result = (value.field)(arg)
method_result = value.method(arg)
```

`value.field` IS RECORD FIELD ACCESS. `(value.field)(arg)` IS ORDINARY FUNCTION
APPLICATION WHOSE CALLEE IS THE VALUE PRODUCED BY THAT FIELD ACCESS. THE
PARENTHESES ARE REQUIRED WHEN CALLING A FUNCTION STORED IN A RECORD FIELD
WITHOUT FIRST BINDING IT TO A NEW NAME.

`value.method(arg)` IS METHOD DISPATCH. `value.field(arg)` THEREFORE ALSO HAS
METHOD-CALL SYNTAX; IT MUST NEVER MEAN "ACCESS THE FIELD AND THEN CALL IT." IF
THE RECEIVER HAS A FUNCTION-VALUED FIELD NAMED `field` BUT NO METHOD NAMED
`field`, `value.field(arg)` MUST REPORT A MISSING METHOD. THE EXISTENCE OR TYPE
OF THE RECORD FIELD IS IRRELEVANT TO THAT METHOD LOOKUP.

`value.?field` IS OPTIONAL RECORD FIELD ACCESS. IT IS NEVER METHOD DISPATCH AND
IT IS NEVER REINTERPRETED AS REQUIRED FIELD ACCESS. `value.?field(args)` IS
REJECTED BY PARSING; A CALL MAY NOT ERASE THE QUERY OPERATION SELECTED BY `.?`.
IF A QUERIED VALUE IS CALLABLE, SOURCE MUST FIRST HANDLE THE QUERY RESULT AND
THEN CALL THE EXTRACTED FUNCTION VALUE.

AN OPTIONAL RECORD TYPE FIELD IS WRITTEN `{ required : A, optional ?: B }`.
THE `?` IS PART OF THE FIELD DECLARATION, NOT PART OF `B`: AT RUN TIME THE FIELD
MAY BE PRESENT WITH A `B` VALUE OR ABSENT. THE PARSER AND EVERY LATER STAGE MUST
PRESERVE THIS PRESENCE CLASSIFICATION EXPLICITLY. IT MUST NOT BE RECOVERED FROM
AN OPTION-LIKE PAYLOAD TYPE, A LAYOUT, A CONSTRUCTOR SHAPE, OR A QUERY USE SITE.

OPTIONAL FIELD ACCESS PRODUCES `Try(value, [FieldMissing])`. A CONTIGUOUS RECORD
FIELD ACCESS CHAIN THAT CONTAINS AT LEAST ONE OPTIONAL ACCESS, SUCH AS
`value.?a.b.?c`, PRODUCES ONE FLAT `Try(_, [FieldMissing])`, NOT A NESTED
`Try(Try(...))`. EVERY SEGMENT RETAINS ITS SOURCE CLASSIFICATION: `.field` IS
REQUIRED AND `.?field` IS OPTIONAL. A REQUIRED SEGMENT AFTER AN OPTIONAL SEGMENT
READS THE SUCCESSFUL PAYLOAD OF THE PRECEDING SEGMENT; IT DOES NOT PERFORM AN
OPTIONAL PRESENCE CHECK. ANY ABSENT OPTIONAL SEGMENT TAKES THE ONE SHARED
`FieldMissing` ERROR PATH.

PARSING REPRESENTS EACH MAXIMAL CONTIGUOUS RECORD-FIELD ACCESS CHAIN AS ONE
`field_access` AST NODE. THE NODE HOLDS THE BASE RECEIVER AND A NONEMPTY,
SOURCE-ORDERED SEGMENT SPAN. EVERY SEGMENT HOLDS ITS COMPOSITE SOURCE TOKEN
(`.field` OR `.?field`) AND ITS EXPLICIT REQUIRED/OPTIONAL MODE, SO ITS EXACT
REGION AND OPERATION ARE AVAILABLE WITHOUT A SYNTHETIC IDENTIFIER EXPRESSION OR
A LATER SOURCE SCAN. THE PARSER MAY EXTEND ONLY THE CURRENT, NOT-YET-OWNED TAIL
PATH; A CLOSED PATH IS THE RECEIVER OF A NEW NODE.

PARENTHESES AND EVERY NON-RECORD-FIELD OPERATION END THE PATH. THIS INCLUDES
TUPLE ACCESS, METHOD CALLS, AND ORDINARY APPLICATION AFTER PROPAGATION. FOR
EXAMPLE, `value.?a.b` IS ONE AST PATH, WHILE `(value.?a).b`,
`value.?a.method().b`, AND `value.?function?(arg).b` EACH START A NEW PATH AT THE
FINAL `.b`.

CONSEQUENTLY, EXTRACTING A PREFIX DOES NOT PRESERVE THIS ACCESS OPERATION:
`x = value.?a; x.b` STARTS A NEW REQUIRED ACCESS ON THE `Try` RESULT, WHILE
`value.?a.b` KEEPS `.b` ON THE SUCCESS PATH OF THE ORIGINAL OPTIONAL CHAIN.

CANONICALIZATION REPRESENTS EVERY REQUIRED-ONLY, OPTIONAL-ONLY, OR MIXED PATH AS
ONE `e_field_access { receiver, segments }` CIR EXPRESSION. ITS SEGMENTS ARE
SOURCE-ORDERED AUXILIARY CIR NODES STORED AS ONE DIRECT CONTIGUOUS RANGE IN THE
NODE AND REGION STORES, NOT THROUGH `index_data`. EVERY SEGMENT RETAINS ITS NAME,
MODE, EXACT REGION, AND STABLE NODE/TYPE-VARIABLE IDENTITY. A SEGMENT'S TYPE
VARIABLE IS THE SUCCESSFUL PAYLOAD PRODUCED BY THAT PATH PREFIX; THE ENCLOSING
EXPRESSION'S TYPE VARIABLE IS THE ONE OBSERVABLE RESULT OF THE COMPLETE PATH.

CHECKING CONSUMES THIS EXPLICIT PATH DIRECTLY. IT OWNS FIELD-PRESENCE TYPING,
REQUIRED/OPTIONAL UNIFICATION, AND THE SINGLE SHARED `Try(_, [FieldMissing])`
WRAP. CANONICALIZATION MUST NOT RECONSTRUCT A PATH FROM NESTED EXPRESSIONS AND
MUST NOT DESUGAR IT TO TAG CONSTRUCTORS OR CONTROL FLOW.

THE DOT-CALL SPELLING HAS NO WHITESPACE BETWEEN THE RECEIVER AND DOT, BETWEEN
THE DOT AND NAME, OR BETWEEN A METHOD NAME AND ITS OPENING PARENTHESIS. TRIVIA
RECOVERY AND FORMAT NORMALIZATION MUST NEVER BE USED AS A SIGNAL FOR CHOOSING
FIELD ACCESS VERSUS METHOD DISPATCH.

THE PARSER, NOT THE TYPE CHECKER, MAKES THE COMPLETE AND FINAL CHOICE:

- A DOTTED LOWERCASE NAME NOT IMMEDIATELY FOLLOWED BY `NoSpaceOpenRound`
  PRODUCES RECORD FIELD ACCESS.
- A DOTTED LOWERCASE NAME IMMEDIATELY FOLLOWED BY `NoSpaceOpenRound` PRODUCES A
  METHOD CALL.
- IN `(value.field)(arg)`, THE INNER PARENTHESES END THE FIELD-ACCESS EXPRESSION;
  THE OUTER ARGUMENT LIST THEREFORE PRODUCES AN ORDINARY FUNCTION APPLICATION.

THE DISTINCTION REMAINS ABSOLUTE WHEN A NOMINAL TYPE HAS A RECORD BACKING AND AN
ASSOCIATED METHOD USES THE SAME NAME AS ONE OF THAT BACKING RECORD'S FIELDS:

```roc
Thing := { f : I64 -> I64 }.{
    f : Thing, I64 -> I64
    f = |_, arg| arg
}

from_field = |thing| (thing.f)(1)
from_method = |thing| thing.f(1)
```

`from_field` ACCESSES THE BACKING RECORD FIELD AND APPLIES THE RESULTING FUNCTION
VALUE. `from_method` PERFORMS STATIC METHOD DISPATCH. THE NOMINAL WRAPPER DOES
NOT TURN FIELDS INTO METHODS, AND THE RECORD BACKING DOES NOT MAKE METHODS INTO
FIELDS.

THE COMPILER MUST PRESERVE THIS CHOICE THROUGH EVERY STAGE:

- PARSING PRODUCES A FIELD-ACCESS NODE FOR `value.field`, AN ORDINARY-APPLY NODE
  AROUND THAT FIELD ACCESS FOR `(value.field)(args)`, AND A METHOD-CALL NODE FOR
  `value.method(args)`.
- CANONICALIZATION AND CHECKING SEND FIELD ACCESS EXCLUSIVELY THROUGH RECORD-ROW
  TYPING, ORDINARY APPLICATION EXCLUSIVELY THROUGH FUNCTION-CALL TYPING, AND
  METHOD CALLS EXCLUSIVELY THROUGH STATIC DISPATCH AND METHOD REGISTRIES.
- CHECKED CIR CONTAINS DISTINCT FIELD-ACCESS, ORDINARY-CALL, AND METHOD-DISPATCH
  NODES. NO STAGE MAY REWRITE ONE FAMILY INTO ANOTHER OR INSPECT THE RECEIVER
  TYPE TO RECOVER A CHOICE THAT SYNTAX ALREADY MADE.
- LATER STAGES CONSUME THE EXPLICIT CHECKED FORM. THEY NEVER PROBE RECORDS WHILE
  LOWERING METHODS, PROBE METHOD REGISTRIES WHILE LOWERING FIELDS, OR TOLERATE A
  PRODUCER THAT EMITTED THE WRONG FORM.

ANY COMPONENT THAT CONFLATES THESE FORMS VIOLATES THE LANGUAGE. THE FIX BELONGS
AT THE FIRST COMPONENT THAT LOST THE SYNTAX DISTINCTION; DOWNSTREAM PRIORITY
RULES, TYPE-DIRECTED DISAMBIGUATION, RETRIES, AND FALLBACKS ARE FORBIDDEN.

### Static Dispatch At The Checked Boundary

Checking reports all user-facing static-dispatch errors. This includes missing
methods, ambiguous constraints, illegal equality use, invalid iterator `for`
constraints, and any other error that should be shown to the programmer.
Checked diagnostics include warning severity: a literal (number or string)
defaulted at a generalization boundary that narrows the definition's inferred
type reports `LITERAL DEFAULTED` as a warning, not an error, worded per
literal kind.

A where alias (`a.Sortable : where [...]`) is a source-level abbreviation for a
set of method constraints, not a type. Its declaration solves to its receiver: a
rigid variable carrying the constraints it names. A signature that references
one instantiates those constraints onto its own type variable, substituting the
declaration's receiver and parameters by name, so the resulting constraint set
is indistinguishable from one written out clause by clause. Nothing downstream
of checking sees a where alias. Because a rigid variable's constraint set is
fixed by the annotation that introduces it, a where alias constrains only its
receiver; canonicalization rejects a constraint written against a parameter
rather than emitting one that could never reach the applied argument.

The checked module outputs normalized dispatch plans. A dispatch plan is a
checked record, not lowered code:

```zig
const DispatchPlan = struct {
    site: DispatchSite,
    dispatcher: DispatchDispatcher,
    method: MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: []const DispatchOperand,
    result_mode: DispatchResultMode,
    resolution: CheckedCallResolution,
};

const CheckedCallResolution = union(enum) {
    direct_closed: DirectCall,
    direct_parametric: DirectCall,
    evidence_dependent: EvidenceChainIndex,
    structural: StructuralDerivation,
    unreachable,
    checked_error,
};

const DirectCall = struct {
    target: MethodTarget,
    target_instantiated_callable: CheckedTypeId,
    nested_evidence: EvidenceIdentity,
    local_context: ?LocalProcedureContext,
    runtime_target: RuntimeTarget,
};

const RuntimeTarget = union(enum) {
    procedure,
    low_level: LowLevel,
    intrinsic: IntrinsicId,
    graph_participating: struct {
        iterator_protocol: ?GraphProtocol,
    },
};

const DispatchSite = union(enum) {
    checked_expr: CheckedExprId,
    checked_node: CheckedNodeId,
};

const DispatchDispatcher = union(enum) {
    arg: u32,
    type_only,
};

const DispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    generated_interpolation_iter: CheckedExprId,
    generated_numeral: NumeralLiteral,
    generated_quote: CheckedStringLiteralId,
};

const DispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
    hash: struct { structural_allowed: bool },
    parser_for: struct { structural_allowed: bool },
    encode_to: struct { structural_allowed: bool },
};

const DispatchResolution = union(enum) {
    unresolved_checked_plan,
    resolved_target: MethodTarget,
};
```

Literal-origin dispatch evidence has the same lifetime as the checked literal
expression or pattern that owns it. If diagnostic recovery replaces that node,
its evidence is retired atomically and cannot be output as a dispatch plan.
An append-only raw-node table that can outlive its owning literal violates the
checked-boundary invariant even if the output step could identify and skip the
stale entry.

Every live literal-origin record leaves checking with one explicit resolution:

- `builtin_direct` means checking proved the target is the corresponding
  builtin scalar, validated the literal's exact digits or bytes, and discharged
  the conversion without looking up or instantiating a method. Monotype
  materializes the value directly.
- `custom_dispatch` means checking selected and typechecked one concrete custom
  conversion callable. `CheckedModule` construction retains its
  dispatcher and callable types, and compile-time evaluation evaluates that
  conversion when a checked constant is required.
- `specialization_dispatch` means the target remains an identity variable in a
  generalized callable. The checked plan retains this erased requirement; each
  Monotype specialization either materializes a builtin directly or consumes
  the callable evidence supplied for that specialization.
- `checked_error` means checking rejected the conversion while retaining the
  literal node for diagnostic recovery. `CheckedModule` stores no
  callable, runtime dispatch plan, or compile-time root for it; the containing
  checked `runtime_error` expression is the failure.

`unresolved` is construction-only and may not cross the checked boundary.
Checking finalizes each live record exactly once after constraint solving;
diagnostic recovery either retires the record with its owner or seals it as
`checked_error`. A residual concrete, non-builtin literal target with no
checker-selected callable is sealed as `checked_error` only when checking has
already reported an error; in an otherwise valid module it is an invariant
violation, because every successful concrete conversion must have positive
builtin or callable evidence. `CheckedModule` construction,
compile-time-root selection, and Monotype consume this resolution directly.
They must not inspect the checked target type to reconstruct which literal path
checking selected. In particular, `builtin_direct` stores neither the synthetic
conversion callable nor a runtime dispatch plan, and
`specialization_dispatch` is not a standalone compile-time root.

Source dispatch, type dispatch, method equality, and iterator `for` plans all
use checked dispatch plans. Iterator `for` uses its own iterator-dispatch
operand shape because the `.next` call receives the compiler-created iterator
state instead of an ordinary checked expression. It contains two plans:

- call `.iter` on the source iterable value
- call `.next` on the compiler-created iterator state local

`site` identifies the source construct that produced the plan for debug
verification and source mapping. It is not the call receiver. Expression-shaped
dispatch uses `checked_expr`; source constructs such as iterator `for` that are
not themselves checked expressions use `checked_node`.

`args` are the runtime arguments in callable order. `dispatcher` says how to
find the method owner. For ordinary value dispatch, method equality, and
iterator `.iter`/`.next`, `dispatcher` is an argument index into `args`; iterator
`.next` uses the compiler-created iterator state operand in `args[0]`. For type
dispatch, `dispatcher` is `type_only`, and `dispatcher_ty` is the checked type
that determines the owner.

CheckedModule construction chooses every target that checking made exact.
It projects the target callable through the checked call relation and classifies
the result as closed or parametric. A closed direct call has no reachable
checked identity variables and no nested evidence forwarded from an enclosing
specialization. A parametric direct call still has an exact target but retains
one of those explicit specialization inputs. Evidence-dependent calls are the
only calls whose target itself comes from an enclosing specialization.

The runtime target category is producer-authored. Canonicalization records the
exact low-level operation when it replaces a provided definition; CheckedModule
records ordinary procedure targets, annotation-only compiler intrinsics whose
monomorphic implementation is emitted at the checked call site, and operations
whose runtime representation must participate in a Monotype graph. Graph participation
covers both producers and representation-sensitive consumers: for example, an
`Iter` method that consumes a generated-private iterator must preserve that
representation even when it returns an ordinary value. An optional exact graph
protocol identifies operations that construct or directly interpret a compiler
representation; other graph-sensitive procedures carry no protocol. No
consumer may inspect a procedure body, builtin name, owner type, or result shape
to reconstruct this category.

Ordinary calls and method dispatches to the same intrinsic consume this exact
identity through one Monotype lowering path. A call-site intrinsic never becomes
an ordinary procedure specialization merely because static dispatch selected it.

Each checked procedure template stores separate spans of direct calls and
dispatch relations. Evidence instantiation iterates only the relation span; it
must not scan every direct call and branch on its resolution. The complete
source plan span may remain as checked ownership/debug data, but is not a
specialization worklist.

`result_mode` tells post-check lowering whether this is an ordinary value call,
method equality, structural hashing, parser derivation, or encoding derivation.
For method equality, the plan carries both `structural_allowed` and `negated`;
boxy lowering consumes those fields directly. `structural_allowed` is an
authorization, not a preference over checked evidence. A constrained or
otherwise evidence-dependent method equality consumes its explicitly planned
dictionary. A concrete builtin equality may lower directly to the same
structural-equality LIR used by explicit `==`/`!=` structural nodes over the
plan's two checked expression operands. If the plan names or requires a user
method, boxy lowering emits the direct or dictionary/vtable call named by the
checked plan instead of searching the method registry at LIR time.

Generic instantiation sites also carry one `CheckedEvidence` entry for every
method requirement in checked scheme order. Direct evidence names the exact
method target, concrete dispatcher checked type, concrete callable checked
type, and the target's nested evidence. Compiler-derived structural evidence
names its structural kind plus the same concrete dispatcher and callable
identities. A forwarded requirement records its enclosing evidence index
explicitly; checked errors and unreachable values remain distinct evidence
kinds.

Boxy dictionary planning consumes those entries one-for-one in dictionary slot
order. Each planned slot records the selected worker or structural operation,
the concrete callable type used by its adapter, and fully planned hidden
dictionary arguments derived from the nested evidence. Static dictionary
emission follows that plan directly. It must not look up the method again,
infer a generated parser or encoder from method-name text, or treat a missing
registry target as structural behavior.

The method registry is an exact table keyed by `(MethodOwner, MethodNameId)`.
It is not an owner-discovery mechanism. Post-check code may use it only after a
concrete monomorphic dispatcher type has already determined the owner.

Some method registry targets are generated structural targets rather than
procedure bodies. A nominal or opaque type can opt in to a compiler-derived
structural codec with an annotation-only associated method such as
`parser_for : _` or `encoder_for : _`. Canonicalization may represent this marker
as `e_anno_only` or, for hosted/type-module processing, as a zero-argument
`e_hosted_lambda`; `CheckedModule.method_registry` records it explicitly as a
generated parser or generated encoder target. Post-check lowering must consume
that explicit target kind and lower the structural parser/encoder from the
dispatch plan's concrete callable type. It must not treat the marker as a
procedure body, synthesize a fake source function, or infer generated behavior
from a missing procedure template.

The Boxy strategy represents compiler-owned opaque serialization evidence
explicitly. `Encoding.FieldName.FieldNames(_shape)`,
`Encoding.FieldName(_shape)`, and `Encoding.ParseTagUnionSpec(a)` are not the
zero-sized source backings that keep their implementation hidden from Roc
code. They have compiler-defined runtime representations whose complete shape
is selected by checking and Boxy planning. Generic format workers can receive,
store, and return those ordinary opaque values without specializing on the
phantom `_shape` parameter. Their compiler intrinsics consume the committed
representation directly; they do not recover a shape from source syntax or a
type descriptor.

Derived container encoders carry two explicit state types. The outer state is
accepted and returned by `encode_tag`, `encode_record`, `encode_tuple`, and
`encode_list`, and by each value-writer callback. The container callback and
its field or item writer instead thread a format-owned cursor type, which
may differ from the outer state. Checking validates that associated cursor type
through the format method's complete callback protocol;
Monotype consumes the same checked method shape when generating callbacks. It
must never assume that the two state types are equal or reconstruct one from
the other.

Canonicalization records each recognized associated underscore opt-in as an
`e_derived_method` CIR expression carrying its exact derived-method kind. An
ordinary annotation without a body remains `e_anno_only`; in a platform package,
only that ordinary form may be rewritten into a hosted declaration. Checking and
insertion into the checked method registry consume the explicit derived-method
kind and must not recover compiler intent from identifier text or the annotation
shape.

A derived parser or encoder is still finite, shape-specific generated code.
Boxy planning records a generated-codec worker from the checked structural
target, concrete dispatcher type, callable type, and nested checked evidence.
The constructor evaluates construction-time methods, builds the explicit
opaque evidence values, and captures them in the returned runtime callable.
The runtime worker contains direct field/tag branches and direct or dictionary
calls selected by the checked evidence. It must not call a central descriptor-
guided codec routine, interpret a runtime shape plan, or enter the LSS pipeline
as a fallback. Stored generated callables retain their generated worker kind
and compiler-assigned capture identities so restoration rebuilds the same
ordinary Boxy callable from `ConstStore` data.

Generated workers follow the same descriptor-production contract as source
workers. Planning assigns the exact descriptor source for every descriptor-
bearing output, including branch results, parser result tags, record fields,
tuples, and captured opaque evidence. Lowering initializes a fresh descriptor
local before constructing the associated value and emits aggregate construction
with those planned child descriptors. It does not copy a concrete aggregate
into erased storage and reconstruct its descriptor afterward. All successful
branches of a generated worker must therefore produce both the committed value
and the exact return descriptor required by its callable ABI.

Compiler-generated operands and callables use this contract at polymorphic
boundaries too. Quote conversion, numeral conversion, interpolation iterators,
and generated codec constructors carry an explicit result descriptor source
through callable packing. When they construct an aggregate, the aggregate's
descriptor environment is the composition of the already-recorded environments
of its fields or iterator state. Lowering must not recover that environment by
inspecting the packed value or by selecting a descriptor from the contextual
result type after construction.

### Structural Serialization Methods

Parsing and encoding are ordinary static-dispatch methods. Roc does not expose a
builtin `Parser`, `Decoder`, or `Encoding` interface type; the public model is
method-based.

The performance target is the same shape as hand-written systems parsers:
formats keep input state as cursors and slices, avoid runtime allocation during
parsing, receive the whole requested structural shape before scanning, and lower
to direct calls rather than callback tables, shape interpreters, or temporary
maps built for convenience. The compiler knows Roc structural shapes and method
requirements. It does not know JSON, HTTP headers, CSV, XML, or any other
serialized format.

A format is ordinary Roc code. Its type owns the methods that describe how that
format reads or writes each shape. Public modules expose small convenience
functions:

```roc
thing = Json.parse(json_str)?
thing = Json.parse_trailing_commas(json_str)?
thing = Json.Utf8.parse(json_bytes)?

json_str = Json.to_str(thing)
json_str = Json.to_str_try(floaty_thing)?
json_bytes = Json.Utf8.encode(thing)?

headers = Encoding.HttpHeader.parse(raw_headers)?
```

The convenience functions construct the internal format state directly, call the
value or type's ordinary method, validate the remaining state if the format
requires it, and return the final public value. A fallible helper such as
`Json.to_str_try` returns a `Try` and preserves the encoder's error type. This
is for values that cannot always be represented as JSON, such as `F32` or `F64`
values that are `NaN`, positive infinity, or negative infinity. An infallible
helper such as `Json.to_str` requires an empty encoder error type and returns
the string directly. They do not need a required `init`, `finish`, or `default`
hook. The runtime cursor types are implementation details of the builtin format
module, not public `Json.State` or
`Encoding.HttpHeader.State` APIs.

The underlying parse method is public and callable. It is deliberately curried:

```roc
a.parser_for : encoding -> (state -> Try({ value : a, rest : state }, err))
a.encoder_for : encoding -> (a, state -> Try(state, err))
```

`parser_for` is a method on the value type being produced. `encoder_for` is a
method on the value type being serialized. Structural types get these methods
from the compiler. Nominal types may define them explicitly, and structural
derivation uses those explicit nominal methods when a field, payload, list
item, nested value, or other sub-shape has that nominal type.

The `encoding` argument is the pure format/configuration value used to construct
the specialized parser. It may represent choices such as JSON object field
renaming, whether JSON accepts trailing commas, JSON tag representation, or a
header matching mode. The `state`
argument is the runtime cursor or output state. Keeping these separate matters:
parser construction can transform the requested structural shape before the
runtime scan starts, while the returned runtime function threads only the cursor
state and parsed values. Encoder construction can similarly precompute
shape-specific metadata before the returned runtime function receives the value
and output state.

For example, the builtin HTTP header helper inside `Builtin.Encoding` has this
shape:

```roc
HttpHeader := [MissingRequired, BadHeader].{
	parse : Str -> Try(output, HttpHeader)
		where [
			output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
		]
	parse = |raw| {
		Output : output

		parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)
		parsed = parse_output(HttpHeaderState.{ raw })?

		Ok(parsed.value)
	}
}
```

The important split is that `Output.parser_for(HttpHeaderEncoding.Caseless)`
constructs the concrete parser and the hidden `HttpHeaderState.{ raw }` is the
runtime input state. Formats with no configurable behavior can still use a
zero-sized internal encoding value.

The error type is inferred from the format methods. All `Try` errors in one
parse or encode operation unify with the public function's returned error type.
When a concrete encode operation cannot fail, its error type is empty, so
`Json.to_str` can bind the underlying encoder result with an exhaustive
`Ok(encoded_state) = ...` pattern and return `Str` directly. When a concrete
encode operation can fail, `Json.to_str_try` returns `Try(Str, err)` instead.

Checking derives structural methods by emitting ordinary static-dispatch
constraints. For example, deriving `a.parser_for` for a concrete shape asks the
encoding and state types for exactly the methods needed by that shape:

- `Str` calls the format's string method;
- records use compiler-generated field sets and the format's record-field
  method;
- tag unions call the format's tag-union method with a compiler-generated
  tag-union spec;
- lists, numbers, booleans, tuples, and other structural forms call the
  corresponding format methods;
- type aliases use their expanded structural shape;
- named nominal values call that nominal type's explicit method. If the method
  is missing, checking reports the missing static-dispatch requirement.

If a format does not support a shape, checking reports the missing method as a
static-dispatch error. Unsupported shapes are not represented as runtime parse
or encode failures. Runtime failures are reserved for input/output conditions
the format can only know while processing bytes or values, such as a malformed
header line, invalid JSON syntax, invalid UTF-8 in a byte input, or a
user-defined nominal method returning an error.

Compile-time evaluation uses the ordinary Roc constant machinery. The
serialization API does not add a special compile-time marker. A derived
`parser_for` constructs its transformed field sets and nested parsers before it
returns the runtime lambda. If that parser construction is evaluated during
checking, those transformed values are stored as checked constants and restored
later as ordinary Roc values. The returned runtime lambda then closes over only
the transformed field sets and nested parser functions. For a parser constructed
at compile time, original record field names that were renamed during
construction do not need to appear in the final runtime data.

Tag-union specs are opaque compiler values. They describe the concrete
structural shape being derived: tag names, payload shapes, and the concrete
payload result positions. They are not arity-specific user APIs, and userspace
code does not construct or pattern match on them. The compiler specializes every
use with the concrete tag-union type, so opaque spec operations lower to direct
tag code.

Userspace format code operates through safe Roc values, opaque specs, opaque
field values, iterators, and slice-returning string/list APIs. The compiler does
not expose raw field-slot indices, unsafe byte indexing, or unchecked memory
primitives as part of the serialization method surface.

Record parsing is driven by the compiler-generated structural `parser_for` method.
The compiler creates a `Encoding.FieldName.FieldNames(_shape)` value for each
concrete record shape:

```roc
Encoding.FieldName(_shape) : opaque
Encoding.FieldName.FieldNames(_shape) : opaque

Encoding.FieldName.FieldNames.rename_fields : Encoding.FieldName.FieldNames(_shape), (Str -> Str) -> Encoding.FieldName.FieldNames(_shape)
Encoding.FieldName.FieldNames.shortest_name : Encoding.FieldName.FieldNames(_shape) -> U64
Encoding.FieldName.FieldNames.longest_name : Encoding.FieldName.FieldNames(_shape) -> U64
Encoding.FieldName.FieldNames.iter : Encoding.FieldName.FieldNames(_shape) -> Iter(Encoding.FieldName(_shape))
Encoding.FieldName.FieldNames.for_size : Encoding.FieldName.FieldNames(_shape), U64 -> Iter(Encoding.FieldName(_shape))

Encoding.FieldName.name : Encoding.FieldName(_shape) -> Str
```

`Encoding.FieldName.FieldNames(_shape)` contains the requested field names and
compiler-owned result positions for one concrete record shape.
`Encoding.FieldName(_shape)` is an opaque handle to one field in that same shape. The
`_shape` parameter is a phantom type: it is not runtime data, but it ties a
field handle to the exact field set that created it. A parser for
`{ cache_control : Str, content_length : U64 }` cannot accept a
`Encoding.FieldName` produced from `{ foo : Str }`, because the phantom types do not
unify. That type-level tie is what lets generated record parsers avoid runtime
bounds checks on field handles. If the only way to obtain a
`Encoding.FieldName(_shape)` is from the matching
`Encoding.FieldName.FieldNames(_shape)`, then the compiler already knows every handle
is in range for that record. There is no user-exposed `U64` slot to validate at
runtime.

The derived `parser_for` constructs field metadata before returning the runtime
lambda:

```roc
renamed_fields = Encoding.FieldName.FieldNames.rename_fields(original_fields, |name| encoding.rename_field(name))
parse_nested = Nested.parser_for(encoding)
```

`encoding.rename_field(name)` is ordinary method-call syntax for a pure format
method whose first argument is the encoding value. Every encoding provides it;
identity is the normal implementation. Taking the encoding value as an argument
lets one encoding type store parser-construction configuration such as JSON
field naming style. `Encoding.FieldName.FieldNames.rename_fields` applies that
function to every requested record field, discards the original names from the
returned `Encoding.FieldName.FieldNames`, and rebuilds the length buckets used by
`Encoding.FieldName.FieldNames.for_size`, `Encoding.FieldName.FieldNames.shortest_name`,
and `Encoding.FieldName.FieldNames.longest_name`. If parser construction is
compile-time evaluated, the renaming work is also compile-time work. For JSON
camel-case decoding, the final runtime parser can contain only `camelCase`
field names. For HTTP header decoding, the final runtime parser can contain only
lowercase kebab-case header names such as `cache-control`.

Formats expose the methods needed for the shapes they support. A format that can
parse strings, `U64`, tag unions, and records uses these method shapes:

```roc
encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err)
encoding.parse_u64 : encoding, state -> Try({ value : U64, rest : state }, err)
encoding.parse_tag_union : encoding, Encoding.ParseTagUnionSpec(a), state -> Try({ value : a, rest : state }, err)

encoding.parse_record_field : encoding, Encoding.FieldName.FieldNames(_shape), state -> Try(
	[
		Field({ field : Encoding.FieldName(_shape), rest : state }),
		TryField({ name : Str, rest : state }),
		TryFieldCaseless({ name : Str, rest : state }),
		Continue({ rest : state }),
		Done({ rest : state }),
	],
	err,
)

encoding.skip_record_field : encoding, state -> Try(state, err)
encoding.missing_record_field : encoding, Str, state -> err
encoding.missing_optional_field : encoding, Str, state -> optional_err
encoding.rename_field : encoding, Str -> Str
```

For `Field`, `TryField`, and `TryFieldCaseless`, `rest` is the state positioned
at the field's value. If the field matches the target record, the generated
parser calls the parser for that field's type from that value-start state and
continues from the value parser's returned `rest`. This is what allows records
with different field shapes:

```roc
{
	content_length : U64,
	x_auth_token : Try(Str, [Missing]),
	cache_control : Str,
}
```

The record loop does not store every value as `Str` first. When it sees the
`content_length` field, it calls the `U64` parser from the value-start state and
continues from that parser's returned state. When it sees `cache_control`, it
calls the `Str` parser. The value parser owns value consumption.

`Field` means the format already matched the input field name against the
provided `Encoding.FieldName.FieldNames(_shape)`, usually by iterating
`Encoding.FieldName.FieldNames.for_size(fields, len)`
or another field iterator. `TryField` means the format parsed a field name and
asks the generated record parser to exact-match it against the transformed
fields. `TryFieldCaseless` is the same, but uses ASCII caseless matching. If a
`TryField` or `TryFieldCaseless` name does not match any target field, generated
code calls the format's `skip_record_field` method with the encoding and `rest`,
then continues with the returned state. This avoids scanning matched values
twice while still letting unknown fields be skipped correctly.

`Continue.rest` advances the record loop after the format has consumed input
that cannot be a relevant field. `Done.rest` is the state remaining after the
record ends. If the generated finisher sees that a required field was never
filled, it calls the format's `missing_record_field` method with the encoding,
field name, and final state to produce the format's concrete parse error value.
Optional fields are expressed by their field type, for example
`Try(Str, [Missing])`. If an optional field is absent, the generated finisher
calls the format's `missing_optional_field` method with the encoding, field
name, and final state at the optional field's error type and stores
`Err(missing)` in that field. This lets the format define the absence tag;
`Missing`, `Absent`, or any other tag name is ordinary userspace data, not a
compiler-known concept. A field annotated as `Try(Str, _)` can infer that error
type from the format method's return type.

Record-field dispatch is optimized around the assumption that serialized record
field names are overwhelmingly small. JSON object keys, HTTP headers, CSV
column names, XML attributes, environment variables, and similar schema fields
are expected to land in Roc's small-string representation almost all the time on
64-bit targets, and still most of the time on 32-bit targets. The optimization
strategy treats this as the hot path, not as a correctness requirement: long
field names remain supported, but generated code is arranged so that small names
take the shortest route.

Formats own conversion from Roc record field names to serialized field names.
HTTP header parsing can rename `cache_control` to `cache-control` at parser
construction time and then use `TryFieldCaseless("Cache-Control")` at runtime.
JSON camel-case parsing can rename `user_id` to `userId` at parser construction
time and then use `TryField("userId")` at runtime. The compiler does not know
those policies; it only knows that it has a transformed
`Encoding.FieldName.FieldNames(_shape)` value and a requested matching mode.

`Encoding.FieldName.FieldNames.shortest_name` and
`Encoding.FieldName.FieldNames.longest_name` are computed after renaming. Formats may
use them to skip impossible fields before doing more expensive work. For
example, if a header name is longer than
`Encoding.FieldName.FieldNames.longest_name(fields)` and the format's `rename_field`
never increases field length for headers, the format can consume the line and
return `Continue` without constructing any temporary field name. This is not a
parse failure: for formats such as HTTP headers and JSON objects, unknown fields
remain ordinary input according to that format's rules. If the target record
actually contains a long renamed field name, the long input field remains
matchable through the same `Encoding.FieldName.FieldNames` iteration APIs.

For small fields, generated record dispatch compares the packed small string
representation directly. Roc zeroes unused SSO bytes, so equality can use
fixed-width word comparisons without masking tail bytes. On 64-bit targets, the
generated dispatcher groups fields into 1-8, 9-16, and 17-23 byte size classes;
on 32-bit targets, the groups are scaled to that target's smaller SSO capacity.
The group selection can be implemented with a branchless or near-branchless
table lookup instead of a source-level length switch.

Within each size class, the compiler chooses the most discriminating word lane
for the concrete field set. For example, if several fields share the same first
eight bytes, the generated code can use the second or third word as the first
comparison instead. The hot miss path compares one machine word per candidate in
that class. Only after a discriminator hit does the code verify the full SSO key
with one, two, or three word comparisons and dispatch to the matched field's
already-constructed value parser. Collision-heavy classes may use another
discriminating lane or a generated perfect hash over the packed SSO words before
final verification.

This keeps the performance center on the common case: no heap allocation, no
runtime field map, no interpretation of a record plan, and no byte-by-byte
string comparison unless the selected format's field-name conversion itself
requires it. Long-field paths must preserve the same public behavior and memory
invariants. If a format must handle long fields without allocation, that path
must use field iteration and slice comparisons rather than constructing a
transformed heap `Str`; it is not allowed to make the SSO path slower for the
sake of generality.

Nested records follow the same construction/runtime split. The outer derived
`parser_for` method eagerly calls every nested parser constructor before
returning its runtime lambda. A nested record gets its own
`Encoding.FieldName.FieldNames(_nested_shape)` value, then renames and rebuckets that
field set through the same `encoding.rename_field` method. A custom nominal
field calls that nominal type's explicit `parser_for` method during parser
construction. At runtime the outer record parser dispatches to the
already-constructed field parser for the matched field shape.

Tag-union parsing follows the same separation. The format's tag-union method
receives the complete tag spec, identifies the input tag according to that
format's own rules, and uses opaque spec operations to parse and assemble the
selected payload. Recursive tag unions are ordinary recursive method calls
through the selected payload type. The compiler knows the Roc shape and the
static-dispatch requirements; it does not know any format-specific tag
representation. Tag-name renaming can use an analogous construction-time
transformation later; record field renaming does not require the compiler to
know any tag-union convention.

The generated code uses direct static calls. Tag spec matching is compiler-
generated exact matching over the concrete tag labels; userspace does not pass a
matcher function to spec operations. It does not pass user callbacks,
does not build a runtime interpretation plan, and does not route shape handling
through a central dispatch function. Generic userspace format code produces
record field events, iterates opaque field sets, and calls opaque tag spec
operations. The record loop and field dispatch are compiler-generated for the
concrete shape; tag spec operations are compiler primitives specialized for the
concrete tag-union shape and lower to direct code.

Input formats return seamless slices whenever the value being produced is a
slice of the original input. Parsing a `Str` from a larger `Str` or validated
byte buffer returns a slice into that buffer when the format can do so. The
format must validate bytes before producing `Str`; `Json.Utf8.parse` validates
string bytes from `List(U8)`, while `Json.parse` starts from an already-valid
`Str`. Hosts that pass request memory to Roc as `Str` must validate that memory
first and keep it alive for the duration of the request.

The HTTP header format receives only the raw header section, starting at the
first header line and ending before the blank line. Its record-field method
parses one CRLF-delimited line at a time. Each non-empty line must contain `:`;
otherwise the method returns the header format's bad-header error.

The header encoding's `rename_field` maps Roc field names to lowercase
kebab-case at parser construction time:

```roc
cache_control -> cache-control
content_length -> content-length
x_auth_token -> x-auth-token
```

At runtime the header parser parses the input line name as a seamless slice. It
may use `Encoding.FieldName.FieldNames.for_size` plus ASCII-caseless comparison
against `Encoding.FieldName.name` to match the transformed field set directly and
return `Field({ field, rest: value_start })`. It may also return
`TryFieldCaseless({ name, rest: value_start })` and let generated record
dispatch perform the ASCII-caseless match. If the name cannot match any target
field, the format consumes the line and returns `Continue({ rest: next_line })`.
Matching `Cache-Control`, `cache-control`, and `CACHE-CONTROL` against the
transformed `cache-control` field set does not require allocating a lowercased
copy. Header values are trimmed and passed to field parsing as seamless `Str`
slices. The format does not allocate a header map.

The JSON `Str` format receives valid UTF-8 text. The JSON `Utf8` format receives
bytes and validates UTF-8 before producing any `Str`. JSON record parsing scans
an object one field event at a time through the compiler-generated record loop,
so object key order does not affect performance beyond normal key matching. A
plain JSON encoding value can use identity `rename_field`. The same JSON
encoding type can carry a camel-case configuration value that renames Roc fields
at parser construction time:

```roc
user_id -> userId
cache_control -> cacheControl
```

The runtime JSON scanner can use `Encoding.FieldName.FieldNames.for_size` and exact
`Encoding.FieldName.name` comparison to match each object key against the
already-renamed field set and return `Field({ field, rest: value_start })` for
known keys. It may also return `TryField({ name, rest: value_start })` and let
generated record dispatch perform exact matching. For unknown keys, it skips the
JSON value according to JSON syntax and returns
`Continue({ rest: after_value })`. The matched field's parser consumes the JSON
value from `value_start`.

JSON tag unions use the externally tagged representation:

```json
{ "Admin": { "name": "Sam" } }
```

Zero-payload tags encode as the tag string, one-payload tags encode as
`{"Tag":payload}`, and multi-payload tags encode as `{"Tag":[...]}`. This
representation avoids collisions between tag names and ordinary record field
names. Other JSON conventions are represented by different JSON format values
with different methods. The compiler receives the null, missing-field, and
tag-union rules through explicit format methods rather than through hard-coded
JSON syntax recovery.

Parsing a Roc `Str` from JSON succeeds only for JSON string values. JSON `null`
and missing object fields are separate format conditions. They are surfaced only
through field or value types that request them, such as `Try(Str, [Null])` or
`Try(Str, [Missing])`; the plain `Str` method does not accept either condition.
`Try(a, [Null])` is the nullable JSON value shape. A format's
`missing_optional_field` method chooses the record-field absence tag for
optional fields; JSON uses `Missing`, but another format may choose `Absent` or
any other tag. `Try(a, [Missing])` and `Try(a, [Missing, Null])` are JSON's
record-field-only shapes: missing fields parse as `Err(Missing)`, explicit
`null` parses as `Err(Null)` only when `Null` is in the row, and encoding
`Err(Missing)` omits the field. Missing fields and `Null` are never conflated.

JSON arrays are used for lists, tuples, and sets. Tuples parse with exact arity.
Sets preserve `Set` insertion order and parse by inserting the array items.
JSON dictionaries use object representation only when the key type has a
lossless object-key codec: strings, bools, numeric types, and zero-payload tags.
Composite dictionary keys are rejected by static dispatch validation; there is
no automatic pair-array fallback. Dictionary and set encoders do not sort,
because Roc does not require keys or items to be sortable.

Concrete HTTP header parser code has this shape inside `Builtin.Encoding`:

```roc
HttpHeaderState :: { raw : Str }

HttpHeaderEncoding :: [Caseless].{
	rename_field : HttpHeaderEncoding, Str -> Str
	parse_str : HttpHeaderEncoding, HttpHeaderState -> Try({ value : Str, rest : HttpHeaderState }, HttpHeader)
	parse_u64 : HttpHeaderEncoding, HttpHeaderState -> Try({ value : U64, rest : HttpHeaderState }, HttpHeader)

	parse_record_field : HttpHeaderEncoding, Encoding.FieldName.FieldNames(_shape), HttpHeaderState -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : HttpHeaderState }),
			TryField({ name : Str, rest : HttpHeaderState }),
			TryFieldCaseless({ name : Str, rest : HttpHeaderState }),
			Continue({ rest : HttpHeaderState }),
			Done({ rest : HttpHeaderState }),
		],
		HttpHeader,
	)

	skip_record_field : HttpHeaderEncoding, HttpHeaderState -> Try(HttpHeaderState, HttpHeader)
	missing_record_field : HttpHeaderEncoding, Str, HttpHeaderState -> HttpHeader
	missing_optional_field : HttpHeaderEncoding, Str, HttpHeaderState -> [Missing]
}

HttpHeader := [MissingRequired, BadHeader].{
	parser_for : () -> (Str -> Try(output, HttpHeader))
		where [
			output.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try({ value : output, rest : HttpHeaderState }, HttpHeader)),
		]
	parser_for = || {
		Output : output
		parse_output = Output.parser_for(HttpHeaderEncoding.Caseless)

		|raw| {
			parsed = parse_output(HttpHeaderState.{ raw })?
			Ok(parsed.value)
		}
	}

	parse : Str -> Try(output, HttpHeader)
}
```

The exact derived parser type for a header record with mixed field shapes is:

```roc
{
	cache_control : Str,
	content_length : U64,
	x_auth_token : Try(Str, [Missing]),
}.parser_for : HttpHeaderEncoding -> (HttpHeaderState -> Try(
	{
		value : {
			cache_control : Str,
			content_length : U64,
			x_auth_token : Try(Str, [Missing]),
		},
		rest : HttpHeaderState,
	},
	Encoding.HttpHeader,
))
```

Because `Encoding.HttpHeader` does not define `parse_tag_union`, trying to parse a
header record that contains a tag union is a compile-time static-dispatch error:

```roc
bad : Try({ mode : [On, Off] }, Encoding.HttpHeader)
bad = Encoding.HttpHeader.parse("mode: On\r\n")
```

```text
checked modules
  -> Monotype
  -> Monotype Lifted
  -> optional Monotype Lifted SpecConstr with capture finalization
  -> Lambda Solved
  -> explicit solved inline plan
  -> direct SolvedLirLower
  -> TRMC, join scalarization, box reuse, return-slot rewriting
  -> optional tag reachability
  -> reachable-procedure pruning
  -> ARC insertion
  -> backend, interpreter, or LirImage
```

`src/lir/checked_pipeline.zig` owns this order. Dev and interpreter builds do
not take a separate Lambda Mono or LIR-lowering route. Size and speed builds do
not bypass Lambda Solved. All modes therefore consume the same Monotype type
identities, the same Lambda Solved callable information, and the same direct
Solved-to-LIR representation decisions.

The explicit `InlineMode` controls the optional specialization work:

- `.none` skips Monotype Lifted SpecConstr and produces an empty solved inline
  plan. Dev and interpreter modes select this.
- `.wrappers` runs SpecConstr and produces wrapper-inline decisions from
  Lambda Solved. Size and speed modes select this.
- optimized eval and focused lowering tests may select `.wrappers` directly.

The mode is compiler input supplied to the checked pipeline. SpecConstr and the
solved inline analyzer consume it directly. They do not infer optimization mode
from the target, backend, symbol names, builtin names, or emitted code. The mode
changes optimization work, not source meaning or the stage route.

Monotype Lifted closure lifting outputs complete function capture slots and
function-reference/direct-call operands. When `.none` skips SpecConstr, the
pipeline consumes that output directly and must not repeat capture analysis.
SpecConstr can change free-variable use while cloning and rewriting function
bodies, so SpecConstr owns one exact capture finalization before it returns its
Monotype Lifted output. No later pipeline stage repeats that finalization.
Each post-lift capture operand explicitly names the callee capture slot it
supplies. Capture finalization preserves that key while rewriting the operand
value and never infers the target slot from the value's own capture identity.
At the lift boundary, each declared key's namespace states how it joins:
source-authored and check-generated keys normalize once through the target slot's
checked identity, while lift-generated keys already name the target's lifted
slot and remain exact. After that boundary, capture recomputation accepts only
lifted keys; it never retries a lookup in another identity namespace.

Optional tag reachability uses a recursive abstract value tree. A struct field
or tag payload carries the complete nested `ValueInfo` output for the value
stored there, and every `set_local` join merges that complete tree. The
pass may remove a switch edge only from this producer-complete fixed point; it
must not flatten nested values to their top-level tag set, because an iterator
successor can change a tag nested below both a step payload and the loop-carried
state record.

`SolvedLirLower` computes the logical Lambda Mono callable, capture, procedure,
and function-free type decisions while directly consuming Lambda Solved syntax.
Release builds do not materialize a second Lambda Mono expression, pattern,
statement, or local tree. Debug builds separately materialize Lambda Mono and
compare its decisions with the direct lowerer; that verifier is not a production
lowering route.

#### Public Iterator Contract

`Iter` and `Stream` remain public Roc builtins with their existing source
types:

```roc
Iter(item) :: {
    len_if_known : [Known(U64), Unknown],
    step : () -> [One({ item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done],
}

Stream(item) :: {
    len_if_known : [Known(U64), Unknown],
    step! : () => [One({ item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done],
}
```

Adapters, custom sources, and consumers remain ordinary Roc functions. There is
no public chain type, iterator trait, extra public step tag, or source-visible
compiler representation. Internal representation data is attached only after
checking, when Monotype creates concrete iterator call results.

#### Explicit Iterator Representation Tiers

A Monotype named type definition records an explicit iterator representation
decision:

```zig
const IteratorRepresentation = enum(u8) {
    none,
    minted,
    forced_dynamic,
};

const IteratorKind = enum(u8) {
    none,
    custom,
    list,
    single,
    range_exclusive,
    range_inclusive,
    map,
    keep_if,
    drop_if,
    take_first,
    drop_first,
    concat,
    append,
    forced_dynamic,
};

const TypeDef = struct {
    // declaration identity fields
    generated: ?TypeDigest = null,
    iterator_representation: IteratorRepresentation = .none,
    iterator_kind: IteratorKind = .none,
    iterator_depth: u8 = 0,
};
```

The fields have these meanings:

- `none` is the ordinary public nominal. It carries no internal chain identity.
- `minted` is a statically bounded internal chain representation.
  `generated` is its chain/callable-evidence digest and `iterator_depth` is
  the producer-computed chain depth. `iterator_kind` records the exact source
  or adapter that produced it.
- `forced_dynamic` is the explicit fixed-point representation selected at the
  mint-depth boundary. It retains the public declaration identity while the
  representation field keeps it distinct from the ordinary public nominal.

These fields participate in named-type equality, cross-store equality, and type
digests. Every type-store translation copies them. A later stage never derives a
tier, producer kind, or mint depth from lowered type shape.

For a minted iterator, Monotype rewrites the public recursive `rest` type in the
step result to the minted self type and records concrete adapter components as
additional nominal arguments. Each adapter layer therefore embeds its concrete
predecessor by value. A bounded chain is a finite tower of distinct nominal
identities rather than one public nominal with a recursive self edge.

The representation producer is `generatedIteratorNode` in
`src/postcheck/monotype/lower.zig`, together with
`InstGraph.finalizeGeneratedIteratorRepresentations` in
`src/postcheck/monotype/solve.zig`. Construction records the exact public
source, producer kind, component nodes, callable evidence, and private backing
in the active instantiation graph. Finalization consumes that complete graph
before any durable Monotype type is sealed. Together they compute:

- `List.iter` as a first-class source representation rather than a public
  recursive `Iter` boundary;
- source depth 1;
- adapter depth as one plus the maximum minted depth reachable by value through
  its components;
- a hard minted depth limit of 16.

A public `Iter` expected type constrains the checked result type; it does not
veto producer-owned representation evidence. A source or adapter whose inputs
prove a bounded chain mints its concrete result and relates that result to the
public type during checking. This keeps constant and non-constant chains on the
same representation path.

A `minted` child contributes its recorded depth. A `forced_dynamic` child
contributes the cap, so every adapter above it remains dynamic. Ordinary named
arguments, records, tuples, tag payloads, lists, and boxes propagate the maximum
depth of values they contain. Function types do not contribute stored chain
depth, and named backings are not traversed.

If the next chain would exceed the limit, Monotype interns one
`forced_dynamic` iterator type per item-type digest. Its public-shaped backing
is recursively rewritten to its own type, giving recursive construction a
finite type fixed point. An exact memoized walk over the finite instantiation
graph computes the maximum stored iterator depth; a value cycle selects the
explicit forced-dynamic fixed point. Graph size alone never changes the
representation decision.

Recursive specialization contributes an explicit second proof of the dynamic
tier. Each in-progress specialization snapshots every permanent member of each
ordered argument's union class. When a recursive edge reaches that
specialization, a request argument introduced after the snapshot is recorded as
a representation-growing recursive slot before the two function interfaces are
related. If that slot subsequently joins distinct minted iterator identities,
the graph records that the resulting iterator class must use the forced-dynamic
fixed point. Recursion through any alias already present in the initial class is
not representation growth and remains eligible for the minted tier. This makes
the distinction producer-authored: finalization consumes the recorded recursive
edge and minted join instead of inferring recursion from a finished type shape,
union-find root selection, or call-stack depth.

A loop `continue` edge is likewise explicit producer-authored recursive value
flow, so every loop-carried slot on that edge is recorded even if an earlier
assignment already joined it to the loop parameter. Recording the slot does not
by itself force dynamic representation; only its later join of distinct minted
identities does.

An imported finished Monotype can participate in such a minted join, but it has
no live graph provenance because its producer has already sealed its identity.
When exactly one side of the join is a graph-owned iterator, union preserves
that side as the class authority. Representation finalization then consumes its
explicit public source and producer topology. It must not depend on operand
order or keep the imported root and thereby discard the only data capable of
authoring a forced-dynamic representation.

The recursive edge itself is also producer-authored. Every draft function and
globally reserved root records the owner that created it, forming an explicit
active ownership tree. A partial open-interface match may reuse an in-progress
specialization only when the current owner descends from that specialization in
this tree. Shared graph cells alone cannot classify two sibling calls as
recursion. Exact completed interfaces may still deduplicate normally, but only
an explicit ancestor edge invokes recursive-interface unification and records
recursive representation growth.

Finalization rebuilds a selected forced-dynamic class with exactly one public
item argument and an exact self-recursive backing before identity sealing.
It does not restamp a minted backing whose component arguments still encode the
growing chain. Once representation finalization, identity sealing, and graph
freezing finish, the durable Monotype is immutable and no consumer may reopen,
widen, or reinterpret it.

Iterator-for lowering obtains the step result shape from the exact generated
iterator node when one is present. The checked step type supplies the public
interface and topology only; it cannot replace or merge the producer-owned
private `rest` representation. This keeps the loop's initial state and every
back-edge state in the same explicit representation family.

Nested call and dispatch operands carry producer evidence through the active
instantiation graph until that graph's single final seal. Relation production
passes the exact result node to the consuming call request; it does not seal an
intermediate `TypeId`, re-import that snapshot, or fall back to the checked
public cell after discarding private representation evidence.

When a dispatch expression produces generated-private evidence for a live
checked-public result cell, Monotype selects that representation through the
dedicated `selectGeneratedPrivateRepresentation` capability before lowering
the dispatch. The capability exists only during relation production, requires
an explicitly directed public-to-private edge, and rejects every class that
contains an imported finished Monotype. Ordinary graph unification rejects the
same edge. This preserves producer selection for branch results without making
public/private merging—or reopening a durable Monotype—available as a general
unification behavior. If the requested public interface is already a finished
Monotype, the producer relates its distinct private result to that immutable
interface without merging either class, and the enclosing procedure or
compile-time wrapper carries the private result cell as its exact output
witness. ConstStore preserves that witness beside the stored value, and restore
relates the checked public interface to it without ordinary unification.

A value-producing `if` or `match` likewise owns one explicit result selection
for all of its inhabited branches. An exact generated-private request already
supplied by the caller remains authoritative. Otherwise, before emitting any
branch body, Monotype asks every branch's checked producer for its exact result
evidence and joins all generated-private evidence into the shared live result
selection. Public-only evidence does not settle the selection. Match patterns
first project their exact binder cells from the shared scrutinee, so a branch
producer reached through a pattern lookup participates in the same pre-emission
pass. Every branch is then emitted once against the settled request. Distinct
minted iterator producers therefore use the ordinary graph representation join,
which keeps a compatible static representation and reaches the defined
forced-dynamic fixed point only when the producer topology requires it. Source
order cannot make one already-emitted branch authoritative, and lowering never
needs to revise emitted branch code. Only after all branches have been lowered
does the selected result relate to the outer interface and seal. Representation
selection never reconstructs branch evidence from finished output IR or
reopens a durable Monotype.

Branches that provably terminate do not participate in result selection. If
every branch terminates, the control-flow expression produces no runtime value:
its checked result variable remains unconstrained, no result relation is
created, and the expression carries the enclosing continuation's declared cell.
An unobservable continuation type is not representation evidence.

Record constructors preserve that distinction structurally. If a field is a
finished generated-private witness, the constructor emits a distinct record
witness that references the field directly and relates that record to the
checked-public container. It never merges the child into the public field cell
or asks a later consumer to recover the child's runtime representation from the
public container shape.

Each generated-private request also retains its exact checked-source function
node. That source node can itself contain upstream private arguments, so a
callee relates its fresh checked root to the source through opaque interface
relations; it never fully unifies the two function graphs. Expected private
results remain request-owned nodes while their checked result cells stay fresh
public interfaces. This keeps an adapter's input and output identities distinct
even when the source signature uses one public `Iter` type variable for both.

Match lowering likewise relates each checked pattern interface to the exact
scrutinee node without merging a generated-private root into that public
interface. Once all pattern relations have settled, record-field/tag-payload
traversal walks the checked pattern and rebinds its pre-registered locals to the
exact child graph cells before the guard or branch body is lowered. Later pattern
materialization consumes those same cells. Branch code therefore specializes
from producer-owned representation evidence rather than from the checked
pattern's public approximation.

The cap is a type-universe bound, not a call-depth or specialization-request
counter. Every generated iterator passes through the same graph-owned producer
and pre-seal finalizer, so recursive functions, loops, and ordinary calls all
receive the same finite representation decision.

#### Tier Unification And Callable Flow

Monotype instantiation and Lambda Solved unification consume the representation
tier explicitly:

- a forced-dynamic iterator wins when related to a minted or ordinary public
  iterator with the same source declaration and item type;
- a minted iterator wins when related to its ordinary public source type;
- distinct minted iterator identities join their item and backing information
  without discarding callable members;
- equal tiers use ordinary named-type equality.

At a forced-dynamic relation, Lambda Solved always unifies the public item type.
A minted peer also joins its generated-private backing into the dynamic backing;
an ordinary public peer has no private representation authority, so its backing
is not merged or reinterpreted.

Lambda Solved transfers callable evidence across a public-to-generated relation
with a separate structure-preserving walk. That walk validates corresponding
public and private structure while retaining both sealed Monotype roots, and it
unifies only callable slots and still-open Lambda Solved slots. This makes a
SpecConstr-authored callable worker visible in the exact private representation
that contains it without using the public representation as a replacement.

When a complete Monotype type clone contains a forced-dynamic iterator,
Lambda Solved marks the callable in that iterator's backing as erased. The mark
runs only after the clone is structurally complete, so the erased callable's
source-function digest never observes a partially built type. The erased
callable then accumulates exact finite members through normal Lambda Solved
unification.

Minted iterator backings keep finite callable slots inline. Only
forced-dynamic backings take this explicit erased-callable boundary. The direct
LIR lowerer dumbly consumes the solved result: finite callables become generated
tag-union values; erased callables become packed erased-callable values and
indirect calls. It does not apply iterator depth policy or repair callable
variant sets.

#### SpecConstr And Loop Scalarization

Monotype Lifted SpecConstr is a general call-pattern specialization pass. It
runs only when `InlineMode` is not `.none`. It consumes explicit lifted
constructor and callable values and creates workers whose arguments are the
parts the callee immediately observes.

Iterator and stream loops are important clients. After wrapper inlining exposes
a known iterator constructor, SpecConstr can:

- split known record, tuple, tag, nominal, and callable arguments into leaves;
- redirect matching direct calls to specialized workers;
- simplify field reads and matches from known values;
- scalarize loop-carried constructor state;
- supply each reachable `continue` edge with the scalar leaves required by the
  loop fixed point.

When the continuation observes only part of a compiler-generated tuple loop
result, SpecConstr may narrow the loop's exit ABI without narrowing its
back-edge state. That rewrite is lexical: the ordinary full expression clone
carries the selected exit ABI while cloning the owning loop body, rewrites
every `break` owned by that loop wherever it occurs (including inside mixed
value-producing branch arms and statement values), and pushes an explicit null
selection while cloning a nested loop body. Initial values remain in the
enclosing lexical loop context. Re-cloned output breaks carry an explicit
SpecConstr-owned selected-ABI stamp, so normalization propagates the already
completed transfer instead of trying to recognize it from its scalar shape.
It is invalid to change the loop result type after rewriting only a terminating
spine or a subset of exits; every selected exit must transfer exactly the
explicitly selected tuple items.

Iterator classification in this pass consumes the explicit iterator
representation field (or the checked public `Builtin.Iter` identity). It does
not identify generated iterator types solely from a nullable generated digest.
The checked public identity is an interned module-and-declaration identity, not
a comparison against type-name text. Adapter-specific rewrites consume the
exact checker-authored `IteratorProcedureId` on the call. The procedure id
identifies the operation; that operation's declared lowering contract supplies
its producer/non-producer role and operand roles, while the solved result type
supplies its explicit representation. A result type's `iterator_kind` describes
the representation but does not by itself prove that an arbitrary expression
constructed it.

Monotype Lifted remains source-shaped when SpecConstr begins: calls and other
strict computations can still occur inside constructor operands and branch
arms. Evaluation order is therefore owned by the clone result, not recovered
from expression ids or assumed from constructor shape. Every cloned symbolic
`Value` is paired with one ordered `BindingChain` containing the strict work
that produced its opaque leaves. The structural owner places that chain at the
source evaluation position before allowing the value skeleton to flow.

SpecConstr collects call patterns from exact direct-call and callable
identities, then performs one value-aware normalization clone of every original
body. This complete traversal replaces the former routing scans that guessed
which bodies contained shape demand, recursive workers, or iterator loops.
Known loop state is handled by the loop clone's explicit fixed-point shapes;
adapter-specific transforms match exact stamped calls. The pass does not scan
whole bodies to classify branch-chosen loops, count construction-call depth,
recognize iterator types by text, or set a guessed body category that changes
how a later clone interprets opaque calls.

Monotype can assign distinct local ids to uses of one checked pattern binder at
one monomorphic type. SpecConstr therefore keeps lexical binder aliases separate
from known-value evidence. Every active binding records its exact local and an
alias keyed by checked binder id plus monomorphic type digest; whole-body
normalization seeds the alias index from the function's arguments and captures.
A separate value index exposes only known structure and loop-carried state to
specialization decisions. Consequently an opaque binder use still resolves to
its active cloned local, but that lexical resolution cannot authorize inlining
or constructor specialization.

Analysis exhaustion is explicit. A bounded query returns `proven`,
`disproven`, or `unknown_budget_exhausted`; only `proven` authorizes a rewrite.
Hard generated-code limits may decline proven work, but they never change a
rewrite-legality result. Exact function uses, source-return presence, and
tail-self-call summaries are collected together as `ProgramProcedureUsage`;
worker localization collects a fresh snapshot after each graph mutation instead
of performing independent per-candidate body scans. A source-relative early
`return` still prevents procedure-to-join localization until lifted returns
carry explicit continuation targets.

Each normalization or discovery clone owns a short-lived scratch arena. Only
accepted call patterns are copied into pass-wide storage, so discarded symbolic
value graphs do not accumulate across functions. Generic analysis follows
existing constructor evidence; a structural consumer requests a producer's
result shape through the separate demand path, which propagates through the
callee's exact used-argument plan.

Inlining a call additionally requires exact closed Monotype identity between
the call-site result and the callee body's result. Independently specialized
graphs may prove a public/generated or minted/forced-dynamic relation without
giving the two results the same runtime representation; that relation is not
permission to substitute the callee's private value into the caller. Such a
call remains explicit for Lambda Solved to consume at the representation
boundary. Rewritten callable workers are likewise keyed by the source template,
the exact callable-use ABI, and the exact capture ABI, so one worker is never
shared across distinct specialization graphs merely because its lexical source
and captures happen to match.

The useful lesson from GHC's SpecConstr is this separation of concerns. GHC's
`Value`/`CallPat` data and `ScEnv` substitution/value environments carry
constructor evidence; simplifier floats own strict work; and specialization
count/size controls bound compiler and generated-code growth without becoming
evidence. Roc follows that ownership model, but does not copy GHC's occurrence
guesses or syntax-driven constructor recognition; Roc has checked procedure
identities, solved representations, explicit keyed captures, and typed
exhaustion results available directly.

SpecConstr is not responsible for making bounded iterator representation
allocation-free. Per-chain minting removes the recursive layout edge in every
mode. SpecConstr improves optimized loop and call shape so later lowering and
LLVM see scalar state and direct operations.

SpecConstr preserves shared control explicitly. When a rewrite would move one
continuation under multiple `match` or `if` arms, it introduces typed lifted
join points and replaces each arm result with a jump. It must never copy
continuation code into more than one arm, which keeps the amount of stored
continuation code independent of branch count and nesting. Within that rule the
rewrite preserves the arms' statically known value structure: it declines
entirely when an arm's result is opaque (an ordinary let binding keeps
downstream tail-call and loop-shape recognition intact); a continuation that
immediately matches the bound value gets one join per continuation branch, and
only the small dispatching match is copied into the arms, where it folds
against each arm's known constructor into a direct jump; a join's parameters
are the decomposed leaves of the values its jump sites supply whenever those
values agree on one structure skeleton, so specialization inside the shared
body still sees the shape; and a join with exactly one jump site is inlined only
when its body is closed under lexical loop control. A body containing a `break`
or `continue` for an enclosing loop retains the typed join, because moving that
body beneath a different loop would retarget the transfer. Otherwise its body
is cloned directly at the site against the site's full symbolic values.

Call-pattern specialization may also expose a tail-recursive worker whose
entire specialized ABI is scalar even though its only external call remains in
the function that initiated specialization. When such a generated worker has
exactly one direct external call, is never used as a function value or root,
and every self call is in a proven tail position, SpecConstr localizes it as a
recursive typed join point at that call site. The worker arguments and keyed
capture operands become join parameters, the external call becomes the initial
jump, and tail self calls become back-edge jumps. This is a code-motion proof,
not a size heuristic: one external use proves the body is not duplicated, and
the syntactic tail-position proof preserves the recursive control boundary.
Workers with procedure-relative early `return`s remain procedures until return
continuations are explicit in the lifted IR. Localizing after worker creation
lets iterator clients retain the scalar ABI specialization already computed by
the general call-pattern machinery, so an enclosing fold can contain the same
self-contained scalar loop as a source loop without changing iterator runtime
representation.

SpecConstr separates symbolic structure from strict work. A cloned value is a
pair of an owned `BindingChain` and a symbolic `Value`. The chain contains the
strict computations which produce the value's opaque leaves, in source
evaluation order. Before a value may be reused through substitution, every
non-work-free leaf is named in that chain and replaced by the resulting local;
budget exhaustion names the entire remaining sub-value as one strict binding.
Cloning a constructor concatenates its children's chains in field or item order.
Cloning a sequential construct consumes the producer's chain before cloning its
continuation and places that chain structurally before the continuation. Cloning
a branch places each arm's chain inside that arm. Introducing a join keeps
bindings before the case outside the join, keeps arm bindings around the
corresponding arm jump, and keeps continuation bindings in the join body. No
binding chain is stored in ambient cloner state, and a nested clone cannot
observe, capture, flush, or move a chain owned by its caller.

This follows the useful ownership discipline of GHC's simplifier floats: an
expression transformation produces an ordered binding collection together with
its expression, collections concatenate in evaluation order, and the owning
structural boundary wraps them around the result. Roc also preserves GHC's
important distinction between purity and speculatability: knowing that an
expression has no language-level effect does not prove that evaluating it early
or not at all preserves strict source evaluation behavior.

This strict chain is the ordering proof. SpecConstr does not count effectful
expressions, record emission windows, recover ordering from expression ids, or
scan cloned bodies to decide whether a binding may cross another binding.
Code motion is a separate decision. Language-level purity is necessary but not
sufficient: a pure call can diverge, and a compiler-authored pure procedure can
contain ordered implementation mutation. SpecConstr therefore does not ask an
opaque computation for permission to move. It names the computation once in
the chain owned by its original position, substitutes only the resulting local,
and discards only structurally work-free value construction around those named
leaves. This keeps iterator structure visible without discarding or commuting a
call, loop, low-level operation, or control transfer. Any future optimization
which does move opaque work needs an explicit earlier-stage total-and-
speculatable proof; it must never manufacture one by scanning a procedure body.

The append-tail peel is narrower than general value-aware cloning. It applies
only when an exact `Iter.append` chain shares one base and a structural proof
shows that the branch condition or scrutinee, guards, and appended items contain
no call, low-level operation, loop, control transfer, collection allocation, or
diagnostic operation. In that work-free case replaying the constructor plan
after the shared base loop cannot move strict work. If any such work exists, the
peel is declined and the ordinary value-aware clone retains the source branch
and its `BindingChain` in source order. In particular, SpecConstr never removes
a source branch and later reuses an effectful condition, scrutinee, or item.

In Debug builds, placing a `BindingChain` verifies its forward/back links and
the type of every binding, and the Monotype Lifted body verifier checks local
scope plus join scope and arity. These checks make a lost binding, an arm-owned
binding escaping through a malformed join, or a chain linked out of source
order a compiler bug.

A loop-carried variable's reassigned copies share its source binder but not
its local id, so once a loop clone rebinds the carried slot, binder identity
is the only path those copies resolve through. Cloning a loop therefore drops
the pre-loop binder value, installs the emitted param under the slot's binder
identity for any value variant—an opaque scalar param must reach the copies
too—and, while the loop clone is active, keeps a reassigned carried binder's
entry pointing at its latest merged value across the restores of escaping
`let` clones. Arm and join boundaries restore plainly, so a reassignment
inside one branch never leaks to its sibling or past the join. Resolving a
carried read to anything else is unsound in one of two ways: a read that
reaches a vanished pre-loop local becomes a phantom argument when capture
recomputation promotes the dangling reference, and a read that reaches the
loop-entry value silently discards the reassignment. Two Debug validators
guard the pair: no rewritten function may gain a capture its source did not
declare, and every local reference in a rewritten body must resolve to an
in-scope binding, argument, or recomputed capture.

Every SpecConstr clone is hygienic. A retained pattern, loop parameter, join
parameter, try-sequence local, or other runtime binder receives a fresh lifted
local identity in each emitted copy, and every occurrence in that binder's
lexical scope is rewritten to the fresh identity. A binder whose uses were
fully replaced by a known value still receives a fresh identity in the emitted
pattern, but that unused output identity does not replace the known-value
substitution. Cloning never relies on later lowering to reconstruct lexical
scope from reused local ids: distinct emitted binders are distinct explicit
identities before Lambda Solved or LIR lowering consumes them.

#### Constant Storage

Compile-time finalization is separate from iterator representation and
SpecConstr. Eligible constant list values become explicit
`static_data_candidate` nodes, and direct LIR lowering emits their bytes into
the data segment. This is why a constant list consumed through `.iter()` can
have zero runtime list allocation in a size cart even though the eval allocation
harness, which does not perform final constant hoisting, observes one base-list
allocation.

Strings and flat scalar lists use one shared content-interned blob store.
`List(U8)` therefore has the same constant-storage cost as a `Str` containing
the same bytes, and equal string/list contents reuse one blob. A packed list
view records its scalar encoding and item count separately from its byte
view. Lists whose items contain pointers or structured values remain
explicit child-node lists so their graph edges and sharing stay visible.

When packed list views reach LIR, the shared literal backing records the maximum
alignment required by every view. Each view offset must also satisfy its own
item alignment. Static-data materialization aligns the backing to that
maximum while keeping the Roc list length and capacity in items rather than
bytes.

LLVM codegen interns one refcounted backing global per blob for the whole
module, but the pointer to the blob's data offset is a WipFunction
instruction: every proc body that restores a view must emit its own GEP from
the interned global. Caching the offset pointer itself would leak the first
body's instruction into every later function sharing that backing.

The direct LIR const plan also records the root's exact Monotype return type.
Finalization clones that type into the durable `ConstStore` type store and saves
its id beside the stored root node. Restoration lowers the saved root type first
and restores the node at that exact type; the checked public type is used only
to assert that the saved representation has the checked root type.
Representation evidence therefore survives CTFE without a consumer
reconstructing it from constant node shape.

For a finite callable inside that exact witness, the Lambda Solved function
type node is the sole authority for the durable `ConstStore` function type.
Runtime callable variants may have different specialization-private Monotype
signatures; the const writer never chooses one variant or requires those
private signatures to be identical in order to reconstruct their shared source
interface.

#### Correctness Boundaries

All modes must preserve the same observable Roc behavior. The optional
specialization mode may change compile-time work and generated shape only.

The following properties require focused tests:

- bounded iterator chains remain minted and have no iterator-attributable box or
  erased callable;
- recursive and over-cap chains terminate and use the forced-dynamic callable
  representation;
- forced-dynamic callable sets contain every member that can reach the boundary;
- wrapper mode and ordinary mode agree on values and effects;
- iterator loop scalarization preserves every reachable transition, including
  adapter-state changes across `continue` edges;
- constant-list zero-allocation claims are tested on a cart path that performs
  compile-time finalization;
- direct Solved-to-LIR decisions agree with the debug materialized Lambda Mono
  verifier.

Backends receive only ordinary LIR and explicit ARC statements. They must not
know whether a value originated as a public iterator, a minted iterator,
forced-dynamic callable state, or a scalarized loop.

## Solver-Mutating Rewrites

Pure unification is the authority on what typechecks. Any code that mutates
the solved type graph or restamps stamped dispatch-plan metadata outside ordinary
unification is one of two things, and must say which:

- Mechanism: the rewrite cannot change which programs typecheck or which
  plans are output for error-free programs. Examples: diagnostic recovery
  on an already-reported error, a descriptor fast path that writes exactly
  what a unify would have produced, recycling of orphaned vars.
- Policy: the rewrite makes a program typecheck that pure unification would
  reject, or changes checked-module output for error-free programs. Every policy
  rewrite implements a rule declared in this document, is named for that
  rule, and has tests pinning both its accepted and its rejected side.

The distinction exists because a probe-then-mutate rewrite is
indistinguishable at review time from a change to the language's typing
rules: it passes its own repro, and nothing in the type system flags that
subsumption or dispatch policy changed. The solved-graph mutation primitive,
`Store.dangerousSetVarRedirect` (src/types/store.zig), therefore requires a
`RedirectRule` enum member naming the declared rule each call site operates
under—an unreasoned redirect does not compile, and adding a caller means
adding or citing a member, which is greppable and reviewable. A new
probe-then-mutate rewrite requires a declared rule in this document first;
"it makes a test pass" is not a rule.

### Hosted Try Question Widening

`?` unwraps a `Try` condition and re-raises its error row into the enclosing
function's return row. When the callee's error row is closed and the
enclosing annotated return's row is open (a rigid extension), ordinary
unification rejects the pair, and that mismatch is a type error by design: a
closed error row is not widened into an open annotated row at use sites
(issue #9798's program is rejected).

The one declared exception is a direct call of a hosted function. A hosted
function's boundary type is an ABI contract keyed by its declared closed row
(see Host Symbol ABI), so the hosted callee cannot adopt the caller's wider
row, and requiring callers to re-tag hosted errors by hand would make hosted
functions unusable with `?`. When the `?` condition is a direct call of a
hosted function—the call's function expression resolves statically to an
`e_hosted_lambda` def; dispatch calls and value-carried functions never
qualify—and every visible error in the callee's row is included in the
expected row (same tag names, mutually usable payloads), the checker widens
the condition at the use site: the condition's root is redirected to a fresh
`Try` at the expected row (`widenTryConditionForExpectedReturn`, cited as
`RedirectRule.hosted_try_question_widening`), leaving the hosted callee's own
declared type untouched. Monotype lowering gives a widened hosted
specialization request a generated Roc adapter at the requested type that
calls the declared-type boundary and re-tags the error into the wider row,
so the extern boundary itself is always emitted at the declared row.

This rule decides which programs typecheck, and that is all it decides. It is
not what keeps the host ABI intact: the extern boundary is pinned by the
producer-side check in Monotype lowering (see Host Symbol ABI), which admits
only the declared type no matter what a use site's type turned out to be. So
the rule can be tightened, loosened, or replaced on typing grounds alone.

Both sides are pinned by tests: accepted—
test/fx-open/issue_9963_hosted_try_question_mark.roc (a direct hosted `?`
inside an open-row platform function builds and the host's Ok is observed as
Ok); rejected—test/fx-open/hosted_try_question_not_included.roc (a direct
hosted `?` whose enclosing annotation omits the hosted error is a type
error), and the issue #9798 regression test in
src/check/test/type_checking_integration.zig (a non-hosted `?` into an open
annotated row is a type error even when the visible errors are included).

### Derived Parser Tag-Row Closure

A compiler-derived structural parser owns the exact set of tags it can
construct. When parser dispatch reaches a tag union with at least one known tag,
an unconstrained flexible extension is therefore closed to the empty tag union
while validating that derived parser. The closure uses ordinary unification and
applies recursively to tag unions in payloads and container components. It does
not close a bare flexible shape before a tag union exists, and it does not close
a rigid extension: a polymorphic open row may contain tags for which no parser
was checked, so that parser dispatch is rejected.

This is the parser counterpart of derived encoder validation, which already
closes an unconstrained flexible tag extension once the encoder's exact
structural shape is selected. Parser eligibility recognizes a flexible
variable only in the explicit tag-extension position; a bare flexible shape or
payload remains unsupported until earlier constraints resolve it.

Both sides are pinned by tests: accepted—
test/cli/JsonTagUnionProtocol.roc (issue #10418's unannotated
`Ok(Friendly) == Json.parse(...)` comparison closes the inferred parser row);
rejected—test/cli/ParserOpenTagUnion.roc (a parser whose result annotation
has a named rigid extension remains a missing-method error).

### Derived Parser Required-Field Error Composition

A compiler-derived structural record parser, rather than its input-format
implementation, owns the failure produced when a required field is absent.
When a parsed record contains at least one field whose type is not the
recognized optional-field shape `Try(_, [Missing, ..])`, the checker requires
the parser's shared error row to contain `MissingRequiredField(Str)`. It does so
by unifying that row with an open row containing the tag. Records whose fields
are all optional do not add this error, and non-record shapes do not add it.
Nested derived shapes contribute the error whenever any reachable derived
record has a required field.

A custom nominal parser nested inside a derived shape keeps its own minimal
error row. During checking, `constrainDerivedParserErrorRowIncludes` closes an
unconstrained extension on the instantiated custom-parser method and requires
every resulting child error tag to occur with the same payload types in the
parent parser row. A rigid open extension is rejected because the compiler
cannot prove which additional errors it may produce. Monotype lowering calls
the custom parser at that checked child row and explicitly injects each child
error tag into the parent row. This lets a custom JSON scalar parser retain
only `InvalidJson(Str)` when it is nested in a record whose generated parser
also needs `MissingRequiredField(Str)`.

Input formats contribute only errors that arise from reading their syntax and
values. They do not implement a missing-required-field callback. Monotype
specialization repeats the declared shape rule when a parser constraint was
generalized before its concrete dispatcher was known: it constrains the
instantiated callable's open error extension to include
`MissingRequiredField(Str)` before materializing the callable monotype. This is
required even when an enclosing generic function consumes and maps every parse
error, because the generated parser runtime still constructs the missing-field
branch. Lowering then consumes that solved error row and directly constructs
`MissingRequiredField(field_name)` when generated record-finalization observes
an absent required field; absence of that tag from the checked monotype is an
invariant violation, not a condition lowering may recover from.

Both sides are pinned by tests: accepted—
test/cli/ParserRequiredFieldError.roc (a non-JSON derived parser reports the
generic error with the missing field name), and
test/cli/JsonParseErrorComposition.roc (JSON scalar parsing has only
`InvalidJson(Str)`, while a required-record parser composes in
`MissingRequiredField(Str)`),
test/cli/JsonParseGenericWrapperErrors.roc (a generalized wrapper may consume
the parser errors without losing the concrete record's required-field error),
and test/cli/ParserCustomNominalField.roc (a custom nominal parser's narrower
error row injects into its containing record row); rejected—
test/cli/ParserMissingRequiredFieldError.roc (a required-record parser cannot
use a closed format error row that omits `MissingRequiredField(Str)`).

### Builtin Str Interpolation Part Compatibility

Builtin `Str` interpolation accepts a part only when ordinary unification can
make the part `Str` and every static-dispatch constraint already carried by the
part is satisfiable by builtin `Str`. The checker decides both conditions in one
commit-probe: success commits the unification and any method evidence; failure
rolls the whole attempt back and reports the interpolation-part type mismatch.

Builtin quote and interpolation literal constraints are discharged directly by
`Str`. A numeral literal constraint is rejected because builtin `Str` does not
materialize numerals, and every non-literal constraint uses the ordinary static
dispatch method-acceptance rule. Rejecting a constrained part retires its copied
static-dispatch constraints together with the erroneous interpolation so no
unresolved static-dispatch constraint can cross the checked boundary.

Both sides are pinned by tests: accepted—
test/cli/issue_10204_imported_interpolation_metadata/Main.roc (an imported
interpolation instantiated with `Str` checks successfully); rejected—
test/cli/issue_10474_record_field_interpolation.roc (a generalized numeral
record field cannot be instantiated as `Str` by interpolation and reports a
type mismatch without `CheckedModule` construction panicking).

### Field Kinds (All-Dynamic Optional Fields)

This section supersedes the earlier "Existential Presence (Sealed Optional
Annotations)" design: `?:` is a static field KIND with one uniform tagged
representation, not solved presence DATA with witness-directed layout. The
quantifier machinery that design needed (universal/existential presence
rigids, the post-body seal rewrite, witness identity across modules) is
removed with it—nothing about a kind is hidden, so there is nothing to
seal, write out, or keep coherent across copies.

A record field's presence axis carries a concrete `required`, `optional`,
or `defaulted` kind, or an undetermined flex presence variable, solved by
ordinary unification:

- `required` (the `present` state): the field is always there; its slot is a
  plain inline slot. Written `field: T` in annotations. Direct `.field`
  access demands this kind.
- `optional` (the `optional` state): the field may or may not be there AT
  RUNTIME; its slot is tagged (an is-present bit plus payload). Written
  `field ?: T` in annotations. `.?field` yields
  `Try(field_type, [MissingField])`, compiled as a runtime test on the tag.
- undetermined (a flex presence var): minted by record LITERALS for every
  field (a literal can serve as either kind; construction wraps the tag
  exactly when the solved kind is optional) and by `.?field` accesses.
  Unification pins it to whichever concrete kind it meets.

The kind rules:

- Annotations pin kinds CONCRETELY, in every syntactic position—argument,
  return, the annotated value's own type, and type-declaration bodies all
  mean the same thing. There is no polarity split: `?:` is `optional`
  everywhere, and `:` is `required` everywhere. This typing rule does not
  make every kind legal at every external interface: the Host Symbol ABI
  categorically rejects every reachable `?:` field, just as it rejects open
  `..` rows (see Host Symbol ABI).
- `required ~ optional` is a TYPE MISMATCH. One value has one layout; a
  record annotated with a required field cannot flow where an optional
  field is expected (or vice versa)—reconstruct the record instead. Only
  an undetermined kind can become either.
- Width absorption is OPT-IN and POLARITY-RESTRICTED. The checker marks each
  unification relation as construction/inference or exact-width. Construction
  relations may survive meeting a closed empty tail only when every surviving
  field already has resolved kind `optional` or `defaulted`; omitted optional
  fields become missing tagged slots and omitted defaulted fields materialize
  their defaults. Calls with committed arguments, nominal construction from an
  existing value, and platform requirements use exact-width relations, so a
  wider value cannot widen a closed parameter, nominal backing, or host ABI
  type during instantiation. An undetermined or `required` field does NOT
  absorb; accepting either would silently merge typo'd extra fields or
  arbitrary record literals. Consequence: a definition may supply an
  optional/defaulted field on one control-flow branch and omit it on another
  exactly when an annotation pins the kind before the branch rows merge; the
  unannotated conditional stays rejected, and `.?` on a field a closed record
  does not declare is a missing-field error. The old `absent` presence state is
  REMOVED: a field a row genuinely lacks is simply not in the row, and every
  field on a row carries a value type.
- `.?field` on a field whose kind resolved `required` or `defaulted` is
  rejected as unintended (the field is always present; use `.`). Accesses
  are recorded and judged at EVERY GENERALIZATION BOUNDARY—receivers are
  settled by then, and a still-undetermined kind pins to `optional` BEFORE
  the scheme forms, so instantiated copies of a generalized function's
  rows carry the concrete kind and the judgment survives instantiation—
  with a finalize pass as the monomorphic backstop.
- Record UPDATE (`{ ..r, field: v }`) follows the CONSTRUCTION rule per supplied
  field: each mentioned field probes the base with a kind-FLEXIBLE field, so
  an optional base pins optional and checks the value against its payload
  type, while required/defaulted pin as before. A probe still flex at its
  owning generalization boundary commits to `required` before the scheme
  forms; a generic update therefore has one stable field layout instead of
  adopting a different kind per caller. This realizes the SET side of the
  typing frame in "Deferred: Unsetting an Optional Field" below.
- Record DESTRUCTURE (IMPLEMENTED) is kind-flexible the same way: each
  destructured field probes the record with a fresh presence var and a
  FRESH payload var, and the binder stays unbound until the deferred
  kind-directed judgment (`Check.judgeRecordDestructBinds`, the
  `pending_record_destructs` queue) binds it—plainly to the payload for
  `required`/`defaulted`, or to the nominal `Try(payload, [MissingField])`
  (constructed exactly as a `.?` access's chain result) for `optional`, so
  destructuring an optional field surfaces its runtime presence. A kind
  still flex at the judgment pins `required` first—a destructure alone
  must not silently make a field optional—and the probe's mint is
  deliberately NOT in `literal_field_kinds` (the judgment owns the kind
  decision; double-committing with the finalize sweep would be
  order-dependent). The judgment runs at every generalization boundary—
  BEFORE the boundary's literal defaulting (a numeral flowing through a
  destructured field is signature-reachable only once the binder is
  unified through the row) and again after `judgeOptionalFieldAccesses`—
  at finalize as the backstop, and at every exhaustiveness-analysis site
  (match expressions and refutable destructure statements—the analysis
  and its union-closing see sub-pattern tag data through the row only once
  the binder is bound). A judgment pass only commits a still-flex kind
  whose presence var the current boundary OWNS (rank at or above the
  boundary's); lower-ranked entries stay pending for the scope that owns
  them, so nested boundaries fired mid-statement never pin an enclosing
  destructure prematurely.
  Nested sub-patterns (`{ x: Ok(y) }`) check against the binder, so on an
  optional field they see the Try. Exhaustiveness analysis consumes that
  exact checker-judged sub-pattern type for every record column, rather
  than looking the field up again on the scrutinee row. Therefore an
  optional column is analyzed in `Try(payload, [MissingField])` space and
  a nested pattern that omits either side is diagnosed statically.

The tagged representation (IMPLEMENTED—nothing about optional fields is
deferred at lowering anymore; the CheckedModule output and lowering are both
complete):

- Slot encoding: PER-FIELD TAG. Every optional field's Monotype slot is the
  closed STRUCTURAL tag union `[#Missing, #Present(τ)]` (the labels are
  compiler-reserved—`#` starts a comment in source, so no user tag can
  spell them; tag variants normalize to sorted label order, so `#Missing`
  is variant 0 with no payload and `#Present` is variant 1 carrying the value—the discriminant
  contract every compiler runtime consumer shares). One uniform
  representation per field regardless of how many optional siblings the
  record has; a per-record presence BITMASK remains a possible later
  layout optimization that would not change this check-level contract.
  The slot is deliberately NOT the nominal `Try(τ, [MissingField])`
  monotype: record-type lowering runs on rows in modules that never
  reference `Try`, so minting the builtin nominal there would need a
  name-based lookup of the Try declaration—reconstruction of data no
  stage output. The structural union is a pure function of the explicit
  row (kind + value type); its byte layout equals Try's backing anyway
  (two variants, one zero-sized payload). Lowering needs NO solver
  witness—the kind is in the row (`CheckedRecordField.kind`, written into
  the CheckedModule and keyed byte-identically by the solver-side and
  checked-side type-digest writers, so `roc check` and full compilation
  agree). A generalized record scheme retains an undetermined kind as its
  checked presence-variable identity. Instantiation gives that identity a
  dedicated field-kind cell plus distinct source-value and runtime-slot
  cells; row unification resolves the kind and relates value-to-value and
  slot-to-slot. Construction consumes the resolved graph kind, never the
  generalized scheme or a reconstructed slot shape; when the construction
  itself is the first required-kind evidence, it explicitly relates that
  field's runtime-slot cell to its source-value cell at the same time.
  Every field-kind cell also records that exact source-value/runtime-slot
  pair when it is instantiated. After all interface and body relations for
  one Monotype specialization have been produced, relation freeze commits
  each still-undetermined field-kind class to `required` and unifies its
  recorded runtime-slot cell with its recorded source-value cell. This is the
  specialization-time defaulting rule for a generalized scheme with no
  optional/defaulted evidence in that specialization; it is not recovery from
  a slot shape, and no unresolved kind or placeholder slot can cross the
  freeze boundary. Draft specialization lookup may consume the same declared
  default before freeze only through an immutable specialization-key view, and
  only when every other cell in the request is already resolved. That view
  reads an undetermined field's explicit source-value cell as the required
  slot, without mutating the live kind or slot cell; its digest is only a bucket
  selector, exact structural equality is the authority, and a match then joins
  the two live request interfaces through ordinary graph unification. A
  selected hoisted-const use whose dispatch requires a concrete Monotype may
  consume the same view as its explicit specialization choice; it immediately
  relates that chosen type back to the live const-use graph, so the required
  field-kind commitment is producer-visible before body emission.
- Where the type lowers: the record arms of `Builder.lowerType` use
  `optionalSlotType` in src/postcheck/monotype/lower.zig, with the instantiation-graph twin
  `optionalSlotNode` (`instFields`, `fieldAccessTypeNode`) building the
  same node shape so graph-solved and directly-lowered occurrences of one
  checked row seal to one Monotype. A Monotype record field retains the
  optional source-value type as non-layout specialization metadata; this
  lets a finished Monotype participate in later graph relations without
  recovering the payload from the tagged slot. An immutable provisional
  interface-replay view additionally marks an undetermined kind explicitly;
  because no runtime slot exists yet, its structural `ty` mirrors the source
  value type and the marker forbids that view from reaching layout or completed
  Monotype output. Downstream (LIR layout, ARC, match
  compilation, interpreter, backends) the slot is an ORDINARY structural
  tag union—no new concepts anywhere below Monotype lowering.
- Boxy planning consumes the checked row's explicit field kind directly.
  Required and defaulted children use their inline representation; optional
  children use a memoized `[#Missing, #Present(payload)]` representation and
  carry an explicit descriptor requirement because the payload may be erased
  through the containing record. Boxy record construction uses that same child
  kind to wrap supplied optional fields, emit missing optional slots, restore
  omitted defaults, and copy unmentioned update slots. Its `.?` lowering
  consumes the checked access segment modes and constructs the promised flat
  `Try` result. It does not infer field kinds from layouts or reserved labels.
- Construction (`lowerRecordExpr`): graph-owned construction consumes the
  resolved field-kind cell. Construction from an already-sealed Monotype
  consumes the target `Type.Field`'s explicit resolved metadata
  (`value_ty` means optional, `default` means defaulted, otherwise required),
  never the generalized checked row and never the runtime slot's shape. A
  SUPPLIED optional field lowers its
  checked value at the Present payload type and wraps it in the `#Present`
  tag; an OMITTED optional field (admitted by width absorption—including
  every field of `{}` against an all-optional row) constructs the
  `#Missing` tag, exactly where an omitted DEFAULTED field materializes its
  default. Record update copies unmentioned optional slots verbatim
  (presence state included); a MENTIONED optional field takes the same
  supplied-field arm as construction—the value lowers at the Present
  payload type and wraps in the `#Present` tag. DESTRUCTURING an optional
  field lowers as exactly a one-segment `.?` chain result per field: the
  bound value is the slot read materialized as Try—`#Present(v)` yields
  `Ok(v)`, `#Missing` yields `Err(MissingField)`
  (`optionalDestructTryExprAtNode`, sharing the slot-test shape of
  `lowerOptionalFieldAccessChain`)—constructed at the binder's own
  checked Try node, with the row's `CheckedRecordField.kind` directing
  required-vs-optional (explicit upstream data; destructs themselves
  serialize no kind). Statement and parameter positions route such
  patterns through the materialized-pattern machinery
  (`patternNeedsExplicitBinding` → `lowerRecordRestPatternBindingThen` /
  `appendRecordRestPatternStatements`: field slot read, Try
  materialization, sub-pattern matched against it); a MATCH branch keeps a
  flat pattern by TRANSLATING the Try-space sub-pattern into slot space
  (`lowerOptionalDestructChildAtSlotNode`: `Ok(p)` ↦ `#Present(p)`—the
  payload types are identical—`Err(p)` ↦ `#Missing`, and a plain binder
  becomes a compiler-local slot bind whose Try value is a `let` prelude
  around the branch body and guard, `OptionalDestructBind`), preserving
  fall-through refutability natively.
- `.?` access: the CheckedModule output is complete—
  `CheckedFieldAccessSegment.mode` records required/optional per segment
  (introduced in `serialized_layout_version` 57; the current version is
  defined beside the checked module's `Serialized` layout), and the body copier's
  former required-only invariant is gone. Monotype field-access segment instantiation
  consumes that mode as field-kind evidence: a required segment commits an undetermined
  kind to required and relates its runtime slot to its source value; an
  optional segment commits optional and relates the explicit tagged slot and
  source-value cells. This happens when the segment's type relation is
  instantiated, before a callee specialization key can depend on the accessed
  result. A
  chain containing any optional segment lowers per-CHAIN
  (`lowerOptionalFieldAccessChain`): each `.?` segment is a runtime test
  (a match) on the field's tagged slot—the first `#Missing` slot
  short-circuits to `Err(MissingField)`, a `#Present` payload continues the
  chain, required segments after an optional one ride that Ok path as
  plain field reads—and the final value wraps in `Ok` exactly once. The
  chain yields the flat `Try(τ_final, [MissingField])` the checker
  promised (never nested), constructed at the access expression's own
  checked Try type.
- Glue / Host Symbol ABI: `?:` is not a host ABI type. The checker walks every
  hosted and provided signature through aliases, nominal backings, containers,
  arguments, and returns, and emits a hard error when any reachable record
  field has the `optional` presence state. Glue output therefore never includes
  the internal `#Missing`/`#Present` slot representation in a platform-host
  contract.
- Inspect rendering reads the reserved slot labels: Monotype `Type.Field`
  carries explicit optional source-value metadata for specialization, but
  runtime consumers still receive the kind consumed into the slot encoding
  by record-type lowering. The slot union's labels are the
  COMPILER-RESERVED names `#Missing`/`#Present`: `#` starts a comment in
  Roc source (the same reserved namespace as compiler-minted `#interp_0`
  idents), so no user-written tag can ever spell them and the slot union
  is a distinct type from any user-annotated `[Missing, Present(τ)]`.
  Record `Str.inspect` expansion runs over the memoized Monotype alone
  (no checked row in hand); `Builder.optionalFieldSlot` recognizes an
  optional slot by an EXACT match on the reserved labels—a lossless
  read-back of the encoding, not a shape heuristic—and renders the
  payload or `<missing>`. Derived JSON codecs read the labels the same
  way (`optionalFieldSlot` in the encode/parse record-field ladders): an
  encoded `#Present` slot emits its field with the payload's encoder and
  `#Missing` omits the field; a parsed record materializes `#Missing`
  for an absent `?:` field and wraps a present one in `#Present`—the
  slot-kind sibling of the Try(τ, [Missing]) codec convention, pinned by
  test/cli/JsonOptionalFieldKinds.roc. A user-annotated `[Missing, Present(τ)]` field
  is an ordinary tag union everywhere (inspect renders `Present(5)` as
  `Present(5)`); the reserved-label union shares its LAYOUT (two
  variants, variant 0 zero-sized) but not its identity. Every other
  consumer (`.?` chains, construction, update, destructure, glue) reads
  the explicit checked kind and never inspects labels at all.

Kind defaulting as a checker pass (IMPLEMENTED): a literal-minted kind var
still undetermined at module finalize (a literal field never used at either
kind) is COMMITTED to `required` (zero-cost) in the solved graph, by
ordinary unification (`Check.defaultLiteralFieldKinds`—the same
fresh-content + unify shape `judgeOptionalFieldAccesses` uses for its
`optional` pin). The mint sites record every literal field's kind var
explicitly at creation; the sweep runs LAST at module (and REPL-expression)
finalize, after `judgeOptionalFieldAccesses` and every other
acceptance/rejection judgment, so it is a pure commitment of already-final
states. A GENERALIZED kind var is skipped BY DESIGN: it is a scheme interior
(e.g. `mk = |v| { a: v }`), and instantiations of the scheme may
legitimately join a `?:` annotation later—which is also why the sweep
never runs at per-def generalization boundaries. Consequently the read
boundaries' still-flex arms (TypeWriter rendering, `copyCheckedRecordFields`
CheckedModule output, the `writeFieldPresenceForKey` type-digest writer) now cover scheme
interiors, which every reader treats required-equivalent; monomorphic
literal-minted kinds reach them already committed.

Deferred (explicitly not yet implemented):

- Fallback destructure patterns (`{ field ?? fallback }`-style), which need
  their own typing rules here before implementation. (Plain DESTRUCTURE of
  an optional field is IMPLEMENTED—it binds `Try(payload, [MissingField])`
  via the deferred kind-directed judgment; see the record-destructure
  bullet above. SETTING an optional field in an update is IMPLEMENTED—
  see the record-update bullet above.)
  UNSETTING a field in an update (`{ ..r, x: _ }`) has its typing rule
  sketched in "Deferred: Unsetting an Optional Field" below.

Pinned by tests in src/check/test/type_checking_integration.zig: accepted—
a value annotated `{ world ?: U8 }` may supply or omit `world`, and its
exported type keeps `world ?: U8`; `.?world` on it typechecks as
`Try(U8, [MissingField])`; one definition MAY supply an optional field on
one `if` branch and omit it on the other ("conditional presence accepted");
DESTRUCTURING an optional field binds `Try(U8, [MissingField])` (statement
and parameter positions, nested `Ok(v)` patterns bind the payload), a
required sibling's destructure binds plainly, and a destructure of a
still-flex literal row pins the field `required` (the literal's row renders
`a:`, not `a ?:`). Rejected—a direct `.world` read of an optional field
is a type error (both on a value's own annotation and on a `?:`-signature
argument), `.?` on a required field is rejected at finalize, and
destructuring a field the record lacks stays a mismatch.

### Defaulted Fields (Construction-Optional Required Fields)

`{ a : U8 ?? 10 }` declares a DEFAULTED field: construction may omit it (the
omitted slot is materialized from the default), but at runtime it is an
ordinary required field—inline slot, read with `.a`, updated and
destructured like any required field. Defaulting is a fourth point on the
field-kind axis (see Field Kinds above):

- Access `.a`, layout, update, and destructure all behave exactly as
  `required`; `.?a` on a defaulted field is rejected at finalize by the same
  always-present judgment as on a required field.
- Construction may omit the field, exactly as `optional`—via the same
  OPT-IN width absorption: a literal row lacking the field absorbs it only
  when the field's kind RESOLVED `defaulted` (an annotation declared it),
  and lowering materializes the DEFAULT VALUE into the inline slot (where an
  optional field would materialize the missing tag). Undetermined kinds
  still never absorb.

Kind unification treats `optional` as layout-incompatible with the two inline
kinds and treats a default identity as construction-only information:

- In an ordinary value relation, `required ~ defaulted → required`. A shared
  field is already supplied, so there is no omission site at which that default
  could be selected. Keeping the identity would taint an ordinary
  `{ a: value }` with whichever defaulted type it encountered first, making a
  later use against an otherwise layout-identical defaulted type fail. A direct
  required `.field` access uses an explicit access-demand relation instead: it
  accepts either inline kind while preserving an existing defaulted
  declaration. The default identity otherwise survives exactly on the
  unmatched field admitted by width absorption, where construction actually
  omitted the field and lowering must materialize the default. The checker
  records that omission decision as `(record expression, field name,
  default identity)` in the checked body; postcheck consumes this explicit
  construction plan instead of trying to recover it from a row that later
  value unification may normalize to `required`.
  A `{ a : U8 ?? 10 }` value still flows freely where `{ a : U8 }` is
  expected—the merged use is required and has the same inline layout.
- `defaulted(d1) ~ defaulted(d2)` unifies exactly when `d1 = d2`. Two
  annotations defaulting one field differently have no coherent merged
  default; the conflict is a type mismatch.
- `optional ~ defaulted` is a type mismatch (tagged slot vs inline slot).

The default VALUE never lives in the type graph. The row's kind carries only
a stable DEFAULT IDENTITY—the declaring module's deep content identity
plus the default expression's stable source-node index, the same key shape as
nominal declarations, rebased by copy_import on every store crossing.
Materialization only RECORDS the default; the expression is checked once at
FINALIZE—after every def is checked and callee effects are resolved—
against an instantiated copy of the field's type. (Materialization can run
before check order even exists: type-declaration generation and scheme
predeclaration both materialize annotations early, so checking there would
pin unchecked defs monomorphically and misjudge purity.) Unification
compares identities only; construction-site lowering is the only consumer
of the value.

Restrictions:

- `?:` and `??` do not combine: a default makes the field never missing,
  which makes the tagged slot and `.?` pointless—`a ?: U8 ?? 10` is
  rejected at canonicalization with exactly that explanation.
- The default must be a LITERAL, defined recursively: a numeric literal
  (including a negated numeral), an interpolation-free string literal (an
  interpolated string references bindings), a tag literal—bare or
  applied, plain or nominal-qualified (the nominal wrapper names a type
  declaration, not a value)—or a list / record / tuple literal whose
  components are all literals. Nothing else: no operators, no calls, no
  lambdas, no control flow, and no name reference of any kind (local,
  module-level, or imported). Judged at canonicalization
  (`Can.defaultNonLiteralNode`, diagnostic `record_default_not_literal`)
  by walking the canonicalized default; on rejection the default is
  dropped. The rule exists because defaults are compiler-materialized at
  construction sites: a reference could form an evaluation cycle the
  compiler will not chase. Banning references bans every VALUE-REFERENCE
  cycle—the direct self-reference judgment, the local-capture (free-variables)
  judgment, and the def-dependency demand edges from annotation defaults were
  all subsumed and deleted, including the
  alias-mediated gap none of them covered (a type declaration's default
  referencing a def, cycling through a value annotated with the alias—
  the demand walk never followed alias lookups). Supporting references
  later is future work that needs declaration-aware cycle edges: demand
  edges that follow a value annotation's alias/apply lookups into the
  referenced declarations' defaults.
- Literal aggregates can still introduce a cycle IMPLICITLY by omitting a
  defaulted field—for example, `Node := { next : Node ?? Node.{} }`. After
  default expressions are checked and field kinds are solved, the checker
  walks each literal and its solved record rows. Every omitted defaulted field
  contributes an explicit `DefaultId` dependency; following those local
  identities back to the starting default is rejected as `recursive_default_value`.
  This judgment runs before building `CheckedModule`, so postcheck lowering
  only receives defaults whose materialization graph is acyclic.
- The declared FIELD type of a default must be CONCRETE: the one archived
  default is materialized at every construction site, so a parametric field
  has no single runtime representation even when the default literal itself
  happens to settle concretely in an instantiated check copy. Judged at
  finalize, after the defaulting rounds so numeral defaults commit first—a
  literal (`?? []`) can still be non-concrete, which is why this axis survives
  the literal restriction. (Purity needs no axis of its own anymore: a literal
  is never effectful, so the finalize-time `effectful_default_value` judgment
  remains only as a backstop invariant.)

The CheckedModule preserves the kind: a defaulted field serializes as a
required field CARRYING its default identity (`CheckedFieldDefault`), with
the origin half translated to the CheckedModule's identity form (the
declaring module's content hash) and rebased at every store crossing,
exactly like nominal identities. Type digests (`TypeDigest`) include the
identity in the same form on both the solver and checked sides, so the two
representations key identically and two rows defaulting a field
differently digest differently.

Construction-site materialization: the declaring module's CheckedModule
archives each locally-declared default as a checked expression
(`CheckedBodyStore.default_exprs`, keyed by `DefaultId.expr_node`—the
source-node walk already records every checked expression, so the table
is a serialized index over it, surviving the discard of the build-time
source-node map). Monotype lowering of a record construction that omits a
defaulted field resolves the field's default on the checked row and
materializes it at the field's monotype. Inlining the archived expression
IS the defining behavior—defaults are literals (see Restrictions
above), so inlining is their evaluation—and the finalized `field_default`
compile-time root is a cache of the same value: materialization prefers
restoring the declaring module's finalized constant and inlines only
while that module's own roots are still mid-finalization (a cache-hit
split, not a fallback—both paths produce the identical literal).
When the default literal uses a custom `from_numeral` or `from_quote`, that
same `field_default` root owns the literal-conversion mode and has the
conversion call's `Try` result type. Its wrapper evaluates the raw conversion
once; finalization reports `Err` with the literal-specific diagnostic or
archives the `Ok` payload as the field-default constant. A second
`numeral_conversion` or `quote_conversion` root for the same checked expression
is forbidden.
CROSS-MODULE materialization is COMPLETE through the same route: the
default identity's declaring-module content hash resolves the declaring
view (`moduleForIdentityHash`), and an imported checked module is always
finalized, so a foreign default always restores from its archived
constant. Checking keeps the `does_fx` → `effectful_default_value`
rejection as a backstop invariant only: canonicalization's literal
restriction already makes an effectful default unreachable from source.

Monotype default identity (`Type.FieldDefault`): the Monotype record
field itself carries the `??` default identity—the declaring module's
identity interned in the program name store plus the default
expression's node index—and every Monotype digest and structural
equality includes it. This is load-bearing, not descriptive: derived
codecs and specializations are keyed by monotype, and a derived JSON
parser for `{ count : U8 ?? 10 }` must FILL an absent key while the
parser for the same-shaped `{ count : U8 }` must ERROR
MissingRequiredField, so rows that disagree about defaults (or about
having one) must be DISTINCT monotypes—"same monotype ⇒ same
behavior" stays an invariant instead of an approximation. The slot
encoding is unchanged (defaulted fields remain plain inline slots;
layout never reads the identity), and the identity rides unchanged
through every downstream carrier of record rows: the instantiation
graph (`InstField`, asserted equal when rows unify), graph sealing,
lambda-solved and lambda-mono types, and const-store type evidence
(`ConstStore.TypeFieldDefault`, translated across name stores like
labels). Both checked→Monotype interning routes (`Builder.lowerType`'s
record arms and the instantiation graph's `instFields`) stamp it from
the checked kind at the same point the kind is consumed into the slot
encoding.

Derived JSON parse of `??` fields (IMPLEMENTED): an absent key fills the
field's archived default into the inline slot—the codec sibling of
construction-site omission—while a present key parses at the inline
type; an explicit `null` is an ERROR for a defaulted field (null is a
value, absence is not—same rule as `?:`), and encode always emits the
field. The parse ladder reads the default straight off the Monotype
field (`parserFieldDefaultFor`), and the checker's derived-parse gate
(`recordParseNeedsRequiredFieldError`) skips `?:`/`??`-kind fields so an
all-self-filling record validates with a closed error row; the lowering
mirrors of that analysis (`parserShapeNeedsRequiredFieldError` and its
graph twin) recognize the same two self-fill cases. Pinned by
test/cli/JsonOptionalFieldKinds.roc. Evaluation ordering: each archived
default is a `field_default` compile-time root, registered BEFORE every
other root kind, and finalization encodes the conservative dependency
edge explicitly—a non-default root is not ready while any requested
`field_default` root is unfinished
(`RootCompletionState.pending_field_defaults`)—so the defaults always
evaluate as their own leading batch before any parser that might
restore them lowers.

Optional-field lowering is COMPLETE (see Field Kinds above): an omitted
optional field constructs the `#Missing` tag in the same
`lowerRecordExpr` slot-fill where an omitted defaulted field
materializes its default.

### Deferred: Unsetting an Optional Field (`{ ..r, x: _ }`)

Not yet implemented—this is the design sketch for when it is. `{ ..r,
x: _ }` UNSETS a field in a record update: the result carries `x` as an
optional slot in the Missing state. Unsetting does NOT remove the field
from the row—the `absent` presence state is gone (Field Kinds above), rows
never shrink, and the slot union `[#Missing, #Present(τ)]` already has a
representation for "not there". That one observation dissolves the
asymmetry the old record-update TODO feared: input and output presence no
longer differ, because presence is a static KIND and unsetting only changes
the runtime STATE of a slot whose kind stays `optional`.

Syntax. Today `x: _` is a parse error: field names in expression records
must be `LowerIdent` (`record_fields_next` in src/parse/Parser.zig), and
after `name:` the value parses through the general expression kernel, where
a bare `Underscore` has no prefix rule and lands in the
`expr_unexpected_token` malformed fallthrough. (`_name` is a
`NamedUnderscore` IDENT expression, so `x: _name` is an ordinary set and
stays one; only bare `_` means unset.) The parser newly accepts a bare `_`
as the ENTIRE field value—`Underscore` directly followed by `,` or `}`—
in expression-record field position only, recorded as an explicit marker on
`AST.RecordField` (a third state beside a value and punning's null; never a
sentinel expression). `_` anywhere else in expressions stays rejected. Note
the grammar split this must not blur: TYPE records already use `_`/`_name`
as field NAMES (unnamed padding fields); expression records do not, and
unset's `_` sits in VALUE position, after the colon.

Typing. The update's result type EQUALS the base type—the field stays in
the row, optional, same value type—so the record-update arm in
src/check/Check.zig keeps its wholesale `unify(base, expr_var)` unchanged.
Only the PER-FIELD demand becomes kind-directed:

- A SET field keeps today's probe (IMPLEMENTED, see the record-update
  bullet in Field Kinds): a single-field record with a kind-FLEXIBLE
  presence—`.unknown` over a fresh presence var, recorded for the
  finalize kind-defaulting sweep—unified into the base, so the base's
  kind decides (`required ~ defaulted → required` accepts a supplied update
  without attaching a construction default, and an optional base field
  checks the value at the payload type).
- An UNSET field mirrors the `.?`-access probe EXACTLY (the optional arm of
  `e_field_access` checking): unify the base with an OPEN single-field
  record `{ x: unknown(π, τ) }`—fresh presence var π, fresh value var
  τ—and record (π, field, region) in the `optional_field_accesses`
  watermark queue with a per-entry USE marker (access vs unset), so the
  one `judgeOptionalFieldAccesses` walk at every generalization boundary
  (finalize as backstop) judges both: a kind resolved `required` or
  `defaulted` is rejected (problems below), a still-flex kind pins to
  `optional` BEFORE the scheme forms—an unset is presence-evidence for
  optionality, exactly like `.?`. The probe must NOT demand concrete
  `optional` in the row, for the same reason `.?` doesn't: a concrete
  demand would let width absorption admit the field into a closed base
  that lacks it, silently accepting typo'd unsets. With the flex-kind
  probe, a field the base row genuinely lacks is an ordinary
  missing-field mismatch.
- Mixed set-and-unset composes with nothing extra: each mentioned field
  runs its own probe against the base, then one wholesale base ~ result
  unify. Chained/nested updates compose because every update's type equals
  its base's type.

Lowering. Trivial by construction: `lowerRecordExpr`
(src/postcheck/monotype/lower.zig) lowers update as CONSTRUCTION-BY-COPY—
the base binds to a let-local, mentioned fields take their new values,
unmentioned fields copy via field access. An unset field takes the existing
`optionalSlotMissingExpr(field.ty)` arm—the same Missing-tag construction
an omitted optional field uses. ARC needs no new rules: the replaced
Present payload is never read, exactly like a SET field's replaced value
today, and is freed when ARC decrefs the base after its last use; backends
keep dumbly following the emitted incref/decref.

Interactions:

- Unset of a DEFAULTED field is rejected by the judgment above: the field
  is an inline slot with no missing state, and "unsetting" it would have to
  rematerialize the default—construction behavior, not update. The
  report carries a did-you-mean note: construct a new record omitting the
  field instead.
- Unset of a field the base row lacks: ordinary missing-field mismatch from
  the probe (flex kinds never absorb).
- `{ x: _ }` WITHOUT `..base` is rejected at canonicalization—unset is
  meaningless in construction, where omission already builds Missing.
- Patterns: `{ x: _ }` in a destructure already means "match `x`, ignore
  the value" (wildcard sub-pattern over the kind-flexible destructure
  probe) and KEEPS that meaning—expression `_` (unset) and pattern `_`
  (wildcard) deliberately diverge. FALLBACK destructure (`{ x ?? fallback }`)
  stays its own deferred item (Field Kinds above); plain destructure of an
  optional field binds the Try (Field Kinds above).
- Glue / Host Symbol ABI: no additional impact. `?:` remains forbidden at the
  boundary; within Roc code, unset only selects variant 0 of the unchanged
  internal slot representation at runtime.

Diagnostics (Title Case, cf. "Optional Access Of Required Field"): "Unset
Of Required Field" and "Unset Of Defaulted Field" at check (the latter with
the construction hint), "Unset Outside Record Update" at canonicalization.
The probe unifies under its own `record_unset` context so a missing-field
mismatch renders against the update site.

Phasing (each stage lands with its pins before the next): (1) PARSE—the
`_` field value with its explicit AST marker; snapshots pin the accepted
form and `_` still rejected elsewhere. (2) CAN—an explicit unset
representation on `e_record`: a SEPARATE span of unset field names (name +
region, no value expression; a new span, not a sentinel `Expr.Idx`, per
AGENTS.md explicitness), plus the outside-update rejection. (3) CHECK—the
probe, the queue's use marker, and the judgment split; pinned in
src/check/test/type_checking_integration.zig: accepted—unset of a
`?:`-annotated field (result keeps `x ?: τ`), unset pinning an undetermined
kind to optional, mixed set-and-unset; rejected—unset of required, of
defaulted, of a missing field, and unset judged through a generalized
function instantiated at a required row. (4) LOWER—route unset fields to
`optionalSlotMissingExpr`; an eval test (run-test-eval, all backends)
proving `.?x` on the updated record yields `Err(MissingField)` while other
fields survive. Beyond this sketch, FALLBACK destructure (`{ x ?? d }`)
remains deferred (orthogonal, listed in Field Kinds; plain optional
destructure is IMPLEMENTED); SETTING an optional field in an update
(Present-wrapping `{ ..r, x: v }` on an optional `x`) is now IMPLEMENTED—
the SET side of this section's typing frame, realized by the kind-flexible
update probe (see the record-update bullet in Field Kinds).

### Rewrite Inventory

Every solver-mutating rewrite in checking, classified. A change that adds a
site to any family below must classify it here.

`dangerousSetVarRedirect` call sites (all in src/check/Check.zig; the
`RedirectRule` member at each site is the citation):

- `widenTryConditionForExpectedReturn`—policy: Hosted Try Question
  Widening (above).
- `markErroneousBranchWithExpected`—mechanism: diagnostic recovery. The
  expression already has a reported error; its var is redirected to a fresh
  var unified with the expected return so checking can continue past it.

Other solved-graph mutations:

- `unifyWithFresh` (`dangerousSetVarDesc`)—mechanism: fast path writing
  exactly the descriptor that unifying a root flex placeholder with fresh
  content would produce.
- `markErroneous` (`setVarContent(.err)`)—mechanism: diagnostic recovery after
  an already-reported error. It marks the checker node's solved class directly,
  preserving the class-wide cascade suppression previously provided by
  unifying that node with a fresh error variable.
- `checkMatchExpr`'s branch-pattern target—mechanism: diagnostic recovery after
  an already-reported error. An erroneous scrutinee cannot relate the branch
  patterns to each other, so they unify against a shared fresh variable instead
  of the scrutinee's. An error-free program never reaches the probe, so no
  program's typechecking or checked-module output changes.
- `resetAnnotationNodes` (`resetVarToUnbound`)—mechanism: recycles
  annotation node vars after the scheme was copied off as a disjoint orphan.
- `finalizeTypeDeclarationValidity` and occurs-check poisoning
  (`setVarContent(.err)`)—policy: Type Declaration Template Validity (above)
  and diagnostic recovery after an already reported problem.
- `finalizeFunctionEffectsAtBoundary`—policy: directed-effect
  materialization at generalization boundaries, the rule declared in
  Checking Effects And Const Roots.
- `closeAbsentConstructedPayloadVars` / `closePayloadVarToEmpty`—policy:
  absent-constructor payload closing. A constructed value's unconstrained,
  ignorable payload vars for tags the expression provably never constructs
  close to the empty tag union, so matches on constructed values are
  exhaustive without wildcard arms.
- `validateDerivedParseTagExt`—policy: Derived Parser Tag-Row Closure
  (above). Once structural parser eligibility has selected a known tag union,
  its unconstrained flexible extension closes to the empty tag union through
  ordinary unification; rigid extensions remain rejected.
- `constrainDerivedParserRequiredFieldError`—policy: Derived Parser
  Required-Field Error Composition (above). A structural probe of derived
  record fields gates ordinary unification of the parser's shared error row
  with `[MissingRequiredField(Str), ..]`.
- `constrainDerivedParserErrorRowIncludes`—policy: Derived Parser
  Required-Field Error Composition (above). A custom parser method's
  instantiated error extension is closed, then its concrete tags gate ordinary
  unification constraints requiring the parent parser row to include them.
- `constrainInterpolationPartToStr`—policy: Builtin Str Interpolation Part
  Compatibility (above). One commit-probe unifies the part with `Str` and
  validates every attached dispatch constraint; only full success is committed.
- Literal defaulting (`commitLiteralDefault`, `commitLiteralGroupDefault`)
—policy: literal defaulting as declared in Static Dispatch At The
  Checked Boundary (the `LITERAL DEFAULTED` warning) and the numeric
  default candidate order (`Dec` first); mutation happens only through
  committed probes of ordinary unification.
- `instantiate.zig` / `copy_import.zig` `dangerousSetVarDesc`—mechanism:
  instantiation and import copying build fresh disjoint graphs.

Stamped-plan restamps on CIR nodes (discharge time): the restamp rule—
only the node's own constraint may restamp a node that already carries a
stamped plan—is documented and enforced at the restamp sites
(`rewriteEqBinopAsMethodEq`, `rewriteDerivedIsEqMethodCallAsStructuralEq`,
`rewriteDerivedMethodCallAsStructuralHash`). Plan stamping at creation
(`replaceExprWithDispatchCall` and friends at plan-creation sites) is the
plan's first stamp, not a restamp; `replaceExprWithRuntimeError` is diagnostic
recovery.

Read-only acceptance probes (no mutation; their rules live in doc comments
at their definitions): `staticDispatchConstraintAcceptsCandidate` states the
method-acceptance rule of static dispatch, with accepted/missing-method/
signature-mismatch branches each pinned by tests;
`numeralCandidateStructurallyRefuted` implements no rule of its own and is
witness-asserted against the probe it pre-filters in safety builds;
`probeCanUseAs`/`tryErrorRowNeedsUseSiteWidening` are the gating probes for
Hosted Try Question Widening.

## Runtime Lowering Strategy

Runtime compilation has one selected explicit lowering strategy:

```zig
const SpecializationStrategy = enum {
    /// Lambda-set specialization: specialize polymorphism and callable flow
    /// through Monotype, Lambda Solved, and Lambda Mono before LIR.
    lss,

    /// Box polymorphic values and closures, pass dictionaries/vtables, and
    /// lower checked data directly to LIR without lambda sets.
    boxy,
};
```

The command-line spelling is `--specialize=yes` for `.lss` and
`--specialize=no` for `.boxy`. All runtime-producing commands accept the flag
except `roc check`, because `roc check` stops at checked module output and the
checked module is independent of post-check lowering strategy. If the flag is
omitted, every optimization level uses `.lss`. `.boxy` is experimental and is
selected only by the explicit `--specialize=no` opt-in. The `--opt` flag still
selects the code-generation backend and optimization family. The
`--specialize` flag selects how checked data becomes LIR before ARC and code
generation.

Compile-time evaluation is not a runtime lowering-strategy selection. CTFE
always uses `.lss`, including when the enclosing runtime compilation explicitly
selects `.boxy`. Runtime strategy flags, cache entries, and host wrappers must
not route a compile-time root through Boxy or reuse Boxy-lowered LIR as a CTFE
result.

Compiler progress text follows the selected strategy. `.lss` reports the
lambda-set-specializing work as `Specializing`. `.boxy` reports the same
pipeline position as `Lowering`, because it does not specialize lambda sets.

The strategy is threaded explicitly through the public checked-to-LIR API and
stored in every lowered-output cache key. It is not recovered from the backend,
optimization level, target, root set, or whether a later stage asks for an
interpreter image. Backends receive ARC-complete LIR and do not know whether the
LIR came from `.lss` or `.boxy` except through ordinary LIR statements,
layouts, descriptor tables, and root metadata.

### `.lss` Runtime Lowering

`.lss` is the lambda-set-specializing strategy. It runs:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono decisions
  -> direct Lambda Mono to LIR lowering
```

It specializes polymorphic procedure templates to closed Monotype types, solves
callable flow with lambda sets, turns finite function values into generated
tag unions, uses packed erased callables only at explicit erased-callable
boundaries, and emits concrete LIR layouts for every local, parameter, return,
and static data request.

`.lss` owns monomorphic static-dispatch elimination. A checked static-dispatch
plan becomes a direct call or explicit structural operation while the current
procedure specialization has concrete type information. Later stages never see
checked dispatch nodes.

### `.boxy` Runtime Lowering

`.boxy` is the boxing strategy. It consumes immutable checked modules directly:

```text
checked modules
  -> checked CIR + checked types + checked roots + checked dispatch plans
  -> boxy representation planning
  -> LIR
```

It does not create Monotype IR, Monotype Lifted IR, Lambda Solved IR, Lambda
Mono decisions, lambda sets, finite callable tag unions, or lambda-set
specializations. It also does not consult those data structures through a
compatibility shim. The selected lowerer owns a separate checked-to-LIR path
whose inputs are CheckedModule data and whose output is ordinary LIR.

Boxy representation has one purpose: reach LIR without whole-program
monomorphic specialization. Runtime performance may be lower because generic
values are boxed, functions are indirect erased callables, and polymorphic
operations use dictionaries/vtables. Compile time is lower because no lambda
sets, callable-flow specialization queue, or Monotype/Lambda Mono syntax
pipeline is constructed for runtime roots.

The boxy representation plan records every checked type it analyzes in an
explicit module-qualified checked-type-to-representation table. Later boxy
stages use that table to select worker layouts for expression results, pattern
bindings, local storage, adapters, descriptors, dictionaries, and layout-only
requests. They do not recover the representation for a checked expression by
scanning body shapes, matching source names, recomputing type structure, or
relying on incidental representation list order.

For runtime roots whose worker source resolves to a checked body in any checked
module view available to post-check lowering, representation planning walks the
reachable checked expressions, statements, and patterns in that body and records
their checked types with that body's checked module id before layout planning
runs. Callable-eval procedure bindings are resolved through the checked
compile-time root table first. A finalized root is read from its owning
`ConstStore`: boxy reads its explicit `FnDef`, follows direct template references
directly, and follows nested function identities through the checked
`NestedProcSiteTable` to the lambda or closure expression that is the runtime
callable body. It does not lower the compile-time entry-wrapper evaluator block
as a runtime worker.

If checking intentionally leaves a callable-eval root pending because the
selected compile-time roots did not need it, but a runtime body references that
binding, planning records a `RuntimeCallableEvalUsePlan` containing the exact
module-qualified checked producer expression. Lowering evaluates that producer
in its owning module with an isolated binder environment; caller-module binder
ids and lambda arguments are unavailable there. This is explicit checked-stage
data, not recovery from the lookup, name, or callable shape.

When a pending callable-eval binding is itself selected as a private worker,
planning follows that same checked producer expression. A producer that is an
explicit lambda, closure, or resolved procedure lookup supplies the worker
source directly; a value use whose producer is a general expression remains a
`RuntimeCallableEvalUsePlan`. Boxy does not request a second compile-time
evaluation, switch CTFE away from LSS, or treat the callable's checked type as
evidence for a missing body.

Imported direct calls, pending callable producers, and restored const functions
therefore keep imported type and expression ids attached to the imported
CheckedModule that owns them; they are not mapped into root-module ids and they
are not recovered by name. A type that
appears only in a local aggregate, temporary receiver, nested expression, or
destructuring pattern is therefore still present in the explicit representation
table consumed by lowering.

A finalized stored function may carry `ConstStore` captures. The worker plan
records the exact module-qualified `ConstFnId` that owns those captures, and
representation planning analyzes the checked type of every captured
binder in the nested function's checked module. Worker lowering reserves one
frame local for each such binder, binds the original checked `PatternBinderId`,
and restores the capture's explicit `ConstNodeId` from the owning `ConstStore`
before entering the checked function body. Capture values are not recovered
from the use site, reconstructed from closure shape, or turned into synthetic
worker arguments. Their source module, value node, binder identity, and checked
type are all explicit checked-stage data.

Entry wrapper procedure templates name an `EntryWrapperTable` entry. Boxy
planning and lowering use the wrapper's checked `body_expr` as the worker body
for that procedure template. The wrapper table is CheckedModule data; the
boxy lowerer does not reconstruct wrapper bodies from root names, source
strings, command kind, or host ABI metadata. Callable-eval roots are the
separate case above: their runtime callable body is recovered from the finalized
`ConstStore` function value, not from the compile-time evaluator entry wrapper.

Checked value lookups are lowered from the checked resolved-value table. Local
parameters, local values, mutable versions, and pattern binders map to the
already-created LIR local for the checked binder. Selected hoisted constants
reuse that local when the selected binder is currently live; otherwise they
restore the selected const use. Top-level constants, imported constants, and
platform-required constants restore their explicit `ConstUseTemplate`.
Procedure-valued lookups are not restored as ordinary constants; they go
through erased callable construction using the checked procedure template or
stored `ConstStore` function value that names the callable. A checked lookup
without a resolved value reference is an invariant failure in boxy lowering.

Restoring a non-function `ConstStore` value in `.boxy` directly emits LIR for
the requested checked type. The const node is read from the module that owns the
stored value, while checked type interpretation uses the module named by the
const use. This distinction preserves imported type identity and host ABI
identity across module boundaries. Scalars, string views, lists, boxes, tuples,
records, tags, booleans, and nominals are materialized as ordinary LIR
construction statements under the already-planned boxy representation. The
lowerer validates that the requested checked type and the expression target
layout agree before emitting statements. Reserved const nodes, pending nodes,
compile-time eval templates that were not finalized, and `fn_value` nodes at an
ordinary value lookup are invariant failures; function values are restored only
through erased callable construction.

The producer representation for a stored value is keyed by the exact
`(ModuleId, ConstTypeId)` identity recorded by `ConstStore`, independently of
the checked type at the runtime use. This matters when the stored value is
monomorphic but the checked binding has a generalized type. Restoration first
emits the stored node under that exact producer representation, then crosses one
explicit planned boundary into the checked use representation. Stored callable
captures follow the same rule. Boxy planning and lowering do not scan the
checked type graph, infer a producer from the requested layout, or treat the
checked use type as evidence for the stored bytes.

`.boxy` represents an unknown type-variable value as one ordinary Roc box
payload pointer. This is the same runtime shape as `Box(T)`: a nullable or
non-null pointer-sized Roc value whose allocation stores the payload bytes and
whose refcount lives immediately before the data pointer according to the
ordinary Roc allocation layout. The type information needed to copy, drop,
allocate, inspect, or dispatch on that payload is not stored in the value.
It travels separately as explicit hidden data.

Function values in `.boxy` use the erased callable representation. A function
value is one Roc refcounted allocation whose data pointer is the function
value. The payload starts with the erased-callable header and stores capture
bytes after the fixed capture offset. Captures may include hidden descriptors
or dictionaries because the host ABI for `Box(function)` already treats
capture bytes as opaque. The value pointer and header layout do not change.
Zero-capture functions may use a static or otherwise immutable erased callable
payload, but their value shape is still the erased callable pointer.

The erased-callable header and public capture pointer are host ABI contracts.
Compiler-created callables may append compiler-private metadata after the
ordinary capture bytes, aligned independently of the public header. This
metadata is not part of `Payload`, does not move the capture pointer, and is
never required on host-created callable values. A compiler-created callable
records the exact immutable descriptor of the value its worker returns. The
runtime registration for that worker records its actual return layout and the
offset of the private metadata; it does not infer either from the call site.

Each executing dev image selects its own sidecar runtime for the current OS
thread so retained callables and overlapping hot-reload generations resolve
descriptor ids against the image that produced them. Libc-linked tools use
native TLS for that selection. The freestanding Linux machine-code shim has no
TLS startup runtime, so it keys the same selection explicitly by the kernel
thread id; it must not emit compiler TLS accesses or collapse concurrent image
selections into one process-global pointer. Standalone linked programs use the
separate process-global runtime installed from their embedded sidecar.

Source `Box(a)` does not add a second box merely because `.boxy` already
represents the internal type variable `a` as a boxed payload pointer. The
compiler-internal boxiness of `a` and the source-level `Box(a)` representation
are representation-equivalent in that case. This equivalence applies only when
one layer is the compiler's internal boxy representation of a type variable.
Explicit source nesting is preserved at concrete host boundaries: if the source
type is `Box(Box(U64))`, the host-visible representation has two ordinary Roc
box layers.

The representation of a container with an unknown item type is committed by
substituting the boxy value representation at the unknown positions. For
example, an internal `.boxy` record field of type `a` is a pointer-sized boxed
payload field, and an internal `.boxy` `List(a)` stores pointer-sized boxed
payload items. If such a value crosses a host ABI boundary, the wrapper uses
the exact checked host layout for that boundary and adapts between the
host-visible layout and the internal boxy layout. Host layout selection is
never derived from the internal boxy layout.

A checked record row is concrete in `.boxy` only when its extension is
explicitly closed by `empty_record` after following checked alias payloads. A
record whose row extension remains open, including the self-recursive empty row
used by checked row variables, is represented as a dynamic boxed value. This
prevents layout planning from inventing a concrete struct shape for a value
whose fields are still row-polymorphic. Field access on such a value is a
polymorphic operation and must be driven by explicit hidden row descriptor or
dictionary data; it is not recovered from field names during lowering.

### Boxy `TypeDesc`

A boxy `TypeDesc` is primarily runtime data for representation. It describes
the operations needed for a value representation:

- size and alignment of the payload representation when the payload is stored
  inside a box or copied between stack/heap slots
- whether the payload contains Roc-managed values
- the explicit nested drop/incref/free/copy plan for payload bytes
- the concrete LIR layout for known concrete payloads
- descriptor references for nested dynamic payload positions
- optional structural operation entries such as equality and hashing
- an optional planned `to_inspect` method slot for a nominal identity that can
  reach the generic `Str.inspect` intrinsic; this narrow method entry preserves
  nominal inspection after the value has been erased

The exact field order and encoding of `TypeDesc` is LIR-owned static data.
Every descriptor has an explicit id in the lowered program. Backends and the
interpreter consume descriptor ids or descriptor-pointer locals through LIR
statements; they do not synthesize descriptors from type names, layout shapes,
or object symbols.

Descriptors are never stored inside ordinary Roc values. A value of type
variable `a` is a one-word box pointer, not `{ data, desc }`. A record field,
list item, tag payload, function argument, return value, or host ABI slot
whose source value is `a` stores only the value representation. The descriptor
is passed as a hidden argument, captured by an erased callable, loaded from a
static descriptor table, or carried in a hidden local that the selected lowerer
introduced explicitly.

### Boxy Dictionaries And Vtables

A boxy dictionary is runtime data for polymorphic behavior and static dispatch.
It is distinct from `TypeDesc`. The descriptor-carried `to_inspect` slot is the
single exception: generic `Str.inspect` receives a value descriptor but no
method dictionary, so the checked inspect demand attaches that one planned
method adapter to the nominal descriptor. A dictionary may contain:

- method function pointers
- hidden `TypeDesc` references required by those methods
- nested dictionaries required by constrained arguments or results
- static metadata for checked dispatch plans

The dictionary is built from checked dispatch plans, checked method registries,
and checked type information. It is not built by method-name search at LIR time.
Boxy planning interns each checked method spelling into one program-wide runtime
slot. Every static dictionary reserves that full slot shape: a method required
by the dictionary occupies its interned slot, and every other slot is an
explicit absent entry. Consequently a dictionary carrying a superset of checked
requirements can satisfy a callee that uses a subset without remapping its
pointer or searching by a module-local method id.
If a polymorphic function requires a method dictionary, that dictionary is an
explicit hidden parameter or capture. If a concrete call site invokes the
function, the caller supplies the exact static dictionary for the concrete
checked type. If a higher-order function captures a polymorphic function, the
erased callable capture stores the hidden dictionary it needs.

LIR exposes indirect dictionary calls as ordinary explicit indirect calls with
a known LIR call shape. The LIR statement names the dictionary local or static
dictionary id, the method slot, the explicit Roc arguments, and every hidden
descriptor/dictionary argument. Dev codegen, LLVM codegen, and the interpreter
implement the same generic indirect-call primitive. They do not know static
checked dispatch behavior, do not look up methods by name, and do not reconstruct
dictionaries. Seeing an indirect call is still useful to a backend because it
can lower the call with the normal target calling convention, keep arguments in
registers, and avoid a universal trampoline.

The program-wide method slot is the runtime dispatch identity. `MethodNameId` values are
module-local checked ids and may be retained on LIR statements or slots for
diagnostics, but a runtime consumer must never compare them across modules or
scan a dictionary by numeric method id. It indexes the exact slot selected by
planning, requires that slot to be present, and validates only the slot bounds
and explicit call shape.

### Boxy Host ABI Adapters

The host ABI is independent of lowering strategy. `.boxy` changes only private
Roc implementation procedures. Any LIR root whose checked root metadata has
`RootAbi.platform` or `RootAbi.hosted`, and any provided static data export,
uses the exact host ABI layout derived from the checked source type. Hidden
`TypeDesc` or dictionary values are never added to a host-visible signature.

For a host-visible function, `.boxy` emits two logical procedure layers:

```text
host ABI wrapper/root proc
  exact checked arg/ret layouts
  no hidden args in the exported or hosted ABI
  adapts between host layouts and internal boxy layouts
  supplies static or derivable hidden TypeDesc/Dict values
  calls private boxy worker

private boxy worker
  internal boxy layouts
  explicit hidden TypeDesc/Dict params
  no host ABI exposure
```

Only the wrapper is listed in `root_procs` with the checked root metadata. The
private worker is an ordinary private LIR proc. Native entrypoint wrappers,
interpreter shims, glue, static data export, and ABI cache digests therefore see
the same host layouts under `.lss` and `.boxy`.

A root plan records both the host relation type and the exact checked
implementation-definition type reached through its procedure source. The host
relation determines the public ABI and the worker boundary; the implementation
definition supplies the concrete call substitution used to plan every hidden
descriptor and dictionary argument. A host wrapper consumes those planned
static mappings. It must not treat a generalized platform relation as concrete
or rediscover the implementation type while lowering.

Hosted calls use the same rule in the opposite direction. A checked hosted
template is resolved through the checked hosted-procedure table and lowers to a
bodyless LIR hosted proc spec with the exact hosted symbol and dispatch index.
The boxy lowerer never lowers the checked `hosted_lambda` body as ordinary Roc
code. The LIR hosted proc retains its exact checked hosted ABI. A boxy call
site adapts internal boxy arguments into that ABI, calls the hosted proc, and
adapts the result back to the internal boxy representation when needed. It must
not change the hosted symbol signature and must not ask the host to provide
hidden descriptors.

`RocBox(RocUnknown)` at the host boundary is opaque unless Roc already has
explicit descriptor data on the Roc side. The host ABI passes only the Roc box
pointer. There is no ABI slot for a payload descriptor, and the Roc box
allocation does not contain one. Therefore `.boxy` may retain, move, return, or
shallow-drop an opaque box according to the existing host ABI contract, but it
must not structurally inspect or recursively drop an unknown payload unless an
explicit `TypeDesc` is available from checked Roc-side data. The compiler must
not use a default descriptor, assume the payload has no nested Roc values, or
derive a descriptor from runtime bytes.

### Compile-Time Evaluation Strategy

`--specialize` does not affect compile-time evaluation during checking
finalization. Compile-time evaluation always selects `.lss` explicitly, uses
the existing checked-finalization pipeline described in Compile-Time Constants,
runs the LIR interpreter or native dev evaluator, and stores checked values in
`ConstStore`. Runtime `.boxy` builds do not make compile-time roots use boxy
descriptors, dictionaries, or host ABI adapters.

### Strategy Equivalence Tests

The compiler does not perform production-release ABI equivalence verification
between `.lss` and `.boxy`. Doing so would spend end-user time on an invariant
that the compiler test suite owns.

Tests must verify that the two strategies produce the same host-visible ABI for
the same CheckedModule: exported and hosted symbol signatures, lowered C ABI
placements, glue type tables, provided static data layouts, entrypoint ABI
digests, and root metadata. Differences in private proc shape, hidden
descriptors/dictionaries, indirect calls, RC statement count, backend code size,
or runtime performance are allowed.

## Shared Post-Check Model

Every typed post-check IR has an explicit typed store. `.lss` uses this for
the Monotype, Monotype Lifted, Lambda Solved, and Lambda Mono stages. `.boxy`
does not create those stages, but its checked-to-LIR lowerer still uses
explicit typed side stores for boxy `TypeDesc`, dictionary, hidden-argument,
and adapter plans while building LIR.

```zig
pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const TypeId = enum(u32) { _ };

pub fn Span(comptime T: type) type {
    return extern struct {
        start: u32,
        len: u32,
    };
}
```

Type-store ownership is explicit at each stage boundary:

- Monotype IR owns the Monotype type store.
- Monotype Lifted IR uses the same Monotype type store because lifting does not
  change types.
- Lambda Solved IR owns a new type store with lambda-set variables.
- Lambda Mono owns a new type store with no function types.
- Boxy lowering consumes the checked type store and owns boxy descriptor and
  dictionary stores. It does not allocate a Monotype or Lambda Solved type
  store.
- LIR owns committed layouts, not post-check type ids.

A mutable post-check type store exposes child spans as stable descriptors plus
indexed item access, never as borrowed slices. Appending while lazily expanding
a recursive type may relocate a packed side array, but its descriptors remain
valid. Raw slice access belongs only to immutable stage views, where relocation
is impossible and consumers retain packed-array iteration performance.

A later stage must not reinterpret an earlier stage's type ids unless the stage
contract says the type store is shared. When a stage changes type meaning, it
consumes the earlier program and produces a new program whose ids are meaningful
only in the new stage.

Symbols are stable compiler names for procedures, local binders, generated
functions, and compiler-created temporaries. A symbol is not a layout id, object
symbol, tag discriminant, or ABI identity.

Stage-local lookup tables are allowed when they are pure indices over data the
stage owns, such as:

- symbol to local environment
- source procedure to specialization record
- type id to already-lowered type id
- direct-lowering type decision to committed LIR layout id
- direct-lowering procedure decision to LIR procedure id

Those tables are not hidden checked-data side channels. They must not contain
data that are missing from the produced IR. If deleting a table would make it
impossible to understand what the output means, the table is an illicit
representation store and the design is wrong.

Stage-local algorithmic worklists are allowed for SCC traversal, unification,
pattern decision construction, layout graph traversal, and similar internal
algorithms. These worklists do not cross stage boundaries and do not output
checked data.

The only meaning-producing worklists in post-check compilation are stage-local
strategy worklists. In `.lss`, these are specialization queues driven by
explicit calls or roots discovered while lowering the previous stage. In
`.boxy`, these are descriptor, dictionary, adapter, and private-worker queues
driven by explicit checked roots, checked type positions, checked dispatch
plans, and LIR statements emitted by the boxy lowerer. They are not general
post-demand repair lists.

Boxy planning runs mutually dependent discovery phases to a fixed point. A
phase that can append to a collection it traverses snapshots that collection's
entry count before traversal; newly discovered entries are consumed by the next
outer iteration. A phase may traverse a live, growing collection only when it
cannot append to that same collection. The fixed-point termination check covers
every collection whose growth can expose additional workers, substitutions,
inspect methods, descriptors, or dictionaries. This prevents reallocation from
invalidating the current traversal and makes discovery order irrelevant to the
planned collection contents.

## Monotype IR

Monotype IR is the first post-check typed representation in the `.lss`
strategy and in the existing compile-time evaluation pipeline. `.boxy` runtime
lowering does not construct Monotype IR. Monotype keeps only the expression and
pattern forms that are meaningful after checking, and every
expression and pattern has a monomorphic type.

Monotype IR is produced from checked modules and explicit root requests. It
performs three jobs:

1. Clone and instantiate checked types into monomorphic type nodes.
2. Create exactly the monomorphic procedure/value specializations reachable
   from the requested roots.
3. Consume checked call resolutions, emitting direct calls/operations or the
   checked structural derivation before any source dispatch form can enter the
   output IR.

### Monotype Types

Monotype types contain no lambda sets.

```zig
const MonoType = union(enum) {
    primitive: Primitive,
    named: NamedType,
    func: Fn,
    record: Span(Field),
    tag_union: Span(Tag),
    tuple: Span(TypeId),
    list: TypeId,
    box: TypeId,
    erased: TypeDigest,
};

const TypeDigest = struct { bytes: [32]u8 };

const TypeDef = struct {
    module: ModuleIdentityId,
    type_name: TypeNameId,
    source_decl: ?u32,
    generated: ?TypeDigest,
    iterator_representation: IteratorRepresentation,
    iterator_depth: u8,
};

const IteratorRepresentation = enum(u8) {
    none,
    minted,
    forced_dynamic,
};

const Fn = struct {
    args: Span(TypeId),
    ret: TypeId,
};

const Field = struct {
    name: RecordFieldLabelId,
    ty: TypeId,
};

const Tag = struct {
    name: TagLabelId,
    payloads: Span(TypeId),
};

const NamedType = struct {
    def: TypeDef,
    kind: NamedKind,
    args: Span(TypeId),
    backing: ?NamedBacking,
};

const NamedBacking = struct {
    ty: TypeId,
    authority: BackingAuthority,
};

const NamedKind = enum {
    nominal,
    opaque,
    alias,
};

const BackingAuthority = union(enum) {
    inspectable,
    runtime_layout_only,
};

const DispatchOwnerHead = union(enum) {
    builtin: BuiltinOwner,
    type_def: TypeDef,
    none,
};
```

`DispatchOwnerHead` is not stored as a duplicate field on every type node. It
is read from type content on demand (the alias-transparent
`dispatchHeadContent` accessor plus the component-lookup seam's key builder):

```zig
fn dispatchOwnerHead(types: *const TypeStore, ty: TypeId) DispatchOwnerHead
```

Builtin type content returns the corresponding builtin owner. `named` type
content returns `type_def` when the definition can own methods, even if
its runtime representation later uses a transparent backing. Anonymous
structural types and non-owning internal nodes return `none`.

This head is only ever a lookup KEY for the exact `(MethodOwner,
MethodNameId)` registry table, consulted for compiler-generated call edges—
it is checked type identity carried on the monotype, not a dispatch decision.
Plan-resolved user dispatch never reads it; those targets come from checked
evidence alone.

Record fields and tag variants are stored in lexicographic order by name. Tag
payloads are stored in payload position order. The index of a field or tag
inside its span is the source row position; it is not a runtime discriminant
or layout slot. Runtime tag discriminants, payload layout, and field offsets are
not chosen until direct LIR lowering commits layouts.

Monotype IR does not need a separate row-closure or row-reconstruction stage.
Row closure, nominal backing instantiation, and structural child ordering are
completed while constructing Monotype types from checked types. The explicit
field-kind commitment described above is part of instantiation-graph relation
freeze, before any Monotype type is sealed. Numeric defaulting is
split by the checked `numeric_default_phase` data: checking defaults open
literals (the first candidate in the numeric default candidate order, `Dec` first, that
satisfies the literal's dispatch constraints), and Monotype commits only the
per-specialization residue of generalized literals.

### Monotype Instantiation

Monotype lowering is a specialization-time instantiation of checked type graphs.
This is the same core model as Cor/LSS: each reachable monomorphic
specialization starts with the checked function/value type graph, creates a
fresh stage-local instantiation for that specialization, constrains the root of
that instantiation to the requested monomorphic type, and lowers the body from
that constrained graph.

The long-term invariant is:

```text
one procedure specialization request
  -> one complete checked interface-relation closure
  -> one exact closed specialization key
  -> one independently owned Monotype body on cache miss
```

This is deliberately different from treating a checked expression id as a
globally reusable monomorphic expression. A checked expression belongs to the
checked module; a Monotype expression belongs to one concrete specialization of
that checked module. The same checked function template may therefore produce
many Monotype bodies, and the same checked nested lambda site may produce many
nested Monotype functions, each with a different monomorphic function type.

Each independently sealed specialization group owns an instantiation graph:
union-find nodes with explicit row-extension links, created by instantiating
checked types on first touch. An ordinary procedure body begins a group by
itself. The root producer may explicitly mark adjacent procedure-template roots
as one shared group so a callee request proven equivalent under the complete
graph-local specialization identity is reused before a second root replays that
request. Test plans select this grouping; ordinary build and platform roots are
independently sealed. Every root still uses a fresh instantiation scope, owns
its own body and durable specialization record, and contributes its own checked
relations; sharing the graph never authorizes importing a checked node from
another scope. The reuse key includes callable family, method scope, checked
source-function key, exact evidence topology, lexical context, and the exact
function request interface. A different or still-unproven request remains
independent.

Instantiation graph node ids are dense, append-only indexes for the lifetime of
the graph. Per-node optional attributes such as a row root's current extension
and a generated-private request's source interface are therefore dense parallel
columns, not hash tables keyed by node id. Union-find redirects may change which
node is a class root, but they never renumber a node; root-owned columns are
updated explicitly when a union moves that ownership.

A context-free callee body never joins the caller group's graph. Instead
CheckedModule stores a complete specialization-interface relation table for
every procedure template. Its records explicitly name checked equalities,
procedure/result relations, ordinary call interfaces and direct targets, and
generalized local-procedure uses. A generalized scope also records its exact
checked scheme root, so evidence paths are replayed against the same callable
shape that authored them. Static-dispatch relation records remain the separate
explicit authority for dispatch constraints.
The `CheckedBodyStore.contains_diagnostic_error` column excludes rejected
expression sites from this relation table; an error-containing checked type is
not a valid specialization constraint and never reaches Monotype instantiation.
Monotype consumes both relation sources before applying defaults or sealing a
specialization request. It does not scan a checked body to rediscover them.
Interface replay materializes local-method evidence from the checked method
declaration and callable type only. Draft-local capture contexts are Monotype
BodyDraft data, are intentionally absent from durable specialization evidence, and
must be attached when the real dispatch call lowers; declaration-only replay
evidence can never be consumed by body emission.

Within the specialization, each body instantiation context has an exact fresh
scope identity, owns one checked module id, and caches nodes by checked type id
inside that `(scope id, checked module id)` context. The module id is an
invariant of the context rather than a repeated hash-map key; entering another
checked module creates another context before any type id from that module is
looked up. The resulting address is still the exact checked identity of the type
variable/content in that body specialization. It is not a structural digest,
source name, runtime layout, object symbol, or generated procedure id. A child
that needs independent generic cells receives a new scope identity; copying
cells into that scope is explicit. Nodes begin unresolved. As relations are
produced, explicit evidence from checked data unifies those nodes:

- the requested root function/value type constrains the checked root type;
- lambda and closure expected function types constrain the nested function
  specialization they create;
- call arguments constrain the callee instantiation through the checked formal
  and actual type relation;
- call results constrain the callee return type and the caller result type;
- a matched record field in deferred template-interface replay relates its
  explicit field-kind cell, source-value cell, and runtime-slot cell as three
  distinct pieces of checked interface evidence;
- static-dispatch plans constrain dispatcher, callable, operand, and result
  types;
- numeric literals and checked numeric defaults constrain numeric type cells;
- named type uses constrain their declaration formals to the instantiated named
  arguments;
- pattern lowering constrains checked pattern types to the monomorphic value
  being matched.

Direct calls in the interface program are dependency edges to the callee's
interface program, not requests to lower that callee's body. Replay first
applies all relations owned by the current scope, then traverses those explicit
dependencies. This ordering makes the caller's complete requested interface
available before any dependency identity is chosen. Transitive replay reaches
a fixed point across arbitrary wrapper depth and recursive call graphs without
making source syntax or body-lowering order part of type meaning.

Repeated open dependency requests are memoized by the complete procedure
family (template, method scope, and checked source-function key), exact evidence
topology, and an immutable provisional Monotype view of the function request
after the caller-owned relations have been applied. Digests select an expected
O(1) bucket only; exact evidence equality and exact structural type equality are
the collision authorities. The first request computes the transitive relation
closure. Equivalent requests retain independent graph cells while relations
are still being produced, then independently consume the representative's
final interface after the whole closure is known. A representative interface
whose checked field-presence cell is still undetermined retains that explicit
state in the provisional view; each duplicate instantiates it into fresh
field-kind, source-value, and runtime-slot graph cells rather than sharing the
representative or committing a slot encoding. An active exact memo entry is
a recursive edge and joins the active representative. Requests with different
concrete interfaces, checked source identities, method scopes, or evidence can
never share an entry. Work is therefore proportional to relation sites plus
unique exact provisional requests, rather than to the number of duplicate call
paths through the same interface problem.

Those constraints are not a fallback mechanism and are not best-effort
inference after checking. They are the Monotype-stage representation of checked
data that are already present in the checked module. If a required relation is
missing from checked output, the producer is incomplete and must be fixed.

Nominal instantiation relies on a stronger CheckedModule invariant than
"the same source name appears twice." A checked nominal declaration owns an
explicit declaration template:

- `formal_args` are the checked roots for the declaration header parameters;
- `backing` is the checked root for the declaration backing template;
- every rigid occurrence in the backing template that refers to a header
  parameter must point at the same checked root as that header formal.

This root identity is the long-term ideal because it makes nominal
instantiation dataflow explicit. `Codec(input, value)` does not require
Monotype, layout lowering, or a backend to rediscover that the `input` in
`run : input -> ...` is the first nominal parameter by reading source text or
matching display names. CheckedModule data stores that relation once, as
checked root identity. Monotype then constrains declaration formal roots to the
concrete named arguments for the current specialization and lowers the
declaration backing through those cells. The result is a backing type in which
every formal occurrence has the same monomorphic meaning as the named type
argument that instantiated it.

Monotype must use the declaration backing template for ordinary local nominal
declarations. For local declarations, the `backing` root on a nominal-use
payload is not the authority for declaration-template instantiation; the
declaration template is authoritative because it carries the formal roots that
connect header parameters to backing occurrences. For imported nominal
declarations, the current CheckedModule must contain the `backing` root that
`CheckedTypeProjector` writes on the nominal-use payload, so Monotype can
consume that root directly without reaching into the source module's
declaration template. Box payload capabilities remain separate explicit
representation authorities; their backing roots come from the capability entry
in checked module data instead of from declaration template lookup.

Named type ownership is already decided before Monotype lowering starts. A
checked alias or nominal payload names its owner checked module id explicitly,
and the lowering input includes every checked module id recorded in checked
lowering visibility. Monotype may build a stage-local lookup table from those
ids to module views for speed, but that lookup is only an address table over
explicit checked data. The source-origin identity remains part of the lowered
type definition identity; the owner checked module id is the module address used
to find checked declarations, representation authorities, method owners, and
type-store entries.

This solves two classes of bugs:

- generic nominal backings cannot accidentally swap, lose, or default one
  type parameter while the named node itself has the right arguments;
- post-check stages do not need syntax-name matching, declaration scanning, or
  layout inspection to recover how a named type's representation is
  instantiated.

The instantiation context must be the only owner of checked-type-to-Monotype
state inside a specialization. Later lowering code must ask the context for the
Monotype type of a checked type, or must add an explicit constraint to the
context. It must not recover types by scanning source syntax, comparing display
strings, deriving names, inspecting layouts, or using incidental expression
shape. It must also not attach a contextual monotype to a checked expression id
as if that checked expression were a reusable runtime value.

Type-only instantiation state is separate from operational body-lowering state.
Creating a fresh checked-type instance swaps only its exact scope, checked-node
cache, and nominal declaration-scope stack; it does not construct a parallel
body-lowering context. Type-only instantiation contexts do not materialize
module-sized body tables.
The dense checked-binder-to-draft-local table is allocated only when a body
actually installs its first binding. Checked string literals are shared under
the exact `(draft owner, checked module id, checked literal id)` address, so
child and call contexts lowering the same retained body neither allocate
parallel literal tables nor append duplicate draft literals. A draft value is
never reused across body owners: suppressing one owner must suppress all of the
content referenced only by that owner. Generated strings remain ordinary
distinct draft entries because they have no checked literal identity.

Checked roots explicitly record whether their graph contains identity
variables, but closure does not authorize reuse across instantiation scopes.
Specialization relations may still refine representation-bearing nominal
backings below a closed public type. Every checked graph therefore instantiates
fresh relation-production cells in its exact scope. Immutable `TypeId` imports
are reserved for types explicitly completed by an earlier Monotype stage.
Function roots and their components remain scope-local request-interface
identities even when the complete checked graph is closed, so distinct
callable requests never merge their relation-production identity.

This distinction matters most for lambdas and closures. Expression-position
functions are checked templates. Lowering a lambda or closure at an expected
function type creates or reuses a nested Monotype function specialization keyed
by the checked nested site, the current function digest, the checked source
function type digest, and the monomorphic function type digest. The complete
checked function root is related to the expected function interface before the
nested body lowers, and the body lowers against that exact request interface.
The request is not a constraint on the parent expression id. This allows the
same checked lambda site to be specialized at multiple function types without
corrupting the parent body or depending on traversal order.

Structural equality follows the same rule. The checker has already established
that the operands are equality-compatible and has either emitted a dispatch plan
that permits derived `is_eq` to lower as structural equality or rewritten the
expression to an explicit structural equality node. Monotype lowering
constrains the two checked operand types to the same instantiation relation and
lowers both operands at that single Monotype operand type. It must not
independently lower the left and right operand types and then attempt to
reconcile the results. Independent operand lowering is order-sensitive: an
unconstrained operand can default to an uninhabited type before the other
operand provides evidence. A shared instantiated operand type preserves the
checked equality relation directly.

The reason this is the long-term design rather than a local implementation
detail is that it makes specialization, dispatch, lambda lowering, and equality
all obey the same ownership rule:

```text
checked stage owns meaning and relations
Monotype instantiation owns monomorphic type cells
later stages consume closed Monotype types only
```

That rule removes a class of bugs caused by contextual rebinding. In the old
failure mode, one traversal path could lower a checked type variable to an
empty tag union or one concrete function type, and a later traversal path could
encounter the same checked type under better evidence and try to assign a
different Monotype type. That is not a valid compiler state; it is evidence that
the stage was not lowering from one constrained specialization graph. The
instantiation model makes the intended data flow explicit, so the first
constraint and every later constraint meet in the same graph node before the
final Monotype body is emitted.

During active Monotype specialization, unresolved checked variables and row
extensions remain instantiation graph nodes. They are not represented by
durable Monotype `TypeId`s.

Type-shaped inspection that can escape relation production or become durable
specialization identity is allowed only for a fully resolved graph node. It
materializes an immutable active snapshot: later graph relations invalidate the
snapshot cache and a subsequent inspection allocates a fresh snapshot rather
than refilling an observed `TypeId`. The draft retains the graph node, not the
snapshot id, and final sealing allocates fresh durable ids. Consequently no
durable `TypeId` can change shape after a consumer has seen it.

Snapshot invalidation is logically immediate but may be physically coalesced.
A relation mutation marks the complete active-snapshot cache stale; the next
inspection clears it once before performing any lookup. Multiple mutations with
no intervening inspection therefore do not repeatedly clear the same cache, and
no inspection may consume an entry produced before the most recent mutation.

Interface-replay memo lookup has one narrower inspection operation. It may
materialize an unresolved request as an immutable provisional scratch view,
applying defaults in that view only. The digest is only a bucket index; exact
structural equality is collision authority, the scratch type is never emitted,
and subsequent relations still act on the original graph node.

The only time an unresolved checked variable with an empty-tag-union row
default may become durable `tag_union []` is final graph sealing, after every
checked interface relation and specialization demand for that body has been
applied.
After sealing, `tag_union []` is closed and uninhabited. Values such as `[]` can
still be represented as `List(tag_union [])` because they contain no items,
and code that would need an actual item value must have constrained the
item type earlier or must be unreachable at runtime.

Expression lowering is demand-aware. A runtime-value demand requires a
constructible monomorphic value. If a checked generic value remains
unconstrained and no runtime value can exist at its final type, lowering it
under a runtime-value demand is a compiler invariant violation.

Runtime-reachability guards captured while lowering a branch can exempt a
demand whose value is proven unreachable, but they are not part of
specialization identity. A request beneath one guard context may reuse a
specialization created beneath another; each such reuse is recorded, and final
sealing re-verifies the reused body's runtime-value demands. A demand already
certified under its creation context covers the one emitted body—
statement-position guards are not monotonic across call sites in one block—
and a reuse whose own context cannot certify an otherwise-uncertified demand
is a compiler invariant violation. Bodies are never duplicated per guard
context.

An inspect-only demand may render results determined by type or callable
identity without lowering a runtime value into Monotype IR. For example,
inspecting a standalone function value may produce `<function>` without
lowering the function body. A subsequent call, export, dispatch target, or
other body-specialization demand must request a concrete body specialization
with sufficient type evidence.

During Monotype construction, an open checked variable is an unresolved graph
node carrying the variable's numeric and row defaults. Unification resolves it
when call-site arguments, expected lambda types, numeric literals, or checked
type relations provide concrete evidence; defaults apply only at the declared
relation-freeze/final-sealing boundary. A context-free root with no requested
Monotype type still instantiates its checked type in a fresh graph and crosses
that boundary; it must not enter the context-free direct-type cache while a
generalized field kind remains open. While solving is still active, users hold instantiation graph
nodes rather than final Monotype type ids. Final graph sealing turns solved
graph nodes into immutable interned Monotype type nodes. Recursive groups may
reserve their ids inside the type interner while the group is being sealed, but
no type id that is visible in Monotype IR is later refilled or changed. This is
ordinary type solving inside one stage. Once Monotype IR is output, no
unresolved node remains reachable and no later stage may change a type.

A Monotype imported into another specialization graph is a finished snapshot,
never a refreshable view: a specialization that needs more than its requested
type is a unification conflict, not a silent rewrite of another group's final
type. Every context-free procedure-template request defers until the requesting
graph is final, when its specialization key is stable. Constraints formerly
owned only by an unresolved callee body are present in the checked interface
program and have already participated in the request's relation closure.
Lexically context-dependent local procedures still lower in their owning graph
because that lexical context is an explicit input rather than a context-free
specialization key.

A deferred procedure-template request has two distinct sources of type
evidence. Caller value flow owns the request's function arguments and return;
the requested checked template owns explicit type-constructor arguments that
may have no value-level occurrence, including phantom nominal arguments. The
caller first relates its complete request to one fresh checked root instance,
then replays the template's interface program through that instance. Each
sibling request receives a fresh instance, so different concrete value
arguments never become aliases merely because they call the same checked
procedure. Exact replay memo hits copy the final interface back into each
request independently; they do not union sibling request cells.

A deferred request stores only the caller-owned interface and a draft call
target. Once the caller graph is frozen, the interface is sealed exactly once
and the target maps directly to an existing exact specialization or to a body
lowered in a fresh graph on miss. Generated structural work may retain explicit
lexical context when that context is one of its inputs, but it follows the same
procedure-body ownership rule; encoding and decoding do not define a separate
specialization path.

A fresh procedure specialization reserves its global function identity before
lowering its body and records that reservation as the active root owner. A call
from beneath that owner may rejoin the reservation when it names the same
checked procedure and method scope, carries the same dispatch evidence, and
matches the active function interface. The checked source root used to reach
the declaration is not recursive ownership: different checked occurrences may
reach the same procedure. For a synthesized partial interface, at least one
argument must overlap the root's initially snapshotted argument classes before
the edge can be classified as recursive. This is the same explicit ownership
rule used by draft-local procedures and prevents either an accidental second
body for the root or a merge of unrelated sibling requests.

Checking must also validate a mono-specialization default against the complete
method callable type before placing direct evidence in the checked dispatch
plan. A same-named method on the default owner is insufficient. An incompatible
default target is a checked type error and the checked body site is poisoned
there. Monotype instantiates the target declaration recorded in the checked
plan at that plan's call arguments; it does not accept an incompatible
relation.

Instantiation graph nodes are cached by the owning checked module id and the
exact checked type id. They are not cached by `TypeDigest`. A digest can
identify closed structural type content for specialization and comparison, but
it cannot distinguish two different open checked variables with the same shape.
Treating those variables as the same node is a compiler bug. Type digests are
alias-transparent and encode recursive back references, so structurally equal
types digest equally regardless of alias spelling or knot-tying ids.

Generated helper code for an empty tag union, such as an inspector requested
only because a container type mentions the empty tag union, has an unreachable
body. Reaching that helper means a runtime value of an uninhabited type existed,
which is a compiler or unsafe-runtime bug.

If Monotype lowering cannot construct a closed monomorphic type from checked
data, that is a compiler bug.

### Row, Nominal, Alias, And Opaque Authority

Monotype lowering is the sole owner of turning checked type data into closed
Monotype type nodes.

For records and tag unions, checking outputs the checked row ids and stored
spans. Record fields and tag variants use lexicographic order by name. Tag
payloads use payload position order. Monotype lowering copies those spans
directly. It does not sort by display text, declaration spelling, runtime
encoding, or incidental map iteration.

Nominal records additionally carry their declared field order as separate
explicit data, because their runtime layout follows declaration order rather
than the lexicographic row order (see Nominal Record Field Order). The
lexicographic row order remains the identity used for field-name resolution;
declared order feeds only layout. In CheckedModule data this is a flat
`CheckedDeclaredField` pool. A named entry stores the record-field label that
must be matched against the lexicographic backing row; a padding entry stores
the ordinal of the corresponding checked padding type in
`padding_field_types`. The padding type itself is not duplicated in the
declared-order entry, so generic nominal instantiation substitutes padding
types in exactly one place. These stay two separate data.

For named types, checking outputs:

- the `TypeDef`
- whether the definition can own methods
- the checked type parameters
- the checked backing type and backing authority, when the compiler has a
  checked representation entry for this definition
- opacity/interface data controlling whether the backing may be inspected
  by Monotype or used only for runtime layout

Monotype lowering instantiates those data. It does not scan declarations to
rediscover a backing, owner, or field order. If a named type is opaque at the
current boundary, Monotype still preserves the named type node and therefore
the dispatch owner derivable from it. A `runtime_layout_only` backing may be
used by layout lowering to represent values, but it is not permission
for Monotype or static dispatch to inspect through the opaque boundary. If no
backing is present, any stage that needs the representation must consume a
separate explicit checked representation authority; it must not rediscover the
backing by scanning declarations.

This keeps static-dispatch ownership, source row order, and eventual runtime
layout as three separate data.

### Monotype Expressions

Monotype expressions preserve the post-check expression shape, not source syntax
that has already served checking.

```zig
const Expr = struct {
    ty: TypeId,
    data: ExprData,
};

const ExprData = union(enum) {
    var_: Symbol,
    int_lit: IntLiteral,
    frac_lit: FracLiteral,
    dec_lit: DecLiteral,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    record_update: struct { base: ExprId, fields: Span(FieldExpr) },
    tag: TagExpr,
    nominal: ExprId,

    let_: struct { bind: TypedSymbol, body: ExprId, rest: ExprId },
    lambda: LambdaExpr,
    fn_def: FnDef,
    call_value: CallValue,
    call_proc: CallProc,
    low_level: LowLevelCall,

    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};

const LoopExpr = struct {
    params: Span(TypedSymbol),
    initial: Span(ExprId),
    body: ExprId,
};

const ContinueExpr = struct {
    values: Span(ExprId),
};

const LambdaExpr = struct {
    args: Span(TypedSymbol),
    body: ExprId,
    source: FnTemplate,
};
```

`record` is a complete constructor: its field span contains every field in the
expression type. `record_update` preserves the evaluated base and exactly the
explicitly updated fields until LIR construction. This distinction is required
while a Monotype solve group is open: the result type can gain fields from later
constraints, so enumerating the current type shape and expanding an update at
that point would permanently omit fields that the final closed type contains.
Lambda Solved relates the base to the update result and checks each explicit
field against the final record type. Direct LIR lowering evaluates the base
once, reads all unchanged fields before evaluating update expressions, and
writes the final fields in committed runtime order. A `record_update` may carry
a structural, alias, or nominal record result type directly; unlike `record`,
`tuple`, and `tag`, it is a base-preserving transformation rather than a fresh
structural constructor that must be wrapped to construct a nominal value. No
later stage reconstructs the base or missing fields from source syntax.

`LoopExpr.params` and `LoopExpr.initial` have the same length. Each initial
value has the type of the corresponding parameter. Every `continue_` reached
from the loop body supplies exactly one value for every loop parameter, in the
same order and with the same types. Loops with no carried state use empty spans.
`break_` carries the loop result when the loop is value-producing and carries no
expression when the loop result is unit/control-only.

Monotype IR has no:

- source static-dispatch call
- source type-dispatch call
- source method-equality node
- source `for` node
- source row variable requiring closure
- uninstantiated checked type variable
- missing checked owner address

`FnDef` is the checked identity for a checked, imported, nested, hosted,
promoted, or checked-stage generated function. It does not contain a capture
record, closure layout, callable tag, erased ABI, or lowered call target.
Captures remain ordinary free variables until Monotype Lifted IR records them
on lifted function definitions.

Checked capture identities are construction-time provenance, not durable
post-check value identity. One checked binder can materialize into several
runtime values when specialization instantiates it more than once. When a
Monotype body materialization is committed, each of its checked capture
identities receives a program-global identity derived from the first final
`LocalId` in that equivalence class. Local aliases within the same
materialization retain one identity; a separate materialization receives a
different identity even when it came from the same checked binder. The checked
binder remains separate metadata for lexical binding and substitution. The original checked capture
identity is also carried in a separate provenance field solely for writing a
compile-time result back to `ConstStore`; it is never used for runtime capture
joining. Consequently, separate
materializations cannot collide merely because they came from one checked
binder. A downstream one-to-one capture rewrite preserves the complete
post-check capture identity explicitly, while a one-to-many materialization
receives distinct identities at the producer boundary. Lifting and specialization
must join capture slots and operands only by that explicit post-check identity;
they never recover identity from binder, symbol, type, source text, or runtime
representation.

`CaptureId`'s raw bits reserve disjoint source-authored, check-generated, and
lift-generated namespaces; the raw integer is therefore not itself a dense
array offset. ID-keyed columns use the explicit `(index, namespace)` dense
ordinal supplied by `CaptureId`, which interleaves the three namespaces without
hashing or allocating across their reserved high-bit gaps.

Draft body ownership is equally strict. A copied lexical binder map may expose
an enclosing value to a nested function, but a source binding pattern always
materializes its runtime local under the current specialization owner. It may
reuse only a pre-registered local owned by that same owner; it must never bind
to a local owned by an enclosing or sibling materialization merely because the
checked binder id is the same. Draft sealing retains or suppresses whole owned
specializations and rejects every retained record that references suppressed
owned content, so cross-materialization local reuse cannot become durable
Monotype IR.

Open nested-specialization reuse is therefore owner-scoped. Before lifting, a
nested body refers to `DraftLocalId`s from one explicit materialization owner;
equal function interfaces, capture types, or checked binder provenance do not
make those locals interchangeable with another owner's locals. Only requests
from the same draft owner may reuse an in-progress nested body. Final
specialization identity and capture ABI identity remain responsible for durable
deduplication after owner-local bodies have been sealed.

### Monotype Specialization

Monotype specialization is `.lss`-only and root driven.

```zig
const MonoSpec = struct {
    proc_template: ProcTemplate,
    fn_ty: TypeId,
};
```

The Monotype builder starts from explicit root requests and checked procedure
templates. When lowering a call to a checked procedure template, it creates or
reuses the specialization keyed by the instantiated monomorphic function type.
The specialization queue continues until no reachable call requests a new
specialization.

Imported modules participate exactly like local modules: their checked modules
provide procedure templates, method registry entries, checked types,
and checked bodies. Module boundaries do not erase or hide callable information
from the final program.

The specialization key is intentionally based on checked callable identity plus
the requested Monotype function type, not on an incidental lowered body or
runtime layout. For top-level, imported, hosted, promoted, and platform-required
procedures, the checked callable identity is the checked procedure template. For
expression-position functions, the checked callable identity is the nested site
inside the checked owner template plus the current function digest. The
Monotype function type is part of the key because one checked callable can be
instantiated many times.

Creating a specialization performs root instantiation before body lowering:

```text
create fresh instantiation context
constrain checked source function type to requested Monotype function type
replay the complete checked specialization-interface relation closure
seal and look up the exact specialization identity
lower arguments and body through that context
emit a closed Monotype definition
```

Calls do not mutate the callee's checked module. A closed call creates or reuses
a callee specialization by constraining a fresh callee instantiation from the
caller's instantiated argument and result types. An open call remains a
caller-owned request while the checked interface programs communicate all
callee-owned constraints through explicit graph relations. The request seals
before any context-free callee body lowers. This is why generic functions
specialize predictably across module boundaries: checked bodies remain
immutable, every monomorphic specialization records its own closed
instantiation, and interface solving never depends on lowering a body into its
caller.

The specialization store must make this lookup direct. It must not scan all
specializations for a callable family and recompute recursive type digests while
lowering a body. A specialization request is identified by:

```zig
const SpecIdentity = struct {
    callable: CallableIdentity,
    method_scope: CheckedModuleDigest,
    source_fn_ty_digest: TypeDigest,
    evidence_digest: EvidenceDigest,
    request_fn_ty_digest: TypeDigest,
    request_fn_ty: TypeId,
};

const CallableIdentity = union(enum) {
    proc_template: struct {
        module: CheckedModuleId,
        proc: ProcTemplateId,
    },
    nested_site: struct {
        module: CheckedModuleId,
        owner_proc: ProcTemplateId,
        owner_fn_digest: FnDigest,
        site: NestedSiteId,
    },
    hosted: HostedId,
    generated: GeneratedId,
};

const SpecStatus = enum {
    reserved,
    lowering,
    ready,
};

const SpecRecord = struct {
    identity: SpecIdentity,
    request_fn_ty: TypeId,
    request_fn_ty_digest: TypeDigest,
    solved_fn_ty: TypeId,
    solved_fn_ty_digest: TypeDigest,
    fn: FnId,
    status: SpecStatus,
};
```

`method_scope` records the exact checked registry scope that selected static
dispatch inside the body; it participates in both draft and durable lookup
keys. `source_fn_ty_digest` records the checked source function type after
instantiation into the requesting graph. `evidence_digest` accelerates lookup
of the exact retained dispatch-evidence topology. `request_fn_ty_digest`
records the closed function type REQUESTED by the call site that reserved the
record. The digests make lookup fast, but they are not the only correctness
check. When a digest match is found, the store must also verify the checked
callable identity, method scope, exact evidence topology, and exact structural
equality of the closed Monotype function type. Digest collisions are therefore
harmless.

The identity is immutable: it is written once when the record is reserved and
never rewritten, so no structure that indexes by identity ever needs a rekey or
a second synchronized entry. Later refinements are data on the record. The
request view may be refined while the record is still `reserved`—once per
deferring graph that seals its view of the request; the solved view records the
body's solved type when the record becomes `ready`. Each refinement registers
an *alias* lookup entry (the new digest also reaches the same record), so a
request shaped like the current request reuses the record even after the body
solved a more specific type—the record is never widened (the one-way snapshot
rule above). Status transitions (`reserved → lowering → ready`) and both
refinements happen only through the specialization store's API. A record
loaded from another shard's cache is a finished snapshot and matches only at
its solved shape: a requester that matches it already has the solved type, so
no evidence needs to flow back.

The in-memory builder owns a transient hash table from lookup keys to
`SpecId`, plus the append-only `SpecRecord` array. The output program owns the
records and the function bodies, not the hash table. A loaded cache file may
build a transient hash table over the mapped records, but the file itself stores
sorted records and fixed spans so it can be consumed without pointer fixups.

Monotype type construction must feed the specialization store with immutable
interned type nodes:

```zig
const MonoTypeStore = struct {
    nodes: []const MonoTypeNode,
    args: []const TypeId,
    fields: []const Field,
    tags: []const Tag,
    payloads: []const TypeId,
    declared_fields: []const DeclaredField,
    digests: []const TypeDigest,
};

const MonoTypeNode = extern struct {
    tag: MonoTypeTag,
    first: u32,
    len: u32,
    extra: u32,
};
```

The mutable instantiation graph may use union-find, row-extension links, and
work queues while solving one specialization's interface and body relations.
Its final output is an immutable `TypeId` in `MonoTypeStore`. After that point,
the type node is never refilled. Recursive groups may reserve type ids before
their contents are available, but those slots are unavailable construction
state: digest lookup and freezing are forbidden until every reserved slot has
been filled. Filling a reserved slot completes a new immutable node; it does
not mutate any older node, so cached digests for unrelated existing types
remain valid.
Rows are normalized once, with field and tag names in explicit sorted order,
and the type digest is stored beside the node when the node is interned. Parent
digests are computed from child digests, so structurally growing records and
function types do not repeatedly walk their whole prefix.

A child digest is cached only when that child's traversal introduced no cycle
edge. The traversal tracks a monotonically increasing cycle count rather than a
boolean "saw a cycle": after one branch reaches a recursive ancestor, a second
branch that also reaches it must not be mistaken for a context-independent
subtree and cached with that ancestor-relative backreference. This keeps cached
digests stable across repeated walks of multiply connected recursive groups.

The type interner enforces exact equality:

```text
same digest
same tag
same child count and metadata
same ordered child ids, field names, tag names, and payload positions
```

The digest table is an acceleration structure only. Exact equality remains the
authority for type identity. This gives generic higher-order code the desired
shape: repeated calls at the same closed function type reuse one specialization
after the first request, and growing structural accumulator types add only the
new record/function nodes instead of redigesting every previous layer.

Open instantiation graphs do not write directly into final Monotype body
sections. While a specialization graph is active, lowering writes to a
`BodyDraft` owned by that graph. A draft mirrors the final
Monotype sections enough for lowering to refer to expressions, patterns, locals,
definitions, nested definitions, side-pool spans, and function signatures, but
all type-bearing fields use a draft type cell:

```zig
const DraftTypeCell = union(enum) {
    graph_node: InstNodeId,
    sealed: TypeId,
};
```

`graph_node` is used for any type cell owned by the active instantiation graph:
expression types, pattern types, binder/local types, typed-local entries,
function arguments, function returns, lambda and nested function signatures,
specialization request function types, layout requests, and runtime schema
requests. `sealed` is used only for closed Monotype types that were already
materialized before this graph was opened, such as imported cache entries or
builder-global primitive and hosted ABI types. If a sealed type must participate
in the current specialization's constraints, the graph imports it and the draft
stores the imported node instead of the original `TypeId`.

A `BodyDraft` may contain ordinary lowering ids, spans, and side pools while it
is active, but those ids are draft-local. They are not cache ids and no later
post-check stage consumes them. The draft is sealed only after:

1. all checked type evidence for every specialization in the group has been applied;
2. deferred procedure-template requests created by this graph have been drained
   or reserved with stable closed request types;
3. nested function bodies that share this graph have finished lowering;
4. every unresolved graph node can be closed from checked data, or can be
   proven to be a truly unconstrained empty tag union.

Sealing performs the only transition from graph nodes to final Monotype
`TypeId`s. It walks every draft type cell, materializes each graph node
through the Monotype type interner, preserves recursive groups privately inside
the interner, computes and stores type digests once, and then copies the fully
sealed records into `MonoProgramBuilder`. This copy also turns draft-local ids
and spans into final shard-local ids and spans. If sealing finds a graph node in
any completed record after this step, that is a compiler bug.

This split is required for future specialization caching. Cache files contain
only sealed `MonoProgramView` sections: fixed-width records, ids, spans, and
offsets into side pools. They never contain union-find nodes, mutable type
views, allocator-owned arrays, hash maps, or draft-local ids. Because every
interior relation in a sealed program is an id or span into the same shard, a
mapped cache file can be read back as a read-only `MonoProgramView` with only
top-level slice creation, shard assignment, and import-table resolution.

The program store is split into a builder and a read-only view:

```zig
const MonoProgramBuilder = struct {
    types: MonoTypeInterner,
    specs: SpecBuilder,
    fns: ArrayList(FnDef),
    exprs: ArrayList(Expr),
    pats: ArrayList(Pat),
    names: NameStoreBuilder,
};

const MonoProgramView = struct {
    types: MonoTypeStore,
    specs: []const SpecRecord,
    fns: []const FnDef,
    exprs: []const Expr,
    pats: []const Pat,
    names: NameStoreView,
};
```

Function slots are shard-aware so future cache files can be mapped directly:

```zig
const ShardId = enum(u32) { _ };
const FnSlot = union(enum) {
    local: FnId,
    imported: ImportedFnId,
};

const ImportedFn = extern struct {
    shard: ShardId,
    fn: FnId,
};
```

A newly built root program has one mutable local shard. A loaded specialization
file is a read-only shard. Calls inside a shard use local `FnId` values when
the target is stored in the same shard. Cross-shard calls use an `ImportedFnId`
into an imports table. Loading resolves each import table entry to
`ImportedFn { shard, fn }` once. Function bodies are not rewritten after the
file is mapped.

The durable format uses only plain old data records, offsets, lengths, and side
pools. Hash maps, union-find nodes, temporary worklists, and allocator-owned
arrays are transient builder data and are never written.

```zig
const SpecializationCacheHeader = extern struct {
    magic: [8]u8,
    format_version: u32,
    compiler_layout_hash: [32]u8,
    validity_id: [32]u8,

    names: FileSlice,
    type_nodes: FileSlice,
    type_args: FileSlice,
    fields: FileSlice,
    tags: FileSlice,
    payloads: FileSlice,
    declared_fields: FileSlice,
    type_digests: FileSlice,

    specs: FileSlice,
    fns: FileSlice,
    defs: FileSlice,
    nested_defs: FileSlice,
    exprs: FileSlice,
    pats: FileSlice,
    stmts: FileSlice,
    locals: FileSlice,
    expr_ids: FileSlice,
    pat_ids: FileSlice,
    typed_locals: FileSlice,
    stmt_ids: FileSlice,
    field_exprs: FileSlice,
    record_destructs: FileSlice,
    str_pattern_steps: FileSlice,
    branches: FileSlice,
    if_branches: FileSlice,
    string_literals: FileSlice,
    imports: FileSlice,
    roots: FileSlice,
    layout_requests: FileSlice,
    runtime_schema_requests: FileSlice,
    comptime_sites: FileSlice,
    source_files: FileSlice,
    expr_locs: FileSlice,
    expr_regions: FileSlice,
    stmt_locs: FileSlice,
    stmt_regions: FileSlice,
    local_names: FileSlice,
    debug_names: FileSlice,
};

const FileSlice = extern struct {
    offset: u64,
    len: u64,
};
```

Any current in-memory field that contains a process pointer or slice must be
converted to an offset record plus a byte or region side pool before it can be
written to these sections. This applies to string literals, source-file names,
local names, debug-name text, and compile-time site branch-region lists. A cache
file must never store process pointers from `[]const u8`, `[]const Region`, hash
maps, or allocator-owned arrays.

The loader validates the header, `format_version`, `compiler_layout_hash`,
`validity_id`, bounds, alignment, and section ordering. It then creates a
`MonoProgramView` by adding the mapped base address to each `FileSlice`. The
only required fixups are:

- converting top-level file slices to process slices;
- assigning a `ShardId` to the mapped file;
- resolving each import-table entry to a loaded shard and function id.

There are no per-expression, per-type, or per-function pointer rewrites. All
interior relations are ids or spans into the same shard.

`validity_id` for a Monotype specialization file includes:

- the format version and compiler layout hash;
- the root checked module id and all checked module ids read by the stored
  specializations;
- the explicit root request set;
- the Monotype configuration that can affect reachable specializations;
- builtin module data consumed by Monotype;
- the source callable identities and source function type digests for the
  stored specializations.

It does not include data that Monotype does not consume. In particular, it does
not include LIR layout decisions, ARC output, backend symbols, object-format
choices, or code-generation options.

Cache loading is an optimization of the same specialization store, not another
lowering path. A loaded `SpecRecord` must pass the same identity and exact type
checks as a freshly produced record before it can satisfy a request. If no
loaded record matches, the builder creates the specialization normally and may
append it to a new cache file after the program is complete.

### Static Dispatch In Monotype

Static dispatch is DECIDED during checking and CONSUMED during Monotype
lowering. Every dispatch site leaves checking with an explicit resolution on
its plan:

- `direct_closed(direct_call)`—checking proved the concrete target, projected
  its exact target-instantiated callable, proved that callable and its nested
  evidence independent of the enclosing specialization, and recorded the
  target's explicit runtime category.
- `direct_parametric(direct_call)`—checking proved the same exact target, but
  the callable or its nested evidence still consumes an explicit enclosing
  specialization identity.
- `evidence_dependent(depth, k)`—the dispatcher is the k-th evidence param of the
  d-th enclosing generalized callable. Each specialization edge supplies the
  answer: dictionary passing resolved entirely at compile time.
- `structural(derivation)`—the checker chose the compiler-derived structural
  implementation (equality, hashing, parsing, encoding, or mapping), either
  from an explicit structural registry declaration on an owned type or for an
  ownerless shape. Mapping derivations include the checker-selected tag and
  direct payload index.
- `checked_error`—checking rejected the site; executing it anyway (running a
  program with reported errors) lowers to an explicit crash.
- `unreachable`—the dispatcher is a constrained variable no
  specialization edge can ever supply and no default applies: the dispatch is
  statically unreachable and lowers to an explicit crash.

Checking records `checked_error` on the equivalence class of the static-dispatch
constraint function variable that owns the rejected edge, as the descriptor
metadata bit `Descriptor.static_dispatch_rejected`. Rejection is not encoded by
changing the constraint callable, its return, the dispatcher, or any operand to
an erroneous type: those solver variables can be shared with independently valid
producers. The marker is metadata about the class, not content, so `Store.union_`
carries it across a merge and `Store.poisonOnMismatch` preserves it when it
replaces content with `err`. A merge rejects the result if either side was
rejected: two constraint callables only unify when they are the same function
type carrying the same dispatch edge, so an edge checking rejected for one of
them is rejected for every site that shares it.
Instantiation and cross-module copies mint fresh, unrejected classes: a fresh
edge is checked on its own terms.

A raw variable index cannot carry this. Two dispatch sites record different raw
variables for what unification later proves is one constraint callable, and a
union-find root is not stable across later merges, so a raw-keyed set answers
"rejected" for whichever occurrence happened to be recorded and misses every
other member of the same class. `Check.markStaticDispatchFnRejected` sets the
class bit and appends one durable `ModuleEnv.rejected_static_dispatches` record
per newly rejected class; `Check.init` rehydrates those records onto their
classes so re-checking an env already carrying them behaves like a fresh check.
Every static-dispatch plan lookup—in the checker and in `EvidencePass`—
asks the class before resolving its receiver, and must not infer rejection by
inspecting a callable result type.

An independently reported checking error may make a plan's receiver itself
erroneous even when checking accepted that plan's dispatch (for example, a
later type mismatch can poison a monomorphic value shared with an earlier
literal conversion). That explicit receiver-error state also resolves to
`checked_error`, but it does not add a rejected-dispatch identity: the receiver
error fences the containing value, while `rejected_static_dispatches` records
only failures of the dispatch check itself. `EvidencePass` never infers either
case from the constraint callable or its return type.

`checked_error` and `unreachable` are rejected, non-returning
dispatches. Monotype lowers both to an ordinary Roc runtime crash instead of a
call, so neither can return a dispatch result value. For `checked_error`, this is
the crash observed if `roc run` continues after reporting the missing method and
execution reaches the rejected dispatch. For `unreachable`, the crash
represents the path that checking proved cannot receive a dispatcher value.
After total plan resolution, `CheckedBodyStore` computes and stores expression
and statement divergence through its exact operand and body dependencies. When
an `evidence_dependent(depth, k)` call becomes `checked_error` or
`unreachable_value` only for
one specialization, Monotype supplies those exact dispatch expression ids to
the same checked-body divergence computation before it replays type relations
or lowers the body. Callable, dispatcher, operand, and result types may be
instantiated only after this callable-or-crash gate. The crash branch uses the
contextual result cell solely to represent the non-returning expression; it
never instantiates the rejected callable's type or contributes a type relation.

Stored generated parser and encoder runtime functions are the one distinct
producer proof: ConstStore emits their explicit generated-runtime function kind
only after compile-time evaluation successfully consumed the constructor
dispatch, and intentionally stores no evidence vector for that runtime. Their
restoration may therefore consume the checked constraint plan's callable shape,
but must still reject a plan-level `checked_error` or `unreachable`.

Closed direct calls never enter dispatch-relation instantiation. Monotype
lowers an exact low-level target directly and creates no procedure
specialization. An ordinary procedure target requests one specialization under
the identity `(target template, target-instantiated checked callable,
nested-evidence identity, method scope)`. Closed checked callable and evidence
identities are interned in CheckedModule, so the first request creates or loads
the specialization and every repeated call is an O(1) hit before durable type
or evidence digests are rebuilt. Graph-participating targets consume their
producer-authored graph protocol instead of taking this sealed-interface path.

Nothing else exists. Monotype lowering never derives a method owner from type
content, never searches a registry by method name, and never intersects
constraints to guess a target.

The `.lss` strategy consumes these plans while producing Monotype IR. The
`.boxy` strategy does not enter Monotype; it consumes the same checked dispatch
plans while lowering checked CIR directly to LIR, choosing dictionary/vtable
calls or concrete structural operations from the explicit checked data.

Totality is enforced at the boundary: `validateDispatchEvidence`, run by the
checked module's `verifyComplete`, asserts that every dispatch-bearing
checked expression names a plan and that every plan and evidence reference
lands inside the checked module data's evidence tables. In debug builds—
where the boundary verifiers run—a missing or corrupt record is a compiler
bug reported at the boundary, not a lowering panic.

**Evidence params.** Every type scheme with dispatch requirements has one
deterministic ordered list of (dispatcher var, constraint) pairs—
its evidence params—enumerated purely from the scheme's type structure
(depth-first: function args then return, type arguments then backing, row
fields and tags then extension, constraints emitted at each var's first
occurrence, then constraint fn types walked the same way). Index `k` in this
list is the shared identity between a plan's `constraint(k)` resolution and
the k-th evidence entry a call edge supplies. The definition's module and any
importing module enumerate identical lists from their structural copies of the
scheme. A dispatcher's requirements are a set keyed by method identity: repeated
source constraints share one callable type and contribute one evidence param.

**Edges supply evidence.** Checking persists every constrained-scheme edge.
An ordinary instantiation records the (pristine var, fresh var) pairs of its
constrained vars. A monomorphic edge to an in-flight recursive value or method
target records the exact shared scheme root and no copy pairs. Checking
resolves each edge's requirements—against the enclosing callable's own
evidence params (producing `constraint(k)` again), against concrete types
(producing `direct` targets through exact registry lookups), through the
monomorphic default rule, or structurally—and stores the result as site
evidence keyed by the use expression. Monotype lowering materializes a
specialization's evidence vector at each call edge and passes it to the callee
specialization; a plan resolved `constraint(k)` reads entry `k` of the
innermost vector (walking lexical parents for nested local functions by
`depth`).

**The default rule.** A constrained var no edge can pin follows exactly the
rule Monotype uses to materialize unresolved variables: numeral literals and
defaultable arithmetic operators default to `Dec`, quote and interpolation
literals default to `Str`, and every requirement on such a var resolves against
the default owner during checking. Structural-capable requirements on other
unpinnable vars resolve structurally; the rest are statically unreachable.

Generalized rank is not evidence that an edge can pin a constrained var. A
body-required `where` constraint may remain unresolved only while its receiver
is in an explicit pinning frontier: reachable from a callable's exported
arguments or result, from a lambda parameter whose call is still outside the
checked body, or from an open literal whose deterministic defaulting pass will
select the owner. Result-position pinning applies to generalized schemes whose
body is evaluated per specialization; an already-evaluated, non-generalized
value cannot gain an owner from a later use. A receiver that occurs only in a
body-local result discarded directly or through local aliases is not in that
frontier and is therefore statically unreachable during checking.

An explicit zero-argument root request is called by its compilation consumer.
Its direct body is therefore not a future-call frontier: a body-required
receiver at a direct call edge there must resolve during checking. Passing a
generalized function as data—even to inspection—does not call it. A nested
closure body remains outside that frontier until an explicit checked call
invokes it. The checker carries that direct-call data through only the
value-producing child of a closure, block, conditional, or match; conditions,
guards, and preceding block statements are not call edges.

A generalized constrained function instantiation is also an explicit pinning
frontier for receivers reachable from that function's argument positions. This
rule follows the instantiated function type recorded at the use; it does not
infer reachability from generalized rank. It preserves nested generalized
helpers whose temporary scheme copy is absent from an enclosing root, while a
result-only receiver must still escape through the enclosing scheme interface.
This frontier applies only when every receiver constraint is a copied `where`
contract; any concrete-use constraint on the same receiver requires resolution
at the current call.

**Compiler-generated edges.** Structural derivations and builtin helpers call
methods on component types with no checked instantiation record. For these,
each checked evidence param also carries the label-addressed PATH from its scheme's
callable to the dispatcher's first occurrence (argument positions, type
arguments, row labels—labels rather than positions, because Monotype sorts
rows). Monotype resolves such a target's requirements by walking those paths
over the concrete monomorphic callable at the consumption site, recursively:
component owners consume the registry's explicit callable-or-structural result;
ownerless shapes take the structural implementations.

Evidence paths describe the normalized logical type, never checked-store row
topology. Record and tag extension chains, including transparent aliases along
those chains, are traversed by the evidence-param producer but are not emitted
as path steps; a field or tag payload in any tail is addressed directly by its
label from the logical row root. A static-dispatch requirement on an open-row
remainder is pathless because closure erases that remainder as a standalone
component; ordinary checked edges can still supply it, while a
compiler-generated edge cannot synthesize it from the closed callable. The
CheckedModule boundary validates both the path grammar and that every nonempty
path resolves over its template's checked callable type before Monotype may
consume it.

Exact registry lookups—`(MethodOwner, MethodNameId)`—happen during
checking, and during path synthesis for compiler-generated
edges. The registry only ever answers exact lookups after the owner is known
from checked type content; no stage asks "which owners could match this
constraint?".

### Iterator `for`

Source `for` loops lower during Monotype construction. The output is ordinary
loop, match, and call structure with explicit loop-carried state.

Given:

```roc
for pattern in iterable {
    body
}
```

Monotype lowering emits the source-level meaning of:

```text
iterator = iterable.iter()
loop iterator:
    step = iterator.next()
    when step is
        Done -> break
        One item next_iterator ->
            match item with
                pattern -> body; continue next_iterator
        Skip { rest } ->
            continue rest
```

Monotype `loop_` carries named parameters and `continue_` supplies the next
values for those parameters. Iterator `for` uses one loop-carried parameter: the
current iterator value. There is no hidden assignment and no mutation-only loop
state.

The exact step tag names and payloads come from the checked/builtin `Iter`
definition and the monomorphic iterator type. The `.iter` and `.next` calls are
resolved through the same Monotype static-dispatch path described above.

A `Skip` carries only `rest`: it signals "advanced one position, produced no
item this step," which is what keeps adapters like `keep_if` non-recursive. A
plain source `for` loop binds nothing from it and simply continues with `rest`.

No `for` node exists after Monotype IR.

## Monotype Lifted IR

Monotype Lifted IR is `.lss`-only. It removes closures and local functions from
expression position. Its type store is the Monotype type store.

The expression language is intentionally close to Monotype IR, and the
implementation consumes Monotype expression storage in place. Expression,
pattern, statement, and side-array ids are preserved across the Monotype to
Monotype Lifted boundary. Patterns and statements are the same storage. Most
expressions are the same storage. Lifting rewrites only the expression variants
whose callable meaning changes:

- `lambda`, `def_ref`, and `fn_def` become `fn_ref`
- a direct-call callee changes from a Monotype function template to a lifted
  function id

This is a representation-sharing rule, not a license for later stages to accept
pre-lift callable forms. After lifting, a valid lifted program has no reachable
`lambda`, `def_ref`, `fn_def`, or template-callee `call_proc` expression. Those
variants may still exist in the shared Zig union because Monotype and Monotype
Lifted use one backing expression representation, but seeing one through the
Monotype Lifted API is a compiler bug.

The lifted stage output adds only the data that lifting owns:

- every function body is a top-level lifted definition
- each lifted function definition declares its capture symbols explicitly
- roots and layout requests refer to lifted function ids
- capture spans appended by lifting are stored in the shared typed-local side
  array

Optional lifted optimization may append typed `join_point` and `jump`
expressions to that shared storage. A join point has an id, typed parameters, a
body that consumes those parameters, and a remainder that enters the shared
control region. The id is in scope in both body and remainder; the parameters
are in scope only in the body. A jump names an in-scope id and supplies exactly
one argument of the declared type for every parameter. The join body and
remainder both produce the enclosing expression's result type. These forms
express shared control only; they do not choose layouts or ownership.

Case-of-case and let-of-case rewrites use these forms to thread branch results
to one continuation. They never clone that continuation into each branch. Join
ids are fresh within the lifted program, and cloning a region freshens both the
id and its parameter locals while preserving lexical scope.

```zig
const LiftedDef = struct {
    symbol: Symbol,
    kind: LiftedDefKind,
};

const LiftedDefKind = union(enum) {
    fn: LiftedFn,
    value: ExprId,
    run: ExprId,
};

const LiftedFn = struct {
    args: Span(TypedSymbol),
    captures: Span(TypedSymbol),
    body: ExprId,
};
```

Function references remain ordinary values with function type. A function value
is not packed here. Captures are explicit metadata on the lifted function
definition; callable flow is solved in Lambda Solved and runtime representation
is committed by direct LIR lowering.

The lifting pass owns free-variable analysis. It does not choose finite
callable representations, erased callable representations, closure object
layouts, or runtime tags.

Release builds must not allocate or fill a second expression, pattern,
statement, branch, field-expression, or span arena for Monotype Lifted. The
normal path may allocate lifted function metadata, capture spans, request
rewrites, and traversal scratch owned by lifting. Debug builds may materialize
the old copied lifted tree only as a verifier; the in-place lifted program
remains the source consumed by Lambda Solved and later stages.

## Lambda Solved IR

Lambda Solved IR is `.lss`-only. It introduces lambda sets into the
stage-local type store and solves callable flow. `.boxy` runtime lowering never
constructs Lambda Solved IR and therefore never encounters a lambda set.

This is where Roc intentionally follows Cor's data model for callable values:
callable representation information is type information in this post-check IR,
not a side representation store.

### Lambda Solved Types

```zig
const LambdaType = union(enum) {
    link: TypeVarId,
    unbound,
    forall,
    content: LambdaContent,
};

const LambdaContent = union(enum) {
    primitive: Primitive,
    record: Span(Field),
    tag_union: Span(Tag),
    tuple: Span(TypeVarId),
    list: TypeVarId,
    box: TypeVarId,
    named: NamedType,
    func: Fn,
    lambda_set: LambdaSet,
    erased: TypeDigest,
};

const Fn = struct {
    args: Span(TypeVarId),
    callable: TypeVarId,
    ret: TypeVarId,
};

const LambdaSet = struct {
    members: Span(LambdaSetMember),
};

const LambdaSetMember = struct {
    lambda: Symbol,
    captures: Span(Capture),
};

const Capture = struct {
    symbol: Symbol,
    ty: TypeVarId,
};
```

A function type has an explicit callable slot:

```text
args -- callable -- ret
```

The callable slot is either a finite lambda set or `erased`.

Finite lambda sets name the exact function symbols that may flow through a
function value and the exact captured values each symbol needs. Erased callable
types represent call sites that must use the erased callable ABI.

Function arity remains fixed. A multi-argument Roc function is represented by
one `func` node with all arguments in `args`. It is not represented as nested
unary functions unless the source type explicitly returns another function.
Lambda-set solving, erased callable ABI solving, and specialization identity all
use the full ordered argument list plus the result type.

Monotype records whether a function signature's argument and result positions
are independent roots or one exact producer-authored graph. The generated
`Iter.fromStep` boundary uses the exact-graph relation because its result
iterator intentionally retains the step function argument's runtime callable
representation; ordinary function signatures use independent roots. Monotype
Lifted retains an exact graph only while that ABI is unchanged. Lambda Solved
then imports it from a single signature root, preserving recursive edges and
intentional sharing between an argument and a nested result slot, and relates
the lifted argument locals and callable member to those exact slots. A
transformation that synthesizes a different function ABI clears the producer
signature and provides its new argument and result slots explicitly. Consumers
never infer signature relationships by comparing `TypeId`s from independently
imported roots: equal type ids describe equal Monotype shapes, not runtime
value flow.

### Lambda Solving

Lambda Solved IR keeps the Monotype Lifted expression storage and adds solved
type arrays beside it. Only the type store changes.

The solver:

- instantiates Monotype Lifted types into Lambda Solved type variables
- adds a fresh callable slot to every function type
- treats references to lifted function symbols as singleton lambda sets
- unifies callable slots through value flow and calls
- propagates erased callable requirements through the same type graph
- generalizes and instantiates polymorphic definitions
- solves recursive groups as groups, not by accidental declaration order
- verifies each lifted jump is lexically scoped and unifies its arguments with
  the corresponding join-point parameter types

Monotype may carry both the definition-private nominal view and the opaque
interface view of one checked `TypeDef`. Lambda solving relates those views only
when their complete definition identities and builtin owners agree. It unifies
their type arguments and checked-public runtime backings for callable flow, but
keeps the opaque view as the representative; the relation therefore cannot
grant structural inspectability. Different definitions, aliases, missing
representation authority, and generated-private backings are never accepted by
this visibility relation.

The solved type graph is the callable representation source of truth. There is
no descriptor replacement, no callable repointing, no post-demand payload
output, and no representation recovery later.

### Erased Callable Requirements

In `.lss`, `erased` callable requirements are explicit data entering Lambda
Solved IR. They are not inferred from backend needs or recovered from runtime
encodings.

The producers are:

- checked platform and hosted function declarations whose ABI requires erased
  function values
- exposed values whose public ABI requires erased callables
- checked builtin operations that are explicitly declared as erased-callable
  boundaries in `Builtin.roc` and the builtin method registry
- checked low-level operations whose signature explicitly contains an erased
  callable parameter or result
- checked root ABI metadata for values that will later be consumed by LirImage
  or glue, when that metadata explicitly names erased callable slots
- a Monotype iterator type explicitly marked `forced_dynamic`, whose recursive
  step callable crosses the dynamic representation boundary

Monotype lowering carries boundary requirements as typed annotations into
Lambda Solved IR. Lambda solving also consumes the explicit iterator
representation tier and marks a completed forced-dynamic backing callable as
erased. It unifies all requirements through the same function
`args/callable/ret` graph used for finite lambda sets. If a callable slot is
forced to `erased`, direct LIR lowering produces packed erased callable values
and indirect erased calls. If no explicit erased requirement reaches a callable
slot, finite lambda-set dispatch is used.

No ordinary source expression becomes erased because direct lowering finds
finite dispatch inconvenient. Erasure is introduced only by explicit checked
boundary data or the explicit Monotype forced-dynamic iterator tier.

This `.lss` rule is separate from `.boxy` closure representation. `.boxy` uses
boxed erased callables for function values by strategy, but it still does not
change host ABI requirements or infer host-facing erased slots from backend
convenience. A `.boxy` erased callable is an internal function-value
representation unless the checked host ABI already requires `Box(function)`.

## Lambda Mono Decisions

Lambda Mono is `.lss`-only. It consumes Lambda Solved IR and chooses
function-free callable, procedure, capture, and type representation data. These
decisions are explicit stage output, but release builds do not store a full
Lambda Mono expression, pattern, or statement tree. The direct `.lss` LIR
builder consumes the Lambda Solved lifted syntax together with Lambda Mono
decision tables. `.boxy` does not construct Lambda Mono decisions.

The Lambda Mono type store has no function type. Function values have already
become ordinary value representations:

- finite callable sets become generated tag-union values
- erased callables become packed erased callable values

```zig
const LambdaMonoType = union(enum) {
    primitive: Primitive,
    record: Span(Field),
    capture_record: Span(CaptureField),
    tag_union: Span(Tag),
    tuple: Span(TypeId),
    list: TypeId,
    box: TypeId,
    named: NamedType,
    callable: Span(FnVariant),
    erased_fn: ErasedFn,
};
```

A finite callable set is an ordinary generated tag union. Each lambda-set member
gets one generated tag. If the member captures values, the tag payload is a
generated record containing those captures. If it captures nothing, the tag is a
zero-payload variant.

The generated callable type carries the source member and the exact Lambda Mono
function target. The target is part of the type node. The LIR builder never
finds a function by scanning symbols or by rebuilding a specialization choice:

```zig
const FnVariant = struct {
    id: FnVariantId,
    source: Symbol,
    target: FnId,
    capture_record: ?TypeId,
};
```

`source` is the original lifted function symbol and is used only while lowering
a `fn_ref` expression into the correct callable variant. `target` is the exact
Lambda Mono function specialization to call for that variant. `capture_record`
is the exact payload type for finite callable values and the exact capture
argument type for erased callable entries.

When Lambda Mono lowers a function reference, it reads the capture span from the
Lambda Solved function value type at that expression site. It then builds a
capture record with those exact slots and stores it in the callable value. It
does not use the source function's own function type as a proxy for the
expression-site callable type.

The release-build direct lowerer owns only the decision data needed to produce
LIR:

- the function-free Lambda Mono type store
- queued function specializations keyed by exact lifted function id, solved
  function type, capture ABI, and capture shape
- callable variants, erased callable entries, capture-record types, and exact
  function targets
- root, layout, runtime-schema, and const-plan requests rewritten to Lambda Mono
  type and function ids
- per-function capture bindings used by direct LIR lowering

The output does not contain copies of lifted expressions, patterns, statements,
branches, field-expression spans, tuple spans, loop spans, or source statement
spans. When a lifted node is unchanged by Lambda Mono, direct LIR lowering reads
the original lifted node. When Lambda Mono changes behavior, direct LIR lowering
uses the explicit Lambda Mono decision associated with that expression, call,
function reference, captured local, or callable pattern.

This keeps the logical Lambda Mono contract explicit without making the
production pipeline pay for a second syntax arena that mostly duplicates
Monotype Lifted IR.

### Logical Lambda Mono Expressions

```zig
const ExprData = union(enum) {
    var_: Symbol,
    unit,
    int_lit: IntLiteral,
    frac_lit: FracLiteral,
    dec_lit: DecLiteral,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    record_update: struct { base: ExprId, fields: Span(FieldExpr) },
    tag: TagExpr,
    nominal: ExprId,

    let_: struct { bind: TypedSymbol, body: ExprId, rest: ExprId },
    direct_call: DirectCall,
    indirect_erased_call: IndirectErasedCall,
    packed_erased_fn: PackedErasedFn,
    low_level: LowLevelCall,

    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
    join_point: JoinPointExpr,
    jump: JumpExpr,
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};
```

The expression forms above define the logical Lambda Mono language. They do not
require release builds to store a contiguous `ExprData` array. The direct LIR
builder may synthesize one logical expression at a time while it lowers a
Lambda Solved lifted node. If it creates helper expressions for capture records,
callable payload patterns, or finite-call branch bodies, those helpers are
builder-local work data and must not become a stage boundary consumed by later
compiler stages.

Lambda Mono uses the same loop-carried `LoopExpr` and `ContinueExpr` shape as
Monotype. A pass that preserves loops must preserve explicit parameters,
initial values, and continue values. LIR lowering turns loop state into LIR
joins and jumps. Lifted join points are already explicit control edges, so LIR
lowering maps them directly to LIR `join`/`jump` with the same parameter order
and types rather than rebuilding or copying their continuation.

Logical Lambda Mono has no `call_value` node. A call through a finite lambda set is
lowered to a match over the generated callable tag union; each branch makes a
`direct_call` to the variant's `target`. A call through an erased callable
becomes `indirect_erased_call`.

Generated callable variants are stage-local ids created by Lambda Mono. The
runtime discriminant and variant slot are chosen later by LIR layout commitment
and then output explicitly in the LIR result.

Lambda Mono specialization is queued by exact lifted function id, solved
function type, callable ABI, and capture shape. The queue is driven only by
explicit callable flow in Lambda Solved IR. Each `FnVariant.target` names the
queued result directly, so later stages consume a direct function id instead of
looking up a symbol.

For a finite callable member with captures, the specialized function receives
the original Roc arguments followed by one compiler-created capture-record
argument. For an erased callable, the erased ABI contains the full ordered Roc
argument list and result layout. Neither path introduces currying or
partial-application wrappers.

Logical Lambda Mono has no generic conversion expression. Any operation that must
survive to statement lowering is represented by a concrete expression form
above. Differences that are only layout choices are handled by layout
selection while lowering those concrete expressions.

If the direct LIR builder sees that Lambda Mono decisions and the committed layouts
require incompatible representations for the same value, compilation has found a
compiler bug. The builder stops with an invariant failure. It must not invent a
conversion path, conversion table, wrapper, or reshaping step to continue.

Recursive boxing, list backing representation, transparent named-type
backing, and zero-sized representations are layout-lowering responsibilities
for ordinary concrete constructors, field reads, tag payload reads, calls, and
patterns. They are not modeled as conversion nodes. If the language exposes a
real runtime operation such as allocating or reading an explicit `Box`, that
operation must enter Lambda Mono as a named concrete expression or low-level
operation whose meaning is defined by its producer stage, not as an
after-the-result conversion.

## Direct LIR Lowering

LIR lowering has two production frontends selected by `SpecializationStrategy`.
Both frontends produce the same ownership-neutral LIR contract consumed by ARC:

- `.lss` consumes Lambda Solved lifted syntax plus Lambda Mono decision tables.
- `.boxy` consumes checked CIR, checked types, checked roots, checked dispatch
  plans, checked method registries, and checked platform/hosted metadata.

There is no strategy fallback. The selected frontend either emits complete LIR
from its explicit inputs or stops on a compiler invariant failure. A frontend
must not call the other strategy to fill missing data.

There is no separate stored layout IR. In `.lss`, the Lambda Mono to LIR
builder owns:

- a layout builder that interns and commits recursive layouts from
  Lambda Mono type nodes
- a procedure builder that maps Lambda Mono procedure ids to LIR procedure ids
- a local builder that allocates LIR locals from lifted binders plus Lambda Mono
  local, capture, and type decisions
- a pattern builder that consumes lifted patterns plus Lambda Mono callable
  pattern decisions and committed layouts, then emits LIR switches, joins, and
  bindings directly
- callable lowering that turns generated callable tag unions into ordinary LIR
  tag operations and erased callable values into explicit packed-erased-callable
  statements
- bool predicate creation from ordinary Bool tag-union layouts
- runtime value schema output from committed nominal layouts
- erased callable code map output from Lambda Mono callable/procedure data

These are builder responsibilities, not a separate meaning-carrying IR.

The `.lss` builder may maintain temporary maps such as `TypeId -> layout.Idx`,
`LambdaMonoFnId -> LirProcSpecId`, `LiftedLocalId -> LirLocalId`, and
`LiftedExprId -> lowered logical expression` while lowering one function
specialization. These maps are caches of work the builder owns. They must not
contain checked data that are absent from Lambda Solved IR, the explicit inline
plan, the builder's logical Lambda Mono decisions, or the LIR result.

The `.boxy` builder owns corresponding temporary maps such as
`CheckedTypeId -> BoxyTypeDescId`, `CheckedDispatchPlanId -> BoxyDictId`,
`CheckedTypeId -> BoxyRuntimeLayout`, `CheckedRootOrder -> LirProcSpecId`, and
`CheckedExprId -> LirLocalId` while lowering one root/worker. A
`BoxyRuntimeLayout` pairs the committed storage `layout.Idx` with any dynamic
descriptor requirement for descriptor-governed Roc box pointers. These maps are
caches of explicit checked and boxy lowering data. They must not recover missing
data from source syntax, type display strings, backend symbols, or runtime
bytes.

Boxy representation planning consumes checked nominal declared-order entries
directly. For usage payloads whose finalized representation points at a local or
imported box-payload capability, the planner obtains the instantiated backing
root and padding roots from that capability rather than treating empty
`padding_field_types` on the usage payload as absence of padding. Imported
capability roots are source-module checked ids; the planner maps them into the
root checked type store by their exported checked type digests. Named
declared fields from imported declarations are matched against the mapped
root backing row through checked field-label text, not by assuming equal
module-local label ids. The planner lowers each named entry to the matching
backing-row child and each padding entry to the instantiated padding type
referenced by its ordinal. The boxy layout planner then emits a nominal struct
node in that declared order and marks it with the shared layout graph's
nominal-struct marker. The ordinary layout store verifies or repairs the field
order; boxy does not implement a separate nominal layout algorithm.

Boxy tag-union planning stores tag variants explicitly, separately from payload
children. The representation's child span still contains payload children in
payload position order plus the row extension child, but the representation also
carries a tag-variant span. Each tag variant records the checked tag label and a
payload span into the child table. Zero-payload variants therefore have an
ordinary variant row with an empty payload span. Layout planning, tag payload
selection, discriminant assignment, and static-data `ConstPlan` construction
consume the variant table directly instead of trying to rediscover variants from
runs of payload children.

Release `.lss` builds must not allocate, fill, traverse, or validate a materialized
Lambda Mono expression, pattern, or statement tree. Release builds may allocate
only the Lambda Mono decision data needed by direct LIR lowering: function-free types,
function specializations, callable variants, capture records, root/layout/schema
requests, and builder-local scratch storage.

Release `.boxy` builds must not allocate Monotype, Monotype Lifted, Lambda
Solved, Lambda Mono, lambda-set, or finite-callable-tag-union syntax as a
compatibility representation. They may allocate only boxy descriptor,
dictionary, adapter, worker, layout, and builder-local scratch data needed to
emit LIR.

### Boxy Checked-To-LIR Lowering

The boxy lowerer emits private worker procs whose explicit arguments are the Roc
source arguments after boxy representation, followed by hidden `TypeDesc` and
dictionary arguments required by the checked type and dispatch plans. Hidden
arguments are ordinary LIR locals in private procs. They are not part of root
metadata and are never exposed to the host ABI.

Before emitting worker bodies, the boxy lowerer resolves every worker plan to a
checked procedure source. A worker source is one of the explicit authorities
recorded in checking: a checked procedure template, a top-level procedure
binding, or a procedure-use template. Resolution follows those references by
CheckedModule digest and table id. Direct checked templates record their checked body.
Callable-eval bindings first resolve the finalized compile-time root payload;
the runtime worker is the stored `ConstStore` function's checked template or
nested lambda expression, not the compile-time entry-wrapper evaluator. The
lowerer-local resolved-worker table therefore records the checked module view,
owning template identity, optional checked body id, and explicit root expression
id that worker emission must lower. Lifted, synthetic, intrinsic, pending
callable-eval roots, and generated runtime functions are not compatibility
fallbacks for this path.

Stored-function capture initialization precedes ordinary body execution but is
part of the ownership-neutral worker LIR seen by ARC. Static descriptors needed
to restore capture constants are materialized first; capture constants are then
restored into their bound frame locals; erased-callable argument captures and
the checked body follow. ARC therefore owns capture lifetimes exactly like
ordinary checked declaration locals. The lowerer does not append backend-only
cleanup or hide capture allocations from ARC.

The worker body builder consumes checked expression and pattern ids directly and
emits statement LIR. Lambda worker arguments become LIR proc arguments, binder
patterns map to those argument locals, and expression lowering writes into an
explicit target local before continuing to the next statement. Literal workers
use the ordinary `assign_literal` statements with layouts selected from the
boxy layout plan; zst values use ordinary empty-struct assignment.

Direct-call descriptor setup has one explicit execution order: evaluate each
source operand into its value and descriptor locals, materialize descriptor
prerequisites selected from those live source values, adapt the arguments to
the worker representations, materialize hidden descriptor and dictionary
arguments, then call the worker. A hidden descriptor with a planned source
argument index consumes that exact value's descriptor. When it names a nested
representation, lowering follows the planned representation path through that
value's descriptor before consulting descriptor-environment bindings; an
environment may contain bindings for other live values with the same generic
representation and is not evidence that those values are interchangeable.

An applied-tag worker argument pattern is irrefutable only when its planned
checked representation contains exactly one tag variant with that checked tag
identity. Lowering validates that data, reserves the payload binders, and uses
the explicit variant payload plan. It never assumes a tag pattern is
irrefutable merely because its observed discriminant is zero.

Checked string segment literals lower to ordinary LIR string-view literals. The
lowerer copies the checked literal bytes into the LIR string store and emits
`assign_literal.str_literal` with a view over exactly those bytes. A
`str_from_quote` expression whose checked target is builtin `Str` follows the
same path. A `str_from_quote` expression with a static-dispatch conversion plan
is not a string literal assignment; it lowers through the checked dispatch plan
for that conversion.

Checked bytes literals follow the same byte-copying LIR literal path as string
segments: the literal bytes are copied into the LIR string store and referenced
by `assign_literal.str_literal`. The checked type remains `List(U8)` and layout
selection comes from the checked type's boxy representation; the lowerer does
not synthesize a list item-by-item from the bytes.

A generalized numeral literal whose checked conversion is a runtime operation
retains its exact checked numeral in the Boxy plan. If its target is
descriptor-governed, lowering emits the descriptor-guided dynamic integer or
fractional literal operation and materializes the exact target descriptor. If
the target is a dynamic box with a fixed payload layout, lowering first emits
the exact scalar payload and then boxes it with that descriptor. It must not
evaluate the conversion with CTFE, choose a machine scalar from the contextual
layout alone, or reconstruct the numeral from formatted text.

Checked builtin string interpolation that has already been represented as a
checked `str` segment list lowers by evaluating each segment expression in
source order and emitting explicit `assign_low_level.str_concat` statements in
left-associative order. Segment locals use layouts selected from their checked
types; if a segment requires non-string adaptation, that is handled by the
checked dispatch/interpolation path rather than guessed during concat lowering.

Checked block lowering is continuation-based. Before the final expression is
lowered, the body builder allocates and binds every checked declaration local
from the block's explicit checked statement list, using the checked pattern type
and the representation table to select the worker layout. It then lowers the
final expression and walks statements backward, emitting the initialization
statements in front of the continuation. This lets `lookup_local` consume the
same checked binder ids that checking produced; the lowerer never reconstructs
lexical scope from source syntax or declaration names.

Mutable checked block statements use the same checked binder metadata, with
explicit LIR writes for the places where mutability matters. `var` declarations
reserve locals from their checked patterns and initialize them through the
ordinary declaration binding path. `var` declarations without an initializer
emit `init_uninitialized` for each non-zst binder local named by the checked
pattern. Reassignment statements consume the checked `reassigned_binders` list:
binders in that list lower to `set_local.replace_existing`, while fresh binders
inside the same destructuring pattern are initialized with ordinary local
binding. The lowerer does not infer reassignment by comparing names or source
spans; the checked statement tells it exactly which binders are mutable writes.

Checked `crash` expressions lower to terminal LIR `crash` statements carrying
the checked string literal bytes copied into the LIR string store. The target
local for the surrounding expression is not initialized on that path because the
path does not continue.

Checked `runtime_error` expressions and statements lower directly to terminal
LIR `runtime_error`. This form is already a CheckedModule marker for an
impossible or intentionally error-recovered path; boxy lowering does not invent
a message or reinterpret it as a user crash.

Checked `return` expressions and statements lower to terminal LIR `ret`
statements. The lowerer validates that the checked return target lambda is the
current boxy worker lambda, lowers the returned expression into a fresh local
with the worker return layout, and emits `ret` for that local. It does not jump
to the surrounding expression continuation, and it does not infer the target
function from lexical names or source position.

Checked `dbg` and `expect` expressions lower to explicit side statements and a
unit result. `dbg` lowers its child into a message local, emits LIR `debug`, and
then assigns the target zst unit value. `expect` lowers its Bool condition into
a condition local, emits LIR `expect`, and then assigns the target zst unit
value. When these forms occur as checked statements instead of expressions, the
same child-lowering and side-statement rules apply without the final unit
assignment.

Checked `if` expressions lower as structured branch-result control flow. The
lowerer allocates one join id for the expression's shared continuation and uses
the expression target local as the join parameter. The final `else` expression
and every branch body lower into that same target local and then jump to the
join. Each branch condition lowers into a Bool local selected from the checked
condition type's boxy representation, and the branch test is an ordinary LIR
switch on the Bool runtime discriminant (`True` is value 1 by the checked Bool
builtin rule). The lowerer consumes the checked if-branch list directly; it does
not recover branch order or condition/body relationships from source syntax.

Checked `and` and `or` binop nodes lower as short-circuit Bool switches. `and`
lowers the left operand into a condition local, evaluates the right operand only
on the true branch, and assigns `False` directly on the default branch. `or`
evaluates the right operand only on the false/default branch and assigns `True`
directly on the true branch. Other checked binop source operators are not
lowered by inspecting the surface operator here: checking rewrites equality to
structural equality or method equality and rewrites arithmetic/comparison
operators to checked dispatch calls, and boxy consumes those explicit checked
plans.

Irrefutable declaration patterns lower as value binding plus explicit
destructuring reads. Assignment patterns bind the RHS value directly. Tuple
patterns read fields by checked tuple index. Record destructuring patterns read
fields by translating the checked field label through the representation plan's
ordered record roles. `as` patterns bind the whole source value and then bind
their inner pattern from that same source. Transparent nominal patterns bind
through the same explicit nominal boundary used by nominal expressions. Refutable
declaration patterns, list rest construction, tag payload tests, literal tests,
and string-pattern tests are lowered through the pattern-decision path rather
than being treated as plain binding.

Aggregate expression lowering allocates temporary LIR locals from each checked
item or field type through the representation table, lowers children in
source evaluation order, then emits the aggregate-building statement with a
local span ordered by the committed boxy layout. Tuple construction is a direct
`assign_struct` over item locals. Record construction uses the same rule with
field-order data from the representation plan.

Nominal construction lowers the checked backing expression into a temporary local
whose layout is selected from the backing checked type. If the backing and
nominal worker layouts are representation-equivalent, construction is pure value
flow and lowers to the same LIR local-alias operation used for ordinary local
copies. If the layouts differ, construction lowers to an explicit
`assign_ref.nominal` from the backing temporary into the nominal target. The
lowerer does not inspect backend layouts or recover nominal backing structure
from names; any tuple, record, or tag field ordering is handled by lowering the
checked backing expression through its own representation plan before the
nominal boundary is emitted.

Aggregate access lowering first lowers the receiver into a temporary whose
layout comes from the receiver's checked type representation, then emits an
`assign_ref` field read. Tuple access uses the checked tuple item index.
Record field access translates the checked field label to a numeric field index
through the representation plan's ordered field roles; it never derives field
indexes from source text, expression field order, or backend layout inspection.

Tag construction consumes the representation plan's tag-variant table. The
checked tag label selects the variant index and discriminant, and the variant's
payload span defines the expected payload arity and argument order. A nullary
tag emits an `assign_tag` with no payload unless the committed union layout is
zero-sized, in which case it emits the ordinary zst assignment. A payload tag
first lowers the payload expressions in source order into either the single
payload local or a payload struct local whose layout comes from the committed
tag-union layout, then emits `assign_tag`. Builtin `Bool` uses the explicit
builtin order `False = 0`, `True = 1`; that mapping is a checked builtin rule,
not a backend layout query.

List construction uses the committed list layout to select the item storage
layout. Each checked item expression lowers into an item local in source
order, and the list literal emits one `assign_list` with an item span in that
same order. Empty list literals emit `assign_list` with an empty item span at
the target list layout. If a checked item's worker layout differs from the
committed item storage layout, the boxy lowerer emits the explicit box/adapt
statement required by that boundary; it never inserts an implicit conversion or
asks the backend to reinterpret list item bytes.

Checked `run_low_level` expressions lower directly from checked CIR. The boxy
body builder allocates one temporary local per checked argument using the
argument expression's checked type and boxy representation plan, lowers those
arguments in source evaluation order, and then emits the primitive operation.
Ordinary primitive operations become `assign_low_level` with the op's explicit
`RcEffect`; ARC and later consumers read that metadata exactly as they do for
`.lss` output. Primitive operations whose raw machine operation is not the
complete Roc behavior are expanded before the raw op is emitted: integer
division and remainder emit explicit zero-denominator checks, signed integer
division emits the lowest-value / negative-one check, signed `negate` and `abs`
emit the lowest-value check, and checked integer multiplication emits the same
overflow proof sequence used by `.lss`. These checks are ordinary LIR
comparisons, switches, literals, and crashes; no backend may rediscover or omit
them. Explicit `Box` boundary low-levels are not treated as ordinary primitive
calls in `.boxy`; they lower through the boxy box/unbox/adapt statements so a
value that already has the host-compatible `Box(...)` representation is reused
instead of double-boxed. `list_map_can_reuse` also carries its compile-time
layout decision at LIR lowering: the lowerer computes the per-pointer-width
interchangeability bits from committed item layouts and emits either a
constant false value or an `assign_low_level` with explicit
`interchangeable` metadata.

Typed numeric `*_from_str` operations have a closed concrete result ABI. The
lowerer consumes the operation's `NumericParseSpec`, emits `assign_low_level`
into a two-variant tag union whose `Err` payload is zero-sized and whose `Ok`
payload is the specified integer, float, or decimal layout, and attaches a
descriptor derived from the exact checked result type. When the worker result
uses erased payload storage, a subsequent `assign_boxy_adapt` carries that
concrete value and descriptor into the planned result representation. The
interpreter and code-generation backends therefore receive the concrete builtin
ABI directly; they do not select numeric payload layouts or descriptor data.

Checked unary operator nodes use that same primitive path when they remain in a
checked body. Unary `-` lowers as `num_negate`, so signed-integer
lowest-value protection is emitted by the low-level expander before the raw
operation. Unary `!` lowers as `bool_not` over the checked Bool representation.
If checking resolved the source operator to a static-dispatch call instead, the
boxy lowerer consumes the checked dispatch plan for that call rather than
reinterpreting the original source operator.

### Boxy Call-Site Substitution And Boundaries

Every Boxy call boundary has an explicit planner-owned substitution. The
substitution maps each type position in the callee worker representation to its
checked call-site instantiation. Direct calls, erased calls, dictionary calls,
iterator calls, constant-evaluation calls, and host wrappers all use the same
mapping model. The producer derives it from checked callable types and checked
dispatch evidence. It does not inspect the expression variant to decide whether
the expression type or parameter type is authoritative.

Alias and nominal wrappers make substitution ordering explicit. Before the
planner descends an alias backing, it records each checked `alias_arg` pair from
the worker and call representations. A nominal instead records each declaration
backing formal together with the exact `actual_rep` for that nominal use. Its
visible `nominal_arg` child describes checked shape and may remain generalized;
it is not evidence for the backing's concrete representation. Direct-call,
erased-callable, and generated-callable descriptor and dictionary planning use
the exact backing substitution. Every occurrence of that worker argument in the
backing graph then uses the recorded call representation, independent of child
traversal order. An identity observation cannot replace an already-recorded
concrete instantiation, and two different concrete instantiations for one worker
representation are an invariant failure. The descriptor source still names the
original call operand root plus the exact instantiated descendant; it never
changes to a sibling value merely because the substitution was learned from the
wrapper's explicit argument metadata.

The substitution is consumed to produce one exact source for every hidden
descriptor, hidden dictionary, and erased-callable metadata capture. A source is
one of static metadata, an argument descriptor, a nested descriptor read from an
argument descriptor, a result descriptor, or another explicitly named planned
value. The lowerer does not recursively compare worker and call representation
trees, match children by source type or display name, or search row extensions
to reconstruct these sources.

Every non-identity representation boundary also has a planned adapter request.
After layouts are committed, the adapter builder resolves each request to an
interned `BoxyAdapter`. The adapter records source and target layouts, source and
target descriptor roles, transfer mode, concrete byte segments, checked field
and tag mappings, and any nested adapters. The interned adapter never contains a
procedure-local id. Each `assign_boxy_adapt` names the concrete source and target
descriptor refs for that invocation; those refs are ordinary statement operands
resolved in the current frame before a machine-code backend enters the shared
runtime. Identity is established by the plan from checked identity and committed
representation data; equal pointer width or equal aggregate byte size is not an
identity proof.

Descriptor specialization for an adapter is a graph construction. Recursive
records, tags, boxes, and lists can revisit the same target representation,
source representation, and source descriptor identity before the enclosing
target descriptor is complete. The builder reserves a target descriptor id at
that revisit, emits the recursive child reference to the reservation, and fills
the reservation when the enclosing descriptor is complete. It does not unroll
the representation graph, impose a recursion-depth limit, or replace the child
with a less precise descriptor.

Reserve-then-fill metadata construction must also respect growable-table
storage. Recursive descriptor or inspect-adapter emission may append to the same
`ArrayList` that owns a reserved placeholder. The producer first constructs the
entire descriptor or method-slot struct in a local variable, then reacquires the
reserved item by id and writes that struct. It never retains an
item pointer, slice, or evaluated assignment address across recursive work
that can reallocate the table.

Lowering consumes the planned boundary. It may emit a primitive box, unbox, tag,
or local-flow statement when the adapter is exactly that primitive operation.
All other boundaries emit `assign_boxy_adapt`. Lowering does not select a
conversion by trying a sequence of record, list, tag, nominal, and callable
shape matchers. Container loops, callable adapter bodies, dictionary method
adapters, and host wrappers all consume the same interned plans.

Runtime descriptor values are immutable identities. `assign_boxy_desc_ref`
initializes a fresh local with one descriptor value. A descriptor local is not
rebound while any value refers to it; materializing a different descriptor uses
a different local. Consequently the descriptor attached to a value is stable
for the value's entire LIR lifetime, and ARC never scans for descriptor updates
or releases values in anticipation of descriptor rebinding.

Runtime-created descriptors use storage whose lifetime is the complete Boxy
runtime, independently from operation-local value or inspect scratch. This
remains true under re-entrant runtime calls: a custom inspect worker can invoke
an adapter while inspect rendering owns temporary storage, but any descriptor
created by that adapter still comes from the persistent descriptor arena.
Descriptor tables and identity caches never retain pointers into resettable
per-call scratch.

A value's descriptor and an operation's boundary descriptor are distinct when
the operation consumes a different representation. The value keeps the source
descriptor that describes its bytes. The adapter, call, or match receives a
separately materialized target descriptor describing the bytes it will produce
or consume. Argument binding, match-condition binding, result binding, and
container item extraction copy descriptor identities into fresh locals; they
never repurpose the source value's descriptor local as operation scratch space.
ARC treats a same-value alias as borrow-capable only when its source and target
name the exact same explicit Boxy RC descriptor reference. A distinct
descriptor reference is an ownership boundary: the alias receives a moved or
retained unit before the source descriptor storage can be reused.

Equal storage layouts do not make two descriptors interchangeable. When a
boundary leaves the bytes unchanged, the value retains its source descriptor if
that descriptor names the committed storage while the operation descriptor
names a logical payload beneath that storage. The operation descriptor remains
boundary metadata; it is not attached to unchanged bytes. A boundary may relabel
an unchanged value only when the producer has proved that the source and target
descriptors use the same complete storage convention.

Tag-row reads are explicit descriptor operations. Nested payload descriptor
read, row-extension descriptor read, and residual-row subtraction are separate
LIR choices with separate operands. Residual-row subtraction names both the
source row descriptor and the target descriptor whose direct variants are
removed. The result preserves the source row's runtime discriminants, payload
layouts, nested descriptors, and extension chain. Neither lowering nor a
backend may infer one of these operations from an integer sentinel or from the
shape of the descriptor it receives.

Box payload reads are also an explicit descriptor operation. An
`assign_boxy_desc_ref` Box-payload read names the committed layout of the
Box value whose allocation payload is being described. The runtime uses that
layout to normalize the two supported descriptor conventions: a box-self
descriptor projects its first nested descriptor, while a payload-direct
descriptor is already the result. Lowering must not encode this operation as an
ordinary nested-index read, because values extracted from recursive containers
can legitimately carry the payload-direct convention. The interpreter and all
machine-code backends execute the same read through the shared Boxy ABI;
backends do not inspect descriptor shape themselves.

For every checked function value expression, the boxy lowerer emits an
`assign_packed_erased_fn`-style LIR statement that creates an erased callable
payload. The payload stores the function entry and capture bytes. Capture bytes
store ordinary captured Roc values plus any hidden descriptors or dictionaries
the function body needs. The erased callable's `on_drop` plan is explicit LIR
data selected before backend lowering. The statement also names the exact
immutable result descriptor stored in compiler-private callable metadata. If a
callable adapter boxes a result, the box carries the exact source payload
descriptor, not an unspecified box template.

When ordinary captured fields require nested descriptors, the producer builds
one exact immutable contents descriptor for the complete capture aggregate and
appends its pointer as a compiler-private final capture field. Both consumers use
that same field: `on_drop` reads it to release the aggregate, while the erased
worker projects each captured field's descriptor in planned field order before
binding or using the value. No consumer reconstructs capture descriptors from
capture bytes, layouts, or the worker's contextual types.

Every callable-value use edge records the exact hidden descriptor arguments for
that use. Descriptors required only by the callable body are captured from
those planned use-site arguments; descriptors represented structurally in the
callable signature remain ordinary callable boundary descriptors. An
uninstantiated declaration use may share a descriptor source only when the plan
contains one unambiguous worker/caller source for the same checked producer.
Lowering must not infer body captures from the declaration's generalized type
or from another use of the same worker.

Callable adapters collect result descriptor requirements as well as argument
and capture requirements. Explicitly planned captures retain their descriptor
identity; result traversal only appends requirement identities that are still
missing. When one requirement appears in multiple callable positions it names
one runtime descriptor identity and is not remapped by a later position. Before
materializing a new capture, the adapter consults the source callable's exact
local descriptor environment keyed by the requirement and source
representation. A descriptor bound to a different live value or merely sharing
the same storage layout is not evidence for that capture.

For every checked call through a function value, the boxy lowerer emits an
erased-call LIR statement. Its result descriptor operand is the call site's
expected representation descriptor; a distinct descriptor output local receives
the descriptor of the value actually written to the result local. A registered
compiler worker supplies that descriptor from its immutable callable metadata,
including when its actual return layout must first be adapted. An unregistered
host callable already writes the public call-site ABI and therefore produces the
call site's expected descriptor. ARC and every consumer use the descriptor
output attached to the returned value; they do not treat the expected descriptor
as evidence about bytes returned by a compiler worker.

Checked direct and function-value uses of the generic `Str.inspect` intrinsic
produce inspect-method demands in the Boxy plan. Each demanded nominal
representation records its exact `to_inspect` worker, owning checked module,
and module-local `MethodNameId`. Descriptor construction consumes that identity
directly; it does not rediscover a method from the representation's source
module. It turns the plan into a method slot carrying the worker procedure,
concrete argument layout and descriptor, hidden descriptor sources, and nested
dictionaries. Transparent nominals may share their backing storage layout, but
they retain a distinct checked descriptor identity when they carry an inspect
method.
Runtime recursive inspection checks this slot before opaque or structural
rendering, adapts the borrowed value into the worker argument representation,
and invokes the worker through the registered-procedure ABI. The prepared call
marks each adapted argument as borrowed or owned so the runtime preserves a
borrowed source, releases an owned temporary when the worker borrows it, and
releases an owned returned `Str` after appending its bytes. Backends do not
resolve method names or select this behavior.

For every checked direct call to a known procedure, the lowerer
emits a direct LIR call to the corresponding private boxy worker and supplies
the hidden descriptors and dictionaries required by that worker. A direct call
may write into the requested result local only when its committed layout and
descriptor representation identity agree with the worker result and the descriptor
boundary is an identity. A descriptor-bearing result target requires a
descriptor supplied by the call, and differing checked return types require the
planned result adapter even when their layouts happen to be equal. When a match
immediately consumes a descriptor-less worker result whose tag representation
otherwise satisfies the direct-call checks, the match result binding receives a
fresh descriptor local and the planned adapter materializes the worker's exact
tag descriptor into it. Pattern lowering consumes that descriptor; it never
interprets the bytes using only the match's contextual open-row descriptor.
Layout compatibility for this decision uses storage representation identity;
the result metadata itself uses checked descriptor identity. In particular,
equal transparent-nominal backing storage cannot erase a descriptor-carried
method or other nominal behavior.

For every checked static-dispatch call, it emits either:

- a concrete direct structural operation when the checked dispatch plan and the
  current type representation make the operation statically concrete, or
- an explicit dictionary/vtable indirect call when the checked plan requires
  polymorphic behavior.

Method equality follows this rule without a separate preference for structural
comparison. In particular, an unresolved constrained equality first consumes
its planned dictionary; `structural_allowed` only authorizes the structural path
selected by checked dispatch when no dictionary is required.

The lowerer must not discover method owners by searching registries at LIR
time. It consumes the checked dispatch plan and checked method registry entries
that checking already produced.

Synthetic static dictionary method adapters construct one explicit descriptor
source scope from the method worker's planned sources and the requirement-side
descriptor mapping. Callable captures in that adapter pass their own
requirement's mapped source representation into static materialization. If a
matched source child is itself descriptor-governed, materialization follows its
explicit descriptor-source chain until it reaches the concrete source; cycles
and conflicting mappings are invariant failures. Once two tag variants have
been matched by checked tag identity, their payload descriptors align by the
checked payload index. The adapter does not search ambient descriptor locals or
reconstruct a nested source from layout shape.

Boxy box/unbox/adapt operations are explicit LIR statements or explicit helper
calls selected by the lowerer:

- boxing a concrete value allocates an ordinary Roc box with the concrete
  payload layout and initializes payload bytes
- boxing a value that is already in boxy type-variable representation reuses
  the same box pointer when the source type layer is the compiler-internal
  boxiness of that type variable
- unboxing a value for a concrete host ABI or concrete operation copies payload
  bytes according to the explicit descriptor or concrete layout
- adapting a container copies, moves, or aliases according to explicit
  descriptor and layout data; it never assumes that equal pointer size implies
  equal runtime encoding

A move adapter transfers one ownership edge for each planned source segment.
Materialization and source release consume the same field, tag, and row-extension
mapping. Tag variants are paired by the adapter's checked tag identity, not by
numeric discriminant equality, because two committed tag layouts may assign the
same tag different discriminants. A target row-extension slot is an ordinary
planned target segment: when it stores the moved source box unchanged, that box
allocation is now owned by the target and must not be released as discarded
source storage.

Allocation identity may be compared only for the source segment and its exact
planned target segment. It must not be searched for elsewhere in the completed
target value, because two distinct source ownership edges may alias the same
allocation. When materialization replaces a dynamic source box with different
target storage, runtime release follows the nested adapter mapping first, then
releases the obsolete outer box allocation without dropping payload ownership
that moved into the target. This rule applies equally to direct tag variants,
residual rows, records, lists, and nested boxes.

Call-result materialization consumes the worker's returned value. When source
and target descriptors differ, the runtime first materializes target bytes,
then retains only target ownership edges that explicitly alias source edges,
and finally decrefs the complete moved source through its source descriptor.
Alias pairing follows box payload descriptors, list allocation identity,
record fields, and the observed tag variant or row extension on both sides.
Tag arguments use their explicit variant payload descriptors; numeric
discriminant equality is not a tag-identity proof. Fresh target allocations are
already owned and are not retained again. This retain-then-drop transfer is the
single call-result ownership protocol; the runtime does not reconstruct a
partial source-release set after conversion.

### Boxy LIR Data

The LIR program contains descriptor, dictionary, and adapter tables when `.boxy`
emits statements that reference them:

```zig
const BoxyTypeDescId = enum(u32) { _ };
const BoxyDictId = enum(u32) { _ };
const BoxyAdapterId = enum(u32) { _ };

const BoxyDescSource = union(enum) {
    static: BoxyTypeDescId,
    local: LirLocalId,
};

const BoxyDictSource = union(enum) {
    static: BoxyDictId,
    local: LirLocalId,
};

const BoxyTransferMode = enum {
    borrow,
    copy,
    move,
};

const BoxyAdaptStep = union(enum) {
    copy_bytes: struct {
        source_offset: u32,
        target_offset: u32,
        layout_idx: layout.Idx,
    },
    dynamic_payload: struct {
        source_offset: u32,
        target_offset: u32,
        source_desc: ?BoxyDescSource,
        target_desc: ?BoxyDescSource,
        mode: BoxyTransferMode,
    },
    nested_adapter: struct {
        source_offset: u32,
        target_offset: u32,
        adapter: BoxyAdapterId,
        mode: BoxyTransferMode,
    },
};
```

The exact descriptor, dictionary, and adapter payload structs are owned by LIR,
not by a backend. Their contents are serialized into LirImage when any reachable
LIR statement references them. A backend may cache lowered helper code for a
descriptor, dictionary, or adapter, but it must not change that data's meaning.

`BoxyDescSource` and `BoxyDictSource` are intentionally split into static side-table
references and local references. A `.static` reference names immutable LIR-owned
metadata. A `.local` reference names a runtime value that already has the
host-compatible representation required for explicit `Box(...)` ABI positions.
Lowering must preserve that distinction. In particular, a local descriptor or
dictionary reference is a normal local read for liveness, ARC, TRMC, backend
stable-location collection, and debug inspection; a static reference is not a
hidden local and must not be rediscovered by scanning local layouts.

Adapters are explicit plans, not backend heuristics. `copy_bytes` copies a
concrete representation segment whose layout is known. `dynamic_payload` moves,
copies, or borrows one descriptor-governed payload segment, optionally changing
from one descriptor to another. `nested_adapter` delegates a subrange to another
adapter plan. Adapters are reusable LIR side-table entries so host wrappers,
container item conversions, method arguments, and method returns can all name
the exact same conversion plan when they need the same representation change.
The interpreter executes adapters through the shared Boxy runtime. Dev, LLVM,
and wasm invoke that same behavior through their backend-specific runtime ABI;
reaching `assign_boxy_adapt` is never treated as unsupported.

Machine-code backends lower the complete Boxy statement surface to the shared
`roc_boxy_*` C ABI. Native LLVM links the target's standalone Boxy runtime
object and an object containing the serialized sidecar. Wasm merges the
relocatable Boxy runtime object and a static-data module containing that same
sidecar into either the final surgical-link module or the emitted relocatable
object. Entrypoint wrappers initialize the embedded runtime before calling Roc
code. Dictionary worker thunks and erased-callable registrations expose only
the proc ids, layouts, descriptor sources, and ownership metadata already
present in LIR; backend code does not derive any of them from procedure bodies.

In-process test invocation context is also an explicit execution ABI input. It
is threaded through ordinary procedures, Boxy dictionary calls and their
registered worker thunks, descriptor-guided inspect callbacks, and registered
Roc erased-callable workers. Exported symbol procedures omit it, and dev, Wasm,
and host-facing Boxy calls pass null.
The public erased-callable payload ABI remains `(ops, ret, args, capture,
ret_desc)`; only a registered in-process worker is invoked with the additional
context argument. The runtime receives in-process ABI selection as an explicit
flag; it does not infer the convention from whether the context pointer is null.

List operations that can copy or release descriptor-governed items use the
corresponding `roc_boxy_list_*` ABI in dev, LLVM, and wasm. The call passes the
exact descriptor attached to the input or result list plus the committed
item layout; the runtime projects the item descriptor and performs the
operation's internal ownership work. Concrete item layouts continue to use
the ordinary builtin ABI with concrete RC helpers. A backend must never set an
"items are refcounted" flag while supplying a missing callback, derive a
callback from erased storage, or inspect a descriptor to choose RC behavior.
An erased-box list that reaches such an operation without its explicit list
descriptor is a producer invariant failure.

Every linked Wasm image has exactly one provider for compiler runtime libcalls.
Standalone Wasm obtains them from the builtins object and the standalone Boxy
runtime suppresses its copies. Evaluator Wasm has no companion builtins object,
so its vtable-mode Boxy runtime exports the small required libcall set itself.
Runtime-object construction must preserve this ownership split; duplicate weak
or strong exports are not resolved by link order.

Dynamic RC in boxy LIR is explicit. A local whose boxy runtime layout is a
dynamic value has a pointer-sized committed storage layout, but its nested
payload drop/copy behavior is not recoverable from that storage layout alone.
ARC therefore emits RC statements whose helper plan contains the relevant
`BoxyDescSource`:

```zig
const RcHelper = union(enum) {
    concrete: layout.RcHelperIdentity,
    boxy: BoxyDescSource,
};
```

The RC operation itself is the LIR statement tag (`incref`, `decref`,
`decref_if_initialized`, or `free`); the helper names only how to perform the
operation for the value. Backends and the interpreter lower that helper
mechanically. They do not inspect the dynamic value, synthesize descriptors, or
choose a shallower drop because a descriptor is missing. A missing descriptor at
a statement that requires one is a lowering invariant failure.

Boxy indirect calls are also explicit. A dictionary-call statement names the
dictionary, method slot, argument span, result target, and hidden argument span.
The backend lowers it as an indirect call with an explicit call shape. It does
not know the checked method name or perform vtable lookup logic.

Its `result_desc` is the exact descriptor for the call-site result
representation, including descriptor-bearing descendants when the root layout
itself is concrete. It is not merely an indication that the result root needs a
descriptor. Method adapters and dictionary thunks use that exact descriptor to
materialize the worker result; consumers attach the produced descriptor to the
returned value without reconstructing nested descriptor positions.

Every `LirProcSpec` whose result descriptor can be exposed by a dictionary
worker records an exact `ret_desc`. The producer sets it only when every return
edge names the same immutable descriptor source and that source is either
static or one of the proc's explicit parameters. Dictionary thunks read this
field directly and write the descriptor beside the returned value. They never
scan the proc body, inspect the returned layout, or reconstruct a descriptor.

The boxy statement surface is:

- `assign_boxy_desc_ref`: materializes a descriptor reference into a local when a
  later host-compatible value or hidden argument needs an addressable descriptor
  value
- `assign_boxy_dict_ref`: materializes a dictionary reference into a local under
  the same rules for dictionary values
- `assign_boxy_box`: creates a boxy top-level box from a payload local, payload
  layout, optional payload descriptor, and explicit transfer mode
- `assign_boxy_reuse_box`: reuses an existing box pointer when the source is
  already in the compiler-internal boxy type-variable representation
- `assign_boxy_unbox`: extracts or projects a concrete target layout from a boxy
  value using the statement's descriptor and source transfer mode
- `assign_boxy_adapt`: applies a named adapter plan to a source local with
  explicit source and target descriptor refs and an explicit transfer mode; the
  descriptor refs are local reads and are not stored in the global adapter
- `assign_boxy_inspect`: produces a `Str` by invoking the planned custom inspect
  method when the explicit `TypeDesc` carries one, otherwise by structural
  inspection; the statement reads the source local, descriptor ref, and
  transfer mode, and never reconstructs behavior from a pointer-shaped layout
- `assign_call_dict`: performs a dictionary/vtable indirect call through a
  method slot with ordinary and hidden argument spans

These statements are ordinary LIR control-flow statements. Passes that only need
successor traversal treat them as straight-line `next` statements. Passes that
reason about local uses must account for their explicit source, descriptor,
dictionary, argument, and hidden-argument locals. Backends must not infer any
missing behavior from target layout shape; if codegen has not implemented a
boxy statement's specified behavior, reaching that statement is an invariant failure in
that backend.

### Debug Lambda Mono Verification

This verification applies only to `.lss`. Debug builds may additionally
materialize the logical Lambda Mono tree for verification. That tree is never
an input to production lowering, never a
substitute result, and never a recovery path. The direct solved-to-LIR builder
always produces the LIR result first. The debug verifier then checks a
separately materialized Lambda Mono tree against the direct path.

The verifier must be guarded so that release builds pay nothing for it: no tree
allocation, no materialized Lambda Mono traversal, no verifier data structures,
and no old Lambda Mono-to-LIR run. The release branch must be compile-time dead
after Zig specializes the debug condition.

The debug verifier checks at least these explicit decisions:

- every direct function specialization has the same lifted function id, solved
  function type, capture ABI, capture span, capture record type, source
  metadata, argument list, and return type as the materialized Lambda Mono tree
- every finite callable type has the same variants, variant ids, source
  symbols, target functions, and capture payload types
- every erased callable type has the same entries, targets, source function
  digest, and capture payload types
- every function reference, direct call, value call, captured local access, and
  callable pattern uses the same target, payload, and capture binding decisions
- root, layout, runtime-schema, const-plan, and requested-layout outputs name
  the same checked ids and Lambda Mono types

The verifier may also lower the materialized Lambda Mono tree with the legacy
Lambda Mono-to-LIR builder and compare that LIR to the direct LIR result. This
comparison is a debug assertion only. A mismatch is a compiler bug. The compiler
must not continue by using the materialized Lambda Mono LIR.

The full Lambda Mono body-lowering differential sweep over the ordinary eval
corpus runs once per day on Ubuntu through `nightly_gate.yml`; it is not part of
PR MiniCI.

### Direct Builder Internal Contracts

The selected LIR builder is one compiler stage, but its internal components have
explicit contracts so the stage does not become an implicit reconstruction layer.
The `.lss` builder components are:

- the layout builder consumes only Lambda Mono type nodes and emits committed
  LIR layouts plus explicit maps from checked ids to runtime encodings for
  direct-builder result data
- the procedure builder consumes only Lambda Mono function ids, root requests,
  and committed layouts, then emits LIR procedure ids and root metadata
- the local builder consumes lifted binder ids, Lambda Mono local types, capture
  bindings, and committed layouts, then emits LIR locals
- the pattern builder consumes lifted patterns, Lambda Mono callable patterns,
  and committed layouts, then emits LIR control flow
- callable lowering consumes generated callable type nodes and committed
  layouts, then emits ordinary tag operations or packed erased callable
  statements
- schema output consumes committed nominal layouts and checked
  nominal identities

The `.boxy` builder components are:

- the boxy representation planner consumes checked types, checked call evidence,
  and checked root metadata, then emits internal boxy layouts, private-worker
  plans, call-site substitutions, exact hidden descriptor/dictionary sources,
  and boundary adapter requests. Each root plan
  points at a worker plan whose source is exactly one checked procedure
  authority: a procedure template, top-level procedure binding, or procedure
  use template. A root without such a source is rejected during planning.
- the descriptor builder consumes checked types, committed payload layouts, and
  nested descriptor references, then emits LIR-owned `TypeDesc` entries
- the dictionary builder consumes checked dispatch plans and checked method
  registry entries, then emits LIR-owned dictionary/vtable entries
- the adapter builder consumes planned boundary requests and committed layouts,
  then interns `BoxyAdapter` plans used by calls, callable captures, containers,
  dictionary methods, and host-shaped wrappers
- the procedure builder consumes worker plans, checked procedure templates,
  boxy hidden parameter plans, and committed layouts, then emits private LIR
  procedure ids
  plus root metadata for host-shaped wrappers
- the local builder consumes checked binder ids, boxy representation plans, and
  committed layouts, then emits LIR locals
- the pattern builder consumes checked patterns and committed boxy layouts, then
  emits LIR control flow
- callable lowering consumes checked function values and hidden capture plans,
  then emits erased-callable packing and erased-call statements

No internal component may inspect source syntax, display names, runtime bytes,
backend symbols, or any data outside the selected builder's explicit inputs.
The `.lss` builder consumes Lambda Solved/Lambda Mono data, not checked bodies.
The `.boxy` builder may consume the checked bodies named by explicit checked
roots and reachable checked procedure templates, because checked CIR is its
source IR. Internal maps are work caches only. If an internal component needs
data that is not in the selected strategy's explicit inputs, committed layouts,
checked identities explicitly passed to the builder, or the LIR result it is
constructing, the earlier stage contract is incomplete. The direct builder must
not invent conversion operations to repair a mismatch between strategy decisions
and committed layouts.

The direct builder returns one explicit output object:

```zig
const LirLowerOutput = struct {
    store: LirStore,
    layouts: LayoutStore,
    root_procs: Span(LirProcSpecId),
    root_metadata: Span(RootMetadata),
    requested_layouts: Span(RequestedLayout),
    runtime_schemas: RuntimeSchemaStore,
    boxy_type_descs: Span(BoxyTypeDesc),
    boxy_dicts: Span(BoxyDict),
    fn_sets: Span(FnSet),
    erased_fns: Span(ErasedFns),
};
```

`store`, `layouts`, `root_procs`, and `root_metadata` are the normal LIR output
consumed by ARC and then by backends, the interpreter, and LirImage.
`requested_layouts` is for static data and provided data exports that asked for
layout decisions during the same lowering. `runtime_schemas` is for glue and
static data. `boxy_type_descs` and `boxy_dicts` are LIR-owned runtime tables
used only when reachable LIR statements reference them. `fn_sets` and
`erased_fns` are temporary compile-time output contexts used by
`CheckedModuleBuilder` while storing function values in `ConstStore`. Capture
slots are stored inside the corresponding function variant or erased-function
entry.

For `.boxy`, layout-only lowering may produce no procedures and still populate
`requested_layouts`. Each requested layout uses the host-visible layout for the
checked type, not the private worker layout, and carries a `ConstPlan` built
from the same explicit boxy representation plan. Alias and unsupported builtin
nominal representations share the child plan id rather than duplicating owned
plan payloads. Tag-union const plans mirror the boxy tag-variant table exactly:
every checked variant gets one `ConstTagVariant`, including zero-payload
variants with an empty owned payload-plan slice. Dynamic values, erased
callables, and opaque static-data shapes require the later
descriptor/callable/static-data support; reaching them while building a
requested-layout `ConstPlan` is a compiler invariant failure, not a fallback to
an approximate plan.

The output owns all of these stores and spans. Consumers borrow the fields they
need and must not add their own side stores for the same data. `LirImage`
contains only the ARC-inserted LIR fields: `store`, `layouts`, `root_procs`,
platform entrypoints, target usize, and any reachable LIR-owned boxy descriptor
or dictionary tables referenced by the image.

For shared-memory `LirImage` IPC, the mapping allocator is output-only.
Compiler scratch, Monotype graphs, and every pre-ARC IR use ordinary
reclaimable compiler storage. The IPC path copies the exact ARC-inserted store,
layout, root, and entrypoint arrays into the mapping; it does not rerun lowering
or derive any missing data while copying. An in-process embedder may instead
own compilation and the final image in one caller-provided arena, then install
offsets with `fillHeaderInBuffer`; that arena owns the whole compilation
lifetime and is not the shared-memory IPC transport.

### Layout Selection

Layout selection is the first stage that chooses runtime encodings:

- struct field order
- tag-union variant order and discriminants
- zero-sized representation
- boxed recursive slots
- list backing layout
- erased callable payload layout
- ABI-visible procedure argument and result layouts
- boxy dynamic value storage layouts plus descriptor-governed runtime-layout
  records
- boxy descriptor and dictionary table layouts

In `.lss`, layout selection consumes Lambda Mono types and produces LIR layouts
plus the runtime schemas and function result data that later compile-time
output, static data export, and glue code need. In `.boxy`, layout selection
consumes checked types plus boxy representation plans and produces committed
storage layouts, boxy runtime-layout records, exact host ABI layouts for
wrapper roots and static data exports, and LIR-owned descriptor/dictionary
layouts. Later stages consume those explicit layouts, runtime-layout records,
schemas, descriptors, dictionaries, and function result data.
They do not rediscover field order, tag discriminants, callable member
encodings, erased callable payload shape, or dynamic box payload behavior.

When layout commitment assigns a runtime discriminant or field offset to a
generated function tag, the builder outputs the mapping from the stage-local
`FnVariantId`/`FnMember` to the runtime encoding in direct-builder result data
for `ConstStore` output and static data export. `LirImage` does not store
function runtime data. It contains only ARC-inserted LIR, committed layouts,
root proc ids, platform entrypoints, and target usize.

### Nominal Record Field Order

Structural record layout is order-insensitive: fields are sorted
lexicographically by name and then stably by descending alignment, so source
field order never affects memory. Nominal records use the same structural
layout by default. A nominal record opts into declared-order layout only by
including an unnamed field in its declaration.

Declared-order nominal layout is for host-facing records that intentionally
mirror a C struct. When a nominal declaration contains an unnamed field, layout
commit sends the fields to the store in declaration order and commits that
order verbatim. The store inserts normal C-style implicit padding between
fields as alignment requires, and rounds the total size up to the maximum field
alignment. It does not repair or reorder the declaration. Padding is represented
in committed layout fields, so later stages consume committed offsets and sizes
instead of reconstructing them.

Nominal record declarations may contain unnamed fields, written `_` or
`_`-prefixed (`_reserved`). An unnamed field reserves the size of its type but
stores nothing, is not accessible, and imposes no alignment requirement on
itself (its bytes are uninitialized), which lets a declaration reproduce a C
struct's explicit padding without a dummy value to initialize. Layout treats
unnamed fields as alignment-one spacers, so they advance the offset by their
size in the committed field order. They contribute their size but not their
alignment to the struct, so pure padding never inflates a struct's alignment.
Using an unnamed field in a structural record type is rejected during
canonicalization.

Declared field order is explicit data. Record rows are sorted lexicographically
by name at several stages (checking, Monotype row lowering, and Monotype
instantiation) because field-name resolution and digests depend on a single
fixed order, so the declared order is not recoverable from the lowered record
itself. Canonicalization preserves it—a nominal declaration's record
annotation keeps its fields in source order—and checking records it as
explicit CheckedModule data distinct from the (lexicographic) backing row,
so later stages consume it without rescanning declarations. Monotype lowering,
boxy planning, and layout lowering all use this checked datum. The struct
commit uses
it only for the unnamed-field opt-in described above; otherwise nominal records
use the structural order of their backing row. Field-name resolution continues
to use the lexicographic row order, independent of the layout offset map. The
same data is consumed by the interpreter's layout store, so all backends agree.

### Pattern Lowering

Pattern decision construction is part of the direct LIR builder. It consumes
strategy-specific patterns and committed layouts and emits LIR control flow.
`.lss` consumes Lambda Mono patterns. `.boxy` consumes checked patterns plus
boxy representation plans. There is no persisted pattern-decision IR.

Match compilation goes through one shared decision-tree module
(`src/postcheck/match_tree.zig`), consumed by both LIR lowerers through thin
accessor contexts. A match lowers to multiway tests—one `switch_stmt` per
tested tag or integer position, one `str_match_set` per tested string position,
one length switch per tested list position—with each occurrence (scrutinee
position) read into at most one local per dominating scope: one discriminant
read per tested tag position, one field read per destructured position, one
length read per list position. Branch bodies and guards are lowered exactly
once; a guard failure re-enters the residual tree for the rows below it without
re-testing columns already known. Exhaustiveness is consumed, never re-derived:
a tag test whose arms cover the occurrence type's committed variant set emits
its last arm as the switch default (a single-variant union emits no dispatch at
all); open matches keep the `comptime_exhaustiveness_failed` / `runtime_error`
terminal.

Named record-rest binders are derived values, not durable pattern decisions.
Monotype pattern lowering removes the rest binder from the record pattern,
captures that exact record occurrence in a compiler-generated local, and
constructs the remainder record explicitly from the solved field cells. When
the guard uses the remainder, the match branch owns an irrefutable statement
span that runs after the structural pattern succeeds and before the guard; its
locals remain in scope for the branch body. A remainder used only by the body
is an ordinary body-local statement. The decision-tree emitter assigns all
successful pattern locals before lowering the branch statement span, so later
stages consume this ordering directly; no source record-rest pattern survives
Monotype lowering.

**The sharing invariant.** Monotype is a DAG: an expression id referenced from
multiple positions is re-lowered at each reference, so downstream control
sharing must go through typed lifted join points or LIR join points, never
through re-lowering a Monotype id twice. Direct lowering preserves lifted
`join_point`/`jump` as LIR `join`/`jump`. PR 9707 removed the one known violator,
the list-pattern desugarer, after measuring ~(items+1)^branches statement
blowup. The match
compiler holds the invariant by construction: rows (branches) are never
duplicated during specialization—a row that does not test the selected
occurrence ends the test group instead of being copied into every arm, and the
rows below the group compile once behind a shared exit join. Because rows never
duplicate, emitted statements are O(total pattern size); a debug statement-count
lint in the emitter asserts a hard multiplier bound per match so an exponential
regression fails loudly instead of shipping.

### ARC

The direct LIR builder emits ownership-neutral LIR. ARC insertion runs after
LIR construction and emits explicit `incref`, `decref`, and `free` statements.
Each explicit RC statement carries the concrete RC helper selected by ARC.
Backends, the interpreter, and LirImage builders follow those statements
mechanically. The ARC algorithm is specified in ARC Borrow Inference below.

### Join-Parameter Scalarization

Between direct LIR lowering and ARC insertion, one normalization splits
struct-typed join parameters into per-field parameters when the parameter is
only ever read field-by-field and every entry can explicitly supply all of its
fields. Each entry snapshots every replacement field before changing any
parameter, then performs the per-field writes. This preserves the original
whole-struct assignment's materialize-before-rebind ordering: a replacement field may
borrow through an old parameter value, so releasing that parameter before all
replacement fields have been materialized would leave a later read dangling.
Single-use literal wrappers disappear after their operands are snapshotted;
non-literal initializers and the initial procedure-argument value are projected
into snapshot locals. Field reads become local aliases. This is required for
refcounted loop state: without it, every jump pays a retain on each refcounted
field read whose wrapper dies at the jump, and ARC cannot turn that into a move
because the wrapper's release covers all fields at once. After scalarization
the state flows through pure alias chains that borrow inference resolves to
moves. Parameters with any whole-value use remain unsplit, and the pass
iterates so nested wrappers dissolve. A struct parameter carrying a Boxy
descriptor also remains unsplit: scalarization may not replace its `assign_ref`
field reads with local aliases unless it also introduces and initializes a
matching descriptor parameter for every resulting field local.

## ARC Borrow Inference

ARC insertion computes a whole-program borrows-with-lifetimes solution over
ownership-neutral LIR, then emits explicit `incref`, `decref`, and `free`
statements from that solution. Roc's borrow inference system is based on
["Fully-Automatic Type Inference for Borrows with
Lifetimes"](https://theory.stanford.edu/~aiken/publications/papers/oopsla26.pdf)
by William Brandon, Benjamin Driscoll, Frank Dai, Jonathan Ragan-Kelley, Mae
Milano, and Alex Aiken (OOPSLA 2026). It adapts the paper's fully automatic
borrow inference for reference-counted pure functional programs, implemented in
the Morphic compiler, to Roc's statement-only LIR.

The motivation is RC traffic. With all-owned insertion, every non-final
occurrence of a refcounted value pays an atomic increment plus a matching
decrement, and read-heavy programs spend a large fraction of their runtime on
RC statements that a borrows-with-lifetimes typing proves unnecessary. Borrow
inference deletes those statements statically. It also keeps refcounts at 1
across read-only uses, which is what lets the runtime `refcount == 1` checks
in list and string operations mutate in place instead of copying.

The ARC stage contract does not change:

- input: ownership-neutral LIR containing no RC statements
- output: the same LIR statement language whose only ownership data is
  explicit RC statements carrying concrete RC helpers
- backends, the interpreter, and LirImage consume the output mechanically and
  make no ownership decisions
- no mode, lifetime, signature, or specialization table appears in checked
  modules, LirImage, or any consumer-visible structure; everything the solver
  computes is ARC-stage-local and is dropped when the stage finishes

Borrow inference runs after every other selected post-check transformation.
For `.lss`, monomorphization, lifting, call-pattern specialization,
lambda-set solving, inlining decisions, Lambda Mono decisions, and LIR lowering
are all complete before solving starts. For `.boxy`, checked-to-LIR lowering,
boxy descriptor/dictionary emission, host adapter emission, and LIR lowering
are all complete before solving starts. This ordering is required, not
incidental:

- inference attaches resources to refcounted positions of committed layouts and
  explicit dynamic boxy descriptors, which exist only after LIR lowering commits
  them
- every specializing or restructuring pass changes which values exist and how
  calls are shaped, which invalidates an ownership solution; solving once,
  last, means the solution is never patched after a later transformation
- earlier `.lss` specialization makes inference more precise: call-pattern
  specialization deletes refcounted aggregate intermediates outright and
  exposes per-position flow that one aggregate-typed parameter would hide.
  `.boxy` intentionally gives up that precision for lower compile time, but it
  still exposes every value and dynamic descriptor explicitly before ARC starts.

The dependency is one-directional. Upstream stages feed borrow inference;
the solution is consumed only by emission within the same ARC stage. No
earlier stage may consult, anticipate, or encode ownership decisions.

Borrow inference is not best-effort analysis. It is a least-fixed-point
computation over finite lattices: deterministic, total, and independent of
traversal order. Every mode and lifetime is the least solution of explicit
constraints generated from LIR statement structure, committed layouts, per-op
`RcEffect` data, and pinned ABI signatures. Every constraint system has a
solution, because the all-owned assignment satisfies all constraints; the
solver outputs the least one. There is no failure path and no recovery path.
An occurrence the solver leaves owned is emitted as a move or an `incref`,
exactly as all-owned insertion would emit it.

### Vocabulary

- `Resource`: one refcounted position of one local—the top-level value, or
  one nested rc position reachable through the local's committed layout.
- `Mode`: `borrowed` or `owned`. The mode lattice is `borrowed < owned`.
  A borrowed resource is an alias whose occurrences emit no RC statements. An
  owned resource is responsible for exactly one reference count: it is
  eventually moved exactly once or decremented exactly once on every path.
- `Lifetime`: a tree-shaped interval of one proc body recording, on each
  control-flow path, the last point at which a value must still be live.
  Lifetimes of values that flow through params and returns are summarized by
  lifetime variables in proc signatures.
- `RcSig`: the solved ownership signature of one proc—a mode for every
  refcounted param and return position, plus the lifetime relation between
  borrowed returns and the params they may borrow from.
- `ArcPlan`: one stage-local slot for a structured path in one ownership
  context. Dependency-solver visits update the slot with concrete moves,
  retains, releases, call demand, and uniqueness bits. It contains no
  ownership set, liveness row, or solver context.
- materialization: consumes an `ArcPlan` and writes RC statements into LIR
  statement chains. Materialization makes no ownership or liveness decisions.

The paper's `dup` corresponds to LIR `incref`, its `drop` to `decref`/`free`,
and its moves to the absence of both at a final owned occurrence.

### Resources Over Layouts

A local participates in inference iff its layout contains refcounted data
(`layoutContainsRefcounted`) or its LIR layout is a boxy dynamic value whose
descriptor says the value owns Roc-managed storage. Each participating local
owns one resource per rc node reachable in its committed layout or dynamic
descriptor:

- the top-level value itself, when its layout is `str`, `list`, `list_of_zst`,
  `box`, `box_of_zst`, or `erased_callable`
- the item resource of a `list`
- the payload resource of a `box`
- one resource per refcounted field of a `struct_`
- one resource per refcounted payload position of each `tag_union` variant
- the captures resource of a `closure` / `erased_callable`
- the top-level and payload resources described by a boxy `TypeDesc`

Rc positions are interned per `layout.Idx` as a stage-local place table. The
place graph is finite: committed layouts guard every recursive occurrence
behind a box (layout commit performs SCC analysis and materializes back-edges
as boxes), and a place path that re-enters a layout already on the path folds
into the earlier place. One place under a recursive box therefore stands for
every unrolled occurrence, which matches the typing rule below that nested
modes are uniform through an owning rc.

Boxy dynamic places are interned per descriptor identity and dynamic position,
not by value pointer. A dynamic value is pointer-sized at the LIR layout level,
but its payload ownership graph is descriptor-defined. ARC consumes the
descriptor reference emitted by boxy lowering; it never treats a pointer-sized
dynamic value as a shallow pointer merely because the layout is one word.

Nested resources carry two modes, following the paper's storage/access split:

- the storage mode is the mode the containing allocation stores at that
  position. Storage modes are equality-constrained along value flow: an
  `owned rc (borrowed rc t)` cannot exist, because dropping the outer rc to
  zero must be allowed to drop the inner rc. Newly created allocations store
  owned content, so in practice storage modes solve to owned everywhere; the
  constraint form is kept because it is what makes payload-read borrowing
  sound.
- the access mode records whether it is safe for a payload read at that
  position to produce a borrow. It is solved from where the read result
  flows, exactly like a top-level occurrence mode.

Top-level resources carry one binding mode plus one occurrence mode per use
site.

### Lifetimes Over Statement Structure

A program point is a position in a proc's statement structure: one step per
statement along `next` chains, alternation at `switch_stmt` branches, and one
region per `join` body and per `join` remainder. `jump` statements create
flow edges between regions, including back edges for loops.

A lifetime is a tree over this structure, built from:

- the empty lifetime (resource never needs to be live)
- a point (one occurrence)
- sequential composition (ends in the bound statement vs. later in the chain)
- alternating composition over switch branches, including one-sided forms for
  values used in only some branches

Lifetimes within one proc form a finite lattice ordered by containment, with
a least-upper-bound operation taken pointwise over branches. Finiteness is
bounded by the proc's statement count and branching depth, which is what
guarantees fixpoint termination. Join regions and back edges do not get
special lifetime constructors: constraints flow between regions through join
parameter resources (below), and the lattice's finiteness makes iteration
over back edges converge.

Lifetimes that cross proc boundaries are not represented as trees. A proc's
borrowed param positions carry lifetime variables; a borrowed return position
carries a join of the param lifetime variables it may borrow from. Callers
instantiate those variables with caller-side lifetimes at each call site.

### Constraints Per Statement Form

Inference lifts each proc body once, assigning fresh resource variables, and
generates constraints per statement:

- `assign_literal` (str), `assign_list`, `assign_struct`, `assign_tag` with
  refcounted payload, `assign_packed_erased_fn`: the target's top-level
  resource is a newly created reference count, so its binding mode is owned.
  Operand occurrences that are stored into the new allocation must be owned
  at that occurrence (storage constraint).
- `assign_ref` with `.field`, `.tag_payload`, `.tag_payload_struct`: a payload
  read. The result may be a borrow of the source. The source must be live as
  long as the result is used (lifetime constraint), and the access mode of
  the source's nested position bounds the result's mode: if the access mode
  is owned, the read emits an `incref` on the result; if borrowed, it emits
  nothing.
- `assign_ref` with `.local`, `.list_reinterpret`, `.nominal`, and
  `set_local`: pure flow. The use resource and binding resource are related
  by flow constraints in both directions (see the equations below). A final
  owned occurrence becomes a move.
- `assign_call`: instantiate the callee's `RcSig`. A borrowed param position
  constrains the argument to be live across the call and emits nothing. An
  owned param position consumes the argument occurrence: a move when the
  occurrence is final, an `incref` otherwise. A borrowed return position
  constrains the result's lifetime to the join of the lifetimes of the
  arguments it may borrow from; an owned return is a fresh owned resource.
- `assign_call_erased`: the erased-callable ABI is a pinned all-owned
  `RcSig`: refcounted args owned, captures owned by the callee, result owned.
  Inference does not flow modes through erased callable values.
- `assign_call_dict` / boxy dictionary indirect calls: instantiate the explicit
  call shape named by the LIR statement. Hidden descriptor and dictionary
  arguments obey the same ownership modes as ordinary arguments according to
  their layouts. Method selection is already encoded in the dictionary slot;
  ARC does not inspect checked method names.
- boxy box/adapt statements: constraints come from the explicit source local,
  target local, concrete layout, and `TypeDesc` references named by the
  statement. Boxing a concrete payload creates a new owned top-level box
  resource whose nested payload storage constraints come from the descriptor.
  Reusing an existing boxy type-variable representation is pure flow of the
  top-level box resource, not a new allocation. Unboxing or adapting into a
  concrete host layout borrows or consumes the box payload according to the
  statement's explicit ownership mode and descriptor.
- `assign_low_level`: constraints come from the op's `RcEffect`. Args in
  `consume_args` are owned occurrences. Args outside `consume_args` are
  borrowed occurrences whose lender must be live at the call. Args in
  `retain_args` are stored by the op, so the stored value's storage
  constraint applies. A new mask, `result_borrows_args`, names the args the
  result may alias without owning (for example `list_get_unsafe` results
  borrow arg 0); the result's mode is then solved like a payload read, with
  the lifetime constraint tied to those args. Ops whose results never alias
  a retained arg produce fresh owned results as today.
  A low-level operation may also declare one explicit ARC-only borrowed-result
  variant. Neutral LIR retains the ordinary source operation, but constraint
  generation uses the borrowed variant's `RcEffect`. After modes solve, an
  actually borrowed result materializes the borrowed operation and its effect;
  an owned result materializes the ordinary consuming operation and its
  effect. If the ordinary operation needs to consume an argument whose solved
  binding is borrowed, ARC emits one retain immediately before the operation
  to supply that consumed unit. Every post-ARC statement therefore contains
  the exact concrete operation and effect the backend executes. The variant
  mapping is static low-level-op data, and only ARC may select from it.
- `join` / `jump`: each join parameter's resources get modes and lifetime
  relations like an intra-proc signature. `set_local` with
  `initialize_join_param` followed by `jump` is a flow edge from the
  jump-site resource into the join-param resource. Back edges contribute the
  same constraints; the fixpoint handles them.
- `ret`: flow into the proc's `RcSig` return position.
- `expect`, `debug`: borrowed reads.
- `crash`, `runtime_error`: terminal; every live owned resource is dropped on
  that path by emission.
- `incref` / `decref` / `free` in the input: a compiler bug (the input
  contract is RC-free LIR), enforced by a debug assertion.

Ownership demands propagate transitively through pure same-value aliases
(`.local`, `.list_reinterpret`, `.nominal`): a consumed alias is a consumed
source, so the chain's single unit moves link by link to the consuming
occurrence instead of the alias paying a retain while the source's unit is
separately released. Payload reads do not propagate demands; borrowing the
container is exactly the win there.

The solver runs three equation groups to their least fixed points, in order,
following the paper's Figure 8 adapted to LIR vocabulary:

```text
approximate lifetimes (escape analysis, pessimistically deep):
  ltApprox(bind) >= if flow(bind, use) then ltApprox(use)

modes:
  access(bind) >= if flow(bind, use) then access(use)
  access(use)  >= if flow(bind, use)
                  and ltApprox(use) escapes scope(bind)
                  then access(bind)
  storage(bind) = storage(use) along flow
  access(r) = owned for every pinned-owned position

precise lifetimes (exact, given solved modes):
  lateralFlow(bind, use) <= flow(bind, use) and use is owned
  verticalFlow(parent, result) <= payload read of an owned position
                                  whose result stays borrowed
  ltPrec(a) >= if lateralFlow(a, b) or verticalFlow(a, b) then ltPrec(b)
```

Approximate lifetimes deliberately over-extend through nested rc positions so
that escape decisions are sound before modes exist. Precise lifetimes are
recomputed after modes are fixed and are the only lifetimes emission may use
for placing `decref` statements; approximate lifetimes are not sound for drop
placement and must not reach emission.

### Pinned Signatures

Some signatures are ABI contracts, not inference results. They are pinned
before solving and never weakened:

- root procs (`runtime_entrypoint`, `provided_export`,
  `platform_required_binding`, `hosted_export`, `test_expect`, `repl_expr`,
  `dev_expr`, and compile-time roots): every refcounted param owned on entry,
  every refcounted return position owned. This is the existing host ABI rule.
  In `.boxy`, host-visible roots are host-shaped wrapper procs. Private boxy
  workers are not root procs and their hidden descriptor/dictionary parameters
  solve like ordinary private proc parameters unless another ABI rule pins them.
- hosted procs: every refcounted arg owned by the host, result owned. This
  keeps the LirImage And Hosted Functions contract unchanged.
- erased-callable procs (`ProcAbi.erased_callable`): all-owned, as above.
- low-level ops: each concrete operation's `RcEffect` is its signature; it is
  explicit static data on the op, never inferred. An ARC-only borrowed-result
  variant is likewise an explicit operation and signature, selected only by
  the ARC rule above.

### Interprocedural Solving

The solver performs one exact structural walk of every ownership-neutral proc
body and records its reachable statements as a per-proc inventory. A neutral
body may back several proc specs: direct calls, returns, join bodies, and
parameter uses are recorded once per proc, while definitions, local
occurrences, visibility links, and uniqueness operations are recorded once per
structurally distinct statement. Pinned-proc escapes, call-graph SCCs,
bindings, signatures, visibility, uniqueness, returns, and join summaries all
consume these typed tables; none independently rediscovers CFG reachability or
decodes the same statements again. The inventory is
stage-local and exact: it records every reachable statement and no unreachable
statement, with no cap or approximation.

The caller-indexed tables also record direct-call tailness and, for each proc,
the parameter positions that can reach consuming low-level runtime uniqueness
checks. Variant planning consumes those solved masks directly; it does not
rescan a proc body or allocate a module-sized visited table. After binding
reaches its fixed point, `Solution.borrowed_call_result` stores the exact set
used by per-proc liveness domains, so ARC insertion performs no second
module-wide statement scan to reconstruct call-result kinds.

The module solver constructs one dense domain containing exactly locals whose
committed layouts participate in ARC. Binding tables, dependency edges,
visibility sets, uniqueness sets, and their worklists use those dense indices;
scalar and otherwise ARC-irrelevant locals allocate no solver rows. Once the
fixed point settles, the result is expanded exactly once into LocalId-indexed
lookup tables for ARC emission.

The proc call graph is derived from the lifted `assign_call` statements. The
parameter/return solver projects local definitions and static demands,
pure-alias edges, direct-call argument dependencies, reachable return locals,
and join bodies from the same lift. No signature round rescans a proc body.
`RcSig` represents procedure argument positions zero through fifteen in one
`u16`; every refcounted parameter at position sixteen or later has the exact
all-owned signature. Functions retain arbitrary arity. The all-owned tail is a
declared ARC capability boundary: it consumes and releases every unit exactly,
but does not participate in borrow inference, borrowed-return lender masks,
unique-parameter seeding, or mode specialization. A return whose only possible
lender is in that tail therefore remains owned. Low-level `RcEffect` argument
masks are a separate `u64` domain and do not inherit this boundary.

Signatures solve in two phases:

1. Parameter modes reach a fixpoint with returns treated as owned: non-pinned
   refcounted parameter positions within the represented prefix start borrowed
   and flip to owned when any occurrence demands a unit under the current
   signatures. One work item is one exact `(callee, parameter position)` bit
   that just flipped. Its reverse adjacency contains precisely the caller
   argument locals newly demanded by that flip; those demands propagate through
   explicit pure-alias edges and may enqueue their owning parameter bits. Every
   bit flips at most once, so the borrowed set only shrinks and the worklist
   terminates.
2. With parameter modes final, a return becomes borrowed when every `ret`
   in the proc returns a borrow anchored on a borrowed parameter of that
   proc, with the parameter positions recorded as the return's lenders. A
   final binding solve then lets callers borrow such results: a call result
   whose lender mask names exactly one refcounted argument is borrow-capable
   in the caller, anchored on that argument.

Unique-return bits use a separate monotone worklist over typed uniqueness
entries collected by the same structural walk. A proc bit feeds only its
direct-call result locals;
a newly born-unique local feeds only its explicit pure-alias dependents; and a
newly unique local feeds only the procs whose recorded `ret` statements return
it. Holder-destroy entries are signature-independent and fixed before this
worklist starts. Proc bits and local birth bits only turn on, each at most once,
so unique-return solving never reruns whole-store uniqueness analysis.

Visibility sharing is an undirected equivalence relation over the typed lift,
so it is solved by union-find and then seeded per component. Pure-alias
uniqueness uses the exact reverse source-to-alias relation: only dependents of a
newly changed origin enter its worklist. After return modes settle, the second
binding phase likewise revisits only changed borrowed-return call results and
their transitive reverse borrow dependents.

The reachable `JoinBody` entries collected during solving are also the sole
input for emission's jump resolution. Emission must not rediscover join
definitions by traversing the ownership-neutral graph again.

Borrowed parameters anchor borrow groups of their own: they are live for the
whole call by ABI, so payload reads from them borrow without the callee
emitting any release for the group.

Tail calls need one rule so that borrow inference never blocks backend
tail-call lowering. LIR has no tail-call statement; a call is in tail
position when the next statement returns the call result. Call-graph SCCs
(computed once, iteratively) feed exactly this rule: a tail-position call to
a proc in the same SCC demands ownership of its refcounted arguments, so
emission never places a release after the call on that path. Calls that
leave the SCC keep borrowed positions, since the caller's drops precede the
tail call there only when the values genuinely die earlier.

### RC Planning and Materialization

The ownership-summary dependency solver also owns planning. Every structured
root, control arm, join body, join remainder, and switch continuation has a
stable `ArcPlan` slot. The key is the producer-authored structured path plus
its ownership context, never statement id alone: the same neutral statement
may be validly reached under several different states. When an entry summary
shrinks, the solver revisits and replaces exactly that slot; a monotonically
increasing slot version prevents an older queued visit from overwriting a
newer decision. Join and switch dependencies patch their registered terminal
plans when keep/common states change. Once the fixed point converges, direct
call demands are mapped to final variants and every reachable plan is complete.

Each plan records every concrete move/retain/release decision and call-variant
or uniqueness choice. For a low-level operation with an ARC-only borrowed
variant, the plan also records the selected concrete operation and `RcEffect`.
Materialization receives neither ownership state nor liveness and only follows
completed plans to rebuild statement chains:

- borrowed occurrence: no statements.
- owned occurrence that is not the final occurrence on its path: `incref`
  before the consuming statement. Adjacent increments of the same local
  coalesce into the `count` field.
- owned final occurrence: a move; no statements.
- owned binding whose precise lifetime ends without a move: `decref` at the
  earliest point its precise lifetime permits on each path. Early placement
  is required, not optional: it bounds liveness growth from borrowing and
  returns refcounts to 1 before later mutation points, preserving in-place
  mutation in the runtime uniqueness checks.
- owned binding that is never used: dropped immediately after creation.
- reassignable local write (`replace_existing`): the previous resource ends
  at the write (decremented unless moved), and the write starts a fresh
  resource. Borrows of the previous value cannot outlive the write; the
  scope-end constraint above forces such occurrences owned instead.
- caller-side adaptation at calls: passing an owned final occurrence to a
  borrowed param borrows it for the call and drops it at its precise
  lifetime end; needing an owned result from a borrowed return position
  emits one `incref` on the result.
- switch branches and join regions balance drops exactly as today: a value
  that dies in one branch and survives another is dropped on the dying
  branch.

Emission also emits `free` where it does today (intent marker for a value
the proc fully releases); `free` keeps its current meaning of decrement plus
deallocation with nested decrefs through the RC helper plan.

RC helper selection stays in this stage. For ordinary concrete layouts, each
emitted statement carries the helper derived from the local's layout. For boxy
dynamic values, each emitted statement carries the dynamic helper plan and
`TypeDesc` reference selected by ARC from explicit LIR descriptor data. In both
cases, helper choice is complete before backends, the interpreter, or LirImage
consume the LIR.

Planning decisions consume one precomputed per-statement liveness table over
the ownership-neutral statement graph. Each proc has its own dense ARC domain,
constructed directly from that proc's complete, unique, sorted `frame_locals`
inventory. The domain contains only locals whose committed layouts contain
refcounted data, their explicit ownership-unit and borrow-group representatives
from the solved ownership graph, and the explicit group and
borrowed-call-result bits required by the equations above. Every ownership-unit
and group-leader representative must belong to the same producer-authored proc
inventory; a missing representative is an invariant violation, never something
ARC reconstructs from the statement graph. A module-wide borrow group may also
contain members from other proc specs that share ownership-neutral locals. The
proc liveness domain counts exactly the members in its own frame, since an
outside member cannot occur on one of that proc's paths. It derives those
counts in one linear frame scan from the solved leader relation, so the module
solution retains no redundant flat group-member table.
Join ownership sets use the resource prefix of this same per-proc domain.
Consequently neither ownership nor liveness rows are widened by locals from
other procedures. Unrelated scalar locals are not ARC resources and never
receive raw liveness bits. This distinction is load-bearing for wide static
initializers: a list of a million scalar items may require a million scalar
LIR locals, but it contributes only the list allocation and its explicit
ownership representatives to ARC's resource-bit width. Widening every row with
non-resource or other-proc locals would make ARC memory quadratic in an input
that needs only linear ownership work.

The table carries exact read-before-rebind decisions. Compile-time performance
work may change its storage or construction, but must not weaken the liveness
questions, omit resource bits, or approximate the least fixed point.

Ownership sets and liveness rows whose explicit domain width fits in one
machine word are stored inline; wider sets use exact allocated words. This is a
representation choice made solely from the producer-authored domain width, not
a heuristic. The ownership-neutral liveness graph is immutable and built once
per source proc, then shared by every ownership variant emitted from that
source. It uses a reusable dense statement-to-node table, so successors,
predecessors, and worklist edges are direct node indices rather than statement
hash lookups. Its strongly-connected-component condensation is solved in
reverse dependency order: an acyclic singleton is evaluated once, and
iteration occurs only inside genuinely cyclic components. Keep-free rows live
at their compact graph nodes and the active source graph supplies their direct
dense statement-to-node lookup.

Each join receives a compact loop identity whose direct cache covers the
forward closure of its explicit body and remainder roots. The join keep-set
adds a boundary row to each reachable loop-edge node. When that keep-set
shrinks, ARC computes the exact new boundary row, seeds only loop edges whose
rows actually changed, and propagates the delta through changed predecessors;
it neither rebuilds the graph nor discards unaffected rows. Every loop-keyed
query is therefore a direct `(loop identity, node index)` lookup with no map or
statement scan.

Join ownership is a must-property. Each reachable jump site contributes a
state that can only shrink; a join summary maintains their running
intersection incrementally, and recomputes the body keep-set from that exact
meet plus the join parameters. A site contribution that shrinks without
changing the global meet cannot schedule downstream work. Each loop identity
records whether its solved rows consumed any keep bits. A keep change that
supplied no boundary bits schedules no liveness work.

Join, jump-site, and continuation-switch identities are compact indices
assigned by the structural lift. Summary tables and plan registrations use
direct slices over those indices. Planning reuses per-emission death,
transfer-position, and call-argument scratch buffers; only converged decisions
are retained in stable `ArcPlan` storage.

All solver-summary, planning, and materialization state for one proc emission
has the same lifetime and is allocated from one proc-scoped arena. Emitted LIR
remains in the `LirStore`; arena-backed plans, ownership snapshots, branch
results, and decision slices are discarded together only after the proc body
and metadata have been committed. `ArcPlan` is the phase boundary: dependency
solving may query ownership and liveness while filling a slot; its materializer
accepts neither and only follows the explicit decisions.

Immediate `incref`/matching-`decref` cancellation is part of retain
construction: count one cancels the pair and larger counts are reduced by one.
The completed graph is never rewritten by a later RC-elision traversal. Final
join metadata is likewise recorded when each final join is materialized,
checked for consistent duplicate ids, sorted once, and committed with
the proc body; it is not recollected from the finished graph.

The debug borrow certifier deliberately spends more: it re-certifies join
bodies per distinct entry state and summarizes per statement for walk
deduplication. Release builds compile the certifier away entirely, so only
debug compiler builds pay, and any certifier slowness is fixed inside the
certifier, never by weakening what it checks.

### Mode Specialization

This section describes ARC mode specialization, not the user-facing
`--specialize` lowering-strategy flag. The two choices are independent concepts:
`--specialize=yes|no` selects `.lss` versus `.boxy` before LIR, while ARC mode
specialization decides how many ownership-signature variants to emit after LIR.

Within its represented prefix, a proc's solved `RcSig` is the most-borrowed
signature its body admits; its tail is all-owned by the declared boundary
above. Callers can always adapt to the inferred prefix, but adaptation has a
cost: passing an owned value to a borrowed param keeps a caller-side drop that
a move would have deleted, and an owned use of a borrowed return pays an
`incref`. Mode specialization removes that adaptation cost by emitting one proc
variant per demanded mode vector.

A demand vector assigns each represented refcounted param position a mode at
or above the solved signature (pointwise more owned); tail positions remain
owned. Return positions are never
demanded: a borrowed return that the caller needs owned pays one retain, and
that retain costs the same whether it is emitted in the caller or inside an
owned-returning variant, so no variant exists to save it. Specialization is
a worklist keyed by `(proc, demand vector)`:

1. Every proc is emitted once at its solved signature (the base variant).
2. While emitting any proc, an `assign_call` site with an owned final argument
   upgrades a borrowed position exactly when ownership changes runtime work in
   the callee: it owns a borrowed return, seeds a reachable uniqueness check,
   or activates an exact owned-only field take. A release-only relocation does
   not create a variant.
3. The call site targets the `(callee, vector)` variant, creating it if new
   and re-emitting it from the callee's ownership-neutral body under the
   demanded vector. Inside the variant, demanded positions override the
   solved borrowed binding to owned, and everything else solves identically.
4. The variant table is keyed by vector content, so identical demands share
   one variant deterministically, independent of discovery order.

Variant bodies are cloned with the existing statement-cloning machinery and
added with `LirStore.addProcSpec`. Root procs are never specialized; their
vectors are pinned. The variant count is bounded by realized demand vectors,
not by the theoretical vector space.

A build without mode specialization is the same worklist with every demand
vector forced to the solved `RcSig`, which yields exactly one variant per
proc. Dev builds (`--opt=dev`) and compile-time evaluation use that
single-variant form, because solving is the only new compile-time cost they
accept. Interpreter builds (`--opt=interpreter`) also use the single-variant
form. `--opt=speed` and `--opt=size` both enable full specialization;
specialization clones proc bodies, but each variant carries fewer RC
statements, and variant counts are bounded by realized demand vectors. All
forms run the identical solver; they differ only in which demand vectors get
a variant, so build modes can never disagree about observable program
results—only RC statement placement and proc count differ.

### In-Place Mutation Interaction

Ops with `may_runtime_uniqueness_check_args` mutate in place when the
checked argument's refcount is 1. Borrow inference helps these checks
succeed by deleting increfs that would otherwise hold refcounts above 1
during read phases, and early drop placement returns counts to 1 before
mutation points.

One interaction is accepted and documented rather than solved here: a borrow
whose lifetime extends past a uniqueness-checked mutation of its lender's
allocation forces the runtime copy path for that mutation. The solution is
still sound and still RC-minimal under the constraint system; it is the
constraint system itself that does not yet weigh mutation points. Extending
the flow analysis to account for `may_runtime_uniqueness_check_args`
positions when choosing between a borrow and an owned move is future design
work and must be added to the equations, not patched in emission.

### Field Takes From Dying Aggregates

A payload read pays a retain whenever its result must be owned, because the
container keeps its stored unit. When the container itself is about to die,
that retain is the difference between mutating in place and copying: the read
result carries count 2 into the mutation's runtime uniqueness check. Field
takes remove the retain by letting the read consume the dying container's
stored unit for that field, dismantling the container instead of releasing it
whole.

A container qualifies for dismantling when all of the following hold:

- its committed layout is a struct containing at least one refcounted field
- its binding is owned, bound exactly once, and is not a join parameter
- every occurrence of it is a field read (directly or through a borrowed
  pure same-value alias whose own occurrences are all field reads) or an
  operand-position whole use: moved into an aggregate or a call, returned,
  or join-carried by `set_local`

A proc parameter solved borrowed qualifies conditionally: its takes are
solved once against the shared ownership-neutral body but recorded as
owned-only, applying exactly in emissions whose demand vector overrides that
parameter to owned—the mode-specialized variants callers with dying
arguments select. Dismantle analysis outputs the exact per-procedure `u16`
parameter-benefit mask consumed by variant admission; the caller does not
rediscover the benefit from field reads or uniqueness checks. The base emission
keeps the borrowed schedule untouched.

Which consuming reads become takes is decided per field by a forward
dataflow from the container's definition over the control-flow graph,
tracking for each refcounted field whether it may and whether it must have
been taken on the paths reaching each point. A consuming read is a take only
where the field cannot have been taken yet—a take where it may already be
gone would double-consume its unit on that path. A borrow of the field, and
any whole use of the container, must likewise run where no take can have
happened: after a take, the container's bytes for that field can alias the
taker's mutation rather than the original value. Every exit the flow reaches—
returns, crashes, and jumps that leave the region, such as a loop's back
edge—must agree on the taken set (`may == must`), so the residual release
is the same however the death point was reached. Merges meet pointwise, and
a loop poisons its own takes: a take inside one reaches itself as
possibly-taken. Consuming reads on exclusive branches thereby take exactly
when every path through the branching takes the field exactly once—the
success and fallback arms of a checked mutation are the archetype—and
a read the flow never reaches keeps its field residual. A field that fails
any rule simply stays residual—its reads keep their retains and its stored
unit is released at the death point—and a container whose take set comes
out empty keeps today's whole release exactly.

Emission changes in exactly two places. A take's read emits no retain: the
result's unit is the container's stored unit for that field, moved rather
than duplicated. The container's release becomes its dismantling: at the
point its whole-value `decref` would have been placed, emission instead reads
each refcounted field that was not taken into a fresh temporary and releases
that temporary, using the field layout's helper and the container's
atomicity. Fields that were taken need nothing: their units continue in the
take targets, which are ordinary owned locals. Liveness, death placement, and
path balancing are untouched.

Like precise lifetimes, take solving is order-sensitive and therefore runs in
the ARC stage against the solved modes rather than inside the mode fixpoint.
It allocates per-candidate tables only: a container that cannot benefit --
wrong layout shape, borrowed, or non-operand whole uses -- contributes
nothing beyond its visit in one linear statement scan, preserving the rule
that ARC memory scales with ownership work actually demanded.

The certifier verifies takes from the emitted LIR alone, with no side tables,
by deferred claims. A field read still binds its result at balance zero, but
the result value remembers which container value and field it came from. When
such a value is consumed or released without a unit—where the certifier
previously failed outright—the consumption instead claims that field's
stored unit from the container, provided the container still holds its own
unit unconditionally and the field is unclaimed; a second claim of the same
field fails as before. Aggregate moves keep their transient-negative
discipline: negative balances attempt their claims when the path's outcome is
fixed, at a terminal's leak check or a jump's quotient. The container's
balance stays at one throughout—borrowed reads of its unclaimed bytes
remain legitimate after any claim—and a claim set covering every refcounted
field marks the unit spent: a terminal treats it as balanced and a jump's
carry check exempts it, while anything less fails as an unspent stored unit.
A claimed container can be neither consumed, moved into an aggregate, nor
released whole.
Claims and claim targets cross join quotients on the summary: owned entries
carry their container's claim set, and borrowed field-read entries carry
their container's representative and field so a claim deferred past a join
still lands.

Partial dismantling across diverging paths -- a field consumed in one switch
arm and not another -- is future work: it needs per-path residual masks, and
the spine rule above is precisely what makes the residual global. Until then,
the record-update lowering's spread-read hoisting is what keeps conditional
consumers in-place, by ending the container's liveness before the mutation
rather than dismantling it.

### Debug Borrow Certifier

Inference is implemented as a solver plus an independent certifier, because
RC misplacement is memory unsafety. Debug builds re-check every emitted proc
against the borrow typing rules:

- every owned resource is moved exactly once or decremented exactly once on
  every path, and never used after its move or drop
- every borrowed occurrence's lender is provably live at that point: the
  borrow's lifetime is contained in the lender's
- every join body holds under the entry state of each jump that reaches it:
  jump states are summarized over the names the body relies on (liveness,
  unit counts, alias partition, and borrow anchors) and joined into a
  forward dataflow fixpoint—summaries agreeing on every name's ownership
  mode share one abstraction whose must-alias partition is the meet of
  theirs (with per-fine-class balances re-attributed by constraint
  propagation), and the body is re-certified only when a jump strictly
  refines that abstraction. Mode disagreements split the abstraction along
  exactly the entry-state modes real in-edges disagree about, so refinement never
  manufactures entry states no jump produced; in the worst case it
  degenerates to one walk per distinct summary, with no capacity cap and no
  skip path. The join is monotone over a finite-height lattice (partition
  refinement is bounded by the name count; balance divergence across
  mode-identical entries is itself a finding—per-iteration accumulation),
  so certification of every procedure runs to completion
- explicit initialized-payload control flow refines conditional ownership:
  the initialized edge promotes the payload to ordinary owned state and the
  uninitialized edge removes its possible unit and binding. Presence
  conditions never survive an edge that has proved them true, so independent
  tests do not accumulate stale mode dimensions at later joins
- every call site satisfies the callee variant's signature, and every pinned
  signature holds

The certifier consumes only the emitted LIR and the stage-local signature
table, and leaves no unverified residue: every procedure is certified to a
fixpoint. The guaranteed property is that every emitted schedule balances
ownership on all paths—each unit released or transferred exactly once, no
use after death, no release of a borrow. A certifier failure is a compiler
bug and stops compilation. Release builds compile the certifier away
entirely, like every other debug-only boundary check.

Final-LIR unique-origin certification is proc-local. It collects each emitted
procedure's reachable statement inventory once and indexes only the
reference-counted locals in that procedure's explicit argument and
`frame_locals` spans. Specialized siblings may share source `LocalId`s, but
their definitions remain separate inventories. The certifier allocates no
store-wide statement or local bitset per procedure; one reusable store-local to
dense-proc-local table maps the explicit inventories into compact analysis
sets.

### Thread-Confined Reference Counts

Reference counts are atomic today because the host may share a Roc value
across threads. Roc code itself is single-threaded within one host call, so
an allocation needs atomic count updates only if a handle to it is ever
visible to the host: it flows into a hosted call, a root return, an erased
or address-escaped boundary—or it originated from one, as a root
parameter, a hosted-call result, or a payload read out of a host-visible
container. Every other allocation is confined to one thread for its whole
life, and its counts may use plain loads and stores.

Atomicity is a property of the allocation but is chosen per RC statement,
so every statement that can touch one allocation must agree. Agreement is
guaranteed by construction: host visibility is a may-property propagated to
a fixpoint over the complete value-flow graph, and two locals can only hold
the same allocation if a chain of those same flow edges connects them, so a
visible allocation marks every local that can hold it.

The analysis is one more monotone bit per local in the ARC solver, over
edges the solver already walks:

- seeds: parameters and returns of pinned procs (roots, hosted procs,
  erased-callable procs, procs whose address escapes)
- pure same-value aliases, in both directions
- containment, in both directions: aggregate and capture operands link to
  the constructed value, and payload reads link to their source—storing a
  visible value makes the container visible, and anything read out of a
  visible container is visible
- direct-call argument-to-parameter and return-to-result relations
- low-level ops, from explicit `RcEffect` data

Bidirectional containment keeps every reachable-value tree uniformly
visible or uniformly confined, so RC helper plans carry a single atomicity
flag rather than per-level flags.

`RcEffect` gains one more explicit mask, `result_shares_args`: the result
may contain handles into these arguments' allocations. Unit-accounting
masks already imply sharing for many ops (`result_aliases_consumed_args`,
`result_borrows_args`, `retain_args` all contribute edges directly), but
unit accounting does not describe handle sharing in general: `str_split_on`
allocates a fresh owned list whose string items are seamless slices into
the argument's allocation, and the byte/string conversions and
prefix/suffix slicing ops are the same. Those ops set `result_shares_args`
explicitly. A refcounted result of an op whose masks say nothing receives a
conservative edge to every refcounted argument in both directions: visible
spreads further than strictly necessary, which only keeps counts atomic
that could have been plain, never the reverse. The mask is explicit
primitive data, exactly like the rest of `RcEffect`; the analysis never
guesses an op's sharing from its name or shape.

Emission attaches the chosen atomicity to each `incref`, `decref`, and
`free` statement as explicit data; backends and the interpreter follow it
mechanically, and helper plans are selected by op, layout, and atomicity.
The runtime builtins already contain both count-update families. Atomic is
always sound, so the analysis only downgrades allocations it proves
confined, and an all-atomic answer reproduces today's behavior exactly.

Beyond cheaper count updates, confinement feeds the optimizer: atomic
operations are opaque to LLVM, but plain count updates participate in its
redundancy elimination, so residual paired increments and decrements that
ownership solving legitimately cannot remove become foldable downstream.
Confined data is also where `refcount == 1` in-place mutation hits most,
and its uniqueness check gets cheaper.

The debug certifier mirrors the analysis with one more rule: no
single-thread RC statement may name a local that is flow-connected to a
host-visibility seed.

### Uniqueness Inference

Ops with `may_runtime_uniqueness_check_args` branch at runtime: when the
checked argument's count is 1 they mutate the allocation in place, and
otherwise they copy. Borrow inference already deletes the RC traffic that
would hold counts above 1 across read phases; uniqueness inference goes one
step further and deletes the check itself wherever the in-place path is the
only one reachable. The win per site is one count load and one branch, but
the sites are the mutation points of hot loops, and removing the branch
also lets LLVM optimize across what was an opaque control split.

A join parameter is an ownership phi, not a foreign definition. Its origin is
born-unique exactly when it has at least one explicit non-self
`initialize_join_param` incoming edge and every such edge carries a born-unique
origin. An incoming edge consumes its source ownership unit; a second consuming
occurrence or a non-consuming read destroys uniqueness through that edge. Join
edges, pure same-value aliases, and unique-return call edges settle in one
monotone dependency graph, so loop back edges preserve a unique circulating
unit only when an explicit unique birth reaches the cycle. A self-assignment is
not an incoming ownership transfer and contributes no edge.

A checked argument's check is deletable when three conditions hold at the
call:

- the value's outermost allocation was born unique in scope: an allocation
  statement, or the result of an op whose `RcEffect` marks its result
  unique
- its count is still 1 on every path from birth to the call: no surviving
  incref, no store into an aggregate, no owned use other than the call
  itself
- no borrow of it is live at the call, under the same lender/holder
  liveness rule the certifier evaluates

The first two conditions are one more monotone bit per local in the ARC
solver—born unique, destroyed by any flow edge that can add a holder—
over the same alias and call edges the solver already walks. The third is a
query against liveness state emission already maintains.

`RcEffect` gains one more explicit mask, `result_unique`: the result's
outermost allocation has count 1 on return. Mutating ops qualify on both of
their paths—in place keeps an allocation whose count was already 1, and
the copy path returns a fresh one—and so do the ops that always allocate
their outermost result, including the slicing ops whose inner items
share (`result_shares_args` describes the inner sharing; uniqueness is a
property of the outermost allocation alone). As with the other masks, an op
without the mask contributes nothing and its results stay conservatively
non-unique; the analysis never guesses from an op's name or shape.

Interprocedurally, `RcSig` gains a unique bit on the return, solved
alongside `ret_mode` with the same pessimistic anchoring, and the mode
specialization demand vector gains a unique entry per owned parameter:
a call site that proves its dying argument unique may demand a variant
whose body elides the checks that parameter reaches. Dev and compile-time
builds stay single-variant and keep every runtime check, exactly as they
keep all-owned calls.

Emission lowers a uniqueness-checked op whose checked argument passes all
three conditions to the check-free entry of the builtin; helper plans are
selected by op, layout, atomicity, and uniqueness. The runtime check is
always sound, so the analysis only deletes checks it proves redundant, and
an all-checked answer reproduces today's behavior exactly.

The debug certifier mirrors the analysis with one more rule: at every
check-free mutation site, the checked value's unit balance is exactly 1,
its origin chain reaches a unique birth, and no borrow of it is live.

This sharpens the interaction documented under In-Place Mutation
Interaction: once the constraint system weighs mutation points when
choosing between a borrow and an owned move, the choice that keeps a
mutation check-free becomes visible to the solver rather than a lucky
outcome of emission order.

### In-Place List.map

`List.map` may overwrite a uniquely owned input list's buffer instead of
allocating an output list when the input and output item representations are
interchangeable in one allocation. Fully concrete items require the same
stride, allocation alignment class, and refcounted-items header shape.
Descriptor-governed items additionally require the same Boxy descriptor
representation identity and therefore the same descriptor behavior. Two
distinct dynamic types are not interchangeable merely because both commit to
the same pointer-sized `box_of_zst` layout: their descriptors can require
different payload layouts and RC header shapes. The hidden header in front of a
list's data and the alignment handed to the allocator both derive from this
explicit representation data, so reusing an allocation across incompatible
descriptors would make item drops or the final free reconstruct the wrong
allocation pointer.

The decision has a compile-time half and a runtime half. `List.map`'s body in
Builtin.roc first calls the consuming `list_map_prepare_reuse` primitive, then
matches on `list_map_can_reuse` for the returned list. The prepare primitive is
an ownership-only identity: its LIR `RcEffect` consumes the input list and
declares that the result aliases that consumed ownership unit, while its runtime
implementation only copies the list handle. This forces ARC to preserve every
later use before the transfer. The subsequent reuse query can therefore observe
the refcount only after all live ownership units are present; leaving the query
on the original, unconsumed argument would allow ARC to move a preservation
retain after that observation and incorrectly report a shared buffer as unique.

The runtime meaning of `list_map_can_reuse` is "uniquely owned and not a
seamless slice"—a slice's buffer points into the middle of an allocation
whose header bookkeeping covers the whole allocation, so a unique slice still
copies. At direct LIR lowering, where representations and layouts are committed,
the primitive lowers to a constant 0 whenever the representations are not
interchangeable (or the optimization is off), so the runtime check never runs
for a pair it could corrupt. Target-independent LIR carries this eligibility for
both pointer widths and each backend resolves the bit for its target.

The in-place branch itself is dropped before it reaches LIR whenever the item
representations are not interchangeable or the optimization is disabled
(`TargetConfig.list_in_place_map`, on for `--opt=size`/`--opt=speed`, off for
dev, interpreter, and compile-time evaluation), so ineligible map
specializations never carry dead in-place machinery and dev builds lower
exactly the copy loop. The fold uses the same representation-eligibility
decision as the primitive. Different fully concrete types may keep the branch
when their layouts are interchangeable; descriptor-bearing types may keep it
only when their descriptor representation identity is the same. The debug
Lambda Mono materializer runs before layout selection and cannot recompute that
decision; instead, direct lowering records each statically resolved match site
as explicit data and the verifier replays the record, so the two derivations
demand the same set of functions without the materializer ever consulting
layouts. A wrong record can only misplace dead code, never a runtime check—the
primitive's own lowering independently gates the runtime path—and a fold
regression surfaces as a Debug stride assertion in the backends rather than as
silent dead code.

Inside an eligible in-place loop, `list_map_extract_unsafe` moves one item's
ownership out of the buffer and `list_map_write_unsafe` moves the transform's
result into the vacated slot. Neither performs RC work: the extracted item is an
ordinary owned local, so ARC places its release according to the transform's
solved convention, and the certifier checks the loop like any other code.
Between the two ops the slot holds stale bytes and the buffer is typed by the
output item while later slots still hold input items. For
descriptor-governed items, the descriptor representation condition
guarantees one descriptor remains valid for both states. The window is otherwise
unobservable because no cleanup path walks live values—`crash` is fatal and
leaks by design—and the loop itself is the only holder of the buffer (the
runtime count of 1 proved there were no other counted handles, and a live borrow
of the list would have forced the copy path through an owned capture's incref).

### Destination-Passing Results and Allocation Reuse

Large result values should be lowered by destination demand rather than by
building a temporary value and then copying it into its final storage. A
destination demand is explicit LIR producer input, not backend policy. It
describes where a result should be written and which existing allocation, if
any, may be reused when ARC proves or checks uniqueness. Backends, the
interpreter, and LirImage consume the resulting LIR mechanically.

The direct LIR builder may create a small bounded set of result variants for a
proc:

- `return_slot(T)`: write a by-memory result into caller-provided `ptr(T)`.
- `reuse_box(T)`: consume `Box(T)` and use its payload storage as the result
  destination when uniqueness permits.
- `reuse_erased_callable`: consume an erased callable allocation and overwrite
  its function pointer, drop callback, and capture bytes when uniqueness and
  payload layout permit.
- `append_into(Str)` / `append_into(List(T))`: build a returned string or list
  by appending into a caller-provided unique accumulator.

These variants are keyed by proc id, result demand, and committed layouts.
Identical keys share one variant. Except for erased-callable entrypoints, root
procs and ABI-pinned procs keep their ordinary signature; wrappers may call an
internal destination variant. An erased-callable entrypoint always has the
uniform `(ops, ret, args, capture, reuse)` ABI described below, so every host and
compiled caller can explicitly opt into or out of the erased-callable result
destination without specializing the function-pointer type.

`return_slot(T)` is selected by layout representation, not by source syntax.
Scalar, pointer, and zero-sized result layouts keep ordinary returns.
By-memory result layouts may be emitted as `proc(out: ptr(T), args...) -> {}`
inside the LIR program. Existing ABI lowering remains responsible for adapting
that internal shape to whatever a target ABI requires at roots and host
boundaries.

`reuse_box(T)` models the common shape:

```roc
Box.box(f(Box.unbox(boxed)))
```

as an allocation-reuse operation over one consumed `Box(T)`. The operation's
RC metadata consumes the box argument, may runtime-check its uniqueness, and
returns the same outer allocation on the reuse path. ARC may set the statement's
`unique_args` bit when the runtime check is proven redundant by the existing
born-unique and no-live-borrow rules. When the check is not redundant, the
runtime check remains. If the box is not unique at runtime, the operation takes
the defined copy path and returns a fresh box. The payload move, replacement,
and old-payload release are all explicit in LIR or in the low-level operation's
documented RC effect; no backend may infer them from `Box` names or pointer
shapes.

The pre-ARC box-reuse proof recognizes both a direct producer call and a
producer that earlier specialization has inlined into one straight-line LIR
region. It follows only explicit `next` edges from `box_unbox` to the terminal
`box_box`/`ret`, requires identical committed box and payload layouts, and uses
proc-wide operand counts to prove that the consumed input box and returned box
have no consumers outside the rewrite. Control-flow regions or additional box
consumers are rejected rather than classified from source shape or names.

`reuse_erased_callable` is the erased-callable counterpart. Erased callables are
not ordinary `Box(T)` payloads; their allocation stores a callable entry, an
optional drop callback, and inline capture bytes. Reuse is allowed only when:

- the old erased callable is consumed and unique, or its runtime uniqueness
  check succeeds
- the new callable payload has the same committed payload size and alignment
  class as the old allocation, in the initial design
- the old capture payload is released by the old drop callback before the
  allocation is overwritten
- the new callable entry, new drop callback, and new capture bytes are written
  before the result is returned

The first implementation should require same-size/same-alignment erased
callable payloads. Broader reuse across different capture layouts requires an
explicit capacity or size input; it must not be guessed from the erased function
type alone, because an arbitrary `Box(a -> b)` value does not identify the
stored capture layout.

Every erased-callable function pointer has five arguments: `ops`, caller-owned
result storage, packed arguments, borrowed capture bytes, and a nullable reuse
data pointer. The fifth argument is an ownership-transfer channel, not a second
borrow of the capture. Null transfers nothing. A non-null value must be the
data pointer of the erased-callable allocation that contains the borrowed
`capture` bytes; unrelated erased-callable allocations cannot be reused because
the function pointer does not carry their capture size or alignment. Non-null
transfers exactly one owned reference to that allocation, and the caller must
not use or decref that ownership unit after the call. On every normal return
path the callee consumes the unit exactly once: it either moves the allocation
into the single statically selected erased-callable result slot (repacking in
place only when uniqueness permits) or decrefs it. This remains true for a
runtime tag variant that does not contain a callable. A shared allocation takes
the fresh allocation path and decrefs the transferred reference; uniqueness
affects only whether allocation can be avoided, never the ownership contract.
Capture bytes needed to construct the result are snapshotted before an in-place
overwrite.

The packed arguments are one fixed-arity struct whose fields remain in source
parameter order and use each runtime representation's natural alignment. The
struct size is rounded up to its maximum field alignment. Post-check lowering
computes and interns the exact field offsets, size, and alignment as an erased
call argument plan. Every erased call and erased-callable procedure names such
a plan, and LIR certification verifies that the plan exactly matches the call
arguments or explicit procedure parameters. Backends consume these offsets
directly for both packing and unpacking; they must not reorder arguments, round
individual fields to backend-private slots, or reconstruct the plan.

The direct LIR builder carries the current return destination while recursively
lowering the selected aggregate or tag payload. That producer-authored context,
plus the explicit result-demand classification, is the only basis for selecting
an erased return-reuse specialization. Later stages must not scan emitted LIR to
reconstruct whether a local eventually flows to a return. A proc with this
hidden ownership input records its exact argument local in
`LirProcSpec.erased_reuse_arg`; every erased-callable entry records that marker
even when its result has no reusable callable slot, because a non-null transfer
must still be decrefed exactly once. Transforms that clone proc arguments must
preserve and remap the marker. Debug LIR certification verifies that every
erased-callable proc's hidden capture and reuse arguments, the reuse argument's
layout, and the ownership marker remain structurally consistent.

An owned erased call records both the callable local used to load the function
and capture pointers and the local whose ownership unit is transferred. Those
locals must denote the same erased-callable allocation. The ownership local may
be an outer nominal or zero-discriminant tag wrapper only when the emitted
`assign_ref` chain proves exact pointer representation at every edge. Debug LIR
certification derives that exact allocation identity from those explicit
producer operations and rejects a call that would pass one allocation to the
machine ABI while consuming another in ARC.

Destination-aware aggregate construction is required for the full benefit of
box reuse. A record update or tag construction whose result is demanded in a
slot should write fields and discriminants into that slot rather than first
forming a whole temporary aggregate. If the destination aliases a consumed
input, lowering must preserve read-before-overwrite order: fields needed later
are moved or copied to temporaries before their slots are overwritten, and every
refcounted field is moved, retained, or released exactly once. This ordering is
part of LIR construction and ARC emission, not backend cleanup.

Append destinations are result demands for producer functions that return
`Str` or `List(T)`. Under `append_into(Str)`, string literals, string slices,
string concatenations, and direct calls to append-capable producers write into
the supplied unique string accumulator. Any expression that cannot append
directly is first lowered to an ordinary result and then appended as an
explicit step. `append_into(List(T))` follows the same rule for list builders.
These variants are created only for realized demands and are keyed by result
kind and item layout, so specialization is bounded by the distinct demands
the program actually uses.

Each stage fully replaces the previous behavior when it lands; there are no
parallel insertion paths at any point:

1. Certifier first, checking the current all-owned insertion output.
2. Intraprocedural inference: borrows for locals, payload reads, and
   low-level ops (including `result_borrows_args`), with every proc `RcSig`
   pinned all-owned.
3. Interprocedural `RcSig` solving over call-graph SCCs, single variant per
   proc.
4. Mode specialization in optimized builds.
5. Thread-confined reference counts: the host-visibility analysis, the
   `result_shares_args` audit of the low-level op table, dual-mode RC
   statements and helper plans, and the certifier rule.
6. Uniqueness inference: the born-unique bit, the `result_unique` audit of
   the low-level op table, the unique entries in `RcSig` and the
   specialization demand vector, check-free helper plans, and the certifier
   rule.

## Dev Backend Register Lifetimes

`LirCodeGen` is the sole authority for LIR local locations. Every assigned
LIR local has one authoritative stable location. Ordinary represented values
use stack slots. A first-class 128-bit integer vector may instead occupy a
`vector_reg` location for its straight-line live range. `LirCodeGen` owns the
live range, register assignment, spill slot, and every transition between the
two; architecture-specific code generators own only the one
floating-point/vector register-allocation mask and instruction encoding. They
do not own a second local-location map or independently move LIR local values.

Floating-point and vector locations draw from the same register-allocation mask:
XMM registers alias on x86-64 and V registers alias on AArch64. Vector locals
remain in registers across chained operations and spill only under actual
register pressure, at control-flow environment boundaries, or before a call
whose ABI may clobber them. SysV x86-64 treats every XMM register as volatile.
Windows x64 allocates only its volatile XMM subset unless full-width save and
restore is emitted. AArch64 does not treat V8-V15 as a persistent 128-bit
vector resource because its ABI preserves only their low 64 bits; a live Q
value is spilled before a call.

The number of simultaneously live instruction-selection temporaries must be
bounded by the emitted operation, never by source or layout nesting depth.
Explicit emission worklists carry stack offsets and reuse their
caller-provided result register across child continuations. They must not retain
one newly allocated architecture register per recursive layout, control-flow node,
list item layer, or tag payload layer. Register-pool exhaustion is therefore
an internal lifetime-invariant failure, not a source-program condition and not
an invitation for an architecture-specific best-effort spill.

## Compile-Time Constants

Compile-time constants use the existing checked-finalization pipeline while a
checked module is being finalized. This path is unaffected by the runtime
`--specialize` flag:

```text
checked CIR
  -> CheckedModuleBuilder during checking finalization
  -> Monotype IR
  -> Monotype Lifted IR
  -> optional SpecConstr
  -> Lambda Solved IR
  -> solved inline plan
  -> direct Solved-to-LIR decisions
  -> LIR
  -> ARC insertion
  -> native dev backend on native compiler hosts
  -> store eval result in ConstStore
```

On native compiler hosts, every `compile_time_*` root uses the dev backend for
compile-time evaluation: ordinary constants, selected hoisted constants,
expects, callable eval roots, numeral conversions, and quote conversions. The
compile-time evaluator does not interpret Monotype IR, Lambda Solved IR,
logical Lambda Mono expressions, or any source-level IR.

The only interpreter path is for compiler hosts that cannot run generated native
code, currently wasm32 and freestanding compiler builds. This decision is made
from the compiler's own build target. The Roc program target selected by
`roc --target` does not affect the compile-time evaluation strategy. A native
compiler host without host dev-backend code generation support is unsupported for
compile-time evaluation; it must not silently use the interpreter.

Diagnostic behavior must be identical between the dev-backend evaluator and the
host-restricted LIR interpreter. A mismatch in crashes, expect failures,
empirical exhaustiveness diagnostics, branch coverage, literal conversion
diagnostics, or stored constants is a compiler bug in one of the evaluators or
in the data they consume.

Runtime `.boxy` builds do not make compile-time roots use boxy lowering.
Compile-time finalization continues to lower the requested compile-time roots
through the existing Monotype/Lambda/LIR path, interpret the result, and store
checked values in `ConstStore`. This keeps `roc check` and compile-time
evaluation independent of runtime backend and specialization choices.

Compile-time ARC insertion runs the same borrow-inference solver as runtime
ARC insertion in its single-variant form: one proc per solved `RcSig`, no
mode specialization. Compile-time evaluation pays for solving once per
evaluated root and never for variant cloning.

The evaluator produces a runtime value. Checking then stores that eval
result as checked-stage data in the checked module's `ConstStore`. `ConstStore`
stores checked Roc values only. It does not contain Monotype nodes, Lambda
Solved data, Lambda Mono decision data, runtime addresses, allocation identity,
layout ids, runtime discriminants, field offsets, LIR locals, LIR procedure
ids, backend symbols, backend bytes, or host handles.

Compile-time finalization evaluates dependency-ready roots in batches. Each
batch is lowered to LIR once, ARC is inserted once, the dev backend emits native
code for the reachable proc specs, and a generated wrapper is emitted for each
root. Roots in the same batch are independent pure computations and may run on a
work queue. The finalizer still commits stored values, diagnostics, coverage
updates, and root completion in sorted request order.

Each root job owns its compile-time host state: `RocOps`, a root-local arena for
Roc allocations, allocation tracking needed by RocOps, branch-hit records,
expect/dbg/crash event lists, failure region state, a call-region stack, a crash
boundary, and its result buffer. Roc runtime allocations are arena allocations
and are bulk-freed after the result has been copied into `ConstStore`. Host
events and failure state stay root-local until deterministic replay.

A crash or empirical exhaustiveness failure in one root records that root's
result and must not change the result of any other root in the same batch. Root
jobs do not write diagnostics, stderr output, checked problems, or `ConstStore`
entries directly. They write root-local event lists and branch-hit data; after
all jobs in the batch finish, checking finalization replays those lists in the
sorted root order.

Slow-root progress reporting observes the same root job state without changing
evaluation. By default, a root that has been running for more than three seconds
may be reported periodically on both TTY and non-TTY stderr. The message names
the one-line source snippet for the root, truncated with `…` if needed, plus the
module, line, column, and elapsed seconds. If the root belongs to a binding, the
snippet should include enough of that binding line to identify the binding.

Progress reporting must not add a fixed latency penalty to fast compile-time
evaluation. A reporting worker may be started when stderr/std_io reporting is
configured, but every wait in that worker must be interruptible by the
finalizer. The worker waits for the earliest running root's slow-report
deadline, normally `root_start + 3s`, or for a stop signal, whichever comes
first. When a root starts, the worker is signaled so it can recompute the next
deadline without polling. When finalization finishes, it signals the worker and
joins it immediately; it must never wait for a plain sleep interval to expire.
Roots that finish before the slow threshold therefore pay only monitor setup and
signaling overhead, not the reporting period. Roots that exceed the threshold
are reported, then the worker waits interruptibly until the next per-root report
deadline or finalization stop.

## Optimized Test Execution

`roc test --opt=size` and `roc test --opt=speed` use one command-level test
plan for all discovered test roots in the app and its imports. The plan is
explicit compiler data. It owns the stable source-order display order, the
checked module owner for each root, the source region used for reporting, the
checked cache id for the root's result, the generated test symbol for uncached
roots, the cached or uncached execution decision, and the result slot. Later
stages consume this plan directly; they never reconstruct test identity from
source paths, symbol names, module traversal order, filesystem order, package
cache paths, or backend output.

Optimized test execution performs at most one LLVM shared-library link for the
whole command for the selected optimization mode and native target. It must not
link one shared library per checked module. The per-module checked cache
boundary remains intact, but cache boundaries are not native link/load
boundaries for optimized tests. Cached test results are merged into the command
result stream before reporting; uncached test roots are compiled into the single
test library and written back to their original checked-module cache entries
after execution.

The command-level plan is built after checking has produced all checked modules
and before any optimized test backend code is generated:

```text
checked modules
  -> command-level test plan
  -> cached-result admission for plan entries
  -> one command-level lowering batch containing every uncached test root
  -> ARC insertion for each checked-module lowering result
  -> per-module LLVM bitcode with exported test entrypoints
  -> one merged LLVM module/object
  -> one native shared library
  -> parallel test-root calls
  -> deterministic transcript and result merge by test-plan order
  -> checked cache writes for fresh test results
  -> final timing summary
```

The user-visible per-test transcript is deterministic. It contains `dbg`
output, failed-expect output, crash diagnostics, and per-test status or failure
rendering. It ends before the final aggregate timing summary. That boundary is
the byte-for-byte contract: for the same source code and the same renderer
configuration, captured stdout bytes and captured stderr bytes in this
transcript are independently byte-for-byte identical regardless of selected
optimization mode, backend, worker count, core count, scheduling, test
completion order, or whether a result was read from cache. The selected
optimization mode is not checked source input and is not a renderer configuration;
it must never change test output data or rendered transcript bytes. A different
per-test transcript under `--opt=size` and `--opt=speed` is a compiler/backend
bug, not a cache-key distinction. Changing color mode, verbosity, terminal
width, or another renderer setting may change only the rendering chosen from
the same structured transcript data. Determinism tests compare each stream's
transcript bytes directly; they do not normalize source snippets, whitespace,
ANSI escapes, or cache labels inside the transcript. The final aggregate
summary is a run epilogue and is outside the byte-for-byte transcript
guarantee; it may include elapsed time and cache/timing annotations for this
invocation. Cache-hit labels such as `(cached)` belong only in that epilogue,
never in the per-test transcript.

The command-level lowering batch may name roots from multiple checked modules.
Each per-module lowering request is built with explicit imported checked-module
views and relation views owned by that checked module. The batch is the
command-level data that ties those per-module lowering outputs back to one test
plan. Each lowered test root carries root metadata that identifies its
test-plan slot and the original checked module root order. LIR results do not
use symbol text as identity. Symbol text is only the exported backend name
needed to locate the entrypoint in the loaded test library.

Every optimized test entrypoint uses the compiler-internal test ABI, not the
public host symbol ABI. The entrypoint receives `RocOps`, a pointer to a
test-invocation context, the return buffer, and the argument buffer. The
invocation context stores mutable observation output produced by generated test
code that cannot live in `RocOps`; currently this includes the `expect_err`
source region. The LLVM backend must not use a shared exported global such as
`roc_expect_err_region` for optimized tests, because a command-level test
library runs multiple roots in parallel.

After the single library is loaded, test roots run on a worker pool. Each root
call owns its `RuntimeHostEnv`, `RocOps`, allocation tracker, crash boundary,
argument buffer, return buffer, test-invocation context, and result slot. The
loaded code and immutable static data are shared across workers. Mutable
observation state is per invocation. Generated test code must be reentrant with
respect to test-root calls: a root call must not write process-global state
except through explicitly thread-safe runtime services or the invocation data
passed to that root.

Workers never write test output, diagnostics, stdout, stderr, or cache files
directly. They send structured transcript events and final root status data to a
command-level output coordinator. Each event carries the test-plan slot, the
logical stream, the event kind, and structured payload data. Final root status
data carries the source region, failure detail, and visibility data needed to
render per-test status or failure reports. The coordinator is the only writer for
user-visible test transcript output.

The coordinator owns `next_to_print` and per-entry buffers. Events for
`next_to_print` are rendered and written immediately, so a slow earliest test
can show `dbg` output in real time. Events for later tests are buffered in that
test's own entry. When `next_to_print` finishes, the coordinator advances
through the longest contiguous prefix of already-finished entries, flushing each
entry's buffered transcript and result before moving to the next entry. If a
running buffered test becomes `next_to_print`, its buffered prefix is flushed
and later events from that test are written live. This produces sequential
plan-order output while still allowing parallel execution. If a buffered
transcript is too large to keep in memory, the coordinator may spill it to a
command-owned temporary file in the command's isolated temp directory; it must
not write transcript spill files into user source directories.

Worker completion order is never user-visible. Results are merged by the
command-level test-plan order: checked module source order first, then checked
test root source order within each module. Cached results and freshly-run
results use the same ordering path. Diagnostics, `dbg`, `expect` failures,
`expect_err` regions, crashes, and backend compiler errors stay attached to
their test-plan entries until rendering and cache writes replay the plan order.

Checked test-result cache entries use the same checked module cache identity
as `roc check`. There is no second test-result cache key.
The test-result bundle may carry its own payload format and compiler-version
admission data, but those fields decide whether the decoded bundle can be used;
they do not extend the checked module cache identity. The result cache key does not
include selected optimization mode, backend, worker count, color mode,
verbosity, terminal width, terminal style, elapsed time, final summary text,
test-run completion order, test-root identity, or test-root order. Test-root
identity and order are explicit checked module data and payload shape, not
additional cache-key inputs. Cache reads validate that a decoded test-result
bundle matches the current checked module's test-root shape and cache format
before admitting it, but this is payload validation rather than a new source of
identity.

Cached test results store structured, pre-render data: the result, ordered
transcript events, source regions, failure data, and raw or structured Roc
payloads needed to render `dbg`, failed expects, crashes, and per-test
status/failure reports.
This cache format predates terminal rendering. It stores enough information to
render the current stdout/stderr transcript just in time, under the current
renderer choices, but it does not store terminal bytes, ANSI escapes, color
mode, verbosity, terminal width, elapsed time, cache-hit labels, or final
summary text. A cached result is represented to the output coordinator as an
already-finished plan entry containing the same structured transcript events
and test status data that a fresh run would have sent. Checked test status data,
including passes, failed expects, and Roc runtime crashes, may be cached.
Compiler backend failures while building or linking the command-level image are
not checked test status data and are not written as successful test-result cache
entries.

If compiling or linking the command-level test library fails, each uncached root
in the plan receives a backend compiler-error result at its original source
region. If one root crashes while running, that root records a failed result
from its own invocation state. Other roots continue unless the process receives
a hard fault that escapes the crash boundary.

The interpreter and dev backend may consume the same command-level test plan,
but they are not allowed to change the result contract. The interpreter may run
roots serially. The dev backend may compile one callable batch and then execute
roots through the same per-invocation state model. Backend choice changes only
how entrypoints are produced, not how tests are identified, ordered, cached, or
reported.

`ConstStore` uses node ids so stored constants can preserve sharing without
duplicating large values. Multiple fields and function captures may reference
the same `ConstNodeId`. The store is an exact directed `ConstStore` graph: a recursive
runtime value therefore keeps the same back-edge that exists in the evaluated
value instead of replacing it with source-level identity.

Roc source cannot define an eager recursive non-function value; checking
reports those definitions as errors and records `Malformed` source nodes
instead. Recursive values delayed through a function are valid. Accordingly,
every cycle in a completed `ConstStore` graph must cross at least one
`ConstFn -> ConstCapture.value` edge. A cycle made only from eager value edges
is a compiler bug. `Malformed` source nodes are never output as valid
`ConstStore` values.

```zig
const ConstStore = struct {
    values: []const StoredValue,
    fns: []const ConstFn,
    node_pool: []const ConstNodeId,
    capture_pool: []const ConstCapture,
    string_bytes: []const u8,
    string_data: []const Span,
};

const ConstValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    string: ConstBlobView,
    list: union(enum) {
        nodes: Span(ConstNodeId),
        scalar_bytes: struct {
            bytes: ConstBlobView,
            len: u32,
            item: ConstPackedScalar,
        },
    },
    tuple: Span(ConstNodeId),
    record: Span(ConstField),
    tag: ConstTag,
    box: ConstNodeId,
    tuple: []const ConstNodeId,
    record: []const ConstNodeId,
    crash: ConstStr,
    tag: struct {
        tag_name: []const u8,
        payloads: []const ConstNodeId,
    },
    nominal: struct {
        named_type: NamedType,
        backing: ConstNodeId,
    },
    fn_value: ConstFnId,
};
```

The serialized store uses POD `StoredValue` ranges into the flat pools. The
public read form reconstructed by `ConstStore.get` is `ConstValue`; its list,
tuple, record, and tag-payload fields are slices.

`ConstStr` is a view into checked-module string backing data:

```zig
const ConstStr = struct {
    data: ConstStrDataId,
    offset: u32,
    len: u32,
};
```

`ConstBlobView` is an `(interned blob id, byte offset, byte length)` view.
Strings and packed scalar lists intern into the same exact-content namespace.
Packed multi-byte scalars use little-endian checked-value bytes;
this is target-independent `ConstStore` data, not a captured host allocation or
backend-owned byte sequence. Post-check lowering validates the byte count
against the scalar layout before making a runtime static-data view.

`ConstScalar` is a closed checked scalar representation:

```zig
const ConstScalar = union(enum) {
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    i128: i128,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    u128: u128,
    f32_bits: u32,
    f64_bits: u64,
    dec_bits: i128,
};
```

`ConstScalar` does not contain target-sized integers, raw pointers, opaque
pointers, host handles, layout ids, field offsets, runtime discriminants, or
backend symbols. `bits` stores the checked literal bits for the declared width;
consumers interpret it only through the checked type attached to the const root.
If Roc later exposes pointer-sized numeric values to compile-time evaluation,
that value kind must be added explicitly here with a checked cache rule.

`ConstStore` may contain:

- scalar literals
- strings, lists, records, tuples, tags, boxes, and nominals
- references to other stored const nodes
- function values

Compile-time evaluation failures are owned by checking finalization because the
module has not been output yet. User-written compile-time crashes, invalid
compile-time host interaction, and unsupported compile-time operations become
checking diagnostics attached to the checked root being finalized, and the
root's `ConstStore` entry is completed with an explicit crash constant. Sibling
roots continue finalizing. OOM remains OOM. A post-check invariant failure while
lowering or interpreting a compile-time root is still a compiler bug, not a
user-facing diagnostic.

While storing an eval result, the `ConstStore` writer reserves a `ConstNodeId`
and inserts the runtime-address-to-node memo before storing its children. Repeated
references and back-edges therefore resolve to that exact node. The builder
verifies that every reserved node was filled exactly once and that every cycle
crosses a delayed function-capture edge. Const materialization and dependency
summarization must use explicit graph traversal state keyed by `ConstNodeId`, so
sharing is preserved, cycles terminate, and traversal is linear in the stored
graph size. A consumer must not recover stored-const identity by comparing node
contents or by reconstructing source-level recursive bindings.

A stored function value keeps checked identity only:

```zig
const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
    captures: Span(ConstCapture),
};

const CaptureId = union(enum) {
    binder: PatternBinderId,
    generated: u32,
};

const ConstCapture = struct {
    id: CaptureId,
    ty: ConstTypeId,
    value: ConstNodeId,
};
```

`fn_def` names a checked, imported, nested, hosted, promoted, or checked-stage
generated procedure template that the checked module owns or references
explicitly.
`captures` bind the exact capture identities required by that function to
stored const nodes. `ty` is the exact target-independent runtime representation
of the captured value. The temporary LIR function-result metadata separately
provides each capture's explicit payload layout and storage mode; when a
recursive slot uses box storage, the writer memoizes that box address before
following it and stores the pointed-to value under the capture's ordinary value
type. Source lambdas use
checked pattern binders. Compiler-generated functions whose captures have no
source pattern, such as structural parser runtime functions, use explicit
generated capture ids assigned by the generator. Capture identity selects the
checked template binder; it is not a substitute for value identity and is never
used to infer a graph back-edge. A stored function does not store a lambda set,
callable-set descriptor, call specialization id, erased ABI, capture layout,
runtime tag, or LIR proc id.

During compile-time evaluation, the direct LIR builder also produces temporary
function result-store data. Storing a function result is scoped by `FnSet`
identity, not by layout alone. Layouts may
collapse zero-sized or same-shaped function values; they are used only for
validation, discriminant reads, and capture extraction.

```zig
const FnResult = union(enum) {
    finite: FnSetId,
    erased: ErasedFnsId,
};

const FnSet = struct {
    layout: LayoutId,
    variants: Span(FnVariant),
};

const FnVariant = struct {
    id: FnVariantId,
    discriminant: RuntimeDiscriminant,
    variant_index: RuntimeVariantIndex,
    payload_layout: LayoutId,
    template: FnTemplate,
    captures: Span(CaptureSlot),
};

const ErasedFns = struct {
    layout: LayoutId,
    entries: Span(ErasedFn),
};

const ErasedFn = struct {
    entry: LirProcSpecId,
    template: FnTemplate,
    captures: Span(CaptureSlot),
};

const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: CheckedTypeId,
};

const CaptureSlot = struct {
    id: CaptureId,
    slot: u32,
    ty: ConstTypeId,
    plan: ConstPlanId,
    storage: enum { value, recursive_box },
};
```

`FnSetId` and `ErasedFnsId` are direct-builder result contexts produced while
lowering the specific value being evaluated. They live only for that lowering
and const storage step. They are not stored in `ConstStore`, not serialized
in checked modules, and not stored in `LirImage`. For a finite singleton set,
storing the result selects the only `FnVariant`. For a finite multi-variant set,
storing the result reads the runtime discriminant and looks it up inside that
`FnSet`. For erased functions, storing the result reads the erased entry
procedure from the runtime value and looks it up inside the explicit
`ErasedFns` context.

`CaptureSlot` says which committed capture-payload slot contains the value for
one captured identity. For source lambdas, the identity is the checked binder.
For generated functions, the identity is a generator-assigned capture id with a
documented role in that generated function kind. The direct LIR builder outputs
these slots while lowering the generated function value. The `ConstStore`
writer recursively stores each captured runtime value, then stores the
resulting `ConstFn`.

Storing an eval result never uses a global id made only of layout,
discriminant, variant slot, byte pattern, display name, object symbol, or
payload shape. The function result-store data is temporary and is not serialized
as a checked module representation cache.

When a later compilation materializes a cached const, Monotype lowering turns
`ConstStore` nodes into ordinary Monotype expressions:

- scalar and aggregate nodes become literal, record, tuple, list, tag, box, and
  nominal expressions
- zero-capture function values become `FnDef` expressions
- capturing function values become compiler-generated Monotype lambda
  expressions by cloning the checked template body referenced by `function`,
  alpha-renaming its parameters, and binding each captured symbol to the
  ordinary Monotype expression materialized from the corresponding captured
  `ConstNodeId`
- generated parser runtime functions materialize through their explicit generated
  function kind: Monotype lowering recovers the checked static-dispatch plan,
  materializes generated captures such as transformed field-name strings by their
  generated capture ids, and regenerates the runtime parser lambda directly
  around those materialized constants

Materialization records an active `(module, ConstNodeId, representation)` before
descending into that node. The first exact graph back-edge reserves and uses a
local; once the node is complete, Monotype lowering emits the ordinary
recursive binding that owns it. Acyclic nodes allocate no such local. Completed
nodes are memoized in the materialization context. This is graph
materialization, not recovery of a discarded source binding: the checked module
data already contains every value edge needed by the operation.

Materializing a cached const does not synthesize a wrapper that calls an
already-packed runtime function value. It builds an ordinary Monotype callable
from the explicit checked template and stored const captures, so the later
lifting and lambda-set solving stages see the same kind of ordinary callable
flow they would have seen if the value had been local source.

When `.boxy` runtime lowering restores a cached non-function const, it does not
create Monotype expressions. It reads the stored `ConstStore` node, interprets
it under the checked type attached to the `ConstUseTemplate`, and emits ordinary
LIR construction statements directly: literals for scalar and string views,
aggregate construction for lists, tuples, records, tags, booleans, boxes, and
nominals, and `crash` for stored compile-time crash values. The store module
owns the const nodes and string backing data; the const use's type module owns
the checked type payloads used to pick layouts and field/tag order. Boxy
restoration therefore preserves imported type identity without projecting
stored values into the root module. Stored `fn_value` nodes are restored only by
the erased callable path described above, never as a raw aggregate lookup.

After that, closure lifting, lambda solving, lambda mono lowering, layout
commitment, and ARC run normally. This is what keeps module boundaries from
changing runtime performance: imported compile-time callables participate in the
same whole-program callable-flow solving as local callables.

Compile-time dependency summaries are produced from explicit checked root data
and `ConstStore` dependencies. They are not discovered by a later stage scanning
bodies for missing data.

## LirImage And Hosted Functions

Platform-hosted functions called through `RocOps.hosted_fns` receive ownership
of every refcounted argument. LIR ARC insertion transfers that ownership at the
hosted-call boundary the same way it transfers an unused argument to an
ordinary Roc callee. Backends must not add their own ownership decisions; they
only lower the explicit LIR `incref`, `decref`, and `free` statements.

The host may read, store, return, or release an owned argument. It must account
for that ownership explicitly:

```text
read and discard: decref the argument when done
store past return: move the argument ownership into storage, or incref a stored
                   copy and then decref the call argument
return the same value: move the argument ownership into the return slot, or
                       incref/decref so exactly one returned ownership remains
```

The compiler must not infer different ownership behavior from hosted function
names, return types, or body absence. Hosted-argument ownership is an ABI rule,
and generated glue must document it for platform authors.

LirImage stores the already-lowered platform entrypoint table. For `.boxy`,
those entrypoints are the host-shaped wrapper procs, not private boxy workers.
The image may also store LIR-owned boxy descriptor and dictionary tables if
reachable LIR statements reference them. Those tables are internal interpreter
data; they do not add fields to platform entrypoint signatures or hosted
function signatures.

LirImage's on-disk and shared-memory form does not serialize
`SafeMultiList`'s private allocation layout. Struct-field and tag-variant tables
are stored as explicit typed columns. A mapped `ProgramView` or Boxy sidecar
view leaves ordinary arrays in place, reconstructs target-native compact
columns with its supplied scratch allocator, and owns those reconstructed
columns until `deinit`. The mapped bytes and the scratch allocator must both
outlive the view. Format version 15 introduced the portable columns; version 16
added `LirProcSpec.ret_desc`.

Hosted proc entries keep their exact checked hosted ABI in both strategies. A
boxy caller adapts arguments before the hosted call and adapts the result after
the hosted call. It must not change the hosted dispatch index, hosted symbol
name, natural C ABI signature, ownership rule, or generated glue declaration.

## Build Outputs And The Targets Header

The `check`, `build`, `run`, and `test` command family resolves its final process
status only after completing all requested independent work. Any check-phase
error makes the final status 1. Otherwise, warnings make the final status 2 and
a clean check leaves status 0 unless the command's own result requires failure.
Checking diagnostics do not prevent `roc build` from writing its requested
output, and they do not prevent `roc test` from running independent test roots.
One-shot build pipelines return the linked output path and explicit checking
diagnostic counts to command orchestration; code generation and linking do not
decide process status.

A platform's `targets:` header section declares, per target, both the link
inputs and the output kind the build produces. The application author never
chooses the output kind; `roc build` produces what the platform declares for
the selected target, and there is no `--no-link` style flag. `--target` and
`--output` (the output path) remain per-build choices.

```text
targets: {
    inputs_dir: "targets/",
    arm64mac: { inputs: ["libhost.a", app], output: Shared },
    x64glibc: { inputs: ["libhost.a", app], output: Exe },
    wasm32: {
        inputs: ["host.wasm", app],
        output: Shared,
        exports: ["start", "update"],
    },
}
```

Each target name appears at most once, so target-to-output-kind is a
function. `output:` is one of:

```text
Exe:     linked executable binary. For wasm32, a command module (has an
         entry).
Archive: one static archive (.a, .lib) containing the declared host inputs,
         the compiled app, and the builtins, with input archives flattened
         in. Archive keeps its inputs because the host must provide
         roc_alloc and the other runtime symbols; the consumer receives a
         single self-contained archive and performs the final link in their
         own build, which extracts members lazily by symbol reference.
Shared:  shared library (.so, .dylib, .dll). For wasm32, a reactor module:
         no entry, the provides entrypoints exported, optionally composable
         into a component with wit-component.
```

The default `roc` command requires the selected target's entry to be
`output: Exe`; library and object platforms report that the output must be
linked or loaded by a host application instead.

The output that static archives previously stood in for on wasm (a linked,
loadable, no-entry module) is `Shared`, not `Archive`; `Archive` is never a
linked module.

For wasm targets, `exports:` is the complete final host-visible function ABI.
Every named function is a link root and is emitted in the module export
section; no other host function becomes public. An explicitly empty list means
that the final module has no exported functions. Omitting the field preserves
compatibility with older platforms by exporting the public function symbols
found in their wasm object inputs. New platforms should always declare the
field so object visibility cannot accidentally enlarge the final ABI or retain
link-only code.

After the final wasm link, size builds run Binaryen at optimize level 2 and
shrink level 2, validate the resulting module, and remove debug, producer, and
target-feature custom sections from non-debug output. Removing the
target-feature custom section does not alter the wasm code section; Binaryen
validates the module after the metadata is removed. The removed custom section
is not part of the runtime ABI. Debug builds retain debugging and
target-feature metadata.

## Host Symbol ABI

Hosts and compiled Roc code share symbols resolved at link time; there is no
host-facing struct of function pointers. `RocOps` survives only as an
interpreter-internal structure (the dev-build translation shim and
compiler-internal evaluation construct one); it is not part of any host ABI,
and glue never emits it.

The platform header maps linker symbols explicitly, symbol-string first, in
both directions:

```text
provides { "roc_main": main_for_host! }
hosted { "roc_stdout_line": Stdout.line!, "roc_stderr_line": Stderr.line! }
```

The symbol string is the identity of an externally-bound function. A hosted
call resolves to the entry at its declaration slot in the `hosted` section;
resolution never matches hosted declarations by signature, by
declaring-module content, or by any content-derived module identity. Two
byte-identical modules whose effectful declarations are wired to different
symbols stay distinct because the platform header that assigns those symbols
is the data that separates them. `provides` follows the same rule: the
exported symbol set is part of the platform relation, and two exports remain
two exports even when they name the same Roc function.

Compiled Roc code references each hosted symbol (and the fixed runtime set:
roc_alloc, roc_dealloc, roc_realloc, roc_dbg, roc_expect_failed, roc_crashed)
as a weak extern and calls it directly with the natural C ABI for its types.
Entrypoints are exported under their `provides` strings with natural C ABI
signatures. No context pointer is threaded through compiled code: hosts that
need per-call context (for example an arena) own its delivery out of band
(global or thread-local state), and must establish it on every thread that
executes Roc code—including threads that invoke stored boxed Roc closures.
Generated glue exposes closure invocation through helpers that set and restore
that state so the contract is enforced by signatures rather than remembered.

The host symbol ABI is identical for `.lss` and `.boxy`. Host-facing signatures
are derived from checked platform/provided/hosted declarations and the shared
C ABI classifier. They are not derived from private `.boxy` worker layouts,
hidden descriptor arguments, hidden dictionaries, or internal boxy dynamic
layouts. A strategy change may alter private Roc procedures and generated code,
but it must not alter:

- exported `provides` symbol names
- hosted symbol names
- argument or return count
- argument or return layouts
- C ABI register/stack placements
- static data symbol layouts
- `RocBox`, `RocList`, `RocStr`, erased-callable, nominal, record, or tag-union
  host representation
- ownership transfer rules for refcounted host ABI values

`Box(T)` in host ABI is always the ordinary Roc box representation: one Roc
refcounted allocation whose data pointer is the value. `Box(function)` is the
ordinary erased-callable representation: one Roc refcounted allocation whose
payload starts with the erased-callable header. `.boxy` may use those same
representations internally, but it may not add a descriptor pointer to either
value shape and may not change allocation headers.

When glue represents an unknown value as `RocUnknown` or `RocBox(RocUnknown)`,
the host sees an opaque pointer-shaped value. The compiler must not expect the
host to provide a payload descriptor for it. If a boxy wrapper needs a payload
descriptor for a host-originating box, that descriptor must be available from
explicit Roc-side checked data; otherwise the wrapper may only treat the box as
opaque. Shallow retain/move/return/drop of an opaque box follows the existing
host ABI contract. Recursive payload teardown or structural inspection requires
an explicit `TypeDesc`.

The declared type is part of that contract, not just the symbol string. A
hosted function's extern boundary is emitted at the hosted declaration's own
checked type, as substituted by the platform/app relation's recorded
requirement solutions (see Platform/App Relation)—the one sanctioned
transformation. The compiler never emits a hosted extern at any other type: not
at a caller's widened error row, not at a narrowed one, and not at a
producer-selected representation that differs from the declared one. A use site
whose own type legitimately differs gets a generated Roc adapter at the
requested type that calls the declared-type boundary and converts around it, so
the boundary itself stays declared-typed; the Hosted Try Question Widening
rule's adapter is one such generated caller.

Every type reachable from a hosted or provided signature must have closed
record and tag-union rows and must contain no runtime-optional (`?:`) record
field. Both restrictions are checker errors over the solved type graph, before
Monotype lowering or glue: the walk follows aliases, nominal backings,
containers, function arguments, and returns, so hiding either `..` or `?:`
behind another type does not make it ABI-legal. A platform must instead use a
required field whose value explicitly represents absence when that state is
part of its host contract. Defaulted (`??`) fields remain ordinary required
inline slots at runtime and are legal at the boundary; construction defaults
are Roc-side information and do not alter the host layout.

A hosted declaration written with type variables is a scheme rather than one
type, and a use instantiates it. The host's single C signature covers every
instantiation because a variable position is a pointer at runtime, so those
slots are the declaration's own; every position the declaration made concrete
is fixed for all uses exactly as above.

Monotype lowering holds that boundary where extern specializations are
produced: emitting a hosted procedure at any other type stops the build with a
compiler-bug report naming the symbol (`requireHostedExternAtDeclaredAbi`,
src/postcheck/monotype/lower.zig). The comparison is structural type equality—
the declared lowering and the request build their graphs separately, so a
recursive nominal is the same type through either graph—with the declaration's
variable slots open when it has any. The check runs in release builds too,
because what it prevents is silent: the host returns `Ok`, the app reads those
bytes as `Err`, and no diagnostic or crash marks the difference. No checker
behavior can therefore produce a wrongly typed extern. A checker rule that
adjusts a hosted call's type at a use site is judged purely as a typing decision
(see Solver-Mutating Rewrites); it is not what keeps the ABI intact.

Weak linkage exists to break the app/host reference cycle without imposing
link order; COFF has no equivalent weak external, and needs none: the app
object participates in the link directly while host archives are searched on
demand, so a single pass resolves the app's references into the host and the
host's references back into the app. Missing host symbols are diagnosed
before linking by scanning the host inputs' symbol tables, not by changing
how the linker resolves symbols.

Because the app references host symbols directly, host inputs are linked
without whole-archive wrapping, and section GC (--gc-sections, -dead_strip,
/opt:ref) removes host functions, host constants, and host helpers that the
application never reaches. This dead-code elimination is a guaranteed,
regression-tested property on every supported target, including wasm32: tests
must verify that unused host functions, unused host constants, and helpers
reachable only from unused host functions are absent from the final binary
(by symbol table inspection and by content-pattern absence), and present when
actually used.

Shared-library output uses the same symbol ABI: the host objects and app
object are linked into one library, app/host resolution happens inside that
link, and dead-strip roots are the exported symbols. Internal `roc_*` symbols
must be hidden in shared libraries—on ELF, default-visibility exports are
preemptible, and two Roc-built libraries loaded into one process would
otherwise interpose each other's runtime symbols.

Interpreter execution (the default `roc` command, embedded interpreter builds,
REPL, compile-time constants, glue evaluation) keeps the same host objects: a
generated translation shim defines the exported entrypoints, marshals their
natural C ABI arguments into interpreter calls, and fills the interpreter's
internal dispatch table with the extern host symbols' addresses. Hosted
dispatch order for that table is the `hosted` section's declaration order.

Canonicalization resolves every `hosted` mapping once to an exact imported
definition. Checking validates that definition is a hosted declaration, and a
successfully checked platform module stores the header-ordered binding table in
its `CheckedModule`, indexed by `(target CheckedModule digest, target CIR
definition)`. `HostedCompiler.replaceAnnoOnlyWithHosted` records the exact
rewritten definition span in `ModuleEnv`; `CheckedModule` construction consumes
it directly. CLI and post-check consumers join hosted procedures to the binding
table by those identities in linear time; they must not reconstruct or match
qualified source names.

## Watch Mode For Check And Test

`roc check --watch` and `roc test --watch` are long-running compiler commands.
They run once immediately, then watch the exact source inputs discovered by that
run. A later filesystem event causes a new run only when at least one watched
input's source file state changes: its bytes change, it appears after being
missing, disappears after existing, or changes between readable and unreadable.
Metadata-only changes such as `touch` do not rerun checking or tests and do not
reprint diagnostics.

The watch set is exact-file based. The implementation may watch directories
because operating systems expose directory-level notification APIs, but directory
events are filtered against the explicit file input set. Files merely present in
a filesystem package or platform directory are not watched unless the compiler
read them as part of the current run. If a source edit adds a new import, the
importing file is already in the watch set; the next run discovers the new input
and refreshes the watch set. URL package files are excluded even when they live
in the local package cache, because their source identity is immutable.

Watch inputs are explicit compiler output. A watch consumer must not recover
module dependencies by scanning source text or reconstruct file imports from
diagnostics. `BuildEnv` owns the shared watch-input collection used by
`roc check --watch` and `roc test --watch`, because both commands already use
`BuildEnv` and because compilation results are transferred there after the
coordinator finishes. After every run, successful or failed, watch mode replaces
the active watch set with the newly discovered explicit input set. Early
failures still include the root source path and any other inputs discovered
before the failure. Missing file imports are included so creating the missing
file can trigger the next run.

File imports are stored in `ModuleEnv` as source-relative dependencies. The
stored path is the literal file-import path interpreted relative to that
module's source directory. It must not be an absolute path, a realpath, a
symlink-resolved path, a cwd-dependent path, or any other host-specific value.
Checked module cache entries include these relative file dependencies so a cache
hit can contribute watch inputs with string concatenation only:

```text
module_source_dir + cached_relative_file_dependency
```

Because file imports are source input to the checked module, their content
identity also participates in the checked module cache key. Changing an imported
file while the importing `.roc` source bytes stay unchanged must miss the
checked module cache and produce fresh checked module data. The cache key input is
the ordered source-relative dependency list plus each dependency's content
digest, never an absolute path or resolved filesystem identity.

When a watched file's parent directory no longer exists, or when a missing file's
parent directory does not exist yet, the watcher registers the nearest existing
ancestor directory and filters events by the unresolved relative suffix. This
keeps watch coverage for later directory creation without widening the logical
watch set.

Filesystem event bursts are debounced for 25ms before re-reading watched inputs.
If another filesystem event with changed bytes arrives while a check/test rerun
is in progress, the in-progress run is cancelled and superseded by the newest
run. Diagnostics and test output are printed for completed runs only. Repeated
runs print a separator before their output instead of clearing the terminal.

## Hot Loading For Default Dev-Shim Runs

The default `roc` command hot loads automatically on the dev backend execution
path: running `roc app.roc` watches the app's source inputs and reloads on
change, with no `--watch` flag and no run subcommand. Non-dev `--opt` levels keep
the existing one-shot behavior, as do apps that cannot use the shared-memory shim
(see below).

The initial compile lowers checked modules to LIR in the compiler process, then
serializes only the dev backend `RunImage` bytes into shared memory. LIR and
compiler IR are never allocated into the shared-memory allocator on this path.
The shared memory contains `RunImage` code section bytes, readonly data bytes,
entrypoint metadata, relocation records, symbol names, and hot-load metadata.
The fixed shared-memory header padding stores only the small atomic hot-load
control block: magic/version fields, the latest descriptor offset, the latest
generation, and host acknowledgement state. It must not contain a fixed table of
loaded-image slots. Loaded images are described by per-image descriptor slots in
the shared-memory mapping plus separately reclaimable image byte ranges, so the
number of retained old generations is limited only by shared-memory capacity.

The compiler launches the host shim first, then installs directory watches for
the exact file input set reported by the coordinator. Coordinator watch inputs
are normalized through the same watch-input collector used by `roc check
--watch` and `roc test --watch`, so relative module paths become logical
absolute paths before reaching the watcher. URL package files are excluded;
filesystem modules, platform/package files, and file imports discovered by the
latest compile are included. After each completed rebuild, the parent refreshes
the watch set from the rebuild worker's serialized watch-input file.

Rebuilds run in short-lived internal compiler child processes. This keeps
cancellation at a process boundary: when a byte-changing filesystem event
arrives while a rebuild is active, the parent kills that rebuild, discards its
captured stdout/stderr text and any uncommitted `RunImage` bytes, and starts a newer generation. A
successful rebuild validates that the checked host interface identity still
matches the already-linked host shim. If the interface changes, the rebuild
reports that the user must restart `roc --watch` and leaves the previous
`RunImage` active.

Successful rebuild workers write a fresh shared-memory image descriptor plus a
new dev `RunImage` into either a compiler-selected free image region or the
append position of the same mapping. Descriptor slots are managed separately
from image bytes and are reused only as descriptors, never as code or data. The
descriptor records the generation, `RunImage` header offset, image bound, image
allocation start/end, lifecycle state, and atomic reference count. The worker
commits the descriptor offset through the hot-load control block with
release/acquire atomics. The host shim checks that control
block at Roc entrypoint boundaries. If a newer generation is available, the shim
retains the latest descriptor, validates and relocates the replacement
`RunImage` in place, marks its code pages read/execute in the shared mapping,
swaps the active entrypoint reference to the new image under the runtime-state
mutex, and acknowledges the generation as accepted. If validation or loading
fails, it acknowledges rejection and keeps using the previous `RunImage`.

Loaded machine-code images are reference-counted by the host shim. Each active
image starts with one reference owned by the active entrypoint table. Entering a
host-callable Roc function increments that image's atomic live count, and
returning from that function decrements it. Swapping to a new image drops the
old image's active-entrypoint reference and moves the old process-local program
descriptor to a retired list. The shim never frees shared-memory image bytes; it
only retains and releases descriptor references. Calls that entered old code
before the swap keep executing old code safely while new entrypoint calls use
the new image.

The compiler parent process is the sole owner of shared-memory image-byte
reclamation. It keeps unbounded process-local lists of descriptor offsets,
reclaimed descriptor slots, and reclaimed image regions. After a rebuild commits
a descriptor, after the host acknowledges, and before choosing storage for
another rebuild, the parent sweeps all known descriptors. The current descriptor
remains live regardless of its reference count. A non-current descriptor with a
nonzero reference count is marked retired and left in place. A non-current
descriptor whose reference count is zero is marked reclaimed, removed from the
live descriptor list, its descriptor slot is returned to the descriptor-slot free
list, and its image allocation range is added to the image free-region list. The
parent coalesces free image regions and rewinds the shared-memory header's
used-size high-water mark to the highest still-live image allocation. New
rebuilds prefer suitably sized reclaimed image regions and otherwise append
below the descriptor-slot area. If filesystem changes arrive faster than the host
can enter the shim, the newest rebuild can still commit a descriptor as long as
shared memory has capacity; there is no small fixed "loaded slot" cap.

This lifetime rule also covers boxed Roc closures that cross the host boundary.
The dev backend generates real erased-callable procedures; it does not insert
trampolines. In shim execution mode, packed erased-callable payloads reserve a
small shim-only prefix before the ordinary capture bytes. The prefix stores a
reference to the owning loaded image and the original capture-drop callback.
Generated erased-callable procedures skip this prefix before reading their
capture, increment the owning image on entry, and decrement it before returning.
The payload's final-drop callback first runs the original capture-drop callback
with the adjusted capture pointer, then releases the payload's retained image
reference. That retained reference keeps an old image alive while a host stores
a boxed Roc closure and later calls it after one or more hot reloads.

Headerless default apps never hot reload. They compile through synthetic
temporary source files that are discarded after each run, so there is nothing
stable to reload; they always run once, even where the shared-memory shim is
available. Hot loading therefore applies only to apps with a real platform
header. Windows uses explicit shared-memory handle inheritance for both the host
shim child and the internal rebuild worker.

## Relationship To Cor LSS

The `.lss` runtime lowering strategy mirrors Cor's LSS experiment after
solving, adapted for Roc's checked module boundary and existing LIR.

| Cor LSS stage | Roc stage |
| --- | --- |
| solved source IR | checked CIR plus checked type store |
| `monotype` | Monotype IR |
| `monotype_lifted` | Monotype Lifted IR |
| `lambdasolved` | Lambda Solved IR |
| `lambdamono` | logical Lambda Mono decisions in direct lowering; materialized only by the debug verifier |
| `ir` | direct Solved-to-LIR builder |
| `eval` | LIR interpreter for compile-time evaluation |

`.lss` intentionally keeps Cor's post-solve shape:

- Monotype IR is closed, monomorphic typed IR.
- Monotype Lifted IR has top-level lifted functions and explicit captures.
- Lambda Solved IR stores callable flow in function types.
- Lambda Mono removes function types by turning finite function values into
  ordinary generated tag unions and erased function values into packed erased
  callables.

Roc adds language and implementation data that Cor's experiment does not need:

- static dispatch and method registries
- checked module caches and imported checked bodies
- opaque, nominal, alias, builtin, platform, hosted, and exposed identities
- target-independent `ConstStore` values
- the existing statement-only LIR, ARC, LirImage, and backend boundaries

The main language difference is static dispatch. Roc keeps static dispatch
separate from checked types. Checking still reports every user-facing
static-dispatch error and outputs checked call classifications. Monotype IR
lowering consumes those classifications and only instantiates the explicit
parametric/evidence relations before replacing dispatch with direct calls; a
closed direct call performs no method lookup or dispatch-graph construction.

Lambda sets are not stored in the checked type store or checked cache.
They are introduced after Monotype Lifted IR, during Lambda Solved IR, exactly
where callable value flow is being solved for the current whole-program root.

Cor's experiment uses unary function types internally. Roc does not. Every pass
that corresponds to Cor's callable pipeline is generalized over the full ordered
argument list:

- Lambda Solved function types are `args/callable/ret`, not nested unary
  `arg/callable/ret` chains.
- Lambda-set unification connects the one callable slot for the whole function
  value.
- Lambda Mono direct calls pass the full argument list, plus a compiler-created
  capture record argument only when the selected finite member has captures.
- Erased callable ABIs contain the full ordered argument list and result layout.
- Specialization identity includes the full function type, not just one argument
  and a nested return function.

Cor's final `ir` is a post-lambda IR stored as a distinct data structure. Roc
does not keep that as a separate persisted stage because the existing LIR
boundary already serves the consumer side. Roc's direct LIR builder corresponds
to Cor's final lowering work, with explicit internal contracts instead of a
serialized Layout IR or extra middle layer.

Cor's experiment also performs some final field and tag lookup by source label
inside its final lowering. Roc does not copy that part. Roc's checked and
post-check stages output ordered spans and checked ids before direct LIR
lowering. Direct LIR lowering consumes those ids and span positions; it does not
look up record fields or tag variants by display label to recover missing row
relationships.

`.boxy` is not a Cor LSS pipeline. It deliberately avoids the Monotype,
Lambda Solved, and Lambda Mono stages for runtime roots. It keeps the same
checked boundary and LIR boundary, but it reaches LIR by boxing polymorphic
values, boxing closures as erased callables, and passing explicit descriptors
and dictionaries. Its correctness is measured by the same observable Roc
Roc language behavior and the same host ABI, not by producing the same private procedures or
callable representation as `.lss`.

## Forbidden Shapes

The post-check pipeline must not contain:

- MIR as a separate compiler layer
- a persisted layout IR between Lambda Mono and LIR, or between boxy
  representation planning and LIR
- post-demand worklists
- implicit fallback lowering paths outside the selected explicit strategy
- comparing against another lowering path to decide compiler behavior
- callable descriptor replacement in `.lss`
- callable value repointing
- late payload output
- generic conversion expressions, post-hoc conversion plan tables, or mismatch
  patching lowering paths
- checked-module runtime payloads, value conversion plans, callable-set
  descriptors, boxy descriptors, boxy dictionaries, or erased ABI decisions
- owner discovery by method-registry intersection
- method-registry intersection used as an ownership source
- backend reference-counting decisions
- descriptor or dictionary pointers stored inside ordinary Roc value
  representations
- host ABI signatures that include hidden boxy `TypeDesc` or dictionary
  arguments
- changing `Box(T)`, `Box(function)`, `RocUnknown`, or any other host-visible
  runtime representation based on the selected lowering strategy
- using shallow drop, default descriptors, runtime bytes, or pointer width as a
  substitute for an explicit `TypeDesc`
- mode, lifetime, or RC-signature data stored in checked modules, LirImage,
  or any structure that outlives ARC insertion
- user-facing errors after checked module output
- release-build checks whose only purpose is maintaining compiler invariants

The allowed replacement is explicit stage ownership:

- checking owns user-facing diagnostics and checked data
- Monotype owns monomorphic specialization and static-dispatch elimination
- Monotype Lifted owns closure lifting
- Lambda Solved owns callable flow in the type graph
- Lambda Mono owns explicit callable value representation
- Boxy lowering owns descriptor/dictionary planning and checked-to-LIR lowering
  for `.boxy`
- LIR lowering owns committed layouts and statement lowering for the selected
  strategy
- ARC owns borrow inference, mode specialization, and reference-count
  insertion
- backends own only backend code generation from explicit LIR

## Debug Invariants

Every stage boundary has debug-only verification. In release builds, invariant
checks must compile away to nothing or to `unreachable` after inlining.

Minimum boundary checks:

- Monotype IR contains no checked static-dispatch, method-equality, type
  dispatch, or source `for` nodes.
- Monotype IR contains only closed monomorphic types.
- Monotype IR contains no runtime tag discriminants, layout ids, or callable
  representation ids.
- Monotype Lifted IR contains no reachable closure expressions, local function
  definitions in expression position, definition references in expression
  position, or direct calls whose callee is still a Monotype function template.
- SpecConstr binding chains are well-linked, source-ordered, type-correct, and
  placed by their owning expression, statement, branch, or jump site.
- Rewritten Monotype Lifted bodies have only lexically scoped local references
  and jumps whose target is in scope and whose argument count matches its join.
- Lambda Solved IR has every function type in `args/callable/ret` form.
- Lambda Solved IR has no unresolved callable slot before direct LIR lowering.
- Lambda Mono decisions contain no function type and no value-call node.
- Lambda Mono decisions contain no unresolved lambda set.
- Lambda Mono decisions contain no runtime tag discriminants or layout ids.
- `.boxy` runtime lowering constructs no Monotype, Monotype Lifted, Lambda
  Solved, Lambda Mono, or lambda-set data for runtime roots.
- `.boxy` value locals that require dynamic payload behavior have an explicit
  `TypeDesc` source available at every statement that boxes, unboxes, copies,
  recursively drops, or structurally inspects the value.
- `.boxy` dictionaries are produced only from checked dispatch plans and
  checked method registries; no LIR stage performs owner discovery by method
  name or registry intersection.
- `.boxy` host-visible root procs have exactly the checked host ABI layouts and
  contain no hidden descriptor or dictionary arguments. Hidden values appear
  only in private workers or opaque erased-callable captures.
- `.boxy` provided static data requests use the exact host-visible checked
  layouts, not internal boxy layouts.
- Checked compile-time stores contain only `ConstStore` data.
- LIR lowering receives only the selected strategy's explicit inputs: Lambda
  Solved lifted syntax plus Lambda Mono decisions for `.lss`, or checked
CheckedModule data plus boxy representation data for `.boxy`.
- ARC insertion receives LIR containing no RC statements.
- ARC insertion sees dynamic boxy RC helper plans only where the LIR statement
  carries an explicit descriptor reference.
- ARC output passes the debug borrow certifier.
- Backends receive only ARC-complete LIR.
- No deduplication, specialization, or callable-merging step maps two
  `hosted` declarations or two `provides` exports to one identity, even when
  their declaring modules are byte-identical.

The test suite also verifies cross-strategy host ABI equivalence. For the same
CheckedModule, `.lss` and `.boxy` must agree on exported and hosted symbol
signatures, lowered C ABI placements, glue type tables, provided static data
layouts, entrypoint ABI digests, and root metadata. This equivalence check is a
test requirement, not a release-build compiler pass.

If a boundary check fails, the compiler stops as a compiler bug.


## Integer SIMD Builtins

This section describes the design of Roc's 128-bit integer SIMD builtins: the
goals, the invariants they must preserve, the target architecture floors they
assume, the type and naming design, and the pinned meaning of every
operation class. The API itself lives in `src/build/roc/Builtin.roc` as eight
nominal vector types nested in `Num`.

### Goals

The purpose of these builtins is to make it possible to write pure-Roc
implementations of compression and image codecs—DEFLATE (plus zlib/gzip
framing), PNG, JPEG, WebP, and AVIF, both encoding and decoding—whose
performance approaches state-of-the-art native implementations (libdeflate,
libjpeg-turbo, libwebp, dav1d, libaom) when both sides are held to 128-bit
vectors and a single thread. The same operations also cover zstd, brotli,
LZ4, base64/hex, UTF-8 validation and transcoding, JSON structural scanning,
hashing (xxh3-class, CRC-32), and FLAC.

The design center is *portable operations with pinned meaning*, not
per-ISA intrinsics: each operation has exactly one meaning, and each backend
lowers it to the best instruction sequence for that target. The one-line
design law:

> **Target-specific lowering is fine; target-specific meaning is not.**
> Speed may vary by target. Bits may not.

The op set was derived from the hot-kernel inventories of the codecs above—
every operation is justified by named kernels that need it (DCT/IDCT
multiply-accumulate, scanline filters, loop filters, CDEF, palette lookups,
LZ77 copies, CRC folding, SAD-based encoder search, and so on). Nothing is
included merely because some ISA has an instruction for it.

### Invariants

1. **Bit-identical results everywhere.** Pure Roc code produces the same
   answer on every target and every backend—LLVM, both dev backends, wasm,
   and the interpreter—and compile-time evaluation, which runs Roc code
   on the dev backend, must agree with all of them. Every SIMD
   operation therefore has a precise scalar reference meaning, including
   its edge cases: shift counts at or past the lane width, out-of-range
   table-lookup indices, saturation boundaries, the `q15` multiply's
   `-32768 × -32768` corner. Where an ISA's native behavior deviates from
   the pinned meaning, that backend emits fixup instructions; it never
   gets to leak its own behavior through.
2. **No target-conditional Roc code.** There is no way, and will be no way,
   for Roc source to ask "which CPU am I on?" The compiler alone knows.
3. **Fixed 128-bit width.** A vector type is 128 bits, always, on every
   target. This is itself a determinism feature: width-polymorphic APIs
   (Highway-style) make *code* portable but let intermediate states vary by
   target, which would leak target-dependence into observable results.
   128-bit is the width that x86-64 (SSE through AVX2's 128-bit forms),
   AArch64 NEON, and wasm simd128 all share natively. Wider types (`U8x32`,
   …) can be added later as *new types* without changing what any existing
   code means.
4. **Ordinary host ABI values.** SIMD vectors may appear anywhere an ordinary
   Roc value may appear, including directly or recursively inside hosted and
   `provides` arguments and results. Every host boundary follows the selected
   target's natural C ABI. Generated C, Zig, and Rust declarations use native
   vector leaf types and C-layout aggregates whose size, alignment, field
   offsets, argument placement, and result placement match the compiled Roc
   code.

Where an operation exists in wasm simd128, we pin wasm's meaning—that
spec already solved "deterministic 128-bit SIMD that lowers well to both SSE
and NEON," and it makes the wasm backend nearly free. (Operations from
wasm's *relaxed-simd* extension are explicitly nondeterministic there;
anything we take from that family gets pinned meaning here instead.)

### Why integer-only (for now)

All five target formats are integer in their specifications: AV1 decode is
bit-exact integer by spec, libjpeg-turbo's production DCT is fixed-point
integer, and DEFLATE/PNG/WebP have no arithmetic beyond integers. Float
shows up only in encoder decision heuristics (rate-distortion lambdas,
perceptual tuning), which convert cleanly to fixed-point—with the side
benefit that Roc encoders become bit-reproducible across targets, which none
of the C encoders guarantee. Float SIMD raises real cross-target determinism
questions (FMA contraction, approximation instructions) that integer SIMD
simply does not have, so it is deferred until something actually needs it.

### Target floors

Because SIMD operations inline into user code, there is no seam where
runtime CPU dispatch could live (dispatch requires an out-of-line call
boundary at kernel granularity, which is exactly what an inlined builtin
does not have—and Roc has no startup hook for eager dispatch either).
Instead, each (architecture, OS) target has a static floor:

- **x86-64:** `x86-64-v3` **plus AES-NI and PCLMULQDQ**—i.e. Intel Haswell
  (2013) and later, every AMD Zen. The v-levels deliberately exclude the two
  crypto instructions (they were fused off on some budget parts before
  ~2017), so the floor names them explicitly. Rationale: `pshufb`-class
  byte shuffles (SSSE3) are load-bearing for nearly every codec kernel; the
  VEX three-operand encodings and scalar BMI2 that v3 brings materially
  speed up entropy-decode loops even at 128-bit vector width; carryless
  multiply is required for competitive CRC-32. As of 2026 this floor covers
  ~95% of the consumer installed base and 100% of what Windows 11 supports;
  RHEL 10 already requires v3.
- **AArch64:** Armv8.0-A plus AES and DotProd. This names exactly the two
  extensions the builtins lower to instead of selecting a CPU model that would
  pull in unrelated architecture revisions. It covers every Apple Silicon Mac,
  every major ARM cloud chip, and Raspberry Pi 5. Raspberry Pi 3/4 lack these
  extensions.
- **wasm:** the `simd128` feature (universally shipped in engines since
  2021; the wasm backend already assumes it). wasm has no carryless
  multiply and no AES instructions, so those two operations get slower—
  but bit-identical—software lowerings on wasm.

Under these floors both native architectures guarantee the same capability
set: full 128-bit integer SIMD, a one-instruction byte shuffle, carryless
multiply, and AES rounds. Floors only ever affect speed, never results.

Each `RocTarget` has one `CpuContract` in `src/target/mod.zig`. Its architecture
baseline and explicit instruction features are the sole source for both the
LLVM target query and runtime host compatibility. A named LLVM CPU model may
supply scheduling information, but all model features outside that contract
are explicitly disabled, so choosing a platform host and choosing instructions
cannot drift apart. `host_cpu.zig` maps the contract's features to CPUID or OS
feature queries, and a compile-time equality check rejects any detector mapping
that does not cover the contract exactly.

Every target for which Roc raises the architecture floor has a `v1` twin. On
x86-64, `x64v1*` means the psABI x86-64-v1 floor (SSE2 and no later
extensions). On AArch64, `arm64v1*` means Armv8.0-A; Apple Silicon has no v1
twin because every supported Mac already implements the macOS floor. On wasm,
`wasm32v1` means the WebAssembly 1.0 core instruction set without simd128.
Operations unavailable at v1 use exact alternate instruction sequences or the
target-independent builtin implementation; the source-level result is
unchanged.

A default-level target and its v1 twin are separate platform target names with
separate host inputs. A compiler must never link the default target's host into
a v1 application or reinterpret an old default-target host as baseline: either
would silently reintroduce instructions above the promised floor. Platform
authors that support both levels build and declare both entries explicitly.

For build or execution without an explicit `--target`, the CLI detects the CPU
features of the machine running the compiler and considers targets in platform
declaration order. A native target is eligible only when every feature in its
static instruction-set floor is present; LLVM scheduling and tuning flags are
not hardware capabilities and do not participate. Wasm remains eligible for
build regardless of host CPU. The default `roc` command additionally requires
`output: Exe`. If the platform declares only a native target whose CPU floor
the host does not meet, selection reports the missing v1 target before code
generation or execution. An explicit `roc build --target` remains a
cross-compilation request and does not require the selected CPU features to be
present on the machine invoking the CLI.

### Type design

Eight concrete nominal types, siblings of the scalar number types in `Num`:

```
U8x16  I8x16  U16x8  I16x8  U32x4  I32x4  U64x2  I64x2
```

Deliberately **not** one parameterized `Vec(lane)` type, for three reasons:

1. A large fraction of the op surface is lane-specific with cross-type
   signatures (widening `U8x16 -> U16x8`, narrowing takes two `U16x8` to
   one `U8x16`, `dot_pairs : I16x8, I16x8 -> I32x4`, byte shuffles only at
   lane width 8, carryless multiply only at width 64). Under `Vec(lane)`
   these all need concrete signatures anyway, so the parameterization buys
   sharing only for the plain lane-wise suite while creating a "what rules
   out `Vec(Str)`" problem and demanding a type-level next-wider-lane
   function.
2. Generic user code over vector types falls out of `where` method
   constraints on the concrete types, exactly the way `List.sum` is generic
   over `plus`/`default`—no `Lane` type or new machinery needed.
3. It matches the house style: `Num` spells out thirteen scalar types
   longhand; eight more siblings are stylistically seamless, and every op
   stays monomorphic, which is what predictable lowering wants.

Comparison results are **same-typed vectors** whose lanes are all-ones or
all-zero (wasm-style)—there is no separate mask type. That is the machine
representation on all three ISAs, and it composes with the bitwise ops and
`bit_select` for free.

#### Naming conventions

- Lane-wise wrapping arithmetic is `plus_wrap` / `minus_wrap` /
  `times_wrap` (never bare `plus`): scalar `plus` is
  overflow-checked-and-crash, vectors cannot cheaply lane-check, and
  hardware vector arithmetic wraps or saturates. The `_wrap` suffix keeps
  the house rule that the suffix names the overflow behavior. Saturating
  variants are `_saturated`, matching the scalars.
- Operations returning per-lane comparison masks end in `_lanes`
  (`eq_lanes`, `gt_lanes`, …); `is_eq` (returning `Bool`, all 128 bits
  equal) keeps its usual meaning for `==` and tests.
- Conversions follow scalar conventions: `to_u16x8_lo`/`_hi` for widening
  halves, `narrow_to_u8x16_saturated`/`_wrap` for (two-input) narrowing,
  `to_u32x4_bits` / `to_u128_bits` / `from_u128_bits` for free
  reinterpretation of the same 128 bits.
- Shifts are `shl_wrap` / `shr_wrap` / `shr_zf_wrap` with
  a scalar `U8` count applied uniformly to every lane, matching the scalar
  methods bit-for-bit per lane (see meaning below).
- Every type carries the standard citizenship methods: `default` (zero
  vector), `is_eq`, `to_hash`, `to_inspect`, plus `splat`, `from_list`,
  `to_list`. The vector types deliberately do not participate in
  `parser_for`/`encoder_for` derivation and have no `from_numeral`—
  vectors are not literals or serialization leaves.

#### Lane order

Lane `i` of a vector occupies bits `[i * lane_bits, (i + 1) * lane_bits)`
of the 128-bit value, and the vector's byte-serialized form (for `load`,
`store`, `to_list` on `U8x16`, hashing) is little-endian—lane 0 first,
each lane's bytes least-significant first. This matches the in-register and
in-memory reality of x86-64, AArch64, and wasm, so no target pays a
byte-swap tax.

### Host ABI

The vector types are full participants in the Host Symbol ABI. There is no
internal-only restriction, wrapper-call convention, byte-array boundary type,
or source-level adapter. A Roc programmer may use a vector directly, place one
inside a record, tuple, tag payload, list item, box payload, or another
ordinary type, and use that type in either direction across a hosted or
`provides` symbol.

Layout commitment records each vector as a 16-byte, 16-byte-aligned native
vector with its lane width and signedness. Host-call classification walks the
complete committed argument and result layouts, including nested aggregates,
and applies the target's C ABI:

- On System V x86-64, a direct 128-bit vector has SSE/SSEUP class and occupies
  one XMM register. A vector member contributes those classes to the containing
  aggregate's recursive eightbyte classification.
- On Windows x86-64's default C convention, a direct 128-bit vector argument
  is passed through an aligned caller temporary and a pointer, while a direct
  vector result is returned in XMM0. An aggregate containing a vector follows
  the Win64 aggregate rules; it is not treated as the vector it contains.
- Under AArch64 C ABIs, including fixed-prototype Windows-on-ARM64, a direct
  128-bit short vector occupies one Q register, and an aggregate that
  transparently wraps exactly one vector is the same Q-register value. Other
  aggregates follow the platform's AAPCS64-derived size rules; in particular,
  an aggregate with two or more vector members is memory-class in both call
  directions. That is the host-boundary contract every supported host
  language's natural declarations compile to: Zig `extern struct`s and Rust
  `repr(C)` structs of vectors use the memory-class convention, and generated C
  glue spells the vector members of such aggregates as vector/byte unions so C
  compilers do not classify them as homogeneous vector aggregates (whose
  AAPCS64 register rule only C toolchains implement). Homogeneity and
  transparent-wrapper discovery recursively erase the same single-variant
  wrappers as generated glue.
- Under the WebAssembly Basic C ABI, a direct vector is `v128`. Transparent
  one-field aggregates retain the field's direct classification; other
  aggregates follow the WebAssembly aggregate rules. An erased callable is the
  direct pointer value exposed by the C, Zig, and Rust glue aliases.

These are target rules, not backend choices. LLVM, the native dev backends, the
interpreter translation shim, wasm, and generated host declarations consume the
same explicit ABI placements. A backend must not classify a vector from its
name or reinterpret it as `U128`; in particular, two general-purpose 64-bit
pieces are not interchangeable with one 128-bit vector register value.

Generated glue makes the same contract usable without handwritten declarations.
C glue selects the target's native integer-vector types, Zig glue uses
lane-typed `@Vector` types with `callconv(.c)`, and Rust glue uses distinct
`repr(transparent)` wrappers over the stable target-architecture SIMD types in
C-ABI extern declarations together with `repr(C)` aggregates. All eight Roc
vector names remain distinct binding names even where the target uses one
underlying register type. C records expose their committed fields, and C tag
unions expose named discriminants, concrete payload storage, and typed
constructors/accessors. Generated size, alignment, discriminant, payload, and
field-offset assertions lock aggregate layout. Cross-language tests compile the
generated C, Zig, and Rust output and call every direct and aggregate shape in
both directions, so a host compiler and Roc must independently choose the same
ABI.

An owning tag-union payload is never projected through a borrowed accessor
that returns an owning value. Generated host glue exposes an unsafe raw
tag payload move operation which takes mutable access to one owned union shell,
moves the active payload out, and leaves the shell logically uninitialized.
The caller must first validate the explicit discriminant and must neither read
nor destroy the shell after the move. A host language with affine or RAII
ownership puts this primitive behind a non-copying owner whose failed
tag-specific payload move returns the still-owned shell. Borrowed inspection, if
needed, returns only a pointer or reference and never fabricates another owning
payload descriptor. The 32-bit aligned-byte representation and native union
representation provide the same move contract.

ABI class assignment, LLVM carrier selection, and concrete argument/result
placement are separate explicit steps. Every register placement records both
its byte pieces and their atomic carrier. Piecewise carriers become independent
LLVM parameters, structure carriers preserve aggregate results even when they
contain one field, scalar 128-bit integer arguments remain one naturally aligned
`i128`, and AArch64 integer pairs, HFAs, and transparent vector aggregates
remain one homogeneous array. On ELF AArch64, those array parameters
additionally carry `alignstack(8)` for F32/F64 HFAs or `alignstack(16)` for
vector aggregates; Mach-O and Windows use the same array carrier without that
LLVM attribute. LLVM wrappers marshal exactly that carrier instead of
flattening it and relying on a later stage to reconstruct argument identity.
Structure returns retain each committed field's exact vector kind.

Concrete argument assignment accounts for each register class's remaining capacity,
preserves a SysV SSE/SSEUP vector as one value, applies the base AAPCS64 and
Windows even-X-register rule for 16-byte-aligned multiword arguments, and
spills an entire atomic value (such as a complete HFA) when the remaining
registers cannot hold it. Apple arm64 removes that
even-register rule and packs stack arguments at their natural alignment; both
differences are explicit target data. On SysV, scalar `I128`/`U128` uses two
`i64` parameters while both INTEGER eightbytes fit and one `i128` parameter
when the complete scalar must go to the stack. Generated `RocDec` is an
aggregate instead and becomes aligned `byval` when both INTEGER eightbytes do
not fit. Wrappers and machine-call consumers follow this explicit data
directly.

The concrete ABI placement is authoritative all the way through native call
emission. Consumers address the recorded register index directly, including
intentional holes before aligned AArch64 multi-register values. Stack
placements remain byte offsets rather than being converted back into abstract
eightbyte slots: this is required for Apple arm64's naturally aligned compact
1/2/4-byte arguments. The inverse entrypoint wrappers copy from those same
byte-exact offsets. AArch64 hosted result capture includes V0 through V3, the
complete result-register set required by legal one-to-four-member HFAs.

On Windows x64, scalar `I128`/`U128` is indirect as an argument and returned in
XMM0 with the compiler's `<2 x i64>` carrier. Generated `RocDec` remains an
aggregate and is indirect in both directions. The ABI classifier distinguishes
the C spelling even though both Roc layouts contain the same 128 payload bits.

The cross-language ABI fixture exercises these exhaustion rules in both call
directions with generated C, Zig, and Rust declarations: nested transparent
HFAs and memory-class vector aggregates after seven SIMD arguments, integer
pairs after seven GP arguments, platform-specific `I128` register alignment,
and exhausted SysV `I128`/`Dec`.
It also exhausts all eight AArch64 GP argument registers before passing
`U8`/`U16`/`U32`, which pins Apple's compact stack offsets in hosted and
provided directions.

### Pinned meaning—the edge cases

These are the cases where ISAs disagree natively and the spec must choose.
The choices below are implemented by the reference implementations and are
the contract every backend must match:

- **Shifts** take the count modulo the lane width: `shl_wrap`, `shr_wrap`,
  and `shr_zf_wrap` shift every lane by `count % lane_bits`, so a count equal
  to the lane width leaves every lane unchanged and larger counts wrap around.
  This matches the scalar `shl_wrap` family.
- **`table_lookup`** (`pshufb` / `tbl` / `i8x16.swizzle`): any index ≥ 16
  yields 0. (wasm/NEON meaning; on x86 `pshufb` needs a one-instruction
  fixup because it wraps indices 16–127.)
- **`times_fixed_q15_saturated`** (`pmulhrsw` / `sqrdmulh` /
  `i16x8.q15mulr_sat_s`): `(-32768, -32768)` **saturates to +32767**
  (wasm/NEON behavior; x86 `pmulhrsw` wraps on exactly this input and
  needs a fixup, elidable whenever one operand is a constant that is not
  -32768).
- **`dot_pairs_saturated`** (`pmaddubsw`): unsigned × signed byte products
  summed pairwise into `I16` lanes **with signed saturation** (x86
  meaning, the useful one for filter kernels; NEON lowers via widening
  multiplies plus a saturating pairwise combine).
- **`sums_of_abs_diffs`** (`psadbw`): result lane 0 holds the sum of
  absolute differences of bytes 0–7, lane 1 of bytes 8–15 (x86 layout,
  pinned; NEON lowers via `uabdl`/`uadalp`).
- **`avg_rounded`** (`pavgb` / `urhadd` / `avgr_u`): `(a + b + 1) >> 1`—
  all three ISAs already agree.
- **Saturating narrowing** uses the source signedness to clamp into the
  destination range exactly as the scalar `to_*_try` bounds would define
  (`packsswb`/`packuswb`-family, `sqxtn`/`sqxtun`, `narrow_i16x8_*`).
- **`get_lane` / `with_lane` / `broadcast_lane`** crash on an out-of-range
  lane index (like `div_by` crashes on zero), and `concat_shift_bytes`
  crashes on a shift count > 16. For `concat_shift_bytes`, a constant count
  selects the immediate `palignr`, AArch64 `ext`, or wasm `i8x16.shuffle`
  form. A runtime count is equally valid and uses shift/combine sequences on
  native targets or a spill plus dynamic 16-byte load on wasm, because those
  three instructions have immediate-only indices.
- **`to_bitmask`** collects each lane's most-significant bit, lane 0 in
  bit 0 (`pmovmskb` / `i8x16.bitmask`; NEON emulates in a few
  instructions). `any_lanes_set`/`all_lanes_set` are defined in terms of
  it.

### Compiler representation and lowering

Each vector type is compiler-provided and has its own first-class committed
layout. The eight layouts share size, alignment, and vector register class but
retain distinct lane width and signedness. Low-level operations are
lane-parameterized; their argument and result layouts encode lane width and
signedness, including both source and destination kinds for widening and
narrowing.

Methods implemented directly by the compiler are bodiless declarations in
`Builtin.roc`. Thin checked methods remain in Roc for range checks and for
one-line compositions such as flipped comparisons, lane broadcast, bit casts
between sibling vector types, collection conversions, inspection, hashing, and
streaming. Every compiler-backed operation is implemented by LLVM, both native
dev backends, wasm, the interpreter, and the Lambda Mono evaluator. Compile-time
evaluation uses the same vector layouts and dev lowering as runtime dev code,
and `ConstStore` preserves all 16 vector bytes under the checked vector type.

The exhaustive bit-level SIMD evaluator in `src/builtins/simd.zig` is the
correctness oracle for the interpreter, Lambda Mono evaluator, and differential
tests. Both native dev backends instead dispatch exhaustively on the static LIR
operation and its explicit source/destination lane kinds and emit native
packed-integer instruction sequences. There is no compiled scalar evaluator,
runtime operation descriptor, or fallback call. Missing native coverage is a
compile-time exhaustiveness failure. A Lambda Mono value that does not have an
integer bit representation is rejected explicitly rather than being replaced
with zero. Wasm emits `v128` operations and exact helper sequences, including
software carryless multiplication where simd128 has no instruction.

Unchecked 16-byte loads are native vector loads. Stores use the explicit LIR
uniqueness decision: the in-place path emits a native vector store, while the
non-unique path may call the allocation-aware list-clone primitive before that
store. This is mechanical consumption of earlier ARC output, never backend
reference-count inference.

Structural equality treats vectors as ordinary value leaves. Solved-to-LIR
lowering converts each vector operand to its complete 128-bit bit image and
uses scalar `U128` equality, so every backend compares every lane without
needing a vector-specific equality code path. This LIR bitcast does not alter
host ABI classification.

LLVM emits generic vector IR for ordinary operations and target intrinsics for
operations whose pinned edge behavior or instruction selection requires them.
The generic `dot_pairs_saturated` lowering widens unsigned and signed bytes to
32-bit lanes, multiplies and pairwise-adds at that width, clamps to signed
16-bit bounds, and narrows only after saturation; no intermediate 16-bit sum
is permitted to wrap.
`src/target/mod.zig` is the single authority for the CPU contract, LLVM target
query, CPU name, and feature delta. Linked builds, optimized tests, LLVM
evaluation, and platform host selection therefore consume the same x86-64-v3
plus AES/PCLMULQDQ, AArch64, or wasm simd128 floor.

The former pure-Roc `{ bits : U128 }` implementations live only in the SIMD
test oracle. They define each operation lane by lane without depending on the
compiler vector types. The differential suite compares the oracle with runtime
LLVM, both dev targets, wasm, the interpreter, the Lambda Mono evaluator, and
compile-time evaluation over fixed edge corpora and deterministic generated
inputs. The host-ABI suite separately compiles generated C, Zig, and Rust glue
and verifies direct and nested vector values in both call directions.

`zig build run-test-simd-differential` owns the shared proof source and runs the
standalone dev and LLVM programs, optimized compile-time tests, every evaluator
backend, and the Lambda Mono comparison in sequence. The corpus contains 294
operation/type cases, fixed boundary values, algebraic properties, and at least
64 deterministic generated inputs per applicable case. Every shift operation is
checked directly against the scalar oracle for every count from zero through one
less than the lane width,
then all 256 possible `U8` counts are proven equal to their oracle-checked
modulo-lane-width count. This targeted loop does not repeat count-independent
cases. Checked memory access pins the final valid 16-byte window, the first
invalid offset, and `U64.highest`; every public
lane accessor pins its exact first-invalid index. The corpus is opt-in to
ordinary test enumeration so the normal eval and Lambda Mono steps do not run
it twice, but MiniCI runs the dedicated no-skip gate explicitly.
The full Lambda Mono body-lowering differential sweep over the ordinary eval
corpus runs once per day on Ubuntu through `nightly_gate.yml`; it is not part of
PR MiniCI. This does not remove the dedicated SIMD gate's Lambda Mono lane.
`zig build run-check-simd-codegen` separately requires both optimized and dev
x86-64 output to contain representative byte-add, pairwise-dot, table-shuffle,
and carryless-multiply instructions, rejects deleted scalar-evaluator and load
helpers from dev binaries, and cross-builds the exhaustive corpus through the
AArch64 dev backend so every supported operation/type lowering is instantiated.
`zig build run-check-glue-abi` compiles
generated Zig and C declarations for x86-64 and AArch64 Linux/macOS/Windows plus
wasm, compiles Rust for native and wasm, and the native/wasm glue runtime matrix
calls the generated contracts in both directions.

### Doc comments name the instructions

Every operation's doc comment states the instruction (or short sequence)
that implements it on x86-64, AArch64/NEON, and wasm simd128—e.g.
`table_lookup` names `pshufb`, `tbl`, and `i8x16.swizzle`—so that
searching the docs for an instruction name finds the Roc builtin that
provides it. Where a target needs a fixup or emulation, the doc says so.

### Memory interop and streaming

- `load : List(U8), U64 -> Try(V, [OutOfBounds, ..])` and
  `store : V, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])` are the
  checked bulk accessors (bytes, little-endian, any alignment—unaligned
  128-bit access is effectively free on every supported CPU). The checked
  form costs nothing in optimized loops: LLVM already merges the bounds
  check of exactly this shape into the loop condition (see issue #10301,
  case 1, where the checked `List.get` loop compiles to the same machine
  loop as C).
- `append_to : V, List(U8) -> List(U8)` appends 16 bytes for output
  assembly.
- `U8x16.iter_list : List(U8) -> { chunks : Iter(U8x16), tail : List(U8) }`
  is the streaming chunk driver. Its current per-chunk overhead is tracked
  in #10301 (case 4); codecs stream via `Iter` of chunks/rows, never per
  byte.

### Scalar-side gaps (tracked separately)

Competitive codecs need the scalar side of the language to hold up too; the
known gaps, deliberately excluded from the SIMD effort, are:

- wrapping scalar arithmetic does not exist, and plain `+`/`-`/`*` are
  checked (crash-on-overflow) even at `--opt=speed`, which also blocks
  auto-vectorization of reductions—#10300;
- `for`/`Iter` loops carry per-item step calls and refcount traffic
  that the equivalent `while` loop does not—#10301;
- no scalar rotate, byte-swap, or unaligned multi-byte loads from
  `List(U8)` (bit-reader fuel for entropy decoders).

### Benchmarking ground rules (for later)

Single-threaded, competitors pinned to their 128-bit code paths (e.g.
dav1d `--cpumask`, libjpeg-turbo's SSE2/NEON paths), same machine,
CI-based. Encoder comparisons are speed at matched output quality
(SSIMULACRA2-class metrics for images), not raw throughput. Their
unrestricted AVX2/AVX-512 numbers may be recorded as context but are not
the pass/fail bar while the language is 128-bit-only.

### Open questions

- Whether `get_lane`-style constant-index ops should eventually require
  compile-time-constant arguments (today: crash on out-of-range, fast
  paths when the optimizer sees a constant).
- Whether a 32/48/64-byte `table_lookup` tier (NEON `tbl2`–`tbl4`) earns
  its place once real kernels are measured (expressible today as multiple
  16-byte lookups plus selects).
- Typed-item loads (`List(U16)` → `U16x8`, etc.)—deferred until a
  kernel wants them; byte buffers are the codec substrate.
- Saturating arithmetic on 32/64-bit lanes, `abs` on `I64x2`, and unsigned
  ordering compares on `U64x2` are omitted because no cataloged kernel
  uses them and hardware support is ragged; any of them can be added later
  without disturbing existing meaning.
