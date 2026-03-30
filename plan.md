DO NOT COMMIT THIS FILE.
DO NOT RUN ZIG OR THE COMPILER UNTIL THE ARCHITECTURAL CUTOVER IS COMPLETE.
DO NOT RUN `zig build`.
DO NOT RUN `zig test`.
DO NOT RUN FILTERED TESTS.
DO NOT RUN `eval` TESTS.
DO NOT RUN `repl` TESTS.
DO NOT DO "JUST A QUICK COMPILE CHECK."
DO NOT USE THE COMPILER AS A MIGRATION DETECTOR.

## Absolute Cutover Rule

Running Zig is forbidden until the architectural cutover is completely finished.

This is a hard gate, not a sequencing preference.

The first Zig run is not part of the migration.
The first Zig run happens only after the migration is already complete.

If any transitional artifact remains anywhere in the repository, Zig must not be run.

That prohibition includes:

- `zig build`
- `zig test`
- filtered test runs
- backend-specific runs
- `eval` runs
- `repl` runs
- "quick" compile checks
- "just to see what breaks" runs

Compiler errors must not be used to discover what still needs to be migrated.
That discovery must happen statically by editing blind, deleting the old architecture, and doing repo-wide grep/audit checks first.

# Ideal Lambda-Set / MIR / LIR Architecture Plan

## Why This Plan Exists

This plan exists because the current compiler still blends together several responsibilities that `cor` deliberately separated:

- exact monotype determination
- lambda-set solving
- lambda-set specialization
- executable IR lowering

That blending is precisely the design risk that `cor` was built to avoid.

Per the motivation for `cor`, the old compiler architecture in `crates/` had correctness bugs that were not realistically fixable within its phase structure. The `cor` prototype demonstrated that the bug class goes away if we:

1. solve lambda sets in their own explicit pass
2. specialize solved lambda sets in their own explicit pass
3. only then lower into executable IR

This plan adopts that lesson completely.

This is not an incremental “make current code a little nicer” plan.
This is a full architectural replacement plan.

The final state must leave no trace of the old architecture:

- no mixed callable-inference/lowering logic
- no callable-resolution recovery in `MirToLir`
- no closure-ness decisions in `Lower`
- no compatibility shims preserving the old design
- no transitional side paths

The cutover strategy is:

1. edit blind
2. replace the architecture completely
3. delete the old architecture completely
4. statically prove that no transitional artifacts remain
5. only then run Zig for the first time
5. then get `eval` tests passing
6. then get `repl` tests passing

No shortcuts. No workarounds. No dual-path compatibility layer.

## Transitional Artifact Definition

For the purposes of this plan, a transitional artifact is any remnant of the old architecture or any bridge between the old and new architecture.

If even one of these exists, the cutover is not complete and Zig must not be run.

Transitional artifacts include:

- an old pass still wired into the active pipeline
- an old data structure still consulted by active code
- a compatibility shim
- a dual-path code path
- a temporary bridge from old architecture to new architecture
- a helper that keeps old semantics alive "for now"
- a TODO that exists only to preserve old behavior during migration
- a comment describing removed architecture as if it still exists
- an obsolete entrypoint that is still reachable
- a summary/recovery pass that exists only because earlier stages do not make the new semantics explicit yet
- an old test or helper that only exists to support the removed architecture
- a wrapper/facade that preserves the old phase boundary instead of replacing it
- any place where later lowering still recovers semantics that the new architecture is supposed to make explicit earlier

The key rule is:

- if a repository-wide grep can still find an old architecture concept and that concept is still semantically live, then the migration is not done
- if a new component still needs old metadata, old summaries, old recovery logic, or old bridge code to function, then the migration is not done

## Non-Negotiable Principles

1. Correctness is the primary design constraint.
2. The phase split must make the known bug classes impossible or dramatically harder by construction.
3. Lowering must never be allowed to “figure out” semantic callable identity on the fly.
4. MIR -> LIR must be mechanical.
5. Backends must remain completely ignorant of RC beyond explicit `incref` / `decref` statements.
6. Defaulting must be explicit and owned by one phase only.
7. No phase may rely on best-effort or fallback behavior.
8. Any intentionally missing implementation must be a loud debug panic.
9. Final architecture must contain zero obsolete data structures, code paths, comments, or tests from the replaced architecture.
10. The first compiler run is a post-cutover certification step, not a migration aid.
11. If a compiler run reveals mixed old/new architecture, stop testing immediately and return to blind editing and deletion.

## Background: What `cor` Actually Demonstrated

The key `cor` pipeline is in:

- `/Users/rtfeldman/code/cor/experiments/lss/lss.ml`

The relevant stage boundary is:

- `monotype_lifted`
- `lambdasolved`
- `lambdamono`
- `ir`

That separation is explicit in:

- `/Users/rtfeldman/code/cor/experiments/lss/lss.ml`

The important point is that `cor` does not merely have “some monomorphization step.”
It has two distinct lambda-related stages after monomorphic type information exists:

1. `lambdasolved`
2. `lambdamono`

### What `lambdasolved` means in `cor`

In:

- `/Users/rtfeldman/code/cor/experiments/lss/lambdasolved/type.ml`

the solved type representation explicitly contains lambda sets:

- `captures = tvar SymbolMap.t`
- `lambda_set = captures SymbolMap.t`
- `LSet of lambda_set`

So after `lambdasolved`, a function-valued thing is not “still some abstract function type whose executable meaning will be rediscovered later.”

It is already a solved set of lambda members, and each member already knows its capture map.

That is the critical semantic boundary.

### What `lambdamono` means in `cor`

In:

- `/Users/rtfeldman/code/cor/experiments/lss/lambdamono/lower.ml`

`lambdamono` consumes solved lambda sets and turns them into explicit executable structure:

- function values become explicit runtime values
- closure members get explicit captures payload
- calls on lambda-set values become explicit branches over members
- specialized functions gain explicit captures arguments when needed

This is the critical executable boundary.

After `lambdamono`, IR lowering is no longer trying to solve lambda sets.
It is only lowering already-specialized callable behavior.

### Why `cor` is designed that way

Because conflating these questions creates correctness hazards:

- “what is the exact monotype here?”
- “what lambda-set members are possible here?”
- “what runtime captures does this callable require?”
- “what code should be emitted for this call?”

If one phase tries to answer all of them while simultaneously lowering code, then:

- traversal order leaks into semantics
- partially-known information can harden into IR
- closure-ness can be rediscovered inconsistently
- generic function templates can be lowered in the wrong specialization context
- downstream passes are forced to recover semantic intent from executable artifacts

`cor` separates these concerns specifically so that:

- lambda-set reasoning is complete before executable lowering
- specialization is complete before executable lowering
- later lowering is mechanical

That is the architecture this plan adopts.

## Diagnosis Of The Current Zig Architecture

The current eval pipeline is effectively:

1. `Monomorphize`
2. `Lower`
3. `MIR Analyses`
4. `MirToLir`
5. `RcInsert`

This is visible in:

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/cir_to_lir.zig`

There is already some good structure here:

- exact callable instantiation has an explicit representation
- MIR is statement-only
- MIR is already much closer to strongest form than before
- LIR is already explicit and RC insertion is a separate pass

Those are good ingredients.

The problem is that the lambda-set and specialization responsibilities are still smeared across multiple phases.

### Current problem areas

1. `Monomorphize.zig` mixes context monotype determination, callable-inst discovery, and pieces of specialization planning.
2. `Lower.zig` still decides whether a callable inst produces a closure value.
3. `Lower.zig` still constructs runtime capture materialization plans while lowering MIR.
4. `Lower.zig` still builds callable-binding metadata to help later phases recover callable identity.
5. `MirToLir.zig` still resolves callable identity from parameters, joins, refs, and call results.
6. `CallableSummary` and related MIR analyses still exist partly because callable meaning is not fully explicit earlier.

This means the current architecture is still not equivalent to `cor`.

### Concrete symptoms of the current blend

The current compiler has artifacts like:

- `CallableTemplateId`
- `CallableInstId`
- `CallableInstSetId`

in `Monomorphize`, which is good.

But it also still has:

- `CallableBinding`
- `callable_bindings`
- callable-projection chasing
- callable resolution through joins and params in `MirToLir`
- closure-ness classification in `Lower`

Those are exactly the artifacts that should disappear once `LambdaSolved` and `LambdaSpecialize` are truly separate.

### Current dispatch transitional artifact

There is also a specific transitional artifact in dispatch resolution.

Today the compiler still has two different generations of dispatch knowledge:

1. checker-era / module-global static-dispatch facts
2. specialization-context-sensitive dispatch facts

The checker-era facts are too early and too weak:

- they are recorded before the new specialization architecture is finished
- they are not the final semantic owner of executable dispatch
- they are not the right abstraction boundary for lowering
- they cannot be the long-term source of truth for specialized generic code

The specialization-context-sensitive facts are closer to the correct architecture, but they still currently coexist with the checker-era side channel instead of fully replacing it.

This is unacceptable in the ideal design.

Long-term there must be exactly one active dispatch-resolution authority, and it must live in the new staged pipeline.

`Lower` must never have to decide:

- whether to trust a checker-era dispatch table
- whether to trust a specialization-era dispatch table
- whether to retry dispatch resolution from exact monotypes
- whether to bridge from one dispatch source to another

That entire class of ambiguity must be deleted.

## The Ideal Long-Term Architecture

The ideal Zig compiler pipeline is:

1. parse / canonicalize / HM solve
2. `CoreCIR`
3. `ContextMono`
4. `LambdaSolved`
5. `LambdaSpecialize`
6. `MIR`
7. `MIR Analyses`
8. `LIR`
9. `RC Insert`
10. backend

This can still be demand-driven and memoized by root and instantiation.
“Separate stage” means separate semantic ownership and separate data structures, not necessarily whole-program eager execution.

## Stage 1: `CoreCIR`

### Purpose

`CoreCIR` is the normalized structured source IR that removes frontend sugar but does not yet flatten into ANF or CFG.

### Responsibilities

- desugar `if` into `match`
- normalize short-circuit forms into explicit structured matches
- normalize dot-call syntax
- resolve cross-module identifiers into explicit def references / global references
- eliminate purely syntactic record/list sugar that should not matter downstream
- canonicalize pattern shapes that can be normalized without semantic loss

### Explicitly not allowed in `CoreCIR`

- no monotype defaulting
- no lambda-set solving
- no closure-ness decisions
- no MIR-local generation
- no layout reasoning

### Why this stage exists

This keeps later semantic passes from having to care about frontend syntactic noise.
It lets us keep MIR simple without forcing every semantic phase to operate on ANF CFG.

## Stage 2: `ContextMono`

### Purpose

This stage owns exact source-level monotypes for reachable program nodes, in specialization context.

This stage is the only place where language defaulting is allowed.

### Responsibilities

- compute exact monotypes for reachable expressions and bindings in context
- own all typevar substitution / contextual monotype lookup
- resolve static dispatch using exact monotypes
- perform explicit language defaulting
- record exact monotypes keyed by specialization context

### Explicitly allowed defaulting

Only here:

- unconstrained numeric defaults
- unconstrained empty structure defaults
- any other deliberate Roc language defaulting rule

### Explicitly forbidden outside this stage

- no defaulting in `Lower`
- no defaulting in MIR analyses
- no defaulting in `MirToLir`

### Correct-by-construction rule

Any phase after `ContextMono` that asks for a source-level monotype must get an exact answer or debug-panic.

No later phase is allowed to consult raw unresolved typevars and “try to make a monotype out of them.”

### Ideal static-dispatch ownership

`ContextMono` must be the only semantic owner of source-level static dispatch resolution.

That means:

- any `e_type_var_dispatch`
- any associated-method lookup resolved from an exact receiver monotype
- any cross-module method implementation selection driven by exact type context

must be resolved here and recorded here.

The output of `ContextMono` should not merely be “some monotype facts plus some leftover dispatch constraints.”
It should contain an explicit resolved-dispatch artifact, conceptually like:

- `ResolvedDispatchSiteId`
- `ResolvedDispatchTargetId`

Each resolved static-dispatch site should contain, conceptually:

- specialization context id
- module id
- expr id
- method name / symbol
- exact receiver monotype
- exact implementation owner
- exact callable template or explicit external/hosted implementation target

This is the only place where checker-era dispatch constraints are allowed to be consulted.

Those checker-era facts must be imported, normalized, or discarded inside `ContextMono`.
They must not remain exposed as an alternate downstream dispatch source.

### Correct-by-construction dispatch rule

After `ContextMono`, there must no longer be any such thing as “look up dispatch from raw typechecker constraints.”

There is only:

- explicit resolved static dispatch owned by `ContextMono`

Any later phase that sees a source-level dispatch node without an attached `ContextMono` resolution must debug-panic.

## Stage 3: `LambdaSolved`

### Purpose

This is the direct analogue of `cor`’s `lambdasolved`.

It owns all solved lambda-set information, independently of executable lowering.

### Responsibilities

- compute solved callable sets for function-valued nodes
- compute solved member sets for higher-order params, expressions, pattern bindings, and call sites
- compute capture provenance for each solved member
- distinguish direct member vs closure member semantically
- intern solved lambda-set and capture-plan structures

### Key design requirement

A solved member must include both:

- callable template identity
- capture provenance plan

This is the `cor` lesson.
A solved lambda-set member is not merely “lambda X.”
It is “lambda X with capture plan Y in specialization context Z.”

### Ideal core data structures

The exact names are flexible, but the architecture should contain equivalents of:

- `LambdaSetId`
- `LambdaSetMemberId`
- `CapturePlanId`
- `SolvedCallableValueId`
- `SolvedCallSiteId`

### `LambdaSetMember`

Should conceptually contain:

- callable template id
- specialization context id
- solved function monotype id
- capture plan id
- representation kind:
  - direct
  - closure
  - hosted / foreign / explicit external callable, if needed

### `CapturePlan`

Should conceptually contain:

- ordered capture entries
- each entry names its exact source
- each entry records the exact source monotype
- no executable MIR locals here
- only semantic capture provenance

Allowed capture sources:

- lexical binding
- closure parent capture
- top-level constant/def
- exact callable member
- explicit source expr, if semantically required

Forbidden:

- “discover capture later in MIR”
- “guess capture shape from runtime closure expr”

### `LambdaSolved` output should answer questions like

- what callable members can this higher-order param take?
- for this call site, which members are possible?
- for this function value, which members are possible?
- for this lambda member, what exact captures does it semantically require?

### How static dispatch enters `LambdaSolved`

Resolved static dispatch from `ContextMono` should enter `LambdaSolved` exactly once, as a singleton callable source.

That is:

- a source-level static-dispatch expr is not a separate downstream dispatch mechanism
- it is just one solved callable source with one exact member

So `LambdaSolved` should treat:

- explicit named callable refs
- resolved static-dispatch refs
- hosted/foreign callable refs

as peers in the solved callable world.

This removes the need for any separate downstream “dispatch target lookup” escape hatch.

### `LambdaSolved` must not

- emit executable code
- create MIR locals
- decide runtime layout
- choose final MIR statement shapes

## Stage 4: `LambdaSpecialize`

### Purpose

This is the direct analogue of `cor`’s `lambdamono`.

It consumes solved lambda sets and turns them into explicit executable callable behavior before MIR lowering.

### Responsibilities

- create specialized callable defs from callable templates plus solved context
- choose exact closure ABI
- choose exact capture field order
- choose exact direct-vs-closure runtime value representation
- turn higher-order calls into explicit executable dispatch

### The most important rule

After `LambdaSpecialize`, there must be no lambda-set reasoning left.

Not in `Lower`.
Not in MIR analyses.
Not in `MirToLir`.

### What specialization should decide

For each callable member:

- specialized def identity
- explicit arg monotypes
- explicit ret monotype
- whether there is a hidden captures arg
- exact captures payload schema
- exact direct callable representation or closure representation

For each higher-order call:

- if singleton solved member:
  - direct specialized call
- if multiple solved members:
  - explicit structured dispatch IR

For each resolved static-dispatch site:

- consume the singleton solved member produced from `ContextMono`
- produce an explicit specialized direct-call target
- or produce an explicit specialized hosted/foreign callable target

There must be no second dispatch-resolution step here.
`LambdaSpecialize` specializes already-resolved dispatch; it does not rediscover it.

### Structured dispatch

Follow the `cor` model:

- a function-valued runtime value is already explicit
- calling it becomes explicit branching over its solved representation

This means specialization must output a structured specialized IR that already contains:

- explicit callable constructors
- explicit callable destructuring
- explicit dispatch branching

### Output IR of this stage

This stage should not emit MIR directly.

It should emit a specialized structured IR, conceptually similar to:

- `SpecializedCIR`
- or `LambdaMonoIR`

This IR is the Zig analogue of `cor`’s `lambdamono`.

### `SpecializedCIR` should contain

- explicit specialized defs
- explicit direct-call targets
- explicit closure constructors
- explicit dispatch branches
- explicit capture destructuring at function entry

It must not contain:

- raw `e_type_var_dispatch`
- raw associated-method lookup nodes
- unresolved cross-module method selection
- any placeholder saying “resolve this dispatch later”

It can still be tree-structured.
It does not need to be ANF yet.

### What must disappear because of this stage

The following concepts should not survive past `LambdaSpecialize`:

- lambda-set ambiguity
- function-valued param callable-binding recovery
- closure-ness classification in `Lower`
- callable projections used to recover identity later
- implicit hidden capture reasoning in MIR -> LIR

## Stage 5: `MIR`

### Purpose

MIR is the ANF / SSA / CFG lowering target after specialization is already complete.

This is where the compiler should flatten control flow and values aggressively.

### Ideal MIR properties

- statement-only
- SSA-style local uniqueness
- explicit `join` / `jump`
- explicit `LocalId -> LocalDef`
- no patterns
- no `if`
- no frontend sugar
- no unresolved cross-module names
- no static dispatch
- no lambda-set reasoning
- no callable recovery machinery

### MIR should be lowered from `SpecializedCIR`, not from raw `CIR`

This is a major design point.

The current `Lower` is `CIR -> MIR`.
The ideal `Lower` is:

- `SpecializedCIR -> MIR`

That is the right analogue to `cor`’s `lambdamono -> ir`.

### Dispatch after specialization

After `LambdaSpecialize`, dispatch has only two legal forms:

1. explicit direct call to a specialized callable target
2. explicit structured branch over an already-specialized callable runtime representation

There is no third form.

In particular, MIR must not contain:

- source-level static-dispatch expressions
- method-lookup expressions
- “call this function-valued local and recover the target later”
- “this expr has a dispatch target in some side table”

### Ideal MIR callable model

The ideal MIR should not need generic “call a function-valued local and figure out what it is.”

Instead:

- direct specialized calls are explicit
- closure construction is explicit
- dynamic callable dispatch is already expanded into explicit MIR control flow

So MIR call forms should be much simpler than they are today.

### Ideal MIR call statements

Ideal end state:

- `assign_call_direct`
- `assign_low_level`
- maybe one explicit external/hosted call form if needed

Not ideal:

- `assign_call` carrying `exact_lambda`
- `assign_call` carrying `exact_requires_hidden_capture`
- downstream phases re-resolving callee identity

### MIR closure representation

MIR may still contain explicit closure construction, but it must be fully explicit:

- specialized lambda/proc target
- explicit capture local span
- fixed hidden captures arg ABI

No inference remains.

## Stage 6: `MIR Analyses`

### Purpose

These analyses should answer only non-callable structural questions needed by later lowering.

Good uses:

- no-return analysis
- result alias/borrow provenance
- ownership/provenance summaries

Bad uses:

- recovering callable identity
- deciding closure-ness
- determining hidden capture ABI

### Desired cleanup

`CallableSummary` should either disappear entirely or collapse into trivial verification that specialized callable facts are internally consistent.

If an analysis still needs to rediscover callable identity, specialization was incomplete.

## Stage 7: `LIR`

### Purpose

Pure layout lowering.

### Responsibilities

- translate MIR locals/CFG into layout-aware locals/CFG
- translate explicit calls into proc-level calls
- translate explicit closure payloads into explicit runtime data
- preserve explicit ownership statements

### Explicitly forbidden

- no callable identity recovery
- no capture-plan reasoning
- no RC inference

## Stage 8: `RC Insert`

### Purpose

Insert explicit `incref` / `decref`.

### Downstream rule

After this stage, all backends are dumb RC consumers.
Backends must not infer RC behavior.

## The Ideal Module Layout

Exact file names can vary, but the architecture should look roughly like this:

- `src/corecir/CoreCIR.zig`
- `src/corecir/Normalize.zig`
- `src/corecir/ContextMono.zig`
- `src/corecir/LambdaSolved.zig`
- `src/corecir/LambdaSpecialize.zig`
- `src/corecir/SpecializedCIR.zig`
- `src/mir/MIR.zig`
- `src/mir/Lower.zig`  // now lowers `SpecializedCIR -> MIR`
- `src/mir/Analyses.zig`
- `src/lir/MirToLir.zig`

The old giant `Monomorphize.zig` should not remain as the semantic owner of all three concerns.

It must be split.

If a thin orchestration wrapper remains, it must only orchestrate the new distinct stages.
It must not continue to own the old blended semantics.

## Correct-By-Construction Rules

### Rule 1: specialization context must always be explicit

Any source-level monotype, solved lambda-set fact, or specialized callable fact must be keyed by explicit specialization context.

No naked template-body lowering.

### Rule 2: `Lower` never decides semantics

`Lower` may only:

- translate already-normalized structured IR
- translate already-specialized callable behavior
- build MIR locals / CFG

It may not:

- discover callable sets
- choose closure-ness
- decide capture schema
- decide dispatch shape
- resolve static dispatch
- choose between checker-era dispatch facts and specialization-era dispatch facts

### Rule 3: `MirToLir` never recovers callable identity

Delete all forms of:

- param binding callable recovery
- join callable override recovery
- projected callable path recovery
- call-result callable inference

### Rule 4: MIR callable values must already be explicit

Any function-valued runtime value that reaches MIR must already have an explicit executable representation chosen by specialization.

### Rule 5: exact source monotypes come only from `ContextMono`

No other phase may consult unresolved typevars and decide a source-level monotype.

### Rule 6: the phase lattice must be strictly narrowing

Each stage removes semantic ambiguity from the previous stage.
No later stage is allowed to recreate ambiguity that should already have been settled.

### Rule 7: dispatch authority is singular

There must be exactly one active dispatch-resolution authority in the compiler:

- `ContextMono` for semantic source-level static dispatch resolution

and exactly one executable dispatch-shape owner:

- `LambdaSpecialize` for executable callable dispatch shape

No other phase may own or expose an alternate dispatch-resolution channel.

In particular:

- `Lower` must not preload typechecker dispatch constraints
- `Lower` must not have a fallback dispatch table
- `MirToLir` must not recover dispatch targets
- no phase may consult raw checker static-dispatch constraints except `ContextMono`
- no phase may keep a second side table of dispatch results once `ContextMono` has normalized them

## Current Code To New Stage Mapping

This section maps major current artifacts to the new architecture.

### Current `Monomorphize.Result` tables

These should be split as follows.

#### Move to `ContextMono`

- `context_expr_monotypes`
- `context_pattern_monotypes`
- source-level monotype substitution / binding tables
- import and normalization of checker-era static-dispatch constraints
- exact resolved static-dispatch sites
- exact dispatch target templates / hosted targets keyed by specialization context

#### Move to `LambdaSolved`

- `expr_callable_inst_sets`
- `call_site_callable_inst_sets`
- `lookup_expr_callable_inst_sets`
- `context_pattern_callable_inst_sets`
- `closure_capture_callable_insts`
- `closure_capture_monotypes`
- singleton solved callable members induced by resolved static-dispatch sites
- any data that says “what callable members are possible here?”

#### Move to `LambdaSpecialize`

- exact specialized callable defs
- exact closure ABI
- exact direct/closure runtime representation
- call-site dispatch lowering decisions
- specialization of resolved static-dispatch sites into explicit direct-call targets
- any data that says “how do I execute this solved callable set?”

### Current `Lower.zig` responsibilities that must move out

Move out of `Lower` entirely:

- `callableInstProducesClosureValue`
- runtime closure capture counting as a semantic specialization question
- construction of `CallableBinding`
- callable-value interpretation of params/captures
- any use of specialization context to decide runtime callable representation

`Lower` should only consume specialization output.

Delete from `Lower` entirely:

- any module-global preloaded dispatch target map
- any direct use of checker static-dispatch constraints
- any fallback path that retries static dispatch from expr/module ids
- any logic that treats unresolved dispatch as something `Lower` should fix up

### Current `MirToLir.zig` responsibilities that must disappear

Delete:

- `resolveCallableForParamProjection`
- `resolveCallableFromJoinParam`
- `resolveCallableValuePath`
- join override machinery used only for callables
- param callable-binding lookup
- projected call-result callable recovery
- any dispatch-target recovery by expr id
- any dispatch fallback lookup
- any side table saying “the real target for this call is elsewhere”

If callable meaning is still implicit there, the new architecture has not been completed.

### Current MIR concepts to delete

Delete from final architecture:

- `CallableBinding`
- `callable_bindings`
- `CallableProjection`
- `exact_lambda` fields on generic call nodes
- `exact_requires_hidden_capture`
- any MIR artifact whose purpose is “help later phases rediscover callable identity”

### Current analyses to simplify/delete

- `CallableSummary` should be deleted or reduced to invariant checking.
- `ResultSummary` can remain, but only for provenance / no-return / ownership semantics.

## Ideal Final Pipeline Execution

The ideal runtime/eval lowering sequence should become:

1. build `CoreCIR`
2. run `ContextMono`
3. run `LambdaSolved`
4. run `LambdaSpecialize`
5. lower `SpecializedCIR -> MIR`
6. run MIR analyses
7. lower `MIR -> LIR`
8. run RC insertion

That replaces the current:

- `Monomorphize`
- `Lower`
- `Analyses`
- `MirToLir`

shape.

## Migration Strategy

This is the required migration order.

Do not run Zig during stages 1 through 10.
Perform the architectural replacement blind, then delete the old architecture, then run Zig for the first time.

## Stage 1: Write The New Modules First

Create the new phase modules and data types before touching the current pipeline entrypoints.

Add:

- `CoreCIR`
- `ContextMono`
- `LambdaSolved`
- `LambdaSpecialize`
- `SpecializedCIR`

Do not reuse old names if doing so obscures ownership.

If `Monomorphize.zig` remains temporarily during the edit, it should only be a thin coordinator and must be deleted or reduced to an obvious facade before the first compile run.

## Stage 2: Move Exact Source Monotypes Into `ContextMono`

Cut all exact source-level monotype ownership out of the old monomorphization blob and place it in `ContextMono`.

This includes:

- exact expr monotypes
- exact pattern monotypes
- defaulting
- static dispatch target monotypes
- cross-module substitution import/export

Then extend that cutover to dispatch ownership:

- move all checker static-dispatch imports into `ContextMono`
- define explicit resolved static-dispatch site records
- make `ContextMono` the only stage that can translate a source-level dispatch expr into an exact implementation target
- prohibit every other phase from consulting raw checker dispatch constraints

At the end of this step:

- `ContextMono` must own exact source monotypes
- `Lower` must have no semantic right to default anything
- `ContextMono` must own all source-level static dispatch resolution

Do not leave compatibility duplicate tables behind.

## Stage 2.5: Collapse Dispatch Resolution To Its Ideal Form

This is the dedicated cutover for the current dispatch transitional artifact.

### Goal

End with exactly one semantic dispatch authority and zero downstream recovery paths.

### Step 1: Introduce explicit resolved static-dispatch artifacts

Add `ContextMono` data structures for resolved static dispatch, conceptually:

- `ResolvedDispatchSiteId`
- `ResolvedDispatchTargetId`

The exact naming can differ, but the architecture must support:

- site identity
- specialization context identity
- source expr identity
- exact receiver monotype
- exact implementation target

### Step 2: Import checker-era dispatch facts only inside `ContextMono`

Any existing typechecker/module-env static-dispatch constraints must be read only here.

They may be:

- normalized into `ContextMono`’s resolved-dispatch records
- validated against exact monotypes
- discarded if redundant

They must not remain exposed as a second active downstream table.

### Step 3: Make `LambdaSolved` consume resolved static dispatch as singleton callable sources

Do not let `LambdaSolved` have a separate notion of “maybe dispatch, maybe callable ref.”

Resolved static dispatch should just become:

- one solved callable member
- with one exact implementation target

### Step 4: Make `LambdaSpecialize` consume only solved dispatch, never raw dispatch

`LambdaSpecialize` must not:

- consult checker dispatch constraints
- consult raw dispatch expr ids in isolation
- recover static dispatch from source-level monotypes on demand

It should only consume:

- resolved static-dispatch sites from `ContextMono`
- solved callable members from `LambdaSolved`

### Step 5: Remove all dispatch logic from `Lower`

Delete:

- preloaded dispatch maps
- dispatch fallback lookup helpers
- direct dispatch-target resolution helpers
- any code path where `Lower` treats dispatch as an unresolved semantic problem

`Lower` should only see specialized direct-call targets or explicit specialized dispatch structure.

### Step 6: Remove all dispatch recovery from MIR/LIR

Delete:

- dispatch-target lookup in MIR lowering helpers
- dispatch-target recovery in MIR analyses
- dispatch-target recovery in `MirToLir`

If a later phase still needs to know “what does this source-level dispatch mean?”, the cutover is incomplete.

### Step 7: Add structural verification

Add verification so that:

- `SpecializedCIR` cannot contain raw source-level dispatch expressions
- MIR cannot contain raw source-level dispatch expressions
- no downstream phase can be given unresolved dispatch nodes without a debug panic

### Step 8: Delete the old dispatch vocabulary

After cutover, delete obsolete names and comments referring to:

- fallback dispatch lookup
- pre-resolved dispatch tables in `Lower`
- checker-versus-specialization dispatch choice
- any comment implying dispatch will be recovered later

### Dispatch acceptance criteria

This cutover is complete only if all of the following are true:

- there is one semantic source of truth for static dispatch: `ContextMono`
- there is one executable dispatch-shape owner: `LambdaSpecialize`
- `LambdaSolved` consumes resolved static dispatch as ordinary solved callable members
- `Lower` has zero dispatch lookup/recovery logic
- MIR analyses have zero dispatch lookup/recovery logic
- `MirToLir` has zero dispatch lookup/recovery logic
- no active code outside `ContextMono` reads checker static-dispatch constraints
- no active code outside `LambdaSpecialize` chooses executable dispatch shape

## Stage 3: Create `LambdaSolved` As Its Own Semantic Output

Take the callable-set and capture-provenance pieces currently smeared through `Monomorphize` and normalize them into explicit solved data structures.

Required outputs:

- solved callable sets for exprs
- solved callable sets for patterns
- solved callable sets for call sites
- solved capture plans for each lambda-set member

Key requirement:

The solved stage must be able to answer semantic questions without referring to MIR or LIR.

## Stage 4: Create `SpecializedCIR`

Add a new structured IR for the output of `LambdaSpecialize`.

It should be tree-structured, not ANF yet.

Required forms:

- explicit specialized def references
- explicit direct calls
- explicit closure constructors
- explicit capture destructuring at function entry
- explicit dispatch `match` / `switch` over function-value representation

This IR is the executable input to MIR lowering.

## Stage 5: Implement `LambdaSpecialize`

Consume `LambdaSolved` and emit `SpecializedCIR`.

This step must:

- specialize callable templates into specialized defs
- assign exact capture ABI
- assign exact function-value runtime representation
- lower higher-order calls into explicit dispatch structure

When this step is finished, no later phase may need lambda-set reasoning.

## Stage 6: Rewrite `Lower` To Consume `SpecializedCIR`

This is the biggest semantic cutover.

Change `Lower` from:

- `CIR -> MIR`

to:

- `SpecializedCIR -> MIR`

At this point:

- delete any remaining source-level callable inference from `Lower`
- delete closure-ness classification from `Lower`
- delete callable-binding generation from `Lower`
- delete any need for `Lower` to rediscover semantic captures

`Lower` should now only:

- create MIR locals
- build CFG
- flatten control flow
- lower explicit closure construction
- lower explicit dispatch control flow

## Stage 7: Simplify MIR To Match The New World

After specialization has been pulled out, simplify MIR aggressively.

Required MIR cleanup:

- keep no `if`
- keep no patterns
- keep no syntactic sugar
- keep explicit SSA local defs
- keep only explicit executable callable representation

Ideal call cleanup:

- remove generic function-valued call recovery fields
- move toward direct specialized call statements

If a dynamic callable dispatch remains, it must exist as already-expanded CFG, not as an implicit function-valued call that later phases interpret.

## Stage 8: Rewrite `MirToLir` To Be Mechanical

Delete all callable-resolution logic.

Delete:

- param projection callable recovery
- join override callable recovery
- ref-path callable recovery
- call-result callable recovery

`MirToLir` should only map already-explicit MIR semantics to layout-aware LIR.

At the end of this step:

- `MirToLir` must not know what a lambda set is
- `MirToLir` must not guess hidden captures

## Stage 9: Delete Old Analyses And Data Structures

Delete anything that only existed to support the old callable recovery architecture.

This includes, unless they have been reduced to obvious invariant checks:

- `CallableSummary`
- `CallableBinding`
- `CallableProjection`
- callable join override maps
- current callable binding environment hacks

Do not leave dead typedefs, comments, tests, or helper functions around.

## Stage 10: Delete The Old Pipeline Entry Structure

Update:

- `src/mir/mod.zig`
- `src/eval/cir_to_lir.zig`
- snapshot / repl / eval entrypoints

so they reflect the new phase order.

Delete any misleading comments such as “lambda set inference happens later on top of MIR.”

The pipeline entry code should clearly show:

1. `CoreCIR`
2. `ContextMono`
3. `LambdaSolved`
4. `LambdaSpecialize`
5. `SpecializedCIR -> MIR`
6. `MIR -> LIR`

## Stage 11: Repo-Wide Deletion Pass Before First Compile

Before the first Zig run, do a full static deletion pass.

Delete every obsolete artifact of the old architecture.

The codebase must contain no remaining:

- callable-binding recovery paths
- callable-summary recovery paths
- checker-era dispatch side paths visible to lowering
- specialization-era dispatch side paths duplicated elsewhere
- old `Monomorphize` all-in-one ownership
- comments describing the old blended pipeline
- tests aimed only at the removed architecture

Do repo-wide grep checks before the first compile run.

Targets that should become empty or only contain new-stage names:

- `CallableBinding`
- `callable_bindings`
- `CallableProjection`
- `exact_requires_hidden_capture`
- `resolved_dispatch_targets` in lowering
- direct reads of raw checker static-dispatch constraints outside `ContextMono`
- old blended `Monomorphize` comments
- callable resolution in `MirToLir`
- any helper whose job is “resolve exact callable later”
- any helper whose job is “resolve exact dispatch later”

## Stage 11.5: Mandatory Static Cutover Checklist

The following checklist must be satisfied before the first Zig run.

This is a hard gate.
If any item is not true, do not run Zig.

1. The new pipeline stages exist and are wired into the active pipeline entrypoints.
2. The old pipeline stages are deleted, unreachable, or reduced to inert scaffolding that is no longer semantically live.
3. No active code path still consumes old callable-resolution metadata.
4. No active code path still consumes old closure-recovery metadata.
5. No active code path still consumes old dispatch-recovery metadata.
6. No active code path still consumes old monomorphization/lowering bridge structures.
7. No compatibility shim or dual-path bridge remains.
8. No old entrypoint still routes through removed architecture.
9. Repo-wide grep confirms that old architecture symbols either no longer exist or exist only in clearly-dead/deleted historical contexts that are about to be removed.
10. Comments and helper names describe only the new architecture.
11. Old tests/helpers aimed only at the removed architecture are deleted or rewritten for the new architecture.
12. `plan.md` has been followed through complete architectural replacement, not partial migration.

The first Zig run is permitted only after all 12 items are true.

## Stage 12: First Compiler Run

Only after stages 1 through 11 are complete:

1. run Zig
2. fix compile errors
3. fix semantic errors

No shortcutting by compiling halfway through the cutover.

The point is to avoid preserving bad architecture because the compiler still needs to build mid-transition.

If the first Zig run reveals that transitional artifacts still exist, that means the run was premature.

In that case:

1. stop running Zig immediately
2. return to blind architectural editing
3. delete the remaining transitional artifacts
4. redo the static cutover checklist
5. only then resume compiler/test runs

Do not continue with compile-fix-test iteration while mixed architecture still exists.
The compiler must not be used as a detector for unfinished migration work.

## Stage 13: Eval Tests

After the new architecture compiles:

1. run eval tests
2. fix all failures
3. keep fixes architectural, not tactical

Priority order:

- fundamental callable execution correctness
- closure captures
- runtime/control-flow correctness
- ownership / RC correctness
- builtins after fundamentals

## Stage 14: REPL Tests

Only after eval tests pass:

1. run repl tests
2. fix failures
3. keep the same no-shortcuts rule

## Final Acceptance Criteria

The migration is done only if all of the following are true.

### Architecture criteria

- there is an explicit `CoreCIR` normalization stage
- there is an explicit `ContextMono` stage
- there is an explicit `LambdaSolved` stage
- there is an explicit `LambdaSpecialize` stage
- `Lower` lowers from specialized structured IR, not raw unspecialized CIR
- `MirToLir` is mechanical

### Correctness criteria

- no source-level defaulting occurs outside `ContextMono`
- no callable identity recovery occurs in MIR or LIR lowering
- no closure-ness decisions occur in `Lower`
- all function-value dispatch is explicit before MIR lowering

### Cleanup criteria

- zero old architecture artifacts remain
- zero comments describe the old architecture
- zero dead helpers remain
- zero compatibility shims remain
- zero transitional wrappers remain

### Testing criteria

- compiler builds
- eval tests pass
- repl tests pass

### Sequencing criteria

- no Zig invocation occurred before the mandatory static cutover checklist was satisfied
- the first Zig run happened only after the old architecture had been fully replaced and deleted
- no compile-fix-test iteration was used to discover unfinished migration work

## Final Note

This plan is intentionally not optimized for “smallest diff” or “lowest temporary breakage.”

It is optimized for the best long-term compiler architecture:

- `cor`-style semantic separation
- strongest-form MIR
- mechanical MIR -> LIR lowering
- correct-by-construction phase boundaries
- zero tolerance for leftover blended-architecture debris

That is the target.
