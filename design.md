# Roc Post-Check Compiler Design

This document describes the architecture that compiler work in this repository
must preserve. It is written for contributors and coding agents that need to
change checking finalization, MIR lowering, IR/LIR lowering, ARC insertion,
runtime-image publication, glue, backends, or tests.

While both `plan.md` and `design.md` exist, new code changes use `design.md` as
the design reference.

## Core Principles

Compiler stages after parsing and error reporting must not use workarounds,
fallbacks, heuristics, or best-effort reconstruction.

Every stage after checking consumes explicit data published by earlier stages.
If a stage needs semantic data that was not published, the producer is
incomplete. The consumer must not recover the missing data from source syntax,
names, body scans, display strings, runtime bytes, object symbols, or backend
state.

All user-facing failures are reported during checking at the latest. Checking is
not complete until type checking, static-dispatch finalization, platform/app
relation publication, compile-time constant evaluation, and callable promotion
have all completed. After a checked artifact is published, any violated
assumption is a compiler bug:

```text
debug build: debug-only assertion
release build: unreachable
```

Post-check stages do not return user-facing semantic errors. They do not emit
fallback code. They do not silently repair missing data. They do not add
release-build runtime checks for compiler invariants.

Backends do not reason about reference counting. They lower and execute the
explicit LIR `incref`, `decref`, and `free` statements emitted before backend
code generation. Reference-counting policy belongs to LIR ARC insertion.

The compiler does not contain a static borrow, alias-permission, lifetime,
uniqueness, parameter-mode, or escape-summary model in this architecture.
Automatic reference counting is intentionally simple and mechanical. Runtime
mutation uses `refcount == 1` to decide whether in-place mutation is allowed.

Roc functions have fixed arity. Roc functions are not automatically curried.
The type:

```roc
Str, Str -> Str
```

is one function that takes exactly two arguments. It is not:

```roc
Str -> (Str -> Str)
```

The compiler must not synthesize partial-application closures, curried call
chains, or missing-argument wrappers unless Roc source explicitly constructs a
function value that returns another function.

`Bool` is an ordinary nominal tag union at runtime. Lowering and backends must
not special-case Bool runtime representation. Internal scalar predicate values
may exist only as control-flow implementation details; when a Roc value is
stored, returned, boxed, passed, exported, or refcounted, Bool uses the ordinary
tag-union representation selected by the layout pipeline.

## Hosted Function Ownership

Platform-hosted functions called through `RocOps.hosted_fns` receive ownership
of every refcounted argument. LIR ARC insertion transfers that ownership at the
hosted-call boundary the same way it transfers an unused argument to an ordinary
Roc callee. Backends must not add their own ownership decisions; they only lower
the explicit LIR `incref`, `decref`, and `free` statements.

The host may read, store, return, or release an owned argument. It must account
for that ownership explicitly:

```text
read and discard: decref the argument when done
store past return: move the argument ownership into storage, or incref a stored
                   copy and then decref the call argument
return the same value: move the argument ownership into the return slot, or
                       incref/decref so exactly one returned ownership remains
```

For example, `Stdout.line! : Str => {}` reads the string and then decrefs it.
A host function that stores a `Box(...)` argument either moves the received
ownership into storage, or increments the box for the stored reference and
decrefs the argument ownership it received. A round-trip host function that
returns one of its arguments must leave exactly one ownership for the returned
Roc value.

The compiler must not infer different ownership behavior from hosted function
names, return types, or body absence. Hosted-argument ownership is an ABI fact,
and generated glue must document it for platform authors.

## Reporting And Sentinel Values

Implementation data must never use sentinel/default values that can be mistaken
for real results.

If a value is valid only after a producer writes it, storage is initialized to
`undefined` or guarded by explicit presence/state metadata. Every consumer must
prove the producer ran before reading the value. A crash or invariant violation
must be reported as the actual crash or invariant violation; it must never be
disguised by reading a convenience default.

The cautionary example is an eval test harness state row initialized to
`NOT_IMPLEMENTED`. When mono specialization crashed before backend execution
began, the summary printed backend rows as `NOT_IMPLEMENTED`, even though no
backend had run. The correct report is that compilation/lowering crashed before
backend execution. Backend rows are absent until backend execution produces
them.

This rule applies everywhere: semantic data, lowering data, backend data, cache
data, RC data, runtime-image data, and test reporting. If code uses a sentinel
as a placeholder for data that must be produced, stop and redesign the producer
ownership and presence model.

## Pipeline Overview

The public post-check pipeline is:

```text
checked artifacts
  -> mono MIR
  -> row-finalized mono MIR
  -> lifted MIR
  -> lambda-solved MIR
  -> executable MIR
  -> IR
  -> LIR
  -> ARC insertion
  -> backend, interpreter, or runtime image
```

Compile-time constants use the same lowering architecture, but run inside
checking finalization:

```text
checked CIR
  -> checked artifact publication in progress
  -> mono MIR
  -> row-finalized mono MIR
  -> lifted MIR
  -> lambda-solved MIR
  -> executable MIR
  -> IR
  -> LIR
  -> ARC insertion
  -> LIR interpreter
  -> CompileTimeValueStore
```

The major implementation modules are:

```text
src/check/checked_artifact.zig
src/check/static_dispatch_registry.zig
src/mir/mono
src/mir/mono_row
src/mir/lifted
src/mir/lambda_solved
src/mir/executable
src/ir
src/lir
src/lir/arc.zig
src/lir/runtime_image.zig
src/backend
src/builtins
```

The only public semantic lowering entrance after checking is the
checked-artifact API in `src/lir/checked_pipeline.zig`. Callers provide
published checked artifacts, read-only import/lowering views, explicit root
requests, relation artifacts, and target configuration. Callers do not pass
loose checked CIR plus optional roots.

## Checked Boundary

Checked CIR is the last source-level representation. It may contain source
static-dispatch forms:

```text
e_dispatch_call
e_type_dispatch_call
e_method_eq
e_structural_eq
```

It owns source expression shape, checked expression types, static-dispatch
legality, implicit structural equality rewriting, nominal equality marking, and
custom equality marking.

It does not own executable method targets, executable layout decisions,
callable packaging, or lambda-set representation.

The checked boundary publishes immutable checked artifacts. A checked artifact
contains every checked-stage output that later stages or importing modules need:

- `ModuleEnv` storage and public exports
- module identity and checking context identity
- direct import artifact keys
- provides/requires metadata after checking finalization
- platform/app relation artifacts
- method registry
- normalized static dispatch plans
- resolved value-reference tables
- artifact-owned checked type store
- artifact-owned checked body store
- checked procedure templates
- promoted procedures
- root requests
- hosted procedure table
- platform-required binding table
- interface representation capabilities
- compile-time root table, when retained for diagnostics or verification
- `CompileTimeValueStore`
- `ConstInstantiationStore`
- `CallableBindingInstantiationStore`
- `SemanticInstantiationProcedureTable`
- private promoted-capture graph nodes
- callable result plans and callable promotion plans
- artifact-owned callable-set descriptor store
- constant reification plans
- nested procedure-site tables
- concrete dependency summaries

A published artifact is complete or it is not published.

### Checked Artifact Keys

The checked artifact cache key is target-independent:

```text
CheckedModuleArtifactKey =
    source_hash
  + compiler_artifact_hash
  + module_identity
  + checking_context_identity
  + direct_import_artifact_keys
```

The key does not include target ABI, pointer width, layout ids, field offsets,
alignment decisions, backend choice, object format, or code-generation options.
Target-specific materialization and object-code caches are separate caches.

`compiler_artifact_hash` is one build-time hash produced by `zig build`. It
covers compiler implementation identity, builtin module data, semantic-affecting
build options, generated builtin interfaces, and checked-artifact serialization
format.

`module_identity` answers which module this is: package identity, module name,
qualified name, module role, and any other semantic identity required by the
module graph.

`checking_context_identity` answers which external context the module was
checked under: package shorthand mapping, resolved imports, auto-import policy,
builtin import policy, platform/app relation, hosted/platform ABI context, and
provides/requires configuration.

`direct_import_artifact_keys` are the keys of direct imported checked artifacts,
not source hashes. This gives transitive invalidation by construction.

The checked artifact cache value restores one complete target-independent
artifact:

- module identity and checking context identity
- deterministic direct import records and imported checked artifact keys
- `ModuleEnv` storage after checked-artifact publication
- checked body store and checked type store
- public export table and provides/requires metadata
- method registry and static dispatch plans
- resolved value-reference table
- checked procedure templates and nested procedure-site table
- root request table
- hosted procedure table and platform-required binding table
- interface capability table
- platform/app relation artifacts when the artifact is checked in that context
- top-level value table
- compile-time root diagnostics data when retained
- `CompileTimeValueStore`
- `ConstInstantiationStore`
- `CallableBindingInstantiationStore`
- `SemanticInstantiationProcedureTable`
- promoted procedures and private promoted-capture data
- artifact-owned callable-set descriptor store
- constant reification plans
- callable result plans and callable promotion plans
- concrete dependency summaries

The cache hit unit includes every target-independent record needed to resume
post-check work: `ModuleEnv` storage when diagnostics/source tooling require it,
public exports, provides/requires, method registry, static-dispatch plans,
resolved refs, checked body and type stores, const/callable/procedure template
tables, root/hosted/platform tables, compile-time roots, compile-time value and
instantiation stores, semantic-instantiation tables, private capture graphs,
callable result and promotion plans, the artifact-owned callable-set descriptor
store, dependency summaries, relation artifacts, imported template closures,
interface capabilities, and canonical-name remap records. If any one component
required by the artifact is missing, the cache entry misses as a unit.

The cache has one compiler artifact hash. It does not store independent builtin
version, syntax version, static-dispatch version, layout version, or glue
version fields that can drift from the compiler artifact hash. If any of those
inputs affect checked-artifact semantics, they are part of the compiler artifact
hash or the checking context identity.

Import records in the cache are deterministic records, not hash-map snapshots.
They include the direct import module identity, resolved artifact key, local
alias/member exposure data, and canonical-name remapping data required by
importers. A cache hit must have the exact direct import checked artifact keys
that checking would have used. It must not accept a hit and then patch import
identity, platform relation identity, or module identity after the fact.

A cache entry can miss before artifact publication even when parsing/checking
succeeds. Examples include a missing imported artifact key, a platform relation
context mismatch, stale compiler artifact hash, changed checking context,
changed direct import artifact key, or cache value that lacks compile-time value
stores. The result is a cache miss and normal publication, not a partial cache
hit with sidecar repair.

### Import Resolution

Import resolution is part of the checked-source boundary. Before `Check.init`,
every `CIR.Import.Idx` must point at the resolved module slot used for external
declarations, qualified lookups, exposed types, and imported values.

Package-qualified imports have exactly one import identity: the full qualified
module name after package shorthand resolution. For example:

```roc
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types| ...
```

The import identity is `pf.Types`, not plain `Types`. Canonicalization may use
`Types` as a local alias for member lookup, but it must not publish a second
available import named `Types` for the app module.

The available-import record consumed by canonicalization carries both the import
identity and the member lookup prefix:

```zig
const AvailableImportView = struct {
    import_identity: ModuleIdentity,
    module: *const ModuleEnv,
    type_module_member_prefix: ?Ident.Idx,
    type_module_decl_node: ?u16,
};
```

For package-qualified type modules, the imported module and the local type
binding are separate concepts. A source import such as:

```roc
import pf.Tree exposing [Tree]
```

has one import identity, `pf.Tree`. The exposed main type `Tree` is published as
a scoped type binding that points back to that import identity. Lowering and
checking must not create a second module identity named `Tree`.

`type_module_decl_node` is the source-region authority for type-module import
diagnostics. If canonicalization cannot publish it for a type-module import,
that is a source diagnostic before checking begins; no later stage may scan
source text to recover the region. Nested associated type lookup through an
imported type module uses the same imported module identity plus a normalized
member suffix. A lookup such as `.Tree` is normalized as a suffix token of the
imported type-module view, not as a fresh unqualified import.

Builtin auto-imports obey the same rule. If canonicalization resolves a name
through an implicit builtin import, the checked artifact publishes a lowerable
imported view for the builtin artifact. Post-check lowering must never assume
the builtin module is globally available without an imported view.

Every exposed type or value from a package-qualified import retains the full
`CIR.Import.Idx` / module identity that resolved it. Checked metadata must not
collapse to basename-only identities such as plain `Types`. At the end of
checking, the import table is verified: every checked import reference names a
resolved module slot, and no package-qualified import has been republished under
only its local alias or basename.

Unresolved imports at checking initialization are build-graph or
canonicalization bugs.

## Static Dispatch

Surface syntax does not determine static dispatch. This source:

```roc
Fmt.decode_str(format, source)
```

may be a module call, or it may be static dispatch whose dispatcher type is
selected by checking. Parser and canonicalization do not decide this from the
spelling. Name resolution and type checking categorize it.

Every checked static-dispatch expression publishes one normalized plan before
mono MIR lowering:

```zig
const StaticDispatchCallPlan = struct {
    expr: CheckedExprId,
    method: MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: Span(CheckedExprId),
    result_mode: StaticDispatchResultMode,
};

const StaticDispatchResultMode = union(enum) {
    value,
    equality: EqualityDispatchMode,
};

const EqualityDispatchMode = struct {
    negated: bool,
    structural_allowed: bool,
};
```

`dispatcher_ty` is the checked type root whose instantiated, fully resolved
monomorphic type determines method lookup. It may come from the receiver, any
argument, the return value, an annotation, or a type root mentioned only by a
`where` constraint. Mono consumes this explicit root. It does not derive the
dispatcher from argument order, result position, receiver syntax, qualified-name
prefix, method name, or module lookup.

`callable_ty` is the fixed-arity function type of the operation. `args` are the
actual value arguments in final call order:

```roc
x.foo(a, b)
```

normalizes to:

```text
args = [x, a, b]
```

If `Fmt` is a type-variable alias rather than a module:

```roc
Fmt.decode_str(format, source)
```

normalizes to:

```text
args = [format, source]
```

Equality normalizes through the same mechanism:

```roc
x == y
x != y
```

becomes an `is_eq` dispatch plan with `args = [x, y]`. `!=` is represented by
`negated = true`; mono emits equality and then `bool_not`.

Mono lowers each `StaticDispatchCallPlan` while lowering one concrete
specialization:

1. Instantiate `dispatcher_ty` and `callable_ty` into the specialization-local
   source type graph.
2. Connect callable return and argument slots to the expression result and
   normalized argument types before lookup.
3. Fully resolve the dispatcher type.
4. Resolve `MethodOwner` from the fully resolved source type, preserving nominal
   identity.
5. Look up `(MethodOwner, method)` in the checked method registry.
6. Instantiate and unify the target procedure callable type with the
   dispatch-site callable type.
7. Register the unified fixed-arity function type in the concrete source type
   store.
8. Lower arguments exactly once using the final unified argument types.
9. Request the target mono specialization and emit `call_proc`.
10. For allowed ownerless equality, emit `structural_eq`.
11. For negated equality, emit `bool_not`.

If lookup is missing for value dispatch, or missing for equality without
`structural_allowed`, this is a compiler bug. Checking must reject ambiguous or
invalid dispatch before artifact publication.

The dispatcher type must be functionally determined by checked semantic data.
If the checked callable type, receiver, arguments, expected return, annotation,
or `where`-only type root does not determine one dispatcher, artifact
publication rejects the expression. Mono does not break ties by source spelling,
module prefix, argument order, literal type, method name, or target availability.

Delayed numeric variables bind during final argument/result lowering for the
dispatch site. A numeric receiver such as `12.34.foo()` or an argument such as
`x.plus(1)` carries a published method identifier subregion and checked numeric
constraint. Mono resolves the delayed numeric only after connecting the
dispatch-site callable graph to the actual argument and result endpoints. It
does not infer the receiver owner from the literal spelling alone.

Chained dispatch is a sequence of independent dispatch plans. In:

```roc
x.foo().bar()
```

`foo` and `bar` each have their own checked `StaticDispatchCallPlan`,
dispatcher root, callable root, method identifier region, and method-registry
target. The `bar` dispatcher consumes the checked result type of `foo`; it does
not inherit `foo`'s dispatcher or target. A dotted expression with no call
arguments is field access, not method invocation, unless checking published a
static-dispatch call plan for that expression.

Method diagnostics use the method identifier subregion published by
canonicalization/checking, not the whole call region and not source recovery.
For `35.foo()` and `12.34.foo()`, the diagnostic target for the missing method
is the `foo` token. That region is carried in checked metadata through artifact
publication.

No dispatch nodes survive mono MIR output.

## MIR Stage Ownership

Each MIR stage that accepts its input program by value owns that input
at function entry:

```text
mono MIR -> row-finalized mono MIR
row-finalized mono MIR -> lifted MIR
lifted MIR -> lambda-solved MIR
lambda-solved MIR -> executable MIR
```

The caller must not keep cleanup ownership after passing a program to an owning
stage. The callee deinitializes input-only stores, moves reused stores by
assigning empty replacements into the input before cleanup, and returns an
output with one clear owner.

There must never be two cleanup owners for the same MIR program, type store,
canonical-name store, row-shape store, literal pool, symbol store, AST store, or
source-type payload store.

## Mono MIR

Mono MIR is the first monomorphic post-check stage. It consumes checked
artifacts, explicit root requests, resolved value references, the checked method
registry, normalized static dispatch plans, compile-time root requests, hosted
procedure data, platform-required bindings, and concrete source type payloads.

Mono owns:

- clone-instantiating checked templates for concrete requests
- resolving static dispatch to concrete `call_proc`, `structural_eq`, and
  `bool_not`
- preserving fixed-arity function types on calls and procedure values
- preserving function values distinctly from direct procedure calls
- assigning monomorphic source types to expressions
- interning program literals into the program literal pool
- reserving mono specializations from explicit roots, `call_proc`, `proc_value`,
  static dispatch targets, equality, compile-time roots, materialization
  dependencies, and platform-required roots

Mono does not:

- lower all top-level functions eagerly
- discover roots by export/name/procedure-order scans
- preserve dispatch nodes after lowering
- package erased callables
- synthesize currying or partial application
- represent top-level/imported constants as runtime thunks

Top-level and imported constants lower as `const_ref`. Top-level, imported,
hosted, platform-required, and promoted procedure calls lower as `call_proc`.
Procedure values lower as empty-capture `proc_value`.

Every `call_proc` and `call_value` stores the exact requested mono source
function type. `args.len` must match that function type's fixed arity exactly.
Checked call sites publish that function type explicitly:

```zig
const CheckedCallSite = struct {
    func: CheckedExprId,
    args: Span(CheckedExprId),
    called_via: CalledVia,
    source_fn_ty_payload: ArtifactCheckedTypeRef,
};
```

`source_fn_ty_payload` is the authority for the fixed-arity source function
type at that call occurrence. `ResolvedValueRef` identifies the callee binding;
it does not choose the function type for the call. Mono specializes direct
procedure calls and types `call_value` from the call-site payload, not from the
callee expression's generic lookup type. For example, a call to
`List.concat([1, 2], [3, 4])` reserves the builtin specialization at
`List(I64), List(I64) -> List(I64)`, never at a stale `List(a), List(a) ->
List(a)` template root. Verifiers assert the call result type is the requested
function return type, each argument expression has the matching requested
argument type, `call_proc.proc` unifies with the requested function type,
`call_value.func` unifies with it, and fixed arity is exact.

Pre-executable MIR keeps direct calls, procedure values, and function-value
calls distinct:

```zig
const ProcValue = struct {
    proc: ProcedureValueRef,
    captures: Span(CaptureArg),
    fn_ty: TypeId,
};

const CallProc = struct {
    proc: ProcedureValueRef,
    args: Span<ExprId>,
    requested_fn_ty: TypeId,
};

const CallValue = struct {
    func: ExprId,
    args: Span<ExprId>,
    requested_fn_ty: TypeId,
};
```

`proc_value` exists when a resolved procedure is used as a value. Top-level and
imported procedure values have empty captures. Lifted local functions and
closures have one `CaptureArg` per target `CaptureSlot`, in slot order.
`call_proc` is a direct source/MIR procedure call; it never carries captures.
`call_value` remains a value-level function call until callable solving decides
whether it becomes a direct call, finite `callable_match`, erased call, or
bridge. The mono specialization queue is closed over both `call_proc`
dependencies and `proc_value` dependencies. A mono-exported `call_proc.proc`
or top-level `proc_value.proc` must name a mono-specialized procedure definition
or a reserved mono-specialization work item that will be drained before the
next stage consumes the program.

Mono uses `MonoSpecializationQueue` as the specialization driver. If a procedure
is needed by a root, direct call, procedure value, equality operation,
compile-time root, or materialization dependency, it is requested explicitly.
No post-check stage may scan source declarations to discover a missing target.

Procedure entry parameters and source parameter patterns are distinct MIR
concepts. The procedure entry parameter list contains raw fixed-arity
parameters whose types come from the instantiated requested function type. If a
source parameter pattern is a simple immutable binder, the raw entry parameter
may use that binder identity. If the pattern is `_`, the raw entry parameter is
synthetic. If the pattern destructures a nominal, tag, record, tuple, list, or
`as` pattern, mono creates a synthetic raw entry parameter, lowers the original
parameter pattern with the same concrete parameter type, and wraps the body in
an explicit source `match` over that synthetic parameter. Pattern binders are
introduced by that explicit match, not recovered from the body.

Mutable source parameters are initialized from immutable ABI parameters. When a
parameter binder is `reassignable`, mono allocates a synthetic ABI-parameter
symbol, then inserts an entry `var_decl` that initializes the source binder's
mutable local from the synthetic parameter before the body runs. All reads and
reassignments use the mutable local. IR must materialize that entry local and
must not make the mutable source binder and ABI parameter name the same IR
variable.

Mono keeps a specialization-local concrete binder environment. Whenever mono
creates a binder from a procedure parameter, local declaration, mutable
declaration, pattern binder, source-match binder, `for` binder, generated
destructuring binder, or mutable version, it records the concrete source type
and mono type for that binder in the current specialization. Later
`lookup_local` nodes lower from this environment. A checked expression type
root is source evidence; once a concrete mono binder has been published, the
binder environment is the type authority for local lookups.

Local function and closure binders are the explicit exception to one-binder,
one-mono-type lookup. A local procedure binder is a specialization template
inside the owning mono procedure. Each lookup or `proc_value` use consumes the
concrete use-site function type already being lowered and instantiates a
separate local procedure instance. Mono must not write that concrete use-site
type back into the ordinary binder environment. In:

```roc
{
    identity = |x| x
    id_num = Box.unbox(Box.box(identity))
    id_str = Box.unbox(Box.box(identity))
    { n: id_num(41), s: id_str("ok") }
}
```

the two `identity` uses instantiate separate local procedure instances at
`I64 -> I64` and `Str -> Str`.

### Debug And Inspect Lowering

Checked `dbg` and source `Str.inspect` lower to ordinary mono MIR. Checked
artifact publication provides a `Builtin.Str.inspect : T -> Str` intrinsic
wrapper template that can be specialized like any other procedure template. If
source `Str.inspect(value)` or a lowered `dbg value` has no published inspect
wrapper for the requested source type, that is a compiler bug.

A checked `dbg expr` lowers by:

1. evaluating `expr` exactly once into a local value
2. calling the specialized `Builtin.Str.inspect` helper for that value's exact
   source type
3. emitting an explicit debug statement with the resulting `Str`
4. returning `{}` when `dbg` appears in expression position

After mono, there are no `dbg` nodes, inspect pseudo-nodes, formatter side
tables, or backend-level inspect decisions. Later stages see only ordinary
calls, locals, `Str` values, and debug statements.

`to_inspect` is a lowering-visible special method of nominal source types. The
specialized inspect helper decides custom/default behavior from checked
artifact data:

- for a nominal with a checked `to_inspect` registry target, it emits a normal
  `call_proc` to that target
- for a transparent nominal without `to_inspect`, it structurally inspects the
  published backing type
- for an opaque nominal without `to_inspect`, it returns the string
  `"<opaque>"`
- for a function value, it returns the string `"<fn>"`

The helper never asks a backend to inspect a runtime value, never scans source
syntax for a method, and never repairs a missing method by compatible shape.
The `to_inspect` target's checked callable type must match the requested
`T -> Str` helper boundary exactly after instantiation.

The default inspect helper must preserve nominal opacity while choosing the
rendered shape. It uses the mono type query that preserves the outer nominal
node before exposing any backing. If the value is an opaque nominal and no
checked `to_inspect` target exists, the helper returns `"<opaque>"` without
inspecting the backing. If the value is a transparent nominal with no
`to_inspect`, the helper structurally inspects through the published backing,
but it does not insert `nominal_reinterpret` merely to unwrap the value. The
inspected value remains the original nominal-typed value; only the selected
inspect shape changes. Recursive transparent nominals reserve their specialized
inspect helper before lowering payload formatting, so recursive payloads call
the nominal helper rather than a backing-only helper with incompatible identity.

Inspect helper lowering is graph-shaped. A helper for a recursive or nested
type reserves helper procedure identities before lowering bodies, and nested
payload formatting calls the specialized helper for the payload type instead of
inlining recursively. `Str.inspect(Box(T))` follows the same rule: it lowers one
Box layer, then calls the specialized `Builtin.Str.inspect : T -> Str` helper
for the unboxed payload. It is not runtime reflection and not layout-driven
printing.

## Row-Finalized Mono MIR

Row-finalized mono MIR is a mandatory type-state between mono MIR and lifted
MIR. It consumes dispatch-free mono MIR plus the specialization-local mono type
store with links resolved for the current specialization.

It owns:

- interning canonical logical record shapes
- interning canonical logical tag-union shapes
- converting record field names to `RecordFieldId`
- converting tag names to `TagId`
- converting tag payload positions to `TagPayloadId`
- rewriting construction, access, update, destructuring, patterns, and payload
  projections to finalized ids
- preserving source evaluation order separately from logical assembly order

After row finalization, later stages do not contain:

- record construction keyed only by source field name
- record access keyed only by source field name
- record destructuring keyed only by source field name
- tag construction keyed only by source tag name
- tag pattern matching keyed only by source tag name
- tag payload projection keyed only by local payload position
- helper APIs that compute row indexes by sorting names, scanning rows, scanning
  expressions, or inspecting physical layout order

The shape of row-finalized construction separates evaluation from assembly:

```zig
const RowShapeStore = struct {
    records: InternStore(RecordShapeKey, RecordShapeId),
    tag_unions: InternStore(TagUnionShapeKey, TagUnionShapeId),
};

const RecordShapeKey = struct {
    fields: Span(RecordShapeField),
    open_tail: ?CanonicalTypeKey,
};

const RecordShapeField = struct {
    label: CanonicalFieldLabelId,
    field: RecordFieldId,
    mono_ty: MonoTypeId,
};

const TagUnionShapeKey = struct {
    tags: Span(TagShape),
    open_tail: ?CanonicalTypeKey,
};

const TagShape = struct {
    label: CanonicalTagLabelId,
    tag: TagId,
    payloads: Span(TagPayloadShape),
};

const TagPayloadShape = struct {
    payload: TagPayloadId,
    mono_ty: MonoTypeId,
};

const RecordFieldEval = struct {
    field: RecordFieldId,
    expr: ExprId,
};

const RecordFieldAssembly = struct {
    field: RecordFieldId,
    eval_index: u32,
};

const TagPayloadEval = struct {
    payload: TagPayloadId,
    expr: ExprId,
};

const TagPayloadAssembly = struct {
    payload: TagPayloadId,
    eval_index: u32,
};

const RecordInit = struct {
    shape: RecordShapeId,
    eval_order: Span(RecordFieldEval),
    assembly_order: Span(RecordFieldAssembly),
};

const TagInit = struct {
    shape: TagUnionShapeId,
    tag: TagId,
    eval_order: Span(TagPayloadEval),
    assembly_order: Span(TagPayloadAssembly),
};
```

Expressions are lowered once from `eval_order`. `assembly_order` refers to
already-lowered evaluation slots. A closure, allocation, call, box operation, or
RC operand inside a record field or tag payload must not be evaluated twice.

Record shapes intern by canonical field identity and finalized logical field
order. Tag-union shapes intern by canonical tag identity, finalized logical tag
order, canonical payload identity, and finalized logical payload order. Open
row/tag tails are preserved only while the owning mono source type remains
open; final row operations must either name an explicit closed shape or seal the
open tail at the extension edge that justified it. Duplicate labels, missing
labels, unresolved row tails, unresolved tag tails, or payload-position
mismatches are compiler bugs.

Row finalization rewrites all row-sensitive operations: record construction,
field access, field update, record destructuring, tag construction, tag
patterns, tag discriminant tests, and tag payload projections. The rewritten
operation carries a finalized shape id plus the finalized field/tag/payload id.
The specialization-local mono type store keeps the mono type for every
finalized field and payload, so lifting and later representation solving do not
recover member types from source names or layout order.

Finalized row ids are valid only with their owning shape. If a boundary re-homes
a source type graph to a different concrete record or tag-union endpoint, it
must re-key every row operation attached to that value or pattern to the new
owning shape before publishing the next type-state. Re-keying matches by
canonical tag/field label and payload logical index between two explicit shapes;
debug builds assert exact agreement, release builds use `unreachable`.

## Lifted MIR

Lifted MIR consumes row-finalized mono MIR and owns lambda lifting plus capture
discovery.

It produces dispatch-free lifted MIR where:

- local functions and closures are lifted to procedure definitions
- captures are explicit `CaptureSlot` procedure metadata
- captured reads are explicit `capture_ref(slot)` expressions
- procedure values carry explicit `CaptureArg` payloads in capture-slot order
- local-function rename environments are stage-private
- every expression has a mandatory type
- `call_proc` and `proc_value` targets refer to MIR procedure identities, not
  executable procedures
- direct-call edge metadata is published per lifted procedure

After lifted MIR, a procedure symbol may appear in expressions only as:

```text
call_proc.proc
proc_value.proc
```

It must not appear as ordinary `var_` data. Local function values and calls go
through `proc_value` and `call_value`, even when captureless. Direct-call
optimization is not a representation invariant.

Recursive local-function groups use an explicit capture graph:

```zig
const LiftedCaptureGraph = struct {
    members: Span(LiftedGroupMember),
    value_edges: Span(CaptureValueEdge),
    proc_value_edges: Span(CaptureProcValueEdge),
};
```

`CaptureValueEdge` records an external local value needed by a member.
`CaptureProcValueEdge` records a self or sibling procedure value constructed or
returned by a member. Capture discovery descends through every expression shape
that can carry a procedure value: records, tuples, tags, lists, boxes,
transparent wrappers, `match` branches, `if` branches, aliases, returns,
captures, mutable joins, and loop joins.

Recursive group lifting:

1. Reserves procedure handles for every member before lowering any body.
2. Assigns stable procedure order keys.
3. Builds the capture graph.
4. Solves capture slots to a least fixed point over ordinary value edges,
   procedure value edges, and capture slots needed by referenced members.
5. Assigns deterministic capture slot indexes.
6. Lowers member bodies.
7. Rewrites captured reads to `capture_ref(slot)`.
8. Rewrites self and sibling references to explicit `proc_value`.

Capture discovery is identity-based, not display-name-based. Top-level
constants, imported constants, top-level procedures, imported procedures,
hosted procedures, platform-required procedures, and promoted procedures are not
capturable local values; they are already explicit mono operations.

Lifted MIR lowers through an explicit lexical scope builder:

```zig
const LiftScopeFrame = struct {
    bindings: Map(SymbolId, BindingLocation),
    mutable_versions: Map(SymbolId, MutableVersionId),
};

const BindingLocation = union(enum) {
    local_value: ExprId,
    lambda_param: ParamId,
    pattern_binder: PatternBinderId,
    local_proc: MirProcedureRef,
    captured_slot: CaptureSlotIndex,
};
```

The scope is keyed by resolved symbols and mutable-version ids, never by
display text. Lambda params, local function names, recursive local-function
members, source-match binders, record and tuple destructuring binders, loop
binders, block-local declarations, and shadowed declarations enter the scope as
their own resolved identities. A local binding that shadows a top-level or
imported name is capturable by that local identity. Top-level constants,
imported constants, top-level/imported/hosted/platform-required/promoted
procedures, and platform-required consts never enter the capturable scope; they
are already explicit mono operations such as `const_ref`, `call_proc`, or
empty-capture `proc_value`.

Mutable source variables are represented as explicit versions before capture
analysis needs them. A captured mutable value captures the current version or
phi input, not a source mutable cell. Capture discovery and slot assignment are
symbol/version based and must not compare display names, generated aliases, or
environment lookup results.

Lifted MIR publishes direct-call metadata. While lowering each procedure body,
every copied or created `call_proc` appends its target to that procedure's
deduplicated `direct_calls` span. A call inside a lifted local function is
recorded on that lifted procedure, not on the enclosing procedure. Lambda-solved
MIR consumes these spans to compute direct-call SCCs. It must not walk lifted
bodies in release code to rediscover direct-call edges. A debug-only verifier
may walk bodies to assert the metadata.

## Lambda-Solved MIR

Lambda-solved MIR consumes lifted MIR and owns callable representation solving.

It owns:

- lambda-set inference
- exact finite callable-set representation
- capture type association with callable members
- erasure propagation
- direct-call SCC representation identity
- recursive solve sessions
- final callable metadata needed by executable MIR

It does not:

- resolve static dispatch
- preserve method names as executable operations
- reconstruct callable targets from source function types
- carry method registries
- emit source/executable duplicate records
- decide executable direct-call signatures from syntax

Every imported function type occurrence receives a fresh callable variable:

```zig
const CallableVarId = enum(u32) { _ };

const LambdaSolvedFnType = struct {
    fixed_arity: u32,
    args: Span(TypeId),
    ret: TypeId,
    callable: CallableVarId,
};
```

Freshness is per occurrence. Two equal source function types do not share
callable representation unless explicit value flow connects them.

Lambda-solved MIR builds representation roots for expression results, binders,
pattern binders, mutable versions, joins, loop phis, parameters, returns,
captures, `call_proc`, `call_value`, `proc_value`, callable values, projection
results, and `Box(T)` boundaries.

`call_proc` is a direct procedure call for solving and SCCs. It is not a
value-level procedure value. `proc_value` is a value-level procedure value with
explicit capture operands.

For `call_proc`, lambda-solved MIR:

- adds a direct-call dependency edge
- instantiates the target callable type
- unifies target type with requested callable type
- unifies arguments and result
- preserves target identity for executable MIR

For `proc_value`, lambda-solved MIR:

- reserves the selected procedure member
- unifies `proc_value.fn_ty` with the target procedure type
- unifies capture operands with target capture slots
- preserves target identity and capture slot order

Finite callable sets are canonical maps of exact callable member instances:

```text
(ProcedureCallableRef, ProcRepresentationInstanceId) -> capture slots
```

Unification unions different members, unifies matching member capture slots
pointwise, and converts to erased representation only through explicit erasure
requirements.

`Box(T)` is the only source of erased callable representation. Non-boxed
records, tuples, tags, lists, functions, and nominals do not introduce erased
callable representation. If executable lowering receives an erased-boundary
request whose root is not `Box(T)`, that is a compiler bug.

Lambda-solved MIR builds solve sessions by reservation. A
`RepresentationSolveSession` owns the mutable member table during construction.
Members may be added by direct calls, recursive direct-call SCCs, procedure
values, finite callable-set calls, finite erased adapters, and erased boundary
requirements. The sealed session member slice is published only after a fixed
point.

Recursive direct-call SCCs use shared identity inside one executable use
context. Non-recursive direct calls are call-site-owned. For example:

```roc
is_even = |n|
    if n == 0 then True else is_odd(n - 1)

is_odd = |n|
    if n == 0 then False else is_even(n - 1)
```

One root use of `is_even` owns one representation member for `is_even` and one
for `is_odd`. Recursive calls reuse those members rather than allocating an
infinite chain.

The reservation key for a target inside the currently solved recursive
direct-call SCC is `(recursive_group_anchor, target_proc)`. The first external
entry into the SCC creates the anchor; every `call_proc` edge whose target is
inside the same SCC reuses the anchored member for that target. A `call_proc`
whose target is outside the SCC remains owned by its ordinary call-site owner.
Synthetic direct-call targets, including promoted wrappers and erased adapter
helpers, can be external direct-call edges when lifted MIR published them in a
procedure's `direct_calls` span. Lambda-solved consumes only the lifted
`direct_calls` metadata to compute SCCs in release builds. A debug verifier may
walk lifted bodies to assert that the metadata matches actual `call_proc`
nodes, including synthetic targets and calls inside lifted local procedures.

For non-recursive higher-order calls, call sites remain distinct:

```roc
pipe = |x, f| f(x)

a = pipe(5, |x| x + 1)
b = pipe(5, |x| x + 2)
```

The two `pipe` calls may have the same source function type but different
callable parameter representation. They must not be merged merely by source
procedure identity.

Lambda-solved body lowering must be stack-safe for deep expression spines. Long
chains of low-level operations use an explicit worklist and postorder
publication of lowered child expressions. The worklist is only a stack-safety
implementation strategy; it must not change evaluation order, regroup
operations, flatten semantic value flow, or infer missing data.

## Executable MIR

Executable MIR consumes lambda-solved MIR and is the final post-check executable
representation consumed by IR.

It owns:

- direct calls
- erased calls
- finite callable-set value construction
- mandatory finite callable-set call lowering to `callable_match`
- packed erased function values
- capture record construction
- explicit bridge insertion
- executable type publication
- logical layout graph construction
- runtime uniqueness mutation sites
- entrypoint wrapper shape

It does not own static dispatch, method lookup, source type reconstruction,
source/executable side tables, erasure decisions, erased-shape compatibility
decisions, or lambda-set inference.

Executable MIR lowers lambda-solved call operations to:

- `call_direct` for exact executable procedure calls
- `call_erased` for explicitly erased callable calls
- explicit bridges plus calls when representation transforms are required
- `callable_set_value` for non-erased callable values
- `packed_erased_fn` for explicitly erased callable values
- `callable_match` for all finite non-erased `call_value` calls

Captured non-erased callables are callable-set values with capture payloads.
Capture presence alone never causes erased packing.

Finite callable-set construction consumes the occurrence-local
`CallableSetConstructionPlan`:

```zig
const CallableSetValue = struct {
    id: CallableSetValueId,
    callable_set_key: CanonicalCallableSetKey,
    member: CallableSetMemberRef,
    capture_record: ?CallableCaptureRecord,
    result_ty: ExecTypeId,
    result_tmp: TempId,
};

const CallableCaptureRecord = struct {
    member: CallableSetMemberId,
    captures: Span<CaptureValueRef>,
};

const CaptureValueRef = struct {
    slot: CaptureSlotIndex,
    value: ExecutableValueRef,
    transform: ?ExecutableValueTransformRef,
    exec_ty: CanonicalExecValueTypeKey,
};
```

Before emitting a callable-set value for occurrence `v`, executable MIR
validates the lambda-solved construction consistency unit:

```text
value_info(v).callable.construction_plan == construction_id
construction.result == v
value_info(v).callable.emission_plan
    == finite_callable_set(construction.callable_set_key)
descriptor(construction.callable_set_key)
    contains construction.selected_member
descriptor member proc_value.source_fn_ty == construction.source_fn_ty
```

These checks are compiler invariants. The executable builder must not repair a
missing or mismatched construction plan by inspecting the expression, consulting
checked CIR, looking up the current lexical symbol, or comparing executable
shapes.

Capture operands are evaluated at callable construction time, in canonical
`CaptureSlot.index` order, and stored in one capture record when needed.
Every capture slot in the selected member has a `CaptureValueRef`, even when
the capture has zero byte width. `callable_match` later destructures the stored
payload. It does not rebuild captures, replay source expressions, sort captures
by name, or omit zero-sized captures from semantic metadata.

Construction resolves each `construction.capture_values[i]` to an
`ExecutableValueRef` exactly once, applies the published capture transform,
builds at most one `CallableCaptureRecord`, and stores that record as the
selected member payload. The resulting callable-set value handle is then used
for later storage, return, aggregate construction, bridge input,
`callable_match`, or erased packing.

The transformed capture operands must match the selected descriptor member. The
operand count equals the descriptor's capture-slot count, slot indexes are
dense and canonical, and each transformed operand executable type equals the
slot's executable type after canonical executable type lowering. Raw source
capture operands may have different executable representations. A mismatch
after the published transform is a compiler bug, not a cue to reorder captures,
inspect target bodies, compare shapes, or synthesize a different record.

All finite callable-set calls lower to `callable_match`, including singleton
sets. Only `call_proc` lowers directly to `call_direct`.

`callable_match` evaluates the callable value and original call arguments
exactly once, stores them in temporaries, branches by callable member tag,
destructures captures, applies explicit argument transforms, emits branch-local
`call_direct`, applies explicit result transforms, and assigns one shared result
temporary.

The node shape is explicit:

```zig
const CallableMatch = struct {
    func_tmp: TempId,
    arg_temps: Span(TempId),
    result_tmp: TempId,
    requested_source_fn_ty: ConcreteSourceTypeRef,
    branches: Span(CallableBranch),
    no_return: bool,
};

const CallableBranch = struct {
    member: CallableSetMemberId,
    direct_target: ExecutableSpecializationKey,
    capture_binders: Span<CaptureBinder>,
    direct_args: Span(ExecutableValueRef),
    branch_call_result: CallableBranchResult,
    result_transform: ResultTransformPlan,
};

const CallableBranchResult = union(enum) {
    direct_call_result: ExecutableValueRef,
    no_return,
};

const ResultTransformPlan = union(enum) {
    identity,
    bridge: ValueTransformPlan,
    no_return,
};
```

`func_tmp` and `arg_temps` are evaluated before branching. A branch's
`direct_args` refer to those temporaries and branch-local capture binders; they
are not arbitrary source expressions. Every branch has a result transform,
including identity. A `no_return` callable match has no shared result value but
still owns the explicit branch terminal. Every returning branch assigns exactly
one shared result temporary after applying its mandatory result transform.

The source function type must be exactly equal across:

- the descriptor member's `ProcedureCallableRef`
- the callable-match request
- the branch executable specialization
- the direct-call source type carried by the branch call

No alias-spelling, compatible-shape, or layout-based comparison is accepted at
this boundary.

Branch targets are selected from exact lambda-solved member instances. A branch
must not choose a target by source procedure name, display string, argument
count, layout compatibility, or descriptor order alone.

Executable MIR consumes `ValueInfoId`, `BindingInfoId`, `ProjectionInfoId`, and
`CallSiteInfoId` values from lambda-solved MIR. A runtime lexical environment
may map source symbols to already-published value handles, but it must not
contain semantic side fields such as `proc`, `callable_target`,
`boxed_payload`, `record_fields`, or `tag_payloads`.

Executable bridges operate on evaluated value handles. They do not compare
compatible shapes to decide whether to bridge. Bridge obligations are explicit
value-transform records selected by lambda-solved/executable boundary data.

Bridge inputs are already-evaluated values:

```zig
const BridgeInput = struct {
    value: ExecutableValueRef,
    value_ty: CanonicalExecValueTypeKey,
    endpoint: ValueTransformEndpointRef,
};

const AggregateAssemblyEdge = struct {
    child_expr: ExecutableExprId,
    evaluated_child: ExecutableValueRef,
    child_ty: CanonicalExecValueTypeKey,
    parent_slot: ValueTransformEndpointRef,
    assembly_bridge: ExecutableValueTransformRef,
};

const RecordFieldAssembly = struct {
    field: FinalizedRecordFieldId,
    edge: AggregateAssemblyEdge,
};

const TupleItemAssembly = struct {
    index: u32,
    edge: AggregateAssemblyEdge,
};

const TagPayloadAssembly = struct {
    tag: FinalizedTagId,
    payload_index: u32,
    edge: AggregateAssemblyEdge,
};

const ListItemAssembly = struct {
    index: u32,
    edge: AggregateAssemblyEdge,
};
```

Record, tuple, tag-payload, and list construction are explicit assembly plans.
Each edge names the source child expression id, the evaluated child value, the
child executable type, the parent slot endpoint, and the bridge required to
write that child into the parent's physical slot. Construction assembly
bridges are separate from pattern-binder bridges, but both obey the same
physical-layout endpoint rules.

For recursive shapes, identity at the source type boundary does not imply a
physical `.direct` bridge. For example:

```roc
Logic : [And(Logic, Logic), Or(Logic, Logic), Lit(Bool)]

make_and = |left, right| And(left, right)

eval = |logic|
    match logic {
        And(left, right) => eval(left) && eval(right)
        Or(left, right) => eval(left) || eval(right)
        Lit(value) => value
    }
```

`make_and` constructs the payload record in the parent union variant's exact
physical slot layout. It must not first build a source-shaped temporary record
and then rely on a whole-record bridge repair. The `eval` pattern binders for
`left` and `right` are projected from the same committed physical slots and
then bridged to their logical consumer endpoints. Lists may need
`list_reinterpret`, and recursive structural slots may need raw-to-logical
bridges, even when the source transform is identity.

Mutation sites are explicit runtime uniqueness operations:

```zig
const RuntimeUniquenessMutation = struct {
    id: MutationSiteId,
    value: ExecutableValueRef,
    value_ty: ExecTypeId,
    unique_path: MutationUniquePathId,
    shared_path: MutationSharedPathId,
    result_tmp: TempId,
};
```

The unique path runs only when the runtime refcount check proves uniqueness. The
shared path allocates or copies as required.

## Source `match`

Ordinary source `match` expressions have explicit executable MIR decision
plans. They are separate from callable-set `callable_match`.

Match lowering preserves:

- scrutinee evaluation order
- branch order
- guard order
- binder scope
- branch-local binder identity
- degenerate or unreachable branch metadata
- pattern-path payload projections
- source match result joins

Branch guards and bodies see representative binders chosen by the match
decision plan. Candidate binders from alternative patterns are rewritten to the
representative binder before guard or body lowering observes them. This keeps:

```roc
match value {
    Ok(x) | Err(x) if x > 0 => x
    _ => 0
}
```

from giving guards and bodies two incompatible binder identities for the same
source name. The decision plan owns that mapping; later stages do not recover it
from source names.

## IR, LIR, ARC, And Backends

IR consumes executable MIR only. It does not import checked CIR, method
registries, or MIR stage builder internals.

LIR consumes IR. IR-to-LIR lowering interns used `ProgramLiteralId` values into
`LirStore.strings` and rewrites LIR literal/crash payloads to final
`base.StringLiteral.Idx` values. MIR and IR use `ProgramLiteralId`; final LIR
uses `LirStore`.

LIR bodies are statement-only and use committed layouts. Literal carriers may be
wider than runtime storage, but materialization writes exactly the committed
layout size. For example, an `i128_literal` carrier for `1.U32` writes exactly
four bytes into a `U32` local.

ARC insertion consumes LIR before backends. Its input is RC-free except for
shared continuations already rewritten during the same insertion pass. A
finished LIR program contains explicit `incref`, `decref`, and `free`.

ARC insertion is mechanical token accounting over explicit LIR values and
control flow. It uses:

- local ownership state
- local use reachability
- branch continuations
- join bodies and jump edges
- loop keep-sets
- call argument transfer metadata
- low-level RC-effect metadata
- refcounted layout metadata

It does not infer ownership from source syntax, backend behavior, builtin names,
or runtime helper implementation.

LIR writes expose write meaning explicitly:

```zig
const SetLocalWriteMode = union(enum) {
    initialize_join_result,
    replace_existing,
    initialize_join_param,
};

const ForListElementSource = enum {
    aliases_iterable_element,
};

const SwitchContinuation = struct {
    continuation: LirStmtId,
};
```

The exact field names may differ. The distinction must not: branch result
initialization, mutable overwrite, join-parameter transfer, and loop-element
binding are separate operations for ARC.

Direct calls and low-level calls transfer owned tokens to callees/helpers unless
the caller needs the same local later. If the caller needs it later, ARC emits
an `incref` before the call. If the local is not owned by the caller, ARC emits
an `incref` to create an owned callee token and leaves caller state unchanged.

Low-level operations publish exact RC effects. Examples:

- `List.get_unsafe`, `List.first`, `List.last`, and `Box.unbox` retain results
  copied out of borrowed storage.
- `Box.box` retains the payload argument copied into the box.
- `List.append_unsafe` consumes the list argument, retains the appended element,
  and marks the result as aliasing the consumed list token.
- Copy-on-write list/string helpers consume mutable candidates and publish
  which consumed arguments can become the result token.

The LIR interpreter uses committed storage for every value:

- callee parameters are materialized into locals using the callee's committed
  parameter layout before the body starts
- callee parameter locals are distinct storage from caller-frame values; a
  normal call never aliases caller frame slots as callee params
- caller cleanup remains the caller's responsibility, and callee parameter
  cleanup remains the callee's responsibility
- calls are mutation barriers: mutable locals that may be observed across the
  call have their ownership and storage state committed before argument
  transfer begins
- consumed low-level args are explicit ABI cases described by
  `LowLevelRcEffect`; ordinary procedure calls use procedure-boundary token
  transfer instead of low-level consumption rules
- locals, join parameters, branch results, call results, and temporary values
  allocate storage with the committed size and alignment
- literal carriers can be wider than storage, but writes truncate or encode to
  the committed layout width
- join-parameter materialization copies or moves according to explicit LIR
  transfer statements and ARC state, not according to source syntax
- boxed erased callable payloads, lists, strings, and static data pointers use
  the same runtime representation as compiled backends

Backend call ABI lowering treats argument placement as simultaneous. If a call
uses a source value that lives in a register or stack slot that will be
clobbered by another outgoing argument, return pointer, hidden capture pointer,
or callee parameter slot, backend lowering first materializes that source into a
stable temporary. The backend may choose registers and stack slots, but it must
preserve the LIR call's simultaneous argument semantics. It must not rely on
left-to-right moves that accidentally overwrite a later source.

Backends and the interpreter consume LIR only. They do not perform RC policy
analysis.

## Compile-Time Constants And Top-Level Values

Checking finalization evaluates compile-time constants through the MIR-family
path and LIR interpreter before publishing the checked artifact.

It produces:

- `CompileTimeValueStore`
- one binding entry for each evaluated top-level constant pattern
- private promoted-capture data for promoted callable captures
- promoted closed procedure values for top-level callable roots
- serialized target-independent constant data for cached/imported artifacts

It does not produce:

- runtime top-level constant thunks
- runtime global initializer procedures
- zero-argument constant wrappers
- runtime top-level closure objects
- runtime global callable-value objects
- startup code that initializes module constants

Compile-time root selection is explicit:

```zig
const ComptimeRoot = struct {
    module: ModuleId,
    pattern: PatternId,
    expr: ExprId,
    lir_root: ExecutableProcId,
    result: ComptimeRootResult,
    kind: ComptimeRootKind,
};

const ComptimeRootResult = union(enum) {
    constant_graph: ConstGraphReificationPlanId,
    callable_result: CallableResultId,
    expect_result: ExpectRootId,
};
```

`ConstGraphReificationPlan` is a graph built from resolved checked source types
and sealed lambda-solved/executable representation data. It describes exactly
how interpreter results are copied into `CompileTimeValueStore`, including
nested callable leaves. Reification does not infer callable identity from source
syntax, runtime closure memory, generated procedure names, field names, tag
names, or allocation order.

Compile-time roots are ordered by an explicit dependency graph. Procedure body
dependency summaries are computed from sealed callable-aware lowering records,
not from source text or mono syntax alone. Summaries record availability uses
and concrete value uses separately.

Top-level values publish as:

```zig
const TopLevelProcedureBinding = struct {
    source_scheme: CanonicalTypeSchemeKey,
    body: ProcedureBindingBody,
};

const ProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateRef,
};

const TopLevelValue = union(enum) {
    const_template: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
    pending_callable_root: ComptimeRootId, // checking-finalization only
};
```

Every published function-typed top-level value is a
`procedure_binding: TopLevelProcedureBindingRef`. A direct function declaration
or top-level lambda uses `ProcedureBindingBody.direct_template`. A generalized
callable root that requires concrete compile-time callable evaluation uses
`ProcedureBindingBody.callable_eval_template`. `callable_eval_template` is not a
separate published top-level value kind. No published artifact contains
`pending_callable_root`.

Top-level callable finalization has three cases:

1. A top-level function declaration or top-level lambda is already a procedure
   declaration. It publishes a `procedure_binding` without interpreter
   execution.
2. A top-level binding with function source type whose expression is not already
   a procedure declaration is a compile-time callable root or callable eval
   template. Fully concrete roots evaluate through MIR/LIR/interpreter and
   promote the result. Generalized roots publish `CallableEvalTemplate` for
   concrete consumers.
3. A top-level binding with non-function source type is a compile-time constant.
   Its value graph may contain callable leaves nested inside records, tuples,
   tags, lists, boxes, aliases, or nominals.

Examples:

```roc
add1 : I64 -> I64
add1 = |x| x + 1
```

is a direct procedure binding.

```roc
make_adder : I64 -> (I64 -> I64)
make_adder = |n| |x| x + n

add5 : I64 -> I64
add5 = make_adder(5)
```

`add5` is evaluated during checking finalization if requested at a concrete
function type. The result is promoted into a procedure binding when it is needed
by exports or runtime roots. It is not a thunk and not a global closure object.

```roc
table = { f: |x| x + 1 }
```

is a valid non-function constant containing a callable leaf. It reifies as a
constant graph whose `f` field is an explicit callable leaf. No runtime thunk is
introduced.

Compile-time evaluation and final binary emission are decoupled. Lowering done
for compile-time evaluation can cache MIR/LIR work for the exact specialization.
That does not mark the specialization as a final binary root. Later runtime
root lowering checks the existing cache before doing the same work and only then
records the specialization for final emission.

## Readonly Provided Constants

Non-function `provides` entries are immutable data exports. A provided procedure
is a procedure entrypoint. A provided constant is a host-linkable symbol in the
target readonly data section.

Example:

```roc
module []

provides [table]

table = {
    names: [["Alice", "Bob"], ["Eve"]],
    count: 3,
}
```

The exported symbol `roc__table` is ordinary target-layout Roc data in the
object file's readonly section, plus relocations to nested readonly heap
allocations. It is not a runtime thunk, runtime initializer, top-level closure
object, or global callable object.

The path is:

```text
ProvidedDataExport.const_ref
  -> concrete ConstInstantiationKey
  -> sealed ConstInstance
  -> explicit ConstMaterializationPlan
  -> TargetStaticDataGraph
  -> object-file readonly symbols and relocations
```

`TargetStaticDataGraph` is target-specific and outside the checked artifact
cache. It owns readonly symbols:

```zig
const TargetStaticDataGraph = struct {
    nodes: []const TargetStaticDataNode,
};

const TargetStaticDataNode = struct {
    symbol_name: []const u8,
    bytes: []const u8,
    alignment: u32,
    visibility: enum { local, exported },
    relocations: []const TargetStaticDataRelocation,
};

const TargetStaticDataRelocation = struct {
    offset: u64,
    target_symbol_name: []const u8,
    addend: i64,
    kind: enum { absolute_pointer },
};
```

Nested heap-shaped values use normal Roc runtime layouts with a static refcount:

```zig
pub const REFCOUNT_STATIC_DATA: isize = 0;
```

Runtime `incref`, `decref`, `free`, and uniqueness checks treat static data as
whole-program-lifetime data:

- `incref` is a no-op.
- `decref` is a no-op.
- static data is never unique.
- static data is never freed.
- runtime helpers must not mutate static allocation metadata.

Static allocations use the same prefix shape as dynamic refcounted allocations.
The refcount word is at `data_ptr - word_size`. For lists with refcounted
elements, the allocation element count word is at `data_ptr - 2 * word_size`.

Static materialization writes the same runtime representation as ordinary
lowering:

- small `Str` inline
- non-small `Str` pointing at readonly byte allocation
- `List(T)` pointing at readonly element allocation
- `Box(T)` pointing at readonly payload allocation
- records and tuples in finalized target field order
- tag unions in ordinary tag-union layout
- Bool as ordinary tag-union data

Recursive values require explicit pointer boundaries such as `Box(T)` or
`List(T)`. Static materialization never creates cycles in inline bytes.

Host integration tests for readonly data exports must read linked symbols
directly:

```zig
extern const roc__answer: i64;
extern const roc__table: Table;
extern const roc__names: RocList;
extern const roc__tree: Tree;
```

These tests prove that primitives, nested records, strings, lists of lists,
boxes, recursive boxed tag unions, and boxed erased callable constants are
host-linkable immutable data.

## Boxed Erased Callables

`Box(function)` is one ordinary Roc refcounted allocation whose data pointer is
the Roc value. The allocation payload starts with a fixed erased-callable header;
hidden capture bytes live inline after that header at fixed 16-byte alignment.

The shared runtime ABI is in `src/builtins/erased_callable.zig`:

```zig
pub const ErasedCallableFn =
    *const fn (
        ops: *RocOps,
        ret: ?[*]u8,
        args: ?[*]const u8,
        capture: ?[*]u8,
    ) callconv(.c) void;

pub const OnDropFn =
    *const fn (capture: ?[*]u8, ops: *RocOps) callconv(.c) void;

pub const Payload = extern struct {
    callable_fn_ptr: ErasedCallableFn,
    on_drop: ?OnDropFn,
};

pub const capture_alignment: u32 = 16;
pub const payload_alignment: u32 = 16;
```

`args` points to a generated fixed-arity argument struct, or is null for arity
0. `ret` points to caller-owned result storage, or is null for zero-sized
results. `capture` is the fixed pointer after the payload header. Capture bytes
are inline; there is no runtime discriminant, no capture offset field, and no
flags field.

Roc-created and host-created boxed functions use the same ABI. The LIR
interpreter uses the same ABI and RC/drop path as compiled backends. There is no
interpreter-private payload encoding.

`on_drop` recursively releases refcounted data inside the hidden capture when a
dynamic boxed callable allocation reaches final drop. It does not free the
outer allocation; the erased-callable runtime frees that allocation after
`on_drop` returns. Static readonly boxed callable allocations use
`REFCOUNT_STATIC_DATA`, so their `on_drop` is not run by normal decref.

Glue generation maps `Box(function)` to the shared erased callable type, not to
a pointer-to-inner-function. Host helpers construct payloads with
`callable_fn_ptr`, optional `on_drop`, and inline 16-byte-aligned capture bytes.
Generated C glue must forward-declare `struct RocOps` before declaring
`RocErasedCallableFn` or `RocErasedCallableOnDrop`, because both typedefs take
`struct RocOps*`. Later hosted-function declarations may repeat the forward
declaration, but they must not be the first declaration. Generated C headers
expose the erased callable payload struct, erased callable pointer type, capture
alignment, payload alignment, capture-offset macro, and payload-size macro so
hosts can allocate the same ABI shape Roc uses internally.

Examples:

```roc
make_boxed_adder : I64 -> Box(I64 -> I64)
make_boxed_adder = |n| Box.box(|x| x + n)

add_one : I64 -> I64
add_one = Box.unbox(make_boxed_adder(1))

main = add_one(41)
```

```roc
make_boxed_adder : I64 -> (I64 -> I64)
make_boxed_adder = |n| {
    boxed_n = Box.box(n)

    |x| x + Box.unbox(boxed_n)
}

add_one : I64 -> I64
add_one = make_boxed_adder(1)

main = add_one(41)
```

Both cases avoid runtime thunks and top-level closure objects. Callable results
are promoted to procedure bindings when required by runtime roots or exports.

Readonly boxed erased callable constants consume sealed callable materialization
data:

- boundary source function type and erased call signature
- stable `Box(T)` provenance
- direct erased procedure code ref or finite adapter with sealed members
- executable wrapper symbol
- exact capture materialization graph
- explicit `on_drop` helper symbol or null

Static data emission does not reconstruct callable function pointers from
source syntax, procedure names, function types, descriptor member order, or
runtime interpreter values.

## Runtime Images And Shared Memory

Runtime-image publication uses shared memory for child-side execution. The
parent lowers checked artifacts through LIR and ARC insertion, then publishes a
runtime image that the child maps and interprets. The child must not receive CIR
or checked artifacts, and it must not deserialize semantic compiler structures.

The runtime image boundary is:

```text
parent:
  checked artifact -> MIR -> IR -> LIR -> ARC -> runtime image publication

child:
  mapped runtime image -> LIR interpreter/backend execution
```

Shared memory is mandatory for this path because serialization/deserialization
of compiler data is too expensive and would expose the child to representations
it should not consume. If shared-memory infrastructure is missing or incomplete,
the fix is to restore and extend that infrastructure, not to add a serialized
semantic-data path.

## Public API Boundaries

Post-check public callers use checked artifacts plus explicit roots and target
configuration. This includes compile, eval, CLI, REPL, dev shim, interpreter
shim, snapshots, glue, platform tests, and host-effect tests.

Forbidden public routes include:

```text
lowerTypedCIRToLir*
lowerTypedCIRToSemanticEval*
SemanticEvalProgram
SemanticEvalTopLevelRoot
NoRootProc
NoRootDefinition
TypedCIR.Modules plus optional roots
post-check root discovery by export/name/expression/procedure-order scan
```

Post-check lowering accepts:

- `CheckedModuleArtifact`
- `ImportedModuleView`
- `LoweringModuleView`
- explicit `RootRequest` sets
- relation artifacts
- target configuration
- compile-time plan sinks only during checking finalization

After publication, missing roots, missing platform data, missing static dispatch
plans, missing constant instances, missing callable binding instances, missing
runtime schemas, and missing layout requests are compiler bugs.

## Platform And App Relations

Platform/app requirements are finalized during checked artifact publication.
Late platform requirement checking in compile, eval, CLI, or backend paths is
forbidden.

App and platform root artifacts for executable builds are published as one
co-finalization group. No artifact in the group is published until:

- platform requirement checking is complete
- app values required by the platform are resolved
- numeric default finalization is complete
- static dispatch finalization is complete
- compile-time constant evaluation is complete
- callable promotion is complete
- platform-required binding construction is complete

Unbound type variables at a `requires` boundary are allowed when checking the
app side. The platform author is responsible for handling them. The app should
not be forced to close tag unions or otherwise over-constrain values solely for
the platform boundary.

## Layouts

Executable MIR constructs an explicit logical layout graph. LIR receives
committed layouts. Recursive physical layout indirection is graph-based and
runs before storage lowering.

Logical row ids from row-finalized mono MIR are not physical offsets. Layout
lowering translates logical record fields, tag ids, and payload ids through the
explicit layout store.

Zero-sized types remain ordinary layout cases. Lists of zero-sized elements use
the runtime representation selected by the layout store. Boxes around zero-sized
payloads, singleton tags, records with zero-sized fields, and recursive nominal
layouts are handled by the same logical-layout graph. They are not special
source-syntax cases.

## Procedure Identity

Procedure identity must be stable across checked, lifted, promoted, hosted,
platform-required, and synthetic procedure sources. A raw symbol alone is not
enough in every context because procedures can be imported, lifted, promoted by
compile-time evaluation, created for semantic instantiation, or generated as
adapters.

Callable procedure templates use explicit variants:

```zig
const CallableProcedureTemplateRef = union(enum) {
    /// Procedure template created from a checked source top-level declaration.
    /// Example:
    ///
    /// add_one : I64 -> I64
    /// add_one = |x| x + 1
    checked: ProcedureTemplateRef,

    /// Procedure template created by lifting a local function or closure out
    /// of an owning checked procedure.
    /// Example:
    ///
    /// make_adder = |n| |x| x + n
    lifted: LiftedProcedureTemplateRef,

    /// Compiler-created procedure template with no direct source declaration,
    /// such as a promoted callable result or generated wrapper.
    /// Example:
    ///
    /// add_one : I64 -> I64
    /// add_one = make_adder(1)
    synthetic: SyntheticProcedureTemplateRef,
};
```

Executable specialization keys identify one executable version of one callable
procedure at one requested source function type and one executable
representation boundary. Later stages do not recover a procedure by display
name, source symbol alone, or function type alone.

## Detailed Data And Stage Contracts

The sections above describe ownership at a high level. This section records the
specific data contracts that make those boundaries enforceable.

### Checked Artifact Bodies And Types

A checked artifact owns the checked bodies and checked type payloads that later
stages consume. Later stages do not read raw CIR arenas, checker-local type
stores, parser identifiers, or mutable `ModuleEnv` internals.

The checked type store is a target-independent graph of source type payloads:

```zig
const CheckedTypeStore = struct {
    nodes: Store(CheckedTypeNode),
    roots: Store(CheckedTypeRoot),
    concrete_roots: Map(CanonicalTypeKey, CheckedTypeRoot),
    schemes: Map(CanonicalTypeSchemeKey, CheckedTypeScheme),
};

const CheckedTypeScheme = struct {
    root: CheckedTypeRoot,
    generalized_vars: Span(CheckedTypeRoot),
};
```

Conceptually, checked type payloads are explicit nodes rather than canonical
keys:

```zig
const CheckedTypeNode = union(enum) {
    primitive: PrimitiveTypeId,
    function: CheckedFunctionType,
    record: CheckedRecordType,
    record_empty,
    tuple: CheckedTupleType,
    tag_union: CheckedTagUnionType,
    tag_union_empty,
    transparent_alias: CheckedAliasType,
    nominal: CheckedNominalType,
    opaque: CheckedOpaqueType,
    box: CheckedTypeRoot,
    list: CheckedTypeRoot,
    open_var: CheckedOpenVar,
    delayed_numeric: CheckedDelayedNumeric,
    constrained: CheckedConstrainedVar,
    generalized: CheckedGeneralizedVar,
    placeholder: CheckedPlaceholder,
    recursive_ref: CheckedTypeRoot,
};

const CheckedFunctionType = struct {
    args: Span(CheckedTypeRoot),
    ret: CheckedTypeRoot,
    occurrence: CallableOccurrenceId,
    needs_instantiation: bool,
};

const CheckedRecordType = struct {
    fields: Span(CheckedRecordField),
    extension_tail: ?CheckedRowTailId,
};

const CheckedTagUnionType = struct {
    tags: Span(CheckedTag),
    extension_tail: ?CheckedTagTailId,
};

const CheckedAliasType = struct {
    alias: CanonicalAliasId,
    args: Span(CheckedTypeRoot),
    backing: CheckedTypeRoot,
};

const CheckedNominalType = struct {
    nominal: CanonicalNominalId,
    args: Span(CheckedTypeRoot),
    backing: CheckedTypeRoot,
    representation: CheckedNominalRepresentationRef,
};

const CheckedOpaqueType = struct {
    nominal: CanonicalNominalId,
    marker: OpaqueRepresentationMarker,
};

const CheckedOpenVar = struct {
    id: CheckedTypeVarId,
    kind: enum { flex, rigid, row_tail, tag_tail },
    constraints: Span(CheckedTypeConstraint),
};
```

The exact node names may differ, but the payload categories and ownership do
not. Function `needs_instantiation` records whether a function occurrence still
contains generalized/open checked variables and therefore must be
clone-instantiated before concrete use. Alias and nominal nodes carry both
wrapper identity and backing payload refs. Opaque nodes carry an explicit
marker proving that their backing is unavailable outside the authorized
capability. Record and tag extension tails are payload fields, not information
recovered from canonical keys. Canonical ids in these nodes are semantic
identities, not parser-local source ids.

The exact Zig field names may differ, but the store must preserve:

- nominal identity
- transparent alias identity
- record and tag row shape
- row extension tails
- function arity, argument roots, return root, and callable occurrence identity
- numeric constraints and delayed numeric-default phase
- static-dispatch method constraints
- source regions required for diagnostics

Canonical type keys are useful for lookup and deduplication, but keys are not
complete payloads. A key may identify that two source types are equivalent for a
particular request; it cannot replace the checked graph needed to lower rows,
nominals, aliases, functions, numeric constraints, or method constraints.

`concrete_roots` and `schemes` are owned by the artifact that owns the checked
type graph. Concrete-root lookup is valid only when the requested type has
already been proven concrete. Scheme lookup resolves to the stored root plus
the stored generalized-variable roots. `record_empty`, `tag_union_empty`, and
`recursive_ref` are explicit checked payload nodes. Recursive checked type
edges are therefore checked payload data, not a later inference from source
syntax, nominal names, or row-finalized shape records.

`CanonicalTypeKey` and `CanonicalTypeSchemeKey` are normalized identities:

```zig
const CanonicalTypeKey = union(enum) {
    primitive: PrimitiveTypeKey,
    nominal: CanonicalNominalKey,
    transparent_alias: CanonicalAliasKey,
    record: CanonicalRecordKey,
    tag_union: CanonicalTagUnionKey,
    tuple: CanonicalTupleKey,
    function: CanonicalFunctionKey,
    variable: CanonicalCheckedVariableKey,
};

const CanonicalTypeSchemeKey = struct {
    generalized: Span(CanonicalCheckedVariableKey),
    body: CanonicalTypeKey,
};
```

Normalization flattens record and tag extension chains, sorts explicit
record/tag labels by canonical label identity, preserves open row and tag tails,
and preserves tag payload order inside each tag. Wrapper identity, transparent
alias identity, nominal identity, method constraints, numeric constraints, and
generalized-variable identity are part of the key when they affect post-check
semantics. Duplicate labels in a normalized row are a compiler bug because
checking has already accepted or rejected the program before publication.

Checked bodies are also artifact-owned. A checked body store contains checked
expressions, statements, patterns, declaration references, local binding ids,
type roots, string-literal ids, and method-token diagnostic regions. It must not
store references into mutable checking arenas. If a later stage needs a checked
expression, pattern, source region, or checked source type, artifact publication
copies or interns it into the artifact-owned stores first.

`CheckedBodyGraphTraversal` is centralized. Checked artifact tables
referenced by body nodes are part of the checked body graph, even when the
reference is not an ordinary child expression in the surface syntax. The
traversal follows child expression, pattern, body, source-type-root, and
literal ids stored in static-dispatch plans, nested procedure sites, const
templates, callable templates, private-capture templates, and future checked
tables. A new checked table that can reference body payloads must register with
this traversal before it can be published.

Canonicalization and checking publish diagnostic subregions separately from
whole expression regions when later diagnostics need token-level precision. For
method calls, the method identifier subregion is explicit:

```zig
const MethodDiagnosticRegion = struct {
    whole_call: Region,
    method_identifier: Region,
};
```

Diagnostics for expressions such as `35.foo()` and `12.34.foo()` use the
published `method_identifier` region. Later stages and diagnostic formatters do
not scan source text to recover where the method name starts.

### Checked Artifact Publication Order

Checking finalization publishes exactly one immutable checked module artifact,
or it publishes nothing. A module is not checked merely because source types
solved. A module is checked only after every checked-stage output required by
importers and post-check lowering has been built, verified, and bundled into
the artifact.

Publication order is part of the design contract:

1. Finish type solving and collect all user-facing diagnostics.
2. Copy every lowering-visible checked type root and checked body into
   artifact-owned `CheckedTypeStore` and `CheckedBodyStore` records.
3. Build the checked method registry, normalized static dispatch plans, nested
   procedure-site table, and builder-form resolved value-reference table.
4. Build checked procedure templates for source procedures, hosted wrappers,
   intrinsic wrappers, entry wrappers, and other compiler-created checked
   procedure bodies that do not depend on evaluated callable roots.
5. Build explicit root requests for concrete runtime, tool, test, REPL,
   development, and compile-time entrypoints. Generic exports are not root
   requests merely because they are exported.
6. Build hosted procedure tables and platform-required declaration tables.
   App-specific platform-required binding tables are built only for executable
   app/platform co-finalization groups.
7. Build public exports, provides/requires metadata, and interface capability
   records.
8. Reserve `ConstRef` identities for non-function top-level constants, allocate
   direct procedure values, create top-level procedure binding rows, and
   initialize the in-progress top-level value table.
9. Build callable-aware summary-only lowering records and the compile-time root
   dependency graph.
10. Evaluate concrete compile-time constants and callable roots through the
   MIR-family path and LIR interpreter in dependency order.
11. Fill evaluated constants into reserved `ConstRef` rows and seal any generic
   const or callable templates required by importers.
12. Promote concrete callable results through a reserve/fill/seal lifecycle for
   promoted procedure values, proc bases, private capture graphs, and checked
   templates.
13. Replace every pending callable root in the top-level value table with a
   sealed procedure binding before publication.
14. Build app-specific platform relation and binding tables for executable
   app/platform groups before either root artifact is published.
15. Verify the artifact has no pending top-level values, builder-only resolved
   refs, missing relation entries, missing static-dispatch plans, or missing
   compile-time value rows.

The published artifact is one complete semantic unit:

```zig
const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    env: ModuleEnv,
    canonical_names: CanonicalNameStore,
    exports: ExportTable,
    checked_types: CheckedTypeStore,
    checked_bodies: CheckedBodyStore,
    provides_requires: ProvidesRequiresMetadata,
    method_registry: MethodRegistry,
    static_dispatch_plans: StaticDispatchPlanTable,
    resolved_value_refs: ResolvedValueRefTable,
    procedure_templates: CheckedProcedureTemplateTable,
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_declarations: PlatformRequiredDeclarationTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    compile_time_roots: ?ComptimeRootTable,
    comptime_values: CompileTimeValueStore,
    const_instances: ConstInstantiationStore,
    callable_binding_instances: CallableBindingInstantiationStore,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTable,
    callable_set_descriptors: CallableSetDescriptorStore,
    platform_requirement_relations: PlatformRequirementRelationTable,
};

const CheckedProcedureTemplateTable = struct {
    templates: Span(CheckedProcedureTemplate),
};
```

Later stages may consume narrowed views of this artifact, but every view is
derived from the complete artifact. A view must not scan or mutate raw checked
modules, raw `ModuleEnv` entries, source declarations, or checker stores to
rebuild missing semantic data. Verification checks that every exported
procedure has either a checked procedure template or an explicit non-procedure
top-level entry, every exported promoted procedure has a
`PromotedProcedureTable` row, and every checked procedure template points at
artifact-owned checked body and checked type payload data. It also checks that
callable-set descriptors, canonical names, and platform relation rows visible
through narrowed views are artifact-owned rows. A narrowed view must not rebuild
them from raw checked modules, source identifier stores, source declarations, or
runtime lowering output.

`CheckedModuleArtifact.callable_set_descriptors` is persisted semantic data.
`LoweredProgram.callable_set_descriptors` is a per-target, per-lowering runtime
copy owned by one lowered program. The runtime table may deep-copy optional
published identities from the artifact table, but those lowering-run rows are
not the source of truth for imported artifacts, cache keys, compile-time values,
or future lowering runs.

The artifact owns the `ModuleEnv` storage that backs source-visible debug
verification and any still-needed source/CIR lookup at the artifact boundary.
Republishing an artifact for the same module transfers that storage into the
replacement artifact. The replacement must not store a borrowed raw pointer
while another artifact owns the allocation, and the original artifact must not
deinitialize storage after the replacement has taken ownership.

Top-level definition binding patterns use the finalized definition type as
their lowering-visible checked type. A raw top-level pattern-node type may still
exist for source regions and binding identity, but it is not the authority for
post-check lowering. Local patterns inside checked bodies use their own checked
pattern type because the body owns those binders.

### Runtime Body Projection

Checked bodies can contain checked statement nodes that are required for
checking, declaration publication, local type declarations, static-dispatch
resolution, or source tooling, but have no runtime effect. Examples include:

```text
import_
alias_decl
nominal_decl
type_anno
type_var_alias
```

The checked body boundary therefore exposes two views:

1. The complete checked body graph, including non-runtime checked statements,
   for artifact publication, diagnostics, and source tooling.
2. The runtime body projection, which preserves source order among runtime
   statements while omitting checked statements that have no runtime effect.

Mono consumes the runtime projection. It must not lower non-runtime checked
statements into MIR, and it must not reinterpret declaration statements to
recover missing data. Checked artifact publication must fully account for the
semantic data from non-runtime statements before mono lowering begins. If any
non-runtime checked statement reaches row-finalized mono, lifted MIR,
lambda-solved MIR, executable MIR, IR, LIR, ARC, or a backend, that is a
compiler bug.

### Resolved Value References

Checking finalization categorizes every value-like reference that can reach
mono MIR before the checked artifact is published. Mono consumes that sealed
categorization. It must not decide whether a reference is local, top-level,
imported, hosted, platform-required, promoted, or callable by scanning names,
source syntax, export tables, declaration order, or the later capture graph.

Conceptual shape:

```zig
const ResolvedValueRefId = enum(u32) { _ };

const ConstUseTemplate = struct {
    const_ref: ConstRef,
    requested_source_ty_template: CanonicalTypeTemplateKey,
    requested_source_ty_payload: ArtifactCheckedTypeRef,
};

const ProcedureBindingRef = union(enum) {
    top_level: ArtifactTopLevelProcedureBindingRef,
    imported: ImportedProcedureBindingRef,
    hosted: HostedProcRef,
    platform_required: RequiredAppProcedureRef,
    promoted: PromotedProcedureRef,
};

const ProcedureUseTemplate = struct {
    binding: ProcedureBindingRef,
    source_fn_ty_template: CanonicalTypeTemplateKey,
    source_fn_ty_payload: ArtifactCheckedTypeRef,
};

const ResolvedValueRef = union(enum) {
    local_param: ParamId,
    local_value: LocalBindingId,
    local_mutable_version: MutableVersionId,
    pattern_binder: PatternBinderId,
    local_proc: LocalProcRef,

    top_level_const: ConstUseTemplate,
    imported_const: ConstUseTemplate,
    platform_required_const: ConstUseTemplate,

    top_level_proc: ProcedureUseTemplate,
    imported_proc: ProcedureUseTemplate,
    hosted_proc: ProcedureUseTemplate,
    platform_required_proc: ProcedureUseTemplate,
    promoted_top_level_proc: ProcedureUseTemplate,

    platform_required_declaration: PlatformRequiredDeclarationId,
};

const ResolvedValueRefRecord = struct {
    expr: CheckedExprId,
    ref: ResolvedValueRef,
    checked_ty: CheckedTypeId,
    scope_depth: u32,
};

const ResolvedValueRefTable = struct {
    records: Store(ResolvedValueRefRecord),
    by_expr: Map(CheckedExprId, ResolvedValueRefId),
};
```

The exact Zig names may differ, but the partition must not. The sealed table
does not contain pending top-level bindings, unresolved import lookups, source
names, or expression shapes that later stages must categorize.

Checking finalization may use builder-only cases such as local top-level binding
and imported top-level binding while reserving top-level table rows and reading
imported artifact views. Those cases must be resolved to sealed const/procedure
entries before publication. A published artifact never exposes builder-only
resolved value refs.

`CheckedPatternBinder` records reassignment permission separately from binder
origin:

```zig
const CheckedPatternBinder = struct {
    id: PatternBinderId,
    pattern: CheckedPatternId,
    reassignable: bool,
};
```

`ResolvedValueRef.local_param` answers where the binder was introduced.
`CheckedPatternBinder.reassignable` answers whether the source binder may be
reassigned. Mono consumes both. Later stages must not infer mutable-parameter
behavior from the spelling of a name.

`ConstUseTemplate.requested_source_ty_template` and
`ProcedureUseTemplate.source_fn_ty_template` are template identities, not
concrete payloads. In a generic checked procedure body, the same template can
instantiate to different concrete payloads in different mono specializations.
Mono clone-instantiates `requested_source_ty_payload` or
`source_fn_ty_payload` in the current specialization, connects it to the
current expression endpoint, and then constructs the concrete
`ConstInstantiationKey`, `CallableBindingInstantiationKey`, or
`ProcedureCallableRef`.

Platform-required const/procedure uses carry identity through the resolved value
ref, but authorization to instantiate private app templates lives in the
platform artifact's relation-owned binding table. Mono obtains the relation
template closure from that table by explicit platform-required binding id. It
must not scan app exports, match procedure names, or grant access to every
private template in the relation artifact.

`platform_required_declaration` is allowed only in standalone platform artifacts
that publish requirement declarations for checking, docs, or glue metadata.
Executable platform artifacts replace every required lookup with
`platform_required_const` or `platform_required_proc` before publication.

### Checked Type Variable Identity

Distinct unresolved checked type variables remain distinct during artifact
publication. Two open type graphs that are structurally similar must not be
deduplicated if doing so would merge variables that checking kept separate.

Publication may create temporary `CheckedSourceTypeRoot` or equivalent ids to
tie source type roots to the payload store while the artifact is being built.
Those ids are publication-local. Published artifacts expose stable checked type
roots and schemes, not references back into the publication workspace.

Conceptually:

```zig
const CheckedSourceTypeRoot = struct {
    source_var: CheckerTypeVar,
    checked_root: CheckedTypeRoot,
    openness: CheckedSourceTypeOpenness,
};

const CheckedTypePublication = struct {
    store: CheckedTypeStore,
    source_type_roots: Span(CheckedSourceTypeRoot),
};

const CheckedSourceTypeOpenness = enum {
    proven_concrete,
    open_checked_graph,
};
```

`CheckedTypePublication.source_type_roots` is publication-only. It is not
serialized, not stored as a MIR input, and not consumed after checked artifact
publication. While the artifact is being built, every publication path that
starts from a checker type variable uses this index to find the selected
checked root: body copying, pattern copying, call-site source function payload
publication, procedure-template root publication, root request publication,
compile-time root publication, resolved value-ref publication, method registry
publication, and static-dispatch plan publication.

Open graphs preserve flex-variable, rigid-variable, row-tail, tag-tail,
delayed-numeric, and method-constrained variable identity exactly as checking
published it. `CheckedTypeStore.rootForKey` or equivalent canonical-key lookup
is valid only for roots whose concreteness has already been proven. For open
graphs, publication and later lowering pass the explicit
`CheckedSourceTypeRoot`; they do not look up a root by key and accidentally
merge distinct unresolved variables.

This rule matters for open records, open tag unions, delayed numeric variables,
method-constrained variables, and `requires` boundaries. If checking leaves two
variables unconstrained, downstream stages must see two variables unless an
explicit checked-stage relation connects them.
Concrete payloads may share one checked root by canonical key, and every source
variable that resolved to that concrete payload still receives an index entry.
Open payloads, generalized variables, row tails, tag tails, and
static-dispatch constrained variables keep distinct checked roots even when
their canonical keys match.

### Numeric Defaulting And Closure

Numeric defaulting is staged. Checking finalization closes values that must be
closed for user-facing diagnostics and artifact publication. Mono specialization
may still resolve numeric variables whose final type depends on concrete call
arguments, static dispatch, or a concrete root request.

Checked payloads mark numeric variables with their defaulting phase:

```zig
const NumericDefaultPhase = enum {
    checking_finalized,
    mono_specialization,
};
```

`NumericDefaultPhase.mono_specialization` is selected when a numeric variable
can be closed only after mono has connected concrete call arguments, concrete
root requests, static-dispatch targets, equality-dispatch targets, mutable
versions, generated binders, or expected endpoints. Checking finalization does
not guess those endpoints early, and mono does not reopen
`checking_finalized` numeric variables.

Mono connects all known argument, return, binder, static-dispatch, equality, and
expected-type slots before defaulting remaining mono-specialization numerics.
Only after those connections are complete may mono apply default numeric types
or close equality-only variables. Row and tag tails are closed only at explicit
extension positions where the checked source type says closure is required.

Numeric low-level operators lower operands under one selected source constraint
type. Defaultable desugared arithmetic operators include `plus`, `minus`,
`times`, `div_by`, `div_trunc_by`, `rem_by`, and unary `negate`; their operand
type is the specialized result/operand type. For comparisons such as `<`, `>`,
`<=`, and `>=`, the result is ordinary Roc `Bool`, so the operand type comes
from the checked numeric constraint, an explicitly known operand type, or the
solved operand type after mono defaulting. Non-numeric method constraints,
equality-only constraints, rows, tag tails, and unconstrained variables are not
numeric-default decisions. Once selected, the operand type is used to lower both
operands, including unsuffixed literals and mutable-parameter values, and is
stored as the low-level operation's `source_constraint_ty`. Literal carrier
width remains separate from storage: an `i128_literal` carrier for `1.U32`
writes and participates in the operation as `U32`, not as a default wide integer
that a later low-level op reconciles.

### Concrete Source Type Payloads

Mono and later stages use `ConcreteSourceTypeRef` values for concrete requested
source types. A concrete source type has two parts:

```zig
const ConcreteSourceTypeRef = enum(u32) { _ };

const ConcreteSourceTypeRoot = struct {
    key: CanonicalTypeKey,
    payload: CheckedTypeRoot,
    owner: ConcreteSourceTypeRef,
};

const ConcreteSourceTypeStore = struct {
    checked_types: CheckedTypeStore,
    roots: Store(ConcreteSourceTypeRoot),
    by_key: Map(CanonicalTypeKey, ConcreteSourceTypeRef),
    by_source: Map(ConcreteSourceTypeSourceKey, ConcreteSourceTypeRef),
};
```

The key is for identity and lookup. The payload is the checked type graph copied
or instantiated into the current lowering run. The owner scopes ids inside the
payload. These three pieces must stay together. A stage must not pass a
canonical key around and later reconstruct the payload from names, source
syntax, or compatible row shape.

Concrete refs also remember why a payload exists:

```zig
const ConcreteSourceTypeSource = union(enum) {
    local_checked_root: CheckedSourceTypeRoot,
    imported_checked_root: ArtifactCheckedTypeRootRef,
    template_instantiation: TemplateInstantiationSource,
    relation_requested_payload: PlatformRelationRowId,
    const_or_callable_request: SemanticInstantiationRequestId,
    executable_payload_projection: ExecutablePayloadProjectionRef,
};

const ConcreteSourceTypeSourceKey = union(enum) {
    exact_source_root: CheckedSourceTypeRootKey,
    imported_exact_source: ArtifactCheckedTypeRootKey,
    instantiated_template: TemplateInstantiationKey,
    relation_payload: PlatformRelationPayloadKey,
    semantic_request: SemanticInstantiationRequestKey,
    executable_projection: ExecutablePayloadProjectionKey,
};
```

`by_source` is stricter than `by_key`. It allows exact-source reuse when the
same checked payload source is requested again, including open payloads whose
canonical key would be too coarse. After substitution, source owners inside the
payload are remapped to the current artifact or lowering run before the payload
is compared or registered. Template-variable unification and concrete-variable
unification are one logical operation: the instantiator creates a concrete
payload, connects template variables to endpoint variables, and registers the
result atomically. It must not first publish a partial template payload and
later patch it with concrete-variable links.

When an imported type payload enters a lowering run, canonical names inside the
payload are remapped through the artifact name resolver before the payload is
registered locally. When the same canonical key is requested with equivalent
payloads, the store may reuse the existing concrete ref; otherwise the request
must carry a new explicit payload.

### Program Literal Pool

String and crash payload lifetimes are separated:

- source lifetime: parser/checker `base.StringLiteral.Idx`
- artifact lifetime: artifact-owned `CheckedStringLiteralId`
- lowered-program lifetime: `ProgramLiteralId`
- final LIR lifetime: `LirStore` string literal id

Conceptually:

```zig
const CheckedStringLiteralId = enum(u32) { _ };

const ProgramLiteralPool = struct {
    owner: LoweringRunId,
    literals: Span<ProgramLiteral>,
    by_artifact_literal: Map<ArtifactStringLiteralRef, ProgramLiteralId>,
};

const ProgramLiteral = union(enum) {
    string: StringBytes,
    crash_payload: StringBytes,
    pattern_literal: StringBytes,
};
```

`CheckedStringLiteralId` indexes `CheckedBodyStore.string_literals` in the
checked artifact that owns the checked body. It is valid only inside that
artifact and inside an artifact-local lowering context that is currently reading
that checked body. It must not be exported as MIR, IR, LIR, cache-key, backend,
interpreter, or runtime-image payload without first resolving to bytes through
the owning checked body store.

Mono creates the program literal pool. MIR stages and IR carry only
`ProgramLiteralId` for lowered string, crash, and string-pattern payloads. They
must not retain source literal ids or re-intern bytes independently. IR-to-LIR
lowering interns every used `ProgramLiteralId` into `LirStore` exactly once for
the final LIR program.
The program pool is owned by the lowering run, not by parser/checker storage and
not by final LIR. A checked artifact literal is copied or referenced through the
artifact string table once when mono lowers it into the program pool. Later MIR,
IR, LIR, ARC, backend, interpreter, and runtime-image stages do not read parser
literal ids and do not independently intern source literal bytes.

The program literal pool covers every checked literal byte payload that
survives checking: string expression literals, string interpolation segments,
bytes literal payloads, string literal patterns in source `match`, and
user-written `crash` expression or statement messages. Imported template
lowering reads imported literal bytes from the imported checked body store view;
it must not read the exporter's `ModuleEnv`, the importer's `ModuleEnv`, or a
parser string store.

### Canonical Name Remapping

Canonical names are interned by exact canonical bytes before they cross a
checked artifact boundary:

```zig
const CanonicalNameId = enum(u32) { _ };

const CanonicalNameStore = struct {
    names: InternMap([]const u8, CanonicalNameId),
};

const RecordFieldLabelId = distinct CanonicalNameId;
const TagLabelId = distinct CanonicalNameId;
const MethodNameId = distinct CanonicalNameId;
const ExportNameId = distinct CanonicalNameId;
const ExternalSymbolNameId = distinct CanonicalNameId;
```

`CanonicalNameId` values are dense, store-local ids. An id from an imported
artifact's `CanonicalNameStore` is not comparable to an id in the lowering
run's store or another artifact's store until it has been remapped through the
central resolver/publisher APIs. The distinct wrappers prevent using a method
name as a row label, a tag label as an export name, or an ABI symbol as a
source-visible name.

The byte sequence is the semantic spelling for the domain being keyed: record
field labels use field-label bytes, tag labels use constructor-label bytes,
method names use method-name bytes, export names use source-visible export
bytes, hosted and external symbols use ABI symbol bytes, glue schemas use
schema-display bytes after normalization, and runtime-image symbols use the
published object/local symbol bytes. Conversion from parser or checker
identifier ids to canonical names may happen only while the exact owning
identifier store is available. Once a value can be imported, cached,
serialized, or compared after that store is gone, it must carry a canonical
name id or a higher-level canonical key.

Imported artifacts use artifact-local canonical-name ids. A lowering run uses
its own canonical-name store. Imported names cross the boundary only through an
explicit resolver:

```zig
const CanonicalNameView = union(enum) {
    module,
    package,
    top_level_value,
    local_value,
    field,
    tag,
    nominal,
    transparent_alias,
    opaque,
    method,
    static_dispatch_member,
    hosted_symbol,
    platform_requirement,
    platform_relation,
    const_instance,
    callable_binding_instance,
    promoted_procedure,
    private_capture,
    erased_callable_code,
    glue_schema,
    runtime_image_symbol,
};

const LoweringRunContext = struct {
    run_names: *CanonicalNameStore,
    artifacts: *const CheckedArtifactAvailabilityRegistry,
    remaps: ArtifactNameRemapStore,
    checked_type_projector: CheckedTypeProjector,
};

const ArtifactLocalNameId = enum(u32) { _ };
const LoweringRunNameId = enum(u32) { _ };

const ArtifactNameResolver = struct {
    artifact: CheckedModuleArtifactKey,

    fn canonicalName(
        self: *const ArtifactNameResolver,
        artifact_name: ArtifactLocalNameId,
    ) LoweringRunNameId;

    fn procBase(
        self: *const ArtifactNameResolver,
        artifact_proc: ArtifactProcBaseId,
    ) ProcBaseKeyRef;

    fn sourceType(
        self: *const ArtifactNameResolver,
        artifact_ty: ArtifactConcreteSourceTypeRef,
    ) ConcreteSourceTypeRef;
};

const ArtifactNamePublisher = struct {
    artifact: CheckedModuleArtifactKey,

    fn publishCanonicalName(
        self: *ArtifactNamePublisher,
        local_name: LoweringRunNameId,
    ) ArtifactLocalNameId;

    fn publishSourceType(
        self: *ArtifactNamePublisher,
        local_ty: ConcreteSourceTypeRef,
    ) ArtifactConcreteSourceTypeRef;
};

const CheckedTypeProjector = struct {
    resolver: ArtifactNameResolver,
    publisher: ?ArtifactNamePublisher,

    fn importRoot(
        self: *CheckedTypeProjector,
        artifact_root: ArtifactCheckedTypeRootRef,
    ) CheckedTypeRoot;

    fn importPayload(
        self: *CheckedTypeProjector,
        artifact_payload: ArtifactCheckedTypePayloadRef,
    ) CheckedTypeRoot;

    fn publishPayload(
        self: *CheckedTypeProjector,
        local_payload: CheckedTypeRoot,
    ) ArtifactCheckedTypePayloadRef;
};
```

The exact APIs may differ, but all imported checked type payloads, procedure
templates, constants, method owners, and relation artifacts must be projected
through the resolver before they are used in the current lowering run. A stage
must not mix artifact-local ids with lowering-run ids.

Artifact-local ids and lowering-run ids use distinct wrapper types. A remap is
created at one of these boundaries:

- importing a checked type payload, nominal declaration, procedure template,
  const template, callable template, relation row, method entry, hosted entry,
  interface capability, private capture graph, or compile-time value graph into
  a lowering run
- publishing a local checked artifact component back into artifact-local
  storage
- comparing a compile-time materialization plan created in one artifact with
  executable names created in another artifact

There is no ad hoc remapping. Code that needs a cross-artifact name receives an
`ArtifactNameResolver` or `ArtifactNamePublisher` in its explicit context.
Debug verification asserts that no artifact-local id is stored in lowering-run
data and no lowering-run id is stored in published artifact data unless it has
passed through the appropriate publisher. The verifier walks lowered keys,
table rows, method registry payloads, static-dispatch plans, const templates,
platform relation artifacts, glue schemas, and runtime-image symbol records;
foreign canonical-name ids in any of those records are compiler bugs.

`CanonicalNameView` documents which identity domain is being remapped. It is
part of the verification contract: a top-level value id must not be accepted
where a nominal id is required, a glue schema id must not be accepted as a
runtime image symbol, and a relation id must not be compared with an imported
module id by raw integer value. `CheckedTypeProjector` is the central mechanism
for checked-type payload remapping; code that needs to import or publish
checked payloads receives the projector explicitly instead of constructing
canonical-name remaps locally.

### Artifact Availability Registry

Checked artifact availability is an exact-key registry. It is an in-memory view
over currently published artifacts:

```zig
const CheckedArtifactAvailabilityRegistry = struct {
    storage: *const CheckedArtifactStorage,
    by_key: Map(CheckedModuleArtifactKey, CheckedArtifactStorageIndex),
    by_module_context: Map(ModuleContextIdentity, CheckedModuleArtifactKey),
};
```

The registry does not own imported semantic data by copying views into a map.
It indexes owned artifact storage and constructs `ImportedModuleView` records
from exact storage entries. Publishing an artifact removes any stale entry for
the same module identity and checking context, inserts the new exact key, and
invalidates views derived from the stale key. Unpublishing removes the exact key
and makes every later lookup of that key fail the stale-key check. Importers
look up direct imports by `CheckedModuleArtifactKey`; a direct import key is not
patched after a cache hit.

Compile-time finalization receives a read-only availability view so it can
assemble imported dependency views deterministically. It requests imported
templates, constants, compile-time values, and plan views through exact keys.
It does not keep ad hoc pointers into another artifact, and it does not rebuild
imported dependencies by module name after publication.

### Imported Template Closure Views

Imported executable use requires an explicit semantic closure. If an imported
procedure, constant template, callable eval template, relation binding, or
promoted wrapper can instantiate private helper templates, the exporting
artifact publishes an `ImportedTemplateClosureView`:

```zig
const ImportedModuleView = struct {
    artifact_key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    names: ArtifactNameResolver,
    templates: ArtifactTemplateResolver,
    checked_bodies: CheckedBodyStoreView,
    checked_types: CheckedTypeStoreView,
    nominal_declarations: NominalDeclarationStoreView,
    top_level_values: TopLevelValueTableView,
    method_entries: MethodRegistryView,
    hosted_entries: HostedProcTableView,
    platform_entries: PlatformRequirementTableView,
    interface_capabilities: InterfaceCapabilityTableView,
    compile_time_values: CompileTimeValueStoreView,
    compile_time_plans: CompileTimePlanStoreView,
    callable_set_descriptors: CallableSetDescriptorStoreView,
};

const ImportedProcedureBindingView = union(enum) {
    direct: struct {
        binding: ArtifactProcedureBindingRef,
        requested_source_fn_ty: ArtifactConcreteSourceTypeRef,
        template_closure: ImportedTemplateClosureView,
    },
    callable_eval: struct {
        callable_eval_template: ArtifactCallableEvalTemplateRef,
        entry_wrapper: ArtifactEntryWrapperRef,
        requested_source_fn_ty: ArtifactConcreteSourceTypeRef,
        template_closure: ImportedTemplateClosureView,
    },
};

const ImportedConstTemplateView = struct {
    template: ArtifactConstTemplateRef,
    source_ty: ArtifactConcreteSourceTypeRef,
    closure: ImportedTemplateClosureView,
};

const ArtifactTemplateResolver = struct {
    artifact: CheckedModuleArtifactKey,

    fn procedure(
        self: *const ArtifactTemplateResolver,
        ref: ArtifactProcedureTemplateRef,
        closure: ImportedTemplateClosureView,
    ) ProcedureTemplateRef;

    fn constTemplate(
        self: *const ArtifactTemplateResolver,
        ref: ArtifactConstTemplateRef,
        closure: ImportedTemplateClosureView,
    ) ConstTemplateRef;

    fn privateCapture(
        self: *const ArtifactTemplateResolver,
        ref: ArtifactPrivateCaptureRef,
        closure: ImportedTemplateClosureView,
    ) PrivateCaptureRef;
};

const ImportedTemplateClosureView = struct {
    owner_artifact: CheckedModuleArtifactKey,
    checked_bodies: Span(ArtifactCheckedBodyRef),
    checked_type_roots: Span(ArtifactCheckedTypeRootRef),
    checked_type_schemes: Span(ArtifactCheckedTypeSchemeRef),
    checked_callable_bodies: Span(ArtifactCheckedCallableBodyRef),
    checked_const_bodies: Span(ArtifactCheckedConstBodyRef),
    procedure_templates: Span(ArtifactProcedureTemplateRef),
    checked_procedure_templates: Span(ArtifactCheckedProcedureTemplateRef),
    callable_eval_templates: Span(ArtifactCallableEvalTemplateRef),
    const_templates: Span(ArtifactConstTemplateRef),
    const_reification_plans: Span(ArtifactConstReificationPlanRef),
    callable_result_plans: Span(ArtifactCallableResultPlanRef),
    callable_promotion_plans: Span(ArtifactCallablePromotionPlanRef),
    promoted_procedures: Span(ArtifactPromotedProcedureRef),
    semantic_instantiation_procedures: Span(ArtifactSemanticInstantiationProcedureRef),
    private_promoted_capture_roots: Span(ArtifactPrivatePromotedCaptureRootRef),
    private_promoted_capture_nodes: Span(ArtifactPrivatePromotedCaptureNodeRef),
    private_captures: Span(ArtifactPrivateCaptureRef),
    private_capture_const_templates: Span(ArtifactConstTemplateRef),
    nested_proc_sites: Span(ArtifactNestedProcSiteRef),
    resolved_value_refs: Span(ArtifactResolvedValueRefId),
    static_dispatch_plans: Span(ArtifactStaticDispatchPlanRef),
    method_entries: Span(ArtifactMethodEntryRef),
    capabilities: Span(ArtifactInterfaceCapabilityRef),
    callable_set_descriptors: Span(ArtifactCallableSetDescriptorRef),
    plan_closures: Span(ArtifactCompileTimePlanRef),
};
```

Importers may instantiate only templates listed in the closure view carried by
the imported item they are using. They must not scan the exporting artifact for
private helpers, infer helpers from source names, or treat a public template id
as permission to access unrelated private templates.

Imported procedure bindings have two forms. A direct binding behaves like an
exported checked procedure template at the requested source function type. A
callable-eval binding behaves like an exported `CallableEvalTemplate` plus the
entry wrapper that evaluates it for one concrete request. The importer selects
the binding form from `ImportedProcedureBindingView`; it must not infer direct
vs evaluated callable behavior from the source expression, from whether the
value is function-typed, or from the presence of captures.

`proc_value` inherits the closure of the procedure binding that created it. If
an imported generic callable value later reserves a
`CallableBindingInstanceRef`, that reservation carries the imported binding
closure forward. The reservation is not authorized by the callable binding key
alone; it is authorized by the binding key plus the closure view that came from
the exporting artifact.

Closure entries are capability-bearing references. A procedure closure entry
authorizes the checked body, checked callable type, nested procedure sites,
private captures, method entries, and interface capabilities named by that
procedure. A const closure entry authorizes the const value graph template,
callable leaves, private const leaves, nominal declaration templates, and
materialization plans named by that const. A promoted-wrapper closure entry
authorizes the private capture graph and wrapper body plan named by the
promoted callable. The importer never receives blanket access to the exporting
artifact.

Imported closures contain template payloads and private helper payloads, not
parameterized concrete dependency summaries. Concrete dependency summaries are
owned by the artifact that owns the concrete `ConstInstantiationKey`,
`CallableBindingInstantiationKey`, `ProcedureCallableRef`, or promoted wrapper
instance. When an importer instantiates an imported template, it creates and
owns the concrete summary rows for that concrete request; it does not patch the
exporting artifact.

Debug verification checks both reachability directions. Every private ref
reachable from an imported template must either be public/exported or
explicitly listed in that template's `ImportedTemplateClosureView`. Every
canonical checked type key or scheme reachable from the closure must have a
corresponding checked type root/scheme in the closure or in the public checked
type view. Missing closure entries are compiler bugs because post-check stages
are not allowed to search the exporting artifact for unlisted helpers.

### Root Requests And Semantic Instantiations

Runtime roots and compile-time semantic instantiations are distinct:

```zig
const LoweringRequestSet = struct {
    runtime_roots: Span(RootProcedureRequest),
    compile_time_requests: Span(CompileTimeEvaluationRequest),
    purpose: LoweringPurpose,
};

const CompileTimeEvaluationRequest = union(enum) {
    local_root: ComptimeRootId,
    const_instance: ConstInstantiationRequest,
    callable_binding: CallableBindingInstantiationRequest,
    expect_root: ExpectRootRequest,
};

const RootRequestSet = struct {
    runtime_roots: Span(RootProcedureRequest),
};

const RootRequestTable = struct {
    requests: Span(RootProcedureRequest),
    by_order: Span(RootRequestId),
};

const RootProcedureRequest = struct {
    id: RootRequestId,
    source: RootSource,
    kind: RootKind,
    abi: RootAbi,
    exposure: RootExposure,
    order: RootOrderKey,
    target: RootTarget,
    requested_fn_ty: ConcreteSourceTypeRef,
    relation: ?PlatformAppRelationKey,
};

const LoweringPurpose = union(enum) {
    compile_time_evaluation,
    runtime_executable,
    test_runner,
    repl,
    glue,
    readonly_data_materialization,
    platform_relation_publication,
    development_tool,
};

const RootAbi = union(enum) {
    roc,
    hosted: HostedAbiKey,
    platform: PlatformAbiKey,
    test,
    repl,
    glue,
};

const RootExposure = union(enum) {
    private_runtime,
    exported_symbol: ExportedSymbolName,
    platform_entrypoint: PlatformEntrypointId,
    runtime_image_only,
};

const RootOrderKey = struct {
    artifact: CheckedModuleArtifactKey,
    source: RootSourceOrderKey,
    ordinal: u32,
};

const RootTemplateSelection = struct {
    template: ProcedureTemplateRef,
    imported_closure: ?ImportedTemplateClosureView,
};

const RootSource = union(enum) {
    app_main,
    exposed_procedure: TopLevelDeclKey,
    platform_required: PlatformRequirementId,
    test_entry: TestEntryId,
    repl_entry: ReplEntryId,
    glue_entry: GlueEntrypointId,
    compile_time_driver: ComptimeRootId,
};

const RootKind = union(enum) {
    concrete_runtime_proc,
    generic_export_template,
    hosted_dispatch_entry,
    platform_entry_wrapper,
    test_wrapper,
    repl_wrapper,
    glue_wrapper,
};

const RootTarget = union(enum) {
    procedure: ProcedureCallableRef,
    hosted: HostedProcedureRef,
    platform_required: PlatformRequiredProcedureUse,
    synthetic: SyntheticProcedureRef,
};

const LoweredRoot = struct {
    request: RootRequestId,
    lir_proc: LirProcSpecId,
    symbol: ?ObjectSymbolId,
    runtime_image_root: ?RuntimeRootId,
};

const SemanticInstantiationRequest = union(enum) {
    const_instance: ConstInstantiationRequest,
    callable_binding: CallableBindingInstantiationRequest,
};
```

`RootRequest` is for source/app/test/platform entrypoints that can become
runtime procedures. `ConstInstantiationRequest` and
`CallableBindingInstantiationRequest` are for concrete semantic instances
created during checking finalization or imported template consumption. A
compile-time semantic instance must not be disguised as a runtime root.
`RootRequestTable` is the artifact-local root selection result. Root selection
consumes this table. It does not scan provides, display names, last lowered
procedures, previous tool invocations, or tool mode to infer extra roots.

`LoweringRequestSet` is the public request envelope: runtime roots and
compile-time evaluation requests travel side by side but remain different
type states. A compile-time request may prepare or reuse MIR/IR/LIR work for a
concrete semantic instance; it does not make that instance a final binary root.
`LoweringPurpose` records whether the request is for compile-time evaluation,
runtime codegen, tests, REPL, glue, readonly data materialization, platform
relation publication, or a development tool. The purpose selects already
published roots and relation rows; it is not permission to infer missing roots.

`CompileTimeEvaluationRequest.local_root` is the only public request variant
that fills `CompileTimeRootTable.payload` for an artifact-local root. Requests
for concrete const/callable instances use their own records and keys. A
`local_root` request can evaluate a local compile-time root selected by
checking finalization or by the test runner; imported concrete instances are
requested through `ConstInstantiationRequest` or
`CallableBindingInstantiationRequest`.

Const and callable instances are keyed by their own request records:
`ConstInstantiationKey`, `CallableBindingInstantiationKey`, and their concrete
payload refs. Those keys own cache identity, dependency summaries, prepared
lowering reuse, and final publication. `CompileTimeRootId` is an artifact-local
planning id used while ordering roots; it is not the public identity of a
concrete semantic instance.

Public generic exports do not force every possible runtime root. A concrete
consumer creates a concrete request with a concrete source type payload. The
requesting artifact owns the resulting concrete const or callable instance.
Public generic exports remain export-table templates until a concrete consumer
requests a `RootProcedureRequest` or semantic instantiation with an exact
source function payload.

Generic export publication and concrete runtime root lowering are separate.
Publishing a generic exported procedure makes its checked template and template
closure available to importers. It does not lower a body for every possible
type. A concrete runtime root is created only when an executable, test, REPL,
glue request, platform relation, readonly data dependency, or imported
consumer provides a concrete source function type.

Imported and platform callable-eval entry template selection carries closure
authorization explicitly:

```zig
const RootProcedureBinding = union(enum) {
    local: RootTemplateSelection,
    imported: RootTemplateSelection,
    platform_relation: struct {
        template: ProcedureTemplateRef,
        relation_closure: PlatformRelationClosureRef,
    },
};
```

The selected template and its imported or relation closure travel together
through root reservation, mono specialization, callable binding instantiation,
and compile-time summary. A root request id or template id alone is not enough
to authorize private body, type, const, callable-result, or promotion-plan
payloads.

### Hosted, Platform, And Interface Tables

Hosted procedure tables and platform-required binding tables are checked
artifact data, not late compile/backend data. Hosted procedure ids are stable in
the executable relation group and use a global hosted dispatch order across the
platform artifacts that participate in one executable. Module-local hosted
indices are not enough at the executable boundary.

```zig
const HostedProcTable = struct {
    entries: Span(HostedProcEntry),
};

const HostedProcEntry = struct {
    id: HostedProcedureRef,
    order: HostedOrderKey,
    source_fn_ty: ConcreteSourceTypeRef,
    host_symbol: ExternalSymbolNameId,
    representation_abi: ProcRepresentationAbi,
    boundary_rc: CallBoundaryRcTemplate,
};

const HostedOrderKey = struct {
    relation_group: ExecutableRelationGroupId,
    order: u32,
};

const GlobalHostedDispatchCatalog = struct {
    entries: Span(HostedProcedureRef),
};

const CallBoundaryRcTemplate = struct {
    args: Span(CallBoundaryArgRc),
    result: CallBoundaryResultRc,
};
```

The global hosted dispatch catalog is built from the hosted tables in every
artifact participating in one executable relation group. Hosted order is stable
before executable MIR and backend lowering. Backends consume the dispatch
catalog and the call-boundary RC template; they do not infer host-call RC
behavior from ABI names, host symbols, or platform source files.

Platform-required declarations publish:

- declaration identity
- requested source type
- checked source type payload in the platform artifact
- relation-owned imported template closure
- selected app binding or missing-binding diagnostic recorded before artifact
  publication
- call-boundary RC transfer metadata

Interface capability publication records what representation information an
importer is allowed to use. Opaque and imported nominal capabilities are explicit
records. If a later stage needs to know whether it may inspect a nominal backing,
that answer comes from the capability record, not from peeking into a private
artifact body.

### Interface Capability Records

Interface capabilities are sealed checked-artifact data:

```zig
const InterfaceCapabilityTable = struct {
    boxed_payload_templates: Span(BoxedPayloadTemplateCapability),
    opaque_atomic_proofs: Span(OpaqueAtomicProof),
    hosted_representation_capabilities: Span(HostedRepresentationCapability),
    platform_representation_capabilities: Span(PlatformRepresentationCapability),
    exported_nominal_representations: Span(ExportedNominalRepresentation),
};

const BoxedPayloadTemplateCapability = struct {
    nominal: NominalTypeKey,
    payload_template: CheckedTypePayloadRef,
    permitted_use: BoxedPayloadUse,
};

const OpaqueAtomicProof = struct {
    nominal: NominalTypeKey,
    proof_owner: ModuleIdentity,
    backing_access: OpaqueBackingAccess,
};

const BoxPayloadCapabilityTemplate = struct {
    nominal: NominalTypeKey,
    params: Span(CheckedTypeParamId),
    backing: NominalBackingRepresentationTemplate,
    capability_key: BoxPayloadCapabilityKey,
};

const NominalPayloadRepresentation = union(enum) {
    transparent_backing: struct {
        nominal: NominalTypeKey,
        backing_plan: BoxPayloadPlanId,
    },
    imported_capability: struct {
        capability_key: BoxPayloadCapabilityKey,
        instantiated_args: Span(CanonicalTypeKey),
        backing_plan: BoxPayloadPlanId,
    },
    opaque_atomic: struct {
        nominal: NominalTypeKey,
        proof: NoReachableCallableSlotsProof,
    },
    hosted_abi: HostedRepresentationCapabilityKey,
    recursive_ref: RepresentationRecursionBinder,
};

const NoReachableCallableSlotsProof = union(enum) {
    closed_backing_no_callable_paths: ClosedBackingProofKey,
    instantiated_args_no_callable_paths: struct {
        nominal: NominalTypeKey,
        instantiated_args: Span(CanonicalTypeKey),
        proof_terms: Span(NoCallableProofTerm),
    },
};
```

The exact field names may differ, but the table records:

- boxed payload templates that importers may instantiate inside explicit
  `Box(T)` boundaries
- proofs that an opaque backing can be treated as atomic for interface use
- hosted and platform representation capabilities needed by relation artifacts
- exported nominal representations that are visible to importers

The module that defines a transparent nominal owns the
`BoxPayloadCapabilityTemplate` values for that nominal's exported boxed-payload
representation. Inside the defining module, the checked nominal declaration can
publish `NominalPayloadRepresentation.transparent_backing` directly. Outside
the defining module, traversal through a nominal inside erased `Box(T)` payload
uses `NominalPayloadRepresentation.imported_capability`, instantiated with the
exact specialization-local fully resolved type arguments and boxed-payload
representation mode being lowered. The instantiated capability becomes an
ordinary node in the importing artifact's `BoxPayloadRepresentationPlan`.

Opaque nominals are atomic outside their defining module only when the
interface publishes an explicit `opaque_atomic` capability with an
instantiation-sensitive `NoReachableCallableSlotsProof`. A proof is valid only
for the exact nominal identity, exact fully resolved type arguments, and exact
boxed-payload representation mode it names. A closed-backing proof applies only
to the closed backing key it names; an instantiated-args proof carries proof
terms for the concrete arguments. Hosted and platform representation
capabilities similarly enter boxed payload traversal only through explicit
capability rows such as `hosted_abi`; post-check lowering must not inspect
private hosted/platform representations to recover them.

Missing capability data is a checking-finalization error. If an imported
opaque, hosted, platform, or cross-module transparent nominal appears inside an
erased `Box(T)` payload and the importer lacks the required boxed-payload
template or opaque proof, checking finalization reports the relation or
interface error before the artifact is published. Post-check erasure must not
inspect private nominal backings to compensate for missing capability data.
If post-check lowering reaches an imported nominal boxed-payload traversal
without a matching capability/proof, the checked artifact is invalid: debug
builds assert at the boundary and release builds use `unreachable`.

Nominal declarations are published as checked artifact records:

```zig
const CheckedNominalDeclaration = struct {
    id: NominalTypeKey,
    module: ModuleIdentity,
    name: ArtifactLocalNameId,
    params: Span(CheckedTypeParamId),
    backing: CheckedTypePayloadRef,
    transparency: NominalTransparency,
    builtin_identity: ?BuiltinNominalIdentity,
};

const FinalizedNominalDeclarations = struct {
    local: Span(CheckedNominalDeclaration),
    imported_templates: Span(ImportedNominalDeclarationTemplate),
};
```

Checking finalization resolves nominal placeholders before artifact
publication. Imported nominal declaration templates instantiate through the
imported closure that authorizes them. Builtin nominals use canonical identity,
such as `Builtin.Try`, rather than display-name matching. Later stages consume
the published declaration or instantiated declaration template; they do not
select a nominal backing from an occurrence-local type.

### Platform/App Relation Typing

Platform/app relation keys are part of checking context identity:

```zig
const PlatformRequiredDeclarationTable = struct {
    declarations: Span(PlatformRequiredDeclaration),
    by_requirement: Map(PlatformRequirementId, PlatformRequiredDeclarationId),
};

const PlatformRequiredDeclaration = struct {
    requirement: PlatformRequirementId,
    declared_source_ty: ConcreteSourceTypeRef,
    kind: enum { const_value, procedure_value },
};

const PlatformRequiredBindingTable = struct {
    relation: PlatformAppRelationKey,
    bindings: Span(PlatformRequiredBinding),
};

const PlatformRequiredBinding = struct {
    declaration: PlatformRequiredDeclarationId,
    app_binding: AppBindingRef,
    requested_source_ty: ConcreteSourceTypeRef,
    requested_payload_copy: CheckedTypePayloadRef,
    closure: ImportedTemplateClosureView,
};

const PlatformRequirementRelationTable = struct {
    relation: PlatformAppRelationKey,
    rows: Span(PlatformRelationRow),
    hash: Hash,
};

const PlatformRequirementContextKey = struct {
    platform_artifact_key: CheckedModuleArtifactKey,
    relation_hash: Hash,
};

const PlatformAppRelationKey = struct {
    platform_requirement_context: PlatformRequirementContextKey,
    app_artifact_key: CheckedModuleArtifactKey,
};
```

The exact representation may differ, but the relation key must identify both
the platform requirement context and the selected app artifact. Relation
checking compares declared source types with requested source types using
checked type payloads from the owning artifacts.

`declared_source_ty` is the type written by the platform requirement.
`requested_source_ty` is the executable-facing type after app-specific
substitution, transparent expansion, row-tail merging, and relation closure.
Both remain published. Glue and post-check lowering consume the requested type;
diagnostics and docs can still report the declared type. Relation keys hash the
declared requirement ids, requested payload copies, selected app bindings,
relation rows, and substitution rows deterministically. They do not hash raw
source names or late-lowered layouts.

Transparent aliases and transparent nominal backings are expanded only where
the checked relation rules permit expansion. App aliases inside `Box(T)` and
function boundaries are unwrapped when the relation requires the platform-facing
type. Open record and tag rows at a `requires` boundary remain open on the app
side; the platform author is responsible for handling them. For platform
required procedures, return rows are directional: the app result may provide a
compatible superset only where the platform relation explicitly permits it.
Record and tag row-tail merging is explicit relation work. The relation table
records which tails remain open, which tails are closed by the app binding, and
which fields/tags become part of the requested payload. Transparent alias and
transparent nominal merging applies recursively through `Box(T)`, records,
tags, lists, function arguments, and function returns according to the same
relation rules. No builtin transparent nominal receives a name-based shortcut.

Transparent nominal compatibility at platform boundaries is ordinary relation
typing, not a builtin exception. For example:

```roc
ResultAlias(ok, err) : [Ok(ok), Err(err)]

app_provides : I64 -> ResultAlias(Str, Error)
```

can satisfy a platform requirement whose source type is the transparent backing
`I64 -> [Ok(Str), Err(Error)]` when the relation permits transparent expansion.
The same rule applies to builtin transparent nominals such as `Try`; there is no
special case for builtin names.

### Platform Relation Use Records

Platform-required uses are authorized by relation-owned closure records:

```zig
const PlatformRequiredProcedureUse = struct {
    relation: PlatformAppRelationKey,
    requirement: PlatformRequirementId,
    procedure_use: ProcedureUseTemplate,
    requested_fn_ty: ConcreteSourceTypeRef,
    app_binding: AppBindingRef,
    requested_payload_copy: CheckedTypePayloadRef,
    relation_template_closure: ImportedTemplateClosureView,
};

const PlatformRequiredConstUse = struct {
    relation: PlatformAppRelationKey,
    requirement: PlatformRequirementId,
    const_use: ConstUseTemplate,
    requested_value_ty: ConcreteSourceTypeRef,
    app_binding: AppBindingRef,
    requested_payload_copy: CheckedTypePayloadRef,
    relation_template_closure: ImportedTemplateClosureView,
};

const PlatformRequiredValueUse = union(enum) {
    const_value: PlatformRequiredConstUse,
    procedure_value: PlatformRequiredProcedureUse,
};
```

The relation artifact owns the authorization closure. Mono, compile-time
finalization, readonly export materialization, and glue may instantiate only
the templates named by the relation-owned closure. Each value use carries the
nested const/procedure use template, the requested payload copy, the selected
app binding, and the closure for that selected binding. A relation-owned
closure authorizes exactly the selected app binding's private templates; it is
not permission to read every private template in the app artifact.

Platform relation tables contain one row per checked requirement/binding pair:

```zig
const PlatformRelationRow = struct {
    requirement: PlatformRequirementId,
    app_binding: ?AppBindingRef,
    value_use: PlatformRequiredValueUse,
    requested_source_ty: ConcreteSourceTypeRef,
    app_source_ty: ConcreteSourceTypeRef,
    requested_payload_copy: CheckedTypePayloadRef,
};

const AppSpecificPlatformRootArtifact = struct {
    relation: PlatformAppRelationKey,
    rows: Span(PlatformRelationRow),
    platform_roots: Span<RootRequestId>,
};

const PlatformForClauseSubstitutionTable = struct {
    relation: PlatformAppRelationKey,
    rows: Span(PlatformForClauseSubstitution),
};

const PlatformForClauseSubstitution = struct {
    requirement: PlatformRequiredDeclarationId,
    platform_alias_name: TypeNameId,
    platform_rigid_root: CheckedTypeId,
    projected_app_type_root: CheckedTypeId,
};
```

`LoweringModuleView.relation_artifacts` exposes these rows to post-check
lowering. The rows are explicit const/procedure value use records; later stages
do not rescan `provides`, re-run relation typing, or match app declarations to
platform requirements by name.

For-clause substitutions are publication-time data. If a platform declaration
introduces type variables or row variables that are filled by the selected app,
the relation artifact publishes the substitution rows that connect platform
variables to app payloads and requested payload copies. Post-check lowering
uses those rows when instantiating platform-required procedures, platform
required constants, hosted wrappers, and glue schemas. It does not reopen the
relation unifier.

The substitution row is built from explicit platform requirement data. A
`[Alias : rigid]` for-clause records the platform alias spelling, the rigid
checked root that appears in the platform requirement annotation and platform
code, and the selected app type declaration projected into the executable
platform artifact's checked type store. Publication applies the substitution
before mono or runtime dependency-summary lowering and before publishing root
requests, procedure templates, checked body expression/pattern type payloads,
resolved refs, hosted and platform interface capabilities, compile-time roots,
entry-wrapper payloads, and glue schemas. `LoweringModuleView.relation_artifacts`
is the only relation source for finalizers and lowering; later stages do not
recover relation rows by scanning imports, platform declarations, provides,
display names, or app source.

Platform-required constants are concretized before runtime dependency-summary
lowering of the executable platform artifact. This is mandatory when a
required constant contains callable fields: the platform runtime root summary
must see the exact const instances, callable binding instances, and procedure
callable refs required by the selected app binding. Checking finalization
iterates `PlatformRequiredBindingTable`, creates the exact const/callable
instance keys for required constants, reserves and fills those concrete
instances, and ensures their dependencies before platform runtime summaries
run. `CompileTimeFinalizer`, `LoweringModuleView`, mono lowering,
compile-time summaries, runtime summaries, and single-root compile-time eval
all receive the same explicit `relation_artifacts` slice. They must not derive
relation artifacts by scanning imports, platform declarations, provides,
display names, or app source.

Platform co-finalization affects cache identity. A platform artifact checked
without a selected app has a different checking context from a platform
artifact checked as part of an executable platform/app group. The executable
platform artifact identity includes the platform requirement context, the app
artifact key, and the resolved relation rows. A cache hit is valid only for the
exact context. Post-hit identity patching is forbidden.

### Glue Input Catalog And Value Materialization

Glue receives a semantic input catalog built from checked artifacts and final
runtime schemas. It does not read `ModuleEnv`, checker stores, raw CIR, parser
ids, or backend object data.
Every catalog row is derived from checked artifact payloads, relation artifacts,
imported module views, lowering roots, and committed runtime schemas. Glue does
not reopen type checking or inspect source declarations.

```zig
const GlueInputCatalog = struct {
    modules: []GlueModuleInfo,
    type_table: []GlueTypeRepr,
    functions: []GlueFunctionInfo,
    hosted_functions: []GlueHostedFunctionInfo,
    entrypoints: []GlueEntrypointInfo,
    provides_entries: []GlueProvidesEntry,
    runtime_schemas: RuntimeValueSchemaStore,
};

const GlueModuleInfo = struct {
    artifact: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    exports: Span(GlueProvidesEntryId),
    imports: Span(ImportedModuleViewId),
};

const GlueFunctionInfo = struct {
    name: ExportedSymbolName,
    source_fn_ty: ConcreteSourceTypeRef,
    root: LoweredRootId,
    args: Span(RuntimeValueSchemaId),
    ret: RuntimeValueSchemaId,
};

const GlueHostedFunctionInfo = struct {
    hosted: HostedProcedureRef,
    source_fn_ty: ConcreteSourceTypeRef,
    abi: HostedAbiKey,
    args: Span(RuntimeValueSchemaId),
    ret: RuntimeValueSchemaId,
};

const GlueEntrypointInfo = struct {
    root: LoweredRootId,
    symbol: ExportedSymbolName,
    args: Span(RuntimeValueSchemaId),
    ret: RuntimeValueSchemaId,
};

const GlueProvidesEntry = union(enum) {
    procedure: GlueFunctionInfoId,
    readonly_constant: ReadonlyConstantSchemaId,
};

const RuntimeValueSchemaStore = struct {
    schemas: Span(RuntimeValueSchema),
    records: Span(RuntimeRecordSchema),
    tag_unions: Span(RuntimeTagUnionSchema),
    layouts: Span(LirLayoutId),
    display_names: Span(NormalizedDisplayName),
};

const RuntimeValueSchema = union(enum) {
    scalar: ScalarSchema,
    record: RuntimeRecordSchemaId,
    tuple: TupleSchema,
    tag_union: RuntimeTagUnionSchemaId,
    list: ListSchema,
    box: BoxSchema,
    boxed_erased_callable: BoxedErasedCallableSchema,
    readonly_constant: ReadonlyConstantSchema,
};

const RuntimeRecordSchema = struct {
    logical_fields: Span(RuntimeRecordFieldSchema),
    layout: LirLayoutId,
    display_name: NormalizedDisplayName,
};

const RuntimeRecordFieldSchema = struct {
    name: CanonicalFieldName,
    logical_index: FinalizedRecordFieldId,
    offset: u32,
    schema: RuntimeValueSchemaId,
};

const RuntimeTagUnionSchema = struct {
    logical_tags: Span(RuntimeTagSchema),
    layout: LirLayoutId,
    display_name: NormalizedDisplayName,
};

const RuntimeTagSchema = struct {
    name: CanonicalTagName,
    logical_index: FinalizedTagId,
    discriminant: RuntimeDiscriminant,
    payloads: Span(RuntimeTagPayloadSchema),
};

const RuntimeTagPayloadSchema = struct {
    payload_index: u32,
    offset: u32,
    schema: RuntimeValueSchemaId,
};
```

`GlueTypeRepr` is derived from artifact-owned checked type payloads and
interface capabilities. Runtime schemas are derived from row-finalized and
layout-committed data. Named record fields, tag names, payload ids, offsets, and
boxed-erased-callable helper types are explicit catalog entries. Glue writers
materialize values by schema id. They must not recover field order, tag order,
boxed callable ABI, or readonly export shape from display strings or generated
Zig type names.

`RuntimeValueSchemaStore` is first produced from lambda-solved nominal/type
nodes while nominal names and row-finalized logical order are both still
available. Executable MIR may extend the store with compiler-generated runtime
schemas, such as entry wrappers, erased callable adapters, readonly data
records, and hidden capture records, before IR/LIR discard high-level names.
The store deliberately distinguishes checked declaration order from finalized
runtime logical order. A field that appeared first in a source annotation may
have a different runtime logical index after row finalization and layout
commitment.

Glue materializes ordinary Roc values through schema-driven writers:

```zig
const GlueRocValueWriter = struct {
    schemas: *const GlueSchemaStore,

    fn writeRecord(
        self: *GlueRocValueWriter,
        schema: GlueRecordSchemaId,
        value: GlueRuntimeValue,
        out: *RuntimeBytes,
    ) void;

    fn writeList(
        self: *GlueRocValueWriter,
        elem_schema: GlueSchemaId,
        rows: []const GlueCatalogRow,
        out: *RuntimeBytes,
    ) void;
};
```

The exact API may differ, but glue writes records, tuples, tags, lists, boxes,
boxed erased callables, and provided constants only from published schemas.
Schemas are not raw checked declarations. They are runtime value schemas whose
field order, tag order, payload order, pointer shape, boxed callable ABI, and
readonly export layout have already been selected by the compiler pipeline.

Glue input writing is layout-directed. For a host-provided input value, glue
selects the exact runtime schema, writes bytes according to that schema's LIR
layout, copies small strings into Roc-compatible string storage when required,
and builds lists, boxes, and tag payloads through the schema's allocation and
static data rules. Output extraction uses the exact LIR return layout published
for the entrypoint; it must not reinterpret a result by display name. Display
names are normalized once in the schema store so generated C, Zig, Rust, and
docs use stable names without participating in semantic identity.

Record writers use committed field offsets from the schema. List writers
allocate by the committed element size and alignment, then write each element
through the element schema. Tag writers use the committed discriminant and
payload layout for the selected tag. Boxed erased callable writers use the
shared erased callable payload schema and 16-byte capture alignment. Output
readers use the exact LIR return layout, including small-string representation,
list header shape, tag payload offsets, and readonly constant pointer shape.
Field-offset lookup is by row-finalized logical field index, not by source
annotation position or generated struct field order. Tag payload lookup is by
committed tag index and payload index. Glue output extraction copies immediate
small strings into host-owned storage before returning them to generated host
code. Display names are normalized once while publishing the schema store; they
are for generated API readability and never participate in semantic identity.

### Runtime Image Shape

Runtime-image publication creates a target-specific viewable image after ARC
insertion:

```zig
const LirRuntimeImageHeader = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    target_usize: u8,
    byte_order: RuntimeByteOrder,
    root_procs: ArrayRef(LirProcSpecId),
    platform_entrypoints: ArrayRef(PlatformEntrypointImage),
    lir_store: LirStoreImage,
    layouts: LayoutStoreImage,
    literal_pool: ProgramLiteralPoolImage,
    hosted_table: HostedProcTableImage,
};

const ArrayRef = extern struct {
    offset: u64,
    len: u32,
    capacity: u32,
};

const LirRuntimeImageView = struct {
    header: LirRuntimeImageHeader,
    root_proc_ids: RuntimeSlice(LirProcSpecId),
    platform_entrypoints: RuntimeSlice(PlatformEntrypointImage),
    store: LirStoreView,
    layouts: CommittedLayoutStoreView,
    literal_pool: ProgramLiteralPoolView,
    hosted_table: HostedProcTableView,
};
```

The field names may differ, but the image is offset-based and shareable. Child
processes map the image and construct views over it. They do not deserialize
compiler semantic structures, allocate checked artifacts, or receive CIR. Header
validation checks magic, format version, target pointer width, byte order, slice
bounds, alignment, and root proc ids before execution.

Runtime-image validation treats the published bytes as an object graph of
offset-addressed slices. Validation checks header magic, version, target id,
compiler artifact hash when present, target pointer width, byte order, slice
bounds, slice alignment, root proc ids, hosted bindings, platform root mappings,
literal-pool ranges, layout indexes, procedure indexes, statement successors,
malformed RC data, and static data relocations before execution. Views are
zero-copy: a valid child-side view is a set of typed slices into the mapped
bytes, not a deserialized copy.

The parent owns runtime-image construction. The child owns only the mapped arena
view and execution state required by the interpreter/backend. Child-side views
are slices over offsets in the image; they are not deserialized copies. A child
view may validate image bounds, alignment, version, and root ids, but it must
not allocate checked-artifact stores or reconstruct MIR/IR/LIR compiler-owned
semantic data.

Runtime image construction has two local views:

```zig
const RuntimeImageArena = struct {
    bytes: []align(16) u8,
    owner: RuntimeImageOwner,
};

const LocalRuntimeImageView = struct {
    arena: RuntimeImageArena,
    header: *const LirRuntimeImageHeader,
    image_slices: RuntimeImageSliceTable,
    literal_pool: ProgramLiteralPoolView,
    root_proc_ids: Span(LirProcSpecId),
};

const InterpreterShimInput = struct {
    image: LocalRuntimeImageView,
    root_proc: LirProcSpecId,
    args: RuntimeArgView,
};
```

The parent-side builder owns a mutable arena until publication. Publication
freezes the image into offset-addressed slices: procedure image, statement
image, local image, layout image, literal pool image, static data references,
root procedure ids, and optional debug tables. The child-side local view maps
the same bytes and builds typed slice views from offsets. The interpreter shim
receives only the local runtime image view, selected root proc id, arguments,
and runtime operations. It does not receive checked artifacts, CIR, MIR builder
stores, or source type stores.
It also does not receive `ModuleEnv`, checked artifacts, MIR, IR, canonicalize
stores, or checker-local type data.

Runtime images use the existing shared-memory allocator infrastructure:

```zig
const RuntimeImagePublication = union(enum) {
    local_arena: LocalRuntimeImageView,
    file_backed_shared_memory: FileBackedRuntimeImage,
    freestanding_playground: FreestandingRuntimeImage,
};

const FileBackedRuntimeImage = struct {
    allocator: SharedMemoryAllocator,
    path: RuntimeImagePath,
    view: LocalRuntimeImageView,
};

const FreestandingRuntimeImage = struct {
    artifact_hash: CompilerArtifactHash,
    image: LocalRuntimeImageView,
};
```

Local execution may use an in-process arena. Multi-process execution uses a
file-backed arena from `SharedMemoryAllocator`. The freestanding playground
uses an embedded image only when the compiler artifact hash in the image header
matches the compiler/runtime that will execute it. A mismatch is an
infrastructure error, not permission to deserialize checked artifacts.

The runtime image includes hosted table images and platform root mappings when
the executable relation group needs them. A hosted table image maps stable
hosted dispatch indexes to host-call descriptors. A platform root mapping maps
published root requests to runtime image root procedure ids. The interpreter
owns only its execution arena and transient value storage; it does not own the
published image bytes.

Interpreter runtime storage is separate from the published image arena. It
contains procedure frames, frame-local slots, join-point maps, temporary
argument arrays, temporary layout arrays, materialized scalar and aggregate
values, and failed-call stack snapshots for diagnostics. Interpreter runtime
storage must not come directly from the caller allocator while interpreter
value storage is active, because compile-time finalization and runtime-image
execution need deterministic lifetime boundaries. Result reification copies
result bytes into compiler-owned compile-time value structures before the
interpreter arena can be discarded.

### Public Error Shape

Post-check lowering APIs may fail for infrastructure reasons such as allocation,
file IO, object emission, process execution, or runtime-image publication. They
do not fail with user-facing semantic errors. Missing method targets, missing
roots, missing checked type payloads, missing platform data, missing const
instances, and missing layout requests are compiler bugs.

Public result types reflect this split. Error names that imply semantic recovery
after checking are not part of post-check API signatures.

Forbidden post-check semantic error names include:

```text
NoRootProc
NoRootDefinition
MissingRoot
MethodNotFound
DispatchOwnerNotFound
UnsupportedSourceType
UnsupportedLayout
SchemaLayoutMismatch
AbstractSchemaType
MissingCompileTimeValue
MissingInterfaceCapability
MissingHostedProc
MissingPlatformRequiredBinding
```

Those conditions are either checking-finalization diagnostics before artifact
publication or compiler invariant violations after publication. They are not
public lowering API errors and must not be encoded as recoverable post-check
results.

### Public Lowering API And Compile-Time Metadata

The public post-check lowering API has one shape:

```zig
pub const LoweringError = Allocator.Error || IOError || ObjectEmissionError ||
    RuntimeImagePublicationError || ProcessExecutionError;

pub fn lowerArtifactsToLir(
    allocator: Allocator,
    root_artifact: CheckedModuleArtifact,
    imports: ImportedModuleViewSet,
    lowering_view: LoweringModuleView,
    roots: RootRequestSet,
    relations: RelationArtifactSet,
    target: TargetConfig,
    metadata_sink: ?*CompileTimeLoweringMetadata,
) LoweringError!LoweredProgram;

pub const LoweredProgram = struct {
    mono: ?MonoProgramDebugRef,
    row_finalized: ?RowFinalizedDebugRef,
    lifted: ?LiftedProgramDebugRef,
    lambda_solved: ?LambdaSolvedDebugRef,
    executable: ?ExecutableProgramDebugRef,
    ir: IrProgram,
    lir: LirProgram,
    arc_inserted_lir: LirProgram,
    runtime_image: ?LirRuntimeImage,
    callable_set_descriptors: []const RuntimeCallableSetDescriptor,
};

pub const RuntimeCallableSetDescriptor = struct {
    key: CanonicalCallableSetKey,
    members: []const RuntimeCallableSetMember,
};

pub const RuntimeCallableSetMember = struct {
    member: CallableSetMemberId,
    proc_value: ProcedureCallableRef,
    source_proc: MirProcedureRef,
    published_proc_value: ?ProcedureCallableRef,
    published_source_proc: ?MirProcedureRef,
    capture_slots: []const CallableSetCaptureSlot,
    capture_shape_key: CaptureShapeKey,
};

pub const CompileTimeLoweringMetadata = struct {
    prepared_stage_outputs: PreparedStageOutputStore,
    dependency_summaries: CompileTimeDependencySummaryStore,
    materialization_requests: ConstMaterializationRequestStore,
    erased_callable_code_map: LoweredErasedCallableCodeMap,
};
```

The exact exported names may differ, but the error type is resource-only.
Semantic errors, missing checked data, missing roots, missing relation rows,
missing static-dispatch plans, missing compile-time values, and missing
capabilities are not public `lowerArtifactsToLir` errors. They are either
reported during checking finalization or are compiler bugs after publication.

Public callers pass checked artifacts, import/lowering views, explicit roots,
relation artifacts, and target config. Compile-time lowering inside checking
finalization may pass a metadata sink so prepared MIR/IR/LIR work and
dependency metadata can be reused under exact keys.

`LoweredProgram.callable_set_descriptors` is owned runtime lowering output for
the current LIR program and interpreter run. It is not the checked artifact's
persisted callable-set descriptor store. Runtime descriptor members carry the
lowering-run identities used by this exact program, `proc_value` and
`source_proc`, because those identities can name lifted, local, or synthetic
procedures that are valid only in the current lowering run. When an artifact
identity also exists, the member carries optional `published_proc_value` and
`published_source_proc`; those are the only identities that may cross a
persistence boundary.

The clone from lambda-solved lowering into `LoweredProgram` deep-copies every
member field, including optional published identities. The LIR interpreter and
compile-time result decoder consume this runtime descriptor table when reading
finite callable values, erased finite-set adapters, and interpreted callable
captures from the current LIR result. Checked artifacts never persist
lowering-run-only identities, and a checked-artifact persisted descriptor is
not a substitute for the runtime descriptor table while interpreting the
current lowered program.

When `metadata_sink` is present, lowering records prepared work and dependency
summaries so later runtime emission can reuse exact-key outputs. The sink
records prepared work only; it does not mark procedures or constants as final
binary roots.

`CompileTimeLoweringMetadata.erased_callable_code_map` is local lowering
metadata for checking finalization. It is not checked-artifact cache data,
backend policy input, or serialized transport. Lowering builds the map while
executable MIR procedures are assigned LIR procedure ids. Each entry records
the LIR proc id, erased callable code ref, source function type, executable
argument keys, executable return key, capture shape, and deterministic object
symbol. Static data emission and erased callable result decoding consume only
validated entries from this map. They must not recover erased callable code by
source name, descriptor order, callable-set search, runtime pointer value, or
wrapper body inspection.

### Canonical Identity And Procedure Refs

Cross-stage keys use canonical ids owned by the stage boundary that publishes
them. They do not use raw parser identifiers, raw local declaration ids,
display names, qualified-name strings, or raw symbols unless the symbol is
wrapped in an identity type whose scope is explicit.

Procedure references are intentionally split:

```zig
const ProcedureTemplateRef = union(enum) {
    checked: CheckedProcedureTemplateRef,
    lifted: LiftedProcedureTemplateRef,
    synthetic: SyntheticProcedureTemplateRef,
    hosted: HostedProcedureRef,
    platform_required: PlatformRequiredProcedureRef,
    promoted: PromotedProcedureRef,
};

const ProcedureCallableRef = struct {
    template: ProcedureTemplateRef,
    requested_source_fn_ty: ConcreteSourceTypeRef,
};

const ProcedureValueRef = struct {
    callable: ProcedureCallableRef,
    capture_schema: CaptureSchemaRef,
};
```

`ProcedureTemplateRef` names the template. `ProcedureCallableRef` names a
template at one requested source function type. `ProcedureValueRef` names a
value-level callable with capture schema. Executable specialization keys add the
representation boundary and backend-independent executable type information.

#### Checked Procedure Templates

Checked procedure templates are artifact-owned checked procedure bodies plus
the checked metadata needed to specialize them:

```zig
const CheckedProcedureTemplateId = enum(u32) { _ };
const CheckedBodyId = enum(u32) { _ };
const NestedProcSiteId = enum(u32) { _ };

const CheckedProcedureBody = union(enum) {
    checked_body: CheckedBodyId,
    promoted_callable_wrapper: PromotedCallableWrapperId,
    hosted_wrapper: HostedWrapperId,
    intrinsic_wrapper: IntrinsicWrapperId,
    entry_wrapper: EntryWrapperId,
};

const CheckedProcedureTemplate = struct {
    proc_base: ProcBaseKeyRef,
    template_id: CheckedProcedureTemplateId,
    body: CheckedProcedureBody,
    checked_fn_scheme: CanonicalTypeSchemeKey,
    checked_fn_root: CheckedTypeRoot,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    resolved_value_refs: ResolvedValueRefTableRef,
    top_level_value_uses: TopLevelUseSummaryRef,
    nested_proc_sites: NestedProcSiteTableRef,
    target: ProcTarget,
};

const CheckedProcedureTemplateRef = struct {
    proc_base: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
};

const MonoSpecializationKey = struct {
    template: ProcedureTemplateRef,
    requested_mono_fn_ty: CanonicalTypeKey,
};
```

Each template owns artifact-stable checked body ids, checked type roots,
static-dispatch plans, resolved value refs, top-level use summaries, nested
procedure-site rows, and target metadata. The checked variant of
`ProcedureTemplateRef` points at `CheckedProcedureTemplateRef`, and
`MonoSpecializationKey` requests one concrete specialization of a procedure
template. Promoted callable results become compiler-created checked wrapper
templates in this table before publication. A promoted callable procedure value
without both a checked template row and a `PromotedProcedureTable` row is a
compiler bug. After artifact publication, lowering must not read raw CIR
expression ids, raw type variables, parser ids, unchecked syntax pointers,
exporter-local `ModuleEnv` lookups, generated symbols, or procedure bodies
outside the published template payload.

#### Procedure Ordering And Targets

Lifting and promotion publish rewrite maps from source-local identities to
procedure refs. Later stages consume those maps. They must not rediscover
procedure identity from source symbol text or from the shape of a callable
value.

Procedure ordering is separate from semantic procedure identity.

```zig
const ProcOrderKey = struct {
    base_order: u32,
    specialization_order: ?u32,
};
```

`ProcOrderKey` is for deterministic ordering, reproducible output, callable-set
member ordering, erased adapter ordering, recursive capture fixed-point
ordering, generated procedure emission order, and stable printed output.
Semantic equality, specialization deduplication, and executable call target
selection use procedure identity plus canonical type/representation keys, not
`ProcOrderKey`.

#### Canonical Graph Key Builder

Exported semantic keys that can mention recursive type, representation,
callable, or capture graphs use one shared key-builder contract:

```zig
const CanonicalGraphKeyBuilder = struct {
    arena: *BumpAllocator,
    seen_types: Map<CanonicalNodeRef, RecBinderId>,
    seen_reps: Map<RepGroupId, RecBinderId>,
    out: ArrayList(u8),
    next_binder: u32,
};

const CanonicalNodeRef = union(enum) {
    lambda_solved_type: TypeId,
    executable_type: ExecTypeId,
    representation_group: RepGroupId,
    proc_base: ProcBaseKeyRef,
    capture_shape: CaptureShapeKeyRef,
};
```

The same recursion binder and backref rules are used by
`MonoSpecializationKey`, `ExecutableSpecializationKey`,
`CanonicalCallableSetKey`, `CaptureShapeKey`, `ErasedCallSigKey`,
`ErasedAdapterKey`, `BoxPayloadCapabilityKey`, boxed payload representation
plan keys, and executable layout-publication keys. Procedure refs are encoded
as `ProcBaseKeyRef`, not raw symbols. Capture components are encoded in
`CaptureSlot.index` order. Row components use finalized row ids. Structural
children are visited in the canonical order defined by their shape. Recursive
callable and capture graphs serialize with stable binders and backrefs; they
must not recursively inline members or captures until traversal bottoms out.

Procedure definitions carry explicit target metadata:

```zig
const ProcTarget = union(enum) {
    user_proc: UserProcTarget,
    hosted_proc: HostedProcTarget,
    intrinsic_wrapper: IntrinsicWrapperTarget,
};

const HostedProcTarget = struct {
    host_symbol: ExternalSymbolNameId,
    dispatch_index: u32,
    representation_abi: ProcRepresentationAbi,
    call_boundary_rc_template: CallBoundaryRcTemplate,
};
```

The exact target variants may differ, but the selected procedure definition
owns whether it is user code, hosted code, or an intrinsic wrapper. Later stages
must not rediscover that role from method names, host symbol text, generated
symbols, layouts, runtime pointers, or surrounding user code.

Procedure identity has separate source, template, mono, and lifted identities:

```zig
const NestedProcSiteKey = struct {
    owner_source_proc: SourceProcKey,
    path: Span(NestedProcSitePathElement),
};

const SourceProcKey = union(enum) {
    top_level: TopLevelDeclKey,
    nested_site: NestedProcSiteKey,
    promoted: PromotedProcedureKey,
    synthetic: SyntheticProcedureKey,
    hosted: HostedProcedureRef,
    platform_required: PlatformRequiredProcedureRef,
};

const ProcBaseKey = struct {
    source: SourceProcKey,
    module: ModuleIdentity,
    artifact_key: CheckedModuleArtifactKey,
};

const MonoSpecializationKey = struct {
    base: ProcBaseKey,
    requested_fn_ty: ConcreteSourceTypeRef,
    type_payload_key: ConcreteSourceTypePayloadKey,
};

const MonoSpecializedProcRef = struct {
    key: MonoSpecializationKey,
    proc_id: MonoProcId,
};
```

A mono output procedure is not a procedure template key. A template key names
checked source/template material. A mono specialization key names one concrete
specialization request. A lifted procedure ref then names the owner
specialization plus the lifted owner body that contains the final procedure
body after local functions and closures have been split out.

Nested procedure sites are published in a stable table:

```zig
const NestedProcSiteTable = struct {
    sites: Span(NestedProcSiteRow),
};

const NestedProcSiteRow = struct {
    id: NestedProcSiteId,
    owner: SourceProcKey,
    path: Span(NestedProcSitePathElement),
    kind: enum { local_function, closure, desugared_closure },
    checked_callable_ty: CheckedTypeId,
};
```

The nested site id is reserved during checked artifact publication. Later stages
use the reserved id. They must not derive nested procedure identity from local
symbol text, expression ids, allocation order, or closure body shape.

Synthetic origins are payload-bearing:

```zig
const SyntheticOrigin = union(enum) {
    erased_adapter: ErasedAdapterOrigin,
    bridge: BridgeOrigin,
    intrinsic_wrapper: IntrinsicWrapperOrigin,
    entry_wrapper: EntryWrapperOrigin,
    promoted_callable: PromotedCallableOrigin,
};
```

Each origin carries the source type, representation boundary, relation/root
identity, and checked or executable payload references required to validate the
generated procedure. A synthetic procedure is still a real procedure with a
stable identity. Its identity is not a generated symbol string.

Executable-only synthetic procedures have their own sealed records:

```zig
const ExecutableSyntheticProcSignaturePlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    params: Span(ExecutableTypePayloadRef),
    ret: ExecutableTypePayloadRef,
    captures: Span(ExecutableTypePayloadRef),
    transforms: Span(PublishedExecutableValueTransformRef),
};

const ExecutableSyntheticProc = struct {
    id: ExecutableSyntheticProcId,
    origin: SyntheticOrigin,
    signature: ExecutableSyntheticProcSignaturePlan,
    body: ExecutableSyntheticProcBody,
};

const LambdaSolvedExecutableSyntheticProcInstance = struct {
    synthetic: ExecutableSyntheticProcId,
    representation_instance: ProcRepresentationInstanceId,
    executable_key: ExecutableSpecializationKey,
};
```

Earlier MIR stages may carry synthetic procedure identity opaquely so calls,
dependencies, and ordering remain explicit, but they do not inspect executable
synthetic bodies. Lambda-solved reserves representation instances for synthetic
procedures just like user procedures. Executable MIR owns the synthetic body and
validates the signature plan before the body can become IR or LIR.

### Mono Specialization Graph

Mono lowering has three type states:

```text
MonoSpecializationGraphBuilder
  -> FinalizedMonoSpecializationGraph
  -> MonoBodyEmitter
```

The builder clone-instantiates checked type graphs, connects params, returns,
binders, patterns, call args, call results, fields, tags, equality operands,
static-dispatch callable slots, and expected-type slots. The finalized graph has
all links resolved, numeric defaults applied, and source type payloads selected.
The body emitter is lookup-only. It must not unify, default, close rows, choose
method owners, or create missing concrete payloads while emitting value MIR.

Mono specialization is driven by explicit requests:

```zig
const MonoSpecializationRequest = struct {
    template: ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
};

const MonoSpecializationReason = union(enum) {
    root: RootRequestId,
    call_proc: CallSiteInfoId,
    proc_value: CallableSetConstructionPlanId,
    static_dispatch_target: StaticDispatchPlanId,
    comptime_dependency_summary: ComptimeSummaryRequestId,
};

const MonoSpecializationQueue = struct {
    requested: Map(MonoSpecializationKey, ReservedMonoProc),
    pending: WorkQueue(MonoSpecializationKey),
};

const ReservedMonoProc = struct {
    proc: MonoSpecializedProcRef,
    local_handle: MonoProcHandle,
    state: enum { reserved, lowering, lowered },
};
```

Every root request, direct call, procedure value, static-dispatch target, and
materialization dependency creates one `MonoSpecializationRequest`. The request
retains the concrete `ConcreteSourceTypeRef` payload for the requested function
type, not only a canonical key. The queue is the only way a target enters mono
body emission; no post-check stage scans source declarations, declaration
names, procedure order, or export tables to find a missing specialization.

Finalization is a separate phase with explicit closure rules:

```zig
const MonoGraphFinalization = struct {
    numeric_defaults: Span<DelayedNumericDefault>,
    equality_only_closures: Span<MonoVarId>,
    unconstrained_closures: Span<MonoVarId>,
    extension_tail_closures: Span<RowOrTagTailClosure>,
    method_constraint_resolutions: Span<MethodRegistryIntersection>,
    callback_return_endpoints: Span<CallbackReturnEndpoint>,
};
```

Delayed numerics default only after every argument, return, binder,
static-dispatch, equality, callback, and expected-type edge has been connected.
Variables that appear only in equality constraints close to `{}` only after the
method registry proves no concrete owner is required. Truly unconstrained flex
variables close to `{}` at finalization. Row and tag tails close only at
extension edges that checking marked as sealed endpoints; an open row variable
elsewhere remains an explicit open variable in the checked source type payload.
Runtime variables constrained by methods resolve through method-registry
intersections, not through name lookup or syntactic receiver shape.
Callback-local return endpoints are connected before any callback body emits
MIR. Body emission begins only after `FinalizedMonoSpecializationGraph` verifies
that no pending graph links, unresolved numeric defaults, unchecked callback
returns, or method constraints remain.

Mono type instantiation has two distinct operations:

1. Constraint-preserving clone-instantiation copies a checked template type into
   the specialization-local concrete source type graph while preserving open
   variable identity. Flex variables, rigid variables, row tails, tag-union
   tails, delayed numeric variables, and static-dispatch constrained variables
   remain explicit variable payloads in the local graph.
2. Final closure/materialization lowers a fully connected specialization
   endpoint to a closed concrete source type. Only then may mono close a truly
   unconstrained flex variable to an explicit zero-field payload or apply a
   delayed numeric default.

The unifier uses clone-instantiation whenever it connects an imported,
target-side, or generic checked template payload to a concrete endpoint. It must
not use final closure/materialization while endpoints are still being connected.

Mono call specialization also distinguishes explicit concrete evidence from
speculative nested-call materialization. When mono instantiates a call, it may
bind parameter type variables from argument expressions only if the argument
already has a concrete type published in the current specialization-local
environment, such as a local parameter, local declaration binder, mutable
version, pattern binder, capture reference, or earlier lowered value. An
argument expression that is itself a call is lowered under the concrete
parameter endpoint selected by the consuming call. Mono must not lower or
instantiate that nested call merely to discover its type.

For example:

```roc
append_one = |acc, x| List.append(acc, x)

clone_via_fold = |xs|
    xs.fold(List.with_capacity(1), append_one)

clone_via_fold([1.I64, 2.I64]).len()
```

`List.with_capacity(1)` tells mono that the fold accumulator is `List(b)`, but
`b` is not known until the callback endpoint connects. Mono preserves `b` while
connecting endpoints, then closes only after receiver, callback, and return
endpoints agree.

Exported mono MIR types are fully monomorphic. They do not contain checker
source vars, generalized variables, rigid vars, unresolved placeholders,
`for_a`, `flex_for_a`, pending checked payloads, or builder-private links.
Nominal identity is preserved. Transparent aliases may resolve methods through
nominal identity, but exported mono values do not depend on checker-local type
variables or alias reconstruction.

Mono finalization publishes instantiated alias and nominal backings whenever a
concrete specialization needs them, even when no runtime value directly appears
for the declaration:

```zig
const InstantiatedNominalBacking = struct {
    nominal: CanonicalNominalId,
    formal_args: Span(CheckedTypeVarId),
    actual_payloads: Span(ConcreteSourceTypeRef),
    backing: ConcreteSourceTypeRef,
    representation: CheckedNominalRepresentationRef,
};
```

Parametric wrappers substitute formal variables through the same
specialization-local graph used for procedure args and returns. Mono starts
from published formal roots for the wrapper declaration; it does not collect
wrapper params from occurrence-local use sites. A nominal value may unify with
its backing only through an explicit wrapper/backing relation published by
checking. The backing can constrain payload variables, but the nominal wrapper
identity remains part of the value when the source type is nominal. For example,
a `Result(ok, err)` transparent alias can expose its backing tags for relation
typing, while a nominal `Try(a)` pattern still lowers through the published
nominal declaration and representation. Nominal pattern lowering uses the
instantiated declaration/backing pair; it must not rebuild a backing from tag
syntax or select a wrapper by display name.

Open record and tag rows are solved by row-equation rules in the
specialization-local graph. Tag-row unification matches explicit tags by
canonical tag identity, rejects duplicate explicit tags, preserves payload order
inside each tag, and creates a fresh shared residual tail when two open rows
leave the same unknown remainder. Record rows use the analogous rule for
canonical field labels and record extension tails. The residual tail is an
explicit graph node shared by both sides of the equation; it is not reconstructed
from missing labels later.

Residual clones preserve open-variable identity and delayed numeric-default
metadata. They do not clone static-dispatch constraint function graphs into new
callable targets. If a residual still contains a constrained static-dispatch
variable, mono must connect it through the original checked method-registry
target or report an invariant failure during graph finalization.

### Type-Only Call Result Queries

Mono sometimes needs the result type of a call before it emits the call. This is
especially important for nested static dispatch, expected-type propagation, and
empty container literals. The type-only query:

1. instantiates the callee fixed-arity function type
2. connects already-known argument slots before reading the result
3. connects the expected result slot if present
4. resolves the callable result type in the finalized graph
5. returns a concrete source type payload for the result

The query does not lower arguments, emit calls, request roots by scanning source,
or mutate already-finalized body emission state.

Only checked semantic data can publish an argument slot for this query. An
expression publishes a closed concrete type when checking has already produced a
closed checked type root for that expression or when mono has already finalized
the value under a concrete consumer endpoint. Literal spelling, empty records,
empty lists, no-payload tags, local syntax shape, display names, and available
method targets are not evidence. If an argument is a nested call, the nested
call is lowered under the endpoint that consumes it; mono does not materialize a
standalone nested result merely to ask what type it would have had in isolation.

Examples:

```roc
outer(inner(x))
empty = []
one = 1
```

`inner(x)` is queried through the argument endpoint of `outer`. `[]` publishes
no element type until its consumer supplies one. `1` publishes only its checked
numeric constraint until final numeric defaulting or an endpoint closes it.

### Block-Local Demand Propagation

Within one mono body, later concrete uses can constrain earlier local bindings.
Mono records block-local demand while building the specialization graph and
propagates it before body emission.

```roc
f = if cond then |x| x + 1 else |x| x + 2
g = f
g(40)
```

The call to `g` supplies demand for `g`, and `g` supplies demand for `f`. Body
emission lowers `f` once at the concrete callable type required by the call. It
does not emit a generic value and then repair the call later.

The demand table is keyed by checked binding identity, not by source name. When
mono sees a resolved local lookup in a concrete expected position, it records
that concrete source type before any declaration for the referenced binder is
emitted. A local call records the concrete callable source type for the callee
from the call's checked source function type after connecting already-known
argument and return slots in the current specialization-local graph.

A declaration first asks the demand table for its binder's concrete source
type. If present, the declaration lowers its body against that exact type. If no
demand exists, the declaration may materialize the checked pattern type only
when that type contains no constrained flex, rigid variable, generalized
variable, or pending payload. Branch and source-match bodies that produce
function-valued results lower against the shared expected result slot from the
join. Record, tuple, tag, list, and `Box(T)` construction propagate a
function-valued field, element, or payload endpoint into the child expression
before the child is lowered.

Demand propagates through ordinary local aliases. In `g = f; g(40)`, the call
records demand for `g`, the alias records the same demand for `f`, and both
declarations lower after that demand is known. Mono must not create a thunk,
choose a direct-call shortcut, scan source syntax, or rebuild callable members
from expression shape as a substitute for this demand flow.

Each static-dispatch expression instantiates its target callable graph with a
fresh dispatch-site instantiator:

```zig
const DispatchSiteInstantiator = struct {
    artifact: CheckedModuleArtifactKey,
    type_view: ArtifactTemplateResolver,
    site: StaticDispatchCallPlanId,
};
```

The instantiator starts from the owning checked artifact and imported type view
for that dispatch site. It does not copy enclosing mono substitutions, reuse a
previously materialized template root, or inherit accidental bindings from
another dispatch expression. The dispatch-site graph is then connected to the
receiver, arguments, expected result, and method-registry target for this
specialization.

### Method Registry And Equality Dispatch

The checked method registry maps `(MethodOwner, MethodNameId)` to procedure
targets with checked callable types. The registry chooses the target for a
resolved owner and method; it does not decide which type controls a particular
call. `StaticDispatchCallPlan.dispatcher_ty` chooses that.

For every procedure-backed target, the registry's checked callable type is the
same checked root as the published procedure template's callable root. The
registry builder must not independently recover the callable type from a raw
definition-node type variable, body type, expression type, or module-env slot.
Debug verification asserts that the registry entry's callable root key matches
the procedure template callable root key.

Hosted, platform, or intrinsic method entries are normalized to explicit
procedure targets with checked callable types before mono consumes the registry.
Mono still emits `call_proc` to a procedure value when a target exists. It must
not special-case a method name as an intrinsic after static-dispatch lookup.

Equality dispatch is categorized during checked artifact publication.
`StaticDispatchCallPlan.result_mode.equality` is used when the original
constraint came from an equality operator, or when checked semantic data proves
the canonical `is_eq` callable type is exactly `item, item -> Bool`. Such a
dispatch can still appear syntactically as an ordinary dispatch call inside a
generic equality implementation. Mono consumes the published equality mode; it
does not infer equality from the method name.

If `result_mode.equality.structural_allowed` is true and method lookup finds no
owner-specific method for a concrete specialization, mono emits `structural_eq`.
If structural equality is not allowed, missing lookup is a compiler bug. `!=`
is represented by `negated = true`, and mono emits equality followed by
`bool_not`.

### Tags And Constructors In Mono

Tag names remain symbolic only until row finalization. Mono must use the
checked expression's specialized type for tag construction. It must not
synthesize a singleton tag source type from local syntax.

For:

```roc
Err("x")
```

mono uses the checked specialized expression type. If the expression type is:

```roc
[Ok(I64), Err(Str)]
```

mono must not invent:

```roc
[Err(Str)]
```

Logical discriminants and payload indexes are computed only by row finalization
from the full mono MIR tag-union type. They are not computed lazily inside
representation solving, executable lowering, IR lowering, or layout lowering.

### Lambda-Solved Callable Plans

Lambda-solved lowering is stack-safe. Deep expression spines created by
desugaring, string concatenation, generated glue, nested low-level calls, or
large aggregate transforms lower through worklists and postorder emission, not
through unbounded recursive descent. This is a stack-safety rule only: the
worklist preserves source evaluation order, explicit value-flow records,
transform boundaries, and low-level operation ordering exactly.

Callable descriptor identity has two layers:

```zig
const ArtifactCallableDescriptorKey = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    member_keys: Span<ArtifactCallableMemberKey>,
    capture_shape: CaptureShapeKey,
};

const LambdaSolvedCallableDescriptor = struct {
    artifact_key: ArtifactCallableDescriptorKey,
    representation_instance: ProcRepresentationInstanceId,
    members: Span<LambdaSolvedCallableMember>,
};
```

Artifact callable descriptors are target-free and contain no session-local
representation ids. Lambda-solved descriptors are session-local and carry exact
`ProcRepresentationInstanceId` values. Executable branch targets come from the
lambda-solved descriptor member selected by the solved session. Executable
lowering never selects a branch target by looking up `source_proc`, source
function type, display name, or descriptor order alone.

Every callable value occurrence that constructs a finite callable-set value has
one `CallableSetConstructionPlan`:

```zig
const CallableSetConstructionPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    selected_member: CallableSetMemberRef,
    capture_values: Span(ValueTransformBoundaryId),
    result: CallableValueEmissionPlan,
};

const CallableValueEmissionPlan = union(enum) {
    finite_callable_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    proc_value_to_erased: ProcValueErasePlan,
    finite_set_to_erased_adapter: ErasedAdapterKey,
};
```

The plan belongs to the construction occurrence, not to the source procedure
globally. The selected member, capture schema, source function type, and
emission plan must agree exactly. If two occurrences choose the same source
procedure but different captures or representation context, they receive
separate construction plans.

`CallableValueEmissionPlanId` values live in the sealed lambda-solved value
metadata store. Every callable value occurrence has exactly one solved emission
plan before executable MIR emission. Executable MIR consumes that plan; it does
not decide finite versus erased representation, invent erased wrappers, or
promote a proc value because of local layout shape. Debug verification rejects
missing plans, plans whose solved representation group does not match the value
occurrence, and erased plans with empty `BoxErasureProvenance`.

Multiple `proc_value` occurrences that select the same `MirProcedureRef` share
one target procedure member inside one solve session. Each occurrence still
keeps its own `CallableSetConstructionPlan`, capture values, consumer-use
transforms, and result transforms. Sharing target members prevents duplicate
executable target reservation while preserving occurrence-local value
semantics.

Representation solve sessions are mutable only while open:

```zig
const RepresentationSolveSession = struct {
    id: RepresentationSolveSessionId,
    root: RepresentationRoot,
    members: GrowableMemberSet,
    state: enum { reserving, solving, sealed },
};

const ProcRepresentationTemplate = struct {
    template: ProcedureTemplateRef,
    type_template_store: CheckedTypeStoreRef,
    representation_template: RepresentationTemplateRef,
    value_template: ValueInfoTemplateRef,
    capture_slot_templates: Span(CaptureSlotTemplate),
};

const ProcedureInstantiationOwner = union(enum) {
    root_request: RootRequestId,
    direct_call: CallSiteInfoId,
    proc_value: CallableSetConstructionPlanId,
    callable_match_member: struct {
        call: CallSiteInfoId,
        member: CallableSetMemberRef,
    },
    erased_adapter_member: ErasedAdapterKey,
    promoted_wrapper: PromotedCallableWrapperRef,
    recursive_group_member: RecursiveGroupMemberId,
};

const ProcPublicValueRoots = struct {
    params: Span<ValueInfoId>,
    ret: ValueInfoId,
    captures: Span<ValueInfoId>,
    function_root: RepRootId,
};

const ProcBoundaryExecutablePayloads = struct {
    artifact: CheckedModuleArtifactKey,
    payloads: ExecutableTypePayloadStoreRef,
    promoted_wrapper: ?PromotedCallableWrapperRef,
};

const ProcRepresentationInstance = struct {
    proc: MirProcedureRef,
    template: ProcRepresentationTemplateRef,
    owner: ProcedureInstantiationOwner,
    executable_specialization_key: ExecutableSpecializationKey,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
    boundary_payloads: ?ProcBoundaryExecutablePayloads,
    boundary_provenance: Span(BoxErasureProvenance),
    state: enum { reserved, building, solving, sealed },
};
```

The mutable builder owns session membership while reservation, body fill, and
solve are still in progress:

```zig
const SolveSessionBuilder = struct {
    session: RepresentationSolveSessionId,
    members: ArrayList(ProcRepresentationInstanceId),
};

const LambdaSolvedBuilderState = struct {
    sessions: ArrayList(SolveSessionBuilder),
    proc_instances: ArrayList(ProcRepresentationInstance),
    value_stores: ArrayList(ValueInfoStore),
};
```

The sealed `RepresentationSolveSession.members` slice is published only after
the builder reaches a fixed point where no reservation rule can add another
member. Until then, every solve or finalization step reads the builder-owned
current member table, or the builder explicitly synchronizes the published
session member view before that step. Synchronization is builder bookkeeping;
it is not a semantic pass and not a downstream repair mechanism.

Sessions reserve recursive members before solving bodies. New members discovered
while solving invalidate the current fixed point and are processed before the
session seals. Published lambda-solved MIR contains only sealed sessions and no
unresolved callable variables.

Generalized procedure templates are never executable inputs. A concrete use
clone-instantiates the checked procedure template into one
`ProcRepresentationInstance`, attaches it to a solve session, allocates its
dense value metadata store, and publishes `ProcPublicValueRoots` before any
cross-procedure value-flow edge can target it. The public parameter, return,
capture, and function roots are the only cross-procedure source of truth for
calls, procedure values, captures, direct-call SCCs, and promoted wrappers.

`ProcBoundaryExecutablePayloads` is present when a procedure boundary is owned
by an adapter, erased promoted wrapper, or promoted finite-adapter member whose
executable parameter/return/capture payloads were already sealed in a checked
artifact. In that case call-boundary transform finalization imports the
artifact-owned executable payload graph into the current
`SessionExecutableTypePayloadStore` and consumes `boundary_provenance` as the
Box-erasure authorization. It must not recover boundary payloads or provenance
from the callee body, source syntax, erased ABI shape, or generated procedure
name.

The fixed-point loop order is part of the contract:

1. reserve or reopen solve sessions
2. assign callable emissions for known construction sites
3. publish value-transform adapter demands
4. append erasure requirements
5. reserve finite erased adapter members
6. materialize executable demands
7. append cross-procedure edges
8. repeat until sessions, adapter demands, and executable demands are stable

A later step must not back-patch an earlier sealed record. Discovery of a new
demand reopens the owning in-progress session or reserves a new session before
publication.

Proc-value emission begins as a builder-only pending plan:

```zig
const PendingProcValueCallablePlan = struct {
    value: ValueInfoId,
    proc: ProcedureCallableRef,
    source_fn_ty: ConcreteSourceTypeRef,
    owner_root: RepRootId,
    captures: Span(ValueInfoId),
};

const ProcValueOwnerSealing = union(enum) {
    finite: CanonicalCallableSetKey,
    erase_proc_value: ProcValueErasePlan,
    erase_finite_set: FiniteSetErasePlan,
};

const ProcedureInstancePurpose = union(enum) {
    materialized_executable_proc: MaterializedProcedureDemand,
    callable_descriptor_member: struct {
        descriptor: LambdaSolvedCallableDescriptorId,
        member: CallableSetMemberId,
    },
};

const MaterializedProcedureDemand = struct {
    reason: MaterializedProcedureDemandReason,
    executable_key: ExecutableSpecializationKey,
};
```

`PendingProcValueCallablePlan` is not published artifact data and is never
executable input. It waits until representation groups, erased-box
requirements, and callable-set membership are known. Function-valued captures
can depend on other callable groups, so the builder must iterate the fixed point
instead of publishing placeholder descriptors. `ProcValueOwnerSealing` chooses
the final finite or erased emission plan for that owner.

Procedure instances have distinct purposes. A descriptor-only instance
publishes public roots, capture roots, and boundary metadata for callable-set
membership, but it does not lower the body. A materialized executable instance
exists only when a root, direct call, finite adapter branch, promoted wrapper,
readonly data dependency, or other explicit `MaterializedProcedureDemand`
requires executable code.

Erased boxed-payload requirements are part of this fixed point and must be
known before strict callable-emission assignment:

```zig
const ProcValueOwnerErasureRequirement = struct {
    target_instance: ProcRepresentationInstanceId,
    public_root: RepRootId,
    executable_key: CanonicalExecValueTypeKey,
    provenance: BoxErasureProvenance,
};

const CallBoundaryErasureRequirement = struct {
    call_site: CallSiteInfoId,
    target_instance: ProcRepresentationInstanceId,
    arg_index: u32,
    caller_arg_root: RepRootId,
    target_exec_arg_ty: CanonicalExecValueTypeKey,
    provenance: BoxErasureProvenance,
};

const ErasedCallBoundaryErasureRequirement = struct {
    call_site: CallSiteInfoId,
    sig_key: ErasedCallSigKey,
    arg_index: u32,
    caller_arg_root: RepRootId,
    abi_arg_exec_key: CanonicalExecValueTypeKey,
    provenance: BoxErasureProvenance,
};
```

When a proc-value owner boundary, direct call boundary, finite callable branch,
or erased callable ABI argument has an executable key that recursively contains
an erased callable payload, lambda-solved appends
`RepresentationRequirement.require_box_erased` for the caller or owner payload
root and solves again. The requirement propagates along explicit representation
edges for aliases, value moves, returns, branch joins, loop phis, mutable
versions, platform wrappers, function args/returns, record fields, tuple
elements, tag payloads, list elements, boxed payloads, and nominal backings.
Deduplication is by payload root and provenance.

Erased `call_value` dispatch must be published early enough for this requirement
step to read the erased ABI argument executable keys. This early publication
records only the solved dispatch kind and erased signature; it does not finalize
`call_raw_arg`, `call_raw_result`, or concrete `ExecutableValueTransformRef`
records. The later value-transform finalizer still owns argument/result
transforms after strict callable emissions are assigned.

The requirement pass consumes explicit proc-value owner reservations, call-site
records, finite callable branch targets, erased ABI records, and already-solved
emission plans. It must not scan source syntax, executable pattern nodes,
branch bodies, tag names, `Box.unbox` syntax, or layouts to rediscover erased
payloads.

For example:

```roc
I64ToI64 : I64 -> I64

boxed_add_one : Box(I64ToI64)
boxed_add_one = Box.box(|value| value + 1)

boxed_add_one_for_host : Box((I64 -> I64))
boxed_add_one_for_host = boxed_add_one
```

The local `Box.box` boundary authorizes erasure for the lambda payload. The
alias `boxed_add_one_for_host` is the same boxed runtime value, so its public
return endpoint must seal as `Box(erased_fn(...))`, with an explicit
already-erased same-ABI retype if the transparent alias changes canonical
source function identity. It must not seal as
`Box(vacant_callable_slot(I64 -> I64))`, and no later stage may repair that
with a vacant-to-erased bridge.

Erased callable representation requires an explicit `BoxBoundaryId`:

```zig
const BoxBoundary = struct {
    direction: BoxErasureDirection,
    input: ExprId,
    box_root: RepRootId,
    payload_root: RepRootId,
    box_ty: ConcreteSourceTypeRef,
    payload_source_ty: ConcreteSourceTypeRef,
    payload_boundary_ty: ConcreteSourceTypeRef,
    payload_plan: BoxPayloadRepresentationPlanId,
};
```

For boxing, `input` is the payload expression, `box_root` is the produced box,
and `payload_root` is the payload being boxed. For unboxing, `input` is the
boxed expression, `box_root` is the consumed box, and `payload_root` is the
unboxed result. `box_ty` must be exactly `Box(payload_boundary_ty)`.
`payload_source_ty` is the source payload type before erased-payload rewriting.
`payload_boundary_ty` is the explicit erased representation of the boxed
payload, and `payload_plan` records the representation requirement for that
payload.

The `BoxBoundaryId` is local to one representation store. Compile-time values
and serialized artifacts store stable erased callable provenance, not local
boundary ids. When a value is already erased, lambda-solved consumes the
published already-erased plan; it does not fabricate a new local boundary.

The lifted-to-lambda-solved import pass allocates a fresh callable variable for
every function type occurrence it imports. This applies recursively inside
parameters, returns, captures, records, tuples, tags, `List(T)`, `Box(T)`,
nominals, function arguments, and function returns. Equal source type ids do not
justify reusing callable variables between unrelated value occurrences.

`ValueFlowGraphBuilder` owns all mutable state while importing one procedure or
materialization root:

```zig
const ValueFlowGraphBuilder = struct {
    current_instance: ProcRepresentationInstanceId,
    solve_session: RepresentationSolveSessionId,
    representation_store: *RepresentationStore,
    dense_values: DenseValueInfoStore,
    lexical_scopes: ScopeStack,
    loop_frames: LoopFrameStack,
    current_return_root: ReturnRootId,
    public_roots: PublicRootTable,
    reserved_values: ReservedValueRecords,
};
```

The builder reserves value records before publishing edges that mention them.
Return value-flow edges are published before return transforms are finalized, so
recursive return representations participate in the same fixed point as params,
captures, joins, and calls. Lexical scopes and loop frames are builder state;
published lambda-solved MIR contains sealed value records and edge tables, not
mutable scope stacks.

#### RepresentationStore

Lambda-solved MIR owns an explicit representation store:

```zig
const RepresentationStore = struct {
    roots: Map(RepRootId, RepVarId),
    vars: Store(RepresentationVar),
    edges: Store(RepresentationEdge),
    requirements: Store(RepresentationRequirement),
    groups: Store(SolvedRepresentationGroup),
};

const RepRootId = union(enum) {
    expr: ExprId,
    binder: BinderId,
    pattern_binder: PatternBinderId,
    proc_param: struct { proc: ProcRepresentationInstanceId, index: u32 },
    proc_return: ProcRepresentationInstanceId,
    capture_slot: struct { proc: ProcRepresentationInstanceId, slot: CaptureSlotIndex },
    call_value_requested_fn: ExprId,
    call_proc_requested_fn: ExprId,
    proc_value_fn: ExprId,
    mutable_var_version: struct { symbol: SymbolId, version: u32 },
    loop_phi: LoopPhiId,
};

const FunctionRepShape = struct {
    args: Span<RepRootId>,
    ret: RepRootId,
    callable: RepRootId,
};

const RepresentationShape = union(enum) {
    unknown,
    primitive: PrimitiveRep,
    record: Span(FieldRepSlot),
    tuple: Span(ElemRepSlot),
    tag_union: Span(TagRepSlot),
    list: RepVarId,
    box: RepVarId,
    nominal: NominalRepSlot,
    function: FunctionRepShape,
    callable: CallableRepShape,
};

const RepresentationEdge = struct {
    from: RepVarId,
    to: RepVarId,
    kind: RepresentationEdgeKind,
};

const RepresentationEdgeKind = union(enum) {
    value_alias,
    value_move,
    function_arg: u32,
    function_return,
    function_callable,
    record_field: RecordFieldId,
    tuple_elem: u32,
    tag_payload: TagPayloadId,
    list_elem,
    box_payload,
    nominal_backing: NominalTypeKey,
    branch_join,
    loop_phi,
    mutable_version,
};

const RepresentationRequirement = union(enum) {
    require_box_erased: BoxErasureRequirement,
    require_shape: RepresentationShape,
};

const BoxErasureRequirement = struct {
    payload_root: RepRootId,
    provenance: BoxErasureProvenance,
};
```

Every expression result, binder, pattern binder, procedure parameter,
procedure return, capture slot, callable requested-function occurrence,
mutable variable version, and loop phi gets its own `RepRootId` before
requirements are solved. Structural children also receive explicit
representation variables through `RepresentationEdgeKind`: record fields,
tuple elements, tag payloads, list elements, boxed payloads, nominal backings,
function arguments, function returns, branch joins, loop phis, and mutable
versions. Box erasure is a representation requirement on a payload root, not an
executable conversion and not layout data.

Function-typed roots have one whole-function shape: `FunctionRepShape`. A
`call_value`, `call_proc`, or `proc_value` creates or references the requested
function root with argument roots, return root, and callable root linked
together. Helper APIs publish arg, return, and callable edges as one operation
so a function cannot have a callable representation without the matching
argument/return representation roots. Executable runtime function values
contain only the solved callable representation; argument and return roots are
boundary metadata used to solve and validate calls, adapters, and erased ABI
records.

Every solved representation group has exactly one `RepresentationShape`.
`unknown` adopts the other shape. Primitives must match exactly. Records merge
by finalized field id from the owning row shape; tuples merge by element index;
tag unions merge by finalized tag and payload ids from the owning tag-union
shape. Lists merge through their element root, boxes through their payload root,
and functions merge only when fixed Roc arity matches, with argument, return,
and callable slots merged pointwise. Transparent nominals merge through their
explicit backing. Imported nominals expose boxed-payload children only through
instantiated interface capabilities. `opaque_atomic` nominals merge only with
the same nominal identity and a valid instantiation-sensitive
`NoReachableCallableSlotsProof`; they do not expose children. Hosted and
platform representations merge only through explicit ABI capability keys.
Recursive shapes reserve placeholders before merging children and serialize
with stable backrefs. Merge rules never inspect source expression syntax,
singleton constructor syntax, display names, physical layout order, procedure
bodies, or generated symbols.

Cross-procedure value flow uses public roots and explicit capability records.
A `call_proc` connects caller argument roots to callee public parameter roots
and callee public return roots back to caller result roots. A `proc_value`
connects the occurrence root to the callee public whole-function root and each
capture operand to the callee public capture root. No representation edge may
target another procedure by source name, display name, source body scanning, or
private builder state from another solve session.

Finite-to-erased transforms are published as demands:

```zig
const FiniteErasedAdapterDemand = struct {
    adapter: ErasedAdapterKey,
    result_ty: CanonicalExecValueTypeKey,
    member_targets: Span<ExecutableSpecializationKey>,
    provenance: NonEmptySpan<BoxErasureProvenance>,
};
```

Demands are emitted from solved value flow for direct function values,
callable-set values, captures, returns, call args, call results, records,
tuples, tags, lists, boxes, and erased adapter member captures. Nested
transforms publish nested demands while their containing value is being solved;
there is no later body walk whose purpose is to find hidden finite-to-erased
requirements. Every demand means the callable-set descriptor named by
`adapter.callable_set_key` is already published, every descriptor member has
exactly one reserved executable target in `member_targets`, and those targets
are solved before value-transform finalization. Adapter argument, capture, and
result transforms may be finalized later, but finalization consumes only these
reserved targets and the non-empty provenance carried by the demand.

Executable erased adapter reservations carry the hidden-capture payload owner:

```zig
const ErasedAdapterProcReservation = struct {
    key: ErasedAdapterKey,
    payload_owner: ErasedAdapterPayloadOwner,
    hidden_capture: ErasedAdapterHiddenCapture,
    member_targets: Span<ExecutableSpecializationKey>,
    branches: Span<FiniteSetEraseAdapterBranchPlan>,
    executable_proc: ExecutableProcId,
};

const ErasedAdapterPayloadOwner = union(enum) {
    solve_session: RepresentationSolveSessionId,
    artifact: CheckedModuleArtifactKey,
};

const ErasedAdapterHiddenCapture = union(enum) {
    none,
    callable_set_value: CanonicalCallableSetKey,
};
```

The executable adapter procedure identity is
`(ErasedAdapterKey, ErasedAdapterPayloadOwner)`, not `ErasedAdapterKey` alone.
`payload_owner.solve_session` names the solve session that published the hidden
callable-set payload for a current lowering run. `payload_owner.artifact` names
the checked artifact that owns a persisted erased adapter payload imported into
the current session. Adapter lowering uses this owner to select the executable
payload store for hidden captures and callable-set match payloads. It must not
choose the hidden capture payload by using the first member, searching sessions
for a matching key, reading erased signatures, reading layouts, inspecting
source syntax, or rebuilding payloads from the adapter body.

Persisted finite erased adapter records are sealed before executable lowering:

```zig
const PersistedFiniteErasedAdapterMember = struct {
    member_id: CallableSetMemberId,
    source_proc: SourceProcKey,
    proc_value: ProcedureCallableRef,
    member_source_payloads: Span<ConcreteSourceTypeRef>,
    lifted_owner_payloads: Span<ConcreteSourceTypeRef>,
    executable: ExecutableSpecializationKey,
    arg_transforms: Span<ValueTransformBoundaryId>,
    capture_transforms: Span<ValueTransformBoundaryId>,
    result_transform: ValueTransformBoundaryId,
};

const PersistedFiniteSetAdapterCodePlan = struct {
    sig: ErasedCallSigKey,
    members: Span<PersistedFiniteErasedAdapterMember>,
};

const FiniteSetErasePlan = struct {
    adapter: ErasedAdapterKey,
    provenance: NonEmptySpan(BoxErasureProvenance),
    member_targets: Span<FiniteSetEraseMemberTarget>,
    branches: Span<FiniteSetEraseAdapterBranchPlan>,
};

const FiniteSetEraseMemberTarget = struct {
    member: CallableSetMemberId,
    target: ProcedureCallableRef,
    executable: ExecutableSpecializationKey,
};

const FiniteSetEraseAdapterBranchPlan = struct {
    member: CallableSetMemberId,
    raw_args: Span<SessionExecutableValueEndpoint>,
    raw_result: SessionExecutableValueEndpoint,
    arg_transforms: Span<ExecutableValueTransformRef>,
    capture_transforms: Span<ExecutableValueTransformRef>,
    result_transform: ExecutableValueTransformRef,
};
```

Finite-erased adapter identity is the full erased adapter key:

```zig
const ErasedAdapterKey = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    callable_set_key: CanonicalCallableSetKey,
    erased_call_sig_key: ErasedCallSigKey,
    capture_shape_key: CaptureShapeKey,
};
```

No shorter identity is valid. Source function type plus callable-set key is not
enough, because the same callable set can be adapted through different erased
ABIs or hidden capture shapes. Executable lowering reserves the adapter under
the full key, validates descriptor member order against the callable-set key,
validates erased ABI equality against `erased_call_sig_key`, and validates
capture schema agreement against `capture_shape_key`.

Descriptor order validates the adapter member table; it is not used to recover
targets. Each member carries concrete procedure identity and executable
specialization data explicitly. For promoted wrappers, persisted finite-adapter
member keys are normalized to the promoted wrapper executable signature before
sealing while preserving concrete procedure identity fields. Private
compile-time keys do not become public adapter payload keys.

`FiniteSetErasePlan.member_targets` and `FiniteSetErasePlan.branches` have one
entry per canonical descriptor member, including singleton callable sets. A
singleton finite set crossing an erased `Box` boundary still uses this adapter
path; singleton status does not authorize a direct erased shortcut. Branch raw
argument and result endpoints are explicit physical ABI endpoints. They are not
recovered from the source function type, descriptor order, layouts, or wrapper
body. Executable MIR may materialize only adapter members named by the sealed
plan and must treat a missing member as a compiler invariant violation.

Finite adapter branch capture inputs have their own endpoint owner:

```zig
const FiniteAdapterCaptureEndpoint = struct {
    adapter: ErasedAdapterKey,
    member: CallableSetMemberId,
    slot: CaptureSlotIndex,
};
```

Conceptually this owner is `erased_finite_adapter_capture`. Each adapter branch
extracts the selected finite callable-set member payload in `CaptureSlot.index`
order, transforms each capture slot from that endpoint owner to the target
`procedure_capture` endpoint, and assembles the direct-call hidden capture tuple
from those transformed target-slot values. The branch capture payload is typed
before the branch body is lowered. Verification checks member count, capture
count, slot order, source function type, erased ABI equality, capture-shape
agreement, descriptor-member identity, and target procedure capture endpoints.

`erased_box_payload_type(T)` is a structural transform available only inside an
explicit `Box(T)` payload. It recursively rewrites reachable function slots to
erased callable representation through nested records, tuples, tags, lists,
nested boxes, function argument and return positions, and nominal backing types
when those backing types are part of the boxed payload. Calling this transform
on a non-`Box(T)` root is a compiler bug.

Box payload erasure is computed as a graph, not a tree:

```zig
const BoxPayloadRepresentationPlan = struct {
    root_payload: ConcreteSourceTypeRef,
    active_payload_to_plan_id: Map<CanonicalTypeKey, BoxPayloadPlanId>,
    completed_payload_to_plan_id: Map<CanonicalTypeKey, BoxPayloadPlanId>,
    nodes: Span(BoxPayloadPlanNode),
};

const BoxPayloadPlanNode = union(enum) {
    primitive,
    record: BoxPayloadRecordPlan,
    tuple: BoxPayloadTuplePlan,
    tag_union: BoxPayloadTagUnionPlan,
    list: BoxPayloadListPlan,
    box: BoxPayloadBoxPlan,
    function: FunctionPayloadRepresentation,
    nominal: BoxPayloadNominalPlan,
    recursive_ref: BoxPayloadPlanId,
};

const BoxPayloadBoxPlan = struct {
    boundary: ?BoxBoundaryId,
    payload_plan: BoxPayloadPlanId,
};

const FunctionPayloadRepresentation = struct {
    args: Span<BoxPayloadPlanId>,
    ret: BoxPayloadPlanId,
    callable: CallableBoxPlan,
    erased_call_sig_key: ErasedCallSigKey,
};

const CallableBoxPlan = union(enum) {
    already_erased: AlreadyErasedCallablePlan,
    proc_value_to_erased: ProcValueErasePlan,
    finite_set_to_erased_adapter: ErasedAdapterKey,
};

const ProcValueErasePlan = struct {
    source_value: ValueInfoId,
    proc_value: ProcedureCallableRef,
    target_instance: ProcRepresentationInstanceId,
    erased_call_sig_key: ErasedCallSigKey,
    capture_shape_key: CaptureShapeKey,
    executable_specialization_key: ExecutableSpecializationKey,
    adapter_arg_transforms: Span(ValueTransformBoundaryId),
    capture_slots: Span(CallableSetCaptureSlot),
    capture_transforms: Span(ValueTransformBoundaryId),
    provenance: NonEmptySpan(BoxErasureProvenance),
};
```

`active_payload_to_plan_id` reserves nodes before children are lowered so
recursive boxed payloads can refer back to an in-progress node through
`recursive_ref`. `completed_payload_to_plan_id` interns finished payload plans.
Materializing-leaf detection is part of the graph: scalar leaves, static data
leaves, finite callable leaves, already-erased callable leaves, and recursive
refs are distinguished explicitly. A function slot recursively transforms every
fixed-arity argument and return payload inside the explicit `Box(T)` payload,
then chooses one callable representation: already-erased pass-through/retype,
direct procedure-value erasure, or finite-set erased adapter. A boxed function
slot is never an opaque leaf and never becomes a tree-shaped closure record by
inspection. `ProcValueErasePlan` is only the `proc_value_to_erased` callable
case; it is not the whole function payload node.

`ProcValueErasePlan` is a boundary-local obligation for one explicit
`proc_value` occurrence. `source_value` names that occurrence. `proc_value`
names the resolved procedure handle at the exact source function type.
`target_instance` and `executable_specialization_key` select the executable
procedure that the generated erased direct-proc adapter will call.
`adapter_arg_transforms` convert raw erased ABI argument endpoints, in
fixed-arity source order, into `procedure_param { target_instance, index }`
endpoints. Those source endpoints are synthetic erased direct-proc adapter
arguments; they are not ordinary call args, captures, or locals.
`capture_transforms` convert occurrence-site capture values into
`procedure_capture { target_instance, slot }` endpoints in `CaptureSlot.index`
order. Executable MIR assembles the hidden erased capture tuple from the
transformed target-slot values. It must not place raw source captures in the
tuple unless the published transform is identity, and identity must still be an
explicit transform record.

Every erased callable has non-empty provenance:

```zig
const BoxErasureProvenance = union(enum) {
    local_boundary: BoxBoundaryId,
    const_graph_box: ConstGraphBoxErasureWitness,
    promoted_wrapper: PromotedCallableWrapperRef,
};

const ConstGraphBoxErasureWitness = struct {
    artifact: CheckedModuleArtifactKey,
    box_plan: ConstGraphReificationPlanRef,
};
```

The provenance explains why the compiler is allowed to treat this value as an
erased boxed callable. Unknown provenance, layout-derived provenance, hosted
symbol provenance, compatibility-derived provenance, and source-shape-derived
provenance are invalid. Hosted values can enter through a published boundary
whose host ABI already promises the erased callable payload shape, but the
compiler still records the boundary provenance instead of treating hosted
origin as a reason by itself.

`const_graph_box` is an artifact-stable witness, not a local lowering boundary.
It names the checked artifact that owns the const graph and the reification
plan whose `Box(T)` node authorized erased callable payloads. Persisted or
imported const-backed values must never copy `local_boundary` provenance across
artifact boundaries.

Already-erased values use explicit transform plans:

```zig
const AlreadyErasedCallableTransformPlan = union(enum) {
    exact: struct {
        source_fn_ty: ConcreteSourceTypeRef,
        abi: ErasedFnAbiKey,
    },
    same_abi_retype: struct {
        from_source_fn_ty: ConcreteSourceTypeRef,
        to_source_fn_ty: ConcreteSourceTypeRef,
        abi: ErasedFnAbiKey,
        transparent_expansion: TransparentAliasExpansionProof,
    },
};
```

`exact` is used when the value already has the requested source function type
and erased ABI. `same_abi_retype` is used only when transparent aliases or
transparent nominal backings prove that two source function types have the same
erased ABI. For example, `I64 -> ResultAlias(Str, Error)` may be retyped to
`I64 -> [Ok(Str), Err(Error)]` only when relation typing publishes the
transparent expansion proof. A `call_value_erased` request must name the exact
source function type it consumes and the exact `ErasedFnAbiKey`; a value whose
source type differs requires an explicit transform plan first.

Erased callable ABI keys are owned by a store:

```zig
const ErasedFnAbiKey = enum(u32) { _ };

const ErasedFnAbi = struct {
    key: ErasedFnAbiKey,
    fixed_arity: u32,
    arg_exec_keys: Span(CanonicalExecValueTypeKey),
    ret_exec_key: CanonicalExecValueTypeKey,
    packed_function_arg: ErasedPackedFunctionArgAbi,
    arg_abis: Span<ErasedValueAbi>,
    result_abi: ErasedResultAbi,
    capture_arg: ErasedCaptureArgAbi,
    hosted_owner: ?HostedAbiKey,
};

const ErasedFnAbiStore = struct {
    abis: Span(ErasedFnAbi),
    arg_exec_keys: Span(CanonicalExecValueTypeKey),
    arg_abis: Span(ErasedValueAbi),
};
```

`ErasedFnAbiKey` is an interned key, not the ABI payload. The `ErasedFnAbi`
payload says the fixed Roc arity, every erased argument executable key, the
erased result executable key, how a packed erased function value is passed, how
each fixed-arity argument is packed, how the result is written/read, and how
the hidden capture handle is passed. A hosted owner is present only when the
host boundary itself owns the ABI obligation. Checked artifacts own published
ABI stores; lambda-solved solve sessions may own session-local ABI stores for
runtime root uses and adapters. A later stage that holds `ErasedCallSigKey.abi`
resolves it through the explicit store for the owning artifact or solve
session. Every `arg_exec_keys` entry and the `ret_exec_key` must have a
matching executable type payload in that same semantic boundary's executable
payload store.

Canonical callable-set descriptors are pure representation data:

```zig
const CanonicalCallableSetDescriptor = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    members: Span(CallableSetMemberDescriptor),
    capture_shape: CaptureShapeKey,
};

const CallableSetMemberDescriptor = struct {
    member_id: CallableSetMemberId,
    order: ProcOrderKey,
    callable: ProcedureCallableRef,
    capture_slots: Span(CaptureSlotDescriptor),
};

const CaptureSlotDescriptor = struct {
    index: u32,
    source_ty: ConcreteSourceTypeRef,
    source_type_key: CanonicalTypeKey,
    exec_value_type_key: CanonicalExecValueTypeKey,
};
```

Descriptor member order is stable `ProcOrderKey` order. Capture field order is
`CaptureSlot.index`. Descriptors may contain procedure callable refs, source
type keys, executable value type keys, capture shape keys, and representation
ids. They must not contain executable proc ids, physical layouts, generated
symbols, runtime pointers, ARC data, backend ABI handles, object-file symbols,
or interpreter handles.

Checked artifacts own a persisted callable-set descriptor store:

```zig
const CallableSetDescriptorStore = struct {
    descriptors: Span<CanonicalCallableSetDescriptor>,
};

const SingletonCallableSetKeyInput = struct {
    source_proc: MirProcedureRef,
    proc_value: ProcedureCallableRef,
    capture_shape_key: CaptureShapeKey,
    capture_slots: Span<CallableSetCaptureSlot>,
};
```

Publication is append-only and idempotent during checking finalization.
Dependency-summary lowering, concrete compile-time root lowering,
const/callable instantiation, callable promotion, private capture
construction, erased adapter creation, and runtime lowering may all discover
descriptors before the artifact seals. A duplicate key must have byte-for-byte
identical semantic contents; a duplicate key with different members, procedure
refs, source function type, capture shape, or capture-slot schema is a compiler
bug.

Every descriptor reachable from a callable result plan, callable value emission
plan, callable-set construction plan, erased adapter key, erased callable code
ref, executable callable-set payload, materialized finite callable value,
promoted wrapper, compile-time constant, private capture, or imported template
closure is copied into the owning checked artifact before publication. A
singleton callable-set key hashes complete semantic member identity, including
source procedure, procedure callable ref, capture shape, and capture slots. It
must not hash session-local `ProcRepresentationInstanceId`, `ExecutableProcId`,
array index, body index, lowering-run ordinal, pointer identity, or allocation
order. Runtime descriptor hash tables are performance-only indexes over
already-published data; they are not semantic inputs.

During a solve session, descriptor candidates can be transient. If later
erasure requirements, promoted wrapper payloads, or capture-slot transforms
change the descriptor key for a representation group, the group emission slot
is replaced in place before the artifact store is sealed. Executable payload
publication consumes only live final descriptor refs: current group emissions,
current `CallableValueInfo.emission_plan` records, current
`CallableSetConstructionPlan` records, and current finite-erased adapter
demands. Stale interned descriptors must not be copied into checked artifacts,
runtime descriptor tables, callable result plans, or executable payloads.

A verifier checks that every live descriptor member's capture-slot executable
keys match the selected target instance's published capture endpoints. The
descriptor store is append-only after publication, but builder-time replacement
before sealing is required so obsolete transient descriptors do not become
semantic data.

Erased call signatures and capture shapes are also canonical keys:

```zig
const ErasedCallSigKey = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    abi: ErasedFnAbiKey,
};

const CaptureShapeKey = struct {
    fields: Span(CaptureShapeFieldKey),
};

const CaptureShapeFieldKey = struct {
    slot_index: u32,
    source_ty: ConcreteSourceTypeRef,
    exec_value_type_key: CanonicalExecValueTypeKey,
};
```

These keys are identity and validation data. They do not include layouts,
offsets, generated symbols, backend calling-convention handles, runtime
function pointers, interpreter procedure ids, ARC behavior, or object-file
section data.

`ErasedCallSigKey` is exactly the source function type plus the erased function
ABI key. Capture type, capture shape, wrapper code, layouts, runtime pointers,
interpreter ids, and object symbols are materialized-value data. They live in
boxed-erased callable value records, capture materialization plans, lowered code
maps, or backend images, not in the erased call signature key.

Lambda-solved MIR attaches metadata to values, not to expression ids:

```zig
const ValueInfoBuildState = enum {
    reserved,
    filling,
    sealed,
};

const ValueInfoBuildRecord = struct {
    state: ValueInfoBuildState,
    value: ValueInfo,
};

const ValueInfoStore = struct {
    values: Store(ValueInfoBuildRecord),
    bindings: Store(BindingInfo),
    projections: Store(ProjectionInfo),
    call_sites: Store(CallSiteInfo),
    capture_boundaries: Store(CaptureBoundaryInfo),
};

const ValueInfo = struct {
    id: ValueInfoId,
    source_ty: ConcreteSourceTypeRef,
    logical_ty: TypeId,
    rep_root: RepRootId,
    solved_group: RepGroupId,
    origin: ValueOrigin,
    binding: ?BindingInfoId,
    callable: ?CallableValueInfo,
    boxed: ?BoxedValueInfo,
    aggregate: ?AggregateValueInfo,
    exec_ty: ?SessionExecutableTypeEndpoint,
    const_backing: ?ConstBackedValueInfo,
    value_alias_needs_executable_transform: bool,
    value_alias_source: ?ValueInfoId,
    value_alias_transform: ?ValueTransformBoundaryId,
};

const ConstBackedValueInfo = struct {
    const_instance: ConstInstanceRef,
    schema: ComptimeSchemaId,
    value: ComptimeValueNodeRef,
};

const BindingInfo = struct {
    binder: ResolvedValueRef,
    reassignable: bool,
    current_version: MutableVersionId,
};

const CallableValueInfo = struct {
    whole_function_root: RepRootId,
    callable_root: RepRootId,
    source_fn_ty: ConcreteSourceTypeRef,
    source: CallableValueSource,
    emission_plan: CallableValueEmissionPlanId,
    construction_plan: ?CallableSetConstructionPlanId,
};

const CallableValueSource = union(enum) {
    proc_value: struct {
        proc: ProcedureCallableRef,
        captures: Span(ValueInfoId),
    },
    finite_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erased_adapter: ErasedAdapterKey,
};

const BoxedValueInfo = struct {
    box_root: RepRootId,
    payload_root: RepRootId,
    payload_value: ?ValueInfoId,
    boundary: ?BoxBoundaryId,
    erasure_provenance: ?BoxErasureProvenance,
};

const AggregateValueInfo = struct {
    fields_or_payloads: Span<ValueInfoId>,
    selected_tags: Span<SelectedTagSummaryId>,
};

const AlreadyErasedCapturePlan = union(enum) {
    none,
    zero_sized_ty: CanonicalExecValueTypeKey,
    executable_key: CanonicalExecValueTypeKey,
    value: ValueInfoId,
    materialized_capture: ArtifactErasedCaptureMaterializationRef,
};

const AlreadyErasedCallablePlan = struct {
    call_sig: ErasedCallSigKey,
    capture_shape_key: CaptureShapeKey,
    capture: AlreadyErasedCapturePlan,
    provenance: NonEmptySpan(BoxErasureProvenance),
};

const ArtifactErasedCaptureMaterializationRef = struct {
    owner: CheckedModuleArtifactKey,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

const CaptureBoundaryOwner = union(enum) {
    callable_set_construction: struct {
        construction: CallableSetConstructionPlanId,
        selected_member: CallableSetMemberRef,
    },
    proc_value_erase: struct {
        emission_plan: CallableValueEmissionPlanId,
        source_value: ValueInfoId,
        proc_value: ProcedureCallableRef,
        erased_call_sig_key: ErasedCallSigKey,
    },
};

const CaptureBoundaryInfo = struct {
    owner: CaptureBoundaryOwner,
    target_instance: ProcRepresentationInstanceId,
    slot: u32,
    source_capture_value: ValueInfoId,
    target_capture_value: ValueInfoId,
    boundary: ValueTransformBoundaryId,
};
```

Value origins include parameters, local binders, mutable versions, pattern
binders, captures, aggregate construction, projections, joins, call results,
low-level results, const materialization nodes, hosted inputs, platform inputs,
and unreachable branch terminals. The metadata follows MIR values through stage
boundaries. It is not reconstructed from source expression ids or syntax.
Builders may reserve `ValueInfoId` and `BindingInfoId` records before their
contents are complete when recursive SCC construction needs stable identity.
Those records must be filled before representation solving and sealed before
export. A reserved build record is never executable MIR input. Callable
identity, capture payload identity, boxed-erased provenance, aggregate member
identity, projection identity, and call-result identity are all attached to MIR
values through these metadata ids, not through expression-indexed maps.

`AlreadyErasedCapturePlan.materialized_capture` preserves a checked-artifact
materialized capture graph for const-backed erased callable values. Replacing it
with only `executable_key` would discard the selected finite callable member and
its already-reified captures, forcing a later stage to recover information from
descriptors or runtime bytes. Rehydration of a const-backed erased callable
imports this materialization graph from the owner artifact into the consuming
session as explicit semantic data.

A `const_instance` expression publishes `ConstBackedValueInfo` when lambda-solved
lowers it. The metadata names the sealed compile-time schema/value node inside
the owning artifact's `CompileTimeValueStore`. Projection lowering first follows
explicit `value_alias_source` chains, then consults `const_backing` before
using ordinary runtime projection metadata. Record fields are selected
by canonical field label after remapping through the owning artifact's name
resolver; tuple elements by index; tag payloads by tag label and payload logical
index; transparent aliases and nominal wrappers unwrap through the sealed
schema/value store with a bounded loop. If the selected child is a callable
leaf, lambda-solved publishes callable metadata for that value immediately.

Const-backed `Box(T)` values publish ordinary boxed-child metadata. The child
payload receives its own `ValueInfo`, `ConstBackedValueInfo`, and `.box_payload`
representation edge, while `BoxedValueInfo.boundary = null` because the
consuming session did not execute a local `Box.box` or `Box.unbox`. If the
payload contains an erased callable, erasure is authorized by
`BoxErasureProvenance.const_graph_box`, which names the artifact-stable const
graph box witness. Local `BoxBoundaryId` values from the original lowering
session must not be copied into persisted or imported const-backed values.

Already-erased const-backed leaves seed solved group emission before
lambda-solved synthesizes any type-only erased boundary metadata. Group emission
equivalence for already-erased values is runtime ABI equivalence: two views may
have different transparent-alias `source_fn_ty` values, but they are the same
runtime callable only when erased ABI, capture shape, and capture plan are
equivalent. Equivalent views union their explicit provenance sets. If ABI,
capture shape, or capture plan differs, the group contains contradictory
executable data and the compiler takes the invariant path. Only a group with no
concrete preexisting callable emission may synthesize a type-only erased
boundary plan from an explicit erased endpoint.

When const-backed `.box` rehydration republishes an erased payload, it also
appends `require_box_erased` for the rehydrated payload root using the
artifact-stable provenance stored with the const graph, normally
`const_graph_box` or `promoted_wrapper`. It must not copy `local_box_boundary`
from the lowering session that originally created the value.

Executable variable lowering consumes alias metadata explicitly. When
`value_alias_needs_executable_transform` is true, the occurrence must have
`value_alias_source` and `value_alias_transform`; executable MIR applies that
transform to the source value and then uses the resulting value handle. It must
not lower a `var_` occurrence by taking the binding's stored value and
re-ascribing the occurrence's executable type. Identity alias transforms are
still explicit records when a runtime alias boundary exists.

Vacant callable-slot aliases are schemas for absent function-typed positions,
not runtime values. Unreachable source-match branch aliases do not force value
transforms because they never reach executable MIR. Source-match reachability is
finalized before executable value-transform finalization. If a reachable runtime
alias transform would read from or write to `vacant_callable_slot`, the artifact
is malformed: debug builds assert and release builds use `unreachable`.

Consumer use plans distinguish building a value for a context from transforming
an existing value:

```zig
const ConsumerUseOwner = union(enum) {
    return_value: ReturnInfoId,
    call_arg: struct { call: CallSiteInfoId, arg_index: u32 },
    binding_write: BindingInfoId,
    reassignment_write: BindingInfoId,
    record_field: struct { parent: ValueInfoId, field: RecordFieldId },
    tuple_elem: struct { parent: ValueInfoId, index: u32 },
    tag_payload: struct { parent: ValueInfoId, tag: TagId, payload: TagPayloadId },
    list_elem: struct { parent: ValueInfoId, index: u32 },
    box_payload: ValueInfoId,
    nominal_backing: struct { parent: ValueInfoId, nominal: NominalTypeKey },
    if_branch_result: struct { join: JoinInfoId, branch: IfBranch },
    source_match_branch_result: struct { join: JoinInfoId, branch: SourceMatchBranchRef },
};

const ConsumerUsePlan = struct {
    owner: ConsumerUseOwner,
    child_value: ValueInfoId,
    expected_endpoint: SessionExecutableValueEndpoint,
    lowering: ConsumerUseLowering,
};

const ConsumerUseLowering = union(enum) {
    construct_directly,
    lower_control_flow_contextually,
    existing_value: ValueTransformBoundaryId,
};

const NominalReinterpretBackingUse = struct {
    owner: ConsumerUseOwner,
    child_value: ValueInfoId,
    expected_endpoint: SessionExecutableValueEndpoint,
};
```

Executable lowering has two internal entrypoints:
`lowerExprProducer(expr)` and
`lowerExprAtEndpoint(expr, expected_endpoint, consumer_use_plan)`.
`lowerExprProducer` lowers a standalone producer when no consumer endpoint is
known or when a value will be transformed later by an explicit boundary.
`lowerExprAtEndpoint` lowers an expression under the endpoint selected by its
consumer. Under an expected endpoint, constructors lower children directly into
target child endpoints, control-flow branch results lower under the join result
endpoint, call args lower under target parameter endpoints, returns lower under
the procedure return endpoint, and capture values lower under capture-slot
endpoints.

`construct_directly` means the child is a constructor form and executable MIR
builds it directly in `expected_endpoint`. `lower_control_flow_contextually`
means the child is a block, `let`, `if`, or source `match`; executable MIR
pushes the same expected endpoint into the completing body or branch results
while non-returning paths remain terminators. `existing_value` means the child
already exists at runtime or has already been materialized from compile-time
storage. Executable MIR evaluates or reads that child exactly once at the
boundary's `from_endpoint`, then applies the published transform to the
`to_endpoint`. It must not lower the child as an unconstrained producer and
then re-ascribe the resulting value.

This distinction matters for inactive tag variants with function slots. A tag
union value can contain variants whose payloads are not currently present at
runtime. Contextual construction records the payload representation required if
that variant is constructed. Existing-value transforms operate only on the
active runtime value. Binding writes, reassignment writes, return expressions,
and branch-result joins are consumer roots with their own endpoints; parent
endpoint provenance flows only into the value actually consumed by that parent.
Lowering must not materialize inactive function slots just because their source
type appears in the union.

`NominalReinterpretBackingUse.owner` is
`.nominal_backing { parent, nominal }` for both nominal construction and
backing reinterpretation. For nominal construction, `expected_endpoint` is the
nominal endpoint's explicit backing endpoint. For backing reinterpretation, the
consumer already wants the backing directly, so `expected_endpoint` is the
consumer endpoint itself. In the backing reinterpretation direction, nominal
identity comes from published child logical/source type data; lambda-solved MIR
must not infer it from syntax, source names, or layout compatibility. The
consumer-use edge is published before the solve session seals.

Call sites publish full call metadata:

```zig
const CallSiteInfo = struct {
    expr: ExprId,
    result: ValueInfoId,
    callee: ?ValueInfoId,
    args: Span(ValueInfoId),
    arg_exprs: Span(ExprId),
    requested_fn_root: RepRootId,
    requested_source_fn_ty: ConcreteSourceTypeRef,
    dispatch: CallDispatchInfo,
    arg_transforms: Span<ValueTransformBoundaryId>,
    arg_consumer_uses: Span<ConsumerUsePlan>,
    result_transform: ?ValueTransformBoundaryId,
};

const CallDispatchInfo = union(enum) {
    direct_proc: CallProcExecutablePlan,
    finite_callable_set: CallValueFiniteDispatchPlan,
    erased_callable: ErasedCallableCallPlan,
    low_level: LowLevelValueFlowSignatureId,
};

const CallValueFiniteDispatchBranch = struct {
    member: CallableSetMemberRef,
    target_instance: ProcRepresentationInstanceId,
    arg_transforms: Span<ValueTransformBoundaryId>,
    result_transform: ValueTransformBoundaryId,
};

const CallValueFiniteDispatchPlan = struct {
    callable_set_key: CanonicalCallableSetKey,
    branches: Span<CallValueFiniteDispatchBranch>,
};

const ErasedCallableCallPlan = struct {
    callable_payload: ValueInfoId,
    sig: ErasedCallSigKey,
    arg_payloads: Span<ErasedAbiArgPayloadId>,
    result_payload: ErasedAbiResultPayloadId,
};
```

Finite-callable branch plans carry branch-local argument transforms and result
transforms. Erased calls pack arguments through the exact erased ABI payloads,
call the payload's `callable_fn_ptr`, and unpack the result through the exact
result payload. They never inspect capture layout at the call site.

`arg_exprs` are the lowered expression ids from the call expression that owns
the `CallSiteInfo`. Call-argument finalization consumes those explicit ids; it
does not scan the enclosing body to find arguments after the fact. For
`call_proc`, `callee` is null because the target is the procedure identity in
the call node. For `call_value`, `callee` names the function value occurrence.
`requested_source_fn_ty` is the exact canonical fixed-arity Roc function type
requested by this call expression, and `requested_fn_root` is the
representation root whose solved callable child determines the dispatch plan.
`arg_consumer_uses` are the authority for `call_proc` and erased-call
arguments. Finite-call branches consume per-branch arg/result transforms from
already evaluated call values to `target_instance` parameter endpoints and from
the branch procedure return to `CallSiteInfo.result`. Finite call arguments are
evaluated once before `callable_match`; branch-specific transforms must not
duplicate argument evaluation. A shared contextual argument consumer-use is
allowed only when an earlier stage explicitly publishes the proof that every
branch endpoint for that argument agrees. `result_transform` is mandatory for
call forms that produce a raw result endpoint and only absent for forms whose
lowering has no raw-result boundary.

Executable value transforms have published and session-local identities:

```zig
const PublishedExecutableValueTransformRef = struct {
    artifact: CheckedModuleArtifactKey,
    transform: ArtifactExecutableValueTransformId,
};

const ExecutableValueTransformRef = union(enum) {
    published: PublishedExecutableValueTransformRef,
    session: SessionExecutableValueTransformId,
};

const SessionExecutableValueTransformStore = struct {
    owner: ExecutableLoweringSessionId,
    transforms: Span(SessionExecutableValueTransformPlan),
};

const TransformEndpointScope = struct {
    root_kind: ValueTransformBoundaryKind,
    root_from: SessionExecutableValueEndpoint,
    root_to: SessionExecutableValueEndpoint,
};

const TransformEndpointPathStep = union(enum) {
    record_field: RecordFieldLabelId,
    tuple_elem: u32,
    tag_payload: struct {
        tag: TagLabelId,
        payload_index: u32,
    },
    list_elem,
    box_payload,
    nominal_backing: NominalTypeKey,
    callable_leaf,
};

const SessionExecutableValueEndpointOwner = union(enum) {
    local_value: ValueInfoId,
    procedure_param: struct { instance: ProcRepresentationInstanceId, index: u32 },
    procedure_return: ProcRepresentationInstanceId,
    procedure_capture: struct { instance: ProcRepresentationInstanceId, slot: u32 },
    call_raw_arg: struct { call: CallSiteInfoId, index: u32 },
    call_raw_result: CallSiteInfoId,
    projection_slot: ProjectionInfoId,
    transform_child: TransformChildEndpoint,
};

const SessionExecutableValueEndpoint = struct {
    owner: SessionExecutableValueEndpointOwner,
    logical_ty: TypeId,
    exec_ty: SessionExecutableTypeEndpoint,
};

const SessionBoxPayloadTransformPlan = struct {
    boundary: ?BoxBoundaryId,
    kind: BoxPayloadTransformKind,
    payload: ExecutableValueTransformRef,
};

const BoxPayloadTransformKind = enum {
    payload_to_box,
    box_to_payload,
    box_to_box,
};

const CallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteCallableValueToErasedPlan,
    proc_value: ProcValueToErasedPlan,
};

const SessionExecutableStructuralBridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: ExecutableValueTransformRef,
    box_box: ExecutableValueTransformRef,
    singleton_to_tag_union: struct {
        source_tag: TagId,
        target_tag: TagId,
        value_transform: ?ExecutableValueTransformRef,
    },
    tag_union_to_singleton: struct {
        source_tag: TagId,
        target_tag: TagId,
        value_transform: ?ExecutableValueTransformRef,
    },
};

const SessionExecutableValueTransformOp = union(enum) {
    identity,
    structural_bridge: SessionExecutableStructuralBridgePlan,
    record: Span(SessionValueTransformRecordField),
    tuple: Span(SessionValueTransformTupleElem),
    tag_union: Span(SessionValueTransformTagCase),
    nominal: struct {
        nominal: NominalTypeKey,
        backing: ExecutableValueTransformRef,
    },
    list: struct {
        elem: ExecutableValueTransformRef,
    },
    box_payload: SessionBoxPayloadTransformPlan,
    callable_to_erased: CallableToErasedTransformPlan,
    already_erased_callable: AlreadyErasedCallableTransformPlan,
};

const SessionExecutableValueTransformPlan = struct {
    scope: ?TransformEndpointScopeId,
    from: SessionExecutableValueEndpoint,
    to: SessionExecutableValueEndpoint,
    provenance: ValueTransformProvenance,
    op: SessionExecutableValueTransformOp,
};
```

Published transform refs name artifact-owned plans that can be reused by
importers, readonly materialization, promoted wrappers, and relation bindings.
Session refs name transform records created while solving one executable
session. Endpoint owners, endpoint scopes, source paths, target paths, and
child transforms are part of the ref's owning store. A session transform must
not be stored in a checked artifact, and a published transform must not point
back to a session-local value id.

Lowering dispatches transform refs by owner. A published ref resolves through
the imported or current artifact view that owns the published transform and
through that artifact's published executable payload store. A session ref
resolves through the current `SessionExecutableValueTransformStore` and current
`SessionExecutableTypePayloadStore`. Recursive children obey the same owner
rule: published children remain published refs, and session children remain
session refs unless an explicit import/remap operation creates a new session
payload and session transform. Structural bridge children may be only identity
bridges or other structural bridges over already published endpoint payloads.
Missing artifact views, missing published payloads, missing session entries, or
a transform key whose endpoints disagree with the expected executable keys are
compiler invariant violations.

`SessionBoxPayloadTransformPlan.boundary` is present only for a real local
`Box.box` or `Box.unbox` boundary in the same `RepresentationStore`. It is null
for promoted-wrapper argument/result transforms whose authorization is the
sealed wrapper provenance. A null boundary authorizes no erasure by itself; any
callable leaf transform inside `payload` still needs non-empty
`BoxErasureProvenance`.

Structural bridges are explicit and direction-sensitive. `direct` means the
same physical value is valid at both endpoints. `zst` materializes or consumes
zero-width data as required by the endpoint. `list_reinterpret` changes the
element endpoint through a published child transform without rebuilding from
source syntax. `nominal_reinterpret` crosses a nominal/backing boundary that was
published as a value transform; it is not a general nominal compatibility check.
`box_unbox`, `box_box`, and `BoxPayloadTransformKind` distinguish payload-to-box,
box-to-payload, and box-to-box endpoints. `singleton_to_tag_union` and
`tag_union_to_singleton` are ordinary tag-union structural bridges for the
published source and target tags. None of these bridge variants may be selected
by comparing layouts or display names in the consuming stage.

`CallableToErasedTransformPlan` has two contexts. An expression-owned explicit
`proc_value` occurrence may use `proc_value` because the occurrence still owns
its capture operands and can pack them through the sealed `ProcValueErasePlan`.
An existing-value transform, such as a projection result, join input, return
value, alias, or aggregate child, may only pack an already constructed finite
callable-set value or pass through/retype an already-erased callable.
Executable MIR must not recover captures from an arbitrary
`ExecutableValueRef`. If `proc_value_to_erased` reaches an existing-value
boundary, lambda-solved failed to publish the required construction-site
metadata.

Value-transform boundaries are explicit:

```zig
const ValueTransformBoundary = struct {
    id: ValueTransformBoundaryId,
    kind: ValueTransformBoundaryKind,
    owner: ValueTransformOwner,
    endpoint_scope: TransformEndpointScope,
    source_path: TransformPath,
    target_path: TransformPath,
    plan: ValueTransformPlan,
};

const ValueTransformBoundaryKind = union(enum) {
    identity,
    call_raw_arg,
    call_raw_result,
    capture_slot,
    aggregate_field,
    projection_result: ProjectionInfoId,
    consumer_use,
    boxed_payload,
    erased_capture,
    return_endpoint,
    join_endpoint,
};
```

Identity transforms are still records when a boundary exists. Owners identify
the call site, capture, aggregate construction, projection, consumer use,
return, or join that required the transform. Raw call argument and raw call
result owners distinguish physical ABI slots from source-level values. A
transform boundary never owns arbitrary source expressions; it owns already
evaluated value refs and endpoint paths.

Projection, join, and return metadata are separate:

```zig
const JoinInputSource = union(enum) {
    if_branch: struct {
        if_expr: IfExprId,
        branch: IfBranch,
    },
    source_match_branch: struct {
        match: SourceMatchId,
        branch: SourceMatchBranchRef,
        alternative: SourceMatchAlternativeRef,
    },
    loop_phi: LoopPhiId,
};

const JoinInputInfo = struct {
    source: JoinInputSource,
    value: ValueInfoId,
};

const ProjectionInfo = struct {
    source: ValueInfoId,
    source_slot: ProjectionSlot,
    endpoint_slot: ProjectionEndpointSlot,
    raw_slot: ?RawPhysicalSlot,
    projected_result: ValueInfoId,
    result_transform: ValueTransformBoundaryId,
};

const JoinInfo = struct {
    result: ValueInfoId,
    inputs: Span(JoinInputInfo),
    root: RepRootId,
    kind: JoinKind,
    endpoint_slot: JoinEndpointSlot,
    input_transforms: Span<ValueTransformBoundaryId>,
    non_returning_branches: Span<SourceMatchBranchRef>,
};

const ReturnInfo = struct {
    source_value: ValueInfoId,
    return_endpoint_slot: ReturnEndpointSlot,
    transform: ?ValueTransformBoundaryId,
};
```

Projection records distinguish the raw physical slot read from a value from the
projected source-level result. `source_slot` names the slot in the producer's
source value. `endpoint_slot` names the slot in the consumer endpoint that gives
authority for the projected child's executable payload. `raw_slot` names the
physical stored location that must be read. Projection lowering derives the
child executable type from the parent aggregate endpoint payload; it must not
re-lower the child expression, inspect syntax, or use layout order to infer the
child type. `source_slot` is the row-finalized source slot named by the
projection expression. `endpoint_slot` is the slot selected inside the actual
published executable parent endpoint after entering any transparent backing or
other explicit endpoint path. Executable MIR accesses the raw stored value
through `endpoint_slot`, not by field-name lookup and not by `source_slot`.
The transform from the raw stored slot to `projected_result` is mandatory and
uses `projection_slot: ProjectionInfoId` as its source endpoint owner and
`projection_result: ProjectionInfoId` as its boundary kind. Even when the
transform is identity, it is a published record.

The same authority rule applies to aggregate construction. Tag construction
builds the payload directly at the parent union variant's exact physical slot
layout. It must not create a source-shaped temporary payload record and then
repair the whole record with a bridge. Record fields, tuple elements, tag
payloads, list elements, and boxed payloads all publish projection/assembly
edges from already evaluated child values to explicit parent endpoint slots.

Joins distinguish an endpoint slot from each incoming source slot and record
non-returning branches explicitly. `JoinInputInfo.source` is semantic
authority for each input: storage order is deterministic, but it does not
identify the branch, alternative, or loop phi. A branch body that terminates
with `return`, `crash`, or runtime error publishes no `JoinInputInfo` and does
not renumber later alternatives. `JoinInfo.result` and `JoinInfo.root` own the
join endpoint; executable MIR must not choose the first incoming value as the
join type authority. Executable MIR consumes `JoinInfo.input_transforms` before
IR lowering. Return records make procedure result transformation visible before
executable MIR.

Procedure boundaries publish executable payloads before body materialization.
For every executable procedure specialization, the boundary owns parameter
payload records, return payload records, and capture-slot payload records.
Executable body lowering consumes those records. It does not create a missing
parameter, return, or capture payload while lowering a body.

Structural executable type payloads also have published and session-local
owners:

```zig
const SessionExecutableTypePayloadStore = struct {
    owner: ExecutableLoweringSessionId,
    entries: Span(SessionExecutableTypePayloadEntry),
    by_key: Map(CanonicalExecValueTypeKey, SessionExecutableTypePayloadRef),
};

const SessionExecutableTypePayloadEntry = struct {
    key: CanonicalExecValueTypeKey,
    payload: SessionExecutableTypePayload,
};

const SessionExecutableTypePayload = union(enum) {
    pending,
    primitive: ExecutablePrimitive,
    record: SessionExecutableRecordPayload,
    tuple: Span(SessionExecutableTupleElemPayload),
    tag_union: SessionExecutableTagUnionPayload,
    list: SessionExecutableTypePayloadChild,
    box: SessionExecutableTypePayloadChild,
    nominal: SessionExecutableNominalPayload,
    callable_set: SessionExecutableCallableSetPayload,
    erased_callable_slot: SessionExecutableErasedFnPayload,
    recursive_ref: SessionExecutableTypePayloadRef,
};

const SessionExecutableRecordPayload = struct {
    shape: RecordShapeKey,
    fields: Span(SessionExecutableRecordFieldPayload),
};

const SessionExecutableRecordFieldPayload = struct {
    field: RecordFieldLabelId,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

const SessionExecutableTupleElemPayload = struct {
    index: u32,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

const SessionExecutableTagUnionPayload = struct {
    shape: TagUnionShapeKey,
    variants: Span(SessionExecutableTagVariantPayload),
};

const SessionExecutableTagVariantPayload = struct {
    tag: TagLabelId,
    payloads: Span(SessionExecutableTagPayload),
};

const SessionExecutableTagPayload = struct {
    payload_index: u32,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

const SessionExecutableNominalPayload = struct {
    nominal: NominalTypeKey,
    backing: SessionExecutableTypePayloadRef,
    backing_key: CanonicalExecValueTypeKey,
};

const SessionExecutableCallableSetPayload = struct {
    key: CanonicalCallableSetKey,
    members: Span(SessionExecutableCallableSetMemberPayload),
};

const SessionExecutableCallableSetMemberPayload = struct {
    member: CallableSetMemberId,
    payload_ty: ?SessionExecutableTypePayloadRef,
    payload_ty_key: ?CanonicalExecValueTypeKey,
};

const SessionExecutableErasedFnPayload = struct {
    call_sig: ErasedCallSigKey,
};
```

An artifact `ExecutableTypePayloadRef` is stable published data. A
`SessionExecutableTypePayloadRef` is valid only inside one lambda-solved or
executable lowering session. Isomorphic payloads can share a canonical key, but
the session still owns the structural payload that tells lowering how to
traverse fields, payloads, recursive refs, callable slots, and erased slots.
Recursive session refs point to payloads in the same
`SessionExecutableTypePayloadStore`; crossing to an artifact payload requires a
published ref and explicit remapping.
Appending a payload transfers ownership to the store. If the key is already
present, the store returns the existing ref and discards the duplicate payload;
this is reuse of already-published session data, not reconstruction from a key.
The `by_key` map is an integrity/reuse index over owned entries. It may never
synthesize a missing structural payload from a canonical key, source type,
layout, ABI, or runtime bytes.

Executable type lowering keeps two separate maps. The active-recursion map is
keyed by the active structural cycle, represented by `RootPayloadCycleKey`; it
prevents infinite descent while a recursive payload is being constructed. The
`completed source type root` map is keyed by the explicit lambda-solved or
session source type root that was lowered; it prevents later uses of the same
root from allocating a second executable type graph. Re-lowering one explicit
source type root into two executable graphs is a compiler bug because
transparent nominal reinterpretation, pattern payloads, and recursive physical
slots all depend on stable one-root/one-graph identity.

`RepresentationGroupId` is value-flow identity, `RootPayloadCycleKey` is active
recursive structural-payload identity, and `CanonicalExecValueTypeKey` is the
completed executable endpoint identity. Payload publication verifies that every
published endpoint and child endpoint round-trips through the payload store:
the key recorded on the payload-store entry is the endpoint's canonical key,
and each child entry's key is the child's canonical key. A mismatch means the
payload store is being used as reconstruction machinery rather than as an
owned structural payload store.

Endpoint lowering resolves all forwarding cases before reserving payload-store
entries. Alias values, join values, and plain function-typed values that
forward to their solved callable group do not own structural payloads at that
value occurrence. They return the forwarded endpoint without reserving a new
payload key. Reserving a payload first and then forwarding leaves an orphan
`pending` payload and is a compiler bug.

Callable-set payloads can be derived before all member capture payloads are
final. The owning session may replace an unconsumed derived callable-set payload
for the same canonical callable-set key before the payload store seals, but only
with a new payload built from the explicit final callable-set descriptor and
member capture-slot payload refs. No downstream stage may have consumed the superseded
payload, and no other payload case may be replaced this way.

Promoted callable values publish the same payload data. Even an empty aggregate
capture such as `List(vacant_callable_slot(...))` has an explicit capture-slot
payload when the callable value's source type contains that slot. Empty capture
bytes are still represented by explicit capture metadata; absence of bytes is
not absence of semantic payload.

### Executable Callable And Direct-Call Plans

Executable procedure definitions carry explicit origin:

```zig
const ExecutableProcOrigin = union(enum) {
    source: MirProcedureRef,
    erased_adapter: ErasedAdapterKey,
};
```

Source procedure lookup may inspect only `ExecutableProcOrigin.source`. Erased
adapter lookup may inspect only `ExecutableProcOrigin.erased_adapter` and must
compare the full `ErasedAdapterKey`. An erased adapter must not fake its origin
by borrowing the first member procedure's `MirProcedureRef`; doing so would
make adapter identity depend on descriptor member order instead of the sealed
adapter key.

Every `call_proc` lowers through an explicit executable plan before it becomes
`call_direct`:

```zig
const CallProcExecutablePlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    target: ProcedureCallableRef,
    executable_specialization: ExecutableSpecializationKey,
    arg_transforms: Span(ValueTransformPlan),
    result_transform: ValueTransformPlan,
};
```

Executable lowering uses this plan. It does not derive the executable target
from source procedure name, function type, argument count, or layout
compatibility.

`callable_match` consumes the original `CallableSetConstructionPlan`, the
canonical callable-set member table, and branch-local value transforms. Every
branch must agree with:

- the requested source function type
- argument count and argument source types
- selected callable member
- capture schema
- executable target specialization
- result source type and executable result transform

Singleton finite callable sets still lower through `callable_match`; singleton
status is not permission to bypass the callable value representation.

Callable-set member tag assignment is deterministic. Member ordering uses the
canonical lambda-solved callable-set member order, which is derived from stable
`ProcOrderKey` values. It must not depend on raw symbols, display names,
hash-map iteration order, allocation order, pointer identity, fresh-symbol
suffixes, or incidental traversal order.

Capture payload field order is `CaptureSlot.index` order for every
callable-set member and every erased capture record. Executable MIR consumes
that order. It does not sort captures by name, scan procedure bodies, or inspect
environments to rebuild capture order.

Executable value type keys are representation identity keys:

```zig
const CanonicalExecValueTypeKey = union(enum) {
    primitive: PrimitiveExecKey,
    record: RecordExecKey,
    tuple: TupleExecKey,
    tag_union: TagUnionExecKey,
    list: CanonicalExecValueTypeKey,
    box: CanonicalExecValueTypeKey,
    nominal: NominalExecKey,
    callable_set: CanonicalCallableSetKey,
    erased_callable_slot: ErasedCallSigKey,
    vacant_callable_slot: CanonicalTypeKey,
};
```

The exact representation may differ, but source function values do not appear
as a runtime `function` case. Function-typed values lower to finite callable
sets, erased callable slots, or vacant callable slots. `vacant_callable_slot` is
an uninhabited structural payload for a function-typed slot. It is never an
expression value and never a call target.

A canonical executable value type key is not a serialized executable type
payload. It is valid only when paired with an explicit structural payload owned
by the relevant boundary. Later stages must never lower a bare key to a type by
reconstructing the payload.

Recursive executable type keying separates root cycles from root layers:

```zig
const RootPayloadCycleKey = struct {
    root: CanonicalExecValueTypeKey,
    cycle_members: Span(CanonicalExecValueTypeKey),
};

const RootPayloadLayerKey = struct {
    root: CanonicalExecValueTypeKey,
    layer_index: u32,
    projection: SolvedGroupProjectionRef,
};

const SolvedGroupProjectionRelation = struct {
    source_group: RepresentationSolveSessionId,
    root_payload: RootPayloadCycleKey,
    projected_layers: Span(RootPayloadLayerKey),
};
```

Payload publication is keyed by `CanonicalExecValueTypeKey` plus the explicit
root cycle/layer relation that makes recursive references meaningful. A
recursive member may share a canonical key but appear at a different root layer;
the payload publication records which solved group projection is being lowered.
Later stages consume the published payload relation. They do not infer
recursive root ownership from layout recursion, nominal names, or call stacks.

Every `ExecutableValueRef` has exactly one executable type as soon as it is
introduced. A `value_ref` expression derives its type from the referenced value;
it does not accept a replacement type from the caller. A bridge, boxed-boundary
transform, aggregate transform, erased packing operation, or callable adapter
argument transform that changes representation allocates and defines a fresh
`ExecutableValueRef`.

Executable MIR owns local ids explicitly:

```zig
const LocalValueId = enum(u32) { _ };

const LetValue = struct {
    local: LocalValueId,
    ty: ExecTypeId,
    defined_in: ExecutableBlockId,
};

const EvalBoundary = struct {
    owner: ExecutableBlockId,
    inputs: Span<ExecutableValueRef>,
    outputs: Span<ExecutableValueRef>,
};
```

Blocks contain ordered statements that define `LocalValueId` values before
those values can be referenced. Branch alternatives, guards, source-match
bodies, loops, and join continuations have branch-local scopes. A value defined
inside one branch cannot escape except through an explicit join/result record.
Runtime uniqueness mutation sites name the local being checked, the value that
is installed on success, and the replacement/cleanup path on failure.
Executable verifiers reject use-before-definition, out-of-scope use,
branch-local escapes, duplicate local definitions, and mutation sites whose
owned token or executable type does not match the published local.

Executable MIR need not be in administrative normal form. It may keep nested
expression structure where that structure does not cross a semantic boundary.
LIR must be in administrative normal form before ARC and backend consumption.

Bridges consume already evaluated `ExecutableValueRef` handles. They do not own
source expressions and must not re-run calls, box operations, field projections,
source matches, aggregate construction, or any expression with allocation,
control-flow, or reference-counting behavior.

Generic zero-sized and singleton values use the same lowering rules as all
other values:

- zero-sized aggregate fields are elided only by the committed layout graph; the
  source value still has a type, value ref, and transform metadata
- a singleton tag union may lower `get_union_id` to a constant discriminant, but
  the value remains a tag union at runtime boundaries
- zero-sized tag payload bridges are explicit bridges, even when the bridge
  emits no bytes
- singleton finite callable sets still finalize as callable-set values and
  lower calls through `callable_match`
- Bool receives no runtime special handling; Bool literals, returns, storage,
  boxes, exports, and host-visible values use ordinary tag-union layout

### Source Match Decision Plans

Source `match` lowering publishes decision plans separate from callable-set
matching:

```zig
const SourceMatchDecisionPlan = struct {
    scrutinees: Span(ValueRef),
    alternatives: Span(SourceMatchAlternative),
    result_join: MatchResultJoin,
};

const SourceMatchAlternative = struct {
    pattern_paths: Span(PatternPathDecision),
    guard: ?GuardDecision,
    representative_binders: Span(BinderRewrite),
    reachability: AlternativeReachability,
};

const SourceMatch = struct {
    id: SourceMatchId,
    scrutinees: Span(ValueInfoId),
    materialized_paths: Span(MaterializedPatternPathValue),
    plan: PatternDecisionPlan,
    result_join: MatchResultJoin,
};

const PatternDecisionPlan = struct {
    nodes: Span(DecisionNode),
    edges: Span(DecisionEdge),
    leaves: Span(DecisionLeaf),
    path_values: Span(PatternPathValuePlan),
    bindings: Span(PatternBinding),
};

const DecisionNode = union(enum) {
    tag_test: TagTestNode,
    union_id_test: UnionIdTestNode,
    byte_union_test: ByteUnionTestNode,
    int_literal_test: IntLiteralTestNode,
    decimal_literal_test: DecimalLiteralTestNode,
    float_literal_test: FloatLiteralTestNode,
    string_literal_test: StringLiteralTestNode,
    list_length_test: ListLengthTestNode,
    guard: GuardDecision,
    degenerate: DegenerateDecisionNode,
};

const DecisionEdge = struct {
    from: DecisionNodeId,
    test: DecisionEdgeTest,
    to: DecisionNodeOrLeaf,
};

const DecisionTestNode = struct {
    path: PatternPathValuePlanId,
    test: DecisionNode,
};

const DecisionLeaf = struct {
    alternative: SourceMatchAlternativeId,
    reachability: AlternativeReachability,
    bindings: Span(PatternBindingId),
    terminal: DecisionLeafTerminal,
    result: SourceBranchResult,
};

const SourceBranchResult = union(enum) {
    returns: struct {
        branch_result: ExecutableValueRef,
        result_transform: ExecutableValueTransformRef,
        final_result: ExecutableValueRef,
    },
    no_return,
    degenerate_runtime_error,
};

const PatternPathValuePlan = union(enum) {
    scrutinee,
    record_field: RecordFieldProjection,
    record_rest: RecordRestProjection,
    tuple_field: TupleFieldProjection,
    tag_payload: TagPayloadProjection,
    opaque_payload: OpaquePayloadProjection,
    newtype_payload: NewtypePayloadProjection,
    list_head: ListHeadProjection,
    list_rest: ListRestProjection,
    literal_carrier: LiteralCarrierProjection,
};

const RecordRestProjection = struct {
    source_record: ValueInfoId,
    excluded_fields: Span(FieldId),
    result_record_shape: RowFinalizedRecordShapeId,
    projected_fields: Span<RecordRestProjectedField>,
};

const RecordRestProjectedField = struct {
    source_field: FinalizedRecordFieldId,
    result_field: FinalizedRecordFieldId,
    transform: ExecutableValueTransformRef,
};

const PatternPathStep = union(enum) {
    scrutinee: MatchScrutineeId,
    record_field: FinalizedRecordFieldId,
    record_rest: RecordRestProjectionId,
    tuple_elem: u32,
    tag_payload: struct { tag: FinalizedTagId, payload_index: u32 },
    opaque_payload,
    newtype_payload,
    list_head: u32,
    list_tail: u32,
    list_rest: ListRestProjectionId,
};

const PatternBinding = struct {
    source_binder: CheckedPatternBinderRef,
    representative: ResolvedValueRef,
    path_value: PatternPathValuePlanId,
    transform: ValueTransformBoundaryId,
};

const MatchScrutinee = struct {
    id: MatchScrutineeId,
    value: ValueInfoId,
    tmp: ExecutableValueRef,
};

const MaterializedPatternPathValue = struct {
    path: PatternPathId,
    value: ExecutableValueRef,
    owner: SourceMatchBranchRef,
};

const AlternativeBinderRemap = struct {
    candidate_binder: CheckedPatternBinderRef,
    representative_binder: ResolvedValueRef,
    transform: PatternBindingTransform,
};

const PatternBindingTransform = struct {
    path_value: PatternPathId,
    value_transform: ExecutableValueTransformRef,
};
```

The decision plan owns:

- scrutinee evaluation order
- the scrutinee temporaries evaluated before the decision root
- the decision root node for each scrutinee path
- branch order
- guard order
- tag, union-id, byte-union, integer, decimal/float, string-literal, and
  list-length tests
- explicit `DecisionEdge.test` payloads for every edge
- pattern-path payload projections
- representative binder selection
- degenerate branch metadata
- unreachable branch metadata
- result join shape
- list head/tail/rest probe metadata
- optional record-field default plans

Source-match IR lowering first evaluates each scrutinee into a stable temporary,
then runs the decision root over those temporaries. Tag and union-id tests use
row-finalized discriminants. Byte-union tests read the committed byte-union
layout. Integer tests compare scalar carriers. Decimal and float literal tests
call the published equality operation for that literal type. String tests
compare against the `ProgramLiteralId` named by the decision node. List-length
probes read length once per path-local cache scope. Guard evaluation happens
only after every path value, representative binder, and mandatory binder
transform needed by that guard has been materialized.

Alternative-pattern binders are rewritten to representative binders before
guard or body lowering. Guards see the same representative binders as the
branch body. Later stages must not rebuild binder equivalence from names,
pattern syntax, or branch order.
The rewrite is stored as `AlternativeBinderRemap`. Branch lowering never uses a
candidate binder directly after remapping; it materializes the candidate path
value, applies the mandatory `PatternBindingTransform`, and binds the
representative binder visible to guards and body expressions.

List element and rest path values are explicit probes. Head indexes, tail
indexes, rest start, and rest length come from checked list-pattern arity. If a
tail index needs the list length, the decision plan names the length probe once
for that path-local scope. Optional record-field defaults are also published in
the decision plan; post-check lowering must not rediscover default behavior by
scanning record field names.

Reachability is value/path-specific. Lambda-solved answers selected-tag summary
queries with an explicit key:

```zig
const SelectedTagPathKey = struct {
    value: ValueInfoId,
    path: PatternPathId,
};

const SelectedTagSummary = union(enum) {
    unknown,
    exact: Span(TagSelection),
};

const TagSelection = struct {
    union_shape: TagUnionShapeId,
    tag: TagId,
};
```

`unknown` means no source-match branch may be pruned from this summary.
`exact` means the selected-tag set is complete for that value path in the solve
session. The summary belongs to a value occurrence and explicit structural path,
not to an entire representation group. Joins, returns, loop phis, mutable
versions, and branch merges can place multiple runtime alternatives in one
representation group, so group-level selected-tag state is unsound.

Every value and call site produced inside a source-match alternative carries
branch ownership:

```zig
const SourceMatchBranchRef = struct {
    match: SourceMatchId,
    branch: SourceMatchBranchId,
    alternative: SourceMatchAlternativeId,
};

const SourceMatchBranchReachability = struct {
    ref: SourceMatchBranchRef,
    reachable: bool,
};
```

Pattern binders, guards, and branch-body locals carry the enclosing branch ref.
The scrutinee does not become branch-owned merely because it is matched. Nested
matches record the nearest branch on each occurrence and parent relations in
the reachability table.

Reachability consumes only explicit solved representation data: selected tag
metadata for constructed values, aliases, projections, proc-value capture
edges, returns, joins, loop phis, mutable versions, and explicit runtime
unknown producers. Proc captures inherit selected-tag summaries through the
published proc-value capture edge; a capture root is not unknown merely because
the lifted target body has no local aggregate metadata.

An unreachable alternative:

- contributes no callable emission plans, finite callable members, erased
  callable plans, direct-call dependencies, const dependencies, executable body,
  IR body, ARC statements, or backend input
- lowers to an explicit `unreachable` terminal
- keeps its branch identity for debug metadata and decision-plan verification
- may contain unresolved function-typed slots only because the branch is proven
  unreachable

Reachable alternatives must have complete finite or erased callable
representation for every function-typed value and complete dispatch for every
call site.

IR lowering of source `match` consumes the decision plan. It does not build an
ordered cascade from source patterns. It materializes only the path values
required by the reached leaf, applies explicit pattern-binder transforms, binds
representative binders, evaluates guards in the guard environment, and assigns
the shared match result exactly once. A degenerate leaf emits the checked
runtime-error path immediately and does not materialize branch binders or lower
the branch body.
Every returning leaf assigns exactly one value to the shared result: the
`SourceBranchResult.returns.final_result` produced by applying the mandatory
result transform to the branch-local result. `no_return` leaves and degenerate
runtime-error leaves have no result transform and do not constrain the shared
result. Branch-local layouts and branch-local return types must not escape the
`SourceMatch` node.
IR consumes `SourceMatch.materialized_paths` and `MatchScrutinee.tmp` records.
It must not reinterpret executable pattern syntax or synthesize projections
from raw source patterns. If a path value is required by an edge, guard, binder,
or body, it must be present in the materialized path table for the owning match
and branch.

Control-flow predicates consume ordinary Roc Bool values through explicit
predicate data:

```zig
const BoolDiscriminants = struct {
    false_discriminant: u16,
    true_discriminant: u16,
};

const BoolCondition = struct {
    expr: ExprId,
    true_discriminant: u16,
};
```

`BoolDiscriminants` is attached to predicate-producing expression nodes whose
semantic result type is the ordinary Roc `Bool` tag union, such as
`structural_eq`, numeric comparisons, string comparisons, byte comparisons, and
low-level predicates. IR may lower a machine predicate internally, but when the
expression result is a Roc value it immediately constructs the ordinary Bool
tag-union value by selecting the resolved zero-payload `False` or `True`
discriminant. There is no Bool runtime representation separate from ordinary
tag-union values.

IR reads the ordinary tag-union discriminant from `expr` and compares it with
`true_discriminant` to produce an internal branch predicate. That internal
predicate is not a Roc value and must not be stored, returned, boxed, exported,
or treated as a runtime Bool representation.

Selected-tag summaries are computed from explicit value-flow sources:

- aggregate construction records an exact tag at the constructed path
- aliasing copies the selected-tag summary for the same path
- field, tuple, tag-payload, list-element, box, and nominal projections publish
  path transforms from producer value to projected value
- proc-value capture edges carry selected-tag summaries from capture operands
  into the captured slot path of the procedure value
- returns carry the returned value/path summary to the caller result path
- joins, `if` results, source-match results, loop phis, and mutable versions
  conservatively union all incoming exact sets for the same value path
- a runtime producer whose selected tag cannot be known publishes `unknown`
- if any incoming path is `unknown`, the joined summary is `unknown`

Before source-match reachability, public procedure roots are rewritten to
procedure-boundary root kinds. Parameter roots, return roots, and capture roots
must not remain generic local value roots. This prevents branch reachability
from treating a public parameter, return, or capture slot as if it were an
ordinary local whose producer body was available.

Source-match reachability verifiers assert:

- every value and call site inside an alternative, guard, or body carries the
  correct `SourceMatchBranchRef`
- unresolved callable slots appear only in alternatives marked unreachable
- every reachable branch has complete finite or erased callable representation
- executable MIR and IR agree on which alternatives are reachable
- degenerate alternatives preserve debug identity while emitting no body
  dependencies

IR source-match lowering uses only the decision plan. It emits Bool patterns
as ordinary tag or union-id tests for the resolved `True` or `False`
tag. There is no separate Bool-literal decision node after row finalization.
Tag tests, byte-union tests, integer tests, decimal/float tests, string literal
comparisons, and list-length probes all consume published decision nodes.
Decimal and float literal tests lower through explicit equality calls for the
literal type. String literal comparisons reference a `ProgramLiteralId`; IR does
not carry source literal ids. Guards lower only after the path values and
representative binders required by that guard have been materialized. Path-local
temporary caches are scoped to one decision path and must not be reused across
different scrutinees, branches, guards, or join contexts. All successful leaves
assign the shared result local through the published result join.

Pattern path materialization is path-local. Record-rest projection constructs a
row-finalized record shape that excludes exactly the fields consumed by the
pattern. List-rest projection materializes the rest list with explicit RC
ownership and tail cleanup. Opaque and newtype payload projection consumes the
published interface capability or nominal declaration record that authorizes the
projection. Guards run after every path value and representative binder required
by that guard has been materialized, and before body-only binders are used.

Path-local temporary caches are limited to one decision path in one match. They
may avoid re-reading the same field or payload while lowering that path, but
they must not cross alternatives, guards, bodies, nested matches, join
contexts, or source-match ids. Every pattern binder receives its mandatory
value transform before the guard or body observes it.

### ARC Token Model

ARC insertion tracks owned tokens, not source variables. A local can have zero
or one owned token in a given control-flow state. Calls and low-level operations
publish transfer metadata. The metadata is bitset-like: every argument index
that can be consumed, retained, released, considered for runtime uniqueness, or
aliased by the result appears in the corresponding set, and absent bits mean
the operation has no such effect for that argument.

```zig
const LowLevelRcEffect = struct {
    allocates_result: bool,
    may_retain: bool,
    may_release: bool,
    runtime_uniqueness_candidate_args: Span(ArgId),
    consume_args: Span(ArgId),
    retain_args: Span(ArgId),
    retain_result: bool,
    result_aliases_consumed_args: Span(ResultAlias),
};
```

If a consumed argument token is still needed by the caller, ARC emits an
`incref` before the call and transfers the new token. If a result aliases a
consumed token, ownership of the token moves to the result. Duplicate arguments,
unowned arguments, borrowed projection results, joins, switch continuations,
early returns, loop breaks, loop continues, and branch-local uses are handled by
the same token accounting.

Ordinary direct calls use the same token-transfer question as low-level calls:
each callee parameter that consumes an owned value needs one token. If the same
owned value appears in two consuming argument positions, ARC retains before the
call so each parameter receives its own token. If an argument is an unowned
alias, such as `ForListElementSource.aliases_iterable_element`, ARC retains or
copies before a consuming call and keeps the iterable owner live through the
call edge. “Used after call” is a CFG reachability property over jumps, joins,
loops, keep-sets, shared continuations, early returns, and switch edges; it is
not determined by local statement order alone. Switch-continuation summaries
ask this same token-transfer question before final rewrite so branch-local
retains and cleanups match the shared continuation's incoming ownership state.

Examples:

- `List.append` consumes the list candidate, may retain the appended element,
  and may return an alias of the consumed list when runtime uniqueness succeeds.
- `List.concat` consumes or retains both list operands according to the
  operation contract and records which consumed operand can be returned as the
  result when uniqueness succeeds.
- `List.drop_at`, `List.sublist`, and list split operations publish whether the
  result aliases the consumed list storage, which element ranges are retained,
  and which consumed tokens must be cleaned before suffix/prefix rewrite.
- `List.reserve` consumes the list candidate, may allocate a replacement, and
  records the result alias of the consumed token when capacity is already
  sufficient.
- `Str.concat` consumes or retains string inputs according to the published
  operation contract and returns either a new allocation or an alias named by
  `result_aliases_consumed_args`.
- `Box.unbox` can retain the projected payload result while keeping the outer
  box cleanup independent.
- `Box.box` allocates a result, retains or copies the payload into the box, and
  consumes no caller token unless the operation contract explicitly says so.

`result_aliases_consumed_args` is always a subset of `consume_args`. If an
operation wants to say that the result may alias an input that is not consumed,
it must model that as retain/copy behavior instead. Consumed-token cleanup
ordering is published by the operation contract before any internal suffix,
prefix, or mutable-storage rewrite. ARC follows that order mechanically.

LIR writes define ownership transfer. `SetLocalWriteMode` or an equivalent
record says whether a write initializes a fresh local, replaces an initialized
mutable local, moves an owned token, copies a value while retaining it, or
writes a branch/join result slot:

```zig
const SetLocalWriteMode = union(enum) {
    initialize,
    replace_mutable: ReplaceMutableMode,
    move_token: SourceTokenId,
    copy_and_retain,
    initialize_branch_result: BranchResultSlotId,
    initialize_join_param: JoinParamId,
};
```

Branch-result locals are initialized exactly once along each branch before the
shared continuation. Mutable replacement emits cleanup of the replaced token
before installing the new token. Join params receive initialized tokens from incoming
edges, not borrowed aliases to predecessor locals. `assign_ref.local` records
move-vs-copy semantics explicitly and transfers the source token when it is a
move. Shared continuations consume per-edge summaries, and branch-local cleanup
runs before the edge enters the continuation.

When a shared continuation has already been rewritten during the current ARC
pass, internally inserted RC statements are part of the continuation summary.
Backends see only the final explicit statements.

Loop element aliases are explicit:

```zig
const ForListElementSource = enum {
    aliases_iterable_element,
    owns_copied_element,
};
```

`aliases_iterable_element` means the loop element local borrows storage owned
by the iterable token. ARC must not decref the element as an owned value. If the
body needs to retain the element beyond the borrowed use, lowering emits an
explicit retain/copy that creates a separate owned token. Loop keep-sets track
the iterable token across calls, branches, and continues so element aliases are
not used after the iterable has been cleaned up.

`decref` is the correctness baseline. ARC may emit `free` only when a direct
free is mechanically justified by the layout, ownership token, and current
refcount path; otherwise it emits `decref` and lets runtime helpers decide
whether final cleanup is required. This keeps the first ARC implementation
simple and correct while preserving a place for later optimizations.

Zero-sized containers can still be refcounted. `List(Zst)`, `Box(Zst)`, static
boxed values, and recursive containers with zero-sized payloads all use the
published layout and allocation metadata to decide whether an RC token exists.
The element size being zero is not permission to skip the outer allocation's
refcount operations.

RC cleanup traversal must be stack-safe. Destructors and runtime helpers for
deep lists, recursive tag unions, boxed children, and nested static/heap data
use iterative work queues or bounded recursion where the runtime representation
requires traversal. Compiler-inserted ARC does not emit recursive cleanup loops
by walking source types.

Mutation barriers are explicit. Reassigning a refcounted local emits cleanup of
the overwritten token before the new token is installed. In-place mutation uses
the runtime uniqueness check `refcount == 1`; the compiler does not prove
static uniqueness in this pass.

Keep-set reachability is flow-sensitive. At a call, branch, loop, switch,
early-return, or join, ARC computes the tokens that must remain live after the
edge and emits cleanup only for tokens outside that keep set. Switch and
source-match continuations publish per-edge summaries so branch-local RC work
is not duplicated or skipped when the continuation is shared.

ARC insertion itself is stack-safe. Straight-line `CFStmtId` chains, generated
helper chains, and shared continuations are walked with explicit buffers and
iterative loops. The pass mutates forward ownership state, records patch sites,
and then splices inserted `incref`, `decref`, and optional `free` statements by
updating next pointers. It does not recurse per statement, per generated helper,
or per continuation edge, so long generated string-concat/glue chains and deep
match lowering cannot overflow the compiler stack.

### Low-Level Operations And Value Flow

Low-level operations have separate lambda-solved and executable records:

```zig
const LambdaSolvedLowLevelCall = struct {
    op: LowLevelOpId,
    arg_exprs: Span(ExprId),
    arg_values: Span(ValueInfoId),
    result: ValueInfoId,
    source_constraint_ty: TypeVarId,
    rc_effect: LowLevelRcEffect,
    value_flow: LowLevelValueFlowSignatureId,
};

const ExecutableLowLevelCall = struct {
    op: LowLevelOpId,
    arg_exprs: Span(ExprId),
    arg_values: Span(ExecutableValueRef),
    result_ty: ExecTypeId,
    result_tmp: TempId,
    abi: LowLevelAbiKey,
    rc_effect: LowLevelRcEffect,
};
```

`LowLevelValueFlowSignatureId` is a lambda-solved representation-solving input.
It records how a low-level operation contributes representation edges before
executable MIR exists:

```zig
const LowLevelValueFlowSignature = union(enum) {
    no_value_flow,
    flows: struct {
        op: LowLevelOpId,
        arg_tys: Span(TypeId),
        result_ty: TypeId,
        edges: Span(LowLevelValueFlowEdge),
        box_boundary: ?BoxBoundaryIntrinsic,
    },
};

const LowLevelValueFlowEdge = union(enum) {
    arg_to_result: struct {
        arg: u32,
        projection: ValueProjectionPath,
    },
    arg_to_result_projection: struct {
        arg: u32,
        arg_projection: ValueProjectionPath,
        result_projection: ValueProjectionPath,
    },
    produced_from_args: struct {
        args: Span(u32),
        result_projection: ValueProjectionPath,
    },
};
```

Executable MIR consumes already-solved executable representations and discards
the value-flow id. IR, LIR, ARC, backends, interpreters, and executable-only
materialization nodes do not read, manufacture, cache, or propagate
lambda-solved value-flow ids.

Executable-only low-level calls can be generated while materializing constants,
promoted captures, erased capture records, or executable value transforms. They
are not lambda-solved expressions and therefore do not have `ValueInfoId`,
representation roots, or `LowLevelValueFlowSignatureId` identities. Their
correctness comes from the sealed executable inputs that requested them, such as
const materialization plans, erased capture materialization plans, executable
value transforms, erased call signatures, finalized row ids, and committed
layout graph records.

`Box.box` and `Box.unbox` are explicit low-level value-flow boundaries while
they are lambda-solved expressions:

```zig
const BoxBoundaryIntrinsic = struct {
    boundary: BoxBoundaryId,
    direction: BoxErasureDirection,
};
```

`Box.box` creates a `BoxBoundaryId`, links the payload argument to the boxed
payload representation, and creates the erased boxed-payload requirement when
the payload contains callable slots. `Box.unbox` creates a `BoxBoundaryId`,
links the boxed payload representation to the result, and gives function slots
in the result erased callable provenance from that boundary.

Every low-level operation that can move, retain, release, project, or produce
non-primitive values while it is still lambda-solved publishes a complete
value-flow signature. Pure numeric and scalar comparison operations can use
`no_value_flow`.

Required low-level signatures are explicit. Examples:

- `List.get_unsafe : List(a), U64 -> a` publishes a borrowed-element projection
  with `arg_to_result_projection` from the list element to the result and a
  retained result when the result is refcounted.
- `List.set : List(a), U64, a -> List(a)` consumes the list candidate, retains
  the new element when needed, publishes an element-flow edge from the new value
  into the result list, and publishes whether the result aliases the consumed
  list token.
- `List.append`, `List.prepend`, `List.concat`, and list split operations
  publish consumed list tokens, retained element/result relationships, and
  result aliasing precisely.
- `Str` operations publish result provenance: new allocation, borrowed slice,
  alias of an input token, scalar result, or produced-from-args string data.
- `Box.box : a -> Box(a)` creates a `BoxBoundaryId`, retains/copies the payload
  into the box, and records boxed-payload representation flow.
- `Box.unbox : Box(a) -> a` creates a `BoxBoundaryId`, projects from boxed
  storage, and records retained result behavior for refcounted payloads.
- call-only intrinsics publish procedure boundary requirements and are not
  representable as ordinary value constructors.

Executable-only low-level calls require authorization from sealed executable
inputs. Valid inputs include const materialization plans, erased capture
materialization plans, executable value transforms, proc-value erase plans,
erased call signatures, boxed boundary records, executable type payload refs,
callable-set descriptors, finalized row ids, and committed layout graph
records. If no sealed input authorizes an executable-only low-level call, the
call is a compiler bug.

### Compile-Time Finalization Details

Compile-time execution starts from explicit root/reification records:

```zig
const ComptimeRoot = union(enum) {
    const_root: ConstRef,
    callable_binding_root: CallableBindingInstantiationKey,
    expect_root: ExpectRootRef,
};

const ComptimeRootResult = union(enum) {
    const_value: ConstInstanceRef,
    callable_binding: CallableBindingInstanceRef,
    diagnostics_only,
};

const ConstGraphReificationPlan = struct {
    root: ConstRef,
    requested_ty: ConcreteSourceTypeRef,
    interpreter_entry: LirProcSpecId,
    output: ConstMaterializationPlanRef,
};

const CallableLeafReificationPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    leaf: CallableLeafTemplateRef,
    result: CallableResultPlanRef,
};
```

These records tie checked/MIR roots to LIR interpreter entrypoints and to the
reified const or callable outputs that become artifact data. The interpreter
returns logical values; reification records decide whether those values publish
const instances, callable binding instances, diagnostics, or promoted callable
outputs.

Compile-time finalization separates availability from concrete use:

```zig
const AvailabilityUse = union(enum) {
    local_root: RootRequestId,
    imported_template: ImportedTemplateRef,
};

const ConcreteValueUse = union(enum) {
    const_instance: ConstInstantiationKey,
    callable_binding: CallableBindingInstantiationKey,
    procedure: ProcedureCallableRef,
};
```

Availability means a producer must be present. Concrete use means this artifact
needs a concrete instance. Dependency summaries record both. A concrete
`ConstInstantiationRequest` or `CallableBindingInstantiationRequest` carries a
canonical key plus a `ConcreteSourceTypeRef` payload. The requesting artifact
owns the resulting concrete instance.

`ConstEvalTemplate` and `CallableEvalTemplate` are reusable checked entry
templates. They are evaluated only for concrete requests. Lowering performed
for compile-time evaluation may cache MIR/LIR work under the exact
specialization key, but that cache entry is not a final binary root until a
runtime/export/materialization path requests it.

Top-level callable finalization has three cases:

```roc
inc = |x| x + 1
also_inc = inc
also_id = |x| x
choose = |cond| if cond then inc else also_id
recorded = { f: inc }
```

1. Direct function or lambda declarations publish procedure bindings directly.
2. Function-typed expression roots such as `also_inc`, `also_id`, or
   `choose(True)` are either templated as callable bindings or evaluated for a
   concrete request, then published through `CallableBindingInstantiationStore`.
3. Non-function constants such as `{ f: inc }` publish const instances whose
   materialization graph contains callable leaves.

None of these cases creates a runtime top-level thunk or global callable value.
Only runtime/export/materialization demand decides which callable leaves become
promoted procedures or erased wrappers.

Callable binding instantiations are stored explicitly:

```zig
const CallableBindingInstantiationStore = struct {
    rows: Map(CallableBindingInstantiationKey, CallableBindingInstantiationState),
};

const CallableBindingInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: CallableBindingInstance,
};

const CallableBindingInstance = struct {
    key: CallableBindingInstantiationKey,
    owner: CheckedModuleArtifactKey,
    body: CallableBindingInstanceBody,
    dependency_summary: ComptimeProcDependencySummaryRef,
    final_callable: ProcedureCallableRef,
    private_comptime_root: ?ExecutableSpecializationKey,
    promotion_plan: ?CallablePromotionPlanId,
    promotion_output: CallablePromotionOutput,
};

const CallableBindingInstanceBody = union(enum) {
    direct: DirectCallableBindingInstance,
    evaluated: EvaluatedCallableBindingInstance,
};

const DirectCallableBindingInstance = struct {
    binding: ProcedureBindingRef,
    template: CallableProcedureTemplateRef,
};

const EvaluatedCallableBindingInstance = struct {
    executable_root: CallableBindingExecutableRoot,
    result_plan: CallableResultPlanId,
    promotion_plan: ?CallablePromotionPlanId,
    promotion_output: CallablePromotionOutput,
};

const CallablePromotionOutput = union(enum) {
    existing_procedure: ProcedureCallableRef,
    promoted_procedure: PromotedProcedureRef,
};
```

Direct instances point at an already published checked procedure template.
They do not contain fake result plans, fake promotion plans, fake executable
roots, fake runtime closure allocations, or fake alias records. Evaluated
instances point at the private compile-time executable root that produced the
callable value and record the concrete `CallableResultPlan`, optional
`CallablePromotionPlan`, and `CallablePromotionOutput`. A sealed instance names
its final `ProcedureCallableRef`, concrete dependency summary, optional
promotion plan, promotion output, and owner artifact. Later stages do not
reinterpret the original expression to decide whether the callable was direct
or evaluated. The requesting artifact owns imported generic callable binding
instances; instantiating an imported callable eval template must not mutate the
exporting artifact or rerun already-published imported roots.

Generic constants use either a value-graph template or an eval template:

```zig
const GenericConstTemplate = union(enum) {
    value_graph: ConstValueGraphTemplateRef,
    eval: ConstEvalTemplateRef,
};
```

A `ConstValueGraphTemplate` is complete checked semantic data that can be
instantiated per concrete use. A `ConstEvalTemplate` is required when the
constant contains inline lambdas, control flow, low-level calls, or other
runtime-like computation whose concrete value graph is not known before
evaluation. The compiler must not invent fake lifted template variants or defer
ownership to a future mono specialization. The checked artifact publishes the
generic template shape and each concrete use instantiates that template under a
concrete `ConstInstantiationKey`.

Function-typed expression roots use callable eval templates and entry wrappers:

```zig
const CallableEvalTemplate = struct {
    id: CallableEvalTemplateId,
    root: ComptimeRootId,
    checked_fn_root: CheckedTypeRoot,
    dependency_closure: ImportedTemplateClosureView,
};

const EntryWrapper = struct {
    id: EntryWrapperId,
    root: ComptimeRootId,
    body_expr: CheckedExprId,
    requested_return_slot: CheckedTypeRoot,
};
```

The wrapper requested return slot is the source of truth for evaluating the
root. It determines the concrete callable result endpoint, not the shape of the
expression after lowering. Imported callable-eval bindings select both the
`CallableEvalTemplate` and the `EntryWrapper` from the exporting artifact's
closure. Reservations for direct calls, `proc_value`, finite callable members,
and promoted wrappers carry that selected closure forward. A
`CallableEvalTemplateId` alone is not authorization to read arbitrary checked
bodies or private helper templates in the exporting artifact.

The top-level value table is reserved before compile-time roots run, so
dependencies can refer to stable top-level value refs. Published artifacts
contain const refs, procedure bindings, generic const templates, callable eval
templates, and concrete instantiation stores. They do not contain pending
compile-time roots.

The compile-time root table tracks source roots and their publication state:

```zig
const CompileTimeRootTable = struct {
    roots: Span(CompileTimeRootRecord),
};

const CompileTimeRootRecord = struct {
    id: ComptimeRootId,
    source: CompileTimeRootSource,
    payload: CompileTimeRootPayload,
    dependency_summary_request: ?ComptimeDependencySummaryRequestId,
};

const CompileTimeRootPayload = union(enum) {
    pending_const,
    pending_callable,
    const_value: ConstInstanceRef,
    callable_binding: CallableBindingInstanceRef,
    expect: ExpectRootRef,
};
```

Dependency summaries are sparse over `CompileTimeRootId`. Selected compile-time
roots require filled dependency summaries. Unselected pending roots may remain
summary-less for exported templates, diagnostics, or future concrete
instantiation. A source `expect` has `CompileTimeRootPayload.expect` from the
start and is not an ordinary compile-time constant-evaluation root while
compiling, building, gluing, or otherwise publishing a non-test artifact. It
becomes runnable only when the test runner selects explicit `.test_expect` root
requests. Treating `.test_expect` as a selected compile-time root during an
ordinary build is a compiler bug because it would evaluate test-only code and
require a dependency summary that no ordinary publication path should produce.

Compile-time roots are ordered by an explicit dependency graph:

```zig
const CompileTimeRootDependencyGraph = struct {
    nodes: Span(CompileTimeRootNode),
    edges: Span(CompileTimeRootEdge),
};

const CompileTimeRootNode = union(enum) {
    compile_time_constant_root: ComptimeRootId,
    callable_binding_root: ComptimeRootId,
    expect_root: ComptimeRootId,
};

const CompileTimeRootEdge = struct {
    from: ComptimeRootId,
    to: CompileTimeRootPrerequisite,
    reason: CompileTimeRootDependencyReason,
};

const CompileTimeRootPrerequisite = union(enum) {
    local_root: ComptimeRootId,
    imported_const: ImportedConstRef,
    imported_callable_binding: ImportedCallableBindingRef,
    imported_procedure: ImportedProcedureRef,
    semantic_instantiation: SemanticInstantiationProcedureKey,
};

const CompileTimeRootDependencyReason = union(enum) {
    top_level_const_use,
    callable_binding_use,
    imported_artifact_value,
    reachable_executable_body,
    callable_capture,
    callable_result,
    const_graph_leaf,
    erased_callable_promotion,
};
```

`from` is the dependent root. `to` is the prerequisite local root or imported
top-level value that must be available first. Dependency reasons name why the
edge exists, such as top-level constant use, callable binding use, imported
artifact value, reachable executable procedure body, callable capture, or erased
callable promotion.

Dependency graph validation is sparse over selected roots. A selected
compile-time root must have a filled dependency summary and every prerequisite
edge required to make its interpreted result available. An unselected pending
root may remain summary-less when it exists only to support an exported generic
template, diagnostics, or a future concrete request. A dependency edge from a
runtime summary to a const/callable instance records concrete availability; it
does not make that runtime root a compile-time evaluation root.

Checking finalization uses a summary-only MIR-family path before runnable
lowering. This path runs far enough to produce sealed call-site records,
callable emission plans, constant-graph reification plans, callable-result
plans, and dependency summaries. It is not runnable MIR and does not emit
executable MIR, IR, LIR, runtime roots, object code, or backend input.

Summary-only lowering has an explicit mode and placeholder:

```zig
const LoweringMode = enum {
    comptime_dependency_summary,
    runnable_compile_time,
    runtime,
};

const SummaryOnlyExpr = union(enum) {
    pending_local_root: ComptimeRootId,
};

const SummaryOnlyDispatch = union(enum) {
    pending_local_root_call,
};
```

`pending_local_root` is valid only while `LoweringMode` is
`comptime_dependency_summary`. It may appear in mono, row-finalized mono,
lifted MIR, and lambda-solved MIR only because dependency summarization stops
there. The dependency-summary collector consumes it as
`ComptimeAvailabilityUse.local_root`. It is forbidden in executable MIR, IR,
LIR, interpreter lowering, runtime lowering, backend input, and runtime images.

Function values that originate from a pending local root carry a summary-only
pending-local-root-origin marker. A `call_value` whose callee has that marker
uses `pending_local_root_call`, records the availability dependency from the
original `pending_local_root` expression, and emits no executable call
dependency. After root ordering, runnable lowering of the same source consumes
the sealed `ConstRef`, `CallableBindingInstance`, or promoted procedure instead
of this placeholder.

Private aggregate LIR roots are allowed only as `comptime_only` interpreter
entrypoints. They are excluded from runtime root lists, backend input, object
emission, generated entry metadata, and runtime images. Debug verification
asserts that no `comptime_only` proc reaches runtime codegen; release builds use
`unreachable` for that malformed pipeline state.

Procedure summaries name concrete executable dependencies:

```zig
const ComptimeProcDependencySummary = struct {
    proc: ExecutableSpecializationKey,
    availability_values: Span(AvailabilityUse),
    concrete_values: Span(ConcreteValueUse),
    call_deps: Span(ComptimeCallDependency),
    const_graph_deps: Span(ConstGraphDependency),
    callable_result_deps: Span(CallableResultDependency),
    callable_leaf_deps: Span(CallableLeafDependency),
    erased_callable_deps: Span(ErasedCallableDependency),
};

const ComptimeCallDependency = union(enum) {
    call_proc: ExecutableSpecializationKey,
    call_value_finite: FiniteCallableCallDependency,
    call_value_erased: ErasedCallableCallDependency,
};

const ConstGraphDependency = struct {
    instance: ConstInstantiationKey,
    availability: Span(AvailabilityUse),
    concrete_uses: Span(ConcreteValueUse),
};

const CallableResultDependency = struct {
    binding: CallableBindingInstantiationKey,
    result_plan: CallableResultPlanRef,
};

const CallableLeafDependency = union(enum) {
    finite_leaf: CallableLeafInstanceRef,
    erased_leaf: ErasedCallableCodeDependency,
};

const ErasedCallableDependency = union(enum) {
    direct_proc_value: ErasedDirectProcCodeDependency,
    finite_set_adapter: ErasedFiniteAdapterDependency,
    supplied_erased_value: SuppliedErasedValueDependency,
};
```

The exact record names may differ, but summaries include direct calls, finite
callable-set calls, erased callable calls, constant graph dependencies,
callable result dependencies, and callable leaves. Finite callable-set calls
name every reachable member executable specialization, including singleton
sets. Erased callable calls name explicit erased code dependency data and
capture availability/concrete-value dependencies.

Already-erased supplied callables have a distinct code dependency:

```zig
const ErasedCallableCodeDependency = union(enum) {
    direct_proc_value: ErasedDirectProcCodeDependency,
    finite_set_adapter: ErasedFiniteAdapterDependency,
    supplied_erased_value: SuppliedErasedValueDependency,
};
```

`supplied_erased_value` is used when the summarized procedure calls an erased
callable supplied as a parameter, capture, or local value. The consuming
procedure summary records the erased call signature and normal capture
dependencies, but does not own the callable's concrete code identity. The
producer of the erased value owns the direct-proc or finite-adapter dependency.

`ConstEvalTemplate` and `CallableEvalTemplate` are reusable checked entry
templates. A concrete request is summarized by running that exact request
through summary-only MIR-family lowering in the requesting artifact.

Value-graph templates are different. They are already explicit checked-artifact
semantic data. Instantiating a concrete value graph writes dependency summary
entries while constructing/remapping each value node into the requesting
artifact. There is no clone-then-scan step and no post-construction graph walk
whose purpose is to rediscover callable leaves.

Eval-template const dependency summaries are accumulated while the interpreter
result is reified into the final `CompileTimeValueStore` graph. The dependency
summary records the final callable leaf that is actually written to the value
graph, after promotion or sealing has selected artifact-owned procedure refs. It
must not be produced by a later walk over a pre-reification
`ConstGraphReificationPlan`, because that can record pre-promotion local or
lifted members instead of the sealed callable leaf.

```zig
const ProcedureCallableDependency = struct {
    proc_value: ProcedureCallableRef,
    source_fn_ty_payload: CheckedTypePayloadRef,
    lifted_owner_source_fn_ty_payload: ?CheckedTypePayloadRef,
};
```

If a const-backed callable leaf reaches lambda-solved and the remapped
`ProcedureCallableRef` is absent from lifted MIR, the bug is upstream: mono did
not consume the checked-artifact dependency summary correctly or checking
finalization failed to publish the finite member procedure dependency.
Lambda-solved must not repair this by scanning source declarations or inventing
a procedure reservation after lifting.

Prepared lowering and final runtime emission are separate. Preparing a
specialization means exact-key stage output already exists and may be reused.
Emitting a specialization means it is reachable from final runtime roots,
provided data exports, platform-required roots, finite callable-set members,
erased adapters, or other published runtime references. A prepared cache entry
does not by itself place anything in the final binary or runtime image.

Dependency summaries run in two purposes:

```zig
const ComptimeDependencySummaryPurpose = enum {
    compile_time,
    runtime,
};

const CompileTimeEvaluationPayload = union(enum) {
    const_graph: ConstGraphReificationPlanRef,
    callable_result: CallableResultPlanRef,
    diagnostics_only,
};
```

Compile-time summary mode can publish `CompileTimeEvaluationPayload` because
the root will be evaluated through the LIR interpreter and reified into const,
callable, or diagnostic artifact data. Runtime summary mode runs for concrete
runtime, test, REPL, development, tool, platform, and glue roots before artifact
publication only to seal semantic dependencies. A runtime summary seals
concrete `ConstInstantiationKey`, `CallableBindingInstantiationKey`, and
`ProcedureCallableRef` dependencies, but it does not produce compile-time
payloads, retained executable MIR, IR, LIR, backend input, interpreted results,
runtime root lists, top-level thunks, global initializer procedures, or runtime
callable objects.

For example:

```roc
OutOfBounds : [OutOfBounds]
OutOfBounds = OutOfBounds

nth = |l, i| {
    match List.get(l, i) {
        Ok(e) => Ok(e)
        Err(OutOfBounds) => Err(OutOfBounds)
    }
}

main = nth(["a"], 2)
```

`main` is a runtime root, but the `nth(List(Str), I64)` specialization still
uses the `OutOfBounds` constant at the concrete tag payload type required by
the `Err` branch. Runtime summary mode records both availability of the
`OutOfBounds` template and the concrete const instance required by runnable
lowering. Checking finalization collects compile-time and runtime summaries,
satisfies local compile-time availability in dependency order, evaluates
selected compile-time roots, and then ensures runtime summaries' concrete
dependencies before publication. Runnable post-check lowering consumes sealed
rows only.

Prepared reuse keys include every input that can affect the stage: checked
artifact identities, compiler artifact hash, mono specialization key, concrete
source type payload key, representation/executable specialization key, and
target/layout inputs for target-specific stages. A stage without a complete key
is not reusable across lowering requests.

The checked artifact cache stores one complete target-independent artifact.
`CompileTimeValueStore`, `ConstInstantiationStore`,
`CallableBindingInstantiationStore`, and semantic-instantiation procedure data
restore together with the checked artifact or the entire cache entry misses.
There is no independently accepted compile-time value sidecar.

Top-level values are resolved only through the top-level value table:

```zig
const TopLevelValueKind = union(enum) {
    const_template: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
    imported_const: ImportedConstRef,
    imported_procedure: ImportedProcedureRef,
    hosted_procedure: HostedProcedureRef,
    platform_required_const: PlatformRequiredConstUse,
    platform_required_procedure: PlatformRequiredProcedureUse,
    promoted_procedure: PromotedProcedureRef,
};

const TopLevelValueEntry = struct {
    name: CanonicalNameId,
    source_ty: ConcreteSourceTypeRef,
    kind: TopLevelValueKind,
};

const TopLevelValueTable = struct {
    entries: Span(TopLevelValueEntry),
};
```

This table is the only post-check lookup table for top-level values. Later
stages do not search declarations, scan exports, inspect source patterns, or
infer top-level value kind from type shape.

Function-typed top-level values always appear here through
`procedure_binding`. If the binding is generalized and requires concrete
callable evaluation, the referenced `TopLevelProcedureBinding` has
`ProcedureBindingBody.callable_eval_template`. This keeps every callable root
behind the same procedure-binding boundary and prevents later stages from
treating callable eval templates as a separate top-level value category.

Promoted procedure provenance records why a callable result became a procedure:

```zig
const PromotedProcedureProvenance = union(enum) {
    local_callable_root_result: LocalComptimeRootId,
    local_const_root_callable_leaf: ConstGraphLeafId,
    callable_binding_instance_result: CallableBindingInstantiationKey,
    const_instance_callable_leaf: ConstInstantiationKey,
    private_capture_callable_leaf: PrivateCaptureLeafId,
};
```

Promotion is demand-driven. A callable evaluated during compile-time execution
can be cached as prepared work, but it becomes a promoted procedure only when a
runtime/export/materialization path needs that callable as a procedure binding
or boxed-erased wrapper target.

Semantic-instantiation procedures are owned by a checked-artifact table:

```zig
const SemanticInstantiationProcedureTable = struct {
    rows: Span(SemanticInstantiationProcedureRow),
};

const SemanticInstantiationProcedureRow = struct {
    key: SemanticInstantiationProcedureKey,
    owner: union(enum) {
        const_instance: ConstInstantiationKey,
        callable_binding: CallableBindingInstantiationKey,
        promoted_procedure: PromotedProcedureRef,
    },
    procedure: ProcedureTemplateRef,
};
```

The table is populated during checking finalization. Later stages may request a
procedure already named by this table; they do not allocate new checked
procedure templates or create checked procedure template ids after artifact
publication.

Constants have templates, requests, stores, states, and instances:

```zig
const ConstOwner = union(enum) {
    local_top_level: TopLevelDeclKey,
    imported_top_level: ImportedConstRef,
    platform_required: PlatformRequiredConstUse,
    const_instance: ConstInstantiationKey,
};

const ConstRef = struct {
    owner: ConstOwner,
    template: ConstTemplateRef,
};

const ConstTemplate = struct {
    owner: ConstOwner,
    source_ty: ConcreteSourceTypeRef,
    eval_template: ConstEvalTemplateRef,
    value_graph_template: ConstValueGraphTemplateRef,
};

const ConstEvalTemplate = struct {
    template: ConstTemplateRef,
    checked_root: CheckedExprId,
    checked_payload_refs: Span(CheckedTypePayloadRef),
    dependency_closure: ImportedTemplateClosureView,
};

const ConstValueGraphTemplate = struct {
    template: ConstTemplateRef,
    nodes: Span(ConstValueGraphTemplateNode),
    checked_payload_refs: Span(CheckedTypePayloadRef),
    callable_leaf_templates: Span(CallableLeafTemplateRef),
};

const ConstInstantiationKey = struct {
    template: ConstTemplateRef,
    requested_source_ty: ConcreteSourceTypeRef,
    requester: CheckedModuleArtifactKey,
};

const ConstInstantiationRequest = struct {
    key: ConstInstantiationKey,
    consumer_endpoint_ty: ConcreteSourceTypeRef,
};

const ConstInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: ConstInstance,
};

const ConstInstance = struct {
    key: ConstInstantiationKey,
    producer_source_ty: ConcreteSourceTypeRef,
    schema: ComptimeSchemaId,
    value: ComptimeValueNodeRef,
    dependency_summary: ?ComptimeDependencySummaryRef,
    reification_plan: ?ConstGraphReificationPlanRef,
    generated_procedures: Span<SemanticInstantiationProcedureRef>,
};

const ConstInstantiationStore = struct {
    owner: CheckedModuleArtifactKey,
    instances: Map(ConstInstantiationKey, ConstInstantiationState),
};
```

The const template key identifies reusable checked semantic material. The
concrete payload identifies one instantiation. A consumer endpoint may request a
narrower or bridged view of a wider stored producer value. The producer type is
kept on the const instance; consumers project or bridge through explicit
materialization records instead of mutating the stored value graph.
When available, dependency summaries and reification plans are part of the
final evaluated instance. They are not recomputed by scanning value graphs
after evaluation. `generated_procedures` are concrete semantic-instantiation
outputs owned by the const instance's artifact; later stages consume those
refs instead of discovering generated helpers by walking declarations or
interpreter values.

Source-visible genericness is determined by the producer's checked source type,
not by builtin internals. For example, a producer whose visible type is
`List(MyTag)` is concrete for `MyTag` even if the builtin `List` implementation
uses internal generic helper types. Builtin implementation genericness must not
turn a concrete source constant into a generalized public constant.

Const reification records nominal declaration edges explicitly. When a nominal
value is reified, the materializer uses the defining artifact's exported nominal
representation or the instantiated declaration template named by the
const/callable template closure. It must not choose a backing from the
consumer-constrained occurrence type or from the local alias spelling at the
use site.

Row-extension normalization is part of const reification. Record and tag
extension chains are normalized into sealed row endpoints before materialized
nodes are compared with executable names. Duplicate fields/tags are rejected
during checking finalization. A `.record_unbound` endpoint must be sealed before
materialization. Tag plans are ordered by executable logical tag index, not by
source extension-chain order.

Const schemas are normalized before comparison with executable payloads. Record
field plans use canonical field labels after artifact-name remapping. Tag union
plans use canonical tag labels, runtime discriminant/logical tag indexes, and
payload logical indexes:

```zig
const ConstTagPlan = struct {
    variants: Span(ConstTagVariantPlan),
};

const ConstTagVariantPlan = struct {
    tag: CanonicalTagLabelId,
    logical_tag_index: u32,
    payloads: Span(ConstTagPayloadPlan),
};

const ConstTagPayloadPlan = struct {
    logical_payload_index: u32,
    node: ConstMaterializationNodeRef,
};
```

Parent executable payload keys are authoritative, even when a value has no
representative child metadata because a branch is inactive, a list is empty, or
a wrapper was physically erased. Verifiers reject missing variants, duplicate
logical indexes, payload arity mismatch, canonical ordering disagreement, and
any attempt to infer tag/field order from source extension-chain order.

Const materialization is a graph:

```zig
const ConstMaterializationPlan = struct {
    root: ConstMaterializationNodeRef,
    nodes: Span(ConstMaterializationNode),
};

const ConstMaterializationNode = union(enum) {
    scalar: ScalarConstNode,
    string: StringConstNode,
    list: ListConstNode,
    record: RecordConstNode,
    tuple: TupleConstNode,
    tag: TagConstNode,
    box: BoxConstNode,
    alias: AliasConstNode,
    nominal: NominalConstNode,
    finite_callable_leaf: FiniteCallableLeafNode,
    erased_callable_leaf: ErasedCallableLeafNode,
    recursive_ref: RecursiveConstRefNode,
};
```

Executable MIR resolves every `const_instance` expression before IR. No
`const_instance` operation reaches IR, LIR, ARC, a backend, an interpreter, or a
runtime image. If executable MIR cannot resolve the instance from the published
store, that is a compiler bug.

Materialization runs with explicit context:

```zig
const ConstMaterializationContext = struct {
    owner_key: ConstOwner,
    canonical_name_view: ArtifactNameResolver,
    value_store: *const CompileTimeValueStore,
    plan_store: *const ConstMaterializationPlanStore,
    executable_name_remap: ArtifactNameResolver,
};
```

Artifact-local names are remapped before comparison with executable names.
Name equality across artifacts is always resolver-mediated.

Transparent aliases are materialized as their backing value without a runtime
nominal wrapper. Nominal/newtype values preserve nominal identity through the
layout/materialization boundary and use the published nominal representation
before lowering the backing. The distinction is explicit in the materialization
node, not rediscovered from display names.

Const reification respects physical erasure of single-child wrappers. If a
logical record or tuple has exactly one child and the committed physical layout
is not `.struct_`, the whole physical value belongs to that one child.
Reification reads the child from the whole payload value and then wraps it back
into the logical record or tuple schema. The same rule applies to newtype-like
wrappers whose physical representation is their backing value. For example:

```roc
ValueCombinationMethod := [Divide, Modulo, Add, Subtract]
Value := [CombinedValue({ combination_method: ValueCombinationMethod })]

main = Value.CombinedValue({ combination_method: ValueCombinationMethod.Add })
```

The source payload under `CombinedValue` is a record, but the physical payload
may be exactly the `ValueCombinationMethod` tag-union layout. Reification still
publishes the logical record schema `{ combination_method: Add }` under the tag.
This is wrapper representation erasure, not special handling for a particular
tag union.

Pure and general materialization modes are separate:

```zig
const ConstMaterializationMode = enum {
    pure_data_only,
    general_const_instance,
};
```

`pure_data_only` rejects callable schemas and can use
`NoReachableCallableSlotsProof` to skip callable walks. `general_const_instance`
permits finite callable leaves and erased callable leaves for a concrete
`ConstInstanceRef`.

Captured finite callables have restricted direct materialization. A finite
callable leaf can materialize directly only when its capture schema is empty.
Captured callable values must already be promoted or exactly erased by a sealed
erased callable record. The materializer does not synthesize capture records by
walking source closures.

`NoReachableCallableSlotsProof` is an explicit proof attached to a pure
constant/materialization request. It uses the interface proof shape described
in `Interface Capability Records`; pure materialization stores only a reference
to that proof plus the root it applies to:

```zig
const PureMaterializationCallableSlotProof = struct {
    root_ty: ConcreteSourceTypeRef,
    proof: NoReachableCallableSlotsProof,
};
```

Only this proof permits a pure materializer to skip callable-slot handling.
Absence of observed callable nodes in a traversal is not a proof.

Compile-time value graph templates may contain callable leaf templates before
interpretation:

```zig
const CallableLeafTemplate = union(enum) {
    finite: FiniteCallableLeafTemplate,
    erased_boxed_callable: ErasedCallableTemplate,
};

const ErasedCallableTemplate = struct {
    sig_template: CheckedTypePayloadRef,
    code_template: ErasedCallableCodeTemplateRef,
    capture_template: ErasedCaptureTemplateRef,
    provenance: BoxErasureProvenance,
};
```

`ErasedCallableTemplate` is template-side semantic data for a generic const or
private capture graph. It becomes an `ErasedCallableLeafInstance` only after a
concrete const/callable request chooses a source function payload, erased ABI,
code dependency, capture materialization, and stable provenance. Generic const
templates may contain erased callable templates; they must not store
post-interpretation sealed values until a concrete instance exists.

Compile-time callable values are explicit logical values:

```zig
const ComptimeCallable = union(enum) {
    finite: ComptimeFiniteCallable,
    erased: ComptimeErasedCallable,
};

const ComptimeFiniteCallable = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    selected_member: CallableSetMemberRef,
    capture_values: Span(ComptimeValueNodeRef),
};

const ErasedCallableCodeRef = union(enum) {
    direct_proc_value: ErasedDirectProcCodeRef,
    finite_set_adapter: ErasedFiniteSetAdapterRef,
};

const ComptimeErasedCallable = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    abi: ErasedFnAbiKey,
    code: ErasedCallableCodeRef,
    capture: ErasedCaptureReificationPlan,
    provenance: BoxErasureProvenance,
};
```

A finite compile-time callable records the selected finite member identity and
its capture values. An erased compile-time callable records erased code,
capture reification, ABI, and provenance. Compile-time callable values are not
thunks and are not runtime global callable values.

Callable result publication uses a plan family:

```zig
const CallableResultPlan = union(enum) {
    finite: FiniteCallableResultPlan,
    erased: ErasedCallableResultPlan,
};

const FiniteCallableResultPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    source_fn_ty_payload: CheckedTypePayloadRef,
    members: Span(CallableResultMemberPlan),
    capture_reification: CaptureSlotReificationPlanRef,
    boundary_source_payload: CheckedTypePayloadRef,
};

const ExecutableSpecializationEndpoint = struct {
    requested_fn_ty: CanonicalTypeKey,
    exec_arg_tys: Span(CanonicalExecValueTypeKey),
    exec_ret_ty: CanonicalExecValueTypeKey,
    callable_repr_mode: CallableReprMode,
    capture_shape_key: CaptureShapeKey,
};

const CallableResultMemberTargetPlan = union(enum) {
    artifact_owned: ExecutableSpecializationKey,
    member_proc_relative: ExecutableSpecializationEndpoint,
};

const CallableResultMemberPlan = struct {
    member: CallableSetMemberId,
    procedure: ProcedureCallableRef,
    member_source_payload: ConcreteSourceTypeRef,
    member_proc_source_fn_ty_payload: CheckedTypePayloadRef,
    member_lifted_owner_source_fn_ty_payload: ?CheckedTypePayloadRef,
    target: CallableResultMemberTargetPlan,
    capture_slots: Span(CaptureSlotReificationPlanRef),
};

const ErasedCallableResultCodePlan = union(enum) {
    materialized_by_lowering: struct {
        code: ErasedCallableCodeRef,
        capture: ErasedCaptureReificationPlan,
    },
    read_from_interpreted_erased_value: struct {
        sig: ErasedCallSigKey,
        value_node: ComptimeValueNodeRef,
    },
};

const ErasedCallableResultPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    source_fn_ty_payload: CheckedTypePayloadRef,
    call_sig: ErasedCallSigKey,
    provenance: NonEmptySpan(BoxErasureProvenance),
    code_plan: ErasedCallableResultCodePlan,
    capture: ErasedCaptureReificationPlan,
    result_ty: CanonicalExecValueTypeKey,
    executable_signature_payloads: ErasedPromotedProcedureExecutableSignature,
};

const ErasedCaptureReificationPlan = union(enum) {
    none,
    static_capture: ConstMaterializationNodeRef,
    private_capture: PrivateCaptureRef,
    interpreted_capture_bytes: InterpretedCaptureBytesRef,
};

const CaptureSlotReificationPlan = union(enum) {
    serializable_leaf: ConstMaterializationNodeRef,
    callable_leaf: CallableLeafInstanceRef,
    callable_schema: CanonicalTypeKey,
    record: Span(CaptureSlotReificationPlanRef),
    tuple: Span(CaptureSlotReificationPlanRef),
    tag_union: Span(CaptureTagReificationPlan),
    list: CaptureSlotReificationPlanRef,
    box: CaptureSlotReificationPlanRef,
    nominal: struct {
        nominal: NominalTypeKey,
        backing: CaptureSlotReificationPlanRef,
    },
    recursive_ref: CaptureSlotReificationPlanRef,
};
```

`materialized_by_lowering` is used when lowering can name the wrapper code and
capture graph directly. `read_from_interpreted_erased_value` is used when the
LIR interpreter produced an erased callable value whose payload must be decoded
through the same erased ABI. Both variants still publish explicit code and
capture dependencies before runtime emission. The promoted wrapper executable
signature payloads are part of the erased result plan because they are built
from the checked boundary source type and erased ABI before interpretation; they
cannot be derived later from an interpreted function pointer.

`CallableResultPlan` and `CaptureSlotReificationPlan` are reserved graphs, not
trees. Recursive callable captures, captures that mention the callable result
being reified, and nested aggregate captures reserve node ids before descending
into children. `recursive_ref` points to a reserved node in the same graph. A
plan is sealed only after every member, aggregate child, nominal backing, and
boxed payload edge has been filled.

Callable result plans are produced before interpretation from
lambda-solved/executable representation data. They carry the exact checked
boundary source payload that made the callable result meaningful. That payload
is semantic data: later finalization must not search checked type stores for a
compatible canonical key, call `rootForKey(source_fn_ty)`, compare display
names, or infer the boundary from the interpreted value's runtime bytes. If a
callable result is finite, every member plan names its exact
`ProcedureCallableRef`, concrete member source payload, member source function
payload, optional lifted-owner source function payload, executable target plan,
and capture-slot reification plan. If a result is erased, the erased result
plan names the source payload, erased signature, non-empty provenance, result
executable type, code-selection plan, capture reification path, and promoted
wrapper executable signature payloads.

`callable_schema` is type-only schema data, not a callable value. It may appear
only for uninhabited or schema-only slots such as the element schema of an empty
`List(I64 -> I64)` capture or the payload schema of an inactive tag variant. It
is invalid as a concrete record field, tuple element, tag payload, box payload,
nominal backing, or alias backing value. `callable_schema` does not count as a
reachable callable value for pure materialization proofs; only `callable_leaf`
represents an actual callable value.

Const-graph builders, capture-slot builders, and callable-result builders run
with explicit callable-set descriptor availability. The legal lookup order is:

1. the owning value context's representation store, when the descriptor belongs
   to that exact solve session
2. the explicit lowering-run descriptor slice passed into the builder

The checked artifact's persisted descriptor store is not a substitute for a
runtime lowering descriptor while the current lowering run is interpreting
values or selecting executable member targets. Every nested builder inherits the
same descriptor availability set. Dropping the descriptor slice while recursing
into a capture slot, callable result, erased hidden capture, serializable leaf,
or executable-key materialization is a compiler bug.

Private captures are graphs:

```zig
const PrivateCaptureRef = struct {
    owner: CheckedModuleArtifactKey,
    key: PrivateCaptureInstantiationKey,
};

const PrivateCaptureInstantiationKey = struct {
    template: PrivateCaptureTemplateRef,
    requested_payload: ConcreteSourceTypePayloadKey,
};

const PrivateCaptureNode = union(enum) {
    scalar,
    string,
    list,
    record,
    tuple,
    tag,
    box,
    finite_callable_leaf: FiniteCallableLeafTemplate,
    private_const_leaf: PrivateCaptureConstLeaf,
    recursive_ref: PrivateCaptureNodeRef,
};

const FiniteCallableLeafTemplate = struct {
    proc_template: CallableProcedureTemplateRef,
    source_fn_ty_template: CheckedTypePayloadRef,
};

const FiniteCallableLeafInstance = struct {
    proc_value: ProcedureCallableRef,
    source_fn_ty: ConcreteSourceTypeRef,
};

const PrivateCaptureConstLeaf = struct {
    const_ref: ConstRef,
    const_instance: ConstInstanceRef,
    requested_source_ty: ConcreteSourceTypeRef,
    mode: PrivateCaptureConstMode,
};

const PrivateCaptureConstMode = enum {
    pure_no_callable_slots,
    general_may_contain_callable_slots,
};
```

Source private captures may contain private const leaves and finite callable
leaves. They must not contain erased callable leaves unless a sealed erased
callable value has already been produced by a boxed boundary or promoted
wrapper. This prevents private-capture materialization from inventing erased
ABI decisions.
Finite callable leaves store only a sealed callable procedure template plus the
exact source function type template or concrete source function type for that
occurrence. They do not store callable-set keys, executable specialization
keys, capture-shape keys, layouts, generated symbols, runtime function
pointers, or capture pointers. Those are produced later when the leaf is
materialized as an ordinary `proc_value` and lambda-solved representation is
solved.

A generalized `ConstValueGraphTemplate` may contain a finite callable leaf only
when the callable template identity is already sealed without depending on a
future owner mono specialization. Existing checked/imported/hosted/platform or
promoted procedure templates can satisfy this. Inline lambdas and local
functions inside generic constants require a `ConstEvalTemplate` unless the
compiler has already created a sealed synthetic checked template whose identity
does not depend on a future lifted owner. Source private capture graphs may
contain finite callable leaves and const leaves; if a non-function subtree
contains an erased boxed callable, the graph stores a concrete
`PrivateCaptureConstLeaf` instead and executable MIR materializes that const
through the callable-aware const path.

Promoted wrappers have a reserve-before-fill lifecycle:

```zig
const PromotedProcedureTable = struct {
    rows: Span(PromotedProcedureRow),
};

const PromotedProcedureRow = struct {
    promoted: PromotedProcedureRef,
    template: CheckedProcedureTemplateRef,
    proc_base: ProcBaseKey,
    provenance: PromotedProcedureProvenance,
    source_binding: ?CheckedPatternBinderRef,
};

const CallablePromotionPlan = struct {
    root: PromotedCallableGraphNodeId,
    nodes: Span(PromotedCallableGraphNode),
    boundary_source_payload: CheckedTypePayloadRef,
};

const PromotedCallableNodeKey = union(enum) {
    wrapper: PromotedCallableWrapperKey,
    private_capture: PrivateCaptureInstantiationKey,
    const_leaf: ConstInstantiationKey,
    callable_leaf: CallableLeafInstanceRef,
    recursive_group: RecursiveCallableCaptureGroupKey,
};

const PromotedCallableGraphNodeId = enum(u32) { _ };

const PromotedCallablePathKey = struct {
    parent: ?PromotedCallableGraphNodeId,
    path: CapturePathDebugSegment,
};

const PromotedCallableWrapper = struct {
    key: PromotedCallableWrapperKey,
    source_fn_ty: ConcreteSourceTypeRef,
    body_plan: PromotedWrapperBodyPlan,
    private_capture: ?PrivateCaptureRef,
    state: enum { reserved, filling, sealed },
};

const PromotedWrapperParam = struct {
    source_ty: ConcreteSourceTypeRef,
    payload: ExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

const PromotedWrapperArg = struct {
    value: CaptureSlotReificationPlanRef,
    transform: ?PublishedExecutableValueTransformRef,
};

const PromotedWrapperBodyPlan = union(enum) {
    finite: FinitePromotedWrapperBodyPlan,
    erased: ErasedPromotedWrapperBodyPlan,
};

const FinitePromotedWrapperBodyPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    callable_set_key: CanonicalCallableSetKey,
    member: CallableSetMemberId,
    member_target: ExecutableSpecializationKey,
    captures: Span(PrivateCaptureRef),
    params: Span(PromotedWrapperParam),
    call_args: Span(PromotedWrapperArg),
};

const ErasedPromotedWrapperBodyPlan = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    params: Span(PromotedWrapperParam),
    executable_signature: ErasedPromotedProcedureExecutableSignature,
    call_sig: ErasedCallSigKey,
    code: SealedErasedCallableCode,
    capture: ErasedCaptureExecutableMaterializationPlan,
    arg_transforms: Span(PublishedExecutableValueTransformRef),
    hidden_capture_arg: ?ErasedHiddenCaptureArgPlan,
    result_transform: PublishedExecutableValueTransformRef,
    provenance: NonEmptySpan(BoxErasureProvenance),
};

const PromotedCallableGraphNode = union(enum) {
    wrapper: PromotedCallableWrapperKey,
    capture: PrivateCaptureRef,
    const_leaf: ConstInstantiationKey,
    callable_leaf: CallableLeafInstanceRef,
    recursive_ref: PromotedCallableGraphNodeRef,
};
```

Recursive callable results reserve wrapper keys before their body plans are
filled. A sealed wrapper names the exact source function type, erased ABI when
relevant, private capture construction, callable leaves, and body plan. Later
stages may lower a sealed wrapper; they must not edit it or add hidden captures.
The `PromotedProcedureTable` is artifact-owned provenance and deterministic
serialization data; it is not a runtime closure environment and not a second
callable representation. Every row points at a sealed checked procedure
template and the stable `ProcBaseKey` used for later specialization, ordering,
cache keys, and debug provenance. `source_binding` is optional debug attachment
only and never semantic identity.
`PromotedCallableNodeKey` is canonical graph identity. `PromotedCallablePathKey`
is debug provenance only; it can explain where a node came from in a capture
graph, but it must not participate in deduplication, equality, cache keys, or
runtime symbol selection. Promoted capture leaves carry concrete
`ConstInstantiationKey` values, never pending const roots or source expression
ids.

The promotion lifecycle is:

1. Reify the callable root through its precomputed `CallableResultPlan`.
2. Reserve callable and private-capture graph nodes before descending into
   captures.
3. Reserve promoted procedure values and checked template slots for every
   callable node that can become a procedure binding.
4. Reify local capture graphs through `CaptureSlotReificationPlan`.
5. Fill wrapper bodies using the reserved procedure values, private captures,
   and template slots.
6. Seal checked procedure templates and promoted wrapper graph nodes.
7. Publish the top-level or nested binding only after the promoted procedure is
   sealed.

Promotion consumes precomputed `CallablePromotionPlan` records. It must not
discover dependencies from interpreter memory, source syntax, allocation order,
generated symbol names, or an extra lowering pass.

Executable-only promoted erased wrappers publish their executable signature
up front:

```zig
const ErasedPromotedProcedureExecutableSignature = struct {
    wrapper_params: Span<ExecutableTypePayloadRef>,
    wrapper_param_keys: Span<CanonicalExecValueTypeKey>,
    wrapper_ret: ExecutableTypePayloadRef,
    wrapper_ret_key: CanonicalExecValueTypeKey,
    erased_call_args: Span<ExecutableTypePayloadRef>,
    erased_call_arg_keys: Span<CanonicalExecValueTypeKey>,
    erased_call_ret: ExecutableTypePayloadRef,
    erased_call_ret_key: CanonicalExecValueTypeKey,
    hidden_capture: ErasedHiddenCaptureArgPlan,
};

const ErasedHiddenCaptureArgPlan = union(enum) {
    none,
    materialized_capture: ErasedCaptureExecutableMaterializationPlan,
};
```

An erased promoted wrapper body is consumed first by executable MIR. Mono,
row-finalized mono, lifted MIR, and lambda-solved MIR may carry the wrapper's
procedure identity and source function type opaquely through `call_proc`,
`proc_value`, and `call_value`, but they must not lower or inspect its body.
Lambda-solved still creates a bodyless `ProcRepresentationInstance` for the
wrapper so ordinary direct calls, procedure values, roots, and value transforms
can target its public params and return. Those roots use the exact executable
keys from `ErasedPromotedProcedureExecutableSignature`; if a caller supplies a
finite callable where the wrapper parameter expects an erased callable slot,
the conversion is a normal call-boundary transform authorized by the wrapper's
sealed `BoxErasureProvenance`.

For example:

```roc
make_boxed : {} -> Box(((I64 -> I64) -> I64))
make_boxed = |_| Box.box(|f| f(41))

apply_boxed : (I64 -> I64) -> I64
apply_boxed = Box.unbox(make_boxed({}))

main = apply_boxed(|x| x + 1)
```

`apply_boxed` is a promoted procedure binding, not a thunk or runtime closure
object. Lambda-solved represents it as a bodyless ordinary procedure boundary;
executable MIR owns the sealed erased-call body. The same boundary is used when
the wrapper is passed as a value:

```roc
call_it : ((I64 -> I64) -> I64) -> I64
call_it = |g| g(|x| x + 1)

main = call_it(apply_boxed)
```

Finite promoted wrapper members can also carry forced executable targets. A
member selected during compile-time evaluation may have executable payloads
that differ from re-solving the same source procedure in a later runtime root.
The wrapper therefore stores the selected member's
`ExecutableSpecializationKey` and artifact-owned payloads explicitly. Later
lambda-solved sessions import those payloads and use the forced target instead
of re-solving the member from its source procedure body.

```zig
const ProcValueExecutableTarget = struct {
    key: ExecutableSpecializationKey,
    artifact: CheckedModuleArtifactKey,
    payloads: ExecutableTypePayloadStoreRef,
    promoted_wrapper: ?MirProcedureRef,
};
```

When a promoted wrapper body emits the selected member as a `proc_value`, that
value carries the forced target. Lambda-solved clones `key` into the reserved
instance's executable specialization key and imports boundary payloads from
`artifact + payloads + promoted_wrapper` before consulting any local session
payload. Re-solving the selected member as an ordinary procedure value is a
compiler bug because it can lose compile-time solved erased payload positions.

Compile-time logical values are stored as nodes with artifact-local schema
names:

```zig
const ComptimeValueNode = union(enum) {
    scalar: ScalarValueNode,
    string: StringValueNode,
    list: ListValueNode,
    record: RecordValueNode,
    tuple: TupleValueNode,
    tag: TagValueNode,
    box: BoxValueNode,
    finite_callable_leaf: FiniteCallableLeafInstance,
    erased_callable_leaf: SealedErasedCallableValue,
    alias: AliasValueNode,
    nominal: NominalValueNode,
    recursive_ref: ComptimeValueNodeRef,
};
```

`CallableResultMemberTargetPlan.artifact_owned` is used when the executable
procedure base is already persistable in the artifact that owns the result
plan. `member_proc_relative` is used only for explicit lifted or local member
procedures whose proc base will be allocated by the future mono reservation
that lowers `member_proc`. In that case, mono first reserves the exact
`member_proc`, then fills the executable specialization base while preserving
the requested source function type, executable arg keys, executable return key,
callable representation mode, and capture shape key exactly.

Callable leaf templates become callable leaf instances only for a concrete
const or callable request. Sealed erased callable values carry source type,
ABI, code, capture, and provenance. Artifact-local schema names are remapped
through the canonical-name resolver before comparison with executable or glue
schemas.

Compile-time plan stores are restored with compile-time values:

```zig
const CompileTimePlanStore = struct {
    materialization_plans: ConstMaterializationPlanStore,
    callable_result_plans: CallableResultPlanStore,
    private_capture_plans: PrivateCapturePlanStore,
    promoted_wrapper_plans: PromotedWrapperPlanStore,
    imported_plan_views: Span(ImportedCompileTimePlanView),
};
```

Imported materialization plan views are zero-copy views paired with the
`CompileTimeValueStore` that owns their nodes. Importing an artifact restores
both the value store and plan store or the artifact cache misses. Imported
modules are not re-run to rebuild compile-time values or plans.

### Static Data Materialization

Readonly provided constants and target-specific constant materialization use a
target static data graph outside the checked artifact cache. Static Roc heap
allocations use the ordinary allocation prefix:

```zig
const TargetStaticDataGraph = struct {
    nodes: Span(TargetStaticDataNode),
    relocations: Span(TargetStaticDataRelocation),
    exports: Span(ReadonlyDataExportSymbol),
};

const TargetStaticDataNode = struct {
    id: TargetStaticDataNodeId,
    section: ObjectReadonlySection,
    alignment: u32,
    size: u64,
    bytes: Span(u8),
    relocations: Span(TargetStaticDataRelocationId),
};

const TargetStaticDataRelocation = struct {
    from: TargetStaticDataNodeId,
    offset: u64,
    target: union(enum) {
        data: TargetStaticDataNodeId,
        text: LocalTextSymbolId,
        external: ExternalSymbolId,
    },
};

const StaticAllocationPrefix = extern struct {
    refcount: usize,
    len_or_capacity: usize,
};
```

```text
data pointer
  - word_size: refcount word
  - 2 * word_size, when required: allocation element count
```

`REFCOUNT_STATIC_DATA` marks whole-program-lifetime data. Runtime helpers
must recognize it and skip increment, decrement, free, and uniqueness mutation.
Static data is never unique, never freed, and never updated by helper
bookkeeping writes. Any helper that writes allocation metadata must first prove
the allocation is dynamically unique; seeing `REFCOUNT_STATIC_DATA` prevents
that proof.

`static_data_ptr_offset` is the target-specific offset from the beginning of a
static allocation node to the Roc data pointer that host code observes:

```zig
fn static_data_ptr_offset(
    word_size: u32,
    element_alignment: u32,
    contains_refcounted_children: bool,
) u32 {
    const required_space =
        if (contains_refcounted_children) 2 * word_size else word_size;
    return align_forward(required_space, element_alignment);
}
```

The refcount word lives at `data_ptr - word_size` and is always
`REFCOUNT_STATIC_DATA`. When the allocation stores list elements containing
refcounted children, the allocation element count word lives at
`data_ptr - 2 * word_size`. Other static refcounted allocations may reserve the
same two-word prefix when `contains_refcounted_children` is true because the
runtime allocation convention uses that prefix for address arithmetic.

Static data emission writes allocation prefixes, payload bytes, and relocations
according to the committed layout graph. Object-format backends place exported
symbols and nested static nodes into readonly sections with the requested
alignment and relocation records; they do not construct static values at
runtime. ELF emits `.rodata` plus `.rela.rodata`; Mach-O emits
`__DATA,__const` plus relocations on that section; COFF emits `.rdata` plus
`.rdata` relocations. For targets whose object formats encode function or data
pointers differently, the object writer still consumes the same logical
relocation graph and performs only format encoding.

Erased callable hidden captures and static boxed callables use stable executable
materialization graphs:

```zig
const ErasedCaptureExecutableMaterializationPlan = union(enum) {
    none,
    zero_sized_typed: CanonicalExecValueTypeKey,
    node: ErasedCaptureExecutableMaterializationNodeId,
};

const ErasedCaptureExecutableMaterializationNode = union(enum) {
    const_instance: ConstInstanceRef,
    pure_const: PureConstInstanceRef,
    pure_value: PureComptimeValueRef,
    finite_callable_set: MaterializedFiniteCallableSetValue,
    erased_callable: MaterializedErasedCallableValue,
    record: StableErasedCaptureRecordMaterialization,
    tuple: Span(ErasedCaptureExecutableMaterializationPlan),
    tag_union: StableErasedCaptureTagMaterialization,
    list: StableErasedCaptureListMaterialization,
    box: StableErasedCaptureBoxMaterialization,
    nominal: StableErasedCaptureNominalMaterialization,
    recursive_ref: ErasedCaptureExecutableMaterializationNodeId,
};

const PureConstInstanceRef = struct {
    const_instance: ConstInstanceRef,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

const PureComptimeValueRef = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueNodeRef,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

const MaterializedFiniteCallableSetValue = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    callable_set_key: CanonicalCallableSetKey,
    selected_member: CallableSetMemberId,
    captures: Span(ErasedCaptureExecutableMaterializationPlan),
};

const MaterializedErasedCallableValue = struct {
    sealed: SealedErasedCallableValue,
};

const StableErasedCaptureRecordMaterialization = struct {
    fields: Span(StableErasedCaptureRecordField),
};

const StableErasedCaptureRecordField = struct {
    field: RecordFieldLabelId,
    value: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureTagMaterialization = struct {
    tag: TagLabelId,
    payloads: Span(StableErasedCaptureTagPayload),
};

const StableErasedCaptureTagPayload = struct {
    payload_index: u32,
    value: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureListMaterialization = struct {
    elems: Span(ErasedCaptureExecutableMaterializationPlan),
};

const StableErasedCaptureBoxMaterialization = struct {
    payload: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureNominalMaterialization = struct {
    nominal: NominalTypeKey,
    backing: ErasedCaptureExecutableMaterializationPlan,
};
```

Stable record, tag, list, box, and nominal materialization records store
canonical field labels, canonical tag labels, canonical payload logical indexes,
and child materialization plans. They do not store lowering-run-local
`RecordShapeId`, `RecordFieldId`, `TagUnionShapeId`, `TagId`, or
`TagPayloadId`. Executable MIR first lowers the expected executable payload for
the materialized capture, creating local row ids for this lowering run, and then
maps stable labels/indexes onto those local ids by checking the explicit
expected payload. Missing fields, payload arity mismatches, or canonical-order
disagreements are compiler bugs.

Materialization records code dependencies separately from layout/data
dependencies. A boxed erased callable constant depends on executable wrapper
code and on a static capture materialization graph. The wrapper symbol is code;
the payload, capture bytes, strings, lists, boxes, and tag data are readonly
data.

Readonly data export collection runs after checked artifact publication:

```zig
const ProvidedExportTable = struct {
    exports: Span(ProvidedExportRow),
};

const ProvidedExportRow = union(enum) {
    procedure: ProvidedProcedureExport,
    data: ProvidedDataExport,
};

const ProvidedDataExport = struct {
    symbol: ExportedSymbolName,
    pattern: CheckedPatternRef,
    const_ref: ConstRef,
    source_ty: ConcreteSourceTypeRef,
    dependency_root: MaterializationDependencyRoot,
};

const MaterializationDependencyRoot = struct {
    owner: CheckedModuleArtifactKey,
    summary: ComptimeDependencySummaryId,
};

const ReadonlyDataMaterializationPlan = struct {
    layout_requests: Span(StaticLayoutRequest),
    code_dependencies: Span(ErasedCallableCodeDependency),
    value: ConstMaterializationNodeRef,
};
```

Collection does not run while checking finalization is evaluating one
compile-time root, because other provided constants in the same artifact may
not have been evaluated or bound yet. The collector reads sealed provided-data
export rows, `CompileTimeValueStore`, and compile-time plan stores, then
publishes layout-request and code-dependency streams for target lowering.

Readonly export selection walks `artifact.provided_exports.exports`. Procedure
exports are skipped by readonly data materialization. Data exports resolve their
checked export pattern through `artifact.compile_time_roots.lookupIdByPattern`;
a missing compile-time root is a compiler bug. The selected root's
dependency-summary request yields the `ComptimeDependencySummaryId` used to
append `MaterializationDependencyRoot { owner, summary }` to mono input.

Provided readonly exports publish two independent dependency streams:

- materialization layout requests, which require hidden captures and nested
  static values to have committed target layouts before bytes are emitted
- code dependencies, which require procedure bodies and erased-call wrappers
  referenced by static data to be lowered even when no runtime procedure root
  calls them

Neither stream turns the data export into a runtime procedure root. Both are
derived from explicit provided data export records, sealed compile-time values,
and compile-time dependency summaries.

Mono consumes every `MaterializationDependencyRoot` through
`reserveComptimeDependencySummaryDependencies`. That reserves the exact
procedure callables, callable-binding instances, const instances, promoted
wrapper dependencies, and semantic-instantiation procedures named by the
summary. It does not append runtime roots, create top-level thunks, or expose
data dependencies as hosted or provided procedure entrypoints.

Static data emission is consumer-only: it consumes sealed materialization nodes,
layout requests, code dependencies, lowered code maps, and committed layouts. It
does not request new semantic roots, evaluate constants, solve callables, or
lower procedures on its own.

Lambda-solved creates non-root materialization solve sessions for provided
readonly data exports and consumes only published `CompileTimeValueStore` values
named by those exports. These sessions materialize representation instances
needed by static data, such as finite erased adapter members and direct erased
procedure dependencies. They are not runtime entrypoints.

Executable MIR collects static-data adapter dependencies only from published
`CompileTimeValueStore` values and reachable erased capture materialization
nodes. When it reaches a sealed erased callable value, it reserves exactly the
direct erased wrapper or finite erased adapter named by the sealed code record.
It must not synthesize an adapter from source syntax, descriptor order, function
type, or object data.

LIR lowering publishes a lowered erased callable code map:

```zig
const LoweredErasedCallableCodeMap = struct {
    entries: Span(LoweredErasedCallableCodeEntry),
};

const LoweredErasedCallableCodeEntry = struct {
    code: ErasedCallableCodeRef,
    lir_proc: LirProcSpecId,
    source_fn_ty: ConcreteSourceTypeRef,
    erased_sig: ErasedCallSigKey,
    exec_arg_tys: Span(CanonicalExecValueTypeKey),
    exec_ret_ty: CanonicalExecValueTypeKey,
    capture_shape: CaptureShapeKey,
    object_symbol: LocalTextSymbolId,
};
```

Static data emission consumes this map to emit relocations to already-lowered
procedure symbols. If a sealed erased callable code ref is absent from the map,
static emission takes the invariant path. It does not create a wrapper, choose a
procedure by name, or inspect callable-set descriptors to recover member
targets.

Each map entry validates against the erased signature that requested it. The
source function type, executable argument keys, executable return key, capture
shape, and lowered LIR procedure ABI must agree with the sealed
`ErasedCallSigKey`. Static data emission consumes only validated map entries.

Every LIR procedure that can be referenced from readonly data receives a
deterministic local text symbol derived from its already-published LIR procedure
identity. Static data emits relocations to those text symbols for erased
callable payload headers.

Readonly data export completion requires all of these properties:

- host-linkable symbols for provided constants
- nested heap values emitted as readonly static allocations
- static refcounts that make runtime incref, decref, free, and uniqueness
  mutation no-ops for static data
- compile-time evaluation decoupled from export emission
- one identical ABI and RC model for compile-time values, readonly exports,
  host-visible values, interpreter execution, and compiled backends
- boxed erased callable host-boundary tests for host-created, Roc-created,
  host-consumed, host-stored, and round-tripped callables

Representative exported symbols include:

```zig
extern const roc__answer: i64;
extern const roc__table: Table;
extern const roc__names: RocList;
extern const roc__tree: Tree;
```

Tests for those symbols must cover primitive constants, nested records, strings,
`List(List(Str))`, boxes, recursive boxed tag unions, boxed erased callable
constants, and static refcount no-op behavior.

### Boxed Erased Callable Operational Details

The erased callable allocation data pointer points at `Payload`. Capture bytes
begin at:

```zig
pub fn captureOffset() usize {
    return std.mem.alignForward(usize, @sizeOf(Payload), capture_alignment);
}
```

All Roc-side construction uses the shared helper in `src/builtins` or passes
the same alignment and allocation metadata. `allocation_has_refcounted_children`
is false for the outer erased-callable allocation because nested cleanup is
owned by `on_drop`.

Every hidden capture layout used with this ABI must have alignment less than or
equal to `capture_alignment`. Roc-created packing, generated host helpers, and
debug verifiers assert this explicitly. Release builds rely on the invariant and
use `unreachable` if an impossible capture layout reaches packing.

LIR carries explicit erased callable data:

- `call_erased` carries the boxed payload value and erased call signature; it
  does not carry capture layout.
- `packed_erased_fn` carries the callable function pointer/code ref, explicit
  `on_drop` helper or null, capture materialization, and erased call signature.

Hidden-capture recipe forms exist only during checking finalization, before the
sealed executable capture plan is published:

```zig
const HiddenCaptureRecipe = union(enum) {
    whole_materialized_capture_value: ValueInfoId,
    proc_capture_tuple: Span(ErasedCaptureSlotReificationRef),
    finite_callable_set_value: CallableResultPlanId,
    singleton_finite_adapter_without_hidden_capture: ErasedAdapterKey,
};
```

These recipes are not executable MIR inputs. Finalization consumes them into
`ErasedCaptureExecutableMaterializationPlan` plus explicit descriptor and
lowered-code-map entries. A singleton finite adapter without hidden capture is
allowed only when explicit callable-set descriptor data and lowered adapter
metadata prove there is exactly one no-capture member. The compiler must not
choose a hidden-capture recipe from arity, source syntax, capture byte size,
pointer nullness, backend layout, code-ref variant, or runtime data.

When `ErasedCallableResultCodePlan.read_from_interpreted_erased_value` has a
non-empty hidden capture, compile-time finalization requests a target-local
layout for that capture payload:

```zig
const LoweredCompileTimeRequestedLayout = struct {
    exec_ty_key: CanonicalExecValueTypeKey,
    layout_idx: LirLayoutId,
};
```

This is per-lowering target metadata scoped to the LIR program the interpreter
is about to run, not checked-artifact cache data and not a semantic side store.
Finalization uses `layout_idx` to interpret the inline capture bytes after
`erased_callable_capture_offset`. It must not infer the capture layout from the
erased callable header, source syntax, selected code, callable-set descriptor
alone, runtime pointer value, or capture byte width.

The interpreter stores the same payload header as compiled backends. For a
Roc-created boxed callable, the function pointer is an interpreter trampoline
and the capture bytes contain an interpreter context plus semantic capture
bytes. For a host-created boxed callable, the interpreter calls the host
function pointer directly through the same ABI.

Interpreter-created erased callable capture bytes begin with an interpreter
context record:

```zig
const InterpreterErasedCallableContext = extern struct {
    interpreter: *Interpreter,
    lir_proc: LirProcSpecId,
    semantic_capture_layout: ?LirLayoutId,
    semantic_capture_byte_offset: u32,
    callable_code_map_entry: LoweredErasedCallableCodeEntryId,
};
```

The exact fields may differ, but the context carries the interpreter pointer,
LIR procedure id, optional semantic capture layout, byte offset to semantic
capture bytes, and a lookup key into the lowered erased callable code map. The
trampoline reads this context, decodes `args` and `ret` through the erased
signature, and executes the same LIR procedure that compiled backends would
call through the payload header. Host-provided boxed callables do not use this
context; they are already valid payloads with host function pointers.

Readonly boxed erased callable constants consume sealed materialization records:

```zig
const SealedErasedCallableValue = struct {
    boundary: ErasedCallableBoundary,
    code: SealedErasedCallableCode,
    capture: ErasedCaptureExecutableMaterializationPlan,
    on_drop: ?OnDropSymbolRef,
};

const ErasedCallableBoundary = struct {
    source_fn_ty: ConcreteSourceTypeRef,
    checked_fn_root: CheckedTypePayloadRef,
    call_sig: ErasedCallSigKey,
    provenance: NonEmptySpan(BoxErasureProvenance),
};

const SealedErasedCallableCode = union(enum) {
    direct_proc: SealedDirectErasedProc,
    finite_adapter: SealedFiniteErasedAdapter,
};

const SealedDirectErasedProc = struct {
    code: ErasedDirectProcCodeRef,
};

const SealedFiniteErasedAdapter = struct {
    adapter_key: ErasedAdapterKey,
    members: NonEmptySpan(SealedFiniteErasedAdapterMember),
};

const SealedFiniteErasedAdapterMember = struct {
    member: CallableSetMemberRef,
    source_proc: MirProcedureRef,
    proc_value: ProcedureCallableRef,
    member_proc_source_fn_ty_payload: CheckedTypePayloadRef,
    member_lifted_owner_source_fn_ty_payload: ?CheckedTypePayloadRef,
    target_key: ExecutableSpecializationKey,
    arg_transforms: Span(PublishedExecutableValueTransformRef),
    capture_transforms: Span(PublishedExecutableValueTransformRef),
    result_transform: PublishedExecutableValueTransformRef,
};

const ErasedCallableLeafInstance = struct {
    sealed: SealedErasedCallableValue,
};
```

The sealed boundary records the exact source function type, erased call
signature, `Box(T)` provenance, `checked_fn_root`, and optional hidden capture
executable type. The code field names either a direct erased procedure wrapper
or a finite adapter with sealed members. The capture field is the complete
materialization graph for inline capture bytes. Provenance becomes stable only
after finalization has consolidated boundary, code, capture, and source
function root into the sealed record.
`SealedErasedCallableValue` is the only artifact-stable representation of a
post-interpretation erased callable. Helpers that publish or consume an erased
callable after interpretation accept this sealed record, not separate
parameters for source function type, signature, code, finite adapter members,
capture, and provenance. Parallel fields are forbidden because they can combine
a boundary from one occurrence with code or capture data from another.

Debug verification rejects empty provenance, `local_box_boundary` provenance in
a sealed artifact value, `boundary.call_sig.source_fn_ty` disagreement with the
boundary source type, finite adapter keys that disagree with the boundary
signature, member target requested-function mismatches, member executable
arg/return mismatches against the erased ABI, branch transforms that disagree
with the sealed member, and capture materialization whose finite callable-set
key disagrees with the sealed finite adapter key. Release builds use
`unreachable` for the same malformed artifact paths.

Hidden captures are never inferred from source lambda syntax, wrapper body
shape, or the absence of visible capture fields. The sealed record publishes
explicit hidden-capture layout requests, 16-byte alignment verification,
capture byte width, and capture materialization nodes. A zero-sized hidden
capture is still an explicit capture with zero byte width so the code/capture
ABI remains identical between static data, interpreter execution, compiled
backends, and glue. Nested sealed erased callables inside the capture are
materialized through their own sealed records. Direct wrapper code and finite
adapter code are collected from the sealed `code` field and lowered before
static data emission consumes the payload.

Already-erased callable imports carry `AlreadyErasedCapturePlan`. A
`materialized_capture` variant means the capture bytes are explicit semantic
data produced by lowering or compile-time materialization. `none` is valid only
when the sealed signature says there is no hidden capture payload. An artifact
must reject `local_box_boundary` provenance and any boundary-local capture ref
that has not been finalized into a sealed already-erased capture plan.

Rehydrating a const-backed `ErasedCallableLeafInstance` imports all sealed
metadata into the current lambda-solved session atomically:

1. Import the erased ABI payload named by `sealed.boundary.call_sig.abi` from
   the owner artifact's `ErasedFnAbiStore`.
2. Import the executable endpoint payload for the erased return key and every
   erased argument key into the current `SessionExecutableTypePayloadStore`.
3. Preserve `AlreadyErasedCapturePlan.materialized_capture` as the hidden
   capture plan; do not replace it with only an executable key.
4. Reserve every direct erased code dependency or finite adapter member target
   named by `sealed.code`.
5. Recurse into nested sealed erased callables inside the materialized capture
   graph and reserve their code dependencies too.

Executable MIR consumes the solved member instances created by this rehydration
step. If it can see a sealed finite adapter but cannot find the corresponding
solved member target, lambda-solved rehydration produced an invalid session;
executable MIR must not synthesize the missing member from adapter keys,
descriptors, source procedure names, function types, layouts, or runtime bytes.

Singleton promoted finite adapters use the same sealing path as multi-member
finite adapters. The promoted wrapper first seals the callable-set descriptor,
then reserves the full `ErasedAdapterKey`, then records the single member's
executable specialization, argument transforms, capture transforms, result
transform, and hidden capture materialization. Singleton status never permits a
direct-call shortcut, a wrapper identified only by source function type, or
capture-free inference from the wrapper body.

The runtime callable selected by a singleton finite adapter may have a source
function type that differs from the erased boundary source type because of a
transparent alias or wrapper view. Finalization seals the adapter key, member
target requested function type, erased ABI arg/return executable keys, and
finite callable-set capture materialization together at the boundary source
type. The sealed record rejects mixed boundary/code/capture source keys: the
boundary source type validates the erased ABI, the member target validates the
direct call, and the capture materialization validates the finite callable-set
descriptor. No later stage may retype only one of those pieces.

For boxed erased callable constants, the sealed boundary source type is the
provided/exported source type at the boundary. It may differ from the alias
spelling that created the value. The erased call signature is validated against
the boundary source type, not against a local alias name.

Compile-time callable result publication uses the canonical
`CallableResultPlan` family from compile-time finalization. For erased callable
results, `ErasedCallableResultPlan` carries the checked source-function payload,
erased signature, non-empty provenance, result executable type, code-selection
plan, capture reification path, and promoted wrapper executable signature
payloads. That plan is enough to decode an interpreted value that contains
callable leaves, publish promoted procedures when a runtime/export path requires
them, and lower erased wrappers without inspecting interpreter memory shape or
source closure syntax. Wrapper body plans are generated from sealed executable
representation data and erased signatures.

An erased callable whose hidden capture layout has zero byte width still has an
explicit capture record when the sealed boundary says a hidden capture
executable type is present. Zero-sized capture means zero bytes are emitted
after the payload header; it does not mean the compiler may treat the capture
as absent.

For static boxed erased callables, the outer allocation has the static refcount.
Normal runtime `decref` never runs `on_drop` for that allocation and never frees
it. Nested capture data reachable from a static boxed callable must therefore be
emitted as static readonly data as well; correctness of static data must not
depend on final-drop callbacks running.

### Layout Graph Contracts

Executable MIR builds a logical layout graph. Recursive graph interning operates
on logical nodes and local recursive references before physical storage
indirection is inserted. Physical layout lowering then commits field offsets,
tag payload offsets, pointer indirections, static allocation prefixes, and
backend storage sizes.

Zero-sized values are represented by the committed layout graph, not by source
syntax rules. Records with zero-sized fields, singleton tag unions, boxes of
zero-sized payloads, lists of zero-sized elements, and recursive nominals all
flow through the same layout graph and committed layout store.

Recursive physical layout commitment publishes explicit slot mappings:

```zig
const RecursiveSlotCommit = struct {
    owner_layout: LogicalLayoutId,
    logical_slot: LogicalSlotId,
    physical_indirection: RecursiveIndirectionId,
    raw_payload_layout: PhysicalLayoutId,
    logical_value_layout: PhysicalLayoutId,
};
```

The mapping distinguishes the raw recursive placeholder layout stored in a
physical recursive slot from the finalized logical value layout expected by
consumers. Constructors, source `match` payload extraction, field access,
capture access, callable-set payload access, erased capture access, and RC plans
consume the same mapping. Later stages do not independently decide whether a
recursive field is direct or indirect by inspecting type syntax, layout names,
or physical offsets.

IR layout lowering preserves nominal source identity before unwrapping a
nominal backing:

```zig
const NominalExecutableType = struct {
    nominal: NominalTypeKey,
    source_ty: CanonicalTypeKey,
    backing: ExecTypeId,
};
```

When lowering a nominal executable type, the layout builder first reserves or
looks up a logical graph node keyed by `source_ty`, then lowers the backing.
Recursive references inside the backing find the reserved nominal graph node.
This prevents two structurally similar backing graphs from receiving different
layout identities for the same nominal source type.

LIR storage lowering owns the physical adapters introduced by recursive layout
commit. If a logical slot edge was committed as a physical box, LIR inserts the
explicit storage operation at the use site. List element extraction reads into a
temporary with the list's exact physical element layout, then bridges that raw
element local into the consumer's logical result layout when the raw recursive
placeholder differs from the finalized logical value layout. Backends continue
to require low-level operation result layouts to exactly match operation ABI
layouts; they do not repair structurally compatible layout ids.

Layout commit caches are graph-node complete. When IR-to-LIR commits one
logical layout graph, it records the physical layout id for every local logical
graph node reached during that commit, not only for the requested root. Later
child graph refs must reuse the cached mapping for their logical graph node and
must not recommit the child subgraph. Committing the same logical runtime value
twice to structurally equivalent but distinct physical layout ids is a compiler
bug. Backend layout-id equality depends on graph-node identity being committed
once and reused everywhere that value's logical node appears.

### Materialization Modes

Materialization runs in explicit modes:

- runtime body projection for procedure bodies
- compile-time constant consumption for interpreter results
- readonly static data emission for exported constants
- glue/runtime schema publication for host-visible values
- hidden capture materialization for boxed erased callables

Each mode receives explicit layout requests, code dependencies, value refs, and
source type payloads. A materializer must not switch modes by inspecting source
syntax, symbol visibility, or backend object state.

## Names And Terminology

Use Roc terminology. Do not describe method constraints, interface
capabilities, dispatch owners, callable sets, or type categories with
terminology from unrelated language models.

Avoid vague terms for semantic data. Prefer precise names such as:

- checked artifact
- relation artifact
- method registry
- static dispatch plan
- root request
- value reference
- callable set
- representation root
- executable specialization key
- layout request
- materialization graph
- runtime image

## Debug Verification

Debug-only verifiers assert stage invariants:

- checked artifacts contain every component consumed after checking
- checked artifact cache hits restore checked artifact data and compile-time
  value stores together
- canonical type keys normalize rows and preserve open checked-variable
  identity without merging distinct flex, rigid, row-tail, tag-tail, numeric, or
  constrained variables
- checked body graph traversal reaches every child id stored in body nodes,
  static-dispatch plans, nested sites, const templates, callable templates, and
  private-capture templates
- method diagnostics use published method identifier subregions
- resolved value refs are sealed, scoped to their owning artifact, and never
  builder-only after publication
- `ErasedCallSigKey` contains only `source_fn_ty` and `ErasedFnAbiKey`
- erased callable ABI keys validate arity, arg/result payloads, capture ABI,
  and hosted ownership consistently across wrappers, interpreters, and glue
- every erased callable value has non-empty `BoxErasureProvenance`
- boxed payload erasure graphs are graph-shaped, reserve recursive payloads
  before children, and distinguish materializing leaves
- interface capabilities and opaque proofs exist for every imported nominal
  representation consumed by boxed-payload erasure
- platform relation rows authorize every platform-required const/procedure use
- mono output contains no dispatch nodes
- mono roots and specializations are explicit
- mono graph finalization closes only the variables allowed by the published
  finalization records, and body emission sees a finalized lookup-only graph
- each static-dispatch site uses a fresh dispatch-site instantiator
- mono exported types contain no checker vars, unresolved placeholders,
  generalized vars, pending checked payloads, or builder-private links
- method registry callable roots match checked procedure template callable roots
- row-finalized mono contains no name-only row operations
- lifted MIR contains no captured `var_` reads and no bare procedure-symbol
  values
- lifted direct-call spans match `call_proc` nodes
- lambda-solved sessions are sealed and contain no unresolved callable variables
- lambda-solved lowering of deep expression spines uses stack-safe worklists
- lambda-solved descriptors carry session-local `ProcRepresentationInstanceId`
  values and executable branches use those descriptors for target selection
- finite erased adapter demands and persisted adapter members are sealed before
  executable lowering
- Box erasure comes only from explicit `Box(T)` boundaries
- callable-set descriptors contain no executable proc ids, layouts, generated
  symbols, runtime pointers, ARC data, or backend ABI handles
- value metadata is attached to MIR values, and call-site metadata names every
  consumer use and transform boundary required by executable lowering
- projection, join, and return records distinguish raw storage slots from
  projected source results and record non-returning branches
- executable MIR consumes lambda-solved metadata directly
- every finite callable-set call lowers to `callable_match`
- every `ExecutableValueRef` has one executable type and every bridge consumes
  already evaluated value refs
- executable locals are defined before use, scoped to their block/branch, and
  escape only through published joins/results
- source-match reachability is value/path-specific, branch-owned, and agrees
  between executable MIR and IR
- Bool has no runtime special-case bridge
- const materialization resolves every `const_instance` before IR
- compile-time value stores and compile-time plan stores restore together, and
  imported materialization plans are zero-copy views rather than re-execution
- private captures and promoted wrappers are sealed before post-check lowering
- static data emission consumes explicit materialization plans, layout requests,
  code dependencies, and lowered erased callable code map entries
- static data graphs use `REFCOUNT_STATIC_DATA`, readonly sections, relocation
  records, and validated `static_data_ptr_offset`
- callable leaf templates point at sealed callable templates, and erased
  callable templates become sealed values only for concrete instances
- `MonoSpecializationKey` records name checked procedure templates or imported
  semantic-instantiation procedures, never mono output procedure ids
- `ConstUseTemplate` and `ProcedureUseTemplate` remain templates until a
  concrete request clones them; concrete `ConstInstantiationKey` or
  `CallableBindingInstantiationKey` values must not appear where a template use
  is required
- semantic-instantiation procedure records have corresponding generated
  procedures in the owning artifact table
- no generalized type variable or callable variable reaches executable-stage
  input without an explicit concrete source type key, executable key, or sealed
  callable descriptor payload
- LIR input to ARC is RC-free
- ARC token summaries cover mutation barriers, keep sets, loop/switch
  continuations, stack-safe cleanup, and ZST container allocations
- low-level RC effects and LIR write modes publish every ownership transfer ARC
  needs before ARC runs
- interpreter storage uses committed LIR size/alignment for every value and the
  same boxed-erased callable ABI as other backends
- backend calls materialize stable sources before ABI argument placement can
  clobber them
- backends consume explicit LIR RC statements only
- runtime-image child views contain only offset-based image slices, hosted table
  images, platform root mappings, and no checked artifacts or CIR

Release builds must not retain verifier-only scans, text-only repository audits, or
verification metadata. The release path for an invariant violation is
`unreachable`.

## Required Tests

Structural tests cover every type-state boundary:

- checked artifact completeness tests for the full `CheckedModuleArtifact`
  record, `CheckedProcedureTemplateTable`, every required table field,
  complete exported procedure and non-procedure top-level entries, promoted
  procedure rows, artifact-owned checked body/type references, and immutable
  downstream views
- checked artifact completeness tests for `module_identity`,
  `checking_context_identity`, artifact-owned `CallableSetDescriptorStore`,
  artifact-owned `CanonicalNameStore`, platform relation rows, runtime versus
  persisted callable descriptor separation, and narrowed views that cannot
  rebuild missing semantic rows from checked modules or source stores
- canonical name store tests for `CanonicalNameId`, domain wrappers such as
  `RecordFieldLabelId`, `TagLabelId`, `MethodNameId`, `ExportNameId`, and
  `ExternalSymbolNameId`, remapping by canonical bytes, imported/exported
  names, row and tag labels, method names, hosted/external symbols, glue schema
  names, runtime-image symbols, wrapper-domain mixups, and foreign-id verifier
  failures
- checked type publication index tests for `CheckedTypePublication`, shared
  concrete roots, distinct open roots with matching canonical keys, every
  publication consumer that starts from a checker type variable, and rejection
  of canonical-key lookup on unproven-open graphs
- mono specialization request tests for every `MonoSpecializationReason`,
  `MonoSpecializationQueue` reservation, `reserved`/`lowering`/`lowered`
  states, exact `ConcreteSourceTypeRef` retention, and no declaration scanning
  for missing specialization targets
- finite `call_value` branch tests for `CallValueFiniteDispatchBranch`,
  `CallValueFiniteDispatchPlan`, branch `target_instance`, per-branch argument
  and result transforms, singleton and multi-member callable sets, shared
  contextual argument proof, and no duplicated argument evaluation
- join input tests for `JoinInputSource`, `JoinInputInfo`, if branches,
  source-match alternatives, loop phis, non-returning branches, deterministic
  storage order versus semantic source identity, and input transforms consumed
  before IR lowering
- nominal reinterpret backing-use tests for `NominalReinterpretBackingUse`,
  nominal construction, backing reinterpretation, direction-sensitive expected
  endpoints, transparent nominal backing cases, and rejection of syntax/name or
  layout-derived nominal identity
- boxed function payload tests for `FunctionPayloadRepresentation`, recursive
  argument and return payload transformation, `CallableBoxPlan.already_erased`,
  `proc_value_to_erased`, `finite_set_to_erased_adapter`, nested boxed
  functions, and verifier failures for opaque function leaves inside `Box(T)`
- callable-result member target tests for
  `CallableResultMemberTargetPlan.artifact_owned`,
  `member_proc_relative`, lifted/local member procedures, proc-base filling,
  endpoint preservation, and platform/requested source function payload
  projection
- forced promoted-wrapper target tests for compile-time selected finite
  members, `ProcValueExecutableTarget`, imported boundary payloads, and
  rejection of re-solving selected members from source procedure bodies
- executable procedure origin tests for `ExecutableProcOrigin.source`,
  `ExecutableProcOrigin.erased_adapter`, full `ErasedAdapterKey` comparison,
  and rejection of first-member source procedure identity reuse
- const instantiation store tests for `ConstInstantiationState.reserved`,
  `evaluating`, and `evaluated`, dependency summary retention, reification plan
  retention, generated semantic-instantiation procedure publication, and no
  post-evaluation graph scanning to rediscover generated dependencies
- canonical graph key builder tests for `CanonicalGraphKeyBuilder`, stable
  recursion binders and backrefs across specialization keys, executable keys,
  callable-set keys, capture-shape keys, erased adapter keys, boxed payload
  capability keys, and layout-publication keys
- graph-node-complete layout commit tests proving every local graph node is
  cached on root commit, child graph refs reuse cached mappings, duplicate
  commits are rejected, and structurally equivalent physical layouts do not
  substitute for logical graph-node identity
- solve-session builder tests for `SolveSessionBuilder`,
  `LambdaSolvedBuilderState`, mutable builder member tables, reservation during
  recursive discovery, explicit synchronization before solve/finalization
  steps, sealed member-slice publication only after fixed point, and rejection
  of stale published session member views
- checked procedure template fixtures for every `CheckedProcedureBody` variant,
  promoted wrapper publication, top-level use summaries, nested procedure-site
  tables, and rejection of raw CIR ids, type vars, parser ids, generated
  symbols, and exporter-local `ModuleEnv` lookups after artifact publication
- executable type memoization tests proving repeated explicit source roots
  reuse one completed executable type graph, while active-recursion roots still
  break recursive descent
- full `CallSiteInfo` tests for direct calls, finite value calls, erased value
  calls, `arg_exprs` ownership, `requested_fn_root`,
  `requested_source_fn_ty`, result transforms, and generic builtin calls such
  as `List.concat`
- finite-erased adapter demand tests for `member_targets`, non-empty
  provenance, all descriptor members reserved before transform finalization,
  and distinct adapter procedures for distinct `ErasedAdapterPayloadOwner`
  values
- executable payload forwarding tests for aliases, joins, and plain
  function-typed forwarded values, including verifier failure for orphan
  `pending` payload entries
- executable payload-store key invariant tests for endpoint key round-trips,
  recursive payloads, child endpoint payload keys, and invalid reuse of
  `by_key` as payload construction machinery
- static data tests for object-section selection, readonly relocations,
  `REFCOUNT_STATIC_DATA`, uniqueness falsehood, no helper metadata writes on
  static allocations, and exact static allocation prefix offsets
- lowered erased callable code map tests for metadata-sink publication, static
  data relocation use, interpreted erased callable result decoding, missing
  map-entry invariant failure, and validation against erased signatures
- platform required constant tests for callable-field required constants,
  pre-runtime-summary concretization, relation artifact propagation through all
  finalizers, and rejection of import/provides/source scanning for relation
  data
- platform for-clause substitution tests for `platform_alias_name`,
  `platform_rigid_root`, `projected_app_type_root`, `Box(Model)`-style platform
  rigid roots, checked body payload replacement, root/procedure/template/glue
  publication, relation artifact propagation, and rejection of source/import
  scanning recovery
- checked type store tests for `record_empty`, `tag_union_empty`,
  `recursive_ref`, concrete root lookup, scheme lookup, and rejection of
  foreign artifact-local checked type ids
- checked artifact publication and cache keys
- `CheckedTypeNode` payload preservation for primitives, functions, records,
  tuples, tag unions, aliases, nominals, opaques, boxes, lists, open vars,
  delayed numerics, constrained vars, generalized vars, and placeholders,
  including `CheckedOpenVar.constraints`, function `needs_instantiation`,
  alias/nominal backing fields, opaque markers, and extension tails
- `CanonicalTypeKey` and `CanonicalTypeSchemeKey` normalization: flattened
  record/tag extension chains, sorted explicit labels, preserved open tails,
  preserved tag payload order, wrapper/constraint/generalized-variable
  identity, and duplicate-label invariant failures
- publication of open checked variables through `CheckedSourceTypeRoot`,
  including flex, rigid, row-tail, tag-tail, numeric, and constrained variables,
  plus verifier fixtures that reject `CheckedTypeStore.rootForKey` lookup for
  open graphs without proven concreteness
- `ConcreteSourceTypeStore` exact-source reuse through
  `ConcreteSourceTypeSource`, `by_source`, source-owner remapping after
  substitution, and atomic template/concrete-variable unification
- `CheckedBodyGraphTraversal` coverage for static-dispatch plans, nested sites,
  const templates, callable templates, private captures, and checked body/table
  ids
- checked call-site payload tests for generic builtins, direct calls,
  `call_value`, call result type authority, fixed arity verifier failures, and
  rejection of callee lookup type as the call specialization authority
- procedure parameter tests for destructuring params, mutable params,
  synthetic ABI params, explicit entry `var_decl`, source `match` wrapping, and
  no aliasing between ABI params and source mutable binders
- specialization-local binder environment tests for local lookups, mutable
  versions, pattern binders, `for` binders, generated destructuring binders, and
  stale checked-root rejection after a concrete mono binder is published
- local procedure binder tests where one local function is used at multiple
  concrete function types, including `I64 -> I64` and `Str -> Str`
- numeric low-level tests for arithmetic, comparisons, unsuffixed literals,
  mutable params, selected operand type, and `source_constraint_ty`
- import-resolution fixtures for package-qualified type modules, main-type
  binding through scoped type bindings, associated type suffix normalization,
  builtin auto-import lowering views, preserved full `Import.Idx`, and verifier
  failures for basename-only checked import identities
- method diagnostic region publication for method identifier subregions such as
  `35.foo()` and `12.34.foo()`
- `dbg` and `Str.inspect` lowering fixtures for statement and expression
  `dbg`, custom `to_inspect`, transparent nominal default inspect, opaque
  nominal `"<opaque>"`, function `"<fn>"`, recursive helper reservation,
  `Box(T)` inspect, transparent default inspect without inserting
  `nominal_reinterpret`, recursive transparent nominal helper reservation, and
  absence of inspect pseudo-nodes after mono
- lifted lexical-scope tests for `LiftScopeFrame`, every `BindingLocation`
  case, mutable-version capture tracking, top-level/imported/hosted/platform
  values excluded from capturable scope, and local shadowing by binder identity
- procedure representation tests for `ProcRepresentationTemplate`,
  `ProcedureInstantiationOwner`, `ProcPublicValueRoots`,
  `ProcRepresentationInstance` state transitions, and rejection of generalized
  templates as executable inputs
- pre-executable call tests for `proc_value`, `call_proc`, `call_value`,
  requested function type authority, capture arg ordering, mono queue closure
  over proc values, and verifier failures for arity, arg/result types, captured
  `var_`, and bare procedure-symbol values after lifting
- adapter-owned boundary tests for `ProcBoundaryExecutablePayloads`,
  `boundary_provenance`, imported artifact payload graphs, promoted-wrapper
  params/returns, and call-boundary transforms that consume sealed payloads
  rather than callee bodies or source syntax
- representation shape tests for all `RepresentationShape` merge cases,
  recursive placeholders, stable backrefs, opaque/imported/hosted capability
  handling, and tag construction against the full row-finalized union shape
- exact `ErasedCallSigKey` shape: `source_fn_ty` plus `ErasedFnAbiKey`, with
  capture type, capture shape, wrapper code, layout ids, runtime pointers, and
  object symbols rejected as key fields
- `ErasedFnAbiStore` equality and validation for artifact/session stores,
  interned key versus payload separation, fixed arity, argument executable
  keys, return executable key, arg/result ABI payloads, hidden capture argument
  ABI, hosted owner, and same source type with different ABI keys
- boxed payload erasure graphs with `active_payload_to_plan_id`,
  `completed_payload_to_plan_id`, `recursive_ref`, materializing-leaf
  detection, function slots, `CallableBoxPlan`, and `ProcValueErasePlan`
- box boundary tests for box/unbox direction, input expression, box root,
  payload root, box type, payload source type, payload boundary type, recursive
  boxed payload plans, and verifier failures for malformed boundaries
- proc-value erasure tests for `source_value`, `target_instance`,
  `executable_specialization_key`, adapter arg transforms, capture transforms,
  no-capture values, zero-sized captures, captured procedures, and erased
  direct-proc adapters
- `BoxErasureProvenance` for every erased callable, including local boundary,
  const-graph box, and promoted-wrapper cases, with verifier fixtures rejecting
  unknown, layout-derived, hosted-derived, and compatibility-derived provenance
- already-erased callable transforms for `.exact` and `.same_abi_retype`,
  including transparent alias examples and exact `call_value_erased` signature
  matching
- already-erased capture plans for no capture, zero-sized typed capture,
  explicit value capture, executable-key capture, artifact materialized capture,
  const-backed rehydration, nested sealed erased callables, and preservation of
  `AlreadyErasedCapturePlan.materialized_capture`
- already-erased group-emission seeding tests for const-backed erased callables,
  transparent same-ABI retype, runtime ABI equivalence across source aliases,
  provenance union, and `require_box_erased` rehydration with artifact-stable
  provenance
- erased capture materialization tests for
  `ErasedCaptureExecutableMaterializationPlan`, stable field labels, stable tag
  labels and payload indexes, pure const proofs, recursive refs,
  materialized finite callable-set captures, nested sealed erased callables, and
  expected-payload mapping to local row ids
- executable-only promoted erased wrappers with
  `ErasedPromotedProcedureExecutableSignature`,
  `ErasedHiddenCaptureArgPlan`, bodyless lambda-solved representation
  instances, forced executable targets, and examples equivalent to
  `make_boxed`, `apply_boxed`, and passing `apply_boxed` as a value
- hidden-capture recipe tests for `whole_materialized_capture_value`,
  `proc_capture_tuple`, `finite_callable_set_value`, and
  `singleton_finite_adapter_without_hidden_capture`, including rejection of
  recipe selection by source syntax, byte size, arity, pointer nullness, layout,
  or code-ref variant
- target-local hidden-capture layout request tests for interpreted erased
  callables with non-empty captures, including
  `LoweredCompileTimeRequestedLayout` and rejection of layout inference from
  erased callable headers or runtime pointers
- sealed erased callable records with consolidated boundary, code, capture,
  provenance, `checked_fn_root`, singleton promoted finite-adapter sealing, and
  `AlreadyErasedCapturePlan.materialized_capture`
- sealed erased callable validation through one `SealedErasedCallableValue`
  record, including `ErasedCallableBoundary`, `SealedErasedCallableCode`,
  direct erased procs, finite adapters, finite adapter members, and verifier
  fixtures for source-type, signature, member-target, transform, and capture
  mismatches
- `ValueInfoStore` attachment to MIR values: `ValueInfo`, `BindingInfo`,
  `CallableValueInfo`, `BoxedValueInfo`, `AggregateValueInfo`, value origins,
  and no expression-id semantic recovery
- const-backed value metadata and projection tests for `ConstBackedValueInfo`,
  `ValueInfo.const_backing`, record/tuple/tag projection from sealed
  schema/value stores, alias and nominal unwrapping, callable leaves in
  const-backed projections, const-backed boxes with `boundary = null`, and
  `BoxErasureProvenance.const_graph_box`
- value-alias transform tests for `value_alias_needs_executable_transform`,
  `value_alias_source`, `value_alias_transform`, identity transform records,
  vacant callable-slot aliases, unreachable branch aliases, and runtime alias
  transform application during executable `var_` lowering
- value metadata build-state tests for `ValueInfoBuildRecord`,
  `ValueInfoBuildState`, dense value/binding/projection/call-site/capture
  boundary stores, `CallableSetConstructionPlan`, `CaptureBoundaryOwner`,
  `CaptureBoundaryInfo`, and metadata ids attached to MIR values
- `ConsumerUsePlan` coverage for contextual construction and existing-value
  transforms, especially inactive tag variants with function slots
- consumer-use lowering tests for `ConsumerUseOwner`, `ConsumerUseLowering`,
  direct construction, contextual control flow, existing values at
  `boundary.from_endpoint`, binding writes, reassignment writes, returns,
  branch joins, and nested erased endpoints
- `CallSiteInfo` and `CallDispatchInfo` coverage for direct, finite, erased,
  and low-level calls; `arg_consumer_uses`; finite branch arg/result
  transforms; and erased call ABI packing/unpacking
- `ValueTransformBoundaryKind` coverage for identity, raw call arg/result,
  capture, aggregate, consumer-use, boxed-payload, erased-capture, return, and
  join boundaries
- published vs session-local value transforms through
  `ExecutableValueTransformRef`, `PublishedExecutableValueTransformRef`, and
  `SessionExecutableValueTransformStore`
- session value-transform endpoint tests for `TransformEndpointScope`, every
  `TransformEndpointPathStep`, every `SessionExecutableValueEndpointOwner`,
  identity, structural, record, tuple, tag, nominal, list, box-payload,
  callable-to-erased, and already-erased operations, nullable
  `SessionBoxPayloadTransformPlan` boundaries, and proc-value/finite-value
  `CallableToErasedTransformPlan`
- structural bridge tests for `direct`, `zst`, `list_reinterpret`,
  `nominal_reinterpret`, `box_unbox`, `box_box`,
  `singleton_to_tag_union`, `tag_union_to_singleton`, and
  `BoxPayloadTransformKind` direction-sensitive behavior
- `SessionExecutableTypePayloadStore` separation from published
  `ExecutableTypePayloadRef`, including isomorphic payloads and recursive
  session refs
- session executable payload tests for record fields, tuple elems, tag
  variants/payloads, nominal backings, callable-set members, erased-function
  payloads, recursive refs, keyed append ownership, `by_key` reuse integrity,
  and derived callable-set payload replacement before store sealing
- `ProjectionInfo`, `JoinInfo`, and `ReturnInfo` coverage for raw slots,
  projected results, endpoint slots, source slots, and non-returning branches
- projection tests for mandatory `ProjectionInfo.result_transform`,
  `projection_slot` endpoint owner, `projection_result` boundary kind,
  endpoint-slot raw access, transparent nominal backing projection, and identity
  transforms represented as records
- recursive executable type keying through `RootPayloadCycleKey`,
  `RootPayloadLayerKey`, solved group projection relations, and payload
  publication keyed by `CanonicalExecValueTypeKey`
- generic zero-sized and singleton lowering, including ZST aggregate elision,
  singleton `get_union_id`, ZST tag bridges, singleton callable-set
  finalization, and Bool lowered as ordinary tag-union data
- source-match decision plan records: `SourceMatch`, `PatternDecisionPlan`,
  `DecisionNode`, `DecisionEdge`, `DecisionLeaf`, `PatternPathValuePlan`,
  `RecordRestProjection`, and `PatternBinding`
- `MatchScrutinee`, `MaterializedPatternPathValue`,
  `AlternativeBinderRemap`, `PatternBindingTransform`, explicit
  `DecisionEdge.test`, and IR consumption of `SourceMatch.materialized_paths`
- source-match record tests for `DecisionTestNode`, `PatternPathStep`,
  `RecordRestProjectedField`, list element/rest probes, optional record-field
  defaults, `SourceBranchResult.returning`, `SourceBranchResult.no_return`,
  `SourceBranchResult.degenerate_runtime_error`, and IR consumption of
  decision-plan records without reinterpreting executable pattern syntax
- pattern path materialization for record rest, list rest, opaque/newtype
  payloads, guard ordering, path-local temp cache limits, and mandatory binder
  transforms
- Bool predicate/value tests for `BoolDiscriminants`, structural equality,
  scalar comparisons, predicate-producing low-level calls that return ordinary
  Roc `Bool` tag-union values immediately, and `BoolCondition` predicates that
  do not escape as Roc values
- checked artifact cache identity: one compiler artifact hash, exact direct
  import checked artifact keys, deterministic import records, and no post-hit
  identity patching
- checked artifact cache misses before publication for platform context
  mismatch, stale compiler artifact hash, changed checking context, changed
  direct import key, and missing compile-time value stores
- `CheckedArtifactAvailabilityRegistry` publish/unpublish lifecycle, owned
  artifact storage indexing, stale-key rejection, and deterministic imported
  dependency view assembly
- complete checked artifact cache hit unit, including `ModuleEnv` where needed,
  public exports, provides/requires, method registry, static dispatch plans,
  resolved refs, checked stores, template tables, root/hosted/platform tables,
  compile-time roots, value/instantiation stores, semantic-instantiation
  tables, private captures, callable result/promotion plans, dependency
  summaries, closures, relation artifacts, interface capabilities, and remaps
- artifact-owned checked body/type stores and source type payloads
- public `lowerArtifactsToLir` API shape, resource-only error type, explicit
  roots, relation artifacts, target config, `LoweredProgram`, and
  `CompileTimeLoweringMetadata`
- runtime callable-set descriptor tests for
  `LoweredProgram.callable_set_descriptors`, runtime versus persisted
  descriptor separation, optional published identities, deep cloning of
  `published_proc_value` and `published_source_proc`, and interpreter decoding
- `CallableSetDescriptorStore` tests for append-only publication, duplicate-key
  semantic equality, singleton key stability, publication from every reachable
  descriptor source, and rejection of session-local ids in published keys
- callable-set descriptor liveness/replacement tests where a transient
  descriptor is superseded before sealing, stale descriptors are not published,
  current emission plans consume the live descriptor, and capture-slot
  executable keys match the selected target endpoints
- `FunctionRepShape` tests proving whole-function roots publish argument,
  return, and callable children together for `call_value`, `call_proc`, and
  `proc_value`, and executable values keep only solved callable representation
- published/session value-transform owner tests for recursive child refs,
  structural-bridge children, missing artifact/session entries, and endpoint
  key mismatch verifier failures
- `CallableToErasedTransformPlan` context tests proving expression-owned
  `proc_value` erasure can pack captures while existing-value transforms only
  accept finite callable-set packing or already-erased pass-through/retype
- finite-erased adapter branch capture tests for
  `erased_finite_adapter_capture` endpoints, slot-order extraction, capture
  transforms to `procedure_capture`, hidden capture tuple assembly, singleton
  captures, and branch payload typing before lowering
- recursive direct-call SCC tests for `(recursive_group_anchor, target_proc)`
  reservation, external ordinary call-site ownership, synthetic direct-call
  targets, and release consumption of lifted `direct_calls` metadata
- const reification row/tag ordering tests for canonical field labels,
  logical tag/payload indexes, parent executable payload authority, missing
  variants, duplicate logical indexes, payload arity mismatch, and canonical
  ordering disagreement
- compile-time callable template tests for `CallableLeafTemplate`,
  `ErasedCallableTemplate`, generic const templates containing erased callable
  templates, and conversion to `ErasedCallableLeafInstance` only for concrete
  requests
- `ConstGraphBoxErasureWitness` tests proving `const_graph_box` provenance
  names the owner artifact and const graph reification plan, and rejecting
  persisted/imported `local_boundary` provenance
- singleton finite adapter boundary retyping tests where boundary source type,
  member target requested function type, erased ABI keys, and capture
  materialization are sealed together and mixed source-key records are rejected
- ARC stack-safety tests for long `CFStmtId` chains, generated helper chains,
  shared continuations, iterative insertion, and next-pointer patching without
  recursive statement traversal
- direct-call ARC token tests for duplicate consuming arguments, borrowed loop
  element aliases, used-after-call reachability through keep sets, and switch
  continuation summaries
- artifact publication verifier tests for callable leaf templates, checked
  template ownership in `MonoSpecializationKey`, template-vs-concrete const and
  procedure use records, semantic-instantiation procedure table entries, and
  rejection of generalized type/callable variables in executable-stage input
- public error-shape audits rejecting post-check semantic error names such as
  `NoRootProc`, `MissingRoot`, `MethodNotFound`, `UnsupportedLayout`,
  `SchemaLayoutMismatch`, and `MissingInterfaceCapability` in public lowering
  APIs
- sealed `ResolvedValueRefTable` partitions and builder-only resolved refs
- full interface capability table publication, including boxed payload
  templates, opaque atomic proofs, hosted/platform representation capabilities,
  and exported nominal representations
- `ConstUseTemplate` and `ProcedureUseTemplate` clone-instantiation into
  concrete request payloads
- `PlatformRequiredProcedureUse` and `PlatformRequiredConstUse` relation-owned
  authorization closures
- `PlatformRequiredValueUse` union rows for const and procedure values,
  including nested `ConstUseTemplate`/`ProcedureUseTemplate`, requested payload
  copies, app bindings, and `relation_template_closure` authorization scoped
  exactly to the selected app binding's private templates
- platform relation rows, requested payload copies, app-specific platform root
  artifacts, and `LoweringModuleView.relation_artifacts`
- `PlatformRequiredDeclarationTable`, `PlatformRequiredBindingTable`, and
  `PlatformRequirementRelationTable`, including `declared_source_ty` versus
  executable `requested_source_ty`, relation-key hashing, row-tail merge, and
  transparent alias/nominal merging across boxes, records, tags, lists, and
  function boundaries
- transparent nominal compatibility at platform boundaries, including `Try`
  without builtin-name special handling
- runtime body projection for non-runtime checked statements
- method registry callable-root equality with checked procedure templates
- checked type-variable identity preservation for open records, open tags,
  delayed numeric variables, and method-constrained variables
- imported canonical name remapping and artifact-local/lowering-run id wrappers
- `LoweringRunContext`, resolver/publisher APIs, remap verification, and no
  ad hoc canonical-name remapping
- imported template closure views authorizing private helper instantiation,
  including bodies, types, schemes, plans, private captures, method entries,
  and capabilities
- `ImportedProcedureBindingView` direct and callable-eval variants,
  `ImportedConstTemplateView`, closure inheritance through `proc_value`, and
  `CallableBindingInstanceRef` reservation with imported binding closures
- nominal declaration publication through `CheckedNominalDeclaration`,
  finalized local declarations, placeholder resolution, declaration-template
  instantiation, and canonical builtin identity such as `Builtin.Try`
- root request tables separated from const/callable semantic instantiation
  requests
- `LoweringRequestSet` split between runtime roots and
  `CompileTimeEvaluationRequest` values, with const/callable instances keyed by
  their own request records rather than `CompileTimeRootId`
- `RootProcedureRequest`, `RootKind`, `RootSource`, `LoweredRoot`, and
  `RootTarget` behavior for generic exports vs concrete runtime roots
- hosted/platform tables and interface capability publication
- hosted table publication for `HostedProcTable`, `HostedProcEntry`,
  `HostedOrderKey`, global hosted dispatch catalog, and
  `CallBoundaryRcTemplate`
- platform for-clause substitution rows published before post-check lowering
- top-level value table entries for const templates, procedure bindings,
  callable eval templates, imported values, hosted/platform values, and promoted
  procedures
- compile-time root/reification records: `ComptimeRoot`,
  `ComptimeRootResult`, `ConstGraphReificationPlan`, and
  `CallableLeafReificationPlan`
- compile-time dependency graph fixtures for prerequisite records, dependency
  reason variants, selected versus pending roots, `.test_expect` publication,
  runtime summaries that seal concrete const/callable/procedure dependencies
  without compile-time evaluation payloads, and runtime roots that require
  concrete const instances
- finite callable leaf and private capture tests for
  `FiniteCallableLeafTemplate`, exact source function type payloads, generic
  value graph templates that require sealed callable template identity,
  `ConstEvalTemplate` selection for inline lambdas/local functions, source
  private capture graphs containing finite callable leaves and const leaves but
  not erased callable leaves, `PrivateCaptureConstLeaf`, and private const
  modes
- `CallableEvalTemplate` and `EntryWrapper` publication, wrapper requested
  return slot authority, imported callable-eval closure selection, and
  closure-carrying reservations for direct calls and procedure values
- top-level callable finalization for direct function/lambda declarations,
  function-typed expression roots, and non-function constants containing
  callable leaves
- top-level procedure binding tests for `TopLevelProcedureBinding`,
  `ProcedureBindingBody.direct_template`,
  `ProcedureBindingBody.callable_eval_template`, generalized callable roots as
  procedure bindings, and absence of published `pending_callable_root`
- `CallableBindingInstantiationStore` records for direct and evaluated bodies,
  owner artifact, concrete dependency summary, final `ProcedureCallableRef`,
  private compile-time executable root, optional promotion plan, and sealed
  instance invariants
- generic constant templates distinguishing `ConstValueGraphTemplate` and
  `ConstEvalTemplate`, including inline-lambda cases and per-use concrete
  instantiation
- compile-time dependency summary records for `ComptimeProcDependencySummary`,
  `ComptimeCallDependency`, `ConstGraphDependency`,
  `CallableResultDependency`, `CallableLeafDependency`,
  `ErasedCallableDependency`, `supplied_erased_value`, availability uses,
  concrete uses, and ownership rules
- pending-local-root summary tests for local const roots, callable roots,
  summary-only `call_value`, `pending_local_root_call`, pending-origin
  propagation through aliases/returns/projections/joins, and rejection outside
  `LoweringMode.comptime_dependency_summary`
- `comptime_only` root tests proving private aggregate interpreter roots are
  excluded from runtime roots, backend input, object emission, generated entry
  metadata, and runtime images
- callable-result-plan tests for exact checked boundary payloads,
  `FiniteCallableResultPlan.source_fn_ty_payload`,
  `ErasedCallableResultPlan.source_fn_ty_payload`,
  `CallableResultMemberTargetPlan`,
  `member_lifted_owner_source_fn_ty_payload`, promoted wrapper executable
  signature payloads, `callable_schema`, empty callable-containing lists, and
  inactive tag variants
- single-child wrapper reification tests for one-field records, one-element
  tuples, and newtype-like wrappers whose physical layout elides the wrapper
- dependency-summary-during-reification tests proving final promoted procedure
  refs are recorded while value graph nodes are written, through
  `ProcedureCallableDependency`
- descriptor-availability propagation tests for nested const-graph,
  capture-slot, callable-result, and erased hidden-capture builders
- `SemanticInstantiationProcedureTable` ownership and no post-publication
  checked procedure template allocation
- mono specialization queue and dispatch elimination
- procedure identity tables for `NestedProcSiteKey`, `SourceProcKey`,
  `ProcBaseKey`, `MonoSpecializationKey`, `MonoSpecializedProcRef`, and lifted
  owner specialization
- nested procedure-site table paths and local-function/closure/desugared
  closure site kinds
- payload-bearing synthetic origins for erased adapters, bridges, intrinsic
  wrappers, entry wrappers, and promoted callables
- `ExecutableSyntheticProc`, `ExecutableSyntheticProcSignaturePlan`, and
  `LambdaSolvedExecutableSyntheticProcInstance`, including opaque carriage
  through earlier MIR and executable-only body validation
- mono finalized specialization graph lookup-only emission
- `MonoGraphFinalization` closure rules for delayed numeric defaults,
  equality-only `{}`, unconstrained `{}`, row/tag tail extension closure,
  method-registry intersections, callback-local return endpoints, and
  graph-finalization-before-emission
- mono numeric defaulting tests for `NumericDefaultPhase.mono_specialization`,
  defaultable arithmetic operators, comparison operand typing, delayed numeric
  static dispatch, mutable numeric params, unsuffixed literals, and constrained
  variables that are not numeric default decisions
- mono instantiated alias/nominal backing publication, parametric wrapper
  formal substitution from published roots, declaration representations with no
  direct runtime values, nominal-vs-backing unification, nominal identity
  preservation, nominal pattern lowering through published declarations, open
  row residual tails, duplicate-label failures, and residual clone metadata
- fresh static-dispatch target instantiation per dispatch expression, with
  tests proving enclosing substitutions and previous template roots are not
  reused
- mono type-only call result queries and block-local demand propagation
- block-local demand propagation fixtures for demand tables keyed by checked
  binding identity, later-use-before-earlier-declaration demand, local alias
  demand such as `g = f; g(40)`, branch/source-match function-valued joins,
  aggregate child endpoint demand, constrained-flex rejection when no demand
  exists, and no thunk/direct-call shortcut or syntax-based callable rebuild
- type-only dispatch/query fixtures for nested calls under consumer endpoints,
  delayed numeric receiver/argument closure, empty list/record literals that do
  not publish heuristic types, and fresh dispatch-site instantiators that do not
  inherit enclosing substitutions or previous template roots
- static dispatch method lookup fixtures for chained dispatch, dotted field
  access with no call args, delayed numeric method regions, method identifier
  diagnostic subregions, and rejection of ambiguous non-functional dispatcher
  roots before publication
- mono nested-call specialization under consuming endpoints
- mono constraint-preserving clone-instantiation before final closure
- mono constructor lowering from checked expression type, not singleton tag
  syntax
- row-finalized shape-store fixtures for `RowShapeStore`, `RecordShapeKey`,
  `TagUnionShapeKey`, `RecordInit`, `TagInit`, record/tag shape interning,
  finalized ids on every row operation, duplicate-label verifier failures,
  unresolved row/tag-tail verifier failures, and no later field/tag name lookup
- lifted capture slots, capture args, capture refs, and direct-call metadata
- lambda-solved callable representation, Box-only erasure, solve sessions, and
  finite callable sets
- stack-safe lambda-solved lowering for deep low-level expression spines,
  generated string concatenation, and glue chains while preserving operation
  order and value-flow records
- artifact callable descriptor keys versus session-local lambda-solved
  descriptors carrying exact `ProcRepresentationInstanceId`, with executable
  branches selecting targets from session descriptors only
- persisted finite erased adapter members carrying member id, source proc,
  proc value, member payloads, lifted owner payloads, executable key, arg,
  capture, and result transforms
- finite-set erased adapter plan fixtures for `FiniteSetErasePlan`,
  `FiniteSetEraseAdapterBranchPlan`, singleton finite set through `Box`,
  multi-member branches, raw arg/result endpoints, descriptor-order validation,
  erased ABI equality, capture-shape agreement, and rejection of synthesized
  missing adapter members
- promoted wrapper finite-adapter normalization to the promoted wrapper
  executable signature while preserving concrete procedure identity fields
- lambda-solved fixed-point loop ordering: solve sessions, callable emissions,
  value-transform adapter demands, erasure requirements, finite adapter member
  reservation, executable demands, cross-procedure edges, then repeat
- `FiniteErasedAdapterDemand` coverage for nested transforms in records,
  tuples, tags, lists, boxes, captures, returns, call args/results, and erased
  adapter member captures
- shared proc-value member reservation for repeated `proc_value` occurrences of
  the same `MirProcedureRef` with occurrence-local construction plans
- `ValueFlowGraphBuilder` state and publication order, including return
  value-flow edge publication before transform finalization
- canonical callable-set descriptor member ordering, capture-slot ordering,
  forbidden descriptor fields, `ErasedCallSigKey`, and `CaptureShapeKey`
- full `ErasedAdapterKey` identity `{ source_fn_ty, callable_set_key,
  erased_call_sig_key, capture_shape_key }` with descriptor-order, erased-ABI,
  and capture-shape validation
- procedure boundary executable payload publication for params, returns, and
  capture slots
- promoted callable capture-slot payload publication, including empty aggregate
  captures such as `List(vacant_callable_slot(...))`
- lambda-solved `CallableSetConstructionPlan` ownership, selected member
  agreement, capture schema agreement, and exact source function type agreement
- `CallableValueEmissionPlan` fixtures covering finite callable sets,
  already-erased values, proc-value-to-erased plans, finite-set erased adapters,
  missing-plan verifier failures, wrong solved-group verifier failures, and
  erased plans without provenance
- proc-value owner sealing fixtures for `PendingProcValueCallablePlan`,
  `ProcValueOwnerSealing`, descriptor-only procedure instances, explicit
  `MaterializedProcedureDemand`, and function-valued captures that depend on
  other callable groups
- dynamic solve-session membership and recursive session fixed points
- executable callable-set construction, `callable_match`, erased packing, and
  source `match` decision plans
- executable callable-set value fixtures for `CallableCaptureRecord`,
  `CaptureValueRef`, capture-slot order, zero-sized capture metadata,
  branch-local direct-call result, mandatory identity result transform, and
  exactly one shared result assignment per returning branch
- executable callable-set construction verifier fixtures for valid
  `CallableSetConstructionPlan` consumption, missing construction plan, wrong
  result value, wrong finite emission plan, missing descriptor member, source
  function type mismatch, capture count/order mismatch, zero-sized capture
  metadata, and transformed capture type mismatch
- full `CallableMatch`/`CallableBranch` node shape: `func_tmp`, `arg_temps`,
  `direct_args`, branch-local result, identity result transform, `no_return`,
  and one shared result assignment
- exact source function type equality across descriptor member, callable-match
  request, and branch executable specialization
- executable `CallProcExecutablePlan` conversion to `call_direct`
- source `match` representative binder rewriting, guard scope, degenerate branch
  metadata, and reachability metadata
- selected-tag summary algorithm coverage for aggregate construction, aliases,
  projections, proc captures, joins, returns, loop phis, mutable versions,
  runtime unknown producers, and conservative union
- procedure public-root replacement before source-match reachability for params,
  returns, and captures
- source `match` IR lowering: Bool patterns as ordinary tag/union-id tests,
  integer/decimal/float/string/list-length tests, guard materialization,
  path-local temp cache limits, degenerate leaves, and shared result joins
- value/path-specific selected-tag reachability and source-match branch
  ownership
- unreachable source-match alternatives skipping callable emission and
  dependency publication
- vacant callable slots proving uninhabited function-typed structural slots are
  not values or call targets
- executable type key/payload separation
- single-typed `ExecutableValueRef` definitions
- executable local-id verifiers for `LocalValueId`, `LetValue`, eval
  boundaries, branch-local scopes, runtime uniqueness mutation sites,
  use-before-definition, out-of-scope use, and branch-local escapes
- bridge single-evaluation from already evaluated value refs
- projection endpoint-authority fixtures for record, tuple, tag-payload,
  list-element, and Box-payload projections; `ProjectionInfo.endpoint_slot`;
  raw-to-logical bridge use; recursive aggregate projection; and rejection of
  source-shaped payload records followed by whole-record repair
- IR consuming executable MIR only
- program literal pool ids preserved through MIR and IR, then interned into
  `LirStore`
- checked literal boundary fixtures for `CheckedStringLiteralId` indexing one
  owning artifact's `CheckedBodyStore.string_literals`, mono resolution into
  the `ProgramLiteralPool`, imported checked literal bytes, interpolation
  segments, bytes literals, string patterns, crash messages, and rejection of
  raw parser/checker literal ids after the checked artifact boundary
- `ProgramLiteralPool` lifetime tests for source literal ids, artifact literal
  ids, `ProgramLiteralId`, final `LirStore` literal ids, mono-created initial
  pools, one-time IR-to-LIR interning, and rejection of later independent
  source-byte re-interning
- LIR statement-only bodies and committed layouts
- ARC insertion of explicit `incref`, `decref`, and `free`
- ARC consumed-token behavior, result-alias behavior, branch cleanup, loop
  cleanup, switch continuation cleanup, and early-return cleanup
- full `LowLevelRcEffect` metadata: allocation/retain/release flags, runtime
  uniqueness candidate args, `consume_args`, `retain_args`, `retain_result`,
  and `result_aliases_consumed_args`, with list/string/box examples
- low-level RC-effect operation fixtures for `List.concat`, `List.drop_at`,
  `List.sublist`, `List.reserve`, strings, boxes, consumed-token ordering
  before suffix rewrite, and `result_aliases_consumed_args` subset checks
- LIR write ownership transfer through `SetLocalWriteMode`, branch-result
  initialization, mutable replacement ordering, join-param initialization,
  `assign_ref.local` move-vs-copy, source-token transfer, shared continuation
  summaries, and branch-local cleanup before continuation
- `ForListElementSource.aliases_iterable_element` loop-element alias fixtures
  proving borrowed elements are not decrefed as owned values and iterable
  keep-sets preserve alias validity across calls, branches, and continues
- ARC correctness baseline: `decref` before optional direct `free`, ZST
  containers as refcounted when their outer allocation has a token,
  stack-safe cleanup traversal, mutation barriers, keep-set reachability, and
  switch continuation summaries
- low-level value-flow signatures with `LowLevelValueFlowEdge.arg_to_result`,
  `arg_to_result_projection`, and `produced_from_args`, plus box boundary
  intrinsic records
- required low-level operation signatures for `List.get_unsafe`, `List.set`,
  append/prepend/concat/split, `Str` result provenance, `Box.box`,
  `Box.unbox`, and call-only intrinsics
- executable-only low-level authorization inputs: const materialization plans,
  erased capture plans, executable value transforms, proc-value erase plans,
  erased signatures, boxed boundaries, executable type payload refs,
  callable-set descriptors, finalized row ids, and committed layout graph
  records
- backend/interpreter consumption of explicit LIR RC statements
- interpreter storage rules: callee parameter materialization, committed
  size/alignment for all value storage, literal carrier width vs storage width,
  join-parameter materialization, caller-owned caller cleanup, callee-owned
  param cleanup, call mutation barriers, and explicit consumed low-level arg ABI
  cases
- backend call ABI clobbering tests proving simultaneous argument placement
  materializes stable sources before overwriting parameter registers or stack
  slots
- runtime-image shared-memory headers/views and child-side execution without
  checked artifacts
- local runtime-image arena/view construction, image slices, literal pool views,
  root proc ids, and interpreter shim boundaries
- runtime image publication through existing `SharedMemoryAllocator`,
  file-backed and local arenas, freestanding playground compiler-artifact hash
  matching, interpreter-owned runtime arena, hosted table image, and platform
  root mappings
- glue input catalogs and runtime value schemas
- semantic glue catalog records for `GlueInputCatalog`, `GlueModuleInfo`,
  `GlueFunctionInfo`, `GlueHostedFunctionInfo`, `GlueEntrypointInfo`, and
  `GlueProvidesEntry`, derived strictly from checked artifacts, relation views,
  import views, and runtime schemas
- glue value materialization through schema writers
- glue runtime schema tests for `RuntimeValueSchemaStore`, layout-directed input
  writing, output extraction by exact LIR return layout, small-string copying,
  and display-name normalization
- C glue erased callable header tests for `struct RocOps` forward declaration
  before `RocErasedCallableFn`, erased callable function/drop typedef ordering,
  payload struct shape, erased callable pointer type, capture alignment macros,
  payload alignment macros, capture-offset macro, and payload-size macro
- runtime-image shared-memory object graph validation for magic, version,
  target, bounds, alignment, roots, hosted table image, platform root mappings,
  zero-copy child-side views, and no child-side `ModuleEnv`, CIR, checked
  artifacts, MIR, or IR
- readonly static data graphs, static heap allocation prefixes, and static
  refcount behavior, including `TargetStaticDataGraph`,
  `TargetStaticDataNode`, `TargetStaticDataRelocation`,
  `REFCOUNT_STATIC_DATA`, and `static_data_ptr_offset`
- issue 9401 readonly data completion coverage: host-linkable symbols, nested
  heap values, static refcount no-ops, decoupled eval/export, identical ABI/RC
  model, and boxed callable host-boundary behavior
- readonly data export dependency collection for provided exports after
  artifact publication, separate layout-request and code-dependency streams,
  non-root lambda-solved materialization sessions, executable adapter
  collection, and consumer-only static data emission
- provided readonly export dependency tests for `ProvidedExportTable`,
  procedure/data export splitting, compile-time-root lookup by export pattern,
  missing compile-time root verifier failure, `MaterializationDependencyRoot`
  append to mono input, `reserveComptimeDependencySummaryDependencies`,
  non-root lambda-solved materialization sessions, erased callable static data
  dependencies, and no runtime root or top-level thunk pollution
- readonly materialization solve sessions and executable adapter collection
- lowered erased callable code maps and deterministic local text symbols for
  readonly callable payloads
- lowered erased callable code map validation for code ref, LIR proc id, source
  function type, executable arg/return keys, capture shape, erased signature,
  and object symbol
- boxed erased callable ABI, capture alignment, explicit `on_drop`, host helper
  generation, and interpreter trampoline behavior
- interpreter erased callable context records containing interpreter pointer,
  LIR proc id, optional semantic capture layout, semantic capture byte offset,
  and code-map lookup
- sealed boxed erased callable constants, zero-sized hidden captures, and static
  nested capture data
- boxed erased callable constant materialization with explicit layout requests,
  16-byte alignment verification, nested sealed erased callables, direct/finite
  adapter code collection, and no capture-free inference from source lambda
  syntax or wrapper code
- boxed erased callable constant boundary source type selection and
  `ErasedCallableResultPlan` / `ErasedCallableLeafInstance` publication
- `ComptimeCallable` finite and erased values, including capture values,
  selected finite member identity, erased code/capture/provenance, and no thunk
  or runtime global callable encoding
- `CallableResultPlan`, `ErasedCallableResultCodePlan.materialized_by_lowering`,
  `.read_from_interpreted_erased_value`, and `ErasedCaptureReificationPlan`
- private capture graphs through `PrivateCaptureRef`,
  `PrivateCaptureInstantiationKey`, `PrivateCaptureNode`, private const
  leaves, and rejected erased callable leaves in source private captures
- promoted wrapper lifecycle with `PromotedCallableWrapper`, promoted graph
  nodes/keys, reserve-before-fill, body plans, private capture construction,
  and sealed wrapper verification
- promoted procedure table fixtures for `PromotedProcedureTable`,
  artifact-owned `PromotedProcedureRow` data, sealed checked procedure
  templates, stable `ProcBaseKey`, debug-only `source_binding`, finite and
  erased `PromotedCallableBodyPlan` variants, and `PromotedWrapperParam`
  payload ordering
- `ComptimeValueNode` logical values, callable leaf templates/instances, sealed
  erased callable values, artifact-local schema names, and remapping
- `CompileTimePlanStore` paired cache restore with `CompileTimeValueStore`,
  zero-copy imported materialization plan views, and no imported-module rerun
- const template and instantiation records: `ConstOwner`, `ConstRef`,
  `ConstTemplate`, `ConstEvalTemplate`, `ConstValueGraphTemplate`,
  `ConstInstantiationKey`, request, store, state, and instance
- const producer type vs consumer endpoint projection/bridging
- source-visible genericness for constants independent of builtin internals
- nominal declaration edges and row-extension normalization during const
  reification
- `ConstMaterializationPlan` node family and `const_instance` elimination before
  IR
- `ConstMaterializationContext`, artifact-local name remapping, transparent
  alias vs nominal/newtype materialization, pure vs general materialization
  modes, captured finite callable materialization limits, and
  `NoReachableCallableSlotsProof`
- recursive slot commit mappings, nominal source-keyed layout identity, and
  raw-to-logical bridges for recursive list element extraction
- box payload capability tests for imported transparent nominals, opaque atomic
  proofs, hosted ABI representation, recursive nominal payloads, exact
  specialization-local type args, boxed-payload representation modes, and
  missing capability/proof verifier failures
- `NoReachableCallableSlotsProof` tests for closed backing proofs,
  instantiated-args proof terms, opaque atomic payloads, and rejected attempts
  to treat missing proof data as atomic outside the defining module
- `RepresentationStore` tests for expression results, binders, pattern
  binders, proc params/returns, capture slots, call requested functions,
  proc values, mutable versions, loop phis, structural child edges,
  `RepresentationRequirement.require_box_erased`, and
  `BoxErasureRequirement` provenance
- erased `Box(T)` requirement tests for proc-value owner reservations, direct
  call args/results, finite-call branch args/results, erased-call ABI args,
  aliases, returns, branch joins, loop phis, mutable versions, platform
  wrappers, structural children, and same-ABI transparent retyping
- `CompileTimeRootTable` tests for selected roots with filled sparse summaries,
  unselected pending roots without summaries, `.expect` payloads, `.test_expect`
  selected only by the test runner, and ordinary builds rejecting test-only root
  evaluation
- runtime summary mode tests proving runtime roots seal concrete
  `ConstInstantiationKey`, `CallableBindingInstantiationKey`, and
  `ProcedureCallableRef` dependencies without producing
  `CompileTimeEvaluationPayload`, executable MIR, IR, LIR, backend input,
  interpreted results, runtime root lists, top-level thunks, or initializer
  procedures
- callable binding instance tests for `reserved`, `evaluating`, and `evaluated`
  states; direct versus evaluated bodies; direct instances without fake result
  or promotion plans; evaluated instances with private executable roots; and
  `CallablePromotionOutput` choosing existing versus promoted procedures
- callable result and capture reification graph tests for
  `FiniteCallableResultPlan`, `CallableResultMemberPlan`,
  `CaptureSlotReificationPlan`, serializable leaves, callable leaves, callable
  schemas, records, tuples, tag unions, lists, boxes, nominals, recursive refs,
  and exact checked boundary source payloads
- callable promotion graph tests for `CallablePromotionPlan`,
  `PromotedCallableNodeKey`, `PromotedCallableGraphNodeId`, debug-only
  `PromotedCallablePathKey`, recursive callable capture groups,
  `ConstInstantiationKey` capture leaves, reserve/fill/seal ordering, and
  dependency discovery only from precomputed plans
- root request tests for `RootAbi`, `RootExposure`, deterministic
  `RootOrderKey`, `LoweringPurpose`, `RootRequestTable`, generic exports
  remaining templates, local compile-time root payload filling, and
  imported/platform `RootTemplateSelection` carrying imported or relation
  closures
- imported template closure verifier tests for missing private refs, missing
  checked type roots/schemes, closure-carried direct/proc-value reservations,
  no parameterized summary data in imported closures, and concrete summaries
  owned by the requesting artifact
- bridge and aggregate assembly tests for `BridgeInput`, record field edges,
  tuple item edges, tag payload edges, list item edges, recursive `Logic`
  construction, recursive match binders, list reinterpret identity transforms,
  raw-to-logical recursive bridges, and rejection of source-shaped temporary
  payload repair
- glue schema tests for `RuntimeRecordSchema`, `RuntimeTagUnionSchema`,
  declaration-order versus row-finalized runtime-order differences, field
  offset lookup by logical field index, tag payload extraction by committed tag
  and payload indices, small-string copying, display-name normalization, and
  compiler-generated runtime schema extension before IR/LIR
- runtime image tests for `LirRuntimeImageHeader`, `ArrayRef`,
  `LirStoreImage`, `LayoutStoreImage`, `ProgramLiteralPoolImage`,
  `HostedProcTableImage`, full child-side validation, zero-copy views,
  interpreter-owned runtime arena allocation boundaries, and result reification
  before arena discard

Behavioral tests cover:

- generic higher-order calls
- generic higher-order calls whose nested call argument is specialized by the
  consuming endpoint, such as `List.fold(List.with_capacity(...), append_one)`
- closures and recursive closures
- recursive callable captures
- callable values in records, tuples, lists, tag payloads, boxes, and constants
- callable values nested inside non-function compile-time constants
- generic constants instantiated at multiple concrete types
- imported constants and imported callable eval templates without imported
  module re-execution
- boxed erased callables across the host boundary in both directions
- host-created and Roc-created boxed function captures, including nested records
  and recursive tag unions
- boxed erased callable constants emitted as readonly data
- boxed erased callables whose captures contain another boxed erased callable
- nested source `match` over tags, records, tuples, lists, literals, guards, and
  multi-scrutinee patterns
- source `match` alternatives that bind the same source name through different
  pattern paths and then use it in a guard and body
- promoted closures whose captured tag value proves one source-match branch
  unreachable while another branch remains reachable
- static dispatch where the dispatcher type appears in receiver, return,
  argument, and `where`-only positions
- structural equality selection only when checking permits it
- equality dispatch calls from generic equality implementations that appear as
  checked dispatch expressions
- delayed numeric defaulting with static dispatch and empty containers
- compile-time constants, callable promotion, and unused compile-time-only work
  staying out of final binaries
- compile-time dependency graphs with finite callable calls, erased callable
  calls, supplied erased callables, and value-graph template dependencies
- readonly exported constants, including `List(List(Str))`, boxed children, and
  boxed erased callable constants
- readonly exported strings, lists, recursive boxed tag unions, records with
  nested heap values, and Bool values as ordinary tag-union data
- readonly boxed erased callable constants with direct wrappers, finite
  adapters, zero-sized hidden captures, and nested static capture data
- checked artifact cache key positive and negative cases
- checked artifact cache hits restoring compile-time value stores together with
  checked artifact data
- platform/app requirement publication
- platform/app relation tests for open `requires` rows, transparent alias
  unwrapping, app aliases inside `Box(T)` and function boundaries, and
  directional return-row compatibility
- platform-required const and procedure values consumed through relation-owned
  authorization closures
- readonly host-linking symbols named `roc__answer`, `roc__table`,
  `roc__names`, and `roc__tree`
- nested readonly static data from compile-time evaluation, including records
  with strings inside `List(List(Str))`, boxed children, recursive tag unions,
  and values accessible to hosts as immutable constants
- host-created boxed erased callables with primitive captures, nested-record
  captures containing strings, recursive-tag captures, and captures containing
  another boxed erased callable
- Roc-created boxed erased callables passed to the host, called immediately,
  stored by the host after incref, called later after Roc drops its local
  reference, released by the host, and round-tripped back to Roc
- runtime-image shared memory publication

Regression tests that expose a compiler semantics bug through a platform test
should also get a narrower eval-level reproduction when possible. Platform tests
then verify host boundary behavior; eval tests isolate compiler semantics.

## Forbidden Patterns

These patterns are not allowed in production compiler stages:

```text
source type reconstruction after checking
owner recovery from expression position
method target recovery from syntax
compatibility-shape repair
expression-indexed semantic maps as semantic truth
callable target lookup by source function type
capture recovery from executable body scanning
post-check dispatch nodes
runtime top-level constant thunks
runtime global initializer procedures for constants
runtime zero-argument constant wrappers
runtime top-level closure objects
runtime global callable objects for top-level bindings
post-check imported-module LIR re-execution
non-Box erased callable boundaries
backend reference-counting policy
static borrow/ownership summaries
Bool runtime special cases
serialized semantic runtime images for child execution
sentinel/default values that can be reported as real results
```

If a change seems to require one of these patterns, stop and revisit the
producer boundary that should have published the required data.
