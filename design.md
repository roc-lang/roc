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
dependencies on the old middle. Any hosted, platform, symbol, or literal
metadata currently reached through MIR or IR modules must move to neutral
modules before those old middle modules are deleted.

Everything in between those boundaries is a Cor-style typed IR pipeline:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono IR
  -> LIR
  -> ARC insertion
  -> backend, interpreter, or LirImage
```

There is no separate MIR layer. There is no separate stored layout IR between
Lambda Mono IR and LIR. Layout selection is owned by the direct Lambda Mono IR
to LIR builder.

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
relation publication, compile-time constant evaluation, and checked module
publication have all completed. After a checked module is published, every
violated assumption is a compiler bug:

```text
debug build: debug-only assertion
release build: unreachable
```

Post-check stages do not return user-facing checking errors. They do not emit
fallback code. They do not silently repair missing data. They do not add
release-build runtime checks for compiler invariants.

Checked identity and runtime encoding are separate facts. A stable id, checked
id, symbol, type variable, procedure reference, callable member, or source row
id may identify what a value means. It must not also be treated as the integer
tag, variant slot, byte offset, ABI register class, object symbol, or memory
layout used to represent that value. The stage that commits a runtime encoding
publishes the explicit mapping from checked ids to runtime encodings, and all
later stages consume that mapping.

Backends do not reason about reference counting. They lower and execute the
explicit LIR `incref`, `decref`, and `free` statements emitted before backend
code generation. Reference-counting policy belongs to LIR ARC insertion.

The compiler does not contain a static borrow, alias-permission, lifetime,
uniqueness, parameter-mode, or escape-summary model in this architecture.
Automatic reference counting is intentionally simple and mechanical. Runtime
mutation uses `refcount == 1` to decide whether in-place mutation is allowed.

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

Post-check names should be short and precise. Do not encode whole explanations
into long compound type or function names. Prefer a small local vocabulary such
as `FnSet`, `FnVariant`, `FnTemplate`, and `CaptureSlot`, then define the exact
meaning in the surrounding design or module comments. Longer names are reserved
for cases where two nearby concepts would otherwise be genuinely ambiguous.
Avoid vague compiler jargon when a plain name is available. The words `bridge`
and `projection` are banned in new post-check docs, APIs, modules, type names,
variable names, and comments. They may appear only in this ban or when quoting
pre-existing code that is scheduled for deletion. Say the specific operation
instead, such as conversion, field read, tag payload read, capture slot, or
wrapper function.

The terms `readback`, `reification`, `value graph`,
`compile-time value store`, and `representation repair` are also banned in new
post-check docs and code. Compile-time evaluation stores results in
`ConstStore`; later compilations restore cached consts from `ConstStore`.
There is no separate phase or store under the const-related names. If two
representations unexpectedly differ for the same value at the same boundary,
that is an invariant violation; fix the producer instead of adding a later
conversion.

Outside the existing Canonicalization phase and existing files scheduled for
rename or deletion, the word `canonical` is banned in new post-check docs and
code. Use the exact term instead: `authoritative` for source-of-truth documents,
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

The word `physical` is banned in new post-check docs and code. Use `layout`
only for memory shape facts such as size, alignment, field offsets, and payload
layout. Use `runtime encoding` for the broader category that includes layouts,
discriminants, callable variant encodings, erased callable code entries, ABI
shape, and runtime schemas.

The word `artifact` is banned in new post-check docs and code. Use the precise
owner instead: `CheckedModule`, `CheckedModuleBuilder`, `checked module cache`,
checked module data, platform relation data, or another exact producer/consumer
name.

The word `semantic` is banned in new post-check docs and code. Use the precise
term instead: checked data, checked facts, checked type store, source meaning,
checked identity, source row position, `FnDef` identity, `FnSet` context, or
another exact stage-owned name.

The word `executable` is banned in new post-check docs and code except in the
phrases `executable binary` and `executable program`. Use the precise stage or
data name instead: Monotype, Lambda Mono, LIR, direct call target, lowered
value, runtime layout, or checked function template.

The word `obligation` is banned in new post-check docs and code. Use the exact
owner instead: checked dispatch plan, erased callable requirement,
specialization queue entry, debug assertion, or deletion target for old code.

## Cache Boundary

The checked module cache is the only checked cache boundary in this design.

The checked module cache id is target-independent:

```text
CheckedModuleId =
    source_hash
  + compiler_build_hash
  + module_identity
  + checking_context_identity
  + direct_import_checked_module_ids
```

The cache id does not include target ABI, pointer width, layout ids, field offsets,
alignment decisions, backend choice, object format, code-generation options, or
post-check specialization state.

Module boundaries are cache boundaries only. They must never change the final
runtime behavior or performance of the compiled program, except for debug
information. Compiling one large module and compiling the same code split across
imports must produce the same reachable specializations, callable
representations, layout decisions, ARC statements, and backend behavior.

The compiler does not cache Monotype IR, Monotype Lifted IR, Lambda Solved IR,
Lambda Mono IR, LIR, or any callable/layout representation derived from them as
part of checked modules. Those structures are target/session products of the
current root compilation.

Monotype IR is target-independent, but it is still post-check and root-specific.
It depends on the roots requested for the current compilation, the reachable
monomorphic specializations, and the static-dispatch and source-loop lowering
performed for that compilation. `ConstStore` entries in checked modules are
therefore checked-stage stored constants, not Monotype nodes.

The checked module cache stores checked Roc values only. Roc language values are
target-independent except for pointer-sized Roc values if the language exposes
them to compile-time evaluation. Compiler runtime representation facts are not
Roc values and must never enter `ConstStore`: runtime addresses, allocation
identity, layout ids, runtime discriminants, field offsets, LIR proc ids,
backend symbols, object-format details, and backend state are all outside the
checked value domain. Host interaction exists only at runtime, so host handles
and host results cannot be compile-time values. If Roc exposes pointer-sized
values to compile-time evaluation, their checked cache format must be an explicit
checked rule before such values may be published.

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

Those forms do not survive Monotype IR lowering.

The checked boundary publishes immutable checked modules. A published checked
module is complete or it is not published. Later stages may read checked
modules but may
not mutate checked source facts, perform additional user-facing checking, or
derive missing checked facts by scanning source syntax.

During checking finalization, compile-time evaluation may need to lower and run
checked roots before the checked module can be published. That work uses a
`CheckedModuleBuilder`, not a published checked module. The builder is the only
mutable owner of the in-progress `ConstStore`, dependency summaries, and checked
root payloads.

Compile-time lowering during checking finalization receives a
`CheckingFinalizationView`. That view exposes checked facts plus
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
representation data. In particular, the checked module does not contain
old middle runtime type payloads, old middle value conversion plans,
callable-set descriptors, erased callable ABI decisions, layout ids, runtime tag
discriminants, or backend encodings.

This is a checked-boundary rule, not merely a pipeline rule. Any checked
module field whose only purpose is to feed the old post-check middle is not
part of the checked boundary. Existing fields with names such as old middle
runtime payload, old middle value conversion, callable descriptor, erased ABI, runtime
encoding, or layout creation are boundary debt. A checked module boundary
checklist must classify each such field exactly once as a checked
fact to keep, a checked fact to add before deletion, or old
representation data to delete before the new post-check pipeline is the public
path.

The checked module may publish checked facts that later stages need, such
as:

- checked procedure and promoted callable templates
- `ConstStore` entries for compile-time constants
- checked dispatch plans
- method registries
- platform, hosted, and exposed function declarations
- opaque, nominal, alias, row, and builtin ownership facts

Those facts must remain target-independent and representation-free.

Imported checked modules must contain every checked procedure template and checked
body that may be instantiated by an importing root. This includes private helper
templates reachable from exported templates, static-dispatch targets, promoted
callables, and compile-time checked callable leaves. Privacy affects source
name lookup and diagnostics. It must not hide checked bodies from whole-program
post-check specialization.

### Function Definitions

`FnDef` is the checked identity of a callable body. It is used in
Monotype `fn_def` expressions and in compile-time checked callable leaves.

```zig
const FnDef = union(enum) {
    local_checked_template: ProcTemplate,
    imported_checked_template: ImportedProcTemplate,
    local_promoted_template: PromotedTemplate,
    imported_promoted_template: ImportedPromotedTemplate,
    local_hosted: HostedProcId,
    imported_hosted: ImportedHostedProc,
    checked_compiler_generated: GeneratedFn,
};

const ImportedProcTemplate = struct {
    module: CheckedModuleId,
    template: ProcTemplate,
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
```

A `FnDef` names the checked body/template to instantiate. It is not a
procedure value, LIR proc id, object symbol, erased ABI id, callable-set member,
layout id, or runtime code pointer. If a callable value captures data, the
captures are stored next to the `FnDef` by the value that contains it; the
`FnDef` itself remains capture-free.

`local_checked_template` is checked-module-relative while the owning builder/checked module
is being processed. Importers refer to the same body through
`imported_checked_template` with an explicit checked module id. Published imported
checked modules must contain the private checked templates reachable through these
references; consumers never recover imported callable bodies from source text,
display names, generated callable shapes, or runtime values.

`checked_compiler_generated` is only for checked-stage generated
templates that are part of the checked module contract. Functions generated
after checking, such as Lambda Mono specializations or adapters, use stage-local
symbols and are not `FnDef` values.

### Static Dispatch At The Checked Boundary

Checking reports all user-facing static-dispatch errors. This includes missing
methods, ambiguous constraints, illegal equality use, invalid iterator `for`
constraints, and any other error that should be shown to the programmer.

The checked module publishes normalized dispatch plans. A dispatch plan is a
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
    compiler_loop_iterator_state: IteratorLoopStateId,
};

const DispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
};
```

Source dispatch, type dispatch, method equality, and iterator `for` plans all
use this one shape. Iterator `for` contains two plans:

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
that determines the owner. The plan does not choose a concrete lowered call target
for every future monomorphic specialization. Concrete target selection needs
monomorphic type information, so it happens while producing Monotype IR.

The method registry is an exact table keyed by `(MethodOwner, MethodNameId)`.
It is not an owner-discovery mechanism. Post-check code may use it only after a
concrete monomorphic dispatcher type has already determined the owner.

## Shared Post-Check Model

Every post-check IR has a typed IR store:

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
- Lambda Mono IR owns a new type store with no function types.
- LIR owns committed layouts, not post-check type ids.

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
- Lambda Mono type id to committed LIR layout id
- Lambda Mono procedure id to LIR procedure id

Those tables are not checked-fact side channels. They must not contain facts that
are missing from the produced IR. If deleting a table would make it impossible
to understand what the output means, the table is an illicit representation
store and the design is wrong.

Stage-local algorithmic worklists are allowed for SCC traversal, unification,
pattern decision construction, layout graph traversal, and similar internal
algorithms. These worklists do not cross stage boundaries and do not publish
checked facts.

The only meaning-producing worklists in post-check compilation are stage-local
specialization queues. A specialization queue is driven by explicit calls or
roots discovered while lowering the previous stage. It is not a general
post-demand repair list.

## Monotype IR

Monotype IR is the first post-check typed representation. It keeps only the
expression and pattern forms that are meaningful after checking, and every
expression and pattern has a monomorphic type.

Monotype IR is produced from checked modules and explicit root requests. It
performs three jobs:

1. Clone and instantiate checked types into monomorphic type nodes.
2. Create exactly the monomorphic procedure/value specializations reachable
   from the requested roots.
3. Resolve checked static-dispatch plans to direct calls or checked structural
   equality before any source dispatch form can enter the output IR.

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
    erased,
};

const CheckedModuleId = enum(u32) { _ };
const TypeDefId = enum(u32) { _ };
const TypeDigest = struct { bytes: [32]u8 };

const TypeDef = struct {
    module: CheckedModuleId,
    id: TypeDefId,
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

`DispatchOwnerHead` is not stored as a duplicate field on every type node. It is
the result of a total function over Monotype type content:

```zig
fn dispatchOwnerHead(types: *const TypeStore, ty: TypeId) DispatchOwnerHead
```

Builtin type content returns the corresponding builtin owner. `named` type
content returns `type_def` when the definition can own methods, even if
its runtime representation later uses a transparent backing. Anonymous
structural types and non-owning internal nodes return `none`.

Record fields and tag variants are stored in lexicographic order by name. Tag
payloads are stored in payload position order. The index of a field or tag
inside its span is the source row position; it is not a runtime discriminant
or layout slot. Runtime tag discriminants, payload layout, and field offsets are
not chosen until direct LIR lowering commits layouts.

Monotype IR does not need a separate row-finalization stage. Row closure,
numeric defaulting, nominal backing instantiation, and structural child ordering
are completed while constructing Monotype types from checked types.

If Monotype lowering cannot construct a closed monomorphic type from checked
facts, that is a compiler bug.

### Row, Nominal, Alias, And Opaque Authority

Monotype lowering is the sole replacement for the old concrete-source-type
recovery layer. It consumes checked type facts and produces closed
Monotype type nodes directly.

For records and tag unions, checking publishes the checked row ids and stored
spans. Record fields and tag variants use lexicographic order by name. Tag
payloads use payload position order. Monotype lowering copies those spans
directly. It does not sort by display text, declaration spelling, runtime
encoding, or incidental map iteration.

For named types, checking publishes:

- the `TypeDef`
- whether the definition can own methods
- the checked type parameters
- the checked backing type and backing authority, when the compiler has a
  checked representation fact for this definition
- opacity/interface facts controlling whether the backing may be inspected
  by Monotype or used only for runtime layout

Monotype lowering instantiates those facts. It does not scan declarations to
rediscover a backing, owner, or field order. If a named type is opaque at the
current boundary, Monotype still preserves the named type node and therefore
the dispatch owner derivable from it. A `runtime_layout_only` backing may be
used by layout lowering to represent values, but it is not permission
for Monotype or static dispatch to inspect through the opaque boundary. If no
backing is present, any stage that needs the representation must consume a
separate explicit checked representation authority; it must not rediscover the
backing by scanning declarations.

This keeps static-dispatch ownership, source row order, and eventual runtime
layout as three separate facts.

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
```

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
- pending owner search

`FnDef` is the checked identity for a checked, imported, hosted, promoted,
or compiler-generated function. It does not contain a capture record, closure
layout, callable tag, erased ABI, or lowered call target. Captures remain ordinary
free variables until Monotype Lifted IR records them on lifted function
definitions.

### Monotype Specialization

Monotype specialization is root driven.

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

### Static Dispatch In Monotype

Static dispatch is resolved while producing Monotype IR.

For each checked dispatch plan in a body specialization:

1. Instantiate the checked dispatcher type into the current Monotype type
   store.
2. Instantiate the checked callable type into the current Monotype type store.
3. Lower every checked operand into the current body specialization.
4. Derive the concrete `MethodOwner` from the instantiated dispatcher type.
5. Look up `(MethodOwner, MethodNameId)` in the checked method registry.
6. Instantiate the target method callable type and verify it matches the plan's
   callable type.
7. Emit a direct `call_proc`, or emit structural equality when the checked plan
   explicitly permits it.

Any failure after checking is a compiler bug. Monotype lowering must not search
for possible owners by intersecting method registries or constraints.

Owner derivation is a type-content operation, not a registry operation.
Monotype type lowering preserves owner-bearing type identity in builtin and
named type nodes for every dispatcher type.

`type_def` covers named nominal, opaque, and alias definitions that can own
methods. Transparent backing representation is separate from this owner head.
Monotype lowering may later erase or reinterpret transparent wrappers for value
representation, but static dispatch reads the owner head before the dispatch
node is removed.

The owner algorithm is fixed:

1. Fully resolve the instantiated dispatcher type.
2. Compute its `DispatchOwnerHead` from Monotype type content.
3. If the head is `builtin`, use that builtin owner.
4. If the head is `type_def`, use that exact `TypeDef`.
5. If the head is `none` and the checked dispatch plan permits structural
   equality, emit structural equality.
6. Otherwise stop with a compiler invariant failure.

The algorithm never asks the method registry "which owners could match this
constraint?" The registry only answers exact lookups after the owner is known.

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
        Skip { count, rest } ->
            continue rest
```

Monotype `loop_` carries named parameters and `continue_` supplies the next
values for those parameters. Iterator `for` uses one loop-carried parameter: the
current iterator value. There is no hidden assignment and no mutation-only loop
state.

The exact step tag names and payloads come from the checked/builtin `Iter`
definition and the monomorphic iterator type. The `.iter` and `.next` calls are
resolved through the same Monotype static-dispatch path described above.

The `Skip` count is part of the iterator step value. A plain source `for` loop
does not bind it, but iterator adapters may use it to preserve efficient skip
information.

No `for` node exists after Monotype IR.

## Monotype Lifted IR

Monotype Lifted IR removes closures and local functions from expression
position. Its type store is the Monotype type store.

The expression language is intentionally close to Monotype IR, but:

- there is no `lambda` expression
- there is no local function definition inside an expression
- every function body is a top-level lifted definition
- each lifted function definition declares its capture symbols explicitly

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
definition; callable representation is not chosen until Lambda Mono IR.

The lifting pass owns free-variable analysis. It does not choose finite
callable representations, erased callable representations, closure object
layouts, or runtime tags.

## Lambda Solved IR

Lambda Solved IR introduces lambda sets into the stage-local type store and
solves callable flow.

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
    erased,
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

### Lambda Solving

Lambda Solved IR has the same lifted expression shape as Monotype Lifted IR.
Only the type store changes.

The solver:

- instantiates Monotype Lifted types into Lambda Solved type variables
- adds a fresh callable slot to every function type
- treats references to lifted function symbols as singleton lambda sets
- unifies callable slots through value flow and calls
- propagates erased callable requirements through the same type graph
- generalizes and instantiates polymorphic definitions
- solves recursive groups as groups, not by accidental declaration order

The solved type graph is the callable representation source of truth. There is
no descriptor replacement, no callable repointing, no post-demand payload
publication, and no representation recovery later.

### Erased Callable Requirements

`erased` callable requirements are explicit facts entering Lambda Solved IR.
They are not inferred from backend needs or recovered from runtime encodings.

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

Monotype lowering carries these requirements as typed checked annotations into
Lambda Solved IR. Lambda solving unifies them through the same function
`args/callable/ret` graph used for finite lambda sets. If a callable slot is
forced to `erased`, Lambda Mono lowering produces packed erased callable values
and indirect erased calls. If no explicit erased requirement reaches a callable
slot, finite lambda-set dispatch is used.

No ordinary source expression becomes erased because a later stage finds finite
dispatch inconvenient. Erasure is introduced only by one of the checked boundary
facts above.

## Lambda Mono IR

Lambda Mono IR consumes Lambda Solved IR and produces function-free typed IR
where callable representation is explicit in values and calls.

The Lambda Mono type store has no function type. Function values have already
become ordinary value representations:

- finite callable sets become generated tag-union values
- erased callables become packed erased callable values

```zig
const LambdaMonoType = union(enum) {
    primitive: Primitive,
    record: Span(Field),
    tag_union: Span(Tag),
    tuple: Span(TypeId),
    list: TypeId,
    box: TypeId,
    named: NamedType,
    erased,
};
```

A finite callable set is an ordinary generated tag union. Each lambda-set member
gets one generated tag. If the member captures values, the tag payload is a
generated record containing those captures. If it captures nothing, the tag is a
zero-payload variant.

The generated tag-union type carries the callable member identity needed by the
LIR builder. The identity is part of the type node, not a separate
representation table:

```zig
const FnTag = struct {
    variant: FnVariantId,
    display_name: ?TagLabelId,
    member: FnMember,
    capture_record: ?TypeId,
};

const FnMember = struct {
    lambda: Symbol,
    source_fn_ty: TypeDigest,
};
```

### Lambda Mono Expressions

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
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};
```

Lambda Mono uses the same loop-carried `LoopExpr` and `ContinueExpr` shape as
Monotype. A pass that preserves loops must preserve explicit parameters,
initial values, and continue values. LIR lowering is the first stage allowed to
turn that state into concrete jumps, blocks, or backend-friendly loop control.

Lambda Mono IR has no `call_value` node. A call through a finite lambda set is
lowered to a match over the generated callable tag union; each branch makes a
`direct_call` to the specialized function for that member. A call through an
erased callable becomes `indirect_erased_call`.

Generated callable variants are stage-local ids created by Lambda Mono. Any
`display_name` exists only for dumps or diagnostics. The runtime discriminant
and variant slot are chosen later by LIR layout commitment and then published
explicitly in the LIR result.

Lambda Mono specialization identity includes the called function symbol, its solved
function type, and the capture shape required by the callable representation.
This is a normal stage-local specialization queue, driven only by explicit
callable flow in Lambda Solved IR.

For a finite callable member with captures, the specialized function receives
the original Roc arguments followed by one compiler-created capture-record
argument. For an erased callable, the erased ABI contains the full ordered Roc
argument list and result layout. Neither path introduces currying or
partial-application wrappers.

Lambda Mono IR has no generic conversion expression. Any operation that must
survive to statement lowering is represented by a concrete expression form
above. Differences that are only layout choices are handled by layout
selection while lowering those concrete expressions.

If the direct LIR builder sees that Lambda Mono IR and the committed layouts
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
after-the-fact conversion.

## Direct LIR Lowering

LIR lowering consumes Lambda Mono IR directly.

There is no separate stored layout IR. The Lambda Mono to LIR builder owns:

- a layout builder that interns and commits recursive layouts from
  Lambda Mono type nodes
- a procedure builder that maps Lambda Mono procedure ids to LIR procedure ids
- a local builder that allocates LIR locals from Lambda Mono expression and
  binder types
- a pattern builder that consumes Lambda Mono patterns plus committed layouts
  and emits LIR switches, joins, and bindings directly
- callable lowering that turns generated callable tag unions into ordinary LIR
  tag operations and erased callable values into explicit packed-erased-callable
  statements
- bool predicate creation from ordinary Bool tag-union layouts
- runtime value schema publication from committed nominal layouts
- erased callable code map publication from Lambda Mono callable/procedure data

These are builder responsibilities, not a separate meaning-carrying IR.

The builder may maintain temporary maps such as `TypeId -> layout.Idx` and
`LambdaMonoProcId -> LirProcSpecId`. These maps are caches of work the builder
owns. They must not contain checked facts that are absent from Lambda Mono IR
or the LIR result.

### Direct Builder Internal Contracts

The direct LIR builder is one compiler stage, but its internal components have
explicit contracts so the stage does not become an implicit reconstruction layer:

- the layout builder consumes only Lambda Mono type nodes and emits committed
  LIR layouts plus explicit maps from checked ids to runtime encodings for
  direct-builder result data
- the procedure builder consumes only Lambda Mono procedure ids, root requests,
  and committed layouts, then emits LIR procedure ids and root metadata
- the local builder consumes Lambda Mono binder types plus committed layouts and
  emits LIR locals
- the pattern builder consumes Lambda Mono patterns plus committed layouts and
  emits LIR control flow
- callable lowering consumes generated callable type nodes and committed
  layouts, then emits ordinary tag operations or packed erased callable
  statements
- schema publication consumes committed nominal layouts and checked
  nominal identities

No internal component may inspect source syntax, checked bodies, display names,
runtime bytes, backend symbols, or old middle data. Internal maps are work
caches only. If an internal component needs a fact that is not in Lambda Mono IR,
committed layouts, checked identities explicitly passed to the builder,
or the LIR result it is constructing, the earlier stage contract is incomplete.
The direct builder must not invent conversion operations to repair a mismatch
between Lambda Mono IR and committed layouts.

The direct builder returns one explicit output object:

```zig
const LirLowerOutput = struct {
    store: LirStore,
    layouts: LayoutStore,
    root_procs: Span(LirProcSpecId),
    root_metadata: Span(RootMetadata),
    requested_layouts: Span(RequestedLayout),
    runtime_schemas: RuntimeSchemaStore,
    fn_sets: Span(FnSet),
    erased_fns: Span(ErasedFns),
    capture_slots: Span(CaptureSlot),
    erased_entries: Span(ErasedEntry),
};
```

`store`, `layouts`, `root_procs`, and `root_metadata` are the normal LIR output
consumed by ARC and then by backends, the interpreter, and LirImage.
`requested_layouts` is for static data and provided data exports that asked for
layout decisions during the same lowering. `runtime_schemas` is for glue and
static data. `fn_sets`, `erased_fns`, and `capture_slots` are temporary
compile-time publication contexts used by `CheckedModuleBuilder` while storing
function values in `ConstStore`. `erased_entries` maps erased callable LIR
procedure ids to the checked callable templates needed by compile-time
publication and glue.

The output owns all of these stores and spans. Consumers borrow the fields they
need and must not add their own side stores for the same facts. `LirImage`
contains only the ARC-inserted LIR fields: `store`, `layouts`, `root_procs`,
platform entrypoints, and target usize.

### Layout Selection

Layout selection is the first stage that chooses runtime encodings:

- struct field order
- tag-union variant order and discriminants
- zero-sized representation
- boxed recursive slots
- list backing layout
- erased callable payload layout
- ABI-visible procedure argument and result layouts

Layout selection consumes Lambda Mono types and produces LIR layouts plus the
runtime schemas and function result data that later compile-time publication,
static data export, and glue code need. Later stages consume those explicit
layouts, schemas, and function result data. They do not rediscover field order,
tag discriminants, callable member encodings, or erased callable payload shape.

When layout commitment assigns a runtime discriminant or field offset to a
generated function tag, the builder publishes the mapping from the stage-local
`FnVariantId`/`FnMember` to the runtime encoding in direct-builder result data
for `ConstStore` publication and static data export. `LirImage` does not store
function runtime data. It contains only ARC-inserted LIR, committed layouts,
root proc ids, platform entrypoints, and target usize.

### Pattern Lowering

Pattern decision construction is part of the direct LIR builder. It consumes
Lambda Mono patterns and committed layouts and emits LIR control flow. There is
no persisted pattern-decision IR.

### ARC

The direct LIR builder emits ownership-neutral LIR. ARC insertion runs after
LIR construction and emits explicit `incref`, `decref`, and `free` statements.
Backends and LirImage builders follow those statements mechanically.

## Compile-Time Constants

Compile-time constants use the same post-check pipeline as runtime code while a
checked module is being finalized:

```text
checked CIR
  -> CheckedModuleBuilder during checking finalization
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono IR
  -> LIR
  -> ARC insertion
  -> LIR interpreter
  -> store eval result in ConstStore
```

The compile-time evaluator is an LIR interpreter. It does not interpret
Monotype IR, Lambda Solved IR, Lambda Mono IR, or any source-level IR.

The LIR interpreter produces a runtime value. Checking then stores that eval
result as checked-stage data in the checked module's `ConstStore`. `ConstStore`
stores checked Roc values only. It does not contain Monotype nodes, Lambda
Solved data, Lambda Mono data, runtime addresses, allocation identity, layout
ids, runtime discriminants, field offsets, LIR locals, LIR procedure ids,
backend symbols, backend bytes, or host handles.

`ConstStore` uses node ids so stored constants can preserve sharing and cycles
without expanding forever. Multiple fields may reference the same `ConstNodeId`.
A recursive value uses a `ref` edge back to an existing node.

```zig
const ConstStore = struct {
    nodes: []const ConstValue,
    roots: []const ConstNodeId,
};

const ConstValue = union(enum) {
    scalar: ConstScalar,
    string: StringLiteralId,
    list: Span(ConstNodeId),
    tuple: Span(ConstNodeId),
    record: Span(ConstField),
    tag: ConstTag,
    box: ConstNodeId,
    declared: ConstDeclared,
    fn_: ConstFn,
    ref: ConstNodeId,
};
```

`ConstScalar` is a closed checked scalar representation:

```zig
const ConstScalar = union(enum) {
    signed_int: struct { width: IntWidth, bits: u128 },
    unsigned_int: struct { width: IntWidth, bits: u128 },
    f32_bits: u32,
    f64_bits: u64,
    dec: DecBits,
};

const IntWidth = enum {
    @"8",
    @"16",
    @"32",
    @"64",
    @"128",
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
module has not been published yet. User-written compile-time crashes, exhausted
compile-time limits, invalid compile-time host interaction, and unsupported
compile-time operations become checking diagnostics attached to the checked root
being finalized. OOM remains OOM. A post-check invariant failure while lowering
or interpreting a compile-time root is still a compiler bug, not a user-facing
diagnostic.

While storing an eval result, the builder may reserve a `ConstNodeId` and fill
it later to represent cycles or shared recursive structure. Publication verifies
that every reserved node was filled exactly once. Restoring cached consts and
dependency summarization must memoize by `ConstNodeId`, so sharing and recursive
references are preserved and traversal terminates. A consumer must not recover
stored-const identity by comparing node contents.

A stored function value keeps checked identity only:

```zig
const ConstFn = struct {
    function: FnDef,
    source_fn_ty: CheckedTypeId,
    captures: Span(ConstCapture),
};

const ConstCapture = struct {
    symbol: Symbol,
    value: ConstNodeId,
};
```

`function` names a checked, imported, hosted, promoted, or compiler-generated
procedure template that the checked module owns or references explicitly.
`captures` bind the exact lexical capture symbols required by that template to
stored const nodes. A stored function does not store a lambda set, callable-set
descriptor, call specialization id, erased ABI, capture layout, runtime
tag, or LIR proc id.

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
    discrim: RuntimeDiscriminant,
    slot: RuntimeVariantIndex,
    payload: LayoutId,
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
    function: FnDef,
    source_fn_ty: CheckedTypeId,
};

const CaptureSlot = struct {
    symbol: Symbol,
    slot: u32,
};
```

`FnSetId` and `ErasedFnsId` are direct-builder result contexts produced while
lowering the specific value being evaluated. They live only for that lowering
and const-publication step. They are not stored in `ConstStore`, not serialized
in checked modules, and not stored in `LirImage`. For a finite singleton set,
storing the result selects the only `FnVariant`. For a finite multi-variant set,
storing the result reads the runtime discriminant and looks it up inside that
`FnSet`. For erased functions, storing the result reads the erased entry
procedure from the runtime value and looks it up inside the explicit
`ErasedFns` context.

`CaptureSlot` says which committed capture-payload slot contains the value for
one captured symbol. The direct LIR builder publishes these slots while lowering
the generated function value. The `ConstStore` writer recursively stores each
captured runtime value, then stores the resulting `ConstFn`.

Storing an eval result never uses a global id made only of layout,
discriminant, variant slot, byte pattern, display name, object symbol, or
payload shape. The function result-store data is temporary and is not serialized
as a checked module representation cache.

When a later compilation restores a cached const, Monotype lowering turns
`ConstStore` nodes into ordinary Monotype expressions:

- scalar and aggregate nodes become literal, record, tuple, list, tag, box, and
  nominal expressions
- zero-capture function values become `FnDef` expressions
- capturing function values become compiler-generated Monotype lambda
  expressions by cloning the checked template body referenced by `function`,
  alpha-renaming its parameters, and binding each captured symbol to the
  ordinary Monotype expression restored from the corresponding captured
  `ConstNodeId`

Restoring a cached const does not synthesize a wrapper that calls an
already-packed runtime function value. It builds an ordinary Monotype callable
from the explicit checked template and stored const captures, so the later
lifting and lambda-set solving stages see the same kind of ordinary callable
flow they would have seen if the value had been local source.

After that, closure lifting, lambda solving, lambda mono lowering, layout
commitment, and ARC run normally. This is what keeps module boundaries from
changing runtime performance: imported compile-time callables participate in the
same whole-program callable-flow solving as local callables.

Compile-time dependency summaries are produced from explicit checked root data
and `ConstStore` dependencies. They are not discovered by a later stage scanning
bodies for missing facts.

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
names, return types, or body absence. Hosted-argument ownership is an ABI fact,
and generated glue must document it for platform authors.

## Relationship To Cor LSS

The post-check design mirrors Cor's LSS experiment after solving, adapted for
Roc's checked module boundary and existing LIR.

| Cor LSS stage | Roc stage |
| --- | --- |
| solved source IR | checked CIR plus checked type store |
| `monotype` | Monotype IR |
| `monotype_lifted` | Monotype Lifted IR |
| `lambdasolved` | Lambda Solved IR |
| `lambdamono` | Lambda Mono IR |
| `ir` | direct Lambda Mono IR to LIR builder |
| `eval` | LIR interpreter for compile-time evaluation |

Roc intentionally keeps Cor's post-solve shape:

- Monotype IR is closed, monomorphic typed IR.
- Monotype Lifted IR has top-level lifted functions and explicit captures.
- Lambda Solved IR stores callable flow in function types.
- Lambda Mono IR removes function types by turning finite function values into
  ordinary generated tag unions and erased function values into packed erased
  callables.

Roc adds language and implementation facts that Cor's experiment does not need:

- static dispatch and method registries
- checked module caches and imported checked bodies
- opaque, nominal, alias, builtin, platform, hosted, and exposed identities
- target-independent `ConstStore` values
- the existing statement-only LIR, ARC, LirImage, and backend boundaries

The main language difference is static dispatch. Roc keeps static dispatch
separate from checked types. Checking still reports every user-facing
static-dispatch error and publishes checked dispatch plans. Monotype IR lowering
uses those plans plus monomorphic type information to replace static dispatch
with direct calls before any later callable/lambda stage runs.

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
serialized Layout IR or MIR-like layer.

Cor's experiment also performs some final field and tag lookup by source label
inside its final lowering. Roc does not copy that part. Roc's checked and
post-check stages publish ordered spans and checked ids before direct LIR
lowering. Direct LIR lowering consumes those ids and span positions; it does not
look up record fields or tag variants by display label to recover missing row
relationships.

## Forbidden Shapes

The post-check pipeline must not contain:

- MIR as a separate compiler layer
- a persisted layout IR between Lambda Mono IR and LIR
- post-demand worklists
- using the old middle path as a source of truth for the new path
- comparing new output against old output to decide compiler behavior
- callable descriptor replacement
- callable value repointing
- late payload publication
- generic conversion expressions, post-hoc conversion plan tables, or mismatch
  patching lowering paths
- checked-module old middle runtime payloads, old middle value conversion plans,
  callable-set descriptors, or erased ABI decisions
- owner discovery by method-registry intersection
- backend reference-counting decisions
- user-facing errors after checked module publication
- release-build checks whose only purpose is maintaining compiler invariants

The allowed replacement is explicit stage ownership:

- checking owns user-facing diagnostics and checked facts
- Monotype owns monomorphic specialization and static-dispatch elimination
- Monotype Lifted owns closure lifting
- Lambda Solved owns callable flow in the type graph
- Lambda Mono owns explicit callable value representation
- LIR lowering owns committed layouts and statement lowering
- ARC owns reference-count insertion
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
- Monotype Lifted IR contains no closure expressions or local function
  definitions in expression position.
- Lambda Solved IR has every function type in `args/callable/ret` form.
- Lambda Solved IR has no unresolved callable slot before Lambda Mono lowering.
- Lambda Mono IR contains no function type and no value-call node.
- Lambda Mono IR contains no unresolved lambda set.
- Lambda Mono IR contains no runtime tag discriminants or layout ids.
- Checked compile-time stores contain only `ConstStore` data.
- LIR lowering receives only Lambda Mono IR.
- Backends receive only ARC-complete LIR.

If a boundary check fails, the compiler stops as a compiler bug.
