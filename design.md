# MIR Static Dispatch Design

## Goal

The compiler should have one explicit source of truth for every semantic fact at
each stage.

For static dispatch, the source of truth before monomorphic specialization is:

- the checked dispatch operation
- the checked dispatch constraint function type
- the checked method registry
- the specialized monomorphic receiver or dispatcher type

After MIR construction, the source of truth is simpler:

- every call is already a direct call to a specialized procedure
- every expression has exactly one monomorphic type
- no unresolved static dispatch operation remains

The intended architecture is a strict monomorphic MIR. It is close in spirit to
"clone CIR into a monomorphic form", but the exported MIR must be stricter than
current `monotype`: it must not carry `dispatch_call`, `type_dispatch_call`,
`method_eq`, source type facts, executable facts, or syntax-derived semantic
reconstructions.

## Core Rule

Static dispatch is resolved while constructing each MIR specialization.

It should not be resolved in a separate later pass, because MIR construction is
the first point where all required inputs are naturally present:

- the source expression shape
- the checked dispatch constraint
- the monomorphic receiver or type-dispatcher type
- the method registry
- the specialization table

Keeping dispatch unresolved after MIR construction would preserve a semantic
question longer than necessary. It would also require another pass to recover
context that the MIR builder already has in hand.

## Stage Contract

### Checked CIR

Checked CIR may contain source-level static dispatch operations.

It may contain operations such as:

- ordinary dispatch: `x.foo(a, b)`
- type dispatch: `Thing.default()`
- method equality dispatch: `x == y` when this is a nominal/custom `is_eq`

Checked CIR is responsible for preserving the explicit facts produced by
checking. It must not require later stages to recover missing information from
syntax.

Checked CIR may carry:

- method name
- receiver expression
- explicit arguments
- type-var alias information for type dispatch
- the dispatch constraint function variable
- source location and error-reporting metadata

Checked CIR should not carry a final direct target for generic dispatch calls,
because in the general case there is no single final target yet.

Example:

```roc
call_to_str : a -> Str where a.to_str : a -> Str
call_to_str = |x| x.to_str()
```

Different specializations of `call_to_str` can resolve to different attached
methods. The checked operation is valid, but the final target is not a global
property of the generic checked call site.

### Checker

The checker validates static dispatch constraints.

It may:

- build static dispatch constraints
- verify that a constrained receiver or type-var alias supports a method
- rewrite implicit structural equality to structural equality
- reject invalid dispatch
- record explicit checked facts needed by later stages

It must not:

- reconstruct missing method information from syntax in later stages
- persist heuristic target candidates
- emit fallback target choices
- encode "best effort" dispatch facts

If the checker can prove that a concrete dispatch call has a target, it may use
that proof for validation. But final target selection for executable code still
belongs to MIR construction, because generic specializations need the same rule.

### MIR Builder

The MIR builder consumes checked CIR and solved/specialized types.

For each requested specialization, it:

1. Allocates or reuses a specialized procedure id.
2. Builds a substitution from checked type variables to MIR monomorphic types.
3. Lowers the checked body into MIR.
4. Gives every MIR expression exactly one monomorphic type.
5. Resolves every static dispatch operation to a direct call.

The MIR builder is the only stage allowed to turn static dispatch into direct
calls.

### Resolved MIR

Resolved MIR must not represent unresolved static dispatch.

The exported MIR expression union should not contain:

```zig
dispatch_call
type_dispatch_call
method_eq
```

It should contain direct calls:

```zig
call_direct {
    proc: SpecializedProcId,
    args: []ExprId,
}
```

and other already-decided monomorphic operations such as literals, locals,
records, tags, structural equality, closures, and control flow.

### Later Stages

Later stages consume explicit MIR facts. They do not resolve methods.

IR and LIR should only see direct calls, erased calls if closure conversion has
introduced them, explicit layout bridges, and explicit reference-counting
operations emitted by earlier stages.

Backends must not think about reference counting. They should only lower the
explicit LIR `incref` and `decref` statements.

## MIR Data Model

The central invariant is:

```text
Every MIR expression id has exactly one MIR monomorphic type id.
```

That can be represented directly:

```zig
const Expr = struct {
    ty: MonoTypeId,
    data: ExprData,
};
```

or with a parallel dense array:

```zig
expr_data: ExprData.Store,
expr_types: MonoTypeId.Store,
```

Either representation is fine if construction makes it impossible to create an
expression without a type.

The builder API should enforce this:

```zig
fn addExpr(builder: *Builder, ty: MonoTypeId, data: ExprData) ExprId
```

There should be no MIR field for:

- source type
- refined source type
- executable source type
- inferred fallback type
- local constructor fact
- callable fact
- value fact

If a later stage needs layout information, bridges, or ownership actions, those
should be explicit later-stage facts derived from MIR types, not a competing
type system inside MIR.

## MIR Expression Shape

Resolved MIR should contain source-like expression structure only where the
operation is still semantically meaningful after specialization.

Reasonable MIR expression variants include:

```zig
const ExprData = union(enum) {
    local: LocalId,
    literal: Literal,
    let: Let,
    if_else: IfElse,
    when: When,
    tag: TagExpr,
    record: RecordExpr,
    tuple: TupleExpr,
    list: ListExpr,
    lambda: LambdaExpr,
    closure: ClosureExpr,
    call_direct: DirectCall,
    call_erased: ErasedCall,
    structural_eq: StructuralEq,
    bool_not: ExprId,
    // etc.
};
```

The exact list can follow existing CIR shape where useful. The important point
is not to preserve source syntax when the semantic choice has already been made.

`x.foo(y)` is not a MIR operation. It becomes `call_direct`.

`Thing.default()` is not a MIR operation. It becomes `call_direct`.

Nominal `x == y` is not a MIR method-equality operation. It becomes
`call_direct` to the specialized `is_eq`, optionally followed by `bool_not` for
inequality.

Anonymous structural equality remains `structural_eq`.

## Method Registry

The compiler should have an explicit checked method registry.

Conceptually:

```zig
const MethodKey = struct {
    owner_module_idx: ModuleIdx,
    owner_type_ident: Ident.Idx,
    method_ident: Ident.Idx,
};

const MethodDefRef = struct {
    module_idx: ModuleIdx,
    def_idx: DefIdx,
};

const MethodTarget = union(enum) {
    proc: MethodDefRef,
    intrinsic: IntrinsicMethod,
};
```

The registry maps:

```text
MethodKey -> MethodTarget
```

The registry should be produced from checked declarations and consumed by the
MIR builder.

It should not be a downstream executable fact. Once MIR has direct calls, later
stages do not need the registry.

If the current `AttachedMethodIndex` remains during migration, it should be
private to the MIR builder or the method-registry adapter. It should not be
threaded through all later stage results.

## Specialization Table

MIR construction should have an explicit specialization table.

Conceptually:

```zig
const SpecializationKey = struct {
    target: MethodDefRef,
    fn_ty: MonoFnTypeId,
    closure_mode: ClosureMode,
};

const SpecializedProc = struct {
    id: SpecializedProcId,
    key: SpecializationKey,
    status: Status,
};

const Status = enum {
    reserved,
    lowering,
    lowered,
};
```

The table must reserve a `SpecializedProcId` before lowering the procedure body.
That supports direct recursive and mutually recursive calls.

For a static dispatch target, MIR construction requests:

```text
specialize(target_method, exact_dispatch_constraint_fn_type)
```

and receives a direct `SpecializedProcId`.

## Owner Resolution

Dispatch owner resolution must be based on a monomorphic type, not an expression.

The API should be shaped like this:

```zig
fn ownerForDispatchType(types: *MonoTypeStore, ty: MonoTypeId) MethodOwner
```

It must not be shaped like this:

```zig
fn ownerForExpr(expr: ExprId) MethodOwner
```

Valid owner cases:

```text
nominal   -> the nominal owner module/type ident
primitive -> builtin primitive owner
list      -> builtin List owner
box       -> builtin Box owner
```

Invalid owner cases:

```text
anonymous record
anonymous tuple
anonymous tag union
function
unresolved variable
placeholder
erased or unknown value
```

Invalid owner cases are compiler invariant failures during MIR construction.
They are not fallback cases.

The type lookup must preserve nominal identity. Transparent aliases and opaque
nominals with attached methods resolve through their nominal identity, not
through their backing structural representation.

## Ordinary Dispatch Lowering

For source:

```roc
x.foo(a, b)
```

checked CIR has:

```text
receiver = x
method_name = foo
args = [a, b]
constraint_fn_var = receiver_ty, a_ty, b_ty -> ret_ty
```

MIR construction does:

1. Instantiate/lower `constraint_fn_var` to a monomorphic MIR function type.
2. Lower the receiver with expected type `constraint_arg[0]`.
3. Read the receiver expression's MIR type.
4. Resolve the dispatch owner from that type.
5. Look up `(owner, method_name)` in the checked method registry.
6. Request or reuse the target specialization at the exact constraint function type.
7. Lower the explicit arguments with expected types `constraint_arg[1..]`.
8. Emit `call_direct`.

The result expression type is the return type of the monomorphic constraint
function type.

In pseudocode:

```zig
fn lowerDispatchCall(node: CheckedDispatchCall) ExprId {
    const fn_ty = lowerCheckedFnType(node.constraint_fn_var);
    const recv_ty = fn_ty.args[0];
    const ret_ty = fn_ty.ret;

    const recv = lowerExprExpected(node.receiver, recv_ty);
    const owner = ownerForDispatchType(typeOf(recv));
    const target = method_registry.require(owner, node.method_name);
    const proc = specialize(target, fn_ty);

    const args = lowerArgsExpected(node.args, fn_ty.args[1..]);
    return addExpr(ret_ty, .{ .call_direct = .{
        .proc = proc,
        .args = concat(recv, args),
    }});
}
```

There is no type refinement step here. The receiver type comes from the checked
constraint after specialization. If that type is not concrete enough to resolve
an owner, MIR construction was requested too early or checking failed to produce
the required explicit facts.

## Type Dispatch Lowering

For source:

```roc
Thing.default()
```

where:

```roc
Thing : thing
```

checked CIR has:

```text
type_var_alias_stmt = Thing : thing
method_name = default
args = []
constraint_fn_var = () -> thing
```

MIR construction does:

1. Instantiate/lower the type-var alias target type to a monomorphic MIR type.
2. Resolve the owner from that monomorphic type.
3. Instantiate/lower the constraint function type.
4. Look up `(owner, method_name)` in the method registry.
5. Specialize the target at the exact constraint function type.
6. Lower explicit args with expected constraint arg types.
7. Emit `call_direct`.

No receiver is prepended for type dispatch.

## Method Equality Lowering

Nominal/custom equality should lower through the same method target mechanism.

For:

```roc
x == y
```

when checking determined that equality is a nominal/static `is_eq`, MIR
construction should resolve:

```text
ownerForDispatchType(typeOf(x))
method_name = is_eq
constraint = x_ty, y_ty -> Bool
```

and emit:

```text
call_direct(SpecializedOwnerIsEq, [x, y])
```

For inequality:

```roc
x != y
```

emit the same direct call followed by `bool_not`.

Anonymous structural equality must remain structural equality. It should never
enter method dispatch lowering.

## Why This Is Not Just Another Fact System

The current problematic shape is "facts beside expressions":

- source type facts
- executable type facts
- callable facts
- value facts
- local constructor facts
- refined expression source types

Those facts can compete with each other and with the checked type store. They
also invite syntax-based reconstruction when one fact is missing.

MIR should not duplicate that pattern.

MIR expressions are the executable facts. Their attached monomorphic types are
the type facts. Direct-call nodes are the dispatch facts.

There is no second channel that can disagree.

## Relation To CIR

MIR can be implemented as a monomorphic, lowered CIR-like tree. That is the
right mental model.

The meaningful differences from checked CIR are:

- MIR is monomorphic.
- MIR has no source type variables.
- MIR has no unresolved static dispatch.
- MIR has no source-only syntax after that syntax has been semantically decided.
- MIR procedure ids are specialization ids, not generic source defs.
- MIR expression ids always point to monomorphic type ids.

So "clone CIR into MIR with new type indices" is a good starting point, as long
as the MIR builder also performs the semantic eliminations that are only
possible at specialization time.

## Static Dispatch Edge Cases

### Generic Dispatch

Generic source:

```roc
f : a -> Str where a.to_str : a -> Str
f = |x| x.to_str()
```

Specializations:

```text
f(I64)      -> direct call to I64.to_str or builtin numeric to_str
f(MyThing)  -> direct call to MyThing.to_str
f(Box(U8))  -> direct call to Box.to_str if valid
```

Each MIR specialization resolves independently.

### Generic Target Methods

If the target method is itself generic, the target specialization key includes
the exact monomorphic dispatch constraint function type.

There is no separate target inference during later lowering.

### Chained Dispatch

Source:

```roc
x.foo().bar()
```

MIR construction lowers `x.foo()` first. The result expression has a
monomorphic type. Then `.bar()` resolves from that result type.

No code should recursively inspect the source expression to discover a method
owner. The lowered MIR expression already has the necessary type.

### Type-Var Alias Dispatch

Source:

```roc
Thing : thing
Thing.default()
```

The owner is the specialized monomorphic `thing`, not the text `Thing` and not a
syntax reconstruction.

### Method Values

If source syntax permits obtaining a method as a value, MIR must still resolve
the method target during construction.

The result should be an explicit function value, closure, or direct procedure
reference according to MIR's function-value representation. It must not be an
unresolved dispatch value.

### Recursive And Mutually Recursive Methods

The specialization table reserves a procedure id before lowering the body.

That allows:

```roc
Thing.f = |x| x.g()
Thing.g = |x| x.f()
```

to lower to direct calls between reserved specialized procedure ids.

### Transparent Aliases

Transparent aliases with attached methods must resolve through nominal identity.

For a transparent tag-union alias, the owner is the alias, not the backing tag
union layout.

This requires type lookup that preserves nominal identity during owner
resolution.

### Anonymous Structural Equality

Anonymous records, tuples, and tag unions do not become method dispatch just
because they support equality.

Checker should rewrite implicit structural equality to structural equality.
MIR preserves it as structural equality.

### Primitive, List, And Box Methods

Primitive, list, and box owners are builtin owners.

These should still go through explicit owner resolution and method registry
lookup. If a builtin method is a compiler intrinsic rather than a source-level
procedure, the registry should say so explicitly with `MethodTarget.intrinsic`.

### Effects, Hosted Calls, And Platform Methods

Effectful or platform-attached methods should not need a special dispatch
mechanism.

If they are attached methods, they resolve to direct targets during MIR
construction. If they are compiler intrinsics, that is represented explicitly in
the method registry.

Effect sequencing and hosted-call ABI concerns are later explicit lowering
concerns. They are not reasons to leave dispatch unresolved.

### Lambda Sets And Erasure

Static dispatch target selection should happen before final erased-call lowering.

MIR may still contain function values or closure/lambda-set information,
depending on exactly where lambda-set solving lives. But direct calls chosen by
static dispatch should already have known specialized procedure ids.

Closure conversion and erased-call lowering can happen after MIR, using MIR's
explicit direct-call targets and expression types.

### Numeric Defaulting

By MIR construction, numeric defaulting and flex resolution must be complete for
any type that participates in owner resolution.

If a dispatch owner type is still flexible, unresolved, or ambiguous, that is a
compiler invariant failure. MIR must not guess.

### Tags And Unions

A tag expression's MIR type comes from the checked expression's specialized
type.

MIR construction must not create an exact singleton tag-union type from tag
syntax. For example, it must not infer `[Err(Str)]` from seeing `Err("x")` when
the checked expression type is `[Ok(I64), Err(Str)]`.

Tag names and method names remain symbolic at this stage. They cannot be lowered
to layout indices until layout has been decided. The required rule is not "avoid
names"; it is "do not reconstruct types from names."

## Deletions

The design removes the need for the old executable fact architecture.

Immediate deletion targets:

- `exactTagSourceTypeForExpr`
- call sites that synthesize source tag-union types from tag syntax
- lints that only catch old names while allowing the same pattern under new names

MIR transition deletion targets:

- `refinedSourceTypeForExpr` and the broader refined-source-type family
- `ValueFact`
- `CallableFact`
- expression fact maps that compete with expression types
- environment entries that carry source/executable semantic facts
- `attachedMethodOwnerForExpr`
- `resolveAttachedMethodTargetFromExpr`
- dispatch specialization functions in `lambdamono`
- exported `dispatch_call`, `type_dispatch_call`, and `method_eq` variants after
  checked CIR
- downstream threading of `attached_method_index`

Some of these may need compatibility adapters during migration. Those adapters
should be temporary, narrow, and deleted as soon as the strict MIR boundary is in
place.

## Correct By Construction

The architecture should make old mistakes unrepresentable.

Required construction rules:

- Resolved MIR has no dispatch variants.
- MIR expression construction requires a monomorphic type.
- Static dispatch resolution is private to the MIR builder.
- Static dispatch resolution accepts a monomorphic type, not an expression.
- Method lookup consumes an explicit checked method registry.
- Later stages cannot import checked dispatch structures.
- IR and LIR cannot represent unresolved dispatch.
- Backends cannot observe source methods or reference-counting decisions.

The strongest API boundary is:

```zig
// Allowed only inside MIR builder:
fn lowerCheckedDispatchCall(...) ExprId

// Private helper:
fn resolveStaticDispatchTarget(owner_ty: MonoTypeId, method: Ident.Idx) MethodTarget

// Exported MIR:
call_direct: SpecializedProcId
```

There should be no public API that answers:

```text
what method does this expression call?
```

after MIR construction. The answer is already encoded as a direct procedure id.

## Verification

MIR verification should assert:

- every expression has a type
- every direct call target exists
- every direct call's argument types match the specialized procedure signature
- every direct call's result type matches the expression type
- no unresolved dispatch operation exists
- no method equality operation exists
- no source type variable appears in exported MIR
- no owner resolution was attempted for structural anonymous types
- all specialization table entries are either lowered or explicitly in progress
  during cycle handling

Even if the MIR type definitions make some checks redundant, debug verification
is still useful for transition work.

## CI And Audit Rules

Audits should be allowlist-based where possible.

Dispatch terms should be allowed only in:

- parser/canonicalize source syntax handling
- checker static dispatch validation
- checked CIR definitions
- MIR builder input lowering
- tests that intentionally mention source dispatch

They should be forbidden in:

- resolved MIR
- IR
- LIR
- backends
- post-MIR lowering
- semantic executable planning

Suspicious names and patterns to forbid outside narrow allowlists:

```text
exactTagSourceTypeForExpr
freshContent(.{ .tag_union
refinedSourceTypeForExpr
OwnerForExpr
ownerForExpr
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
resolve.*TargetFromExpr
dispatch_call
type_dispatch_call
method_eq
ValueFact
CallableFact
```

The important audit principle is to forbid the family of behavior, not just one
function name. The forbidden behavior is:

```text
later compiler stage reconstructs semantic type or dispatch information from
syntax instead of consuming explicit facts produced by earlier stages
```

## Test Coverage

The test suite should include structural MIR assertions and end-to-end behavior.

Required cases:

- generic dispatch specializes to different nominal methods
- generic target methods specialize at the exact dispatch constraint type
- chained dispatch resolves from the lowered result type
- type-var alias dispatch resolves from the specialized alias target
- custom equality lowers to direct `is_eq`
- inequality lowers to direct `is_eq` plus boolean negation
- anonymous record equality remains structural equality
- transparent tag-union aliases resolve through nominal identity
- cross-module attached methods resolve through the method registry
- recursive and mutually recursive methods lower to reserved specialized proc ids
- primitive methods resolve through builtin owners
- list and box methods resolve through builtin owners
- effect/platform methods use explicit method targets or explicit intrinsics
- tag construction never creates singleton source tag-union types
- no resolved MIR test fixture contains `dispatch_call`, `type_dispatch_call`, or
  `method_eq`

There should also be audit tests that intentionally try to add forbidden helper
names or forbidden dispatch variants in downstream modules.

## Migration Plan

1. Delete immediate syntax-derived type reconstruction.
2. Strengthen semantic audits so the deleted pattern cannot return under a new
   name.
3. Introduce the explicit checked method registry if the existing attached method
   index is not precise enough.
4. Introduce a strict MIR module or harden current `monotype` into strict MIR.
5. Add MIR builder APIs that require monomorphic types on expression creation.
6. Move static dispatch resolution into MIR construction.
7. Make exported MIR unable to represent dispatch.
8. Delete downstream dispatch handling from `lambdasolved` and `lambdamono`.
9. Delete executable fact carriers made obsolete by typed MIR.
10. Add structural MIR verification and CI audits.

The migration can be incremental internally, but the destination boundary should
not be compromised: resolved MIR contains direct calls, not dispatch facts.

## Final Invariant

After MIR construction, there are no semantic questions left about which
function a static dispatch call invokes.

Later stages may still decide layouts, insert bridges, lower closures, solve ABI
details, and emit explicit reference-counting operations. They may not choose or
reinterpret method targets.

