# Lazy Specialization-Time Method Resolution

## Summary

Method resolution should happen lazily during specialization, not during checking.

The compiler should:

- preserve the syntactic distinction between field access and method call from parsing onward
- type-check method calls during checking
- record the exact solved vars needed for later specialization on each method-call node
- emit per-module attached-method definition metadata from checking
- build one global attached-method index after checked modules are loaded
- resolve each method call to an exact target only when specialization has a concrete receiver instantiation
- rewrite method calls to direct calls during specialization

No runtime dictionary passing.
No post-check alias/source-var recovery.
No fallback or heuristic method lookup.

## Critical Syntax Fact

Roc already has a syntactic distinction:

- `a.foo` is always record access
- `a.foo()` is always method call

This is already known at parsing time.

That means:

- there is no later-stage disambiguation problem to solve
- no compiler stage should ever try to infer whether a dot expression is field access or method dispatch
- field access and method call must remain separate IR node kinds all the way through post-check lowering

## High-Level Pipeline

The intended flow is:

1. Parsing / AST / CIR keep field access and method call distinct.
2. Checking type-checks method calls and records solved vars on those method-call nodes.
3. Each checked module outputs explicit attached-method definition metadata for the methods it defines.
4. Once all checked modules are loaded into the global compilation context, the compiler builds a global attached-method index.
5. Specialization is the only place that resolves method calls to exact defs.
6. After specialization, method calls are rewritten to direct calls and later stages never do method lookup.

## What Checking Does

Checking should happen exactly where it already does today:

- method-call syntax first encountered in [`src/check/Check.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/Check.zig)
- method constraints solved there as part of the checker's normal type-inference logic

Checking is responsible for:

- proving that a method call is well-typed
- solving the call's receiver/arg/result types
- storing exact solved vars on the method-call node
- emitting metadata for attached methods defined by the current module

Checking is **not** responsible for:

- choosing the final exact target def for a polymorphic method call
- reifying polymorphic method calls into direct calls

### Method-call node facts that must survive checking

Each method-call node must explicitly preserve at least:

- `method_ident`
- `receiver_var`
- `arg_vars`
- `ret_var`
- optionally the full call `fn_var` if downstream code benefits from it

These are explicit checked-output facts, not side effects hidden in internal checker tables.

## Per-Module Attached-Method Metadata

Each checked module should emit explicit metadata for the concrete attached methods it defines.

Example shape:

```zig
const AttachedMethodDef = struct {
    receiver_type_def_idx: CIR.Def.Idx,
    method_ident: base.Ident.Idx,
    method_def_idx: CIR.Def.Idx,
};
```

This is still per-module and cache-friendly.

It does not resolve any polymorphic call sites.
It only says:

- this module defines method `map`
- on receiver type `List`
- at def `List.map`

## Global Attached-Method Index

After checked modules are loaded into the global compilation context, the compiler should build one global attached-method index.

Example shape:

```zig
const NominalTypeKey = struct {
    module_idx: u32,
    type_def_idx: CIR.Def.Idx,
};

const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const AttachedMethodKey = struct {
    receiver: NominalTypeKey,
    method_ident: base.Ident.Idx,
};
```

Then the global index is:

```zig
const AttachedMethodIndex = HashMap(AttachedMethodKey, MethodTarget);
```

This is only a catalog of concrete methods that exist globally.

Examples:

- `(Builtin.List, map) -> Builtin.List.map`
- `(Builtin.Dict, map) -> Builtin.Dict.map`
- `(Builtin.Set, map) -> Builtin.Set.map`

This is not call-site resolution.

## Exactly Where Method Resolution Code Runs

Method resolution code should run in exactly one place:

- specialization

It should not run in:

- parsing
- checking
- `typed_cir`
- monotype lowering
- IR lowering
- LIR lowering
- any backend

The intended home is the specialization step that already has a concrete instantiation map for the specialized function body.

Practically, that means the resolution code should live in lambdamono specialization or in a helper called only from there.

## Specialization-Time Resolution Algorithm

When specialization encounters a method-call node:

1. Read the node's explicit `receiver_var`.
2. Apply the current specialization instantiation map to that var.
3. Read the instantiated type.
4. Extract the instantiated receiver's exact head nominal identity.
5. Form an `AttachedMethodKey` from:
   - that exact nominal identity
   - the node's `method_ident`
6. Look up the exact `MethodTarget` in the global attached-method index.
7. Rewrite the method-call node into a direct call to that target.

After this rewrite, later stages should see only:

- direct calls
- indirect calls
- ordinary field access

No later stage should ever see unresolved method dispatch.

## Polymorphic Receiver Example

Source:

```roc
apply = |a, fn| a.map(fn)
```

During checking:

- this remains a method-call node
- the checker records exact solved vars on that node
- no exact target def is chosen yet

During specialization:

### Specialization 1

If `a` instantiates to `List U64`:

- instantiated receiver nominal = `Builtin.List`
- key = `(Builtin.List, map)`
- target = `Builtin.List.map`
- rewrite method call to direct call `Builtin.List.map(a, fn)`

### Specialization 2

If `a` instantiates to `Dict Str U64`:

- instantiated receiver nominal = `Builtin.Dict`
- key = `(Builtin.Dict, map)`
- target = `Builtin.Dict.map`
- rewrite method call to direct call `Builtin.Dict.map(a, fn)`

Same source function.
Different specializations.
Still fully static and monomorphized.

## Cross-Module Example

Module `A`:

```roc
type MyBox a

map : MyBox a, (a -> b) -> MyBox b
```

Module `B`:

```roc
apply = |a, fn| a.map(fn)
```

Checking:

- module `A` emits metadata for `MyBox.map`
- module `B` emits a method-call node in `apply`

Global compilation setup:

- load checked modules
- build attached-method index entry:
  - `(A.MyBox, map) -> A.MyBox.map`

Specialization of `B.apply` for `A.MyBox`:

- instantiate receiver var to `A.MyBox`
- resolve key `(A.MyBox, map)`
- rewrite to direct call `A.MyBox.map`

No runtime indirection.
No need for check-time direct resolution.

## Important Representation Requirement

This design only works if nominal identity survives until specialization-time method resolution.

That means the instantiated type representation available during specialization must still preserve:

- exact defining module
- exact receiver type def

It must not erase nominal identity too early to backing types.

Otherwise different receiver types that lower to the same backing representation would become indistinguishable, and specialization-time method resolution would be impossible or wrong.

So nominal wrappers must survive at least until method-call rewriting is complete.

## Invariants

1. `a.foo` and `a.foo()` remain distinct from parsing onward.
2. Field access never participates in method resolution.
3. Checking records explicit solved vars on method-call nodes.
4. Checking emits concrete attached-method definition metadata per module.
5. A global attached-method index is built only after checked modules are loaded globally.
6. Exact method target selection runs only during specialization.
7. After specialization, unresolved method-call nodes no longer exist.
8. No later stage performs method lookup from receiver vars, alias statements, names, or text.

## What This Design Forbids

This design forbids:

- resolving polymorphic method calls to direct calls during checking
- runtime dictionary passing
- alias-statement-driven dispatch recovery
- source-var-driven dispatch recovery
- text-based method lookup in later stages
- fallback or heuristic method lookup anywhere after parsing/error reporting

## What This Should Delete

This design implies deleting any later-stage method-resolution path that tries to recover a target from checked type variables or alias statements instead of resolving directly from the specialization instantiation plus the global attached-method index.

In particular, post-check stages should not have APIs of the form:

- `(receiver_var, method_name) -> maybe target`
- anything that depends on source-var alias statements as a dispatch side channel

## Final Outcome

The end state is:

- method syntax is preserved explicitly until specialization
- method calls are resolved lazily and statically during specialization
- specialization rewrites them into direct calls
- all later stages are free of method lookup logic

That matches the desired architecture:

- no runtime extra arguments
- no fallback
- no heuristic recovery
- no later-stage guessing
- clean support for cross-module polymorphism and monomorphization
