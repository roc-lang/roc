DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

# Strongest-Form MIR Plan

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Execution Constraint

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

This plan is specifically for making the structural changes in the dark, without
compiler feedback. Every step below assumes:

- DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!
- do not run tests
- do not “just check what errors remain”
- make the design changes directly and cleanly
- do not leave anything in a temporary workaround, placeholder, or transitional state
- do not leave obsolete structures, compat layers, or dead artifacts behind
- if something is intentionally deferred, leave a loud debug panic saying it is TODO / not implemented yet

## Goal

Make strongest-form MIR as close to LIR as possible while still preserving the
MIR-specific concepts that LIR intentionally no longer has:

- lambdas
- closures
- monotypes
- specialization / lambda-set inference inputs

This means strongest-form MIR should be:

- monomorphic
- statement-only
- local-id-based
- explicit control flow
- pattern-free
- free of nested value expressions
- free of side-table callable-identity reconstruction

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Core Principles

1. `Symbol` is only for global/top-level identity.
2. Executable flow uses `LocalId` only.
3. All nontrivial CIR expressions are normalized into explicit MIR local-defining
   statements during CIR -> MIR lowering.
4. `if` does not exist in strongest-form MIR. It is lowered to explicit branch
   control flow before or during CIR -> MIR lowering.
5. Destructuring does not exist in strongest-form MIR. It is lowered into
   explicit branch/lambda-entry statements.
6. MIR remains lambda-aware and monotype-aware.
7. LIR remains the phase where lambdas disappear.

While applying these principles:

- DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!
- DO NOT LEAVE ANY TEMPORARY WORKAROUNDS, PLACEHOLDERS, OR TRANSITIONAL ARTIFACTS BEHIND!

## Target Strongest-Form MIR

### Local Model

Use:

- `LocalId`
- `Local { monotype, reassignable }`
- `LocalSpan`

No executable MIR statement should carry source/global naming metadata on
ordinary locals.

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

Strongest-form MIR should also be:

- fully cleaned up
- free of obsolete parallel representations
- free of compatibility shims
- free of “we will clean this up later” scaffolding

If a feature is not implemented in the new design yet, it must fail loudly with
an explicit TODO panic instead of silently falling back or keeping the old model
alive.

### Top-Level Model

Keep explicit top-level defs keyed by `Symbol`:

- `ConstDef { symbol, body, monotype, source_region }`
- `FunctionDef { symbol, lambda }`

### Lambda Model

Keep explicit MIR lambdas:

- `Lambda { fn_monotype, params, body, ret_monotype, debug_name, source_region, captures_param, recursion, hosted }`

MIR must still support:

- lambda-set inference
- specialization / monomorphization
- closure capture reasoning
- callable identity reasoning

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

Delete obsolete code completely. Do not leave:

- dead data structures
- unused legacy stmt variants
- old side tables kept “just in case”
- compatibility adapters
- partial migrations that preserve both old and new representations

### Statement Language

Target executable MIR statement surface:

```zig
pub const RefOp = union(enum) {
    local: LocalId,
    field: struct { source: LocalId, field_idx: u32 },
    tag_payload: struct { source: LocalId, payload_idx: u32 },
    nominal: struct { backing: LocalId },
    discriminant: struct { source: LocalId },
};

pub const CFStmt = union(enum) {
    assign_symbol: struct { target: LocalId, symbol: Symbol, next: CFStmtId },
    assign_ref: struct { target: LocalId, op: RefOp, next: CFStmtId },
    assign_literal: struct { target: LocalId, literal: LiteralValue, next: CFStmtId },
    assign_lambda: struct { target: LocalId, lambda: LambdaId, next: CFStmtId },
    assign_closure: struct { target: LocalId, lambda: LambdaId, captures: LocalSpan, next: CFStmtId },
    assign_call: struct { target: LocalId, callee: LocalId, args: LocalSpan, next: CFStmtId },
    assign_low_level: struct { target: LocalId, op: CIR.Expr.LowLevel, args: LocalSpan, next: CFStmtId },
    assign_list: struct { target: LocalId, elems: LocalSpan, next: CFStmtId },
    assign_struct: struct { target: LocalId, fields: LocalSpan, next: CFStmtId },
    assign_tag: struct { target: LocalId, name: Monotype.Name, args: LocalSpan, next: CFStmtId },
    debug: struct { value: LocalId, next: CFStmtId },
    expect: struct { condition: LocalId, next: CFStmtId },
    runtime_error: RuntimeError,
    switch_stmt: struct { scrutinee: LocalId, branches: SwitchBranchSpan, default_branch: CFStmtId },
    borrow_scope: struct { id: BorrowScopeId, body: CFStmtId, remainder: CFStmtId },
    scope_exit: struct { id: BorrowScopeId },
    join: struct { id: JoinPointId, params: LocalSpan, body: CFStmtId, remainder: CFStmtId },
    jump: struct { id: JoinPointId, args: LocalSpan },
    ret: struct { value: LocalId },
    crash: StringLiteral.Idx,
};
```

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## What Should Be Deleted

Delete these from strongest-form MIR:

- `Pattern`
- `PatternId`
- `PatternSpan`
- `MatchBranch` pattern trees
- `BranchPattern`
- `RestIndex`
- `BorrowBinding`
- `assign_alias`
- `assign_field`
- `assign_tag_payload`
- `assign_nominal`
- `assign_closure_call`
- `assign_str_escape_and_quote`
- `SeedMember`
- `SeedMemberSpan`
- `symbol_seed_members`
- `local_seed_members`

Potentially also delete:

- `ClosureMember`
- `CaptureBinding`

if callable identity is redesigned cleanly enough that these no longer need to
exist as semantic side metadata.

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Why Patterns Should Go

Recursive patterns in strongest-form MIR keep tree-structured reasoning alive in
exactly the phase where we want explicit local dataflow instead.

Instead:

- lambda parameter destructuring should become explicit lambda-entry statements
- `match` destructuring should become explicit branch-entry statements
- `as` patterns should become explicit local aliases
- list/record destructures should be lowered before strongest-form MIR

The strongest-form branch/test model should be minimal:

- wildcard/default
- scalar literal test
- tag/discriminant test

Everything else becomes explicit statements inside branch bodies.

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Why `assign_ref` Is The Best Next Cleanup

These current MIR statement kinds are all really the same idea:

- `assign_alias`
- `assign_field`
- `assign_tag_payload`
- `assign_nominal`

They should collapse into:

```zig
assign_ref { target, op: RefOp, next }
```

This directly reduces branching in:

- `ResultSummary`
- `MirToLir`
- callable/value provenance logic

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Callable Identity Design

The current callable-identity setup is too fragmented. Today it is spread
across:

- explicit top-level defs
- local def reconstruction
- `symbol_seed_members`
- `local_seed_members`
- ad hoc call-result reasoning

The target design is:

1. top-level callable identity comes from `FunctionDef` / `ConstDef`
2. intra-body callable identity comes from the explicit local def graph
3. callable-valued call results come from a dedicated callable-summary analysis

This means no seed-member side tables in strongest-form MIR.

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Compatibility With Lambda-Set Inference And Specialization

This plan does **not** remove lambdas from MIR.

MIR will still retain:

- `Lambda`
- closure creation
- function-typed locals
- monotypes
- top-level function defs

So MIR remains the correct layer for:

- lambda-set inference
- specialization / monomorphization
- closure capture analysis
- callable identity reasoning

The cleanup is about representation style, not about deleting the semantic
concepts those analyses need.

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

## Migration Order

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!

1. Introduce MIR `RefOp` and `assign_ref`.
2. Rewrite MIR analyses and MIR -> LIR lowering to use `assign_ref`.
3. Replace pattern-driven `match_stmt` lowering with value-test branches plus
   explicit branch-entry statements.
4. Lower all parameter destructuring into lambda-entry statements.
5. Delete MIR patterns entirely.
6. Collapse `assign_call` and `assign_closure_call` into one call form with
   `callee: LocalId`.
7. Replace seed-member side tables with:
   - local def graph resolution
   - a dedicated callable-summary analysis for callable-valued call results
8. Simplify `MirToLir` again once MIR has the reduced surface.

At every migration step:

- DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!
- do not add temporary compatibility shims just to make something compile
- do not add workarounds
- make the intended structural change directly
- delete obsolete artifacts completely once the new structure exists
- if the new structure does not implement some case yet, replace the old path with a loud TODO panic rather than preserving the old path

## Expected Result

At the end of this plan:

- MIR and LIR are structurally parallel
- MIR is still lambda-aware and monotype-aware
- LIR stays proc/layout/ownership oriented
- MIR -> LIR becomes mostly structural translation instead of recursive
  pattern/callable reconstruction
- lambda-set inference and specialization continue to work at the MIR layer
- there are no obsolete old-world MIR/LIR artifacts left anywhere in the active pipeline
- any intentionally deferred feature fails loudly with an explicit TODO panic

Final reminder:

DO NOT USE THE COMPILER! DO NOT RUN ZIG FOR ANY REASON!
