# For-Clause Implementation: Fixing the Box Type Mismatch Bug

## CRITICAL EXPECTATIONS

**This implementation MUST be complete with ALL tests passing.** There are no excuses, no "WIP", no partial solutions. The requirements are:

1. **ALL core tests must pass** - `zig build minici` must complete successfully
2. **ALL integration tests must pass** - `test/int/app.roc` must pass all 4 tests (init, render, update, render-after-update)
3. **High quality code only** - No workarounds, hacks, or shortcuts
4. **No unresolved rigids at runtime** - After for-clause mapping, `Box(Model)` must correctly resolve to `Box({ value: I64 })`. There should be NO `Box(model)` with unresolved rigid type variables at runtime.

This is not optional. This is not "nice to have". This is the bare minimum expectation for this implementation to be considered complete.

---

## Problem Summary

When a Roc platform declares type variables that an app must provide concrete types for, the interpreter loses type information across separate entrypoint invocations. This causes `Box(model)` to be incorrectly interpreted as `Box(Dec)` instead of `Box({ value: I64 })`.

### Root Cause

The current syntax `requires { model } { init : {} -> model, ... }` introduces `model` as a rigid type variable that exists only within the scope of the requires signatures. When the platform defines functions like:

```roc
render_for_host : Box(model) -> I64
```

The `model` variable is an unresolved type variable at the platform's top-level scope. During interpretation, when the type system encounters this unresolved flex/rigid variable, it defaults to `Dec` (decimal) - a scalar type. This causes field access on what should be a record to fail.

### The Fix: For-Clauses

We will introduce a new `for` clause syntax that:
1. Removes the separate `{ model }` rigids clause
2. Associates type aliases directly with entrypoint functions
3. Ensures type aliases resolve to concrete types after unification with the app

## New Syntax

### Before (Current)
```roc
platform ""
    requires { model } { init : {} -> model, update : model, I64 -> model, render : model -> I64 }
    ...

init_for_host : {} -> Box(model)
render_for_host : Box(model) -> I64
```

### After (New)
```roc
platform ""
    requires {
        [Model : model] for main : () -> {
            init : {} -> model,
            update : model, I64 -> model,
            render : model -> I64
        }
    }
    ...

init_for_host : {} -> Box(Model)
render_for_host : Box(Model) -> I64
```

### App Side Changes

The app must now provide a `main` function that returns a record:

```roc
app [main] { pf: platform "./platform/main.roc" }

Model : { value: I64 }

main = || {
    init: |{}| { value: 0 },
    update: |m, delta| { value: m.value + delta },
    render: |m| m.value,
}
```

### Key Semantics

1. **`[Model : model]`** - Creates a type alias `Model` in the platform's top-level scope, bound to the rigid `model`
2. **`for main`** - Associates this requirement with an entrypoint named `main`
3. **`: () -> { ... }`** - The type signature of `main` (0-arg function returning a record)
4. After unification with the app's `main`, `Model` becomes a concrete type (e.g., `{ value: I64 }`)

## Implementation Phases

### Phase 1: Parser Changes

**Files to modify:**
- `src/parse/tokenize.zig` - Already has `KwFor`, no changes needed
- `src/parse/Parser.zig` - Modify `parsePlatformHeader`
- `src/parse/AST.zig` - Add new AST node types
- `src/parse/NodeStore.zig` - Add storage for new nodes

**New AST Types:**

```zig
// In AST.zig
pub const ForClauseTypeAlias = struct {
    /// The alias name (e.g., "Model")
    alias_name: Token.Idx,
    /// The rigid variable name (e.g., "model")
    rigid_name: Token.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

pub const RequiresEntry = struct {
    /// Type aliases: [Model : model, Foo : foo]
    type_aliases: ForClauseTypeAlias.Span,
    /// The entrypoint name (e.g., "main")
    entrypoint_name: Token.Idx,
    /// The type annotation for this entrypoint
    type_anno: TypeAnno.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};
```

**Parser changes:**

In `parsePlatformHeader`, replace the current two-record parsing:
```zig
// OLD: requires { model } { init : ... }
// NEW: requires { [Model : model] for main : () -> { init : ... } }
```

The new parsing will:
1. Expect `{` after `requires`
2. Parse one or more `RequiresEntry` items separated by commas
3. Each entry: `[` aliases `]` `for` name `:` type_anno

### Phase 2: AST Structure Updates

**Files to modify:**
- `src/parse/AST.zig` - Update `Header.platform` struct

```zig
// OLD
platform: struct {
    name: Token.Idx,
    requires_rigids: Collection.Idx,      // REMOVE
    requires_signatures: TypeAnno.Idx,    // REMOVE
    exposes: Collection.Idx,
    packages: Collection.Idx,
    provides: Collection.Idx,
    targets: ?TargetsSection.Idx,
    region: TokenizedRegion,
},

// NEW
platform: struct {
    name: Token.Idx,
    requires_entries: RequiresEntry.Span, // NEW
    exposes: Collection.Idx,
    packages: Collection.Idx,
    provides: Collection.Idx,
    targets: ?TargetsSection.Idx,
    region: TokenizedRegion,
},
```

### Phase 3: Canonicalization Updates

**Files to modify:**
- `src/canonicalize/Can.zig` - Update `processRequiresSignatures`
- `src/canonicalize/ModuleEnv.zig` - Update `RequiredType` struct
- `src/canonicalize/CIR.zig` - Add structures for for-clause aliases

**Key changes:**

1. **Process for-clause type aliases as top-level type aliases**
   - For each `[Model : model]`, create a type alias `Model` in the platform's scope
   - The alias initially refers to a rigid type variable

2. **Store mapping from alias to rigid**
   - This allows the type checker to unify through the alias

3. **Update RequiredType to track entrypoints**
   ```zig
   pub const RequiredType = struct {
       /// The entrypoint name (e.g., "main")
       entrypoint_name: Ident.Idx,
       /// Type aliases introduced by this entrypoint
       type_aliases: TypeAliasMapping.SafeList.Range,
       /// The type annotation for this entrypoint
       type_anno: CIR.TypeAnno.Idx,
       region: Region,
   };

   pub const TypeAliasMapping = struct {
       /// The alias name (e.g., "Model")
       alias_ident: Ident.Idx,
       /// The rigid name (e.g., "model")
       rigid_ident: Ident.Idx,
       /// The type variable created for this alias
       type_var: types.Var,
   };
   ```

### Phase 4: Type Checking Updates

**Files to modify:**
- `src/check/Check.zig` - Update `checkPlatformRequirements`

**Key changes:**

1. **Unify entrypoint types**
   - Match platform's `main : () -> { init: ..., ... }` with app's `main` export
   - This unification propagates concrete types back through the rigid variables

2. **Propagate types to aliases**
   - After unification, the rigid `model` becomes concrete (e.g., `{ value: I64 }`)
   - Update the `Model` alias to point to this concrete type
   - This makes `Box(Model)` resolve to `Box({ value: I64 })`

3. **No more flex fallback needed**
   - Since `Model` is a concrete type alias (not a flex var), `getRuntimeLayout` won't default to `Dec`

### Phase 5: Test Case Updates

**Files to modify:**
- `test/int/app.roc`
- `test/int/platform/main.roc`

**app.roc (new):**
```roc
app [main] { pf: platform "./platform/main.roc" }

Model : { value: I64 }

main = || {
    init: |{}| { value: 0 },
    update: |m, delta| { value: m.value + delta },
    render: |m| m.value,
}
```

**platform/main.roc (new):**
```roc
platform ""
    requires {
        [Model : model] for main : () -> {
            init : {} -> model,
            update : model, I64 -> model,
            render : model -> I64
        }
    }
    exposes []
    packages {}
    provides { init_for_host: "init", update_for_host: "update", render_for_host: "render" }
    targets: { ... }

init_for_host : {} -> Box(Model)
init_for_host = |{}| {
    record = main({}).init({})
    Box.box(record)
}

update_for_host : Box(Model), I64 -> Box(Model)
update_for_host = |boxed_model, value| {
    m = Box.unbox(boxed_model)
    Box.box(main({}).update(m, value))
}

render_for_host : Box(Model) -> I64
render_for_host = |boxed_model| {
    m = Box.unbox(boxed_model)
    main({}).render(m)
}
```

## Detailed Code Locations

### Parser (`src/parse/Parser.zig`)

Lines 363-633: `parsePlatformHeader` - Complete rewrite of requires parsing section (lines 395-479)

Current code to replace:
```zig
// Lines 402-441: Parse requires rigids { model }
// Lines 443-479: Parse requires signatures { init : ... }
```

New parsing flow:
1. After `KwRequires`, expect `{`
2. Loop parsing `RequiresEntry` items:
   - `[` type_alias_list `]` `for` lower_ident `:` type_anno
3. Close with `}`

### AST (`src/parse/AST.zig`)

Line 1625-1634: `Header.platform` struct - needs new `requires_entries` field

Lines ~2000+: Add new `ForClauseTypeAlias` and `RequiresEntry` types

### NodeStore (`src/parse/NodeStore.zig`)

Add storage arrays and methods for:
- `ForClauseTypeAlias`
- `RequiresEntry`
- Scratch space for parsing

### Canonicalizer (`src/canonicalize/Can.zig`)

Lines 1776-1787: Platform header processing - update to call new method
Lines 2738-2803: `processRequiresSignatures` - complete rewrite

New approach:
1. For each `RequiresEntry`:
   - Process type aliases into platform's type scope
   - Create type alias definitions accessible at top level
   - Store mapping for type checking phase

### ModuleEnv (`src/canonicalize/ModuleEnv.zig`)

Lines 462-471: `RequiredType` struct - update to new structure with entrypoints and aliases

### Type Checker (`src/check/Check.zig`)

Lines 1090-1147: `checkPlatformRequirements` - update to:
1. Unify entrypoint functions
2. Propagate concrete types back to aliases
3. Ensure aliases become concrete (not flex)

## Testing Strategy

After each phase:
1. Run `zig build` to verify compilation
2. Run existing tests to check for regressions
3. At the end, run `zig build minici` for full test suite

## Commit Strategy

- **Commit 1**: Phase 1 - Parser changes (new AST types, parsing logic)
- **Commit 2**: Phase 2 - AST structure updates
- **Commit 3**: Phase 3 - Canonicalization updates
- **Commit 4**: Phase 4 - Type checking updates
- **Commit 5**: Phase 5 - Test case updates and final verification

Each commit should leave the codebase in a compilable state (though tests may fail until the final phase).

## Success Criteria

**ALL of these must be true. No exceptions. No partial success.**

1. `zig build` succeeds after each phase
2. The test case in `test/int/` passes all 4 tests:
   - Test 1: init returns Box(Model)
   - Test 2: render(Box(Model)) returns 0
   - Test 3: update(Box(Model), 42) returns new Box(Model)
   - Test 4: render(updated Box(Model)) returns 42
3. `zig build minici` passes completely - all 1910+ tests pass
4. No regressions in other tests
5. Works in BOTH interpreter mode AND compiled mode

If any of these criteria are not met, the implementation is NOT DONE.

## Why This Fix Works

The fundamental insight is that the current design tries to use rigid type variables (`model`) at the platform's top level, but these rigids aren't unified with concrete types until type checking happens - and by then, the interpreter has already defaulted them to `Dec`.

The for-clause design introduces **type aliases** (`Model`) that:
1. Are defined at the platform's top level (not inside a function scope)
2. Start as aliases to rigid variables
3. Get unified with concrete types during type checking
4. Remain as concrete type aliases afterward

This means when the interpreter encounters `Box(Model)`, it looks up `Model`, finds a concrete type like `{ value: I64 }`, and correctly computes the layout. No more defaulting to `Dec`!
