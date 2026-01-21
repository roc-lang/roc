# LSP Record and Static Dispatch Completion Issues - Analysis and Plan

## Problem Summary

Two completion issues have been identified:

1. **Record field completion only works when the field name is completely entered**
   - `myrec.field1|` shows "field1" as a completion
   - `myrec.fie|` shows nothing

2. **Static dispatch completion isn't working at all**
   - `val.to_str|` (where val is a nominal type with to_str method) shows no methods

## Root Cause Analysis (Updated After Testing)

### Issue 1: Region Boundary Check - Already Fixed

The user implemented the inclusive region check fix. This is no longer the issue.

### Issue 2: Type Variables Have Error Content Due to Incomplete Code

**This is the CORE issue.**

**Location**: `lsp/syntax.zig` completion code + `check/unify.zig:330`

**Evidence from logs**:
```
module_env_opt=true, build_succeeded=true
addFieldsFromTypeVar: type_var=@enumFromInt(62), content tag=err
```

**Problem Flow**:
1. User types incomplete code: `x.to_str|` (cursor mid-method-name)
2. Parser creates AST/CIR for incomplete expression
3. Type checker runs on incomplete code
4. Type unification fails because the expression is incomplete
5. Unification failure sets type content to `.err` (`check/unify.zig:330`)
6. Build "succeeds" (no exception thrown) but type variables have `.err` content
7. Completion code looks up type variable → gets `.err` → no completions returned

**Key Insight**: The build can "succeed" (not throw an exception) while still producing `.err` type content. Type errors are stored as "problems" but don't fail the build outright.

### Issue 3: Fresh Build Types Used Instead of Snapshot Types

**Location**: `lsp/syntax.zig:2978-3014` (module_env_opt selection logic)

The current code uses the fresh build's module_env when `build_succeeded=true`:
```zig
const module_env_opt: ?*ModuleEnv = blk: {
    if (!build_succeeded or build_has_reports) {
        // Use snapshot (only when build failed or has reports)
    }
    // Otherwise use current build's env
    if (self.getModuleEnvByPath(absolute_path)) |module_env| {
        break :blk module_env;
    }
    // ...
};
```

**Problem**: When typing incomplete code:
- Build "succeeds" (no exception)
- `build_has_reports` might be false for some type errors
- Code uses fresh build's types which have `.err` content
- Snapshot with good types is ignored

## Proposed Fixes (Revised)

### Fix 1: Always Prefer Snapshot Environment for Type Lookups

**Rationale**: When typing, code is almost always incomplete. The snapshot from the last clean build has correct type information. We should prefer it for completion lookups.

**Implementation** (`lsp/syntax.zig` ~line 2978):
```zig
const module_env_opt: ?*ModuleEnv = blk: {
    // ALWAYS try snapshot first for completion - typing usually produces incomplete code
    if (self.snapshot_envs.get(absolute_path)) |snapshot_env| {
        const snapshot_module_env = self.getModuleEnvByPathInEnv(snapshot_env, absolute_path);
        if (snapshot_module_env) |module_env| {
            break :blk module_env;
        }
    }

    // Fall back to current build only if no snapshot available
    if (build_succeeded and !build_has_reports) {
        if (self.getModuleEnvByPath(absolute_path)) |module_env| {
            break :blk module_env;
        }
    }
    // ...
};
```

### Fix 2: Use Snapshot CIR Alongside Snapshot Types

**Problem**: Even if we use snapshot types, we're currently using the CURRENT CIR to find expressions via `findDotReceiverTypeVar`. The current CIR has different indices than the snapshot's types.

**Solution**: When using snapshot types, also use the snapshot's CIR for expression lookups.

**Implementation**: Modify `findDotReceiverTypeVar` to take a `store` parameter, and use `snapshot_env`'s store when using snapshot types:
```zig
if (self.findDotReceiverTypeVar(snapshot_module_env, snapshot_module_env.store, cursor_offset)) |type_var| {
    // type_var indices now match the types
}
```

### Fix 3: Handle Flex/Rigid Types with Static Dispatch Constraints

For generic type parameters like `a where [ a.to_str : a -> b ]`, the type content is `.flex` or `.rigid` with constraints, not a concrete type.

**Location**: `addMethodsFromTypeVar` should check for flex/rigid content and extract method names from `constraints`:

```zig
switch (content) {
    .flex => |flex| {
        // Extract method names from flex.constraints
        try self.addMethodsFromConstraints(&items, module_env, flex.constraints);
    },
    .rigid => |rigid| {
        // Extract method names from rigid.constraints
        try self.addMethodsFromConstraints(&items, module_env, rigid.constraints);
    },
    // ... existing alias/structure handling
}
```

## Test Cases Added

Two new failing tests in `lsp/test/syntax_test.zig`:
1. `record field completion with partial field name` - Tests `my_record.fo|`
2. `static dispatch completion for nominal type methods` - Tests `val.to|`

## Implementation Order

1. **Fix 1**: Prefer snapshot environment for type lookups (highest impact)
2. **Fix 2**: Use snapshot CIR when using snapshot types (ensures index consistency)
3. **Fix 3**: Handle flex/rigid types with constraints (needed for generic static dispatch)

## Files to Modify

1. `lsp/syntax.zig`:
   - `getCompletionsAtPosition`: Prefer snapshot for module_env selection
   - `findDotReceiverTypeVar`: Accept store parameter for CIR lookups
   - `addMethodsFromTypeVar`: Handle flex/rigid with constraints

## Compilation Fix Applied

Fixed compilation error in `findDotReceiverTypeVarInExpr` at line 3900:
- Changed `dispatch.value` to iterate over `dispatch.args`
