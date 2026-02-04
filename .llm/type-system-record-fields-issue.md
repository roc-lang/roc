# Type System Record Fields Issue

**Date**: 2026-01-21  
**Status**: UNRESOLVED - Blocked by type system limitation  
**Related Files**: 
- `src/lsp/syntax.zig` (LSP completion logic)
- `src/lsp/test/syntax_test.zig` (failing tests)
- `src/canonicalize/CIR.zig` (type annotations)
- `src/types/` (type system)

## Problem Summary

When implementing LSP record field completion, we discovered that the Roc type system only stores a SUBSET of record fields in type variables, even when there's an explicit type annotation declaring all fields. This causes completion to only suggest the fields that were actually accessed in the code, not all fields declared in the type annotation.

## Expected Behavior

Given this Roc code:
```roc
my_record : { foo : Str, bar : I64 }
my_record = { foo: "hello", bar: 42 }

get_foo = my_record.foo
get_bar = my_record.bar

incomplete = my_record.  # <-- cursor here
```

When requesting LSP completion after `my_record.`, it should suggest BOTH fields: `foo` and `bar`.

## Actual Behavior

Completion only suggests ONE field (whichever was processed last by the type checker), typically `bar`.

## Root Cause Analysis

### Investigation Steps

1. **Verified snapshot mechanism works correctly**
   - Snapshot is created from clean code with full type annotation
   - Snapshot environment is correctly retrieved during completion
   - Used `used_snapshot=true` flag to track when snapshot is being used

2. **Verified name-based lookup finds correct definition**
   - Modified `addRecordFieldCompletions` to ALWAYS use name-based lookup in top-level defs/statements
   - Removed `if (found_binding == null)` checks to always search defs/statements
   - Confirmed it finds the `my_record` definition (pattern=@enumFromInt(1))

3. **Discovered type variable only contains 1 field**
   - Debug output shows: `record.fields=.{ .start = @enumFromInt(13), .count = 1 }`
   - Only `bar` field is stored in the type variable
   - This occurs even though:
     - There's an explicit type annotation: `my_record : { foo : Str, bar : I64 }`
     - Both fields are accessed: `get_foo = my_record.foo` and `get_bar = my_record.bar`

4. **Investigated type annotations vs inferred types**
   - Found that `Def` has an `annotation: ?Annotation.Idx` field (src/canonicalize/CIR.zig:119)
   - `Annotation` contains `anno: TypeAnno.Idx` which is the AST of the type annotation
   - TypeAnno for the record shows: `.record = .{ .fields = .{ .span = ... } }`
   - The annotation DOES contain both fields in the AST
   - BUT the type variable (`ModuleEnv.varFrom(def.pattern)`) only has 1 field

### Key Finding: Row Polymorphism vs Explicit Annotations

The issue appears to be how Roc's type system handles **row polymorphism** with explicit type annotations.

When you write:
```roc
my_record : { foo : Str, bar : I64 }
my_record = { foo: "hello", bar: 42 }
```

The type checker appears to:
1. Parse the type annotation and store it as AST in `def.annotation`
2. Infer the type from usage and row polymorphism
3. Store only the USED fields in the type variable associated with the pattern

When `get_foo = my_record.foo` is processed, `my_record` gets a specialized type `{ foo : Str | r }` (record with `foo` field and polymorphic rest).

When `get_bar = my_record.bar` is processed, `my_record` gets another specialized type `{ bar : I64 | r }`.

The type variable stored in the definition appears to be one of these SPECIALIZED types, not the FULL type from the annotation!

## Attempted Fixes

### Fix 1: Always Use Snapshot Environment ✅ (Implemented)
**Location**: `src/lsp/syntax.zig:2978-2022`

Changed `getCompletionsAtPosition` to ALWAYS try snapshot environment first, before falling back to current build.

**Rationale**: When typing incomplete code, the current build fails but snapshot has correct types from last clean build.

**Result**: SUCCESS - Snapshot is now correctly used, but still only shows 1 field due to underlying type system issue.

### Fix 2: Skip CIR-Based Lookup When Using Snapshot ✅ (Implemented)
**Location**: `src/lsp/syntax.zig:3037`

When `used_snapshot=true`, skip `findDotReceiverTypeVar` (which analyzes dot expressions in CIR) and use name-based lookup instead.

**Rationale**: Cursor positions in incomplete code don't correspond to snapshot's CIR.

**Result**: SUCCESS - Name-based lookup works, but still only finds 1 field.

### Fix 3: Always Use Name-Based Def/Stmt Lookup ✅ (Implemented)
**Location**: `src/lsp/syntax.zig:3275-3330`

Removed `if (found_binding == null)` checks to ALWAYS search top-level defs and statements by name, even when a local binding is found.

**Rationale**: Local binding's pattern_idx may point to incomplete code without full type info. Top-level defs from snapshot should have better type info.

**Result**: SUCCESS - Finds correct definition, but type variable still only has 1 field.

### Fix 4: Attempt to Use Type Annotation ⚠️ (Partially Implemented)
**Location**: `src/lsp/syntax.zig:3305-3318`

Added code to detect when `def.annotation` exists and extract the `TypeAnno` AST.

**Code Added**:
```zig
if (def.annotation) |anno_idx| {
    std.debug.print("addRecordFieldCompletions: def has annotation, trying to get type from it\n", .{});
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    std.debug.print("addRecordFieldCompletions: type_anno={any}\n", .{type_anno});
    
    // Try to get the type var from the annotation
    // Type annotations should have been checked and have associated type vars
    // For now, fall through to using pattern's type var
}
```

**Result**: INCOMPLETE - Successfully accesses the annotation AST which shows `.record = .{ .fields = .{ .span = ... } }`, but need to:
1. Extract field names from the `TypeAnno.record.fields` span
2. Convert TypeAnno field types to type variables for completion detail strings
3. This requires understanding the TypeAnnotation.zig API

**Status**: Left as TODO for future work. The annotation DOES contain all fields, we just need to extract them.

## Debug Output Example

When completion is requested for `my_record.` with snapshot:

```
completion: used_snapshot=true
addRecordFieldCompletions: looking for 'my_record' at offset 60
addRecordFieldCompletions: checking 3 top-level defs
  ALL DEFS: 'my_record' pattern=@enumFromInt(1) def_idx=@enumFromInt(16)
  ALL DEFS: 'get_foo' pattern=@enumFromInt(2) def_idx=@enumFromInt(19)
  ALL DEFS: 'get_bar' pattern=@enumFromInt(3) def_idx=@enumFromInt(22)
addRecordFieldCompletions: FOUND def 'my_record' pattern=@enumFromInt(1)
addRecordFieldCompletions: def has annotation, trying to get type from it
addRecordFieldCompletions: type_anno=.{ .record = .{ .fields = .{ .span = ... } } }
addRecordFieldCompletions: type_var for def=@enumFromInt(1)
addFieldsFromTypeVar: type_var=@enumFromInt(1), content tag=structure
addFieldsFromTypeVar: found record after resolve
addFieldsFromRecord: record.fields=.{ .start = @enumFromInt(13), .count = 1 }
addFieldsFromRecord: field 'bar'
```

**Key observations**:
- Snapshot has 3 defs: my_record, get_foo, get_bar (only ONE my_record def, not two)
- Type annotation AST shows `.record = .{ .fields = ... }` (contains both fields in AST)
- Type variable shows `count = 1` (only one field stored)
- Only 'bar' field is retrieved

## Potential Solutions

### Solution A: Extract Fields from Type Annotation AST (Recommended)
**Complexity**: Medium  
**Impact**: High  
**Files**: `src/lsp/syntax.zig`

Modify `addRecordFieldCompletions` to:
1. Check if `def.annotation` exists
2. If it does, parse the `TypeAnno.record.fields` span to extract ALL field names
3. For each field in the annotation, create a completion item
4. For type details, either:
   - Use a simplified string from the TypeAnno AST
   - Or look up the field's type var if available

**Advantages**:
- Gets ALL fields declared in type annotation
- Doesn't require changing type system
- LSP-only change

**Challenges**:
- Need to understand TypeAnnotation.zig API for extracting fields
- Need to handle nested records, aliases, etc.
- Type detail strings might be less accurate than resolved types

**Next Steps**:
1. Study `src/canonicalize/TypeAnnotation.zig` to understand the API
2. Find how to iterate over `TypeAnno.record.fields.span`
3. Extract field names and type information
4. Implement field extraction in `addRecordFieldCompletions`

### Solution B: Fix Type System to Preserve All Fields
**Complexity**: Very High  
**Impact**: High (affects entire compiler)  
**Files**: `src/check/`, `src/types/`, potentially many others

Modify the type checker to:
1. When a definition has an explicit type annotation, store the FULL type from the annotation
2. Don't narrow the stored type based on usage due to row polymorphism
3. Keep both the "annotation type" and "usage type" separate

**Advantages**:
- Fixes root cause
- Benefits all tools, not just LSP
- More semantically correct

**Challenges**:
- Very complex change to core type system
- Need deep understanding of Hindley-Milner type inference with row polymorphism
- Could affect performance (storing more type information)
- Risk of breaking other parts of compiler
- Requires extensive testing

**Status**: Out of scope for LSP completion fix. Should be filed as separate issue.

### Solution C: Change Test Strategy (Workaround)
**Complexity**: Low  
**Impact**: Low (test-only)  
**Files**: `src/lsp/test/syntax_test.zig`

Modify tests to avoid relying on explicit type annotations:
1. Use records where ALL fields are accessed in a single expression
2. Or test with records that have only 1 field
3. Document the limitation

**Advantages**:
- Quick fix to unblock testing
- No changes to production code

**Disadvantages**:
- Doesn't fix the actual problem
- Tests don't cover real-world usage patterns

## Relevant Code Locations

### Type Annotation Structures
- `src/canonicalize/CIR.zig:113-130` - `Def` struct with `annotation` field
- `src/canonicalize/CIR.zig:326-330` - `Annotation` struct with `anno: TypeAnno.Idx`
- `src/canonicalize/TypeAnnotation.zig` - TypeAnno union and field extraction
- `src/canonicalize/NodeStore.zig:102-108` - DefData with `anno_idx`

### Type System
- `src/types/` - Type variable storage and resolution
- `src/check/` - Type checking and inference
- `types.Record` - Record type with `fields: { start, count }`

### LSP Completion
- `src/lsp/syntax.zig:2978-3022` - Snapshot environment selection
- `src/lsp/syntax.zig:3032-3050` - Record field completion dispatch
- `src/lsp/syntax.zig:3242-3330` - `addRecordFieldCompletions` name-based lookup
- `src/lsp/syntax.zig:3396-3457` - `addFieldsFromTypeVar` and `addFieldsFromRecord`

## Testing Status

### Failing Tests
1. `test.record field completion works for modules` (src/lsp/test/syntax_test.zig:287)
   - Expected: foo=true, bar=true
   - Actual: foo=false, bar=true

2. `test.record completion uses snapshot env when builds fail` (src/lsp/test/syntax_test.zig:375)
   - Similar issue

3. `test.record field completion with partial field name` (src/lsp/test/syntax_test.zig:437)
   - Similar issue

### Test Characteristics
All failing tests:
- Create clean code with explicit type annotation: `my_record : { foo : Str, bar : I64 }`
- Create snapshot via `checker.check(uri, clean_contents, null)`
- Test completion on incomplete code that triggers snapshot usage
- Expect both fields to be suggested

## Recommendations

1. **Immediate**: Implement Solution A (extract from annotation AST)
   - Most practical fix for LSP completion
   - Doesn't require deep type system changes
   - Should work for most common cases

2. **Short-term**: File issue about type system not preserving annotation types
   - Document the problem for compiler maintainers
   - Include this analysis as context
   - Reference Solution B as potential fix

3. **Long-term**: Consider Solution B if this affects other tools
   - May be needed for other IDE features (go-to-definition, hover, etc.)
   - Requires design discussion with compiler team
   - Significant effort, should be planned carefully

## Open Questions

1. **Why does the type system only store one field?**
   - Is this intentional for row polymorphism?
   - Is it a bug in how annotations constrain inferred types?
   - Does this affect runtime behavior or just tools?

2. **How do other Roc tools handle this?**
   - Does the REPL have similar issues?
   - Does the error reporting system need full field info?
   - Are there existing workarounds in other parts of the codebase?

3. **What's the performance impact of Solution B?**
   - How much extra memory for storing full annotation types?
   - Does it slow down type checking?
   - Are there cases where specialized types are actually needed?

4. **TypeAnnotation.zig API questions:**
   - How to iterate over fields in `TypeAnno.record.fields.span`?
   - How to convert TypeAnno types to strings for completion details?
   - How to handle record extensions (`{ foo : Str | rest }`)?
   - How to handle nested records and aliases?

## Related Issues

- Row polymorphism and type inference interaction
- LSP completion accuracy vs type system architecture
- Trade-offs between type specialization and annotation preservation

## Next Steps for Future Work

1. Study `src/canonicalize/TypeAnnotation.zig` API
2. Find examples of code that iterates over TypeAnno record fields
3. Implement field extraction from TypeAnno in `addRecordFieldCompletions`
4. Add helper function `addFieldsFromTypeAnnotation`
5. Test with various record types (nested, with extensions, etc.)
6. Update tests to verify all fields are suggested
7. Remove debug print statements once tests pass

## Code Changes Summary

### Files Modified
- `src/lsp/syntax.zig` - Multiple changes to completion logic
- `src/lsp/test/syntax_test.zig` - Tests now use `module []` instead of `app`

### Key Changes in syntax.zig

**Line 2978-3022**: Always try snapshot first
```zig
var used_snapshot = false;
const module_env_opt: ?*ModuleEnv = blk: {
    if (self.snapshot_envs.get(absolute_path)) |snapshot_env| {
        const snapshot_module_env = self.getModuleEnvByPathInEnv(snapshot_env, absolute_path);
        if (snapshot_module_env) |module_env| {
            used_snapshot = true;
            break :blk module_env;
        }
    }
    // ... fallback logic
};
```

**Line 3037**: Skip CIR-based lookup when using snapshot
```zig
if (used_snapshot or self.findDotReceiverTypeVar(module_env, cursor_offset) == null) {
    std.debug.print("completion: using name-based lookup (snapshot={}, or findDotReceiverTypeVar failed)", .{used_snapshot});
    try self.addRecordFieldCompletions(&items, module_env, record_access.variable_name, record_access.variable_start);
```

**Line 3275-3330**: Always search defs/statements (removed `if (found_binding == null)`)
```zig
// Check top-level definitions (not just when found_binding==null)
const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
for (defs_slice) |def_idx| {
    // ... find by name ...
}

// Check statements (apps use statements for definitions)
const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
for (statements_slice) |stmt_idx| {
    // ... find by name ...
}
```

**Line 3305-3318**: Added annotation detection (incomplete)
```zig
if (def.annotation) |anno_idx| {
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    // TODO: Extract fields from type_anno instead of using pattern's type var
}
```

### Debug Print Statements Added

Many `std.debug.print` statements were added for investigation. These should be removed once the issue is resolved:
- Line 3024: `completion: context=..., used_snapshot=...`
- Line 3287: `ALL DEFS: ...` 
- Line 3305: `def has annotation...`
- Line 3310: `type_anno=...`
- Line 3432: `addFieldsFromRecord: record.fields=...`
- And many more throughout the completion logic

## Conclusion

This investigation revealed a fundamental limitation in how Roc's type system stores record type information. While the LSP completion logic has been improved to correctly use snapshots and name-based lookup, the underlying type variables only contain a subset of fields due to row polymorphism specialization.

The most practical solution is to extract field information directly from the type annotation AST (`def.annotation`), which does contain all declared fields. This requires further work to understand the TypeAnnotation.zig API and implement field extraction logic.

The alternative—fixing the type system to preserve all annotation fields—is a much larger undertaking that should be considered separately as a compiler-wide improvement.
