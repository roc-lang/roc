# Migration Status Update

## Session Summary

Completed migration work on eliminating extra_data and data_* field usage in the Roc compiler.

### What Was Fixed This Session

#### 1. Expression Tuple & Tag Nodes (NodeStore)
- **Before**: `replaceExprWithTuple` and `replaceExprWithTag` directly set data_1/data_2/data_3
- **After**: Now use `node.setPayload()` with typed `expr_tuple` and `expr_tag` structures
- **Files**: src/canonicalize/NodeStore.zig
- **Impact**: Better type safety, no data loss, same memory footprint

#### 2. Match Branch Pattern Nodes
- **Before**: `addMatchBranchPattern` and getter directly accessed data_1/data_2
- **After**: Now use `setPayload` and `getPayload` with typed `match_branch_pattern` structure
- **Files**: src/canonicalize/NodeStore.zig
- **Impact**: 2 fields now typed instead of generic

#### 3. Hosted Compiler Definition Update
- **Before**: Directly manipulated `extra_data.items.items[extra_start + 1]`
- **After**: Uses proper `env.store.setDefExpr(def_idx, expr_idx)` API
- **Files**: src/canonicalize/HostedCompiler.zig
- **Impact**: Cleaner code, properly uses typed def_data list

#### 4. Compile-Time Evaluator Variable-Length Data
- **Before**: Created tag and tuple expression spans by appending to `extra_data`
- **After**: Uses scratch buffer API (`scratchExprTop()` and `addScratchExpr()`)
- **Files**: src/eval/comptime_evaluator.zig (3 fixes)
- **Changes**:
  - Line 1070-1083: Tag expression argument spans
  - Line 1165-1178: Tag expression with arguments
  - Line 1223-1234: Tuple element spans
- **Impact**: Consistent with rest of compiler, uses scratch buffers for variable-length data

### Current State

**All tests passing**: 2273 tests ✓

**Remaining references to data_*/extra_data**: ~259 (mostly in problem/diagnostic handling which is separate domain)

**Active cleanups completed**:
- Core expression/pattern handling ✓
- Tuple and tag node creation ✓
- Definition updates ✓
- Compile-time folding ✓

### CRITICAL: Migration Is NOT Complete

**Current State: INCOMPLETE**

There are still ~259 references to `data_1`, `data_2`, `data_3`, and fields using them throughout the codebase.

**Success Criteria**: The migration is **ONLY SUCCESSFUL** when:
```bash
grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
# Must return: 0
```

If ANY references remain, the migration has NOT succeeded.

### Remaining Work (MUST BE COMPLETED)

The ~259 remaining references are in these node types - ALL MUST BE MIGRATED:

#### 1. Statement Nodes
- `statement_reassign` - Uses data_1 for assign_ident (Can.zig:10758)
- `statement_expr` - Uses data_1 for statement/expr indices
- Other statement types

#### 2. Type Annotation Nodes  
- `type_header` - Uses data_1/data_2/data_3 for names and packed args
- `anno_record_field` - Uses data_1/data_2 for name/type
- Other annotation types

#### 3. Problem/Diagnostic Nodes
- All problem node variants store diagnostic info in data_1/data_2/data_3
- Examples: `problem_feature_does_not_exist`, `problem_ident_not_found`, etc.

#### 4. Exposed/Module Nodes
- `exposed_item` - Uses data_1/data_2/data_3 for names and wildcard flag
- Other module-level constructs

#### 5. Other Expression/Pattern Nodes
- `record_field` - Uses data_1/data_2
- `pattern_capture` - Uses data_1/data_2/data_3
- `if_branch` - Uses data_1/data_2
- And others

### Migration Path Forward

Each remaining type needs:
1. **Define a typed payload struct** in Node.zig with semantic field names
2. **Add to the Payload union** in Node.zig  
3. **Replace all reads** with `node.getPayload().type_name`
4. **Replace all writes** with `node.setPayload(.{ .type_name = ... })`
5. **Verify** that the grep command returns 0

### Verification

Memory footprint: **MUST NOT INCREASE** - all changes maintain exact same Node layout
- Node is 16 bytes: 3 x u32 + Tag
- Payload union reinterprets those same 12 bytes
- Zero overhead, zero memory change

### Final Status

**This migration is NOT done until the grep returns 0. There is no "mostly done".**

Every remaining reference must be eliminated through proper typed payload structures.
