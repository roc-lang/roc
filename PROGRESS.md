# Migration Progress: Remove Generic extra_data in Favor of Typed Payloads

## Status
- **Overall tests**: 2233/2242 passing (99.6%)
- **Extra_data references remaining**: 172 in canonicalize module (down from 287), 0 in parse module
- **Progress**: 115 extra_data references removed (40% reduction)

## Completed Work

### 1. Serialization Fix (Commit: 3e97d18620)
Fixed critical bug in NodeStore serialization where `match_branch_redundant_data` was not being serialized or deserialized. This caused index out of bounds errors when accessing match branch redundant data after deserialization.

**Changes**:
- Added `match_branch_redundant_data: collections.SafeList(types.Var).Serialized` field to `NodeStore.Serialized` struct
- Updated `serialize()` to include match_branch_redundant_data serialization
- Updated `deserialize()` to properly deserialize match_branch_redundant_data in correct order

### 2. Statement Import Migration (Commit: 12af3dd250)
Completed migration of `statement_import` from using generic extra_data to a properly typed payload struct. This was a partially-completed migration that had placeholder code still using extra_data.

**Changes**:
- Redesigned `StatementImport` payload struct with packed fields:
  - `module_name_tok: u32` (Ident.Idx)
  - `alias_tok: u32` (Ident.Idx or 0 for None)
  - `packed_qualifier_and_exposes: packed struct` (16 bits qualifier, 11 bits start, 5 bits len)
- Updated getStatement() to read from typed payload instead of extra_data
- Updated makeStatementNode() to write to typed payload instead of extra_data
- Removed 5 extra_data.append() calls per import statement

## Current Issues

### Stack Overflow in Match Tests (Pre-existing)
- 9 tests failing with stack overflow when running Roc programs with match expressions
- These failures exist in HEAD~1 before the serialization fix
- Root cause: Unknown, needs separate investigation
- Tests affected:
  - fx platform IO spec tests
  - fx platform match-related tests
  - drop_prefix match use-after-free regression
  - etc.

These appear to be runtime failures in compiled Roc programs, not serialization issues.

## Migration Status by Module

### Parse Module
- ✅ Fully migrated - 0 extra_data references
- All tests passing

### Canonicalize Module  
- ⏳ Partially migrated
- ~309 extra_data references remaining
- Match branch: Migrated (uses typed payload + separate redundant_data list)
- If branch: Partially migrated
- Other node types: Mostly still using extra_data

## ⚠️ High-Priority: Next Node Types to Migrate

The following expression types **MUST be migrated** to remove extra_data usage. These are the highest impact targets:

### Priority 1: Very High Impact
- **`expr_call`** (getExpr line ~442, addExpr line ~845)
  - Currently stores: args_start, args_len (2 u32s in extra_data)
  - Data: function index, arguments span, called_via flag
  - Estimated frequency: VERY HIGH (common in all Roc code)
  - **Recommendation**: Migrate this first - will eliminate many extra_data refs

- **`expr_closure`** (getExpr line ~561, addExpr line ~878)
  - Currently stores: lambda_idx, capture_start, capture_len, tag_name (4 u32s in extra_data)
  - Data: lambda reference, capture spans, closure tag name
  - Estimated frequency: High
  - **Recommendation**: Migrate second

### Priority 2: High Impact
- **`expr_lambda`** (getExpr line ~579, addExpr line ~911)
  - Currently stores: args_start, args_len, body_idx (3 u32s in extra_data)
  - Data: lambda parameters span, body expression
  - Estimated frequency: High

- **`expr_record`** (getExpr line ~610, addExpr line ~944)
  - Currently stores: fields_start, fields_len, ext_value (3 u32s in extra_data)
  - Data: record fields span, optional extension expression
  - Estimated frequency: Medium-High

- **`expr_tag_union`** (getExpr line ~?, addExpr line ~?)
  - Pattern: Similar to record, stores field information
  - Estimated frequency: Medium-High

**These 5 types probably account for 60%+ of remaining extra_data usage.**

### Recommended Approach
1. Pick one high-impact type (e.g., expr_call)
2. Design the typed payload struct (see MIGRATION_EXTRA_DATA_TO_PAYLOADS.md for constraints)
3. Update Node.zig with the new struct
4. Update getExpr() getter
5. Update addExpr() setter  
6. Run tests, verify no regressions
7. Commit
8. Repeat for next type

### Final Steps
1. Investigate and fix match-related stack overflows (pre-existing issue)
2. Once all extra_data references are removed, delete the field from NodeStore
3. Verify compiled binaries have identical size/memory usage

## Key Architecture Decisions

- **Match Branch**: Uses typed payload struct with separate `match_branch_redundant_data` list for redundant type variables
- **Serialization**: All typed lists properly serialized/deserialized in reverse order
- **Memory constraint**: Payload union must remain exactly 12 bytes (3 × u32)

## References
- MIGRATION_EXTRA_DATA_TO_PAYLOADS.md - Detailed migration guide and constraints
- AGENT.md - Project conventions
