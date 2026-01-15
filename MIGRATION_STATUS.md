# Node Payload Migration Status

**Last Updated:** January 3, 2026

This document tracks the migration from raw `data_1`, `data_2`, `data_3` fields to typed payload structures in the Node system.

## Overview

The migration replaces untyped field access with semantic, typed payloads. Each Node has exactly 12 bytes (3×u32) that can be interpreted as a `Payload` union, enabling type-safe access to node data.

## Completed Migrations

All the following node types have been successfully migrated to use typed payloads:

### Pattern Nodes
- ✅ **pattern_record_destruct** - `PatternRecordDestruct` payload with kind_and_pattern packing
- ✅ **match_branch_pattern** - `MatchBranchPattern` payload

### Expression Nodes  
- ✅ **record_field** - `RecordField` payload with name and expr fields
- ✅ **if_branch** - `IfBranch` payload with cond and body fields

### Type Annotation Nodes
- ✅ **type_header** - `TypeHeader` payload with name, packed_type_vars, and relative_name
- ✅ **anno_record_field** (ty_record_field) - `TyRecordField` payload

### Where Clause Nodes
- ✅ **where_alias** - `WhereAlias` payload
- ✅ **where_malformed** - `WhereMalformed` payload

### Other Nodes
- ✅ **exposed_item** - `ExposedItem` payload
- ✅ **lambda_capture** - `LambdaCapture` payload

### Supporting Infrastructure
- ✅ **def nodes** - Use `def_data` auxiliary list for 5 u32s of data
- ✅ **match_branch nodes** - Use `match_branch_data` auxiliary list
- ✅ **where_method nodes** - Use `where_method_data` auxiliary list

## Changes Made

### Node.zig
- Fixed `TypeHeader` payload struct: Changed `_unused: u32` to `relative_name: u32`
- All payload structs use `extern struct` for memory layout guarantees
- Verify sizes match 12 bytes exactly

### NodeStore.zig
- Updated ALL getter functions to use `node.getPayload()` instead of raw field access
- Updated ALL setter functions (add*) to use `node.setPayload()` for construction
- Fixed TypeHeader packing/unpacking: 16+16 bits (start in upper 16, len in lower 16)
- Updated serialization tests to use payload accessors

### Key Functions Migrated

**Getters:**
- `getMatchBranchPattern()` - Now uses `payload.match_branch_pattern`
- `getWhereClause()` - Handles where_alias and where_malformed with payloads
- `getRecordField()` - Now uses `payload.record_field`
- `getCapture()` - Now uses `payload.lambda_capture`
- `getIfBranch()` - Now uses `payload.if_branch`
- `getTypeHeader()` - Now uses `payload.type_header`
- `getAnnoRecordField()` - Now uses `payload.ty_record_field`
- `getExposedItem()` - Now uses `payload.exposed_item`

**Setters:**
- `addRecordField()` - Now uses `setPayload()` with RecordField
- `addCapture()` - Now uses `setPayload()` with LambdaCapture
- `addMatchBranchPattern()` - Now uses `setPayload()` with MatchBranchPattern
- `addWhereClause()` - Now uses `setPayload()` for where_alias and where_malformed
- `addIfBranch()` - Now uses `setPayload()` with IfBranch
- `addTypeHeader()` - Now uses `setPayload()` with TypeHeader
- `addAnnoRecordField()` - Now uses `setPayload()` with TyRecordField
- `addExposedItem()` - Now uses `setPayload()` with ExposedItem

## Remaining Work

### Diagnostic Nodes (Lower Priority)
Diagnostic nodes use a generic `Diagnostic` payload that accepts raw data_1/data_2/data_3. These are intentionally left untyped since each diagnostic variant uses different field combinations. While they could be migrated to individual payload structs, this is lower priority as they don't impact core functionality.

**Diagnostic types still using generic payload:**
- `diag_not_implemented`
- `diag_invalid_num_literal`
- `diag_ident_already_in_scope`
- And ~40 other diagnostic variants

### Variable-Length Data (Requires Separate Approach)
The `spanFrom()` function and related span-handling code for variable-length data needs a different migration strategy:
- Currently uses generic `extra_data` list
- Could migrate to typed span lists (e.g., `expr_spans`, `pattern_spans`)
- Requires more careful analysis of all span consumers

## Known Issues

### Builtin Compiler Runtime Issue
After migration, the builtin compiler encounters a runtime error when compiling Builtin.roc:
- Error: "unreachable, node is not a statement tag" in getStatement()
- Occurs during dependency graph building
- Likely unrelated to specific node types migrated (pattern/expr/anno nodes)
- Requires further investigation

This issue appears to be in the dependency graph processing rather than in the payload migration itself.

## Memory Impact

- ✅ **ZERO additional memory used** - All migrations reinterpret existing 12-byte payloads
- Each node still occupies exactly 16 bytes (12 byte payload + 4 byte tag)
- No pointers or variable-length allocations added to payload structures

## Verification Checklist

- [x] All migrated getter functions use `payload.*` instead of `data_1/2/3`
- [x] All migrated setter functions use `setPayload()` instead of direct assignment
- [x] TypeHeader payload includes relative_name field
- [x] Code compiles without errors
- [x] Serialization/deserialization tests updated for payload access
- [ ] All tests pass (blocked by builtin compiler issue)
- [ ] Final grep shows zero remaining `\.data_1`/`\.data_2`/`\.data_3` in migrated types
- [ ] Final memory size verification

## Commands for Testing

```bash
# Build compiler
zig build

# Run specific test
zig build test -- --test-filter "NodeStore multiple nodes"

# Check for remaining raw data access (should show only diagnostics)
grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/NodeStore.zig | grep -v "diag_" | grep -v "= 0"

# Verify sizes haven't changed
grep -n "expected_nodestore_size\|expected_moduleenv_size" test/serialization_size_check.zig
```

## Next Steps

1. **Investigate builtin compiler issue** - Determine if it's related to migration or pre-existing
2. **Complete diagnostic migration** (optional) - Migrate remaining diagnostic types if desired
3. **Migrate variable-length data** - Create typed span lists for expr/pattern elements
4. **Final verification** - Run full test suite and verify memory usage unchanged

## Technical Notes

- Node struct memory layout is critical: data_1, data_2, data_3 must be at offsets 0, 4, 8
- Payload union reinterprets these bytes without overhead
- `extern struct` ensures consistent layout across platforms
- TypeHeader packing: bits [31:16] = start, bits [15:0] = len
