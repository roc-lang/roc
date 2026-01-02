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

## Next Steps

1. **Continue migrating remaining node types** in canonicalize module
2. **Investigate match-related stack overflows** (separate from this task)
3. **Remove extra_data field entirely** once all migrations complete
4. **Verify zero-byte memory increase** in production builds

## Key Architecture Decisions

- **Match Branch**: Uses typed payload struct with separate `match_branch_redundant_data` list for redundant type variables
- **Serialization**: All typed lists properly serialized/deserialized in reverse order
- **Memory constraint**: Payload union must remain exactly 12 bytes (3 × u32)

## References
- MIGRATION_EXTRA_DATA_TO_PAYLOADS.md - Detailed migration guide and constraints
- AGENT.md - Project conventions
