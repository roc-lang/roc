# Code Review: remove-extra-data Branch

## Overview
Reviewing diff between `origin/main` and `remove-extra-data` branch.

**Files changed:** 18
**Lines added:** ~4126
**Lines removed:** ~1837

---

## Issues and Concerns

### 1. CRITICAL: Planning/Status files should not be committed

The following files appear to be working notes/documentation that should NOT be part of the final commit:

- `MIGRATION_PLAN.md` (245 lines)
- `MIGRATION_README.md` (150 lines)
- `MIGRATION_STATUS.md` (148 lines)
- `MIGRATION_UPDATE.md` (111 lines)
- `PROGRESS.md` (114 lines)
- `docs/extra_data_elimination.md` (381 lines)

These are internal planning documents with session notes, TODOs, and status tracking. They should be removed before merging.

**Note:** The status docs mention "211 references remaining" and explicitly state the migration is "NOT complete". This raises the question of whether this branch is ready for review/merge at all.

### 2. Node.zig Changes - Overall Good Design

The migration to typed payloads is well-structured:
- `Payload` union with typed variants for each node type
- `getPayload()`/`setPayload()` API
- Compile-time size verification (12-byte payload, 16-byte node)

**Concerns:**

a) **`Payload.Diagnostic` is still untyped** - It just wraps `data_1`, `data_2`, `data_3` with no semantic names:
```zig
pub const Diagnostic = extern struct {
    data_1: u32,
    data_2: u32,
    data_3: u32,
};
```
This defeats the purpose of the migration for diagnostic nodes.

b) **`Payload.Raw` still exists** - While needed for migration, the planning docs indicate this should eventually be removed. The branch shouldn't be merged until this is removed or there's a plan for when it will be.

### 3. Misleading Payload Struct Field Names and Comments

Several payload structs have comments claiming fields are "packed" when they're actually just storing separate values in different u32 fields. This defeats the purpose of semantic naming:

**TyTagUnion:**
```zig
pub const TyTagUnion = extern struct {
    /// Packed: tags_start (20 bits), tags_len (12 bits)  <- COMMENT IS WRONG
    packed_tags: u32,   // Actually just stores start, not packed
    ext_kind: u32,      // Actually stores len, misnamed
    _unused: u32,       // Actually stores ext
};
```

**TyTag:**
```zig
pub const TyTag = extern struct {
    name: u32,
    /// Packed: args_start (20 bits), args_len (12 bits)  <- COMMENT IS WRONG
    packed_args: u32,   // Actually just stores start
    _unused: u32,       // Actually stores len
};
```

**TyTuple:** Same issue - `packed_elems` just stores start, `_unused1` stores len.

**TyRecord:** `packed_fields` stores start, `ext_plus_one` actually stores len (not ext+1!).

This is not a functional bug (getter/setter are consistent), but the naming is misleading and will cause confusion for future maintainers.

### 4. Potential Deserialization Bug on 32-bit Platforms

In `NodeStore.Serialized.deserialize()`, there's a comment explaining that fields must be deserialized in reverse order to avoid corruption on 32-bit platforms. However, `diag_region_data` is deserialized AFTER the `self` pointer is cast to `store`:

```zig
const deserialized_where_method_data = self.where_method_data.deserialize(base_addr).*;

// Overwrite ourself with the deserialized version, and return our pointer after casting it to NodeStore
const store = @as(*NodeStore, @ptrFromInt(@intFromPtr(self)));

const deserialized_diag_region_data = self.diag_region_data.deserialize(base_addr).*;  // BUG?
```

This could potentially read corrupted data on 32-bit platforms since it's accessing `self` after it might have been partially overwritten. The line should be moved up with the other deserializations.

### 5. parse/NodeStore.zig Successfully Removes extra_data

The parse module has fully migrated away from `extra_data`:
- The `extra_data` field is completely removed
- Replaced with typed extra data lists (`platform_header_extra`, `import_extra`, etc.)
- Replaced with typed span content lists (`expr_span_data`, `statement_span_data`, etc.)

This is a good example of what the complete migration should look like. **However**, note that the canonicalize `NodeStore` still has `extra_data` field, so the migration is incomplete.

### 6. NodeStore Size Increased Significantly

The serialization size check shows:
- `expected_nodestore_size` changed from **96 to 168** bytes (+72 bytes, ~75% increase)
- `expected_moduleenv_size` changed from **1160 to 1232** bytes (+72 bytes)

This is due to adding new auxiliary lists to NodeStore:
- `int_values: SafeList(i128)`
- `dec_values: SafeList(RocDec)`
- `def_data: SafeList(DefData)`
- `match_branch_data: SafeList(MatchBranchData)`
- `where_method_data: SafeList(WhereMethodData)`
- `diag_region_data: SafeList(Region)`

While the Node struct itself is still 16 bytes, the NodeStore overhead increased. The migration docs claim "ZERO additional memory used" but this is only true per-node, not for the overall data structure.

### 7. Test Updates Required Due to Bit-Packing Constraints

The test file changes show that many random values now have constrained bit widths:
- `rand_span()` now uses `u20` for start and `u11` for len (was `u32` and `u30`)
- `e_hosted_lambda.index` is now `u8` (was `u32`)
- `e_hosted_lambda.body` is now `u24` (was full `u32`)
- Various other fields have reduced ranges

This is a necessary trade-off for packing more data into 12 bytes, but it does mean the compiler now has limits:
- Max span start: ~1M entries
- Max span len: ~2K items
- Max hosted lambda index: 255

These limits should be documented and validated at runtime to prevent silent overflow.

### 8. Migration Progress (Incomplete)

Current state of the migration:

```bash
$ grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
155  # References remaining

$ grep -rn "extra_data" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
35   # extra_data references remaining
```

The migration documents state "There is NO 'mostly done'. Every single reference must be eliminated." With 155 `data_*` references and 35 `extra_data` references remaining, **this branch is not ready for merge**.

---

## Summary

### Critical Issues (Must Fix)
1. **Planning documentation files should be deleted** - MIGRATION_PLAN.md, MIGRATION_README.md, etc.
2. **Migration is incomplete** - 155 `data_*` references and 35 `extra_data` references remain
3. **Potential 32-bit deserialization bug** - `diag_region_data` deserialized after pointer cast

### Code Quality Issues (Should Fix)
4. **Misleading payload field names** - `packed_tags`, `ext_kind`, `packed_args`, etc. have wrong names
5. **`Payload.Diagnostic` still uses untyped `data_1/data_2/data_3`** - defeats migration purpose
6. **`Payload.Raw` still exists** - should be removed when migration completes

### Observations (FYI)
7. NodeStore struct size increased by 72 bytes due to auxiliary lists
8. Various fields now have reduced bit-width limits that need documentation
9. parse/NodeStore.zig shows the complete migration pattern (no extra_data)

### Recommendation

**Do not merge this branch yet.** The migration is substantial work-in-progress but is explicitly documented as incomplete. Either:
1. Complete the remaining migration (eliminate all `data_*` and `extra_data` references)
2. Or split the branch into mergeable chunks with the planning docs removed

