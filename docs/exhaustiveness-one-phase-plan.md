# Plan: Convert Exhaustiveness to 1-Phase Design with Rich Error Messages

## Project Policy: No "Known Limitations"

**"Known limitations" are unacceptable in this project.** Any time we discover functionality that doesn't work correctly, it must be reframed as **incomplete work that must be completed**. We do not:
- Document limitations and move on
- Add workarounds that mask bugs
- Update test expectations to accept broken behavior

Instead, we:
- Use `std.debug.assert` to catch invariant violations in debug builds
- Return proper errors (like `error.TypeError`) that propagate correctly
- Track incomplete work explicitly and implement it fully

---

## Overview

The exhaustiveness implementation in `src/check/exhaustive.zig` is being converted from a 2-phase design to a 1-phase on-demand reification design.

**Old 2-phase design:**
1. Converts patterns to `UnresolvedPattern` without type info
2. Batch-reifies all patterns upfront, tracking failures via `has_unresolved_ctor`
3. Skips checking entirely when any pattern couldn't be resolved

**New 1-phase design:**
1. Patterns are converted to `UnresolvedPattern` (sketched)
2. Reification happens on-demand during usefulness/exhaustiveness checking
3. Type errors propagate immediately via `error.TypeError`
4. `Check.zig` catches `TypeError` and skips exhaustiveness error reporting

---

## Progress Summary

### Completed

- **Phase 2: Error Infrastructure** - DONE
  - HumanIndex type for ordinal formatting
  - Severity enum and updated Error types
  - Enhanced pattern formatting (records, tuples, guards, literals)
  - Record field names stored in RenderAs

- **Phase 3: 1-Phase Reification** - DONE
  - `ReifyError` type added (`OutOfMemory` or `TypeError`)
  - `SketchedMatrix` type for working with `UnresolvedPattern`
  - `isUsefulSketched` - on-demand reification during usefulness checking
  - `checkExhaustiveSketched` - on-demand reification during exhaustiveness checking
  - `checkRedundancySketched` - redundancy checking with sketched patterns
  - `checkMatch` updated to use 1-phase approach
  - `Check.zig` handles `TypeError` by skipping exhaustiveness errors
  - Scrutinee type validated at start of `checkMatch` (flex/rigid vars return TypeError)
  - Debug assertions added for invariant checking

- **Phase 3.6: Record Pattern Field Lookup** - DONE
  - Added `getRecordFieldTypeByName` function to look up field types by name
  - Added `ColumnTypes.specializeByRecordPattern` method for field-name-based specialization
  - Updated `specializeByConstructorSketched` to align fields across patterns with different field sets
  - Updated `collectCtorsSketched` to collect all unique fields from all patterns
  - Updated `isUsefulSketched` and `checkExhaustiveSketched` to use field-name-based lookup for records
  - Note: When patterns have type errors (e.g., TYPE MISMATCH), exhaustiveness checking is skipped since field type lookup fails on Error types. This is reasonable behavior.

### Incomplete Work (Must Be Completed)

- **Phase 4: Remove TODOs** - Some removed, others still valid
- **Phase 5: Context-specific error messages**
- **Phase 6: Final cleanup**

---

## Important Instructions for Implementer

1. **Commit frequently** - After each major step, make a commit.

2. **NEVER commit this plan document** - This markdown file is for guidance only.

3. **Run tests after each change** - Use `zig build test-check` for quick iteration, `zig build test` for full suite.

4. **No workarounds** - If something doesn't work, fix it properly or track it as incomplete work.

5. **Use debug assertions** - When invariants should hold, use `std.debug.assert`. This catches bugs in debug builds.

6. **Reference the old implementation**:
   - `crates/compiler/exhaustive/src/lib.rs` - Core algorithm
   - `crates/compiler/can/src/exhaustive.rs` - Pattern sketching and reification

---

## Phase 3.6: Fix Record Pattern Field Type Lookup (COMPLETE)

### The Problem

The current implementation treats record patterns positionally, but records are matched by field **name**, not position. This causes a mismatch when different patterns have different fields.

**Example that fails:**
```roc
match ... {
    { name, age } => ...           # Pattern has 2 fields
    { name, address: { city } } => ... # Pattern has 2 different fields
    {} => ...                      # Pattern has 0 fields
}
```

After type unification, the scrutinee type becomes `{ name, age, address: { city } }` with 3 fields. But when we specialize by the first pattern's constructor, we:
1. Call `getCtorArgTypes(type_store, column_type, tag_id)`
2. Get back 3 field types (for all fields in the unified type)
3. But the pattern only has 2 args (for `name` and `age`)
4. The arity mismatch causes `specializeByConstructor` to return `TypeError`

**Current behavior:** Exhaustiveness checking is skipped for these cases.

**Required behavior:** Correctly look up field types by name, not position.

### The Fix

Modify record pattern handling to look up field types by field name instead of position.

#### Step 1: Add a function to get field type by name

```zig
/// Look up a record field's type by its name.
/// Returns null if the field doesn't exist in the record type.
fn getRecordFieldTypeByName(
    type_store: *TypeStore,
    record_type: Var,
    field_name: Ident.Idx,
) ?Var {
    const resolved = type_store.resolveVar(record_type);
    // Handle .structure -> .record and .record_unbound
    // Look up the field by name in the fields list
    // Return the field's type variable
}
```

#### Step 2: Add a specialized method for record patterns

In `ColumnTypes`, add:

```zig
/// Specialize column types for a record pattern.
/// Unlike tag unions (positional), records are matched by field name.
/// `field_names` are the names of the fields being destructured.
/// Returns the types for those specific fields in the given order.
pub fn specializeByRecordPattern(
    self: ColumnTypes,
    allocator: std.mem.Allocator,
    field_names: []const Ident.Idx,
) error{ OutOfMemory, TypeError }!ColumnTypes {
    std.debug.assert(self.types.len > 0);

    const record_type = self.types[0];
    const field_types = try allocator.alloc(Var, field_names.len);

    for (field_names, 0..) |name, i| {
        field_types[i] = getRecordFieldTypeByName(self.type_store, record_type, name)
            orelse return error.TypeError;
    }

    // New types: [field_types..., self.types[1...]...]
    const new_types = try allocator.alloc(Var, field_types.len + self.types.len - 1);
    @memcpy(new_types[0..field_types.len], field_types);
    if (self.types.len > 1) {
        @memcpy(new_types[field_types.len..], self.types[1..]);
    }

    return .{ .types = new_types, .type_store = self.type_store };
}
```

#### Step 3: Use the new method for record patterns

In `isUsefulSketched`, when handling `.known_ctor`:

```zig
.known_ctor => |kc| {
    const specialized = try specializeByConstructorSketched(...);

    // Use field-name-based lookup for records
    const specialized_types = switch (kc.union_info.render_as) {
        .record => |field_names| try column_types.specializeByRecordPattern(allocator, field_names),
        else => try column_types.specializeByConstructor(allocator, kc.tag_id, kc.args.len),
    };

    // ... rest unchanged
}
```

Similarly update `checkExhaustiveSketched` and any other places that call `specializeByConstructor` for `known_ctor` patterns.

#### Step 4: Handle tuples correctly

Tuples ARE positional, so they should continue using `specializeByConstructor`. The `render_as` field distinguishes them:
- `.record` -> use `specializeByRecordPattern`
- `.tuple` -> use `specializeByConstructor`
- `.tag` / `.opaque` -> use `specializeByConstructor`

### Files to Modify

1. `src/check/exhaustive.zig`:
   - Add `getRecordFieldTypeByName` function
   - Add `ColumnTypes.specializeByRecordPattern` method
   - Update `isUsefulSketched` to use the new method for records
   - Update `checkExhaustiveSketched` similarly
   - Update the old `isUseful` and `checkExhaustive` if they're still used

### Test Cases

These tests now pass with correct redundancy detection:
- `test/snapshots/match_expr/record_destructure.md` ✓
- `test/snapshots/match_expr/nested_record_patterns.md` ✓
- `test/snapshots/match_expr/record_pattern_edge_cases.md` ✓ (updated expectations - skips redundancy when type errors exist)

---

## Phase 4: Remove Remaining TODOs

TODOs that remain in the code and need separate fixes:

| Location | TODO | Fix Needed |
|----------|------|------------|
| ~line 131 | Track type info for wildcards | Ensure type info is always available |
| ~line 297 | String deduplication | Compare actual string content |
| ~line 981 | Nominal type emptiness | Inspect type arguments properly |
| ~line 1000 | Flex vs rigid extension | Study old implementation's approach |
| ~line 1076 | Type parameter substitution | Implement proper substitution |

**Commit after each TODO resolution**

---

## Phase 5: Add Context-Specific Error Messages

The old implementation has different messages for different contexts:

```rust
match context {
    BadArg => "put a `when` in the function body to account for all possibilities",
    BadDestruct => "Use a `when` to account for all possibilities",
    BadCase => "Add branches for them!",
}
```

Enhance error reporting in `Check.zig` to use context-specific messages.

---

## Phase 6: Final Cleanup and Verification

1. Remove any remaining dead code
2. Run full test suite: `zig build test`
3. Add tests for error message quality
4. Manual testing with real Roc code

---

## Current State

The 1-phase reification is **fully implemented** including record pattern field lookup (Phase 3.6). All tests pass.

To verify:
```bash
zig build test
```

The record pattern tests (`record_destructure.md`, `nested_record_patterns.md`) now correctly detect redundant patterns.

**Note on type errors:** When patterns contain type errors (e.g., TYPE MISMATCH), exhaustiveness checking is skipped because field type lookup fails on Error types. This is intentional - we shouldn't report pattern errors when the patterns themselves are type-invalid. The test `record_pattern_edge_cases.md` expectations were updated to reflect this behavior.
