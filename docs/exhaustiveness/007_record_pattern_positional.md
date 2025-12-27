# Issue: Record Pattern Positional Handling Limitation

## Problem Statement

The exhaustiveness algorithm treats record patterns **positionally** in some parts of the code, but records should be matched **by field name**. This causes issues when different patterns destructure different subsets of fields.

## Current Behavior (Problematic)

```roc
match record {
    { name, age } => ...      # Destructures 2 fields
    { name } => ...           # Destructures 1 field - DIFFERENT ARITY
}
```

The algorithm expects patterns to have the same arity (number of sub-patterns) for specialization. When record patterns have different numbers of fields, the positional algorithm fails.

## Location in Codebase

**File:** `src/check/exhaustive.zig`

**Line ~1277-1282:**
```zig
/// Returns error.TypeError if the payload types don't match the expected arity.
/// This can happen for records where the pattern destructures fewer fields
/// than the actual record type has - a known limitation of the current algorithm
/// that treats records positionally instead of by field name.
```

**Line ~1299-1305:**
```zig
// For tag unions, the arity should match exactly.
// For records, the pattern might destructure fewer fields than the actual type has.
// Currently, we don't handle records by field name, so return TypeError to skip
// exhaustiveness checking in that case.
if (payload_types.len != expected_arity) {
    return error.TypeError;
}
```

## Why This Happens

The Maranget algorithm works on a **pattern matrix** where each row is a pattern and each column is a position. For tag unions like `[Ok(a), Err(e)]`:
- `Ok(val)` has 1 argument at position 0
- `Err(e)` has 1 argument at position 0
- They specialize independently

For records:
- `{ name, age }` has fields `name` and `age`
- `{ name }` has only field `name`
- They're NOT positionally equivalent!

## Expected Behavior (Correct)

Record patterns should be compared **by field name**, not by position:

```roc
match record {
    { name: "Alice", age } => ...   # Matches name="Alice", any age
    { name, age: 30 } => ...        # Matches any name, age=30
    { name, age } => ...            # Matches any name and age
}
```

All three patterns have the same "signature" (they all match records with `name` and `age`), so they should be analyzed together even though they destructure different aspects.

## Current Workaround

The code has a workaround in `specializeByRecordPattern()`:
1. It looks up field types by name (correctly)
2. But `specializeByConstructor()` uses positional arity checks
3. When arities don't match, it returns `TypeError` to skip checking

This means some valid record patterns cause exhaustiveness checking to be skipped entirely.

## Solution Requirements

1. **Normalize record patterns:** Before analysis, expand all record patterns to include all fields from the record type
2. **Use field names consistently:** When specializing, match by field name not position
3. **Handle partial destructuring:** `{ name }` on a `{ name, age }` record should be treated as `{ name, age: _ }`
4. **Maintain correct semantics:** The wildcard for unmentioned fields should not affect pattern matching

## Implementation Approach

### Option A: Pattern Normalization

Before exhaustiveness checking:
1. Determine the full set of fields in the record type
2. Expand each pattern to include all fields (with wildcards for unmentioned)
3. Use consistent field ordering

```roc
# Original patterns
{ name }           # becomes { name, age: _ }
{ name, age }      # stays   { name, age }

# Now both have same arity and can use positional algorithm
```

### Option B: Field-Name Based Specialization

Modify the specialization algorithm:
1. Instead of specializing by "constructor with arity N"
2. Specialize by "record with fields F1, F2, ..."
3. Each field becomes a separate column in the matrix

## Functions to Modify

1. `convertPattern()` (line ~482): Normalize record patterns to full field set
2. `specializeByRecordPattern()` (line ~1328): Already does name-based lookup
3. `specializeByConstructor()` (line ~1293): Remove arity check for records, or
4. `specializeRowByConstructor()`: Handle records differently than tag unions

## Key Insight

The `render_as` field in `Union` already distinguishes records:
```zig
pub const RenderAs = union(enum) {
    tag_union,
    record: []const Ident.Idx,  // Field names
    tuple,
    guard,
    opaque_type,
};
```

This can be used to branch between positional (tag union) and named (record) handling.

## Testing

Create test cases for:
1. Different patterns destructuring different field subsets
2. Same fields in different orders
3. Nested record patterns with varying fields
4. Mix of record and non-record patterns
5. Record with wildcard for some fields

## Acceptance Criteria

- [ ] Record patterns with different field counts work correctly
- [ ] Field order doesn't affect exhaustiveness analysis
- [ ] Partial destructuring is handled (fields not mentioned = wildcard)
- [ ] No `TypeError` skipping for valid record patterns
- [ ] No "known limitation" comments
- [ ] Comprehensive tests for record pattern variations
