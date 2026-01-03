# Eliminating extra_data: A Step-by-Step Guide

## CRITICAL CONSTRAINT

**Memory usage MUST be EXACTLY THE SAME after this migration.**

This is not optional. This is not a suggestion. If even ONE MORE BYTE of memory is used after this migration, the migration has COMPLETELY FAILED.

Why? Because **that's how unions work**. A union reinterprets existing bytes - it doesn't add new ones. Every "typed payload" is just a different view of the same 12 bytes that were always there.

## Background

The `NodeStore` has a field called `extra_data: SafeList(u32)` that serves as a catch-all dumping ground for data that doesn't fit in a node's 12-byte payload (3 × u32). This is:

1. **Untyped** - Just a bag of u32s with no structure
2. **Error-prone** - Easy to read/write wrong offsets
3. **Hard to understand** - Must trace code to know what each u32 means

The goal is to eliminate `extra_data` entirely by using:
- **Typed payload unions** for data ≤12 bytes
- **Specialized typed lists** for data >12 bytes
- **Existing scratch array spans** for variable-length data

## How Node Payloads Work

Each `Node` has:
```zig
data_1: u32,
data_2: u32,
data_3: u32,
tag: Tag,
```

The first three fields (12 bytes total) can be reinterpreted as ANY 12-byte struct via the `Payload` union in `Node.zig`:

```zig
pub const Payload = extern union {
    raw: Raw,                    // Generic access: data_1, data_2, data_3
    expr_var: ExprVar,           // Typed access for expr_var nodes
    expr_call: ExprCall,         // Typed access for expr_call nodes
    // ... etc
};
```

**This union adds ZERO memory.** It's just different ways to interpret the same 12 bytes.

## Current extra_data Users (What Needs Migration)

### Category 1: EASY - Data fits in 12 bytes

These are using extra_data for NO GOOD REASON. The data fits in 12 bytes and should use typed payloads.

#### 1.1 `pattern_record_destruct` (2 u32s in extra_data)

**Current layout:**
- `data_1`: label (Ident.Idx)
- `data_2`: ident (Ident.Idx)
- `data_3`: extra_data index
- `extra_data[0]`: kind tag (0=Required, 1=SubPattern)
- `extra_data[1]`: pattern index

**Fix:** Pack kind into `data_3`:
```zig
pub const RecordDestruct = extern struct {
    label: u32,      // Ident.Idx
    ident: u32,      // Ident.Idx
    // bit 0: kind tag (0=Required, 1=SubPattern)
    // bits 1-31: pattern index
    packed_kind: u32,
};
```

**Files to modify:**
- `Node.zig`: Add `RecordDestruct` payload struct to the Payload union
- `NodeStore.zig`:
  - `addRecordDestruct` (line 2067): Update to use packed payload
  - `getRecordDestruct` (line 2697): Update to read from packed payload

---

### Category 2: MEDIUM - Data exceeds 12 bytes, needs auxiliary list

These store more than 12 bytes and need a specialized typed list (like `int_values` or `diag_region_data`).

#### 2.1 `def` (5 u32s in extra_data)

**Current layout:**
- `data_1`: extra_data index
- `data_2`: count (5)
- `data_3`: unused
- `extra_data[0]`: pattern (Idx)
- `extra_data[1]`: expr (Idx)
- `extra_data[2-3]`: kind (encoded as 2 u32s)
- `extra_data[4]`: annotation (Idx or 0)

**Fix:** Add `def_data: SafeList(DefData)` to NodeStore:
```zig
pub const DefData = extern struct {
    pattern: u32,        // CIR.Pattern.Idx
    expr: u32,           // CIR.Expr.Idx
    kind_tag: u8,        // 0=let, 1=stmt, 2=ignored
    _pad: [3]u8,
    kind_var: u32,       // TypeVar (only valid if kind_tag != 0)
    annotation: u32,     // CIR.Annotation.Idx or 0
};
```

**Node payload:**
```zig
pub const Def = extern struct {
    def_data_idx: u32,   // Index into def_data list
    _unused1: u32,
    _unused2: u32,
};
```

**Files to modify:**
- `Node.zig`: Add `Def` payload struct to the Payload union
- `NodeStore.zig`:
  - Add `def_data: SafeList(DefData)` field (around line 32, near other SafeLists)
  - `addDef` (line 2602): Update to append to def_data and store index
  - `getDef` (line 2635): Update to read from def_data
  - `setDefExpr` (line 2661): Update to modify def_data entry

#### 2.2 `match_branch` (5 u32s in extra_data)

**Current layout:**
- `data_1`: extra_data index
- `extra_data[0]`: patterns.span.start
- `extra_data[1]`: patterns.span.len
- `extra_data[2]`: value (Expr.Idx)
- `extra_data[3]`: guard (Expr.Idx or 0)
- `extra_data[4]`: redundant (types.Var)

**Fix:** Add `match_branch_data: SafeList(MatchBranchData)` to NodeStore:
```zig
pub const MatchBranchData = extern struct {
    patterns_start: u32,
    patterns_len: u32,
    value: u32,          // CIR.Expr.Idx
    guard: u32,          // CIR.Expr.Idx or 0
    redundant: u32,      // types.Var
};
```

**Node payload:**
```zig
pub const MatchBranch = extern struct {
    branch_data_idx: u32,  // Index into match_branch_data list
    _unused1: u32,
    _unused2: u32,
};
```

**Files to modify:**
- `Node.zig`: Add/update `MatchBranch` payload struct in the Payload union
- `NodeStore.zig`:
  - Add `match_branch_data: SafeList(MatchBranchData)` field (around line 32)
  - `addMatchBranch` (line 2118): Update to append to match_branch_data
  - `getMatchBranch` (line 959): Update to read from match_branch_data

#### 2.3 `where_method` (3 u32s in extra_data)

**Current layout:**
- `data_1`: var_ (TypeAnno.Idx)
- `data_2`: method_name (Ident.Idx)
- `data_3`: extra_data index
- `extra_data[0]`: args.span.start
- `extra_data[1]`: args.span.len
- `extra_data[2]`: ret (TypeAnno.Idx)

**Fix:** Add `where_method_data: SafeList(WhereMethodData)`:
```zig
pub const WhereMethodData = extern struct {
    args_start: u32,
    args_len: u32,
    ret: u32,           // CIR.TypeAnno.Idx
};
```

**Node payload:**
```zig
pub const WhereMethod = extern struct {
    var_: u32,           // TypeAnno.Idx
    method_name: u32,    // Ident.Idx
    method_data_idx: u32, // Index into where_method_data list
};
```

**Files to modify:**
- `Node.zig`: Add `WhereMethod` payload struct to the Payload union
- `NodeStore.zig`:
  - Add `where_method_data: SafeList(WhereMethodData)` field (around line 32)
  - `addWhereClause` (line 2161): Update `.w_method` case to use where_method_data
  - `getWhereClause` (line 1003): Update to read from where_method_data

#### 2.4 `expr_typed_int` and `expr_typed_frac` (4 u32s for 128-bit value)

**Current layout:**
- `data_1`: type_name (Ident.Idx)
- `data_2`: value kind
- `data_3`: extra_data index
- `extra_data[0-3]`: 128-bit value as 4 u32s

**Fix:** These already have a solution pattern - use `int_values: SafeList(i128)` style.

Actually check if `int_values` already exists and can be reused. If not, add:
```zig
typed_int_values: SafeList(i128),
typed_frac_values: SafeList(f128), // or whatever the actual type is
```

**Node payload:**
```zig
pub const ExprTypedInt = extern struct {
    type_name: u32,      // Ident.Idx
    value_kind: u32,
    value_idx: u32,      // Index into typed_int_values
};
```

**Files to modify:**
- `Node.zig`: Verify `ExprTypedInt` and `ExprTypedFrac` payload structs exist
- `NodeStore.zig`:
  - Check if `int_values` list can be reused, or add new typed value lists
  - Getter for expr_typed_int (line 509): Update to read from value list
  - Getter for expr_typed_frac (line 523): Update to read from value list
  - Setter in addExpr `.e_typed_int` case (line 1734): Update to use value list
  - Setter in addExpr `.e_typed_frac` case (line 1749): Update to use value list

---

### Category 3: VARIABLE LENGTH - Use scratch spans directly

These store variable-length data (N items). The scratch array mechanism already exists.

#### 3.1 `spanFrom` function (~line 2760)

This function copies scratch buffer items INTO extra_data and returns a span. This is backwards.

**Current behavior:**
1. Build up items in scratch buffer
2. Copy all items to extra_data
3. Return span pointing into extra_data

**Fix:** Keep items in scratch buffer, return span pointing into scratch:

The scratch buffers should be persistent (not cleared), and spans should point directly into them. This requires:

1. Converting scratch buffers from temporary to permanent storage
2. Updating all span consumers to read from the correct buffer

**Affected functions:**
- `spanFrom` (line 2760): The core function that copies to extra_data
- `replaceExprWithTuple` (line 894): Uses variable elem_indices
- `replaceExprWithTag` (line 920): Uses variable arg_indices

**Alternative approach:** Create typed span lists:
```zig
expr_spans: SafeList(CIR.Expr.Idx),      // For tuple elements, tag args, etc.
pattern_spans: SafeList(CIR.Pattern.Idx), // For pattern lists
// etc.
```

Then spans point into these typed lists instead of generic extra_data.

---

## Migration Checklist

For each node type:

- [ ] **pattern_record_destruct** - Pack 2 u32s into payload (EASY)
- [ ] **def** - Add `def_data` auxiliary list
- [ ] **match_branch** - Add `match_branch_data` auxiliary list
- [ ] **where_method** - Add `where_method_data` auxiliary list
- [ ] **expr_typed_int** - Use typed value list
- [ ] **expr_typed_frac** - Use typed value list
- [ ] **spanFrom / variable-length** - Use typed span lists

## After Migration

Once all uses are migrated:

1. **Delete the extra_data field** from NodeStore
2. **Delete extra_data from Serialized** struct
3. **Update serialization_size_check.zig** expected sizes
4. **Run all tests** - they must pass
5. **Verify memory usage** - must be EXACTLY the same or LESS

## Verification

Before and after the migration, measure:
```
sizeof(NodeStore)
sizeof(NodeStore.Serialized)
Peak memory usage running test suite
```

These numbers must be equal or the "after" must be smaller. If "after" is larger, **THE MIGRATION FAILED**.

---

## Appendix: Why This Works

A union in Zig (or C) is just a way to interpret bytes differently:

```zig
const Payload = extern union {
    raw: extern struct { a: u32, b: u32, c: u32 },
    typed: extern struct { x: u16, y: u16, z: u32, w: u32 },
};
```

Both `raw` and `typed` occupy the same 12 bytes. Accessing `.raw` or `.typed` just tells the compiler how to interpret those bytes.

There is no "union overhead". There is no "extra memory for the union". The union IS the bytes. Different fields are different VIEWS of the same bytes.

This is why the migration is guaranteed to use the same memory - we're not adding anything, we're just giving names to bytes that were already there.
