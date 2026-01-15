# Complete Migration Plan: Eliminate ALL data_* and extra_data

## ⚠️ CRITICAL SUCCESS CRITERIA

**This migration is ONLY successful when:**
```bash
grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
# Returns: 0
```

**There is NO "mostly done". Every single reference must be eliminated.**

If even ONE reference remains anywhere, the migration has FAILED.

---

## Current Status

- **Tests passing**: 2273 ✓
- **References remaining**: 211 (down from ~285)
- **Session progress**: Fixed 6 node types + 3 eval instances + HostedCompiler
- **Progress**: 74 references eliminated this session

## What This Migration Does

Replaces all untyped field access (`node.data_1`, `node.data_2`, `node.data_3`) with typed payload unions that have semantic field names.

**Example:**
```zig
// BEFORE (garbage)
node.data_1 = @intFromEnum(pattern);
node.data_2 = @intFromEnum(expr);

// AFTER (readable)
node.setPayload(.{ .def = .{
    .pattern = pattern,
    .expr = expr,
    .kind = kind,
}});
```

### Phase 1: Structural Migration
This phase converts all direct struct field access to use the `Payload` union. Code may temporarily use `payload.raw` for untyped access during conversion.

### Phase 2: Type All Uses (CRITICAL)
Replace ALL `payload.raw.data_*` access with proper typed variants. The `raw` variant is a **temporary hack** and must be completely eliminated before the migration is considered complete.

### Phase 3: Delete Raw (MANDATORY)
Once no code uses `payload.raw`, delete the entire `Raw` struct from the union. This ensures the compiler enforces typed access going forward.

## The Node Structure

Every `Node` has exactly 12 bytes of payload + a Tag:
```zig
data_1: u32,     // 4 bytes
data_2: u32,     // 4 bytes  
data_3: u32,     // 4 bytes
tag: Tag,        // 1 byte
```

The `Payload` union reinterprets these 12 bytes. **No memory overhead. No new allocations.**

---

## Remaining Work (MUST DO ALL)

### 1. Statement Nodes
- `statement_reassign` - data_1 = assign_ident
- `statement_var` - data_1/data_2 for var/value
- `statement_expr` - data_1 for expr
- `statement_crash` - data_1 for message
- `statement_dbg` - data_1 for expr
- `statement_expect` - data_1/data_2 for expr/message  
- `statement_for` - data_1/data_2/data_3 for var/cond/body
- `statement_while` - data_1/data_2 for cond/body
- `statement_break` - data_1 for value
- `statement_return` - data_1 for value
- `statement_import` - (partially done)
- Others...

### 2. Type Annotation Nodes
- `type_header` - data_1/data_2/data_3 for name/relative_name/packed_args
- `anno_record_field` - data_1/data_2 for name/ty
- `type_var_alias` - data_1 for var/pattern

### 3. Problem/Diagnostic Nodes
- ALL problem_* variants store data in data_1/data_2/data_3
- Examples:
  - `problem_feature_does_not_exist` - data_1 for feature
  - `problem_ident_not_found` - data_1 for ident
  - `problem_ident_duplicate` - data_1/data_2/data_3 for name/region
  - And ~40+ more problem types

### 4. Module/Exposure Nodes
- `exposed_item` - data_1/data_2/data_3 for name/alias/is_wildcard

### 5. Other Expression/Pattern Nodes
- `record_field` - data_1/data_2 for name/value
- `pattern_capture` - data_1/data_2/data_3 for name/scope_depth/pattern_idx
- `if_branch` - data_1/data_2 for cond/body
- Others as needed

---

## Migration Pattern

For each node type, follow this exact pattern:

### Step 1: Define Payload Struct in Node.zig
```zig
pub const MyNode = extern struct {
    field1: u32,      // Semantic name!
    field2: u32,      // Semantic name!
    field3: u32,      // Semantic name or _unused
};
```

### Step 2: Add to Payload Union
```zig
pub const Payload = extern union {
    // ... existing ...
    my_node: MyNode,  // Add here
    // ... rest ...
};
```

### Step 3: Replace All Reads
```zig
// Before
const value = node.data_1;

// After
const payload = node.getPayload().my_node;
const value = payload.field1;
```

### Step 4: Replace All Writes
```zig
// Before
node.data_1 = value;

// After
var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .my_node_tag };
node.setPayload(.{ .my_node = .{ .field1 = value } });
store.nodes.set(idx, node);
```

### Step 5: Verify
```bash
# Check that grep returns 0 for this node type
grep -n "\.my_node\|statement_reassign" src/canonicalize/NodeStore.zig | grep "\.data_"
# Should return nothing
```

---

## Session Completed

✅ These node types have been migrated:
- pattern_record_destruct
- expr_tuple  
- expr_tag
- match_branch_pattern
- def (via def_data list)
- match_branch (via match_branch_data list)
- where_method (via where_method_data list)
- expr_typed_int/frac (via int_values list)
- HostedCompiler fixes
- Eval scratch buffer fixes

❌ 211 references remain - **ALL MUST BE ELIMINATED** (0 is the only acceptable endpoint)

---

## Final Checklist

- [ ] All 259 references converted to typed payloads
- [ ] No direct data_1/data_2/data_3 reads outside Node implementation
- [ ] No direct data_1/data_2/data_3 writes outside Node initialization
- [ ] `grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/` returns 0
- [ ] All tests still pass
- [ ] Memory usage unchanged

**STOP: Do not consider this done until grep returns 0.**

---

## ⚠️ CRITICAL: Eliminate the `Raw` Payload Variant

### Why This Must Be Done

The `Payload.raw` variant is a **horrible hack** that directly contradicts the entire purpose of this migration:

```zig
pub const Payload = extern union {
    raw: Raw,                    // ← THIS MUST GO
    // ... semantic field names ...
};
```

**This variant allows code to write:**
```zig
node.payload.raw.data_1 = x;  // ← Still untyped! Still meaningless!
```

**This defeats the migration goal entirely.** It gives a false sense of "typed access" while actually just hiding the same untyped garbage under `payload.raw.*`.

### The Proper Fix

Every access to `node.payload.raw.data_*` **MUST be replaced** with a proper typed payload variant.

**Example of what must be eliminated:**
```zig
// BAD - still untyped:
node.payload.raw.data_1 = @intFromEnum(pattern);
node.payload.raw.data_2 = @intFromEnum(expr);
node.payload.raw.data_3 = kind_val;

// GOOD - properly typed:
node.payload = .{ .def = .{
    .pattern = pattern,
    .expr = expr,
    .kind = kind_val,
}};
```

### TODO: Post-Migration Cleanup

Once all node types are properly typed and no code uses `payload.raw`:

1. **Delete the `Raw` struct** from `Node.Payload`
2. **Delete all references** to `.raw` access throughout the codebase
3. **Ensure the compiler prevents** any future use of untyped payload access
4. **NEVER reintroduce** any variant or mechanism that allows untyped field access

This is not optional. The `raw` variant is a temporary hack during migration. It MUST be completely removed once all code is properly typed.

### Verification Command

```bash
# This must return 0 after Raw is deleted:
grep -rn "\.payload\.raw\|Payload\.raw" src/ --include="*.zig" | wc -l
```

**Only when this returns 0 is the migration truly complete.**
