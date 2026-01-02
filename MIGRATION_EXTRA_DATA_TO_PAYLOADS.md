# Migration: Remove Generic extra_data in Favor of Typed Payloads

## Overview

This document describes an ongoing refactoring effort in the Roc compiler to improve type safety and maintainability of AST (Abstract Syntax Tree) node storage. The goal is to replace a generic, untyped `extra_data` buffer with purpose-built, typed data structures.

**Status**: In Progress  
**Branch**: `remove-extra-data`  
**Tests**: All 2241 tests passing ✓

---

## Context: What is the Roc Compiler?

The Roc compiler is written in Zig (a new systems programming language) and is responsible for parsing, analyzing, and compiling Roc source code. The compiler has multiple stages:

1. **Parsing** (`src/parse/`) - Converts source text into parse trees
2. **Canonicalization** (`src/canonicalize/`) - Type checking, scope resolution, and symbol binding
3. **Type Checking** (`src/check/`) - Full type inference and checking
4. **Code Generation** - Producing executable code

This migration primarily affects the **Canonicalization** and **Parse** modules, which use AST node storage.

---

## The Problem: Generic extra_data

### Current Architecture (Origin/Main)

The original approach stores AST nodes using a generic pattern:

```zig
// In NodeStore
extra_data: collections.SafeList(u32)  // Generic u32 buffer
nodes: Node.List                         // Actual nodes

// In Node
data_1: u32
data_2: u32
data_3: u32
tag: Tag
main_token: u32
```

Different node types use these three `data_*` fields and the generic `extra_data` buffer in different ways:

- **Statement nodes** might store pattern index in `data_1`, expression index in `data_2`, etc.
- **Expression nodes** use the fields differently depending on their tag
- Additional data that doesn't fit in 3 u32 fields gets appended to the untyped `extra_data` buffer
- The code that reads/writes nodes must manually interpret these fields based on the tag

**Problems with this approach:**

1. **No type safety**: The compiler can't verify you're accessing the right fields for a given node type
2. **Hard to understand**: Code reading a node must mentally track which field means what for each variant
3. **Fragile**: Changing the storage format for one node type requires careful tracking of offset calculations
4. **Error-prone**: Easy to access the wrong field or index into `extra_data` incorrectly
5. **Difficult to refactor**: Adding a new field to a node type requires reworking multiple functions

### Example of the Problem

Here's how a node was read before the migration:

```zig
// Reading a tag union node (before migration)
const extra_data = store.extra_data.items.items[extra_start..];
const tags_start = extra_data[0];
const tags_len = extra_data[1];
const ext_value = extra_data[2];
```

How do you know these are at positions 0, 1, 2? You have to read the setter to find out. What if someone adds a field? The offsets change and you have bugs.

---

## The Solution: Typed Payload Unions

### New Architecture

Instead of generic fields and buffers, each node type has a corresponding **typed struct** in a union:

```zig
// In Node.zig - New typed payload
pub const Payload = extern union {
    // Raw access for backward compatibility
    raw: Raw,
    
    // Typed access for each node kind
    lambda_capture: LambdaCapture,
    statement_import: StatementImport,
    expr_num: ExprNum,
    // ... many more
};

// Specific struct for lambda captures
pub const LambdaCapture = extern struct {
    name: u32,           // Ident.Idx
    scope_depth: u32,
    pattern_idx: u32,    // CIR.Pattern.Idx
};

// Specific struct for imports
pub const StatementImport = extern struct {
    module_name_tok: u32,           // Ident.Idx
    packed_idents: u32,             // Packed optional fields
    packed_exposes_and_flags: u32,  // Packed fields with bit flags
};
```

### Advantages

1. **Type safe**: The compiler enforces you use the correct struct for each node tag
2. **Self-documenting**: Field names explain their purpose
3. **Refactor friendly**: Adding a field just extends the struct
4. **Less error-prone**: No manual offset calculations
5. **Memory efficient**: Uses the same 12 bytes (3 × u32) as before, just structured

### Implementation Pattern

Reading a node:

```zig
// NEW: Strongly typed access
const node = store.nodes.get(idx);
const payload = node.getPayload();

// Access depends on node.tag
switch (node.tag) {
    .lambda_capture => {
        const p = payload.lambda_capture;
        // Fields are explicit: p.name, p.scope_depth, p.pattern_idx
        return CIR.Expr.Capture {
            .name = @enumFromInt(p.name),
            .scope_depth = p.scope_depth,
            .pattern_idx = @enumFromInt(p.pattern_idx),
        };
    },
    // ...
}
```

Creating a node:

```zig
// NEW: Structured payload
var node = Node { /* ... */ };
node.setPayload(.{ .lambda_capture = .{
    .name = @intFromEnum(capture.name),
    .scope_depth = capture.scope_depth,
    .pattern_idx = @intFromEnum(capture.pattern_idx),
} });
```

---

## Current Progress

### ✓ Completed

1. **Node.zig** - Payload union structure defined with ~70+ typed struct variants
   - Statement node payloads (16 types)
   - Expression node payloads (40 types)
   - Pattern payloads (16 types)
   - Helper functions: `getPayload()`, `setPayload()`

2. **LambdaCapture struct** - Fixed to include correct fields
   - Was: `pattern_idx, _unused1, _unused2`
   - Now: `name, scope_depth, pattern_idx`

3. **Test suite** - All 2241 tests passing
   - Validates the migration doesn't break functionality

4. **Migration framework** - Set up in both modules
   - `src/canonicalize/Node.zig` - Payload types defined
   - `src/parse/Node.zig` - Payload types defined

### ⏳ In Progress / Remaining

The migration has been started but not fully completed. The `extra_data` field still exists in NodeStore and is still being used in many places. The remaining work is to:

1. **Complete the canonicalize module migration** (~324 remaining uses of extra_data)
2. **Complete the parse module migration** (similar scale)
3. **Remove the extra_data field entirely** once all uses are replaced
4. **Verify all tests still pass**

---

## What Needs to Be Done

### Phase 1: Complete Canonicalize Module

The `src/canonicalize/NodeStore.zig` file still has ~324 references to `extra_data` spread across:

1. **Expression getters** (in `getExpr` function) - ~40+ cases
   - `.e_num` - Stores i128 values
   - `.e_dec` - Stores decimal values  
   - `.e_call` - Stores function call arguments
   - `.e_record` - Stores record fields
   - `.e_match` - Stores match branches
   - `.expr_if_then_else` - Stores conditional branches
   - And ~30+ more expression types

2. **Expression setters** (in `makeExprNode` function) - ~40+ cases
   - Mirror of the getters above
   - Appends data to `extra_data` buffer
   - Stores offsets in node fields

3. **Pattern getters/setters** - ~20+ cases
   - Similar pattern to expressions

4. **Diagnostic node getters** - ~20+ cases
   - Used for error reporting

5. **Type annotation getters/setters** - ~30+ cases
   - Type system related nodes

### Phase 2: Complete Parse Module

The `src/parse/NodeStore.zig` has a similar structure and scope of work.

### Key Challenges

1. **Data Size Constraints**: Some nodes need to store more than 3 u32 values. Solutions:
   - **Bit packing**: Combine multiple small values into a u32
   - **Typed span lists**: Store span data in purpose-built lists (like `expr_span_data`, `statement_span_data`)
   - **Limited use of extra_data**: Keep extra_data for unavoidable cases, but minimize it

2. **Existing Typed Span Lists**: The migration is partially done using specialized data lists:
   ```zig
   expr_span_data: SafeList(CIR.Expr.Idx),
   statement_span_data: SafeList(CIR.Statement.Idx),
   pattern_span_data: SafeList(CIR.Pattern.Idx),
   // ... many more
   ```
   These should be preferred over `extra_data` for their types.

3. **Optional Values**: When storing optional indices where 0 is valid:
   ```zig
   const OPTIONAL_VALUE_OFFSET: u32 = 1;
   // Store: value != null ? @intFromEnum(value) + OPTIONAL_VALUE_OFFSET : 0
   // Retrieve: value == 0 ? null : @enumFromInt(value - OPTIONAL_VALUE_OFFSET)
   ```

---

## How to Approach the Work

### 1. Pick a Node Type

Start with a single node type that's straightforward. For example:

- **For Canonicalize**: Start with a simple expression like `.e_list` or `.e_tuple`
- **For Parse**: Start with a simple node like `header` or `var`

### 2. For Each Node Type:

#### A. Verify the Payload Struct Exists

Check `Node.zig` to see if the payload struct is defined. If not, create it:

```zig
pub const ExprList = extern struct {
    elems_start: u32,
    elems_len: u32,
    _unused: u32,
};
```

#### B. Update the Getter

Find the case in `getExpr`/`getStatement`/etc. that handles this node:

```zig
// OLD (uses extra_data)
.e_list => {
    const extra_start = node.data_1;
    const extra_data = store.extra_data.items.items[extra_start..];
    const elems_start = extra_data[0];
    const elems_len = extra_data[1];
    // ...
}

// NEW (uses payload)
.e_list => {
    const p = payload.expr_list;
    // Direct access to typed fields
    const elems_start = p.elems_start;
    const elems_len = p.elems_len;
    // ...
}
```

#### C. Update the Setter

Find the corresponding case in `makeExprNode`/`makeStatementNode`/etc.:

```zig
// OLD
.e_list => |e| {
    const extra_start = store.extra_data.len();
    _ = try store.extra_data.append(store.gpa, e.elems.span.start);
    _ = try store.extra_data.append(store.gpa, e.elems.span.len);
    node.setPayload(.{ .raw = .{
        .data_1 = @intCast(extra_start),
        .data_2 = 0,
        .data_3 = 0,
    } });
}

// NEW
.e_list => |e| {
    node.setPayload(.{ .expr_list = .{
        .elems_start = e.elems.span.start,
        .elems_len = e.elems.span.len,
        ._unused = 0,
    } });
}
```

#### D. Test

Run the test suite to verify nothing broke:

```bash
zig build test
```

### 3. Iterative Process

- Complete one node type
- Verify tests pass
- Commit changes
- Move to the next node type

---

## Testing Strategy

### Running Tests

```bash
# Run all tests
zig build test

# Run specific module tests
zig build test -- --test-filter "can"      # Canonicalize tests
zig build test -- --test-filter "parse"    # Parse tests

# Run snapshot tests (validate compiler output)
zig build snapshot -- <file_path>
zig build snapshot && zig build test
```

### Understanding Test Failures

If tests fail after your changes:

1. **Check the error message** - Usually points to the exact location
2. **Verify the payload struct** - Ensure it has all needed fields
3. **Check the getter/setter symmetry** - What you write must match what you read
4. **Look at similar node types** - Find one that's already migrated and follow the pattern
5. **Use git diff** - Compare your changes to the original to spot mistakes

---

## Code Structure Reference

### Key Files

- **`src/canonicalize/Node.zig`** - Defines canonical AST node structure and payloads
- **`src/canonicalize/NodeStore.zig`** - Implements node storage and getter/setter functions
- **`src/canonicalize/CIR.zig`** - Canonical Intermediate Representation (types used in nodes)
- **`src/parse/Node.zig`** - Defines parse tree node structure
- **`src/parse/NodeStore.zig`** - Implements parse node storage

### Important Constants

```zig
const OPTIONAL_VALUE_OFFSET: u32 = 1;  // For encoding optional values
const root_node_idx: Node.Idx = undefined;  // Root of the tree
```

### Common Patterns

**Reading a span from a node:**
```zig
const span_start = node.data_1;
const span_len = node.data_2;
const items = store.expr_span_data.items[span_start..][0..span_len];
```

**Packing optional fields:**
```zig
// Store: has_value ? (value + OFFSET) : 0
node.main_token = if (opt_value) |val| @intFromEnum(val) + OFFSET else 0;

// Retrieve: 0 = null, otherwise value - OFFSET
const val = if (node.main_token == 0) null else @enumFromInt(node.main_token - OFFSET);
```

---

## Common Issues & Solutions

### Issue: "Unused field in struct"

**Cause**: You have an `_unusedN: u32` but the payload union must be exactly 12 bytes (3 × u32).

**Solution**: Ensure every struct variant has exactly 3 u32 fields total.

### Issue: "extra_data index out of bounds"

**Cause**: The getter is reading from the wrong offset or reading more data than was written.

**Solution**: Verify the setter and getter are symmetric - same number of appends and reads, same order.

### Issue: Tests fail with type mismatch

**Cause**: The payload struct field types don't match how they're being cast.

**Solution**: Check the cast operations (@intFromEnum, @enumFromInt, @bitCast) are correct.

### Issue: "Index calculation incorrect"

**Cause**: Manual offset calculations in extra_data are wrong.

**Solution**: Replace all offset calculations with direct field access using the payload struct.

---

## Next Steps for New Contributors

1. **Read this document** - You've done that! ✓

2. **Understand the current state**:
   ```bash
   git log --oneline -10
   # See the migration history
   
   grep -n "extra_data" src/canonicalize/NodeStore.zig | wc -l
   # See how many references remain (~324)
   ```

3. **Pick a simple node type** from the remaining work

4. **Follow the pattern** in "How to Approach the Work" section above

5. **Reference existing migrations**:
   - Look at the git history to see which node types have been migrated
   - Study those as examples
   - The statement_import case is a good reference (it's been partially done)

6. **Commit in small batches**:
   ```bash
   git add src/canonicalize/Node.zig src/canonicalize/NodeStore.zig
   git commit -m "Migrate [NodeType] to typed payload"
   ```

7. **Run tests after each change**:
   ```bash
   zig build test
   ```

---

## Long-term Goal

Once complete, the benefits will include:

- ✓ Type-safe node access
- ✓ Self-documenting code
- ✓ Easier refactoring
- ✓ Fewer runtime errors
- ✓ Better compiler error messages
- ✓ Complete removal of `extra_data` buffer

The migration represents a significant quality-of-life improvement for future compiler development.

---

## References & Resources

- **Zig Language**: https://ziglang.org/documentation/
- **Roc Repository**: https://github.com/roc-lang/roc
- **AGENT.md** in this repo - Project conventions and build commands
- **Git Log**: `git log --oneline remove-extra-data` - See all commits on this branch

---

## Questions & Debugging

If you get stuck:

1. **Check git blame**: See who last modified that function and find related commits
2. **Look at git log**: `git log -p src/canonicalize/NodeStore.zig` to see how similar types were migrated
3. **Compare patterns**: Find a node type that's already migrated and follow its exact structure
4. **Use git diff**: Compare your work against origin/main to spot differences
5. **Run specific tests**: `zig build test -- --test-filter "specific_case"` to isolate failures

Good luck! This is important infrastructure work that will benefit the entire Roc compiler project.
