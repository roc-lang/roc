# Data Field Migration: Complete Elimination Required

## Quick Status

```
$ grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
211
```

**Success criteria**: 0

**Current state**: 211 references remain. Migration is NOT complete.

---

## Binary Success/Failure

This is a **binary** project:
- **COMPLETE** = `grep` returns **0**
- **INCOMPLETE** = `grep` returns anything else (even 1)

There is no "good enough" or "mostly done". Either all references are gone, or the migration failed.

---

## Why This Matters

Currently, accessing node data looks like:
```zig
const pattern = @enumFromInt(node.data_1);      // What is data_1?
const expr = @enumFromInt(node.data_2);          // What is data_2?
const annotation = if (node.data_3 == 0) null   // What is data_3?
    else @enumFromInt(node.data_3);
```

After migration, it will look like:
```zig
const payload = node.getPayload().def;
const pattern = payload.pattern;                 // Clear
const expr = payload.expr;                       // Clear
const annotation = payload.annotation;           // Clear
```

This eliminates the cognitive burden of matching field numbers to meanings.

---

## Recommended Approach

### 1. Category: Problem/Diagnostic Nodes (~80 references)

All problem_* node variants need conversion:

**Current pattern** (example):
```zig
// In addProblem()
node.data_1 = @bitCast(ident);
node.data_2 = region.start.offset;
node.data_3 = region.end.offset;

// In getProblem()
const ident = @bitCast(p.data_1);
const region = .{
    .start = .{ .offset = p.data_2 },
    .end = .{ .offset = p.data_3 },
};
```

**After migration**:
```zig
pub const ProblemFeatureNotFound = extern struct {
    ident: u32,       // Ident.Idx
    start_offset: u32,
    end_offset: u32,
};

// In addProblem()
node.setPayload(.{ .problem_feature_not_found = .{
    .ident = @bitCast(ident),
    .start_offset = region.start.offset,
    .end_offset = region.end.offset,
} });

// In getProblem()
const payload = p.getPayload().problem_feature_not_found;
const ident = @bitCast(payload.ident);
const region = .{
    .start = .{ .offset = payload.start_offset },
    .end = .{ .offset = payload.end_offset },
};
```

### 2. Category: Statement Nodes (~40 references)

Similar pattern as above - identify all statement_* variants and convert each.

### 3. Category: Type Annotation Nodes (~20 references)

Type headers, annotation fields, etc.

### 4. Category: Expression/Pattern Nodes (~30 references)

Record fields, pattern captures, if branches, etc.

### 5. Category: Module/Exposure Nodes (~20 references)

Module-level declarations and exposures.

---

## How to Verify Progress

After each batch of migrations, run:
```bash
grep -rn "\.data_1\|\.data_2\|\.data_3" src/canonicalize/ src/eval/ --include="*.zig" | wc -l
```

Track progress:
- Start: 211
- Goal: 0

---

## Completion Checklist

After completing all migrations:

- [ ] `grep -rn "\.data_1\|\.data_2\|\.data_3"` returns 0
- [ ] All tests still pass: `zig build test`
- [ ] Memory usage unchanged (verified by comparing sizeof)
- [ ] No new code reading `node.data_1/2/3` directly
- [ ] All node creation uses `node.setPayload()`
- [ ] All node reading uses `node.getPayload()`

---

## Document References

- `MIGRATION_PLAN.md` - Detailed migration instructions
- `MIGRATION_UPDATE.md` - Session progress summary
- `Node.zig` - Payload union definitions
- `NodeStore.zig` - Node creation and reading

---

## Final Note

**This is not a "nice to have" or "technical debt cleanup".**

The migration is part of the project requirements. The project is not done until `grep` returns 0. Every single reference must be eliminated through proper typed payload structures.
