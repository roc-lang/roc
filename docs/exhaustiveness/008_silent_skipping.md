# Issue: Silent Skipping of Exhaustiveness Checking

## Problem Statement

Multiple error conditions in the exhaustiveness checker cause checking to be **silently skipped**. When this happens, users receive no feedback that exhaustiveness checking was not performed, potentially hiding non-exhaustive matches.

## Current Behavior (Problematic)

When any of these conditions occur, exhaustiveness checking is skipped without notification:

1. **Polymorphic types** (flex/rigid in patterns)
2. **Tag not found in union**
3. **Type is not a union**
4. **Error types in column types**
5. **Arity mismatch** (especially for records)
6. **Field not found in record**

The `error.TypeError` is caught and the match is considered "okay" with no further checking.

## Location in Codebase

**File:** `src/check/Check.zig`

**Line ~4625-4632:**
```zig
const result = exhaustive.checkMatch(...) catch |err| switch (err) {
    error.OutOfMemory => return error.OutOfMemory,
    error.TypeError => {
        // Type error in pattern - exhaustiveness checking can't proceed
        // This is expected when there are polymorphic types or type mismatches
        // Don't report exhaustiveness errors in this case
        return does_fx;  // <-- SILENT RETURN
    },
};
```

**File:** `src/check/exhaustive.zig**

Multiple locations return `error.TypeError`:
- Line ~731: Polymorphic arg in known_ctor
- Line ~752: Tag not found in union
- Line ~771: Polymorphic arg in ctor
- Line ~785: Not a union type
- Line ~1293: Error type in specializeByConstructor
- Line ~1304: Arity mismatch
- Line ~1333: Error type in specializeByRecordPattern
- Line ~1340: Field not found in record

## Expected Behavior (Correct)

When exhaustiveness checking cannot be performed, users should be informed:

### Option A: Warning Message
```
Warning: Exhaustiveness checking skipped for this match expression
Reason: The condition type contains unresolved type variables
```

### Option B: Defer and Retry
If the type might be resolved later, defer checking until it is.

### Option C: Conservative Error
If we can't check, assume it might be non-exhaustive and require a wildcard:
```
Error: Cannot verify exhaustiveness - consider adding a wildcard pattern
```

## Why Silent Skipping Is Bad

1. **False sense of security:** Users think their code is checked when it isn't
2. **Hidden bugs:** Non-exhaustive matches slip through
3. **Inconsistent behavior:** Some matches are checked, others aren't
4. **Debugging difficulty:** When a match fails at runtime, users don't know why the compiler didn't catch it

## Solution Requirements

1. **Track skip reasons:** When TypeError occurs, record WHY checking was skipped
2. **Report to users:** Add a warning or info message explaining the skip
3. **Categorize by severity:**
   - Type errors in code → don't double-report (already have type error)
   - Polymorphic types → informational (design limitation)
   - Internal issues → should be bugs to fix
4. **No silent returns:** Every skip should either produce output or be a known, documented case

## Implementation Approach

### Step 1: Create Skip Reason Enum

```zig
pub const CheckSkipReason = enum {
    polymorphic_type,
    type_contains_errors,
    unresolved_tag,
    record_field_mismatch,
    internal_type_error,
};

pub const CheckResult = union(enum) {
    success: struct {
        is_exhaustive: bool,
        missing_patterns: []const Pattern,
        redundant_indices: []const u32,
    },
    skipped: CheckSkipReason,
};
```

### Step 2: Return Skip Reason Instead of TypeError

Instead of:
```zig
return error.TypeError;
```

Use:
```zig
return .{ .skipped = .polymorphic_type };
```

### Step 3: Report Skips in Check.zig

```zig
const result = exhaustive.checkMatch(...);
switch (result) {
    .success => |s| {
        // Handle exhaustiveness/redundancy as before
    },
    .skipped => |reason| {
        // Report warning if appropriate
        if (reason != .type_contains_errors) {  // Don't double-report type errors
            _ = try self.problems.appendProblem(self.cir.gpa, .{
                .exhaustiveness_check_skipped = .{
                    .match_expr = expr_idx,
                    .reason = reason,
                },
            });
        }
    },
}
```

## Categories of Skip Reasons

### Should Produce Warning
- Polymorphic types (user should know checking was limited)
- Unresolved type variables

### Should NOT Produce Warning (Type Error Already Reported)
- Type contains `.err` (there's already a type error)
- Tag not found (there's already a type mismatch error)
- Field not found (there's already a record error)

### Should Be Fixed (Not Skipped)
- Record arity mismatch → Issue 007
- Other internal issues → Should be bugs

## Functions to Modify

1. `checkMatch()` (line ~3057): Return `CheckResult` instead of `error.TypeError`
2. `reifyPattern()` (line ~700): Return skip reason instead of error
3. `specializeByConstructor()` (line ~1293): Return skip reason
4. `specializeByRecordPattern()` (line ~1328): Return skip reason
5. `Check.zig` `checkMatchExpr()`: Handle skip reasons appropriately

## Testing

Create test cases that verify:
1. Polymorphic type match produces skip warning
2. Type error in code doesn't produce duplicate warning
3. Skip reason is correctly identified
4. Warning message is clear and helpful

## Acceptance Criteria

- [ ] No silent `return does_fx` after catching TypeError
- [ ] All skip reasons are categorized and documented
- [ ] Appropriate warnings are shown to users
- [ ] Type errors don't cause duplicate warnings
- [ ] Users understand when/why checking was skipped
- [ ] No "Don't report exhaustiveness errors in this case" without explanation to user
