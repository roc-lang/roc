# Bug Fix Plan for Roc Compiler Issues

This document provides a detailed plan for fixing the remaining bugs found during testing.

## Status Summary

| Bug # | Description | Status | Difficulty |
|-------|-------------|--------|------------|
| 1 | Type annotations on helper functions panic | FIXED | - |
| 2 | Tuple pattern matching panic | FIXED | - |
| 3 | Custom type aliases panic | FIXED | - |
| 4 | `?` operator not implemented | **BUG** | Medium |
| 5-8 | Missing builtins (Num.to_str, List.range, etc.) | Feature Request | - |
| 9 | Numeric fold produces garbage values | **BUG** | Hard |
| 10 | For loop on list literals segfault | FIXED | - |
| 11 | List.first error handling crash | FIXED | - |
| 12 | String literal matching in match broken | FIXED | Medium |
| 13-14 | Args and memory leaks | Platform-specific | - |

## Bugs Requiring Fixes

---

## Bug 4: `?` Operator Not Implemented

### Reproduction
```roc
main! = || {
    first = List.first(["hello"])?
    Stdout.line!(first)
}
```

**Error:** `NOT IMPLEMENTED: canonicalize suffix_single_question expression`

### Root Cause Location
- **File:** `/Users/rtfeldman/code/roc5/src/canonicalize/Can.zig`
- **Lines:** 5073-5079

```zig
.suffix_single_question => |_| {
    const feature = try self.env.insertString("canonicalize suffix_single_question expression");
    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
        .feature = feature,
        .region = Region.zero(),
    } });
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
},
```

### How Similar Operators Work

The `!` (bang) operator is handled at lines 5097-5107:
```zig
.OpBang => {
    const can_operand = (try self.canonicalizeExpr(unary.expr)) orelse return null;
    const expr_idx = try self.env.addExpr(Expr{
        .e_unary_not = Expr.UnaryNot.init(can_operand.idx),
    }, region);
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars };
},
```

### Fix Plan

1. **Desugar `expr?` to a match expression:**
   ```roc
   match expr {
       Ok(value) => value,
       Err(e) => return Err(e),
   }
   ```

2. **Implementation steps:**
   - Canonicalize the inner expression
   - Create a match expression with two branches:
     - `Ok(value)` pattern → extract and return the value
     - `Err(e)` pattern → early return with the error
   - Handle free variables from the inner expression
   - Use existing `e_match` and `e_return` expression types

3. **Files to modify:**
   - `src/canonicalize/Can.zig` (main implementation)
   - May need to add helper functions for generating the match pattern

### Test File
- `/Users/rtfeldman/code/roc5/test/fx/bug_04_question_mark_not_implemented.roc`

---

## Bug 9: Numeric Fold Produces Incorrect Values

### Reproduction
```roc
main! = || {
    sum = [1, 2, 3, 4, 5].fold(0, |acc, n| acc + n)
    Stdout.line!("Sum: ${I64.to_str(sum)}")
}
```

**Expected:** `Sum: 15`
**Actual:** `Sum: -3446744073709551616` (garbage value)

### Root Cause Location
- **File:** `/Users/rtfeldman/code/roc5/src/eval/interpreter.zig`
- **Lines:** 10780-10857 (`for_loop_body_done` handler)

### How fold Works (Builtin Definition)
From `/Users/rtfeldman/code/roc5/src/build/roc/Builtin.roc:97-106`:
```roc
fold : List(item), state, (state, item -> state) -> state
fold = |list, init, step| {
    var $state = init
    for item in list {
        $state = step($state, item)
    }
    $state
}
```

### Root Cause Analysis

The `for_loop_body_done` continuation handler has a binding cleanup issue:

1. **Line 10786:** `body_result.decref(&self.runtime_layout_store, roc_ops);`
   - Immediately discards the loop body result without preserving mutable variable updates

2. **Line 10789:** `self.trimBindingList(&self.bindings, fl.loop_bindings_start, roc_ops);`
   - Trims bindings back to the loop scope start
   - **BUG:** This removes/corrupts the mutable variable binding (`$state`) that tracks the accumulator

The execution flow:
1. Before loop: `$state` binding created with value 0
2. Iteration 1: `$state = step($state, 1)` → binding updated to 1
3. `for_loop_body_done` called, binding is trimmed away
4. Iteration 2: Tries to read `$state` but binding is corrupted/missing
5. Returns uninitialized memory (garbage)

### Fix Plan

1. **Track mutable variable bindings separately:**
   - Mutable variables (`var $x`) should not be trimmed by iteration scope cleanup
   - Either mark bindings as "mutable" or track their indices separately

2. **Implementation options:**
   - **Option A:** Add a `is_mutable` flag to bindings, skip trimming mutable bindings
   - **Option B:** Track `mutable_bindings_start` separately from `loop_bindings_start`
   - **Option C:** Check if a binding was updated via reassignment before trimming

3. **Files to modify:**
   - `src/eval/interpreter.zig`
     - Line 171-178: Binding struct (add mutable flag)
     - Line 5015-5027: `trimBindingList` function (skip mutable bindings)
     - Line 10780-10857: `for_loop_body_done` handler (preserve mutable bindings)
     - Line 10937-10963: Reassignment handling (mark binding as mutable)

4. **Key code locations:**
   - Binding creation: lines 10825 (`new_loop_bindings_start`)
   - Binding trimming: line 10789 (`trimBindingList`)
   - Reassignment: lines 10947-10950 (where binding value is updated)

### Test File
- `/Users/rtfeldman/code/roc5/test/fx/bug_09_numeric_fold_wrong.roc`

---

## Bug 12: String Literal Matching Falls Through to Wildcard

### Reproduction
```roc
greet = |name| {
    match name {
        "Alice" => "Hello Alice!"
        "Bob" => "Hey Bob!"
        _ => "Hello stranger!"
    }
}
```

**Expected:** Returns correct greeting for "Alice" and "Bob"
**Actual:** Always returns "Hello stranger!" (wildcard case)

### Root Cause Location
- **File:** `/Users/rtfeldman/code/roc5/src/eval/interpreter.zig`
- **Lines:** 5077-5082 (string pattern matching)

```zig
.str_literal => |sl| {
    if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .str)) return false;
    const lit = self.env.getString(sl.literal);
    const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
    return rs.eqlSlice(lit);
},
```

### Potential Root Causes

1. **Layout type check failing:** The condition at line 5078 might incorrectly reject valid string values if the type system reports a different layout than expected

2. **String literal retrieval issue:** `self.env.getString(sl.literal)` might return an incorrect or empty string

3. **RocStr pointer dereferencing issue:** The `value.ptr` might not be correctly aligned or dereferenced

4. **eqlSlice comparison issue:** The string comparison function might have a subtle bug

### Fix Plan

1. **Debug and diagnose:**
   - Add logging to verify which checks are failing
   - Check if layout detection correctly identifies string types
   - Verify string literal is correctly retrieved from environment

2. **Investigation steps:**
   - Print `value.layout.tag` and `value.layout.data.scalar.tag` values
   - Print the retrieved literal string
   - Print the RocStr contents being compared

3. **Likely fix locations:**
   - Layout type detection (line 5078)
   - String retrieval (line 5079)
   - Pointer alignment/casting (line 5080)

4. **Files to modify:**
   - `src/eval/interpreter.zig` (lines 5077-5082)
   - Potentially pattern canonicalization in `src/canonicalize/Can.zig`

### Test File
- `/Users/rtfeldman/code/roc5/test/fx/bug_12_string_match_broken.roc`

---

## Implementation Order

**Recommended order:**

1. **Bug 12 (String match)** - Likely a simple fix, good for understanding the codebase
2. **Bug 9 (Numeric fold)** - More complex, requires careful binding management
3. **Bug 4 (`?` operator)** - Feature implementation, requires understanding desugaring

---

## Test Commands

Run all bug reproduction tests:
```bash
zig build test -Dcli-tests=true --summary all 2>&1 | grep -A5 "fx_platform_test"
```

Run a single reproduction manually:
```bash
./zig-out/bin/roc test/fx/bug_04_question_mark_not_implemented.roc
./zig-out/bin/roc test/fx/bug_09_numeric_fold_wrong.roc
./zig-out/bin/roc test/fx/bug_12_string_match_broken.roc
```

---

## Learnings and Updates

*(This section will be updated as fixes are implemented)*

### Bug 12 Investigation Notes (FIXED)

**Root Cause:** The string pattern canonicalization was using the wrong token to extract the string literal content.

**Details:**
- String patterns in the AST have two fields: `string_tok` (the StringStart token position) and `expr` (the parsed string expression)
- The old code at `Can.zig:6348` tried to extract content from `string_tok` directly, but `StringStart` is just a delimiter token (the opening quote), not the actual string content
- String tokenization works as: `StringStart` → `StringPart` (actual content) → `StringEnd`
- The actual string content lives in the `expr` field as a parsed string expression with `string_part` tokens

**Fix Location:** `src/canonicalize/Can.zig` lines 6348-6407

**Fix Implementation:**
1. Get the string expression from `e.expr` instead of `e.string_tok`
2. Extract parts from the string expression's parts list
3. For simple string literals, get the `string_part` token from the first (and only) part
4. Resolve that token to get the actual string content
5. Process escape sequences and create the `str_literal` pattern

**Code Change:**
```zig
.string => |e| {
    const str_expr = self.parse_ir.store.getExpr(e.expr);
    switch (str_expr) {
        .string => |se| {
            const parts = self.parse_ir.store.exprSlice(se.parts);
            if (parts.len == 1) {
                const part = self.parse_ir.store.getExpr(parts[0]);
                switch (part) {
                    .string_part => |sp| {
                        const part_text = self.parse_ir.resolve(sp.token);
                        // ... create str_literal pattern
                    },
                    else => {},
                }
            }
        },
        else => {},
    }
},
```

**Key Insight:** The interpreter's `str_literal` pattern matching code was correct all along - the bug was that canonicalization was storing empty/wrong strings in the pattern, so the comparison always failed and fell through to the wildcard case

### Bug 9 Investigation Notes
- TBD

### Bug 4 Implementation Notes
- TBD
