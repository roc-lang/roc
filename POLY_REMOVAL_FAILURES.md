# Poly Removal Test Failures Analysis

**Build Summary:** 1331/1443 tests passing (94 failures, 18 skipped)

## Snapshot Tests
✅ All snapshot tests pass (`zig build snapshot` completes with no errors)

## Critical Failures

### 1. Interpreter Crash (1 failure)
**Test:** `test.eval_test.test.eval simple number`
**Status:** Crash with signal 6 (SIGABRT)

**Description:**
The eval test suite crashes immediately when trying to evaluate a simple number expression.

**Potential Causes:**
- Assertion failure in interpreter when handling nominal number types
- Missing case in interpreter type translation for nominal types
- Layout mismatch between compile-time nominal type and runtime representation

**Investigation Steps:**
1. Run the failing test in isolation to get full stack trace
2. Check if the crash occurs during type translation or during evaluation
3. Verify nominal number type backing var resolution in interpreter
4. Check if the issue is specific to certain number types or all of them

---

## Type Display Mismatches (31 failures)

### 2. Old Type Format Expected (31 failures)
**Tests:** Various type checking integration tests
**Pattern:** Expected old format like `Num(Int(Unsigned64))`, got new format like `U64`

**Examples:**
- `test.cross_module_test.test.cross-module - polymorphic function with multiple uses passes`
  - Expected: `Num(Int(Unsigned64))`
  - Got: `U64`
- `test.builtin_scope_test.test.builtin types are still available without import`
  - Expected: `List(Num(Int(Unsigned64)))`
  - Got: `List(U64)`
- Multiple tests in `type_checking_integration.test.check type - num - *` suite
- Multiple tests in `let_polymorphism_integration_test.test.*` suite

**Root Cause:**
Test expectations still reference the old polymorphic type format. These are hardcoded string comparisons in test code that need to be updated to expect the new nominal type names.

**Fix Strategy:**
1. Update all test expectations from `Num(Int(Unsigned64))` → `U64`
2. Update all test expectations from `Num(Frac(Dec))` → `Dec`
3. Pattern match for similar conversions across all test files
4. Most of these are in:
   - `src/check/test/type_checking_integration.zig`
   - `src/check/test/let_polymorphism_integration_test.zig`
   - `src/check/test/cross_module_test.zig`
   - `src/check/test/builtin_scope_test.zig`

---

## Missing Static Dispatch Support (1 critical failure)

### 3. Missing `plus` Method on Nominal Number Types
**Test:** `test.num_type_requirements_test.test.F32: fits`
**Error:** `**MISSING METHOD** The value before this **+** operator has the type **F32**, which has no **plus** method`

**Description:**
The `+` operator tries to call the `plus` method on `F32` via static dispatch, but the method lookup is failing.

**Investigation Findings:**
1. ✅ `F32.plus` IS defined in Builtin.roc (line 351: `plus : F32, F32 -> F32`)
2. ✅ Static dispatch constraints ARE being created in Check.zig (lines 3587-3626)
3. ❌ The constraint resolution is FAILING to find the method on the nominal type

**Root Cause:**
When the type checker unifies a constrained flex var (requiring a `plus` method) with a nominal type like `F32`, it needs to verify that `F32` actually has a `plus` method in its associated items. This verification is failing - the nominal type's associated items are not being found/checked during constraint resolution.

**Potential Issues:**
1. Nominal type associated items are not being registered/loaded from Builtin.roc
2. The unification code for nominal types doesn't check static dispatch constraints
3. The constraint resolution mechanism doesn't know how to look up methods on nominal types
4. Number types are in a nested module `Num` in Builtin.roc, which might affect lookup

**Next Steps:**
1. Check unify.zig for how it handles StaticDispatchConstraint + nominal type unification
2. Verify that nominal type associated items are being parsed and stored
3. Check if List methods work (to compare working vs non-working nominal types)
4. Investigate if the nested module structure (`Builtin.Num.F32`) is causing issues

**Related Failures:**
This likely affects all 6 num_type_requirements_test failures:
- `U8: 255 fits`
- `U8: 256 does not fit`
- `U8: negative does not fit`
- `I8: -128 fits`
- `I8: -129 does not fit`
- `F32: fits`

---

## Missing Type Error Reporting (25 failures)

### 4. Type Errors Not Being Reported
**Pattern:** Tests expect 1 error, but 0 errors are found
**Count:** 25 failures

**Examples:**
- `test.type_checking_integration.test.check type - list  - diff elems 1`
- `test.type_checking_integration.test.check type - num - cannot coerce 500 to u8`
- `test.type_checking_integration.test.check type - polymorphic function function param should be constrained`
- `test.type_checking_integration.test.check type - if else - invalid condition 1`
- `test.type_checking_integration.test.check type - unary minus mismatch`
- `test.custom_num_type_test.test.Custom number type without from_int_digits: integer literal does not unify`

**Full List of Affected Tests:**
1. `check type - list  - diff elems 1`
2. `check type - num - cannot coerce 500 to u8`
3. `check type - polymorphic function function param should be constrained`
4. `check type - alias with mismatch arg`
5. `check type - nominal recursive type anno mismatch`
6. `check type - nominal recursive type wrong type`
7. `check type - if else - invalid condition 1`
8. `check type - if else - invalid condition 2`
9. `check type - if else - different branch types 1`
10. `check type - if else - different branch types 2`
11. `check type - if else - different branch types 3`
12. `check type - match - diff branch types`
13. `check type - unary minus mismatch`
14. `check type - record - update fail`
15. `check type - expect not bool`
16. `check type - for mismatch`
17. `check type - equirecursive static dispatch - motivating example (current behavior)`
18. `Custom number type without from_int_digits: integer literal does not unify`

**Root Cause:**
Type checking is succeeding when it should fail. This suggests:
1. Type checking for nominal number types is too permissive
2. Unification is not properly checking constraints
3. Error detection logic was removed along with special-cased num types

**Potential Causes:**
- Number literal type checking removed constraint validation
- Unification between flex vars and nominal types always succeeds
- Missing validation for number literal range/sign requirements
- The `.num`, `.int`, `.frac` annotation error cases now just return `.err` without reporting problems

**Investigation Steps:**
1. Check one specific test (e.g., "cannot coerce 500 to u8") to understand what error should be reported
2. Verify if the issue is specific to number types or affects other types
3. Check if unification is returning success when it should fail
4. Look for missing problem reporting in the new nominal type handling code

---

## Number Type Inference Tests (7 failures)

### 5. Number Type Inference Logic Missing
**Tests:** `test.num_type_inference_test.test.*`
**Count:** 7 failures

**Failed Tests:**
1. `infers type for small nums`
2. `infers type for nums with specific requirements`
3. `infers num requirements correctly`
4. `edge case: negative 0`
5. `edge case: positive 0`
6. `infer hexadecimal literals as unbound num`
7. `infer binary literals as unbound num`
8. `infer octal literals as unbound num`

**Description:**
These tests verify that number literal requirements are tracked correctly (e.g., whether a literal fits in U8, requires signed types, etc.).

**Root Cause:**
The number type inference tests were specifically testing the old `num_unbound`, `int_unbound`, `frac_unbound` requirement tracking system. With plain flex vars replacing these types, the requirement tracking infrastructure is no longer in place.

**Potential Resolution Strategies:**

**Option A: Delete These Tests**
These tests may be obsolete if we're moving to a static dispatch + defaulting model without requirement tracking.

**Option B: Reimplement with Constraints**
If literal requirements are still needed, they should be implemented as static dispatch constraints on flex vars.

**Decision Needed:**
Are number literal requirements (fits_in_u8, needs_signed, etc.) still part of the design, or are they being replaced by static dispatch validation?

---

## Summary by Category

| Category | Count | Severity |
|----------|-------|----------|
| Interpreter crashes | 1 | Critical |
| Missing static dispatch | 1 + 5 related | Critical |
| Type display format | 31 | Low (cosmetic) |
| Missing error reporting | 25 | High |
| Num inference tests | 7 | Medium (may be obsolete) |

**Total:** 94 failures

---

## Prioritized Fix Order

### Priority 1: Critical Functionality
1. **Fix interpreter crash** - Blocks all eval tests
2. **Fix static dispatch for nominal number types** - Math operators don't work

### Priority 2: Type Safety
3. **Restore error reporting** - Type errors silently pass (25 tests)

### Priority 3: Test Infrastructure
4. **Update type display expectations** - Tedious but straightforward (31 tests)

### Priority 4: Design Decision Required
5. **Number inference tests** - Need to decide if these are obsolete (7 tests)

---

## Next Steps

1. **Investigate interpreter crash:** Run `zig build test` with filters to isolate the eval crash and get full stack trace
2. **Check static dispatch:** Verify if `F32.plus` and similar methods are defined in Builtin.roc and accessible
3. **Review error reporting:** Examine one failing test (e.g., "500 to u8") to understand what validation was removed
4. **Bulk update test expectations:** Create script or pattern to update old type format → new format across test files
