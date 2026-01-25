# DevEvaluator Dec (Fixed-Point Decimal) Bugs

## Overview

The DevEvaluator is truncating `Dec` (fixed-point decimal) values to integers, losing all fractional information. This affects 5 arithmetic operations: negate, plus, minus, times, and div_by.

## Background: How Dec Works

`Dec` is a fixed-point decimal type stored as `i128` scaled by 10^18. For example:
- `3.14dec` is stored as `3140000000000000000` (3.14 × 10^18)
- `0.5dec` is stored as `500000000000000000` (0.5 × 10^18)

The Interpreter correctly preserves and displays these values with their fractional parts. The DevEvaluator is truncating them.

## How to Reproduce

Run the eval tests:
```bash
zig build test-eval
```

The 5 failing tests are in `src/eval/test/arithmetic_comprehensive_test.zig`.

## Bug Details

### 1. Dec: negate

**Test code:**
```roc
{
    a : Dec
    a = 3.14dec
    -a
}
```

**Expected:** `-3.14`
**DevEvaluator returns:** `-3`

The negation operation is working (sign is correct), but the fractional part `.14` is lost.

---

### 2. Dec: plus

**Test code:**
```roc
{
    a : Dec
    a = 3.14159dec
    b : Dec
    b = 2.71828dec
    a + b
}
```

**Expected:** `5.85987`
**DevEvaluator returns:** `5`

The addition is computed but the fractional part `.85987` is truncated.

---

### 3. Dec: minus

**Test code:**
```roc
{
    a : Dec
    a = 10.0dec
    b : Dec
    b = 3.5dec
    a - b
}
```

**Expected:** `6.5`
**DevEvaluator returns:** `6`

The subtraction is computed but the fractional part `.5` is lost.

---

### 4. Dec: times

**Test code:**
```roc
{
    a : Dec
    a = -3.0dec
    b : Dec
    b = 2.5dec
    a * b
}
```

**Expected:** `-7.5`
**DevEvaluator returns:** `-7`

The multiplication is computed but the fractional part `.5` is lost.

---

### 5. Dec: div_by

**Test code:**
```roc
{
    a : Dec
    a = 1.0dec
    b : Dec
    b = 3.0dec
    a / b
}
```

**Expected:** `0.333333333333333333`
**DevEvaluator returns:** `0`

The division result `0.333...` is truncated to `0` (integer truncation toward zero).

## Root Cause Analysis

The pattern is consistent: the DevEvaluator appears to be treating the i128 result as an integer rather than as a scaled fixed-point value. When formatting the result for display:

- The i128 value (e.g., `6500000000000000000` for 6.5) should be divided by 10^18 to get the decimal representation
- Instead, it appears to be doing integer division or casting, producing just `6`

The bug is likely in one of these areas:
1. Result formatting/rendering code that handles Dec output
2. Type confusion where Dec is being treated as a regular integer

## Expected Behavior

Dec values should be formatted by dividing the i128 by 10^18 and displaying the result with appropriate decimal places. The LLVM evaluator and Interpreter both handle this correctly - only the DevEvaluator has this bug.

## Files to Investigate

- `src/eval/dev_evaluator.zig` - Main DevEvaluator implementation
- `src/eval/render_helpers.zig` - Result rendering (check Dec formatting path for DevEvaluator)
- `src/eval/test/helpers.zig` - Test harness (see how DevEvaluator results are compared)
