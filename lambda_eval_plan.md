# Roc Lambda Evaluation: Implementation Plan

## 1. Overview

The core lambda evaluation system is a **Work In Progress**. It successfully handles single and multi-parameter functions with a production-quality calling convention, robust memory management, and a comprehensive test suite. All core lambda tests are passing.

**Current Status**: Multi-parameter lambdas work perfectly: `(|x, y| x + y)(3, 4)` → `7`. **Unary operations are now complete**, including complex expressions like `(|x| if x > 0 x else -x)(-5)` → `5`. The system is ready for advanced lambda features.

This document provides the necessary context for implementing advanced lambda features like curried functions and closures.

## 2. Key Architectural Concepts

Understanding these two systems is critical for working on the interpreter.

### A. The 6-Phase Calling Convention

Function calls are executed in a precise, six-phase sequence. This convention uses a **landing pad** for the return value, ensuring memory safety and simplifying result handling.

1.  **`alloc_return_space`**: Allocate space for the return value on the stack.
2.  **`eval_expr`**: Evaluate the function and its arguments.
3.  **`call_function`**: Orchestrate the call.
4.  **`bind_parameters`**: Bind arguments to the function's parameter patterns.
5.  **`eval_function_body`**: Execute the lambda's body.
6.  **`copy_result_to_return_space`**: Copy the result to the landing pad.
7.  **`cleanup_function`**: Deallocate the call frame, leaving only the return value.

### B. Structured Debugging System

A powerful debugging system is built into the interpreter, controlled by `comptime` flags. It provides detailed tracing with zero performance overhead in release builds.

-   **Features**: Trace stack operations, calling convention phases, parameter binding, and inspect memory layouts.
-   **Usage**: Enable flags in `src/eval/debug.zig` or `zig build test -Dtrace-eval` to get detailed logs that can quickly identify issues like memory corruption or incorrect stack calculations -- note provides a lot of detail, use shell helpers like `grep` to filter logs.

### C. Critical Implementation Insights

**Memory Management**: The interpreter uses stack-based allocation exclusively. The `completeUnaryMinus` function follows the same pattern as `completeBinop` - it modifies the operand in-place and maintains the layout stack.

#### Final Implementation Status

**Debugging Commands**:
- `zig build test -Dtrace-eval` - Enable structured debugging
- `zig build snapshot -- <file.md>` - Generate compilation stage snapshots

## Next Phase Roadmap

With unary operations complete, development can now proceed to advanced lambda features.

### Phase 11: ✅ COMPLETE - Unary Operations

**Status**: ✅ COMPLETE - All unary operations work perfectly!
- ✅ `(|x| -x)(5)` → `-5` (basic unary minus)
- ✅ `(|x| if x > 0 x else -x)(-5)` → `5` (complex conditionals with unary minus)
- ✅ `(|x| -(-x))(5)` → `5` (double negation)
- ✅ Negative literal arguments: `(|x| x + 1)(-5)` → `-4`

### Phase 12: Curried Lambda Support

-   **Description**: Enable lambdas to return other lambdas, allowing for partial application.
-   **Target**: `((|x| |y| x + y)(3))(4)` → `7`.
-   **Challenge**: Requires nested closure evaluation and careful memory management for the lambda chain.

### Phase 13: Variable Capture (True Closures)

-   **Description**: Allow lambdas to capture and use variables from their enclosing scope.
-   **Implementation Sketch**:
    1.  Extend `SimpleClosure` to a `FullClosure` that can store captured variables.
        ```zig
        const FullClosure = struct {
            body_expr_idx: CIR.Expr.Idx,
            args_pattern_span: CIR.Pattern.Span,
            captured_env: std.ArrayList(CapturedVariable), // New!
        };
        ```
    2.  Implement capture detection during lambda creation.
    3.  Enhance the `.e_lookup_local` logic to search the captured environment after checking parameter bindings.
-   **Target**: `{ x = 10; f = |y| x + y; f(5) }` → `15`.

### Phase 14: Recursive Functions

-   **Description**: Implement support for self-referential functions. This will require a mechanism for a function to bind itself by name within its own body.
-   **Challenge**: Must include tail-call optimization to prevent stack overflow in common recursive patterns.

## What Works So Far

The lambda evaluation system has achieved results in several key areas:

### ✅ Core Lambda System (100% Complete)
- **Single-parameter functions**: `(|x| x + 1)(5)` → `6`
- **Multi-parameter functions**: `(|x, y| x + y)(3, 4)` → `7`
- **Complex arithmetic**: `(|x| x * 2 + 1)(10)` → `21`
- **Parameter reuse**: `(|x| x + x)(7)` → `14`

### ✅ Production Calling Convention
- **6-phase execution**: Allocation → Evaluation → Orchestration → Binding → Execution → Cleanup
- **Landing pad pattern**: Return values positioned at stack base for simple extraction
- **Memory safety**: Zero heap allocation, automatic stack cleanup, no memory leaks
- **Parameter binding**: Robust argument-to-parameter mapping with arity checking

### ✅ If-Expressions (Conditional Logic)
- **Simple conditionals**: `(|x| if x > 0 x else 0)(5)` → `5`
- **All comparisons**: `>`, `<`, `>=`, `<=`, `==`, `!=` work well
- **Complex conditions**: `(|x| if x == 0 1 else x)(42)` → `42`
- **Nested conditionals**: Full support for if-else chains

### ✅ Unary Operations (NEW!)
- **Basic unary minus**: `(|x| -x)(5)` → `-5`
- **Double negation**: `(|x| -(-x))(5)` → `5`
- **Complex expressions**: `(|x| if x > 0 x else -x)(-5)` → `5`
- **Negative arguments**: `(|x| x + 1)(-5)` → `-4`

### ✅ Binary Operations
- **Arithmetic**: `+`, `-`, `*`, `/` with proper precedence
- **Comparisons**: All comparison operators with boolean results
- **Full integration**: Works with unary operations

### ✅ Debugging Infrastructure
- **Structured tracing**: `zig build test -Dtrace-eval` provides detailed execution logs
- **Memory inspection**: Visual stack layouts, corruption detection, state snapshots
- **Zero overhead**: Comptime-controlled with no release performance impact
- **Problem isolation**: Can identify complex issues in minutes instead of hours

## Quick Reference

### Testing Commands
```bash
# Run all tests
zig build test

# Enable debug tracing
zig build test -Dtrace-eval

# Test specific functionality
zig build test | grep -A5 "lambda"

# Debug compilation issues
zig build test 2>&1 | grep "error:" | head -10
```

### Key Files for Expression Implementation
- `src/check/canonicalize/Expression.zig` - Add CIR expression type
- `src/check/canonicalize.zig` - Add canonicalization logic
- `src/eval/interpreter.zig` - Add evaluation logic
- `src/check/check_types.zig` - Add type checking logic
- `src/check/canonicalize/test/node_store_test.zig` - Add test coverage

### Current System Capabilities
**Function Calls**: ✅ Complete
**If-Expressions**: ✅ Complete
**Binary Operations**: ✅ Complete
**Unary Operations**: ✅ Complete
**Variable Capture**: ❌ Not started (next priority)
**Recursion**: ❌ Not started
