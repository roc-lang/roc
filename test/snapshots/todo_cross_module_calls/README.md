# TODO: Cross-Module Function Calls in REPL

These tests are temporarily disabled because the interpreter does not yet support cross-module function calls.

## Problem

When evaluating expressions like `Bool.not(True)` in the REPL:

1. The canonicalizer correctly creates an `e_lookup_external` for `Bool.not` pointing to the function definition in the Bool module
2. The interpreter looks up the function and finds it's a lambda (with expression indices from the Bool module)
3. When the lambda is invoked, its body contains expression indices specific to the Bool module
4. The interpreter tries to evaluate these indices using the REPL module's expression store
5. This causes "index out of bounds" errors because the REPL module has far fewer expressions than the Bool module

## Solution

A proper fix requires architectural changes to the interpreter:

- Lambda/closure values need to capture their source module
- When invoking a lambda, the interpreter must evaluate its body in the lambda's source module context
- This likely requires changes to the `Value` type and the evaluation model to track module contexts

## Tests in this directory

### Explicit qualified function calls
- `qualified_bool_not.md` - Tests `Bool.not(True)`
- `qualified_bool_not_false.md` - Tests `Bool.not(False)`
- `qualified_bool_complex.md` - Tests `Bool.not` with a variable

### String operations (use Bool.equals)
- `string_equality_basic.md` - String equality with `==` operator
- `string_edge_cases.md` - Edge cases for string equality
- `string_multiline_comparison.md` - Multiline string comparisons
- `string_interpolation_comparison.md` - String interpolation with comparisons
- `string_ordering_unsupported.md` - String ordering operators (not yet implemented)

### Boolean expressions with qualified tags
- `repl_boolean_expressions.md` - `Bool.True`, `Bool.False`, and boolean operators

### Numeric operations (use comparison functions)
- `nested_ifs.md` - Nested if with `>` operator
- `fibonacci.md` - Recursive function using `<=` operator

These tests should be moved back to `test/snapshots/` once cross-module function calls are implemented in the interpreter.
