# Roc Check Tests

This directory contains test files for the `roc check` command. The tests are organized by the type of errors or features they test.

## Directory Structure

### `parse_errors/`
Tests for parsing errors, including:
- `missing_closing_paren.roc` - Syntax error with unclosed parenthesis
- `missing_header.roc` - File without required module/app header

### `canon_errors/`
Tests for canonicalization errors, including:
- `undefined_variable.roc` - Reference to undefined variable (no module header)
- `undefined_variable_with_module.roc` - Reference to undefined variable in proper module

### `type_errors/`
Tests for type checking errors, including:
- `multiple_type_errors.roc` - App with multiple type errors (type mismatches, undefined variables)
- `heterogeneous_lists.roc` - Nested lists with inconsistent types

### `valid/`
Valid Roc files for testing successful compilation:
- `simple_module.roc` - Basic module exposing a single value
- `module_with_function.roc` - Module with an exposed function

### `features/`
Tests for specific language features:
- `pattern_matching.roc` - Tests pattern matching (currently triggers "not implemented" error)

## Usage

These test files can be used with `roc check` to verify error reporting and successful compilation:

```bash
# Test parse error handling
roc check tests/check_tests/parse_errors/missing_header.roc

# Test with caching disabled
roc check tests/check_tests/valid/simple_module.roc --no-cache

# Test with verbose output
roc check tests/check_tests/valid/simple_module.roc --verbose
```

## Adding New Tests

When adding new test files:
1. Place them in the appropriate subdirectory based on what they test
2. Use descriptive filenames that indicate what is being tested
3. Add a comment at the top of the file explaining the test case
4. Update this README if adding a new category of tests