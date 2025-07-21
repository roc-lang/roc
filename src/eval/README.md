# Eval

Runtime evaluation and interpretation system for executing Roc programs during compilation.

## Directory Structure

- **[interpreter.zig](./interpreter.zig)**: Core interpreter with stack-based evaluation engine
- **[stack.zig](./stack.zig)**: Stack memory management for runtime evaluation
- **[eval_test.zig](./eval_test.zig)**: Comprehensive test suite for interpreter functionality
- **[docs/](./docs/)**: Additional documentation and debugging guides

## Quick Start

Run all interpreter tests:
```bash
zig build test
```

Run tests with detailed debugging output:
```bash
zig build test -Dtrace-eval
```

## Architecture Notes

The evaluation system uses a stack-based virtual machine with:

- **Work Stack**: Queue of operations to execute
- **Layout Stack**: Type information for stack-allocated values
- **Stack Memory**: Raw memory for storing computed values
- **Parameter Bindings**: Maps pattern variables to stack locations

### Stack Calling Convention

Function calls follow this stack layout:
```
[return_space] [function] [arg1] [arg2] ... [argN]
```

The interpreter maintains this invariant throughout evaluation, with the layout stack tracking type information for each stack slot.
