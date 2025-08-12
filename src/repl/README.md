# REPL Module

This directory contains the Read-Eval-Print Loop (REPL) implementation for the Roc compiler.

## Overview

The REPL provides an interactive environment for evaluating Roc expressions and maintaining state across multiple evaluations. It uses the full Roc compiler pipeline including parsing, canonicalization, type checking, and interpretation.

### Features

- **Expression Evaluation**: Evaluates simple expressions and literals through the full compiler pipeline
- **State Management**: Tracks all past definitions in order, allowing redefinition/shadowing
- **Special Commands**: Supports `:help`, `:reset`, and `:exit` commands
- **Memory Safety**: Proper memory management with no leaks or segfaults

### Current Capabilities

The REPL currently supports:
- Numeric literals: `42` → `42`
- Simple assignments: `x = 5` → `5` (evaluates and shows the RHS value)
- Special commands for help and state management

### Limitations

Context-aware evaluation is not yet implemented. Expressions that reference past definitions will return `<needs context>`. For example:
```
» x = 5
5
» x + 1
<needs context>
```

## Architecture

The REPL maintains:
- A list of past definitions (`PastDef` structs) that store source code and identifiers
- An evaluation stack for the interpreter
- Proper memory management with each definition owning its source string

## Testing

The REPL includes comprehensive tests for:
- Initialization and cleanup
- Special command handling
- Simple expression evaluation
- State management and redefinitions
- Memory management

## Snapshot Testing

REPL snapshots use a special format where:
- Input lines begin with `» `
- Expected outputs are separated by `---` lines
- The REPL maintains state across evaluations within a single snapshot

Example:
```
# SOURCE
» 1 + 1
» x = 42
» "Hello, World!"
# EXPECTED
2
---
42
---
"Hello, World!"
```

## Future Work

The main remaining work is implementing context-aware evaluation so expressions can reference past definitions. This requires solving how to evaluate expressions at the module level in Roc's compilation model.

Once context-aware evaluation is implemented, the REPL will support:
- Variable references across definitions
- Complex expressions with arithmetic operations
- Function definitions and calls
- Pattern matching
- Module imports
