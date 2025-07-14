# REPL Module

This directory contains the Read-Eval-Print Loop (REPL) implementation for the Roc compiler.

## Overview

The REPL provides an interactive environment for evaluating Roc expressions and maintaining state across multiple evaluations.

## Current Status

The current implementation (`eval.zig`) is a placeholder that provides hardcoded responses for snapshot testing. This allows us to develop and test the snapshot infrastructure for REPL interactions before the full REPL is implemented.

## Snapshot Testing

REPL snapshots use a special format where:
- Input lines begin with `» ` 
- Expected outputs are separated by `---` lines
- The REPL maintains state across evaluations within a single snapshot

Example:
```
# SOURCE
» 1 + 1
» 0.1 + 0.2
» "Hello, World!"
» []
# EXPECTED
2
---
0.3
---
"Hello, World!"
---
[] : List(_size)
```

## Future Work

The placeholder implementation will be replaced with a full REPL that:
- Parses and evaluates Roc expressions
- Maintains a persistent environment across evaluations
- Provides type information for expressions
- Handles errors gracefully
- Supports multi-line input for complex expressions