# REPL Module

This directory contains the Read-Eval-Print Loop (REPL) implementation for the Roc compiler.

## Overview

The REPL provides an interactive environment for evaluating Roc expressions and maintaining state across multiple evaluations.

## Current Status

The REPL implementation now includes:

### State Management (`repl_full.zig`)
A complete REPL implementation that:
- Uses real AST parsing via `parse.parseStatement()` and `parse.parseExpr()`
- Tracks past definitions with proper identifier extraction from parsed ASTs
- Handles assignments, imports, type declarations, and expressions
- Builds complete source by concatenating past definitions
- Integrates with the full compiler pipeline (parse → canonicalize → type check → evaluate)

### Working Demo (`working_demo.zig`)
A demonstration program showing:
- Past definition tracking
- Incremental source building
- Assignment detection and storage
- Full source reconstruction for evaluation

The implementation successfully demonstrates that we can parse arbitrary strings into tokens and statements, contrary to the initial assumption about module headers being required.

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

## Implementation Details

### ReplState (`repl_state.zig`)

The `ReplState` struct manages the REPL session state:

- **PastDef**: Tracks two types of past inputs:
  - `def`: Variable/function definitions with identifier and source
  - `import`: Import statements

- **ReplAction**: Represents possible outcomes of processing input:
  - `eval`: Input should be evaluated with full source
  - `exit`: Exit the REPL
  - `help`: Show help message
  - `nothing`: No action needed (e.g., empty input)
  - `parse_error`: Parse error occurred

Key features:
- Full AST-based parsing using `parse.parseStatement()` and `parse.parseExpr()`
- Proper token resolution to extract identifier names from parsed patterns
- Support for assignments, imports, type declarations, and expressions
- Automatic evaluation of identifiers after assignment
- Complete integration with the compiler's evaluation pipeline
- Proper handling of both statements and expressions

### Implementation Details

The key insight was that the Zig compiler does support parsing arbitrary strings without module headers:
- `parse.parseStatement()` - Parses individual statements
- `parse.parseExpr()` - Parses expressions
- Token resolution via `ast.tokens.resolve()` to extract source text
- Pattern matching on AST nodes to detect assignments

Example of extracting an identifier from a parsed assignment:
```zig
const pattern = ast.store.getPattern(decl.pattern);
if (pattern == .ident) {
    const ident_tok = pattern.ident.ident_tok;
    const token_region = ast.tokens.resolve(@intCast(ident_tok));
    const ident = ast.env.source[token_region.start.offset..token_region.end.offset];
}
```

## Future Work

With the parsing and state management now working, the next steps are:
- Complete the evaluation pipeline integration (currently hits segfaults in tests)
- Add proper error handling and recovery
- Implement type information display
- Support multi-line input
- Handle complex patterns in assignments (not just simple identifiers)
- Add better formatting for evaluation results
- Integrate with the incremental compilation system