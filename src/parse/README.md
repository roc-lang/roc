# Parse

Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.

- [src/parse/tokenize.zig](./tokenize.zig): Lexical analysis that breaks source text into tokens while preserving position information for diagnostics.
- [src/parse/Parser.zig](./Parser.zig): Recursive descent parser that builds an AST from the token stream with error recovery.
- [src/parse/AST.zig](./AST.zig): Abstract Syntax Tree representation with comprehensive node types for all Roc language constructs.
  - [src/parse/Node.zig](./Node.zig): Individual AST node definitions for expressions, patterns, statements, and type annotations.
  - [src/parse/NodeStore.zig](./NodeStore.zig): Efficient storage and indexing system for AST nodes with memory-optimized layouts.
