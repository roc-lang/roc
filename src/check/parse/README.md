# Parse

Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.

- [Tokenization](./tokenize.zig): Lexical analysis that breaks source text into tokens while preserving position information for diagnostics.
- [Parser](./Parser.zig): Recursive descent parser that builds an AST from the token stream with error recovery.
- [AST](./AST.zig): Abstract Syntax Tree representation with comprehensive node types for all Roc language constructs.
  - [Node](./Node.zig): Internal AST node definitions for expressions, patterns, statements, and type annotations.
  - [NodeStore](./NodeStore.zig): Efficient storage and indexing system for AST nodes with memory-optimized layouts.
