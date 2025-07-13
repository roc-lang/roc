# Canonicalize

Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.

- [CIR](./CIR.zig): Canonical Intermediate Representation definitions.
  - [Expression Processing](./Expression.zig): Represents expressions like number and string literals, functions, if-then, match expressions.
  - [Pattern Handling](./Pattern.zig): Represents patterns like list patterns, tuple patterns, and wildcard patterns.
  - [Statement Processing](./Statement.zig): Represents statements like assignments, function definitions, alias and nominal type declarations.
  - [Type Annotations](./TypeAnnotation.zig): Represents type annotations such as function signatures and explicit type declarations.
- [Diagnostics](./Diagnostic.zig): Represents semantic Errors and Warnings to provide detailed feedback.
- [Scope Management](./Scope.zig): Tracks variable bindings, shadowing, and lexical scoping throughout the program.
- [Node Management](./Node.zig) and [NodeStore](./NodeStore.zig): Efficient internal storage and management of the CIR.