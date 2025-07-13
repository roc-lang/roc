# Canonicalize

Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.

- [src/check/canonicalize/CIR.zig](./CIR.zig): Canonical Intermediate Representation definitions.
  - [src/check/canonicalize/Expression.zig](./Expression.zig): Represents expressions like number and string literals, functions, if-then, match expressions.
  - [src/check/canonicalize/Pattern.zig](./Pattern.zig): Represents patterns like list patterns, tuple patterns, and wildcard patterns.
  - [src/check/canonicalize/Statement.zig](./Statement.zig): Represents statements like assignments, function definitions, alias and nominal type declarations.
  - [src/check/canonicalize/TypeAnnotation.zig](./TypeAnnotation.zig): Represents type annotations such as function signatures and explicit type declarations.
- [src/check/canonicalize/Diagnostic.zig](./Diagnostic.zig): Represents semantic Errors and Warnings to provide detailed feedback.
- [src/check/canonicalize/Scope.zig](./Scope.zig): Tracks variable bindings, shadowing, and lexical scoping throughout the program.
- [src/check/canonicalize/Node.zig](./Node.zig) and [src/check/canonicalize/NodeStore.zig](./NodeStore.zig): Efficient internal storage and management of the CIR.