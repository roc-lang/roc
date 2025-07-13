# Check

Transforms Roc source code through parsing, canonicalization, and type checking.

- [src/check/parse.zig](./parse.zig) and [src/check/parse/](./parse/): Converts source text into an Abstract Syntax Tree (AST) through tokenization and parsing.
- [src/check/canonicalize.zig](./canonicalize.zig) and [src/check/canonicalize/](./canonicalize/): Transforms AST into Canonical Intermediate Representation (CIR) with desugaring and scope resolution.
- [src/check/check_types.zig](./check_types.zig) and [src/check/check_types/](./check_types/): Performs Hindley-Milner type inference with constraint solving and unification.