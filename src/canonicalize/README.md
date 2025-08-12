# Canonicalize

Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through semantic analysis of the program.

The canonicalize module is the second stage of the Roc compiler pipeline. It performs semantic analysis, resolves names, and transforms the AST into a normalized intermediate representation that's easier for the type checker to process.
