# Check Types

Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).

The check module is the third stage of the Roc compiler pipeline. It performs type inference using the Hindley-Milner algorithm, ensuring type safety and generating the necessary type information for code generation. This stage catches type errors and ensures the program is well-typed before proceeding to evaluation or compilation.
