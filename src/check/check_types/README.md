# Check Types

Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).

- [src/check/check_types/unify.zig](./unify.zig): Core unification algorithm that solves type constraints and identifies type conflicts.
  - [src/check/check_types/occurs.zig](./occurs.zig): Prevents infinite type construction during unification (e.g., `a = List a`).
- [src/check/check_types/instantiate.zig](./instantiate.zig): Creates fresh type variables for polymorphic types at usage sites.
- [src/check/check_types/snapshot.zig](./snapshot.zig): Handles type state snapshots for backtracking during inference.
- [src/check/check_types/problem.zig](./problem.zig): Collects and formats type errors with precise diagnostic information.
- [src/check/check_types/copy_import.zig](./copy_import.zig): Manages type information sharing and copying between modules.