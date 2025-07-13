# Check Types

Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).

- [Unification](./unify.zig): Core unification algorithm that solves type constraints and identifies type conflicts.
  - [Occurs Check](./occurs.zig): Prevents infinite type construction during unification.
- [Instantiation](./instantiate.zig): Creates fresh type variables for polymorphic types at usage sites.
- [Snapshot Management](./snapshot.zig): Handles type state snapshots for backtracking during inference.
- [Problem Reporting](./problem.zig): Collects and formats type errors with precise diagnostic information.
- [Cross-Module Types](./copy_import.zig): Manages type information sharing and copying between modules.
