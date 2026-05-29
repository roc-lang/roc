# IPC (Inter-Process Communication)

This directory contains helpers for communication between the `roc` CLI and the Roc interpreter, which is spawned as a child process.

The communication is implemented using shared memory to ensure high performance.

The `roc` CLI owns parse, canonicalization, type checking, checked module publication, post-check lowering, LIR lowering, and ARC insertion. After ARC insertion, the parent publishes a target-specific LIR image into the existing shared-memory region. The interpreter child maps that same shared memory, validates the LIR image header, creates zero-copy LIR views, and evaluates explicit roots.

The interpreter child must not receive or inspect `ModuleEnv`, CIR, checked modules, or post-check IRs, and it must not run compiler lowering stages. Parent-child IPC for the LIR image is shared-memory mapping only, not serialization/deserialization.

This allows for a very responsive CLI and a fast development loop.
