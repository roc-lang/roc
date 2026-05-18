# IPC (Inter-Process Communication)

This directory contains helpers for communication between the `roc` CLI and the Roc interpreter, which is spawned as a child process.

The communication is implemented using shared memory to ensure high performance.

The `roc` CLI owns semantic compilation: parse, canonicalize, type check, checked artifact publication, MIR-family lowering, IR lowering, LIR lowering, and ARC insertion. After ARC insertion, the parent publishes a target-specific LIR runtime image into the existing shared-memory region. The interpreter child maps that same shared memory, validates the runtime-image header, creates zero-copy LIR views, and evaluates explicit roots.

The interpreter child must not receive or inspect `ModuleEnv`, CIR, checked artifacts, MIR, or IR, and it must not run semantic compiler stages. Parent-child IPC for the runtime image is shared-memory mapping only, not serialization/deserialization.

This allows for a very responsive CLI and a fast development loop.
