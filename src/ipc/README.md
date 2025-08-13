# IPC (Inter-Process Communication)

This directory contains helpers for communication between the `roc` CLI and the Roc interpreter, which is spawned as a child process.

The communication is implemented using shared memory to ensure high performance.

The `roc` CLI is responsible for managing cached Roc modules and processing them through the compiler pipeline (tokenize, parse, canonicalize, typecheck). The fully type-checked module environment is then sent to the Roc interpreter for evaluation.

This allows for a very responsive CLI and a fast development loop.
