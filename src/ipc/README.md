# ipc

TODO

- helpers for communicating between the roc cli and the roc interpreter (spawned as a child process) using shared memory
- the roc cli is responsible for managing the cached roc modules, processing these through the compiler pipeline (tokenize, parse, canonicalize, typecheck) and then provides the fully type-checked ModuleEnv's to the roc interpreter for evaluation.
