# LLVM Compile

Compiles LLVM bitcode into native shared libraries for JIT evaluation.

This module sits between the LLVM IR codegen backend (`src/backend/llvm/`) and
the evaluators that execute compiled Roc code. It handles the LLVM-to-native
compilation pipeline and platform-specific linking so that callers can simply
`dlopen` the result and call `roc_eval`.
