# Fuzzing

The typed source generators must not hardcode userspace Roc names.

All generated type names, tag names, field names, function names, local names,
module names, and filenames must come from the fuzzer's symbol generator. Do not
add fixture modules, canned domain models, semantic identifiers, or raw Roc
snippets that bake in userspace names.

Do not add catalog-style fuzzers made from handwritten scenario emitters. Fuzzers
should generate small typed language constructs and compose them randomly, so
new features multiply the reachable program space instead of adding one more
fixed example.

Builtin names are the only exception. Hardcoding `Bool`, `Str`, `List`, `Try`,
`Ok`, `Err`, and public builtin associated function names is allowed when the
generated program is deliberately exercising builtin behavior.

Generated platform/app fuzzers must also generate app-provided names, platform
aliases, and platform wrapper names. External ABI symbol strings and target names
are platform data, not userspace Roc identifiers.

On macOS, AFL++ may fail with `shmget() failed` under the default System V
shared-memory limits. Prefix AFL commands with `AFL_MAP_SIZE=2097152` unless the
machine has already been configured with `afl-system-config`.
