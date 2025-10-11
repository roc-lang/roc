# Builtin Indices - Design Document

## Problem

Currently, the compiler has hardcoded constants like `BUILTIN_BOOL = 2` and `BUILTIN_RESULT = 13` that assume builtin types are always at specific statement indices. This is extremely brittle - adding a method to Bool.roc breaks everything because the indices change.

## Solution Overview

Generate a `builtin_indices.bin` file at build time that contains the actual statement indices for builtin type declarations. This file is created by:
1. Compiling builtin .roc modules (Bool.roc, Result.roc, etc.)
2. Using string-based lookups to find type declarations in those compiled modules
3. Serializing the found indices into a binary file
4. Embedding that file in the compiler binary

At runtime, we load `builtin_indices.bin` once and use those indices everywhere we currently use hardcoded constants.

## Benefits

- **Flexible builtin modules**: Can add/remove methods to Bool.roc without breaking anything
- **No runtime overhead**: String lookups happen at build time, not runtime
- **Single source of truth**: The actual .roc source files determine the indices
- **Type-safe**: Indices are checked at build time to ensure they exist

## Implementation Plan

### 1. Create BuiltinIndices struct (src/build/builtin_compiler/main.zig)

```zig
const BuiltinIndices = struct {
    /// Statement index of Bool type declaration within Bool module
    bool_type: u32,
    /// Statement index of Result type declaration within Result module
    result_type: u32,
    // Future: Add more as needed (Dict, Set, etc.)
};
```

### 2. Modify builtin_compiler to find and serialize indices

In `src/build/builtin_compiler/main.zig`, after compiling each module:

```zig
// After compiling Bool.roc
const bool_type_idx = try findTypeDeclaration(bool_env, "Bool");

// After compiling Result.roc
const result_type_idx = try findTypeDeclaration(result_env, "Result");

// Create indices struct
const indices = BuiltinIndices{
    .bool_type = bool_type_idx,
    .result_type = result_type_idx,
};

// Serialize to builtin_indices.bin
try serializeBuiltinIndices(gpa, indices, "zig-out/builtins/builtin_indices.bin");
```

The `findTypeDeclaration` function:
- Iterates through `module_env.all_statements`
- Looks for `s_nominal_decl` nodes
- Compares type name via string equality
- Returns the statement index when found

### 3. Embed builtin_indices.bin in the compiler (build.zig)

Add to the compiled_builtins module creation:

```zig
const indices_bin = b.addWriteFiles();
indices_bin.step.dependOn(&write_compiled_builtins.step);
const indices_path = indices_bin.add("builtin_indices.bin",
    write_compiled_builtins.getDirectory().path(b, "builtin_indices.bin"));

// Add to compiled_builtins_source generation
try file.writer().writeAll("pub const builtin_indices_bin = @embedFile(\"zig-out/builtins/builtin_indices.bin\");\n");
```

### 4. Load indices at appropriate times

#### For REPL (src/repl/eval.zig)

```zig
pub const Repl = struct {
    // ... existing fields ...
    builtin_indices: BuiltinIndices,

    pub fn init(...) !Repl {
        // Load indices once at REPL startup
        const compiled_builtins = @import("compiled_builtins");
        const builtin_indices = deserializeBuiltinIndices(compiled_builtins.builtin_indices_bin);

        // Load modules once at startup
        var bool_module = try loadCompiledModule(...);
        var result_module = try loadCompiledModule(...);

        return Repl{
            // ...
            .builtin_indices = builtin_indices,
            .bool_module = bool_module,
            .result_module = result_module,
        };
    }
};
```

During eval:
```zig
// Inject Bool at whatever index it ends up at
const bool_stmt = self.bool_module.env.store.getStatement(
    @enumFromInt(self.builtin_indices.bool_type)
);
const bool_decl_idx = try cir.store.addStatement(bool_stmt, ...);
// NO assertion about what index it's at!

// Store the ACTUAL index for later use
const actual_bool_idx = bool_decl_idx;
const actual_result_idx = result_decl_idx;
```

#### For type checker (src/check/Check.zig)

Replace:
```zig
const bool_var = ModuleEnv.varFrom(can.Can.BUILTIN_BOOL); // OLD
```

With:
```zig
const bool_var = ModuleEnv.varFrom(self.builtin_indices.bool_type); // NEW
```

This requires passing `builtin_indices` to the type checker, or loading it once globally.

### 5. Remove hardcoded constants (src/canonicalize/Can.zig)

Delete:
```zig
pub const BUILTIN_BOOL: Statement.Idx = @enumFromInt(2);
pub const BUILTIN_RESULT: Statement.Idx = @enumFromInt(13);
```

These are replaced by the dynamically-loaded indices.

### 6. Update the REPL injection to use actual indices

The key insight: we DON'T need Bool to be at index 2 in the REPL's module. We just need to:
1. Inject Bool and Result statements (at whatever indices they land at)
2. Tell the type checker where they actually are
3. Register unqualified tags (True, False, Ok, Err) with those actual indices

## Key Changes to Current Code

### builtin_compiler/main.zig
- Add `findTypeDeclaration()` function
- Add `serializeBuiltinIndices()` function
- Collect indices after compiling each module
- Write builtin_indices.bin file
- REMOVE the assertion that Bool is at index 2

### build.zig
- Embed builtin_indices.bin in compiled_builtins module
- Add it to the generated Zig source

### eval.zig
- Load builtin_indices.bin once at REPL init
- Store `BuiltinIndices` in `Repl` struct
- Use indices from struct instead of hardcoded values
- REMOVE assertions about specific index values
- Pass actual injection indices to canonicalizer/type checker

### Can.zig
- DELETE `BUILTIN_BOOL` and `BUILTIN_RESULT` constants

### Check.zig
- Accept builtin indices as parameter (or load globally)
- Use provided indices instead of `Can.BUILTIN_BOOL` constant

## Migration Path

1. Implement findTypeDeclaration in builtin_compiler
2. Generate builtin_indices.bin at build time
3. Embed it in compiled_builtins
4. Load it in REPL at startup
5. Use loaded indices in REPL injection
6. Update type checker to accept/use dynamic indices
7. Remove hardcoded constants
8. Verify all tests pass

## Open Questions

1. Should we load builtin_indices.bin globally (like a constant) or pass it around?
   - **Proposed**: Load once globally in a namespace/struct that type checker can access

2. What if we can't find a type declaration?
   - **Proposed**: Build-time error - fail the build immediately

3. How do we handle the indices being from the Bool/Result module's node store vs the current module?
   - **Proposed**: The indices in builtin_indices.bin refer to positions within Bool.bin and Result.bin respectively. When injecting into the REPL module, we copy the statements and they get NEW indices in the REPL's node store. We use those new indices for everything.

4. Do we still need to maintain the 2 and 13 indices somehow?
   - **Proposed**: NO! That's the whole point. Let them be whatever they are.

## Success Criteria

- [ ] Can add/remove methods to Bool.roc without build failures
- [ ] No hardcoded index constants anywhere
- [ ] String lookups only happen at build time
- [ ] REPL can evaluate `True` and `False` correctly
- [ ] Type checker can validate `if` expressions
- [ ] All existing tests pass
