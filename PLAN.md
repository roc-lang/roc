# Plan: Implementing Str.roc with Primitive Type Mapping

## Goal
Add Str.roc to define methods on the `.str` primitive type, similar to Bool.roc and Result.roc, while preserving the `.str` primitive at runtime for interpreter compatibility.

## Key Approach
1. Define `Str` as a nominal type in `Str.roc`
2. **Before serialization** to `.bin`, transform all `Str` nominal types to `.str` primitive
3. This ensures the serialized `.bin` contains methods on `.str` primitive
4. String literals (already typed as `.str`) automatically get these methods

## Implementation Status

### ✅ Completed

1. **Created Str.roc** (src/build/roc/Str.roc)
   - Basic structure: `Str := [ProvidedByCompiler].{}`
   - Empty methods block for now

2. **Updated BuiltinIndices structures**
   - src/canonicalize/CIR.zig: Added `str_type: Statement.Idx`
   - src/build/builtin_compiler/main.zig: Added `str_type` field

3. **Added Str.roc compilation** (src/build/builtin_compiler/main.zig)
   - Reads Str.roc source
   - Compiles module
   - Finds Str type declaration
   - **Calls transformation** before serialization

4. **Implemented transformStrNominalToPrimitive()** (src/build/builtin_compiler/main.zig:44-94)
   - Iterates through all type variables in type store
   - Finds nominal types with ident "Str"
   - Replaces with `FlatType.str` primitive
   - Critical for interpreter compatibility

5. **Updated serialization**
   - Serializes Str.bin to zig-out/builtins/
   - Includes str_type in builtin_indices.bin

6. **Updated TestEnv.zig** (src/check/test/TestEnv.zig)
   - Loads str_module in both init functions
   - Sets exposed node indices
   - Adds to module_envs for auto-importing
   - Adds to imported_envs array
   - Cleans up in deinit()

### 🚧 Current Blocker

**Issue**: Canonicalization panic when compiling Str.roc

```
thread panic: reached unreachable code
src/canonicalize/NodeStore.zig:1111:21: getTypeHeader
    std.debug.assert(node.tag == .type_header);
```

**Location**: During `can_result.canonicalizeFile()` in builtin_compiler/main.zig:334

**Root Cause**: Unknown - The assertion fails because a node is not tagged as `.type_header` when it's expected to be.

**Hypotheses**:
1. "Str" might be a reserved keyword or have special meaning in parser
2. The `.{}` syntax might interact badly with "Str" specifically
3. There might be existing built-in handling of "Str" that conflicts

**Next Steps to Debug**:
1. Add debug logging to see what node.tag actually is
2. Try renaming to "String" temporarily to test if name is issue
3. Check if parser has special handling for "Str"
4. Review NodeStore.zig:1111 context to understand the assertion

### ⏳ Remaining Work (After Blocker Fixed)

1. **Update remaining auto-import sites**:
   - src/eval/interpreter.zig
   - src/compile/compile_package.zig
   - src/cli/main.zig
   - src/playground_wasm/main.zig
   - src/repl/eval.zig
   - src/snapshot_tool/main.zig
   - src/check/test/compiled_builtins_test.zig

2. **Verify build succeeds**
   - Run `zig build`
   - Check that Str.bin is generated
   - Verify transformation worked

3. **Add methods to Str.roc**
   - Start with `is_empty : Str -> Bool`
   - Follow Bool.roc pattern for method definitions

4. **Write integration tests**
   - Test `"hello".is_empty()` type-checks
   - Test in interpreter
   - Verify methods work on string literals

## Architecture Details

### How Transformation Works

The `transformStrNominalToPrimitive()` function:

```zig
fn transformStrNominalToPrimitive(env: *ModuleEnv) !void {
    // 1. Find "Str" identifier in module
    const str_ident = env.common.findIdent("Str") orelse return;

    // 2. Iterate all type variables
    var i: u32 = 0;
    while (i < env.types.len()) : (i += 1) {
        const var_idx = @enumFromInt(i);

        // 3. Skip redirects
        if (env.types.isRedirect(var_idx)) continue;

        // 4. Check for Str nominal type
        const resolved = env.types.resolveVar(var_idx);
        if (resolved.desc.content == .structure) {
            if (structure == .nominal_type) {
                if (nominal.ident.ident_idx == str_ident) {
                    // 5. Replace with .str primitive
                    const new_content = Content{ .structure = FlatType.str };
                    try env.types.setVarDesc(var_idx, new_desc);
                }
            }
        }
    }
}
```

### Why This Approach

**Advantages**:
- Type checker sees nominal `Str` with methods during Str.roc compilation
- Transformation converts to `.str` primitive before serialization
- Serialized .bin has methods on `.str` primitive
- String literals (already `.str`) automatically get methods
- Interpreter sees `.str` primitive (no breaking changes)

**Alternatives Considered**:
- ❌ Keep Str as nominal: Would break interpreter
- ❌ Define methods directly on .str: No source representation
- ❌ Separate type for methods: Requires explicit conversions

## Files Modified

**New**:
- src/build/roc/Str.roc

**Modified**:
- src/canonicalize/CIR.zig
- src/build/builtin_compiler/main.zig
- src/check/test/TestEnv.zig

**Pending**:
- src/eval/interpreter.zig
- src/compile/compile_package.zig
- src/cli/main.zig
- src/playground_wasm/main.zig
- src/repl/eval.zig
- src/snapshot_tool/main.zig
- src/check/test/compiled_builtins_test.zig

## Testing Strategy

**Phase 1**: Basic infrastructure (IN PROGRESS)
- [x] Create Str.roc
- [ ] Fix canonicalization panic ⚠️ BLOCKED
- [ ] Verify Str.bin generation
- [ ] Check transformation via debug logging

**Phase 2**: Integration
- [ ] Update all auto-import sites
- [ ] Run full test suite
- [ ] Verify no regressions

**Phase 3**: Methods
- [ ] Add `is_empty` method
- [ ] Test `"hello".is_empty()` in type checker
- [ ] Test in interpreter
- [ ] Add more methods incrementally

## Critical Insights

1. **Transformation timing**: Must happen AFTER canonicalization/type-checking but BEFORE serialization

2. **Type identity**: The transformation preserves everything except the nominal vs primitive distinction

3. **Method resolution**: Once transformed, methods defined on `Str` nominal become methods on `.str` primitive

4. **Interpreter compatibility**: By using `.str` primitive in the serialized form, the interpreter sees the same type it always has

5. **Auto-importing**: Str must be auto-imported just like Bool and Result for ergonomic use

## Open Questions

1. Is "Str" a reserved word or keyword in the Roc parser?
2. Does the canonicalizer have special handling for "Str" that conflicts?
3. Should we add methods that take `self` vs free functions?
4. How do we handle methods that need compiler intrinsics (e.g., actual string operations)?
