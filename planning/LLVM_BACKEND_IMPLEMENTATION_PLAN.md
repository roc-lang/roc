# LLVM Backend Implementation Plan

**DO NOT COMMIT THIS FILE** - This is a working planning document that should be deleted after implementation is complete.

**IMPORTANT**: Make commits as you complete each phase! Don't wait until the end.

---

## Overview

This document describes how to wire the monomorphization process into the LLVM backend, enabling Roc programs to be compiled to native code via LLVM. The end goal is:

1. **Dual-mode snapshot tests** (`type=repl`): Run both the interpreter AND LLVM backend, verify outputs match exactly
2. **New CLI flag**: `roc repl --optimize` uses LLVM backend instead of interpreter
3. **Full compilation pipeline**: Monomorphized IR → LLVM IR → Bitcode → Object Code → Executable

---

## Current Architecture

### Compilation Pipeline (Existing)

```
Roc Source Code
    ↓
Parser (src/parse/)
    ↓
Canonicalizer (src/canonicalize/Can.zig) → CIR (Canonical IR)
    ↓
Type Checker (src/check/Check.zig) → assigns types
    ↓
[NEW] Monomorphizer (src/canonicalize/Monomorphizer.zig)
    ↓
[NEW] ClosureTransformer (src/canonicalize/ClosureTransformer.zig)
    ↓
Interpreter (src/eval/interpreter.zig) → evaluates directly
```

### Key Components

| File | Purpose |
|------|---------|
| `src/canonicalize/Monomorphizer.zig` | Specializes polymorphic functions to concrete types |
| `src/canonicalize/ClosureTransformer.zig` | Defunctionalizes closures with captures into tagged unions |
| `src/canonicalize/RocEmitter.zig` | Converts CIR back to Roc source (for testing) |
| `src/eval/interpreter.zig` | Direct CIR evaluation with type-carrying runtime |
| `src/backend/llvm/Builder.zig` | Constructs LLVM IR in memory |
| `src/backend/llvm/bindings.zig` | Compiles LLVM bitcode to object code |
| `src/snapshot_tool/main.zig` | Snapshot test infrastructure |
| `src/repl/` | REPL implementation |

### Monomorphization Flow

1. **Finding Phase**: Walk code, discover needed specializations
2. **Making Phase**: Create specialized function versions
3. **Closure Transformation**: Convert closures to tagged unions with capture records

---

## Implementation Plan

### Phase 1: Understand the Existing Code (Research Only)

**Files to study:**

1. `src/canonicalize/Monomorphizer.zig`
   - `PartialProc`, `PendingSpecialization`, `SpecializedProc` types
   - `processPendingSpecializations()` main entry point
   - `makeSpecialization()` creates specialized versions

2. `src/canonicalize/ClosureTransformer.zig`
   - `ClosureRepresentation` enum
   - `LambdaSet` type
   - How closures become tagged union values

3. `src/eval/interpreter.zig`
   - `Interpreter.eval()` method
   - How values are represented (`StackValue`)
   - How functions are called

4. `src/backend/llvm/Builder.zig`
   - How to create functions, basic blocks, instructions
   - Type representation
   - Constant creation

5. `src/snapshot_tool/main.zig`
   - `processReplSnapshot()` function (line ~3652)
   - `generateReplOutputSection()` function
   - How `type=repl` tests work

**Commit**: None (research phase)

---

### Phase 2: Create LLVM IR Emitter

**Goal**: Create a new file that translates monomorphized CIR to LLVM IR.

**Create file**: `src/backend/llvm/emit.zig`

This module should:

1. Take monomorphized CIR as input
2. Walk the CIR expressions and emit corresponding LLVM IR
3. Handle:
   - Numeric literals (integers, floats)
   - Arithmetic operations (+, -, *, /)
   - Function definitions and calls
   - Record types (structs in LLVM)
   - Tag unions (tagged unions in LLVM)
   - Closures (already defunctionalized by ClosureTransformer)
   - Pattern matching (lowered to switches/branches)

**Key types to define:**

```zig
pub const LlvmEmitter = struct {
    builder: *llvm.Builder,
    allocator: std.mem.Allocator,

    // Maps from CIR types/values to LLVM equivalents
    type_map: std.AutoHashMap(CIR.TypeIdx, llvm.Type),
    value_map: std.AutoHashMap(CIR.ExprIdx, llvm.Value),

    pub fn init(allocator: Allocator) LlvmEmitter { ... }
    pub fn emitModule(self: *LlvmEmitter, module_env: *ModuleEnv) !void { ... }
    pub fn emitFunction(self: *LlvmEmitter, func: SpecializedProc) !llvm.Function { ... }
    pub fn emitExpr(self: *LlvmEmitter, expr: CIR.Expr) !llvm.Value { ... }
    pub fn emitType(self: *LlvmEmitter, ty: CIR.Type) !llvm.Type { ... }
};
```

**Start simple**: First handle just integer literals and arithmetic, then expand.

**Commit**: "Add LLVM IR emitter skeleton for monomorphized CIR"

---

### Phase 3: Integrate Emitter with Bitcode Pipeline

**Goal**: Connect emitter output to bitcode writer and object code generation.

**Create file**: `src/backend/llvm/codegen.zig`

This is the high-level API:

```zig
pub const CodegenResult = struct {
    object_code: []const u8,
    // or path to object file
};

pub fn compileToObjectCode(
    allocator: Allocator,
    module_env: *ModuleEnv,
    target_triple: []const u8,
    optimize: bool,
) !CodegenResult {
    // 1. Run monomorphization if not already done
    // 2. Run closure transformation
    // 3. Create LlvmEmitter
    // 4. Emit LLVM IR
    // 5. Serialize to bitcode
    // 6. Compile bitcode to object code using bindings.zig
    // 7. Return result
}
```

**Commit**: "Add high-level codegen API connecting emitter to object code"

---

### Phase 4: Create LLVM-Based Evaluator

**Goal**: Create an evaluator that uses LLVM JIT to run code.

**Create file**: `src/eval/llvm_evaluator.zig`

This should:

1. Compile the expression/module to object code
2. Use LLVM's JIT to execute
3. Return result as a string (for REPL output)

```zig
pub const LlvmEvaluator = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) LlvmEvaluator { ... }

    /// Evaluate an expression and return its string representation
    pub fn evalToString(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr: CIR.ExprIdx,
    ) ![]const u8 { ... }
};
```

**Note**: For initial implementation, you might compile to object code, link, and execute as a subprocess. JIT can come later.

**Commit**: "Add LLVM-based evaluator for running compiled code"

---

### Phase 5: Add `--optimize` Flag to REPL

**Goal**: Allow `roc repl --optimize` to use LLVM backend.

**Modify files:**

1. `src/cli/main.zig`
   - Add `--optimize` flag parsing in REPL command
   - Pass flag to REPL initialization

2. `src/repl/` (find the REPL implementation)
   - Add `use_llvm: bool` field
   - When `use_llvm` is true, use `LlvmEvaluator` instead of `Interpreter`

**Example CLI change:**

```zig
// In rocRepl or wherever REPL is initialized
const use_llvm = args.hasFlag("optimize");
if (use_llvm) {
    var evaluator = try LlvmEvaluator.init(allocator);
    // Use LLVM evaluator
} else {
    var interpreter = try Interpreter.init(...);
    // Use interpreter
}
```

**Commit**: "Add --optimize flag to REPL for LLVM backend"

---

### Phase 6: Dual-Mode Snapshot Testing

**Goal**: Make `type=repl` tests ALWAYS run BOTH interpreter and LLVM, verifying identical output. No flag needed - this is automatic for all type=repl snapshots.

**Modify file**: `src/snapshot_tool/main.zig`

Specifically, modify `generateReplOutputSection()` to always run both evaluators:

```zig
fn generateReplOutputSection(...) !bool {
    // ... existing code to run interpreter ...

    // ALWAYS also run LLVM evaluator and compare
    var llvm_evaluator = try LlvmEvaluator.init(output.gpa);
    defer llvm_evaluator.deinit();

    for (inputs.items, 0..) |input, i| {
        const interp_output = actual_outputs.items[i];

        // Run LLVM evaluator
        const llvm_output = try llvm_evaluator.evalToString(...);
        defer output.gpa.free(llvm_output);

        // Compare outputs - fail the snapshot if they differ
        if (!std.mem.eql(u8, interp_output, llvm_output)) {
            std.debug.print(
                \\LLVM/Interpreter MISMATCH in {s}:
                \\  Input: {s}
                \\  Interpreter: {s}
                \\  LLVM:        {s}
                \\
            , .{ snapshot_path, input, interp_output, llvm_output });
            success = false;
        }
    }

    // ... rest of existing code ...
}
```

**Commit**: "Add automatic dual-mode testing to type=repl snapshots"

---

### Phase 7: Expand Type Support

**Goal**: Support all Roc types in LLVM emission.

Implement in `src/backend/llvm/emit.zig`:

1. **Primitive types**: Bool, I8, I16, I32, I64, I128, U8, U16, U32, U64, U128, F32, F64
2. **Strings**: Pointer to length-prefixed data or small string optimization
3. **Lists**: Pointer to { length, capacity, elements }
4. **Records**: LLVM struct types
5. **Tag Unions**: Tagged unions (discriminant + payload)
6. **Functions**: Function pointers or defunctionalized closures

For each type, you need:
- Type mapping (CIR type → LLVM type)
- Value creation
- Operations (for numerics: +, -, *, /, etc.)

**Commit**: "Support [type name] in LLVM backend" (one commit per major type)

---

### Phase 8: Handle Builtins

**Goal**: Implement builtin functions for LLVM.

Builtins live in `src/builtins/` and need LLVM implementations:

1. **List operations**: List.map, List.get, List.len, List.append, etc.
2. **String operations**: Str.concat, Str.len, etc.
3. **Numeric operations**: Num.add, Num.mul, etc. (mostly already covered by operators)
4. **Debug operations**: dbg, crash, etc.

Strategy:
- Some builtins can be inlined as LLVM IR
- Others may need runtime library functions (link against `src/builtins/static_lib.zig`)

**Commit**: "Implement [builtin name] for LLVM backend" (grouped commits)

---

### Phase 9: Testing & Verification

**Goal**: Ensure all existing `type=repl` tests pass with dual-mode.

1. Run `zig build snapshot` and check for mismatches
2. Fix any differences between interpreter and LLVM output
3. Add new test cases for edge cases

**Test files to check:**
- `test/snapshots/list_map.md`
- `test/snapshots/try_map_ok.md`
- `test/snapshots/try_map_err.md`
- All other `type=repl` tests

**Commit**: "Fix [specific issue] in LLVM backend"

---

## File Structure After Implementation

```
src/
├── backend/
│   └── llvm/
│       ├── Builder.zig      # LLVM IR builder (from Zig)
│       ├── bindings.zig     # LLVM C API bindings
│       ├── bitcode_writer.zig
│       ├── BitcodeReader.zig
│       ├── ir.zig           # LLVM IR types
│       ├── emit.zig         # NEW: CIR → LLVM IR translation
│       ├── codegen.zig      # NEW: High-level compilation API
│       └── README.md
├── eval/
│   ├── interpreter.zig      # Existing interpreter
│   ├── llvm_evaluator.zig   # NEW: LLVM-based evaluation
│   └── ...
├── canonicalize/
│   ├── Monomorphizer.zig    # Specializes polymorphic functions
│   ├── ClosureTransformer.zig # Defunctionalizes closures
│   └── ...
└── ...
```

---

## Key Interfaces

### CIR Expression Types (from `src/canonicalize/Expression.zig`)

The emitter needs to handle all expression types:

- `literal` - Integer, float, string, bool literals
- `call` - Function calls
- `lambda` - Lambda expressions (after closure transform: captured vars in record)
- `record` - Record construction
- `record_access` - Field access
- `tag` - Tag union construction
- `match` - Pattern matching
- `if_then_else` - Conditionals
- `binary_op` - Binary operations
- `unary_op` - Unary operations
- `let` - Let bindings
- `block` - Statement blocks

### Type Mapping Strategy

| Roc Type | LLVM Type |
|----------|-----------|
| Bool | i1 |
| I8, U8 | i8 |
| I16, U16 | i16 |
| I32, U32 | i32 |
| I64, U64 | i64 |
| I128, U128 | i128 |
| F32 | float |
| F64 | double |
| Str | { ptr, len } or SSO struct |
| List a | { ptr, len, capacity } |
| Record | struct { fields... } |
| TagUnion | struct { tag: iN, payload: union } |
| Function | ptr or defunctionalized struct |

---

## Testing Strategy

### Unit Tests

Add tests in `src/backend/llvm/` for:
- Type mapping correctness
- Expression emission
- Roundtrip (source → CIR → LLVM → execute → compare)

### Integration Tests

The dual-mode `type=repl` snapshot tests automatically verify:
- Interpreter and LLVM produce identical output
- All existing tests continue to pass

### Manual Testing

```bash
# Run all snapshot tests
zig build snapshot

# Run specific test
zig build snapshot -- test/snapshots/list_map.md

# Check for mismatches without updating
zig build snapshot -- --check

# Use LLVM REPL manually
zig build run -- repl --optimize
```

---

## Common Pitfalls

1. **Type layout differences**: Ensure LLVM struct layouts match interpreter expectations
2. **Reference counting**: LLVM code must properly increment/decrement refcounts
3. **String representation**: Match the exact string format the interpreter uses
4. **Floating point precision**: Be careful with float formatting for output comparison
5. **Error handling**: LLVM and interpreter should produce identical error messages

---

## Resources

- `src/backend/llvm/README.md` - LLVM backend documentation
- `src/canonicalize/Monomorphizer.zig` - Monomorphization implementation
- `src/eval/test/mono_emit_test.zig` - Existing monomorphization tests
- `~/code/zig/src/codegen/llvm.zig` - Reference: Zig's LLVM codegen

---

## Summary Checklist

- [x] Phase 1: Study existing code (Monomorphizer, ClosureTransformer, Interpreter) ✓
- [x] Phase 2: Create `emit.zig` - CIR → LLVM IR translation ✓
- [x] Phase 3: Create `codegen.zig` - high-level compilation API ✓
- [x] Phase 4: Create `llvm_evaluator.zig` - LLVM-based evaluation ✓
- [x] Phase 5: Add `--optimize` flag to REPL ✓
- [x] Phase 6: Wire up dual-mode testing in snapshot tool (always runs both) ✓
- [x] Phase 7: Expand type support (all Roc types) ✓
- [x] Phase 8: Implement CIR expression translation (numeric literals working) ✓
- [x] Phase 9: All `type=repl` snapshots pass dual-mode testing ✓

**Remember: Commit after each phase!**
