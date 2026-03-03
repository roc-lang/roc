# Fallback/Workaround Audit: CIR→MIR → LIR → Codegen

## LIR → Dev Backend (`src/backend/dev/`)

### 16. Refcounting silently skips `closure` layouts (6 sites)
**`src/backend/dev/LirCodeGen.zig:16675, 16810, 16837, 16936, 16965, 16998`**
```zig
.list, .list_of_zst => { ... },
.scalar => { ... },
.box, .box_of_zst => { ... },
.struct_, .tag_union => { ... },
else => {},
```
The `else => {}` silently skips RC for `.closure` layouts. Closures can capture refcounted values (Str, List). Silent skip → memory leaks or use-after-free.

### 21. Null layout for non-immediate args can produce wrong register counts
**`src/backend/dev/LirCodeGen.zig:15485`**
```zig
else => {},  // arg_layout stays null
```
When `arg_layout` is null for non-immediate locations (`.stack`, `.stack_str`, `.list_stack`), `calcArgRegCount` falls back to 1 register. Multi-word types like strings (3 words) would get wrong register count → call corruption.

### 22. Dead no-op conditional
**`src/backend/dev/LirCodeGen.zig:15489`**
```zig
if (arg_loc == .list_stack) {} else if (arg_loc == .stack) {}
```
Completely dead statement — both branches have empty bodies. Leftover debugging code.

### 23. `getExprLayout` `else => null` drops available layout info
**`src/backend/dev/LirCodeGen.zig:1401`**

Returns `null` for `.tag_payload_access` (which has `.payload_layout`), `.zero_arg_tag` (has `.union_layout`), `.hosted_call` (has `.ret_layout`), and others. Callers must fall back to less precise resolution.

### 24. Callable-arg inlining silently skipped for block/if_then_else defs
**`src/backend/dev/LirCodeGen.zig:12818`**

When `call_has_callable_args` is true but the def is a `.block`/`.if_then_else`/`.match_expr` that returns a callable, `else => {}` skips inlining. Falls through to compiled proc route, but closure capture data may go stale.

### 25. Multi-word return values silently truncated to single register
**`src/backend/dev/LirCodeGen.zig:15167, 15181`**

For lists or large structs in non-stack locations, falls back to `moveToReturnRegister` which moves a single 64-bit value. A non-empty list (24 bytes, 3 registers) would only set the first return register.

### 26. Inner tag patterns silently dropped in `emitStringPatternCheck`
**`src/backend/dev/LirCodeGen.zig:7652`**
```zig
switch (inner_arg) {
    .bind => { ... },
    .wildcard => {},
    else => {},
}
```
Pattern types like `.int_literal`, `.str_literal`, `.tag` (nested), `.as_pattern` are silently ignored.

### 27. ObjectFileCompiler `else => {}` for non-function/data relocations
**`src/backend/dev/ObjectFileCompiler.zig:234`**

`.local_data` and `.jmp_to_return` relocations silently ignored when collecting external symbol references. Should be explicitly enumerated.

---

## LIR → Wasm Backend (`src/backend/wasm/`)

### 28. RC operations silently skipped for unhandled layout tags (2 sites)
**`src/backend/wasm/WasmCodeGen.zig:1127, 1223`**
```zig
.struct_, .tag_union => { try self.emitRcAtPtr(...); },
else => {},
```
Same issue as #16. `else => {}` silently drops RC for `.closure`, `.box`, etc. after `layoutContainsRefcounted` returned true.

### 29. `compileProcBody` silently returns on missing closure_value
**`src/backend/wasm/WasmCodeGen.zig:5991-5992`**
```zig
const self_cv = self.closure_values.get(key) orelse return;
const func_idx = self_cv.func_idx orelse return;
```
If registration failed, the wasm function will be declared but have no body — producing a broken module at runtime. No error or assertion.

### 30. Struct field call errors masked as `OutOfMemory`
**`src/backend/wasm/WasmCodeGen.zig:6937, 6969`**
```zig
return error.OutOfMemory;  // actually: struct field call target not resolvable
```
When a struct field expression can't be resolved or the field value isn't a lambda/closure, returns `error.OutOfMemory` instead of a meaningful error. Misleads all upstream error handling.

### 31. `list_contains` uses bytewise equality for all element types
**`src/backend/wasm/WasmCodeGen.zig:9800-9919`**

For composite-type elements (strings, records containing strings), does scalar wasm value comparison. Two strings with same content but different heap pointers compare as not-equal. `List.contains` produces wrong results for Str, records, etc.

### 32. `exprByteSize` fallback uses wasm ValType instead of layout
**`src/backend/wasm/WasmCodeGen.zig:2286-2302`**

When `exprLayoutIdx` returns null and the expr isn't a known composite, falls back to ValType-derived size. All `.i32` expressions (including pointers to larger structures) report as 4 bytes.

### 33. `exprLayoutIdx` catch-all `else => null`
**`src/backend/wasm/WasmCodeGen.zig:2230`**

Returns `null` for `.list`, `.empty_list` (which has `.elem_layout`), `.hosted_call` (has `.ret_layout`), and others. Callers fall back to imprecise behavior.

### 34. `isCompositeExpr` catch-all `else => false`
**`src/backend/wasm/WasmCodeGen.zig:2334`**

`.while_loop`, `.for_loop`, `.discriminant_switch` reported as non-composite. If they return records, binding code treats the result as scalar → incorrect codegen.

### 35. `emitConversion` silently no-ops for unhandled cross-type conversions
**`src/backend/wasm/WasmCodeGen.zig:4897-4927`**

Float-to-int conversions (f64→i32, f64→i64, f32→i32, f32→i64) are silently skipped. If codegen needs float-to-int conversion, raw float bits would be misinterpreted as integer.

### 36. `expect` evaluates condition but discards it
**`src/backend/wasm/WasmCodeGen.zig:1016-1021`**

`expect` drops the condition value without checking it. Should trap/abort when false in debug builds.

### 37. `dbg` is a complete no-op
**`src/backend/wasm/WasmCodeGen.zig:1012-1015`**

Evaluates the expression but never prints. Users get no output with no indication why.

### 38. Composite call stabilization missing for loop expressions
**`src/backend/wasm/WasmCodeGen.zig:2372`**

`exprNeedsCompositeCallStabilization` returns `false` for `.while_loop` and `.for_loop`, which could contain calls returning composites.

---

## LIR → LLVM Backend (`src/backend/llvm/`)

### 39. Silently swallowed CPU/features buffer overflow
**`src/backend/llvm/codegen.zig:132-143`**
```zig
std.fmt.bufPrintZ(&cpu_buf, "{s}", .{cpu}) catch null
```
If CPU name exceeds 64 bytes or features exceed 256 bytes, `catch null` silently drops them. Compilation proceeds without requested CPU/feature flags → silently wrong code generation. The `target_triple` path correctly returns an error for the same situation.

### 40. Bitcode serialization error silently loses details
**`src/backend/llvm/codegen.zig:112-114`**
```zig
const bitcode_words = builder.toBitcode(self.allocator, producer) catch {
    return CodegenResult.err("Failed to serialize bitcode");
};
```
Discards the actual error value. OOM is indistinguishable from other failures.

### 41. `getWipFunction() orelse return error.OutOfMemory` misattributes error
**`src/backend/llvm/codegen.zig:202`**

"No active function" is a logic/state error, not OOM. Misleads upstream error handling.

### 42. `endFunction` silently succeeds when no function is active
**`src/backend/llvm/emit.zig:330-337`**

If called with no active function, silently does nothing. Every other emitter method correctly returns `error.NoActiveFunction`.

### 43. Hardcoded `.i64` for Str/List length/capacity fields
**`src/backend/llvm/emit.zig:54-75`**

`len` and `capacity` are hardcoded to `.i64`. On 32-bit targets (wasm32), these should be `.i32`, causing ABI mismatches with Roc builtins.

### 44. `tagDiscriminantType` hardcodes boundaries independently
**`src/backend/llvm/emit.zig:196-206`**

Discriminant type boundaries (256, 65536, etc.) are hardcoded instead of derived from Roc's layout rules. If they diverge from what `layout.zig` uses, tag union layouts will be wrong.

### 45. `isZeroInit` `else => false` misses struct/array/splat zero cases
**`src/backend/llvm/Builder.zig:7549`**

Structures, packed structures, and arrays where all elements are zero are incorrectly reported as non-zero-init. Missed optimization opportunities.

### 46. `getBase` `else => .none` for unhandled constant tags
**`src/backend/llvm/Builder.zig:7580`**

`addrspacecast` of a global pointer silently loses its base tracking. Could cause incorrect constant folding or relocation handling.

### 47. Uncertain `sret` attribute ID
**`src/backend/llvm/Builder.zig:1366`**
```zig
sret = 29, // TODO: ?
```
If this attribute kind number is wrong, structure-return attributes are silently misencoded → ABI violations.
