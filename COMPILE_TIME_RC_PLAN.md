# Compile-Time RC Insertion Plan

## CRITICAL CONSTRAINTS - READ THIS FIRST

1. **DELETE RUNTIME RC FIRST** - Before ANY other work, ALL runtime RC code must be PERMANENTLY deleted
2. **NEVER RESTORE RUNTIME RC** - Even if tests fail, runtime RC must NEVER be restored. Failing tests mean the project is incomplete.
3. **This is a COMPILER** - We are rewriting the compiler in Zig. Runtime RC is fundamentally wrong for a compiler.

---

## Understanding How crates/ Does It

The Rust implementation in `crates/compiler/mono/src/inc_dec.rs` works as follows:

### Core Algorithm

1. **Track symbol ownership**: Each refcounted symbol is either `Owned` or `Borrowed`
2. **Symbols start as Owned** when introduced by Let bindings or function arguments
3. **When a symbol is "consumed"** (used in an expression), it transitions from Owned to Borrowed
4. **At scope exit**: If a symbol is still Owned (wasn't consumed), insert a `Dec` for it
5. **For multiple uses**: If a symbol is used N times, insert `Inc(N-1)` because the first use is "free" (transfers ownership)

### Key Functions in inc_dec.rs

```rust
// Main entry - processes each procedure
insert_inc_dec_operations_proc()

// Recursively processes statements, tracking ownership
insert_refcount_operations_stmt()

// Handles Let bindings - adds symbol as Owned, processes continuation,
// if symbol still Owned at end, insert Dec
insert_refcount_operations_binding()

// For expressions with multiple symbol uses, insert Inc for each use after the first
consume_and_insert_inc_stmt()

// Insert Dec for symbols that are still Owned at scope exit
consume_and_insert_dec_stmt()
```

### Branch Handling

For if/else and match:
1. Process each branch independently with a cloned environment
2. Find symbols consumed in SOME but not ALL branches
3. For those symbols, insert Dec in branches where they weren't consumed
4. This ensures all branches have the same "consumption state" at the end

---

## Phase 1: DELETE ALL RUNTIME RC (DO THIS FIRST)

### Files to Modify

#### 1.1 `src/eval/interpreter.zig`

Delete/remove these functions entirely or make them no-ops that don't touch RC:

- `trimBindingList` - Change to just truncate, NO decref loop
- `cleanupBindings` - Change to just clear, NO decref loop
- `upsertBinding` - Remove the decref when replacing a binding
- `popCollectedValues` - Remove the decref calls

**KEEP the e_incref/e_decref/e_free handlers** - these execute the CIR RC instructions that the compile-time pass inserts.

#### 1.2 `src/eval/StackValue.zig`

**KEEP incref() and decref()** - These are called by the e_incref/e_decref handlers to actually perform the RC operations. They must work.

---

## Phase 2: Implement Proper Compile-Time RC Insertion

The RC pass in `src/rc/insert.zig` must implement the same algorithm as `crates/compiler/mono/src/inc_dec.rs`.

### 2.1 Core Data Structures

```zig
const Ownership = enum { owned, borrowed };

const SymbolState = struct {
    ownership: Ownership,
    // ... other fields as needed
};

// Map from pattern_idx to ownership state
symbol_ownership: std.AutoHashMap(CIR.Pattern.Idx, Ownership)
```

### 2.2 Algorithm Overview

For each expression/statement:

1. **Let binding**:
   - Add the bound symbol as `owned`
   - Process the continuation
   - If symbol is still `owned` at end, insert `e_decref`
   - Remove symbol from environment

2. **Symbol usage** (e_lookup):
   - If symbol is `owned`, consume it (set to `borrowed`), no Inc needed
   - If symbol is `borrowed`, insert `e_incref` (we need another reference)

3. **Function call arguments**:
   - Each argument is a usage of that symbol
   - Count uses per symbol, insert `Inc(N-1)` for N uses

4. **Branches (if/match)**:
   - Clone environment for each branch
   - Process each branch independently
   - Find symbols consumed in some but not all branches
   - Insert Dec in branches where symbol wasn't consumed

5. **Lambda/closure parameters**:
   - Parameters are `borrowed` by default (caller owns them)
   - If parameter needs to survive past the call, it gets incref'd

### 2.3 Key Implementation Points

**Process BACKWARD from continuation**:
```
let x = expr1 in
let y = expr2 in
body
```
Process `body` first, then `let y`, then `let x`. This way we know which symbols are consumed before we decide whether to insert Dec.

**Track uses during expression processing**:
When we see a symbol used, check its current ownership:
- If owned and this is first use: consume it (set to borrowed)
- If borrowed or subsequent use: will need Inc

**Branch reconciliation**:
```
if cond then
    use x  // x consumed here
else
    42     // x NOT consumed here - need Dec for x
```

---

## Phase 3: Integration

### 3.1 Where the RC Pass Runs

The RC pass should run AFTER type checking and layout computation, but BEFORE interpretation/code generation. It transforms the CIR to add explicit `e_incref`, `e_decref`, `e_free` instructions.

### 3.2 Verification

After implementing, run tests:
```bash
zig build test
```

Expected: ALL tests pass with NO memory leaks.

If tests fail with leaks, the RC pass is incomplete - DO NOT restore runtime RC.

---

## Summary Checklist

### Phase 1 (DELETE RUNTIME RC)
- [ ] `trimBindingList` - just truncate, no decref
- [ ] `cleanupBindings` - just clear, no decref
- [ ] `upsertBinding` - no decref when replacing
- [ ] `popCollectedValues` - no decref
- [ ] Verify build succeeds

### Phase 2 (IMPLEMENT COMPILE-TIME RC)
- [ ] Track symbol ownership (owned vs borrowed)
- [ ] Process statements backward from continuation
- [ ] Insert Dec for symbols still owned at scope exit
- [ ] Insert Inc for multiple uses of same symbol
- [ ] Handle branch reconciliation
- [ ] Handle function arguments
- [ ] Handle lambda parameters

### Phase 3 (VERIFY)
- [ ] All tests pass
- [ ] No memory leaks
