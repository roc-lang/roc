# Ownership Semantics in Roc Builtins

This document defines the canonical terminology for ownership semantics in Roc's builtin functions.
Understanding these patterns is critical for correctly implementing and calling builtins.

## Core Invariant

**refcount = number of live references to the data**

When refcount reaches 0, memory is freed.

Basic operations:
1. **Create**: allocate with refcount = 1
2. **Share**: increment refcount (data is shared, both references valid)
3. **Release**: decrement refcount (if 0, free memory)

---

## Argument Handling (2 patterns)

These describe how a function treats its input arguments.

### Borrow

A **borrowing** function reads its arguments without affecting their refcount.

- Caller retains ownership
- No refcount change at call boundary
- Caller can still use argument after call

**Examples**: `strEqual`, `listLen`, `strContains`, `countUtf8Bytes`

### Consume

A **consuming** function takes ownership of its argument.

- Caller transfers ownership to callee
- Caller must not use argument after call (logically moved)
- Function is responsible for cleanup (decref when done)

**Examples**: `strConcat`, `listConcat`, `strJoinWith`

---

## Result Patterns (3 types)

These describe the relationship between a function's result and its arguments.

### Independent

Result is a new allocation, unrelated to arguments.

- Normal ownership: caller owns result
- Caller must decref when done

**Example**: `strConcat` returns newly allocated combined string

### Copy-on-Write (Same-if-unique)

Result may be the same allocation as an argument.

- Consumes the input argument
- If `isUnique()`: mutates in place, returns same pointer
- If shared: decrefs argument internally, allocates new, returns new pointer
- **Critical**: `result.ptr` may equal `arg.ptr`

**Examples**: `strWithAsciiUppercased`, `strTrim`, `listAppend`

**Interpreter handling**: Check if `result.bytes == arg.bytes`:
- If same: skip decref (ownership passed through)
- If different: builtin already decrefd internally

### Seamless Slice

Result shares underlying data with argument via seamless slice.

- Borrows argument (caller keeps ownership)
- Builtin calls `incref` internally to share the allocation
- Result points into argument's memory
- `SEAMLESS_SLICE_BIT` marks the slice in length field

**Examples**: `strToUtf8`, `substringUnsafe`, `listSublist`

**Interpreter handling**: Decref the argument after call (builtin only incref'd for sharing;
the original binding's reference must still be released)

---

## Complete Taxonomy

The key insight is that seamless slices can be created by either borrowing OR consuming functions:

| Pattern | Arg Handling | Result Type | Interpreter After Call |
|---------|--------------|-------------|------------------------|
| Pure borrow | Borrow | Independent | Decref arg |
| Borrowing seamless slice | Borrow | Slice (incref'd) | Decref arg |
| Pure consume | Consume | Independent | **Don't decref** |
| Copy-on-write | Consume | Same-if-unique | **Don't decref** |
| Consuming seamless slice | Consume | Slice (inherited) | **Don't decref** |

**Simple rule**: Decref if and only if the builtin **borrows**. Never decref for **consume**.

### Why This Matters

For **borrowing** seamless slice (e.g., `strToUtf8`):
- Builtin calls `incref` to share the allocation
- Caller still has their reference
- Interpreter must decref (release the borrowed copy)

For **consuming** seamless slice (e.g., `strTrim` with offset):
- Builtin does NOT call `incref`
- Slice inherits the caller's reference
- Interpreter must NOT decref (ownership transferred)

### Copy-on-Write Detail

For copy-on-write builtins like `strWithAsciiUppercased`:
- If input is **unique**: mutates in place, returns same pointer
- If input is **shared**: builtin decrefs internally, allocates new

In BOTH cases, the interpreter should NOT decref:
- Unique case: ownership passed through to result
- Shared case: builtin already handled the decref

The previous heuristic (`result.bytes == arg.bytes`) was incomplete—it missed
the shared case where the builtin decrefs internally.

---

## Interpreter Contract

The interpreter uses ownership metadata per builtin:

| Argument Type | Interpreter Action After Call |
|---------------|-------------------------------|
| **Borrow** | Decref argument (release our copy) |
| **Consume** | Don't decref (ownership transferred to builtin) |

This requires each low-level op to declare whether each argument is borrowed or consumed.
See `src/check/lower_ops.zig` for the ownership metadata.

---

## Standard Terminology

| Term | Definition |
|------|------------|
| **Borrow** | Function reads argument without affecting refcount. Caller retains ownership. |
| **Consume** | Function takes ownership of argument. Caller loses access. Function handles cleanup. |
| **Copy-on-Write** | Consume variant: if unique, returns same allocation; if shared, decrefs and allocates new. |
| **Seamless Slice** | Result shares underlying data with argument. Builtin calls incref internally. |
| **Own** | The entity responsible for eventually calling decref. |
| **Unique** | Refcount == 1. Safe to mutate in place. |

---

## Function Documentation Format

Every builtin should document its ownership semantics:

```zig
/// Brief description of what the function does.
///
/// ## Ownership
/// - `arg1`: **consumes** - caller loses ownership
/// - `arg2`: **borrows** - caller retains ownership
/// - Returns: **independent** / **copy-on-write** / **seamless-slice**
///
/// ## Notes
/// Additional implementation details relevant to callers.
pub fn exampleFunction(...) ReturnType { ... }
```

---

## Function Categories

### str.zig

**Borrow args, Independent result:**
- `strEqual` - borrows both → Bool
- `strContains` - borrows both → Bool
- `strStartsWith` / `strEndsWith` - borrows both → Bool
- `strNumberOfBytes` / `countUtf8Bytes` - borrows → U64

**Consume arg, Copy-on-Write result:**
- `strWithAsciiUppercased` - consumes → Str (same-if-unique)
- `strWithAsciiLowercased` - consumes → Str (same-if-unique)

**Consume arg, Seamless-slice OR Copy-on-Write result:**
- `strTrim` / `strTrimStart` / `strTrimEnd` - consumes → Str
  - If unique with no offset needed: shrinks in place (copy-on-write)
  - Otherwise: creates consuming seamless slice (inherits reference)

**Consume args, Independent result:**
- `strConcat` - consumes first, borrows second → new Str
- `strJoinWith` - consumes list, borrows separator → new Str

**Borrow arg, Seamless-slice result (incref'd):**
- `strToUtf8` - borrows → List (seamless slice, calls incref)
- `strDropPrefix` / `strDropSuffix` - borrows → Str (seamless slice or incref'd original)

**Borrow arg, Seamless-slice result (no incref - caller must handle):**
- `substringUnsafe` - borrows → Str (seamless slice, NO incref!)
  - **WARNING**: Caller is responsible for refcount management

### list.zig

**Borrow args, Independent result:**
- `listLen` - borrows → U64
- `listIsEmpty` - borrows → Bool
- `listGetUnsafe` - borrows → pointer (no ownership transfer)

**Consume args, Copy-on-Write result:**
- `listAppend` - consumes list, borrows element → List (same-if-unique)
- `listPrepend` - consumes list, borrows element → List (same-if-unique)

**Consume args, Independent result:**
- `listConcat` - consumes both → new List
- `listMap` / `listKeepIf` / `listDropIf` - consumes → new List

**Consume arg, Seamless-slice OR Copy-on-Write result:**
- `listSublist` - consumes → List
  - If unique at start: shrinks in place (copy-on-write)
  - Otherwise: creates consuming seamless slice (inherits reference)
