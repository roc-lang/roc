# Fix Duplicated LLVM Bindings Files

## Problem

There are two near-identical LLVM bindings files in the codebase:

1. `src/llvm_compile/bindings.zig` (558 lines)
2. `src/cli/llvm_bindings.zig` (462 lines)

This duplication means:
- Bug fixes to one file won't automatically apply to the other
- The files can drift out of sync over time
- Maintenance burden is doubled

## Current Situation

Both files provide Zig bindings to the LLVM C API. They define similar structures and external function declarations for:

- LLVM context and module management
- Target machine creation
- JIT compilation (ORC API)
- LLD linker invocation

### Differences Between the Files

The files have slightly different type signatures in some places. For example, the LLD function declarations have different pointer types:

In `src/llvm_compile/bindings.zig`:
```zig
pub extern fn LLVMLinkMachO(
    argc: c_int,
    argv: [*:null]const ?[*:0]const u8,
    // ...
) bool;
```

In `src/cli/llvm_bindings.zig`:
```zig
pub extern fn LLVMLinkMachO(
    argc: c_int,
    argv: [*]const [*:0]const u8,
    // ...
) bool;
```

These different signatures (`[*:null]const ?[*:0]const u8` vs `[*]const [*:0]const u8`) affect how the caller needs to prepare the argument array.

## Why Two Files Exist

Looking at the code structure:

- `src/llvm_compile/` is a standalone module for JIT compilation, used by the snapshot test runner
- `src/cli/` contains CLI-specific code including AOT compilation

The bindings were likely duplicated to avoid import path issues or to allow the modules to be compiled independently.

## What Needs to Change

### Option 1: Single Shared File (Recommended)

Move the bindings to a shared location and have both modules import from there:

1. Keep `src/llvm_compile/bindings.zig` as the single source of truth
2. Update `src/cli/llvm_eval.zig` to import from `../llvm_compile/bindings.zig` or a shared path
3. Delete `src/cli/llvm_bindings.zig`
4. Reconcile any signature differences (prefer the more correct/strict version)

### Option 2: Create a Shared Module

Create a new shared location:

1. Create `src/shared/llvm_bindings.zig` with the unified bindings
2. Update both `src/llvm_compile/` and `src/cli/` to import from the shared location
3. Delete the duplicated files

## Reconciling Differences

When merging, you'll need to decide on the correct signatures. For the LLD functions:

- `[*:null]const ?[*:0]const u8` - A null-terminated array of optional null-terminated strings
- `[*]const [*:0]const u8` - An array of null-terminated strings (not null-terminated array)

Check how each is used:

In `src/cli/llvm_eval.zig`:
```zig
try args.append(null); // null terminator
const success = llvm_bindings.LinkMachO(
    @intCast(args.items.len - 1),
    @ptrCast(args.items.ptr),
    // ...
);
```

The code adds a null terminator and passes `len - 1` as the count, suggesting the `[*:null]` version might be more accurate to how it's actually used.

## Files to Modify

- `src/llvm_compile/bindings.zig` - Make this the canonical version
- `src/cli/llvm_eval.zig` - Update import path
- Delete `src/cli/llvm_bindings.zig`

## Build Configuration

Check `build.zig` to ensure both modules can access the shared bindings. You may need to update module dependencies.

## Testing

After consolidating:
- Run the snapshot tests (`zig build snapshot`) - these use the JIT path
- Test AOT compilation if possible - this uses the CLI path
- Verify both paths still compile and link correctly

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
