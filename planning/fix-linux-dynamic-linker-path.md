# Fix Hardcoded Linux Dynamic Linker Path

## Problem

The LLVM backend hardcodes the Linux dynamic linker path to the x86_64 version. This will fail on aarch64 (ARM64) Linux systems, which use a different dynamic linker path.

## Location

`src/cli/llvm_eval.zig`, lines 131-132:

```zig
try args.append(try allocator.dupeZ(u8, "-dynamic-linker"));
try args.append(try allocator.dupeZ(u8, "/lib64/ld-linux-x86-64.so.2"));
```

## Dynamic Linker Paths by Architecture

Different architectures use different dynamic linker paths:

| Architecture | Dynamic Linker Path |
|--------------|---------------------|
| x86_64 | `/lib64/ld-linux-x86-64.so.2` |
| aarch64 | `/lib/ld-linux-aarch64.so.1` |
| i386 | `/lib/ld-linux.so.2` |
| arm (32-bit) | `/lib/ld-linux-armhf.so.3` |
| riscv64 | `/lib/ld-linux-riscv64-lp64d.so.1` |

## Context

When linking an ELF executable on Linux, the linker needs to know the path to the dynamic linker (also called the "interpreter" or "program loader"). This is the program that loads shared libraries at runtime.

The current code only handles x86_64, but Roc should work on aarch64 Linux as well (e.g., ARM servers, Raspberry Pi, Apple Silicon running Linux).

## What Needs to Change

Update the Linux linking section to select the correct dynamic linker based on the target architecture:

```zig
.linux => {
    // Linux uses ld.lld
    try args.append(try allocator.dupeZ(u8, "ld.lld"));
    try args.append(try allocator.dupeZ(u8, "-o"));
    try args.append(try allocator.dupeZ(u8, exe_path));
    try args.append(try allocator.dupeZ(u8, obj_path));
    try args.append(try allocator.dupeZ(u8, "-lc"));
    try args.append(try allocator.dupeZ(u8, "-dynamic-linker"));

    const dynamic_linker = switch (builtin.cpu.arch) {
        .x86_64 => "/lib64/ld-linux-x86-64.so.2",
        .aarch64 => "/lib/ld-linux-aarch64.so.1",
        .x86 => "/lib/ld-linux.so.2",
        .arm => "/lib/ld-linux-armhf.so.3",
        .riscv64 => "/lib/ld-linux-riscv64-lp64d.so.1",
        else => return error.UnsupportedTarget,
    };
    try args.append(try allocator.dupeZ(u8, dynamic_linker));

    try args.append(null);
    // ... rest of linking
}
```

## Additional Considerations

### Library Path

The library path may also differ. Currently there's no `-L` argument for Linux, but if one is added later, consider:

- x86_64: `/lib/x86_64-linux-gnu` or `/lib64`
- aarch64: `/lib/aarch64-linux-gnu` or `/lib`

### Musl vs Glibc

The paths above are for glibc-based systems. Musl-based systems (like Alpine Linux) use different paths:

- Musl x86_64: `/lib/ld-musl-x86_64.so.1`
- Musl aarch64: `/lib/ld-musl-aarch64.so.1`

For now, focusing on glibc is reasonable since it's the most common, but this could be extended later.

## Files to Modify

- `src/cli/llvm_eval.zig` - Update the Linux linking section

## Also Update the Target Triple

While you're at it, check the `getHostTriple()` function (lines 200-215). It currently returns the correct architecture-specific triple, which is good. Just verify it stays consistent with the dynamic linker selection.

## Testing

- Test on x86_64 Linux: Verify existing behavior still works
- Test on aarch64 Linux: Verify linking succeeds with the correct dynamic linker
- If no ARM Linux machine is available, at least verify the code paths are correct by inspection

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
