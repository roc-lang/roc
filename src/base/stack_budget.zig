//! The stack budget for every thread that executes Roc code or deeply
//! recursive compiler passes.
//!
//! `build.zig` applies `roc_stack_size` to the compiler executables'
//! `stack_size`, which becomes the PT_GNU_STACK program header; Zig's start
//! code raises RLIMIT_STACK to match, so the main thread gets this budget.
//! Every spawned thread that can run Roc code (test-runner workers,
//! compile-time evaluation workers, compile coordinator workers) must pass the
//! same value to `std.Thread.spawn`, so a Roc program's recursion depth limit
//! never depends on which thread the work happens to be scheduled on.
//!
//! This file must stay dependency-free: build.zig imports it by path.

const std = @import("std");
const builtin = @import("builtin");

/// Stack size, in bytes, for the main thread and for every spawned thread
/// that executes Roc code.
pub const roc_stack_size: usize = 64 * 1024 * 1024;

/// The `stack_size` to pass to `std.Thread.spawn` for a thread that executes
/// Roc code.
///
/// On Windows, Zig 0.17 passes `stack_size` to `NtCreateThreadEx` as the
/// stack *commit*, so every 64 MiB thread charges 64 MiB against the system
/// commit limit up front, and a few dozen of them exhaust it
/// (`NTSTATUS 0xc000012d`). The thread's reserve is the larger of that value
/// and the executable's PE `SizeOfStackReserve`, so when the executable
/// already reserves `roc_stack_size` (build.zig sets that on the compiler and
/// test-runner executables) a minimal commit gives the same reserve without
/// the charge. Executables that do not reserve it keep the full request.
pub fn spawnStackSize() usize {
    if (builtin.os.tag != .windows) return roc_stack_size;
    if (executableStackReserve() >= roc_stack_size) return 64 * 1024;
    return roc_stack_size;
}

fn executableStackReserve() u64 {
    if (builtin.os.tag != .windows) return 0;
    const image: [*]const u8 = @ptrCast(std.os.windows.peb().ImageBaseAddress);
    const pe_offset = std.mem.readInt(u32, image[0x3c..][0..4], .little);
    // PE signature (4) + COFF file header (20) precede the optional header.
    const optional = image[pe_offset + 24 ..];
    // PE32+ only; SizeOfStackReserve is 72 bytes into the optional header.
    if (std.mem.readInt(u16, optional[0..2], .little) != 0x20b) return 0;
    return std.mem.readInt(u64, optional[72..][0..8], .little);
}

test "spawnStackSize never exceeds the Roc stack budget" {
    try std.testing.expect(spawnStackSize() <= roc_stack_size);
    try std.testing.expect(spawnStackSize() >= 64 * 1024);
}
