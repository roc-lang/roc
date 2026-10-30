//! The stack-bound check compiler-owned runtime code runs where it recurses.
//!
//! A wasm platform that reserves the addresses below its `global_base` has its
//! stack above static data, where nothing traps an overflow, so every function
//! that can recurse checks the stack bounds at its entry. Builtins never
//! recurse; the runtime functions that walk values and layouts of any depth
//! call `check` first. It compiles to nothing unless the object's root declares
//! `pub const roc_wasm_stack_checks = true;`, which only the stack-checked wasm
//! runtime objects do, so every other build pays nothing for it.

const builtin = @import("builtin");

/// Whether this object is a stack-checked wasm runtime object.
pub const enabled: bool = builtin.cpu.arch == .wasm32 and
    @hasDecl(@import("root"), "roc_wasm_stack_checks") and @import("root").roc_wasm_stack_checks;

/// Trap if the stack lacks the room this function's calls can need. The
/// bound is `__stack_low` plus the room the composed object's analysis fills
/// in as the relocation's addend (`StackRoom.fillCheckRooms`); a failed check
/// calls the compiler's overflow function, which leaves the stack pointer
/// below the stack for the host to see and traps.
pub inline fn check() void {
    if (comptime !enabled) return;
    const stack_low = @extern(*const u8, .{ .name = "__stack_low" });
    const overflow = @extern(*const fn () callconv(.c) noreturn, .{ .name = "roc_wasm_stack_overflow" });
    if (@frameAddress() < @intFromPtr(stack_low)) overflow();
}
