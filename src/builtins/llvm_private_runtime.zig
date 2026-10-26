//! The private runtime the LLVM backend links into every object it builds.
//!
//! LLVM's code generator lowers some operations to calls rather than
//! instructions: block copies and fills to `memcpy`, `memmove` and `memset`
//! (`bzero` for a zero fill on Darwin), a float remainder to `fmod`, rounding
//! on a CPU without rounding instructions to `floor` and its siblings, wide
//! arithmetic the target has no instruction for to compiler-rt helpers, and a
//! large stack frame on Windows to a stack probe. build.zig compiles this root
//! to bitcode once per target, for the lowest CPU that target names, and the
//! LLVM backend links it into the object it builds and gives the routines
//! `shim_symbols.llvm_private_runtime_set` names internal linkage. Every such
//! call binds to the object's own definition, so the object leaves only the
//! `roc_*` symbols for its platform to supply.

const std = @import("std");
const builtin = @import("builtin");
const compiler_rt = @import("compiler_rt");

/// The runtime must not pull in Zig's panic formatting machinery.
pub const panic = std.debug.no_panic;

comptime {
    // Referencing compiler-rt compiles every routine it exports.
    _ = @import("compiler_rt");
    if (builtin.os.tag.isDarwin()) {
        @export(&bzero, .{ .name = "bzero", .linkage = .weak, .visibility = .hidden });
    }
}

/// Darwin targets lower a zero fill of unknown length to `bzero`, which
/// compiler-rt does not define because libSystem does.
fn bzero(dest: ?[*]u8, len: usize) callconv(.c) void {
    @disableIntrinsics();
    _ = compiler_rt.memset(dest, 0, len);
}
