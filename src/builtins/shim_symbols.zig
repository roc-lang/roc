//! The symbol names that cross the boundary between compiled Roc output, the
//! platform shims, and hosts.
//!
//! Emitters (the backends and test-host generators) and definers (the shims and
//! hosts) both reference these constants, so a boundary symbol is spelled in
//! exactly one place. Each constant's name is identical to its value; a comptime
//! check below enforces that, and `host_abi.zig` keeps its `extern_host`
//! declarations in lockstep with `runtime_set`.
//!
//! This file is a leaf on purpose (`std` is used at comptime only): freestanding
//! hosts that cannot import the full `builtins` module import it directly.

const std = @import("std");

/// Returns the process-global `RocOps` built by the prelinked shim.
pub const roc_shim_get_ops = "roc_shim_get_ops";
/// Dispatches an entrypoint index into the prelinked shim.
pub const roc_entrypoint = "roc_entrypoint";
/// Dispatches an entrypoint index into the shim, passing an embedded LIR image.
pub const roc_entrypoint_from_image = "roc_entrypoint_from_image";
/// The hosted dispatch table defined by the generated platform shim module.
pub const roc_shim_hosted_fns = "roc_shim_hosted_fns";
/// Number of entries in `roc_shim_hosted_fns`.
pub const roc_shim_hosted_count = "roc_shim_hosted_count";
/// Backing array for `roc_shim_hosted_fns` in the generated shim module.
pub const roc_shim_hosted_fns_table = "roc_shim_hosted_fns_table";
/// The default platform's `main` implementation inside the machine-code shim.
pub const roc_shim_default_main = "roc_shim_default_main";
/// The default platform's pre-main runtime initialization hook.
pub const roc_default_runtime_init = "roc_default_runtime_init";
/// The synthetic default platform's exported Roc entrypoint.
pub const roc_default_start_main = "roc_default_start_main";
/// The default platform's process-exit hosted function.
pub const roc_default_exit = "roc_default_exit";
/// The default platform's line-echo hosted function.
pub const roc_default_echo_line = "roc_default_echo_line";
/// Backtrace entry table emitted into default-platform executables.
pub const roc_default_backtrace_table = "roc_default_backtrace_table";
/// Number of entries in `roc_default_backtrace_table`.
pub const roc_default_backtrace_count = "roc_default_backtrace_count";
/// The glue platform's entrypoint resolved by `roc glue`.
pub const roc_make_glue = "roc_make_glue";

/// Allocation callback in the fixed runtime set every host defines.
pub const roc_alloc = "roc_alloc";
/// Deallocation callback in the fixed runtime set every host defines.
pub const roc_dealloc = "roc_dealloc";
/// Reallocation callback in the fixed runtime set every host defines.
pub const roc_realloc = "roc_realloc";
/// `dbg` output callback in the fixed runtime set every host defines.
pub const roc_dbg = "roc_dbg";
/// Failed-`expect` callback in the fixed runtime set every host defines.
pub const roc_expect_failed = "roc_expect_failed";
/// Crash callback in the fixed runtime set every host defines.
pub const roc_crashed = "roc_crashed";

/// The fixed runtime symbols every symbol-ABI host defines, in
/// `host_abi.extern_host` declaration order.
pub const runtime_set = [_][:0]const u8{
    roc_alloc,
    roc_dealloc,
    roc_realloc,
    roc_dbg,
    roc_expect_failed,
    roc_crashed,
};

comptime {
    // Every constant in this module spells the symbol it names.
    for (@typeInfo(@This()).@"struct".decls) |decl| {
        const value = @field(@This(), decl.name);
        if (@TypeOf(value) == [:0]const u8 or (@typeInfo(@TypeOf(value)) == .pointer and
            @typeInfo(@TypeOf(value)).pointer.size == .one))
        {
            const text: []const u8 = value;
            if (!std.mem.eql(u8, text, decl.name)) {
                @compileError("shim_symbols." ++ decl.name ++ " must equal \"" ++ decl.name ++ "\"");
            }
        }
    }
}
