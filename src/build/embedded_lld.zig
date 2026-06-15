//! Shared wrapper for the embedded LLD entrypoints exposed by zig_llvm.cpp.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");

/// COFF stack-probe (___chkstk_ms) object generation, shared by every
/// embedded-lld COFF link of Roc-generated code.
pub const stack_probe = @import("stack_probe.zig");

/// Object format handled by one of the embedded LLD frontends.
pub const Format = enum {
    elf,
    coff,
    macho,
    wasm,

    pub fn detectFromSystem() Format {
        return detectFromOs(builtin.target.os.tag);
    }

    pub fn detectFromOs(os: std.Target.Os.Tag) Format {
        return switch (os) {
            .windows => .coff,
            .macos, .ios, .watchos, .tvos => .macho,
            .freestanding => .wasm,
            else => .elf,
        };
    }
};

/// Options forwarded directly to the embedded LLD entrypoint.
pub const Options = struct {
    can_exit_early: bool = false,
    disable_output: bool = false,
};

/// Errors returned by the embedded LLD wrapper.
pub const Error = error{
    LinkFailed,
    OutOfMemory,
};

extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

/// Link using the embedded LLD entrypoint for `format`.
pub fn link(
    allocator: std.mem.Allocator,
    format: Format,
    args: []const []const u8,
    options: Options,
) Error!void {
    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const c_args = try arena.alloc([*:0]const u8, args.len);
    for (args, 0..) |arg, i| {
        c_args[i] = (try arena.dupeZ(u8, arg)).ptr;
    }

    const success = switch (format) {
        .elf => ZigLLDLinkELF(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
        ),
        .coff => ZigLLDLinkCOFF(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
        ),
        .macho => ZigLLDLinkMachO(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
        ),
        .wasm => ZigLLDLinkWasm(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
        ),
    };

    if (!success) return Error.LinkFailed;
}
