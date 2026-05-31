//! Shared wrapper for the embedded LLD entrypoints exposed by zig_llvm.cpp.

const std = @import("std");
const builtin = @import("builtin");

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

pub const Options = struct {
    can_exit_early: bool = false,
    disable_output: bool = false,
};

pub const Error = error{
    LinkFailed,
    OutOfMemory,
};

extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

pub fn link(
    allocator: std.mem.Allocator,
    format: Format,
    args: []const []const u8,
    options: Options,
) Error!void {
    var arena_impl = std.heap.ArenaAllocator.init(allocator);
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
