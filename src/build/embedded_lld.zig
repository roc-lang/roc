//! Shared wrapper for the embedded LLD entrypoints exposed by zig_llvm.cpp.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
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

const Embedded = struct {
    extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;

    fn link(
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
            c_args[i] = (try arena.dupeSentinel(u8, arg, 0)).ptr;
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
};

fn externalLldSubcommand(format: Format) ?[]const u8 {
    return switch (format) {
        .elf => "ld.lld",
        .coff => "lld-link",
        .wasm => "wasm-ld",
        .macho => null,
    };
}

fn linkExternal(
    allocator: std.mem.Allocator,
    format: Format,
    args: []const []const u8,
    options: Options,
) Error!void {
    if (options.can_exit_early or options.disable_output) return Error.LinkFailed;
    const subcommand = externalLldSubcommand(format) orelse return Error.LinkFailed;

    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var argv = try std.array_list.Managed([]const u8).initCapacity(arena, args.len + 1);
    try argv.append(build_options.external_lld_zig_exe);
    try argv.append(subcommand);
    if (args.len > 1) {
        for (args[1..]) |arg| {
            try argv.append(arg);
        }
    }

    const std_io = std.Io.Threaded.global_single_threaded.io();
    const result = std.process.run(arena, std_io, .{ .argv = argv.items }) catch return Error.LinkFailed;
    if (result.stdout.len != 0) {
        std.Io.File.stdout().writeStreamingAll(std_io, result.stdout) catch {};
    }
    if (result.stderr.len != 0) {
        std.Io.File.stderr().writeStreamingAll(std_io, result.stderr) catch {};
    }

    if (result.term != .exited or result.term.exited != 0) return Error.LinkFailed;
}

/// Link using the embedded LLD entrypoint for `format`.
pub fn link(
    allocator: std.mem.Allocator,
    format: Format,
    args: []const []const u8,
    options: Options,
) Error!void {
    if (comptime build_options.external_lld) {
        return linkExternal(allocator, format, args, options);
    } else {
        return Embedded.link(allocator, format, args, options);
    }
}
