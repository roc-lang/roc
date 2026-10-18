//! Shared wrapper for the embedded LLD entrypoints exposed by zig_llvm.cpp.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");

/// COFF stack-probe (___chkstk_ms) object generation, shared by every
/// embedded-lld COFF link of Roc-generated code.
pub const stack_probe = @import("stack_probe.zig");

/// Darwin sysroot resolution, shared by every embedded-lld Mach-O link of
/// Roc-generated code.
pub const darwin_sysroot = @import("darwin_sysroot.zig");

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
            .linux,
            .other,
            .contiki,
            .fuchsia,
            .hermit,
            .managarm,
            .haiku,
            .hurd,
            .illumos,
            .plan9,
            .rtems,
            .serenity,
            .dragonfly,
            .freebsd,
            .openbsd,
            .netbsd,
            .driverkit,
            .maccatalyst,
            .visionos,
            .uefi,
            .@"3ds",
            .wiiu,
            .@"switch",
            .gba,
            .psx,
            .tios,
            .ashetos,
            .ps3,
            .ps4,
            .ps5,
            .psp,
            .vita,
            .emscripten,
            .wasi,
            .amdhsa,
            .amdpal,
            .cuda,
            .mesa3d,
            .nvcl,
            .opencl,
            .opengl,
            .vulkan,
            => .elf,
        };
    }
};

/// Options forwarded directly to the embedded LLD entrypoint.
pub const Options = struct {
    can_exit_early: bool = false,
    disable_output: bool = false,
};

/// What one embedded link reported: whether it produced its output, and
/// everything the linker wrote to its error stream, warnings included. The
/// diagnostics belong to the caller's allocator.
pub const Outcome = struct {
    succeeded: bool,
    diagnostics: []u8,
};

extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool, diagnostics: *?[*]const u8, diagnostics_len: *usize) bool;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool, diagnostics: *?[*]const u8, diagnostics_len: *usize) bool;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool, diagnostics: *?[*]const u8, diagnostics_len: *usize) bool;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool, diagnostics: *?[*]const u8, diagnostics_len: *usize) bool;
extern fn ZigLLDFreeDiagnostics(diagnostics: ?[*]const u8) void;

/// Link using the embedded LLD entrypoint for `format`. The linker's own
/// output file is the link's result; the returned outcome says whether it was
/// produced and carries the linker's diagnostics for the caller to print and
/// read.
pub fn link(
    allocator: std.mem.Allocator,
    format: Format,
    args: []const []const u8,
    options: Options,
) std.mem.Allocator.Error!Outcome {
    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const c_args = try arena.alloc([*:0]const u8, args.len);
    for (args, 0..) |arg, i| {
        c_args[i] = (try arena.dupeSentinel(u8, arg, 0)).ptr;
    }

    var diagnostics_ptr: ?[*]const u8 = null;
    var diagnostics_len: usize = 0;
    const success = switch (format) {
        .elf => ZigLLDLinkELF(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
            &diagnostics_ptr,
            &diagnostics_len,
        ),
        .coff => ZigLLDLinkCOFF(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
            &diagnostics_ptr,
            &diagnostics_len,
        ),
        .macho => ZigLLDLinkMachO(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
            &diagnostics_ptr,
            &diagnostics_len,
        ),
        .wasm => ZigLLDLinkWasm(
            @intCast(c_args.len),
            c_args.ptr,
            options.can_exit_early,
            options.disable_output,
            &diagnostics_ptr,
            &diagnostics_len,
        ),
    };
    defer ZigLLDFreeDiagnostics(diagnostics_ptr);

    const diagnostics = if (diagnostics_ptr) |ptr| try allocator.dupe(u8, ptr[0..diagnostics_len]) else try allocator.alloc(u8, 0);
    return .{ .succeeded = success, .diagnostics = diagnostics };
}
