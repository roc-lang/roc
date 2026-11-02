//! Build helper: copy a freshly linked macOS roc executable and remove its
//! dyld export trie and weak-binding info (see src/cli/macho/DyldExportStrip.zig
//! for why), then rewrite the ad-hoc code signature the edit invalidated.
//! Fails, naming the path, if the executable loads a dylib from outside the
//! OS or carries a run-path search entry.
//!
//! Usage:
//!   strip_macho_exports <input-binary> <output-binary>

const std = @import("std");
const build_options = @import("build_options");
const DyldExportStrip = @import("dyld_export_strip");

/// Largest binary this tool will process; the roc CLI is ~220 MB.
const max_binary_bytes: usize = 4 * 1024 * 1024 * 1024;

/// Copies the input executable to the output path, strips its dyld export
/// and weak-bind info, and re-signs it.
pub fn main(init: std.process.Init) !void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const stderr_file: std.Io.File = .stderr();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) {
        stderr_file.writeStreamingAll(io, "Usage: strip_macho_exports <input-binary> <output-binary>\n") catch {};
        std.process.exit(2);
    }
    const input_path = args[1];
    const output_path = args[2];

    const contents = try std.Io.Dir.cwd().readFileAlloc(io, input_path, arena, .limited(max_binary_bytes));
    try std.Io.Dir.cwd().writeFile(io, .{
        .sub_path = output_path,
        .data = contents,
        .flags = .{ .permissions = .executable_file },
    });

    var diagnostic: DyldExportStrip.Diagnostic = .{};
    _ = DyldExportStrip.stripFile(io, gpa, arena, output_path, &diagnostic) catch |err| switch (err) {
        error.NonSystemDylib, error.RunPathEntry => |rejected| {
            const what = switch (rejected) {
                error.NonSystemDylib => "loads a dylib from outside macOS",
                error.RunPathEntry => "has a run-path search entry",
            };
            const message = try std.fmt.allocPrint(arena, "{s} {s}: {s}\nEvery library the roc CLI uses beyond the OS must be linked statically (see src/cli/macho/DyldExportStrip.zig).\n", .{ input_path, what, diagnostic.path });
            stderr_file.writeStreamingAll(io, message) catch {};
            std.process.exit(1);
        },
        else => |e| return e,
    };
}
