/// Runner to enable reproducing fuzzing failures.
///
/// The default persistent mode fuzzer is really efficient, but can't be run from the command line.
/// This runners makes a simple script to reproduce failures from the command line (stdin or file).
const std = @import("std");

// TODO: add an extern func zig_pretty_print or something to dump the test case in a pretty printed format.
// For example, for intermediate IRs, would pretty print the input IR.
// Then make all the fuzzer implement it.

extern fn zig_fuzz_init() void;

extern fn zig_fuzz_test(buf: [*]u8, len: isize) void;

const MAX_SIZE = std.math.maxInt(u32);

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len == 1) {
        // By default just read from stdin.
        std.debug.print("Reading bytes for repro from stdin\n", .{});
        const bytes = try std.io.getStdIn().readToEndAlloc(gpa, @intCast(MAX_SIZE));
        defer gpa.free(bytes);

        zig_fuzz_init();
        zig_fuzz_test(bytes.ptr, @intCast(bytes.len));
    } else if (args.len == 2) {
        if (std.mem.eql(u8, args[1], "-h") or std.mem.eql(u8, args[1], "--help")) {
            // Special case for help message.
            std.debug.print(
                \\ This script will run a single iteration of a fuzz test.
                \\ It can read input in 3 ways:
                \\   1. With no args: reads from stdin
                \\   2. With 1 arg: reads from a file specified by the arg
                \\   3. With N arg: uses the commandline args as the repro input
            , .{});
            return;
        }
        // Use the file passed in as the source.
        const path = args[1];
        std.debug.print("Reading bytes for repro from {s}\n", .{path});
        const file = try std.fs.cwd().openFile(path, .{});
        const bytes = try file.readToEndAlloc(gpa, @intCast(MAX_SIZE));
        defer gpa.free(bytes);

        zig_fuzz_init();
        zig_fuzz_test(bytes.ptr, @intCast(bytes.len));
    } else {
        // If many args are passed in, use them as the fuzz input.
        std.debug.print("Using commandline args as bytes for repro\n", .{});
        var args_str = std.ArrayList(u8).init(gpa);
        defer args_str.deinit();

        var w = args_str.writer();
        for (args, 0..) |arg, i| {
            try w.writeAll(arg);
            if (i != args.len - 1) {
                try w.writeByte(' ');
            }
        }

        zig_fuzz_init();
        zig_fuzz_test(args_str.items.ptr, @intCast(args_str.items.len));
    }
}
