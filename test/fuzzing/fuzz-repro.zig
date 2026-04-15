//! Runner to enable reproducing fuzzing failures.
//!
//! The default persistent mode fuzzer is really efficient, but can't be run from the command line.
//! This runners makes a simple script to reproduce failures from the command line (stdin or file).

const std = @import("std");
const fuzz_test = @import("fuzz_test");

// TODO: add a func zig_pretty_print or something to dump the test case in a pretty printed format.
// For example, for intermediate IRs, would pretty print the input IR.
// Then make all the fuzzer implement it.
// Another option is just adding a debug flag that defaults to false that the repro script can turn on to print more.
// Also could just dump out a lot of state before panicking.

const MAX_SIZE = std.math.maxInt(u32);

// TODO: rethink this interface and make it simpler (probably with full cli arg passing and more flags).
const HELP =
    \\ This script will run a single iteration of a fuzz test.
    \\ It can read input in f ways:
    \\   1. With no args: reads from stdin
    \\   2. With file arg: reads from a file specified by the arg
    \\   3. With `-b/--base64`: uses the base64 encoded arg as the repro input
    \\
    \\ Add `-v/--verbose` to get a more verbose print out.
    \\
;

/// CLI entrypoint for fuzzing failure reproducer.
pub fn main(init: std.process.Init) !void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    var args_list: std.ArrayList([]const u8) = .empty;
    defer args_list.deinit(gpa);
    var args_iter = std.process.Args.Iterator.init(init.minimal.args);
    while (args_iter.next()) |arg| {
        try args_list.append(gpa, arg);
    }
    const args = args_list.items;

    var data: ?[]const u8 = null;
    var base64 = false;
    var verbose = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            verbose = true;
        } else if (std.mem.eql(u8, arg, "-b") or std.mem.eql(u8, arg, "--base64")) {
            base64 = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            std.debug.print(HELP, .{});
            std.process.exit(0);
        } else if (data == null) {
            data = arg;
        } else {
            std.debug.print("unexpected arg: '{s}'\n", .{arg});
            std.process.exit(1);
        }
    }
    if (data == null) {
        if (base64) {
            std.debug.print("Reading from stdin doesn't support base64\n", .{});
            std.process.exit(1);
        }
        // No input data, just read from stdin.
        std.debug.print("Reading bytes for repro from stdin\n", .{});
        var read_buf: [4096]u8 = undefined;
        var stdin_reader = std.Io.File.stdin().readerStreaming(init.io, &read_buf);
        var contents: std.ArrayList(u8) = .empty;
        defer contents.deinit(gpa);
        while (true) {
            const buf = contents.addManyAsSlice(gpa, 4096) catch return error.OutOfMemory;
            const n = stdin_reader.interface.readSliceShort(buf) catch break;
            if (n == 0) break;
            contents.shrinkRetainingCapacity(contents.items.len - 4096 + n);
        }
        const bytes = contents.items;

        fuzz_test.zig_fuzz_init();
        fuzz_test.zig_fuzz_test_inner(bytes.ptr, @intCast(bytes.len), verbose);
    } else if (base64) {
        // Read arg as base64 string.
        std.debug.print("Using bytes as base64 encoded repro: {s}\n", .{data.?});
        const decoded_size = try std.base64.standard.Decoder.calcSizeForSlice(data.?);
        const bytes = try gpa.alloc(u8, decoded_size);
        defer gpa.free(bytes);

        try std.base64.standard.Decoder.decode(bytes, data.?);
        fuzz_test.zig_fuzz_init();
        fuzz_test.zig_fuzz_test_inner(bytes.ptr, @intCast(bytes.len), verbose);
    } else {
        // Read file pointed to by arg.
        std.debug.print("Reading bytes for repro from {s}\n", .{data.?});
        const bytes = try std.Io.Dir.cwd().readFileAllocOptions(init.io, data.?, gpa, .limited(@intCast(MAX_SIZE)), std.mem.Alignment.of(u8), null);
        defer gpa.free(bytes);

        fuzz_test.zig_fuzz_init();
        fuzz_test.zig_fuzz_test_inner(bytes.ptr, @intCast(bytes.len), verbose);
    }
}
