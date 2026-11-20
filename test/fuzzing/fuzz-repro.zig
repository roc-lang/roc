//! Runner to enable reproducing fuzzing failures.
//!
//! The default persistent mode fuzzer is really efficient, but can't be run from the command line.
//! This runner reproduces failures of any fuzz target from the command line (stdin or file).

const std = @import("std");
const build_options = @import("build_options");

const targets = @import("targets.zig");

/// A fuzz target the runner can reproduce, named as its `fuzz-<name>.zig` file is.
const FuzzTarget = struct {
    name: []const u8,
    init: *const fn () void,
    run: *const fn (buf: [*]u8, len: isize, debug: bool) void,
};

const fuzz_targets = fuzz_targets: {
    const names = @typeInfo(targets).@"struct".decl_names;
    var list: [names.len]FuzzTarget = undefined;
    for (names, &list) |name, *fuzz_target| {
        const file = @field(targets, name);
        fuzz_target.* = .{ .name = name, .init = file.zig_fuzz_init, .run = file.zig_fuzz_test_inner };
    }
    const final = list;
    break :fuzz_targets final;
};

const MAX_SIZE = std.math.maxInt(u32);

const target_names = names: {
    var names: []const u8 = fuzz_targets[0].name;
    for (fuzz_targets[1..]) |fuzz_target| names = names ++ ", " ++ fuzz_target.name;
    break :names names;
};

// TODO: rethink this interface and make it simpler (probably with full cli arg passing and more flags).
const HELP =
    \\ Usage: repro <target> [-v] [-b] [input]
    \\
    \\ This runs a single iteration of the fuzz test <target>, one of:
    \\
++ "   " ++ target_names ++ "\n" ++
    \\
    \\ It can read input in 3 ways:
    \\   1. With no args: reads from stdin
    \\   2. With file arg: reads from a file specified by the arg
    \\   3. With `-b/--base64`: uses the base64 encoded arg as the repro input
    \\
    \\ Add `-v/--verbose` to get a more verbose print out.
    \\
;

/// CLI entrypoint for fuzzing failure reproducer.
pub fn main(init: std.process.Init) anyerror!void {
    var gpa_impl = std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer {
        _ = build_options.debugGpaOk(gpa_impl.deinit());
    }
    const gpa = gpa_impl.allocator();

    var args_list: std.ArrayList([]const u8) = .empty;
    defer args_list.deinit(gpa);
    var args_iter = try init.minimal.args.iterateAllocator(gpa);
    while (args_iter.next()) |arg| {
        try args_list.append(gpa, arg);
    }
    const args = args_list.items;

    var target: ?FuzzTarget = null;
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
        } else if (target == null) {
            target = for (fuzz_targets) |candidate| {
                if (std.mem.eql(u8, arg, candidate.name)) break candidate;
            } else {
                std.debug.print("unknown fuzz target: '{s}'\n\n" ++ HELP, .{arg});
                std.process.exit(1);
            };
        } else if (data == null) {
            data = arg;
        } else {
            std.debug.print("unexpected arg: '{s}'\n", .{arg});
            std.process.exit(1);
        }
    }
    const fuzz_test = target orelse {
        std.debug.print("missing fuzz target\n\n" ++ HELP, .{});
        std.process.exit(1);
    };

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

        fuzz_test.init();
        fuzz_test.run(bytes.ptr, @intCast(bytes.len), verbose);
    } else if (base64) {
        // Read arg as base64 string.
        std.debug.print("Using bytes as base64 encoded repro: {s}\n", .{data.?});
        const decoded_size = try std.base64.standard.Decoder.calcSizeForSlice(data.?);
        const bytes = try gpa.alloc(u8, decoded_size);
        defer gpa.free(bytes);

        try std.base64.standard.Decoder.decode(bytes, data.?);
        fuzz_test.init();
        fuzz_test.run(bytes.ptr, @intCast(bytes.len), verbose);
    } else {
        // Read file pointed to by arg.
        std.debug.print("Reading bytes for repro from {s}\n", .{data.?});
        const bytes = try std.Io.Dir.cwd().readFileAllocOptions(init.io, data.?, gpa, .limited(@intCast(MAX_SIZE)), std.mem.Alignment.of(u8), null);
        defer gpa.free(bytes);

        fuzz_test.init();
        fuzz_test.run(bytes.ptr, @intCast(bytes.len), verbose);
    }
}
