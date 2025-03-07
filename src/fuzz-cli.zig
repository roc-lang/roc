//! This is just a silly fuzz test to start getting the infra setup.
//! It shows the basic that other fuzz tests likely should build off of.
//!
//! Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
//!
//! To run:
//!  1. zig build fuzz-cli
//!  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/cli/ -o /tmp/cli-out/ zig-out/bin/fuzz-cli
//!
//! Other afl commands also available in `./zig-out/AFLplusplus/bin`

const std = @import("std");
const cli = @import("cli.zig");
const RocCmd = cli.RocCmd;
const RocOpt = cli.RocOpt;

/// Hook for AFL++ to initialize the fuzz test environment.
pub export fn zig_fuzz_init() void {}

/// Hook for AFL++ to run the fuzz test.
pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    // We reinitialize the gpa on every loop of the fuzzer.
    // This enables the gpa to do leak checking on each iteration.
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = general_purpose_allocator.deinit();
    }
    const allocator = general_purpose_allocator.allocator();

    // Convert the input buffer into what is expected by the test.
    const buf_slice = buf[0..@intCast(len)];
    if (debug) {
        std.debug.print("Args:\n==========\n{s}\n==========\n\n", .{buf_slice});
    }

    var args_list = std.ArrayList([]const u8).init(allocator);
    defer args_list.deinit();
    var it = std.mem.splitScalar(u8, buf_slice, ' ');
    while (it.next()) |param| {
        args_list.append(param) catch @panic("OOM");
    }

    const args = args_list.items;
    if (args.len <= 1) {
        return;
    }

    const cmd = args[1];

    if (RocCmd.parse(cmd)) |roc_command| {
        const parsed_opt = RocOpt.parse(args[2..]) catch return;
        const opt = parsed_opt.opt;
        const cmd_args = args[(2 + parsed_opt.next_index)..];

        // Sadly, there is not really something to verify here.
        // We just verify it doesn't crash.
        _ = roc_command;
        _ = opt;
        _ = cmd_args;
    }
}
