//! Demo program to showcase REPL state functionality with real evaluation

const std = @import("std");
const repl_state = @import("repl_state.zig");
const eval = @import("eval.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var state = try repl_state.ReplState.init(allocator);
    defer state.deinit();

    var repl = try eval.Repl.init(allocator);
    defer repl.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Roc REPL Demo - Now with evaluation!\n", .{});
    try stdout.print("Type :help for help, :exit to quit\n\n", .{});

    while (true) {
        try stdout.print("> ", .{});

        var buf: [1024]u8 = undefined;
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const result = try repl.step(line);
            defer allocator.free(result);

            if (result.len > 0) {
                try stdout.print("{s}\n", .{result});
            }

            // Check if we should exit
            const trimmed = std.mem.trim(u8, line, " \t\n\r");
            if (std.mem.eql(u8, trimmed, ":exit") or
                std.mem.eql(u8, trimmed, ":quit") or
                std.mem.eql(u8, trimmed, ":q") or
                std.mem.eql(u8, trimmed, "exit") or
                std.mem.eql(u8, trimmed, "quit") or
                std.mem.eql(u8, trimmed, "exit()") or
                std.mem.eql(u8, trimmed, "quit()"))
            {
                break;
            }
        } else {
            // EOF
            break;
        }
    }
}
