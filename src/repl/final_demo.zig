//! Final demonstration of the Roc REPL with real parsing and evaluation
//! This shows that redefinition works correctly, matching the Rust REPL behavior

const std = @import("std");
const repl_module = @import("repl_full.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    try stdout.print("Roc REPL - Final Demo\n", .{});
    try stdout.print("=====================\n\n", .{});
    try stdout.print("This demonstrates real parsing and evaluation with proper redefinition support.\n\n", .{});

    var repl = try repl_module.Repl.init(allocator);
    defer repl.deinit();

    // Example 1: Basic arithmetic
    try stdout.print("Example 1: Basic arithmetic\n", .{});
    try stdout.print("---------------------------\n", .{});
    try runExample(&repl, allocator, &[_][]const u8{
        "42",
        "1 + 2",
        "10 + 20",
    });

    // Example 2: Variable definitions and redefinition
    try stdout.print("\nExample 2: Variables and redefinition\n", .{});
    try stdout.print("-------------------------------------\n", .{});
    try runExample(&repl, allocator, &[_][]const u8{
        "x = 5",
        "x",
        "y = x + 1",
        "y",
        "x = 6", // Redefine x
        "x", // Should show 6
        "y", // Should show 7 (uses new x)
        "z = x + y", // z = 6 + 7
        "z", // Should show 13
    });

    // Example 3: Multiple redefinitions showing shadowing
    try stdout.print("\nExample 3: Multiple redefinitions\n", .{});
    try stdout.print("---------------------------------\n", .{});
    try runExample(&repl, allocator, &[_][]const u8{
        "a = 1",
        "a = a + 1",
        "a = a + 1",
        "a = a + 1",
        "a", // Should show 4
    });

    // Show final state
    try stdout.print("\nFinal REPL State:\n", .{});
    try stdout.print("-----------------\n", .{});
    try stdout.print("Total definitions: {}\n", .{repl.past_defs.items.len});

    var ident_count: usize = 0;
    var import_count: usize = 0;
    for (repl.past_defs.items) |def| {
        if (def.ident != null) {
            ident_count += 1;
        } else if (def.is_import) {
            import_count += 1;
        }
    }

    try stdout.print("  - Variable definitions: {}\n", .{ident_count});
    try stdout.print("  - Import statements: {}\n", .{import_count});

    try stdout.print("\nLast 10 definitions:\n", .{});
    const start = if (repl.past_defs.items.len > 10) repl.past_defs.items.len - 10 else 0;
    for (repl.past_defs.items[start..], start..) |def, i| {
        try stdout.print("  [{}] {s}\n", .{ i, def.source });
    }

    try stdout.print("\nKey insights:\n", .{});
    try stdout.print("-------------\n", .{});
    try stdout.print("1. Real parsing using parse.parseStatement() and parse.parseExpr()\n", .{});
    try stdout.print("2. Proper token resolution to extract identifiers\n", .{});
    try stdout.print("3. Simple arithmetic evaluation (integers and addition)\n", .{});
    try stdout.print("4. Variable lookups with proper shadowing\n", .{});
    try stdout.print("5. All definitions preserved in order\n", .{});
    try stdout.print("6. Later definitions shadow earlier ones naturally\n", .{});
    try stdout.print("7. NO SEGFAULTS! 🎉\n", .{});

    // Interactive mode prompt
    try stdout.print("\nEnter interactive mode? (y/n): ", .{});

    var buf: [10]u8 = undefined;
    if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |response| {
        if (response.len > 0 and (response[0] == 'y' or response[0] == 'Y')) {
            try runInteractive(&repl, allocator);
        }
    }
}

fn runExample(repl: *repl_module.Repl, allocator: std.mem.Allocator, inputs: []const []const u8) !void {
    const stdout = std.io.getStdOut().writer();

    for (inputs) |input| {
        try stdout.print("> {s}\n", .{input});
        const result = try repl.step(input);
        defer allocator.free(result);

        if (result.len > 0) {
            try stdout.print("{s}\n", .{result});
        }
    }
}

fn runInteractive(repl: *repl_module.Repl, allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    try stdout.print("\nInteractive REPL Mode\n", .{});
    try stdout.print("--------------------\n", .{});
    try stdout.print("Type expressions or assignments. Use :exit to quit.\n\n", .{});

    while (true) {
        try stdout.print("> ", .{});

        var buf: [1024]u8 = undefined;
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r\n");

            if (std.mem.eql(u8, trimmed, ":exit") or
                std.mem.eql(u8, trimmed, ":quit") or
                std.mem.eql(u8, trimmed, ":q"))
            {
                break;
            }

            if (trimmed.len == 0) continue;

            const result = try repl.step(trimmed);
            defer allocator.free(result);

            if (result.len > 0) {
                try stdout.print("{s}\n", .{result});
            }
        } else {
            // EOF
            break;
        }
    }

    try stdout.print("\nGoodbye!\n", .{});
}
