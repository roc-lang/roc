//! Comprehensive example showing REPL redefinition behavior
//! This demonstrates that later definitions shadow earlier ones,
//! matching the behavior of the Rust REPL.

const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Roc REPL Redefinition Example\n", .{});
    try stdout.print("==============================\n\n", .{});

    var repl = try SimpleRepl.init(allocator);
    defer repl.deinit();

    // Example 1: Basic redefinition
    try stdout.print("Example 1: Basic redefinition\n", .{});
    try stdout.print("-----------------------------\n", .{});
    try runExample(&repl, &[_][]const u8{
        "x = 5",
        "x",
        "x = 10",
        "x",
    });

    // Reset for next example
    repl.deinit();
    repl = try SimpleRepl.init(allocator);

    // Example 2: Dependent values with redefinition
    try stdout.print("\nExample 2: Dependent values\n", .{});
    try stdout.print("---------------------------\n", .{});
    try runExample(&repl, &[_][]const u8{
        "x = 5",
        "y = x + 1",
        "y", // Should be 6
        "x = 10",
        "y", // Still 6 - y's definition hasn't changed
        "z = x + 1",
        "z", // Should be 11
    });

    // Reset for next example
    repl.deinit();
    repl = try SimpleRepl.init(allocator);

    // Example 3: Multiple redefinitions
    try stdout.print("\nExample 3: Multiple redefinitions\n", .{});
    try stdout.print("---------------------------------\n", .{});
    try runExample(&repl, &[_][]const u8{
        "counter = 0",
        "counter = counter + 1",
        "counter = counter + 1",
        "counter = counter + 1",
        "counter", // Shows how each definition builds on previous state
    });

    // Reset for next example
    repl.deinit();
    repl = try SimpleRepl.init(allocator);

    // Example 4: Shadowing with different types (conceptually)
    try stdout.print("\nExample 4: Type changes (conceptual)\n", .{});
    try stdout.print("------------------------------------\n", .{});
    try runExample(&repl, &[_][]const u8{
        "data = 42",
        "data",
        "data = \"hello\"",
        "data",
        "data = [1, 2, 3]",
        "data",
    });

    try stdout.print("\nKey Insights:\n", .{});
    try stdout.print("-------------\n", .{});
    try stdout.print("1. Later definitions shadow earlier ones\n", .{});
    try stdout.print("2. Existing dependent values keep their original definitions\n", .{});
    try stdout.print("3. The full source includes ALL definitions in order\n", .{});
    try stdout.print("4. This matches the behavior of the Rust REPL implementation\n", .{});
}

fn runExample(repl: *SimpleRepl, inputs: []const []const u8) !void {
    const stdout = std.io.getStdOut().writer();

    for (inputs) |input| {
        try stdout.print("> {s}\n", .{input});
        const result = try repl.process(input);
        defer repl.allocator.free(result);

        if (result.len > 0) {
            try stdout.print("{s}\n", .{result});
        }
    }

    // Show the accumulated state
    try stdout.print("\nCurrent state ({} definitions):\n", .{repl.past_defs.items.len});
    for (repl.past_defs.items, 0..) |def, i| {
        try stdout.print("  [{}] {s}\n", .{ i, def.source });
    }
}

// Simple REPL implementation for demonstration
const PastDef = struct {
    source: []const u8,
    ident: ?[]const u8,

    pub fn deinit(self: *PastDef, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
        if (self.ident) |ident| {
            allocator.free(ident);
        }
    }
};

const SimpleRepl = struct {
    allocator: std.mem.Allocator,
    past_defs: std.ArrayList(PastDef),

    pub fn init(allocator: std.mem.Allocator) !SimpleRepl {
        return SimpleRepl{
            .allocator = allocator,
            .past_defs = std.ArrayList(PastDef).init(allocator),
        };
    }

    pub fn deinit(self: *SimpleRepl) void {
        for (self.past_defs.items) |*def| {
            def.deinit(self.allocator);
        }
        self.past_defs.deinit();
    }

    pub fn process(self: *SimpleRepl, input: []const u8) ![]const u8 {
        const trimmed = std.mem.trim(u8, input, " \t\n\r");

        // Simple pattern matching for assignments
        if (detectAssignment(trimmed)) |ident| {
            // Store the definition (allows redefinition)
            try self.past_defs.append(.{
                .source = try self.allocator.dupe(u8, trimmed),
                .ident = try self.allocator.dupe(u8, ident),
            });

            // Return the value being assigned
            if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
                const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");
                return try self.allocator.dupe(u8, value);
            }
            return try self.allocator.dupe(u8, "");
        }

        // For simple identifiers, show their last definition
        if (isSimpleIdentifier(trimmed)) {
            var i = self.past_defs.items.len;
            while (i > 0) {
                i -= 1;
                const def = self.past_defs.items[i];
                if (def.ident) |ident| {
                    if (std.mem.eql(u8, ident, trimmed)) {
                        if (std.mem.indexOf(u8, def.source, "=")) |eq_pos| {
                            const value = std.mem.trim(u8, def.source[eq_pos + 1 ..], " \t");
                            return try std.fmt.allocPrint(self.allocator, "{s} (from definition #{d})", .{ value, i });
                        }
                    }
                }
            }
            return try std.fmt.allocPrint(self.allocator, "<undefined: {s}>", .{trimmed});
        }

        // For other expressions
        return try std.fmt.allocPrint(self.allocator, "<expression: {s}>", .{trimmed});
    }
};

fn detectAssignment(line: []const u8) ?[]const u8 {
    const eq_pos = std.mem.indexOf(u8, line, "=") orelse return null;

    // Make sure it's not == or !=
    if (eq_pos > 0 and line[eq_pos - 1] == '!') return null;
    if (eq_pos + 1 < line.len and line[eq_pos + 1] == '=') return null;

    const ident_part = std.mem.trim(u8, line[0..eq_pos], " \t");

    if (ident_part.len == 0) return null;
    if (!isSimpleIdentifier(ident_part)) return null;

    return ident_part;
}

fn isSimpleIdentifier(text: []const u8) bool {
    if (text.len == 0) return false;
    if (!std.ascii.isLower(text[0])) return false;

    for (text) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    }

    return true;
}
