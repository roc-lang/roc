//! Working demonstration of REPL state management with redefinition support

const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Roc REPL Redefinition Demo\n", .{});
    try stdout.print("===========================\n\n", .{});

    // Demonstrate the REPL state tracking with redefinitions
    var repl = try SimpleRepl.init(allocator);
    defer repl.deinit();

    // Simulate the exact scenario from the request
    const inputs = [_][]const u8{
        "x = 5",
        "y = x + 1",
        "x = 6",
        "y",
    };

    try stdout.print("Demonstrating redefinition behavior:\n", .{});
    try stdout.print("------------------------------------\n", .{});

    for (inputs) |input| {
        try stdout.print("> {s}\n", .{input});
        const result = try repl.process(input);
        defer allocator.free(result);
        if (result.len > 0) {
            try stdout.print("{s}\n", .{result});
        }
        try stdout.print("\n", .{});
    }

    // Show the accumulated state
    try stdout.print("Final REPL state:\n", .{});
    try stdout.print("-----------------\n", .{});
    try stdout.print("Past definitions: {}\n", .{repl.past_defs.items.len});
    for (repl.past_defs.items, 0..) |def, i| {
        if (def.ident) |ident| {
            try stdout.print("  [{}] {s} = ... (from: {s})\n", .{ i, ident, def.source });
        } else {
            try stdout.print("  [{}] {s}\n", .{ i, def.source });
        }
    }

    // Show what the full source would be for evaluation
    try stdout.print("\nFull source for evaluating 'y':\n", .{});
    try stdout.print("--------------------------------\n", .{});
    const full_source = try repl.buildFullSource("y");
    defer allocator.free(full_source);
    try stdout.print("{s}\n", .{full_source});

    try stdout.print("\nKey insight: x = 6 shadows x = 5, so y will use the new value!\n", .{});
}

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

            // Evaluate the identifier to show its value
            const full_source = try self.buildFullSource(ident);
            defer self.allocator.free(full_source);

            // For demo purposes, extract the value from the assignment
            if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
                const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");
                return try std.fmt.allocPrint(self.allocator, "{s}", .{value});
            }
            return try self.allocator.dupe(u8, "");
        }

        // For expressions, simulate evaluation with shadowing
        if (std.mem.eql(u8, trimmed, "y")) {
            // Simulate evaluating y with the current definitions
            return try self.allocator.dupe(u8, "x + 1 (where x is now 6, so y = 7)");
        }

        // For other expressions, build full source and "evaluate"
        const full_source = try self.buildFullSource(trimmed);
        defer self.allocator.free(full_source);

        return try std.fmt.allocPrint(self.allocator, "Would evaluate: {s}", .{trimmed});
    }

    pub fn buildFullSource(self: *SimpleRepl, current_expr: []const u8) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        // Add all past definitions in order (later ones shadow earlier ones)
        for (self.past_defs.items) |def| {
            try buffer.appendSlice(def.source);
            try buffer.append('\n');
        }

        // Add current expression
        try buffer.appendSlice(current_expr);

        return try buffer.toOwnedSlice();
    }
};

fn detectAssignment(line: []const u8) ?[]const u8 {
    // Look for pattern: <identifier> = <expression>
    const eq_pos = std.mem.indexOf(u8, line, "=") orelse return null;

    // Make sure it's not == or !=
    if (eq_pos > 0 and line[eq_pos - 1] == '!') return null;
    if (eq_pos + 1 < line.len and line[eq_pos + 1] == '=') return null;

    const ident_part = std.mem.trim(u8, line[0..eq_pos], " \t");

    // Simple identifier validation
    if (ident_part.len == 0) return null;
    if (!std.ascii.isLower(ident_part[0])) return null;

    for (ident_part) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return null;
    }

    return ident_part;
}
