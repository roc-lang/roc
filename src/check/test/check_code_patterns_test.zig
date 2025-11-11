//! Tests that verify Check.zig follows certain code patterns and conventions.
//! These are compile-time/static checks on the source code itself.

const std = @import("std");
const testing = std.testing;

test "Check.zig should not call self.types.fresh directly" {
    // self.fresh and self.freshFromContent automatically add variables to the var pool,
    // but self.types.fresh (and its family) do NOT. Therefore, no code in Check.zig
    // should call self.types.fresh directly - it should always use self.fresh or
    // self.freshFromContent instead.

    const check_zig_source = @embedFile("../Check.zig");

    // Search for forbidden patterns
    const forbidden_patterns = [_][]const u8{
        "self.types.fresh(",
        "self.types.freshFromContent(",
    };

    const Violation = struct {
        pattern: []const u8,
        line_num: usize,
        line_content: []const u8,
    };
    var found_violations = try std.ArrayList(Violation).initCapacity(testing.allocator, 8);
    defer found_violations.deinit(testing.allocator);

    var line_iter = std.mem.splitScalar(u8, check_zig_source, '\n');
    var line_num: usize = 1;

    while (line_iter.next()) |line| : (line_num += 1) {
        // Skip comment lines
        const trimmed = std.mem.trim(u8, line, " \t");
        if (std.mem.startsWith(u8, trimmed, "//")) {
            continue;
        }

        // Check for forbidden patterns
        for (forbidden_patterns) |pattern| {
            if (std.mem.indexOf(u8, line, pattern)) |_| {
                try found_violations.append(testing.allocator, .{
                    .pattern = pattern,
                    .line_num = line_num,
                    .line_content = line,
                });
            }
        }
    }

    // Report violations if any were found
    if (found_violations.items.len > 0) {
        std.debug.print("\n\n", .{});
        std.debug.print("===============================================\n", .{});
        std.debug.print("ERROR: Found forbidden direct calls to self.types.fresh in Check.zig\n", .{});
        std.debug.print("===============================================\n\n", .{});
        std.debug.print("self.fresh and self.freshFromContent automatically add variables to the var pool,\n", .{});
        std.debug.print("but self.types.fresh (and family) do NOT. Use self.fresh or self.freshFromContent instead.\n\n", .{});

        for (found_violations.items) |violation| {
            std.debug.print("Line {}: Found '{s}'\n", .{ violation.line_num, violation.pattern });
            std.debug.print("  {s}\n\n", .{violation.line_content});
        }

        std.debug.print("Please replace these with self.fresh() or self.freshFromContent() instead.\n", .{});
        std.debug.print("===============================================\n\n", .{});

        return error.ForbiddenDirectCallToTypesFresh;
    }
}
