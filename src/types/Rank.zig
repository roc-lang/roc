const std = @import("std");

/// Rank represents a hierarchical level in type checking and generalization.
/// It's used to track the scope level of type variables and ensure proper
/// type generalization. Each rank indicates a different scope or phase in
/// type checking.
const Rank = @This();

value: u32,

/// Special rank value used to mark variables that have been generalized.
/// This rank is reserved specifically for variables that have completed
/// the generalization process and is distinct from all other ranks used
/// in type checking.
pub const GENERALIZED: Rank = .{ .value = 0 };

/// Checks if this rank represents a generalized variable.
pub fn isGeneralized(self: Rank) bool {
    return std.meta.eql(self, GENERALIZED);
}

/// Creates a new Rank representing the top level scope.
/// This is the starting rank for type checking and represents
/// the outermost scope level before any imports or nested scopes.
pub fn toplevel() Rank {
    return .{ .value = 1 };
}

/// Creates a new Rank for handling imports (rank 2).
///
/// Type checking starts at rank 1 (toplevel). When there are rigid/flex variables
/// introduced by a constraint, these must be generalized relative to toplevel,
/// and hence are introduced at rank 2.
///
/// Even if there are no rigid imports, introducing at rank 2 is correct
/// (if slightly inefficient) because there are no rigids anyway so
/// generalization is trivial.
pub fn import() Rank {
    return .{ .value = 2 };
}

/// Returns a new Rank that is one level higher than the current rank.
/// Used when entering a new scope level during type checking or when
/// new variables need to be introduced at a deeper level.
pub fn next(self: Rank) Rank {
    return .{ .value = self.value + 1 };
}

/// Converts the rank to a usize value.
/// Useful for array indexing and other operations requiring usize.
pub fn intoUsize(self: Rank) usize {
    return @as(usize, self.value);
}

/// Implements formatting for Rank.
/// Displays the rank as a decimal number.
pub fn format(
    self: Rank,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try std.fmt.format(writer, "{d}", .{self.value});
}

/// Creates a new Rank from a usize value.
pub fn fromUsize(value: usize) Rank {
    return .{ .value = @as(u32, @intCast(value)) };
}

test "Rank" {
    // Test group for basic rank creation and comparison
    {
        // Test GENERALIZED constant
        const generalized = Rank.GENERALIZED;
        try std.testing.expect(generalized.value == 0);
        try std.testing.expect(generalized.isGeneralized());

        // Test toplevel rank
        const top_level = Rank.toplevel();
        try std.testing.expect(top_level.value == 1);
        try std.testing.expect(!top_level.isGeneralized());

        // Test import rank
        const import_rank = Rank.import();
        try std.testing.expect(import_rank.value == 2);
        try std.testing.expect(!import_rank.isGeneralized());
    }

    // Test group for rank progression
    {
        const r1 = Rank.toplevel();
        const r2 = r1.next();
        const r3 = r2.next();

        try std.testing.expect(r2.value == r1.value + 1);
        try std.testing.expect(r3.value == r2.value + 1);
        try std.testing.expect(!r2.isGeneralized());
        try std.testing.expect(!r3.isGeneralized());
    }

    // Test group for conversions
    {
        // Test usize conversion
        const rank = Rank{ .value = 42 };
        try std.testing.expect(rank.intoUsize() == 42);

        // Test fromUsize
        const converted = Rank.fromUsize(42);
        try std.testing.expect(converted.value == 42);

        // Test conversion roundtrip
        const original = Rank{ .value = 100 };
        const roundtrip = Rank.fromUsize(original.intoUsize());
        try std.testing.expect(original.value == roundtrip.value);
    }

    // Test group for formatting
    {
        const rank = Rank{ .value = 5 };
        var buf: [10]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        try rank.format("", .{}, fbs.writer());

        const result = fbs.getWritten();
        try std.testing.expectEqualStrings("5", result);
    }

    // Test group for relationships between different ranks
    {
        const generalized = Rank.GENERALIZED;
        const top_level = Rank.toplevel();
        const import_rank = Rank.import();

        // Verify expected relationships
        try std.testing.expect(generalized.value < top_level.value);
        try std.testing.expect(top_level.value < import_rank.value);
        try std.testing.expect(import_rank.value == top_level.value + 1);
    }
}
