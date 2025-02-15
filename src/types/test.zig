const std = @import("std");
const Mark = @import("store.zig").Mark;
const Rank = @import("store.zig").Rank;
const Descriptor = @import("store.zig").Descriptor;
const Content = @import("store.zig").Content;

test "Mark constants have correct values" {
    try std.testing.expectEqual(@as(i32, 0), Mark.GET_VAR_NAMES.value);
    try std.testing.expectEqual(@as(i32, 1), Mark.OCCURS.value);
    try std.testing.expectEqual(@as(i32, 2), Mark.VISITED_IN_OCCURS_CHECK.value);
    try std.testing.expectEqual(@as(i32, 3), Mark.NONE.value);
}

test "Mark.next increments value" {
    const mark = Mark{ .value = 1 };
    const next_mark = mark.next();
    try std.testing.expectEqual(@as(i32, 2), next_mark.value);
}

test "Mark equality" {
    const mark1 = Mark{ .value = 1 };
    const mark2 = Mark{ .value = 1 };
    const mark3 = Mark{ .value = 2 };

    try std.testing.expect(mark1.eql(mark2));
    try std.testing.expect(!mark1.eql(mark3));
}

test "Mark formatting" {
    var buf: [50]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    // Test all constant values
    try std.fmt.format(writer, "{}", .{Mark.NONE});
    try std.testing.expectEqualStrings("none", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.VISITED_IN_OCCURS_CHECK});
    try std.testing.expectEqualStrings("visited_in_occurs_check", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.OCCURS});
    try std.testing.expectEqualStrings("occurs", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.GET_VAR_NAMES});
    try std.testing.expectEqualStrings("get_var_names", fbs.getWritten());
    fbs.reset();

    // Test non-constant value
    const custom_mark = Mark{ .value = 42 };
    try std.fmt.format(writer, "{}", .{custom_mark});
    try std.testing.expectEqualStrings("Mark(42)", fbs.getWritten());
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

test "Descriptor basics" {
    // Test creation from content only
    {
        const content = Content{ .FlexVar = null };
        const desc = Descriptor.fromContent(content);

        try std.testing.expect(std.meta.eql(desc.rank, Rank.GENERALIZED));
        try std.testing.expect(std.meta.eql(desc.mark, Mark.NONE));
        try std.testing.expect(std.meta.eql(desc.copy, null));
    }

    // Test full initialization
    {
        const content = Content{ .FlexVar = null };
        const rank = Rank.toplevel();
        const mark = Mark.NONE;
        const copy = null;

        const desc = Descriptor.init(content, rank, mark, copy);

        try std.testing.expect(std.meta.eql(desc.rank, rank));
        try std.testing.expect(std.meta.eql(desc.mark, mark));
        try std.testing.expect(std.meta.eql(desc.copy, copy));
    }

    // Test default descriptor
    {
        const desc = Descriptor.default();
        try std.testing.expect(std.meta.eql(desc.rank, Rank.GENERALIZED));
        try std.testing.expect(std.meta.eql(desc.mark, Mark.NONE));
        try std.testing.expect(std.meta.eql(desc.copy, null));
    }

    // Test formatting
    {
        const desc = Descriptor.default();
        var buf: [100]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        try desc.format("", .{}, fbs.writer());

        // Note: Exact string comparison depends on Content's format implementation
        const written = fbs.getWritten();
        try std.testing.expect(written.len > 0);
    }
}
