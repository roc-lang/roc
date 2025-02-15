const std = @import("std");
const Mark = @import("store.zig").Mark;
const Rank = @import("store.zig").Rank;
const Descriptor = @import("store.zig").Descriptor;
const Content = @import("store.zig").Content;
const UnificationTable = @import("store.zig").UnificationTable;

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

test "UnificationTable basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var table = try UnificationTable.init(allocator, 8);
    defer table.deinit();

    // Create some variables
    const var1 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const var2 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    // Test root keys
    try testing.expectEqual(var1, table.rootKey(var1));
    try testing.expectEqual(var2, table.rootKey(var2));

    // Unify variables
    table.unifyRoots(var1, var2, Descriptor.default());

    // Test unification worked
    try testing.expect(table.unioned(var1, var2));
}

test "UnificationTable advanced operations" {
    // Initialize table with space for 10 variables
    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();

    // Create four variables v1, v2, v3, v4 with default descriptors
    const desc = Descriptor.default();
    const v1 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v1.val = 0
    const v2 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v2.val = 1
    const v3 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v3.val = 2
    const v4 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v4.val = 3

    std.debug.print("\nInitial state: four independent variables\n", .{});
    std.debug.print("v1={}, v2={}, v3={}, v4={}\n", .{ v1.val, v2.val, v3.val, v4.val });

    // First unification: v1 <- v2
    // After this: v2 should point to v1 (v1 is root)
    std.debug.print("\nUnifying v1({}) <- v2({})\n", .{ v1.val, v2.val });
    try table.unify(v1, v2);
    try std.testing.expect(!table.isRedirect(v1)); // v1 should be root
    try std.testing.expect(table.isRedirect(v2)); // v2 should point to v1

    // Second unification: v2 <- v3
    // After this: v3 should point to v2, which points to v1 (v1 is root)
    std.debug.print("\nUnifying v2({}) <- v3({})\n", .{ v2.val, v3.val });
    try table.unify(v2, v3);
    try std.testing.expect(!table.isRedirect(v1)); // v1 should still be root
    try std.testing.expect(table.isRedirect(v2)); // v2 should still point to v1
    try std.testing.expect(table.isRedirect(v3)); // v3 should point to v2

    // Third unification: v3 <- v4
    // After this: v4 should point to v3, which points to v2, which points to v1 (v1 is root)
    std.debug.print("\nUnifying v3({}) <- v4({})\n", .{ v3.val, v4.val });
    try table.unify(v3, v4);
    try std.testing.expect(!table.isRedirect(v1)); // v1 should still be root
    try std.testing.expect(table.isRedirect(v2)); // v2 should still point to v1
    try std.testing.expect(table.isRedirect(v3)); // v3 should still point to v2
    try std.testing.expect(table.isRedirect(v4)); // v4 should point to v3

    // Final state should be: v4 -> v3 -> v2 -> v1
    // Where v1 is the root of the entire chain
    const root = table.rootKey(v1);
    try std.testing.expectEqual(v1, root); // v1 should be the root

    // Verify we get the same root regardless of which variable we start from
    try std.testing.expectEqual(v1, table.rootKey(v2));
    try std.testing.expectEqual(v1, table.rootKey(v3));
    try std.testing.expectEqual(v1, table.rootKey(v4));
}

test "UnificationTable redirect detection" {
    // Initialize table with space for 2 variables
    var table = try UnificationTable.init(std.testing.allocator, 2);
    defer table.deinit();

    // Create two variables v0, v1 with default descriptors
    const desc = Descriptor.default();
    const v0 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v0.val = 0
    const v1 = table.push(desc.content, desc.rank, desc.mark, desc.copy); // v1.val = 1

    std.debug.print("\nInitial state: two independent variables\n", .{});
    std.debug.print("v0={}, v1={}\n", .{ v0.val, v1.val });

    // Initially both variables should be roots (no redirects)
    try std.testing.expect(!table.isRedirect(v0)); // v0 starts as root
    try std.testing.expect(!table.isRedirect(v1)); // v1 starts as root

    // Unify v0 <- v1
    // After this: v1 should point to v0 (v0 is root)
    std.debug.print("\nUnifying v0({}) <- v1({})\n", .{ v0.val, v1.val });
    try table.unify(v0, v1);

    // Check final state
    try std.testing.expect(!table.isRedirect(v0)); // v0 should be root
    try std.testing.expect(table.isRedirect(v1)); // v1 should point to v0

    // Verify root finding
    const root = table.rootKey(v1);
    try std.testing.expectEqual(v0, root); // v0 should be the root
}

test "UnificationTable descriptor modification" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var table = try UnificationTable.init(allocator, 4);
    defer table.deinit();

    // Create variable with initial descriptor
    const var1 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    // Modify descriptor
    const new_desc = Descriptor.init(Content{ .FlexVar = null }, Rank.toplevel(), Mark.OCCURS, null);
    table.setDescriptor(var1, new_desc);

    // Verify modifications
    const retrieved_desc = table.getDescriptor(var1);
    try testing.expectEqual(new_desc.rank, retrieved_desc.rank);
    try testing.expectEqual(new_desc.mark, retrieved_desc.mark);
    try testing.expectEqual(new_desc.copy, retrieved_desc.copy);
}
