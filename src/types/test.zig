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
    try std.testing.expectEqualStrings("NONE", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.VISITED_IN_OCCURS_CHECK});
    try std.testing.expectEqualStrings("VISITED_IN_OCCURS_CHECK", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.OCCURS});
    try std.testing.expectEqualStrings("OCCURS", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{Mark.GET_VAR_NAMES});
    try std.testing.expectEqualStrings("GET_VAR_NAMES", fbs.getWritten());
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

test "unification table - basic" {
    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_basic_operations.snap", .{});
    defer file.close();

    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();
    table.debug_capture = file.writer();

    table.debugLog("INFO: Create two flex variables\n", .{});
    const v1 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const v2 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    table.debugPrint();

    table.debugLog("INFO: Check they are not unified\n", .{});
    try std.testing.expect(!table.unioned(v1, v2));

    table.debugLog("INFO: Unify them\n", .{});
    try table.unify(v1, v2);

    table.debugLog("INFO: Check they are now unified\n", .{});
    try std.testing.expect(table.unioned(v1, v2));
    try std.testing.expect(table.rootKey(v1).val == table.rootKey(v2).val);

    table.debugPrint();
}

test "unification table - advanced" {
    var table = try UnificationTable.init(std.testing.allocator, 4);
    defer table.deinit();

    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_advanced_operations.snap", .{});
    defer file.close();
    table.debug_capture = file.writer();

    table.debugLog("INFO: Create four variables with default descriptors\n", .{});
    const desc = Descriptor.default();
    const alpha = table.push(desc.content, desc.rank, desc.mark, desc.copy);
    const beta = table.push(desc.content, desc.rank, desc.mark, desc.copy);
    const gamma = table.push(desc.content, desc.rank, desc.mark, desc.copy);
    const delta = table.push(desc.content, desc.rank, desc.mark, desc.copy);

    table.debugPrint();

    try table.unify(alpha, beta);
    try std.testing.expect(!table.isRedirect(alpha));
    try std.testing.expect(table.isRedirect(beta));

    try table.unify(beta, gamma);
    try std.testing.expect(!table.isRedirect(alpha));
    try std.testing.expect(table.isRedirect(beta));
    try std.testing.expect(table.isRedirect(gamma));

    try table.unify(gamma, delta);
    try std.testing.expect(!table.isRedirect(alpha));
    try std.testing.expect(table.isRedirect(beta));
    try std.testing.expect(table.isRedirect(gamma));
    try std.testing.expect(table.isRedirect(delta));

    // Final state should be: delta -> gamma -> bravo -> alpha
    // Where alpha is the root of the entire chain
    const root = table.rootKey(alpha);
    try std.testing.expectEqual(alpha, root);

    // Verify we get the same root regardless of which variable we start from
    try std.testing.expectEqual(alpha, table.rootKey(beta));
    try std.testing.expectEqual(alpha, table.rootKey(gamma));
    try std.testing.expectEqual(alpha, table.rootKey(delta));

    table.debugPrint();
}

test "unification table - redirect" {
    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_redirect.snap", .{});
    defer file.close();

    const allocator = std.testing.allocator;
    var table = try UnificationTable.init(allocator, 3);
    defer table.deinit();

    table.debug_capture = file.writer();

    const alpha = table.push(Content{ .FlexVar = null }, Rank.toplevel(), Mark.NONE, null);
    const beta = table.push(Content{ .FlexVar = null }, Rank.toplevel(), Mark.NONE, null);

    table.debugPrint();

    table.debugLog("INFO: Perform unification, redirect {} to {}\n", .{ beta, alpha });
    try table.unify(alpha, beta);

    try std.testing.expect(!table.isRedirect(alpha));
    try std.testing.expect(table.isRedirect(beta));

    table.debugPrint();
}

test "unification table - descriptor" {
    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_descriptor_modification.snap", .{});
    defer file.close();

    var table = try UnificationTable.init(std.testing.allocator, 4);
    defer table.deinit();

    table.debug_capture = file.writer();

    // Create variable with initial descriptor
    const start = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    table.debugPrint();

    table.debugLog("INFO: Modify descriptor change Mark from {} to {}\n", .{ Mark.NONE, Mark.OCCURS });
    table.debugLog("INFO: Changing Mark from {} to {}\n", .{ Mark.NONE, Mark.OCCURS });
    table.debugLog("INFO: Changing Rank from {} to {}\n", .{ Rank.GENERALIZED, Rank.toplevel() });

    const expected = Descriptor.init(Content{ .FlexVar = null }, Rank.toplevel(), Mark.OCCURS, null);
    table.setDescriptor(start, expected);

    const finish = table.getDescriptor(start);
    try std.testing.expectEqual(expected.rank, finish.rank);
    try std.testing.expectEqual(expected.mark, finish.mark);
    try std.testing.expectEqual(expected.copy, finish.copy);

    table.debugPrint();
}

test "unification table - transitive" {
    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();

    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_transitive.snap", .{});
    defer file.close();
    table.debug_capture = file.writer();

    const alpha = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const beta = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const gamma = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    table.debugPrint();

    try std.testing.expect(!table.unioned(alpha, beta));
    try std.testing.expect(!table.unioned(beta, gamma));
    try std.testing.expect(!table.unioned(alpha, gamma));

    try table.unify(alpha, beta);
    try table.unify(beta, gamma);

    // All three should now be unified
    try std.testing.expect(table.unioned(alpha, beta));
    try std.testing.expect(table.unioned(beta, gamma));
    try std.testing.expect(table.unioned(alpha, gamma));

    // All should have the same root
    const root = table.rootKey(alpha);
    try std.testing.expect(table.rootKey(beta).val == root.val);
    try std.testing.expect(table.rootKey(gamma).val == root.val);

    table.debugPrint();
}

test "unification table - path compression" {
    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();

    const file = try std.fs.cwd().createFile("src/types/snapshots/unification_table_compression.snap", .{});
    defer file.close();
    table.debug_capture = file.writer();

    const v1 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const v2 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const v3 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const v4 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    table.debugPrint();

    // Create a chain: v4 -> v3 -> v2 -> v1
    try table.unify(v1, v2);
    try table.unify(v2, v3);
    try table.unify(v3, v4);

    // Force path compression by getting root
    _ = table.rootKey(v4);

    // After path compression, all variables should point directly to root
    try std.testing.expect(table.entries[v2.val].parent.?.val == v1.val);
    try std.testing.expect(table.entries[v3.val].parent.?.val == v1.val);
    try std.testing.expect(table.entries[v4.val].parent.?.val == v1.val);

    table.debugPrint();
}

test "unification table - occurs check" {
    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();

    const v1 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    const v2 = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    // Basic occurs check should return false initially
    try std.testing.expect(!try table.occurs(v1, v2));

    // After unification, occurs check should detect the relationship
    try table.unify(v1, v2);
    try std.testing.expect(try table.occurs(v1, v2));
}

test "unification table - rank handling" {
    var table = try UnificationTable.init(std.testing.allocator, 10);
    defer table.deinit();

    const rank1 = Rank{ .value = 1 };
    const rank2 = Rank{ .value = 2 };

    const v1 = table.push(Content{ .FlexVar = null }, rank1, Mark.NONE, null);
    const v2 = table.push(Content{ .FlexVar = null }, rank2, Mark.NONE, null);

    // Check initial ranks
    try std.testing.expect(std.meta.eql(table.getRank(v1), rank1));
    try std.testing.expect(std.meta.eql(table.getRank(v2), rank2));

    // After unification, variables should share the same rank
    try table.unify(v1, v2);
    const unified_rank = table.getRank(v1);
    try std.testing.expect(std.meta.eql(table.getRank(v2), unified_rank));
}

test "unification table - capacity management" {
    var table = try UnificationTable.init(std.testing.allocator, 2);
    defer table.deinit();

    // Initial capacity is 2
    try std.testing.expect(table.entries.len == 2);

    // Add variables until we exceed capacity
    _ = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);
    _ = table.push(Content{ .FlexVar = null }, Rank.GENERALIZED, Mark.NONE, null);

    // This should trigger a capacity increase
    try table.reserve(1);
    try std.testing.expect(table.entries.len > 2);
}
