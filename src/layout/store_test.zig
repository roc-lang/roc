//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts

const std = @import("std");
const base = @import("../base.zig");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("layout.zig");
const Store = @import("store.zig").Store;
const Ident = @import("../base/Ident.zig");
const collections = @import("../collections.zig");
const target = @import("../base/target.zig");

test "addTypeVar - maximum nesting depth" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create deeply nested record structure
    const max_depth = 100;
    var current_var = type_store.freshFromContent(.{ .structure = .str });

    var depth: usize = 0;
    while (depth < max_depth) : (depth += 1) {
        const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("field"), base.Region.zero());
        var fields = std.ArrayList(types.RecordField).init(gpa);
        defer fields.deinit();

        try fields.append(.{
            .name = field_name,
            .var_ = current_var,
        });

        const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
        const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
        current_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });
    }

    // This should still work - we don't want arbitrary limits on nesting
    const result = try layout_store.addTypeVar(&type_store, current_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));
    try testing.expect(result_layout.* == .record);
}

test "addTypeVar - record with maximum fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create record with many fields
    const num_fields = 1000;
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    var i: usize = 0;
    while (i < num_fields) : (i += 1) {
        var name_buf: [20]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "field_{}", .{i}) catch unreachable;
        const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text(name_str), base.Region.zero());

        // Alternate between different types to test alignment sorting
        const field_var = if (i % 3 == 0)
            type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } })
        else if (i % 3 == 1)
            type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } })
        else
            type_store.freshFromContent(.{ .structure = .str });

        try fields.append(.{
            .name = field_name,
            .var_ = field_var,
        });
    }

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Should handle large number of fields
    const result = try layout_store.addTypeVar(&type_store, record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));

    switch (result_layout.*) {
        .record => |rec| {
            const record_fields = layout_store.record_fields.rangeToSlice(rec.getFields());
            try testing.expectEqual(num_fields, record_fields.len);

            // Verify fields are sorted by alignment then name for the target used to create the layout
            const target_usize = module_env.target.target_usize;
            var prev_alignment: ?std.mem.Alignment = null;
            var prev_name_in_group: ?[]const u8 = null;

            for (record_fields.items(.layout), record_fields.items(.name)) |field_layout_idx, field_name| {
                const field_layout = layout_store.layouts.get(@enumFromInt(field_layout_idx.idx));
                const field_alignment = field_layout.alignment(target_usize);
                const field_name_str = type_store.env.idents.getText(field_name);

                if (prev_alignment) |prev| {
                    // Alignment should be descending or equal
                    try testing.expect(field_alignment.toByteUnits() <= prev.toByteUnits());

                    // If same alignment, names should be ascending
                    if (field_alignment.toByteUnits() == prev.toByteUnits()) {
                        if (prev_name_in_group) |prev_name| {
                            try testing.expect(std.mem.order(u8, prev_name, field_name_str) == .lt);
                        }
                        prev_name_in_group = field_name_str;
                    } else {
                        prev_name_in_group = field_name_str;
                    }
                }

                prev_alignment = field_alignment;
            }
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record with very long field names" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create record with very long field names
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    const long_name_a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    const long_name_b = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";

    const field_name_a = type_store.env.idents.insert(gpa, base.Ident.for_text(long_name_a), base.Region.zero());
    const field_name_b = type_store.env.idents.insert(gpa, base.Ident.for_text(long_name_b), base.Region.zero());

    try fields.append(.{
        .name = field_name_a,
        .var_ = type_store.freshFromContent(.{ .structure = .str }),
    });
    try fields.append(.{
        .name = field_name_b,
        .var_ = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } }),
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Should handle long field names
    const result = try layout_store.addTypeVar(&type_store, record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));
    try testing.expect(result_layout.* == .record);
}

test "addTypeVar - alternating zero-sized and non-zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create record with alternating zero and non-zero sized fields
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    const num_fields = 20;
    var i: usize = 0;
    var expected_non_zero_count: usize = 0;

    while (i < num_fields) : (i += 1) {
        var name_buf: [20]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "field_{}", .{i}) catch unreachable;
        const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text(name_str), base.Region.zero());

        const field_var = if (i % 2 == 0) blk: {
            expected_non_zero_count += 1;
            break :blk type_store.freshFromContent(.{ .structure = .str });
        } else blk: {
            // Zero-sized: empty record
            break :blk type_store.freshFromContent(.{ .structure = .empty_record });
        };

        try fields.append(.{
            .name = field_name,
            .var_ = field_var,
        });
    }

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(&type_store, record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));

    switch (result_layout.*) {
        .record => |rec| {
            const record_fields = layout_store.record_fields.rangeToSlice(rec.getFields());
            // Only non-zero-sized fields should remain
            try testing.expectEqual(expected_non_zero_count, record_fields.len);
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record field type changes through alias" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an alias that points to a concrete type
    const backing_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    const alias_var = backing_var;

    // Create record using the alias
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("aliased"), base.Region.zero());
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = alias_var,
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(&type_store, record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));

    switch (result_layout.*) {
        .record => |rec| {
            const record_fields = layout_store.record_fields.rangeToSlice(rec.getFields());
            try testing.expectEqual(@as(usize, 1), record_fields.len);

            // The field should have the backing type's layout (u64)
            const field_layout_idx = record_fields.items(.layout)[0];
            const field_layout = layout_store.layouts.get(@enumFromInt(field_layout_idx.idx));
            try testing.expect(field_layout.* == .int);
            try testing.expect(field_layout.*.int == .u64);
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - mixed container types" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create complex nested structure: List(Box(Record { a: Str, b: List(U64) }))

    // Inner list of U64
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    const inner_list_var = type_store.freshFromContent(.{ .structure = .{ .list = u64_var } });

    // Record with two fields
    const field_a = type_store.env.idents.insert(gpa, base.Ident.for_text("a"), base.Region.zero());
    const field_b = type_store.env.idents.insert(gpa, base.Ident.for_text("b"), base.Region.zero());

    var record_fields = std.ArrayList(types.RecordField).init(gpa);
    defer record_fields.deinit();

    try record_fields.append(.{
        .name = field_a,
        .var_ = type_store.freshFromContent(.{ .structure = .str }),
    });
    try record_fields.append(.{
        .name = field_b,
        .var_ = inner_list_var,
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, record_fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Box the record
    const box_var = type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

    // List of boxes
    const outer_list_var = type_store.freshFromContent(.{ .structure = .{ .list = box_var } });

    // Should handle complex nesting
    const result = try layout_store.addTypeVar(&type_store, outer_list_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));

    // Verify it's a list
    try testing.expect(result_layout.* == .list);

    // Verify the inner structure
    const box_layout = layout_store.layouts.get(@enumFromInt(result_layout.list.idx));
    try testing.expect(box_layout.* == .box);

    const record_layout = layout_store.layouts.get(@enumFromInt(box_layout.*.box.idx));
    try testing.expect(record_layout.* == .record);

    const rec = switch (record_layout.*) {
        .record => |r| r,
        else => unreachable,
    };
    const rec_fields = layout_store.record_fields.rangeToSlice(rec.getFields());
    try testing.expectEqual(@as(usize, 2), rec_fields.len);

    // Fields should be sorted by alignment then name
    // Both str and list have pointer alignment, so should be sorted by name (a, b)
    const field_0_name = type_store.env.idents.getText(rec_fields.items(.name)[0]);
    const field_1_name = type_store.env.idents.getText(rec_fields.items(.name)[1]);
    try testing.expectEqualStrings("a", field_0_name);
    try testing.expectEqualStrings("b", field_1_name);

    // Verify field types
    const field_0_layout_idx = rec_fields.items(.layout)[0];
    const field_1_layout_idx = rec_fields.items(.layout)[1];
    const field_0_layout = layout_store.layouts.get(@enumFromInt(field_0_layout_idx.idx));
    const field_1_layout = layout_store.layouts.get(@enumFromInt(field_1_layout_idx.idx));
    try testing.expect(field_0_layout.* == .str);
    try testing.expect(field_1_layout.* == .list);
}

test "addTypeVar - record size calculation with padding" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create record that requires padding: { a: U8, b: U64, c: U16 }
    // Expected layout (sorted by alignment): b (u64), c (u16), a (u8)
    // Memory layout: [b: 8 bytes][c: 2 bytes][padding: 5 bytes][a: 1 byte] = 16 bytes total

    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    const field_a = type_store.env.idents.insert(gpa, base.Ident.for_text("a"), base.Region.zero());
    const field_b = type_store.env.idents.insert(gpa, base.Ident.for_text("b"), base.Region.zero());
    const field_c = type_store.env.idents.insert(gpa, base.Ident.for_text("c"), base.Region.zero());

    try fields.append(.{
        .name = field_a,
        .var_ = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } }),
    });
    try fields.append(.{
        .name = field_b,
        .var_ = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } }),
    });
    try fields.append(.{
        .name = field_c,
        .var_ = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u16 } } } }),
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(&type_store, record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.idx));

    switch (result_layout.*) {
        .record => |rec| {
            // Record should have 8-byte alignment (from u64)
            try testing.expectEqual(8, rec.alignment.toByteUnits());

            // Size should be 16 bytes (with padding)
            try testing.expectEqual(16, rec.size);

            // Verify field order
            const rec_fields = layout_store.record_fields.rangeToSlice(rec.getFields());
            try testing.expectEqual(3, rec_fields.len);

            // First field should be 'b' (u64, 8-byte alignment)
            const field_0_name = type_store.env.idents.getText(rec_fields.items(.name)[0]);
            try testing.expectEqualStrings("b", field_0_name);

            // Second field should be 'c' (u16, 2-byte alignment)
            const field_1_name = type_store.env.idents.getText(rec_fields.items(.name)[1]);
            try testing.expectEqualStrings("c", field_1_name);

            // Third field should be 'a' (u8, 1-byte alignment)
            const field_2_name = type_store.env.idents.getText(rec_fields.items(.name)[2]);
            try testing.expectEqualStrings("a", field_2_name);
        },
        else => try testing.expect(false),
    }
}
