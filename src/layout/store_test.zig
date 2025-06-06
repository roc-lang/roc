//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts

const std = @import("std");
const base = @import("../base.zig");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("layout.zig");
const layout_store_ = @import("store.zig");
const Ident = @import("../base/Ident.zig");
const collections = @import("../collections.zig");
const target = @import("../base/target.zig");

const LayoutError = layout_store_.LayoutError;
const Store = layout_store_.Store;

test "addTypeVar - str" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Convert to layout
    const str_layout_idx = try layout_store.addTypeVar(str_var);

    // Verify the layout
    const str_layout = layout_store.getLayout(str_layout_idx);
    try testing.expect(str_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.str, str_layout.data.scalar.tag);
}

test "addTypeVar - bool" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a bool layout directly (since we don't have bool types in the type system yet)
    const bool_layout = layout.Layout.booleanType();
    const bool_layout_idx = try layout_store.insertLayout(bool_layout);

    // Verify the layout
    const retrieved_layout = layout_store.getLayout(bool_layout_idx);
    try testing.expect(retrieved_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.bool, retrieved_layout.data.scalar.tag);
    try testing.expectEqual(@as(u32, 1), layout_store.layoutSize(retrieved_layout.*));
}

test "addTypeVar - list of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Create a list of str type variable
    const list_str_var = type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_str_var);

    // Verify the layout - List(Str) should use scalar optimization
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list_of_scalar);

    // Verify the scalar data
    try testing.expectEqual(layout.ScalarTag.str, list_layout.data.list_of_scalar.tag);
    try testing.expectEqual({}, list_layout.data.list_of_scalar.data.str);
}

test "addTypeVar - list of box of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Create a box of str type variable
    const box_str_var = type_store.freshFromContent(.{ .structure = .{ .box = str_var } });

    // Create a list of box of str type variable
    const list_box_str_var = type_store.freshFromContent(.{ .structure = .{ .list = box_str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_box_str_var);

    // Verify the layout - List(Box(Str)) should use index since Box(Str) is not a scalar
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the outer list element (Box(Str)) - Box(Str) should use scalar optimization
    const box_layout = layout_store.getLayout(list_layout.data.list);
    try testing.expect(box_layout.tag == .box_of_scalar);

    // Verify the scalar data
    try testing.expectEqual(layout.ScalarTag.str, box_layout.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_layout.data.box_of_scalar.data.str);
}

test "addTypeVar - box of flex_var compiles to box of host_opaque" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex_var type variable
    const flex_var = type_store.freshFromContent(.{ .flex_var = null });

    // Create a box of flex_var type variable
    const box_flex_var = type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_flex_var);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_scalar);

    // Verify the element is host_opaque
    try testing.expectEqual(layout.ScalarTag.host_opaque, box_layout.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_layout.data.box_of_scalar.data.host_opaque);
}

test "addTypeVar - num u32" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a u32 type variable
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Convert to layout
    const u32_layout_idx = try layout_store.addTypeVar(u32_var);

    // Verify the layout
    const u32_layout = layout_store.getLayout(u32_layout_idx);
    try testing.expect(u32_layout.tag == .scalar);
    try testing.expect(u32_layout.data.scalar.data.int == .u32);
}

test "addTypeVar - num f64" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a f64 type variable
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });

    // Convert to layout
    const f64_layout_idx = try layout_store.addTypeVar(f64_var);

    // Verify the layout
    const f64_layout = layout_store.getLayout(f64_layout_idx);
    try testing.expect(f64_layout.tag == .scalar);
    try testing.expect(f64_layout.data.scalar.data.frac == .f64);
}

test "addTypeVar - list of num i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an i128 type variable
    const i128_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i128 } } } });

    // Create a list of i128 type variable
    const list_i128_var = type_store.freshFromContent(.{ .structure = .{ .list = i128_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_i128_var);

    // Verify the layout - List(I128) should use scalar optimization
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list_of_scalar);

    // Verify the scalar data
    try testing.expectEqual(layout.ScalarTag.int, list_layout.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i128, list_layout.data.list_of_scalar.data.int);
}

test "addTypeVar - num dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a dec type variable
    const dec_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .dec } } } });

    // Convert to layout
    const dec_layout_idx = try layout_store.addTypeVar(dec_var);

    // Verify the layout
    const num_dec_layout = layout_store.getLayout(dec_layout_idx);
    try testing.expect(num_dec_layout.tag == .scalar);
    try testing.expect(num_dec_layout.data.scalar.data.frac == .dec);
}

test "addTypeVar - flex num var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex number type variable (Num(a))
    const flex_num_var = type_store.freshFromContent(.{ .structure = .{ .num = .flex_var } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(flex_num_var);

    // Verify the layout
    const num_layout = layout_store.getLayout(layout_idx);
    try testing.expect(num_layout.tag == .scalar);
    try testing.expect(num_layout.data.scalar.data.int == .i128);
}

test "addTypeVar - flex int var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex int type variable (Int(a))
    const flex_int_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .flex_var } } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(flex_int_var);

    // Verify the layout
    const int_i64_layout = layout_store.getLayout(layout_idx);
    try testing.expect(int_i64_layout.tag == .scalar);
    try testing.expect(int_i64_layout.data.scalar.data.int == .i128);
}

test "addTypeVar - flex frac var defaults to dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    const flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .flex_var } } });

    // Convert to layout - should default to dec
    const layout_idx = try layout_store.addTypeVar(flex_frac_var);

    // Verify the layout
    const frac_f64_layout = layout_store.getLayout(layout_idx);
    try testing.expect(frac_f64_layout.tag == .scalar);
    try testing.expect(frac_f64_layout.data.scalar.data.frac == .dec);
}

test "addTypeVar - list of flex num var defaults to list of i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex num type variable (Num(a))
    const flex_num_var = type_store.freshFromContent(.{ .structure = .{ .num = .flex_var } });

    // Create a list of flex num type variable
    const list_flex_num_var = type_store.freshFromContent(.{ .structure = .{ .list = flex_num_var } });

    // Convert to layout - should default to list of i128
    const list_layout_idx = try layout_store.addTypeVar(list_flex_num_var);

    // Verify the layout - List(FlexVar) should use scalar optimization defaulting to I128
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list_of_scalar);

    // Verify the scalar data
    try testing.expectEqual(layout.ScalarTag.int, list_layout.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i128, list_layout.data.list_of_scalar.data.int);
}

test "addTypeVar - box of flex frac var defaults to box of dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    const flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .flex_var } } });

    // Create a box of flex frac type variable
    const box_flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .box = flex_frac_var } });

    // Convert to layout - should default to box of dec
    const box_layout_idx = try layout_store.addTypeVar(box_flex_frac_var);

    // Verify the layout - Box(FlexFrac) should use scalar optimization defaulting to Dec
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_scalar);

    // Verify the scalar data
    try testing.expectEqual(layout.ScalarTag.frac, box_layout.data.box_of_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.dec, box_layout.data.box_of_scalar.data.frac);
}

test "addTypeVar - box of rigid_var compiles to box of host_opaque" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an ident for the rigid var
    const ident_idx = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());

    // Create a rigid_var type variable
    const rigid_var = type_store.freshFromContent(.{ .rigid_var = ident_idx });

    // Create a box of rigid_var type variable
    const box_rigid_var = type_store.freshFromContent(.{ .structure = .{ .box = rigid_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_rigid_var);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_scalar);

    // Verify the element is host_opaque
    try testing.expectEqual(layout.ScalarTag.host_opaque, box_layout.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_layout.data.box_of_scalar.data.host_opaque);
}

test "addTypeVar - box of empty record compiles to box_of_zst" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    // Create a box of empty record type variable
    const box_empty_record_var = type_store.freshFromContent(.{ .structure = .{ .box = empty_record_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_empty_record_var);

    // Verify the layout is box_of_zst
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_zst);
}

test "addTypeVar - list of empty tag union compiles to list_of_zst" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Create a list of empty tag union type variable
    const list_empty_tag_union_var = type_store.freshFromContent(.{ .structure = .{ .list = empty_tag_union_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_empty_tag_union_var);

    // Verify the layout is list_of_zst
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list_of_zst);
}

test "alignment - record alignment is max of field alignments" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create field identifiers
    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());
    const field3_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field3"), base.Region.zero());

    // Create type variables for fields
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create record type { field1: U8, field2: U32, field3: U64 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
        .{ .name = field3_ident, .var_ = u64_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    // Test alignment calculation
    // Record alignment should be the max of its field alignments
    // U8 has alignment 1, U32 has alignment 4, U64 has alignment 8
    // So the record should have alignment 8
    for (target.TargetUsize.all()) |target_usize| {
        const alignment = record_layout.alignment(target_usize);
        try testing.expectEqual(@as(u32, 8), alignment.toByteUnits());
    }

    // Test with different field order - alignment should still be the same
    const fields2 = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field3_ident, .var_ = u64_var },
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
    });

    const ext2 = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var2 = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields2, .ext = ext2 } } });

    const record_layout_idx2 = try layout_store.addTypeVar(record_var2);
    const record_layout2 = layout_store.getLayout(record_layout_idx2);

    for (target.TargetUsize.all()) |target_usize| {
        const alignment2 = record_layout2.alignment(target_usize);
        try testing.expectEqual(@as(u32, 8), alignment2.toByteUnits());
    }
}

test "alignment - deeply nested record alignment (non-recursive)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create field identifiers
    const inner_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("inner"), base.Region.zero());
    const middle_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("middle"), base.Region.zero());
    const outer_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("outer"), base.Region.zero());
    const data_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("data"), base.Region.zero());

    // Create a U64 field (alignment 8)
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create innermost record: { data: U64 }
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = data_ident, .var_ = u64_var },
    });
    const inner_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = inner_ext } } });

    // Create middle record: { inner: { data: U64 } }
    const middle_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = inner_ident, .var_ = inner_record_var },
    });
    const middle_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const middle_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = middle_ext } } });

    // Create outer record: { middle: { inner: { data: U64 } } }
    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = middle_ident, .var_ = middle_record_var },
    });
    const outer_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = outer_ext } } });

    // Create outermost record: { outer: { middle: { inner: { data: U64 } } } }
    const outermost_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = outer_ident, .var_ = outer_record_var },
    });
    const outermost_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const outermost_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outermost_fields, .ext = outermost_ext } } });

    // Convert to layout
    const outermost_layout_idx = try layout_store.addTypeVar(outermost_record_var);
    const outermost_layout = layout_store.getLayout(outermost_layout_idx);

    // Test alignment calculation
    // The deeply nested record should still have alignment 8 (from the U64 field)
    for (target.TargetUsize.all()) |target_usize| {
        const alignment = outermost_layout.alignment(target_usize);
        try testing.expectEqual(@as(u32, 8), alignment.toByteUnits());
    }
}

test "addTypeVar - bare empty record returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(empty_record_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - bare empty tag union returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(empty_tag_union_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - simple record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create field types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Create field identifiers
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const age_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("age"), base.Region.zero());

    // Create record type { name: str, age: u32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = age_ident, .var_ = u32_var },
    });

    // Create record type
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify the fields are sorted by alignment then name
    // Both str and u32 have same alignment on 64-bit systems (8 bytes for str pointer, 4 bytes for u32 but u32 comes first due to smaller alignment)
    // Actually str has alignment of usize (8 on 64-bit), u32 has alignment 4
    // So str should come first (higher alignment), then u32
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // First field: name (str) - higher alignment (8 bytes on 64-bit)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.tag == .scalar);

    // Second field: age (u32) - lower alignment (4 bytes)
    const age_field = field_slice.get(1);
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.tag == .scalar);
    try testing.expect(age_layout.data.scalar.data.int == .u32);

    // Only 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "record size calculation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test record with multiple fields requiring padding
    // { a: u8, b: u32, c: u8, d: u64 }
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());
    const c_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero());
    const d_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero());

    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u8_var },
        .{ .name = b_ident, .var_ = u32_var },
        .{ .name = c_ident, .var_ = u8_var },
        .{ .name = d_ident, .var_ = u64_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // After sorting by alignment then name:
    // d: u64 (8 bytes) at offset 0
    // b: u32 (4 bytes) at offset 8
    // a: u8 (1 byte) at offset 12
    // c: u8 (1 byte) at offset 13
    // Total: 14 bytes, but aligned to 8 bytes = 16 bytes
    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(@as(u32, 16), layout_store.layoutSize(record_layout.*));
        try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
    }
}

test "addTypeVar - nested record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create inner record type { x: i32, y: i32 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());

    const point_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const point_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = point_fields, .ext = empty_ext } } });

    // Create outer record type { name: Str, position: { x: i32, y: i32 } }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const position_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("position"), base.Region.zero());

    const player_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = position_ident, .var_ = point_var },
    });

    const player_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = player_fields, .ext = empty_ext } } });

    // Convert to layout
    const player_layout_idx = try layout_store.addTypeVar(player_var);

    // Verify the outer layout
    const player_layout = layout_store.getLayout(player_layout_idx);
    try testing.expect(player_layout.tag == .record);

    // Verify the outer fields
    const player_record_data = layout_store.getRecordData(player_layout.data.record.idx);
    const outer_field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(player_record_data.fields.start), .end = @enumFromInt(player_record_data.fields.start + player_record_data.fields.count) });

    // First field: name (str)
    const name_field = outer_field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.tag == .scalar);

    // Second field: position (record)
    const position_field = outer_field_slice.get(1);
    try testing.expect(position_field.name == position_ident);
    const position_layout = layout_store.getLayout(position_field.layout);
    try testing.expect(position_layout.tag == .record);

    // Exactly 2 outer fields
    try testing.expectEqual(@as(usize, 2), outer_field_slice.len);

    // Verify the inner record fields
    const position_record_data = layout_store.getRecordData(position_layout.data.record.idx);
    const inner_field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(position_record_data.fields.start), .end = @enumFromInt(position_record_data.fields.start + position_record_data.fields.count) });

    // Inner field x (i32)
    const x_field = inner_field_slice.get(0);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.tag == .scalar);
    try testing.expect(x_layout.data.scalar.data.int == .i32);

    // Inner field y (i32)
    const y_field = inner_field_slice.get(1);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.tag == .scalar);
    try testing.expect(y_layout.data.scalar.data.int == .i32);

    // Exactly 2 inner fields
    try testing.expectEqual(@as(usize, 2), inner_field_slice.len);
}

test "addTypeVar - list of records" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create record type { id: u64, active: bool }
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    // For bool, we'll use u8 as a placeholder
    const bool_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const id_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("id"), base.Region.zero());
    const active_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("active"), base.Region.zero());

    const record_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = id_ident, .var_ = u64_var },
        .{ .name = active_ident, .var_ = bool_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = record_fields, .ext = empty_ext } } });

    // Create list of records
    const list_record_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_record_var);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the record element
    const record_layout = layout_store.getLayout(list_layout.data.list);
    try testing.expect(record_layout.tag == .record);

    // Verify the record fields
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // First field: id (u64)
    const id_field = field_slice.get(0);
    try testing.expect(id_field.name == id_ident);
    const id_layout = layout_store.getLayout(id_field.layout);
    try testing.expect(id_layout.tag == .scalar);
    try testing.expect(id_layout.data.scalar.data.int == .u64);

    // The bool field is actually a u8
    const active_field = field_slice.get(1);
    try testing.expect(active_field.name == active_ident);
    const active_field_layout = layout_store.getLayout(active_field.layout);
    try testing.expect(active_field_layout.tag == .scalar);
    try testing.expect(active_field_layout.data.scalar.data.int == .u8);

    // Exactly 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with extension" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create extension record { y: i32, z: f64 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());
    const z_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("z"), base.Region.zero());

    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = i32_var },
        .{ .name = z_ident, .var_ = f64_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have all 3 fields (x from main, y and z from extension)
    const record_data = layout_store.getRecordData(record_layout.data.record.idx);
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_data.fields.start), .end = @enumFromInt(record_data.fields.start + record_data.fields.count) });
    try testing.expectEqual(@as(usize, 3), field_slice.len);

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str and f64 are 8-byte aligned, i32 is 4-byte aligned
    // So order should be: x (str, 8-byte), z (f64, 8-byte), y (i32, 4-byte)

    // Field x (str)
    const x_field = field_slice.get(0);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.tag == .scalar);

    // Field z (f64) - comes before y due to alignment
    const z_field = field_slice.get(1);
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.tag == .scalar);
    try testing.expect(z_layout.data.scalar.data.frac == .f64);

    // Field y (i32) - comes last due to smaller alignment
    const y_field = field_slice.get(2);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.tag == .scalar);
    try testing.expect(y_layout.data.scalar.data.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - record extension with str type fails" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a record with str as extension (invalid)
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("field"), base.Region.zero());
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = type_store.freshFromContent(.{ .structure = .str }),
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const str_ext = type_store.freshFromContent(.{ .structure = .str });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = str_ext } } });

    const result = layout_store.addTypeVar(record_var);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - record extension with num type fails" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a record with number as extension (invalid)
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("field"), base.Region.zero());
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = type_store.freshFromContent(.{ .structure = .str }),
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const num_ext = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = num_ext } } });

    const result = layout_store.addTypeVar(record_var);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - deeply nested containers with zero-sized inner type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create List(Box(List(Box(empty_record))))
    const empty_record = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_box = type_store.freshFromContent(.{ .structure = .{ .box = empty_record } });
    const inner_list = type_store.freshFromContent(.{ .structure = .{ .list = inner_box } });
    const outer_box = type_store.freshFromContent(.{ .structure = .{ .box = inner_list } });
    const outer_list_var = type_store.freshFromContent(.{ .structure = .{ .list = outer_box } });

    const result = try layout_store.addTypeVar(outer_list_var);
    const result_layout = layout_store.getLayout(result);

    // Should resolve to List(Box(List(Box(empty_record))))
    // Outer list
    try testing.expect(result_layout.tag == .list);

    // Outer box
    const outer_box_layout = layout_store.getLayout(result_layout.data.list);
    try testing.expect(outer_box_layout.tag == .box);

    // Inner list
    const inner_list_layout = layout_store.getLayout(outer_box_layout.data.box);
    try testing.expect(inner_list_layout.tag == .list);

    // Inner box (should be box_of_zst since the innermost type is empty record)
    const inner_box_layout = layout_store.getLayout(inner_list_layout.data.list);
    try testing.expect(inner_box_layout.tag == .box_of_zst);
}

test "addTypeVar - record with single zero-sized field in container" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create List({ only_field: {} })
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("only_field"), base.Region.zero());
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = type_store.freshFromContent(.{ .structure = .empty_record }),
    });

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });
    const list_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    const result = try layout_store.addTypeVar(list_var);
    const result_layout = layout_store.getLayout(result);

    // List of empty record should be list_of_zst
    try testing.expect(result_layout.tag == .list_of_zst);
}

test "addTypeVar - record field ordering stability" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create multiple records with same fields but different order
    const field_a = type_store.env.idents.insert(gpa, base.Ident.for_text("aaa"), base.Region.zero());
    const field_b = type_store.env.idents.insert(gpa, base.Ident.for_text("bbb"), base.Region.zero());
    const field_c = type_store.env.idents.insert(gpa, base.Ident.for_text("ccc"), base.Region.zero());

    // All fields have same type (same alignment)
    const field_type = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Record 1: a, b, c
    var fields1 = std.ArrayList(types.RecordField).init(gpa);
    defer fields1.deinit();
    try fields1.append(.{ .name = field_a, .var_ = field_type });
    try fields1.append(.{ .name = field_b, .var_ = field_type });
    try fields1.append(.{ .name = field_c, .var_ = field_type });

    // Record 2: c, a, b
    var fields2 = std.ArrayList(types.RecordField).init(gpa);
    defer fields2.deinit();
    try fields2.append(.{ .name = field_c, .var_ = field_type });
    try fields2.append(.{ .name = field_a, .var_ = field_type });
    try fields2.append(.{ .name = field_b, .var_ = field_type });

    // Record 3: b, c, a
    var fields3 = std.ArrayList(types.RecordField).init(gpa);
    defer fields3.deinit();
    try fields3.append(.{ .name = field_b, .var_ = field_type });
    try fields3.append(.{ .name = field_c, .var_ = field_type });
    try fields3.append(.{ .name = field_a, .var_ = field_type });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });

    const fields1_slice = type_store.record_fields.appendSlice(gpa, fields1.items);
    const record1_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields1_slice, .ext = empty_ext } } });

    const fields2_slice = type_store.record_fields.appendSlice(gpa, fields2.items);
    const record2_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields2_slice, .ext = empty_ext } } });

    const fields3_slice = type_store.record_fields.appendSlice(gpa, fields3.items);
    const record3_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields3_slice, .ext = empty_ext } } });

    const result1 = try layout_store.addTypeVar(record1_var);
    const result2 = try layout_store.addTypeVar(record2_var);
    const result3 = try layout_store.addTypeVar(record3_var);

    const layout1 = layout_store.getLayout(result1);
    const layout2 = layout_store.getLayout(result2);
    const layout3 = layout_store.getLayout(result3);

    // All should produce records with fields in same order (sorted by name: aaa, bbb, ccc)
    switch (layout1.tag) {
        .record => {
            const fields_1 = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(layout1.data.record.idx).getFields());

            switch (layout2.tag) {
                .record => {
                    const fields_2 = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(layout2.data.record.idx).getFields());

                    switch (layout3.tag) {
                        .record => {
                            const fields_3 = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(layout3.data.record.idx).getFields());

                            // All should have 3 fields
                            try testing.expectEqual(@as(usize, 3), fields_1.len);
                            try testing.expectEqual(@as(usize, 3), fields_2.len);
                            try testing.expectEqual(@as(usize, 3), fields_3.len);

                            // All should have same field order
                            var i: usize = 0;
                            while (i < 3) : (i += 1) {
                                const name1 = type_store.env.idents.getText(fields_1.items(.name)[i]);
                                const name2 = type_store.env.idents.getText(fields_2.items(.name)[i]);
                                const name3 = type_store.env.idents.getText(fields_3.items(.name)[i]);

                                try testing.expectEqualStrings(name1, name2);
                                try testing.expectEqualStrings(name2, name3);
                            }

                            // Verify correct alphabetical order
                            try testing.expectEqualStrings("aaa", type_store.env.idents.getText(fields_1.items(.name)[0]));
                            try testing.expectEqualStrings("bbb", type_store.env.idents.getText(fields_1.items(.name)[1]));
                            try testing.expectEqualStrings("ccc", type_store.env.idents.getText(fields_1.items(.name)[2]));
                        },
                        else => try testing.expect(false),
                    }
                },
                else => try testing.expect(false),
            }
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - empty record in different contexts" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test 1: Bare empty record
    const empty_record = type_store.freshFromContent(.{ .structure = .empty_record });
    const result1 = layout_store.addTypeVar(empty_record);
    try testing.expectError(LayoutError.ZeroSizedType, result1);

    // Test 2: Box of empty record
    const box_empty = type_store.freshFromContent(.{ .structure = .{ .box = empty_record } });
    const result2 = try layout_store.addTypeVar(box_empty);
    const result2_layout = layout_store.getLayout(result2);
    try testing.expect(result2_layout.tag == .box_of_zst);

    // Test 3: List of empty record
    const list_empty = type_store.freshFromContent(.{ .structure = .{ .list = empty_record } });
    const result3 = try layout_store.addTypeVar(list_empty);
    const result3_layout = layout_store.getLayout(result3);
    try testing.expect(result3_layout.tag == .list_of_zst);

    // Test 4: Record containing only empty record field
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("empty"), base.Region.zero());
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();
    try fields.append(.{
        .name = field_name,
        .var_ = empty_record,
    });
    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_with_empty = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result4 = layout_store.addTypeVar(record_with_empty);
    try testing.expectError(LayoutError.ZeroSizedType, result4);
}

test "addTypeVar - record alignment edge cases" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create record with fields of all different alignments
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    // Add fields with different alignments (1, 2, 4, 8, 16 bytes)
    const alignments = [_]struct { name: []const u8, type: types.FlatType }{
        .{ .name = "align1", .type = .{ .num = .{ .int = .{ .exact = .u8 } } } }, // 1-byte alignment
        .{ .name = "align2", .type = .{ .num = .{ .int = .{ .exact = .u16 } } } }, // 2-byte alignment
        .{ .name = "align4", .type = .{ .num = .{ .int = .{ .exact = .u32 } } } }, // 4-byte alignment
        .{ .name = "align8", .type = .{ .num = .{ .int = .{ .exact = .u64 } } } }, // 8-byte alignment
        .{ .name = "align16", .type = .{ .num = .{ .int = .{ .exact = .u128 } } } }, // 16-byte alignment
    };

    for (alignments) |field_info| {
        const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text(field_info.name), base.Region.zero());
        try fields.append(.{
            .name = field_name,
            .var_ = type_store.freshFromContent(.{ .structure = field_info.type }),
        });
    }

    const fields_slice = type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.getLayout(result);
    switch (result_layout.tag) {
        .record => {
            // Record should have 16-byte alignment (maximum of all fields)
            try testing.expectEqual(@as(u32, 16), result_layout.data.record.alignment.toByteUnits());

            // Fields should be sorted by alignment (descending) then name
            const rec_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 5), rec_fields.len);

            // Verify order: align16, align8, align4, align2, align1
            const expected_order = [_][]const u8{ "align16", "align8", "align4", "align2", "align1" };
            for (expected_order, 0..) |expected_name, i| {
                const actual_name = type_store.env.idents.getText(rec_fields.items(.name)[i]);
                try testing.expectEqualStrings(expected_name, actual_name);
            }
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record with duplicate field in extension (matching types)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());

    // Create extension record { x: str, y: i32 }
    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with same type)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - should succeed since types match
    const record_layout_idx = try layout_store.addTypeVar(main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have 3 fields (x appears twice - from main and extension, plus y from extension)
    // TODO: Field deduplication should happen at the type-checking level, not in layout generation
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // All fields here have same type alignment, so sorted by name: x, x, y

    // Field x (str) - first occurrence
    const x_field1 = field_slice.get(0);
    try testing.expect(x_field1.name == x_ident);
    const x_layout1 = layout_store.getLayout(x_field1.layout);
    try testing.expect(x_layout1.tag == .scalar);

    // Field x (str) - second occurrence
    const x_field2 = field_slice.get(1);
    try testing.expect(x_field2.name == x_ident);
    const x_layout2 = layout_store.getLayout(x_field2.layout);
    try testing.expect(x_layout2.tag == .scalar);

    // Field y (i32)
    const y_field = field_slice.get(2);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.tag == .scalar);
    try testing.expect(y_layout.data.scalar.data.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - record with duplicate field in extension (mismatched types)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    // Create extension record { x: i32 }
    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with different types)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - currently succeeds with both fields present
    // TODO: Type checking should catch duplicate fields with mismatched types before layout generation
    const record_layout_idx = try layout_store.addTypeVar(main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // We get both fields even though they have the same name but different types
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str is 8-byte aligned, i32 is 4-byte aligned
    // So order should be: x (str, 8-byte), x (i32, 4-byte)

    // Field x (str) - from main record
    const x_field1 = field_slice.get(0);
    try testing.expect(x_field1.name == x_ident);
    const x_layout1 = layout_store.getLayout(x_field1.layout);
    try testing.expect(x_layout1.tag == .scalar);

    // Field x (i32) - from extension
    const x_field2 = field_slice.get(1);
    try testing.expect(x_field2.name == x_ident);
    const x_layout2 = layout_store.getLayout(x_field2.layout);
    try testing.expect(x_layout2.tag == .scalar);
    try testing.expect(x_layout2.data.scalar.data.int == .i32);

    // Exactly 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with invalid extension type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a str type to use as invalid extension
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    // Create main record { x: str } with str as extension (invalid)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = str_var } } });

    // Convert to layout - should fail due to invalid extension
    const result = layout_store.addTypeVar(main_record_var);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - record with chained extensions" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });

    const w_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("w"), base.Region.zero());
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());
    const z_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("z"), base.Region.zero());

    // Create innermost extension record { z: u8 }
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = z_ident, .var_ = u8_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create middle extension record { y: f64 } extending inner
    const middle_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = f64_var },
    });

    const middle_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = inner_record_var } } });

    // Create outermost record { w: str, x: i32 } extending middle
    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = w_ident, .var_ = str_var },
        .{ .name = x_ident, .var_ = i32_var },
    });

    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = middle_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(outer_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have all 4 fields from all levels
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str and f64 are 8-byte aligned, i32 is 4-byte aligned, u8 is 1-byte aligned
    // So order should be: w (str, 8-byte), y (f64, 8-byte), x (i32, 4-byte), z (u8, 1-byte)

    // Field w (str)
    const w_field = field_slice.get(0);
    try testing.expect(w_field.name == w_ident);
    const w_layout = layout_store.getLayout(w_field.layout);
    try testing.expect(w_layout.tag == .scalar);

    // Field y (f64) - comes before x due to alignment
    const y_field = field_slice.get(1);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.tag == .scalar);
    try testing.expect(y_layout.data.scalar.data.frac == .f64);

    // Field x (i32)
    const x_field = field_slice.get(2);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.tag == .scalar);
    try testing.expect(x_layout.data.scalar.data.int == .i32);

    // Field z (u8)
    const z_field = field_slice.get(3);
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.tag == .scalar);
    try testing.expect(z_layout.data.scalar.data.int == .u8);

    // Exactly 4 fields
    try testing.expectEqual(@as(usize, 4), field_slice.len);
}

test "addTypeVar - record with zero-sized fields dropped" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });

    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const empty_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("empty"), base.Region.zero());
    const age_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("age"), base.Region.zero());

    // Create record { name: str, empty: {}, age: i32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = empty_ident, .var_ = empty_record_var },
        .{ .name = age_ident, .var_ = i32_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we only have 2 fields (empty field should be dropped)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // Debug: Check the actual field count
    const field_count = field_slice.len;
    try testing.expect(field_count == 2);

    // Field name (str)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.tag == .scalar);

    // Second field: age (i32)
    const age_field = field_slice.get(1);
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.tag == .scalar);
    try testing.expect(age_layout.data.scalar.data.int == .i32);

    // Exactly 2 fields (empty field was dropped)
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with all zero-sized fields becomes empty" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());

    // Create record { field1: {}, field2: [] }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_tag_union_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout - should fail because all fields are zero-sized
    const result = layout_store.addTypeVar(record_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - box of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());

    // Create record { field1: {}, field2: {} }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_record_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create box of this record
    const box_record_var = type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

    // Convert to layout - should become box_of_zst
    const box_layout_idx = try layout_store.addTypeVar(box_record_var);

    // Verify the layout is box_of_zst
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_zst);
}

test "addTypeVar - comprehensive nested record combinations" {
    // This tests:
    // 1. 1344 different combinations (64 + 256 + 1024) of nested record structures
    // 2. Proper dropping of of zero-sized fields at all nesting levels
    // 3. Proper layout generation for fields that aren't zero-sized

    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create field names we'll reuse
    const field_names = [_]Ident.Idx{
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("e"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("f"), base.Region.zero()),
    };

    // Test all combinations
    var outer_field_count: usize = 1;
    while (outer_field_count <= 3) : (outer_field_count += 1) {
        // Generate all possible field type combinations for outer record
        var field_type_combo: usize = 0;
        const max_combo = std.math.pow(usize, 4, outer_field_count); // 4 possibilities per field

        while (field_type_combo < max_combo) : (field_type_combo += 1) {
            // Create a new type store and layout store for each test
            var test_type_store = types_store.Store.init(&module_env);
            defer test_type_store.deinit();

            var test_layout_store = Store.init(&module_env, &test_type_store);
            defer test_layout_store.deinit();

            // Build outer record fields
            var outer_fields = std.ArrayList(types.RecordField).init(gpa);
            defer outer_fields.deinit();

            var expected_non_zero_fields: usize = 0;
            var expected_total_fields: usize = 0;

            var field_idx: usize = 0;
            while (field_idx < outer_field_count) : (field_idx += 1) {
                // Determine field type based on combination
                const field_type_idx = (field_type_combo / std.math.pow(usize, 4, field_idx)) % 4;

                const field_var = switch (field_type_idx) {
                    0 => blk: {
                        // Non-record: str
                        expected_non_zero_fields += 1;
                        expected_total_fields += 1;
                        break :blk test_type_store.freshFromContent(.{ .structure = .str });
                    },
                    1 => blk: {
                        // Empty record (0 fields)
                        expected_total_fields += 1;
                        // This field will be dropped as zero-sized
                        break :blk test_type_store.freshFromContent(.{ .structure = .empty_record });
                    },
                    2 => blk: {
                        // Record with 1-2 non-zero fields
                        expected_total_fields += 1;
                        const inner_field_count = 1 + (field_idx % 2); // 1 or 2 fields
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        var inner_idx: usize = 0;
                        while (inner_idx < inner_field_count) : (inner_idx += 1) {
                            const inner_var = test_type_store.freshFromContent(.{ .structure = .str });
                            try inner_fields.append(.{
                                .name = field_names[3 + inner_idx],
                                .var_ = inner_var,
                            });
                        }

                        expected_non_zero_fields += 1; // The nested record itself counts as 1
                        expected_total_fields += inner_field_count;

                        const inner_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
                        const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    3 => blk: {
                        // Record with mix of zero and non-zero fields
                        expected_total_fields += 1;
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        // Add one empty record field (will be dropped)
                        const empty_record_var = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        try inner_fields.append(.{
                            .name = field_names[3],
                            .var_ = empty_record_var,
                        });
                        expected_total_fields += 1;

                        // Add one str field (will be kept)
                        const str_var = test_type_store.freshFromContent(.{ .structure = .str });
                        try inner_fields.append(.{
                            .name = field_names[4],
                            .var_ = str_var,
                        });
                        expected_total_fields += 1;

                        // This nested record will have 1 non-zero field (the str field) after dropping zero-sized ones
                        expected_non_zero_fields += 1;

                        const inner_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
                        const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    else => unreachable,
                };

                try outer_fields.append(.{
                    .name = field_names[field_idx],
                    .var_ = field_var,
                });
            }

            // Create outer record
            const outer_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, outer_fields.items);
            const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
            const outer_record_var = test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields_slice, .ext = empty_ext } } });

            // Convert to layout
            const result = test_layout_store.addTypeVar(outer_record_var) catch |err| {
                if (err == LayoutError.ZeroSizedType) {
                    // This is expected if all fields were zero-sized
                    try testing.expect(expected_non_zero_fields == 0);
                    continue;
                }
                return err;
            };

            // Verify the result
            const result_layout = test_layout_store.getLayout(result);

            if (expected_non_zero_fields == 0) {
                // Should have returned an error, not reached here
                try testing.expect(false);
            } else {
                try testing.expect(result_layout.tag == .record);

                // Count actual non-zero fields in the result
                const result_record_data = test_layout_store.getRecordData(result_layout.data.record.idx);
                const field_slice = test_layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(result_record_data.fields.start), .end = @enumFromInt(result_record_data.fields.start + result_record_data.fields.count) });
                const actual_field_count = field_slice.len;

                // Verify each field has a valid layout
                for (0..field_slice.len) |i| {
                    const field = field_slice.get(i);
                    const field_layout = test_layout_store.getLayout(field.layout);
                    switch (field_layout.tag) {
                        .scalar => {}, // Valid non-zero field
                        .record => {
                            // Verify nested record has fields
                            const nested_record_data = test_layout_store.getRecordData(field_layout.data.record.idx);
                            const nested_slice = test_layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(nested_record_data.fields.start), .end = @enumFromInt(nested_record_data.fields.start + nested_record_data.fields.count) });
                            try testing.expect(nested_slice.len > 0);
                        },
                        else => {
                            // Unexpected layout type
                            try testing.expect(false);
                        },
                    }
                }

                try testing.expect(actual_field_count == expected_non_zero_fields);
            }
        }
    }
}

test "addTypeVar - nested record with inner record having all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create inner record with only zero-sized fields
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());

    // Create inner record
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = empty_record_var },
        .{ .name = b_ident, .var_ = empty_record_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create outer record { name: str, data: inner_record }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const data_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("data"), base.Region.zero());

    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = data_ident, .var_ = inner_record_var },
    });

    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(outer_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we only have 1 field (data field should be dropped because inner record is empty)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // Field name (str)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.tag == .scalar);

    // Only 1 field (data field was dropped because the inner record was empty)
    try testing.expectEqual(@as(usize, 1), field_slice.len);
}

test "addTypeVar - list of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create empty record type
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());

    // Create record { field: {} }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field_ident, .var_ = empty_record_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create list of that record
    const list_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout - should be list_of_zst
    const list_layout_idx = try layout_store.addTypeVar(list_var);
    const list_layout = layout_store.getLayout(list_layout_idx);

    try testing.expect(list_layout.tag == .list_of_zst);
}

test "alignment - record with log2 alignment representation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test 1: Record with U8 field (alignment 1, log2 = 0)
    {
        const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u8_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.tag == .record);
        try testing.expectEqual(1, record_layout.data.record.alignment.toByteUnits());

        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 1), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 1), layout_store.layoutSize(record_layout.*)); // size = 1 byte
        }
    }

    // Test 2: Record with U32 field (alignment 4, log2 = 2)
    {
        const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u32_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.tag == .record);
        try testing.expectEqual(@as(u32, 4), record_layout.data.record.alignment.toByteUnits()); // alignment = 4

        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 4), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 4), layout_store.layoutSize(record_layout.*)); // size = 4 bytes
        }
    }

    // Test 3: Record with U64 field (alignment 8, log2 = 3)
    {
        const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u64_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.tag == .record);
        try testing.expectEqual(@as(u32, 8), record_layout.data.record.alignment.toByteUnits()); // alignment = 8

        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 8), layout_store.layoutSize(record_layout.*)); // size = 8 bytes
        }
    }

    // Test 4: Record with mixed fields - should use max alignment
    {
        const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
        const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
        const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("small"), base.Region.zero());
        const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("large"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field1_ident, .var_ = u8_var },
            .{ .name = field2_ident, .var_ = u64_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.tag == .record);
        try testing.expectEqual(@as(u32, 8), record_layout.data.record.alignment.toByteUnits()); // max alignment = 8

        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
            // After sorting: u64 (8 bytes) at offset 0, u8 (1 byte) at offset 8, total size 16 (aligned to 8)
            try testing.expectEqual(@as(u32, 16), layout_store.layoutSize(record_layout.*));
        }
    }
}

test "record fields sorted by alignment then name" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types with different alignments
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create field names that would sort differently alphabetically
    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());
    const c_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero());
    const d_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero());

    // Create record with fields in a specific order to test sorting
    // { a: u32, b: u64, c: u8, d: u64 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u32_var }, // alignment 4
        .{ .name = b_ident, .var_ = u64_var }, // alignment 8
        .{ .name = c_ident, .var_ = u8_var }, // alignment 1
        .{ .name = d_ident, .var_ = u64_var }, // alignment 8
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // Verify fields are sorted by alignment (descending) then by name (ascending)
    // Expected order: b (u64, align 8), d (u64, align 8), a (u32, align 4), c (u8, align 1)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // First field: b (u64, alignment 8)
    const field1 = field_slice.get(0);
    try testing.expect(field1.name == b_ident);
    const layout1 = layout_store.getLayout(field1.layout);
    try testing.expect(layout1.tag == .scalar);
    try testing.expect(layout1.data.scalar.data.int == .u64);

    // Second field: d (u64, alignment 8)
    const field2 = field_slice.get(1);
    try testing.expect(field2.name == d_ident);
    const layout2 = layout_store.getLayout(field2.layout);
    try testing.expect(layout2.tag == .scalar);
    try testing.expect(layout2.data.scalar.data.int == .u64);

    // Third field: a (u32, alignment 4)
    const field3 = field_slice.get(2);
    try testing.expect(field3.name == a_ident);
    const layout3 = layout_store.getLayout(field3.layout);
    try testing.expect(layout3.tag == .scalar);
    try testing.expect(layout3.data.scalar.data.int == .u32);

    // Fourth field: c (u8, alignment 1)
    const field4 = field_slice.get(3);
    try testing.expect(field4.name == c_ident);
    const layout4 = layout_store.getLayout(field4.layout);
    try testing.expect(layout4.tag == .scalar);
    try testing.expect(layout4.data.scalar.data.int == .u8);

    // Exactly 4 fields
    try testing.expectEqual(@as(usize, 4), field_slice.len);
}

test "record fields with same alignment sorted by name" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create types with same alignment
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const f32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f32 } } } });

    // Create field names that are not in alphabetical order
    const zebra_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("zebra"), base.Region.zero());
    const apple_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("apple"), base.Region.zero());
    const banana_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("banana"), base.Region.zero());

    // Create record with fields that all have alignment 4
    // { zebra: i32, apple: u32, banana: f32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = zebra_ident, .var_ = i32_var },
        .{ .name = apple_ident, .var_ = u32_var },
        .{ .name = banana_ident, .var_ = f32_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // Verify fields are sorted alphabetically since they all have the same alignment
    // Expected order: apple, banana, zebra
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start), .end = @enumFromInt(layout_store.getRecordData(record_layout.data.record.idx).fields.start + layout_store.getRecordData(record_layout.data.record.idx).fields.count) });

    // First field: apple
    const field1 = field_slice.get(0);
    try testing.expect(field1.name == apple_ident);
    const layout1 = layout_store.getLayout(field1.layout);
    try testing.expect(layout1.tag == .scalar);
    try testing.expect(layout1.data.scalar.data.int == .u32);

    // Second field: banana
    const field2 = field_slice.get(1);
    try testing.expect(field2.name == banana_ident);
    const layout2 = layout_store.getLayout(field2.layout);
    try testing.expect(layout2.tag == .scalar);
    try testing.expect(layout2.data.scalar.data.frac == .f32);

    // Third field: zebra
    const field3 = field_slice.get(2);
    try testing.expect(field3.name == zebra_ident);
    const layout3 = layout_store.getLayout(field3.layout);
    try testing.expect(layout3.tag == .scalar);
    try testing.expect(layout3.data.scalar.data.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - maximum nesting depth" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
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
    const result = try layout_store.addTypeVar(current_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));
    try testing.expect(result_layout.tag == .record);
}

test "addTypeVar - record with maximum fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
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
    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(num_fields, record_fields.len);

            // Verify fields are sorted by alignment then name for the target used to create the layout
            const target_usize = module_env.target.target_usize;
            var prev_alignment: ?std.mem.Alignment = null;
            var prev_name_in_group: ?[]const u8 = null;

            for (record_fields.items(.layout), record_fields.items(.name)) |field_layout_idx, field_name| {
                const field_layout = layout_store.layouts.get(@enumFromInt(field_layout_idx.int_idx));
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

    var layout_store = Store.init(&module_env, &type_store);
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
    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));
    try testing.expect(result_layout.tag == .record);
}

test "addTypeVar - alternating zero-sized and non-zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
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

    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(result_layout.data.record.idx).getFields());
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

    var layout_store = Store.init(&module_env, &type_store);
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

    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 1), record_fields.len);

            // The field should have the backing type's layout (u64)
            const field_layout_idx = record_fields.items(.layout)[0];
            const field_layout = layout_store.layouts.get(@enumFromInt(field_layout_idx.int_idx));
            try testing.expect(field_layout.tag == .scalar);
            try testing.expect(field_layout.data.scalar.data.int == .u64);
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

    var layout_store = Store.init(&module_env, &type_store);
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
    const result = try layout_store.addTypeVar(outer_list_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));

    // Verify it's a list
    try testing.expect(result_layout.tag == .list);

    // Verify the inner structure
    const box_layout = layout_store.layouts.get(@enumFromInt(result_layout.data.list.int_idx));
    try testing.expect(box_layout.tag == .box);

    const record_layout = layout_store.layouts.get(@enumFromInt(box_layout.data.box.int_idx));
    try testing.expect(record_layout.tag == .record);

    const rec = switch (record_layout.tag) {
        .record => record_layout.data.record,
        else => unreachable,
    };
    const rec_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(rec.idx).getFields());
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
    const field_0_layout = layout_store.layouts.get(@enumFromInt(field_0_layout_idx.int_idx));
    const field_1_layout = layout_store.layouts.get(@enumFromInt(field_1_layout_idx.int_idx));
    try testing.expect(field_0_layout.tag == .scalar);
    try testing.expect(field_1_layout.tag == .list_of_scalar);
    try testing.expectEqual(layout.ScalarTag.int, field_1_layout.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u64, field_1_layout.data.list_of_scalar.data.int);
}

test "addTypeVar - record size calculation with padding" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
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

    const result = try layout_store.addTypeVar(record_var);
    const result_layout = layout_store.layouts.get(@enumFromInt(result.int_idx));

    switch (result_layout.tag) {
        .record => {
            // Record should have 8-byte alignment (from u64)
            try testing.expectEqual(8, result_layout.data.record.alignment.toByteUnits());

            // Size should be 16 bytes (with padding)
            const rec_data = layout_store.getRecordData(result_layout.data.record.idx);
            try testing.expectEqual(16, rec_data.size);

            // Verify field order
            const rec_fields = layout_store.record_fields.rangeToSlice(layout_store.getRecordData(result_layout.data.record.idx).getFields());
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

test "addTypeVar - all scalar types use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test Box(I32) - should use box_of_scalar
    {
        const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
        const box_i32_var = type_store.freshFromContent(.{ .structure = .{ .box = i32_var } });

        const result = try layout_store.addTypeVar(box_i32_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.int, result_layout.data.box_of_scalar.tag);
        try testing.expectEqual(types.Num.Int.Precision.i32, result_layout.data.box_of_scalar.data.int);
    }

    // Test Box(F64) - should use box_of_scalar
    {
        const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
        const box_f64_var = type_store.freshFromContent(.{ .structure = .{ .box = f64_var } });

        const result = try layout_store.addTypeVar(box_f64_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.frac, result_layout.data.box_of_scalar.tag);
        try testing.expectEqual(types.Num.Frac.Precision.f64, result_layout.data.box_of_scalar.data.frac);
    }

    // Test Box(Bool) - should use box_of_scalar
    {
        const bool_layout = layout.Layout.booleanType();
        const box_bool_layout = layout.Layout.boxOfScalar(bool_layout.data.scalar);
        const box_bool_layout_idx = try layout_store.insertLayout(box_bool_layout);

        const result_layout = layout_store.getLayout(box_bool_layout_idx);

        try testing.expect(result_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.bool, result_layout.data.box_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.box_of_scalar.data.bool);
    }

    // Test Box(Str) - should use box_of_scalar
    {
        const str_var = type_store.freshFromContent(.{ .structure = .str });
        const box_str_var = type_store.freshFromContent(.{ .structure = .{ .box = str_var } });

        const result = try layout_store.addTypeVar(box_str_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.str, result_layout.data.box_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.box_of_scalar.data.str);
    }
}

test "addTypeVar - list of scalar types uses scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test List(U8) - should use list_of_scalar
    {
        const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
        const list_u8_var = type_store.freshFromContent(.{ .structure = .{ .list = u8_var } });

        const result = try layout_store.addTypeVar(list_u8_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.int, result_layout.data.list_of_scalar.tag);
        try testing.expectEqual(types.Num.Int.Precision.u8, result_layout.data.list_of_scalar.data.int);
    }

    // Test List(F32) - should use list_of_scalar
    {
        const f32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f32 } } } });
        const list_f32_var = type_store.freshFromContent(.{ .structure = .{ .list = f32_var } });

        const result = try layout_store.addTypeVar(list_f32_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.frac, result_layout.data.list_of_scalar.tag);
        try testing.expectEqual(types.Num.Frac.Precision.f32, result_layout.data.list_of_scalar.data.frac);
    }

    // Test List(Bool) - should use list_of_scalar
    {
        const bool_layout = layout.Layout.booleanType();
        const list_bool_layout = layout.Layout.listOfScalar(bool_layout.data.scalar);
        const list_bool_layout_idx = try layout_store.insertLayout(list_bool_layout);

        const result_layout = layout_store.getLayout(list_bool_layout_idx);

        try testing.expect(result_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.bool, result_layout.data.list_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.list_of_scalar.data.bool);
    }

    // Test List(Str) - should use list_of_scalar
    {
        const str_var = type_store.freshFromContent(.{ .structure = .str });
        const list_str_var = type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

        const result = try layout_store.addTypeVar(list_str_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.str, result_layout.data.list_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.list_of_scalar.data.str);
    }
}

test "addTypeVar - box and list of non-scalar types use indexed approach" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a record type (non-scalar)
    const field_name = type_store.env.idents.insert(gpa, base.Ident.for_text("field"), base.Region.zero());
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const fields = type_store.record_fields.appendSlice(gpa, &[_]types.RecordField{
        .{ .name = field_name, .var_ = str_var },
    });
    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Test Box(Record) - should use .box with index
    {
        const box_record_var = type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

        const result = try layout_store.addTypeVar(box_record_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        // The data.box should contain an index to the record layout
        const record_layout = layout_store.getLayout(result_layout.data.box);
        try testing.expect(record_layout.tag == .record);
    }

    // Test List(Record) - should use .list with index
    {
        const list_record_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

        const result = try layout_store.addTypeVar(list_record_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        // The data.list should contain an index to the record layout
        const record_layout = layout_store.getLayout(result_layout.data.list);
        try testing.expect(record_layout.tag == .record);
    }

    // Test Box(List(I32)) - should use .box with index since List(I32) is not a scalar
    {
        const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
        const list_i32_var = type_store.freshFromContent(.{ .structure = .{ .list = i32_var } });
        const box_list_var = type_store.freshFromContent(.{ .structure = .{ .box = list_i32_var } });

        const result = try layout_store.addTypeVar(box_list_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        // The data.box should contain an index to the list layout
        const list_layout = layout_store.getLayout(result_layout.data.box);
        try testing.expect(list_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.int, list_layout.data.list_of_scalar.tag);
        try testing.expectEqual(types.Num.Int.Precision.i32, list_layout.data.list_of_scalar.data.int);
    }
}

test "addTypeVar - host opaque types use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create a flex var (becomes host opaque when boxed)
    const flex_var = type_store.freshFromContent(.{ .flex_var = null });

    // Test Box(FlexVar) - should use box_of_scalar with host_opaque
    {
        const box_flex_var = type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });

        const result = try layout_store.addTypeVar(box_flex_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.host_opaque, result_layout.data.box_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.box_of_scalar.data.host_opaque);
    }

    // Test List(FlexVar) - should use list_of_scalar with host_opaque
    {
        const list_flex_var = type_store.freshFromContent(.{ .structure = .{ .list = flex_var } });

        const result = try layout_store.addTypeVar(list_flex_var);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.host_opaque, result_layout.data.list_of_scalar.tag);
        try testing.expectEqual({}, result_layout.data.list_of_scalar.data.host_opaque);
    }
}

test "addTypeVar - mixed scalar optimization in nested structures" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create List(Box(I64)) - List should use index, Box should use scalar
    const i64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i64 } } } });
    const box_i64_var = type_store.freshFromContent(.{ .structure = .{ .box = i64_var } });
    const list_box_var = type_store.freshFromContent(.{ .structure = .{ .list = box_i64_var } });

    const result = try layout_store.addTypeVar(list_box_var);
    const result_layout = layout_store.getLayout(result);

    // Outer list should use index approach since Box(I64) is not a scalar
    try testing.expect(result_layout.tag == .list);

    // Inner box should use scalar optimization
    const box_layout = layout_store.getLayout(result_layout.data.list);
    try testing.expect(box_layout.tag == .box_of_scalar);
    try testing.expectEqual(layout.ScalarTag.int, box_layout.data.box_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i64, box_layout.data.box_of_scalar.data.int);
}

test "addTypeVar - all integer precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    const int_precisions = [_]types.Num.Int.Precision{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 };

    for (int_precisions) |precision| {
        // Test Box(IntType)
        const int_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = precision } } } });
        const box_int_var = type_store.freshFromContent(.{ .structure = .{ .box = int_var } });

        const box_result = try layout_store.addTypeVar(box_int_var);
        const box_layout = layout_store.getLayout(box_result);

        try testing.expect(box_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.int, box_layout.data.box_of_scalar.tag);
        try testing.expectEqual(precision, box_layout.data.box_of_scalar.data.int);

        // Test List(IntType)
        const list_int_var = type_store.freshFromContent(.{ .structure = .{ .list = int_var } });

        const list_result = try layout_store.addTypeVar(list_int_var);
        const list_layout = layout_store.getLayout(list_result);

        try testing.expect(list_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.int, list_layout.data.list_of_scalar.tag);
        try testing.expectEqual(precision, list_layout.data.list_of_scalar.data.int);
    }
}

test "addTypeVar - all boolean precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Test Box(Bool)
    {
        const bool_layout = layout.Layout.booleanType();
        const box_bool_layout = layout.Layout.boxOfScalar(bool_layout.data.scalar);
        const box_bool_layout_idx = try layout_store.insertLayout(box_bool_layout);

        const box_layout = layout_store.getLayout(box_bool_layout_idx);

        try testing.expect(box_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.bool, box_layout.data.box_of_scalar.tag);
        try testing.expectEqual({}, box_layout.data.box_of_scalar.data.bool);
    }

    // Test List(Bool)
    {
        const bool_layout = layout.Layout.booleanType();
        const list_bool_layout = layout.Layout.listOfScalar(bool_layout.data.scalar);
        const list_bool_layout_idx = try layout_store.insertLayout(list_bool_layout);

        const list_layout = layout_store.getLayout(list_bool_layout_idx);

        try testing.expect(list_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.bool, list_layout.data.list_of_scalar.tag);
        try testing.expectEqual({}, list_layout.data.list_of_scalar.data.bool);
    }
}

test "addTypeVar - all frac precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    const frac_precisions = [_]types.Num.Frac.Precision{ .f32, .f64, .dec };

    for (frac_precisions) |precision| {
        // Test Box(FracType)
        const frac_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = precision } } } });
        const box_frac_var = type_store.freshFromContent(.{ .structure = .{ .box = frac_var } });

        const box_result = try layout_store.addTypeVar(box_frac_var);
        const box_layout = layout_store.getLayout(box_result);

        try testing.expect(box_layout.tag == .box_of_scalar);
        try testing.expectEqual(layout.ScalarTag.frac, box_layout.data.box_of_scalar.tag);
        try testing.expectEqual(precision, box_layout.data.box_of_scalar.data.frac);

        // Test List(FracType)
        const list_frac_var = type_store.freshFromContent(.{ .structure = .{ .list = frac_var } });

        const list_result = try layout_store.addTypeVar(list_frac_var);
        const list_layout = layout_store.getLayout(list_result);

        try testing.expect(list_layout.tag == .list_of_scalar);
        try testing.expectEqual(layout.ScalarTag.frac, list_layout.data.list_of_scalar.tag);
        try testing.expectEqual(precision, list_layout.data.list_of_scalar.data.frac);
    }
}

test "layouts_by_var uses ArrayListMap with pre-allocation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store with types store
    var layout_store = Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    // Create some type variables
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const num_var = type_store.freshFromContent(.{ .structure = types.int_u32 });
    const list_var = type_store.freshFromContent(.{ .structure = .{ .list = num_var } });

    // Convert to layouts
    _ = try layout_store.addTypeVar(str_var);
    _ = try layout_store.addTypeVar(num_var);
    _ = try layout_store.addTypeVar(list_var);

    // Verify no Var 0 was used (all Vars start from 1)
    try testing.expect(@intFromEnum(str_var) > 0);
    try testing.expect(@intFromEnum(num_var) > 0);
    try testing.expect(@intFromEnum(list_var) > 0);

    // Verify the ArrayListMap was initialized with capacity
    // The capacity should be at least as large as the types store
    try testing.expect(layout_store.layouts_by_var.entries.capacity >= type_store.getNumVars());
}
