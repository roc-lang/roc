//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const layout = @import("layout.zig");
const layout_store_ = @import("store.zig");
const collections = @import("collections");
const ModuleEnv = @import("can").ModuleEnv;

const types_store = types.store;
const Ident = base.Ident;
const target = base.target;
const LayoutError = layout_store_.LayoutError;
const Store = layout_store_.Store;
const TypeScope = types.TypeScope;
const testing = std.testing;

/// A helper struct to manage the boilerplate of setting up and tearing down
/// the necessary environments for layout tests.
const LayoutTest = struct {
    gpa: std.mem.Allocator,
    module_env: ModuleEnv,
    type_store: types_store.Store,
    layout_store: Store,
    type_scope: TypeScope,

    fn deinit(self: *LayoutTest) void {
        self.layout_store.deinit();
        self.type_scope.deinit();
        self.type_store.deinit();
        self.module_env.deinit();
    }
};

test "addTypeVar - basic scalar types" {
    const gpa = testing.allocator;
    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);
    defer layout_store.deinit();

    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();

    // Test string type
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const str_layout_idx = try layout_store.addTypeVar(str_var, &type_scope);
    const str_layout = layout_store.getLayout(str_layout_idx);
    try testing.expect(str_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.str, str_layout.data.scalar.tag);
}

test "addTypeVar - bool type" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const bool_layout = layout.Layout.boolType();
    const bool_layout_idx = try lt.layout_store.insertLayout(bool_layout);

    const retrieved_layout = lt.layout_store.getLayout(bool_layout_idx);
    try testing.expect(retrieved_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.bool, retrieved_layout.data.scalar.tag);
    try testing.expectEqual(@as(u32, 1), lt.layout_store.layoutSize(retrieved_layout));
}

test "addTypeVar - default layouts for polymorphic types" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Flex number var (Num a) defaults to i128
    const num_var = try lt.type_store.fresh();
    const flex_num_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = num_var } } });
    const num_layout_idx = try lt.layout_store.addTypeVar(flex_num_var, &lt.type_scope);
    const num_layout = lt.layout_store.getLayout(num_layout_idx);
    try testing.expect(num_layout.tag == .scalar);
    try testing.expect(num_layout.data.scalar.data.int == .i128);

    // Flex int var (Int a) defaults to i128
    const int_var = try lt.type_store.fresh();
    const flex_int_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .int_poly = int_var } } });
    const int_layout_idx = try lt.layout_store.addTypeVar(flex_int_var, &lt.type_scope);
    const int_layout = lt.layout_store.getLayout(int_layout_idx);
    try testing.expect(int_layout.tag == .scalar);
    try testing.expect(int_layout.data.scalar.data.int == .i128);

    // Flex frac var (Frac a) defaults to dec
    const frac_var = try lt.type_store.fresh();
    const flex_frac_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac_poly = frac_var } } });
    const frac_layout_idx = try lt.layout_store.addTypeVar(flex_frac_var, &lt.type_scope);
    const frac_layout = lt.layout_store.getLayout(frac_layout_idx);
    try testing.expect(frac_layout.tag == .scalar);
    try testing.expect(frac_layout.data.scalar.data.frac == .dec);
}

test "addTypeVar - host opaque types compile to opaque_ptr" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Box of flex_var
    const flex_var = try lt.type_store.freshFromContent(.{ .flex_var = null });
    const box_flex_var = try lt.type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });
    const box_flex_idx = try lt.layout_store.addTypeVar(box_flex_var, &lt.type_scope);
    const box_flex_layout = lt.layout_store.getLayout(box_flex_idx);
    try testing.expect(box_flex_layout.tag == .box);
    try testing.expectEqual(layout.Idx.opaque_ptr, box_flex_layout.data.box);

    // Box of rigid_var
    const ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("a"));
    const rigid_var = try lt.type_store.freshFromContent(.{ .rigid_var = ident_idx });
    const box_rigid_var = try lt.type_store.freshFromContent(.{ .structure = .{ .box = rigid_var } });
    const box_rigid_idx = try lt.layout_store.addTypeVar(box_rigid_var, &lt.type_scope);
    const box_rigid_layout = lt.layout_store.getLayout(box_rigid_idx);
    try testing.expect(box_rigid_layout.tag == .box);
    try testing.expectEqual(layout.Idx.opaque_ptr, box_rigid_layout.data.box);
}

test "addTypeVar - scalar optimization for containers" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Test List(Scalar)
    const str_var = try lt.type_store.freshFromContent(.{ .structure = .str });
    const list_str_var = try lt.type_store.freshFromContent(.{ .structure = .{ .list = str_var } });
    const list_layout_idx = try lt.layout_store.addTypeVar(list_str_var, &lt.type_scope);
    const list_layout = lt.layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);
    try testing.expectEqual(layout.Idx.str, list_layout.data.list);

    // Test Box(Scalar)
    const box_str_var = try lt.type_store.freshFromContent(.{ .structure = .{ .box = str_var } });
    const box_layout_idx = try lt.layout_store.addTypeVar(box_str_var, &lt.type_scope);
    const box_layout = lt.layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box);
    try testing.expectEqual(layout.Idx.str, box_layout.data.box);

    // Test List(Box(Scalar)) - outer container uses index, inner uses scalar optimization
    const list_box_str_var = try lt.type_store.freshFromContent(.{ .structure = .{ .list = box_str_var } });
    const list_box_idx = try lt.layout_store.addTypeVar(list_box_str_var, &lt.type_scope);
    const list_box_layout = lt.layout_store.getLayout(list_box_idx);
    try testing.expect(list_box_layout.tag == .list);
    const inner_box_layout = lt.layout_store.getLayout(list_box_layout.data.list);
    try testing.expect(inner_box_layout.tag == .box);
    try testing.expectEqual(layout.Idx.str, inner_box_layout.data.box);
}

test "addTypeVar - zero-sized types (ZST)" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Bare ZSTs should error
    try testing.expectError(LayoutError.ZeroSizedType, lt.layout_store.addTypeVar(empty_record_var, &lt.type_scope));
    try testing.expectError(LayoutError.ZeroSizedType, lt.layout_store.addTypeVar(empty_tag_union_var, &lt.type_scope));

    // ZSTs inside containers should use optimized layouts
    const box_zst_var = try lt.type_store.freshFromContent(.{ .structure = .{ .box = empty_record_var } });
    const box_zst_idx = try lt.layout_store.addTypeVar(box_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_zst_idx).tag == .box_of_zst);

    const list_zst_var = try lt.type_store.freshFromContent(.{ .structure = .{ .list = empty_tag_union_var } });
    const list_zst_idx = try lt.layout_store.addTypeVar(list_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(list_zst_idx).tag == .list_of_zst);
}

test "addTypeVar - record with dropped zero-sized fields" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const str_var = try lt.type_store.freshFromContent(.{ .structure = .str });
    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("name")), .var_ = str_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("empty")), .var_ = empty_record_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("age")), .var_ = i32_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);

    try testing.expect(record_layout.tag == .record);
    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());
    try testing.expectEqual(@as(usize, 2), field_slice.len); // "empty" field should be dropped
}

test "addTypeVar - record with only zero-sized fields errors" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a")), .var_ = empty_record_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b")), .var_ = empty_record_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });

    // Bare record with only ZST fields should error
    try testing.expectError(LayoutError.ZeroSizedType, lt.layout_store.addTypeVar(record_var, &lt.type_scope));

    // Box of such a record should become box_of_zst
    const box_record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .box = record_var } });
    const box_idx = try lt.layout_store.addTypeVar(box_record_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
}

test "record field sorting by alignment then name" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const u8_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const u32_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u64_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("c_u32")), .var_ = u32_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a_u64")), .var_ = u64_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("d_u8")), .var_ = u8_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b_u64")), .var_ = u64_var },
    });

    const empty_ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_ext } } });
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);

    try testing.expect(record_layout.tag == .record);
    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());

    // Expected order: a_u64, b_u64 (sorted by name), c_u32, d_u8
    try testing.expectEqualStrings("a_u64", lt.module_env.getIdent(field_slice.get(0).name));
    try testing.expectEqualStrings("b_u64", lt.module_env.getIdent(field_slice.get(1).name));
    try testing.expectEqualStrings("c_u32", lt.module_env.getIdent(field_slice.get(2).name));
    try testing.expectEqualStrings("d_u8", lt.module_env.getIdent(field_slice.get(3).name));
}

test "record size and alignment calculation" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // { a: u8, b: u32, c: u8, d: u64 }
    // After sorting by alignment: d: u64, b: u32, a: u8, c: u8
    // Layout: [d: 8 bytes] [b: 4 bytes] [a: 1 byte] [c: 1 byte] [padding: 2 bytes] -> Total: 16 bytes
    // Alignment should be max of fields, which is 8 from u64.
    const u8_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const u32_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u64_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a")), .var_ = u8_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b")), .var_ = u32_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("c")), .var_ = u8_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("d")), .var_ = u64_var },
    });

    const empty_ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_ext } } });
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);

    try testing.expect(record_layout.tag == .record);
    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(@as(u32, 16), lt.layout_store.layoutSize(record_layout));
        try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
    }
}

test "record with chained extensions" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Innermost: { z: u8 }
    const u8_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const empty_ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("z")), .var_ = u8_var }});
    const inner_rec = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Middle: { y: f64 } extends inner
    const f64_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
    const middle_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("y")), .var_ = f64_var }});
    const middle_rec = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = inner_rec } } });

    // Outer: { x: str } extends middle
    const str_var = try lt.type_store.freshFromContent(.{ .structure = .str });
    const outer_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("x")), .var_ = str_var }});
    const outer_rec_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = middle_rec } } });

    const record_idx = try lt.layout_store.addTypeVar(outer_rec_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .record);

    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());
    try testing.expectEqual(@as(usize, 3), field_slice.len);

    // Expected order by alignment: x (str), y (f64), z (u8)
    try testing.expectEqualStrings("x", lt.module_env.getIdent(field_slice.get(0).name));
    try testing.expectEqualStrings("y", lt.module_env.getIdent(field_slice.get(1).name));
    try testing.expectEqualStrings("z", lt.module_env.getIdent(field_slice.get(2).name));
}

test "record extension with non-record type fails" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const str_var = try lt.type_store.freshFromContent(.{ .structure = .str });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = str_var }});

    // Try to extend a str, which is invalid
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = str_var } } });
    try testing.expectError(LayoutError.InvalidRecordExtension, lt.layout_store.addTypeVar(record_var, &lt.type_scope));
}

test "deeply nested containers with inner ZST" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create List(Box(List(Box(empty_record))))
    const empty_record = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_box = try lt.type_store.freshFromContent(.{ .structure = .{ .box = empty_record } });
    const inner_list = try lt.type_store.freshFromContent(.{ .structure = .{ .list = inner_box } });
    const outer_box = try lt.type_store.freshFromContent(.{ .structure = .{ .box = inner_list } });
    const outer_list_var = try lt.type_store.freshFromContent(.{ .structure = .{ .list = outer_box } });

    const result_idx = try lt.layout_store.addTypeVar(outer_list_var, &lt.type_scope);
    const outer_list_layout = lt.layout_store.getLayout(result_idx);
    try testing.expect(outer_list_layout.tag == .list);

    const outer_box_layout = lt.layout_store.getLayout(outer_list_layout.data.list);
    try testing.expect(outer_box_layout.tag == .box);

    const inner_list_layout = lt.layout_store.getLayout(outer_box_layout.data.box);
    try testing.expect(inner_list_layout.tag == .list);

    // The innermost element is Box(empty_record), which should resolve to box_of_zst
    const inner_box_layout = lt.layout_store.getLayout(inner_list_layout.data.list);
    try testing.expect(inner_box_layout.tag == .box_of_zst);
}
