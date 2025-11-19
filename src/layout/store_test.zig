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

    /// Helper to create a nominal Box type with the given element type
    fn mkBoxType(self: *LayoutTest, elem_var: types.Var) !types.Var {
        const box_ident_idx = try self.module_env.insertIdent(base.Ident.for_text("Box"));
        const builtin_module_idx = try self.module_env.insertIdent(base.Ident.for_text("Builtin"));
        const box_content = try self.type_store.mkNominal(
            .{ .ident_idx = box_ident_idx },
            elem_var,
            &[_]types.Var{elem_var},
            builtin_module_idx,
        );
        return try self.type_store.freshFromContent(box_content);
    }
};

test "addTypeVar - bool type" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
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
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Flex number var (Num a) defaults to Dec
    const num_var = try lt.type_store.fresh();
    const flex_num_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = num_var } } });
    const num_layout_idx = try lt.layout_store.addTypeVar(flex_num_var, &lt.type_scope);
    const num_layout = lt.layout_store.getLayout(num_layout_idx);
    try testing.expect(num_layout.tag == .scalar);
    try testing.expect(num_layout.data.scalar.data.frac == .dec);

    // Flex int var (Int a) defaults to Dec
    const int_var = try lt.type_store.fresh();
    const flex_int_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .int_poly = int_var } } });
    const int_layout_idx = try lt.layout_store.addTypeVar(flex_int_var, &lt.type_scope);
    const int_layout = lt.layout_store.getLayout(int_layout_idx);
    try testing.expect(int_layout.tag == .scalar);
    try testing.expect(int_layout.data.scalar.data.frac == .dec);

    // Flex frac var (Frac a) defaults to Dec
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
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Box of flex_var
    const flex_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const box_flex_var = try lt.mkBoxType(flex_var);
    const box_flex_idx = try lt.layout_store.addTypeVar(box_flex_var, &lt.type_scope);
    const box_flex_layout = lt.layout_store.getLayout(box_flex_idx);
    try testing.expect(box_flex_layout.tag == .box);
    try testing.expectEqual(layout.Idx.opaque_ptr, box_flex_layout.data.box);

    // Box of rigid_var
    const ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("a"));
    const rigid_var = try lt.type_store.freshFromContent(.{ .rigid = types.Rigid.init(ident_idx) });
    const box_rigid_var = try lt.mkBoxType(rigid_var);
    const box_rigid_idx = try lt.layout_store.addTypeVar(box_rigid_var, &lt.type_scope);
    const box_rigid_layout = lt.layout_store.getLayout(box_rigid_idx);
    try testing.expect(box_rigid_layout.tag == .box);
    try testing.expectEqual(layout.Idx.opaque_ptr, box_rigid_layout.data.box);
}

test "addTypeVar - zero-sized types (ZST)" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident gets set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.builtin_module_ident = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Bare ZSTs should return .zst layout
    const empty_record_idx = try lt.layout_store.addTypeVar(empty_record_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(empty_record_idx).tag == .zst);
    const empty_tag_union_idx = try lt.layout_store.addTypeVar(empty_tag_union_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(empty_tag_union_idx).tag == .zst);

    // ZSTs inside containers should use optimized layouts
    const box_zst_var = try lt.mkBoxType(empty_record_var);
    const box_zst_idx = try lt.layout_store.addTypeVar(box_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_zst_idx).tag == .box_of_zst);

    const list_zst_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        empty_tag_union_var,
        &[_]types.Var{empty_tag_union_var},
        builtin_module_idx,
    );
    const list_zst_var = try lt.type_store.freshFromContent(list_zst_content);
    const list_zst_idx = try lt.layout_store.addTypeVar(list_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(list_zst_idx).tag == .list_of_zst);
}

test "addTypeVar - record with zero-sized fields keeps them" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const zst_var1 = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const zst_var2 = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = try lt.type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("zst1")), .var_ = zst_var1 },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("zst2")), .var_ = zst_var2 },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("age")), .var_ = i32_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = zst_var2 } } });
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);

    try testing.expect(record_layout.tag == .record);
    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());
    try testing.expectEqual(@as(usize, 3), field_slice.len); // All fields including ZST "empty" field are kept
}

test "addTypeVar - record with only zero-sized fields" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a")), .var_ = empty_record_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b")), .var_ = empty_record_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });

    // Bare record with only ZST fields should create a record with ZST fields
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .record);
    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());
    try testing.expectEqual(@as(usize, 2), field_slice.len); // Both ZST fields are kept

    // Box of such a record should be box_of_zst since the record only contains ZST fields
    const box_record_var = try lt.mkBoxType(record_var);
    const box_idx = try lt.layout_store.addTypeVar(box_record_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
}

test "record field sorting by alignment then name" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
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
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
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
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
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

    // Outer: { x: zst } extends middle - zst field will be dropped
    const zst_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const outer_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("x")), .var_ = zst_var }});
    const outer_rec_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = middle_rec } } });

    const record_idx = try lt.layout_store.addTypeVar(outer_rec_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .record);

    const field_slice = lt.layout_store.record_fields.sliceRange(lt.layout_store.getRecordData(record_layout.data.record.idx).getFields());
    try testing.expectEqual(@as(usize, 3), field_slice.len); // All fields including ZST x are kept

    // Verify all three fields are present (order may vary for ZST fields)
    try testing.expectEqualStrings("y", lt.module_env.getIdent(field_slice.get(0).name));
    // Fields 1 and 2 should be x and z in some order
    const name1 = lt.module_env.getIdent(field_slice.get(1).name);
    const name2 = lt.module_env.getIdent(field_slice.get(2).name);
    const has_x = std.mem.eql(u8, name1, "x") or std.mem.eql(u8, name2, "x");
    const has_z = std.mem.eql(u8, name1, "z") or std.mem.eql(u8, name2, "z");
    try testing.expect(has_x and has_z);
}

test "record extension with empty_record succeeds" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const zst_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = zst_var }});

    // Extending empty_record is valid - creates a record with ZST fields
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = zst_var } } });
    const record_idx = try lt.layout_store.addTypeVar(record_var, &lt.type_scope);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .record);
}

test "deeply nested containers with inner ZST" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident gets set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.builtin_module_ident = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create List(Box(List(Box(empty_record))))
    const empty_record = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_box = try lt.mkBoxType(empty_record);
    const inner_list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        inner_box,
        &[_]types.Var{inner_box},
        builtin_module_idx,
    );
    const inner_list = try lt.type_store.freshFromContent(inner_list_content);
    const outer_box = try lt.mkBoxType(inner_list);
    const outer_list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        outer_box,
        &[_]types.Var{outer_box},
        builtin_module_idx,
    );
    const outer_list_var = try lt.type_store.freshFromContent(outer_list_content);

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

test "nested ZST detection - List of record with ZST field" {
    // Test: List({ field: {} }) should be list_of_zst
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident gets set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.builtin_module_ident = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = empty_record_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });

    // List of this record should be list_of_zst since the record only has ZST fields
    const list_content = try lt.type_store.mkNominal(.{ .ident_idx = list_ident_idx }, record_var, &[_]types.Var{record_var}, builtin_module_idx);
    const list_var = try lt.type_store.freshFromContent(list_content);
    const list_idx = try lt.layout_store.addTypeVar(list_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(list_idx).tag == .list_of_zst);
}

test "nested ZST detection - Box of tuple with ZST elements" {
    // Test: Box(((), ())) should be box_of_zst
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create a tuple with two empty record elements: ((), ())
    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const tuple_elems = try lt.type_store.vars.appendSlice(lt.gpa, &[_]types.Var{ empty_record_var, empty_record_var });
    const tuple_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = tuple_elems } } });

    // The tuple should be ZST since both elements are ZST
    const tuple_idx = try lt.layout_store.addTypeVar(tuple_var, &lt.type_scope);
    const tuple_layout = lt.layout_store.getLayout(tuple_idx);
    try testing.expect(lt.layout_store.layoutSize(tuple_layout) == 0);

    // Box of it should be box_of_zst
    const box_var = try lt.mkBoxType(tuple_var);
    const box_idx = try lt.layout_store.addTypeVar(box_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
}

test "nested ZST detection - deeply nested" {
    // Test: List({ field: ({ field2: {} }, ()) }) should be list_of_zst
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident gets set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.builtin_module_ident = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Start from the inside: {} (empty record)
    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });

    // { field2: {} }
    const inner_record_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("field2")), .var_ = empty_record_var },
    });
    const inner_record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_record_fields, .ext = empty_record_var } } });

    // ({ field2: {} }, ()) - tuple with ZST record and ZST empty record
    const tuple_elems = try lt.type_store.vars.appendSlice(lt.gpa, &[_]types.Var{ inner_record_var, empty_record_var });
    const tuple_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = tuple_elems } } });

    // { field: ({ field2: {} }, ()) }
    const outer_record_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = tuple_var },
    });
    const outer_record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_record_fields, .ext = empty_record_var } } });

    // List({ field: ({ field2: {} }, ()) })
    const list_content = try lt.type_store.mkNominal(.{ .ident_idx = list_ident_idx }, outer_record_var, &[_]types.Var{outer_record_var}, builtin_module_idx);
    const list_var = try lt.type_store.freshFromContent(list_content);
    const list_idx = try lt.layout_store.addTypeVar(list_var, &lt.type_scope);

    // Since the entire nested structure is ZST, the list should be list_of_zst
    try testing.expect(lt.layout_store.getLayout(list_idx).tag == .list_of_zst);
}
