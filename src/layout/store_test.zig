//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts

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

test "addTypeVar - str" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = try type_store.freshFromContent(.{ .structure = .str });

    // Convert to layout
    const str_layout_idx = try layout_store.addTypeVar(str_var, &type_scope);

    // Verify the layout
    const str_layout = layout_store.getLayout(str_layout_idx);
    try testing.expect(str_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.str, str_layout.data.scalar.tag);
}

test "addTypeVar - bool" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a bool layout directly (since we don't have bool types in the type system yet)
    const bool_layout = layout.Layout.boolType();
    const bool_layout_idx = try layout_store.insertLayout(bool_layout);

    // Verify the layout
    const retrieved_layout = layout_store.getLayout(bool_layout_idx);
    try testing.expect(retrieved_layout.tag == .scalar);
    try testing.expectEqual(layout.ScalarTag.bool, retrieved_layout.data.scalar.tag);
    try testing.expectEqual(@as(u32, 1), layout_store.layoutSize(retrieved_layout));
}

test "addTypeVar - list of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = try type_store.freshFromContent(.{ .structure = .str });

    // Create a list of str type variable
    const list_str_var = try type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_str_var, &type_scope);

    // Verify the layout - List(Str) should be a list with str sentinel
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the element is the str sentinel
    try testing.expectEqual(layout.Idx.str, list_layout.data.list);
}

test "addTypeVar - list of box of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = try type_store.freshFromContent(.{ .structure = .str });

    // Create a box of str type variable
    const box_str_var = try type_store.freshFromContent(.{ .structure = .{ .box = str_var } });

    // Create a list of box of str type variable
    const list_box_str_var = try type_store.freshFromContent(.{ .structure = .{ .list = box_str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_box_str_var, &type_scope);

    // Verify the layout - List(Box(Str)) should use index since Box(Str) is not a scalar
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the outer list element (Box(Str))
    const box_layout = layout_store.getLayout(list_layout.data.list);
    try testing.expect(box_layout.tag == .box);

    // Verify the element is the str sentinel
    try testing.expectEqual(layout.Idx.str, box_layout.data.box);
}

test "addTypeVar - box of flex_var compiles to box of opaque_ptr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex_var type variable
    const flex_var = try type_store.freshFromContent(.{ .flex_var = null });

    // Create a box of flex_var type variable
    const box_flex_var = try type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_flex_var, &type_scope);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box);

    // Verify the element is opaque_ptr sentinel
    try testing.expectEqual(layout.Idx.opaque_ptr, box_layout.data.box);
}

test "addTypeVar - num u32" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a u32 type variable
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });

    // Convert to layout
    const u32_layout_idx = try layout_store.addTypeVar(u32_var, &type_scope);

    // Verify the layout
    const u32_layout = layout_store.getLayout(u32_layout_idx);
    try testing.expect(u32_layout.tag == .scalar);
    try testing.expect(u32_layout.data.scalar.data.int == .u32);
}

test "addTypeVar - num f64" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a f64 type variable
    const f64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });

    // Convert to layout
    const f64_layout_idx = try layout_store.addTypeVar(f64_var, &type_scope);

    // Verify the layout
    const f64_layout = layout_store.getLayout(f64_layout_idx);
    try testing.expect(f64_layout.tag == .scalar);
    try testing.expect(f64_layout.data.scalar.data.frac == .f64);
}

test "addTypeVar - list of num i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an i128 type variable
    const i128_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i128 } } } });

    // Create a list of i128 type variable
    const list_i128_var = try type_store.freshFromContent(.{ .structure = .{ .list = i128_var } });

    // Convert to layout
    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_i128_var, &type_scope);

    // Verify the layout - List(I128) should be a list with i128 sentinel
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the element is the i128 sentinel
    try testing.expectEqual(layout.Idx.i128, list_layout.data.list);
}

test "addTypeVar - num dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a dec type variable
    const dec_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } });

    // Convert to layout
    const dec_layout_idx = try layout_store.addTypeVar(dec_var, &type_scope);

    // Verify the layout
    const num_dec_layout = layout_store.getLayout(dec_layout_idx);
    try testing.expect(num_dec_layout.tag == .scalar);
    try testing.expect(num_dec_layout.data.scalar.data.frac == .dec);
}

test "addTypeVar - flex num var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex number type variable (Num(a))
    const num_var = try type_store.fresh();
    const requirements = types.Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = 0,
    };
    const flex_num_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = .{ .var_ = num_var, .requirements = requirements } } } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(flex_num_var, &type_scope);

    // Verify the layout
    const num_layout = layout_store.getLayout(layout_idx);
    try testing.expect(num_layout.tag == .scalar);
    try testing.expect(num_layout.data.scalar.data.int == .i128);
}

test "addTypeVar - flex int var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex int type variable (Int(a))
    const int_var = try type_store.fresh();
    const int_requirements = types.Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = 0,
    };
    const flex_int_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .int_poly = .{ .var_ = int_var, .requirements = int_requirements } } } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(flex_int_var, &type_scope);

    // Verify the layout
    const int_i64_layout = layout_store.getLayout(layout_idx);
    try testing.expect(int_i64_layout.tag == .scalar);
    try testing.expect(int_i64_layout.data.scalar.data.int == .i128);
}

test "addTypeVar - flex frac var defaults to dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    const frac_var = try type_store.fresh();
    const frac_requirements = types.Num.FracRequirements{
        .fits_in_f32 = true,
        .fits_in_dec = true,
    };
    const flex_frac_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac_poly = .{ .var_ = frac_var, .requirements = frac_requirements } } } });

    // Convert to layout - should default to dec
    const layout_idx = try layout_store.addTypeVar(flex_frac_var, &type_scope);

    // Verify the layout
    const frac_f64_layout = layout_store.getLayout(layout_idx);
    try testing.expect(frac_f64_layout.tag == .scalar);
    try testing.expect(frac_f64_layout.data.scalar.data.frac == .dec);
}

test "addTypeVar - list of flex num var defaults to list of i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex num type variable (Num(a))
    // Create a polyorphic int with specific requirements
    const poly_var = try type_store.fresh();
    const poly_requirements = types.Num.IntRequirements{
        .sign_needed = true,
        .bits_needed = 64,
    };
    const poly_int_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .int_poly = .{ .var_ = poly_var, .requirements = poly_requirements } } } });

    // Create a list of flex num type variable
    const list_flex_num_var = try type_store.freshFromContent(.{ .structure = .{ .list = poly_int_var } });

    // Convert to layout - should default to list of i128
    const list_layout_idx = try layout_store.addTypeVar(list_flex_num_var, &type_scope);

    // Verify the layout - List(FlexVar) should be a list with i128 sentinel (default for Num)
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the element is the i128 sentinel (default for flex Num)
    try testing.expectEqual(layout.Idx.i128, list_layout.data.list);
}

test "addTypeVar - box of flex frac var defaults to box of dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    // Create a polymorphic frac with specific requirements
    const poly_frac_var_inner = try type_store.fresh();
    const poly_frac_requirements = types.Num.FracRequirements{
        .fits_in_f32 = false,
        .fits_in_dec = true,
    };
    const poly_frac_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac_poly = .{ .var_ = poly_frac_var_inner, .requirements = poly_frac_requirements } } } });

    // Create a box of flex frac type variable
    const box_flex_frac_var = try type_store.freshFromContent(.{ .structure = .{ .box = poly_frac_var } });

    // Convert to layout - should default to box of dec
    const box_layout_idx = try layout_store.addTypeVar(box_flex_frac_var, &type_scope);

    // Verify the layout - Box(FlexFrac) should be a box with dec sentinel (default for Frac)
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box);

    // Verify the element is the dec sentinel (default for flex Frac)
    try testing.expectEqual(layout.Idx.dec, box_layout.data.box);
}

test "addTypeVar - box of rigid_var compiles to box of opaque_ptr" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an ident for the rigid var
    const ident_idx = try module_env.insertIdent(base.Ident.for_text("a"));

    // Create a rigid_var type variable
    const rigid_var = try type_store.freshFromContent(.{ .rigid_var = ident_idx });

    // Create a box of rigid_var type variable
    const box_rigid_var = try type_store.freshFromContent(.{ .structure = .{ .box = rigid_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_rigid_var, &type_scope);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box);

    // Verify the element is opaque_ptr sentinel
    try testing.expectEqual(layout.Idx.opaque_ptr, box_layout.data.box);
}

test "addTypeVar - box of empty record compiles to box_of_zst" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });

    // Create a box of empty record type variable
    const box_empty_record_var = try type_store.freshFromContent(.{ .structure = .{ .box = empty_record_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(box_empty_record_var, &type_scope);

    // Verify the layout is box_of_zst
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.tag == .box_of_zst);
}

test "addTypeVar - list of empty tag union compiles to list_of_zst" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = try type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Create a list of empty tag union type variable
    const list_empty_tag_union_var = try type_store.freshFromContent(.{ .structure = .{ .list = empty_tag_union_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_empty_tag_union_var, &type_scope);

    // Verify the layout is list_of_zst
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list_of_zst);
}

test "alignment - record alignment is max of field alignments" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create field identifiers
    const field1_ident = try module_env.insertIdent(base.Ident.for_text("field1"));
    const field2_ident = try module_env.insertIdent(base.Ident.for_text("field2"));
    const field3_ident = try module_env.insertIdent(base.Ident.for_text("field3"));

    // Create type variables for fields
    const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    _ = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u16 } } } });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    // Create record type { field1: U8, field2: U32, field3: U64 }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
        .{ .name = field3_ident, .var_ = u64_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
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
    const fields2 = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = field3_ident, .var_ = u64_var },
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
    });

    const ext2 = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var2 = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields2, .ext = ext2 } } });

    const record_layout_idx2 = try layout_store.addTypeVar(record_var2, &type_scope);
    const record_layout2 = layout_store.getLayout(record_layout_idx2);

    for (target.TargetUsize.all()) |target_usize| {
        const alignment2 = record_layout2.alignment(target_usize);
        try testing.expectEqual(@as(u32, 8), alignment2.toByteUnits());
    }
}

test "alignment - deeply nested record alignment (non-recursive)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create field identifiers
    const inner_ident = try module_env.insertIdent(base.Ident.for_text("inner"));
    const middle_ident = try module_env.insertIdent(base.Ident.for_text("middle"));
    const outer_ident = try module_env.insertIdent(base.Ident.for_text("outer"));
    const data_ident = try module_env.insertIdent(base.Ident.for_text("data"));

    // Create a U64 field (alignment 8)
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    // Create innermost record: { data: U64 }
    const inner_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = data_ident, .var_ = u64_var },
    });
    const inner_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = inner_ext } } });

    // Create middle record: { inner: { data: U64 } }
    const middle_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = inner_ident, .var_ = inner_record_var },
    });
    const middle_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const middle_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = middle_ext } } });

    // Create outer record: { middle: { inner: { data: U64 } } }
    const outer_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = middle_ident, .var_ = middle_record_var },
    });
    const outer_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const outer_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = outer_ext } } });

    // Create outermost record: { outer: { middle: { inner: { data: U64 } } } }
    const outermost_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = outer_ident, .var_ = outer_record_var },
    });
    const outermost_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const outermost_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outermost_fields, .ext = outermost_ext } } });

    // Convert to layout
    const outermost_layout_idx = try layout_store.addTypeVar(outermost_record_var, &type_scope);
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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(empty_record_var, &type_scope);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - bare empty tag union returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = try type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(empty_tag_union_var, &type_scope);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - simple record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create field types
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });

    // Create field identifiers
    const name_ident = try module_env.insertIdent(base.Ident.for_text("name"));
    const age_ident = try module_env.insertIdent(base.Ident.for_text("age"));

    // Create record type { name: str, age: u32 }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = age_ident, .var_ = u32_var },
    });

    // Create record type
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify the fields are sorted by alignment then name
    // Both str and u32 have same alignment on 64-bit systems (8 bytes for str pointer, 4 bytes for u32 but u32 comes first due to smaller alignment)
    // Actually str has alignment of usize (8 on 64-bit), u32 has alignment 4
    // So str should come first (higher alignment), then u32
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test record with multiple fields requiring padding
    // { a: u8, b: u32, c: u8, d: u64 }
    const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    const a_ident = try module_env.insertIdent(base.Ident.for_text("a"));
    const b_ident = try module_env.insertIdent(base.Ident.for_text("b"));
    const c_ident = try module_env.insertIdent(base.Ident.for_text("c"));
    const d_ident = try module_env.insertIdent(base.Ident.for_text("d"));

    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u8_var },
        .{ .name = b_ident, .var_ = u32_var },
        .{ .name = c_ident, .var_ = u8_var },
        .{ .name = d_ident, .var_ = u64_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // After sorting by alignment then name:
    // d: u64 (8 bytes) at offset 0
    // b: u32 (4 bytes) at offset 8
    // a: u8 (1 byte) at offset 12
    // c: u8 (1 byte) at offset 13
    // Total: 14 bytes, but aligned to 8 bytes = 16 bytes
    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(@as(u32, 16), layout_store.layoutSize(record_layout));
        try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
    }
}

test "addTypeVar - nested record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create inner record type { x: i32, y: i32 }
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const y_ident = try module_env.insertIdent(base.Ident.for_text("y"));

    const point_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const point_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = point_fields, .ext = empty_ext } } });

    // Create outer record type { name: Str, position: { x: i32, y: i32 } }
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const name_ident = try module_env.insertIdent(base.Ident.for_text("name"));
    const position_ident = try module_env.insertIdent(base.Ident.for_text("position"));

    const player_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = position_ident, .var_ = point_var },
    });

    const player_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = player_fields, .ext = empty_ext } } });

    // Convert to layout
    const player_layout_idx = try layout_store.addTypeVar(player_var, &type_scope);

    // Verify the outer layout
    const player_layout = layout_store.getLayout(player_layout_idx);
    try testing.expect(player_layout.tag == .record);

    // Verify the outer fields
    const player_record_data = layout_store.getRecordData(player_layout.data.record.idx);
    const outer_field_slice = layout_store.record_fields.sliceRange(player_record_data.getFields());

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
    const inner_field_slice = layout_store.record_fields.sliceRange(position_record_data.getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create record type { id: u64, active: bool }
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    // For bool, we'll use u8 as a placeholder
    const bool_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const id_ident = try module_env.insertIdent(base.Ident.for_text("id"));
    const active_ident = try module_env.insertIdent(base.Ident.for_text("active"));

    const record_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = id_ident, .var_ = u64_var },
        .{ .name = active_ident, .var_ = bool_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = record_fields, .ext = empty_ext } } });

    // Create list of records
    const list_record_var = try type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(list_record_var, &type_scope);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.tag == .list);

    // Verify the record element
    const record_layout = layout_store.getLayout(list_layout.data.list);
    try testing.expect(record_layout.tag == .record);

    // Verify the record fields
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create extension record { y: i32, z: u16 }
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const u16_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u16 } } } });
    const y_ident = try module_env.insertIdent(base.Ident.for_text("y"));
    const z_ident = try module_env.insertIdent(base.Ident.for_text("z"));

    const ext_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = i32_var },
        .{ .name = z_ident, .var_ = u16_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));

    const main_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(main_record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have all 3 fields (x from main, y and z from extension)
    const record_data = layout_store.getRecordData(record_layout.data.record.idx);
    const field_slice = layout_store.record_fields.sliceRange(record_data.getFields());
    try testing.expectEqual(@as(usize, 3), field_slice.len);

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str and f64 are 8-byte aligned, i32 is 4-byte aligned
    // So order should be: x (str, 8-byte), z (f64, 8-byte), y (i32, 4-byte)

    // Field x (str)
    const x_field = field_slice.get(0);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.tag == .scalar);

    // Field y (i32) - comes before z due to alignment
    const y_field = field_slice.get(1);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.tag == .scalar);
    try testing.expect(y_layout.data.scalar.data.int == .i32);

    // Field z (u16) - comes last due to smaller alignment
    const z_field = field_slice.get(2);
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.tag == .scalar);
    try testing.expect(z_layout.data.scalar.data.int == .u16);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - record extension with str type fails" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a record with str as extension (invalid)
    const field_name = try module_env.insertIdent(Ident.for_text("field"));
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = try type_store.freshFromContent(.{ .structure = .str }),
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const str_ext = try type_store.freshFromContent(.{ .structure = .str });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = str_ext } } });

    const result = layout_store.addTypeVar(record_var, &type_scope);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - record extension with num type fails" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a record with number as extension (invalid)
    const field_name = try module_env.insertIdent(Ident.for_text("field"));
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = try type_store.freshFromContent(.{ .structure = .str }),
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const num_ext = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = num_ext } } });

    const result = layout_store.addTypeVar(record_var, &type_scope);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - deeply nested containers with zero-sized inner type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create List(Box(List(Box(empty_record))))
    const empty_record = try type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_box = try type_store.freshFromContent(.{ .structure = .{ .box = empty_record } });
    const inner_list = try type_store.freshFromContent(.{ .structure = .{ .list = inner_box } });
    const outer_box = try type_store.freshFromContent(.{ .structure = .{ .box = inner_list } });
    const outer_list_var = try type_store.freshFromContent(.{ .structure = .{ .list = outer_box } });

    const result = try layout_store.addTypeVar(outer_list_var, &type_scope);
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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create List({ only_field: {} })
    const field_name = try module_env.insertIdent(Ident.for_text("only_field"));
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = try type_store.freshFromContent(.{ .structure = .empty_record }),
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });
    const list_var = try type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    const result = try layout_store.addTypeVar(list_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    // List of empty record should be list_of_zst
    try testing.expect(result_layout.tag == .list_of_zst);
}

test "addTypeVar - record field ordering stability" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create multiple records with same fields but different order
    const field_a = try module_env.insertIdent(Ident.for_text("aaa"));
    const field_b = try module_env.insertIdent(Ident.for_text("bbb"));
    const field_c = try module_env.insertIdent(Ident.for_text("ccc"));

    // All fields have same type (same alignment)
    const field_type = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });

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

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });

    const fields1_slice = try type_store.record_fields.appendSlice(gpa, fields1.items);
    const record1_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields1_slice, .ext = empty_ext } } });

    const fields2_slice = try type_store.record_fields.appendSlice(gpa, fields2.items);
    const record2_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields2_slice, .ext = empty_ext } } });

    const fields3_slice = try type_store.record_fields.appendSlice(gpa, fields3.items);
    const record3_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields3_slice, .ext = empty_ext } } });

    const result1 = try layout_store.addTypeVar(record1_var, &type_scope);
    const result2 = try layout_store.addTypeVar(record2_var, &type_scope);
    const result3 = try layout_store.addTypeVar(record3_var, &type_scope);

    const layout1 = layout_store.getLayout(result1);
    const layout2 = layout_store.getLayout(result2);
    const layout3 = layout_store.getLayout(result3);

    // All should produce records with fields in same order (sorted by name: aaa, bbb, ccc)
    switch (layout1.tag) {
        .record => {
            const fields_1 = layout_store.record_fields.sliceRange(layout_store.getRecordData(layout1.data.record.idx).getFields());

            switch (layout2.tag) {
                .record => {
                    const fields_2 = layout_store.record_fields.sliceRange(layout_store.getRecordData(layout2.data.record.idx).getFields());

                    switch (layout3.tag) {
                        .record => {
                            const fields_3 = layout_store.record_fields.sliceRange(layout_store.getRecordData(layout3.data.record.idx).getFields());

                            // All should have 3 fields
                            try testing.expectEqual(@as(usize, 3), fields_1.len);
                            try testing.expectEqual(@as(usize, 3), fields_2.len);
                            try testing.expectEqual(@as(usize, 3), fields_3.len);

                            // All should have same field order
                            var i: usize = 0;
                            while (i < 3) : (i += 1) {
                                const name1 = module_env.getIdent(fields_1.items(.name)[i]);
                                const name2 = module_env.getIdent(fields_2.items(.name)[i]);
                                const name3 = module_env.getIdent(fields_3.items(.name)[i]);

                                try testing.expectEqualStrings(name1, name2);
                                try testing.expectEqualStrings(name2, name3);
                            }

                            // Verify correct alphabetical order
                            try testing.expectEqualStrings("aaa", module_env.getIdent(fields_1.items(.name)[0]));
                            try testing.expectEqualStrings("bbb", module_env.getIdent(fields_1.items(.name)[1]));
                            try testing.expectEqualStrings("ccc", module_env.getIdent(fields_1.items(.name)[2]));
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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test 1: Bare empty record
    const empty_record = try type_store.freshFromContent(.{ .structure = .empty_record });
    const result1 = layout_store.addTypeVar(empty_record, &type_scope);
    try testing.expectError(LayoutError.ZeroSizedType, result1);

    // Test 2: Box of empty record
    const box_empty = try type_store.freshFromContent(.{ .structure = .{ .box = empty_record } });
    const result2 = try layout_store.addTypeVar(box_empty, &type_scope);
    const result2_layout = layout_store.getLayout(result2);
    try testing.expect(result2_layout.tag == .box_of_zst);

    // Test 3: List of empty record
    const list_empty = try type_store.freshFromContent(.{ .structure = .{ .list = empty_record } });
    const result3 = try layout_store.addTypeVar(list_empty, &type_scope);
    const result3_layout = layout_store.getLayout(result3);
    try testing.expect(result3_layout.tag == .list_of_zst);

    // Test 4: Record containing only empty record field
    const field_name = try module_env.insertIdent(Ident.for_text("empty"));
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();
    try fields.append(.{
        .name = field_name,
        .var_ = empty_record,
    });
    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_with_empty = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result4 = layout_store.addTypeVar(record_with_empty, &type_scope);
    try testing.expectError(LayoutError.ZeroSizedType, result4);
}

test "addTypeVar - record alignment edge cases" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create record with fields of all different alignments
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    // Add fields with different alignments (1, 2, 4, 8, 16 bytes)
    const alignments = [_]struct { name: []const u8, type: types.FlatType }{
        .{ .name = "align1", .type = .{ .num = .{ .num_compact = .{ .int = .u8 } } } }, // 1-byte alignment
        .{ .name = "align2", .type = .{ .num = .{ .num_compact = .{ .int = .u16 } } } }, // 2-byte alignment
        .{ .name = "align4", .type = .{ .num = .{ .num_compact = .{ .int = .u32 } } } }, // 4-byte alignment
        .{ .name = "align8", .type = .{ .num = .{ .num_compact = .{ .int = .u64 } } } }, // 8-byte alignment
        .{ .name = "align16", .type = .{ .num = .{ .num_compact = .{ .int = .u128 } } } }, // 16-byte alignment
    };

    for (alignments) |field_info| {
        const field_name = try module_env.insertIdent(Ident.for_text(field_info.name));
        try fields.append(.{
            .name = field_name,
            .var_ = try type_store.freshFromContent(.{ .structure = field_info.type }),
        });
    }

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);
    switch (result_layout.tag) {
        .record => {
            // Record should have 16-byte alignment (maximum of all fields)
            try testing.expectEqual(@as(u32, 16), result_layout.data.record.alignment.toByteUnits());

            // Fields should be sorted by alignment (descending) then name
            const rec_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 5), rec_fields.len);

            // Verify order: align16, align8, align4, align2, align1
            const expected_order = [_][]const u8{ "align16", "align8", "align4", "align2", "align1" };
            for (expected_order, 0..) |expected_name, i| {
                const actual_name = module_env.getIdent(rec_fields.items(.name)[i]);
                try testing.expectEqualStrings(expected_name, actual_name);
            }
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record with duplicate field in extension (matching types)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const y_ident = try module_env.insertIdent(base.Ident.for_text("y"));

    // Create extension record { x: str, y: i32 }
    const ext_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with same type)
    const main_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - should succeed since types match
    const record_layout_idx = try layout_store.addTypeVar(main_record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have 3 fields (x appears twice - from main and extension, plus y from extension)
    // TODO: Field deduplication should happen at the type-checking level, not in layout generation
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));

    // Create extension record { x: i32 }
    const ext_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with different types)
    const main_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - currently succeeds with both fields present
    // TODO: Type checking should catch duplicate fields with mismatched types before layout generation
    const record_layout_idx = try layout_store.addTypeVar(main_record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // We get both fields even though they have the same name but different types
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a str type to use as invalid extension
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const x_ident = try module_env.insertIdent(Ident.for_text("x"));

    // Create main record { x: str } with str as extension (invalid)
    const main_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = str_var } } });

    // Convert to layout - should fail due to invalid extension
    const result = layout_store.addTypeVar(main_record_var, &type_scope);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - record with chained extensions" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const f64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
    const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    const w_ident = try module_env.insertIdent(base.Ident.for_text("w"));
    const x_ident = try module_env.insertIdent(base.Ident.for_text("x"));
    const y_ident = try module_env.insertIdent(base.Ident.for_text("y"));
    const z_ident = try module_env.insertIdent(base.Ident.for_text("z"));

    // Create innermost extension record { z: u8 }
    const inner_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = z_ident, .var_ = u8_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create middle extension record { y: f64 } extending inner
    const middle_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = f64_var },
    });

    const middle_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = inner_record_var } } });

    // Create outermost record { w: str, x: i32 } extending middle
    const outer_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = w_ident, .var_ = str_var },
        .{ .name = x_ident, .var_ = i32_var },
    });

    const outer_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = middle_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(outer_record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we have all 4 fields from all levels
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const name_ident = try module_env.insertIdent(base.Ident.for_text("name"));
    const empty_ident = try module_env.insertIdent(base.Ident.for_text("empty"));
    const age_ident = try module_env.insertIdent(base.Ident.for_text("age"));

    // Create record { name: str, empty: {}, age: i32 }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = empty_ident, .var_ = empty_record_var },
        .{ .name = age_ident, .var_ = i32_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .record);

    // Verify we only have 2 fields (empty field should be dropped)
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = try type_store.freshFromContent(.{ .structure = .empty_tag_union });

    const field1_ident = try module_env.insertIdent(base.Ident.for_text("field1"));
    const field2_ident = try module_env.insertIdent(base.Ident.for_text("field2"));

    // Create record { field1: {}, field2: [] }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_tag_union_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout - should fail because all fields are zero-sized
    const result = layout_store.addTypeVar(record_var, &type_scope);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - box of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });

    const field1_ident = try module_env.insertIdent(base.Ident.for_text("field1"));
    const field2_ident = try module_env.insertIdent(base.Ident.for_text("field2"));

    // Create record { field1: {}, field2: {} }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_record_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create box of this record
    const box_record_var = try type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

    // Convert to layout - should become box_of_zst
    const box_layout_idx = try layout_store.addTypeVar(box_record_var, &type_scope);

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create field names we'll reuse
    const field_names = [_]Ident.Idx{
        try module_env.insertIdent(base.Ident.for_text("a")),
        try module_env.insertIdent(base.Ident.for_text("b")),
        try module_env.insertIdent(base.Ident.for_text("c")),
        try module_env.insertIdent(base.Ident.for_text("d")),
        try module_env.insertIdent(base.Ident.for_text("e")),
        try module_env.insertIdent(base.Ident.for_text("f")),
    };

    // Test all combinations
    var outer_field_count: usize = 1;
    while (outer_field_count <= 3) : (outer_field_count += 1) {
        // Generate all possible field type combinations for outer record
        var field_type_combo: usize = 0;
        const max_combo = std.math.pow(usize, 4, outer_field_count); // 4 possibilities per field

        while (field_type_combo < max_combo) : (field_type_combo += 1) {
            // Create a new type store and layout store for each test
            var test_type_store = try types_store.Store.init(module_env.gpa);
            defer test_type_store.deinit();

            var test_layout_store = try Store.init(&module_env, &test_type_store);
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
                        break :blk try test_type_store.freshFromContent(.{ .structure = .str });
                    },
                    1 => blk: {
                        // Empty record (0 fields)
                        expected_total_fields += 1;
                        // This field will be dropped as zero-sized
                        break :blk try test_type_store.freshFromContent(.{ .structure = .empty_record });
                    },
                    2 => blk: {
                        // Record with 1-2 non-zero fields
                        expected_total_fields += 1;
                        const inner_field_count = 1 + (field_idx % 2); // 1 or 2 fields
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        var inner_idx: usize = 0;
                        while (inner_idx < inner_field_count) : (inner_idx += 1) {
                            const inner_var = try test_type_store.freshFromContent(.{ .structure = .str });
                            try inner_fields.append(.{
                                .name = field_names[3 + inner_idx],
                                .var_ = inner_var,
                            });
                        }

                        expected_non_zero_fields += 1; // The nested record itself counts as 1
                        expected_total_fields += inner_field_count;

                        const inner_fields_slice = try test_type_store.record_fields.appendSlice(module_env.gpa, inner_fields.items);
                        const empty_ext = try test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk try test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    3 => blk: {
                        // Record with mix of zero and non-zero fields
                        expected_total_fields += 1;
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        // Add one empty record field (will be dropped)
                        const empty_record_var = try test_type_store.freshFromContent(.{ .structure = .empty_record });
                        try inner_fields.append(.{
                            .name = field_names[3],
                            .var_ = empty_record_var,
                        });
                        expected_total_fields += 1;

                        // Add one str field (will be kept)
                        const str_var = try test_type_store.freshFromContent(.{ .structure = .str });
                        try inner_fields.append(.{
                            .name = field_names[4],
                            .var_ = str_var,
                        });
                        expected_total_fields += 1;

                        // This nested record will have 1 non-zero field (the str field) after dropping zero-sized ones
                        expected_non_zero_fields += 1;

                        const inner_fields_slice = try test_type_store.record_fields.appendSlice(module_env.gpa, inner_fields.items);
                        const empty_ext = try test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk try test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    else => unreachable,
                };

                try outer_fields.append(.{
                    .name = field_names[field_idx],
                    .var_ = field_var,
                });
            }

            // Create outer record
            const outer_fields_slice = try test_type_store.record_fields.appendSlice(module_env.gpa, outer_fields.items);
            const empty_ext = try test_type_store.freshFromContent(.{ .structure = .empty_record });
            const outer_record_var = try test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields_slice, .ext = empty_ext } } });

            // Convert to layout
            const result = test_layout_store.addTypeVar(outer_record_var, &type_scope) catch |err| {
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
                // The record might be optimized to record if it has only one field
                try testing.expect(result_layout.tag == .record);

                // Count actual non-zero fields in the result
                const result_record_data = test_layout_store.getRecordData(result_layout.data.record.idx);
                const field_slice = test_layout_store.record_fields.sliceRange(result_record_data.getFields());
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
                            const nested_slice = test_layout_store.record_fields.sliceRange(nested_record_data.getFields());
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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create inner record with only zero-sized fields
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const a_ident = try module_env.insertIdent(base.Ident.for_text("a"));
    const b_ident = try module_env.insertIdent(base.Ident.for_text("b"));

    // Create inner record
    const inner_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = empty_record_var },
        .{ .name = b_ident, .var_ = empty_record_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create outer record { name: str, data: inner_record }
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const name_ident = try module_env.insertIdent(base.Ident.for_text("name"));
    const data_ident = try module_env.insertIdent(base.Ident.for_text("data"));

    const outer_fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = data_ident, .var_ = inner_record_var },
    });

    const outer_record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(outer_record_var, &type_scope);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    // The record should have a single string field
    try testing.expect(record_layout.tag == .record);
}

test "addTypeVar - list of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create empty record type
    const empty_record_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const field_ident = try module_env.insertIdent(base.Ident.for_text("field"));

    // Create record { field: {} }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = field_ident, .var_ = empty_record_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create list of that record
    const list_var = try type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout - should be list_of_zst
    const list_layout_idx = try layout_store.addTypeVar(list_var, &type_scope);
    const list_layout = layout_store.getLayout(list_layout_idx);

    try testing.expect(list_layout.tag == .list_of_zst);
}

test "alignment - record with log2 alignment representation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test 1: Record with U8 field (alignment 1, log2 = 0)
    {
        const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
        const field_ident = try module_env.insertIdent(base.Ident.for_text("field"));
        const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u8_var },
        });
        const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
        const record_layout = layout_store.getLayout(record_layout_idx);

        // The record was optimized to record
        try testing.expect(record_layout.tag == .record);

        // Verify alignment is still correct
        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 1), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 1), layout_store.layoutSize(record_layout)); // size = 1 byte
        }
    }

    // Test 2: Record with U32 field (alignment 4, log2 = 2)
    {
        const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
        const field_ident = try module_env.insertIdent(base.Ident.for_text("field"));
        const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u32_var },
        });
        const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
        const record_layout = layout_store.getLayout(record_layout_idx);

        // The record was optimized to record
        try testing.expect(record_layout.tag == .record);

        // Verify alignment is still correct
        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 4), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 4), layout_store.layoutSize(record_layout)); // size = 4 bytes
        }
    }

    // Test 3: Record with U64 field (alignment 8, log2 = 3)
    {
        const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
        const field_ident = try module_env.insertIdent(base.Ident.for_text("field"));
        const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u64_var },
        });
        const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
        const record_layout = layout_store.getLayout(record_layout_idx);

        // The record was optimized to record
        try testing.expect(record_layout.tag == .record);

        // Verify alignment is still correct
        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
            try testing.expectEqual(@as(u32, 8), layout_store.layoutSize(record_layout)); // size = 8 bytes
        }
    }

    // Test 4: Record with mixed fields - should use max alignment
    {
        const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
        const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
        const field1_ident = try module_env.insertIdent(base.Ident.for_text("small"));
        const field2_ident = try module_env.insertIdent(base.Ident.for_text("large"));
        const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
            .{ .name = field1_ident, .var_ = u8_var },
            .{ .name = field2_ident, .var_ = u64_var },
        });
        const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.tag == .record);
        try testing.expectEqual(@as(u32, 8), record_layout.data.record.alignment.toByteUnits()); // max alignment = 8

        for (target.TargetUsize.all()) |target_usize| {
            try testing.expectEqual(@as(u32, 8), record_layout.alignment(target_usize).toByteUnits());
            // After sorting: u64 (8 bytes) at offset 0, u8 (1 byte) at offset 8, total size 16 (aligned to 8)
            try testing.expectEqual(@as(u32, 16), layout_store.layoutSize(record_layout));
        }
    }
}

test "record fields sorted by alignment then name" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types with different alignments
    const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });

    // Create field names that would sort differently alphabetically
    const a_ident = try module_env.insertIdent(base.Ident.for_text("a"));
    const b_ident = try module_env.insertIdent(base.Ident.for_text("b"));
    const c_ident = try module_env.insertIdent(base.Ident.for_text("c"));
    const d_ident = try module_env.insertIdent(base.Ident.for_text("d"));

    // Create record with fields in a specific order to test sorting
    // { a: u32, b: u64, c: u8, d: u64 }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u32_var }, // alignment 4
        .{ .name = b_ident, .var_ = u64_var }, // alignment 8
        .{ .name = c_ident, .var_ = u8_var }, // alignment 1
        .{ .name = d_ident, .var_ = u64_var }, // alignment 8
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // Verify fields are sorted by alignment (descending) then by name (ascending)
    // Expected order: b (u64, align 8), d (u64, align 8), a (u32, align 4), c (u8, align 1)
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    // Create type store
    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types with same alignment
    const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const f32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });

    // Create field names that are not in alphabetical order
    const zebra_ident = try module_env.insertIdent(base.Ident.for_text("zebra"));
    const apple_ident = try module_env.insertIdent(base.Ident.for_text("apple"));
    const banana_ident = try module_env.insertIdent(base.Ident.for_text("banana"));

    // Create record with fields that all have alignment 4
    // { zebra: i32, apple: u32, banana: f32 }
    const fields = try type_store.record_fields.appendSlice(module_env.gpa, &[_]types.RecordField{
        .{ .name = zebra_ident, .var_ = i32_var },
        .{ .name = apple_ident, .var_ = u32_var },
        .{ .name = banana_ident, .var_ = f32_var },
    });

    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(record_var, &type_scope);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    // Verify fields are sorted alphabetically since they all have the same alignment
    // Expected order: apple, banana, zebra
    const field_slice = layout_store.record_fields.sliceRange(layout_store.getRecordData(record_layout.data.record.idx).getFields());

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

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create deeply nested record structure
    const max_depth = 100;
    var current_var = try type_store.freshFromContent(.{ .structure = .str });

    var depth: usize = 0;
    while (depth < max_depth) : (depth += 1) {
        const field_name = try module_env.insertIdent(Ident.for_text("field"));
        var fields = std.ArrayList(types.RecordField).init(gpa);
        defer fields.deinit();

        try fields.append(.{
            .name = field_name,
            .var_ = current_var,
        });

        const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
        const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
        current_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });
    }

    // This should still work - we don't want arbitrary limits on nesting
    const result = try layout_store.addTypeVar(current_var, &type_scope);
    const result_layout = layout_store.getLayout(result);
    try testing.expect(result_layout.tag == .record);
}

test "addTypeVar - record with maximum fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create record with many fields
    const num_fields = 1000;
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    var i: usize = 0;
    while (i < num_fields) : (i += 1) {
        var name_buf: [20]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "field_{}", .{i}) catch unreachable;
        const field_name = try module_env.insertIdent(Ident.for_text(name_str));

        // Alternate between different types to test alignment sorting
        const field_var = if (i % 3 == 0)
            try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } })
        else if (i % 3 == 1)
            try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } })
        else
            try type_store.freshFromContent(.{ .structure = .str });

        try fields.append(.{
            .name = field_name,
            .var_ = field_var,
        });
    }

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Should handle large number of fields
    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(num_fields, record_fields.len);
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record field alignments differ between targets" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create fields with different alignments on 32-bit vs 64-bit
    // str/pointer types have 4-byte alignment on 32-bit, 8-byte on 64-bit
    const str_field_name = try module_env.insertIdent(Ident.for_text("str_field"));
    const u64_field_name = try module_env.insertIdent(Ident.for_text("u64_field"));
    const u32_field_name = try module_env.insertIdent(Ident.for_text("u32_field"));
    const u8_field_name = try module_env.insertIdent(Ident.for_text("u8_field"));

    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    const u32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, &[_]types.RecordField{
        .{ .name = str_field_name, .var_ = str_var },
        .{ .name = u64_field_name, .var_ = u64_var },
        .{ .name = u32_field_name, .var_ = u32_var },
        .{ .name = u8_field_name, .var_ = u8_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 4), record_fields.len);

            // Find each field by name and verify its alignment on different targets
            var str_layout: ?layout.Layout = null;
            var u64_layout: ?layout.Layout = null;
            var u32_layout: ?layout.Layout = null;
            var u8_layout: ?layout.Layout = null;

            for (record_fields.items(.name), record_fields.items(.layout)) |name, layout_idx| {
                const field_name = module_env.getIdent(name);
                const field_layout = layout_store.getLayout(layout_idx);

                if (std.mem.eql(u8, field_name, "str_field")) {
                    str_layout = field_layout;
                } else if (std.mem.eql(u8, field_name, "u64_field")) {
                    u64_layout = field_layout;
                } else if (std.mem.eql(u8, field_name, "u32_field")) {
                    u32_layout = field_layout;
                } else if (std.mem.eql(u8, field_name, "u8_field")) {
                    u8_layout = field_layout;
                }
            }

            // Verify all fields were found
            try testing.expect(str_layout != null);
            try testing.expect(u64_layout != null);
            try testing.expect(u32_layout != null);
            try testing.expect(u8_layout != null);

            // Verify alignments on 32-bit target
            const target_32 = target.TargetUsize.u32;
            try testing.expectEqual(@as(u32, 4), str_layout.?.alignment(target_32).toByteUnits());
            try testing.expectEqual(@as(u32, 8), u64_layout.?.alignment(target_32).toByteUnits());
            try testing.expectEqual(@as(u32, 4), u32_layout.?.alignment(target_32).toByteUnits());
            try testing.expectEqual(@as(u32, 1), u8_layout.?.alignment(target_32).toByteUnits());

            // Verify alignments on 64-bit target
            const target_64 = target.TargetUsize.u64;
            try testing.expectEqual(@as(u32, 8), str_layout.?.alignment(target_64).toByteUnits());
            try testing.expectEqual(@as(u32, 8), u64_layout.?.alignment(target_64).toByteUnits());
            try testing.expectEqual(@as(u32, 4), u32_layout.?.alignment(target_64).toByteUnits());
            try testing.expectEqual(@as(u32, 1), u8_layout.?.alignment(target_64).toByteUnits());

            // Verify that str field has different alignment on different targets
            try testing.expect(str_layout.?.alignment(target_32).toByteUnits() !=
                str_layout.?.alignment(target_64).toByteUnits());
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record field sorting follows alignment then name order" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create fields with different alignments to test sorting
    // Names are chosen to test alphabetical sorting within same alignment group
    const zebra_field_name = try module_env.insertIdent(Ident.for_text("zebra"));
    const apple_field_name = try module_env.insertIdent(Ident.for_text("apple"));
    const banana_field_name = try module_env.insertIdent(Ident.for_text("banana"));
    const carrot_field_name = try module_env.insertIdent(Ident.for_text("carrot"));

    // Create fields with specific alignments:
    // zebra: u64 (8-byte alignment)
    // apple: u32 (4-byte alignment)
    // banana: u32 (4-byte alignment)
    // carrot: u8 (1-byte alignment)
    const zebra_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    const apple_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const banana_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const carrot_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, &[_]types.RecordField{
        .{ .name = zebra_field_name, .var_ = zebra_var },
        .{ .name = apple_field_name, .var_ = apple_var },
        .{ .name = banana_field_name, .var_ = banana_var },
        .{ .name = carrot_field_name, .var_ = carrot_var },
    });

    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 4), record_fields.len);

            // Note: The actual target used for sorting depends on the native platform,
            // but we can verify the sorting principles are followed
            const native_target = if (@sizeOf(usize) == 8) target.TargetUsize.u64 else target.TargetUsize.u32;

            // Verify fields are sorted by alignment (descending) then by name (ascending)
            var prev_alignment: ?std.mem.Alignment = null;
            var prev_name_in_group: ?[]const u8 = null;

            for (record_fields.items(.layout), record_fields.items(.name)) |field_layout_idx, field_name| {
                const field_layout = layout_store.getLayout(field_layout_idx);
                const field_alignment = field_layout.alignment(native_target);
                const field_name_str = module_env.getIdent(field_name);

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
                        // Different alignment group, reset name tracking
                        prev_name_in_group = field_name_str;
                    }
                } else {
                    prev_name_in_group = field_name_str;
                }

                prev_alignment = field_alignment;
            }
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - pointer types have target-dependent alignment" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create types that contain pointers
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const box_var = try type_store.freshFromContent(.{ .structure = .{ .box = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } }) } });
    const list_var = try type_store.freshFromContent(.{ .structure = .{ .list = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } }) } });

    // Convert to layouts
    const str_idx = try layout_store.addTypeVar(str_var, &type_scope);
    const box_idx = try layout_store.addTypeVar(box_var, &type_scope);
    const list_idx = try layout_store.addTypeVar(list_var, &type_scope);

    const str_layout = layout_store.getLayout(str_idx);
    const box_layout = layout_store.getLayout(box_idx);
    const list_layout = layout_store.getLayout(list_idx);

    // Verify pointer types have different alignment on different targets
    const target_32 = target.TargetUsize.u32;
    const target_64 = target.TargetUsize.u64;

    // All pointer-containing types should have 4-byte alignment on 32-bit
    try testing.expectEqual(@as(u32, 4), str_layout.alignment(target_32).toByteUnits());
    try testing.expectEqual(@as(u32, 4), box_layout.alignment(target_32).toByteUnits());
    try testing.expectEqual(@as(u32, 4), list_layout.alignment(target_32).toByteUnits());

    // All pointer-containing types should have 8-byte alignment on 64-bit
    try testing.expectEqual(@as(u32, 8), str_layout.alignment(target_64).toByteUnits());
    try testing.expectEqual(@as(u32, 8), box_layout.alignment(target_64).toByteUnits());
    try testing.expectEqual(@as(u32, 8), list_layout.alignment(target_64).toByteUnits());
}

test "addTypeVar - record with very long field names" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create record with very long field names
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    const long_name_a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    const long_name_b = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";

    const field_name_a = try module_env.insertIdent(Ident.for_text(long_name_a));
    const field_name_b = try module_env.insertIdent(Ident.for_text(long_name_b));

    try fields.append(.{
        .name = field_name_a,
        .var_ = try type_store.freshFromContent(.{ .structure = .str }),
    });
    try fields.append(.{
        .name = field_name_b,
        .var_ = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } }),
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Should handle long field names
    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);
    try testing.expect(result_layout.tag == .record);
}

test "addTypeVar - alternating zero-sized and non-zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
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
        const field_name = try module_env.insertIdent(Ident.for_text(name_str));

        const field_var = if (i % 2 == 0) blk: {
            expected_non_zero_count += 1;
            break :blk try type_store.freshFromContent(.{ .structure = .str });
        } else blk: {
            // Zero-sized: empty record
            break :blk try type_store.freshFromContent(.{ .structure = .empty_record });
        };

        try fields.append(.{
            .name = field_name,
            .var_ = field_var,
        });
    }

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            // Only non-zero-sized fields should remain
            try testing.expectEqual(expected_non_zero_count, record_fields.len);
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - record field type changes through alias" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create an alias that points to a concrete type
    const backing_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    const alias_var = backing_var;

    // Create record using the alias
    const field_name = try module_env.insertIdent(Ident.for_text("aliased"));
    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    try fields.append(.{
        .name = field_name,
        .var_ = alias_var,
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            const record_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(@as(usize, 1), record_fields.len);

            // The field should have the backing type's layout (u64)
            const field_layout_idx = record_fields.items(.layout)[0];
            const field_layout = layout_store.getLayout(field_layout_idx);
            try testing.expect(field_layout.tag == .scalar);
            try testing.expect(field_layout.data.scalar.data.int == .u64);
        },

        else => try testing.expect(false),
    }
}

test "addTypeVar - mixed container types" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create complex nested structure: List(Box(Record { a: Str, b: List(U64) }))

    // Inner list of U64
    const u64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } });
    const inner_list_var = try type_store.freshFromContent(.{ .structure = .{ .list = u64_var } });

    // Record with two fields
    const field_a = try module_env.insertIdent(Ident.for_text("a"));
    const field_b = try module_env.insertIdent(Ident.for_text("b"));

    var record_fields = std.ArrayList(types.RecordField).init(gpa);
    defer record_fields.deinit();

    try record_fields.append(.{
        .name = field_a,
        .var_ = try type_store.freshFromContent(.{ .structure = .str }),
    });
    try record_fields.append(.{
        .name = field_b,
        .var_ = inner_list_var,
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, record_fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    // Box the record
    const box_var = try type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

    // List of boxes
    const outer_list_var = try type_store.freshFromContent(.{ .structure = .{ .list = box_var } });

    // Should handle complex nesting
    const result = try layout_store.addTypeVar(outer_list_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    // Verify it's a list
    try testing.expect(result_layout.tag == .list);

    // Verify the inner structure
    const box_layout = layout_store.getLayout(result_layout.data.list);
    try testing.expect(box_layout.tag == .box);

    const record_layout = layout_store.getLayout(box_layout.data.box);
    try testing.expect(record_layout.tag == .record);

    const rec = switch (record_layout.tag) {
        .record => record_layout.data.record,
        else => unreachable,
    };
    const rec_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(rec.idx).getFields());
    try testing.expectEqual(@as(usize, 2), rec_fields.len);

    // Fields should be sorted by alignment then name
    // Both str and list have pointer alignment, so should be sorted by name (a, b)
    const field_0_name = module_env.getIdent(rec_fields.items(.name)[0]);
    const field_1_name = module_env.getIdent(rec_fields.items(.name)[1]);
    try testing.expectEqualStrings("a", field_0_name);
    try testing.expectEqualStrings("b", field_1_name);

    // Verify field types
    const field_0_layout_idx = rec_fields.items(.layout)[0];
    const field_1_layout_idx = rec_fields.items(.layout)[1];
    const field_0_layout = layout_store.getLayout(field_0_layout_idx);
    const field_1_layout = layout_store.getLayout(field_1_layout_idx);
    try testing.expect(field_0_layout.tag == .scalar);
    try testing.expect(field_1_layout.tag == .list);
    try testing.expectEqual(layout.Idx.u64, field_1_layout.data.list);
}

test "addTypeVar - record size calculation with padding" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create record that requires padding: { a: U8, b: U64, c: U16 }
    // Expected layout (sorted by alignment): b (u64), c (u16), a (u8)
    // Memory layout: [b: 8 bytes][c: 2 bytes][padding: 5 bytes][a: 1 byte] = 16 bytes total

    var fields = std.ArrayList(types.RecordField).init(gpa);
    defer fields.deinit();

    const field_a = try module_env.insertIdent(Ident.for_text("a"));
    const field_b = try module_env.insertIdent(Ident.for_text("b"));
    const field_c = try module_env.insertIdent(Ident.for_text("c"));

    try fields.append(.{
        .name = field_a,
        .var_ = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } }),
    });
    try fields.append(.{
        .name = field_b,
        .var_ = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u64 } } } }),
    });
    try fields.append(.{
        .name = field_c,
        .var_ = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u16 } } } }),
    });

    const fields_slice = try type_store.record_fields.appendSlice(gpa, fields.items);
    const empty_ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_slice, .ext = empty_ext } } });

    const result = try layout_store.addTypeVar(record_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    switch (result_layout.tag) {
        .record => {
            // Record should have 8-byte alignment (from u64)
            try testing.expectEqual(8, result_layout.data.record.alignment.toByteUnits());

            // Size should be 16 bytes (with padding)
            const rec_data = layout_store.getRecordData(result_layout.data.record.idx);
            try testing.expectEqual(16, rec_data.size);

            // Verify field order
            const rec_fields = layout_store.record_fields.sliceRange(layout_store.getRecordData(result_layout.data.record.idx).getFields());
            try testing.expectEqual(3, rec_fields.len);

            // First field should be 'b' (u64, 8-byte alignment)
            const field_0_name = module_env.getIdent(rec_fields.items(.name)[0]);
            try testing.expectEqualStrings("b", field_0_name);

            // Second field should be 'c' (u16, 2-byte alignment)
            const field_1_name = module_env.getIdent(rec_fields.items(.name)[1]);
            try testing.expectEqualStrings("c", field_1_name);

            // Third field should be 'a' (u8, 1-byte alignment)
            const field_2_name = module_env.getIdent(rec_fields.items(.name)[2]);
            try testing.expectEqualStrings("a", field_2_name);
        },
        else => try testing.expect(false),
    }
}

test "addTypeVar - all scalar types use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test Box(I32) - should use box with i32 sentinel
    {
        const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
        const box_i32_var = try type_store.freshFromContent(.{ .structure = .{ .box = i32_var } });

        const result = try layout_store.addTypeVar(box_i32_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.i32, result_layout.data.box);
    }

    // Test Box(F64) - should use box with f64 sentinel
    {
        const f64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
        const box_f64_var = try type_store.freshFromContent(.{ .structure = .{ .box = f64_var } });

        const result = try layout_store.addTypeVar(box_f64_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.f64, result_layout.data.box);
    }

    // Test Box(Bool) - should use box with bool sentinel
    {
        // Create a bool layout directly and insert it
        const bool_layout = layout.Layout.boolType();
        const bool_idx = try layout_store.insertLayout(bool_layout);

        // Create a box layout containing the bool
        const box_layout = layout.Layout.box(bool_idx);
        const box_idx = try layout_store.insertLayout(box_layout);

        const result_layout = layout_store.getLayout(box_idx);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.bool, result_layout.data.box);
    }

    // Test Box(Str) - should use box with str sentinel
    {
        const str_var = try type_store.freshFromContent(.{ .structure = .str });
        const box_str_var = try type_store.freshFromContent(.{ .structure = .{ .box = str_var } });

        const result = try layout_store.addTypeVar(box_str_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.str, result_layout.data.box);
    }
}

test "addTypeVar - list of scalar types uses scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test List(U8) - should use list with u8 sentinel
    {
        const u8_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });
        const list_u8_var = try type_store.freshFromContent(.{ .structure = .{ .list = u8_var } });

        const result = try layout_store.addTypeVar(list_u8_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.u8, result_layout.data.list);
    }

    // Test List(F32) - should use list with f32 sentinel
    {
        const f32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });
        const list_f32_var = try type_store.freshFromContent(.{ .structure = .{ .list = f32_var } });

        const result = try layout_store.addTypeVar(list_f32_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.f32, result_layout.data.list);
    }

    // Test List(Bool)
    {
        // Create a bool layout directly and insert it
        const bool_layout = layout.Layout.boolType();
        const bool_idx = try layout_store.insertLayout(bool_layout);

        // Create a list layout containing the bool
        const list_layout = layout.Layout.list(bool_idx);
        const list_idx = try layout_store.insertLayout(list_layout);

        const result_layout = layout_store.getLayout(list_idx);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.bool, result_layout.data.list);
    }

    // Test List(Str) - should use list with str sentinel
    {
        const str_var = try type_store.freshFromContent(.{ .structure = .str });
        const list_str_var = try type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

        const result = try layout_store.addTypeVar(list_str_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.str, result_layout.data.list);
    }
}

test "addTypeVar - box and list of non-scalar types use indexed approach" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a record type (non-scalar)
    const field_name = try module_env.insertIdent(Ident.for_text("field"));
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const fields = try type_store.record_fields.appendSlice(gpa, &[_]types.RecordField{
        .{ .name = field_name, .var_ = str_var },
    });
    const ext = try type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Test Box(Record) - should use .box with index
    {
        const box_record_var = try type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

        const result = try layout_store.addTypeVar(box_record_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        // The data.box should contain an index to the record layout
        const record_layout = layout_store.getLayout(result_layout.data.box);
        // Single field records now use regular record layout
        try testing.expect(record_layout.tag == .record);
    }

    // Test List(Record) - should use .list with index
    {
        const list_record_var = try type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

        const result = try layout_store.addTypeVar(list_record_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        // The data.list should contain an index to the record layout
        const record_layout = layout_store.getLayout(result_layout.data.list);
        // Single field records now use regular record layout
        try testing.expect(record_layout.tag == .record);
    }

    // Test Box(List(I32)) - should use .box with index since List(I32) is not a scalar
    {
        const i32_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
        const list_i32_var = try type_store.freshFromContent(.{ .structure = .{ .list = i32_var } });
        const box_list_var = try type_store.freshFromContent(.{ .structure = .{ .box = list_i32_var } });

        const result = try layout_store.addTypeVar(box_list_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        // The data.box should contain an index to the list layout
        const list_layout = layout_store.getLayout(result_layout.data.box);
        try testing.expect(list_layout.tag == .list);
        try testing.expectEqual(layout.Idx.i32, list_layout.data.list);
    }
}

test "addTypeVar - host opaque types use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a flex var (becomes host opaque when boxed)
    const flex_var = try type_store.freshFromContent(.{ .flex_var = null });

    // Test Box(FlexVar) - should use box with opaque_ptr sentinel
    {
        const box_flex_var = try type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });

        const result = try layout_store.addTypeVar(box_flex_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.opaque_ptr, result_layout.data.box);
    }

    // Test List(FlexVar) - should use list with opaque_ptr sentinel
    {
        const list_flex_var = try type_store.freshFromContent(.{ .structure = .{ .list = flex_var } });

        const result = try layout_store.addTypeVar(list_flex_var, &type_scope);
        const result_layout = layout_store.getLayout(result);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.opaque_ptr, result_layout.data.list);
    }
}

test "addTypeVar - mixed scalar optimization in nested structures" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create List(Box(I64)) - List should use index, Box should use scalar
    const i64_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const box_i64_var = try type_store.freshFromContent(.{ .structure = .{ .box = i64_var } });
    const list_box_var = try type_store.freshFromContent(.{ .structure = .{ .list = box_i64_var } });

    const result = try layout_store.addTypeVar(list_box_var, &type_scope);
    const result_layout = layout_store.getLayout(result);

    // Outer list should use index approach since Box(I64) is not a scalar
    try testing.expect(result_layout.tag == .list);

    // Inner box should use box with i64 sentinel
    const box_layout = layout_store.getLayout(result_layout.data.list);
    try testing.expect(box_layout.tag == .box);
    try testing.expectEqual(layout.Idx.i64, box_layout.data.box);
}

test "addTypeVar - all integer precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    const int_precisions = [_]types.Num.Int.Precision{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 };

    for (int_precisions) |precision| {
        // Test Box(IntType)
        const int_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = precision } } } });
        const box_int_var = try type_store.freshFromContent(.{ .structure = .{ .box = int_var } });

        const box_result = try layout_store.addTypeVar(box_int_var, &type_scope);
        const box_layout = layout_store.getLayout(box_result);

        try testing.expect(box_layout.tag == .box);
        // Check that the box contains the correct sentinel for this integer type
        const expected_idx = switch (precision) {
            .u8 => layout.Idx.u8,
            .i8 => layout.Idx.i8,
            .u16 => layout.Idx.u16,
            .i16 => layout.Idx.i16,
            .u32 => layout.Idx.u32,
            .i32 => layout.Idx.i32,
            .u64 => layout.Idx.u64,
            .i64 => layout.Idx.i64,
            .u128 => layout.Idx.u128,
            .i128 => layout.Idx.i128,
        };
        try testing.expectEqual(expected_idx, box_layout.data.box);

        // Test List(IntType)
        const list_int_var = try type_store.freshFromContent(.{ .structure = .{ .list = int_var } });

        const list_result = try layout_store.addTypeVar(list_int_var, &type_scope);
        const list_layout = layout_store.getLayout(list_result);

        try testing.expect(list_layout.tag == .list);
        try testing.expectEqual(expected_idx, list_layout.data.list);
    }
}

test "addTypeVar - all boolean precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Test Box(Bool)
    {
        // Create a bool layout directly and insert it
        const bool_layout = layout.Layout.boolType();
        const bool_idx = try layout_store.insertLayout(bool_layout);

        // Create a box layout containing the bool
        const box_layout = layout.Layout.box(bool_idx);
        const box_idx = try layout_store.insertLayout(box_layout);

        const result_layout = layout_store.getLayout(box_idx);

        try testing.expect(result_layout.tag == .box);
        try testing.expectEqual(layout.Idx.bool, result_layout.data.box);
    }

    // Test List(Bool)
    {
        // Create a bool layout directly and insert it
        const bool_layout = layout.Layout.boolType();
        const bool_idx = try layout_store.insertLayout(bool_layout);

        // Create a list layout containing the bool
        const list_layout = layout.Layout.list(bool_idx);
        const list_idx = try layout_store.insertLayout(list_layout);

        const result_layout = layout_store.getLayout(list_idx);

        try testing.expect(result_layout.tag == .list);
        try testing.expectEqual(layout.Idx.bool, result_layout.data.list);
    }
}

test "addTypeVar - all frac precisions use scalar optimization" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    const frac_precisions = [_]types.Num.Frac.Precision{ .f32, .f64, .dec };

    for (frac_precisions) |precision| {
        // Test Box(FracType)
        const frac_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = precision } } } });
        const box_frac_var = try type_store.freshFromContent(.{ .structure = .{ .box = frac_var } });

        const box_result = try layout_store.addTypeVar(box_frac_var, &type_scope);
        const box_layout = layout_store.getLayout(box_result);

        try testing.expect(box_layout.tag == .box);
        // Check that the box contains the correct sentinel for this float type
        const expected_idx = switch (precision) {
            .f32 => layout.Idx.f32,
            .f64 => layout.Idx.f64,
            .dec => layout.Idx.dec,
        };
        try testing.expectEqual(expected_idx, box_layout.data.box);

        // Test List(FracType)
        const list_frac_var = try type_store.freshFromContent(.{ .structure = .{ .list = frac_var } });

        const list_result = try layout_store.addTypeVar(list_frac_var, &type_scope);
        const list_layout = layout_store.getLayout(list_result);

        try testing.expect(list_layout.tag == .list);
        try testing.expectEqual(expected_idx, list_layout.data.list);
    }
}

test "layouts_by_var uses ArrayListMap with pre-allocation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    // Create layout store with types store
    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create some type variables
    const str_var = try type_store.freshFromContent(.{ .structure = .str });
    const num_var = try type_store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const list_var = try type_store.freshFromContent(.{ .structure = .{ .list = num_var } });

    // Convert to layouts
    _ = try layout_store.addTypeVar(str_var, &type_scope);
    _ = try layout_store.addTypeVar(num_var, &type_scope);
    _ = try layout_store.addTypeVar(list_var, &type_scope);

    // Verify variables were created
    try testing.expect(str_var != num_var);
    try testing.expect(num_var != list_var);
    try testing.expect(str_var != list_var);

    // Verify the ArrayListMap was initialized with capacity
    // The length should be at least as large as the types store
    try testing.expect(layout_store.layouts_by_var.entries.len >= type_store.slots.backing.len());
}

test "idxFromScalar - arithmetic mapping with no branches" {
    const testing = std.testing;

    // Test every possible scalar type to ensure arithmetic mapping is correct

    // bool
    {
        const scalar = layout.Scalar{ .data = .{ .bool = {} }, .tag = .bool };
        const idx = Store.idxFromScalar(scalar);
        try testing.expectEqual(layout.Idx.bool, idx);
    }

    // str
    {
        const scalar = layout.Scalar{ .data = .{ .str = {} }, .tag = .str };
        const idx = Store.idxFromScalar(scalar);
        try testing.expectEqual(layout.Idx.str, idx);
    }

    // opaque_ptr
    {
        const scalar = layout.Scalar{ .data = .{ .opaque_ptr = {} }, .tag = .opaque_ptr };
        const idx = Store.idxFromScalar(scalar);
        try testing.expectEqual(layout.Idx.opaque_ptr, idx);
    }

    // int
    {
        const int_tests = [_]struct { precision: types.Num.Int.Precision, expected: layout.Idx }{
            .{ .precision = .u8, .expected = .u8 },
            .{ .precision = .i8, .expected = .i8 },
            .{ .precision = .u16, .expected = .u16 },
            .{ .precision = .i16, .expected = .i16 },
            .{ .precision = .u32, .expected = .u32 },
            .{ .precision = .i32, .expected = .i32 },
            .{ .precision = .u64, .expected = .u64 },
            .{ .precision = .i64, .expected = .i64 },
            .{ .precision = .u128, .expected = .u128 },
            .{ .precision = .i128, .expected = .i128 },
        };

        for (int_tests) |test_case| {
            const scalar = layout.Scalar{ .data = .{ .int = test_case.precision }, .tag = .int };
            const idx = Store.idxFromScalar(scalar);
            try testing.expectEqual(test_case.expected, idx);
        }
    }

    // frac
    {
        const frac_tests = [_]struct { precision: types.Num.Frac.Precision, expected: layout.Idx }{
            .{ .precision = .f32, .expected = .f32 },
            .{ .precision = .f64, .expected = .f64 },
            .{ .precision = .dec, .expected = .dec },
        };

        for (frac_tests) |test_case| {
            const scalar = layout.Scalar{ .data = .{ .frac = test_case.precision }, .tag = .frac };
            const idx = Store.idxFromScalar(scalar);
            try testing.expectEqual(test_case.expected, idx);
        }
    }

    // Verify the arithmetic mapping directly
    // bool (tag 0) -> idx 0
    try testing.expectEqual(@as(u28, 0), @intFromEnum(layout.Idx.bool));

    // str (tag 1) -> idx 1
    try testing.expectEqual(@as(u28, 1), @intFromEnum(layout.Idx.str));

    // opaque_ptr (tag 2) -> idx 2
    try testing.expectEqual(@as(u28, 2), @intFromEnum(layout.Idx.opaque_ptr));

    // int (tag 3) with precision 0-9 -> idx 3-12
    try testing.expectEqual(@as(u28, 3), @intFromEnum(layout.Idx.u8)); // 3 + 0
    try testing.expectEqual(@as(u28, 4), @intFromEnum(layout.Idx.i8)); // 3 + 1
    try testing.expectEqual(@as(u28, 5), @intFromEnum(layout.Idx.u16)); // 3 + 2
    try testing.expectEqual(@as(u28, 6), @intFromEnum(layout.Idx.i16)); // 3 + 3
    try testing.expectEqual(@as(u28, 7), @intFromEnum(layout.Idx.u32)); // 3 + 4
    try testing.expectEqual(@as(u28, 8), @intFromEnum(layout.Idx.i32)); // 3 + 5
    try testing.expectEqual(@as(u28, 9), @intFromEnum(layout.Idx.u64)); // 3 + 6
    try testing.expectEqual(@as(u28, 10), @intFromEnum(layout.Idx.i64)); // 3 + 7
    try testing.expectEqual(@as(u28, 11), @intFromEnum(layout.Idx.u128)); // 3 + 8
    try testing.expectEqual(@as(u28, 12), @intFromEnum(layout.Idx.i128)); // 3 + 9

    // frac (tag 4) with precision 2-4 -> idx 13-15
    try testing.expectEqual(@as(u28, 13), @intFromEnum(layout.Idx.f32)); // 13 + (2 - 2)
    try testing.expectEqual(@as(u28, 14), @intFromEnum(layout.Idx.f64)); // 13 + (3 - 2)
    try testing.expectEqual(@as(u28, 15), @intFromEnum(layout.Idx.dec)); // 13 + (4 - 2)
}

test "putRecord and getRecordFieldOffsetByName" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = try ModuleEnv.init(gpa, "");
    defer module_env.deinit();

    var type_store = try types_store.Store.init(gpa);
    defer type_store.deinit();

    var layout_store = try Store.init(&module_env, &type_store);

    // Create empty type scope
    var type_scope = TypeScope.init(gpa);
    defer type_scope.deinit();
    defer layout_store.deinit();

    // Create a record with out-of-order fields
    const field_layouts = &[_]layout.Layout{
        layout.Layout.int(.u8),
        layout.Layout.int(.u64),
        layout.Layout.int(.u32),
    };

    const b_ident = try module_env.insertIdent(Ident.for_text("b"));
    const a_ident = try module_env.insertIdent(Ident.for_text("a"));
    const c_ident = try module_env.insertIdent(Ident.for_text("c"));

    const field_names = &[_]Ident.Idx{
        b_ident,
        a_ident,
        c_ident,
    };

    const record_layout_idx = try layout_store.putRecord(field_layouts, field_names);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.tag == .record);

    const record_idx = record_layout.data.record.idx;

    // Fields should be sorted by alignment, then name.
    // Expected order: a (u64), c (u32), b (u8)
    // Offsets:
    // a: 0
    // c: 8
    // b: 12
    const offset_a = layout_store.getRecordFieldOffsetByName(record_idx, "a").?;
    try testing.expectEqual(@as(u32, 0), offset_a);

    const offset_c = layout_store.getRecordFieldOffsetByName(record_idx, "c").?;
    try testing.expectEqual(@as(u32, 8), offset_c);

    const offset_b = layout_store.getRecordFieldOffsetByName(record_idx, "b").?;
    try testing.expectEqual(@as(u32, 12), offset_b);

    // Test non-existent field
    try testing.expectEqual(null, layout_store.getRecordFieldOffsetByName(record_idx, "d"));
}
