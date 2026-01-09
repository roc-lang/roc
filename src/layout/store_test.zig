//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const layout = @import("layout.zig");
const layout_store_ = @import("store.zig");
const ModuleEnv = @import("can").ModuleEnv;

const types_store = types.store;
const Ident = base.Ident;
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
    /// Note: Caller must have already inserted "Box" and "Builtin" idents and set builtin_module_ident
    fn mkBoxType(self: *LayoutTest, elem_var: types.Var, box_ident_idx: base.Ident.Idx, builtin_module_idx: base.Ident.Idx) !types.Var {
        const box_content = try self.type_store.mkNominal(
            .{ .ident_idx = box_ident_idx },
            elem_var,
            &[_]types.Var{elem_var},
            builtin_module_idx,
            false,
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
    try testing.expectEqual(layout.ScalarTag.int, retrieved_layout.data.scalar.tag);
    try testing.expectEqual(types.Int.Precision.u8, retrieved_layout.data.scalar.data.int);
    try testing.expectEqual(@as(u32, 1), lt.layout_store.layoutSize(retrieved_layout));
}

test "addTypeVar - host opaque types compile to opaque_ptr" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Set up builtin module ident and Box ident for Box recognition
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box")); // Insert Box ident first
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Box of flex_var
    const flex_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const box_flex_var = try lt.mkBoxType(flex_var, box_ident_idx, builtin_module_idx);
    const box_flex_idx = try lt.layout_store.addTypeVar(box_flex_var, &lt.type_scope);
    const box_flex_layout = lt.layout_store.getLayout(box_flex_idx);
    try testing.expect(box_flex_layout.tag == .box);
    try testing.expectEqual(layout.Idx.opaque_ptr, box_flex_layout.data.box);

    // Box of rigid_var
    const ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("a"));
    const rigid_var = try lt.type_store.freshFromContent(.{ .rigid = types.Rigid.init(ident_idx) });
    const box_rigid_var = try lt.mkBoxType(rigid_var, box_ident_idx, builtin_module_idx);
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

    // Setup identifiers BEFORE Store.init so list_ident and box_ident get set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const box_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Box")); // Insert Box ident for box_ident lookup
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.idents.builtin_module = builtin_module_idx;

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
    const box_zst_var = try lt.mkBoxType(empty_record_var, box_ident_idx, builtin_module_idx);
    const box_zst_idx = try lt.layout_store.addTypeVar(box_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_zst_idx).tag == .box_of_zst);

    const list_zst_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        empty_tag_union_var,
        &[_]types.Var{empty_tag_union_var},
        builtin_module_idx,
        false,
    );
    const list_zst_var = try lt.type_store.freshFromContent(list_zst_content);
    const list_zst_idx = try lt.layout_store.addTypeVar(list_zst_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(list_zst_idx).tag == .list_of_zst);
}

test "addTypeVar - record with only zero-sized fields" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Set up builtin module ident and Box ident for Box recognition
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box")); // Insert Box ident first
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

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
    const box_record_var = try lt.mkBoxType(record_var, box_ident_idx, builtin_module_idx);
    const box_idx = try lt.layout_store.addTypeVar(box_record_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
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
    // Test: List(Box(List(Box(empty_record))))
    // Expected layout chain: list -> box -> list -> box_of_zst
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident and box_ident get set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const box_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Box")); // Insert Box ident for box_ident lookup
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create List(Box(List(Box(empty_record))))
    const empty_record = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_box = try lt.mkBoxType(empty_record, box_ident_idx, builtin_module_idx);
    const inner_list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        inner_box,
        &[_]types.Var{inner_box},
        builtin_module_idx,
        false,
    );
    const inner_list = try lt.type_store.freshFromContent(inner_list_content);
    const outer_box = try lt.mkBoxType(inner_list, box_ident_idx, builtin_module_idx);
    const outer_list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        outer_box,
        &[_]types.Var{outer_box},
        builtin_module_idx,
        false,
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

    // Setup identifiers BEFORE Store.init so list_ident and box_ident get set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box")); // Insert Box ident for box_ident lookup
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = empty_record_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });

    // List of this record should be list_of_zst since the record only has ZST fields
    const list_content = try lt.type_store.mkNominal(.{ .ident_idx = list_ident_idx }, record_var, &[_]types.Var{record_var}, builtin_module_idx, false);
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

    // Set up builtin module ident and Box ident for Box recognition
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box")); // Insert Box ident first
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

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
    const box_var = try lt.mkBoxType(tuple_var, box_ident_idx, builtin_module_idx);
    const box_idx = try lt.layout_store.addTypeVar(box_var, &lt.type_scope);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
}

test "nested ZST detection - deeply nested" {
    // Test: List({ field: ({ field2: {} }, ()) }) should be list_of_zst
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init so list_ident and box_ident get set correctly
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box")); // Insert Box ident for box_ident lookup
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    // Set the builtin_module_ident so the layout store can recognize Builtin types
    lt.module_env.idents.builtin_module = builtin_module_idx;

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
    const list_content = try lt.type_store.mkNominal(.{ .ident_idx = list_ident_idx }, outer_record_var, &[_]types.Var{outer_record_var}, builtin_module_idx, false);
    const list_var = try lt.type_store.freshFromContent(list_content);
    const list_idx = try lt.layout_store.addTypeVar(list_var, &lt.type_scope);

    // Since the entire nested structure is ZST, the list should be list_of_zst
    try testing.expect(lt.layout_store.getLayout(list_idx).tag == .list_of_zst);
}

test "addTypeVar - flex var with method constraint returning open tag union" {
    // This test verifies that layout computation handles method constraints
    // with open tag unions correctly. The scenario is:
    // 1. Method syntax creates a flex var with a StaticDispatchConstraint
    // 2. The constraint's fn_var points to: List(a) -> Try(a, [ListWasEmpty, ..others])
    // 3. The ..others is a flex var extension on the tag union
    //
    // The actual fix for List.first() method syntax was in the interpreter
    // (unifying the method's parameter type with the receiver type), but this
    // test ensures the layout store handles such types correctly.
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers BEFORE Store.init
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const try_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Try"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    const first_ident_idx = try lt.module_env.insertIdent(Ident.for_text("first"));

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create the element type variable `a` (will be the list element)
    const elem_var = try lt.type_store.fresh();

    // Create List(a)
    const list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        elem_var,
        &[_]types.Var{elem_var},
        builtin_module_idx,
        false,
    );
    const list_var = try lt.type_store.freshFromContent(list_content);

    // Create [ListWasEmpty, ..others] - open tag union with flex extension
    const others_flex_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const list_was_empty_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("ListWasEmpty")),
        .args = types.Var.SafeList.Range.empty(),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{list_was_empty_tag});
    const error_tag_union = types.TagUnion{ .tags = tags_range, .ext = others_flex_var };
    const error_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = error_tag_union } });

    // Create Try(a, [ListWasEmpty, ..others]) as a nominal type wrapping [Ok(a), Err([ListWasEmpty, ..others])]
    const ok_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Ok")),
        .args = try lt.type_store.appendVars(&[_]types.Var{elem_var}),
    };
    const err_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Err")),
        .args = try lt.type_store.appendVars(&[_]types.Var{error_tag_union_var}),
    };
    const try_tags_range = try lt.type_store.appendTags(&[_]types.Tag{ ok_tag, err_tag });
    const try_backing_tag_union = types.TagUnion{
        .tags = try_tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const try_backing_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = try_backing_tag_union } });
    const try_content = try lt.type_store.mkNominal(
        .{ .ident_idx = try_ident_idx },
        try_backing_var,
        &[_]types.Var{ elem_var, error_tag_union_var },
        builtin_module_idx,
        false,
    );
    const try_var = try lt.type_store.freshFromContent(try_content);

    // Create function type: List(a) -> Try(a, [ListWasEmpty, ..others])
    const fn_content = try lt.type_store.mkFuncPure(&[_]types.Var{list_var}, try_var);
    const fn_var = try lt.type_store.freshFromContent(fn_content);

    // Create StaticDispatchConstraint for `.first` method
    const first_constraint = types.StaticDispatchConstraint{
        .fn_name = first_ident_idx,
        .fn_var = fn_var,
        .origin = .method_call,
    };
    const constraints_range = try lt.type_store.appendStaticDispatchConstraints(&[_]types.StaticDispatchConstraint{first_constraint});

    // Create flex var with the constraint (this is what method syntax produces)
    const constrained_flex = try lt.type_store.freshFromContent(.{
        .flex = types.Flex.init().withConstraints(constraints_range),
    });

    // Now create a List with this constrained flex element
    const outer_list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        constrained_flex,
        &[_]types.Var{constrained_flex},
        builtin_module_idx,
        false,
    );
    const outer_list_var = try lt.type_store.freshFromContent(outer_list_content);

    // This should NOT cause an infinite loop - should handle the open tag union extension properly
    const result_idx = try lt.layout_store.addTypeVar(outer_list_var, &lt.type_scope);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // The list should have a valid layout - either list or list_of_zst
    // The flex var with a constraint should be treated as ZST (since no from_numeral constraint)
    try testing.expect(result_layout.tag == .list or result_layout.tag == .list_of_zst);

    // Also test computing layout of the Try return type directly
    // This is what would happen when evaluating the result of list.first()
    const try_result_idx = try lt.layout_store.addTypeVar(try_var, &lt.type_scope);
    const try_result_layout = lt.layout_store.getLayout(try_result_idx);
    // Try should be a tag_union
    try testing.expect(try_result_layout.tag == .tag_union);
}

test "addTypeVar - type alias inside Try nominal (issue #8708)" {
    // Regression test for issue #8708:
    // Using a type alias as a type argument to Try caused TypeContainedMismatch error.
    //
    // The bug was that aliases were added to in_progress_vars during layout computation
    // but never removed (because alias handling just continues to the backing type).
    // This caused spurious cycle detection when the alias was encountered again.
    //
    // Example Roc code that triggered the bug:
    //   TokenContents : [EndOfFileToken]
    //   get_val : {} -> Try(TokenContents, Str)

    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers
    const try_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Try"));
    const token_contents_ident_idx = try lt.module_env.insertIdent(Ident.for_text("TokenContents"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create the underlying tag union: [EndOfFileToken]
    const end_of_file_token_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("EndOfFileToken")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const token_tags_range = try lt.type_store.appendTags(&[_]types.Tag{end_of_file_token_tag});
    const token_tag_union = types.TagUnion{
        .tags = token_tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const token_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = token_tag_union } });

    // Create the alias: TokenContents : [EndOfFileToken]
    const alias_content = try lt.type_store.mkAlias(
        .{ .ident_idx = token_contents_ident_idx },
        token_tag_union_var,
        &[_]types.Var{},
    );
    const token_contents_alias_var = try lt.type_store.freshFromContent(alias_content);

    // Create an error type (Str is common for errors)
    const str_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record }); // simplified

    // Create Try backing: [Ok(TokenContents), Err(Str)]
    const ok_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Ok")),
        .args = try lt.type_store.appendVars(&[_]types.Var{token_contents_alias_var}),
    };
    const err_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Err")),
        .args = try lt.type_store.appendVars(&[_]types.Var{str_var}),
    };
    const try_tags_range = try lt.type_store.appendTags(&[_]types.Tag{ ok_tag, err_tag });
    const try_backing_tag_union = types.TagUnion{
        .tags = try_tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const try_backing_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = try_backing_tag_union } });

    // Create the Try nominal type: Try(TokenContents, Str)
    const try_content = try lt.type_store.mkNominal(
        .{ .ident_idx = try_ident_idx },
        try_backing_var,
        &[_]types.Var{ token_contents_alias_var, str_var },
        builtin_module_idx,
        false,
    );
    const try_var = try lt.type_store.freshFromContent(try_content);

    // This should succeed without TypeContainedMismatch error.
    // Before the fix, this would fail because the alias was incorrectly detected as a cycle.
    const result_idx = try lt.layout_store.addTypeVar(try_var, &lt.type_scope);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // Try should have a tag_union layout
    try testing.expect(result_layout.tag == .tag_union);
}

test "addTypeVar - recursive nominal type with nested Box at depth 2+ (issue #8816)" {
    // Regression test for issue #8816:
    // Recursive nominal types where the recursion goes through Box at depth 2+
    // would cause a segfault during layout computation.
    //
    // The bug was that when computing the layout of a recursive type inside a Box,
    // we would try to create a placeholder for the raw layout (not the boxed layout),
    // but the raw_layout_placeholders cache was missing, causing the placeholder lookup
    // to fail when we encountered the recursive type at depth 2+.
    //
    // Example Roc code that triggered the bug:
    //   RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))]
    //   depth2 = RichDoc.Wrapped(Box.box(RichDoc.Wrapped(Box.box(RichDoc.PlainText("two")))))

    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers
    const rich_doc_ident_idx = try lt.module_env.insertIdent(Ident.for_text("RichDoc"));
    const box_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create a recursive type: RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))]
    // We create the recursive reference by first creating a flex var, then updating it
    // to point to the nominal type content after we've created the full structure.

    // Create a fresh var for the recursive reference
    const recursive_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });

    // Create Box(recursive_var) - this references the recursive var before we define the nominal
    const box_content = try lt.type_store.mkNominal(
        .{ .ident_idx = box_ident_idx },
        recursive_var,
        &[_]types.Var{recursive_var},
        builtin_module_idx,
        false,
    );
    const box_recursive_var = try lt.type_store.freshFromContent(box_content);

    // Create Str (simplified as empty record for this test)
    const str_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });

    // Create [PlainText(Str), Wrapped(Box(RichDoc))]
    const plain_text_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("PlainText")),
        .args = try lt.type_store.appendVars(&[_]types.Var{str_var}),
    };
    const wrapped_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Wrapped")),
        .args = try lt.type_store.appendVars(&[_]types.Var{box_recursive_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{ plain_text_tag, wrapped_tag });
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    // Create the nominal type content: RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))]
    const rich_doc_content = try lt.type_store.mkNominal(
        .{ .ident_idx = rich_doc_ident_idx },
        tag_union_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    );

    // Close the recursive loop by updating the recursive_var to point to the nominal content
    try lt.type_store.setVarContent(recursive_var, rich_doc_content);

    // Also create a fresh var with the content for testing (layout computation will follow the recursion)
    const rich_doc_var = try lt.type_store.freshFromContent(rich_doc_content);

    // This should succeed without segfault.
    // Before the fix, this would fail when computing the layout for depth 2+ nesting.
    const result_idx = try lt.layout_store.addTypeVar(rich_doc_var, &lt.type_scope);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // RichDoc should have a tag_union layout (since the nominal wraps a tag union)
    try testing.expect(result_layout.tag == .tag_union);
}

test "layoutSizeAlign - recursive nominal type with record containing List (issue #8923)" {
    // Regression test for issue #8923:
    // Recursive nominal types where the recursion goes through a record containing
    // List of the recursive type would cause infinite recursion in layoutSizeAlign.
    //
    // The bug was that layoutSizeAlign was dynamically computing sizes for records
    // and tag unions by recursively calling itself on field layouts, which caused
    // infinite recursion when the type contained itself through a List in a record.
    //
    // The fix was to use pre-computed sizes from RecordData.size, TupleData.size,
    // and TagUnionData.size instead of dynamically computing them.
    //
    // Example Roc code that triggered the bug:
    //   Statement := [
    //       FuncCall({ name: Str, args: List(U64) }),
    //       ForLoop({ identifiers: List(Str), block: List(Statement) }),  # Recursive!
    //   ]

    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers
    const statement_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Statement"));
    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.layout_store = try Store.init(&lt.module_env, &lt.type_store, null);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create a recursive type: Statement := [FuncCall({...}), ForLoop({block: List(Statement)})]
    // We create the recursive reference by first creating a flex var, then updating it
    // to point to the nominal type content after we've created the full structure.

    // Create a fresh var for the recursive reference
    const recursive_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });

    // Create List(recursive_var) - this is the key difference from issue #8816
    // The recursion goes through List in a record field, not through Box
    const list_recursive_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        recursive_var,
        &[_]types.Var{recursive_var},
        builtin_module_idx,
        false,
    );
    const list_recursive_var = try lt.type_store.freshFromContent(list_recursive_content);

    // Create a record { block: List(Statement) }
    const block_field_ident = try lt.module_env.insertIdent(Ident.for_text("block"));
    const empty_record = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const for_loop_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = block_field_ident, .var_ = list_recursive_var },
    });
    const for_loop_record_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .record = .{ .fields = for_loop_fields, .ext = empty_record } },
    });

    // Create a simple record for FuncCall { name: Str } (simplified)
    const name_field_ident = try lt.module_env.insertIdent(Ident.for_text("name"));
    const str_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record }); // Simplified Str
    const func_call_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = name_field_ident, .var_ = str_var },
    });
    const func_call_record_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .record = .{ .fields = func_call_fields, .ext = empty_record } },
    });

    // Create [FuncCall({...}), ForLoop({block: List(Statement)})]
    const func_call_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("FuncCall")),
        .args = try lt.type_store.appendVars(&[_]types.Var{func_call_record_var}),
    };
    const for_loop_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("ForLoop")),
        .args = try lt.type_store.appendVars(&[_]types.Var{for_loop_record_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{ func_call_tag, for_loop_tag });
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    // Create the nominal type content: Statement := [FuncCall({...}), ForLoop({block: List(Statement)})]
    const statement_content = try lt.type_store.mkNominal(
        .{ .ident_idx = statement_ident_idx },
        tag_union_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    );

    // Close the recursive loop by updating the recursive_var to point to the nominal content
    try lt.type_store.setVarContent(recursive_var, statement_content);

    // Create a fresh var with the content for testing
    const statement_var = try lt.type_store.freshFromContent(statement_content);

    // This should succeed without infinite recursion.
    // Before the fix, layoutSizeAlign would infinitely recurse when computing the size.
    const result_idx = try lt.layout_store.addTypeVar(statement_var, &lt.type_scope);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // Statement should have a tag_union layout (since the nominal wraps a tag union)
    try testing.expect(result_layout.tag == .tag_union);

    // Verify layoutSizeAlign works without infinite recursion by calling layoutSize
    // (which internally calls layoutSizeAlign)
    const size = lt.layout_store.layoutSize(result_layout);
    // The size should be > 0 (a tag union with payloads has non-zero size)
    try testing.expect(size > 0);
}
