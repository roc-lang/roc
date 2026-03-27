//! Tests for the layout store
//! These tests cover various scenarios including boundary conditions, error cases, and complex type layouts.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const mir = @import("mir");
const layout = @import("layout.zig");
const layout_graph_ = @import("graph.zig");
const layout_store_ = @import("store.zig");
const type_layout_resolver_ = @import("type_layout_resolver.zig");
const mir_monotype_resolver_ = @import("mir_monotype_resolver.zig");
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
    module_env_ptr: [1]*const ModuleEnv = undefined, // Backing storage for all_module_envs
    type_store: types_store.Store,
    layout_store: Store,
    type_scope: TypeScope,

    fn init(gpa: std.mem.Allocator) !LayoutTest {
        var result: LayoutTest = undefined;
        result.gpa = gpa;
        result.module_env = try ModuleEnv.init(gpa, "");
        try result.module_env.initModuleEnvFields("LayoutTest");
        result.type_store = try types_store.Store.init(gpa);
        result.type_scope = TypeScope.init(gpa);
        // Note: module_env_ptr must be set AFTER the struct is in its final location
        // (after the function returns), otherwise the pointer becomes stale.
        // For simple init, we call initLayoutStore after return.
        return result;
    }

    fn initWithIdents(gpa: std.mem.Allocator) !LayoutTest {
        var result: LayoutTest = undefined;
        result.gpa = gpa;
        result.module_env = try ModuleEnv.init(gpa, "");
        try result.module_env.initModuleEnvFields("LayoutTest");
        result.type_store = try types_store.Store.init(gpa);
        result.type_scope = TypeScope.init(gpa);
        // Note: layout_store and module_env_ptr should be initialized AFTER
        // idents are set up AND after the struct is in its final location.
        return result;
    }

    fn initLayoutStore(self: *LayoutTest) !void {
        // Set module_env_ptr HERE, after the struct is in its final memory location.
        // Setting it in init/initWithIdents causes stale pointer bugs since the
        // struct is moved when returned.
        self.module_env_ptr[0] = &self.module_env;
        self.layout_store = try Store.init(&self.module_env_ptr, null, self.gpa, base.target.TargetUsize.native);
    }

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

fn expectTypeAndMonotypeResolversAgree(
    allocator: std.mem.Allocator,
    lt: *LayoutTest,
    type_var: types.Var,
) !void {
    var type_layout_resolver = type_layout_resolver_.Resolver.init(&lt.layout_store);
    defer type_layout_resolver.deinit();
    type_layout_resolver.setOverrideTypesStore(&lt.type_store);
    const type_layout_idx = try type_layout_resolver.resolve(0, type_var, &lt.type_scope, null);

    var mono_store = try mir.Monotype.Store.init(allocator);
    defer mono_store.deinit(allocator);

    var scratches = try mir.Monotype.Store.Scratches.init(allocator);
    defer scratches.deinit();
    scratches.ident_store = lt.module_env.getIdentStoreConst();
    scratches.module_env = &lt.module_env;
    scratches.module_idx = 0;
    scratches.all_module_envs = &lt.module_env_ptr;

    var specializations = std.AutoHashMap(types.Var, mir.Monotype.Idx).init(allocator);
    defer specializations.deinit();
    var nominal_cycle_breakers = std.AutoHashMap(types.Var, mir.Monotype.Idx).init(allocator);
    defer nominal_cycle_breakers.deinit();

    const mono_idx = try mono_store.fromTypeVar(
        allocator,
        &lt.type_store,
        type_var,
        lt.module_env.idents,
        &specializations,
        &nominal_cycle_breakers,
        &scratches,
    );

    var mir_layout_resolver = mir_monotype_resolver_.Resolver.init(allocator, &mono_store, &lt.layout_store);
    defer mir_layout_resolver.deinit();
    const mono_layout_idx = try mir_layout_resolver.resolve(mono_idx, null);

    try testing.expectEqual(type_layout_idx, mono_layout_idx);
}

fn resolveTypeVar(lt: *LayoutTest, type_var: types.Var) !layout.Idx {
    var type_layout_resolver = type_layout_resolver_.Resolver.init(&lt.layout_store);
    defer type_layout_resolver.deinit();
    type_layout_resolver.setOverrideTypesStore(&lt.type_store);
    return type_layout_resolver.resolve(0, type_var, &lt.type_scope, null);
}

const ZstLeaf = enum {
    unit,
    u64,
    str,
};

const ZstWrapper = enum {
    record1,
    record2_zst,
    tuple1,
    tuple2_zst,
    tag1,
    tag1_pair_with_zst,
};

const ZstMatrixCase = struct {
    name: []const u8,
    wrappers: []const ZstWrapper,
    leaf: ZstLeaf,
    expect_zero_sized: bool,
};

fn mkBuiltinType0(lt: *LayoutTest, builtin_ident_idx: base.Ident.Idx, builtin_module_idx: base.Ident.Idx) !types.Var {
    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const nominal = try lt.type_store.mkNominal(
        .{ .ident_idx = builtin_ident_idx },
        unit_var,
        &.{},
        builtin_module_idx,
        false,
    );
    return try lt.type_store.freshFromContent(nominal);
}

fn mkLeafVar(lt: *LayoutTest, builtin_module_idx: base.Ident.Idx, leaf: ZstLeaf) !types.Var {
    return switch (leaf) {
        .unit => try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
        .u64 => try mkBuiltinType0(lt, lt.module_env.idents.u64_type, builtin_module_idx),
        .str => try mkBuiltinType0(lt, lt.module_env.idents.str, builtin_module_idx),
    };
}

fn wrapZstShape(lt: *LayoutTest, inner: types.Var, wrapper: ZstWrapper) !types.Var {
    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });

    return switch (wrapper) {
        .record1 => blk: {
            const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
                .{ .name = try lt.module_env.insertIdent(Ident.for_text("value")), .var_ = inner },
            });
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{
                .fields = fields,
                .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
            } } });
        },
        .record2_zst => blk: {
            const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
                .{ .name = try lt.module_env.insertIdent(Ident.for_text("value")), .var_ = inner },
                .{ .name = try lt.module_env.insertIdent(Ident.for_text("phantom")), .var_ = empty_record_var },
            });
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{
                .fields = fields,
                .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
            } } });
        },
        .tuple1 => blk: {
            const elems = try lt.type_store.vars.appendSlice(lt.gpa, &[_]types.Var{inner});
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems } } });
        },
        .tuple2_zst => blk: {
            const elems = try lt.type_store.vars.appendSlice(lt.gpa, &[_]types.Var{ inner, empty_record_var });
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems } } });
        },
        .tag1 => blk: {
            const tag = types.Tag{
                .name = try lt.module_env.insertIdent(Ident.for_text("Only")),
                .args = try lt.type_store.appendVars(&[_]types.Var{inner}),
            };
            const tags = try lt.type_store.appendTags(&[_]types.Tag{tag});
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = .{
                .tags = tags,
                .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
            } } });
        },
        .tag1_pair_with_zst => blk: {
            const tag = types.Tag{
                .name = try lt.module_env.insertIdent(Ident.for_text("Pair")),
                .args = try lt.type_store.appendVars(&[_]types.Var{ inner, empty_record_var }),
            };
            const tags = try lt.type_store.appendTags(&[_]types.Tag{tag});
            break :blk try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = .{
                .tags = tags,
                .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
            } } });
        },
    };
}

fn buildWrappedZstCase(
    lt: *LayoutTest,
    builtin_module_idx: base.Ident.Idx,
    leaf: ZstLeaf,
    wrappers: []const ZstWrapper,
) !types.Var {
    var current = try mkLeafVar(lt, builtin_module_idx, leaf);
    for (wrappers) |wrapper| {
        current = try wrapZstShape(lt, current, wrapper);
    }
    return current;
}

fn expectZstContainerSpecialization(
    lt: *LayoutTest,
    builtin_module_idx: base.Ident.Idx,
    list_ident_idx: base.Ident.Idx,
    box_ident_idx: base.Ident.Idx,
    type_var: types.Var,
    expect_zero_sized: bool,
) !void {
    const resolved_idx = try resolveTypeVar(lt, type_var);
    const resolved_layout = lt.layout_store.getLayout(resolved_idx);
    try testing.expectEqual(expect_zero_sized, lt.layout_store.layoutSize(resolved_layout) == 0);

    const list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        type_var,
        &[_]types.Var{type_var},
        builtin_module_idx,
        false,
    );
    const list_var = try lt.type_store.freshFromContent(list_content);
    const list_idx = try resolveTypeVar(lt, list_var);
    const list_layout = lt.layout_store.getLayout(list_idx);
    try testing.expectEqual(
        if (expect_zero_sized) layout.LayoutTag.list_of_zst else layout.LayoutTag.list,
        list_layout.tag,
    );

    const box_var = try lt.mkBoxType(type_var, box_ident_idx, builtin_module_idx);
    const box_idx = try resolveTypeVar(lt, box_var);
    const box_layout = lt.layout_store.getLayout(box_idx);
    try testing.expectEqual(
        if (expect_zero_sized) layout.LayoutTag.box_of_zst else layout.LayoutTag.box,
        box_layout.tag,
    );
}

test "fromTypeVar - bool type" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const bool_layout = layout.Layout.boolType();
    const bool_layout_idx = try lt.layout_store.insertLayout(bool_layout);

    try testing.expectEqual(layout.Idx.bool, bool_layout_idx);
    const retrieved_layout = lt.layout_store.getLayout(bool_layout_idx);
    try testing.expect(retrieved_layout.tag == .tag_union);
    const tu_data = lt.layout_store.getTagUnionData(retrieved_layout.data.tag_union.idx);
    try testing.expectEqual(@as(u8, 1), tu_data.discriminant_size);
    try testing.expectEqual(@as(u16, 0), tu_data.discriminant_offset);
    try testing.expectEqual(@as(u32, 2), tu_data.variants.count);
    try testing.expectEqual(@as(u32, 1), lt.layout_store.layoutSize(retrieved_layout));
}

test "putTagUnion interns two-nullary enums to canonical bool layout" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const enum_layout = try lt.layout_store.putTagUnion(&.{ .zst, .zst });
    try testing.expectEqual(layout.Idx.bool, enum_layout);
}

test "fromTypeVar - unresolved boxed type vars use box_of_zst" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    // Set up builtin module ident and Box ident for Box recognition
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box")); // Insert Box ident first
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    try lt.initLayoutStore();

    // Box of flex_var
    const flex_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const box_flex_var = try lt.mkBoxType(flex_var, box_ident_idx, builtin_module_idx);
    const box_flex_idx = try resolveTypeVar(&lt, box_flex_var);
    const box_flex_layout = lt.layout_store.getLayout(box_flex_idx);
    try testing.expect(box_flex_layout.tag == .box_of_zst);

    // Box of rigid_var
    const ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("a"));
    const rigid_var = try lt.type_store.freshFromContent(.{ .rigid = types.Rigid.init(ident_idx) });
    const box_rigid_var = try lt.mkBoxType(rigid_var, box_ident_idx, builtin_module_idx);
    const box_rigid_idx = try resolveTypeVar(&lt, box_rigid_var);
    const box_rigid_layout = lt.layout_store.getLayout(box_rigid_idx);
    try testing.expect(box_rigid_layout.tag == .box_of_zst);
}

test "fromTypeVar - zero-sized types (ZST)" {
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Bare ZSTs should return .zst layout
    const empty_record_idx = try resolveTypeVar(&lt, empty_record_var);
    try testing.expect(lt.layout_store.getLayout(empty_record_idx).tag == .zst);
    const empty_tag_union_idx = try resolveTypeVar(&lt, empty_tag_union_var);
    try testing.expect(lt.layout_store.getLayout(empty_tag_union_idx).tag == .zst);

    // ZSTs inside containers should use optimized layouts
    const box_zst_var = try lt.mkBoxType(empty_record_var, box_ident_idx, builtin_module_idx);
    const box_zst_idx = try resolveTypeVar(&lt, box_zst_var);
    try testing.expect(lt.layout_store.getLayout(box_zst_idx).tag == .box_of_zst);

    const list_zst_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        empty_tag_union_var,
        &[_]types.Var{empty_tag_union_var},
        builtin_module_idx,
        false,
    );
    const list_zst_var = try lt.type_store.freshFromContent(list_zst_content);
    const list_zst_idx = try resolveTypeVar(&lt, list_zst_var);
    try testing.expect(lt.layout_store.getLayout(list_zst_idx).tag == .list_of_zst);
}

test "fromTypeVar - record with only zero-sized fields" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Set up builtin module ident and Box ident for Box recognition
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box")); // Insert Box ident first
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a")), .var_ = empty_record_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b")), .var_ = empty_record_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_record_var } } });

    // Bare record with only ZST fields should create a record with ZST fields
    const record_idx = try resolveTypeVar(&lt, record_var);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .struct_);
    const field_slice = lt.layout_store.struct_fields.sliceRange(lt.layout_store.getStructData(record_layout.data.struct_.idx).getFields());
    try testing.expectEqual(@as(usize, 2), field_slice.len); // Both ZST fields are kept

    // Box of such a record should be box_of_zst since the record only contains ZST fields
    const box_record_var = try lt.mkBoxType(record_var, box_ident_idx, builtin_module_idx);
    const box_idx = try resolveTypeVar(&lt, box_record_var);
    try testing.expect(lt.layout_store.getLayout(box_idx).tag == .box_of_zst);
}

test "single-tag union with zero-sized payload keeps tag_union layout and size 0" {
    var lt = try LayoutTest.init(testing.allocator);
    defer lt.deinit();
    try lt.initLayoutStore();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const singleton_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("OneTag")),
        .args = try lt.type_store.appendVars(&[_]types.Var{empty_record_var}),
    };
    const tag_range = try lt.type_store.appendTags(&[_]types.Tag{singleton_tag});
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = tag_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    } } });

    const tag_union_idx = try resolveTypeVar(&lt, tag_union_var);
    const tag_union_layout = lt.layout_store.getLayout(tag_union_idx);
    try testing.expectEqual(layout.LayoutTag.tag_union, tag_union_layout.tag);
    try testing.expectEqual(@as(u32, 0), lt.layout_store.layoutSize(tag_union_layout));

    const tu_data = lt.layout_store.getTagUnionData(tag_union_layout.data.tag_union.idx);
    try testing.expectEqual(@as(u8, 0), tu_data.discriminant_size);
    try testing.expectEqual(@as(u16, 0), tu_data.discriminant_offset);
    try testing.expectEqual(@as(u32, 1), tu_data.variants.count);
}

test "single-tag union with non-zero-sized payload keeps tag_union layout and payload size" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const u64_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u64_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));

    const singleton_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("OneTag")),
        .args = try lt.type_store.appendVars(&[_]types.Var{u64_var}),
    };
    const tag_range = try lt.type_store.appendTags(&[_]types.Tag{singleton_tag});
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = tag_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    } } });

    const tag_union_idx = try resolveTypeVar(&lt, tag_union_var);
    const tag_union_layout = lt.layout_store.getLayout(tag_union_idx);
    try testing.expectEqual(layout.LayoutTag.tag_union, tag_union_layout.tag);
    try testing.expectEqual(@as(u32, 8), lt.layout_store.layoutSize(tag_union_layout));

    const tu_data = lt.layout_store.getTagUnionData(tag_union_layout.data.tag_union.idx);
    try testing.expectEqual(@as(u8, 0), tu_data.discriminant_size);
    try testing.expectEqual(@as(u16, 8), tu_data.discriminant_offset);
    try testing.expectEqual(@as(u32, 1), tu_data.variants.count);
}

test "record extension with empty_record succeeds" {
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);
    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const zst_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &.{.{ .name = try lt.module_env.insertIdent(Ident.for_text("field")), .var_ = zst_var }});

    // Extending empty_record is valid - creates a record with ZST fields
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = zst_var } } });
    const record_idx = try resolveTypeVar(&lt, record_var);
    const record_layout = lt.layout_store.getLayout(record_idx);
    try testing.expect(record_layout.tag == .struct_);
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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

    const result_idx = try resolveTypeVar(&lt, outer_list_var);
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
    const list_idx = try resolveTypeVar(&lt, list_var);
    try testing.expect(lt.layout_store.getLayout(list_idx).tag == .list_of_zst);
}

test "nested ZST detection - singleton record wrapping singleton tag becomes list_of_zst" {
    // Test: List({ one_field : [OneTag({})] }) should still canonicalize to list_of_zst.
    // This is currently the desired long-term behavior even though the current compiler
    // preserves singleton records and singleton tag unions as real containers.
    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });

    const singleton_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("OneTag")),
        .args = try lt.type_store.appendVars(&[_]types.Var{empty_record_var}),
    };
    const tag_range = try lt.type_store.appendTags(&[_]types.Tag{singleton_tag});
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = tag_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    } } });

    const record_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("one_field")), .var_ = tag_union_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = record_fields,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
    } } });

    const list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        record_var,
        &[_]types.Var{record_var},
        builtin_module_idx,
        false,
    );
    const list_var = try lt.type_store.freshFromContent(list_content);

    const list_idx = try resolveTypeVar(&lt, list_var);
    try testing.expectEqual(layout.LayoutTag.list_of_zst, lt.layout_store.getLayout(list_idx).tag);
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create a tuple with two empty record elements: ((), ())
    const empty_record_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const tuple_elems = try lt.type_store.vars.appendSlice(lt.gpa, &[_]types.Var{ empty_record_var, empty_record_var });
    const tuple_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = tuple_elems } } });

    // The tuple should be ZST since both elements are ZST
    const tuple_idx = try resolveTypeVar(&lt, tuple_var);
    const tuple_layout = lt.layout_store.getLayout(tuple_idx);
    try testing.expect(lt.layout_store.layoutSize(tuple_layout) == 0);

    // Box of it should be box_of_zst
    const box_var = try lt.mkBoxType(tuple_var, box_ident_idx, builtin_module_idx);
    const box_idx = try resolveTypeVar(&lt, box_var);
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
    const list_idx = try resolveTypeVar(&lt, list_var);

    // Since the entire nested structure is ZST, the list should be list_of_zst
    try testing.expect(lt.layout_store.getLayout(list_idx).tag == .list_of_zst);
}

test "zst combinatorics matrix for nested singleton ordinary-data wrappers" {
    const cases = [_]ZstMatrixCase{
        .{
            .name = "unit leaf through single-field record and single-tag union stays zero-sized",
            .wrappers = &.{ .record1, .tag1 },
            .leaf = .unit,
            .expect_zero_sized = true,
        },
        .{
            .name = "unit leaf through tuple singleton and multi-payload single-tag stays zero-sized",
            .wrappers = &.{ .tuple1, .tag1_pair_with_zst, .record2_zst },
            .leaf = .unit,
            .expect_zero_sized = true,
        },
        .{
            .name = "unit leaf through deeper nested singleton wrappers stays zero-sized",
            .wrappers = &.{ .record1, .tag1, .tuple2_zst, .record2_zst, .tag1_pair_with_zst },
            .leaf = .unit,
            .expect_zero_sized = true,
        },
        .{
            .name = "u64 leaf through single-field record and single-tag union stays non-zero-sized",
            .wrappers = &.{ .record1, .tag1 },
            .leaf = .u64,
            .expect_zero_sized = false,
        },
        .{
            .name = "u64 leaf through deeper singleton wrappers stays non-zero-sized",
            .wrappers = &.{ .record1, .tag1, .tuple2_zst, .record2_zst, .tag1_pair_with_zst },
            .leaf = .u64,
            .expect_zero_sized = false,
        },
        .{
            .name = "str leaf through mixed singleton wrappers stays non-zero-sized",
            .wrappers = &.{ .tuple1, .record2_zst, .tag1, .tuple2_zst },
            .leaf = .str,
            .expect_zero_sized = false,
        },
    };

    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const list_ident_idx = try lt.module_env.insertIdent(Ident.for_text("List"));
    const box_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    for (cases) |case| {
        // std.debug.print("running zst combinatorics case: {s}\n", .{case.name});

        const type_var = try buildWrappedZstCase(&lt, builtin_module_idx, case.leaf, case.wrappers);
        try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, type_var);
        try expectZstContainerSpecialization(
            &lt,
            builtin_module_idx,
            list_ident_idx,
            box_ident_idx,
            type_var,
            case.expect_zero_sized,
        );
    }
}

test "fromTypeVar - flex var with method constraint returning open tag union" {
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
    const result_idx = try resolveTypeVar(&lt, outer_list_var);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // The list should have a valid layout - either list or list_of_zst
    // The flex var with a constraint should be treated as ZST (since no from_numeral constraint)
    try testing.expect(result_layout.tag == .list or result_layout.tag == .list_of_zst);

    // Also test computing layout of the Try return type directly
    // This is what would happen when evaluating the result of list.first()
    const try_result_idx = try resolveTypeVar(&lt, try_var);
    const try_result_layout = lt.layout_store.getLayout(try_result_idx);
    // Try should be a tag_union
    try testing.expect(try_result_layout.tag == .tag_union);
}

test "fromTypeVar - type alias inside Try nominal (issue #8708)" {
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
        builtin_module_idx,
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
    const result_idx = try resolveTypeVar(&lt, try_var);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // Try should have a tag_union layout
    try testing.expect(result_layout.tag == .tag_union);
}

test "fromTypeVar - recursive nominal type with nested Box at depth 2+ (issue #8816)" {
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
        lt.module_env.qualified_module_ident,
        false,
    );

    // Close the recursive loop by updating the recursive_var to point to the nominal content
    try lt.type_store.setVarContent(recursive_var, rich_doc_content);

    // Also create a fresh var with the content for testing (layout computation will follow the recursion)
    const rich_doc_var = try lt.type_store.freshFromContent(rich_doc_content);

    // This should succeed without segfault.
    // Before the fix, this would fail when computing the layout for depth 2+ nesting.
    const result_idx = try resolveTypeVar(&lt, rich_doc_var);
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

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
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
        lt.module_env.qualified_module_ident,
        false,
    );

    // Close the recursive loop by updating the recursive_var to point to the nominal content
    try lt.type_store.setVarContent(recursive_var, statement_content);

    // Create a fresh var with the content for testing
    const statement_var = try lt.type_store.freshFromContent(statement_content);

    // This should succeed without infinite recursion.
    // Before the fix, layoutSizeAlign would infinitely recurse when computing the size.
    const result_idx = try resolveTypeVar(&lt, statement_var);
    const result_layout = lt.layout_store.getLayout(result_idx);

    // Statement should have a tag_union layout (since the nominal wraps a tag union)
    try testing.expect(result_layout.tag == .tag_union);

    // Verify layoutSizeAlign works without infinite recursion by calling layoutSize
    // (which internally calls layoutSizeAlign)
    const size = lt.layout_store.layoutSize(result_layout);
    // The size should be > 0 (a tag union with payloads has non-zero size)
    try testing.expect(size > 0);
}

test "fromTypeVar - recursive nominal with Box has no double-boxing (issue #8916)" {
    // Regression test for issue #8916:
    // When computing layouts for recursive nominal types like Nat := [Zero, Suc(Box(Nat))],
    // the inner Box's element layout was incorrectly being set to another Box layout
    // instead of the tag_union layout. This caused Box.unbox to return a value with
    // the wrong layout, leading to incorrect pattern matching results.
    //
    // The bug was in the container finalization code: when a tag union backing a
    // recursive nominal finished processing, the code would incorrectly update
    // layout_idx to the boxed layout even for Box/List containers, causing double-boxing.

    var lt: LayoutTest = undefined;
    lt.gpa = testing.allocator;
    lt.module_env = try ModuleEnv.init(lt.gpa, "");
    lt.type_store = try types_store.Store.init(lt.gpa);

    // Setup identifiers
    const nat_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Nat"));
    const box_ident_idx = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;

    lt.module_env_ptr[0] = &lt.module_env;
    lt.layout_store = try Store.init(&lt.module_env_ptr, null, lt.gpa, base.target.TargetUsize.native);
    lt.type_scope = TypeScope.init(lt.gpa);
    defer lt.deinit();

    // Create a recursive type: Nat := [Zero, Suc(Box(Nat))]

    // Create a fresh var for the recursive reference
    const recursive_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });

    // Create Box(recursive_var)
    const box_content = try lt.type_store.mkNominal(
        .{ .ident_idx = box_ident_idx },
        recursive_var,
        &[_]types.Var{recursive_var},
        builtin_module_idx,
        false,
    );
    const box_recursive_var = try lt.type_store.freshFromContent(box_content);

    // Create [Zero, Suc(Box(Nat))]
    const zero_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Zero")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}), // No payload
    };
    const suc_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Suc")),
        .args = try lt.type_store.appendVars(&[_]types.Var{box_recursive_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{ zero_tag, suc_tag });
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    // Create the nominal type content: Nat := [Zero, Suc(Box(Nat))]
    const nat_content = try lt.type_store.mkNominal(
        .{ .ident_idx = nat_ident_idx },
        tag_union_var,
        &[_]types.Var{},
        lt.module_env.qualified_module_ident,
        false,
    );

    // Close the recursive loop
    try lt.type_store.setVarContent(recursive_var, nat_content);

    // Create a var for Nat
    const nat_var = try lt.type_store.freshFromContent(nat_content);

    // Compute the layout
    const nat_layout_idx = try resolveTypeVar(&lt, nat_var);
    const nat_layout = lt.layout_store.getLayout(nat_layout_idx);

    // Nat should have a tag_union layout
    try testing.expect(nat_layout.tag == .tag_union);

    // Get the tag union data to inspect the Suc variant's payload layout
    const tu_data = lt.layout_store.getTagUnionData(nat_layout.data.tag_union.idx);
    const variants = lt.layout_store.getTagUnionVariants(tu_data);

    // Find the Suc variant
    // Variants should be ordered by tag name, so we need to find which one has a payload
    try testing.expect(variants.len == 2);

    // Find which variant has a non-zst payload (that's the Suc variant with Box(Nat))
    var suc_variant_idx: usize = 0;
    for (0..variants.len) |i| {
        const payload_layout = lt.layout_store.getLayout(variants.get(i).payload_layout);
        if (payload_layout.tag != .zst) {
            suc_variant_idx = i;
            break;
        }
    }

    // The Suc variant's payload should be a canonical single-field payload container
    // whose only field is Box(Nat).
    const suc_payload_layout = lt.layout_store.getLayout(variants.get(suc_variant_idx).payload_layout);
    try testing.expect(suc_payload_layout.tag == .struct_);

    const payload_data = lt.layout_store.getStructData(suc_payload_layout.data.struct_.idx);
    const payload_fields = lt.layout_store.struct_fields.sliceRange(payload_data.getFields());
    try testing.expectEqual(@as(usize, 1), payload_fields.len);
    try testing.expectEqual(@as(u16, 0), payload_fields.get(0).index);
    try testing.expect(lt.layout_store.getLayout(payload_fields.get(0).layout).tag == .box);

    // CRITICAL: The element of this Box should be a tag_union, NOT another box.
    // Before the fix, this would be .box (double-boxing bug).
    const box_elem_idx = lt.layout_store.getLayout(payload_fields.get(0).layout).data.box;
    const box_elem_layout = lt.layout_store.getLayout(box_elem_idx);
    try testing.expect(box_elem_layout.tag == .tag_union);
}

// -- Record field offset by canonical/original index --
// These tests verify that record layouts sort by alignment first and
// preserve canonical record-field order as the explicit tie-breaker.

test "putRecord - same alignment preserves canonical field order" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const u64_layout = layout.Layout.int(.u64);
    const record_idx = try lt.layout_store.putRecord(&.{ u64_layout, u64_layout });
    const record_layout = lt.layout_store.getLayout(record_idx);
    const rid = record_layout.data.struct_.idx;

    try testing.expectEqual(@as(u32, 0), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 0));
    try testing.expectEqual(@as(u32, 8), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 1));
}

test "putRecord - alignment overrides canonical order" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const u8_layout = layout.Layout.int(.u8);
    const u64_layout = layout.Layout.int(.u64);
    const record_idx = try lt.layout_store.putRecord(&.{ u8_layout, u64_layout });
    const record_layout = lt.layout_store.getLayout(record_idx);
    const rid = record_layout.data.struct_.idx;

    try testing.expectEqual(@as(u32, 8), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 0));
    try testing.expectEqual(@as(u32, 0), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 1));
}

test "putRecord - equal-alignment ties do not depend on sort stability" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const u64_layout = layout.Layout.int(.u64);
    const record_idx = try lt.layout_store.putRecord(&.{ u64_layout, u64_layout, u64_layout });
    const record_layout = lt.layout_store.getLayout(record_idx);
    const rid = record_layout.data.struct_.idx;

    try testing.expectEqual(@as(u32, 0), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 0));
    try testing.expectEqual(@as(u32, 8), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 1));
    try testing.expectEqual(@as(u32, 16), lt.layout_store.getStructFieldOffsetByOriginalIndex(rid, 2));
}

test "putTuple interns identical tuple shapes to the same layout idx" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const tuple_layout_1 = try lt.layout_store.putTuple(&.{
        layout.Layout.int(.u64),
        layout.Layout.int(.u64),
    });
    const tuple_layout_2 = try lt.layout_store.putTuple(&.{
        layout.Layout.int(.u64),
        layout.Layout.int(.u64),
    });

    try testing.expectEqual(tuple_layout_1, tuple_layout_2);
}

test "putTagUnion interns identical variant payload shapes to the same layout idx" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    const tuple_payload = try lt.layout_store.putTuple(&.{
        layout.Layout.int(.u64),
        layout.Layout.int(.u64),
    });

    const tag_union_1 = try lt.layout_store.putTagUnion(&.{ .zst, tuple_payload });
    const tag_union_2 = try lt.layout_store.putTagUnion(&.{ .zst, tuple_payload });

    try testing.expectEqual(tag_union_1, tag_union_2);
}

test "internGraph interns identical recursive tag unions regardless of construction order" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    var graph_1: layout_graph_.Graph = .{};
    defer graph_1.deinit(testing.allocator);

    const tag_union_1 = try graph_1.reserveNode(testing.allocator);
    const list_1 = try graph_1.reserveNode(testing.allocator);
    graph_1.setNode(list_1, .{ .list = .{ .local = tag_union_1 } });
    const variants_1 = try graph_1.appendRefs(testing.allocator, &[_]layout_graph_.Ref{
        .{ .canonical = layout.Idx.zst },
        .{ .local = list_1 },
    });
    graph_1.setNode(tag_union_1, .{ .tag_union = variants_1 });

    var graph_2: layout_graph_.Graph = .{};
    defer graph_2.deinit(testing.allocator);

    const list_2 = try graph_2.reserveNode(testing.allocator);
    const tag_union_2 = try graph_2.reserveNode(testing.allocator);
    graph_2.setNode(list_2, .{ .list = .{ .local = tag_union_2 } });
    const variants_2 = try graph_2.appendRefs(testing.allocator, &[_]layout_graph_.Ref{
        .{ .canonical = layout.Idx.zst },
        .{ .local = list_2 },
    });
    graph_2.setNode(tag_union_2, .{ .tag_union = variants_2 });

    const idx_1 = try lt.layout_store.internGraph(&graph_1, .{ .local = tag_union_1 });
    const idx_2 = try lt.layout_store.internGraph(&graph_2, .{ .local = tag_union_2 });

    try testing.expectEqual(idx_1, idx_2);
}

test "internGraph interns identical recursive tuple-list graphs regardless of construction order" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    var graph_1: layout_graph_.Graph = .{};
    defer graph_1.deinit(testing.allocator);

    const tuple_1 = try graph_1.reserveNode(testing.allocator);
    const list_1 = try graph_1.reserveNode(testing.allocator);
    graph_1.setNode(list_1, .{ .list = .{ .local = tuple_1 } });
    const fields_1 = try graph_1.appendFields(testing.allocator, &[_]layout_graph_.Field{
        .{ .index = 0, .child = .{ .canonical = layout.Idx.u64 } },
        .{ .index = 1, .child = .{ .local = list_1 } },
    });
    graph_1.setNode(tuple_1, .{ .struct_ = fields_1 });

    var graph_2: layout_graph_.Graph = .{};
    defer graph_2.deinit(testing.allocator);

    const list_2 = try graph_2.reserveNode(testing.allocator);
    const tuple_2 = try graph_2.reserveNode(testing.allocator);
    graph_2.setNode(list_2, .{ .list = .{ .local = tuple_2 } });
    const fields_2 = try graph_2.appendFields(testing.allocator, &[_]layout_graph_.Field{
        .{ .index = 0, .child = .{ .canonical = layout.Idx.u64 } },
        .{ .index = 1, .child = .{ .local = list_2 } },
    });
    graph_2.setNode(tuple_2, .{ .struct_ = fields_2 });

    const idx_1 = try lt.layout_store.internGraph(&graph_1, .{ .local = tuple_1 });
    const idx_2 = try lt.layout_store.internGraph(&graph_2, .{ .local = tuple_2 });

    try testing.expectEqual(idx_1, idx_2);
}

test "internGraph interns identical recursive tag unions with boxes regardless of construction order" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    var graph_1: layout_graph_.Graph = .{};
    defer graph_1.deinit(testing.allocator);

    const tag_union_1 = try graph_1.reserveNode(testing.allocator);
    const box_1 = try graph_1.reserveNode(testing.allocator);
    graph_1.setNode(box_1, .{ .box = .{ .local = tag_union_1 } });
    const variants_1 = try graph_1.appendRefs(testing.allocator, &[_]layout_graph_.Ref{
        .{ .local = box_1 },
        .{ .canonical = layout.Idx.zst },
    });
    graph_1.setNode(tag_union_1, .{ .tag_union = variants_1 });

    var graph_2: layout_graph_.Graph = .{};
    defer graph_2.deinit(testing.allocator);

    const box_2 = try graph_2.reserveNode(testing.allocator);
    const tag_union_2 = try graph_2.reserveNode(testing.allocator);
    graph_2.setNode(box_2, .{ .box = .{ .local = tag_union_2 } });
    const variants_2 = try graph_2.appendRefs(testing.allocator, &[_]layout_graph_.Ref{
        .{ .local = box_2 },
        .{ .canonical = layout.Idx.zst },
    });
    graph_2.setNode(tag_union_2, .{ .tag_union = variants_2 });

    const idx_1 = try lt.layout_store.internGraph(&graph_1, .{ .local = tag_union_1 });
    const idx_2 = try lt.layout_store.internGraph(&graph_2, .{ .local = tag_union_2 });

    try testing.expectEqual(idx_1, idx_2);
}

test "internGraph handles mixed canonical children with local recursive refs" {
    var lt = try LayoutTest.init(testing.allocator);
    try lt.initLayoutStore();
    defer lt.deinit();

    var graph_1: layout_graph_.Graph = .{};
    defer graph_1.deinit(testing.allocator);

    const record_1 = try graph_1.reserveNode(testing.allocator);
    const list_1 = try graph_1.reserveNode(testing.allocator);
    graph_1.setNode(list_1, .{ .list = .{ .local = record_1 } });
    const fields_1 = try graph_1.appendFields(testing.allocator, &[_]layout_graph_.Field{
        .{ .index = 0, .child = .{ .canonical = layout.Idx.str } },
        .{ .index = 1, .child = .{ .local = list_1 } },
    });
    graph_1.setNode(record_1, .{ .struct_ = fields_1 });

    var graph_2: layout_graph_.Graph = .{};
    defer graph_2.deinit(testing.allocator);

    const list_2 = try graph_2.reserveNode(testing.allocator);
    const record_2 = try graph_2.reserveNode(testing.allocator);
    graph_2.setNode(list_2, .{ .list = .{ .local = record_2 } });
    const fields_2 = try graph_2.appendFields(testing.allocator, &[_]layout_graph_.Field{
        .{ .index = 0, .child = .{ .canonical = layout.Idx.str } },
        .{ .index = 1, .child = .{ .local = list_2 } },
    });
    graph_2.setNode(record_2, .{ .struct_ = fields_2 });

    const idx_1 = try lt.layout_store.internGraph(&graph_1, .{ .local = record_1 });
    const idx_2 = try lt.layout_store.internGraph(&graph_2, .{ .local = record_2 });

    try testing.expectEqual(idx_1, idx_2);
}

test "type and monotype layout resolvers agree for nested ordinary data layouts" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const list_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("List"));
    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const u64_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u64_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));
    const u8_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u8_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));
    const str_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.str },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));

    const tuple_vars = try lt.type_store.appendVars(&[_]types.Var{ u8_var, str_var });
    const tuple_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = tuple_vars } } });

    const box_tuple_var = try lt.mkBoxType(tuple_var, box_ident_idx, builtin_module_idx);

    const list_content = try lt.type_store.mkNominal(
        .{ .ident_idx = list_ident_idx },
        u64_var,
        &[_]types.Var{u64_var},
        builtin_module_idx,
        false,
    );
    const list_u64_var = try lt.type_store.freshFromContent(list_content);

    const fields = try lt.type_store.record_fields.appendSlice(testing.allocator, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("a")), .var_ = list_u64_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("b")), .var_ = box_tuple_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .record = .{ .fields = fields, .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }) } },
    });

    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, record_var);
}

test "type and monotype layout resolvers preserve singleton ordinary-data structs" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const u64_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u64_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));

    const record_fields = try lt.type_store.record_fields.appendSlice(testing.allocator, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("only")), .var_ = u64_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .record = .{
            .fields = record_fields,
            .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
        } },
    });

    const tuple_vars = try lt.type_store.appendVars(&[_]types.Var{u64_var});
    const tuple_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .tuple = .{ .elems = tuple_vars } },
    });

    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, record_var);
    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, tuple_var);

    const record_layout_idx = try resolveTypeVar(&lt, record_var);
    const record_layout = lt.layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.tag == .struct_);
    const record_data = lt.layout_store.getStructData(record_layout.data.struct_.idx);
    const record_layout_fields = lt.layout_store.struct_fields.sliceRange(record_data.getFields());
    try testing.expectEqual(@as(usize, 1), record_layout_fields.len);
    try testing.expectEqual(@as(u16, 0), record_layout_fields.get(0).index);
    try testing.expectEqual(layout.Idx.u64, record_layout_fields.get(0).layout);

    const tuple_layout_idx = try resolveTypeVar(&lt, tuple_var);
    const tuple_layout = lt.layout_store.getLayout(tuple_layout_idx);
    try testing.expect(tuple_layout.tag == .struct_);
    const tuple_data = lt.layout_store.getStructData(tuple_layout.data.struct_.idx);
    const tuple_layout_fields = lt.layout_store.struct_fields.sliceRange(tuple_data.getFields());
    try testing.expectEqual(@as(usize, 1), tuple_layout_fields.len);
    try testing.expectEqual(@as(u16, 0), tuple_layout_fields.get(0).index);
    try testing.expectEqual(layout.Idx.u64, tuple_layout_fields.get(0).layout);
}

test "type and monotype layout resolvers preserve singleton tag payload containers" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const u64_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u64_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));

    const only_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Only")),
        .args = try lt.type_store.appendVars(&[_]types.Var{u64_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{only_tag});
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, tag_union_var);

    const union_layout_idx = try resolveTypeVar(&lt, tag_union_var);
    const union_layout = lt.layout_store.getLayout(union_layout_idx);
    try testing.expect(union_layout.tag == .tag_union);

    const tu_data = lt.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
    const variants = lt.layout_store.getTagUnionVariants(tu_data);
    try testing.expectEqual(@as(usize, 1), variants.len);

    const payload_layout_idx = variants.get(0).payload_layout;
    const payload_layout = lt.layout_store.getLayout(payload_layout_idx);
    try testing.expect(payload_layout.tag == .struct_);

    const payload_data = lt.layout_store.getStructData(payload_layout.data.struct_.idx);
    const payload_fields = lt.layout_store.struct_fields.sliceRange(payload_data.getFields());
    try testing.expectEqual(@as(usize, 1), payload_fields.len);
    try testing.expectEqual(@as(u16, 0), payload_fields.get(0).index);
    try testing.expectEqual(layout.Idx.u64, payload_fields.get(0).layout);
}

test "type and monotype layout resolvers agree for recursive nominal layouts" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const box_ident_idx = try lt.module_env.insertIdent(base.Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const nat_ident = try lt.module_env.insertIdent(Ident.for_text("Nat"));
    const recursive_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const box_recursive_var = try lt.mkBoxType(recursive_var, box_ident_idx, builtin_module_idx);

    const zero_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Zero")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const suc_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Suc")),
        .args = try lt.type_store.appendVars(&[_]types.Var{box_recursive_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{ zero_tag, suc_tag });
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    const nat_content = try lt.type_store.mkNominal(
        .{ .ident_idx = nat_ident },
        tag_union_var,
        &[_]types.Var{},
        lt.module_env.qualified_module_ident,
        false,
    );
    try lt.type_store.setVarContent(recursive_var, nat_content);
    const nat_var = try lt.type_store.freshFromContent(nat_content);
    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, nat_var);
}

test "type and monotype layout resolvers agree for directly recursive tag union layouts" {
    var lt = try LayoutTest.initWithIdents(testing.allocator);
    defer lt.deinit();

    const builtin_module_idx = try lt.module_env.insertIdent(base.Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const inner_ident = try lt.module_env.insertIdent(Ident.for_text("Inner"));
    const recursive_var = try lt.type_store.freshFromContent(.{ .flex = types.Flex.init() });
    const unit_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const u64_var = try lt.type_store.freshFromContent(try lt.type_store.mkNominal(
        .{ .ident_idx = lt.module_env.idents.u64_type },
        unit_var,
        &[_]types.Var{},
        builtin_module_idx,
        false,
    ));

    const branch_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Branch")),
        .args = try lt.type_store.appendVars(&[_]types.Var{recursive_var}),
    };
    const leaf_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Leaf")),
        .args = try lt.type_store.appendVars(&[_]types.Var{u64_var}),
    };
    const tags_range = try lt.type_store.appendTags(&[_]types.Tag{ branch_tag, leaf_tag });
    const tag_union = types.TagUnion{
        .tags = tags_range,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = tag_union } });

    const inner_content = try lt.type_store.mkNominal(
        .{ .ident_idx = inner_ident },
        tag_union_var,
        &[_]types.Var{},
        lt.module_env.qualified_module_ident,
        false,
    );
    try lt.type_store.setVarContent(recursive_var, inner_content);
    const inner_var = try lt.type_store.freshFromContent(inner_content);

    try expectTypeAndMonotypeResolversAgree(testing.allocator, &lt, inner_var);

    const inner_layout_idx = try resolveTypeVar(&lt, inner_var);
    const inner_layout = lt.layout_store.getLayout(inner_layout_idx);
    try testing.expect(inner_layout.tag == .tag_union);

    const size = lt.layout_store.layoutSize(inner_layout);
    try testing.expect(size > 0);

    const disc_offset = lt.layout_store.getTagUnionDiscriminantOffset(inner_layout.data.tag_union.idx);
    try testing.expect(disc_offset < size);
    try testing.expect(lt.layout_store.layoutContainsRefcounted(inner_layout));
}

test "fromTypeVar - no-payload nominal tag union gets canonical tag_union layout, not box" {
    var lt = try LayoutTest.init(testing.allocator);
    defer lt.deinit();

    const my_enum_ident_idx = try lt.module_env.insertIdent(Ident.for_text("MyEnum"));
    _ = try lt.module_env.insertIdent(Ident.for_text("Box"));
    const builtin_module_idx = try lt.module_env.insertIdent(Ident.for_text("Builtin"));
    lt.module_env.idents.builtin_module = builtin_module_idx;
    try lt.initLayoutStore();

    const a_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("A")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const b_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("B")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const c_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("C")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const enum_tags = try lt.type_store.appendTags(&[_]types.Tag{ a_tag, b_tag, c_tag });
    const enum_tag_union = types.TagUnion{
        .tags = enum_tags,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const enum_backing_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = enum_tag_union } });

    const my_enum_content = try lt.type_store.mkNominal(
        .{ .ident_idx = my_enum_ident_idx },
        enum_backing_var,
        &[_]types.Var{},
        lt.module_env.qualified_module_ident,
        false,
    );
    const my_enum_var = try lt.type_store.freshFromContent(my_enum_content);

    const index_var = try lt.type_store.freshFromContent(.{ .structure = .empty_record });
    const record_fields = try lt.type_store.record_fields.appendSlice(lt.gpa, &[_]types.RecordField{
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("index")), .var_ = index_var },
        .{ .name = try lt.module_env.insertIdent(Ident.for_text("value")), .var_ = my_enum_var },
    });
    const record_var = try lt.type_store.freshFromContent(.{
        .structure = .{ .record = .{
            .fields = record_fields,
            .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_record }),
        } },
    });

    const foo_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Foo")),
        .args = try lt.type_store.appendVars(&[_]types.Var{record_var}),
    };
    const bar_tag = types.Tag{
        .name = try lt.module_env.insertIdent(Ident.for_text("Bar")),
        .args = try lt.type_store.appendVars(&[_]types.Var{}),
    };
    const outer_tags = try lt.type_store.appendTags(&[_]types.Tag{ foo_tag, bar_tag });
    const outer_tag_union = types.TagUnion{
        .tags = outer_tags,
        .ext = try lt.type_store.freshFromContent(.{ .structure = .empty_tag_union }),
    };
    const outer_tag_union_var = try lt.type_store.freshFromContent(.{ .structure = .{ .tag_union = outer_tag_union } });

    const result_idx = try resolveTypeVar(&lt, outer_tag_union_var);
    const result_layout = lt.layout_store.getLayout(result_idx);
    try testing.expect(result_layout.tag == .tag_union);

    const enum_layout_idx = try resolveTypeVar(&lt, my_enum_var);
    const enum_layout = lt.layout_store.getLayout(enum_layout_idx);
    try testing.expect(enum_layout.tag == .tag_union);
}
