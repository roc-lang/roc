//! Tests for rigid variable instantiation in the type system.
//!
//! This module contains tests that verify the correct behavior of rigid type
//! variables during instantiation, particularly for polymorphic functions
//! where type variables need to be properly instantiated with concrete types.

const std = @import("std");
const base = @import("base");

const types_mod = @import("../types.zig");
const Store = @import("../store.zig").Store;
const Instantiator = @import("../instantiate.zig").Instantiator;

const Ident = base.Ident;

const TypeIdent = types_mod.TypeIdent;
const Var = types_mod.Var;
const Desc = types_mod.Descriptor;
const Rank = types_mod.Rank;
const Mark = types_mod.Mark;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const Alias = types_mod.Alias;
const NominalType = types_mod.NominalType;
const FlatType = types_mod.FlatType;
const Builtin = types_mod.Builtin;
const Tuple = types_mod.Tuple;
const Num = types_mod.Num;
const NumCompact = types_mod.Num.Compact;
const Func = types_mod.Func;
const Record = types_mod.Record;
const RecordField = types_mod.RecordField;
const TwoRecordFields = types_mod.TwoRecordFields;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;
const TwoTags = types_mod.TwoTags;

const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const RecordFieldSafeList = RecordField.SafeList;
const TwoRecordFieldsSafeMultiList = TwoRecordFields.SafeMultiList;
const TwoRecordFieldsSafeList = TwoRecordFields.SafeList;
const TagSafeList = Tag.SafeList;
const TagSafeMultiList = Tag.SafeMultiList;
const TwoTagsSafeList = TwoTags.SafeList;

test "instantiate - flex var creates new flex var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContent(.{ .flex = Flex.init() });

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should still be flex
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .flex);
}

test "instantiate - rigid var with fresh_flex creates flex var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContent(try env.mkRigidVar("a"));

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should now be flex
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .flex);
}

test "instantiate - rigid var with fresh_rigid creates new rigid var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContent(try env.mkRigidVar("a"));

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_rigid,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should still be rigid
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .rigid);
}

test "instantiate - rigid var with substitute_rigids substitutes correctly" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const ident_idx = try env.idents.insert(gpa, Ident.for_text("a"));
    const original = try env.types.freshFromContent(.{ .rigid = Rigid.init(ident_idx) });

    const substitute_var = try env.types.freshFromContent(.{ .structure = .{ .num = Num.int_u8 } });

    var rigid_subs = std.AutoHashMapUnmanaged(Ident.Idx, Var){};
    defer rigid_subs.deinit(gpa);
    try rigid_subs.put(gpa, ident_idx, substitute_var);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .{ .substitute_rigids = &rigid_subs },
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be substituted with our provided var
    try std.testing.expectEqual(substitute_var, instantiated);
}

test "instantiate - preserves rigid var structure in function" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a -> a function
    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));
    const func_content = try env.mkFuncPure(&[_]Var{rigid_a}, rigid_a);
    const original = try env.types.freshFromContent(func_content);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different function
    try std.testing.expect(instantiated != original);

    // Get the function structure
    const resolved = env.types.resolveVar(instantiated);
    const func = resolved.desc.content.structure.fn_pure;

    const args = env.types.sliceVars(func.args);
    try std.testing.expectEqual(1, args.len);

    // The arg and return should be the SAME new flex var
    try std.testing.expectEqual(args[0], func.ret);
}

test "instantiate - tuple with multiple vars" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));
    const rigid_b = try env.types.freshFromContent(try env.mkRigidVar("b"));
    const tuple_content = try env.mkTuple(&[_]Var{ rigid_a, rigid_b, rigid_a });
    const original = try env.types.freshFromContent(tuple_content);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const tuple = resolved.desc.content.structure.tuple;
    const elems = env.types.sliceVars(tuple.elems);

    try std.testing.expectEqual(3, elems.len);
    // First and third should be the same (both were rigid_a)
    try std.testing.expectEqual(elems[0], elems[2]);
    // Second should be different (was rigid_b)
    try std.testing.expect(elems[0] != elems[1]);
}

test "instantiate - record with multiple fields" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));
    const rigid_b = try env.types.freshFromContent(try env.mkRigidVar("b"));

    const record_info = try env.mkRecordClosed(&[_]RecordField{
        try env.mkRecordField("x", rigid_a),
        try env.mkRecordField("y", rigid_b),
        try env.mkRecordField("z", rigid_a),
    });
    const original = try env.types.freshFromContent(record_info.content);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const record = resolved.desc.content.structure.record;
    const fields = env.types.getRecordFieldsSlice(record.fields);

    try std.testing.expectEqual(3, fields.len);

    const x_var = fields.items(.var_)[0];
    const y_var = fields.items(.var_)[1];
    const z_var = fields.items(.var_)[2];

    // x and z should be the same (both were rigid_a)
    try std.testing.expectEqual(x_var, z_var);
    // y should be different (was rigid_b)
    try std.testing.expect(x_var != y_var);
}

test "instantiate - tag union preserves structure" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));

    const tag_union_info = try env.mkTagUnionClosed(&[_]Tag{
        try env.mkTag("Some", &[_]Var{rigid_a}),
        try env.mkTag("None", &[_]Var{}),
    });
    const original = try env.types.freshFromContent(tag_union_info.content);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const tag_union = resolved.desc.content.structure.tag_union;
    const tags = env.types.getTagsSlice(tag_union.tags);

    try std.testing.expectEqual(2, tags.len);

    // Check Some tag has one arg that's different from original
    const some_args = env.types.sliceVars(tags.items(.args)[0]);
    try std.testing.expectEqual(1, some_args.len);
    try std.testing.expect(some_args[0] != rigid_a);

    // Check None tag has no args
    const none_args = env.types.sliceVars(tags.items(.args)[1]);
    try std.testing.expectEqual(0, none_args.len);
}

test "instantiate - alias preserves structure" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));
    const backing = try env.types.freshFromContent(.{ .structure = .{ .list = rigid_a } });
    const alias_content = try env.mkAlias("MyList", backing, &[_]Var{rigid_a});
    const original = try env.types.freshFromContent(alias_content);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const alias = resolved.desc.content.alias;

    // Get the args
    const args = env.types.sliceAliasArgs(alias);
    try std.testing.expectEqual(1, args.len);

    // Get the backing var
    const backing_var = env.types.getAliasBackingVar(alias);
    const backing_resolved = env.types.resolveVar(backing_var);
    const backing_list_elem = backing_resolved.desc.content.structure.list;

    // The alias arg and the list element should be the same fresh var
    try std.testing.expectEqual(args[0], backing_list_elem);
}

test "instantiate - num types" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));

    // Test num_poly
    {
        const num_poly = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = rigid_a } } });

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
        };

        const instantiated = try instantiator.instantiateVar(num_poly);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure.num == .num_poly);
        try std.testing.expect(resolved.desc.content.structure.num.num_poly != rigid_a);
    }

    // Test int_poly
    {
        const int_poly = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .int_poly = rigid_a } } });

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
        };

        const instantiated = try instantiator.instantiateVar(int_poly);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure.num == .int_poly);
        try std.testing.expect(resolved.desc.content.structure.num.int_poly != rigid_a);
    }

    // Test that concrete types remain unchanged
    {
        const u8_var = try env.types.freshFromContent(.{ .structure = .{ .num = Num.int_u8 } });

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
        };

        const instantiated = try instantiator.instantiateVar(u8_var);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure.num == .num_compact);
        try std.testing.expectEqual(Num.Int.Precision.u8, resolved.desc.content.structure.num.num_compact.int);
    }
}

test "instantiate - box and list" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));

    // Test Box a
    {
        const box_var = try env.types.freshFromContent(.{ .structure = .{ .box = rigid_a } });

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
        };

        const instantiated = try instantiator.instantiateVar(box_var);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure == .box);
        try std.testing.expect(resolved.desc.content.structure.box != rigid_a);
    }

    // Test List a
    {
        const list_var = try env.types.freshFromContent(.{ .structure = .{ .list = rigid_a } });

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
        };

        const instantiated = try instantiator.instantiateVar(list_var);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure == .list);
        try std.testing.expect(resolved.desc.content.structure.list != rigid_a);
    }
}

test "instantiate - multiple instantiations are independent" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContent(try env.mkRigidVar("a"));
    const func_content = try env.mkFuncPure(&[_]Var{rigid_a}, rigid_a);
    const original = try env.types.freshFromContent(func_content);

    // First instantiation
    var var_map1 = std.AutoHashMap(Var, Var).init(gpa);
    defer var_map1.deinit();

    var instantiator1 = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &var_map1,
        .rigid_behavior = .fresh_flex,
    };

    const inst1 = try instantiator1.instantiateVar(original);

    // Second instantiation
    var var_map2 = std.AutoHashMap(Var, Var).init(gpa);
    defer var_map2.deinit();

    var instantiator2 = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &var_map2,
        .rigid_behavior = .fresh_flex,
    };

    const inst2 = try instantiator2.instantiateVar(original);

    // The two instantiations should be completely independent
    try std.testing.expect(inst1 != inst2);

    const resolved1 = env.types.resolveVar(inst1);
    const func1 = resolved1.desc.content.structure.fn_pure;
    const args1 = env.types.sliceVars(func1.args);

    const resolved2 = env.types.resolveVar(inst2);
    const func2 = resolved2.desc.content.structure.fn_pure;
    const args2 = env.types.sliceVars(func2.args);

    // Even the inner vars should be different
    try std.testing.expect(args1[0] != args2[0]);
    try std.testing.expect(func1.ret != func2.ret);
}

/// Env to make test setup/teardown easier
const TestEnv = struct {
    const Self = @This();

    gpa: std.mem.Allocator,
    types: Store,
    idents: Ident.Store,
    var_map: std.AutoHashMap(Var, Var),

    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .types = try Store.initCapacity(gpa, 16, 8),
            .idents = try Ident.Store.initCapacity(gpa, 16),
            .var_map = std.AutoHashMap(Var, Var).init(gpa),
        };
    }

    /// Deinit the test env, including deallocing the module_env from the heap
    fn deinit(self: *Self) void {
        self.types.deinit();
        self.idents.deinit(self.gpa);
        self.var_map.deinit();
    }

    fn mkTypeIdent(self: *Self, name: []const u8) std.mem.Allocator.Error!TypeIdent {
        const ident_idx = try self.idents.insert(self.gpa, Ident.for_text(name));
        return TypeIdent{ .ident_idx = ident_idx };
    }

    // helpers - alias //

    fn mkAlias(self: *Self, name: []const u8, backing_var: Var, args: []const Var) std.mem.Allocator.Error!Content {
        return try self.types.mkAlias(try self.mkTypeIdent(name), backing_var, args);
    }

    // helpers - rigid var //

    fn mkRigidVar(self: *Self, name: []const u8) std.mem.Allocator.Error!Content {
        const ident_idx = try self.idents.insert(self.gpa, Ident.for_text(name));
        return Self.mkRigidVarFromIdent(ident_idx);
    }

    fn mkRigidVarFromIdent(ident_idx: Ident.Idx) Content {
        return .{ .rigid = Rigid.init(ident_idx) };
    }

    // helpers - tuple //

    fn mkTuple(self: *Self, slice: []const Var) std.mem.Allocator.Error!Content {
        const elems_range = try self.types.appendVars(slice);
        return Content{ .structure = .{ .tuple = .{ .elems = elems_range } } };
    }

    // helpers - records //

    fn mkRecordField(self: *Self, name: []const u8, var_: Var) std.mem.Allocator.Error!RecordField {
        const ident_idx = try self.idents.insert(self.gpa, Ident.for_text(name));
        return Self.mkRecordFieldFromIdent(ident_idx, var_);
    }

    fn mkRecordFieldFromIdent(ident_idx: Ident.Idx, var_: Var) RecordField {
        return RecordField{ .name = ident_idx, .var_ = var_ };
    }

    const RecordInfo = struct { record: Record, content: Content };

    fn mkRecord(self: *Self, fields: []const RecordField, ext_var: Var) std.mem.Allocator.Error!RecordInfo {
        const fields_range = try self.types.appendRecordFields(fields);
        const record = Record{ .fields = fields_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .record = record } }, .record = record };
    }

    fn mkRecordOpen(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordInfo {
        const ext_var = try self.types.freshFromContent(.{ .flex = Flex.init() });
        return self.mkRecord(fields, ext_var);
    }

    fn mkRecordClosed(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordInfo {
        const ext_var = try self.types.freshFromContent(.{ .structure = .empty_record });
        return self.mkRecord(fields, ext_var);
    }

    // helpers - func //

    fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return try self.types.mkFuncPure(args, ret);
    }

    // helpers - tag union //

    const TagUnionInfo = struct { tag_union: TagUnion, content: Content };

    fn mkTagArgs(self: *Self, args: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        return try self.types.appendVars(args);
    }

    fn mkTag(self: *Self, name: []const u8, args: []const Var) std.mem.Allocator.Error!Tag {
        const ident_idx = try self.idents.insert(self.gpa, Ident.for_text(name));
        return Tag{ .name = ident_idx, .args = try self.types.appendVars(args) };
    }

    fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!TagUnionInfo {
        const tags_range = try self.types.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .tag_union = tag_union } }, .tag_union = tag_union };
    }

    fn mkTagUnionOpen(self: *Self, tags: []const Tag) std.mem.Allocator.Error!TagUnionInfo {
        const ext_var = try self.types.freshFromContent(.{ .flex = Flex.init() });
        return self.mkTagUnion(tags, ext_var);
    }

    fn mkTagUnionClosed(self: *Self, tags: []const Tag) std.mem.Allocator.Error!TagUnionInfo {
        const ext_var = try self.types.freshFromContent(.{ .structure = .empty_tag_union });
        return self.mkTagUnion(tags, ext_var);
    }
};
