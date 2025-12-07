//! TODO
const std = @import("std");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");

const unify_mod = @import("../unify.zig");
const problem_mod = @import("../problem.zig");
const occurs = @import("../occurs.zig");
const snapshot_mod = @import("../snapshot.zig");

const Region = base.Region;
const Ident = base.Ident;

const ModuleEnv = can.ModuleEnv;

const Scratch = unify_mod.Scratch;
const Result = unify_mod.Result;

const Slot = types_mod.Slot;
const ResolvedVarDesc = types_mod.ResolvedVarDesc;
const ResolvedVarDescs = types_mod.ResolvedVarDescs;

const TypeIdent = types_mod.TypeIdent;
const Var = types_mod.Var;
const Desc = types_mod.Descriptor;
const Rank = types_mod.Rank;
const Mark = types_mod.Mark;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const RecursionVar = types_mod.RecursionVar;
const Content = types_mod.Content;
const Alias = types_mod.Alias;
const NominalType = types_mod.NominalType;
const FlatType = types_mod.FlatType;
const Builtin = types_mod.Builtin;
const Tuple = types_mod.Tuple;
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

const Problem = problem_mod.Problem;

/// A lightweight test harness used in unification and type inference tests.
///
/// `TestEnv` bundles together the following components:
/// * a module env for holding things like idents
/// * a type store for registering and resolving types
/// * a reusable `Scratch` buffer for managing field partitions and temporary variables
///
/// This is intended to simplify unit test setup, particularly for unifying records,
/// functions, aliases, and other structured types.
const TestEnv = struct {
    const Self = @This();

    module_env: *ModuleEnv,
    snapshots: snapshot_mod.Store,
    problems: problem_mod.Store,
    type_writer: types_mod.TypeWriter,
    scratch: Scratch,
    occurs_scratch: occurs.Scratch,

    /// Init everything needed to test unify
    /// This includes allocating module_env on the heap
    ///
    /// TODO: Is heap allocation unideal here? If we want to optimize tests, we
    /// could pull module_env's initialization out of here, but this results in
    /// slight more verbose setup for each test
    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        const module_env = try gpa.create(ModuleEnv);
        module_env.* = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        try module_env.initCIRFields("Test");
        return .{
            .module_env = module_env,
            .snapshots = try snapshot_mod.Store.initCapacity(gpa, 16),
            .problems = try problem_mod.Store.initCapacity(gpa, 16),
            .type_writer = try types_mod.TypeWriter.initFromParts(gpa, &module_env.types, module_env.getIdentStore(), null),
            .scratch = try Scratch.init(module_env.gpa),
            .occurs_scratch = try occurs.Scratch.init(module_env.gpa),
        };
    }

    /// Deinit the test env, including deallocing the module_env from the heap
    fn deinit(self: *Self) void {
        self.module_env.deinit();
        self.module_env.gpa.destroy(self.module_env);
        self.snapshots.deinit();
        self.problems.deinit(self.module_env.gpa);
        self.type_writer.deinit();
        self.scratch.deinit();
        self.occurs_scratch.deinit();
    }

    /// Helper function to call unify with args from TestEnv
    fn unify(self: *Self, a: Var, b: Var) std.mem.Allocator.Error!Result {
        return try unify_mod.unify(
            self.module_env,
            &self.module_env.types,
            &self.problems,
            &self.snapshots,
            &self.type_writer,
            &self.scratch,
            &self.occurs_scratch,
            a,
            b,
        );
    }

    const Error = error{ VarIsNotRoot, IsNotRecord, IsNotTagUnion };

    /// Get a desc from a root var
    fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.module_env.types.getSlot(var_)) {
            .root => |desc_idx| return self.module_env.types.getDesc(desc_idx),
            .redirect => return error.VarIsNotRoot,
        }
    }

    /// Unwrap a record or throw
    fn getRecordOrErr(desc: Desc) error{IsNotRecord}!Record {
        return desc.content.unwrapRecord() orelse error.IsNotRecord;
    }

    /// Unwrap a record or throw
    fn getTagUnionOrErr(desc: Desc) error{IsNotTagUnion}!TagUnion {
        return desc.content.unwrapTagUnion() orelse error.IsNotTagUnion;
    }

    fn mkTypeIdent(self: *Self, name: []const u8) std.mem.Allocator.Error!TypeIdent {
        const ident_idx = try self.module_env.getIdentStore().insert(self.module_env.gpa, Ident.for_text(name));
        return TypeIdent{ .ident_idx = ident_idx };
    }

    // helpers - rigid var //

    fn mkRigidVar(self: *Self, name: []const u8) std.mem.Allocator.Error!Content {
        const ident_idx = try self.module_env.getIdentStore().insert(self.module_env.gpa, Ident.for_text(name));
        return Self.mkRigidVarFromIdent(ident_idx);
    }

    fn mkRigidVarFromIdent(ident_idx: Ident.Idx) Content {
        return .{ .rigid = Rigid.init(ident_idx) };
    }

    // helpers - alias //

    fn mkAlias(self: *Self, name: []const u8, backing_var: Var, args: []const Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkAlias(try self.mkTypeIdent(name), backing_var, args);
    }

    // helpers - structure - tuple //

    fn mkTuple(self: *Self, slice: []const Var) std.mem.Allocator.Error!Content {
        const elems_range = try self.module_env.types.appendVars(slice);
        return Content{ .structure = .{ .tuple = .{ .elems = elems_range } } };
    }

    // helpers - nominal type //

    fn mkNominalType(self: *Self, name: []const u8, backing_var: Var, args: []const Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkNominal(
            try self.mkTypeIdent(name),
            backing_var,
            args,
            Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
            false, // Use nominal for tests
        );
    }

    fn mkList(self: *Self, elem_var: Var) std.mem.Allocator.Error!Content {
        return try self.mkNominalType("List", elem_var, &[_]Var{elem_var});
    }

    fn mkBox(self: *Self, elem_var: Var) std.mem.Allocator.Error!Content {
        return try self.mkNominalType("Box", elem_var, &[_]Var{elem_var});
    }

    // helpers - structure - func //

    fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkFuncPure(args, ret);
    }

    fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkFuncEffectful(args, ret);
    }

    fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkFuncUnbound(args, ret);
    }

    fn mkFuncFlex(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        // For flex functions, we use unbound since we don't know the effectfulness yet
        return try self.module_env.types.mkFuncUnbound(args, ret);
    }

    // helpers - structure - records //

    fn mkRecordField(self: *Self, name: []const u8, var_: Var) std.mem.Allocator.Error!RecordField {
        const ident_idx = try self.module_env.getIdentStore().insert(self.module_env.gpa, Ident.for_text(name));
        return Self.mkRecordFieldFromIdent(ident_idx, var_);
    }

    fn mkRecordFieldFromIdent(ident_idx: Ident.Idx, var_: Var) RecordField {
        return RecordField{ .name = ident_idx, .var_ = var_ };
    }

    const RecordInfo = struct { record: Record, content: Content };

    fn mkRecord(self: *Self, fields: []const RecordField, ext_var: Var) std.mem.Allocator.Error!RecordInfo {
        const fields_range = try self.module_env.types.appendRecordFields(fields);
        const record = Record{ .fields = fields_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .record = record } }, .record = record };
    }

    fn mkRecordOpen(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordInfo {
        const ext_var = try self.module_env.types.freshFromContent(.{ .flex = Flex.init() });
        return self.mkRecord(fields, ext_var);
    }

    fn mkRecordClosed(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordInfo {
        const ext_var = try self.module_env.types.freshFromContent(.{ .structure = .empty_record });
        return self.mkRecord(fields, ext_var);
    }

    // helpers - structure - tag union //

    const TagUnionInfo = struct { tag_union: TagUnion, content: Content };

    fn mkTagArgs(self: *Self, args: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        return try self.module_env.types.appendVars(args);
    }

    fn mkTag(self: *Self, name: []const u8, args: []const Var) std.mem.Allocator.Error!Tag {
        const ident_idx = try self.module_env.getIdentStore().insert(self.module_env.gpa, Ident.for_text(name));
        return Tag{ .name = ident_idx, .args = try self.module_env.types.appendVars(args) };
    }

    fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!TagUnionInfo {
        const tags_range = try self.module_env.types.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .tag_union = tag_union } }, .tag_union = tag_union };
    }

    fn mkTagUnionOpen(self: *Self, tags: []const Tag) std.mem.Allocator.Error!TagUnionInfo {
        const ext_var = try self.module_env.types.freshFromContent(.{ .flex = Flex.init() });
        return self.mkTagUnion(tags, ext_var);
    }

    fn mkTagUnionClosed(self: *Self, tags: []const Tag) std.mem.Allocator.Error!TagUnionInfo {
        const ext_var = try self.module_env.types.freshFromContent(.{ .structure = .empty_tag_union });
        return self.mkTagUnion(tags, ext_var);
    }
};

// unification - flex_vars //

test "unify - identical" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.fresh();
    const desc = try env.getDescForRootVar(a);

    const result = try env.unify(a, a);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(desc, try env.getDescForRootVar(a));
}

test "unify - both flex vars" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.fresh();
    const b = try env.module_env.types.fresh();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
}

// unification - rigid //

test "rigid_var - unifies with flex_var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid = try env.mkRigidVar("a");
    const a = try env.module_env.types.freshFromContent(.{ .flex = Flex.init() });
    const b = try env.module_env.types.freshFromContent(rigid);

    const result = try env.unify(a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - unifies with flex_var (other way)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid = try env.mkRigidVar("a");
    const a = try env.module_env.types.freshFromContent(rigid);
    const b = try env.module_env.types.freshFromContent(.{ .flex = Flex.init() });

    const result = try env.unify(a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - cannot unify with alias (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const alias = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const rigid = try env.module_env.types.freshFromContent(try env.mkRigidVar("a"));

    const result = try env.unify(alias, rigid);
    try std.testing.expectEqual(false, result.isOk());
}

test "rigid_var - cannot unify with identical ident str (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid1 = try env.module_env.types.freshFromContent(try env.mkRigidVar("a"));
    const rigid2 = try env.module_env.types.freshFromContent(try env.mkRigidVar("a"));

    const result = try env.unify(rigid1, rigid2);
    try std.testing.expectEqual(false, result.isOk());
}
// unification - aliases //

test "unify - aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create alias `a` with its backing var and arg
    const a_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{str}));
    const a_alias = try env.mkAlias("AliasA", a_backing_var, &[_]Var{str});
    const a = try env.module_env.types.freshFromContent(a_alias);

    // Create alias `b` with its backing var and arg
    const b_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{str}));
    const b_alias = try env.mkAlias("AliasB", b_backing_var, &[_]Var{str});
    const b = try env.module_env.types.freshFromContent(b_alias);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - alias with concrete" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const a_alias = try env.mkAlias("Alias", a_backing_var, &[_]Var{});

    const a = try env.module_env.types.freshFromContent(a_alias);
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);

    // Assert that the alias was preserved
    const resolved = env.module_env.types.resolveVar(a);
    try std.testing.expect(resolved.desc.content == .alias);

    // Assert that the alias backing var was preserved
    const resolved_backing = env.module_env.types.resolveVar(
        env.module_env.types.getAliasBackingVar(resolved.desc.content.alias),
    );
    try std.testing.expectEqual(Content{ .structure = .empty_record }, resolved_backing.desc.content);

    // Assert that a & b redirect to the alias
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(b));
}

test "unify - alias with concrete other way" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const b_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const b_alias = try env.mkAlias("Alias", b_backing_var, &[_]Var{});

    const a = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const b = try env.module_env.types.freshFromContent(b_alias);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);

    // Assert that the alias was preserved
    const resolved = env.module_env.types.resolveVar(a);
    try std.testing.expect(resolved.desc.content == .alias);

    // Assert that the alias backing var was preserved
    const resolved_backing = env.module_env.types.resolveVar(
        env.module_env.types.getAliasBackingVar(resolved.desc.content.alias),
    );
    try std.testing.expectEqual(Content{ .structure = .empty_record }, resolved_backing.desc.content);

    // Assert that a & b redirect to the alias
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(b));
}

// unification - structure/flex_vars //

test "unify - a is builtin and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .empty_record };

    const a = try env.module_env.types.freshFromContent(str);
    const b = try env.module_env.types.fresh();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a is flex_var and b is builtin" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .empty_record };

    const a = try env.module_env.types.fresh();
    const b = try env.module_env.types.freshFromContent(str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - builtin //

test "unify - a & b are both str" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .empty_record };

    const a = try env.module_env.types.freshFromContent(str);
    const b = try env.module_env.types.freshFromContent(str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b box with same arg unify" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .empty_record };
    const str_var = try env.module_env.types.freshFromContent(str);

    const box_str = try env.mkBox(str_var);

    const a = try env.module_env.types.freshFromContent(box_str);
    const b = try env.module_env.types.freshFromContent(box_str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(box_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b list with same arg unify" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .empty_record };
    const str_var = try env.module_env.types.freshFromContent(str);

    const list_str = try env.mkList(str_var);

    const a = try env.module_env.types.freshFromContent(list_str);
    const b = try env.module_env.types.freshFromContent(list_str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(list_str, (try env.getDescForRootVar(b)).content);
}
// unification - structure/structure - tuple //
// unification - structure/structure - poly/compact_int //
// unification - structure/structure - poly/compact_frac //
// unification - structure/structure - poly/poly rigid //
// unification - structure/structure - poly/compact rigid //
// unification - structure/structure - func //

test "unify - first is flex, second is func" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const tag_payload = try env.module_env.types.fresh();
    const tag = try env.mkTag("Some", &[_]Var{tag_payload});
    const backing_var = try env.module_env.types.freshFromContent((try env.mkTagUnionOpen(&[_]Tag{tag})).content);
    const nominal_type = try env.module_env.types.freshFromContent(try env.mkNominalType("List", backing_var, &[_]Var{}));
    const arg = try env.module_env.types.fresh();
    const func = try env.mkFuncUnbound(&[_]Var{arg}, nominal_type);

    const a = try env.module_env.types.fresh();
    const b = try env.module_env.types.freshFromContent(func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
}
// unification - structure/structure - nominal type //

// unification - nominal types with anonymous tag unions //

test "unify - anonymous tag union unifies with nominal tag union (nominal on left)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Foo := [A(Str), B]
    const str_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const tag_a = try env.mkTag("A", &[_]Var{str_var});
    const tag_b = try env.mkTag("B", &[_]Var{});
    const backing_tu = try env.mkTagUnionClosed(&[_]Tag{ tag_a, tag_b });
    const backing_var = try env.module_env.types.freshFromContent(backing_tu.content);
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Foo", backing_var, &[_]Var{}),
    );

    // Create anonymous tag union: [A(Str)]
    const anon_tag_a = try env.mkTag("A", &[_]Var{str_var});
    const anon_tu = try env.mkTagUnionOpen(&[_]Tag{anon_tag_a});
    const anon_var = try env.module_env.types.freshFromContent(anon_tu.content);

    // Unify: Foo ~ [A(Str)]
    const result = try env.unify(nominal_var, anon_var);

    // Should succeed and merge to nominal type
    try std.testing.expectEqual(.ok, result);
    const resolved = env.module_env.types.resolveVar(anon_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

test "unify - anonymous tag union unifies with nominal (nominal on right)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Foo := [A, B, C]
    const tag_a = try env.mkTag("A", &[_]Var{});
    const tag_b = try env.mkTag("B", &[_]Var{});
    const tag_c = try env.mkTag("C", &[_]Var{});
    const backing_tu = try env.mkTagUnionClosed(&[_]Tag{ tag_a, tag_b, tag_c });
    const backing_var = try env.module_env.types.freshFromContent(backing_tu.content);
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Foo", backing_var, &[_]Var{}),
    );

    // Create anonymous tag union: [B]
    const anon_tag_b = try env.mkTag("B", &[_]Var{});
    const anon_tu = try env.mkTagUnionOpen(&[_]Tag{anon_tag_b});
    const anon_var = try env.module_env.types.freshFromContent(anon_tu.content);

    // Unify: [B] ~ Foo  (swapped order)
    const result = try env.unify(anon_var, nominal_var);

    // Should succeed and merge to nominal type
    try std.testing.expectEqual(.ok, result);
    const resolved = env.module_env.types.resolveVar(anon_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

test "unify - anonymous tag union with wrong tag fails" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Foo := [A, B]
    const tag_a = try env.mkTag("A", &[_]Var{});
    const tag_b = try env.mkTag("B", &[_]Var{});
    const backing_tu = try env.mkTagUnionClosed(&[_]Tag{ tag_a, tag_b });
    const backing_var = try env.module_env.types.freshFromContent(backing_tu.content);
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Foo", backing_var, &[_]Var{}),
    );

    // Create anonymous tag union: [D]  (D doesn't exist in Foo)
    const anon_tag_d = try env.mkTag("D", &[_]Var{});
    const anon_tu = try env.mkTagUnionOpen(&[_]Tag{anon_tag_d});
    const anon_var = try env.module_env.types.freshFromContent(anon_tu.content);

    // Unify: Foo ~ [D]  - should fail
    const result = try env.unify(nominal_var, anon_var);

    // Should fail
    try std.testing.expectEqual(false, result.isOk());
}

test "unify - anonymous tag union with multiple tags unifies" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Foo := [A, B, C]
    const tag_a = try env.mkTag("A", &[_]Var{});
    const tag_b = try env.mkTag("B", &[_]Var{});
    const tag_c = try env.mkTag("C", &[_]Var{});
    const backing_tu = try env.mkTagUnionClosed(&[_]Tag{ tag_a, tag_b, tag_c });
    const backing_var = try env.module_env.types.freshFromContent(backing_tu.content);
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Foo", backing_var, &[_]Var{}),
    );

    // Create anonymous tag union: [A, B]
    const anon_tag_a = try env.mkTag("A", &[_]Var{});
    const anon_tag_b = try env.mkTag("B", &[_]Var{});
    const anon_tu = try env.mkTagUnionOpen(&[_]Tag{ anon_tag_a, anon_tag_b });
    const anon_var = try env.module_env.types.freshFromContent(anon_tu.content);

    // Unify: Foo ~ [A, B]
    const result = try env.unify(nominal_var, anon_var);

    // Should succeed
    try std.testing.expectEqual(.ok, result);
    const resolved = env.module_env.types.resolveVar(anon_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

// unification - empty nominal types //

test "unify - empty nominal type with empty tag union (nominal on left)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Empty := []
    const backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Empty", backing_var, &[_]Var{}),
    );

    // Create empty tag union: []
    const empty_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });

    // Unify: Empty ~ []
    const result = try env.unify(nominal_var, empty_var);

    // Should succeed and merge to nominal type
    try std.testing.expectEqual(.ok, result);
    const resolved = env.module_env.types.resolveVar(empty_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

test "unify - empty tag union with empty nominal type (nominal on right)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create empty tag union: []
    const empty_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });

    // Create nominal type: Empty := []
    const backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Empty", backing_var, &[_]Var{}),
    );

    // Unify: [] ~ Empty
    const result = try env.unify(empty_var, nominal_var);

    // Should succeed and merge to nominal type
    try std.testing.expectEqual(.ok, result);
    const resolved = env.module_env.types.resolveVar(empty_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

test "unify - two empty nominal types" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Empty1 := []
    const backing_var1 = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });
    const nominal_var1 = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Empty1", backing_var1, &[_]Var{}),
    );

    // Create nominal type: Empty2 := []
    const backing_var2 = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });
    const nominal_var2 = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Empty2", backing_var2, &[_]Var{}),
    );

    // Unify: Empty1 ~ Empty2 - should fail (different nominal types)
    const result = try env.unify(nominal_var1, nominal_var2);

    // Should fail because they're different nominal types
    try std.testing.expectEqual(false, result.isOk());
}

test "unify - empty nominal type with non-empty tag union fails" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nominal type: Empty := []
    const backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_tag_union });
    const nominal_var = try env.module_env.types.freshFromContent(
        try env.mkNominalType("Empty", backing_var, &[_]Var{}),
    );

    // Create non-empty tag union: [A]
    const anon_tag_a = try env.mkTag("A", &[_]Var{});
    const anon_tu = try env.mkTagUnionOpen(&[_]Tag{anon_tag_a});
    const anon_var = try env.module_env.types.freshFromContent(anon_tu.content);

    // Unify: Empty ~ [A] - should fail
    const result = try env.unify(nominal_var, anon_var);

    // Should fail
    try std.testing.expectEqual(false, result.isOk());
}

// unification - records - partition fields //

test "partitionFields - same record" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_x = try env.module_env.types.fresh();
    const var_y = try env.module_env.types.fresh();
    const field_x = try env.mkRecordField("field_x", var_x);
    const field_y = try env.mkRecordField("field_y", var_y);

    const range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ field_x, field_y });

    const result = try unify_mod.partitionFields(env.module_env.getIdentStore(), &env.scratch, range, range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(2, result.in_both.len());

    const both_slice = env.scratch.in_both_fields.sliceRange(result.in_both);
    try std.testing.expectEqual(field_x, both_slice[0].a);
    try std.testing.expectEqual(field_x, both_slice[0].b);
    try std.testing.expectEqual(field_y, both_slice[1].a);
    try std.testing.expectEqual(field_y, both_slice[1].b);
}

test "partitionFields - disjoint fields" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_a1 = try env.module_env.types.fresh();
    const var_a2 = try env.module_env.types.fresh();
    const var_b1 = try env.module_env.types.fresh();
    const a1 = try env.mkRecordField("a1", var_a1);
    const a2 = try env.mkRecordField("a2", var_a2);
    const b1 = try env.mkRecordField("b1", var_b1);

    const a_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, a2 });
    const b_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{b1});

    const result = try unify_mod.partitionFields(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(2, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(0, result.in_both.len());

    const only_in_a_slice = env.scratch.only_in_a_fields.sliceRange(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);
    try std.testing.expectEqual(a2, only_in_a_slice[1]);

    const only_in_b_slice = env.scratch.only_in_b_fields.sliceRange(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionFields - overlapping fields" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_a1 = try env.module_env.types.fresh();
    const var_both = try env.module_env.types.fresh();
    const var_b1 = try env.module_env.types.fresh();
    const a1 = try env.mkRecordField("a1", var_a1);
    const both = try env.mkRecordField("both", var_both);
    const b1 = try env.mkRecordField("b1", var_b1);

    const a_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, both });
    const b_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ b1, both });

    const result = try unify_mod.partitionFields(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(1, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(1, result.in_both.len());

    const both_slice = env.scratch.in_both_fields.sliceRange(result.in_both);
    try std.testing.expectEqual(both, both_slice[0].a);
    try std.testing.expectEqual(both, both_slice[0].b);

    const only_in_a_slice = env.scratch.only_in_a_fields.sliceRange(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);

    const only_in_b_slice = env.scratch.only_in_b_fields.sliceRange(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionFields - reordering is normalized" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_f1 = try env.module_env.types.fresh();
    const var_f2 = try env.module_env.types.fresh();
    const var_f3 = try env.module_env.types.fresh();
    const f1 = try env.mkRecordField("f1", var_f1);
    const f2 = try env.mkRecordField("f2", var_f2);
    const f3 = try env.mkRecordField("f3", var_f3);

    const a_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ f3, f1, f2 });
    const b_range = try env.scratch.appendSliceGatheredFields(&[_]RecordField{ f1, f2, f3 });

    const result = try unify_mod.partitionFields(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(3, result.in_both.len());

    const both = env.scratch.in_both_fields.sliceRange(result.in_both);
    try std.testing.expectEqual(f1, both[0].a);
    try std.testing.expectEqual(f1, both[0].b);
    try std.testing.expectEqual(f2, both[1].a);
    try std.testing.expectEqual(f2, both[1].b);
    try std.testing.expectEqual(f3, both[2].a);
    try std.testing.expectEqual(f3, both[2].b);
}

// unification - structure/structure - records closed //

test "unify - identical closed records" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const fields = [_]RecordField{try env.mkRecordField("a", str)};
    const record_data = try env.mkRecordClosed(&fields);
    const record_data_fields = env.module_env.types.record_fields.sliceRange(record_data.record.fields);

    const a = try env.module_env.types.freshFromContent(record_data.content);
    const b = try env.module_env.types.freshFromContent(record_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    const b_record_fields = env.module_env.types.record_fields.sliceRange(b_record.fields);
    try std.testing.expectEqualSlices(Ident.Idx, record_data_fields.items(.name), b_record_fields.items(.name));
    try std.testing.expectEqualSlices(Var, record_data_fields.items(.var_), b_record_fields.items(.var_));
}

test "unify - closed record mismatch on diff fields (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const field1 = try env.mkRecordField("field1", str);
    const field2 = try env.mkRecordField("field2", str);

    const a_record_data = try env.mkRecordClosed(&[_]RecordField{ field1, field2 });
    const a = try env.module_env.types.freshFromContent(a_record_data.content);

    const b_record_data = try env.mkRecordClosed(&[_]RecordField{field1});
    const b = try env.module_env.types.freshFromContent(b_record_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - structure/structure - records open //

test "unify - identical open records" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const field_shared = try env.mkRecordField("x", str);

    const a_rec_data = try env.mkRecordOpen(&[_]RecordField{field_shared});
    const a = try env.module_env.types.freshFromContent(a_rec_data.content);
    const b_rec_data = try env.mkRecordOpen(&[_]RecordField{field_shared});
    const b = try env.module_env.types.freshFromContent(b_rec_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.module_env.types.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared.name, b_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_shared.var_, b_record_fields.items(.var_)[0]);

    const b_ext = env.module_env.types.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

// unification - structure/structure - records open+closed //

test "unify - open record extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const field_x = try env.mkRecordField("field_x", str);
    const field_y = try env.mkRecordField("field_y", str);

    const open = try env.module_env.types.freshFromContent((try env.mkRecordOpen(&[_]RecordField{ field_x, field_y })).content);
    const closed = try env.module_env.types.freshFromContent((try env.mkRecordClosed(&[_]RecordField{field_x})).content);

    const result = try env.unify(open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.module_env.types.getSlot(open));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(closed)).content);
}

test "unify - closed record extends open" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const field_x = try env.mkRecordField("field_x", str);
    const field_y = try env.mkRecordField("field_y", str);

    const open = try env.module_env.types.freshFromContent((try env.mkRecordOpen(&[_]RecordField{field_x})).content);
    const closed = try env.module_env.types.freshFromContent((try env.mkRecordClosed(&[_]RecordField{ field_x, field_y })).content);

    const result = try env.unify(open, closed);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.module_env.types.getSlot(open));
}

// unification - tag unions - partition tags //

test "partitionTags - same tags" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_x = try env.module_env.types.fresh();
    const var_y = try env.module_env.types.fresh();
    const tag_x = try env.mkTag("X", &[_]Var{var_x});
    const tag_y = try env.mkTag("Y", &[_]Var{var_y});

    const range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ tag_x, tag_y });

    const result = try unify_mod.partitionTags(env.module_env.getIdentStore(), &env.scratch, range, range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(2, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.sliceRange(result.in_both);
    try std.testing.expectEqual(tag_x, both_slice[0].a);
    try std.testing.expectEqual(tag_x, both_slice[0].b);
    try std.testing.expectEqual(tag_y, both_slice[1].a);
    try std.testing.expectEqual(tag_y, both_slice[1].b);
}

test "partitionTags - disjoint fields" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_a1 = try env.module_env.types.fresh();
    const var_a2 = try env.module_env.types.fresh();
    const var_b1 = try env.module_env.types.fresh();
    const a1 = try env.mkTag("A1", &[_]Var{var_a1});
    const a2 = try env.mkTag("A2", &[_]Var{var_a2});
    const b1 = try env.mkTag("B1", &[_]Var{var_b1});

    const a_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ a1, a2 });
    const b_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{b1});

    const result = try unify_mod.partitionTags(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(2, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(0, result.in_both.len());

    const only_in_a_slice = env.scratch.only_in_a_tags.sliceRange(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);
    try std.testing.expectEqual(a2, only_in_a_slice[1]);

    const only_in_b_slice = env.scratch.only_in_b_tags.sliceRange(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionTags - overlapping tags" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_a = try env.module_env.types.fresh();
    const var_both = try env.module_env.types.fresh();
    const var_b = try env.module_env.types.fresh();
    const a1 = try env.mkTag("A", &[_]Var{var_a});
    const both = try env.mkTag("Both", &[_]Var{var_both});
    const b1 = try env.mkTag("B", &[_]Var{var_b});

    const a_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ a1, both });
    const b_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ b1, both });

    const result = try unify_mod.partitionTags(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(1, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(1, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.sliceRange(result.in_both);
    try std.testing.expectEqual(both, both_slice[0].a);
    try std.testing.expectEqual(both, both_slice[0].b);

    const only_in_a_slice = env.scratch.only_in_a_tags.sliceRange(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);

    const only_in_b_slice = env.scratch.only_in_b_tags.sliceRange(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionTags - reordering is normalized" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const var_f1 = try env.module_env.types.fresh();
    const var_f2 = try env.module_env.types.fresh();
    const var_f3 = try env.module_env.types.fresh();
    const f1 = try env.mkTag("F1", &[_]Var{var_f1});
    const f2 = try env.mkTag("F2", &[_]Var{var_f2});
    const f3 = try env.mkTag("F3", &[_]Var{var_f3});

    const a_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ f3, f1, f2 });
    const b_range = try env.scratch.appendSliceGatheredTags(&[_]Tag{ f1, f2, f3 });

    const result = try unify_mod.partitionTags(env.module_env.getIdentStore(), &env.scratch, a_range, b_range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(3, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.sliceRange(result.in_both);
    try std.testing.expectEqual(f1, both_slice[0].a);
    try std.testing.expectEqual(f1, both_slice[0].b);
    try std.testing.expectEqual(f2, both_slice[1].a);
    try std.testing.expectEqual(f2, both_slice[1].b);
    try std.testing.expectEqual(f3, both_slice[2].a);
    try std.testing.expectEqual(f3, both_slice[2].b);
}

// unification - structure/structure - tag unions closed //

test "unify - identical closed tag_unions" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const tag = try env.mkTag("A", &[_]Var{str});
    const tags = [_]Tag{tag};
    const tag_union_data = try env.mkTagUnionClosed(&tags);

    const a = try env.module_env.types.freshFromContent(tag_union_data.content);
    const b = try env.module_env.types.freshFromContent(tag_union_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    const b_tags = env.module_env.types.tags.sliceRange(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag.name, b_tags_names[0]);
    try std.testing.expectEqual(tag.args, b_tags_args[0]);

    try std.testing.expectEqual(1, b_tags.len);

    const b_tag_args = env.module_env.types.vars.sliceRange(b_tags_args[0]);
    try std.testing.expectEqual(1, b_tag_args.len);
    try std.testing.expectEqual(str, b_tag_args[0]);
}

// unification - structure/structure - tag unions open //

test "unify - identical open tag unions" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const tag_shared = try env.mkTag("Shared", &[_]Var{ str, str });

    const tag_union_a = try env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const a = try env.module_env.types.freshFromContent(tag_union_a.content);

    const tag_union_b = try env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const b = try env.module_env.types.freshFromContent(tag_union_b.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.module_env.types.tags.sliceRange(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext = env.module_env.types.resolveVar(b_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

// unification - structure/structure - records open+closed //

test "unify - open tag extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const tag_shared = try env.mkTag("Shared", &[_]Var{str});
    const tag_a_only = try env.mkTag("A", &[_]Var{str});

    const a = try env.module_env.types.freshFromContent((try env.mkTagUnionOpen(&[_]Tag{ tag_shared, tag_a_only })).content);
    const b = try env.module_env.types.freshFromContent((try env.mkTagUnionClosed(&[_]Tag{tag_shared})).content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(b)).content);
}

test "unify - closed tag union extends open" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const tag_shared = try env.mkTag("Shared", &[_]Var{str});
    const tag_b_only = try env.mkTag("B", &[_]Var{str});

    const a = try env.module_env.types.freshFromContent((try env.mkTagUnionOpen(&[_]Tag{tag_shared})).content);
    const b = try env.module_env.types.freshFromContent((try env.mkTagUnionClosed(&[_]Tag{ tag_shared, tag_b_only })).content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.module_env.types.tags.sliceRange(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.module_env.types.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.module_env.types.tags.sliceRange(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_b_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_b_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.module_env.types.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .structure = .empty_tag_union }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

// unification - recursion //

test "unify - fails on infinite type" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const a = try env.module_env.types.fresh();
    const a_elems_range = try env.module_env.types.appendVars(&[_]Var{ a, str_var });
    const a_tuple = types_mod.Tuple{ .elems = a_elems_range };
    try env.module_env.types.setRootVarContent(a, Content{ .structure = .{ .tuple = a_tuple } });

    const b = try env.module_env.types.fresh();
    const b_elems_range = try env.module_env.types.appendVars(&[_]Var{ b, str_var });
    const b_tuple = types_mod.Tuple{ .elems = b_elems_range };
    try env.module_env.types.setRootVarContent(b, Content{ .structure = .{ .tuple = b_tuple } });

    const result = try env.unify(a, b);

    switch (result) {
        .ok => try std.testing.expect(false),
        .problem => |problem_idx| {
            const problem = env.problems.get(problem_idx);
            try std.testing.expectEqual(.infinite_recursion, @as(Problem.Tag, problem));
        },
    }
}

test "unify - fails on anonymous recursion" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a tag union that recursively contains itself (anonymous recursion)
    // This is like: a = [A a] unifying with b = [A b]
    const tag_var_a = try env.module_env.types.fresh();
    const tag_a = try env.mkTag("A", &[_]Var{tag_var_a});
    const tag_union_a = try env.mkTagUnionClosed(&[_]Tag{tag_a});
    try env.module_env.types.setRootVarContent(tag_var_a, tag_union_a.content);

    const tag_var_b = try env.module_env.types.fresh();
    const tag_b = try env.mkTag("A", &[_]Var{tag_var_b});
    const tag_union_b = try env.mkTagUnionClosed(&[_]Tag{tag_b});
    try env.module_env.types.setRootVarContent(tag_var_b, tag_union_b.content);

    const result = try env.unify(tag_var_a, tag_var_b);

    switch (result) {
        .ok => try std.testing.expect(false),
        .problem => |problem_idx| {
            const problem = env.problems.get(problem_idx);
            try std.testing.expectEqual(.anonymous_recursion, @as(Problem.Tag, problem));
        },
    }
}

test "unify - succeeds on nominal, tag union recursion" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    var types_store = &env.module_env.types;

    // Create vars in the required order for adjacency to work out
    const a = try types_store.fresh();
    const b = try types_store.fresh();
    const elem = try types_store.fresh();
    const ext = try types_store.fresh();

    // Create the tag union content that references type_a_nominal
    const a_cons_tag = try env.mkTag("Cons", &[_]Var{ elem, a });
    const a_nil_tag = try env.mkTag("Nil", &[_]Var{});
    const a_backing = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ a_cons_tag, a_nil_tag }, ext));
    try types_store.setVarContent(a, try env.mkNominalType("TypeA", a_backing, &.{}));

    const b_cons_tag = try env.mkTag("Cons", &[_]Var{ elem, b });
    const b_nil_tag = try env.mkTag("Nil", &[_]Var{});
    const b_backing = try types_store.freshFromContent(try types_store.mkTagUnion(&.{ b_cons_tag, b_nil_tag }, ext));
    try types_store.setVarContent(b, try env.mkNominalType("TypeA", b_backing, &.{}));

    const result_nominal_type = try env.unify(a, b);
    try std.testing.expectEqual(.ok, result_nominal_type);

    const result_tag_union = try env.unify(a_backing, b_backing);
    try std.testing.expectEqual(.ok, result_tag_union);
}

// static dispatch constraints //

test "unify - flex with no constraints unifies with flex with constraints" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create constraint: a.sort : List(a) -> List(a)
    const list_a = try env.module_env.types.fresh();
    const sort_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{list_a}, list_a));
    const sort_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("sort")),
        .fn_var = sort_fn,
        .origin = .where_clause,
    };

    const constraints_range = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{sort_constraint});

    const a = try env.module_env.types.freshFromContent(.{ .flex = Flex.init() });
    const b = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints_range,
    } });

    const result = try env.unify(a, b);
    try std.testing.expectEqual(.ok, result);

    const resolved = env.module_env.types.resolveVar(a);
    try std.testing.expect(resolved.desc.content == .flex);
    try std.testing.expectEqual(constraints_range, resolved.desc.content.flex.constraints);
}

test "unify - flex with constraints unifies with flex with same constraints" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create constraint: a.to_str : Str -> Str
    const to_str_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{str}, str));
    const sort_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("to_str")),
        .fn_var = to_str_fn,
        .origin = .where_clause,
    };

    const a_constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{sort_constraint});
    const b_constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{sort_constraint});

    const a = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = a_constraints,
    } });
    const b = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = b_constraints,
    } });

    const result = try env.unify(a, b);
    try std.testing.expectEqual(.ok, result);
}

test "unify - empty constraints unify with any" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const foo_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{str}, str));
    const foo_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("foo")),
        .fn_var = foo_fn,
        .origin = .where_clause,
    };
    const constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{foo_constraint});

    const empty_range = types_mod.StaticDispatchConstraint.SafeList.Range.empty();

    const a = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = empty_range,
    } });
    const b = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints,
    } });

    const result = try env.unify(a, b);
    try std.testing.expectEqual(.ok, result);
}

// capture constraints

test "unify - flex with constraints vs structure captures deferred check" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create constraint: a.to_str : Str -> Str
    const to_str_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{str}, str));
    const to_str_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("to_str")),
        .fn_var = to_str_fn,
        .origin = .where_clause,
    };
    const constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{to_str_constraint});

    const flex_var = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints,
    } });
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const result = try env.unify(flex_var, structure_var);
    try std.testing.expectEqual(.ok, result);

    // Check that constraint was captured
    try std.testing.expectEqual(1, env.scratch.deferred_constraints.len());
    const deferred = env.scratch.deferred_constraints.items.items[0];
    try std.testing.expectEqual(
        env.module_env.types.resolveVar(structure_var).var_,
        env.module_env.types.resolveVar(deferred.var_).var_,
    );
    try std.testing.expectEqual(constraints, deferred.constraints);
}

test "unify - structure vs flex with constraints captures deferred check (reversed)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create constraint: a.to_str : Str -> Str
    const to_str_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{str}, str));
    const to_str_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("to_str")),
        .fn_var = to_str_fn,
        .origin = .where_clause,
    };
    const constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{to_str_constraint});

    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const flex_var = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints,
    } });

    const result = try env.unify(structure_var, flex_var);
    try std.testing.expectEqual(.ok, result);

    // Check that constraint was captured (note: vars might be swapped due to merge order)
    try std.testing.expectEqual(1, env.scratch.deferred_constraints.len());
    const deferred = env.scratch.deferred_constraints.items.items[0];
    try std.testing.expectEqual(
        env.module_env.types.resolveVar(flex_var).var_,
        env.module_env.types.resolveVar(deferred.var_).var_,
    );
    try std.testing.expectEqual(constraints, deferred.constraints);
}

test "unify - flex with no constraints vs structure does not capture" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const flex_var = try env.module_env.types.freshFromContent(.{ .flex = Flex.init() });
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const result = try env.unify(flex_var, structure_var);
    try std.testing.expectEqual(.ok, result);

    // Check that NO constraint was captured
    try std.testing.expectEqual(0, env.scratch.deferred_constraints.len());
}

test "unify - flex vs nominal type captures constraint" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create constraint
    const ord_fn = try env.module_env.types.freshFromContent(try env.mkFuncPure(&[_]Var{str}, str));
    const ord_constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("ord")),
        .fn_var = ord_fn,
        .origin = .where_clause,
    };
    const constraints = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{ord_constraint});

    const flex_var = try env.module_env.types.freshFromContent(.{ .flex = .{
        .name = null,
        .constraints = constraints,
    } });

    // Create nominal type (e.g., Path)
    const backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const nominal_var = try env.module_env.types.freshFromContent(try env.mkNominalType("Path", backing_var, &[_]Var{}));

    const result = try env.unify(flex_var, nominal_var);
    try std.testing.expectEqual(.ok, result);

    // Check that constraint was captured
    try std.testing.expectEqual(1, env.scratch.deferred_constraints.len());
    const deferred = env.scratch.deferred_constraints.items.items[0];
    try std.testing.expectEqual(
        env.module_env.types.resolveVar(nominal_var).var_,
        env.module_env.types.resolveVar(deferred.var_).var_,
    );
    try std.testing.expectEqual(constraints, deferred.constraints);
}

// RecursionVar tests

test "recursion_var - can be created and points to structure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a structure variable (e.g., a Str type)
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create a RecursionVar pointing to the structure
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // Verify the recursion var was created correctly
    const resolved = env.module_env.types.resolveVar(rec_var);
    try std.testing.expect(resolved.desc.content == .recursion_var);

    const rec_var_content = resolved.desc.content.recursion_var;
    try std.testing.expectEqual(structure_var, rec_var_content.structure);
}

test "recursion_var - unifies with its structure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a structure variable
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create a RecursionVar pointing to the structure
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // Unify the recursion var with its structure - this should succeed
    // because RecursionVar represents equirecursive types
    const result = try env.unify(rec_var, structure_var);
    try std.testing.expectEqual(.ok, result);
}

test "recursion_var - does not cause infinite loop during resolution" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a circular structure: rec_var -> structure_var -> rec_var
    const structure_var = try env.module_env.types.fresh();

    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // Make structure_var point back to rec_var to create a cycle
    // This tests that resolveVar and other operations handle cycles properly
    _ = try env.unify(structure_var, rec_var);

    // If we get here without hanging, the test passed
    // Just verify the vars are connected
    const resolved_rec = env.module_env.types.resolveVar(rec_var);
    const resolved_structure = env.module_env.types.resolveVar(structure_var);

    // Both should resolve to the same root var
    try std.testing.expectEqual(resolved_rec.var_, resolved_structure.var_);
}

// Equirecursive unification tests

test "recursion_var - unifies with alias" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create an alias MyStr = Str
    const str_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const alias_content = try env.mkAlias("MyStr", str_var, &[_]Var{});
    const alias_var = try env.module_env.types.freshFromContent(alias_content);

    // Create a RecursionVar pointing to str
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = str_var,
            .name = null,
        },
    });

    // RecursionVar should unify with alias by going through to the backing var
    const result = try env.unify(rec_var, alias_var);
    try std.testing.expectEqual(.ok, result);
}

test "recursion_var - cannot unify with rigid" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a rigid var
    const rigid_content = try env.mkRigidVar("a");
    const rigid_var = try env.module_env.types.freshFromContent(rigid_content);

    // Create a RecursionVar
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // RecursionVar cannot unify with rigid
    const result = try env.unify(rec_var, rigid_var);
    try std.testing.expectEqual(false, result.isOk());
}

test "recursion_var - two recursion vars with same structure unify" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a shared structure
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create two RecursionVars both pointing to the same structure
    const rec_var_1 = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    const rec_var_2 = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // They should unify successfully
    const result = try env.unify(rec_var_1, rec_var_2);
    try std.testing.expectEqual(.ok, result);
}

test "recursion_var - equirecursive unification with nested structure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a list structure: List(a) where a is flexible
    const elem_var = try env.module_env.types.fresh();
    const list_content = try env.mkList(elem_var);
    const list_var = try env.module_env.types.freshFromContent(list_content);

    // Create a RecursionVar that points to the list
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = list_var,
            .name = null,
        },
    });

    // Create another list with the same structure
    const elem_var_2 = try env.module_env.types.fresh();
    const list_content_2 = try env.mkList(elem_var_2);
    const list_var_2 = try env.module_env.types.freshFromContent(list_content_2);

    // RecursionVar should unify with the list
    const result = try env.unify(rec_var, list_var_2);
    try std.testing.expectEqual(.ok, result);

    // The element vars should also have been unified
    const resolved_elem_1 = env.module_env.types.resolveVar(elem_var);
    const resolved_elem_2 = env.module_env.types.resolveVar(elem_var_2);
    try std.testing.expectEqual(resolved_elem_1.var_, resolved_elem_2.var_);
}

test "recursion_var - unifies with flex preserving constraints" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a flex var with static dispatch constraints
    const fn_var = try env.module_env.types.freshFromContent(Content{
        .structure = .{ .fn_pure = .{
            .args = VarSafeList.Range.empty(),
            .ret = try env.module_env.types.fresh(),
            .needs_instantiation = false,
        } },
    });

    const constraint = types_mod.StaticDispatchConstraint{
        .fn_name = try env.module_env.getIdentStore().insert(env.module_env.gpa, Ident.for_text("to_str")),
        .fn_var = fn_var,
        .origin = .method_call,
    };

    const constraints_range = try env.module_env.types.appendStaticDispatchConstraints(&[_]types_mod.StaticDispatchConstraint{constraint});
    const flex_with_constraints = try env.module_env.types.freshFromContent(Content{
        .flex = Flex.init().withConstraints(constraints_range),
    });

    // Create a RecursionVar
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // Unify - should defer the constraints
    const result = try env.unify(rec_var, flex_with_constraints);
    try std.testing.expectEqual(.ok, result);

    // Should have deferred the constraints
    try std.testing.expectEqual(@as(usize, 1), env.scratch.deferred_constraints.len());
}

// TypeWriter tests for RecursionVar

test "type_writer - recursion_var displays structure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a structure variable (empty record)
    const structure_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    // Create a RecursionVar pointing to it
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = structure_var,
            .name = null,
        },
    });

    // Write the recursion var to a string
    const TypeWriter = types_mod.TypeWriter;
    var writer = try TypeWriter.initFromParts(gpa, &env.module_env.types, env.module_env.getIdentStore(), null);
    defer writer.deinit();

    const result = try writer.writeGet(rec_var);

    // Should display as "{}" (the structure it points to)
    try std.testing.expectEqualStrings("{}", result);
}

test "type_writer - recursion_var with cycle displays correctly" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a self-referential structure: rec_var -> list -> rec_var
    const rec_var_placeholder = try env.module_env.types.fresh();

    // Create a list that points to the placeholder
    const list_content = try env.mkList(rec_var_placeholder);
    const list_var = try env.module_env.types.freshFromContent(list_content);

    // Create a RecursionVar that points to the list
    const rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = list_var,
            .name = null,
        },
    });

    // Unify the placeholder with the rec_var to create the cycle
    _ = try env.unify(rec_var_placeholder, rec_var);

    // Write the recursion var
    const TypeWriter = types_mod.TypeWriter;
    var writer = try TypeWriter.initFromParts(gpa, &env.module_env.types, env.module_env.getIdentStore(), null);
    defer writer.deinit();

    const result = try writer.writeGet(rec_var);

    // Should display as "List(...)" - the cycle is detected and shown as "..."
    try std.testing.expectEqualStrings("List(...)", result);
}

test "type_writer - nested recursion_var displays correctly" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create nested structure: RecursionVar -> List -> RecursionVar -> empty_record
    const empty_record_var = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const inner_rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = empty_record_var,
            .name = null,
        },
    });

    const list_content = try env.mkList(inner_rec_var);
    const list_var = try env.module_env.types.freshFromContent(list_content);

    const outer_rec_var = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = list_var,
            .name = null,
        },
    });

    // Write the outer recursion var
    const TypeWriter = types_mod.TypeWriter;
    var writer = try TypeWriter.initFromParts(gpa, &env.module_env.types, env.module_env.getIdentStore(), null);
    defer writer.deinit();

    const result = try writer.writeGet(outer_rec_var);

    // Should display as "List({})" - following through the RecursionVars
    try std.testing.expectEqualStrings("List({})", result);
}

// Integration test for recursive constraints (motivating example)

test "recursion_var - integration: deep recursion with RecursionVar prevents infinite loops" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Simulate the motivating example problem: recursive constraint chains
    // Without RecursionVar, this would infinite loop during unification
    // With RecursionVar, it terminates successfully

    // Create a deep chain of RecursionVars pointing to each other
    // var1 -> rec_var1 -> var2 -> rec_var2 -> var3 -> rec_var3 -> var1 (cycle!)

    const var1 = try env.module_env.types.fresh();
    const var2 = try env.module_env.types.fresh();
    const var3 = try env.module_env.types.fresh();

    // Create rec_var3 pointing to var1 (will create the cycle)
    const rec_var3 = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = var1,
            .name = null,
        },
    });

    // Create rec_var2 pointing to var3
    const rec_var2 = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = var3,
            .name = null,
        },
    });

    // Create rec_var1 pointing to var2
    const rec_var1 = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = var2,
            .name = null,
        },
    });

    // Now unify to create the chain
    _ = try env.unify(var1, rec_var1); // var1 -> rec_var1 -> var2
    _ = try env.unify(var2, rec_var2); // var2 -> rec_var2 -> var3
    const result = try env.unify(var3, rec_var3); // var3 -> rec_var3 -> var1 (CYCLE!)

    // **KEY TEST**: This should succeed without infinite loop!
    // Before RecursionVar, this would hang indefinitely
    try std.testing.expectEqual(.ok, result);

    // Verify the unification created connections between the vars
    // The exact vars may differ due to redirects, but they should all be connected
    const resolved_var1 = env.module_env.types.resolveVar(var1);
    const resolved_var2 = env.module_env.types.resolveVar(var2);
    const resolved_var3 = env.module_env.types.resolveVar(var3);

    // At least one should be a RecursionVar, indicating the cycle was detected
    const has_recursion_var = resolved_var1.desc.content == .recursion_var or
        resolved_var2.desc.content == .recursion_var or
        resolved_var3.desc.content == .recursion_var;
    try std.testing.expect(has_recursion_var);

    // The type should display with cycle detection
    const TypeWriter = types_mod.TypeWriter;
    var writer = try TypeWriter.initFromParts(gpa, &env.module_env.types, env.module_env.getIdentStore(), null);
    defer writer.deinit();

    const display = try writer.writeGet(var1);

    // Should display "..." indicating cycle detection
    try std.testing.expect(std.mem.indexOf(u8, display, "...") != null);
}

test "recursion_var - integration: multiple recursive constraints unify correctly" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Test that two different recursive constraint chains can unify
    // Chain A: a1 -> rec_var_a -> a2 -> a1 (cycle)
    // Chain B: b1 -> rec_var_b -> b2 -> b1 (cycle)
    // They should unify if their base structures match

    // Create chain A
    const a1 = try env.module_env.types.fresh();
    const a2 = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const rec_var_a = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = a2,
            .name = null,
        },
    });

    _ = try env.unify(a1, rec_var_a);

    // Create chain B with same base structure
    const b1 = try env.module_env.types.fresh();
    const b2 = try env.module_env.types.freshFromContent(Content{ .structure = .empty_record });

    const rec_var_b = try env.module_env.types.freshFromContent(Content{
        .recursion_var = .{
            .structure = b2,
            .name = null,
        },
    });

    _ = try env.unify(b1, rec_var_b);

    // Unify chain A with chain B - should succeed because both point to Str
    const result = try env.unify(a1, b1);
    try std.testing.expectEqual(.ok, result);

    // Verify they unified - both should ultimately point to the same content
    // Note: They may not be the exact same var due to redirects, but should be equivalent
    const resolved_a1 = env.module_env.types.resolveVar(a1);
    const resolved_b1 = env.module_env.types.resolveVar(b1);

    // Check that both resolve to the same structure content
    try std.testing.expect(resolved_a1.desc.content == .recursion_var or resolved_a1.desc.content == .structure);
    try std.testing.expect(resolved_b1.desc.content == .recursion_var or resolved_b1.desc.content == .structure);

    // The key test: no infinite loop occurred during unification
    // If we got here, RecursionVar successfully prevented the infinite loop
}
