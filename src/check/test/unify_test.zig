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
        try module_env.initCIRFields(gpa, "Test");
        return .{
            .module_env = module_env,
            .snapshots = try snapshot_mod.Store.initCapacity(gpa, 16),
            .problems = try problem_mod.Store.initCapacity(gpa, 16),
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
        return .{ .rigid_var = ident_idx };
    }

    // helpers - alias //

    fn mkAlias(self: *Self, name: []const u8, backing_var: Var, args: []const Var) std.mem.Allocator.Error!Content {
        return try self.module_env.types.mkAlias(try self.mkTypeIdent(name), backing_var, args);
    }

    // helpers - nums //

    fn mkNumPoly(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = var_ } } });
    }

    fn mkNumPolyFlex(self: *Self) std.mem.Allocator.Error!Var {
        const flex_var = try self.module_env.types.freshFromContent(.{ .flex_var = null });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = flex_var } } });
    }

    fn mkNumPolyRigid(self: *Self, name: []const u8) std.mem.Allocator.Error!Var {
        const rigid_var = try self.module_env.types.freshFromContent(try self.mkRigidVar(name));
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = rigid_var } } });
    }

    // helpers - nums - ints //

    fn mkIntConcrete(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        const int_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = var_ } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntPolyFlex(self: *Self) std.mem.Allocator.Error!Var {
        const flex_var = try self.module_env.types.freshFromContent(.{ .flex_var = null });
        const int_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = flex_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntPolyRigid(self: *Self, name: []const u8) std.mem.Allocator.Error!Var {
        const rigid_var = try self.module_env.types.freshFromContent(try self.mkRigidVar(name));
        const int_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = rigid_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntPoly(self: *Self, prec: Num.Int.Precision) std.mem.Allocator.Error!Var {
        const prec_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = prec } } });
        const int_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = prec_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    // helpers - nums - fracs //

    fn mkFracConcrete(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        const int_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = var_ } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkFracPolyFlex(self: *Self) std.mem.Allocator.Error!Var {
        const flex_var = try self.module_env.types.freshFromContent(.{ .flex_var = null });
        const frac_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = flex_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkFracPolyRigid(self: *Self, name: []const u8) std.mem.Allocator.Error!Var {
        const rigid_var = try self.module_env.types.freshFromContent(try self.mkRigidVar(name));
        const frac_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = rigid_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkFracPoly(self: *Self, prec: Num.Frac.Precision) std.mem.Allocator.Error!Var {
        const prec_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_precision = prec } } });
        const frac_var = try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = prec_var } } });
        return try self.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
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
        );
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
        const ext_var = try self.module_env.types.freshFromContent(.{ .flex_var = null });
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
        const ext_var = try self.module_env.types.freshFromContent(.{ .flex_var = null });
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

test "unify - a is flex_var and b is not" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.fresh();
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

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
    const a = try env.module_env.types.freshFromContent(.{ .flex_var = null });
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
    const b = try env.module_env.types.freshFromContent(.{ .flex_var = null });

    const result = try env.unify(a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - cannot unify with alias (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const alias = try env.module_env.types.freshFromContent(Content{ .structure = .str });
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

test "unify - alias with same args" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    // Create alias `a` with its backing var and args in sequence
    const a_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{ str, bool_ }));
    const a_alias = try env.mkAlias("AliasName", a_backing_var, &[_]Var{ str, bool_ });
    const a = try env.module_env.types.freshFromContent(a_alias);

    // Create alias `b` with its backing var and args in sequence
    const b_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{ str, bool_ }));
    const b_alias = try env.mkAlias("AliasName", b_backing_var, &[_]Var{ str, bool_ });
    const b = try env.module_env.types.freshFromContent(b_alias);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

test "unify - alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    // Create alias `a` with its backing var and arg
    const a_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{ str, bool_ }));
    const a_alias = try env.mkAlias("Alias", a_backing_var, &[_]Var{str});
    const a = try env.module_env.types.freshFromContent(a_alias);

    // Create alias `b` with its backing var and arg
    const b_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{ str, bool_ }));
    const b_alias = try env.mkAlias("Alias", b_backing_var, &[_]Var{bool_});
    const b = try env.module_env.types.freshFromContent(b_alias);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - alias with flex" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const a_backing_var = try env.module_env.types.freshFromContent(try env.mkTuple(&[_]Var{ str, bool_ })); // backing var
    const a_alias = try env.mkAlias("Alias", a_backing_var, &[_]Var{bool_});

    const a = try env.module_env.types.freshFromContent(a_alias);
    const b = try env.module_env.types.fresh();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - alias with concrete" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const a_alias = try env.mkAlias("Alias", a_backing_var, &[_]Var{});

    const a = try env.module_env.types.freshFromContent(a_alias);
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .str });

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);

    // Assert that the alias was preserved
    const resolved = env.module_env.types.resolveVar(a);
    try std.testing.expect(resolved.desc.content == .alias);

    // Assert that the alias backing var was preserved
    const resolved_backing = env.module_env.types.resolveVar(
        env.module_env.types.getAliasBackingVar(resolved.desc.content.alias),
    );
    try std.testing.expectEqual(Content{ .structure = .str }, resolved_backing.desc.content);

    // Assert that a & b redirect to the alias
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(b));
}

test "unify - alias with concrete other way" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const b_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const b_alias = try env.mkAlias("Alias", b_backing_var, &[_]Var{});

    const a = try env.module_env.types.freshFromContent(Content{ .structure = .str });
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
    try std.testing.expectEqual(Content{ .structure = .str }, resolved_backing.desc.content);

    // Assert that a & b redirect to the alias
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = resolved.var_ }, env.module_env.types.getSlot(b));
}

// unification - structure/flex_vars //

test "unify - a is builtin and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };

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

    const str = Content{ .structure = .str };

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

    const str = Content{ .structure = .str };

    const a = try env.module_env.types.freshFromContent(str);
    const b = try env.module_env.types.freshFromContent(str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const int = Content{ .structure = .{ .num = Num.int_i8 } };

    const a = try env.module_env.types.freshFromContent(int);
    const b = try env.module_env.types.freshFromContent(str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b box with same arg unify" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const box_str = Content{ .structure = .{ .box = str_var } };

    const a = try env.module_env.types.freshFromContent(box_str);
    const b = try env.module_env.types.freshFromContent(box_str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(box_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b box with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const i64_ = Content{ .structure = .{ .num = Num.int_i64 } };
    const i64_var = try env.module_env.types.freshFromContent(i64_);

    const box_str = Content{ .structure = .{ .box = str_var } };
    const box_i64 = Content{ .structure = .{ .box = i64_var } };

    const a = try env.module_env.types.freshFromContent(box_str);
    const b = try env.module_env.types.freshFromContent(box_i64);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b list with same arg unify" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const list_str = Content{ .structure = .{ .list = str_var } };

    const a = try env.module_env.types.freshFromContent(list_str);
    const b = try env.module_env.types.freshFromContent(list_str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(list_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b list with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const u8_ = Content{ .structure = .{ .num = Num.int_u8 } };
    const u8_var = try env.module_env.types.freshFromContent(u8_);

    const list_str = Content{ .structure = .{ .list = str_var } };
    const list_u8 = Content{ .structure = .{ .list = u8_var } };

    const a = try env.module_env.types.freshFromContent(list_str);
    const b = try env.module_env.types.freshFromContent(list_u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - tuple //

test "unify - a & b are same tuple" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const bool_ = Content{ .structure = .{ .num = Num.int_i8 } };
    const bool_var = try env.module_env.types.freshFromContent(bool_);

    const tuple_str_bool = try env.mkTuple(&[_]Var{ str_var, bool_var });

    const a = try env.module_env.types.freshFromContent(tuple_str_bool);
    const b = try env.module_env.types.freshFromContent(tuple_str_bool);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(tuple_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are tuples with args flipped (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = try env.module_env.types.freshFromContent(str);

    const bool_ = Content{ .structure = .{ .num = Num.int_i8 } };
    const bool_var = try env.module_env.types.freshFromContent(bool_);

    const tuple_str_bool = try env.mkTuple(&[_]Var{ str_var, bool_var });
    const tuple_bool_str = try env.mkTuple(&[_]Var{ bool_var, str_var });

    const a = try env.module_env.types.freshFromContent(tuple_str_bool);
    const b = try env.module_env.types.freshFromContent(tuple_bool_str);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact/compact //

test "unify - two compact ints" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.module_env.types.freshFromContent(int_i32);
    const b = try env.module_env.types.freshFromContent(int_i32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact ints (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact fracs" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.module_env.types.freshFromContent(frac_f32);
    const b = try env.module_env.types.freshFromContent(frac_f32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact fracs (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.frac_f32 } });
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.frac_dec } });

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/poly //

test "unify - two poly ints" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkIntPoly(Num.Int.Precision.u8);
    const b = try env.mkIntPoly(Num.Int.Precision.u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
}

test "unify - two poly ints (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkIntPoly(Num.Int.Precision.u8);
    const b = try env.mkIntPoly(Num.Int.Precision.i128);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - two poly fracs" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkFracPoly(Num.Frac.Precision.f64);
    const b = try env.mkFracPoly(Num.Frac.Precision.f64);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
}

test "unify - two poly fracs (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkFracPoly(Num.Frac.Precision.f32);
    const b = try env.mkFracPoly(Num.Frac.Precision.f64);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact_int //

test "unify - Num(flex) and compact int" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.mkNumPolyFlex();
    const b = try env.module_env.types.freshFromContent(int_i32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(flex)) and compact int" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.mkIntPolyFlex();
    const b = try env.module_env.types.freshFromContent(int_i32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(U8)) and compact int U8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = try env.mkIntPoly(Num.Int.Precision.u8);
    const b = try env.module_env.types.freshFromContent(int_u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_u8, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(U8)) and compact int I32 (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.mkIntPoly(Num.Int.Precision.u8);
    const b = try env.module_env.types.freshFromContent(int_i32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact_frac //

test "unify - Num(flex) and compact frac" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.mkNumPolyFlex();
    const b = try env.module_env.types.freshFromContent(frac_f32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(flex)) and compact frac" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.mkFracPolyFlex();
    const b = try env.module_env.types.freshFromContent(frac_f32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(Dec)) and compact frac Dec" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_dec = Content{ .structure = .{ .num = Num.frac_dec } };
    const a = try env.mkFracPoly(Num.Frac.Precision.dec);
    const b = try env.module_env.types.freshFromContent(frac_dec);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_dec, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(F32)) and compact frac Dec (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.mkFracPoly(Num.Frac.Precision.dec);
    const b = try env.module_env.types.freshFromContent(frac_f32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact_int/poly //

test "unify - compact int and Num(flex)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.module_env.types.freshFromContent(int_i32);
    const b = try env.mkNumPolyFlex();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int and Num(Int(flex))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.module_env.types.freshFromContent(int_i32);
    const b = try env.mkIntPolyFlex();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int and U8 Num(Int(U8))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = try env.module_env.types.freshFromContent(int_u8);
    const b = try env.mkIntPoly(Num.Int.Precision.u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(int_u8, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int U8 and  Num(Int(I32)) (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = try env.module_env.types.freshFromContent(int_i32);
    const b = try env.mkIntPoly(Num.Int.Precision.u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact_frac/poly //

test "unify - compact frac and Num(flex)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.module_env.types.freshFromContent(frac_f32);
    const b = try env.mkNumPolyFlex();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac and Num(Frac(flex))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.module_env.types.freshFromContent(frac_f32);
    const b = try env.mkFracPolyFlex();

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac and Dec Num(Frac(Dec))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_dec = Content{ .structure = .{ .num = Num.frac_dec } };
    const a = try env.module_env.types.freshFromContent(frac_dec);
    const b = try env.mkFracPoly(Num.Frac.Precision.dec);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(frac_dec, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac Dec and Num(Frac(F32)) (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.module_env.types.freshFromContent(frac_f32);
    const b = try env.mkFracPoly(Num.Frac.Precision.dec);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/poly rigid //

test "unify - Num(rigid) and Num(rigid)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid = try env.module_env.types.freshFromContent(try env.mkRigidVar("b"));
    const num = Content{ .structure = .{ .num = .{ .num_poly = rigid } } };
    const a = try env.module_env.types.freshFromContent(num);
    const b = try env.module_env.types.freshFromContent(num);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(num, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(rigid_a) and Num(rigid_b)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkNumPolyRigid("a");
    const b = try env.mkNumPolyRigid("b");

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(rigid)) and Num(Int(rigid))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const num = try env.mkIntPolyRigid("a");
    const a = num;
    const b = num;

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual((try env.getDescForRootVar(num)).content, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual((try env.getDescForRootVar(num)).content, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(rigid_a)) and Num(Int(rigid_b))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkIntPolyRigid("a");
    const b = try env.mkIntPolyRigid("b");

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(rigid)) and Num(Frac(rigid))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const num = try env.mkFracPolyRigid("b");
    const a = num;
    const b = num;

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual((try env.getDescForRootVar(num)).content, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual((try env.getDescForRootVar(num)).content, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(rigid_a)) and Num(Frac(rigid_b))" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.mkFracPolyRigid("a");
    const b = try env.mkFracPolyRigid("b");

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact/poly rigid //

test "unify - compact int U8 and Num(Int(rigid)) (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = try env.module_env.types.freshFromContent(int_u8);
    const b = try env.mkFracPolyRigid("a");

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac Dec and Num(Frac(rigid)) (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.module_env.types.freshFromContent(frac_f32);
    const b = try env.mkFracPolyRigid("a");

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact rigid //

test "unify - Num(Int(rigid)) and compact int U8 (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = try env.mkFracPolyRigid("a");
    const b = try env.module_env.types.freshFromContent(int_u8);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(rigid)) and compact frac Dec (fails)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = try env.mkFracPolyRigid("a");
    const b = try env.module_env.types.freshFromContent(frac_f32);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - func //

test "unify - func are same" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num = try env.mkNumPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const func = try env.mkFuncFlex(&[_]Var{ str, num }, int_i32);

    const a = try env.module_env.types.freshFromContent(func);
    const b = try env.module_env.types.freshFromContent(func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return args (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

    const a = try env.module_env.types.freshFromContent(try env.mkFuncFlex(&[_]Var{int_i32}, str));
    const b = try env.module_env.types.freshFromContent(try env.mkFuncFlex(&[_]Var{str}, str));

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return types (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

    const a = try env.module_env.types.freshFromContent(try env.mkFuncFlex(&[_]Var{str}, int_i32));
    const b = try env.module_env.types.freshFromContent(try env.mkFuncFlex(&[_]Var{str}, str));

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs pure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const func = try env.mkFuncPure(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(func);
    const b = try env.module_env.types.freshFromContent(func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs effectful" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const func = try env.mkFuncEffectful(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(func);
    const b = try env.module_env.types.freshFromContent(func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first eff, second pure (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const pure_func = try env.mkFuncPure(&[_]Var{ str, int_poly }, int_i32);
    const eff_func = try env.mkFuncEffectful(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(eff_func);
    const b = try env.module_env.types.freshFromContent(pure_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first pure, second eff (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const pure_func = try env.mkFuncPure(&[_]Var{ str, int_poly }, int_i32);
    const eff_func = try env.mkFuncEffectful(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(pure_func);
    const b = try env.module_env.types.freshFromContent(eff_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first pure, second unbound" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const pure_func = try env.mkFuncPure(&[_]Var{ str, int_poly }, int_i32);
    const unbound_func = try env.mkFuncUnbound(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(pure_func);
    const b = try env.module_env.types.freshFromContent(unbound_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(pure_func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first unbound, second pure" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const pure_func = try env.mkFuncPure(&[_]Var{ str, int_poly }, int_i32);
    const unbound_func = try env.mkFuncUnbound(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(unbound_func);
    const b = try env.module_env.types.freshFromContent(pure_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(pure_func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first effectful, second unbound" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const eff_func = try env.mkFuncEffectful(&[_]Var{ str, int_poly }, int_i32);
    const unbound_func = try env.mkFuncUnbound(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(eff_func);
    const b = try env.module_env.types.freshFromContent(unbound_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(eff_func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first unbound, second effectful" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const int_poly = try env.mkIntPolyFlex();
    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const eff_func = try env.mkFuncEffectful(&[_]Var{ str, int_poly }, int_i32);
    const unbound_func = try env.mkFuncUnbound(&[_]Var{ str, int_poly }, int_i32);

    const a = try env.module_env.types.freshFromContent(unbound_func);
    const b = try env.module_env.types.freshFromContent(eff_func);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(eff_func, (try env.getDescForRootVar(b)).content);
}

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

test "unify - a & b are both the same nominal type" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const arg = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const a_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const a = try env.module_env.types.freshFromContent(try env.mkNominalType("MyType", a_backing_var, &[_]Var{arg}));

    const b_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const b_nominal = try env.mkNominalType("MyType", b_backing_var, &[_]Var{arg});
    const b = try env.module_env.types.freshFromContent(b_nominal);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(b_nominal, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff nominal types (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const arg = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const a_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const a = try env.module_env.types.freshFromContent(try env.mkNominalType("MyType", a_backing_var, &[_]Var{arg}));

    const b_backing_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const b = try env.module_env.types.freshFromContent(try env.mkNominalType("AnotherType", b_backing_var, &[_]Var{arg}));

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are both the same nominal type with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const arg_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const str_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });

    const a_backing = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const a = try env.module_env.types.freshFromContent(try env.mkNominalType("MyType", a_backing, &[_]Var{arg_var}));

    const b_backing = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const b = try env.module_env.types.freshFromContent(try env.mkNominalType("MyType", b_backing, &[_]Var{str_var}));

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - records - partition fields //

test "partitionFields - same record" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const field_x = try env.mkRecordField("field_x", @enumFromInt(0));
    const field_y = try env.mkRecordField("field_y", @enumFromInt(1));

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

    const a1 = try env.mkRecordField("a1", @enumFromInt(0));
    const a2 = try env.mkRecordField("a2", @enumFromInt(1));
    const b1 = try env.mkRecordField("b1", @enumFromInt(2));

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

    const a1 = try env.mkRecordField("a1", @enumFromInt(0));
    const both = try env.mkRecordField("both", @enumFromInt(1));
    const b1 = try env.mkRecordField("b1", @enumFromInt(2));

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

    const f1 = try env.mkRecordField("f1", @enumFromInt(0));
    const f2 = try env.mkRecordField("f2", @enumFromInt(1));
    const f3 = try env.mkRecordField("f3", @enumFromInt(2));

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

test "unify - open record a extends b" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_shared = try env.mkRecordField("x", str);
    const field_a_only = try env.mkRecordField("y", int);

    const a_rec_data = try env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
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

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(env.scratch.fresh_vars.get(@enumFromInt(0)).*, b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.module_env.types.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.module_env.types.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_a_only.name, b_ext_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_a_only.var_, b_ext_record_fields.items(.var_)[0]);

    const b_ext_ext = env.module_env.types.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_record.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - open record b extends a" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_shared = try env.mkRecordField("field_shared", str);
    const field_b_only = try env.mkRecordField("field_b_only", int);

    const a_rec_data = try env.mkRecordOpen(&[_]RecordField{field_shared});
    const a = try env.module_env.types.freshFromContent(a_rec_data.content);
    const b_rec_data = try env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
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

    const b_ext_record = try TestEnv.getRecordOrErr(env.module_env.types.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.module_env.types.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_b_only.name, b_ext_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_b_only.var_, b_ext_record_fields.items(.var_)[0]);

    const b_ext_ext = env.module_env.types.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_record.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - both extend open record" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const field_shared = try env.mkRecordField("x", str);
    const field_a_only = try env.mkRecordField("y", int);
    const field_b_only = try env.mkRecordField("z", bool_);

    const a_rec_data = try env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
    const a = try env.module_env.types.freshFromContent(a_rec_data.content);
    const b_rec_data = try env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
    const b = try env.module_env.types.freshFromContent(b_rec_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_record.fields.len());
    const b_record_fields = env.module_env.types.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared, b_record_fields.get(0));
    try std.testing.expectEqual(field_a_only, b_record_fields.get(1));
    try std.testing.expectEqual(field_b_only, b_record_fields.get(2));

    const b_ext = env.module_env.types.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(3, env.scratch.fresh_vars.len());

    const only_a_var = env.scratch.fresh_vars.get(@enumFromInt(0)).*;
    const only_a_record = try TestEnv.getRecordOrErr(env.module_env.types.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_record.fields.len());
    const only_a_record_fields = env.module_env.types.getRecordFieldsSlice(only_a_record.fields);
    try std.testing.expectEqual(field_a_only, only_a_record_fields.get(0));

    const only_b_var = env.scratch.fresh_vars.get(@enumFromInt(1)).*;
    const only_b_record = try TestEnv.getRecordOrErr(env.module_env.types.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_record.fields.len());
    const only_b_record_fields = env.module_env.types.getRecordFieldsSlice(only_b_record.fields);
    try std.testing.expectEqual(field_b_only, only_b_record_fields.get(0));

    const ext_var = env.scratch.fresh_vars.get(@enumFromInt(2)).*;
    const ext_content = env.module_env.types.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - record mismatch on shared field (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_a = try env.mkRecordField("x", str);
    const field_b = try env.mkRecordField("x", int);

    const a_rec_data = try env.mkRecordOpen(&[_]RecordField{field_a});
    const a = try env.module_env.types.freshFromContent(a_rec_data.content);

    const b_rec_data = try env.mkRecordOpen(&[_]RecordField{field_b});
    const b = try env.module_env.types.freshFromContent(b_rec_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - structure/structure - records open+closed //

test "unify - open record extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

    const field_x = try env.mkRecordField("field_x", str);
    const field_y = try env.mkRecordField("field_y", str);

    const open = try env.module_env.types.freshFromContent((try env.mkRecordOpen(&[_]RecordField{field_x})).content);
    const closed = try env.module_env.types.freshFromContent((try env.mkRecordClosed(&[_]RecordField{ field_x, field_y })).content);

    const result = try env.unify(open, closed);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.module_env.types.getSlot(open));
}

test "unify - open vs closed records with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_x_str = try env.mkRecordField("field_x_str", str);
    const field_x_int = try env.mkRecordField("field_x_int", int);

    const open = try env.module_env.types.freshFromContent((try env.mkRecordOpen(&[_]RecordField{field_x_str})).content);
    const closed = try env.module_env.types.freshFromContent((try env.mkRecordClosed(&[_]RecordField{field_x_int})).content);

    const result = try env.unify(open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.module_env.types.getSlot(open));

    const desc = try env.getDescForRootVar(closed);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open records with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_x_str = try env.mkRecordField("field_x_str", str);
    const field_x_int = try env.mkRecordField("field_x_int", int);

    const closed = try env.module_env.types.freshFromContent((try env.mkRecordClosed(&[_]RecordField{field_x_int})).content);
    const open = try env.module_env.types.freshFromContent((try env.mkRecordOpen(&[_]RecordField{field_x_str})).content);

    const result = try env.unify(closed, open);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = open }, env.module_env.types.getSlot(closed));

    const desc = try env.getDescForRootVar(open);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - tag unions - partition tags //

test "partitionTags - same tags" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const tag_x = try env.mkTag("X", &[_]Var{@enumFromInt(0)});
    const tag_y = try env.mkTag("Y", &[_]Var{@enumFromInt(1)});

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

    const a1 = try env.mkTag("A1", &[_]Var{@enumFromInt(0)});
    const a2 = try env.mkTag("A2", &[_]Var{@enumFromInt(1)});
    const b1 = try env.mkTag("B1", &[_]Var{@enumFromInt(2)});

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

    const a1 = try env.mkTag("A", &[_]Var{@enumFromInt(0)});
    const both = try env.mkTag("Both", &[_]Var{@enumFromInt(1)});
    const b1 = try env.mkTag("B", &[_]Var{@enumFromInt(2)});

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

    const f1 = try env.mkTag("F1", &[_]Var{@enumFromInt(0)});
    const f2 = try env.mkTag("F2", &[_]Var{@enumFromInt(1)});
    const f3 = try env.mkTag("F3", &[_]Var{@enumFromInt(2)});

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

test "unify - closed tag_unions with diff args (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const a_tag = try env.mkTag("A", &[_]Var{str});
    const a_tags = [_]Tag{a_tag};
    const a_tag_union_data = try env.mkTagUnionClosed(&a_tags);
    const a = try env.module_env.types.freshFromContent(a_tag_union_data.content);

    const b_tag = try env.mkTag("A", &[_]Var{int});
    const b_tags = [_]Tag{b_tag};
    const b_tag_union_data = try env.mkTagUnionClosed(&b_tags);
    const b = try env.module_env.types.freshFromContent(b_tag_union_data.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - structure/structure - tag unions open //

test "unify - identical open tag unions" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

test "unify - open tag union a extends b" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_a_only = try env.mkTag("A", &[_]Var{str});
    const tag_shared = try env.mkTag("Shared", &[_]Var{ int, int });

    const tag_union_a = try env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
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

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.module_env.types.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.module_env.types.tags.sliceRange(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_a_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_a_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.module_env.types.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - open tag union b extends a" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_b_only = try env.mkTag("A", &[_]Var{ str, int });
    const tag_shared = try env.mkTag("Shared", &[_]Var{int});

    const tag_union_a = try env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const a = try env.module_env.types.freshFromContent(tag_union_a.content);

    const tag_union_b = try env.mkTagUnionOpen(&[_]Tag{ tag_b_only, tag_shared });
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

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.module_env.types.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.module_env.types.tags.sliceRange(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_b_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_b_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.module_env.types.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - both extend open tag union" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a_only = try env.mkTag("A", &[_]Var{bool_});
    const tag_b_only = try env.mkTag("B", &[_]Var{ str, int });
    const tag_shared = try env.mkTag("Shared", &[_]Var{int});

    const tag_union_a = try env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
    const a = try env.module_env.types.freshFromContent(tag_union_a.content);

    const tag_union_b = try env.mkTagUnionOpen(&[_]Tag{ tag_b_only, tag_shared });
    const b = try env.module_env.types.freshFromContent(tag_union_b.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_tag_union.tags.len());

    const b_tags = env.module_env.types.tags.sliceRange(b_tag_union.tags);
    try std.testing.expectEqual(3, b_tags.len);
    try std.testing.expectEqual(tag_shared, b_tags.get(0));
    try std.testing.expectEqual(tag_a_only, b_tags.get(1));
    try std.testing.expectEqual(tag_b_only, b_tags.get(2));

    const b_ext = env.module_env.types.resolveVar(b_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(3, env.scratch.fresh_vars.len());

    const only_a_var = env.scratch.fresh_vars.get(@enumFromInt(0)).*;
    const only_a_tag_union = try TestEnv.getTagUnionOrErr(env.module_env.types.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_tag_union.tags.len());
    const only_a_tags = env.module_env.types.getTagsSlice(only_a_tag_union.tags);
    try std.testing.expectEqual(tag_a_only, only_a_tags.get(0));

    const only_b_var = env.scratch.fresh_vars.get(@enumFromInt(1)).*;
    const only_b_tag_union = try TestEnv.getTagUnionOrErr(env.module_env.types.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_tag_union.tags.len());
    const only_b_tags = env.module_env.types.getTagsSlice(only_b_tag_union.tags);
    try std.testing.expectEqual(tag_b_only, only_b_tags.get(0));

    const ext_var = env.scratch.fresh_vars.get(@enumFromInt(2)).*;
    const ext_content = env.module_env.types.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - open tag unions a & b have same tag name with diff args (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_a_only = try env.mkTag("A", &[_]Var{str});
    const tag_shared = try env.mkTag("A", &[_]Var{ int, int });

    const tag_union_a = try env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
    const a = try env.module_env.types.freshFromContent(tag_union_a.content);

    const tag_union_b = try env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const b = try env.module_env.types.freshFromContent(tag_union_b.content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - structure/structure - records open+closed //

test "unify - open tag extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

test "unify - open vs closed tag union with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a = try env.mkTag("A", &[_]Var{str});
    const tag_b = try env.mkTag("A", &[_]Var{bool_});

    const a = try env.module_env.types.freshFromContent((try env.mkTagUnionOpen(&[_]Tag{tag_a})).content);
    const b = try env.module_env.types.freshFromContent((try env.mkTagUnionClosed(&[_]Tag{tag_b})).content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open tag union with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str = try env.module_env.types.freshFromContent(Content{ .structure = .str });
    const bool_ = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a = try env.mkTag("A", &[_]Var{str});
    const tag_b = try env.mkTag("B", &[_]Var{bool_});

    const a = try env.module_env.types.freshFromContent((try env.mkTagUnionClosed(&[_]Tag{tag_a})).content);
    const b = try env.module_env.types.freshFromContent((try env.mkTagUnionOpen(&[_]Tag{tag_b})).content);

    const result = try env.unify(a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.module_env.types.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - recursion //

test "unify - fails on infinite type" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const str_var = try env.module_env.types.freshFromContent(Content{ .structure = .str });

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

    const list_var_a = try env.module_env.types.fresh();
    const list_content_a = Content{
        .structure = .{ .list = list_var_a },
    };
    try env.module_env.types.setRootVarContent(list_var_a, list_content_a);

    const list_var_b = try env.module_env.types.fresh();
    const list_content_b = Content{
        .structure = .{ .list = list_var_b },
    };
    try env.module_env.types.setRootVarContent(list_var_b, list_content_b);

    const result = try env.unify(list_var_a, list_var_b);

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

// number requirements - num //

test "unify - num literal 255 fits in U8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 255 (8 bits unsigned)
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements{
            .sign_needed = false,
            .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
        },
        .frac_requirements = Num.FracRequirements.init(),
    } } } });

    // Create U8 type
    const u8_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    // They should unify successfully
    const result = try env.unify(unbound_var, u8_var);
    try std.testing.expect(result == .ok);
}

test "unify - num literal 256 does not fit fits in U8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 256 (9 bits, no sign)
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements{
            .sign_needed = false,
            .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
        },
        .frac_requirements = Num.FracRequirements.init(),
    } } } });

    // Create U8 type
    const u8_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    // They should NOT unify - type mismatch expected
    const result = try env.unify(unbound_var, u8_var);
    try std.testing.expect(result == .problem);
}

test "integer literal -128 fits in I8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 255 (8 bits unsigned)
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements{
            .sign_needed = true,
            .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
        },
        .frac_requirements = Num.FracRequirements.init(),
    } } } });

    // Create i8 type
    const i8_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    // They should unify successfully
    const result = try env.unify(unbound_var, i8_var);
    try std.testing.expect(result == .ok);
}

test "unify - num literal fits in frac" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 255 (8 bits unsigned)
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements.init(),
        .frac_requirements = Num.FracRequirements{
            .fits_in_f32 = true,
            .fits_in_dec = true,
        },
    } } } });

    // Create Frac type
    const frac_var = try env.mkFracPolyFlex();

    // They should unify successfully
    const result = try env.unify(unbound_var, frac_var);
    try std.testing.expect(result == .ok);
}

test "unify - unbound bigger than dec does not fit in dec" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements.init(),
        .frac_requirements = Num.FracRequirements{
            .fits_in_f32 = true,
            .fits_in_dec = false,
        },
    } } } });

    const dec_var = try env.mkFracPoly(Num.Frac.Precision.dec);

    const result = try env.unify(unbound_var, dec_var);
    try std.testing.expect(result == .problem);
}

test "unify - two unbound nums take the more restrictive reqs" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements{
            .sign_needed = false,
            .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
        },
        .frac_requirements = Num.FracRequirements{
            .fits_in_f32 = true,
            .fits_in_dec = false,
        },
    } } } });

    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_unbound = .{
        .int_requirements = Num.IntRequirements{
            .sign_needed = true,
            .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
        },
        .frac_requirements = Num.FracRequirements{
            .fits_in_f32 = false,
            .fits_in_dec = true,
        },
    } } } });

    const result = try env.unify(a, b);
    try std.testing.expect(result == .ok);

    // Assert that the more restrictive parts (ie the falses) of each unbound
    // appear in the unified type
    const resolved = env.module_env.types.resolveVar(a).desc.content;
    try std.testing.expect(.structure == resolved);
    try std.testing.expect(.num == resolved.structure);
    try std.testing.expect(.num_unbound == resolved.structure.num);

    try std.testing.expectEqual(true, resolved.structure.num.num_unbound.int_requirements.sign_needed);
    try std.testing.expectEqual(@intFromEnum(Num.Int.BitsNeeded.@"9_to_15"), resolved.structure.num.num_unbound.int_requirements.bits_needed);

    try std.testing.expectEqual(false, resolved.structure.num.num_unbound.frac_requirements.fits_in_f32);
    try std.testing.expectEqual(false, resolved.structure.num.num_unbound.frac_requirements.fits_in_dec);
}

// number requirements - int //

test "unify - int literal 255 fits in U8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 255 (8 bits unsigned)
    const int_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_unbound = Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
    } } } });
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });

    // Create U8 type
    const u8_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    // They should unify successfully
    const result = try env.unify(unbound_var, u8_var);
    try std.testing.expect(result == .ok);
}

test "unify - int literal 256 does not fit fits in U8" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a literal with value 256 (9 bits, no sign)
    const int_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_unbound = Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
    } } } });
    const unbound_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });

    // Create U8 type
    const u8_var = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    // They should NOT unify - type mismatch expected
    const result = try env.unify(unbound_var, u8_var);
    try std.testing.expect(result == .problem);
}

test "unify - two unbound ints take the more restrictive reqs" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a_int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_unbound = Num.IntRequirements{
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
    } } } });
    const a = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = a_int } } });

    const b_int = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_unbound = Num.IntRequirements{
        .sign_needed = true,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
    } } } });
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = b_int } } });

    const result = try env.unify(a, b);
    try std.testing.expect(result == .ok);

    // Assert that the more restrictive parts of each unbound appear in the
    // unified type
    const resolved = env.module_env.types.resolveVar(a).desc.content;
    try std.testing.expect(.structure == resolved);
    try std.testing.expect(.num == resolved.structure);
    try std.testing.expect(.num_poly == resolved.structure.num);

    const resolved_int = env.module_env.types.resolveVar(resolved.structure.num.num_poly).desc.content;
    try std.testing.expect(.structure == resolved_int);
    try std.testing.expect(.num == resolved_int.structure);
    try std.testing.expect(.int_unbound == resolved_int.structure.num);
    try std.testing.expectEqual(true, resolved_int.structure.num.int_unbound.sign_needed);
    try std.testing.expectEqual(@intFromEnum(Num.Int.BitsNeeded.@"9_to_15"), resolved_int.structure.num.int_unbound.bits_needed);
}

// number requirements - frac //

test "unify - two unbound fracs take the more restrictive reqs" {
    const gpa = std.testing.allocator;

    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const a_frac = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_unbound = Num.FracRequirements{
        .fits_in_f32 = true,
        .fits_in_dec = false,
    } } } });
    const a = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = a_frac } } });

    const b_frac = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_unbound = Num.FracRequirements{
        .fits_in_f32 = false,
        .fits_in_dec = true,
    } } } });
    const b = try env.module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = b_frac } } });

    const result = try env.unify(a, b);
    try std.testing.expect(result == .ok);

    // Assert that the more restrictive parts of each unbound appear in the
    // unified type
    const resolved = env.module_env.types.resolveVar(a).desc.content;
    try std.testing.expect(.structure == resolved);
    try std.testing.expect(.num == resolved.structure);
    try std.testing.expect(.num_poly == resolved.structure.num);

    const resolved_frac = env.module_env.types.resolveVar(resolved.structure.num.num_poly).desc.content;
    try std.testing.expect(.structure == resolved_frac);
    try std.testing.expect(.num == resolved_frac.structure);
    try std.testing.expect(.frac_unbound == resolved_frac.structure.num);
    try std.testing.expectEqual(false, resolved_frac.structure.num.frac_unbound.fits_in_f32);
    try std.testing.expectEqual(false, resolved_frac.structure.num.frac_unbound.fits_in_dec);
}
