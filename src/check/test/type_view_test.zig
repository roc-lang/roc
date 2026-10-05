//! Invariants for read-only nominal substitution, including recursive scopes.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const Reader = @import("../type_view.zig");
const exhaustive = @import("../exhaustive.zig");
const TestEnv = @import("TestEnv.zig");
const Var = types.Var;
const testing = std.testing;

const Fixture = struct {
    store: types.Store,
    idents: base.Ident.Store,

    fn init() !Fixture {
        var store = try types.Store.initCapacity(testing.allocator, 32, 0);
        errdefer store.deinit();
        return .{ .store = store, .idents = try base.Ident.Store.initCapacity(testing.allocator, 8) };
    }
    fn deinit(self: *Fixture) void {
        self.store.deinit();
        self.idents.deinit(testing.allocator);
    }
    fn ident(self: *Fixture, text: []const u8) !base.Ident.Idx {
        return self.idents.insert(testing.allocator, try base.Ident.from_bytes(text));
    }
    fn rigid(self: *Fixture, text: []const u8) !Var {
        return self.store.freshFromContent(.{ .rigid = types.Rigid.init(try self.ident(text)) });
    }
    fn tuple(self: *Fixture, vars: []const Var) !Var {
        return self.store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try self.store.appendVars(vars) } } });
    }
    fn declaration(self: *Fixture, text: []const u8, formals: []const Var, backing: Var) !types.NominalType {
        const name: types.TypeIdent = .{ .ident_idx = try self.ident(text) };
        const source = try types.NominalType.Source.initChecked(
            try types.SourceDecl.fromStatementChecked(@intCast(self.store.nominalDeclCount())),
            false,
            false,
        );
        const args = try self.store.appendVars(formals);
        _ = try self.store.registerNominalDecl(.{
            .ident = name,
            .origin_module = @enumFromInt(0),
            .source = source,
            .formals = args,
            .backing = backing,
            .flags = .{ .valid = true },
        });
        return .{ .ident = name, .origin_module = @enumFromInt(0), .source = source, .args = args };
    }
    fn application(self: *Fixture, nominal: types.NominalType, args: []const Var) !types.NominalType {
        var result = nominal;
        result.args = try self.store.appendVars(args);
        return result;
    }
};

fn tupleChildren(reader: *Reader, root: Var) ![]const Var {
    const resolved = try reader.resolveVar(root);
    return reader.sliceVars(resolved.desc.content.structure.tuple.elems);
}

fn builtinIdents(f: *Fixture, cache: *exhaustive.NominalOpenCache) !exhaustive.BuiltinIdents {
    const sentinel = try f.ident("NotNumeric");
    var result: exhaustive.BuiltinIdents = undefined;
    inline for (std.meta.fields(exhaustive.BuiltinIdents)) |field| {
        if (field.type == base.Ident.Idx) @field(result, field.name) = sentinel;
    }
    result.idents = &f.idents;
    result.open_cache = cache;
    return result;
}

test "nominal views exhaustive boundary returns actual blockers only and permits later closure" {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const b = try f.rigid("b");
    const actual_a = try f.rigid("a");
    const actual_b = try f.store.fresh();
    const private = try f.store.fresh();
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const ok = try f.ident("Ok");
    const err = try f.ident("Err");
    const ghost = try f.ident("Ghost");
    const tags = try f.store.appendTags(&.{
        .{ .name = ok, .args = try f.store.appendVars(&.{a}) },
        .{ .name = err, .args = try f.store.appendVars(&.{b}) },
        .{ .name = ghost, .args = try f.store.appendVars(&.{private}) },
    });
    const backing = try f.store.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = tags, .ext = empty } } });
    const nominal = try f.declaration("TryLike", &.{ a, b }, backing);
    const app = try f.application(nominal, &.{ actual_a, actual_b });
    const root = try f.store.freshFromContent(.{ .structure = .{ .nominal_type = app } });
    var cache = exhaustive.NominalOpenCache.init(testing.allocator);
    defer cache.deinit();
    const idents = try builtinIdents(&f, &cache);
    const before = f.store.len();
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var blockers: std.ArrayList(Var) = .empty;
    defer blockers.deinit(testing.allocator);
    try exhaustive.collectAbsentCtorPayloadBlockersForConstructedTags(
        arena.allocator(),
        &f.store,
        idents,
        root,
        &.{ok},
        &blockers,
    );
    try testing.expectEqualSlices(Var, &.{actual_b}, blockers.items);
    try testing.expectEqual(before, f.store.len());
    try testing.expect(f.store.resolveVar(private).desc.content == .flex);
    // The returned root is a real solver root, and the reader has ended before
    // this mutation. A new query sees the closed argument, not stale views.
    try f.store.setVarContent(actual_b, .{ .structure = .empty_tag_union });
    blockers.clearRetainingCapacity();
    try exhaustive.collectAbsentCtorPayloadBlockersForConstructedTags(
        arena.allocator(),
        &f.store,
        idents,
        root,
        &.{ok},
        &blockers,
    );
    try testing.expectEqual(@as(usize, 0), blockers.items.len);
    try testing.expectEqual(before, f.store.len());
}

test "nominal views private unknown assumptions remain application scoped" {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const unknown = try f.store.fresh();
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const unit = try f.store.freshFromContent(.{ .structure = .empty_record });
    const nominal = try f.declaration("Private", &.{a}, try f.tuple(&.{unknown}));
    const left_app = try f.application(nominal, &.{empty});
    const right_app = try f.application(nominal, &.{unit});
    var cache = exhaustive.NominalOpenCache.init(testing.allocator);
    defer cache.deinit();
    const idents = try builtinIdents(&f, &cache);
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const left = (try reader.openNominalBacking(left_app)).?;
    const right = (try reader.openNominalBacking(right_app)).?;
    const left_unknown = (try tupleChildren(&reader, left))[0];
    const right_unknown = (try tupleChildren(&reader, right))[0];
    try testing.expect(left_unknown != right_unknown);
    const left_pattern: exhaustive.Pattern = .{ .anything = left };
    const right_pattern: exhaustive.Pattern = .{ .anything = right };
    try testing.expect(!try left_pattern.isInhabitedWithKnownEmpty(&reader, idents, &.{left_unknown}));
    try testing.expect(try right_pattern.isInhabitedWithKnownEmpty(&reader, idents, &.{left_unknown}));
    try testing.expect(!try right_pattern.isInhabitedWithKnownEmpty(&reader, idents, &.{right_unknown}));
}

test "nominal views ignored open rows keep scoped known-empty assumptions" {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const open = try f.rigid("_others");
    const left_empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const right_empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const tags = try f.store.appendTags(&.{
        .{ .name = try f.ident("Known"), .args = try f.store.appendVars(&.{a}) },
    });
    const backing = try f.store.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = tags, .ext = open } } });
    const nominal = try f.declaration("Open", &.{a}, backing);
    const left_app = try f.application(nominal, &.{left_empty});
    const right_app = try f.application(nominal, &.{right_empty});
    var cache = exhaustive.NominalOpenCache.init(testing.allocator);
    defer cache.deinit();
    const idents = try builtinIdents(&f, &cache);
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const left = (try reader.openNominalBacking(left_app)).?;
    const right = (try reader.openNominalBacking(right_app)).?;
    const left_ext = (try reader.resolveVar(left)).desc.content.structure.tag_union.ext;
    const right_ext = (try reader.resolveVar(right)).desc.content.structure.tag_union.ext;
    try testing.expect(left_ext != right_ext);
    try testing.expectEqual(@as(?Var, null), reader.sourceVar(left_ext));
    try testing.expect((try reader.resolveVar(left_ext)).desc.content.rigid.name.attributes.ignored);
    const left_pattern: exhaustive.Pattern = .{ .anything = left };
    const right_pattern: exhaustive.Pattern = .{ .anything = right };
    try testing.expect(try left_pattern.isInhabited(&reader, idents));
    try testing.expect(!try left_pattern.isInhabitedWithKnownEmpty(&reader, idents, &.{left_ext}));
    try testing.expect(try right_pattern.isInhabitedWithKnownEmpty(&reader, idents, &.{left_ext}));
}

test "nominal views shared uninhabited payloads are not recursive cycles" {
    var f = try Fixture.init();
    defer f.deinit();
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const payload = try f.tuple(&.{empty});
    const tags = try f.store.appendTags(&.{
        .{ .name = try f.ident("First"), .args = try f.store.appendVars(&.{payload}) },
        .{ .name = try f.ident("Second"), .args = try f.store.appendVars(&.{payload}) },
    });
    const union_var = try f.store.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = tags, .ext = empty } } });
    var cache = exhaustive.NominalOpenCache.init(testing.allocator);
    defer cache.deinit();
    const idents = try builtinIdents(&f, &cache);
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const pattern: exhaustive.Pattern = .{ .anything = union_var };
    try testing.expect(!try pattern.isInhabited(&reader, idents));
    try testing.expect(!try exhaustive.isCtorPayloadTypeInhabited(&reader, idents, union_var));
}

test "nominal views nested SCC answers are independent of alternative order" {
    var f = try Fixture.init();
    defer f.deinit();
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const a = try f.store.fresh();
    const b = try f.store.fresh();
    const c = try f.store.fresh();
    const d = try f.store.fresh();
    try f.store.setVarContent(a, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{ b, empty }) } } });
    try f.store.setVarContent(b, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{a}) } } });
    try f.store.setVarContent(c, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{ d, a }) } } });
    try f.store.setVarContent(d, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{c}) } } });
    const first: types.Tag = .{ .name = try f.ident("First"), .args = try f.store.appendVars(&.{c}) };
    const second: types.Tag = .{ .name = try f.ident("Second"), .args = try f.store.appendVars(&.{d}) };
    const forward = try f.store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try f.store.appendTags(&.{ first, second }),
        .ext = empty,
    } } });
    const backward = try f.store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try f.store.appendTags(&.{ second, first }),
        .ext = empty,
    } } });
    var cache = exhaustive.NominalOpenCache.init(testing.allocator);
    defer cache.deinit();
    const idents = try builtinIdents(&f, &cache);
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    for ([_]Var{ forward, backward, a, b, c, d, backward, forward }) |root| {
        const pattern: exhaustive.Pattern = .{ .anything = root };
        try testing.expect(!try pattern.isInhabited(&reader, idents));
    }
}

test "nominal views preserve scoped actuals aliases and owned unknowns without solver growth" {
    var f = try Fixture.init();
    defer f.deinit();
    const inner_a = try f.rigid("a");
    const outer_a = try f.rigid("a");
    const associated_a = try f.rigid("a");
    const actual_a = try f.rigid("a");
    const unknown = try f.store.fresh();
    const inner = try f.declaration("Inner", &.{inner_a}, try f.tuple(&.{inner_a}));
    const inner_app = try f.application(inner, &.{associated_a});
    const inner_var = try f.store.freshFromContent(.{ .structure = .{ .nominal_type = inner_app } });
    const alias_backing = try f.tuple(&.{outer_a});
    const alias = try f.store.freshFromContent(.{ .alias = .{
        .ident = .{ .ident_idx = try f.ident("Alias") },
        .vars = .{ .nonempty = try f.store.appendVars(&.{ alias_backing, outer_a, unknown }) },
        .source_arg_count = 1,
        .origin_module = @enumFromInt(0),
    } });
    const outer = try f.declaration("Outer", &.{outer_a}, try f.tuple(&.{ inner_var, outer_a, unknown, alias }));
    const app = try f.application(outer, &.{actual_a});
    const before = f.store.len();
    const vars_before = f.store.vars.len();
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const opened = (try reader.openNominalBacking(app)).?;
    const children = try tupleChildren(&reader, opened);
    try testing.expectEqual(actual_a, children[1]);
    try testing.expectEqual(@as(?Var, actual_a), reader.sourceVar(children[1]));
    try testing.expectEqual(@as(?Var, null), reader.sourceVar(children[2]));
    try testing.expect((try reader.resolveVar(children[2])).desc.content == .flex);
    const projected_inner = (try reader.resolveVar(children[0])).desc.content.structure.nominal_type;
    const nested = (try reader.openNominalBacking(projected_inner)).?;
    try testing.expectEqual(actual_a, (try tupleChildren(&reader, nested))[0]);
    const projected_alias = (try reader.resolveVar(children[3])).desc.content.alias;
    try testing.expectEqual(actual_a, (try tupleChildren(&reader, reader.getAliasBackingVar(projected_alias)))[0]);
    try testing.expectEqual(children[2], reader.sliceVars(projected_alias.vars.nonempty)[2]);
    // Captured children remain valid across all the nested projections.
    try testing.expectEqual(actual_a, children[1]);
    try testing.expectEqual(opened, (try reader.openNominalBacking(app)).?);
    try testing.expectEqual(before, f.store.len());
    try testing.expectEqual(vars_before, f.store.vars.len());
}

test "nominal views recursion keys distinguish permutations and converge after closed resets" {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const b = try f.rigid("b");
    const backing = try f.store.fresh();
    const pair = try f.declaration("Pair", &.{ a, b }, backing);
    const swapped = try f.store.freshFromContent(.{ .structure = .{
        .nominal_type = try f.application(pair, &.{ b, a }),
    } });
    try f.store.setVarContent(backing, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{ a, swapped }) } } });
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const unit = try f.store.freshFromContent(.{ .structure = .empty_record });
    const pair_app = try f.application(pair, &.{ empty, unit });
    const reset_backing = try f.store.fresh();
    const reset = try f.declaration("Reset", &.{a}, reset_backing);
    const closed = try f.store.freshFromContent(.{ .structure = .{
        .nominal_type = try f.application(reset, &.{unit}),
    } });
    try f.store.setVarContent(reset_backing, .{ .structure = .{ .tuple = .{ .elems = try f.store.appendVars(&.{ a, closed }) } } });
    const reset_app = try f.application(reset, &.{empty});
    const before = f.store.len();
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const first = (try reader.openNominalBacking(pair_app)).?;
    const first_children = try tupleChildren(&reader, first);
    const second = (try reader.openNominalBacking((try reader.resolveVar(first_children[1])).desc.content.structure.nominal_type)).?;
    const second_children = try tupleChildren(&reader, second);
    try testing.expect(first != second);
    try testing.expectEqual(empty, first_children[0]);
    try testing.expectEqual(unit, second_children[0]);
    try testing.expectEqual(first, (try reader.openNominalBacking((try reader.resolveVar(second_children[1])).desc.content.structure.nominal_type)).?);
    const reset_first = (try reader.openNominalBacking(reset_app)).?;
    const reset_children = try tupleChildren(&reader, reset_first);
    const reset_second = (try reader.openNominalBacking((try reader.resolveVar(reset_children[1])).desc.content.structure.nominal_type)).?;
    const stable_children = try tupleChildren(&reader, reset_second);
    const reset_third = (try reader.openNominalBacking((try reader.resolveVar(stable_children[1])).desc.content.structure.nominal_type)).?;
    try testing.expect(reset_first != reset_second);
    try testing.expectEqual(reset_second, reset_third);
    try testing.expectEqual(before, f.store.len());
}

fn allocationFailureCase(gpa: std.mem.Allocator) !void {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const empty = try f.store.freshFromContent(.{ .structure = .empty_tag_union });
    const optional = try f.store.freshFromContent(.{ .field_presence = .optional });
    const defaulted = try f.store.freshFromContent(.{ .field_presence = .{ .defaulted = .{ .origin_module = @enumFromInt(0), .expr_node = 42 } } });
    const ext = try f.store.freshFromContent(.{ .structure = .empty_record });
    const fields = try f.store.appendRecordFields(&.{
        .{ .name = try f.ident("optional"), .presence = .unknown(optional, a) },
        .{ .name = try f.ident("defaulted"), .presence = .unknown(defaulted, a) },
    });
    const record = try f.store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });
    const nominal = try f.declaration("Fields", &.{a}, record);
    const app = try f.application(nominal, &.{empty});
    const before = f.store.len();
    var reader = Reader.init(gpa, &f.store);
    defer reader.deinit();
    const opened = (try reader.openNominalBacking(app)).?;
    const projected = (try reader.resolveVar(opened)).desc.content.structure.record;
    const first = reader.getRecordFieldAt(projected.fields, 0).presence;
    const second = reader.getRecordFieldAt(projected.fields, 1).presence;
    try testing.expectEqual(empty, first.typeVar());
    try testing.expect((try reader.resolveVar(first.presenceVar().?)).desc.content.field_presence == .optional);
    try testing.expectEqual(@as(u32, 42), (try reader.resolveVar(second.presenceVar().?)).desc.content.field_presence.defaulted.expr_node);
    try testing.expectEqual(before, f.store.len());
}

test "nominal views preserve field presence and clean up every allocation failure" {
    try testing.checkAllAllocationFailures(testing.allocator, allocationFailureCase, .{});
}

test "nominal views retain captured field slices across projection growth" {
    var f = try Fixture.init();
    defer f.deinit();
    const a = try f.rigid("a");
    const ext = try f.store.freshFromContent(.{ .structure = .empty_record });
    const fields = try f.store.appendRecordFields(&.{
        .{ .name = try f.ident("value"), .presence = .required(a) },
    });
    const record = try f.store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });
    const nominal = try f.declaration("Record", &.{a}, record);
    var applications: [64]types.NominalType = undefined;
    var actuals: [64]Var = undefined;
    for (&applications, &actuals) |*app, *actual| {
        actual.* = try f.store.fresh();
        app.* = try f.application(nominal, &.{actual.*});
    }
    const before = f.store.len();
    var reader = Reader.init(testing.allocator, &f.store);
    defer reader.deinit();
    const first = (try reader.resolveVar((try reader.openNominalBacking(applications[0])).?)).desc.content.structure.record;
    const captured = reader.getRecordFieldsSlice(first.fields).items(.presence);
    for (applications[1..], actuals[1..]) |app, actual| {
        const projected = (try reader.resolveVar((try reader.openNominalBacking(app)).?)).desc.content.structure.record;
        try testing.expectEqual(actual, reader.getRecordFieldAt(projected.fields, 0).presence.typeVar());
    }
    try testing.expectEqual(actuals[0], captured[0].typeVar());
    try testing.expectEqual(before, f.store.len());
}

test "nominal views exhaustive recursive permutation and mutual closed reset" {
    const source =
        \\Pair(a, b) := [Again(Pair(b, a)), Stop(a)]
        \\Left(a) := [LeftEnd(a), ToRight(Right({}))]
        \\Right(a) := [RightEnd(a), ToLeft(Left({}))]
        \\pair_value : Pair(I64, Str)
        \\pair_value = Stop(42)
        \\pair_result = match pair_value {
        \\    Stop(_) => 1
        \\    Again(_) => 2
        \\}
        \\left_value : Left(I64)
        \\left_value = LeftEnd(42)
        \\result : I64
        \\result = match left_value {
        \\    LeftEnd(_) => pair_result
        \\    ToRight(_) => 0
        \\}
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertLastDefType("I64");
}

test "nominal views polymorphic Try empty error through record tuple and list patterns" {
    const source =
        \\unwrap : Try(a, []) -> a
        \\unwrap = |value| match value {
        \\    Ok(payload) => payload
        \\}
        \\extract : { value: Try((a, a), []), rest: List(a) } -> a
        \\extract = |record| match record {
        \\    { value: Ok((left, _)), rest: [] } => left
        \\    { value: Ok((_, right)), rest: [_, ..] } => right
        \\}
        \\result : I64
        \\result = unwrap(Ok(extract({ value: Ok((1, 2)), rest: [] })))
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertLastDefType("I64");
}
