//! Tests for replayed openings of nominal declaration backings
//! (`instantiate.BackingOpenings`): a replay must leave the store exactly as
//! walking the backing template does.

const std = @import("std");
const base = @import("base");

const types_mod = @import("../types.zig");
const Store = @import("../store.zig").Store;
const instantiate = @import("../instantiate.zig");

const Ident = base.Ident;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const NominalDecl = types_mod.NominalDecl;
const NominalType = types_mod.NominalType;
const Rank = types_mod.Rank;
const DenseMap = @import("collections").DenseMap(Var, Var);

const origin: base.ModuleIdentity.Idx = @fromBackingInt(1);

const TestError = std.mem.Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual };

/// Register `Decl(x, y) := { a : Box(x), b : [T(x, y), U], c : Wrap(x, y), d : x -> y, e ?: y }`
/// in `store`, where `Wrap(x, y) : (x, y)`. With `shared`, the backing's
/// `c` field is a variable outside the template that every opening shares.
fn registerDecl(store: *Store, idents: *Ident.Store, gpa: std.mem.Allocator, shared: bool) std.mem.Allocator.Error!NominalDecl.Idx {
    const name = struct {
        fn of(ids: *Ident.Store, allocator: std.mem.Allocator, text: []const u8) std.mem.Allocator.Error!Ident.Idx {
            return ids.insert(allocator, Ident.for_text(text));
        }
    }.of;
    const g: Rank = .generalized;
    const x = try store.freshFromContentWithRank(.{ .rigid = Rigid.init(try name(idents, gpa, "x")) }, g);
    const y = try store.freshFromContentWithRank(.{ .rigid = Rigid.init(try name(idents, gpa, "y")) }, g);
    const box = try store.freshFromContentWithRank(try store.mkNominalWithSourceDeclAndBuiltinOrigin(
        .{ .ident_idx = try name(idents, gpa, "Box") },
        &.{x},
        origin,
        null,
        false,
        false,
    ), g);
    const tags = try store.appendTags(&.{
        .{ .name = try name(idents, gpa, "T"), .args = try store.appendVars(&.{ x, y }) },
        .{ .name = try name(idents, gpa, "U"), .args = try store.appendVars(&.{}) },
    });
    const tag_ext = try store.freshFromContentWithRank(.{ .structure = .empty_tag_union }, g);
    const tag_union = try store.freshFromContentWithRank(.{ .structure = .{ .tag_union = .{ .tags = tags, .ext = tag_ext } } }, g);
    const pair = try store.freshFromContentWithRank(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ x, y }) } } }, g);
    const wrap = if (shared)
        try store.freshFromContentWithRank(.{ .flex = Flex.init() }, .outermost)
    else
        try store.freshFromContentWithRank(try store.mkAlias(.{ .ident_idx = try name(idents, gpa, "Wrap") }, pair, &.{ x, y }, origin), g);
    const func = try store.freshFromContentWithRank(try store.mkFuncPure(&.{x}, y), g);
    const presence = try store.freshFromContentWithRank(.{ .field_presence = .optional }, g);
    const fields = try store.appendRecordFields(&.{
        .{ .name = try name(idents, gpa, "a"), .presence = .required(box) },
        .{ .name = try name(idents, gpa, "b"), .presence = .required(tag_union) },
        .{ .name = try name(idents, gpa, "c"), .presence = .required(wrap) },
        .{ .name = try name(idents, gpa, "d"), .presence = .required(func) },
        .{ .name = try name(idents, gpa, "e"), .presence = .unknown(presence, y) },
    });
    const record_ext = try store.freshFromContentWithRank(.{ .structure = .empty_record }, g);
    const backing = try store.freshFromContentWithRank(.{ .structure = .{ .record = .{ .fields = fields, .ext = record_ext } } }, g);
    return store.registerNominalDecl(.{
        .ident = .{ .ident_idx = try name(idents, gpa, "Decl") },
        .origin_module = origin,
        .source = try NominalType.Source.initChecked(try types_mod.SourceDecl.fromStatementChecked(3), false, false),
        .formals = try store.appendVars(&.{ x, y }),
        .backing = backing,
        .flags = .{ .valid = true },
    });
}

/// Where an opening's args come from.
const Args = enum {
    /// Two fresh variables.
    fresh,
    /// One fresh variable, twice.
    repeated,
    /// The args of a nominal application, a slice of the vars pool that the
    /// opening's own appends move.
    pool,
};

/// Open the declaration in `walked` by walking its template and in
/// `replayed` through its recorded openings, with the same fresh args at the
/// same rank, and require the stores to stay identical.
fn openBoth(walked: *Store, replayed: *Store, idents: *Ident.Store, var_map: *DenseMap, decl_idx: NominalDecl.Idx, rank: Rank, args_from: Args) TestError!void {
    var args: [2]Var = undefined;
    for (&args, 0..) |*arg, index| {
        if (args_from == .repeated and index == 1) {
            arg.* = args[0];
            continue;
        }
        arg.* = try walked.freshFromContentWithRank(.{ .flex = Flex.init() }, rank);
        try std.testing.expectEqual(arg.*, try replayed.freshFromContentWithRank(.{ .flex = Flex.init() }, rank));
    }
    var walked_args: []const Var = &args;
    var replayed_args: []const Var = &args;
    if (args_from == .pool) {
        walked_args = try poolArgs(walked, idents, &args);
        replayed_args = try poolArgs(replayed, idents, &args);
        try std.testing.expectEqualSlices(Var, walked_args, replayed_args);
    }
    const walked_opened = try instantiate.instantiateNominalBacking(walked, idents, var_map, walked.getNominalDecl(decl_idx), walked_args, rank, .instantiation);
    const opening = try instantiate.openNominalBackingRecorded(replayed, idents, var_map, decl_idx, replayed.getNominalDecl(decl_idx), replayed_args, rank);
    try std.testing.expectEqual(walked_opened, opening.opened);
    try expectSameStores(walked, replayed);
}

/// `args` as the args of an application in `store`'s vars pool, with the
/// pool filled to its capacity so that the next append moves it.
fn poolArgs(store: *Store, idents: *Ident.Store, args: []const Var) std.mem.Allocator.Error![]const Var {
    const content = try store.mkNominalWithSourceDeclAndBuiltinOrigin(
        .{ .ident_idx = try idents.insert(store.gpa, Ident.for_text("App")) },
        args,
        origin,
        null,
        false,
        false,
    );
    while (store.vars.capacity > store.vars.items.len) _ = try store.appendVars(&.{});
    return store.sliceNominalArgs(content.structure.nominal_type);
}

fn expectSameStores(walked: *const Store, replayed: *const Store) TestError!void {
    try std.testing.expectEqual(walked.len(), replayed.len());
    var index: u32 = 0;
    while (index < walked.len()) : (index += 1) {
        const var_: Var = @fromBackingInt(index);
        const a = walked.resolveVar(var_);
        const b = replayed.resolveVar(var_);
        try std.testing.expectEqual(a.var_, b.var_);
        try std.testing.expect(std.meta.eql(walked.rowAt(a.desc_idx), replayed.rowAt(b.desc_idx)));
    }
    try std.testing.expectEqualSlices(Var, walked.vars.items, replayed.vars.items);
    try std.testing.expectEqual(walked.record_fields.len(), replayed.record_fields.len());
    index = 0;
    while (index < walked.record_fields.len()) : (index += 1) {
        try std.testing.expect(std.meta.eql(walked.record_fields.get(@fromBackingInt(index)), replayed.record_fields.get(@fromBackingInt(index))));
    }
    try std.testing.expectEqual(walked.tags.len(), replayed.tags.len());
    index = 0;
    while (index < walked.tags.len()) : (index += 1) {
        try std.testing.expect(std.meta.eql(walked.tags.get(@fromBackingInt(index)), replayed.tags.get(@fromBackingInt(index))));
    }
    try std.testing.expectEqual(walked.groundEpoch(), replayed.groundEpoch());
}

const Fixture = struct {
    idents: Ident.Store,
    walked: Store,
    replayed: Store,
    var_map: DenseMap,
    decl_idx: NominalDecl.Idx,

    fn init(fixture: *Fixture, gpa: std.mem.Allocator, shared: bool) TestError!void {
        fixture.idents = try Ident.Store.initCapacity(gpa, 16);
        fixture.walked = try Store.initCapacity(gpa, 16, 8);
        fixture.replayed = try Store.initCapacity(gpa, 16, 8);
        fixture.var_map = DenseMap.init(gpa);
        fixture.decl_idx = try registerDecl(&fixture.walked, &fixture.idents, gpa, shared);
        try std.testing.expectEqual(fixture.decl_idx, try registerDecl(&fixture.replayed, &fixture.idents, gpa, shared));
    }

    fn deinit(fixture: *Fixture, gpa: std.mem.Allocator) void {
        fixture.var_map.deinit();
        fixture.replayed.deinit();
        fixture.walked.deinit();
        fixture.idents.deinit(gpa);
    }

    fn open(fixture: *Fixture, rank: Rank, args_from: Args) TestError!void {
        try openBoth(&fixture.walked, &fixture.replayed, &fixture.idents, &fixture.var_map, fixture.decl_idx, rank, args_from);
    }
};

test "replayed backing openings write what walking the template writes" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, false);
    defer fixture.deinit(gpa);

    try fixture.open(.outermost, .fresh);
    const recording = fixture.replayed.backing_openings.recordings.get(fixture.decl_idx).?;
    try std.testing.expect(recording.replayable);
    try fixture.open(@fromBackingInt(@intCast(3)), .fresh);
    try fixture.open(@fromBackingInt(@intCast(2)), .fresh);
    // Repeated args are substituted positionally like any others.
    try fixture.open(.outermost, .repeated);
    try fixture.open(.outermost, .pool);
}

test "an opening reads args the pool it appends to held" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, false);
    defer fixture.deinit(gpa);

    try fixture.open(.outermost, .pool);
    try fixture.open(.outermost, .pool);
}

test "a replay inside a probe writes what walking the template writes" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, false);
    defer fixture.deinit(gpa);

    // The recording made inside a discarded probe outlives it.
    const walked_mark = fixture.walked.beginProbe();
    const replayed_mark = fixture.replayed.beginProbe();
    try fixture.open(.outermost, .fresh);
    fixture.walked.endProbeNo(&walked_mark);
    fixture.replayed.endProbeNo(&replayed_mark);
    try expectSameStores(&fixture.walked, &fixture.replayed);
    try fixture.open(@fromBackingInt(@intCast(2)), .fresh);

    _ = fixture.walked.beginProbe();
    _ = fixture.replayed.beginProbe();
    try fixture.open(.outermost, .fresh);
    try fixture.walked.endProbeYes();
    try fixture.replayed.endProbeYes();
    try expectSameStores(&fixture.walked, &fixture.replayed);
    try fixture.open(@fromBackingInt(@intCast(2)), .fresh);
}

test "an opening with repeated args is not recorded" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, false);
    defer fixture.deinit(gpa);

    try fixture.open(.outermost, .repeated);
    try std.testing.expect(fixture.replayed.backing_openings.recordings.get(fixture.decl_idx) == null);
    try fixture.open(.outermost, .fresh);
    try std.testing.expect(fixture.replayed.backing_openings.recordings.get(fixture.decl_idx).?.replayable);
}

test "an opening that shares a template variable is walked every time" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, true);
    defer fixture.deinit(gpa);

    try fixture.open(.outermost, .fresh);
    try std.testing.expect(!fixture.replayed.backing_openings.recordings.get(fixture.decl_idx).?.replayable);
    try fixture.open(@fromBackingInt(@intCast(2)), .fresh);
}

/// Merge two classes of the declaration's template with equal classes,
/// keeping the other class's descriptor, as unification merges equal
/// variable-free classes: `Box(x)` with an application of `Box` to the same
/// `x` whose args are stored elsewhere, and the record's closed extension
/// with an empty record that carries a flag an opening copies.
fn mergeTemplateClasses(store: *Store, idents: *Ident.Store, decl_idx: NominalDecl.Idx) std.mem.Allocator.Error!void {
    const decl = store.getNominalDecl(decl_idx);
    const x = store.sliceVars(decl.formals)[0];
    const record = store.resolveVar(decl.backing).desc.content.structure.record;
    const template_box = store.getRecordFieldAt(record.fields, 0).presence.var_;
    const other_box = try store.freshFromContentWithRank(try store.mkNominalWithSourceDeclAndBuiltinOrigin(
        .{ .ident_idx = try idents.insert(store.gpa, Ident.for_text("Box")) },
        &.{x},
        origin,
        null,
        false,
        false,
    ), .generalized);
    try store.unionKeepingContent(template_box, other_box, .b, .generalized);

    const other_ext = try store.freshFromContentWithRank(.{ .structure = .empty_record }, .generalized);
    try store.markComparedPlain(other_ext);
    try store.unionKeepingContent(record.ext, other_ext, .b, .generalized);
}

test "an opening after template classes merged walks the template as it now is" {
    const gpa = std.testing.allocator;
    var fixture: Fixture = undefined;
    try fixture.init(gpa, false);
    defer fixture.deinit(gpa);

    try fixture.open(.outermost, .fresh);
    try std.testing.expect(fixture.replayed.backing_openings.recordings.get(fixture.decl_idx).?.replayable);
    try mergeTemplateClasses(&fixture.walked, &fixture.idents, fixture.decl_idx);
    try mergeTemplateClasses(&fixture.replayed, &fixture.idents, fixture.decl_idx);
    try expectSameStores(&fixture.walked, &fixture.replayed);
    try fixture.open(@fromBackingInt(@intCast(2)), .fresh);
    try std.testing.expect(fixture.replayed.backing_openings.recordings.get(fixture.decl_idx).?.replayable);
    try fixture.open(.outermost, .fresh);
}
