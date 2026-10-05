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
const NominalOpening = @import("../instantiate.zig").NominalOpening;

const Ident = base.Ident;

const TypeIdent = types_mod.TypeIdent;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const Record = types_mod.Record;
const RecordField = types_mod.RecordField;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;

fn nominalOpeningFixture(env: *TestEnv) !struct {
    decl: types_mod.NominalDecl,
    actual: Var,
    formal: Var,
    associated_formal: Var,
    private: Var,
} {
    const formal = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    // Associated references can name the formal through a different root.
    const associated_formal = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const private = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    const actual = try env.types.freshFromContentWithRank(.{ .structure = .empty_record }, .outermost);
    const tail = try env.types.freshFromContentWithRank(.{ .structure = .empty_tag_union }, .generalized);
    const row = try env.mkTagUnion(&.{
        try env.mkTag("Some", &.{ formal, private }),
        try env.mkTag("Other", &.{ associated_formal, private }),
    }, tail);
    return .{
        .decl = .{
            .ident = try env.mkTypeIdent("Choice"),
            .origin_module = @enumFromInt(0),
            .source = types_mod.NominalType.Source.init(types_mod.SourceDecl.fromStatement(0), false, false),
            .formals = try env.types.appendVars(&.{formal}),
            .backing = try env.types.freshFromContentWithRank(row.content, .generalized),
            .flags = .{ .valid = true },
        },
        .actual = actual,
        .formal = formal,
        .associated_formal = associated_formal,
        .private = private,
    };
}

test "nominal opening - delayed demands preserve substitutions sharing and rank" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const baseline = env.types.len();
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    try std.testing.expectEqual(baseline, env.types.len());
    try std.testing.expectEqual(fixture.actual, try opening.demand(fixture.formal));
    try std.testing.expectEqual(fixture.actual, try opening.demand(fixture.associated_formal));
    const private = try opening.demand(fixture.private);
    try std.testing.expect(private != fixture.private);
    try std.testing.expectEqual(types_mod.Rank.outermost, env.types.resolveVar(private).desc.rank);
    const backing = try opening.materialize();
    try std.testing.expectEqual(backing, try opening.materialize());
    const row = env.types.resolveVar(backing).desc.content.structure.tag_union;
    const tags = env.types.tags.sliceRange(row.tags);
    for (tags.items(.args)) |args_range| {
        const args = env.types.sliceVars(args_range);
        try std.testing.expectEqual(fixture.actual, args[0]);
        try std.testing.expectEqual(private, args[1]);
    }
    const tail = env.types.resolveVar(row.ext);
    try std.testing.expect(tail.desc.content.structure == .empty_tag_union);
    try std.testing.expectEqual(types_mod.Rank.outermost, tail.desc.rank);
    // No template variable was changed by any demand.
    try std.testing.expect(env.types.resolveVar(fixture.private).desc.content == .flex);
    try std.testing.expectEqual(types_mod.Rank.generalized, env.types.resolveVar(fixture.private).desc.rank);
}

test "nominal opening - demand delta publishes only newly produced bindings" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    var changes: std.ArrayListUnmanaged(NominalOpening.BindingChange) = .empty;
    defer changes.deinit(env.gpa);
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    opening.binding_changes = &changes;
    const seeded = opening.var_map.count();
    const private = try opening.demand(fixture.private);
    try std.testing.expectEqual(@as(usize, 1), changes.items.len);
    try std.testing.expectEqual(fixture.private, changes.items[0].template);
    try std.testing.expectEqual(private, changes.items[0].owned);
    try std.testing.expectEqual(fixture.actual, try opening.demand(fixture.formal));
    try std.testing.expectEqual(@as(usize, 1), changes.items.len);
    _ = try opening.materialize();
    try std.testing.expectEqual(opening.var_map.count() - seeded, changes.items.len);
    for (changes.items) |change| {
        try std.testing.expect(change.inserted);
        try std.testing.expectEqual(change.owned, opening.var_map.get(change.template).?);
    }
    const published = changes.items.len;
    _ = try opening.materialize();
    try std.testing.expectEqual(private, try opening.demand(fixture.private));
    try std.testing.expectEqual(published, changes.items.len);
}

test "nominal opening - independent applications do not share private cells" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    var first = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer first.deinit();
    var second = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, types_mod.Rank.outermost.next());
    defer second.deinit();
    const first_private = try first.demand(fixture.private);
    _ = try first.materialize();
    // Reversed demand order has the same within-opening equalities.
    const second_backing = try second.materialize();
    const second_private = try second.demand(fixture.private);
    try std.testing.expect(first_private != second_private);
    try std.testing.expectEqual(types_mod.Rank.outermost.next(), env.types.resolveVar(second_private).desc.rank);
    const row = env.types.resolveVar(second_backing).desc.content.structure.tag_union;
    for (env.types.tags.sliceRange(row.tags).items(.args)) |args_range| {
        const args = env.types.sliceVars(args_range);
        try std.testing.expectEqual(fixture.actual, args[0]);
        try std.testing.expectEqual(second_private, args[1]);
    }
}

test "nominal opening - latent reads distinguish schema and owned children" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    const baseline = env.types.len();
    const associated = opening.read(.{ .template = fixture.associated_formal });
    try std.testing.expectEqual(fixture.actual, associated.reference.owned);
    const private = opening.read(.{ .template = fixture.private });
    try std.testing.expectEqual(fixture.private, private.reference.template);
    const schema = opening.read(.{ .template = fixture.decl.backing });
    try std.testing.expectEqual(fixture.decl.backing, schema.reference.template);
    const schema_args = env.types.tags.sliceRange(schema.content.structure.tag_union.tags).items(.args);
    for (schema_args) |args_range| {
        const args = env.types.sliceVars(args_range);
        const formal = opening.read(opening.child(schema.reference, args[0]));
        try std.testing.expectEqual(fixture.actual, formal.reference.owned);
        const unknown = opening.read(opening.child(schema.reference, args[1]));
        try std.testing.expectEqual(fixture.private, unknown.reference.template);
    }
    try std.testing.expectEqual(baseline, env.types.len());
    const backing = try opening.materialize();
    const owned = opening.read(.{ .template = fixture.decl.backing });
    try std.testing.expectEqual(backing, owned.reference.owned);
    const owned_args = env.types.tags.sliceRange(owned.content.structure.tag_union.tags).items(.args);
    for (owned_args) |args_range| {
        const args = env.types.sliceVars(args_range);
        // These are instance cells, not template IDs needing substitution again.
        try std.testing.expectEqual(args[1], opening.read(opening.child(owned.reference, args[1])).reference.owned);
    }
}

test "nominal opening - speculative materialization restores demand map and cells" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    const baseline = env.types.len();
    const mapped = opening.var_map.count();
    var checkpoint = try opening.checkpoint();
    defer checkpoint.deinit();
    var savepoint = try env.types.createSavepoint();
    _ = try opening.materialize();
    try std.testing.expect(env.types.len() > baseline);
    env.types.rollbackToSavepoint(&savepoint);
    opening.restore(&checkpoint);
    try std.testing.expectEqual(baseline, env.types.len());
    try std.testing.expectEqual(mapped, opening.var_map.count());
    try std.testing.expectEqual(fixture.private, opening.read(.{ .template = fixture.private }).reference.template);
    // A subsequent successful demand creates a complete fresh map.
    const private = try opening.demand(fixture.private);
    const backing = try opening.materialize();
    const tags = env.types.tags.sliceRange(env.types.resolveVar(backing).desc.content.structure.tag_union.tags);
    for (tags.items(.args)) |range| try std.testing.expectEqual(private, env.types.sliceVars(range)[1]);
}

test "nominal opening - complete demand has eager allocation and sharing equivalence" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const eager_baseline = env.types.len();
    _ = try @import("../instantiate.zig").instantiateNominalBacking(
        &env.types,
        &env.idents,
        &env.var_map,
        fixture.decl,
        &.{fixture.actual},
        .outermost,
        .instantiation,
    );
    const eager_cells = env.types.len() - eager_baseline;
    const lazy_baseline = env.types.len();
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    _ = try opening.demand(fixture.private);
    _ = try opening.materialize();
    try std.testing.expectEqual(eager_cells, env.types.len() - lazy_baseline);
    try std.testing.expectEqual(env.var_map.count(), opening.var_map.count());
    var iterator = env.var_map.iterator();
    while (iterator.next()) |entry| {
        const eager = env.types.resolveVar(entry.value_ptr.*);
        const demanded = env.types.resolveVar(opening.var_map.get(entry.key_ptr.*).?);
        try std.testing.expectEqual(eager.desc.rank, demanded.desc.rank);
        try std.testing.expectEqual(std.meta.activeTag(eager.desc.content), std.meta.activeTag(demanded.desc.content));
        if (entry.value_ptr.* == fixture.actual) try std.testing.expectEqual(fixture.actual, demanded.var_);
    }
}

test "nominal opening - late cells retain per-root rank history without resetting owned ranks" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const opening_rank = types_mod.Rank.outermost.next().next();
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, opening_rank);
    defer opening.deinit();
    try opening.recordRankHistory(fixture.private, .outermost);
    try std.testing.expectEqual(types_mod.Rank.outermost, opening.effectiveRank(.{ .template = fixture.private }));
    try std.testing.expectEqual(opening_rank, opening.effectiveRank(.{ .template = fixture.decl.backing }));
    var checkpoint = try opening.checkpoint();
    defer checkpoint.deinit();
    try opening.recordRankHistory(fixture.decl.backing, types_mod.Rank.outermost.next());
    opening.restore(&checkpoint);
    try std.testing.expectEqual(opening_rank, opening.effectiveRank(.{ .template = fixture.decl.backing }));
    const private = try opening.demand(fixture.private);
    try std.testing.expectEqual(types_mod.Rank.outermost, env.types.resolveVar(private).desc.rank);
    // Once demanded, ordinary solver rank changes are authoritative.
    try env.types.setDescRank(env.types.resolveVar(private).desc_idx, .generalized);
    const backing = try opening.materialize();
    try std.testing.expectEqual(opening_rank, env.types.resolveVar(backing).desc.rank);
    try std.testing.expectEqual(types_mod.Rank.generalized, env.types.resolveVar(private).desc.rank);
}

test "nominal opening - materialization retains latent recursive constraints and effects" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const effect = try env.types.freshFromContentWithRank(.{ .structure = .{ .fn_effectful = .{
        .args = try env.types.appendVars(&.{}),
        .ret = fixture.formal,
    } } }, .generalized);
    const method = try env.types.freshFromContentWithRank(.{ .structure = .{ .fn_unbound = .{
        .args = try env.types.appendVars(&.{fixture.private}),
        .ret = fixture.private,
        .effect_deps = try env.types.appendVars(&.{effect}),
    } } }, .generalized);
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.idents.insert(env.gpa, Ident.for_text("method")),
        .fn_var = method,
        .origin = .method_call,
    }});
    try env.types.setVarContent(fixture.private, .{ .flex = Flex.init().withConstraints(constraints) });
    var opening = try NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost);
    defer opening.deinit();
    // Demanding the visible formal must not force an unrelated private graph.
    const baseline = env.types.len();
    try std.testing.expectEqual(fixture.actual, try opening.demand(fixture.formal));
    try std.testing.expectEqual(baseline, env.types.len());
    _ = try opening.materialize();
    const private = try opening.demand(fixture.private);
    const copied_constraints = env.types.sliceStaticDispatchConstraints(env.types.resolveVar(private).desc.content.flex.constraints);
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len);
    const copied_method = env.types.resolveVar(copied_constraints[0].fn_var).desc.content.structure.fn_unbound;
    try std.testing.expectEqual(private, env.types.sliceVars(copied_method.args)[0]);
    try std.testing.expectEqual(private, copied_method.ret);
    const copied_effects = env.types.sliceVars(copied_method.effect_deps);
    try std.testing.expectEqual(@as(usize, 1), copied_effects.len);
    const copied_effect = env.types.resolveVar(copied_effects[0]).desc.content.structure.fn_effectful;
    try std.testing.expectEqual(fixture.actual, copied_effect.ret);
    try std.testing.expect(copied_constraints[0].fn_var != method);
    try std.testing.expect(copied_effects[0] != effect);
}

test "nominal opening - allocation failures release owned session state" {
    var fail_offset: usize = 0;
    while (true) : (fail_offset += 1) {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var env = try TestEnv.init(failing.allocator());
        defer env.deinit();
        const fixture = try nominalOpeningFixture(&env);
        // Only fail the new opening/demand operation, not fixture setup.
        failing.fail_index = failing.alloc_index + fail_offset;
        var opening = NominalOpening.init(&env.types, &env.idents, fixture.decl, &.{fixture.actual}, .outermost) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            continue;
        };
        defer opening.deinit();
        opening.recordRankHistory(fixture.private, .outermost) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            continue;
        };
        var checkpoint = opening.checkpoint() catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            continue;
        };
        defer checkpoint.deinit();
        var savepoint = try env.types.createSavepoint();
        _ = opening.demand(fixture.private) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectError(error.OutOfMemory, opening.materialize());
            env.types.rollbackToSavepoint(&savepoint);
            opening.restore(&checkpoint);
            failing.fail_index = std.math.maxInt(usize);
            _ = try opening.materialize();
            continue;
        };
        _ = opening.materialize() catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectError(error.OutOfMemory, opening.materialize());
            env.types.rollbackToSavepoint(&savepoint);
            opening.restore(&checkpoint);
            failing.fail_index = std.math.maxInt(usize);
            _ = try opening.materialize();
            continue;
        };
        env.types.commitSavepoint(&savepoint);
        try std.testing.expect(!failing.has_induced_failure);
        break;
    }
}

test "persistent nominal opening - existing maps and latent ranks roll back with the Store" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const declaration = try env.types.registerNominalDecl(fixture.decl);
    const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
    const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
    const actuals = try env.types.appendVars(&.{fixture.actual});
    const region = base.Region.from_raw_offsets(12, 34);
    const owner = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), region);
    try std.testing.expect(region.eq(env.types.nominal_rows.getOpening(owner).creationRegion()));
    const associated = env.types.readNominalReference(.{ .template = .{
        .opening = owner,
        .var_ = fixture.associated_formal,
    } });
    try std.testing.expectEqual(fixture.actual, associated.reference.owned);
    try env.types.nominal_rows.recordRank(env.gpa, owner, fixture.private, .outermost);
    var before = try env.types.nominal_rows.clone(env.gpa);
    defer before.deinit(env.gpa);
    const baseline = env.types.len();
    var saved = try env.types.createSavepoint();
    try env.types.nominal_rows.recordRank(env.gpa, owner, fixture.private, .generalized);
    _ = try env.types.demandNominalTemplate(&env.idents, owner, fixture.private);
    const backing = try env.types.demandNominalTemplate(&env.idents, owner, fixture.decl.backing);
    const residual = env.types.resolveVar(backing).desc.content.structure.tag_union.ext;
    const fragment = try env.types.nominal_rows.appendFragment(env.gpa, owner, &.{0}, residual);
    try std.testing.expect(env.types.nominal_rows.excludes(fragment, 0));
    try std.testing.expect(!env.types.nominal_rows.excludes(fragment, 1));
    env.types.rollbackToSavepoint(&saved);
    try std.testing.expectEqual(baseline, env.types.len());
    try std.testing.expect(env.types.nominal_rows.eql(&before));
    const latent = env.types.readNominalReference(.{ .template = .{ .opening = owner, .var_ = fixture.private } });
    try std.testing.expect(latent.reference == .template);
    try std.testing.expectEqual(types_mod.Rank.outermost, latent.desc.rank);
    const private = try env.types.demandNominalTemplate(&env.idents, owner, fixture.private);
    const second = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), region);
    try std.testing.expect(private != try env.types.demandNominalTemplate(&env.idents, second, fixture.private));
    try env.types.setNominalReferenceRank(.{ .template = .{ .opening = owner, .var_ = fixture.private } }, .generalized);
    _ = try env.types.demandNominalTemplate(&env.idents, owner, fixture.decl.backing);
    try std.testing.expectEqual(types_mod.Rank.generalized, env.types.resolveVar(private).desc.rank);
}

test "persistent nominal opening - failed capture leaves no partial ownership metadata" {
    var fail_offset: usize = 0;
    while (true) : (fail_offset += 1) {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var env = try TestEnv.init(failing.allocator());
        defer env.deinit();
        const fixture = try nominalOpeningFixture(&env);
        const declaration = try env.types.registerNominalDecl(fixture.decl);
        const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
        const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
        const actuals = try env.types.appendVars(&.{fixture.actual});
        var before = try env.types.nominal_rows.clone(std.testing.allocator);
        defer before.deinit(std.testing.allocator);
        const baseline = env.types.len();
        failing.fail_index = failing.alloc_index + fail_offset;
        _ = env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero()) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            try std.testing.expectEqual(baseline, env.types.len());
            try std.testing.expect(env.types.nominal_rows.eql(&before));
            failing.fail_index = std.math.maxInt(usize);
            _ = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
            continue;
        };
        try std.testing.expect(!failing.has_induced_failure);
        break;
    }
}

test "persistent nominal opening - serialized copies retain sparse sharing and later rank history" {
    const collections = @import("collections");
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const declaration = try env.types.registerNominalDecl(fixture.decl);
    const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
    const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
    const actuals = try env.types.appendVars(&.{fixture.actual});
    const owner = try env.types.createNominalOpening(schema, actuals, types_mod.Rank.outermost.next().next(), @enumFromInt(0), base.Region.zero());
    const foreign_schema: @import("../nominal_rows.zig").Schema.Idx = @enumFromInt(7);
    const foreign_private: Var = @enumFromInt(@intFromEnum(fixture.private) + 1000);
    _ = try env.types.nominal_rows.publishSchemaImport(env.gpa, @enumFromInt(0), foreign_schema, schema, &.{
        .{ .source = @enumFromInt(@intFromEnum(fixture.formal) + 1000), .local = fixture.formal },
        .{ .source = @enumFromInt(@intFromEnum(fixture.associated_formal) + 1000), .local = fixture.associated_formal },
        .{ .source = foreign_private, .local = fixture.private },
        .{ .source = @enumFromInt(@intFromEnum(fixture.decl.backing) + 1000), .local = fixture.decl.backing },
        .{ .source = @enumFromInt(@intFromEnum(row.ext) + 1000), .local = row.ext },
    });
    try env.types.nominal_rows.recordRank(env.gpa, owner, fixture.decl.backing, types_mod.Rank.outermost.next());
    const private = try env.types.demandNominalTemplate(&env.idents, owner, fixture.private);
    try env.types.setDescRank(env.types.resolveVar(private).desc_idx, .generalized);
    const residual = try env.types.demandNominalTemplate(&env.idents, owner, row.ext);
    const fragment = try env.types.nominal_rows.appendFragment(env.gpa, owner, &.{0}, residual);
    const baseline = env.types.len();
    var writer = collections.CompactWriter.init();
    defer writer.deinit(env.gpa);
    const header = try writer.appendAlloc(env.gpa, Store.Serialized);
    try header.serialize(&env.types, env.gpa, &writer);
    // Freezing sparse state must not demand the undemanded backing.
    try std.testing.expectEqual(baseline, env.types.len());
    const buffer = try env.gpa.alignedAlloc(u8, .@"16", writer.total_bytes);
    defer env.gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const frozen: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const CopyChecks = struct {
        fn clone(gpa: std.mem.Allocator, source: *const Store) !void {
            var result = try source.clone(gpa);
            defer result.deinit();
            try std.testing.expect(result.nominal_rows.eql(&source.nominal_rows));
        }

        fn thaw(gpa: std.mem.Allocator, source: *const Store, serialized: *const Store.Serialized, base_addr: usize) !void {
            var result = try serialized.deserializeWithCopy(base_addr, gpa);
            defer result.deinit();
            try std.testing.expect(result.nominal_rows.eql(&source.nominal_rows));
        }
    };
    try std.testing.checkAllAllocationFailures(env.gpa, CopyChecks.clone, .{&env.types});
    try std.testing.checkAllAllocationFailures(env.gpa, CopyChecks.thaw, .{ &env.types, frozen, @intFromPtr(buffer.ptr) });
    const view = frozen.deserializeInto(@intFromPtr(buffer.ptr), env.gpa);
    try std.testing.expect(view.nominal_rows.eql(&env.types.nominal_rows));
    var copied = try frozen.deserializeWithCopy(@intFromPtr(buffer.ptr), env.gpa);
    defer copied.deinit();
    try std.testing.expect(copied.nominal_rows.eql(&env.types.nominal_rows));
    try std.testing.expect(copied.nominal_rows.excludes(fragment, 0));
    const imported = copied.nominal_rows.schemaImport(@enumFromInt(0), foreign_schema).?;
    try std.testing.expectEqual(fixture.private, copied.nominal_rows.translateTemplate(imported, foreign_private));
    try std.testing.expectEqual(row.ext, copied.nominal_rows.translateTemplate(imported, @enumFromInt(@intFromEnum(row.ext) + 1000)));
    // The legacy relocated representation retains the same sparse tables.
    var relocated_writer = collections.CompactWriter.init();
    defer relocated_writer.deinit(env.gpa);
    _ = try env.types.serialize(env.gpa, &relocated_writer);
    const relocated_buffer = try env.gpa.alignedAlloc(u8, .@"16", relocated_writer.total_bytes);
    defer env.gpa.free(relocated_buffer);
    _ = try relocated_writer.writeToBuffer(relocated_buffer);
    const relocated: *Store = @ptrCast(@alignCast(relocated_buffer.ptr));
    relocated.relocate(@intCast(@intFromPtr(relocated_buffer.ptr)));
    try std.testing.expect(relocated.nominal_rows.eql(&env.types.nominal_rows));
    try std.testing.expectEqual(baseline, env.types.len());
    // The copy must remain usable after the original Store has disappeared.
    const replacement = try Store.init(env.gpa);
    env.types.deinit();
    env.types = replacement;
    const backing = try copied.demandNominalTemplate(&env.idents, owner, fixture.decl.backing);
    try std.testing.expectEqual(types_mod.Rank.outermost.next(), copied.resolveVar(backing).desc.rank);
    const copied_row = copied.resolveVar(backing).desc.content.structure.tag_union;
    for (copied.tags.sliceRange(copied_row.tags).items(.args)) |args_range| {
        const args = copied.sliceVars(args_range);
        try std.testing.expectEqual(fixture.actual, args[0]);
        try std.testing.expectEqual(private, args[1]);
    }
    try std.testing.expectEqual(types_mod.Rank.generalized, copied.resolveVar(private).desc.rank);
}

test "persistent nominal opening - failed demands invalidate and paired rollback restores stored state" {
    var fail_offset: usize = 0;
    while (true) : (fail_offset += 1) {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var env = try TestEnv.init(failing.allocator());
        defer env.deinit();
        const fixture = try nominalOpeningFixture(&env);
        const declaration = try env.types.registerNominalDecl(fixture.decl);
        const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
        const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
        const actuals = try env.types.appendVars(&.{fixture.actual});
        const owner = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
        var before = try env.types.nominal_rows.clone(std.testing.allocator);
        defer before.deinit(std.testing.allocator);
        const baseline = env.types.len();
        var saved = try env.types.createSavepoint();
        failing.fail_index = failing.alloc_index + fail_offset;
        _ = env.types.demandNominalTemplate(&env.idents, owner, fixture.decl.backing) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            if (env.types.nominal_rows.getOpening(owner).status == .failed) {
                try std.testing.expectError(error.OutOfMemory, env.types.demandNominalTemplate(&env.idents, owner, fixture.private));
            }
            env.types.rollbackToSavepoint(&saved);
            try std.testing.expectEqual(baseline, env.types.len());
            try std.testing.expect(env.types.nominal_rows.eql(&before));
            failing.fail_index = std.math.maxInt(usize);
            _ = try env.types.demandNominalTemplate(&env.idents, owner, fixture.decl.backing);
            continue;
        };
        env.types.commitSavepoint(&saved);
        try std.testing.expect(!failing.has_induced_failure);
        break;
    }
}

test "persistent nominal opening - frozen tables ignore spare capacity and destination poison" {
    const collections = @import("collections");
    const Rows = @import("../nominal_rows.zig");
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const declaration = try env.types.registerNominalDecl(fixture.decl);
    const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
    const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
    const actuals = try env.types.appendVars(&.{fixture.actual});
    const owner = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
    _ = try env.types.demandNominalTemplate(&env.idents, owner, fixture.private);
    try env.types.nominal_rows.recordRank(env.gpa, owner, fixture.decl.backing, .generalized);
    _ = try env.types.nominal_rows.appendFragment(env.gpa, owner, &.{0}, row.ext);
    _ = try env.types.nominal_rows.publishSchemaImport(env.gpa, @enumFromInt(1), @enumFromInt(2), schema, &.{
        .{ .source = @enumFromInt(1000), .local = fixture.private },
    });
    var oversized = try env.types.nominal_rows.clone(env.gpa);
    defer oversized.deinit(env.gpa);
    inline for (.{ "schemas", "openings", "fragments", "bindings", "names", "ranks", "exclusions", "schema_imports", "template_translations" }) |field| {
        const list = &@field(oversized, field).items;
        try list.ensureTotalCapacity(env.gpa, list.items.len + 67);
        @memset(std.mem.sliceAsBytes(list.items.ptr[list.items.len..list.capacity]), 0xA7);
    }
    const Freeze = struct {
        fn bytes(gpa: std.mem.Allocator, tables: *const Rows.Tables, poison: u8) ![]align(16) u8 {
            var writer = collections.CompactWriter.init();
            defer writer.deinit(gpa);
            const header = try writer.appendAlloc(gpa, Rows.Tables.Serialized);
            @memset(std.mem.asBytes(header), poison);
            try header.serialize(tables, gpa, &writer);
            const buffer = try gpa.alignedAlloc(u8, .@"16", writer.total_bytes);
            errdefer gpa.free(buffer);
            @memset(buffer, poison);
            _ = try writer.writeToBuffer(buffer);
            return buffer;
        }
    };
    const first = try Freeze.bytes(env.gpa, &env.types.nominal_rows, 0xB3);
    defer env.gpa.free(first);
    const second = try Freeze.bytes(env.gpa, &oversized, 0xA7);
    defer env.gpa.free(second);
    try std.testing.expectEqualSlices(u8, first, second);
}

test "persistent nominal opening - restored sparse map retains recursive constraints and effects" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const fixture = try nominalOpeningFixture(&env);
    const effect = try env.types.freshFromContentWithRank(.{ .structure = .{ .fn_effectful = .{
        .args = try env.types.appendVars(&.{}),
        .ret = fixture.formal,
    } } }, .generalized);
    const method = try env.types.freshFromContentWithRank(.{ .structure = .{ .fn_unbound = .{
        .args = try env.types.appendVars(&.{fixture.private}),
        .ret = fixture.private,
        .effect_deps = try env.types.appendVars(&.{effect}),
    } } }, .generalized);
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.idents.insert(env.gpa, Ident.for_text("method")),
        .fn_var = method,
        .origin = .method_call,
    }});
    try env.types.setVarContent(fixture.private, .{ .flex = Flex.init().withConstraints(constraints) });
    const declaration = try env.types.registerNominalDecl(fixture.decl);
    const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
    const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
    const actuals = try env.types.appendVars(&.{fixture.actual});
    const owner = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
    const baseline = env.types.len();
    try std.testing.expectEqual(fixture.actual, try env.types.demandNominalTemplate(&env.idents, owner, fixture.formal));
    try std.testing.expectEqual(baseline, env.types.len());
    const private = try env.types.demandNominalTemplate(&env.idents, owner, fixture.private);
    var copied = try env.types.clone(env.gpa);
    defer copied.deinit();
    // The next session is reconstructed from persisted map entries, not the
    // instantiator that allocated the recursive constraint graph.
    const backing = try copied.demandNominalTemplate(&env.idents, owner, fixture.decl.backing);
    const copied_row = copied.resolveVar(backing).desc.content.structure.tag_union;
    for (copied.tags.sliceRange(copied_row.tags).items(.args)) |args| {
        try std.testing.expectEqual(private, copied.sliceVars(args)[1]);
    }
    const copied_constraints = copied.sliceStaticDispatchConstraints(copied.resolveVar(private).desc.content.flex.constraints);
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len);
    const copied_method = copied.resolveVar(copied_constraints[0].fn_var).desc.content.structure.fn_unbound;
    try std.testing.expectEqual(private, copied.sliceVars(copied_method.args)[0]);
    try std.testing.expectEqual(private, copied_method.ret);
    const copied_effects = copied.sliceVars(copied_method.effect_deps);
    try std.testing.expectEqual(@as(usize, 1), copied_effects.len);
    try std.testing.expectEqual(fixture.actual, copied.resolveVar(copied_effects[0]).desc.content.structure.fn_effectful.ret);
    const second = try copied.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
    try std.testing.expect(private != try copied.demandNominalTemplate(&env.idents, second, fixture.private));
}

test "persistent nominal opening - table publication and rank writes are failure atomic" {
    const Mutation = enum { schema_import, fragment, rank };
    inline for (std.meta.tags(Mutation)) |mutation| {
        var fail_offset: usize = 0;
        while (true) : (fail_offset += 1) {
            var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
            var env = try TestEnv.init(failing.allocator());
            defer env.deinit();
            const fixture = try nominalOpeningFixture(&env);
            const declaration = try env.types.registerNominalDecl(fixture.decl);
            const row = env.types.resolveVar(fixture.decl.backing).desc.content.structure.tag_union;
            const schema = try env.types.registerNominalRowSchema(declaration, row.tags, row.ext);
            const actuals = try env.types.appendVars(&.{fixture.actual});
            const owner = try env.types.createNominalOpening(schema, actuals, .outermost, @enumFromInt(0), base.Region.zero());
            var before = try env.types.nominal_rows.clone(std.testing.allocator);
            defer before.deinit(std.testing.allocator);
            var saved = try env.types.createSavepoint();
            failing.fail_index = failing.alloc_index + fail_offset;
            const result: std.mem.Allocator.Error!void = switch (mutation) {
                .schema_import => blk: {
                    _ = env.types.nominal_rows.publishSchemaImport(env.gpa, @enumFromInt(1), @enumFromInt(9), schema, &.{
                        .{ .source = @enumFromInt(1000), .local = fixture.formal },
                        .{ .source = @enumFromInt(1001), .local = fixture.associated_formal },
                        .{ .source = @enumFromInt(1002), .local = fixture.private },
                        .{ .source = @enumFromInt(1003), .local = fixture.decl.backing },
                        .{ .source = @enumFromInt(1004), .local = row.ext },
                    }) catch |err| break :blk err;
                    break :blk;
                },
                .fragment => blk: {
                    _ = env.types.nominal_rows.appendFragment(env.gpa, owner, &.{ 0, 1 }, fixture.actual) catch |err| break :blk err;
                    break :blk;
                },
                .rank => env.types.nominal_rows.recordRank(env.gpa, owner, fixture.private, .generalized),
            };
            result catch |err| {
                try std.testing.expectEqual(error.OutOfMemory, err);
                try std.testing.expect(failing.has_induced_failure);
                // A failed publication has not exposed any partial table entry
                // or changed an existing opening's visible history.
                try std.testing.expect(env.types.nominal_rows.eql(&before));
                env.types.rollbackToSavepoint(&saved);
                try std.testing.expect(env.types.nominal_rows.eql(&before));
                continue;
            };
            env.types.rollbackToSavepoint(&saved);
            try std.testing.expect(env.types.nominal_rows.eql(&before));
            try std.testing.expect(!failing.has_induced_failure);
            break;
        }
    }
}

test "instantiate - generalized flex var creates new flex var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should still be flex
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .flex);
}

test "instantiate - non-generalized flex var DOES NOT create new flex var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .outermost);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated == original);
}

test "instantiate - generalized rigid var with fresh_flex creates flex var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should now be flex
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .flex);
}

test "instantiate - generalized rigid var with fresh_rigid creates new rigid var" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const original = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_rigid,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    // Should be a different variable
    try std.testing.expect(instantiated != original);

    // Should still be rigid
    const resolved = env.types.resolveVar(instantiated);
    try std.testing.expect(resolved.desc.content == .rigid);
}

test "instantiate - preserves generalized rigid var structure in function" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a -> a function
    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const func_content = try env.mkFuncPure(&[_]Var{rigid_a}, rigid_a);
    const original = try env.types.freshFromContentWithRank(func_content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
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

test "expected shape preserves sharing without copying dispatch-only graphs" {
    const gpa = std.testing.allocator;
    inline for (.{ false, true }) |is_rigid| {
        var env = try TestEnv.init(gpa);
        defer env.deinit();

        const name = try env.idents.insert(gpa, Ident.for_text("a"));
        const method_name = try env.idents.insert(gpa, Ident.for_text("method"));
        const method_arg = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
        const method = try env.types.freshFromContentWithRank(try env.mkFuncPure(&.{method_arg}, method_arg), .generalized);
        const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
            .fn_name = method_name,
            .fn_var = method,
            .origin = .method_call,
        }});
        const source = try env.types.freshFromContentWithRank(if (is_rigid)
            .{ .rigid = .{ .name = name, .constraints = constraints } }
        else
            .{ .flex = .{ .name = name, .constraints = constraints } }, .generalized);
        const root = try env.types.freshFromContentWithRank(try env.mkFuncPure(&.{source}, source), .generalized);

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
            .rank_behavior = .ignore_rank,
            .current_rank = .outermost,
            .purpose = .expected_shape,
        };
        const shape = try instantiator.instantiateVar(root);
        const func = env.types.resolveVar(shape).desc.content.structure.fn_pure;
        try std.testing.expectEqual(env.types.getVarAt(func.args, 0), func.ret);
        try std.testing.expect(func.ret != source);
        try std.testing.expectEqual(@as(usize, 0), env.types.resolveVar(func.ret).desc.content.flex.constraints.len());
        try std.testing.expect(!env.var_map.contains(method));
        try std.testing.expect(!env.var_map.contains(method_arg));
        const original = env.types.resolveVar(source).desc.content;
        try std.testing.expectEqual(constraints, if (is_rigid) original.rigid.constraints else original.flex.constraints);
    }
}

test "instantiate - func with some generalized and some not preserve non-generalized" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    // Create a, b -> a function
    const var_a = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    const var_b = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .outermost);
    const var_fn = try env.types.freshFromContentWithRank(try env.mkFuncPure(&[_]Var{ var_a, var_b }, var_a), .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const var_fn_inst = try instantiator.instantiateVar(var_fn);

    // Should be a different function
    try std.testing.expect(var_fn_inst != var_fn);

    // Get the function structure
    const resolved = env.types.resolveVar(var_fn_inst);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .fn_pure);
    const fn_inst = resolved.desc.content.structure.fn_pure;

    const args = env.types.sliceVars(fn_inst.args);
    try std.testing.expectEqual(2, args.len);

    const var_a_inst = args[0];
    const var_b_inst = args[1];

    // The arg and return should be the SAME new flex var
    try std.testing.expect(var_a != var_a_inst);
    try std.testing.expectEqual(var_a_inst, fn_inst.ret);

    // Since var_b was NOT generalized, it should be the same after instantiation
    try std.testing.expectEqual(var_b, var_b_inst);
}

test "instantiate type scheme copies a monomorphic structural spine" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const quantified = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    const monomorphic = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .outermost);
    const inner = try env.types.freshFromContentWithRank(
        try env.mkFuncPure(&.{quantified}, quantified),
        .outermost,
    );
    const scheme = try env.types.freshFromContentWithRank(
        try env.mkFuncPure(&.{monomorphic}, inner),
        .outermost,
    );

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };
    const instantiated = try instantiator.instantiateTypeScheme(scheme);

    const outer_fn = env.types.resolveVar(instantiated).desc.content.structure.fn_pure;
    const outer_args = env.types.sliceVars(outer_fn.args);
    try std.testing.expectEqual(monomorphic, outer_args[0]);
    try std.testing.expect(inner != outer_fn.ret);

    const inner_fn = env.types.resolveVar(outer_fn.ret).desc.content.structure.fn_pure;
    const inner_args = env.types.sliceVars(inner_fn.args);
    try std.testing.expect(quantified != inner_args[0]);
    try std.testing.expectEqual(inner_args[0], inner_fn.ret);
}

test "instantiate - tuple with multiple vars" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const rigid_b = try env.types.freshFromContentWithRank(try env.mkRigidVar("b"), .generalized);
    const tuple_content = try env.mkTuple(&[_]Var{ rigid_a, rigid_b, rigid_a });
    const original = try env.types.freshFromContentWithRank(tuple_content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
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

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const rigid_b = try env.types.freshFromContentWithRank(try env.mkRigidVar("b"), .generalized);

    const record_info = try env.mkRecordClosed(&[_]RecordField{
        try env.mkRecordField("x", rigid_a),
        try env.mkRecordField("y", rigid_b),
        try env.mkRecordField("z", rigid_a),
    });
    const original = try env.types.freshFromContentWithRank(record_info.content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const record = resolved.desc.content.structure.record;
    const fields = env.types.getRecordFieldsSlice(record.fields);

    try std.testing.expectEqual(3, fields.len);

    const x_var = fields.items(.presence)[0].typeVar();
    const y_var = fields.items(.presence)[1].typeVar();
    const z_var = fields.items(.presence)[2].typeVar();

    // x and z should be the same (both were rigid_a)
    try std.testing.expectEqual(x_var, z_var);
    // y should be different (was rigid_b)
    try std.testing.expect(x_var != y_var);
}

test "instantiate - tag union preserves structure" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);

    const tag_union_info = try env.mkTagUnionClosed(&[_]Tag{
        try env.mkTag("Some", &[_]Var{rigid_a}),
        try env.mkTag("None", &[_]Var{}),
    });
    const original = try env.types.freshFromContentWithRank(tag_union_info.content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const instantiated = try instantiator.instantiateVar(original);

    const resolved = env.types.resolveVar(instantiated);
    const tag_union = resolved.desc.content.structure.tag_union;
    const tags = env.types.getTagsSlice(tag_union.tags);

    try std.testing.expectEqual(2, tags.len);

    // Tags are sorted alphabetically by name after instantiation.
    // Find tags by name rather than assuming order.
    const tag_names = tags.items(.name);
    const tag_args = tags.items(.args);

    var some_idx: ?usize = null;
    var none_idx: ?usize = null;
    for (tag_names, 0..) |name, i| {
        const name_str = env.idents.getText(name);
        if (std.mem.eql(u8, name_str, "Some")) some_idx = i;
        if (std.mem.eql(u8, name_str, "None")) none_idx = i;
    }

    // Check Some tag has one arg that's different from original
    const some_args = env.types.sliceVars(tag_args[some_idx.?]);
    try std.testing.expectEqual(1, some_args.len);
    try std.testing.expect(some_args[0] != rigid_a);

    // Check None tag has no args
    const none_args = env.types.sliceVars(tag_args[none_idx.?]);
    try std.testing.expectEqual(0, none_args.len);
}

test "instantiate - alias preserves structure" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const list_ident_idx = try env.idents.insert(gpa, .for_text("List"));
    const builtin_module_idx = base.ModuleIdentity.Idx.NONE;
    const backing_content = try env.types.mkNominal(
        .{ .ident_idx = list_ident_idx },
        &[_]Var{rigid_a},
        builtin_module_idx,
        false,
    );
    const backing = try env.types.freshFromContentWithRank(backing_content, .generalized);
    const alias_content = try env.mkAlias("MyList", backing, &[_]Var{rigid_a}, builtin_module_idx);
    const original = try env.types.freshFromContentWithRank(alias_content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
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
    const backing_nominal = backing_resolved.desc.content.structure.nominal_type;
    const backing_list_args = env.types.sliceNominalArgs(backing_nominal);
    const backing_list_elem = backing_list_args[0];

    // The alias arg and the list element should be the same fresh var
    try std.testing.expectEqual(args[0], backing_list_elem);
}

test "instantiate - nominal type application instantiates its args" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);

    // Test Box a
    {
        const box_ident_idx = try env.idents.insert(gpa, .for_text("Box"));
        const builtin_module_idx = base.ModuleIdentity.Idx.NONE;
        const box_content = try env.types.mkNominal(
            .{ .ident_idx = box_ident_idx },
            &[_]Var{rigid_a},
            builtin_module_idx,
            false,
        );
        const box_var = try env.types.freshFromContentWithRank(box_content, .generalized);

        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
            .current_rank = .outermost,
        };

        const instantiated = try instantiator.instantiateVar(box_var);
        const resolved = env.types.resolveVar(instantiated);

        try std.testing.expect(resolved.desc.content.structure == .nominal_type);
        const nominal = resolved.desc.content.structure.nominal_type;
        const args = env.types.sliceNominalArgs(nominal);
        try std.testing.expect(args.len == 1);
        try std.testing.expect(args[0] != rigid_a);
    }
}

test "instantiate - multiple instantiations are independent" {
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = try env.types.freshFromContentWithRank(try env.mkRigidVar("a"), .generalized);
    const func_content = try env.mkFuncPure(&[_]Var{rigid_a}, rigid_a);
    const original = try env.types.freshFromContentWithRank(func_content, .generalized);

    // First instantiation
    var var_map1 = @import("collections").DenseMap(Var, Var).init(gpa);
    defer var_map1.deinit();

    var instantiator1 = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &var_map1,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };

    const inst1 = try instantiator1.instantiateVar(original);

    // Second instantiation
    var var_map2 = @import("collections").DenseMap(Var, Var).init(gpa);
    defer var_map2.deinit();

    var instantiator2 = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &var_map2,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
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
    var_map: @import("collections").DenseMap(Var, Var),

    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .types = try Store.initCapacity(gpa, 16, 8),
            .idents = try Ident.Store.initCapacity(gpa, 16),
            .var_map = @import("collections").DenseMap(Var, Var).init(gpa),
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

    fn mkAlias(self: *Self, name: []const u8, backing_var: Var, args: []const Var, module_idx: base.ModuleIdentity.Idx) std.mem.Allocator.Error!Content {
        return try self.types.mkAlias(try self.mkTypeIdent(name), backing_var, args, module_idx);
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
        return RecordField{ .name = ident_idx, .presence = .required(var_) };
    }

    const RecordInfo = struct { record: Record, content: Content };

    fn mkRecord(self: *Self, fields: []const RecordField, ext_var: Var) std.mem.Allocator.Error!RecordInfo {
        const fields_range = try self.types.appendRecordFields(fields);
        const record = Record{ .fields = fields_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .record = record } }, .record = record };
    }

    fn mkRecordClosed(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordInfo {
        const ext_var = try self.types.freshFromContentWithRank(.{ .structure = .empty_record }, .outermost);
        return self.mkRecord(fields, ext_var);
    }

    // helpers - func //

    fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return try self.types.mkFuncPure(args, ret);
    }

    // helpers - tag union //

    const TagUnionInfo = struct { tag_union: TagUnion, content: Content };

    fn mkTag(self: *Self, name: []const u8, args: []const Var) std.mem.Allocator.Error!Tag {
        const ident_idx = try self.idents.insert(self.gpa, Ident.for_text(name));
        return Tag{ .name = ident_idx, .args = try self.types.appendVars(args) };
    }

    fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!TagUnionInfo {
        const tags_range = try self.types.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .tag_union = tag_union } }, .tag_union = tag_union };
    }

    fn mkTagUnionClosed(self: *Self, tags: []const Tag) std.mem.Allocator.Error!TagUnionInfo {
        const ext_var = try self.types.freshFromContentWithRank(.{ .structure = .empty_tag_union }, .outermost);
        return self.mkTagUnion(tags, ext_var);
    }
};

test "instantiate - annotation tag closure authority belongs to the definition" {
    var env = try TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const original = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    try env.types.markAnnotationTagExt(original);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
    };
    const use = try instantiator.instantiateVar(original);
    try std.testing.expect(!env.types.resolveVar(use).desc.flags.annotation_tag_ext);
    try std.testing.expect(env.types.resolveVar(original).desc.flags.annotation_tag_ext);

    env.var_map.clearRetainingCapacity();
    instantiator.preserve_annotation_tag_ext = true;
    const faithful_copy = try instantiator.instantiateVar(original);
    try std.testing.expect(faithful_copy != original);
    try std.testing.expect(env.types.resolveVar(faithful_copy).desc.flags.annotation_tag_ext);
}

test "instantiate - rejected nominal positions produce error and unwind partial copies" {
    const Provider = struct {
        fn position(_: *anyopaque, _: types_mod.NominalType, index: u32, _: types_mod.Polarity) std.mem.Allocator.Error!?types_mod.Polarity {
            return if (index == 1) null else .pos;
        }
    };
    for ([_]Instantiator.PolarityVarBehavior{ .close, .preserve, .resolve_by_polarity }) |behavior| {
        const gpa = std.testing.allocator;
        var env = try TestEnv.init(gpa);
        defer env.deinit();
        const first = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
        const second = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
        const content = try env.types.mkNominal(try env.mkTypeIdent("Rejected"), &.{ first, second }, .NONE, false);
        const original = try env.types.freshFromContentWithRank(content, .generalized);
        var instantiator = Instantiator{
            .store = &env.types,
            .idents = &env.idents,
            .var_map = &env.var_map,
            .rigid_behavior = .fresh_flex,
            .current_rank = .outermost,
            .polarity_var_behavior = behavior,
            .polarity_var_ident = try env.idents.insert(gpa, .for_text(types_mod.polarity_var_text)),
            .nominal_argument_position = .{ .context = &env, .resolve = Provider.position },
        };
        const result = try instantiator.instantiateVar(original);
        try std.testing.expect(env.types.resolveVar(result).desc.content == .err);
        try std.testing.expectEqualDeep(content, env.types.resolveVar(original).desc.content);
        const independent = try instantiator.instantiateVar(first);
        try std.testing.expect(env.types.resolveVar(independent).desc.content == .flex);
    }
}

test "instantiate - markers in a constraint signature get their own choices" {
    // The copy descends into a flex's static-dispatch constraints, so their
    // markers are occurrences the choice walk decides: the signature's
    // result opens and its argument closes.
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();
    const marker_ident = try env.idents.insert(gpa, .for_text(types_mod.polarity_var_text));
    const arg_marker = try env.types.freshFromContentWithRank(.{ .rigid = Rigid.init(marker_ident) }, .generalized);
    const ret_marker = try env.types.freshFromContentWithRank(.{ .rigid = Rigid.init(marker_ident) }, .generalized);
    const arg_union = try env.types.freshFromContentWithRank((try env.mkTagUnion(&.{try env.mkTag("B", &.{})}, arg_marker)).content, .generalized);
    const ret_union = try env.types.freshFromContentWithRank((try env.mkTagUnion(&.{try env.mkTag("A", &.{})}, ret_marker)).content, .generalized);
    const method = try env.types.freshFromContentWithRank(try env.mkFuncPure(&.{arg_union}, ret_union), .generalized);
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.idents.insert(gpa, Ident.for_text("method")),
        .fn_var = method,
        .origin = .method_call,
    }});
    const source = try env.types.freshFromContentWithRank(.{ .flex = .{ .name = null, .constraints = constraints } }, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
        .polarity_var_behavior = .resolve_by_polarity,
        .polarity_var_ident = marker_ident,
    };
    const copy = try instantiator.instantiateVar(source);
    const copied_constraints = env.types.resolveVar(copy).desc.content.flex.constraints;
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len());
    const copied_method = env.types.static_dispatch_constraints.items.items[@intFromEnum(copied_constraints.start)].fn_var;
    const func = env.types.resolveVar(copied_method).desc.content.structure.fn_pure;
    const copied_arg = env.types.resolveVar(env.types.getVarAt(func.args, 0)).desc.content.structure.tag_union;
    const copied_ret = env.types.resolveVar(func.ret).desc.content.structure.tag_union;
    try std.testing.expect(env.types.resolveVar(copied_arg.ext).desc.content == .structure);
    try std.testing.expect(env.types.resolveVar(copied_arg.ext).desc.content.structure == .empty_tag_union);
    try std.testing.expect(env.types.resolveVar(copied_ret.ext).desc.content == .flex);
}

test "instantiate - markers in interpolation metadata get their own choices" {
    // The copy descends into an interpolation constraint's parts and item
    // var, so their markers are occurrences the choice walk decides.
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();
    const marker_ident = try env.idents.insert(gpa, .for_text(types_mod.polarity_var_text));
    const part_marker = try env.types.freshFromContentWithRank(.{ .rigid = Rigid.init(marker_ident) }, .generalized);
    const item_marker = try env.types.freshFromContentWithRank(.{ .rigid = Rigid.init(marker_ident) }, .generalized);
    const part_union = try env.types.freshFromContentWithRank((try env.mkTagUnion(&.{try env.mkTag("P", &.{})}, part_marker)).content, .generalized);
    const item_union = try env.types.freshFromContentWithRank((try env.mkTagUnion(&.{try env.mkTag("I", &.{})}, item_marker)).content, .generalized);
    const method = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    const region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 1 } };
    const parts = try env.types.appendInterpolationParts(&.{.{ .var_ = part_union, .region = region }});
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.idents.insert(gpa, Ident.for_text("from_interpolation")),
        .fn_var = method,
        .origin = .method_call,
        .interpolation = .{
            .expr_region = .some(region),
            .item_var = item_union,
            .interpolated_parts = parts,
        },
    }});
    const source = try env.types.freshFromContentWithRank(.{ .flex = .{ .name = null, .constraints = constraints } }, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
        .polarity_var_behavior = .resolve_by_polarity,
        .polarity_var_ident = marker_ident,
    };
    const copy = try instantiator.instantiateVar(source);
    const copied_constraints = env.types.resolveVar(copy).desc.content.flex.constraints;
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len());
    const metadata = env.types.static_dispatch_constraints.items.items[@intFromEnum(copied_constraints.start)].interpolation;
    try std.testing.expect(metadata.isPresent());
    const copied_part = env.types.resolveVar(env.types.getInterpolationPartAt(metadata.interpolated_parts, 0).var_).desc.content.structure.tag_union;
    const copied_item = env.types.resolveVar(metadata.item_var).desc.content.structure.tag_union;
    try std.testing.expect(env.types.resolveVar(copied_part.ext).desc.content == .flex);
    try std.testing.expect(env.types.resolveVar(copied_item.ext).desc.content == .flex);
}

test "instantiate - markers reached through a record presence var get their own choices" {
    // The copy visits a kind-carrying field's presence var as well as its
    // type var; a marker in a constraint on that presence var is an
    // occurrence the choice walk decides.
    const gpa = std.testing.allocator;
    var env = try TestEnv.init(gpa);
    defer env.deinit();
    const marker_ident = try env.idents.insert(gpa, .for_text(types_mod.polarity_var_text));
    const ret_marker = try env.types.freshFromContentWithRank(.{ .rigid = Rigid.init(marker_ident) }, .generalized);
    const ret_union = try env.types.freshFromContentWithRank((try env.mkTagUnion(&.{try env.mkTag("A", &.{})}, ret_marker)).content, .generalized);
    const method = try env.types.freshFromContentWithRank(try env.mkFuncPure(&.{}, ret_union), .generalized);
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.idents.insert(gpa, Ident.for_text("method")),
        .fn_var = method,
        .origin = .method_call,
    }});
    const presence = try env.types.freshFromContentWithRank(.{ .flex = .{ .name = null, .constraints = constraints } }, .generalized);
    const field_type = try env.types.freshFromContentWithRank(.{ .flex = Flex.init() }, .generalized);
    const field = RecordField{ .name = try env.idents.insert(gpa, Ident.for_text("x")), .presence = .unknown(presence, field_type) };
    const source = try env.types.freshFromContentWithRank((try env.mkRecordClosed(&.{field})).content, .generalized);

    var instantiator = Instantiator{
        .store = &env.types,
        .idents = &env.idents,
        .var_map = &env.var_map,
        .rigid_behavior = .fresh_flex,
        .current_rank = .outermost,
        .polarity_var_behavior = .resolve_by_polarity,
        .polarity_var_ident = marker_ident,
    };
    const copy = try instantiator.instantiateVar(source);
    const record = env.types.resolveVar(copy).desc.content.structure.record;
    const copied_presence = env.types.getRecordFieldAt(record.fields, 0).presence.presenceVar().?;
    const copied_constraints = env.types.resolveVar(copied_presence).desc.content.flex.constraints;
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len());
    const copied_method = env.types.static_dispatch_constraints.items.items[@intFromEnum(copied_constraints.start)].fn_var;
    const copied_ret = env.types.resolveVar(env.types.resolveVar(copied_method).desc.content.structure.fn_pure.ret).desc.content.structure.tag_union;
    try std.testing.expect(env.types.resolveVar(copied_ret.ext).desc.content == .flex);
}
