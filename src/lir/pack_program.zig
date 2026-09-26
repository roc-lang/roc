//! Pack programs: one module's closed exports lowered as a program of their own.
//!
//! The object cache stores compiled specializations per module
//! (`projects/big/package-object-cache.md`, "Entry format and on-disk
//! layout"). A module's pack holds every specialization reachable from its
//! exports that no imported module's pack already holds, and its content is
//! a function of the module and its transitive imports alone. The roots of
//! that lowering are the module's exported procedures whose checked types are
//! closed: no type variables and no function types anywhere, including
//! nominal backings. Those are the procedures that instantiate the same way
//! in every program, so lowering them here produces exactly the code an app
//! would produce for them.
//!
//! The passes that use whole-program facts stay sound at the pack boundary
//! without special modes: tag reachability already treats every procedure
//! parameter as fully constructed (`seedBoundaries` in `tag_reachability.zig`)
//! and only narrows through the returns of procedures whose bodies it can see,
//! all of which are inside the pack; ARC solves ownership signatures to a
//! fixpoint over the pack's own procedures and the manifest records the
//! resulting signature of every root for the program that links the pack.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");

const checked = check.CheckedModule;
const CheckedPipeline = @import("checked_pipeline.zig");
const core = @import("lir_core");
const LIR = core.LIR;
const LirStore = core.LirStore;
const BodyClone = @import("body_clone.zig");

const Allocator = std.mem.Allocator;

/// The exported procedures of `artifact` whose checked types are closed, as
/// root requests in export order.
pub fn closedExportRoots(
    allocator: Allocator,
    artifact: *const checked.CheckedModuleArtifact,
) Allocator.Error![]checked.RootRequest {
    const exported = try checked.exportedProcedureRoots(allocator, artifact);
    defer allocator.free(exported);

    var roots = std.ArrayList(checked.RootRequest).empty;
    errdefer roots.deinit(allocator);
    const types = artifact.checked_types.view();
    for (exported) |request| {
        if (!try checkedTypeIsClosed(allocator, types, request.checked_type)) continue;
        var root = request;
        root.order = @intCast(roots.items.len);
        try roots.append(allocator, root);
    }
    return try roots.toOwnedSlice(allocator);
}

/// Whether a checked type mentions no type variable, no function type, and
/// no record field of undetermined kind anywhere, including through aliases,
/// nominal arguments, and nominal backings. A closed procedure type
/// instantiates identically in every program, which is what lets its
/// specialization be shared.
///
/// A nominal's backing is its declaration's template, written over the
/// declaration's formal parameters. Those parameters stand for the nominal's
/// arguments, which the walk checks on their own, so a formal parameter met
/// inside a backing is bound rather than free.
pub fn checkedTypeIsClosed(
    allocator: Allocator,
    types: checked.CheckedTypeStoreView,
    root: checked.CheckedTypeId,
) Allocator.Error!bool {
    var visited = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer visited.deinit();
    var bound_formals = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer bound_formals.deinit();
    var stack = std.ArrayList(checked.CheckedTypeId).empty;
    defer stack.deinit(allocator);
    // The root is the procedure's own function type; only function types
    // inside its arguments and result make the procedure open.
    switch (types.payload(root)) {
        .function => |func| {
            try stack.appendSlice(allocator, func.args);
            try stack.append(allocator, func.ret);
        },
        .pending,
        .err,
        .empty_record,
        .empty_tag_union,
        .flex,
        .rigid,
        .alias,
        .record,
        .tuple,
        .nominal,
        .tag_union,
        => try stack.append(allocator, root),
    }
    while (stack.pop()) |ty| {
        const gop = try visited.getOrPut(ty);
        if (gop.found_existing) continue;
        switch (types.payload(ty)) {
            .pending, .err, .empty_record, .empty_tag_union => {},
            .flex, .function => return false,
            .rigid => if (!bound_formals.contains(ty)) return false,
            .alias => |alias| {
                try stack.append(allocator, alias.backing);
                try stack.appendSlice(allocator, alias.args);
            },
            .record => |record| {
                for (record.fields) |field| {
                    // A field whose kind each use decides is not one type.
                    switch (field.kind.tag) {
                        .required, .optional, .defaulted => {},
                        .undetermined, .err => return false,
                    }
                    try stack.append(allocator, field.ty);
                }
                try stack.append(allocator, record.ext);
            },
            .tuple => |elems| try stack.appendSlice(allocator, elems),
            .nominal => |nominal| {
                try stack.appendSlice(allocator, nominal.args);
                // Declared fields name backing record fields or index into
                // `padding_field_types`; the backing below covers the named ones.
                try stack.appendSlice(allocator, nominal.padding_field_types);
                if (types.nominalBackingTemplateForPayload(nominal)) |backing| {
                    // A backing template exists only for a declared nominal.
                    const declaration = types.nominalDeclarationForPayload(nominal) orelse unreachable;
                    for (declaration.formalArgs(types)) |formal| try bound_formals.put(formal, {});
                    try stack.append(allocator, backing);
                }
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| try stack.appendSlice(allocator, tag.argsSlice(types));
                try stack.append(allocator, tag_union.ext);
            },
        }
    }
    return true;
}

/// Lower `artifact`'s closed exports as a program. `imports` and
/// `relations` are the module views the checked pipeline needs for the
/// artifact as a root, exactly as for an app.
pub fn lowerPackProgram(
    allocator: Allocator,
    artifact: *const checked.CheckedModuleArtifact,
    imports: []const checked.ImportedModuleView,
    relations: []const checked.ImportedModuleView,
    roots: []const checked.RootRequest,
    target: CheckedPipeline.TargetConfig,
) CheckedPipeline.LowerResourceError!CheckedPipeline.LoweredProgram {
    var pack_target = target;
    pack_target.keep_specialization_procs = true;
    return CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = checked.loweringViewWithRelations(artifact, relations),
            .imports = imports,
        },
        .{
            .requests = roots,
            .include_internal_static_data = true,
        },
        pack_target,
    );
}

/// The pack's manifest: one line per root procedure with its content symbol
/// and the ownership signature ARC solved for it, in root order.
pub fn manifestBytes(
    allocator: Allocator,
    lowered: *const CheckedPipeline.LoweredProgram,
) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    const store = &lowered.lir_result.store;
    const procs = store.getProcSpecs();
    for (lowered.lir_result.root_procs.items) |proc_id| {
        const proc = procs[@intFromEnum(proc_id)];
        const symbol = try proc.identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "root {s} borrowed_params={x} {s}\n", .{ symbol, proc.rc_borrowed_params, store.procDebugName(proc_id) orelse "" });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    for (lowered.lir_result.spec_procs.items) |spec_proc| {
        const symbol = try procs[@intFromEnum(spec_proc.proc)].identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "spec {s} {s}\n", .{ &std.fmt.bytesToHex(spec_proc.key, .lower), symbol });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    for (procs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        const symbol = try proc.identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "proc {s} {s}\n", .{ symbol, store.procDebugName(@enumFromInt(@as(u32, @intCast(index)))) orelse "" });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    return try bytes.toOwnedSlice(allocator);
}

fn testTypeId(index: usize) checked.CheckedTypeId {
    return @enumFromInt(index);
}

test "closed export types reject records with undetermined field kinds" {
    const allocator = std.testing.allocator;
    const leaf = testTypeId(0);
    const undetermined = testTypeId(1);
    const required = testTypeId(2);
    const label: check.CanonicalNames.RecordFieldLabelId = @enumFromInt(7);
    const fields = [_]checked.CheckedRecordField{
        .{ .name = label, .ty = leaf, .kind = .undetermined(leaf) },
        .{ .name = label, .ty = leaf, .kind = .required },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .empty_record,
        .{ .record = .{ .fields = .{ .start = 0, .len = 1 }, .ext = leaf } },
        .{ .record = .{ .fields = .{ .start = 1, .len = 1 }, .ext = leaf } },
    };
    const types = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .record_field_pool = &fields,
    };

    try std.testing.expect(!try checkedTypeIsClosed(allocator, types, undetermined));
    try std.testing.expect(try checkedTypeIsClosed(allocator, types, required));
}

fn testNominalDeclarationId(index: usize) checked.CheckedNominalDeclarationId {
    return @enumFromInt(index);
}

fn testModuleIdentity(index: usize) check.CanonicalNames.ModuleIdentityId {
    return @enumFromInt(index);
}

fn testTypeName(index: usize) check.CanonicalNames.TypeNameId {
    return @enumFromInt(index);
}

fn testTagLabel(index: usize) check.CanonicalNames.TagLabelId {
    return @enumFromInt(index);
}

test "closed export types bind a nominal backing's formal parameters to its arguments" {
    const allocator = std.testing.allocator;
    // `Wrap(a) := [Some(a)]` declared over the rigid formal `a`, applied to
    // `U8`-like leaf `unit` in one type and to a free rigid `b` in the other.
    const unit = testTypeId(0);
    const formal = testTypeId(1);
    const backing = testTypeId(2);
    const wrap_unit = testTypeId(3);
    const free_rigid = testTypeId(4);
    const wrap_free = testTypeId(5);
    const declaration_id = testNominalDeclarationId(0);
    const nominal_key = check.CanonicalNames.NominalTypeKey{ .module = testModuleIdentity(0), .type_name = testTypeName(0) };
    const stored_nominal = checked.StoredNominal{
        .name = nominal_key.type_name,
        .origin_module = nominal_key.module,
        .owner_module = .{},
        .is_opaque = false,
        .representation = .{ .local_declaration = declaration_id },
        .args = .{ .start = 1, .len = 1 },
    };
    var stored_nominal_free = stored_nominal;
    stored_nominal_free.args = .{ .start = 2, .len = 1 };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .empty_record,
        .{ .rigid = .{} },
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = 1 }, .ext = unit } },
        .{ .nominal = stored_nominal },
        .{ .rigid = .{} },
        .{ .nominal = stored_nominal_free },
    };
    // Pool slots: [0] the formal as the tag's argument and the declaration's
    // formal list, [1] `unit` as the closed argument, [2] the free rigid.
    const type_id_pool = [_]checked.CheckedTypeId{ formal, unit, free_rigid };
    const tags = [_]checked.CheckedTag{.{ .name = testTagLabel(0), .args_start = 0, .args_len = 1 }};
    const declarations = [_]checked.CheckedNominalDeclaration{.{
        .id = declaration_id,
        .nominal = nominal_key,
        .source_statement = 0,
        .declaration_root = wrap_unit,
        .backing = backing,
        .fa_start = 0,
        .fa_len = 1,
    }};
    const types = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .nominal_declarations = &declarations,
        .type_id_pool = &type_id_pool,
        .tag_pool = &tags,
    };

    try std.testing.expect(try checkedTypeIsClosed(allocator, types, wrap_unit));
    try std.testing.expect(!try checkedTypeIsClosed(allocator, types, wrap_free));
    try std.testing.expect(!try checkedTypeIsClosed(allocator, types, backing));
}

/// Which procedures convert a specialized custom literal when they run, by
/// procedure id: a crash carrying a literal's rejection site is the `Err` arm
/// of that conversion. A program that evaluated its literal roots at compile
/// time reads their completed values instead, so only a program lowered
/// without evaluating them runs one. An object-cache entry must not carry
/// such a procedure: a program served by it would skip the compile-time
/// evaluation, and the diagnostics, of every literal root its own lowering
/// registers. Caller owns the result.
pub fn literalConvertingProcs(allocator: Allocator, store: *const LirStore) Allocator.Error![]bool {
    const converts = try allocator.alloc(bool, store.procSpecCount());
    errdefer allocator.free(converts);
    @memset(converts, false);
    const seen = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen);
    var visited = std.ArrayList(LIR.CFStmtId).empty;
    defer visited.deinit(allocator);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    @memset(seen, false);
    for (converts, 0..) |*result, index| {
        for (visited.items) |stmt_id| seen[@intFromEnum(stmt_id)] = false;
        visited.clearRetainingCapacity();
        work.clearRetainingCapacity();
        const body = store.getProcSpec(@enumFromInt(index)).body orelse continue;
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if (seen[@intFromEnum(stmt_id)]) continue;
            seen[@intFromEnum(stmt_id)] = true;
            try visited.append(allocator, stmt_id);
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .crash and stmt.crash.literal_rejection != null) {
                result.* = true;
                break;
            }
            try BodyClone.appendSuccessorsWithAllocator(store, &work, stmt_id, allocator);
        }
    }
    return converts;
}
