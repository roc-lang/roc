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

/// Whether a checked type mentions no type variable and no function type
/// anywhere, including through aliases, nominal arguments, and nominal
/// backings. A closed procedure type instantiates identically in every
/// program, which is what lets its specialization be shared.
pub fn checkedTypeIsClosed(
    allocator: Allocator,
    types: checked.CheckedTypeStoreView,
    root: checked.CheckedTypeId,
) Allocator.Error!bool {
    var visited = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer visited.deinit();
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
        .record_unbound,
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
            .flex, .rigid, .record_unbound, .function => return false,
            .alias => |alias| {
                try stack.append(allocator, alias.backing);
                try stack.appendSlice(allocator, alias.args);
            },
            .record => |record| {
                for (record.fields) |field| try stack.append(allocator, field.ty);
                try stack.append(allocator, record.ext);
            },
            .tuple => |elems| try stack.appendSlice(allocator, elems),
            .nominal => |nominal| {
                try stack.appendSlice(allocator, nominal.args);
                // Declared fields name backing record fields or index into
                // `padding_field_types`; the backing below covers the named ones.
                try stack.appendSlice(allocator, nominal.padding_field_types);
                if (types.nominalBackingTemplateForPayload(nominal)) |backing| try stack.append(allocator, backing);
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
        target,
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
