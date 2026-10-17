//! Pack programs: one module's closed exports lowered as a program of their own.
//!
//! The object cache stores compiled specializations per module
//! (https://github.com/roc-lang/roc/issues/12167, "Entry format and on-disk
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

/// Whether a checked type mentions no type variable, no function type
/// outside a `Box`, and no record field of undetermined kind anywhere,
/// including through aliases, nominal arguments, and nominal backings. A
/// closed procedure type instantiates identically in every program, which is
/// what lets its specialization be shared. Every function a `Box` holds is an
/// erased callable (design.md "Erased Callable Requirements"): one
/// pointer-sized value called through one convention whichever closure the
/// program puts in it, so it does not tie the procedure's code to the
/// program's lambda sets. A type variable inside a box still does.
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
    const Item = struct { ty: checked.CheckedTypeId, boxed: bool };
    // A type met both inside and outside a box is walked once in each.
    var visited = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer visited.deinit();
    var visited_boxed = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer visited_boxed.deinit();
    var bound_formals = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
    defer bound_formals.deinit();
    var stack = std.ArrayList(Item).empty;
    defer stack.deinit(allocator);
    const push = struct {
        fn many(gpa: Allocator, items: *std.ArrayList(Item), tys: []const checked.CheckedTypeId, boxed: bool) Allocator.Error!void {
            for (tys) |ty| try items.append(gpa, .{ .ty = ty, .boxed = boxed });
        }
    }.many;
    // The root is the procedure's own function type; only function types
    // inside its arguments and result make the procedure open.
    switch (types.payload(root)) {
        .function => |func| {
            try push(allocator, &stack, func.args, false);
            try stack.append(allocator, .{ .ty = func.ret, .boxed = false });
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
        => try stack.append(allocator, .{ .ty = root, .boxed = false }),
    }
    while (stack.pop()) |item| {
        const ty = item.ty;
        const boxed = item.boxed;
        const gop = try (if (boxed) &visited_boxed else &visited).getOrPut(ty);
        if (gop.found_existing) continue;
        switch (types.payload(ty)) {
            .pending, .err, .empty_record, .empty_tag_union => {},
            .flex => return false,
            .function => |func| {
                if (!boxed) return false;
                try push(allocator, &stack, func.args, true);
                try stack.append(allocator, .{ .ty = func.ret, .boxed = true });
            },
            .rigid => if (!bound_formals.contains(ty)) return false,
            .alias => |alias| {
                try stack.append(allocator, .{ .ty = alias.backing, .boxed = boxed });
                try push(allocator, &stack, alias.args, boxed);
            },
            .record => |record| {
                for (record.fields) |field| {
                    // A field whose kind each use decides is not one type.
                    switch (field.kind.tag) {
                        .required, .optional, .defaulted => {},
                        .undetermined, .err => return false,
                    }
                    try stack.append(allocator, .{ .ty = field.ty, .boxed = boxed });
                }
                try stack.append(allocator, .{ .ty = record.ext, .boxed = boxed });
            },
            .tuple => |elems| try push(allocator, &stack, elems, boxed),
            .nominal => |nominal| {
                try push(allocator, &stack, nominal.args, boxed or nominal.builtin == .box);
                // Declared fields name backing record fields or index into
                // `padding_field_types`; the backing below covers the named ones.
                try push(allocator, &stack, nominal.padding_field_types, boxed);
                if (types.nominalBackingTemplateForPayload(nominal)) |backing| {
                    // A backing template exists only for a declared nominal.
                    const declaration = types.nominalDeclarationForPayload(nominal) orelse unreachable;
                    for (declaration.formalArgs(types)) |formal| try bound_formals.put(formal, {});
                    try stack.append(allocator, .{ .ty = backing, .boxed = boxed });
                }
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| try push(allocator, &stack, tag.argsSlice(types), boxed);
                try stack.append(allocator, .{ .ty = tag_union.ext, .boxed = boxed });
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
        const proc = procs[@backingInt(proc_id)];
        const symbol = try proc.identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "root {s} borrowed_params={x} {s}\n", .{ symbol, proc.rc_borrowed_params, store.procDebugName(proc_id) orelse "" });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    for (lowered.lir_result.spec_procs.items) |spec_proc| {
        const symbol = try procs[@backingInt(spec_proc.proc)].identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "spec {s} {s}\n", .{ &std.fmt.bytesToHex(spec_proc.key, .lower), symbol });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    for (procs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        const symbol = try proc.identity.symbolName(allocator);
        defer allocator.free(symbol);
        const line = try std.fmt.allocPrint(allocator, "proc {s} {s}\n", .{ symbol, store.procDebugName(@fromBackingInt(@intCast(@as(u32, @intCast(index))))) orelse "" });
        defer allocator.free(line);
        try bytes.appendSlice(allocator, line);
    }
    return try bytes.toOwnedSlice(allocator);
}

fn testTypeId(index: usize) checked.CheckedTypeId {
    return @fromBackingInt(@intCast(index));
}

test "closed export types reject records with undetermined field kinds" {
    const allocator = std.testing.allocator;
    const leaf = testTypeId(0);
    const undetermined = testTypeId(1);
    const required = testTypeId(2);
    const label: check.CanonicalNames.RecordFieldLabelId = @fromBackingInt(@intCast(7));
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
    return @fromBackingInt(@intCast(index));
}

fn testModuleIdentity(index: usize) check.CanonicalNames.ModuleIdentityId {
    return @fromBackingInt(@intCast(index));
}

fn testTypeName(index: usize) check.CanonicalNames.TypeNameId {
    return @fromBackingInt(@intCast(index));
}

fn testTagLabel(index: usize) check.CanonicalNames.TagLabelId {
    return @fromBackingInt(@intCast(index));
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
    const nominal_key = check.CanonicalNames.NominalTypeKey{ .module = testModuleIdentity(0), .source_decl = 0 };
    const stored_nominal = checked.StoredNominal{
        .name = testTypeName(0),
        .origin_module = nominal_key.module,
        .owner_module = .{},
        .source_decl = nominal_key.source_decl,
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

test "closed export types admit functions inside a box but not type variables" {
    const allocator = std.testing.allocator;
    // `unit`, a function `unit -> unit`, `Box(unit -> unit)`, a bare
    // function in a record, a free rigid `b`, and `Box(b)`.
    const unit = testTypeId(0);
    const func = testTypeId(1);
    const boxed_func = testTypeId(2);
    const record_with_func = testTypeId(3);
    const free_rigid = testTypeId(4);
    const boxed_rigid = testTypeId(5);
    const nominal_key = check.CanonicalNames.NominalTypeKey{ .module = testModuleIdentity(0), .source_decl = 0 };
    const box_nominal = checked.StoredNominal{
        .name = testTypeName(0),
        .origin_module = nominal_key.module,
        .owner_module = .{},
        .source_decl = nominal_key.source_decl,
        .builtin = .box,
        .is_opaque = true,
        .representation = .{ .builtin = .box },
        .args = .{ .start = 1, .len = 1 },
    };
    var box_rigid_nominal = box_nominal;
    box_rigid_nominal.args = .{ .start = 2, .len = 1 };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .empty_record,
        .{ .function = .{ .kind = .pure, .args = .{ .start = 0, .len = 1 }, .ret = unit } },
        .{ .nominal = box_nominal },
        .{ .record = .{ .fields = .{ .start = 0, .len = 1 }, .ext = unit } },
        .{ .rigid = .{} },
        .{ .nominal = box_rigid_nominal },
    };
    // Pool slots: [0] the function's argument, [1] the boxed function, [2]
    // the boxed rigid.
    const type_id_pool = [_]checked.CheckedTypeId{ unit, func, free_rigid };
    const label: check.CanonicalNames.RecordFieldLabelId = @fromBackingInt(@intCast(0));
    const fields = [_]checked.CheckedRecordField{.{ .name = label, .ty = func, .kind = .required }};
    const types = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_id_pool,
        .record_field_pool = &fields,
    };

    try std.testing.expect(try checkedTypeIsClosed(allocator, types, boxed_func));
    try std.testing.expect(!try checkedTypeIsClosed(allocator, types, record_with_func));
    try std.testing.expect(!try checkedTypeIsClosed(allocator, types, boxed_rigid));
}

/// Which procedures make an erased call or build an erased callable, by
/// procedure id. The erased-call runtime those use is passed this program's
/// layout indices, so a linked object-cache object cannot offer an entry
/// whose code reaches one. Caller owns the result.
pub fn erasedCallingProcs(allocator: Allocator, store: *const LirStore) Allocator.Error![]bool {
    const erased = try allocator.alloc(bool, store.procSpecCount());
    errdefer allocator.free(erased);
    @memset(erased, false);
    const seen = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen);
    var visited = std.ArrayList(LIR.CFStmtId).empty;
    defer visited.deinit(allocator);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    @memset(seen, false);
    for (erased, 0..) |*result, index| {
        for (visited.items) |stmt_id| seen[@backingInt(stmt_id)] = false;
        visited.clearRetainingCapacity();
        work.clearRetainingCapacity();
        const proc = store.getProcSpecImmediate(@fromBackingInt(@intCast(index)));
        if (proc.abi == .erased_callable) {
            result.* = true;
            continue;
        }
        const body = proc.body orelse continue;
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if (seen[@backingInt(stmt_id)]) continue;
            seen[@backingInt(stmt_id)] = true;
            try visited.append(allocator, stmt_id);
            const uses_runtime = switch (store.getCFStmtImmediate(stmt_id).*) {
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
                => true,
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .join,
                .jump,
                .ret,
                .crash,
                => false,
            };
            if (uses_runtime) {
                result.* = true;
                break;
            }
            try BodyClone.appendSuccessors(store, &work, stmt_id, allocator);
        }
    }
    return erased;
}

/// Whether `proc`'s body holds code copied from a procedure whose identity
/// names its template by site (`LIR.InlineScope.copies_site_code`), which the
/// procedure's own identity therefore does not name.
pub fn procCopiesSiteCode(allocator: Allocator, store: *const LirStore, proc: LIR.LirProcSpecId) Allocator.Error!bool {
    const body = store.getProcSpecImmediate(proc).body orelse return false;
    const seen = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen);
    @memset(seen, false);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);
    while (work.pop()) |stmt_id| {
        if (seen[@backingInt(stmt_id)]) continue;
        seen[@backingInt(stmt_id)] = true;
        var scope = store.stmtInlineScope(stmt_id);
        while (scope != LIR.InlineScopeId.none) {
            const inline_scope = store.inlineScope(scope);
            if (inline_scope.copies_site_code) return true;
            scope = inline_scope.parent;
        }
        try BodyClone.appendSuccessors(store, &work, stmt_id, allocator);
    }
    return false;
}

/// The procedures `root`'s code reaches through direct calls, `root`
/// included, as a set over `store`'s procedures. Caller owns the result.
pub fn procCallClosure(allocator: Allocator, store: *const LirStore, root: LIR.LirProcSpecId) Allocator.Error![]bool {
    const reached = try allocator.alloc(bool, store.procSpecCount());
    errdefer allocator.free(reached);
    @memset(reached, false);
    const seen = try allocator.alloc(bool, store.cfStmtCount());
    defer allocator.free(seen);
    @memset(seen, false);
    var procs = std.ArrayList(LIR.LirProcSpecId).empty;
    defer procs.deinit(allocator);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try procs.append(allocator, root);
    reached[@backingInt(root)] = true;
    while (procs.pop()) |proc_id| {
        const body = store.getProcSpecImmediate(proc_id).body orelse continue;
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if (seen[@backingInt(stmt_id)]) continue;
            seen[@backingInt(stmt_id)] = true;
            const callee: ?LIR.LirProcSpecId = switch (store.getCFStmtImmediate(stmt_id).*) {
                .assign_call => |call| call.proc,
                .assign_packed_erased_fn => |packed_fn| packed_fn.proc,
                .assign_literal => |literal| literal.fresh_alternative,
                .init_uninitialized,
                .assign_ref,
                .assign_call_erased,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .join,
                .jump,
                .ret,
                .crash,
                => null,
            };
            if (callee) |reached_callee| {
                const callee_id = reached_callee;
                if (!reached[@backingInt(callee_id)]) {
                    reached[@backingInt(callee_id)] = true;
                    try procs.append(allocator, callee_id);
                }
            }
            try BodyClone.appendSuccessors(store, &work, stmt_id, allocator);
        }
    }
    return reached;
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
        for (visited.items) |stmt_id| seen[@backingInt(stmt_id)] = false;
        visited.clearRetainingCapacity();
        work.clearRetainingCapacity();
        const body = store.getProcSpecImmediate(@fromBackingInt(@intCast(index))).body orelse continue;
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if (seen[@backingInt(stmt_id)]) continue;
            seen[@backingInt(stmt_id)] = true;
            try visited.append(allocator, stmt_id);
            const stmt = store.getCFStmtImmediate(stmt_id);
            if (stmt.* == .crash and stmt.crash.literal_rejection != null) {
                result.* = true;
                break;
            }
            try BodyClone.appendSuccessors(store, &work, stmt_id, allocator);
        }
    }
    return converts;
}
