//! Structural LIR tests for post-check wrapper inlining.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const postcheck = @import("postcheck");
const helpers = eval.test_helpers;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;
const LayoutIdx = @import("layout").Idx;

const LoweredSource = struct {
    resources: helpers.ParsedResources,
    lowered: lir.CheckedPipeline.LoweredProgram,

    fn deinit(self: *LoweredSource, allocator: Allocator) void {
        self.lowered.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

const LiftedSource = struct {
    resources: helpers.ParsedResources,
    lifted: postcheck.MonotypeLifted.Ast.Program,

    fn deinit(self: *LiftedSource, allocator: Allocator) void {
        self.lifted.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn lowerModule(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
) anyerror!LoweredSource {
    return lowerModuleWithProcDebugNames(allocator, source, inline_mode, false);
}

fn lowerModuleWithProcDebugNames(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    proc_debug_names: bool,
) anyerror!LoweredSource {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, .module, source, &.{});
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    const import_count = resources.import_artifacts.len + if (resources.borrowed_builtin_artifact == null) @as(usize, 0) else 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_count);
    defer allocator.free(import_views);

    var view_index: usize = 0;
    if (resources.borrowed_builtin_artifact) |builtin_artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(builtin_artifact);
        view_index += 1;
    }
    for (resources.import_artifacts) |*artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(artifact);
        view_index += 1;
    }

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .inline_mode = inline_mode,
            .proc_debug_names = proc_debug_names,
        },
    );
    errdefer lowered.deinit();

    return .{
        .resources = resources,
        .lowered = lowered,
    };
}

fn mainProcArgLayouts(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror![]LayoutIdx {
    const proc = lowered.lir_result.store.getProcSpec(try rootProc(lowered));
    const arg_locals = lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(LayoutIdx, arg_locals.len);
    errdefer allocator.free(arg_layouts);

    for (arg_locals, 0..) |local_id, index| {
        arg_layouts[index] = lowered.lir_result.store.getLocal(local_id).layout_idx;
    }

    return arg_layouts;
}

fn runLoweredWithHostEvents(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!eval.RuntimeHostEnv.RecordedRun {
    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interpreter = try eval.Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const result = interpreter.eval(.{
        .proc_id = try rootProc(lowered),
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.Crash => return runtime_env.snapshot(allocator),
        else => return err,
    };
    switch (result) {
        .value => {},
    }

    return runtime_env.snapshot(allocator);
}

fn expectOptimizedDbgEvents(source: []const u8, expected: []const []const u8) anyerror!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var run = try runLoweredWithHostEvents(allocator, &optimized.lowered);
    defer run.deinit(allocator);

    try std.testing.expectEqual(eval.RuntimeHostEnv.Termination.returned, run.termination);
    try std.testing.expectEqual(expected.len, run.events.len);
    for (expected, run.events) |expected_event, actual_event| {
        switch (actual_event) {
            .dbg => |msg| try std.testing.expectEqualStrings(expected_event, msg),
            else => return error.TestUnexpectedResult,
        }
    }
}

fn liftModuleAfterSpecConstr(
    allocator: Allocator,
    source: []const u8,
) anyerror!LiftedSource {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, .module, source, &.{});
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    const import_count = resources.import_artifacts.len + if (resources.borrowed_builtin_artifact == null) @as(usize, 0) else 1;
    const import_views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, import_count);
    defer allocator.free(import_views);

    var view_index: usize = 0;
    if (resources.borrowed_builtin_artifact) |builtin_artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(builtin_artifact);
        view_index += 1;
    }
    for (resources.import_artifacts) |*artifact| {
        import_views[view_index] = check.CheckedArtifact.importedView(artifact);
        view_index += 1;
    }

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = import_views,
        },
        .{ .requests = resources.checked_artifact.root_requests.requests },
        .{},
    );
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_owned = false;
    mono = undefined;
    errdefer lifted.deinit();

    try postcheck.MonotypeLifted.SpecConstr.run(allocator, &lifted);

    return .{
        .resources = resources,
        .lifted = lifted,
    };
}

fn rootProc(lowered: *const lir.CheckedPipeline.LoweredProgram) anyerror!LIR.LirProcSpecId {
    try std.testing.expectEqual(@as(usize, 1), lowered.lir_result.root_procs.items.len);
    return lowered.lir_result.root_procs.items[0];
}

fn collectAssignCallProcs(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) anyerror![]LIR.LirProcSpecId {
    const proc = lowered.lir_result.store.getProcSpec(proc_id);
    const body = proc.body orelse return allocator.alloc(LIR.LirProcSpecId, 0);

    var calls = std.ArrayList(LIR.LirProcSpecId).empty;
    errdefer calls.deinit(allocator);

    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (lowered.lir_result.store.getCFStmt(stmt_id)) {
            .assign_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_literal => |stmt| try work.append(allocator, stmt.next),
            .assign_call => |stmt| {
                try calls.append(allocator, stmt.proc);
                try work.append(allocator, stmt.next);
            },
            .assign_call_erased => |stmt| try work.append(allocator, stmt.next),
            .assign_packed_erased_fn => |stmt| try work.append(allocator, stmt.next),
            .assign_low_level => |stmt| try work.append(allocator, stmt.next),
            .assign_list => |stmt| try work.append(allocator, stmt.next),
            .assign_struct => |stmt| try work.append(allocator, stmt.next),
            .assign_tag => |stmt| try work.append(allocator, stmt.next),
            .set_local => |stmt| try work.append(allocator, stmt.next),
            .debug => |stmt| try work.append(allocator, stmt.next),
            .expect => |stmt| try work.append(allocator, stmt.next),
            .incref => |stmt| try work.append(allocator, stmt.next),
            .decref => |stmt| try work.append(allocator, stmt.next),
            .free => |stmt| try work.append(allocator, stmt.next),
            .switch_stmt => |stmt| {
                if (stmt.continuation) |continuation| try work.append(allocator, continuation);
                try work.append(allocator, stmt.default_branch);
                for (lowered.lir_result.store.getCFSwitchBranches(stmt.branches)) |branch| {
                    try work.append(allocator, branch.body);
                }
            },
            .join => |stmt| {
                try work.append(allocator, stmt.body);
                try work.append(allocator, stmt.remainder);
            },
            .runtime_error,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            .expect_err,
            => {},
        }
    }

    return try calls.toOwnedSlice(allocator);
}

const ProcShape = struct {
    arg_count: usize,
    direct_call_count: usize = 0,
    erased_call_count: usize = 0,
    self_call_count: usize = 0,
    switch_count: usize = 0,
    join_count: usize = 0,
    max_join_param_count: usize = 0,
    jump_count: usize = 0,
    struct_assign_count: usize = 0,
    tag_assign_count: usize = 0,
};

fn collectProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    proc_id: LIR.LirProcSpecId,
) anyerror!ProcShape {
    const proc = lowered.lir_result.store.getProcSpec(proc_id);
    var shape = ProcShape{
        .arg_count = lowered.lir_result.store.getLocalSpan(proc.args).len,
    };

    const body = proc.body orelse return shape;

    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, body);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |stmt_id| {
        const visited_entry = try visited.getOrPut(stmt_id);
        if (visited_entry.found_existing) continue;

        switch (lowered.lir_result.store.getCFStmt(stmt_id)) {
            .assign_ref => |stmt| try work.append(allocator, stmt.next),
            .assign_literal => |stmt| try work.append(allocator, stmt.next),
            .assign_call => |stmt| {
                shape.direct_call_count += 1;
                if (stmt.proc == proc_id) shape.self_call_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_call_erased => |stmt| {
                shape.erased_call_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_packed_erased_fn => |stmt| try work.append(allocator, stmt.next),
            .assign_low_level => |stmt| try work.append(allocator, stmt.next),
            .assign_list => |stmt| try work.append(allocator, stmt.next),
            .assign_struct => |stmt| {
                shape.struct_assign_count += 1;
                try work.append(allocator, stmt.next);
            },
            .assign_tag => |stmt| {
                shape.tag_assign_count += 1;
                try work.append(allocator, stmt.next);
            },
            .set_local => |stmt| try work.append(allocator, stmt.next),
            .debug => |stmt| try work.append(allocator, stmt.next),
            .expect => |stmt| try work.append(allocator, stmt.next),
            .incref => |stmt| try work.append(allocator, stmt.next),
            .decref => |stmt| try work.append(allocator, stmt.next),
            .free => |stmt| try work.append(allocator, stmt.next),
            .switch_stmt => |stmt| {
                shape.switch_count += 1;
                if (stmt.continuation) |continuation| try work.append(allocator, continuation);
                try work.append(allocator, stmt.default_branch);
                for (lowered.lir_result.store.getCFSwitchBranches(stmt.branches)) |branch| {
                    try work.append(allocator, branch.body);
                }
            },
            .join => |stmt| {
                shape.join_count += 1;
                shape.max_join_param_count = @max(
                    shape.max_join_param_count,
                    lowered.lir_result.store.getLocalSpan(stmt.params).len,
                );
                try work.append(allocator, stmt.body);
                try work.append(allocator, stmt.remainder);
            },
            .jump => {
                shape.jump_count += 1;
            },
            .runtime_error,
            .loop_continue,
            .loop_break,
            .ret,
            .crash,
            .expect_err,
            => {},
        }
    }

    return shape;
}

const IterCollectShape = enum {
    specialized,
    generic,
};

fn procShapeMatchesIterCollect(shape: ProcShape, wanted: IterCollectShape) bool {
    // Fingerprints of the `Iter.collect` -> `List.from_iter` worker over a range
    // (an Unknown-length iterator). `from_iter` branches on `len_if_known`, so its
    // unoptimized (generic) worker contains both the Known and Unknown loops; spec
    // constr selects the single live arm, leaving a leaner specialized worker. A
    // range carries no length, so the specialized worker takes 2 args (a Known
    // length would thread a third).
    return switch (wanted) {
        .specialized => shape.arg_count == 2 and
            shape.direct_call_count >= 10 and
            shape.switch_count >= 10 and
            shape.join_count >= 16 and
            shape.jump_count >= 20,
        .generic => shape.arg_count == 1 and
            shape.direct_call_count == 6 and
            shape.switch_count == 10 and
            shape.join_count == 15 and
            shape.jump_count == 22 and
            shape.struct_assign_count >= 8,
    };
}

fn reachableIterCollectShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    wanted: IterCollectShape,
) anyerror!bool {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const shape = try collectProcShape(allocator, lowered, proc_id);
        if (procShapeMatchesIterCollect(shape, wanted)) return true;

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return false;
}

fn reachableProcShapeCount(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime matches: fn (ProcShape) bool,
) anyerror!usize {
    var work = std.ArrayList(LIR.LirProcSpecId).empty;
    defer work.deinit(allocator);
    try work.append(allocator, try rootProc(lowered));

    var visited = std.AutoHashMap(LIR.LirProcSpecId, void).init(allocator);
    defer visited.deinit();

    var count: usize = 0;
    while (work.pop()) |proc_id| {
        const visited_entry = try visited.getOrPut(proc_id);
        if (visited_entry.found_existing) continue;

        const shape = try collectProcShape(allocator, lowered, proc_id);
        if (matches(shape)) count += 1;

        const calls = try collectAssignCallProcs(allocator, lowered, proc_id);
        defer allocator.free(calls);
        for (calls) |call| try work.append(allocator, call);
    }
    return count;
}

fn reachableProcShape(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    comptime matches: fn (ProcShape) bool,
) anyerror!bool {
    return (try reachableProcShapeCount(allocator, lowered, matches)) > 0;
}

fn markReachableLiftedExpr(
    program: *const postcheck.MonotypeLifted.Ast.Program,
    expr_id: postcheck.MonotypeLifted.Ast.ExprId,
    reachable: []bool,
) void {
    const index = @intFromEnum(expr_id);
    if (reachable[index]) return;
    reachable[index] = true;

    switch (program.exprs.items[index].data) {
        .local,
        .unit,
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .fn_ref,
        .crash,
        => {},
        .list,
        .tuple,
        => |items| for (program.exprSpan(items)) |child| markReachableLiftedExpr(program, child, reachable),
        .record => |fields| for (program.fieldExprSpan(fields)) |field| markReachableLiftedExpr(program, field.value, reachable),
        .tag => |tag| for (program.exprSpan(tag.payloads)) |payload| markReachableLiftedExpr(program, payload, reachable),
        .nominal,
        .return_,
        .dbg,
        .expect,
        => |child| markReachableLiftedExpr(program, child, reachable),
        .expect_err => |expect_err| markReachableLiftedExpr(program, expect_err.msg, reachable),
        .let_ => |let_| {
            markReachableLiftedExpr(program, let_.value, reachable);
            markReachableLiftedExpr(program, let_.rest, reachable);
        },
        .lambda,
        .def_ref,
        .fn_def,
        => {},
        .call_value => |call| {
            markReachableLiftedExpr(program, call.callee, reachable);
            for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable);
        },
        .call_proc => |call| {
            for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable);
        },
        .low_level => |call| for (program.exprSpan(call.args)) |arg| markReachableLiftedExpr(program, arg, reachable),
        .field_access => |field| markReachableLiftedExpr(program, field.receiver, reachable),
        .tuple_access => |access| markReachableLiftedExpr(program, access.tuple, reachable),
        .structural_eq => |eq| {
            markReachableLiftedExpr(program, eq.lhs, reachable);
            markReachableLiftedExpr(program, eq.rhs, reachable);
        },
        .match_ => |match| {
            markReachableLiftedExpr(program, match.scrutinee, reachable);
            for (program.branchSpan(match.branches)) |branch| {
                if (branch.guard) |guard| markReachableLiftedExpr(program, guard, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
        },
        .if_ => |if_| {
            for (program.ifBranchSpan(if_.branches)) |branch| {
                markReachableLiftedExpr(program, branch.cond, reachable);
                markReachableLiftedExpr(program, branch.body, reachable);
            }
            markReachableLiftedExpr(program, if_.final_else, reachable);
        },
        .block => |block| {
            for (program.stmtSpan(block.statements)) |stmt| markReachableLiftedStmt(program, stmt, reachable);
            markReachableLiftedExpr(program, block.final_expr, reachable);
        },
        .loop_ => |loop| {
            for (program.exprSpan(loop.initial_values)) |initial| markReachableLiftedExpr(program, initial, reachable);
            markReachableLiftedExpr(program, loop.body, reachable);
        },
        .break_ => |maybe| if (maybe) |value| markReachableLiftedExpr(program, value, reachable),
        .continue_ => |continue_| for (program.exprSpan(continue_.values)) |value| markReachableLiftedExpr(program, value, reachable),
    }
}

fn markReachableLiftedStmt(
    program: *const postcheck.MonotypeLifted.Ast.Program,
    stmt_id: postcheck.MonotypeLifted.Ast.StmtId,
    reachable: []bool,
) void {
    switch (program.stmts.items[@intFromEnum(stmt_id)]) {
        .let_ => |let_| markReachableLiftedExpr(program, let_.value, reachable),
        .expr,
        .expect,
        .dbg,
        .return_,
        => |expr| markReachableLiftedExpr(program, expr, reachable),
        .crash => {},
    }
}

fn countUnreachableLiftedDirectCalls(
    allocator: Allocator,
    program: *const postcheck.MonotypeLifted.Ast.Program,
) anyerror!usize {
    const reachable = try allocator.alloc(bool, program.exprs.items.len);
    defer allocator.free(reachable);
    @memset(reachable, false);

    for (program.fns.items) |fn_| {
        switch (fn_.body) {
            .roc => |body| markReachableLiftedExpr(program, body, reachable),
            .hosted => {},
        }
    }

    var count: usize = 0;
    for (program.exprs.items, reachable) |expr, is_reachable| {
        if (!is_reachable and expr.data == .call_proc) count += 1;
    }
    return count;
}

fn directRecordWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn directRecordWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn whileRecordStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 2 and
        shape.jump_count >= 2;
}

fn whileRecordStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.join_count >= 1 and
        shape.max_join_param_count == 1 and
        shape.jump_count >= 2;
}

fn directTupleWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn directTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn unusedStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn unusedStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 1;
}

fn taggedStepWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.tag_assign_count == 0;
}

fn taggedStepWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.tag_assign_count >= 1;
}

fn multiTupleWorkerIsFullySpecialized(shape: ProcShape) bool {
    return shape.arg_count == 5 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count == 0;
}

fn multiTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 3 and
        shape.self_call_count == 0 and
        shape.jump_count >= 1 and
        shape.struct_assign_count >= 2;
}

fn opaqueLetCallWorkerDoesNotDuplicateCall(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.direct_call_count == 2 and
        shape.struct_assign_count == 0;
}

fn opaqueLetCallWorkerDuplicatesCall(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.direct_call_count > 2 and
        shape.struct_assign_count == 0;
}

fn expectIterCollectWorkerSpecialized(source: []const u8) anyerror!void {
    const allocator = std.testing.allocator;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableIterCollectShape(allocator, &optimized.lowered, .specialized));
    try std.testing.expect(!try reachableIterCollectShape(allocator, &optimized.lowered, .generic));

    try std.testing.expect(!try reachableIterCollectShape(allocator, &unoptimized.lowered, .specialized));
    try std.testing.expect(try reachableIterCollectShape(allocator, &unoptimized.lowered, .generic));
}

fn rootDirectCallTarget(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) anyerror!LIR.LirProcSpecId {
    const root = try rootProc(lowered);
    const root_calls = try collectAssignCallProcs(allocator, lowered, root);
    defer allocator.free(root_calls);

    try std.testing.expectEqual(@as(usize, 1), root_calls.len);
    return root_calls[0];
}

fn expectRootTargetCallCount(
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
    expected: usize,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, inline_mode);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);

    try std.testing.expectEqual(expected, target_calls.len);
}

fn expectRootTargetHasCalls(
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, inline_mode);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);

    try std.testing.expect(target_calls.len > 0);
}

test "direct call wrapper is inlined when inline mode is enabled" {
    try expectRootTargetCallCount(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| callee(x)
        \\
        \\main : U64
        \\main = wrapper(41)
    , .direct_call_wrappers, 0);
}

test "direct call wrapper is not inlined when inline mode is none" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| callee(x)
        \\
        \\main : U64
        \\main = wrapper(41)
    , .none);
}

test "zero statement block wrapper is inlined" {
    try expectRootTargetCallCount(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| {
        \\    callee(x)
        \\}
        \\
        \\main : U64
        \\main = wrapper(41)
    , .direct_call_wrappers, 0);
}

test "block wrapper with statements is not inlined" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| {
        \\    y = x
        \\    callee(y)
        \\}
        \\
        \\main : U64
        \\main = wrapper(41)
    , .direct_call_wrappers);
}

test "call value wrapper is not inlined" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\apply : (U64 -> U64), U64 -> U64
        \\apply = |fn, x| fn(x)
        \\
        \\main : U64
        \\main = apply(callee, 41)
    , .direct_call_wrappers);
}

test "self-recursive direct wrapper is not inlined" {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator,
        \\module [main]
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| wrapper(x)
        \\
        \\main : U64 -> U64
        \\main = |x| wrapper(x)
    , .direct_call_wrappers);
    defer lowered_source.deinit(allocator);

    // The root still calls the wrapper as a separate proc (not inlined). The
    // wrapper's own self-call is gone: the TRMC pass rewrote it into a tail
    // jump, recorded as a TCE transform.
    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    try std.testing.expectEqual(
        LIR.TailTransform.tce,
        lowered_source.lowered.lir_result.store.getProcSpec(target).tail_transform,
    );
    const target_calls = try collectAssignCallProcs(allocator, &lowered_source.lowered, target);
    defer allocator.free(target_calls);
    try std.testing.expectEqual(@as(usize, 0), target_calls.len);
}

test "mutually recursive direct wrappers are not inlined" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\a : U64 -> U64
        \\a = |x| b(x)
        \\
        \\b : U64 -> U64
        \\b = |x| a(x)
        \\
        \\main : U64 -> U64
        \\main = |x| a(x)
    , .direct_call_wrappers);
}

test "capturing direct wrapper is not inlined" {
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\callee : U64 -> U64
        \\callee = |x| x + 1
        \\
        \\main : U64
        \\main = {
        \\    offset = 1
        \\    wrapper = |x| callee(x + offset)
        \\    wrapper(41)
        \\}
    , .direct_call_wrappers);
}
// ─── TRMC pass outcomes through the full pipeline ───

fn expectRootTargetTailTransform(
    source: []const u8,
    expected: LIR.TailTransform,
) anyerror!void {
    const allocator = std.testing.allocator;
    var lowered_source = try lowerModule(allocator, source, .none);
    defer lowered_source.deinit(allocator);

    const target = try rootDirectCallTarget(allocator, &lowered_source.lowered);
    try std.testing.expectEqual(
        expected,
        lowered_source.lowered.lir_result.store.getProcSpec(target).tail_transform,
    );
}

test "trmc: recursive list builder is TRMC-transformed through the pipeline" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\repeat : I64, I64 -> LinkedList
        \\repeat = |value, n| if n <= 0.I64 LinkedList.Nil else LinkedList.Cons(value, repeat(value, n - 1))
        \\
        \\main = repeat(7.I64, 3.I64)
    , .trmc);
}

test "trmc: accumulator recursion is TCE-transformed through the pipeline" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\sum_to : I64, I64 -> I64
        \\sum_to = |n, acc| if n == 0.I64 acc else sum_to(n - 1, acc + n)
        \\
        \\main = sum_to(10.I64, 0.I64)
    , .tce);
}

test "trmc: result used before the constructor is not transformed" {
    try expectRootTargetTailTransform(
        \\module [main]
        \\
        \\LinkedList := [Nil, Cons(I64, LinkedList)]
        \\
        \\length_acc : LinkedList, I64 -> I64
        \\length_acc = |list, acc| match list {
        \\    Nil => acc
        \\    Cons(_, rest) => length_acc(rest, acc + 1)
        \\}
        \\
        \\with_lengths : I64 -> LinkedList
        \\with_lengths = |n| if n <= 0.I64 LinkedList.Nil else {
        \\    rest = with_lengths(n - 1)
        \\    LinkedList.Cons(length_acc(rest, 0), rest)
        \\}
        \\
        \\main = with_lengths(4.I64)
    , .none);
}

test "plant iter pipeline specializes collect worker after inlining" {
    try expectIterCollectWorkerSpecialized(
        \\module [main]
        \\
        \\Plant : { seed : I64 }
        \\
        \\random_plant : I64 -> Plant
        \\random_plant = |seed| { seed: seed }
        \\
        \\starting_plants : () -> List(Plant)
        \\starting_plants = || {
        \\    (0.I64..=15)
        \\        .map(|i| random_plant(i * 12))
        \\        .collect()
        \\}
        \\
        \\main : List(Plant)
        \\main = starting_plants()
    );
}

test "direct iter collect worker specializes constructor recursive call" {
    try expectIterCollectWorkerSpecialized(
        \\module [main]
        \\
        \\Plant : { seed : I64 }
        \\
        \\random_plant : I64 -> Plant
        \\random_plant = |seed| { seed: seed }
        \\
        \\main : List(Plant)
        \\main =
        \\    Iter.collect(
        \\        Iter.map(0.I64..=15, |i| random_plant(i * 12)),
        \\    )
    );
}

test "spec constr does not duplicate opaque let-bound direct calls" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\read_twice : State -> I64
        \\read_twice = |state| {
        \\    x = tick(state.n)
        \\    x + x
        \\}
        \\
        \\main : I64
        \\main = read_twice({ n: 1 })
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDoesNotDuplicateCall));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDuplicatesCall));
}

test "spec constr does not duplicate opaque known-match payloads" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One(I64)]
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\read_twice : State -> I64
        \\read_twice = |state|
        \\    match One(tick(state.n)) {
        \\        One(x) => x + x
        \\    }
        \\
        \\main : I64
        \\main = read_twice({ n: 1 })
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDoesNotDuplicateCall));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, opaqueLetCallWorkerDuplicatesCall));
}

test "spec constr preserves direct call argument effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\use_after : State, I64 -> I64
        \\use_after = |state, x| {
        \\    dbg "callee-before"
        \\    state.n + x
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    use_after({ n: state.n }, tap(state.n))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg\"", "\"callee-before\"" });
}

test "spec constr preserves left-to-right order for multiple unsafe call args" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap_one : I64 -> I64
        \\tap_one = |n| {
        \\    dbg "arg-one"
        \\    n
        \\}
        \\
        \\tap_two : I64 -> I64
        \\tap_two = |n| {
        \\    dbg "arg-two"
        \\    n + 1
        \\}
        \\
        \\combine_after : State, I64, I64 -> I64
        \\combine_after = |state, x, y| {
        \\    dbg "callee-before"
        \\    state.n + x + y
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    combine_after({ n: state.n }, tap_one(state.n), tap_two(state.n))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg-one\"", "\"arg-two\"", "\"callee-before\"" });
}

test "spec constr preserves substituted capture order before direct call args" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap_capture : I64 -> I64
        \\tap_capture = |n| {
        \\    dbg "capture"
        \\    n
        \\}
        \\
        \\tap_arg : I64 -> I64
        \\tap_arg = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\outer : State, I64 -> I64
        \\outer = |state, seed| {
        \\    inner = |next, arg| {
        \\        dbg "callee-before"
        \\        seed + next.n + arg
        \\    }
        \\    inner({ n: seed }, tap_arg(state.n))
        \\}
        \\
        \\main : I64
        \\main = outer({ n: 1 }, tap_capture(2))
    , &.{ "\"capture\"", "\"arg\"", "\"callee-before\"" });
}

test "spec constr preserves callable argument effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "arg"
        \\    n
        \\}
        \\
        \\call_it : State, (I64 -> I64) -> I64
        \\call_it = |state, f|
        \\    f(tap(state.n))
        \\
        \\outer : State -> I64
        \\outer = |state| {
        \\    f = |x| {
        \\        dbg "fn-before"
        \\        x
        \\    }
        \\    call_it({ n: state.n }, f)
        \\}
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"arg\"", "\"fn-before\"" });
}

test "spec constr preserves known-match single-use payload effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One(I64)]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    match One(tap(state.n)) {
        \\        One(x) => {
        \\            dbg "branch-before"
        \\            x
        \\        }
        \\    }
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"payload\"", "\"branch-before\"" });
}

test "spec constr preserves nested known-match payload effect order" {
    try expectOptimizedDbgEvents(
        \\module [main]
        \\
        \\State : { n : I64 }
        \\Step : [One({ item : I64 })]
        \\
        \\tap : I64 -> I64
        \\tap = |n| {
        \\    dbg "payload"
        \\    n
        \\}
        \\
        \\consume : State, Step -> I64
        \\consume = |state, step|
        \\    match step {
        \\        One({ item }) => {
        \\            dbg "branch-before"
        \\            state.n + item
        \\        }
        \\    }
        \\
        \\outer : State -> I64
        \\outer = |state|
        \\    consume({ n: state.n }, One({ item: tap(state.n) }))
        \\
        \\main : I64
        \\main = outer({ n: 1 })
    , &.{ "\"payload\"", "\"branch-before\"" });
}

test "spec constr writes dynamically discovered workers once" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Step : [Start(I64), Loop(I64)]
        \\
        \\go : Step -> I64
        \\go = |step|
        \\    match step {
        \\        Start(n) => {
        \\            next = Loop(n)
        \\            go(next)
        \\        }
        \\        Loop(n) => tick(n)
        \\    }
        \\
        \\tick : I64 -> I64
        \\tick = |n| n + 1
        \\
        \\main : I64
        \\main = go(Start(1))
    ;

    var lifted = try liftModuleAfterSpecConstr(allocator, source);
    defer lifted.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), try countUnreachableLiftedDirectCalls(allocator, &lifted.lifted));
}

test "spec constr specializes recursive record state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_record : State -> I64
        \\sum_record = |state|
        \\    if state.n == 0 {
        \\        state.acc
        \\    } else {
        \\        sum_record({ n: state.n - 1, acc: state.acc + state.n })
        \\    }
        \\
        \\main : I64
        \\main = sum_record({ n: 4, acc: 0 })
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr examples for inspected loop state.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, directRecordWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, directRecordWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, directRecordWorkerIsGeneric));
}

test "spec constr specializes record state carried by while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Start : { n : I64 }
        \\State : { n : I64, acc : I64 }
        \\
        \\sum_from : Start -> I64
        \\sum_from = |start| {
        \\    var $state = { n: start.n, acc: 0 }
        \\
        \\    while $state.n != 0 {
        \\        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
        \\    }
        \\
        \\    $state.acc
        \\}
        \\
        \\main : I64
        \\main = sum_from({ n: 4 })
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, whileRecordStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, whileRecordStateWorkerIsGeneric));
}

test "spec constr specializes recursive tuple state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\sum_tuple : (I64, I64) -> I64
        \\sum_tuple = |state|
        \\    match state {
        \\        (n, acc) =>
        \\            if n == 0 {
        \\                acc
        \\            } else {
        \\                sum_tuple((n - 1, acc + n))
        \\            }
        \\    }
        \\
        \\main : I64
        \\main = sum_tuple((4, 0))
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr strict-tuple examples.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, directTupleWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, directTupleWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, directTupleWorkerIsGeneric));
}

test "spec constr leaves uninspected constructor arguments generic" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\unused_state : { n : I64 }, I64 -> I64
        \\unused_state = |state, n|
        \\    if n == 0 {
        \\        0
        \\    } else {
        \\        unused_state({ n: n }, n - 1)
        \\    }
        \\
        \\main : I64
        \\main = unused_state({ n: 0 }, 3)
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's Note [Good arguments].
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, unusedStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, unusedStateWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, unusedStateWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, unusedStateWorkerIsGeneric));
}

test "spec constr specializes tagged recursive state" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\Step : [Done, More(I64)]
        \\
        \\count_down : Step, I64 -> I64
        \\count_down = |step, acc|
        \\    match step {
        \\        Done => acc
        \\        More(n) =>
        \\            if n == 0 {
        \\                count_down(Done, acc)
        \\            } else {
        \\                count_down(More(n - 1), acc + n)
        \\            }
        \\    }
        \\
        \\main : I64
        \\main = count_down(More(4), 0)
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's SpecConstr constructor-call examples.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, taggedStepWorkerIsSpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, taggedStepWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, taggedStepWorkerIsSpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, taggedStepWorkerIsGeneric));
}

test "spec constr uses fully known entry shape for multiple tuple states" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\roman : I64, (I64, I64), (I64, I64) -> I64
        \\roman = |n, p, q|
        \\    if n == 0 {
        \\        p.0 + q.0
        \\    } else if n > 2 {
        \\        roman(n - 1, (p.1, p.0), q)
        \\    } else {
        \\        roman(n - 1, p, (q.1, q.0))
        \\    }
        \\
        \\main : I64
        \\main = roman(4, (1, 2), (3, 4))
    ;

    var optimized = try lowerModule(allocator, source, .direct_call_wrappers);
    defer optimized.deinit(allocator);

    var unoptimized = try lowerModule(allocator, source, .none);
    defer unoptimized.deinit(allocator);

    // Adapted from the GHC code base's testsuite/tests/eyeball/spec-constr1.hs.
    try std.testing.expect(try reachableProcShape(allocator, &optimized.lowered, multiTupleWorkerIsFullySpecialized));
    try std.testing.expect(!try reachableProcShape(allocator, &optimized.lowered, multiTupleWorkerIsGeneric));

    try std.testing.expect(!try reachableProcShape(allocator, &unoptimized.lowered, multiTupleWorkerIsFullySpecialized));
    try std.testing.expect(try reachableProcShape(allocator, &unoptimized.lowered, multiTupleWorkerIsGeneric));
}

test "LIR statements and procs carry resolved source locations" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\add2 : U64 -> U64
        \\add2 = |n| n + 2
        \\
        \\mul3 : U64 -> U64
        \\mul3 = |n| n * 3
        \\
        \\main : U64
        \\main = {
        \\    x = 40
        \\    mul3(add2(x))
        \\}
    ;

    var lowered_source = try lowerModuleWithProcDebugNames(allocator, source, .none, true);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    try std.testing.expectEqual(store.cf_stmts.items.len, store.cf_stmt_locs.items.len);
    try std.testing.expectEqual(store.proc_specs.items.len, store.proc_locs.items.len);
    try std.testing.expect(store.proc_debug_names.items.len > 0);
    for (store.proc_debug_names.items) |entry| {
        try std.testing.expect(entry.proc < store.proc_specs.items.len);
    }
    try std.testing.expect(store.sourceFileCount() >= 1);

    var located: usize = 0;
    for (store.cf_stmt_locs.items, store.cf_stmts.items) |loc, stmt| {
        switch (stmt) {
            .incref, .decref, .free => try std.testing.expect(!loc.hasLocation()),
            else => {},
        }
        if (loc.hasLocation()) {
            located += 1;
            try std.testing.expect(loc.file < store.sourceFileCount());
            try std.testing.expect(loc.line >= 1);
            try std.testing.expect(loc.column >= 1);
        }
    }
    try std.testing.expect(located > 0);

    var located_procs: usize = 0;
    for (store.proc_locs.items) |loc| {
        if (loc.hasLocation()) {
            located_procs += 1;
            try std.testing.expect(loc.file < store.sourceFileCount());
        }
    }
    try std.testing.expect(located_procs > 0);

    var found_add2 = false;
    var found_mul3 = false;
    for (0..store.proc_specs.items.len) |i| {
        const name = store.procDebugName(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, name, "add2")) found_add2 = true;
        if (std.mem.eql(u8, name, "mul3")) found_mul3 = true;
    }
    try std.testing.expect(found_add2);
    try std.testing.expect(found_mul3);
}

test "LIR statements carry source locations under optimizing inline mode" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\add2 : U64 -> U64
        \\add2 = |n| n + 2
        \\
        \\main : U64
        \\main = {
        \\    x = 40
        \\    add2(x)
        \\}
    ;

    var lowered_source = try lowerModule(allocator, source, .direct_call_wrappers);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    var located: usize = 0;
    for (store.cf_stmt_locs.items) |loc| {
        if (loc.hasLocation()) located += 1;
    }
    try std.testing.expect(located > 0);
}

test "LIR locals carry source-level names" {
    const allocator = std.testing.allocator;

    const source =
        \\module [main]
        \\
        \\compute : U64 -> U64
        \\compute = |n| {
        \\    first_part = n * 2
        \\    second_part = first_part + 1
        \\    second_part
        \\}
        \\
        \\main : U64
        \\main = compute(20)
    ;

    var lowered_source = try lowerModule(allocator, source, .none);
    defer lowered_source.deinit(allocator);

    const store = &lowered_source.lowered.lir_result.store;
    try std.testing.expectEqual(store.locals.items.len, store.local_names.items.len);

    var found_first = false;
    var found_second = false;
    for (0..store.locals.items.len) |i| {
        const name = store.localName(@enumFromInt(i)) orelse continue;
        if (std.mem.eql(u8, name, "first_part")) found_first = true;
        if (std.mem.eql(u8, name, "second_part")) found_second = true;
    }
    try std.testing.expect(found_first);
    try std.testing.expect(found_second);
}
