//! Structural LIR tests for post-check wrapper inlining.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const helpers = eval.test_helpers;

const Allocator = std.mem.Allocator;
const LIR = lir.LIR;

const LoweredSource = struct {
    resources: helpers.ParsedResources,
    lowered: lir.CheckedPipeline.LoweredProgram,

    fn deinit(self: *LoweredSource, allocator: Allocator) void {
        self.lowered.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn lowerModule(
    allocator: Allocator,
    source: []const u8,
    inline_mode: lir.CheckedPipeline.InlineMode,
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
        .{ .requests = resources.checked_artifact.root_requests.runtime_requests },
        .{
            .target_usize = base.target.TargetUsize.native,
            .inline_mode = inline_mode,
        },
    );
    errdefer lowered.deinit();

    return .{
        .resources = resources,
        .lowered = lowered,
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
    return switch (wanted) {
        .specialized => shape.arg_count == 3 and
            shape.direct_call_count >= 10 and
            shape.switch_count >= 10 and
            shape.join_count >= 20 and
            shape.jump_count >= 20,
        .generic => shape.arg_count == 1 and
            shape.direct_call_count == 3 and
            shape.switch_count == 6 and
            shape.join_count == 9 and
            shape.jump_count == 11 and
            shape.struct_assign_count >= 10,
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

fn directRecordWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 1 and
        shape.struct_assign_count == 0;
}

fn directRecordWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 1 and
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
        shape.self_call_count == 1 and
        shape.struct_assign_count == 0;
}

fn directTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 1 and
        shape.self_call_count == 1 and
        shape.struct_assign_count >= 1;
}

fn unusedStateWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 1 and
        shape.struct_assign_count == 0;
}

fn unusedStateWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 1 and
        shape.struct_assign_count >= 1;
}

fn taggedStepWorkerIsSpecialized(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count == 1 and
        shape.direct_call_count >= 2 and
        shape.tag_assign_count == 0;
}

fn taggedStepWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 2 and
        shape.self_call_count >= 1 and
        shape.tag_assign_count >= 1;
}

fn multiTupleWorkerIsFullySpecialized(shape: ProcShape) bool {
    return shape.arg_count == 5 and
        shape.self_call_count == 2 and
        shape.struct_assign_count == 0;
}

fn multiTupleWorkerIsGeneric(shape: ProcShape) bool {
    return shape.arg_count == 3 and
        shape.self_call_count == 2 and
        shape.struct_assign_count >= 2;
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
    try expectRootTargetHasCalls(
        \\module [main]
        \\
        \\wrapper : U64 -> U64
        \\wrapper = |x| wrapper(x)
        \\
        \\main : U64
        \\main = wrapper(1)
    , .direct_call_wrappers);
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
        \\main : U64
        \\main = a(1)
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
        \\    0.I64.to(15)
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
        \\        Iter.map(0.I64.to(15), |i| random_plant(i * 12)),
        \\    )
    );
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
