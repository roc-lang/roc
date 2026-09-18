//! Body-shard ownership and relocation contracts, checked against serial construction.
//!
//! The serial oracle allocates real typed IDs rather than reproducing the
//! relocation implementation. Unequal table lengths expose accidental use of
//! one arena's offset for another.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const Ast = @import("ast.zig");
const Type = @import("../monotype/type.zig");
const testing = std.testing;
const Allocator = std.mem.Allocator;

test "body shards borrow compile-time descriptors through publication and rollback" {
    const Common = @import("../common.zig");
    var source = try prefix(testing.allocator, 0);
    defer source.deinit();
    const root_a: Common.ComptimeValueRoot = .{
        .module = std.mem.zeroes(check.CheckedModule.ModuleId),
        .root = @enumFromInt(9),
        .const_locator = null,
    };
    var root_b = root_a;
    root_b.module.bytes[0] = 1;
    root_b.const_locator = .{
        .artifact = root_b.module,
        .owner = .{ .hoisted_expr = .{ .module_idx = 1, .expr = @enumFromInt(2) } },
        .template = @enumFromInt(3),
        .source_scheme = std.mem.zeroes(@FieldType(check.CheckedModule.ConstLocator, "source_scheme")),
    };
    const a = try source.addComptimeValueRoot(root_a);
    const b = try source.addComptimeValueRoot(root_b);
    {
        var worker = try source.cloneForSpecConstrBody(testing.allocator, @enumFromInt(0));
        defer worker.deinit();
        try testing.expectEqual(@as(usize, 0), worker.comptime_value_roots.len());
        try testing.expectEqualDeep(root_a, worker.getComptimeValueRoot(a));
        try testing.expectEqualDeep(root_b, worker.getComptimeValueRoot(b));
        const mark = worker.markSpecConstrAnalysis();
        _ = try expr(&worker, .{ .comptime_value = .{ .root = a, .initializer = @enumFromInt(0) } });
        worker.rewindSpecConstrAnalysis(mark);
        worker.finishSpecConstrAnalysis(mark);
        try testing.expectEqualDeep(root_b, worker.getComptimeValueRoot(b));
        const value = try expr(&worker, .{ .comptime_value = .{ .root = b, .initializer = @enumFromInt(0) } });
        var function = worker.getFn(@enumFromInt(0));
        function.body = .{ .roc = value };
        worker.setFn(@enumFromInt(0), function);
        // Publication must preserve the frozen root domain, even when body IDs move.
        _ = try expr(&source, .unit);
        try source.appendSpecConstrBody(&worker, 100, 17, 50, 23);
    }
    const published = source.getExpr(source.getFn(@enumFromInt(0)).body.roc).data.comptime_value;
    try testing.expectEqual(b, published.root);
    try testing.expectEqualDeep(root_b, source.getComptimeValueRoot(published.root));
    try testing.expectEqualDeep(root_a, source.view().getComptimeValueRoot(a));
    const mark = source.markSpecConstrAnalysis();
    _ = try source.addComptimeValueRoot(root_a);
    source.rewindSpecConstrAnalysis(mark);
    try testing.expectEqual(@as(usize, 2), source.comptime_value_roots.len());
    _ = try source.addComptimeValueRoot(root_b);
    source.finishSpecConstrAnalysis(mark);
    try testing.expectEqual(@as(usize, 2), source.comptime_value_roots.len());
}

fn emptyProgram(allocator: Allocator) Ast.Program {
    return Ast.Program.init(
        allocator,
        check.CheckedNames.NameStore.init(allocator),
        Type.Store.init(allocator),
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Ast.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        100,
    );
}

fn prefix(allocator: Allocator, extra: usize) Allocator.Error!Ast.Program {
    var p = emptyProgram(allocator);
    errdefer p.deinit();
    const ty = try p.types.add(.zst);
    _ = try p.names.internRecordFieldLabel("value");
    _ = try p.names.internTagLabel("Ok");
    _ = try p.addLocalWithBinder(@enumFromInt(3), ty, @enumFromInt(7));
    try p.setLocalName(@enumFromInt(0), "frozen_local");
    const frozen_body = try p.addExpr(.{ .ty = ty, .data = .unit });
    _ = try p.addPat(.{ .ty = ty, .data = .wildcard });
    _ = try p.addStmt(.{ .expr = frozen_body });
    _ = try p.addStringLiteral("frozen");
    _ = try p.addInlineScope(.{
        .source_symbol = @enumFromInt(3),
        .source_loc = base.SourceLoc.none,
        .call_site = base.SourceLoc.none,
    });
    _ = try p.addExprSpan(&.{frozen_body});
    _ = try p.addPatSpan(&.{@enumFromInt(0)});
    _ = try p.addStmtSpan(&.{@enumFromInt(0)});
    _ = try p.addTypedLocalSpan(&.{.{ .local = @enumFromInt(0), .ty = ty }});
    for (0..2) |i| {
        _ = try p.addFn(.{
            .symbol = @enumFromInt(10 + @as(u32, @intCast(i))),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = frozen_body },
            .ret = ty,
        });
    }
    for (0..extra) |_| {
        _ = try p.addExpr(.{ .ty = ty, .data = .unit });
        _ = try p.addStringLiteral("unrelated prefix payload");
    }
    try p.names.prepareForReadSharing();
    try p.types.prepareForReadSharing(&p.names);
    return p;
}

fn expr(p: *Ast.Program, data: Ast.ExprData) Allocator.Error!Ast.ExprId {
    return p.addExpr(.{ .ty = @enumFromInt(0), .data = data });
}

/// Every reference either comes from the frozen prefix or from an API result.
/// Only the supplied generated identity domains differ in the serial oracle.
fn body(p: *Ast.Program, fn_id: Ast.FnId, symbol: u32, join: u32) Allocator.Error!void {
    const ty: Type.TypeId = @enumFromInt(0);
    const frozen: Ast.ExprId = @enumFromInt(0);
    const local = try p.addLocalWithBinder(@enumFromInt(symbol), ty, @enumFromInt(7));
    try p.setLocalName(local, "private_local");
    const other = try p.addLocal(@enumFromInt(4), ty);
    const text = try p.addStringLiteral("private string");
    const scope = try p.addInlineScope(.{
        .source_symbol = @enumFromInt(symbol),
        .source_loc = .{ .file = 5, .line = 100, .column = 30 },
        .call_site = .{ .file = 6, .line = 20, .column = 40 },
        .parent = @enumFromInt(0),
    });
    const child_scope = try p.addInlineScope(.{
        .source_symbol = @enumFromInt(4),
        .source_loc = base.SourceLoc.none,
        .call_site = base.SourceLoc.none,
        .parent = scope,
    });
    p.current_inline_scope = child_scope;
    p.current_loc = .{ .file = 7, .line = 100, .column = 20 };
    p.current_region = base.Region.from_raw_offsets(100, 120);
    const value = try expr(p, .{ .local = local });
    const string = try expr(p, .{ .str_lit = text });
    const args = try p.addExprSpan(&.{ frozen, value, string });
    const params = try p.addTypedLocalSpan(&.{
        .{ .local = local, .ty = ty },
        .{ .local = @enumFromInt(0), .ty = ty },
        .{ .local = other, .ty = ty },
    });
    const bind = try p.addPat(.{ .ty = ty, .data = .{ .bind = local } });
    const pats = try p.addPatSpan(&.{ @enumFromInt(0), bind });
    const tuple_pat = try p.addPat(.{ .ty = ty, .data = .{ .tuple = pats } });
    const stmt = try p.addStmt(.{ .let_ = .{ .pat = tuple_pat, .value = value } });
    _ = try p.addStmt(.{ .crash = text });
    const statements = try p.addStmtSpan(&.{ @enumFromInt(0), stmt });
    const fields = try p.addFieldExprSpan(&.{.{ .name = @enumFromInt(0), .value = value }});
    const segments = try p.addFieldAccessSegmentSpan(&.{.{ .field = @enumFromInt(0) }});
    const destructs = try p.addRecordDestructSpan(&.{.{ .name = @enumFromInt(0), .pattern = bind }});
    const steps = try p.addStrPatternStepSpan(&.{
        .{ .capture = bind, .delimiter = text },
        .{ .capture = null, .delimiter = @enumFromInt(0) },
    });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .record = destructs } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .str_pattern = .{ .prefix = text, .steps = steps, .end = .tail } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .as = .{ .pattern = bind, .local = other } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .list = .{ .patterns = pats, .rest = .{ .index = 100, .pattern = bind } } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .tag = .{ .name = @enumFromInt(0), .payloads = pats } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .nominal = bind } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .str_lit = text } });
    _ = try expr(p, .{ .record = fields });
    _ = try expr(p, .{ .record_update = .{ .base = frozen, .fields = fields } });
    _ = try expr(p, .{ .field_access = .{ .receiver = value, .segments = segments } });
    _ = try expr(p, .{ .tag = .{ .name = @enumFromInt(0), .payloads = args } });
    _ = try expr(p, .{ .list = args });
    _ = try expr(p, .{ .tuple = args });
    _ = try expr(p, .{ .nominal = value });
    _ = try expr(p, .{ .let_ = .{ .bind = bind, .value = frozen, .rest = value } });
    _ = try expr(p, .{ .call_value = .{ .callee = value, .args = args } });
    const captures = try p.addCaptureOperandSpan(&.{
        .{ .id = p.captureIdOfLocal(@enumFromInt(0)), .value = frozen },
        .{ .id = p.captureIdOfLocal(local), .value = value },
    });
    _ = try p.addFnDefCaptureSpan(&.{
        .{ .id = p.captureIdOfLocal(local), .value = value },
        .{ .id = p.captureIdOfLocal(@enumFromInt(0)), .value = frozen },
    });
    _ = try expr(p, .{ .fn_ref = .{ .fn_id = fn_id, .captures = captures } });
    _ = try expr(p, .{ .call_proc = .{ .callee = .{ .lifted = fn_id }, .args = args, .captures = captures } });
    _ = try expr(p, .{ .try_sequence = .{
        .try_expr = frozen,
        .ok_local = local,
        .err_is_cold = true,
        .err_target = @enumFromInt(join),
        .ok_body = value,
    } });
    _ = try expr(p, .{ .try_sequence = .{
        .try_expr = value,
        .ok_local = other,
        .err_target = @enumFromInt(2),
        .ok_body = frozen,
    } });
    _ = try expr(p, .{ .try_sequence = .{
        .try_expr = frozen,
        .ok_local = local,
        .ok_body = value,
    } });
    _ = try expr(p, .{ .try_record_sequence = .{
        .try_expr = value,
        .value_local = local,
        .value_field = @enumFromInt(0),
        .rest_local = other,
        .rest_field = @enumFromInt(0),
        .err_target = @enumFromInt(join),
        .ok_body = frozen,
    } });
    _ = try expr(p, .{ .uninitialized_payload = .{ .condition = local, .mask = 100 } });
    _ = try expr(p, .{ .if_initialized_payload = .{
        .cond = value,
        .cond_mask = 100,
        .payload = local,
        .initialized = value,
        .uninitialized = frozen,
    } });
    _ = try expr(p, .{ .structural_eq = .{ .lhs = frozen, .rhs = value, .negated = true } });
    _ = try expr(p, .{ .structural_hash = .{ .value = value, .hasher = frozen } });
    _ = try expr(p, .{ .jump = .{
        .target = @enumFromInt(join),
        .args = args,
        .loop_params = params,
        .loop_values = args,
    } });
    _ = try expr(p, .{ .jump = .{ .target = @enumFromInt(2), .args = .{ .start = 0, .len = 1 } } });
    const continuation = try expr(p, .{ .continue_ = .{ .values = args } });
    const loop = try expr(p, .{ .loop_ = .{ .params = params, .initial_values = args, .body = continuation } });
    const joined = try expr(p, .{ .join_point = .{
        .id = @enumFromInt(join),
        .params = params,
        .retained = .{ .start = 0, .len = 1 },
        .body = loop,
        .remainder = value,
    } });
    const branches = try p.addBranchSpan(&.{.{ .pat = bind, .bindings = statements, .guard = frozen, .body = joined }});
    _ = try expr(p, .{ .match_ = .{ .scrutinee = value, .branches = branches } });
    const ifs = try p.addIfBranchSpan(&.{.{ .cond = frozen, .body = value }});
    _ = try expr(p, .{ .if_ = .{ .branches = ifs, .final_else = joined } });
    _ = try expr(p, .{ .tuple_access = .{ .tuple = value, .elem_index = 100 } });
    _ = try expr(p, .{ .break_ = value });
    _ = try expr(p, .{ .break_ = null });
    _ = try expr(p, .{ .dbg = value });
    _ = try expr(p, .{ .expect = value });
    _ = try expr(p, .{ .crash = text });
    const result = try expr(p, .{ .block = .{ .statements = statements, .final_expr = joined } });
    var patch = p.getFn(fn_id);
    patch.body = .{ .roc = result };
    patch.args = params;
    p.setFn(fn_id, patch);
}

/// Compare the actual owned coordinator, not its worker's virtual view.
fn equalPrograms(expected: *const Ast.Program, actual: *const Ast.Program) error{TestExpectedEqual}!void {
    const a = expected.view();
    const b = actual.view();
    try testing.expectEqual(expected.next_lift_capture_id, actual.next_lift_capture_id);
    try testing.expectEqual(expected.names.recordFieldLabelCount(), actual.names.recordFieldLabelCount());
    try testing.expectEqual(expected.names.tagLabelCount(), actual.names.tagLabelCount());
    try testing.expectEqualDeep(expected.types.get(@enumFromInt(0)), actual.types.get(@enumFromInt(0)));
    inline for (@typeInfo(Ast.ProgramView).@"struct".fields) |field| {
        if (comptime !std.mem.eql(u8, field.name, "names") and
            !std.mem.eql(u8, field.name, "types") and
            !std.mem.eql(u8, field.name, "proc_debug_names") and
            !std.mem.eql(u8, field.name, "next_symbol") and
            !std.mem.eql(u8, field.name, "string_literals"))
        {
            try testing.expectEqualDeep(@field(a, field.name), @field(b, field.name));
        }
    }
    try testing.expectEqual(a.string_literals.len, b.string_literals.len);
    for (a.string_literals, b.string_literals) |left, right| {
        try testing.expectEqualStrings(left.backing, right.backing);
        try testing.expectEqual(left.offset, right.offset);
        try testing.expectEqual(left.len, right.len);
    }
}

test "body shards relocate two overlapping private domains exactly like serial construction" {
    var coordinator = try prefix(testing.allocator, 0);
    defer coordinator.deinit();
    var expected = try prefix(testing.allocator, 0);
    defer expected.deinit();
    {
        var first = try coordinator.cloneForSpecConstrBody(testing.allocator, @enumFromInt(0));
        defer first.deinit();
        var second = try coordinator.cloneForSpecConstrBody(testing.allocator, @enumFromInt(1));
        defer second.deinit();
        try body(&first, @enumFromInt(0), 100, 50);
        try body(&second, @enumFromInt(1), 100, 50);
        const original_exprs = try testing.allocator.dupe(Ast.Expr, second.exprs.unsafeRawItemsForView());
        defer testing.allocator.free(original_exprs);
        const original_ids = try testing.allocator.dupe(Ast.ExprId, second.expr_ids.unsafeRawItemsForView());
        defer testing.allocator.free(original_ids);
        const original_fn = second.getFn(@enumFromInt(1));
        try testing.expectEqual(coordinator.getFn(@enumFromInt(0)).body.roc, second.getFn(@enumFromInt(0)).body.roc);
        try testing.expectEqualDeep(coordinator.getExpr(@enumFromInt(0)), second.getExpr(@enumFromInt(0)));
        try coordinator.appendSpecConstrBody(&first, 100, 0, 50, 0);
        try coordinator.appendSpecConstrBody(&second, 100, 17, 50, 23);
        // Publication relocates the new destination rows, never donor storage.
        // Only inspect owned rows here: source getters cease at publication.
        try testing.expectEqualDeep(original_exprs, second.exprs.unsafeRawItemsForView());
        try testing.expectEqualDeep(original_ids, second.expr_ids.unsafeRawItemsForView());
        try testing.expectEqualDeep(original_fn, second.body_prefix.?.patch);
    }
    // All private owners are gone before inspecting literals and local names.
    try body(&expected, @enumFromInt(0), 100, 50);
    try body(&expected, @enumFromInt(1), 117, 73);
    try equalPrograms(&expected, &coordinator);
    try testing.expectEqualStrings("frozen_local", coordinator.localName(@enumFromInt(0)));
}

test "body shard creation and private append do not scale with unrelated prefix" {
    var source = try prefix(testing.allocator, 10000);
    defer source.deinit();
    // A whole-prefix clone cannot fit. The private body fits comfortably.
    var storage: [64 * 1024]u8 = undefined;
    var fixed = std.heap.FixedBufferAllocator.init(&storage);
    var worker = try source.cloneForSpecConstrBody(fixed.allocator(), @enumFromInt(0));
    defer worker.deinit();
    try testing.expectEqual(@as(usize, 0), worker.exprs.len());
    try testing.expectEqual(@as(usize, 0), worker.locals.len());
    try testing.expectEqual(@as(usize, 0), worker.string_literals.len());
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqualStrings("unrelated prefix payload", worker.getStringLiteral(@enumFromInt(10000)).backing);
    try testing.expectEqualStrings("value", worker.names.recordFieldLabelText(@enumFromInt(0)));
    try testing.expectEqualStrings("Ok", worker.names.tagLabelText(@enumFromInt(0)));
    try body(&worker, @enumFromInt(0), 100, 50);
    try testing.expectEqualDeep(@as(Type.Content, .zst), worker.types.get(@enumFromInt(0)));
}

test "body shard speculative rewind owns only its suffix" {
    var source = try prefix(testing.allocator, 0);
    defer source.deinit();
    var worker = try source.cloneForSpecConstrBody(testing.allocator, @enumFromInt(0));
    defer worker.deinit();
    const mark = worker.markSpecConstrAnalysis();
    try body(&worker, @enumFromInt(0), 100, 50);
    worker.rewindSpecConstrAnalysis(mark);
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqual(source.localCount(), worker.localCount());
    try testing.expectEqual(@as(usize, 0), worker.exprs.len());
    try testing.expectEqual(@as(usize, 0), worker.string_literals.len());
    try testing.expectEqualDeep(source.getExpr(@enumFromInt(0)), worker.getExpr(@enumFromInt(0)));
    try testing.expectEqualDeep(source.getFn(@enumFromInt(0)), worker.getFn(@enumFromInt(0)));
    try testing.expectEqualStrings("frozen", worker.getStringLiteral(@enumFromInt(0)).backing);
    // Reuse rewound identities and finalize a second private analysis.
    const next = worker.markSpecConstrAnalysis();
    try body(&worker, @enumFromInt(0), 100, 50);
    worker.finishSpecConstrAnalysis(next);
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqual(source.localCount(), worker.localCount());
    try testing.expectEqualDeep(source.getFn(@enumFromInt(0)), worker.getFn(@enumFromInt(0)));
    try testing.expectEqual(@as(usize, 1), source.exprCount());
}

test "body shard coordinator allocation failures leave append logically atomic" {
    var expected = try prefix(testing.allocator, 0);
    defer expected.deinit();
    var committed = try prefix(testing.allocator, 0);
    defer committed.deinit();
    try body(&committed, @enumFromInt(0), 117, 73);
    var failures: usize = 0;
    for (0..1000) |fail_index| {
        var failing = testing.FailingAllocator.init(testing.allocator, .{});
        var coordinator = try prefix(failing.allocator(), 0);
        defer coordinator.deinit();
        var worker = try coordinator.cloneForSpecConstrBody(testing.allocator, @enumFromInt(0));
        defer worker.deinit();
        try body(&worker, @enumFromInt(0), 100, 50);
        failing.fail_index = failing.alloc_index + fail_index;
        coordinator.appendSpecConstrBody(&worker, 100, 17, 50, 23) catch |err| {
            try testing.expectEqual(error.OutOfMemory, err);
            failures += 1;
            try equalPrograms(&expected, &coordinator);
            try testing.expectEqual(expected.next_symbol, coordinator.next_symbol);
            // The same worker must remain reusable after a failed commit.
            failing.fail_index = std.math.maxInt(usize);
            try coordinator.appendSpecConstrBody(&worker, 100, 17, 50, 23);
            try equalPrograms(&committed, &coordinator);
            continue;
        };
        try equalPrograms(&committed, &coordinator);
        try testing.expect(failures > 0);
        return;
    }
    return error.AppendNeverSucceeded;
}
