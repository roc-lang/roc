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
    var ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var source = try prefix(testing.allocator, 0, &ids);
    defer source.deinit();
    const root_a: Common.ComptimeValueRoot = .{
        .module = std.mem.zeroes(check.CheckedModule.ModuleId),
        .root = .{ .checked = @enumFromInt(9) },
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
    try source.addComptimeValueRead(root_b);
    {
        var worker = try source.cloneForSpecConstrBody(testing.allocator, ids.functions[0]);
        defer worker.deinit();
        try testing.expectEqual(@as(usize, 0), worker.comptime_value_roots.len());
        try testing.expectEqual(@as(usize, 0), worker.comptime_value_reads.len());
        try testing.expectEqual(@as(usize, 1), worker.body_prefix.?.len("comptime_value_reads"));
        try testing.expectEqualDeep(root_a, worker.getComptimeValueRoot(a));
        try testing.expectEqualDeep(root_b, worker.getComptimeValueRoot(b));
        const mark = worker.markSpecConstrAnalysis();
        _ = try expr(&worker, ids.ty, .{ .comptime_value = .{ .root = a, .initializer = ids.expression } });
        worker.rewindSpecConstrAnalysis(mark);
        worker.finishSpecConstrAnalysis(mark);
        try testing.expectEqualDeep(root_b, worker.getComptimeValueRoot(b));
        const value = try expr(&worker, ids.ty, .{ .comptime_value = .{ .root = b, .initializer = ids.expression } });
        var function = worker.getFn(ids.functions[0]);
        function.body = .{ .roc = value };
        worker.setFn(ids.functions[0], function);
        // Publication must preserve the frozen root domain, even when body IDs move.
        _ = try expr(&source, ids.ty, .unit);
        try source.appendSpecConstrBody(&worker, 100, 17, 50, 23);
    }
    try testing.expectEqualDeep(&[_]Common.ComptimeValueRoot{root_b}, source.comptimeValueReadsView());
    const published = source.getExpr(source.getFn(ids.functions[0]).body.roc).data.comptime_value;
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

const PrefixIds = struct {
    ty: Type.TypeId,
    field: check.CheckedNames.RecordFieldLabelId,
    tag: check.CheckedNames.TagLabelId,
    local: Ast.LocalId,
    expression: Ast.ExprId,
    pattern: Ast.PatId,
    statement: Ast.StmtId,
    string: Ast.StringLiteralId,
    last_string: Ast.StringLiteralId,
    scope: Ast.InlineScopeId,
    functions: [2]Ast.FnId,
};

fn prefix(allocator: Allocator, extra: usize, ids: *PrefixIds) Allocator.Error!Ast.Program {
    var p = emptyProgram(allocator);
    errdefer p.deinit();
    const ty = try p.types.add(.zst);
    ids.ty = ty;
    ids.field = try p.names.internRecordFieldLabel("value");
    ids.tag = try p.names.internTagLabel("Ok");
    ids.local = try p.addLocalWithBinder(@enumFromInt(3), ty, @enumFromInt(7));
    try p.setLocalName(ids.local, "frozen_local");
    const frozen_body = try p.addExpr(.{ .ty = ty, .data = .unit });
    ids.expression = frozen_body;
    ids.pattern = try p.addPat(.{ .ty = ty, .data = .wildcard });
    ids.statement = try p.addStmt(.{ .expr = frozen_body });
    ids.string = try p.addStringLiteral("frozen");
    ids.last_string = ids.string;
    ids.scope = try p.addInlineScope(.{
        .source_symbol = @enumFromInt(3),
        .source_loc = base.SourceLoc.none,
        .call_site = base.SourceLoc.none,
    });
    _ = try p.addExprSpan(&.{frozen_body});
    _ = try p.addPatSpan(&.{ids.pattern});
    _ = try p.addStmtSpan(&.{ids.statement});
    _ = try p.addTypedLocalSpan(&.{.{ .local = ids.local, .ty = ty }});
    for (0..2) |i| {
        ids.functions[i] = try p.addFn(.{
            .symbol = @enumFromInt(10 + @as(u32, @intCast(i))),
            .args = .empty(),
            .captures = .empty(),
            .body = .{ .roc = frozen_body },
            .ret = ty,
        });
    }
    for (0..extra) |_| {
        _ = try p.addExpr(.{ .ty = ty, .data = .unit });
        ids.last_string = try p.addStringLiteral("unrelated prefix payload");
    }
    try p.names.prepareForReadSharing();
    try p.types.prepareForReadSharing(&p.names);
    return p;
}

fn expr(p: *Ast.Program, ty: Type.TypeId, data: Ast.ExprData) Allocator.Error!Ast.ExprId {
    return p.addExpr(.{ .ty = ty, .data = data });
}

/// Every reference either comes from the frozen prefix or from an API result.
/// Only the supplied generated identity domains differ in the serial oracle.
fn body(p: *Ast.Program, ids: PrefixIds, fn_id: Ast.FnId, symbol: u32, join: u32) Allocator.Error!void {
    const ty = ids.ty;
    const frozen = ids.expression;
    const local = try p.addLocalWithBinder(@enumFromInt(symbol), ty, @enumFromInt(7));
    try p.setLocalName(local, "private_local");
    const other = try p.addLocal(@enumFromInt(4), ty);
    const text = try p.addStringLiteral("private string");
    const scope = try p.addInlineScope(.{
        .source_symbol = @enumFromInt(symbol),
        .source_loc = .{ .file = 5, .line = 100, .column = 30 },
        .call_site = .{ .file = 6, .line = 20, .column = 40 },
        .parent = ids.scope,
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
    const value = try expr(p, ty, .{ .local = local });
    const string = try expr(p, ty, .{ .str_lit = text });
    const args = try p.addExprSpan(&.{ frozen, value, string });
    const params = try p.addTypedLocalSpan(&.{
        .{ .local = local, .ty = ty },
        .{ .local = ids.local, .ty = ty },
        .{ .local = other, .ty = ty },
    });
    const bind = try p.addPat(.{ .ty = ty, .data = .{ .bind = local } });
    const pats = try p.addPatSpan(&.{ ids.pattern, bind });
    const tuple_pat = try p.addPat(.{ .ty = ty, .data = .{ .tuple = pats } });
    const stmt = try p.addStmt(.{ .let_ = .{ .pat = tuple_pat, .value = value } });
    _ = try p.addStmt(.{ .crash = text });
    const statements = try p.addStmtSpan(&.{ ids.statement, stmt });
    const fields = try p.addFieldExprSpan(&.{.{ .name = ids.field, .value = value }});
    const segments = try p.addFieldAccessSegmentSpan(&.{.{ .field = ids.field }});
    const destructs = try p.addRecordDestructSpan(&.{.{ .name = ids.field, .pattern = bind }});
    const steps = try p.addStrPatternStepSpan(&.{
        .{ .capture = bind, .delimiter = text },
        .{ .capture = null, .delimiter = ids.string },
    });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .record = destructs } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .str_pattern = .{ .prefix = text, .steps = steps, .end = .tail } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .as = .{ .pattern = bind, .local = other } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .list = .{ .patterns = pats, .rest = .{ .index = 100, .pattern = bind } } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .tag = .{ .name = ids.tag, .payloads = pats } } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .nominal = bind } });
    _ = try p.addPat(.{ .ty = ty, .data = .{ .str_lit = text } });
    _ = try expr(p, ty, .{ .record = fields });
    _ = try expr(p, ty, .{ .record_update = .{ .base = frozen, .fields = fields } });
    _ = try expr(p, ty, .{ .field_access = .{ .receiver = value, .segments = segments } });
    _ = try expr(p, ty, .{ .tag = .{ .name = ids.tag, .payloads = args } });
    _ = try expr(p, ty, .{ .list = args });
    _ = try expr(p, ty, .{ .tuple = args });
    _ = try expr(p, ty, .{ .nominal = value });
    _ = try expr(p, ty, .{ .let_ = .{ .bind = bind, .value = frozen, .rest = value } });
    _ = try expr(p, ty, .{ .call_value = .{ .callee = value, .args = args } });
    const captures = try p.addCaptureOperandSpan(&.{
        .{ .id = p.captureIdOfLocal(ids.local), .value = frozen },
        .{ .id = p.captureIdOfLocal(local), .value = value },
    });
    _ = try p.addFnDefCaptureSpan(&.{
        .{ .id = p.captureIdOfLocal(local), .value = value },
        .{ .id = p.captureIdOfLocal(ids.local), .value = frozen },
    });
    _ = try expr(p, ty, .{ .fn_ref = .{ .fn_id = fn_id, .captures = captures } });
    _ = try expr(p, ty, .{ .call_proc = .{ .callee = .{ .lifted = fn_id }, .args = args, .captures = captures } });
    _ = try expr(p, ty, .{ .try_sequence = .{
        .try_expr = frozen,
        .ok_local = local,
        .err_is_cold = true,
        .err_target = @enumFromInt(join),
        .ok_body = value,
    } });
    _ = try expr(p, ty, .{ .try_sequence = .{
        .try_expr = value,
        .ok_local = other,
        .err_target = @enumFromInt(2),
        .ok_body = frozen,
    } });
    _ = try expr(p, ty, .{ .try_sequence = .{
        .try_expr = frozen,
        .ok_local = local,
        .ok_body = value,
    } });
    _ = try expr(p, ty, .{ .try_record_sequence = .{
        .try_expr = value,
        .value_local = local,
        .value_field = ids.field,
        .rest_local = other,
        .rest_field = ids.field,
        .err_target = @enumFromInt(join),
        .ok_body = frozen,
    } });
    _ = try expr(p, ty, .{ .uninitialized_payload = .{ .condition = local, .mask = 100 } });
    _ = try expr(p, ty, .{ .if_initialized_payload = .{
        .cond = value,
        .cond_mask = 100,
        .payload = local,
        .initialized = value,
        .uninitialized = frozen,
    } });
    _ = try expr(p, ty, .{ .structural_eq = .{ .lhs = frozen, .rhs = value, .negated = true } });
    _ = try expr(p, ty, .{ .structural_hash = .{ .value = value, .hasher = frozen } });
    _ = try expr(p, ty, .{ .jump = .{
        .target = @enumFromInt(join),
        .args = args,
        .loop_params = params,
        .loop_values = args,
    } });
    _ = try expr(p, ty, .{ .jump = .{ .target = @enumFromInt(2), .args = .{ .start = 0, .len = 1 } } });
    const continuation = try expr(p, ty, .{ .continue_ = .{ .values = args } });
    const loop = try expr(p, ty, .{ .loop_ = .{ .params = params, .initial_values = args, .body = continuation } });
    const joined = try expr(p, ty, .{ .join_point = .{
        .id = @enumFromInt(join),
        .params = params,
        .retained = .{ .start = 0, .len = 1 },
        .body = loop,
        .remainder = value,
    } });
    const branches = try p.addBranchSpan(&.{.{ .pat = bind, .bindings = statements, .guard = frozen, .body = joined }});
    _ = try expr(p, ty, .{ .match_ = .{ .scrutinee = value, .branches = branches } });
    const ifs = try p.addIfBranchSpan(&.{.{ .cond = frozen, .body = value }});
    _ = try expr(p, ty, .{ .if_ = .{ .branches = ifs, .final_else = joined } });
    _ = try expr(p, ty, .{ .tuple_access = .{ .tuple = value, .elem_index = 100 } });
    _ = try expr(p, ty, .{ .break_ = value });
    _ = try expr(p, ty, .{ .break_ = null });
    _ = try expr(p, ty, .{ .dbg = value });
    _ = try expr(p, ty, .{ .expect = value });
    _ = try expr(p, ty, .{ .crash = text });
    const result = try expr(p, ty, .{ .block = .{ .statements = statements, .final_expr = joined } });
    var patch = p.getFn(fn_id);
    patch.body = .{ .roc = result };
    patch.args = params;
    p.setFn(fn_id, patch);
}

/// Compare the actual owned coordinator, not its worker's virtual view.
fn equalPrograms(expected: *const Ast.Program, expected_ids: PrefixIds, actual: *const Ast.Program, actual_ids: PrefixIds) error{TestExpectedEqual}!void {
    const a = expected.view();
    const b = actual.view();
    try testing.expectEqual(expected.next_lift_capture_id, actual.next_lift_capture_id);
    try testing.expectEqual(expected.names.recordFieldLabelCount(), actual.names.recordFieldLabelCount());
    try testing.expectEqual(expected.names.tagLabelCount(), actual.names.tagLabelCount());
    try testing.expectEqualDeep(expected.types.get(expected_ids.ty), actual.types.get(actual_ids.ty));
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
    var ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var coordinator = try prefix(testing.allocator, 0, &ids);
    defer coordinator.deinit();
    var expected_ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var expected = try prefix(testing.allocator, 0, &expected_ids);
    defer expected.deinit();
    {
        var first = try coordinator.cloneForSpecConstrBody(testing.allocator, ids.functions[0]);
        defer first.deinit();
        var second = try coordinator.cloneForSpecConstrBody(testing.allocator, ids.functions[1]);
        defer second.deinit();
        try body(&first, ids, ids.functions[0], 100, 50);
        try body(&second, ids, ids.functions[1], 100, 50);
        const original_exprs = try testing.allocator.dupe(Ast.Expr, second.exprs.unsafeRawItemsForView());
        defer testing.allocator.free(original_exprs);
        const original_ids = try testing.allocator.dupe(Ast.ExprId, second.expr_ids.unsafeRawItemsForView());
        defer testing.allocator.free(original_ids);
        const original_fn = second.getFn(ids.functions[1]);
        try testing.expectEqual(coordinator.getFn(ids.functions[0]).body.roc, second.getFn(ids.functions[0]).body.roc);
        try testing.expectEqualDeep(coordinator.getExpr(ids.expression), second.getExpr(ids.expression));
        try coordinator.appendSpecConstrBody(&first, 100, 0, 50, 0);
        try coordinator.appendSpecConstrBody(&second, 100, 17, 50, 23);
        // Publication relocates the new destination rows, never donor storage.
        // Only inspect owned rows here: source getters cease at publication.
        try testing.expectEqualDeep(original_exprs, second.exprs.unsafeRawItemsForView());
        try testing.expectEqualDeep(original_ids, second.expr_ids.unsafeRawItemsForView());
        try testing.expectEqualDeep(original_fn, second.body_prefix.?.patch);
    }
    // All private owners are gone before inspecting literals and local names.
    try body(&expected, expected_ids, expected_ids.functions[0], 100, 50);
    try body(&expected, expected_ids, expected_ids.functions[1], 117, 73);
    try equalPrograms(&expected, expected_ids, &coordinator, ids);
    try testing.expectEqualStrings("frozen_local", coordinator.localName(ids.local));
}

test "body shard creation and private append do not scale with unrelated prefix" {
    var ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var source = try prefix(testing.allocator, 10000, &ids);
    defer source.deinit();
    // A whole-prefix clone cannot fit. The private body fits comfortably.
    var storage: [64 * 1024]u8 = undefined;
    var fixed = std.heap.FixedBufferAllocator.init(&storage);
    var worker = try source.cloneForSpecConstrBody(fixed.allocator(), ids.functions[0]);
    defer worker.deinit();
    try testing.expectEqual(@as(usize, 0), worker.exprs.len());
    try testing.expectEqual(@as(usize, 0), worker.locals.len());
    try testing.expectEqual(@as(usize, 0), worker.string_literals.len());
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqualStrings("unrelated prefix payload", worker.getStringLiteral(ids.last_string).backing);
    try testing.expectEqualStrings("value", worker.names.recordFieldLabelText(ids.field));
    try testing.expectEqualStrings("Ok", worker.names.tagLabelText(ids.tag));
    try body(&worker, ids, ids.functions[0], 100, 50);
    try testing.expectEqualDeep(@as(Type.Content, .zst), worker.types.get(ids.ty));
}

test "body shard speculative rewind owns only its suffix" {
    var ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var source = try prefix(testing.allocator, 0, &ids);
    defer source.deinit();
    var worker = try source.cloneForSpecConstrBody(testing.allocator, ids.functions[0]);
    defer worker.deinit();
    const mark = worker.markSpecConstrAnalysis();
    try body(&worker, ids, ids.functions[0], 100, 50);
    worker.rewindSpecConstrAnalysis(mark);
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqual(source.localCount(), worker.localCount());
    try testing.expectEqual(@as(usize, 0), worker.exprs.len());
    try testing.expectEqual(@as(usize, 0), worker.string_literals.len());
    try testing.expectEqualDeep(source.getExpr(ids.expression), worker.getExpr(ids.expression));
    try testing.expectEqualDeep(source.getFn(ids.functions[0]), worker.getFn(ids.functions[0]));
    try testing.expectEqualStrings("frozen", worker.getStringLiteral(ids.string).backing);
    // Reuse rewound identities and finalize a second private analysis.
    const next = worker.markSpecConstrAnalysis();
    try body(&worker, ids, ids.functions[0], 100, 50);
    worker.finishSpecConstrAnalysis(next);
    try testing.expectEqual(source.exprCount(), worker.exprCount());
    try testing.expectEqual(source.localCount(), worker.localCount());
    try testing.expectEqualDeep(source.getFn(ids.functions[0]), worker.getFn(ids.functions[0]));
    try testing.expectEqual(@as(usize, 1), source.exprCount());
}

test "body shard coordinator allocation failures leave append logically atomic" {
    var expected_ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var expected = try prefix(testing.allocator, 0, &expected_ids);
    defer expected.deinit();
    var committed_ids: PrefixIds = undefined; // Fully initialized by prefix before use.
    var committed = try prefix(testing.allocator, 0, &committed_ids);
    defer committed.deinit();
    try body(&committed, committed_ids, committed_ids.functions[0], 117, 73);
    var failures: usize = 0;
    for (0..1000) |fail_index| {
        var failing = testing.FailingAllocator.init(testing.allocator, .{});
        var ids: PrefixIds = undefined; // Fully initialized by prefix before use.
        var coordinator = try prefix(failing.allocator(), 0, &ids);
        defer coordinator.deinit();
        var worker = try coordinator.cloneForSpecConstrBody(testing.allocator, ids.functions[0]);
        defer worker.deinit();
        try body(&worker, ids, ids.functions[0], 100, 50);
        failing.fail_index = failing.alloc_index + fail_index;
        coordinator.appendSpecConstrBody(&worker, 100, 17, 50, 23) catch |err| {
            try testing.expectEqual(error.OutOfMemory, err);
            failures += 1;
            try equalPrograms(&expected, expected_ids, &coordinator, ids);
            try testing.expectEqual(expected.next_symbol, coordinator.next_symbol);
            // The same worker must remain reusable after a failed commit.
            failing.fail_index = std.math.maxInt(usize);
            try coordinator.appendSpecConstrBody(&worker, 100, 17, 50, 23);
            try equalPrograms(&committed, committed_ids, &coordinator, ids);
            continue;
        };
        try equalPrograms(&committed, committed_ids, &coordinator, ids);
        try testing.expect(failures > 0);
        return;
    }
    return error.AppendNeverSucceeded;
}
