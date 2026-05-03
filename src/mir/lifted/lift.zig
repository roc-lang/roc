//! Lambda lifting state for row-finalized mono MIR.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const Symbol = symbol_mod.Symbol;

pub const ProcOrderKey = struct {
    ordinal: u32,
};

pub const LiftedGroupMember = struct {
    source_symbol: Symbol,
    lifted_proc: canonical.ProcedureValueRef,
    order_key: ProcOrderKey,
    args: Ast.Span(Ast.TypedSymbol),
    capture_slots: Ast.Span(Ast.CaptureSlot),
};

pub const CaptureValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    source_symbol: Symbol,
    source_ty: Type.TypeId,
    source_type_key: canonical.CanonicalTypeKey,
};

pub const CaptureProcValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    referenced_proc: canonical.ProcedureValueRef,
};

pub const LiftedCaptureGraph = struct {
    members: []const LiftedGroupMember = &.{},
    value_edges: []const CaptureValueEdge = &.{},
    proc_value_edges: []const CaptureProcValueEdge = &.{},
};

pub const Proc = struct {
    proc: canonical.MirProcedureRef,
    order_key: ProcOrderKey,
    body: Ast.DefId,
};

pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(ids.ExecutableSyntheticProc),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_metadata: std.ArrayList(ids.RootMetadata),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, row_result: MonoRow.Result) Allocator.Error!Program {
    var input = row_result;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.program.canonical_names;
    input.program.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.concrete_source_types = input.program.concrete_source_types;
    input.program.concrete_source_types = ConcreteSourceType.Store.init(allocator);
    program.types = input.program.types;
    input.program.types = Type.Store.init(allocator);
    program.literal_pool = input.program.literal_pool;
    input.program.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.program.symbols;
    input.program.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.shapes;
    input.shapes = MonoRow.Store.init(allocator);

    var lifter = BodyLifter{
        .allocator = allocator,
        .program = &program,
        .input = &input.program.ast,
        .output = &program.ast,
        .local_procs = std.AutoHashMap(Symbol, LocalProcInfo).init(allocator),
        .capture_proc_symbols = std.AutoHashMap(Symbol, void).init(allocator),
        .capture_slots = std.AutoHashMap(Symbol, u32).init(allocator),
    };
    defer lifter.deinit();

    try program.procs.ensureTotalCapacity(allocator, input.program.procs.items.len);
    for (input.program.procs.items) |proc| {
        const order_key = lifter.nextProcOrder();
        try program.procs.append(allocator, .{
            .proc = proc.proc,
            .order_key = order_key,
            .body = try lifter.lowerDef(proc.key, proc.body),
        });
    }
    try program.executable_synthetic_procs.appendSlice(allocator, input.program.executable_synthetic_procs.items);
    try program.root_procs.appendSlice(allocator, input.program.root_procs.items);
    try program.root_metadata.appendSlice(allocator, input.program.root_metadata.items);

    input.deinit();
    return program;
}

const LocalProcInfo = struct {
    source_symbol: Symbol,
    proc: canonical.MirProcedureRef,
    fn_ty: Type.TypeId,
    args: Ast.Span(Ast.TypedSymbol),
    capture_slots: Ast.Span(Ast.CaptureSlot),
};

const PreviousLocalProc = struct {
    symbol: Symbol,
    previous: ?LocalProcInfo,
};

const CaptureCandidate = struct {
    symbol: Symbol,
    ty: Type.TypeId,
    source_ty: canonical.CanonicalTypeKey,
};

const BoundRestore = struct {
    symbol: Symbol,
    was_bound: bool,
};

const CaptureSlotRestore = struct {
    symbol: Symbol,
    slot: ?u32,
};

const CaptureSet = struct {
    allocator: Allocator,
    values: std.ArrayList(CaptureCandidate),
    proc_edges: std.ArrayList(Symbol),

    fn init(allocator: Allocator) CaptureSet {
        return .{
            .allocator = allocator,
            .values = .empty,
            .proc_edges = .empty,
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.proc_edges.deinit(self.allocator);
        self.values.deinit(self.allocator);
    }

    fn addValue(
        self: *CaptureSet,
        symbol: Symbol,
        ty: Type.TypeId,
        source_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!bool {
        for (self.values.items) |existing| {
            if (existing.symbol == symbol) return false;
        }
        try self.values.append(self.allocator, .{ .symbol = symbol, .ty = ty, .source_ty = source_ty });
        return true;
    }

    fn addProcEdge(self: *CaptureSet, symbol: Symbol) Allocator.Error!void {
        for (self.proc_edges.items) |existing| {
            if (existing == symbol) return;
        }
        try self.proc_edges.append(self.allocator, symbol);
    }
};

const BodyLifter = struct {
    allocator: Allocator,
    program: *Program,
    input: *const MonoRow.Ast.Store,
    output: *Ast.Store,
    local_procs: std.AutoHashMap(Symbol, LocalProcInfo),
    capture_proc_symbols: std.AutoHashMap(Symbol, void),
    capture_slots: std.AutoHashMap(Symbol, u32),
    owner_key: canonical.MonoSpecializationKey = undefined,
    next_order: u32 = 0,

    fn deinit(self: *BodyLifter) void {
        self.capture_slots.deinit();
        self.capture_proc_symbols.deinit();
        self.local_procs.deinit();
    }

    fn nextProcOrder(self: *BodyLifter) ProcOrderKey {
        const order = ProcOrderKey{ .ordinal = self.next_order };
        self.next_order += 1;
        return order;
    }

    fn lowerDef(
        self: *BodyLifter,
        owner_key: canonical.MonoSpecializationKey,
        def_id: MonoRow.Ast.DefId,
    ) Allocator.Error!Ast.DefId {
        self.owner_key = owner_key;
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .debug_name = def.debug_name,
            .value = switch (def.value) {
                .fn_ => |fn_| .{ .fn_ = .{
                    .args = try self.lowerTypedSymbolSpan(fn_.args),
                    .captures = Ast.Span(Ast.CaptureSlot).empty(),
                    .body = try self.lowerExpr(fn_.body),
                } },
                .hosted_fn => |hosted| .{ .hosted_fn = .{
                    .proc = hosted.proc,
                    .args = try self.lowerTypedSymbolSpan(hosted.args),
                    .ret_ty = hosted.ret_ty,
                    .hosted = hosted.hosted,
                } },
                .val => |expr| .{ .val = try self.lowerExpr(expr) },
                .run => |run| .{ .run = .{ .body = try self.lowerExpr(run.body) } },
            },
        });
    }

    fn lowerExpr(self: *BodyLifter, expr_id: MonoRow.Ast.ExprId) Allocator.Error!Ast.ExprId {
        const expr = self.input.getExpr(expr_id);
        return try self.output.addExpr(expr.ty, expr.source_ty, switch (expr.data) {
            .var_ => |symbol| try self.lowerVar(expr.ty, symbol),
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .bool_lit => |value| .{ .bool_lit = value },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_instance => |const_instance| .{ .const_instance = const_instance },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order),
                .assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order),
                .constructor_ty = tag.constructor_ty,
            } },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .eval_order = try self.lowerRecordFieldEvalSpan(record.eval_order),
                .assembly_order = try self.lowerRecordFieldAssemblySpan(record.assembly_order),
            } },
            .nominal_reinterpret => |backing| .{ .nominal_reinterpret = try self.lowerExpr(backing) },
            .access => |access| .{ .access = .{
                .record = try self.lowerExpr(access.record),
                .field = access.field,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .clos => |clos| try self.lowerClosure(expr.ty, clos),
            .call_value => |call| .{ .call_value = .{
                .func = try self.lowerExpr(call.func),
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
                .requested_source_fn_ty = call.requested_source_fn_ty,
            } },
            .call_proc => |call| .{ .call_proc = .{
                .proc = call.proc,
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
                .requested_source_fn_ty = call.requested_source_fn_ty,
            } },
            .proc_value => |proc_value| .{ .proc_value = .{
                .proc = proc_value.proc,
                .captures = try self.lowerCaptureArgSpan(proc_value.captures),
                .fn_ty = proc_value.fn_ty,
            } },
            .inspect => |child| .{ .inspect = try self.lowerExpr(child) },
            .low_level => |low_level| .{ .low_level = .{
                .op = low_level.op,
                .args = try self.lowerExprSpan(low_level.args),
                .source_constraint_ty = low_level.source_constraint_ty,
            } },
            .block => |block| try self.lowerBlock(block),
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .tag_payload => |payload| .{ .tag_payload = .{
                .tag_union = try self.lowerExpr(payload.tag_union),
                .payload = payload.payload,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .unit => .unit,
            .return_ => |child| .{ .return_ = try self.lowerExpr(child) },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .match_ => |match_| .{ .match_ = .{
                .cond = try self.lowerExpr(match_.cond),
                .branches = try self.lowerBranchSpan(match_.branches),
                .is_try_suffix = match_.is_try_suffix,
            } },
            .if_ => |if_| .{ .if_ = .{
                .cond = try self.lowerExpr(if_.cond),
                .then_body = try self.lowerExpr(if_.then_body),
                .else_body = try self.lowerExpr(if_.else_body),
            } },
            .for_ => |for_| .{ .for_ = .{
                .patt = try self.lowerPat(for_.patt),
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
            .let_ => |let_| .{ .let_ = .{
                .bind = .{
                    .ty = let_.bind.ty,
                    .source_ty = let_.bind.source_ty,
                    .symbol = let_.bind.symbol,
                },
                .body = try self.lowerExpr(let_.body),
                .rest = try self.lowerExpr(let_.rest),
            } },
        });
    }

    fn lowerVar(self: *BodyLifter, ty: Type.TypeId, symbol: Symbol) Allocator.Error!Ast.Expr.Data {
        if (self.capture_slots.get(symbol)) |slot| {
            return .{ .capture_ref = slot };
        }
        if (self.local_procs.get(symbol)) |local_proc| {
            return try self.localProcValue(ty, local_proc);
        }
        return .{ .var_ = symbol };
    }

    fn isKnownLocalProc(self: *const BodyLifter, symbol: Symbol) bool {
        return self.local_procs.contains(symbol) or self.capture_proc_symbols.contains(symbol);
    }

    fn localProcValue(self: *BodyLifter, ty: Type.TypeId, local_proc: LocalProcInfo) Allocator.Error!Ast.Expr.Data {
        _ = ty;
        const slots = self.output.sliceCaptureSlotSpan(local_proc.capture_slots);
        if (slots.len == 0) {
            return .{ .proc_value = .{
                .proc = local_proc.proc,
                .captures = Ast.Span(Ast.CaptureArg).empty(),
                .fn_ty = local_proc.fn_ty,
            } };
        }

        const capture_args = try self.allocator.alloc(Ast.CaptureArg, slots.len);
        defer self.allocator.free(capture_args);
        for (slots, 0..) |slot, i| {
            const expr = if (self.capture_slots.get(slot.source_symbol)) |captured_slot|
                try self.output.addExpr(slot.ty, slot.source_ty, .{ .capture_ref = captured_slot })
            else
                try self.output.addExpr(slot.ty, slot.source_ty, .{ .var_ = slot.source_symbol });
            capture_args[i] = .{
                .slot = slot.index,
                .symbol = slot.source_symbol,
                .expr = expr,
            };
        }
        return .{ .proc_value = .{
            .proc = local_proc.proc,
            .captures = try self.output.addCaptureArgSpan(capture_args),
            .fn_ty = local_proc.fn_ty,
        } };
    }

    fn lowerClosure(self: *BodyLifter, ty: Type.TypeId, clos: anytype) Allocator.Error!Ast.Expr.Data {
        const info = try self.createLiftedProc(
            Symbol.none,
            clos.site,
            clos.source_fn_ty,
            ty,
            clos.args,
            clos.body,
        );
        return try self.localProcValue(ty, info);
    }

    fn lowerBlock(self: *BodyLifter, block: anytype) Allocator.Error!Ast.Expr.Data {
        const input_stmts = self.input.sliceStmtSpan(block.stmts);
        var restorations = std.ArrayList(PreviousLocalProc).empty;
        defer restorations.deinit(self.allocator);
        errdefer restoreLocalProcList(self, restorations.items);

        var local_fn_stmt_ids = std.ArrayList(MonoRow.Ast.StmtId).empty;
        defer local_fn_stmt_ids.deinit(self.allocator);

        for (input_stmts) |stmt_id| {
            const stmt = self.input.getStmt(stmt_id);
            switch (stmt) {
                .local_fn => |local_fn| {
                    const info = try self.reserveLiftedProc(
                        local_fn.bind.symbol,
                        local_fn.site,
                        local_fn.source_fn_ty,
                        local_fn.bind.ty,
                        try self.lowerTypedSymbolSpan(local_fn.args),
                        Ast.Span(Ast.CaptureSlot).empty(),
                    );
                    const previous = try self.local_procs.fetchPut(local_fn.bind.symbol, info);
                    try restorations.append(self.allocator, .{
                        .symbol = local_fn.bind.symbol,
                        .previous = if (previous) |entry| entry.value else null,
                    });
                    try local_fn_stmt_ids.append(self.allocator, stmt_id);
                },
                else => {},
            }
        }

        try self.fillReservedLocalProcGroup(local_fn_stmt_ids.items);

        const lowered_stmts = try self.allocator.alloc(Ast.StmtId, input_stmts.len);
        defer self.allocator.free(lowered_stmts);
        var lowered_count: usize = 0;
        for (input_stmts) |stmt_id| {
            const stmt = self.input.getStmt(stmt_id);
            switch (stmt) {
                .local_fn => {},
                else => {
                    lowered_stmts[lowered_count] = try self.lowerStmt(stmt_id);
                    lowered_count += 1;
                },
            }
        }
        const final_expr = try self.lowerExpr(block.final_expr);
        const lowered_stmt_span = try self.output.addStmtSpan(lowered_stmts[0..lowered_count]);

        restoreLocalProcList(self, restorations.items);

        return .{ .block = .{
            .stmts = lowered_stmt_span,
            .final_expr = final_expr,
        } };
    }

    fn reserveLiftedProc(
        self: *BodyLifter,
        source_symbol: Symbol,
        site: canonical.NestedProcSiteId,
        source_fn_ty: canonical.CanonicalTypeKey,
        fn_ty: Type.TypeId,
        args: Ast.Span(Ast.TypedSymbol),
        capture_slots: Ast.Span(Ast.CaptureSlot),
    ) Allocator.Error!LocalProcInfo {
        const owner_base = self.program.canonical_names.procBase(self.owner_key.template.proc_base);
        const proc_base = try self.program.canonical_names.internProcBase(.{
            .module_name = owner_base.module_name,
            .export_name = null,
            .kind = .checked_source,
            .ordinal = @intFromEnum(site),
            .nested_proc_site = .{
                .owner_template = self.owner_key.template,
                .site = site,
            },
            .owner_mono_specialization = self.owner_key,
        });
        return .{
            .source_symbol = source_symbol,
            .proc = .{
                .proc = .{
                    .artifact = self.owner_key.template.artifact,
                    .proc_base = proc_base,
                },
                .callable = .{
                    .template = .{ .lifted = .{
                        .owner_mono_specialization = self.owner_key,
                        .site = site,
                    } },
                    .source_fn_ty = source_fn_ty,
                },
            },
            .fn_ty = fn_ty,
            .args = args,
            .capture_slots = capture_slots,
        };
    }

    fn createLiftedProc(
        self: *BodyLifter,
        source_symbol: Symbol,
        site: canonical.NestedProcSiteId,
        source_fn_ty: canonical.CanonicalTypeKey,
        fn_ty: Type.TypeId,
        args: MonoRow.Ast.Span(MonoRow.Ast.TypedSymbol),
        body: MonoRow.Ast.ExprId,
    ) Allocator.Error!LocalProcInfo {
        const lowered_args = try self.lowerTypedSymbolSpan(args);
        const captures = try self.captureSlotsForBody(args, body);
        const info = try self.reserveLiftedProc(source_symbol, site, source_fn_ty, fn_ty, lowered_args, captures);
        try self.lowerLiftedProcBody(info, body);
        return info;
    }

    fn fillReservedLocalProcGroup(self: *BodyLifter, stmt_ids: []const MonoRow.Ast.StmtId) Allocator.Error!void {
        if (stmt_ids.len == 0) return;

        const sets = try self.allocator.alloc(CaptureSet, stmt_ids.len);
        defer self.allocator.free(sets);
        for (sets) |*set| set.* = CaptureSet.init(self.allocator);
        defer {
            for (sets) |*set| set.deinit();
        }

        for (stmt_ids, 0..) |stmt_id, i| {
            const local_fn = switch (self.input.getStmt(stmt_id)) {
                .local_fn => |local_fn| local_fn,
                else => liftInvariant("local function group contained non-local-function statement"),
            };
            var bound = std.AutoHashMap(Symbol, void).init(self.allocator);
            defer bound.deinit();
            for (self.input.sliceTypedSymbolSpan(local_fn.args)) |arg| {
                try bound.put(arg.symbol, {});
            }
            try self.collectExprCaptures(local_fn.body, &bound, &sets[i]);
        }

        try self.closeLocalGroupProcEdges(stmt_ids, sets);

        for (stmt_ids, 0..) |stmt_id, i| {
            const local_fn = switch (self.input.getStmt(stmt_id)) {
                .local_fn => |local_fn| local_fn,
                else => unreachable,
            };
            var info = self.local_procs.get(local_fn.bind.symbol) orelse liftInvariant("reserved local function was missing during lifting");
            info.capture_slots = try self.captureSlotSpanFromSet(&sets[i]);
            try self.local_procs.put(local_fn.bind.symbol, info);
        }

        for (stmt_ids) |stmt_id| {
            const local_fn = switch (self.input.getStmt(stmt_id)) {
                .local_fn => |local_fn| local_fn,
                else => unreachable,
            };
            const info = self.local_procs.get(local_fn.bind.symbol) orelse liftInvariant("reserved local function was missing during lifting");
            try self.lowerLiftedProcBody(info, local_fn.body);
        }
    }

    fn lowerLiftedProcBody(self: *BodyLifter, info: LocalProcInfo, body: MonoRow.Ast.ExprId) Allocator.Error!void {
        var previous_slots = std.ArrayList(CaptureSlotRestore).empty;
        defer previous_slots.deinit(self.allocator);
        errdefer restoreCaptureSlotList(self, previous_slots.items);
        const slots = self.output.sliceCaptureSlotSpan(info.capture_slots);
        for (slots) |slot| {
            const previous = try self.capture_slots.fetchPut(slot.source_symbol, slot.index);
            try previous_slots.append(self.allocator, .{
                .symbol = slot.source_symbol,
                .slot = if (previous) |entry| entry.value else null,
            });
        }

        const lowered_body = try self.lowerExpr(body);

        const def = try self.output.addDef(.{
            .proc = info.proc,
            .debug_name = if (info.source_symbol == Symbol.none) null else info.source_symbol,
            .value = .{ .fn_ = .{
                .args = info.args,
                .captures = info.capture_slots,
                .body = lowered_body,
            } },
        });
        try self.program.procs.append(self.allocator, .{
            .proc = info.proc,
            .order_key = self.nextProcOrder(),
            .body = def,
        });

        restoreCaptureSlotList(self, previous_slots.items);
    }

    fn captureSlotsForBody(
        self: *BodyLifter,
        args: MonoRow.Ast.Span(MonoRow.Ast.TypedSymbol),
        body: MonoRow.Ast.ExprId,
    ) Allocator.Error!Ast.Span(Ast.CaptureSlot) {
        var bound = std.AutoHashMap(Symbol, void).init(self.allocator);
        defer bound.deinit();
        for (self.input.sliceTypedSymbolSpan(args)) |arg| {
            try bound.put(arg.symbol, {});
        }

        var captures = CaptureSet.init(self.allocator);
        defer captures.deinit();
        try self.collectExprCaptures(body, &bound, &captures);
        try self.closeProcEdges(&captures);

        return try self.captureSlotSpanFromSet(&captures);
    }

    fn captureSlotSpanFromSet(self: *BodyLifter, captures: *const CaptureSet) Allocator.Error!Ast.Span(Ast.CaptureSlot) {
        if (captures.values.items.len == 0) return Ast.Span(Ast.CaptureSlot).empty();
        const slots = try self.allocator.alloc(Ast.CaptureSlot, captures.values.items.len);
        defer self.allocator.free(slots);
        for (captures.values.items, 0..) |capture, i| {
            slots[i] = .{
                .index = @intCast(i),
                .source_symbol = capture.symbol,
                .ty = capture.ty,
                .source_ty = capture.source_ty,
            };
        }
        return try self.output.addCaptureSlotSpan(slots);
    }

    fn collectExprCaptures(
        self: *BodyLifter,
        expr_id: MonoRow.Ast.ExprId,
        bound: *std.AutoHashMap(Symbol, void),
        captures: *CaptureSet,
    ) Allocator.Error!void {
        const expr = self.input.getExpr(expr_id);
        switch (expr.data) {
            .var_ => |symbol| {
                if (bound.contains(symbol)) return;
                if (self.isKnownLocalProc(symbol)) {
                    try captures.addProcEdge(symbol);
                    return;
                }
                _ = try captures.addValue(symbol, expr.ty, expr.source_ty);
            },
            .tag => |tag| {
                for (self.input.sliceTagPayloadEvalSpan(tag.eval_order)) |payload| {
                    try self.collectExprCaptures(payload.value, bound, captures);
                }
            },
            .record => |record| {
                for (self.input.sliceRecordFieldEvalSpan(record.eval_order)) |field| {
                    try self.collectExprCaptures(field.value, bound, captures);
                }
            },
            .nominal_reinterpret => |child| try self.collectExprCaptures(child, bound, captures),
            .access => |access| try self.collectExprCaptures(access.record, bound, captures),
            .structural_eq => |eq| {
                try self.collectExprCaptures(eq.lhs, bound, captures);
                try self.collectExprCaptures(eq.rhs, bound, captures);
            },
            .bool_not => |child| try self.collectExprCaptures(child, bound, captures),
            .let_ => |let_| {
                try self.collectExprCaptures(let_.body, bound, captures);
                const previous = try bound.fetchPut(let_.bind.symbol, {});
                defer {
                    if (previous) |_| {
                        bound.put(let_.bind.symbol, {}) catch unreachable;
                    } else {
                        _ = bound.remove(let_.bind.symbol);
                    }
                }
                try self.collectExprCaptures(let_.rest, bound, captures);
            },
            .clos => |clos| {
                var previous_args = std.ArrayList(BoundRestore).empty;
                defer previous_args.deinit(self.allocator);
                for (self.input.sliceTypedSymbolSpan(clos.args)) |arg| {
                    const previous = try bound.fetchPut(arg.symbol, {});
                    try previous_args.append(self.allocator, .{
                        .symbol = arg.symbol,
                        .was_bound = previous != null,
                    });
                }
                try self.collectExprCaptures(clos.body, bound, captures);
                restoreBoundList(bound, previous_args.items);
            },
            .call_value => |call| {
                try self.collectExprCaptures(call.func, bound, captures);
                try self.collectExprSpanCaptures(call.args, bound, captures);
            },
            .call_proc => |call| try self.collectExprSpanCaptures(call.args, bound, captures),
            .proc_value => |proc_value| {
                for (self.input.sliceCaptureArgSpan(proc_value.captures)) |capture| {
                    try self.collectExprCaptures(capture.expr, bound, captures);
                }
            },
            .inspect => |child| try self.collectExprCaptures(child, bound, captures),
            .low_level => |low_level| try self.collectExprSpanCaptures(low_level.args, bound, captures),
            .match_ => |match_| {
                try self.collectExprCaptures(match_.cond, bound, captures);
                for (self.input.sliceBranchSpan(match_.branches)) |branch_id| {
                    const branch = self.input.getBranch(branch_id);
                    var pattern_binders = std.ArrayList(BoundRestore).empty;
                    defer pattern_binders.deinit(self.allocator);
                    try self.bindPatternSymbols(branch.pat, bound, &pattern_binders);
                    if (branch.guard) |guard| try self.collectExprCaptures(guard, bound, captures);
                    try self.collectExprCaptures(branch.body, bound, captures);
                    restoreBoundList(bound, pattern_binders.items);
                }
            },
            .if_ => |if_| {
                try self.collectExprCaptures(if_.cond, bound, captures);
                try self.collectExprCaptures(if_.then_body, bound, captures);
                try self.collectExprCaptures(if_.else_body, bound, captures);
            },
            .block => |block| {
                try self.collectBlockCaptures(block, bound, captures);
            },
            .tuple => |items| try self.collectExprSpanCaptures(items, bound, captures),
            .tag_payload => |payload| try self.collectExprCaptures(payload.tag_union, bound, captures),
            .tuple_access => |access| try self.collectExprCaptures(access.tuple, bound, captures),
            .list => |items| try self.collectExprSpanCaptures(items, bound, captures),
            .return_ => |child| try self.collectExprCaptures(child, bound, captures),
            .for_ => |for_| {
                try self.collectExprCaptures(for_.iterable, bound, captures);
                var pattern_binders = std.ArrayList(BoundRestore).empty;
                defer pattern_binders.deinit(self.allocator);
                try self.bindPatternSymbols(for_.patt, bound, &pattern_binders);
                try self.collectExprCaptures(for_.body, bound, captures);
                restoreBoundList(bound, pattern_binders.items);
            },
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .bool_lit,
            .str_lit,
            .const_instance,
            .unit,
            .crash,
            .runtime_error,
            => {},
        }
    }

    fn collectExprSpanCaptures(
        self: *BodyLifter,
        span: MonoRow.Ast.Span(MonoRow.Ast.ExprId),
        bound: *std.AutoHashMap(Symbol, void),
        captures: *CaptureSet,
    ) Allocator.Error!void {
        for (self.input.sliceExprSpan(span)) |expr| {
            try self.collectExprCaptures(expr, bound, captures);
        }
    }

    fn collectStmtCaptures(
        self: *BodyLifter,
        stmt_id: MonoRow.Ast.StmtId,
        bound: *std.AutoHashMap(Symbol, void),
        captures: *CaptureSet,
        local_binders: *std.ArrayList(BoundRestore),
    ) Allocator.Error!void {
        const stmt = self.input.getStmt(stmt_id);
        switch (stmt) {
            .local_fn => {},
            .decl => |decl| {
                try self.collectExprCaptures(decl.body, bound, captures);
                try self.bindSymbol(decl.bind.symbol, bound, local_binders);
            },
            .var_decl => |decl| {
                try self.collectExprCaptures(decl.body, bound, captures);
                try self.bindSymbol(decl.bind.symbol, bound, local_binders);
            },
            .reassign => |reassign| try self.collectExprCaptures(reassign.body, bound, captures),
            .expr => |expr| try self.collectExprCaptures(expr, bound, captures),
            .debug => |expr| try self.collectExprCaptures(expr, bound, captures),
            .expect => |expr| try self.collectExprCaptures(expr, bound, captures),
            .return_ => |expr| try self.collectExprCaptures(expr, bound, captures),
            .for_ => |for_| {
                try self.collectExprCaptures(for_.iterable, bound, captures);
                var pattern_binders = std.ArrayList(BoundRestore).empty;
                defer pattern_binders.deinit(self.allocator);
                try self.bindPatternSymbols(for_.patt, bound, &pattern_binders);
                try self.collectExprCaptures(for_.body, bound, captures);
                restoreBoundList(bound, pattern_binders.items);
            },
            .while_ => |while_| {
                try self.collectExprCaptures(while_.cond, bound, captures);
                try self.collectExprCaptures(while_.body, bound, captures);
            },
            .crash,
            .break_,
            => {},
        }
    }

    fn collectBlockCaptures(
        self: *BodyLifter,
        block: anytype,
        bound: *std.AutoHashMap(Symbol, void),
        captures: *CaptureSet,
    ) Allocator.Error!void {
        const stmts = self.input.sliceStmtSpan(block.stmts);

        var local_fn_stmt_ids = std.ArrayList(MonoRow.Ast.StmtId).empty;
        defer local_fn_stmt_ids.deinit(self.allocator);

        var proc_symbol_restores = std.ArrayList(BoundRestore).empty;
        defer proc_symbol_restores.deinit(self.allocator);
        errdefer restoreCaptureProcSymbolList(self, proc_symbol_restores.items);

        for (stmts) |stmt_id| {
            const stmt = self.input.getStmt(stmt_id);
            switch (stmt) {
                .local_fn => |local_fn| {
                    const previous = try self.capture_proc_symbols.fetchPut(local_fn.bind.symbol, {});
                    try proc_symbol_restores.append(self.allocator, .{
                        .symbol = local_fn.bind.symbol,
                        .was_bound = previous != null,
                    });
                    try local_fn_stmt_ids.append(self.allocator, stmt_id);
                },
                else => {},
            }
        }

        const local_sets = try self.captureSetsForLocalFnGroup(local_fn_stmt_ids.items);
        defer {
            for (local_sets) |*set| set.deinit();
            self.allocator.free(local_sets);
        }

        var local_binders = std.ArrayList(BoundRestore).empty;
        defer local_binders.deinit(self.allocator);
        for (stmts) |stmt_id| try self.collectStmtCaptures(stmt_id, bound, captures, &local_binders);
        try self.collectExprCaptures(block.final_expr, bound, captures);
        restoreBoundList(bound, local_binders.items);

        try self.addLocalGroupProcEdgeValues(local_fn_stmt_ids.items, local_sets, captures);
        restoreCaptureProcSymbolList(self, proc_symbol_restores.items);
    }

    fn captureSetsForLocalFnGroup(
        self: *BodyLifter,
        stmt_ids: []const MonoRow.Ast.StmtId,
    ) Allocator.Error![]CaptureSet {
        const sets = try self.allocator.alloc(CaptureSet, stmt_ids.len);
        errdefer self.allocator.free(sets);
        for (sets) |*set| set.* = CaptureSet.init(self.allocator);
        errdefer {
            for (sets) |*set| set.deinit();
        }

        for (stmt_ids, 0..) |stmt_id, i| {
            const local_fn = switch (self.input.getStmt(stmt_id)) {
                .local_fn => |local_fn| local_fn,
                else => liftInvariant("local function capture group contained non-local-function statement"),
            };
            var bound = std.AutoHashMap(Symbol, void).init(self.allocator);
            defer bound.deinit();
            for (self.input.sliceTypedSymbolSpan(local_fn.args)) |arg| {
                try bound.put(arg.symbol, {});
            }
            try self.collectExprCaptures(local_fn.body, &bound, &sets[i]);
        }

        try self.closeLocalGroupProcEdges(stmt_ids, sets);
        return sets;
    }

    fn closeLocalGroupProcEdges(
        self: *BodyLifter,
        stmt_ids: []const MonoRow.Ast.StmtId,
        sets: []CaptureSet,
    ) Allocator.Error!void {
        var changed = true;
        var guard: usize = 0;
        while (changed) {
            changed = false;
            guard += 1;
            if (guard > 1024) liftInvariant("recursive local-function capture fixed point did not converge");
            for (sets) |*set| {
                for (set.proc_edges.items) |proc_symbol| {
                    if (findLocalFnIndex(self.input, stmt_ids, proc_symbol)) |target_index| {
                        for (sets[target_index].values.items) |capture| {
                            if (try set.addValue(capture.symbol, capture.ty, capture.source_ty)) changed = true;
                        }
                    } else if (self.local_procs.get(proc_symbol)) |proc| {
                        for (self.output.sliceCaptureSlotSpan(proc.capture_slots)) |slot| {
                            if (try set.addValue(slot.source_symbol, slot.ty, slot.source_ty)) changed = true;
                        }
                    }
                }
            }
        }
    }

    fn addLocalGroupProcEdgeValues(
        self: *BodyLifter,
        stmt_ids: []const MonoRow.Ast.StmtId,
        sets: []const CaptureSet,
        captures: *CaptureSet,
    ) Allocator.Error!void {
        for (captures.proc_edges.items) |proc_symbol| {
            const target_index = findLocalFnIndex(self.input, stmt_ids, proc_symbol) orelse continue;
            for (sets[target_index].values.items) |capture| {
                _ = try captures.addValue(capture.symbol, capture.ty, capture.source_ty);
            }
        }
    }

    fn bindPatternSymbols(
        self: *BodyLifter,
        pat_id: MonoRow.Ast.PatId,
        bound: *std.AutoHashMap(Symbol, void),
        restorations: *std.ArrayList(BoundRestore),
    ) Allocator.Error!void {
        const pat = self.input.getPat(pat_id);
        switch (pat.data) {
            .var_ => |symbol| try self.bindSymbol(symbol, bound, restorations),
            .as => |as| {
                try self.bindSymbol(as.symbol, bound, restorations);
                try self.bindPatternSymbols(as.pattern, bound, restorations);
            },
            .nominal => |child| try self.bindPatternSymbols(child, bound, restorations),
            .record => |record| {
                for (self.input.sliceRecordFieldPatternSpan(record.fields)) |field| {
                    try self.bindPatternSymbols(field.pattern, bound, restorations);
                }
                if (record.rest) |rest| try self.bindPatternSymbols(rest, bound, restorations);
            },
            .tuple => |items| {
                for (self.input.slicePatSpan(items)) |item| {
                    try self.bindPatternSymbols(item, bound, restorations);
                }
            },
            .list => |list| {
                for (self.input.slicePatSpan(list.items)) |item| {
                    try self.bindPatternSymbols(item, bound, restorations);
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |pattern| try self.bindPatternSymbols(pattern, bound, restorations);
                }
            },
            .tag => |tag| {
                for (self.input.sliceTagPayloadPatternSpan(tag.payloads)) |payload| {
                    try self.bindPatternSymbols(payload.pattern, bound, restorations);
                }
            },
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .wildcard,
            => {},
        }
    }

    fn bindSymbol(
        self: *BodyLifter,
        symbol: Symbol,
        bound: *std.AutoHashMap(Symbol, void),
        restorations: *std.ArrayList(BoundRestore),
    ) Allocator.Error!void {
        const previous = try bound.fetchPut(symbol, {});
        try restorations.append(self.allocator, .{
            .symbol = symbol,
            .was_bound = previous != null,
        });
    }

    fn closeProcEdges(self: *BodyLifter, captures: *CaptureSet) Allocator.Error!void {
        var changed = true;
        var guard: usize = 0;
        while (changed) {
            changed = false;
            guard += 1;
            if (guard > 1024) liftInvariant("recursive local-function capture fixed point did not converge");
            for (captures.proc_edges.items) |proc_symbol| {
                const proc = self.local_procs.get(proc_symbol) orelse continue;
                for (self.output.sliceCaptureSlotSpan(proc.capture_slots)) |slot| {
                    if (try captures.addValue(slot.source_symbol, slot.ty, slot.source_ty)) changed = true;
                }
            }
        }
    }

    fn lowerPat(self: *BodyLifter, pat_id: MonoRow.Ast.PatId) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        return try self.output.addPat(.{ .ty = pat.ty, .source_ty = pat.source_ty, .data = switch (pat.data) {
            .bool_lit => |value| .{ .bool_lit = value },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .nominal => |child| .{ .nominal = try self.lowerPat(child) },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .fields = try self.lowerRecordFieldPatternSpan(record.fields),
                .rest = if (record.rest) |rest| try self.lowerPat(rest) else null,
            } },
            .tuple => |items| .{ .tuple = try self.lowerPatSpan(items) },
            .list => |list| .{ .list = .{
                .items = try self.lowerPatSpan(list.items),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |pattern| try self.lowerPat(pattern) else null,
                } else null,
            } },
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .symbol = as.symbol,
            } },
            .var_ => |symbol| .{ .var_ = symbol },
            .wildcard => .wildcard,
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads),
            } },
        } });
    }

    fn lowerBranch(self: *BodyLifter, branch_id: MonoRow.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        return try self.output.addBranch(.{
            .pat = try self.lowerPat(branch.pat),
            .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
            .body = try self.lowerExpr(branch.body),
            .degenerate = branch.degenerate,
        });
    }

    fn lowerStmt(self: *BodyLifter, stmt_id: MonoRow.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .local_fn => liftInvariant("lifted MIR local function statement reached statement lowering outside block lifting"),
            .decl => |decl| .{ .decl = .{
                .bind = decl.bind,
                .body = try self.lowerExpr(decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = decl.bind,
                .body = try self.lowerExpr(decl.body),
            } },
            .reassign => |reassign| .{ .reassign = .{
                .target = reassign.target,
                .body = try self.lowerExpr(reassign.body),
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
            .break_ => .break_,
            .for_ => |for_| .{ .for_ = .{
                .patt = try self.lowerPat(for_.patt),
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    fn lowerExprSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.ExprId).empty();
        const output_items = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |expr, i| {
            output_items[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(output_items);
    }

    fn lowerStmtSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn lowerTagPayloadPatternSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadPattern)) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const input_items = self.input.sliceTagPayloadPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPat(payload.pattern),
            };
        }
        return try self.output.addTagPayloadPatternSpan(output_items);
    }

    fn lowerRecordFieldPatternSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.RecordFieldPattern)) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        const input_items = self.input.sliceRecordFieldPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.output.addRecordFieldPatternSpan(output_items);
    }

    fn lowerTypedSymbolSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TypedSymbol).empty();
        const output_items = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |symbol, i| {
            output_items[i] = .{ .ty = symbol.ty, .source_ty = symbol.source_ty, .symbol = symbol.symbol };
        }
        return try self.output.addTypedSymbolSpan(output_items);
    }

    fn lowerCaptureArgSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.CaptureArg)) Allocator.Error!Ast.Span(Ast.CaptureArg) {
        const input_items = self.input.sliceCaptureArgSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.CaptureArg).empty();
        const output_items = try self.allocator.alloc(Ast.CaptureArg, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |capture, i| {
            output_items[i] = .{
                .slot = capture.slot,
                .symbol = capture.symbol,
                .expr = try self.lowerExpr(capture.expr),
            };
        }
        return try self.output.addCaptureArgSpan(output_items);
    }

    fn lowerRecordFieldEvalSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.RecordFieldEval)) Allocator.Error!Ast.Span(Ast.RecordFieldEval) {
        const input_items = self.input.sliceRecordFieldEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldEval).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldEvalSpan(output_items);
    }

    fn lowerRecordFieldAssemblySpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldAssembly) {
        const input_items = self.input.sliceRecordFieldAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldAssemblySpan(output_items);
    }

    fn lowerTagPayloadEvalSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadEval)) Allocator.Error!Ast.Span(Ast.TagPayloadEval) {
        const input_items = self.input.sliceTagPayloadEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadEval).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadEvalSpan(output_items);
    }

    fn lowerTagPayloadAssemblySpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadAssembly) {
        const input_items = self.input.sliceTagPayloadAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadAssemblySpan(output_items);
    }
};

fn findLocalFnIndex(
    input: *const MonoRow.Ast.Store,
    stmt_ids: []const MonoRow.Ast.StmtId,
    symbol: Symbol,
) ?usize {
    for (stmt_ids, 0..) |stmt_id, i| {
        const stmt = input.getStmt(stmt_id);
        switch (stmt) {
            .local_fn => |local_fn| if (local_fn.bind.symbol == symbol) return i,
            else => {},
        }
    }
    return null;
}

fn restoreBoundList(
    bound: *std.AutoHashMap(Symbol, void),
    restorations: []const BoundRestore,
) void {
    var i = restorations.len;
    while (i > 0) {
        i -= 1;
        const restore = restorations[i];
        if (restore.was_bound) {
            bound.putAssumeCapacity(restore.symbol, {});
        } else {
            _ = bound.remove(restore.symbol);
        }
    }
}

fn restoreLocalProcList(
    lifter: *BodyLifter,
    restorations: []const PreviousLocalProc,
) void {
    var i = restorations.len;
    while (i > 0) {
        i -= 1;
        const restore = restorations[i];
        if (restore.previous) |previous| {
            lifter.local_procs.putAssumeCapacity(restore.symbol, previous);
        } else {
            _ = lifter.local_procs.remove(restore.symbol);
        }
    }
}

fn restoreCaptureSlotList(
    lifter: *BodyLifter,
    restorations: []const CaptureSlotRestore,
) void {
    var i = restorations.len;
    while (i > 0) {
        i -= 1;
        const restore = restorations[i];
        if (restore.slot) |slot| {
            lifter.capture_slots.putAssumeCapacity(restore.symbol, slot);
        } else {
            _ = lifter.capture_slots.remove(restore.symbol);
        }
    }
}

fn restoreCaptureProcSymbolList(
    lifter: *BodyLifter,
    restorations: []const BoundRestore,
) void {
    var i = restorations.len;
    while (i > 0) {
        i -= 1;
        const restore = restorations[i];
        if (restore.was_bound) {
            lifter.capture_proc_symbols.putAssumeCapacity(restore.symbol, {});
        } else {
            _ = lifter.capture_proc_symbols.remove(restore.symbol);
        }
    }
}

fn liftInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "lifted capture graph has explicit edge records" {
    std.testing.refAllDecls(@This());
}
