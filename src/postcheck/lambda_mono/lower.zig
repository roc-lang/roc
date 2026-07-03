//! Lambda Solved IR to Lambda Mono IR.

const std = @import("std");

const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Solved = @import("../lambda_solved/ast.zig");
const SolvedType = @import("../lambda_solved/type.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;

/// Whether inline expects should materialize during lowering.
pub const InlineExpectMode = enum {
    run,
    omit,
};

/// Options used by Lambda Mono lowering.
pub const Options = struct {
    inline_expects: InlineExpectMode = .run,
};

/// Lower Lambda Solved IR into Lambda Mono IR.
pub fn run(
    allocator: Allocator,
    solved: Solved.Program,
    /// Match sites the direct lowerer statically resolved; replayed here so
    /// both derivations demand the same set of functions.
    folded_matches: []const Lifted.Program.FoldedMatch,
    options: Options,
) Common.LowerError!Ast.Program {
    var owned = solved;
    errdefer owned.deinit();

    var string_literals = owned.lifted.string_literals;
    owned.lifted.string_literals = .empty;
    var name_store = owned.lifted.names;
    owned.lifted.names = @import("check").CheckedNames.NameStore.init(allocator);
    var program = Ast.Program.init(allocator, name_store, string_literals);
    name_store = undefined;
    string_literals = undefined;
    program.source_files = owned.lifted.source_files;
    owned.lifted.source_files = .empty;
    errdefer program.deinit();

    const solved_view = movedSolvedView(&owned, &program);
    var lowerer = try Lowerer.init(allocator, solved_view, &program, options);
    defer lowerer.folded_matches.deinit(allocator);
    for (folded_matches) |folded| {
        try lowerer.folded_matches.put(allocator, folded.scrutinee, folded.body);
    }
    defer lowerer.deinit();
    try lowerer.lower();
    program.next_symbol = lowerer.symbols.next;

    owned.deinit();
    return program;
}

fn movedSolvedView(source: *const Solved.Program, moved: *const Ast.Program) Solved.ProgramView {
    return .{
        .lifted = .{
            .names = &moved.names,
            .next_symbol = source.lifted.next_symbol,
            .types = source.lifted.types.view(),
            .imported_fns = source.lifted.imported_fns.items,
            .fns = source.lifted.fns.items,
            .exprs = source.lifted.exprs.items,
            .pats = source.lifted.pats.items,
            .stmts = source.lifted.stmts.items,
            .locals = source.lifted.locals.items,
            .expr_ids = source.lifted.expr_ids.items,
            .pat_ids = source.lifted.pat_ids.items,
            .typed_locals = source.lifted.typed_locals.items,
            .stmt_ids = source.lifted.stmt_ids.items,
            .field_exprs = source.lifted.field_exprs.items,
            .capture_operands = source.lifted.capture_operands.items,
            .record_destructs = source.lifted.record_destructs.items,
            .str_pattern_steps = source.lifted.str_pattern_steps.items,
            .branches = source.lifted.branches.items,
            .if_branches = source.lifted.if_branches.items,
            .string_literals = moved.string_literals.items,
            .proc_debug_names = &source.lifted.proc_debug_names,
            .roots = source.lifted.roots.items,
            .layout_requests = source.lifted.layout_requests.items,
            .runtime_schema_requests = source.lifted.runtime_schema_requests.items,
            .comptime_sites = source.lifted.comptime_sites.items,
            .source_files = moved.source_files.items,
            .expr_locs = source.lifted.expr_locs.items,
            .expr_regions = source.lifted.expr_regions.items,
            .stmt_locs = source.lifted.stmt_locs.items,
            .stmt_regions = source.lifted.stmt_regions.items,
            .local_names = source.lifted.local_names.items,
        },
        .types = source.types.view(),
        .defs = source.defs.items,
        .local_tys = source.local_tys.items,
        .expr_tys = source.expr_tys.items,
        .pat_tys = source.pat_tys.items,
        .fn_tys = source.fn_tys.items,
        .layout_requests = source.layout_requests.items,
        .runtime_schema_requests = source.runtime_schema_requests.items,
    };
}

const CaptureBinding = struct {
    record: Ast.ExprId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
};

const CaptureAbi = enum {
    finite,
    erased,
};

const CaptureSpanId = struct {
    start: u32,
    len: u32,

    fn from(span: SolvedType.Span) CaptureSpanId {
        return .{ .start = span.start, .len = span.len };
    }
};

const CaptureTypeId = struct {
    start: u32,
    len: u32,
    solved_fn_ty: SolvedType.TypeVarId,

    fn from(span: SolvedType.Span, solved_fn_ty: SolvedType.TypeVarId) CaptureTypeId {
        return .{
            .start = span.start,
            .len = span.len,
            .solved_fn_ty = solved_fn_ty,
        };
    }
};

const FnSpec = struct {
    source: Lifted.FnId,
    solved_fn_ty: SolvedType.TypeVarId,
    abi: CaptureAbi,
    captures: CaptureSpanId,
    capture_ty: ?Type.TypeId,
};

const FnSpecContext = struct {
    pub fn hash(_: FnSpecContext, spec: FnSpec) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, @intFromEnum(spec.source));
        std.hash.autoHash(&hasher, @intFromEnum(spec.solved_fn_ty));
        std.hash.autoHash(&hasher, spec.abi);
        std.hash.autoHash(&hasher, spec.captures.start);
        std.hash.autoHash(&hasher, spec.captures.len);
        if (spec.capture_ty) |capture_ty| {
            std.hash.autoHash(&hasher, @intFromEnum(capture_ty));
        } else {
            std.hash.autoHash(&hasher, @as(u32, std.math.maxInt(u32)));
        }
        return hasher.final();
    }

    pub fn eql(_: FnSpecContext, lhs: FnSpec, rhs: FnSpec) bool {
        return lhs.source == rhs.source and
            lhs.solved_fn_ty == rhs.solved_fn_ty and
            lhs.abi == rhs.abi and
            lhs.captures.start == rhs.captures.start and
            lhs.captures.len == rhs.captures.len and
            lhs.capture_ty == rhs.capture_ty;
    }
};

const CaptureTypeMap = std.HashMap(CaptureTypeId, Type.TypeId, CaptureSpanContext, std.hash_map.default_max_load_percentage);

const CaptureSpanContext = struct {
    pub fn hash(_: CaptureSpanContext, span: CaptureTypeId) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, span.start);
        std.hash.autoHash(&hasher, span.len);
        std.hash.autoHash(&hasher, @intFromEnum(span.solved_fn_ty));
        return hasher.final();
    }

    pub fn eql(_: CaptureSpanContext, lhs: CaptureTypeId, rhs: CaptureTypeId) bool {
        return lhs.start == rhs.start and
            lhs.len == rhs.len and
            lhs.solved_fn_ty == rhs.solved_fn_ty;
    }
};

const Lowerer = struct {
    allocator: Allocator,
    solved: Solved.ProgramView,
    program: *Ast.Program,
    type_map: std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId),
    local_map: []?Ast.LocalId,
    expr_map: []?Ast.ExprId,
    pat_map: []?Ast.PatId,
    stmt_map: []?Ast.StmtId,
    comptime_site_map: []?Ast.ComptimeSiteId,
    fn_specs: std.ArrayList(FnSpec),
    fn_spec_map: std.HashMap(FnSpec, Ast.FnId, FnSpecContext, std.hash_map.default_max_load_percentage),
    fn_written: std.ArrayList(bool),
    source_symbols: std.AutoHashMap(Common.Symbol, Lifted.FnId),
    capture_types: CaptureTypeMap,
    captures: std.AutoHashMap(Lifted.LocalId, CaptureBinding),
    symbols: Common.SymbolGen,
    erased_capture_ptr_ty: ?Type.TypeId = null,
    unit_ty: ?Type.TypeId = null,
    inline_expects: InlineExpectMode,
    /// Replays the match resolutions direct LIR lowering recorded, so the
    /// debug verifier sees the same set of demanded functions. Keyed by the
    /// match's scrutinee expression.
    folded_matches: std.AutoHashMapUnmanaged(Lifted.ExprId, Lifted.ExprId) = .empty,

    fn init(
        allocator: Allocator,
        solved: Solved.ProgramView,
        program: *Ast.Program,
        options: Options,
    ) Allocator.Error!Lowerer {
        const local_map = try allocator.alloc(?Ast.LocalId, solved.lifted.locals.len);
        errdefer allocator.free(local_map);
        @memset(local_map, null);

        const expr_map = try allocator.alloc(?Ast.ExprId, solved.lifted.exprs.len);
        errdefer allocator.free(expr_map);
        @memset(expr_map, null);

        const pat_map = try allocator.alloc(?Ast.PatId, solved.lifted.pats.len);
        errdefer allocator.free(pat_map);
        @memset(pat_map, null);

        const stmt_map = try allocator.alloc(?Ast.StmtId, solved.lifted.stmts.len);
        errdefer allocator.free(stmt_map);
        @memset(stmt_map, null);

        const comptime_site_map = try allocator.alloc(?Ast.ComptimeSiteId, solved.lifted.comptime_sites.len);
        errdefer allocator.free(comptime_site_map);
        @memset(comptime_site_map, null);

        return .{
            .allocator = allocator,
            .solved = solved,
            .program = program,
            .type_map = std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId).init(allocator),
            .local_map = local_map,
            .expr_map = expr_map,
            .pat_map = pat_map,
            .stmt_map = stmt_map,
            .comptime_site_map = comptime_site_map,
            .fn_specs = .empty,
            .fn_spec_map = std.HashMap(FnSpec, Ast.FnId, FnSpecContext, std.hash_map.default_max_load_percentage).initContext(allocator, .{}),
            .fn_written = .empty,
            .source_symbols = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator),
            .capture_types = CaptureTypeMap.initContext(allocator, .{}),
            .captures = std.AutoHashMap(Lifted.LocalId, CaptureBinding).init(allocator),
            .symbols = .{ .next = solved.lifted.next_symbol },
            .inline_expects = options.inline_expects,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.captures.deinit();
        self.capture_types.deinit();
        self.source_symbols.deinit();
        self.fn_written.deinit(self.allocator);
        self.fn_spec_map.deinit();
        self.fn_specs.deinit(self.allocator);
        self.type_map.deinit();
        self.allocator.free(self.stmt_map);
        self.allocator.free(self.comptime_site_map);
        self.allocator.free(self.pat_map);
        self.allocator.free(self.expr_map);
        self.allocator.free(self.local_map);
    }

    fn lower(self: *Lowerer) Allocator.Error!void {
        try self.indexSourceFns();

        try self.program.roots.ensureTotalCapacity(self.allocator, self.solved.lifted.roots.len);
        for (self.solved.lifted.roots) |root| {
            try self.program.roots.append(self.allocator, .{
                .fn_id = try self.ensureOwnFnSpec(root.fn_id, .finite),
                .request = root.request,
            });
        }

        try self.program.layout_requests.ensureTotalCapacity(self.allocator, self.solved.layout_requests.len);
        for (self.solved.layout_requests) |request| {
            try self.program.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = try self.lowerType(request.ty),
            });
        }

        try self.program.runtime_schema_requests.ensureTotalCapacity(self.allocator, self.solved.runtime_schema_requests.len);
        for (self.solved.runtime_schema_requests) |request| {
            try self.program.runtime_schema_requests.append(self.allocator, .{
                .def = request.def,
                .ty = try self.lowerType(request.ty),
            });
        }

        try self.lowerQueuedFns();
    }

    fn indexSourceFns(self: *Lowerer) Allocator.Error!void {
        for (self.solved.lifted.fns, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const result = try self.source_symbols.getOrPut(fn_.symbol);
            if (result.found_existing) Common.invariant("two lifted functions had the same symbol");
            result.value_ptr.* = fn_id;
        }
    }

    fn lowerQueuedFns(self: *Lowerer) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.fn_specs.items.len) : (index += 1) {
            if (self.fn_written.items[index]) continue;
            const out_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.lowerFnSpec(out_id, self.fn_specs.items[index]);
        }
    }

    fn lowerFnSpec(self: *Lowerer, out_id: Ast.FnId, spec: FnSpec) Allocator.Error!void {
        const fn_id = spec.source;
        const fn_ = self.solved.lifted.fns[@intFromEnum(fn_id)];
        self.captures.clearRetainingCapacity();
        @memset(self.local_map, null);
        @memset(self.expr_map, null);
        @memset(self.pat_map, null);
        @memset(self.stmt_map, null);

        const solved_fn_ty = spec.solved_fn_ty;
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };

        const solved_args = self.solved.types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(fn_.args);
        if (solved_args.len != lifted_args.len) Common.invariant("Lambda Mono function arity changed after Lambda Solved");

        var args = std.ArrayList(Ast.TypedLocal).empty;
        defer args.deinit(self.allocator);

        for (lifted_args, 0..) |arg, i| {
            const arg_ty = try self.lowerType(solved_args[i]);
            const local = try self.localFor(arg.local, arg_ty);
            try args.append(self.allocator, .{ .local = local, .ty = arg_ty });
        }

        switch (spec.abi) {
            .finite => {
                if (spec.capture_ty) |capture_ty| {
                    const capture_local = try self.program.addLocal(self.symbols.fresh(), capture_ty);
                    const capture_expr = try self.program.addExpr(.{
                        .ty = capture_ty,
                        .data = .{ .local = capture_local },
                    });
                    try args.append(self.allocator, .{ .local = capture_local, .ty = capture_ty });
                    try self.bindCaptureRecord(spec.captures, capture_ty, capture_expr);
                }
            },
            .erased => {
                const capture_ptr_ty = try self.erasedCapturePtrType();
                const capture_ptr_local = try self.program.addLocal(self.symbols.fresh(), capture_ptr_ty);
                const capture_expr = try self.program.addExpr(.{
                    .ty = capture_ptr_ty,
                    .data = .{ .local = capture_ptr_local },
                });
                try args.append(self.allocator, .{ .local = capture_ptr_local, .ty = capture_ptr_ty });

                if (spec.capture_ty) |capture_ty| {
                    const loaded = try self.program.addExpr(.{
                        .ty = capture_ty,
                        .data = .{ .low_level = .{
                            .op = .erased_capture_load,
                            .args = try self.program.addExprSpan(&.{capture_expr}),
                        } },
                    });
                    try self.bindCaptureRecord(spec.captures, capture_ty, loaded);
                }
            },
        }

        const body: Ast.FnBody = switch (fn_.body) {
            .roc => |expr| .{ .roc = try self.lowerExpr(expr) },
            .hosted => .hosted,
        };
        const ret = try self.lowerType(func.ret);

        const args_span = try self.program.addTypedLocalSpan(args.items);
        const symbol = self.program.fns.items[@intFromEnum(out_id)].symbol;
        self.program.fns.items[@intFromEnum(out_id)] = .{
            .symbol = symbol,
            .source = fn_.source,
            .args = args_span,
            .body = body,
            .ret = ret,
        };
        self.fn_written.items[@intFromEnum(out_id)] = true;
    }

    fn ensureOwnFnSpec(self: *Lowerer, fn_id: Lifted.FnId, abi: CaptureAbi) Allocator.Error!Ast.FnId {
        const fn_symbol = self.solved.lifted.fns[@intFromEnum(fn_id)].symbol;
        const solved_fn_ty = self.solved.types.root(self.solved.fn_tys[@intFromEnum(fn_id)]);
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };

        const callable = self.solved.types.rootContent(func.callable);
        const members = switch (callable) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("function callable slot was unresolved before Lambda Mono"),
        };

        for (self.solved.types.memberSpan(members)) |member| {
            if (member.lambda != fn_symbol) continue;
            return try self.ensureFnSpec(fn_id, solved_fn_ty, abi, member.captures);
        }

        if (std.meta.activeTag(callable) == .erased) {
            return try self.ensureFnSpec(fn_id, solved_fn_ty, abi, .empty());
        }
        Common.invariant("function callable slot did not contain its own lambda member");
    }

    fn ensureFnSpec(
        self: *Lowerer,
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        abi: CaptureAbi,
        captures: SolvedType.Span,
    ) Allocator.Error!Ast.FnId {
        const capture_items = self.solved.types.captureSpan(captures);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        const spec = FnSpec{
            .source = source,
            .solved_fn_ty = root_fn_ty,
            .abi = abi,
            .captures = CaptureSpanId.from(captures),
            .capture_ty = if (capture_items.len == 0) null else try self.captureRecordType(captures, root_fn_ty),
        };

        const result = try self.fn_spec_map.getOrPut(spec);
        if (result.found_existing) return result.value_ptr.*;

        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.program.fns.items.len)));
        const source_fn = self.solved.lifted.fns[@intFromEnum(source)];
        const symbol = self.symbols.fresh();
        try self.program.fns.append(self.allocator, undefined);
        try self.fn_specs.append(self.allocator, spec);
        try self.fn_written.append(self.allocator, false);
        result.value_ptr.* = fn_id;
        if (self.solved.lifted.procDebugName(source_fn.symbol)) |name| {
            try self.program.setProcDebugName(symbol, name);
        }

        const ret_ty = try self.lowerType(switch (self.solved.types.rootContent(spec.solved_fn_ty)) {
            .func => |func| func.ret,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        });

        self.program.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = symbol,
            .source = source_fn.source,
            .args = .empty(),
            .body = .hosted,
            .ret = ret_ty,
        };
        return fn_id;
    }

    fn sourceFnForSymbol(self: *Lowerer, symbol: Common.Symbol) Lifted.FnId {
        return self.source_symbols.get(symbol) orelse
            Common.invariant("Lambda Mono callable member referenced a missing lifted function symbol");
    }

    fn bindCaptureRecord(self: *Lowerer, captures_id: CaptureSpanId, capture_ty: Type.TypeId, capture_expr: Ast.ExprId) Allocator.Error!void {
        const captures = self.solved.types.captureSpan(.{ .start = captures_id.start, .len = captures_id.len });
        const fields = switch (self.program.types.get(capture_ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            else => Common.invariant("function capture argument was not a capture record"),
        };
        if (captures.len != fields.len) Common.invariant("function capture argument arity differed from capture slots");

        for (captures, fields) |capture, field| {
            if (capture.capture_id != field.capture_id) {
                Common.invariant("function capture argument fields differed from capture slots");
            }
            try self.captures.put(capture.local, .{
                .record = capture_expr,
                .symbol = capture.symbol,
                .ty = field.ty,
            });
        }
    }

    fn capturesForFn(self: *Lowerer, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.fns[@intFromEnum(fn_id)].symbol;
        const func = switch (self.solved.types.rootContent(self.solved.fn_tys[@intFromEnum(fn_id)])) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };
        const callable = switch (self.solved.types.rootContent(func.callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("callable value did not have a resolved callable slot"),
        };
        for (self.solved.types.memberSpan(callable)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        return .empty();
    }

    fn erasedCapturePtrType(self: *Lowerer) Allocator.Error!Type.TypeId {
        if (self.erased_capture_ptr_ty) |ty| return ty;
        const ty = try self.program.types.add(.erased_capture_ptr);
        self.erased_capture_ptr_ty = ty;
        return ty;
    }

    fn lowerExpr(self: *Lowerer, expr_id: Lifted.ExprId) Allocator.Error!Ast.ExprId {
        const index = @intFromEnum(expr_id);
        if (self.expr_map[index]) |cached| return cached;

        const expr = self.solved.lifted.exprs[index];
        const saved_loc = self.program.current_loc;
        defer self.program.current_loc = saved_loc;
        const saved_region = self.program.current_region;
        defer self.program.current_region = saved_region;
        const expr_loc = self.solved.lifted.exprLoc(expr_id);
        if (expr_loc.hasLocation()) self.program.current_loc = expr_loc;
        const expr_region = self.solved.lifted.exprRegion(expr_id);
        if (!expr_region.isEmpty()) self.program.current_region = expr_region;
        const ty = try self.lowerExprTy(expr_id);
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| try self.lowerLocalExpr(local, ty),
            .unit => .unit,
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .bytes_lit => |value| .{ .bytes_lit = value },
            .uninitialized => .uninitialized,
            .uninitialized_payload => |payload| .{ .uninitialized_payload = .{
                .condition = try self.localFor(payload.condition, try self.lowerType(self.solved.local_tys[@intFromEnum(payload.condition)])),
                .mask = payload.mask,
            } },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .record => |fields| .{ .record = try self.lowerFieldExprSpan(fields) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerExprSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerExpr(backing) },
            .let_ => |let_| .{ .let_ = .{
                .bind = try self.lowerPat(let_.bind),
                .value = try self.lowerExpr(let_.value),
                .rest = try self.lowerExpr(let_.rest),
                .comptime_site = if (let_.comptime_site) |site| try self.lowerComptimeSite(site) else null,
            } },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached Lambda Mono"),
            .fn_ref => |fn_ref| try self.lowerCallableValue(expr_id, fn_ref.fn_id, fn_ref.captures, ty),
            .call_value => |call| try self.lowerValueCall(ty, call),
            .call_proc => |call| blk: {
                break :blk switch (Lifted.directCallee(call)) {
                    .local => |callee| .{ .direct_call = .{
                        .target = .{ .local = try self.ensureOwnFnSpec(callee, .finite) },
                        .args = try self.lowerDirectCallArgs(callee, call.args, call.captures),
                        .is_cold = call.is_cold,
                    } },
                    .imported => |imported| .{ .direct_call = .{
                        .target = .{ .imported = imported },
                        .args = try self.lowerImportedDirectCallArgs(call.args),
                        .is_cold = call.is_cold,
                    } },
                };
            },
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = try self.lowerExprSpan(call.args),
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = try self.lowerExpr(field.receiver),
                .field = field.field,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .structural_hash => |h| .{ .structural_hash = .{
                .value = try self.lowerExpr(h.value),
                .hasher = try self.lowerExpr(h.hasher),
            } },
            .match_ => |match| blk: {
                if (self.folded_matches.get(match.scrutinee)) |folded_body| {
                    break :blk .{ .block = .{
                        .statements = .empty(),
                        .final_expr = try self.lowerExpr(folded_body),
                    } };
                }
                break :blk .{ .match_ = .{
                    .scrutinee = try self.lowerExpr(match.scrutinee),
                    .branches = try self.lowerBranchSpan(match.branches),
                    .comptime_site = if (match.comptime_site) |site| try self.lowerComptimeSite(site) else null,
                } };
            },
            .if_ => |if_| .{ .if_ = .{
                .branches = try self.lowerIfBranchSpan(if_.branches),
                .final_else = try self.lowerExpr(if_.final_else),
            } },
            .if_initialized_payload => |payload_switch| .{ .if_initialized_payload = .{
                .cond = try self.lowerExpr(payload_switch.cond),
                .cond_mask = payload_switch.cond_mask,
                .payload = try self.localFor(payload_switch.payload, try self.lowerType(self.solved.local_tys[@intFromEnum(payload_switch.payload)])),
                .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
                .initialized = try self.lowerExpr(payload_switch.initialized),
                .uninitialized = try self.lowerExpr(payload_switch.uninitialized),
            } },
            .try_sequence => |sequence| .{ .try_sequence = .{
                .try_expr = try self.lowerExpr(sequence.try_expr),
                .ok_local = try self.localFor(sequence.ok_local, try self.lowerType(self.solved.local_tys[@intFromEnum(sequence.ok_local)])),
                .err_is_cold = sequence.err_is_cold,
                .ok_body = try self.lowerExpr(sequence.ok_body),
            } },
            .try_record_sequence => |sequence| .{ .try_record_sequence = .{
                .try_expr = try self.lowerExpr(sequence.try_expr),
                .value_local = try self.localFor(sequence.value_local, try self.lowerType(self.solved.local_tys[@intFromEnum(sequence.value_local)])),
                .value_field = sequence.value_field,
                .rest_local = try self.localFor(sequence.rest_local, try self.lowerType(self.solved.local_tys[@intFromEnum(sequence.rest_local)])),
                .rest_field = sequence.rest_field,
                .err_is_cold = sequence.err_is_cold,
                .ok_body = try self.lowerExpr(sequence.ok_body),
            } },
            .block => |block| .{ .block = .{
                .statements = try self.lowerStmtSpan(block.statements),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .loop_ => |loop| .{ .loop_ = .{
                .params = try self.lowerTypedLocalSpan(loop.params),
                .initial_values = try self.lowerExprSpan(loop.initial_values),
                .body = try self.lowerExpr(loop.body),
            } },
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.lowerExpr(value) else null },
            .continue_ => |continue_| .{ .continue_ = .{ .values = try self.lowerExprSpan(continue_.values) } },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.value) },
            .crash => |msg| .{ .crash = msg },
            .comptime_branch_taken => |taken| .{ .comptime_branch_taken = .{
                .site = try self.lowerComptimeSite(taken.site),
                .branch_index = taken.branch_index,
                .body = try self.lowerExpr(taken.body),
            } },
            .comptime_exhaustiveness_failed => |site| .{ .comptime_exhaustiveness_failed = try self.lowerComptimeSite(site) },
            .dbg => |child| .{ .dbg = try self.lowerExpr(child) },
            .expect_err => |expect_err| .{ .expect_err = .{
                .msg = try self.lowerExpr(expect_err.msg),
                .region = expect_err.region,
            } },
            .expect => |child| if (self.inline_expects == .omit)
                .unit
            else
                .{ .expect = try self.lowerExpr(child) },
        };

        const lowered = try self.program.addExpr(.{ .ty = ty, .data = data });
        self.expr_map[index] = lowered;
        return lowered;
    }

    fn lowerLocalExpr(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        if (self.captures.get(local)) |capture| {
            return .{ .capture_access = .{
                .record = capture.record,
                .symbol = capture.symbol,
            } };
        }
        return .{ .local = try self.localFor(local, ty) };
    }

    fn lowerComptimeSite(self: *Lowerer, site: Lifted.ComptimeSiteId) Allocator.Error!Ast.ComptimeSiteId {
        const index = @intFromEnum(site);
        if (self.comptime_site_map[index]) |existing| return existing;

        const source = self.solved.lifted.comptimeSite(site);
        const lowered = try self.program.addComptimeSite(source.kind, source.region, source.checked_site, source.branch_regions);
        self.comptime_site_map[index] = lowered;
        return lowered;
    }

    fn lowerCallableValue(
        self: *Lowerer,
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        captures_span: Lifted.Span(Lifted.CaptureOperand),
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        const captures = self.memberCapturesForExpr(expr_id, fn_id);
        const capture_operands = self.solved.lifted.captureOperandSpan(captures_span);
        if (self.solved.lifted.typedLocalSpan(self.solved.lifted.fns[@intFromEnum(fn_id)].captures).len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }
        return switch (self.program.types.get(ty)) {
            .callable => |variants| blk: {
                const fn_symbol = self.solved.lifted.fns[@intFromEnum(fn_id)].symbol;
                for (self.program.types.fnVariantSpan(variants)) |variant| {
                    if (variant.source != fn_symbol) continue;
                    break :blk .{ .callable = .{
                        .ty = ty,
                        .variant = variant.id,
                        .payload = if (variant.capture_ty) |capture_ty| try self.buildCaptureRecordFromExprs(captures, capture_operands, capture_ty) else null,
                    } };
                }
                Common.invariant("finite callable type did not contain referenced function");
            },
            .erased_fn => |erased| blk: {
                const fn_symbol = self.solved.lifted.fns[@intFromEnum(fn_id)].symbol;
                for (self.program.types.fnVariantSpan(erased.members)) |variant| {
                    if (variant.source != fn_symbol) continue;
                    break :blk .{ .packed_erased_fn = .{
                        .target = variant.target,
                        .capture = if (variant.capture_ty) |capture_ty| try self.buildCaptureRecordFromExprs(captures, capture_operands, capture_ty) else null,
                    } };
                }
                Common.invariant("erased callable type did not contain referenced function");
            },
            else => Common.invariant("function value lowered to non-callable Lambda Mono type"),
        };
    }

    fn memberCapturesForExpr(self: *Lowerer, expr_id: Lifted.ExprId, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.fns[@intFromEnum(fn_id)].symbol;
        const expr_ty = self.solved.expr_tys[@intFromEnum(expr_id)];
        const callable = switch (self.solved.types.rootContent(expr_ty)) {
            .func => |func| func.callable,
            .lambda_set, .erased => expr_ty,
            else => Common.invariant("function reference expression had no callable Lambda Solved type"),
        };
        const members = switch (self.solved.types.rootContent(callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("function reference callable slot was unresolved before Lambda Mono"),
        };
        for (self.solved.types.memberSpan(members)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        Common.invariant("function reference callable slot did not contain referenced function");
    }

    fn lowerDirectCallArgs(
        self: *Lowerer,
        fn_id: Lifted.FnId,
        args_span: Lifted.Span(Lifted.ExprId),
        capture_operands_span: Lifted.Span(Lifted.CaptureOperand),
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const args = try self.lowerExprSlice(self.solved.lifted.exprSpan(args_span));
        defer self.allocator.free(args);

        const captures = self.capturesForFn(fn_id);
        if (captures.len != 0) {
            const capture_operands = self.solved.lifted.captureOperandSpan(capture_operands_span);
            if (capture_operands.len != captures.len) Common.invariant("direct call capture operand count differed from callee capture count");
            const target_fn = try self.ensureOwnFnSpec(fn_id, .finite);
            const capture_ty = self.fn_specs.items[@intFromEnum(target_fn)].capture_ty orelse
                Common.invariant("capturing direct call target had no capture record type");
            const call_args = try self.allocator.alloc(Ast.ExprId, args.len + 1);
            defer self.allocator.free(call_args);
            @memcpy(call_args[0..args.len], args);
            call_args[args.len] = try self.buildCaptureRecordFromExprs(captures, capture_operands, capture_ty);
            return try self.program.addExprSpan(call_args);
        }
        if (self.solved.lifted.captureOperandSpan(capture_operands_span).len != 0) {
            Common.invariant("direct call carried capture operands for a capture-free callee");
        }

        return try self.program.addExprSpan(args);
    }

    fn lowerImportedDirectCallArgs(self: *Lowerer, args_span: Lifted.Span(Lifted.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const args = try self.lowerExprSlice(self.solved.lifted.exprSpan(args_span));
        defer self.allocator.free(args);
        return try self.program.addExprSpan(args);
    }

    fn lowerValueCall(self: *Lowerer, ty: Type.TypeId, call: anytype) Allocator.Error!Ast.ExprData {
        const callee = try self.lowerExpr(call.callee);
        const callee_ty = self.program.exprs.items[@intFromEnum(callee)].ty;
        const args = try self.lowerExprSlice(self.solved.lifted.exprSpan(call.args));
        defer self.allocator.free(args);

        return switch (self.program.types.get(callee_ty)) {
            .callable => |variants| blk: {
                const branches = try self.allocator.alloc(Ast.Branch, variants.len);
                defer self.allocator.free(branches);
                for (self.program.types.fnVariantSpan(variants), 0..) |variant, i| {
                    const payload_pat = if (variant.capture_ty) |capture_ty| blk_payload: {
                        const local = try self.program.addLocal(self.symbols.fresh(), capture_ty);
                        break :blk_payload try self.program.addPat(.{ .ty = capture_ty, .data = .{ .bind = local } });
                    } else null;

                    const pat = try self.program.addPat(.{
                        .ty = callee_ty,
                        .data = .{ .callable = .{
                            .variant = variant.id,
                            .payload = payload_pat,
                        } },
                    });

                    const call_args = try self.allocator.alloc(Ast.ExprId, args.len + if (payload_pat != null) @as(usize, 1) else 0);
                    defer self.allocator.free(call_args);
                    @memcpy(call_args[0..args.len], args);
                    if (payload_pat) |pat_id| {
                        const bind_local = switch (self.program.pats.items[@intFromEnum(pat_id)].data) {
                            .bind => |local| local,
                            else => unreachable,
                        };
                        call_args[args.len] = try self.program.addExpr(.{
                            .ty = variant.capture_ty.?,
                            .data = .{ .local = bind_local },
                        });
                    }

                    branches[i] = .{
                        .pat = pat,
                        .body = try self.program.addExpr(.{
                            .ty = ty,
                            .data = .{ .direct_call = .{
                                .target = .{ .local = variant.target },
                                .args = try self.program.addExprSpan(call_args),
                            } },
                        }),
                    };
                }

                break :blk .{ .match_ = .{
                    .scrutinee = callee,
                    .branches = try self.program.addBranchSpan(branches),
                    .comptime_site = null,
                } };
            },
            .erased_fn => .{ .indirect_erased_call = .{
                .callee = callee,
                .args = try self.program.addExprSpan(args),
            } },
            else => Common.invariant("value call callee had no callable Lambda Mono representation"),
        };
    }

    fn buildCaptureRecordFromExprs(
        self: *Lowerer,
        capture_span: SolvedType.Span,
        capture_operands: []const Lifted.CaptureOperand,
        capture_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const captures = self.solved.types.captureSpan(capture_span);
        const fields = switch (self.program.types.get(capture_ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            else => Common.invariant("callable capture payload was not a capture record"),
        };
        if (captures.len != fields.len) Common.invariant("callable capture payload arity differed from captured locals");
        if (captures.len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }

        // Member captures, capture-record fields, and keyed operands are all in
        // ascending CaptureId order, so the join is an exact indexed walk.
        const values = try self.allocator.alloc(Ast.ExprId, captures.len);
        defer self.allocator.free(values);
        for (captures, fields, capture_operands, 0..) |capture, field, operand, i| {
            if (capture.capture_id != field.capture_id) {
                Common.invariant("callable capture payload fields differed from captured locals");
            }
            const capture_id = capture.capture_id orelse Common.invariant("member capture had no CaptureId");
            if (operand.id != capture_id) {
                Common.invariant("capture operand CaptureId did not match its member capture slot");
            }
            values[i] = try self.lowerExpr(operand.value);
        }
        return try self.program.addExpr(.{
            .ty = capture_ty,
            .data = .{ .capture_record = try self.program.addExprSpan(values) },
        });
    }

    fn lowerPat(self: *Lowerer, pat_id: Lifted.PatId) Allocator.Error!Ast.PatId {
        const index = @intFromEnum(pat_id);
        if (self.pat_map[index]) |cached| return cached;
        const pat = self.solved.lifted.pats[index];
        const ty = try self.lowerTypeForSolvedPat(pat_id);
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = try self.localFor(local, ty) },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .local = try self.localFor(as.local, ty),
            } },
            .record => |fields| .{ .record = try self.lowerRecordDestructSpan(fields) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpan(items) },
            .list => |list| .{ .list = .{
                .patterns = try self.lowerPatSpan(list.patterns),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |rest_pattern| try self.lowerPat(rest_pattern) else null,
                } else null,
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerPatSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerPat(backing) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .str_pattern => |str| .{ .str_pattern = try self.lowerStrPattern(str) },
        };
        const lowered = try self.program.addPat(.{ .ty = ty, .data = data });
        self.pat_map[index] = lowered;
        return lowered;
    }

    fn lowerStrPattern(self: *Lowerer, str: Lifted.StrPattern) Allocator.Error!Ast.StrPattern {
        const input_steps = self.solved.lifted.strPatternStepSpan(str.steps);
        const steps = try self.allocator.alloc(Ast.StrPatternStep, input_steps.len);
        defer self.allocator.free(steps);

        for (input_steps, 0..) |step, i| {
            steps[i] = .{
                .capture = if (step.capture) |capture| try self.lowerPat(capture) else null,
                .delimiter = step.delimiter,
            };
        }

        return .{
            .prefix = str.prefix,
            .steps = try self.program.addStrPatternStepSpan(steps),
            .end = str.end,
        };
    }

    fn lowerStmt(self: *Lowerer, stmt_id: Lifted.StmtId) Allocator.Error!Ast.StmtId {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_map[index]) |cached| return cached;
        const saved_loc = self.program.current_loc;
        defer self.program.current_loc = saved_loc;
        const saved_region = self.program.current_region;
        defer self.program.current_region = saved_region;
        const stmt_loc = self.solved.lifted.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) self.program.current_loc = stmt_loc;
        const stmt_region = self.solved.lifted.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) self.program.current_region = stmt_region;
        const lowered_stmt: Ast.Stmt = switch (self.solved.lifted.stmts[index]) {
            .uninitialized => |pat| .{ .uninitialized = try self.lowerPat(pat) },
            .let_ => |let_| .{ .let_ = .{
                .pat = try self.lowerPat(let_.pat),
                .value = try self.lowerExpr(let_.value),
                .recursive = let_.recursive,
                .comptime_site = if (let_.comptime_site) |site| try self.lowerComptimeSite(site) else null,
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .expect => |expr| if (self.inline_expects == .omit)
                .{ .expr = try self.unitExpr() }
            else
                .{ .expect = try self.lowerExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.lowerExpr(expr) },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.value) },
            .crash => |msg| .{ .crash = msg },
        };
        const lowered = try self.program.addStmt(lowered_stmt);
        self.stmt_map[index] = lowered;
        return lowered;
    }

    fn unitExpr(self: *Lowerer) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = try self.unitType(),
            .data = .unit,
        });
    }

    fn unitType(self: *Lowerer) Allocator.Error!Type.TypeId {
        if (self.unit_ty) |ty| return ty;
        const ty = try self.program.types.add(.zst);
        self.unit_ty = ty;
        return ty;
    }

    fn lowerExprTy(self: *Lowerer, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeId {
        return try self.lowerType(self.solved.expr_tys[@intFromEnum(expr_id)]);
    }

    fn lowerTypeForSolvedPat(self: *Lowerer, pat_id: Lifted.PatId) Allocator.Error!Type.TypeId {
        return try self.lowerType(self.solved.pat_tys[@intFromEnum(pat_id)]);
    }

    fn lowerType(self: *Lowerer, solved_ty: SolvedType.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.solved.types.root(solved_ty);
        if (self.type_map.get(root)) |cached| return cached;

        const content = self.solved.types.get(root);
        if (content == .func) {
            const reserved = try self.program.types.add(.zst);
            try self.type_map.put(root, reserved);
            errdefer {
                if (self.type_map.get(root) == reserved) _ = self.type_map.remove(root);
            }
            self.program.types.set(reserved, try self.lowerCallableForFn(content.func.callable, root));
            return reserved;
        }

        const reserved = try self.program.types.add(.zst);
        try self.type_map.put(root, reserved);
        self.program.types.set(reserved, try self.lowerTypeContent(content));
        return reserved;
    }

    fn lowerTypeContent(self: *Lowerer, content: SolvedType.Content) Allocator.Error!Type.Content {
        return switch (content) {
            .link => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved link"),
            .unbound, .forall => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved type"),
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |erased| .{ .erased_fn = .{
                .source_fn_ty = erased.source_fn_ty,
                .members = try self.lowerFnMembersFromOwnTypes(erased.members, .erased),
            } },
            .func => Common.invariant("function type reached content lowering without its call signature"),
            .list => |elem| .{ .list = try self.lowerType(elem) },
            .box => |elem| .{ .box = try self.lowerType(elem) },
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSpan(self.solved.types.span(items));
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.program.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.allocator.alloc(Type.Field, fields.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.fieldSpan(fields), 0..) |field, i| {
                    lowered[i] = .{ .name = field.name, .ty = try self.lowerType(field.ty) };
                }
                break :blk .{ .record = try self.program.types.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try self.allocator.alloc(Type.Tag, tags.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solved.types.span(tag.payloads));
                    defer self.allocator.free(payloads);
                    lowered[i] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.program.types.addSpan(payloads),
                    };
                }
                break :blk .{ .tag_union = try self.program.types.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = try self.lowerTypeSpan(self.solved.types.span(named.args));
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.program.types.addSpan(args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.lowerType(backing.ty),
                        .use = backing.use,
                    } else null,
                    .declared_order = try self.lowerDeclaredOrder(named.declared_order),
                } };
            },
            .lambda_set => |members| .{ .callable = try self.lowerFnMembersFromOwnTypes(members, .finite) },
        };
    }

    fn lowerCallableForFn(
        self: *Lowerer,
        callable: SolvedType.TypeVarId,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Allocator.Error!Type.Content {
        return switch (self.solved.types.rootContent(callable)) {
            .lambda_set => |members| .{ .callable = try self.lowerFnMembers(members, .finite, solved_fn_ty) },
            .erased => |erased| .{ .erased_fn = .{
                .source_fn_ty = erased.source_fn_ty,
                .members = try self.lowerFnMembers(erased.members, .erased, solved_fn_ty),
            } },
            else => Common.invariant("function callable slot was unresolved before Lambda Mono"),
        };
    }

    /// Re-materializes a nominal record's declared field order from the Lambda
    /// Solved store into the Lambda Mono store. Named entries copy the shared
    /// field-name id; padding entries re-lower their reserved type.
    fn lowerDeclaredOrder(self: *Lowerer, span: SolvedType.Span) Allocator.Error!Type.Span {
        const source = self.solved.types.declaredFieldSpan(span);
        if (source.len == 0) return Type.Span.empty();
        const lowered = try self.allocator.alloc(Type.DeclaredField, source.len);
        defer self.allocator.free(lowered);
        for (source, 0..) |entry, i| {
            lowered[i] = switch (entry) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.lowerType(ty) },
            };
        }
        return try self.program.types.addDeclaredFields(lowered);
    }

    fn lowerFnMembers(
        self: *Lowerer,
        members: SolvedType.Span,
        abi: CaptureAbi,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Allocator.Error!Type.Span {
        const solved_members = self.solved.types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                root_fn_ty,
                abi,
                member.captures,
            );
            variants[i] = .{
                .id = undefined, // assigned by addFnVariants before the variant is stored
                .source = member.lambda,
                .target = target,
                .capture_ty = self.fn_specs.items[@intFromEnum(target)].capture_ty,
            };
        }
        return try self.program.types.addFnVariants(variants);
    }

    fn lowerFnMembersFromOwnTypes(self: *Lowerer, members: SolvedType.Span, abi: CaptureAbi) Allocator.Error!Type.Span {
        const solved_members = self.solved.types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                self.solved.types.root(self.solved.fn_tys[@intFromEnum(source)]),
                abi,
                member.captures,
            );
            variants[i] = .{
                .id = undefined, // assigned by addFnVariants before the variant is stored
                .source = member.lambda,
                .target = target,
                .capture_ty = self.fn_specs.items[@intFromEnum(target)].capture_ty,
            };
        }
        return try self.program.types.addFnVariants(variants);
    }

    fn captureRecordType(
        self: *Lowerer,
        captures: SolvedType.Span,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Allocator.Error!Type.TypeId {
        const id = CaptureTypeId.from(captures, self.solved.types.root(solved_fn_ty));
        if (self.capture_types.get(id)) |existing| return existing;

        const capture_items = self.solved.types.captureSpan(captures);
        const fields = try self.allocator.alloc(Type.CaptureField, capture_items.len);
        defer self.allocator.free(fields);
        for (capture_items, 0..) |capture, i| {
            fields[i] = .{
                .symbol = capture.symbol,
                .binder = capture.binder,
                .capture_id = capture.capture_id,
                .ty = try self.lowerType(capture.ty),
            };
        }
        const ty = try self.program.types.add(.{ .capture_record = try self.program.types.addCaptureFields(fields) });
        try self.capture_types.put(id, ty);
        return ty;
    }

    fn lowerTypeSpan(self: *Lowerer, items: []const SolvedType.TypeVarId) Allocator.Error![]Type.TypeId {
        const lowered = try self.allocator.alloc(Type.TypeId, items.len);
        errdefer self.allocator.free(lowered);
        for (items, 0..) |item, i| lowered[i] = try self.lowerType(item);
        return lowered;
    }

    fn localFor(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Allocator.Error!Ast.LocalId {
        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| return existing;
        const lifted_local = self.solved.lifted.locals[index];
        const lowered = try self.program.addLocalWithBinder(lifted_local.symbol, ty, lifted_local.binder);
        try self.program.setLocalName(lowered, self.solved.lifted.localName(@enumFromInt(index)));
        self.local_map[index] = lowered;
        return lowered;
    }

    fn lowerExprSpan(self: *Lowerer, span: Lifted.Span(Lifted.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const lowered = try self.lowerExprSlice(self.solved.lifted.exprSpan(span));
        defer self.allocator.free(lowered);
        return try self.program.addExprSpan(lowered);
    }

    fn lowerExprSlice(self: *Lowerer, exprs: []const Lifted.ExprId) Allocator.Error![]Ast.ExprId {
        const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
        errdefer self.allocator.free(lowered);
        for (exprs, 0..) |expr, i| lowered[i] = try self.lowerExpr(expr);
        return lowered;
    }

    fn lowerPatSpan(self: *Lowerer, span: Lifted.Span(Lifted.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.solved.lifted.patSpan(span);
        const lowered = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerPat(item);
        return try self.program.addPatSpan(lowered);
    }

    fn lowerStmtSpan(self: *Lowerer, span: Lifted.Span(Lifted.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.solved.lifted.stmtSpan(span);
        const lowered = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerStmt(item);
        return try self.program.addStmtSpan(lowered);
    }

    fn lowerTypedLocalSpan(self: *Lowerer, span: Lifted.Span(Lifted.TypedLocal)) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const input_items = self.solved.lifted.typedLocalSpan(span);
        const lowered = try self.allocator.alloc(Ast.TypedLocal, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| {
            const lifted_local = self.solved.lifted.locals[@intFromEnum(item.local)];
            const ty = try self.lowerTypeByLiftedLocal(lifted_local.id);
            lowered[i] = .{ .local = try self.localFor(item.local, ty), .ty = ty };
        }
        return try self.program.addTypedLocalSpan(lowered);
    }

    fn lowerTypeByLiftedLocal(self: *Lowerer, local: Lifted.LocalId) Allocator.Error!Type.TypeId {
        const mapped = self.local_map[@intFromEnum(local)] orelse {
            return try self.lowerType(self.solved.local_tys[@intFromEnum(local)]);
        };
        return self.program.locals.items[@intFromEnum(mapped)].ty;
    }

    fn lowerFieldExprSpan(self: *Lowerer, span: Lifted.Span(Lifted.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const input_items = self.solved.lifted.fieldExprSpan(span);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.program.addFieldExprSpan(lowered);
    }

    fn lowerRecordDestructSpan(self: *Lowerer, span: Lifted.Span(Lifted.RecordDestruct)) Allocator.Error!Ast.Span(Ast.RecordDestruct) {
        const input_items = self.solved.lifted.recordDestructSpan(span);
        const lowered = try self.allocator.alloc(Ast.RecordDestruct, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.program.addRecordDestructSpan(lowered);
    }

    fn lowerBranchSpan(self: *Lowerer, span: Lifted.Span(Lifted.Branch)) Allocator.Error!Ast.Span(Ast.Branch) {
        const input_items = self.solved.lifted.branchSpan(span);
        const lowered = try self.allocator.alloc(Ast.Branch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .pat = try self.lowerPat(branch.pat),
                .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.program.addBranchSpan(lowered);
    }

    fn lowerIfBranchSpan(self: *Lowerer, span: Lifted.Span(Lifted.IfBranch)) Allocator.Error!Ast.Span(Ast.IfBranch) {
        const input_items = self.solved.lifted.ifBranchSpan(span);
        const lowered = try self.allocator.alloc(Ast.IfBranch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .cond = try self.lowerExpr(branch.cond),
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.program.addIfBranchSpan(lowered);
    }
};

test "lambda mono lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
