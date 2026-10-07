//! Lambda Solved IR to Lambda Mono IR.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Solved = @import("../lambda_solved/ast.zig");
const SolvedType = @import("../lambda_solved/type.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// Whether inline expects should materialize during lowering.
pub const InlineExpectMode = enum {
    run,
    omit,
};

/// Producer-owned identity of one Lambda Mono function specialization. Debug
/// cross-checks use this exact key instead of reconstructing correspondence
/// from independently allocated Monotype shapes.
pub const SpecializationCaptureAbi = enum {
    finite,
    erased,
};

/// Which Lambda Mono function owns the capture ABI recorded for a specialization.
pub const SpecializationCaptureSource = enum {
    solved,
    own,
};

/// Exact producer data used to compare Lambda Mono specialization identities.
pub const SpecializationIdentity = struct {
    source: Lifted.FnId,
    solved_fn_ty: SolvedType.TypeVarId,
    abi: SpecializationCaptureAbi,
    captures_source: SpecializationCaptureSource,
    captures_start: u32,
    captures_len: u32,
};

/// Options used by Lambda Mono lowering.
pub const Options = struct {
    inline_expects: InlineExpectMode = .run,
    debug_specialization_identities: ?*std.ArrayList(SpecializationIdentity) = null,
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

    var string_literals = owned.lifted.takeStringLiterals();
    var const_fn_evidence = owned.lifted.const_fn_evidence.takeArrayList();
    var const_fn_evidence_frames = owned.lifted.const_fn_evidence_frames.takeArrayList();
    var name_store = owned.lifted.names;
    owned.lifted.names = @import("check").CheckedNames.NameStore.init(allocator);
    var program = Ast.Program.init(allocator, name_store, string_literals, const_fn_evidence, const_fn_evidence_frames);
    name_store = undefined;
    string_literals = undefined;
    const_fn_evidence = undefined;
    const_fn_evidence_frames = undefined;
    program.source_files = Ast.ProgramList(base.SourceFileEntry, "source_files").fromArrayList(owned.lifted.takeSourceFiles());
    program.static_data_values = Ast.ProgramList(Ast.StaticDataValue, "static_data_values").fromArrayList(owned.lifted.takeStaticDataValues());
    errdefer program.deinit();

    const solved_view = movedSolvedView(&owned, &program);
    // Expressions keep their Lifted-domain IDs, but debug evaluation must not
    // borrow descriptors from the consumed Solved program.
    try program.comptime_value_roots.appendSlice(allocator, solved_view.lifted.comptime_value_roots);
    try program.field_access_segments.ensureUnusedCapacity(allocator, solved_view.lifted.field_access_segments.len);
    for (solved_view.lifted.field_access_segments) |segment| {
        program.field_access_segments.appendAssumeCapacity(.{ .field = segment.field });
    }
    var lowerer = try Lowerer.init(allocator, solved_view, &program, options);
    defer lowerer.deinit();
    defer lowerer.folded_matches.deinit(allocator);
    for (folded_matches) |folded| {
        try lowerer.folded_matches.put(allocator, folded.scrutinee, folded.body);
    }
    try lowerer.lower();
    if (options.debug_specialization_identities) |identities| {
        if (identities.items.len != program.fnCount()) {
            Common.invariant("Lambda Mono debug specialization identities diverged from the function table");
        }
    }
    program.next_symbol = lowerer.symbols.next;

    owned.deinit();
    return program;
}

fn movedSolvedView(source: *const Solved.Program, moved: *const Ast.Program) Solved.ProgramView {
    const lifted = source.lifted.view();
    return .{
        .lifted = .{
            .names = &moved.names,
            .next_symbol = lifted.next_symbol,
            .types = lifted.types,
            .const_fn_evidence = moved.const_fn_evidence.unsafeRawItemsForView(),
            .const_fn_evidence_frames = moved.const_fn_evidence_frames.unsafeRawItemsForView(),
            .fns = lifted.fns,
            .exprs = lifted.exprs,
            .pats = lifted.pats,
            .stmts = lifted.stmts,
            .locals = lifted.locals,
            .expr_ids = lifted.expr_ids,
            .pat_ids = lifted.pat_ids,
            .typed_locals = lifted.typed_locals,
            .stmt_ids = lifted.stmt_ids,
            .field_exprs = lifted.field_exprs,
            .field_access_segments = lifted.field_access_segments,
            .fn_def_captures = lifted.fn_def_captures,
            .capture_operands = lifted.capture_operands,
            .record_destructs = lifted.record_destructs,
            .str_pattern_steps = lifted.str_pattern_steps,
            .branches = lifted.branches,
            .if_branches = lifted.if_branches,
            .string_literals = moved.string_literals.unsafeRawItemsForView(),
            .proc_debug_names = lifted.proc_debug_names,
            .roots = lifted.roots,
            .literal_roots = lifted.literal_roots,
            .layout_requests = lifted.layout_requests,
            .comptime_value_reads = lifted.comptime_value_reads,
            .runtime_schema_requests = lifted.runtime_schema_requests,
            .static_data_values = moved.static_data_values.unsafeRawItemsForView(),
            .comptime_sites = lifted.comptime_sites,
            .comptime_value_roots = lifted.comptime_value_roots,
            .lowering_modules = lifted.lowering_modules,
            .platform_requirement_filling = lifted.platform_requirement_filling,
            .source_files = moved.source_files.unsafeRawItemsForView(),
            .expr_locs = lifted.expr_locs,
            .expr_regions = lifted.expr_regions,
            .stmt_locs = lifted.stmt_locs,
            .stmt_regions = lifted.stmt_regions,
            .inline_scopes = lifted.inline_scopes,
            .expr_inline_scopes = lifted.expr_inline_scopes,
            .stmt_inline_scopes = lifted.stmt_inline_scopes,
            .local_names = lifted.local_names,
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
    storage_ty: Type.TypeId,
};

const CaptureAbi = SpecializationCaptureAbi;
const CaptureSpanSource = SpecializationCaptureSource;

const CaptureSpanId = struct {
    source: CaptureSpanSource,
    start: u32,
    len: u32,

    fn fromSolved(span: SolvedType.Span) CaptureSpanId {
        return .{ .source = .solved, .start = span.start, .len = span.len };
    }

    fn fromOwn(start: u32, len: u32) CaptureSpanId {
        return .{ .source = .own, .start = start, .len = len };
    }
};

/// A capture span identifies its captures structurally (source, start and
/// length) rather than as a dense index, so it is a hashable key.
const CaptureSpanKey = CaptureSpanId;

fn specializationIdentityCaptureStart(span: CaptureSpanId) u32 {
    return switch (span.source) {
        .solved => span.start,
        .own => 0,
    };
}

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
        std.hash.autoHash(&hasher, @backingInt(spec.source));
        std.hash.autoHash(&hasher, @backingInt(spec.solved_fn_ty));
        std.hash.autoHash(&hasher, spec.abi);
        std.hash.autoHash(&hasher, spec.captures.source);
        std.hash.autoHash(&hasher, spec.captures.start);
        std.hash.autoHash(&hasher, spec.captures.len);
        if (spec.capture_ty) |capture_ty| {
            std.hash.autoHash(&hasher, @backingInt(capture_ty));
        } else {
            std.hash.autoHash(&hasher, @as(u32, std.math.maxInt(u32)));
        }
        return hasher.final();
    }

    pub fn eql(_: FnSpecContext, lhs: FnSpec, rhs: FnSpec) bool {
        return lhs.source == rhs.source and
            lhs.solved_fn_ty == rhs.solved_fn_ty and
            lhs.abi == rhs.abi and
            lhs.captures.source == rhs.captures.source and
            lhs.captures.start == rhs.captures.start and
            lhs.captures.len == rhs.captures.len and
            lhs.capture_ty == rhs.capture_ty;
    }
};

const Lowerer = struct {
    allocator: Allocator,
    solved: Solved.ProgramView,
    program: *Ast.Program,
    type_map: collections.DenseMap(SolvedType.TypeVarId, Type.TypeId),
    local_map: []?Ast.LocalId,
    expr_map: []?Ast.ExprId,
    pat_map: []?Ast.PatId,
    stmt_map: []?Ast.StmtId,
    comptime_site_map: []?Ast.ComptimeSiteId,
    fn_specs: std.ArrayList(FnSpec),
    fn_spec_map: std.HashMap(FnSpec, Ast.FnId, FnSpecContext, std.hash_map.default_max_load_percentage),
    fn_written: std.ArrayList(bool),
    source_symbols: std.AutoHashMap(Common.Symbol, Lifted.FnId),
    /// Lowered capture record of every capture span seen so far. A capture
    /// record depends only on its captures, so one record serves every
    /// function type that carries the same span.
    capture_types: std.AutoHashMap(CaptureSpanKey, Type.TypeId),
    captures: collections.DenseMap(Lifted.LocalId, CaptureBinding),
    own_captures: std.ArrayList(SolvedType.Capture),
    own_capture_spans: []?CaptureSpanId,
    symbols: Common.SymbolGen,
    erased_capture_ptr_ty: ?Type.TypeId = null,
    unit_ty: ?Type.TypeId = null,
    inline_expects: InlineExpectMode,
    debug_specialization_identities: ?*std.ArrayList(SpecializationIdentity),
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
        if (options.debug_specialization_identities) |identities| {
            if (identities.items.len != 0) {
                Common.invariant("Lambda Mono debug specialization identity output was not empty");
            }
        }
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

        const own_capture_spans = try allocator.alloc(?CaptureSpanId, solved.lifted.fns.len);
        errdefer allocator.free(own_capture_spans);
        @memset(own_capture_spans, null);

        return .{
            .allocator = allocator,
            .solved = solved,
            .program = program,
            .type_map = collections.DenseMap(SolvedType.TypeVarId, Type.TypeId).init(allocator),
            .local_map = local_map,
            .expr_map = expr_map,
            .pat_map = pat_map,
            .stmt_map = stmt_map,
            .comptime_site_map = comptime_site_map,
            .fn_specs = .empty,
            .fn_spec_map = std.HashMap(FnSpec, Ast.FnId, FnSpecContext, std.hash_map.default_max_load_percentage).initContext(allocator, .{}),
            .fn_written = .empty,
            .source_symbols = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator),
            .capture_types = std.AutoHashMap(CaptureSpanKey, Type.TypeId).init(allocator),
            .captures = collections.DenseMap(Lifted.LocalId, CaptureBinding).init(allocator),
            .own_captures = .empty,
            .own_capture_spans = own_capture_spans,
            .symbols = .{ .next = solved.lifted.next_symbol },
            .inline_expects = options.inline_expects,
            .debug_specialization_identities = options.debug_specialization_identities,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.captures.deinit();
        self.capture_types.deinit();
        self.allocator.free(self.own_capture_spans);
        self.own_captures.deinit(self.allocator);
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
                .owner = root.owner,
            });
        }

        try self.program.literal_roots.ensureTotalCapacity(self.allocator, self.solved.lifted.literal_roots.len);
        for (self.solved.lifted.literal_roots) |root| {
            try self.program.literal_roots.append(self.allocator, .{
                .fn_id = try self.ensureOwnFnSpec(root.fn_id, .finite),
                .module = root.module,
                .site = root.site,
            });
        }

        try self.program.layout_requests.ensureTotalCapacity(self.allocator, self.solved.layout_requests.len);
        for (self.solved.layout_requests) |request| {
            try self.program.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = try self.lowerType(request.ty),
                .initializer = if (request.fn_id) |fn_id| try self.ensureOwnFnSpec(fn_id, .finite) else null,
                .const_locator = request.const_locator,
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
            const fn_id: Lifted.FnId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
            const result = try self.source_symbols.getOrPut(fn_.symbol);
            if (result.found_existing) Common.invariant("two lifted functions had the same symbol");
            result.value_ptr.* = fn_id;
        }
    }

    fn lowerQueuedFns(self: *Lowerer) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.fn_specs.items.len) : (index += 1) {
            if (self.fn_written.items[index]) continue;
            const out_id: Ast.FnId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
            try self.lowerFnSpec(out_id, self.fn_specs.items[index]);
        }
    }

    fn lowerFnSpec(self: *Lowerer, out_id: Ast.FnId, spec: FnSpec) Allocator.Error!void {
        const fn_id = spec.source;
        const fn_ = self.solved.lifted.fns[@backingInt(fn_id)];
        self.captures.clearRetainingCapacity();
        @memset(self.local_map, null);
        @memset(self.expr_map, null);
        @memset(self.pat_map, null);
        @memset(self.stmt_map, null);

        const solved_fn_ty = spec.solved_fn_ty;
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .lambda_set, .erased, .zst, .mono => Common.invariant("Lambda Mono function table contains a non-function type"),
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
        const symbol = self.program.getFn(out_id).symbol;
        self.program.setFn(out_id, .{
            .symbol = symbol,
            .source = fn_.source,
            .args = args_span,
            .body = body,
            .ret = ret,
        });
        self.fn_written.items[@backingInt(out_id)] = true;
    }

    fn ensureOwnFnSpec(self: *Lowerer, fn_id: Lifted.FnId, abi: CaptureAbi) Allocator.Error!Ast.FnId {
        return (try self.runFrames(try self.ownFnSpecTask(fn_id, abi))).get(.fn_id);
    }

    fn sourceFnForSymbol(self: *Lowerer, symbol: Common.Symbol) Lifted.FnId {
        return self.source_symbols.get(symbol) orelse
            Common.invariant("Lambda Mono callable member referenced a missing lifted function symbol");
    }

    fn captureSpan(self: *const Lowerer, span: CaptureSpanId) []const SolvedType.Capture {
        return switch (span.source) {
            .solved => self.solved.types.captureSpan(.{ .start = span.start, .len = span.len }),
            .own => self.own_captures.items[span.start..][0..span.len],
        };
    }

    fn ownCaptureSpanForFn(self: *Lowerer, fn_id: Lifted.FnId) Allocator.Error!CaptureSpanId {
        const raw_fn = @backingInt(fn_id);
        if (raw_fn >= self.own_capture_spans.len) Common.invariant("own capture span requested for a missing lifted function");
        if (self.own_capture_spans[raw_fn]) |existing| return existing;

        const fn_ = self.solved.lifted.fns[raw_fn];
        const capture_locals = self.solved.lifted.typedLocalSpan(fn_.captures);
        if (capture_locals.len == 0) {
            const empty = CaptureSpanId.fromOwn(0, 0);
            self.own_capture_spans[raw_fn] = empty;
            return empty;
        }

        const start: u32 = @intCast(self.own_captures.items.len);
        errdefer self.own_captures.shrinkRetainingCapacity(@intCast(start));

        try self.own_captures.ensureUnusedCapacity(self.allocator, capture_locals.len);
        for (capture_locals) |capture| {
            const local = self.solved.lifted.locals[@backingInt(capture.local)];
            self.own_captures.appendAssumeCapacity(.{
                .local = capture.local,
                .symbol = local.symbol,
                .binder = local.binder,
                .capture_id = local.capture_id,
                .checked_capture_id = local.checked_capture_id,
                .ty = self.solved.local_tys[@backingInt(capture.local)],
            });
        }

        const span = CaptureSpanId.fromOwn(start, @intCast(capture_locals.len));
        self.own_capture_spans[raw_fn] = span;
        return span;
    }

    fn bindCaptureRecord(self: *Lowerer, captures_id: CaptureSpanId, capture_ty: Type.TypeId, capture_expr: Ast.ExprId) Allocator.Error!void {
        const captures = self.captureSpan(captures_id);
        const fields = switch (self.program.types.get(capture_ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            .primitive, .named, .record, .tuple, .tag_union, .callable, .list, .box, .erased_fn, .erased_capture_ptr, .zst => Common.invariant("function capture argument was not a capture record"),
        };
        if (captures.len != fields.len) Common.invariant("function capture argument arity differed from capture slots");

        for (0..captures.len) |index| {
            const capture = captures[index];
            const field = GuardedList.at(fields, index);
            if (capture.capture_id != field.capture_id) {
                Common.invariant("function capture argument fields differed from capture slots");
            }
            try self.captures.put(capture.local, .{
                .record = capture_expr,
                .symbol = capture.symbol,
                .ty = field.ty,
                .storage_ty = field.storage_ty,
            });
        }
    }

    fn capturesForFn(self: *Lowerer, fn_id: Lifted.FnId) Allocator.Error!CaptureSpanId {
        return try self.ownCaptureSpanForFn(fn_id);
    }

    fn erasedCapturePtrType(self: *Lowerer) Allocator.Error!Type.TypeId {
        if (self.erased_capture_ptr_ty) |ty| return ty;
        const ty = try self.program.types.add(.erased_capture_ptr);
        self.erased_capture_ptr_ty = ty;
        return ty;
    }

    // Lowering //
    //
    // Lowering an expression, statement, pattern, or type lowers its parts
    // from inside its own lowering. Every such computation suspends as a
    // `Frame` on one heap-backed stack while a part lowers, so nesting never
    // becomes native call depth. A frame issues its parts in the order a
    // direct recursive lowering evaluated them, so every id is allocated in
    // the same order.

    const Task = union(enum) {
        expr: ExprTask,
        stmt: StmtTask,
        pat: PatTask,
        /// A sequence of parts, each lowered in order.
        seq: SeqTask,
        callable_value: CallableValueTask,
        direct_call_args: DirectCallArgsTask,
        value_call: ValueCallTask,
        capture_record_expr: CaptureRecordExprTask,
        /// A local's type lowered, then the local itself.
        typed_local: TypedLocalTask,
        type_var: TypeVarTask,
        fn_spec: FnSpecTask,
        capture_record_type: CaptureRecordTypeTask,
        members: MembersTask,
        declared_order: DeclaredOrderTask,
    };

    const Result = union(enum) {
        expr: Ast.ExprId,
        comptime_site: Ast.ComptimeSiteId,
        pat: Ast.PatId,
        stmt: Ast.StmtId,
        ty: Type.TypeId,
        fn_id: Ast.FnId,
        local: Ast.LocalId,
        type_span: Type.Span,
        expr_span: Ast.Span(Ast.ExprId),
        pat_span: Ast.Span(Ast.PatId),
        stmt_span: Ast.Span(Ast.StmtId),
        field_span: Ast.Span(Ast.FieldExpr),
        destruct_span: Ast.Span(Ast.RecordDestruct),
        branch_span: Ast.Span(Ast.Branch),
        if_branch_span: Ast.Span(Ast.IfBranch),
        typed_local_span: Ast.Span(Ast.TypedLocal),
        str_pattern: Ast.StrPattern,
        data: Ast.ExprData,
        /// Owned by the lowerer's allocator.
        slice: []Ast.ExprId,

        fn get(self: Result, comptime tag: std.meta.Tag(Result)) @FieldType(Result, @tagName(tag)) {
            if (std.meta.activeTag(self) != tag) Common.invariant("Lambda Mono lowering frame received the wrong result kind");
            return @field(self, @tagName(tag));
        }
    };

    const Frame = struct {
        cursor: u8 = 0,
        index: usize = 0,
        task: Task,
    };

    const Step = union(enum) {
        call: Task,
        ret: Result,
    };

    fn runFrames(self: *Lowerer, root: Task) Allocator.Error!Result {
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(self.allocator);
        errdefer for (frames.items) |*frame| self.releaseFrame(frame);
        try frames.append(self.allocator, .{ .task = root });
        var input: ?Result = null;
        while (true) {
            const frame = &frames.items[frames.items.len - 1];
            switch (try self.stepFrame(frame, input)) {
                .call => |task| {
                    try frames.append(self.allocator, .{ .task = task });
                    input = null;
                },
                .ret => |result| {
                    var finished = frames.pop().?;
                    self.releaseFrame(&finished);
                    if (frames.items.len == 0) return result;
                    input = result;
                },
            }
        }
    }

    /// Free what a frame still owns.
    fn releaseFrame(self: *Lowerer, frame: *Frame) void {
        switch (frame.task) {
            inline .expr, .stmt, .pat => |*task| task.parts.deinit(self.allocator),
            .seq => |*task| task.results.deinit(self.allocator),
            .direct_call_args => |*task| {
                self.allocator.free(task.args);
                task.args = &.{};
            },
            .value_call => |*task| {
                self.allocator.free(task.args);
                task.args = &.{};
            },
            .capture_record_expr => |*task| task.values.deinit(self.allocator),
            .type_var => |*task| {
                task.tys.deinit(self.allocator);
                task.fields.deinit(self.allocator);
                task.tags.deinit(self.allocator);
            },
            .capture_record_type => |*task| task.fields.deinit(self.allocator),
            .members => |*task| task.variants.deinit(self.allocator),
            .declared_order => |*task| task.lowered.deinit(self.allocator),
            .callable_value, .typed_local, .fn_spec => {},
        }
    }

    fn stepFrame(self: *Lowerer, frame: *Frame, input: ?Result) Allocator.Error!Step {
        return switch (frame.task) {
            .expr => |*task| self.stepExpr(frame, task, input),
            .stmt => |*task| self.stepStmt(frame, task, input),
            .pat => |*task| self.stepPat(frame, task, input),
            .seq => |*task| self.stepSeq(frame, task, input),
            .callable_value => |*task| self.stepCallableValue(frame, task, input),
            .direct_call_args => |*task| self.stepDirectCallArgs(frame, task, input),
            .value_call => |*task| self.stepValueCall(frame, task, input),
            .capture_record_expr => |*task| self.stepCaptureRecordExpr(frame, task, input),
            .typed_local => |*task| {
                if (frame.cursor == 0) {
                    if (task.known_ty) |ty| return .{ .ret = .{ .local = try self.localFor(task.local, ty) } };
                    frame.cursor = 1;
                    return .{ .call = .{ .type_var = .{ .var_id = self.solved.local_tys[@backingInt(task.local)] } } };
                }
                return .{ .ret = .{ .local = try self.localFor(task.local, input.?.ty) } };
            },
            .type_var => |*task| self.stepTypeVar(frame, task, input),
            .fn_spec => |*task| self.stepFnSpec(frame, task, input),
            .capture_record_type => |*task| self.stepCaptureRecordType(frame, task, input),
            .members => |*task| self.stepMembers(frame, task, input),
            .declared_order => |*task| self.stepDeclaredOrder(frame, task, input),
        };
    }

    // Wrappers for callers outside the lowering frames.

    fn lowerExpr(self: *Lowerer, expr_id: Lifted.ExprId) Allocator.Error!Ast.ExprId {
        return (try self.runFrames(.{ .expr = .{ .expr_id = expr_id } })).get(.expr);
    }

    fn lowerType(self: *Lowerer, solved_ty: SolvedType.TypeVarId) Allocator.Error!Type.TypeId {
        return (try self.runFrames(.{ .type_var = .{ .var_id = solved_ty } })).get(.ty);
    }

    /// The function specialization `ensureOwnFnSpec` selects, as a task.
    fn ownFnSpecTask(self: *Lowerer, fn_id: Lifted.FnId, abi: CaptureAbi) Allocator.Error!Task {
        const solved_fn_ty = self.solved.types.root(self.solved.fn_tys[@backingInt(fn_id)]);
        switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => {},
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .lambda_set, .erased, .zst, .mono => Common.invariant("Lambda Mono function table contains a non-function type"),
        }
        return .{ .fn_spec = .{ .source = fn_id, .solved_fn_ty = solved_fn_ty, .abi = abi, .captures = try self.ownCaptureSpanForFn(fn_id) } };
    }

    /// One part a node lowers, in evaluation order.
    const Part = union(enum) {
        expr: Lifted.ExprId,
        stmt: Lifted.StmtId,
        pat: Lifted.PatId,
        ty: SolvedType.TypeVarId,
        /// A local's type, then the local.
        local: Lifted.LocalId,
        /// A local lowered at an already-lowered type.
        local_at: struct { local: Lifted.LocalId, ty: Type.TypeId },
        comptime_site: Lifted.ComptimeSiteId,
        expr_span: Lifted.Span(Lifted.ExprId),
        field_span: Lifted.Span(Lifted.FieldExpr),
        pat_span: Lifted.Span(Lifted.PatId),
        stmt_span: Lifted.Span(Lifted.StmtId),
        typed_local_span: Lifted.Span(Lifted.TypedLocal),
        destruct_span: Lifted.Span(Lifted.RecordDestruct),
        branch_span: Lifted.Span(Lifted.Branch),
        if_branch_span: Lifted.Span(Lifted.IfBranch),
        str_pattern: Lifted.StrPattern,
        own_fn_spec: Lifted.FnId,
        callable_value: CallableValueTask,
        direct_call_args: struct { fn_id: Lifted.FnId, args: Lifted.Span(Lifted.ExprId), captures: Lifted.Span(Lifted.CaptureOperand) },
        value_call: struct { ty: Type.TypeId, callee: Lifted.ExprId, args: Lifted.Span(Lifted.ExprId) },
    };

    /// The parts one node lowers, in order.
    const Plan = struct {
        buf: [6]Part = undefined,
        len: u8 = 0,

        fn add(self: *Plan, part: Part) void {
            self.buf[self.len] = part;
            self.len += 1;
        }

        fn slice(self: *const Plan) []const Part {
            return self.buf[0..self.len];
        }
    };

    /// Lower `part` at once when it needs no frame, appending its result to
    /// `results`; otherwise the task that lowers it.
    fn partTask(self: *Lowerer, part: Part, results: *std.ArrayList(Result)) Allocator.Error!?Task {
        return switch (part) {
            .comptime_site => |site| {
                try results.append(self.allocator, .{ .comptime_site = try self.lowerComptimeSite(site) });
                return null;
            },
            .local_at => |local| {
                try results.append(self.allocator, .{ .local = try self.localFor(local.local, local.ty) });
                return null;
            },
            .expr => |expr_id| .{ .expr = .{ .expr_id = expr_id } },
            .stmt => |stmt_id| .{ .stmt = .{ .stmt_id = stmt_id } },
            .pat => |pat_id| .{ .pat = .{ .pat_id = pat_id } },
            .ty => |ty| .{ .type_var = .{ .var_id = ty } },
            .local => |local| .{ .typed_local = .{ .local = local } },
            .expr_span => |span| .{ .seq = .{ .kind = .{ .exprs = span } } },
            .field_span => |span| .{ .seq = .{ .kind = .{ .fields = span } } },
            .pat_span => |span| .{ .seq = .{ .kind = .{ .pats = span } } },
            .stmt_span => |span| .{ .seq = .{ .kind = .{ .stmts = span } } },
            .typed_local_span => |span| .{ .seq = .{ .kind = .{ .typed_locals = span } } },
            .destruct_span => |span| .{ .seq = .{ .kind = .{ .destructs = span } } },
            .branch_span => |span| .{ .seq = .{ .kind = .{ .branches = span } } },
            .if_branch_span => |span| .{ .seq = .{ .kind = .{ .if_branches = span } } },
            .str_pattern => |str| .{ .seq = .{ .kind = .{ .str_pattern = str } } },
            .own_fn_spec => |fn_id| try self.ownFnSpecTask(fn_id, .finite),
            .callable_value => |callable| .{ .callable_value = callable },
            .direct_call_args => |call| .{ .direct_call_args = .{ .fn_id = call.fn_id, .args_span = call.args, .captures_span = call.captures } },
            .value_call => |call| .{ .value_call = .{ .ty = call.ty, .callee = call.callee, .args_span = call.args } },
        };
    }

    /// Issue the next of `plan`, recording each result in `parts`; null
    /// once all are lowered.
    fn nextPart(self: *Lowerer, parts: *std.ArrayList(Result), plan: []const Part, input: ?Result) Allocator.Error!?Step {
        if (input) |result| try parts.append(self.allocator, result);
        while (parts.items.len < plan.len) {
            if (try self.partTask(plan[parts.items.len], parts)) |task| return .{ .call = task };
        }
        return null;
    }

    const ExprTask = struct {
        expr_id: Lifted.ExprId,
        saved_loc: base.SourceLoc = undefined,
        saved_region: base.Region = undefined,
        ty: Type.TypeId = undefined,
        plan: Plan = .{},
        parts: std.ArrayList(Result) = .empty,
    };

    fn stepExpr(self: *Lowerer, frame: *Frame, task: *ExprTask, input: ?Result) Allocator.Error!Step {
        const index = @backingInt(task.expr_id);
        const expr = self.solved.lifted.exprs[index];
        switch (frame.cursor) {
            0 => {
                if (self.expr_map[index]) |cached| return .{ .ret = .{ .expr = cached } };
                task.saved_loc = self.program.current_loc;
                task.saved_region = self.program.current_region;
                const expr_loc = self.solved.lifted.exprLoc(task.expr_id);
                if (expr_loc.hasLocation()) self.program.current_loc = expr_loc;
                const expr_region = self.solved.lifted.exprRegion(task.expr_id);
                if (!expr_region.isEmpty()) self.program.current_region = expr_region;
                frame.cursor = 1;
                return .{ .call = .{ .type_var = .{ .var_id = self.solved.expr_tys[index] } } };
            },
            1 => {
                task.ty = input.?.get(.ty);
                try self.planExprParts(task, expr);
                frame.cursor = 2;
                if (try self.nextPart(&task.parts, task.plan.slice(), null)) |step| return step;
            },
            else => if (try self.nextPart(&task.parts, task.plan.slice(), input)) |step| return step,
        }
        const data = try self.buildExprData(task, expr);
        const lowered = try self.program.addExpr(.{ .ty = task.ty, .data = data });
        self.expr_map[index] = lowered;
        self.program.current_loc = task.saved_loc;
        self.program.current_region = task.saved_region;
        return .{ .ret = .{ .expr = lowered } };
    }

    fn planExprParts(self: *Lowerer, task: *ExprTask, expr: Lifted.Expr) Allocator.Error!void {
        const plan = &task.plan;
        switch (expr.data) {
            .local,
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .@"unreachable",
            .uninitialized,
            .crash,
            .checked_error,
            => {},
            .comptime_value => |value| plan.add(.{ .expr = value.initializer }),
            .static_data_candidate => |candidate| plan.add(.{ .expr = candidate.runtime_expr }),
            .typed_boundary => |boundary| plan.add(.{ .expr = boundary.value }),
            .uninitialized_payload => |payload| plan.add(.{ .local = payload.condition }),
            .list, .tuple => |items| plan.add(.{ .expr_span = items }),
            .record => |fields| plan.add(.{ .field_span = fields }),
            .record_update => |update| {
                plan.add(.{ .expr = update.base });
                plan.add(.{ .field_span = update.fields });
            },
            .tag => |tag| plan.add(.{ .expr_span = tag.payloads }),
            .nominal => |backing| plan.add(.{ .expr = backing }),
            .let_ => |let_| {
                plan.add(.{ .pat = let_.bind });
                plan.add(.{ .expr = let_.value });
                plan.add(.{ .expr = let_.rest });
                if (let_.comptime_site) |site| plan.add(.{ .comptime_site = site });
            },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached Lambda Mono"),
            .fn_ref => |fn_ref| plan.add(.{ .callable_value = .{
                .expr_id = task.expr_id,
                .fn_id = fn_ref.fn_id,
                .captures_span = fn_ref.captures,
                .ty = task.ty,
            } }),
            .call_value => |call| plan.add(.{ .value_call = .{ .ty = task.ty, .callee = call.callee, .args = call.args } }),
            .call_proc => |call| switch (Lifted.directCallee(call)) {
                .local => |callee| {
                    plan.add(.{ .own_fn_spec = callee });
                    plan.add(.{ .direct_call_args = .{ .fn_id = callee, .args = call.args, .captures = call.captures } });
                },
            },
            .low_level => |call| plan.add(.{ .expr_span = call.args }),
            .field_access => |field| plan.add(.{ .expr = field.receiver }),
            .tuple_access => |access| plan.add(.{ .expr = access.tuple }),
            .structural_eq => |eq| {
                plan.add(.{ .expr = eq.lhs });
                plan.add(.{ .expr = eq.rhs });
            },
            .structural_hash => |h| {
                plan.add(.{ .expr = h.value });
                plan.add(.{ .expr = h.hasher });
            },
            .match_ => |match| {
                if (self.folded_matches.get(match.scrutinee)) |folded_body| {
                    plan.add(.{ .expr = folded_body });
                } else {
                    plan.add(.{ .expr = match.scrutinee });
                    plan.add(.{ .branch_span = match.branches });
                    if (match.comptime_site) |site| plan.add(.{ .comptime_site = site });
                }
            },
            .if_ => |if_| {
                plan.add(.{ .if_branch_span = if_.branches });
                plan.add(.{ .expr = if_.final_else });
            },
            .if_initialized_payload => |payload_switch| {
                plan.add(.{ .expr = payload_switch.cond });
                plan.add(.{ .local = payload_switch.payload });
                plan.add(.{ .expr = payload_switch.initialized });
                plan.add(.{ .expr = payload_switch.uninitialized });
            },
            .try_sequence => |sequence| {
                plan.add(.{ .expr = sequence.try_expr });
                plan.add(.{ .local = sequence.ok_local });
                plan.add(.{ .expr = sequence.ok_body });
            },
            .try_record_sequence => |sequence| {
                plan.add(.{ .expr = sequence.try_expr });
                plan.add(.{ .local = sequence.value_local });
                plan.add(.{ .local = sequence.rest_local });
                plan.add(.{ .expr = sequence.ok_body });
            },
            .block => |block| {
                plan.add(.{ .stmt_span = block.statements });
                plan.add(.{ .expr = block.final_expr });
            },
            .loop_ => |loop| {
                plan.add(.{ .typed_local_span = loop.params });
                plan.add(.{ .expr_span = loop.initial_values });
                plan.add(.{ .expr = loop.body });
            },
            .break_ => |maybe| if (maybe) |value| plan.add(.{ .expr = value }),
            .continue_ => |continue_| plan.add(.{ .expr_span = continue_.values }),
            .join_point => |join_point| {
                plan.add(.{ .typed_local_span = join_point.params });
                plan.add(.{ .typed_local_span = join_point.retained });
                plan.add(.{ .expr = join_point.body });
                plan.add(.{ .expr = join_point.remainder });
            },
            .jump => |jump| {
                plan.add(.{ .expr_span = jump.args });
                plan.add(.{ .typed_local_span = jump.loop_params });
                plan.add(.{ .expr_span = jump.loop_values });
            },
            .return_ => |ret| plan.add(.{ .expr = ret.value }),
            .comptime_branch_taken => |taken| {
                plan.add(.{ .comptime_site = taken.site });
                plan.add(.{ .expr = taken.body });
            },
            .comptime_exhaustiveness_failed => |site| plan.add(.{ .comptime_site = site }),
            .dbg => |child| plan.add(.{ .expr = child }),
            .expect_err => |expect_err| plan.add(.{ .expr = expect_err.msg }),
            .literal_rejected => |rejected| plan.add(.{ .expr = rejected.msg }),
            .expect => |child| if (self.inline_expects != .omit) plan.add(.{ .expr = child }),
        }
    }

    fn buildExprData(self: *Lowerer, task: *ExprTask, expr: Lifted.Expr) Allocator.Error!Ast.ExprData {
        const parts = task.parts.items;
        return switch (expr.data) {
            .local => |local| try self.lowerLocalExpr(local, task.ty),
            .unit => .unit,
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .bytes_lit => |value| .{ .bytes_lit = value },
            .comptime_value => |value| .{ .comptime_value = .{
                .root = value.root,
                .initializer = parts[0].get(.expr),
            } },
            .static_data_candidate => |candidate| .{ .static_data_candidate = .{
                .static_data = candidate.static_data,
                .storage = candidate.storage,
                .runtime_expr = parts[0].get(.expr),
            } },
            .typed_boundary => .{ .typed_boundary = .{ .value = parts[0].get(.expr) } },
            .@"unreachable" => .@"unreachable",
            .uninitialized => .uninitialized,
            .uninitialized_payload => |payload| .{ .uninitialized_payload = .{
                .condition = parts[0].get(.local),
                .mask = payload.mask,
            } },
            .list => .{ .list = parts[0].get(.expr_span) },
            .tuple => .{ .tuple = parts[0].get(.expr_span) },
            .record => .{ .record = parts[0].get(.field_span) },
            .record_update => .{ .record_update = .{
                .base = parts[0].get(.expr),
                .fields = parts[1].get(.field_span),
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = parts[0].get(.expr_span),
            } },
            .nominal => .{ .nominal = parts[0].get(.expr) },
            .let_ => |let_| .{ .let_ = .{
                .bind = parts[0].get(.pat),
                .value = parts[1].get(.expr),
                .rest = parts[2].get(.expr),
                .comptime_site = if (let_.comptime_site != null) parts[3].get(.comptime_site) else null,
            } },
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached Lambda Mono"),
            .fn_ref, .call_value => parts[0].get(.data),
            .call_proc => |call| .{ .direct_call = .{
                .target = .{ .local = parts[0].get(.fn_id) },
                .args = parts[1].get(.expr_span),
                .is_cold = call.is_cold,
            } },
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = parts[0].get(.expr_span),
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = parts[0].get(.expr),
                .segments = self.lowerFieldAccessSegmentSpan(field.segments),
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = parts[0].get(.expr),
                .elem_index = access.elem_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = parts[0].get(.expr),
                .rhs = parts[1].get(.expr),
                .negated = eq.negated,
            } },
            .structural_hash => .{ .structural_hash = .{
                .value = parts[0].get(.expr),
                .hasher = parts[1].get(.expr),
            } },
            .match_ => |match| if (self.folded_matches.get(match.scrutinee) != null)
                .{ .block = .{
                    .statements = .empty(),
                    .final_expr = parts[0].get(.expr),
                } }
            else
                .{ .match_ = .{
                    .scrutinee = parts[0].get(.expr),
                    .branches = parts[1].get(.branch_span),
                    .comptime_site = if (match.comptime_site != null) parts[2].get(.comptime_site) else null,
                } },
            .if_ => .{ .if_ = .{
                .branches = parts[0].get(.if_branch_span),
                .final_else = parts[1].get(.expr),
            } },
            .if_initialized_payload => |payload_switch| .{ .if_initialized_payload = .{
                .cond = parts[0].get(.expr),
                .cond_mask = payload_switch.cond_mask,
                .payload = parts[1].get(.local),
                .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
                .initialized = parts[2].get(.expr),
                .uninitialized = parts[3].get(.expr),
            } },
            .try_sequence => |sequence| .{ .try_sequence = .{
                .try_expr = parts[0].get(.expr),
                .ok_local = parts[1].get(.local),
                .err_is_cold = sequence.err_is_cold,
                .err_target = sequence.err_target,
                .ok_body = parts[2].get(.expr),
            } },
            .try_record_sequence => |sequence| .{ .try_record_sequence = .{
                .try_expr = parts[0].get(.expr),
                .value_local = parts[1].get(.local),
                .value_field = sequence.value_field,
                .rest_local = parts[2].get(.local),
                .rest_field = sequence.rest_field,
                .err_is_cold = sequence.err_is_cold,
                .err_target = sequence.err_target,
                .ok_body = parts[3].get(.expr),
            } },
            .block => .{ .block = .{
                .statements = parts[0].get(.stmt_span),
                .final_expr = parts[1].get(.expr),
            } },
            .loop_ => .{ .loop_ = .{
                .params = parts[0].get(.typed_local_span),
                .initial_values = parts[1].get(.expr_span),
                .body = parts[2].get(.expr),
            } },
            .break_ => |maybe| .{ .break_ = if (maybe != null) parts[0].get(.expr) else null },
            .continue_ => .{ .continue_ = .{ .values = parts[0].get(.expr_span) } },
            .join_point => |join_point| .{ .join_point = .{
                .id = join_point.id,
                .params = parts[0].get(.typed_local_span),
                .retained = parts[1].get(.typed_local_span),
                .body = parts[2].get(.expr),
                .remainder = parts[3].get(.expr),
            } },
            .jump => |jump| .{ .jump = .{
                .target = jump.target,
                .args = parts[0].get(.expr_span),
                .loop_params = parts[1].get(.typed_local_span),
                .loop_values = parts[2].get(.expr_span),
            } },
            .return_ => .{ .return_ = parts[0].get(.expr) },
            .crash => |msg| .{ .crash = msg },
            .checked_error => |msg| .{ .checked_error = msg },
            .comptime_branch_taken => |taken| .{ .comptime_branch_taken = .{
                .site = parts[0].get(.comptime_site),
                .branch_index = taken.branch_index,
                .body = parts[1].get(.expr),
            } },
            .comptime_exhaustiveness_failed => .{ .comptime_exhaustiveness_failed = parts[0].get(.comptime_site) },
            .dbg => .{ .dbg = parts[0].get(.expr) },
            .expect_err => |expect_err| .{ .expect_err = .{
                .msg = parts[0].get(.expr),
                .region = expect_err.region,
            } },
            .literal_rejected => |rejected| .{ .literal_rejected = .{
                .msg = parts[0].get(.expr),
                .site = rejected.site,
            } },
            .expect => if (self.inline_expects == .omit)
                .unit
            else
                .{ .expect = parts[0].get(.expr) },
        };
    }

    const StmtTask = struct {
        stmt_id: Lifted.StmtId,
        saved_loc: base.SourceLoc = undefined,
        saved_region: base.Region = undefined,
        plan: Plan = .{},
        parts: std.ArrayList(Result) = .empty,
    };

    fn stepStmt(self: *Lowerer, frame: *Frame, task: *StmtTask, input: ?Result) Allocator.Error!Step {
        const index = @backingInt(task.stmt_id);
        const stmt = self.solved.lifted.stmts[index];
        if (frame.cursor == 0) {
            if (self.stmt_map[index]) |cached| return .{ .ret = .{ .stmt = cached } };
            task.saved_loc = self.program.current_loc;
            task.saved_region = self.program.current_region;
            const stmt_loc = self.solved.lifted.stmtLoc(task.stmt_id);
            if (stmt_loc.hasLocation()) self.program.current_loc = stmt_loc;
            const stmt_region = self.solved.lifted.stmtRegion(task.stmt_id);
            if (!stmt_region.isEmpty()) self.program.current_region = stmt_region;
            switch (stmt) {
                .uninitialized => |pat| task.plan.add(.{ .pat = pat }),
                .let_ => |let_| {
                    task.plan.add(.{ .pat = let_.pat });
                    task.plan.add(.{ .expr = let_.value });
                    if (let_.comptime_site) |site| task.plan.add(.{ .comptime_site = site });
                },
                .expr, .dbg => |expr| task.plan.add(.{ .expr = expr }),
                .expect => |expr| if (self.inline_expects != .omit) task.plan.add(.{ .expr = expr }),
                .return_ => |ret| task.plan.add(.{ .expr = ret.value }),
                .crash, .checked_error => {},
            }
            frame.cursor = 1;
            if (try self.nextPart(&task.parts, task.plan.slice(), null)) |step| return step;
        } else if (try self.nextPart(&task.parts, task.plan.slice(), input)) |step| return step;
        const parts = task.parts.items;
        const lowered_stmt: Ast.Stmt = switch (stmt) {
            .uninitialized => .{ .uninitialized = parts[0].get(.pat) },
            .let_ => |let_| .{ .let_ = .{
                .pat = parts[0].get(.pat),
                .value = parts[1].get(.expr),
                .recursive = let_.recursive,
                .comptime_site = if (let_.comptime_site != null) parts[2].get(.comptime_site) else null,
            } },
            .expr => .{ .expr = parts[0].get(.expr) },
            .expect => if (self.inline_expects == .omit)
                .{ .expr = try self.unitExpr() }
            else
                .{ .expect = parts[0].get(.expr) },
            .dbg => .{ .dbg = parts[0].get(.expr) },
            .return_ => .{ .return_ = parts[0].get(.expr) },
            .crash => |msg| .{ .crash = msg },
            .checked_error => |msg| .{ .checked_error = msg },
        };
        const lowered = try self.program.addStmt(lowered_stmt);
        self.stmt_map[index] = lowered;
        self.program.current_loc = task.saved_loc;
        self.program.current_region = task.saved_region;
        return .{ .ret = .{ .stmt = lowered } };
    }

    const PatTask = struct {
        pat_id: Lifted.PatId,
        ty: Type.TypeId = undefined,
        plan: Plan = .{},
        parts: std.ArrayList(Result) = .empty,
    };

    fn stepPat(self: *Lowerer, frame: *Frame, task: *PatTask, input: ?Result) Allocator.Error!Step {
        const index = @backingInt(task.pat_id);
        const pat = self.solved.lifted.pats[index];
        switch (frame.cursor) {
            0 => {
                if (self.pat_map[index]) |cached| return .{ .ret = .{ .pat = cached } };
                frame.cursor = 1;
                return .{ .call = .{ .type_var = .{ .var_id = self.solved.pat_tys[index] } } };
            },
            1 => {
                task.ty = input.?.get(.ty);
                const plan = &task.plan;
                switch (pat.data) {
                    .bind => |local| plan.add(.{ .local_at = .{ .local = local, .ty = task.ty } }),
                    .as => |as| {
                        plan.add(.{ .pat = as.pattern });
                        plan.add(.{ .local_at = .{ .local = as.local, .ty = task.ty } });
                    },
                    .record => |fields| plan.add(.{ .destruct_span = fields }),
                    .tuple => |items| plan.add(.{ .pat_span = items }),
                    .list => |list| {
                        plan.add(.{ .pat_span = list.patterns });
                        if (list.rest) |rest| if (rest.pattern) |rest_pattern| plan.add(.{ .pat = rest_pattern });
                    },
                    .tag => |tag| plan.add(.{ .pat_span = tag.payloads }),
                    .nominal => |backing| plan.add(.{ .pat = backing }),
                    .str_pattern => |str| plan.add(.{ .str_pattern = str }),
                    .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => {},
                }
                frame.cursor = 2;
                if (try self.nextPart(&task.parts, task.plan.slice(), null)) |step| return step;
            },
            else => if (try self.nextPart(&task.parts, task.plan.slice(), input)) |step| return step,
        }
        const parts = task.parts.items;
        const data: Ast.PatData = switch (pat.data) {
            .bind => .{ .bind = parts[0].get(.local) },
            .wildcard => .wildcard,
            .as => .{ .as = .{
                .pattern = parts[0].get(.pat),
                .local = parts[1].get(.local),
            } },
            .record => .{ .record = parts[0].get(.destruct_span) },
            .tuple => .{ .tuple = parts[0].get(.pat_span) },
            .list => |list| .{ .list = .{
                .patterns = parts[0].get(.pat_span),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern != null) parts[1].get(.pat) else null,
                } else null,
            } },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = parts[0].get(.pat_span),
            } },
            .nominal => .{ .nominal = parts[0].get(.pat) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .str_pattern => .{ .str_pattern = parts[0].get(.str_pattern) },
        };
        const lowered = try self.program.addPat(.{ .ty = task.ty, .data = data });
        self.pat_map[index] = lowered;
        return .{ .ret = .{ .pat = lowered } };
    }

    /// A span of parts lowered in order, then added as one span.
    const SeqTask = struct {
        kind: union(enum) {
            exprs: Lifted.Span(Lifted.ExprId),
            /// The lowered expressions returned as an owned slice.
            expr_slice: Lifted.Span(Lifted.ExprId),
            fields: Lifted.Span(Lifted.FieldExpr),
            pats: Lifted.Span(Lifted.PatId),
            stmts: Lifted.Span(Lifted.StmtId),
            typed_locals: Lifted.Span(Lifted.TypedLocal),
            destructs: Lifted.Span(Lifted.RecordDestruct),
            branches: Lifted.Span(Lifted.Branch),
            if_branches: Lifted.Span(Lifted.IfBranch),
            str_pattern: Lifted.StrPattern,
        },
        results: std.ArrayList(Result) = .empty,
    };

    /// The parts one element of a sequence lowers.
    fn seqElementParts(self: *Lowerer, task: *SeqTask, index: usize, out: *Plan) void {
        const lifted = self.solved.lifted;
        switch (task.kind) {
            .exprs, .expr_slice => |span| out.add(.{ .expr = GuardedList.at(lifted.exprSpan(span), index) }),
            .fields => |span| out.add(.{ .expr = GuardedList.at(lifted.fieldExprSpan(span), index).value }),
            .pats => |span| out.add(.{ .pat = GuardedList.at(lifted.patSpan(span), index) }),
            .stmts => |span| out.add(.{ .stmt = GuardedList.at(lifted.stmtSpan(span), index) }),
            .typed_locals => |span| {
                const item = GuardedList.at(lifted.typedLocalSpan(span), index);
                if (self.local_map[@backingInt(item.local)]) |mapped| {
                    out.add(.{ .local_at = .{ .local = item.local, .ty = self.program.getLocal(mapped).ty } });
                } else {
                    out.add(.{ .local = item.local });
                }
            },
            .destructs => |span| out.add(.{ .pat = GuardedList.at(lifted.recordDestructSpan(span), index).pattern }),
            .branches => |span| {
                const branch = GuardedList.at(lifted.branchSpan(span), index);
                out.add(.{ .pat = branch.pat });
                out.add(.{ .stmt_span = branch.bindings });
                if (branch.guard) |guard| out.add(.{ .expr = guard });
                out.add(.{ .expr = branch.body });
            },
            .if_branches => |span| {
                const branch = GuardedList.at(lifted.ifBranchSpan(span), index);
                out.add(.{ .expr = branch.cond });
                out.add(.{ .expr = branch.body });
            },
            .str_pattern => |str| if (GuardedList.at(lifted.strPatternStepSpan(str.steps), index).capture) |capture| {
                out.add(.{ .pat = capture });
            },
        }
    }

    fn seqLen(self: *Lowerer, task: *SeqTask) usize {
        const lifted = self.solved.lifted;
        return switch (task.kind) {
            .exprs, .expr_slice => |span| lifted.exprSpan(span).len,
            .fields => |span| lifted.fieldExprSpan(span).len,
            .pats => |span| lifted.patSpan(span).len,
            .stmts => |span| lifted.stmtSpan(span).len,
            .typed_locals => |span| lifted.typedLocalSpan(span).len,
            .destructs => |span| lifted.recordDestructSpan(span).len,
            .branches => |span| lifted.branchSpan(span).len,
            .if_branches => |span| lifted.ifBranchSpan(span).len,
            .str_pattern => |str| lifted.strPatternStepSpan(str.steps).len,
        };
    }

    fn stepSeq(self: *Lowerer, frame: *Frame, task: *SeqTask, input: ?Result) Allocator.Error!Step {
        if (input) |result| try task.results.append(self.allocator, result);
        const len = self.seqLen(task);
        // `frame.index` is the element whose parts are lowering; `cursor`
        // counts that element's parts already issued.
        while (frame.index < len) {
            var element: Plan = .{};
            self.seqElementParts(task, frame.index, &element);
            const parts = element.slice();
            while (frame.cursor < parts.len) {
                const part = parts[frame.cursor];
                frame.cursor += 1;
                if (try self.partTask(part, &task.results)) |child| return .{ .call = child };
            }
            frame.index += 1;
            frame.cursor = 0;
        }
        return .{ .ret = try self.finishSeq(task) };
    }

    fn finishSeq(self: *Lowerer, task: *SeqTask) Allocator.Error!Result {
        const lifted = self.solved.lifted;
        const results = task.results.items;
        switch (task.kind) {
            .exprs, .expr_slice => {
                const lowered = try self.allocator.alloc(Ast.ExprId, results.len);
                for (lowered, results) |*out, result| out.* = result.get(.expr);
                if (task.kind == .expr_slice) return .{ .slice = lowered };
                defer self.allocator.free(lowered);
                return .{ .expr_span = try self.program.addExprSpan(lowered) };
            },
            .fields => |span| {
                const lowered = try self.allocator.alloc(Ast.FieldExpr, results.len);
                defer self.allocator.free(lowered);
                const fields = lifted.fieldExprSpan(span);
                for (lowered, results, 0..) |*out, result, i| out.* = .{ .name = GuardedList.at(fields, i).name, .value = result.get(.expr) };
                return .{ .field_span = try self.program.addFieldExprSpan(lowered) };
            },
            .pats => {
                const lowered = try self.allocator.alloc(Ast.PatId, results.len);
                defer self.allocator.free(lowered);
                for (lowered, results) |*out, result| out.* = result.get(.pat);
                return .{ .pat_span = try self.program.addPatSpan(lowered) };
            },
            .stmts => {
                const lowered = try self.allocator.alloc(Ast.StmtId, results.len);
                defer self.allocator.free(lowered);
                for (lowered, results) |*out, result| out.* = result.get(.stmt);
                return .{ .stmt_span = try self.program.addStmtSpan(lowered) };
            },
            .typed_locals => {
                const lowered = try self.allocator.alloc(Ast.TypedLocal, results.len);
                defer self.allocator.free(lowered);
                for (lowered, results) |*out, result| {
                    const local = result.get(.local);
                    out.* = .{ .local = local, .ty = self.program.getLocal(local).ty };
                }
                return .{ .typed_local_span = try self.program.addTypedLocalSpan(lowered) };
            },
            .destructs => |span| {
                const lowered = try self.allocator.alloc(Ast.RecordDestruct, results.len);
                defer self.allocator.free(lowered);
                const destructs = lifted.recordDestructSpan(span);
                for (lowered, results, 0..) |*out, result, i| out.* = .{ .name = GuardedList.at(destructs, i).name, .pattern = result.get(.pat) };
                return .{ .destruct_span = try self.program.addRecordDestructSpan(lowered) };
            },
            .branches => |span| {
                const branches = lifted.branchSpan(span);
                const lowered = try self.allocator.alloc(Ast.Branch, branches.len);
                defer self.allocator.free(lowered);
                var position: usize = 0;
                for (lowered, 0..) |*out, i| {
                    const branch = GuardedList.at(branches, i);
                    out.* = .{
                        .pat = results[position].get(.pat),
                        .bindings = results[position + 1].get(.stmt_span),
                        .guard = if (branch.guard != null) results[position + 2].get(.expr) else null,
                        .body = results[position + 2 + @intFromBool(branch.guard != null)].get(.expr),
                    };
                    position += 3 + @as(usize, @intFromBool(branch.guard != null));
                }
                return .{ .branch_span = try self.program.addBranchSpan(lowered) };
            },
            .if_branches => {
                const lowered = try self.allocator.alloc(Ast.IfBranch, results.len / 2);
                defer self.allocator.free(lowered);
                for (lowered, 0..) |*out, i| out.* = .{
                    .cond = results[2 * i].get(.expr),
                    .body = results[2 * i + 1].get(.expr),
                };
                return .{ .if_branch_span = try self.program.addIfBranchSpan(lowered) };
            },
            .str_pattern => |str| {
                const input_steps = lifted.strPatternStepSpan(str.steps);
                const steps = try self.allocator.alloc(Ast.StrPatternStep, input_steps.len);
                defer self.allocator.free(steps);
                var position: usize = 0;
                for (steps, 0..) |*out, i| {
                    const step = GuardedList.at(input_steps, i);
                    out.* = .{
                        .capture = if (step.capture != null) blk: {
                            position += 1;
                            break :blk results[position - 1].get(.pat);
                        } else null,
                        .delimiter = step.delimiter,
                    };
                }
                return .{ .str_pattern = .{
                    .prefix = str.prefix,
                    .steps = try self.program.addStrPatternStepSpan(steps),
                    .end = str.end,
                } };
            },
        }
    }

    const TypedLocalTask = struct {
        local: Lifted.LocalId,
        known_ty: ?Type.TypeId = null,
    };

    const CallableValueTask = struct {
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        captures_span: Lifted.Span(Lifted.CaptureOperand),
        ty: Type.TypeId,
        variant: Type.FnVariant = undefined,
    };

    fn stepCallableValue(self: *Lowerer, frame: *Frame, task: *CallableValueTask, input: ?Result) Allocator.Error!Step {
        const ty_content = self.program.types.get(task.ty);
        if (frame.cursor == 0) {
            const captures = self.memberCapturesForExpr(task.expr_id, task.fn_id);
            const capture_operands = self.solved.lifted.captureOperandSpan(task.captures_span);
            if (self.solved.lifted.typedLocalSpan(self.solved.lifted.fns[@backingInt(task.fn_id)].captures).len != capture_operands.len) {
                Common.invariant("function reference capture operand count differed from lifted function captures");
            }
            const variants = switch (ty_content) {
                .callable => |variants| variants,
                .erased_fn => |erased| erased.members,
                .primitive, .named, .record, .capture_record, .tuple, .tag_union, .list, .box, .erased_capture_ptr, .zst => Common.invariant("function value lowered to non-callable Lambda Mono type"),
            };
            const fn_symbol = self.solved.lifted.fns[@backingInt(task.fn_id)].symbol;
            const variant_span = self.program.types.fnVariantSpan(variants);
            const found = for (0..variant_span.len) |index| {
                const variant = GuardedList.at(variant_span, index);
                if (variant.source == fn_symbol) break variant;
            } else switch (ty_content) {
                .callable => Common.invariant("finite callable type did not contain referenced function"),
                .primitive, .named, .record, .capture_record, .tuple, .tag_union, .list, .box, .erased_fn, .erased_capture_ptr, .zst => Common.invariant("erased callable type did not contain referenced function"),
            };
            task.variant = found;
            if (found.capture_ty) |capture_ty| {
                frame.cursor = 1;
                return .{ .call = .{ .capture_record_expr = .{ .capture_span = captures, .operands_span = task.captures_span, .capture_ty = capture_ty } } };
            }
        }
        const payload: ?Ast.ExprId = if (frame.cursor == 1) input.?.get(.expr) else null;
        return .{ .ret = .{ .data = if (ty_content == .erased_fn)
            .{ .packed_erased_fn = .{
                .target = task.variant.target,
                .capture = payload,
            } }
        else
            .{ .callable = .{
                .ty = task.ty,
                .variant = task.variant.id,
                .payload = payload,
            } } } };
    }

    const DirectCallArgsTask = struct {
        fn_id: Lifted.FnId,
        args_span: Lifted.Span(Lifted.ExprId),
        captures_span: Lifted.Span(Lifted.CaptureOperand),
        /// Owned.
        args: []Ast.ExprId = &.{},
        captures: CaptureSpanId = undefined,
    };

    fn stepDirectCallArgs(self: *Lowerer, frame: *Frame, task: *DirectCallArgsTask, input: ?Result) Allocator.Error!Step {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .seq = .{ .kind = .{ .expr_slice = task.args_span } } } };
            },
            1 => {
                task.args = input.?.get(.slice);
                task.captures = try self.capturesForFn(task.fn_id);
                const capture_items = self.captureSpan(task.captures);
                if (capture_items.len != 0) {
                    const capture_operands = self.solved.lifted.captureOperandSpan(task.captures_span);
                    if (capture_operands.len != capture_items.len) Common.invariant("direct call capture operand count differed from callee capture count");
                    frame.cursor = 2;
                    return .{ .call = try self.ownFnSpecTask(task.fn_id, .finite) };
                }
                if (self.solved.lifted.captureOperandSpan(task.captures_span).len != 0) {
                    Common.invariant("direct call carried capture operands for a capture-free callee");
                }
                return .{ .ret = .{ .expr_span = try self.program.addExprSpan(task.args) } };
            },
            2 => {
                const target_fn = input.?.get(.fn_id);
                const capture_ty = self.fn_specs.items[@backingInt(target_fn)].capture_ty orelse
                    Common.invariant("capturing direct call target had no capture record type");
                frame.cursor = 3;
                return .{ .call = .{ .capture_record_expr = .{ .capture_span = task.captures, .operands_span = task.captures_span, .capture_ty = capture_ty } } };
            },
            else => {
                const call_args = try self.allocator.alloc(Ast.ExprId, task.args.len + 1);
                defer self.allocator.free(call_args);
                @memcpy(call_args[0..task.args.len], task.args);
                call_args[task.args.len] = input.?.get(.expr);
                return .{ .ret = .{ .expr_span = try self.program.addExprSpan(call_args) } };
            },
        }
    }

    const ValueCallTask = struct {
        ty: Type.TypeId,
        callee: Lifted.ExprId,
        args_span: Lifted.Span(Lifted.ExprId),
        callee_expr: Ast.ExprId = undefined,
        /// Owned.
        args: []Ast.ExprId = &.{},
    };

    fn stepValueCall(self: *Lowerer, frame: *Frame, task: *ValueCallTask, input: ?Result) Allocator.Error!Step {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                return .{ .call = .{ .expr = .{ .expr_id = task.callee } } };
            },
            1 => {
                task.callee_expr = input.?.get(.expr);
                frame.cursor = 2;
                return .{ .call = .{ .seq = .{ .kind = .{ .expr_slice = task.args_span } } } };
            },
            else => task.args = input.?.get(.slice),
        }
        const callee = task.callee_expr;
        const ty = task.ty;
        const args = task.args;
        const callee_ty = self.program.getExpr(callee).ty;
        return .{ .ret = .{ .data = switch (self.program.types.get(callee_ty)) {
            .callable => |variants| blk: {
                const branches = try self.allocator.alloc(Ast.Branch, variants.len);
                defer self.allocator.free(branches);
                const variant_span = self.program.types.fnVariantSpan(variants);
                for (0..variant_span.len) |i| {
                    const variant = GuardedList.at(variant_span, i);
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
                        const bind_local = switch (self.program.getPat(pat_id).data) {
                            .bind => |local| local,
                            .wildcard, .as, .record, .tuple, .list, .tag, .callable, .nominal, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => unreachable,
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
            .primitive, .named, .record, .capture_record, .tuple, .tag_union, .list, .box, .erased_capture_ptr, .zst => Common.invariant("value call callee had no callable Lambda Mono representation"),
        } } };
    }

    const CaptureRecordExprTask = struct {
        capture_span: CaptureSpanId,
        operands_span: Lifted.Span(Lifted.CaptureOperand),
        capture_ty: Type.TypeId,
        values: std.ArrayList(Ast.ExprId) = .empty,
    };

    fn stepCaptureRecordExpr(self: *Lowerer, frame: *Frame, task: *CaptureRecordExprTask, input: ?Result) Allocator.Error!Step {
        const captures = self.captureSpan(task.capture_span);
        const capture_operands = self.solved.lifted.captureOperandSpan(task.operands_span);
        if (frame.cursor == 0) {
            const fields = switch (self.program.types.get(task.capture_ty)) {
                .capture_record => |field_span| self.program.types.captureFieldSpan(field_span),
                .primitive, .named, .record, .tuple, .tag_union, .callable, .list, .box, .erased_fn, .erased_capture_ptr, .zst => Common.invariant("callable capture payload was not a capture record"),
            };
            if (captures.len != fields.len) Common.invariant("callable capture payload arity differed from captured locals");
            if (captures.len != capture_operands.len) {
                Common.invariant("function reference capture operand count differed from lifted function captures");
            }

            // Member captures, capture-record fields, and keyed operands are all in
            // ascending CaptureId order, so the join is an exact indexed walk.
            for (0..captures.len) |i| {
                const capture = captures[i];
                const field = GuardedList.at(fields, i);
                const operand = GuardedList.at(capture_operands, i);
                if (capture.capture_id != field.capture_id) {
                    Common.invariant("callable capture payload fields differed from captured locals");
                }
                const capture_id = capture.capture_id orelse Common.invariant("member capture had no CaptureId");
                if (operand.id != capture_id) {
                    Common.invariant("capture operand CaptureId did not match its member capture slot");
                }
            }
            frame.cursor = 1;
        } else {
            try task.values.append(self.allocator, input.?.get(.expr));
        }
        if (task.values.items.len < capture_operands.len) {
            return .{ .call = .{ .expr = .{ .expr_id = GuardedList.at(capture_operands, task.values.items.len).value } } };
        }
        return .{ .ret = .{ .expr = try self.program.addExpr(.{
            .ty = task.capture_ty,
            .data = .{ .capture_record = try self.program.addExprSpan(task.values.items) },
        }) } };
    }

    const TypeVarTask = struct {
        var_id: SolvedType.TypeVarId,
        root: SolvedType.TypeVarId = undefined,
        reserved: Type.TypeId = undefined,
        /// The erased callable whose members are lowering; null for a
        /// finite lambda set.
        erased: ?@FieldType(SolvedType.Content, "erased") = null,
        tys: std.ArrayList(Type.TypeId) = .empty,
        fields: std.ArrayList(Type.Field) = .empty,
        tags: std.ArrayList(Type.Tag) = .empty,
        field_ty: Type.TypeId = undefined,
        args: Type.Span = undefined,
        backing: Type.TypeId = undefined,
    };

    /// Cursor states of a type lowering.
    const TypeVarCursor = struct {
        const start = 0;
        const callable = 1;
        const children = 2;
        /// A record field's value type, after its type.
        const field_value = 3;
        const named_backing = 4;
        const named_declared_order = 5;
    };

    fn typeVarStep(var_id: SolvedType.TypeVarId) Step {
        return .{ .call = .{ .type_var = .{ .var_id = var_id } } };
    }

    fn finishTypeVar(self: *Lowerer, task: *TypeVarTask, content: Type.Content) Step {
        self.program.types.set(task.reserved, content);
        return .{ .ret = .{ .ty = task.reserved } };
    }

    fn stepTypeVar(self: *Lowerer, frame: *Frame, task: *TypeVarTask, input: ?Result) Allocator.Error!Step {
        const solved_types = self.solved.types;
        switch (frame.cursor) {
            TypeVarCursor.start => {
                task.root = solved_types.root(task.var_id);
                if (self.type_map.get(task.root)) |cached| return .{ .ret = .{ .ty = cached } };
                const content = solved_types.get(task.root);
                task.reserved = try self.program.types.add(.zst);
                try self.type_map.put(task.root, task.reserved);
                switch (content) {
                    .func => |func| {
                        frame.cursor = TypeVarCursor.callable;
                        return switch (solved_types.rootContent(func.callable)) {
                            .lambda_set => |members| .{ .call = .{ .members = .{ .members = members, .abi = .finite, .solved_fn_ty = task.root } } },
                            .erased => |erased| {
                                task.erased = erased;
                                return .{ .call = .{ .members = .{ .members = erased.members, .abi = .erased, .solved_fn_ty = task.root } } };
                            },
                            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .func, .zst, .mono => Common.invariant("function callable slot was unresolved before Lambda Mono"),
                        };
                    },
                    .link => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved link"),
                    .unbound, .forall => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved type"),
                    .mono => Common.invariant("Lambda Mono type lowering saw an unfinalized lazy Monotype leaf"),
                    .primitive => |primitive| return self.finishTypeVar(task, .{ .primitive = primitive }),
                    .zst => return self.finishTypeVar(task, .zst),
                    .erased => |erased| {
                        task.erased = erased;
                        frame.cursor = TypeVarCursor.callable;
                        return .{ .call = .{ .members = .{ .members = erased.members, .abi = .erased, .solved_fn_ty = null } } };
                    },
                    .lambda_set => |members| {
                        frame.cursor = TypeVarCursor.callable;
                        return .{ .call = .{ .members = .{ .members = members, .abi = .finite, .solved_fn_ty = null } } };
                    },
                    .list, .box, .tuple, .record, .tag_union, .named => frame.cursor = TypeVarCursor.children,
                }
            },
            TypeVarCursor.callable => {
                const members = input.?.get(.type_span);
                return self.finishTypeVar(task, if (task.erased) |erased|
                    .{ .erased_fn = .{ .source_fn_ty = erased.source_fn_ty, .members = members } }
                else
                    .{ .callable = members });
            },
            TypeVarCursor.children => {
                const lowered = input.?.get(.ty);
                switch (solved_types.get(task.root)) {
                    .list, .box, .tuple, .named, .tag_union => try task.tys.append(self.allocator, lowered),
                    .record => |fields| {
                        const field = solved_types.fieldSpan(fields)[frame.index];
                        if (field.value_ty) |value_ty| {
                            task.field_ty = lowered;
                            frame.cursor = TypeVarCursor.field_value;
                            return typeVarStep(value_ty);
                        }
                        try task.fields.append(self.allocator, .{ .name = field.name, .ty = lowered, .value_ty = null, .default = field.default });
                        frame.index += 1;
                    },
                    .link, .unbound, .forall, .primitive, .lambda_set, .erased, .func, .zst, .mono => Common.invariant("Lambda Mono type lowering resumed a type without children"),
                }
            },
            TypeVarCursor.field_value => {
                const field = solved_types.fieldSpan(solved_types.get(task.root).record)[frame.index];
                try task.fields.append(self.allocator, .{ .name = field.name, .ty = task.field_ty, .value_ty = input.?.get(.ty), .default = field.default });
                frame.index += 1;
                frame.cursor = TypeVarCursor.children;
            },
            TypeVarCursor.named_backing => {
                task.backing = input.?.get(.ty);
                frame.cursor = TypeVarCursor.named_declared_order;
                return .{ .call = .{ .declared_order = .{ .span = solved_types.get(task.root).named.declared_order } } };
            },
            TypeVarCursor.named_declared_order => {
                const named = solved_types.get(task.root).named;
                return self.finishTypeVar(task, .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = task.args,
                    .backing = if (named.backing) |backing| .{
                        .ty = task.backing,
                        .use = backing.use,
                        .authority = backing.authority,
                    } else null,
                    .declared_order = input.?.get(.type_span),
                } });
            },
            TypeVarCursor.named_declared_order + 1...std.math.maxInt(u8) => unreachable,
        }
        switch (solved_types.get(task.root)) {
            .list => |elem| {
                if (task.tys.items.len == 0) return typeVarStep(elem);
                return self.finishTypeVar(task, .{ .list = task.tys.items[0] });
            },
            .box => |elem| {
                if (task.tys.items.len == 0) return typeVarStep(elem);
                return self.finishTypeVar(task, .{ .box = task.tys.items[0] });
            },
            .tuple => |items| {
                const solved_items = solved_types.span(items);
                if (task.tys.items.len < solved_items.len) return typeVarStep(solved_items[task.tys.items.len]);
                return self.finishTypeVar(task, .{ .tuple = try self.program.types.addSpan(task.tys.items) });
            },
            .record => |fields| {
                const solved_fields = solved_types.fieldSpan(fields);
                if (frame.index < solved_fields.len) return typeVarStep(solved_fields[frame.index].ty);
                return self.finishTypeVar(task, .{ .record = try self.program.types.addFields(task.fields.items) });
            },
            .tag_union => |tags| {
                const solved_tags = solved_types.tagSpan(tags);
                while (frame.index < solved_tags.len) {
                    const tag = solved_tags[frame.index];
                    const payloads = solved_types.span(tag.payloads);
                    if (task.tys.items.len < payloads.len) return typeVarStep(payloads[task.tys.items.len]);
                    try task.tags.append(self.allocator, .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.program.types.addSpan(task.tys.items),
                    });
                    task.tys.clearRetainingCapacity();
                    frame.index += 1;
                }
                return self.finishTypeVar(task, .{ .tag_union = try self.program.types.addTags(task.tags.items) });
            },
            .named => |named| {
                const args = solved_types.span(named.args);
                if (task.tys.items.len < args.len) return typeVarStep(args[task.tys.items.len]);
                task.args = try self.program.types.addSpan(task.tys.items);
                if (named.backing) |backing| {
                    frame.cursor = TypeVarCursor.named_backing;
                    return typeVarStep(backing.ty);
                }
                frame.cursor = TypeVarCursor.named_declared_order;
                return .{ .call = .{ .declared_order = .{ .span = named.declared_order } } };
            },
            .link, .unbound, .forall, .primitive, .lambda_set, .erased, .func, .zst, .mono => Common.invariant("Lambda Mono type lowering resumed a type without children"),
        }
    }

    const FnSpecTask = struct {
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        abi: CaptureAbi,
        captures: CaptureSpanId,
        spec: FnSpec = undefined,
        fn_id: Ast.FnId = undefined,
        symbol: Common.Symbol = undefined,
    };

    fn stepFnSpec(self: *Lowerer, frame: *Frame, task: *FnSpecTask, input: ?Result) Allocator.Error!Step {
        switch (frame.cursor) {
            0 => {
                frame.cursor = 1;
                if (self.captureSpan(task.captures).len != 0) {
                    return .{ .call = .{ .capture_record_type = .{ .captures = task.captures } } };
                }
            },
            1 => {},
            else => {
                const ret_ty = input.?.get(.ty);
                const source_fn = self.solved.lifted.fns[@backingInt(task.source)];
                self.program.setFn(task.fn_id, .{
                    .symbol = task.symbol,
                    .source = source_fn.source,
                    .args = .empty(),
                    .body = .hosted,
                    .ret = ret_ty,
                });
                return .{ .ret = .{ .fn_id = task.fn_id } };
            },
        }
        const root_fn_ty = self.solved.types.root(task.solved_fn_ty);
        task.spec = FnSpec{
            .source = task.source,
            .solved_fn_ty = root_fn_ty,
            .abi = task.abi,
            .captures = task.captures,
            .capture_ty = if (input) |capture_record| capture_record.get(.ty) else null,
        };

        const result = try self.fn_spec_map.getOrPut(task.spec);
        if (result.found_existing) return .{ .ret = .{ .fn_id = result.value_ptr.* } };

        task.fn_id = @fromBackingInt(@intCast(@as(u32, @intCast(self.program.fnCount()))));
        const source_fn = self.solved.lifted.fns[@backingInt(task.source)];
        task.symbol = self.symbols.fresh();
        try self.program.fns.append(self.allocator, undefined);
        try self.fn_specs.append(self.allocator, task.spec);
        try self.fn_written.append(self.allocator, false);
        if (self.debug_specialization_identities) |identities| {
            try identities.append(self.allocator, .{
                .source = task.spec.source,
                .solved_fn_ty = task.spec.solved_fn_ty,
                .abi = task.spec.abi,
                .captures_source = task.spec.captures.source,
                .captures_start = specializationIdentityCaptureStart(task.spec.captures),
                .captures_len = task.spec.captures.len,
            });
        }
        result.value_ptr.* = task.fn_id;
        if (self.solved.lifted.procDebugName(source_fn.symbol)) |name| {
            try self.program.setProcDebugName(task.symbol, name);
        }

        frame.cursor = 2;
        return typeVarStep(switch (self.solved.types.rootContent(task.spec.solved_fn_ty)) {
            .func => |func| func.ret,
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .lambda_set, .erased, .zst, .mono => Common.invariant("Lambda Mono function table contains a non-function type"),
        });
    }

    const CaptureRecordTypeTask = struct {
        captures: CaptureSpanId,
        ty: Type.TypeId = undefined,
        fields: std.ArrayList(Type.CaptureField) = .empty,
    };

    fn stepCaptureRecordType(self: *Lowerer, frame: *Frame, task: *CaptureRecordTypeTask, input: ?Result) Allocator.Error!Step {
        if (frame.cursor == 0) {
            if (self.capture_types.get(task.captures)) |existing| return .{ .ret = .{ .ty = existing } };

            // A capture may contain a callable whose lambda set refers back to
            // this span. Reserve the record before descending into its fields.
            task.ty = try self.program.types.add(.zst);
            try self.capture_types.put(task.captures, task.ty);
            frame.cursor = 1;
        } else {
            const capture = self.captureSpan(task.captures)[task.fields.items.len];
            const capture_ty = input.?.get(.ty);
            try task.fields.append(self.allocator, .{
                .symbol = capture.symbol,
                .binder = capture.binder,
                .capture_id = capture.capture_id,
                .checked_capture_id = capture.checked_capture_id,
                .ty = capture_ty,
                .storage_ty = capture_ty,
            });
        }
        const capture_items = self.captureSpan(task.captures);
        if (task.fields.items.len < capture_items.len) return typeVarStep(capture_items[task.fields.items.len].ty);
        self.program.types.set(task.ty, .{ .capture_record = try self.program.types.addCaptureFields(task.fields.items) });
        return .{ .ret = .{ .ty = task.ty } };
    }

    const MembersTask = struct {
        members: SolvedType.Span,
        abi: CaptureAbi,
        /// The function type every member is specialized at; null lowers
        /// each member at its own function type.
        solved_fn_ty: ?SolvedType.TypeVarId,
        variants: std.ArrayList(Type.FnVariant) = .empty,
    };

    fn stepMembers(self: *Lowerer, frame: *Frame, task: *MembersTask, input: ?Result) Allocator.Error!Step {
        const solved_members = self.solved.types.memberSpan(task.members);
        if (frame.cursor == 0) {
            frame.cursor = 1;
        } else {
            const member = solved_members[task.variants.items.len];
            const target = input.?.get(.fn_id);
            try task.variants.append(self.allocator, .{
                .id = undefined, // assigned by addFnVariants before the variant is stored
                .source = member.lambda,
                .target = target,
                .capture_ty = self.fn_specs.items[@backingInt(target)].capture_ty,
            });
        }
        if (task.variants.items.len < solved_members.len) {
            const member = solved_members[task.variants.items.len];
            const source = self.sourceFnForSymbol(member.lambda);
            return .{ .call = .{ .fn_spec = .{
                .source = source,
                .solved_fn_ty = if (task.solved_fn_ty) |fn_ty|
                    self.solved.types.root(fn_ty)
                else
                    self.solved.types.root(self.solved.fn_tys[@backingInt(source)]),
                .abi = task.abi,
                .captures = CaptureSpanId.fromSolved(member.captures),
            } } };
        }
        return .{ .ret = .{ .type_span = try self.program.types.addFnVariants(task.variants.items) } };
    }

    /// Re-materializes a nominal record's declared field order from the Lambda
    /// Solved store into the Lambda Mono store. Named entries copy the shared
    /// field-name id; padding entries re-lower their reserved type.
    const DeclaredOrderTask = struct {
        span: SolvedType.Span,
        lowered: std.ArrayList(Type.DeclaredField) = .empty,
    };

    fn stepDeclaredOrder(self: *Lowerer, frame: *Frame, task: *DeclaredOrderTask, input: ?Result) Allocator.Error!Step {
        const source = self.solved.types.declaredFieldSpan(task.span);
        if (frame.cursor == 0) {
            if (source.len == 0) return .{ .ret = .{ .type_span = Type.Span.empty() } };
            frame.cursor = 1;
        } else {
            try task.lowered.append(self.allocator, .{ .padding = input.?.get(.ty) });
        }
        while (task.lowered.items.len < source.len) {
            switch (source[task.lowered.items.len]) {
                .named => |name| try task.lowered.append(self.allocator, .{ .named = name }),
                .padding => |ty| return typeVarStep(ty),
            }
        }
        return .{ .ret = .{ .type_span = try self.program.types.addDeclaredFields(task.lowered.items) } };
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
        const index = @backingInt(site);
        if (self.comptime_site_map[index]) |existing| return existing;

        const source = self.solved.lifted.comptimeSite(site);
        const lowered = try self.program.addComptimeSite(source.kind, source.owner, source.region, source.checked_site, source.branch_regions);
        self.comptime_site_map[index] = lowered;
        return lowered;
    }

    fn memberCapturesForExpr(self: *Lowerer, expr_id: Lifted.ExprId, fn_id: Lifted.FnId) CaptureSpanId {
        const fn_symbol = self.solved.lifted.fns[@backingInt(fn_id)].symbol;
        const expr_ty = self.solved.expr_tys[@backingInt(expr_id)];
        const callable = switch (self.solved.types.rootContent(expr_ty)) {
            .func => |func| func.callable,
            .lambda_set, .erased => expr_ty,
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .zst, .mono => Common.invariant("function reference expression had no callable Lambda Solved type"),
        };
        const members = switch (self.solved.types.rootContent(callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            .link, .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .func, .zst, .mono => Common.invariant("function reference callable slot was unresolved before Lambda Mono"),
        };
        for (self.solved.types.memberSpan(members)) |member| {
            if (member.lambda == fn_symbol) return CaptureSpanId.fromSolved(member.captures);
        }
        Common.invariant("function reference callable slot did not contain referenced function");
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

    fn localFor(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Allocator.Error!Ast.LocalId {
        const index = @backingInt(local);
        if (self.local_map[index]) |existing| return existing;
        const lifted_local = self.solved.lifted.locals[index];
        const lowered = try self.program.addLocalWithBinder(lifted_local.symbol, ty, lifted_local.binder);
        try self.program.setLocalName(lowered, self.solved.lifted.localName(@fromBackingInt(@intCast(index))));
        self.local_map[index] = lowered;
        return lowered;
    }

    fn lowerFieldAccessSegmentSpan(
        _: *Lowerer,
        span: Lifted.Span(Lifted.FieldAccessSegment),
    ) Ast.Span(Ast.FieldAccessSegment) {
        if (span.len == 0) Common.invariant("field access path had no segments");
        return .{ .start = span.start, .len = span.len };
    }
};

test "lambda mono lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
