//! MIR proc result-summary analysis.
//!
//! This is the release-path source of truth for proc result provenance.
//! It computes param-relative proc result contracts before LIR lowering so
//! call-result semantics do not depend on post-hoc analysis of finished LIR.

const std = @import("std");
const builtin = @import("builtin");

const MIR = @import("MIR.zig");
const LambdaSet = @import("LambdaSet.zig");

const Allocator = std.mem.Allocator;

/// One projection step used in param-relative provenance summaries.
pub const RefProjection = union(enum) {
    field: u16,
    tag_payload,
    nominal,
};

/// Span into flat ref-projection storage owned by the summary table.
pub const RefProjectionSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty projection span.
    pub fn empty() RefProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span contains no projections.
    pub fn isEmpty(self: RefProjectionSpan) bool {
        return self.len == 0;
    }
};

/// Param-relative alias/borrow contract with an optional projection path.
pub const ParamRefContract = struct {
    param_index: u8,
    projections: RefProjectionSpan = .empty(),
};

/// Proc- or root-level summary of result provenance.
pub const ProcResultContract = union(enum) {
    fresh,
    alias_of_param: ParamRefContract,
    borrow_of_param: ParamRefContract,
};

/// Expression-level summary of whether a MIR expression produces a normal value.
pub const ExprResultContract = union(enum) {
    no_return,
    concrete: ProcResultContract,
    borrow_of_fresh,
};

/// Immutable root-expression summary entry.
pub const RootContractEntry = struct {
    expr_id: MIR.ExprId,
    contract: ProcResultContract,
};

/// Finalized proc/root result-summary table used by downstream lowering.
pub const Table = struct {
    allocator: Allocator,
    ref_projections: std.ArrayList(RefProjection),
    proc_contracts: std.ArrayList(ProcResultContract),
    expr_contracts: std.ArrayList(?ExprResultContract),
    root_contract_entries: std.ArrayList(RootContractEntry),

    /// Initializes an empty summary table.
    pub fn init(allocator: Allocator) Table {
        return .{
            .allocator = allocator,
            .ref_projections = std.ArrayList(RefProjection).empty,
            .proc_contracts = std.ArrayList(ProcResultContract).empty,
            .expr_contracts = std.ArrayList(?ExprResultContract).empty,
            .root_contract_entries = std.ArrayList(RootContractEntry).empty,
        };
    }

    /// Releases all storage owned by this summary table.
    pub fn deinit(self: *Table) void {
        self.ref_projections.deinit(self.allocator);
        self.proc_contracts.deinit(self.allocator);
        self.expr_contracts.deinit(self.allocator);
        self.root_contract_entries.deinit(self.allocator);
    }

    fn addRefProjectionSpan(self: *Table, projections: []const RefProjection) Allocator.Error!RefProjectionSpan {
        if (projections.len == 0) return RefProjectionSpan.empty();

        const start = @as(u32, @intCast(self.ref_projections.items.len));
        try self.ref_projections.appendSlice(self.allocator, projections);
        return .{ .start = start, .len = @intCast(projections.len) };
    }

    /// Resolves a stored projection span to its projection slice.
    pub fn getRefProjectionSpan(self: *const Table, span: RefProjectionSpan) []const RefProjection {
        if (span.len == 0) return &.{};
        if (builtin.mode == .Debug) {
            const end = @as(u64, span.start) + @as(u64, span.len);
            if (end > self.ref_projections.items.len) {
                std.debug.panic(
                    "ProcResultSummary invariant violated: projection span start={d} len={d} exceeds ref-projection storage len={d}",
                    .{ span.start, span.len, self.ref_projections.items.len },
                );
            }
        }
        return self.ref_projections.items[span.start..][0..span.len];
    }

    /// Returns the precomputed result contract for a MIR proc.
    pub fn getProcContract(self: *const Table, proc_id: MIR.ProcId) ProcResultContract {
        return self.proc_contracts.items[@intFromEnum(proc_id)];
    }

    /// Returns the precomputed result contract for a requested root expression.
    pub fn getRootContract(self: *const Table, expr_id: MIR.ExprId) ProcResultContract {
        var low: usize = 0;
        var high: usize = self.root_contract_entries.items.len;
        const needle = @intFromEnum(expr_id);

        while (low < high) {
            const mid = low + (high - low) / 2;
            const entry = self.root_contract_entries.items[mid];
            const mid_expr_id = @intFromEnum(entry.expr_id);

            if (mid_expr_id == needle) return entry.contract;
            if (mid_expr_id < needle) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }

        std.debug.panic(
            "ProcResultSummary invariant violated: root expr {d} was not precomputed in MIR analyses",
            .{@intFromEnum(expr_id)},
        );
    }

    /// Returns the precomputed result contract for one MIR expression.
    pub fn getExprContract(self: *const Table, expr_id: MIR.ExprId) ExprResultContract {
        return self.expr_contracts.items[@intFromEnum(expr_id)] orelse std.debug.panic(
            "ProcResultSummary invariant violated: expr {d} was not precomputed in MIR analyses",
            .{@intFromEnum(expr_id)},
        );
    }
};

const BorrowRegion = union(enum) {
    proc,
    scope: u32,
};

const BorrowedParamOrigin = struct {
    param_index: u8,
    projections: RefProjectionSpan = .empty(),
    region: BorrowRegion,
};

const BorrowedFreshOrigin = struct {
    projections: RefProjectionSpan = .empty(),
    region: BorrowRegion,
};

const Origin = union(enum) {
    fresh,
    alias_of_param: ParamRefContract,
    borrow_of_param: BorrowedParamOrigin,
    borrow_of_fresh: BorrowedFreshOrigin,
};

const ExprOutcome = struct {
    value: ?Origin = null,
    breaks: bool = false,

    fn noReturn() ExprOutcome {
        return .{};
    }

    fn fromValue(origin: Origin) ExprOutcome {
        return .{ .value = origin };
    }

    fn hasNormalValue(self: ExprOutcome) bool {
        return self.value != null;
    }
};

const ProcSummaryState = union(enum) {
    no_return,
    concrete: ProcResultContract,
};

const ReturnAccumulator = struct {
    inferred: ?ProcResultContract = null,
};

const LocalOriginMap = std.AutoHashMap(u64, Origin);

const Analyzer = struct {
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lambda_sets: *const LambdaSet.Store,
    table: *Table,
    proc_states: ?[]const ProcSummaryState,
    active_value_defs: *std.AutoHashMapUnmanaged(u64, void),
    next_scope_id: u32 = 0,

    fn procSummary(self: *const Analyzer, proc_id: MIR.ProcId) ProcSummaryState {
        if (self.proc_states) |states| {
            return states[@intFromEnum(proc_id)];
        }
        return .{ .concrete = self.table.getProcContract(proc_id) };
    }

    fn resolveCallableProcId(self: *const Analyzer, expr_id: MIR.ExprId) ?MIR.ProcId {
        if (self.mir_store.resolveCallableProcId(expr_id)) |proc_id| return proc_id;

        const lambda_set_idx = self.lambda_sets.getExprLambdaSet(expr_id) orelse return null;
        const members = self.lambda_sets.getMembers(self.lambda_sets.getLambdaSet(lambda_set_idx).members);
        if (members.len == 1) return members[0].proc;

        const expected_fn_monotype = self.mir_store.typeOf(expr_id);
        var resolved: ?MIR.ProcId = null;
        for (members) |member| {
            if (self.mir_store.getProc(member.proc).fn_monotype != expected_fn_monotype) continue;
            if (resolved) |existing| {
                if (existing != member.proc) return null;
            } else {
                resolved = member.proc;
            }
        }

        return resolved;
    }

    fn panicUnresolvedCallable(self: *const Analyzer, expr_id: MIR.ExprId) noreturn {
        const expr = self.mir_store.getExpr(expr_id);
        std.debug.print(
            "ProcResultSummary unresolved callable: expr={d} tag={s} fn_monotype={d}\n",
            .{ @intFromEnum(expr_id), @tagName(expr), @intFromEnum(self.mir_store.typeOf(expr_id)) },
        );

        switch (expr) {
            .lookup => |symbol| {
                std.debug.print("  lookup symbol={d}\n", .{symbol.raw()});
                if (self.mir_store.getSymbolSeedProcSet(symbol)) |proc_ids| {
                    std.debug.print("  seed procs:", .{});
                    for (proc_ids) |proc_id| {
                        std.debug.print(
                            " {d}(fn_mono={d})",
                            .{ @intFromEnum(proc_id), @intFromEnum(self.mir_store.getProc(proc_id).fn_monotype) },
                        );
                    }
                    std.debug.print("\n", .{});
                } else {
                    std.debug.print("  seed procs: none\n", .{});
                }
            },
            else => {},
        }

        if (self.lambda_sets.getExprLambdaSet(expr_id)) |lambda_set_idx| {
            const members = self.lambda_sets.getMembers(self.lambda_sets.getLambdaSet(lambda_set_idx).members);
            std.debug.print("  lambda members:", .{});
            for (members) |member| {
                std.debug.print(
                    " {d}(fn_mono={d})",
                    .{ @intFromEnum(member.proc), @intFromEnum(self.mir_store.getProc(member.proc).fn_monotype) },
                );
            }
            std.debug.print("\n", .{});
        } else {
            std.debug.print("  lambda members: none\n", .{});
        }

        std.debug.panic(
            "ProcResultSummary invariant violated: call callee must resolve to a unique proc before strongest-form lowering",
            .{},
        );
    }

    fn freshScopeRegion(self: *Analyzer) BorrowRegion {
        defer self.next_scope_id += 1;
        return .{ .scope = self.next_scope_id };
    }

    fn singleProjectionSpan(self: *Analyzer, projection: RefProjection) Allocator.Error!RefProjectionSpan {
        return self.table.addRefProjectionSpan(&.{projection});
    }

    fn concatProjectionSpans(
        self: *Analyzer,
        left: RefProjectionSpan,
        right: RefProjectionSpan,
    ) Allocator.Error!RefProjectionSpan {
        if (left.isEmpty()) return right;
        if (right.isEmpty()) return left;

        const left_items = self.table.getRefProjectionSpan(left);
        const right_items = self.table.getRefProjectionSpan(right);
        const combined = try self.allocator.alloc(RefProjection, left_items.len + right_items.len);
        defer self.allocator.free(combined);

        @memcpy(combined[0..left_items.len], left_items);
        @memcpy(combined[left_items.len..], right_items);
        return self.table.addRefProjectionSpan(combined);
    }

    fn aliasOrigin(
        self: *Analyzer,
        origin: Origin,
        extra_projections: RefProjectionSpan,
    ) Allocator.Error!Origin {
        return switch (origin) {
            .fresh => .fresh,
            .alias_of_param => |aliased| .{ .alias_of_param = .{
                .param_index = aliased.param_index,
                .projections = try self.concatProjectionSpans(aliased.projections, extra_projections),
            } },
            .borrow_of_param => |borrowed| .{ .borrow_of_param = .{
                .param_index = borrowed.param_index,
                .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
                .region = borrowed.region,
            } },
            .borrow_of_fresh => |borrowed| .{ .borrow_of_fresh = .{
                .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
                .region = borrowed.region,
            } },
        };
    }

    fn borrowOrigin(
        self: *Analyzer,
        origin: Origin,
        region: BorrowRegion,
        extra_projections: RefProjectionSpan,
    ) Allocator.Error!Origin {
        return switch (origin) {
            .fresh => .{ .borrow_of_fresh = .{
                .projections = extra_projections,
                .region = region,
            } },
            .alias_of_param => |aliased| .{ .borrow_of_param = .{
                .param_index = aliased.param_index,
                .projections = try self.concatProjectionSpans(aliased.projections, extra_projections),
                .region = region,
            } },
            .borrow_of_param => |borrowed| .{ .borrow_of_param = .{
                .param_index = borrowed.param_index,
                .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
                .region = region,
            } },
            .borrow_of_fresh => |borrowed| .{ .borrow_of_fresh = .{
                .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
                .region = region,
            } },
        };
    }

    fn mergeReturnContract(
        self: *Analyzer,
        accumulator: *ReturnAccumulator,
        next: ProcResultContract,
    ) void {
        if (accumulator.inferred) |current| {
            accumulator.inferred = mergeContracts(self.table, current, next);
        } else {
            accumulator.inferred = next;
        }
    }

    fn procContractFromOrigin(_: *Analyzer, origin: Origin) ProcResultContract {
        return switch (origin) {
            .fresh => .fresh,
            .alias_of_param => |aliased| .{ .alias_of_param = aliased },
            .borrow_of_param => |borrowed| switch (borrowed.region) {
                .proc => .{ .borrow_of_param = .{
                    .param_index = borrowed.param_index,
                    .projections = borrowed.projections,
                } },
                .scope => |scope_id| std.debug.panic(
                    "ProcResultSummary invariant violated: scoped borrow from scope {d} escaped via return",
                    .{scope_id},
                ),
            },
            .borrow_of_fresh => .fresh,
        };
    }

    fn mergeReturnedOrigin(self: *Analyzer, accumulator: *ReturnAccumulator, origin: Origin) void {
        self.mergeReturnContract(accumulator, self.procContractFromOrigin(origin));
    }

    fn exprResultFromOutcome(_: *Analyzer, outcome: ExprOutcome) ExprResultContract {
        return if (outcome.value) |origin|
            switch (origin) {
                .fresh => .{ .concrete = .fresh },
                .alias_of_param => |aliased| .{ .concrete = .{ .alias_of_param = aliased } },
                .borrow_of_param => |borrowed| switch (borrowed.region) {
                    .proc => .{ .concrete = .{ .borrow_of_param = .{
                        .param_index = borrowed.param_index,
                        .projections = borrowed.projections,
                    } } },
                    .scope => |scope_id| std.debug.panic(
                        "ProcResultSummary invariant violated: scoped param borrow from scope {d} escaped via expr summary",
                        .{scope_id},
                    ),
                },
                .borrow_of_fresh => .borrow_of_fresh,
            }
        else
            .no_return;
    }

    fn recordExprContract(
        self: *Analyzer,
        expr_id: MIR.ExprId,
        contract: ExprResultContract,
    ) void {
        if (self.table.expr_contracts.items.len == 0) return;
        const slot = &self.table.expr_contracts.items[@intFromEnum(expr_id)];
        if (slot.*) |existing| {
            if (!exprContractsEqual(self.table, existing, contract)) {
                std.debug.panic(
                    "ProcResultSummary invariant violated: expr {d} was summarized with incompatible contracts",
                    .{@intFromEnum(expr_id)},
                );
            }
        } else {
            slot.* = contract;
        }
    }

    fn finishProcOutcome(
        self: *Analyzer,
        accumulator: *ReturnAccumulator,
        outcome: ExprOutcome,
    ) ProcSummaryState {
        if (outcome.breaks) {
            std.debug.panic(
                "ProcResultSummary invariant violated: break escaped the nearest loop while summarizing proc/root result",
                .{},
            );
        }

        if (outcome.value) |origin| {
            self.mergeReturnedOrigin(accumulator, origin);
            return .{ .concrete = accumulator.inferred.? };
        }

        if (accumulator.inferred) |contract| {
            return .{ .concrete = contract };
        }
        return .no_return;
    }

    fn cloneEnv(self: *Analyzer, source: *const LocalOriginMap) Allocator.Error!LocalOriginMap {
        var clone = LocalOriginMap.init(self.allocator);
        var it = source.iterator();
        while (it.next()) |entry| {
            try clone.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        return clone;
    }

    fn bindPattern(
        self: *Analyzer,
        env: *LocalOriginMap,
        pattern_id: MIR.PatternId,
        origin: Origin,
        region: BorrowRegion,
    ) Allocator.Error!void {
        switch (self.mir_store.getPattern(pattern_id)) {
            .bind => |symbol| try env.put(symbol.raw(), origin),
            .wildcard => {},
            .as_pattern => |as_pattern| {
                try env.put(as_pattern.symbol.raw(), origin);
                try self.bindPattern(env, as_pattern.pattern, origin, region);
            },
            .tag => |tag| {
                const args = self.mir_store.getPatternSpan(tag.args);
                if (args.len == 0) return;
                if (args.len != 1) {
                    std.debug.panic(
                        "ProcResultSummary invariant violated: tag pattern payload arity {d} must be wrapped before strongest-form summary",
                        .{args.len},
                    );
                }
                const projection = try self.singleProjectionSpan(.tag_payload);
                const payload_origin = try self.borrowOrigin(origin, region, projection);
                try self.bindPattern(env, args[0], payload_origin, region);
            },
            .struct_destructure => |destructure| {
                const fields = self.mir_store.getPatternSpan(destructure.fields);
                for (fields, 0..) |field_pattern, i| {
                    const projection = try self.singleProjectionSpan(.{ .field = @intCast(i) });
                    const field_origin = try self.borrowOrigin(origin, region, projection);
                    try self.bindPattern(env, field_pattern, field_origin, region);
                }
            },
            .list_destructure => std.debug.panic(
                "ProcResultSummary invariant violated: list destructure must be lowered before strongest-form summary",
                .{},
            ),
            .int_literal,
            .str_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .runtime_error,
            => {},
        }
    }

    fn bindBorrowPattern(
        self: *Analyzer,
        env: *LocalOriginMap,
        pattern_id: MIR.PatternId,
        source_origin: Origin,
        scope_region: BorrowRegion,
    ) Allocator.Error!void {
        const borrowed = try self.borrowOrigin(source_origin, scope_region, RefProjectionSpan.empty());
        try self.bindPattern(env, pattern_id, borrowed, scope_region);
    }

    fn analyzeNonLocalLookup(
        self: *Analyzer,
        symbol: MIR.Symbol,
    ) Allocator.Error!ExprOutcome {
        const def_expr = self.mir_store.getValueDef(symbol) orelse std.debug.panic(
            "ProcResultSummary invariant violated: non-local lookup symbol {d} has no MIR value definition",
            .{symbol.raw()},
        );

        const key = symbol.raw();
        const gop = try self.active_value_defs.getOrPut(self.allocator, key);
        if (gop.found_existing) {
            std.debug.panic(
                "ProcResultSummary invariant violated: cyclic MIR value definition for symbol {d}",
                .{key},
            );
        }
        defer _ = self.active_value_defs.remove(key);

        var empty_env = LocalOriginMap.init(self.allocator);
        defer empty_env.deinit();

        var value_accumulator = ReturnAccumulator{};
        const outcome = try self.analyzeExpr(&empty_env, .proc, &value_accumulator, def_expr, 0);

        return if (outcome.value != null)
            ExprOutcome{ .value = .fresh, .breaks = outcome.breaks }
        else
            ExprOutcome{ .breaks = outcome.breaks };
    }

    fn analyzeMatchExpr(
        self: *Analyzer,
        env: *LocalOriginMap,
        region: BorrowRegion,
        accumulator: *ReturnAccumulator,
        cond_expr: MIR.ExprId,
        branches: MIR.BranchSpan,
        loop_depth: u32,
    ) Allocator.Error!ExprOutcome {
        const cond_outcome = try self.analyzeExpr(env, region, accumulator, cond_expr, loop_depth);
        const scrutinee = cond_outcome.value orelse return ExprOutcome{ .breaks = cond_outcome.breaks };

        var merged_origin: ?Origin = null;
        var breaks = cond_outcome.breaks;
        for (self.mir_store.getBranches(branches)) |branch| {
            for (self.mir_store.getBranchPatterns(branch.patterns)) |branch_pattern| {
                var branch_env = try self.cloneEnv(env);
                defer branch_env.deinit();

                try self.bindPattern(&branch_env, branch_pattern.pattern, scrutinee, region);

                if (!branch.guard.isNone()) {
                    const guard_outcome = try self.analyzeExpr(&branch_env, region, accumulator, branch.guard, loop_depth);
                    breaks = breaks or guard_outcome.breaks;
                    if (!guard_outcome.hasNormalValue()) {
                        continue;
                    }
                }

                const body_outcome = try self.analyzeExpr(&branch_env, region, accumulator, branch.body, loop_depth);
                breaks = breaks or body_outcome.breaks;
                if (body_outcome.value) |origin| {
                    merged_origin = if (merged_origin) |existing|
                        mergeOrigins(self.table, existing, origin)
                    else
                        origin;
                }
            }
        }

        return ExprOutcome{
            .value = merged_origin,
            .breaks = breaks,
        };
    }

    fn analyzeStmt(
        self: *Analyzer,
        env: *LocalOriginMap,
        region: BorrowRegion,
        accumulator: *ReturnAccumulator,
        stmt: MIR.Stmt,
        loop_depth: u32,
    ) Allocator.Error!ExprOutcome {
        const binding = switch (stmt) {
            .decl_const => |inner| inner,
            .decl_var => |inner| inner,
            .mutate_var => |inner| inner,
        };
        const outcome = try self.analyzeExpr(env, region, accumulator, binding.expr, loop_depth);
        if (outcome.value) |origin| {
            try self.bindPattern(env, binding.pattern, origin, region);
            return ExprOutcome{ .value = .fresh, .breaks = outcome.breaks };
        }
        return ExprOutcome{ .breaks = outcome.breaks };
    }

    fn instantiateCallContract(
        self: *Analyzer,
        contract: ProcResultContract,
        arg_origins: []const Origin,
        region: BorrowRegion,
    ) Allocator.Error!Origin {
        return switch (contract) {
            .fresh => .fresh,
            .alias_of_param => |param_ref| blk: {
                if (param_ref.param_index >= arg_origins.len) {
                    std.debug.panic(
                        "ProcResultSummary invariant violated: proc result aliases arg {d}, but call only has {d} args",
                        .{ param_ref.param_index, arg_origins.len },
                    );
                }
                break :blk try self.aliasOrigin(arg_origins[param_ref.param_index], param_ref.projections);
            },
            .borrow_of_param => |param_ref| blk: {
                if (param_ref.param_index >= arg_origins.len) {
                    std.debug.panic(
                        "ProcResultSummary invariant violated: proc result borrows arg {d}, but call only has {d} args",
                        .{ param_ref.param_index, arg_origins.len },
                    );
                }
                break :blk try self.borrowOrigin(arg_origins[param_ref.param_index], region, param_ref.projections);
            },
        };
    }

    fn analyzeExpr(
        self: *Analyzer,
        env: *LocalOriginMap,
        region: BorrowRegion,
        accumulator: *ReturnAccumulator,
        expr_id: MIR.ExprId,
        loop_depth: u32,
    ) Allocator.Error!ExprOutcome {
        const outcome = try switch (self.mir_store.getExpr(expr_id)) {
            .int,
            .frac_f32,
            .frac_f64,
            .dec,
            .str,
            .proc_ref,
            .closure_make,
            => ExprOutcome.fromValue(.fresh),

            .lookup => |symbol| blk: {
                if (env.get(symbol.raw())) |origin| break :blk ExprOutcome.fromValue(origin);
                break :blk try self.analyzeNonLocalLookup(symbol);
            },

            .list => |list_data| blk: {
                var breaks = false;
                for (self.mir_store.getExprSpan(list_data.elems)) |elem_expr| {
                    const elem_outcome = try self.analyzeExpr(env, region, accumulator, elem_expr, loop_depth);
                    breaks = breaks or elem_outcome.breaks;
                    if (!elem_outcome.hasNormalValue()) {
                        break :blk ExprOutcome{ .breaks = breaks };
                    }
                }
                break :blk ExprOutcome{ .value = .fresh, .breaks = breaks };
            },

            .struct_ => |struct_data| blk: {
                var breaks = false;
                for (self.mir_store.getExprSpan(struct_data.fields)) |field_expr| {
                    const field_outcome = try self.analyzeExpr(env, region, accumulator, field_expr, loop_depth);
                    breaks = breaks or field_outcome.breaks;
                    if (!field_outcome.hasNormalValue()) {
                        break :blk ExprOutcome{ .breaks = breaks };
                    }
                }
                break :blk ExprOutcome{ .value = .fresh, .breaks = breaks };
            },

            .tag => |tag_data| blk: {
                var breaks = false;
                for (self.mir_store.getExprSpan(tag_data.args)) |arg_expr| {
                    const arg_outcome = try self.analyzeExpr(env, region, accumulator, arg_expr, loop_depth);
                    breaks = breaks or arg_outcome.breaks;
                    if (!arg_outcome.hasNormalValue()) {
                        break :blk ExprOutcome{ .breaks = breaks };
                    }
                }
                break :blk ExprOutcome{ .value = .fresh, .breaks = breaks };
            },

            .str_escape_and_quote => |inner| blk: {
                const inner_outcome = try self.analyzeExpr(env, region, accumulator, inner, loop_depth);
                if (!inner_outcome.hasNormalValue()) break :blk ExprOutcome{ .breaks = inner_outcome.breaks };
                break :blk ExprOutcome{ .value = .fresh, .breaks = inner_outcome.breaks };
            },

            .struct_access => |access| blk: {
                const source_outcome = try self.analyzeExpr(env, region, accumulator, access.struct_, loop_depth);
                const source = source_outcome.value orelse break :blk ExprOutcome{ .breaks = source_outcome.breaks };
                const projection = try self.singleProjectionSpan(.{ .field = @intCast(access.field_idx) });
                break :blk ExprOutcome{
                    .value = try self.borrowOrigin(source, region, projection),
                    .breaks = source_outcome.breaks,
                };
            },

            .run_low_level => |ll| blk: {
                const arg_exprs = self.mir_store.getExprSpan(ll.args);
                const arg_origins = try self.allocator.alloc(Origin, arg_exprs.len);
                defer self.allocator.free(arg_origins);
                var breaks = false;

                for (arg_exprs, 0..) |arg_expr, i| {
                    const arg_outcome = try self.analyzeExpr(env, region, accumulator, arg_expr, loop_depth);
                    breaks = breaks or arg_outcome.breaks;
                    const origin = arg_outcome.value orelse break :blk ExprOutcome{ .breaks = breaks };
                    arg_origins[i] = origin;
                }

                switch (ll.op) {
                    .list_get_unsafe => {
                        if (arg_origins.len == 0) {
                            std.debug.panic(
                                "ProcResultSummary invariant violated: list_get_unsafe must have a list argument",
                                .{},
                            );
                        }
                        break :blk ExprOutcome{
                            .value = try self.borrowOrigin(
                                arg_origins[0],
                                region,
                                RefProjectionSpan.empty(),
                            ),
                            .breaks = breaks,
                        };
                    },
                    else => break :blk switch (ll.op.procResultSemantics()) {
                        .fresh => ExprOutcome{ .value = .fresh, .breaks = breaks },
                        .borrow_arg => |arg_index| blk_inner: {
                            if (arg_index >= arg_origins.len) {
                                std.debug.panic(
                                    "ProcResultSummary invariant violated: low-level {s} borrows arg {d}, but call only has {d} args",
                                    .{ @tagName(ll.op), arg_index, arg_origins.len },
                                );
                            }
                            break :blk_inner ExprOutcome{
                                .value = try self.borrowOrigin(
                                    arg_origins[arg_index],
                                    region,
                                    RefProjectionSpan.empty(),
                                ),
                                .breaks = breaks,
                            };
                        },
                        .no_return => ExprOutcome{ .breaks = breaks },
                        .requires_explicit_summary => std.debug.panic(
                            "ProcResultSummary invariant violated: low-level result {s} requires explicit provenance summary",
                            .{@tagName(ll.op)},
                        ),
                    },
                }
            },

            .call => |call| blk: {
                const proc_id = self.resolveCallableProcId(call.func) orelse self.panicUnresolvedCallable(call.func);
                const arg_exprs = self.mir_store.getExprSpan(call.args);
                const arg_origins = try self.allocator.alloc(Origin, arg_exprs.len);
                defer self.allocator.free(arg_origins);
                var breaks = false;

                for (arg_exprs, 0..) |arg_expr, i| {
                    const arg_outcome = try self.analyzeExpr(env, region, accumulator, arg_expr, loop_depth);
                    breaks = breaks or arg_outcome.breaks;
                    const origin = arg_outcome.value orelse break :blk ExprOutcome{ .breaks = breaks };
                    arg_origins[i] = origin;
                }

                const callee_summary = self.procSummary(proc_id);
                break :blk switch (callee_summary) {
                    .no_return => ExprOutcome{ .breaks = breaks },
                    .concrete => |contract| ExprOutcome{
                        .value = try self.instantiateCallContract(contract, arg_origins, region),
                        .breaks = breaks,
                    },
                };
            },

            .block => |block| blk: {
                var block_env = try self.cloneEnv(env);
                defer block_env.deinit();
                var breaks = false;

                for (self.mir_store.getStmts(block.stmts)) |stmt| {
                    const stmt_outcome = try self.analyzeStmt(&block_env, region, accumulator, stmt, loop_depth);
                    breaks = breaks or stmt_outcome.breaks;
                    if (!stmt_outcome.hasNormalValue()) {
                        break :blk ExprOutcome{ .breaks = breaks };
                    }
                }

                const final_outcome = try self.analyzeExpr(&block_env, region, accumulator, block.final_expr, loop_depth);
                break :blk ExprOutcome{
                    .value = final_outcome.value,
                    .breaks = breaks or final_outcome.breaks,
                };
            },

            .borrow_scope => |scope| blk: {
                const scope_region = self.freshScopeRegion();
                var scope_env = try self.cloneEnv(env);
                defer scope_env.deinit();

                for (self.mir_store.getBorrowBindings(scope.bindings)) |binding| {
                    const source_outcome = try self.analyzeExpr(&scope_env, scope_region, accumulator, binding.expr, loop_depth);
                    const source = source_outcome.value orelse break :blk ExprOutcome{ .breaks = source_outcome.breaks };
                    try self.bindBorrowPattern(&scope_env, binding.pattern, source, scope_region);
                }

                const body_outcome = try self.analyzeExpr(&scope_env, scope_region, accumulator, scope.body, loop_depth);
                break :blk if (body_outcome.value) |origin| switch (origin) {
                    .borrow_of_param => |borrowed| switch (borrowed.region) {
                        .proc => ExprOutcome{ .value = origin, .breaks = body_outcome.breaks },
                        .scope => |scope_id| std.debug.panic(
                            "ProcResultSummary invariant violated: borrow scope result escaped local scope {d}",
                            .{scope_id},
                        ),
                    },
                    .borrow_of_fresh => |borrowed| switch (borrowed.region) {
                        .proc => ExprOutcome{ .value = origin, .breaks = body_outcome.breaks },
                        .scope => |scope_id| std.debug.panic(
                            "ProcResultSummary invariant violated: borrow scope result escaped local scope {d}",
                            .{scope_id},
                        ),
                    },
                    else => ExprOutcome{ .value = origin, .breaks = body_outcome.breaks },
                } else ExprOutcome{ .breaks = body_outcome.breaks };
            },

            .dbg_expr => |dbg_expr| self.analyzeExpr(env, region, accumulator, dbg_expr.expr, loop_depth),
            .expect => |expect| self.analyzeExpr(env, region, accumulator, expect.body, loop_depth),

            .runtime_err_can,
            .runtime_err_type,
            .runtime_err_ellipsis,
            .runtime_err_anno_only,
            .crash,
            => ExprOutcome.noReturn(),

            .return_expr => |ret| blk: {
                const outcome = try self.analyzeExpr(env, region, accumulator, ret.expr, loop_depth);
                if (outcome.value) |origin| {
                    self.mergeReturnedOrigin(accumulator, origin);
                }
                break :blk ExprOutcome{ .breaks = outcome.breaks };
            },

            .match_expr => |match_expr| try self.analyzeMatchExpr(
                env,
                region,
                accumulator,
                match_expr.cond,
                match_expr.branches,
                loop_depth,
            ),

            .loop => |loop_expr| blk: {
                const body_outcome = try self.analyzeExpr(env, region, accumulator, loop_expr.body, loop_depth + 1);
                break :blk if (body_outcome.breaks)
                    ExprOutcome.fromValue(.fresh)
                else
                    ExprOutcome.noReturn();
            },
            .break_expr => blk: {
                if (loop_depth == 0) {
                    std.debug.panic(
                        "ProcResultSummary invariant violated: break_expr escaped the nearest loop during strongest-form summary",
                        .{},
                    );
                }
                break :blk ExprOutcome{ .breaks = true };
            },
        };

        self.recordExprContract(expr_id, self.exprResultFromOutcome(outcome));
        return outcome;
    }

    fn analyzeProc(self: *Analyzer, proc_id: MIR.ProcId) Allocator.Error!ProcSummaryState {
        const proc = self.mir_store.getProc(proc_id);
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();

        const param_count = self.mir_store.procValueParamCount(proc);
        for (0..param_count) |i| {
            const param_pattern = self.mir_store.getProcValueParamPattern(proc, i);
            const origin: Origin = .{ .alias_of_param = .{
                .param_index = @intCast(i),
            } };
            try self.bindPattern(&env, param_pattern, origin, .proc);
        }

        var accumulator = ReturnAccumulator{};
        const outcome = try self.analyzeExpr(&env, .proc, &accumulator, proc.body, 0);
        return self.finishProcOutcome(&accumulator, outcome);
    }

    fn analyzeRootExpr(self: *Analyzer, expr_id: MIR.ExprId) Allocator.Error!ProcResultContract {
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();

        var accumulator = ReturnAccumulator{};
        const outcome = try self.analyzeExpr(&env, .proc, &accumulator, expr_id, 0);
        return switch (self.finishProcOutcome(&accumulator, outcome)) {
            .no_return => .fresh,
            .concrete => |contract| contract,
        };
    }
};

/// Builds proc and root result summaries from finished MIR.
pub fn build(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lambda_sets: *const LambdaSet.Store,
    root_expr_ids: []const MIR.ExprId,
) Allocator.Error!Table {
    var table = Table.init(allocator);
    errdefer table.deinit();
    var active_value_defs = std.AutoHashMapUnmanaged(u64, void){};
    defer active_value_defs.deinit(allocator);

    var states = std.ArrayList(ProcSummaryState).empty;
    defer states.deinit(allocator);
    try states.appendNTimes(allocator, .no_return, mir_store.procCount());

    var changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getProcs(), 0..) |_, i| {
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .lambda_sets = lambda_sets,
                .table = &table,
                .proc_states = states.items,
                .active_value_defs = &active_value_defs,
            };
            const proc_id: MIR.ProcId = @enumFromInt(@as(u32, @intCast(i)));
            const next_state = try analyzer.analyzeProc(proc_id);
            if (!summaryStatesEqual(&table, states.items[i], next_state)) {
                states.items[i] = next_state;
                changed = true;
            }
        }
    }

    try table.proc_contracts.ensureTotalCapacityPrecise(allocator, states.items.len);
    for (states.items) |state| {
        const contract = switch (state) {
            .no_return => ProcResultContract.fresh,
            .concrete => |inner| inner,
        };
        table.proc_contracts.appendAssumeCapacity(contract);
    }

    try table.expr_contracts.ensureTotalCapacityPrecise(allocator, mir_store.exprCount());
    try table.expr_contracts.appendNTimes(allocator, null, mir_store.exprCount());

    for (mir_store.getProcs(), 0..) |_, i| {
        var analyzer = Analyzer{
            .allocator = allocator,
            .mir_store = mir_store,
            .lambda_sets = lambda_sets,
            .table = &table,
            .proc_states = null,
            .active_value_defs = &active_value_defs,
        };
        const proc_id: MIR.ProcId = @enumFromInt(@as(u32, @intCast(i)));
        _ = try analyzer.analyzeProc(proc_id);
    }

    var seen_root_exprs = std.AutoHashMap(u32, void).init(allocator);
    defer seen_root_exprs.deinit();

    try table.root_contract_entries.ensureTotalCapacityPrecise(allocator, root_expr_ids.len);
    for (root_expr_ids) |expr_id| {
        const gop = try seen_root_exprs.getOrPut(@intFromEnum(expr_id));
        if (gop.found_existing) continue;

        var analyzer = Analyzer{
            .allocator = allocator,
            .mir_store = mir_store,
            .lambda_sets = lambda_sets,
            .table = &table,
            .proc_states = null,
            .active_value_defs = &active_value_defs,
        };
        const contract = try analyzer.analyzeRootExpr(expr_id);
        table.root_contract_entries.appendAssumeCapacity(.{
            .expr_id = expr_id,
            .contract = contract,
        });
    }

    for (1..table.root_contract_entries.items.len) |i| {
        var j = i;
        while (j > 0) {
            const current = table.root_contract_entries.items[j];
            const previous = table.root_contract_entries.items[j - 1];
            if (@intFromEnum(previous.expr_id) <= @intFromEnum(current.expr_id)) break;

            table.root_contract_entries.items[j - 1] = current;
            table.root_contract_entries.items[j] = previous;
            j -= 1;
        }
    }

    return table;
}

fn contractEqual(table: *const Table, left: ProcResultContract, right: ProcResultContract) bool {
    return switch (left) {
        .fresh => right == .fresh,
        .alias_of_param => |left_param| switch (right) {
            .alias_of_param => |right_param| left_param.param_index == right_param.param_index and projectionSpansEqual(table, left_param.projections, right_param.projections),
            else => false,
        },
        .borrow_of_param => |left_param| switch (right) {
            .borrow_of_param => |right_param| left_param.param_index == right_param.param_index and projectionSpansEqual(table, left_param.projections, right_param.projections),
            else => false,
        },
    };
}

fn mergeContracts(table: *const Table, left: ProcResultContract, right: ProcResultContract) ProcResultContract {
    return if (contractEqual(table, left, right))
        left
    else
        .fresh;
}

fn exprContractsEqual(table: *const Table, left: ExprResultContract, right: ExprResultContract) bool {
    return switch (left) {
        .no_return => right == .no_return,
        .borrow_of_fresh => right == .borrow_of_fresh,
        .concrete => |left_contract| switch (right) {
            .concrete => |right_contract| contractEqual(table, left_contract, right_contract),
            else => false,
        },
    };
}

fn summaryStatesEqual(table: *const Table, left: ProcSummaryState, right: ProcSummaryState) bool {
    return switch (left) {
        .no_return => right == .no_return,
        .concrete => |left_contract| switch (right) {
            .concrete => |right_contract| contractEqual(table, left_contract, right_contract),
            .no_return => false,
        },
    };
}

fn originEqual(table: *const Table, left: Origin, right: Origin) bool {
    return switch (left) {
        .fresh => right == .fresh,
        .alias_of_param => |left_param| switch (right) {
            .alias_of_param => |right_param| left_param.param_index == right_param.param_index and projectionSpansEqual(table, left_param.projections, right_param.projections),
            else => false,
        },
        .borrow_of_param => |left_param| switch (right) {
            .borrow_of_param => |right_param| left_param.param_index == right_param.param_index and std.meta.eql(left_param.region, right_param.region) and projectionSpansEqual(table, left_param.projections, right_param.projections),
            else => false,
        },
        .borrow_of_fresh => |left_fresh| switch (right) {
            .borrow_of_fresh => |right_fresh| std.meta.eql(left_fresh.region, right_fresh.region) and projectionSpansEqual(table, left_fresh.projections, right_fresh.projections),
            else => false,
        },
    };
}

fn mergeOrigins(table: *const Table, left: Origin, right: Origin) Origin {
    return if (originEqual(table, left, right))
        left
    else
        .fresh;
}

fn projectionSpansEqual(table: *const Table, left: RefProjectionSpan, right: RefProjectionSpan) bool {
    const left_items = table.getRefProjectionSpan(left);
    const right_items = table.getRefProjectionSpan(right);
    if (left_items.len != right_items.len) return false;
    for (left_items, right_items) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}
