//! MIR proc result-summary analysis.
//!
//! This is the release-path source of truth for proc result provenance.
//! It computes param-relative proc result contracts before LIR lowering so
//! call-result semantics do not depend on post-hoc analysis of finished LIR.

const std = @import("std");

const MIR = @import("MIR.zig");

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
    root_contract_entries: std.ArrayList(RootContractEntry),

    /// Initializes an empty summary table.
    pub fn init(allocator: Allocator) Table {
        return .{
            .allocator = allocator,
            .ref_projections = std.ArrayList(RefProjection).empty,
            .proc_contracts = std.ArrayList(ProcResultContract).empty,
            .root_contract_entries = std.ArrayList(RootContractEntry).empty,
        };
    }

    /// Releases all storage owned by this summary table.
    pub fn deinit(self: *Table) void {
        self.ref_projections.deinit(self.allocator);
        self.proc_contracts.deinit(self.allocator);
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

const ExprOutcome = union(enum) {
    no_return,
    value: Origin,
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
    table: *Table,
    proc_states: ?[]const ProcSummaryState,
    next_scope_id: u32 = 0,

    fn procSummary(self: *const Analyzer, proc_id: MIR.ProcId) ProcSummaryState {
        if (self.proc_states) |states| {
            return states[@intFromEnum(proc_id)];
        }
        return .{ .concrete = self.table.getProcContract(proc_id) };
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
            if (!contractEqual(self.table, current, next)) {
                std.debug.panic(
                    "ProcResultSummary invariant violated: proc returns disagree on result provenance",
                    .{},
                );
            }
        } else {
            accumulator.inferred = next;
        }
    }

    fn contractFromOrigin(_: *Analyzer, origin: Origin) ProcResultContract {
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
            .borrow_of_fresh => std.debug.panic(
                "ProcResultSummary invariant violated: borrowed return is not rooted in a proc parameter",
                .{},
            ),
        };
    }

    fn mergeReturnedOrigin(self: *Analyzer, accumulator: *ReturnAccumulator, origin: Origin) void {
        self.mergeReturnContract(accumulator, self.contractFromOrigin(origin));
    }

    fn finishProcOutcome(
        self: *Analyzer,
        accumulator: *ReturnAccumulator,
        outcome: ExprOutcome,
    ) ProcSummaryState {
        switch (outcome) {
            .no_return => {
                if (accumulator.inferred) |contract| {
                    return .{ .concrete = contract };
                }
                return .no_return;
            },
            .value => |origin| {
                self.mergeReturnedOrigin(accumulator, origin);
                return .{ .concrete = accumulator.inferred.? };
            },
        }
    }

    fn cloneEnv(self: *Analyzer, source: *const LocalOriginMap) Allocator.Error!LocalOriginMap {
        var clone = LocalOriginMap.init(self.allocator);
        var it = source.iterator();
        while (it.next()) |entry| {
            try clone.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        return clone;
    }

    fn bindPattern(self: *Analyzer, env: *LocalOriginMap, pattern_id: MIR.PatternId, origin: Origin) Allocator.Error!void {
        switch (self.mir_store.getPattern(pattern_id)) {
            .bind => |symbol| try env.put(symbol.raw(), origin),
            .wildcard => {},
            .as_pattern => |as_pattern| {
                try env.put(as_pattern.symbol.raw(), origin);
                try self.bindPattern(env, as_pattern.pattern, origin);
            },
            else => std.debug.panic(
                "ProcResultSummary invariant violated: pattern {s} must be lowered before proc result summary",
                .{@tagName(self.mir_store.getPattern(pattern_id))},
            ),
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
        try self.bindPattern(env, pattern_id, borrowed);
    }

    fn analyzeStmt(self: *Analyzer, env: *LocalOriginMap, region: BorrowRegion, accumulator: *ReturnAccumulator, stmt: MIR.Stmt) Allocator.Error!ExprOutcome {
        const binding = switch (stmt) {
            .decl_const => |inner| inner,
            .decl_var => |inner| inner,
            .mutate_var => |inner| inner,
        };
        const outcome = try self.analyzeExpr(env, region, accumulator, binding.expr);
        switch (outcome) {
            .no_return => return .no_return,
            .value => |origin| {
                try self.bindPattern(env, binding.pattern, origin);
                return .{ .value = .fresh };
            },
        }
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
    ) Allocator.Error!ExprOutcome {
        return switch (self.mir_store.getExpr(expr_id)) {
            .int,
            .frac_f32,
            .frac_f64,
            .dec,
            .str,
            .proc_ref,
            .closure_make,
            => .{ .value = .fresh },

            .lookup => |symbol| .{ .value = env.get(symbol.raw()) orelse std.debug.panic(
                "ProcResultSummary invariant violated: lookup symbol {d} has no proc-local provenance",
                .{symbol.raw()},
            ) },

            .list => |list_data| blk: {
                for (self.mir_store.getExprSpan(list_data.elems)) |elem_expr| {
                    switch (try self.analyzeExpr(env, region, accumulator, elem_expr)) {
                        .no_return => break :blk .no_return,
                        .value => {},
                    }
                }
                break :blk .{ .value = .fresh };
            },

            .struct_ => |struct_data| blk: {
                for (self.mir_store.getExprSpan(struct_data.fields)) |field_expr| {
                    switch (try self.analyzeExpr(env, region, accumulator, field_expr)) {
                        .no_return => break :blk .no_return,
                        .value => {},
                    }
                }
                break :blk .{ .value = .fresh };
            },

            .tag => |tag_data| blk: {
                for (self.mir_store.getExprSpan(tag_data.args)) |arg_expr| {
                    switch (try self.analyzeExpr(env, region, accumulator, arg_expr)) {
                        .no_return => break :blk .no_return,
                        .value => {},
                    }
                }
                break :blk .{ .value = .fresh };
            },

            .str_escape_and_quote => |inner| blk: {
                switch (try self.analyzeExpr(env, region, accumulator, inner)) {
                    .no_return => break :blk .no_return,
                    .value => break :blk .{ .value = .fresh },
                }
            },

            .struct_access => |access| blk: {
                const source = switch (try self.analyzeExpr(env, region, accumulator, access.struct_)) {
                    .no_return => break :blk .no_return,
                    .value => |origin| origin,
                };
                const projection = try self.singleProjectionSpan(.{ .field = @intCast(access.field_idx) });
                break :blk .{ .value = try self.borrowOrigin(source, region, projection) };
            },

            .run_low_level => |ll| blk: {
                const arg_exprs = self.mir_store.getExprSpan(ll.args);
                const arg_origins = try self.allocator.alloc(Origin, arg_exprs.len);
                defer self.allocator.free(arg_origins);

                for (arg_exprs, 0..) |arg_expr, i| {
                    switch (try self.analyzeExpr(env, region, accumulator, arg_expr)) {
                        .no_return => break :blk .no_return,
                        .value => |origin| arg_origins[i] = origin,
                    }
                }

                switch (ll.op) {
                    .list_get_unsafe => {
                        if (arg_origins.len == 0) {
                            std.debug.panic(
                                "ProcResultSummary invariant violated: list_get_unsafe must have a list argument",
                                .{},
                            );
                        }
                        break :blk .{ .value = try self.borrowOrigin(
                            arg_origins[0],
                            region,
                            RefProjectionSpan.empty(),
                        ) };
                    },
                    else => break :blk switch (ll.op.procResultSemantics()) {
                        .fresh => .{ .value = .fresh },
                        .borrow_arg => |arg_index| blk_inner: {
                            if (arg_index >= arg_origins.len) {
                                std.debug.panic(
                                    "ProcResultSummary invariant violated: low-level {s} borrows arg {d}, but call only has {d} args",
                                    .{ @tagName(ll.op), arg_index, arg_origins.len },
                                );
                            }
                            break :blk_inner .{ .value = try self.borrowOrigin(
                                arg_origins[arg_index],
                                region,
                                RefProjectionSpan.empty(),
                            ) };
                        },
                        .no_return => .no_return,
                        .requires_explicit_summary => std.debug.panic(
                            "ProcResultSummary invariant violated: low-level result {s} requires explicit provenance summary",
                            .{@tagName(ll.op)},
                        ),
                    },
                }
            },

            .call => |call| blk: {
                const proc_id = self.mir_store.resolveCallableProcId(call.func) orelse std.debug.panic(
                    "ProcResultSummary invariant violated: call callee must be a direct proc-backed MIR value before strongest-form lowering",
                    .{},
                );
                const arg_exprs = self.mir_store.getExprSpan(call.args);
                const arg_origins = try self.allocator.alloc(Origin, arg_exprs.len);
                defer self.allocator.free(arg_origins);

                for (arg_exprs, 0..) |arg_expr, i| {
                    switch (try self.analyzeExpr(env, region, accumulator, arg_expr)) {
                        .no_return => break :blk .no_return,
                        .value => |origin| arg_origins[i] = origin,
                    }
                }

                const callee_summary = self.procSummary(proc_id);
                break :blk switch (callee_summary) {
                    .no_return => .no_return,
                    .concrete => |contract| .{ .value = try self.instantiateCallContract(contract, arg_origins, region) },
                };
            },

            .block => |block| blk: {
                var block_env = try self.cloneEnv(env);
                defer block_env.deinit();

                for (self.mir_store.getStmts(block.stmts)) |stmt| {
                    switch (try self.analyzeStmt(&block_env, region, accumulator, stmt)) {
                        .no_return => break :blk .no_return,
                        .value => {},
                    }
                }

                break :blk try self.analyzeExpr(&block_env, region, accumulator, block.final_expr);
            },

            .borrow_scope => |scope| blk: {
                const scope_region = self.freshScopeRegion();
                var scope_env = try self.cloneEnv(env);
                defer scope_env.deinit();

                for (self.mir_store.getBorrowBindings(scope.bindings)) |binding| {
                    const source = switch (try self.analyzeExpr(&scope_env, scope_region, accumulator, binding.expr)) {
                        .no_return => break :blk .no_return,
                        .value => |origin| origin,
                    };
                    try self.bindBorrowPattern(&scope_env, binding.pattern, source, scope_region);
                }

                const body_outcome = try self.analyzeExpr(&scope_env, scope_region, accumulator, scope.body);
                break :blk switch (body_outcome) {
                    .no_return => .no_return,
                    .value => |origin| switch (origin) {
                        .borrow_of_param => |borrowed| switch (borrowed.region) {
                            .proc => .{ .value = origin },
                            .scope => |scope_id| std.debug.panic(
                                "ProcResultSummary invariant violated: borrow scope result escaped local scope {d}",
                                .{scope_id},
                            ),
                        },
                        .borrow_of_fresh => |borrowed| switch (borrowed.region) {
                            .proc => .{ .value = origin },
                            .scope => |scope_id| std.debug.panic(
                                "ProcResultSummary invariant violated: borrow scope result escaped local scope {d}",
                                .{scope_id},
                            ),
                        },
                        else => .{ .value = origin },
                    },
                };
            },

            .dbg_expr => |dbg_expr| self.analyzeExpr(env, region, accumulator, dbg_expr.expr),
            .expect => |expect| self.analyzeExpr(env, region, accumulator, expect.body),

            .runtime_err_can,
            .runtime_err_type,
            .runtime_err_ellipsis,
            .runtime_err_anno_only,
            .crash,
            => .no_return,

            .return_expr => |ret| blk: {
                const outcome = try self.analyzeExpr(env, region, accumulator, ret.expr);
                switch (outcome) {
                    .no_return => break :blk .no_return,
                    .value => |origin| {
                        self.mergeReturnedOrigin(accumulator, origin);
                        break :blk .no_return;
                    },
                }
            },

            .match_expr, .loop, .break_expr => std.debug.panic(
                "ProcResultSummary requires control-flow lowering before strongest-form MIR proc summary for expr tag {s}",
                .{@tagName(self.mir_store.getExpr(expr_id))},
            ),
        };
    }

    fn analyzeProc(self: *Analyzer, proc_id: MIR.ProcId) Allocator.Error!ProcSummaryState {
        const proc = self.mir_store.getProc(proc_id);
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();

        const params = self.mir_store.getPatternSpan(proc.params);
        for (params, 0..) |param_pattern, i| {
            switch (self.mir_store.getPattern(param_pattern)) {
                .bind => |symbol| try env.put(symbol.raw(), .{ .alias_of_param = .{
                    .param_index = @intCast(i),
                } }),
                .wildcard => {},
                .as_pattern => |as_pattern| {
                    const origin: Origin = .{ .alias_of_param = .{
                        .param_index = @intCast(i),
                    } };
                    try env.put(as_pattern.symbol.raw(), origin);
                    try self.bindPattern(&env, as_pattern.pattern, origin);
                },
                else => std.debug.panic(
                    "ProcResultSummary invariant violated: proc param pattern {s} must be lowered before strongest-form summary",
                    .{@tagName(self.mir_store.getPattern(param_pattern))},
                ),
            }
        }

        var accumulator = ReturnAccumulator{};
        const outcome = try self.analyzeExpr(&env, .proc, &accumulator, proc.body);
        return self.finishProcOutcome(&accumulator, outcome);
    }

    fn analyzeRootExpr(self: *Analyzer, expr_id: MIR.ExprId) Allocator.Error!ProcResultContract {
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();

        var accumulator = ReturnAccumulator{};
        const outcome = try self.analyzeExpr(&env, .proc, &accumulator, expr_id);
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
    root_expr_ids: []const MIR.ExprId,
) Allocator.Error!Table {
    var table = Table.init(allocator);
    errdefer table.deinit();

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
                .table = &table,
                .proc_states = states.items,
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

    var seen_root_exprs = std.AutoHashMap(u32, void).init(allocator);
    defer seen_root_exprs.deinit();

    try table.root_contract_entries.ensureTotalCapacityPrecise(allocator, root_expr_ids.len);
    for (root_expr_ids) |expr_id| {
        const gop = try seen_root_exprs.getOrPut(@intFromEnum(expr_id));
        if (gop.found_existing) continue;

        var analyzer = Analyzer{
            .allocator = allocator,
            .mir_store = mir_store,
            .table = &table,
            .proc_states = null,
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

fn summaryStatesEqual(table: *const Table, left: ProcSummaryState, right: ProcSummaryState) bool {
    return switch (left) {
        .no_return => right == .no_return,
        .concrete => |left_contract| switch (right) {
            .concrete => |right_contract| contractEqual(table, left_contract, right_contract),
            .no_return => false,
        },
    };
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
