//! MIR lambda/constant result-summary analysis.
//!
//! This is the release-path source of truth for param-relative result
//! provenance before MIR lowers to LIR. It operates directly on strongest-form
//! statement MIR:
//! - lambda bodies are summarized relative to their parameters
//! - top-level constants are summarized from their statement bodies
//! - summaries preserve field/tag/nominal projection paths
//! - scoped borrows are rejected if they escape via `ret`

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

/// Lambda- or const-level summary of result provenance.
pub const ResultContract = union(enum) {
    fresh,
    alias_of_param: ParamRefContract,
    borrow_of_param: ParamRefContract,
};

/// Finalized MIR result-summary table.
pub const Table = struct {
    allocator: Allocator,
    ref_projections: std.ArrayList(RefProjection),
    lambda_contracts: std.ArrayList(ResultContract),
    const_contracts: std.ArrayList(ResultContract),

    /// Initializes an empty summary table.
    pub fn init(allocator: Allocator) Table {
        return .{
            .allocator = allocator,
            .ref_projections = std.ArrayList(RefProjection).empty,
            .lambda_contracts = std.ArrayList(ResultContract).empty,
            .const_contracts = std.ArrayList(ResultContract).empty,
        };
    }

    /// Releases all storage owned by this summary table.
    pub fn deinit(self: *Table) void {
        self.ref_projections.deinit(self.allocator);
        self.lambda_contracts.deinit(self.allocator);
        self.const_contracts.deinit(self.allocator);
    }

    fn addRefProjectionSpan(self: *Table, projections: []const RefProjection) Allocator.Error!RefProjectionSpan {
        if (projections.len == 0) return RefProjectionSpan.empty();

        const start = @as(u32, @intCast(self.ref_projections.items.len));
        try self.ref_projections.appendSlice(self.allocator, projections);
        return .{ .start = start, .len = @intCast(projections.len) };
    }

    /// Resolves one stored projection span to its projection slice.
    pub fn getRefProjectionSpan(self: *const Table, span: RefProjectionSpan) []const RefProjection {
        if (span.len == 0) return &.{};

        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.ref_projections.items.len) {
            std.debug.panic(
                "ResultSummary invariant violated: projection span start={d} len={d} exceeds ref-projection storage len={d}",
                .{ span.start, span.len, self.ref_projections.items.len },
            );
        }

        return self.ref_projections.items[span.start..][0..span.len];
    }

    /// Returns the precomputed result contract for one MIR lambda.
    pub fn getLambdaContract(self: *const Table, lambda_id: MIR.LambdaId) ResultContract {
        return self.lambda_contracts.items[@intFromEnum(lambda_id)];
    }

    /// Returns the precomputed result contract for one MIR top-level constant.
    pub fn getConstContract(self: *const Table, const_id: MIR.ConstDefId) ResultContract {
        return self.const_contracts.items[@intFromEnum(const_id)];
    }
};

const BorrowRegion = union(enum) {
    body,
    scope: MIR.BorrowScopeId,
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

const CallableSummaryState = union(enum) {
    no_return,
    concrete: ResultContract,
};

const ReturnAccumulator = struct {
    inferred: ?ResultContract = null,
};

const LocalOriginMap = std.AutoHashMap(u32, Origin);

const JoinOriginState = struct {
    params: []const MIR.LocalId,
    merged_origins: []?Origin,
};

const ActiveJoinMap = std.AutoHashMap(u32, JoinOriginState);

const Analyzer = struct {
    allocator: Allocator,
    mir_store: *const MIR.Store,
    table: *Table,
    lambda_states: ?[]const CallableSummaryState,
    active_joins: *ActiveJoinMap,

    fn lambdaSummary(self: *const Analyzer, lambda_id: MIR.LambdaId) CallableSummaryState {
        if (self.lambda_states) |states| {
            return states[@intFromEnum(lambda_id)];
        }
        return .{ .concrete = self.table.getLambdaContract(lambda_id) };
    }

    fn localKey(local: MIR.LocalId) u32 {
        return @intFromEnum(local);
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

    fn instantiateCallContract(
        self: *Analyzer,
        contract: ResultContract,
        arg_origins: []const Origin,
        region: BorrowRegion,
    ) Allocator.Error!Origin {
        return switch (contract) {
            .fresh => .fresh,
            .alias_of_param => |param_ref| blk: {
                if (param_ref.param_index >= arg_origins.len) {
                    std.debug.panic(
                        "ResultSummary invariant violated: lambda result aliases arg {d}, but call only has {d} args",
                        .{ param_ref.param_index, arg_origins.len },
                    );
                }
                break :blk try self.aliasOrigin(arg_origins[param_ref.param_index], param_ref.projections);
            },
            .borrow_of_param => |param_ref| blk: {
                if (param_ref.param_index >= arg_origins.len) {
                    std.debug.panic(
                        "ResultSummary invariant violated: lambda result borrows arg {d}, but call only has {d} args",
                        .{ param_ref.param_index, arg_origins.len },
                    );
                }
                break :blk try self.borrowOrigin(arg_origins[param_ref.param_index], region, param_ref.projections);
            },
        };
    }

    fn mergeReturnContract(
        self: *Analyzer,
        accumulator: *ReturnAccumulator,
        next: ResultContract,
    ) void {
        if (accumulator.inferred) |current| {
            accumulator.inferred = mergeContracts(self.table, current, next);
        } else {
            accumulator.inferred = next;
        }
    }

    fn resultContractFromOrigin(_: *Analyzer, origin: Origin) ResultContract {
        return switch (origin) {
            .fresh => .fresh,
            .alias_of_param => |aliased| .{ .alias_of_param = aliased },
            .borrow_of_param => |borrowed| switch (borrowed.region) {
                .body => .{ .borrow_of_param = .{
                    .param_index = borrowed.param_index,
                    .projections = borrowed.projections,
                } },
                .scope => |scope_id| std.debug.panic(
                    "ResultSummary invariant violated: scoped borrow from scope {d} escaped via return",
                    .{@intFromEnum(scope_id)},
                ),
            },
            .borrow_of_fresh => .fresh,
        };
    }

    fn mergeReturnedOrigin(self: *Analyzer, accumulator: *ReturnAccumulator, origin: Origin) void {
        self.mergeReturnContract(accumulator, self.resultContractFromOrigin(origin));
    }

    fn finishSummaryState(_: *Analyzer, accumulator: *ReturnAccumulator) CallableSummaryState {
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

    fn originIsScopedBorrow(origin: Origin) bool {
        return switch (origin) {
            .borrow_of_param => |borrowed| switch (borrowed.region) {
                .body => false,
                .scope => true,
            },
            .borrow_of_fresh => |borrowed| switch (borrowed.region) {
                .body => false,
                .scope => true,
            },
            else => false,
        };
    }

    fn originsEqual(self: *const Analyzer, left: Origin, right: Origin) bool {
        return switch (left) {
            .fresh => right == .fresh,
            .alias_of_param => |left_param| switch (right) {
                .alias_of_param => |right_param| left_param.param_index == right_param.param_index and
                    projectionSpansEqual(self.table, left_param.projections, right_param.projections),
                else => false,
            },
            .borrow_of_param => |left_borrow| switch (right) {
                .borrow_of_param => |right_borrow| left_borrow.param_index == right_borrow.param_index and
                    projectionSpansEqual(self.table, left_borrow.projections, right_borrow.projections) and
                    std.meta.eql(left_borrow.region, right_borrow.region),
                else => false,
            },
            .borrow_of_fresh => |left_borrow| switch (right) {
                .borrow_of_fresh => |right_borrow| projectionSpansEqual(self.table, left_borrow.projections, right_borrow.projections) and
                    std.meta.eql(left_borrow.region, right_borrow.region),
                else => false,
            },
        };
    }

    fn mergeOrigins(self: *const Analyzer, left: Origin, right: Origin) Origin {
        if (self.originsEqual(left, right)) return left;

        if (originIsScopedBorrow(left) or originIsScopedBorrow(right)) {
            std.debug.panic(
                "ResultSummary invariant violated: join merged incompatible scoped-borrow origins",
                .{},
            );
        }

        return .fresh;
    }

    fn originForLocal(_: *Analyzer, env: *const LocalOriginMap, local: MIR.LocalId) Origin {
        return env.get(localKey(local)) orelse std.debug.panic(
            "ResultSummary invariant violated: local {d} had no known origin during summary",
            .{@intFromEnum(local)},
        );
    }

    fn bindPattern(
        self: *Analyzer,
        env: *LocalOriginMap,
        pattern_id: MIR.PatternId,
        origin: Origin,
        region: BorrowRegion,
    ) Allocator.Error!void {
        switch (self.mir_store.getPattern(pattern_id)) {
            .bind => |local| try env.put(localKey(local), origin),
            .wildcard => {},
            .as_pattern => |as_pattern| {
                try env.put(localKey(as_pattern.local), origin);
                try self.bindPattern(env, as_pattern.pattern, origin, region);
            },
            .tag => |tag| {
                const args = self.mir_store.getPatternSpan(tag.args);
                if (args.len == 0) return;
                if (args.len != 1) {
                    std.debug.panic(
                        "ResultSummary invariant violated: strongest-form MIR tag patterns must have at most one wrapped payload pattern",
                        .{},
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
                "ResultSummary invariant violated: list destructure is not implemented in strongest-form MIR result summaries",
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

    fn discardScopeBoundResults(self: *Analyzer, env: *LocalOriginMap, scope_id: MIR.BorrowScopeId) void {
        var to_remove = std.ArrayList(u32).empty;
        defer to_remove.deinit(self.allocator);

        var it = env.iterator();
        while (it.next()) |entry| {
            const should_remove = switch (entry.value_ptr.*) {
                .fresh, .alias_of_param => false,
                .borrow_of_param => |borrowed| switch (borrowed.region) {
                    .body => false,
                    .scope => |borrowed_scope| borrowed_scope == scope_id,
                },
                .borrow_of_fresh => |borrowed| switch (borrowed.region) {
                    .body => false,
                    .scope => |borrowed_scope| borrowed_scope == scope_id,
                },
            };
            if (should_remove) {
                to_remove.append(self.allocator, entry.key_ptr.*) catch unreachable;
            }
        }

        for (to_remove.items) |key| {
            _ = env.remove(key);
        }
    }

    fn analyzeStmt(
        self: *Analyzer,
        env: *LocalOriginMap,
        region: BorrowRegion,
        accumulator: *ReturnAccumulator,
        stmt_id: MIR.CFStmtId,
    ) Allocator.Error!bool {
        switch (self.mir_store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_alias => |assign| {
                try env.put(localKey(assign.target), self.originForLocal(env, assign.source));
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_literal => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_lambda => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_closure => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_call => |assign| {
                const args = self.mir_store.getLocalSpan(assign.args);
                const arg_origins = try self.allocator.alloc(Origin, args.len);
                defer self.allocator.free(arg_origins);

                for (args, 0..) |arg, i| {
                    arg_origins[i] = self.originForLocal(env, arg);
                }

                const summary = self.lambdaSummary(assign.lambda);
                switch (summary) {
                    .no_return => return false,
                    .concrete => |contract| try env.put(
                        localKey(assign.target),
                        try self.instantiateCallContract(contract, arg_origins, region),
                    ),
                }
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_closure_call => |assign| {
                const args = self.mir_store.getLocalSpan(assign.args);
                const arg_origins = try self.allocator.alloc(Origin, args.len + 1);
                defer self.allocator.free(arg_origins);

                for (args, 0..) |arg, i| {
                    arg_origins[i] = self.originForLocal(env, arg);
                }
                arg_origins[args.len] = self.originForLocal(env, assign.closure);

                const summary = self.lambdaSummary(assign.lambda);
                switch (summary) {
                    .no_return => return false,
                    .concrete => |contract| try env.put(
                        localKey(assign.target),
                        try self.instantiateCallContract(contract, arg_origins, region),
                    ),
                }
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_low_level => |assign| {
                const args = self.mir_store.getLocalSpan(assign.args);
                const arg_origins = try self.allocator.alloc(Origin, args.len);
                defer self.allocator.free(arg_origins);

                for (args, 0..) |arg, i| {
                    arg_origins[i] = self.originForLocal(env, arg);
                }

                const origin = switch (assign.op.procResultSemantics()) {
                    .fresh => Origin.fresh,
                    .borrow_arg => |arg_index| blk: {
                        if (arg_index >= arg_origins.len) {
                            std.debug.panic(
                                "ResultSummary invariant violated: low-level {s} borrows arg {d}, but call only has {d} args",
                                .{ @tagName(assign.op), arg_index, arg_origins.len },
                            );
                        }
                        break :blk try self.borrowOrigin(arg_origins[arg_index], region, RefProjectionSpan.empty());
                    },
                    .no_return => return false,
                    .requires_explicit_summary => std.debug.panic(
                        "ResultSummary invariant violated: low-level result {s} requires explicit provenance summary",
                        .{@tagName(assign.op)},
                    ),
                };

                try env.put(localKey(assign.target), origin);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_list => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_struct => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_tag => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_field => |assign| {
                const source = self.originForLocal(env, assign.source);
                const projection = try self.singleProjectionSpan(.{ .field = @intCast(assign.field_idx) });
                try env.put(localKey(assign.target), try self.borrowOrigin(source, region, projection));
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_tag_payload => |assign| {
                _ = assign.payload_idx;
                const source = self.originForLocal(env, assign.source);
                const projection = try self.singleProjectionSpan(.tag_payload);
                try env.put(localKey(assign.target), try self.borrowOrigin(source, region, projection));
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_nominal => |assign| {
                const source = self.originForLocal(env, assign.backing);
                const projection = try self.singleProjectionSpan(.nominal);
                try env.put(localKey(assign.target), try self.aliasOrigin(source, projection));
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .assign_str_escape_and_quote => |assign| {
                try env.put(localKey(assign.target), .fresh);
                return self.analyzeStmt(env, region, accumulator, assign.next);
            },
            .debug => |debug_stmt| {
                _ = debug_stmt.value;
                return self.analyzeStmt(env, region, accumulator, debug_stmt.next);
            },
            .expect => |expect_stmt| {
                _ = expect_stmt.condition;
                return self.analyzeStmt(env, region, accumulator, expect_stmt.next);
            },
            .runtime_error => return false,
            .match_stmt => |match_stmt| {
                const scrutinee = self.originForLocal(env, match_stmt.scrutinee);
                for (self.mir_store.getMatchBranches(match_stmt.branches)) |branch| {
                    for (self.mir_store.getBranchPatterns(branch.patterns)) |branch_pattern| {
                        _ = branch_pattern.degenerate;
                        var branch_env = try self.cloneEnv(env);
                        defer branch_env.deinit();

                        try self.bindPattern(&branch_env, branch_pattern.pattern, scrutinee, region);
                        _ = try self.analyzeStmt(&branch_env, region, accumulator, branch.body);
                    }
                }
                return false;
            },
            .borrow_scope => |scope| {
                var scope_env = try self.cloneEnv(env);
                defer scope_env.deinit();

                const scope_region: BorrowRegion = .{ .scope = scope.id };
                for (self.mir_store.getBorrowBindings(scope.bindings)) |binding| {
                    const source_origin = self.originForLocal(&scope_env, binding.source);
                    try self.bindBorrowPattern(&scope_env, binding.pattern, source_origin, scope_region);
                }

                _ = try self.analyzeStmt(&scope_env, scope_region, accumulator, scope.body);
                self.discardScopeBoundResults(&scope_env, scope.id);
                return self.analyzeStmt(&scope_env, region, accumulator, scope.remainder);
            },
            .scope_exit => return false,
            .join => |join| {
                const join_key = @intFromEnum(join.id);
                const join_params = self.mir_store.getLocalSpan(join.params);
                const merged_origins = try self.allocator.alloc(?Origin, join_params.len);
                errdefer self.allocator.free(merged_origins);
                @memset(merged_origins, null);

                const gop = try self.active_joins.getOrPut(join_key);
                if (gop.found_existing) {
                    std.debug.panic(
                        "ResultSummary invariant violated: nested/duplicate active join {d}",
                        .{join_key},
                    );
                }
                gop.value_ptr.* = .{
                    .params = join_params,
                    .merged_origins = merged_origins,
                };
                defer {
                    const removed = self.active_joins.fetchRemove(join_key) orelse unreachable;
                    self.allocator.free(removed.value.merged_origins);
                }

                const remainder_continues = try self.analyzeStmt(env, region, accumulator, join.remainder);
                if (remainder_continues) {
                    std.debug.panic(
                        "ResultSummary invariant violated: join {d} remainder fell through without explicit jump",
                        .{join_key},
                    );
                }

                var saw_incoming = false;
                for (merged_origins) |incoming| {
                    if (incoming != null) {
                        saw_incoming = true;
                        break;
                    }
                }
                if (!saw_incoming) return false;

                var body_env = try self.cloneEnv(env);
                defer body_env.deinit();

                for (join_params, merged_origins, 0..) |param, incoming, i| {
                    try body_env.put(
                        localKey(param),
                        incoming orelse std.debug.panic(
                            "ResultSummary invariant violated: join {d} param {d} had no incoming origin",
                            .{ join_key, i },
                        ),
                    );
                }

                return self.analyzeStmt(&body_env, region, accumulator, join.body);
            },
            .jump => |jump| {
                const join_state = self.active_joins.getPtr(@intFromEnum(jump.id)) orelse std.debug.panic(
                    "ResultSummary invariant violated: jump to unknown active join {d}",
                    .{@intFromEnum(jump.id)},
                );
                const args = self.mir_store.getLocalSpan(jump.args);
                if (args.len != join_state.params.len) {
                    std.debug.panic(
                        "ResultSummary invariant violated: jump to join {d} passed {d} args, expected {d}",
                        .{ @intFromEnum(jump.id), args.len, join_state.params.len },
                    );
                }

                for (args, 0..) |arg, i| {
                    const incoming = self.originForLocal(env, arg);
                    if (join_state.merged_origins[i]) |current| {
                        join_state.merged_origins[i] = self.mergeOrigins(current, incoming);
                    } else {
                        join_state.merged_origins[i] = incoming;
                    }
                }
                return false;
            },
            .ret => |ret| {
                self.mergeReturnedOrigin(accumulator, self.originForLocal(env, ret.value));
                return false;
            },
            .crash => return false,
        }
    }

    fn analyzeLambda(self: *Analyzer, lambda_id: MIR.LambdaId) Allocator.Error!CallableSummaryState {
        const lambda = self.mir_store.getLambda(lambda_id);
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();
        var active_joins = ActiveJoinMap.init(self.allocator);
        defer {
            var it = active_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_origins);
            }
            active_joins.deinit();
        }

        self.active_joins = &active_joins;

        const visible_params = self.mir_store.getPatternSpan(lambda.params);
        for (visible_params, 0..) |pattern_id, i| {
            const origin: Origin = .{ .alias_of_param = .{
                .param_index = @intCast(i),
            } };
            try self.bindPattern(&env, pattern_id, origin, .body);
        }

        if (!lambda.captures_param.isNone()) {
            const origin: Origin = .{ .alias_of_param = .{
                .param_index = @intCast(visible_params.len),
            } };
            try self.bindPattern(&env, lambda.captures_param, origin, .body);
        }

        var accumulator = ReturnAccumulator{};
        _ = try self.analyzeStmt(&env, .body, &accumulator, lambda.body);
        return self.finishSummaryState(&accumulator);
    }

    fn analyzeConst(self: *Analyzer, const_id: MIR.ConstDefId) Allocator.Error!CallableSummaryState {
        const def = self.mir_store.getConstDef(const_id);
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();
        var active_joins = ActiveJoinMap.init(self.allocator);
        defer {
            var it = active_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_origins);
            }
            active_joins.deinit();
        }

        self.active_joins = &active_joins;

        var accumulator = ReturnAccumulator{};
        _ = try self.analyzeStmt(&env, .body, &accumulator, def.body);
        return self.finishSummaryState(&accumulator);
    }
};

/// Builds lambda and constant result summaries from finished statement MIR.
pub fn build(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    requested_root_consts: []const MIR.ConstDefId,
) Allocator.Error!Table {
    var table = Table.init(allocator);
    errdefer table.deinit();

    for (requested_root_consts) |const_id| {
        _ = mir_store.getConstDef(const_id);
    }

    var states = std.ArrayList(CallableSummaryState).empty;
    defer states.deinit(allocator);
    try states.appendNTimes(allocator, .no_return, mir_store.getLambdas().len);

    var changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getLambdas(), 0..) |_, i| {
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .lambda_states = states.items,
                .active_joins = undefined,
            };
            const lambda_id: MIR.LambdaId = @enumFromInt(@as(u32, @intCast(i)));
            const next_state = try analyzer.analyzeLambda(lambda_id);
            if (!summaryStatesEqual(&table, states.items[i], next_state)) {
                states.items[i] = next_state;
                changed = true;
            }
        }
    }

    try table.lambda_contracts.ensureTotalCapacityPrecise(allocator, states.items.len);
    for (states.items) |state| {
        const contract = switch (state) {
            .no_return => ResultContract.fresh,
            .concrete => |inner| inner,
        };
        table.lambda_contracts.appendAssumeCapacity(contract);
    }

    try table.const_contracts.ensureTotalCapacityPrecise(allocator, mir_store.const_defs.items.len);
    for (mir_store.const_defs.items, 0..) |_, i| {
        var analyzer = Analyzer{
            .allocator = allocator,
            .mir_store = mir_store,
            .table = &table,
            .lambda_states = null,
            .active_joins = undefined,
        };
        const const_id: MIR.ConstDefId = @enumFromInt(@as(u32, @intCast(i)));
        const state = try analyzer.analyzeConst(const_id);
        const contract = switch (state) {
            .no_return => ResultContract.fresh,
            .concrete => |inner| inner,
        };
        table.const_contracts.appendAssumeCapacity(contract);
    }

    return table;
}

fn summaryStatesEqual(table: *const Table, left: CallableSummaryState, right: CallableSummaryState) bool {
    return switch (left) {
        .no_return => right == .no_return,
        .concrete => |left_contract| switch (right) {
            .concrete => |right_contract| procContractsEqual(table, left_contract, right_contract),
            else => false,
        },
    };
}

fn mergeContracts(table: *const Table, left: ResultContract, right: ResultContract) ResultContract {
    return if (procContractsEqual(table, left, right))
        left
    else
        .fresh;
}

fn procContractsEqual(table: *const Table, left: ResultContract, right: ResultContract) bool {
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

fn projectionSpansEqual(table: *const Table, left: RefProjectionSpan, right: RefProjectionSpan) bool {
    const left_items = table.getRefProjectionSpan(left);
    const right_items = table.getRefProjectionSpan(right);
    if (left_items.len != right_items.len) return false;
    for (left_items, right_items) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}
