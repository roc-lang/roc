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

const CallableSummary = @import("CallableSummary.zig");
const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

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
    no_return,
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

const ReturnAccumulator = struct {
    inferred: ?ResultContract = null,
};

const LocalOriginMap = std.AutoHashMap(u32, Origin);

const JoinOriginState = struct {
    params: []const MIR.LocalId,
    merged_origins: []?Origin,
    saw_incoming: bool,
};

const ActiveJoinMap = std.AutoHashMap(u32, JoinOriginState);

const CallableValueDef = union(enum) {
    symbol: MIR.Symbol,
    lambda: MIR.LambdaId,
    closure: MIR.LambdaId,
    alias: MIR.LocalId,
    nominal: MIR.LocalId,
    field: struct {
        source: MIR.LocalId,
        field_idx: u32,
    },
    tag_payload: struct {
        source: MIR.LocalId,
        payload_idx: u32,
    },
    struct_value: MIR.LocalSpan,
    tag_value: MIR.LocalSpan,
    call_result: struct {
        callee: MIR.LocalId,
    },
};

const CallableResolution = struct {
    lambda: MIR.LambdaId,
    requires_hidden_capture: bool,
};

const JoinCallableState = struct {
    params: []const MIR.LocalId,
    merged_callables: []?CallableResolution,
};

const ActiveJoinCallableMap = std.AutoHashMap(u32, JoinCallableState);

fn callableResolutionsEqual(left: CallableResolution, right: CallableResolution) bool {
    return left.lambda == right.lambda and left.requires_hidden_capture == right.requires_hidden_capture;
}

const Analyzer = struct {
    allocator: Allocator,
    mir_store: *const MIR.Store,
    table: *Table,
    callable_summary: *const CallableSummary.Table,
    lambda_states: ?[]const ResultContract,
    active_joins: *ActiveJoinMap,
    active_callable_joins: *ActiveJoinCallableMap,
    current_lambda: ?MIR.LambdaId,
    current_body_root: MIR.CFStmtId,
    callable_defs: *const std.AutoHashMap(u32, CallableValueDef),
    callable_overrides: *const std.AutoHashMap(u32, CallableResolution),

    fn lambdaSummary(self: *const Analyzer, lambda_id: MIR.LambdaId) ResultContract {
        if (self.lambda_states) |states| {
            return states[@intFromEnum(lambda_id)];
        }
        return self.table.getLambdaContract(lambda_id);
    }

    fn localKey(local: MIR.LocalId) u32 {
        return @intFromEnum(local);
    }

    fn singleProjectionSpan(self: *Analyzer, projection: RefProjection) Allocator.Error!RefProjectionSpan {
        return self.table.addRefProjectionSpan(&.{projection});
    }

    fn localIsCurrentLambdaParamOrCapture(self: *const Analyzer, local_id: MIR.LocalId) bool {
        const lambda_id = self.current_lambda orelse return false;
        const lambda = self.mir_store.getLambda(lambda_id);

        for (self.mir_store.getLocalSpan(lambda.params)) |param| {
            if (param == local_id) return true;
        }

        return lambda.captures_param == local_id;
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
            .no_return => std.debug.panic(
                "ResultSummary invariant violated: no-return callable must not be instantiated as a value-producing call result",
                .{},
            ),
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

    fn finishSummaryState(_: *Analyzer, accumulator: *ReturnAccumulator) ResultContract {
        if (accumulator.inferred) |contract| {
            return contract;
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

    fn localMonotypeIsFunc(self: *const Analyzer, local_id: MIR.LocalId) bool {
        return self.mir_store.monotype_store.getMonotype(self.mir_store.getLocal(local_id).monotype) == .func;
    }

    fn monotypeMayContainCallable(self: *const Analyzer, mono_idx: Monotype.Idx) bool {
        return switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
            .func => true,
            .record => |record| blk: {
                for (self.mir_store.monotype_store.getFields(record.fields)) |field| {
                    if (self.monotypeMayContainCallable(field.type_idx)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |tuple_data| blk: {
                for (self.mir_store.monotype_store.getIdxSpan(tuple_data.elems)) |elem| {
                    if (self.monotypeMayContainCallable(elem)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (self.mir_store.monotype_store.getTags(tag_union.tags)) |tag| {
                    for (self.mir_store.monotype_store.getIdxSpan(tag.payloads)) |payload| {
                        if (self.monotypeMayContainCallable(payload)) break :blk true;
                    }
                }
                break :blk false;
            },
            .box => |box_data| self.monotypeMayContainCallable(box_data.inner),
            .list,
            .prim,
            .unit,
            => false,
            .recursive_placeholder => std.debug.panic(
                "ResultSummary invariant violated: recursive_placeholder survived monotype construction",
                .{},
            ),
        };
    }

    fn localMayContainCallable(self: *const Analyzer, local_id: MIR.LocalId) bool {
        return self.monotypeMayContainCallable(self.mir_store.getLocal(local_id).monotype);
    }

    fn recordCallableDef(
        self: *Analyzer,
        defs: *std.AutoHashMap(u32, CallableValueDef),
        local_id: MIR.LocalId,
        def: CallableValueDef,
    ) Allocator.Error!void {
        if (!self.localMayContainCallable(local_id)) return;

        const key = localKey(local_id);
        const gop = try defs.getOrPut(key);
        if (gop.found_existing) {
            std.debug.panic(
                "ResultSummary invariant violated: function-valued local {d} had multiple reaching defs",
                .{@intFromEnum(local_id)},
            );
        }
        gop.value_ptr.* = def;
    }

    fn collectCallableDefs(
        self: *Analyzer,
        stmt_id: MIR.CFStmtId,
        defs: *std.AutoHashMap(u32, CallableValueDef),
        visited: *std.AutoHashMap(u32, void),
    ) Allocator.Error!void {
        const key = @as(u32, @intFromEnum(stmt_id));
        const gop = try visited.getOrPut(key);
        if (gop.found_existing) return;

        switch (self.mir_store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .symbol = assign.symbol });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_ref => |assign| {
                switch (assign.op) {
                    .local => |source| try self.recordCallableDef(defs, assign.target, .{ .alias = source }),
                    .nominal => |nominal| try self.recordCallableDef(defs, assign.target, .{ .nominal = nominal.backing }),
                    .field => |field| try self.recordCallableDef(defs, assign.target, .{ .field = .{
                        .source = field.source,
                        .field_idx = field.field_idx,
                    } }),
                    .tag_payload => |payload| try self.recordCallableDef(defs, assign.target, .{ .tag_payload = .{
                        .source = payload.source,
                        .payload_idx = payload.payload_idx,
                    } }),
                    .discriminant => {},
                }
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_literal => |assign| try self.collectCallableDefs(assign.next, defs, visited),
            .assign_lambda => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .lambda = assign.lambda });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_closure => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .closure = assign.lambda });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_call => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .call_result = .{
                    .callee = assign.callee,
                } });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_low_level => |assign| try self.collectCallableDefs(assign.next, defs, visited),
            .assign_list => |assign| try self.collectCallableDefs(assign.next, defs, visited),
            .assign_struct => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .struct_value = assign.fields });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .assign_tag => |assign| {
                try self.recordCallableDef(defs, assign.target, .{ .tag_value = assign.args });
                try self.collectCallableDefs(assign.next, defs, visited);
            },
            .debug => |stmt| try self.collectCallableDefs(stmt.next, defs, visited),
            .expect => |stmt| try self.collectCallableDefs(stmt.next, defs, visited),
            .runtime_error, .scope_exit, .jump, .ret, .crash => {},
            .switch_stmt => |switch_stmt| {
                for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectCallableDefs(branch.body, defs, visited);
                }
                try self.collectCallableDefs(switch_stmt.default_branch, defs, visited);
            },
            .borrow_scope => |scope_stmt| {
                try self.collectCallableDefs(scope_stmt.body, defs, visited);
                try self.collectCallableDefs(scope_stmt.remainder, defs, visited);
            },
            .join => |join_stmt| {
                try self.collectCallableDefs(join_stmt.body, defs, visited);
                try self.collectCallableDefs(join_stmt.remainder, defs, visited);
            },
        }
    }

    fn callableProjectionPathMatches(
        self: *const Analyzer,
        binding_span: MIR.CallableProjectionSpan,
        reversed_path: []const MIR.CallableProjection,
    ) bool {
        const binding = self.mir_store.getCallableProjectionSpan(binding_span);
        if (binding.len != reversed_path.len) return false;

        var i: usize = 0;
        while (i < binding.len) : (i += 1) {
            const binding_proj = binding[i];
            const path_proj = reversed_path[reversed_path.len - 1 - i];
            switch (binding_proj) {
                .field => |binding_idx| switch (path_proj) {
                    .field => |path_idx| if (binding_idx != path_idx) return false,
                    else => return false,
                },
                .tag_payload => |binding_idx| switch (path_proj) {
                    .tag_payload => |path_idx| if (binding_idx != path_idx) return false,
                    else => return false,
                },
                .nominal => switch (path_proj) {
                    .nominal => {},
                    else => return false,
                },
            }
        }

        return true;
    }

    fn resolveCallableForParamProjection(
        self: *Analyzer,
        source_param: MIR.LocalId,
        reversed_path: []const MIR.CallableProjection,
    ) CallableResolution {
        const lambda_id = self.current_lambda orelse std.debug.panic(
            "ResultSummary invariant violated: param-projection callable resolution escaped lambda analysis",
            .{},
        );
        const lambda = self.mir_store.getLambda(lambda_id);

        for (self.mir_store.getCallableBindings(lambda.callable_bindings)) |binding| {
            if (binding.source_param != source_param) continue;
            if (!self.callableProjectionPathMatches(binding.projections, reversed_path)) continue;
            return .{
                .lambda = binding.lambda,
                .requires_hidden_capture = binding.requires_hidden_capture,
            };
        }

        std.debug.panic(
            "ResultSummary TODO: missing exact callable binding for parameter local {d} in lambda {d} (bindings={d}, path_len={d})",
            .{
                @intFromEnum(source_param),
                @intFromEnum(lambda_id),
                self.mir_store.getCallableBindings(lambda.callable_bindings).len,
                reversed_path.len,
            },
        );
    }

    fn resolveCallableFromStructValue(
        self: *Analyzer,
        defs: *const std.AutoHashMap(u32, CallableValueDef),
        fields: MIR.LocalSpan,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (reversed_path.items.len == 0) {
            std.debug.panic(
                "ResultSummary invariant violated: callable resolution reached a non-callable struct value",
                .{},
            );
        }

        const next_projection = reversed_path.items[reversed_path.items.len - 1];
        const field_idx = switch (next_projection) {
            .field => |idx| idx,
            else => std.debug.panic(
                "ResultSummary invariant violated: callable struct resolution expected a field projection, found {s}",
                .{@tagName(next_projection)},
            ),
        };

        const field_locals = self.mir_store.getLocalSpan(fields);
        if (field_idx >= field_locals.len) {
            std.debug.panic(
                "ResultSummary invariant violated: callable field index {d} is out of bounds for arity {d}",
                .{ field_idx, field_locals.len },
            );
        }

        reversed_path.items.len -= 1;
        defer reversed_path.items.len += 1;
        return self.resolveCallableForLocal(defs, field_locals[field_idx], reversed_path);
    }

    fn resolveCallableFromTagValue(
        self: *Analyzer,
        defs: *const std.AutoHashMap(u32, CallableValueDef),
        args: MIR.LocalSpan,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (reversed_path.items.len == 0) {
            std.debug.panic(
                "ResultSummary invariant violated: callable resolution reached a non-callable tag value",
                .{},
            );
        }

        const next_projection = reversed_path.items[reversed_path.items.len - 1];
        const payload_idx = switch (next_projection) {
            .tag_payload => |idx| idx,
            else => std.debug.panic(
                "ResultSummary invariant violated: callable tag resolution expected a payload projection, found {s}",
                .{@tagName(next_projection)},
            ),
        };

        const payload_locals = self.mir_store.getLocalSpan(args);
        if (payload_idx >= payload_locals.len) {
            std.debug.panic(
                "ResultSummary invariant violated: callable payload index {d} is out of bounds for arity {d}",
                .{ payload_idx, payload_locals.len },
            );
        }

        reversed_path.items.len -= 1;
        defer reversed_path.items.len += 1;
        return self.resolveCallableForLocal(defs, payload_locals[payload_idx], reversed_path);
    }

    fn resolveCallableForLocal(
        self: *Analyzer,
        defs: *const std.AutoHashMap(u32, CallableValueDef),
        local_id: MIR.LocalId,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (self.callable_overrides.get(localKey(local_id))) |resolved| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "ResultSummary TODO: exact callable resolution through projected join-param values is not implemented yet",
                    .{},
                );
            }
            return resolved;
        }

        const def = defs.get(localKey(local_id)) orelse {
            if (!self.localIsCurrentLambdaParamOrCapture(local_id)) {
                std.debug.panic(
                    "ResultSummary invariant violated: function-valued local {d} had no callable definition or join binding",
                    .{@intFromEnum(local_id)},
                );
            }
            return self.resolveCallableForParamProjection(local_id, reversed_path.items);
        };

        return switch (def) {
            .symbol => |symbol| std.debug.panic(
                "ResultSummary invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
                .{symbol.raw()},
            ),
            .lambda => |lambda_id| .{
                .lambda = lambda_id,
                .requires_hidden_capture = false,
            },
            .closure => |lambda_id| .{
                .lambda = lambda_id,
                .requires_hidden_capture = true,
            },
            .alias => |source| blk: {
                const resolved = try self.resolveCallableForLocal(defs, source, reversed_path);
                break :blk .{
                    .lambda = resolved.lambda,
                    .requires_hidden_capture = resolved.requires_hidden_capture,
                };
            },
            .nominal => |backing| blk: {
                const resolved = try self.resolveCallableForLocal(defs, backing, reversed_path);
                break :blk .{
                    .lambda = resolved.lambda,
                    .requires_hidden_capture = resolved.requires_hidden_capture,
                };
            },
            .field => |field| blk: {
                if (defs.get(localKey(field.source))) |source_def| switch (source_def) {
                    .struct_value => |fields| {
                        const field_locals = self.mir_store.getLocalSpan(fields);
                        if (field.field_idx >= field_locals.len) {
                            std.debug.panic(
                                "ResultSummary invariant violated: callable field index {d} is out of bounds for arity {d}",
                                .{ field.field_idx, field_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableForLocal(defs, field_locals[field.field_idx], reversed_path);
                    },
                    .symbol, .call_result => {},
                    else => {},
                };

                try reversed_path.append(self.allocator, .{ .field = field.field_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableForLocal(defs, field.source, reversed_path);
            },
            .tag_payload => |payload| blk: {
                if (defs.get(localKey(payload.source))) |source_def| {
                    switch (source_def) {
                        .tag_value => |args| {
                            const payload_locals = self.mir_store.getLocalSpan(args);
                            if (payload.payload_idx >= payload_locals.len) {
                                std.debug.panic(
                                    "ResultSummary invariant violated: callable payload index {d} is out of bounds for arity {d}",
                                    .{ payload.payload_idx, payload_locals.len },
                                );
                            }
                            break :blk try self.resolveCallableForLocal(defs, payload_locals[payload.payload_idx], reversed_path);
                        },
                        .symbol, .call_result => {},
                        else => {},
                    }
                }

                try reversed_path.append(self.allocator, .{ .tag_payload = payload.payload_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableForLocal(defs, payload.source, reversed_path);
            },
            .struct_value => |fields| try self.resolveCallableFromStructValue(defs, fields, reversed_path),
            .tag_value => |args| try self.resolveCallableFromTagValue(defs, args, reversed_path),
            .call_result => |call_result| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "ResultSummary TODO: exact callable resolution through projected call results is not implemented yet",
                        .{},
                    );
                }

                const callee = try self.resolveCallableForLocal(defs, call_result.callee, reversed_path);
                return switch (self.callable_summary.getLambdaContract(callee.lambda)) {
                    .no_return => std.debug.panic(
                        "ResultSummary invariant violated: call-result callable resolution reached no-return lambda {d}",
                        .{ @intFromEnum(callee.lambda) },
                    ),
                    .exact_lambda => |lambda_id| .{
                        .lambda = lambda_id,
                        .requires_hidden_capture = false,
                    },
                    .exact_closure => |lambda_id| .{
                        .lambda = lambda_id,
                        .requires_hidden_capture = true,
                    },
                };
            },
        };
    }

    fn collectReturnedCallable(
        self: *Analyzer,
        stmt_id: MIR.CFStmtId,
        defs: *const std.AutoHashMap(u32, CallableValueDef),
        out: *?CallableResolution,
    ) Allocator.Error!void {
        switch (self.mir_store.getCFStmt(stmt_id)) {
            .assign_symbol => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_ref => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_literal => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_lambda => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_closure => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_call => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_low_level => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_list => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_struct => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .assign_tag => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .debug => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .expect => |stmt| try self.collectReturnedCallable(stmt.next, defs, out),
            .runtime_error, .scope_exit, .jump, .crash => {},
            .switch_stmt => |switch_stmt| {
                for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectReturnedCallable(branch.body, defs, out);
                }
                try self.collectReturnedCallable(switch_stmt.default_branch, defs, out);
            },
            .borrow_scope => |scope_stmt| {
                try self.collectReturnedCallable(scope_stmt.body, defs, out);
                try self.collectReturnedCallable(scope_stmt.remainder, defs, out);
            },
            .join => |join_stmt| {
                try self.collectReturnedCallable(join_stmt.body, defs, out);
                try self.collectReturnedCallable(join_stmt.remainder, defs, out);
            },
            .ret => |ret_stmt| {
                if (!self.localMonotypeIsFunc(ret_stmt.value)) return;
                var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                defer reversed_path.deinit(self.allocator);
                const resolved = try self.resolveCallableForLocal(defs, ret_stmt.value, &reversed_path);
                if (out.*) |current| {
                    if (current.lambda != resolved.lambda or current.requires_hidden_capture != resolved.requires_hidden_capture) {
                        std.debug.panic(
                            "ResultSummary invariant violated: callable body returned incompatible callable identities",
                            .{},
                        );
                    }
                } else {
                    out.* = resolved;
                }
            },
        }
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
            .assign_ref => |assign| {
                const origin = switch (assign.op) {
                    .local => |source| if (self.localMonotypeIsFunc(source))
                        .fresh
                    else
                        self.originForLocal(env, source),
                    .discriminant => .fresh,
                    .field => |field| try self.borrowOrigin(
                        self.originForLocal(env, field.source),
                        region,
                        try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
                    ),
                    .tag_payload => |payload| try self.borrowOrigin(
                        self.originForLocal(env, payload.source),
                        region,
                        try self.singleProjectionSpan(.tag_payload),
                    ),
                    .nominal => |nominal| try self.aliasOrigin(
                        self.originForLocal(env, nominal.backing),
                        try self.singleProjectionSpan(.nominal),
                    ),
                };
                try env.put(localKey(assign.target), origin);
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
                var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                defer reversed_path.deinit(self.allocator);
                const callee = try self.resolveCallableForLocal(self.callable_defs, assign.callee, &reversed_path);

                const args = self.mir_store.getLocalSpan(assign.args);
                const arg_origins = try self.allocator.alloc(Origin, args.len + @intFromBool(callee.requires_hidden_capture));
                defer self.allocator.free(arg_origins);

                for (args, 0..) |arg, i| {
                    arg_origins[i] = self.originForLocal(env, arg);
                }
                if (callee.requires_hidden_capture) {
                    arg_origins[args.len] = self.originForLocal(env, assign.callee);
                }

                switch (self.lambdaSummary(callee.lambda)) {
                    .no_return => return false,
                    else => try env.put(
                        localKey(assign.target),
                        try self.instantiateCallContract(self.lambdaSummary(callee.lambda), arg_origins, region),
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
            .debug => |stmt| return self.analyzeStmt(env, region, accumulator, stmt.next),
            .expect => |stmt| return self.analyzeStmt(env, region, accumulator, stmt.next),
            .runtime_error => return false,
            .switch_stmt => |switch_stmt| {
                for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                    var branch_env = try self.cloneEnv(env);
                    defer branch_env.deinit();
                    _ = try self.analyzeStmt(&branch_env, region, accumulator, branch.body);
                }
                var default_env = try self.cloneEnv(env);
                defer default_env.deinit();
                _ = try self.analyzeStmt(&default_env, region, accumulator, switch_stmt.default_branch);
                return false;
            },
            .borrow_scope => |scope| {
                var scope_env = try self.cloneEnv(env);
                defer scope_env.deinit();

                const scope_region: BorrowRegion = .{ .scope = scope.id };
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
                const merged_callables = try self.allocator.alloc(?CallableResolution, join_params.len);
                errdefer self.allocator.free(merged_callables);
                @memset(merged_callables, null);

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
                    .saw_incoming = false,
                };
                const callable_gop = try self.active_callable_joins.getOrPut(join_key);
                if (callable_gop.found_existing) {
                    std.debug.panic(
                        "ResultSummary invariant violated: nested/duplicate active callable join {d}",
                        .{join_key},
                    );
                }
                callable_gop.value_ptr.* = .{
                    .params = join_params,
                    .merged_callables = merged_callables,
                };
                defer {
                    const removed = self.active_joins.fetchRemove(join_key) orelse unreachable;
                    self.allocator.free(removed.value.merged_origins);
                    const callable_removed = self.active_callable_joins.fetchRemove(join_key) orelse unreachable;
                    self.allocator.free(callable_removed.value.merged_callables);
                }

                const remainder_continues = try self.analyzeStmt(env, region, accumulator, join.remainder);
                if (remainder_continues) {
                    std.debug.panic(
                        "ResultSummary invariant violated: join {d} remainder fell through without explicit jump",
                        .{join_key},
                    );
                }

                if (!gop.value_ptr.saw_incoming) return false;

                var body_env = try self.cloneEnv(env);
                defer body_env.deinit();
                var body_callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
                defer body_callable_overrides.deinit();
                try body_callable_overrides.ensureTotalCapacity(@intCast(join_params.len));

                for (join_params, merged_origins, merged_callables, 0..) |param, incoming, incoming_callable, i| {
                    try body_env.put(
                        localKey(param),
                        incoming orelse std.debug.panic(
                            "ResultSummary invariant violated: join {d} param {d} had no incoming origin",
                            .{ join_key, i },
                        ),
                    );
                    if (incoming_callable) |callable| {
                        body_callable_overrides.putAssumeCapacity(localKey(param), callable);
                    }
                }

                const saved_callable_overrides = self.callable_overrides;
                self.callable_overrides = &body_callable_overrides;
                defer self.callable_overrides = saved_callable_overrides;

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
                const callable_join_state = self.active_callable_joins.getPtr(@intFromEnum(jump.id)) orelse std.debug.panic(
                    "ResultSummary invariant violated: jump to unknown active callable join {d}",
                    .{@intFromEnum(jump.id)},
                );

                join_state.saw_incoming = true;
                for (args, 0..) |arg, i| {
                    const incoming = self.originForLocal(env, arg);
                    if (join_state.merged_origins[i]) |current| {
                        join_state.merged_origins[i] = self.mergeOrigins(current, incoming);
                    } else {
                        join_state.merged_origins[i] = incoming;
                    }

                    const param = callable_join_state.params[i];
                    if (self.localMayContainCallable(param)) {
                        var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                        defer reversed_path.deinit(self.allocator);
                        const incoming_callable = try self.resolveCallableForLocal(self.callable_defs, arg, &reversed_path);
                        if (callable_join_state.merged_callables[i]) |current_callable| {
                            if (!callableResolutionsEqual(current_callable, incoming_callable)) {
                                std.debug.panic(
                                    "ResultSummary TODO: merging incompatible exact callable identities across join {d} param {d} is not implemented yet",
                                    .{ @intFromEnum(jump.id), i },
                                );
                            }
                        } else {
                            callable_join_state.merged_callables[i] = incoming_callable;
                        }
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

    fn analyzeLambda(self: *Analyzer, lambda_id: MIR.LambdaId) Allocator.Error!ResultContract {
        const lambda = self.mir_store.getLambda(lambda_id);
        var callable_defs = std.AutoHashMap(u32, CallableValueDef).init(self.allocator);
        defer callable_defs.deinit();
        var callable_visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer callable_visited.deinit();
        try self.collectCallableDefs(lambda.body, &callable_defs, &callable_visited);

        self.callable_defs = &callable_defs;
        self.current_lambda = lambda_id;
        self.current_body_root = lambda.body;
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();
        var callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
        defer callable_overrides.deinit();
        var active_joins = ActiveJoinMap.init(self.allocator);
        defer {
            var it = active_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_origins);
            }
            active_joins.deinit();
        }
        var active_callable_joins = ActiveJoinCallableMap.init(self.allocator);
        defer {
            var it = active_callable_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_callables);
            }
            active_callable_joins.deinit();
        }

        self.active_joins = &active_joins;
        self.active_callable_joins = &active_callable_joins;
        self.callable_overrides = &callable_overrides;

        const visible_params = self.mir_store.getLocalSpan(lambda.params);
        for (visible_params, 0..) |param_local, i| {
            try env.put(localKey(param_local), .{ .alias_of_param = .{
                .param_index = @intCast(i),
            } });
        }

        if (lambda.captures_param) |captures_param| {
            try env.put(localKey(captures_param), .{ .alias_of_param = .{
                .param_index = @intCast(visible_params.len),
            } });
        }

        var accumulator = ReturnAccumulator{};
        _ = try self.analyzeStmt(&env, .body, &accumulator, lambda.body);
        return self.finishSummaryState(&accumulator);
    }

    fn analyzeConst(self: *Analyzer, const_id: MIR.ConstDefId) Allocator.Error!ResultContract {
        const def = self.mir_store.getConstDef(const_id);
        var callable_defs = std.AutoHashMap(u32, CallableValueDef).init(self.allocator);
        defer callable_defs.deinit();
        var callable_visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer callable_visited.deinit();
        try self.collectCallableDefs(def.body, &callable_defs, &callable_visited);

        self.callable_defs = &callable_defs;
        self.current_lambda = null;
        self.current_body_root = def.body;
        var env = LocalOriginMap.init(self.allocator);
        defer env.deinit();
        var callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
        defer callable_overrides.deinit();
        var active_joins = ActiveJoinMap.init(self.allocator);
        defer {
            var it = active_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_origins);
            }
            active_joins.deinit();
        }
        var active_callable_joins = ActiveJoinCallableMap.init(self.allocator);
        defer {
            var it = active_callable_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_callables);
            }
            active_callable_joins.deinit();
        }

        self.active_joins = &active_joins;
        self.active_callable_joins = &active_callable_joins;
        self.callable_overrides = &callable_overrides;

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
    callable_summary: *const CallableSummary.Table,
) Allocator.Error!Table {
    var table = Table.init(allocator);
    errdefer table.deinit();

    for (requested_root_consts) |const_id| {
        _ = mir_store.getConstDef(const_id);
    }

    var reachable_lambdas = std.AutoHashMap(u32, void).init(allocator);
    defer reachable_lambdas.deinit();
    var reachable_consts = std.AutoHashMap(u32, void).init(allocator);
    defer reachable_consts.deinit();
    var visited_reachable_stmts = std.AutoHashMap(u32, void).init(allocator);
    defer visited_reachable_stmts.deinit();
    for (requested_root_consts) |const_id| {
        try collectReachableConst(mir_store, const_id, &reachable_lambdas, &reachable_consts, &visited_reachable_stmts);
    }

    var states = std.ArrayList(ResultContract).empty;
    defer states.deinit(allocator);
    try states.appendNTimes(allocator, .no_return, mir_store.getLambdas().len);

    var changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getLambdas(), 0..) |_, i| {
            if (!reachable_lambdas.contains(@intCast(i))) continue;
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .callable_summary = callable_summary,
                .lambda_states = states.items,
                .active_joins = undefined,
                .active_callable_joins = undefined,
                .current_lambda = null,
                .current_body_root = undefined,
                .callable_defs = undefined,
                .callable_overrides = undefined,
            };

            const lambda_id: MIR.LambdaId = @enumFromInt(@as(u32, @intCast(i)));
            const next_state = try analyzer.analyzeLambda(lambda_id);
            if (!procContractsEqual(&table, states.items[i], next_state)) {
                states.items[i] = next_state;
                changed = true;
            }
        }
    }

    try table.lambda_contracts.ensureTotalCapacityPrecise(allocator, states.items.len);
    for (states.items) |state| {
        table.lambda_contracts.appendAssumeCapacity(state);
    }

    try table.const_contracts.ensureTotalCapacityPrecise(allocator, mir_store.const_defs.items.len);
    for (mir_store.const_defs.items, 0..) |_, i| {
        if (!reachable_consts.contains(@intCast(i))) {
            table.const_contracts.appendAssumeCapacity(.no_return);
            continue;
        }
        var analyzer = Analyzer{
            .allocator = allocator,
            .mir_store = mir_store,
            .table = &table,
            .callable_summary = callable_summary,
            .lambda_states = null,
            .active_joins = undefined,
            .active_callable_joins = undefined,
            .current_lambda = null,
            .current_body_root = undefined,
            .callable_defs = undefined,
            .callable_overrides = undefined,
        };
        const const_id: MIR.ConstDefId = @enumFromInt(@as(u32, @intCast(i)));
        const contract = try analyzer.analyzeConst(const_id);
        table.const_contracts.appendAssumeCapacity(contract);
    }

    return table;
}

fn collectReachableLambda(
    mir_store: *const MIR.Store,
    lambda_id: MIR.LambdaId,
    reachable_lambdas: *std.AutoHashMap(u32, void),
    reachable_consts: *std.AutoHashMap(u32, void),
    visited_stmts: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try reachable_lambdas.getOrPut(@intFromEnum(lambda_id));
    if (gop.found_existing) return;
    try collectReachableFromStmt(mir_store, mir_store.getLambda(lambda_id).body, reachable_lambdas, reachable_consts, visited_stmts);
}

fn collectReachableConst(
    mir_store: *const MIR.Store,
    const_id: MIR.ConstDefId,
    reachable_lambdas: *std.AutoHashMap(u32, void),
    reachable_consts: *std.AutoHashMap(u32, void),
    visited_stmts: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try reachable_consts.getOrPut(@intFromEnum(const_id));
    if (gop.found_existing) return;
    try collectReachableFromStmt(mir_store, mir_store.getConstDef(const_id).body, reachable_lambdas, reachable_consts, visited_stmts);
}

fn collectReachableFromStmt(
    mir_store: *const MIR.Store,
    stmt_id: MIR.CFStmtId,
    reachable_lambdas: *std.AutoHashMap(u32, void),
    reachable_consts: *std.AutoHashMap(u32, void),
    visited_stmts: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try visited_stmts.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (mir_store.getCFStmt(stmt_id)) {
        .assign_symbol => |stmt| {
            if (mir_store.getConstDefForSymbol(stmt.symbol)) |const_id| {
                try collectReachableConst(mir_store, const_id, reachable_lambdas, reachable_consts, visited_stmts);
            }
            try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_ref => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_literal => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_lambda => |stmt| {
            try collectReachableLambda(mir_store, stmt.lambda, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_closure => |stmt| {
            try collectReachableLambda(mir_store, stmt.lambda, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_call => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_low_level => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_list => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_struct => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_tag => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .debug => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .expect => |stmt| try collectReachableFromStmt(mir_store, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .runtime_error, .scope_exit, .jump, .ret, .crash => {},
        .switch_stmt => |stmt| {
            for (mir_store.getSwitchBranches(stmt.branches)) |branch| {
                try collectReachableFromStmt(mir_store, branch.body, reachable_lambdas, reachable_consts, visited_stmts);
            }
            try collectReachableFromStmt(mir_store, stmt.default_branch, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .borrow_scope => |stmt| {
            try collectReachableFromStmt(mir_store, stmt.body, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(mir_store, stmt.remainder, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .join => |stmt| {
            try collectReachableFromStmt(mir_store, stmt.body, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(mir_store, stmt.remainder, reachable_lambdas, reachable_consts, visited_stmts);
        },
    }
}

fn mergeContracts(table: *const Table, left: ResultContract, right: ResultContract) ResultContract {
    return if (procContractsEqual(table, left, right))
        left
    else
        .fresh;
}

fn procContractsEqual(table: *const Table, left: ResultContract, right: ResultContract) bool {
    return switch (left) {
        .no_return => right == .no_return,
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
