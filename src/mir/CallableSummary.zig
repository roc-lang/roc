//! MIR exact-callable summary analysis.
//!
//! This release-path analysis computes the exact callable identity returned by
//! each strongest-form MIR lambda and constant body whenever that identity is
//! fully determined after monomorphization. It is intentionally stricter than
//! general lambda-set reasoning:
//! - it returns only exact lambda/closure identities
//! - unsupported aggregate/call-result projection cases must panic loudly
//! - generic higher-order ambiguity must already have been specialized away

const std = @import("std");

const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;

/// Exact callable-return contract for one MIR lambda or constant.
pub const Contract = union(enum) {
    no_return,
    exact_lambda: MIR.LambdaId,
    exact_closure: MIR.LambdaId,
};

/// Finalized exact-callable summary table.
pub const Table = struct {
    allocator: Allocator,
    lambda_contracts: std.ArrayList(Contract),
    const_contracts: std.ArrayList(Contract),

    /// Initializes an empty callable-summary table.
    pub fn init(allocator: Allocator) Table {
        return .{
            .allocator = allocator,
            .lambda_contracts = std.ArrayList(Contract).empty,
            .const_contracts = std.ArrayList(Contract).empty,
        };
    }

    /// Releases all storage owned by this summary table.
    pub fn deinit(self: *Table) void {
        self.lambda_contracts.deinit(self.allocator);
        self.const_contracts.deinit(self.allocator);
    }

    /// Returns the precomputed exact-callable contract for one MIR lambda.
    pub fn getLambdaContract(self: *const Table, lambda_id: MIR.LambdaId) Contract {
        return self.lambda_contracts.items[@intFromEnum(lambda_id)];
    }

    /// Returns the precomputed exact-callable contract for one MIR constant.
    pub fn getConstContract(self: *const Table, const_id: MIR.ConstDefId) Contract {
        return self.const_contracts.items[@intFromEnum(const_id)];
    }
};

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

const Analyzer = struct {
    allocator: Allocator,
    mir_store: *const MIR.Store,
    table: *Table,
    lambda_states: ?[]const Contract,
    const_states: ?[]const Contract,
    current_lambda: ?MIR.LambdaId,
    value_defs: *const std.AutoHashMap(u32, CallableValueDef),

    fn lambdaContract(self: *const Analyzer, lambda_id: MIR.LambdaId) Contract {
        if (self.lambda_states) |states| return states[@intFromEnum(lambda_id)];
        return self.table.getLambdaContract(lambda_id);
    }

    fn constContract(self: *const Analyzer, const_id: MIR.ConstDefId) Contract {
        if (self.const_states) |states| return states[@intFromEnum(const_id)];
        return self.table.getConstContract(const_id);
    }

    fn localKey(local: MIR.LocalId) u32 {
        return @intFromEnum(local);
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
                "CallableSummary invariant violated: recursive_placeholder survived monotype construction",
                .{},
            ),
        };
    }

    fn localMayContainCallable(self: *const Analyzer, local_id: MIR.LocalId) bool {
        return self.monotypeMayContainCallable(self.mir_store.getLocal(local_id).monotype);
    }

    fn contractsEqual(left: Contract, right: Contract) bool {
        return switch (left) {
            .no_return => right == .no_return,
            .exact_lambda => |left_lambda| switch (right) {
                .exact_lambda => |right_lambda| left_lambda == right_lambda,
                else => false,
            },
            .exact_closure => |left_lambda| switch (right) {
                .exact_closure => |right_lambda| left_lambda == right_lambda,
                else => false,
            },
        };
    }

    fn mergeContracts(left: Contract, right: Contract) Contract {
        if (contractsEqual(left, right)) return left;
        std.debug.panic(
            "CallableSummary TODO: merging incompatible exact callable identities is not implemented yet",
            .{},
        );
    }

    fn resolutionToContract(resolution: CallableResolution) Contract {
        return if (resolution.requires_hidden_capture)
            .{ .exact_closure = resolution.lambda }
        else
            .{ .exact_lambda = resolution.lambda };
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
                "CallableSummary invariant violated: MIR local {d} had multiple reaching value defs",
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
            "CallableSummary TODO: exact callable resolution for function-valued params outside lambda bodies is not implemented yet",
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
            "CallableSummary TODO: missing exact callable binding for parameter local {d}",
            .{@intFromEnum(source_param)},
        );
    }

    fn resolveCallableForLocalPath(
        self: *Analyzer,
        local_id: MIR.LocalId,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        const def = self.value_defs.get(localKey(local_id)) orelse {
            return self.resolveCallableForParamProjection(local_id, reversed_path.items);
        };

        return switch (def) {
            .lambda => |lambda_id| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "CallableSummary invariant violated: callable projections cannot target a direct lambda value",
                        .{},
                    );
                }
                return .{
                    .lambda = lambda_id,
                    .requires_hidden_capture = false,
                };
            },
            .closure => |lambda_id| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "CallableSummary invariant violated: callable projections cannot target a direct closure value",
                        .{},
                    );
                }
                return .{
                    .lambda = lambda_id,
                    .requires_hidden_capture = true,
                };
            },
            .alias => |source| self.resolveCallableForLocalPath(source, reversed_path),
            .nominal => |backing| self.resolveCallableForLocalPath(backing, reversed_path),
            .field => |field| blk: {
                const source_def = self.value_defs.get(localKey(field.source));
                if (source_def) |resolved_source_def| switch (resolved_source_def) {
                    .struct_value => |fields| {
                        const field_locals = self.mir_store.getLocalSpan(fields);
                        if (field.field_idx >= field_locals.len) {
                            std.debug.panic(
                                "CallableSummary invariant violated: callable field index {d} exceeds struct arity {d}",
                                .{ field.field_idx, field_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableForLocalPath(field_locals[field.field_idx], reversed_path);
                    },
                    .symbol, .call_result => {},
                    else => {},
                };

                try reversed_path.append(self.allocator, .{ .field = field.field_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableForLocalPath(field.source, reversed_path);
            },
            .tag_payload => |payload| blk: {
                const source_def = self.value_defs.get(localKey(payload.source));
                if (source_def) |resolved_source_def| switch (resolved_source_def) {
                    .tag_value => |args| {
                        const payload_locals = self.mir_store.getLocalSpan(args);
                        if (payload.payload_idx >= payload_locals.len) {
                            std.debug.panic(
                                "CallableSummary invariant violated: callable payload index {d} exceeds tag arity {d}",
                                .{ payload.payload_idx, payload_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableForLocalPath(payload_locals[payload.payload_idx], reversed_path);
                    },
                    .symbol, .call_result => {},
                    else => {},
                };

                try reversed_path.append(self.allocator, .{ .tag_payload = payload.payload_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableForLocalPath(payload.source, reversed_path);
            },
            .symbol => |symbol| std.debug.panic(
                "CallableSummary invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
                .{symbol.raw()},
            ),
            .call_result => |call_result| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "CallableSummary TODO: exact callable resolution through projected call results is not implemented yet",
                        .{},
                    );
                }

                const callee = try self.resolveCallableForLocalPath(
                    call_result.callee,
                    reversed_path,
                );
                return switch (self.lambdaContract(callee.lambda)) {
                    .no_return => std.debug.panic(
                        "CallableSummary invariant violated: call-result callable resolution reached no-return lambda {d}",
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
            .struct_value => std.debug.panic(
                "CallableSummary invariant violated: callable resolution reached a non-callable struct value",
                .{},
            ),
            .tag_value => std.debug.panic(
                "CallableSummary invariant violated: callable resolution reached a non-callable tag value",
                .{},
            ),
        };
    }

    fn collectReturnedCallable(
        self: *Analyzer,
        stmt_id: MIR.CFStmtId,
        out: *?Contract,
    ) Allocator.Error!void {
        switch (self.mir_store.getCFStmt(stmt_id)) {
            .assign_symbol => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_ref => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_literal => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_lambda => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_closure => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_call => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_low_level => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_list => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_struct => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .assign_tag => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .debug => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .expect => |stmt| try self.collectReturnedCallable(stmt.next, out),
            .runtime_error, .scope_exit, .jump, .crash => {},
            .switch_stmt => |switch_stmt| {
                for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectReturnedCallable(branch.body, out);
                }
                try self.collectReturnedCallable(switch_stmt.default_branch, out);
            },
            .borrow_scope => |scope_stmt| {
                try self.collectReturnedCallable(scope_stmt.body, out);
                try self.collectReturnedCallable(scope_stmt.remainder, out);
            },
            .join => |join_stmt| {
                try self.collectReturnedCallable(join_stmt.body, out);
                try self.collectReturnedCallable(join_stmt.remainder, out);
            },
            .ret => |ret_stmt| {
                if (!self.localMonotypeIsFunc(ret_stmt.value)) return;

                var projections = std.ArrayList(MIR.CallableProjection).empty;
                defer projections.deinit(self.allocator);
                const resolved = try self.resolveCallableForLocalPath(ret_stmt.value, &projections);
                const contract = resolutionToContract(resolved);

                if (out.*) |current| {
                    out.* = mergeContracts(current, contract);
                } else {
                    out.* = contract;
                }
            },
        }
    }

    fn analyzeLambda(self: *Analyzer, lambda_id: MIR.LambdaId) Allocator.Error!Contract {
        const lambda = self.mir_store.getLambda(lambda_id);
        var defs = std.AutoHashMap(u32, CallableValueDef).init(self.allocator);
        defer defs.deinit();
        var visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer visited.deinit();
        try self.collectCallableDefs(lambda.body, &defs, &visited);

        self.current_lambda = lambda_id;
        self.value_defs = &defs;

        var returned: ?Contract = null;
        try self.collectReturnedCallable(lambda.body, &returned);
        return returned orelse .no_return;
    }

    fn analyzeConst(self: *Analyzer, const_id: MIR.ConstDefId) Allocator.Error!Contract {
        const def = self.mir_store.getConstDef(const_id);
        var defs = std.AutoHashMap(u32, CallableValueDef).init(self.allocator);
        defer defs.deinit();
        var visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer visited.deinit();
        try self.collectCallableDefs(def.body, &defs, &visited);

        self.current_lambda = null;
        self.value_defs = &defs;

        var returned: ?Contract = null;
        try self.collectReturnedCallable(def.body, &returned);
        return returned orelse .no_return;
    }
};

/// Builds exact-callable summaries from finished strongest-form MIR.
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

    var lambda_states = std.ArrayList(Contract).empty;
    defer lambda_states.deinit(allocator);
    try lambda_states.appendNTimes(allocator, .no_return, mir_store.getLambdas().len);

    var changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getLambdas(), 0..) |_, i| {
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .lambda_states = lambda_states.items,
                .const_states = null,
                .current_lambda = null,
                .value_defs = undefined,
            };
            const next = try analyzer.analyzeLambda(@enumFromInt(@as(u32, @intCast(i))));
            if (!Analyzer.contractsEqual(lambda_states.items[i], next)) {
                lambda_states.items[i] = next;
                changed = true;
            }
        }
    }

    try table.lambda_contracts.appendSlice(allocator, lambda_states.items);

    var const_states = std.ArrayList(Contract).empty;
    defer const_states.deinit(allocator);
    try const_states.appendNTimes(allocator, .no_return, mir_store.getConstDefs().len);

    changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getConstDefs(), 0..) |_, i| {
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .lambda_states = table.lambda_contracts.items,
                .const_states = const_states.items,
                .current_lambda = null,
                .value_defs = undefined,
            };
            const next = try analyzer.analyzeConst(@enumFromInt(@as(u32, @intCast(i))));
            if (!Analyzer.contractsEqual(const_states.items[i], next)) {
                const_states.items[i] = next;
                changed = true;
            }
        }
    }

    try table.const_contracts.appendSlice(allocator, const_states.items);
    return table;
}
