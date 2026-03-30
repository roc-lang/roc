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

const CallableResolution = struct {
    lambda: MIR.LambdaId,
    requires_hidden_capture: bool,
};

const JoinCallableState = struct {
    params: []const MIR.LocalId,
    merged_callables: []?CallableResolution,
    saw_incoming: bool,
};

const ActiveJoinCallableMap = std.AutoHashMap(u32, JoinCallableState);

fn callableResolutionsEqual(left: CallableResolution, right: CallableResolution) bool {
    return left.lambda == right.lambda and left.requires_hidden_capture == right.requires_hidden_capture;
}

const Analyzer = struct {
    allocator: Allocator,
    mir_store: *const MIR.Store,
    table: *Table,
    lambda_states: ?[]const Contract,
    current_lambda: ?MIR.LambdaId,
    active_callable_joins: *ActiveJoinCallableMap,
    callable_overrides: *const std.AutoHashMap(u32, CallableResolution),

    fn lambdaContract(self: *const Analyzer, lambda_id: MIR.LambdaId) Contract {
        if (self.lambda_states) |states| return states[@intFromEnum(lambda_id)];
        return self.table.getLambdaContract(lambda_id);
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
            "CallableSummary invariant violated: param-projection callable resolution escaped lambda analysis",
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

    fn resolveCallableFromStructValue(
        self: *Analyzer,
        fields: MIR.LocalSpan,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (reversed_path.items.len == 0) {
            std.debug.panic(
                "CallableSummary invariant violated: callable resolution reached a non-callable struct value",
                .{},
            );
        }

        const next_projection = reversed_path.items[reversed_path.items.len - 1];
        const field_idx = switch (next_projection) {
            .field => |idx| idx,
            else => std.debug.panic(
                "CallableSummary invariant violated: callable struct resolution expected a field projection, found {s}",
                .{@tagName(next_projection)},
            ),
        };

        const field_locals = self.mir_store.getLocalSpan(fields);
        if (field_idx >= field_locals.len) {
            std.debug.panic(
                "CallableSummary invariant violated: callable field index {d} exceeds struct arity {d}",
                .{ field_idx, field_locals.len },
            );
        }

        reversed_path.items.len -= 1;
        defer reversed_path.items.len += 1;
        return self.resolveCallableForLocalPath(field_locals[field_idx], reversed_path);
    }

    fn resolveCallableFromTagValue(
        self: *Analyzer,
        args: MIR.LocalSpan,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (reversed_path.items.len == 0) {
            std.debug.panic(
                "CallableSummary invariant violated: callable resolution reached a non-callable tag value",
                .{},
            );
        }

        const next_projection = reversed_path.items[reversed_path.items.len - 1];
        const payload_idx = switch (next_projection) {
            .tag_payload => |idx| idx,
            else => std.debug.panic(
                "CallableSummary invariant violated: callable tag resolution expected a payload projection, found {s}",
                .{@tagName(next_projection)},
            ),
        };

        const payload_locals = self.mir_store.getLocalSpan(args);
        if (payload_idx >= payload_locals.len) {
            std.debug.panic(
                "CallableSummary invariant violated: callable payload index {d} exceeds tag arity {d}",
                .{ payload_idx, payload_locals.len },
            );
        }

        reversed_path.items.len -= 1;
        defer reversed_path.items.len += 1;
        return self.resolveCallableForLocalPath(payload_locals[payload_idx], reversed_path);
    }

    fn resolveCallableFromJoinParam(
        self: *Analyzer,
        join_param: MIR.LocalDef.join_param,
        reversed_path: []const MIR.CallableProjection,
    ) CallableResolution {
        _ = join_param;
        if (reversed_path.len != 0) {
            std.debug.panic(
                "CallableSummary TODO: exact callable resolution through projected join-param values is not implemented yet",
                .{},
            );
        }
        std.debug.panic(
            "CallableSummary invariant violated: join-param callable resolution escaped active join overrides",
            .{},
        );
    }

    fn resolveCallableForLocalPath(
        self: *Analyzer,
        local_id: MIR.LocalId,
        reversed_path: *std.ArrayList(MIR.CallableProjection),
    ) Allocator.Error!CallableResolution {
        if (self.callable_overrides.get(localKey(local_id))) |resolved| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "CallableSummary TODO: exact callable resolution through projected join-param values is not implemented yet",
                    .{},
                );
            }
            return resolved;
        }

        return switch (self.mir_store.getLocalDef(local_id)) {
            .param, .captures_param => self.resolveCallableForParamProjection(local_id, reversed_path.items),
            .join_param => |join_param| self.resolveCallableFromJoinParam(join_param, reversed_path.items),
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
            .ref => |ref_op| switch (ref_op) {
                .local => |source| self.resolveCallableForLocalPath(source, reversed_path),
                .nominal => |nominal| blk: {
                    try reversed_path.append(self.allocator, .nominal);
                    defer _ = reversed_path.pop();
                    break :blk try self.resolveCallableForLocalPath(nominal.backing, reversed_path);
                },
                .field => |field| blk: {
                    switch (self.mir_store.getLocalDef(field.source)) {
                        .struct_ => |fields| {
                            const field_locals = self.mir_store.getLocalSpan(fields);
                            if (field.field_idx >= field_locals.len) {
                                std.debug.panic(
                                    "CallableSummary invariant violated: callable field index {d} exceeds struct arity {d}",
                                    .{ field.field_idx, field_locals.len },
                                );
                            }
                            break :blk try self.resolveCallableForLocalPath(field_locals[field.field_idx], reversed_path);
                        },
                        else => {},
                    }

                    try reversed_path.append(self.allocator, .{ .field = field.field_idx });
                    defer _ = reversed_path.pop();
                    break :blk try self.resolveCallableForLocalPath(field.source, reversed_path);
                },
                .tag_payload => |payload| blk: {
                    switch (self.mir_store.getLocalDef(payload.source)) {
                        .tag => |tag_value| {
                            const payload_locals = self.mir_store.getLocalSpan(tag_value.args);
                            if (payload.payload_idx >= payload_locals.len) {
                                std.debug.panic(
                                    "CallableSummary invariant violated: callable payload index {d} exceeds tag arity {d}",
                                    .{ payload.payload_idx, payload_locals.len },
                                );
                            }
                            break :blk try self.resolveCallableForLocalPath(payload_locals[payload.payload_idx], reversed_path);
                        },
                        else => {},
                    }

                    try reversed_path.append(self.allocator, .{ .tag_payload = payload.payload_idx });
                    defer _ = reversed_path.pop();
                    break :blk try self.resolveCallableForLocalPath(payload.source, reversed_path);
                },
                .discriminant => std.debug.panic(
                    "CallableSummary invariant violated: callable resolution reached a discriminant ref local {d}",
                    .{@intFromEnum(local_id)},
                ),
            },
            .symbol => |symbol| std.debug.panic(
                "CallableSummary invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
                .{symbol.raw()},
            ),
            .call => |call_result| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "CallableSummary TODO: exact callable resolution through projected call results is not implemented yet",
                        .{},
                    );
                }

                const callee = if (call_result.exact_lambda) |lambda_id|
                    CallableResolution{
                        .lambda = lambda_id,
                        .requires_hidden_capture = call_result.exact_requires_hidden_capture,
                    }
                else
                    try self.resolveCallableForLocalPath(
                        call_result.callee,
                        reversed_path,
                    );
                return switch (self.lambdaContract(callee.lambda)) {
                    .no_return => std.debug.panic(
                        "CallableSummary invariant violated: call-result callable resolution reached no-return lambda {d}",
                        .{@intFromEnum(callee.lambda)},
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
            .struct_ => |fields| self.resolveCallableFromStructValue(fields, reversed_path),
            .tag => |tag_value| self.resolveCallableFromTagValue(tag_value.args, reversed_path),
            .literal,
            .low_level,
            .list,
            => std.debug.panic(
                "CallableSummary invariant violated: callable resolution reached non-callable local def {s} for local {d}",
                .{ @tagName(self.mir_store.getLocalDef(local_id)), @intFromEnum(local_id) },
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
            .runtime_error, .scope_exit, .crash => {},
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
                const join_key = @intFromEnum(join_stmt.id);
                const join_params = self.mir_store.getLocalSpan(join_stmt.params);
                const merged_callables = try self.allocator.alloc(?CallableResolution, join_params.len);
                defer self.allocator.free(merged_callables);
                @memset(merged_callables, null);

                const gop = try self.active_callable_joins.getOrPut(join_key);
                if (gop.found_existing) {
                    std.debug.panic(
                        "CallableSummary invariant violated: nested/duplicate active join {d}",
                        .{join_key},
                    );
                }
                gop.value_ptr.* = .{
                    .params = join_params,
                    .merged_callables = merged_callables,
                    .saw_incoming = false,
                };
                defer _ = self.active_callable_joins.remove(join_key);

                try self.collectReturnedCallable(join_stmt.remainder, out);
                if (!gop.value_ptr.saw_incoming) return;

                var body_callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
                defer body_callable_overrides.deinit();
                try body_callable_overrides.ensureTotalCapacity(@intCast(join_params.len));

                for (join_params, merged_callables, 0..) |param, incoming_callable, i| {
                    if (!self.localMayContainCallable(param)) continue;
                    body_callable_overrides.putAssumeCapacity(
                        localKey(param),
                        incoming_callable orelse std.debug.panic(
                            "CallableSummary invariant violated: join {d} param {d} had no incoming exact callable",
                            .{ join_key, i },
                        ),
                    );
                }

                const saved_callable_overrides = self.callable_overrides;
                self.callable_overrides = &body_callable_overrides;
                defer self.callable_overrides = saved_callable_overrides;

                try self.collectReturnedCallable(join_stmt.body, out);
            },
            .jump => |jump| {
                const join_state = self.active_callable_joins.getPtr(@intFromEnum(jump.id)) orelse std.debug.panic(
                    "CallableSummary invariant violated: jump to unknown active join {d}",
                    .{@intFromEnum(jump.id)},
                );
                const args = self.mir_store.getLocalSpan(jump.args);
                if (args.len != join_state.params.len) {
                    std.debug.panic(
                        "CallableSummary invariant violated: jump to join {d} passed {d} args, expected {d}",
                        .{ @intFromEnum(jump.id), args.len, join_state.params.len },
                    );
                }

                join_state.saw_incoming = true;
                for (args, 0..) |arg, i| {
                    const param = join_state.params[i];
                    if (!self.localMayContainCallable(param)) continue;

                    var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                    defer reversed_path.deinit(self.allocator);
                    const incoming_callable = try self.resolveCallableForLocalPath(arg, &reversed_path);
                    if (join_state.merged_callables[i]) |current| {
                        if (!callableResolutionsEqual(current, incoming_callable)) {
                            std.debug.panic(
                                "CallableSummary TODO: merging incompatible exact callable identities across join {d} param {d} is not implemented yet",
                                .{ @intFromEnum(jump.id), i },
                            );
                        }
                    } else {
                        join_state.merged_callables[i] = incoming_callable;
                    }
                }
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
        var callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
        defer callable_overrides.deinit();
        var active_callable_joins = ActiveJoinCallableMap.init(self.allocator);
        defer {
            var it = active_callable_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_callables);
            }
            active_callable_joins.deinit();
        }

        self.current_lambda = lambda_id;
        self.callable_overrides = &callable_overrides;
        self.active_callable_joins = &active_callable_joins;

        var returned: ?Contract = null;
        try self.collectReturnedCallable(lambda.body, &returned);
        return returned orelse .no_return;
    }

    fn analyzeConst(self: *Analyzer, const_id: MIR.ConstDefId) Allocator.Error!Contract {
        const def = self.mir_store.getConstDef(const_id);
        var callable_overrides = std.AutoHashMap(u32, CallableResolution).init(self.allocator);
        defer callable_overrides.deinit();
        var active_callable_joins = ActiveJoinCallableMap.init(self.allocator);
        defer {
            var it = active_callable_joins.valueIterator();
            while (it.next()) |value| {
                self.allocator.free(value.merged_callables);
            }
            active_callable_joins.deinit();
        }

        self.current_lambda = null;
        self.callable_overrides = &callable_overrides;
        self.active_callable_joins = &active_callable_joins;

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

    var reachable_lambdas = std.AutoHashMap(u32, void).init(allocator);
    defer reachable_lambdas.deinit();
    var reachable_consts = std.AutoHashMap(u32, void).init(allocator);
    defer reachable_consts.deinit();
    var visited_reachable_stmts = std.AutoHashMap(u32, void).init(allocator);
    defer visited_reachable_stmts.deinit();
    for (requested_root_consts) |const_id| {
        try collectReachableConst(allocator, mir_store, const_id, &reachable_lambdas, &reachable_consts, &visited_reachable_stmts);
    }

    var lambda_states = std.ArrayList(Contract).empty;
    defer lambda_states.deinit(allocator);
    try lambda_states.appendNTimes(allocator, .no_return, mir_store.getLambdas().len);

    var changed = true;
    while (changed) {
        changed = false;
        for (mir_store.getLambdas(), 0..) |_, i| {
            if (!reachable_lambdas.contains(@intCast(i))) continue;
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .lambda_states = lambda_states.items,
                .current_lambda = null,
                .active_callable_joins = undefined,
                .callable_overrides = undefined,
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
            if (!reachable_consts.contains(@intCast(i))) continue;
            var analyzer = Analyzer{
                .allocator = allocator,
                .mir_store = mir_store,
                .table = &table,
                .lambda_states = table.lambda_contracts.items,
                .current_lambda = null,
                .active_callable_joins = undefined,
                .callable_overrides = undefined,
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

fn callableProjectionPathMatches(
    mir_store: *const MIR.Store,
    binding_path: MIR.CallableProjectionSpan,
    reversed_path: []const MIR.CallableProjection,
) bool {
    const binding_projections = mir_store.getCallableProjectionSpan(binding_path);
    if (binding_projections.len != reversed_path.len) return false;

    for (binding_projections, 0..) |binding_proj, i| {
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

fn resolveReachableCalleeLambda(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    current_lambda: ?MIR.LambdaId,
    local_id: MIR.LocalId,
    reversed_path: *std.ArrayList(MIR.CallableProjection),
) Allocator.Error!?MIR.LambdaId {
    return switch (mir_store.getLocalDef(local_id)) {
        .param, .captures_param => blk: {
            if (current_lambda) |lambda_id| {
                const lambda = mir_store.getLambda(lambda_id);
                for (mir_store.getCallableBindings(lambda.callable_bindings)) |binding| {
                    if (binding.source_param != local_id) continue;
                    if (!callableProjectionPathMatches(mir_store, binding.projections, reversed_path.items)) continue;
                    break :blk binding.lambda;
                }
            }
            break :blk null;
        },
        .join_param => std.debug.panic(
            "CallableSummary invariant violated: reachable callable resolution must not inspect join params outside join analysis",
            .{},
        ),
        .lambda => |lambda_id| if (reversed_path.items.len == 0)
            lambda_id
        else
            std.debug.panic(
                "CallableSummary invariant violated: direct lambda local {d} was used with callable projections",
                .{@intFromEnum(local_id)},
            ),
        .closure => |lambda_id| if (reversed_path.items.len == 0)
            lambda_id
        else
            std.debug.panic(
                "CallableSummary invariant violated: closure local {d} was used with callable projections",
                .{@intFromEnum(local_id)},
            ),
        .ref => |ref_op| switch (ref_op) {
            .local => |source| try resolveReachableCalleeLambda(allocator, mir_store, current_lambda, source, reversed_path),
            .nominal => |nominal| blk: {
                try reversed_path.append(allocator, .nominal);
                defer _ = reversed_path.pop();
                break :blk try resolveReachableCalleeLambda(allocator, mir_store, current_lambda, nominal.backing, reversed_path);
            },
            .field => |field| blk: {
                switch (mir_store.getLocalDef(field.source)) {
                    .struct_ => |fields| {
                        const field_locals = mir_store.getLocalSpan(fields);
                        if (field.field_idx >= field_locals.len) {
                            std.debug.panic(
                                "CallableSummary invariant violated: reachable callable field index {d} exceeds struct arity {d}",
                                .{ field.field_idx, field_locals.len },
                            );
                        }
                        break :blk try resolveReachableCalleeLambda(
                            allocator,
                            mir_store,
                            current_lambda,
                            field_locals[field.field_idx],
                            reversed_path,
                        );
                    },
                    else => {},
                }

                try reversed_path.append(allocator, .{ .field = field.field_idx });
                defer _ = reversed_path.pop();
                break :blk try resolveReachableCalleeLambda(allocator, mir_store, current_lambda, field.source, reversed_path);
            },
            .tag_payload => |payload| blk: {
                switch (mir_store.getLocalDef(payload.source)) {
                    .tag => |tag_value| {
                        const payloads = mir_store.getLocalSpan(tag_value.args);
                        if (payload.payload_idx >= payloads.len) {
                            std.debug.panic(
                                "CallableSummary invariant violated: reachable callable payload index {d} exceeds tag arity {d}",
                                .{ payload.payload_idx, payloads.len },
                            );
                        }
                        break :blk try resolveReachableCalleeLambda(
                            allocator,
                            mir_store,
                            current_lambda,
                            payloads[payload.payload_idx],
                            reversed_path,
                        );
                    },
                    else => {},
                }

                try reversed_path.append(allocator, .{ .tag_payload = payload.payload_idx });
                defer _ = reversed_path.pop();
                break :blk try resolveReachableCalleeLambda(allocator, mir_store, current_lambda, payload.source, reversed_path);
            },
            .discriminant => std.debug.panic(
                "CallableSummary invariant violated: reachable callable resolution reached a discriminant ref local {d}",
                .{@intFromEnum(local_id)},
            ),
        },
        .struct_ => |fields| {
                if (reversed_path.items.len == 0) {
                    std.debug.panic(
                        "CallableSummary invariant violated: reachable callable resolution reached a non-callable struct value",
                        .{},
                    );
                }
                const projection = reversed_path.pop() orelse unreachable;
                defer reversed_path.append(allocator, projection) catch unreachable;
                const field_idx = switch (projection) {
                    .field => |idx| idx,
                    else => std.debug.panic(
                        "CallableSummary invariant violated: reachable struct callable resolution expected a field projection",
                        .{},
                    ),
                };
                const field_locals = mir_store.getLocalSpan(fields);
                if (field_idx >= field_locals.len) {
                    std.debug.panic(
                        "CallableSummary invariant violated: reachable callable field index {d} exceeds struct arity {d}",
                        .{ field_idx, field_locals.len },
                    );
                }
                return try resolveReachableCalleeLambda(
                    allocator,
                    mir_store,
                    current_lambda,
                    field_locals[field_idx],
                    reversed_path,
                );
            },
        .tag => |tag_value| {
                const args = tag_value.args;
                if (reversed_path.items.len == 0) {
                    std.debug.panic(
                        "CallableSummary invariant violated: reachable callable resolution reached a non-callable tag value",
                        .{},
                    );
                }
                const projection = reversed_path.pop() orelse unreachable;
                defer reversed_path.append(allocator, projection) catch unreachable;
                const payload_idx = switch (projection) {
                    .tag_payload => |idx| idx,
                    else => std.debug.panic(
                        "CallableSummary invariant violated: reachable tag callable resolution expected a payload projection",
                        .{},
                    ),
                };
                const payload_locals = mir_store.getLocalSpan(args);
                if (payload_idx >= payload_locals.len) {
                    std.debug.panic(
                        "CallableSummary invariant violated: reachable callable payload index {d} exceeds tag arity {d}",
                        .{ payload_idx, payload_locals.len },
                    );
                }
                return try resolveReachableCalleeLambda(
                    allocator,
                    mir_store,
                    current_lambda,
                    payload_locals[payload_idx],
                    reversed_path,
                );
            },
        .call => |call_result| {
                if (reversed_path.items.len != 0) {
                    std.debug.panic(
                        "CallableSummary TODO: exact callable reachability through projected call results is not implemented yet",
                        .{},
                    );
                }
                if (call_result.exact_lambda) |lambda_id| return lambda_id;
                return try resolveReachableCalleeLambda(
                    allocator,
                    mir_store,
                    current_lambda,
                    call_result.callee,
                    reversed_path,
                );
            },
        .symbol => |symbol| std.debug.panic(
            "CallableSummary invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
            .{symbol.raw()},
        ),
        .literal,
        .low_level,
        .list,
        => std.debug.panic(
            "CallableSummary invariant violated: reachable callable resolution reached non-callable local def {s} for local {d}",
            .{ @tagName(mir_store.getLocalDef(local_id)), @intFromEnum(local_id) },
        ),
    };
}

fn collectReachableLambda(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lambda_id: MIR.LambdaId,
    reachable_lambdas: *std.AutoHashMap(u32, void),
    reachable_consts: *std.AutoHashMap(u32, void),
    visited_stmts: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try reachable_lambdas.getOrPut(@intFromEnum(lambda_id));
    if (gop.found_existing) return;
    for (mir_store.getCallableBindings(mir_store.getLambda(lambda_id).callable_bindings)) |binding| {
        try collectReachableLambda(
            allocator,
            mir_store,
            binding.lambda,
            reachable_lambdas,
            reachable_consts,
            visited_stmts,
        );
    }
    try collectReachableFromStmt(allocator, mir_store, lambda_id, mir_store.getLambda(lambda_id).body, reachable_lambdas, reachable_consts, visited_stmts);
}

fn collectReachableConst(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    const_id: MIR.ConstDefId,
    reachable_lambdas: *std.AutoHashMap(u32, void),
    reachable_consts: *std.AutoHashMap(u32, void),
    visited_stmts: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try reachable_consts.getOrPut(@intFromEnum(const_id));
    if (gop.found_existing) return;
    try collectReachableFromStmt(allocator, mir_store, null, mir_store.getConstDef(const_id).body, reachable_lambdas, reachable_consts, visited_stmts);
}

fn collectReachableFromStmt(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    current_lambda: ?MIR.LambdaId,
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
                try collectReachableConst(allocator, mir_store, const_id, reachable_lambdas, reachable_consts, visited_stmts);
            }
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_ref => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_literal => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_lambda => |stmt| {
            try collectReachableLambda(allocator, mir_store, stmt.lambda, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_closure => |stmt| {
            try collectReachableLambda(allocator, mir_store, stmt.lambda, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_call => |stmt| {
            if (stmt.exact_lambda) |lambda_id| {
                try collectReachableLambda(allocator, mir_store, lambda_id, reachable_lambdas, reachable_consts, visited_stmts);
            } else {
                var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                defer reversed_path.deinit(allocator);
                if (try resolveReachableCalleeLambda(
                    allocator,
                    mir_store,
                    current_lambda,
                    stmt.callee,
                    &reversed_path,
                )) |lambda_id| {
                    try collectReachableLambda(allocator, mir_store, lambda_id, reachable_lambdas, reachable_consts, visited_stmts);
                }
            }
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .assign_low_level => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_list => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_struct => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .assign_tag => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .debug => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .expect => |stmt| try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.next, reachable_lambdas, reachable_consts, visited_stmts),
        .runtime_error, .scope_exit, .jump, .ret, .crash => {},
        .switch_stmt => |stmt| {
            for (mir_store.getSwitchBranches(stmt.branches)) |branch| {
                try collectReachableFromStmt(allocator, mir_store, current_lambda, branch.body, reachable_lambdas, reachable_consts, visited_stmts);
            }
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.default_branch, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .borrow_scope => |stmt| {
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.body, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.remainder, reachable_lambdas, reachable_consts, visited_stmts);
        },
        .join => |stmt| {
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.body, reachable_lambdas, reachable_consts, visited_stmts);
            try collectReachableFromStmt(allocator, mir_store, current_lambda, stmt.remainder, reachable_lambdas, reachable_consts, visited_stmts);
        },
    }
}
