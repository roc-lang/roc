//! Solved-callable-driven executable specialization queue.

const std = @import("std");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

/// Opaque planner-owned fact handle retained by queued specializations.
pub const FactId = enum(u32) { _ };

/// Pending specialization work item keyed by source symbol and solved function type.
pub const Pending = struct {
    /// Controls whether the specialization uses the natural or erased-boundary representation.
    pub const ReprMode = enum(u8) {
        natural,
        erased_boundary,
    };

    /// Current lifecycle state for a pending specialization.
    pub const Status = enum(u8) {
        pending,
        specializing,
        done,
    };

    name: Symbol,
    repr_mode: ReprMode,
    fn_ty: TypeVarId,
    source_def: SourceDef,
    source_captures: []const solved.Type.Capture,
    requested_types: ?solved.Type.Store,
    requested_ty: ?TypeVarId,
    key_bytes: []const u8,
    specialized_symbol: Symbol,
    status: Status = .pending,
    specialized: ?ast.DefVal = null,
    summary_types: ?*solved.Type.Store = null,
    summary_fn_ty: ?TypeVarId = null,
    summary_fn_source_ty: ?TypeVarId = null,
    summary_captures: ?[]const solved.Type.Capture = null,
    summary_seeded: bool = false,
    callable: FactId,
    exec_capture_ty: ?type_mod.TypeId = null,
    capture_facts: []const FactId = &.{},
    arg_facts: []const FactId = &.{},
    exec_args_tys: ?[]type_mod.TypeId = null,
    exec_ret_ty: type_mod.TypeId,
};

/// Source definition carried by a queued callable specialization.
pub const SourceDef = union(enum) {
    fn_: solved.Ast.FnDef,
    hosted_fn: solved.Ast.HostedFnDef,
};

/// Queue of pending and completed executable specializations.
pub const Queue = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayList(Pending),
    by_key: std.StringHashMap(usize),

    /// Initialize an empty specialization queue.
    pub fn init(allocator: std.mem.Allocator) Queue {
        return .{
            .allocator = allocator,
            .items = .empty,
            .by_key = std.StringHashMap(usize).init(allocator),
        };
    }

    /// Release all specialization queue state.
    pub fn deinit(self: *Queue) void {
        for (self.items.items) |*item| {
            if (item.requested_types) |*types| types.deinit();
            if (item.summary_types) |types| {
                types.deinit();
                self.allocator.destroy(types);
            }
            if (item.source_captures.len != 0) {
                self.allocator.free(item.source_captures);
            }
            if (item.summary_captures) |captures| {
                if (captures.len != 0) self.allocator.free(captures);
            }
            if (item.exec_args_tys) |args| {
                if (args.len != 0) self.allocator.free(args);
            }
            if (item.capture_facts.len != 0) {
                self.allocator.free(item.capture_facts);
            }
            if (item.arg_facts.len != 0) {
                self.allocator.free(item.arg_facts);
            }
            self.allocator.free(item.key_bytes);
        }
        self.items.deinit(self.allocator);
        self.by_key.deinit();
    }

    /// Return the next specialization that has not started yet.
    pub fn nextPending(self: *Queue) ?*Pending {
        for (self.items.items) |*item| {
            if (item.status == .pending) return item;
        }
        return null;
    }

    /// Materialize completed specializations as executable definitions.
    pub fn solvedDefs(self: *const Queue, allocator: std.mem.Allocator) std.mem.Allocator.Error![]ast.Def {
        var out = std.ArrayList(ast.Def).empty;
        errdefer out.deinit(allocator);
        for (self.items.items) |item| {
            const specialized = item.specialized orelse continue;
            try out.append(allocator, .{
                .bind = item.specialized_symbol,
                .result_ty = switch (specialized) {
                    .fn_, .hosted_fn => item.exec_ret_ty,
                    .val, .run => debugPanic("lambdamono.specializations.solvedDefs unexpected non-callable specialization"),
                },
                .value = specialized,
            });
        }
        return try out.toOwnedSlice(allocator);
    }
};

/// Function-environment entry used to look up solved source definitions by symbol.
pub const FEnvEntry = struct {
    name: Symbol,
    fn_ty: TypeVarId,
    fn_def: solved.Ast.FnDef,
    captures: []const solved.Type.Capture,
    capture_symbols: []const Symbol,
};

/// Build a lookup table of solved function definitions from the solved program.
pub fn buildFEnv(allocator: std.mem.Allocator, input: *const solved.Lower.Result) std.mem.Allocator.Error![]FEnvEntry {
    var out = std.ArrayList(FEnvEntry).empty;
    errdefer {
        for (out.items) |entry| {
            if (entry.captures.len != 0) allocator.free(entry.captures);
            if (entry.capture_symbols.len != 0) allocator.free(entry.capture_symbols);
        }
        out.deinit(allocator);
    }

    for (input.store.defsSlice()) |def| {
        switch (def.value) {
            .fn_ => |fn_def| {
                const captures = input.types.requireLambdaCaptures(def.bind.ty, def.bind.symbol);
                const copied_captures = try allocator.dupe(solved.Type.Capture, captures);
                errdefer allocator.free(copied_captures);
                const capture_symbols = try allocator.alloc(Symbol, captures.len);
                errdefer allocator.free(capture_symbols);
                for (captures, 0..) |capture, i| {
                    capture_symbols[i] = capture.symbol;
                }
                try out.append(allocator, .{
                    .name = def.bind.symbol,
                    .fn_ty = def.bind.ty,
                    .fn_def = fn_def,
                    .captures = copied_captures,
                    .capture_symbols = capture_symbols,
                });
            },
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

/// Release a function-environment table built by `buildFEnv`.
pub fn deinitFEnv(allocator: std.mem.Allocator, fenv: []const FEnvEntry) void {
    for (fenv) |entry| {
        if (entry.captures.len != 0) allocator.free(entry.captures);
        if (entry.capture_symbols.len != 0) allocator.free(entry.capture_symbols);
    }
    if (fenv.len != 0) allocator.free(fenv);
}

/// Look up a source function definition by its callable target.
pub fn lookupFnExact(fenv: []const FEnvEntry, name: Symbol) ?FEnvEntry {
    for (fenv) |entry| {
        if (entry.name == name) return entry;
    }
    return null;
}

/// Queue or reuse a specialization for a solved source function.
pub fn specializeFnWithExecArgs(
    queue: *Queue,
    fenv: []const FEnvEntry,
    solved_types: *const solved.Type.Store,
    symbols: *symbol_mod.Store,
    requested_name: Symbol,
    repr_mode: Pending.ReprMode,
    requested_ty: TypeVarId,
    exec_types: ?*type_mod.Store,
    callable: FactId,
    exec_capture_ty: ?type_mod.TypeId,
    capture_facts: []const FactId,
    arg_facts: []const FactId,
    exec_arg_tys: []const type_mod.TypeId,
    exec_ret_ty: type_mod.TypeId,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFnExact(fenv, requested_name) orelse
        debugPanic("lambdamono.specializations.specializeFn missing function");
    const key = try makeKey(
        queue.allocator,
        solved_types,
        requested_name,
        repr_mode,
        requested_ty,
        exec_types,
        callable,
        exec_capture_ty,
        capture_facts,
        arg_facts,
        exec_arg_tys,
        exec_ret_ty,
    );
    errdefer queue.allocator.free(key);
    if (queue.by_key.get(key)) |idx| {
        queue.allocator.free(key);
        return queue.items.items[idx].specialized_symbol;
    }

    var requested_types = solved.Type.Store.init(queue.allocator);
    errdefer requested_types.deinit();
    var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(queue.allocator);
    defer mapping.deinit();
    const requested_ty_copy = try cloneTypeRec(
        queue.allocator,
        solved_types,
        &requested_types,
        &mapping,
        requested_ty,
    );
    const source_entry = symbols.get(requested_name);
    const specialized_symbol = try symbols.add(source_entry.name, specializedOrigin(source_entry.origin, requested_name));
    try queue.items.append(queue.allocator, .{
        .name = entry.name,
        .repr_mode = repr_mode,
        .fn_ty = entry.fn_ty,
        .source_def = .{ .fn_ = entry.fn_def },
        .source_captures = try queue.allocator.dupe(solved.Type.Capture, entry.captures),
        .requested_types = requested_types,
        .requested_ty = requested_ty_copy,
        .key_bytes = key,
        .specialized_symbol = specialized_symbol,
        .callable = callable,
        .exec_capture_ty = exec_capture_ty,
        .capture_facts = try queue.allocator.dupe(FactId, capture_facts),
        .arg_facts = try queue.allocator.dupe(FactId, arg_facts),
        .exec_args_tys = try queue.allocator.dupe(type_mod.TypeId, exec_arg_tys),
        .exec_ret_ty = exec_ret_ty,
    });
    try queue.by_key.put(key, queue.items.items.len - 1);
    return specialized_symbol;
}

/// Queue or reuse a specialization for a hosted source function.
pub fn specializeHostedWithExecArgs(
    queue: *Queue,
    solved_types: *const solved.Type.Store,
    symbols: *symbol_mod.Store,
    requested_name: Symbol,
    repr_mode: Pending.ReprMode,
    requested_ty: TypeVarId,
    hosted_fn: solved.Ast.HostedFnDef,
    exec_types: ?*type_mod.Store,
    callable: FactId,
    exec_capture_ty: ?type_mod.TypeId,
    capture_facts: []const FactId,
    arg_facts: []const FactId,
    exec_arg_tys: []const type_mod.TypeId,
    exec_ret_ty: type_mod.TypeId,
) std.mem.Allocator.Error!Symbol {
    const key = try makeKey(
        queue.allocator,
        solved_types,
        requested_name,
        repr_mode,
        requested_ty,
        exec_types,
        callable,
        exec_capture_ty,
        capture_facts,
        arg_facts,
        exec_arg_tys,
        exec_ret_ty,
    );
    errdefer queue.allocator.free(key);
    if (queue.by_key.get(key)) |idx| {
        queue.allocator.free(key);
        return queue.items.items[idx].specialized_symbol;
    }

    var requested_types = solved.Type.Store.init(queue.allocator);
    errdefer requested_types.deinit();
    var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(queue.allocator);
    defer mapping.deinit();
    const requested_ty_copy = try cloneTypeRec(
        queue.allocator,
        solved_types,
        &requested_types,
        &mapping,
        requested_ty,
    );
    const source_entry = symbols.get(requested_name);
    const specialized_symbol = try symbols.add(source_entry.name, specializedOrigin(source_entry.origin, requested_name));
    try queue.items.append(queue.allocator, .{
        .name = requested_name,
        .repr_mode = repr_mode,
        .fn_ty = hosted_fn.bind.ty,
        .source_def = .{ .hosted_fn = hosted_fn },
        .source_captures = &.{},
        .requested_types = requested_types,
        .requested_ty = requested_ty_copy,
        .key_bytes = key,
        .specialized_symbol = specialized_symbol,
        .callable = callable,
        .exec_capture_ty = exec_capture_ty,
        .capture_facts = try queue.allocator.dupe(FactId, capture_facts),
        .arg_facts = try queue.allocator.dupe(FactId, arg_facts),
        .exec_args_tys = try queue.allocator.dupe(type_mod.TypeId, exec_arg_tys),
        .exec_ret_ty = exec_ret_ty,
    });
    try queue.by_key.put(key, queue.items.items.len - 1);
    return specialized_symbol;
}

fn cloneTypeRec(
    allocator: std.mem.Allocator,
    source_types: *const solved.Type.Store,
    target_types: *solved.Type.Store,
    mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ty: TypeVarId,
) std.mem.Allocator.Error!TypeVarId {
    const id = source_types.unlinkPreservingNominalConst(ty);
    if (mapping.get(id)) |cached| return cached;

    const cloned = switch (source_types.getNode(id)) {
        .link => unreachable,
        .for_a, .flex_for_a => try target_types.freshFlexForA(),
        .unbd => try target_types.freshUnbd(),
        .nominal => |nominal| blk: {
            const placeholder = try target_types.freshUnbd();
            try mapping.put(id, placeholder);
            const args = source_types.sliceTypeVarSpan(nominal.args);
            const cloned_args = try allocator.alloc(TypeVarId, args.len);
            defer allocator.free(cloned_args);
            for (args, 0..) |arg, i| {
                cloned_args[i] = try cloneTypeRec(allocator, source_types, target_types, mapping, arg);
            }
            target_types.setNode(placeholder, .{ .nominal = .{
                .module_idx = nominal.module_idx,
                .ident = nominal.ident,
                .is_opaque = nominal.is_opaque,
                .args = try target_types.addTypeVarSpan(cloned_args),
                .backing = try cloneTypeRec(allocator, source_types, target_types, mapping, nominal.backing),
            } });
            break :blk placeholder;
        },
        .content => |content| blk: {
            const placeholder = try target_types.freshUnbd();
            try mapping.put(id, placeholder);
            const node = switch (content) {
                .primitive => solved.Type.Node{ .content = .{ .primitive = content.primitive } },
                .func => |func| blk2: {
                    const args = source_types.sliceTypeVarSpan(func.args);
                    const out_args = try allocator.alloc(TypeVarId, args.len);
                    defer allocator.free(out_args);
                    for (args, 0..) |arg, i| {
                        out_args[i] = try cloneTypeRec(allocator, source_types, target_types, mapping, arg);
                    }
                    break :blk2 solved.Type.Node{ .content = .{ .func = .{
                        .args = try target_types.addTypeVarSpan(out_args),
                        .lset = try cloneTypeRec(allocator, source_types, target_types, mapping, func.lset),
                        .ret = try cloneTypeRec(allocator, source_types, target_types, mapping, func.ret),
                    } } };
                },
                .list => |elem| solved.Type.Node{ .content = .{
                    .list = try cloneTypeRec(allocator, source_types, target_types, mapping, elem),
                } },
                .box => |elem| solved.Type.Node{ .content = .{
                    .box = try cloneTypeRec(allocator, source_types, target_types, mapping, elem),
                } },
                .tuple => |tuple| blk2: {
                    const elems = source_types.sliceTypeVarSpan(tuple);
                    const out = try allocator.alloc(TypeVarId, elems.len);
                    defer allocator.free(out);
                    for (elems, 0..) |elem, i| {
                        out[i] = try cloneTypeRec(allocator, source_types, target_types, mapping, elem);
                    }
                    break :blk2 solved.Type.Node{ .content = .{
                        .tuple = try target_types.addTypeVarSpan(out),
                    } };
                },
                .record => |record| blk2: {
                    const fields = source_types.sliceFields(record.fields);
                    const out = try allocator.alloc(solved.Type.Field, fields.len);
                    defer allocator.free(out);
                    for (fields, 0..) |field, i| {
                        out[i] = .{
                            .name = field.name,
                            .ty = try cloneTypeRec(allocator, source_types, target_types, mapping, field.ty),
                        };
                    }
                    break :blk2 solved.Type.Node{ .content = .{
                        .record = .{ .fields = try target_types.addFields(out) },
                    } };
                },
                .tag_union => |tag_union| blk2: {
                    const tags = source_types.sliceTags(tag_union.tags);
                    const out = try allocator.alloc(solved.Type.Tag, tags.len);
                    defer allocator.free(out);
                    for (tags, 0..) |tag, i| {
                        const args = source_types.sliceTypeVarSpan(tag.args);
                        const out_args = try allocator.alloc(TypeVarId, args.len);
                        defer allocator.free(out_args);
                        for (args, 0..) |arg, arg_i| {
                            out_args[arg_i] = try cloneTypeRec(allocator, source_types, target_types, mapping, arg);
                        }
                        out[i] = .{
                            .name = tag.name,
                            .args = try target_types.addTypeVarSpan(out_args),
                        };
                    }
                    break :blk2 solved.Type.Node{ .content = .{
                        .tag_union = .{ .tags = try target_types.addTags(out) },
                    } };
                },
                .lambda_set => |lambda_set| blk2: {
                    const lambdas = source_types.sliceLambdas(lambda_set);
                    const out = try allocator.alloc(solved.Type.Lambda, lambdas.len);
                    defer allocator.free(out);
                    for (lambdas, 0..) |lambda, i| {
                        const captures = source_types.sliceCaptures(lambda.captures);
                        const out_captures = try allocator.alloc(solved.Type.Capture, captures.len);
                        defer allocator.free(out_captures);
                        for (captures, 0..) |capture, capture_i| {
                            out_captures[capture_i] = .{
                                .symbol = capture.symbol,
                                .ty = try cloneTypeRec(allocator, source_types, target_types, mapping, capture.ty),
                            };
                        }
                        out[i] = .{
                            .symbol = lambda.symbol,
                            .captures = try target_types.addCaptures(out_captures),
                        };
                    }
                    break :blk2 solved.Type.Node{ .content = .{
                        .lambda_set = try target_types.addLambdas(out),
                    } };
                },
            };
            target_types.setNode(placeholder, node);
            break :blk placeholder;
        },
    };

    try mapping.put(id, cloned);
    return cloned;
}

fn makeKey(
    allocator: std.mem.Allocator,
    solved_types: *const solved.Type.Store,
    requested_name: Symbol,
    repr_mode: Pending.ReprMode,
    requested_ty: TypeVarId,
    exec_types: ?*type_mod.Store,
    callable: FactId,
    exec_capture_ty: ?type_mod.TypeId,
    capture_facts: []const FactId,
    arg_facts: []const FactId,
    exec_arg_tys: []const type_mod.TypeId,
    exec_ret_ty: type_mod.TypeId,
) std.mem.Allocator.Error![]const u8 {
    const ty_key = try solved_types.structuralKeyOwned(requested_ty);
    defer allocator.free(ty_key);

    var key = std.ArrayList(u8).empty;
    errdefer key.deinit(allocator);

    const source_raw: u32 = @intFromEnum(requested_name);
    try key.appendSlice(allocator, std.mem.asBytes(&source_raw));
    try key.append(allocator, @intFromEnum(repr_mode));

    const ty_len: u32 = @intCast(ty_key.len);
    try key.appendSlice(allocator, std.mem.asBytes(&ty_len));
    try key.appendSlice(allocator, ty_key);

    try key.appendSlice(allocator, std.mem.asBytes(&callable));

    const has_exec_capture: u8 = if (exec_capture_ty != null) 1 else 0;
    try key.append(allocator, has_exec_capture);
    if (exec_capture_ty) |capture_ty| {
        const exec_store = exec_types orelse
            debugPanic("lambdamono.specializations.makeKey missing executable type store for executable capture key");
        const capture_key = try exec_store.structuralKeyOwned(capture_ty);
        defer exec_store.allocator.free(capture_key);
        const capture_key_len: u32 = @intCast(capture_key.len);
        try key.appendSlice(allocator, std.mem.asBytes(&capture_key_len));
        try key.appendSlice(allocator, capture_key);
    }

    const capture_fact_count: u32 = @intCast(capture_facts.len);
    try key.appendSlice(allocator, std.mem.asBytes(&capture_fact_count));
    for (capture_facts) |fact| {
        try key.appendSlice(allocator, std.mem.asBytes(&fact));
    }

    const arg_fact_count: u32 = @intCast(arg_facts.len);
    try key.appendSlice(allocator, std.mem.asBytes(&arg_fact_count));
    for (arg_facts) |fact| {
        try key.appendSlice(allocator, std.mem.asBytes(&fact));
    }

    const exec_arg_count: u32 = @intCast(exec_arg_tys.len);
    try key.appendSlice(allocator, std.mem.asBytes(&exec_arg_count));
    if (exec_arg_tys.len != 0) {
        const exec_store = exec_types orelse
            debugPanic("lambdamono.specializations.makeKey missing executable type store for executable signature key");
        for (exec_arg_tys) |exec_arg_ty| {
            const exec_key = try exec_store.structuralKeyOwned(exec_arg_ty);
            defer exec_store.allocator.free(exec_key);
            const exec_key_len: u32 = @intCast(exec_key.len);
            try key.appendSlice(allocator, std.mem.asBytes(&exec_key_len));
            try key.appendSlice(allocator, exec_key);
        }
    }

    const exec_store = exec_types orelse
        debugPanic("lambdamono.specializations.makeKey missing executable type store for executable return key");
    const ret_key = try exec_store.structuralKeyOwned(exec_ret_ty);
    defer exec_store.allocator.free(ret_key);
    const ret_key_len: u32 = @intCast(ret_key.len);
    try key.appendSlice(allocator, std.mem.asBytes(&ret_key_len));
    try key.appendSlice(allocator, ret_key);

    return try key.toOwnedSlice(allocator);
}

fn specializedOrigin(origin: symbol_mod.BindingOrigin, source_symbol: Symbol) symbol_mod.BindingOrigin {
    return switch (origin) {
        .lifted_local_fn, .lifted_local_fn_alias, .specialized_local_fn => .{
            .specialized_local_fn = .{ .source_symbol = source_symbol.raw() },
        },
        else => .{
            .specialized_top_level_def = .{ .source_symbol = source_symbol.raw() },
        },
    };
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
