//! Solved-callable-driven executable specialization queue.

const std = @import("std");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

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
    fn_def: solved.Ast.FnDef,
    requested_types: solved.Type.Store,
    requested_ty: TypeVarId,
    key_bytes: []const u8,
    specialized_symbol: Symbol,
    status: Status = .pending,
    specialized: ?ast.FnDef = null,
    result_ty: ?type_mod.TypeId = null,
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
            item.requested_types.deinit();
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
            const fn_def = item.specialized orelse continue;
            try out.append(allocator, .{
                .bind = item.specialized_symbol,
                .result_ty = item.result_ty orelse debugPanic("lambdamono.specializations.solvedDefs missing result type"),
                .value = .{ .fn_ = fn_def },
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
};

/// Build a lookup table of solved function definitions from the solved program.
pub fn buildFEnv(allocator: std.mem.Allocator, input: *const solved.Lower.Result) std.mem.Allocator.Error![]FEnvEntry {
    var out = std.ArrayList(FEnvEntry).empty;
    errdefer out.deinit(allocator);

    for (input.store.defsSlice()) |def| {
        switch (def.value) {
            .fn_ => |fn_def| try out.append(allocator, .{
                .name = def.bind.symbol,
                .fn_ty = def.bind.ty,
                .fn_def = fn_def,
            }),
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

/// Look up a source function definition by its exact symbol.
pub fn lookupFnExact(fenv: []const FEnvEntry, name: Symbol) ?FEnvEntry {
    for (fenv) |entry| {
        if (entry.name == name) return entry;
    }
    return null;
}

/// Queue or reuse a specialization for a solved source function.
pub fn specializeFn(
    queue: *Queue,
    fenv: []const FEnvEntry,
    solved_types: *const solved.Type.Store,
    symbols: *symbol_mod.Store,
    requested_name: Symbol,
    repr_mode: Pending.ReprMode,
    requested_ty: TypeVarId,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFnExact(fenv, requested_name) orelse
        debugPanic("lambdamono.specializations.specializeFn missing function");
    const key = try makeKey(queue.allocator, solved_types, requested_name, repr_mode, requested_ty);
    errdefer queue.allocator.free(key);
    if (queue.by_key.get(key)) |idx| {
        queue.allocator.free(key);
        return queue.items.items[idx].specialized_symbol;
    }

    var requested_types = solved.Type.Store.init(queue.allocator);
    errdefer requested_types.deinit();
    const requested_ty_copy = try cloneTypeIntoStore(queue.allocator, solved_types, &requested_types, requested_ty);

    const source_entry = symbols.get(requested_name);
    const specialized_symbol = try symbols.add(source_entry.name, specializedOrigin(source_entry.origin, requested_name));
    try queue.items.append(queue.allocator, .{
        .name = entry.name,
        .repr_mode = repr_mode,
        .fn_ty = entry.fn_ty,
        .fn_def = entry.fn_def,
        .requested_types = requested_types,
        .requested_ty = requested_ty_copy,
        .key_bytes = key,
        .specialized_symbol = specialized_symbol,
    });
    try queue.by_key.put(key, queue.items.items.len - 1);
    return specialized_symbol;
}

fn cloneTypeIntoStore(
    allocator: std.mem.Allocator,
    source_types: *const solved.Type.Store,
    target_types: *solved.Type.Store,
    ty: TypeVarId,
) std.mem.Allocator.Error!TypeVarId {
    var mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator);
    defer mapping.deinit();
    return try cloneTypeRec(allocator, source_types, target_types, &mapping, ty);
}

fn cloneTypeRec(
    allocator: std.mem.Allocator,
    source_types: *const solved.Type.Store,
    target_types: *solved.Type.Store,
    mapping: *std.AutoHashMap(TypeVarId, TypeVarId),
    ty: TypeVarId,
) std.mem.Allocator.Error!TypeVarId {
    const id = source_types.unlinkConst(ty);
    if (mapping.get(id)) |cached| return cached;

    const cloned = switch (source_types.getNode(id)) {
        .link => unreachable,
        .for_a => try target_types.freshForA(),
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
                .func => solved.Type.Node{ .content = .{ .func = .{
                    .arg = try cloneTypeRec(allocator, source_types, target_types, mapping, content.func.arg),
                    .lset = try cloneTypeRec(allocator, source_types, target_types, mapping, content.func.lset),
                    .ret = try cloneTypeRec(allocator, source_types, target_types, mapping, content.func.ret),
                } } },
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
) std.mem.Allocator.Error![]const u8 {
    const ty_key = try solved_types.structuralKeyOwned(requested_ty);
    defer allocator.free(ty_key);

    const source_raw: u32 = @intFromEnum(requested_name);
    const prefix_len = @sizeOf(u32) + @sizeOf(u8);
    const key = try allocator.alloc(u8, prefix_len + ty_key.len);
    @memcpy(key[0..@sizeOf(u32)], std.mem.asBytes(&source_raw));
    key[@sizeOf(u32)] = @intFromEnum(repr_mode);
    @memcpy(key[prefix_len..], ty_key);
    return key;
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
