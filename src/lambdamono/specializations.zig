//! Cor-style executable specialization queue.

const std = @import("std");
const solved = @import("../lambdasolved/mod.zig");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const lower_type = @import("lower_type.zig");
const symbol_mod = @import("../symbol/mod.zig");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

pub const CaptureSpec = union(enum) {
    toplevel,
    lset: type_mod.TypeId,
    erased: type_mod.TypeId,
};

pub const Pending = struct {
    name: Symbol,
    fn_ty: TypeVarId,
    fn_def: solved.Ast.FnDef,
    requested_ty: TypeVarId,
    specialized_symbol: Symbol,
    captures_new: ?[]lower_type.CaptureBinding = null,
    specialized: ?ast.FnDef = null,
};

pub const Queue = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayList(Pending),

    pub fn init(allocator: std.mem.Allocator) Queue {
        return .{
            .allocator = allocator,
            .items = .empty,
        };
    }

    pub fn deinit(self: *Queue) void {
        for (self.items.items) |item| {
            if (item.captures_new) |captures_new| self.allocator.free(captures_new);
        }
        self.items.deinit(self.allocator);
    }

    pub fn nextPending(self: *Queue) ?*Pending {
        for (self.items.items) |*item| {
            if (item.specialized == null) return item;
        }
        return null;
    }

    pub fn solvedDefs(self: *const Queue, allocator: std.mem.Allocator) std.mem.Allocator.Error![]ast.Def {
        var out = std.ArrayList(ast.Def).empty;
        errdefer out.deinit(allocator);
        for (self.items.items) |item| {
            const fn_def = item.specialized orelse continue;
            try out.append(allocator, .{
                .bind = item.specialized_symbol,
                .value = .{ .fn_ = fn_def },
            });
        }
        return try out.toOwnedSlice(allocator);
    }
};

pub const FEnvEntry = struct {
    name: Symbol,
    fn_ty: TypeVarId,
    fn_def: solved.Ast.FnDef,
};

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

pub fn lookupFn(fenv: []const FEnvEntry, name: Symbol) ?FEnvEntry {
    for (fenv) |entry| {
        if (entry.name == name) return entry;
    }
    return null;
}

pub fn specializeFnLset(
    queue: *Queue,
    fenv: []const FEnvEntry,
    symbols: *symbol_mod.Store,
    solved_types: *solved.Type.Store,
    mono_types: *type_mod.Store,
    requested_name: Symbol,
    requested_ty: TypeVarId,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFn(fenv, requested_name) orelse debugPanic("lambdamono.specializations.specializeFnLset missing function");

    var mono_cache = lower_type.MonoCache.init(queue.allocator);
    defer mono_cache.deinit();

    const requested_fn = lower_type.extractFn(solved_types, requested_ty);
    const arg_ty = try lower_type.lowerType(solved_types, mono_types, &mono_cache, requested_fn.arg, symbols);
    const ret_ty = try lower_type.lowerType(solved_types, mono_types, &mono_cache, requested_fn.ret, symbols);
    const captures_spec: CaptureSpec = switch (try lower_type.extractLsetFn(
        solved_types,
        mono_types,
        &mono_cache,
        requested_ty,
        requested_name,
        symbols,
    )) {
        .toplevel => .toplevel,
        .lset => |info| .{ .lset = info.ty },
    };

    for (queue.items.items) |item| {
        if (item.name != requested_name) continue;
        var item_cache = lower_type.MonoCache.init(queue.allocator);
        defer item_cache.deinit();

        const item_fn = lower_type.extractFn(solved_types, item.requested_ty);
        const item_arg = try lower_type.lowerType(solved_types, mono_types, &item_cache, item_fn.arg, symbols);
        const item_ret = try lower_type.lowerType(solved_types, mono_types, &item_cache, item_fn.ret, symbols);
        const item_captures: CaptureSpec = if (item.captures_new) |item_captures_new|
            if (item_captures_new.len == 0)
                .toplevel
            else
                .{ .erased = try lower_type.lowerCaptureBindings(
                    solved_types,
                    mono_types,
                    &item_cache,
                    item_captures_new,
                    symbols,
                ) }
        else switch (try lower_type.extractLsetFn(
            solved_types,
            mono_types,
            &item_cache,
            item.requested_ty,
            item.name,
            symbols,
        )) {
            .toplevel => .toplevel,
            .lset => |info| .{ .lset = info.ty },
        };

        if (!mono_types.equalIds(item_arg, arg_ty)) continue;
        if (!mono_types.equalIds(item_ret, ret_ty)) continue;
        if (!captureSpecsEqual(mono_types, item_captures, captures_spec)) continue;
        return item.specialized_symbol;
    }

    const source_entry = symbols.get(requested_name);
    const specialized_symbol = try symbols.add(source_entry.name, .{
        .specialized_top_level_def = .{
            .source_symbol = requested_name.raw(),
        },
    });
    try queue.items.append(queue.allocator, .{
        .name = entry.name,
        .fn_ty = entry.fn_ty,
        .fn_def = entry.fn_def,
        .requested_ty = requested_ty,
        .specialized_symbol = specialized_symbol,
    });
    return specialized_symbol;
}

pub fn specializeFnErased(
    queue: *Queue,
    fenv: []const FEnvEntry,
    symbols: *symbol_mod.Store,
    solved_types: *solved.Type.Store,
    mono_types: *type_mod.Store,
    requested_name: Symbol,
    requested_ty: TypeVarId,
    captures_new: []const lower_type.CaptureBinding,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFn(fenv, requested_name) orelse debugPanic("lambdamono.specializations.specializeFnErased missing function");

    var mono_cache = lower_type.MonoCache.init(queue.allocator);
    defer mono_cache.deinit();

    const requested_fn = lower_type.extractFn(solved_types, requested_ty);
    const arg_ty = try lower_type.lowerType(solved_types, mono_types, &mono_cache, requested_fn.arg, symbols);
    const ret_ty = try lower_type.lowerType(solved_types, mono_types, &mono_cache, requested_fn.ret, symbols);
    const captures_spec: CaptureSpec = if (captures_new.len == 0)
        .toplevel
    else
        .{ .erased = try lower_type.lowerCaptureBindings(
            solved_types,
            mono_types,
            &mono_cache,
            captures_new,
            symbols,
        ) };

    for (queue.items.items) |item| {
        if (item.name != requested_name) continue;
        var item_cache = lower_type.MonoCache.init(queue.allocator);
        defer item_cache.deinit();

        const item_fn = lower_type.extractFn(solved_types, item.requested_ty);
        const item_arg = try lower_type.lowerType(solved_types, mono_types, &item_cache, item_fn.arg, symbols);
        const item_ret = try lower_type.lowerType(solved_types, mono_types, &item_cache, item_fn.ret, symbols);
        const item_captures: CaptureSpec = if (item.captures_new) |item_captures_new|
            if (item_captures_new.len == 0)
                .toplevel
            else
                .{ .erased = try lower_type.lowerCaptureBindings(
                    solved_types,
                    mono_types,
                    &item_cache,
                    item_captures_new,
                    symbols,
                ) }
        else switch (try lower_type.extractLsetFn(
            solved_types,
            mono_types,
            &item_cache,
            item.requested_ty,
            item.name,
            symbols,
        )) {
            .toplevel => CaptureSpec.toplevel,
            .lset => |info| CaptureSpec{ .lset = info.ty },
        };

        if (!mono_types.equalIds(item_arg, arg_ty)) continue;
        if (!mono_types.equalIds(item_ret, ret_ty)) continue;
        if (!captureSpecsEqual(mono_types, item_captures, captures_spec)) continue;
        return item.specialized_symbol;
    }

    const source_entry = symbols.get(requested_name);
    const specialized_symbol = try symbols.add(source_entry.name, .{
        .specialized_top_level_def = .{
            .source_symbol = requested_name.raw(),
        },
    });
    try queue.items.append(queue.allocator, .{
        .name = entry.name,
        .fn_ty = entry.fn_ty,
        .fn_def = entry.fn_def,
        .requested_ty = requested_ty,
        .specialized_symbol = specialized_symbol,
        .captures_new = try queue.allocator.dupe(lower_type.CaptureBinding, captures_new),
    });
    return specialized_symbol;
}

fn captureSpecsEqual(
    mono_types: *const type_mod.Store,
    left: CaptureSpec,
    right: CaptureSpec,
) bool {
    return switch (left) {
        .toplevel => right == .toplevel,
        .lset => |left_ty| switch (right) {
            .toplevel => false,
            .lset => |right_ty| mono_types.equalIds(left_ty, right_ty),
            .erased => false,
        },
        .erased => |left_ty| switch (right) {
            .toplevel => false,
            .lset => false,
            .erased => |right_ty| mono_types.equalIds(left_ty, right_ty),
        },
    };
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
