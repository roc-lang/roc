//! Cor-style executable specialization queue.

const std = @import("std");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const lower_type = @import("lower_type.zig");
const symbol_mod = @import("symbol");

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
    sig: SigKey,
    specialized_symbol: Symbol,
    specialized: ?ast.FnDef = null,
};

pub const Queue = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayList(Pending),
    by_key: std.AutoHashMap(SigKey, usize),

    pub fn init(allocator: std.mem.Allocator) Queue {
        return .{
            .allocator = allocator,
            .items = .empty,
            .by_key = std.AutoHashMap(SigKey, usize).init(allocator),
        };
    }

    pub fn deinit(self: *Queue) void {
        self.items.deinit(self.allocator);
        self.by_key.deinit();
    }

    pub fn nextPending(self: *Queue) ?*Pending {
        for (self.items.items) |*item| {
            if (item.specialized == null) return item;
        }
        return null;
    }

    pub fn solvedDefs(
        self: *const Queue,
        allocator: std.mem.Allocator,
        _: *solved.Type.Store,
        _: *type_mod.Store,
        _: *const symbol_mod.Store,
    ) std.mem.Allocator.Error![]ast.Def {
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

const CaptureKind = enum(u8) {
    toplevel,
    lset,
    erased,
};

pub const SigKey = struct {
    name: Symbol,
    arg_ty: type_mod.TypeId,
    ret_ty: type_mod.TypeId,
    capture_kind: CaptureKind,
    capture_ty: ?type_mod.TypeId,
};

pub fn makeSigKey(name: Symbol, arg_ty: type_mod.TypeId, ret_ty: type_mod.TypeId, captures: CaptureSpec) SigKey {
    return switch (captures) {
        .toplevel => .{
            .name = name,
            .arg_ty = arg_ty,
            .ret_ty = ret_ty,
            .capture_kind = .toplevel,
            .capture_ty = null,
        },
        .lset => |capture_ty| .{
            .name = name,
            .arg_ty = arg_ty,
            .ret_ty = ret_ty,
            .capture_kind = .lset,
            .capture_ty = capture_ty,
        },
        .erased => |capture_ty| .{
            .name = name,
            .arg_ty = arg_ty,
            .ret_ty = ret_ty,
            .capture_kind = .erased,
            .capture_ty = capture_ty,
        },
    };
}

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

pub fn lookupFnExact(fenv: []const FEnvEntry, name: Symbol) ?FEnvEntry {
    for (fenv) |entry| {
        if (entry.name == name) return entry;
    }
    return null;
}

pub fn lookupFnByCanonicalSource(fenv: []const FEnvEntry, symbols: *const symbol_mod.Store, source_name: Symbol) ?FEnvEntry {
    const canonical_name = canonicalSourceSymbol(symbols, source_name);
    var match: ?FEnvEntry = null;
    for (fenv) |entry| {
        if (canonicalSourceSymbol(symbols, entry.name) != canonical_name) continue;
        if (match != null) debugPanic("lambdamono.specializations.lookupFnByCanonicalSource ambiguous canonical source");
        match = entry;
    }
    return match;
}

pub fn specializeFnLset(
    queue: *Queue,
    fenv: []const FEnvEntry,
    symbols: *symbol_mod.Store,
    requested_name: Symbol,
    requested_ty: TypeVarId,
    sig: SigKey,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFnExact(fenv, requested_name) orelse
        debugPanic("lambdamono.specializations.specializeFnLset missing function");
    if (queue.by_key.get(sig)) |idx| {
        return queue.items.items[idx].specialized_symbol;
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
        .sig = sig,
        .specialized_symbol = specialized_symbol,
    });
    try queue.by_key.put(sig, queue.items.items.len - 1);
    return specialized_symbol;
}

pub fn specializeFnErased(
    queue: *Queue,
    fenv: []const FEnvEntry,
    symbols: *symbol_mod.Store,
    requested_name: Symbol,
    requested_ty: TypeVarId,
    sig: SigKey,
) std.mem.Allocator.Error!Symbol {
    const entry = lookupFnExact(fenv, requested_name) orelse
        debugPanic("lambdamono.specializations.specializeFnErased missing function");
    if (queue.by_key.get(sig)) |idx| {
        return queue.items.items[idx].specialized_symbol;
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
        .sig = sig,
        .specialized_symbol = specialized_symbol,
    });
    try queue.by_key.put(sig, queue.items.items.len - 1);
    return specialized_symbol;
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn canonicalSourceSymbol(symbols: *const symbol_mod.Store, symbol: Symbol) Symbol {
    var current = symbol;
    var depth: usize = 0;
    while (depth < 8) : (depth += 1) {
        const next: ?Symbol = switch (symbols.get(current).origin) {
            .specialized_top_level_def => |info| Symbol.fromRaw(info.source_symbol),
            .specialized_local_fn => |info| Symbol.fromRaw(info.source_symbol),
            .lifted_local_fn => |info| Symbol.fromRaw(info.source_symbol),
            .lifted_local_fn_alias => |info| Symbol.fromRaw(info.source_symbol),
            else => null,
        };
        const resolved = next orelse break;
        if (resolved == current) break;
        current = resolved;
    }
    return current;
}
