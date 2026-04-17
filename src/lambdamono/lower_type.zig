//! Lower solved lambda-set types into executable lambdamono types.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const solved = @import("lambdasolved");
const mono = @import("type.zig");
const symbol_mod = @import("symbol");

const TypeVarId = solved.Type.TypeVarId;
const Symbol = symbol_mod.Symbol;

pub const CaptureInfo = struct {
    captures: []const solved.Type.Capture,
    ty: mono.TypeId,
};

pub const CaptureBinding = struct {
    symbol: Symbol,
    solved_ty: TypeVarId,
    lowered_ty: mono.TypeId,
};

pub const SpecificLambdaRepr = union(enum) {
    toplevel,
    lset: CaptureInfo,
};

pub const LambdaRepr = union(enum) {
    lset: []const solved.Type.Lambda,
    erased,
};

pub const MonoCache = struct {
    active: std.AutoHashMap(TypeVarId, mono.TypeId),
    provisional: std.AutoHashMap(TypeVarId, mono.TypeId),
    resolved: std.AutoHashMap(TypeVarId, mono.TypeId),

    pub fn init(allocator: std.mem.Allocator) MonoCache {
        return .{
            .active = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .provisional = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .resolved = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *MonoCache) void {
        self.resolved.deinit();
        self.provisional.deinit();
        self.active.deinit();
    }
};

pub fn extractFn(types: *solved.Type.Store, ty: TypeVarId) struct {
    arg: TypeVarId,
    lset: TypeVarId,
    ret: TypeVarId,
} {
    const id = types.unlink(ty);
    return switch (types.getNode(id)) {
        .content => |content| switch (content) {
            .func => |func| .{
                .arg = func.arg,
                .lset = func.lset,
                .ret = func.ret,
            },
            else => debugPanic("lambdamono.lower_type.extractFn expected function"),
        },
        else => debugPanic("lambdamono.lower_type.extractFn expected function"),
    };
}

pub fn lambdaRepr(types: *solved.Type.Store, ty: TypeVarId) LambdaRepr {
    const fn_ty = extractFn(types, ty);
    const lset = types.unlink(fn_ty.lset);
    return switch (types.getNode(lset)) {
        .content => |content| switch (content) {
            .lambda_set => |span| .{ .lset = types.sliceLambdas(span) },
            .primitive => |prim| switch (prim) {
                .erased => .erased,
                else => debugPanic("lambdamono.lower_type.lambdaRepr expected lambda set"),
            },
            else => debugPanic("lambdamono.lower_type.lambdaRepr expected lambda set"),
        },
        .unbd,
        .for_a,
        => .erased,
        else => debugPanic("lambdamono.lower_type.lambdaRepr expected lambda set"),
    };
}

pub fn extractLsetFn(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    lambda_symbol: Symbol,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!SpecificLambdaRepr {
    return switch (lambdaRepr(types, ty)) {
        .erased => debugPanic("lambdamono.lower_type.extractLsetFn attempted concrete lambda extraction from erased callable"),
        .lset => |lambdas| blk: {
            for (lambdas) |lambda| {
                if (lambda.symbol != lambda_symbol) continue;
                const captures = types.sliceCaptures(lambda.captures);
                if (captures.len == 0) break :blk .toplevel;
                break :blk .{ .lset = .{
                    .captures = captures,
                    .ty = try lowerCaptures(types, mono_types, mono_cache, captures, symbols),
                } };
            }
            debugPanic("lambdamono.lower_type.extractLsetFn missing lambda in lambda set");
        },
    };
}

pub fn lowerCaptureBindings(
    _: *solved.Type.Store,
    mono_types: *mono.Store,
    _: *MonoCache,
    captures: []const CaptureBinding,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    const fields = try mono_types.allocator.alloc(mono.Field, captures.len);
    defer mono_types.allocator.free(fields);

    for (captures, 0..) |capture, i| {
        fields[i] = .{
            .name = symbols.get(capture.symbol).name,
            .ty = capture.lowered_ty,
        };
    }

    const raw = try mono_types.addType(.{ .record = .{
        .fields = try mono_types.dupeFields(fields),
    } });
    if (!mono_types.isFullyResolved(raw)) return raw;
    return try mono_types.internTypeId(raw);
}

fn lowerTypeRec(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    const id = unlinkExecutable(types, ty);
    if (mono_cache.active.get(id)) |active| return active;
    if (mono_cache.resolved.get(id)) |cached| return cached;
    if (mono_cache.provisional.get(id)) |provisional| {
        if (mono_types.isFullyResolved(provisional)) {
            const canonical = try mono_types.internTypeId(provisional);
            const removed = mono_cache.provisional.remove(id);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(removed);
            } else if (!removed) {
                unreachable;
            }
            try mono_cache.resolved.put(id, canonical);
            return canonical;
        }
        return provisional;
    }

    const placeholder = try mono_types.addType(.placeholder);
    try mono_cache.active.put(id, placeholder);

    const lowered: mono.Content = switch (types.getNode(id)) {
        .link => unreachable,
        .nominal => |nominal| blk: {
            const args = types.sliceTypeVarSpan(nominal.args);
            const lowered_args = try mono_types.allocator.alloc(mono.TypeId, args.len);
            defer mono_types.allocator.free(lowered_args);
            for (args, 0..) |arg, i| {
                lowered_args[i] = try lowerTypeRec(types, mono_types, mono_cache, arg, symbols);
            }
            break :blk .{ .nominal = .{
                .module_idx = nominal.module_idx,
                .ident = nominal.ident,
                .is_opaque = nominal.is_opaque,
                .args = try mono_types.dupeTypeIds(lowered_args),
                .backing = try lowerTypeRec(types, mono_types, mono_cache, nominal.backing, symbols),
            } };
        },
        .for_a, .unbd => .{ .record = .{ .fields = &.{} } },
        .content => |content| switch (content) {
            .func => blk: {
                const unit_ty = try mono_types.internResolved(.{
                    .record = .{ .fields = &.{} },
                });
                break :blk .{ .erased_fn = unit_ty };
            },
            .primitive => |prim| .{ .primitive = prim },
            .list => |elem| .{
                .list = try lowerTypeRec(types, mono_types, mono_cache, elem, symbols),
            },
            .box => |elem| .{
                .box = try lowerTypeRec(types, mono_types, mono_cache, elem, symbols),
            },
            .tuple => |tuple| blk: {
                const elems = types.sliceTypeVarSpan(tuple);
                const lowered_elems = try mono_types.allocator.alloc(mono.TypeId, elems.len);
                defer mono_types.allocator.free(lowered_elems);
                for (elems, 0..) |elem, i| {
                    lowered_elems[i] = try lowerTypeRec(types, mono_types, mono_cache, elem, symbols);
                }
                break :blk .{ .tuple = try mono_types.dupeTypeIds(lowered_elems) };
            },
            .record => |record| blk: {
                const fields = types.sliceFields(record.fields);
                const out = try mono_types.allocator.alloc(mono.Field, fields.len);
                defer mono_types.allocator.free(out);
                for (fields, 0..) |field, i| {
                    out[i] = .{
                        .name = field.name,
                        .ty = try lowerTypeRec(types, mono_types, mono_cache, field.ty, symbols),
                    };
                }
                break :blk .{ .record = .{
                    .fields = try mono_types.dupeFields(out),
                } };
            },
            .tag_union => |tag_union| blk: {
                const tags = types.sliceTags(tag_union.tags);
                const out = try mono_types.allocator.alloc(mono.Tag, tags.len);
                defer mono_types.allocator.free(out);
                defer for (out[0..tags.len]) |tag| {
                    if (tag.args.len > 0) mono_types.allocator.free(tag.args);
                };
                for (tags, 0..) |tag, i| {
                    const args = types.sliceTypeVarSpan(tag.args);
                    const lowered_args = try mono_types.allocator.alloc(mono.TypeId, args.len);
                    defer mono_types.allocator.free(lowered_args);
                    for (args, 0..) |arg, arg_i| {
                        lowered_args[arg_i] = try lowerTypeRec(types, mono_types, mono_cache, arg, symbols);
                    }
                    out[i] = .{
                        .name = .{ .ctor = tag.name },
                        .args = try mono_types.dupeTypeIds(lowered_args),
                    };
                }
                break :blk .{ .tag_union = .{
                    .tags = try mono_types.dupeTags(out),
                } };
            },
            .lambda_set => |span| try lowerLambdaSet(types, mono_types, mono_cache, types.sliceLambdas(span), symbols),
        },
    };

    mono_types.setType(placeholder, lowered);
    const removed = mono_cache.active.remove(id);
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(removed);
    } else if (!removed) {
        unreachable;
    }
    if (mono_types.isFullyResolved(placeholder)) {
        const canonical = try mono_types.internTypeId(placeholder);
        try mono_cache.resolved.put(id, canonical);
        return canonical;
    }

    try mono_cache.provisional.put(id, placeholder);
    return placeholder;
}

fn lowerLambdaSet(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    lambdas: []const solved.Type.Lambda,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.Content {
    const copied_lambdas = try mono_types.allocator.dupe(solved.Type.Lambda, lambdas);
    defer mono_types.allocator.free(copied_lambdas);

    const out = try mono_types.allocator.alloc(mono.Tag, copied_lambdas.len);
    defer mono_types.allocator.free(out);
    defer for (out[0..copied_lambdas.len]) |tag| {
        if (tag.args.len > 0) mono_types.allocator.free(tag.args);
    };

    for (copied_lambdas, 0..) |lambda, i| {
        const captures = try mono_types.allocator.dupe(solved.Type.Capture, types.sliceCaptures(lambda.captures));
        defer mono_types.allocator.free(captures);
        if (captures.len == 0) {
            out[i] = .{
                .name = lambdaTagKey(lambda.symbol),
                .args = &.{},
            };
        } else {
            const captures_ty = try lowerCaptures(types, mono_types, mono_cache, captures, symbols);
            const args = try mono_types.allocator.alloc(mono.TypeId, 1);
            defer mono_types.allocator.free(args);
            args[0] = captures_ty;
            out[i] = .{
                .name = lambdaTagKey(lambda.symbol),
                .args = try mono_types.dupeTypeIds(args),
            };
        }
    }

    return .{ .tag_union = .{
        .tags = try mono_types.dupeTags(out),
    } };
}

pub fn lowerCaptures(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    captures: []const solved.Type.Capture,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    const capture_bindings = try mono_types.allocator.alloc(CaptureBinding, captures.len);
    defer mono_types.allocator.free(capture_bindings);

    for (captures, 0..) |capture, i| {
        capture_bindings[i] = .{
            .symbol = capture.symbol,
            .solved_ty = capture.ty,
            .lowered_ty = try lowerTypeRec(types, mono_types, mono_cache, capture.ty, symbols),
        };
    }

    return try lowerCaptureBindings(types, mono_types, mono_cache, capture_bindings, symbols);
}

fn unlinkExecutable(types: *solved.Type.Store, ty: TypeVarId) TypeVarId {
    const id = types.unlinkPreservingNominal(ty);
    return switch (types.getNode(id)) {
        .nominal => id,
        .content => |content| switch (content) {
            .func => |func| if (lambdaSetIsErased(types, func.lset)) id else unlinkExecutable(types, func.lset),
            else => id,
        },
        else => id,
    };
}

fn lambdaSetIsErased(types: *solved.Type.Store, lset: TypeVarId) bool {
    const id = types.unlink(lset);
    return switch (types.getNode(id)) {
        .content => |content| switch (content) {
            .primitive => |prim| prim == .erased,
            else => false,
        },
        else => false,
    };
}

pub fn lambdaTagKey(symbol: Symbol) mono.TagName {
    return .{ .lambda = symbol };
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
