//! Lower solved lambda-set types into executable lambdamono types.

const std = @import("std");
const builtin = @import("builtin");
const solved = @import("lambdasolved");
const mono = @import("type.zig");
const symbol_mod = @import("symbol");

const TypeVarId = solved.Type.TypeVarId;
const Symbol = symbol_mod.Symbol;

/// Lowered capture binding used when constructing executable capture records.
pub const CaptureBinding = struct {
    symbol: Symbol,
    lowered_ty: mono.TypeId,
};

/// Specialization-local executable type cache over a frozen solved world.
pub const MonoCache = struct {
    active: std.AutoHashMap(TypeVarId, mono.TypeId),
    provisional: std.AutoHashMap(TypeVarId, mono.TypeId),
    resolved: std.AutoHashMap(TypeVarId, mono.TypeId),
    erased_active: std.AutoHashMap(TypeVarId, mono.TypeId),
    erased_provisional: std.AutoHashMap(TypeVarId, mono.TypeId),
    erased_resolved: std.AutoHashMap(TypeVarId, mono.TypeId),

    /// Initialize an empty specialization-local executable type cache.
    pub fn init(allocator: std.mem.Allocator) MonoCache {
        return .{
            .active = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .provisional = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .resolved = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .erased_active = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .erased_provisional = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
            .erased_resolved = std.AutoHashMap(TypeVarId, mono.TypeId).init(allocator),
        };
    }

    /// Release all cache memory.
    pub fn deinit(self: *MonoCache) void {
        self.erased_resolved.deinit();
        self.erased_provisional.deinit();
        self.erased_active.deinit();
        self.resolved.deinit();
        self.provisional.deinit();
        self.active.deinit();
    }
};

/// Lower solved capture bindings into an executable record type.
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

const LowerMode = enum {
    natural,
    erased_boundary,
};

const CacheView = struct {
    active: *std.AutoHashMap(TypeVarId, mono.TypeId),
    provisional: *std.AutoHashMap(TypeVarId, mono.TypeId),
    resolved: *std.AutoHashMap(TypeVarId, mono.TypeId),
};

fn cacheForMode(mono_cache: *MonoCache, mode: LowerMode) CacheView {
    return switch (mode) {
        .natural => .{
            .active = &mono_cache.active,
            .provisional = &mono_cache.provisional,
            .resolved = &mono_cache.resolved,
        },
        .erased_boundary => .{
            .active = &mono_cache.erased_active,
            .provisional = &mono_cache.erased_provisional,
            .resolved = &mono_cache.erased_resolved,
        },
    };
}

fn lowerTypeRec(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    return try lowerTypeRecMode(types, mono_types, mono_cache, ty, symbols, .natural);
}

fn lowerTypeRecMode(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error!mono.TypeId {
    const id = types.unlinkPreservingNominal(ty);
    const cache = cacheForMode(mono_cache, mode);
    if (cache.active.get(id)) |active| return active;
    if (cache.resolved.get(id)) |cached| return cached;
    if (cache.provisional.get(id)) |provisional| {
        if (mono_types.isFullyResolved(provisional)) {
            const canonical = try mono_types.internTypeId(provisional);
            const removed = cache.provisional.remove(id);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(removed);
            } else if (!removed) {
                unreachable;
            }
            try cache.resolved.put(id, canonical);
            return canonical;
        }
        return provisional;
    }

    const placeholder = try mono_types.addType(.placeholder);
    try cache.active.put(id, placeholder);

    const lowered: mono.Content = switch (types.getNode(id)) {
        .link => unreachable,
        .nominal => |nominal| blk: {
            break :blk .{ .nominal = .{
                .module_idx = nominal.module_idx,
                .ident = nominal.ident,
                .is_opaque = nominal.is_opaque,
                .args = try lowerNominalArgs(types, mono_types, mono_cache, nominal.args, symbols, mode),
                .backing = try lowerTypeRecMode(types, mono_types, mono_cache, nominal.backing, symbols, mode),
            } };
        },
        .unbd, .for_a, .flex_for_a => switch (mode) {
            .natural => .unbd,
            .erased_boundary => .{ .primitive = .erased },
        },
        .content => |content| switch (content) {
            .func => try lowerFunctionType(types, mono_types, mono_cache, id, symbols, mode),
            .primitive => |prim| .{ .primitive = prim },
            .list => |elem| .{
                .list = try lowerTypeRecMode(types, mono_types, mono_cache, elem, symbols, mode),
            },
            .box => |elem| .{ .box = switch (mode) {
                .natural => try lowerBoxPayloadType(types, mono_types, mono_cache, elem, symbols),
                .erased_boundary => try lowerTypeRecMode(types, mono_types, mono_cache, elem, symbols, .erased_boundary),
            } },
            .tuple => |tuple| blk: {
                const elems = types.sliceTypeVarSpan(tuple);
                const lowered_elems = try mono_types.allocator.alloc(mono.TypeId, elems.len);
                defer mono_types.allocator.free(lowered_elems);
                for (elems, 0..) |elem, i| {
                    lowered_elems[i] = try lowerTypeRecMode(types, mono_types, mono_cache, elem, symbols, mode);
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
                        .ty = try lowerTypeRecMode(types, mono_types, mono_cache, field.ty, symbols, mode),
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
                        lowered_args[arg_i] = try lowerTypeRecMode(types, mono_types, mono_cache, arg, symbols, mode);
                    }
                    out[i] = .{
                        .name = .{ .ctor = tag.name },
                        .args = try mono_types.dupeTypeIds(lowered_args),
                    };
                }
                break :blk .{ .tag_union = .{
                    .tags = try mono_types.dupeTags(out),
                    .call = null,
                } };
            },
            .lambda_set => |_| std.debug.panic(
                "lambdamono.lower_type bare lambda_set reached executable lowering without an enclosing function type",
                .{},
            ),
        },
    };

    mono_types.setType(placeholder, lowered);
    const removed = cache.active.remove(id);
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(removed);
    } else if (!removed) {
        unreachable;
    }
    if (mono_types.isFullyResolved(placeholder)) {
        const canonical = try mono_types.internTypeId(placeholder);
        try cache.resolved.put(id, canonical);
        return canonical;
    }

    try cache.provisional.put(id, placeholder);
    return placeholder;
}

fn lowerNominalArgs(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    args_span: solved.Type.Span(TypeVarId),
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error![]const mono.TypeId {
    const args = types.sliceTypeVarSpan(args_span);
    const lowered_args = try mono_types.allocator.alloc(mono.TypeId, args.len);
    defer mono_types.allocator.free(lowered_args);
    for (args, 0..) |arg, i| {
        lowered_args[i] = try lowerTypeRecMode(types, mono_types, mono_cache, arg, symbols, mode);
    }
    return try mono_types.dupeTypeIds(lowered_args);
}

fn lowerCallableSig(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    fn_ty: TypeVarId,
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error!mono.CallableSig {
    const fn_shape = types.fnShape(fn_ty);
    const arg_vars = types.sliceTypeVarSpan(fn_shape.args);
    const lowered_args = try mono_types.allocator.alloc(mono.TypeId, arg_vars.len);
    defer mono_types.allocator.free(lowered_args);
    for (arg_vars, 0..) |arg_var, i| {
        lowered_args[i] = try lowerTypeRecMode(types, mono_types, mono_cache, arg_var, symbols, mode);
    }
    return .{
        .args = try mono_types.dupeTypeIds(lowered_args),
        .ret = try lowerTypeRecMode(types, mono_types, mono_cache, fn_shape.ret, symbols, mode),
    };
}

fn lowerLambdaSetTags(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    lambdas: []const solved.Type.Lambda,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error![]const mono.Tag {
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
            const captures_ty = try lowerCapturesMode(types, mono_types, mono_cache, captures, symbols, .natural);
            const args = try mono_types.allocator.alloc(mono.TypeId, 1);
            defer mono_types.allocator.free(args);
            args[0] = captures_ty;
            out[i] = .{
                .name = lambdaTagKey(lambda.symbol),
                .args = try mono_types.dupeTypeIds(args),
            };
        }
    }

    return try mono_types.dupeTags(out);
}

fn commonErasedCaptureType(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    lambdas: []const solved.Type.Lambda,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!?mono.TypeId {
    return try commonErasedCaptureTypeMode(types, mono_types, mono_cache, lambdas, symbols, .erased_boundary);
}

fn commonErasedCaptureTypeMode(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    lambdas: []const solved.Type.Lambda,
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error!?mono.TypeId {
    var common: ?mono.TypeId = null;
    for (lambdas) |lambda| {
        const captures = types.sliceCaptures(lambda.captures);
        const next: ?mono.TypeId = if (captures.len == 0)
            null
        else
            try lowerCapturesMode(types, mono_types, mono_cache, captures, symbols, mode);
        if (common == null) {
            common = next;
            continue;
        }
        if (common == null and next == null) continue;
        if (common == null or next == null or common.? != next.?) {
            @branchHint(.cold);
            std.debug.panic(
                "lambdamono.lower_type boxed callable variants require a common capture type",
                .{},
            );
        }
    }
    return common;
}

fn lowerBoxPayloadType(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    elem: TypeVarId,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    if (types.maybeLambdaRepr(elem)) |repr| {
        const fn_id = requireFunctionType(types, elem);
        const call_sig = try lowerCallableSig(types, mono_types, mono_cache, fn_id, symbols, .erased_boundary);
        return switch (repr) {
            .lset => |lambdas| try mono_types.internResolved(.{
                .erased_fn = .{
                    .capture = try commonErasedCaptureType(types, mono_types, mono_cache, lambdas, symbols),
                    .call = call_sig,
                },
            }),
            .erased => try mono_types.internResolved(.{ .erased_fn = .{
                .capture = null,
                .call = call_sig,
            } }),
        };
    }
    return try lowerTypeRec(types, mono_types, mono_cache, elem, symbols);
}

/// Lower solved lambda captures into an executable capture-record type.
pub fn lowerCaptures(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    captures: []const solved.Type.Capture,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    return try lowerCapturesMode(types, mono_types, mono_cache, captures, symbols, .natural);
}

fn lowerCapturesMode(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    captures: []const solved.Type.Capture,
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error!mono.TypeId {
    const capture_bindings = try mono_types.allocator.alloc(CaptureBinding, captures.len);
    defer mono_types.allocator.free(capture_bindings);

    for (captures, 0..) |capture, i| {
        capture_bindings[i] = .{
            .symbol = capture.symbol,
            .lowered_ty = try lowerTypeRecMode(types, mono_types, mono_cache, capture.ty, symbols, mode),
        };
    }

    return try lowerCaptureBindings(types, mono_types, mono_cache, capture_bindings, symbols);
}

/// Lower one solved type into an executable type without applying boundary erasure.
pub fn lowerType(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    return try lowerTypeRec(types, mono_types, mono_cache, ty, symbols);
}

/// Lower one solved type into an erased-boundary executable type.
pub fn lowerErasedBoundaryType(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    ty: TypeVarId,
    symbols: *const symbol_mod.Store,
) std.mem.Allocator.Error!mono.TypeId {
    return try lowerTypeRecMode(types, mono_types, mono_cache, ty, symbols, .erased_boundary);
}

fn lowerFunctionType(
    types: *solved.Type.Store,
    mono_types: *mono.Store,
    mono_cache: *MonoCache,
    fn_ty: TypeVarId,
    symbols: *const symbol_mod.Store,
    mode: LowerMode,
) std.mem.Allocator.Error!mono.Content {
    const callable_mode: LowerMode = if (mode == .erased_boundary or lambdaSetIsErased(types, types.fnShape(fn_ty).lset))
        .erased_boundary
    else
        .natural;

    const call_sig = try lowerCallableSig(types, mono_types, mono_cache, fn_ty, symbols, callable_mode);
    return switch (callable_mode) {
        .natural => .{ .tag_union = .{
            .tags = try lowerLambdaSetTags(
                types,
                mono_types,
                mono_cache,
                switch (types.lambdaRepr(fn_ty)) {
                    .lset => |lambdas| lambdas,
                    .erased => std.debug.panic("lambdamono.lower_type natural callable lowering encountered erased lambda repr", .{}),
                },
                symbols,
            ),
            .call = call_sig,
        } },
        .erased_boundary => .{ .erased_fn = .{
            .capture = switch (types.lambdaRepr(fn_ty)) {
                .lset => |lambdas| try commonErasedCaptureTypeMode(
                    types,
                    mono_types,
                    mono_cache,
                    lambdas,
                    symbols,
                    .erased_boundary,
                ),
                .erased => null,
            },
            .call = call_sig,
        } },
    };
}

fn requireFunctionType(types: *solved.Type.Store, ty: TypeVarId) TypeVarId {
    const id = types.unlinkPreservingNominal(ty);
    return switch (types.getNode(id)) {
        .nominal => |nominal| requireFunctionType(types, nominal.backing),
        .content => |content| switch (content) {
            .func => id,
            else => std.debug.panic("lambdamono.lower_type expected function type for callable lowering", .{}),
        },
        else => std.debug.panic("lambdamono.lower_type expected function type for callable lowering", .{}),
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

/// Convert a lambda symbol into its executable tag-union constructor key.
pub fn lambdaTagKey(symbol: Symbol) mono.TagName {
    return .{ .lambda = symbol };
}
