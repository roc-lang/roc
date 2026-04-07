//! Lower executable lambdamono types into logical cor-style IR layouts.

const std = @import("std");
const mono = @import("lambdamono");
const ir = @import("layout.zig");

pub const LayoutCache = struct {
    allocator: std.mem.Allocator,
    resolved_by_type: std.AutoHashMap(mono.Type.TypeId, ir.LayoutId),
    active_by_type: std.AutoHashMap(mono.Type.TypeId, ir.LayoutId),
    interned_by_key: std.StringHashMap(ir.LayoutId),
    scratch_key: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) LayoutCache {
        return .{
            .allocator = allocator,
            .resolved_by_type = std.AutoHashMap(mono.Type.TypeId, ir.LayoutId).init(allocator),
            .active_by_type = std.AutoHashMap(mono.Type.TypeId, ir.LayoutId).init(allocator),
            .interned_by_key = std.StringHashMap(ir.LayoutId).init(allocator),
            .scratch_key = .empty,
        };
    }

    pub fn deinit(self: *LayoutCache) void {
        var keys = self.interned_by_key.keyIterator();
        while (keys.next()) |key| {
            self.allocator.free(key.*);
        }
        self.scratch_key.deinit(self.allocator);
        self.interned_by_key.deinit();
        self.active_by_type.deinit();
        self.resolved_by_type.deinit();
    }
};

pub fn lowerType(
    mono_types: *mono.Type.Store,
    ir_layouts: *ir.Store,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!ir.LayoutId {
    return try lowerTypeRec(mono_types, ir_layouts, cache, ty);
}

fn lowerTypeRec(
    mono_types: *mono.Type.Store,
    ir_layouts: *ir.Store,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!ir.LayoutId {
    if (cache.resolved_by_type.get(ty)) |cached| {
        return cached;
    }
    if (cache.active_by_type.get(ty)) |active| {
        return active;
    }

    if (try lookupInternedLayoutId(mono_types, cache, ty)) |cached| {
        try cache.resolved_by_type.put(ty, cached);
        return cached;
    }

    const placeholder = try ir_layouts.addPlaceholder();
    try cache.active_by_type.put(ty, placeholder);

    const lowered_content: ir.Content = switch (mono_types.getTypePreservingNominal(ty)) {
        .primitive => |prim| .{ .primitive = lowerPrim(prim) },
        .nominal => |backing| .{
            .nominal = try lowerTypeRec(mono_types, ir_layouts, cache, backing),
        },
        .list => |elem| .{
            .list = try lowerTypeRec(mono_types, ir_layouts, cache, elem),
        },
        .box => |elem| .{
            .box = try lowerTypeRec(mono_types, ir_layouts, cache, elem),
        },
        .tuple => |tuple| blk: {
            const elems = mono_types.sliceTypeSpan(tuple);
            const layouts = try ir_layouts.allocator.alloc(ir.LayoutId, elems.len);
            defer ir_layouts.allocator.free(layouts);

            for (elems, 0..) |elem, i| {
                layouts[i] = try lowerTypeRec(mono_types, ir_layouts, cache, elem);
            }
            break :blk .{ .struct_ = try ir_layouts.addLayoutSpan(layouts) };
        },
        .record => |record| blk: {
            const fields = mono_types.sliceFields(record.fields);
            const layouts = try ir_layouts.allocator.alloc(ir.LayoutId, fields.len);
            defer ir_layouts.allocator.free(layouts);

            for (fields, 0..) |field, i| {
                layouts[i] = try lowerTypeRec(mono_types, ir_layouts, cache, field.ty);
            }
            break :blk .{ .struct_ = try ir_layouts.addLayoutSpan(layouts) };
        },
        .tag_union => |tag_union| blk: {
            const tags = mono_types.sliceTags(tag_union.tags);
            const variants = try ir_layouts.allocator.alloc(ir.LayoutId, tags.len);
            defer ir_layouts.allocator.free(variants);

            for (tags, 0..) |tag, i| {
                const args = mono_types.sliceTypeSpan(tag.args);
                if (args.len == 0) {
                    variants[i] = try ir_layouts.addContent(.{ .struct_ = try ir_layouts.addLayoutSpan(&.{}) });
                } else {
                    const payload_layouts = try ir_layouts.allocator.alloc(ir.LayoutId, args.len);
                    defer ir_layouts.allocator.free(payload_layouts);
                    for (args, 0..) |arg, arg_i| {
                        payload_layouts[arg_i] = try lowerTypeRec(mono_types, ir_layouts, cache, arg);
                    }
                    variants[i] = try ir_layouts.addContent(.{ .struct_ = try ir_layouts.addLayoutSpan(payload_layouts) });
                }
            }
            break :blk .{ .union_ = try ir_layouts.addLayoutSpan(variants) };
        },
    };

    ir_layouts.setContent(placeholder, lowered_content);
    _ = cache.active_by_type.remove(ty);
    try rememberInternedLayoutId(mono_types, cache, ty, placeholder);
    try cache.resolved_by_type.put(ty, placeholder);
    return placeholder;
}

fn lookupInternedLayoutId(
    mono_types: *mono.Type.Store,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
) std.mem.Allocator.Error!?ir.LayoutId {
    try buildTypeKey(mono_types, cache, ty);
    return cache.interned_by_key.get(cache.scratch_key.items);
}

fn rememberInternedLayoutId(
    mono_types: *mono.Type.Store,
    cache: *LayoutCache,
    ty: mono.Type.TypeId,
    layout_id: ir.LayoutId,
) std.mem.Allocator.Error!void {
    try buildTypeKey(mono_types, cache, ty);
    if (cache.interned_by_key.get(cache.scratch_key.items) != null) return;

    const owned_key = try cache.allocator.dupe(u8, cache.scratch_key.items);
    errdefer cache.allocator.free(owned_key);
    try cache.interned_by_key.put(owned_key, layout_id);
}

fn buildTypeKey(
    mono_types: *mono.Type.Store,
    cache: *LayoutCache,
    root: mono.Type.TypeId,
) std.mem.Allocator.Error!void {
    cache.scratch_key.clearRetainingCapacity();
    try cache.scratch_key.appendSlice(cache.allocator, "ITY");

    const VisitState = enum(u8) { unseen, active, done };
    var states = std.AutoHashMap(mono.Type.TypeId, VisitState).init(cache.allocator);
    defer states.deinit();
    var binder_ids = std.AutoHashMap(mono.Type.TypeId, u32).init(cache.allocator);
    defer binder_ids.deinit();
    var next_binder: u32 = 0;

    const Builder = struct {
        mono_types: *mono.Type.Store,
        cache: *LayoutCache,
        states: *std.AutoHashMap(mono.Type.TypeId, VisitState),
        binder_ids: *std.AutoHashMap(mono.Type.TypeId, u32),
        next_binder: *u32,

        fn appendValue(self: *@This(), value: anytype) std.mem.Allocator.Error!void {
            const bytes = std.mem.asBytes(&value);
            try self.cache.scratch_key.appendSlice(self.cache.allocator, bytes);
        }

        fn serializeType(self: *@This(), ty: mono.Type.TypeId) std.mem.Allocator.Error!void {
            const state = self.states.get(ty) orelse .unseen;
            switch (state) {
                .active, .done => {
                    try self.appendValue(@as(u8, 1));
                    try self.appendValue(self.binder_ids.get(ty).?);
                    return;
                },
                .unseen => {},
            }

            try self.states.put(ty, .active);
            try self.binder_ids.put(ty, self.next_binder.*);
            self.next_binder.* += 1;

            try self.appendValue(@as(u8, 2));
            switch (self.mono_types.getTypePreservingNominal(ty)) {
                .primitive => |prim| {
                    try self.appendValue(@as(u8, 10));
                    try self.appendValue(@intFromEnum(prim));
                },
                .nominal => |backing| {
                    try self.appendValue(@as(u8, 11));
                    try self.serializeType(backing);
                },
                .list => |elem| {
                    try self.appendValue(@as(u8, 12));
                    try self.serializeType(elem);
                },
                .box => |elem| {
                    try self.appendValue(@as(u8, 13));
                    try self.serializeType(elem);
                },
                .tuple => |tuple| {
                    const elems = self.mono_types.sliceTypeSpan(tuple);
                    try self.appendValue(@as(u8, 14));
                    try self.appendValue(@as(u32, @intCast(elems.len)));
                    for (elems) |elem| {
                        try self.serializeType(elem);
                    }
                },
                .record => |record| {
                    const fields = self.mono_types.sliceFields(record.fields);
                    try self.appendValue(@as(u8, 15));
                    try self.appendValue(@as(u32, @intCast(fields.len)));
                    for (fields) |field| {
                        try self.appendValue(field.name);
                        try self.serializeType(field.ty);
                    }
                },
                .tag_union => |tag_union| {
                    const tags = self.mono_types.sliceTags(tag_union.tags);
                    try self.appendValue(@as(u8, 16));
                    try self.appendValue(@as(u32, @intCast(tags.len)));
                    for (tags) |tag| {
                        try self.appendValue(tag.name);
                        const args = self.mono_types.sliceTypeSpan(tag.args);
                        try self.appendValue(@as(u32, @intCast(args.len)));
                        for (args) |arg| {
                            try self.serializeType(arg);
                        }
                    }
                },
            }

            try self.states.put(ty, .done);
        }
    };

    var builder = Builder{
        .mono_types = mono_types,
        .cache = cache,
        .states = &states,
        .binder_ids = &binder_ids,
        .next_binder = &next_binder,
    };
    try builder.serializeType(root);
}

fn lowerPrim(prim: mono.Type.Prim) ir.Prim {
    return switch (prim) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .erased => .opaque_ptr,
    };
}

test "ir lower_type tests" {
    std.testing.refAllDecls(@This());
}
