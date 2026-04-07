//! Lower executable lambdamono types into logical cor-style IR layouts.

const std = @import("std");
const mono = @import("lambdamono");
const ir = @import("layout.zig");

pub const LayoutCache = std.AutoHashMap(mono.Type.TypeId, ir.LayoutId);

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
    if (cache.get(ty)) |cached| {
        return cached;
    }

    const placeholder = try ir_layouts.addPlaceholder();
    try cache.put(ty, placeholder);

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
    return placeholder;
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
