//! Literal declaration recipes preserve the exact Monotype specialization.
//! Exporting its ConstStore type closure is independent of lambda solving:
//! replay of the explicitly declared conversion produces callable solutions.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");
const Type = @import("monotype/type.zig");
const Common = @import("common.zig");
const Representation = @import("lir_core").Program.LiteralRepresentation;
const Const = check.ConstStore;
const Allocator = std.mem.Allocator;

/// Export the type closure rooted at `root` as a portable `Representation`.
pub fn representation(allocator: Allocator, types: Type.Store.View, names: *const check.CheckedNames.NameStore, root: Type.TypeId) Allocator.Error!Representation {
    var export_: Export = .{
        .allocator = allocator,
        .source = types,
        .store = Const.ConstTypeStore.init(allocator),
        .map = collections.DenseMap(Type.TypeId, Const.ConstTypeId).init(allocator),
    };
    defer export_.store.deinit();
    defer export_.map.deinit();
    defer export_.queue.deinit(allocator);
    const id = try export_.request(root);
    var index: usize = 0;
    while (index < export_.queue.items.len) : (index += 1) {
        const source = export_.queue.items[index];
        const target = export_.map.get(source).?;
        export_.store.fill(target, try export_.content(types.get(source)));
    }
    return Representation.capture(allocator, &export_.store, names, id);
}

const Export = struct {
    allocator: Allocator,
    source: Type.Store.View,
    store: Const.ConstTypeStore,
    map: collections.DenseMap(Type.TypeId, Const.ConstTypeId),
    queue: std.ArrayList(Type.TypeId) = .empty,

    fn request(self: *Export, ty: Type.TypeId) Allocator.Error!Const.ConstTypeId {
        if (self.map.get(ty)) |id| return id;
        const id = try self.store.reserve();
        try self.map.put(ty, id);
        try self.queue.append(self.allocator, ty);
        return id;
    }

    fn span(self: *Export, source: Type.Span) Allocator.Error!Const.ConstRange {
        const source_items = self.source.span(source);
        const ids = try self.allocator.alloc(Const.ConstTypeId, source_items.len);
        defer self.allocator.free(ids);
        for (source_items, ids) |ty, *id| id.* = try self.request(ty);
        return self.store.appendTypeSpan(ids);
    }

    fn content(self: *Export, value: Type.Content) Allocator.Error!Const.ConstType {
        return switch (value) {
            .primitive => |primitive| .{ .primitive = switch (primitive) {
                inline .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => |tag| @field(Const.Primitive, @tagName(tag)),
            } },
            .zst => .zst,
            .erased => |key| .{ .erased = key },
            .list => |ty| .{ .list = try self.request(ty) },
            .box => |ty| .{ .box = try self.request(ty) },
            .tuple => |items| .{ .tuple = try self.span(items) },
            .func => |func| .{ .func = .{ .args = try self.span(func.args), .ret = try self.request(func.ret) } },
            .record => |items| blk: {
                const source = self.source.fieldSpan(items);
                const fields = try self.allocator.alloc(Const.TypeField, source.len);
                defer self.allocator.free(fields);
                for (source, fields) |field, *out| {
                    if (field.kind_state != .resolved) Common.invariant("literal recipe exported an unresolved field slot");
                    out.* = .{
                        .name = field.name,
                        .ty = try self.request(field.ty),
                        .value_ty = if (field.value_ty) |ty| try self.request(ty) else null,
                        .default = if (field.default) |default| .{ .module = default.module, .expr_node = default.expr_node } else null,
                    };
                }
                break :blk .{ .record = try self.store.appendFieldSpan(fields) };
            },
            .tag_union => |items| blk: {
                const source = self.source.tagSpan(items);
                const tags = try self.allocator.alloc(Const.TypeTag, source.len);
                defer self.allocator.free(tags);
                for (source, tags) |tag, *out| out.* = .{
                    .name = tag.name,
                    .checked_name = tag.checked_name,
                    .payloads = try self.span(tag.payloads),
                };
                break :blk .{ .tag_union = try self.store.appendTagSpan(tags) };
            },
            .named => |named| blk: {
                const source = self.source.declaredFieldSpan(named.declared_order);
                const declared = try self.allocator.alloc(Const.TypeDeclaredField, source.len);
                defer self.allocator.free(declared);
                for (source, declared) |field, *out| out.* = switch (field) {
                    .named => |name| .{ .named = name },
                    .padding => |ty| .{ .padding = try self.request(ty) },
                };
                const topology: ?Const.IteratorTopology = if (named.def.iterator_topology) |value_| .{
                    .len_field = value_.len_field,
                    .step_field = value_.step_field,
                    .known_tag = value_.known_tag,
                    .unknown_tag = value_.unknown_tag,
                    .done_tag = value_.done_tag,
                    .one_tag = value_.one_tag,
                    .skip_tag = value_.skip_tag,
                    .item_field = value_.item_field,
                    .rest_field = value_.rest_field,
                } else null;
                break :blk .{ .named = .{
                    .named_type = .{ .module = named.named_type.module, .ty = named.named_type.ty },
                    .def = .{
                        .module = named.def.module,
                        .type_name = named.def.type_name,
                        .source_decl = named.def.source_decl,
                        .generated = named.def.generated,
                        .iterator_representation = named.def.iterator_representation,
                        .iterator_kind = named.def.iterator_kind,
                        .iterator_depth = named.def.iterator_depth,
                        .iterator_topology = topology,
                    },
                    .kind = switch (named.kind) {
                        inline .nominal, .@"opaque", .alias => |tag| @field(Const.TypeNamedKind, @tagName(tag)),
                    },
                    .builtin_owner = named.builtin_owner,
                    .args = try self.span(named.args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.request(backing.ty),
                        .use = switch (backing.use) {
                            inline .inspectable, .runtime_layout_only => |tag| @field(Const.TypeBackingUse, @tagName(tag)),
                        },
                        .authority = switch (backing.authority) {
                            inline .checked_public, .generated_private => |tag| @field(Const.TypeBackingAuthority, @tagName(tag)),
                        },
                    } else null,
                    .declared_order = try self.store.appendDeclaredFieldSpan(declared),
                } };
            },
        };
    }
};
