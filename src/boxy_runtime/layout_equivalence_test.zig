//! The descriptor-composition equivalence test (design.md "One
//! Representation Per Type"): a corpus of closed types is laid out both by
//! the layout store and by Boxy runtime composition, and the two must agree
//! on size, alignment, sort key, refcounting, field order and offsets,
//! variant discriminants, discriminant placement and width, and payload
//! shapes.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const layout = @import("layout");
const boxy = @import("boxy_runtime");

const testing = std.testing;
const compose = boxy.compose;
const TypeDesc = boxy.TypeDesc;
const Name = boxy.Name;
const Store = layout.Store;
const Idx = layout.Idx;
const target_usize = base.target.TargetUsize.native;

/// A closed type in the corpus.
const Ty = union(enum) {
    scalar: boxy.Scalar,
    str,
    bool,
    list: *const Ty,
    box: *const Ty,
    record: []const NamedTy,
    /// A nominal record with `_` padding, in declared order.
    declared: []const DeclaredTy,
    tuple: []const Ty,
    tag_union: []const TagTy,
};

const NamedTy = struct { name: []const u8, ty: Ty };
const DeclaredTy = struct { name: ?[]const u8, ty: Ty };
const TagTy = struct { name: []const u8, payloads: []const Ty };

const Builder = struct {
    store: *Store,
    test_env: *builtins.utils.TestEnv,
    cache: *compose.Cache,
    arena: std.mem.Allocator,
    names: std.StringHashMapUnmanaged(*const Name) = .empty,

    fn ops(self: *Builder) *builtins.host_abi.RocOps {
        return self.test_env.getOps();
    }

    fn name(self: *Builder, text: []const u8) !*const Name {
        if (self.names.get(text)) |existing| return existing;
        const created = try self.arena.create(Name);
        created.* = .{ .id = self.names.count(), .len = @intCast(text.len), .bytes = text.ptr };
        try self.names.put(self.arena, text, created);
        return created;
    }

    /// Each name's rank in byte-wise text order among `names`.
    fn ranks(self: *Builder, names: []const []const u8) ![]u32 {
        const order = try self.arena.alloc(u32, names.len);
        for (order, 0..) |*slot, index| slot.* = @intCast(index);
        std.mem.sort(u32, order, names, struct {
            fn lessThan(all: []const []const u8, a: u32, b: u32) bool {
                return std.mem.order(u8, all[a], all[b]) == .lt;
            }
        }.lessThan);
        const result = try self.arena.alloc(u32, names.len);
        for (order, 0..) |index, rank| result[index] = @intCast(rank);
        return result;
    }

    fn desc(self: *Builder, ty: Ty) !*const TypeDesc {
        return switch (ty) {
            .scalar => |scalar| compose.scalarDesc(scalar),
            .str => compose.strDesc(),
            .bool => compose.boolDesc(),
            .list => |elem| compose.list(self.cache, self.ops(), try self.desc(elem.*)),
            .box => |payload| compose.box(self.cache, self.ops(), try self.desc(payload.*)),
            .record => |fields| blk: {
                const inputs = try self.arena.alloc(compose.FieldInput, fields.len);
                for (fields, inputs) |field, *input| input.* = .{ .name = try self.name(field.name), .desc = try self.desc(field.ty) };
                break :blk compose.record(self.cache, self.ops(), inputs);
            },
            .declared => |fields| blk: {
                const inputs = try self.arena.alloc(compose.DeclaredFieldInput, fields.len);
                for (fields, inputs) |field, *input| input.* = .{
                    .name = if (field.name) |text| try self.name(text) else null,
                    .desc = try self.desc(field.ty),
                    .is_padding = @intFromBool(field.name == null),
                };
                break :blk compose.declaredRecord(self.cache, self.ops(), inputs);
            },
            .tuple => |elems| blk: {
                const descs = try self.arena.alloc(*const TypeDesc, elems.len);
                for (elems, descs) |elem, *slot| slot.* = try self.desc(elem);
                break :blk compose.tuple(self.cache, self.ops(), descs);
            },
            .tag_union => |tags| blk: {
                const inputs = try self.arena.alloc(compose.VariantInput, tags.len);
                for (tags, inputs) |tag, *input| {
                    const payloads = try self.arena.alloc(*const TypeDesc, tag.payloads.len);
                    for (tag.payloads, payloads) |payload, *slot| slot.* = try self.desc(payload);
                    input.* = .{ .name = try self.name(tag.name), .payload_count = @intCast(payloads.len), .payloads = payloads.ptr };
                }
                break :blk compose.tagUnion(self.cache, self.ops(), inputs);
            },
        };
    }

    fn layoutOf(self: *Builder, ty: Ty) !Idx {
        return switch (ty) {
            .scalar => |scalar| switch (scalar) {
                inline else => |s| @field(Idx, @tagName(s)),
            },
            .str => .str,
            .bool => .bool,
            .list => |elem| blk: {
                const elem_idx = try self.layoutOf(elem.*);
                if (self.store.layoutSize(self.store.getLayout(elem_idx)) == 0) break :blk try self.store.insertLayout(layout.Layout.listOfZst());
                break :blk try self.store.insertList(elem_idx);
            },
            .box => |payload| blk: {
                const payload_idx = try self.layoutOf(payload.*);
                if (self.store.layoutSize(self.store.getLayout(payload_idx)) == 0) break :blk try self.store.insertLayout(layout.Layout.boxOfZst());
                break :blk try self.store.insertBox(payload_idx);
            },
            .record => |fields| blk: {
                const names = try self.arena.alloc([]const u8, fields.len);
                for (fields, names) |field, *slot| slot.* = field.name;
                const field_ranks = try self.ranks(names);
                const struct_fields = try self.arena.alloc(layout.StructField, fields.len);
                for (fields, struct_fields, field_ranks) |field, *slot, rank| slot.* = .{ .index = rank, .layout = try self.layoutOf(field.ty) };
                break :blk try self.store.putStructFields(struct_fields);
            },
            .declared => |fields| blk: {
                var named: std.ArrayList([]const u8) = .empty;
                for (fields) |field| if (field.name) |text| try named.append(self.arena, text);
                const field_ranks = try self.ranks(named.items);
                const struct_fields = try self.arena.alloc(layout.StructField, fields.len);
                var next_named: usize = 0;
                var next_padding: u32 = @intCast(named.items.len);
                for (fields, struct_fields) |field, *slot| {
                    const index = if (field.name != null) field_ranks[next_named] else next_padding;
                    if (field.name != null) next_named += 1 else next_padding += 1;
                    slot.* = .{ .index = index, .layout = try self.layoutOf(field.ty), .is_padding = field.name == null };
                }
                break :blk try self.store.putNominalStructFields(struct_fields);
            },
            .tuple => |elems| blk: {
                const layouts = try self.arena.alloc(layout.Layout, elems.len);
                for (elems, layouts) |elem, *slot| slot.* = self.store.getLayout(try self.layoutOf(elem));
                break :blk try self.store.putTuple(layouts);
            },
            .tag_union => |tags| blk: {
                const names = try self.arena.alloc([]const u8, tags.len);
                for (tags, names) |tag, *slot| slot.* = tag.name;
                const tag_ranks = try self.ranks(names);
                const variants = try self.arena.alloc(Idx, tags.len);
                for (tags, tag_ranks) |tag, rank| {
                    variants[rank] = switch (tag.payloads.len) {
                        0 => .zst,
                        1 => try self.layoutOf(tag.payloads[0]),
                        else => payload: {
                            const layouts = try self.arena.alloc(layout.Layout, tag.payloads.len);
                            for (tag.payloads, layouts) |payload, *slot| slot.* = self.store.getLayout(try self.layoutOf(payload));
                            break :payload try self.store.putTuple(layouts);
                        },
                    };
                }
                break :blk try self.store.putTagUnion(variants);
            },
        };
    }

    /// Require the composed descriptor and the committed layout to describe
    /// the same memory shape.
    fn expectSame(self: *Builder, d: *const TypeDesc, idx: Idx) !void {
        const l = self.store.getLayout(idx);
        try testing.expectEqual(self.store.layoutSize(l), d.size);
        try testing.expectEqual(@as(u32, @intCast(l.alignment(target_usize).toByteUnits())), d.alignment);
        try testing.expectEqual(@as(u8, @backingInt(l.sortKey())), d.sort_key);
        try testing.expectEqual(self.store.layoutContainsRefcounted(l), d.flags.contains_refcounted);
        switch (d.kind) {
            .record, .tuple => {
                if (l.tag == .zst) return testing.expectEqual(@as(u32, 0), d.size);
                try testing.expectEqual(layout.LayoutTag.struct_, l.tag);
                const struct_idx = l.getStruct().idx;
                const fields = d.fields();
                try testing.expectEqual(self.store.getStructData(struct_idx).fields.count, @as(u32, @intCast(fields.len)));
                for (fields, 0..) |field, slot| {
                    const committed = self.store.getStructField(struct_idx, @intCast(slot));
                    try testing.expectEqual(committed.index, field.semantic_index);
                    try testing.expectEqual(committed.is_padding, field.is_padding != 0);
                    try testing.expectEqual(self.store.getStructFieldOffset(struct_idx, @intCast(slot)), field.offset);
                    try self.expectSame(field.desc, committed.layout);
                }
            },
            .tag_union => {
                if (l.tag == .zst) return testing.expectEqual(@as(u32, 0), d.size);
                try testing.expectEqual(layout.LayoutTag.tag_union, l.tag);
                const tu_idx = l.getTagUnion().idx;
                const data = self.store.getTagUnionData(tu_idx);
                try testing.expectEqual(@as(u32, data.discriminant_size), d.discriminant_size);
                try testing.expectEqual(self.store.getTagUnionDiscriminantOffset(tu_idx), d.discriminant_offset);
                const committed = self.store.getTagUnionVariants(data);
                const variants = d.variants();
                try testing.expectEqual(committed.len, variants.len);
                for (variants, 0..) |variant, index| {
                    try testing.expectEqual(@as(u32, @intCast(index)), variant.discriminant);
                    try self.expectSame(variant.payload, committed.get(index).payload_layout);
                }
            },
            .list => if (l.tag == .list) try self.expectSame(d.child.?, l.getIdx()) else {
                try testing.expectEqual(layout.LayoutTag.list_of_zst, l.tag);
                try testing.expectEqual(@as(u32, 0), d.child.?.size);
            },
            .box => if (l.tag == .box) try self.expectSame(d.child.?, l.getIdx()) else {
                try testing.expectEqual(layout.LayoutTag.box_of_zst, l.tag);
                try testing.expectEqual(@as(u32, 0), d.child.?.size);
            },
            .bool => try testing.expectEqual(Idx.bool, idx),
            else => {},
        }
    }
};

fn sc(scalar: boxy.Scalar) Ty {
    return .{ .scalar = scalar };
}

const unit: Ty = .{ .record = &.{} };
const i64_list: Ty = .{ .list = &sc(.i64) };
const str_list: Ty = .{ .list = &.str };
const mixed_record: Ty = .{ .record = &.{
    .{ .name = "count", .ty = sc(.u8) },
    .{ .name = "big", .ty = sc(.i128) },
    .{ .name = "name", .ty = .str },
    .{ .name = "amount", .ty = sc(.dec) },
    .{ .name = "items", .ty = i64_list },
    .{ .name = "flag", .ty = .bool },
    .{ .name = "half", .ty = sc(.u16) },
    .{ .name = "ratio", .ty = sc(.f32) },
} };
const nested_record: Ty = .{ .record = &.{
    .{ .name = "inner", .ty = mixed_record },
    .{ .name = "tag", .ty = sc(.u32) },
    .{ .name = "empty", .ty = unit },
    .{ .name = "point", .ty = .{ .tuple = &.{ sc(.f64), sc(.u8), .str } } },
} };
const result_union: Ty = .{ .tag_union = &.{
    .{ .name = "Ok", .payloads = &.{i64_list} },
    .{ .name = "Err", .payloads = &.{.str} },
} };
const three_union: Ty = .{ .tag_union = &.{
    .{ .name = "Leaf", .payloads = &.{} },
    .{ .name = "Pair", .payloads = &.{ sc(.u8), sc(.u64), .str } },
    .{ .name = "One", .payloads = &.{sc(.u16)} },
} };
const single_union: Ty = .{ .tag_union = &.{.{ .name = "Only", .payloads = &.{ sc(.u32), sc(.u8) } }} };
const unit_union: Ty = .{ .tag_union = &.{.{ .name = "Unit", .payloads = &.{} }} };
const declared_record: Ty = .{ .declared = &.{
    .{ .name = "tag", .ty = sc(.u8) },
    .{ .name = null, .ty = sc(.u16) },
    .{ .name = "value", .ty = sc(.u64) },
    .{ .name = "byte", .ty = sc(.u8) },
    .{ .name = null, .ty = sc(.u8) },
} };

const corpus = [_]Ty{
    sc(.u8),                                                                               sc(.i8),                                                           sc(.u16),                                                                                                                                            sc(.i16),                 sc(.u32),                      sc(.i32),
    sc(.u64),                                                                              sc(.i64),                                                          sc(.u128),                                                                                                                                           sc(.i128),                sc(.f32),                      sc(.f64),
    sc(.dec),                                                                              sc(.u8x16),                                                        sc(.i64x2),                                                                                                                                          sc(.opaque_ptr),          .str,                          .bool,
    unit,                                                                                  mixed_record,                                                      nested_record,                                                                                                                                       result_union,             three_union,                   single_union,
    unit_union,                                                                            declared_record,                                                   i64_list,                                                                                                                                            str_list,                 .{ .list = &unit },            .{ .box = &unit },
    .{ .list = &mixed_record },                                                            .{ .box = &mixed_record },                                         .{ .list = &result_union },                                                                                                                          .{ .box = &three_union }, .{ .list = &declared_record }, .{ .box = &.str },
    .{ .record = &.{ .{ .name = "z", .ty = unit }, .{ .name = "y", .ty = unit_union } } }, .{ .tuple = &.{ unit, sc(.u8), .{ .box = &sc(.u8) }, sc(.u16) } }, .{ .record = &.{ .{ .name = "nested", .ty = nested_record }, .{ .name = "union", .ty = three_union }, .{ .name = "single", .ty = single_union } } },
};

test "composed descriptors have the layout store's shapes" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    var store = try Store.init(testing.allocator, target_usize);
    defer store.deinit();
    var test_env = builtins.utils.TestEnv.init(testing.allocator);
    defer test_env.deinit();
    var cache: compose.Cache = .{};
    var builder: Builder = .{ .store = &store, .test_env = &test_env, .cache = &cache, .arena = arena_state.allocator() };

    for (corpus) |ty| try builder.expectSame(try builder.desc(ty), try builder.layoutOf(ty));
}

test "tag unions of 257 variants widen their discriminant like the store's" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    var store = try Store.init(testing.allocator, target_usize);
    defer store.deinit();
    var test_env = builtins.utils.TestEnv.init(testing.allocator);
    defer test_env.deinit();
    var cache: compose.Cache = .{};
    var builder: Builder = .{ .store = &store, .test_env = &test_env, .cache = &cache, .arena = arena };

    const tags = try arena.alloc(TagTy, 257);
    for (tags, 0..) |*tag, index| {
        const payloads: []const Ty = switch (index % 3) {
            0 => &.{},
            1 => &.{sc(.u32)},
            else => &.{ .str, sc(.u8) },
        };
        tag.* = .{ .name = try std.fmt.allocPrint(arena, "T{d}", .{index}), .payloads = payloads };
    }
    const ty: Ty = .{ .tag_union = tags };
    const d = try builder.desc(ty);
    try testing.expectEqual(@as(u32, 2), d.discriminant_size);
    try builder.expectSame(d, try builder.layoutOf(ty));
}
