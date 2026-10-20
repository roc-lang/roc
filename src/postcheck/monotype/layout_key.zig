//! Layout representatives of Monotype types (design.md "Specialization By
//! Demand").
//!
//! A specialization that demands only the layout of a quantified variable is
//! keyed by a representative of the type substituted for it: a Monotype type
//! built from structure alone, so types that differ only in what layout
//! selection ignores share one representative, and therefore one
//! specialization. A record and a tuple whose components agree position for
//! position commit the same struct layout, and a nominal type commits its
//! backing's layout, so the representative drops field names and nominal
//! wrappers. Everything it keeps feeds layout selection, so two types with one
//! representative commit one layout.

const std = @import("std");
const check = @import("check");

const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const names = check.CheckedNames;
const collections = @import("collections");
const GuardedList = collections.GuardedList;

/// The layout representative of `root`, interned in `store`; null when a
/// layout-only specialization cannot stand for it. That is when the type
/// holds a function (its layout is a lambda set's, which only the program
/// decides), is recursive, or is a nominal type whose layout is not its
/// backing's: one with no runtime backing, an iterator, or a record that
/// declares its own field order.
pub fn layoutRepresentative(
    allocator: Allocator,
    store: *Type.Store,
    name_store: *const names.NameStore,
    root: Type.TypeId,
) Allocator.Error!?Type.TypeId {
    var builder = Builder{
        .allocator = allocator,
        .store = store,
        .name_store = name_store,
        .memo = collections.DenseMap(Type.TypeId, ?Type.TypeId).init(allocator),
        .active = collections.DenseMap(Type.TypeId, void).init(allocator),
    };
    defer builder.deinit();
    return builder.representative(root);
}

/// Whether every value of `root` occupies at least one byte, decided from
/// structure alone. A list or box of zero-sized items has a runtime
/// representation of its own, so a demand hole, whose list stands for any
/// ordinary list, stands only for items known to be sized. False when the
/// structure does not decide it: a function, whose lambda set the program
/// decides, an erased callable, which a box holds as a layout of its own,
/// a recursive type, and a builtin whose representation follows its
/// arguments.
pub fn knownSized(allocator: Allocator, store: *const Type.Store, root: Type.TypeId) Allocator.Error!bool {
    var visiting = collections.DenseMap(Type.TypeId, void).init(allocator);
    defer visiting.deinit();
    var pending: std.ArrayList(Type.TypeId) = .empty;
    defer pending.deinit(allocator);
    // A type is sized when some one of its components is; walk components
    // until one decides it.
    try pending.append(allocator, root);
    while (pending.pop()) |ty| {
        if ((try visiting.getOrPut(ty)).found_existing) continue;
        switch (store.get(ty)) {
            .primitive, .list, .box => return true,
            // A boxed erased callable is a layout of its own, not a box.
            .erased, .func, .zst, .hole => {},
            .record => |fields| {
                const span = store.fieldSpan(fields);
                for (0..span.len) |index| try pending.append(allocator, GuardedList.at(span, index).ty);
            },
            .tuple => |items| {
                const span = store.span(items);
                for (0..span.len) |index| try pending.append(allocator, GuardedList.at(span, index));
            },
            .tag_union => |tags| {
                const tag_span = store.tagSpan(tags);
                // More than one tag needs a discriminant.
                if (tag_span.len > 1) return true;
                for (0..tag_span.len) |tag_index| {
                    const payloads = store.span(GuardedList.at(tag_span, tag_index).payloads);
                    for (0..payloads.len) |index| try pending.append(allocator, GuardedList.at(payloads, index));
                }
            },
            .named => |named| switch (Builder.namedLayout(named)) {
                .itself => return true,
                .none => {},
                .backing => |backing| try pending.append(allocator, backing),
            },
        }
    }
    return false;
}

/// Whether `root` holds a function anywhere, a boxed erased callable included.
/// Lambda solving relates the members of every callable a type holds, so a
/// demand-shaped signature, whose call relates nothing, must hold none.
pub fn holdsFunction(allocator: Allocator, store: *const Type.Store, root: Type.TypeId) Allocator.Error!bool {
    var visited = collections.DenseMap(Type.TypeId, void).init(allocator);
    defer visited.deinit();
    var pending: std.ArrayList(Type.TypeId) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, root);
    while (pending.pop()) |ty| {
        if ((try visited.getOrPut(ty)).found_existing) continue;
        switch (store.get(ty)) {
            .func, .erased => return true,
            .primitive, .zst, .hole => {},
            .list, .box => |elem| try pending.append(allocator, elem),
            .record => |fields| {
                const span = store.fieldSpan(fields);
                for (0..span.len) |index| {
                    const field = GuardedList.at(span, index);
                    try pending.append(allocator, field.ty);
                    if (field.value_ty) |value_ty| try pending.append(allocator, value_ty);
                }
            },
            .tuple => |items| {
                const span = store.span(items);
                for (0..span.len) |index| try pending.append(allocator, GuardedList.at(span, index));
            },
            .tag_union => |tags| {
                const tag_span = store.tagSpan(tags);
                for (0..tag_span.len) |tag_index| {
                    const payloads = store.span(GuardedList.at(tag_span, tag_index).payloads);
                    for (0..payloads.len) |index| try pending.append(allocator, GuardedList.at(payloads, index));
                }
            },
            .named => |named| {
                const args = store.span(named.args);
                for (0..args.len) |index| try pending.append(allocator, GuardedList.at(args, index));
                if (named.backing) |backing| try pending.append(allocator, backing.ty);
                const declared = store.declaredFieldSpan(named.declared_order);
                for (0..declared.len) |index| switch (GuardedList.at(declared, index)) {
                    .named => {},
                    .padding => |padding| try pending.append(allocator, padding),
                };
            },
        }
    }
    return false;
}

const Builder = struct {
    allocator: Allocator,
    store: *Type.Store,
    name_store: *const names.NameStore,
    /// Each finished type's representative; null when it has none.
    memo: collections.DenseMap(Type.TypeId, ?Type.TypeId),
    /// Types whose components are being represented. Reaching one again is a
    /// cycle.
    active: collections.DenseMap(Type.TypeId, void),
    frames: std.ArrayList(Frame) = .empty,
    scratch: std.ArrayList(Type.TypeId) = .empty,
    tag_scratch: std.ArrayList(Type.Store.TagInput) = .empty,

    const Frame = struct {
        ty: Type.TypeId,
        expanded: bool = false,
    };

    fn deinit(self: *Builder) void {
        self.memo.deinit();
        self.active.deinit();
        self.frames.deinit(self.allocator);
        self.scratch.deinit(self.allocator);
        self.tag_scratch.deinit(self.allocator);
    }

    fn representative(self: *Builder, root: Type.TypeId) Allocator.Error!?Type.TypeId {
        try self.frames.append(self.allocator, .{ .ty = root });
        while (self.frames.items.len != 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            const ty = frame.ty;
            if (self.memo.contains(ty)) {
                _ = self.frames.pop();
                continue;
            }
            if (!frame.expanded) {
                if (self.active.contains(ty)) return null;
                frame.expanded = true;
                try self.active.put(ty, {});
                const before = self.frames.items.len;
                try self.pushComponents(ty);
                if (self.frames.items.len != before) continue;
            }
            _ = self.active.remove(ty);
            _ = self.frames.pop();
            const result = try self.finish(ty);
            try self.memo.put(ty, result);
        }
        return self.memo.get(root).?;
    }

    fn push(self: *Builder, ty: Type.TypeId) Allocator.Error!void {
        if (self.memo.contains(ty)) return;
        try self.frames.append(self.allocator, .{ .ty = ty });
    }

    fn pushComponents(self: *Builder, ty: Type.TypeId) Allocator.Error!void {
        switch (self.store.get(ty)) {
            .primitive, .zst, .hole, .erased, .func => {},
            .list, .box => |elem| try self.push(elem),
            .record => |fields| {
                const span = self.store.fieldSpan(fields);
                for (0..span.len) |index| try self.push(GuardedList.at(span, index).ty);
            },
            .tuple => |items| {
                const span = self.store.span(items);
                for (0..span.len) |index| try self.push(GuardedList.at(span, index));
            },
            .tag_union => |tags| {
                const tag_span = self.store.tagSpan(tags);
                for (0..tag_span.len) |tag_index| {
                    const payloads = self.store.span(GuardedList.at(tag_span, tag_index).payloads);
                    for (0..payloads.len) |index| try self.push(GuardedList.at(payloads, index));
                }
            },
            .named => |named| switch (namedLayout(named)) {
                .itself, .none => {},
                .backing => |backing| try self.push(backing),
            },
        }
    }

    /// How a named type's layout relates to its representative.
    const NamedLayout = union(enum) {
        /// A builtin whose layout is fixed: it is its own representative.
        itself,
        /// Its layout is its backing's.
        backing: Type.TypeId,
        /// No representative stands for it.
        none,
    };

    fn namedLayout(named: Type.NamedContent) NamedLayout {
        if (named.builtin_owner) |owner| switch (owner) {
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => return .itself,
            // These representations follow their arguments' structure.
            .iter, .stream, .parse_tag_union_spec, .fields, .field => return .none,
            .list, .box, .dict, .set, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => {},
        };
        if (named.declared_order.len != 0) return .none;
        const backing = named.backing orelse return .none;
        return .{ .backing = backing.ty };
    }

    fn finish(self: *Builder, ty: Type.TypeId) Allocator.Error!?Type.TypeId {
        switch (self.store.get(ty)) {
            .primitive, .zst, .hole => return ty,
            .erased, .func => return null,
            .list => |elem| {
                const rep = self.memo.get(elem).? orelse return null;
                return try self.store.internList(self.name_store, rep);
            },
            .box => |elem| {
                const rep = self.memo.get(elem).? orelse return null;
                return try self.store.internBox(self.name_store, rep);
            },
            .record => |fields| {
                self.scratch.clearRetainingCapacity();
                const span = self.store.fieldSpan(fields);
                for (0..span.len) |index| {
                    const rep = self.memo.get(GuardedList.at(span, index).ty).? orelse return null;
                    try self.scratch.append(self.allocator, rep);
                }
                return try self.store.internTuple(self.name_store, self.scratch.items);
            },
            .tuple => |items| {
                self.scratch.clearRetainingCapacity();
                const span = self.store.span(items);
                for (0..span.len) |index| {
                    const rep = self.memo.get(GuardedList.at(span, index)).? orelse return null;
                    try self.scratch.append(self.allocator, rep);
                }
                return try self.store.internTuple(self.name_store, self.scratch.items);
            },
            .tag_union => |tags| {
                // Payload representatives in one shared buffer; each tag's
                // slice is cut from it once every payload is known.
                self.scratch.clearRetainingCapacity();
                self.tag_scratch.clearRetainingCapacity();
                const tag_span = self.store.tagSpan(tags);
                for (0..tag_span.len) |tag_index| {
                    const tag = GuardedList.at(tag_span, tag_index);
                    const payloads = self.store.span(tag.payloads);
                    for (0..payloads.len) |index| {
                        const rep = self.memo.get(GuardedList.at(payloads, index)).? orelse return null;
                        try self.scratch.append(self.allocator, rep);
                    }
                    try self.tag_scratch.append(self.allocator, .{ .name = tag.name, .checked_name = tag.name, .payloads = &.{} });
                }
                var start: usize = 0;
                for (self.tag_scratch.items, 0..) |*input, tag_index| {
                    const count = self.store.span(GuardedList.at(tag_span, tag_index).payloads).len;
                    input.payloads = self.scratch.items[start .. start + count];
                    start += count;
                }
                return try self.store.internTagUnion(self.name_store, self.tag_scratch.items);
            },
            .named => |named| return switch (namedLayout(named)) {
                .itself => ty,
                .none => null,
                .backing => |backing| self.memo.get(backing).?,
            },
        }
    }
};

const testing = std.testing;

test "records, tuples, and nominal wrappers with one layout share a representative" {
    var name_store = names.NameStore.init(testing.allocator);
    defer name_store.deinit();
    var store = Type.Store.init(testing.allocator);
    defer store.deinit();

    const x_field = try name_store.internRecordFieldLabel("x");
    const y_field = try name_store.internRecordFieldLabel("y");
    const a_field = try name_store.internRecordFieldLabel("a");
    const b_field = try name_store.internRecordFieldLabel("b");
    const u64_ty = try store.internPrimitive(&name_store, .u64);
    const u8_ty = try store.internPrimitive(&name_store, .u8);
    const point = try store.internRecord(&name_store, &.{
        .{ .name = x_field, .ty = u64_ty, .default = null },
        .{ .name = y_field, .ty = u8_ty, .default = null },
    });
    const other = try store.internRecord(&name_store, &.{
        .{ .name = a_field, .ty = u64_ty, .default = null },
        .{ .name = b_field, .ty = u8_ty, .default = null },
    });
    const pair = try store.internTuple(&name_store, &.{ u64_ty, u8_ty });
    const flipped = try store.internTuple(&name_store, &.{ u8_ty, u64_ty });

    const point_rep = (try layoutRepresentative(testing.allocator, &store, &name_store, point)).?;
    try testing.expectEqual(point_rep, (try layoutRepresentative(testing.allocator, &store, &name_store, other)).?);
    try testing.expectEqual(point_rep, (try layoutRepresentative(testing.allocator, &store, &name_store, pair)).?);
    // Layout selection keeps each field's position, so positions that differ
    // are different layouts.
    try testing.expect(point_rep != (try layoutRepresentative(testing.allocator, &store, &name_store, flipped)).?);
    // A representative is its own representative.
    try testing.expectEqual(point_rep, (try layoutRepresentative(testing.allocator, &store, &name_store, point_rep)).?);

    const points = try store.internList(&name_store, point);
    const pairs = try store.internList(&name_store, pair);
    try testing.expectEqual(
        (try layoutRepresentative(testing.allocator, &store, &name_store, points)).?,
        (try layoutRepresentative(testing.allocator, &store, &name_store, pairs)).?,
    );
    // Scalars of one width but different kinds keep different layouts.
    const i8_ty = try store.internPrimitive(&name_store, .i8);
    try testing.expect((try layoutRepresentative(testing.allocator, &store, &name_store, u8_ty)).? !=
        (try layoutRepresentative(testing.allocator, &store, &name_store, i8_ty)).?);
}

test "a type holding a function has no layout representative" {
    var name_store = names.NameStore.init(testing.allocator);
    defer name_store.deinit();
    var store = Type.Store.init(testing.allocator);
    defer store.deinit();

    const u64_ty = try store.internPrimitive(&name_store, .u64);
    const func = try store.internFunc(&name_store, &.{u64_ty}, u64_ty);
    const funcs = try store.internList(&name_store, func);
    const holder = try store.internTuple(&name_store, &.{ u64_ty, funcs });
    try testing.expectEqual(@as(?Type.TypeId, null), try layoutRepresentative(testing.allocator, &store, &name_store, func));
    try testing.expectEqual(@as(?Type.TypeId, null), try layoutRepresentative(testing.allocator, &store, &name_store, holder));
}

test "only structurally sized types are known sized" {
    var name_store = names.NameStore.init(testing.allocator);
    defer name_store.deinit();
    var store = Type.Store.init(testing.allocator);
    defer store.deinit();

    const a_tag = try name_store.internTagLabel("A");
    const b_tag = try name_store.internTagLabel("B");
    const unit = try store.internZst(&name_store);
    const u8_ty = try store.internPrimitive(&name_store, .u8);
    const units = try store.internTuple(&name_store, &.{ unit, unit });
    const mixed = try store.internTuple(&name_store, &.{ unit, u8_ty });
    const one_empty_tag = try store.internTagUnion(&name_store, &.{.{ .name = a_tag, .checked_name = a_tag, .payloads = &.{} }});
    const two_empty_tags = try store.internTagUnion(&name_store, &.{
        .{ .name = a_tag, .checked_name = a_tag, .payloads = &.{} },
        .{ .name = b_tag, .checked_name = b_tag, .payloads = &.{} },
    });
    const func = try store.internFunc(&name_store, &.{u8_ty}, u8_ty);
    const empty_list = try store.internList(&name_store, unit);

    try testing.expect(try knownSized(testing.allocator, &store, u8_ty));
    try testing.expect(try knownSized(testing.allocator, &store, mixed));
    try testing.expect(try knownSized(testing.allocator, &store, two_empty_tags));
    try testing.expect(try knownSized(testing.allocator, &store, empty_list));
    try testing.expect(!try knownSized(testing.allocator, &store, unit));
    try testing.expect(!try knownSized(testing.allocator, &store, units));
    try testing.expect(!try knownSized(testing.allocator, &store, one_empty_tag));
    try testing.expect(!try knownSized(testing.allocator, &store, func));
}
