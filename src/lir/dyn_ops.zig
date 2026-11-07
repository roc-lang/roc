//! Operand and ownership facts of `CFStmt.dyn` operations (`LIR.DynOp`),
//! shared by every LIR pass so each reads one definition of what a dynamic
//! operation uses and how it treats ownership.

const std = @import("std");
const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const LocalId = LIR.LocalId;
const LocalSpan = LIR.LocalSpan;
const DynOp = LIR.DynOp;

/// How an operation treats the ownership of its target and value operands
/// (see `LIR.DynOp`).
pub const Ownership = union(enum) {
    /// No reference-counted value is read or produced: `frame_alloc`.
    none,
    /// The target borrows from `source` (a component address or a load of a
    /// component's bytes).
    borrow_component: LocalId,
    /// The target is an alias of `source`'s value.
    alias: LocalId,
    /// The value operands are consumed into the owned target.
    construct,
    /// The value operands are borrowed; the target is owned (or not reference
    /// counted).
    borrow_operands,
    /// `call_erased`: the closure is borrowed, or consumed by an owned call
    /// (`reuse_closure`), the argument record consumed, and the result
    /// owned.
    call_erased,
};

/// How `op` treats the ownership of its operands and target.
pub fn ownership(op: DynOp) Ownership {
    return switch (op) {
        .frame_alloc => .none,
        // A table's words are static data, which the target holds as a
        // static-data literal read holds its value: a fresh owned unit of
        // image data. The table itself holds no unit.
        .table_entry => .borrow_operands,
        .copy => |copy| .{ .alias = copy.src },
        .field_addr => |field| .{ .borrow_component = field.base },
        .list_item_addr => |elem| .{ .borrow_component = elem.list },
        .box_payload_addr => |payload| .{ .borrow_component = payload.box },
        .tag_payload_addr => |payload| .{ .borrow_component = payload.base },
        .view => |view| .{ .borrow_component = view.owner orelse view.address },
        .tag_discriminant, .eq, .hash, .inspect, .adapt => .borrow_operands,
        .record, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .closure => .construct,
        .call_erased => .call_erased,
    };
}

/// The local operands an operation reads, in a fixed order: single operands
/// first, then the locals of each span. Descriptor and plan operands are
/// included; they are ordinary `opaque_ptr` locals, and an index operand a
/// local holds (`LIR.DynIndex`) is an ordinary `u32` local.
pub const Operands = struct {
    singles: [4]LocalId = undefined,
    single_count: u8 = 0,
    spans: [3]LocalSpan = undefined,
    span_count: u8 = 0,

    fn single(self: *Operands, local: LocalId) void {
        self.singles[self.single_count] = local;
        self.single_count += 1;
    }

    fn dynIndex(self: *Operands, value: LIR.DynIndex) void {
        switch (value) {
            .immediate => {},
            .local => |local| self.single(local),
        }
    }

    fn span(self: *Operands, value: LocalSpan) void {
        self.spans[self.span_count] = value;
        self.span_count += 1;
    }

    pub fn of(op: DynOp) Operands {
        var result: Operands = .{};
        switch (op) {
            .frame_alloc => {},
            .table_entry => |entry| result.single(entry.table),
            .copy => |copy| {
                result.single(copy.src);
                result.single(copy.desc);
            },
            .field_addr => |field| {
                result.single(field.base);
                result.single(field.desc);
                result.dynIndex(field.field_index);
            },
            .list_item_addr => |elem| {
                result.single(elem.list);
                result.single(elem.elem_desc);
                result.single(elem.index);
            },
            .box_payload_addr => |payload| {
                result.single(payload.box);
                result.single(payload.desc);
            },
            .tag_discriminant => |tag| {
                result.single(tag.base);
                result.single(tag.desc);
            },
            .tag_payload_addr => |payload| {
                result.single(payload.base);
                result.single(payload.desc);
            },
            .record => |record| {
                result.single(record.desc);
                result.span(record.fields);
            },
            .slot_alloc => |slot| result.single(slot.desc),
            .slot_store => |store_| {
                result.single(store_.slot);
                result.single(store_.value);
                result.single(store_.desc);
            },
            .record_update => |update| {
                result.single(update.base);
                result.single(update.base_desc);
                result.single(update.desc);
                result.single(update.plan);
                result.span(update.values);
            },
            .tag => |tag| {
                result.single(tag.desc);
                result.dynIndex(tag.discriminant);
                result.span(tag.payloads);
            },
            .list => |list| {
                result.single(list.elem_desc);
                result.span(list.elems);
            },
            .box => |box| {
                result.single(box.desc);
                result.single(box.payload);
            },
            .eq => |eq| {
                result.single(eq.lhs);
                result.single(eq.rhs);
                result.single(eq.desc);
            },
            .hash => |hash| {
                result.single(hash.hasher);
                result.single(hash.value);
                result.single(hash.desc);
            },
            .inspect => |inspect| {
                result.single(inspect.value);
                result.single(inspect.desc);
                result.single(inspect.names);
            },
            .call_erased => |call| {
                result.single(call.closure);
                if (call.args) |args| result.single(args);
            },
            .closure => |closure| {
                result.single(closure.capture);
                if (closure.reuse) |reuse| result.single(reuse);
            },
            .view => |view| {
                result.single(view.address);
                if (view.owner) |owner| result.single(owner);
            },
            .adapt => |adapt| {
                result.single(adapt.src);
                result.single(adapt.plan);
            },
        }
        return result;
    }

    /// Every operand local, appended to `out` in operand order.
    pub fn appendAll(self: Operands, store: *const LirStore, allocator: std.mem.Allocator, out: *std.ArrayList(LocalId)) std.mem.Allocator.Error!void {
        try out.appendSlice(allocator, self.singles[0..self.single_count]);
        for (self.spans[0..self.span_count]) |value| {
            const locals = store.getLocalSpan(value);
            for (0..locals.len) |index| try out.append(allocator, LirStore.GuardedList.at(locals, index));
        }
    }

    /// Call `visit(context, local)` for every operand local in operand order.
    pub fn each(self: Operands, store: *const LirStore, context: anytype, comptime visit: anytype) @typeInfo(@TypeOf(visit)).@"fn".return_type.? {
        for (self.singles[0..self.single_count]) |local| try visit(context, local);
        for (self.spans[0..self.span_count]) |value| {
            const locals = store.getLocalSpan(value);
            for (0..locals.len) |index| try visit(context, LirStore.GuardedList.at(locals, index));
        }
    }
};

/// The value operands a `construct` operation consumes, in order.
pub fn constructedValues(store: *const LirStore, allocator: std.mem.Allocator, op: DynOp, out: *std.ArrayList(LocalId)) std.mem.Allocator.Error!void {
    switch (op) {
        .record => |record| try appendSpan(store, allocator, record.fields, out),
        .record_update => |update| {
            try out.append(allocator, update.base);
            try appendSpan(store, allocator, update.values, out);
        },
        .tag => |tag| try appendSpan(store, allocator, tag.payloads, out),
        .list => |list| try appendSpan(store, allocator, list.elems, out),
        .box => |box| try out.append(allocator, box.payload),
        .slot_alloc => {},
        .slot_store => |slot| try out.appendSlice(allocator, &.{ slot.slot, slot.value }),
        .closure => |closure| {
            try out.append(allocator, closure.capture);
            if (closure.reuse) |reuse| try out.append(allocator, reuse);
        },
        .frame_alloc, .table_entry, .copy, .field_addr, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_payload_addr, .eq, .hash, .inspect, .call_erased, .view, .adapt => unreachable, // only construct operations consume value operands
    }
}

fn appendSpan(store: *const LirStore, allocator: std.mem.Allocator, span: LocalSpan, out: *std.ArrayList(LocalId)) std.mem.Allocator.Error!void {
    const locals = store.getLocalSpan(span);
    for (0..locals.len) |index| try out.append(allocator, LirStore.GuardedList.at(locals, index));
}

/// The errors `remapOperands` returns: those of `map`, and allocation's.
fn RemapError(comptime map: anytype) type {
    const Return = @typeInfo(@TypeOf(map)).@"fn".return_type.?;
    return @typeInfo(Return).error_union.error_set || std.mem.Allocator.Error;
}

/// Replace every operand local of `op` through `map` (which may fail),
/// storing new spans in `store`.
pub fn remapOperands(store: *LirStore, op: DynOp, context: anytype, comptime map: anytype) RemapError(map)!DynOp {
    const Ctx = @TypeOf(context);
    const Error = RemapError(map);
    const Remap = struct {
        fn one(ctx: Ctx, local: LocalId) Error!LocalId {
            return try map(ctx, local);
        }
        fn opt(ctx: Ctx, local: ?LocalId) Error!?LocalId {
            return if (local) |value| try map(ctx, value) else null;
        }
        fn dynIndex(ctx: Ctx, value: LIR.DynIndex) Error!LIR.DynIndex {
            return switch (value) {
                .immediate => value,
                .local => |local| .{ .local = try map(ctx, local) },
            };
        }
        fn span(s: *LirStore, ctx: Ctx, value: LocalSpan) Error!LocalSpan {
            if (value.len == 0) return value;
            var items: std.ArrayList(LocalId) = .empty;
            defer items.deinit(s.allocator);
            const locals = s.getLocalSpan(value);
            try items.ensureTotalCapacity(s.allocator, locals.len);
            for (0..locals.len) |index| items.appendAssumeCapacity(LirStore.GuardedList.at(locals, index));
            for (items.items) |*item| item.* = try map(ctx, item.*);
            return try s.addLocalSpan(items.items);
        }
    };
    return switch (op) {
        .frame_alloc => op,
        .table_entry => |entry| .{ .table_entry = .{ .table = try Remap.one(context, entry.table), .index = entry.index } },
        .copy => |copy| .{ .copy = .{ .src = try Remap.one(context, copy.src), .desc = try Remap.one(context, copy.desc) } },
        .field_addr => |field| .{ .field_addr = .{ .base = try Remap.one(context, field.base), .desc = try Remap.one(context, field.desc), .field_index = try Remap.dynIndex(context, field.field_index) } },
        .list_item_addr => |elem| .{ .list_item_addr = .{ .list = try Remap.one(context, elem.list), .elem_desc = try Remap.one(context, elem.elem_desc), .index = try Remap.one(context, elem.index) } },
        .box_payload_addr => |payload| .{ .box_payload_addr = .{ .box = try Remap.one(context, payload.box), .desc = try Remap.one(context, payload.desc) } },
        .tag_discriminant => |tag| .{ .tag_discriminant = .{ .base = try Remap.one(context, tag.base), .desc = try Remap.one(context, tag.desc) } },
        .tag_payload_addr => |payload| .{ .tag_payload_addr = .{
            .base = try Remap.one(context, payload.base),
            .desc = try Remap.one(context, payload.desc),
            .payload_index = payload.payload_index,
        } },
        .record => |record| .{ .record = .{ .desc = try Remap.one(context, record.desc), .fields = try Remap.span(store, context, record.fields) } },
        .slot_alloc => |slot| .{ .slot_alloc = .{ .desc = try Remap.one(context, slot.desc) } },
        .slot_store => |slot| .{ .slot_store = .{ .slot = try Remap.one(context, slot.slot), .value = try Remap.one(context, slot.value), .desc = try Remap.one(context, slot.desc) } },
        .record_update => |update| .{ .record_update = .{
            .base = try Remap.one(context, update.base),
            .base_desc = try Remap.one(context, update.base_desc),
            .desc = try Remap.one(context, update.desc),
            .plan = try Remap.one(context, update.plan),
            .values = try Remap.span(store, context, update.values),
        } },
        .tag => |tag| .{ .tag = .{ .desc = try Remap.one(context, tag.desc), .discriminant = try Remap.dynIndex(context, tag.discriminant), .payloads = try Remap.span(store, context, tag.payloads) } },
        .list => |list| .{ .list = .{ .elem_desc = try Remap.one(context, list.elem_desc), .elems = try Remap.span(store, context, list.elems) } },
        .box => |box| .{ .box = .{ .desc = try Remap.one(context, box.desc), .payload = try Remap.one(context, box.payload) } },
        .eq => |eq| .{ .eq = .{ .lhs = try Remap.one(context, eq.lhs), .rhs = try Remap.one(context, eq.rhs), .desc = try Remap.one(context, eq.desc) } },
        .hash => |hash| .{ .hash = .{ .hasher = try Remap.one(context, hash.hasher), .value = try Remap.one(context, hash.value), .desc = try Remap.one(context, hash.desc) } },
        .inspect => |inspect| .{ .inspect = .{ .value = try Remap.one(context, inspect.value), .desc = try Remap.one(context, inspect.desc), .names = try Remap.one(context, inspect.names) } },
        .call_erased => |call| .{ .call_erased = .{
            .closure = try Remap.one(context, call.closure),
            .args = try Remap.opt(context, call.args),
            .reuse_closure = call.reuse_closure,
            .reuse_source = try Remap.opt(context, call.reuse_source),
            .deferred = call.deferred,
            .drive = call.drive,
        } },
        .closure => |closure| .{ .closure = .{ .proc = closure.proc, .capture = try Remap.one(context, closure.capture), .reuse = try Remap.opt(context, closure.reuse), .reuse_unique = closure.reuse_unique } },
        .view => |view| .{ .view = .{ .address = try Remap.one(context, view.address), .owner = try Remap.opt(context, view.owner) } },
        .adapt => |adapt| .{ .adapt = .{ .src = try Remap.one(context, adapt.src), .plan = try Remap.one(context, adapt.plan) } },
    };
}

test "every operand of a record construction is visited in order" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    const desc: LocalId = @fromBackingInt(1);
    const a: LocalId = @fromBackingInt(2);
    const b: LocalId = @fromBackingInt(3);
    const op: DynOp = .{ .record = .{ .desc = desc, .fields = try store.addLocalSpan(&.{ a, b }) } };
    var out: std.ArrayList(LocalId) = .empty;
    defer out.deinit(std.testing.allocator);
    try Operands.of(op).appendAll(&store, std.testing.allocator, &out);
    try std.testing.expectEqualSlices(LocalId, &.{ desc, a, b }, out.items);
    try std.testing.expectEqual(Ownership.construct, ownership(op));
}
