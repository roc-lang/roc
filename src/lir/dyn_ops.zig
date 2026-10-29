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
    /// No reference-counted value is read or produced: descriptor, name, and
    /// dictionary composition, `frame_alloc`, and `tag_discriminant_of`.
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
    /// `call_erased`: the closure is borrowed, the argument record consumed,
    /// and the result owned.
    call_erased,
};

pub fn ownership(op: DynOp) Ownership {
    return switch (op) {
        .frame_alloc,
        .desc_list,
        .desc_box,
        .desc_record,
        .desc_tuple,
        .desc_presence_slot,
        .desc_tag_union,
        .desc_args,
        .desc_nominal,
        .desc_recursive_box,
        .desc_recursive_open,
        .desc_recursive_close,
        .tag_discriminant_of,
        .desc_index,
        => .none,
        // A dictionary is an immortal erased callable built from its
        // captures, which it holds as an aggregate holds its fields.
        .dict_compose => .construct,
        // A dictionary record copies the words of its callables, which are
        // immortal, into an immortal record of its own.
        .dict_record => .borrow_operands,
        // A dictionary's callables are immortal; the target borrows from it.
        .dict_method => |method| .{ .borrow_component = method.dict },
        .copy => |copy| .{ .alias = copy.src },
        .field_addr => |field| .{ .borrow_component = field.base },
        .field_addr_named => |field| .{ .borrow_component = field.base },
        .list_item_addr => |elem| .{ .borrow_component = elem.list },
        .box_payload_addr => |payload| .{ .borrow_component = payload.box },
        .tag_payload_addr => |payload| .{ .borrow_component = payload.base },
        .view => |view| .{ .borrow_component = view.address },
        .tag_discriminant, .eq, .hash, .inspect, .numeral, .adapt => .borrow_operands,
        .record, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .closure => .construct,
        .call_erased => .call_erased,
    };
}

/// The local operands an operation reads, in a fixed order: single operands
/// first, then the locals of each span. Descriptor and name operands are
/// included; they are ordinary `opaque_ptr` locals.
pub const Operands = struct {
    singles: [4]LocalId = undefined,
    single_count: u8 = 0,
    spans: [3]LocalSpan = undefined,
    span_count: u8 = 0,

    fn single(self: *Operands, local: LocalId) void {
        self.singles[self.single_count] = local;
        self.single_count += 1;
    }

    fn span(self: *Operands, value: LocalSpan) void {
        self.spans[self.span_count] = value;
        self.span_count += 1;
    }

    pub fn of(op: DynOp) Operands {
        var result: Operands = .{};
        switch (op) {
            .frame_alloc => {},
            .desc_list => |desc| result.single(desc.elem),
            .desc_box => |desc| result.single(desc.payload),
            .desc_record => |desc| {
                if (desc.ext) |ext| result.single(ext);
                result.span(desc.names);
                result.span(desc.descs);
            },
            .desc_tuple => |desc| result.span(desc.elems),
            .desc_presence_slot => |desc| {
                result.single(desc.missing);
                result.single(desc.present);
                result.single(desc.payload);
            },
            .desc_tag_union => |desc| {
                if (desc.ext) |ext| result.single(ext);
                result.span(desc.names);
                result.span(desc.payloads);
            },
            .desc_args => |desc| result.span(desc.elems),
            .desc_nominal => |desc| {
                result.single(desc.backing);
                result.single(desc.name);
                if (desc.methods) |methods| result.single(methods);
            },
            .desc_recursive_box => |desc| result.single(desc.payload),
            .desc_recursive_open => |desc| {
                result.single(desc.key);
                result.span(desc.vars);
            },
            .desc_recursive_close => |desc| {
                result.single(desc.shell);
                result.single(desc.desc);
                result.single(desc.key);
                result.span(desc.vars);
            },
            .dict_compose => |dict| result.span(dict.captures),
            .dict_record => |dict| result.span(dict.methods),
            .dict_method => |method| result.single(method.dict),
            .copy => |copy| {
                result.single(copy.src);
                result.single(copy.desc);
            },
            .field_addr => |field| {
                result.single(field.base);
                result.single(field.desc);
            },
            .field_addr_named => |field| {
                result.single(field.base);
                result.single(field.desc);
                result.single(field.name);
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
            .tag_discriminant_of => |tag| {
                result.single(tag.desc);
                result.single(tag.name);
            },
            .desc_index => |index| {
                result.single(index.desc);
                result.single(index.candidates);
            },
            .tag_payload_addr => |payload| {
                result.single(payload.base);
                result.single(payload.desc);
                result.single(payload.name);
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
                result.span(update.names);
                result.span(update.values);
            },
            .tag => |tag| {
                result.single(tag.desc);
                result.single(tag.name);
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
            },
            .call_erased => |call| {
                result.single(call.closure);
                if (call.args) |args| result.single(args);
            },
            .closure => |closure| result.single(closure.capture),
            .view => |view| result.single(view.address),
            .numeral => |numeral| {
                result.single(numeral.desc);
                result.single(numeral.table);
            },
            .adapt => |adapt| {
                result.single(adapt.src);
                result.single(adapt.from);
                result.single(adapt.to);
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
        .dict_compose => |dict| try appendSpan(store, allocator, dict.captures, out),
        .closure => |closure| try out.append(allocator, closure.capture),
        .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_tag_union, .desc_args, .desc_nominal, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .dict_record, .dict_method, .copy, .field_addr, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_discriminant_of, .desc_index, .tag_payload_addr, .eq, .hash, .inspect, .call_erased, .view, .numeral, .adapt => unreachable, // only construct operations consume value operands
    }
}

/// The value operands a `borrow_operands` operation reads, in order.
pub fn borrowedValues(store: *const LirStore, allocator: std.mem.Allocator, op: DynOp, out: *std.ArrayList(LocalId)) std.mem.Allocator.Error!void {
    switch (op) {
        .tag_discriminant => |tag| try out.append(allocator, tag.base),
        .eq => |eq| try out.appendSlice(allocator, &.{ eq.lhs, eq.rhs }),
        .hash => |hash| try out.appendSlice(allocator, &.{ hash.hasher, hash.value }),
        .inspect => |inspect| try out.append(allocator, inspect.value),
        .numeral => {},
        .adapt => |adapt| try out.append(allocator, adapt.src),
        .dict_record => |dict| try appendSpan(store, allocator, dict.methods, out),
        .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_tag_union, .desc_args, .desc_nominal, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .dict_compose, .dict_method, .copy, .field_addr, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant_of, .desc_index, .tag_payload_addr, .record, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .call_erased, .closure, .view => unreachable, // only borrow_operands operations borrow value operands
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
        .desc_list => |desc| .{ .desc_list = .{ .elem = try Remap.one(context, desc.elem) } },
        .desc_box => |desc| .{ .desc_box = .{ .payload = try Remap.one(context, desc.payload) } },
        .desc_record => |desc| .{ .desc_record = .{
            .names = try Remap.span(store, context, desc.names),
            .descs = try Remap.span(store, context, desc.descs),
            .ext = try Remap.opt(context, desc.ext),
        } },
        .desc_tuple => |desc| .{ .desc_tuple = .{ .elems = try Remap.span(store, context, desc.elems) } },
        .desc_presence_slot => |desc| .{ .desc_presence_slot = .{
            .missing = try Remap.one(context, desc.missing),
            .present = try Remap.one(context, desc.present),
            .payload = try Remap.one(context, desc.payload),
        } },
        .desc_tag_union => |desc| .{ .desc_tag_union = .{
            .names = try Remap.span(store, context, desc.names),
            .payload_counts = desc.payload_counts,
            .payloads = try Remap.span(store, context, desc.payloads),
            .ext = try Remap.opt(context, desc.ext),
        } },
        .desc_args => |desc| .{ .desc_args = .{ .elems = try Remap.span(store, context, desc.elems) } },
        .desc_nominal => |desc| .{ .desc_nominal = .{
            .backing = try Remap.one(context, desc.backing),
            .name = try Remap.one(context, desc.name),
            .methods = try Remap.opt(context, desc.methods),
            .inspect_opaque = desc.inspect_opaque,
        } },
        .desc_recursive_box => |desc| .{ .desc_recursive_box = .{ .payload = try Remap.one(context, desc.payload) } },
        .desc_recursive_open => |desc| .{ .desc_recursive_open = .{ .key = try Remap.one(context, desc.key), .vars = try Remap.span(store, context, desc.vars) } },
        .desc_recursive_close => |desc| .{ .desc_recursive_close = .{
            .shell = try Remap.one(context, desc.shell),
            .desc = try Remap.one(context, desc.desc),
            .key = try Remap.one(context, desc.key),
            .vars = try Remap.span(store, context, desc.vars),
        } },
        .dict_compose => |dict| .{ .dict_compose = .{ .worker = dict.worker, .captures = try Remap.span(store, context, dict.captures) } },
        .dict_record => |dict| .{ .dict_record = .{ .methods = try Remap.span(store, context, dict.methods) } },
        .dict_method => |method| .{ .dict_method = .{ .dict = try Remap.one(context, method.dict), .index = method.index } },
        .copy => |copy| .{ .copy = .{ .src = try Remap.one(context, copy.src), .desc = try Remap.one(context, copy.desc) } },
        .field_addr => |field| .{ .field_addr = .{ .base = try Remap.one(context, field.base), .desc = try Remap.one(context, field.desc), .field_index = field.field_index } },
        .field_addr_named => |field| .{ .field_addr_named = .{ .base = try Remap.one(context, field.base), .desc = try Remap.one(context, field.desc), .name = try Remap.one(context, field.name) } },
        .list_item_addr => |elem| .{ .list_item_addr = .{ .list = try Remap.one(context, elem.list), .elem_desc = try Remap.one(context, elem.elem_desc), .index = try Remap.one(context, elem.index) } },
        .box_payload_addr => |payload| .{ .box_payload_addr = .{ .box = try Remap.one(context, payload.box), .desc = try Remap.one(context, payload.desc) } },
        .tag_discriminant => |tag| .{ .tag_discriminant = .{ .base = try Remap.one(context, tag.base), .desc = try Remap.one(context, tag.desc) } },
        .tag_discriminant_of => |tag| .{ .tag_discriminant_of = .{ .desc = try Remap.one(context, tag.desc), .name = try Remap.one(context, tag.name) } },
        .desc_index => |index| .{ .desc_index = .{ .desc = try Remap.one(context, index.desc), .candidates = try Remap.one(context, index.candidates) } },
        .tag_payload_addr => |payload| .{ .tag_payload_addr = .{
            .base = try Remap.one(context, payload.base),
            .desc = try Remap.one(context, payload.desc),
            .name = try Remap.one(context, payload.name),
            .payload_index = payload.payload_index,
        } },
        .record => |record| .{ .record = .{ .desc = try Remap.one(context, record.desc), .fields = try Remap.span(store, context, record.fields) } },
        .slot_alloc => |slot| .{ .slot_alloc = .{ .desc = try Remap.one(context, slot.desc) } },
        .slot_store => |slot| .{ .slot_store = .{ .slot = try Remap.one(context, slot.slot), .value = try Remap.one(context, slot.value), .desc = try Remap.one(context, slot.desc) } },
        .record_update => |update| .{ .record_update = .{
            .base = try Remap.one(context, update.base),
            .base_desc = try Remap.one(context, update.base_desc),
            .desc = try Remap.one(context, update.desc),
            .names = try Remap.span(store, context, update.names),
            .values = try Remap.span(store, context, update.values),
        } },
        .tag => |tag| .{ .tag = .{ .desc = try Remap.one(context, tag.desc), .name = try Remap.one(context, tag.name), .payloads = try Remap.span(store, context, tag.payloads) } },
        .list => |list| .{ .list = .{ .elem_desc = try Remap.one(context, list.elem_desc), .elems = try Remap.span(store, context, list.elems) } },
        .box => |box| .{ .box = .{ .desc = try Remap.one(context, box.desc), .payload = try Remap.one(context, box.payload) } },
        .eq => |eq| .{ .eq = .{ .lhs = try Remap.one(context, eq.lhs), .rhs = try Remap.one(context, eq.rhs), .desc = try Remap.one(context, eq.desc) } },
        .hash => |hash| .{ .hash = .{ .hasher = try Remap.one(context, hash.hasher), .value = try Remap.one(context, hash.value), .desc = try Remap.one(context, hash.desc) } },
        .inspect => |inspect| .{ .inspect = .{ .value = try Remap.one(context, inspect.value), .desc = try Remap.one(context, inspect.desc) } },
        .call_erased => |call| .{ .call_erased = .{
            .closure = try Remap.one(context, call.closure),
            .args = try Remap.opt(context, call.args),
            .deferred = call.deferred,
            .drive = call.drive,
        } },
        .closure => |closure| .{ .closure = .{ .proc = closure.proc, .capture = try Remap.one(context, closure.capture) } },
        .view => |view| .{ .view = .{ .address = try Remap.one(context, view.address) } },
        .numeral => |numeral| .{ .numeral = .{ .desc = try Remap.one(context, numeral.desc), .table = try Remap.one(context, numeral.table) } },
        .adapt => |adapt| .{ .adapt = .{ .src = try Remap.one(context, adapt.src), .from = try Remap.one(context, adapt.from), .to = try Remap.one(context, adapt.to) } },
    };
}

test "every operand of a composed record is visited in order" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();
    const a: LocalId = @fromBackingInt(1);
    const b: LocalId = @fromBackingInt(2);
    const c: LocalId = @fromBackingInt(3);
    const d: LocalId = @fromBackingInt(4);
    const op: DynOp = .{ .desc_record = .{
        .names = try store.addLocalSpan(&.{ a, b }),
        .descs = try store.addLocalSpan(&.{ c, d }),
    } };
    var out: std.ArrayList(LocalId) = .empty;
    defer out.deinit(std.testing.allocator);
    try Operands.of(op).appendAll(&store, std.testing.allocator, &out);
    try std.testing.expectEqualSlices(LocalId, &.{ a, b, c, d }, out.items);
    try std.testing.expectEqual(Ownership.none, ownership(op));
}
