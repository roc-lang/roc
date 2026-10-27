//! Descriptor-guided operations on values: copy, incref, decref, equality,
//! hashing, inspection, the shared capture drop, and the element callbacks
//! the builtin list functions take.
//!
//! Each walker reads a value at an address through its descriptor. Equality,
//! hashing, and inspection follow the shared value rules (`value_rules`), so a
//! value compared, hashed, or rendered here means what it means in
//! specialized code. Incref, decref, and inspection keep their pending work on
//! an explicit stack allocated through `RocOps` rather than on the thread's
//! call stack.

const std = @import("std");
const builtins = @import("builtins");
const value_rules = @import("value_rules");
const shape = @import("layout_shape");
const desc_mod = @import("desc.zig");
const compose = @import("compose.zig");

const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocDec = builtins.dec.RocDec;
const utils = builtins.utils;
const hash_mod = builtins.hash;
const erased_callable = builtins.erased_callable;
const structural = value_rules.structural;
const inspect_rules = value_rules.inspect;

const TypeDesc = desc_mod.TypeDesc;
const Name = desc_mod.Name;
const Field = desc_mod.Field;

const invariant = compose.invariant;

/// A value of the type its descriptor describes, at an address.
pub const Value = struct {
    desc: *const TypeDesc,
    ptr: [*]u8,

    fn at(desc: *const TypeDesc, ptr: [*]u8) Value {
        return .{ .desc = desc, .ptr = ptr };
    }

    fn field(self: Value, f: *const Field) Value {
        return .{ .desc = f.desc, .ptr = self.ptr + f.offset };
    }

    fn word(self: Value) ?[*]u8 {
        return @as(*const ?[*]u8, @ptrCast(@alignCast(self.ptr))).*;
    }

    fn rocStr(self: Value) *RocStr {
        return @ptrCast(@alignCast(self.ptr));
    }

    fn rocList(self: Value) *const RocList {
        return @ptrCast(@alignCast(self.ptr));
    }

    /// A box's payload, read through the box pointer with any tag bits in its
    /// low bits cleared.
    fn boxPayload(self: Value) Value {
        const payload = self.desc.child.?;
        const raw: usize = @intFromPtr(self.word());
        const data = raw & ~utils.DATA_PTR_TAG_MASK;
        return .{ .desc = payload, .ptr = if (data == 0) zeroSizedPtr() else @ptrFromInt(data) };
    }

    /// The argument `index` of the variant `variant` of a tag union value.
    fn tagPayload(self: Value, variant: *const desc_mod.Variant, index: u32) Value {
        if (variant.payload_count == 1) return .at(variant.payload, self.ptr);
        return Value.at(variant.payload, self.ptr).field(variant.payload.fieldBySemanticIndex(index));
    }
};

var zero_sized_storage: u8 align(16) = 0;

/// An address for a value that occupies no bytes.
pub fn zeroSizedPtr() [*]u8 {
    return @ptrCast(&zero_sized_storage);
}

/// A last-in, first-out work stack, inline while small and grown through
/// `RocOps` after.
fn Stack(comptime T: type, comptime inline_count: usize) type {
    return struct {
        inline_items: [inline_count]T = undefined,
        heap: ?[*]T = null,
        capacity: usize = inline_count,
        len: usize = 0,

        const Self = @This();

        fn items(self: *Self) [*]T {
            return self.heap orelse &self.inline_items;
        }

        fn push(self: *Self, item: T, ops: *RocOps) void {
            if (self.len == self.capacity) self.grow(ops);
            self.items()[self.len] = item;
            self.len += 1;
        }

        fn pop(self: *Self) ?T {
            if (self.len == 0) return null;
            self.len -= 1;
            return self.items()[self.len];
        }

        /// Reverse the items pushed since the stack held `start` items, so
        /// items pushed in rendering order pop in rendering order.
        fn reverseFrom(self: *Self, start: usize) void {
            std.mem.reverse(T, self.items()[start..self.len]);
        }

        fn grow(self: *Self, ops: *RocOps) void {
            const new_capacity = self.capacity * 2;
            const new_bytes = new_capacity * @sizeOf(T);
            if (self.heap) |heap| {
                self.heap = @ptrCast(@alignCast(ops.realloc(@ptrCast(heap), new_bytes, @alignOf(T))));
            } else {
                const heap: [*]T = @ptrCast(@alignCast(ops.allocRaw(new_bytes, @alignOf(T))));
                @memcpy(heap[0..self.len], self.inline_items[0..self.len]);
                self.heap = heap;
            }
            self.capacity = new_capacity;
        }

        fn deinit(self: *Self, ops: *RocOps) void {
            if (self.heap) |heap| ops.dealloc(@ptrCast(heap), @alignOf(T));
        }
    };
}

// Copy.

/// Copy a value's `desc.size` bytes. Reference counts are untouched.
pub fn copy(desc: *const TypeDesc, dst: [*]u8, src: [*]const u8) void {
    @memmove(dst[0..desc.size], src[0..desc.size]);
}

// Incref.

/// Add `count` to every refcounted allocation the value holds directly: a
/// `Str`'s, a `List`'s, a non-zero-sized box's, or a function's own
/// allocation, through any records, tuples, tag payloads, and nominals that
/// contain them. A list's elements and a box's payload are not visited; they
/// are owned by their container's one allocation.
pub fn incref(desc: *const TypeDesc, ptr: [*]u8, count: isize, ops: *RocOps) void {
    if (!desc.flags.contains_refcounted) return;
    var stack: Stack(Value, 32) = .{};
    defer stack.deinit(ops);
    stack.push(.at(desc, ptr), ops);
    while (stack.pop()) |value| {
        const d = value.desc;
        switch (d.kind) {
            .zst, .scalar, .bool => {},
            .str => value.rocStr().incref(@intCast(count), ops),
            .list => value.rocList().incref(count, d.child.?.flags.contains_refcounted, ops),
            .box, .recursive_box => if (d.child.?.size != 0) utils.increfDataPtr(value.word(), count, .atomic, ops),
            .erased_callable, .opaque_unknown => utils.increfDataPtr(value.word(), count, .atomic, ops),
            .record, .tuple => for (d.fields()) |*f| {
                if (f.is_padding == 0 and f.desc.flags.contains_refcounted) stack.push(value.field(f), ops);
            },
            .tag_union => {
                const variant = &d.variants()[d.readDiscriminant(value.ptr)];
                if (variant.payload.flags.contains_refcounted) stack.push(.at(variant.payload, value.ptr), ops);
            },
            .nominal => stack.push(.at(d.child.?, value.ptr), ops),
        }
    }
}

// Decref.

const DecrefWork = union(enum) {
    value: Value,
    /// Release a unique box whose payload was released before it.
    release_box: struct { alloc: ?[*]u8, alignment: u32 },
    /// Release a unique list whose elements were released before it.
    release_list: struct { list: RocList, alignment: u32 },
};

const DecrefWalker = struct {
    stack: Stack(DecrefWork, 32) = .{},
    ops: *RocOps,
    element_desc: *const TypeDesc,

    fn pushElement(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
        const self: *DecrefWalker = @ptrCast(@alignCast(context.?));
        self.stack.push(.{ .value = .at(self.element_desc, element.?) }, self.ops);
    }
};

/// Release one reference to the value. An allocation whose count reaches zero
/// releases its contents first: a list's elements, a box's payload, or a
/// function's capture (through its `on_drop`).
pub fn decref(desc: *const TypeDesc, ptr: [*]u8, ops: *RocOps) void {
    if (!desc.flags.contains_refcounted) return;
    var walker: DecrefWalker = .{ .ops = ops, .element_desc = desc };
    defer walker.stack.deinit(ops);
    walker.stack.push(.{ .value = .at(desc, ptr) }, ops);
    while (walker.stack.pop()) |work| switch (work) {
        .release_box => |box| utils.decrefDataPtr(box.alloc, box.alignment, true, .atomic, ops),
        .release_list => |list| utils.decref(list.list.getAllocationDataPtr(ops), list.list.capacity_or_alloc_ptr, list.alignment, true, .atomic, ops),
        .value => |value| {
            const d = value.desc;
            switch (d.kind) {
                .zst, .scalar, .bool => {},
                .str => value.rocStr().decref(ops),
                .list => {
                    const elem = d.child.?;
                    const list = value.rocList().*;
                    const alignment: u32 = if (elem.size == 0) 1 else elem.alignment;
                    if (elem.flags.contains_refcounted and list.isUnique(ops)) {
                        walker.stack.push(.{ .release_list = .{ .list = list, .alignment = alignment } }, ops);
                        walker.element_desc = elem;
                        const start = walker.stack.len;
                        list.decrefElements(elem.size, @ptrCast(&walker), &DecrefWalker.pushElement, ops);
                        walker.stack.reverseFrom(start);
                    } else {
                        list.decref(alignment, elem.size, elem.flags.contains_refcounted, null, &builtins.list.rcNone, ops);
                    }
                },
                .box, .recursive_box => {
                    const payload = d.child.?;
                    if (payload.size == 0) continue;
                    const alloc = value.word();
                    if (payload.flags.contains_refcounted and alloc != null and utils.isUnique(alloc, ops)) {
                        walker.stack.push(.{ .release_box = .{ .alloc = alloc, .alignment = payload.alignment } }, ops);
                        walker.stack.push(.{ .value = value.boxPayload() }, ops);
                    } else {
                        utils.decrefDataPtr(alloc, payload.alignment, payload.flags.contains_refcounted, .atomic, ops);
                    }
                },
                .erased_callable => erased_callable.decref(value.word(), ops),
                .opaque_unknown => ops.crash("boxy runtime: a RocBox(RocUnknown) cannot be released through its descriptor, which does not know its payload's alignment"),
                .record, .tuple => {
                    const fields = d.fields();
                    var index = fields.len;
                    while (index > 0) {
                        index -= 1;
                        const f = &fields[index];
                        if (f.is_padding == 0 and f.desc.flags.contains_refcounted) walker.stack.push(.{ .value = value.field(f) }, ops);
                    }
                },
                .tag_union => {
                    const variant = &d.variants()[d.readDiscriminant(value.ptr)];
                    if (variant.payload.flags.contains_refcounted) walker.stack.push(.{ .value = .at(variant.payload, value.ptr) }, ops);
                },
                .nominal => walker.stack.push(.{ .value = .at(d.child.?, value.ptr) }, ops),
            }
        },
    };
}

/// The shared `on_drop` of an erased callable whose capture record is
/// dynamic. The record's first word is its own descriptor; releasing the
/// record through it releases every capture.
pub fn captureDrop(capture: ?[*]u8, ops: *RocOps) callconv(.c) void {
    const record = capture.?;
    const record_desc: *const TypeDesc = @ptrCast(@alignCast(@as(*const ?*const anyopaque, @ptrCast(@alignCast(record))).*.?));
    decref(record_desc, record, ops);
}

// Builtin list element callbacks. The context is the element descriptor; the
// builtins pass no `RocOps`, so these reach the host through
// `in_process_host.ops()`.

pub fn elemIncref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    const elem_desc: *const TypeDesc = @ptrCast(@alignCast(context.?));
    incref(elem_desc, element orelse zeroSizedPtr(), 1, builtins.in_process_host.ops());
}

pub fn elemDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    const elem_desc: *const TypeDesc = @ptrCast(@alignCast(context.?));
    decref(elem_desc, element orelse zeroSizedPtr(), builtins.in_process_host.ops());
}

// Method overrides.

/// The packed argument record of a method call, built in a buffer that is
/// inline when small.
const ArgsBuffer = struct {
    inline_bytes: [256]u8 align(16) = undefined,
    heap: ?[*]u8 = null,

    fn get(self: *ArgsBuffer, size: usize, ops: *RocOps) [*]u8 {
        if (size <= self.inline_bytes.len) return &self.inline_bytes;
        const bytes: [*]u8 = @ptrCast(ops.allocRaw(size, 16));
        self.heap = bytes;
        return bytes;
    }

    fn deinit(self: *ArgsBuffer, ops: *RocOps) void {
        if (self.heap) |bytes| ops.dealloc(@ptrCast(bytes), 16);
    }
};

/// Call the erased callable `callable` with the packed arguments at `args`,
/// writing its result to `ret`.
fn callMethod(callable: [*]u8, ret: ?[*]u8, args: ?[*]const u8, ops: *RocOps) void {
    var out_desc: ?*const anyopaque = null;
    erased_callable.payloadPtr(callable).callable_fn_ptr(ops, ret, args, erased_callable.capturePtr(callable), null, &out_desc);
}

fn argShape(d: *const TypeDesc) shape.StructFieldShape {
    return .{ .size = d.size, .alignment = d.alignment, .sort_key = @fromBackingInt(@as(u3, @intCast(d.sort_key))) };
}

/// `is_eq` through an override. The callee owns its arguments, so each
/// operand is retained for it.
fn callIsEq(callable: [*]u8, lhs: Value, rhs: Value, ops: *RocOps) bool {
    var measure: shape.StructMeasure = .{};
    const lhs_offset = measure.place(argShape(lhs.desc));
    const rhs_offset = measure.place(argShape(rhs.desc));
    var buffer: ArgsBuffer = .{};
    defer buffer.deinit(ops);
    const args = buffer.get(measure.finish().size, ops);
    copy(lhs.desc, args + lhs_offset, lhs.ptr);
    copy(rhs.desc, args + rhs_offset, rhs.ptr);
    incref(lhs.desc, lhs.ptr, 1, ops);
    incref(rhs.desc, rhs.ptr, 1, ops);
    var result: u8 = 0;
    callMethod(callable, @ptrCast(&result), args, ops);
    return result == 1;
}

/// `to_hash` through an override, with the `Hasher` record `{ state: U64 }`.
fn callToHash(callable: [*]u8, value: Value, hasher: u64, ops: *RocOps) u64 {
    var measure: shape.StructMeasure = .{};
    const value_offset = measure.place(argShape(value.desc));
    const hasher_offset = measure.place(argShape(compose.scalarDesc(.u64)));
    var buffer: ArgsBuffer = .{};
    defer buffer.deinit(ops);
    const args = buffer.get(measure.finish().size, ops);
    copy(value.desc, args + value_offset, value.ptr);
    std.mem.writeInt(u64, (args + hasher_offset)[0..8], hasher, .little);
    incref(value.desc, value.ptr, 1, ops);
    var result: u64 = 0;
    callMethod(callable, @ptrCast(&result), args, ops);
    return result;
}

/// `to_inspect` through an override.
fn callToInspect(callable: [*]u8, value: Value, ops: *RocOps) RocStr {
    var buffer: ArgsBuffer = .{};
    defer buffer.deinit(ops);
    const args = buffer.get(value.desc.size, ops);
    copy(value.desc, args, value.ptr);
    incref(value.desc, value.ptr, 1, ops);
    var result: RocStr = RocStr.empty();
    callMethod(callable, @ptrCast(&result), args, ops);
    return result;
}

/// The override `method` of a descriptor, if it has one.
fn override(d: *const TypeDesc, comptime method: std.meta.FieldEnum(desc_mod.Methods)) ?[*]u8 {
    const all = d.methods orelse return null;
    return @field(all, @tagName(method));
}

// Equality.

const EqCtx = struct {
    ops: *RocOps,

    pub const Operand = Value;
    pub const WalkError = error{};
    pub const Aggregate = structural.OperandPair(Value);
    pub const TagUnion = structural.OperandPair(Value);
    pub const List = struct { lhs: RocList, rhs: RocList, elem: *const TypeDesc };

    const Eval = structural.EqEval(EqCtx);
    const Pair = structural.OperandPair(Value);

    pub fn eqStep(self: EqCtx, lhs: Value, rhs: Value) WalkError!Eval.Step {
        const d = lhs.desc;
        return switch (d.kind) {
            .zst => .always_equal,
            .scalar => .{ .done = scalarsEqual(d.scalar, lhs.ptr, rhs.ptr, self.ops) },
            .str => .{ .done = lhs.rocStr().eql(rhs.rocStr().*) },
            .bool => .{ .done = lhs.ptr[0] == rhs.ptr[0] },
            .list => .{ .list = .{ .lhs = lhs.rocList().*, .rhs = rhs.rocList().*, .elem = d.child.? } },
            .box, .recursive_box => if (d.child.?.size == 0)
                .always_equal
            else
                .{ .retarget = .{ .lhs = lhs.boxPayload(), .rhs = rhs.boxPayload() } },
            .record, .tuple => .{ .components = .{ .lhs = lhs, .rhs = rhs } },
            .tag_union => .{ .tag_union = .{ .lhs = lhs, .rhs = rhs } },
            .erased_callable => invariant(self.ops, "a function value reached descriptor-guided equality"),
            .opaque_unknown => self.ops.crash("boxy runtime: a RocBox(RocUnknown) has no equality"),
            .nominal => if (override(d, .is_eq)) |is_eq|
                .{ .done = callIsEq(is_eq, lhs, rhs, self.ops) }
            else
                .{ .retarget = .{ .lhs = .at(d.child.?, lhs.ptr), .rhs = .at(d.child.?, rhs.ptr) } },
        };
    }

    pub fn componentCount(_: EqCtx, aggregate: Aggregate) u32 {
        return aggregate.lhs.desc.count;
    }

    pub fn componentOperands(_: EqCtx, aggregate: Aggregate, index: u32) WalkError!?Pair {
        const f = aggregate.lhs.desc.fieldBySemanticIndex(index);
        if (f.is_padding != 0) return null;
        return .{ .lhs = aggregate.lhs.field(f), .rhs = aggregate.rhs.field(f) };
    }

    pub fn variants(_: EqCtx, tag_union: TagUnion) WalkError!structural.VariantPair(Value) {
        const d = tag_union.lhs.desc;
        const lhs = d.readDiscriminant(tag_union.lhs.ptr);
        if (lhs != d.readDiscriminant(tag_union.rhs.ptr)) return .different;
        return .{ .same = lhs };
    }

    pub fn payloadCount(_: EqCtx, tag_union: TagUnion, discriminant: u32) u32 {
        return tag_union.lhs.desc.variants()[discriminant].payload_count;
    }

    pub fn payloadOperands(_: EqCtx, tag_union: TagUnion, discriminant: u32, index: u32) WalkError!Pair {
        const variant = &tag_union.lhs.desc.variants()[discriminant];
        return .{ .lhs = tag_union.lhs.tagPayload(variant, index), .rhs = tag_union.rhs.tagPayload(variant, index) };
    }

    pub fn listLens(_: EqCtx, list: List) [2]u64 {
        return .{ list.lhs.len(), list.rhs.len() };
    }

    pub fn itemOperands(_: EqCtx, list: List, index: u64) WalkError!Pair {
        return .{ .lhs = listItem(list.lhs, list.elem, index), .rhs = listItem(list.rhs, list.elem, index) };
    }
};

fn listItem(list: RocList, elem: *const TypeDesc, index: u64) Value {
    if (elem.size == 0) return .at(elem, zeroSizedPtr());
    return .at(elem, list.bytes.? + @as(usize, @intCast(index)) * elem.size);
}

fn readScalar(comptime T: type, ptr: [*]const u8) T {
    return @as(*align(1) const T, @ptrCast(ptr)).*;
}

fn scalarsEqual(scalar: desc_mod.Scalar, lhs: [*]const u8, rhs: [*]const u8, ops: *RocOps) bool {
    return switch (scalar) {
        .u8, .i8 => lhs[0] == rhs[0],
        .u16, .i16 => readScalar(u16, lhs) == readScalar(u16, rhs),
        .u32, .i32 => readScalar(u32, lhs) == readScalar(u32, rhs),
        .u64, .i64 => readScalar(u64, lhs) == readScalar(u64, rhs),
        .u128, .i128, .dec => readScalar(u128, lhs) == readScalar(u128, rhs),
        .f32 => readScalar(f32, lhs) == readScalar(f32, rhs),
        .f64 => readScalar(f64, lhs) == readScalar(f64, rhs),
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => readScalar(u128, lhs) == readScalar(u128, rhs),
        .opaque_ptr => invariant(ops, "an opaque pointer reached descriptor-guided equality"),
    };
}

/// Structural equality of two values of the type `desc` describes, honoring
/// nominal `is_eq` overrides.
pub fn eq(desc: *const TypeDesc, lhs: [*]u8, rhs: [*]u8, ops: *RocOps) bool {
    const ctx: EqCtx = .{ .ops = ops };
    return EqCtx.Eval.eq(ctx, .at(desc, lhs), .at(desc, rhs)) catch |err| switch (err) {};
}

// Hashing.

const HashCtx = struct {
    ops: *RocOps,

    pub const Operand = Value;
    pub const Hasher = u64;
    pub const WalkError = error{};
    pub const Aggregate = Value;
    pub const TagUnion = Value;
    pub const List = struct { list: RocList, elem: *const TypeDesc };

    const Eval = structural.HashEval(HashCtx);

    pub fn hashStep(self: HashCtx, value: Value, hasher: u64) WalkError!Eval.Step {
        const d = value.desc;
        return switch (d.kind) {
            .zst => .nothing,
            .scalar => .{ .done = hashScalar(d.scalar, value.ptr, hasher, self.ops) },
            .str => blk: {
                const bytes = value.rocStr().asSlice();
                break :blk .{ .done = hash_mod.hasher_write_bytes(hasher, @backingInt(hash_mod.HasherDomain.str), bytes.ptr, bytes.len) };
            },
            .bool => .{ .done = hash_mod.hasher_write_u64(hasher, @backingInt(hash_mod.HasherDomain.bool), value.ptr[0], 1) },
            .list => .{ .list = .{ .list = value.rocList().*, .elem = d.child.? } },
            .box, .recursive_box => if (d.child.?.size == 0) .nothing else .{ .retarget = value.boxPayload() },
            .record, .tuple => .{ .components = value },
            .tag_union => .{ .tag_union = value },
            .erased_callable => invariant(self.ops, "a function value reached descriptor-guided hashing"),
            .opaque_unknown => self.ops.crash("boxy runtime: a RocBox(RocUnknown) has no hash"),
            .nominal => if (override(d, .to_hash)) |to_hash|
                .{ .done = callToHash(to_hash, value, hasher, self.ops) }
            else
                .{ .retarget = .at(d.child.?, value.ptr) },
        };
    }

    pub fn componentCount(_: HashCtx, aggregate: Aggregate) u32 {
        return aggregate.desc.count;
    }

    pub fn componentOperand(_: HashCtx, aggregate: Aggregate, index: u32) WalkError!?Value {
        const f = aggregate.desc.fieldBySemanticIndex(index);
        if (f.is_padding != 0) return null;
        return aggregate.field(f);
    }

    pub fn variant(_: HashCtx, tag_union: TagUnion) WalkError!structural.HashVariant(Value) {
        const discriminant = tag_union.desc.readDiscriminant(tag_union.ptr);
        return .{ .variant = .{ .discriminant = discriminant, .row_index = discriminant } };
    }

    pub fn payloadCount(_: HashCtx, tag_union: TagUnion, discriminant: u32) u32 {
        return tag_union.desc.variants()[discriminant].payload_count;
    }

    pub fn payloadOperand(_: HashCtx, tag_union: TagUnion, discriminant: u32, index: u32) WalkError!Value {
        return tag_union.tagPayload(&tag_union.desc.variants()[discriminant], index);
    }

    pub fn writeU64(_: HashCtx, hasher: u64, value: u64) u64 {
        return hash_mod.hasher_write_u64(hasher, @backingInt(hash_mod.HasherDomain.u64), value, 8);
    }

    pub fn listLen(_: HashCtx, list: List) u64 {
        return list.list.len();
    }

    pub fn itemOperand(_: HashCtx, list: List, index: u64) WalkError!Value {
        return listItem(list.list, list.elem, index);
    }
};

fn hashScalar(scalar: desc_mod.Scalar, ptr: [*]const u8, hasher: u64, ops: *RocOps) u64 {
    const D = hash_mod.HasherDomain;
    const write = hash_mod.hasher_write_u64;
    return switch (scalar) {
        .u8 => write(hasher, @backingInt(D.u8), ptr[0], 1),
        .i8 => write(hasher, @backingInt(D.i8), ptr[0], 1),
        .u16 => write(hasher, @backingInt(D.u16), readScalar(u16, ptr), 2),
        .i16 => write(hasher, @backingInt(D.i16), readScalar(u16, ptr), 2),
        .u32 => write(hasher, @backingInt(D.u32), readScalar(u32, ptr), 4),
        .i32 => write(hasher, @backingInt(D.i32), readScalar(u32, ptr), 4),
        .u64 => write(hasher, @backingInt(D.u64), readScalar(u64, ptr), 8),
        .i64 => write(hasher, @backingInt(D.i64), readScalar(u64, ptr), 8),
        .f32 => hash_mod.hasher_write_f32_bits(hasher, readScalar(u32, ptr)),
        .f64 => hash_mod.hasher_write_f64_bits(hasher, readScalar(u64, ptr)),
        .u128 => hash128(hasher, D.u128, ptr),
        .i128 => hash128(hasher, D.i128, ptr),
        .dec => hash128(hasher, D.dec, ptr),
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => hash128(hasher, D.u128, ptr),
        .opaque_ptr => invariant(ops, "an opaque pointer reached descriptor-guided hashing"),
    };
}

fn hash128(hasher: u64, domain: hash_mod.HasherDomain, ptr: [*]const u8) u64 {
    const bits = readScalar(u128, ptr);
    return hash_mod.hasher_write_u128(hasher, @backingInt(domain), @truncate(bits), @truncate(bits >> 64));
}

/// Feed a value of the type `desc` describes into the `Hasher` state
/// `hasher`, honoring nominal `to_hash` overrides.
pub fn hash(desc: *const TypeDesc, hasher: u64, ptr: [*]u8, ops: *RocOps) u64 {
    const ctx: HashCtx = .{ .ops = ops };
    return HashCtx.Eval.hash(ctx, .at(desc, ptr), hasher) catch |err| switch (err) {};
}

// Inspection.

const InspectWork = union(enum) {
    literal: []const u8,
    label: *const Name,
    child: Value,
    /// The items of a list from `index` on, then its closing bracket.
    list_items: struct { list: RocList, elem: *const TypeDesc, index: u64 },
};

const InspectCtx = struct {
    ops: *RocOps,

    pub const Ty = Value;
    pub const Primitive = Value;
    pub const Record = Value;
    pub const Tuple = Value;
    pub const TagUnion = Value;
    pub const PresenceSlot = void;
    pub const Override = Value;
    pub const Child = Value;
    pub const Label = *const Name;

    const Spec = inspect_rules.InspectSpec(InspectCtx);

    pub fn inspectShape(self: InspectCtx, value: Value) Spec.Shape {
        const d = value.desc;
        return switch (d.kind) {
            .zst => .empty_record,
            .scalar => if (d.scalar.isVector())
                .{ .nominal = .{ .override = if (override(d, .to_inspect) != null) value else invariant(self.ops, "a SIMD vector descriptor has no to_inspect"), .backing = null } }
            else
                .{ .primitive = value },
            .str => .{ .primitive = value },
            .bool => .bool,
            .list => .{ .list = value },
            .box => .{ .box = if (d.child.?.size == 0) .at(d.child.?, zeroSizedPtr()) else value.boxPayload() },
            .record => .{ .record = value },
            .tuple => .{ .tuple = value },
            .tag_union => .{ .tag_union = value },
            .erased_callable => .function,
            .nominal => .{ .nominal = .{
                .override = if (override(d, .to_inspect) != null) value else null,
                .backing = if (d.flags.inspect_opaque) null else .at(d.child.?, value.ptr),
            } },
            .opaque_unknown => self.ops.crash("boxy runtime: a RocBox(RocUnknown) has no inspection"),
            .recursive_box => .{ .transparent = value.boxPayload() },
        };
    }

    pub fn recordSlotCount(_: InspectCtx, record: Value) u32 {
        return record.desc.count;
    }

    pub fn recordField(_: InspectCtx, record: Value, slot: u32) ?inspect_rules.RecordField(InspectCtx) {
        const f = record.desc.fieldBySemanticIndex(slot);
        if (f.is_padding != 0) return null;
        return .{ .label = f.name.?, .child = record.field(f) };
    }

    pub fn tupleElemCount(_: InspectCtx, tuple: Value) u32 {
        return tuple.desc.count;
    }

    pub fn tupleElem(_: InspectCtx, tuple: Value, index: u32) Value {
        return tuple.field(tuple.desc.fieldBySemanticIndex(index));
    }

    pub fn variantCount(_: InspectCtx, tag_union: Value) u32 {
        return tag_union.desc.count;
    }

    pub fn variantName(_: InspectCtx, tag_union: Value, variant: u32) *const Name {
        return tag_union.desc.variants()[variant].name;
    }

    pub fn payloadCount(_: InspectCtx, tag_union: Value, variant: u32) u32 {
        return tag_union.desc.variants()[variant].payload_count;
    }

    pub fn payload(_: InspectCtx, tag_union: Value, variant: u32, index: u32) Value {
        return tag_union.tagPayload(&tag_union.desc.variants()[variant], index);
    }
};

/// Collects a rendering's parts onto the work stack in rendering order.
const PartsSink = struct {
    stack: *Stack(InspectWork, 32),
    ops: *RocOps,

    pub const PartsError = error{};

    pub fn literal(self: *PartsSink, text: []const u8) PartsError!void {
        self.stack.push(.{ .literal = text }, self.ops);
    }

    pub fn label(self: *PartsSink, name: *const Name) PartsError!void {
        self.stack.push(.{ .label = name }, self.ops);
    }

    pub fn child(self: *PartsSink, value: Value) PartsError!void {
        self.stack.push(.{ .child = value }, self.ops);
    }
};

/// The text being rendered, grown geometrically through the builtin string
/// operations.
const Output = struct {
    str: RocStr = RocStr.empty(),

    fn append(self: *Output, bytes: []const u8, ops: *RocOps) void {
        if (bytes.len == 0) return;
        const len = self.str.len();
        const capacity = self.str.getCapacity();
        if (len + bytes.len > capacity) {
            self.str = builtins.str.reserve(self.str, @max(bytes.len, capacity), .InPlace, ops);
        }
        @memcpy(self.str.asU8ptrMut()[len .. len + bytes.len], bytes);
        self.str.setLen(len + bytes.len);
    }

    /// Append a string the output takes ownership of.
    fn appendOwned(self: *Output, owned: RocStr, ops: *RocOps) void {
        self.append(owned.asSlice(), ops);
        owned.decref(ops);
    }
};

fn appendPrimitive(out: *Output, value: Value, ops: *RocOps) void {
    const d = value.desc;
    if (d.kind == .str) return out.appendOwned(builtins.str.strEscapeAndQuote(value.rocStr().*, ops), ops);
    var buffer: [builtins.compiler_rt_128.float_string_capacity]u8 = undefined;
    const ptr = value.ptr;
    const text: []const u8 = switch (d.scalar) {
        .u8 => intText(u8, &buffer, ptr),
        .i8 => intText(i8, &buffer, ptr),
        .u16 => intText(u16, &buffer, ptr),
        .i16 => intText(i16, &buffer, ptr),
        .u32 => intText(u32, &buffer, ptr),
        .i32 => intText(i32, &buffer, ptr),
        .u64 => intText(u64, &buffer, ptr),
        .i64 => intText(i64, &buffer, ptr),
        .u128 => intText(u128, &buffer, ptr),
        .i128 => intText(i128, &buffer, ptr),
        .f32 => builtins.str.floatToStrBytes(&buffer, readScalar(u32, ptr), true),
        .f64 => builtins.str.floatToStrBytes(&buffer, readScalar(u64, ptr), false),
        .dec => (RocDec{ .num = readScalar(i128, ptr) }).format_to_buf(buffer[0..RocDec.max_str_length]),
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => invariant(ops, "a SIMD vector renders through its to_inspect"),
        .opaque_ptr => invariant(ops, "an opaque pointer reached descriptor-guided inspection"),
    };
    out.append(text, ops);
}

fn intText(comptime T: type, buffer: []u8, ptr: [*]const u8) []const u8 {
    return builtins.compiler_rt_128.int_to_str(T, buffer, readScalar(T, ptr));
}

/// What `Str.inspect` renders for a value of the type `desc` describes,
/// honoring nominal `to_inspect` overrides.
pub fn inspect(desc: *const TypeDesc, ptr: [*]u8, ops: *RocOps) RocStr {
    const ctx: InspectCtx = .{ .ops = ops };
    const Spec = InspectCtx.Spec;
    var out: Output = .{};
    var stack: Stack(InspectWork, 32) = .{};
    defer stack.deinit(ops);
    var sink: PartsSink = .{ .stack = &stack, .ops = ops };
    stack.push(.{ .child = .at(desc, ptr) }, ops);
    while (stack.pop()) |work| switch (work) {
        .literal => |text| out.append(text, ops),
        .label => |name| out.append(name.text(), ops),
        .list_items => |items| {
            if (items.index == items.list.len()) {
                out.append(inspect_rules.text.list_close, ops);
                continue;
            }
            stack.push(.{ .list_items = .{ .list = items.list, .elem = items.elem, .index = items.index + 1 } }, ops);
            stack.push(.{ .child = listItem(items.list, items.elem, items.index) }, ops);
            const separator = Spec.listSeparator(items.index);
            if (separator.len != 0) stack.push(.{ .literal = separator }, ops);
        },
        .child => |value| {
            const start = stack.len;
            switch (Spec.action(ctx, value)) {
                .literal => |text| out.append(text, ops),
                .primitive => |primitive| appendPrimitive(&out, primitive, ops),
                .bool => out.append(Spec.boolText(value.ptr[0] == 1), ops),
                .record => |record| Spec.recordParts(ctx, record, &sink) catch |err| switch (err) {},
                .tuple => |tuple| Spec.tupleParts(ctx, tuple, &sink) catch |err| switch (err) {},
                .tag_union => |tag_union| Spec.variantParts(ctx, tag_union, tag_union.desc.readDiscriminant(tag_union.ptr), &sink) catch |err| switch (err) {},
                .uninhabited => ops.crash(inspect_rules.text.uninhabited_crash),
                .presence_slot => invariant(ops, "a descriptor described a field-presence slot"),
                .list => |list_value| {
                    out.append(inspect_rules.text.list_open, ops);
                    stack.push(.{ .list_items = .{ .list = list_value.rocList().*, .elem = list_value.desc.child.?, .index = 0 } }, ops);
                },
                .box => |payload| {
                    sink.literal(inspect_rules.text.box_open) catch |err| switch (err) {};
                    sink.child(payload) catch |err| switch (err) {};
                    sink.literal(inspect_rules.text.box_close) catch |err| switch (err) {};
                },
                .override => |overridden| out.appendOwned(callToInspect(override(overridden.desc, .to_inspect).?, overridden, ops), ops),
                .backing => |backing| stack.push(.{ .child = backing }, ops),
                .transparent => |named| stack.push(.{ .child = named }, ops),
                .descriptor => invariant(ops, "a descriptor described a value as dynamic"),
            }
            stack.reverseFrom(start);
        },
    };
    return out.str;
}
