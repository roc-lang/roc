//! Descriptors built for tests of the walkers and of code that reads
//! descriptors. A compiled program's descriptors are static data the
//! compiler lays out from the layout store (`postcheck/boxy/static_descs.zig`);
//! these are laid out with the same shape code (`layout_shape`), so a test's
//! descriptor has the shape the compiler emits for the same type. A builder
//! lays types out at one pointer width: the native one, or another target's,
//! whose descriptors a test serializes for that target.

const std = @import("std");
const builtins = @import("builtins");
const shape = @import("layout_shape");
const desc_mod = @import("desc.zig");

const field_order = shape.field_order;
const SortKey = shape.SortKey;
const RocList = builtins.list.RocList;

const TypeDesc = desc_mod.TypeDesc;
const Field = desc_mod.Field;
const Variant = desc_mod.Variant;
const Name = desc_mod.Name;
const Names = desc_mod.Names;
const Methods = desc_mod.Methods;
const Kind = desc_mod.Kind;

/// The `size` of a recursive placeholder until it is closed: never zero, so
/// a recursion box over it holds an allocation.
const open_size = std.math.maxInt(u32);

/// One record field: its label and its value's descriptor.
pub const FieldInput = struct {
    name: *const Name,
    desc: *const TypeDesc,
};

/// One field of a declared-order record. A padding field has no name.
pub const DeclaredFieldInput = struct {
    name: ?*const Name,
    desc: *const TypeDesc,
    is_padding: bool = false,
};

/// One tag: its name and its arguments' descriptors.
pub const VariantInput = struct {
    name: *const Name,
    payloads: []const *const TypeDesc,
};

/// Builds descriptors in memory that lives as long as the builder.
pub const Builder = struct {
    arena: std.heap.ArenaAllocator,
    width: desc_mod.PointerWidth,
    str_leaf: TypeDesc,
    erased_callable_leaf: TypeDesc,
    /// Each record's, tuple's, and tag union's labels: a field's by memory
    /// slot (null for a tuple element or padding), a variant's by
    /// discriminant. A descriptor holds no names; its names block does.
    labels: std.AutoHashMapUnmanaged(*const TypeDesc, []const ?*const Name) = .empty,
    /// Each names block `names` built, by descriptor.
    names_blocks: std.AutoHashMapUnmanaged(*const TypeDesc, ?*const Names) = .empty,

    pub fn init(allocator: std.mem.Allocator, width: desc_mod.PointerWidth) Builder {
        const word = width.bytes();
        return .{
            .arena = .init(allocator),
            .width = width,
            .str_leaf = desc_mod.leaf(.str, .u8, builtins.str.RocStr.word_count * word, word, .pointer, true),
            .erased_callable_leaf = desc_mod.leaf(.erased_callable, .u8, word, word, .pointer, true),
        };
    }

    /// The descriptor of `Str` at the builder's width.
    pub fn str(self: *const Builder) *const TypeDesc {
        return if (self.width == desc_mod.PointerWidth.native) desc_mod.strDesc() else &self.str_leaf;
    }

    /// The descriptor every function value shares, at the builder's width.
    pub fn erasedCallable(self: *const Builder) *const TypeDesc {
        return if (self.width == desc_mod.PointerWidth.native) desc_mod.erasedCallableDesc() else &self.erased_callable_leaf;
    }

    pub fn deinit(self: *Builder) void {
        self.arena.deinit();
    }

    fn alloc(self: *Builder, comptime T: type, count: usize) []T {
        return self.arena.allocator().alloc(T, count) catch @panic("out of memory building a test descriptor");
    }

    fn one(self: *Builder, value: TypeDesc) *const TypeDesc {
        const built = self.alloc(TypeDesc, 1);
        built[0] = value;
        return &built[0];
    }

    fn putLabels(self: *Builder, desc: *const TypeDesc, labels: []const ?*const Name) void {
        self.labels.put(self.arena.allocator(), desc, labels) catch @panic("out of memory building a test descriptor");
    }

    /// The names block of `desc`, which holds the labels and tag names its
    /// descriptor does not (`desc.Names`); null for a type without
    /// components. Built on first request, after every descriptor it
    /// reaches is complete, so a recursive type's block reaches itself.
    pub fn names(self: *Builder, desc: *const TypeDesc) ?*const Names {
        if (self.names_blocks.get(desc)) |known| return known;
        const block: ?*Names = switch (desc.kind) {
            .zst, .scalar, .str, .bool, .erased_callable => null,
            .list, .box, .nominal, .recursive_box, .record, .tuple, .tag_union => &self.alloc(Names, 1)[0],
        };
        self.names_blocks.put(self.arena.allocator(), desc, block) catch @panic("out of memory building a test descriptor");
        const built = block orelse return null;
        built.* = .{ .entries = null, .child = null };
        switch (desc.kind) {
            .zst, .scalar, .str, .bool, .erased_callable => unreachable, // these have no block
            .list, .box, .nominal, .recursive_box => built.child = self.names(desc.child.?),
            .record, .tuple, .tag_union => {
                const labels = self.labels.get(desc) orelse &.{};
                const entries = self.alloc(desc_mod.NamesEntry, desc.count);
                for (entries, 0..) |*entry, index| {
                    const component = if (desc.kind == .tag_union) desc.variants()[index].payload else desc.fields()[index].desc;
                    entry.* = .{ .label = if (index < labels.len) labels[index] else null, .names = self.names(component) };
                }
                if (desc.count != 0) built.entries = @ptrCast(entries.ptr);
            },
        }
        return built;
    }

    /// `List(elem)`. A list of a zero-sized element is still three words.
    pub fn list(self: *Builder, elem: *const TypeDesc) *const TypeDesc {
        return self.container(.list, elem, true);
    }

    /// `Box(payload)`. A box of a zero-sized payload is one word and holds no
    /// allocation. A box of a function is the function's erased callable,
    /// with the function as its `child` so the value still inspects as a box.
    pub fn box(self: *Builder, payload: *const TypeDesc) *const TypeDesc {
        if (payload.kind == .erased_callable and payload.child == null) {
            var built = payload.*;
            built.child = payload;
            return self.one(built);
        }
        return self.container(.box, payload, payload.size != 0);
    }

    /// The implicit box a layout inserts to break a recursive type. Its
    /// payload may be an open recursive placeholder (`recursiveOpen`).
    pub fn recursiveBox(self: *Builder, payload: *const TypeDesc) *const TypeDesc {
        return self.container(.recursive_box, payload, payload.size != 0);
    }

    /// A placeholder for a recursive type's descriptor, for its components'
    /// recursive boxes to point at until `recursiveClose` fills it.
    pub fn recursiveOpen(self: *Builder) *const TypeDesc {
        var shell = desc_mod.leaf(.zst, .u8, 0, 1, .align_1, false);
        shell.size = open_size;
        return self.one(shell);
    }

    /// Fill the placeholder `shell` with the recursive type's descriptor
    /// `desc`.
    pub fn recursiveClose(self: *Builder, shell: *const TypeDesc, desc: *const TypeDesc) *const TypeDesc {
        @constCast(shell).* = desc.*;
        if (self.labels.get(desc)) |labels| self.putLabels(shell, labels);
        return shell;
    }

    fn container(self: *Builder, comptime kind: Kind, child: *const TypeDesc, contains_refcounted: bool) *const TypeDesc {
        const word = self.width.bytes();
        const size: u32 = if (kind == .list) RocList.word_count * word else word;
        var built = desc_mod.leaf(kind, .u8, size, word, .pointer, contains_refcounted);
        built.child = child;
        built.flags.recursive = kind == .recursive_box;
        return self.one(built);
    }

    /// A structural record. Fields may come in any order: a field's semantic
    /// index is its label's rank in byte-wise text order, and fields are laid
    /// out by descending sort key, then ascending semantic index.
    pub fn record(self: *Builder, fields: []const FieldInput) *const TypeDesc {
        const by_name = self.alloc(u32, fields.len);
        sortByName(FieldInput, fields, by_name);
        const members = self.alloc(Member, fields.len);
        for (by_name, 0..) |input_index, rank| {
            const field = fields[input_index];
            members[input_index] = .{ .name = field.name, .desc = field.desc, .field_index = @intCast(rank), .is_padding = false };
        }
        return self.buildStruct(.record, .structural, members);
    }

    /// A nominal record laid out in declared order with C-style padding, as
    /// a record with `_` fields is. `fields` is in declared order; a named
    /// field's semantic index is its label's rank among the named fields, and
    /// padding fields rank after them in declared order.
    pub fn declaredRecord(self: *Builder, fields: []const DeclaredFieldInput) *const TypeDesc {
        var named_count: u32 = 0;
        for (fields) |field| {
            if (!field.is_padding) named_count += 1;
        }
        const named = self.alloc(FieldInput, named_count);
        const named_positions = self.alloc(u32, named_count);
        {
            var next: u32 = 0;
            for (fields, 0..) |field, index| {
                if (field.is_padding) continue;
                named[next] = .{ .name = field.name.?, .desc = field.desc };
                named_positions[next] = @intCast(index);
                next += 1;
            }
        }
        const by_name = self.alloc(u32, named_count);
        sortByName(FieldInput, named, by_name);
        const members = self.alloc(Member, fields.len);
        for (by_name, 0..) |named_index, rank| {
            const position = named_positions[named_index];
            members[position] = .{ .name = fields[position].name, .desc = fields[position].desc, .field_index = @intCast(rank), .is_padding = false };
        }
        var padding_rank = named_count;
        for (fields, 0..) |field, position| {
            if (!field.is_padding) continue;
            members[position] = .{ .name = null, .desc = field.desc, .field_index = padding_rank, .is_padding = true };
            padding_rank += 1;
        }
        return self.buildStruct(.record, .declared, members);
    }

    /// A tuple: element `i` has semantic index `i`; elements are laid out
    /// structurally.
    pub fn tuple(self: *Builder, elems: []const *const TypeDesc) *const TypeDesc {
        return self.positional(.structural, elems);
    }

    /// The packed argument record of an erased call: the arguments in
    /// declared order, each at its natural alignment
    /// (`layout.erased_call_abi.plan`).
    pub fn args(self: *Builder, descs: []const *const TypeDesc) *const TypeDesc {
        return self.positional(.declared, descs);
    }

    fn positional(self: *Builder, order: Order, descs: []const *const TypeDesc) *const TypeDesc {
        const members = self.alloc(Member, descs.len);
        for (descs, 0..) |d, index| members[index] = .{ .name = null, .desc = d, .field_index = @intCast(index), .is_padding = false };
        return self.buildStruct(.tuple, order, members);
    }

    /// An optional record field's presence slot over `payload`: the tag
    /// union `[missing, present(payload)]`, marked as a presence slot.
    pub fn presenceSlot(self: *Builder, missing: *const Name, present: *const Name, payload: *const TypeDesc) *const TypeDesc {
        var built = self.tagUnion(&.{
            .{ .name = missing, .payloads = &.{} },
            .{ .name = present, .payloads = &.{payload} },
        }).*;
        built.flags.presence_slot = true;
        const slot = self.one(built);
        self.putLabels(slot, &.{ missing, present });
        return slot;
    }

    /// A closed tag union. Tags may come in any order: a tag's discriminant
    /// is its name's rank in byte-wise text order.
    pub fn tagUnion(self: *Builder, variants: []const VariantInput) *const TypeDesc {
        const by_name = self.alloc(u32, variants.len);
        sortByName(VariantInput, variants, by_name);
        const discriminant_size = shape.discriminantSize(variants.len);
        var measure = shape.TagUnionMeasure.init(discriminant_size);
        const entries = self.alloc(Variant, variants.len);
        const labels = self.alloc(?*const Name, variants.len);
        var contains_refcounted = false;
        var recursive = false;
        for (by_name, 0..) |input_index, discriminant| {
            const input = variants[input_index];
            const payload = switch (input.payloads.len) {
                0 => desc_mod.zstDesc(),
                1 => input.payloads[0],
                else => self.tuple(input.payloads),
            };
            measure.addPayload(.{ .size = payload.size, .alignment = payload.alignment, .sort_key = sortKeyOf(payload) });
            contains_refcounted = contains_refcounted or payload.flags.contains_refcounted;
            recursive = recursive or payload.flags.recursive;
            entries[discriminant] = .{
                .discriminant = @intCast(discriminant),
                .payload_count = @intCast(input.payloads.len),
                .payload = payload,
            };
            labels[discriminant] = input.name;
        }
        const metrics = measure.finish();
        const zero_sized = metrics.size == 0;
        const built = self.one(.{
            .size = metrics.size,
            .alignment = if (zero_sized) 1 else metrics.alignment,
            .kind = .tag_union,
            .scalar = .u8,
            .sort_key = @backingInt(if (zero_sized) SortKey.align_1 else metrics.sort_key),
            .flags = .{ .contains_refcounted = contains_refcounted, .recursive = recursive },
            .count = @intCast(variants.len),
            .discriminant_offset = metrics.discriminant_offset,
            .discriminant_size = discriminant_size,
            .child = null,
            .entries = if (variants.len == 0) null else @ptrCast(entries.ptr),
            .methods = null,
        });
        self.putLabels(built, labels);
        return built;
    }

    /// A nominal type over `backing`, whose layout it shares.
    pub fn nominal(self: *Builder, backing: *const TypeDesc, methods: ?*const Methods, inspect_opaque: bool) *const TypeDesc {
        return self.one(.{
            .size = backing.size,
            .alignment = backing.alignment,
            .kind = .nominal,
            .scalar = .u8,
            .sort_key = backing.sort_key,
            .flags = .{ .contains_refcounted = backing.flags.contains_refcounted, .inspect_opaque = inspect_opaque, .recursive = backing.flags.recursive },
            .count = 0,
            .discriminant_offset = 0,
            .discriminant_size = 0,
            .child = backing,
            .entries = null,
            .methods = methods,
        });
    }

    /// Lay out `members` (indexed by input position) as one struct and build
    /// its descriptor. Semantic indices must be a permutation of the
    /// positions.
    fn buildStruct(self: *Builder, kind: Kind, order: Order, members: []const Member) *const TypeDesc {
        const memory_order = self.alloc(u32, members.len);
        switch (order) {
            .declared => for (memory_order, 0..) |*slot, index| {
                slot.* = @intCast(index);
            },
            .structural => {
                const keys = self.alloc(field_order.StructuralField, members.len);
                for (members, keys) |member, *key| key.* = .{
                    .sort_key = if (member.is_padding) .align_1 else sortKeyOf(member.desc),
                    .semantic_index = member.field_index,
                };
                field_order.computeStructuralFieldOrder(keys, memory_order);
            },
        }

        var refcounted_count: u32 = 0;
        var recursive = false;
        for (members) |member| {
            if (member.is_padding) continue;
            if (member.desc.flags.contains_refcounted) refcounted_count += 1;
            recursive = recursive or member.desc.flags.recursive;
        }
        // The entries, then the trailer of reference-holding field slots.
        const trailer_words: usize = if (refcounted_count == 0) 0 else refcounted_count + 1;
        const entries_bytes = std.mem.alignForward(usize, members.len * @sizeOf(Field), @alignOf(u32));
        const block = self.arena.allocator().alignedAlloc(u8, .of(Field), entries_bytes + trailer_words * @sizeOf(u32)) catch @panic("out of memory building a test descriptor");
        const entries: [*]Field = @ptrCast(@alignCast(block.ptr));
        const trailer: [*]u32 = @ptrCast(@alignCast(block.ptr + entries_bytes));
        if (refcounted_count != 0) trailer[0] = refcounted_count;
        var next_refcounted: usize = 1;
        const labels = self.alloc(?*const Name, members.len);
        var measure: shape.StructMeasure = .{};
        for (memory_order, 0..) |member_index, slot| {
            const member = members[member_index];
            const offset = measure.place(.{
                .size = member.desc.size,
                .alignment = member.desc.alignment,
                .sort_key = sortKeyOf(member.desc),
                .is_padding = member.is_padding,
            });
            if (!member.is_padding and member.desc.flags.contains_refcounted) {
                trailer[next_refcounted] = @intCast(slot);
                next_refcounted += 1;
            }
            entries[slot] = .{
                .field_index = member.field_index,
                .offset = offset,
                .is_padding = @intFromBool(member.is_padding),
                .field_slot = std.math.maxInt(u32),
                .desc = member.desc,
            };
            labels[slot] = member.name;
        }
        for (entries[0..members.len], 0..) |entry, slot| {
            std.debug.assert(entry.field_index < members.len and entries[entry.field_index].field_slot == std.math.maxInt(u32));
            entries[entry.field_index].field_slot = @intCast(slot);
        }

        const metrics = measure.finish();
        const zero_sized = metrics.size == 0;
        const built = self.one(.{
            .size = metrics.size,
            .alignment = if (zero_sized) 1 else metrics.alignment,
            .kind = kind,
            .scalar = .u8,
            .sort_key = @backingInt(if (zero_sized) SortKey.align_1 else metrics.sort_key),
            .flags = .{ .contains_refcounted = refcounted_count != 0, .declared_order = order == .declared, .recursive = recursive },
            .count = @intCast(members.len),
            .discriminant_offset = 0,
            .discriminant_size = 0,
            .child = null,
            .entries = if (members.len == 0) null else @ptrCast(entries),
            .methods = null,
        });
        self.putLabels(built, labels);
        return built;
    }
};

const Order = enum { structural, declared };

const Member = struct {
    name: ?*const Name,
    desc: *const TypeDesc,
    field_index: u32,
    is_padding: bool,
};

fn sortKeyOf(d: *const TypeDesc) SortKey {
    return @fromBackingInt(@as(u3, @intCast(d.sort_key)));
}

fn nameOrder(a: *const Name, b: *const Name) std.math.Order {
    return std.mem.order(u8, a.text(), b.text());
}

/// Fill `out` with the indices of `items` ordered by name text. A test
/// names each label once.
fn sortByName(comptime T: type, items: []const T, out: []u32) void {
    for (out, 0..) |*slot, index| slot.* = @intCast(index);
    const Ctx = struct {
        items: []const T,
        fn lessThan(ctx: @This(), a: u32, b: u32) bool {
            return nameOrder(ctx.items[a].name, ctx.items[b].name) == .lt;
        }
    };
    std.sort.pdq(u32, out, Ctx{ .items = items }, Ctx.lessThan);
    var index: usize = 1;
    while (index < out.len) : (index += 1) std.debug.assert(nameOrder(items[out[index - 1]].name, items[out[index]].name) != .eq);
}
