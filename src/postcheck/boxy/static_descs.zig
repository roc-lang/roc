//! Static descriptors and names (design.md "Type Descriptors").
//!
//! Every closed type a body needs a descriptor for gets one static
//! descriptor, interned by the type's join-set representative, emitted as
//! readonly static data in the runtime's descriptor format at the target's
//! pointer width. Each descriptor (and each name) is a static-data slot whose
//! value is the pointer to the descriptor (or `Name`) node, so a body reads it
//! into an `opaque_ptr` local with an ordinary `static_data` literal. Nodes
//! reference one another through relocations; recursive types make the node
//! graph cyclic, which needs no special treatment because every node is
//! reserved before any is filled.

const std = @import("std");
const base = @import("base");
const layout = @import("layout");
const lir_core = @import("lir_core");
const boxy_runtime = @import("boxy_runtime");
const types = @import("types.zig");
const Layouts = @import("layouts.zig").Layouts;

const Allocator = std.mem.Allocator;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const desc_format = boxy_runtime.desc;
const TypeId = types.TypeId;
const NameId = types.NameId;

const StaticDataExport = LirProgram.StaticDataExport;
const StaticDataRelocation = LirProgram.StaticDataRelocation;
const ExportIndex = u32;

pub const StaticDescs = struct {
    allocator: Allocator,
    store: *types.Store,
    layouts: *Layouts,
    result: *LirProgram.Result,
    width: desc_format.PointerWidth,
    exports: std.ArrayList(StaticDataExport) = .empty,
    /// Descriptor slot and node of each closed type, by representative.
    desc_slots: std.AutoHashMapUnmanaged(Key, Reserved) = .empty,
    name_slots: std.AutoHashMapUnmanaged(NameId, Reserved) = .empty,
    pending: std.ArrayList(Pending) = .empty,

    /// A descriptor's identity: a type, or the recursion box around one.
    const Key = struct { ty: TypeId, recursive_box: bool };
    const Reserved = struct { slot: LIR.StaticDataId, node: ExportIndex };
    const Pending = struct { key: Key, node: ExportIndex };

    pub fn init(allocator: Allocator, store: *types.Store, layouts: *Layouts, result: *LirProgram.Result, target_usize: base.target.TargetUsize) StaticDescs {
        return .{
            .allocator = allocator,
            .store = store,
            .layouts = layouts,
            .result = result,
            .width = switch (target_usize) {
                .u32 => .u32,
                .u64 => .u64,
            },
        };
    }

    pub fn deinit(self: *StaticDescs) void {
        for (self.exports.items) |item| {
            self.allocator.free(item.symbol_name);
            self.allocator.free(item.bytes);
            for (item.relocations) |relocation| {
                if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
            }
            self.allocator.free(item.relocations);
        }
        self.exports.deinit(self.allocator);
        self.desc_slots.deinit(self.allocator);
        self.name_slots.deinit(self.allocator);
        self.pending.deinit(self.allocator);
    }

    /// Hand the emitted exports to the program; the caller owns them after.
    pub fn takeExports(self: *StaticDescs) Allocator.Error![]StaticDataExport {
        return try self.exports.toOwnedSlice(self.allocator);
    }

    /// The static-data slot holding the descriptor of closed type `ty`.
    pub fn descriptorSlot(self: *StaticDescs, ty: TypeId) Allocator.Error!LIR.StaticDataId {
        const reserved = try self.reserveDescriptor(.{ .ty = self.layouts.joins.representative(ty), .recursive_box = false });
        try self.drain();
        return reserved.slot;
    }

    /// The static-data slot holding the `Name` of `name`.
    pub fn nameSlot(self: *StaticDescs, name: NameId) Allocator.Error!LIR.StaticDataId {
        return (try self.reserveName(name)).slot;
    }

    fn ptrSize(self: *const StaticDescs) u32 {
        return self.width.bytes();
    }

    fn addSlot(self: *StaticDescs) Allocator.Error!LIR.StaticDataId {
        const id: LIR.StaticDataId = @fromBackingInt(@intCast(self.result.static_data_values.items.len));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = .opaque_ptr,
        });
        return id;
    }

    /// Append an export of `size` zero bytes and return its index.
    fn addExport(self: *StaticDescs, name: []u8, size: u32, alignment: u32, value_id: ?LIR.StaticDataId) Allocator.Error!ExportIndex {
        errdefer self.allocator.free(name);
        const bytes = try self.allocator.alloc(u8, size);
        @memset(bytes, 0);
        const index: ExportIndex = @intCast(self.exports.items.len);
        try self.exports.append(self.allocator, .{
            .symbol_name = name,
            .value_id = value_id,
            .bytes = bytes,
            .alignment = alignment,
            .is_global = false,
            .is_exported = false,
        });
        return index;
    }

    /// A slot whose value points at a fresh node of `node_size` bytes.
    fn addSlotWithNode(self: *StaticDescs, node_size: u32, node_alignment: u32) Allocator.Error!Reserved {
        const slot = try self.addSlot();
        const slot_export = try self.addExport(try LirProgram.staticDataSymbolName(self.allocator, slot), self.ptrSize(), self.ptrSize(), slot);
        const node = try self.addExport(try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(slot), 1), node_size, node_alignment, null);
        try self.setRelocations(slot_export, &.{.{ .offset = 0, .target = node }});
        return .{ .slot = slot, .node = node };
    }

    /// One pointer inside a node: at `offset`, the address of `target`.
    const Pointer = struct { offset: u32, target: ExportIndex };

    fn setRelocations(self: *StaticDescs, owner: ExportIndex, pointers: []const Pointer) Allocator.Error!void {
        const relocations = try self.allocator.alloc(StaticDataRelocation, pointers.len);
        errdefer self.allocator.free(relocations);
        for (pointers, relocations) |pointer, *relocation| {
            relocation.* = .{
                .offset = pointer.offset,
                .target_symbol_name = self.exports.items[pointer.target].symbol_name,
                .target = .{ .data_symbol = @fromBackingInt(pointer.target) },
            };
        }
        const item = &self.exports.items[owner];
        self.allocator.free(item.relocations);
        item.relocations = relocations;
    }

    fn reserveName(self: *StaticDescs, name: NameId) Allocator.Error!Reserved {
        if (self.name_slots.get(name)) |existing| return existing;
        const name_layout = desc_format.externLayout(desc_format.Name, self.width);
        const reserved = try self.addSlotWithNode(name_layout.size, name_layout.alignment);
        const text = self.store.nameText(name);
        const text_node = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(reserved.slot), 2),
            @intCast(@max(text.len, 1)),
            1,
            null,
        );
        @memcpy(@constCast(self.exports.items[text_node].bytes)[0..text.len], text);
        const bytes = @constCast(self.exports.items[reserved.node].bytes);
        writeU32(bytes, name_layout.of(.id), @backingInt(name));
        writeU32(bytes, name_layout.of(.len), @intCast(text.len));
        try self.setRelocations(reserved.node, &.{.{ .offset = name_layout.of(.bytes), .target = text_node }});
        try self.name_slots.put(self.allocator, name, reserved);
        return reserved;
    }

    fn reserveDescriptor(self: *StaticDescs, key: Key) Allocator.Error!Reserved {
        if (self.desc_slots.get(key)) |existing| return existing;
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        const reserved = try self.addSlotWithNode(desc_layout.size, desc_layout.alignment);
        try self.desc_slots.put(self.allocator, key, reserved);
        try self.pending.append(self.allocator, .{ .key = key, .node = reserved.node });
        return reserved;
    }

    fn drain(self: *StaticDescs) Allocator.Error!void {
        while (self.pending.pop()) |item| try self.fill(item);
    }

    /// The descriptor node of a component stored at `committed`: the
    /// component type's own descriptor, or the recursion box around it when
    /// the committed layout boxed a component the type does not box.
    fn componentNode(self: *StaticDescs, ty: TypeId, committed: layout.Idx) Allocator.Error!ExportIndex {
        const rep = self.layouts.joins.representative(ty);
        const boxed_by_layout = self.layouts.layouts.getLayout(committed).tag == .box and self.store.get(rep) != .box;
        return (try self.reserveDescriptor(.{ .ty = rep, .recursive_box = boxed_by_layout })).node;
    }

    fn fill(self: *StaticDescs, item: Pending) Allocator.Error!void {
        const desc_layout = desc_format.TypeDesc.layout(self.width);
        var pointers: std.ArrayList(Pointer) = .empty;
        defer pointers.deinit(self.allocator);

        if (item.key.recursive_box) {
            const payload = (try self.reserveDescriptor(.{ .ty = item.key.ty, .recursive_box = false })).node;
            const bytes = @constCast(self.exports.items[item.node].bytes);
            writeU32(bytes, desc_layout.of(.size), self.ptrSize());
            writeU32(bytes, desc_layout.of(.alignment), self.ptrSize());
            bytes[desc_layout.of(.kind)] = @backingInt(desc_format.Kind.recursive_box);
            bytes[desc_layout.of(.sort_key)] = @backingInt(layout.SortKey.pointer);
            bytes[desc_layout.of(.flags)] = @bitCast(desc_format.Flags{ .contains_refcounted = true });
            try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = payload });
            return try self.setRelocations(item.node, pointers.items);
        }

        const ty = item.key.ty;
        const committed = try self.layouts.layoutOf(ty);
        const layouts = self.layouts.layouts;
        const committed_layout = layouts.getLayout(committed);
        const size_align = layouts.layoutSizeAlign(committed_layout);
        var kind: desc_format.Kind = undefined;
        var scalar: desc_format.Scalar = .u8;
        var count: u32 = 0;
        var discriminant_offset: u32 = 0;
        var discriminant_size: u32 = 0;
        var flags: desc_format.Flags = .{ .contains_refcounted = layouts.layoutContainsRefcounted(committed_layout) };

        switch (self.store.get(ty)) {
            .primitive => |primitive| switch (primitive) {
                .bool => {
                    kind = .bool;
                    discriminant_size = 1;
                },
                .str => kind = .str,
                else => {
                    kind = .scalar;
                    scalar = scalarOf(primitive);
                },
            },
            .empty_record => kind = .record,
            .function => kind = .erased_callable,
            .list => |elem| {
                kind = .list;
                const elem_layout = switch (committed_layout.tag) {
                    .list => committed_layout.getIdx(),
                    else => try self.layouts.layoutOf(elem),
                };
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(elem, elem_layout) });
            },
            .box => |payload| {
                kind = .box;
                const payload_layout = switch (committed_layout.tag) {
                    .box => committed_layout.getIdx(),
                    else => try self.layouts.layoutOf(payload),
                };
                try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = try self.componentNode(payload, payload_layout) });
            },
            .record => |record| {
                kind = .record;
                const fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                const storage = try self.allocator.alloc(TypeId, fields.len);
                defer self.allocator.free(storage);
                for (fields, storage) |field, *slot| slot.* = try self.layouts.fieldStorage(field);
                count = @intCast(fields.len);
                try self.fillFields(item.node, &pointers, committed_layout, storage, fields);
            },
            .tuple => |items_span| {
                kind = .tuple;
                const items = try self.allocator.dupe(TypeId, self.store.span(items_span));
                defer self.allocator.free(items);
                count = @intCast(items.len);
                try self.fillFields(item.node, &pointers, committed_layout, items, null);
            },
            .tag_union => |tag_union| {
                kind = .tag_union;
                const tags = try self.allocator.dupe(types.Tag, self.store.tagSpan(tag_union.tags));
                defer self.allocator.free(tags);
                count = @intCast(tags.len);
                if (committed_layout.tag == .tag_union) {
                    const info = layouts.getTagUnionInfo(committed_layout);
                    discriminant_offset = info.discriminant_offset;
                    discriminant_size = info.data.discriminant_size;
                }
                try self.fillVariants(item.node, &pointers, committed_layout, tags);
            },
            .nominal => |nominal| {
                if (nominal.builtin == .bool) {
                    kind = .bool;
                    discriminant_size = 1;
                } else {
                    kind = .nominal;
                    flags.inspect_opaque = nominal.is_opaque;
                    const backing = try self.layouts.converter.backing(ty);
                    const backing_node = (try self.reserveDescriptor(.{ .ty = self.layouts.joins.representative(backing), .recursive_box = false })).node;
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.child), .target = backing_node });
                    try pointers.append(self.allocator, .{ .offset = desc_layout.of(.name), .target = (try self.reserveName(nominal.name)).node });
                }
            },
            .variable => types.invariant("a type variable reached static descriptor emission"),
        }

        const bytes = @constCast(self.exports.items[item.node].bytes);
        writeU32(bytes, desc_layout.of(.size), size_align.size);
        writeU32(bytes, desc_layout.of(.alignment), @intCast(size_align.alignment.toByteUnits()));
        bytes[desc_layout.of(.kind)] = @backingInt(kind);
        bytes[desc_layout.of(.scalar)] = @backingInt(scalar);
        bytes[desc_layout.of(.sort_key)] = @backingInt(committed_layout.sortKey());
        bytes[desc_layout.of(.flags)] = @bitCast(flags);
        writeU32(bytes, desc_layout.of(.count), count);
        writeU32(bytes, desc_layout.of(.discriminant_offset), discriminant_offset);
        writeU32(bytes, desc_layout.of(.discriminant_size), discriminant_size);
        try self.setRelocations(item.node, pointers.items);
    }

    /// The `Field` entries of a record or tuple, in the committed layout's
    /// memory order. `names` is null for a tuple.
    fn fillFields(
        self: *StaticDescs,
        owner: ExportIndex,
        pointers: *std.ArrayList(Pointer),
        committed_layout: layout.Layout,
        storage: []const TypeId,
        names: ?[]const types.Field,
    ) Allocator.Error!void {
        if (storage.len == 0) return;
        const layouts = self.layouts.layouts;
        const field_layout = desc_format.Field.layout(self.width);
        const owner_slot = self.slotOfNode(owner);
        const entries = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(owner_slot), 2),
            field_layout.size * @as(u32, @intCast(storage.len)),
            field_layout.alignment,
            null,
        );
        var entry_pointers: std.ArrayList(Pointer) = .empty;
        defer entry_pointers.deinit(self.allocator);
        const bytes_len = self.exports.items[entries].bytes.len;
        _ = bytes_len;
        if (committed_layout.tag == .struct_) {
            const struct_idx = committed_layout.getStruct().idx;
            const memory_count = layouts.getStructData(struct_idx).fields.count;
            if (memory_count != storage.len) types.invariant("a record's committed layout disagrees with its field count");
            var position: u32 = 0;
            while (position < memory_count) : (position += 1) {
                const field = layouts.getStructField(struct_idx, position);
                const base_offset = position * field_layout.size;
                const bytes = @constCast(self.exports.items[entries].bytes);
                writeU32(bytes, base_offset + field_layout.of(.semantic_index), field.index);
                writeU32(bytes, base_offset + field_layout.of(.offset), layouts.getStructFieldOffset(struct_idx, position));
                writeU32(bytes, base_offset + field_layout.of(.is_padding), @intFromBool(field.is_padding));
                // The entry at memory position `field.index` names this slot.
                writeU32(bytes, field.index * field_layout.size + field_layout.of(.semantic_order), position);
                const component = storage[field.index];
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentNode(component, field.layout) });
                if (names) |record_fields| {
                    try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.name), .target = (try self.reserveName(record_fields[field.index].name)).node });
                }
            }
        } else {
            // Every field is zero-sized: each sits at offset 0, in semantic order.
            for (storage, 0..) |component, index| {
                const base_offset: u32 = @as(u32, @intCast(index)) * field_layout.size;
                const bytes = @constCast(self.exports.items[entries].bytes);
                writeU32(bytes, base_offset + field_layout.of(.semantic_index), @intCast(index));
                writeU32(bytes, base_offset + field_layout.of(.semantic_order), @intCast(index));
                try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.desc), .target = try self.componentNode(component, try self.layouts.layoutOf(component)) });
                if (names) |record_fields| {
                    try entry_pointers.append(self.allocator, .{ .offset = base_offset + field_layout.of(.name), .target = (try self.reserveName(record_fields[index].name)).node });
                }
            }
        }
        try self.setRelocations(entries, entry_pointers.items);
        try pointers.append(self.allocator, .{ .offset = desc_format.TypeDesc.layout(self.width).of(.entries), .target = entries });
    }

    fn fillVariants(
        self: *StaticDescs,
        owner: ExportIndex,
        pointers: *std.ArrayList(Pointer),
        committed_layout: layout.Layout,
        tags: []const types.Tag,
    ) Allocator.Error!void {
        if (tags.len == 0) return;
        const layouts = self.layouts.layouts;
        const variant_layout = desc_format.Variant.layout(self.width);
        const owner_slot = self.slotOfNode(owner);
        const entries = try self.addExport(
            try LirProgram.staticDataNodeSymbolName(self.allocator, @backingInt(owner_slot), 2),
            variant_layout.size * @as(u32, @intCast(tags.len)),
            variant_layout.alignment,
            null,
        );
        var entry_pointers: std.ArrayList(Pointer) = .empty;
        defer entry_pointers.deinit(self.allocator);
        const zst = try self.store.emptyRecord();
        for (tags, 0..) |tag, index| {
            const payloads = try self.allocator.dupe(TypeId, self.store.span(tag.payloads));
            defer self.allocator.free(payloads);
            const payload_ty = (try self.layouts.tagPayload(payloads)) orelse zst;
            const payload_layout = if (committed_layout.tag == .tag_union)
                layouts.getTagUnionInfo(committed_layout).variants.get(index).payload_layout
            else
                try self.layouts.layoutOf(payload_ty);
            const base_offset: u32 = @as(u32, @intCast(index)) * variant_layout.size;
            const bytes = @constCast(self.exports.items[entries].bytes);
            writeU32(bytes, base_offset + variant_layout.of(.discriminant), @intCast(index));
            writeU32(bytes, base_offset + variant_layout.of(.payload_count), @intCast(payloads.len));
            try entry_pointers.append(self.allocator, .{ .offset = base_offset + variant_layout.of(.name), .target = (try self.reserveName(tag.name)).node });
            try entry_pointers.append(self.allocator, .{ .offset = base_offset + variant_layout.of(.payload), .target = try self.componentNode(payload_ty, payload_layout) });
        }
        try self.setRelocations(entries, entry_pointers.items);
        try pointers.append(self.allocator, .{ .offset = desc_format.TypeDesc.layout(self.width).of(.entries), .target = entries });
    }

    /// The slot a node export was created for: node `roc__d{slot}_1`
    /// directly follows its slot's export.
    fn slotOfNode(self: *const StaticDescs, node: ExportIndex) LIR.StaticDataId {
        return self.exports.items[node - 1].value_id orelse types.invariant("a descriptor node did not follow its slot");
    }
};

fn writeU32(bytes: []u8, offset: u32, value: u32) void {
    std.mem.writeInt(u32, bytes[offset..][0..4], value, .little);
}

fn scalarOf(primitive: types.Primitive) desc_format.Scalar {
    return switch (primitive) {
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
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
        .bool, .str => unreachable, // `fill` gives Bool and Str their own kinds
    };
}
