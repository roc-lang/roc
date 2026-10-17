//! The readonly node graph a frozen root is written into: symbol-addressed
//! byte nodes, the data relocations between them, shared allocation
//! reservations, and the final conversion into owned static-data exports.
//!
//! `native_root_export.zig` fills a graph by reading an evaluated value from
//! memory; `frozen_root_transcode.zig` fills one by reading another program's
//! frozen exports. Both write through this one target-width encoding.

const std = @import("std");
const compilerInvariant = @import("base").invariant;
const layout = @import("layout");
const lir = @import("lir");
const static_data = @import("static_data");
const Allocator = std.mem.Allocator;
const Program = lir.Program;
const SymbolId = static_data.StaticDataSymbolId;

/// A byte position inside one node of the graph.
pub const Destination = struct {
    symbol: SymbolId,
    offset: usize = 0,

    /// The position `amount` bytes further into the same node.
    pub fn offsetBy(self: Destination, amount: usize) Destination {
        return .{ .symbol = self.symbol, .offset = self.offset + amount };
    }
};

/// One readonly data symbol being built.
pub const Node = struct {
    name: []const u8,
    bytes: []u8,
    alignment: u32,
    relocations: std.ArrayList(static_data.StaticDataRelocation) = .empty,
    /// The evaluated capacities of the empty lists inside the root value.
    empty_list_capacities: std.ArrayList(static_data.EmptyListCapacity) = .empty,
};

/// A graph for the root of `slot` in `program`. `AllocationKey` identifies a
/// source allocation together with the typed view it is frozen at, so a
/// shared or recursive allocation is written once.
pub fn Graph(comptime AllocationKey: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        program: *const Program.Result,
        slot: lir.LIR.StaticDataId,
        nodes: std.ArrayList(Node) = .empty,
        allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,

        /// The target pointer width in bytes.
        pub fn word(self: *const Self) usize {
            return self.program.layouts.targetUsize().size();
        }

        /// The target size of `idx`.
        pub fn size(self: *const Self, idx: layout.Idx) usize {
            return self.program.layouts.layoutSize(self.program.layouts.getLayout(idx));
        }

        /// The target alignment of `idx`.
        pub fn alignment(self: *const Self, idx: layout.Idx) u32 {
            return @intCast(self.program.layouts.getLayout(idx).alignment(self.program.layouts.targetUsize()).toByteUnits());
        }

        /// Append a zeroed node.
        pub fn addNode(self: *Self, name: []const u8, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
            const node_bytes = try self.allocator.alloc(u8, byte_count);
            @memset(node_bytes, 0);
            const symbol: SymbolId = @fromBackingInt(@intCast(self.nodes.items.len));
            try self.nodes.append(self.allocator, .{ .name = name, .bytes = node_bytes, .alignment = alignment_ });
            return symbol;
        }

        fn addChildNode(self: *Self, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
            const name = try Program.staticDataNodeSymbolName(self.allocator, @backingInt(self.slot), @intCast(self.nodes.items.len));
            return self.addNode(name, byte_count, alignment_);
        }

        /// The node `dest` lies in.
        pub fn node(self: *Self, dest: Destination) *Node {
            return &self.nodes.items[@backingInt(dest.symbol)];
        }

        /// The `count` bytes at `dest`.
        pub fn bytes(self: *Self, dest: Destination, count: usize) []u8 {
            return self.node(dest).bytes[dest.offset..][0..count];
        }

        /// Write one target-width little-endian word at `dest`.
        pub fn writeWord(self: *Self, dest: Destination, value: u64) void {
            switch (self.word()) {
                4 => std.mem.writeInt(u32, self.bytes(dest, 4)[0..4], @intCast(value), .little),
                8 => std.mem.writeInt(u64, self.bytes(dest, 8)[0..8], value, .little),
                else => invariant("unsupported target pointer width"),
            }
        }

        /// Make the word at `dest` point at `target`.
        pub fn relocate(self: *Self, dest: Destination, target: Destination) Allocator.Error!void {
            // Zero pointer bytes explicitly; only the relocation gives them meaning.
            self.writeWord(dest, 0);
            try self.node(dest).relocations.append(self.allocator, .{
                .offset = dest.offset,
                .target_symbol_name = self.node(target).name,
                .target = .{ .data_symbol = target.symbol },
                .addend = @intCast(target.offset),
            });
        }

        /// The payload position of the allocation `key` names, reserving a
        /// fresh node behind its header the first time it is seen.
        pub fn reserveAllocation(self: *Self, key: AllocationKey, byte_count: usize, alignment_: u32, contains_refcounted: bool, element_count: ?usize) Allocator.Error!struct { dest: Destination, fresh: bool } {
            if (self.allocations.get(key)) |existing| return .{ .dest = existing, .fresh = false };
            const header_size: usize = (if (contains_refcounted) @as(usize, 2) else 1) * self.word();
            const payload_offset = std.mem.alignForward(usize, header_size, alignment_);
            const allocation_size = std.math.add(usize, payload_offset, byte_count) catch return error.OutOfMemory;
            const symbol = try self.addChildNode(allocation_size, @intCast(@max(alignment_, self.word())));
            const dest = Destination{ .symbol = symbol, .offset = payload_offset };
            // A zero RC header is the runtime's immutable/static allocation marker.
            if (contains_refcounted) self.writeWord(.{ .symbol = symbol, .offset = payload_offset - 2 * self.word() }, element_count orelse 0);
            try self.allocations.put(self.allocator, key, dest);
            return .{ .dest = dest, .fresh = true };
        }

        /// A new node holding the closed Boxy descriptor `id`. Descriptors
        /// are pointer-free image rows whose child references remain IDs in
        /// the program's immutable sidecar, exactly as in LirImage.
        pub fn frozenDescriptor(self: *Self, id: Program.BoxyTypeDescId) Allocator.Error!Destination {
            const descriptor = &self.program.boxy_type_descs.items[@backingInt(id)];
            if (descriptor.closure != .closed) invariant("frozen descriptor recipe retained runtime context");
            const dest = Destination{ .symbol = try self.addChildNode(@sizeOf(Program.BoxyTypeDesc), @alignOf(Program.BoxyTypeDesc)) };
            @memcpy(self.bytes(dest, @sizeOf(Program.BoxyTypeDesc)), std.mem.asBytes(descriptor));
            return dest;
        }

        /// A new node holding the closed Boxy dictionary `id`.
        pub fn frozenDictionary(self: *Self, id: Program.BoxyDictId) Allocator.Error!Destination {
            const dict = self.program.boxy_dicts.items[@backingInt(id)];
            if (dict.template) invariant("frozen dictionary retained a runtime template");
            const dest = Destination{ .symbol = try self.addChildNode(@sizeOf(Program.BoxyDict), @alignOf(Program.BoxyDict)) };
            @memcpy(self.bytes(dest, @sizeOf(Program.BoxyDict)), std.mem.asBytes(&dict));
            return dest;
        }

        /// Copy the graph into exports owned by `allocator`. The first export
        /// is the requested slot.
        pub fn finish(self: *Self, allocator: Allocator) Allocator.Error![]static_data.StaticDataExport {
            const exports = try allocator.alloc(static_data.StaticDataExport, self.nodes.items.len);
            var done: usize = 0;
            errdefer {
                for (exports[0..done]) |export_| {
                    allocator.free(export_.symbol_name);
                    allocator.free(export_.bytes);
                    for (export_.relocations) |relocation| {
                        if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
                    }
                    allocator.free(export_.relocations);
                    allocator.free(export_.empty_list_capacities);
                }
                allocator.free(exports);
            }
            for (self.nodes.items, exports, 0..) |source, *dest, index| {
                const name = try allocator.dupe(u8, source.name);
                errdefer allocator.free(name);
                const owned_bytes = try allocator.dupe(u8, source.bytes);
                errdefer allocator.free(owned_bytes);
                const relocations = try allocator.dupe(static_data.StaticDataRelocation, source.relocations.items);
                errdefer allocator.free(relocations);
                const capacities = try allocator.dupe(static_data.EmptyListCapacity, source.empty_list_capacities.items);
                errdefer allocator.free(capacities);
                // All names are assigned in a separate pass once every symbol exists.
                for (relocations) |*relocation| relocation.owns_target_symbol_name = false;
                dest.* = .{
                    .symbol_name = name,
                    .value_id = if (index == 0) self.slot else null,
                    .symbol_identity = if (index == 0) self.program.static_data_values.items[@backingInt(self.slot)].symbol_identity else null,
                    .bytes = owned_bytes,
                    .alignment = source.alignment,
                    .is_global = false,
                    .is_exported = false,
                    .root_slot = true,
                    .relocations = relocations,
                    .empty_list_capacities = capacities,
                };
                done += 1;
            }
            for (exports) |*export_| {
                const relocations: []static_data.StaticDataRelocation = @constCast(export_.relocations);
                for (relocations) |*relocation| {
                    switch (relocation.target) {
                        .data_symbol => |symbol| relocation.target_symbol_name = exports[@backingInt(symbol)].symbol_name,
                        .named => {
                            relocation.target_symbol_name = try allocator.dupe(u8, relocation.target_symbol_name);
                            relocation.owns_target_symbol_name = true;
                        },
                    }
                }
            }
            return exports;
        }
    };
}

/// The field layout and offset of Boxy capture `slot` inside capture layout `idx`.
pub fn boxyCaptureLocation(program: *const Program.Result, idx: layout.Idx, slot: u32) struct { idx: layout.Idx, offset: usize } {
    const physical = program.layouts.getLayout(idx);
    if (physical.tag == .struct_) return .{
        .idx = program.layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, @intCast(slot)),
        .offset = program.layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, @intCast(slot)),
    };
    if (slot != 0) invariant("non-struct Boxy capture had multiple fields");
    return .{ .idx = idx, .offset = 0 };
}

fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .debug) compilerInvariant("frozen graph invariant violated: {s}", .{message});
    unreachable;
}
