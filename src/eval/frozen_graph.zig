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

/// One readonly data symbol being built. `name` and `bytes` are allocated
/// by the graph's `output` allocator and become the export's own slices.
pub const Node = struct {
    name: []const u8,
    bytes: []u8,
    alignment: u32,
    relocations: std.ArrayList(static_data.StaticDataRelocation) = .empty,
};

/// A graph for the root of `slot` in `program`. `AllocationKey` identifies a
/// source allocation together with the typed view it is frozen at, so a
/// shared or recursive allocation is written once.
pub fn Graph(comptime AllocationKey: type) type {
    return struct {
        const Self = @This();

        /// Scratch for building the graph, released by the caller.
        allocator: Allocator,
        /// Owner of every node's name and bytes, which `finish` hands to
        /// the exports; `deinit` frees them when the graph never finishes.
        output: Allocator,
        program: *const Program.Result,
        slot: lir.LIR.StaticDataId,
        nodes: std.ArrayList(Node) = .empty,
        allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,
        finished: bool = false,

        /// Free the node names and bytes `finish` did not hand out.
        pub fn deinit(self: *Self) void {
            if (self.finished) return;
            for (self.nodes.items) |built| {
                self.output.free(built.name);
                self.output.free(built.bytes);
            }
        }

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

        /// Append a zeroed node named `name`, which `output` allocated and
        /// the graph owns from here on, including when this fails.
        pub fn addNode(self: *Self, name: []const u8, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
            errdefer self.output.free(name);
            // A node the target's address space cannot hold cannot exist there.
            if (self.word() == 4 and std.math.cast(u32, byte_count) == null) return error.OutOfMemory;
            const node_bytes = try self.output.alloc(u8, byte_count);
            errdefer self.output.free(node_bytes);
            @memset(node_bytes, 0);
            const symbol: SymbolId = @fromBackingInt(@intCast(self.nodes.items.len));
            try self.nodes.append(self.allocator, .{ .name = name, .bytes = node_bytes, .alignment = alignment_ });
            return symbol;
        }

        fn addChildNode(self: *Self, byte_count: usize, alignment_: u32) Allocator.Error!SymbolId {
            const name = try Program.staticDataNodeSymbolName(self.output, @backingInt(self.slot), @intCast(self.nodes.items.len));
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

        /// Write one target-width little-endian word at `dest`. A value the
        /// target's word cannot hold, such as a length a 64-bit evaluation
        /// produced for a 32-bit target, is past what that target can address.
        pub fn writeWord(self: *Self, dest: Destination, value: u64) Allocator.Error!void {
            switch (self.word()) {
                4 => std.mem.writeInt(u32, self.bytes(dest, 4)[0..4], std.math.cast(u32, value) orelse return error.OutOfMemory, .little),
                8 => std.mem.writeInt(u64, self.bytes(dest, 8)[0..8], value, .little),
                else => invariant("unsupported target pointer width"),
            }
        }

        /// Make the word at `dest` point at `target`.
        pub fn relocate(self: *Self, dest: Destination, target: Destination) Allocator.Error!void {
            // Zero pointer bytes explicitly; only the relocation gives them meaning.
            try self.writeWord(dest, 0);
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
            const symbol = try self.addChildNode(std.math.add(usize, payload_offset, byte_count) catch return error.OutOfMemory, @intCast(@max(alignment_, self.word())));
            const dest = Destination{ .symbol = symbol, .offset = payload_offset };
            // A zero RC header is the runtime's immutable/static allocation marker.
            if (contains_refcounted) try self.writeWord(.{ .symbol = symbol, .offset = payload_offset - 2 * self.word() }, element_count orelse 0);
            try self.allocations.put(self.allocator, key, dest);
            return .{ .dest = dest, .fresh = true };
        }

        /// Hand the graph to exports owned by `output`: each export takes
        /// its node's name and bytes. The first export is the requested slot.
        pub fn finish(self: *Self) Allocator.Error![]static_data.StaticDataExport {
            const allocator = self.output;
            const exports = try allocator.alloc(static_data.StaticDataExport, self.nodes.items.len);
            var done: usize = 0;
            errdefer {
                for (exports[0..done]) |export_| {
                    for (export_.relocations) |relocation| {
                        if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
                    }
                    allocator.free(export_.relocations);
                }
                allocator.free(exports);
            }
            for (self.nodes.items, exports, 0..) |source, *dest, index| {
                const relocations = try allocator.dupe(static_data.StaticDataRelocation, source.relocations.items);
                errdefer allocator.free(relocations);
                // All names are assigned in a separate pass once every symbol exists.
                for (relocations) |*relocation| relocation.owns_target_symbol_name = false;
                dest.* = .{
                    .symbol_name = source.name,
                    .value_id = if (index == 0) self.slot else null,
                    .bytes = source.bytes,
                    .alignment = source.alignment,
                    .is_global = false,
                    .is_exported = false,
                    .relocations = relocations,
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
            self.finished = true;
            return exports;
        }
    };
}

/// A frozen root written as one block, the stored image of a value that is
/// data alone (`check.ConstStore.ConstImage`): the root value at offset 0,
/// then each allocation it reaches behind its static header, and a list of
/// the words that point into the block, each holding the block offset it
/// points at. It has `Graph`'s interface, with every destination in the one
/// block, so the root freezer writes either form.
pub fn ImageGraph(comptime AllocationKey: type) type {
    return struct {
        const Self = @This();
        /// An image holds no function, so the freezer never reaches the
        /// parts of its plans that name code.
        pub const is_image = true;

        /// Scratch for the allocation table and the result.
        allocator: Allocator,
        program: *const Program.Result,
        block: std.ArrayList(u8) = .empty,
        pointers: std.ArrayList(u32) = .empty,
        allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,
        alignment_: u32 = 1,

        pub fn deinit(self: *Self) void {
            self.block.deinit(self.allocator);
            self.pointers.deinit(self.allocator);
            self.allocations.deinit(self.allocator);
        }

        /// Images are at the 64-bit layout.
        pub fn word(self: *const Self) usize {
            _ = self;
            return 8;
        }

        pub fn size(self: *const Self, idx: layout.Idx) usize {
            return self.program.layouts.layoutSize(self.program.layouts.getLayout(idx));
        }

        pub fn alignment(self: *const Self, idx: layout.Idx) u32 {
            return @intCast(self.program.layouts.getLayout(idx).alignment(self.program.layouts.targetUsize()).toByteUnits());
        }

        /// Reserve `byte_count` zeroed bytes at `alignment_` at the end of
        /// the block and return their offset.
        fn extend(self: *Self, byte_count: usize, alignment_: u32) Allocator.Error!usize {
            const start = std.mem.alignForward(usize, self.block.items.len, alignment_);
            const end = std.math.add(usize, start, byte_count) catch return error.OutOfMemory;
            // Pointer words and pointer-list entries are 32-bit block offsets.
            if (end > std.math.maxInt(u32)) return error.OutOfMemory;
            try self.block.appendNTimes(self.allocator, 0, end - self.block.items.len);
            self.alignment_ = @max(self.alignment_, alignment_);
            return start;
        }

        /// Start the block with the root value's `byte_count` bytes.
        pub fn root(self: *Self, byte_count: usize, alignment_: u32) Allocator.Error!Destination {
            if (self.block.items.len != 0) invariant("an image's root was reserved twice");
            _ = try self.extend(byte_count, alignment_);
            return .{ .symbol = @fromBackingInt(0) };
        }

        pub fn bytes(self: *Self, dest: Destination, count: usize) []u8 {
            return self.block.items[dest.offset..][0..count];
        }

        pub fn writeWord(self: *Self, dest: Destination, value: u64) Allocator.Error!void {
            std.mem.writeInt(u64, self.bytes(dest, 8)[0..8], value, .little);
        }

        /// Make the word at `dest` point at `target`: it holds the block
        /// offset of `target`, and the pointer list names it.
        pub fn relocate(self: *Self, dest: Destination, target: Destination) Allocator.Error!void {
            try self.writeWord(dest, target.offset);
            try self.pointers.append(self.allocator, @intCast(dest.offset));
        }

        pub fn reserveAllocation(self: *Self, key: AllocationKey, byte_count: usize, alignment_: u32, contains_refcounted: bool, element_count: ?usize) Allocator.Error!struct { dest: Destination, fresh: bool } {
            if (self.allocations.get(key)) |existing| return .{ .dest = existing, .fresh = false };
            const header_size: usize = (if (contains_refcounted) @as(usize, 2) else 1) * self.word();
            const payload_offset = std.mem.alignForward(usize, header_size, alignment_);
            const start = try self.extend(std.math.add(usize, payload_offset, byte_count) catch return error.OutOfMemory, @intCast(@max(alignment_, self.word())));
            const dest = Destination{ .symbol = @fromBackingInt(0), .offset = start + payload_offset };
            // A zero RC header is the runtime's immutable/static allocation
            // marker; `extend` zeroed it.
            if (contains_refcounted) try self.writeWord(.{ .symbol = @fromBackingInt(0), .offset = dest.offset - 2 * self.word() }, element_count orelse 0);
            try self.allocations.put(self.allocator, key, dest);
            return .{ .dest = dest, .fresh = true };
        }

        /// A finished image: the block, its pointer words in ascending
        /// order, and its alignment, all owned by the graph.
        pub const Result = struct { bytes: []const u8, pointers: []const u32, alignment: u32 };

        pub fn finish(self: *Self) Result {
            std.mem.sortUnstable(u32, self.pointers.items, {}, std.sort.asc(u32));
            return .{ .bytes = self.block.items, .pointers = self.pointers.items, .alignment = self.alignment_ };
        }
    };
}

fn invariant(comptime message: []const u8) noreturn {
    compilerInvariant("frozen graph invariant violated: {s}", .{message});
}
