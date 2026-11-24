//! The readonly node graph a frozen root is written into: symbol-addressed
//! byte nodes, the data relocations between them, shared allocation
//! reservations, and the final conversion into owned static-data exports.
//!
//! `native_root_export.zig` fills a graph by reading an evaluated value from
//! memory; `frozen_root_transcode.zig` fills one by reading another program's
//! frozen exports. Both write through this one target-width encoding.

const std = @import("std");
const compilerInvariant = @import("base").invariant;
const base_target = @import("base").target;
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

        /// The pointer width the graph's destinations are at: the program's.
        pub fn width(self: *const Self) base_target.TargetUsize {
            return self.program.layouts.targetUsize();
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

        /// The symbol every destination of an image names: an image is one
        /// block, which its destinations address by offset alone, so no
        /// reader of a destination reads it.
        const image_block: SymbolId = undefined;

        /// Scratch for the allocation table and the result.
        allocator: Allocator,
        program: *const Program.Result,
        block: std.ArrayList(u8) = .empty,
        pointers: std.ArrayList(u32) = .empty,
        allocations: std.AutoHashMapUnmanaged(AllocationKey, Destination) = .empty,
        alignment_: u32 = 1,
        /// The allocations after the root, in block order.
        segments: std.ArrayList(Segment) = .empty,

        /// An allocation of the block, from the first byte of its header at
        /// `start`, at `alignment`; `payload` is its first payload byte, what
        /// a pointer word holds.
        const Segment = struct { start: usize, end: usize, alignment: u32, payload: usize };

        pub fn deinit(self: *Self) void {
            self.block.deinit(self.allocator);
            self.pointers.deinit(self.allocator);
            self.allocations.deinit(self.allocator);
            self.segments.deinit(self.allocator);
        }

        /// Images are at the 64-bit layout, whatever the evaluation's
        /// width.
        pub fn word(_: *const Self) usize {
            return 8;
        }

        pub fn width(_: *const Self) base_target.TargetUsize {
            return .u64;
        }

        pub fn size(self: *const Self, idx: layout.Idx) usize {
            return self.program.layouts.sizeAt(self.program.layouts.getLayout(idx), .u64);
        }

        pub fn alignment(self: *const Self, idx: layout.Idx) u32 {
            return @intCast(self.program.layouts.getLayout(idx).alignment(.u64).toByteUnits());
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
            return .{ .symbol = image_block };
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
            const dest = Destination{ .symbol = image_block, .offset = start + payload_offset };
            // A zero RC header is the runtime's immutable/static allocation
            // marker; `extend` zeroed it.
            if (contains_refcounted) try self.writeWord(.{ .symbol = image_block, .offset = dest.offset - 2 * self.word() }, element_count orelse 0);
            try self.allocations.put(self.allocator, key, dest);
            try self.segments.append(self.allocator, .{ .start = start, .end = start + payload_offset + byte_count, .alignment = @intCast(@max(alignment_, self.word())), .payload = dest.offset });
            return .{ .dest = dest, .fresh = true };
        }

        /// A finished image: the block, its pointer words in ascending
        /// order, and its alignment, all owned by the graph.
        pub const Result = struct { bytes: []const u8, pointers: []const u32, alignment: u32 };

        pub fn finish(self: *Self) Allocator.Error!Result {
            std.mem.sortUnstable(u32, self.pointers.items, {}, std.sort.asc(u32));
            try self.mergeEqualAllocations();
            return .{ .bytes = self.block.items, .pointers = self.pointers.items, .alignment = self.alignment_ };
        }

        /// Make allocations of equal content one, and put them in one order:
        /// an evaluation reads equal immutable data (two equal literals) from
        /// one static allocation, which its image holds once, so an image
        /// holds each content once, whichever producer wrote it, and in the
        /// order a breadth-first walk from the root reaches it, whichever
        /// order its producer visited it in. Two allocations are equal when
        /// their bytes are, with each pointer word naming an allocation of
        /// equal content; the first in block order is kept, and every pointer
        /// to another points to it.
        fn mergeEqualAllocations(self: *Self) Allocator.Error!void {
            const segments = self.segments.items;
            if (segments.len < 2) return;
            const gpa = self.allocator;
            var by_payload: std.AutoHashMapUnmanaged(usize, u32) = .empty;
            defer by_payload.deinit(gpa);
            for (segments, 0..) |segment, index| try by_payload.put(gpa, segment.payload, @intCast(index));

            // Each segment's class of equal content, its children's first:
            // a value's allocations reach one another acyclically.
            const unassigned = std.math.maxInt(u32);
            const class = try gpa.alloc(u32, segments.len);
            defer gpa.free(class);
            @memset(class, unassigned);
            var class_first: std.ArrayList(u32) = .empty;
            defer class_first.deinit(gpa);
            var keys: std.StringHashMapUnmanaged(u32) = .empty;
            defer {
                var it = keys.keyIterator();
                while (it.next()) |key| gpa.free(key.*);
                keys.deinit(gpa);
            }
            var stack: std.ArrayList(u32) = .empty;
            defer stack.deinit(gpa);
            for (0..segments.len) |first| {
                if (class[first] != unassigned) continue;
                try stack.append(gpa, @intCast(first));
                while (stack.items.len != 0) {
                    const index = stack.items[stack.items.len - 1];
                    if (class[index] != unassigned) {
                        _ = stack.pop();
                        continue;
                    }
                    const segment = segments[index];
                    const pointers = self.pointersIn(segment.start, segment.end);
                    var waiting = false;
                    for (pointers) |at| if (by_payload.get(self.pointerTarget(at))) |child| if (class[child] == unassigned) {
                        try stack.append(gpa, child);
                        waiting = true;
                    };
                    if (waiting) continue;
                    _ = stack.pop();
                    // The bytes, each pointer word naming its target's class
                    // (or, for a target in the root, its offset), behind the
                    // payload's place and the alignment.
                    const content = self.block.items[segment.start..segment.end];
                    const key = try gpa.alloc(u8, 16 + content.len);
                    std.mem.writeInt(u64, key[0..8], segment.payload - segment.start, .little);
                    std.mem.writeInt(u64, key[8..16], segment.alignment, .little);
                    @memcpy(key[16..], content);
                    for (pointers) |at| {
                        const target = self.pointerTarget(at);
                        const named: u64 = if (by_payload.get(target)) |child| (@as(u64, 1) << 63) | class[child] else target;
                        std.mem.writeInt(u64, key[16 + at - segment.start ..][0..8], named, .little);
                    }
                    const entry = try keys.getOrPut(gpa, key);
                    if (entry.found_existing) {
                        gpa.free(key);
                        class[index] = entry.value_ptr.*;
                        class_first.items[class[index]] = @min(class_first.items[class[index]], index);
                    } else {
                        entry.value_ptr.* = @intCast(class_first.items.len);
                        class[index] = entry.value_ptr.*;
                        try class_first.append(gpa, index);
                    }
                }
            }
            const kept = try gpa.alloc(bool, segments.len);
            defer gpa.free(kept);
            for (kept, 0..) |*keep, index| keep.* = class_first.items[class[index]] == index;

            // The kept allocations in the order a breadth-first walk from the
            // root reaches them, each one's pointer words in block order: one
            // order for one value, whichever order its producer visited it in.
            var order: std.ArrayList(u32) = .empty;
            defer order.deinit(gpa);
            const placed = try gpa.alloc(bool, segments.len);
            defer gpa.free(placed);
            @memset(placed, false);
            try self.appendReached(self.pointersIn(0, segments[0].start), &by_payload, class, class_first.items, placed, &order);
            var next: usize = 0;
            while (next < order.items.len) : (next += 1) {
                const segment = segments[order.items[next]];
                try self.appendReached(self.pointersIn(segment.start, segment.end), &by_payload, class, class_first.items, placed, &order);
            }
            var in_place = order.items.len == segments.len;
            for (order.items, 0..) |index, position| {
                if (index != position) in_place = false;
            }
            if (in_place) return;

            // Lay the kept allocations out again, after the root, in that
            // order; a merged one moves to its kept equal.
            const new_start = try gpa.alloc(usize, segments.len);
            defer gpa.free(new_start);
            var block: std.ArrayList(u8) = .empty;
            errdefer block.deinit(gpa);
            try block.appendSlice(gpa, self.block.items[0..segments[0].start]);
            for (order.items) |index| {
                const segment = segments[index];
                new_start[index] = std.mem.alignForward(usize, block.items.len, segment.alignment);
                try block.appendNTimes(gpa, 0, new_start[index] - block.items.len);
                try block.appendSlice(gpa, self.block.items[segment.start..segment.end]);
            }
            for (kept, 0..) |keep, index| {
                if (!keep) new_start[index] = new_start[class_first.items[class[index]]];
            }
            var pointers: std.ArrayList(u32) = .empty;
            errdefer pointers.deinit(gpa);
            for (self.pointers.items) |at| {
                const holder = self.segmentOf(at);
                if (holder) |index| if (!kept[index] or !placed[index]) continue;
                const moved_at = self.movedOffset(at, new_start);
                const moved_target = self.movedOffset(self.pointerTarget(at), new_start);
                std.mem.writeInt(u64, block.items[moved_at..][0..8], moved_target, .little);
                try pointers.append(gpa, @intCast(moved_at));
            }
            std.mem.sortUnstable(u32, pointers.items, {}, std.sort.asc(u32));
            self.block.deinit(gpa);
            self.block = block;
            self.pointers.deinit(gpa);
            self.pointers = pointers;
        }

        /// Append to `order` the kept allocation of each allocation the
        /// pointer words `pointers` point at that `placed` does not hold yet.
        fn appendReached(self: *const Self, pointers: []const u32, by_payload: *const std.AutoHashMapUnmanaged(usize, u32), class: []const u32, class_first: []const u32, placed: []bool, order: *std.ArrayList(u32)) Allocator.Error!void {
            for (pointers) |at| {
                const target = by_payload.get(self.pointerTarget(at)) orelse continue;
                const kept = class_first[class[target]];
                if (placed[kept]) continue;
                placed[kept] = true;
                try order.append(self.allocator, kept);
            }
        }

        /// The sorted pointer words in `[start, end)`.
        fn pointersIn(self: *const Self, start: usize, end: usize) []const u32 {
            const all = self.pointers.items;
            const first = std.sort.lowerBound(u32, all, @as(u32, @intCast(start)), orderU32);
            const last = std.sort.lowerBound(u32, all, @as(u32, @intCast(end)), orderU32);
            return all[first..last];
        }

        fn orderU32(context: u32, item: u32) std.math.Order {
            return std.math.order(context, item);
        }

        /// The block offset the pointer word at `at` holds.
        fn pointerTarget(self: *const Self, at: u32) usize {
            return @intCast(std.mem.readInt(u64, self.block.items[at..][0..8], .little));
        }

        /// The segment holding offset `offset`, or null in the root.
        fn segmentOf(self: *const Self, offset: usize) ?usize {
            const segments = self.segments.items;
            if (segments.len == 0 or offset < segments[0].start) return null;
            var low: usize = 0;
            var high: usize = segments.len;
            while (high - low > 1) {
                const mid = (low + high) / 2;
                if (segments[mid].start <= offset) low = mid else high = mid;
            }
            return low;
        }

        /// Where `offset` is once the kept segments are laid out again at
        /// `new_start`, a merged segment's being its kept equal's.
        fn movedOffset(self: *const Self, offset: usize, new_start: []const usize) usize {
            const index = self.segmentOf(offset) orelse return offset;
            return new_start[index] + (offset - self.segments.items[index].start);
        }
    };
}

fn invariant(comptime message: []const u8) noreturn {
    compilerInvariant("frozen graph invariant violated: {s}", .{message});
}
