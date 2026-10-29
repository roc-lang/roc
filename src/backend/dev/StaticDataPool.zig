//! Session-lifetime static data for hot-reload run images.
//!
//! Static data is immortal: its refcount is `REFCOUNT_STATIC_DATA`, so no
//! increment, decrement, or drop ever observes when a value stops pointing at
//! it. A value the host keeps can point into the static data of the image that
//! created it long after that image stops being current, so a hot-reload
//! session keeps the static data of every image the host shim accepted for the
//! rest of the session.
//!
//! Each image writes the static data entries it does not find in the pool into
//! one page-aligned chunk at the end of its allocation. Once the host shim
//! accepts the image, the compiler parent links that chunk into the session
//! pool and never reclaims it, while the rest of the image stays reclaimable.
//! A later image reuses a pool entry whose content is exactly its own instead
//! of writing another copy, so the pool grows only with distinct content.
//!
//! An entry whose relocations reach a function pointer names its own image's
//! code, so no other image can reuse it. Its image records that its static data
//! references code, and the host shim never releases such an image.

const std = @import("std");
const base = @import("base");
const static_data = @import("static_data");

const Allocator = std.mem.Allocator;
const StaticDataExport = static_data.StaticDataExport;
const StaticDataRelocation = static_data.StaticDataRelocation;

/// Magic number identifying a static pool chunk ("ROCP" in little-endian bytes).
pub const CHUNK_MAGIC: u32 = 0x50434F52;

/// Header at the start of one image's static pool chunk. Every offset is
/// relative to the base of the shared mapping.
pub const Chunk = extern struct {
    magic: u32 = CHUNK_MAGIC,
    _padding: u32 = 0,
    /// The next older chunk in the session pool, or zero for the oldest. Only
    /// the compiler parent writes it, when it links this chunk into the pool.
    next_chunk_offset: u64 = 0,
    entries_offset: u64,
    entry_count: u64,
    relocations_offset: u64,
    relocation_count: u64,
};

/// One reusable static data entry in a chunk.
pub const Entry = extern struct {
    /// Content hash; see `Plan.init`.
    hash: u64,
    /// Offset of the entry's first byte.
    data_offset: u64,
    len: u64,
    symbol_offset: u64,
    alignment: u32,
    relocation_count: u32,
    /// Index of the entry's first relocation in its chunk's relocation table.
    /// An entry's relocations are sorted by `offset`.
    first_relocation: u64,
};

/// One address relocation inside a reusable static data entry.
pub const Relocation = extern struct {
    offset: u64,
    addend: i64,
    /// `Entry.data_offset` of the entry this relocation points into.
    target_data_offset: u64,
};

/// Errors raised while planning static data placement.
pub const Error = Allocator.Error || error{
    InvalidStaticDataAlignment,
    InvalidStaticDataRelocation,
    InvalidStaticPool,
    UnsupportedStaticDataRelocation,
};

/// The session pool as seen through one process's shared mapping.
pub const Pool = struct {
    mapping: []const u8,
    /// Offset of the newest chunk, or zero for an empty pool.
    head_offset: usize,

    pub const empty: Pool = .{ .mapping = &.{}, .head_offset = 0 };
};

/// Where one static data export lives in an image.
pub const Placement = union(enum) {
    /// Written into this image's data section at `data_offset`. A `listed`
    /// entry is recorded in the image's chunk so later images can reuse it.
    local: struct { data_offset: usize, listed: bool },
    /// Reused from the pool: the mapping offset of the entry's first byte.
    pooled: usize,
};

const none: u32 = std.math.maxInt(u32);

/// Placement decisions for one image's static data exports.
pub const Plan = struct {
    allocator: Allocator,
    placements: []Placement,
    /// Content hash of each export that can be pooled. This and the
    /// classification fields through `sorted_relocations` are empty unless
    /// `pooling`.
    hashes: []u64,
    /// Whether each export can be pooled: its relocations reach no code.
    poolable: []bool,
    /// Address relocation target export of each relocation, flattened in
    /// export order, or `none` for a function pointer.
    targets: []u32,
    target_starts: []u32,
    /// Exports with every address target before its sources.
    order: []u32,
    /// Each export's relocation indices sorted by offset, flattened like `targets`.
    sorted_relocations: []u32,
    /// Whether this image records its reusable entries in a chunk.
    pooling: bool,
    /// Whether any export's static data names generated code.
    static_code_references: bool,
    local_data_len: usize = 0,
    max_local_alignment: usize = 1,
    local_relocation_count: usize = 0,
    listed_count: usize = 0,
    listed_relocation_count: usize = 0,

    /// Lay every export out locally. With `pooling`, also classify each export
    /// and give each one whose relocations reach no code a content hash built
    /// from its length, alignment, symbol offset, the bytes outside its pointer
    /// slots, and each relocation's offset, addend, and target hash.
    pub fn init(allocator: Allocator, exports: []const StaticDataExport, pooling: bool) Error!Plan {
        var plan = Plan{
            .allocator = allocator,
            .placements = &.{},
            .hashes = &.{},
            .poolable = &.{},
            .targets = &.{},
            .target_starts = &.{},
            .order = &.{},
            .sorted_relocations = &.{},
            .pooling = pooling,
            .static_code_references = false,
        };
        errdefer plan.deinit();

        if (exports.len >= none) return error.UnsupportedStaticDataRelocation;
        plan.placements = try allocator.alloc(Placement, exports.len);
        for (plan.placements) |*placement| placement.* = .{ .local = .{ .data_offset = 0, .listed = false } };
        for (exports) |data_export| {
            for (data_export.relocations) |relocation| {
                if (relocation.kind == .function_pointer) plan.static_code_references = true;
            }
        }
        if (!pooling) {
            try plan.layOutLocals(exports);
            return plan;
        }

        plan.hashes = try allocator.alloc(u64, exports.len);
        @memset(plan.hashes, 0);
        plan.poolable = try allocator.alloc(bool, exports.len);
        plan.target_starts = try allocator.alloc(u32, exports.len + 1);

        var relocation_count: usize = 0;
        for (exports) |data_export| relocation_count += data_export.relocations.len;
        if (relocation_count >= none) return error.UnsupportedStaticDataRelocation;
        plan.targets = try allocator.alloc(u32, relocation_count);
        plan.sorted_relocations = try allocator.alloc(u32, relocation_count);

        var names = std.StringHashMapUnmanaged(u32).empty;
        defer names.deinit(allocator);
        for (exports, 0..) |data_export, i| {
            const entry = try names.getOrPut(allocator, data_export.symbol_name);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(i);
        }

        var cursor: u32 = 0;
        for (exports, 0..) |data_export, i| {
            plan.target_starts[i] = cursor;
            for (data_export.relocations, 0..) |relocation, r| {
                plan.targets[cursor] = switch (relocation.kind) {
                    .function_pointer => none,
                    .address => switch (relocation.target) {
                        .data_symbol => |id| blk: {
                            const index = @backingInt(id);
                            if (index >= exports.len) return error.UnsupportedStaticDataRelocation;
                            break :blk index;
                        },
                        .named => names.get(relocation.target_symbol_name) orelse return error.UnsupportedStaticDataRelocation,
                    },
                };
                plan.sorted_relocations[cursor] = @intCast(r);
                cursor += 1;
            }
            const sorted = plan.sorted_relocations[plan.target_starts[i]..cursor];
            std.mem.sort(u32, sorted, data_export.relocations, relocationOffsetLessThan);
            var slot_end: u64 = 0;
            for (sorted) |r| {
                const offset = data_export.relocations[r].offset;
                if (offset < slot_end or offset > data_export.bytes.len or @sizeOf(usize) > data_export.bytes.len - offset) {
                    return error.InvalidStaticDataRelocation;
                }
                slot_end = offset + @sizeOf(usize);
            }
        }
        plan.target_starts[exports.len] = cursor;

        plan.order = try postOrder(allocator, exports.len, plan.targets, plan.target_starts);

        for (plan.order) |i| {
            var poolable = true;
            for (plan.targets[plan.target_starts[i]..plan.target_starts[i + 1]]) |target| {
                if (target == none or !plan.poolable[target]) poolable = false;
            }
            plan.poolable[i] = poolable;
            if (poolable) plan.hashes[i] = plan.contentHash(exports, i);
        }

        try plan.layOutLocals(exports);
        return plan;
    }

    pub fn deinit(self: *Plan) void {
        self.allocator.free(self.placements);
        self.allocator.free(self.hashes);
        self.allocator.free(self.poolable);
        self.allocator.free(self.targets);
        self.allocator.free(self.target_starts);
        self.allocator.free(self.order);
        self.allocator.free(self.sorted_relocations);
        self.* = undefined;
    }

    /// Reuse every pool entry whose content is exactly an export's. The image
    /// will occupy mapping offsets `[image_start, image_end)`; an entry is
    /// reused only when the whole span from the image to the entry fits a
    /// signed 32-bit displacement, which every code-to-data relocation encoding
    /// reaches.
    pub fn reusePool(
        self: *Plan,
        exports: []const StaticDataExport,
        pool: Pool,
        image_start: usize,
        image_end: usize,
    ) Error!void {
        if (!self.pooling) base.invariant("static data invariant violated: pool reuse planned for an image that does not pool", .{});
        if (pool.head_offset == 0) return;

        var index = try PoolIndex.build(self.allocator, pool);
        defer index.deinit(self.allocator);

        for (self.order) |i| {
            if (!self.poolable[i]) continue;
            const reused = index.find(self, exports, i, pool.mapping, image_start, image_end) orelse continue;
            self.placements[i] = .{ .pooled = reused };
        }
        try self.layOutLocals(exports);
    }

    fn layOutLocals(self: *Plan, exports: []const StaticDataExport) Error!void {
        self.local_data_len = 0;
        self.max_local_alignment = 1;
        self.local_relocation_count = 0;
        self.listed_count = 0;
        self.listed_relocation_count = 0;
        for (exports, self.placements, 0..) |data_export, *placement, i| {
            if (placement.* == .pooled) continue;
            const alignment = exportAlignment(data_export);
            if (!std.math.isPowerOfTwo(alignment)) return error.InvalidStaticDataAlignment;
            self.max_local_alignment = @max(self.max_local_alignment, alignment);
            const data_offset = std.mem.alignForward(usize, self.local_data_len, alignment);
            self.local_data_len = data_offset + data_export.bytes.len;
            self.local_relocation_count += data_export.relocations.len;
            const listed = self.pooling and self.poolable[i];
            if (listed) {
                self.listed_count += 1;
                self.listed_relocation_count += data_export.relocations.len;
            }
            placement.* = .{ .local = .{ .data_offset = data_offset, .listed = listed } };
        }
    }

    /// Relocation indices of export `i`, sorted by offset.
    pub fn sortedRelocations(self: *const Plan, i: usize) []const u32 {
        return self.sorted_relocations[self.target_starts[i]..self.target_starts[i + 1]];
    }

    /// Target export of relocation `r` of export `i`, for an address relocation.
    pub fn relocationTarget(self: *const Plan, i: usize, r: usize) u32 {
        const target = self.targets[self.target_starts[i] + r];
        if (target == none) base.invariant("static data invariant violated: relocation {d} of export {d} is not an address relocation", .{ r, i });
        return target;
    }

    fn contentHash(self: *const Plan, exports: []const StaticDataExport, i: usize) u64 {
        const data_export = exports[i];
        var hasher = std.hash.Wyhash.init(0);
        hashInt(&hasher, @as(u64, data_export.bytes.len));
        hashInt(&hasher, @as(u64, exportAlignment(data_export)));
        hashInt(&hasher, @as(u64, data_export.symbol_offset));
        hashInt(&hasher, @as(u64, data_export.relocations.len));
        var cursor: usize = 0;
        for (self.sortedRelocations(i)) |r| {
            const relocation = data_export.relocations[r];
            hashInt(&hasher, relocation.offset);
            hashInt(&hasher, @as(u64, @bitCast(relocation.addend)));
            hashInt(&hasher, self.hashes[self.relocationTarget(i, r)]);
            const slot: usize = @intCast(relocation.offset);
            hasher.update(data_export.bytes[cursor..slot]);
            cursor = slot + @sizeOf(usize);
        }
        hasher.update(data_export.bytes[cursor..]);
        return hasher.final();
    }
};

/// The alignment an export's bytes are placed at; zero means byte-aligned.
pub fn exportAlignment(data_export: StaticDataExport) u32 {
    return if (data_export.alignment == 0) 1 else data_export.alignment;
}

fn hashInt(hasher: *std.hash.Wyhash, value: u64) void {
    hasher.update(std.mem.asBytes(&std.mem.nativeToLittle(u64, value)));
}

fn relocationOffsetLessThan(relocations: []const StaticDataRelocation, a: u32, b: u32) bool {
    return relocations[a].offset < relocations[b].offset;
}

/// Order exports so every address target precedes the exports pointing at it.
fn postOrder(allocator: Allocator, count: usize, targets: []const u32, target_starts: []const u32) Allocator.Error![]u32 {
    const order = try allocator.alloc(u32, count);
    errdefer allocator.free(order);
    const state = try allocator.alloc(enum(u2) { unvisited, open, done }, count);
    defer allocator.free(state);
    @memset(state, .unvisited);

    const Frame = struct { node: u32, next_target: u32 };
    var stack = std.ArrayList(Frame).empty;
    defer stack.deinit(allocator);

    var emitted: usize = 0;
    for (0..count) |root| {
        if (state[root] != .unvisited) continue;
        state[root] = .open;
        try stack.append(allocator, .{ .node = @intCast(root), .next_target = target_starts[root] });
        while (stack.items.len > 0) {
            const frame = &stack.items[stack.items.len - 1];
            if (frame.next_target == target_starts[frame.node + 1]) {
                state[frame.node] = .done;
                order[emitted] = frame.node;
                emitted += 1;
                _ = stack.pop();
                continue;
            }
            const target = targets[frame.next_target];
            frame.next_target += 1;
            if (target == none) continue;
            switch (state[target]) {
                .done => {},
                .open => base.invariant("static data invariant violated: export {d} reaches itself through address relocations", .{target}),
                .unvisited => {
                    state[target] = .open;
                    try stack.append(allocator, .{ .node = target, .next_target = target_starts[target] });
                },
            }
        }
    }
    if (emitted != count) base.invariant("static data invariant violated: ordered {d} of {d} exports", .{ emitted, count });
    return order;
}

const PoolIndex = struct {
    const Candidate = struct {
        entry: *align(1) const Entry,
        relocations: []align(1) const Relocation,
        next: u32,
    };

    candidates: std.ArrayList(Candidate) = .empty,
    heads: std.AutoHashMapUnmanaged(u64, u32) = .empty,

    fn deinit(self: *PoolIndex, allocator: Allocator) void {
        self.candidates.deinit(allocator);
        self.heads.deinit(allocator);
    }

    fn build(allocator: Allocator, pool: Pool) Error!PoolIndex {
        var index = PoolIndex{};
        errdefer index.deinit(allocator);

        // Every chunk occupies at least its own header, which bounds a walk
        // over a corrupted link.
        var remaining_chunks = pool.mapping.len / @sizeOf(Chunk);
        var chunk_offset = pool.head_offset;
        while (chunk_offset != 0) {
            if (remaining_chunks == 0) return error.InvalidStaticPool;
            remaining_chunks -= 1;
            const chunk = try mappedSlice(Chunk, pool.mapping, chunk_offset, 1);
            if (chunk[0].magic != CHUNK_MAGIC) return error.InvalidStaticPool;
            const entries = try mappedSlice(Entry, pool.mapping, chunk[0].entries_offset, chunk[0].entry_count);
            const relocations = try mappedSlice(Relocation, pool.mapping, chunk[0].relocations_offset, chunk[0].relocation_count);
            try index.candidates.ensureUnusedCapacity(allocator, entries.len);
            for (entries) |*entry| {
                if (entry.first_relocation > relocations.len or entry.relocation_count > relocations.len - entry.first_relocation) {
                    return error.InvalidStaticPool;
                }
                _ = try mappedSlice(u8, pool.mapping, entry.data_offset, entry.len);
                const first: usize = @intCast(entry.first_relocation);
                const head = try index.heads.getOrPut(allocator, entry.hash);
                const next = if (head.found_existing) head.value_ptr.* else none;
                if (index.candidates.items.len >= none) return error.InvalidStaticPool;
                head.value_ptr.* = @intCast(index.candidates.items.len);
                index.candidates.appendAssumeCapacity(.{
                    .entry = entry,
                    .relocations = relocations[first..][0..entry.relocation_count],
                    .next = next,
                });
            }
            chunk_offset = std.math.cast(usize, chunk[0].next_chunk_offset) orelse return error.InvalidStaticPool;
        }
        return index;
    }

    /// The mapping offset of a pool entry whose content is exactly export `i`'s.
    fn find(
        self: *const PoolIndex,
        plan: *const Plan,
        exports: []const StaticDataExport,
        i: usize,
        mapping: []const u8,
        image_start: usize,
        image_end: usize,
    ) ?usize {
        const data_export = exports[i];
        const relocations = data_export.relocations;
        for (plan.sortedRelocations(i)) |r| {
            if (plan.placements[plan.relocationTarget(i, r)] != .pooled) return null;
        }

        var candidate_index = self.heads.get(plan.hashes[i]) orelse return null;
        while (candidate_index != none) {
            const candidate = self.candidates.items[candidate_index];
            candidate_index = candidate.next;
            const entry = candidate.entry;
            if (entry.len != data_export.bytes.len or
                entry.alignment != exportAlignment(data_export) or
                entry.symbol_offset != data_export.symbol_offset or
                entry.relocation_count != relocations.len)
            {
                continue;
            }
            const data_offset: usize = @intCast(entry.data_offset);
            const span_start = @min(image_start, data_offset);
            const span_end = @max(image_end, data_offset + data_export.bytes.len);
            if (span_end - span_start > std.math.maxInt(i32)) continue;

            const entry_bytes = mapping[data_offset..][0..data_export.bytes.len];
            var cursor: usize = 0;
            var equal = true;
            for (plan.sortedRelocations(i), candidate.relocations) |r, pooled_relocation| {
                const relocation = relocations[r];
                const target_offset = plan.placements[plan.relocationTarget(i, r)].pooled;
                if (pooled_relocation.offset != relocation.offset or
                    pooled_relocation.addend != relocation.addend or
                    pooled_relocation.target_data_offset != target_offset)
                {
                    equal = false;
                    break;
                }
                const slot: usize = @intCast(relocation.offset);
                if (!std.mem.eql(u8, entry_bytes[cursor..slot], data_export.bytes[cursor..slot])) {
                    equal = false;
                    break;
                }
                cursor = slot + @sizeOf(usize);
            }
            if (equal and std.mem.eql(u8, entry_bytes[cursor..], data_export.bytes[cursor..])) return data_offset;
        }
        return null;
    }
};

fn mappedSlice(comptime T: type, mapping: []const u8, offset: u64, count: u64) Error![]align(1) const T {
    const start = std.math.cast(usize, offset) orelse return error.InvalidStaticPool;
    const len = std.math.cast(usize, count) orelse return error.InvalidStaticPool;
    const byte_len = std.math.mul(usize, len, @sizeOf(T)) catch return error.InvalidStaticPool;
    if (start > mapping.len or byte_len > mapping.len - start) return error.InvalidStaticPool;
    const ptr: [*]align(1) const T = @ptrCast(mapping.ptr + start);
    return ptr[0..len];
}
