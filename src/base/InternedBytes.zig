//! Shared byte-interning index for compiler string stores.
//!
//! This owns the duplicate-prone hashing, probing, lookup, and resize algorithm.
//! Storage owners provide a comptime policy for id encoding, text lookup, and
//! fresh-entry append layout.

const std = @import("std");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const SafeList = collections.SafeList;

const default_hash_seed: u64 = 0;
const max_load_numerator: usize = 4;
const max_load_denominator: usize = 5;

/// Hashes interned bytes with the compiler's fixed deterministic string seed.
pub fn hash(bytes: []const u8) u64 {
    return std.hash.Wyhash.hash(default_hash_seed, bytes);
}

/// Probe result containing either an existing id or the empty slot for insertion.
pub fn FindResult(comptime Id: type) type {
    return struct {
        idx: ?Id,
        slot: usize,
    };
}

/// Policy-specialized open-addressed byte dedup index.
pub fn Index(comptime Policy: type) type {
    return struct {
        cells: SafeList(Policy.Cell) = .{},
        fingerprints: SafeList(u8) = .{},
        len: u32 = 0,

        const Self = @This();
        const use_fingerprints = if (@hasDecl(Policy, "use_fingerprints")) Policy.use_fingerprints else false;

        pub const Find = FindResult(Policy.Id);

        pub fn fromCells(cells: SafeList(Policy.Cell), len: u32) Self {
            return .{ .cells = cells, .fingerprints = .{}, .len = len };
        }

        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.cells.deinit(gpa);
            if (use_fingerprints) self.fingerprints.deinit(gpa);
        }

        pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
            var cells = try self.cells.clone(gpa);
            errdefer cells.deinit(gpa);

            var fingerprints = if (use_fingerprints)
                try self.fingerprints.clone(gpa)
            else
                SafeList(u8){};
            errdefer if (use_fingerprints) fingerprints.deinit(gpa);

            return .{
                .cells = cells,
                .fingerprints = fingerprints,
                .len = self.len,
            };
        }

        pub fn count(self: *const Self) u32 {
            return self.len;
        }

        pub fn lookup(self: *const Self, owner: anytype, bytes: []const u8) ?Policy.Id {
            return self.findStringOrSlot(owner, Policy.hash(bytes), bytes).idx;
        }

        pub fn contains(self: *const Self, owner: anytype, bytes: []const u8) bool {
            return self.lookup(owner, bytes) != null;
        }

        pub fn insert(self: *Self, owner: anytype, gpa: Allocator, bytes: []const u8) Allocator.Error!Policy.Id {
            const bytes_hash = Policy.hash(bytes);
            const found = self.findStringOrSlot(owner, bytes_hash, bytes);
            if (found.idx) |existing| return existing;

            var table_changed = false;
            if (self.cells.len() == 0) {
                try self.allocate(gpa, Policy.initial_index_capacity);
                table_changed = true;
            } else if (wouldExceedMaxLoad(@as(usize, self.len) + 1, @intCast(self.cells.len()))) {
                try self.resize(owner, gpa);
                table_changed = true;
            }

            const id = try Policy.appendEntry(owner, gpa, bytes);
            self.len += 1;

            const slot = if (table_changed)
                self.findStringOrSlot(owner, bytes_hash, bytes).slot
            else
                found.slot;
            self.cells.items.items[slot] = Policy.cellForId(id);
            if (use_fingerprints) self.fingerprints.items.items[slot] = fingerprint(bytes_hash);
            return id;
        }

        pub fn findStringOrSlot(self: *const Self, owner: anytype, bytes_hash: u64, bytes: []const u8) Find {
            const table_size: usize = @intCast(self.cells.len());
            if (table_size == 0) return .{ .idx = null, .slot = 0 };

            const needle_fingerprint = fingerprint(bytes_hash);
            var slot = @as(usize, @intCast(bytes_hash & @as(u64, @intCast(table_size - 1))));
            while (true) {
                const cell = self.cells.items.items[slot];
                if (cell == Policy.empty_cell) return .{ .idx = null, .slot = slot };

                if (use_fingerprints and self.fingerprints.items.items[slot] != needle_fingerprint) {
                    slot = (slot + 1) & (table_size - 1);
                    continue;
                }

                const id = Policy.idFromCell(cell);
                if (std.mem.eql(u8, bytes, Policy.textForId(owner, id))) {
                    return .{ .idx = id, .slot = slot };
                }

                slot = (slot + 1) & (table_size - 1);
            }
        }

        pub fn resize(self: *Self, owner: anytype, gpa: Allocator) Allocator.Error!void {
            const old_cells = self.cells;
            const old_fingerprints = self.fingerprints;
            const new_size = @max(Policy.initial_index_capacity, @as(usize, @intCast(old_cells.len())) * 2);

            try self.allocate(gpa, new_size);

            for (old_cells.items.items) |cell| {
                if (cell == Policy.empty_cell) continue;
                const id = Policy.idFromCell(cell);
                const text = Policy.textForId(owner, id);
                const text_hash = Policy.hash(text);
                const slot = self.findStringOrSlot(owner, text_hash, text).slot;
                self.cells.items.items[slot] = cell;
                if (use_fingerprints) self.fingerprints.items.items[slot] = fingerprint(text_hash);
            }

            @constCast(&old_cells).deinit(gpa);
            if (use_fingerprints) @constCast(&old_fingerprints).deinit(gpa);
        }

        fn allocate(self: *Self, gpa: Allocator, requested_size: usize) Allocator.Error!void {
            const table_size = powerOfTwoTableSize(requested_size);
            var cells = try SafeList(Policy.Cell).initCapacity(gpa, table_size);
            errdefer cells.deinit(gpa);
            try cells.items.ensureTotalCapacityPrecise(gpa, table_size);
            cells.items.items.len = table_size;
            @memset(cells.items.items, Policy.empty_cell);

            var fingerprints = SafeList(u8){};
            if (use_fingerprints) {
                fingerprints = try SafeList(u8).initCapacity(gpa, table_size);
                errdefer fingerprints.deinit(gpa);
                try fingerprints.items.ensureTotalCapacityPrecise(gpa, table_size);
                fingerprints.items.items.len = table_size;
                @memset(fingerprints.items.items, 0);
            }

            self.cells = cells;
            self.fingerprints = fingerprints;
        }
    };
}

fn wouldExceedMaxLoad(new_len: usize, table_size: usize) bool {
    return new_len * max_load_denominator >= table_size * max_load_numerator;
}

fn powerOfTwoTableSize(requested_size: usize) usize {
    const min_size = @max(requested_size, 1);
    return std.math.ceilPowerOfTwo(usize, min_size) catch unreachable;
}

fn fingerprint(bytes_hash: u64) u8 {
    return @truncate(bytes_hash >> 56);
}

const testing = std.testing;

const CollisionId = enum(u32) {
    none = 0,
    _,
};

const CollisionOwner = struct {
    bytes: std.ArrayList(u8) = .empty,
    ranges: std.ArrayList(Range) = .empty,

    const Range = struct { start: u32, len: u32 };

    fn deinit(self: *CollisionOwner, gpa: Allocator) void {
        self.bytes.deinit(gpa);
        self.ranges.deinit(gpa);
    }

    fn textForId(self: *const CollisionOwner, id: CollisionId) []const u8 {
        const range = self.ranges.items[@intFromEnum(id) - 1];
        return self.bytes.items[range.start .. range.start + range.len];
    }

    fn append(self: *CollisionOwner, gpa: Allocator, bytes: []const u8) Allocator.Error!CollisionId {
        const id: CollisionId = @enumFromInt(self.ranges.items.len + 1);
        const start: u32 = @intCast(self.bytes.items.len);
        try self.bytes.appendSlice(gpa, bytes);
        try self.ranges.append(gpa, .{ .start = start, .len = @intCast(bytes.len) });
        return id;
    }
};

const ConstantHashPolicy = struct {
    pub const Id = CollisionId;
    pub const Cell = CollisionId;
    pub const empty_cell: Cell = .none;
    pub const initial_index_capacity: usize = 2;
    pub const use_fingerprints = true;

    pub fn cellForId(id: Id) Cell {
        return id;
    }

    pub fn idFromCell(cell: Cell) Id {
        return cell;
    }

    pub fn textForId(owner: *const CollisionOwner, id: Id) []const u8 {
        return owner.textForId(id);
    }

    pub fn appendEntry(owner: *CollisionOwner, gpa: Allocator, bytes: []const u8) Allocator.Error!Id {
        return owner.append(gpa, bytes);
    }

    pub fn entryCount(_: *const CollisionOwner, index: *const Index(ConstantHashPolicy)) u32 {
        return index.len;
    }

    pub fn hash(_: []const u8) u64 {
        return 0;
    }
};

test "InternedBytes.Index resolves duplicates under full collision" {
    const gpa = testing.allocator;

    var owner = CollisionOwner{};
    defer owner.deinit(gpa);

    var index = Index(ConstantHashPolicy){};
    defer index.deinit(gpa);

    const alpha = try index.insert(&owner, gpa, "alpha");
    const beta = try index.insert(&owner, gpa, "beta");
    const gamma = try index.insert(&owner, gpa, "gamma");

    try testing.expectEqual(alpha, try index.insert(&owner, gpa, "alpha"));
    try testing.expectEqual(beta, index.lookup(&owner, "beta").?);
    try testing.expectEqual(gamma, index.lookup(&owner, "gamma").?);
    try testing.expectEqual(@as(?CollisionId, null), index.lookup(&owner, "delta"));
    try testing.expectEqualStrings("alpha", owner.textForId(alpha));
    try testing.expectEqualStrings("beta", owner.textForId(beta));
    try testing.expectEqualStrings("gamma", owner.textForId(gamma));
}
