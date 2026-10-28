//! Bounded catalog with positional writes and retained read mappings.
//!
//! Four independent crit-bit indexes share fixed-size records. A key has a
//! fixed width, so a lookup visits at most key_bits branch records. Mutations
//! replace a bounded set of records through one redo transaction, never a
//! growing WAL. The caller holds the shard's kernel lock through recovery and
//! the transaction. No stored pointer is trusted before checking its bounds.

const std = @import("std");
const Io = std.Io;
const Dir = Io.Dir;

/// Bytes per catalog record, including its checksum.
pub const record_size = 512;
/// Fixed-width key ordered lexicographically by the crit-bit indexes.
pub const Key = [64]u8;
/// Opaque fixed-width payload stored in a leaf.
pub const Value = [384]u8;
/// Maximum branch depth for any lookup.
pub const key_bits = @bitSizeOf(Key);
/// Maximum number of changed records in a redo transaction.
pub const max_changes = 24;
/// Physical record number; zero denotes a missing node.
pub const RecordId = u64;
/// Independent indexes sharing one record allocator and transaction.
pub const Tree = enum(u2) { artifacts, recency, owners, spare };
const Record = [record_size]u8;
const magic = "ROCIDX01";
const transaction_magic = "ROCTXN01";
const redo_size = 24 + record_size + max_changes * (8 + record_size);

/// Storage failures and invalid on-disk catalog data.
pub const Error = error{ IoFailure, CorruptCatalog, CapacityExceeded };

/// Observable work, including recovery. Tests assert bounds on actual I/O.
pub const Counters = struct {
    reads: usize = 0,
    mapped_reads: usize = 0,
    writes: usize = 0,
    read_bytes: usize = 0,
    written_bytes: usize = 0,
};

/// Index roots, allocation state, and store accounting persisted in record zero.
pub const Header = struct {
    roots: [4]RecordId = @splat(0),
    next: RecordId = 1,
    free: RecordId = 0,
    sequence: u64 = 0,
    /// Store-owned accounting, allocation generation, and reconciliation data.
    user: [16]u64 = @splat(0),

    fn encode(self: Header) Record {
        var bytes: Record = @splat(0);
        @memcpy(bytes[8..16], magic);
        for (self.roots, 0..) |root, i| put(u64, &bytes, 16 + i * 8, root);
        put(u64, &bytes, 48, self.next);
        put(u64, &bytes, 56, self.free);
        put(u64, &bytes, 64, self.sequence);
        for (self.user, 0..) |value, i| put(u64, &bytes, 72 + i * 8, value);
        seal(&bytes);
        return bytes;
    }

    fn decode(bytes: *const Record) Error!Header {
        if (!valid(bytes) or !std.mem.eql(u8, bytes[8..16], magic)) return error.CorruptCatalog;
        var result = Header{};
        for (&result.roots, 0..) |*root, i| root.* = get(u64, bytes, 16 + i * 8);
        result.next = get(u64, bytes, 48);
        result.free = get(u64, bytes, 56);
        result.sequence = get(u64, bytes, 64);
        for (&result.user, 0..) |*value, i| value.* = get(u64, bytes, 72 + i * 8);
        if (result.next == 0 or result.next > std.math.maxInt(u64) / record_size) return error.CorruptCatalog;
        if (result.free >= result.next) return error.CorruptCatalog;
        for (result.roots) |root| if (root >= result.next) return error.CorruptCatalog;
        return result;
    }
};

/// Decoded branch, leaf, or free-list record.
pub const Node = struct {
    pub const Kind = enum(u8) { free, leaf, branch };
    kind: Kind = .free,
    bit: u16 = 0,
    children: [2]RecordId = @splat(0),
    key: Key = @splat(0),
    value: Value = @splat(0),

    fn encode(self: Node) Record {
        var bytes: Record = @splat(0);
        bytes[8] = @backingInt(self.kind);
        put(u16, &bytes, 10, self.bit);
        for (self.children, 0..) |child, i| put(u64, &bytes, 16 + i * 8, child);
        @memcpy(bytes[32..96], &self.key);
        @memcpy(bytes[96..480], &self.value);
        seal(&bytes);
        return bytes;
    }

    fn decode(bytes: *const Record, next: RecordId) Error!Node {
        if (!valid(bytes)) return error.CorruptCatalog;
        const kind = std.enums.fromInt(Kind, bytes[8]) orelse return error.CorruptCatalog;
        const result: Node = .{
            .kind = kind,
            .bit = get(u16, bytes, 10),
            .children = .{ get(u64, bytes, 16), get(u64, bytes, 24) },
            .key = bytes[32..96].*,
            .value = bytes[96..480].*,
        };
        for (result.children) |child| if (child >= next) return error.CorruptCatalog;
        if (kind == .branch and (result.bit >= key_bits or result.children[0] == 0 or
            result.children[1] == 0 or result.children[0] == result.children[1])) return error.CorruptCatalog;
        return result;
    }
};

/// Decoded pages retained by one catalog reader. Reopening metadata ownership
/// validates the published sequence before these pages can be read again.
pub const ReadCache = struct {
    ids: [64]RecordId = @splat(0),
    nodes: [64]Node = undefined,
    fn clear(self: *ReadCache) void {
        @memset(&self.ids, 0);
    }
    fn invalidate(self: *ReadCache, id: RecordId) void {
        const slot = id % self.ids.len;
        if (self.ids[slot] == id) self.ids[slot] = 0;
    }
};

/// Open catalog with bounded recovery and positional record access.
pub const Index = struct {
    dir: Dir,
    io: Io,
    file: Io.File,
    header: Header,
    counters: *Counters,
    read_cache: ?*ReadCache = null,
    mapped: ?Io.File.MemoryMap = null,

    /// `dir` and the shard lock are borrowed. The lock file is separate from
    /// both files that this protocol replaces or truncates.
    pub fn open(dir: Dir, io: Io, counters: *Counters) Error!Index {
        const file = dir.createFile(io, "catalog", .{ .read = true, .truncate = false }) catch return error.IoFailure;
        errdefer file.close(io);
        var result: Index = .{ .dir = dir, .io = io, .file = file, .header = .{}, .counters = counters };
        try result.recover();
        const len = file.length(io) catch return error.IoFailure;
        if (len == 0) {
            try result.writeRecord(0, (Header{}).encode());
        }
        result.header = try Header.decode(&try result.readRecord(0));
        if ((file.length(io) catch return error.IoFailure) < result.header.next * record_size) return error.CorruptCatalog;
        return result;
    }

    /// Reacquire the current published catalog under the kernel shard lock.
    /// Any external transaction invalidates the invocation's decoded pages.
    pub fn refresh(self: *Index) Error!void {
        errdefer {
            if (self.read_cache) |cache| cache.clear();
            self.dropMapping();
        }
        try self.recover();
        const current = try Header.decode(&try self.readRecord(0));
        if ((self.file.length(self.io) catch return error.IoFailure) < current.next * record_size) return error.CorruptCatalog;
        if (current.sequence != self.header.sequence) {
            if (self.read_cache) |cache| cache.clear();
            if (self.mapped) |*mapped| mapped.read(self.io) catch return error.IoFailure;
        }
        self.header = current;
    }

    fn dropMapping(self: *Index) void {
        if (self.mapped) |*mapped| mapped.destroy(self.io);
        self.mapped = null;
    }

    pub fn close(self: *Index) void {
        self.dropMapping();
        self.file.close(self.io);
    }

    fn readRecord(self: *Index, id: RecordId) Error!Record {
        var storage: Record = undefined;
        return (try self.readRecordRef(id, &storage)).*;
    }

    /// Borrow mapped bytes only while the caller owns the shard. Decoding
    /// validates those bytes directly, without first copying the whole record.
    fn readRecordRef(self: *Index, id: RecordId, storage: *Record) Error!*const Record {
        if (id > std.math.maxInt(u64) / record_size) return error.CorruptCatalog;
        if (id != 0 and self.read_cache != null) {
            if (self.mapped) |mapped| {
                if (id * record_size >= mapped.memory.len) self.dropMapping();
            }
            if (self.mapped == null) {
                const len = std.math.cast(usize, self.header.next * record_size) orelse return error.CapacityExceeded;
                self.mapped = self.file.createMemoryMap(self.io, .{ .len = len, .protection = .{ .read = true }, .populate = false }) catch return error.IoFailure;
            }
            const memory = self.mapped.?.memory;
            const offset = std.math.cast(usize, id * record_size) orelse return error.CorruptCatalog;
            if (offset > memory.len or memory.len - offset < record_size) return error.CorruptCatalog;
            self.counters.mapped_reads += 1;
            return memory[offset..][0..record_size];
        }
        self.counters.reads += 1;
        self.counters.read_bytes += record_size;
        const len = self.file.readPositionalAll(self.io, storage, id * record_size) catch return error.IoFailure;
        if (len != storage.len) return error.CorruptCatalog;
        return storage;
    }

    fn writeRecord(self: *Index, id: RecordId, bytes: Record) Error!void {
        if (self.read_cache) |cache| cache.invalidate(id);
        if (id > std.math.maxInt(u64) / record_size) return error.CorruptCatalog;
        self.counters.writes += 1;
        self.counters.written_bytes += record_size;
        self.file.writePositionalAll(self.io, &bytes, id * record_size) catch return error.IoFailure;
        if (self.mapped) |*mapped| mapped.read(self.io) catch return error.IoFailure;
    }

    pub fn node(self: *Index, id: RecordId) Error!Node {
        var storage: Node = undefined;
        return (try self.nodeRef(id, &storage)).*;
    }

    /// Borrowed only until another node read, refresh, or transaction write.
    fn nodeRef(self: *Index, id: RecordId, storage: *Node) Error!*const Node {
        if (id == 0 or id >= self.header.next) return error.CorruptCatalog;
        if (self.read_cache) |cache| {
            const slot = id % cache.ids.len;
            if (cache.ids[slot] != id) {
                var bytes: Record = undefined;
                cache.nodes[slot] = try Node.decode(try self.readRecordRef(id, &bytes), self.header.next);
                cache.ids[slot] = id;
            }
            return &cache.nodes[slot];
        }
        storage.* = try Node.decode(&try self.readRecord(id), self.header.next);
        return storage;
    }

    pub fn transaction(self: *Index) Transaction {
        return .{ .index = self, .header = self.header };
    }

    /// A redo record is validated in full before applying any of its writes.
    /// Recovery work is bounded even for a hostile length or record count.
    fn recover(self: *Index) Error!void {
        const redo = self.dir.openFile(self.io, "transaction", .{}) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return error.IoFailure,
        };
        defer redo.close(self.io);
        var bytes: [redo_size]u8 = undefined;
        self.counters.reads += 1;
        const len = redo.readPositionalAll(self.io, &bytes, 0) catch return error.IoFailure;
        self.counters.read_bytes += len;
        if (len < 24 + record_size or !std.mem.eql(u8, bytes[8..16], transaction_magic)) return error.CorruptCatalog;
        const count = get(u64, &bytes, 16);
        if (count > max_changes or len != 24 + record_size + count * (8 + record_size)) return error.CorruptCatalog;
        if (get(u64, &bytes, 0) != std.hash.Wyhash.hash(0, bytes[8..len])) return error.CorruptCatalog;
        const header = try Header.decode(bytes[24..][0..record_size]);
        var ids: [max_changes]u64 = undefined;
        for (0..@intCast(count)) |i| {
            const offset = 24 + record_size + i * (8 + record_size);
            const id = get(u64, &bytes, offset);
            if (id == 0 or id >= header.next) return error.CorruptCatalog;
            for (ids[0..i]) |previous| if (previous == id) return error.CorruptCatalog;
            ids[i] = id;
            _ = try Node.decode(bytes[offset + 8 ..][0..record_size], header.next);
        }
        for (ids[0..@intCast(count)], 0..) |id, i| {
            const offset = 24 + record_size + i * (8 + record_size) + 8;
            try self.writeRecord(id, bytes[offset..][0..record_size].*);
        }
        // Header last: an interrupted replay still has its complete redo.
        try self.writeRecord(0, header.encode());
        self.dir.deleteFile(self.io, "transaction") catch return error.IoFailure;
    }
};

/// A bounded set of node changes committed through one redo record.
pub const Transaction = struct {
    pub const Entry = struct { id: RecordId, key: Key, value: Value };
    index: *Index,
    header: Header,
    changes: [max_changes]struct { id: RecordId, node: Node } = undefined,
    count: usize = 0,
    // The shard lock excludes other writers for this transaction's lifetime.
    // Pending writes are consulted first. A bounded memo avoids reading the
    // same catalog path separately for each tree lookup/update in a transaction.
    read_ids: [64]RecordId = @splat(0),
    read_nodes: [64]Node = undefined,

    pub fn node(self: *Transaction, id: RecordId) Error!Node {
        return (try self.nodeRef(id)).*;
    }

    /// Borrow until the next node read or mutation. Traversals consume each
    /// branch before advancing, so they need not copy its leaf-sized payload.
    fn nodeRef(self: *Transaction, id: RecordId) Error!*const Node {
        if (id == 0 or id >= self.header.next) return error.CorruptCatalog;
        for (self.changes[0..self.count]) |*change| if (change.id == id) return &change.node;
        const slot = id % self.read_ids.len;
        if (self.index.read_cache != null) return self.index.nodeRef(id, &self.read_nodes[slot]);
        if (self.read_ids[slot] != id) {
            _ = try self.index.nodeRef(id, &self.read_nodes[slot]);
            self.read_ids[slot] = id;
        }
        return &self.read_nodes[slot];
    }

    fn set(self: *Transaction, id: RecordId, value: Node) void {
        for (self.changes[0..self.count]) |*change| if (change.id == id) {
            change.node = value;
            return;
        };
        // Store operations change a fixed number of records, independent of
        // tree depth. Exceeding that declared transaction bound is a bug.
        std.debug.assert(self.count < max_changes);
        self.changes[self.count] = .{ .id = id, .node = value };
        self.count += 1;
    }

    /// Update a leaf whose physical identity an earlier indexed lookup supplied.
    /// The stored key is checked before any mutation; catalog slots are untrusted.
    pub fn replaceValue(self: *Transaction, id: RecordId, key: Key, value: Value) Error!void {
        var current = try self.node(id);
        if (current.kind != .leaf or !std.mem.eql(u8, &current.key, &key)) return error.CorruptCatalog;
        current.value = value;
        self.set(id, current);
    }

    fn allocate(self: *Transaction, value: Node) Error!RecordId {
        const id = if (self.header.free != 0) blk: {
            const id = self.header.free;
            const old = try self.node(id);
            if (old.kind != .free or old.children[0] == id) return error.CorruptCatalog;
            self.header.free = old.children[0];
            break :blk id;
        } else blk: {
            if (self.header.next == std.math.maxInt(u64) / record_size) return error.CapacityExceeded;
            const id = self.header.next;
            self.header.next += 1;
            break :blk id;
        };
        self.set(id, value);
        return id;
    }

    fn release(self: *Transaction, id: RecordId) Error!void {
        self.set(id, .{ .children = .{ self.header.free, 0 } });
        self.header.free = id;
    }

    pub fn find(self: *Transaction, tree: Tree, key: Key) Error!?struct { id: RecordId, value: Value } {
        var id = self.header.roots[@backingInt(tree)];
        var lower_bit: usize = 0;
        while (id != 0) {
            const entry = try self.nodeRef(id);
            switch (entry.kind) {
                .leaf => return if (std.mem.eql(u8, &entry.key, &key)) .{ .id = id, .value = entry.value } else null,
                .free => return error.CorruptCatalog,
                .branch => {
                    if (entry.bit < lower_bit) return error.CorruptCatalog;
                    lower_bit = @as(usize, entry.bit) + 1;
                    id = entry.children[direction(key, entry.bit)];
                },
            }
        }
        return null;
    }

    /// The first leaf at or after a key. A persisted cursor stores that key,
    /// never the branch positions, which insertion and deletion can change.
    pub fn lowerBound(self: *Transaction, tree: Tree, key: Key) Error!?Entry {
        var id = self.header.roots[@backingInt(tree)];
        var alternatives: [key_bits]RecordId = undefined;
        var count: usize = 0;
        var lower_bit: usize = 0;
        while (id != 0) {
            const entry = try self.nodeRef(id);
            switch (entry.kind) {
                .free => return error.CorruptCatalog,
                .leaf => {
                    if (std.mem.order(u8, &entry.key, &key) != .lt) return .{ .id = id, .key = entry.key, .value = entry.value };
                    if (count == 0) return null;
                    count -= 1;
                    id = alternatives[count];
                    return self.minimum(id);
                },
                .branch => {
                    if (entry.bit < lower_bit) return error.CorruptCatalog;
                    // Crit-bit nodes omit common key bits. Compare that
                    // prefix before selecting a child for an absent key.
                    if (!std.mem.eql(u8, &entry.key, &key) and firstDifference(entry.key, key) < entry.bit) {
                        if (std.mem.order(u8, &entry.key, &key) == .gt) return self.minimum(id);
                        if (count == 0) return null;
                        count -= 1;
                        return self.minimum(alternatives[count]);
                    }
                    lower_bit = @as(usize, entry.bit) + 1;
                    const side = direction(key, entry.bit);
                    if (side == 0) {
                        alternatives[count] = entry.children[1];
                        count += 1;
                    }
                    id = entry.children[side];
                },
            }
        }
        return null;
    }

    fn minimum(self: *Transaction, start: RecordId) Error!?Entry {
        var id = start;
        var lower_bit: usize = 0;
        while (id != 0) {
            const entry = try self.nodeRef(id);
            switch (entry.kind) {
                .free => return error.CorruptCatalog,
                .leaf => return .{ .id = id, .key = entry.key, .value = entry.value },
                .branch => {
                    if (entry.bit < lower_bit) return error.CorruptCatalog;
                    lower_bit = @as(usize, entry.bit) + 1;
                    id = entry.children[0];
                },
            }
        }
        return null;
    }

    pub fn putValue(self: *Transaction, tree: Tree, key: Key, value: Value) Error!RecordId {
        const root = &self.header.roots[@backingInt(tree)];
        if (root.* == 0) {
            root.* = try self.allocate(.{ .kind = .leaf, .key = key, .value = value });
            return root.*;
        }
        var id = root.*;
        var lower_bit: usize = 0;
        var old: Node = undefined;
        while (true) {
            old = try self.node(id);
            if (old.kind == .leaf) break;
            if (old.kind != .branch or old.bit < lower_bit) return error.CorruptCatalog;
            lower_bit = @as(usize, old.bit) + 1;
            id = old.children[direction(key, old.bit)];
        }
        if (std.mem.eql(u8, &old.key, &key)) {
            old.value = value;
            self.set(id, old);
            return id;
        }
        const bit = firstDifference(old.key, key);
        var parent: RecordId = 0;
        var side: u1 = 0;
        id = root.*;
        lower_bit = 0;
        while (true) {
            const entry = try self.node(id);
            if (entry.kind == .leaf or (entry.kind == .branch and entry.bit >= bit)) break;
            if (entry.kind != .branch or entry.bit < lower_bit) return error.CorruptCatalog;
            lower_bit = @as(usize, entry.bit) + 1;
            parent = id;
            side = direction(key, entry.bit);
            id = entry.children[side];
        }
        const leaf = try self.allocate(.{ .kind = .leaf, .key = key, .value = value });
        var branch: Node = .{ .kind = .branch, .bit = bit, .key = key };
        const new_side = direction(key, bit);
        branch.children[new_side] = leaf;
        branch.children[new_side ^ 1] = id;
        const branch_id = try self.allocate(branch);
        if (parent == 0) root.* = branch_id else {
            var entry = try self.node(parent);
            entry.children[side] = branch_id;
            self.set(parent, entry);
        }
        return leaf;
    }

    pub fn remove(self: *Transaction, tree: Tree, key: Key) Error!bool {
        const root = &self.header.roots[@backingInt(tree)];
        var id = root.*;
        var parent: RecordId = 0;
        var grandparent: RecordId = 0;
        var parent_side: u1 = 0;
        var side: u1 = 0;
        var lower_bit: usize = 0;
        while (id != 0) {
            const entry = try self.node(id);
            if (entry.kind == .leaf) {
                if (!std.mem.eql(u8, &entry.key, &key)) return false;
                if (parent == 0) root.* = 0 else {
                    const branch = try self.node(parent);
                    const sibling = branch.children[side ^ 1];
                    if (grandparent == 0) root.* = sibling else {
                        var grand = try self.node(grandparent);
                        grand.children[parent_side] = sibling;
                        self.set(grandparent, grand);
                    }
                    try self.release(parent);
                }
                try self.release(id);
                return true;
            }
            if (entry.kind != .branch or entry.bit < lower_bit) return error.CorruptCatalog;
            lower_bit = @as(usize, entry.bit) + 1;
            grandparent = parent;
            parent_side = side;
            parent = id;
            side = direction(key, entry.bit);
            id = entry.children[side];
        }
        return false;
    }

    /// Publish redo separately so fault-injection tests can terminate between
    /// publication, individual record writes, and checkpoint completion.
    pub fn prepare(self: *Transaction) Error!void {
        if (self.header.sequence == std.math.maxInt(u64)) return error.CapacityExceeded;
        self.header.sequence += 1;
        var bytes: [redo_size]u8 = @splat(0);
        @memcpy(bytes[8..16], transaction_magic);
        put(u64, &bytes, 16, self.count);
        @memcpy(bytes[24..][0..record_size], &self.header.encode());
        for (self.changes[0..self.count], 0..) |change, i| {
            const offset = 24 + record_size + i * (8 + record_size);
            put(u64, &bytes, offset, change.id);
            @memcpy(bytes[offset + 8 ..][0..record_size], &change.node.encode());
        }
        const len = 24 + record_size + self.count * (8 + record_size);
        put(u64, &bytes, 0, std.hash.Wyhash.hash(0, bytes[8..len]));
        self.index.counters.writes += 1;
        self.index.counters.written_bytes += len;
        self.index.dir.writeFile(self.index.io, .{ .sub_path = "transaction.tmp", .data = bytes[0..len] }) catch return error.IoFailure;
        self.index.dir.rename("transaction.tmp", self.index.dir, "transaction", self.index.io) catch return error.IoFailure;
    }

    pub fn commit(self: *Transaction) Error!void {
        try self.prepare();
        try self.index.recover();
        self.index.header = self.header;
    }
};

fn direction(key: Key, bit: u16) u1 {
    return @truncate(key[bit / 8] >> @as(u3, @intCast(7 - bit % 8)));
}

fn firstDifference(a: Key, b: Key) u16 {
    for (a, b, 0..) |x, y, i| if (x != y) return @intCast(i * 8 + @clz(x ^ y));
    unreachable; // putValue handles equal keys before finding their first differing bit
}

/// Encode an integer at a fixed byte offset in little-endian order.
pub fn put(comptime T: type, bytes: []u8, offset: usize, value: T) void {
    std.mem.writeInt(T, bytes[offset..][0..@sizeOf(T)], value, .little);
}

/// Decode an integer from a fixed byte offset in little-endian order.
pub fn get(comptime T: type, bytes: []const u8, offset: usize) T {
    return std.mem.readInt(T, bytes[offset..][0..@sizeOf(T)], .little);
}

fn seal(bytes: *Record) void {
    put(u64, bytes, 0, std.hash.Wyhash.hash(0, bytes[8..]));
}

fn valid(bytes: *const Record) bool {
    return get(u64, bytes, 0) == std.hash.Wyhash.hash(0, bytes[8..]);
}

test "catalog transactions recover every partial application and reuse freed slots" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var counters = Counters{};
    var index = try Index.open(tmp.dir, io, &counters);
    defer index.close();
    var keys: [40]Key = undefined;
    for (&keys, 0..) |*key, i| {
        key.* = @splat(0);
        std.mem.writeInt(u64, key[0..8], i * 37, .big);
        var tx = index.transaction();
        _ = try tx.putValue(.artifacts, key.*, @splat(@intCast(i)));
        // Leave a durable intent and an arbitrary prefix of applied pages.
        try tx.prepare();
        for (tx.changes[0 .. i % (tx.count + 1)]) |change| try index.writeRecord(change.id, change.node.encode());
        try index.recover();
        index.header = try Header.decode(&try index.readRecord(0));
    }
    const high_water = index.header.next;
    for (keys, 0..) |key, i| {
        var tx = index.transaction();
        const found = (try tx.find(.artifacts, key)).?;
        try std.testing.expectEqual(@as(Value, @splat(@intCast(i))), found.value);
        try std.testing.expect(try tx.remove(.artifacts, key));
        try tx.commit();
    }
    for (keys) |key| {
        var tx = index.transaction();
        _ = try tx.putValue(.artifacts, key, @splat(1));
        try tx.commit();
    }
    try std.testing.expectEqual(high_water, index.header.next);
    var tx = index.transaction();
    for (keys) |key| {
        const before = counters.reads;
        const found = (try tx.lowerBound(.artifacts, key)).?;
        try std.testing.expectEqual(key, found.key);
        try std.testing.expect(counters.reads - before <= key_bits * 2 + 2);
    }
    for (0..40 * 37) |number| {
        var key: Key = @splat(0);
        std.mem.writeInt(u64, key[0..8], number, .big);
        const found = try tx.lowerBound(.artifacts, key);
        const expected = (number + 36) / 37;
        if (expected >= keys.len) try std.testing.expect(found == null) else try std.testing.expectEqual(keys[expected], found.?.key);
    }
}

test "corrupt redo is rejected before writing catalog pages" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var counters = Counters{};
    var index = try Index.open(tmp.dir, std.testing.io, &counters);
    defer index.close();
    var tx = index.transaction();
    _ = try tx.putValue(.artifacts, @splat(1), @splat(2));
    try tx.prepare();
    const redo = try tmp.dir.openFile(std.testing.io, "transaction", .{ .mode = .read_write });
    defer redo.close(std.testing.io);
    try redo.writePositionalAll(std.testing.io, "bad", 0);
    const writes = counters.writes;
    try std.testing.expectError(error.CorruptCatalog, index.recover());
    try std.testing.expectEqual(writes, counters.writes);
}

test "catalog acquisition retains paths and sees pending writes" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var counters = Counters{};
    var index = try Index.open(tmp.dir, io, &counters);
    defer index.close();
    var key: Key = @splat(1);
    {
        var tx = index.transaction();
        _ = try tx.putValue(.artifacts, key, @splat(1));
        try tx.commit();
    }
    var tx = index.transaction();
    const found = (try tx.find(.artifacts, key)).?;
    const reads = counters.reads;
    for (0..10) |_| try std.testing.expectEqual(found.value, (try tx.find(.artifacts, key)).?.value);
    try std.testing.expectEqual(reads, counters.reads);
    _ = try tx.putValue(.artifacts, key, @splat(2));
    try std.testing.expectEqual(@as(Value, @splat(2)), (try tx.find(.artifacts, key)).?.value);
    try std.testing.expect(try tx.remove(.artifacts, key));
    try std.testing.expectEqual(null, try tx.find(.artifacts, key));
    key[0] = 2;
    _ = try tx.putValue(.artifacts, key, @splat(3));
    try std.testing.expectEqual(@as(Value, @splat(3)), (try tx.find(.artifacts, key)).?.value);
    try tx.commit();
    var fresh = index.transaction();
    try std.testing.expectEqual(@as(Value, @splat(3)), (try fresh.find(.artifacts, key)).?.value);
}

test "catalog sessions reuse decoded pages and invalidate external commits and interrupted redo" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var counters = Counters{};
    var index = try Index.open(tmp.dir, io, &counters);
    defer index.close();
    var cache: ReadCache = .{};
    index.read_cache = &cache;
    const key: Key = @splat(3);
    {
        var tx = index.transaction();
        _ = try tx.putValue(.artifacts, key, @splat(1));
        try tx.commit();
    }
    {
        var tx = index.transaction();
        _ = (try tx.find(.artifacts, key)).?;
    }
    const before = counters.reads;
    for (0..10) |_| {
        var tx = index.transaction();
        try std.testing.expectEqual(@as(Value, @splat(1)), (try tx.find(.artifacts, key)).?.value);
    }
    try std.testing.expectEqual(before, counters.reads);
    {
        var other = try Index.open(tmp.dir, io, &counters);
        defer other.close();
        var tx = other.transaction();
        _ = try tx.putValue(.artifacts, key, @splat(2));
        try tx.commit();
    }
    try index.refresh();
    {
        var tx = index.transaction();
        try std.testing.expectEqual(@as(Value, @splat(2)), (try tx.find(.artifacts, key)).?.value);
    }
    {
        var other = try Index.open(tmp.dir, io, &counters);
        defer other.close();
        var tx = other.transaction();
        _ = try tx.putValue(.artifacts, key, @splat(3));
        try tx.prepare();
        // A writer died after publishing redo, before publishing the header.
    }
    try index.refresh();
    {
        var tx = index.transaction();
        try std.testing.expectEqual(@as(Value, @splat(3)), (try tx.find(.artifacts, key)).?.value);
        _ = try tx.putValue(.artifacts, key, @splat(4));
        try tx.commit();
    }
    var last = index.transaction();
    try std.testing.expectEqual(@as(Value, @splat(4)), (try last.find(.artifacts, key)).?.value);
}

test "mapped catalog validates growth and rejects truncation before reading retained memory" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var counters: Counters = .{};
    var index = try Index.open(tmp.dir, io, &counters);
    defer index.close();
    var cache: ReadCache = .{};
    index.read_cache = &cache;
    const first: Key = @splat(1);
    {
        var tx = index.transaction();
        _ = try tx.putValue(.artifacts, first, @splat(3));
        try tx.commit();
    }
    {
        var tx = index.transaction();
        _ = (try tx.find(.artifacts, first)).?;
        try std.testing.expect(index.mapped != null);
    }
    const old_len = index.mapped.?.memory.len;
    {
        var other = try Index.open(tmp.dir, io, &counters);
        defer other.close();
        for (2..24) |number| {
            var tx = other.transaction();
            _ = try tx.putValue(.artifacts, @splat(@intCast(number)), @splat(7));
            try tx.commit();
        }
    }
    try index.refresh();
    try std.testing.expect(index.mapped != null);
    {
        var tx = index.transaction();
        try std.testing.expectEqual(@as(Value, @splat(7)), (try tx.find(.artifacts, @splat(23))).?.value);
        try std.testing.expect(index.mapped.?.memory.len > old_len);
    }
    // Windows refuses to truncate a file below a mapped view, so no other
    // process can shrink the catalog under this mapping there.
    if (@import("builtin").os.tag == .windows) return;
    try index.file.setLength(io, record_size);
    try std.testing.expectError(error.CorruptCatalog, index.refresh());
    try std.testing.expect(index.mapped == null);
}
