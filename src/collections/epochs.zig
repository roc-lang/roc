//! Scratch that is reset many times: visited sets, on-stack flags, SCC
//! indices and low-links, per-procedure solver tables.
//!
//! Each entry carries a `u32` stamp and is valid only when its stamp equals
//! the current epoch, so a reset is one increment and data riding alongside a
//! stamp is never initialized: it is read only when its stamp matches, which
//! makes it a write-before-read table.
//!
//! The wrap rule is defined once, here: when advancing the epoch would
//! overflow `u32`, the stamps are cleared once and the epoch restarts at 1.
//! Stamp 0 is never an epoch, so a cleared or newly grown stamp is never
//! valid.
//!
//! A site whose reset boundary already has a monotonically increasing
//! identifier (the procedure index within a pass) supplies it as a natural
//! epoch through `resetTo` instead of counting resets.
//!
//! Capacity is retained across resets; `ensureLen` clears only the new
//! suffix of the stamps.

const std = @import("std");
const storage_work = @import("storage_work.zig");
const zeroed_pages = @import("zeroed_pages.zig");

const Allocator = std.mem.Allocator;

/// Where a set's epochs come from.
pub const EpochSource = enum {
    /// `reset` increments an internal counter.
    internal,
    /// `resetTo` installs an increasing identifier the site already has.
    natural,
};

/// How a flag-only set stores membership.
pub const SetRepresentation = enum {
    /// A stamp per entry: reset is one increment. For large domains.
    stamps,
    /// A bit per entry, cleared wholesale on reset. For small domains.
    bitset,
};

fn keyIndex(comptime Key: type, k: Key) usize {
    const info = @typeInfo(Key);
    if (info == .@"enum") return @backingInt(k);
    if (info == .int) return k;
    @compileError("epoch tables are keyed by an integer or a typed index enum, not " ++ @typeName(Key));
}

const first_epoch: u32 = 1;

/// The shared epoch rule: the current epoch, its advance, and its wrap.
const Epoch = struct {
    current: u32 = first_epoch,

    /// Advance to the next epoch. Returns true when the stamps must be
    /// cleared because the epoch wrapped.
    fn advance(self: *Epoch) bool {
        storage_work.add(.epoch_resets, 1);
        const next, const overflowed = @addWithOverflow(self.current, 1);
        if (overflowed != 0) {
            storage_work.add(.epoch_wraps, 1);
            self.current = first_epoch;
            return true;
        }
        self.current = next;
        return false;
    }

    /// Install a natural epoch `identifier`, which must exceed every
    /// identifier installed since the stamps were last cleared.
    fn install(self: *Epoch, identifier: u32) void {
        const next = identifier + first_epoch;
        std.debug.assert(next > self.current);
        storage_work.add(.epoch_resets, 1);
        self.current = next;
    }
};

fn growStamps(gpa: Allocator, stamps: *[]u32, new_len: usize) Allocator.Error!void {
    const old_len = stamps.len;
    if (new_len <= old_len) return;
    stamps.* = try gpa.realloc(stamps.*, new_len);
    @memset(stamps.*[old_len..], 0);
    storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(u32));
}

fn clearStamps(stamps: []u32) void {
    @memset(stamps, 0);
    storage_work.add(.bytes_filled, stamps.len * @sizeOf(u32));
}

/// A set of `Key`s that empties in constant time.
pub fn EpochSet(comptime Key: type, comptime representation: SetRepresentation, comptime source: EpochSource) type {
    if (representation == .bitset and source == .natural) {
        @compileError("a wholesale-cleared bitset has no epochs to supply");
    }
    return struct {
        const Self = @This();

        storage: switch (representation) {
            .stamps => []u32,
            .bitset => std.DynamicBitSetUnmanaged,
        } = switch (representation) {
            .stamps => &.{},
            .bitset => .{},
        },
        epoch: if (representation == .stamps) Epoch else void = if (representation == .stamps) .{ .current = if (source == .natural) 0 else first_epoch } else {},

        logical_len: usize = 0,
        borrowed: bool = false,
        pages: zeroed_pages.Pages = .{ .bytes = &.{} },

        /// An empty fixed-domain stamped set on fresh zero pages.
        pub fn initSparse(key_count: usize) Allocator.Error!Self {
            if (representation != .stamps) @compileError("sparse epoch storage requires stamps");
            const bytes = std.math.mul(usize, key_count, @sizeOf(u32)) catch return error.OutOfMemory;
            const pages = try zeroed_pages.map(bytes, .sparse);
            return .{
                .storage = @as([*]u32, @ptrCast(pages.bytes.ptr))[0..key_count],
                .logical_len = key_count,
                .pages = pages,
            };
        }

        /// Borrow an owner's zeroed stamp domain without taking its storage.
        /// The owner must not reuse that domain while this cursor is active.
        pub fn borrow(stamps: []u32) Self {
            if (representation != .stamps) @compileError("only stamp domains can be borrowed");
            return .{ .storage = stamps, .logical_len = stamps.len, .borrowed = true };
        }

        /// An empty set over keys below `key_count`.
        pub fn init(gpa: Allocator, key_count: usize) Allocator.Error!Self {
            var self: Self = .{};
            try self.ensureLen(gpa, key_count);
            return self;
        }

        /// Free the set.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            switch (representation) {
                .stamps => if (!self.borrowed) {
                    if (self.pages.bytes.len != 0) self.pages.unmap() else gpa.free(self.storage);
                },
                .bitset => self.storage.deinit(gpa),
            }
            self.* = .{};
        }

        /// The number of keys the set can hold.
        pub fn len(self: *const Self) usize {
            return switch (representation) {
                .stamps => self.logical_len,
                .bitset => self.storage.bit_length,
            };
        }

        /// Accept keys below `new_len`; the new keys are absent.
        pub fn ensureLen(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            switch (representation) {
                .stamps => {
                    if (new_len <= self.logical_len) return;
                    // A mapped set has a fixed domain; frozen graph clones
                    // never grow. Mutable graphs retain amortized capacity.
                    std.debug.assert(self.pages.bytes.len == 0 and !self.borrowed);
                    if (new_len > self.storage.len) {
                        const capacity = @max(new_len, self.storage.len +| self.storage.len / 2 +| 8);
                        self.storage = try gpa.realloc(self.storage, capacity);
                    }
                    clearStamps(self.storage[self.logical_len..new_len]);
                    self.logical_len = new_len;
                },
                .bitset => if (new_len > self.storage.bit_length) {
                    const old_len = self.storage.bit_length;
                    try self.storage.resize(gpa, new_len, false);
                    storage_work.add(.bytes_filled, (new_len - old_len + 7) / 8);
                },
            }
        }

        /// Reserve room for `capacity` keys without changing the domain.
        pub fn ensureTotalCapacityPrecise(self: *Self, gpa: Allocator, capacity: usize) Allocator.Error!void {
            if (representation != .stamps) @compileError("only stamped sets reserve key capacity");
            if (capacity <= self.storage.len) return;
            self.storage = try gpa.realloc(self.storage, capacity);
        }

        /// Add one absent key; room for it must already be reserved.
        pub fn appendAbsentAssumeCapacity(self: *Self) void {
            if (representation != .stamps) @compileError("only stamped sets append keys");
            std.debug.assert(self.logical_len < self.storage.len);
            self.storage[self.logical_len] = 0;
            storage_work.add(.bytes_filled, @sizeOf(u32));
            self.logical_len += 1;
        }

        /// Forget every key, retaining capacity. Keys added afterwards are
        /// cleared as they are added, so this touches no stamps.
        pub fn clearRetainingCapacity(self: *Self) void {
            if (representation != .stamps) @compileError("only stamped sets truncate their domain");
            self.logical_len = 0;
        }

        /// `ensureLen` by moving to a new block, so the number of allocations
        /// a caller makes does not depend on whether the heap could extend
        /// the old block in place.
        pub fn ensureLenRelocating(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            if (representation != .stamps) @compileError("only stamped sets relocate their stamps");
            if (new_len <= self.logical_len) return;
            std.debug.assert(self.pages.bytes.len == 0 and !self.borrowed);
            if (new_len > self.storage.len) {
                const grown = try gpa.alloc(u32, new_len);
                @memcpy(grown[0..self.logical_len], self.storage[0..self.logical_len]);
                gpa.free(self.storage);
                self.storage = grown;
            }
            clearStamps(self.storage[self.logical_len..new_len]);
            self.logical_len = new_len;
        }

        /// Discard a mutable graph suffix while retaining its stamp capacity.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            std.debug.assert(new_len <= self.len());
            self.clearRange(new_len, self.len());
            if (representation == .stamps) {
                self.logical_len = new_len;
            } else {
                @compileError("bitset domains are retained across resets, not truncated");
            }
        }

        /// Whether `k` is in the set.
        pub inline fn contains(self: *const Self, k: Key) bool {
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            return switch (representation) {
                .stamps => self.epoch.current != 0 and self.storage[i] == self.epoch.current,
                .bitset => self.storage.isSet(i),
            };
        }

        /// Add `k`.
        pub inline fn insert(self: *Self, k: Key) void {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            switch (representation) {
                .stamps => self.storage[i] = self.epoch.current,
                .bitset => self.storage.set(i),
            }
        }

        /// Add `k`, returning whether it was already present.
        pub inline fn testAndInsert(self: *Self, k: Key) bool {
            const present = self.contains(k);
            self.insert(k);
            return present;
        }

        /// Remove `k`.
        pub inline fn remove(self: *Self, k: Key) void {
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            switch (representation) {
                .stamps => self.storage[i] = 0,
                .bitset => self.storage.unset(i),
            }
        }

        /// Empty the set.
        pub fn reset(self: *Self) void {
            if (source != .internal) @compileError("a natural-epoch set is reset with resetTo");
            switch (representation) {
                .stamps => if (self.epoch.advance()) clearStamps(self.storage),
                .bitset => {
                    self.storage.unsetAll();
                    storage_work.add(.bytes_filled, (self.storage.bit_length + 7) / 8);
                },
            }
        }

        /// Empty the set by installing the natural epoch `identifier`.
        pub fn resetTo(self: *Self, identifier: u32) void {
            if (source != .natural) @compileError("an internal-epoch set is reset with reset");
            self.epoch.install(identifier);
        }

        /// Empty a natural-epoch set before its identifiers restart.
        pub fn clear(self: *Self) void {
            switch (representation) {
                .stamps => {
                    clearStamps(self.storage);
                    self.epoch = .{ .current = if (source == .natural) 0 else first_epoch };
                },
                .bitset => self.storage.unsetAll(),
            }
        }

        /// Remove every key in `[start, end)`; for rolling back a suffix.
        pub fn clearRange(self: *Self, start: usize, end: usize) void {
            std.debug.assert(start <= end and end <= self.len());
            switch (representation) {
                .stamps => clearStamps(self.storage[start..end]),
                .bitset => self.storage.setRangeValue(.{ .start = start, .end = end }, false),
            }
        }
    };
}

/// A column of `T` per `Key` that empties in constant time: an entry holds a
/// datum only when it was set in the current epoch. Data of at most four
/// bytes are interleaved with their stamps, so one probe touches one cache
/// line; wider data live in a separate column. Data are never initialized:
/// `set` always writes the stamp and the datum together, and `get` reads the
/// datum only when its stamp matches.
pub fn EpochColumn(comptime Key: type, comptime T: type, comptime source: EpochSource) type {
    const interleaved = @sizeOf(T) <= 4;
    return struct {
        const Self = @This();

        const Entry = struct { stamp: u32, datum: T };

        entries: if (interleaved) []Entry else void = if (interleaved) &.{} else {},
        stamps: if (interleaved) void else []u32 = if (interleaved) {} else &.{},
        data: if (interleaved) void else []T = if (interleaved) {} else &.{},
        epoch: Epoch = .{ .current = if (source == .natural) 0 else first_epoch },

        /// An empty column over keys below `key_count`.
        pub fn init(gpa: Allocator, key_count: usize) Allocator.Error!Self {
            var self: Self = .{};
            errdefer self.deinit(gpa);
            try self.ensureLen(gpa, key_count);
            return self;
        }

        /// Free the column.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (interleaved) {
                gpa.free(self.entries);
            } else {
                gpa.free(self.stamps);
                gpa.free(self.data);
            }
            self.* = .{};
        }

        /// The number of keys the column can hold.
        pub fn len(self: *const Self) usize {
            return if (interleaved) self.entries.len else self.stamps.len;
        }

        /// Accept keys below `new_len`; the new keys hold nothing.
        pub fn ensureLen(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            if (interleaved) {
                const old_len = self.entries.len;
                if (new_len <= old_len) return;
                const capacity = @max(new_len, old_len +| old_len / 2 +| 8);
                self.entries = try gpa.realloc(self.entries, capacity);
                for (self.entries[old_len..]) |*entry| entry.* = .{ .stamp = 0, .datum = undefined };
                storage_work.add(.bytes_filled, (capacity - old_len) * @sizeOf(u32));
            } else {
                if (new_len <= self.stamps.len) return;
                const old_len = self.data.len;
                // A failed stamp allocation can leave spare data capacity.
                if (new_len > old_len) {
                    self.data = try gpa.realloc(self.data, new_len);
                    if (std.debug.runtime_safety) @memset(self.data[old_len..], undefined);
                }
                try growStamps(gpa, &self.stamps, new_len);
            }
        }

        /// `ensureLen` by moving to a new block, so the number of allocations
        /// a caller makes does not depend on whether the heap could extend
        /// the old block in place.
        pub fn ensureLenRelocating(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            if (!interleaved) @compileError("only interleaved columns relocate as one block");
            const old_len = self.entries.len;
            if (new_len <= old_len) return;
            const grown = try gpa.alloc(Entry, new_len);
            @memcpy(grown[0..old_len], self.entries);
            gpa.free(self.entries);
            self.entries = grown;
            for (self.entries[old_len..]) |*entry| entry.* = .{ .stamp = 0, .datum = undefined };
            storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(u32));
        }

        /// The datum at `k` in the current epoch, or null.
        pub inline fn get(self: *const Self, k: Key) ?T {
            const i = keyIndex(Key, k);
            if (interleaved) {
                const entry = &self.entries[i];
                return if (self.epoch.current != 0 and entry.stamp == self.epoch.current) entry.datum else null;
            }
            return if (self.epoch.current != 0 and self.stamps[i] == self.epoch.current) self.data[i] else null;
        }

        /// A pointer to the datum at `k`, which must be set in the current
        /// epoch.
        pub inline fn ptr(self: *Self, k: Key) *T {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            if (interleaved) {
                std.debug.assert(self.entries[i].stamp == self.epoch.current);
                return &self.entries[i].datum;
            }
            std.debug.assert(self.stamps[i] == self.epoch.current);
            return &self.data[i];
        }

        /// Whether `k` holds a datum in the current epoch.
        pub inline fn contains(self: *const Self, k: Key) bool {
            const i = keyIndex(Key, k);
            if (self.epoch.current == 0) return false;
            if (interleaved) return self.entries[i].stamp == self.epoch.current;
            return self.stamps[i] == self.epoch.current;
        }

        /// Store `value` at `k` in the current epoch.
        pub inline fn set(self: *Self, k: Key, value: T) void {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            if (interleaved) {
                self.entries[i] = .{ .stamp = self.epoch.current, .datum = value };
            } else {
                self.stamps[i] = self.epoch.current;
                self.data[i] = value;
            }
        }

        /// Remove the datum at `k`.
        pub inline fn remove(self: *Self, k: Key) void {
            const i = keyIndex(Key, k);
            if (interleaved) self.entries[i].stamp = 0 else self.stamps[i] = 0;
        }

        /// Empty the column.
        pub fn reset(self: *Self) void {
            if (source != .internal) @compileError("a natural-epoch column is reset with resetTo");
            if (self.epoch.advance()) self.clearAllStamps();
        }

        /// Empty the column by installing the natural epoch `identifier`.
        pub fn resetTo(self: *Self, identifier: u32) void {
            if (source != .natural) @compileError("an internal-epoch column is reset with reset");
            self.epoch.install(identifier);
        }

        /// Empty a natural-epoch column before its identifiers restart.
        pub fn clear(self: *Self) void {
            self.clearAllStamps();
            self.epoch = .{ .current = if (source == .natural) 0 else first_epoch };
        }

        /// Remove every datum in `[start, end)`; for rolling back a suffix.
        pub fn clearRange(self: *Self, start: usize, end: usize) void {
            if (interleaved) {
                for (self.entries[start..end]) |*entry| entry.stamp = 0;
                storage_work.add(.bytes_filled, (end - start) * @sizeOf(u32));
            } else {
                clearStamps(self.stamps[start..end]);
            }
        }

        fn clearAllStamps(self: *Self) void {
            if (interleaved) {
                for (self.entries) |*entry| entry.stamp = 0;
                storage_work.add(.bytes_filled, self.entries.len * @sizeOf(u32));
            } else {
                clearStamps(self.stamps);
            }
        }
    };
}

/// Whether an `EpochBitSet` also lists its members.
pub const MemberOrder = enum {
    /// Membership and a member count only; the set cannot be iterated.
    unlisted,
    /// Members are also listed in insertion order for iteration.
    insertion,
};

/// A set of `Key`s that empties in constant time, at about one and a half
/// bits per key of its domain.
///
/// Keys are bits packed 64 to a word, and each word carries a `u32` stamp. A
/// word holds members only while its stamp equals the current epoch; a word
/// with any other stamp reads as empty and is rewritten whole by its first
/// insertion of the epoch, so emptying the set is one epoch advance. Lookup
/// and insertion are branchless within the column: the stamp comparison
/// becomes a mask over the word.
///
/// Nothing is allocated before the first insertion, and the column grows to
/// the words the inserted keys reach, so a set that only ever holds low keys
/// spans only those words. Keys past the column's end are absent.
///
/// A set serves one traversal at a time when it is held through
/// `beginTraversal` and `endTraversal`; safety builds assert that no
/// traversal begins while another holds it.
pub fn EpochBitSet(comptime Key: type, comptime order: MemberOrder) type {
    return struct {
        const Self = @This();

        /// Sixty-four keys and their stamp. The halves of the bits are
        /// stored apart so a word takes 12 bytes rather than 16.
        const Word = extern struct { stamp: u32, lo: u32, hi: u32 };

        allocator: Allocator,
        words: []Word = &.{},
        /// The members of the current epoch, in insertion order.
        keys: if (order == .insertion) std.ArrayList(Key) else void = if (order == .insertion) .empty else {},
        /// The number of members of the current epoch.
        member_count: if (order == .unlisted) usize else void = if (order == .unlisted) 0 else {},
        epoch: Epoch = .{},
        /// Whether a traversal holds the set; tracked only in safety builds.
        traversal_active: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.words);
            if (order == .insertion) self.keys.deinit(self.allocator);
            self.* = undefined;
        }

        /// The number of members.
        pub fn count(self: *const Self) usize {
            return if (order == .insertion) self.keys.items.len else self.member_count;
        }

        /// Whether `k` is a member.
        pub inline fn contains(self: *const Self, k: Key) bool {
            const i = keyIndex(Key, k);
            const w = i >> 6;
            if (w >= self.words.len) return false;
            const word = self.words[w];
            const bits = wordBits(word) & self.liveMask(word);
            return ((bits >> @as(u6, @truncate(i))) & 1) != 0;
        }

        /// Add `k`, returning whether it was already a member.
        pub inline fn testAndInsert(self: *Self, k: Key) Allocator.Error!bool {
            const i = keyIndex(Key, k);
            const w = i >> 6;
            if (w >= self.words.len) try self.growWords(w + 1);
            if (order == .insertion) try self.keys.ensureUnusedCapacity(self.allocator, 1);
            const word = &self.words[w];
            const bits = wordBits(word.*) & self.liveMask(word.*);
            const bit = @as(u64, 1) << @as(u6, @truncate(i));
            const present = (bits & bit) != 0;
            const updated = bits | bit;
            word.* = .{ .stamp = self.epoch.current, .lo = @truncate(updated), .hi = @truncate(updated >> 32) };
            if (!present) {
                if (order == .insertion) self.keys.appendAssumeCapacity(k) else self.member_count += 1;
            }
            return present;
        }

        /// Add `k`.
        pub inline fn insert(self: *Self, k: Key) Allocator.Error!void {
            _ = try self.testAndInsert(k);
        }

        /// Remove every member.
        pub fn clearRetainingCapacity(self: *Self) void {
            if (order == .insertion) self.keys.clearRetainingCapacity() else self.member_count = 0;
            if (self.epoch.advance()) {
                for (self.words) |*word| word.stamp = 0;
                storage_work.add(.bytes_filled, self.words.len * @sizeOf(u32));
            }
        }

        /// Empty the set and hold it for one traversal. Beginning another
        /// traversal before `endTraversal` is a compiler invariant violation.
        pub fn beginTraversal(self: *Self) void {
            if (std.debug.runtime_safety) {
                std.debug.assert(!self.traversal_active);
                self.traversal_active = true;
            }
            self.clearRetainingCapacity();
        }

        /// Release the set held by `beginTraversal`.
        pub fn endTraversal(self: *Self) void {
            if (std.debug.runtime_safety) {
                std.debug.assert(self.traversal_active);
                self.traversal_active = false;
            }
        }

        /// Iterates the members in insertion order. Like `DenseMap`'s
        /// iterators, it copies each key, so a key pointer stays valid while
        /// the set grows, and members inserted during iteration are visited
        /// too.
        pub fn keyIterator(self: *const Self) KeyIterator {
            if (order != .insertion) @compileError("an unlisted set cannot be iterated");
            return .{ .set = self };
        }

        /// Iterates the members in insertion order, by value.
        pub fn constKeyIterator(self: *const Self) ConstKeyIterator {
            if (order != .insertion) @compileError("an unlisted set cannot be iterated");
            return .{ .set = self };
        }

        /// Iterates the members in insertion order as entries holding a key
        /// pointer, the shape map iterators have.
        pub fn iterator(self: *const Self) Iterator {
            if (order != .insertion) @compileError("an unlisted set cannot be iterated");
            return .{ .inner = .{ .set = self } };
        }

        pub const KeyIterator = struct {
            set: *const Self,
            position: usize = 0,
            key: Key = undefined,

            pub fn next(self: *KeyIterator) ?*const Key {
                if (self.position >= self.set.keys.items.len) return null;
                self.key = self.set.keys.items[self.position];
                self.position += 1;
                return &self.key;
            }
        };

        pub const ConstKeyIterator = struct {
            set: *const Self,
            position: usize = 0,

            pub fn next(self: *ConstKeyIterator) ?Key {
                if (self.position >= self.set.keys.items.len) return null;
                const key = self.set.keys.items[self.position];
                self.position += 1;
                return key;
            }
        };

        /// An iterated member.
        pub const Entry = struct { key_ptr: *const Key };

        pub const Iterator = struct {
            inner: KeyIterator,

            pub fn next(self: *Iterator) ?Entry {
                return .{ .key_ptr = self.inner.next() orelse return null };
            }
        };

        inline fn wordBits(word: Word) u64 {
            return (@as(u64, word.hi) << 32) | word.lo;
        }

        /// All ones when `word` belongs to the current epoch, else zero.
        inline fn liveMask(self: *const Self, word: Word) u64 {
            return @as(u64, 0) -% @intFromBool(word.stamp == self.epoch.current);
        }

        fn growWords(self: *Self, min_len: usize) Allocator.Error!void {
            const old_len = self.words.len;
            const new_len = @max(min_len, old_len +| old_len / 2 +| 4);
            self.words = try self.allocator.realloc(self.words, new_len);
            for (self.words[old_len..]) |*word| word.* = .{ .stamp = 0, .lo = 0, .hi = 0 };
            storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(Word));
        }
    };
}

/// A pool of reusable `EpochBitSet`s for passes that repeatedly need
/// short-lived sets over one domain. A pooled set keeps its words, so
/// acquiring one costs an epoch advance. The pool hands out sets by value, so
/// nested scopes may acquire further sets while an outer one is still in use,
/// and each nesting depth keeps words of its own.
pub fn EpochBitSetPool(comptime Key: type, comptime order: MemberOrder) type {
    return struct {
        const Self = @This();

        pub const Set = EpochBitSet(Key, order);

        /// Sets kept beyond this many are freed on release instead of pooled.
        const max_pooled = 8;

        allocator: Allocator,
        sets: [max_pooled]Set = undefined,
        len: usize = 0,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            for (self.sets[0..self.len]) |*set| set.deinit();
            self.* = undefined;
        }

        /// An empty set, reusing a pooled set's storage when one is available.
        pub fn acquire(self: *Self) Set {
            if (self.len > 0) {
                self.len -= 1;
                return self.sets[self.len];
            }
            return Set.init(self.allocator);
        }

        /// Return an acquired set to the pool. The caller's set is consumed.
        pub fn release(self: *Self, set: *Set) void {
            if (self.len == max_pooled) {
                set.deinit();
                return;
            }
            set.clearRetainingCapacity();
            self.sets[self.len] = set.*;
            self.len += 1;
            set.* = undefined;
        }
    };
}

/// A map from `Key` to `V` that empties in constant time and iterates in
/// insertion order, with the observable behavior of a `DenseMap` that is
/// never removed from: the same lookups, the same entries, and the same
/// iteration order after any sequence of insertions and clears.
///
/// Each key owns a slot holding a stamp and the value side by side. A slot
/// holds an entry only when its stamp equals the current epoch, so emptying
/// the map is one epoch advance. Slots live in pages of 64 consecutive keys,
/// found through a directory indexed by the key's page; a page is allocated,
/// with its stamps cleared, the first time one of its keys is inserted, so
/// the map's storage follows the keys it has held rather than the size of
/// their domain. Keys whose page does not exist hold nothing. The keys of the
/// current epoch are also listed in insertion order for iteration.
///
/// This suits maps that are cleared and refilled many times over one domain
/// that outlives them, such as a type store's variables.
pub fn EpochMap(comptime Key: type, comptime V: type) type {
    return struct {
        const Self = @This();

        const Slot = struct { stamp: u32, value: V };
        const page_shift = 6;
        const page_len = 1 << page_shift;
        const Page = [page_len]Slot;

        pub const Entry = struct {
            key_ptr: *const Key,
            value_ptr: *V,
        };

        pub const GetOrPutResult = struct {
            value_ptr: *V,
            found_existing: bool,
        };

        allocator: Allocator,
        /// The page of each run of 64 keys, null until one of them is
        /// inserted. It spans the pages up to the highest one inserted into.
        pages: std.ArrayList(?*Page) = .empty,
        /// The number of pages allocated.
        allocated_pages: usize = 0,
        /// The keys holding entries in the current epoch, in insertion order.
        keys: std.ArrayList(Key) = .empty,
        epoch: Epoch = .{},

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            for (self.pages.items) |page| if (page) |allocated| self.allocator.destroy(allocated);
            self.pages.deinit(self.allocator);
            self.keys.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn count(self: *const Self) usize {
            return self.keys.items.len;
        }

        pub inline fn contains(self: *const Self, key: Key) bool {
            return self.liveSlot(keyIndex(Key, key)) != null;
        }

        pub inline fn get(self: *const Self, key: Key) ?V {
            const slot = self.liveSlot(keyIndex(Key, key)) orelse return null;
            return slot.value;
        }

        pub inline fn getPtr(self: *Self, key: Key) ?*V {
            return &(self.liveSlot(keyIndex(Key, key)) orelse return null).value;
        }

        pub fn put(self: *Self, key: Key, value: V) Allocator.Error!void {
            const result = try self.getOrPut(key);
            result.value_ptr.* = value;
        }

        pub fn putNoClobber(self: *Self, key: Key, value: V) Allocator.Error!void {
            const result = try self.getOrPut(key);
            std.debug.assert(!result.found_existing);
            result.value_ptr.* = value;
        }

        pub fn getOrPut(self: *Self, key: Key) Allocator.Error!GetOrPutResult {
            const i = keyIndex(Key, key);
            const page = try self.ensurePage(i >> page_shift);
            const slot = &page[i & (page_len - 1)];
            if (slot.stamp == self.epoch.current) return .{ .value_ptr = &slot.value, .found_existing = true };
            try self.keys.append(self.allocator, key);
            slot.stamp = self.epoch.current;
            return .{ .value_ptr = &slot.value, .found_existing = false };
        }

        pub fn getOrPutValue(self: *Self, key: Key, value: V) Allocator.Error!GetOrPutResult {
            const result = try self.getOrPut(key);
            if (!result.found_existing) result.value_ptr.* = value;
            return result;
        }

        /// Reserve room for `additional_count` more entries in the key list.
        pub fn ensureUnusedCapacity(self: *Self, additional_count: usize) Allocator.Error!void {
            try self.keys.ensureUnusedCapacity(self.allocator, additional_count);
        }

        /// Forget every entry, retaining capacity.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.keys.clearRetainingCapacity();
            if (self.epoch.advance()) {
                for (self.pages.items) |page| {
                    const allocated = page orelse continue;
                    for (allocated) |*slot| slot.stamp = 0;
                }
                storage_work.add(.bytes_filled, self.allocated_pages * page_len * @sizeOf(u32));
            }
        }

        pub fn iterator(self: *Self) Iterator {
            return .{ .map = self };
        }

        pub fn keyIterator(self: *Self) KeyIterator {
            return .{ .map = self };
        }

        pub fn valueIterator(self: *Self) ValueIterator {
            return .{ .map = self };
        }

        pub fn constKeyIterator(self: *const Self) ConstKeyIterator {
            return .{ .map = self };
        }

        /// Iterates the keys of a map it cannot change.
        pub const ConstKeyIterator = struct {
            map: *const Self,
            position: usize = 0,

            pub fn next(self: *ConstKeyIterator) ?Key {
                if (self.position >= self.map.keys.items.len) return null;
                const key = self.map.keys.items[self.position];
                self.position += 1;
                return key;
            }
        };

        /// Like `DenseMap`'s iterators, these copy each key, so a key pointer
        /// stays valid while the map grows, and entries inserted during
        /// iteration are visited too.
        pub const Iterator = struct {
            map: *Self,
            position: usize = 0,
            key: Key = undefined,

            pub fn next(self: *Iterator) ?Entry {
                if (self.position >= self.map.keys.items.len) return null;
                self.key = self.map.keys.items[self.position];
                self.position += 1;
                return .{ .key_ptr = &self.key, .value_ptr = &self.map.liveSlot(keyIndex(Key, self.key)).?.value };
            }
        };

        pub const KeyIterator = struct {
            map: *Self,
            position: usize = 0,
            key: Key = undefined,

            pub fn next(self: *KeyIterator) ?*const Key {
                if (self.position >= self.map.keys.items.len) return null;
                self.key = self.map.keys.items[self.position];
                self.position += 1;
                return &self.key;
            }
        };

        pub const ValueIterator = struct {
            map: *Self,
            position: usize = 0,

            pub fn next(self: *ValueIterator) ?*V {
                if (self.position >= self.map.keys.items.len) return null;
                const key = self.map.keys.items[self.position];
                self.position += 1;
                return &self.map.liveSlot(keyIndex(Key, key)).?.value;
            }
        };

        inline fn liveSlot(self: *const Self, i: usize) ?*Slot {
            const page_index = i >> page_shift;
            if (page_index >= self.pages.items.len) return null;
            const page = self.pages.items[page_index] orelse return null;
            const slot = &page[i & (page_len - 1)];
            return if (slot.stamp == self.epoch.current) slot else null;
        }

        /// The page holding the keys of `page_index`, allocated with its
        /// stamps cleared if none of them was inserted before.
        fn ensurePage(self: *Self, page_index: usize) Allocator.Error!*Page {
            if (page_index >= self.pages.items.len) {
                const old_len = self.pages.items.len;
                try self.pages.resize(self.allocator, page_index + 1);
                @memset(self.pages.items[old_len..], null);
            }
            if (self.pages.items[page_index]) |page| return page;
            const page = try self.allocator.create(Page);
            for (page) |*slot| slot.* = .{ .stamp = 0, .value = undefined };
            storage_work.add(.bytes_filled, page_len * @sizeOf(u32));
            self.pages.items[page_index] = page;
            self.allocated_pages += 1;
            return page;
        }
    };
}

const testing = std.testing;
const TestKey = enum(u32) { _ };

fn testKey(i: u32) TestKey {
    return @fromBackingInt(i);
}

test "a reset empties a stamped set" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 8);
    defer set.deinit(testing.allocator);
    set.insert(testKey(3));
    try testing.expect(set.contains(testKey(3)));
    try testing.expect(!set.testAndInsert(testKey(5)));
    try testing.expect(set.testAndInsert(testKey(5)));
    set.reset();
    try testing.expect(!set.contains(testKey(3)));
    try testing.expect(!set.contains(testKey(5)));
}

test "a reset empties a bitset" {
    var set = try EpochSet(u32, .bitset, .internal).init(testing.allocator, 100);
    defer set.deinit(testing.allocator);
    set.insert(99);
    set.reset();
    try testing.expect(!set.contains(99));
    set.insert(1);
    set.remove(1);
    try testing.expect(!set.contains(1));
}

test "the wrap clears every stamp and restarts at one" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 4);
    defer set.deinit(testing.allocator);
    set.insert(testKey(0));
    set.epoch.current = std.math.maxInt(u32) - 1;
    set.insert(testKey(1));
    set.reset();
    try testing.expectEqual(std.math.maxInt(u32), set.epoch.current);
    try testing.expect(!set.contains(testKey(1)));
    set.insert(testKey(2));
    set.reset();
    try testing.expectEqual(@as(u32, 1), set.epoch.current);
    // Key 0 was stamped with epoch 1 long ago; the wrap cleared it.
    for (0..4) |i| try testing.expect(!set.contains(testKey(@intCast(i))));
}

test "ensureLen clears only the new suffix" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 2);
    defer set.deinit(testing.allocator);
    set.insert(testKey(1));
    const filled_before = storage_work.get(.bytes_filled);
    try set.ensureLen(testing.allocator, 10);
    if (storage_work.enabled) try testing.expectEqual(filled_before + 8 * @sizeOf(u32), storage_work.get(.bytes_filled));
    try testing.expect(set.contains(testKey(1)));
    try testing.expect(!set.contains(testKey(9)));
}

test "natural epochs empty the set without counting" {
    var set = try EpochSet(TestKey, .stamps, .natural).init(testing.allocator, 4);
    defer set.deinit(testing.allocator);
    set.resetTo(0);
    set.insert(testKey(2));
    set.resetTo(1);
    try testing.expect(!set.contains(testKey(2)));
    set.insert(testKey(3));
    set.clear();
    set.resetTo(0);
    try testing.expect(!set.contains(testKey(3)));
}

test "an interleaved column holds data only in its epoch" {
    var column = try EpochColumn(TestKey, u16, .internal).init(testing.allocator, 4);
    defer column.deinit(testing.allocator);
    try testing.expectEqual(@as(?u16, null), column.get(testKey(1)));
    column.set(testKey(1), 42);
    try testing.expectEqual(@as(?u16, 42), column.get(testKey(1)));
    column.ptr(testKey(1)).* += 1;
    try testing.expectEqual(@as(?u16, 43), column.get(testKey(1)));
    column.reset();
    try testing.expectEqual(@as(?u16, null), column.get(testKey(1)));
    try testing.expect(!column.contains(testKey(1)));
}

test "a wide column keeps separate stamps and survives a wrap" {
    var column = try EpochColumn(TestKey, u64, .internal).init(testing.allocator, 3);
    defer column.deinit(testing.allocator);
    column.set(testKey(0), 7);
    column.epoch.current = std.math.maxInt(u32);
    column.reset();
    try testing.expectEqual(@as(u32, 1), column.epoch.current);
    try testing.expectEqual(@as(?u64, null), column.get(testKey(0)));
    column.set(testKey(2), 9);
    try column.ensureLen(testing.allocator, 6);
    try testing.expectEqual(@as(?u64, 9), column.get(testKey(2)));
    try testing.expectEqual(@as(?u64, null), column.get(testKey(5)));
    column.clearRange(2, 3);
    try testing.expectEqual(@as(?u64, null), column.get(testKey(2)));
}

test "a natural-epoch column empties on each identifier" {
    var column = try EpochColumn(u32, u32, .natural).init(testing.allocator, 2);
    defer column.deinit(testing.allocator);
    column.resetTo(4);
    column.set(0, 1);
    column.resetTo(5);
    try testing.expectEqual(@as(?u32, null), column.get(0));
}

fn exerciseWideColumnAllocationFailures(gpa: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var column = try EpochColumn(u32, u64, .internal).init(gpa, 3);
    defer column.deinit(gpa);
    column.set(2, 91);
    try column.ensureLen(gpa, 20);
    try testing.expectEqual(@as(?u64, 91), column.get(2));
    try testing.expectEqual(@as(?u64, null), column.get(19));
}

test "wide column construction and growth release allocations on failure" {
    try testing.checkAllAllocationFailures(testing.allocator, exerciseWideColumnAllocationFailures, .{});
}

test "wide column retries a smaller growth after stamp allocation fails" {
    var failing = testing.FailingAllocator.init(testing.allocator, .{});
    const gpa = failing.allocator();
    var column = try EpochColumn(u32, u64, .internal).init(gpa, 3);
    defer column.deinit(gpa);
    column.set(2, 91);
    // Data growth succeeds, but the stamp allocation fails.
    failing.fail_index = failing.alloc_index + 1;
    failing.resize_fail_index = failing.resize_index;
    try testing.expectError(error.OutOfMemory, column.ensureLen(gpa, 2000));
    failing.fail_index = std.math.maxInt(usize);
    failing.resize_fail_index = std.math.maxInt(usize);
    try column.ensureLen(gpa, 10);
    try testing.expectEqual(@as(?u64, 91), column.get(2));
    try testing.expectEqual(@as(?u64, null), column.get(9));
}

test "natural epochs accept the largest real u32 identifier" {
    var set = try EpochSet(u32, .stamps, .natural).init(testing.allocator, 2);
    defer set.deinit(testing.allocator);
    try testing.expect(!set.contains(0));
    set.resetTo(std.math.maxInt(u32) - 2);
    set.insert(0);
    set.resetTo(std.math.maxInt(u32) - 1);
    try testing.expect(!set.contains(0));
    set.insert(1);
    try testing.expect(set.contains(1));
    set.clear();
    try testing.expect(!set.contains(1));
    set.resetTo(0);
    try testing.expect(!set.contains(1));
}

test "epoch suffix rollback clears reused indices without reallocating" {
    var set = try EpochSet(u32, .stamps, .internal).init(testing.allocator, 3);
    defer set.deinit(testing.allocator);
    set.insert(2);
    const capacity = set.storage.len;
    set.shrinkRetainingCapacity(1);
    try set.ensureLen(testing.allocator, 3);
    try testing.expect(!set.contains(2));
    try testing.expectEqual(capacity, set.storage.len);
}

test "a reserved stamped set appends absent keys and truncates without touching stamps" {
    var set: EpochSet(u32, .stamps, .internal) = .{};
    defer set.deinit(testing.allocator);
    try set.ensureTotalCapacityPrecise(testing.allocator, 4);
    set.appendAbsentAssumeCapacity();
    set.appendAbsentAssumeCapacity();
    try testing.expectEqual(@as(usize, 2), set.len());
    set.insert(1);
    set.clearRetainingCapacity();
    try testing.expectEqual(@as(usize, 0), set.len());
    set.appendAbsentAssumeCapacity();
    set.appendAbsentAssumeCapacity();
    try testing.expect(!set.contains(1));
}

test "relocating growth keeps stamps and leaves new keys absent" {
    var set: EpochSet(u32, .stamps, .internal) = .{};
    defer set.deinit(testing.allocator);
    try set.ensureLenRelocating(testing.allocator, 2);
    set.insert(1);
    try set.ensureLenRelocating(testing.allocator, 8);
    try testing.expect(set.contains(1));
    try testing.expect(!set.contains(7));

    var column: EpochColumn(u32, u32, .internal) = .{};
    defer column.deinit(testing.allocator);
    try column.ensureLenRelocating(testing.allocator, 2);
    column.set(1, 9);
    try column.ensureLenRelocating(testing.allocator, 8);
    try testing.expectEqual(@as(?u32, 9), column.get(1));
    try testing.expectEqual(@as(?u32, null), column.get(7));
}

test "sparse epoch domains begin absent and reset without filling" {
    var set = try EpochSet(u32, .stamps, .internal).initSparse(100000);
    defer set.deinit(testing.allocator);
    try testing.expect(!set.contains(99999));
    set.insert(99999);
    set.reset();
    try testing.expect(!set.contains(99999));
}

test "borrowed epoch cursors never free their owner's stamps" {
    var stamps: [4]u32 = @splat(0);
    var set = EpochSet(u32, .stamps, .internal).borrow(&stamps);
    defer set.deinit(testing.allocator);
    set.insert(2);
    try testing.expect(set.contains(2));
    set.epoch.current = std.math.maxInt(u32);
    set.reset();
    try testing.expect(!set.contains(2));
    try testing.expectEqualSlices(u32, &.{ 0, 0, 0, 0 }, &stamps);
}

test "an epoch map matches a dense map's entries and iteration order" {
    const DenseMap = @import("DenseMap.zig").DenseMap;
    var dense = DenseMap(TestKey, u32).init(testing.allocator);
    defer dense.deinit();
    var stamped = EpochMap(TestKey, u32).init(testing.allocator);
    defer stamped.deinit();

    var prng = std.Random.DefaultPrng.init(0x5eed);
    const random = prng.random();
    for (0..4000) |step| {
        const key = testKey(random.uintLessThan(u32, 300));
        switch (random.uintLessThan(u8, 10)) {
            0 => {
                dense.clearRetainingCapacity();
                stamped.clearRetainingCapacity();
            },
            1, 2, 3 => {
                try dense.put(key, @intCast(step));
                try stamped.put(key, @intCast(step));
            },
            4, 5 => {
                const a = try dense.getOrPut(key);
                const b = try stamped.getOrPut(key);
                try testing.expectEqual(a.found_existing, b.found_existing);
                if (!a.found_existing) {
                    a.value_ptr.* = 7;
                    b.value_ptr.* = 7;
                }
            },
            else => {
                try testing.expectEqual(dense.get(key), stamped.get(key));
                try testing.expectEqual(dense.contains(key), stamped.contains(key));
            },
        }
        try testing.expectEqual(dense.count(), stamped.count());
    }

    var dense_entries = dense.iterator();
    var stamped_entries = stamped.iterator();
    while (dense_entries.next()) |expected| {
        const actual = stamped_entries.next().?;
        try testing.expectEqual(expected.key_ptr.*, actual.key_ptr.*);
        try testing.expectEqual(expected.value_ptr.*, actual.value_ptr.*);
    }
    try testing.expect(stamped_entries.next() == null);
}

test "an epoch map grows mid-epoch and keeps earlier entries" {
    var map = EpochMap(u32, u32).init(testing.allocator);
    defer map.deinit();
    try map.put(2, 20);
    try testing.expectEqual(@as(?u32, null), map.get(5000));
    try map.put(5000, 50);
    try testing.expectEqual(@as(?u32, 20), map.get(2));
    try testing.expectEqual(@as(?u32, 50), map.get(5000));
    try testing.expectEqual(@as(?u32, null), map.get(4999));
    var keys = map.keyIterator();
    try testing.expectEqual(@as(u32, 2), keys.next().?.*);
    try testing.expectEqual(@as(u32, 5000), keys.next().?.*);
    try testing.expect(keys.next() == null);
    var values = map.valueIterator();
    try testing.expectEqual(@as(u32, 20), values.next().?.*);
    try testing.expectEqual(@as(u32, 50), values.next().?.*);
    try testing.expect(values.next() == null);
}

test "an epoch map wrap clears every stamp" {
    var map = EpochMap(u32, void).init(testing.allocator);
    defer map.deinit();
    try map.put(1, {});
    map.epoch.current = std.math.maxInt(u32) - 1;
    try map.put(2, {});
    map.clearRetainingCapacity();
    try testing.expectEqual(std.math.maxInt(u32), map.epoch.current);
    try testing.expect(!map.contains(2));
    try map.put(3, {});
    map.clearRetainingCapacity();
    try testing.expectEqual(@as(u32, 1), map.epoch.current);
    // Key 1 was stamped with epoch 1 long ago; the wrap cleared it.
    for (0..4) |i| try testing.expect(!map.contains(@intCast(i)));
    try testing.expectEqual(@as(usize, 0), map.count());
}

test "an epoch map key beyond its column after a clear is absent" {
    var map = EpochMap(u32, u32).init(testing.allocator);
    defer map.deinit();
    try map.put(9, 1);
    map.clearRetainingCapacity();
    try testing.expect(!map.contains(9));
    const result = try map.getOrPut(9);
    try testing.expect(!result.found_existing);
    result.value_ptr.* = 4;
    try testing.expectEqual(@as(?u32, 4), map.get(9));
}

test "an epoch bitset matches a dense set's membership, count and insertion order" {
    const DenseMap = @import("DenseMap.zig").DenseMap;
    var dense = DenseMap(TestKey, void).init(testing.allocator);
    defer dense.deinit();
    var listed = EpochBitSet(TestKey, .insertion).init(testing.allocator);
    defer listed.deinit();
    var unlisted = EpochBitSet(TestKey, .unlisted).init(testing.allocator);
    defer unlisted.deinit();

    var prng = std.Random.DefaultPrng.init(0xb175);
    const random = prng.random();
    for (0..6000) |_| {
        const key = testKey(random.uintLessThan(u32, 700));
        switch (random.uintLessThan(u8, 12)) {
            0 => {
                dense.clearRetainingCapacity();
                listed.clearRetainingCapacity();
                unlisted.clearRetainingCapacity();
            },
            1, 2, 3, 4 => {
                const expected = (try dense.getOrPut(key)).found_existing;
                try testing.expectEqual(expected, try listed.testAndInsert(key));
                try testing.expectEqual(expected, try unlisted.testAndInsert(key));
            },
            else => {
                try testing.expectEqual(dense.contains(key), listed.contains(key));
                try testing.expectEqual(dense.contains(key), unlisted.contains(key));
            },
        }
        try testing.expectEqual(dense.count(), listed.count());
        try testing.expectEqual(dense.count(), unlisted.count());
    }

    var expected_keys = dense.constKeyIterator();
    var actual_keys = listed.constKeyIterator();
    var entries = listed.iterator();
    while (expected_keys.next()) |expected| {
        try testing.expectEqual(expected, actual_keys.next().?);
        try testing.expectEqual(expected, entries.next().?.key_ptr.*);
    }
    try testing.expect(actual_keys.next() == null);
    try testing.expect(entries.next() == null);
}

test "an epoch bitset allocates nothing before an insertion and grows to the words it touches" {
    var set = EpochBitSet(u32, .unlisted).init(testing.allocator);
    defer set.deinit();
    try testing.expect(!set.contains(5));
    set.beginTraversal();
    try testing.expect(!set.contains(1_000_000));
    set.endTraversal();
    try testing.expectEqual(@as(usize, 0), set.words.len);
    try set.insert(3);
    try testing.expect(set.words.len < 8);
    try testing.expect(!set.contains(1_000_000));
    // Growth in the middle of an epoch keeps earlier members.
    try set.insert(1000);
    try testing.expect(set.contains(3));
    try testing.expect(set.contains(1000));
    try testing.expect(!set.contains(999));
    try testing.expect(!set.contains(1001));
    try testing.expect(!set.contains(@intCast(64 * set.words.len + 5)));
    try testing.expectEqual(@as(usize, 2), set.count());
}

test "an epoch bitset word from an earlier epoch reads empty and is reset by its first insertion" {
    var set = EpochBitSet(u32, .unlisted).init(testing.allocator);
    defer set.deinit();
    for ([_]u32{ 0, 1, 63, 64, 130 }) |key| try set.insert(key);
    set.clearRetainingCapacity();
    for ([_]u32{ 0, 1, 63, 64, 130 }) |key| try testing.expect(!set.contains(key));
    try testing.expectEqual(@as(usize, 0), set.count());
    // Inserting one key of a stale word drops the word's old members.
    try testing.expect(!try set.testAndInsert(1));
    try testing.expect(set.contains(1));
    try testing.expect(!set.contains(0));
    try testing.expect(!set.contains(63));
    try testing.expect(try set.testAndInsert(1));
    try testing.expect(!set.contains(64));
    try testing.expect(!set.contains(130));
}

test "an epoch bitset wrap clears every stamp and restarts at one" {
    var set = EpochBitSet(u32, .insertion).init(testing.allocator);
    defer set.deinit();
    try set.insert(2);
    set.epoch.current = std.math.maxInt(u32) - 1;
    try set.insert(70);
    set.clearRetainingCapacity();
    try testing.expectEqual(std.math.maxInt(u32), set.epoch.current);
    try testing.expect(!set.contains(70));
    try set.insert(100);
    set.clearRetainingCapacity();
    try testing.expectEqual(@as(u32, 1), set.epoch.current);
    // Key 2 was stamped with epoch 1 long ago; the wrap cleared it.
    for (0..200) |i| try testing.expect(!set.contains(@intCast(i)));
    for (set.words) |word| try testing.expectEqual(@as(u32, 0), word.stamp);
    try testing.expectEqual(@as(usize, 0), set.count());
}

test "an epoch bitset traversal empties the set each time it begins" {
    var set = EpochBitSet(TestKey, .unlisted).init(testing.allocator);
    defer set.deinit();
    set.beginTraversal();
    try set.insert(testKey(9));
    if (std.debug.runtime_safety) try testing.expect(set.traversal_active);
    set.endTraversal();
    set.beginTraversal();
    try testing.expect(!set.contains(testKey(9)));
    set.endTraversal();
    if (std.debug.runtime_safety) try testing.expect(!set.traversal_active);
}

test "an epoch bitset pool hands out empty sets that keep their words, nested or not" {
    var pool = EpochBitSetPool(u32, .insertion).init(testing.allocator);
    defer pool.deinit();
    var outer = pool.acquire();
    try outer.insert(400);
    var inner = pool.acquire();
    try testing.expect(!inner.contains(400));
    try inner.insert(7);
    pool.release(&inner);
    try testing.expect(outer.contains(400));
    pool.release(&outer);
    var again = pool.acquire();
    defer pool.release(&again);
    try testing.expect(again.words.len > 400 / 64);
    try testing.expect(!again.contains(400));
    try testing.expect(!again.contains(7));
    try testing.expectEqual(@as(usize, 0), again.count());
}

fn exerciseBitSetAllocationFailures(gpa: Allocator) (Allocator.Error || error{TestUnexpectedResult})!void {
    var set = EpochBitSet(u32, .insertion).init(gpa);
    defer set.deinit();
    for ([_]u32{ 5, 900, 6, 5000, 900 }) |key| _ = try set.testAndInsert(key);
    try testing.expect(set.contains(5000));
    try testing.expect(set.count() == 4);
}

test "an epoch bitset stays consistent when an allocation fails" {
    try testing.checkAllAllocationFailures(testing.allocator, exerciseBitSetAllocationFailures, .{});
}

test "an epoch map allocates only the pages its keys touch" {
    var map = EpochMap(u32, u32).init(testing.allocator);
    defer map.deinit();
    try testing.expectEqual(@as(?u32, null), map.get(100_000));
    try testing.expectEqual(@as(usize, 0), map.allocated_pages);
    try map.put(100_000, 1);
    try map.put(100_001, 2);
    try testing.expectEqual(@as(usize, 1), map.allocated_pages);
    try map.put(3, 3);
    try testing.expectEqual(@as(usize, 2), map.allocated_pages);
    try testing.expectEqual(@as(?u32, null), map.get(4));
    try testing.expectEqual(@as(?u32, null), map.get(50_000));
    map.clearRetainingCapacity();
    try testing.expectEqual(@as(?u32, null), map.get(100_000));
    try map.put(100_002, 4);
    try testing.expectEqual(@as(usize, 2), map.allocated_pages);
    try testing.expectEqual(@as(?u32, 4), map.get(100_002));
    try testing.expectEqual(@as(?u32, null), map.get(100_001));
}

fn exerciseEpochMapAllocationFailures(gpa: Allocator) (Allocator.Error || error{TestUnexpectedResult})!void {
    var map = EpochMap(u32, u32).init(gpa);
    defer map.deinit();
    for ([_]u32{ 5, 900, 6, 5000, 900 }) |key| try map.put(key, key + 1);
    try testing.expect(map.get(5000) == 5001);
    try testing.expect(map.count() == 4);
}

test "an epoch map stays consistent when an allocation fails" {
    try testing.checkAllAllocationFailures(testing.allocator, exerciseEpochMapAllocationFailures, .{});
}
