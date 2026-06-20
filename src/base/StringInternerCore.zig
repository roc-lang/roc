//! The shared open-addressing dedup core behind both string interners.
//!
//! `SmallStringInterner` (byte-offset ids, backing `Ident`) and
//! `SerialStringInterner` (dense serial ids, backing `CanonicalNameStore`) are the
//! same data structure — a deduplicating string→id map implemented as a
//! linear-probing hash table over a flat byte buffer — differing ONLY in how an id
//! maps to its text and how a hash-table cell encodes an id. That difference is
//! captured by an `Encoding` policy; the probe/insert/resize/lookup algorithm (the
//! identical, bug-prone part) lives here once, parameterized by it.
//!
//! Each interner keeps its own storage layout and (de)serialization, which are
//! genuinely encoding-specific (a `ranges` array for serial ids, null-terminated
//! bytes for offset ids). The `Encoding` an interner passes provides:
//!
//!   - `Id`: the public id type (an `enum(u32)`).
//!   - `Cell`: the hash-table cell type; `empty_cell` marks an empty slot.
//!   - `initial_index_capacity`: table size to lazily allocate on first insert.
//!   - `fn count(self) u32`: number of interned entries (load-factor input).
//!   - `fn cellForId(id) Cell` / `fn idFromCell(cell) Id`: cell ⇄ id (cell ≠ empty).
//!   - `fn appendEntry(self, gpa, string) !Id`: store a NEW entry's text, return its id.
//!   - `fn textForId(self, id) []const u8`: the text for an id (for probe comparison).
//!
//! `self` must expose `index: SafeList(Encoding.Cell)` (the probe table) and
//! `supports_inserts: bool` (false once frozen/deserialized).

const std = @import("std");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const SafeList = collections.SafeList;

/// Result of a probe: the existing id if the string is interned, plus the slot
/// where it lives (when found) or where it should be inserted (when not).
pub fn FindResult(comptime Id: type) type {
    return struct { idx: ?Id, slot: u64 };
}

/// Find `string` in the table via linear probing. Returns the existing id (or null)
/// and the relevant slot. Safe on a frozen interner (pure read).
pub fn findStringOrSlot(comptime Enc: type, self: anytype, string: []const u8) FindResult(Enc.Id) {
    const table_size = self.index.len();
    if (table_size == 0) return .{ .idx = null, .slot = 0 };

    const hash = std.hash.Fnv1a_32.hash(string);
    var slot: u64 = @intCast(hash % table_size);
    while (true) {
        const cell = self.index.items.items[@intCast(slot)];
        if (cell == Enc.empty_cell) return .{ .idx = null, .slot = slot };

        const id = Enc.idFromCell(cell);
        if (std.mem.eql(u8, string, Enc.textForId(self, id))) return .{ .idx = id, .slot = slot };

        slot = (slot + 1) % table_size;
    }
}

/// Look up `string`'s id without modifying the interner.
pub fn lookup(comptime Enc: type, self: anytype, string: []const u8) ?Enc.Id {
    return findStringOrSlot(Enc, self, string).idx;
}

/// Whether `string` is already interned.
pub fn contains(comptime Enc: type, self: anytype, string: []const u8) bool {
    return findStringOrSlot(Enc, self, string).idx != null;
}

/// Double the table and rehash every live entry into it.
pub fn resizeHashTable(comptime Enc: type, self: anytype, gpa: Allocator) Allocator.Error!void {
    const old_table = self.index;
    const new_size: usize = @intCast(old_table.len() * 2);

    self.index = try SafeList(Enc.Cell).initCapacity(gpa, new_size);
    try self.index.items.ensureTotalCapacityPrecise(gpa, new_size);
    self.index.items.items.len = new_size;
    @memset(self.index.items.items, Enc.empty_cell);

    for (old_table.items.items) |cell| {
        if (cell != Enc.empty_cell) {
            const id = Enc.idFromCell(cell);
            const slot = findStringOrSlot(Enc, self, Enc.textForId(self, id)).slot;
            self.index.items.items[@intCast(slot)] = cell;
        }
    }

    @constCast(&old_table).deinit(gpa);
}

/// Intern `string`, returning its id. Deduplicates: equal text always returns the
/// same id. Lazily allocates the probe table on first insert (so a default-empty
/// interner is valid), and grows it past an 80% load factor. The probe table is the
/// only thing this touches directly; new-entry text storage and id allocation are
/// the `Encoding`'s job (`appendEntry`).
pub fn insert(comptime Enc: type, self: anytype, gpa: Allocator, string: []const u8) Allocator.Error!Enc.Id {
    const found = findStringOrSlot(Enc, self, string);
    if (found.idx) |existing| return existing;

    // A frozen (deserialized) interner aliases buffer memory and cannot grow; an
    // insert of a string it does not already contain is a bug in the caller.
    std.debug.assert(self.supports_inserts);

    var table_changed = false;
    if (self.index.len() == 0) {
        try self.index.items.ensureTotalCapacityPrecise(gpa, Enc.initial_index_capacity);
        self.index.items.items.len = Enc.initial_index_capacity;
        @memset(self.index.items.items, Enc.empty_cell);
        table_changed = true;
    } else if ((Enc.count(self) + 1) * 5 >= self.index.len() * 4) {
        try resizeHashTable(Enc, self, gpa);
        table_changed = true;
    }

    const id = try Enc.appendEntry(self, gpa, string);

    // Appending an entry never touches the probe table, so reuse the empty slot from
    // the initial probe; re-probe only when the table was just (re)allocated.
    const slot = if (table_changed) findStringOrSlot(Enc, self, string).slot else found.slot;
    self.index.items.items[@intCast(slot)] = Enc.cellForId(id);
    return id;
}
