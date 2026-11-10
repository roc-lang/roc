//! What one module's checking can change an anonymous record or tag union
//! into, reduced to what decides whether a use of a binding can change it
//! (design.md "Anonymous Structure Is Copied Per Use").
//!
//! A use changes an anonymous tag union only by lifting it into a nominal
//! type, and relating the two lifts only when the declaration's backing is a
//! tag union whose row is open or lists every tag the anonymous union lists
//! (`unifyTagUnionWithNominal`). A use changes an anonymous record by lifting
//! it into a nominal type whose backing is a record (`unifyRecordWithNominal`)
//! or by widening it with an optional or defaulted field it lacks. Lifting
//! into a closed backing needs every field of the record the backing cannot
//! absorb to be one the backing lists, and a backing absorbs only optional
//! and defaulted fields. A
//! widening field's name was written in some module, as a `?:` or defaulted
//! field of an annotation, a `.?` access, or an unset, and reaches every
//! module that can relate to it through the type stores the module copies
//! types from.
//!
//! An anonymous structure nothing a module can see could change is therefore
//! never changed by any relation in that module, so every use can share it.
//! Names are compared by a hash of their text, as they come from several
//! modules' ident stores; a collision only makes a structure look changeable,
//! which costs a copy and never a wrong type.

const std = @import("std");
const base = @import("base");

const types_mod = @import("types.zig");
const store_mod = @import("store.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Var = types_mod.Var;
const Store = store_mod.Store;

/// The hash a tag or field name is compared by.
pub fn nameHash(text: []const u8) u64 {
    return std.hash.Wyhash.hash(0, text);
}

/// Closed name sets, each the names a backing lists, with a query for
/// whether some set lists every name of a given group.
pub const NameSets = struct {
    /// Some backing of this kind exists, so a structure with no names that
    /// count can lift into it.
    any: bool = false,
    /// Some backing can take in a structure whatever names it lists: its
    /// row is open, it is one of its declaration's own type variables, or
    /// the declaration is invalid (relating to it poisons the structure).
    every: bool = false,
    /// The sorted name hashes of each closed backing, back to back.
    names: std.ArrayListUnmanaged(u64) = .empty,
    /// Each closed backing's slice of `names`.
    sets: std.ArrayListUnmanaged(Set) = .empty,
    /// For each name hash, the first link of the list of sets listing it.
    first_link: std.AutoHashMapUnmanaged(u64, u32) = .empty,
    links: std.ArrayListUnmanaged(Link) = .empty,

    const Set = struct { start: u32, len: u32 };
    const Link = struct { set: u32, next: u32 };
    const no_link = std.math.maxInt(u32);

    pub fn deinit(self: *NameSets, gpa: Allocator) void {
        self.names.deinit(gpa);
        self.sets.deinit(gpa);
        self.first_link.deinit(gpa);
        self.links.deinit(gpa);
    }

    /// Record a closed backing listing the names whose hashes are `hashes`,
    /// which this sorts and deduplicates in place.
    pub fn addClosed(self: *NameSets, gpa: Allocator, hashes: []u64) Allocator.Error!void {
        self.any = true;
        if (hashes.len == 0) return;
        const unique = sortUnique(hashes);
        const set_index: u32 = std.math.cast(u32, self.sets.items.len) orelse return error.OutOfMemory;
        const start: u32 = std.math.cast(u32, self.names.items.len) orelse return error.OutOfMemory;
        try self.names.appendSlice(gpa, unique);
        try self.sets.append(gpa, .{ .start = start, .len = @intCast(unique.len) });
        for (unique) |hash| {
            const entry = try self.first_link.getOrPut(gpa, hash);
            const next = if (entry.found_existing) entry.value_ptr.* else no_link;
            entry.value_ptr.* = std.math.cast(u32, self.links.items.len) orelse return error.OutOfMemory;
            try self.links.append(gpa, .{ .set = set_index, .next = next });
        }
    }

    /// Whether a structure whose counted names hash to `hashes` can lift
    /// into some recorded backing.
    pub fn mayTakeIn(self: *const NameSets, hashes: []const u64) bool {
        if (self.every) return true;
        if (!self.any) return false;
        if (hashes.len == 0) return true;
        var link = self.first_link.get(hashes[0]) orelse return false;
        while (link != no_link) : (link = self.links.items[link].next) {
            const set = self.sets.items[self.links.items[link].set];
            const listed = self.names.items[set.start..][0..set.len];
            for (hashes[1..]) |hash| {
                if (!sortedContains(listed, hash)) break;
            } else return true;
        }
        return false;
    }
};

/// Everything one module's checking can change an anonymous structure into.
pub const UseAbsorbers = struct {
    /// Declarations whose backing is a tag union, by the tags it lists.
    tags: NameSets = .{},
    /// Declarations whose backing is a record or empty record, by the
    /// fields it lists.
    records: NameSets = .{},
    /// The name of every optional or defaulted field the module can relate
    /// a record to, which a relation can add to a record lacking it.
    widening_fields: std.AutoHashMapUnmanaged(u64, void) = .empty,
    /// Scratch for one query's or one backing's name hashes.
    scratch: std.ArrayListUnmanaged(u64) = .empty,

    pub fn deinit(self: *UseAbsorbers, gpa: Allocator) void {
        self.tags.deinit(gpa);
        self.records.deinit(gpa);
        self.widening_fields.deinit(gpa);
        self.scratch.deinit(gpa);
    }

    /// A declaration invalid in its module, which relating to poisons any
    /// structure.
    pub fn addInvalidDecl(self: *UseAbsorbers) void {
        self.tags.every = true;
        self.records.every = true;
    }

    /// Record a field name a relation can widen a record with.
    pub fn addWideningField(self: *UseAbsorbers, gpa: Allocator, text: []const u8) Allocator.Error!void {
        try self.widening_fields.put(gpa, nameHash(text), {});
    }

    /// Record everything in `store` this module can relate to: each
    /// declaration a module other than the declaring one can lift into
    /// (opaque declarations are left out when `include_opaque` is false),
    /// and the name of each optional or defaulted field of a record.
    pub fn addStore(
        self: *UseAbsorbers,
        gpa: Allocator,
        store: *const Store,
        idents: *const Ident.Store,
        include_opaque: bool,
    ) Allocator.Error!void {
        for (store.nominal_decls.items.items) |decl| {
            if (!decl.isValid()) {
                self.addInvalidDecl();
                continue;
            }
            if (!include_opaque and decl.source.isOpaque()) continue;
            try self.addBacking(gpa, store, idents, decl.backing);
        }
        // A checked module's store lists the name of every optional or
        // defaulted field its records hold (`Store.widening_names`), filled
        // when checking sealed it.
        for (store.widening_names.items.items) |hash| try self.widening_fields.put(gpa, hash, {});
    }

    fn addBacking(self: *UseAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!void {
        switch (try self.classifyBacking(gpa, store, idents, backing)) {
            .none => {},
            .every => self.addInvalidDecl(),
            .open_tags => self.tags.every = true,
            .open_records => self.records.every = true,
            .tags => try self.tags.addClosed(gpa, self.scratch.items),
            .records => try self.records.addClosed(gpa, self.scratch.items),
        }
    }

    const BackingKind = enum {
        /// No anonymous structure lifts into it.
        none,
        /// Every anonymous structure can lift into it.
        every,
        /// A tag union with an open row, which every tag union can lift into.
        open_tags,
        /// A record with an open row, which every record can lift into.
        open_records,
        /// A closed tag union listing the name hashes left in `scratch`.
        tags,
        /// A closed record listing the name hashes left in `scratch`.
        records,
    };

    fn classifyBacking(self: *UseAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!BackingKind {
        self.scratch.clearRetainingCapacity();
        switch (store.resolveVar(backing).desc.content) {
            .flex, .rigid, .err => return .every,
            .alias, .field_presence => return .none,
            .structure => |flat| switch (flat) {
                .empty_tag_union => return .tags,
                .tag_union => |tag_union| {
                    var row = tag_union;
                    while (true) {
                        for (store.getTagsSlice(row.tags).items(.name)) |name| {
                            try self.scratch.append(gpa, nameHash(idents.getText(name)));
                        }
                        switch (store.resolveVar(row.ext).desc.content) {
                            .structure => |ext| switch (ext) {
                                .empty_tag_union => return .tags,
                                .tag_union => |next| row = next,
                                else => return .open_tags,
                            },
                            else => return .open_tags,
                        }
                    }
                },
                .empty_record => return .records,
                .record => |record| {
                    var row = record;
                    while (true) {
                        for (store.getRecordFieldsSlice(row.fields).items(.name)) |name| {
                            try self.scratch.append(gpa, nameHash(idents.getText(name)));
                        }
                        switch (store.resolveVar(row.ext).desc.content) {
                            .structure => |ext| switch (ext) {
                                .empty_record => return .records,
                                .record => |next| row = next,
                                else => return .open_records,
                            },
                            else => return .open_records,
                        }
                    }
                },
                .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound => return .none,
            },
        }
    }

    /// Whether every lift into `backing`, a backing in `store`, is one these
    /// absorbers already allow.
    pub fn coversBacking(self: *UseAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!bool {
        return switch (try self.classifyBacking(gpa, store, idents, backing)) {
            .none => true,
            .every => self.tags.every and self.records.every,
            .open_tags => self.tags.every,
            .open_records => self.records.every,
            .tags => coversSet(&self.tags, self.scratch.items),
            .records => coversSet(&self.records, self.scratch.items),
        };
    }

    /// Whether every optional or defaulted field of `record`, a record in
    /// `store`, is one these absorbers allow to widen with.
    pub fn coversRecordFields(self: *const UseAbsorbers, store: *const Store, idents: *const Ident.Store, record: types_mod.Record) bool {
        const fields = store.getRecordFieldsSlice(record.fields);
        for (fields.items(.name), fields.items(.presence)) |name, presence| {
            if (kindOf(store, presence) == .may_widen and !self.widening_fields.contains(nameHash(idents.getText(name)))) return false;
        }
        return true;
    }

    fn coversSet(sets: *const NameSets, hashes: []u64) bool {
        if (sets.every) return true;
        if (!sets.any) return false;
        if (hashes.len == 0) return true;
        return sets.mayTakeIn(sortUnique(hashes));
    }

    /// Whether the class `desc` is anonymous structure a use can change and
    /// so must copy (`DescriptorFlags.per_use_structure`).
    pub fn copiedPerUse(
        self: *UseAbsorbers,
        gpa: Allocator,
        store: *const Store,
        idents: *const Ident.Store,
        desc: types_mod.Descriptor,
    ) Allocator.Error!bool {
        if (!desc.liftsIntoNominal()) return false;
        switch (desc.content.structure) {
            .empty_tag_union => return self.tags.any or self.tags.every,
            .tag_union => |tag_union| {
                if (self.tags.every) return true;
                if (!self.tags.any) return false;
                self.scratch.clearRetainingCapacity();
                for (store.getTagsSlice(tag_union.tags).items(.name)) |name| {
                    try self.scratch.append(gpa, nameHash(idents.getText(name)));
                }
                return self.tags.mayTakeIn(sortUnique(self.scratch.items));
            },
            .empty_record => return self.records.any or self.records.every or self.widening_fields.count() != 0,
            .record => |record| {
                const fields = store.getRecordFieldsSlice(record.fields);
                // Widening adds a field the record lacks. The record's own
                // fields stand for its whole row: a field further along the
                // row only removes widenings, so leaving it out is safe.
                if (self.widening_fields.count() != 0) {
                    self.scratch.clearRetainingCapacity();
                    for (fields.items(.name)) |name| try self.scratch.append(gpa, nameHash(idents.getText(name)));
                    const own = sortUnique(self.scratch.items);
                    var widening = self.widening_fields.keyIterator();
                    while (widening.next()) |hash| {
                        if (!sortedContains(own, hash.*)) return true;
                    }
                }
                if (self.records.every) return true;
                if (!self.records.any) return false;
                // Lifting into a closed backing needs every field the
                // backing could not absorb to be listed. The backing absorbs
                // only an optional or defaulted field, and a field whose
                // kind is unsettled becomes one only by relating to a
                // widening field of its name.
                self.scratch.clearRetainingCapacity();
                for (fields.items(.name), fields.items(.presence)) |name, presence| {
                    const hash = nameHash(idents.getText(name));
                    const absorbable = switch (kindOf(store, presence)) {
                        .required => false,
                        .may_widen => true,
                        .unsettled => self.widening_fields.contains(hash),
                    };
                    if (!absorbable) try self.scratch.append(gpa, hash);
                }
                return self.records.mayTakeIn(sortUnique(self.scratch.items));
            },
            else => unreachable,
        }
    }
};

const FieldKind = enum {
    /// A required field, which a closed backing must list.
    required,
    /// An optional or defaulted field, which can widen a record lacking it.
    may_widen,
    /// A field whose kind is not settled yet.
    unsettled,
};

fn kindOf(store: *const Store, presence: types_mod.RecordField.Presence) FieldKind {
    return switch (presence.decode()) {
        .required => .required,
        .unknown => |unknown| switch (store.resolveVar(unknown.presence).desc.content) {
            .field_presence => |kind| switch (kind) {
                .required => .required,
                .optional, .defaulted => .may_widen,
            },
            else => .unsettled,
        },
    };
}

fn sortUnique(hashes: []u64) []u64 {
    std.mem.sortUnstable(u64, hashes, {}, std.sort.asc(u64));
    var len: usize = 0;
    for (hashes) |hash| {
        if (len != 0 and hashes[len - 1] == hash) continue;
        hashes[len] = hash;
        len += 1;
    }
    return hashes[0..len];
}

fn sortedContains(sorted: []const u64, hash: u64) bool {
    var low: usize = 0;
    var high: usize = sorted.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (sorted[mid] == hash) return true;
        if (sorted[mid] < hash) low = mid + 1 else high = mid;
    }
    return false;
}
