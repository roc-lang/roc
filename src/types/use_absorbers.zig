//! The nominal type declarations one module's checking can lift an anonymous
//! tag union into, reduced to what decides whether a lift is possible
//! (design.md "Anonymous Structure Is Copied Per Use").
//!
//! Relating an anonymous tag union to a nominal type lifts it only when the
//! declaration's backing is itself a tag union whose row either is open or
//! lists every tag the anonymous union lists (`unifyTagUnionWithNominal`).
//! An anonymous tag union that no declaration a module can see could take in
//! is therefore never changed by any relation in that module, so every use
//! can share it. Names are compared by a hash of their text, as declarations
//! come from several modules' ident stores; a collision only makes a union
//! look liftable, which costs a copy and never a wrong type.

const std = @import("std");
const base = @import("base");

const types_mod = @import("types.zig");
const store_mod = @import("store.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Var = types_mod.Var;
const Store = store_mod.Store;

/// Lift-relevant facts about every declaration a module's checking can see.
pub const TagUnionAbsorbers = struct {
    /// Some declaration's backing is a tag union, so an anonymous union with
    /// no tags of its own can lift into it.
    any: bool = false,
    /// Some declaration can take in a tag union whatever tags it lists: its
    /// backing row is open, its backing is one of its own type variables, or
    /// it is invalid (relating to it poisons the union).
    every: bool = false,
    /// The sorted name hashes of each closed backing, back to back.
    names: std.ArrayListUnmanaged(u64) = .empty,
    /// Each closed backing's slice of `names`.
    sets: std.ArrayListUnmanaged(Set) = .empty,
    /// For each name hash, the first link of the list of closed backings
    /// listing it.
    first_link: std.AutoHashMapUnmanaged(u64, u32) = .empty,
    links: std.ArrayListUnmanaged(Link) = .empty,
    /// Scratch for one query's or one backing's name hashes.
    scratch: std.ArrayListUnmanaged(u64) = .empty,

    const Set = struct { start: u32, len: u32 };
    const Link = struct { set: u32, next: u32 };
    const no_link = std.math.maxInt(u32);

    pub fn deinit(self: *TagUnionAbsorbers, gpa: Allocator) void {
        self.names.deinit(gpa);
        self.sets.deinit(gpa);
        self.first_link.deinit(gpa);
        self.links.deinit(gpa);
        self.scratch.deinit(gpa);
    }

    /// The hash a tag name is compared by.
    pub fn nameHash(text: []const u8) u64 {
        return std.hash.Wyhash.hash(0, text);
    }

    /// Record a declaration whose backing is a closed tag union listing the
    /// tags whose name hashes are in `self.scratch`.
    pub fn addClosedFromScratch(self: *TagUnionAbsorbers, gpa: Allocator) Allocator.Error!void {
        self.any = true;
        const hashes = self.scratch.items;
        if (hashes.len == 0) return;
        std.mem.sortUnstable(u64, hashes, {}, std.sort.asc(u64));
        var len: usize = 0;
        for (hashes) |hash| {
            if (len != 0 and hashes[len - 1] == hash) continue;
            hashes[len] = hash;
            len += 1;
        }
        const set_index: u32 = std.math.cast(u32, self.sets.items.len) orelse return error.OutOfMemory;
        const start: u32 = std.math.cast(u32, self.names.items.len) orelse return error.OutOfMemory;
        try self.names.appendSlice(gpa, hashes[0..len]);
        try self.sets.append(gpa, .{ .start = start, .len = @intCast(len) });
        for (hashes[0..len]) |hash| {
            const entry = try self.first_link.getOrPut(gpa, hash);
            const next = if (entry.found_existing) entry.value_ptr.* else no_link;
            entry.value_ptr.* = std.math.cast(u32, self.links.items.len) orelse return error.OutOfMemory;
            try self.links.append(gpa, .{ .set = set_index, .next = next });
        }
    }

    /// Record every declaration in `store`'s declaration table that a module
    /// other than the declaring one can lift into: opaque declarations are
    /// left out when `include_opaque` is false.
    pub fn addStoreDecls(
        self: *TagUnionAbsorbers,
        gpa: Allocator,
        store: *const Store,
        idents: *const Ident.Store,
        include_opaque: bool,
    ) Allocator.Error!void {
        for (store.nominal_decls.items.items) |decl| {
            if (!decl.isValid()) {
                self.every = true;
                continue;
            }
            if (!include_opaque and decl.source.isOpaque()) continue;
            try self.addBacking(gpa, store, idents, decl.backing);
        }
    }

    fn addBacking(self: *TagUnionAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!void {
        switch (try self.classifyBacking(gpa, store, idents, backing)) {
            .none => {},
            .every => self.every = true,
            .empty => self.any = true,
            .closed => try self.addClosedFromScratch(gpa),
        }
    }

    const BackingKind = enum {
        /// No anonymous tag union lifts into it.
        none,
        /// Every anonymous tag union can lift into it.
        every,
        /// An empty tag union.
        empty,
        /// A closed tag union listing the name hashes left in `scratch`.
        closed,
    };

    fn classifyBacking(self: *TagUnionAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!BackingKind {
        switch (store.resolveVar(backing).desc.content) {
            .flex, .rigid, .err => return .every,
            .alias, .field_presence => return .none,
            .structure => |flat| switch (flat) {
                .empty_tag_union => return .empty,
                .tag_union => |tag_union| {
                    self.scratch.clearRetainingCapacity();
                    var row = tag_union;
                    while (true) {
                        for (store.getTagsSlice(row.tags).items(.name)) |name| {
                            try self.scratch.append(gpa, nameHash(idents.getText(name)));
                        }
                        switch (store.resolveVar(row.ext).desc.content) {
                            .structure => |ext| switch (ext) {
                                .empty_tag_union => return .closed,
                                .tag_union => |next| row = next,
                                else => return .every,
                            },
                            else => return .every,
                        }
                    }
                },
                .record, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .empty_record => return .none,
            },
        }
    }

    /// Whether every lift into `backing`, a backing in `store`, is one these
    /// absorbers already allow.
    pub fn coversBacking(self: *TagUnionAbsorbers, gpa: Allocator, store: *const Store, idents: *const Ident.Store, backing: Var) Allocator.Error!bool {
        return switch (try self.classifyBacking(gpa, store, idents, backing)) {
            .none => true,
            .every => self.every,
            .empty => self.any or self.every,
            .closed => self.every or (self.any and (self.scratch.items.len == 0 or self.scratchListedBySomeSet())),
        };
    }

    /// Whether relating the anonymous tag union whose own tags are
    /// `own_tags` to some recorded declaration can lift it. Only the union's
    /// own tags count, as only they decide whether the lift proceeds.
    pub fn mayAbsorb(
        self: *TagUnionAbsorbers,
        gpa: Allocator,
        store: *const Store,
        idents: *const Ident.Store,
        own_tags: types_mod.Tag.SafeMultiList.Range,
    ) Allocator.Error!bool {
        if (self.every) return true;
        if (!self.any) return false;
        const names = store.getTagsSlice(own_tags).items(.name);
        if (names.len == 0) return true;
        self.scratch.clearRetainingCapacity();
        for (names) |name| try self.scratch.append(gpa, nameHash(idents.getText(name)));
        return self.scratchListedBySomeSet();
    }

    /// Whether some closed backing lists every name hash in `scratch`, which
    /// is not empty.
    fn scratchListedBySomeSet(self: *const TagUnionAbsorbers) bool {
        var link = self.first_link.get(self.scratch.items[0]) orelse return false;
        while (link != no_link) : (link = self.links.items[link].next) {
            const set = self.sets.items[self.links.items[link].set];
            const listed = self.names.items[set.start..][0..set.len];
            for (self.scratch.items[1..]) |hash| {
                if (!sortedContains(listed, hash)) break;
            } else return true;
        }
        return false;
    }

    /// Whether the class `desc` is anonymous structure a use can change and
    /// so must copy (`DescriptorFlags.per_use_structure`): a record or empty
    /// record, which a use can also widen, or a tag union some recorded
    /// declaration can take in.
    pub fn copiedPerUse(
        self: *TagUnionAbsorbers,
        gpa: Allocator,
        store: *const Store,
        idents: *const Ident.Store,
        desc: types_mod.Descriptor,
    ) Allocator.Error!bool {
        if (!desc.liftsIntoNominal()) return false;
        return switch (desc.content.structure) {
            .tag_union => |tag_union| try self.mayAbsorb(gpa, store, idents, tag_union.tags),
            .empty_tag_union => self.any or self.every,
            else => true,
        };
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
};
