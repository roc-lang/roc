//! Row joins for `.boxy` (design.md "Row Joins"): a union-find over Boxy
//! types, seeded from the explicit flow edges of the checked bodies, that
//! makes rows flowing into one another one row. Every ground member of a
//! join set takes the set's representative, whose tag rows list every
//! member's tags.

const std = @import("std");
const collections = @import("collections");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const TypeId = types.TypeId;

/// The program's row joins over Boxy types.
pub const Joins = struct {
    allocator: Allocator,
    /// Union-find parent of each joined type; absent types are their own
    /// singleton set.
    parent: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty,
    /// The representative of each joined ground type, filled by `seal`.
    representatives: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty,
    sealed: bool = false,

    pub fn init(allocator: Allocator) Joins {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Joins) void {
        self.parent.deinit(self.allocator);
        self.representatives.deinit(self.allocator);
    }

    fn find(self: *Joins, ty: TypeId) TypeId {
        var current = ty;
        while (self.parent.get(current)) |next| {
            if (next == current) break;
            current = next;
        }
        return current;
    }

    /// Record that values of `a` flow to positions of `b` (or back): the
    /// pair, and every pair at matching positions under them, join.
    pub fn join(self: *Joins, store: *const types.Store, a: TypeId, b: TypeId) Allocator.Error!void {
        if (self.sealed) types.invariant("a row join was recorded after the joins were sealed");
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ a, b });
        while (pending.pop()) |pair| {
            // A variable is no flow of any one type: it joins nothing.
            if (store.get(pair[0]) == .variable or store.get(pair[1]) == .variable) continue;
            const left = self.find(pair[0]);
            const right = self.find(pair[1]);
            if (left == right) continue;
            const lo = if (@backingInt(left) < @backingInt(right)) left else right;
            const hi = if (lo == left) right else left;
            try self.parent.put(self.allocator, hi, lo);
            try self.parent.put(self.allocator, lo, lo);
            try self.descend(store, &pending, pair[0], pair[1]);
        }
    }

    fn descend(self: *Joins, store: *const types.Store, pending: *std.ArrayList([2]TypeId), a: TypeId, b: TypeId) Allocator.Error!void {
        const left = store.get(a);
        const right = store.get(b);
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return;
        switch (left) {
            .list => |elem| try pending.append(self.allocator, .{ elem, right.list }),
            .box => |payload| try pending.append(self.allocator, .{ payload, right.box }),
            .tuple => |items| {
                const other = store.span(right.tuple);
                const mine = store.span(items);
                if (mine.len != other.len) return;
                for (mine, other) |x, y| try pending.append(self.allocator, .{ x, y });
            },
            .record => |record| {
                for (store.fieldSpan(record.fields)) |field| {
                    for (store.fieldSpan(right.record.fields)) |other| {
                        if (other.name == field.name) try pending.append(self.allocator, .{ field.ty, other.ty });
                    }
                }
            },
            .tag_union => |tag_union| {
                for (store.tagSpan(tag_union.tags)) |tag| {
                    for (store.tagSpan(right.tag_union.tags)) |other| {
                        // Same-label tags are one variant, whose payloads
                        // join, only when their payloads are compatible.
                        if (other.name != tag.name) continue;
                        if (!try self.payloadsCompatible(store, tag, other)) continue;
                        for (store.span(tag.payloads), store.span(other.payloads)) |x, y| try pending.append(self.allocator, .{ x, y });
                    }
                }
            },
            .nominal => |nominal| {
                if (nominal.decl.module != right.nominal.decl.module or nominal.decl.decl != right.nominal.decl.decl) return;
                for (store.nominalArgs(nominal), store.nominalArgs(right.nominal)) |x, y| try pending.append(self.allocator, .{ x, y });
            },
            .function => |function| {
                const mine = store.span(function.args);
                const theirs = store.span(right.function.args);
                if (mine.len != theirs.len) return;
                for (mine, theirs) |x, y| try pending.append(self.allocator, .{ x, y });
                try pending.append(self.allocator, .{ function.ret, right.function.ret });
            },
            .primitive, .empty_record, .variable => {},
        }
    }

    /// Whether two same-label tags are one variant: equally many payloads,
    /// pairwise compatible.
    pub fn payloadsCompatible(self: *Joins, store: *const types.Store, a: types.Tag, b: types.Tag) Allocator.Error!bool {
        const mine = store.span(a.payloads);
        const theirs = store.span(b.payloads);
        if (mine.len != theirs.len) return false;
        for (mine, theirs) |x, y| {
            if (!try compatible(self.allocator, store, x, y)) return false;
        }
        return true;
    }

    /// Elect every set's representative. A set whose ground members are tag
    /// unions is represented by the tag union listing every member's tags,
    /// in label order; any other set by its least member.
    pub fn seal(self: *Joins, store: *types.Store) Allocator.Error!void {
        self.sealed = true;
        var members: collections.DenseMapUnmanaged(TypeId, std.ArrayList(TypeId)) = .empty;
        defer {
            var it = members.valueIterator();
            while (it.next()) |list| list.deinit(self.allocator);
            members.deinit(self.allocator);
        }
        var keys = self.parent.keyIterator();
        while (keys.next()) |key| {
            if (!store.isClosed(key.*)) continue;
            const root = self.find(key.*);
            const entry = try members.getOrPut(self.allocator, root);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.append(self.allocator, key.*);
        }
        var sets = members.iterator();
        while (sets.next()) |set| {
            const list = set.value_ptr.items;
            std.mem.sort(TypeId, list, {}, struct {
                fn lessThan(_: void, x: TypeId, y: TypeId) bool {
                    return @backingInt(x) < @backingInt(y);
                }
            }.lessThan);
            const rep = try self.representativeOf(store, list);
            for (list) |member| try self.representatives.put(self.allocator, member, rep);
        }
    }

    fn representativeOf(self: *Joins, store: *types.Store, members: []const TypeId) Allocator.Error!TypeId {
        var all_tag_unions = true;
        for (members) |member| {
            if (store.get(member) != .tag_union) all_tag_unions = false;
        }
        if (!all_tag_unions or members.len == 1) return members[0];
        var tags: std.ArrayList(types.TagInput) = .empty;
        defer tags.deinit(self.allocator);
        for (members) |member| {
            const tag_union = store.get(member).tag_union;
            for (store.tagSpan(tag_union.tags)) |tag| {
                const payloads = store.span(tag.payloads);
                var present = false;
                for (tags.items) |existing| {
                    if (existing.name != tag.name or existing.payloads.len != payloads.len) continue;
                    var same = true;
                    for (existing.payloads, payloads) |x, y| {
                        if (!try compatible(self.allocator, store, x, y)) same = false;
                    }
                    if (same) present = true;
                }
                if (!present) try tags.append(self.allocator, .{ .name = tag.name, .payloads = payloads });
            }
        }
        // Labels in text order; a label's incompatible variants by their
        // payloads' identities.
        std.mem.sort(types.TagInput, tags.items, store, struct {
            fn lessThan(s: *types.Store, x: types.TagInput, y: types.TagInput) bool {
                switch (std.mem.order(u8, s.nameText(x.name), s.nameText(y.name))) {
                    .lt => return true,
                    .gt => return false,
                    .eq => {},
                }
                if (x.payloads.len != y.payloads.len) return x.payloads.len < y.payloads.len;
                for (x.payloads, y.payloads) |a, b| {
                    if (a != b) return @backingInt(a) < @backingInt(b);
                }
                return false;
            }
        }.lessThan);
        return try store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = null } });
    }

    /// The type whose layout a ground type takes.
    pub fn representative(self: *const Joins, ty: TypeId) TypeId {
        return self.representatives.get(ty) orelse ty;
    }
};

/// Whether two types could unify without a structural conflict: the same
/// kind of type, the same primitive, records with the same fields, tuples
/// and functions of the same arity, the same nominal; a variable is
/// compatible with anything.
pub fn compatible(allocator: Allocator, store: *const types.Store, a: TypeId, b: TypeId) Allocator.Error!bool {
    var pending: std.ArrayList([2]TypeId) = .empty;
    defer pending.deinit(allocator);
    var seen: std.AutoHashMapUnmanaged(struct { TypeId, TypeId }, void) = .empty;
    defer seen.deinit(allocator);
    try pending.append(allocator, .{ a, b });
    while (pending.pop()) |pair| {
        if (pair[0] == pair[1]) continue;
        if ((try seen.getOrPut(allocator, .{ pair[0], pair[1] })).found_existing) continue;
        const left = store.get(pair[0]);
        const right = store.get(pair[1]);
        if (left == .variable or right == .variable) continue;
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
        switch (left) {
            .primitive => |primitive| if (primitive != right.primitive) return false,
            .empty_record => {},
            .list => |elem| try pending.append(allocator, .{ elem, right.list }),
            .box => |payload| try pending.append(allocator, .{ payload, right.box }),
            .tuple => |items| {
                const mine = store.span(items);
                const theirs = store.span(right.tuple);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| try pending.append(allocator, .{ x, y });
            },
            .record => |record| {
                const mine = store.fieldSpan(record.fields);
                const theirs = store.fieldSpan(right.record.fields);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| {
                    if (x.name != y.name) return false;
                    try pending.append(allocator, .{ x.ty, y.ty });
                }
            },
            .tag_union => |tag_union| {
                // Rows merge their tags; only shared labels at one arity
                // must agree.
                for (store.tagSpan(tag_union.tags)) |tag| {
                    for (store.tagSpan(right.tag_union.tags)) |other| {
                        if (tag.name != other.name or tag.payloads.len != other.payloads.len) continue;
                        for (store.span(tag.payloads), store.span(other.payloads)) |x, y| try pending.append(allocator, .{ x, y });
                    }
                }
            },
            .nominal => |nominal| {
                if (nominal.decl.module != right.nominal.decl.module or nominal.decl.decl != right.nominal.decl.decl) return false;
                for (store.nominalArgs(nominal), store.nominalArgs(right.nominal)) |x, y| try pending.append(allocator, .{ x, y });
            },
            .function => |function| {
                const mine = store.span(function.args);
                const theirs = store.span(right.function.args);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| try pending.append(allocator, .{ x, y });
                try pending.append(allocator, .{ function.ret, right.function.ret });
            },
            .variable => unreachable, // answered above
        }
    }
    return true;
}
