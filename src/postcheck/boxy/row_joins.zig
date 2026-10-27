//! Row joins for `.boxy` (design.md "Row Joins"): a union-find over Boxy
//! types, seeded from the explicit flow edges of the checked bodies, that
//! makes rows flowing into one another one row. Every ground member of a
//! join set takes the set's representative, whose tag rows list every
//! member's tags.

const std = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const TypeId = types.TypeId;

pub const Joins = struct {
    allocator: Allocator,
    /// Union-find parent of each joined type; absent types are their own
    /// singleton set.
    parent: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty,
    /// The representative of each joined ground type, filled by `seal`.
    representatives: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty,
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
                        if (other.name != tag.name) continue;
                        const mine = store.span(tag.payloads);
                        const theirs = store.span(other.payloads);
                        if (mine.len != theirs.len) continue;
                        for (mine, theirs) |x, y| try pending.append(self.allocator, .{ x, y });
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

    /// Elect every set's representative. A set whose ground members are tag
    /// unions is represented by the tag union listing every member's tags,
    /// in label order; any other set by its least member.
    pub fn seal(self: *Joins, store: *types.Store) Allocator.Error!void {
        self.sealed = true;
        var members: std.AutoHashMapUnmanaged(TypeId, std.ArrayList(TypeId)) = .empty;
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
                    if (existing.name == tag.name and existing.payloads.len == payloads.len) present = true;
                }
                if (!present) try tags.append(self.allocator, .{ .name = tag.name, .payloads = payloads });
            }
        }
        std.mem.sort(types.TagInput, tags.items, store, struct {
            fn lessThan(s: *types.Store, x: types.TagInput, y: types.TagInput) bool {
                return std.mem.order(u8, s.nameText(x.name), s.nameText(y.name)) == .lt;
            }
        }.lessThan);
        return try store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = null } });
    }

    /// The type whose layout a ground type takes.
    pub fn representative(self: *const Joins, ty: TypeId) TypeId {
        return self.representatives.get(ty) orelse ty;
    }
};
