//! Lambda-set instances: which instantiations of each generalized binding
//! group are one instance, and how a type reads under an instance
//! (design.md "Lambda Set Instances").
//!
//! Lambda solving generalizes each finished binding group over the callable
//! slots its function types own and records, for every reference to such a
//! group, the instantiation the reference was solved at
//! (`Ast.Polymorphism`). The solver's last step computes this table from
//! that record without changing the program: LIR lowering lowers each
//! function once per instance of its group, reading the function's own
//! types under the instance's substitution.
//!
//! Groups are visited callers first, so every instantiation of a group is
//! known when the group is visited: each was made at a reference in an
//! already visited body, or is an instantiation such a body names read under
//! one of that body's instances. An instantiation read under an instance is
//! an occurrence (`Occ`). Occurrences whose images are equal as infinite
//! types are one instance. The first instance of a group is the one every
//! type outside a further instance reads (the global environment); each
//! further instance is an environment (`Env`) of its own, whose types are
//! the group's own types with the instance's images for its scheme nodes.

const std = @import("std");
const collections = @import("collections");
const GuardedList = collections.GuardedList;
const diagnostics = @import("base").dev_diagnostics;

const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;

/// The instance a type is read under: `global` reads every group at its
/// first instance; any other value names a further instance of one group
/// (`Table.instances`).
pub const Env = enum(u32) {
    global = 0,
    _,
};

/// A solved type read under an instance. A resolved reference (`resolve`)
/// is canonical: two resolved references denote one type exactly when they
/// are equal.
pub const Ref = struct {
    ty: Type.TypeVarId,
    env: Env,

    pub fn global(ty: Type.TypeVarId) Ref {
        return .{ .ty = ty, .env = .global };
    }
};

/// One lifted function at one instance of its group. The ids below the
/// lifted function count are the lifted functions at their first instance;
/// each further instance numbers its group's functions after them, in group
/// order.
pub const SpecFnId = enum(u32) { _ };

/// What instance discovery found, for the `lambda_solve_stats` census.
pub const Census = struct {
    groups: usize = 0,
    generic_groups: usize = 0,
    /// References that were solved at an instantiation of a generic group.
    instantiated_references: usize = 0,
    /// Distinct instances over every generic group that has one, and the
    /// most any group has.
    instances: usize = 0,
    max_instances: usize = 0,
    /// Instances whose functions' procedure identities equal an earlier
    /// instance's: they differ only in erased callables' members or in a
    /// function's own callable set. Counted only with the diagnostic set.
    identity_equal_instances: usize = 0,
    /// Functions and expressions the further instances lower, against the
    /// program's expressions.
    instance_fns: usize = 0,
    instance_exprs: usize = 0,
    exprs: usize = 0,
};

const none_class = std.math.maxInt(u32);
const no_rep = std.math.maxInt(u32);

/// An instantiation read under an instance: a row made in, or named by, a
/// body read under `env`.
const Occ = struct {
    row: Type.InstId,
    env: Env,
};

/// A further instance of a generalized group.
pub const Instance = struct {
    group: u32,
    class: u32,
    /// The occurrence whose images this instance's scheme nodes read.
    rep: Occ,
    /// The spec function id of the group's first function at this instance.
    first_fn: u32,
};

/// The body types of a group with further instances that each instance
/// reads under its own environment, and the instantiations its bodies and
/// those types name whose images are such types.
const Copied = struct {
    types: collections.DenseMapUnmanaged(Type.TypeVarId, void) = .empty,
    insts: collections.DenseMapUnmanaged(Type.InstId, void) = .empty,

    fn deinit(self: *Copied, allocator: Allocator) void {
        self.types.deinit(allocator);
        self.insts.deinit(allocator);
    }
};

const SchemePos = struct {
    group: u32,
    position: u32,
};

/// A lambda-set member read under an instance.
pub const ResolvedMember = struct {
    /// The member's function at its instance.
    source: Lifted.FnId,
    env: Env,
    /// `symbolOf(source, env)`.
    symbol: Common.Symbol,
    /// The member's capture span, whose types read under `captures_env`.
    captures: Type.Span,
    captures_env: Env,
};

/// The instances of every generalized group of one solved program.
pub const Table = struct {
    /// Index 0 stands for `Env.global` and is never read.
    instances: std.ArrayList(Instance) = .empty,
    /// Per group: the environment of its second instance, and how many
    /// instances it has (zero when nothing instantiated it).
    group_first_env: []u32 = &.{},
    group_classes: []u32 = &.{},
    /// Per instantiation row: its instance read in the global environment.
    row_class: []u32 = &.{},
    /// Instances of occurrences read under a further instance.
    pair_class: std.AutoHashMapUnmanaged(Occ, u32) = .empty,
    /// Per group: the row of its first instance, or `no_rep`.
    rep0: []u32 = &.{},
    /// The scheme nodes of every group with an instance.
    scheme_pos: collections.DenseMapUnmanaged(Type.TypeVarId, SchemePos) = .empty,
    /// Per group: its copied types, for a group with further instances.
    copied: []?Copied = &.{},
    /// Types that equal instances showed equal, each mapped to the one type
    /// that stands for its class.
    canon: std.AutoHashMapUnmanaged(Ref, Ref) = .empty,
    /// Per lifted function: its position in its binding group.
    group_positions: []u32 = &.{},
    fn_by_symbol: std.AutoHashMapUnmanaged(Common.Symbol, Lifted.FnId) = .empty,
    fn_count: u32 = 0,
    /// Symbol of the first spec function past the lifted functions.
    first_symbol: u32 = 0,
    /// Source and environment of every spec function past the lifted ones.
    spec_fns: std.ArrayList(SpecFn) = .empty,
    census: Census = .{},

    const SpecFn = struct { source: Lifted.FnId, env: Env };

    pub fn deinit(self: *Table, allocator: Allocator) void {
        self.instances.deinit(allocator);
        allocator.free(self.group_first_env);
        allocator.free(self.group_classes);
        allocator.free(self.row_class);
        self.pair_class.deinit(allocator);
        allocator.free(self.rep0);
        self.scheme_pos.deinit(allocator);
        for (self.copied) |*entry| if (entry.*) |*copied| copied.deinit(allocator);
        allocator.free(self.copied);
        self.canon.deinit(allocator);
        allocator.free(self.group_positions);
        self.fn_by_symbol.deinit(allocator);
        self.spec_fns.deinit(allocator);
        self.* = .{};
    }

    /// Whether some group has a further instance.
    pub fn hasFurtherInstances(self: *const Table) bool {
        return self.spec_fns.items.len != 0;
    }

    /// How many spec functions the program has.
    pub fn specFnCount(self: *const Table) usize {
        return self.fn_count + self.spec_fns.items.len;
    }

    pub fn specFnSource(self: *const Table, id: SpecFnId) Lifted.FnId {
        const index = @backingInt(id);
        if (index < self.fn_count) return @fromBackingInt(index);
        return self.spec_fns.items[index - self.fn_count].source;
    }

    pub fn specFnEnv(self: *const Table, id: SpecFnId) Env {
        const index = @backingInt(id);
        if (index < self.fn_count) return .global;
        return self.spec_fns.items[index - self.fn_count].env;
    }

    /// `fn_id` at the instance `env` reads it under.
    pub fn specFn(self: *const Table, fn_id: Lifted.FnId, env: Env) SpecFnId {
        if (env == .global) return @fromBackingInt(@backingInt(fn_id));
        const instance = self.instances.items[@backingInt(env)];
        return @fromBackingInt(instance.first_fn + self.group_positions[@backingInt(fn_id)]);
    }

    /// The symbol naming `fn_id` at `env`: its own symbol at its first
    /// instance, and a symbol past every lifted symbol at a further one,
    /// numbered in spec function order.
    pub fn symbolOf(self: *const Table, program: *const Ast.Program, fn_id: Lifted.FnId, env: Env) Common.Symbol {
        if (env == .global) return program.lifted.getFn(fn_id).symbol;
        return @fromBackingInt(self.first_symbol + (@backingInt(self.specFn(fn_id, env)) - self.fn_count));
    }

    /// The group whose further instance `env` is.
    pub fn envGroup(self: *const Table, env: Env) u32 {
        return self.instances.items[@backingInt(env)].group;
    }

    fn classEnv(self: *const Table, group: u32, class: u32) Env {
        if (class == none_class or class == 0) return .global;
        return @fromBackingInt(self.group_first_env[group] + class - 1);
    }

    fn classOf(self: *const Table, occ: Occ) u32 {
        if (occ.env == .global) return self.row_class[@backingInt(occ.row)];
        return self.pair_class.get(occ) orelse none_class;
    }

    fn copiedTypes(self: *const Table, env: Env) ?*const collections.DenseMapUnmanaged(Type.TypeVarId, void) {
        if (env == .global) return null;
        const entry = &self.copied[self.envGroup(env)];
        return if (entry.*) |*copied| &copied.types else null;
    }

    /// The occurrence a member or reference naming `inst` denotes in a type
    /// or body read under `env`.
    fn occOf(self: *const Table, program: *const Ast.Program, inst: Type.InstId, env: Env) Occ {
        if (env == .global) return .{ .row = inst, .env = .global };
        const instance = self.instances.items[@backingInt(env)];
        if (inst == Ast.Polymorphism.identity(instance.group)) return instance.rep;
        if (program.poly.isIdentity(inst)) return .{ .row = inst, .env = .global };
        const copied = self.copied[instance.group] orelse return .{ .row = inst, .env = .global };
        if (copied.insts.contains(inst)) return .{ .row = inst, .env = env };
        return .{ .row = inst, .env = .global };
    }

    fn findCanon(self: *const Table, ref: Ref) Ref {
        var current = ref;
        while (self.canon.get(current)) |parent| current = parent;
        return current;
    }
};

/// `ref` resolved: its root, followed through the scheme nodes its
/// instance substitutes, read under the instance that owns it, and
/// canonical among the types equal instances showed equal.
pub fn resolve(program: *const Ast.Program, ref: Ref) Ref {
    const table = &program.instances;
    const store = &program.types;
    var ty = ref.ty;
    var env = ref.env;
    while (true) {
        const root = store.root(ty);
        if (table.scheme_pos.get(root)) |pos| {
            if (env != .global and table.envGroup(env) == pos.group) {
                const rep = table.instances.items[@backingInt(env)].rep;
                ty = program.poly.instImages(rep.row)[pos.position];
                env = rep.env;
            } else {
                ty = program.poly.instImages(@fromBackingInt(table.rep0[pos.group]))[pos.position];
                env = .global;
            }
            continue;
        }
        const kept: Env = if (table.copiedTypes(env)) |copied| (if (copied.contains(root)) env else .global) else .global;
        const resolved: Ref = .{ .ty = root, .env = kept };
        if (table.canon.count() == 0) return resolved;
        return table.findCanon(resolved);
    }
}

/// The content of a resolved reference.
pub fn content(program: *const Ast.Program, ref: Ref) Type.Content {
    return program.types.get(ref.ty);
}

/// The instance of `target` a reference to it at `expr`, in a body read
/// under `env`, names.
pub fn referenceEnv(program: *const Ast.Program, expr: Lifted.ExprId, target: Lifted.FnId, env: Env) Env {
    const table = &program.instances;
    const group = program.poly.fn_groups.items[@backingInt(target)];
    if (program.poly.expr_insts.get(expr)) |inst| {
        return table.classEnv(group, table.classOf(table.occOf(program, inst, env)));
    }
    if (env != .global and table.envGroup(env) == group) return env;
    return .global;
}

/// The instance of a lambda-set member's function, for a set read under
/// `env`.
pub fn memberEnv(program: *const Ast.Program, member: Type.FnMember, env: Env) Env {
    const table = &program.instances;
    const occ = table.occOf(program, member.inst, env);
    const group = program.poly.instantiations.items[@backingInt(occ.row)].group;
    return table.classEnv(group, table.classOf(occ));
}

/// The members of the set `members`, read under `env`: each names its
/// function at its instance, ordered by that function's symbol, one member
/// per function and instance (the first in the set's own order).
pub fn canonicalMembers(
    allocator: Allocator,
    program: *const Ast.Program,
    members: Type.Span,
    env: Env,
    out: *std.ArrayList(ResolvedMember),
) Allocator.Error!void {
    const table = &program.instances;
    out.clearRetainingCapacity();
    try out.ensureTotalCapacity(allocator, members.count());
    for (0..members.count()) |index| {
        const member = program.types.memberItem(members, index);
        const source = table.fn_by_symbol.get(member.lambda) orelse
            Common.invariant("a lambda-set member named no lifted function");
        const member_env = memberEnv(program, member, env);
        out.appendAssumeCapacity(.{
            .source = source,
            .env = member_env,
            .symbol = table.symbolOf(program, source, member_env),
            .captures = member.captures,
            .captures_env = env,
        });
    }
    if (out.items.len < 2) return;
    std.mem.sort(ResolvedMember, out.items, {}, struct {
        fn lessThan(_: void, lhs: ResolvedMember, rhs: ResolvedMember) bool {
            return @backingInt(lhs.symbol) < @backingInt(rhs.symbol);
        }
    }.lessThan);
    var kept: usize = 0;
    for (out.items) |member| {
        if (kept != 0 and out.items[kept - 1].symbol == member.symbol) continue;
        out.items[kept] = member;
        kept += 1;
    }
    out.shrinkRetainingCapacity(kept);
}

/// Compute `program.instances`, and reserve the symbols of every spec
/// function past the lifted ones.
pub fn discover(allocator: Allocator, program: *Ast.Program) Allocator.Error!void {
    program.instances.deinit(allocator);
    var discovery = try Discovery.init(allocator, program);
    defer discovery.deinit();
    try discovery.run();
    if (diagnostics.isSet(.lambda_solve_stats)) {
        const census = program.instances.census;
        std.debug.print("lambda-instances groups={d} generic_groups={d} instantiated_references={d} instances={d} max_instances={d} identity_equal_instances={d} instance_fns={d} instance_exprs={d} exprs={d}\n", .{
            census.groups,       census.generic_groups, census.instantiated_references,
            census.instances,    census.max_instances,  census.identity_equal_instances,
            census.instance_fns, census.instance_exprs, census.exprs,
        });
    }
}

const Discovery = struct {
    allocator: Allocator,
    program: *Ast.Program,
    /// Per binding group, every occurrence of an instantiation of it found
    /// so far.
    group_occs: []std.ArrayList(Occ) = &.{},

    // Equality scratch.
    assumed: std.AutoHashMapUnmanaged([2]Ref, void) = .empty,
    assumed_occs: std.AutoHashMapUnmanaged([2]Occ, void) = .empty,
    pending: std.ArrayList([2]Ref) = .empty,
    /// Whether comparisons follow procedure identity (`proc_identity.zig`):
    /// erased callables by their source function type only, and a
    /// function's own callable set not at all.
    relaxed: bool = false,
    /// Pairs of equal types, recorded once their group's instances are
    /// known.
    merges: std.ArrayList([2]Ref) = .empty,

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Discovery {
        return .{ .allocator = allocator, .program = program };
    }

    fn deinit(self: *Discovery) void {
        for (self.group_occs) |*list| list.deinit(self.allocator);
        self.allocator.free(self.group_occs);
        self.assumed.deinit(self.allocator);
        self.assumed_occs.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.merges.deinit(self.allocator);
    }

    fn table(self: *Discovery) *Table {
        return &self.program.instances;
    }

    fn poly(self: *Discovery) *const Ast.Polymorphism {
        return &self.program.poly;
    }

    fn types(self: *Discovery) *const Type.Store {
        return &self.program.types;
    }

    fn run(self: *Discovery) Allocator.Error!void {
        const allocator = self.allocator;
        const groups = self.program.binding_groups;
        const group_count = groups.count();
        const poly_ = self.poly();
        const t = self.table();
        const fn_count = self.program.lifted.fnCount();

        t.fn_count = @intCast(fn_count);
        t.first_symbol = self.program.lifted.next_symbol;
        t.census.groups = group_count;
        t.census.exprs = self.program.lifted.exprCount();
        t.census.instantiated_references = poly_.expr_insts.count();

        t.group_first_env = try allocator.alloc(u32, group_count);
        @memset(t.group_first_env, 0);
        t.group_classes = try allocator.alloc(u32, group_count);
        @memset(t.group_classes, 0);
        t.rep0 = try allocator.alloc(u32, group_count);
        @memset(t.rep0, no_rep);
        t.copied = try allocator.alloc(?Copied, group_count);
        @memset(t.copied, null);
        t.row_class = try allocator.alloc(u32, poly_.instantiations.items.len);
        @memset(t.row_class, none_class);
        t.group_positions = try allocator.alloc(u32, fn_count);
        @memset(t.group_positions, 0);
        try t.instances.append(allocator, undefined);
        for (0..group_count) |group_index| {
            for (groups.group(group_index), 0..) |fn_id, position| {
                t.group_positions[@backingInt(fn_id)] = @intCast(position);
            }
        }
        try t.fn_by_symbol.ensureTotalCapacity(allocator, @intCast(fn_count));
        for (0..fn_count) |index| {
            const fn_id: Lifted.FnId = @fromBackingInt(@as(u32, @intCast(index)));
            t.fn_by_symbol.putAssumeCapacity(self.program.lifted.getFn(fn_id).symbol, fn_id);
        }

        self.group_occs = try allocator.alloc(std.ArrayList(Occ), group_count);
        @memset(self.group_occs, .empty);
        for (poly_.instantiations.items, 0..) |row, index| {
            if (row.images == null) continue;
            try self.group_occs[row.group].append(allocator, .{ .row = @fromBackingInt(@as(u32, @intCast(index))), .env = .global });
        }
        for (0..group_count) |group_index| {
            if (!poly_.schemes.items[group_index].isGeneric()) t.row_class[group_index] = 0;
        }

        var group_index = group_count;
        while (group_index > 0) {
            group_index -= 1;
            if (!poly_.schemes.items[group_index].isGeneric()) continue;
            t.census.generic_groups += 1;
            try self.visitGroup(@intCast(group_index));
        }

        // Every merge is made; point each merged type at its class's type.
        var entries = t.canon.iterator();
        while (entries.next()) |entry| entry.value_ptr.* = t.findCanon(entry.value_ptr.*);
        self.program.lifted.next_symbol = t.first_symbol + @as(u32, @intCast(t.spec_fns.items.len));
    }

    fn setClass(self: *Discovery, occ: Occ, class: u32) Allocator.Error!void {
        const t = self.table();
        if (occ.env == .global) {
            t.row_class[@backingInt(occ.row)] = class;
        } else {
            try t.pair_class.put(self.allocator, occ, class);
        }
    }

    fn visitGroup(self: *Discovery, group: u32) Allocator.Error!void {
        const t = self.table();
        var reps: std.ArrayList(Occ) = .empty;
        defer reps.deinit(self.allocator);
        self.merges.clearRetainingCapacity();

        // Instances are numbered once every occurrence is placed, so
        // comparisons meet this group's occurrences as types, not as
        // numbered instances.
        var classes: std.ArrayList(u32) = .empty;
        defer classes.deinit(self.allocator);
        for (self.group_occs[group].items) |occ| {
            const class = for (reps.items, 0..) |rep, class_index| {
                const merges_before = self.merges.items.len;
                if (try self.instancesEqual(rep, occ)) break class_index;
                self.merges.shrinkRetainingCapacity(merges_before);
            } else blk: {
                try reps.append(self.allocator, occ);
                break :blk reps.items.len - 1;
            };
            try classes.append(self.allocator, @intCast(class));
        }
        for (self.group_occs[group].items, classes.items) |occ, class| try self.setClass(occ, class);
        if (reps.items.len == 0) return;
        if (diagnostics.isSet(.lambda_solve_stats) and reps.items.len > 1) {
            self.relaxed = true;
            defer self.relaxed = false;
            for (reps.items[1..], 1..) |later, later_index| {
                for (reps.items[0..later_index]) |earlier| {
                    const merges_before = self.merges.items.len;
                    defer self.merges.shrinkRetainingCapacity(merges_before);
                    if (try self.instancesEqual(earlier, later)) {
                        t.census.identity_equal_instances += 1;
                        break;
                    }
                }
            }
        }
        t.census.instances += reps.items.len;
        t.census.max_instances = @max(t.census.max_instances, reps.items.len);

        if (reps.items.len > 1) try self.addFurtherInstances(group, reps.items);

        for (self.merges.items) |pair| {
            const kept = t.findCanon(pair[0]);
            const merged = t.findCanon(pair[1]);
            if (!std.meta.eql(kept, merged)) try t.canon.put(self.allocator, merged, kept);
        }

        // The first instance is what every type outside a further instance
        // reads: each scheme node reads as that instance's image, which
        // instantiation built as a copy of it and unification then only
        // extended.
        const first = reps.items[0];
        if (first.env != .global) Common.invariant("a group's first instance was not an instantiation its source made");
        t.rep0[group] = @backingInt(first.row);
        const scheme = self.poly().schemes.items[group];
        for (self.poly().schemeNodes(scheme), 0..) |node, position| {
            try t.scheme_pos.put(self.allocator, self.types().root(node), .{ .group = group, .position = @intCast(position) });
        }
        t.row_class[@backingInt(Ast.Polymorphism.identity(group))] = 0;
        t.group_classes[group] = @intCast(reps.items.len);
    }

    /// Number `group`'s further instances, give each its spec functions,
    /// and add the occurrences its bodies name to the groups they
    /// instantiate, in the order the bodies and types name them.
    fn addFurtherInstances(self: *Discovery, group: u32, reps: []const Occ) Allocator.Error!void {
        const t = self.table();
        const fns = self.program.binding_groups.group(group);
        var region = try Region.init(self, group);
        defer region.deinit(self.allocator);

        t.group_first_env[group] = @intCast(t.instances.items.len);
        for (reps[1..], 1..) |rep, class| {
            const first_fn: u32 = @intCast(t.specFnCount());
            const env: Env = @fromBackingInt(@as(u32, @intCast(t.instances.items.len)));
            try t.instances.append(self.allocator, .{ .group = group, .class = @intCast(class), .rep = rep, .first_fn = first_fn });
            for (fns) |fn_id| try t.spec_fns.append(self.allocator, .{ .source = fn_id, .env = env });
        }
        t.census.instance_fns += (reps.len - 1) * fns.len;
        t.census.instance_exprs += (reps.len - 1) * region.exprs.items.len;

        const order = try region.copyOrder(self);
        defer self.allocator.free(order);
        for (1..reps.len) |class| {
            const env = t.classEnv(group, @intCast(class));
            for (order) |inst| {
                const row = self.poly().instantiations.items[@backingInt(inst)];
                try self.group_occs[row.group].append(self.allocator, .{ .row = inst, .env = env });
            }
        }
        t.copied[group] = .{ .types = region.copied, .insts = region.copied_insts };
        region.copied = .empty;
        region.copied_insts = .empty;
    }

    // Equality of occurrences as infinite types.

    /// Whether `rep` and `other`, two occurrences of one group's
    /// instantiations, give every scheme node equal images. Each pair of
    /// types compared is assumed equal while its parts are compared, so
    /// cycles close; any difference fails the whole comparison, so success
    /// means the assumed pairs are a bisimulation. On success the pairs it
    /// related are appended to `merges`.
    fn instancesEqual(self: *Discovery, rep: Occ, other: Occ) Allocator.Error!bool {
        self.assumed.clearRetainingCapacity();
        self.assumed_occs.clearRetainingCapacity();
        self.pending.clearRetainingCapacity();
        if (self.relaxed) {
            if (!try self.pushIdentityRoots(rep, other)) return false;
        } else if (!try self.pushOccPair(rep, other)) return false;
        while (self.pending.pop()) |pair| {
            const a = resolve(self.program, pair[0]);
            const b = resolve(self.program, pair[1]);
            if (std.meta.eql(a, b)) continue;
            const entry = try self.assumed.getOrPut(self.allocator, .{ a, b });
            if (entry.found_existing) continue;
            if (!try self.contentsMatch(a, b)) return false;
            try self.merges.append(self.allocator, .{ a, b });
        }
        return true;
    }

    fn pushOccPair(self: *Discovery, a: Occ, b: Occ) Allocator.Error!bool {
        if (std.meta.eql(a, b)) return true;
        const poly_ = self.poly();
        const row_a = poly_.instantiations.items[@backingInt(a.row)];
        const row_b = poly_.instantiations.items[@backingInt(b.row)];
        if (row_a.group != row_b.group) return false;
        const t = self.table();
        const class_a = t.classOf(a);
        const class_b = t.classOf(b);
        if (class_a != none_class or class_b != none_class) return class_a == class_b;
        if (row_a.images == null or row_b.images == null) return false;
        const entry = try self.assumed_occs.getOrPut(self.allocator, .{ a, b });
        if (entry.found_existing) return true;
        // Every scheme node's image, not only the function types': a set
        // can hold two members of one lambda at two instantiations, so the
        // path from a function type to a member's captures does not single
        // out one instantiation's captures.
        const images_a = poly_.instImages(a.row);
        const images_b = poly_.instImages(b.row);
        for (images_a, images_b) |image_a, image_b| {
            try self.pending.append(self.allocator, .{ .{ .ty = image_a, .env = a.env }, .{ .ty = image_b, .env = b.env } });
        }
        return true;
    }

    /// The parts of two occurrences' function types a procedure identity
    /// renders: arguments, results and the function's own captures, not its
    /// own callable set.
    fn pushIdentityRoots(self: *Discovery, a: Occ, b: Occ) Allocator.Error!bool {
        const poly_ = self.poly();
        const row = poly_.instantiations.items[@backingInt(a.row)];
        const scheme = poly_.schemes.items[row.group];
        const fns = self.program.binding_groups.group(row.group);
        const roots = poly_.fn_roots.items[scheme.fn_roots_start..][0..fns.len];
        const store = self.types();
        for (fns, roots) |fn_id, position| {
            const left_ref = resolve(self.program, .{ .ty = poly_.instImages(a.row)[position], .env = a.env });
            const right_ref = resolve(self.program, .{ .ty = poly_.instImages(b.row)[position], .env = b.env });
            const left = store.get(left_ref.ty).func;
            const right = store.get(right_ref.ty).func;
            if (!try self.pushSpanPairs(left.args, left_ref.env, right.args, right_ref.env)) return false;
            try self.pending.append(self.allocator, .{ .{ .ty = left.ret, .env = left_ref.env }, .{ .ty = right.ret, .env = right_ref.env } });
            const symbol = self.program.lifted.getFn(fn_id).symbol;
            const left_captures = self.ownCaptures(.{ .ty = left.callable, .env = left_ref.env }, symbol) orelse return false;
            const right_captures = self.ownCaptures(.{ .ty = right.callable, .env = right_ref.env }, symbol) orelse return false;
            if (left_captures.span.count() != right_captures.span.count()) return false;
            for (0..left_captures.span.count()) |index| {
                try self.pending.append(self.allocator, .{
                    .{ .ty = store.captureItem(left_captures.span, index).ty, .env = left_captures.env },
                    .{ .ty = store.captureItem(right_captures.span, index).ty, .env = right_captures.env },
                });
            }
        }
        return true;
    }

    const EnvSpan = struct { span: Type.Span, env: Env };

    fn ownCaptures(self: *Discovery, callable: Ref, symbol: Common.Symbol) ?EnvSpan {
        const store = self.types();
        const resolved = resolve(self.program, callable);
        const callable_content = store.get(resolved.ty);
        const members = if (callable_content == .lambda_set) callable_content.lambda_set else if (callable_content == .erased) callable_content.erased.members else return null;
        for (0..members.count()) |index| {
            const member = store.memberItem(members, index);
            if (member.lambda == symbol) return .{ .span = member.captures, .env = resolved.env };
        }
        return null;
    }

    fn pushSpanPairs(self: *Discovery, a: Type.Span, a_env: Env, b: Type.Span, b_env: Env) Allocator.Error!bool {
        if (a.count() != b.count()) return false;
        const store = self.types();
        for (0..a.count()) |index| {
            try self.pending.append(self.allocator, .{ .{ .ty = store.spanItem(a, index), .env = a_env }, .{ .ty = store.spanItem(b, index), .env = b_env } });
        }
        return true;
    }

    fn pushPair(self: *Discovery, a: Type.TypeVarId, a_env: Env, b: Type.TypeVarId, b_env: Env) Allocator.Error!void {
        try self.pending.append(self.allocator, .{ .{ .ty = a, .env = a_env }, .{ .ty = b, .env = b_env } });
    }

    fn contentsMatch(self: *Discovery, a: Ref, b: Ref) Allocator.Error!bool {
        const store = self.types();
        const left = store.get(a.ty);
        const right = store.get(b.ty);
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
        switch (left) {
            .link, .unbound, .mono => return false,
            .zst, .hole => return true,
            .primitive => |primitive| return primitive == right.primitive,
            .list => |elem| try self.pushPair(elem, a.env, right.list, b.env),
            .box => |elem| try self.pushPair(elem, a.env, right.box, b.env),
            .tuple => |items| return try self.pushSpanPairs(items, a.env, right.tuple, b.env),
            .func => |func| {
                if (!try self.pushSpanPairs(func.args, a.env, right.func.args, b.env)) return false;
                try self.pushPair(func.callable, a.env, right.func.callable, b.env);
                try self.pushPair(func.ret, a.env, right.func.ret, b.env);
            },
            .record => |fields| {
                const other = right.record;
                if (fields.count() != other.count()) return false;
                for (0..fields.count()) |index| {
                    const lf = store.fieldItem(fields, index);
                    const rf = store.fieldItem(other, index);
                    if (lf.name != rf.name or !std.meta.eql(lf.default, rf.default)) return false;
                    try self.pushPair(lf.ty, a.env, rf.ty, b.env);
                    const lv = lf.value_ty.get();
                    const rv = rf.value_ty.get();
                    if ((lv == null) != (rv == null)) return false;
                    if (lv) |value| try self.pushPair(value, a.env, rv.?, b.env);
                }
            },
            .tag_union => |tags| {
                const other = right.tag_union;
                if (tags.count() != other.count()) return false;
                for (0..tags.count()) |index| {
                    const lt = store.tagItem(tags, index);
                    const rt = store.tagItem(other, index);
                    if (lt.name != rt.name or lt.checked_name != rt.checked_name) return false;
                    if (!try self.pushSpanPairs(lt.payloads, a.env, rt.payloads, b.env)) return false;
                }
            },
            .named => |named| {
                const other = right.named;
                if (!std.meta.eql(named.named_type, other.named_type) or !std.meta.eql(named.def, other.def) or
                    named.kind != other.kind or named.builtin_owner != other.builtin_owner) return false;
                if (!try self.pushSpanPairs(named.args, a.env, other.args, b.env)) return false;
                const lb = named.backing.get();
                const rb = other.backing.get();
                if ((lb == null) != (rb == null)) return false;
                if (lb) |backing| {
                    if (backing.use != rb.?.use or backing.authority != rb.?.authority) return false;
                    try self.pushPair(backing.ty, a.env, rb.?.ty, b.env);
                }
                if (named.declared_order.count() != other.declared_order.count()) return false;
                for (0..named.declared_order.count()) |index| {
                    switch (store.declaredFieldItem(named.declared_order, index)) {
                        .named => |name| switch (store.declaredFieldItem(other.declared_order, index)) {
                            .named => |other_name| if (name != other_name) return false,
                            .padding => return false,
                        },
                        .padding => |padding| switch (store.declaredFieldItem(other.declared_order, index)) {
                            .named => return false,
                            .padding => |other_padding| try self.pushPair(padding, a.env, other_padding, b.env),
                        },
                    }
                }
            },
            .lambda_set => |members| return try self.membersMatch(members, a.env, right.lambda_set, b.env),
            .erased => |erased| {
                const other = right.erased;
                if (!std.mem.eql(u8, &erased.source_fn_ty.bytes, &other.source_fn_ty.bytes)) return false;
                if (self.relaxed) return true;
                if ((erased.abi_fn == null) != (other.abi_fn == null)) return false;
                if (erased.abi_fn) |abi_fn| try self.pushPair(abi_fn, a.env, other.abi_fn.?, b.env);
                return try self.membersMatch(erased.members, a.env, other.members, b.env);
            },
        }
        return true;
    }

    fn membersMatch(self: *Discovery, a: Type.Span, a_env: Env, b: Type.Span, b_env: Env) Allocator.Error!bool {
        const store = self.types();
        const t = self.table();
        if (a.count() != b.count()) return false;
        for (0..a.count()) |index| {
            const lm = store.memberItem(a, index);
            const rm = store.memberItem(b, index);
            if (lm.lambda != rm.lambda) return false;
            if (!try self.pushOccPair(t.occOf(self.program, lm.inst, a_env), t.occOf(self.program, rm.inst, b_env))) return false;
            if (lm.captures.count() != rm.captures.count()) return false;
            for (0..lm.captures.count()) |capture| {
                try self.pushPair(store.captureItem(lm.captures, capture).ty, a_env, store.captureItem(rm.captures, capture).ty, b_env);
            }
        }
        return true;
    }
};

/// The parts of a generic group's bodies, and which of their types depend on
/// the group's instance.
const Region = struct {
    group: u32,
    owner: Type.Owner,
    exprs: std.ArrayList(Lifted.ExprId) = .empty,
    stmts: std.ArrayList(Lifted.StmtId) = .empty,
    pats: std.ArrayList(Lifted.PatId) = .empty,
    /// Scheme position by scheme node.
    positions: collections.DenseMapUnmanaged(Type.TypeVarId, u32) = .empty,
    /// The body types that reach a scheme node or one of the group's own
    /// lambdas, outside the scheme: each instance reads them under its own
    /// environment.
    copied: collections.DenseMapUnmanaged(Type.TypeVarId, void) = .empty,
    copied_nodes: std.ArrayList(Type.TypeVarId) = .empty,
    /// The instantiations in the bodies whose images such a type is.
    copied_insts: collections.DenseMapUnmanaged(Type.InstId, void) = .empty,

    fn deinit(self: *Region, allocator: Allocator) void {
        self.exprs.deinit(allocator);
        self.stmts.deinit(allocator);
        self.pats.deinit(allocator);
        self.positions.deinit(allocator);
        self.copied.deinit(allocator);
        self.copied_nodes.deinit(allocator);
        self.copied_insts.deinit(allocator);
    }

    fn init(discovery: *Discovery, group: u32) Allocator.Error!Region {
        var region: Region = .{ .group = group, .owner = Type.first_group_owner + group };
        errdefer region.deinit(discovery.allocator);
        const poly_ = discovery.poly();
        const scheme = poly_.schemes.items[group];
        for (poly_.schemeNodes(scheme), 0..) |node, position| try region.positions.put(discovery.allocator, node, @intCast(position));
        try region.collectBodies(discovery);
        try region.findCopied(discovery);
        return region;
    }

    /// The instantiations, in the order the group's bodies and then its
    /// copied types name them, that each further instance reads under its
    /// own environment.
    fn copyOrder(self: *const Region, discovery: *Discovery) Allocator.Error![]Type.InstId {
        const allocator = discovery.allocator;
        const poly_ = discovery.poly();
        const store = discovery.types();
        const identity = Ast.Polymorphism.identity(self.group);
        var order: std.ArrayList(Type.InstId) = .empty;
        errdefer order.deinit(allocator);
        var seen: collections.DenseMapUnmanaged(Type.InstId, void) = .empty;
        defer seen.deinit(allocator);
        for (self.exprs.items) |expr_id| {
            const inst = poly_.expr_insts.get(expr_id) orelse continue;
            const target = namedFn(discovery.program.lifted.getExpr(expr_id).data);
            if (poly_.fn_groups.items[@backingInt(target)] == self.group) continue;
            if (inst == identity or !self.copied_insts.contains(inst)) continue;
            if ((try seen.getOrPut(allocator, inst)).found_existing) continue;
            try order.append(allocator, inst);
        }
        for (self.copied_nodes.items) |node| {
            const node_content = store.get(node);
            const members = switch (node_content) {
                .lambda_set => |members| members,
                .erased => |erased| erased.members,
                .link, .unbound, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .func, .zst, .hole, .mono => continue,
            };
            for (0..members.count()) |index| {
                const inst = store.memberItem(members, index).inst;
                if (inst == identity or !self.copied_insts.contains(inst)) continue;
                if ((try seen.getOrPut(allocator, inst)).found_existing) continue;
                try order.append(allocator, inst);
            }
        }
        return try order.toOwnedSlice(allocator);
    }

    /// Every expression, statement and pattern of the group's bodies.
    fn collectBodies(self: *Region, discovery: *Discovery) Allocator.Error!void {
        const allocator = discovery.allocator;
        const lifted = &discovery.program.lifted;
        var seen_exprs = try std.DynamicBitSetUnmanaged.initEmpty(allocator, lifted.exprCount());
        defer seen_exprs.deinit(allocator);
        var stack: std.ArrayList(Lifted.ExprChild) = .empty;
        defer stack.deinit(allocator);
        var pat_stack: std.ArrayList(Lifted.PatId) = .empty;
        defer pat_stack.deinit(allocator);
        for (discovery.program.binding_groups.group(self.group)) |fn_id| {
            switch (lifted.getFn(fn_id).body) {
                .roc => |body| try stack.append(allocator, .{ .expr = body }),
                .hosted => {},
            }
        }
        while (stack.pop()) |child| switch (child) {
            .stmt => |stmt_id| {
                try self.stmts.append(allocator, stmt_id);
                switch (lifted.getStmt(stmt_id)) {
                    .uninitialized => |pat| try pat_stack.append(allocator, pat),
                    .let_ => |let_| try pat_stack.append(allocator, let_.pat),
                    .expr, .expect, .dbg, .return_, .crash, .checked_error => {},
                }
                try Lifted.appendStmtChildren(allocator, lifted, stmt_id, &stack);
            },
            .expr => |expr_id| {
                if (seen_exprs.isSet(@backingInt(expr_id))) continue;
                seen_exprs.set(@backingInt(expr_id));
                try self.exprs.append(allocator, expr_id);
                const data = lifted.getExpr(expr_id).data;
                if (data == .let_) try pat_stack.append(allocator, data.let_.bind);
                if (data == .match_) {
                    const branches = lifted.branchSpan(data.match_.branches);
                    for (0..branches.len) |index| try pat_stack.append(allocator, GuardedList.at(branches, index).pat);
                }
                try Lifted.appendChildren(allocator, lifted, expr_id, &stack);
            },
        };
        while (pat_stack.pop()) |pat_id| {
            try self.pats.append(allocator, pat_id);
            switch (lifted.getPat(pat_id).data) {
                .as => |as| try pat_stack.append(allocator, as.pattern),
                .nominal => |inner| try pat_stack.append(allocator, inner),
                .tuple => |items| {
                    const pats = lifted.patSpan(items);
                    for (0..pats.len) |index| try pat_stack.append(allocator, GuardedList.at(pats, index));
                },
                .tag => |tag| {
                    const pats = lifted.patSpan(tag.payloads);
                    for (0..pats.len) |index| try pat_stack.append(allocator, GuardedList.at(pats, index));
                },
                .record => |fields| {
                    const destructs = lifted.recordDestructSpan(fields);
                    for (0..destructs.len) |index| try pat_stack.append(allocator, GuardedList.at(destructs, index).pattern);
                },
                .list => |list| {
                    const pats = lifted.patSpan(list.patterns);
                    for (0..pats.len) |index| try pat_stack.append(allocator, GuardedList.at(pats, index));
                    if (list.rest.get()) |rest| if (rest.pattern.get()) |rest_pat| try pat_stack.append(allocator, rest_pat);
                },
                .str_pattern => |str| {
                    const steps = lifted.strPatternStepSpan(str.steps);
                    for (0..steps.len) |index| if (GuardedList.at(steps, index).capture.get()) |capture| try pat_stack.append(allocator, capture);
                },
                .bind, .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => {},
            }
        }
    }

    /// The group's body types outside its scheme that each instance reads
    /// under its own environment: those the group owns that reach a scheme
    /// node, a callable naming one of the group's own lambdas, or an
    /// instantiation whose images are such types. Type nodes are numbered
    /// from zero; instantiation nodes carry `inst_bit`.
    fn findCopied(self: *Region, discovery: *Discovery) Allocator.Error!void {
        const allocator = discovery.allocator;
        const program = discovery.program;
        const store = discovery.types();
        const poly_ = discovery.poly();
        const lifted = &program.lifted;
        const identity = Ast.Polymorphism.identity(self.group);
        var graph: Graph = .{};
        defer graph.deinit(allocator);

        for (program.binding_groups.group(self.group)) |fn_id| {
            try graph.push(allocator, program.fn_tys.items[@backingInt(fn_id)], Graph.no_parent);
            const fn_ = lifted.getFn(fn_id);
            try graph.pushLocals(discovery, fn_.args);
            try graph.pushLocals(discovery, fn_.captures);
        }
        for (self.exprs.items) |expr_id| {
            try graph.push(allocator, program.expr_tys.items[@backingInt(expr_id)], Graph.no_parent);
            if (poly_.expr_insts.get(expr_id)) |inst| _ = try graph.noteInst(discovery, inst, Graph.no_parent);
            for (exprLocals(lifted, expr_id).slice()) |local| try graph.push(allocator, program.local_tys.items[@backingInt(local)], Graph.no_parent);
            const data = lifted.getExpr(expr_id).data;
            if (data == .loop_) try graph.pushLocals(discovery, data.loop_.params);
            if (data == .join_point) {
                try graph.pushLocals(discovery, data.join_point.params);
                try graph.pushLocals(discovery, data.join_point.retained);
            }
            if (data == .jump) try graph.pushLocals(discovery, data.jump.loop_params);
        }
        for (self.pats.items) |pat_id| {
            try graph.push(allocator, program.pat_tys.items[@backingInt(pat_id)], Graph.no_parent);
            const data = lifted.getPat(pat_id).data;
            if (data == .bind) try graph.push(allocator, program.local_tys.items[@backingInt(data.bind)], Graph.no_parent);
            if (data == .as) try graph.push(allocator, program.local_tys.items[@backingInt(data.as.local)], Graph.no_parent);
        }

        var children: std.ArrayList(Type.TypeVarId) = .empty;
        defer children.deinit(allocator);
        var member_insts: std.ArrayList(Type.InstId) = .empty;
        defer member_insts.deinit(allocator);
        while (graph.work.pop()) |item| {
            const root = store.root(item.ty);
            // A class no link has given an owner was first read by the
            // body that reaches it.
            const owner = store.owners.items[@backingInt(root)];
            if (owner != self.owner and owner != Type.unowned) continue;
            const entry = try graph.index_of.getOrPut(allocator, root);
            if (entry.found_existing) {
                try graph.edge(allocator, item.parent, entry.value_ptr.*);
                continue;
            }
            const index: u32 = @intCast(graph.nodes.items.len);
            entry.value_ptr.* = index;
            try graph.nodes.append(allocator, root);
            try graph.edge(allocator, item.parent, index);
            if (self.positions.contains(root)) {
                // A scheme node: substituted, never copied, and a seed.
                try graph.reaches.append(allocator, true);
                continue;
            }
            children.clearRetainingCapacity();
            member_insts.clearRetainingCapacity();
            const own = try appendChildren(store, root, &children, &member_insts, identity, allocator);
            try graph.reaches.append(allocator, own);
            for (children.items) |child| try graph.push(allocator, child, index);
            for (member_insts.items) |inst| {
                if (poly_.isIdentity(inst)) continue;
                _ = try graph.noteInst(discovery, inst, index);
            }
        }

        var changed = true;
        while (changed) {
            changed = false;
            for (graph.edges.items) |edge_| {
                if (!graph.get(edge_[1]) or graph.get(edge_[0])) continue;
                graph.set(edge_[0]);
                changed = true;
            }
        }
        for (graph.nodes.items, graph.reaches.items) |node, reaches| {
            if (!reaches or self.positions.contains(node)) continue;
            try self.copied.put(allocator, node, {});
            try self.copied_nodes.append(allocator, node);
        }
        for (graph.insts.items, graph.inst_reaches.items) |inst, reaches| {
            if (reaches) try self.copied_insts.put(allocator, inst, {});
        }
    }

    /// The reference graph `findCopied` walks.
    const Graph = struct {
        const no_parent = std.math.maxInt(u32);
        const inst_bit: u32 = 1 << 31;
        const Work = struct { ty: Type.TypeVarId, parent: u32 };

        work: std.ArrayList(Work) = .empty,
        nodes: std.ArrayList(Type.TypeVarId) = .empty,
        reaches: std.ArrayList(bool) = .empty,
        index_of: collections.DenseMapUnmanaged(Type.TypeVarId, u32) = .empty,
        insts: std.ArrayList(Type.InstId) = .empty,
        inst_reaches: std.ArrayList(bool) = .empty,
        inst_index: collections.DenseMapUnmanaged(Type.InstId, u32) = .empty,
        /// Parent to child.
        edges: std.ArrayList([2]u32) = .empty,

        fn deinit(self: *Graph, allocator: Allocator) void {
            self.work.deinit(allocator);
            self.nodes.deinit(allocator);
            self.reaches.deinit(allocator);
            self.index_of.deinit(allocator);
            self.insts.deinit(allocator);
            self.inst_reaches.deinit(allocator);
            self.inst_index.deinit(allocator);
            self.edges.deinit(allocator);
        }

        fn push(self: *Graph, allocator: Allocator, ty: Type.TypeVarId, parent: u32) Allocator.Error!void {
            try self.work.append(allocator, .{ .ty = ty, .parent = parent });
        }

        fn pushLocals(self: *Graph, discovery: *Discovery, span: Lifted.Span(Lifted.TypedLocal)) Allocator.Error!void {
            const program = discovery.program;
            const locals = program.lifted.typedLocalSpan(span);
            for (0..locals.len) |index| try self.push(discovery.allocator, program.local_tys.items[@backingInt(GuardedList.at(locals, index).local)], no_parent);
        }

        fn edge(self: *Graph, allocator: Allocator, parent: u32, child: u32) Allocator.Error!void {
            if (parent == no_parent) return;
            try self.edges.append(allocator, .{ parent, child });
        }

        fn noteInst(self: *Graph, discovery: *Discovery, inst: Type.InstId, parent: u32) Allocator.Error!u32 {
            const allocator = discovery.allocator;
            const entry = try self.inst_index.getOrPut(allocator, inst);
            if (!entry.found_existing) {
                entry.value_ptr.* = @as(u32, @intCast(self.insts.items.len)) | inst_bit;
                try self.insts.append(allocator, inst);
                try self.inst_reaches.append(allocator, false);
                for (discovery.poly().instImages(inst)) |image| try self.push(allocator, image, entry.value_ptr.*);
            }
            try self.edge(allocator, parent, entry.value_ptr.*);
            return entry.value_ptr.*;
        }

        fn get(self: *const Graph, index: u32) bool {
            if (index & inst_bit != 0) return self.inst_reaches.items[index & ~inst_bit];
            return self.reaches.items[index];
        }

        fn set(self: *Graph, index: u32) void {
            if (index & inst_bit != 0) {
                self.inst_reaches.items[index & ~inst_bit] = true;
            } else {
                self.reaches.items[index] = true;
            }
        }
    };
};

/// The locals an expression names in fields of its own.
const ExprLocals = struct {
    items: [3]Lifted.LocalId = undefined,
    len: usize = 0,

    fn add(self: *ExprLocals, local: Lifted.LocalId) void {
        self.items[self.len] = local;
        self.len += 1;
    }

    fn slice(self: *const ExprLocals) []const Lifted.LocalId {
        return self.items[0..self.len];
    }
};

fn exprLocals(lifted: *const Lifted.Program, expr_id: Lifted.ExprId) ExprLocals {
    var out: ExprLocals = .{};
    const data = lifted.getExpr(expr_id).data;
    if (data == .local) out.add(data.local);
    if (data == .uninitialized_payload) out.add(data.uninitialized_payload.condition);
    if (data == .if_initialized_payload) out.add(data.if_initialized_payload.payload);
    if (data == .try_sequence) out.add(data.try_sequence.ok_local);
    if (data == .try_record_sequence) {
        out.add(data.try_record_sequence.value_local);
        out.add(data.try_record_sequence.rest_local);
    }
    return out;
}

/// `root`'s child types into `children` and the instantiations its members
/// name into `insts`; true when it names `identity`.
fn appendChildren(
    store: *const Type.Store,
    root: Type.TypeVarId,
    children: *std.ArrayList(Type.TypeVarId),
    insts: *std.ArrayList(Type.InstId),
    identity: Type.InstId,
    allocator: Allocator,
) Allocator.Error!bool {
    var own = false;
    switch (store.get(root)) {
        .link => Common.invariant("Lambda Solved root returned a link"),
        .unbound, .primitive, .zst, .hole, .mono => {},
        .list, .box => |elem| try children.append(allocator, elem),
        .tuple => |items| for (0..items.count()) |index| try children.append(allocator, store.spanItem(items, index)),
        .record => |fields| for (0..fields.count()) |index| {
            const field = store.fieldItem(fields, index);
            try children.append(allocator, field.ty);
            if (field.value_ty.get()) |value_ty| try children.append(allocator, value_ty);
        },
        .tag_union => |tags| for (0..tags.count()) |tag_index| {
            const tag = store.tagItem(tags, tag_index);
            for (0..tag.payloads.count()) |index| try children.append(allocator, store.spanItem(tag.payloads, index));
        },
        .func => |func| {
            for (0..func.args.count()) |index| try children.append(allocator, store.spanItem(func.args, index));
            try children.append(allocator, func.callable);
            try children.append(allocator, func.ret);
        },
        .named => |named| {
            for (0..named.args.count()) |index| try children.append(allocator, store.spanItem(named.args, index));
            if (named.backing.get()) |backing| try children.append(allocator, backing.ty);
            for (0..named.declared_order.count()) |index| switch (store.declaredFieldItem(named.declared_order, index)) {
                .named => {},
                .padding => |padding| try children.append(allocator, padding),
            };
        },
        .lambda_set => |members| own = try appendMembers(store, members, children, insts, identity, allocator),
        .erased => |erased| {
            own = try appendMembers(store, erased.members, children, insts, identity, allocator);
            if (erased.abi_fn) |abi_fn| try children.append(allocator, abi_fn);
        },
    }
    return own;
}

fn appendMembers(
    store: *const Type.Store,
    members: Type.Span,
    children: *std.ArrayList(Type.TypeVarId),
    insts: *std.ArrayList(Type.InstId),
    identity: Type.InstId,
    allocator: Allocator,
) Allocator.Error!bool {
    var own = false;
    for (0..members.count()) |member_index| {
        const member = store.memberItem(members, member_index);
        if (member.inst == identity) own = true;
        try insts.append(allocator, member.inst);
        for (0..member.captures.count()) |index| try children.append(allocator, store.captureItem(member.captures, index).ty);
    }
    return own;
}

/// The function a reference names: only `fn_ref` and direct `call_proc`
/// expressions record an instantiation.
fn namedFn(data: Lifted.ExprData) Lifted.FnId {
    if (data == .fn_ref) return data.fn_ref.fn_id;
    if (data == .call_proc) return Lifted.localDirectCallee(data.call_proc).?;
    Common.invariant("an instantiation was recorded for an expression that names no function");
}

test "lambda solved instances declarations are referenced" {
    std.testing.refAllDecls(@This());
}
