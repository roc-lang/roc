//! Place derivations: which reads of a static value take their fresh form
//! (design.md "Place derivations").
//!
//! A place is a local and a path from its value through its own bytes: struct
//! fields and tag variant payloads, never a list's items or a box's payload.
//! Each local carries derivations `(at, base, from)`: its place `at` may hold
//! the place `from` of a base, which is a candidate read, a tracked parameter
//! of the procedure, or `shared` (a second holder). Derivations flow over the
//! uniqueness settlement's final edges and through callees' carry rows; a
//! demand site (a place whose write in place relies on a birth) demands the
//! base places its derivations reach, unless a shared derivation overlaps it.
//! Demanded candidates take their fresh form; demanded parameter places are
//! the procedure's fresh demands, which its callers' call sites demand in
//! turn. Carry rows and fresh demands are least fixpoints over finite path
//! sets, computed from empty after the settlement converges.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// The local index that names no local of the analyzed domain.
pub const no_local: u32 = std.math.maxInt(u32);

/// One step of a path: a struct field by its original index, or a tag union
/// variant's payload (`variant_step` set, the variant index below it).
pub const Step = u32;
const variant_step: Step = 1 << 31;

/// A struct field's step.
pub fn fieldStep(index: u32) Step {
    std.debug.assert(index < variant_step);
    return index;
}

/// A tag union variant payload's step.
pub fn variantStep(variant: u32) Step {
    std.debug.assert(variant < variant_step);
    return variant_step | variant;
}

/// At most two steps: a field, a variant payload, or a variant payload's
/// struct field.
pub const Steps = struct {
    items: [2]Step = .{ 0, 0 },
    len: u8 = 0,

    pub fn one(step: Step) Steps {
        return .{ .items = .{ step, 0 }, .len = 1 };
    }

    pub fn two(first: Step, second: Step) Steps {
        return .{ .items = .{ first, second }, .len = 2 };
    }

    fn slice(self: *const Steps) []const Step {
        return self.items[0..self.len];
    }
};

/// A same-place edge: an alias, a join edge.
pub const Copy = struct { source: u32, target: u32 };

/// `target`'s place `steps` holds `source`; a store that is not `source`'s
/// last use carries nothing, since `source` keeps holding its value.
pub const Inject = struct { source: u32, target: u32, steps: Steps, live: bool };

/// `target` holds `source`'s place `steps`: a committed take or a payload
/// view.
pub const Project = struct { source: u32, target: u32, steps: Steps };

/// A direct call. `target` is `no_local` when the result is untracked; its
/// arguments are `Inputs.call_args[args_start..][0..args_len]`.
pub const Call = struct { target: u32, proc: u32, args_start: u32, args_len: u32 };

/// One argument of a direct call: its local (`no_local` when untracked), and
/// whether the call is its last use; an argument the caller uses again
/// carries nothing into the result.
pub const CallArg = struct { local: u32, live: bool };

/// A tracked parameter: position `position` of procedure `proc` is `local`.
pub const Param = struct { proc: u32, position: u16, local: u32 };

/// The edges and seeds of one analysis's derivations, over its dense local
/// domain. Candidate `c` is the read whose target is `candidates[c]`.
pub const Inputs = struct {
    local_count: u32 = 0,
    copies: []const Copy = &.{},
    injects: []const Inject = &.{},
    projects: []const Project = &.{},
    calls: []const Call = &.{},
    call_args: []const CallArg = &.{},
    candidates: []const u32 = &.{},
    params: []const Param = &.{},
    /// Locals the occurrence scan gives another holder.
    shared: []const u32 = &.{},
    /// Arguments a low-level operation checks at runtime or writes in place
    /// unchecked.
    demand_sites: []const u32 = &.{},

    pub fn clone(self: *const Inputs, allocator: Allocator) Allocator.Error!Inputs {
        return .{
            .local_count = self.local_count,
            .copies = try allocator.dupe(Copy, self.copies),
            .injects = try allocator.dupe(Inject, self.injects),
            .projects = try allocator.dupe(Project, self.projects),
            .calls = try allocator.dupe(Call, self.calls),
            .call_args = try allocator.dupe(CallArg, self.call_args),
            .candidates = try allocator.dupe(u32, self.candidates),
            .params = try allocator.dupe(Param, self.params),
            .shared = try allocator.dupe(u32, self.shared),
            .demand_sites = try allocator.dupe(u32, self.demand_sites),
        };
    }

    pub fn deinit(self: *Inputs, allocator: Allocator) void {
        allocator.free(self.copies);
        allocator.free(self.injects);
        allocator.free(self.projects);
        allocator.free(self.calls);
        allocator.free(self.call_args);
        allocator.free(self.candidates);
        allocator.free(self.params);
        allocator.free(self.shared);
        allocator.free(self.demand_sites);
    }
};

/// Collects `Inputs` while an analysis scans its statements.
pub const InputsBuilder = struct {
    copies: std.ArrayList(Copy) = .empty,
    injects: std.ArrayList(Inject) = .empty,
    projects: std.ArrayList(Project) = .empty,
    calls: std.ArrayList(Call) = .empty,
    call_args: std.ArrayList(CallArg) = .empty,
    params: std.ArrayList(Param) = .empty,
    demand_sites: std.ArrayList(u32) = .empty,

    pub fn deinit(self: *InputsBuilder, allocator: Allocator) void {
        self.copies.deinit(allocator);
        self.injects.deinit(allocator);
        self.projects.deinit(allocator);
        self.calls.deinit(allocator);
        self.call_args.deinit(allocator);
        self.params.deinit(allocator);
        self.demand_sites.deinit(allocator);
    }

    /// The finished inputs, owning `candidates` and `shared` and copies of
    /// every collected list.
    pub fn finish(
        self: *const InputsBuilder,
        allocator: Allocator,
        local_count: usize,
        candidates: []const u32,
        shared: []const u32,
    ) Allocator.Error!Inputs {
        const borrowed: Inputs = .{
            .local_count = @intCast(local_count),
            .copies = self.copies.items,
            .injects = self.injects.items,
            .projects = self.projects.items,
            .calls = self.calls.items,
            .call_args = self.call_args.items,
            .candidates = candidates,
            .params = self.params.items,
            .shared = shared,
            .demand_sites = self.demand_sites.items,
        };
        return borrowed.clone(allocator);
    }
};

/// One analyzed domain: an ownership component, or the whole store.
pub const Unit = struct {
    inputs: *const Inputs,
    /// The procedures whose parameters and returns the domain holds.
    procs: []const u32,
    /// Per entry of `procs`, the dense locals that procedure returns.
    returns: []const []const u32,
    /// Output: bit `c` set => candidate `c` takes its fresh form.
    needed: []u64,
};

/// One fresh demand: the place at `steps` of parameter `position`, where a
/// write in place relies on the argument's birth.
pub const FreshDemand = struct { position: u32, steps_start: u32, steps_len: u32 };

/// Every procedure's fresh demands, ordered by position and then path.
pub const FreshDemands = struct {
    offsets: []u32 = &.{},
    entries: []FreshDemand = &.{},
    steps: []Step = &.{},

    pub fn deinit(self: *FreshDemands, allocator: Allocator) void {
        allocator.free(self.offsets);
        allocator.free(self.entries);
        allocator.free(self.steps);
    }

    pub fn clone(self: *const FreshDemands, allocator: Allocator) Allocator.Error!FreshDemands {
        const offsets = try allocator.dupe(u32, self.offsets);
        errdefer allocator.free(offsets);
        const entries = try allocator.dupe(FreshDemand, self.entries);
        errdefer allocator.free(entries);
        return .{ .offsets = offsets, .entries = entries, .steps = try allocator.dupe(Step, self.steps) };
    }

    pub fn of(self: *const FreshDemands, proc: usize) []const FreshDemand {
        if (proc + 1 >= self.offsets.len) return &.{};
        return self.entries[self.offsets[proc]..self.offsets[proc + 1]];
    }

    pub fn stepsOf(self: *const FreshDemands, demand: FreshDemand) []const Step {
        return self.steps[demand.steps_start..][0..demand.steps_len];
    }

    /// Whether the procedure demands parameter `position`'s place `steps`.
    pub fn demands(self: *const FreshDemands, proc: usize, position: u32, steps: []const Step) bool {
        for (self.of(proc)) |demand| {
            if (demand.position != position) continue;
            const own = self.stepsOf(demand);
            if (own.len != steps.len) continue;
            var equal = true;
            for (own, steps) |a, b| equal = equal and a == b;
            if (equal) return true;
        }
        return false;
    }
};

/// An interned path; `root` is the empty path.
const PathId = u32;
const root: PathId = 0;

/// Paths interned as a trie: each node is its parent path plus one step.
const PathTable = struct {
    parents: std.ArrayList(PathId) = .empty,
    steps: std.ArrayList(Step) = .empty,
    depths: std.ArrayList(u32) = .empty,
    map: std.AutoHashMapUnmanaged(u64, PathId) = .empty,
    scratch: std.ArrayList(Step) = .empty,

    fn init(allocator: Allocator) Allocator.Error!PathTable {
        var table: PathTable = .{};
        try table.parents.append(allocator, root);
        try table.steps.append(allocator, 0);
        try table.depths.append(allocator, 0);
        return table;
    }

    fn deinit(self: *PathTable, allocator: Allocator) void {
        self.parents.deinit(allocator);
        self.steps.deinit(allocator);
        self.depths.deinit(allocator);
        self.map.deinit(allocator);
        self.scratch.deinit(allocator);
    }

    fn depth(self: *const PathTable, path: PathId) u32 {
        return self.depths.items[path];
    }

    fn child(self: *PathTable, allocator: Allocator, parent: PathId, step: Step) Allocator.Error!PathId {
        const key = (@as(u64, parent) << 32) | step;
        const entry = try self.map.getOrPut(allocator, key);
        if (entry.found_existing) return entry.value_ptr.*;
        const id: PathId = @intCast(self.parents.items.len);
        entry.value_ptr.* = id;
        try self.parents.append(allocator, parent);
        try self.steps.append(allocator, step);
        try self.depths.append(allocator, self.depth(parent) + 1);
        return id;
    }

    fn appendSteps(self: *PathTable, allocator: Allocator, base: PathId, steps: []const Step) Allocator.Error!PathId {
        var path = base;
        for (steps) |step| path = try self.child(allocator, path, step);
        return path;
    }

    fn ancestorAt(self: *const PathTable, path: PathId, at_depth: u32) PathId {
        var current = path;
        while (self.depth(current) > at_depth) current = self.parents.items[current];
        return current;
    }

    fn isPrefix(self: *const PathTable, prefix: PathId, path: PathId) bool {
        const prefix_depth = self.depth(prefix);
        if (prefix_depth > self.depth(path)) return false;
        return self.ancestorAt(path, prefix_depth) == prefix;
    }

    /// `base` extended by the steps of `path` below its ancestor `prefix`.
    fn graft(self: *PathTable, allocator: Allocator, base: PathId, prefix: PathId, path: PathId) Allocator.Error!PathId {
        std.debug.assert(self.isPrefix(prefix, path));
        if (prefix == path) return base;
        self.scratch.clearRetainingCapacity();
        var current = path;
        while (current != prefix) : (current = self.parents.items[current]) try self.scratch.append(allocator, self.steps.items[current]);
        var result = base;
        var index = self.scratch.items.len;
        while (index > 0) {
            index -= 1;
            result = try self.child(allocator, result, self.scratch.items[index]);
        }
        return result;
    }

    fn intern(self: *PathTable, allocator: Allocator, steps: []const Step) Allocator.Error!PathId {
        return self.appendSteps(allocator, root, steps);
    }

    /// The path's steps, outermost first, into `out`.
    fn stepsOf(self: *const PathTable, allocator: Allocator, path: PathId, out: *std.ArrayList(Step)) Allocator.Error!void {
        const start = out.items.len;
        var current = path;
        while (current != root) : (current = self.parents.items[current]) try out.append(allocator, self.steps.items[current]);
        std.mem.reverse(Step, out.items[start..]);
    }

    /// Lexicographic order of two paths' steps.
    fn order(self: *const PathTable, a: PathId, b: PathId) std.math.Order {
        if (a == b) return .eq;
        const a_depth = self.depth(a);
        const b_depth = self.depth(b);
        const common = @min(a_depth, b_depth);
        var left = self.ancestorAt(a, common);
        var right = self.ancestorAt(b, common);
        if (left == right) return std.math.order(a_depth, b_depth);
        // Climb to the children of the deepest common ancestor.
        while (self.parents.items[left] != self.parents.items[right]) {
            left = self.parents.items[left];
            right = self.parents.items[right];
        }
        return std.math.order(self.steps.items[left], self.steps.items[right]);
    }
};

/// The base of every derivation that stands for a second holder.
const shared_base: u32 = std.math.maxInt(u32);
/// A carry row's parameter when the row states a second holder.
const shared_param: u32 = std.math.maxInt(u32);

const Atom = struct { at: PathId, base: u32, from: PathId };

/// A carry row: the result's place `at` may hold argument `param`'s place
/// `from`, or, for `shared_param`, may have another holder.
const Row = struct {
    at: PathId,
    param: u32,
    from: PathId,

    fn lessThan(paths: *const PathTable, a: Row, b: Row) bool {
        if (a.param != b.param) return a.param < b.param;
        switch (paths.order(a.at, b.at)) {
            .lt => return true,
            .gt => return false,
            .eq => {},
        }
        return paths.order(a.from, b.from) == .lt;
    }
};

/// A parameter place a unit's demand site reaches, with its procedure.
const Demanded = struct { proc: u32, demand: Demand };

const Demand = struct {
    position: u32,
    path: PathId,

    fn lessThan(paths: *const PathTable, a: Demand, b: Demand) bool {
        if (a.position != b.position) return a.position < b.position;
        return paths.order(a.path, b.path) == .lt;
    }
};

/// Rows of edge indices grouped by their source local.
const Adjacency = struct {
    offsets: []u32,
    items: []u32,

    fn build(allocator: Allocator, local_count: usize, keys: []const u32) Allocator.Error!Adjacency {
        const offsets = try allocator.alloc(u32, local_count + 1);
        @memset(offsets, 0);
        for (keys) |key| offsets[key + 1] += 1;
        for (0..local_count) |index| offsets[index + 1] += offsets[index];
        const items = try allocator.alloc(u32, keys.len);
        const fill = try allocator.dupe(u32, offsets[0..local_count]);
        defer allocator.free(fill);
        for (keys, 0..) |key, index| {
            items[fill[key]] = @intCast(index);
            fill[key] += 1;
        }
        return .{ .offsets = offsets, .items = items };
    }

    fn row(self: Adjacency, key: u32) []const u32 {
        return self.items[self.offsets[key]..self.offsets[key + 1]];
    }
};

const no_atom: u32 = std.math.maxInt(u32);

/// One unit's derivations and the edge indices that drive them.
const UnitState = struct {
    unit: *const Unit,
    copies: Adjacency,
    injects: Adjacency,
    projects: Adjacency,
    /// Per local, the call arguments it is: indices into `Inputs.call_args`.
    args: Adjacency,
    /// The call each argument belongs to.
    arg_call: []u32,
    inject_steps: []PathId,
    project_steps: []PathId,
    head: []u32,
    atoms: std.ArrayList(struct { local: u32, atom: Atom, next: u32 }) = .empty,
    seen: std.AutoHashMapUnmanaged(struct { local: u32, at: PathId, base: u32, from: PathId }, void) = .empty,
    work: std.ArrayList(u32) = .empty,

    fn init(allocator: Allocator, unit: *const Unit, paths: *PathTable) Allocator.Error!UnitState {
        const inputs = unit.inputs;
        const local_count = inputs.local_count;
        const keys = try allocator.alloc(u32, @max(@max(inputs.copies.len, inputs.injects.len), @max(inputs.projects.len, inputs.call_args.len)));
        defer allocator.free(keys);
        for (inputs.copies, 0..) |edge, index| keys[index] = edge.source;
        const copies = try Adjacency.build(allocator, local_count, keys[0..inputs.copies.len]);
        for (inputs.injects, 0..) |edge, index| keys[index] = edge.source;
        const injects = try Adjacency.build(allocator, local_count, keys[0..inputs.injects.len]);
        for (inputs.projects, 0..) |edge, index| keys[index] = edge.source;
        const projects = try Adjacency.build(allocator, local_count, keys[0..inputs.projects.len]);
        const arg_call = try allocator.alloc(u32, inputs.call_args.len);
        var arg_count: usize = 0;
        for (inputs.calls, 0..) |call, call_index| {
            for (inputs.call_args[call.args_start..][0..call.args_len], call.args_start..) |arg, arg_index| {
                arg_call[arg_index] = @intCast(call_index);
                if (arg.local != no_local) arg_count += 1;
            }
        }
        // A local's argument rows name indices into `call_args`.
        const arg_keys = try allocator.alloc(u32, arg_count);
        defer allocator.free(arg_keys);
        const arg_ids = try allocator.alloc(u32, arg_count);
        defer allocator.free(arg_ids);
        var fill: usize = 0;
        for (inputs.call_args, 0..) |arg, arg_index| {
            if (arg.local == no_local) continue;
            arg_keys[fill] = arg.local;
            arg_ids[fill] = @intCast(arg_index);
            fill += 1;
        }
        const args = try Adjacency.build(allocator, local_count, arg_keys);
        for (args.items) |*item| item.* = arg_ids[item.*];
        const inject_steps = try allocator.alloc(PathId, inputs.injects.len);
        for (inputs.injects, inject_steps) |edge, *path| path.* = try paths.intern(allocator, edge.steps.slice());
        const project_steps = try allocator.alloc(PathId, inputs.projects.len);
        for (inputs.projects, project_steps) |edge, *path| path.* = try paths.intern(allocator, edge.steps.slice());
        const head = try allocator.alloc(u32, local_count);
        @memset(head, no_atom);
        return .{
            .unit = unit,
            .copies = copies,
            .injects = injects,
            .projects = projects,
            .args = args,
            .arg_call = arg_call,
            .inject_steps = inject_steps,
            .project_steps = project_steps,
            .head = head,
        };
    }

    fn reset(self: *UnitState) void {
        @memset(self.head, no_atom);
        self.atoms.clearRetainingCapacity();
        self.seen.clearRetainingCapacity();
        self.work.clearRetainingCapacity();
    }

    fn add(self: *UnitState, allocator: Allocator, local: u32, atom: Atom) Allocator.Error!void {
        const normalized: Atom = if (atom.base == shared_base) .{ .at = atom.at, .base = shared_base, .from = root } else atom;
        const entry = try self.seen.getOrPut(allocator, .{ .local = local, .at = normalized.at, .base = normalized.base, .from = normalized.from });
        if (entry.found_existing) return;
        const index: u32 = @intCast(self.atoms.items.len);
        try self.atoms.append(allocator, .{ .local = local, .atom = normalized, .next = self.head[local] });
        self.head[local] = index;
        try self.work.append(allocator, index);
    }

    /// Derives every local's derivations from the seeds and the callees'
    /// current rows.
    fn solve(self: *UnitState, allocator: Allocator, paths: *PathTable, rows: []const []const Row) Allocator.Error!void {
        self.reset();
        const inputs = self.unit.inputs;
        const candidate_count: u32 = @intCast(inputs.candidates.len);
        for (inputs.candidates, 0..) |target, index| try self.add(allocator, target, .{ .at = root, .base = @intCast(index), .from = root });
        for (inputs.params, 0..) |param, index| try self.add(allocator, param.local, .{ .at = root, .base = candidate_count + @as(u32, @intCast(index)), .from = root });
        for (inputs.shared) |local| try self.add(allocator, local, .{ .at = root, .base = shared_base, .from = root });
        for (inputs.calls) |call| {
            if (call.target == no_local) continue;
            for (rows[call.proc]) |row| {
                if (row.param == shared_param) try self.add(allocator, call.target, .{ .at = row.at, .base = shared_base, .from = root });
            }
        }
        while (self.work.pop()) |index| {
            const entry = self.atoms.items[index];
            const local = entry.local;
            const atom = entry.atom;
            for (self.copies.row(local)) |edge_index| try self.add(allocator, inputs.copies[edge_index].target, atom);
            for (self.injects.row(local)) |edge_index| {
                if (!inputs.injects[edge_index].live) continue;
                const steps = self.inject_steps[edge_index];
                try self.add(allocator, inputs.injects[edge_index].target, .{
                    .at = try paths.graft(allocator, steps, root, atom.at),
                    .base = atom.base,
                    .from = atom.from,
                });
            }
            for (self.projects.row(local)) |edge_index| {
                const steps = self.project_steps[edge_index];
                const target = inputs.projects[edge_index].target;
                // A second holder of the container as a whole is not one of
                // the field: a read that carries the field's stored unit is
                // one no copy of the container reaches.
                if (atom.base == shared_base and !paths.isPrefix(steps, atom.at)) continue;
                if (paths.isPrefix(atom.at, steps)) {
                    try self.add(allocator, target, .{ .at = root, .base = atom.base, .from = try paths.graft(allocator, atom.from, atom.at, steps) });
                } else if (paths.isPrefix(steps, atom.at)) {
                    try self.add(allocator, target, .{ .at = try paths.graft(allocator, root, steps, atom.at), .base = atom.base, .from = atom.from });
                }
            }
            for (self.args.row(local)) |arg_index| {
                const call = inputs.calls[self.arg_call[arg_index]];
                if (call.target == no_local or !inputs.call_args[arg_index].live) continue;
                const position = arg_index - call.args_start;
                for (rows[call.proc]) |row| {
                    if (row.param != position) continue;
                    if (paths.isPrefix(atom.at, row.from)) {
                        try self.add(allocator, call.target, .{ .at = row.at, .base = atom.base, .from = try paths.graft(allocator, atom.from, atom.at, row.from) });
                    } else if (paths.isPrefix(row.from, atom.at)) {
                        try self.add(allocator, call.target, .{ .at = try paths.graft(allocator, row.at, row.from, atom.at), .base = atom.base, .from = atom.from });
                    }
                }
            }
        }
    }

    /// The carry rows of the unit's procedure at `slot` of `procs`.
    fn rowsOf(self: *const UnitState, allocator: Allocator, paths: *const PathTable, slot: usize) Allocator.Error![]Row {
        const inputs = self.unit.inputs;
        const proc = self.unit.procs[slot];
        const candidate_count: u32 = @intCast(inputs.candidates.len);
        var rows: std.ArrayList(Row) = .empty;
        for (self.unit.returns[slot]) |local| {
            var cursor = self.head[local];
            while (cursor != no_atom) : (cursor = self.atoms.items[cursor].next) {
                const atom = self.atoms.items[cursor].atom;
                if (atom.base == shared_base) {
                    try rows.append(allocator, .{ .at = atom.at, .param = shared_param, .from = root });
                } else if (atom.base >= candidate_count) {
                    const param = inputs.params[atom.base - candidate_count];
                    if (param.proc == proc) try rows.append(allocator, .{ .at = atom.at, .param = param.position, .from = atom.from });
                }
            }
        }
        std.mem.sort(Row, rows.items, paths, Row.lessThan);
        var kept: usize = 0;
        for (rows.items) |row| {
            if (kept != 0) {
                const last = rows.items[kept - 1];
                if (last.at == row.at and last.param == row.param and last.from == row.from) continue;
            }
            rows.items[kept] = row;
            kept += 1;
        }
        rows.shrinkRetainingCapacity(kept);
        return rows.toOwnedSlice(allocator);
    }

    /// Records the base places the demand site `(local, site)` reaches.
    fn demandAt(
        self: *const UnitState,
        allocator: Allocator,
        paths: *PathTable,
        local: u32,
        site: PathId,
        needed: []u64,
        demanded: *std.ArrayList(Demanded),
    ) Allocator.Error!void {
        var cursor = self.head[local];
        while (cursor != no_atom) : (cursor = self.atoms.items[cursor].next) {
            const atom = self.atoms.items[cursor].atom;
            if (atom.base != shared_base) continue;
            if (paths.isPrefix(atom.at, site) or paths.isPrefix(site, atom.at)) return;
        }
        const inputs = self.unit.inputs;
        const candidate_count: u32 = @intCast(inputs.candidates.len);
        cursor = self.head[local];
        while (cursor != no_atom) : (cursor = self.atoms.items[cursor].next) {
            const atom = self.atoms.items[cursor].atom;
            if (atom.base == shared_base) continue;
            const place = if (paths.isPrefix(atom.at, site))
                try paths.graft(allocator, atom.from, atom.at, site)
            else if (paths.isPrefix(site, atom.at))
                atom.from
            else
                continue;
            if (atom.base < candidate_count) {
                needed[atom.base / 64] |= @as(u64, 1) << @as(u6, @intCast(atom.base % 64));
            } else {
                const param = inputs.params[atom.base - candidate_count];
                try demanded.append(allocator, .{ .proc = param.proc, .demand = .{ .position = param.position, .path = place } });
            }
        }
    }
};

fn sameRows(a: []const Row, b: []const Row) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left.at != right.at or left.param != right.param or left.from != right.from) return false;
    }
    return true;
}

fn sameDemands(a: []const Demand, b: []const Demand) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left.position != right.position or left.path != right.path) return false;
    }
    return true;
}

/// Settles carry rows, then fresh demands, each to its least fixpoint over
/// the units, fills every unit's `needed`, and returns every procedure's
/// fresh demands. A procedure no unit holds has no rows and no demands.
pub fn settle(gpa: Allocator, units: []const Unit, proc_count: usize) Allocator.Error!FreshDemands {
    var memory = std.heap.ArenaAllocator.init(gpa);
    defer memory.deinit();
    const arena = memory.allocator();
    var paths = try PathTable.init(arena);

    const states = try arena.alloc(UnitState, units.len);
    for (units, states) |*unit, *state| state.* = try UnitState.init(arena, unit, &paths);

    // The units whose calls reach each procedure, re-derived when its rows or
    // demands grow.
    const caller_counts = try arena.alloc(u32, proc_count + 1);
    @memset(caller_counts, 0);
    for (units) |unit| for (unit.inputs.calls) |call| {
        caller_counts[call.proc + 1] += 1;
    };
    for (0..proc_count) |proc| caller_counts[proc + 1] += caller_counts[proc];
    const caller_units = try arena.alloc(u32, caller_counts[proc_count]);
    const caller_fill = try arena.dupe(u32, caller_counts[0..proc_count]);
    for (units, 0..) |unit, unit_index| for (unit.inputs.calls) |call| {
        caller_units[caller_fill[call.proc]] = @intCast(unit_index);
        caller_fill[call.proc] += 1;
    };

    const rows = try arena.alloc([]const Row, proc_count);
    @memset(rows, &.{});
    var dirty = try std.bit_set.DynamicBitSetUnmanaged.initFull(arena, units.len);
    while (dirty.count() != 0) {
        var iter = dirty.iterator(.{});
        while (iter.next()) |unit_index| {
            dirty.unset(unit_index);
            const state = &states[unit_index];
            try state.solve(arena, &paths, rows);
            for (units[unit_index].procs, 0..) |proc, slot| {
                const new = try state.rowsOf(arena, &paths, slot);
                if (sameRows(rows[proc], new)) continue;
                rows[proc] = new;
                for (caller_units[caller_counts[proc]..caller_counts[proc + 1]]) |caller| dirty.set(caller);
            }
        }
    }

    const demands = try arena.alloc([]const Demand, proc_count);
    @memset(demands, &.{});
    var demanded: std.ArrayList(Demanded) = .empty;
    var unit_demands: std.ArrayList(Demand) = .empty;
    dirty.setRangeValue(.{ .start = 0, .end = units.len }, true);
    while (dirty.count() != 0) {
        var iter = dirty.iterator(.{});
        while (iter.next()) |unit_index| {
            dirty.unset(unit_index);
            const unit = units[unit_index];
            const state = &states[unit_index];
            const inputs = unit.inputs;
            @memset(unit.needed, 0);
            demanded.clearRetainingCapacity();
            for (inputs.demand_sites) |local| try state.demandAt(arena, &paths, local, root, unit.needed, &demanded);
            for (inputs.calls) |call| {
                for (demands[call.proc]) |demand| {
                    if (demand.position >= call.args_len) continue;
                    const arg = inputs.call_args[call.args_start + demand.position];
                    if (arg.local == no_local) continue;
                    try state.demandAt(arena, &paths, arg.local, demand.path, unit.needed, &demanded);
                }
            }
            for (unit.procs) |proc| {
                unit_demands.clearRetainingCapacity();
                for (demanded.items) |item| if (item.proc == proc) try unit_demands.append(arena, item.demand);
                std.mem.sort(Demand, unit_demands.items, @as(*const PathTable, &paths), Demand.lessThan);
                var kept: usize = 0;
                for (unit_demands.items) |demand| {
                    if (kept != 0 and unit_demands.items[kept - 1].position == demand.position and unit_demands.items[kept - 1].path == demand.path) continue;
                    unit_demands.items[kept] = demand;
                    kept += 1;
                }
                if (sameDemands(demands[proc], unit_demands.items[0..kept])) continue;
                demands[proc] = try arena.dupe(Demand, unit_demands.items[0..kept]);
                for (caller_units[caller_counts[proc]..caller_counts[proc + 1]]) |caller| dirty.set(caller);
            }
        }
    }

    var result: FreshDemands = .{};
    errdefer result.deinit(gpa);
    result.offsets = try gpa.alloc(u32, proc_count + 1);
    var entries: std.ArrayList(FreshDemand) = .empty;
    defer entries.deinit(gpa);
    var steps: std.ArrayList(Step) = .empty;
    defer steps.deinit(gpa);
    for (demands, 0..) |list, proc| {
        result.offsets[proc] = @intCast(entries.items.len);
        for (list) |demand| {
            const start: u32 = @intCast(steps.items.len);
            try paths.stepsOf(gpa, demand.path, &steps);
            try entries.append(gpa, .{ .position = demand.position, .steps_start = start, .steps_len = @intCast(steps.items.len - start) });
        }
    }
    result.offsets[proc_count] = @intCast(entries.items.len);
    result.entries = try entries.toOwnedSlice(gpa);
    result.steps = try steps.toOwnedSlice(gpa);
    return result;
}

test "a demand on a parameter's field composes through a returned tag's payload" {
    // callee(d) = Found({ data: d }); caller(d) = append((callee(d)).Found.data.entries).
    const allocator = std.testing.allocator;
    const callee_inputs: Inputs = .{
        .local_count = 3,
        .injects = &.{
            .{ .source = 0, .target = 1, .steps = Steps.one(fieldStep(0)), .live = true },
            .{ .source = 1, .target = 2, .steps = Steps.one(variantStep(0)), .live = true },
        },
        .params = &.{.{ .proc = 0, .position = 0, .local = 0 }},
    };
    // Caller locals: 0 the parameter, 1 the call's result, 2 the payload
    // view, 3 the data take, 4 the entries take.
    const caller_inputs: Inputs = .{
        .local_count = 5,
        .projects = &.{
            .{ .source = 1, .target = 2, .steps = Steps.one(variantStep(0)) },
            .{ .source = 2, .target = 3, .steps = Steps.one(fieldStep(0)) },
            .{ .source = 3, .target = 4, .steps = Steps.one(fieldStep(1)) },
        },
        .calls = &.{.{ .target = 1, .proc = 0, .args_start = 0, .args_len = 1 }},
        .call_args = &.{.{ .local = 0, .live = true }},
        .params = &.{.{ .proc = 1, .position = 0, .local = 0 }},
        .demand_sites = &.{4},
    };
    var callee_needed: [1]u64 = .{0};
    var caller_needed: [1]u64 = .{0};
    const units = [_]Unit{
        .{ .inputs = &callee_inputs, .procs = &.{0}, .returns = &.{&.{2}}, .needed = &callee_needed },
        .{ .inputs = &caller_inputs, .procs = &.{1}, .returns = &.{&.{}}, .needed = &caller_needed },
    };
    var demands = try settle(allocator, &units, 2);
    defer demands.deinit(allocator);
    try std.testing.expect(demands.demands(1, 0, &.{fieldStep(1)}));
    try std.testing.expectEqual(@as(usize, 1), demands.of(1).len);
    try std.testing.expectEqual(@as(usize, 0), demands.of(0).len);
}

test "a shared field demands nothing, and a store that is not the last use carries nothing" {
    const allocator = std.testing.allocator;
    // Locals: 0 and 1 parameters, 2 a record of both, 3 and 4 its takes;
    // parameter 0 has another holder, and the store of parameter 1 is not
    // its last use.
    const inputs: Inputs = .{
        .local_count = 5,
        .injects = &.{
            .{ .source = 0, .target = 2, .steps = Steps.one(fieldStep(0)), .live = true },
            .{ .source = 1, .target = 2, .steps = Steps.one(fieldStep(1)), .live = false },
        },
        .projects = &.{
            .{ .source = 2, .target = 3, .steps = Steps.one(fieldStep(0)) },
            .{ .source = 2, .target = 4, .steps = Steps.one(fieldStep(1)) },
        },
        .params = &.{ .{ .proc = 0, .position = 0, .local = 0 }, .{ .proc = 0, .position = 1, .local = 1 } },
        .shared = &.{0},
        .demand_sites = &.{ 3, 4 },
    };
    var needed: [1]u64 = .{0};
    const units = [_]Unit{.{ .inputs = &inputs, .procs = &.{0}, .returns = &.{&.{}}, .needed = &needed }};
    var demands = try settle(allocator, &units, 1);
    defer demands.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), demands.of(0).len);
}

test "a second holder of a record as a whole does not reach the field a take moves out" {
    const allocator = std.testing.allocator;
    // Locals: 0 the record parameter, held twice, and 1 its take.
    const inputs: Inputs = .{
        .local_count = 2,
        .projects = &.{.{ .source = 0, .target = 1, .steps = Steps.one(fieldStep(2)) }},
        .params = &.{.{ .proc = 0, .position = 0, .local = 0 }},
        .shared = &.{0},
        .demand_sites = &.{1},
    };
    var needed: [1]u64 = .{0};
    const units = [_]Unit{.{ .inputs = &inputs, .procs = &.{0}, .returns = &.{&.{}}, .needed = &needed }};
    var demands = try settle(allocator, &units, 1);
    defer demands.deinit(allocator);
    try std.testing.expect(demands.demands(0, 0, &.{fieldStep(2)}));
}

test "a candidate whose struct field reaches a callee's field demand is needed" {
    const allocator = std.testing.allocator;
    // fill(r) = append(r.items); main = fill(candidate).
    const fill_inputs: Inputs = .{
        .local_count = 2,
        .projects = &.{.{ .source = 0, .target = 1, .steps = Steps.one(fieldStep(1)) }},
        .params = &.{.{ .proc = 0, .position = 0, .local = 0 }},
        .demand_sites = &.{1},
    };
    const main_inputs: Inputs = .{
        .local_count = 2,
        .calls = &.{.{ .target = 1, .proc = 0, .args_start = 0, .args_len = 1 }},
        .call_args = &.{.{ .local = 0, .live = true }},
        .candidates = &.{0},
    };
    var fill_needed: [1]u64 = .{0};
    var main_needed: [1]u64 = .{0};
    const units = [_]Unit{
        .{ .inputs = &fill_inputs, .procs = &.{0}, .returns = &.{&.{}}, .needed = &fill_needed },
        .{ .inputs = &main_inputs, .procs = &.{1}, .returns = &.{&.{}}, .needed = &main_needed },
    };
    var demands = try settle(allocator, &units, 2);
    defer demands.deinit(allocator);
    try std.testing.expect(demands.demands(0, 0, &.{fieldStep(1)}));
    try std.testing.expectEqual(@as(u64, 1), main_needed[0]);
}
