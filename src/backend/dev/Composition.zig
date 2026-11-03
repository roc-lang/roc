//! One program's composition from its units' pieces (design.md "Unit Pieces
//! And Whole-Program Dead Code"): the excerpts the program's roots reach,
//! across every unit, in the canonical order of an arranged image, with each
//! procedure and helper defined once, by the first unit defining it, and
//! every constant the kept code names.
//!
//! The walk is exact: an excerpt is kept when a kept excerpt, an entrypoint
//! wrapper or a host-visible constant names it. Resolving what an excerpt
//! names (its references by index, its symbolic references by identity or
//! name, its relocations by symbol, and the function pointers in the
//! constants it carries) is the costly half of the walk and depends on
//! nothing but the excerpt and the program's index of definitions, so it
//! runs per unit on the post-check executor, from the procedures the checked
//! program reaches in each unit (`Options.seeds`). The walk that decides what
//! is kept then follows the resolved edges, resolving on the spot whatever
//! the seeds missed; the seeds decide only what is resolved in parallel,
//! never what is kept.
//!
//! The arranged image borrows the units' code, metadata and constants. Only
//! its references, which index the image, and its line entries, which name
//! the image's source-file table, are its own.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const lir = @import("lir");
const CodeExcerpt = @import("CodeExcerpt.zig");
const tasks = base.post_check_task_executor;
const Allocator = std.mem.Allocator;
const Set = CodeExcerpt.Set;
const Excerpt = CodeExcerpt.Excerpt;
const DataItem = CodeExcerpt.DataItem;
const DataGraph = CodeExcerpt.DataGraph;
const ProcIdentity = lir.ProcIdentity;
const invariant = base.invariant;

/// One excerpt of one unit.
pub const Ref = struct { unit: u32, index: u32 };

/// What the composition keeps.
pub const Roots = struct {
    /// Every procedure excerpt of every unit is a root: an image the compiler
    /// loads whole, since the evaluator may call any export.
    every_proc: bool = false,
    procs: []const ProcIdentity = &.{},
    /// Symbols the host binds by name, roots where they name code.
    data: []const []const u8 = &.{},
    /// Every entrypoint wrapper is a root.
    entrypoints: bool = true,
};

pub const Options = struct {
    /// Per unit, parallel to the sets: the procedures the checked program
    /// reaches in it, which the parallel resolution starts from. Empty for
    /// every unit when nothing is known; a unit the seeds misjudge is
    /// resolved as the walk reaches it.
    seeds: []const []const ProcIdentity = &.{},
    executor: ?tasks.Executor = null,
};

/// Compose `sets`, the units' pieces in unit order, into one arranged image.
pub fn compose(
    allocator: Allocator,
    sets: []const *const Set,
    host_visible: []const DataItem,
    helper_aliases: *const std.StringHashMap([]const u8),
    roots: Roots,
    options: Options,
) Allocator.Error!CodeExcerpt.ArrangedImage {
    std.debug.assert(options.seeds.len == 0 or options.seeds.len == sets.len);
    var index = try Index.init(allocator, sets, helper_aliases);
    defer index.deinit(allocator);
    const units = try allocator.alloc(Unit, sets.len);
    var unit_count: usize = 0;
    defer {
        for (units[0..unit_count]) |*unit| unit.deinit();
        allocator.free(units);
    }
    for (sets, units, 0..) |set, *unit, id| {
        unit.* = try Unit.init(allocator, set, @intCast(id), &index);
        unit_count += 1;
    }
    var work = std.ArrayList(Ref).empty;
    defer work.deinit(allocator);
    for (roots.procs) |identity| if (index.by_proc.get(identity)) |ref| try work.append(allocator, ref);
    for (roots.data) |name| if (index.lookupName(name)) |ref| try work.append(allocator, ref);
    for (host_visible) |item| for (item.relocations) |relocation| {
        if (index.lookupName(relocation.name)) |ref| try work.append(allocator, ref);
    };
    for (sets, 0..) |set, unit_id| for (set.excerpts, 0..) |excerpt, i| {
        const ref: Ref = .{ .unit = @intCast(unit_id), .index = @intCast(i) };
        switch (excerpt.kind) {
            .entrypoint => if (roots.entrypoints) try work.append(allocator, ref),
            .proc => if (roots.every_proc) try work.append(allocator, index.canonical(excerpt, ref)),
            .rc_helper, .message_pool_run, .branch_island => {},
        }
    };
    for (work.items) |ref| try units[ref.unit].seeds.append(allocator, ref.index);
    for (options.seeds) |identities| for (identities) |identity| {
        if (index.by_proc.get(identity)) |ref| try units[ref.unit].seeds.append(allocator, ref.index);
    };
    try resolveSeeded(units, options.executor);
    var reach = try Reach.init(allocator, units);
    defer reach.deinit();
    try reach.walk(work.items);
    return try merge(allocator, sets, units, &reach, host_visible, &index);
}

/// Where every procedure and helper of the program is defined: in the first
/// unit defining it, which every reference to it lands on.
const Index = struct {
    by_proc: std.AutoHashMapUnmanaged(ProcIdentity, Ref) = .empty,
    /// Procedures by the leading bytes their symbol names spell.
    by_symbol: std.AutoHashMapUnmanaged([16]u8, Ref) = .empty,
    /// Helpers by name, their aliases included.
    by_name: std.StringHashMapUnmanaged(Ref) = .empty,

    fn init(allocator: Allocator, sets: []const *const Set, helper_aliases: *const std.StringHashMap([]const u8)) Allocator.Error!Index {
        var self: Index = .{};
        errdefer self.deinit(allocator);
        for (sets, 0..) |set, unit_id| for (set.excerpts, 0..) |excerpt, i| {
            const ref: Ref = .{ .unit = @intCast(unit_id), .index = @intCast(i) };
            switch (excerpt.kind) {
                .proc => |identity| {
                    const gop = try self.by_proc.getOrPut(allocator, identity);
                    if (gop.found_existing) continue;
                    gop.value_ptr.* = ref;
                    const symbol = try self.by_symbol.getOrPut(allocator, identity.bytes[0..16].*);
                    if (!symbol.found_existing) symbol.value_ptr.* = ref;
                },
                .rc_helper => |name| {
                    const gop = try self.by_name.getOrPut(allocator, name);
                    if (!gop.found_existing) gop.value_ptr.* = ref;
                },
                .entrypoint, .message_pool_run, .branch_island => {},
            }
        };
        var aliases = helper_aliases.iterator();
        while (aliases.next()) |alias| {
            if (self.by_name.get(alias.value_ptr.*)) |ref| try self.by_name.put(allocator, alias.key_ptr.*, ref);
        }
        return self;
    }

    fn deinit(self: *Index, allocator: Allocator) void {
        self.by_proc.deinit(allocator);
        self.by_symbol.deinit(allocator);
        self.by_name.deinit(allocator);
    }

    /// The excerpt a symbol names: a procedure by its symbol name, or a
    /// helper by its name or an alias of it. Null for a symbol no unit
    /// defines, such as a host or runtime function.
    fn lookupName(self: *const Index, name: []const u8) ?Ref {
        if (name.len == ProcIdentity.symbol_name_len and std.mem.startsWith(u8, name, ProcIdentity.symbol_name_prefix)) {
            var leading: [16]u8 = undefined;
            if (std.fmt.hexToBytes(&leading, name[ProcIdentity.symbol_name_prefix.len..])) |_| {
                return self.by_symbol.get(leading);
            } else |_| {}
        }
        return self.by_name.get(name);
    }

    /// The definition `excerpt`, at `ref`, stands for: a procedure's or a
    /// helper's first definition, and any other excerpt itself.
    fn canonical(self: *const Index, excerpt: Excerpt, ref: Ref) Ref {
        return switch (excerpt.kind) {
            .proc => |identity| self.by_proc.get(identity) orelse unreachable, // every procedure excerpt was indexed
            .rc_helper => |name| self.by_name.get(name) orelse unreachable, // every helper excerpt was indexed
            .entrypoint, .message_pool_run, .branch_island => ref,
        };
    }
};

/// One unit's pieces and what each of them names, resolved as needed.
const Unit = struct {
    gpa: Allocator,
    set: *const Set,
    id: u32,
    index: *const Index,
    arena: collections.SingleThreadArena,
    /// Per excerpt, the excerpts its code names, once resolved.
    edges: []?[]const Ref,
    /// Per constant graph the unit's excerpts view, per node, the code the
    /// node's function pointers name, once resolved.
    nodes: std.AutoHashMapUnmanaged(*const DataGraph, []?[]const Ref) = .empty,
    /// The excerpts the parallel resolution starts from.
    seeds: std.ArrayList(u32) = .empty,
    scratch: std.ArrayList(Ref) = .empty,
    node_work: std.ArrayList(u32) = .empty,
    oom: bool = false,

    fn init(gpa: Allocator, set: *const Set, id: u32, index: *const Index) Allocator.Error!Unit {
        const edges = try gpa.alloc(?[]const Ref, set.excerpts.len);
        @memset(edges, null);
        return .{ .gpa = gpa, .set = set, .id = id, .index = index, .arena = collections.SingleThreadArena.init(gpa), .edges = edges };
    }

    fn deinit(self: *Unit) void {
        self.nodes.deinit(self.gpa);
        self.seeds.deinit(self.gpa);
        self.scratch.deinit(self.gpa);
        self.node_work.deinit(self.gpa);
        self.gpa.free(self.edges);
        self.arena.deinit();
    }

    /// The excerpts excerpt `i`'s code and carried constants name, each the
    /// definition a reference to it lands on.
    fn resolveExcerpt(self: *Unit, i: u32) Allocator.Error![]const Ref {
        if (self.edges[i]) |edges| return edges;
        self.scratch.clearRetainingCapacity();
        const excerpt = self.set.excerpts[i];
        for (excerpt.refs) |ref| {
            try self.scratch.append(self.gpa, self.index.canonical(self.set.excerpts[ref.target], .{ .unit = self.id, .index = ref.target }));
        }
        for (excerpt.symbolic_refs) |ref| {
            const target: ?Ref = switch (ref.binding) {
                .local => |local| self.index.canonical(self.set.excerpts[local], .{ .unit = self.id, .index = local }),
                .absent => null,
                .unresolved => switch (ref.target) {
                    .proc => |identity| self.index.by_proc.get(identity),
                    .rc_helper => |name| self.index.by_name.get(name),
                },
            };
            if (target) |next| try self.scratch.append(self.gpa, next);
        }
        for (excerpt.relocations) |relocation| {
            if (self.index.lookupName(relocation.name)) |next| try self.scratch.append(self.gpa, next);
        }
        for (excerpt.data) |item| for (item.relocations) |relocation| {
            if (!relocation.function) continue;
            if (self.index.lookupName(relocation.name)) |next| try self.scratch.append(self.gpa, next);
        };
        const edges = try self.arena.allocator().dupe(Ref, self.scratch.items);
        self.edges[i] = edges;
        return edges;
    }

    fn nodeSlots(self: *Unit, graph: *const DataGraph) Allocator.Error![]?[]const Ref {
        const gop = try self.nodes.getOrPut(self.gpa, graph);
        if (!gop.found_existing) {
            const slots = try self.arena.allocator().alloc(?[]const Ref, graph.items.len);
            @memset(slots, null);
            gop.value_ptr.* = slots;
        }
        return gop.value_ptr.*;
    }

    /// The excerpts the function pointers of `graph`'s node `node` name.
    fn nodeEdges(self: *Unit, graph: *const DataGraph, node: u32) Allocator.Error![]const Ref {
        const slots = try self.nodeSlots(graph);
        if (slots[node]) |edges| return edges;
        self.scratch.clearRetainingCapacity();
        for (graph.items[node].item.relocations) |relocation| {
            if (!relocation.function) continue;
            if (self.index.lookupName(relocation.name)) |next| try self.scratch.append(self.gpa, next);
        }
        const edges = try self.arena.allocator().dupe(Ref, self.scratch.items);
        slots[node] = edges;
        return edges;
    }

    /// Resolve the excerpts the seeds reach within this unit, and the
    /// constants they carry.
    fn prepare(self: *Unit) Allocator.Error!void {
        while (self.seeds.pop()) |i| {
            if (self.edges[i] != null) continue;
            const edges = try self.resolveExcerpt(i);
            for (edges) |next| {
                if (next.unit == self.id and self.edges[next.index] == null) try self.seeds.append(self.gpa, next.index);
            }
            if (self.set.excerpts[i].data_graph) |view| try self.prepareNodes(view);
        }
    }

    fn prepareNodes(self: *Unit, view: CodeExcerpt.DataGraphView) Allocator.Error!void {
        const slots = try self.nodeSlots(view.graph);
        try self.node_work.appendSlice(self.gpa, view.roots);
        while (self.node_work.pop()) |node| {
            if (slots[node] != null) continue;
            _ = try self.nodeEdges(view.graph, node);
            for (view.graph.items[node].edges) |edge| {
                try self.node_work.append(self.gpa, edge orelse invariant("prepared constant graph has a missing data target", .{}));
            }
        }
    }

    fn prepareTask(context: *anyopaque, _: tasks.Worker) ?*anyopaque {
        const self: *Unit = @ptrCast(@alignCast(context));
        self.prepare() catch {
            self.oom = true;
        };
        return null;
    }
};

/// Resolve every unit's seeded excerpts, each unit as one task of
/// `executor`, or one after another without one.
fn resolveSeeded(units: []Unit, executor: ?tasks.Executor) Allocator.Error!void {
    if (executor) |pool| {
        var count: usize = 0;
        for (units) |unit| count += @intFromBool(unit.seeds.items.len != 0);
        if (count == 0) return;
        const gpa = units[0].gpa;
        const jobs = try gpa.alloc(tasks.Task, count);
        defer gpa.free(jobs);
        const completions = try gpa.alloc(tasks.Completion, count);
        defer gpa.free(completions);
        var next: usize = 0;
        for (units) |*unit| {
            if (unit.seeds.items.len == 0) continue;
            jobs[next] = .{ .id = unit.id, .context = unit, .run = Unit.prepareTask };
            next += 1;
        }
        try pool.run(jobs, completions);
        for (units) |unit| if (unit.oom) return error.OutOfMemory;
        return;
    }
    for (units) |*unit| try unit.prepare();
}

/// The walk that decides what the program keeps.
const Reach = struct {
    gpa: Allocator,
    units: []Unit,
    /// Per unit, per excerpt.
    reached: [][]bool,
    /// Per constant graph, per node.
    nodes_seen: std.AutoHashMapUnmanaged(*const DataGraph, []bool) = .empty,
    work: std.ArrayList(Ref) = .empty,
    node_work: std.ArrayList(Node) = .empty,

    const Node = struct { graph: *const DataGraph, node: u32 };

    fn init(gpa: Allocator, units: []Unit) Allocator.Error!Reach {
        const reached = try gpa.alloc([]bool, units.len);
        var allocated: usize = 0;
        errdefer {
            for (reached[0..allocated]) |flags| gpa.free(flags);
            gpa.free(reached);
        }
        for (units, reached) |unit, *flags| {
            flags.* = try gpa.alloc(bool, unit.set.excerpts.len);
            @memset(flags.*, false);
            allocated += 1;
        }
        return .{ .gpa = gpa, .units = units, .reached = reached };
    }

    fn deinit(self: *Reach) void {
        for (self.reached) |flags| self.gpa.free(flags);
        self.gpa.free(self.reached);
        var flags_it = self.nodes_seen.valueIterator();
        while (flags_it.next()) |flags| self.gpa.free(flags.*);
        self.nodes_seen.deinit(self.gpa);
        self.work.deinit(self.gpa);
        self.node_work.deinit(self.gpa);
    }

    fn nodeFlags(self: *Reach, graph: *const DataGraph) Allocator.Error![]bool {
        const gop = try self.nodes_seen.getOrPut(self.gpa, graph);
        if (!gop.found_existing) {
            errdefer _ = self.nodes_seen.remove(graph);
            const flags = try self.gpa.alloc(bool, graph.items.len);
            @memset(flags, false);
            gop.value_ptr.* = flags;
        }
        return gop.value_ptr.*;
    }

    fn walk(self: *Reach, roots: []const Ref) Allocator.Error!void {
        try self.work.appendSlice(self.gpa, roots);
        while (self.work.pop()) |ref| {
            const flags = self.reached[ref.unit];
            if (flags[ref.index]) continue;
            flags[ref.index] = true;
            const unit = &self.units[ref.unit];
            try self.work.appendSlice(self.gpa, try unit.resolveExcerpt(ref.index));
            const view = unit.set.excerpts[ref.index].data_graph orelse continue;
            for (view.roots) |root| try self.node_work.append(self.gpa, .{ .graph = view.graph, .node = root });
            while (self.node_work.pop()) |node| {
                const node_flags = try self.nodeFlags(node.graph);
                if (node_flags[node.node]) continue;
                node_flags[node.node] = true;
                try self.work.appendSlice(self.gpa, try unit.nodeEdges(node.graph, node.node));
                for (node.graph.items[node.node].edges) |edge| {
                    try self.node_work.append(self.gpa, .{ .graph = node.graph, .node = edge orelse invariant("prepared constant graph has a missing data target", .{}) });
                }
            }
        }
    }
};

const dropped: u32 = std.math.maxInt(u32);

/// Lay the kept excerpts out canonically (`CodeExcerpt.arrange`): procedures
/// by identity, then helpers by name, then entrypoints by name; a message
/// pool run follows the first kept excerpt whose code addresses it, in the
/// order that excerpt first addresses its runs, and the rest follow in code
/// order. The image's constants are the host-visible ones, then every
/// constant the kept code carries, one per name, in name order.
fn merge(
    allocator: Allocator,
    sets: []const *const Set,
    units: []Unit,
    reach: *const Reach,
    host_visible: []const DataItem,
    index: *const Index,
) Allocator.Error!CodeExcerpt.ArrangedImage {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();

    var kept = std.ArrayList(Ref).empty;
    defer kept.deinit(allocator);
    for (reach.reached, 0..) |flags, unit_id| for (flags, 0..) |flag, i| {
        if (flag) try kept.append(allocator, .{ .unit = @intCast(unit_id), .index = @intCast(i) });
    };
    // The owner of each message pool run: the first kept excerpt whose code
    // addresses it.
    const owner_of = try allocator.alloc([]u32, sets.len);
    var owners_allocated: usize = 0;
    defer {
        for (owner_of[0..owners_allocated]) |owners| allocator.free(owners);
        allocator.free(owner_of);
    }
    for (sets, owner_of) |set, *owners| {
        owners.* = try allocator.alloc(u32, set.excerpts.len);
        @memset(owners.*, dropped);
        owners_allocated += 1;
    }
    for (kept.items) |ref| for (sets[ref.unit].excerpts[ref.index].refs) |edge| {
        if (sets[ref.unit].excerpts[edge.target].kind != .message_pool_run) continue;
        if (owner_of[ref.unit][edge.target] == dropped) owner_of[ref.unit][edge.target] = ref.index;
    };
    var heads = std.ArrayList(Ref).empty;
    defer heads.deinit(allocator);
    var unowned_runs = std.ArrayList(Ref).empty;
    defer unowned_runs.deinit(allocator);
    for (kept.items) |ref| switch (sets[ref.unit].excerpts[ref.index].kind) {
        .message_pool_run => if (owner_of[ref.unit][ref.index] == dropped) try unowned_runs.append(allocator, ref),
        .proc, .rc_helper, .entrypoint, .branch_island => try heads.append(allocator, ref),
    };
    std.mem.sort(Ref, heads.items, sets, headBefore);
    std.mem.sort(Ref, unowned_runs.items, sets, struct {
        fn lessThan(owner: []const *const Set, lhs: Ref, rhs: Ref) bool {
            return std.mem.lessThan(u8, owner[lhs.unit].excerpts[lhs.index].code, owner[rhs.unit].excerpts[rhs.index].code);
        }
    }.lessThan);

    const position = try allocator.alloc([]u32, sets.len);
    var positions_allocated: usize = 0;
    defer {
        for (position[0..positions_allocated]) |positions| allocator.free(positions);
        allocator.free(position);
    }
    for (sets, position) |set, *positions| {
        positions.* = try allocator.alloc(u32, set.excerpts.len);
        @memset(positions.*, dropped);
        positions_allocated += 1;
    }
    var order = try std.ArrayList(Ref).initCapacity(allocator, kept.items.len);
    defer order.deinit(allocator);
    for (heads.items) |head| {
        position[head.unit][head.index] = @intCast(order.items.len);
        order.appendAssumeCapacity(head);
        for (sets[head.unit].excerpts[head.index].refs) |edge| {
            const run: Ref = .{ .unit = head.unit, .index = edge.target };
            if (sets[run.unit].excerpts[run.index].kind != .message_pool_run or owner_of[run.unit][run.index] != head.index) continue;
            if (position[run.unit][run.index] != dropped) continue;
            position[run.unit][run.index] = @intCast(order.items.len);
            order.appendAssumeCapacity(run);
        }
    }
    for (unowned_runs.items) |run| {
        position[run.unit][run.index] = @intCast(order.items.len);
        order.appendAssumeCapacity(run);
    }
    std.debug.assert(order.items.len == kept.items.len);

    // The image's source-file table unites the units', each file once.
    var source_files = std.ArrayList([32]u8).empty;
    const file_maps = try allocator.alloc([]u32, sets.len);
    var maps_allocated: usize = 0;
    defer {
        for (file_maps[0..maps_allocated]) |map| allocator.free(map);
        allocator.free(file_maps);
    }
    for (sets, file_maps) |set, *map| {
        map.* = try allocator.alloc(u32, set.source_files.len);
        maps_allocated += 1;
        for (set.source_files, map.*) |identity, *file| {
            file.* = for (source_files.items, 0..) |existing, i| {
                if (std.mem.eql(u8, &existing, &identity)) break @intCast(i);
            } else index: {
                try source_files.append(a, identity);
                break :index @intCast(source_files.items.len - 1);
            };
        }
    }

    const excerpts = try a.alloc(Excerpt, order.items.len);
    for (order.items, excerpts) |ref, *out| {
        const set = sets[ref.unit];
        const source = set.excerpts[ref.index];
        out.* = source;
        const refs = try a.dupe(CodeExcerpt.Reference, source.refs);
        for (refs) |*edge| edge.target = placed(position, index.canonical(set.excerpts[edge.target], .{ .unit = ref.unit, .index = edge.target }));
        out.refs = refs;
        if (source.symbolic_refs.len != 0) {
            const symbolic_refs = try a.dupe(CodeExcerpt.SymbolicReference, source.symbolic_refs);
            for (symbolic_refs) |*edge| if (edge.binding == .local) {
                const local = edge.binding.local;
                edge.binding = .{ .local = placed(position, index.canonical(set.excerpts[local], .{ .unit = ref.unit, .index = local })) };
            };
            out.symbolic_refs = symbolic_refs;
        }
        const map = file_maps[ref.unit];
        if (source.lines.len != 0) {
            const lines = try a.dupe(CodeExcerpt.LineEntry, source.lines);
            for (lines) |*line| line.loc = CodeExcerpt.rebindLoc(line.loc, map);
            out.lines = lines;
        }
        out.loc = CodeExcerpt.rebindLoc(source.loc, map);
    }
    _ = units;

    // Every constant the kept code names, once per name.
    var all_items = std.ArrayList(DataItem).empty;
    defer all_items.deinit(allocator);
    var collected = std.StringHashMap(void).init(allocator);
    defer collected.deinit();
    var visible = std.StringHashMap(void).init(allocator);
    defer visible.deinit();
    for (host_visible) |item| {
        // A constant the program named for itself is named by content, so
        // no host can name it.
        if (!item.program_local_name) try visible.put(item.name, {});
        if ((try collected.getOrPut(item.name)).found_existing) continue;
        try all_items.append(allocator, item);
    }
    var collector = CodeExcerpt.DataCollector{ .allocator = allocator };
    defer collector.deinit();
    for (excerpts) |excerpt| try collector.add(excerpt);
    for (collector.items.items) |item| {
        // Each unit named its constants by content, so constants of
        // different units meet under one name and nothing is renamed here.
        if (item.program_local_name and !item.is_mutable) invariant("a composed unit carries a constant it did not name by content: {s}", .{item.name});
        if ((try collected.getOrPut(item.name)).found_existing) continue;
        try all_items.append(allocator, item);
    }
    const data = try a.alloc(CodeExcerpt.ArrangedDatum, all_items.items.len);
    for (all_items.items, data) |item, *datum| datum.* = .{ .item = item, .host_visible = visible.contains(item.name) };
    // One datum per name, so ordering by name alone is total.
    std.mem.sortUnstable(CodeExcerpt.ArrangedDatum, data, {}, struct {
        fn lessThan(_: void, lhs: CodeExcerpt.ArrangedDatum, rhs: CodeExcerpt.ArrangedDatum) bool {
            return std.mem.lessThan(u8, lhs.item.name, rhs.item.name);
        }
    }.lessThan);

    const files = try source_files.toOwnedSlice(a);
    const retained = try Set.retainOwners(a, excerpts);
    return .{ .set = .{ .arena = arena, .excerpts = excerpts, .source_files = files, .owners = retained }, .data = data };
}

/// Where a kept excerpt's definition landed in the image.
fn placed(position: []const []const u32, ref: Ref) u32 {
    const at = position[ref.unit][ref.index];
    if (at == dropped) invariant("a kept excerpt names an excerpt the composition dropped", .{});
    return at;
}

/// The canonical order of two head excerpts: by kind, then by what names
/// the excerpt.
fn headBefore(sets: []const *const Set, lhs: Ref, rhs: Ref) bool {
    const left = sets[lhs.unit].excerpts[lhs.index];
    const right = sets[rhs.unit].excerpts[rhs.index];
    const left_rank = CodeExcerpt.canonicalRank(left.kind);
    const right_rank = CodeExcerpt.canonicalRank(right.kind);
    if (left_rank != right_rank) return left_rank < right_rank;
    return switch (left.kind) {
        .proc => |identity| std.mem.lessThan(u8, &identity.bytes, &right.kind.proc.bytes),
        .rc_helper => |name| std.mem.lessThan(u8, name, right.kind.rc_helper),
        .entrypoint => |name| std.mem.lessThan(u8, name, right.kind.entrypoint),
        .branch_island, .message_pool_run => std.mem.lessThan(u8, left.code, right.code),
    };
}

fn testIdentity(seed: u8) ProcIdentity {
    return .{ .bytes = @splat(seed) };
}

fn testSet(allocator: Allocator, excerpts: []const Excerpt) Allocator.Error!Set {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const owned = try arena.allocator().dupe(Excerpt, excerpts);
    return .{ .arena = arena, .excerpts = owned };
}

fn testCompose(allocator: Allocator, sets: []const *const Set, seeds: []const []const ProcIdentity) Allocator.Error!CodeExcerpt.ArrangedImage {
    var aliases = std.StringHashMap([]const u8).init(allocator);
    defer aliases.deinit();
    return try compose(allocator, sets, &.{}, &aliases, .{}, .{ .seeds = seeds });
}

test "composition keeps what the roots reach, defines shared code once, and ignores misjudged seeds" {
    const allocator = std.testing.allocator;
    const p1 = testIdentity(1);
    const p2 = testIdentity(2);
    const p3 = testIdentity(3);
    const dead = testIdentity(9);
    const call: CodeExcerpt.NamedRelocation = .{ .offset = 0, .name = "roc__rc_x", .scope = .shared, .kind = .{ .function = .aarch64_call26 } };
    // Unit A: the entrypoint calls P1, which calls P2 of unit B and the
    // helper both units define; D is dead.
    var unit_a = try testSet(allocator, &.{
        .{ .kind = .{ .entrypoint = "roc__main" }, .code = "E", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .proc = p1 }, .code = "P1", .entry = 0, .frame = null, .refs = &.{}, .symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = p2 } }}, .relocations = &.{call}, .data = &.{} },
        .{ .kind = .{ .rc_helper = "roc__rc_x" }, .code = "HA", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .proc = dead }, .code = "D", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    defer unit_a.deinit();
    var unit_b = try testSet(allocator, &.{
        .{ .kind = .{ .proc = p2 }, .code = "P2", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .rc_helper = "roc__rc_x" }, .code = "HB", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .proc = p3 }, .code = "P3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    defer unit_b.deinit();
    const sets = [_]*const Set{ &unit_a, &unit_b };
    const seedings = [_][]const []const ProcIdentity{
        &.{},
        &.{ &.{dead}, &.{} },
        &.{ &.{p1}, &.{p2} },
        &.{ &.{ p1, dead }, &.{ p2, p3 } },
    };
    for (seedings) |seeds| {
        var composed = try testCompose(allocator, &sets, seeds);
        defer composed.deinit();
        const excerpts = composed.set.excerpts;
        try std.testing.expectEqual(@as(usize, 4), excerpts.len);
        try std.testing.expectEqualStrings("P1", excerpts[0].code);
        try std.testing.expectEqualStrings("P2", excerpts[1].code);
        try std.testing.expectEqualStrings("HA", excerpts[2].code);
        try std.testing.expectEqualStrings("E", excerpts[3].code);
        // The entrypoint's call lands on P1, and P2's on the one helper.
        try std.testing.expectEqual(@as(u32, 0), excerpts[3].refs[0].target);
        try std.testing.expectEqual(@as(u32, 2), excerpts[1].refs[0].target);
        try std.testing.expect(excerpts[0].symbolic_refs[0].binding == .unresolved);
        try std.testing.expectEqual(@as(usize, 0), composed.data.len);
    }
}

test "composition with every procedure a root keeps only the helpers they reach" {
    const allocator = std.testing.allocator;
    var unit = try testSet(allocator, &.{
        .{ .kind = .{ .proc = testIdentity(1) }, .code = "P1", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .rc_helper = "roc__rc_used" }, .code = "HU", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .rc_helper = "roc__rc_unused" }, .code = "HX", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    defer unit.deinit();
    var aliases = std.StringHashMap([]const u8).init(allocator);
    defer aliases.deinit();
    var composed = try compose(allocator, &.{&unit}, &.{}, &aliases, .{ .every_proc = true }, .{});
    defer composed.deinit();
    try std.testing.expectEqual(@as(usize, 2), composed.set.excerpts.len);
    try std.testing.expectEqualStrings("P1", composed.set.excerpts[0].code);
    try std.testing.expectEqualStrings("HU", composed.set.excerpts[1].code);
}
