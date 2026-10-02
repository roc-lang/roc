//! Canonical enumeration of a type scheme's static-dispatch constraints
//! ("evidence params").
//!
//! Every scheme with dispatch constraints gets one ordered param list: index
//! `k` in that list is the identity a dispatch plan's `constraint(k)`
//! resolution and a call edge's k-th evidence entry both refer to. The order
//! is defined purely by the scheme's type structure, so the definition's own
//! module and a caller holding a structural copy of the scheme (an import
//! copy, or the pristine root recorded by a `SchemeUseRecord`)
//! enumerate identical lists without sharing var identities.
//!
//! Order contract: depth-first over the resolved type structure—function
//! args then return, ordinary alias/nominal args then backing, and logical row
//! fields/tags across the complete extension chain, all in store order—
//! emitting one target parameter per dispatcher/method at the constrained
//! var's first occurrence; then every independent constraint fn type is walked
//! the same way in source order (they can
//! bind further constrained vars, e.g. `where [a.iter : a -> i, i.next : ..]`).
//!
//! Each param also carries the semantic PATH from the scheme root to its
//! dispatcher's first occurrence (function arg positions, type arguments,
//! row labels, …). Row-extension storage chains, including transparent aliases
//! within them, are normalized away: a field or tag payload in any tail is
//! addressed directly from the logical row root. Compiler-generated call edges—structural-derivation
//! component calls, builtin helper calls—have no checked instantiation
//! records, so monotype resolves a target's obligations by walking these
//! paths over the concrete monomorphic callable instead.

const std = @import("std");
const Ident = @import("base").Ident;
const types_mod = @import("types");

const Allocator = std.mem.Allocator;
const Var = types_mod.Var;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;

/// One semantic step from a type to one of its components. `data` is a
/// positional index, or the row label's `Ident.Idx` bits for `record_field`
/// and `tag_payload_tag`. Labels (not positions) address rows because row
/// order differs between checked and monomorphic types.
pub const PathStep = extern struct {
    kind: u32,
    data: u32,

    pub const Kind = enum(u32) {
        fn_arg = 0,
        fn_ret = 1,
        alias_arg = 2,
        alias_backing = 3,
        nominal_arg = 4,
        nominal_backing = 5,
        tuple_elem = 6,
        record_field = 7,
        // 8 was the checked-store `record_ext` step. Durable paths normalize
        // row-extension topology away, so the value remains retired.
        tag_payload_tag = 9,
        tag_payload_index = 10,
        // 11 was the checked-store `tag_ext` step and remains retired.
    };

    /// Decode a serialized path kind without trapping on a retired or corrupt
    /// discriminant. Artifact boundary validation uses this before consumers
    /// call `stepKind`.
    pub fn kindOrNull(self: PathStep) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn stepKind(self: PathStep) Kind {
        return self.kindOrNull() orelse unreachable;
    }
};

/// One (constrained scheme var, constraint) pair, at its canonical index.
const ConstraintCallableSource = struct {
    intro_expr: ?u32,
    callable_var: Var,
};

/// One canonical procedure evidence parameter and its producer-authored
/// dispatcher source.
pub const EvidenceParam = struct {
    /// Resolved root of the constrained scheme var.
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    /// Independent relations sharing this target, in constraint order.
    callable_contracts: []const StaticDispatchConstraint = &.{},
    contract_start: u32 = 0,
    contract_len: u32 = 0,
    /// Producer-authored origin from which a specialization must obtain the
    /// dispatcher's concrete type. `path` is relative to this source.
    source: Source,
    /// Producer-authored scheme-only codec; no uninstantiated root contract.
    requires_instantiation: bool = false,
    /// Assigned once when the owner schema is frozen into the checked pool.
    published_index: ?u32 = null,
    /// Last of the semantic steps from the scheme root to the dispatcher's
    /// first occurrence, in `Scratch.paramPathNodes()`, or `no_path_node`.
    /// Empty when no path over the normalized callable exists: dispatchers
    /// reachable only through a constraint's fn type, and open-row remainder
    /// vars erased when the row closes. Valid until the next
    /// `enumerateEvidenceParams` call with the same scratch.
    path_node: u32 = no_path_node,
    path_len: u32 = 0,

    pub const Source = union(enum) {
        scheme_callable,
        /// Exact checked expression whose dispatch constraint introduced the
        /// callable containing this parameter.
        constraint_callable: ConstraintCallableSource,
        erased_row_remainder,
        /// An explicit composite codec relation owned by the binding scheme.
        scheme_requirement,
    };
};

const WalkSource = union(enum) {
    scheme_callable,
    constraint_callable: ConstraintCallableSource,
};

const QueuedCallable = struct {
    var_: Var,
    intro_expr: ?u32,
};

const StackEntry = struct {
    var_: Var,
    /// Last step of this entry's path in `Scratch.path_nodes`, or
    /// `no_path_node` for the walk root.
    path_node: u32,
    path_len: u32,
    /// A row continuation is checked-store traversal state, not a semantic
    /// path component. Its fields/tags extend the logical row at `path`.
    row_context: RowContext = .none,
};

const RowContext = enum { none, record, tag };

pub const no_path_node = std.math.maxInt(u32);

/// One step of a path. Paths share their prefixes through `parent` (another
/// node of the same pool, or `no_path_node` for a first step), so a scheme's
/// paths cost one node per distinct prefix however deep its type nests.
pub const PathNode = extern struct {
    parent: u32,
    step: PathStep,
};

/// Append the `len` steps of the path ending at `last` to `out`, root first.
pub fn appendPathSteps(
    gpa: Allocator,
    nodes: []const PathNode,
    last: u32,
    len: u32,
    out: *std.ArrayListUnmanaged(PathStep),
) Allocator.Error!void {
    const steps = try out.addManyAsSlice(gpa, len);
    var node = last;
    var i = len;
    while (i > 0) {
        i -= 1;
        steps[i] = nodes[node].step;
        node = nodes[node].parent;
    }
    std.debug.assert(node == no_path_node);
}

/// Values of path nodes, each computed once per root from its parent's value:
/// a scheme whose paths share prefixes resolves in time proportional to its
/// distinct prefixes rather than to the sum of its path lengths. `ctx.apply(value,
/// nodes, node)` maps the value at `node`'s parent (or the root) through
/// `node`'s step, returning null when the step does not apply. A
/// `tag_payload_tag` node's value is its tag union; the `tag_payload_index`
/// node after it selects the payload using its parent's tag label.
pub fn PathMemo(comptime T: type) type {
    return struct {
        const Self = @This();
        const Key = struct { root: u32, node: u32 };

        values: std.AutoHashMapUnmanaged(Key, T) = .{},
        chain: std.ArrayListUnmanaged(u32) = .empty,

        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.values.deinit(gpa);
            self.chain.deinit(gpa);
            self.* = .{};
        }

        /// The value at `last`, with `root` as the value before a first step.
        /// `root_key` identifies `root` among the roots this memo resolves
        /// from. `nodes` must link every parent before its children.
        pub fn resolve(
            self: *Self,
            gpa: Allocator,
            nodes: []const PathNode,
            last: u32,
            root_key: u32,
            root: T,
            ctx: anytype,
        ) Allocator.Error!?T {
            self.chain.clearRetainingCapacity();
            var value = root;
            var node = last;
            while (node != no_path_node) {
                if (self.values.get(.{ .root = root_key, .node = node })) |known| {
                    value = known;
                    break;
                }
                try self.chain.append(gpa, node);
                node = nodes[node].parent;
            }
            var i = self.chain.items.len;
            while (i > 0) {
                i -= 1;
                const next = self.chain.items[i];
                value = (try ctx.apply(value, nodes, next)) orelse return null;
                try self.values.put(gpa, .{ .root = root_key, .node = next }, value);
            }
            return value;
        }
    };
}

/// Reusable scratch state for `enumerateEvidenceParams`.
pub const Scratch = struct {
    visited: std.AutoHashMapUnmanaged(Var, void) = .{},
    stack: std.ArrayListUnmanaged(StackEntry) = .empty,
    fn_var_queue: std.ArrayListUnmanaged(QueuedCallable) = .empty,
    /// Runtime evidence selects a method target, not one particular
    /// instantiation of that target's callable scheme. Independent same-name
    /// calls therefore share this receiver-local slot while retaining their
    /// own callable relations in the checked plans.
    emitted_methods: std.AutoHashMapUnmanaged(Ident.Idx, u32) = .{},
    contract_classes: std.AutoHashMapUnmanaged(struct { param: u32, callable: Var }, void) = .{},
    contract_entries: std.ArrayListUnmanaged(struct { param: u32, constraint: StaticDispatchConstraint }) = .empty,
    contract_pool: std.ArrayListUnmanaged(StaticDispatchConstraint) = .empty,
    /// Prefix-shared paths of every stack entry, linked toward the walk root.
    path_nodes: std.ArrayListUnmanaged(PathNode) = .empty,
    /// The nodes of emitted params' paths, parents before children.
    param_path_nodes: std.ArrayListUnmanaged(PathNode) = .empty,
    /// `path_nodes` index -> `param_path_nodes` index, or `no_path_node`.
    path_node_remap: std.ArrayListUnmanaged(u32) = .empty,
    path_chain: std.ArrayListUnmanaged(u32) = .empty,
    /// Child collection buffer for one node's children, in declared order.
    children: std.ArrayListUnmanaged(Child) = .empty,

    const Child = struct {
        var_: Var,
        steps: [2]PathStep = undefined,
        step_len: u32,
        row_context: RowContext = .none,
    };

    pub fn deinit(self: *Scratch, gpa: Allocator) void {
        self.visited.deinit(gpa);
        self.stack.deinit(gpa);
        self.fn_var_queue.deinit(gpa);
        self.emitted_methods.deinit(gpa);
        self.contract_classes.deinit(gpa);
        self.contract_entries.deinit(gpa);
        self.contract_pool.deinit(gpa);
        self.path_nodes.deinit(gpa);
        self.param_path_nodes.deinit(gpa);
        self.path_node_remap.deinit(gpa);
        self.path_chain.deinit(gpa);
        self.children.deinit(gpa);
        self.* = .{};
    }

    fn clear(self: *Scratch) void {
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        self.fn_var_queue.clearRetainingCapacity();
        self.emitted_methods.clearRetainingCapacity();
        self.contract_classes.clearRetainingCapacity();
        self.contract_entries.clearRetainingCapacity();
        self.contract_pool.clearRetainingCapacity();
        self.path_nodes.clearRetainingCapacity();
        self.param_path_nodes.clearRetainingCapacity();
        self.path_node_remap.clearRetainingCapacity();
        self.path_chain.clearRetainingCapacity();
        self.children.clearRetainingCapacity();
    }

    /// The path nodes that the last enumeration's params refer to.
    pub fn paramPathNodes(self: *const Scratch) []const PathNode {
        return self.param_path_nodes.items;
    }

    fn appendPathNode(self: *Scratch, gpa: Allocator, parent: u32, path_step: PathStep) Allocator.Error!u32 {
        const node: u32 = @intCast(self.path_nodes.items.len);
        try self.path_nodes.append(gpa, .{ .parent = parent, .step = path_step });
        return node;
    }

    /// Move `walk_node`'s path into `param_path_nodes`, sharing the prefixes
    /// already moved there, and return its index in that pool.
    fn publishPathNode(self: *Scratch, gpa: Allocator, walk_node: u32) Allocator.Error!u32 {
        if (walk_node == no_path_node) return no_path_node;
        self.path_chain.clearRetainingCapacity();
        var node = walk_node;
        while (node != no_path_node and self.path_node_remap.items[node] == no_path_node) {
            try self.path_chain.append(gpa, node);
            node = self.path_nodes.items[node].parent;
        }
        var parent = if (node == no_path_node) no_path_node else self.path_node_remap.items[node];
        var i = self.path_chain.items.len;
        while (i > 0) {
            i -= 1;
            const next = self.path_chain.items[i];
            const published: u32 = @intCast(self.param_path_nodes.items.len);
            try self.param_path_nodes.append(gpa, .{ .parent = parent, .step = self.path_nodes.items[next].step });
            self.path_node_remap.items[next] = published;
            parent = published;
        }
        return self.path_node_remap.items[walk_node];
    }
};

/// Append the scheme's evidence params to `out` in canonical order.
pub fn enumerateEvidenceParams(
    gpa: Allocator,
    store: *const types_mod.Store,
    root: Var,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    return enumerateEvidenceParamsWithRequirements(gpa, store, root, &.{}, scratch, out);
}

/// Captured codec requirement retaining its receiver, callable constraint, and
/// checker-authored need for instantiation before selecting a concrete codec.
pub const SchemeRequirement = struct {
    receiver: Var,
    constraint: StaticDispatchConstraint,
    requires_instantiation: bool,
};

/// Append the complete scheme contract. Explicit requirements retain producer
/// order and exact callable identity, even when their receivers share a type.
pub fn enumerateEvidenceParamsWithRequirements(
    gpa: Allocator,
    store: *const types_mod.Store,
    root: Var,
    requirements: []const SchemeRequirement,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    scratch.clear();

    const out_base = out.items.len;
    try walk(gpa, store, root, .scheme_callable, scratch, out);
    for (requirements) |requirement| {
        try out.append(gpa, .{
            .dispatcher_var = store.resolveVar(requirement.receiver).var_,
            .constraint = requirement.constraint,
            .source = .scheme_requirement,
            .requires_instantiation = requirement.requires_instantiation,
        });
        try scratch.fn_var_queue.append(gpa, .{
            .var_ = requirement.constraint.fn_var,
            .intro_expr = requirement.constraint.provenance.intro_expr.get(),
        });
    }
    // Constraint fn types can bind further constrained vars; the queue holds
    // every emitted constraint's fn var in emission order. `walk` may grow the
    // queue while we drain it—index-based drain keeps that sound. Params
    // found through the queue retain a semantic path relative to the exact
    // constraint callable which introduced them.
    var queue_index: usize = 0;
    while (queue_index < scratch.fn_var_queue.items.len) : (queue_index += 1) {
        const queued = scratch.fn_var_queue.items[queue_index];
        const source: WalkSource = .{ .constraint_callable = .{
            .intro_expr = queued.intro_expr,
            .callable_var = queued.var_,
        } };
        try walk(gpa, store, queued.var_, source, scratch, out);
    }
    // Keep only the nodes on emitted params' paths.
    try scratch.path_node_remap.appendNTimes(gpa, no_path_node, scratch.path_nodes.items.len);
    var contract_start: u32 = 0;
    for (out.items[out_base..]) |*param| {
        param.path_node = try scratch.publishPathNode(gpa, param.path_node);
        param.contract_start = contract_start;
        contract_start += param.contract_len;
        param.contract_len = 0;
    }
    try scratch.contract_pool.resize(gpa, contract_start);
    for (scratch.contract_entries.items) |contract| {
        const param = &out.items[contract.param];
        scratch.contract_pool.items[param.contract_start + param.contract_len] = contract.constraint;
        param.contract_len += 1;
    }
    for (out.items[out_base..]) |*param| {
        param.callable_contracts = scratch.contract_pool.items[param.contract_start..][0..param.contract_len];
    }
}

fn walk(
    gpa: Allocator,
    store: *const types_mod.Store,
    walk_root: Var,
    source: WalkSource,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    const stack_base = scratch.stack.items.len;
    try scratch.stack.append(gpa, .{ .var_ = walk_root, .path_node = no_path_node, .path_len = 0 });

    while (scratch.stack.items.len > stack_base) {
        const entry = scratch.stack.pop().?;
        const resolved = store.resolveVar(entry.var_);
        const seen = try scratch.visited.getOrPut(gpa, resolved.var_);
        if (seen.found_existing) continue;

        if (entry.row_context != .none) {
            try walkRowContinuation(gpa, store, resolved, entry, scratch, out);
            continue;
        }

        switch (resolved.desc.content) {
            .flex => |flex| try emitConstraints(gpa, store, resolved.var_, flex.constraints, entry, source, false, scratch, out),
            .rigid => |rigid| try emitConstraints(gpa, store, resolved.var_, rigid.constraints, entry, source, false, scratch, out),
            .alias => |alias| {
                scratch.children.clearRetainingCapacity();
                for (store.sliceAliasArgs(alias), 0..) |arg, i| {
                    try scratch.children.append(gpa, child(arg, step(.alias_arg, @intCast(i))));
                }
                try scratch.children.append(gpa, child(store.getAliasBackingVar(alias), step(.alias_backing, 0)));
                try pushChildren(gpa, scratch, entry);
            },
            .structure => |flat_type| switch (flat_type) {
                .record => |record| try pushRecordChildren(gpa, scratch, entry, store, record.fields, record.ext),
                .tuple => |tuple| {
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceVars(tuple.elems), 0..) |elem, i| {
                        try scratch.children.append(gpa, child(elem, step(.tuple_elem, @intCast(i))));
                    }
                    try pushChildren(gpa, scratch, entry);
                },
                .nominal_type => |nominal| {
                    // A nominal application's structure is its args; backing
                    // structure is declaration data and is not part of the
                    // scheme's type graph, so this enumerator never emits a
                    // `.nominal_backing` step (consumers treat that kind
                    // exactly like `.alias_backing`).
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceNominalArgs(nominal), 0..) |arg, i| {
                        try scratch.children.append(gpa, child(arg, step(.nominal_arg, @intCast(i))));
                    }
                    try pushChildren(gpa, scratch, entry);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceVars(func.args), 0..) |arg, i| {
                        try scratch.children.append(gpa, child(arg, step(.fn_arg, @intCast(i))));
                    }
                    try scratch.children.append(gpa, child(func.ret, step(.fn_ret, 0)));
                    try pushChildren(gpa, scratch, entry);
                },
                .tag_union => |tag_union| try pushTagChildren(gpa, scratch, entry, store, tag_union),
                .empty_record, .empty_tag_union => {},
            },
            // A presence variable carries no static-dispatch constraints and has
            // no children; it is a leaf like `.err`.
            .field_presence => {},
            .err => {},
        }
    }
}

/// Traverse a checked row tail while keeping the path anchored at the logical
/// row root. Transparent aliases and extension links are producer topology;
/// neither is present after Monotype flattens the row.
fn walkRowContinuation(
    gpa: Allocator,
    store: *const types_mod.Store,
    resolved: types_mod.ResolvedVarDesc,
    entry: StackEntry,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    switch (resolved.desc.content) {
        // An open row remainder is not a standalone component of the closed,
        // flattened monomorphic row. Keep its obligation in canonical order,
        // but publish it pathless rather than misidentifying the whole row as
        // its dispatcher.
        .flex => |flex| try emitConstraints(gpa, store, resolved.var_, flex.constraints, entry, .scheme_callable, true, scratch, out),
        .rigid => |rigid| try emitConstraints(gpa, store, resolved.var_, rigid.constraints, entry, .scheme_callable, true, scratch, out),
        .alias => |alias| {
            scratch.children.clearRetainingCapacity();
            try scratch.children.append(gpa, rowContinuation(store.getAliasBackingVar(alias), entry.row_context));
            try pushChildren(gpa, scratch, entry);
        },
        .structure => |flat_type| switch (entry.row_context) {
            .record => switch (flat_type) {
                .record => |record| try pushRecordChildren(gpa, scratch, entry, store, record.fields, record.ext),
                .empty_record => {},
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .tag_union,
                .empty_tag_union,
                => unreachable,
            },
            .tag => switch (flat_type) {
                .tag_union => |tag_union| try pushTagChildren(gpa, scratch, entry, store, tag_union),
                .empty_tag_union => {},
                .record,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                => unreachable,
            },
            .none => unreachable,
        },
        // A presence variable carries no static-dispatch constraints and has no
        // children; it is a leaf like `.err`.
        .field_presence => {},
        .err => {},
    }
}

fn step(kind: PathStep.Kind, data: u32) PathStep {
    return .{ .kind = @intFromEnum(kind), .data = data };
}

fn child(var_: Var, path_step: PathStep) Scratch.Child {
    return .{ .var_ = var_, .steps = .{ path_step, undefined }, .step_len = 1 };
}

fn tagPayloadChild(var_: Var, tag_name_bits: u32, payload_index: u32) Scratch.Child {
    return .{
        .var_ = var_,
        .steps = .{
            step(.tag_payload_tag, tag_name_bits),
            step(.tag_payload_index, payload_index),
        },
        .step_len = 2,
    };
}

fn rowContinuation(var_: Var, context: RowContext) Scratch.Child {
    std.debug.assert(context != .none);
    return .{ .var_ = var_, .step_len = 0, .row_context = context };
}

/// Push the collected children so pops visit them in declared order, each
/// with `entry`'s path extended by its semantic steps. A zero-step child is a
/// checked-store row continuation and retains the logical row-root path.
fn pushChildren(gpa: Allocator, scratch: *Scratch, entry: StackEntry) Allocator.Error!void {
    var i = scratch.children.items.len;
    while (i > 0) {
        i -= 1;
        const next_child = scratch.children.items[i];
        if (next_child.step_len == 0) {
            try scratch.stack.append(gpa, .{
                .var_ = next_child.var_,
                .path_node = entry.path_node,
                .path_len = entry.path_len,
                .row_context = next_child.row_context,
            });
            continue;
        }
        var path_node = entry.path_node;
        for (next_child.steps[0..next_child.step_len]) |path_step| {
            path_node = try scratch.appendPathNode(gpa, path_node, path_step);
        }
        try scratch.stack.append(gpa, .{
            .var_ = next_child.var_,
            .path_node = path_node,
            .path_len = entry.path_len + next_child.step_len,
            .row_context = next_child.row_context,
        });
    }
}

/// Collect one child per record field, addressed by label.
///
/// Only the field's VALUE-axis var is walked. A field with a dynamic kind also
/// carries a presence-axis var, but a
/// presence var is an evidence leaf exactly like the `.field_presence` arms
/// above: static-dispatch constraints attach to expression receivers, a
/// presence var is never addressable from an expression, and unification only
/// ever pairs it with other presence-axis content—`unify.zig`'s
/// `unifyFlex`/`unifyFieldPresence` assert that a flex meeting a presence fact
/// carries zero constraints (and still `recordDeferredConstraint` it), so no
/// constraint can hide behind a presence var.
///
/// `.unknown` fields are minted for literals, `.?` accesses, `?:`
/// annotations, and the record-update probe in `Check.zig`, which carries one
/// kind-flexible `.unknown` field per mentioned update field (creation
/// semantics: the base's kind decides).
fn collectRecordFieldChildren(
    gpa: Allocator,
    scratch: *Scratch,
    store: *const types_mod.Store,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
) Allocator.Error!void {
    scratch.children.clearRetainingCapacity();
    const fields = store.getRecordFieldsSlice(fields_range);
    for (fields.items(.name), fields.items(.presence)) |name, presence| {
        try scratch.children.append(gpa, child(presence.typeVar(), step(.record_field, @bitCast(name))));
    }
}

fn pushRecordChildren(
    gpa: Allocator,
    scratch: *Scratch,
    entry: StackEntry,
    store: *const types_mod.Store,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
    ext: Var,
) Allocator.Error!void {
    try collectRecordFieldChildren(gpa, scratch, store, fields_range);
    try scratch.children.append(gpa, rowContinuation(ext, .record));
    try pushChildren(gpa, scratch, entry);
}

/// Each payload gets a (tag label, payload index) semantic step pair. The
/// extension is scheduled last as zero-step producer traversal state.
fn pushTagChildren(
    gpa: Allocator,
    scratch: *Scratch,
    entry: StackEntry,
    store: *const types_mod.Store,
    tag_union: types_mod.TagUnion,
) Allocator.Error!void {
    scratch.children.clearRetainingCapacity();
    const tags = store.getTagsSlice(tag_union.tags);
    for (tags.items(.name), tags.items(.args)) |tag_name, args| {
        for (store.sliceVars(args), 0..) |arg, payload_index| {
            try scratch.children.append(gpa, tagPayloadChild(arg, @bitCast(tag_name), @intCast(payload_index)));
        }
    }
    try scratch.children.append(gpa, rowContinuation(tag_union.ext, .tag));
    try pushChildren(gpa, scratch, entry);
}

fn emitConstraints(
    gpa: Allocator,
    store: *const types_mod.Store,
    dispatcher_root: Var,
    constraints: StaticDispatchConstraint.SafeList.Range,
    entry: StackEntry,
    source: WalkSource,
    erased_row_remainder: bool,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    scratch.emitted_methods.clearRetainingCapacity();
    for (store.sliceStaticDispatchConstraints(constraints)) |constraint| {
        const emitted = try scratch.emitted_methods.getOrPut(gpa, constraint.fn_name);
        if (!emitted.found_existing) {
            emitted.value_ptr.* = @intCast(out.items.len);
            try out.append(gpa, .{
                .dispatcher_var = dispatcher_root,
                .constraint = constraint,
                .source = if (erased_row_remainder) .erased_row_remainder else switch (source) {
                    .scheme_callable => .scheme_callable,
                    .constraint_callable => |constraint_callable| .{ .constraint_callable = constraint_callable },
                },
                .path_node = entry.path_node,
                .path_len = entry.path_len,
            });
        } else {
            const param_index = emitted.value_ptr.*;
            const param = &out.items[param_index];
            const callable = store.resolveVar(constraint.fn_var).var_;
            if (callable != store.resolveVar(param.constraint.fn_var).var_) {
                const distinct = try scratch.contract_classes.getOrPut(gpa, .{ .param = param_index, .callable = callable });
                if (!distinct.found_existing) {
                    try scratch.contract_entries.append(gpa, .{ .param = param_index, .constraint = constraint });
                    param.contract_len += 1;
                }
            }
        }
        // A shared target does not make the callables interchangeable: each
        // one can expose further independently constrained variables.
        try scratch.fn_var_queue.append(gpa, .{
            .var_ = constraint.fn_var,
            .intro_expr = constraint.provenance.intro_expr.get(),
        });
    }
}
