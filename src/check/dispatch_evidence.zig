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
//! Order contract: depth-first over the resolved type structure — function
//! args then return, ordinary alias/nominal args then backing, and logical row
//! fields/tags across the complete extension chain, all in store order —
//! emitting each constrained var's constraints in range order at its first
//! occurrence; then the collected
//! constraints' fn types are walked the same way in emission order (they can
//! bind further constrained vars, e.g. `where [a.iter : a -> i, i.next : ..]`).
//!
//! Each param also carries the semantic PATH from the scheme root to its
//! dispatcher's first occurrence (function arg positions, type arguments,
//! row labels, …). Row-extension storage chains, including transparent aliases
//! within them, are normalized away: a field or tag payload in any tail is
//! addressed directly from the logical row root. Compiler-generated call edges — structural-derivation
//! component calls, builtin helper calls — have no checked instantiation
//! records, so monotype resolves a target's obligations by walking these
//! paths over the concrete monomorphic callable instead.

const std = @import("std");
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
        return switch (self.kind) {
            @intFromEnum(Kind.fn_arg) => .fn_arg,
            @intFromEnum(Kind.fn_ret) => .fn_ret,
            @intFromEnum(Kind.alias_arg) => .alias_arg,
            @intFromEnum(Kind.alias_backing) => .alias_backing,
            @intFromEnum(Kind.nominal_arg) => .nominal_arg,
            @intFromEnum(Kind.nominal_backing) => .nominal_backing,
            @intFromEnum(Kind.tuple_elem) => .tuple_elem,
            @intFromEnum(Kind.record_field) => .record_field,
            @intFromEnum(Kind.tag_payload_tag) => .tag_payload_tag,
            @intFromEnum(Kind.tag_payload_index) => .tag_payload_index,
            else => null,
        };
    }

    pub fn stepKind(self: PathStep) Kind {
        return self.kindOrNull() orelse unreachable;
    }
};

/// One (constrained scheme var, constraint) pair, at its canonical index.
pub const EvidenceParam = struct {
    /// Resolved root of the constrained scheme var.
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    /// Semantic steps from the scheme root to the dispatcher's first
    /// occurrence. Empty when no path over the normalized callable exists:
    /// dispatchers reachable only through a constraint's fn type, and open-row
    /// remainder vars erased when the row closes. Aliases the scratch path
    /// pool: valid until the next `enumerateEvidenceParams` call with the same
    /// scratch.
    path: []const PathStep = &.{},
    /// Pool offsets backing `path`; fixed up into the slice once the walk's
    /// pool stops growing.
    path_start: u32 = 0,
    path_len: u32 = 0,
};

const StackEntry = struct {
    var_: Var,
    path_start: u32,
    path_len: u32,
    /// A row continuation is checked-store traversal state, not a semantic
    /// path component. Its fields/tags extend the logical row at `path`.
    row_context: RowContext = .none,
};

const RowContext = enum { none, record, tag };

/// Reusable scratch state for `enumerateEvidenceParams`.
pub const Scratch = struct {
    visited: std.AutoHashMapUnmanaged(Var, void) = .{},
    stack: std.ArrayListUnmanaged(StackEntry) = .empty,
    fn_var_queue: std.ArrayListUnmanaged(Var) = .empty,
    /// Flat pool backing every stack entry's (and emitted param's) path.
    path_pool: std.ArrayListUnmanaged(PathStep) = .empty,
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
        self.path_pool.deinit(gpa);
        self.children.deinit(gpa);
        self.* = .{};
    }

    fn clear(self: *Scratch) void {
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        self.fn_var_queue.clearRetainingCapacity();
        self.path_pool.clearRetainingCapacity();
        self.children.clearRetainingCapacity();
    }

    fn pathSlice(self: *const Scratch, start: u32, len: u32) []const PathStep {
        return self.path_pool.items[start .. start + len];
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
    scratch.clear();

    const out_base = out.items.len;
    try walk(gpa, store, root, true, scratch, out);
    // Constraint fn types can bind further constrained vars; the queue holds
    // every emitted constraint's fn var in emission order. `walk` may grow the
    // queue while we drain it — index-based drain keeps that sound. Params
    // found through the queue are pathless: no path over the scheme's
    // callable reaches them.
    var queue_index: usize = 0;
    while (queue_index < scratch.fn_var_queue.items.len) : (queue_index += 1) {
        try walk(gpa, store, scratch.fn_var_queue.items[queue_index], false, scratch, out);
    }
    // The pool has stopped growing: materialize each param's path slice (the
    // walk records offsets because interim appends may reallocate the pool).
    for (out.items[out_base..]) |*param| {
        param.path = scratch.pathSlice(param.path_start, param.path_len);
    }
}

fn walk(
    gpa: Allocator,
    store: *const types_mod.Store,
    walk_root: Var,
    with_paths: bool,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    const stack_base = scratch.stack.items.len;
    try scratch.stack.append(gpa, .{ .var_ = walk_root, .path_start = 0, .path_len = 0 });

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
            .flex => |flex| try emitConstraints(gpa, store, resolved.var_, flex.constraints, entry, with_paths, scratch, out),
            .rigid => |rigid| try emitConstraints(gpa, store, resolved.var_, rigid.constraints, entry, with_paths, scratch, out),
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
                .record_unbound => |fields_range| {
                    scratch.children.clearRetainingCapacity();
                    const fields = store.getRecordFieldsSlice(fields_range);
                    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
                        try scratch.children.append(gpa, child(field_var, step(.record_field, @bitCast(name))));
                    }
                    try pushChildren(gpa, scratch, entry);
                },
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
        .flex => |flex| try emitConstraints(gpa, store, resolved.var_, flex.constraints, entry, false, scratch, out),
        .rigid => |rigid| try emitConstraints(gpa, store, resolved.var_, rigid.constraints, entry, false, scratch, out),
        .alias => |alias| {
            scratch.children.clearRetainingCapacity();
            try scratch.children.append(gpa, rowContinuation(store.getAliasBackingVar(alias), entry.row_context));
            try pushChildren(gpa, scratch, entry);
        },
        .structure => |flat_type| switch (entry.row_context) {
            .record => switch (flat_type) {
                .record => |record| try pushRecordChildren(gpa, scratch, entry, store, record.fields, record.ext),
                .record_unbound => |fields_range| {
                    scratch.children.clearRetainingCapacity();
                    const fields = store.getRecordFieldsSlice(fields_range);
                    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
                        try scratch.children.append(gpa, child(field_var, step(.record_field, @bitCast(name))));
                    }
                    try pushChildren(gpa, scratch, entry);
                },
                .empty_record => {},
                else => unreachable,
            },
            .tag => switch (flat_type) {
                .tag_union => |tag_union| try pushTagChildren(gpa, scratch, entry, store, tag_union),
                .empty_tag_union => {},
                else => unreachable,
            },
            .none => unreachable,
        },
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
                .path_start = entry.path_start,
                .path_len = entry.path_len,
                .row_context = next_child.row_context,
            });
            continue;
        }
        const path_start: u32 = @intCast(scratch.path_pool.items.len);
        // Reserve before self-append: the source range aliases the pool.
        try scratch.path_pool.ensureUnusedCapacity(gpa, entry.path_len + next_child.step_len);
        scratch.path_pool.appendSliceAssumeCapacity(scratch.pathSlice(entry.path_start, entry.path_len));
        scratch.path_pool.appendSliceAssumeCapacity(next_child.steps[0..next_child.step_len]);
        try scratch.stack.append(gpa, .{
            .var_ = next_child.var_,
            .path_start = path_start,
            .path_len = entry.path_len + next_child.step_len,
            .row_context = next_child.row_context,
        });
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
    scratch.children.clearRetainingCapacity();
    const fields = store.getRecordFieldsSlice(fields_range);
    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
        try scratch.children.append(gpa, child(field_var, step(.record_field, @bitCast(name))));
    }
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
    with_paths: bool,
    scratch: *Scratch,
    out: *std.ArrayListUnmanaged(EvidenceParam),
) Allocator.Error!void {
    for (store.sliceStaticDispatchConstraints(constraints)) |constraint| {
        try out.append(gpa, .{
            .dispatcher_var = dispatcher_root,
            .constraint = constraint,
            .path_start = if (with_paths) entry.path_start else 0,
            .path_len = if (with_paths) entry.path_len else 0,
        });
        try scratch.fn_var_queue.append(gpa, constraint.fn_var);
    }
}
