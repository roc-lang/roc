//! Canonical enumeration of a type scheme's static-dispatch constraints
//! ("evidence params").
//!
//! Every scheme with dispatch constraints gets one ordered param list: index
//! `k` in that list is the identity a dispatch plan's `constraint(k)`
//! resolution and a call edge's k-th evidence entry both refer to. The order
//! is defined purely by the scheme's type structure, so the definition's own
//! module and a caller holding a structural copy of the scheme (an import
//! copy, or the pristine root recorded by a `SchemeInstantiationRecord`)
//! enumerate identical lists without sharing var identities.
//!
//! Order contract: depth-first over the resolved type structure — function
//! args then return, alias/nominal args then backing, row fields/tags then
//! extension, all in store order — emitting each constrained var's
//! constraints in range order at its first occurrence; then the collected
//! constraints' fn types are walked the same way in emission order (they can
//! bind further constrained vars, e.g. `where [a.iter : a -> i, i.next : ..]`).
//!
//! Each param also carries the semantic PATH from the scheme root to its
//! dispatcher's first occurrence (function arg positions, type arguments,
//! row labels, …). Compiler-generated call edges — structural-derivation
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
        fn_arg,
        fn_ret,
        alias_arg,
        alias_backing,
        nominal_arg,
        nominal_backing,
        tuple_elem,
        record_field,
        record_ext,
        tag_payload_tag,
        tag_payload_index,
        tag_ext,
    };

    pub fn stepKind(self: PathStep) Kind {
        return @enumFromInt(self.kind);
    }
};

/// One (constrained scheme var, constraint) pair, at its canonical index.
pub const EvidenceParam = struct {
    /// Resolved root of the constrained scheme var.
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    /// Semantic steps from the scheme root to the dispatcher's first
    /// occurrence. Empty for dispatchers reachable only through a
    /// constraint's fn type (no path over the callable exists). Aliases the
    /// scratch path pool: valid until the next `enumerateEvidenceParams`
    /// call with the same scratch.
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
};

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
        step: PathStep,
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

/// The canonical index of `dispatcher_root`'s `fn_name` constraint within
/// `params`, or null.
pub fn paramIndex(
    params: []const EvidenceParam,
    dispatcher_root: Var,
    fn_name: anytype,
) ?u32 {
    for (params, 0..) |param, k| {
        if (param.dispatcher_var == dispatcher_root and param.constraint.fn_name.eql(fn_name)) {
            return @intCast(k);
        }
    }
    return null;
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

        switch (resolved.desc.content) {
            .flex => |flex| try emitConstraints(gpa, store, resolved.var_, flex.constraints, entry, with_paths, scratch, out),
            .rigid => |rigid| try emitConstraints(gpa, store, resolved.var_, rigid.constraints, entry, with_paths, scratch, out),
            .alias => |alias| {
                scratch.children.clearRetainingCapacity();
                for (store.sliceAliasArgs(alias), 0..) |arg, i| {
                    try scratch.children.append(gpa, .{ .var_ = arg, .step = step(.alias_arg, @intCast(i)) });
                }
                try scratch.children.append(gpa, .{ .var_ = store.getAliasBackingVar(alias), .step = step(.alias_backing, 0) });
                try pushChildren(gpa, scratch, entry);
            },
            .structure => |flat_type| switch (flat_type) {
                .record => |record| {
                    scratch.children.clearRetainingCapacity();
                    const fields = store.getRecordFieldsSlice(record.fields);
                    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
                        try scratch.children.append(gpa, .{ .var_ = field_var, .step = step(.record_field, @bitCast(name)) });
                    }
                    try scratch.children.append(gpa, .{ .var_ = record.ext, .step = step(.record_ext, 0) });
                    try pushChildren(gpa, scratch, entry);
                },
                .record_unbound => |fields_range| {
                    scratch.children.clearRetainingCapacity();
                    const fields = store.getRecordFieldsSlice(fields_range);
                    for (fields.items(.name), fields.items(.var_)) |name, field_var| {
                        try scratch.children.append(gpa, .{ .var_ = field_var, .step = step(.record_field, @bitCast(name)) });
                    }
                    try pushChildren(gpa, scratch, entry);
                },
                .tuple => |tuple| {
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceVars(tuple.elems), 0..) |elem, i| {
                        try scratch.children.append(gpa, .{ .var_ = elem, .step = step(.tuple_elem, @intCast(i)) });
                    }
                    try pushChildren(gpa, scratch, entry);
                },
                .nominal_type => |nominal| {
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceNominalArgs(nominal), 0..) |arg, i| {
                        try scratch.children.append(gpa, .{ .var_ = arg, .step = step(.nominal_arg, @intCast(i)) });
                    }
                    try scratch.children.append(gpa, .{ .var_ = store.getNominalBackingVar(nominal), .step = step(.nominal_backing, 0) });
                    try pushChildren(gpa, scratch, entry);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    scratch.children.clearRetainingCapacity();
                    for (store.sliceVars(func.args), 0..) |arg, i| {
                        try scratch.children.append(gpa, .{ .var_ = arg, .step = step(.fn_arg, @intCast(i)) });
                    }
                    try scratch.children.append(gpa, .{ .var_ = func.ret, .step = step(.fn_ret, 0) });
                    try pushChildren(gpa, scratch, entry);
                },
                .tag_union => |tag_union| try pushChildrenTagged(gpa, scratch, entry, store, tag_union),
                .empty_record, .empty_tag_union => {},
            },
            .err => {},
        }
    }
}

fn step(kind: PathStep.Kind, data: u32) PathStep {
    return .{ .kind = @intFromEnum(kind), .data = data };
}

/// Push the collected children so pops visit them in declared order, each
/// with `entry`'s path extended by its own step.
fn pushChildren(gpa: Allocator, scratch: *Scratch, entry: StackEntry) Allocator.Error!void {
    var i = scratch.children.items.len;
    while (i > 0) {
        i -= 1;
        const child = scratch.children.items[i];
        const path_start: u32 = @intCast(scratch.path_pool.items.len);
        // Reserve before self-append: the source range aliases the pool.
        try scratch.path_pool.ensureUnusedCapacity(gpa, entry.path_len + 1);
        scratch.path_pool.appendSliceAssumeCapacity(scratch.pathSlice(entry.path_start, entry.path_len));
        scratch.path_pool.appendAssumeCapacity(child.step);
        try scratch.stack.append(gpa, .{
            .var_ = child.var_,
            .path_start = path_start,
            .path_len = entry.path_len + 1,
        });
    }
}

/// Tag-union variant of `pushChildren`: each payload gets a
/// (tag label, payload index) step pair.
fn pushChildrenTagged(
    gpa: Allocator,
    scratch: *Scratch,
    entry: StackEntry,
    store: *const types_mod.Store,
    tag_union: types_mod.TagUnion,
) Allocator.Error!void {
    const tags = store.getTagsSlice(tag_union.tags);
    // Push ext first so it pops last.
    {
        const path_start: u32 = @intCast(scratch.path_pool.items.len);
        try scratch.path_pool.ensureUnusedCapacity(gpa, entry.path_len + 1);
        scratch.path_pool.appendSliceAssumeCapacity(scratch.pathSlice(entry.path_start, entry.path_len));
        scratch.path_pool.appendAssumeCapacity(step(.tag_ext, 0));
        try scratch.stack.append(gpa, .{ .var_ = tag_union.ext, .path_start = path_start, .path_len = entry.path_len + 1 });
    }
    var i: usize = tags.len;
    while (i > 0) {
        i -= 1;
        const tag_name = tags.items(.name)[i];
        const tag_args = store.sliceVars(tags.items(.args)[i]);
        var j: usize = tag_args.len;
        while (j > 0) {
            j -= 1;
            const path_start: u32 = @intCast(scratch.path_pool.items.len);
            try scratch.path_pool.ensureUnusedCapacity(gpa, entry.path_len + 2);
            scratch.path_pool.appendSliceAssumeCapacity(scratch.pathSlice(entry.path_start, entry.path_len));
            scratch.path_pool.appendAssumeCapacity(step(.tag_payload_tag, @bitCast(tag_name)));
            scratch.path_pool.appendAssumeCapacity(step(.tag_payload_index, @intCast(j)));
            try scratch.stack.append(gpa, .{ .var_ = tag_args[j], .path_start = path_start, .path_len = entry.path_len + 2 });
        }
    }
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
