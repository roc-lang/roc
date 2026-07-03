//! This module implements Hindley-Milner style type unification with extensions for:
//!
//! * flex/rigid variables
//! * type aliases
//! * tuples
//! * builtins (List, Str, Box, etc)
//! * functions
//! * extensible row-based record types
//! * extensible tag unions
//! * custom, nominal types
//! * numbers (polymorphic & compacted)
//! * effect and purity tracking
//!
//! The primary entrypoint is `unify`, which takes two type variables (`Var`) and attempts
//! to unify their structure in the given `Store`, using a mutable `Scratch` context
//! to hold intermediate buffers and allocations.
//!
//! The `Scratch` struct is designed to be reused across many unification calls.
//! It owns internal scratch buffers for record fields, shared fields, and fresh variables.
//!
//! Each call to `unify` will reset the scratch buffer. If the unification succeeds,
//! the caller can access `scratch.fresh_vars` to retrieve all type variables created
//! during that unification pass. These fresh variables are useful for later type
//! generalization, let-binding, or monomorphization.
//!
//! NOTE: Subsequent calls to `unify` will reset `fresh_vars`. It is up to the caller
//! to use/store them if necessary.
//!
//! ### Example
//!
//! ```zig
//! const result = unify(&types_store, &scratch, a_var, b_var);
//! if (result == .ok) {
//!     for (scratch.fresh_vars.items) |v| {
//!         // handle fresh type variable `v`
//!     }
//! }
//! ```
//!
//! After use, the scratch buffer should either be deinited or reused in
//! subsequent unification runs.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const problem_mod = @import("problem.zig");
const occurs = @import("occurs.zig");
const snapshot_mod = @import("snapshot.zig");

const Ident = base.Ident;
const MkSafeList = collections.SafeList;
const Allocator = std.mem.Allocator;

const ResolvedVarDesc = types_mod.ResolvedVarDesc;
const ResolvedVarDescs = types_mod.ResolvedVarDescs;

const Var = types_mod.Var;
const Rank = types_mod.Rank;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const Alias = types_mod.Alias;
const NominalType = types_mod.NominalType;
const FlatType = types_mod.FlatType;
const Tuple = types_mod.Tuple;
const Func = types_mod.Func;
const RecordField = types_mod.RecordField;
const TwoRecordFields = types_mod.TwoRecordFields;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;
const TwoTags = types_mod.TwoTags;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const TwoStaticDispatchConstraints = types_mod.TwoStaticDispatchConstraints;

const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const RecordFieldSafeList = RecordField.SafeList;
const TwoRecordFieldsSafeList = TwoRecordFields.SafeList;
const TagSafeList = Tag.SafeList;
const TagSafeMultiList = Tag.SafeMultiList;
const TwoTagsSafeList = TwoTags.SafeList;

const Problem = problem_mod.Problem;
const Context = problem_mod.Context;

const NominalDirection = enum {
    a_is_nominal,
    b_is_nominal,
};

/// The result of unification
pub const Result = union(enum) {
    const Self = @This();

    ok,
    /// A mismatch that WAS recorded as a diagnostic (the poison_to_err path).
    problem: Problem.Idx,
    /// A mismatch detected under `write_no_report`: nothing recorded, nothing
    /// poisoned. The caller decides whether/how to report it.
    mismatch,

    pub fn isOk(self: Self) bool {
        return self == .ok;
    }

    pub fn isProblem(self: Self) bool {
        switch (self) {
            .ok => return false,
            .problem, .mismatch => return true,
        }
    }
};

/// Borrowed bundle of the stable dependencies every unification needs.
/// All fields are borrowed; construct cheaply from the owner on each call.
pub const Env = struct {
    /// Allocator that owns `problems`; used to grow it. Must be the same
    /// allocator that created the problem store (see `appendProblem` below).
    problems_gpa: Allocator,
    ident_store: *const Ident.Store,
    qualified_module_ident: Ident.Idx,
    types: *types_mod.Store,
    problems: *problem_mod.Store,
    snapshots: *snapshot_mod.Store,
    type_writer: *types_mod.TypeWriter,
    unify_scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
};

/// Controls what a top-level type mismatch does to the two operands.
pub const MismatchBehavior = enum {
    /// Merge both operands into a single `.err` type. This is the default: it
    /// stops the now-erroneous vars from producing cascading downstream errors
    /// (anything unifies OK against `.err`).
    poison_to_err,
    /// Merge on success exactly like a normal unify, but on a top-level mismatch
    /// record NOTHING and poison NOTHING — return `Result.mismatch`. The caller
    /// owns the diagnostic (with correct expected/actual roles) and any
    /// rollback. Used by the branch-vs-expected check.
    write_no_report,
};

/// Per-call options. Both axes default to the common case.
pub const Options = struct {
    context: Context = .none,
    on_mismatch: MismatchBehavior = .poison_to_err,
};

/// Unify two type variables.
///
/// * Resolves type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
pub fn unify(env: *const Env, a: Var, b: Var, opts: Options) std.mem.Allocator.Error!Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First reset the scratch store
    env.unify_scratch.reset();

    // Unify
    var unifier = Unifier.init(env.ident_store, env.qualified_module_ident, env.types, env.unify_scratch, env.occurs_scratch);
    unifier.scheduleGuardedPair(a, b, .abort) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    unifier.runWorkLoop() catch |err| {
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.TypeMismatch => {},
        }

        // write_no_report: no record, no poison — the caller owns it.
        if (opts.on_mismatch == .write_no_report) return Result.mismatch;

        const expected_snapshot = try env.snapshots.snapshotVarForError(env.types, env.type_writer, a);
        const actual_snapshot = try env.snapshots.snapshotVarForError(env.types, env.type_writer, b);
        const problem_idx = try env.problems.appendProblem(env.problems_gpa, .{ .type_mismatch = .{
            .types = .{
                .expected_var = a,
                .expected_snapshot = expected_snapshot,
                .actual_var = b,
                .actual_snapshot = actual_snapshot,
            },
            .context = opts.context,
        } });
        // Only `poison_to_err` reaches here (`write_no_report` returned above).
        try env.types.union_(a, b, .{ .content = .err, .rank = Rank.generalized });
        return Result{ .problem = problem_idx };
    };

    return .ok;
}

/// A temporary unification context used to unify two type variables within a `Store`.
///
/// `Unifier` is created per unification call and:
/// * Resolves and compresses type variables
/// * Applies unification logic across all supported `Content` variants
/// * Allocates new variables into the store (via `fresh`)
/// * Writes unified structure back into the store (via `merge`)
///
/// It works in tandem with `Scratch`, which owns reusable memory buffers
/// for intermediate field lists, shared record partitions, and fresh variables.
///
/// `Unifier` supports:
/// * flexible and rigid type variables
/// * type aliases
/// * extensible records with row polymorphism
/// * basic support for function, tuple, and number types
///
/// Callers are not expected to construct `Unifier`. Instead  call `unify(...)`.
const Unifier = struct {
    const Self = @This();

    ident_store: *const Ident.Store,
    qualified_module_ident: Ident.Idx,
    types_store: *types_mod.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    /// The unresolved "actual" var before resolution, used for deferred constraint origin tracking.
    /// This allows error messages to point to the original expression rather than the resolved type.
    unresolved_b: ?Var,

    /// Init unifier
    pub fn init(
        ident_store: *const Ident.Store,
        qualified_module_ident: Ident.Idx,
        types_store: *types_mod.Store,
        scratch: *Scratch,
        occurs_scratch: *occurs.Scratch,
    ) Unifier {
        return .{
            .ident_store = ident_store,
            .qualified_module_ident = qualified_module_ident,
            .types_store = types_store,
            .scratch = scratch,
            .occurs_scratch = occurs_scratch,
            .unresolved_b = null,
        };
    }

    fn getTypeIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.ident_store.getText(idx);
    }

    // merge

    /// Link the variables & updated the content in the type_store
    /// In the old compiler, this function was called "merge"
    fn merge(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) std.mem.Allocator.Error!void {
        try self.types_store.union_(vars.a.var_, vars.b.var_, .{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
    }

    /// Create a new type variable *in this pool*
    fn fresh(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) std.mem.Allocator.Error!Var {
        const var_ = try self.types_store.register(.{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
        _ = try self.scratch.fresh_vars.append(self.scratch.gpa, var_);
        return var_;
    }

    /// Record a deferred constraint check for later verification.
    /// Always uses b's var for error regions (via unresolved_b) because b represents
    /// the "actual" type from the user's code, while a represents the "expected" type
    /// (e.g., from a function signature). When constraints aren't satisfied, we want
    /// to highlight where the user's code is, not the constraint's origin.
    /// NOTE: if flex-side dispatch constraints ever start FIRING mid-unify
    /// instead of deferring here, revisit `structurallyIncompatiblePair` (bottom
    /// of this module) — the defaulting pre-filter's soundness fence relies on
    /// this deferral.
    fn recordDeferredConstraint(
        self: *Self,
        vars: *const ResolvedVarDescs,
        constraints: StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!void {
        const dispatcher_var = self.unresolved_b orelse vars.b.var_;
        return self.recordDeferredConstraintOn(dispatcher_var, constraints);
    }

    fn recordDeferredConstraintOn(
        self: *Self,
        dispatcher_var: Var,
        constraints: StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!void {
        if (constraints.len() > 0) {
            _ = try self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                .var_ = dispatcher_var,
                .constraints = constraints,
            });
        }
    }

    /// Check if we're already unifying this pair of descriptors (recursion guard).
    /// This prevents infinite recursion on self-referential types.
    /// We check pairs because unifying (A, B) shouldn't block unifying (A, C).
    fn isPairVisited(self: *Self, a_var: Var, b_var: Var) bool {
        const a_resolved = self.types_store.resolveVar(a_var);
        const b_resolved = self.types_store.resolveVar(b_var);

        // Visited vars are stored as pairs: [a1, b1, a2, b2, ...]
        const items = self.scratch.visited_vars.items.items;
        var i: usize = 0;
        while (i + 1 < items.len) : (i += 2) {
            const visited_a = self.types_store.resolveVar(items[i]);
            const visited_b = self.types_store.resolveVar(items[i + 1]);

            // Check both orderings since unify(A,B) is symmetric with unify(B,A)
            if ((a_resolved.desc_idx == visited_a.desc_idx and b_resolved.desc_idx == visited_b.desc_idx) or
                (a_resolved.desc_idx == visited_b.desc_idx and b_resolved.desc_idx == visited_a.desc_idx))
            {
                return true;
            }
        }
        return false;
    }

    /// Check if a single var is already being unified in constraint unification (legacy mark-based behavior).
    /// This is used for constraint unification where we skip if EITHER var is visited.
    fn hasConstraintVisitedVar(self: *Self, var_: Var) bool {
        const resolved = self.types_store.resolveVar(var_);
        for (self.scratch.constraint_visited_vars.items.items) |visited_var| {
            const visited_resolved = self.types_store.resolveVar(visited_var);
            if (resolved.desc_idx == visited_resolved.desc_idx) return true;
        }
        return false;
    }

    // unification

    const Error = std.mem.Allocator.Error || error{
        TypeMismatch,
    };

    fn scheduleGuardedPair(self: *Self, a_var: Var, b_var: Var, on_mismatch: MismatchHandling) std.mem.Allocator.Error!void {
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .mismatch_handler = on_mismatch });
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .guarded_pair = .{
            .a = a_var,
            .b = b_var,
        } });
    }

    fn scheduleMerge(self: *Self, vars: ResolvedVarDescs, content: Content) std.mem.Allocator.Error!void {
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .merge = .{
            .vars = vars,
            .content = content,
        } });
    }

    fn newMismatchFlag(self: *Self) std.mem.Allocator.Error!u32 {
        const flag_idx: u32 = @intCast(self.scratch.mismatch_flags.items.items.len);
        _ = try self.scratch.mismatch_flags.append(self.scratch.gpa, false);
        return flag_idx;
    }

    fn runWorkLoop(self: *Self) Error!void {
        while (self.scratch.unify_work_stack.items.pop()) |frame| {
            self.processFrame(frame) catch |err| switch (err) {
                error.TypeMismatch => try self.handleTypeMismatch(),
                error.OutOfMemory => return error.OutOfMemory,
            };
        }
    }

    fn processFrame(self: *Self, frame: WorkFrame) Error!void {
        switch (frame) {
            .mismatch_handler => {},
            .guarded_pair => |pair| try self.processGuardedPair(pair.a, pair.b),
            .guard_handler => |handler| {
                self.scratch.visited_vars.items.items.len = handler.visited_vars_len;
                self.unresolved_b = handler.saved_unresolved_b;
            },
            .unify_vars => |vars| try self.unifyVars(&vars),
            .merge => |merge_frame| try self.merge(&merge_frame.vars, merge_frame.content),
            .merge_to_nominal => |merge_frame| try self.mergeToNominal(&merge_frame.vars, merge_frame.direction),
            .same_alias_after_args => |post| try self.processSameAliasAfterArgs(post),
            .shared_fields_after_children => |post| try self.processSharedFieldsAfterChildren(post),
            .shared_tags_after_children => |post| try self.processSharedTagsAfterChildren(post),
        }
    }

    fn processGuardedPair(self: *Self, a_var: Var, b_var: Var) Error!void {
        switch (self.types_store.checkVarsEquiv(a_var, b_var)) {
            .equiv => return,
            .not_equiv => |vars| {
                if (self.isPairVisited(a_var, b_var)) {
                    return;
                }

                const visited_vars_len: u32 = @intCast(self.scratch.visited_vars.items.items.len);
                _ = try self.scratch.visited_vars.append(self.scratch.gpa, a_var);
                _ = try self.scratch.visited_vars.append(self.scratch.gpa, b_var);

                const saved_unresolved_b = self.unresolved_b;
                self.unresolved_b = b_var;

                _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .guard_handler = .{
                    .visited_vars_len = visited_vars_len,
                    .saved_unresolved_b = saved_unresolved_b,
                } });
                _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .unify_vars = vars });
            },
        }
    }

    fn handleTypeMismatch(self: *Self) Error!void {
        while (self.scratch.unify_work_stack.items.pop()) |frame| {
            switch (frame) {
                .guard_handler => |handler| {
                    self.scratch.visited_vars.items.items.len = handler.visited_vars_len;
                    self.unresolved_b = handler.saved_unresolved_b;
                },
                .mismatch_handler => |handler| {
                    switch (handler) {
                        .propagate => {},
                        else => return try self.applyMismatchHandling(handler),
                    }
                },
                else => {},
            }
        }

        return error.TypeMismatch;
    }

    fn applyMismatchHandling(self: *Self, handling: MismatchHandling) Error!void {
        switch (handling) {
            .propagate => return error.TypeMismatch,
            .abort => return error.TypeMismatch,
            .ignore => return,
            .set_flag => |flag_idx| {
                self.scratch.mismatch_flags.items.items[flag_idx] = true;
            },
        }
    }

    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        try self.scheduleGuardedPair(a_var, b_var, .propagate);
    }

    /// Unify two vars
    /// Internal entry point for unification logic.
    fn unifyVars(self: *Self, vars: *const ResolvedVarDescs) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (vars.a.desc.content) {
            .flex => |flex| {
                try self.unifyFlex(vars, flex, vars.b.desc.content);
            },
            .rigid => |rigid| {
                try self.unifyRigid(vars, rigid, vars.b.desc.content);
            },
            .alias => |a_alias| {
                try self.unifyAlias(vars, a_alias, vars.b.desc.content);
            },
            .structure => |a_flat_type| {
                try self.unifyStructure(vars, a_flat_type, vars.b.desc.content);
            },
            .err => try self.merge(vars, .err),
        }
    }

    // Unify flex //

    /// Unify when `a` was a flex
    fn unifyFlex(self: *Self, vars: *const ResolvedVarDescs, a_flex: Flex, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                const mb_ident = blk: {
                    if (a_flex.name) |a_ident| {
                        break :blk a_ident;
                    } else {
                        break :blk b_flex.name;
                    }
                };

                const merged_constraints = try self.unifyStaticDispatchConstraints(a_flex.constraints, b_flex.constraints);
                try self.merge(vars, Content{ .flex = .{
                    .name = mb_ident,
                    .constraints = merged_constraints,
                } });
            },
            .rigid => |b_rigid| {
                try self.recordDeferredConstraint(vars, a_flex.constraints);
                try self.merge(vars, .{ .rigid = b_rigid });
            },
            .alias => |b_alias| {
                if (a_flex.constraints.len() == 0) {
                    try self.merge(vars, b_content);
                } else {
                    // Merge against backing var, so we don't loose static dispatch constraints
                    const backing_var = self.types_store.getAliasBackingVar(b_alias);
                    try self.unifyGuarded(vars.a.var_, backing_var);
                }
            },
            .structure => {
                try self.recordDeferredConstraint(vars, a_flex.constraints);
                try self.merge(vars, b_content);
            },
            .err => try self.merge(vars, .err),
        }
    }

    // Unify rigid //

    /// Unify when `a` was a rigid
    fn unifyRigid(self: *Self, vars: *const ResolvedVarDescs, a_rigid: Rigid, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                try self.recordDeferredConstraintOn(vars.a.var_, b_flex.constraints);
                try self.merge(vars, .{ .rigid = a_rigid });
            },
            .rigid => return error.TypeMismatch,
            .alias => |b_alias| {
                // Aliases are transparent, so expand to the backing var and
                // unify that against the rigid. This mirrors the `.rigid` branch
                // of `unifyAlias`, keeping unification commutative.
                const backing_var = self.types_store.getAliasBackingVar(b_alias);
                try self.unifyGuarded(backing_var, vars.a.var_);
            },
            .structure => return error.TypeMismatch,
            .err => try self.merge(vars, .err),
        }
    }

    // Unify alias //

    /// Unify when `a` was a alias
    fn unifyAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        const backing_var = self.types_store.getAliasBackingVar(a_alias);

        switch (b_content) {
            .flex => |b_flex| {
                if (b_flex.constraints.len() == 0) {
                    try self.merge(vars, Content{ .alias = a_alias });
                } else {
                    // Merge against backing var, so we don't loose static dispatch constraints
                    try self.unifyGuarded(backing_var, vars.b.var_);
                }
            },
            .rigid => {
                try self.unifyGuarded(backing_var, vars.b.var_);
            },
            .alias => |b_alias| {
                const b_backing_var = self.types_store.getAliasBackingVar(b_alias);
                if (sameAliasIdentity(a_alias, b_alias)) {
                    try self.unifyTwoAliases(vars, a_alias, b_alias);
                } else {
                    try self.unifyGuarded(backing_var, b_backing_var);
                }
            },
            .structure => {
                // Structural aliases are transparent. The concrete structure
                // constrains the alias backing; alias spelling is checked
                // presentation data, not union-find representative shape.
                try self.unifyGuarded(vars.b.var_, backing_var);
            },
            .err => try self.merge(vars, .err),
        }
    }

    /// Unify two aliases
    ///
    /// This function assumes the caller has already checked that the alias names match
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    ///
    /// NOTE: the rust version of this function `unify_two_aliases` is *significantly* more
    /// complicated than the version here
    fn unifyTwoAliases(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_alias: Alias) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        const did_arg_error_flag = try self.newMismatchFlag();

        var a_args_range = a_alias.vars.nonempty;
        a_args_range.dropFirstElem();
        var b_args_range = b_alias.vars.nonempty;
        b_args_range.dropFirstElem();

        if (a_args_range.len() != b_args_range.len()) {
            return error.TypeMismatch;
        }

        // Rust compiler comment:
        // Don't report real_var mismatches, because they must always be surfaced higher, from the argument types.
        const a_backing_var = self.types_store.getAliasBackingVar(a_alias);
        const b_backing_var = self.types_store.getAliasBackingVar(b_alias);
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .same_alias_after_args = .{
            .vars = vars.*,
            .a_backing_var = a_backing_var,
            .b_backing_var = b_backing_var,
            .did_arg_error_flag = did_arg_error_flag,
        } });

        var i: u32 = a_args_range.len();
        while (i > 0) {
            i -= 1;
            try self.scheduleGuardedPair(
                self.types_store.getVarAt(a_args_range, i),
                self.types_store.getVarAt(b_args_range, i),
                .{ .set_flag = did_arg_error_flag },
            );
        }
    }

    // Unify structure //

    /// Unify when `a` is a structure type
    fn unifyStructure(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_content: Content,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                try self.recordDeferredConstraint(vars, b_flex.constraints);
                try self.merge(vars, Content{ .structure = a_flat_type });
            },
            .rigid => return error.TypeMismatch,
            .alias => |b_alias| {
                const backing_var = self.types_store.getAliasBackingVar(b_alias);
                // Structural aliases are transparent. The concrete structure
                // constrains the alias backing; alias spelling is checked
                // presentation data, not union-find representative shape.
                try self.unifyGuarded(vars.a.var_, backing_var);
            },
            .structure => |b_flat_type| {
                try self.unifyFlatType(vars, a_flat_type, b_flat_type);
            },
            .err => try self.merge(vars, .err),
        }
    }

    /// Unify when `a` is a structure type
    fn unifyFlatType(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_flat_type: FlatType,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (a_flat_type) {
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        try self.unifyTuple(vars, a_tuple, b_tuple);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .nominal_type => |a_type| {
                // NOTE: this arm short-circuits err-backed nominals to a
                // SUCCESSFUL `.err` merge; if that changes, revisit
                // `structurallyIncompatiblePair` (bottom of this module) — the
                // defaulting pre-filter's soundness fence encodes it.
                const a_backing_var = self.types_store.getNominalBackingVar(a_type);
                const a_backing_resolved = self.types_store.resolveVar(a_backing_var);
                if (a_backing_resolved.desc.content == .err) {
                    try self.merge(vars, .err);
                    return;
                }

                switch (b_flat_type) {
                    .nominal_type => |b_type| {
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }

                        try self.unifyNominalType(vars, a_type, b_type);
                    },
                    .tag_union => |b_tag_union| {
                        // Try to unify nominal tag union (a) with anonymous tag union (b)
                        try self.unifyTagUnionWithNominal(vars, a_type, a_backing_var, a_backing_resolved, b_tag_union, .a_is_nominal);
                    },
                    .empty_tag_union => {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!a_type.canLiftInner(self.qualified_module_ident)) {
                            return error.TypeMismatch;
                        }

                        if (a_backing_resolved.desc.content == .structure and
                            a_backing_resolved.desc.content.structure == .empty_tag_union)
                        {
                            try self.merge(vars, vars.a.desc.content);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        // Try to unify nominal record (a) with anonymous record (b)
                        try self.unifyRecordWithNominal(vars, a_type, a_backing_var, a_backing_resolved, b_record.fields, .{ .ext = b_record.ext }, .a_is_nominal);
                    },
                    .record_unbound => |b_fields| {
                        // Try to unify nominal record (a) with anonymous unbound record (b)
                        try self.unifyRecordWithNominal(vars, a_type, a_backing_var, a_backing_resolved, b_fields, .unbound, .a_is_nominal);
                    },
                    .empty_record => {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!a_type.canLiftInner(self.qualified_module_ident)) {
                            return error.TypeMismatch;
                        }

                        if (a_backing_resolved.desc.content == .structure and
                            a_backing_resolved.desc.content.structure == .empty_record)
                        {
                            try self.merge(vars, vars.a.desc.content);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_pure => |a_func| {
                switch (b_flat_type) {
                    .fn_pure => |b_func| {
                        try self.scheduleMerge(vars.*, vars.a.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_unbound => |b_func| {
                        // pure unifies with unbound -> pure
                        try self.scheduleMerge(vars.*, vars.a.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_effectful => {
                        // pure cannot unify with effectful
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_effectful => |a_func| {
                switch (b_flat_type) {
                    .fn_effectful => |b_func| {
                        try self.scheduleMerge(vars.*, vars.a.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_unbound => |b_func| {
                        // effectful unifies with unbound -> effectful
                        try self.scheduleMerge(vars.*, vars.a.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_pure => {
                        // effectful cannot unify with pure
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_unbound => |a_func| {
                switch (b_flat_type) {
                    .fn_pure => |b_func| {
                        // unbound unifies with pure -> pure
                        try self.scheduleMerge(vars.*, vars.b.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_effectful => |b_func| {
                        // unbound unifies with effectful -> effectful
                        try self.scheduleMerge(vars.*, vars.b.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    .fn_unbound => |b_func| {
                        // unbound unifies with unbound -> unbound
                        try self.scheduleMerge(vars.*, vars.a.desc.content);
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record => |a_record| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_record.fields.len() == 0) {
                            try self.unifyGuarded(a_record.ext, vars.b.var_);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        try self.unifyTwoRecords(
                            vars,
                            a_record.fields,
                            .{ .ext = a_record.ext },
                            b_record.fields,
                            .{ .ext = b_record.ext },
                        );
                    },
                    .record_unbound => |b_fields| {
                        try self.unifyTwoRecords(
                            vars,
                            a_record.fields,
                            .{ .ext = a_record.ext },
                            b_fields,
                            .unbound,
                        );
                    },
                    .nominal_type => |b_type| {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!b_type.canLiftInner(self.qualified_module_ident)) {
                            return error.TypeMismatch;
                        }

                        // Try to unify anonymous record (a) with nominal record (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }
                        try self.unifyRecordWithNominal(vars, b_type, b_backing_var, b_backing_resolved, a_record.fields, .{ .ext = a_record.ext }, .b_is_nominal);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record_unbound => |a_fields| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_fields.len() == 0) {
                            // Both are empty, merge as empty_record
                            try self.merge(vars, Content{ .structure = .empty_record });
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        try self.unifyTwoRecords(
                            vars,
                            a_fields,
                            .unbound,
                            b_record.fields,
                            .{ .ext = b_record.ext },
                        );
                    },
                    .record_unbound => |b_fields| {
                        try self.unifyTwoRecords(
                            vars,
                            a_fields,
                            .unbound,
                            b_fields,
                            .unbound,
                        );
                    },
                    .nominal_type => |b_type| {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!b_type.canLiftInner(self.qualified_module_ident)) {
                            return error.TypeMismatch;
                        }

                        // Try to unify anonymous unbound record (a) with nominal record (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }
                        try self.unifyRecordWithNominal(vars, b_type, b_backing_var, b_backing_resolved, a_fields, .unbound, .b_is_nominal);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_record => {
                switch (b_flat_type) {
                    .empty_record => {
                        try self.merge(vars, Content{ .structure = .empty_record });
                    },
                    .record => |b_record| {
                        if (b_record.fields.len() == 0) {
                            try self.unifyGuarded(vars.a.var_, b_record.ext);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record_unbound => |b_fields| {
                        if (b_fields.len() == 0) {
                            // Both are empty, merge as empty_record
                            try self.merge(vars, Content{ .structure = .empty_record });
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .nominal_type => |b_type| {
                        // Try to unify empty record (a) with nominal record (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }

                        // Check if the nominal's backing is also an empty record (or record with 0 fields)
                        const backing_is_empty = blk: {
                            if (b_backing_resolved.desc.content != .structure) break :blk false;
                            const backing_flat = b_backing_resolved.desc.content.structure;
                            if (backing_flat == .empty_record) break :blk true;
                            if (backing_flat == .record) {
                                const fields = self.types_store.getRecordFieldsSlice(backing_flat.record.fields);
                                if (fields.len == 0) break :blk true;
                            }
                            break :blk false;
                        };
                        if (backing_is_empty) {
                            // Both are empty, unify with the nominal
                            try self.merge(vars, vars.b.desc.content);
                        } else {
                            // Nominal has a non-empty backing, can't unify
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .tag_union => |a_tag_union| {
                switch (b_flat_type) {
                    .empty_tag_union => {
                        if (a_tag_union.tags.len() == 0) {
                            try self.unifyGuarded(a_tag_union.ext, vars.b.var_);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .tag_union => |b_tag_union| {
                        try self.unifyTwoTagUnions(vars, a_tag_union, b_tag_union);
                    },
                    .nominal_type => |b_type| {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!b_type.canLiftInner(self.qualified_module_ident)) {
                            return error.TypeMismatch;
                        }

                        // Try to unify anonymous tag union (a) with nominal tag union (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }
                        try self.unifyTagUnionWithNominal(vars, b_type, b_backing_var, b_backing_resolved, a_tag_union, .b_is_nominal);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_tag_union => {
                switch (b_flat_type) {
                    .empty_tag_union => {
                        try self.merge(vars, Content{ .structure = .empty_tag_union });
                    },
                    .tag_union => |b_tag_union| {
                        if (b_tag_union.tags.len() == 0) {
                            try self.unifyGuarded(vars.a.var_, b_tag_union.ext);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .nominal_type => |b_type| {
                        // Try to unify empty tag union (a) with nominal tag union (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            try self.merge(vars, .err);
                            return;
                        }

                        // Check if the nominal's backing is also an empty tag union
                        if (b_backing_resolved.desc.content == .structure and
                            b_backing_resolved.desc.content.structure == .empty_tag_union)
                        {
                            // Both are empty, unify with the nominal
                            try self.merge(vars, vars.b.desc.content);
                        } else {
                            // Nominal has a non-empty backing, can't unify
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
        }
    }

    /// unify tuples
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unifyTuple(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tuple: Tuple,
        b_tuple: Tuple,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (a_tuple.elems.len() != b_tuple.elems.len()) {
            return error.TypeMismatch;
        }

        // Unify element types first (recursion guard in unifyGuarded prevents infinite loops)
        try self.scheduleMerge(vars.*, vars.b.desc.content);
        var i: u32 = a_tuple.elems.len();
        while (i > 0) {
            i -= 1;
            try self.scheduleGuardedPair(
                self.types_store.getVarAt(a_tuple.elems, i),
                self.types_store.getVarAt(b_tuple.elems, i),
                .propagate,
            );
        }
    }

    // Unify nominal type //

    /// Unify when `a` was a nominal type
    fn unifyNominalType(self: *Self, vars: *const ResolvedVarDescs, a_type: NominalType, b_type: NominalType) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Check if either nominal type has an invalid backing variable.
        // NOTE: if you change this err short-circuit (a SUCCESSFUL merge to
        // `.err`, not a mismatch), revisit `structurallyIncompatiblePair`
        // (bottom of this module) — the defaulting pre-filter's soundness fence
        // encodes it.
        const a_backing_var = self.types_store.getNominalBackingVar(a_type);
        const a_backing_resolved = self.types_store.resolveVar(a_backing_var);
        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
        if (a_backing_resolved.desc.content == .err or b_backing_resolved.desc.content == .err) {
            // Invalid nominal type - propagate the error
            try self.merge(vars, .err);
            return;
        }

        // NOTE: if distinct nominal identities ever start unifying (e.g.
        // implicit numeric coercion), revisit `structurallyIncompatiblePair`
        // (bottom of this module) — its refutation theorem is exactly this
        // check.
        if (!sameNominalIdentity(a_type, b_type)) {
            return error.TypeMismatch;
        }

        if (a_type.vars.nonempty.count != b_type.vars.nonempty.count) {
            return error.TypeMismatch;
        }

        try self.scheduleMerge(vars.*, vars.b.desc.content);
        var a_args_range = a_type.vars.nonempty;
        a_args_range.dropFirstElem();
        var b_args_range = b_type.vars.nonempty;
        b_args_range.dropFirstElem();
        var i: u32 = a_args_range.len();
        while (i > 0) {
            i -= 1;
            try self.scheduleGuardedPair(
                self.types_store.getVarAt(a_args_range, i),
                self.types_store.getVarAt(b_args_range, i),
                .propagate,
            );
        }
    }

    /// After a structural value successfully unifies with a nominal's backing,
    /// merge the result to the nominal side — the nominal "wins" over the
    /// anonymous record/tag it lifted from.
    fn mergeToNominal(self: *Self, vars: *const ResolvedVarDescs, direction: NominalDirection) Error!void {
        switch (direction) {
            .a_is_nominal => try self.merge(vars, vars.a.desc.content),
            .b_is_nominal => try self.merge(vars, vars.b.desc.content),
        }
    }

    fn processSameAliasAfterArgs(self: *Self, post: SameAliasAfterArgs) Error!void {
        if (self.scratch.mismatch_flags.items.items[post.did_arg_error_flag]) {
            return error.TypeMismatch;
        }

        try self.scheduleMerge(post.vars, post.vars.b.desc.content);
        try self.scheduleGuardedPair(post.a_backing_var, post.b_backing_var, .ignore);
    }

    fn processSharedFieldsAfterChildren(self: *Self, post: SharedFieldsAfterChildren) Error!void {
        if (self.scratch.mismatch_flags.items.items[post.did_field_error_flag]) {
            return error.TypeMismatch;
        }

        const range_start: u32 = @intCast(self.types_store.record_fields.len());

        for (self.scratch.in_both_fields.sliceRange(post.shared_fields_range)) |shared| {
            _ = try self.types_store.appendRecordFields(&[_]RecordField{.{
                .name = shared.b.name,
                .var_ = shared.b.var_,
            }});
        }

        if (post.mb_a_extended_fields) |extended_fields| {
            _ = try self.types_store.appendRecordFields(
                self.scratch.only_in_a_fields.sliceRange(extended_fields),
            );
        }
        if (post.mb_b_extended_fields) |extended_fields| {
            _ = try self.types_store.appendRecordFields(
                self.scratch.only_in_b_fields.sliceRange(extended_fields),
            );
        }

        try self.merge(&post.vars, Content{ .structure = FlatType{ .record = .{
            .fields = self.types_store.record_fields.rangeToEnd(range_start),
            .ext = post.ext,
        } } });
    }

    fn processSharedTagsAfterChildren(self: *Self, post: SharedTagsAfterChildren) Error!void {
        const range_start: u32 = @intCast(self.types_store.tags.len());

        for (self.scratch.in_both_tags.sliceRange(post.shared_tags_range)) |tags| {
            _ = try self.types_store.appendTags(&[_]Tag{.{
                .name = tags.b.name,
                .args = tags.b.args,
            }});
        }

        if (post.mb_a_extended_tags) |extended_tags| {
            _ = try self.types_store.appendTags(
                self.scratch.only_in_a_tags.sliceRange(extended_tags),
            );
        }
        if (post.mb_b_extended_tags) |extended_tags| {
            _ = try self.types_store.appendTags(
                self.scratch.only_in_b_tags.sliceRange(extended_tags),
            );
        }

        try self.merge(&post.vars, Content{ .structure = FlatType{ .tag_union = .{
            .tags = self.types_store.tags.rangeToEnd(range_start),
            .ext = post.ext,
        } } });
    }

    fn unifyTagUnionWithNominal(
        self: *Self,
        vars: *const ResolvedVarDescs,
        nominal_type: NominalType,
        nominal_backing_var: Var,
        nominal_backing_resolved: ResolvedVarDesc,
        anon_tag_union: TagUnion,
        direction: NominalDirection,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // If this nominal is opaque and we're not in the origin module, error
        if (!nominal_type.canLiftInner(self.qualified_module_ident)) {
            return error.TypeMismatch;
        }

        // Check if the nominal's backing type is a tag union (including empty)
        const nominal_backing_content = nominal_backing_resolved.desc.content;
        if (nominal_backing_content != .structure) {
            return error.TypeMismatch;
        }

        const nominal_backing_flat = nominal_backing_content.structure;

        // Handle empty tag union case
        if (nominal_backing_flat == .empty_tag_union) {
            // The nominal's backing is an empty tag union []
            // The anon_tag_union should also be empty for unification to succeed
            if (anon_tag_union.tags.len() == 0) {
                // Both are empty - unify the extension variables
                _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .merge_to_nominal = .{
                    .vars = vars.*,
                    .direction = direction,
                } });
                try self.unifyGuarded(anon_tag_union.ext, nominal_backing_var);
                return;
            } else {
                // Anon has tags but nominal is empty
                return error.TypeMismatch;
            }
        }

        if (nominal_backing_flat != .tag_union) {
            // Nominal's backing is not a tag union (could be record, tuple, etc.)
            // Cannot unify anonymous tag union with non-tag-union nominal
            return error.TypeMismatch;
        }

        const nominal_backing_tag_union = nominal_backing_flat.tag_union;

        // Check if all tags in the anon union exist in the nominal union BEFORE
        // doing any modifications. This prevents corrupting type information when
        // unification fails (which would cause bad error messages).
        // Check that nominal has an open extension or all anon tags are in nominal
        const nominal_ext_resolved = self.types_store.resolveVar(nominal_backing_tag_union.ext);
        const nominal_is_closed = nominal_ext_resolved.desc.content == .structure and
            nominal_ext_resolved.desc.content.structure == .empty_tag_union;

        if (nominal_is_closed) {
            // For closed nominals, every tag in anon must exist in nominal
            const anon_tag_slice = self.types_store.tags.sliceRange(anon_tag_union.tags);
            const nominal_tag_slice = self.types_store.tags.sliceRange(nominal_backing_tag_union.tags);
            const anon_names = anon_tag_slice.items(.name);
            const nominal_names = nominal_tag_slice.items(.name);

            for (anon_names) |anon_name| {
                var found = false;
                for (nominal_names) |nominal_name| {
                    if (anon_name.eql(nominal_name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    return error.TypeMismatch;
                }
            }
        }

        // Unification succeeded — the nominal type wins.
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .merge_to_nominal = .{
            .vars = vars.*,
            .direction = direction,
        } });

        // Now safe to proceed with full unification.
        try self.unifyTwoTagUnions(vars, anon_tag_union, nominal_backing_tag_union);
    }

    fn unifyRecordWithNominal(
        self: *Self,
        vars: *const ResolvedVarDescs,
        nominal_type: NominalType,
        _: Var, // nominal_backing_var - unused for records
        nominal_backing_resolved: ResolvedVarDesc,
        anon_record_fields: RecordField.SafeMultiList.Range,
        anon_record_ext: RecordExt,
        direction: NominalDirection,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // If this nominal is opaque and we're not in the origin module, error
        if (!nominal_type.canLiftInner(self.qualified_module_ident)) {
            return error.TypeMismatch;
        }

        // Check if the nominal's backing type is a record (including empty)
        const nominal_backing_content = nominal_backing_resolved.desc.content;
        if (nominal_backing_content != .structure) {
            return error.TypeMismatch;
        }

        const nominal_backing_flat = nominal_backing_content.structure;

        // Handle empty record case
        if (nominal_backing_flat == .empty_record) {
            // The nominal's backing is an empty record {}
            // The anon record should also be empty for unification to succeed
            if (anon_record_fields.len() == 0) {
                // Both are empty — merge to the NOMINAL type.
                try self.mergeToNominal(vars, direction);
                return;
            } else {
                // Anon has fields but nominal is empty
                return error.TypeMismatch;
            }
        }

        if (nominal_backing_flat != .record) {
            // Nominal's backing is not a record (could be tag union, tuple, etc.)
            // Cannot unify anonymous record with non-record nominal
            return error.TypeMismatch;
        }

        const nominal_backing_record = nominal_backing_flat.record;

        // Unification succeeded — the nominal type wins.
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .merge_to_nominal = .{
            .vars = vars.*,
            .direction = direction,
        } });

        // Unify the record fields
        try self.unifyTwoRecords(
            vars,
            anon_record_fields,
            anon_record_ext,
            nominal_backing_record.fields,
            .{ .ext = nominal_backing_record.ext },
        );
    }

    /// unify func
    ///
    /// this checks:
    /// * that the arg arities are the same
    /// * that parallel args unify
    /// * that ret unifies
    fn unifyFunc(
        self: *Self,
        _: *const ResolvedVarDescs,
        a_func: Func,
        b_func: Func,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (a_func.args.len() != b_func.args.len()) {
            return error.TypeMismatch;
        }

        try self.scheduleGuardedPair(a_func.ret, b_func.ret, .propagate);
        var i: u32 = a_func.args.len();
        while (i > 0) {
            i -= 1;
            try self.scheduleGuardedPair(
                self.types_store.getVarAt(a_func.args, i),
                self.types_store.getVarAt(b_func.args, i),
                .propagate,
            );
        }
    }

    /// Unify two extensible records.
    ///
    /// This function implements Elm-style record unification.
    ///
    /// Each record consists of:
    /// - a fixed set of known fields (`fields`)
    /// - an extensible tail variable (`ext`) that may point to additional unknown fields
    ///
    /// Given two records `a` and `b`, we:
    ///   1. Collect all known fields by unwrapping their `ext` chains.
    ///   2. Partition the field sets into:
    ///      - `in_both`: shared fields present in both `a` and `b`
    ///      - `only_in_a`: fields only present in `a`
    ///      - `only_in_b`: fields only present in `b`
    ///   3. Determine the relationship between the two records based on these partitions.
    ///
    /// Four cases follow:
    ///
    /// ---
    ///
    /// **Case 1: Exactly the Same Fields**
    ///
    /// a = { x, y, ..others_a }
    /// b = { x, y, ..others_b }
    ///
    /// - All fields are shared
    /// - We unify `others_a ~ others_b`
    /// - Then unify each shared field pair
    ///
    /// ---
    ///
    /// **Case 2: `a` Extends `b`**
    ///
    /// a = { x, y, z, ..others_a }
    /// b = { x, y, ..others_b }
    ///
    /// - `a` has additional fields not in `b`
    /// - We generate a new var `only_in_a_var = { z, ..others_a }`
    /// - Unify `only_in_a_var ~ others_b`
    /// - Then unify shared fields
    ///
    /// ---
    ///
    /// **Case 3: `b` Extends `a`**
    ///
    /// a = { x, y, ..others_a }
    /// b = { x, y, z, ..others_b }
    ///
    /// - Same as Case 2, but reversed
    /// - `b` has additional fields not in `a`
    /// - We generate a new var `only_in_b_var = { z, ..others_b }`
    /// - Unify `others_a ~ only_in_b_var`
    /// - Then unify shared fields
    ///
    /// ---
    ///
    /// **Case 4: Both Extend Each Other**
    ///
    /// a = { x, y, z, ..others_a }
    /// b = { x, y, w, ..others_b }
    ///
    /// - Each has unique fields the other lacks
    /// - Generate:
    ///     - shared_others = fresh flex_var
    ///     - only_in_a_var = { z, ..shared_others }
    ///     - only_in_b_var = { w, ..shared_others }
    /// - Unify:
    ///     - `others_a ~ only_in_b_var`
    ///     - `only_in_a_var ~ others_b`
    /// - Then unify shared fields into `{ x, y, ..shared_others }`
    ///
    /// ---
    ///
    /// All field unification is done using `unifySharedFields`, and new variables are created using `fresh`.
    ///
    /// This function does not attempt to deduplicate fields or reorder them — callers are responsible
    /// for providing consistent field names.
    fn unifyTwoRecords(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_fields: RecordField.SafeMultiList.Range,
        a_ext: RecordExt,
        b_fields: RecordField.SafeMultiList.Range,
        b_ext: RecordExt,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // First, unwrap all fields for record, erroring if we encounter an
        // invalid record ext var
        const a_gathered_fields = try self.gatherRecordFields(a_fields, a_ext);
        const b_gathered_fields = try self.gatherRecordFields(b_fields, b_ext);

        // Then partition the fields
        const partitioned = try self.partitionFields(
            self.scratch,
            a_gathered_fields.range,
            b_gathered_fields.range,
        );

        // Determine how the fields of a & b extend
        const a_has_uniq_fields = partitioned.only_in_a.len() > 0;
        const b_has_uniq_fields = partitioned.only_in_b.len() > 0;

        var fields_ext: FieldsExtension = .exactly_the_same;
        if (a_has_uniq_fields and b_has_uniq_fields) {
            fields_ext = .both_extend;
        } else if (a_has_uniq_fields) {
            fields_ext = .a_extends_b;
        } else if (b_has_uniq_fields) {
            fields_ext = .b_extends_a;
        }

        const a_gathered_ext = blk: {
            switch (a_gathered_fields.ext) {
                .unbound => break :blk try self.fresh(vars, .{ .flex = Flex.init() }),
                .ext => |ext_var| break :blk ext_var,
            }
        };
        const b_gathered_ext = blk: {
            switch (b_gathered_fields.ext) {
                .unbound => break :blk try self.fresh(vars, .{ .flex = Flex.init() }),
                .ext => |ext_var| break :blk ext_var,
            }
        };

        // Unify fields
        switch (fields_ext) {
            .exactly_the_same => {
                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    a_gathered_ext,
                );

                // Unify exts
                try self.unifyGuarded(a_gathered_ext, b_gathered_ext);
            },
            .a_extends_b => {
                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = try self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.sliceRange(partitioned.only_in_a),
                );
                const only_in_a_var = try self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_ext,
                } } });

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    only_in_a_var,
                );

                // Unify the sub record with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_ext);
            },
            .b_extends_a => {
                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = try self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.sliceRange(partitioned.only_in_b),
                );
                const only_in_b_var = try self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = b_gathered_ext,
                } } });

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    only_in_b_var,
                );

                // Unify the sub record with a's ext
                try self.unifyGuarded(a_gathered_ext, only_in_b_var);
            },
            .both_extend => {
                // Create a new ext var
                const new_ext_var = try self.fresh(vars, .{ .flex = Flex.init() });

                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = try self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.sliceRange(partitioned.only_in_a),
                );
                const only_in_a_var = try self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = new_ext_var,
                } } });

                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = try self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.sliceRange(partitioned.only_in_b),
                );
                const only_in_b_var = try self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = new_ext_var,
                } } });

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    partitioned.in_both,
                    partitioned.only_in_a,
                    partitioned.only_in_b,
                    new_ext_var,
                );

                // Unify the sub records with exts
                // The order we unify is important! If these are flipped, then
                // you can get into weird partial-merge states
                try self.unifyGuarded(a_gathered_ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_gathered_ext);
            },
        }
    }

    const FieldsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const RecordExt = union(enum) { ext: Var, unbound };

    const GatheredFields = struct { ext: RecordExt, range: RecordFieldSafeList.Range };

    /// Recursively unwraps the fields of an extensible record, flattening all visible fields
    /// into `scratch.gathered_fields` and following through:
    /// * aliases (by chasing `.backing_var`)
    /// * record extension chains (via nested `.record.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered fields in `gathered_fields`
    /// * the final tail extension variable, which is either a flex var or an empty record
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-record type).
    fn gatherRecordFields(self: *Self, record_fields: RecordField.SafeMultiList.Range, record_ext: RecordExt) Error!GatheredFields {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = try self.scratch.copyGatherFieldsFromMultiList(
            &self.types_store.record_fields,
            record_fields,
        );

        // Note: If a field name appears multiple times (e.g., in both base and extension),
        // we keep the leftmost field (left-bias semantics, like Haskell's Map.union).
        // Duplicates within a single record's extension chain are rare and typically
        // indicate a type system bug (e.g., malformed type like `{ name: Str, ..{ name: U32 } }`).
        // The outer unification logic handles unifying fields across *different* records.

        // Recursively gather fields from extensions
        var ext = record_ext;
        var guard = types_mod.debug.IterationGuard.init("gatherRecordFields");
        while (true) {
            guard.tick();
            switch (ext) {
                .unbound => {
                    return .{ .ext = ext, .range = range };
                },
                .ext => |ext_var| {
                    switch (self.types_store.resolveVar(ext_var).desc.content) {
                        .flex => {
                            return .{ .ext = .{ .ext = ext_var }, .range = range };
                        },
                        .rigid => {
                            return .{ .ext = .{ .ext = ext_var }, .range = range };
                        },
                        .alias => |alias| {
                            ext = .{ .ext = self.types_store.getAliasBackingVar(alias) };
                        },
                        .structure => |flat_type| {
                            switch (flat_type) {
                                .record => |ext_record| {
                                    const next_fields = self.types_store.record_fields.sliceRange(ext_record.fields);

                                    // Merge extension fields while maintaining sorted order
                                    try self.scratch.mergeSortedExtensionFields(
                                        &range,
                                        next_fields.items(.name),
                                        next_fields.items(.var_),
                                        self.ident_store,
                                    );

                                    ext = .{ .ext = ext_record.ext };
                                },
                                .record_unbound => |fields| {
                                    const next_fields = self.types_store.record_fields.sliceRange(fields);

                                    // Merge extension fields while maintaining sorted order
                                    try self.scratch.mergeSortedExtensionFields(
                                        &range,
                                        next_fields.items(.name),
                                        next_fields.items(.var_),
                                        self.ident_store,
                                    );

                                    return .{ .ext = ext, .range = range };
                                },
                                else => return .{ .ext = ext, .range = range },
                            }
                        },
                        else => return .{ .ext = ext, .range = range },
                    }
                },
            }
        }
    }

    const PartitionedRecordFields = struct {
        only_in_a: RecordFieldSafeList.Range,
        only_in_b: RecordFieldSafeList.Range,
        in_both: TwoRecordFieldsSafeList.Range,
    };

    /// Given two ranges of record fields stored in `scratch.gathered_fields`, this function:
    /// * sorts both slices in-place by field name
    /// * partitions them into three disjoint groups:
    ///     - fields only in `a`
    ///     - fields only in `b`
    ///     - fields present in both (by name)
    ///
    /// These groups are stored into dedicated scratch buffers:
    /// * `only_in_a_fields`
    /// * `only_in_b_fields`
    /// * `in_both_fields`
    ///
    /// The result is a set of ranges that can be used to slice those buffers.
    ///
    /// The caller must not mutate the field ranges between `gatherRecordFields` and `partitionFields`.
    fn partitionFields(
        self: *const Self,
        scratch: *Scratch,
        a_fields_range: RecordFieldSafeList.Range,
        b_fields_range: RecordFieldSafeList.Range,
    ) std.mem.Allocator.Error!PartitionedRecordFields {
        // Sort the fields (gathering maintains partial order, but unification may create unsorted unions)
        const a_fields = scratch.gathered_fields.sliceRange(a_fields_range);
        std.mem.sort(RecordField, a_fields, self, struct {
            fn less(unifier: *const Self, a: RecordField, b: RecordField) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.name), unifier.getTypeIdentText(b.name)) == .lt;
            }
        }.less);
        const b_fields = scratch.gathered_fields.sliceRange(b_fields_range);
        std.mem.sort(RecordField, b_fields, self, struct {
            fn less(unifier: *const Self, a: RecordField, b: RecordField) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.name), unifier.getTypeIdentText(b.name)) == .lt;
            }
        }.less);

        // Get the start of index of the new range
        const a_fields_start: u32 = @intCast(scratch.only_in_a_fields.len());
        const b_fields_start: u32 = @intCast(scratch.only_in_b_fields.len());
        const both_fields_start: u32 = @intCast(scratch.in_both_fields.len());

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_fields.len and b_i < b_fields.len) {
            const a_next = a_fields[a_i];
            const b_next = b_fields[b_i];
            const ord = std.mem.order(u8, self.getTypeIdentText(a_next.name), self.getTypeIdentText(b_next.name));
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_fields.append(scratch.gpa, TwoRecordFields{
                        .a = a_next,
                        .b = b_next,
                    });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_fields.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_fields.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_fields.len) {
            const a_next = a_fields[a_i];
            _ = try scratch.only_in_a_fields.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_fields.len) {
            const b_next = b_fields[b_i];
            _ = try scratch.only_in_b_fields.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_fields.rangeToEnd(a_fields_start),
            .only_in_b = scratch.only_in_b_fields.rangeToEnd(b_fields_start),
            .in_both = scratch.in_both_fields.rangeToEnd(both_fields_start),
        };
    }

    /// Given a list of shared fields & a list of extended fields, unify the shared
    /// Then merge a new record with both shared+extended fields
    fn unifySharedFields(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_fields_range: TwoRecordFieldsSafeList.Range, // Into scratch.in_both_fields
        mb_a_extended_fields: ?RecordFieldSafeList.Range, // Into scratch.only_in_a_fields
        mb_b_extended_fields: ?RecordFieldSafeList.Range, // Into scratch.only_in_b_fields
        ext: Var,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        const did_field_error_flag = try self.newMismatchFlag();
        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .shared_fields_after_children = .{
            .vars = vars.*,
            .shared_fields_range = shared_fields_range,
            .mb_a_extended_fields = mb_a_extended_fields,
            .mb_b_extended_fields = mb_b_extended_fields,
            .ext = ext,
            .did_field_error_flag = did_field_error_flag,
        } });

        var i: u32 = shared_fields_range.len();
        while (i > 0) {
            i -= 1;
            const idx: TwoRecordFieldsSafeList.Idx = @enumFromInt(@intFromEnum(shared_fields_range.start) + i);
            const shared = self.scratch.in_both_fields.get(idx).*;
            try self.scheduleGuardedPair(shared.a.var_, shared.b.var_, .{ .set_flag = did_field_error_flag });
        }
    }

    /// Unify two extensible tag union.
    ///
    /// This function implements Elm-style record unification, but for tag unions.
    ///
    /// Each tag union consists of:
    /// - a fixed set of known tags (`tags`)
    /// - an extensible tail variable (`ext`) that may point to additional unknown tags
    ///
    /// Given two tag unions `a` and `b`, we:
    ///   1. Collect all known tags by unwrapping their `ext` chains.
    ///   2. Partition the tags sets into:
    ///      - `in_both`: shared fields present in both `a` and `b`
    ///      - `only_in_a`: fields only present in `a`
    ///      - `only_in_b`: fields only present in `b`
    ///   3. Determine the relationship between the two tag unions based on these partitions.
    ///
    /// Four cases follow:
    ///
    /// ---
    ///
    /// **Case 1: Exactly the Same Tags**
    ///
    /// a = [ X ]ext_a
    /// b = [ X ]ext_b
    ///
    /// - All tags are shared
    /// - We unify `ext_a ~ ext_b`
    /// - Then unify each shared tag pair
    ///
    /// ---
    ///
    /// **Case 2: `a` Extends `b`**
    ///
    /// a = [ X, Y, Z ]ext_a
    /// b = [ X, Y ]ext_b
    ///
    /// - `a` has additional tags not in `b`
    /// - We generate a new var `only_in_a_var = [ Z ]ext_a`
    /// - Unify `only_in_a_var ~ ext_b`
    /// - Then unify shared tags into `[ X, Y ]only_in_a_var`
    ///
    /// ---
    ///
    /// **Case 3: `b` Extends `a`**
    ///
    /// a = [ X, Y ]ext_a
    /// b = [ X, Y, Z ]ext_b
    ///
    /// - Same as Case 2, but reversed
    /// - `b` has additional tags not in `a`
    /// - We generate a new var `only_in_b_var = [ Z ]ext_b`
    /// - Unify `ext_a ~ only_in_b_var`
    /// - Then unify shared tags into `[ X, Y ]only_in_b_var`
    ///
    /// ---
    ///
    /// **Case 4: Both Extend Each Other**
    ///
    /// a = [ X, Y, Z ]ext_a
    /// b = [ X, Y, W ]ext_b
    ///
    /// - Each has unique tags the other lacks
    /// - Generate:
    ///     - shared_ext = fresh flex_var
    ///     - only_in_a_var = [ Z ]shared_ext
    ///     - only_in_b_var = [ W ]shared_ext
    /// - Unify:
    ///     - `ext_a ~ only_in_b_var`
    ///     - `only_in_a_var ~ ext_b`
    /// - Then unify shared tags into `[ X, Y ]shared_ext`
    ///
    /// ---
    ///
    /// All tag unification is done using `unifySharedTags`, and new variables are created using `fresh`.
    ///
    /// This function does not attempt to deduplicate tags or reorder them — callers are responsible
    /// for providing consistent tag names.
    fn unifyTwoTagUnions(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tag_union: TagUnion,
        b_tag_union: TagUnion,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Unwrap all fields for tag unions, erroring on invalid ext var
        const a_gathered_tags = try self.gatherTagUnionTags(a_tag_union);
        const b_gathered_tags = try self.gatherTagUnionTags(b_tag_union);

        // Then partition the tags
        const partitioned = try self.partitionTags(
            self.scratch,
            a_gathered_tags.range,
            b_gathered_tags.range,
        );

        // Determine how the tags of a & b extend
        const a_has_uniq_tags = partitioned.only_in_a.len() > 0;
        const b_has_uniq_tags = partitioned.only_in_b.len() > 0;

        var tags_ext: TagsExtension = .exactly_the_same;
        if (a_has_uniq_tags and b_has_uniq_tags) {
            tags_ext = .both_extend;
        } else if (a_has_uniq_tags) {
            tags_ext = .a_extends_b;
        } else if (b_has_uniq_tags) {
            tags_ext = .b_extends_a;
        }

        // Check if extensions are closed (empty_tag_union)
        const a_ext_resolved = self.types_store.resolveVar(a_gathered_tags.ext);
        const b_ext_resolved = self.types_store.resolveVar(b_gathered_tags.ext);
        const a_ext_is_closed = a_ext_resolved.desc.content == .structure and
            a_ext_resolved.desc.content.structure == .empty_tag_union;
        const b_ext_is_closed = b_ext_resolved.desc.content == .structure and
            b_ext_resolved.desc.content.structure == .empty_tag_union;

        // Fast-fail cases where unification will definitely fail (before any merging)
        // This prevents corrupting type information for better error messages
        if (tags_ext == .both_extend and (a_ext_is_closed or b_ext_is_closed)) {
            // If we have unique tags on both sides AND either side is closed,
            // unification cannot succeed - fail immediately
            return error.TypeMismatch;
        }
        if (tags_ext == .a_extends_b and b_ext_is_closed) {
            // a has unique tags but b is closed - can't extend
            return error.TypeMismatch;
        }
        if (tags_ext == .b_extends_a and a_ext_is_closed) {
            // b has unique tags but a is closed - can't extend
            return error.TypeMismatch;
        }

        // Unify tags (recursion guard in unifyGuarded prevents infinite loops,
        // and unifySharedTags handles the final merge)
        switch (tags_ext) {
            .exactly_the_same => {
                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    a_gathered_tags.ext,
                );

                // Unify exts
                try self.unifyGuarded(a_gathered_tags.ext, b_gathered_tags.ext);
            },
            .a_extends_b => {
                // Create a new variable of a tag_union with only a's uniq tags
                // This copies tags from scratch into type_store
                const only_in_a_tags_range = try self.types_store.appendTags(
                    self.scratch.only_in_a_tags.sliceRange(partitioned.only_in_a),
                );
                const only_in_a_var = try self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = a_gathered_tags.ext,
                } } });

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    only_in_a_var,
                );

                // Unify the sub tag_union with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);
            },
            .b_extends_a => {
                // Create a new variable of a tag_union with only b's uniq tags
                // This copies tags from scratch into type_store
                const only_in_b_tags_range = try self.types_store.appendTags(
                    self.scratch.only_in_b_tags.sliceRange(partitioned.only_in_b),
                );
                const only_in_b_var = try self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = b_gathered_tags.ext,
                } } });

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    partitioned.in_both,
                    null,
                    null,
                    only_in_b_var,
                );

                // Unify the sub tag_union with a's ext
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);
            },
            .both_extend => {
                // Create a shared extension variable first
                // This is critical: both only_in_a_var and only_in_b_var must use this
                // shared extension to avoid creating circular type references
                const new_ext_var = try self.fresh(vars, .{ .flex = Flex.init() });

                // Create a new variable of a tag_union with only a's uniq tags
                // Uses new_ext_var (not a_gathered_tags.ext) to prevent cycles
                const only_in_a_tags_range = try self.types_store.appendTags(
                    self.scratch.only_in_a_tags.sliceRange(partitioned.only_in_a),
                );
                const only_in_a_var = try self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = new_ext_var,
                } } });

                // Create a new variable of a tag_union with only b's uniq tags
                // Uses new_ext_var (not b_gathered_tags.ext) to prevent cycles
                const only_in_b_tags_range = try self.types_store.appendTags(
                    self.scratch.only_in_b_tags.sliceRange(partitioned.only_in_b),
                );
                const only_in_b_var = try self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = new_ext_var,
                } } });

                // Unify shared tags
                // Include all tags in the merged type - both shared and unique
                // The unique tags must be included because the merged type is what
                // callers see, and the extension chain via only_in_a_var/only_in_b_var
                // is for proper type equality, not for visibility
                try self.unifySharedTags(
                    vars,
                    partitioned.in_both,
                    partitioned.only_in_a,
                    partitioned.only_in_b,
                    new_ext_var,
                );

                // Unify the sub tag_unions with exts
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);
            },
        }
    }

    const TagsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const GatheredTags = struct { ext: Var, range: TagSafeList.Range };

    /// Recursively unwraps the tags of an extensible tag_union, flattening all visible tags
    /// into `scratch.gathered_tags` and following through:
    /// * aliases (by chasing `.backing_var`)
    /// * tag_union extension chains (via nested `.tag_union.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered tags in `gathered_tags`
    /// * the final tail extension variable, which is either a flex var or an empty tag_union
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-tag_union type).
    fn gatherTagUnionTags(self: *Self, tag_union: TagUnion) Error!GatheredTags {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = try self.scratch.copyGatherTagsFromMultiList(
            &self.types_store.tags,
            tag_union.tags,
        );

        // then loop gathering extensible tags
        var ext_var = tag_union.ext;
        var guard = types_mod.debug.IterationGuard.init("gatherTagUnionTags");
        while (true) {
            guard.tick();
            switch (self.types_store.resolveVar(ext_var).desc.content) {
                .flex => {
                    return .{ .ext = ext_var, .range = range };
                },
                .rigid => {
                    return .{ .ext = ext_var, .range = range };
                },
                .alias => |alias| {
                    ext_var = self.types_store.getAliasBackingVar(alias);
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .tag_union => |ext_tag_union| {
                            const next_tags = self.types_store.tags.sliceRange(ext_tag_union.tags);

                            // Merge extension tags while maintaining sorted order
                            try self.scratch.mergeSortedExtensionTags(
                                &range,
                                next_tags.items(.name),
                                next_tags.items(.args),
                                self.ident_store,
                            );

                            ext_var = ext_tag_union.ext;
                        },
                        else => return .{ .ext = ext_var, .range = range },
                    }
                },
                else => return .{ .ext = ext_var, .range = range },
            }
        }
    }

    const PartitionedTags = struct {
        only_in_a: TagSafeList.Range,
        only_in_b: TagSafeList.Range,
        in_both: TwoTagsSafeList.Range,
    };

    /// Given two ranges of tag_union tags stored in `scratch.gathered_tags`, this function:
    /// * sorts both slices in-place by tag name
    /// * partitions them into three disjoint groups:
    ///     - tags only in `a`
    ///     - tags only in `b`
    ///     - tags present in both (by name)
    ///
    /// These groups are stored into dedicated scratch buffers:
    /// * `only_in_a_tags`
    /// * `only_in_b_tags`
    /// * `in_both_tags`
    ///
    /// The result is a set of ranges that can be used to slice those buffers.
    ///
    /// The caller must not mutate the field ranges between `gatherTagUnionTags` and `partitionTags`.
    fn partitionTags(
        self: *const Self,
        scratch: *Scratch,
        a_tags_range: TagSafeList.Range,
        b_tags_range: TagSafeList.Range,
    ) std.mem.Allocator.Error!PartitionedTags {
        // Sort the tags (gathering maintains partial order, but unification may create unsorted unions)
        const a_tags = scratch.gathered_tags.sliceRange(a_tags_range);
        std.mem.sort(Tag, a_tags, self, struct {
            fn less(unifier: *const Self, a: Tag, b: Tag) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.name), unifier.getTypeIdentText(b.name)) == .lt;
            }
        }.less);
        const b_tags = scratch.gathered_tags.sliceRange(b_tags_range);
        std.mem.sort(Tag, b_tags, self, struct {
            fn less(unifier: *const Self, a: Tag, b: Tag) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.name), unifier.getTypeIdentText(b.name)) == .lt;
            }
        }.less);

        // Get the start of index of the new range
        const a_tags_start: u32 = @intCast(scratch.only_in_a_tags.len());
        const b_tags_start: u32 = @intCast(scratch.only_in_b_tags.len());
        const both_tags_start: u32 = @intCast(scratch.in_both_tags.len());

        // Iterate over the tags in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_tags.len and b_i < b_tags.len) {
            const a_next = a_tags[a_i];
            const b_next = b_tags[b_i];
            const ord = std.mem.order(u8, self.getTypeIdentText(a_next.name), self.getTypeIdentText(b_next.name));
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_tags.append(scratch.gpa, TwoTags{ .a = a_next, .b = b_next });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_tags.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_tags.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_tags.len) {
            const a_next = a_tags[a_i];
            _ = try scratch.only_in_a_tags.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_tags.len) {
            const b_next = b_tags[b_i];
            _ = try scratch.only_in_b_tags.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_tags.rangeToEnd(a_tags_start),
            .only_in_b = scratch.only_in_b_tags.rangeToEnd(b_tags_start),
            .in_both = scratch.in_both_tags.rangeToEnd(both_tags_start),
        };
    }

    /// Given a list of shared tags & a list of extended tags, unify the shared tags.
    /// Then merge a new tag_union with both shared+extended tags
    fn unifySharedTags(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_tags_range: TwoTags.SafeList.Range, // Into scratch.in_both_tags
        mb_a_extended_tags: ?Tag.SafeList.Range, // Into scratch.only_in_a_tags
        mb_b_extended_tags: ?Tag.SafeList.Range, // Into scratch.only_in_b_tags
        ext: Var,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        _ = try self.scratch.unify_work_stack.append(self.scratch.gpa, .{ .shared_tags_after_children = .{
            .vars = vars.*,
            .shared_tags_range = shared_tags_range,
            .mb_a_extended_tags = mb_a_extended_tags,
            .mb_b_extended_tags = mb_b_extended_tags,
            .ext = ext,
        } });

        var shared_idx: u32 = shared_tags_range.len();
        while (shared_idx > 0) {
            shared_idx -= 1;
            const idx: TwoTagsSafeList.Idx = @enumFromInt(@intFromEnum(shared_tags_range.start) + shared_idx);
            const tags = self.scratch.in_both_tags.get(idx).*;
            if (tags.a.args.len() != tags.b.args.len()) return error.TypeMismatch;

            var arg_idx: u32 = tags.a.args.len();
            while (arg_idx > 0) {
                arg_idx -= 1;
                try self.scheduleGuardedPair(
                    self.types_store.getVarAt(tags.a.args, arg_idx),
                    self.types_store.getVarAt(tags.b.args, arg_idx),
                    .propagate,
                );
            }
        }
    }

    // constraints //

    fn unifyStaticDispatchConstraints(
        self: *Self,
        a_constraints: StaticDispatchConstraint.SafeList.Range,
        b_constraints: StaticDispatchConstraint.SafeList.Range,
    ) Error!StaticDispatchConstraint.SafeList.Range {
        const a_len = a_constraints.len();
        const b_len = b_constraints.len();

        // Early exits for empty ranges
        if (a_len == 0 and b_len == 0) {
            return .empty();
        } else if (a_len == 0 and b_len > 0) {
            return b_constraints;
        } else if (a_len > 0 and b_len == 0) {
            return a_constraints;
        }

        // Partition constraints
        const partitioned = try self.partitionStaticDispatchConstraints(a_constraints, b_constraints);

        // Unify shared constraints
        // IMPORTANT: We must use index-based iteration here, not slice-based.
        // The unifyStaticDispatchConstraint call can recursively call back into
        // unifyStaticDispatchConstraints, which appends to the same scratch buffer.
        // If that append causes reallocation, a cached slice would be invalidated.
        if (partitioned.in_both.len() > 0) {
            const in_both_start: usize = @intFromEnum(partitioned.in_both.start);
            for (0..partitioned.in_both.len()) |i| {
                // Re-fetch on each iteration since the backing array may have moved
                const two_constraints = self.scratch.in_both_static_dispatch_constraints.items.items[in_both_start + i];
                // TODO: Catch type mismatch and throw a custom error message?
                try self.unifyStaticDispatchConstraint(two_constraints.a, two_constraints.b);
            }
        }

        const top: u32 = @intCast(self.types_store.static_dispatch_constraints.len());

        // Ensure we have enough memory for the new contiguous list.
        const capacity = partitioned.in_both.len() + partitioned.only_in_a.len() + partitioned.only_in_b.len();
        try self.types_store.static_dispatch_constraints.items.ensureUnusedCapacity(
            self.types_store.gpa,
            capacity,
        );

        for (self.scratch.in_both_static_dispatch_constraints.sliceRange(partitioned.in_both)) |two_constraints| {
            var constraint = two_constraints.b;
            if (two_constraints.a.origin == .from_literal and two_constraints.b.origin == .from_literal) {
                if (mergeFromNumeralLiteralInfo(
                    two_constraints.a.origin.numeralInfo(),
                    two_constraints.b.origin.numeralInfo(),
                )) |merged| {
                    constraint.origin = .{ .from_literal = .{ .numeral = merged } };
                }
            }
            // Preserve a body-forced where-clause across unification. The bit only
            // exists on a `where_clause` origin, so it survives only by keeping that
            // origin — or by promoting a payload-less `method_call` to it (the gap the
            // sweep would otherwise miss when a same-named direct call wins the merge).
            // A `from_literal`/operator origin is left intact: its payload (numeral
            // info, binop negation) is load-bearing for defaulting and reporting, and
            // such a receiver is pinned by defaulting rather than flagged by the sweep,
            // so dropping the bit there is correct (overwriting it breaks e.g. ranges).
            {
                const a_forced = two_constraints.a.origin == .where_clause and two_constraints.a.origin.where_clause.body_required;
                const b_forced = two_constraints.b.origin == .where_clause and two_constraints.b.origin.where_clause.body_required;
                if (a_forced or b_forced) {
                    switch (constraint.origin) {
                        .where_clause => constraint.origin.where_clause.body_required = true,
                        .method_call => constraint.origin = .{ .where_clause = .{ .body_required = true } },
                        .from_literal, .desugared_binop, .desugared_unaryop => {},
                    }
                }
            }
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(constraint);
        }
        for (self.scratch.only_in_a_static_dispatch_constraints.sliceRange(partitioned.only_in_a)) |only_a| {
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(only_a);
        }
        for (self.scratch.only_in_b_static_dispatch_constraints.sliceRange(partitioned.only_in_b)) |only_b| {
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(only_b);
        }

        return self.types_store.static_dispatch_constraints.rangeToEnd(top);
    }

    /// Unify two static dispatch constraints
    fn unifyStaticDispatchConstraint(
        self: *Self,
        a_constraint: StaticDispatchConstraint,
        b_constraint: StaticDispatchConstraint,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Self-referential constraints like `a.plus : a, a -> a` are valid and expected.
        // To prevent infinite recursion when unifying them, we track visited constraint
        // function variables in the scratch visited_vars list.
        //
        // This works together with the occurs check in occurs.zig which follows constraints
        // to detect truly infinite types.

        // Check if EITHER constraint function var is already being unified (legacy mark behavior)
        if (self.hasConstraintVisitedVar(a_constraint.fn_var) or self.hasConstraintVisitedVar(b_constraint.fn_var)) {
            // Already unifying one of these constraint functions - skip to prevent infinite recursion
            return;
        }

        // Track these vars as being unified (in separate list from general unification to match legacy mark semantics)
        _ = try self.scratch.constraint_visited_vars.append(self.scratch.gpa, a_constraint.fn_var);
        _ = try self.scratch.constraint_visited_vars.append(self.scratch.gpa, b_constraint.fn_var);
        defer self.scratch.constraint_visited_vars.items.items.len -= 2;

        // Unify the constraint function types
        try self.unifyGuarded(a_constraint.fn_var, b_constraint.fn_var);
    }

    const PartitionedStaticDispatchConstraints = struct {
        only_in_a: StaticDispatchConstraint.SafeList.Range,
        only_in_b: StaticDispatchConstraint.SafeList.Range,
        in_both: TwoStaticDispatchConstraints.SafeList.Range,
    };

    /// Given two ranges of record fields stored in `scratch.gathered_fields`, this function:
    /// * sorts both slices in-place by field name
    /// * partitions them into three disjoint groups:
    ///     - fields only in `a`
    ///     - fields only in `b`
    ///     - fields present in both (by name)
    ///
    /// These groups are stored into dedicated scratch buffers:
    /// * `only_in_a_fields`
    /// * `only_in_b_fields`
    /// * `in_both_fields`
    ///
    /// The result is a set of ranges that can be used to slice those buffers.
    fn partitionStaticDispatchConstraints(
        self: *const Self,
        a_constraints_range: StaticDispatchConstraint.SafeList.Range,
        b_constraints_range: StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!PartitionedStaticDispatchConstraints {
        const scratch = self.scratch;

        // First sort the fields
        const a_constraints = self.types_store.static_dispatch_constraints.sliceRange(a_constraints_range);
        std.mem.sort(StaticDispatchConstraint, a_constraints, self, struct {
            fn less(unifier: *const Self, a: StaticDispatchConstraint, b: StaticDispatchConstraint) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.fn_name), unifier.getTypeIdentText(b.fn_name)) == .lt;
            }
        }.less);
        const b_constraints = self.types_store.static_dispatch_constraints.sliceRange(b_constraints_range);
        std.mem.sort(StaticDispatchConstraint, b_constraints, self, struct {
            fn less(unifier: *const Self, a: StaticDispatchConstraint, b: StaticDispatchConstraint) bool {
                return std.mem.order(u8, unifier.getTypeIdentText(a.fn_name), unifier.getTypeIdentText(b.fn_name)) == .lt;
            }
        }.less);

        // Get the start of index of the new range
        const a_constraints_start: u32 = @intCast(scratch.only_in_a_static_dispatch_constraints.len());
        const b_constraints_start: u32 = @intCast(scratch.only_in_b_static_dispatch_constraints.len());
        const both_constraints_start: u32 = @intCast(scratch.in_both_static_dispatch_constraints.len());

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_constraints.len and b_i < b_constraints.len) {
            const a_next = a_constraints[a_i];
            const b_next = b_constraints[b_i];
            const ord = std.mem.order(u8, self.getTypeIdentText(a_next.fn_name), self.getTypeIdentText(b_next.fn_name));
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_static_dispatch_constraints.append(scratch.gpa, TwoStaticDispatchConstraints{
                        .a = a_next,
                        .b = b_next,
                    });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_static_dispatch_constraints.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_static_dispatch_constraints.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_constraints.len) {
            const a_next = a_constraints[a_i];
            _ = try scratch.only_in_a_static_dispatch_constraints.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_constraints.len) {
            const b_next = b_constraints[b_i];
            _ = try scratch.only_in_b_static_dispatch_constraints.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_static_dispatch_constraints.rangeToEnd(a_constraints_start),
            .only_in_b = scratch.only_in_b_static_dispatch_constraints.rangeToEnd(b_constraints_start),
            .in_both = scratch.in_both_static_dispatch_constraints.rangeToEnd(both_constraints_start),
        };
    }
};

fn mergeFromNumeralLiteralInfo(
    a: ?types_mod.NumeralInfo,
    b: ?types_mod.NumeralInfo,
) ?types_mod.NumeralInfo {
    const a_info = a orelse return b;
    const b_info = b orelse return a;

    var merged = if (!numeralInfoFitsDec(a_info)) a_info else b_info;
    merged.is_negative = a_info.is_negative or b_info.is_negative;
    merged.is_fractional = a_info.is_fractional or b_info.is_fractional;
    merged.fits_dec = mergeFitsDec(a_info.fits_dec, b_info.fits_dec);
    merged.frac_requirements = mergeFracRequirements(a_info, b_info);
    return merged;
}

fn mergeFitsDec(a: ?bool, b: ?bool) ?bool {
    if (a == false or b == false) return false;
    if (a == true or b == true) return true;
    return null;
}

fn mergeFracRequirements(
    a: types_mod.NumeralInfo,
    b: types_mod.NumeralInfo,
) ?types_mod.FracRequirements {
    if (!a.is_fractional) return b.frac_requirements;
    if (!b.is_fractional) return a.frac_requirements;

    const a_req = a.frac_requirements orelse return null;
    const b_req = b.frac_requirements orelse return null;
    return a_req.unify(b_req);
}

fn numeralInfoFitsDec(info: types_mod.NumeralInfo) bool {
    if (!info.is_fractional) return true;
    if (info.fits_dec) |fits| return fits;
    const requirements = info.frac_requirements orelse return false;
    return requirements.fits_in_dec;
}

/// A list of constraint that should apply to concrete type
pub const DeferredConstraintCheck = struct {
    var_: Var,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub const SafeList = MkSafeList(@This());
};

const MismatchHandling = union(enum) {
    propagate,
    abort,
    ignore,
    set_flag: u32,
};

const SameAliasAfterArgs = struct {
    vars: ResolvedVarDescs,
    a_backing_var: Var,
    b_backing_var: Var,
    did_arg_error_flag: u32,
};

const SharedFieldsAfterChildren = struct {
    vars: ResolvedVarDescs,
    shared_fields_range: TwoRecordFieldsSafeList.Range,
    mb_a_extended_fields: ?RecordFieldSafeList.Range,
    mb_b_extended_fields: ?RecordFieldSafeList.Range,
    ext: Var,
    did_field_error_flag: u32,
};

const SharedTagsAfterChildren = struct {
    vars: ResolvedVarDescs,
    shared_tags_range: TwoTags.SafeList.Range,
    mb_a_extended_tags: ?Tag.SafeList.Range,
    mb_b_extended_tags: ?Tag.SafeList.Range,
    ext: Var,
};

const WorkFrame = union(enum) {
    mismatch_handler: MismatchHandling,
    guarded_pair: struct {
        a: Var,
        b: Var,
    },
    guard_handler: struct {
        visited_vars_len: u32,
        saved_unresolved_b: ?Var,
    },
    unify_vars: ResolvedVarDescs,
    merge: struct {
        vars: ResolvedVarDescs,
        content: Content,
    },
    merge_to_nominal: struct {
        vars: ResolvedVarDescs,
        direction: NominalDirection,
    },
    same_alias_after_args: SameAliasAfterArgs,
    shared_fields_after_children: SharedFieldsAfterChildren,
    shared_tags_after_children: SharedTagsAfterChildren,

    pub const SafeList = MkSafeList(@This());
};

/// Public helper functions for tests
pub fn partitionFields(
    ident_store: *const Ident.Store,
    qualified_module_ident: Ident.Idx,
    types_store: *types_mod.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    a_fields_range: RecordFieldSafeList.Range,
    b_fields_range: RecordFieldSafeList.Range,
) std.mem.Allocator.Error!Unifier.PartitionedRecordFields {
    var unifier = Unifier.init(ident_store, qualified_module_ident, types_store, scratch, occurs_scratch);
    return try unifier.partitionFields(scratch, a_fields_range, b_fields_range);
}

/// Partitions tags from two tag ranges for unification.
pub fn partitionTags(
    ident_store: *const Ident.Store,
    qualified_module_ident: Ident.Idx,
    types_store: *types_mod.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    a_tags_range: TagSafeList.Range,
    b_tags_range: TagSafeList.Range,
) std.mem.Allocator.Error!Unifier.PartitionedTags {
    var unifier = Unifier.init(ident_store, qualified_module_ident, types_store, scratch, occurs_scratch);
    return try unifier.partitionTags(scratch, a_tags_range, b_tags_range);
}

/// A reusable memory arena used across unification calls to avoid per-call allocations.
///
/// `Scratch` owns several typed scratch arrays, each designed to hold a specific type of
/// temporary data needed during unification. These include:
///
/// * `fresh_vars`: type variables created during a unification pass
/// * For records
///   * `gathered_fields`: accumulated record fields from recursive extensions
///   * `only_in_a_fields`, `only_in_b_fields`: disjoint fields after `partitionFields`
///   * `in_both_fields`: shared fields with matching names
/// * For tag unions
///   * `gathered_tags`: accumulated tags from recursive extensions
///   * `only_in_a_tags`, `only_in_b_tags`: disjoint tags after `partitionTags`
///   * `in_both_tags`: shared tags with matching names
/// * For occurs:
///   * `occurs_scratch`: Scratch data need by occurs
/// * For errors:
///   * `err`: Data about the error thrown
///
/// `Scratch` should be initialized once and reused for many unification runs.
/// Each call to `unify` will reset the scratch buffer at the start.
///
/// Note that while the types store uses MultiLists for record fields & tags, this
/// struct uses regular safe list. There are several reasons for this:
/// 1. We have to sort tags/fields during unification, and MultiList doesn't
///    have a great way to do this
/// 2. These allocations are freed after this stage of compilation completes, so
///    while SafeLists waste some space compared to MultiList, the cost isn't too
///    high
///
/// NOTE: Record fields and tags are merged in sorted order during gathering
/// (via mergeSortedExtensionFields/mergeSortedExtensionTags). However, unification
/// itself creates new tag unions that may not be sorted, so partitionFields/partitionTags
/// still perform a final sort. To fully eliminate these sorts, we would need to ensure
/// unifySharedTags also produces sorted output.
pub const Scratch = struct {
    const Self = @This();

    // a scratch allocator.
    gpa: std.mem.Allocator,

    // used by caller of unify
    fresh_vars: VarSafeList,

    // explicit unification work stack
    unify_work_stack: WorkFrame.SafeList,
    mismatch_flags: MkSafeList(bool),

    // records - used internal by unification
    gathered_fields: RecordFieldSafeList,
    only_in_a_fields: RecordFieldSafeList,
    only_in_b_fields: RecordFieldSafeList,
    in_both_fields: TwoRecordFieldsSafeList,

    // records - used internal by unification
    gathered_tags: TagSafeList,
    only_in_a_tags: TagSafeList,
    only_in_b_tags: TagSafeList,
    in_both_tags: TwoTagsSafeList,

    // constraints
    deferred_constraints: DeferredConstraintCheck.SafeList,
    only_in_a_static_dispatch_constraints: StaticDispatchConstraint.SafeList,
    only_in_b_static_dispatch_constraints: StaticDispatchConstraint.SafeList,
    in_both_static_dispatch_constraints: TwoStaticDispatchConstraints.SafeList,

    // occurs
    occurs_scratch: occurs.Scratch,

    // Vars currently being unified (recursion guard for self-referential types)
    visited_vars: VarSafeList,

    // Constraint function vars currently being unified (separate from visited_vars to match legacy mark behavior)
    constraint_visited_vars: VarSafeList,

    /// Init scratch
    pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        // Initial capacities are conservative estimates. Lists grow dynamically as needed.
        // These values handle common cases without reallocation:
        // - fresh_vars: 8 - most unifications create few fresh variables
        // - fields/tags: 32 - typical records/unions have fewer than 32 members
        // Future optimization: profile real codebases to tune these values.
        return .{
            .gpa = gpa,
            .fresh_vars = try VarSafeList.initCapacity(gpa, 8),
            .unify_work_stack = try WorkFrame.SafeList.initCapacity(gpa, 32),
            .mismatch_flags = try MkSafeList(bool).initCapacity(gpa, 8),
            .gathered_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_a_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_b_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .in_both_fields = try TwoRecordFieldsSafeList.initCapacity(gpa, 32),
            .gathered_tags = try TagSafeList.initCapacity(gpa, 32),
            .only_in_a_tags = try TagSafeList.initCapacity(gpa, 32),
            .only_in_b_tags = try TagSafeList.initCapacity(gpa, 32),
            .in_both_tags = try TwoTagsSafeList.initCapacity(gpa, 32),
            .deferred_constraints = try DeferredConstraintCheck.SafeList.initCapacity(gpa, 32),
            .only_in_a_static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, 32),
            .only_in_b_static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, 32),
            .in_both_static_dispatch_constraints = try TwoStaticDispatchConstraints.SafeList.initCapacity(gpa, 32),
            .occurs_scratch = try occurs.Scratch.init(gpa),
            .visited_vars = try VarSafeList.initCapacity(gpa, 16),
            .constraint_visited_vars = try VarSafeList.initCapacity(gpa, 16),
        };
    }

    /// Deinit scratch
    pub fn deinit(self: *Self) void {
        self.fresh_vars.deinit(self.gpa);
        self.unify_work_stack.deinit(self.gpa);
        self.mismatch_flags.deinit(self.gpa);
        self.gathered_fields.deinit(self.gpa);
        self.only_in_a_fields.deinit(self.gpa);
        self.only_in_b_fields.deinit(self.gpa);
        self.in_both_fields.deinit(self.gpa);
        self.gathered_tags.deinit(self.gpa);
        self.only_in_a_tags.deinit(self.gpa);
        self.only_in_b_tags.deinit(self.gpa);
        self.in_both_tags.deinit(self.gpa);
        self.deferred_constraints.deinit(self.gpa);
        self.only_in_a_static_dispatch_constraints.deinit(self.gpa);
        self.only_in_b_static_dispatch_constraints.deinit(self.gpa);
        self.in_both_static_dispatch_constraints.deinit(self.gpa);
        self.occurs_scratch.deinit();
        self.visited_vars.deinit(self.gpa);
        self.constraint_visited_vars.deinit(self.gpa);
    }

    /// Reset the scratch arrays, retaining the allocated memory
    pub fn reset(self: *Scratch) void {
        self.unify_work_stack.items.clearRetainingCapacity();
        self.mismatch_flags.items.clearRetainingCapacity();
        self.gathered_fields.items.clearRetainingCapacity();
        self.only_in_a_fields.items.clearRetainingCapacity();
        self.only_in_b_fields.items.clearRetainingCapacity();
        self.in_both_fields.items.clearRetainingCapacity();
        self.gathered_tags.items.clearRetainingCapacity();
        self.only_in_a_tags.items.clearRetainingCapacity();
        self.only_in_b_tags.items.clearRetainingCapacity();
        self.in_both_tags.items.clearRetainingCapacity();
        self.deferred_constraints.items.clearRetainingCapacity();
        self.only_in_a_static_dispatch_constraints.items.clearRetainingCapacity();
        self.only_in_b_static_dispatch_constraints.items.clearRetainingCapacity();
        self.in_both_static_dispatch_constraints.items.clearRetainingCapacity();
        self.fresh_vars.items.clearRetainingCapacity();
        self.occurs_scratch.reset();
        self.visited_vars.items.clearRetainingCapacity();
        self.constraint_visited_vars.items.clearRetainingCapacity();
    }

    // helpers //

    /// Given a multi list of record fields and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherFieldsFromMultiList(
        self: *Self,
        multi_list: *const RecordFieldSafeMultiList,
        range: RecordFieldSafeMultiList.Range,
    ) std.mem.Allocator.Error!RecordFieldSafeList.Range {
        const start_int = self.gathered_fields.len();
        const record_fields_slice = multi_list.sliceRange(range);
        for (record_fields_slice.items(.name), record_fields_slice.items(.var_)) |name, var_| {
            _ = try self.gathered_fields.append(
                self.gpa,
                RecordField{ .name = name, .var_ = var_ },
            );
        }
        return self.gathered_fields.rangeToEnd(@intCast(start_int));
    }

    /// Merge sorted extension fields into an already-sorted gathered range.
    /// Maintains sorted order and left-bias semantics (base fields take precedence).
    /// The range is updated in-place to reflect the new count.
    /// Returns whether any new fields were added.
    fn mergeSortedExtensionFields(
        self: *Self,
        range: *RecordFieldSafeList.Range,
        ext_names: []const Ident.Idx,
        ext_vars: []const Var,
        ident_store: *const Ident.Store,
    ) std.mem.Allocator.Error!void {
        std.debug.assert(ext_names.len == ext_vars.len);
        if (ext_names.len == 0) return;

        // Get current gathered fields
        const current_fields = self.gathered_fields.sliceRange(range.*);
        const current_len = current_fields.len;

        // Count how many extension fields are NOT duplicates
        var new_count: usize = 0;
        for (ext_names) |ext_name| {
            const is_dup = for (current_fields) |existing| {
                if (existing.name.eql(ext_name)) break true;
            } else false;
            if (!is_dup) new_count += 1;
        }

        if (new_count == 0) return;

        // Allocate space for merged result
        try self.gathered_fields.items.ensureUnusedCapacity(self.gpa, new_count);

        // We need to merge in-place. Strategy:
        // 1. Append all new (non-duplicate) extension fields to the end
        // 2. Then do an in-place merge of the two sorted regions

        // Append non-duplicate extension fields
        for (ext_names, ext_vars) |name, var_| {
            const is_dup = for (current_fields) |existing| {
                if (existing.name.eql(name)) break true;
            } else false;
            if (!is_dup) {
                self.gathered_fields.items.appendAssumeCapacity(RecordField{ .name = name, .var_ = var_ });
            }
        }

        // Now we have: [sorted_base | sorted_extension]
        // Do an in-place merge using the standard merge technique
        // Get the slice starting from range.start with the new total length
        const start_idx: usize = @intFromEnum(range.start);
        const total_len = current_len + new_count;
        const items = self.gathered_fields.items.items[start_idx..][0..total_len];

        // In-place merge: we have [sorted_left | sorted_right]
        // Use rotation-based merge which is O(n) for this case
        inPlaceMergeFields(items, current_len, ident_store);

        // Update range count
        range.count = @intCast(total_len);
    }

    /// Merge sorted extension tags into an already-sorted gathered range.
    /// Maintains sorted order and left-bias semantics (base tags take precedence).
    fn mergeSortedExtensionTags(
        self: *Self,
        range: *TagSafeList.Range,
        ext_names: []const Ident.Idx,
        ext_args: []const Var.SafeList.Range,
        ident_store: *const Ident.Store,
    ) std.mem.Allocator.Error!void {
        std.debug.assert(ext_names.len == ext_args.len);
        if (ext_names.len == 0) return;

        // Get current gathered tags
        const current_tags = self.gathered_tags.sliceRange(range.*);
        const current_len = current_tags.len;

        // Count how many extension tags are NOT duplicates
        var new_count: usize = 0;
        for (ext_names) |ext_name| {
            const is_dup = for (current_tags) |existing| {
                if (existing.name.eql(ext_name)) break true;
            } else false;
            if (!is_dup) new_count += 1;
        }

        if (new_count == 0) return;

        // Allocate space for merged result
        try self.gathered_tags.items.ensureUnusedCapacity(self.gpa, new_count);

        // Append non-duplicate extension tags
        for (ext_names, ext_args) |name, args| {
            const is_dup = for (current_tags) |existing| {
                if (existing.name.eql(name)) break true;
            } else false;
            if (!is_dup) {
                self.gathered_tags.items.appendAssumeCapacity(Tag{ .name = name, .args = args });
            }
        }

        // In-place merge
        const start_idx: usize = @intFromEnum(range.start);
        const total_len = current_len + new_count;
        const items = self.gathered_tags.items.items[start_idx..][0..total_len];

        inPlaceMergeTags(items, current_len, ident_store);

        // Update range count
        range.count = @intCast(total_len);
    }

    /// Given a multi list of tag and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherTagsFromMultiList(
        self: *Self,
        multi_list: *const TagSafeMultiList,
        range: TagSafeMultiList.Range,
    ) std.mem.Allocator.Error!TagSafeList.Range {
        const start_int = self.gathered_tags.len();
        const tag_slice = multi_list.sliceRange(range);
        for (tag_slice.items(.name), tag_slice.items(.args)) |ident, args| {
            _ = try self.gathered_tags.append(
                self.gpa,
                Tag{ .name = ident, .args = args },
            );
        }
        return self.gathered_tags.rangeToEnd(@intCast(start_int));
    }

    /// Exposed for tests
    pub fn appendSliceGatheredFields(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordFieldSafeList.Range {
        return try self.gathered_fields.appendSlice(self.gpa, fields);
    }

    /// Exposed for tests
    pub fn appendSliceGatheredTags(self: *Self, fields: []const Tag) std.mem.Allocator.Error!TagSafeList.Range {
        return try self.gathered_tags.appendSlice(self.gpa, fields);
    }
};

fn sameAliasIdentity(a: Alias, b: Alias) bool {
    if (!a.origin_module.eql(b.origin_module)) return false;
    if (a.source_decl.present or b.source_decl.present) {
        return a.source_decl.eql(b.source_decl);
    }
    return a.ident.ident_idx.eql(b.ident.ident_idx);
}

fn sameNominalIdentity(a: NominalType, b: NominalType) bool {
    if (!a.origin_module.eql(b.origin_module)) return false;
    const a_source_decl = a.sourceDecl();
    const b_source_decl = b.sourceDecl();
    if (a_source_decl.present or b_source_decl.present) {
        return a_source_decl.eql(b_source_decl);
    }
    return a.ident.ident_idx.eql(b.ident.ident_idx);
}

/// In-place merge of two sorted regions of record fields.
/// Given an array [left_sorted | right_sorted], produces [merged_sorted].
/// Uses insertion sort approach which is O(n*m) but efficient for small arrays.
fn inPlaceMergeFields(items: []RecordField, left_len: usize, ident_store: *const Ident.Store) void {
    if (left_len == 0 or left_len == items.len) return;

    // Simple approach: insertion sort the right portion into the left
    // This is O(n*m) but records typically have few fields
    var i = left_len;
    while (i < items.len) : (i += 1) {
        const key = items[i];
        var j = i;
        // Shift elements right until we find the correct position
        while (j > 0 and RecordField.orderByName(ident_store, items[j - 1], key) == .gt) {
            items[j] = items[j - 1];
            j -= 1;
        }
        items[j] = key;
    }
}

/// In-place merge of two sorted regions of tags.
fn inPlaceMergeTags(items: []Tag, left_len: usize, ident_store: *const Ident.Store) void {
    if (left_len == 0 or left_len == items.len) return;

    var i = left_len;
    while (i < items.len) : (i += 1) {
        const key = items[i];
        var j = i;
        while (j > 0 and Tag.orderByName(ident_store, items[j - 1], key) == .gt) {
            items[j] = items[j - 1];
            j -= 1;
        }
        items[j] = key;
    }
}

// Structural pre-filter pair classification (numeral defaulting) //
//
// This lives in THIS module because it is a theorem about this module's
// behavior: any change to the err short-circuits, the `sameNominalIdentity`
// requirement, the no-backing-unification rule, or flex-side constraint
// deferral must land in the same review diff as this classifier. Each such
// unify internal carries a pointer comment back here. Called from
// `numeralCandidateStructurallyRefuted` in Check.zig.

/// Classification of one (method position, constraint position) pair by
/// `structurallyIncompatiblePair`.
pub const StructuralPairInspection = enum {
    /// This pair proves the probe's method unify must fail.
    refuting,
    /// This pair cannot soften another pair's refutation (it either fails —
    /// which also fails the probe — or merges without spreading `.err`), but
    /// proves nothing by itself.
    safe,
    /// Outside the closed world the soundness argument covers; the whole
    /// candidate must fall through to the full probe.
    uninspectable,
};

/// Classify one (method signature position, dispatch-constraint position)
/// pair for the numeral-defaulting structural pre-filter. Pure and read-only:
/// resolves vars in `method_types` and `constraint_types` respectively,
/// mutating neither; no Check or Unifier state is consulted.
///
/// CONTRACT: returns `.refuting` only when `unify` on this pair MUST fail,
/// given the fence conditions hold across the whole constraint range — see
/// `numeralCandidateStructurallyRefuted` in Check.zig for the range-wide
/// fence (it classifies every argument/return position of EVERY constraint in
/// the range, requires matching arities, and abandons refutation for the
/// whole candidate on any `.uninspectable`).
///
/// The refutation core is ONE theorem about this module: distinct zero-arg
/// builtin nominal identities never unify (`unifyNominalType` requires
/// `sameNominalIdentity`, and there is no implicit numeric coercion). A
/// `.refuting` pair is a dispatcher position — the method side is the
/// candidate's own concrete builtin nominal — whose constraint side is a
/// CONCRETE builtin numeric nominal with a different source decl, so that
/// position's unify must throw and the probe cannot succeed.
///
/// SOUNDNESS FENCE: the theorem is decisive only if no successful earlier
/// unify in the probe can soften the mismatching pair by merging `.err` into
/// either side (this module short-circuits err-backed nominals to a
/// SUCCESSFUL err merge — see `unifyNominalType` and the `.nominal_type` arms
/// of `unifyFlatType`). The method side is freshly instantiated per
/// constraint, so its only err sources are its own positions; the constraint
/// side shares vars across the range. Both are closed off by requiring, for
/// every pair across the range, that
///   - every method position is exactly a builtin numeric nominal: builtin
///     origin, ZERO type args (so a same-identity merge unifies no argument
///     pairs), and a backing whose root content is not `.err` (this closed
///     world runs no lift path, and `unifyNominalType` never unifies
///     backings, so backings are only ever READ for the err short-circuit —
///     their root content cannot change mid-probe);
///   - every constraint position resolves to a flex var, a rigid var, or a
///     nominal whose backing root is not `.err` (terminal-or-failing shapes
///     whose pairwise unifies either fail outright — which also fails the
///     probe, consistent with refutation — or merge without creating or
///     spreading err; flex-side dispatch constraints are deferred by
///     `recordDeferredConstraint`, never fired mid-unify);
///   - arities match (so every pair above is actually the pair `unifyFunc`
///     visits) — enforced by the range-wide caller.
/// Any deviation — aliases, records, tag unions, tuples, err content,
/// polymorphic method positions, non-fn shapes — returns `.uninspectable`,
/// making the whole candidate fall through to the probe rather than weakening
/// soundness.
pub fn structurallyIncompatiblePair(
    method_types: *const types_mod.Store,
    constraint_types: *const types_mod.Store,
    candidate_source_decl: u32,
    method_pos_var: Var,
    constraint_pos_var: Var,
) StructuralPairInspection {
    // Method side: exactly a builtin numeric nominal (builtin origin, no type
    // args, backing root not `.err`). The dispatcher-position check below is
    // made per position, not assumed — but the pre-filter's EFFECTIVENESS
    // comes from builtin numeric methods being homogeneous (`T, T -> T`, e.g.
    // `Dec.plus : Dec, Dec -> Dec`): every position of such a method IS the
    // dispatcher type, so a concrete mismatch at any position refutes the
    // candidate.
    const method_content = method_types.resolveVar(method_pos_var).desc.content;
    const method_nominal = method_content.unwrapNominalType() orelse return .uninspectable;
    if (!method_nominal.originIsBuiltin()) return .uninspectable;
    const method_decl = method_nominal.sourceDeclOptional() orelse return .uninspectable;
    if (method_types.sliceNominalArgs(method_nominal).len != 0) return .uninspectable;
    const method_backing = method_types.resolveVar(method_types.getNominalBackingVar(method_nominal)).desc.content;
    if (method_backing == .err) return .uninspectable;

    // Whether this method position IS the dispatcher type C: source-decl
    // equality alone, no origin-module comparison. Both sides are
    // builtin-origin nominals (the method via the `originIsBuiltin` gate
    // above, the candidate by construction in
    // `numeralCandidateStructurallyRefuted`), there is exactly one builtin
    // module, and every builtin-origin source decl is a statement index into
    // that single module's store — so equal decls name the same declaration.
    // `copyVar` and instantiation preserve the packed source bits verbatim,
    // making this exactly the `source_decl.eql` the post-copy
    // `sameNominalIdentity` evaluates for present decls. Soundness never
    // rested on origin modules anyway: `.refuting` below also requires
    // `constraint_decl != candidate_source_decl`, hence method_decl !=
    // constraint_decl (both present, both builtin), which already forces
    // `sameNominalIdentity` — and therefore `unifyNominalType` — to fail
    // regardless of the origin idents.
    const is_dispatcher_pos = method_decl == candidate_source_decl;

    const constraint_content = constraint_types.resolveVar(constraint_pos_var).desc.content;
    switch (constraint_content) {
        // Flex merges with the concrete method position (its dispatch
        // constraints are deferred, not fired mid-probe); rigid vs concrete
        // fails the probe outright. Neither can spread `.err`.
        .flex, .rigid => return .safe,
        .structure => |flat| switch (flat) {
            .nominal_type => |constraint_nominal| {
                // An err-backed nominal unifies SUCCESSFULLY into `.err` and
                // can contaminate shared vars — outside the closed world.
                const constraint_backing = constraint_types.resolveVar(constraint_types.getNominalBackingVar(constraint_nominal)).desc.content;
                if (constraint_backing == .err) return .uninspectable;
                if (!is_dispatcher_pos) return .safe;
                if (!constraint_nominal.originIsBuiltin()) return .safe;
                const constraint_decl = constraint_nominal.sourceDeclOptional() orelse return .safe;
                if (constraint_decl != candidate_source_decl) return .refuting;
                return .safe;
            },
            else => return .uninspectable,
        },
        else => return .uninspectable,
    }
}
