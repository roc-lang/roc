//! Type generalization for Hindley-Milner type inference.
//!
//! This module implements the generalization phase of Hindley-Milner type inference,
//! which determines which type variables can be made polymorphic (generalized).
//!
//! ## Generalization Overview
//!
//! In Hindley-Milner type systems, we use "ranks" to track the scope level where
//! type variables are introduced. When we finish inferring a let-binding, we attempt
//! to generalize its type - converting concrete type variables into polymorphic ones
//! that can be instantiated differently at each use site.
//!
//! **Key insight:** Generalization is per-variable, not per-type. A type can be
//! "partially generalized" where some variables are quantified while others remain
//! as shared unification variables that escaped from outer scopes.
//!
//! ## Ranks
//!
//! - **Rank 0 (generalized):** Polymorphic type variables (post-generalization)
//! - **Rank 1 (outermost):** Top-level definitions not being generalized (value restriction)
//! - **Rank 2 (top_level):** Variables introduced at the outermost let-binding
//! - **Rank 3+:** Variables introduced in nested let-bindings
//!
//! **Invariant:** After rank adjustment, all variables in a type being generalized
//! at rank N have rank <= N. Variables with rank = N get generalized; variables
//! with rank < N escaped and remain monomorphic (shared with outer scope).
//!
//! ## Example - Partial Generalization
//!
//! ```roc
//! x = 10                    # x : α, rank 1, not generalized (value restriction)
//!
//! process = |y, _z| {       # rank 2
//!   [x, y]                  # unifies α with y's type
//! }
//! ```
//!
//! When generalizing `process` at rank 2:
//! - `y` unifies with `α` (from `x`), pulling `y` down to rank 1
//! - `_z` stays at rank 2
//! - Result: `process : ∀β. (α, β) -> List(α)`—only `_z` is generalized
//!
//! Later, `process(1.U8, "hello")` constrains the shared `α` to `U8`,
//! which also constrains `x` to `U8`.
//!
//! ## Main entry point
//! - `Generalizer.generalize()` - Generalize all variables at a given rank

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const TypesStore = @import("store.zig").Store;
const Var = @import("types.zig").Var;
const Rank = @import("types.zig").Rank;
const DescStoreIdx = @import("store.zig").DescStoreIdx;
const RecordField = @import("types.zig").RecordField;

/// The descriptor a rank frame settles once its children have reported.
const RankFill = struct {
    desc_idx: DescStoreIdx,
};

/// One suspended step of rank adjustment. A frame is pushed only after its var
/// is marked in `rank_adjusted_vars`, so a cyclic type re-reaching that var
/// short-circuits on the mark instead of descending again—the same cycle
/// termination the recursion used.
///
/// Child runs are held as slices into the type store. Rank adjustment only
/// rewrites descriptors and never appends to the store, so a run stays valid
/// across the children that suspend the frame holding it.
const RankFrame = union(enum) {
    over_args: OverArgsFrame,
    func: FuncFrame,
    record: RecordFrame,
    record_unbound: RecordUnboundFrame,
    tag_union: TagUnionFrame,
};

/// An applied container (alias, nominal type, tuple): the max over its args.
const OverArgsFrame = struct {
    fill: RankFill,
    args: []const Var,
    idx: u32 = 0,
    acc: Rank = Rank.generalized,
    /// Whether a requested child's rank is still to be folded in.
    awaiting: bool = false,
};

const FuncFrame = struct {
    fill: RankFill,
    args: []const Var,
    effect_deps: []const Var,
    ret: Var,
    idx: u32 = 0,
    acc: Rank = Rank.generalized,
    awaiting: bool = false,
    stage: enum { ret, await_ret, args, effect_deps } = .ret,
};

const RecordFrame = struct {
    fill: RankFill,
    fields: []const RecordField.Presence,
    ext: Var,
    idx: u32 = 0,
    acc: Rank = Rank.generalized,
    awaiting: bool = false,
    field_axis: enum { type_var, presence_var } = .type_var,
    stage: enum { ext, await_ext, fields } = .ext,
};

const RecordUnboundFrame = struct {
    fill: RankFill,
    fields: []const RecordField.Presence,
    idx: u32 = 0,
    acc: Rank,
    awaiting: bool = false,
    field_axis: enum { type_var, presence_var } = .type_var,
};

const TagUnionFrame = struct {
    fill: RankFill,
    tag_args: []const Var.SafeList.Range,
    ext: Var,
    tag_idx: u32 = 0,
    arg_idx: u32 = 0,
    acc: Rank = Rank.generalized,
    awaiting: bool = false,
    stage: enum { ext, await_ext, tags } = .ext,
};

/// Manages the generalization process for type variables.
///
/// The Generalizer is responsible for determining which type variables at a given
/// rank can be safely generalized (made polymorphic) and which have "escaped" their
/// scope by being referenced from outer scopes.
///
/// ## Main entry point
/// - `Generalizer.generalize()` - Generalize all variables at a given rank
pub const Generalizer = struct {
    gpa: std.mem.Allocator,
    /// Borrowed reference to the type store
    store: *TypesStore,
    /// Tracks which variables we've already adjusted (for handling recursive types)
    rank_adjusted_vars: std.AutoHashMap(Var, void),
    /// Temporary pool for processing variables during rank adjustment
    tmp_var_pool: VarPool,
    /// Map of which variables we are generalizing this pass
    vars_to_generalized: std.AutoHashMap(Var, void),
    /// Suspended steps of rank adjustment, innermost last. The walk descends
    /// on this heap stack rather than the native one, so the depth it can
    /// reach is bounded only by available memory.
    rank_frames: std.ArrayList(RankFrame),
    /// Settled child ranks, consumed by the frame that requested them.
    pending_ranks: std.ArrayList(Rank),

    const Self = @This();

    // general //

    pub fn init(gpa: std.mem.Allocator, store: *TypesStore) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .store = store,
            .tmp_var_pool = try VarPool.init(gpa),
            .rank_adjusted_vars = std.AutoHashMap(Var, void).init(gpa),
            .vars_to_generalized = std.AutoHashMap(Var, void).init(gpa),
            .rank_frames = .empty,
            .pending_ranks = .empty,
        };
    }

    /// Reset the state of the generalizer
    pub fn reset(self: *Self) void {
        self.tmp_var_pool.clearRetainingCapacity();
        self.rank_adjusted_vars.clearRetainingCapacity();
        self.vars_to_generalized.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self, _: std.mem.Allocator) void {
        self.tmp_var_pool.deinit();
        self.rank_adjusted_vars.deinit();
        self.vars_to_generalized.deinit();
        self.rank_frames.deinit(self.gpa);
        self.pending_ranks.deinit(self.gpa);
    }

    /// Performs generalization for all variables at the given rank.
    ///
    /// This is the main entry point for the generalization algorithm. It processes all
    /// type variables introduced at `rank_to_generalize` and determines which can be
    /// generalized (made polymorphic) and which have escaped to outer scopes.
    ///
    /// ## Algorithm steps:
    ///
    /// 1. **Copy to temporary pool:** Move all vars at this rank into a temporary pool
    ///    for processing, preserving their current ranks
    ///
    /// 2. **Adjust ranks:** Process vars from lowest to highest rank, adjusting each
    ///    var's rank based on the ranks of variables it references. Variables that
    ///    were unified with outer-scope variables will have their ranks lowered.
    ///
    /// 3. **Separate escaped from generalizable:** After rank adjustment, each variable
    ///    originally at rank_to_generalize either:
    ///    - Has rank < rank_to_generalize: "escaped" by referencing outer-scope vars
    ///    - Has rank == rank_to_generalize: safe to generalize
    ///
    ///    Note: rank > rank_to_generalize should never occur and would indicate a bug.
    ///
    /// 4. **Update var pool:**
    ///    - Move escaped vars to their (now lower) rank pools, where they remain
    ///      as shared unification variables with outer scope
    ///    - Set generalizable vars to rank .generalized
    ///    - Clear the original rank pool
    ///
    /// ## Parameters
    /// - `var_pool`: The main variable pool tracking all vars by rank
    /// - `rank_to_generalize`: The rank level to generalize (must be var_pool.current_rank)
    pub fn generalize(self: *Self, _: std.mem.Allocator, var_pool: *VarPool, rank_to_generalize: Rank) std.mem.Allocator.Error!void {
        if (rank_to_generalize == Rank.generalized) return;

        std.debug.assert(var_pool.current_rank == rank_to_generalize);
        const rank_to_generalize_int = @intFromEnum(rank_to_generalize);

        // Reset internal state from any previous generalization
        self.reset();

        // Prepare temporary pool to hold variables during processing
        try self.tmp_var_pool.ensureRanksThrough(rank_to_generalize);
        self.tmp_var_pool.current_rank = rank_to_generalize;

        const vars_to_generalize = var_pool.getVarsForRank(rank_to_generalize);
        try self.vars_to_generalized.ensureUnusedCapacity(@intCast(vars_to_generalize.len));

        // Copy all variables at this rank into the temporary pool, resolving redirects
        for (vars_to_generalize) |var_| {
            const resolved = self.store.resolveVar(var_);
            try self.tmp_var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
            // Only add to vars_to_generalized if not already generalized.
            // A var that was already generalized in a previous pass should not be
            // re-processed (which could incorrectly change its rank).
            if (resolved.desc.rank != .generalized) {
                try self.vars_to_generalized.put(resolved.var_, {});
            }
        }

        // Adjust ranks to maintain invariant: ranks never increase going deeper.
        // Process from lowest to highest rank so that lower ranks are finalized first,
        // ensuring we have accurate rank information when processing higher ranks.
        for (self.tmp_var_pool.slice(), 0..) |vars_at_rank, group_rank_int| {
            const group_rank: Rank = @enumFromInt(group_rank_int);
            for (vars_at_rank.items) |var_| {
                _ = try self.adjustRank(var_, group_rank);
            }
        }

        // Move variables from lower ranks (generalized through rank_to_generalize-1) back to main pool.
        // These are vars that were initially at rank_to_generalize but had their ranks
        // lowered during adjustment because they reference outer-scope variables.
        for (self.tmp_var_pool.sliceExceptCurrentRank()) |vars_at_rank| {
            for (vars_at_rank.items) |var_| {
                const resolved = self.store.resolveVar(var_);
                if (resolved.is_root) {
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                }
            }
        }

        // Process variables still at rank_to_generalize after adjustment.
        // These either escaped (rank lowered) or can be generalized (rank unchanged).
        for (self.tmp_var_pool.ranks.items[rank_to_generalize_int].items) |rank_var| {
            const resolved = self.store.resolveVar(rank_var);
            if (resolved.is_root) {
                const resolved_rank_int = @intFromEnum(resolved.desc.rank);
                // Adjustment only lowers ranks; a rank above the one being
                // generalized means a reducer broke the invariant (see line ~122).
                std.debug.assert(resolved_rank_int <= rank_to_generalize_int);
                if (resolved_rank_int < rank_to_generalize_int) {
                    // Escaped var, so move it to the right pool.
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                } else {
                    // Safe to generalize
                    try self.store.setDescRank(resolved.desc_idx, Rank.generalized);
                }
            }
        }

        // Clear the rank we just processed from the main pool
        var_pool.ranks.items[rank_to_generalize_int].clearRetainingCapacity();
    }
    // adjust rank //

    /// Adjusts type variable ranks to prepare for generalization.
    ///
    /// This implements the rank adjustment phase of Hindley-Milner
    /// generalization. The key insight is that generalization is
    /// **per-variable**, not per-type. A type can be "partially
    /// generalized"—some variables are quantified while others remain as shared
    /// unification variables that escaped from outer scopes.
    ///
    /// **Core Invariant:** Ranks never increase as you traverse deeper into a
    /// type structure. This ensures we can identify which variables originated
    /// from outer scopes.
    ///
    ///
    /// ## Two classes of variables:
    ///
    /// 1. **Variables that can be generalized** (rank == rank being generalized):
    ///    - Introduced at the current scope
    ///    - Will be quantified during generalization
    ///    - Each call site instantiates these with fresh variables
    ///
    /// 2. **Escaped variables** (rank < rank being generalized):
    ///    - Introduced at an outer scope
    ///    - NOT quantified. They remain as shared unification variables
    ///    - All references share the same variable, enabling value restriction
    ///
    /// ## Example - Partial Generalization:
    /// ```
    /// x = 10               # x : α, rank 0, not generalized (value restriction)
    ///
    /// process = |y, _z| {  # rank 1
    ///   [x, y]             # unifies α with y's type
    /// }
    /// ```
    /// When generalizing `process` at rank 1:
    /// - `y` unifies with `α` (from `x`), pulling `y` down to rank 0
    /// - `_z` stays at rank 1
    /// - Result: `process : ∀β. (α, β) -> List(α)`
    /// - Only `_z` (as `β`) is generalized; `α` is shared with `x`
    /// - The variable for the entire function type is marked as generalized
    ///   because it contains at least one generalizable variable (`β`).
    ///   During instantiation, the walk recurses into the function and only
    ///   creates fresh copies only for variables that are themselves marked
    ///   generalized.
    ///
    /// When instantiating `process`:
    /// - `β` (was `_z`) → fresh variable
    /// - `α` (was `y`/`x`) → left alone, still the original unification variable
    ///
    /// This means `process(1.U8, "hello")` constrains `x` to `U8`, and subsequent
    /// calls must respect that constraint.
    ///
    /// ## Recursion handling:
    /// - `rank_adjusted_vars` tracks variables we've already processed to handle cycles
    /// - For recursive types like `type List a = [Nil, Cons a (List a)]`, we mark the
    ///   variable as "seen" immediately before descending, preventing infinite loops
    ///
    /// The walk runs on an explicit heap worklist, so the depth it can descend
    /// is bounded only by available memory, never by the native stack.
    fn adjustRank(self: *Self, var_: Var, group_rank: Rank) std.mem.Allocator.Error!Rank {
        const frames_base = self.rank_frames.items.len;
        const values_base = self.pending_ranks.items.len;
        // A completed walk drains both buffers back to their entry length. An
        // allocation failure mid-walk can leave entries behind on buffers this
        // generalizer keeps for the next adjustment, so unwind them here.
        errdefer {
            self.rank_frames.items.len = frames_base;
            self.pending_ranks.items.len = values_base;
        }

        if (!try self.requestRank(var_, group_rank)) {
            while (self.rank_frames.items.len > frames_base) {
                const top = &self.rank_frames.items[self.rank_frames.items.len - 1];
                // A step either suspends after requesting exactly one child
                // (having already written its own resume state), or finishes
                // without requesting anything—so popping on finish always
                // removes the frame the step ran for.
                const finished = switch (top.*) {
                    .over_args => |*frame| try self.stepOverArgs(frame, group_rank),
                    .func => |*frame| try self.stepFunc(frame, group_rank),
                    .record => |*frame| try self.stepRecord(frame, group_rank),
                    .record_unbound => |*frame| try self.stepRecordUnbound(frame, group_rank),
                    .tag_union => |*frame| try self.stepTagUnion(frame, group_rank),
                };
                if (finished) {
                    self.rank_frames.items.len -= 1;
                }
            }
        }

        std.debug.assert(self.pending_ranks.items.len == values_base + 1);
        return self.pending_ranks.pop().?;
    }

    /// Adjust one var's rank: short-circuit an already-adjusted var, settle a
    /// var that is not being generalized, and otherwise mark the var as seen
    /// and either settle its rank immediately (contents with no children) or
    /// push the frame that will reduce over its children. Returns true when
    /// the resulting rank is already on the value stack; false when a frame
    /// was pushed.
    fn requestRank(self: *Self, var_: Var, group_rank: Rank) std.mem.Allocator.Error!bool {
        const resolved = self.store.resolveVar(var_);

        // Check if this variable is one we're trying to generalize at this rank
        const is_var_to_generalize = self.vars_to_generalized.contains(resolved.var_);

        // Early return for already-processed vars to handle recursive types
        if (is_var_to_generalize and self.rank_adjusted_vars.contains(resolved.var_)) {
            try self.pending_ranks.append(self.gpa, resolved.desc.rank);
            return true;
        }

        if (!is_var_to_generalize) {
            // For other vars: rank can only DECREASE (maintain invariant)
            // This ensures that if an outer type references an inner variable,
            // the outer type's rank is lowered to match
            try self.settleRank(resolved.desc_idx, resolved.desc.rank.min(group_rank));
            return true;
        }

        // Mark as seen before descending to handle cycles
        try self.rank_adjusted_vars.put(resolved.var_, {});

        // For vars being generalized: rank INCREASES to max of nested vars
        // This allows us to detect when a variable "escapes" by referencing
        // variables from outer scopes (lower ranks)
        const fill = RankFill{ .desc_idx = resolved.desc_idx };
        switch (resolved.desc.content) {
            .flex => {
                // Here, we start at group_rank (since flex should be generalized).
                // Constraints are deliberately not descended into.
                try self.settleRank(fill.desc_idx, group_rank);
                return true;
            },
            .rigid => {
                // Here, we start at group_rank (since rigid should be generalized).
                // Constraints are deliberately not descended into.
                try self.settleRank(fill.desc_idx, group_rank);
                return true;
            },
            .field_presence => {
                // A settled presence marker is ground. An unresolved presence
                // is represented by an ordinary flex/rigid var and reaches the
                // corresponding arm above.
                try self.settleRank(fill.desc_idx, .outermost);
                return true;
            },
            .err => {
                try self.settleRank(fill.desc_idx, group_rank);
                return true;
            },
            .alias => |alias| {
                // THEORY: we don't need to descend into the backing type. Everything
                // in the alias RHS is either a reference to an arg (already visited
                // via the args below) or a concrete type (which resolves to
                // `outermost` on its own, so it can't raise the rank). Traversing the
                // backing var would therefore be redundant—the rank is just the max
                // over the args.
                return try self.pushOverArgs(fill, self.store.sliceAliasArgs(alias));
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_record, .empty_tag_union => {
                    // THEORY: Empty records/tag unions never need to be generalized
                    try self.settleRank(fill.desc_idx, .outermost);
                    return true;
                },
                .tuple => |tuple| return try self.pushOverArgs(fill, self.store.sliceVars(tuple.elems)),
                .nominal_type => |nominal| {
                    // Same as .alias: don't descend into the backing type, take
                    // the max over the args.
                    return try self.pushOverArgs(fill, self.store.sliceNominalArgs(nominal));
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    try self.rank_frames.append(self.gpa, .{ .func = .{
                        .fill = fill,
                        .args = self.store.sliceVars(func.args),
                        .effect_deps = self.store.sliceVars(func.effect_deps),
                        .ret = func.ret,
                    } });
                    return false;
                },
                .record => |record| {
                    try self.rank_frames.append(self.gpa, .{ .record = .{
                        .fill = fill,
                        .fields = self.store.getRecordFieldsSlice(record.fields).items(.presence),
                        .ext = record.ext,
                    } });
                    return false;
                },
                .record_unbound => |record_fields| {
                    // Unbounds are special-cased: An unbound represents a flex
                    // var _at the same rank_ as the unbound record, which would
                    // reduce to group_rank, so that seeds the max directly.
                    try self.rank_frames.append(self.gpa, .{ .record_unbound = .{
                        .fill = fill,
                        .fields = self.store.getRecordFieldsSlice(record_fields).items(.presence),
                        .acc = group_rank,
                    } });
                    return false;
                },
                .tag_union => |tag_union| {
                    try self.rank_frames.append(self.gpa, .{ .tag_union = .{
                        .fill = fill,
                        .tag_args = self.store.getTagsSlice(tag_union.tags).items(.args),
                        .ext = tag_union.ext,
                    } });
                    return false;
                },
            },
        }
    }

    /// Rank reduction shared by applied containers (aliases, nominal types,
    /// tuples): the container contributes nothing itself, so its rank is the
    /// max over its args. Seed at `generalized` (the max-identity) and let the
    /// args raise it—seeding at `outermost` would floor the result there even
    /// when every arg is already generalized, wrongly blocking generalization of
    /// the type that uses the container. No args means a ground type, which sits
    /// at `outermost`.
    fn pushOverArgs(self: *Self, fill: RankFill, args: []const Var) std.mem.Allocator.Error!bool {
        if (args.len == 0) {
            try self.settleRank(fill.desc_idx, Rank.outermost);
            return true;
        }
        try self.rank_frames.append(self.gpa, .{ .over_args = .{ .fill = fill, .args = args } });
        return false;
    }

    /// Record a var's finished rank and hand it to the frame that asked for it.
    fn settleRank(self: *Self, desc_idx: DescStoreIdx, new_rank: Rank) std.mem.Allocator.Error!void {
        try self.store.setDescRank(desc_idx, new_rank);
        try self.pending_ranks.append(self.gpa, new_rank);
    }

    fn stepOverArgs(self: *Self, frame: *OverArgsFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        while (true) {
            if (frame.awaiting) {
                frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                frame.idx += 1;
                frame.awaiting = false;
                continue;
            }
            if (frame.idx < frame.args.len) {
                frame.awaiting = true;
                if (!try self.requestRank(frame.args[frame.idx], group_rank)) return false;
                continue;
            }
            try self.settleRank(frame.fill.desc_idx, frame.acc);
            return true;
        }
    }

    fn stepFunc(self: *Self, frame: *FuncFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .ret => {
                    frame.stage = .await_ret;
                    if (!try self.requestRank(frame.ret, group_rank)) return false;
                    continue;
                },
                .await_ret => {
                    frame.acc = self.pending_ranks.pop().?;
                    frame.stage = .args;
                },
                .args => {
                    if (frame.awaiting) {
                        frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                        frame.idx += 1;
                        frame.awaiting = false;
                        continue;
                    }
                    if (frame.idx < frame.args.len) {
                        frame.awaiting = true;
                        if (!try self.requestRank(frame.args[frame.idx], group_rank)) return false;
                        continue;
                    }
                    frame.idx = 0;
                    frame.stage = .effect_deps;
                },
                .effect_deps => {
                    if (frame.awaiting) {
                        frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                        frame.idx += 1;
                        frame.awaiting = false;
                        continue;
                    }
                    if (frame.idx < frame.effect_deps.len) {
                        frame.awaiting = true;
                        if (!try self.requestRank(frame.effect_deps[frame.idx], group_rank)) return false;
                        continue;
                    }
                    try self.settleRank(frame.fill.desc_idx, frame.acc);
                    return true;
                },
            }
        }
    }

    fn stepRecord(self: *Self, frame: *RecordFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .ext => {
                    frame.stage = .await_ext;
                    if (!try self.requestRank(frame.ext, group_rank)) return false;
                    continue;
                },
                .await_ext => {
                    frame.acc = self.pending_ranks.pop().?;
                    frame.stage = .fields;
                },
                .fields => {
                    if (frame.awaiting) {
                        frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                        frame.awaiting = false;
                        if (frame.field_axis == .type_var and frame.fields[frame.idx].presenceVar() != null) {
                            frame.field_axis = .presence_var;
                        } else {
                            frame.idx += 1;
                            frame.field_axis = .type_var;
                        }
                        continue;
                    }
                    if (frame.idx < frame.fields.len) {
                        const presence = frame.fields[frame.idx];
                        const child = switch (frame.field_axis) {
                            .type_var => presence.typeVar(),
                            .presence_var => presence.presenceVar().?,
                        };
                        frame.awaiting = true;
                        if (!try self.requestRank(child, group_rank)) return false;
                        continue;
                    }
                    try self.settleRank(frame.fill.desc_idx, frame.acc);
                    return true;
                },
            }
        }
    }

    fn stepRecordUnbound(self: *Self, frame: *RecordUnboundFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        while (true) {
            if (frame.awaiting) {
                frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                frame.awaiting = false;
                if (frame.field_axis == .type_var and frame.fields[frame.idx].presenceVar() != null) {
                    frame.field_axis = .presence_var;
                } else {
                    frame.idx += 1;
                    frame.field_axis = .type_var;
                }
                continue;
            }
            if (frame.idx < frame.fields.len) {
                const presence = frame.fields[frame.idx];
                const child = switch (frame.field_axis) {
                    .type_var => presence.typeVar(),
                    .presence_var => presence.presenceVar().?,
                };
                frame.awaiting = true;
                if (!try self.requestRank(child, group_rank)) return false;
                continue;
            }
            try self.settleRank(frame.fill.desc_idx, frame.acc);
            return true;
        }
    }

    fn stepTagUnion(self: *Self, frame: *TagUnionFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .ext => {
                    frame.stage = .await_ext;
                    if (!try self.requestRank(frame.ext, group_rank)) return false;
                    continue;
                },
                .await_ext => {
                    frame.acc = self.pending_ranks.pop().?;
                    frame.stage = .tags;
                },
                .tags => {
                    if (frame.awaiting) {
                        frame.acc = frame.acc.max(self.pending_ranks.pop().?);
                        frame.arg_idx += 1;
                        frame.awaiting = false;
                        continue;
                    }
                    if (frame.tag_idx >= frame.tag_args.len) {
                        try self.settleRank(frame.fill.desc_idx, frame.acc);
                        return true;
                    }
                    const args = self.store.sliceVars(frame.tag_args[frame.tag_idx]);
                    if (frame.arg_idx >= args.len) {
                        frame.tag_idx += 1;
                        frame.arg_idx = 0;
                        continue;
                    }
                    frame.awaiting = true;
                    if (!try self.requestRank(args[frame.arg_idx], group_rank)) return false;
                    continue;
                },
            }
        }
    }
};

const VarArrayList = std.array_list.Managed(Var);

/// A pool of variables grouped by rank, use to manage & generalize variables
/// introduced during unification
pub const VarPool = struct {
    const Self = @This();

    ranks: std.array_list.Managed(VarArrayList),
    current_rank: Rank,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
        var ranks = try std.array_list.Managed(VarArrayList).initCapacity(allocator, 16);
        for (0..16) |_| {
            ranks.appendAssumeCapacity(try VarArrayList.initCapacity(allocator, 16));
        }
        return Self{
            .ranks = ranks,
            .current_rank = Rank.generalized,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.ranks.items) |*rank| {
            rank.deinit();
        }
        self.ranks.deinit();
    }

    // Reset the var pool
    pub fn clearRetainingCapacity(self: *Self) void {
        for (self.ranks.items) |*rank| {
            rank.clearRetainingCapacity();
        }
        self.current_rank = Rank.generalized;
    }

    // Ensure the var pool has ranks up to and including `next_rank`
    pub fn ensureRanksThrough(self: *Self, next_rank: Rank) std.mem.Allocator.Error!void {
        const required_len = @intFromEnum(next_rank) + 1;
        while (self.ranks.items.len < required_len) {
            try self.ranks.append(try VarArrayList.initCapacity(self.allocator, 16));
        }
    }

    // Get a slice of ranks, up to and including the current rank
    pub fn slice(self: *Self) []const VarArrayList {
        return self.ranks.items[0 .. @intFromEnum(self.current_rank) + 1];
    }

    // Get a slice of ranks, up to, but not including, the current rank
    pub fn sliceExceptCurrentRank(self: *Self) []const VarArrayList {
        return self.ranks.items[0..@intFromEnum(self.current_rank)];
    }

    pub fn pushRank(self: *Self) std.mem.Allocator.Error!void {
        self.current_rank = self.current_rank.next();
        if (@intFromEnum(self.current_rank) >= self.ranks.items.len) {
            try self.ranks.append(try VarArrayList.initCapacity(self.allocator, 16));
        }
    }

    pub fn popRank(self: *Self) void {
        if (@intFromEnum(self.current_rank) > 0) {
            self.ranks.items[@intFromEnum(self.current_rank)].clearRetainingCapacity();
            self.current_rank = self.current_rank.prev();
        }
    }

    /// Merge another VarPool's vars into this one, rank by rank.
    /// Both pools must be at the same current_rank.
    pub fn mergeFrom(self: *Self, other: *const VarPool) std.mem.Allocator.Error!void {
        std.debug.assert(self.current_rank == other.current_rank);
        const upper = @intFromEnum(self.current_rank) + 1;
        for (0..upper) |rank_idx| {
            try self.ranks.items[rank_idx].appendSlice(other.ranks.items[rank_idx].items);
        }
    }

    pub fn addVarToRank(self: *Self, variable: Var, rank: Rank) Allocator.Error!void {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to add var at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        try self.ranks.items[@intFromEnum(rank)].append(variable);
    }

    pub fn addVarsToRank(self: *Self, variables: []Var, rank: Rank) Allocator.Error!void {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to add var at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        try self.ranks.items[@intFromEnum(rank)].appendSlice(variables);
    }

    /// Shrink the vars recorded for `rank` back to `new_len`, discarding
    /// entries appended after a speculative probe captured the length—
    /// the rollback counterpart to the `addVarToRank` calls the probe made.
    pub fn shrinkRank(self: *Self, rank: Rank, new_len: usize) void {
        std.debug.assert(@intFromEnum(rank) <= @intFromEnum(self.current_rank));
        std.debug.assert(new_len <= self.ranks.items[@intFromEnum(rank)].items.len);
        self.ranks.items[@intFromEnum(rank)].shrinkRetainingCapacity(new_len);
    }

    pub fn getVarsForRank(self: *Self, rank: Rank) []Var {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to get vars at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        return self.ranks.items[@intFromEnum(rank)].items;
    }
};

// helpers for tests //

fn mkVar(n: u32) Var {
    return @enumFromInt(n);
}

fn expectVarsEqual(actual: []Var, expected: []const Var) error{TestExpectedEqual}!void {
    try std.testing.expectEqual(expected.len, actual.len);
    for (expected, actual) |e, a| {
        try std.testing.expectEqual(e, a);
    }
}

// tests //

test "mergeFrom - merge empty into empty" {
    const gpa = std.testing.allocator;
    var pool_a = try VarPool.init(gpa);
    defer pool_a.deinit();
    var pool_b = try VarPool.init(gpa);
    defer pool_b.deinit();

    try pool_a.mergeFrom(&pool_b);

    try std.testing.expectEqual(Rank.generalized, pool_a.current_rank);
}

test "mergeFrom - vars at multiple ranks" {
    const gpa = std.testing.allocator;
    var pool_a = try VarPool.init(gpa);
    defer pool_a.deinit();
    var pool_b = try VarPool.init(gpa);
    defer pool_b.deinit();

    // Both pools at rank 3
    try pool_a.pushRank(); // 1
    try pool_a.pushRank(); // 2
    try pool_a.pushRank(); // 3
    try pool_a.addVarToRank(mkVar(1), .outermost);
    try pool_a.addVarToRank(mkVar(30), @enumFromInt(3));

    try pool_b.pushRank(); // 1
    try pool_b.pushRank(); // 2
    try pool_b.pushRank(); // 3
    try pool_b.addVarToRank(mkVar(10), .outermost);
    try pool_b.addVarToRank(mkVar(20), @enumFromInt(2));

    try pool_a.mergeFrom(&pool_b);

    try expectVarsEqual(pool_a.getVarsForRank(.outermost), &.{ mkVar(1), mkVar(10) });
    try expectVarsEqual(pool_a.getVarsForRank(@enumFromInt(2)), &.{mkVar(20)});
    try expectVarsEqual(pool_a.getVarsForRank(@enumFromInt(3)), &.{mkVar(30)});
}

// Depth pin for rank adjustment. Generalization visits every var the
// instantiator produced, and instantiation depth is bounded only by heap, so
// this walk must be too. A 40,000-element chain of tuples—a position rank
// adjustment does descend, unlike alias and nominal backing vars—is past what
// a per-node native frame can hold on any ordinary 8 MiB stack: the recursive
// walk this replaced segfaulted on exactly this chain.
test "generalize - adjusts a spine deeper than any native-stack budget" {
    const gpa = std.testing.allocator;
    const depth: u32 = 40000;

    var store = try TypesStore.initCapacity(gpa, depth + 8, 8);
    defer store.deinit();

    var pool = try VarPool.init(gpa);
    defer pool.deinit();
    try pool.pushRank();

    var gen = try Generalizer.init(gpa, &store);
    defer gen.deinit(gpa);

    const rank: Rank = .outermost;
    var chain = try std.array_list.Managed(Var).initCapacity(gpa, depth + 1);
    defer chain.deinit();

    var current = try store.freshFromContentWithRank(.{ .structure = .empty_record }, rank);
    try chain.append(current);
    for (0..depth) |_| {
        const elems = try store.appendVars(&.{current});
        current = try store.freshFromContentWithRank(
            .{ .structure = .{ .tuple = .{ .elems = elems } } },
            rank,
        );
        try chain.append(current);
    }

    // Outermost first, so the pool's first var is the one whose adjustment
    // has to walk the whole chain.
    var i = chain.items.len;
    while (i > 0) {
        i -= 1;
        try pool.addVarToRank(chain.items[i], rank);
    }

    try gen.generalize(gpa, &pool, rank);
    try std.testing.expectEqual(Rank.generalized, store.resolveVar(current).desc.rank);
}
