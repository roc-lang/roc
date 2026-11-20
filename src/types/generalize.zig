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
const base = @import("base");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const TypesStore = @import("store.zig").Store;
const Var = @import("types.zig").Var;
const Rank = @import("types.zig").Rank;
const DescStoreIdx = @import("store.zig").DescStoreIdx;
const RecordField = @import("types.zig").RecordField;
const types = @import("types.zig");

/// Whether a class is an uncommitted record tail or an uncommitted tag tail
/// (design.md "Uncommitted Record Tails", "Uncommitted Tag Tails").
pub fn isUncommittedTail(desc: types.Descriptor) bool {
    return switch (desc.content) {
        .flex => desc.flags.uncommitted_record_tail,
        .structure => |flat| flat == .empty_tag_union and desc.flags.uncommitted_tag_tail,
        .rigid, .alias, .field_presence, .err => false,
    };
}

/// The descriptor a rank frame settles once its children have reported.
const RankFill = struct {
    desc_idx: DescStoreIdx,
};

/// One suspended step of rank adjustment. A frame is pushed only after its
/// var's class is marked `rank_adjusted`, so a cyclic type re-reaching that var
/// short-circuits on the mark instead of descending again—the same cycle
/// termination the recursion used.
///
/// Child runs are held as slices into the type store. Rank adjustment only
/// rewrites descriptors and never appends to the store, so a run stays valid
/// across the children that suspend the frame holding it.
///
/// A frame's fields say which child it visits next. A step keeps them in
/// locals and writes them back only when a child suspends it, with
/// `awaiting` set: that child's rank is then on `pending_ranks` when the
/// frame is stepped again.
const RankFrame = union(enum) {
    over_args: OverArgsFrame,
    func: FuncFrame,
    record: RecordFrame,
    tag_union: TagUnionFrame,
};

/// An applied container (alias, nominal type, tuple): the max over its args.
const OverArgsFrame = struct {
    fill: RankFill,
    args: []const Var,
    idx: u32 = 0,
    acc: Rank = Rank.generalized,
    /// Whether the child at `idx` suspended the frame, so its rank is still
    /// to be folded in.
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
    stage: enum { ret, args, effect_deps } = .ret,
};

const RecordFrame = struct {
    fill: RankFill,
    fields: []const RecordField.Presence,
    ext: Var,
    idx: u32 = 0,
    acc: Rank = Rank.generalized,
    awaiting: bool = false,
    field_axis: enum { type_var, presence_var } = .type_var,
    stage: enum { ext, fields } = .ext,
};

const TagUnionFrame = struct {
    fill: RankFill,
    tag_args: []const Var.SafeList.Range,
    ext: Var,
    tag_idx: u32 = 0,
    arg_idx: u32 = 0,
    acc: Rank = Rank.generalized,
    awaiting: bool = false,
    stage: enum { ext, tags } = .ext,
};

/// A var queued for one generalization pass with the rank it started at.
const RankedVar = struct {
    /// The checked representative of the var's class.
    var_: Var,
    /// The class's descriptor. A pass merges no classes, so the descriptor
    /// names the class for the whole pass, unless the caller committed
    /// tails between `adjust` and `quantify`, which resolves the var again.
    desc_idx: DescStoreIdx,
    rank: Rank,
    /// Whether `var_` is its class's checked representative: always when
    /// queued, and for as long as `desc_idx` names its class.
    is_root: bool = true,

    fn lessThan(_: void, a: RankedVar, b: RankedVar) bool {
        return @backingInt(a.rank) < @backingInt(b.rank);
    }
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
    /// The vars at the rank being generalized, each with the rank it had when
    /// the pass began, ordered by that rank. Only ranks the pass's vars hold
    /// are visited, so a pass costs its own vars however deeply ranks nest.
    tmp_vars: std.ArrayListUnmanaged(RankedVar) = .empty,
    /// Scratch for `groupByRank`: the vars starting at the rank being
    /// generalized, in queue order.
    at_rank_vars: std.ArrayListUnmanaged(RankedVar) = .empty,
    /// Suspended steps of rank adjustment, innermost last. The walk descends
    /// on this heap stack rather than the native one, so the depth it can
    /// reach is bounded only by available memory.
    rank_frames: std.ArrayList(RankFrame),
    /// Settled child ranks, consumed by the frame that requested them.
    pending_ranks: std.ArrayList(Rank),
    /// When a walk starts in an enclosing scope, an all-generalized set of
    /// children contributes no live rank. It cannot quantify the container.
    empty_child_rank: Rank = .generalized,
    /// The uncommitted record tails the last `adjust` left at the rank it
    /// generalizes (design.md "Uncommitted Record Tails"). The caller commits
    /// those its binding may not quantify before `quantify`; the rest are
    /// quantified like any other variable the pass owns.
    owned_tails: std.ArrayListUnmanaged(Var) = .empty,
    /// How many of `tmp_vars` start below the rank the last `adjust`
    /// generalizes: they come first (`groupByRank`).
    below_rank_count: usize = 0,

    const Self = @This();

    // general //

    pub fn init(gpa: std.mem.Allocator, store: *TypesStore) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .store = store,
            .rank_frames = .empty,
            .pending_ranks = .empty,
        };
    }

    /// Orders `tmp_vars` by starting rank, keeping each rank's vars in the
    /// order they were queued, and returns how many start below the rank
    /// being generalized. Most of a pass's vars start at that rank, so those
    /// move to the back as one group and only the vars below it are sorted;
    /// nothing here scales with how deeply ranks nest.
    fn groupByRank(self: *Self, rank_to_generalize: Rank) usize {
        const items = self.tmp_vars.items;
        // `generalize` reserved room for every var.
        const at_rank_vars = self.at_rank_vars.allocatedSlice();
        std.debug.assert(self.at_rank_vars.items.len == 0 and at_rank_vars.len >= items.len);
        var below: usize = 0;
        var at_rank: usize = 0;
        for (items) |ranked| {
            // Which group a var joins is unpredictable, so it is written to
            // the end of both and only its own group grows.
            const is_at_rank = ranked.rank == rank_to_generalize;
            items[below] = ranked;
            at_rank_vars[at_rank] = ranked;
            below += @intFromBool(!is_at_rank);
            at_rank += @intFromBool(is_at_rank);
        }
        @memcpy(items[below..], at_rank_vars[0..at_rank]);
        std.sort.block(RankedVar, items[0..below], {}, RankedVar.lessThan);
        return below;
    }

    /// Reset the state of the generalizer
    pub fn reset(self: *Self) void {
        self.tmp_vars.clearRetainingCapacity();
        self.owned_tails.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self, _: std.mem.Allocator) void {
        self.tmp_vars.deinit(self.gpa);
        self.at_rank_vars.deinit(self.gpa);
        self.owned_tails.deinit(self.gpa);
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
    pub fn generalize(self: *Self, var_pool: *VarPool, rank_to_generalize: Rank) std.mem.Allocator.Error!void {
        try self.adjustMarked(var_pool, rank_to_generalize, .all);
        try self.quantify(var_pool, rank_to_generalize, false);
    }

    /// The first half of `generalize`: adjust the ranks of the vars at
    /// `rank_to_generalize`, and list in `owned_tails` the uncommitted record
    /// tails the pass would quantify. The caller may unify between this and
    /// `quantify`, so no class is left marked (`Descriptor.Word`).
    pub fn adjust(self: *Self, _: std.mem.Allocator, var_pool: *VarPool, rank_to_generalize: Rank) std.mem.Allocator.Error!void {
        try self.adjustMarked(var_pool, rank_to_generalize, .all);
        self.clearMarks();
    }

    /// Propagate ownership from enclosing scopes before a boundary decision
    /// on a flex variable. A flex keeps the boundary rank unless one of these
    /// walks reaches it. Do not settle boundary-local structures or quantify
    /// anything: the decision can add new constraints.
    pub fn adjustEscapes(self: *Self, var_pool: *VarPool) std.mem.Allocator.Error!void {
        try self.adjustMarked(var_pool, var_pool.current_rank, .enclosing);
        self.clearMarks();
    }

    fn clearMarks(self: *Self) void {
        for (self.tmp_vars.items) |ranked| {
            if (self.store.wordAt(ranked.desc_idx).marked()) self.store.clearGeneralizationMarks(ranked.desc_idx);
        }
    }

    /// `adjust`, leaving the classes it generalizes and the ones whose rank
    /// it adjusted marked for `quantify`.
    fn adjustMarked(self: *Self, var_pool: *VarPool, rank_to_generalize: Rank, comptime scope: enum { enclosing, all }) std.mem.Allocator.Error!void {
        if (rank_to_generalize == Rank.generalized) return;

        std.debug.assert(var_pool.current_rank == rank_to_generalize);
        std.debug.assert(var_pool.generalizesInnermostScope());
        const rank_to_generalize_int = @backingInt(rank_to_generalize);

        // Reset internal state from any previous generalization
        self.reset();

        const vars_to_generalize = var_pool.getVarsForRank(rank_to_generalize);
        try self.tmp_vars.ensureUnusedCapacity(self.gpa, vars_to_generalize.len);
        try self.at_rank_vars.ensureUnusedCapacity(self.gpa, vars_to_generalize.len);

        // The pass marks the classes it generalizes and the ones whose rank
        // it has adjusted in their descriptor words (`Descriptor.Word`).
        // Every class it marks is one of `tmp_vars`, and each is unmarked
        // again before the pass returns, however it returns.
        errdefer for (self.tmp_vars.items) |ranked| {
            self.store.clearGeneralizationMarks(ranked.desc_idx);
        };

        // Copy all variables at this rank into the temporary list, resolving redirects
        for (vars_to_generalize) |var_| {
            const resolved = self.store.resolveVarWord(var_);
            if (builtin.mode == .debug and @backingInt(resolved.word.rank) > rank_to_generalize_int) {
                std.debug.panic("trying to add var at rank {}, but current rank is {}", .{ @backingInt(resolved.word.rank), rank_to_generalize_int });
            }
            // No earlier pass left a mark behind, and this one has adjusted
            // no rank yet.
            std.debug.assert(!resolved.word.rank_adjusted);
            self.tmp_vars.appendAssumeCapacity(.{ .var_ = resolved.var_, .desc_idx = resolved.desc_idx, .rank = resolved.word.rank });
            // Only a var not already generalized is one this pass
            // generalizes. A var that was already generalized in a previous
            // pass should not be re-processed (which could incorrectly change
            // its rank).
            self.store.markGeneralizingIf(resolved.desc_idx, resolved.word.rank != .generalized);
        }

        // Group the vars by their starting rank, keeping each group's order.
        self.below_rank_count = self.groupByRank(rank_to_generalize);

        // Adjust ranks to maintain invariant: ranks never increase going deeper.
        // Process from lowest to highest rank so that lower ranks are finalized first,
        // ensuring we have accurate rank information when processing higher ranks.
        const starts = if (scope == .enclosing) self.tmp_vars.items[0..self.below_rank_count] else self.tmp_vars.items;
        for (starts) |ranked| {
            try self.adjustPooledRank(ranked, rank_to_generalize);
        }
        if (scope == .enclosing) return;

        self.debugAssertQueuedDescriptors();

        for (self.tmp_vars.items) |ranked| {
            const word = self.store.wordAt(ranked.desc_idx);
            if (word.kind != .flex or !word.flags().uncommitted_record_tail) continue;
            if (@backingInt(word.rank) != rank_to_generalize_int) continue;
            try self.owned_tails.append(self.gpa, ranked.var_);
        }
    }

    /// Each queued var is its class's checked representative and names the
    /// descriptor it was queued with: nothing has merged classes since.
    fn debugAssertQueuedDescriptors(self: *const Self) void {
        if (!std.debug.runtime_safety) return;
        for (self.tmp_vars.items) |ranked| {
            const resolved = self.store.resolveVarWord(ranked.var_);
            std.debug.assert(ranked.is_root and resolved.is_root and resolved.desc_idx == ranked.desc_idx);
        }
    }

    /// The second half of `generalize`: quantify the vars `adjust` left at
    /// `rank_to_generalize` and return the escaped ones to their pools. When
    /// the caller committed tails `adjust` listed (`committed_tails`), ranks
    /// are adjusted again first, so structure that held them alone escapes
    /// the quantification. Otherwise the caller has merged no classes since
    /// `adjust`, and each queued var still names the descriptor it was
    /// queued with.
    pub fn quantify(self: *Self, var_pool: *VarPool, rank_to_generalize: Rank, committed_tails: bool) std.mem.Allocator.Error!void {
        if (rank_to_generalize == Rank.generalized) return;
        const rank_to_generalize_int = @backingInt(rank_to_generalize);
        if (committed_tails) {
            // Committing a tail merges classes, so each queued var is
            // resolved again. `adjust` left no class marked: mark again the
            // classes this pass generalizes, and adjust every rank again
            // from there.
            for (self.tmp_vars.items) |*ranked| {
                const resolved = self.store.resolveVarWord(ranked.var_);
                ranked.desc_idx = resolved.desc_idx;
                ranked.is_root = resolved.is_root;
                self.store.markGeneralizingIf(resolved.desc_idx, resolved.word.rank != .generalized);
            }
            errdefer for (self.tmp_vars.items) |ranked| {
                self.store.clearGeneralizationMarks(ranked.desc_idx);
            };
            for (self.tmp_vars.items) |ranked| {
                try self.adjustPooledRank(ranked, rank_to_generalize);
            }
        } else {
            self.debugAssertQueuedDescriptors();
        }
        const current_start = self.below_rank_count;

        // Move variables from lower ranks (generalized through rank_to_generalize-1) back to main pool.
        // These are vars that were initially at rank_to_generalize but had their ranks
        // lowered during adjustment because they reference outer-scope variables.
        for (self.tmp_vars.items[0..current_start]) |ranked| {
            const word = self.store.wordAt(ranked.desc_idx);
            if (word.marked()) self.store.clearGeneralizationMarks(ranked.desc_idx);
            if (ranked.is_root) try var_pool.addVarToRank(ranked.var_, word.rank);
        }

        // Process variables still at rank_to_generalize after adjustment.
        // These either escaped (rank lowered) or can be generalized (rank unchanged).
        for (self.tmp_vars.items[current_start..]) |ranked| {
            const word = self.store.wordAt(ranked.desc_idx);
            if (word.marked()) self.store.clearGeneralizationMarks(ranked.desc_idx);
            if (!ranked.is_root) continue;
            const rank_int = @backingInt(word.rank);
            // Adjustment only lowers ranks; a rank above the one being
            // generalized means a reducer broke the invariant (see line ~122).
            std.debug.assert(rank_int <= rank_to_generalize_int);
            if (rank_int < rank_to_generalize_int) {
                // Escaped var, so move it to the right pool.
                try var_pool.addVarToRank(ranked.var_, word.rank);
            } else {
                // Safe to generalize
                try self.store.setDescRank(ranked.desc_idx, Rank.generalized);
            }
        }

        // Clear the rank we just processed from the main pool
        var_pool.ranks.items[rank_to_generalize_int].clearRetainingCapacity();
    }
    // adjust rank //

    fn adjustPooledRank(self: *Self, ranked: RankedVar, boundary: Rank) std.mem.Allocator.Error!void {
        self.empty_child_rank = if (ranked.rank == boundary) .generalized else ranked.rank;
        _ = try self.adjustRank(ranked.desc_idx, ranked.rank);
    }

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
    /// - The `rank_adjusted` mark tracks variables we've already processed to handle cycles
    /// - For recursive types like `type List a = [Nil, Cons a (List a)]`, we mark the
    ///   variable as "seen" immediately before descending, preventing infinite loops
    ///
    /// The walk runs on an explicit heap worklist, so the depth it can descend
    /// is bounded only by available memory, never by the native stack.
    fn adjustRank(self: *Self, desc_idx: DescStoreIdx, group_rank: Rank) std.mem.Allocator.Error!Rank {
        const frames_base = self.rank_frames.items.len;
        const values_base = self.pending_ranks.items.len;
        // A completed walk drains both buffers back to their entry length. An
        // allocation failure mid-walk can leave entries behind on buffers this
        // generalizer keeps for the next adjustment, so unwind them here.
        errdefer {
            self.rank_frames.items.len = frames_base;
            self.pending_ranks.items.len = values_base;
        }

        // A request pushes at most one frame, and so does a step: the
        // request that suspends it. Room for that frame is reserved before
        // each, so a step's frame stays where it is while the step runs.
        try self.rank_frames.ensureUnusedCapacity(self.gpa, 1);
        if (try self.requestDescRank(desc_idx, group_rank)) |rank| return rank;

        while (self.rank_frames.items.len > frames_base) {
            try self.rank_frames.ensureUnusedCapacity(self.gpa, 1);
            const top = &self.rank_frames.items[self.rank_frames.items.len - 1];
            // A step either suspends after requesting a child that pushed a
            // frame (having written its own resume state), or finishes
            // without leaving a frame behind—so popping on finish always
            // removes the frame the step ran for.
            const finished = switch (top.*) {
                .over_args => |*frame| try self.stepOverArgs(frame, group_rank),
                .func => |*frame| try self.stepFunc(frame, group_rank),
                .record => |*frame| try self.stepRecord(frame, group_rank),
                .tag_union => |*frame| try self.stepTagUnion(frame, group_rank),
            };
            if (finished) {
                self.rank_frames.items.len -= 1;
            }
        }

        std.debug.assert(self.pending_ranks.items.len == values_base + 1);
        return self.pending_ranks.pop().?;
    }

    /// Adjust the rank of `var_`'s class; see `requestDescRank`.
    inline fn requestRank(self: *Self, var_: Var, group_rank: Rank) std.mem.Allocator.Error!?Rank {
        return self.requestDescRank(self.store.resolveDescIdx(var_), group_rank);
    }

    /// Adjust one class's rank: short-circuit an already-adjusted class,
    /// settle a class that is not being generalized, and otherwise mark the
    /// class as seen and either settle its rank immediately (contents with
    /// no children) or push the frame that will reduce over its children.
    /// Returns the settled rank, or null when a frame was pushed: that frame
    /// puts the rank on `pending_ranks` when it finishes. The caller has
    /// reserved room for the frame.
    fn requestDescRank(self: *Self, desc_idx: DescStoreIdx, group_rank: Rank) std.mem.Allocator.Error!?Rank {
        const word = self.store.wordAt(desc_idx);

        // Check if this variable is one we're trying to generalize at this rank
        const is_var_to_generalize = word.generalizing;

        // Early return for already-processed vars to handle recursive types.
        // A var reached again while its own walk is still in progress (a
        // cycle) has not settled yet, so its descriptor still holds its
        // pre-adjustment rank, which can exceed the rank this walk is
        // settling. Checking admits cyclic graphs until the settled-state
        // occurs sweep reports them, and every cycle this walk can reach is
        // one that sweep rejects; capping the in-progress contribution at
        // `group_rank` keeps adjustment from ever raising a rank. A var that
        // already settled holds a rank no higher than `group_rank`.
        if (is_var_to_generalize and word.rank_adjusted) {
            return word.rank.min(group_rank);
        }

        if (!is_var_to_generalize) {
            // For other vars: rank can only DECREASE (maintain invariant)
            // This ensures that if an outer type references an inner variable,
            // the outer type's rank is lowered to match
            return try self.settledRank(desc_idx, word.rank.min(group_rank));
        }

        // Publish the traversal's scope before marking this node as seen.
        // A back-edge must observe the rank of this walk, not the node's
        // original inner scope: otherwise an effect-dependency cycle can
        // raise an enclosing function back to that inner scope.
        if (@backingInt(word.rank) > @backingInt(group_rank)) {
            try self.store.setDescRank(desc_idx, group_rank);
        }
        self.store.markRankAdjusted(desc_idx);

        // For vars being generalized: rank INCREASES to max of nested vars
        // This allows us to detect when a variable "escapes" by referencing
        // variables from outer scopes (lower ranks). Only a var whose
        // children the walk visits has its payload read.
        const fill = RankFill{ .desc_idx = desc_idx };
        switch (word.kind) {
            // A variable starts at group_rank, since it should be
            // generalized; constraints are deliberately not descended
            // into. An error sits at group_rank too.
            .flex, .rigid, .err => return try self.settledRank(desc_idx, group_rank),
            // A settled presence marker is ground (an unresolved presence
            // is an ordinary flex/rigid var). An uncommitted tag tail
            // generalizes like a variable, so every use of the binding
            // owns its copy (design.md "Uncommitted Tag Tails"). Every
            // other empty record or tag union is ground.
            .field_presence, .empty_record, .empty_tag_union => return try self.settledRank(desc_idx, if (word.flags().uncommitted_tag_tail) group_rank else .outermost),
            // THEORY: we don't need to descend into the backing type. Everything
            // in the alias RHS is either a reference to an arg (already visited
            // via the args below) or a concrete type (which resolves to
            // `outermost` on its own, so it can't raise the rank). Traversing the
            // backing var would therefore be redundant—the rank is just the max
            // over the args.
            .alias => return try self.pushOverArgs(fill, self.store.sliceAliasAllArgs(self.store.aliasAt(desc_idx))),
            .tuple => return try self.pushOverArgs(fill, self.store.sliceVars(self.store.tupleAt(desc_idx).elems)),
            // Same as .alias: don't descend into the backing type, take
            // the max over the args.
            .nominal_type => return try self.pushOverArgs(fill, self.store.sliceNominalArgs(self.store.nominalAt(desc_idx))),
            .fn_pure, .fn_effectful, .fn_unbound => {
                const func = self.store.funcAt(desc_idx);
                self.rank_frames.appendAssumeCapacity(.{ .func = .{
                    .fill = fill,
                    .args = self.store.sliceVars(func.args),
                    .effect_deps = self.store.sliceVars(func.effect_deps),
                    .ret = func.ret,
                } });
                return null;
            },
            .record => {
                const record = self.store.recordAt(desc_idx);
                self.rank_frames.appendAssumeCapacity(.{ .record = .{
                    .fill = fill,
                    .fields = self.store.getRecordFieldsSlice(record.fields).items(.presence),
                    .ext = record.ext,
                } });
                return null;
            },
            .tag_union => {
                const tag_union = self.store.tagUnionAt(desc_idx);
                self.rank_frames.appendAssumeCapacity(.{ .tag_union = .{
                    .fill = fill,
                    .tag_args = self.store.getTagsSlice(tag_union.tags).items(.args),
                    .ext = tag_union.ext,
                } });
                return null;
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
    fn pushOverArgs(self: *Self, fill: RankFill, args: []const Var) std.mem.Allocator.Error!?Rank {
        if (args.len == 0) return try self.settledRank(fill.desc_idx, Rank.outermost);
        self.rank_frames.appendAssumeCapacity(.{ .over_args = .{ .fill = fill, .args = args } });
        return null;
    }

    /// Record the finished rank of a class a request settles itself, and
    /// return it.
    inline fn settledRank(self: *Self, desc_idx: DescStoreIdx, new_rank: Rank) std.mem.Allocator.Error!Rank {
        try self.store.setDescRank(desc_idx, new_rank);
        return new_rank;
    }

    /// Record a frame's finished rank and hand it to the frame that
    /// requested its class.
    fn settleRank(self: *Self, desc_idx: DescStoreIdx, new_rank: Rank) std.mem.Allocator.Error!void {
        const rank = if (new_rank == .generalized) self.empty_child_rank else new_rank;
        try self.store.setDescRank(desc_idx, rank);
        try self.pending_ranks.append(self.gpa, rank);
    }

    fn stepOverArgs(self: *Self, frame: *OverArgsFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        var acc = frame.acc;
        var idx = frame.idx;
        if (frame.awaiting) {
            acc = acc.max(self.pending_ranks.pop().?);
            idx += 1;
        }
        while (idx < frame.args.len) : (idx += 1) {
            const child_rank = (try self.requestRank(frame.args[idx], group_rank)) orelse {
                frame.acc = acc;
                frame.idx = idx;
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(child_rank);
        }
        try self.settleRank(frame.fill.desc_idx, acc);
        return true;
    }

    fn stepFunc(self: *Self, frame: *FuncFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        var acc = frame.acc;
        var idx = frame.idx;
        var stage = frame.stage;
        if (frame.awaiting) {
            acc = acc.max(self.pending_ranks.pop().?);
            switch (stage) {
                .ret => stage = .args,
                .args, .effect_deps => idx += 1,
            }
        }
        if (stage == .ret) {
            const ret_rank = (try self.requestRank(frame.ret, group_rank)) orelse {
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(ret_rank);
            stage = .args;
        }
        if (stage == .args) {
            while (idx < frame.args.len) : (idx += 1) {
                const child_rank = (try self.requestRank(frame.args[idx], group_rank)) orelse {
                    frame.acc = acc;
                    frame.idx = idx;
                    frame.stage = .args;
                    frame.awaiting = true;
                    return false;
                };
                acc = acc.max(child_rank);
            }
            idx = 0;
        }
        while (idx < frame.effect_deps.len) : (idx += 1) {
            const child_rank = (try self.requestRank(frame.effect_deps[idx], group_rank)) orelse {
                frame.acc = acc;
                frame.idx = idx;
                frame.stage = .effect_deps;
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(child_rank);
        }
        try self.settleRank(frame.fill.desc_idx, acc);
        return true;
    }

    fn stepRecord(self: *Self, frame: *RecordFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        var acc = frame.acc;
        var idx = frame.idx;
        var stage = frame.stage;
        // Whether the field at `idx` has its type var settled, so its
        // presence var is next.
        var presence_next = frame.field_axis == .presence_var;
        if (frame.awaiting) {
            acc = acc.max(self.pending_ranks.pop().?);
            switch (stage) {
                .ext => stage = .fields,
                .fields => if (!presence_next and frame.fields[idx].presenceVar() != null) {
                    presence_next = true;
                } else {
                    idx += 1;
                    presence_next = false;
                },
            }
        }
        if (stage == .ext) {
            const ext_rank = (try self.requestRank(frame.ext, group_rank)) orelse {
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(ext_rank);
        }
        while (idx < frame.fields.len) : (idx += 1) {
            const presence = frame.fields[idx];
            if (!presence_next) {
                const type_rank = (try self.requestRank(presence.typeVar(), group_rank)) orelse {
                    frame.acc = acc;
                    frame.idx = idx;
                    frame.field_axis = .type_var;
                    frame.stage = .fields;
                    frame.awaiting = true;
                    return false;
                };
                acc = acc.max(type_rank);
            }
            presence_next = false;
            const presence_var = presence.presenceVar() orelse continue;
            const presence_rank = (try self.requestRank(presence_var, group_rank)) orelse {
                frame.acc = acc;
                frame.idx = idx;
                frame.field_axis = .presence_var;
                frame.stage = .fields;
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(presence_rank);
        }
        try self.settleRank(frame.fill.desc_idx, acc);
        return true;
    }

    fn stepTagUnion(self: *Self, frame: *TagUnionFrame, group_rank: Rank) std.mem.Allocator.Error!bool {
        var acc = frame.acc;
        var tag_idx = frame.tag_idx;
        var arg_idx = frame.arg_idx;
        if (frame.awaiting) {
            acc = acc.max(self.pending_ranks.pop().?);
            switch (frame.stage) {
                .ext => {},
                .tags => arg_idx += 1,
            }
        } else if (frame.stage == .ext) {
            const ext_rank = (try self.requestRank(frame.ext, group_rank)) orelse {
                frame.awaiting = true;
                return false;
            };
            acc = acc.max(ext_rank);
        }
        while (tag_idx < frame.tag_args.len) : (tag_idx += 1) {
            const args = self.store.sliceVars(frame.tag_args[tag_idx]);
            while (arg_idx < args.len) : (arg_idx += 1) {
                const child_rank = (try self.requestRank(args[arg_idx], group_rank)) orelse {
                    frame.acc = acc;
                    frame.tag_idx = tag_idx;
                    frame.arg_idx = arg_idx;
                    frame.stage = .tags;
                    frame.awaiting = true;
                    return false;
                };
                acc = acc.max(child_rank);
            }
            arg_idx = 0;
        }
        try self.settleRank(frame.fill.desc_idx, acc);
        return true;
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
    /// The innermost scopes `pushRank` entered without a rank of their own,
    /// because the scope around them was at `deepest_rank`. Each is left by
    /// the `popRank` that pairs with its push, before any scope with a rank.
    declined_scopes: u32 = 0,
    /// The deepest rank a scope gets. Always `Rank.deepest` outside tests,
    /// which lower it to reach the limit without nesting 16,382 scopes.
    deepest_rank: Rank = .deepest,

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
        self.declined_scopes = 0;
    }

    // Ensure the var pool has ranks up to and including `next_rank`
    pub fn ensureRanksThrough(self: *Self, next_rank: Rank) std.mem.Allocator.Error!void {
        const required_len = @as(usize, @backingInt(next_rank)) + 1;
        while (self.ranks.items.len < required_len) {
            try self.ranks.append(try VarArrayList.initCapacity(self.allocator, 16));
        }
    }

    // Get a slice of ranks, up to and including the current rank
    pub fn slice(self: *Self) []const VarArrayList {
        return self.ranks.items[0 .. @as(usize, @backingInt(self.current_rank)) + 1];
    }

    /// Enter a nested generalization scope at the next rank. A scope entered
    /// inside one at `deepest_rank` gets no rank of its own: it is checked at
    /// the enclosing rank, and `generalizesInnermostScope` is false until
    /// the matching `popRank`. Reporting that is the caller's job.
    pub fn pushRank(self: *Self) std.mem.Allocator.Error!void {
        if (self.current_rank == self.deepest_rank) {
            self.declined_scopes += 1;
            return;
        }
        self.current_rank = self.current_rank.next();
        if (@backingInt(self.current_rank) >= self.ranks.items.len) {
            try self.ranks.append(try VarArrayList.initCapacity(self.allocator, 16));
        }
    }

    /// Whether the innermost scope entered has a rank of its own, so leaving
    /// it generalizes that rank.
    pub fn generalizesInnermostScope(self: *const Self) bool {
        return self.declined_scopes == 0;
    }

    pub fn popRank(self: *Self) void {
        if (self.declined_scopes != 0) {
            self.declined_scopes -= 1;
            return;
        }
        if (@backingInt(self.current_rank) > 0) {
            self.ranks.items[@backingInt(self.current_rank)].clearRetainingCapacity();
            self.current_rank = self.current_rank.prev();
        }
    }

    /// Merge another VarPool's vars into this one, rank by rank.
    /// Both pools must be at the same current_rank.
    pub fn mergeFrom(self: *Self, other: *const VarPool) std.mem.Allocator.Error!void {
        std.debug.assert(self.current_rank == other.current_rank);
        const upper = @as(usize, @backingInt(self.current_rank)) + 1;
        for (0..upper) |rank_idx| {
            try self.ranks.items[rank_idx].appendSlice(other.ranks.items[rank_idx].items);
        }
    }

    pub fn addVarToRank(self: *Self, variable: Var, rank: Rank) Allocator.Error!void {
        if (builtin.mode == .debug) {
            if (@backingInt(rank) > @backingInt(self.current_rank)) {
                base.invariant("trying to add var at rank {}, but current rank is {}", .{ @backingInt(rank), @backingInt(self.current_rank) });
            }
        }
        try self.ranks.items[@backingInt(rank)].append(variable);
    }

    /// Shrink the vars recorded for `rank` back to `new_len`, discarding
    /// entries appended after a speculative probe captured the length—
    /// the rollback counterpart to the `addVarToRank` calls the probe made.
    pub fn shrinkRank(self: *Self, rank: Rank, new_len: usize) void {
        std.debug.assert(@backingInt(rank) <= @backingInt(self.current_rank));
        std.debug.assert(new_len <= self.ranks.items[@backingInt(rank)].items.len);
        self.ranks.items[@backingInt(rank)].shrinkRetainingCapacity(new_len);
    }

    pub fn getVarsForRank(self: *Self, rank: Rank) []Var {
        if (builtin.mode == .debug) {
            if (@backingInt(rank) > @backingInt(self.current_rank)) {
                base.invariant("trying to get vars at rank {}, but current rank is {}", .{ @backingInt(rank), @backingInt(self.current_rank) });
            }
        }
        return self.ranks.items[@backingInt(rank)].items;
    }
};

// helpers for tests //

fn mkVar(n: u32) Var {
    return @fromBackingInt(@intCast(n));
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
    try pool_a.addVarToRank(mkVar(30), @fromBackingInt(@intCast(3)));

    try pool_b.pushRank(); // 1
    try pool_b.pushRank(); // 2
    try pool_b.pushRank(); // 3
    try pool_b.addVarToRank(mkVar(10), .outermost);
    try pool_b.addVarToRank(mkVar(20), @fromBackingInt(@intCast(2)));

    try pool_a.mergeFrom(&pool_b);

    try expectVarsEqual(pool_a.getVarsForRank(.outermost), &.{ mkVar(1), mkVar(10) });
    try expectVarsEqual(pool_a.getVarsForRank(@fromBackingInt(@intCast(2))), &.{mkVar(20)});
    try expectVarsEqual(pool_a.getVarsForRank(@fromBackingInt(@intCast(3))), &.{mkVar(30)});
}

test "pushRank - a scope inside one at the deepest rank shares that rank until its pop" {
    const gpa = std.testing.allocator;
    var pool = try VarPool.init(gpa);
    defer pool.deinit();
    pool.deepest_rank = @fromBackingInt(2);

    try pool.pushRank(); // 1
    try pool.pushRank(); // 2
    try std.testing.expect(pool.generalizesInnermostScope());
    try pool.pushRank(); // declined
    try pool.pushRank(); // declined
    try std.testing.expectEqual(@as(Rank, @fromBackingInt(2)), pool.current_rank);
    try std.testing.expect(!pool.generalizesInnermostScope());
    pool.popRank();
    try std.testing.expect(!pool.generalizesInnermostScope());
    pool.popRank();
    try std.testing.expect(pool.generalizesInnermostScope());
    try std.testing.expectEqual(@as(Rank, @fromBackingInt(2)), pool.current_rank);
    pool.popRank();
    try std.testing.expectEqual(Rank.outermost, pool.current_rank);
}

test "Rank - the scope inside the deepest rank is the deepest rank" {
    try std.testing.expectEqual(Rank.deepest, Rank.deepest.innerScope());
    try std.testing.expectEqual(@as(Rank, @fromBackingInt(2)), Rank.outermost.innerScope());
}

test "issue 13204 - structural rank adjustment does not quantify an escaped container" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 16, 8);
    defer store.deinit();
    var pool = try VarPool.init(gpa);
    defer pool.deinit();
    try pool.pushRank();
    try pool.pushRank();
    var gen = try Generalizer.init(gpa, &store);
    defer gen.deinit(gpa);

    // Both containers have the same child. Only one container belongs to
    // the boundary; the other's slot is shared with an enclosing scope.
    const child = try store.freshFromContentWithRank(.{ .flex = types.Flex.init() }, .generalized);
    const tuple: types.Content = .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{child}) } } };
    const escaped = try store.freshFromContentWithRank(tuple, .outermost);
    const owned = try store.freshFromContentWithRank(tuple, pool.current_rank);
    try pool.addVarToRank(escaped, .outermost);
    try pool.addVarToRank(escaped, pool.current_rank);
    try pool.addVarToRank(owned, pool.current_rank);

    try gen.generalize(&pool, pool.current_rank);
    try std.testing.expectEqual(Rank.outermost, store.resolveVar(escaped).desc.rank);
    try std.testing.expectEqual(Rank.generalized, store.resolveVar(owned).desc.rank);
}

test "generalize - effect cycles inherit the enclosing traversal rank" {
    const gpa = std.testing.allocator;
    const outer: Rank = @fromBackingInt(@intCast(2));
    const inner: Rank = @fromBackingInt(@intCast(3));

    for ([_]usize{ 1, 2, 32 }) |depth| {
        var store = try TypesStore.initCapacity(gpa, 64, 8);
        defer store.deinit();
        var pool = try VarPool.init(gpa);
        defer pool.deinit();
        try pool.pushRank();
        try pool.pushRank();
        try pool.pushRank();
        var gen = try Generalizer.init(gpa, &store);
        defer gen.deinit(gpa);

        const captured = try store.freshFromContentWithRank(.{ .flex = @import("types.zig").Flex.init() }, outer);
        const result = try store.freshFromContentWithRank(.{ .flex = @import("types.zig").Flex.init() }, inner);
        const independent = try store.freshFromContentWithRank(.{ .flex = @import("types.zig").Flex.init() }, inner);
        const cycle = try gpa.alloc(Var, depth);
        defer gpa.free(cycle);
        for (cycle) |*node| node.* = try store.freshFromContentWithRank(.{ .flex = @import("types.zig").Flex.init() }, inner);
        for (cycle, 0..) |node, i| {
            try store.setVarContent(node, try store.mkFuncUnboundWithEffectDeps(
                &.{},
                result,
                &.{cycle[(i + 1) % depth]},
            ));
            try pool.addVarToRank(node, inner);
        }
        try store.setVarContent(captured, try store.mkFuncUnboundWithEffectDeps(&.{}, result, &.{cycle[0]}));
        // The captured root was unified into this pool from an outer scope.
        try pool.addVarToRank(captured, outer);
        try pool.addVarToRank(captured, inner);
        try pool.addVarToRank(result, inner);
        try pool.addVarToRank(independent, inner);

        try gen.generalize(&pool, inner);
        try std.testing.expectEqual(outer, store.resolveVar(captured).desc.rank);
        try std.testing.expectEqual(outer, store.resolveVar(result).desc.rank);
        for (cycle) |node| try std.testing.expectEqual(outer, store.resolveVar(node).desc.rank);
        try std.testing.expectEqual(Rank.generalized, store.resolveVar(independent).desc.rank);

        pool.popRank();
        try gen.generalize(&pool, outer);
        try std.testing.expectEqual(Rank.generalized, store.resolveVar(captured).desc.rank);
        try std.testing.expectEqual(Rank.generalized, store.resolveVar(result).desc.rank);
    }
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

    try gen.generalize(&pool, rank);
    try std.testing.expectEqual(Rank.generalized, store.resolveVar(current).desc.rank);
}
