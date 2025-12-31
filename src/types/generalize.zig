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
//! **Key idea:** A variable can only be generalized if it doesn't "escape" its scope
//! by being referenced by variables from outer (lower-ranked) scopes.
//!
//! ## Ranks
//!
//! - **Rank 0 (generalized):** Polymorphic type variables (post-generalization)
//! - **Rank 1 (top_level):** Built-in types, constants
//! - **Rank 2:** Variables introduced at the outermost let-binding
//! - **Rank 3+:** Variables introduced in nested let-bindings
//!
//! ## Example
//!
//! ```roc
//! id = |x| x                    # Can generalize: forall a. a -> a
//!
//! apply = |f, x|
//!   result = f(x)               # Cannot fully generalize
//!   result
//! ```
//!
//! When generalizing the inner `result`, we discover it references `f` and `x` from
//! the outer scope, so it "escapes" and cannot be generalized.
//!
//! ## Main entry point
//!
//! - `Generalizer.generalize()` - Generalize all variables at a given rank

const std = @import("std");
const builtin = @import("builtin");

const TypesStore = @import("store.zig").Store;
const Var = @import("types.zig").Var;
const Content = @import("types.zig").Content;
const Rank = @import("types.zig").Rank;

/// Manages the generalization process for type variables.
///
/// The Generalizer is responsible for determining which type variables at a given
/// rank can be safely generalized (made polymorphic) and which have "escaped" their
/// scope by being referenced from outer scopes.
///
/// ## Algorithm Overview
///
/// 1. **Build temporary rank table:** Copy vars at the rank we're generalizing into
///    a temporary pool for processing
///
/// 2. **Adjust ranks:** Walk through all variables and adjust their ranks to maintain
///    the invariant that ranks never increase as you go deeper into types. This phase
///    detects which variables have escaped.
///
/// 3. **Categorize variables:** After rank adjustment:
///    - If var.rank < rank_to_generalize: Variable escaped (move to lower rank pool)
///    - If var.rank == rank_to_generalize: Can generalize (set rank to .generalized)
///
/// 4. **Update pools:** Move escaped variables to their correct rank pools and set
///    generalized variables to rank .generalized
///
/// ## Entry point
///
/// - `generalize()` - Main function that performs generalization for a given rank
///
/// ## Internal state
///
/// This type holds temporary state during generalization and should be reset between
/// uses. Fields are not owned - store is borrowed, and temporary structures are reused.
pub const Generalizer = struct {
    /// Borrowed reference to the type store
    store: *TypesStore,
    /// Tracks which variables we've already adjusted (for handling recursive types)
    rank_adjusted_vars: std.AutoHashMap(Var, void),
    /// Temporary pool for processing variables during rank adjustment
    tmp_var_pool: VarPool,
    /// Map of which variables we are generalizing this pass
    vars_to_generalized: std.AutoHashMap(Var, void),

    const Self = @This();

    // general //

    pub fn init(gpa: std.mem.Allocator, store: *TypesStore) std.mem.Allocator.Error!Self {
        return .{
            .store = store,
            .tmp_var_pool = try VarPool.init(gpa),
            .rank_adjusted_vars = std.AutoHashMap(Var, void).init(gpa),
            .vars_to_generalized = std.AutoHashMap(Var, void).init(gpa),
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
    ///    var's rank based on the ranks of variables it references. This enforces the
    ///    invariant that ranks never increase as you traverse deeper into types.
    ///
    /// 3. **Separate escaped from generalizable:** After rank adjustment:
    ///    - Vars with rank < rank_to_generalize have "escaped" (reference outer vars)
    ///    - Vars with rank == rank_to_generalize can be safely generalized
    ///
    /// 4. **Update var pool:**
    ///    - Move escaped vars to their (now lower) rank pools
    ///    - Set generalizable vars to rank ∞ (Rank.generalized) if should_generalize is true
    ///    - Clear the original rank pool
    ///
    /// ## Parameters
    /// - `var_pool`: The main variable pool tracking all vars by rank
    /// - `rank_to_generalize`: The rank level to generalize (must be var_pool.current_rank)
    /// - `should_generalize`: If true, actually generalize eligible vars. If false, just
    ///   clean up the rank pool without generalizing. Only lambda expressions should
    ///   have their types generalized (value restriction).
    pub fn generalize(self: *Self, _: std.mem.Allocator, var_pool: *VarPool, rank_to_generalize: Rank, _: bool) std.mem.Allocator.Error!void {
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
            try self.vars_to_generalized.put(resolved.var_, {});
        }

        // Adjust ranks to maintain invariant: ranks never increase going deeper.
        // Process from lowest to highest rank so that lower ranks are finalized first,
        // ensuring we have accurate rank information when processing higher ranks.
        for (self.tmp_var_pool.slice(), 0..) |vars_at_rank, group_rank_int| {
            const group_rank: Rank = @enumFromInt(group_rank_int);
            for (vars_at_rank.items) |var_| {
                _ = try self.adjustRank(var_, group_rank, vars_to_generalize);
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
                if (resolved_rank_int < rank_to_generalize_int) {
                    // Escaped var, so move it to the right pool.
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                } else {
                    // Safe to generalize
                    self.store.setDescRank(resolved.desc_idx, Rank.generalized);
                }
            }
        }

        // Clear the rank we just processed from the main pool
        var_pool.ranks.items[rank_to_generalize_int].clearRetainingCapacity();
    }

    // adjust rank //

    /// Adjusts type variable ranks to prepare for generalization.
    ///
    /// This implements the rank adjustment phase of Hindley-Milner generalization.
    /// The key insight is that a type can only be generalized if all the type variables
    /// it references are also being generalized at the same time (are at the same rank).
    ///
    /// **Core Invariant:** Ranks never increase as you traverse deeper into a type structure.
    /// This means the outermost rank represents the maximum rank of the entire type,
    /// making it easy to determine which variables can be generalized.
    ///
    /// ## Two classes of variables:
    ///
    /// 1. **Variables being generalized** (in `vars_to_generalize`):
    ///    - Start at `group_rank` (the rank we're trying to generalize)
    ///    - Ranks can be INCREASED to the max rank found in their contents
    ///    - Final rank = max(group_rank, ranks of all nested variables)
    ///    - If final rank > group_rank, the variable "escaped" and cannot be generalized
    ///
    /// 2. **Other variables** (not in `vars_to_generalize`):
    ///    - Already introduced at some earlier (lower) rank
    ///    - Ranks can only be LOWERED to maintain the invariant
    ///    - Final rank = min(current_rank, group_rank)
    ///    - This ensures outer types don't incorrectly claim to be "more general" than their contents
    ///
    /// ## Example:
    /// ```
    /// let outer = |x|         # rank 1, introduces var 'x' at rank 1
    ///   let inner = |y|       # rank 2, introduces var 'y' at rank 2
    ///     (x, y)              # references 'x' from rank 1
    ///   inner
    /// ```
    /// When generalizing rank 2, we process `inner`'s type. We find it references `x`
    /// which is at rank 1 (lower/outer scope). Since `x` is NOT in vars_to_generalize
    /// for rank 2, `inner`'s effective rank becomes max(2, 1) = 2, but `x` stays at rank 1.
    /// This creates an "escape" - we cannot generalize `inner` because it captures
    /// a not-yet-generalized variable from an outer scope.
    ///
    /// ## Recursion handling:
    /// - `rank_adjusted_vars` tracks variables we've already processed to handle cycles
    /// - For recursive types like `type List a = [Nil, Cons a (List a)]`, we mark the
    ///   variable as "seen" immediately before recursing, preventing infinite loops
    fn adjustRank(self: *Self, var_: Var, group_rank: Rank, vars_to_generalize: []Var) std.mem.Allocator.Error!Rank {
        const resolved = self.store.resolveVar(var_);

        // Check if this variable is one we're trying to generalize at this rank
        const is_var_to_generalize = self.vars_to_generalized.contains(resolved.var_);

        // Early return for already-processed vars to handle recursive types
        if (is_var_to_generalize and self.rank_adjusted_vars.contains(resolved.var_)) {
            return resolved.desc.rank;
        }

        // Calculate the new rank based on whether we're generalizing this var
        const new_rank = if (is_var_to_generalize) blk: {
            // Mark as seen before recursing to handle cycles
            _ = try self.rank_adjusted_vars.put(resolved.var_, {});

            // For vars being generalized: rank INCREASES to max of nested vars
            // This allows us to detect when a variable "escapes" by referencing
            // variables from outer scopes (lower ranks)
            break :blk try self.adjustRankContent(resolved.desc.content, group_rank, vars_to_generalize);
        } else blk: {
            // For other vars: rank can only DECREASE (maintain invariant)
            // This ensures that if an outer type references an inner variable,
            // the outer type's rank is lowered to match
            break :blk resolved.desc.rank.min(group_rank);
        };

        self.store.setDescRank(resolved.desc_idx, new_rank);
        return new_rank;
    }

    fn adjustRankContent(self: *Self, content: Content, group_rank: Rank, vars_to_generalize: []Var) std.mem.Allocator.Error!Rank {
        return switch (content) {
            .flex => |_| {
                // Here, we start at group_rank (since flex should be generalized),
                // then we recurse into the constraints.
                const next_rank = group_rank;
                // for (self.store.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                //     next_rank = next_rank.max(try self.adjustRank(constraint.fn_var, group_rank, vars_to_generalize));
                // }
                return next_rank;
            },
            .rigid => |_| {
                // Here, we start at group_rank (since rigid should be generalized),
                // then we recurse into the constraints.
                const next_rank = group_rank;
                // for (self.store.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                //     next_rank = next_rank.max(try self.adjustRank(constraint.fn_var, group_rank, vars_to_generalize));
                // }
                return next_rank;
            },
            .alias => |alias| {
                // THEORY: Here, we don't need to recurse into the backing type because:
                // 1. We visit the type arguments (args)
                // 2. Anything in the RHS of the alias is either:
                //    - A reference to an arg (already visited via args)
                //    - A concrete type (adjustRankContent would resolve to outermost)
                // So traversing the backing var would be redundant.
                //
                // We use outermost as a default, as the type container itself
                // does not contribute to the rank calculation.
                var next_rank = Rank.outermost;
                var args_iter = self.store.iterAliasArgs(alias);
                while (args_iter.next()) |arg_var| {
                    next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                }
                return next_rank;
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .empty_record, .empty_tag_union => {
                        // THEORY: Empty records/tag unions never need to be generalized
                        return .outermost;
                    },
                    .tuple => |tuple| {
                        if (tuple.elems.len() > 0) {
                            const elems = self.store.sliceVars(tuple.elems);
                            var next_rank = try self.adjustRank(elems[0], group_rank, vars_to_generalize);
                            for (elems[1..]) |arg_var| {
                                next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                            }
                            return next_rank;
                        } else {
                            // THEORY: Empty tuples never need to be generalized
                            return .outermost;
                        }
                    },
                    .nominal_type => |nominal| {
                        // THEORY: Here, we don't need to recurse into the backing type because:
                        // 1. We visit the type arguments (args)
                        // 2. Anything in the RHS of the nominal type is either:
                        //    - A reference to an arg (already visited via args)
                        //    - A concrete type (adjustRankContent would resolve to outermost)
                        // So traversing the backing var would be redundant.
                        //
                        // We use outermost as a default, as the type container itself
                        // does not contribute to the rank calculation.
                        var next_rank = Rank.outermost;
                        var args_iter = self.store.iterNominalArgs(nominal);
                        while (args_iter.next()) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .fn_pure => |func| {
                        var next_rank = try self.adjustRank(func.ret, group_rank, vars_to_generalize);
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .fn_effectful => |func| {
                        var next_rank = try self.adjustRank(func.ret, group_rank, vars_to_generalize);
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .fn_unbound => |func| {
                        var next_rank = try self.adjustRank(func.ret, group_rank, vars_to_generalize);
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .record => |record| {
                        var next_rank = try self.adjustRank(record.ext, group_rank, vars_to_generalize);
                        for (self.store.getRecordFieldsSlice(record.fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .record_unbound => |record_fields| {
                        var next_rank = blk: {
                            // Unbounds are special-cased: An unbound represents a
                            // flex var _at the same rank_ as the unbound record. So,
                            // if we actually had that, it would recurse and unwrap
                            // to group_rank. So we just return that directly here.
                            break :blk group_rank;
                        };
                        for (self.store.getRecordFieldsSlice(record_fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .tag_union => |tag_union| {
                        var next_rank = try self.adjustRank(tag_union.ext, group_rank, vars_to_generalize);
                        for (self.store.getTagsSlice(tag_union.tags).items(.args)) |arg_range| {
                            for (self.store.sliceVars(arg_range)) |tag_arg_var| {
                                next_rank = next_rank.max(try self.adjustRank(tag_arg_var, group_rank, vars_to_generalize));
                            }
                        }
                        return next_rank;
                    },
                }
            },
            .err => return group_rank,
        };
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
            .current_rank = Rank.top_level,
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

    pub fn addVarToRank(self: *Self, variable: Var, rank: Rank) !void {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to add var at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        try self.ranks.items[@intFromEnum(rank)].append(variable);
    }

    pub fn addVarsToRank(self: *Self, variables: []Var, rank: Rank) !void {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to add var at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        try self.ranks.items[@intFromEnum(rank)].appendSlice(variables);
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
