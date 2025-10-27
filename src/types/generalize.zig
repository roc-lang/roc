//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
const builtin = @import("builtin");
const collections = @import("collections");

const TypesStore = @import("store.zig").Store;
const Var = @import("types.zig").Var;
const Content = @import("types.zig").Content;
const FlatType = @import("types.zig").FlatType;
const Alias = @import("types.zig").Alias;
const Func = @import("types.zig").Func;
const Record = @import("types.zig").Record;
const TagUnion = @import("types.zig").TagUnion;
const RecordField = @import("types.zig").RecordField;
const Tag = @import("types.zig").Tag;
const Num = @import("types.zig").Num;
const NominalType = @import("types.zig").NominalType;
const Tuple = @import("types.zig").Tuple;
const Rank = @import("types.zig").Rank;
const Ident = base.Ident;

/// Type to manage instantiation.
///
/// Entry point is `instantiateVar`
///
/// This type does not own any of it's fields – it's a convenience wrapper to
/// making threading it's field through all the recursive functions easier
pub const Generalizer = struct {
    // not owned
    store: *TypesStore,
    rank_adjusted_vars: std.AutoHashMap(Var, void),
    tmp_var_pool: VarPool,
    escaped_vars: std.ArrayList(EscapedVar),

    const EscapedVar = struct { var_: Var, rank: Rank };

    const Self = @This();

    // general //

    pub fn init(gpa: std.mem.Allocator, store: *TypesStore) std.mem.Allocator.Error!Self {
        return .{
            .store = store,
            .rank_adjusted_vars = std.AutoHashMap(Var, void).init(gpa),
            .tmp_var_pool = try VarPool.init(gpa),
            .escaped_vars = try std.ArrayList(EscapedVar).initCapacity(gpa, 32),
        };
    }

    /// Reset the state of the generalizer
    pub fn reset(self: *Self) void {
        self.rank_adjusted_vars.clearRetainingCapacity();
        self.tmp_var_pool.clearRetainingCapacity();
        self.escaped_vars.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
        self.rank_adjusted_vars.deinit();
        self.tmp_var_pool.deinit();
        self.escaped_vars.deinit(gpa);
    }

    pub fn generalize(self: *Self, _: std.mem.Allocator, var_pool: *VarPool, rank_to_generalize: Rank) std.mem.Allocator.Error!void {
        std.debug.assert(var_pool.current_rank == rank_to_generalize);
        const rank_to_generalize_int = @intFromEnum(rank_to_generalize);

        // Reset internal state
        self.reset();

        // Ensure the tmp pool has enough ranks
        try self.tmp_var_pool.ensureRanksThrough(rank_to_generalize);
        self.tmp_var_pool.current_rank = rank_to_generalize;

        const vars_to_generalize = var_pool.getVarsForRank(rank_to_generalize);

        // Build tmp rank table based on the vars at this level to generalize
        for (vars_to_generalize) |var_| {
            // if (!self.store.isRedirect(var_)) {
            const resolved = self.store.resolveVar(var_);
            try self.tmp_var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
        }

        // Adjust ranks such that the rank can never increase as you unwrap
        // through the structurd
        //
        // Process from lowest to highest so lower ranks are finalized first
        for (self.tmp_var_pool.slice(), 0..) |vars_at_rank, group_rank_int| {
            const group_rank: Rank = @enumFromInt(group_rank_int);
            for (vars_at_rank.items) |var_| {
                _ = try self.adjustRank(var_, group_rank, vars_to_generalize);
            }
        }

        // For ranks 0 through (rank_to_generalize - 1), move to the correct pool
        for (self.tmp_var_pool.sliceExceptCurrentRank()) |vars_at_rank| {
            // Skip redundant vars
            for (vars_at_rank.items) |var_| {
                if (!self.store.isRedirect(var_)) {
                    const resolved = self.store.resolveVar(var_);
                    // After adjustRank, the variable might have a different rank
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                }
            }
        }

        // Iterate over the rank to generalize
        for (self.tmp_var_pool.ranks.items[rank_to_generalize_int].items) |rank_var| {
            // Skip redundant vars
            if (!self.store.isRedirect(rank_var)) {
                const resolved = self.store.resolveVar(rank_var);
                if (@intFromEnum(resolved.desc.rank) < rank_to_generalize_int) {
                    // Var escaped, move to the correct pool
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                } else {
                    // Didn't escape, generalize it
                    self.store.setDescRank(resolved.desc_idx, Rank.generalized);
                }
            }
        }

        // Clear the rank we just processed
        var_pool.ranks.items[rank_to_generalize_int].clearRetainingCapacity();
    }

    // adjust rank //

    /// Adjust the rank of a type such that the rank never increase as you move deeper
    /// This way, the outermost rank is representative of of the entire structure
    fn adjustRank(self: *Self, var_: Var, group_rank: Rank, vars_to_generalize: []Var) std.mem.Allocator.Error!Rank {
        // Resolve the var
        const resolved = self.store.resolveVar(var_);

        const is_var_to_adjust = blk: {
            for (vars_to_generalize) |var_to_generalize| {
                if (var_to_generalize == resolved.var_) {
                    break :blk true;
                }
            }
            break :blk false;
        };

        if (is_var_to_adjust) {
            if (self.rank_adjusted_vars.contains(resolved.var_)) {
                return resolved.desc.rank;
            } else {

                // Add the resolved var to the list of seen vars immediately, in case
                // this is a recursive type
                _ = try self.rank_adjusted_vars.put(resolved.var_, {});

                // Get the max rank of this vars
                const max_rank = try self.adjustRankContent(resolved.desc.content, group_rank, vars_to_generalize);

                // Set the rank
                self.store.setDescRank(resolved.desc_idx, max_rank);

                return max_rank;
            }
        } else {
            const next_rank = resolved.desc.rank.min(group_rank);

            self.store.setDescRank(resolved.desc_idx, next_rank);
            return next_rank;
        }
    }

    fn adjustRankContent(self: *Self, content: Content, group_rank: Rank, vars_to_generalize: []Var) std.mem.Allocator.Error!Rank {
        return switch (content) {
            .flex, .rigid => {
                // Note that here, we do _not_ recurse into constraints. This is
                // because constraints are not part to the type per-se.
                return group_rank;
            },
            .alias => |alias| {
                var next_rank = Rank.top_level;
                var args_iter = self.store.iterAliasArgs(alias);
                while (args_iter.next()) |arg_var| {
                    next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                }
                next_rank = next_rank.max(try self.adjustRank(self.store.getAliasBackingVar(alias), group_rank, vars_to_generalize));
                return next_rank;
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .str, .empty_record, .empty_tag_union => return Rank.top_level,
                    .list_unbound => {
                        // Unbounds are special-cased: An unbound represents a
                        // flex var _at the same rank_ as the  unbound list. So,
                        // if we actually had that, it would recurse and unwrap
                        // to group_rank. So we just return that directly here.
                        return group_rank;
                    },
                    .box => |inner_var| {
                        return Rank.top_level.max(try self.adjustRank(inner_var, group_rank, vars_to_generalize));
                    },
                    .list => |inner_var| {
                        return Rank.top_level.max(try self.adjustRank(inner_var, group_rank, vars_to_generalize));
                    },
                    .tuple => |tuple| {
                        var next_rank = Rank.top_level;
                        for (self.store.sliceVars(tuple.elems)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        return next_rank;
                    },
                    .num => |num| {
                        switch (num) {
                            .num_poly => |poly_var| {
                                return Rank.top_level.max(try self.adjustRank(poly_var, group_rank, vars_to_generalize));
                            },
                            .int_poly => |poly_var| {
                                return Rank.top_level.max(try self.adjustRank(poly_var, group_rank, vars_to_generalize));
                            },
                            .frac_poly => |poly_var| {
                                return Rank.top_level.max(try self.adjustRank(poly_var, group_rank, vars_to_generalize));
                            },

                            // Unbound - optimizations like list_unbound
                            .num_unbound, .int_unbound, .frac_unbound => return group_rank,

                            // Concrete - fully determined types with no variables
                            .int_precision, .frac_precision, .num_compact => return Rank.top_level,
                        }
                    },
                    .nominal_type => |nominal| {
                        var next_rank = Rank.top_level;
                        var args_iter = self.store.iterNominalArgs(nominal);
                        while (args_iter.next()) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        next_rank = next_rank.max(try self.adjustRank(self.store.getNominalBackingVar(nominal), group_rank, vars_to_generalize));
                        return next_rank;
                    },
                    .fn_pure => |func| {
                        var next_rank = Rank.top_level;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, group_rank, vars_to_generalize));
                        return next_rank;
                    },
                    .fn_effectful => |func| {
                        var next_rank = Rank.top_level;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, group_rank, vars_to_generalize));
                        return next_rank;
                    },
                    .fn_unbound => |func| {
                        var next_rank = Rank.top_level;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, group_rank, vars_to_generalize));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, group_rank, vars_to_generalize));

                        next_rank = blk: {
                            // Unbounds are special-cased: An unbound represents a
                            // flex var _at the same rank_ as the unbound fn. So,
                            // if we actually had that, it would recurse and unwrap
                            // to group_rank. So we just return that directly here.
                            break :blk next_rank.max(group_rank);
                        };

                        return next_rank;
                    },
                    .record => |record| {
                        var next_rank = Rank.top_level;
                        for (self.store.getRecordFieldsSlice(record.fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, group_rank, vars_to_generalize));
                        }
                        next_rank = next_rank.max(try self.adjustRank(record.ext, group_rank, vars_to_generalize));
                        return next_rank;
                    },
                    .record_unbound => |record_fields| {
                        var next_rank = Rank.top_level;
                        for (self.store.getRecordFieldsSlice(record_fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, group_rank, vars_to_generalize));
                        }
                        next_rank = blk: {
                            // Unbounds are special-cased: An unbound represents a
                            // flex var _at the same rank_ as the unbound record. So,
                            // if we actually had that, it would recurse and unwrap
                            // to group_rank. So we just return that directly here.
                            break :blk next_rank.max(group_rank);
                        };
                        return next_rank;
                    },
                    .tag_union => |tag_union| {
                        var next_rank = Rank.top_level;
                        for (self.store.getTagsSlice(tag_union.tags).items(.args)) |arg_range| {
                            for (self.store.sliceVars(arg_range)) |tag_arg_var| {
                                next_rank = next_rank.max(try self.adjustRank(tag_arg_var, group_rank, vars_to_generalize));
                            }
                        }
                        next_rank = next_rank.max(try self.adjustRank(tag_union.ext, group_rank, vars_to_generalize));
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

    pub fn getVarsForRank(self: *Self, rank: Rank) []Var {
        if (builtin.mode == .Debug) {
            if (@intFromEnum(rank) > @intFromEnum(self.current_rank)) {
                std.debug.panic("trying to get vars at rank {}, but current rank is {}", .{ @intFromEnum(rank), @intFromEnum(self.current_rank) });
            }
        }
        return self.ranks.items[@intFromEnum(rank)].items;
    }
};
