//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
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

    const Self = @This();

    // general //

    pub fn init(gpa: std.mem.Allocator, store: *TypesStore) std.mem.Allocator.Error!Self {
        return .{
            .store = store,
            .rank_adjusted_vars = std.AutoHashMap(Var, void).init(gpa),
        };
    }

    /// Reset the state of the generalizer
    pub fn reset(self: *Self) void {
        self.rank_adjusted_vars.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self) void {
        self.rank_adjusted_vars.deinit();
    }

    // generalize //

    /// Adjust the rank of a type such that the rank never increase as you move deeper
    /// This way, the outermost rank is representative of of the entire structure
    pub fn generalize(self: *Self, var_pool: *VarPool, rank_to_generalize: Rank) std.mem.Allocator.Error!void {
        // std.debug.assert(var_pool.current_rank == rank_to_generalize);

        // First, reset internal state
        self.reset();

        // Get vars at the current rank to generalize
        const vars_to_generalize = var_pool.getVarsForRank(rank_to_generalize);

        // First, adjust the rank_var's rank is representative of it's entire structure
        //
        // Note that this has to be done it it's own pass so all ranks are
        // adjusted  _before_ we generalize
        for (vars_to_generalize) |rank_var| {
            _ = try self.adjustRank(rank_var, rank_to_generalize);
        }

        // Iterate over all the variables at the rank we're trying to generalize
        for (vars_to_generalize) |rank_var| {
            // Only process non-redundant vars
            if (!self.store.isRedirect(rank_var)) {
                const resolved = self.store.resolveVar(rank_var);
                if (@intFromEnum(resolved.desc.rank) < @intFromEnum(rank_to_generalize)) {
                    // If this var is less than the current rank (meaning it
                    // escaped it's scope), move it to the correct pool
                    try var_pool.addVarToRank(resolved.var_, resolved.desc.rank);
                } else {
                    // Otherwise, generalize it
                    self.store.setDescRank(resolved.desc_idx, Rank.generalized);
                }
            }
        }
    }

    // adjust rank //

    /// Adjust the rank of a type such that the rank never increase as you move deeper
    /// This way, the outermost rank is representative of of the entire structure
    fn adjustRank(self: *Self, var_: Var, group_rank: Rank) std.mem.Allocator.Error!Rank {
        // Resolve the var
        const resolved = self.store.resolveVar(var_);

        if (self.rank_adjusted_vars.contains(resolved.var_)) {
            return group_rank;
        } else {

            // Add the resolved var to the list of seen vars immediately, in case
            // this is a recursive type
            _ = try self.rank_adjusted_vars.put(resolved.var_, {});

            // Get the max rank of this vars
            const max_rank = try self.adjustRankContent(resolved.desc.content, group_rank);

            // Set the rank
            self.store.setDescRank(resolved.desc_idx, max_rank);

            return max_rank;
        }
    }

    fn adjustRankContent(self: *Self, content: Content, group_rank: Rank) std.mem.Allocator.Error!Rank {
        return switch (content) {
            .flex, .rigid, .err => return group_rank,
            .alias => |alias| {
                var next_rank = group_rank;
                var args_iter = self.store.iterAliasArgs(alias);
                while (args_iter.next()) |arg_var| {
                    next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                }
                next_rank = next_rank.max(try self.adjustRank(self.store.getAliasBackingVar(alias), next_rank));
                return next_rank;
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .str, .empty_record, .empty_tag_union, .list_unbound => return group_rank,
                    .box => |inner_var| {
                        return group_rank.max(try self.adjustRank(inner_var, group_rank));
                    },
                    .list => |inner_var| {
                        return group_rank.max(try self.adjustRank(inner_var, group_rank));
                    },
                    .tuple => |tuple| {
                        var next_rank = group_rank;
                        for (self.store.sliceVars(tuple.elems)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                        }
                        return next_rank;
                    },
                    .num => return group_rank,
                    .nominal_type => |nominal| {
                        var next_rank = group_rank;
                        var args_iter = self.store.iterNominalArgs(nominal);
                        while (args_iter.next()) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                        }
                        next_rank = next_rank.max(try self.adjustRank(self.store.getNominalBackingVar(nominal), next_rank));
                        return next_rank;
                    },
                    .fn_pure => |func| {
                        var next_rank = group_rank;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, next_rank));
                        return next_rank;
                    },
                    .fn_effectful => |func| {
                        var next_rank = group_rank;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, next_rank));
                        return next_rank;
                    },
                    .fn_unbound => |func| {
                        var next_rank = group_rank;
                        for (self.store.sliceVars(func.args)) |arg_var| {
                            next_rank = next_rank.max(try self.adjustRank(arg_var, next_rank));
                        }
                        next_rank = next_rank.max(try self.adjustRank(func.ret, next_rank));
                        return next_rank;
                    },
                    .record => |record| {
                        var next_rank = group_rank;
                        for (self.store.getRecordFieldsSlice(record.fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, next_rank));
                        }
                        next_rank = next_rank.max(try self.adjustRank(record.ext, next_rank));
                        return next_rank;
                    },
                    .record_unbound => |record_fields| {
                        var next_rank = group_rank;
                        for (self.store.getRecordFieldsSlice(record_fields).items(.var_)) |rec_var| {
                            next_rank = next_rank.max(try self.adjustRank(rec_var, next_rank));
                        }
                        return next_rank;
                    },
                    .tag_union => |tag_union| {
                        var next_rank = group_rank;
                        for (self.store.getTagsSlice(tag_union.tags).items(.args)) |arg_range| {
                            for (self.store.sliceVars(arg_range)) |tag_arg_var| {
                                next_rank = next_rank.max(try self.adjustRank(tag_arg_var, next_rank));
                            }
                        }
                        return next_rank;
                    },
                }
            },
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
        std.debug.assert(@intFromEnum(rank) <= @intFromEnum(self.current_rank));
        try self.ranks.items[@intFromEnum(rank)].append(variable);
    }

    pub fn getVarsForRank(self: *Self, rank: Rank) []Var {
        std.debug.assert(@intFromEnum(rank) <= @intFromEnum(self.current_rank));
        return self.ranks.items[@intFromEnum(rank)].items;
    }
};
