//! Polymorphic Let-Binding Instantiation (Lazy, Demand-Driven)
//!
//! Follows COR's approach: specializations are discovered DURING lowering
//! and solved in a separate loop afterward.
//!
//! ## Architecture
//!
//! 1. During lowering: When e_lookup_local finds a polymorphic binding with concrete type,
//!    request_specialization() creates a fresh symbol and queues it as "needed"
//!
//! 2. After lowering: solve_specializations() loop processes the queue,
//!    re-lowering each original definition with its concrete type
//!
//! 3. The result: a mapping of (fresh_symbol → lowered_mono_expr)
//!    which is added to the MonoExprStore

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");

const ir = @import("MonoIR.zig");

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Ident = base.Ident;
const MonoSymbol = ir.MonoSymbol;

const Self = @This();

/// Represents a specialization that needs to be solved
pub const NeededSpecialization = struct {
    /// The original CIR definition expression (the lambda body)
    def_expr: CIR.Expr.Idx,

    /// The module containing this definition
    module_idx: u16,

    /// Fresh symbol to use for this specialization
    name_new: MonoSymbol,

    /// Concrete type to instantiate with
    type_var: types_mod.Var,

    /// The lowered MonoExpr result (computed lazily)
    lowered: ?ir.MonoExprId = null,
};

/// Key for the specialization cache: (module_idx, def_expr_idx, concrete_type_var)
const SpecializationKey = struct {
    module_idx: u16,
    def_expr_idx: u32,
    type_var_u16: u16,

    fn compute(module_idx: u16, def_expr_idx: u32, type_var: types_mod.Var) SpecializationKey {
        return .{
            .module_idx = module_idx,
            .def_expr_idx = def_expr_idx,
            .type_var_u16 = @truncate(@intFromEnum(type_var)),
        };
    }

    fn equals(a: SpecializationKey, b: SpecializationKey) bool {
        return a.module_idx == b.module_idx and
               a.def_expr_idx == b.def_expr_idx and
               a.type_var_u16 == b.type_var_u16;
    }
};

allocator: Allocator,
all_module_envs: []const *ModuleEnv,

/// Counter for generating fresh ident indices
next_synthetic_idx: u29,

/// List of specializations that have been requested
/// This grows as lowering discovers polymorphic usages
needed: std.ArrayList(NeededSpecialization),

/// Cache of already-specialized (key → fresh_symbol) to avoid duplicates
cache: std.AutoHashMap(u64, MonoSymbol),

/// Initialize the instantiator
pub fn init(allocator: Allocator, all_module_envs: []const *ModuleEnv) Self {
    return .{
        .allocator = allocator,
        .all_module_envs = all_module_envs,
        .next_synthetic_idx = std.math.maxInt(u29) - 1,
        .needed = std.ArrayList(NeededSpecialization).empty,
        .cache = std.AutoHashMap(u64, MonoSymbol).init(allocator),
    };
}

/// Cleanup
pub fn deinit(self: *Self) void {
    self.needed.deinit(self.allocator);
    self.cache.deinit();
}

/// Request a specialization for a polymorphic binding
///
/// Called by Lower.zig when it encounters an e_lookup_local that references
/// a polymorphic definition with a concrete type.
///
/// Returns the fresh symbol to use instead of the original symbol.
pub fn requestSpecialization(
    self: *Self,
    module_idx: u16,
    def_expr: CIR.Expr.Idx,
    type_var: types_mod.Var,
) Allocator.Error!MonoSymbol {
    // Create a u64 key for the cache
    const key: u64 = (@as(u64, module_idx) << 48) |
                     (@as(u64, @intFromEnum(def_expr)) << 16) |
                     @as(u64, @as(u16, @truncate(@intFromEnum(type_var))));

    // Check if already specialized
    if (self.cache.get(key)) |symbol| {
        return symbol;
    }

    // Create fresh symbol
    const fresh_ident = Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = self.next_synthetic_idx,
    };
    self.next_synthetic_idx -= 1;

    const fresh_symbol = MonoSymbol{
        .module_idx = module_idx,
        .ident_idx = fresh_ident,
    };

    // Cache it
    try self.cache.put(key, fresh_symbol);

    // Add to needed list
    try self.needed.append(self.allocator, .{
        .def_expr = def_expr,
        .module_idx = module_idx,
        .name_new = fresh_symbol,
        .type_var = type_var,
    });

    return fresh_symbol;
}

/// Get the next specialization that needs to be solved
///
/// Used by the post-lowering loop to process specializations.
/// Returns null when all specializations have been solved.
pub fn nextNeededSpecialization(self: *Self) ?*NeededSpecialization {
    for (self.needed.items) |*spec| {
        if (spec.lowered == null) {
            return spec;
        }
    }
    return null;
}

/// Get all solved specializations (definitions ready for codegen)
pub fn solvedSpecializations(self: *Self) []const NeededSpecialization {
    return self.needed.items;
}
