//! Monomorphizer - Specializes polymorphic functions to concrete types
//!
//! This module implements the monomorphization pass which:
//! 1. Finds all polymorphic function definitions
//! 2. Identifies call sites with concrete types
//! 3. Creates specialized versions of functions for each concrete type
//!
//! Following the Cor approach, monomorphization happens BEFORE lambda set
//! inference, so this module focuses on type-based specialization only.

const std = @import("std");
const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const RocEmitter = @import("RocEmitter.zig");

const Self = @This();

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR
module_env: *ModuleEnv,

/// The type store for looking up concrete types
types_store: *types.Store,

/// Map from (original_name, concrete_type_hash) -> specialized_name
specializations: std.AutoHashMap(SpecializationKey, base.Ident.Idx),

/// Counter for generating unique specialization names
specialization_counter: u32,

/// Key for looking up specializations
pub const SpecializationKey = struct {
    original_ident: base.Ident.Idx,
    type_hash: u64,
};

/// Initialize the monomorphizer
pub fn init(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    types_store: *types.Store,
) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .types_store = types_store,
        .specializations = std.AutoHashMap(SpecializationKey, base.Ident.Idx).init(allocator),
        .specialization_counter = 0,
    };
}

/// Free resources
pub fn deinit(self: *Self) void {
    self.specializations.deinit();
}

/// Check if a type variable represents a polymorphic type
pub fn isPolymorphic(self: *Self, type_var: types.Var) bool {
    const resolved = self.types_store.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .flex, .rigid => true,
        .structure, .alias, .recursion_var, .err => false,
    };
}

/// Get a hash for a concrete type (for use as specialization key)
pub fn typeHash(self: *Self, type_var: types.Var) u64 {
    const resolved = self.types_store.resolveVar(type_var);
    // Simple hash based on the type's rank and content tag
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(std.mem.asBytes(&resolved.desc.rank));

    // Add more detail based on content
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            hasher.update(std.mem.asBytes(&@as(u8, @intFromEnum(flat_type))));
            switch (flat_type) {
                .nominal_type => |nom| {
                    hasher.update(std.mem.asBytes(&nom.ident.ident_idx));
                },
                else => {},
            }
        },
        .flex => hasher.update("flex"),
        .rigid => hasher.update("rigid"),
        .alias => hasher.update("alias"),
        .recursion_var => hasher.update("recursion"),
        .err => hasher.update("err"),
    }

    return hasher.final();
}

/// Get the type name for a concrete type (for specialization suffix)
pub fn getTypeName(self: *Self, type_var: types.Var) []const u8 {
    const resolved = self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            switch (flat_type) {
                .nominal_type => |nom| {
                    return self.module_env.getIdent(nom.ident.ident_idx);
                },
                else => return "Unknown",
            }
        },
        .flex, .rigid => return "a",
        .alias, .recursion_var, .err => return "Unknown",
    }
}

/// Create a specialized name for a function
pub fn createSpecializedName(
    self: *Self,
    original_name: base.Ident.Idx,
    type_var: types.Var,
) !base.Ident.Idx {
    const key = SpecializationKey{
        .original_ident = original_name,
        .type_hash = self.typeHash(type_var),
    };

    // Check if we already have this specialization
    if (self.specializations.get(key)) |existing| {
        return existing;
    }

    // Create new specialized name: original_TypeName_N
    const original = self.module_env.getIdent(original_name);
    const type_name = self.getTypeName(type_var);
    self.specialization_counter += 1;

    const specialized = try std.fmt.allocPrint(
        self.allocator,
        "{s}_{s}_{d}",
        .{ original, type_name, self.specialization_counter },
    );
    defer self.allocator.free(specialized);

    const specialized_ident = try self.module_env.insertIdent(base.Ident.for_text(specialized));

    try self.specializations.put(key, specialized_ident);
    return specialized_ident;
}

/// Monomorphize a module's top-level expressions
/// Returns a new ModuleEnv with specialized functions
pub fn monomorphize() !void {
    // Phase 1: Just traverse and identify what needs specialization
    // For now, this is a placeholder for the full implementation

    // In a full implementation, we would:
    // 1. Find all top-level function definitions
    // 2. Analyze their types to find polymorphic ones
    // 3. Find all call sites and their concrete types
    // 4. Create specialized versions
    // 5. Update call sites to use specialized versions
}

// Tests

const testing = std.testing;

test "monomorphizer: isPolymorphic with flex var" {
    // This test would need a proper types.Store setup
    // For now, just verify the type compiles
}

test "monomorphizer: typeHash produces consistent results" {
    // This test would need a proper types.Store setup
    // For now, just verify the type compiles
}

test "monomorphizer: createSpecializedName" {
    // Create a minimal test environment
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    // Note: We'd need a proper types.Store to fully test this
    // For now, just verify creation and cleanup work
    var mono = Self.init(allocator, module_env, undefined);
    defer mono.deinit();

    try testing.expectEqual(@as(u32, 0), mono.specialization_counter);
}
