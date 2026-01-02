//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const can = @import("can");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins.zig");

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const LoadedModule = builtin_loading.LoadedModule;
const BuiltinIndices = CIR.BuiltinIndices;
const BuiltinTypes = builtins.BuiltinTypes;

/// Information about a single builtin module
pub const ModuleInfo = struct {
    name: []const u8,
    module: *const LoadedModule,
};

/// Centralized container for all builtin modules
pub const BuiltinModules = struct {
    allocator: Allocator,
    builtin_module: LoadedModule,
    builtin_indices: BuiltinIndices,

    /// Get an array of all builtin modules for iteration
    /// For compatibility, we expose the Builtin module for each auto-imported type
    pub fn modules(self: *const BuiltinModules) [4]ModuleInfo {
        return .{
            .{ .name = "Bool", .module = &self.builtin_module },
            .{ .name = "Try", .module = &self.builtin_module },
            .{ .name = "Str", .module = &self.builtin_module },
            .{ .name = "Decode", .module = &self.builtin_module },
        };
    }

    /// Create a BuiltinTypes instance from these builtin modules
    pub fn asBuiltinTypes(self: *const BuiltinModules) BuiltinTypes {
        return BuiltinTypes.init(
            self.builtin_indices,
            self.builtin_module.env,
            self.builtin_module.env,
            self.builtin_module.env,
        );
    }

    /// Initialize all builtin modules by deserializing from the compiled Builtin.bin file
    pub fn init(allocator: Allocator) !BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load the single Builtin module
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *BuiltinModules) void {
        self.builtin_module.deinit();
    }
};
