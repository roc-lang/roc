//! Centralized loading and management of builtin modules (Bool, Result, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const can = @import("can");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins.zig");

const ModuleEnv = can.ModuleEnv;
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
    bool_module: LoadedModule,
    result_module: LoadedModule,
    str_module: LoadedModule,
    builtin_indices: BuiltinIndices,

    /// Get an array of all builtin modules for iteration
    pub fn modules(self: *const BuiltinModules) [3]ModuleInfo {
        return .{
            .{ .name = "Bool", .module = &self.bool_module },
            .{ .name = "Result", .module = &self.result_module },
            .{ .name = "Str", .module = &self.str_module },
        };
    }

    /// Get an array of all builtin module environments for type checking
    pub fn envs(self: *const BuiltinModules) [3]*const ModuleEnv {
        return .{
            self.bool_module.env,
            self.result_module.env,
            self.str_module.env,
        };
    }

    /// Create a BuiltinTypes instance from these builtin modules
    pub fn asBuiltinTypes(self: *const BuiltinModules) BuiltinTypes {
        return BuiltinTypes.init(
            self.builtin_indices,
            self.bool_module.env,
            self.result_module.env,
            self.str_module.env,
        );
    }

    /// Initialize all builtin modules by deserializing from the compiled .bin files
    pub fn init(allocator: Allocator) !BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load each module from its compiled .bin file using the actual .roc source
        var bool_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.bool_bin, "Bool", compiled_builtins.bool_source);
        errdefer bool_module.deinit();

        var result_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.result_bin, "Result", compiled_builtins.result_source);
        errdefer result_module.deinit();

        var str_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.str_bin, "Str", compiled_builtins.str_source);
        errdefer str_module.deinit();

        return BuiltinModules{
            .allocator = allocator,
            .bool_module = bool_module,
            .result_module = result_module,
            .str_module = str_module,
            .builtin_indices = indices,
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *BuiltinModules) void {
        self.str_module.deinit();
        self.result_module.deinit();
        self.bool_module.deinit();
    }
};
