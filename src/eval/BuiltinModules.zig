//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const can = @import("can");
const check = @import("check");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins.zig");
const CompileTimeFinalization = @import("compile_time_finalization.zig");

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
    checked_artifact: check.CheckedArtifact.CheckedModuleArtifact,

    /// Get an array of all builtin modules for iteration
    /// For compatibility, we expose the Builtin module for each auto-imported type
    pub fn modules(self: *const BuiltinModules) [3]ModuleInfo {
        return .{
            .{ .name = "Bool", .module = &self.builtin_module },
            .{ .name = "Try", .module = &self.builtin_module },
            .{ .name = "Str", .module = &self.builtin_module },
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
    pub fn init(allocator: Allocator) anyerror!BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load the single Builtin module
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        // Ownership of the loaded env+buffer transfers to `checked_artifact`
        // (via its `compiled_buffer` module env) once it is deserialized below.
        var artifact_owns_env = false;
        errdefer if (!artifact_owns_env) builtin_module.deinit();

        // Prepare the builtin env for runtime use: enable interner inserts (so
        // the finalization interpreter can intern new identifiers) and finalize
        // method tables. This preparation previously happened as a side effect of
        // `TypedCIR.Modules.init` inside `publishFromTypedModule`; it stays here so
        // the env's lifecycle (and `compiled_buffer` deinit) is unchanged.
        var typed_modules = try check.TypedCIR.Modules.init(allocator, &.{
            .{ .precompiled = builtin_module.env },
        });
        defer typed_modules.deinit();

        // Deserialize the cached pre-finalize CheckedArtifact and re-attach the
        // freshly loaded module env. This replaces the expensive, deterministic
        // `publishFromTypedModule` assembly, which is now done at build time.
        var checked_artifact = try check.CheckedArtifact.CheckedModuleArtifact.deserialize(
            compiled_builtins.builtin_checked_bin,
            allocator,
            .{ .compiled_buffer = .{
                .env = builtin_module.env,
                .buffer = builtin_module.buffer,
            } },
        );
        artifact_owns_env = true;
        errdefer checked_artifact.deinitDeserialized(allocator);

        // Run compile-time finalization. This used to run as part of
        // `publishFromTypedModule`; it stays at load time because it requires the
        // interpreter (`eval`). The builtin has no imports, available, or relation
        // artifacts, and reports no problems (matching the previous publish call).
        try CompileTimeFinalization.finalizer().run(
            allocator,
            &checked_artifact,
            &.{},
            &.{},
            &.{},
            null,
        );
        try checked_artifact.verifyComplete();

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
            .checked_artifact = checked_artifact,
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *BuiltinModules) void {
        // The artifact was produced by `deserialize`, so free it via the
        // matching deserializer free path (which also releases the module env).
        self.checked_artifact.deinitDeserialized(self.allocator);
    }
};
