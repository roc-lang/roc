//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins.zig");

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const LoadedModule = builtin_loading.LoadedModule;
const BuiltinIndices = CIR.BuiltinIndices;
const BuiltinTypes = builtins.BuiltinTypes;
const CompactWriter = collections.CompactWriter;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

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
    /// Self-describing frozen artifact: it owns the 16-byte-aligned buffer its
    /// sub-stores alias (stored in its `serialized_backing`), so `deinit` is a single
    /// `checked_artifact.deinit` — no separate buffer field/teardown here.
    checked_artifact: CheckedModuleArtifact,

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

    /// Initialize all builtin modules by relocating the baked CheckedModuleArtifact.
    ///
    /// The artifact is produced at build time and embedded; loading it is a single
    /// copy-into-aligned-buffer plus an O(1) relocate, replacing the per-startup
    /// re-publish that previously cost several seconds in Debug builds.
    pub fn init(allocator: Allocator) anyerror!BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load the single Builtin module (env + buffer); the artifact's module_env
        // storage takes ownership of these.
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        // Prepare the env exactly as the build-time compiler did before it published
        // the baked artifact (enable runtime ident inserts, ensure module-name idents,
        // finalize method tables) so the runtime env pairs identically with the baked
        // artifact and its interner stays heap-owned for `compiled_buffer` teardown.
        // Call the prep directly — building a whole `Modules` graph just to discard it
        // would do O(builtin defs) hashmap work on every startup.
        try check.TypedCIR.prepareRuntimeEnv(allocator, builtin_module.env);

        // The baked blob is the serialized artifact followed by a layout-version
        // trailer. Validate and strip it before relocating: a mismatch means the
        // embedded blob is stale relative to this compiler (a build-system
        // inconsistency), which we reject rather than relocate into a differently-
        // shaped struct.
        const serialized_bytes = try CheckedModuleArtifact.splitVersionTrailer(compiled_builtins.builtin_artifact_bin);

        // Copy the serialized region into a freshly-allocated 16-byte-aligned buffer
        // so the relocated sub-stores can alias it for the life of the artifact.
        const artifact_buffer = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, serialized_bytes.len);
        errdefer allocator.free(artifact_buffer);
        @memcpy(artifact_buffer, serialized_bytes);

        const serialized: *const CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(artifact_buffer.ptr));
        // `deserialize` records `artifact_buffer` in the artifact's `serialized_backing`,
        // so the artifact owns it; `errdefer` above hands ownership over on success.
        const checked_artifact = serialized.deserialize(
            artifact_buffer,
            allocator,
            .{ .compiled_buffer = .{
                .env = builtin_module.env,
                .buffer = builtin_module.buffer,
            } },
        );

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
            .checked_artifact = checked_artifact,
        };
    }

    /// Clean up all resources. The artifact is self-describing (buffer-backed via
    /// `serialized_backing`), so a single `deinit` frees its backing buffer AND its
    /// `compiled_buffer` env storage (the builtin env + Builtin.bin buffer) correctly.
    pub fn deinit(self: *BuiltinModules) void {
        self.checked_artifact.deinit(self.allocator);
    }
};
