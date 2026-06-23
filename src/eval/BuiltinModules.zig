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

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const LoadedModule = builtin_loading.LoadedModule;
const BuiltinIndices = CIR.BuiltinIndices;
const CompactWriter = collections.CompactWriter;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

/// Centralized container for all builtin modules
pub const BuiltinModules = struct {
    allocator: Allocator,
    /// NON-OWNING view of the builtin module: its `env` and `buffer` are owned by
    /// `checked_artifact` (via its `compiled_buffer` storage / `serialized_backing`).
    /// `deinit` frees them through `checked_artifact` only — never call
    /// `builtin_module.deinit()`, which would double-free the env and buffer.
    builtin_module: LoadedModule,
    builtin_indices: BuiltinIndices,
    /// Self-describing frozen artifact: it owns the 16-byte-aligned buffer its
    /// sub-stores alias (stored in its `serialized_backing`), so `deinit` is a single
    /// `checked_artifact.deinit` — no separate buffer field/teardown here.
    checked_artifact: CheckedModuleArtifact,

    /// Initialize all builtin modules by relocating the baked CheckedModuleArtifact.
    ///
    /// The artifact is produced at build time and embedded; loading it is a single
    /// copy-into-aligned-buffer plus an O(1) relocate (no per-startup re-checking).
    pub fn init(allocator: Allocator) anyerror!BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Relocate the single Builtin module (env + buffer) directly against the
        // embedded blob: the relocated sub-stores only ever read through it, so we
        // alias the read-only @embedFile'd bytes instead of copying them into a heap
        // buffer on every startup. The artifact's module_env storage references these.
        var builtin_module = try builtin_loading.loadCompiledModuleBorrowed(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
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

        // Relocate the artifact directly against the embedded blob. Its sub-stores
        // only ever read through this backing, so aliasing the 16-byte-aligned
        // read-only bytes (the serialized region starts at offset 0) avoids a
        // per-startup alloc + memcpy. `@constCast` satisfies the shared mutable-typed
        // signature; nothing writes through it.
        const artifact_buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 =
            @alignCast(@constCast(serialized_bytes));

        const serialized: *const CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(artifact_buffer.ptr));
        // L-10: bounds-check every relocatable marker against the buffer before any
        // sub-store aliases it, so a corrupt/truncated baked blob fails cleanly.
        try serialized.validate(artifact_buffer.len);
        // Both backings (this artifact buffer and the env's Builtin.bin buffer) alias
        // embedded read-only blobs, so mark them not-owned: the single
        // `checked_artifact.deinit` then tears down only the heap-allocated bits and
        // never frees into rodata.
        var checked_artifact = serialized.deserialize(
            artifact_buffer,
            allocator,
            .{ .compiled_buffer = .{
                .env = builtin_module.env,
                .buffer = builtin_module.buffer,
                .owns_buffer = false,
            } },
        );
        checked_artifact.owns_serialized_backing = false;

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
