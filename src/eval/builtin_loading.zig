//! Utilities for loading compiled builtin modules

const std = @import("std");
const Allocator = std.mem.Allocator;
const can = @import("can");
const collections = @import("collections");

const ModuleEnv = can.ModuleEnv;

/// Wrapper for a loaded compiled builtin module that tracks the buffer
pub const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    /// Whether `buffer` is heap-owned (true: freed on deinit) or an aliased view of a
    /// statically-allocated, read-only blob such as the @embedFile'd builtin bytes
    /// (false: never freed). Relocation and all later use only read through `buffer`,
    /// so aliasing the static blob saves a per-startup copy; see
    /// `loadCompiledModuleBorrowed`.
    owns_buffer: bool,
    gpa: std.mem.Allocator,

    pub fn deinit(self: *LoadedModule) void {
        // If runtime inserts were enabled, free interner-owned heap memory first.
        self.env.common.idents.interner.deinit(self.gpa);

        // For builtins loaded via deserializeInto (not deserializeWithMutableTypes),
        // most data points into the buffer. Only the imports hashmap was heap-allocated.
        self.env.imports.deinitMapOnly(self.gpa);

        // Free the heap-allocated ModuleEnv struct itself (Option F pattern)
        self.gpa.destroy(self.env);

        // Free the buffer that holds the serialized data, unless it aliases a
        // statically-allocated blob the loader never owned.
        if (self.owns_buffer) self.gpa.free(self.buffer);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
pub fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) (Allocator.Error || error{Internal})!can.CIR.BuiltinIndices {
    if (bin_data.len < @sizeOf(can.CIR.BuiltinIndices)) {
        return error.Internal;
    }

    // Copy to properly aligned memory
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(can.CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const can.CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
pub fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) (Allocator.Error || error{Internal})!LoadedModule {
    if (bin_data.len < @sizeOf(ModuleEnv.Serialized)) {
        return error.Internal;
    }

    // Copy the embedded data to properly aligned memory
    // CompactWriter requires specific alignment for serialization
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    errdefer gpa.free(buffer);
    @memcpy(buffer, bin_data);

    // Cast to the serialized structure and deserialize
    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );
    const base_addr = @intFromPtr(buffer.ptr);
    const env = try serialized_ptr.deserializeInto(base_addr, gpa, source, module_name);

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .owns_buffer = true,
        .gpa = gpa,
    };
}

/// Like `loadCompiledModule`, but relocates directly against a statically-allocated,
/// 16-byte-aligned blob (e.g. the @embedFile'd builtin bytes) instead of copying it
/// into a fresh heap buffer first. Relocation and all later use only read through the
/// buffer (the relocated sub-stores alias it; runtime ident inserts copy out of it),
/// so aliasing the read-only blob is sound and saves an O(blob size) alloc + memcpy on
/// every compiler startup. The returned module's `buffer` is not owned and is never
/// freed, so the caller must keep the blob alive for the module's lifetime — trivially
/// true for embedded data.
pub fn loadCompiledModuleBorrowed(
    gpa: std.mem.Allocator,
    bin_data: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) const u8,
    module_name: []const u8,
    source: []const u8,
) (Allocator.Error || error{Internal})!LoadedModule {
    if (bin_data.len < @sizeOf(ModuleEnv.Serialized)) {
        return error.Internal;
    }

    // The relocated sub-stores alias these bytes read-only; the mutable-typed `buffer`
    // field and the `*Serialized` cast carry no write through this const blob.
    const buffer = @constCast(bin_data);
    const serialized_ptr: *const ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const base_addr = @intFromPtr(buffer.ptr);
    const env = try serialized_ptr.deserializeInto(base_addr, gpa, source, module_name);

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .owns_buffer = false,
        .gpa = gpa,
    };
}

test "loadCompiledModule rejects empty data" {
    try std.testing.expectError(
        error.Internal,
        loadCompiledModule(std.testing.allocator, "", "Builtin", ""),
    );
}
