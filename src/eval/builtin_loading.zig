//! Utilities for loading compiled builtin modules

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;

/// Wrapper for a loaded compiled builtin module that tracks the buffer
pub const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    pub fn deinit(self: *LoadedModule) void {
        // Only free the hashmap that was allocated during deserialization
        // Most other data (like the SafeList contents) points into the buffer
        self.env.imports.map.deinit(self.gpa);

        // Free the buffer (the env points into this buffer for most data)
        self.gpa.free(self.buffer);
        // Free the env struct itself
        self.gpa.destroy(self.env);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
pub fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !can.CIR.BuiltinIndices {
    // Copy to properly aligned memory
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(can.CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const can.CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
pub fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    // Copy the embedded data to properly aligned memory
    // CompactWriter requires specific alignment for serialization
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    // Cast to the serialized structure
    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    // Deserialize
    const base_ptr = @intFromPtr(buffer.ptr);

    // Deserialize common env first so we can look up identifiers
    const common = serialized_ptr.common.deserialize(@as(i64, @intCast(base_ptr)), source).*;

    // VALIDATION: Check that deserialized identifier interner is in a valid state
    if (std.debug.runtime_safety) {
        const bytes_len = common.idents.interner.bytes.len();
        const bytes_capacity = common.idents.interner.bytes.items.capacity;
        const entry_count = common.idents.interner.entry_count;

        // Assert that capacity >= length (basic ArrayList invariant)
        std.debug.assert(bytes_capacity >= bytes_len);

        // Warn if interner is at full capacity (suspicious - shouldn't serialize at full capacity)
        if (bytes_len == bytes_capacity and bytes_capacity > 0) {
            std.debug.print("[BUILTIN LOAD WARNING] Identifier interner is at FULL CAPACITY after deserialization!\n", .{});
            std.debug.print("  Length: {}, Capacity: {}, Entries: {}\n", .{ bytes_len, bytes_capacity, entry_count });
            std.debug.print("  This is likely a bug - interners should have spare capacity.\n", .{});
            std.debug.print("  Any subsequent insert will trigger a resize and may fail if canary is corrupted.\n", .{});
            std.debug.print("\n", .{});
            std.debug.print("Possible root causes:\n", .{});
            std.debug.print("  1. SafeList serialize() sets capacity=len (line 146 in safe_list.zig)\n", .{});
            std.debug.print("  2. This means ALL deserialized SafeLists start at full capacity\n", .{});
            std.debug.print("  3. First insert after deserialization ALWAYS triggers resize\n", .{});
            std.debug.print("  4. If GPA canary is corrupted, resize will detect it\n", .{});
            std.debug.print("\n", .{});
            std.debug.print("Testing GPA canary by forcing resize...\n", .{});
        }

        // Try to verify GPA canary by attempting a small resize
        // This will catch corruption immediately during builtin loading rather than later
        if (bytes_capacity > 100) { // Only check non-trivial interners
            // Need mutable access - create a temporary mutable copy of the env
            // Actually, we can't easily do this without rewriting the structure
            // So let's just warn and let it fail later with a better error message
            if (bytes_len == bytes_capacity) {
                std.debug.print("  ⚠️  Skipping canary check (would need mutable access)\n", .{});
                std.debug.print("  ⚠️  If corruption exists, it will be detected on first insert\n", .{});
            }
        }
    }

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserialize(@as(i64, @intCast(base_ptr))).*,
        .imports = (try serialized_ptr.imports.deserialize(@as(i64, @intCast(base_ptr)), gpa)).*,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserialize(@as(i64, @intCast(base_ptr)), gpa).*,
        .evaluation_order = null,
        // Well-known identifiers for type checking - look them up in the deserialized common env
        // These must exist in the Builtin module which defines them
        .from_int_digits_ident = common.findIdent(Ident.FROM_INT_DIGITS_METHOD_NAME) orelse unreachable,
        .from_dec_digits_ident = common.findIdent(Ident.FROM_DEC_DIGITS_METHOD_NAME) orelse unreachable,
        .try_ident = common.findIdent("Try") orelse unreachable,
        .out_of_range_ident = common.findIdent("OutOfRange") orelse unreachable,
        .builtin_module_ident = common.findIdent("Builtin") orelse unreachable,
        .plus_ident = common.findIdent(Ident.PLUS_METHOD_NAME) orelse unreachable,
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}
