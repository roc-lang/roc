//! Utilities for loading compiled builtin modules

const std = @import("std");
const builtin = @import("builtin");
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
    const result = indices_ptr.*;
    return result;
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
    const base_ptr_i64: i64 = @intCast(base_ptr);

    // Deserialize common env first so we can look up identifiers
    const common = serialized_ptr.common.deserialize(base_ptr_i64, source).*;

    const types = serialized_ptr.types.deserialize(base_ptr_i64, gpa).*;

    const external_decls = serialized_ptr.external_decls.deserialize(base_ptr_i64).*;

    const imports = (try serialized_ptr.imports.deserialize(base_ptr_i64, gpa)).*;

    const store = serialized_ptr.store.deserialize(base_ptr_i64, gpa).*;

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = types,
        .module_kind = serialized_ptr.module_kind,
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = external_decls,
        .imports = imports,
        .module_name = module_name,
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = serialized_ptr.diagnostics,
        .store = store,
        .evaluation_order = null,
        // Well-known identifiers for type checking - look them up in the deserialized common env
        // These must exist in the Builtin module which defines them
        .from_int_digits_ident = common.findIdent(Ident.FROM_INT_DIGITS_METHOD_NAME) orelse unreachable,
        .from_dec_digits_ident = common.findIdent(Ident.FROM_DEC_DIGITS_METHOD_NAME) orelse unreachable,
        .try_ident = common.findIdent("Try") orelse unreachable,
        .out_of_range_ident = common.findIdent("OutOfRange") orelse unreachable,
        .builtin_module_ident = common.findIdent("Builtin") orelse unreachable,
        .plus_ident = common.findIdent(Ident.PLUS_METHOD_NAME) orelse unreachable,
        .minus_ident = common.findIdent(base.Ident.MINUS_METHOD_NAME) orelse unreachable,
        .times_ident = common.findIdent(base.Ident.TIMES_METHOD_NAME) orelse unreachable,
        .div_ident = common.findIdent(base.Ident.DIV_METHOD_NAME) orelse unreachable,
        .div_trunc_ident = common.findIdent(base.Ident.DIV_TRUNC_METHOD_NAME) orelse unreachable,
        .rem_ident = common.findIdent(base.Ident.REM_METHOD_NAME) orelse unreachable,
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}
