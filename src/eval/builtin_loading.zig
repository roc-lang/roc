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

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserialize(@as(i64, @intCast(base_ptr)), gpa).*, // Pass gpa to types deserialize
        .module_kind = serialized_ptr.module_kind.decode(),
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .requires_types = serialized_ptr.requires_types.deserialize(@as(i64, @intCast(base_ptr))).*,
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
        .minus_ident = common.findIdent("minus") orelse unreachable,
        .times_ident = common.findIdent("times") orelse unreachable,
        .div_by_ident = common.findIdent("div_by") orelse unreachable,
        .div_trunc_by_ident = common.findIdent("div_trunc_by") orelse unreachable,
        .rem_by_ident = common.findIdent("rem_by") orelse unreachable,
        .negate_ident = common.findIdent(Ident.NEGATE_METHOD_NAME) orelse unreachable,
        .not_ident = common.findIdent("not") orelse unreachable,
        .is_lt_ident = common.findIdent("is_lt") orelse unreachable,
        .is_lte_ident = common.findIdent("is_lte") orelse unreachable,
        .is_gt_ident = common.findIdent("is_gt") orelse unreachable,
        .is_gte_ident = common.findIdent("is_gte") orelse unreachable,
        .is_eq_ident = common.findIdent("is_eq") orelse unreachable,
        .is_ne_ident = common.findIdent("is_ne") orelse unreachable,
        // Fully-qualified type identifiers for type checking and layout generation
        .builtin_try_ident = common.findIdent("Builtin.Try") orelse unreachable,
        .builtin_numeral_ident = common.findIdent("Builtin.Num.Numeral") orelse unreachable,
        .list_type_ident = common.findIdent("List") orelse unreachable,
        .box_type_ident = common.findIdent("Box") orelse unreachable,
        .u8_type_ident = common.findIdent("Builtin.Num.U8") orelse unreachable,
        .i8_type_ident = common.findIdent("Builtin.Num.I8") orelse unreachable,
        .u16_type_ident = common.findIdent("Builtin.Num.U16") orelse unreachable,
        .i16_type_ident = common.findIdent("Builtin.Num.I16") orelse unreachable,
        .u32_type_ident = common.findIdent("Builtin.Num.U32") orelse unreachable,
        .i32_type_ident = common.findIdent("Builtin.Num.I32") orelse unreachable,
        .u64_type_ident = common.findIdent("Builtin.Num.U64") orelse unreachable,
        .i64_type_ident = common.findIdent("Builtin.Num.I64") orelse unreachable,
        .u128_type_ident = common.findIdent("Builtin.Num.U128") orelse unreachable,
        .i128_type_ident = common.findIdent("Builtin.Num.I128") orelse unreachable,
        .f32_type_ident = common.findIdent("Builtin.Num.F32") orelse unreachable,
        .f64_type_ident = common.findIdent("Builtin.Num.F64") orelse unreachable,
        .dec_type_ident = common.findIdent("Builtin.Num.Dec") orelse unreachable,
        .deferred_numeric_literals = try ModuleEnv.DeferredNumericLiteral.SafeList.initCapacity(gpa, 0),
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}
