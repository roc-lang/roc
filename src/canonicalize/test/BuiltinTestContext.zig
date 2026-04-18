//! Helpers for loading the compiled Builtin module in canonicalize tests.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");

const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");

const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    fn deinit(self: *LoadedModule) void {
        self.env.imports.map.deinit(self.gpa);
        self.gpa.free(self.buffer);
        self.gpa.destroy(self.env);
    }
};

fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    const serialized_ptr = @as(
        *ModuleEnv.Serialized,
        @ptrCast(@alignCast(buffer.ptr)),
    );

    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    const base_ptr = @intFromPtr(buffer.ptr);
    const common = serialized_ptr.common.deserializeInto(base_ptr, source);

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserializeInto(base_ptr, gpa),
        .module_kind = serialized_ptr.module_kind.decode(),
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .requires_types = serialized_ptr.requires_types.deserializeInto(base_ptr),
        .for_clause_aliases = serialized_ptr.for_clause_aliases.deserializeInto(base_ptr),
        .provides_entries = serialized_ptr.provides_entries.deserializeInto(base_ptr),
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserializeInto(base_ptr),
        .imports = try serialized_ptr.imports.deserializeInto(base_ptr, gpa),
        .module_name = module_name,
        .display_module_name_idx = base.Ident.Idx.NONE,
        .qualified_module_ident = base.Ident.Idx.NONE,
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserializeInto(base_ptr, gpa),
        .evaluation_order = null,
        .idents = ModuleEnv.CommonIdents.find(&common),
        .deferred_numeric_literals = try ModuleEnv.DeferredNumericLiteral.SafeList.initCapacity(gpa, 0),
        .import_mapping = @import("types").import_mapping.ImportMapping.init(gpa),
        .method_idents = serialized_ptr.method_idents.deserializeInto(base_ptr),
        .method_call_fns = serialized_ptr.method_call_fns.deserializeInto(base_ptr),
        .rigid_vars = std.AutoHashMapUnmanaged(base.Ident.Idx, @import("types").Var){},
    };

    return .{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

/// Loads the compiled Builtin module for canonicalize tests that need real builtin types.
pub const BuiltinTestContext = struct {
    builtin_indices: CIR.BuiltinIndices,
    builtin_module: LoadedModule,

    pub fn init(gpa: std.mem.Allocator) !BuiltinTestContext {
        return .{
            .builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin),
            .builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source),
        };
    }

    pub fn deinit(self: *BuiltinTestContext) void {
        self.builtin_module.deinit();
    }

    pub fn canInitContext(self: *const BuiltinTestContext) Can.ModuleInitContext {
        return .{
            .builtin_types = .{
                .builtin_module_env = self.builtin_module.env,
                .builtin_indices = self.builtin_indices,
            },
        };
    }
};
