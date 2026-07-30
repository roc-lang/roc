//! Static views and validation for compiler-embedded builtins.

const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("collections");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const CompactWriter = collections.CompactWriter;

/// A tiny owned wrapper around a ModuleEnv whose internal slices point at static builtin bytes.
pub const BuiltinModuleView = struct {
    env: *ModuleEnv,
    gpa: Allocator,

    /// Destroy the wrapper ModuleEnv without freeing the static builtin backing bytes.
    pub fn deinit(self: *BuiltinModuleView) void {
        self.gpa.destroy(self.env);
        self.* = undefined;
    }
};

/// Validation failures for compiler-embedded builtin data.
pub const BuiltinValidationError = error{
    StaleEmbeddedBuiltins,
    CorruptEmbeddedBuiltins,
};

/// Errors that can occur while making a ModuleEnv view over static serialized bytes.
pub const StaticViewError = Allocator.Error || error{CorruptEmbeddedBuiltins};

/// Build a ModuleEnv view over aligned static Builtin.bin bytes without copying the backing data.
pub fn moduleView(
    gpa: Allocator,
    backing: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    module_name: []const u8,
    source: []const u8,
) StaticViewError!BuiltinModuleView {
    if (backing.len < @sizeOf(ModuleEnv.Serialized)) return error.CorruptEmbeddedBuiltins;

    const serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(backing.ptr));
    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    env.* = serialized.viewStatic(
        @intFromPtr(backing.ptr),
        gpa,
        source,
        module_name,
    ) catch return error.CorruptEmbeddedBuiltins;

    return .{
        .env = env,
        .gpa = gpa,
    };
}

/// Validate that generated builtin metadata matches the compiler's current builtin registry.
pub fn validateBuiltinManifest(registry_hash: u64, indices_layout_hash: u64) error{StaleEmbeddedBuiltins}!void {
    if (registry_hash != CIR.BUILTIN_TYPE_REGISTRY_HASH) return error.StaleEmbeddedBuiltins;
    if (indices_layout_hash != CIR.BUILTIN_INDICES_LAYOUT_HASH) return error.StaleEmbeddedBuiltins;
}

/// Validate that generated builtin indices point at the expected declarations in the static module.
pub fn validateBuiltinIndices(env: *const ModuleEnv, indices: CIR.BuiltinIndices) BuiltinValidationError!void {
    inline for (CIR.builtin_type_specs) |spec| {
        const stmt_idx = @field(indices, spec.type_field);
        const stmt_name = statementQualifiedName(env, stmt_idx) orelse return error.CorruptEmbeddedBuiltins;
        if (!std.mem.eql(u8, stmt_name, spec.qualified_name)) return error.CorruptEmbeddedBuiltins;

        const ident = @field(indices, spec.ident_field);
        if (!std.mem.eql(u8, env.getIdentText(ident), spec.qualified_name)) return error.CorruptEmbeddedBuiltins;
    }

    if (!std.mem.eql(u8, env.getIdentText(indices.ok_ident), "Ok")) return error.CorruptEmbeddedBuiltins;
    if (!std.mem.eql(u8, env.getIdentText(indices.err_ident), "Err")) return error.CorruptEmbeddedBuiltins;

    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        const header_idx = switch (stmt) {
            .s_nominal_decl => |decl| decl.header,
            else => continue,
        };
        const header = env.store.getTypeHeader(header_idx);
        const name = env.getIdentText(header.name);
        if (isBuiltinContainerName(name)) continue;
        if (!indicesContainStatement(indices, stmt_idx)) return error.CorruptEmbeddedBuiltins;
    }
}

fn statementQualifiedName(env: *const ModuleEnv, stmt_idx: CIR.Statement.Idx) ?[]const u8 {
    if (@intFromEnum(stmt_idx) >= env.store.nodes.len()) return null;
    const stmt = env.store.getStatement(stmt_idx);
    const header_idx = switch (stmt) {
        .s_nominal_decl => |decl| decl.header,
        .s_alias_decl => |alias| alias.header,
        else => return null,
    };
    const header = env.store.getTypeHeader(header_idx);
    return env.getIdentText(header.name);
}

fn isBuiltinContainerName(name: []const u8) bool {
    inline for (CIR.builtin_type_container_names) |container_name| {
        if (std.mem.eql(u8, name, container_name)) return true;
    }
    return false;
}

fn indicesContainStatement(indices: CIR.BuiltinIndices, stmt_idx: CIR.Statement.Idx) bool {
    inline for (CIR.builtin_type_specs) |spec| {
        if (@field(indices, spec.type_field) == stmt_idx) return true;
    }
    return false;
}
