//! Static views and validation for compiler-embedded builtins.

const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("collections");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const CompactWriter = collections.CompactWriter;

pub const BuiltinModuleView = struct {
    env: *ModuleEnv,
    gpa: Allocator,

    pub fn deinit(self: *BuiltinModuleView) void {
        self.gpa.destroy(self.env);
        self.* = undefined;
    }
};

pub const BuiltinValidationError = error{
    StaleEmbeddedBuiltins,
    CorruptEmbeddedBuiltins,
};

pub const StaticViewError = Allocator.Error || error{CorruptEmbeddedBuiltins};

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

pub fn validateBuiltinManifest(registry_hash: u64, indices_layout_hash: u64) error{StaleEmbeddedBuiltins}!void {
    if (registry_hash != CIR.BUILTIN_TYPE_REGISTRY_HASH) return error.StaleEmbeddedBuiltins;
    if (indices_layout_hash != CIR.BUILTIN_INDICES_LAYOUT_HASH) return error.StaleEmbeddedBuiltins;
}

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
