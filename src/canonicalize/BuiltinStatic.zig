//! Static views and validation for compiler-embedded builtins.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const collections = @import("collections");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const CompactWriter = collections.CompactWriter;

/// A tiny owned wrapper around a ModuleEnv whose internal slices point at static builtin bytes.
pub const BuiltinModuleView = struct {
    env: *ModuleEnv,
    gpa: Allocator,

    /// Destroy the wrapper ModuleEnv. Its lists borrow the static builtin
    /// bytes, which `deinit` never frees.
    pub fn deinit(self: *BuiltinModuleView) void {
        self.env.deinit();
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
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.CorruptSerializedModuleEnv => return error.CorruptEmbeddedBuiltins,
    };
    errdefer env.deinit();

    // The view's lists read the embedded bytes in place, and every compilation
    // in the process shares them, so nothing may write through the view: its
    // type store is sealed, it has no imports or node scratch, and the baked
    // module is already prepared for checked-artifact use, so nothing inserts
    // into its identifier or string-literal tables. Debug builds enforce that
    // by making the bytes read-only, so a write faults where it happens.
    if (comptime builtin.mode == .debug) protectReadOnly(backing);

    // Checked publication can request many keys from this frozen source view.
    // Share its lazy rank generation across those walks without touching the
    // embedded interner or adding anything to the serialized representation.
    try env.common.idents.enableTextRanks(gpa);

    return .{
        .env = env,
        .gpa = gpa,
    };
}

/// Make the whole pages inside `bytes` read-only, on the targets that can.
fn protectReadOnly(bytes: []u8) void {
    const can_protect = builtin.os.tag == .linux or (builtin.os.tag.isDarwin() and builtin.link_libc);
    if (comptime !can_protect) return;

    const page = std.heap.pageSize();
    const start = std.mem.alignForward(usize, @intFromPtr(bytes.ptr), page);
    const end = std.mem.alignBackward(usize, @intFromPtr(bytes.ptr) + bytes.len, page);
    if (end <= start) return;
    const failed = if (comptime builtin.os.tag == .linux)
        std.os.linux.errno(std.os.linux.mprotect(@ptrFromInt(start), end - start, .{ .READ = true })) != .SUCCESS
    else
        std.c.mprotect(@ptrFromInt(start), end - start, .{ .READ = true }) != 0;
    // Changing the protection of the process's own data pages cannot fail.
    std.debug.assert(!failed);
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
        if (stmt != .s_nominal_decl) continue;
        const header_idx = stmt.s_nominal_decl.header;
        const header = env.store.getTypeHeader(header_idx);
        const name = env.getIdentText(header.name);
        if (isBuiltinContainerName(name)) continue;
        if (!indicesContainStatement(indices, stmt_idx)) return error.CorruptEmbeddedBuiltins;
    }
}

fn statementQualifiedName(env: *const ModuleEnv, stmt_idx: CIR.Statement.Idx) ?[]const u8 {
    if (@backingInt(stmt_idx) >= env.store.nodes.len()) return null;
    const stmt = env.store.getStatement(stmt_idx);
    const header_idx = if (stmt == .s_nominal_decl)
        stmt.s_nominal_decl.header
    else if (stmt == .s_alias_decl)
        stmt.s_alias_decl.header
    else
        return null;
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
