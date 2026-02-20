//! Test environment for MIR lowering tests, providing utilities to parse,
//! canonicalize, type-check, and lower Roc expressions to MIR.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("can").CIR;
const Can = @import("can").Can;
const ModuleEnv = @import("can").ModuleEnv;
const collections = @import("collections");
const Allocators = base.Allocators;

const Check = @import("check").Check;

const MIR = @import("../MIR.zig");
const Lower = @import("../Lower.zig");

const testing = std.testing;

const compiled_builtins = @import("compiled_builtins");

/// Wrapper for a loaded compiled module that tracks the buffer
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

/// Deserialize BuiltinIndices from the binary data generated at build time
fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);

    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
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
        .import_mapping = types.import_mapping.ImportMapping.init(gpa),
        .method_idents = serialized_ptr.method_idents.deserializeInto(base_ptr),
        .rigid_vars = std.AutoHashMapUnmanaged(base.Ident.Idx, types.Var){},
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,
checker: Check,
builtin_indices: CIR.BuiltinIndices,

mir_store: *MIR.Store,
lower: *Lower,

builtin_module: LoadedModule,
owned_source: ?[]u8 = null,
owns_builtin_module: bool = true,
imported_envs_list: ?std.ArrayList(*const ModuleEnv) = null,

const MirTestEnv = @This();

/// Initialize where the provided source is a single expression, wrapped as `main = <expr>`
pub fn initExpr(comptime source_expr: []const u8) !MirTestEnv {
    const gpa = std.testing.allocator;

    const source_wrapper =
        \\main =
    ;

    const total_len = source_wrapper.len + 1 + source_expr.len;
    var source = try gpa.alloc(u8, total_len);
    errdefer gpa.free(source);

    std.mem.copyForwards(u8, source[0..source_wrapper.len], source_wrapper);
    source[source_wrapper.len] = ' ';
    std.mem.copyForwards(u8, source[source_wrapper.len + 1 ..], source_expr);

    var env = try initFull("Test", source);
    env.owned_source = source;
    return env;
}

/// Full init: parse → canonicalize → type-check → init Lower
pub fn initFull(module_name: []const u8, source: []const u8) !MirTestEnv {
    const gpa = std.testing.allocator;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const can_ptr = try gpa.create(Can);
    errdefer gpa.destroy(can_ptr);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    try Can.populateModuleEnvs(
        &module_envs,
        module_env,
        builtin_module.env,
        builtin_indices,
    );

    // Parse
    const parse_ast = try parse.parse(&allocators, &module_env.common);
    errdefer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(module_name);

    can_ptr.* = try Can.init(&allocators, module_env, parse_ast, &module_envs);
    errdefer can_ptr.deinit();

    try can_ptr.canonicalizeFile();

    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    defer imported_envs.deinit(gpa);

    try imported_envs.append(gpa, builtin_module.env);

    module_env.imports.resolveImports(module_env, imported_envs.items);

    // Type Check
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    errdefer checker.deinit();

    try checker.checkFile();

    // Init MIR Store and Lower (heap-allocated so pointers stay stable)
    const mir_store = try gpa.create(MIR.Store);
    errdefer gpa.destroy(mir_store);
    mir_store.* = try MIR.Store.init(gpa);
    errdefer mir_store.deinit(gpa);

    // all_module_envs must include Builtin so the lowerer can resolve methods
    // on builtin nominal types (Dec, I64, etc.) via cross-module dispatch.
    const all_module_envs_slice = try gpa.alloc(*ModuleEnv, 2);
    errdefer gpa.free(all_module_envs_slice);
    all_module_envs_slice[0] = @constCast(builtin_module.env);
    all_module_envs_slice[1] = module_env;

    const lower = try gpa.create(Lower);
    errdefer gpa.destroy(lower);
    lower.* = try Lower.init(
        gpa,
        mir_store,
        @as([]const *ModuleEnv, all_module_envs_slice),
        &module_env.types,
        builtin_indices,
        1, // current_module_idx = test module (Builtin is at index 0)
        null,
    );
    errdefer lower.deinit();

    return MirTestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can_ptr,
        .checker = checker,
        .builtin_indices = builtin_indices,
        .mir_store = mir_store,
        .lower = lower,
        .builtin_module = builtin_module,
    };
}

/// Lower the first (and usually only) def's expression and return the MIR ExprId.
pub fn lowerFirstDef(self: *MirTestEnv) !MIR.ExprId {
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    if (defs_slice.len == 0) return error.TestUnexpectedResult;

    const first_def = self.module_env.store.getDef(defs_slice[0]);
    return self.lower.lowerExpr(first_def.expr);
}

/// Lower a named def's expression and return the MIR ExprId.
pub fn lowerNamedDef(self: *MirTestEnv, name: []const u8) !MIR.ExprId {
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.module_env.store.getDef(def_idx);
        const pattern = self.module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = self.module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, name)) {
                return self.lower.lowerExpr(def.expr);
            }
        }
    }
    return error.TestUnexpectedResult;
}

/// Get the CIR ident and expr indices for a named def, for direct lowerExternalDef calls.
pub fn getDefExprByName(self: *MirTestEnv, name: []const u8) !struct { ident_idx: base.Ident.Idx, expr_idx: CIR.Expr.Idx } {
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.module_env.store.getDef(def_idx);
        const pattern = self.module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = self.module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, name)) {
                return .{ .ident_idx = pattern.assign.ident, .expr_idx = def.expr };
            }
        }
    }
    return error.TestUnexpectedResult;
}

/// Initialize with raw source (no `main =` wrapping), for type modules and nominal type tests.
pub fn initModule(module_name: []const u8, source: []const u8) !MirTestEnv {
    return initFull(module_name, source);
}

/// Initialize with an import from another MirTestEnv module.
pub fn initWithImport(module_name: []const u8, source: []const u8, other_module_name: []const u8, other_env: *const MirTestEnv) !MirTestEnv {
    const gpa = std.testing.allocator;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const can_ptr = try gpa.create(Can);
    errdefer gpa.destroy(can_ptr);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    // Reuse the Builtin module from the imported module
    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_env = other_env.builtin_module.env;

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    // Put the other module in the env map
    const other_module_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));

    const statement_idx = blk: {
        if (other_env.module_env.module_kind == .type_module) {
            const type_ident = other_env.module_env.common.findIdent(other_module_name);
            if (type_ident) |ident| {
                if (other_env.module_env.getExposedNodeIndexById(ident)) |node_idx| {
                    break :blk @as(CIR.Statement.Idx, @enumFromInt(node_idx));
                }
            }
        }
        break :blk null;
    };

    const other_qualified_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));
    try module_envs.put(other_module_ident, .{
        .env = other_env.module_env,
        .statement_idx = statement_idx,
        .qualified_type_ident = other_qualified_ident,
    });

    try Can.populateModuleEnvs(
        &module_envs,
        module_env,
        builtin_env,
        builtin_indices,
    );

    // Parse
    const parse_ast = try parse.parse(&allocators, &module_env.common);
    errdefer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(module_name);

    can_ptr.* = try Can.init(&allocators, module_env, parse_ast, &module_envs);
    errdefer can_ptr.deinit();

    try can_ptr.canonicalizeFile();

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_env,
        .builtin_indices = builtin_indices,
    };

    // Build imported_envs array
    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    errdefer imported_envs.deinit(gpa);

    try imported_envs.append(gpa, builtin_env);

    // Process explicit imports
    const import_count = module_env.imports.imports.items.items.len;
    for (module_env.imports.imports.items.items[0..import_count]) |str_idx| {
        const import_name = module_env.getString(str_idx);
        if (std.mem.eql(u8, import_name, other_module_name)) {
            try imported_envs.append(gpa, other_env.module_env);
        }
    }

    module_env.imports.resolveImports(module_env, imported_envs.items);

    // Type Check
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    errdefer checker.deinit();

    try checker.checkFile();

    // Init MIR Store and Lower
    const mir_store = try gpa.create(MIR.Store);
    errdefer gpa.destroy(mir_store);
    mir_store.* = try MIR.Store.init(gpa);
    errdefer mir_store.deinit(gpa);

    // all_module_envs: [builtin_env, other_env.module_env, this_module_env]
    const all_module_envs_slice = try gpa.alloc(*ModuleEnv, 3);
    errdefer gpa.free(all_module_envs_slice);
    all_module_envs_slice[0] = @constCast(builtin_env);
    all_module_envs_slice[1] = other_env.module_env;
    all_module_envs_slice[2] = module_env;

    const lower = try gpa.create(Lower);
    errdefer gpa.destroy(lower);
    lower.* = try Lower.init(
        gpa,
        mir_store,
        @as([]const *ModuleEnv, all_module_envs_slice),
        &module_env.types,
        builtin_indices,
        2, // current_module_idx = this module
        null,
    );
    errdefer lower.deinit();

    return MirTestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can_ptr,
        .checker = checker,
        .builtin_indices = builtin_indices,
        .mir_store = mir_store,
        .lower = lower,
        .builtin_module = other_env.builtin_module,
        .owns_builtin_module = false,
        .imported_envs_list = imported_envs,
    };
}

pub fn deinit(self: *MirTestEnv) void {
    const all_module_envs_ptr = self.lower.all_module_envs;

    self.lower.deinit();
    self.gpa.destroy(self.lower);
    self.mir_store.deinit(self.gpa);
    self.gpa.destroy(self.mir_store);
    self.gpa.free(all_module_envs_ptr);

    self.can.deinit();
    self.gpa.destroy(self.can);
    self.parse_ast.deinit();
    self.checker.deinit();

    self.module_env.deinit();
    self.gpa.destroy(self.module_env);

    if (self.owned_source) |buffer| {
        self.gpa.free(buffer);
    }

    if (self.imported_envs_list) |*list| {
        list.deinit(self.gpa);
    }

    if (self.owns_builtin_module) {
        self.builtin_module.deinit();
    }
}
