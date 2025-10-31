//! Build-time compiler for Roc builtin module (Builtin.roc).
//!
//! This executable runs during `zig build` on the host machine to:
//! 1. Parse and type-check the Builtin.roc module (which contains nested Bool, Result, Str, Dict, Set types)
//! 2. Serialize the resulting ModuleEnv to a binary file
//! 3. Output Builtin.bin to zig-out/builtins/ (which gets embedded in the roc binary)

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const types = @import("types");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const Check = check.Check;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;

/// Indices of builtin type declarations within the Builtin module.
/// These are determined at build time via string lookup and serialized to builtin_indices.bin.
const BuiltinIndices = struct {
    /// Statement index of nested Bool type declaration within Builtin module
    bool_type: CIR.Statement.Idx,
    /// Statement index of nested Try type declaration within Builtin module
    try_type: CIR.Statement.Idx,
    /// Statement index of nested Dict type declaration within Builtin module
    dict_type: CIR.Statement.Idx,
    /// Statement index of nested Set type declaration within Builtin module
    set_type: CIR.Statement.Idx,
    /// Statement index of nested Str type declaration within Builtin module
    str_type: CIR.Statement.Idx,
};

/// Transform all Str nominal types to .str primitive types in a module.
/// This is necessary because the interpreter needs .str to be a primitive type,
/// but we define methods on Str as a nominal type in Str.roc for ergonomics.
///
/// This transformation happens after type-checking but before serialization,
/// ensuring that the serialized .bin file contains methods associated with
/// the .str primitive type rather than a nominal Str type.
fn transformStrNominalToPrimitive(env: *ModuleEnv) !void {
    const types_mod = @import("types");
    const Content = types_mod.Content;
    const FlatType = types_mod.FlatType;

    // Get the Str identifier in this module
    const str_ident_opt = env.common.findIdent("Str");
    if (str_ident_opt == null) {
        // No Str ident found, nothing to transform
        return;
    }
    const str_ident = str_ident_opt.?;

    // Iterate through all slots in the type store
    var i: u32 = 0;
    while (i < env.types.len()) : (i += 1) {
        const var_idx = @as(types_mod.Var, @enumFromInt(i));

        // Skip redirects, only process roots
        if (env.types.isRedirect(var_idx)) {
            continue;
        }

        const resolved = env.types.resolveVar(var_idx);
        const desc = resolved.desc;

        // Check if this descriptor contains a nominal type
        switch (desc.content) {
            .structure => |structure| {
                switch (structure) {
                    .nominal_type => |nominal| {
                        // Check if this is the Str nominal type
                        // TypeIdent has an ident_idx field that references the identifier
                        if (nominal.ident.ident_idx == str_ident) {
                            // Replace with .str primitive type
                            const new_content = Content{ .structure = FlatType.str };
                            const new_desc = types_mod.Descriptor{
                                .content = new_content,
                                .rank = desc.rank,
                                .mark = desc.mark,
                            };
                            try env.types.setVarDesc(var_idx, new_desc);
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}

/// Build-time compiler that compiles builtin .roc sources into serialized ModuleEnvs.
/// This runs during `zig build` on the host machine to generate .bin files
/// that get embedded into the final roc executable.
///
/// Note: Command-line arguments are ignored. The .roc files are read from fixed paths.
/// The build system may pass file paths as arguments for cache tracking, but we don't use them.
pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa_impl.deinit();
        if (leaked == .leak) {
            std.debug.print("WARNING: Memory leaked!\n", .{});
        }
    }
    const gpa = gpa_impl.allocator();

    // Ignore command-line arguments - they're only used by Zig's build system for cache tracking

    // Read the Builtin.roc source file at runtime
    // NOTE: We must free this source manually; CommonEnv.deinit() does not free the source.
    const builtin_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Builtin.roc", 1024 * 1024);

    // Compile Builtin.roc (it's completely self-contained)
    const builtin_env = try compileModule(
        gpa,
        "Builtin",
        builtin_roc_source,
        &.{}, // No module dependencies
        null, // bool_stmt not available yet (will be found within Builtin)
        null, // try_stmt not available yet (will be found within Builtin)
    );
    defer {
        builtin_env.deinit();
        gpa.destroy(builtin_env);
        gpa.free(builtin_roc_source);
    }

    // Find nested type declarations in Builtin module
    // These are nested inside Builtin's record extension (Builtin := [].{...})
    const bool_type_idx = try findTypeDeclaration(builtin_env, "Bool");
    const try_type_idx = try findTypeDeclaration(builtin_env, "Try");
    const dict_type_idx = try findTypeDeclaration(builtin_env, "Dict");
    const set_type_idx = try findTypeDeclaration(builtin_env, "Set");
    const str_type_idx = try findTypeDeclaration(builtin_env, "Str");

    // Expose the nested types so they can be found by getExposedNodeIndexById
    // For builtin types, the statement index IS the node index
    const bool_ident = builtin_env.common.findIdent("Bool") orelse unreachable;
    const try_ident = builtin_env.common.findIdent("Try") orelse unreachable;
    const dict_ident = builtin_env.common.findIdent("Dict") orelse unreachable;
    const set_ident = builtin_env.common.findIdent("Set") orelse unreachable;
    const str_ident = builtin_env.common.findIdent("Str") orelse unreachable;

    try builtin_env.common.setNodeIndexById(gpa, bool_ident, @intCast(@intFromEnum(bool_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, try_ident, @intCast(@intFromEnum(try_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dict_ident, @intCast(@intFromEnum(dict_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, set_ident, @intCast(@intFromEnum(set_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, str_ident, @intCast(@intFromEnum(str_type_idx)));

    // Transform Str nominal types to .str primitive types
    // This must happen BEFORE serialization to ensure the .bin file contains
    // methods associated with the .str primitive, not a nominal type
    try transformStrNominalToPrimitive(builtin_env);

    // Create output directory
    try std.fs.cwd().makePath("zig-out/builtins");

    // Serialize the single Builtin module
    try serializeModuleEnv(gpa, builtin_env, "zig-out/builtins/Builtin.bin");

    // Create and serialize builtin indices
    const builtin_indices = BuiltinIndices{
        .bool_type = bool_type_idx,
        .try_type = try_type_idx,
        .dict_type = dict_type_idx,
        .set_type = set_type_idx,
        .str_type = str_type_idx,
    };
    try serializeBuiltinIndices(builtin_indices, "zig-out/builtins/builtin_indices.bin");
}

const ModuleDep = struct {
    name: []const u8,
    env: *const ModuleEnv,
};

fn compileModule(
    gpa: Allocator,
    module_name: []const u8,
    source: []const u8,
    deps: []const ModuleDep,
    bool_stmt_opt: ?CIR.Statement.Idx,
    try_stmt_opt: ?CIR.Statement.Idx,
) !*ModuleEnv {
    // This follows the pattern from TestEnv.init() in src/check/test/TestEnv.zig

    // 1. Create ModuleEnv
    var module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // 2. Create common idents (needed for type checking)
    const module_ident = try module_env.insertIdent(base.Ident.for_text(module_name));
    const list_ident = try module_env.insertIdent(base.Ident.for_text("List"));
    const box_ident = try module_env.insertIdent(base.Ident.for_text("Box"));

    // Use provided bool_stmt and try_stmt if available, otherwise use undefined
    const common_idents: Check.CommonIdents = .{
        .module_name = module_ident,
        .list = list_ident,
        .box = box_ident,
        .bool_stmt = bool_stmt_opt orelse undefined,
        .try_stmt = try_stmt_opt orelse undefined,
        .builtin_module = null,
    };

    // 3. Parse
    var parse_ast = try gpa.create(parse.AST);
    defer {
        parse_ast.deinit(gpa);
        gpa.destroy(parse_ast);
    }

    parse_ast.* = try parse.parse(&module_env.common, gpa);
    parse_ast.store.emptyScratch();

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        std.debug.print("Parse errors in {s}:\n", .{module_name});
        for (parse_ast.tokenize_diagnostics.items) |diag| {
            std.debug.print("  Tokenize error: {any}\n", .{diag});
        }
        for (parse_ast.parse_diagnostics.items) |diag| {
            std.debug.print("  Parse error: {any}\n", .{diag});
        }
        return error.ParseError;
    }

    // 4. Create module imports map (for cross-module references)
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    // Add dependencies (e.g., Dict for Set, Bool for Str)
    // IMPORTANT: Use the module's own ident store, not a temporary one,
    // because auto-import lookups will use the module's ident store
    for (deps) |dep| {
        const dep_ident = try module_env.insertIdent(base.Ident.for_text(dep.name));
        try module_envs.put(dep_ident, .{ .env = dep.env });
    }

    // 5. Canonicalize
    try module_env.initCIRFields(gpa, module_name);

    var can_result = try gpa.create(Can);
    defer {
        can_result.deinit();
        gpa.destroy(can_result);
    }

    can_result.* = try Can.init(module_env, parse_ast, &module_envs);

    try can_result.canonicalizeFile();
    try can_result.validateForChecking();

    // 6. Type check
    // Build the list of other modules for type checking
    var imported_envs = std.ArrayList(*const ModuleEnv).empty;
    defer imported_envs.deinit(gpa);

    // Add dependencies
    for (deps) |dep| {
        try imported_envs.append(gpa, dep.env);
    }

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        common_idents,
    );
    defer checker.deinit();

    try checker.checkFile();

    // Check for type errors
    if (checker.problems.problems.items.len > 0) {
        std.debug.print("Type errors found in {s}:\n", .{module_name});
        for (checker.problems.problems.items) |prob| {
            std.debug.print("  - Problem: {any}\n", .{prob});
        }
        return error.TypeCheckError;
    }

    return module_env;
}

fn serializeModuleEnv(
    gpa: Allocator,
    env: *const ModuleEnv,
    output_path: []const u8,
) !void {
    // This follows the pattern from module_env_test.zig

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Create output file
    const file = try std.fs.cwd().createFile(output_path, .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(env, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, file);
}

/// Find a type declaration by name in a compiled module
/// Returns the statement index of the type declaration
/// For builtin_compiler, types are always in all_statements (not builtin_statements)
/// because we're compiling Builtin.roc itself, not importing from it.
fn findTypeDeclaration(env: *const ModuleEnv, type_name: []const u8) !CIR.Statement.Idx {
    // Construct the qualified name (e.g., "Builtin.Bool")
    // Types in nested declarations are stored with their full qualified names
    var qualified_name_buf: [256]u8 = undefined;
    const qualified_name = try std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ env.module_name, type_name });

    // Search in all_statements (where Builtin.roc's own types are stored)
    const all_stmts = env.store.sliceStatements(env.all_statements);
    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_nominal_decl => |decl| {
                const header = env.store.getTypeHeader(decl.header);
                const ident_idx = header.name;
                const ident_text = env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_text, qualified_name)) {
                    return stmt_idx;
                }
            },
            else => continue,
        }
    }

    return error.TypeDeclarationNotFound;
}

/// Serialize BuiltinIndices to a binary file
fn serializeBuiltinIndices(
    indices: BuiltinIndices,
    output_path: []const u8,
) !void {
    // Create output file
    const file = try std.fs.cwd().createFile(output_path, .{});
    defer file.close();

    // Write the struct directly as binary data
    // This is a simple struct with two u32 fields, so we can write it directly
    try file.writeAll(std.mem.asBytes(&indices));
}
