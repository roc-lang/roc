//! Build-time compiler for Roc builtin modules (Bool.roc, Result.roc, Dict.roc, and Set.roc).
//!
//! This executable runs during `zig build` on the host machine to:
//! 1. Parse and type-check the builtin .roc modules
//! 2. Serialize the resulting ModuleEnvs to binary files
//! 3. Output .bin files to zig-out/builtins/ (which get embedded in the roc binary)

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

/// Indices of builtin type declarations within their respective modules.
/// These are determined at build time via string lookup and serialized to builtin_indices.bin.
const BuiltinIndices = struct {
    /// Statement index of Bool type declaration within Bool module
    bool_type: CIR.Statement.Idx,
    /// Statement index of Result type declaration within Result module
    result_type: CIR.Statement.Idx,
    /// Statement index of Str type declaration within Str module
    str_type: CIR.Statement.Idx,
};

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

    // Read the .roc source files at runtime
    // NOTE: We must free these sources manually; CommonEnv.deinit() does not free the source.
    const bool_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Bool.roc", 1024 * 1024);

    const result_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Result.roc", 1024 * 1024);

    const str_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Str.roc", 1024 * 1024);

    const dict_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Dict.roc", 1024 * 1024);

    const set_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Set.roc", 1024 * 1024);

    // Compile Bool.roc (it's completely self-contained, doesn't use Bool or Result types)
    const bool_env = try compileModule(
        gpa,
        "Bool",
        bool_roc_source,
        &.{}, // No module dependencies
        null, // bool_stmt not available yet
        null, // result_stmt not available yet
    );
    defer {
        bool_env.deinit();
        gpa.destroy(bool_env);
        gpa.free(bool_roc_source);
    }

    // Find Bool type declaration via string lookup
    const bool_type_idx = try findTypeDeclaration(bool_env, "Bool");

    // Compile Result.roc (doesn't use Bool or Result types in its definition)
    const result_env = try compileModule(
        gpa,
        "Result",
        result_roc_source,
        &.{}, // No module dependencies
        null, // bool_stmt not needed for Result
        null, // result_stmt not available yet
    );
    defer {
        result_env.deinit();
        gpa.destroy(result_env);
        gpa.free(result_roc_source);
    }

    // Find Result type declaration via string lookup
    const result_type_idx = try findTypeDeclaration(result_env, "Result");

    // Compile Str.roc (doesn't use Bool or Result types in its definition)
    const str_env = try compileModule(
        gpa,
        "Str",
        str_roc_source,
        &.{}, // No module dependencies
        null, // bool_stmt not needed for Str
        null, // result_stmt not needed for Str
    );
    defer {
        str_env.deinit();
        gpa.destroy(str_env);
        gpa.free(str_roc_source);
    }

    // Find Str type declaration via string lookup
    const str_type_idx = try findTypeDeclaration(str_env, "Str");

    // Mutate the Str type from nominal to str_primitive
    // This allows Str to be treated as a true primitive type throughout the compiler
    {
        const str_var = can.ModuleEnv.varFrom(str_type_idx);
        const resolved = str_env.types.resolveVar(str_var);
        var new_desc = resolved.desc;
        new_desc.content = .{ .structure = .str_primitive };
        try str_env.types.setVarDesc(resolved.var_, new_desc);
    }

    // Compile Dict.roc (may use Result type, so we provide the indices)
    const dict_env = try compileModule(
        gpa,
        "Dict",
        dict_roc_source,
        &.{}, // No module dependencies
        null, // bool_stmt not in Dict's module
        null, // result_stmt not in Dict's module
    );
    defer {
        dict_env.deinit();
        gpa.destroy(dict_env);
        gpa.free(dict_roc_source);
    }

    // Compile Set.roc (imports Dict, may use Result)
    const set_env = try compileModule(
        gpa,
        "Set",
        set_roc_source,
        &[_]ModuleDep{
            .{ .name = "Dict", .env = dict_env },
        },
        null, // bool_stmt not in Set's module
        null, // result_stmt not in Set's module
    );
    defer {
        set_env.deinit();
        gpa.destroy(set_env);
        gpa.free(set_roc_source);
    }

    // Create output directory
    try std.fs.cwd().makePath("zig-out/builtins");

    // Serialize modules
    try serializeModuleEnv(gpa, bool_env, "zig-out/builtins/Bool.bin");
    try serializeModuleEnv(gpa, result_env, "zig-out/builtins/Result.bin");
    try serializeModuleEnv(gpa, str_env, "zig-out/builtins/Str.bin");
    try serializeModuleEnv(gpa, dict_env, "zig-out/builtins/Dict.bin");
    try serializeModuleEnv(gpa, set_env, "zig-out/builtins/Set.bin");

    // Create and serialize builtin indices
    const builtin_indices = BuiltinIndices{
        .bool_type = bool_type_idx,
        .result_type = result_type_idx,
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
    result_stmt_opt: ?CIR.Statement.Idx,
) !*ModuleEnv {
    // This follows the pattern from TestEnv.init() in src/check/test/TestEnv.zig

    // 1. Create ModuleEnv
    var module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // 2. Create common idents (needed for type checking)
    const module_ident = try module_env.insertIdent(base.Ident.for_text(module_name));
    const list_ident = try module_env.insertIdent(base.Ident.for_text("List"));
    const box_ident = try module_env.insertIdent(base.Ident.for_text("Box"));

    // Use provided bool_stmt and result_stmt if available, otherwise use undefined
    const common_idents: Check.CommonIdents = .{
        .module_name = module_ident,
        .list = list_ident,
        .box = box_ident,
        .bool_stmt = bool_stmt_opt orelse undefined,
        .result_stmt = result_stmt_opt orelse undefined,
        .str_stmt = undefined, // Not available yet when compiling builtins
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

    // Create temporary ident store for module name lookup
    var temp_idents = try base.Ident.Store.initCapacity(gpa, 16);
    defer temp_idents.deinit(gpa);

    // Add dependencies (e.g., Dict for Set)
    for (deps) |dep| {
        const dep_ident = try temp_idents.insert(gpa, base.Ident.for_text(dep.name));
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
    var imported_envs = std.array_list.Managed(*const ModuleEnv).init(gpa);
    defer imported_envs.deinit();

    // Add dependencies
    for (deps) |dep| {
        try imported_envs.append(dep.env);
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
fn findTypeDeclaration(env: *const ModuleEnv, type_name: []const u8) !CIR.Statement.Idx {
    const all_stmts = env.store.sliceStatements(env.all_statements);

    std.debug.print("[DEBUG findTypeDeclaration] Looking for type '{s}', have {} statements\n", .{ type_name, all_stmts.len });

    // Search through all statements to find the one with matching name
    for (all_stmts, 0..) |stmt_idx, i| {
        const stmt = env.store.getStatement(stmt_idx);
        std.debug.print("[DEBUG findTypeDeclaration] Statement {}: {s}\n", .{ i, @tagName(stmt) });
        switch (stmt) {
            .s_nominal_decl => |decl| {
                std.debug.print("[DEBUG findTypeDeclaration] Found s_nominal_decl, header={}\n", .{decl.header});
                const header = env.store.getTypeHeader(decl.header);
                const ident_idx = header.name;
                const ident_text = env.getIdentText(ident_idx);
                std.debug.print("[DEBUG findTypeDeclaration] Type name: '{s}'\n", .{ident_text});
                if (std.mem.eql(u8, ident_text, type_name)) {
                    std.debug.print("[DEBUG findTypeDeclaration] FOUND! Returning stmt_idx={}\n", .{stmt_idx});
                    return stmt_idx;
                }
            },
            else => continue,
        }
    }

    std.debug.print("ERROR: Could not find type declaration '{s}' in module\n", .{type_name});
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
