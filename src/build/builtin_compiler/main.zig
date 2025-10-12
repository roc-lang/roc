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
    const bool_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Bool.roc", 1024 * 1024);
    defer gpa.free(bool_roc_source);

    const result_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Result.roc", 1024 * 1024);
    defer gpa.free(result_roc_source);

    const dict_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Dict.roc", 1024 * 1024);
    defer gpa.free(dict_roc_source);

    const set_roc_source = try std.fs.cwd().readFileAlloc(gpa, "src/build/roc/Set.roc", 1024 * 1024);
    defer gpa.free(set_roc_source);

    // Compile Bool.roc without injecting anything (it's completely self-contained)
    const bool_env = try compileModule(
        gpa,
        "Bool",
        bool_roc_source,
        &.{}, // No module dependencies
        .{ .inject_bool = false, .inject_result = false },
    );
    defer {
        bool_env.deinit();
        gpa.destroy(bool_env);
    }

    // Verify that Bool's type declaration is at the expected index (2)
    // This is critical for the compiler's hardcoded BUILTIN_BOOL constant
    const bool_type_idx = bool_env.all_statements.span.start;
    if (bool_type_idx != 2) {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("WARNING: Expected Bool at index 2, but got {}!\n", .{bool_type_idx});
        return error.UnexpectedBoolIndex;
    }

    // Compile Result.roc (injects Bool since Result might use if expressions)
    const result_env = try compileModule(
        gpa,
        "Result",
        result_roc_source,
        &.{}, // No module dependencies
        .{ .inject_bool = true, .inject_result = false },
    );
    defer {
        result_env.deinit();
        gpa.destroy(result_env);
    }

    // Compile Dict.roc (needs Bool injected for if expressions, and Result for error handling)
    const dict_env = try compileModule(
        gpa,
        "Dict",
        dict_roc_source,
        &.{}, // No module dependencies
        .{}, // Inject Bool and Result (defaults)
    );
    defer {
        dict_env.deinit();
        gpa.destroy(dict_env);
    }

    // Compile Set.roc (imports Dict, needs Bool and Result injected)
    const set_env = try compileModule(
        gpa,
        "Set",
        set_roc_source,
        &[_]ModuleDep{
            .{ .name = "Dict", .env = dict_env },
        },
        .{}, // Inject Bool and Result (defaults)
    );
    defer {
        set_env.deinit();
        gpa.destroy(set_env);
    }

    // Create output directory
    try std.fs.cwd().makePath("zig-out/builtins");

    // Serialize modules
    try serializeModuleEnv(gpa, bool_env, "zig-out/builtins/Bool.bin");
    try serializeModuleEnv(gpa, result_env, "zig-out/builtins/Result.bin");
    try serializeModuleEnv(gpa, dict_env, "zig-out/builtins/Dict.bin");
    try serializeModuleEnv(gpa, set_env, "zig-out/builtins/Set.bin");
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
    can_options: Can.InitOptions,
) !*ModuleEnv {
    // This follows the pattern from TestEnv.init() in src/check/test/TestEnv.zig

    // 1. Create ModuleEnv
    var module_env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    module_env.* = try ModuleEnv.init(gpa, arena_allocator, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    // 2. Create common idents (needed for type checking)
    const module_ident = try module_env.insertIdent(base.Ident.for_text(module_name));
    const list_ident = try module_env.insertIdent(base.Ident.for_text("List"));
    const box_ident = try module_env.insertIdent(base.Ident.for_text("Box"));

    const common_idents: Check.CommonIdents = .{
        .module_name = module_ident,
        .list = list_ident,
        .box = box_ident,
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
    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    defer module_envs.deinit();

    // Add dependencies (e.g., Dict for Set)
    for (deps) |dep| {
        try module_envs.put(dep.name, dep.env);
    }

    // 5. Canonicalize
    try module_env.initCIRFields(gpa, module_name);

    var can_result = try gpa.create(Can);
    defer {
        can_result.deinit();
        gpa.destroy(can_result);
    }

    can_result.* = try Can.init(module_env, parse_ast, &module_envs, can_options);

    try can_result.canonicalizeFile();
    try can_result.validateForChecking();

    // 6. Type check
    // Build the list of other modules for type checking
    var other_modules = std.ArrayList(*const ModuleEnv).init(gpa);
    defer other_modules.deinit();

    // Add dependencies
    for (deps) |dep| {
        try other_modules.append(dep.env);
    }

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        other_modules.items,
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
