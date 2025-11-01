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
const Content = types.Content;
const Var = types.Var;

const max_builtin_bytes = 1024 * 1024;

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
    /// Statement index of nested List type declaration within Builtin module
    list_type: CIR.Statement.Idx,
};

/// Transform all Str nominal types to .str primitive types in a module.
/// This is necessary because the interpreter needs .str to be a primitive type,
/// but we define methods on Str as a nominal type in Str.roc for ergonomics.
///
/// This transformation happens after type-checking but before serialization,
/// ensuring that the serialized .bin file contains methods associated with
/// the .str primitive type rather than a nominal Str type.
fn transformStrNominalToPrimitive(env: *ModuleEnv) !void {
    const FlatType = types.FlatType;

    // Get the Str identifier in this module
    const str_ident = env.common.findIdent("Str") orelse {
        @panic("Str identifier not found in Builtin module");
    };

    // Iterate through all slots in the type store
    for (0..env.types.len()) |i| {
        const var_idx = @as(Var, @enumFromInt(i));

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
                            const new_desc = types.Descriptor{
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

/// Replace specific e_anno_only expressions with e_low_level_lambda operations.
/// This transforms standalone annotations into low-level builtin lambda operations
/// that will be recognized by the compiler backend.
/// Returns a list of new def indices created.
fn replaceStrIsEmptyWithLowLevel(env: *ModuleEnv) !std.ArrayList(CIR.Def.Idx) {
    const gpa = env.gpa;
    var new_def_indices = std.ArrayList(CIR.Def.Idx).empty;

    // Ensure types array has entries for all existing nodes
    // This is necessary because varFrom(node_idx) assumes type_var index == node index
    const current_nodes = env.store.nodes.len();
    const current_types = env.types.len();
    if (current_types < current_nodes) {
        // Fill the gap with fresh type variables
        var i: u64 = current_types;
        while (i < current_nodes) : (i += 1) {
            _ = env.types.fresh() catch unreachable;
        }
    }

    // Build a hashmap of (qualified name -> low-level operation)
    var low_level_map = std.AutoHashMap(base.Ident.Idx, CIR.Expr.LowLevel).init(gpa);
    defer low_level_map.deinit();

    // Add all low-level operations to the map using full qualified names
    // Associated items are stored as defs with qualified names like "Builtin.Str.is_empty"
    // We need to find the actual ident that was created during canonicalization
    if (env.common.findIdent("Builtin.Str.is_empty")) |str_is_empty_ident| {
        try low_level_map.put(str_is_empty_ident, .str_is_empty);
    }

    // Iterate through all defs and replace matching anno-only defs with low-level implementations
    const all_defs = env.store.sliceDefs(env.all_defs);
    for (all_defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const expr = env.store.getExpr(def.expr);

        // Check if this is an anno-only def (e_anno_only expression)
        if (expr == .e_anno_only and def.annotation != null) {
            // Get the identifier from the pattern
            const pattern = env.store.getPattern(def.pattern);
            if (pattern == .assign) {
                const ident = pattern.assign.ident;

                // Check if this identifier matches a low-level operation
                if (low_level_map.fetchRemove(ident)) |entry| {
                    const low_level_op = entry.value;

                    // Create a dummy parameter pattern for the lambda
                    // Use the identifier "_arg" for the parameter
                    const arg_ident = env.common.findIdent("_arg") orelse try env.common.insertIdent(gpa, base.Ident.for_text("_arg"));
                    const arg_pattern_idx = try env.addPattern(.{ .assign = .{ .ident = arg_ident } }, base.Region.zero());

                    // Create a pattern span containing just this one parameter
                    const patterns_start = env.store.scratchTop("patterns");
                    try env.store.scratch.?.patterns.append(arg_pattern_idx);
                    const args_span = CIR.Pattern.Span{ .span = .{ .start = @intCast(patterns_start), .len = 1 } };

                    // Create an e_runtime_error body that crashes when the function is called
                    const error_msg_lit = try env.insertString("Low-level builtin not yet implemented in interpreter");
                    const diagnostic_idx = try env.addDiagnostic(.{ .not_implemented = .{
                        .feature = error_msg_lit,
                        .region = base.Region.zero(),
                    } });
                    const body_idx = try env.addExpr(.{ .e_runtime_error = .{ .diagnostic = diagnostic_idx } }, base.Region.zero());

                    // Create e_low_level_lambda expression
                    const expr_idx = try env.addExpr(.{ .e_low_level_lambda = .{
                        .op = low_level_op,
                        .args = args_span,
                        .body = body_idx,
                    } }, base.Region.zero());

                    // Now replace the e_anno_only expression with the e_low_level_lambda
                    // We need to modify the def's expr field to point to our new expression
                    // CIR.Def.Idx and Node.Idx have the same underlying representation
                    const def_node_idx = @as(@TypeOf(env.store.nodes).Idx, @enumFromInt(@intFromEnum(def_idx)));
                    var def_node = env.store.nodes.get(def_node_idx);
                    def_node.data_2 = @intFromEnum(expr_idx);
                    env.store.nodes.set(def_node_idx, def_node);

                    // Track this replaced def index
                    try new_def_indices.append(gpa, def_idx);
                }
            }
        }
    }

    // Verify all low-level operations were found in the builtins
    if (low_level_map.count() > 0) {
        var missing_buf = try std.ArrayList(u8).initCapacity(gpa, 512);
        defer missing_buf.deinit(gpa);
        const writer = missing_buf.writer(gpa);

        try writer.writeAll("\n\nError: The following low-level operations were not found in Builtin.roc:\n");
        var iter = low_level_map.iterator();
        while (iter.next()) |entry| {
            const ident_text = env.getIdentText(entry.key_ptr.*);
            const op_name = @tagName(entry.value_ptr.*);
            try writer.print("  - {s} (mapped to .{s})\n", .{ ident_text, op_name });
        }
        try writer.writeAll("\nEither:\n");
        try writer.writeAll("  1. Remove the obsolete entry from the low_level_map in builtin_compiler/main.zig, OR\n");
        try writer.writeAll("  2. Add a standalone type annotation to Builtin.roc for it to match\n\n");

        std.debug.print("{s}", .{missing_buf.items});
        return error.LowLevelOperationsNotFound;
    }

    return new_def_indices;
}

/// Transform all List nominal types to .list primitive types in a module.
/// This is similar to transformStrNominalToPrimitive but handles List's type parameter.
///
/// List is parameterized (List(a)), so we need to preserve the element type parameter
/// when transforming from nominal to primitive. The transformation replaces
/// nominal List(a) with FlatType{ .list = element_type_var }.
fn transformListNominalToPrimitive(env: *ModuleEnv) !void {
    // Get the List identifier in this module
    const list_ident = env.common.findIdent("List") orelse {
        @panic("List identifier not found in Builtin module");
    };

    // Iterate through all slots in the type store
    for (0..env.types.len()) |i| {
        const var_idx = @as(Var, @enumFromInt(i));

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
                        // Check if this is the List nominal type
                        if (nominal.ident.ident_idx == list_ident) {
                            // List should have exactly 1 type parameter (the element type)
                            // sliceNominalArgs returns only the type arguments (excludes backing var)
                            const type_args = env.types.sliceNominalArgs(nominal);

                            if (type_args.len != 1) {
                                @panic("List nominal type must have exactly 1 type parameter");
                            }

                            const element_type_var = type_args[0];

                            // Replace with .list primitive type with the element type
                            const new_content = Content{ .structure = .{ .list = element_type_var } };
                            const new_desc = types.Descriptor{
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

fn readFileAllocPath(gpa: Allocator, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        var file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        return try file.readToEndAlloc(gpa, max_builtin_bytes);
    }
    return try std.fs.cwd().readFileAlloc(gpa, path, max_builtin_bytes);
}

/// Build-time compiler that compiles builtin .roc sources into serialized ModuleEnvs.
/// This runs during `zig build` on the host machine to generate .bin files
/// that get embedded into the final roc executable.
///
/// The build system passes the absolute path to Builtin.roc as the first argument for cache tracking;
/// we honor that when present so the compiler works regardless of the current working directory.
pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa_impl.deinit();
        if (leaked == .leak) {
            std.debug.print("WARNING: Memory leaked!\n", .{});
        }
    }
    const gpa = gpa_impl.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    // Prefer the absolute path provided by the build system, but fall back to the
    // project-relative path so manual runs (e.g. `zig build run`) still succeed.
    const builtin_src_path = if (args.len >= 2) args[1] else "src/build/roc/Builtin.roc";

    // Read the Builtin.roc source file at runtime
    // NOTE: We must free this source manually; CommonEnv.deinit() does not free the source.
    const builtin_roc_source = try readFileAllocPath(gpa, builtin_src_path);

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
    const list_type_idx = try findTypeDeclaration(builtin_env, "List");

    // Expose the nested types so they can be found by getExposedNodeIndexById
    // For builtin types, the statement index IS the node index
    const bool_ident = builtin_env.common.findIdent("Bool") orelse unreachable;
    const try_ident = builtin_env.common.findIdent("Try") orelse unreachable;
    const dict_ident = builtin_env.common.findIdent("Dict") orelse unreachable;
    const set_ident = builtin_env.common.findIdent("Set") orelse unreachable;
    const str_ident = builtin_env.common.findIdent("Str") orelse unreachable;
    const list_ident = builtin_env.common.findIdent("List") orelse unreachable;

    try builtin_env.common.setNodeIndexById(gpa, bool_ident, @intCast(@intFromEnum(bool_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, try_ident, @intCast(@intFromEnum(try_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, dict_ident, @intCast(@intFromEnum(dict_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, set_ident, @intCast(@intFromEnum(set_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, str_ident, @intCast(@intFromEnum(str_type_idx)));
    try builtin_env.common.setNodeIndexById(gpa, list_ident, @intCast(@intFromEnum(list_type_idx)));

    // Transform nominal types to primitive types as necessary.
    // This must happen BEFORE serialization to ensure the .bin file contains
    // methods associated with the .str primitive, not a nominal type
    try transformStrNominalToPrimitive(builtin_env);
    try transformListNominalToPrimitive(builtin_env);

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
        .list_type = list_type_idx,
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

    // Check for canonicalization errors
    const can_diagnostics = try module_env.getDiagnostics();
    defer gpa.free(can_diagnostics);
    if (can_diagnostics.len > 0) {
        std.debug.print("Canonicalization errors in {s}:\n", .{module_name});
        for (can_diagnostics) |diag| {
            switch (diag) {
                .undeclared_type => |d| {
                    const type_name = module_env.getIdentText(d.name);
                    std.debug.print("  - Undeclared type: {s}\n", .{type_name});
                },
                .nested_value_not_found => |d| {
                    const parent = module_env.getIdentText(d.parent_name);
                    const nested = module_env.getIdentText(d.nested_name);
                    std.debug.print("  - Nested value not found: {s}.{s}\n", .{ parent, nested });
                },
                else => {
                    std.debug.print("  - Diagnostic: {any}\n", .{diag});
                },
            }
        }
        return error.CanonicalizeError;
    }

    // 5.5. Transform low-level operations (must happen before type checking)
    // For the Builtin module, transform annotation-only defs into low-level operations
    if (std.mem.eql(u8, module_name, "Builtin")) {
        // Transform annotation-only defs and get the list of new def indices
        var new_def_indices = try replaceStrIsEmptyWithLowLevel(module_env);
        defer new_def_indices.deinit(gpa);

        if (new_def_indices.items.len > 0) {
            // Rebuild all_defs span to include both old and new defs
            // First, get the old def indices from extra_data
            const old_span = module_env.all_defs.span;
            const old_def_count = old_span.len;

            // Allocate new space in extra_data for all defs (old + new)
            const new_span_start: u32 = @intCast(module_env.store.extra_data.len());

            // Copy old def indices
            var i: u32 = 0;
            while (i < old_def_count) : (i += 1) {
                const idx = @as(collections.SafeList(u32).Idx, @enumFromInt(old_span.start + i));
                const old_def_idx = module_env.store.extra_data.get(idx).*;
                _ = try module_env.store.extra_data.append(gpa, old_def_idx);
            }

            // Append new def indices
            for (new_def_indices.items) |new_def_idx| {
                _ = try module_env.store.extra_data.append(gpa, @intFromEnum(new_def_idx));
            }

            // Update all_defs to point to the new span
            module_env.all_defs.span.start = new_span_start;
            module_env.all_defs.span.len = old_def_count + @as(u32, @intCast(new_def_indices.items.len));

            // Rebuild the dependency graph and evaluation order to include new defs
            const DependencyGraph = @import("can").DependencyGraph;
            var graph = try DependencyGraph.buildDependencyGraph(
                module_env,
                module_env.all_defs,
                gpa,
            );
            defer graph.deinit();

            const eval_order = try DependencyGraph.computeSCCs(&graph, gpa);
            // Free the old evaluation order if it exists
            if (module_env.evaluation_order) |old_order| {
                old_order.deinit();
                gpa.destroy(old_order);
            }
            const eval_order_ptr = try gpa.create(DependencyGraph.EvaluationOrder);
            eval_order_ptr.* = eval_order;
            module_env.evaluation_order = eval_order_ptr;
        }
    }

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
