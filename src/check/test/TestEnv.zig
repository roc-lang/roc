//! Test environment for canonicalization testing, providing utilities to parse, canonicalize, and inspect Roc expressions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("can").CIR;
const Can = @import("can").Can;
const ModuleEnv = @import("can").ModuleEnv;
const CoreCtx = @import("can").CoreCtx;
const builtin_static = @import("can").BuiltinStatic;

const Check = @import("../Check.zig");
const TypedCIR = @import("../typed_cir.zig");
const report_mod = @import("../report.zig");

const testing = std.testing;
// Allocators was removed in Zig 0.16 migration

const compiled_builtins = @import("compiled_builtins");

/// Errors that can occur while constructing or using a check test environment.
pub const TestEnvError = Allocator.Error || error{ WriteFailed, TestExpectedEqual, TestUnexpectedResult, CorruptEmbeddedBuiltins };

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,
checker: Check,
type_writer: types.TypeWriter,

module_envs: std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),

// Static Builtin module view (created per test, cleaned up in deinit)
builtin_module: builtin_static.BuiltinModuleView,
// Whether this TestEnv owns the builtin_module and should deinit it
owns_builtin_module: bool,
/// Heap-allocated source buffer owned by this TestEnv (if any)
owned_source: ?[]u8 = null,
published_owns_module_env: bool = false,

/// Test environment for canonicalization testing, providing a convenient wrapper around ModuleEnv, AST, and Can.
const TestEnv = @This();

/// Initialize where the provided source is an entire file
///
/// Accepts another module that should already be can'd and type checked, and will
/// add that module as an import to this module.
/// IMPORTANT: This reuses the Builtin module from the imported module to ensure
/// type variables from auto-imported types (Bool, Try, Str) are shared across modules.
pub fn initWithImport(module_name: []const u8, source: []const u8, other_module_name: []const u8, other_test_env: *const TestEnv) Allocator.Error!TestEnv {
    const gpa = std.testing.allocator;

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    // Allocate our ModuleEnv and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

    std.debug.assert(!std.mem.eql(u8, module_name, other_module_name));

    // Reuse the Builtin module from the imported module
    // This ensures type variables for auto-imported types (Bool, Try, Str) are shared
    const builtin_indices = compiled_builtins.builtinIndices(CIR);
    const builtin_env = other_test_env.builtin_module.env;

    // Initialize the module_env so we can use its ident store
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    // Put the other module in the env map using module_env's ident store
    const other_module_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));

    // For type modules, look up the exposed type statement index
    // The type name matches the module name for type modules
    const statement_idx = blk: {
        if (other_test_env.module_env.module_kind == .type_module) {
            // Type modules expose their main type under the module name
            const type_ident = other_test_env.module_env.common.findIdent(other_module_name);
            if (type_ident) |ident| {
                if (other_test_env.module_env.getExposedTypeNodeIndexById(ident)) |node_idx| {
                    // The node index IS the statement index for type declarations
                    break :blk @as(CIR.Statement.Idx, @enumFromInt(node_idx));
                }
            }
        }
        break :blk null;
    };

    // For user modules, the qualified name is just the module name itself
    // Note: Insert into module_env (calling module), not other_test_env.module_env (target module)
    // since Ident.Idx values are not transferable between stores.
    const other_qualified_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));
    try module_envs.put(other_module_ident, .{
        .env = other_test_env.module_env,
        .statement_idx = statement_idx,
        .qualified_type_ident = other_qualified_ident,
    });

    // Parse the AST
    const parse_ast = try parse.file(gpa, &module_env.common);
    errdefer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(module_name);

    can.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_env,
            .builtin_indices = builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    errdefer can.deinit();

    try can.canonicalizeFile();
    // Note: We skip validateForChecking() in unit tests since tests may not be valid
    // type modules. The validation is for real modules that will be imported.

    // Get Bool, Try, and Str statement indices from the IMPORTED modules (not copied!)
    const bool_stmt_in_bool_module = builtin_indices.bool_type;
    const try_stmt_in_result_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = bool_stmt_in_bool_module,
        .try_stmt = try_stmt_in_result_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = other_test_env.builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    // Build imported_envs array
    // Always include the builtin module for auto-imported types (Bool, Str, etc.)
    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    defer imported_envs.deinit(gpa);

    // Add builtin module unconditionally (needed for auto-imported types)
    try imported_envs.append(gpa, other_test_env.builtin_module.env);

    // Process explicit imports
    const import_count = module_env.imports.imports.items.items.len;
    for (module_env.imports.imports.items.items[0..import_count]) |str_idx| {
        const import_name = module_env.getString(str_idx);
        if (std.mem.eql(u8, import_name, other_module_name)) {
            // Cross-module import - append the other test module's env
            try imported_envs.append(gpa, other_test_env.module_env);
        }
    }

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.clearResolvedModules();
    try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs.items);

    // Type Check - Pass all imported modules
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    checker.fixupTypeWriter();
    errdefer checker.deinit();

    try checker.checkFile();
    _ = try checker.problems.flushAllPendingStaticExhaustiveness(gpa);

    var type_writer = try module_env.initTypeWriter();
    errdefer type_writer.deinit();

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .checker = checker,
        .type_writer = type_writer,
        .module_envs = module_envs,
        .builtin_module = other_test_env.builtin_module,
        .owns_builtin_module = false, // Borrowed from other_test_env
    };
}

/// Initialize where the provided source is an entire file
pub fn init(module_name: []const u8, source: []const u8) TestEnvError!TestEnv {
    return initWithExecutableRootNames(module_name, source, &.{});
}

/// Initialize a source file and mark selected top-level defs as executable
/// zero-arg roots for checker validation.
pub fn initWithExecutableRootNames(module_name: []const u8, source: []const u8, explicit_root_names: []const []const u8) TestEnvError!TestEnv {
    const gpa = std.testing.allocator;

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    // Allocate our ModuleEnv and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

    // Load Builtin module once - Bool, Try, and Str are all types within this module
    const builtin_indices = compiled_builtins.builtinIndices(CIR);
    var builtin_module = try builtin_static.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
    errdefer builtin_module.deinit();

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    // Parse the AST
    const parse_ast = try parse.file(gpa, &module_env.common);
    errdefer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(module_name);

    can.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
        .imported_modules = &module_envs,
        .explicit_root_names = explicit_root_names,
    });
    errdefer can.deinit();

    try can.canonicalizeFile();
    // Note: We skip validateForChecking() in unit tests since tests may not be valid
    // type modules. The validation is for real modules that will be imported.

    // Get Bool, Try, and Str statement indices from the IMPORTED modules (not copied!)
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

    // Build imported_envs array
    // Always include the builtin module for auto-imported types (Bool, Str, etc.)
    var imported_envs = try std.ArrayList(*const ModuleEnv).initCapacity(gpa, 2);
    defer imported_envs.deinit(gpa);

    // Add builtin module unconditionally (needed for auto-imported types)
    try imported_envs.append(gpa, builtin_module.env);

    // Resolve imports - map each import to its index in imported_envs
    module_env.imports.clearResolvedModules();
    try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs.items);

    // Type Check - Pass the imported modules in other_modules parameter
    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    checker.fixupTypeWriter();
    for (explicit_root_names) |root_name| {
        const root_def_idx = can.explicitRootDefByName(root_name) orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("test invariant violated: explicit executable root `{s}` was not found", .{root_name});
            }
            unreachable;
        };
        try checker.addExecutableRootDef(root_def_idx);
    }
    errdefer checker.deinit();

    try checker.checkFile();
    _ = try checker.problems.flushAllPendingStaticExhaustiveness(gpa);

    var type_writer = try module_env.initTypeWriter();
    errdefer type_writer.deinit();

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .checker = checker,
        .type_writer = type_writer,
        .module_envs = module_envs,
        .builtin_module = builtin_module,
        .owns_builtin_module = true, // We own this module
    };
}

/// Canonicalize a source module without type checking and count module-not-found diagnostics.
pub fn countModuleNotFoundDiagnosticsAfterCanonicalization(module_name: []const u8, source: []const u8) TestEnvError!usize {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    const builtin_indices = compiled_builtins.builtinIndices(CIR);
    var builtin_module = try builtin_static.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
    defer builtin_module.deinit();

    try module_env.initCIRFields(module_name);

    const roc_ctx = CoreCtx.testing(gpa, gpa);
    var czer = try Can.initModule(roc_ctx, &module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer czer.deinit();

    try czer.canonicalizeFile();

    const diagnostics = try module_env.getDiagnostics();
    defer gpa.free(diagnostics);

    var module_not_found_count: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .module_not_found) {
            module_not_found_count += 1;
        }
    }

    return module_not_found_count;
}

/// Initialize where the provided source a single expression
pub fn initExpr(module_name: []const u8, comptime source_expr: []const u8) TestEnvError!TestEnv {
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

    var test_env = try TestEnv.init(module_name, source);
    test_env.owned_source = source;
    return test_env;
}

pub fn deinit(self: *TestEnv) void {
    self.can.deinit();
    self.gpa.destroy(self.can);
    self.parse_ast.deinit();

    self.checker.deinit();
    self.type_writer.deinit();

    // ModuleEnv.deinit calls self.common.deinit() to clean up CommonEnv's internals
    // Since common is now a value field, we don't need to free it separately
    if (!self.published_owns_module_env) {
        self.module_env.deinit();
        self.gpa.destroy(self.module_env);
        if (self.owned_source) |buffer| {
            self.gpa.free(buffer);
        }
    }

    self.module_envs.deinit();

    // Clean up loaded Builtin module (only if we own it)
    if (self.owns_builtin_module) {
        self.builtin_module.deinit();
    }
}

/// Transfer ownership of the published checked module into a typed-CIR source module.
pub fn takePublishedSourceModule(self: *TestEnv) TypedCIR.Modules.SourceModule {
    self.published_owns_module_env = true;
    const owned_source = self.owned_source;
    self.owned_source = null;
    return .{ .owned_checked = .{
        .env = self.module_env,
        .owned_source = owned_source,
    } };
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertDefType(self: *TestEnv, target_def_name: []const u8, expected: []const u8) TestEnvError!void {
    return self.assertDefTypeOptions(target_def_name, expected, .{ .allow_type_errors = false });
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertDefTypeOptions(self: *TestEnv, target_def_name: []const u8, expected: []const u8, comptime options: struct { allow_type_errors: bool }) TestEnvError!void {
    try self.assertNoParseProblems();
    try self.assertNoCanProblems();
    if (!options.allow_type_errors) {
        try self.assertNoTypeProblems();
    }

    try testing.expect(self.module_env.all_defs.span.len > 0);

    const idents = self.module_env.getIdentStoreConst();
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.module_env.store.getDef(def_idx);
        const ptrn = self.module_env.store.getPattern(def.pattern);

        switch (ptrn) {
            .assign => |assign| {
                const def_name = idents.getText(assign.ident);
                if (std.mem.eql(u8, target_def_name, def_name)) {
                    try self.type_writer.write(ModuleEnv.varFrom(def_idx), .wrap);
                    try testing.expectEqualStrings(expected, self.type_writer.get());
                    return;
                }
            },
            else => {
                return error.TestUnexpectedResult;
            },
        }
    }
    return error.TestUnexpectedResult;
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertLastDefType(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoErrors();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];
    const last_def_var = ModuleEnv.varFrom(last_def_idx);

    try self.type_writer.write(last_def_var, .wrap);
    try testing.expectEqualStrings(expected, self.type_writer.get());
}

/// Like `assertLastDefType`, but the module must also produce warning-severity
/// type problems — and ONLY warnings — whose report titles match
/// `expected_warning_titles` exactly, in order. (Plain pass-mode fails on ANY
/// problem, warnings included, so warning-producing tests must declare their
/// warnings here rather than silently tolerate them.)
pub fn assertLastDefTypeWithWarnings(
    self: *TestEnv,
    expected: []const u8,
    expected_warning_titles: []const []const u8,
) TestEnvError!void {
    try self.assertNoParseProblems();
    try self.assertNoCanProblems();
    try self.assertOnlyTypeWarnings(expected_warning_titles);

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];
    const last_def_var = ModuleEnv.varFrom(last_def_idx);

    try self.type_writer.write(last_def_var, .wrap);
    try testing.expectEqualStrings(expected, self.type_writer.get());
}

/// Construct a ReportBuilder wired to this TestEnv's allocator, module env,
/// checker snapshots, problems, and regions. All test call sites are
/// identical, so this single helper avoids repeating the nine arguments.
fn initReportBuilder(self: *TestEnv) Allocator.Error!report_mod.ReportBuilder {
    return report_mod.ReportBuilder.init(
        self.gpa,
        self.module_env,
        self.module_env,
        &self.checker.snapshots,
        &self.checker.problems,
        "test",
        &.{},
        &self.checker.import_mapping,
        &self.checker.regions,
        null,
    );
}

/// Assert that every type problem is warning-severity and that the rendered
/// titles match `expected_titles` exactly, in order. Any error-severity
/// problem (or a count/title mismatch) fails the assertion.
fn assertOnlyTypeWarnings(self: *TestEnv, expected_titles: []const []const u8) TestEnvError!void {
    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    try testing.expectEqual(expected_titles.len, self.checker.problems.problems.items.len);
    for (self.checker.problems.problems.items, 0..) |problem, i| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        try testing.expectEqualStrings(expected_titles[i], report.title);
        try testing.expectEqual(.warning, report.severity);
    }
}

/// Assert that the last definition's type contains the given substring
pub fn assertLastDefTypeContains(self: *TestEnv, expected_substring: []const u8) TestEnvError!void {
    try self.assertNoErrors();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];
    const last_def_var = ModuleEnv.varFrom(last_def_idx);

    try self.type_writer.write(last_def_var, .wrap);
    const type_str = self.type_writer.get();
    if (std.mem.find(u8, type_str, expected_substring) == null) {
        std.debug.print("Expected type to contain '{s}', but got: {s}\n", .{ expected_substring, type_str });
        return error.TestExpectedEqual;
    }
}

/// Get the inferred type descriptor of the last declaration
///
/// Also assert that there were no problems processing the source code.
pub fn getLastExprType(self: *TestEnv) TestEnvError!types.Descriptor {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    return self.module_env.types.resolveVar(ModuleEnv.varFrom(last_def_idx)).desc;
}

/// Assert that there were no parse, canonicalization, or type checking errors.
pub fn assertNoErrors(self: *TestEnv) TestEnvError!void {
    try self.assertNoParseProblems();
    try self.assertNoCanProblems();
    try self.assertNoTypeProblems();
}

/// Assert that there was a single type error when checking the input. Assert
/// that the title of the type error matches the expected title.
pub fn assertOneTypeError(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();

    // Assert 1 problem
    try testing.expectEqual(1, self.checker.problems.problems.items.len);
    const problem = self.checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

/// Assert that there was a single type error when checking the input. Assert
/// that the title of the type error matches the expected title.
pub fn assertOneTypeErrorMsg(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();

    // Assert 1 problem
    try testing.expectEqual(1, self.checker.problems.problems.items.len);
    const problem = self.checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    try renderReportToMarkdownBuffer(&report_buf, &report);

    try testing.expectEqualStrings(expected, report_buf.items);
}

/// Assert that checking produced exactly the expected problems (errors AND
/// warnings), in order, each matching its expected rendered message exactly.
pub fn assertTypeErrorMsgs(self: *TestEnv, expected: []const []const u8) TestEnvError!void {
    try self.assertNoParseProblems();

    try testing.expectEqual(expected.len, self.checker.problems.problems.items.len);

    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    for (expected, self.checker.problems.problems.items) |expected_msg, problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
        defer report_buf.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);

        try testing.expectEqualStrings(expected_msg, report_buf.items);
    }
}

/// Assert that canonicalization produced exactly one diagnostic with the expected title.
pub fn assertOneCanError(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoParseProblems();

    const diagnostics = try self.module_env.getDiagnostics();
    defer self.gpa.free(diagnostics);

    try testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try self.module_env.diagnosticToReport(diagnostics[0], self.gpa, self.module_env.module_name);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

/// Assert that canonicalization produced exactly one diagnostic with the expected rendered message.
pub fn assertOneCanErrorMsg(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoParseProblems();

    const diagnostics = try self.module_env.getDiagnostics();
    defer self.gpa.free(diagnostics);

    try testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try self.module_env.diagnosticToReport(diagnostics[0], self.gpa, self.module_env.module_name);
    defer report.deinit();

    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    try renderReportToMarkdownBuffer(&report_buf, &report);
    try testing.expectEqualStrings(expected, report_buf.items);
}

/// Assert that the first type error matches the expected title (allows multiple errors).
pub fn assertFirstTypeError(self: *TestEnv, expected: []const u8) TestEnvError!void {
    try self.assertNoParseProblems();

    // Assert at least 1 problem
    try testing.expect(self.checker.problems.problems.items.len >= 1);
    const problem = self.checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

fn renderReportToMarkdownBuffer(buf: *std.array_list.Managed(u8), report: anytype) (Allocator.Error || error{WriteFailed})!void {
    buf.clearRetainingCapacity();
    var unmanaged = buf.moveToUnmanaged();
    defer buf.* = unmanaged.toManaged(buf.allocator);

    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(buf.allocator, &unmanaged);
    defer unmanaged = writer_alloc.toArrayList();

    report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
        else => return err,
    };
}

fn assertNoParseProblems(self: *TestEnv) TestEnvError!void {
    if (self.parse_ast.hasErrors()) {
        var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
        defer report_buf.deinit();

        for (self.parse_ast.tokenize_diagnostics.items) |tok_diag| {
            var report = try self.parse_ast.tokenizeDiagnosticToReport(tok_diag, self.gpa, null);
            defer report.deinit();

            try renderReportToMarkdownBuffer(&report_buf, &report);
            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }

        for (self.parse_ast.parse_diagnostics.items) |diag| {
            var report = try self.parse_ast.parseDiagnosticToReport(&self.module_env.common, diag, self.gpa, self.module_env.module_name);
            defer report.deinit();

            try renderReportToMarkdownBuffer(&report_buf, &report);
            try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
        }
    }
}

fn assertNoCanProblems(self: *TestEnv) TestEnvError!void {
    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    const diagnostics = try self.module_env.getDiagnostics();
    defer self.gpa.free(diagnostics);

    for (diagnostics) |d| {
        var report = try self.module_env.diagnosticToReport(d, self.gpa, self.module_env.module_name);
        defer report.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);

        // Ignore "Missing `main!` Function" error - it's expected in test modules
        if (std.mem.find(u8, report_buf.items, "Missing `main!` Function") != null) {
            continue;
        }

        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }
}

fn assertNoTypeProblems(self: *TestEnv) TestEnvError!void {
    var report_builder = try self.initReportBuilder();
    defer report_builder.deinit();

    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    for (self.checker.problems.problems.items) |problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);
        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }

    try testing.expectEqual(0, self.checker.problems.problems.items.len);
}
