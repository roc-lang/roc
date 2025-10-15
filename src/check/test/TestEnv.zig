//! Test environment for canonicalization testing, providing utilities to parse, canonicalize, and inspect Roc expressions.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("can").CIR;
const Can = @import("can").Can;
const ModuleEnv = @import("can").ModuleEnv;

const Check = @import("../Check.zig");
const problem_mod = @import("../problem.zig");

const CommonEnv = base.CommonEnv;
const testing = std.testing;

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,
checker: Check,
type_writer: types.TypeWriter,

module_envs: std.StringHashMap(*const ModuleEnv),
other_envs: std.array_list.Managed(*const ModuleEnv),

/// Test environment for canonicalization testing, providing a convenient wrapper around ModuleEnv, AST, and Can.
const TestEnv = @This();

/// Initialize where the provided source is an entire file
///
/// Accepts another module that should already be can'd and type checked, and will
/// add that module as an import to this module
pub fn initWithImport(source: []const u8, other_module_name: []const u8, other_module_env: *const ModuleEnv) !TestEnv {
    const gpa = std.testing.allocator;

    // Allocate our ModuleEnv, AST, and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const parse_ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(parse_ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    var module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    var other_envs = std.array_list.Managed(*const ModuleEnv).init(gpa);

    // Put the other module in the env map
    try module_envs.put(other_module_name, other_module_env);

    const module_name = "Test";
    std.debug.assert(!std.mem.eql(u8, module_name, other_module_name));

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, "test");
    can.* = try Can.init(module_env, parse_ast, &module_envs, .{});
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Pull out the imported index
    std.debug.assert(can.import_indices.size == 1);
    const import_idx = can.import_indices.get(other_module_name).?;
    std.debug.assert(@intFromEnum(import_idx) == 0);
    try other_envs.append(other_module_env);

    // Type Check
    var checker = try Check.init(gpa, &module_env.types, module_env, other_envs.items, &module_env.store.regions, module_common_idents);
    errdefer checker.deinit();

    try checker.checkFile();

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
        .other_envs = other_envs,
    };
}

/// Initialize where the provided source is an entire file
pub fn init(source: []const u8) !TestEnv {
    const gpa = std.testing.allocator;

    // Allocate our ModuleEnv, AST, and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const parse_ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(parse_ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    const module_envs = std.StringHashMap(*const ModuleEnv).init(gpa);
    const other_envs = std.array_list.Managed(*const ModuleEnv).init(gpa);

    const module_name = "Test";

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = module_name;
    try module_env.common.calcLineStarts(gpa);

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Parse the AST
    parse_ast.* = try parse.parse(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);
    parse_ast.store.emptyScratch();

    // Canonicalize
    try module_env.initCIRFields(gpa, "test");
    can.* = try Can.init(module_env, parse_ast, null, .{});
    errdefer can.deinit();

    try can.canonicalizeFile();
    try can.validateForChecking();

    // Type Check
    var checker = try Check.init(gpa, &module_env.types, module_env, &.{}, &module_env.store.regions, module_common_idents);
    errdefer checker.deinit();

    try checker.checkFile();

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
        .other_envs = other_envs,
    };
}

/// Initialize where the provided source a single expression
pub fn initExpr(comptime source_expr: []const u8) !TestEnv {
    const source_wrapper =
        \\module []
        \\ 
        \\main =
    ;

    var source: [source_wrapper.len + 1 + source_expr.len]u8 = undefined;
    std.mem.copyForwards(u8, source[0..], source_wrapper);
    std.mem.copyForwards(u8, source[source_wrapper.len..], " ");
    std.mem.copyForwards(u8, source[source_wrapper.len + 1 ..], source_expr);

    return TestEnv.init(&source);
}

pub fn deinit(self: *TestEnv) void {
    self.can.deinit();
    self.gpa.destroy(self.can);
    self.parse_ast.deinit(self.gpa);
    self.gpa.destroy(self.parse_ast);

    self.checker.deinit();
    self.type_writer.deinit();

    // ModuleEnv.deinit calls self.common.deinit() to clean up CommonEnv's internals
    // Since common is now a value field, we don't need to free it separately
    self.module_env.deinit();
    self.gpa.destroy(self.module_env);

    self.module_envs.deinit();
    self.other_envs.deinit();
}

/// Get the inferred type of the last declaration and compare it to the provided
/// expected type string.
///
/// Also assert that there were no problems processing the source code.
pub fn assertLastDefType(self: *TestEnv, expected: []const u8) !void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    try testing.expectEqualStrings(expected, try self.type_writer.writeGet(ModuleEnv.varFrom(last_def_idx)));
}

/// Get the inferred type descriptor of the last declaration
///
/// Also assert that there were no problems processing the source code.
pub fn getLastExprType(self: *TestEnv) !types.Descriptor {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();
    try self.assertNoTypeProblems();

    try testing.expect(self.module_env.all_defs.span.len > 0);
    const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    return self.module_env.types.resolveVar(ModuleEnv.varFrom(last_def_idx)).desc;
}

/// Assert that there was a single type error when checking the input. Assert
/// that the title of the type error matches the expected title.
pub fn assertOneTypeError(self: *TestEnv, expected: []const u8) !void {
    try self.assertNoParseProblems();
    // try self.assertNoCanProblems();

    // Assert 1 problem
    try testing.expectEqual(1, self.checker.problems.problems.items.len);
    const problem = self.checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = problem_mod.ReportBuilder.init(
        self.gpa,
        self.module_env,
        self.module_env,
        &self.checker.snapshots,
        "test",
        &.{},
    );
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected, report.title);
}

fn renderReportToMarkdownBuffer(buf: *std.array_list.Managed(u8), report: anytype) !void {
    buf.clearRetainingCapacity();
    var generic_writer = buf.writer();
    var adapter = generic_writer.adaptToNewApi(&.{});
    report.render(&adapter.new_interface, .markdown) catch |err| switch (err) {
        error.WriteFailed => return adapter.err.?,
        else => return err,
    };
}

fn assertNoParseProblems(self: *TestEnv) !void {
    if (self.parse_ast.hasErrors()) {
        var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
        defer report_buf.deinit();

        for (self.parse_ast.tokenize_diagnostics.items) |tok_diag| {
            var report = try self.parse_ast.tokenizeDiagnosticToReport(tok_diag, self.gpa);
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

fn assertNoCanProblems(self: *TestEnv) !void {
    var report_buf = try std.array_list.Managed(u8).initCapacity(self.gpa, 256);
    defer report_buf.deinit();

    const diagnostics = try self.module_env.getDiagnostics();
    for (diagnostics) |d| {
        var report = try self.module_env.diagnosticToReport(d, self.gpa, self.module_env.module_name);
        defer report.deinit();

        try renderReportToMarkdownBuffer(&report_buf, &report);
        try testing.expectEqualStrings("EXPECTED NO ERROR", report_buf.items);
    }
}

fn assertNoTypeProblems(self: *TestEnv) !void {
    var report_builder = problem_mod.ReportBuilder.init(self.gpa, self.module_env, self.module_env, &self.checker.snapshots, "test", &.{});
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
