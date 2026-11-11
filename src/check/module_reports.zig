//! The reports of one module, phase by phase.
//!
//! Each phase of compiling a module records its problems in its own form:
//! tokenize and parse diagnostics on the AST, canonicalization diagnostics on
//! the module env, type problems in the checker. These functions are the one
//! place that turns each of them into reports and that fixes the order the
//! phases are reported in. The build coordinator appends each phase from the
//! task that ran it; a tool that checks one module on its own (the snapshot
//! tool, the playground, the REPL) appends them through the same functions, so
//! what it shows is what a build reports.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const reporting = @import("reporting");

const Check = @import("Check.zig");
const CheckedReports = @import("checked_reports.zig");
const report = @import("report.zig");
const snapshot = @import("snapshot.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Report = reporting.Report;

/// Frees every report in `reports`, then the list.
pub fn deinit(gpa: Allocator, reports: *std.ArrayList(Report)) void {
    for (reports.items) |*owned| owned.deinit();
    reports.deinit(gpa);
}

fn appendOwned(gpa: Allocator, reports: *std.ArrayList(Report), built: Report) Allocator.Error!void {
    var owned = built;
    errdefer owned.deinit();
    try reports.append(gpa, owned);
}

/// Appends a report for every tokenize diagnostic.
pub fn appendTokenize(gpa: Allocator, reports: *std.ArrayList(Report), ast: *const parse.AST, filename: ?[]const u8) Allocator.Error!void {
    for (ast.tokenize_diagnostics.items) |diagnostic| {
        try appendOwned(gpa, reports, try ast.tokenizeDiagnosticToReport(diagnostic, gpa, filename));
    }
}

/// Appends a report for every parse diagnostic.
pub fn appendParse(gpa: Allocator, reports: *std.ArrayList(Report), ast: *parse.AST, env: *const base.CommonEnv, filename: []const u8) Allocator.Error!void {
    for (ast.parse_diagnostics.items) |diagnostic| {
        try appendOwned(gpa, reports, try ast.parseDiagnosticToReport(env, diagnostic, gpa, filename));
    }
}

/// Appends the reports of parsing a file: tokenize diagnostics, then parse
/// diagnostics.
pub fn appendSyntax(gpa: Allocator, reports: *std.ArrayList(Report), ast: *parse.AST, env: *const base.CommonEnv, filename: []const u8) Allocator.Error!void {
    try appendTokenize(gpa, reports, ast, filename);
    try appendParse(gpa, reports, ast, env, filename);
}

/// Appends a report for every canonicalization diagnostic the module has
/// recorded from index `first` on. The diagnostics that draining deferred
/// imports adds go after the ones canonicalization recorded, so the task that
/// drains them reports from where canonicalization stopped.
pub fn appendCanonicalize(gpa: Allocator, reports: *std.ArrayList(Report), env: *ModuleEnv, first: u32, filename: []const u8) Allocator.Error!void {
    const diagnostics = try env.getDiagnosticsFrom(first);
    defer env.gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        try appendOwned(gpa, reports, try env.diagnosticToReport(diagnostic, gpa, filename));
    }
}

/// Appends a report for every type problem the checker has recorded.
pub fn appendTypes(
    gpa: Allocator,
    reports: *std.ArrayList(Report),
    env: *ModuleEnv,
    checker: *const Check,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
    platform_requirement_source: ?report.PlatformRequirementSource,
) Allocator.Error!void {
    var builder = try report.ReportBuilder.init(
        gpa,
        env,
        env,
        &checker.snapshots,
        &checker.problems,
        filename,
        other_modules,
        &checker.import_mapping,
        &checker.regions,
        platform_requirement_source,
    );
    defer builder.deinit();
    for (checker.problems.problems.items) |type_problem| {
        try appendOwned(gpa, reports, try builder.build(type_problem));
    }
}

/// Appends the reports a checked module's checking and compile-time evaluation
/// produced, rendered from the problems its checked cache entry stores with
/// the same builders and inputs the compilation that stored them used:
/// checking's problems with its snapshots and regions, then evaluation's.
pub fn appendStored(
    gpa: Allocator,
    reports: *std.ArrayList(Report),
    env: *ModuleEnv,
    stored: *const CheckedReports,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
    platform_requirement_source: ?report.PlatformRequirementSource,
) Allocator.Error!void {
    if (stored.isEmpty()) return;
    var problems = try stored.instantiateProblems(gpa);
    defer problems.deinit();
    var import_mapping = try stored.instantiateImportMapping(gpa);
    defer import_mapping.deinit();
    const checking_count: usize = @intCast(stored.checking_problem_count);
    if (checking_count != 0) {
        var snapshots = try snapshot.Store.initCapacity(gpa, stored.outcome.contents.len);
        defer snapshots.deinit();
        var regions = base.Region.List{};
        defer regions.deinit(gpa);
        // Snapshots render with the names, import display names and default
        // sources the checker's type writer rendered them with.
        try stored.instantiateRendering(gpa, &snapshots, &regions, .{
            .idents = env.getIdentStoreConst(),
            .import_mapping = import_mapping,
            .default_source = ModuleEnv.typeWriterDefaultSource,
            .default_source_ctx = env,
        });
        var builder = try report.ReportBuilder.init(
            gpa,
            env,
            env,
            &snapshots,
            &problems,
            filename,
            other_modules,
            &import_mapping,
            &regions,
            platform_requirement_source,
        );
        defer builder.deinit();
        for (problems.problems.items[0..checking_count]) |type_problem| {
            try appendOwned(gpa, reports, try builder.build(type_problem));
        }
    }
    if (checking_count == problems.problems.items.len) return;
    var builder = try report.ReportBuilder.initEvaluation(gpa, env, &problems, filename, other_modules, &import_mapping);
    defer builder.deinit();
    for (problems.problems.items[checking_count..]) |evaluation_problem| {
        try appendOwned(gpa, reports, try builder.build(evaluation_problem));
    }
}

/// Appends the type reports of a module that compile-time finalization never
/// sees. Finalization is what settles the exhaustiveness checks the checker
/// deferred to compile-time evaluation; with none coming, the ones that hold
/// statically become problems here, before the problems are reported.
pub fn appendUnfinalizedTypes(
    gpa: Allocator,
    reports: *std.ArrayList(Report),
    env: *ModuleEnv,
    checker: *Check,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
) Allocator.Error!void {
    _ = try checker.problems.flushPendingStaticExhaustiveness();
    try appendTypes(gpa, reports, env, checker, filename, other_modules, null);
}

/// Appends every report of a checked module in the order a build reports them:
/// tokenize, parse, canonicalize, then type checking.
pub fn appendModule(
    gpa: Allocator,
    reports: *std.ArrayList(Report),
    ast: *parse.AST,
    env: *ModuleEnv,
    checker: *const Check,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
) Allocator.Error!void {
    try appendSyntax(gpa, reports, ast, &env.common, filename);
    try appendCanonicalize(gpa, reports, env, 0, filename);
    try appendTypes(gpa, reports, env, checker, filename, other_modules, null);
}

/// `appendModule` for a module that compile-time finalization never sees; see
/// `appendUnfinalizedTypes`.
pub fn appendUnfinalizedModule(
    gpa: Allocator,
    reports: *std.ArrayList(Report),
    ast: *parse.AST,
    env: *ModuleEnv,
    checker: *Check,
    filename: []const u8,
    other_modules: []const *const ModuleEnv,
) Allocator.Error!void {
    try appendSyntax(gpa, reports, ast, &env.common, filename);
    try appendCanonicalize(gpa, reports, env, 0, filename);
    try appendUnfinalizedTypes(gpa, reports, env, checker, filename, other_modules);
}
