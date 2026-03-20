//! Runs expect expressions
//!
//! This module evaluates expect expressions using the LIR interpreter pipeline.
//! CIR expressions are lowered through CIR → MIR → LIR → RC, then evaluated directly.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const layout = @import("layout");
const reporting = @import("reporting");
const eval_mod = @import("mod.zig");

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;

const LirProgram = eval_mod.LirProgram;
const LirInterpreter = eval_mod.LirInterpreter;
const Value = eval_mod.Value;
const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;

const Evaluation = enum {
    passed,
    failed,
    not_a_bool,
};

/// Categorizes the type of test failure
pub const FailureType = enum {
    /// expect evaluated to false
    simple_failure,
    /// interpreter error during evaluation
    eval_error,
    /// expression didn't return a bool
    not_bool,
};

/// Detailed information about a test failure
pub const FailureInfo = union(FailureType) {
    /// No additional info needed
    simple_failure,
    /// The specific interpreter error
    eval_error: anyerror,
    /// No additional info needed
    not_bool,
};

/// The result of evaluating a single top-level `expect` expression.
pub const TestResult = struct {
    passed: bool,
    region: base.Region,
    failure_info: ?FailureInfo = null,
    // Legacy error message for HTML report compatibility
    error_msg: ?[]const u8 = null,
};

const TestSummary = struct {
    passed: u32,
    failed: u32,
};

/// A test runner that can evaluate expect expressions in a module.
pub const TestRunner = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    lir_program: LirProgram,
    all_module_envs: []*ModuleEnv,
    crash: CrashContext,
    test_results: std.array_list.Managed(TestResult),

    pub fn init(
        allocator: std.mem.Allocator,
        module_env: *ModuleEnv,
        other_modules: []const *const can.ModuleEnv,
        builtin_module_env: ?*const can.ModuleEnv,
    ) !TestRunner {
        // Build all_module_envs: builtin + other + module_env (deduped)
        var envs: std.ArrayList(*ModuleEnv) = .empty;
        errdefer envs.deinit(allocator);

        if (builtin_module_env) |be| {
            try envs.append(allocator, @constCast(be));
        }
        for (other_modules) |m| {
            const ptr: *ModuleEnv = @constCast(m);
            // Don't add duplicates
            var already_present = false;
            for (envs.items) |e| {
                if (e == ptr) {
                    already_present = true;
                    break;
                }
            }
            if (!already_present) {
                try envs.append(allocator, ptr);
            }
        }
        // Add module_env if not already present
        {
            var found = false;
            for (envs.items) |e| {
                if (e == module_env) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                try envs.append(allocator, module_env);
            }
        }

        return TestRunner{
            .allocator = allocator,
            .env = module_env,
            .lir_program = LirProgram.init(allocator, base.target.TargetUsize.native),
            .all_module_envs = try envs.toOwnedSlice(allocator),
            .crash = CrashContext.init(allocator),
            .test_results = std.array_list.Managed(TestResult).init(allocator),
        };
    }

    pub fn deinit(self: *TestRunner) void {
        self.lir_program.deinit();
        self.allocator.free(self.all_module_envs);
        self.crash.deinit();
        self.test_results.deinit();
    }

    pub fn crashState(self: *TestRunner) CrashState {
        return self.crash.state;
    }

    /// Evaluates a single expect expression, returning whether it passed, failed or did not evaluate to a boolean.
    pub fn eval(self: *TestRunner, expr_idx: CIR.Expr.Idx) !Evaluation {
        // Lower CIR expression through the full pipeline: CIR → MIR → LIR → RC
        var lower_result = self.lir_program.lowerExpr(
            self.env,
            expr_idx,
            self.all_module_envs,
            null,
        ) catch |err| {
            return err;
        };
        defer lower_result.deinit();

        // Create LIR interpreter and evaluate
        var interp = LirInterpreter.init(
            self.allocator,
            &lower_result.lir_store,
            lower_result.layout_store,
            null,
        );
        defer interp.deinit();

        const eval_result = interp.eval(lower_result.final_expr_id) catch |err| {
            return err;
        };
        const value = switch (eval_result) {
            .value => |v| v,
            .early_return => |v| v,
            .break_expr => return error.RuntimeError,
        };

        // Check if result is a bool (layout.Idx.bool == 0)
        if (lower_result.result_layout == .bool) {
            const is_true = value.read(u8) != 0;
            return if (is_true) .passed else .failed;
        }

        return .not_a_bool;
    }

    /// Evaluates all expect statements in the module, returning a summary of the results.
    /// Detailed results can be found in `test_results`.
    pub fn eval_all(self: *TestRunner) !TestSummary {
        var passed: u32 = 0;
        var failed: u32 = 0;
        self.test_results.clearAndFree();

        const statements = self.env.store.sliceStatements(self.env.all_statements);
        for (statements) |stmt_idx| {
            const stmt = self.env.store.getStatement(stmt_idx);
            if (stmt == .s_expect) {
                const region = self.env.store.getStatementRegion(stmt_idx);
                // TODO this can probably be optimized. Maybe run tests in parallel?
                const result = self.eval(stmt.s_expect.body) catch |err| {
                    failed += 1;
                    const error_msg = try std.fmt.allocPrint(self.allocator, "Test evaluation failed: {}", .{err});
                    try self.test_results.append(.{
                        .region = region,
                        .passed = false,
                        .failure_info = .{ .eval_error = err },
                        .error_msg = error_msg,
                    });
                    continue;
                };
                switch (result) {
                    .not_a_bool => {
                        failed += 1;
                        const error_msg = try std.fmt.allocPrint(self.allocator, "Test did not evaluate to a boolean", .{});
                        try self.test_results.append(.{
                            .region = region,
                            .passed = false,
                            .failure_info = .not_bool,
                            .error_msg = error_msg,
                        });
                    },
                    .failed => {
                        failed += 1;
                        try self.test_results.append(.{
                            .region = region,
                            .passed = false,
                            .failure_info = .simple_failure,
                        });
                    },
                    .passed => {
                        passed += 1;
                        try self.test_results.append(.{ .region = region, .passed = true });
                    },
                }
            }
        }

        return .{
            .passed = passed,
            .failed = failed,
        };
    }

    /// Create a Report for a failed test.
    /// Caller is responsible for calling report.deinit().
    pub fn createReport(self: *const TestRunner, test_result: TestResult, filename: []const u8) !reporting.Report {
        std.debug.assert(!test_result.passed); // Only call for failed tests

        const failure_info = test_result.failure_info orelse {
            // Fallback for legacy tests without failure_info
            var report = reporting.Report.init(self.allocator, "TEST FAILURE", .runtime_error);
            errdefer report.deinit();
            try report.document.addText("This expect failed but no failure information is available.");
            return report;
        };

        switch (failure_info) {
            .simple_failure => {
                var report = reporting.Report.init(self.allocator, "TEST FAILURE", .runtime_error);
                errdefer report.deinit();

                try report.document.addText("This ");
                try report.document.addAnnotated("expect", .keyword);
                try report.document.addText(" failed:");
                try report.document.addLineBreak();

                // Show the source code with highlighting
                const region_info = self.env.calcRegionInfo(test_result.region);
                try report.document.addSourceRegion(
                    region_info,
                    .error_highlight,
                    filename,
                    self.env.common.source,
                    self.env.getLineStarts(),
                );
                try report.document.addLineBreak();

                try report.document.addText("The expression evaluated to ");
                try report.document.addAnnotated("False", .emphasized);
                try report.document.addText(".");

                return report;
            },
            .eval_error => |err| {
                var report = reporting.Report.init(self.allocator, "TEST EVALUATION ERROR", .runtime_error);
                errdefer report.deinit();

                try report.document.addText("This ");
                try report.document.addAnnotated("expect", .keyword);
                try report.document.addText(" could not be evaluated:");
                try report.document.addLineBreak();

                // Show the source code with highlighting
                const region_info = self.env.calcRegionInfo(test_result.region);
                try report.document.addSourceRegion(
                    region_info,
                    .error_highlight,
                    filename,
                    self.env.common.source,
                    self.env.getLineStarts(),
                );
                try report.document.addLineBreak();

                // Show the error type
                const error_name = @errorName(err);
                try report.document.addText("Error: ");
                try report.document.addAnnotated(error_name, .error_highlight);
                try report.document.addLineBreak();
                try report.document.addLineBreak();

                // Add helpful explanation based on error type
                const explanation = switch (err) {
                    error.Crash => "The test crashed during evaluation.",
                    error.RuntimeError => "A runtime error occurred during evaluation.",
                    error.OutOfMemory => "Out of memory during evaluation.",
                    else => "This usually indicates a bug in the test itself.",
                };
                try report.document.addText(explanation);

                return report;
            },
            .not_bool => {
                var report = reporting.Report.init(self.allocator, "EXPECT TYPE ERROR", .runtime_error);
                errdefer report.deinit();

                try report.document.addText("This ");
                try report.document.addAnnotated("expect", .keyword);
                try report.document.addText(" expression must evaluate to a ");
                try report.document.addAnnotated("Bool", .type_variable);
                try report.document.addText(":");
                try report.document.addLineBreak();

                // Show the source code with highlighting
                const region_info = self.env.calcRegionInfo(test_result.region);
                try report.document.addSourceRegion(
                    region_info,
                    .error_highlight,
                    filename,
                    self.env.common.source,
                    self.env.getLineStarts(),
                );
                try report.document.addLineBreak();

                try report.document.addText("The expression did not evaluate to a ");
                try report.document.addAnnotated("Bool", .type_variable);
                try report.document.addText(" value.");
                try report.document.addLineBreak();
                try report.document.addLineBreak();
                try report.document.addText("Every ");
                try report.document.addAnnotated("expect", .keyword);
                try report.document.addText(" must have a boolean expression\u{2014}either ");
                try report.document.addAnnotated("True", .tag_name);
                try report.document.addText(" or ");
                try report.document.addAnnotated("False", .tag_name);
                try report.document.addText(".");

                return report;
            },
        }
    }

    /// Write a html report of the test results to the given writer.
    pub fn write_html_report(self: *const TestRunner, writer: *std.Io.Writer) !void {
        if (self.test_results.items.len > 0) {
            try writer.writeAll("<div class=\"test-results\">\n");
            for (self.test_results.items) |result| {
                const region_info = self.env.calcRegionInfo(result.region);
                const line_number = region_info.start_line_idx + 1;
                try writer.writeAll("<span class=\"test-evaluation\">");
                if (result.passed) {
                    try writer.writeAll("<span class=\"test-passed\">PASSED</span>");
                } else {
                    try writer.writeAll("<span class=\"test-failed\">FAILED</span>");
                }
                try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\">@{d}</span>\n", .{ result.region.start.offset, result.region.end.offset, line_number });
                try writer.print("<span class=\"test-message\">{s}</span>\n", .{result.error_msg orelse ""});
                try writer.writeAll("</span>\n");
            }
            try writer.writeAll("</div>\n");
        }
    }
};
