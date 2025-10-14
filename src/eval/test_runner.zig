//! Runs expect expressions
//!
//! This module is a wrapper around the interpreter used to simplify evaluating expect expressions.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const reporting = @import("reporting");
const Interpreter = @import("interpreter.zig").Interpreter;
const eval_mod = @import("mod.zig");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;

const EvalError = Interpreter.Error;
const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());
    const base_ptr = result orelse {
        std.debug.panic("Out of memory during testRocAlloc", .{});
    };
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    test_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
    const new_total_size = realloc_args.new_length + size_storage_bytes;
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = test_env.allocator.realloc(old_slice, new_total_size) catch {
        std.debug.panic("Out of memory during testRocRealloc", .{});
    };
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = dbg_args;
    _ = env;
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = expect_args;
    _ = env;
    @panic("testRocExpectFailed not implemented yet");
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    test_env.crash.recordCrash(msg_slice) catch |err| {
        std.debug.panic("failed to record crash message for test runner: {}", .{err});
    };
}

const Evaluation = enum {
    passed,
    failed,
    not_a_bool,
};

/// Categorizes the type of test failure
const FailureType = enum {
    /// expect evaluated to false
    simple_failure,
    /// interpreter error during evaluation
    eval_error,
    /// expression didn't return a bool
    not_bool,
};

/// Detailed information about a test failure
const FailureInfo = union(FailureType) {
    /// No additional info needed
    simple_failure,
    /// The specific interpreter error
    eval_error: EvalError,
    /// No additional info needed
    not_bool,
};

// Track test results
const TestResult = struct {
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
    interpreter: Interpreter,
    crash: CrashContext,
    roc_ops: ?RocOps,
    test_results: std.array_list.Managed(TestResult),

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
    ) !TestRunner {
        return TestRunner{
            .allocator = allocator,
            .env = cir,
            .interpreter = try Interpreter.init(allocator, cir),
            .crash = CrashContext.init(allocator),
            .roc_ops = null,
            .test_results = std.array_list.Managed(TestResult).init(allocator),
        };
    }

    pub fn deinit(self: *TestRunner) void {
        self.interpreter.deinit();
        self.crash.deinit();
        self.test_results.deinit();
    }

    fn get_ops(self: *TestRunner) *RocOps {
        if (self.roc_ops == null) {
            self.roc_ops = RocOps{
                .env = @ptrCast(self),
                .roc_alloc = testRocAlloc,
                .roc_dealloc = testRocDealloc,
                .roc_realloc = testRocRealloc,
                .roc_dbg = testRocDbg,
                .roc_expect_failed = testRocExpectFailed,
                .roc_crashed = testRocCrashed,
                .host_fns = undefined, // Not used in tests
            };
        }
        self.crash.reset();
        return &(self.roc_ops.?);
    }

    pub fn crashState(self: *TestRunner) CrashState {
        return self.crash.state;
    }

    /// Evaluates a single expect expression, returning whether it passed, failed or did not evaluate to a boolean.
    pub fn eval(self: *TestRunner, expr_idx: CIR.Expr.Idx) EvalError!Evaluation {
        const ops = self.get_ops();
        const result = try self.interpreter.evalMinimal(expr_idx, ops);
        const layout_cache = &self.interpreter.runtime_layout_store;
        defer result.decref(layout_cache, ops);

        if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .bool) {
            const is_true = result.asBool();
            return if (is_true) Evaluation.passed else Evaluation.failed;
        }

        return Evaluation.not_a_bool;
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
                    error.TypeMismatch => "The test expression has incompatible types and cannot be evaluated.",
                    error.DivisionByZero => "The test expression attempts to divide by zero.",
                    error.NotImplemented => "The test uses a feature that is not yet implemented in the interpreter.",
                    error.ZeroSizedType => "The test expression results in a zero-sized type.",
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
