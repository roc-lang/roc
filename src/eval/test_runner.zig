//! Runs `expect` expressions through the shared LIR interpreter path.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check = @import("check");
const types = @import("types");
const import_mapping_mod = types.import_mapping;
const reporting = @import("reporting");
const eval_mod = @import("mod.zig");
const pipeline = @import("pipeline.zig");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const EvalError = eval_mod.Interpreter.Error;
const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;
const BuiltinTypes = eval_mod.BuiltinTypes;

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse
        @panic("Out of memory during expect evaluation");
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(result.ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    alloc_args.answer = @ptrFromInt(@intFromPtr(result.ptr) + size_storage_bytes);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    test_env.allocator.rawFree(@as([*]u8, @ptrCast(base_ptr))[0..total_size], align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
    const new_total_size = realloc_args.new_length + size_storage_bytes;
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = test_env.allocator.realloc(old_slice, new_total_size) catch
        @panic("Out of memory during expect realloc");
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn testRocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    const formatted = std.fmt.allocPrint(test_env.allocator, "Expect failed: {s}", .{trimmed}) catch
        @panic("failed to allocate expect failure message");
    test_env.crash.recordCrash(formatted) catch {
        test_env.allocator.free(formatted);
        @panic("failed to record expect failure");
    };
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    test_env.crash.recordCrash(msg_slice) catch @panic("failed to record crash message");
}

const Evaluation = enum { passed, failed };

/// Classification for an `expect` failure.
pub const FailureType = enum {
    simple_failure,
    eval_error,
};

/// Additional failure data captured for an `expect`.
pub const FailureInfo = union(FailureType) {
    simple_failure,
    eval_error: EvalError,
};

/// Result of executing one source-level `expect`.
pub const TestResult = struct {
    passed: bool,
    region: base.Region,
    failure_info: ?FailureInfo = null,
    error_msg: ?[]const u8 = null,
};

const TestSummary = struct {
    passed: u32,
    failed: u32,
};

/// Executes lowered expect procs through the LIR interpreter.
pub const TestRunner = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    semantic_program: pipeline.SemanticEvalProgram,
    interpreter: eval_mod.Interpreter,
    crash: CrashContext,
    roc_ops: ?RocOps,
    test_results: std.array_list.Managed(TestResult),

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        _: BuiltinTypes,
        other_modules: []const *const can.ModuleEnv,
        _: ?*const can.ModuleEnv,
        _: *const import_mapping_mod.ImportMapping,
    ) !TestRunner {
        const all_module_envs = try collectSemanticEvalModuleEnvs(allocator, cir, other_modules);
        defer allocator.free(all_module_envs);

        const source_modules = try allocator.alloc(check.TypedCIR.Modules.SourceModule, all_module_envs.len);
        defer allocator.free(source_modules);
        for (all_module_envs, 0..) |env, i| {
            source_modules[i] = .{ .precompiled = @constCast(env) };
        }

        var typed_cir_modules = try check.TypedCIR.Modules.init(allocator, source_modules);
        defer typed_cir_modules.deinit();

        var semantic_program = try pipeline.lowerTypedCIRToSemanticEvalProgramForTarget(
            allocator,
            &typed_cir_modules,
            all_module_envs,
            base.target.TargetUsize.native,
        );
        errdefer semantic_program.deinit();

        var runner = TestRunner{
            .allocator = allocator,
            .env = cir,
            .semantic_program = semantic_program,
            .interpreter = undefined,
            .crash = CrashContext.init(allocator),
            .roc_ops = null,
            .test_results = std.array_list.Managed(TestResult).init(allocator),
        };
        errdefer runner.crash.deinit();
        errdefer runner.test_results.deinit();

        runner.interpreter = try eval_mod.Interpreter.init(
            allocator,
            &runner.semantic_program.lowered.lir_result.store,
            &runner.semantic_program.lowered.lir_result.layouts,
            runner.get_ops(),
        );
        return runner;
    }

    pub fn deinit(self: *TestRunner) void {
        self.freeTestResultMessages();
        self.interpreter.deinit();
        self.semantic_program.deinit();
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
                .hosted_fns = .{ .count = 0, .fns = undefined },
            };
        }
        self.crash.reset();
        return &(self.roc_ops.?);
    }

    pub fn crashState(self: *TestRunner) CrashState {
        return self.crash.state;
    }

    pub fn eval(self: *TestRunner, expect_root: pipeline.SemanticEvalExpectRoot) EvalError!Evaluation {
        const proc_spec = self.semantic_program.lowered.lir_result.store.getProcSpec(expect_root.proc_id);
        _ = self.get_ops();
        _ = try self.interpreter.eval(.{
            .proc_id = expect_root.proc_id,
            .ret_layout = proc_spec.ret_layout,
        });

        if (self.crash.crashMessage() != null) return .failed;
        return .passed;
    }

    fn freeTestResultMessages(self: *TestRunner) void {
        for (self.test_results.items) |result| {
            if (result.error_msg) |message| self.allocator.free(message);
        }
    }

    pub fn eval_all(self: *TestRunner) !TestSummary {
        var passed: u32 = 0;
        var failed: u32 = 0;
        self.freeTestResultMessages();
        self.test_results.clearAndFree();

        for (self.semantic_program.expect_roots) |expect_root| {
            const region = self.env.store.getStatementRegion(expect_root.statement_idx);
            const result = self.eval(expect_root) catch |err| {
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
                .failed => {
                    failed += 1;
                    const error_msg = if (self.crash.crashMessage()) |message|
                        try self.allocator.dupe(u8, message)
                    else
                        null;
                    try self.test_results.append(.{
                        .region = region,
                        .passed = false,
                        .failure_info = .simple_failure,
                        .error_msg = error_msg,
                    });
                },
                .passed => {
                    passed += 1;
                    try self.test_results.append(.{ .region = region, .passed = true });
                },
            }
        }

        return .{
            .passed = passed,
            .failed = failed,
        };
    }

    pub fn createReport(self: *const TestRunner, test_result: TestResult, filename: []const u8) !reporting.Report {
        std.debug.assert(!test_result.passed);

        const failure_info = test_result.failure_info orelse {
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

                const region_info = self.env.calcRegionInfo(test_result.region);
                try report.document.addSourceRegion(
                    region_info,
                    .error_highlight,
                    filename,
                    self.env.common.source,
                    self.env.getLineStarts(),
                );
                try report.document.addLineBreak();

                try report.document.addText("Error: ");
                try report.document.addAnnotated(@errorName(err), .error_highlight);
                try report.document.addLineBreak();
                try report.document.addLineBreak();

                const explanation = switch (err) {
                    error.DivisionByZero => "The test expression attempts to divide by zero.",
                    error.RuntimeError => "The test expression hit a runtime error during evaluation.",
                    error.Crash => "The test expression crashed during evaluation.",
                    else => "The test expression could not be evaluated.",
                };
                try report.document.addText(explanation);
                return report;
            },
        }
    }

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

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    if (!env.display_module_name_idx.isNone()) {
        return env.idents.builtin_module.eql(env.display_module_name_idx);
    }
    return std.mem.eql(u8, env.module_name, "Builtin");
}

fn collectSemanticEvalModuleEnvs(
    allocator: std.mem.Allocator,
    root_env: *const ModuleEnv,
    other_envs: []const *const ModuleEnv,
) ![]const *const ModuleEnv {
    var seen = std.AutoHashMap(usize, void).init(allocator);
    defer seen.deinit();

    var builtin_env: ?*const ModuleEnv = null;
    var extras = std.ArrayList(*const ModuleEnv).empty;
    defer extras.deinit(allocator);

    try seen.put(@intFromPtr(root_env), {});
    for (other_envs) |env| {
        if (seen.contains(@intFromPtr(env))) continue;
        try seen.put(@intFromPtr(env), {});
        if (isBuiltinModuleEnv(env)) {
            builtin_env = env;
        } else {
            try extras.append(allocator, env);
        }
    }

    const total_len: usize = 1 + @intFromBool(builtin_env != null) + extras.items.len;
    const all_envs = try allocator.alloc(*const ModuleEnv, total_len);
    all_envs[0] = root_env;
    var next: usize = 1;
    if (builtin_env) |builtin| {
        all_envs[next] = builtin;
        next += 1;
    }
    for (extras.items) |env| {
        all_envs[next] = env;
        next += 1;
    }
    return all_envs;
}
