//! Runs expect expressions
//!
//! This module is a wrapper around the interpreter used to simplify evaluating expect expressions.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const stack = @import("stack.zig");
const layout = @import("layout");
const types = @import("types");

const Interpreter = @import("interpreter.zig").Interpreter;
const EvalError = @import("interpreter.zig").EvalError;

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const LayoutStore = layout.Store;
const TypeStore = types.store.Store;
const CIR = can.CIR;

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
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

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
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

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
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

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = dbg_args;
    _ = env;
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = expect_args;
    _ = env;
    @panic("testRocExpectFailed not implemented yet");
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    const test_env: *TestRunner = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];

    test_env.interpreter.has_crashed = true;
    const owned_msg = test_env.allocator.dupe(u8, msg_slice) catch |err| {
        std.log.err("Failed to allocate crash message: {}", .{err});
        test_env.interpreter.crash_message = "Failed to store crash message";
        return;
    };
    test_env.interpreter.crash_message = owned_msg;
}

const Evaluation = enum {
    passed,
    failed,
    not_a_bool,
};

// Track test results
const TestResult = struct {
    passed: bool,
    line_number: u32,
    error_msg: ?[]const u8 = null,
};

const TestSummary = struct {
    passed: u32,
    failed: u32,
};

pub const TestRunner = struct {
    allocator: Allocator,
    env: *const ModuleEnv,
    interpreter: Interpreter,
    roc_ops: ?RocOps,
    test_results: std.ArrayList(TestResult),

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *const ModuleEnv,
        stack_memory: *stack.Stack,
        layout_cache: *LayoutStore,
        type_store: *TypeStore,
    ) !TestRunner {
        const runner = TestRunner{
            .allocator = allocator,
            .env = cir,
            .interpreter = try Interpreter.init(allocator, cir, stack_memory, layout_cache, type_store),
            .roc_ops = null,
            .test_results = std.ArrayList(TestResult).init(allocator),
        };

        return runner;
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
        return &(self.roc_ops.?);
    }

    pub fn eval(self: *TestRunner, expr_idx: CIR.Expr.Idx) EvalError!Evaluation {
        const result = try self.interpreter.eval(expr_idx, self.get_ops());
        if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .bool) {
            const is_true = result.asBool();
            if (is_true) {
                return Evaluation.passed;
            } else {
                return Evaluation.failed;
            }
        } else {
            return Evaluation.not_a_bool;
        }
    }

    pub fn eval_all(self: *TestRunner) std.fmt.AllocPrintError!TestSummary {
        var passed: u32 = 0;
        var failed: u32 = 0;
        self.test_results.clearAndFree();

        const statements = self.env.store.sliceStatements(self.env.all_statements);
        for (statements) |stmt_idx| {
            const stmt = self.env.store.getStatement(stmt_idx);
            if (stmt == .s_expect) {
                const region = self.env.store.getStatementRegion(stmt_idx);
                const region_info = self.env.calcRegionInfo(region);
                const line_number = region_info.start_line_idx + 1;
                // TODO this can probably be optimized. Maybe run tests in parallel?
                const result = self.eval(stmt.s_expect.body) catch |err| {
                    failed += 1;
                    const error_msg = try std.fmt.allocPrint(self.allocator, "Test evaluation failed: {}", .{err});
                    try self.test_results.append(.{ .line_number = line_number, .passed = false, .error_msg = error_msg });
                    continue;
                };
                switch (result) {
                    .not_a_bool => {
                        failed += 1;
                        const error_msg = try std.fmt.allocPrint(self.allocator, "Test did not evaluate to a boolean", .{});
                        try self.test_results.append(.{ .line_number = line_number, .passed = false, .error_msg = error_msg });
                    },
                    .failed => {
                        failed += 1;
                        try self.test_results.append(.{ .line_number = line_number, .passed = false });
                    },
                    .passed => {
                        passed += 1;
                        try self.test_results.append(.{ .line_number = line_number, .passed = true });
                    },
                }
            }
        }

        return .{
            .passed = passed,
            .failed = failed,
        };
    }

    pub fn deinit(self: *TestRunner) void {
        self.interpreter.deinit(self.get_ops());
        self.test_results.deinit();
    }
};
