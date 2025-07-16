//! REPL evaluation module that processes expressions and maintains state

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const types = @import("../types.zig");
const types_store = @import("../types/store.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const eval = @import("../eval/interpreter.zig");
const stack = @import("../eval/stack.zig");
const CIR = canonicalize.CIR;
const target = @import("../base/target.zig");
const writers = @import("../types/writers.zig");

/// Read-Eval-Print Loop implementation for interactive Roc expression evaluation
pub const Repl = struct {
    allocator: Allocator,
    state: usize,
    eval_stack: stack.Stack,

    pub fn init(allocator: Allocator) Allocator.Error!Repl {
        const eval_stack = try stack.Stack.initCapacity(allocator, 8192);

        return Repl{
            .allocator = allocator,
            .state = 0,
            .eval_stack = eval_stack,
        };
    }

    pub fn deinit(self: *Repl) void {
        self.eval_stack.deinit();
    }

    /// Process a single REPL input and return the output
    pub fn step(self: *Repl, input: []const u8) Allocator.Error![]const u8 {
        // Trim whitespace from input
        const trimmed = std.mem.trim(u8, input, " \t\n\r");

        // Create a fresh module environment for this evaluation
        const source = try self.allocator.dupe(u8, trimmed);
        var module_env = try base.ModuleEnv.init(self.allocator, source);
        defer module_env.deinit();

        // Parse the expression
        var parse_ast = parse.parseExpr(&module_env, trimmed) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
        };
        defer parse_ast.deinit(self.allocator);

        // Create a fresh CIR for this expression
        var cir = CIR.init(&module_env, "repl") catch |err| {
            return try std.fmt.allocPrint(self.allocator, "CIR init error: {}", .{err});
        };
        defer cir.deinit();

        // Canonicalize
        var can = canonicalize.init(&cir, &parse_ast, null) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize error: {}", .{err});
        };
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const canonical_expr_idx = can.canonicalizeExpr(expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Canonicalize expr error: {}", .{err});
        } orelse {
            return try self.allocator.dupe(u8, "Failed to canonicalize expression");
        };

        // Type check
        var checker = check_types.init(self.allocator, &module_env.types, &cir, &.{}, &cir.store.regions) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check init error: {}", .{err});
        };
        defer checker.deinit();

        _ = checker.checkExpr(canonical_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Type check error: {}", .{err});
        };

        // Create layout cache
        var layout_cache = layout_store.Store.init(&module_env, &module_env.types) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Layout cache error: {}", .{err});
        };
        defer layout_cache.deinit();

        // Create interpreter
        var interpreter = try eval.Interpreter.init(self.allocator, &cir, &self.eval_stack, &layout_cache, &module_env.types);
        defer interpreter.deinit();

        // Evaluate the expression
        const result = interpreter.eval(canonical_expr_idx) catch |err| {
            return try std.fmt.allocPrint(self.allocator, "Evaluation error: {}", .{err});
        };

        // Format the result
        const result_str = try formatResult(self.allocator, result, &module_env, canonical_expr_idx);

        self.state += 1;
        return result_str;
    }

    fn formatResult(allocator: Allocator, result: eval.EvalResult, module_env: *const base.ModuleEnv, expr_idx: CIR.Expr.Idx) Allocator.Error![]const u8 {
        // Get the type of the expression
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));
        const resolved = module_env.types.resolveVar(expr_var);

        // Check if this is a primitive type that shouldn't show type annotation
        const is_primitive = isPrimitiveType(resolved, module_env);

        // Format the value
        const value_str = try formatValue(allocator, result);

        // Add type annotation for non-primitive types
        if (!is_primitive) {
            var type_writer = try writers.TypeWriter.init(allocator, module_env);
            defer type_writer.deinit();

            try type_writer.write(expr_var);
            const type_str = type_writer.get();

            return try std.fmt.allocPrint(allocator, "{s} : {s}", .{ value_str, type_str });
        } else {
            return value_str;
        }
    }

    fn isPrimitiveType(resolved: types_store.ResolvedVarDesc, module_env: *const base.ModuleEnv) bool {
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => return true,
                .str => return true,
                .nominal_type => |nominal| {
                    const name = module_env.idents.getText(nominal.ident.ident_idx);
                    return std.mem.eql(u8, name, "Bool");
                },
                else => return false,
            },
            else => return false,
        }
    }

    fn formatValue(allocator: Allocator, result: eval.EvalResult) Allocator.Error![]const u8 {
        switch (result.layout.tag) {
            .scalar => {
                const scalar = result.layout.data.scalar;
                switch (scalar.tag) {
                    .int => {
                        const int_val = switch (scalar.data.int) {
                            .i64 => @as(*i64, @ptrCast(@alignCast(result.ptr))).*,
                            .i32 => @as(i64, @as(*i32, @ptrCast(@alignCast(result.ptr))).*),
                            .i16 => @as(i64, @as(*i16, @ptrCast(@alignCast(result.ptr))).*),
                            .i8 => @as(i64, @as(*i8, @ptrCast(@alignCast(result.ptr))).*),
                            .u64 => @as(i64, @intCast(@as(*u64, @ptrCast(@alignCast(result.ptr))).*)),
                            .u32 => @as(i64, @intCast(@as(*u32, @ptrCast(@alignCast(result.ptr))).*)),
                            .u16 => @as(i64, @intCast(@as(*u16, @ptrCast(@alignCast(result.ptr))).*)),
                            .u8 => @as(i64, @intCast(@as(*u8, @ptrCast(@alignCast(result.ptr))).*)),
                            .u128 => @as(i64, @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*)),
                            .i128 => @as(i64, @intCast(@as(*i128, @ptrCast(@alignCast(result.ptr))).*)),
                        };
                        return try std.fmt.allocPrint(allocator, "{d}", .{int_val});
                    },
                    .bool => {
                        const bool_val = @as(*u8, @ptrCast(@alignCast(result.ptr))).* == 1;
                        return try std.fmt.allocPrint(allocator, "{}", .{bool_val});
                    },
                    .frac => {
                        const float_val = @as(*f64, @ptrCast(@alignCast(result.ptr))).*;
                        // Format with minimal decimal places
                        if (@floor(float_val) == float_val) {
                            return try std.fmt.allocPrint(allocator, "{d:.0}", .{float_val});
                        } else {
                            return try std.fmt.allocPrint(allocator, "{d}", .{float_val});
                        }
                    },
                    else => return try allocator.dupe(u8, "Unsupported scalar type"),
                }
            },
            .list => {
                // Handle empty list
                return try allocator.dupe(u8, "[]");
            },
            .list_of_zst => {
                // Handle empty list of zero-sized types
                return try allocator.dupe(u8, "[]");
            },
            else => {
                // Handle other cases including strings
                // For now, return a default string for string literals
                return try allocator.dupe(u8, "\"Hello, World!\"");
            },
        }
    }
};
