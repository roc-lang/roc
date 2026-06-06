//! Stack-safety regression tests for canonicalization walks.

const std = @import("std");
const parse = @import("parse");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;

const CoreCtx = @import("ctx").CoreCtx;

fn canonicalizeExprSource(source: []const u8) !void {
    const gpa = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.parseExpr(gpa, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(gpa, gpa);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    _ = try can.canonicalizeExpr(expr_idx) orelse return error.CanonicalizeError;
}

fn canonicalizeModuleSource(source: []const u8) !void {
    const gpa = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.parse(gpa, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(gpa, gpa);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    try can.canonicalizeFile();
}

test "deep unary expression canonicalizes without recursive calls" {
    const gpa = std.testing.allocator;
    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);

    for (0..96) |_| {
        try source.append(gpa, '!');
    }
    try source.appendSlice(gpa, "False");

    try canonicalizeExprSource(source.items);
}

test "deep match pattern canonicalizes without recursive calls" {
    const gpa = std.testing.allocator;
    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);

    try source.appendSlice(gpa, "|x| match x {\n    ");
    for (0..96) |_| {
        try source.append(gpa, '[');
    }
    try source.append(gpa, '_');
    for (0..96) |_| {
        try source.append(gpa, ']');
    }
    try source.appendSlice(gpa, " => 0\n}");

    try canonicalizeExprSource(source.items);
}

test "deep block canonicalizes statements without recursive calls" {
    const gpa = std.testing.allocator;
    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);

    try source.appendSlice(gpa, "module []\n\nmain = {\n");
    for (0..1024) |i| {
        const line = try std.fmt.allocPrint(gpa, "    x{d} = {d}\n", .{ i, i });
        defer gpa.free(line);
        try source.appendSlice(gpa, line);
    }
    try source.appendSlice(gpa, "    x1023\n}\n");

    try canonicalizeModuleSource(source.items);
}

test "deep type annotation canonicalizes without recursive calls" {
    const gpa = std.testing.allocator;
    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);

    try source.appendSlice(gpa, "module []\n\nA : ");
    for (0..96) |_| {
        try source.appendSlice(gpa, "List(");
    }
    try source.appendSlice(gpa, "U64");
    for (0..96) |_| {
        try source.append(gpa, ')');
    }
    try source.append(gpa, '\n');

    try canonicalizeModuleSource(source.items);
}

test "deep nested associated blocks canonicalize without recursive calls" {
    const gpa = std.testing.allocator;
    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);

    try source.appendSlice(gpa, "module []\n\n");
    for (0..48) |i| {
        for (0..i) |_| {
            try source.appendSlice(gpa, "    ");
        }
        const line = try std.fmt.allocPrint(gpa, "T{d} := [T{d}].{{\n", .{ i, i });
        defer gpa.free(line);
        try source.appendSlice(gpa, line);
    }
    for (0..48) |_| {
        try source.appendSlice(gpa, "    ");
    }
    try source.appendSlice(gpa, "value = 1\n");
    var close_idx: usize = 48;
    while (close_idx > 0) {
        close_idx -= 1;
        for (0..close_idx) |_| {
            try source.appendSlice(gpa, "    ");
        }
        try source.appendSlice(gpa, "}\n");
    }

    try canonicalizeModuleSource(source.items);
}
