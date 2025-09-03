//! Module environment tests for the new CIR architecture
//! Tests the module environment's ability to manage type variables, expressions, and imports

const std = @import("std");
const base = @import("base");
const types = @import("types");
const canonicalize = @import("can");
const parse = @import("parse");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = canonicalize.ModuleEnv;
const CIR = canonicalize.CIR;
const AST = parse.AST;

test "module env - create and destroy" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    try testing.expectEqualStrings(env.module_name, "TestModule");
}

test "module env - add expression with type" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a simple CIR with an integer literal
    // Need to create an AST first
    var ast = AST{};
    var cir = CIR.init(&ast, &env.types);
    defer cir.deinit(test_allocator);

    // Create an integer literal expression
    const expr_idx = try cir.addNum(.{ .int = .{ .val = 42, .width = .i32 } }, .{ .offset = 0 });

    // The expression should have an associated type variable
    const type_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Verify the type variable exists in the type store
    const resolved = env.types.resolveVar(type_var);
    try testing.expect(resolved.desc.content == .flex_var or resolved.desc.content == .structure);
}

test "module env - identifier management" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Get the identifier store
    const idents = env.getIdents();

    // Add some identifiers
    const foo_idx = try idents.getOrCreate("foo", test_allocator);
    const bar_idx = try idents.getOrCreate("bar", test_allocator);
    const foo_idx2 = try idents.getOrCreate("foo", test_allocator); // Should reuse same index

    try testing.expect(foo_idx == foo_idx2);
    try testing.expect(foo_idx != bar_idx);

    // Retrieve identifiers
    const foo_str = idents.getText(foo_idx);
    const bar_str = idents.getText(bar_idx);

    try testing.expectEqualStrings(foo_str, "foo");
    try testing.expectEqualStrings(bar_str, "bar");
}

test "module env - string literal management" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Add string literals to byte_slices
    const hello_idx = try env.byte_slices.intern("Hello, World!", test_allocator);
    const empty_idx = try env.byte_slices.intern("", test_allocator);
    const unicode_idx = try env.byte_slices.intern("🦀 Rust 🦀", test_allocator);

    // Retrieve string literals
    const hello_str = env.byte_slices.get(hello_idx);
    const empty_str = env.byte_slices.get(empty_idx);
    const unicode_str = env.byte_slices.get(unicode_idx);

    try testing.expectEqualStrings(hello_str, "Hello, World!");
    try testing.expectEqualStrings(empty_str, "");
    try testing.expectEqualStrings(unicode_str, "🦀 Rust 🦀");

    // Same string should reuse index
    const hello_idx2 = try env.byte_slices.intern("Hello, World!", test_allocator);
    try testing.expect(hello_idx == hello_idx2);
}

test "module env - type variable association" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a CIR
    // Need to create an AST first
    var ast = AST{};
    var cir = CIR.init(&ast, &env.types);
    defer cir.deinit(test_allocator);

    // Create various expressions and verify they get type variables
    const int_expr = try cir.addNum(.{ .int = .{ .val = 42, .width = .i32 } }, .{ .offset = 0 });
    const str_idx = try env.byte_slices.intern("test", test_allocator);
    const str_expr = try cir.addStr(str_idx, .{ .offset = 10 });
    const bool_expr = try cir.addIntern(.{ .tag = @intFromEnum(CIR.Builtin.Bool.true_), .data = 0 }, .{ .offset = 20 });

    // Each expression should have a unique type variable
    const int_var = @as(types.Var, @enumFromInt(@intFromEnum(int_expr)));
    const str_var = @as(types.Var, @enumFromInt(@intFromEnum(str_expr)));
    const bool_var = @as(types.Var, @enumFromInt(@intFromEnum(bool_expr)));

    try testing.expect(@intFromEnum(int_var) != @intFromEnum(str_var));
    try testing.expect(@intFromEnum(str_var) != @intFromEnum(bool_var));
    try testing.expect(@intFromEnum(int_var) != @intFromEnum(bool_var));
}

test "module env - pattern management" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a CIR
    // Need to create an AST first
    var ast = AST{};
    var cir = CIR.init(&ast, &env.types);
    defer cir.deinit(test_allocator);

    // Create a pattern (identifier pattern)
    const foo_ident = try env.getIdents().getOrCreate("foo", test_allocator);
    const pattern_idx = try cir.addPattern(.{ .identifier = foo_ident }, .{ .offset = 0 });

    // Pattern should have an associated type variable
    const pattern_var = @as(types.Var, @enumFromInt(@intFromEnum(pattern_idx)));

    // Verify it exists
    const resolved = env.types.resolveVar(pattern_var);
    try testing.expect(resolved.desc.content == .flex_var or resolved.desc.content == .structure);
}

test "module env - scope management" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a CIR with scope tracking
    // Need to create an AST first
    var ast = AST{};
    var cir = CIR.init(&ast, &env.types);
    defer cir.deinit(test_allocator);

    // Create a scope
    const can = @import("can");
    const Scope = can.Scope;
    var scope = Scope.init(false);
    defer scope.deinit(test_allocator);

    // Add an identifier to the scope
    const x_ident = try env.getIdents().getOrCreate("x", test_allocator);
    const x_pattern = try cir.addPattern(.{ .identifier = x_ident }, .{ .offset = 0 });
    try scope.idents.put(test_allocator, x_ident, x_pattern);

    // Lookup should find it
    const found_pattern = scope.idents.get(x_ident);
    try testing.expect(found_pattern != null);
    try testing.expect(found_pattern.? == x_pattern);

    // Non-existent identifier should return null
    const y_ident = try env.getIdents().getOrCreate("y", test_allocator);
    const not_found = scope.idents.get(y_ident);
    try testing.expect(not_found == null);
}

test "module env - binary operation creation" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a CIR
    // Need to create an AST first
    var ast = AST{};
    var cir = CIR.init(&ast, &env.types);
    defer cir.deinit(test_allocator);

    // Create operands
    const lhs = try cir.addNum(.{ .int = .{ .val = 2, .width = .i32 } }, .{ .offset = 0 });
    const rhs = try cir.addNum(.{ .int = .{ .val = 3, .width = .i32 } }, .{ .offset = 5 });

    // Create a binary operation (2 + 3)
    const add_expr = try cir.addIntern(.{
        .tag = @intFromEnum(CIR.Builtin.BinOp.plus),
        .data = @bitCast(CIR.BinOp{ .lhs = lhs, .rhs = rhs }),
    }, .{ .offset = 0 });

    // The operation should have a type variable
    const add_var = @as(types.Var, @enumFromInt(@intFromEnum(add_expr)));
    const resolved = env.types.resolveVar(add_var);
    try testing.expect(resolved.desc.content == .flex_var or resolved.desc.content == .structure);
}

test "module env - function type creation" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a function type: Int -> Int
    const int_var = try env.types.fresh();
    const int_content = types.Content{ .structure = .{ .int = .i32 } };
    try env.types.setVarContent(int_var, int_content);

    // Create function type
    const func_var = try env.types.fresh();
    const args_range = try env.types.appendVars(&[_]types.Var{int_var});
    const func_content = types.Content{ .structure = .{ .fn_pure = .{
        .args = args_range,
        .ret = int_var,
    } } };
    try env.types.setVarContent(func_var, func_content);

    // Verify the function type
    const resolved = env.types.resolveVar(func_var);
    switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .fn_pure => |f| {
                try testing.expect(f.ret == int_var);
                const args = env.types.sliceVars(f.args);
                try testing.expect(args.len == 1);
                try testing.expect(args[0] == int_var);
            },
            else => try testing.expect(false),
        },
        else => try testing.expect(false),
    }
}

test "module env - list type creation" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a List Int type
    const int_var = try env.types.fresh();
    const int_content = types.Content{ .structure = .{ .int = .i32 } };
    try env.types.setVarContent(int_var, int_content);

    const list_var = try env.types.mkList(test_allocator, env.getIdents(), int_var, null);

    // Verify the list type
    const resolved = env.types.resolveVar(list_var);
    switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .list => |elem| {
                try testing.expect(elem == int_var);
            },
            else => try testing.expect(false),
        },
        else => try testing.expect(false),
    }
}

test "module env - record type creation" {
    const env = try test_allocator.create(ModuleEnv);
    defer test_allocator.destroy(env);
    env.* = try ModuleEnv.init(test_allocator, "TestModule");
    defer env.deinit();

    // Create a record type { x: Int, y: String }
    const int_var = try env.types.fresh();
    const int_content = types.Content{ .structure = .{ .int = .i32 } };
    try env.types.setVarContent(int_var, int_content);

    const str_var = try env.types.mkStr(test_allocator, env.getIdents(), null);

    // Create field names
    const x_ident = try env.getIdents().getOrCreate("x", test_allocator);
    const y_ident = try env.getIdents().getOrCreate("y", test_allocator);

    // Create record type
    var fields = std.ArrayList(types.RecordField).init(test_allocator);
    defer fields.deinit();

    try fields.append(.{ .name = x_ident, .type_var = int_var });
    try fields.append(.{ .name = y_ident, .type_var = str_var });

    const record_var = try env.types.mkRecord(test_allocator, env.getIdents(), fields.items, null);

    // Verify the record type
    const resolved = env.types.resolveVar(record_var);
    switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .record => |r| {
                const record_fields = env.types.sliceRecordFields(r.fields);
                try testing.expect(record_fields.len == 2);
                try testing.expect(record_fields[0].name == x_ident);
                try testing.expect(record_fields[0].type_var == int_var);
                try testing.expect(record_fields[1].name == y_ident);
                try testing.expect(record_fields[1].type_var == str_var);
            },
            else => try testing.expect(false),
        },
        else => try testing.expect(false),
    }
}
