//! Tests for exposed item shadowing validation during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct handling of
//! exposed items that are declared but not implemented, and validation
//! of shadowing behavior during the canonicalization process.

const std = @import("std");
const compile = @import("compile");
const parse = @import("parse");
const base = @import("base");

const Can = @import("../mod.zig").Can;
const ModuleEnv = @import("../ModuleEnv.zig");

const AST = parse.AST;
const tokenize = parse.tokenize;
const testing = std.testing;

test "exposed but not implemented - values" {
    const allocator = testing.allocator;

    const source =
        \\module [foo, bar]
        \\
        \\foo = 42
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");

    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);

    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);

    const root_node_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_node_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);

    // Check that we have an "exposed but not implemented" diagnostic for 'bar'
    var found_bar_error = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .exposed_but_not_implemented => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "bar")) {
                    found_bar_error = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_bar_error);
}

test "exposed but not implemented - types" {
    const allocator = testing.allocator;

    const source =
        \\module [MyType, OtherType]
        \\
        \\MyType : [A, B]
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");

    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);

    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);

    const root_node_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_node_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);

    // Check that we have an "exposed but not implemented" diagnostic for 'OtherType'
    var found_other_type_error = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .exposed_but_not_implemented => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "OtherType")) {
                    found_other_type_error = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_other_type_error);
}

test "redundant exposed entries" {
    const allocator = testing.allocator;
    const source =
        \\module [foo, bar, foo, MyType, bar]
        \\
        \\foo = 42
        \\bar = "hello"
        \\MyType : [A, B]
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Check that we have redundant exposed warnings
    var found_foo_redundant = false;
    var found_bar_redundant = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .redundant_exposed => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "foo")) {
                    found_foo_redundant = true;
                } else if (std.mem.eql(u8, ident_text, "bar")) {
                    found_bar_redundant = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_foo_redundant);
    try testing.expect(found_bar_redundant);
}

test "shadowing with exposed items" {
    const allocator = testing.allocator;
    const source =
        \\module [x, y]
        \\
        \\x = 1
        \\x = 2
        \\
        \\y = "first"
        \\y = "second"
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Check that we have shadowing warnings
    var shadowing_count: usize = 0;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .shadowing_warning => shadowing_count += 1,
            else => {},
        }
    }
    // Should have warnings for both x and y being shadowed
    try testing.expectEqual(@as(usize, 2), shadowing_count);
}

test "shadowing non-exposed items" {
    const allocator = testing.allocator;
    const source =
        \\module []
        \\
        \\notExposed = 1
        \\notExposed = 2
        \\# Shadowing is allowed for non-exposed items
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Check that we still get shadowing warnings for non-exposed items
    var found_shadowing = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .shadowing_warning => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "notExposed")) {
                    found_shadowing = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_shadowing);
}

test "exposed items correctly tracked across shadowing" {
    const allocator = testing.allocator;
    const source =
        \\module [x, y, z]
        \\
        \\x = 1
        \\x = 2
        \\
        \\y = "defined"
        \\
        \\# z is exposed but never defined
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Should have:
    // - Shadowing warning for x
    // - No "exposed but not implemented" for x (it is implemented)
    // - No "exposed but not implemented" for y (it is implemented)
    // - "exposed but not implemented" for z (never defined)
    var found_x_shadowing = false;
    var found_z_not_implemented = false;
    var found_unexpected_not_implemented = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .shadowing_warning => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "x")) {
                    found_x_shadowing = true;
                }
            },
            .exposed_but_not_implemented => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "z")) {
                    found_z_not_implemented = true;
                } else if (std.mem.eql(u8, ident_text, "x") or std.mem.eql(u8, ident_text, "y")) {
                    found_unexpected_not_implemented = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_x_shadowing);
    try testing.expect(found_z_not_implemented);
    try testing.expect(!found_unexpected_not_implemented);
}

test "complex case with redundant, shadowing, and not implemented" {
    const allocator = testing.allocator;
    const source =
        \\module [a, b, a, c, NotImplemented]
        \\
        \\a = 1
        \\a = 2
        \\
        \\b = "hello"
        \\
        \\c = 100
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    var found_a_redundant = false;
    var found_a_shadowing = false;
    var found_not_implemented = false;
    for (env.diagnostics.items) |diag| {
        switch (diag.tag) {
            .redundant_exposed => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "a")) {
                    found_a_redundant = true;
                }
            },
            .shadowing_warning => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "a")) {
                    found_a_shadowing = true;
                }
            },
            .exposed_but_not_implemented => {
                const ident_text = env.common.getIdent(diag.ident);
                if (std.mem.eql(u8, ident_text, "NotImplemented")) {
                    found_not_implemented = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_a_redundant);
    try testing.expect(found_a_shadowing);
    try testing.expect(found_not_implemented);
}

test "exposed_items is populated correctly" {
    const allocator = testing.allocator;
    const source =
        \\module [foo, bar, MyType, foo]
        \\
        \\foo = 42
        \\bar = "hello"
        \\MyType : [A, B]
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Check that exposed_items contains the correct number of items
    // The exposed items were added during canonicalization
    // Should have exactly 3 entries (duplicates not stored)
    try testing.expectEqual(@as(usize, 3), env.common.exposed_items.count());
    // Check that exposed_items contains all exposed items
    const foo_idx = env.common.idents.findByString("foo").?;
    const bar_idx = env.common.idents.findByString("bar").?;
    const mytype_idx = env.common.idents.findByString("MyType").?;
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(foo_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(bar_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(mytype_idx)));
}

test "exposed_items persists after canonicalization" {
    const allocator = testing.allocator;
    const source =
        \\module [x, y, z]
        \\
        \\x = 1
        \\y = 2
        \\# z is not defined
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // All exposed items should be in exposed_items, even those not implemented
    const x_idx = env.common.idents.findByString("x").?;
    const y_idx = env.common.idents.findByString("y").?;
    const z_idx = env.common.idents.findByString("z").?;
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(x_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(y_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(z_idx)));
    // Verify the map persists in env after canonicalization is complete
    try testing.expectEqual(@as(usize, 3), env.common.exposed_items.count());
}

test "exposed_items never has entries removed" {
    const allocator = testing.allocator;
    const source =
        \\module [foo, bar, foo, baz]
        \\
        \\foo = 42
        \\bar = "hello"
        \\baz = 3.14
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // All exposed items should remain in exposed_items
    // Even though foo appears twice and baz is not implemented,
    // exposed_items should have all unique exposed identifiers
    const foo_idx = env.common.idents.findByString("foo").?;
    const bar_idx = env.common.idents.findByString("bar").?;
    const baz_idx = env.common.idents.findByString("baz").?;
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(foo_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(bar_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(baz_idx)));
    // Should have exactly 3 unique entries
    try testing.expectEqual(@as(usize, 3), env.common.exposed_items.count());
}

test "exposed_items handles identifiers with different attributes" {
    const allocator = testing.allocator;
    // Module exposing foo and foo! - these should be treated as different identifiers
    // Note: Using foo and foo! to test that attributes are properly included in the key
    const source =
        \\module [foo, foo!]
        \\
        \\foo = 42
        \\foo! = |x| x + 1
    ;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields(allocator, "Test");
    var ast = try parse.parse(&env.common, allocator);
    defer ast.deinit(allocator);
    var czer = Can.init(&ast, &env.types);
    defer czer.deinit(allocator);
    const root_idx: parse.AST.Node.Idx = @enumFromInt(ast.root_node_idx);
    _ = try czer.canonicalizeFileBlock(allocator, root_idx, env.common.source, &env.common.idents, &env.common, &env.diagnostics);
    // Both should be in exposed_items as separate entries
    const foo_idx = env.common.idents.findByString("foo").?;
    const foo_effectful_idx = env.common.idents.findByString("foo!").?;
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(foo_idx)));
    try testing.expect(env.common.exposed_items.containsById(env.gpa, @bitCast(foo_effectful_idx)));
    // Should have exactly 2 entries - if we only used u29 without attributes, they might incorrectly merge
    try testing.expectEqual(@as(usize, 2), env.common.exposed_items.count());
    // Verify they have different full u32 values (index + attributes)
    const foo_u32 = @as(u32, @bitCast(foo_idx));
    const foo_effectful_u32 = @as(u32, @bitCast(foo_effectful_idx));
    try testing.expect(foo_u32 != foo_effectful_u32);
}
