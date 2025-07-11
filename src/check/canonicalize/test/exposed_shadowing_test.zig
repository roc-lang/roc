//! TODO

const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const AST = @import("../../parse/AST.zig");
const CIR = @import("../CIR.zig");
const canonicalize = @import("../../canonicalize.zig");
const parse = @import("../../parse.zig");
const tokenize = @import("../../parse/tokenize.zig");

test "exposed but not implemented - values" {
    const allocator = testing.allocator;

    const source =
        \\module [foo, bar]
        \\
        \\foo = 42
    ;

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we have an "exposed but not implemented" diagnostic for 'bar'
    var found_bar_error = false;
    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .exposed_but_not_implemented => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we have an "exposed but not implemented" diagnostic for 'OtherType'
    var found_other_type_error = false;
    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .exposed_but_not_implemented => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we have redundant exposed warnings
    var found_foo_redundant = false;
    var found_bar_redundant = false;
    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .redundant_exposed => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we have shadowing warnings
    var shadowing_count: usize = 0;
    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we still get shadowing warnings for non-exposed items
    var found_shadowing = false;
    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .shadowing_warning => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Should have:
    // - Shadowing warning for x
    // - No "exposed but not implemented" for x (it is implemented)
    // - No "exposed but not implemented" for y (it is implemented)
    // - "exposed but not implemented" for z (never defined)

    var found_x_shadowing = false;
    var found_z_not_implemented = false;
    var found_unexpected_not_implemented = false;

    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .shadowing_warning => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
                if (std.mem.eql(u8, ident_text, "x")) {
                    found_x_shadowing = true;
                }
            },
            .exposed_but_not_implemented => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    var found_a_redundant = false;
    var found_a_shadowing = false;
    var found_not_implemented = false;

    for (0..cir.store.scratch_diagnostics.top()) |i| {
        const diag_idx = cir.store.scratch_diagnostics.items.items[i];
        const diag = cir.store.getDiagnostic(diag_idx);
        switch (diag) {
            .redundant_exposed => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
                if (std.mem.eql(u8, ident_text, "a")) {
                    found_a_redundant = true;
                }
            },
            .shadowing_warning => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
                if (std.mem.eql(u8, ident_text, "a")) {
                    found_a_shadowing = true;
                }
            },
            .exposed_but_not_implemented => |d| {
                const ident_text = cir.env.idents.getText(d.ident);
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

test "exposed_by_str is populated correctly" {
    const allocator = testing.allocator;

    const source =
        \\module [foo, bar, MyType, foo]
        \\
        \\foo = 42
        \\bar = "hello"
        \\MyType : [A, B]
    ;

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that exposed_by_str contains all exposed items
    try testing.expect(env.exposed_by_str.contains("foo"));
    try testing.expect(env.exposed_by_str.contains("bar"));
    try testing.expect(env.exposed_by_str.contains("MyType"));

    // Should have exactly 3 entries (duplicates not stored)
    try testing.expectEqual(@as(usize, 3), env.exposed_by_str.count());
}

test "exposed_by_str persists after canonicalization" {
    const allocator = testing.allocator;

    const source =
        \\module [x, y, z]
        \\
        \\x = 1
        \\y = 2
        \\# z is not defined
    ;

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // All exposed items should be in exposed_by_str, even those not implemented
    try testing.expect(env.exposed_by_str.contains("x"));
    try testing.expect(env.exposed_by_str.contains("y"));
    try testing.expect(env.exposed_by_str.contains("z"));

    // Verify the map persists in env after canonicalization is complete
    try testing.expectEqual(@as(usize, 3), env.exposed_by_str.count());
}

test "exposed_by_str never has entries removed" {
    const allocator = testing.allocator;

    const source =
        \\module [foo, bar, foo, baz]
        \\
        \\foo = 42
        \\bar = "hello"
        \\baz = 3.14
    ;

    var env = base.ModuleEnv.init(allocator, try allocator.dupe(u8, source), try allocator.dupe(u8, "test.roc"));
    defer env.deinit();

    var ast = parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // All exposed items should remain in exposed_by_str
    // Even though foo appears twice and baz is not implemented,
    // exposed_by_str should have all unique exposed identifiers
    try testing.expect(env.exposed_by_str.contains("foo"));
    try testing.expect(env.exposed_by_str.contains("bar"));
    try testing.expect(env.exposed_by_str.contains("baz"));

    // Should have exactly 3 unique entries
    try testing.expectEqual(@as(usize, 3), env.exposed_by_str.count());
}
