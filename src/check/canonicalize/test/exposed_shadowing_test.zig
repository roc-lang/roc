//! Tests for exposed item shadowing validation during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct handling of
//! exposed items that are declared but not implemented, and validation
//! of shadowing behavior during the canonicalization process.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
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

test "exposed_items is populated correctly" {
    const allocator = testing.allocator;

    const source =
        \\module [foo, bar, MyType, foo]
        \\
        \\foo = 42
        \\bar = "hello"
        \\MyType : [A, B]
    ;

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that exposed_items contains all exposed items
    // First find the intern indices for the exposed items
    var foo_idx: ?u32 = null;
    var bar_idx: ?u32 = null;
    var my_type_idx: ?u32 = null;

    // The exposed_items map contains the actual intern indices used during canonicalization
    // We need to find these specific indices, not just any index for the same text
    for (env.exposed_items.items.entries.items) |entry| {
        const text = env.idents.interner.getText(@enumFromInt(entry.key));
        if (std.mem.eql(u8, text, "foo") and foo_idx == null) {
            foo_idx = entry.key;
        } else if (std.mem.eql(u8, text, "bar") and bar_idx == null) {
            bar_idx = entry.key;
        } else if (std.mem.eql(u8, text, "MyType") and my_type_idx == null) {
            my_type_idx = entry.key;
        }
    }

    try testing.expect(foo_idx != null);
    try testing.expect(bar_idx != null);
    try testing.expect(my_type_idx != null);

    // Freeze the environment to ensure maps are sorted and deduplicated
    try env.freeze();

    try testing.expect(env.exposed_items.isExposed(allocator, foo_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, bar_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, my_type_idx.?));

    // ExposedItems behavior explanation:
    // The new ExposedItems combines the old exposed_by_str and exposed_nodes into a single collection.
    // In this test: module [foo, bar, MyType, foo]
    //
    // We get 6 entries because:
    // 1. createExposedScope adds each exposed item (including duplicates) with node index 0
    //    - "foo" (first occurrence) -> intern index A, node index 0
    //    - "bar" -> intern index B, node index 0
    //    - "MyType" -> intern index C, node index 0
    //    - "foo" (second occurrence) -> intern index D, node index 0 (different intern index!)
    // 2. canonicalizeFile adds items with definitions using their actual node indices
    //    - "foo" definition -> intern index E, node index > 0
    //    - "bar" definition -> intern index F, node index > 0
    //
    // Total: 6 entries (4 from declarations + 2 from definitions)
    // The interner creates unique indices for each occurrence, so no deduplication occurs.
    try testing.expectEqual(@as(usize, 6), env.exposed_items.count());
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // All exposed items should be in exposed_items, even those not implemented
    // First find the intern indices
    var x_idx: ?u32 = null;
    var y_idx: ?u32 = null;
    var z_idx: ?u32 = null;

    // Find the intern indices that were actually stored in exposed_items
    for (env.exposed_items.items.entries.items) |entry| {
        const text = env.idents.interner.getText(@enumFromInt(entry.key));
        if (std.mem.eql(u8, text, "x") and x_idx == null) {
            x_idx = entry.key;
        } else if (std.mem.eql(u8, text, "y") and y_idx == null) {
            y_idx = entry.key;
        } else if (std.mem.eql(u8, text, "z") and z_idx == null) {
            z_idx = entry.key;
        }
    }

    try testing.expect(x_idx != null);
    try testing.expect(y_idx != null);
    try testing.expect(z_idx != null);

    // Freeze the environment to ensure maps are sorted
    try env.freeze();

    try testing.expect(env.exposed_items.isExposed(allocator, x_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, y_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, z_idx.?));

    // ExposedItems behavior explanation:
    // In this test: module [x, y, z] with x=1, y=2, and z undefined
    //
    // We get 5 entries because:
    // 1. createExposedScope adds all declared items with node index 0:
    //    - "x" -> intern index A, node index 0
    //    - "y" -> intern index B, node index 0
    //    - "z" -> intern index C, node index 0
    // 2. canonicalizeFile adds only items with definitions:
    //    - "x" definition -> intern index D, node index > 0
    //    - "y" definition -> intern index E, node index > 0
    //    - "z" has no definition, so no additional entry
    //
    // Total: 5 entries (3 from declarations + 2 from definitions)
    // "z" appears only once since it has no definition to add a second entry.
    try testing.expectEqual(@as(usize, 5), env.exposed_items.count());
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

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // All exposed items should remain in exposed_items
    // Even though foo appears twice and baz is not implemented,
    // exposed_items should have all unique exposed identifiers
    // First find the intern indices
    var foo_idx: ?u32 = null;
    var bar_idx: ?u32 = null;
    var baz_idx: ?u32 = null;
    // Find the intern indices that were actually stored in exposed_items
    for (env.exposed_items.items.entries.items) |entry| {
        const text = env.idents.interner.getText(@enumFromInt(entry.key));
        if (std.mem.eql(u8, text, "foo") and foo_idx == null) {
            foo_idx = entry.key;
        } else if (std.mem.eql(u8, text, "bar") and bar_idx == null) {
            bar_idx = entry.key;
        } else if (std.mem.eql(u8, text, "baz") and baz_idx == null) {
            baz_idx = entry.key;
        }
    }

    try testing.expect(foo_idx != null);
    try testing.expect(bar_idx != null);
    try testing.expect(baz_idx != null);

    // Freeze the environment to ensure maps are sorted
    try env.freeze();

    try testing.expect(env.exposed_items.isExposed(allocator, foo_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, bar_idx.?));
    try testing.expect(env.exposed_items.isExposed(allocator, baz_idx.?));

    // ExposedItems behavior explanation:
    // In this test: module [foo, bar, foo, baz] with all items having definitions
    //
    // We get 7 entries because:
    // 1. createExposedScope adds each declared item (including duplicates) with node index 0:
    //    - "foo" (first occurrence) -> intern index A, node index 0
    //    - "bar" -> intern index B, node index 0
    //    - "foo" (second occurrence) -> intern index C, node index 0 (different intern index!)
    //    - "baz" -> intern index D, node index 0
    // 2. canonicalizeFile adds all items with definitions:
    //    - "foo" definition -> intern index E, node index > 0
    //    - "bar" definition -> intern index F, node index > 0
    //    - "baz" definition -> intern index G, node index > 0
    //
    // Total: 7 entries (4 from declarations + 3 from definitions)
    // Even though "foo" appears twice in the declaration, each gets a unique intern index.
    try testing.expectEqual(@as(usize, 7), env.exposed_items.count());
}
