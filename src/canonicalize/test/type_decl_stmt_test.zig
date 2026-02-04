//! Tests for local type declarations in block contexts.
//!
//! Local type declarations allow defining type aliases, nominal types, and opaque types
//! within function bodies and blocks, scoped to that block.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");

const Allocators = base.Allocators;
const testing = std.testing;
const Ident = base.Ident;
const Statement = CIR.Statement;

test "local type alias is parsed and canonicalized" {
    const source =
        \\|_| {
        \\    MyNum : U64
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    // Check diagnostics - should have no errors
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local nominal type is parsed and canonicalized" {
    const source =
        \\|_| {
        \\    Counter := U64
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local opaque type is parsed and canonicalized" {
    // Use U8 instead of Str since Str is an auto-imported type, not a builtin
    const source =
        \\|_| {
        \\    Secret :: U8
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "nested blocks with local types" {
    // Use builtin types (U64, U8) instead of Str
    const source =
        \\|_| {
        \\    OuterType : U64
        \\    inner = {
        \\        InnerType : U8
        \\        42
        \\    }
        \\    inner
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "multiple local types in same block" {
    // Use builtin types (U64, U8) instead of Str
    const source =
        \\|_| {
        \\    First : U64
        \\    Second : U8
        \\    Third : { a: U64, b: U8 }
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local type with type parameters" {
    // Type parameters use parentheses syntax: MyList(a)
    const source =
        \\|_| {
        \\    MyWrapper(a) : List(a)
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "expression that looks like type decl but isn't - record field" {
    // Record fields use lowercase names with colons: { name: "value" }
    // This should NOT be parsed as a type declaration
    const source =
        \\|_| {
        \\    x = { name: 42, count: 10 }
        \\    x
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    // This should parse as a record, not as a type declaration
    // No type-related errors expected
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var type_decl_errors: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .undeclared_type => type_decl_errors += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), type_decl_errors);
}

test "local type alias can be used in annotation" {
    // Test that a locally defined type alias can be used in a type annotation
    const source =
        \\|_| {
        \\    MyNum : U64
        \\    x : MyNum
        \\    x = 42
        \\    x
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "scopeLookupTypeDecl API is accessible" {
    const gpa = testing.allocator;
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const ast = try parse.parseExpr(&allocators, &env.common);
    defer ast.deinit();

    var can = try Can.init(&env, ast, null);
    defer can.deinit();

    // Enter a scope
    try can.scopeEnter(gpa, true);

    // Look up a type that doesn't exist - should return null
    const my_type_ident = try env.insertIdent(Ident.for_text("MyType"));
    const type_lookup = can.scopeLookupTypeDecl(my_type_ident);

    try testing.expect(type_lookup == null);
}

test "introduceType API is accessible" {
    const gpa = testing.allocator;
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const ast = try parse.parseExpr(&allocators, &env.common);
    defer ast.deinit();

    var can = try Can.init(&env, ast, null);
    defer can.deinit();

    // Enter a scope for local type declarations
    try can.scopeEnter(gpa, true);

    // Create a type header manually
    const type_name = try env.insertIdent(Ident.for_text("LocalType"));
    const type_header = CIR.TypeHeader{
        .name = type_name,
        .relative_name = type_name,
        .args = CIR.TypeAnno.Span{ .span = base.DataSpan.empty() },
    };
    const header_idx = try env.addTypeHeader(type_header, base.Region.zero());

    // Create a type alias statement
    const alias_stmt = Statement{
        .s_alias_decl = .{
            .header = header_idx,
            .anno = .placeholder,
        },
    };
    const stmt_idx = try env.addStatement(alias_stmt, base.Region.zero());

    // Introduce the type into scope
    try can.introduceType(type_name, stmt_idx, base.Region.zero());

    // Verify the type is now in scope
    const type_lookup = can.scopeLookupTypeDecl(type_name);
    try testing.expect(type_lookup != null);
    try testing.expect(type_lookup.? == stmt_idx);
}

test "local type scoping - not visible after exiting block" {
    const gpa = testing.allocator;
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const ast = try parse.parseExpr(&allocators, &env.common);
    defer ast.deinit();

    var can = try Can.init(&env, ast, null);
    defer can.deinit();

    // Enter outer scope
    try can.scopeEnter(gpa, true);

    // Enter inner scope
    try can.scopeEnter(gpa, false);

    // Create and introduce a local type in the inner scope
    const type_name = try env.insertIdent(Ident.for_text("InnerType"));
    const type_header = CIR.TypeHeader{
        .name = type_name,
        .relative_name = type_name,
        .args = CIR.TypeAnno.Span{ .span = base.DataSpan.empty() },
    };
    const header_idx = try env.addTypeHeader(type_header, base.Region.zero());
    const alias_stmt = Statement{
        .s_alias_decl = .{
            .header = header_idx,
            .anno = .placeholder,
        },
    };
    const stmt_idx = try env.addStatement(alias_stmt, base.Region.zero());
    try can.introduceType(type_name, stmt_idx, base.Region.zero());

    // Type should be visible in inner scope
    const lookup_in_inner = can.scopeLookupTypeDecl(type_name);
    try testing.expect(lookup_in_inner != null);

    // Exit inner scope
    try can.scopeExit(gpa);

    // Type should NOT be visible in outer scope anymore
    const lookup_in_outer = can.scopeLookupTypeDecl(type_name);
    try testing.expect(lookup_in_outer == null);
}
