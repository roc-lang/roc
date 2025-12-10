//! Tests for type declarations in statement context (local type aliases)
//!
//! Note: The parser currently only produces `type_decl` statements for top-level
//! and associated block contexts. When parsing with `parseStatement`, type
//! declarations are parsed as `type_anno` (type annotations) instead.
//!
//! These tests verify that the canonicalizer correctly handles the type_decl
//! statement variant when it appears in block context, even though the current
//! parser doesn't produce this variant outside of top-level/associated contexts.
//! The implementation is ready for when the parser is updated to support local
//! type declarations.

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const types = @import("types");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");

const Ident = base.Ident;
const Statement = CIR.Statement;

test "scopeLookupTypeDecl is public and works" {
    // Test that scopeLookupTypeDecl is accessible (made public for local type support)
    const gpa = std.testing.allocator;

    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    // Enter a scope
    try can.scopeEnter(gpa, true);

    // Look up a type that doesn't exist - should return null
    const my_type_ident = try env.insertIdent(Ident.for_text("MyType"));
    const type_lookup = can.scopeLookupTypeDecl(my_type_ident);

    try std.testing.expect(type_lookup == null);
}

test "introduceType works for local types" {
    // Test that introduceType correctly adds types to the current scope
    const gpa = std.testing.allocator;

    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
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
    try std.testing.expect(type_lookup != null);
    try std.testing.expect(type_lookup.? == stmt_idx);
}

test "local type is scoped correctly" {
    // Test that local types are properly scoped - not visible after exiting scope
    const gpa = std.testing.allocator;

    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
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
    try std.testing.expect(lookup_in_inner != null);

    // Exit inner scope
    try can.scopeExit(gpa);

    // Type should NOT be visible in outer scope anymore
    const lookup_in_outer = can.scopeLookupTypeDecl(type_name);
    try std.testing.expect(lookup_in_outer == null);
}
