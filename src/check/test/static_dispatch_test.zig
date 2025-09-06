//! Tests for static dispatch on nominal types with method-style syntax

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const parse = @import("parse");
const types = @import("types");
const can = @import("can");
const check = @import("../mod.zig");

const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Check = check.Check;

// Helper to parse and canonicalize source code for testing
fn parseAndCanonicalizeSource(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    expr_idx: CIR.Expr.Idx,
} {
    // Initialize the ModuleEnv
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    module_env.common.source = source;
    try module_env.common.calcLineStarts(module_env.gpa);

    // Parse the source code
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Initialize CIR fields
    try module_env.initCIRFields(allocator, "test");

    // Create CIR
    const cir_inst = try allocator.create(CIR);
    cir_inst.* = CIR.init(parse_ast, &module_env.types);

    // Canonicalize the expression
    const root_node = @as(parse.AST.Node.Idx, @enumFromInt(parse_ast.root_node_idx));
    const expr_idx = try cir_inst.canonicalizeFileBlock(allocator, root_node, source, &module_env.common.idents, &module_env.common, null);

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir_inst,
        .expr_idx = expr_idx,
    };
}

test "static dispatch - dot access on records" {
    // Test that dot access on records works for field access
    // This is a precursor to static dispatch on nominal types
    const source = "{ x: 42, y: 99 }.x";

    const resources = try parseAndCanonicalizeSource(test_allocator, source);
    defer {
        resources.module_env.deinit();
        test_allocator.destroy(resources.module_env);
        resources.parse_ast.deinit(test_allocator);
        test_allocator.destroy(resources.parse_ast);
        resources.cir.deinit(test_allocator);
        test_allocator.destroy(resources.cir);
    }

    // Create type checker
    var regions = base.Region.List{};
    var checker = try Check.initForCIR(test_allocator, &resources.module_env.types, &regions);
    defer checker.deinit();

    // Type check the expression
    const result_var = try checker.checkCIRExpr(CIR, resources.cir, resources.expr_idx);

    // The result should be an integer type
    const resolved = resources.module_env.types.resolveVar(result_var);

    // For now, just check that type checking completes without errors
    // The actual type should be Num (integer) once dot access is fully implemented
    try testing.expect(resolved.desc.content != .err);
}

test "static dispatch - function application" {
    // Test that we can type check function application
    // This is another precursor to static dispatch
    const source = "(|x| x + 1)(42)";

    const resources = try parseAndCanonicalizeSource(test_allocator, source);
    defer {
        resources.module_env.deinit();
        test_allocator.destroy(resources.module_env);
        resources.parse_ast.deinit(test_allocator);
        test_allocator.destroy(resources.parse_ast);
        resources.cir.deinit(test_allocator);
        test_allocator.destroy(resources.cir);
    }

    // Create type checker
    var regions = base.Region.List{};
    var checker = try Check.initForCIR(test_allocator, &resources.module_env.types, &regions);
    defer checker.deinit();

    // Type check the expression
    const result_var = try checker.checkCIRExpr(CIR, resources.cir, resources.expr_idx);

    // The result should be a number type
    const resolved = resources.module_env.types.resolveVar(result_var);

    // Check that type checking completes without errors
    try testing.expect(resolved.desc.content != .err);
}

// NOTE: Full static dispatch tests with nominal types are pending:
// - Need nominal type value creation (e.g., `Person { name: "Alice" }`)
// - Need module import infrastructure for cross-module method calls
// - Need dot access on nominal types to resolve to method calls
//
// The type checker infrastructure for static dispatch exists:
// - Can detect when a dot access is on a nominal type
// - Can find the origin module where the type was defined
// - Can look up methods in that module's exports
// - Can import and unify the method types correctly
//
// Once nominal type values and module imports are available,
// we can enable full static dispatch tests.
