const std = @import("std");
const base = @import("base");
const parse = @import("parse");

const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const Ident = base.Ident;
const Region = base.Region;
const Scope = @import("../Scope.zig");
const Pattern = CIR.Pattern;
const TypeAnno = CIR.TypeAnno;

/// Context helper for Scope tests
const ScopeTestContext = struct {
    self: Can,
    module_env: *ModuleEnv,
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) !ScopeTestContext {
        // heap allocate ModuleEnv for testing
        const module_env = try gpa.create(ModuleEnv);
        module_env.* = try ModuleEnv.init(gpa, "");
        try module_env.initCIRFields(gpa, "test");

        return ScopeTestContext{
            .self = try Can.init(module_env, undefined, null),
            .module_env = module_env,
            .gpa = gpa,
        };
    }

    fn deinit(ctx: *ScopeTestContext) void {
        ctx.self.deinit();
        ctx.module_env.deinit();
        ctx.gpa.destroy(ctx.module_env);
    }
};

test "basic scope initialization" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Test that we start with one scope (top-level)
    try std.testing.expect(ctx.self.scopes.items.len == 1);
}

test "empty scope has no items" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("foo"));
    const result = ctx.self.scopeLookup(.ident, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .not_found = {} }, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("foo"));
    const bar_ident = try ctx.module_env.insertIdent(Ident.for_text("bar"));
    const foo_pattern: Pattern.Idx = @enumFromInt(1);
    const bar_pattern: Pattern.Idx = @enumFromInt(2);

    // Add identifiers
    const foo_result = ctx.self.scopeIntroduceInternal(gpa, .ident, foo_ident, foo_pattern, false, true);
    const bar_result = ctx.self.scopeIntroduceInternal(gpa, .ident, bar_ident, bar_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, foo_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, bar_result);

    // Lookup should find them
    const foo_lookup = ctx.self.scopeLookup(.ident, foo_ident);
    const bar_lookup = ctx.self.scopeLookup(.ident, bar_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = foo_pattern }, foo_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = bar_pattern }, bar_lookup);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const x_ident = try ctx.module_env.insertIdent(Ident.for_text("x"));
    const outer_pattern: Pattern.Idx = @enumFromInt(1);
    const inner_pattern: Pattern.Idx = @enumFromInt(2);

    // Add x to outer scope
    const outer_result = ctx.self.scopeIntroduceInternal(gpa, .ident, x_ident, outer_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, outer_result);

    // Enter new scope
    try ctx.self.scopeEnter(gpa, false);

    // x from outer scope should still be visible
    const outer_lookup = ctx.self.scopeLookup(.ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, outer_lookup);

    // Add x to inner scope (shadows outer)
    const inner_result = ctx.self.scopeIntroduceInternal(gpa, .ident, x_ident, inner_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .shadowing_warning = outer_pattern }, inner_result);

    // Now x should resolve to inner scope
    const inner_lookup = ctx.self.scopeLookup(.ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = inner_pattern }, inner_lookup);

    // Exit inner scope
    try ctx.self.scopeExit(gpa);

    // x should resolve to outer scope again
    const after_exit_lookup = ctx.self.scopeLookup(.ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, after_exit_lookup);
}

test "top level var error" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const var_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern: Pattern.Idx = @enumFromInt(1);

    // Should fail to introduce var at top level
    const result = ctx.self.scopeIntroduceInternal(gpa, .ident, var_ident, pattern, true, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .top_level_var_error = {} }, result);
}

test "type variables are tracked separately from value identifiers" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Create identifiers for 'a' - one for value, one for type
    const a_ident = try ctx.module_env.insertIdent(Ident.for_text("a"));
    const pattern: Pattern.Idx = @enumFromInt(1);
    const type_anno: TypeAnno.Idx = @enumFromInt(1);

    // Introduce 'a' as a value identifier
    const value_result = ctx.self.scopeIntroduceInternal(gpa, .ident, a_ident, pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, value_result);

    // Introduce 'a' as a type variable - should succeed because they're in separate namespaces
    const current_scope = &ctx.self.scopes.items[ctx.self.scopes.items.len - 1];
    const type_result = current_scope.introduceTypeVar(gpa, a_ident, type_anno, null);
    try std.testing.expectEqual(Scope.TypeVarIntroduceResult{ .success = {} }, type_result);

    // Lookup 'a' as value should find the pattern
    const value_lookup = ctx.self.scopeLookup(.ident, a_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern }, value_lookup);

    // Lookup 'a' as type variable should find the type annotation
    const type_lookup = current_scope.lookupTypeVar(a_ident);
    try std.testing.expectEqual(Scope.TypeVarLookupResult{ .found = type_anno }, type_lookup);
}

test "var reassignment within same function" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter function scope
    try ctx.self.scopeEnter(gpa, true);

    const count_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Reassign var (not a declaration)
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, reassign_result);

    // Should resolve to the reassigned value
    const lookup_result = ctx.self.scopeLookup(.ident, count_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, lookup_result);
}

test "var reassignment across function boundary fails" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter first function scope
    try ctx.self.scopeEnter(gpa, true);

    const count_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var in first function
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Enter second function scope (function boundary)
    try ctx.self.scopeEnter(gpa, true);

    // Try to reassign var from different function - should fail
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .var_across_function_boundary = pattern1 }, reassign_result);
}

test "identifiers with and without underscores are different" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const sum_ident = try ctx.module_env.insertIdent(Ident.for_text("sum"));
    const sum_underscore_ident = try ctx.module_env.insertIdent(Ident.for_text("sum_"));
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Enter function scope so we can use var
    try ctx.self.scopeEnter(gpa, true);

    // Introduce regular identifier
    const regular_result = ctx.self.scopeIntroduceInternal(gpa, .ident, sum_ident, pattern1, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, regular_result);

    // Introduce var with underscore - should not conflict
    const var_result = ctx.self.scopeIntroduceInternal(gpa, .ident, sum_underscore_ident, pattern2, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, var_result);

    // Both should be found independently
    const regular_lookup = ctx.self.scopeLookup(.ident, sum_ident);
    const var_lookup = ctx.self.scopeLookup(.ident, sum_underscore_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern1 }, regular_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, var_lookup);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("Foo"));
    const ident_pattern: Pattern.Idx = @enumFromInt(1);
    const alias_pattern: Pattern.Idx = @enumFromInt(2);

    // Add as both ident and alias (they're in separate namespaces)
    const ident_result = ctx.self.scopeIntroduceInternal(gpa, .ident, foo_ident, ident_pattern, false, true);
    const alias_result = ctx.self.scopeIntroduceInternal(gpa, .alias, foo_ident, alias_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, ident_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, alias_result);

    // Both should be found in their respective namespaces
    const ident_lookup = ctx.self.scopeLookup(.ident, foo_ident);
    const alias_lookup = ctx.self.scopeLookup(.alias, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = ident_pattern }, ident_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = alias_pattern }, alias_lookup);
}
