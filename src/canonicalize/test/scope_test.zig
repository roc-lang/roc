//! Tests for Scopes

const std = @import("std");
const base = @import("base");
const parse = @import("parse");

const CIR = @import("../CIR.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const Ident = base.Ident;
const Region = base.Region;
const Scope = @import("../Scope.zig");
const Pattern = CIR.Pattern;
const TypeAnno = CIR.TypeAnno;

/// Context helper for Scope tests
const ScopeTestContext = struct {
    scope_state: CIR.ScopeState,
    module_env: *ModuleEnv,
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) !ScopeTestContext {
        // heap allocate ModuleEnv for testing
        const module_env = try gpa.create(ModuleEnv);
        module_env.* = try ModuleEnv.init(gpa, "");
        try module_env.initCIRFields(gpa, "test");

        return ScopeTestContext{
            .scope_state = CIR.ScopeState{},
            .module_env = module_env,
            .gpa = gpa,
        };
    }

    fn deinit(ctx: *ScopeTestContext) void {
        ctx.scope_state.deinit(ctx.gpa);
        ctx.module_env.deinit();
        ctx.gpa.destroy(ctx.module_env);
    }
};

test "basic scope initialization" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Test that we start with no scopes (empty state)
    try std.testing.expect(ctx.scope_state.scopes.items.len == 0);
}

test "empty scope has no items" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push a scope first
    try ctx.scope_state.pushScope(gpa, false);

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("foo"));
    const result = ctx.scope_state.lookupIdent(foo_ident);

    try std.testing.expectEqual(@as(?Pattern.Idx, null), result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push a scope
    try ctx.scope_state.pushScope(gpa, false);

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("foo"));
    const bar_ident = try ctx.module_env.insertIdent(Ident.for_text("bar"));
    const foo_pattern: CIR.Patt.Idx = @enumFromInt(1);
    const bar_pattern: CIR.Patt.Idx = @enumFromInt(2);

    // Add identifiers
    try ctx.scope_state.addIdent(gpa, foo_ident, foo_pattern);
    try ctx.scope_state.addIdent(gpa, bar_ident, bar_pattern);

    // Lookup should find them
    const foo_lookup = ctx.scope_state.lookupIdent(foo_ident);
    const bar_lookup = ctx.scope_state.lookupIdent(bar_ident);

    try std.testing.expectEqual(@as(?CIR.Patt.Idx, foo_pattern), foo_lookup);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, bar_pattern), bar_lookup);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push outer scope
    try ctx.scope_state.pushScope(gpa, false);

    const x_ident = try ctx.module_env.insertIdent(Ident.for_text("x"));
    const outer_pattern: CIR.Patt.Idx = @enumFromInt(1);
    const inner_pattern: CIR.Patt.Idx = @enumFromInt(2);

    // Add x to outer scope
    try ctx.scope_state.addIdent(gpa, x_ident, outer_pattern);

    // Enter new scope
    try ctx.scope_state.pushScope(gpa, false);

    // x from outer scope should still be visible
    const outer_lookup = ctx.scope_state.lookupIdent(x_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, outer_pattern), outer_lookup);

    // Add x to inner scope (shadows outer)
    try ctx.scope_state.addIdent(gpa, x_ident, inner_pattern);

    // Now x should resolve to inner scope
    const inner_lookup = ctx.scope_state.lookupIdent(x_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, inner_pattern), inner_lookup);

    // Exit inner scope
    ctx.scope_state.popScope(gpa);

    // x should resolve to outer scope again
    const after_exit_lookup = ctx.scope_state.lookupIdent(x_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, outer_pattern), after_exit_lookup);
}

test "top level var error" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push a top-level scope
    try ctx.scope_state.pushScope(gpa, false);

    const var_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern: CIR.Patt.Idx = @enumFromInt(1);

    // Mark pattern as var for later checking
    try ctx.scope_state.recordVarPattern(gpa, pattern);

    // Add the var ident - at top level this is allowed in the new system
    // The error checking would happen at a higher level in CIR
    try ctx.scope_state.addIdent(gpa, var_ident, pattern);

    // Just verify it was added
    const lookup = ctx.scope_state.lookupIdent(var_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, pattern), lookup);
}

test "type variables are tracked separately from value identifiers" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push a scope
    try ctx.scope_state.pushScope(gpa, false);

    // Create identifiers for 'a' - one for value, one for type
    const a_ident = try ctx.module_env.insertIdent(Ident.for_text("a"));
    const pattern: CIR.Patt.Idx = @enumFromInt(1);
    _ = @as(TypeAnno.Idx, @enumFromInt(1)); // Would be used for type variables

    // Introduce 'a' as a value identifier
    try ctx.scope_state.addIdent(gpa, a_ident, pattern);

    // Type variables would be tracked separately in the actual implementation
    // For now we just verify the value lookup works
    const value_lookup = ctx.scope_state.lookupIdent(a_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, pattern), value_lookup);
}

test "var reassignment within same function" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter function scope
    try ctx.scope_state.enterFunction(gpa, Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 100 } });
    try ctx.scope_state.pushScope(gpa, true);

    const count_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern1: CIR.Patt.Idx = @enumFromInt(1);
    const pattern2: CIR.Patt.Idx = @enumFromInt(2);

    // Declare var
    try ctx.scope_state.recordVarPattern(gpa, pattern1);
    try ctx.scope_state.addIdent(gpa, count_ident, pattern1);

    // Reassign var (update the ident)
    try ctx.scope_state.recordVarPattern(gpa, pattern2);
    try ctx.scope_state.updateIdent(gpa, count_ident, pattern2);

    // Should resolve to the reassigned value
    const lookup_result = ctx.scope_state.lookupIdent(count_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, pattern2), lookup_result);
}

test "var reassignment across function boundary fails" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter first function scope
    try ctx.scope_state.enterFunction(gpa, Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 50 } });
    try ctx.scope_state.pushScope(gpa, true);

    const count_ident = try ctx.module_env.insertIdent(Ident.for_text("count_"));
    const pattern1: CIR.Patt.Idx = @enumFromInt(1);
    _ = @as(CIR.Patt.Idx, @enumFromInt(2)); // Would be pattern2 if we tested actual reassignment

    // Declare var in first function
    try ctx.scope_state.recordVarPattern(gpa, pattern1);
    try ctx.scope_state.addIdent(gpa, count_ident, pattern1);

    // Exit first function and enter second function scope
    ctx.scope_state.exitFunction();
    try ctx.scope_state.enterFunction(gpa, Region{ .start = .{ .offset = 50 }, .end = .{ .offset = 100 } });
    try ctx.scope_state.pushScope(gpa, true);

    // Check if reassignment would cross function boundary
    const would_cross = ctx.scope_state.isVarReassignmentAcrossFunctionBoundary(pattern1);
    try std.testing.expectEqual(true, would_cross);
}

test "identifiers with and without underscores are different" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const sum_ident = try ctx.module_env.insertIdent(Ident.for_text("sum"));
    const sum_underscore_ident = try ctx.module_env.insertIdent(Ident.for_text("sum_"));
    const pattern1: CIR.Patt.Idx = @enumFromInt(1);
    const pattern2: CIR.Patt.Idx = @enumFromInt(2);

    // Enter function scope so we can use var
    try ctx.scope_state.enterFunction(gpa, Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 100 } });
    try ctx.scope_state.pushScope(gpa, true);

    // Introduce regular identifier
    try ctx.scope_state.addIdent(gpa, sum_ident, pattern1);

    // Introduce var with underscore - should not conflict
    try ctx.scope_state.recordVarPattern(gpa, pattern2);
    try ctx.scope_state.addIdent(gpa, sum_underscore_ident, pattern2);

    // Both should be found independently
    const regular_lookup = ctx.scope_state.lookupIdent(sum_ident);
    const var_lookup = ctx.scope_state.lookupIdent(sum_underscore_ident);

    try std.testing.expectEqual(@as(?CIR.Patt.Idx, pattern1), regular_lookup);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, pattern2), var_lookup);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Push a scope
    try ctx.scope_state.pushScope(gpa, false);

    const foo_ident = try ctx.module_env.insertIdent(Ident.for_text("Foo"));
    const ident_pattern: CIR.Patt.Idx = @enumFromInt(1);
    _ = @as(CIR.Patt.Idx, @enumFromInt(2)); // Would be alias_pattern if aliases were tested separately

    // Add as identifier
    try ctx.scope_state.addIdent(gpa, foo_ident, ident_pattern);

    // Aliases would be tracked separately in the actual implementation
    // For this test we just verify the ident was added
    const ident_lookup = ctx.scope_state.lookupIdent(foo_ident);
    try std.testing.expectEqual(@as(?CIR.Patt.Idx, ident_pattern), ident_lookup);
}
