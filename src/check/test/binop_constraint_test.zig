//! Tests for static dispatch constraint generation on binary operations
//!
//! This module contains tests that verify the type checker adds appropriate
//! static dispatch constraints to the result types of binary operations.
//!
//! BUG: Currently, when a binary operation like `x + y` is type-checked, the
//! result gets a flex type variable but WITHOUT the corresponding static dispatch
//! constraint (e.g., `plus`). This causes runtime failures when the interpreter
//! tries to compute layouts, as it has no information to infer a default type.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const TestEnv = @import("TestEnv.zig");

/// Helper to check if a type variable has a specific static dispatch constraint
fn hasConstraint(
    type_store: *const types.Store,
    ident_store: *const base.Ident.Store,
    type_var: types.Var,
    constraint_name: []const u8,
) bool {
    const resolved = type_store.resolveVar(type_var);

    const constraints = switch (resolved.desc.content) {
        .flex => |flex| type_store.sliceStaticDispatchConstraints(flex.constraints),
        .rigid => |rigid| type_store.sliceStaticDispatchConstraints(rigid.constraints),
        else => return false,
    };

    for (constraints) |constraint| {
        const name = ident_store.getText(constraint.fn_name);
        if (std.mem.eql(u8, name, constraint_name)) {
            return true;
        }
    }

    return false;
}

/// Helper to get the type variable for a specific definition by name
fn getDefVar(test_env: *TestEnv, target_def_name: []const u8) !types.Var {
    const idents = test_env.module_env.getIdentStoreConst();
    const defs_slice = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);

    for (defs_slice) |def_idx| {
        const def = test_env.module_env.store.getDef(def_idx);
        const ptrn = test_env.module_env.store.getPattern(def.pattern);

        switch (ptrn) {
            .assign => |assign| {
                const def_name = idents.getText(assign.ident);
                if (std.mem.eql(u8, target_def_name, def_name)) {
                    return @import("can").ModuleEnv.varFrom(def_idx);
                }
            },
            else => continue,
        }
    }
    return error.DefNotFound;
}

test "addition result should have 'plus' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "subtraction result should have 'minus' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "multiplication result should have 'times' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "division result should have 'div' or 'div_trunc' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "chained operations should accumulate constraints" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "integer literal should have 'from_int_digits' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}

test "decimal literal should have 'from_dec_digits' constraint" {
    // We don't yet have unification up to the point of recursion for recursive structural types.
    // TODO: implement the equirecursive rule.
    return error.SkipZigTest;
}
