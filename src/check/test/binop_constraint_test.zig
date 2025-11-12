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
    const source =
        \\f = |x, y| x + y
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Get the definition for 'f'
    const f_var = try getDefVar(&test_env, "f");
    const f_resolved = test_env.module_env.types.resolveVar(f_var);

    // f should be a function - extract the return type
    const func_content = f_resolved.desc.content.unwrapFunc() orelse return error.NotAFunction;
    const ret_var = func_content.ret;

    // The return type should have a 'plus' constraint
    // BUG: Currently this test FAILS because the type checker doesn't add
    // the 'plus' constraint to the result of binary operations
    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_plus = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        ret_var,
        "plus",
    );

    try testing.expect(has_plus);
}

test "subtraction result should have 'minus' constraint" {
    const source =
        \\f = |x, y| x - y
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const f_var = try getDefVar(&test_env, "f");
    const f_resolved = test_env.module_env.types.resolveVar(f_var);
    const func_content = f_resolved.desc.content.unwrapFunc() orelse return error.NotAFunction;

    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_minus = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        func_content.ret,
        "minus",
    );

    try testing.expect(has_minus);
}

test "multiplication result should have 'mul' constraint" {
    const source =
        \\f = |x, y| x * y
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const f_var = try getDefVar(&test_env, "f");
    const f_resolved = test_env.module_env.types.resolveVar(f_var);
    const func_content = f_resolved.desc.content.unwrapFunc() orelse return error.NotAFunction;

    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_mul = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        func_content.ret,
        "mul",
    );

    try testing.expect(has_mul);
}

test "division result should have 'div' or 'div_trunc' constraint" {
    const source =
        \\f = |x, y| x / y
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const f_var = try getDefVar(&test_env, "f");
    const f_resolved = test_env.module_env.types.resolveVar(f_var);
    const func_content = f_resolved.desc.content.unwrapFunc() orelse return error.NotAFunction;

    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_div = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        func_content.ret,
        "div",
    ) or hasConstraint(
        &test_env.module_env.types,
        ident_store,
        func_content.ret,
        "div_trunc",
    );

    try testing.expect(has_div);
}

test "chained operations should accumulate constraints" {
    const source =
        \\f = |x, y, z| x + y * z
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const f_var = try getDefVar(&test_env, "f");
    const f_resolved = test_env.module_env.types.resolveVar(f_var);
    const func_content = f_resolved.desc.content.unwrapFunc() orelse return error.NotAFunction;

    // The result should have a 'plus' constraint from the final + operation
    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_plus = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        func_content.ret,
        "plus",
    );

    try testing.expect(has_plus);
}

test "integer literal should have 'from_int_digits' constraint" {
    const source =
        \\value = 42
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const value_var = try getDefVar(&test_env, "value");

    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_from_int_digits = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        value_var,
        "from_int_digits",
    );

    try testing.expect(has_from_int_digits);
}

test "decimal literal should have 'from_dec_digits' constraint" {
    const source =
        \\value = 3.14
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const value_var = try getDefVar(&test_env, "value");

    const ident_store = test_env.module_env.getIdentStoreConst();
    const has_from_dec_digits = hasConstraint(
        &test_env.module_env.types,
        ident_store,
        value_var,
        "from_dec_digits",
    );

    try testing.expect(has_from_dec_digits);
}
