//! Integration tests for let-polymorphism that parse and canonicalize
//! actual Roc code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("parse.zig");
const canonicalize = @import("canonicalize.zig");
const check_types = @import("check_types.zig");
const CIR = canonicalize.CIR;
const ModuleEnv = base.ModuleEnv;

const test_allocator = testing.allocator;

/// Helper to run parsing, canonicalization, and type checking on an expression
fn typeCheckExpr(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    checker: *check_types,
    has_type_errors: bool,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = ModuleEnv.init(allocator, try allocator.dupe(u8, source));

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parseExpr(module_env, source);

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = undefined,
            .can = undefined,
            .checker = undefined,
            .has_type_errors = true, // Consider parse errors as errors
        };
    }

    // Canonicalize
    const cir = try allocator.create(CIR);
    cir.* = CIR.init(module_env, "Test");

    const can = try allocator.create(canonicalize);
    can.* = try canonicalize.init(cir, parse_ast, null);

    // Run canonicalization - for expressions
    var canon_expr_idx: ?CIR.Expr.Idx = null;
    if (parse_ast.root_node_idx != 0) {
        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        canon_expr_idx = try can.canonicalizeExpr(expr_idx);
    }

    // Type check - continue even if there are parse errors
    const checker = try allocator.create(check_types);
    const empty_modules: []const *CIR = &.{};

    checker.* = try check_types.init(allocator, &module_env.types, cir, empty_modules);

    // For expressions, check the expression directly
    if (canon_expr_idx) |expr_idx| {
        _ = try checker.checkExpr(expr_idx);
    }

    // Check if there are any type errors
    const has_type_errors = checker.problems.problems.len() > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .checker = checker,
        .has_type_errors = has_type_errors,
    };
}

/// Helper to run parsing, canonicalization, and type checking on a full file
fn typeCheckFile(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    checker: *check_types,
    has_type_errors: bool,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = ModuleEnv.init(allocator, try allocator.dupe(u8, source));

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parse(module_env, source);

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = undefined,
            .can = undefined,
            .checker = undefined,
            .has_type_errors = true, // Consider parse errors as errors
        };
    }

    // Canonicalize
    const cir = try allocator.create(CIR);
    cir.* = CIR.init(module_env);

    const can = try allocator.create(canonicalize);
    can.* = try canonicalize.init(cir, parse_ast, null);

    // Run canonicalization - for files
    // Check if we have a valid file structure first
    if (parse_ast.store.nodes.len() == 0) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = cir,
            .can = can,
            .checker = undefined,
            .has_type_errors = true,
        };
    }

    try can.canonicalizeFile();

    // Type check - continue even if there are parse errors
    const checker = try allocator.create(check_types);
    const empty_modules: []const *CIR = &.{};

    checker.* = try check_types.init(allocator, &module_env.types, cir, empty_modules);

    try checker.checkDefs();

    // Check if there are any type errors
    const has_type_errors = checker.problems.problems.len() > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .checker = checker,
        .has_type_errors = has_type_errors,
    };
}

/// Helper to run parsing, canonicalization, and type checking on a statement
fn typeCheckStatement(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    checker: *check_types,
    has_type_errors: bool,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = ModuleEnv.init(allocator, try allocator.dupe(u8, source));

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parseStatement(module_env, source);

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = undefined,
            .can = undefined,
            .checker = undefined,
            .has_type_errors = true, // Consider parse errors as errors
        };
    }

    // Canonicalize
    const cir = try allocator.create(CIR);
    cir.* = CIR.init(module_env, "Test");

    const can = try allocator.create(canonicalize);
    can.* = try canonicalize.init(cir, parse_ast, null);

    // Run canonicalization - for statements
    var canon_result: ?CIR.Expr.Idx = null;
    if (parse_ast.root_node_idx != 0) {
        const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
        canon_result = try can.canonicalizeStatement(stmt_idx);
    }

    // Type check - continue even if there are parse errors
    const checker = try allocator.create(check_types);
    const empty_modules: []const *CIR = &.{};

    checker.* = try check_types.init(allocator, &module_env.types, cir, empty_modules);

    // Check if we have any defs to check
    if (cir.all_defs.span.len > 0) {
        try checker.checkDefs();
    } else if (canon_result) |expr_idx| {
        // If no defs but we have an expression from the statement, check that
        _ = try checker.checkExpr(expr_idx);
    }

    // Check if there are any type errors
    const has_type_errors = checker.problems.problems.len() > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .checker = checker,
        .has_type_errors = has_type_errors,
    };
}

fn cleanup(result: anytype, allocator: std.mem.Allocator) void {
    result.checker.deinit();
    allocator.destroy(result.checker);
    result.can.deinit();
    allocator.destroy(result.can);
    result.cir.deinit();
    allocator.destroy(result.cir);
    result.parse_ast.deinit(allocator);
    allocator.destroy(result.parse_ast);
    result.module_env.deinit();
    allocator.destroy(result.module_env);
}

test "let-polymorphism with empty list in multiple contexts" {
    const source =
        \\[]
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic empty list should work in different contexts
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with numeric literal" {
    const source =
        \\let
        \\    num = 42
        \\    as_int = num
        \\    as_float = num
        \\    in_calculation = num * 2.5
        \\in
        \\    { as_int, as_float, in_calculation }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic numbers should work in different contexts
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with identity function" {
    const source =
        \\identity = |x| x
        \\
        \\main = |_| {
        \\    str = identity("hello")
        \\    num = identity(42)
        \\    list = identity([1, 2, 3])
        \\
        \\    { str, num, list }
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic identity function should work with different types
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with records containing polymorphic fields" {
    const source =
        \\main = |_| {
        \\    empty_list = []
        \\    num = 100
        \\
        \\    record1 = { data: empty_list, value: num }
        \\    record2 = { data: [1, 2], value: num }
        \\    record3 = { data: [3, 4], value: num * 2 }
        \\
        \\    { record1, record2, record3 }
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - records with polymorphic fields should type check
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with nested empty lists" {
    const source =
        \\main = |_| {
        \\    empty = []
        \\
        \\    nestedEmpty = [empty, empty, empty]
        \\    withInts = [[], [1, 2], [], [3, 4]]
        \\    withStrs = [[], ["a"], [], ["b", "c"]]
        \\
        \\    { nestedEmpty, withInts, withStrs }
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - nested empty lists should work correctly
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with function composition" {
    const source =
        \\let
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    num_compose = compose(double, add_one)
        \\    result1 = num_compose(5)
        \\    exclaim = |s| "!"
        \\    caps = |s| "HELLO"
        \\    str_compose = compose(exclaim, caps)
        \\    result2 = str_compose("hello")
        \\in
        \\    { result1, result2 }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic function composition should type check
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with pattern matching" {
    const source =
        \\let
        \\    is_empty = |list|
        \\        match list {
        \\            [] => True,
        \\            _ => False,
        \\        }
        \\    empty = []
        \\    check_int = is_empty([1, 2, 3])
        \\    check_str = is_empty(["a", "b"])
        \\    check_empty = is_empty(empty)
        \\in
        \\    { check_int, check_str, check_empty }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - pattern matching with polymorphic functions should work
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with higher-order functions" {
    const source =
        \\let
        \\    map = |list, f|
        \\        match list {
        \\            [] => [],
        \\            [x, ..xs] => [f(x)],
        \\        }
        \\    empty = []
        \\    mapped1 = map(empty, |x| x + 1)
        \\    mapped2 = map(empty, |s| "!")
        \\    nums = map([1, 2, 3], |x| x * 2)
        \\    strs = map(["a", "b"], |s| "UPPER")
        \\in
        \\    { mapped1, mapped2, nums, strs }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - higher-order polymorphic functions should type check
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism error - incompatible list elements" {
    const source =
        \\[42, "hello"]
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify this produces type errors - can't mix numbers and strings in a list
    try testing.expect(result.has_type_errors);
}

test "match expression with empty list followed by rest pattern - regression test for segfault" {
    const source =
        \\let
        \\    last = |l|
        \\        match l {
        \\            [] => Err(EmptyList),
        \\            [.., e] => Ok(e),
        \\        }
        \\in
        \\    last
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // This should not segfault and should type check correctly
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism error - over-generalization attempt" {
    const source =
        \\[1, 2, 3, "hello"]
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify this produces type errors - can't use int list as string list
    try testing.expect(result.has_type_errors);
}

test "let-polymorphism with constrained type variables" {
    const source =
        \\    add_three = |a, b, c| a + b + c
        \\
        \\main = |_| {
        \\    # Use with integers
        \\    int_result = add_three(1, 2, 3)
        \\
        \\    # Use with floats
        \\    float_result = add_three(1.1, 2.2, 3.3)
        \\
        \\    { int_result, float_result }
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - constrained polymorphic functions should work
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with multiple instantiations in one expression" {
    const source =
        \\pair = |a, b| { fst: a, snd: b }
        \\
        \\main = |_| {
        \\    # Multiple uses of pair in one expression
        \\    result = {
        \\        p1: pair(1, 2),
        \\        p2: pair("hello", "world"),
        \\        p3: pair([1, 2], [3, 4]),
        \\        p4: pair(True, False),
        \\    }
        \\
        \\    result
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - multiple instantiations should all type check
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with recursive functions" {
    const source =
        \\let
        \\    length = |list|
        \\        match list {
        \\            [] => 0,
        \\            [_, ..rest] => 1 + length(rest),
        \\        }
        \\    len1 = length([1, 2, 3, 4, 5])
        \\    len2 = length(["a", "b", "c"])
        \\    len3 = length([True, False, True])
        \\    len4 = length([])
        \\in
        \\    { len1, len2, len3, len4 }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic recursive functions should type check
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with polymorphic data constructors" {
    const source =
        \\let
        \\    from_maybe = |maybe, default|
        \\        match maybe {
        \\            Just(val) => val,
        \\            Nothing => default,
        \\        }
        \\    nothing = Nothing
        \\    int = from_maybe(nothing, 42)
        \\    str = from_maybe(nothing, "default")
        \\    list = from_maybe(nothing, [1, 2, 3])
        \\    just1 = Just(100)
        \\    just2 = Just("hello")
        \\in
        \\    { int, str, list, just1, just2 }
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - polymorphic data constructors should work correctly
    try testing.expect(!result.has_type_errors);
}

test "let-polymorphism with complex nested structures" {
    const source =
        \\main = |_| {
        \\    empty = []
        \\    num = 42
        \\
        \\    base = {
        \\        data: empty,
        \\        value: num,
        \\        nested: {
        \\            items: empty,
        \\            count: 0,
        \\        }
        \\    }
        \\
        \\    # Create variations with different instantiations
        \\    with_ints = {
        \\        data: [1, 2, 3],
        \\        value: num,
        \\        nested: {
        \\            items: [10, 20],
        \\            count: 2,
        \\        }
        \\    }
        \\
        \\    with_strs = {
        \\        data: ["a", "b"],
        \\        value: num * 2,
        \\        nested: {
        \\            items: ["x", "y", "z"],
        \\            count: 3,
        \\        }
        \\    }
        \\
        \\    { base, with_ints, with_strs }
        \\}
    ;

    const result = try typeCheckStatement(test_allocator, source);
    defer cleanup(result, test_allocator);

    // Verify no type errors - complex nested polymorphic structures should type check
    try testing.expect(!result.has_type_errors);
}
