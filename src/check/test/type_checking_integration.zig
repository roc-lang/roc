//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const types_mod = @import("types");
const problem_mod = @import("../problem.zig");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

// primitives - nums //

test "check type - num - unbound" {
    const source =
        \\50
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(_size)");
}

test "check type - num - int suffix 1" {
    const source =
        \\10u8
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Int(Unsigned8))");
}

test "check type - num - int suffix 2" {
    const source =
        \\10i128
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Int(Signed128))");
}

test "check type - num - float" {
    const source =
        \\10.1
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Frac(_size))");
}

test "check type - num - float suffix 1" {
    const source =
        \\10.1f32
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Frac(Float32))");
}

test "check type - num - float suffix 2" {
    const source =
        \\10.1f64
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Frac(Float64))");
}

test "check type - num - float suffix 3" {
    const source =
        \\10.1dec
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Num(Frac(Decimal))");
}

// primitives - strs //

test "check type - str" {
    const source =
        \\"hello"
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Str");
}

// primitives - lists //

test "check type - list empty" {
    const source =
        \\[]
    ;
    try assertExprTypeCheckPass(test_allocator, source, "List(_elem)");
}

test "check type - list - same elems 1" {
    const source =
        \\["hello", "world"]
    ;
    try assertExprTypeCheckPass(test_allocator, source, "List(Str)");
}

test "check type - list - same elems 2" {
    const source =
        \\[100, 200]
    ;
    try assertExprTypeCheckPass(test_allocator, source, "List(Num(_size))");
}

test "check type - list - 1st elem more specific coreces 2nd elem" {
    const source =
        \\[100u64, 200]
    ;
    try assertExprTypeCheckPass(test_allocator, source, "List(Num(Int(Unsigned64)))");
}

test "check type - list - 2nd elem more specific coreces 1st elem" {
    const source =
        \\[100, 200u32]
    ;
    try assertExprTypeCheckPass(test_allocator, source, "List(Num(Int(Unsigned32)))");
}

test "check type - list  - diff elems 1" {
    const source =
        \\["hello", 10]
    ;
    try assertExprTypeCheckFail(test_allocator, source, "INCOMPATIBLE LIST ELEMENTS");
}

// number requirements //

test "check type - num - cannot coerce 500 to u8" {
    const source =
        \\[500, 200u8]
    ;
    try assertExprTypeCheckFail(test_allocator, source, "NUMBER DOES NOT FIT IN TYPE");
}

// records //

test "check type - record" {
    const source =
        \\{
        \\  hello: "Hello",
        \\  world: 10,
        \\}
    ;
    try assertExprTypeCheckPass(test_allocator, source, "{ hello: Str, world: Num(_size) }");
}

// tags //

test "check type - tag" {
    const source =
        \\MyTag
    ;
    try assertExprTypeCheckPass(test_allocator, source, "[MyTag]_others");
}

test "check type - tag - args" {
    const source =
        \\MyTag("hello", 1)
    ;
    try assertExprTypeCheckPass(test_allocator, source, "[MyTag(Str, Num(_size))]_others");
}

// blocks //

test "check type - block - return expr" {
    const source =
        \\{
        \\    "Hello"
        \\}
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Str");
}

test "check type - block - implicit empty record" {
    const source =
        \\{
        \\    test = "hello"
        \\}
    ;
    try assertExprTypeCheckPass(test_allocator, source, "{}");
}

test "check type - block - local value decl" {
    const source =
        \\{
        \\    test = "hello"
        \\
        \\    test
        \\}
    ;
    try assertExprTypeCheckPass(test_allocator, source, "Str");
}

// function //

test "check type - def - value" {
    const source =
        \\module []
        \\
        \\pairU64 = "hello"
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Str");
}

test "check type - def - func" {
    const source =
        \\module []
        \\
        \\id = |_| 20
    ;
    try assertFileTypeCheckPass(test_allocator, source, "_arg -> Num(_size)");
}

test "check type - def - id without annotation" {
    const source =
        \\module []
        \\
        \\id = |x| x
    ;
    try assertFileTypeCheckPass(test_allocator, source, "a -> a");
}

test "check type - def - id with annotation" {
    const source =
        \\module []
        \\
        \\id : a -> a
        \\id = |x| x
    ;
    try assertFileTypeCheckPass(test_allocator, source, "a -> a");
}

test "check type - def - func with annotation 1" {
    const source =
        \\module []
        \\
        \\id : x -> Str
        \\id = |_| "test"
    ;
    try assertFileTypeCheckPass(test_allocator, source, "x -> Str");
}

test "check type - def - func with annotation 2" {
    const source =
        \\module []
        \\
        \\id : x -> Num(_size)
        \\id = |_| 15
    ;
    try assertFileTypeCheckPass(test_allocator, source, "x -> Num(_size)");
}

// calling functions

test "check type - def - monomorphic id" {
    const source =
        \\module []
        \\
        \\idStr : Str -> Str
        \\idStr = |x| x
        \\
        \\test = idStr("hello")
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Str");
}

test "check type - def - polymorphic id 1" {
    const source =
        \\module []
        \\
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = id(5)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(_size)");
}

test "check type - def - polymorphic id 2" {
    const source =
        \\module []
        \\
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = (id(5), id("hello"))
    ;
    try assertFileTypeCheckPass(test_allocator, source, "(Num(_size), Str)");
}

test "check type - def - polymorphic higher order 1" {
    const source =
        \\module []
        \\
        \\f = |g, v| g(v)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "a -> b, a -> b");
}

test "check type - top level polymorphic function is generalized" {
    const source =
        \\module []
        \\
        \\id = |x| x
        \\
        \\result = {
        \\    a = id(42)
        \\    b = id("hello")
        \\    a
        \\}
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(_size)");
}

test "check type - let-def polymorphic function is generalized" {
    const source =
        \\module []
        \\
        \\result = {
        \\    id = |x| x
        \\    a = id(42)
        \\    b = id("hello")
        \\    a
        \\}
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(_size)");
}

test "check type - polymorphic function function param should be constrained" {
    const source =
        \\module []
        \\
        \\id = |x| x
        \\
        \\use_twice = |f| {
        \\    a = f(42)
        \\    b = f("hello")
        \\    a
        \\}
        \\result = use_twice(id)
    ;
    try assertFileTypeCheckFail(test_allocator, source, "TYPE MISMATCH");
}

// type aliases //

test "check type - basic alias" {
    const source =
        \\module []
        \\
        \\MyAlias : Str
        \\
        \\x : MyAlias
        \\x = "hello"
    ;
    try assertFileTypeCheckPass(test_allocator, source, "MyAlias");
}

test "check type - alias with arg" {
    const source =
        \\module []
        \\
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Num(size))
        \\x = [15]
    ;
    try assertFileTypeCheckPass(test_allocator, source, "MyListAlias(Num(size))");
}

test "check type - alias with mismatch arg" {
    const source =
        \\module []
        \\
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Str)
        \\x = [15]
    ;
    try assertFileTypeCheckFail(test_allocator, source, "TYPE MISMATCH");
}

// nominal types //

test "check type - basic nominal" {
    const source =
        \\module []
        \\
        \\MyNominal := [MyNominal]
        \\
        \\x : MyNominal
        \\x = MyNominal.MyNominal
    ;
    try assertFileTypeCheckPass(test_allocator, source, "MyNominal");
}

test "check type - nominal with tag arg" {
    const source =
        \\module []
        \\
        \\MyNominal := [MyNominal(Str)]
        \\
        \\x : MyNominal
        \\x = MyNominal.MyNominal("hello")
    ;
    try assertFileTypeCheckPass(test_allocator, source, "MyNominal");
}

test "check type - nominal with type and tag arg" {
    const source =
        \\module []
        \\
        \\MyNominal(a) := [MyNominal(a)]
        \\
        \\x : MyNominal(U8)
        \\x = MyNominal.MyNominal(10)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "MyNominal(Num(Int(Unsigned8)))");
}

test "check type - nominal with with rigid vars" {
    const source =
        \\module []
        \\
        \\Pair(a) := [Pair(a, a)]
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(1, 2)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Pair(Num(Int(Unsigned64)))");
}

test "check type - nominal with with rigid vars mismatch" {
    const source =
        \\module []
        \\
        \\Pair(a) := [Pair(a, a)]
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(1, "Str")
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID NOMINAL TAG");
}

test "check type - nominal recursive type" {
    const source =
        \\module []
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x : ConsList(Str)
        \\x = ConsList.Cons("hello", ConsList.Nil)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "ConsList(Str)");
}

test "check type - nominal recursive type anno mismatch" {
    const source =
        \\module []
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x : ConsList(Num(size))
        \\x = ConsList.Cons("hello", ConsList.Nil)
    ;
    try assertFileTypeCheckFail(test_allocator, source, "TYPE MISMATCH");
}

test "check type - two nominal types" {
    const source =
        \\module []
        \\
        \\Elem(a) := [Elem(a)]
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x = ConsList.Cons(Elem.Elem("hello"), ConsList.Nil)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "ConsList(Elem(Str))");
}

test "check type - nominal recursive type no args" {
    const source =
        \\module []
        \\
        \\StrConsList := [Nil, Cons(Str, StrConsList)]
        \\
        \\x : StrConsList
        \\x = StrConsList.Cons("hello", StrConsList.Nil)
    ;
    try assertFileTypeCheckPass(test_allocator, source, "StrConsList");
}

test "check type - nominal recursive type wrong type" {
    const source =
        \\module []
        \\
        \\StrConsList := [Nil, Cons(Str, StrConsList)]
        \\
        \\x : StrConsList
        \\x = StrConsList.Cons(10, StrConsList.Nil)
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID NOMINAL TAG");
}

test "check type - nominal w/ polymorphic function" {
    const source =
        \\module []
        \\
        \\Pair(a) := [Pair(a, a)]
        \\
        \\mkPairInvalid : a, b -> Pair(a)
        \\mkPairInvalid = |x, y| Pair.Pair(x, y)
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID NOMINAL TAG");
}

// if-else

test "check type - if else" {
    const source =
        \\module []
        \\
        \\x : Str
        \\x = if True "true" else "false"
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Str");
}

test "check type - if else - qualified bool" {
    const source =
        \\module []
        \\
        \\x : Str
        \\x = if Bool.True "true" else "false"
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Str");
}

test "check type - if else - invalid condition 1" {
    const source =
        \\module []
        \\
        \\x : Str
        \\x = if Truee "true" else "false"
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID IF CONDITION");
}

test "check type - if else - invalid condition 2" {
    const source =
        \\module []
        \\
        \\x : Str
        \\x = if Bool.Falsee "true" else "false"
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID NOMINAL TAG");
}

test "check type - if else - invalid condition 3" {
    const source =
        \\module []
        \\
        \\x : Str
        \\x = if "True" "true" else "false"
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID IF CONDITION");
}

test "check type - if else - different branch types 1" {
    const source =
        \\module []
        \\
        \\x = if True "true" else 10
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 2" {
    const source =
        \\module []
        \\
        \\x = if True "true" else if False "false" else 10
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 3" {
    const source =
        \\module []
        \\
        \\x = if True "true" else if False 10 else "last"
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INCOMPATIBLE IF BRANCHES");
}

// match

test "check type - match" {
    const source =
        \\module []
        \\
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => "false"
        \\  }
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Str");
}

test "check type - match - diff cond types 1" {
    const source =
        \\module []
        \\
        \\x =
        \\  match "hello" {
        \\    True => "true"
        \\    False => "false"
        \\  }
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INCOMPATIBLE MATCH PATTERNS");
}

test "check type - match - diff branch types" {
    const source =
        \\module []
        \\
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => 100
        \\  }
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INCOMPATIBLE MATCH BRANCHES");
}

// unary not

test "check type - unary not" {
    const source =
        \\module []
        \\
        \\x = !True
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Bool");
}

test "check type - unary not mismatch" {
    const source =
        \\module []
        \\
        \\x = !"Hello"
    ;
    try assertFileTypeCheckFail(test_allocator, source, "TYPE MISMATCH");
}

// unary not

test "check type - unary minus" {
    const source =
        \\module []
        \\
        \\x = -10
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(_size)");
}

test "check type - unary minus mismatch" {
    const source =
        \\module []
        \\
        \\x = "hello"
        \\
        \\y = -x
    ;
    try assertFileTypeCheckFail(test_allocator, source, "TYPE MISMATCH");
}

// binops

test "check type - binops math" {
    const source =
        \\module []
        \\
        \\x = 10 + 10u32
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(Int(Unsigned32))");
}

test "check type - binops ord" {
    const source =
        \\module []
        \\
        \\x = 10.0f32 > 15
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Num(Frac(Float32))");
}

test "check type - binops and" {
    const source =
        \\module []
        \\
        \\x = True and False
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Bool");
}

test "check type - binops and mismatch" {
    const source =
        \\module []
        \\
        \\x = "Hello" and False
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID BOOL OPERATION");
}

test "check type - binops or" {
    const source =
        \\module []
        \\
        \\x = True or False
    ;
    try assertFileTypeCheckPass(test_allocator, source, "Bool");
}

test "check type - binops or mismatch" {
    const source =
        \\module []
        \\
        \\x = "Hello" or False
    ;
    try assertFileTypeCheckFail(test_allocator, source, "INVALID BOOL OPERATION");
}

// helpers - expr //

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that type checking the expr passes
fn assertExprTypeCheckPass(allocator: std.mem.Allocator, comptime source_expr: []const u8, expected_type: []const u8) !void {
    const source_wrapper =
        \\module []
        \\ 
        \\test =
    ;

    var source: [source_wrapper.len + source_expr.len]u8 = undefined;
    std.mem.copyForwards(u8, source[0..], source_wrapper);
    std.mem.copyForwards(u8, source[source_wrapper.len..], source_expr);

    return assertFileTypeCheckPass(allocator, &source, expected_type);
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that type checking the expr fails with exactly one problem, and the title of the problem matches the provided one.
fn assertExprTypeCheckFail(allocator: std.mem.Allocator, comptime source_expr: []const u8, expected_problem_title: []const u8) !void {
    const source_wrapper =
        \\module []
        \\ 
        \\test =
    ;

    var source: [source_wrapper.len + source_expr.len]u8 = undefined;
    std.mem.copyForwards(u8, source[0..], source_wrapper);
    std.mem.copyForwards(u8, source[source_wrapper.len..], source_expr);

    return assertFileTypeCheckFail(allocator, &source, expected_problem_title);
}

// helpers - whole file //

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that the type of the final definition in the source matches the one provided
fn assertFileTypeCheckPass(allocator: std.mem.Allocator, source: []const u8, expected_type: []const u8) !void {
    // Set up module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    try module_env.initCIRFields(allocator, "test");
    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Parse
    var parse_ast = try parse.parse(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);
    try testing.expectEqual(false, parse_ast.hasErrors());

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    try testing.expect(czer.env.all_defs.span.len > 0);
    const defs_slice = czer.env.store.sliceDefs(czer.env.all_defs);
    const last_def_idx = defs_slice[defs_slice.len - 1];

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions, module_common_idents);
    defer checker.deinit();
    try checker.checkFile();

    // Assert no problems
    var report_buf = try std.ArrayList(u8).initCapacity(allocator, 256);
    defer report_buf.deinit();

    var report_builder = problem_mod.ReportBuilder.init(
        allocator,
        &module_env,
        &module_env,
        &checker.snapshots,
        "test",
        &.{},
    );
    defer report_builder.deinit();

    for (checker.problems.problems.items) |problem| {
        var report = try report_builder.build(problem);
        defer report.deinit();

        report_buf.clearRetainingCapacity();
        try report.render(report_buf.writer(), .markdown);

        try testing.expectEqualStrings("EXPECT NO ERROR", report_buf.items);
    }
    try testing.expectEqual(0, checker.problems.problems.items.len);

    // Assert the rendered type string matches what we expect
    var type_writer = try module_env.initTypeWriter();
    defer type_writer.deinit();
    try type_writer.write(ModuleEnv.varFrom(last_def_idx));
    try testing.expectEqualStrings(expected_type, type_writer.get());
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that the type of the final definition in the source matches the one provided
fn assertFileTypeCheckFail(allocator: std.mem.Allocator, source: []const u8, expected_problem_title: []const u8) !void {
    // Set up module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    try module_env.initCIRFields(allocator, "test");
    const module_common_idents: Check.CommonIdents = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("test")),
        .list = try module_env.insertIdent(base.Ident.for_text("List")),
        .box = try module_env.insertIdent(base.Ident.for_text("Box")),
    };

    // Parse
    var parse_ast = try parse.parse(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);
    try testing.expectEqual(false, parse_ast.hasErrors());

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    try testing.expect(czer.env.all_defs.span.len > 0);
    // const defs_slice = czer.env.store.sliceDefs(czer.env.all_defs);
    // const last_def_idx = defs_slice[defs_slice.len - 1];

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions, module_common_idents);
    defer checker.deinit();
    try checker.checkFile();

    // Assert 1 problem
    try testing.expectEqual(1, checker.problems.problems.items.len);
    const problem = checker.problems.problems.items[0];

    // Assert the rendered problem matches the expected problem
    var report_builder = problem_mod.ReportBuilder.init(
        allocator,
        &module_env,
        &module_env,
        &checker.snapshots,
        "test",
        &.{},
    );
    defer report_builder.deinit();

    var report = try report_builder.build(problem);
    defer report.deinit();

    try testing.expectEqualStrings(expected_problem_title, report.title);
}
