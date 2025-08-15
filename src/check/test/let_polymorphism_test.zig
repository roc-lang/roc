//! Tests for let-polymorphism in the Zig compiler
//! These tests verify that polymorphic values can be used in multiple contexts
//! with different type instantiations.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const TypesStore = types.Store;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;
const Instantiate = types.instantiate.Instantiate;

// test env //

const TestEnv = struct {
    module_env: *ModuleEnv,
    store: *TypesStore,
    regions: *base.Region.List,
    var_subs: *Instantiate.SeenVars,
    rigid_var_subs: *Instantiate.RigidToFlexSubs,
};

fn setupTestEnvironment(allocator: std.mem.Allocator) !TestEnv {
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "");

    const store = try allocator.create(TypesStore);
    store.* = try TypesStore.init(allocator);

    const regions = try allocator.create(base.Region.List);
    regions.* = try base.Region.List.initCapacity(allocator, 256);

    const var_subs = try allocator.create(Instantiate.SeenVars);
    var_subs.* = Instantiate.SeenVars.init(allocator);

    const rigid_var_subs = try allocator.create(Instantiate.RigidToFlexSubs);
    rigid_var_subs.* = try Instantiate.RigidToFlexSubs.init(allocator);

    return .{
        .module_env = module_env,
        .store = store,
        .regions = regions,
        .var_subs = var_subs,
        .rigid_var_subs = rigid_var_subs,
    };
}

fn cleanup(env: TestEnv, allocator: std.mem.Allocator) void {
    env.regions.deinit(allocator);
    allocator.destroy(env.regions);

    env.store.deinit();
    allocator.destroy(env.store);

    env.module_env.deinit();
    allocator.destroy(env.module_env);

    env.var_subs.deinit();
    allocator.destroy(env.var_subs);

    env.rigid_var_subs.deinit(allocator);
    allocator.destroy(env.rigid_var_subs);
}

fn instantiateVar(env: TestEnv, var_: types.Var, rigid_subs: []const Instantiate.IdentVar) std.mem.Allocator.Error!types.Var {
    env.var_subs.clearRetainingCapacity();
    env.rigid_var_subs.items.clearRetainingCapacity();

    for (rigid_subs) |sub| {
        try env.rigid_var_subs.append(test_allocator, sub);
    }

    var inst = Instantiate.init(env.store, env.module_env.getIdentStore(), env.var_subs);
    var instantiate_ctx = Instantiate.Ctx{
        .rigid_var_subs = env.rigid_var_subs,
    };
    return inst.instantiateVar(var_, &instantiate_ctx);
}

// tests //

test "let-polymorphism with empty list" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic empty list type: forall a. List a
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const list_elem_var = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });
    const poly_list_var = try env.store.freshFromContent(types.Content{ .structure = .{ .list = list_elem_var } });

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(poly_list_var));

    // First usage: instantiate for integers
    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const int_list_var = try instantiateVar(env, poly_list_var, &.{
        .{ .ident = "a", .var_ = int_var },
    });

    // Second usage: instantiate for strings
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const str_list_var = try instantiateVar(env, poly_list_var, &.{
        .{ .ident = "a", .var_ = str_var },
    });

    // Verify the two instantiations are different
    try testing.expect(int_list_var != str_list_var);
    try testing.expect(int_list_var != poly_list_var);
    try testing.expect(str_list_var != poly_list_var);
}

test "let-polymorphism with numeric literal" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic number: forall a. Num a => a
    // First create a flex var for the type parameter
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(.{ .rigid_var = a_ident });

    // Then create a num_poly that references it
    const num_content = types.Content{ .structure = .{ .num = .{ .num_poly = .{
        .var_ = type_param,
        .requirements = .{ .sign_needed = false, .bits_needed = 0 },
    } } } };
    const num_var = try env.store.freshFromContent(num_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(num_var));

    // First usage: instantiate as I32
    const i32_prec = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const i32_instance = try instantiateVar(env, num_var, &.{
        .{ .ident = "a", .var_ = i32_prec },
    });

    // Second usage: instantiate as F64
    const f64_prec = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .frac_precision = .f64 } } });
    const f64_instance = try instantiateVar(env, num_var, &.{
        .{ .ident = "a", .var_ = f64_prec },
    });

    // Verify the two instantiations are different
    try testing.expect(i32_instance != f64_instance);
    try testing.expect(i32_instance != num_var);
    try testing.expect(f64_instance != num_var);
}

test "let-polymorphism with polymorphic function" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create polymorphic identity function: forall a. a -> a
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    const func_content = try env.store.mkFuncPure(&.{type_param}, type_param);
    const func_var = try env.store.freshFromContent(func_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(func_var));

    // First usage: instantiate for use with strings
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const str_func = try instantiateVar(env, func_var, &.{
        .{ .ident = "a", .var_ = str_var },
    });

    // Second usage: instantiate for use with numbers
    const num_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const num_func = try instantiateVar(env, func_var, &.{
        .{ .ident = "a", .var_ = num_var },
    });

    // Verify we got different instantiations
    try testing.expect(str_func != num_func);
    try testing.expect(str_func != func_var);
    try testing.expect(num_func != func_var);
}

test "let-polymorphism with nested structures" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic record type: forall a. { data: List a, count: U64 }
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const elem_var = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    const list_content = types.Content{ .structure = .{ .list = elem_var } };
    const list_var = try env.store.freshFromContent(list_content);

    const u64_content = types.Content{ .structure = .{ .num = .{ .int_precision = .u64 } } };
    const u64_var = try env.store.freshFromContent(u64_content);

    // Create record fields
    var fields = std.ArrayList(types.RecordField).init(test_allocator);
    defer fields.deinit();

    const data_field_name = try env.module_env.insertIdent(base.Ident.for_text("data"));
    const count_field_name = try env.module_env.insertIdent(base.Ident.for_text("count"));

    try fields.append(.{ .name = data_field_name, .var_ = list_var });
    try fields.append(.{ .name = count_field_name, .var_ = u64_var });

    const fields_range = try env.store.appendRecordFields(fields.items);
    const record_content = types.Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = try env.store.fresh() } } };
    const record_var = try env.store.freshFromContent(record_content);

    // Verify it needs instantiation (because of the polymorphic list)
    try testing.expect(env.store.needsInstantiation(record_var));

    // First usage: instantiate for integers
    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const int_record = try instantiateVar(env, record_var, &.{
        .{ .ident = "a", .var_ = int_var },
    });

    // Second usage: instantiate for booleans
    const bool_content = try env.store.mkBool(env.module_env.gpa, env.module_env.getIdentStore(), try env.store.fresh());
    const bool_var = try env.store.freshFromContent(bool_content);
    const bool_record = try instantiateVar(env, record_var, &.{
        .{ .ident = "a", .var_ = bool_var },
    });

    // Verify different instantiations
    try testing.expect(int_record != bool_record);
    try testing.expect(int_record != record_var);
    try testing.expect(bool_record != record_var);
}

test "let-polymorphism prevents over-generalization" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a list with a concrete element (not polymorphic)
    const i32_content = types.Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_var = try env.store.freshFromContent(i32_content);

    const list_i32_content = types.Content{ .structure = .{ .list = i32_var } };
    const list_i32_var = try env.store.freshFromContent(list_i32_content);

    // This should NOT need instantiation because it's already concrete
    try testing.expect(!env.store.needsInstantiation(list_i32_var));
}

test "let-polymorphism with multiple type parameters" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic function: forall a b. (a, b) -> (b, a)
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const b_ident = try env.module_env.insertIdent(base.Ident.for_text("b"));
    const type_a = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });
    const type_b = try env.store.freshFromContent(types.Content{ .rigid_var = b_ident });

    // Create input tuple (a, b)
    const input_elems = try env.store.appendVars(&.{ type_a, type_b });
    const input_tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = input_elems } } };
    const input_tuple_var = try env.store.freshFromContent(input_tuple_content);

    // Create output tuple (b, a)
    const output_elems = try env.store.appendVars(&.{ type_b, type_a });
    const output_tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = output_elems } } };
    const output_tuple_var = try env.store.freshFromContent(output_tuple_content);

    // Create the function type
    const func_content = try env.store.mkFuncPure(&.{input_tuple_var}, output_tuple_var);
    const func_var = try env.store.freshFromContent(func_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(func_var));

    // Multiple instantiations should produce different variables
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });

    const inst1 = try instantiateVar(env, func_var, &.{
        .{ .ident = "a", .var_ = str_var },
        .{ .ident = "b", .var_ = int_var },
    });

    const inst2 = try instantiateVar(env, func_var, &.{
        .{ .ident = "a", .var_ = int_var },
        .{ .ident = "b", .var_ = str_var },
    });

    const bool_content = try env.store.mkBool(env.module_env.gpa, env.module_env.getIdentStore(), try env.store.fresh());
    const bool_var = try env.store.freshFromContent(bool_content);
    const inst3 = try instantiateVar(env, func_var, &.{
        .{ .ident = "a", .var_ = bool_var },
        .{ .ident = "b", .var_ = str_var },
    });

    try testing.expect(inst1 != inst2);
    try testing.expect(inst2 != inst3);
    try testing.expect(inst1 != inst3);
    try testing.expect(inst1 != func_var);
}

test "let-polymorphism with constrained type variables" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a constrained type variable: Num a => a
    // First create a rigid var for the type parameter
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    // Then create a num_poly that references it
    const num_content = types.Content{ .structure = .{ .num = .{ .num_poly = .{ .var_ = type_param, .requirements = .{ .sign_needed = false, .bits_needed = 0 } } } } };
    const num_var = try env.store.freshFromContent(num_content);

    // Create a function that uses this constrained variable: Num a => a -> a -> a
    const add_func_content = try env.store.mkFuncPure(&.{ num_var, num_var }, num_var);
    const add_func_var = try env.store.freshFromContent(add_func_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(add_func_var));

    // Instantiate for different numeric types
    const i32_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const int_add = try instantiateVar(env, add_func_var, &.{
        .{ .ident = "a", .var_ = i32_var },
    });

    const f64_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .frac_precision = .f64 } } });
    const float_add = try instantiateVar(env, add_func_var, &.{
        .{ .ident = "a", .var_ = f64_var },
    });

    try testing.expect(int_add != float_add);
    try testing.expect(int_add != add_func_var);
    try testing.expect(float_add != add_func_var);
}

test "let-polymorphism with simple tag union" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a simple polymorphic Option type: forall a. Option a = Some a | None

    // First create the type parameter
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    // Create the Some tag with a single argument
    const some_tag_name = try env.module_env.insertIdent(base.Ident.for_text("Some"));
    const some_args = try env.store.appendVars(&.{type_param});

    // Create the None tag with no arguments
    const none_tag_name = try env.module_env.insertIdent(base.Ident.for_text("None"));
    // For a tag with no arguments, we use an empty slice
    const none_args = try env.store.appendVars(&.{});

    // Create tag union
    var tags = std.ArrayList(types.Tag).init(test_allocator);
    defer tags.deinit();

    try tags.append(.{ .name = some_tag_name, .args = some_args });
    try tags.append(.{ .name = none_tag_name, .args = none_args });

    const tags_range = try env.store.appendTags(tags.items);
    const tag_union_content = types.Content{ .structure = .{ .tag_union = .{ .tags = tags_range, .ext = try env.store.fresh() } } };
    const option_var = try env.store.freshFromContent(tag_union_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(option_var));

    // Instantiate for different element types
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const string_option = try instantiateVar(env, option_var, &.{
        .{ .ident = "a", .var_ = str_var },
    });

    const num_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const number_option = try instantiateVar(env, option_var, &.{
        .{ .ident = "a", .var_ = num_var },
    });

    try testing.expect(string_option != number_option);
    try testing.expect(string_option != option_var);
    try testing.expect(number_option != option_var);
}

test "let-polymorphism interaction with pattern matching" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic Maybe type: forall a. Maybe a = Just a | Nothing
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    const maybe_var = try env.store.fresh();

    // Create tags
    const just_tag_name = try env.module_env.insertIdent(base.Ident.for_text("Just"));
    const nothing_tag_name = try env.module_env.insertIdent(base.Ident.for_text("Nothing"));

    var tags = std.ArrayList(types.Tag).init(test_allocator);
    defer tags.deinit();

    try tags.append(.{ .name = just_tag_name, .args = try env.store.appendVars(&.{type_param}) });
    try tags.append(.{ .name = nothing_tag_name, .args = types.Var.SafeList.Range.empty() });

    const tags_range = try env.store.appendTags(tags.items);
    const maybe_content = types.Content{ .structure = .{ .tag_union = .{ .tags = tags_range, .ext = try env.store.fresh() } } };
    try env.store.setVarContent(maybe_var, maybe_content);

    // Create a function that pattern matches on Maybe: forall a. Maybe a -> Bool
    const bool_content = try env.store.mkBool(env.module_env.gpa, env.module_env.getIdentStore(), try env.store.fresh());
    const bool_var = try env.store.freshFromContent(bool_content);

    const is_just_func_content = try env.store.mkFuncPure(&.{maybe_var}, bool_var);
    const is_just_func_var = try env.store.freshFromContent(is_just_func_content);

    // Verify it needs instantiation
    try testing.expect(env.store.needsInstantiation(is_just_func_var));

    // Instantiate for different types
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const str_is_just = try instantiateVar(env, is_just_func_var, &.{
        .{ .ident = "a", .var_ = str_var },
    });

    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const int_is_just = try instantiateVar(env, is_just_func_var, &.{
        .{ .ident = "a", .var_ = int_var },
    });

    try testing.expect(str_is_just != int_is_just);
    try testing.expect(str_is_just != is_just_func_var);
}

test "let-polymorphism preserves sharing within single instantiation" {
    const env = try setupTestEnvironment(test_allocator);
    defer cleanup(env, test_allocator);

    // Create a polymorphic type that appears multiple times in a structure
    // forall a. { first: a, second: a, pair: (a, a) }
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(types.Content{ .rigid_var = a_ident });

    // Create tuple (a, a)
    const pair_elems = try env.store.appendVars(&.{ type_param, type_param });
    const pair_content = types.Content{ .structure = .{ .tuple = .{ .elems = pair_elems } } };
    const pair_var = try env.store.freshFromContent(pair_content);

    // Create record
    var fields = std.ArrayList(types.RecordField).init(test_allocator);
    defer fields.deinit();

    const first_name = try env.module_env.insertIdent(base.Ident.for_text("first"));
    const second_name = try env.module_env.insertIdent(base.Ident.for_text("second"));
    const pair_name = try env.module_env.insertIdent(base.Ident.for_text("pair"));

    try fields.append(.{ .name = first_name, .var_ = type_param });
    try fields.append(.{ .name = second_name, .var_ = type_param });
    try fields.append(.{ .name = pair_name, .var_ = pair_var });

    const fields_range = try env.store.appendRecordFields(fields.items);
    const record_content = types.Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = try env.store.fresh() } } };
    const record_var = try env.store.freshFromContent(record_content);

    try testing.expect(env.store.needsInstantiation(record_var));

    // Instantiate once
    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const inst = try instantiateVar(env, record_var, &.{
        .{ .ident = "a", .var_ = str_var },
    });

    // Within this single instantiation, all occurrences of 'a' should be replaced
    // with the same fresh variable (preserving the constraint that first, second,
    // and both elements of pair must have the same type)

    // Get another instantiation to verify they're different
    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = types.Num.int_u32 } });
    const inst2 = try instantiateVar(env, record_var, &.{
        .{ .ident = "a", .var_ = int_var },
    });
    try testing.expect(inst != inst2);
}

/// Helper to parse, canonicalize, and type check source code
fn typeCheckSource(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    czer: *Can,
    checker: *Check,
    has_type_errors: bool,
    canon_expr_idx: Can.CanonicalizedExpr,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Empty scratch space
    parse_ast.store.emptyScratch();

    // Initialize CIR fields
    try module_env.initCIRFields(allocator, "test");

    // Canonicalize
    const czer = try allocator.create(Can);
    czer.* = try Can.init(module_env, parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_result = try czer.canonicalizeExpr(expr_idx);
    const canon_expr = canon_result orelse return error.CanonicalizeError;

    // Type check
    const checker = try allocator.create(Check);
    const empty_modules: []const *ModuleEnv = &.{};
    checker.* = try Check.init(allocator, &module_env.types, module_env, empty_modules, &module_env.store.regions);

    _ = try checker.checkExpr(canon_expr.get_idx());

    const has_type_errors = checker.problems.problems.items.len > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .czer = czer,
        .checker = checker,
        .has_type_errors = has_type_errors,
        .canon_expr_idx = canon_expr,
    };
}

fn cleanupTypeCheckSource(result: anytype, allocator: std.mem.Allocator) void {
    result.checker.deinit();
    allocator.destroy(result.checker);
    result.czer.deinit();
    allocator.destroy(result.czer);
    result.parse_ast.deinit(allocator);
    allocator.destroy(result.parse_ast);
    result.module_env.deinit();
    allocator.destroy(result.module_env);
}

test "identity function should have polymorphic parameter type" {
    const source = "|x| x";

    const result = try typeCheckSource(test_allocator, source);
    defer cleanupTypeCheckSource(result, test_allocator);

    // The canonicalized expression is already computed in typeCheckSource
    const expr_idx = result.canon_expr_idx.get_idx();
    const cir_expr = result.module_env.store.getExpr(expr_idx);

    // Verify it's a lambda
    try testing.expect(cir_expr == .e_lambda);

    // Get the lambda's type
    const lambda_var = ModuleEnv.varFrom(expr_idx);
    const lambda_resolved = result.module_env.types.resolveVar(lambda_var);

    // The lambda should have a function type
    try testing.expect(lambda_resolved.desc.content == .structure);

    const structure = lambda_resolved.desc.content.structure;
    const func = switch (structure) {
        .fn_pure => |f| f,
        .fn_effectful => |f| f,
        .fn_unbound => |f| f,
        else => return error.NotAFunction,
    };

    // Get the parameter type
    const param_types = result.module_env.types.sliceVars(func.args);
    try testing.expect(param_types.len == 1);

    const param_var = param_types[0];
    const param_resolved = result.module_env.types.resolveVar(param_var);

    // The parameter should be a polymorphic variable (flex_var or rigid_var)
    // NOT an error type!
    const is_polymorphic = param_resolved.desc.content == .flex_var or
        param_resolved.desc.content == .rigid_var;

    try testing.expect(is_polymorphic);
}

test "nested identity in apply function should have polymorphic types" {
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, val| f(val)
        \\    app(id, 10)
        \\}
    ;

    const result = try typeCheckSource(test_allocator, source);
    defer cleanupTypeCheckSource(result, test_allocator);

    // Find the id function in the canonicalized output
    // This is more complex, but we want to check that when id is used
    // inside app, it still has polymorphic types

    // Should type check without errors
    try testing.expect(!result.has_type_errors);
}

test "apply function with identity should resolve types correctly" {
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, val| f(val)
        \\    num = app(id, 42)
        \\    str = app(id, "hello")
        \\    { num, str }
        \\}
    ;

    const result = try typeCheckSource(test_allocator, source);
    defer cleanupTypeCheckSource(result, test_allocator);

    // This test should now pass with our let-polymorphism implementation
    // The id function is properly generalized and can be used polymorphically

    // Should type check without errors
    try testing.expect(!result.has_type_errors);
}

test "deeply nested polymorphic functions" {
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, val| f(val)
        \\    twice = |f, val| f(f(val))
        \\    
        \\    result1 = twice(id, 42)
        \\    result2 = app(|x| app(id, x), 100)
        \\    
        \\    { result1, result2 }
        \\}
    ;

    const result = try typeCheckSource(test_allocator, source);
    defer cleanupTypeCheckSource(result, test_allocator);

    // Should type check without errors
    try testing.expect(!result.has_type_errors);
}

/// Helper to run parsing, canonicalization, and type checking on an expression
fn typeCheckExpr(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *ModuleEnv,
    can: *Can,
    checker: *Check,
    has_type_errors: bool,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = undefined,
            .can = undefined,
            .checker = undefined,
            .has_type_errors = true,
        };
    }

    // Canonicalize - CIR is an alias for ModuleEnv, so we use the same module_env
    const cir = module_env;

    const czer = try allocator.create(Can);
    czer.* = try Can.init(cir, parse_ast, null);

    // Run canonicalization - for expressions
    var canon_expr_idx: ?CanonicalizedExpr = null;
    if (parse_ast.root_node_idx != 0) {
        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        canon_expr_idx = try czer.canonicalizeExpr(expr_idx);
    }

    // Type check - continue even if there are parse errors
    const checker = try allocator.create(Check);
    const empty_modules: []const *ModuleEnv = &.{};

    checker.* = try Check.init(allocator, &module_env.types, cir, empty_modules, &cir.store.regions);

    // For expressions, check the expression directly
    if (canon_expr_idx) |expr_idx| {
        _ = try checker.checkExpr(expr_idx.get_idx());
    }

    // Check if there are any type errors
    const has_type_errors = checker.problems.problems.items.len > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = czer,
        .checker = checker,
        .has_type_errors = has_type_errors,
    };
}

fn cleanupExpr(result: anytype, allocator: std.mem.Allocator) void {
    result.checker.deinit();
    allocator.destroy(result.checker);
    result.can.deinit();
    allocator.destroy(result.can);
    result.parse_ast.deinit(allocator);
    allocator.destroy(result.parse_ast);
    result.module_env.deinit();
    allocator.destroy(result.module_env);
}

test "let-polymorphism with function composition" {
    const source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    num_compose = compose(double, add_one)
        \\    result1 = num_compose(5)
        \\    { result1 }
        \\}
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanupExpr(result, test_allocator);

    // Verify no type errors - polymorphic function composition should type check
    try testing.expect(!result.has_type_errors);
}

test "direct polymorphic identity usage - integration test" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanupExpr(result, test_allocator);

    try testing.expect(!result.has_type_errors);
}

test "higher-order function with polymorphic identity - integration test" {
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanupExpr(result, test_allocator);

    try testing.expect(!result.has_type_errors);
}
