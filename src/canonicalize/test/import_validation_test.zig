//! Tests for import validation during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct validation
//! of import statements, including handling of missing modules, unexposed
//! types and values, and proper resolution of valid imports during
//! the canonicalization process.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const parse = @import("parse");

const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const CIR = @import("../CIR.zig");
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;

const CoreCtx = @import("ctx").CoreCtx;
const testing = std.testing;
const expectEqual = testing.expectEqual;

fn expectNoZeroTargetExternalLookup(env: *const ModuleEnv) error{TestUnexpectedResult}!void {
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < env.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (env.store.nodes.get(node_idx).tag != .expr_external_lookup) continue;

        const expr_idx: CIR.Expr.Idx = @enumFromInt(raw_node_idx);
        switch (env.store.getExpr(expr_idx)) {
            .e_lookup_external => |external| {
                try testing.expect(external.target_node_idx != 0);
            },
            else => unreachable,
        }
    }
}

// Helper function to parse and canonicalize source code
fn parseAndCanonicalizeSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    module_envs: ?*std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
) Allocator.Error!struct {
    parse_env: *ModuleEnv,
    ast: *parse.AST,
    can: *Can,
    builtin_ctx: BuiltinTestContext,
} {
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const parse_env = try allocator.create(ModuleEnv);
    // Note: We pass allocator for both gpa and arena since the ModuleEnv
    // will be cleaned up by the caller
    parse_env.* = try ModuleEnv.init(allocator, source);

    const ast = try parse.file(allocator, &parse_env.common);

    // Initialize CIR fields
    try parse_env.initCIRFields("Test");

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    errdefer builtin_ctx.deinit();

    const can = try allocator.create(Can);
    can.* = try Can.initModule(roc_ctx, parse_env, ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_ctx.builtin_module.env,
            .builtin_indices = builtin_ctx.builtin_indices,
        },
        .imported_modules = module_envs,
    });

    return .{
        .parse_env = parse_env,
        .ast = ast,
        .can = can,
        .builtin_ctx = builtin_ctx,
    };
}

test "file imports reject absolute paths before recording dependencies" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    const source =
        \\import "/tmp/data.txt" as data : Str
        \\
        \\main = data
    ;

    var result = try parseAndCanonicalizeSource(allocator, source, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }

    try result.can.canonicalizeFile();

    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    var found_absolute_path = false;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .file_import_absolute_path => |data| {
                found_absolute_path = true;
                try testing.expectEqualStrings("/tmp/data.txt", result.parse_env.getString(data.path));
            },
            .file_import_not_found, .file_import_io_error => return error.UnexpectedFileImportReadDiagnostic,
            else => {},
        }
    }

    try testing.expect(found_absolute_path);
    try expectEqual(@as(usize, 0), result.parse_env.file_dependencies.items.items.len);
}

test "import validation - mix of MODULE NOT FOUND, TYPE NOT EXPOSED, VALUE NOT EXPOSED, and working imports" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    // First, create some module environments with exposed items
    // Create module environment for "Json" module
    const json_env = try allocator.create(ModuleEnv);
    json_env.* = try ModuleEnv.init(allocator, "");
    defer {
        json_env.deinit();
        allocator.destroy(json_env);
    }
    // Add exposed items to Json module
    const Ident = base.Ident;
    const decode_idx = try json_env.common.idents.insert(allocator, Ident.for_text("decode"));
    try json_env.setExposedValueNodeIndexById(decode_idx, 1);
    const encode_idx = try json_env.common.idents.insert(allocator, Ident.for_text("encode"));
    try json_env.setExposedValueNodeIndexById(encode_idx, 2);
    const json_error_idx = try json_env.common.idents.insert(allocator, Ident.for_text("JsonError"));
    try json_env.setExposedTypeNodeIndexById(json_error_idx, 3);
    const decode_problem_idx = try json_env.common.idents.insert(allocator, Ident.for_text("DecodeProblem"));
    try json_env.setExposedTypeNodeIndexById(decode_problem_idx, 4);

    // Create module environment for "Utils" module
    const utils_env = try allocator.create(ModuleEnv);
    utils_env.* = try ModuleEnv.init(allocator, "");
    defer {
        utils_env.deinit();
        allocator.destroy(utils_env);
    }
    // Add exposed items to Utils module
    const map_idx = try utils_env.common.idents.insert(allocator, Ident.for_text("map"));
    try utils_env.setExposedValueNodeIndexById(map_idx, 1);
    const filter_idx = try utils_env.common.idents.insert(allocator, Ident.for_text("filter"));
    try utils_env.setExposedValueNodeIndexById(filter_idx, 2);
    const result_idx = try utils_env.common.idents.insert(allocator, Ident.for_text("Try"));
    try utils_env.setExposedTypeNodeIndexById(result_idx, 3);
    // Parse source code with various import statements
    const source =
        \\# Import from existing module with valid items
        \\import DataJson exposing [decode, JsonError]
        \\
        \\# Import from existing module with some invalid items
        \\import Utils exposing [map, doesNotExist, Try, InvalidType]
        \\
        \\# Import from non-existent module
        \\import NonExistent exposing [something, SomeType]
        \\
        \\# Valid import with all exposed items
        \\import DataJson exposing [encode, DecodeProblem]
        \\
        \\main = "test"
    ;
    // Parse the source
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const parse_env = try allocator.create(ModuleEnv);
    parse_env.* = try ModuleEnv.init(allocator, source);
    defer {
        parse_env.deinit();
        allocator.destroy(parse_env);
    }
    const ast = try parse.file(allocator, &parse_env.common);
    defer ast.deinit();
    // Initialize CIR fields
    try parse_env.initCIRFields("Test");

    // Now create module_envs using parse_env's ident store
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();
    const json_module_ident = try parse_env.common.idents.insert(allocator, Ident.for_text("DataJson"));
    const json_qualified_ident = try json_env.common.insertIdent(json_env.gpa, Ident.for_text("DataJson"));
    try module_envs.put(json_module_ident, .{ .env = json_env, .qualified_type_ident = json_qualified_ident });
    const utils_module_ident = try parse_env.common.idents.insert(allocator, Ident.for_text("Utils"));
    const utils_qualified_ident = try utils_env.common.insertIdent(utils_env.gpa, Ident.for_text("Utils"));
    try module_envs.put(utils_module_ident, .{ .env = utils_env, .qualified_type_ident = utils_qualified_ident });

    // Canonicalize with module validation
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var can = try Can.initModule(roc_ctx, parse_env, ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_ctx.builtin_module.env,
            .builtin_indices = builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer can.deinit();
    try can.canonicalizeFile();
    // Collect all diagnostics
    var module_not_found_count: u32 = 0;
    var value_not_exposed_count: u32 = 0;
    var type_not_exposed_count: u32 = 0;
    var found_does_not_exist = false;
    var found_invalid_type = false;
    var found_non_existent = false;
    const diagnostics = try parse_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .module_not_found => |d| {
                module_not_found_count += 1;
                const module_name = parse_env.getIdent(d.module_name);
                if (std.mem.eql(u8, module_name, "NonExistent")) {
                    found_non_existent = true;
                }
            },
            .value_not_exposed => |d| {
                value_not_exposed_count += 1;
                const value_name = parse_env.getIdent(d.value_name);
                if (std.mem.eql(u8, value_name, "doesNotExist")) {
                    found_does_not_exist = true;
                }
            },
            .type_not_exposed => |d| {
                type_not_exposed_count += 1;
                const type_name = parse_env.getIdent(d.type_name);
                if (std.mem.eql(u8, type_name, "InvalidType")) {
                    found_invalid_type = true;
                }
            },
            else => {},
        }
    }
    // Verify we got the expected errors
    try expectEqual(@as(u32, 1), module_not_found_count); // NonExistent module
    try expectEqual(@as(u32, 1), value_not_exposed_count); // doesNotExist
    try expectEqual(@as(u32, 1), type_not_exposed_count); // InvalidType
    try expectEqual(true, found_non_existent);
    try expectEqual(true, found_does_not_exist);
    try expectEqual(true, found_invalid_type);
    // Verify that valid imports didn't generate errors
    // The imports for decode, JsonError, map, Try, encode, and DecodeProblem should all work
}

test "import validation - type module associated values are importable via exposing" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const Ident = base.Ident;

    // A type module whose associated values should be importable via
    // `import FooBar exposing [square]`. Its associated items are exposed under
    // the qualified `FooBar.square` form (which also powers qualified access),
    // so the importer must resolve the bare `exposing` name to that form.
    const foobar_source =
        \\FooBar :: {}.{
        \\    square : U64 -> U64
        \\    square = |x| x * x
        \\
        \\    cube : U64 -> U64
        \\    cube = |x| x * x * x
        \\}
    ;
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const foobar_env = try allocator.create(ModuleEnv);
    foobar_env.* = try ModuleEnv.init(allocator, foobar_source);
    defer {
        foobar_env.deinit();
        allocator.destroy(foobar_env);
    }
    const foobar_ast = try parse.file(allocator, &foobar_env.common);
    defer foobar_ast.deinit();
    try foobar_env.initCIRFields("FooBar");

    var foobar_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer foobar_builtin_ctx.deinit();

    var foobar_can = try Can.initModule(roc_ctx, foobar_env, foobar_ast, foobar_builtin_ctx.canInitContext());
    defer foobar_can.deinit();
    try foobar_can.canonicalizeFile();

    // Now canonicalize an importer that uses an exposed associated value.
    const importer_source =
        \\import FooBar exposing [square]
        \\
        \\main = square(12)
    ;
    const importer_env = try allocator.create(ModuleEnv);
    importer_env.* = try ModuleEnv.init(allocator, importer_source);
    defer {
        importer_env.deinit();
        allocator.destroy(importer_env);
    }
    const importer_ast = try parse.file(allocator, &importer_env.common);
    defer importer_ast.deinit();
    try importer_env.initCIRFields("Importer");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();
    const foobar_module_ident = try importer_env.common.idents.insert(allocator, Ident.for_text("FooBar"));
    const foobar_qualified_ident = try foobar_env.common.insertIdent(foobar_env.gpa, Ident.for_text("FooBar"));
    try module_envs.put(foobar_module_ident, .{ .env = foobar_env, .qualified_type_ident = foobar_qualified_ident });

    var importer_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer importer_builtin_ctx.deinit();

    var importer_can = try Can.initModule(roc_ctx, importer_env, importer_ast, .{
        .builtin_types = .{
            .builtin_module_env = importer_builtin_ctx.builtin_module.env,
            .builtin_indices = importer_builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer importer_can.deinit();
    try importer_can.canonicalizeFile();

    // The exposed associated value must resolve cleanly: no "value not exposed"
    // at the import, and no unresolved reference where `square` is used.
    const diagnostics = try importer_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .value_not_exposed => |d| {
                std.debug.print("unexpected VALUE NOT EXPOSED for {s}\n", .{importer_env.getIdent(d.value_name)});
                return error.UnexpectedValueNotExposed;
            },
            .qualified_ident_does_not_exist => |d| {
                std.debug.print("unexpected unresolved ident {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedUnresolvedIdent;
            },
            .ident_not_in_scope => |d| {
                std.debug.print("unexpected ident not in scope {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedIdentNotInScope;
            },
            else => {},
        }
    }
}

test "import validation - exposed nested type associated function resolves via short name (issue 9697)" {
    // repro for https://github.com/roc-lang/roc/issues/9697
    //
    // A nested type imported via `import Chess exposing [Square]` must let its
    // associated function be called through the exposed short name, exactly the
    // way the fully qualified `Chess.Square.create` already works. The exposed
    // `Square` resolves fine as a *type*; the bug is that `Square.create` (a
    // static-dispatch lookup of the nested type's associated value) is reported
    // as "does not exist".
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const Ident = base.Ident;

    const chess_source =
        \\Chess :: {}.{
        \\    Square :: { rank : U8, file : U8 }.{
        \\        create = |{ rank, file }| { { rank, file } }
        \\    }
        \\    to_rank = |{ rank }| rank
        \\    to_file = |{ file }| file
        \\}
    ;
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const chess_env = try allocator.create(ModuleEnv);
    chess_env.* = try ModuleEnv.init(allocator, chess_source);
    defer {
        chess_env.deinit();
        allocator.destroy(chess_env);
    }
    const chess_ast = try parse.file(allocator, &chess_env.common);
    defer chess_ast.deinit();
    try chess_env.initCIRFields("Chess");

    var chess_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer chess_builtin_ctx.deinit();

    var chess_can = try Can.initModule(roc_ctx, chess_env, chess_ast, chess_builtin_ctx.canInitContext());
    defer chess_can.deinit();
    try chess_can.canonicalizeFile();

    const importer_source =
        \\import Chess exposing [Square]
        \\
        \\main = Square.create({ rank: 3, file: 5 })
    ;
    const importer_env = try allocator.create(ModuleEnv);
    importer_env.* = try ModuleEnv.init(allocator, importer_source);
    defer {
        importer_env.deinit();
        allocator.destroy(importer_env);
    }
    const importer_ast = try parse.file(allocator, &importer_env.common);
    defer importer_ast.deinit();
    try importer_env.initCIRFields("Importer");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();
    const chess_module_ident = try importer_env.common.idents.insert(allocator, Ident.for_text("Chess"));
    const chess_qualified_ident = try chess_env.common.insertIdent(chess_env.gpa, Ident.for_text("Chess"));
    try module_envs.put(chess_module_ident, .{ .env = chess_env, .qualified_type_ident = chess_qualified_ident });

    var importer_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer importer_builtin_ctx.deinit();

    var importer_can = try Can.initModule(roc_ctx, importer_env, importer_ast, .{
        .builtin_types = .{
            .builtin_module_env = importer_builtin_ctx.builtin_module.env,
            .builtin_indices = importer_builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer importer_can.deinit();
    try importer_can.canonicalizeFile();

    // The associated function reached through the exposed short name must
    // resolve. Any of these diagnostics on the importer means `Square.create`
    // failed to resolve.
    const diagnostics = try importer_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .qualified_ident_does_not_exist => |d| {
                std.debug.print("unexpected unresolved qualified ident: {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedUnresolvedIdent;
            },
            .nested_value_not_found => |d| {
                std.debug.print("unexpected nested value not found: {s}.{s}\n", .{ importer_env.getIdent(d.parent_name), importer_env.getIdent(d.nested_name) });
                return error.UnexpectedNestedValueNotFound;
            },
            .nested_type_not_found => |d| {
                std.debug.print("unexpected nested type not found: {s}.{s}\n", .{ importer_env.getIdent(d.parent_name), importer_env.getIdent(d.nested_name) });
                return error.UnexpectedNestedTypeNotFound;
            },
            .value_not_exposed => |d| {
                std.debug.print("unexpected value not exposed: {s}\n", .{importer_env.getIdent(d.value_name)});
                return error.UnexpectedValueNotExposed;
            },
            .type_not_exposed => |d| {
                std.debug.print("unexpected type not exposed: {s}\n", .{importer_env.getIdent(d.type_name)});
                return error.UnexpectedTypeNotExposed;
            },
            .ident_not_in_scope => |d| {
                std.debug.print("unexpected ident not in scope: {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedIdentNotInScope;
            },
            else => {},
        }
    }
}

test "import validation - exposing a type module's main type by name is not a redeclaration" {
    // A type module's main type is auto-exposed, so naming it explicitly in
    // `exposing` (as platform code does with `import NodeB exposing [NodeB]`)
    // binds the same external type to the same name twice. That is idempotent,
    // not a DUPLICATE DEFINITION.
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const Ident = base.Ident;

    const shape_source =
        \\Shape :: {}.{
        \\    area : U64 -> U64
        \\    area = |x| x
        \\}
    ;
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const shape_env = try allocator.create(ModuleEnv);
    shape_env.* = try ModuleEnv.init(allocator, shape_source);
    defer {
        shape_env.deinit();
        allocator.destroy(shape_env);
    }
    const shape_ast = try parse.file(allocator, &shape_env.common);
    defer shape_ast.deinit();
    try shape_env.initCIRFields("Shape");

    var shape_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer shape_builtin_ctx.deinit();

    var shape_can = try Can.initModule(roc_ctx, shape_env, shape_ast, shape_builtin_ctx.canInitContext());
    defer shape_can.deinit();
    try shape_can.canonicalizeFile();

    const importer_source =
        \\import Shape exposing [Shape]
        \\
        \\main = Shape.area(5)
    ;
    const importer_env = try allocator.create(ModuleEnv);
    importer_env.* = try ModuleEnv.init(allocator, importer_source);
    defer {
        importer_env.deinit();
        allocator.destroy(importer_env);
    }
    const importer_ast = try parse.file(allocator, &importer_env.common);
    defer importer_ast.deinit();
    try importer_env.initCIRFields("Importer");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();
    const shape_module_ident = try importer_env.common.idents.insert(allocator, Ident.for_text("Shape"));
    const shape_qualified_ident = try shape_env.common.insertIdent(shape_env.gpa, Ident.for_text("Shape"));
    try module_envs.put(shape_module_ident, .{ .env = shape_env, .qualified_type_ident = shape_qualified_ident });

    var importer_builtin_ctx = try BuiltinTestContext.init(allocator);
    defer importer_builtin_ctx.deinit();

    var importer_can = try Can.initModule(roc_ctx, importer_env, importer_ast, .{
        .builtin_types = .{
            .builtin_module_env = importer_builtin_ctx.builtin_module.env,
            .builtin_indices = importer_builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer importer_can.deinit();
    try importer_can.canonicalizeFile();

    const diagnostics = try importer_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .shadowing_warning => |d| {
                std.debug.print("unexpected redeclaration of {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedRedeclaration;
            },
            .qualified_ident_does_not_exist => |d| {
                std.debug.print("unexpected unresolved qualified ident: {s}\n", .{importer_env.getIdent(d.ident)});
                return error.UnexpectedUnresolvedIdent;
            },
            else => {},
        }
    }
}

test "unresolved exposed value is not imported as external lookup target zero" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const Ident = base.Ident;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const a_source =
        \\T := [].{
        \\    f = |x| x
        \\}
    ;

    var a_env = try ModuleEnv.init(allocator, a_source);
    defer a_env.deinit();
    try a_env.initCIRFields("A");

    const a_ast = try parse.file(allocator, &a_env.common);
    defer a_ast.deinit();

    var a_can = try Can.initModule(roc_ctx, &a_env, a_ast, builtin_ctx.canInitContext());
    defer a_can.deinit();
    try a_can.canonicalizeFile();

    const a_diagnostics = try a_env.getDiagnostics();
    defer allocator.free(a_diagnostics);
    for (a_diagnostics) |diagnostic| {
        switch (diagnostic) {
            else => return error.UnexpectedImportDiagnostic,
        }
    }

    const importer_source =
        \\import A
        \\
        \\main = A.f({})
    ;

    var importer_env = try ModuleEnv.init(allocator, importer_source);
    defer importer_env.deinit();
    try importer_env.initCIRFields("Importer");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const a_import_ident = try importer_env.insertIdent(Ident.for_text("A"));
    const a_qualified_ident = try a_env.insertIdent(Ident.for_text("A"));
    try module_envs.put(a_import_ident, .{
        .env = &a_env,
        .qualified_type_ident = a_qualified_ident,
    });

    const importer_ast = try parse.file(allocator, &importer_env.common);
    defer importer_ast.deinit();

    var importer_can = try Can.initModule(roc_ctx, &importer_env, importer_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_ctx.builtin_module.env,
            .builtin_indices = builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer importer_can.deinit();
    try importer_can.canonicalizeFile();

    var found_missing_f = false;
    const importer_diagnostics = try importer_env.getDiagnostics();
    defer allocator.free(importer_diagnostics);
    for (importer_diagnostics) |diagnostic| {
        switch (diagnostic) {
            .nested_value_not_found => |d| {
                if (std.mem.eql(u8, importer_env.getIdent(d.parent_name), "A") and
                    std.mem.eql(u8, importer_env.getIdent(d.nested_name), "f"))
                {
                    found_missing_f = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(found_missing_f);
    try expectNoZeroTargetExternalLookup(&importer_env);
}

test "import validation - no module_envs provided" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    // Parse source code with import statements
    const source =
        \\import DataJson exposing [decode, JsonError]
        \\
        \\main = "test"
    ;
    // Let's do it manually instead of using the helper to isolate the issue
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const parse_env = try allocator.create(ModuleEnv);
    parse_env.* = try ModuleEnv.init(allocator, source);
    defer {
        parse_env.deinit();
        allocator.destroy(parse_env);
    }
    const ast = try parse.file(allocator, &parse_env.common);
    defer ast.deinit();
    // Initialize CIR fields
    try parse_env.initCIRFields("Test");
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    // Create czer without any explicit import envs
    var can = try Can.initModule(roc_ctx, parse_env, ast, builtin_ctx.canInitContext());
    defer can.deinit();
    try can.canonicalizeFile();
    const diagnostics = try parse_env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .module_not_found => {
                // expected this error message, ignore
            },
            else => {
                // these errors are not expected
                try testing.expect(false);
            },
        }
    }
}

test "import interner - Import.Idx functionality" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    // Parse source code with multiple imports, including duplicates
    const source =
        \\import List
        \\import Dict
        \\import List  # Duplicate - should get same Import.Idx
        \\import Json.Decode
        \\import Set
        \\import Json.Decode  # Another duplicate
        \\
        \\main = "test"
    ;
    // Parse and canonicalize without module validation to focus on Import.Idx
    var result = try parseAndCanonicalizeSource(allocator, source, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }
    try result.can.canonicalizeFile();
    // Check that the explicit user imports are deduplicated.
    // Builtin is also present as an implicit compiler-owned import.
    var explicit_import_count: usize = 0;
    // Verify each unique module has an Import.Idx by checking the imports list
    var found_list = false;
    var found_dict = false;
    var found_json_decode = false;
    var found_set = false;
    for (result.parse_env.imports.imports.items.items) |import_string_idx| {
        const module_name = result.parse_env.getString(import_string_idx);
        if (CIR.Import.isCompilerBuiltinImportName(module_name)) continue;

        explicit_import_count += 1;
        if (std.mem.eql(u8, module_name, "List")) {
            found_list = true;
        } else if (std.mem.eql(u8, module_name, "Dict")) {
            found_dict = true;
        } else if (std.mem.eql(u8, module_name, "Json")) {
            found_json_decode = true;
        } else if (std.mem.eql(u8, module_name, "Set")) {
            found_set = true;
        }
    }
    try expectEqual(@as(usize, 4), explicit_import_count);
    // Verify all expected modules were found
    try expectEqual(true, found_list);
    try expectEqual(true, found_dict);
    try expectEqual(true, found_json_decode);
    try expectEqual(true, found_set);
}

test "import interner - many imports keep stable module identity keys" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    const import_count = 320;

    var source = std.ArrayList(u8).empty;
    defer source.deinit(allocator);

    for (0..import_count) |i| {
        const line = try std.fmt.allocPrint(allocator, "import T{d}\n", .{i});
        defer allocator.free(line);
        try source.appendSlice(allocator, line);
    }
    try source.appendSlice(allocator, "\nmain = \"test\"\n");

    var result = try parseAndCanonicalizeSource(allocator, source.items, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }

    try result.can.canonicalizeFile();

    var explicit_import_count: usize = 0;
    for (result.parse_env.imports.imports.items.items) |import_string_idx| {
        const module_name = result.parse_env.getString(import_string_idx);
        if (CIR.Import.isCompilerBuiltinImportName(module_name)) continue;

        explicit_import_count += 1;
        try testing.expect(std.mem.startsWith(u8, module_name, "T"));
    }

    try expectEqual(@as(usize, import_count), explicit_import_count);
}

test "import interner - comprehensive usage example" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    // Parse source with imports used in different contexts
    const source =
        \\import List exposing [map, filter]
        \\import Dict
        \\import Try exposing [Try, withDefault]
        \\
        \\process : List Str -> Dict Str Nat
        \\process = \items ->
        \\    items
        \\    |> List.map Str.toLower
        \\    |> List.filter \item -> Str.length item > 3
        \\    |> List.foldl Dict.empty \dict, item ->
        \\        Dict.update dict item \maybeCount ->
        \\            when maybeCount is
        \\                Present count -> Present (count + 1)
        \\                Missing -> Present 1
    ;
    // Parse and canonicalize without module validation to focus on import interning
    var result = try parseAndCanonicalizeSource(allocator, source, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }
    try result.can.canonicalizeFile();
    // Check that the explicit user imports are present once each.
    // Builtin is also present as an implicit compiler-owned import.
    var explicit_import_count: usize = 0;
    // Verify each unique module was imported
    var found_list = false;
    var found_dict = false;
    var found_result = false;
    for (result.parse_env.imports.imports.items.items) |import_string_idx| {
        const module_name = result.parse_env.getString(import_string_idx);
        if (CIR.Import.isCompilerBuiltinImportName(module_name)) continue;

        explicit_import_count += 1;
        if (std.mem.eql(u8, module_name, "List")) {
            found_list = true;
        } else if (std.mem.eql(u8, module_name, "Dict")) {
            found_dict = true;
        } else if (std.mem.eql(u8, module_name, "Try")) {
            found_result = true;
        }
    }
    try expectEqual(@as(usize, 3), explicit_import_count);
    // Verify all expected modules were found
    try expectEqual(true, found_list);
    try expectEqual(true, found_dict);
    try expectEqual(true, found_result);
}

test "module scopes - imports work in module scope" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    // Parse source with imports used in module scope
    const source =
        \\import List
        \\import Dict
        \\
        \\process = \items ->
        \\    # List and Dict are available here
        \\    list = List.map items \x -> x + 1
        \\    dict = Dict.empty
        \\    { list, dict }
    ;
    // Parse and canonicalize without external module validation
    var result = try parseAndCanonicalizeSource(allocator, source, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }
    try result.can.canonicalizeFile();
    // Verify that List and Dict imports were processed correctly
    const imports = result.parse_env.imports.imports;
    try testing.expect(imports.len() >= 2); // List and Dict
    var has_list = false;
    var has_dict = false;
    for (imports.items.items) |import_string_idx| {
        const import_name = result.parse_env.getString(import_string_idx);
        if (std.mem.eql(u8, import_name, "List")) has_list = true;
        if (std.mem.eql(u8, import_name, "Dict")) has_dict = true;
    }
    try testing.expect(has_list);
    try testing.expect(has_dict);
}

test "module-qualified lookups with e_lookup_external" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    // Parse source with module-qualified lookups
    const source =
        \\import List
        \\import Dict
        \\
        \\main =
        \\    list = List.map [1, 2, 3] \x -> x * 2
        \\    dict = Dict.insert Dict.empty "key" "value"
        \\    List.len list
    ;
    // Parse and canonicalize
    var result = try parseAndCanonicalizeSource(allocator, source, null);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }
    try result.can.canonicalizeFile();
    // Verify the module names are correct
    const imports_list = result.parse_env.imports.imports;
    try testing.expect(imports_list.len() >= 2); // List and Dict
    var has_list = false;
    var has_dict = false;
    for (imports_list.items.items) |import_string_idx| {
        const import_name = result.parse_env.getString(import_string_idx);
        if (std.mem.eql(u8, import_name, "List")) has_list = true;
        if (std.mem.eql(u8, import_name, "Dict")) has_dict = true;
    }
    try testing.expect(has_list);
    try testing.expect(has_dict);
}

test "exposed_items - tracking CIR node indices for exposed items" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    // Create module environments with exposed items
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    // Create temporary ident store for module name lookup
    var temp_idents = try base.Ident.Store.initCapacity(allocator, 16);
    defer temp_idents.deinit(allocator);

    // Create a "MathUtils" module with some exposed definitions
    const math_env = try allocator.create(ModuleEnv);
    math_env.* = try ModuleEnv.init(allocator, "");
    defer {
        math_env.deinit();
        allocator.destroy(math_env);
    }
    // Add exposed items
    const Ident = base.Ident;
    const add_idx = try math_env.common.idents.insert(allocator, Ident.for_text("add"));
    try math_env.setExposedValueNodeIndexById(add_idx, 1);
    const multiply_idx = try math_env.common.idents.insert(allocator, Ident.for_text("multiply"));
    try math_env.setExposedValueNodeIndexById(multiply_idx, 2);
    const pi_idx = try math_env.common.idents.insert(allocator, Ident.for_text("PI"));
    try math_env.setExposedTypeNodeIndexById(pi_idx, 3);

    const math_utils_ident = try temp_idents.insert(allocator, Ident.for_text("MathUtils"));
    const math_utils_qualified_ident = try math_env.common.insertIdent(math_env.gpa, Ident.for_text("MathUtils"));
    try module_envs.put(math_utils_ident, .{ .env = math_env, .qualified_type_ident = math_utils_qualified_ident });
    // Parse source that uses these exposed items
    const source =
        \\import MathUtils exposing [add, multiply, PI]
        \\
        \\calculate = \x, y ->
        \\    sum = add x y
        \\    product = multiply x y
        \\    circumference = multiply (multiply 2 PI) x
        \\    { sum, product, circumference }
    ;
    // Parse and canonicalize with module environments
    var result = try parseAndCanonicalizeSource(allocator, source, &module_envs);
    defer {
        result.can.deinit();
        allocator.destroy(result.can);
        result.builtin_ctx.deinit();
        result.ast.deinit();
        result.parse_env.deinit();
        allocator.destroy(result.parse_env);
    }
    try result.can.canonicalizeFile();
    // Verify the MathUtils import was registered
    const imports_list = result.parse_env.imports.imports;
    var has_mathutils = false;
    for (imports_list.items.items) |import_string_idx| {
        const import_name = result.parse_env.getString(import_string_idx);
        if (std.mem.eql(u8, import_name, "MathUtils")) {
            has_mathutils = true;
            break;
        }
    }
    try testing.expect(has_mathutils);
}

test "imported type-module tag rejects alias target" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const imported_source =
        \\Other : [Tag]
    ;

    var imported_env = try ModuleEnv.init(allocator, imported_source);
    defer imported_env.deinit();
    try imported_env.initCIRFields("Other");

    const imported_ast = try parse.file(allocator, &imported_env.common);
    defer imported_ast.deinit();

    var imported_can = try Can.initModule(roc_ctx, &imported_env, imported_ast, builtin_ctx.canInitContext());
    defer imported_can.deinit();
    try imported_can.canonicalizeFile();

    const source =
        \\import Other exposing [Other]
        \\
        \\bad = Other.Tag
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Main");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const other_ident = try env.insertIdent(base.Ident.for_text("Other"));
    const other_qualified_ident = try imported_env.insertIdent(base.Ident.for_text("Other"));
    try module_envs.put(other_ident, .{
        .env = &imported_env,
        .qualified_type_ident = other_qualified_ident,
    });

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_ctx.builtin_module.env,
            .builtin_indices = builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer can.deinit();
    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);

    var found_alias_error = false;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .type_alias_but_needed_nominal => |d| {
                const name = env.getIdent(d.name);
                if (std.mem.eql(u8, name, "Other")) {
                    found_alias_error = true;
                }
            },
            else => {},
        }
    }

    try testing.expect(found_alias_error);
}

test "imported nested associated types resolve by qualified export key" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    const roc_ctx = CoreCtx.testing(allocator, allocator);

    const imported_source =
        \\A := [A].{
        \\    Inner := [AInner]
        \\}
        \\
        \\B := [B].{
        \\    Inner := [BInner]
        \\}
    ;

    var imported_env = try ModuleEnv.init(allocator, imported_source);
    defer imported_env.deinit();
    try imported_env.initCIRFields("Types");

    const imported_ast = try parse.file(allocator, &imported_env.common);
    defer imported_ast.deinit();

    var imported_can = try Can.initModule(roc_ctx, &imported_env, imported_ast, builtin_ctx.canInitContext());
    defer imported_can.deinit();
    try imported_can.canonicalizeFile();

    const source =
        \\import Types
        \\
        \\a : Types.A.Inner
        \\a = Types.A.Inner.AInner
        \\
        \\b : Types.B.Inner
        \\b = Types.B.Inner.BInner
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Main");

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const types_ident = try env.insertIdent(base.Ident.for_text("Types"));
    const types_qualified_ident = try imported_env.insertIdent(base.Ident.for_text("Types"));
    try module_envs.put(types_ident, .{
        .env = &imported_env,
        .qualified_type_ident = types_qualified_ident,
    });

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_ctx.builtin_module.env,
            .builtin_indices = builtin_ctx.builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer can.deinit();
    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);

    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .type_not_exposed,
            .type_from_missing_module,
            .module_not_imported,
            .undeclared_type,
            .type_alias_but_needed_nominal,
            .qualified_ident_does_not_exist,
            => return error.UnexpectedDiagnostic,
            else => {},
        }
    }
}

test "export count safety - ensures safe u16 casting" {
    var gpa_state = std.heap.DebugAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    // This test verifies that we check export counts to ensure safe casting to u16
    // The check triggers when exposed_items.len >= maxInt(u16) (65535)
    // This leaves 0 available as a potential sentinel value if needed
    // Verify the threshold is what we expect
    try expectEqual(@as(u32, 65535), std.math.maxInt(u16));
    // Test the diagnostic for exactly maxInt(u16) exports
    var env1 = try ModuleEnv.init(allocator, "");
    defer env1.deinit();
    try env1.initCIRFields("Test");
    const diag_at_limit = CIR.Diagnostic{
        .too_many_exports = .{
            .count = 65535, // Exactly at the limit
            .region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 10 } },
        },
    };
    const diag_idx1 = try env1.store.addDiagnostic(diag_at_limit);
    const retrieved1 = env1.store.getDiagnostic(diag_idx1);
    switch (retrieved1) {
        .too_many_exports => |d| {
            try expectEqual(@as(u32, 65535), d.count);
        },
        else => return error.UnexpectedDiagnostic,
    }
    // Test the diagnostic for exceeding the limit
    var env2 = try ModuleEnv.init(allocator, "");
    defer env2.deinit();
    try env2.initCIRFields("Test");
    const diag_over_limit = CIR.Diagnostic{
        .too_many_exports = .{
            .count = 70000, // Well over the limit
            .region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 10 } },
        },
    };
    const diag_idx2 = try env2.store.addDiagnostic(diag_over_limit);
    const retrieved2 = env2.store.getDiagnostic(diag_idx2);
    switch (retrieved2) {
        .too_many_exports => |d| {
            try expectEqual(@as(u32, 70000), d.count);
        },
        else => return error.UnexpectedDiagnostic,
    }
    // Demonstrate that values under the limit can be safely cast to u16
    const safe_count: u32 = 65534; // Just under the limit
    const casted: u16 = @intCast(safe_count); // This is safe
    try expectEqual(@as(u16, 65534), casted);
    // The actual runtime check in createExposedScope ensures that we never
    // attempt to cast values >= 65535 to u16, preventing overflow
}
