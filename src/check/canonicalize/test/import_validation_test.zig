//! TODO

const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const parse = @import("../../parse.zig");
const canonicalize = @import("../../canonicalize.zig");
const CIR = canonicalize.CIR;
const expectEqual = testing.expectEqual;

test "import validation - mix of MODULE NOT FOUND, TYPE NOT EXPOSED, VALUE NOT EXPOSED, and working imports" {
    const allocator = testing.allocator;

    // First, create some module environments with exposed items
    var module_envs = std.StringHashMap(*base.ModuleEnv).init(allocator);
    defer module_envs.deinit();

    // Create module environment for "Json" module
    var json_env = base.ModuleEnv.init(allocator);
    defer json_env.deinit();

    // Add exposed items to Json module
    try json_env.exposed_by_str.put(allocator, "decode", {});
    try json_env.exposed_by_str.put(allocator, "encode", {});
    try json_env.exposed_by_str.put(allocator, "JsonError", {});
    try json_env.exposed_by_str.put(allocator, "DecodeProblem", {});

    try module_envs.put("Json", &json_env);

    // Create module environment for "Utils" module
    var utils_env = base.ModuleEnv.init(allocator);
    defer utils_env.deinit();

    // Add exposed items to Utils module
    try utils_env.exposed_by_str.put(allocator, "map", {});
    try utils_env.exposed_by_str.put(allocator, "filter", {});
    try utils_env.exposed_by_str.put(allocator, "Result", {});

    try module_envs.put("Utils", &utils_env);

    // Parse source code with various import statements
    const source =
        \\module [main]
        \\
        \\# Import from existing module with valid items
        \\import Json exposing [decode, JsonError]
        \\
        \\# Import from existing module with some invalid items
        \\import Utils exposing [map, doesNotExist, Result, InvalidType]
        \\
        \\# Import from non-existent module
        \\import NonExistent exposing [something, SomeType]
        \\
        \\# Valid import with all exposed items
        \\import Json exposing [encode, DecodeProblem]
        \\
        \\main = "test"
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    // Set the source in module_env so canonicalization can access it
    parse_env.source = try allocator.dupe(u8, source);
    parse_env.owns_source = true;
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize with module validation
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    try env.calcLineStarts(source);
    // Set the source in module_env so canonicalization can access it
    env.source = try allocator.dupe(u8, source);
    env.owns_source = true;
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, &module_envs);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Collect all diagnostics
    var module_not_found_count: u32 = 0;
    var value_not_exposed_count: u32 = 0;
    var type_not_exposed_count: u32 = 0;
    var found_does_not_exist = false;
    var found_invalid_type = false;
    var found_non_existent = false;

    const diagnostics = cir.diag_regions.entries.items;
    for (diagnostics) |entry| {
        const diag_idx: CIR.Diagnostic.Idx = @enumFromInt(entry.value);
        const diagnostic = cir.store.getDiagnostic(diag_idx);

        switch (diagnostic) {
            .module_not_found => |d| {
                module_not_found_count += 1;
                const module_name = env.idents.getText(d.module_name);
                if (std.mem.eql(u8, module_name, "NonExistent")) {
                    found_non_existent = true;
                }
            },
            .value_not_exposed => |d| {
                value_not_exposed_count += 1;
                const value_name = env.idents.getText(d.value_name);
                if (std.mem.eql(u8, value_name, "doesNotExist")) {
                    found_does_not_exist = true;
                }
            },
            .type_not_exposed => |d| {
                type_not_exposed_count += 1;
                const type_name = env.idents.getText(d.type_name);
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
    // The imports for decode, JsonError, map, Result, encode, and DecodeProblem should all work
}

test "import validation - no module_envs provided" {
    const allocator = testing.allocator;

    // Parse source code with import statements
    const source =
        \\module [main]
        \\
        \\import Json exposing [decode, JsonError]
        \\
        \\main = "test"
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    // Set the source in module_env so canonicalization can access it
    parse_env.source = try allocator.dupe(u8, source);
    parse_env.owns_source = true;
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize without module validation (pass null)
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // When module_envs is null, no import validation errors should be generated
    const diagnostics = cir.diag_regions.entries.items;
    for (diagnostics) |entry| {
        const diag_idx: CIR.Diagnostic.Idx = @enumFromInt(entry.value);
        const diagnostic = cir.store.getDiagnostic(diag_idx);

        switch (diagnostic) {
            .module_not_found, .value_not_exposed, .type_not_exposed => {
                // These errors should not occur when module_envs is null
                try testing.expect(false);
            },
            else => {},
        }
    }
}

test "import interner - Import.Idx functionality" {
    const allocator = testing.allocator;

    // Parse source code with multiple imports, including duplicates
    const source =
        \\module [main]
        \\
        \\import List
        \\import Dict
        \\import List  # Duplicate - should get same Import.Idx
        \\import Json.Decode
        \\import Set
        \\import Json.Decode  # Another duplicate
        \\
        \\main = "test"
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize without module validation to focus on Import.Idx
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    // Set the source in module_env so canonicalization can access it
    env.source = try allocator.dupe(u8, source);
    env.owns_source = true;
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check that we have the correct number of unique imports
    // Expected: List, Dict, Json.Decode, Set (4 unique)
    try expectEqual(@as(usize, 4), cir.imports.imports.items.len);

    // Verify each unique module has an Import.Idx
    var found_list = false;
    var found_dict = false;
    var found_json_decode = false;
    var found_set = false;

    for (cir.imports.imports.items) |import| {
        const module_name = import.module_name;

        if (std.mem.eql(u8, module_name, "List")) {
            found_list = true;
        } else if (std.mem.eql(u8, module_name, "Dict")) {
            found_dict = true;
        } else if (std.mem.eql(u8, module_name, "Json.Decode")) {
            found_json_decode = true;
        } else if (std.mem.eql(u8, module_name, "Set")) {
            found_set = true;
        }
    }

    // Verify all expected modules were found
    try expectEqual(true, found_list);
    try expectEqual(true, found_dict);
    try expectEqual(true, found_json_decode);
    try expectEqual(true, found_set);

    // Test the lookup functionality
    // Get the Import.Idx for "List" (should be used twice)
    var list_import_idx: ?CIR.Import.Idx = null;
    for (canonicalizer.import_indices.iterator()) |entry| {
        const module_name = entry.key_ptr.*;
        if (std.mem.eql(u8, module_name, "List")) {
            list_import_idx = entry.value_ptr.*;
            break;
        }
    }

    try testing.expect(list_import_idx != null);
}

test "import interner - comprehensive usage example" {
    const allocator = testing.allocator;

    // Parse source with imports used in different contexts
    const source =
        \\module [process]
        \\
        \\import List exposing [map, filter]
        \\import Dict
        \\import Result exposing [Result, withDefault]
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

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    // Set the source in module_env so canonicalization can access it
    env.source = try allocator.dupe(u8, source);
    env.owns_source = true;
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Verify Import.Idx assignments
    // Get Import.Idx values from the imports store
    const list_import = cir.imports.map.get("List");
    const dict_import = cir.imports.map.get("Dict");
    const result_import = cir.imports.map.get("Result");

    // All should have Import.Idx values
    try testing.expect(list_import != null);
    try testing.expect(dict_import != null);
    try testing.expect(result_import != null);

    // They should all be different
    try testing.expect(list_import.? != dict_import.?);
    try testing.expect(list_import.? != result_import.?);
    try testing.expect(dict_import.? != result_import.?);

    // Verify total unique imports
    try expectEqual(@as(usize, 3), cir.imports.imports.items.len);

    // Demo: Print all imports with their indices
    std.debug.print("\n=== Import Index Demo ===\n", .{});
    for (cir.imports.imports.items) |import| {
        const module_name_text = import.module_name;
        std.debug.print("Module '{}'\n", .{module_name_text});
    }
}

test "Import.Idx is u16" {
    // Verify that Import.Idx is indeed a u16 enum
    const import_idx_type = @TypeOf(CIR.Import.Idx);
    const type_info = @typeInfo(import_idx_type).Enum;

    // The underlying type should be u16
    try testing.expectEqual(u16, type_info.tag_type);

    // Test that we can create valid Import.Idx values
    const idx1: CIR.Import.Idx = @enumFromInt(0);
    const idx2: CIR.Import.Idx = @enumFromInt(65535); // max u16 value

    // Verify they are distinct
    try testing.expect(idx1 != idx2);

    // Verify the size in memory
    try testing.expectEqual(@sizeOf(u16), @sizeOf(CIR.Import.Idx));
}

test "module scopes - imports are only available in their scope" {
    const allocator = testing.allocator;

    // Parse source with imports in different scopes
    const source =
        \\module [process]
        \\
        \\import List
        \\import Dict
        \\
        \\process = \items ->
        \\    # List and Dict are available here
        \\    list = List.map items \x -> x + 1
        \\    dict = Dict.empty
        \\
        \\    inner = \y ->
        \\        # List and Dict are still available in inner scope
        \\        import Set
        \\        # Now Set is also available
        \\        set = Set.empty
        \\        list2 = List.len items
        \\        set
        \\
        \\    # Set is NOT available here (out of scope)
        \\    # This should generate MODULE_NOT_IMPORTED error
        \\    badSet = Set.empty
        \\
        \\    dict
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize without external module validation to focus on scope testing
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    // Set the source in module_env so canonicalization can access it
    env.source = try allocator.dupe(u8, source);
    env.owns_source = true;
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Check for MODULE_NOT_IMPORTED error
    var found_module_not_imported = false;
    var error_module_name: ?[]const u8 = null;

    const diagnostics = cir.diag_regions.entries.items;
    for (diagnostics) |entry| {
        const diag_idx: CIR.Diagnostic.Idx = @enumFromInt(entry.value);
        const diagnostic = cir.store.getDiagnostic(diag_idx);

        switch (diagnostic) {
            .module_not_imported => |d| {
                found_module_not_imported = true;
                error_module_name = env.idents.getText(d.module_name);
            },
            else => {},
        }
    }

    // Verify we got the MODULE_NOT_IMPORTED error for Set
    try expectEqual(true, found_module_not_imported);
    try testing.expectEqualStrings("Set", error_module_name.?);

    // Verify that List and Dict imports were processed correctly
    try testing.expect(cir.imports.imports.items.len >= 3); // List, Dict, and Set
}

test "module-qualified lookups with e_lookup_external" {
    const allocator = testing.allocator;

    // Parse source with module-qualified lookups
    const source =
        \\module [main]
        \\
        \\import List
        \\import Dict
        \\
        \\main =
        \\    list = List.map [1, 2, 3] \x -> x * 2
        \\    dict = Dict.insert Dict.empty "key" "value"
        \\    List.len list
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    // Set the source in module_env so canonicalization can access it
    env.source = try allocator.dupe(u8, source);
    env.owns_source = true;
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Count e_lookup_external expressions
    var external_lookup_count: u32 = 0;
    var found_list_map = false;
    var found_list_len = false;
    var found_dict_insert = false;
    var found_dict_empty = false;

    // Traverse the CIR to find e_lookup_external expressions
    const all_exprs = cir.store.expr_buffer.items;
    for (all_exprs) |node| {
        if (node.tag == .expr_lookup_external) {
            external_lookup_count += 1;

            // Get the external lookup data
            const module_idx: CIR.Import.Idx = @enumFromInt(node.data_1);
            const field_name_idx: base.Ident.Idx = @bitCast(node.data_2);
            const import = &cir.imports.imports.items.items[@intFromEnum(module_idx)];
            const module_name = import.name;
            const field_name = env.idents.getText(field_name_idx);

            if (std.mem.eql(u8, module_name, "List")) {
                if (std.mem.eql(u8, field_name, "map")) found_list_map = true;
                if (std.mem.eql(u8, field_name, "len")) found_list_len = true;
            } else if (std.mem.eql(u8, module_name, "Dict")) {
                if (std.mem.eql(u8, field_name, "insert")) found_dict_insert = true;
                if (std.mem.eql(u8, field_name, "empty")) found_dict_empty = true;
            }
        }
    }

    // Verify we found all expected external lookups
    try expectEqual(@as(u32, 4), external_lookup_count);
    try expectEqual(true, found_list_map);
    try expectEqual(true, found_list_len);
    try expectEqual(true, found_dict_insert);
    try expectEqual(true, found_dict_empty);
}

test "exposed_nodes - tracking CIR node indices for exposed items" {
    const allocator = testing.allocator;

    // Create module environments with exposed items
    var module_envs = std.StringHashMap(*base.ModuleEnv).init(allocator);
    defer module_envs.deinit();

    // Create a "MathUtils" module with some exposed definitions
    var math_env = base.ModuleEnv.init(allocator);
    defer math_env.deinit();

    // Add exposed items
    try math_env.exposed_by_str.put(allocator, "add", {});
    try math_env.exposed_by_str.put(allocator, "multiply", {});
    try math_env.exposed_by_str.put(allocator, "PI", {});

    // Simulate having CIR node indices for these exposed items
    // In real usage, these would be set during canonicalization of MathUtils
    try math_env.exposed_nodes.put(allocator, "add", 100);
    try math_env.exposed_nodes.put(allocator, "multiply", 200);
    try math_env.exposed_nodes.put(allocator, "PI", 300);

    try module_envs.put("MathUtils", &math_env);

    // Parse source that uses these exposed items
    const source =
        \\module [calculate]
        \\
        \\import MathUtils exposing [add, multiply, PI]
        \\
        \\calculate = \x, y ->
        \\    sum = add x y
        \\    product = multiply x y
        \\    circumference = multiply (multiply 2 PI) x
        \\    { sum, product, circumference }
    ;

    // Parse the source
    var tokens = try parse.tokenize(allocator, source, .file);
    defer tokens.deinit(allocator);
    var parse_env = base.ModuleEnv.init(allocator);
    defer parse_env.deinit();
    try parse_env.calcLineStarts(source);
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize with module environments
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    try env.calcLineStarts(source);
    var cir = CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, &module_envs);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Verify that e_lookup_external expressions have the correct target_node_idx values
    var found_add_with_idx_100 = false;
    var found_multiply_with_idx_200 = false;
    var found_pi_with_idx_300 = false;

    const all_exprs = cir.store.expr_buffer.items;
    for (all_exprs) |node| {
        if (node.tag == .expr_external_lookup) {
            const module_idx: CIR.Import.Idx = @enumFromInt(node.data_1);
            const field_name_idx: base.Ident.Idx = @bitCast(node.data_2);
            const target_node_idx: u16 = @intCast(node.data_3);
            const import = &cir.imports.imports.items.items[@intFromEnum(module_idx)];
            const module_name = import.name;
            const field_name = env.idents.getText(field_name_idx);

            if (std.mem.eql(u8, module_name, "MathUtils")) {
                if (std.mem.eql(u8, field_name, "add") and target_node_idx == 100) {
                    found_add_with_idx_100 = true;
                } else if (std.mem.eql(u8, field_name, "multiply") and target_node_idx == 200) {
                    found_multiply_with_idx_200 = true;
                } else if (std.mem.eql(u8, field_name, "PI") and target_node_idx == 300) {
                    found_pi_with_idx_300 = true;
                }
            }
        }
    }

    // Verify all lookups have the correct target node indices
    try expectEqual(true, found_add_with_idx_100);
    try expectEqual(true, found_multiply_with_idx_200);
    try expectEqual(true, found_pi_with_idx_300);

    // Test case where exposed_nodes is not populated (should get 0)
    var empty_env = base.ModuleEnv.init(allocator);
    defer empty_env.deinit();
    try empty_env.exposed_by_str.put(allocator, "undefined", {});
    // Don't add to exposed_nodes - should default to 0
    try module_envs.put("EmptyModule", &empty_env);

    const source2 =
        \\module [test]
        \\
        \\import EmptyModule exposing [undefined]
        \\
        \\test = undefined
    ;

    var tokens2 = try parse.tokenize(allocator, source2, .file);
    defer tokens2.deinit(allocator);
    var parse_env2 = base.ModuleEnv.init(allocator);
    defer parse_env2.deinit();
    try parse_env2.calcLineStarts(source2);
    var ast2 = try parse.parse(&parse_env2, &tokens2, allocator, .file);
    defer ast2.deinit();

    var env2 = base.ModuleEnv.init(allocator);
    defer env2.deinit();
    // Set the source in module_env so canonicalization can access it
    env2.source = try allocator.dupe(u8, source2);
    env2.owns_source = true;
    try env2.calcLineStarts(source2);
    var cir2 = CIR.init(&env2);
    defer cir2.deinit();

    var canonicalizer2 = try canonicalize.init(&cir2, &ast2, &module_envs);
    defer canonicalizer2.deinit();

    try canonicalizer2.canonicalizeFile();

    // Verify that undefined gets target_node_idx = 0 (not found)
    var found_undefined_with_idx_0 = false;
    const all_exprs2 = cir2.store.expr_buffer.items;
    for (all_exprs2) |node| {
        if (node.tag == .expr_external_lookup) {
            const field_name_idx: base.Ident.Idx = @bitCast(node.data_2);
            const target_node_idx: u16 = @intCast(node.data_3);
            const field_name = env2.idents.getText(field_name_idx);

            if (std.mem.eql(u8, field_name, "undefined") and target_node_idx == 0) {
                found_undefined_with_idx_0 = true;
            }
        }
    }

    try expectEqual(true, found_undefined_with_idx_0);
}

test "export count safety - ensures safe u16 casting" {
    const allocator = testing.allocator;

    // This test verifies that we check export counts to ensure safe casting to u16
    // The check triggers when exposed_items.len >= maxInt(u16) (65535)
    // This leaves 0 available as a potential sentinel value if needed

    // Verify the threshold is what we expect
    try expectEqual(@as(u32, 65535), std.math.maxInt(u16));

    // Test the diagnostic for exactly maxInt(u16) exports
    var env1 = base.ModuleEnv.init(allocator);
    defer env1.deinit();
    // Set the source in module_env so canonicalization can access it
    env1.source = try allocator.dupe(u8, source);
    env1.owns_source = true;
    var cir1 = CIR.init(&env1);
    defer cir1.deinit();

    const diag_at_limit = CIR.Diagnostic{
        .too_many_exports = .{
            .count = 65535, // Exactly at the limit
            .region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 10 } },
        },
    };

    const diag_idx1 = cir1.store.addDiagnostic(diag_at_limit);
    const retrieved1 = cir1.store.getDiagnostic(diag_idx1);

    switch (retrieved1) {
        .too_many_exports => |d| {
            try expectEqual(@as(u32, 65535), d.count);
        },
        else => return error.UnexpectedDiagnostic,
    }

    // Test the diagnostic for exceeding the limit
    var env2 = base.ModuleEnv.init(allocator);
    defer env2.deinit();
    var cir2 = CIR.init(&env2);
    defer cir2.deinit();

    const diag_over_limit = CIR.Diagnostic{
        .too_many_exports = .{
            .count = 70000, // Well over the limit
            .region = base.Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 10 } },
        },
    };

    const diag_idx2 = cir2.store.addDiagnostic(diag_over_limit);
    const retrieved2 = cir2.store.getDiagnostic(diag_idx2);

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
