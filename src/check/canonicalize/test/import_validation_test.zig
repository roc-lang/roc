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
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize with module validation
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
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
    var ast = try parse.parse(&parse_env, &tokens, allocator, .file);
    defer ast.deinit();

    // Canonicalize without module validation (pass null)
    var env = base.ModuleEnv.init(allocator);
    defer env.deinit();
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
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
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
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

    for (cir.imports.imports.items, 0..) |import, idx| {
        const import_idx: CIR.Import.Idx = @enumFromInt(idx);
        const module_name = import.module_name;

        // Verify we can look up the module name from Import.Idx
        const retrieved_name = cir.imports.getModuleName(import_idx);
        try testing.expectEqualStrings(module_name, retrieved_name);

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

    // Verify we can retrieve the correct module name from the Import.Idx
    const retrieved_list_name = cir.imports.getModuleName(list_import_idx.?);
    try testing.expectEqualStrings("List", retrieved_list_name);
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
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Verify Import.Idx assignments
    // Get Import.Idx values
    const list_import = canonicalizer.getImportIdx("List");
    const dict_import = canonicalizer.getImportIdx("Dict");
    const result_import = canonicalizer.getImportIdx("Result");

    // All should have Import.Idx values
    try testing.expect(list_import != null);
    try testing.expect(dict_import != null);
    try testing.expect(result_import != null);

    // They should all be different
    try testing.expect(list_import.? != dict_import.?);
    try testing.expect(list_import.? != result_import.?);
    try testing.expect(dict_import.? != result_import.?);

    // Verify we can look up module names from Import.Idx
    const list_name = cir.imports.getModuleName(list_import.?);
    const dict_name = cir.imports.getModuleName(dict_import.?);
    const result_name = cir.imports.getModuleName(result_import.?);

    try testing.expectEqualStrings("List", list_name);
    try testing.expectEqualStrings("Dict", dict_name);
    try testing.expectEqualStrings("Result", result_name);

    // Verify total unique imports
    try expectEqual(@as(usize, 3), cir.imports.imports.items.len);

    // Demo: Print all imports with their indices
    std.debug.print("\n=== Import Index Demo ===\n", .{});
    for (cir.imports.imports.items, 0..) |import, idx| {
        const import_idx: CIR.Import.Idx = @enumFromInt(idx);
        const module_name_text = import.module_name;
        std.debug.print("Import.Idx {} -> module '{}'\n", .{ @intFromEnum(import_idx), module_name_text });
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
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
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
    try env.calcLineStarts(source);
    var cir = CIR.init(&env);
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

            const module_name = cir.imports.getModuleName(module_idx);
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
