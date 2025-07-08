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
