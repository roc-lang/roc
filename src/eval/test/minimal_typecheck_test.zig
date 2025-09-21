const std = @import("std");
const testing = std.testing;
const ModuleEnv = @import("can").ModuleEnv;
const Can = @import("can").Can;
const Check = @import("check").Check;
const parse = @import("parse");

test "minimal type-checking bug reproduction" {
    const allocator = testing.allocator;

    // Math module with a method
    const math_source =
        \\module [double]
        \\
        \\double = \obj ->
        \\    obj.value * 2
    ;

    // Main module that uses static dispatch
    const main_source =
        \\module []
        \\
        \\import Math exposing [double]
        \\
        \\obj = { value: 7 }
        \\
        \\main = obj.double {}
    ;  // Note: Roc uses `obj.double {}` not `obj.double()`

    // Set up Math module
    var math_env = try ModuleEnv.init(allocator, math_source);
    defer math_env.deinit();

    math_env.module_name = "Math";
    math_env.common.source = math_source;
    try math_env.common.calcLineStarts(allocator);

    var math_parse = try parse.parse(&math_env.common, allocator);
    defer math_parse.deinit(allocator);

    try math_env.initCIRFields(allocator, "Math");

    var math_czer = try Can.init(&math_env, &math_parse, null);
    defer math_czer.deinit();
    try math_czer.canonicalizeFile();

    // Type check Math module
    var math_checker = try Check.init(allocator, &math_env.types, &math_env, &.{}, &math_env.store.regions);
    defer math_checker.deinit();
    try math_checker.checkDefs();

    std.debug.print("\nMath module type-checked successfully\n", .{});

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    // Set up imports
    try main_env.imports.append("Math", &main_env.common.strings);

    var main_czer = try Can.init(&main_env, &main_parse, &math_env);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    std.debug.print("Main module canonicalized successfully\n", .{});

    // Type check Main module - THIS IS WHERE IT FAILS
    const other_modules = [_]*ModuleEnv{&math_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();

    std.debug.print("About to type-check Main module...\n", .{});
    try main_checker.checkDefs();

    std.debug.print("Main module type-checked successfully!\n", .{});
}