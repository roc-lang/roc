const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("problem.zig"));
    testing.refAllDeclsRecursive(@import("types.zig"));
    testing.refAllDeclsRecursive(@import("collections.zig"));
    testing.refAllDeclsRecursive(@import("collections/utils.zig"));
    testing.refAllDeclsRecursive(@import("collections/safe_list.zig"));
    testing.refAllDeclsRecursive(@import("collections/interner/StringLiteral.zig"));
    testing.refAllDeclsRecursive(@import("base/Ident.zig"));
    testing.refAllDeclsRecursive(@import("base/Module.zig"));
    testing.refAllDeclsRecursive(@import("base/ModuleEnv.zig"));
    testing.refAllDeclsRecursive(@import("base/Package.zig"));
    testing.refAllDeclsRecursive(@import("base/Region.zig"));
    testing.refAllDeclsRecursive(@import("base.zig"));
    testing.refAllDeclsRecursive(@import("coordinate.zig"));
    testing.refAllDeclsRecursive(@import("check/parse.zig"));
    testing.refAllDeclsRecursive(@import("check/resolve_imports.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types.zig"));
    testing.refAllDeclsRecursive(@import("check/canonicalize.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test.zig"));
    testing.refAllDeclsRecursive(@import("build/specialize_types.zig"));
    testing.refAllDeclsRecursive(@import("build/lift_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/specialize_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/solve_functions.zig"));
    testing.refAllDeclsRecursive(@import("build/lower_statements.zig"));
    testing.refAllDeclsRecursive(@import("build/reference_count.zig"));
}
