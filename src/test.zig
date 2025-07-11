const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("builtins/main.zig"));
    testing.refAllDeclsRecursive(@import("cache/mod.zig"));
    testing.refAllDeclsRecursive(@import("cache/CacheModule.zig"));
    testing.refAllDeclsRecursive(@import("serialization/mod.zig"));

    // TODO: Remove after hooking up
    testing.refAllDeclsRecursive(@import("reporting.zig"));
    testing.refAllDeclsRecursive(@import("reporting/test.zig"));
    testing.refAllDeclsRecursive(@import("eval/stack.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/unify.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/let_polymorphism_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test/static_dispatch_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test/nominal_type_origin_test.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
    testing.refAllDeclsRecursive(@import("layout/layout.zig"));
    testing.refAllDeclsRecursive(@import("layout/store.zig"));
    testing.refAllDeclsRecursive(@import("layout/store_test.zig"));
    testing.refAllDeclsRecursive(@import("types/test_rigid_instantiation.zig"));
    testing.refAllDeclsRecursive(@import("snapshot_expected_test.zig"));
}
