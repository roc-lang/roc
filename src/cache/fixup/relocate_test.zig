//! Test file to verify that all relocate methods compile correctly
//! This is a compilation test only - not meant to be run

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types.zig");
const CIR = @import("../../check/canonicalize.zig").CIR;

test "ModuleEnv relocate method compiles" {
    const allocator = std.testing.allocator;

    var env = try base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    // This should compile - we're just checking the method exists
    env.relocate(0);
}

test "CIR relocate method compiles" {
    const allocator = std.testing.allocator;

    var env = try base.ModuleEnv.init(allocator, "test");
    defer env.deinit();

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    // This should compile - we're just checking the method exists
    cir.relocate(0);
}

test "Collection relocate methods compile" {
    const allocator = std.testing.allocator;

    // SafeList
    var list = collections.SafeList(u32){};
    defer list.deinit(allocator);
    list.relocate(0);

    // SmallStringInterner
    var interner = try collections.SmallStringInterner.initCapacity(allocator, 10);
    defer interner.deinit(allocator);
    interner.relocate(0);

    // SafeStringHashMap
    var map = collections.SafeStringHashMap(u16){};
    defer map.deinit(allocator);
    map.relocate(0);
}

test "types.Store relocate method compiles" {
    const allocator = std.testing.allocator;

    var store = try types.Store.init(allocator);
    defer store.deinit();

    // This should compile - we're just checking the method exists
    store.relocate(0);
}

test "All major structures have relocate methods" {
    // This test just ensures we haven't forgotten any major structures
    // If this compiles, it means all the relocate methods are defined

    comptime {
        // Verify these types have relocate methods
        _ = base.ModuleEnv.relocate;
        _ = CIR.relocate;
        _ = collections.SafeList(u32).relocate;
        _ = collections.SmallStringInterner.relocate;
        _ = collections.SafeStringHashMap(void).relocate;
        _ = types.Store.relocate;
        _ = base.Ident.Store.relocate;
        _ = base.StringLiteral.Store.relocate;
    }
}
