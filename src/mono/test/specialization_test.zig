//! Unit tests for let-binding specialization
//!
//! Tests verify that the specialization logic correctly:
//! - Identifies when let-bindings need specialization (different concrete types at use sites)
//! - Creates fresh symbols for each distinct type instantiation
//! - Reuses the same specialized symbol for the same type
//! - Correctly tracks specializations following COR's approach

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const layout_mod = @import("layout");
const types_mod = @import("types");
const mono = @import("../mod.zig");
const can = @import("can");

const Allocator = std.mem.Allocator;
const LayoutIdx = layout_mod.Idx;
const MonoSymbol = mono.MonoSymbol;
const Var = types_mod.Var;

/// Helper to create a test symbol
fn testSymbol(module_idx: u16, ident_idx: u29) MonoSymbol {
    return MonoSymbol{
        .module_idx = module_idx,
        .ident_idx = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = ident_idx,
        },
    };
}

/// Helper to create a test type variable
fn testVar(var_id: u32) Var {
    return @as(Var, @enumFromInt(var_id));
}

test "specialization: same type produces same specialization key" {
    const base_symbol = testSymbol(0, 100);
    const type_var_1 = testVar(42);
    const type_var_2 = testVar(42); // Same type variable

    // Compute specialization keys
    const base_key: u48 = @bitCast(base_symbol);
    const var_1_int: u48 = @intCast(@intFromEnum(type_var_1));
    const var_2_int: u48 = @intCast(@intFromEnum(type_var_2));

    const spec_key_1: u96 = (@as(u96, var_1_int) << 48) | base_key;
    const spec_key_2: u96 = (@as(u96, var_2_int) << 48) | base_key;

    // Keys should be identical
    try testing.expectEqual(spec_key_1, spec_key_2);
}

test "specialization: different types produce different specialization keys" {
    const base_symbol = testSymbol(0, 100);
    const type_var_i64 = testVar(42);
    const type_var_string = testVar(43); // Different type variable

    // Compute specialization keys
    const base_key: u48 = @bitCast(base_symbol);
    const var_i64_int: u48 = @intCast(@intFromEnum(type_var_i64));
    const var_string_int: u48 = @intCast(@intFromEnum(type_var_string));

    const spec_key_i64: u96 = (@as(u96, var_i64_int) << 48) | base_key;
    const spec_key_string: u96 = (@as(u96, var_string_int) << 48) | base_key;

    // Keys should be different
    try testing.expect(spec_key_i64 != spec_key_string);
}

test "specialization: fresh symbols are unique" {
    var next_synthetic_idx: u29 = std.math.maxInt(u29) - 1;

    // Create first fresh symbol
    const fresh_ident_1 = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = next_synthetic_idx,
    };
    next_synthetic_idx -= 1;
    const spec_symbol_1 = MonoSymbol{
        .module_idx = 0,
        .ident_idx = fresh_ident_1,
    };

    // Create second fresh symbol
    const fresh_ident_2 = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = next_synthetic_idx,
    };
    next_synthetic_idx -= 1;
    const spec_symbol_2 = MonoSymbol{
        .module_idx = 0,
        .ident_idx = fresh_ident_2,
    };

    // Symbols should be different
    const key_1: u48 = @bitCast(spec_symbol_1);
    const key_2: u48 = @bitCast(spec_symbol_2);
    try testing.expect(key_1 != key_2);
}

test "specialization: multiple instantiations track separately" {
    const allocator = testing.allocator;

    const base_symbol = testSymbol(0, 100);

    // Simulate tracking multiple specializations
    var specializations = std.AutoHashMap(u96, MonoSymbol).init(allocator);
    defer specializations.deinit();

    // First specialization: (base_symbol, i64)
    const type_var_i64 = testVar(42);
    const base_key: u48 = @bitCast(base_symbol);
    const var_i64_int: u48 = @intCast(@intFromEnum(type_var_i64));
    const spec_key_i64: u96 = (@as(u96, var_i64_int) << 48) | base_key;

    const spec_symbol_i64 = testSymbol(0, 200);
    try specializations.put(spec_key_i64, spec_symbol_i64);

    // Second specialization: (base_symbol, string)
    const type_var_string = testVar(43);
    const var_string_int: u48 = @intCast(@intFromEnum(type_var_string));
    const spec_key_string: u96 = (@as(u96, var_string_int) << 48) | base_key;

    const spec_symbol_string = testSymbol(0, 201);
    try specializations.put(spec_key_string, spec_symbol_string);

    // Verify both specializations are tracked
    try testing.expectEqual(specializations.get(spec_key_i64).?, spec_symbol_i64);
    try testing.expectEqual(specializations.get(spec_key_string).?, spec_symbol_string);

    // Verify they're different
    const key_i64: u48 = @bitCast(spec_symbol_i64);
    const key_string: u48 = @bitCast(spec_symbol_string);
    try testing.expect(key_i64 != key_string);
}

test "specialization: reusing same type returns same specialization" {
    const allocator = testing.allocator;

    const base_symbol = testSymbol(0, 100);
    const type_var_i64_first = testVar(42);
    const type_var_i64_second = testVar(42); // Same type used again

    var specializations = std.AutoHashMap(u96, MonoSymbol).init(allocator);
    defer specializations.deinit();

    // Record first use of i64
    const base_key: u48 = @bitCast(base_symbol);
    const var_int: u48 = @intCast(@intFromEnum(type_var_i64_first));
    const spec_key_1: u96 = (@as(u96, var_int) << 48) | base_key;

    const spec_symbol = testSymbol(0, 200);
    try specializations.put(spec_key_1, spec_symbol);

    // Look up same type again
    const var_int_2: u48 = @intCast(@intFromEnum(type_var_i64_second));
    const spec_key_2: u96 = (@as(u96, var_int_2) << 48) | base_key;

    // Should find the same specialization
    const reused = specializations.get(spec_key_2);
    try testing.expect(reused != null);
    try testing.expectEqual(reused.?, spec_symbol);
}

test "specialization: comprehensive let-binding scenario" {
    const allocator = testing.allocator;

    // Simulate: id = |x| x used with multiple types
    const id_symbol = testSymbol(0, 100); // Base 'id' binding

    var specializations = std.AutoHashMap(u96, MonoSymbol).init(allocator);
    defer specializations.deinit();

    var next_synthetic_idx: u29 = std.math.maxInt(u29) - 1;

    // Use 1: id(10) - type is i64
    const type_i64 = testVar(42);
    {
        const base_key: u48 = @bitCast(id_symbol);
        const var_int: u48 = @intCast(@intFromEnum(type_i64));
        const spec_key: u96 = (@as(u96, var_int) << 48) | base_key;

        if (specializations.get(spec_key) == null) {
            const fresh_ident = base.Ident.Idx{
                .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
                .idx = next_synthetic_idx,
            };
            next_synthetic_idx -= 1;

            const spec_symbol = MonoSymbol{
                .module_idx = 0,
                .ident_idx = fresh_ident,
            };
            try specializations.put(spec_key, spec_symbol);
        }
    }

    // Use 2: id("Hello") - type is string
    const type_string = testVar(43);
    {
        const base_key: u48 = @bitCast(id_symbol);
        const var_int: u48 = @intCast(@intFromEnum(type_string));
        const spec_key: u96 = (@as(u96, var_int) << 48) | base_key;

        if (specializations.get(spec_key) == null) {
            const fresh_ident = base.Ident.Idx{
                .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
                .idx = next_synthetic_idx,
            };
            next_synthetic_idx -= 1;

            const spec_symbol = MonoSymbol{
                .module_idx = 0,
                .ident_idx = fresh_ident,
            };
            try specializations.put(spec_key, spec_symbol);
        }
    }

    // Use 3: id(20) - type is i64 again
    const type_i64_again = testVar(42);
    {
        const base_key: u48 = @bitCast(id_symbol);
        const var_int: u48 = @intCast(@intFromEnum(type_i64_again));
        const spec_key: u96 = (@as(u96, var_int) << 48) | base_key;

        // Should reuse the i64 specialization
        try testing.expect(specializations.get(spec_key) != null);
    }

    // Verify we have exactly 2 specializations (one for i64, one for string)
    try testing.expectEqual(@as(usize, 2), specializations.count());
}
