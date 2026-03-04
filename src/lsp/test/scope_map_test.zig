//! Tests for scope reconstruction used in LSP code completion.
//!
//! These tests verify that the ScopeMap correctly:
//! - Initializes and deinitializes without leaking memory
//! - Tracks binding visibility ranges correctly
//! - Reports isVisibleAt correctly for various offsets

const std = @import("std");
const scope_map = @import("../scope_map.zig");
const base = @import("base");

const ScopeMap = scope_map.ScopeMap;
const Binding = scope_map.Binding;
const Ident = base.Ident;

/// Helper to create an Ident.Idx for testing
fn testIdent(idx: u29) Ident.Idx {
    return .{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = idx,
    };
}

// Unit tests for Binding and ScopeMap data structures

test "ScopeMap init and deinit" {
    const allocator = std.testing.allocator;
    var sm = ScopeMap.init(allocator);
    defer sm.deinit();

    // Empty scope map should have no bindings
    try std.testing.expectEqual(@as(usize, 0), sm.bindings.items.len);
}

test "ScopeMap.isVisibleAt returns true for offset within range" {
    const binding = Binding{
        .ident = testIdent(0),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 10,
        .visible_to = 50,
        .is_parameter = false,
    };

    // Inside the range
    try std.testing.expect(ScopeMap.isVisibleAt(binding, 10)); // Start (inclusive)
    try std.testing.expect(ScopeMap.isVisibleAt(binding, 25)); // Middle
    try std.testing.expect(ScopeMap.isVisibleAt(binding, 49)); // Just before end
}

test "ScopeMap.isVisibleAt returns false for offset outside range" {
    const binding = Binding{
        .ident = testIdent(0),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 10,
        .visible_to = 50,
        .is_parameter = false,
    };

    // Outside the range
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 0)); // Before start
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 9)); // Just before start
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 50)); // At end (exclusive)
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 100)); // After end
}

test "ScopeMap.isVisibleAt handles zero-width binding" {
    const binding = Binding{
        .ident = testIdent(0),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 10,
        .visible_to = 10, // Zero width
        .is_parameter = false,
    };

    // Zero-width binding should not be visible at any offset
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 9));
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 10));
    try std.testing.expect(!ScopeMap.isVisibleAt(binding, 11));
}

test "ScopeMap.isVisibleAt handles parameter flag correctly" {
    const param_binding = Binding{
        .ident = testIdent(0),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 0,
        .visible_to = 100,
        .is_parameter = true,
    };

    const local_binding = Binding{
        .ident = testIdent(1),
        .pattern_idx = @enumFromInt(1),
        .visible_from = 0,
        .visible_to = 100,
        .is_parameter = false,
    };

    // Both should be visible - is_parameter doesn't affect visibility
    try std.testing.expect(ScopeMap.isVisibleAt(param_binding, 50));
    try std.testing.expect(ScopeMap.isVisibleAt(local_binding, 50));
    try std.testing.expect(param_binding.is_parameter);
    try std.testing.expect(!local_binding.is_parameter);
}

test "ScopeMap.isVisibleAt handles max offset" {
    const binding = Binding{
        .ident = testIdent(0),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 0,
        .visible_to = std.math.maxInt(u32),
        .is_parameter = false,
    };

    // Should be visible at large offsets
    try std.testing.expect(ScopeMap.isVisibleAt(binding, 0));
    try std.testing.expect(ScopeMap.isVisibleAt(binding, 1000000));
    try std.testing.expect(ScopeMap.isVisibleAt(binding, std.math.maxInt(u32) - 1));
}

test "ScopeMap with no bindings has empty items" {
    const allocator = std.testing.allocator;
    var sm = ScopeMap.init(allocator);
    defer sm.deinit();

    try std.testing.expectEqual(@as(usize, 0), sm.bindings.items.len);
}

test "ScopeMap manually added bindings are queryable" {
    const allocator = std.testing.allocator;
    var sm = ScopeMap.init(allocator);
    defer sm.deinit();

    // Manually add bindings (simulating what build() would do)
    try sm.bindings.append(allocator, .{
        .ident = testIdent(1),
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 0,
        .visible_to = 100,
        .is_parameter = false,
    });

    try sm.bindings.append(allocator, .{
        .ident = testIdent(2),
        .pattern_idx = @enumFromInt(1),
        .visible_from = 50,
        .visible_to = 100,
        .is_parameter = false,
    });

    // At offset 25, only first binding should be visible
    var visible_count: usize = 0;
    for (sm.bindings.items) |binding| {
        if (ScopeMap.isVisibleAt(binding, 25)) {
            visible_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), visible_count);

    // At offset 75, both bindings should be visible
    visible_count = 0;
    for (sm.bindings.items) |binding| {
        if (ScopeMap.isVisibleAt(binding, 75)) {
            visible_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), visible_count);

    // At offset 150, no bindings should be visible
    visible_count = 0;
    for (sm.bindings.items) |binding| {
        if (ScopeMap.isVisibleAt(binding, 150)) {
            visible_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), visible_count);
}

test "Binding struct has expected fields" {
    // This test ensures the Binding struct maintains its expected shape
    const binding = Binding{
        .ident = testIdent(42),
        .pattern_idx = @enumFromInt(7),
        .visible_from = 100,
        .visible_to = 200,
        .is_parameter = true,
    };

    try std.testing.expectEqual(@as(u29, 42), binding.ident.idx);
    try std.testing.expectEqual(@as(u32, 7), @intFromEnum(binding.pattern_idx));
    try std.testing.expectEqual(@as(u32, 100), binding.visible_from);
    try std.testing.expectEqual(@as(u32, 200), binding.visible_to);
    try std.testing.expect(binding.is_parameter);
}

test "ScopeMap bindings can track nested scopes" {
    // Test simulating nested scopes like:
    // {
    //   x = 1          // visible_from: 0, visible_to: 100
    //   {
    //     y = 2        // visible_from: 20, visible_to: 80
    //     z = 3        // visible_from: 40, visible_to: 80
    //   }
    // }
    const allocator = std.testing.allocator;
    var sm = ScopeMap.init(allocator);
    defer sm.deinit();

    // Outer scope variable
    try sm.bindings.append(allocator, .{
        .ident = testIdent(1), // x
        .pattern_idx = undefined, // not read in isVisibleAt tests
        .visible_from = 0,
        .visible_to = 100,
        .is_parameter = false,
    });

    // Inner scope variables
    try sm.bindings.append(allocator, .{
        .ident = testIdent(2), // y
        .pattern_idx = @enumFromInt(1),
        .visible_from = 20,
        .visible_to = 80,
        .is_parameter = false,
    });

    try sm.bindings.append(allocator, .{
        .ident = testIdent(3), // z
        .pattern_idx = @enumFromInt(2),
        .visible_from = 40,
        .visible_to = 80,
        .is_parameter = false,
    });

    // At offset 10: only x visible
    var count: usize = 0;
    for (sm.bindings.items) |b| {
        if (ScopeMap.isVisibleAt(b, 10)) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), count);

    // At offset 30: x and y visible
    count = 0;
    for (sm.bindings.items) |b| {
        if (ScopeMap.isVisibleAt(b, 30)) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), count);

    // At offset 50: x, y, and z visible
    count = 0;
    for (sm.bindings.items) |b| {
        if (ScopeMap.isVisibleAt(b, 50)) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);

    // At offset 90: only x visible (inner scope ended)
    count = 0;
    for (sm.bindings.items) |b| {
        if (ScopeMap.isVisibleAt(b, 90)) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}
