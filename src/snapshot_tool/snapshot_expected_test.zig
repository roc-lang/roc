//! Tests for validating that EXPECTED sections in snapshots match their PROBLEMS sections.
//! This ensures that the expected problems documented in each snapshot file accurately
//! reflect the actual problems reported by the compiler.

const std = @import("std");
const testing = std.testing;
const snapshot_mod = @import("main.zig");

test "snapshot validation" {
    const allocator = testing.allocator;
    if (!try snapshot_mod.checkSnapshotExpectations(allocator)) {
        return error.SnapshotValidationFailed;
    }
}
