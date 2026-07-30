//! Verifies platform-specific host trampoline assembly invariants.

const std = @import("std");

test "AArch64 host trampoline stores all four homogeneous aggregate result registers" {
    const assembly_source = @embedFile("../host_trampoline.S");
    try std.testing.expect(std.mem.find(u8, assembly_source, "stp q2, q3, [x9, #32]") != null);
}
