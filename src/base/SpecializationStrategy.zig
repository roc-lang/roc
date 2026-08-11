//! Runtime checked-to-LIR lowering strategy.

const std = @import("std");

/// Selects how runtime roots reach LIR after checking.
pub const SpecializationStrategy = enum {
    /// Lambda-set specialization: specialize polymorphism and callable flow
    /// before producing LIR.
    lss,
    /// Box closures and type-variable values, pass descriptor/dictionary data
    /// explicitly, and lower checked artifacts directly to LIR.
    boxy,

    pub fn fromCliValue(value: []const u8) ?SpecializationStrategy {
        if (std.mem.eql(u8, value, "yes")) return .lss;
        if (std.mem.eql(u8, value, "no")) return .boxy;
        return null;
    }

    pub fn cliOptions() []const u8 {
        return "yes,no";
    }
};
