//! Opt-in compiler performance features and their stable cache-key bits.
//! Kept independent of build_options so the build and compiler use one registry.

const std = @import("std");

pub const Feature = enum(u3) {
    inhabitedness_memo,
    nominal_views,
    constructor_projection,
    tag_projection,
    settled_scratch,
    early_ctfe_cache,
    const_completion,

    pub fn mask(self: Feature) u8 {
        return @as(u8, 1) << @intFromEnum(self);
    }

    pub fn optionName(self: Feature) []const u8 {
        return switch (self) {
            .inhabitedness_memo => "perf-inhabitedness-memo",
            .nominal_views => "perf-nominal-views",
            .constructor_projection => "perf-constructor-projection",
            .tag_projection => "perf-tag-projection",
            .settled_scratch => "perf-settled-scratch",
            .early_ctfe_cache => "perf-early-ctfe-cache",
            .const_completion => "perf-const-completion",
        };
    }

    pub fn description(self: Feature) []const u8 {
        return switch (self) {
            .inhabitedness_memo => "Memoize inhabitedness within stable exhaustiveness analyses (default: off)",
            .nominal_views => "Use read-only substituted nominal views for analysis (default: off)",
            .constructor_projection => "Project nominal construction onto the selected payload (default: off)",
            .tag_projection => "Project expected tag payloads without copying the whole union (default: off)",
            .settled_scratch => "Isolate settled-row validation scratch from recurring checker walks (default: off)",
            .early_ctfe_cache => "Reuse eligible object packs before compile-time Monotype body lowering (default: off)",
            .const_completion => "Avoid redundant stored-constant completion-relation work (default: off)",
        };
    }
};

/// Fixed byte encoding, including a domain version, for compiler artifact keys.
pub fn cacheIdentity(flags: u8) [2]u8 {
    return .{ 1, flags };
}

test "compiler performance features have unique flags and cache identities" {
    var used: u8 = 0;
    inline for (std.meta.tags(Feature)) |feature| {
        try std.testing.expectEqual(@as(u8, 0), used & feature.mask());
        used |= feature.mask();
        try std.testing.expect(!std.mem.eql(u8, &cacheIdentity(0), &cacheIdentity(feature.mask())));
        inline for (std.meta.tags(Feature)) |other| {
            if (feature != other) try std.testing.expect(!std.mem.eql(u8, feature.optionName(), other.optionName()));
        }
    }
}
