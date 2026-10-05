//! Opt-in compiler performance features and their stable cache-key bits.
//! Kept independent of build_options so the build and compiler use one registry.

const std = @import("std");

pub const Mask = u16;

pub const Feature = enum(u4) {
    inhabitedness_memo,
    nominal_views,
    constructor_projection,
    tag_projection,
    settled_scratch,
    early_ctfe_cache,
    const_completion,
    lazy_nominal_rows,
    finalized_literal_cache,
    late_callable_cache,

    pub fn mask(self: Feature) Mask {
        return @as(Mask, 1) << @intFromEnum(self);
    }

    /// Keep the measured bundle stable while follow-up experiments are opt-in.
    pub fn enabledByAll(self: Feature) bool {
        return switch (self) {
            .lazy_nominal_rows, .finalized_literal_cache, .late_callable_cache => false,
            .inhabitedness_memo, .nominal_views, .constructor_projection, .tag_projection, .settled_scratch, .early_ctfe_cache, .const_completion => true,
        };
    }

    pub fn resolve(self: Feature, all: bool, explicit: ?bool) bool {
        return explicit orelse (all and self.enabledByAll());
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
            .lazy_nominal_rows => "perf-lazy-nominal-rows",
            .finalized_literal_cache => "perf-finalized-literal-cache",
            .late_callable_cache => "perf-late-callable-cache",
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
            .lazy_nominal_rows => "Experiment with delayed nominal row instantiation (default: off, independent of perf-all)",
            .finalized_literal_cache => "Experiment with caching finalized literal specializations (default: off, independent of perf-all)",
            .late_callable_cache => "Experiment with late higher-order object lookup (default: off, independent of perf-all)",
        };
    }
};

/// Fixed byte encoding, including a domain version, for compiler artifact keys.
pub fn cacheIdentity(flags: Mask) [3]u8 {
    // Literal-on artifacts require the strengthened noncallable certificate
    // contract. Gate-off and the original seven-feature bundle retain their
    // measured namespace; old literal-on paths cannot block new publication.
    const version: u8 = if (flags & Feature.finalized_literal_cache.mask() != 0) 3 else 2;
    return .{ version, @truncate(flags), @truncate(flags >> 8) };
}

/// Semantic compiler identity from its declared provenance, feature contract,
/// and builtin source. Filesystem acquisition belongs to the build driver.
pub fn compilerArtifactHash(compiler_version: []const u8, performance_identity: [3]u8, builtin_source: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-checked-artifact-v1");
    hasher.update(compiler_version);
    hasher.update("compiler-performance-features");
    hasher.update(&performance_identity);
    hasher.update(builtin_source);
    return hasher.finalResult();
}

test "finalized literal compiler contract preserves legacy hash vectors and separates new proof" {
    const legacy = compilerArtifactHash("fixture-version", .{ 2, 0, 1 }, "fixture-builtins");
    const current = compilerArtifactHash("fixture-version", cacheIdentity(Feature.finalized_literal_cache.mask()), "fixture-builtins");
    const bundle = compilerArtifactHash("fixture-version", cacheIdentity(127), "fixture-builtins");
    try std.testing.expectEqualStrings("e10d11861504fc00f1255b41b0884e0782fa694224d4259e945e811c53a105c8", &std.fmt.bytesToHex(legacy, .lower));
    try std.testing.expectEqualStrings("27e454c4953a7c555e1be4fc11524680c1d07653102917ee83fcc4620e049714", &std.fmt.bytesToHex(current, .lower));
    try std.testing.expectEqualStrings("274489e48aab22649a6b0d7909bda0fa89216684082f4fa995c217c541c7f302", &std.fmt.bytesToHex(bundle, .lower));
}

test "compiler performance features have unique flags and cache identities" {
    var used: Mask = 0;
    inline for (std.meta.tags(Feature)) |feature| {
        try std.testing.expectEqual(@as(Mask, 0), used & feature.mask());
        used |= feature.mask();
        try std.testing.expect(!std.mem.eql(u8, &cacheIdentity(0), &cacheIdentity(feature.mask())));
        inline for (std.meta.tags(Feature)) |other| {
            if (feature != other) try std.testing.expect(!std.mem.eql(u8, feature.optionName(), other.optionName()));
        }
    }
}

test "compiler follow-up gates preserve the measured bundle and explicit overrides" {
    var bundle: Mask = 0;
    inline for (std.meta.tags(Feature)) |feature| {
        try std.testing.expect(!feature.resolve(false, null));
        try std.testing.expect(feature.resolve(false, true));
        try std.testing.expect(!feature.resolve(true, false));
        try std.testing.expectEqual(feature.enabledByAll(), feature.resolve(true, null));
        if (feature.resolve(true, null)) bundle |= feature.mask();
    }
    try std.testing.expectEqual(@as(Mask, 127), bundle);
    try std.testing.expectEqualSlices(u8, &.{ 2, 127, 0 }, &cacheIdentity(bundle));
    try std.testing.expectEqualSlices(u8, &.{ 3, 0, 1 }, &cacheIdentity(Feature.finalized_literal_cache.mask()));
    try std.testing.expectEqualSlices(u8, &.{ 3, 127, 1 }, &cacheIdentity(bundle | Feature.finalized_literal_cache.mask()));
    try std.testing.expect(!std.mem.eql(u8, &.{ 2, 0, 1 }, &cacheIdentity(Feature.finalized_literal_cache.mask())));
}
