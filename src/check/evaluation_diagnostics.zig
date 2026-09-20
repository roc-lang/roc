//! Deferred evaluation diagnostics are checked data, independent of the solver.
//! Each recipe preserves the source site, checked type display and missing
//! patterns selected during checking. Evaluation only decides its outcome.

const std = @import("std");
const base = @import("base");
const problem = @import("problem.zig");
const serde = @import("artifact_serialize.zig");
const Allocator = std.mem.Allocator;

pub const Exhaustiveness = struct {
    kind: problem.Store.EmpiricalSiteKind,
    mode: problem.Store.PendingStaticExhaustivenessMode,
    source: problem.Store.ExhaustivenessSiteSource,
    site: ?@import("checked_ids.zig").CheckedExhaustivenessSiteId,
    region: base.Region,
    type_display: serde.Span,
    missing_patterns: serde.Span,
    empirical: bool,
};

pub const Templates = struct {
    exhaustiveness: []Exhaustiveness = &.{},
    strings: []u8 = &.{},
    missing_patterns: []serde.Span = &.{},

    pub fn fromStore(allocator: Allocator, store: *const problem.Store) Allocator.Error!Templates {
        if (store.pending_static_exhaustiveness.items.len == 0) return .{};
        var result = Templates{};
        errdefer result.deinit(allocator);
        result.exhaustiveness = try allocator.alloc(Exhaustiveness, store.pending_static_exhaustiveness.items.len);
        result.strings = try allocator.dupe(u8, store.extra_strings_backing.items);
        result.missing_patterns = try allocator.alloc(serde.Span, store.missing_patterns_backing.items.len);
        for (result.missing_patterns, store.missing_patterns_backing.items) |*out, input| {
            out.* = .{ .start = @intCast(input.start), .len = @intCast(input.count) };
        }
        for (result.exhaustiveness, store.pending_static_exhaustiveness.items) |*out, input| {
            const fields: struct { type_display: problem.ExtraStringIdx, missing: problem.MissingPatternsRange, empirical: bool } = switch (input.problem) {
                .non_exhaustive_match => |value| .{ .type_display = value.condition_type, .missing = value.missing_patterns, .empirical = value.empirical },
                .non_exhaustive_destructure => |value| .{ .type_display = value.value_type, .missing = value.missing_patterns, .empirical = value.empirical },
                else => unreachable,
            };
            out.* = .{
                .kind = input.kind,
                .mode = input.mode,
                .source = input.source,
                .site = input.site,
                .region = input.region,
                .type_display = .{ .start = @intCast(fields.type_display.start), .len = @intCast(fields.type_display.count) },
                .missing_patterns = .{ .start = @intCast(fields.missing.start), .len = @intCast(fields.missing.count) },
                .empirical = fields.empirical,
            };
        }
        return result;
    }

    pub fn instantiate(self: *const Templates, allocator: Allocator) Allocator.Error!problem.Store {
        var result = try problem.Store.init(allocator);
        errdefer result.deinit(allocator);
        try result.extra_strings_backing.appendSlice(self.strings);
        try result.missing_patterns_backing.ensureTotalCapacity(self.missing_patterns.len);
        for (self.missing_patterns) |input| result.missing_patterns_backing.appendAssumeCapacity(.{ .start = input.start, .count = input.len });
        try result.pending_static_exhaustiveness.ensureTotalCapacityPrecise(allocator, self.exhaustiveness.len);
        for (self.exhaustiveness) |input| {
            result.pending_static_exhaustiveness.appendAssumeCapacity(.{
                .kind = input.kind,
                .mode = input.mode,
                .source = input.source,
                .site = input.site,
                .region = input.region,
                .problem = switch (input.source) {
                    .match_expr => |expr| .{ .non_exhaustive_match = .{
                        .match_expr = expr,
                        .condition_type = .{ .start = input.type_display.start, .count = input.type_display.len },
                        .missing_patterns = .{ .start = input.missing_patterns.start, .count = input.missing_patterns.len },
                        .empirical = input.empirical,
                    } },
                    .destructure_pattern => |pattern| .{ .non_exhaustive_destructure = .{
                        .pattern = pattern,
                        .value_type = .{ .start = input.type_display.start, .count = input.type_display.len },
                        .missing_patterns = .{ .start = input.missing_patterns.start, .count = input.missing_patterns.len },
                        .empirical = input.empirical,
                    } },
                },
            });
        }
        return result;
    }

    pub fn deinit(self: *Templates, allocator: Allocator) void {
        allocator.free(self.exhaustiveness);
        allocator.free(self.strings);
        allocator.free(self.missing_patterns);
        self.* = .{};
    }

    pub const Serialized = extern struct {
        exhaustiveness: serde.SerializedSlice(Exhaustiveness) = .{},
        strings: serde.SerializedSlice(u8) = .{},
        missing_patterns: serde.SerializedSlice(serde.Span) = .{},
        const Serde = serde.SliceStoreSerde(Templates, @This());
        pub const serialize = Serde.serialize;
        pub const deserialize = Serde.deserialize;
    };
};

test "evaluation diagnostics retain exact checked text across serialization" {
    const allocator = std.testing.allocator;
    var templates = blk: {
        var source = try problem.Store.init(allocator);
        defer source.deinit(allocator);
        const type_text = try source.putExtraString("[Left(Str), Right(U64)]");
        const missing = try source.putExtraString("Right(_)");
        try source.missing_patterns_backing.append(missing);
        try source.appendPendingStaticExhaustiveness(allocator, .match, .empirical, .{ .match_expr = @enumFromInt(3) }, base.Region.zero(), .{ .non_exhaustive_match = .{
            .match_expr = @enumFromInt(3),
            .condition_type = type_text,
            .missing_patterns = .{ .start = 0, .count = 1 },
        } });
        source.assignPendingStaticExhaustivenessSite(.{ .match_expr = @enumFromInt(3) }, @enumFromInt(2));
        break :blk try Templates.fromStore(allocator, &source);
    };
    defer templates.deinit(allocator);
    var round_trip = try serde.roundTripForTest(allocator, Templates, &templates);
    defer allocator.free(round_trip.buffer);
    var evaluation = try round_trip.loaded.instantiate(allocator);
    defer evaluation.deinit(allocator);
    try std.testing.expect(try evaluation.appendEmpiricalExhaustivenessFailure(allocator, @enumFromInt(2)));
    const diagnostic = evaluation.problems.items[0].non_exhaustive_match;
    try std.testing.expect(diagnostic.empirical);
    try std.testing.expectEqualStrings("[Left(Str), Right(U64)]", evaluation.getExtraString(diagnostic.condition_type));
    try std.testing.expectEqualStrings("Right(_)", evaluation.getExtraString(evaluation.getMissingPatterns(diagnostic.missing_patterns)[0]));
}
