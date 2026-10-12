//! Producer-owned source-event descriptors for ordinary native artifacts.
//! Checked identities remain explicit across emission and cache publication.

const std = @import("std");
const base = @import("base");

/// The source location an event reports; `has_location` is false when the producer had none.
pub const SourceDescriptor = struct {
    checked_module: [32]u8,
    source_identity: [32]u8,
    region: base.Region,
    line: u32,
    column: u32,
    has_location: bool,
};
/// The literal conversion a `LiteralDescriptor` names.
pub const LiteralKind = enum(u8) { numeral, quote, interpolation };
/// A checked literal conversion expression.
pub const LiteralDescriptor = struct {
    checked_module: [32]u8,
    checked_expr: u32,
    kind: LiteralKind,
};
/// A compile-time root's producer, layout, and role (stored value or failure message).
pub const RootDescriptor = struct {
    module: [32]u8,
    producer: union(enum) {
        checked: u32,
        literal: struct {
            literal: LiteralDescriptor,
            procedure_identity: [32]u8,
        },
    },
    layout_digest: [32]u8,
    role: union(enum(u8)) {
        value,
        failure_message: struct {
            failed_field: u32,
            message_field: u32,
            failed_offset: u32,
            message_offset: u32,
        },
    },
};
/// A failure event's source and, when present, its rejected literal and guarding root.
pub const FailureDescriptor = struct {
    source: SourceDescriptor,
    checked_error: bool,
    literal_rejection: ?LiteralDescriptor,
    guard_root: ?RootDescriptor,
};
/// A branching site (match, destructure, or if) and the regions of its branches.
pub const SiteDescriptor = struct {
    checked_module: [32]u8,
    checked_site: ?u32,
    checked_origin: ?@import("lir").LIR.CheckedSiteOrigin = null,
    procedure_identity: [32]u8,
    kind: enum(u8) { match, destructure, if_ },
    region: base.Region,
    branch_regions: []const base.Region,
};
/// Semantic keys used by the normal source-event observer's prebound tables.
pub const Binding = union(enum(u8)) {
    source_file: SourceDescriptor,
    failure: FailureDescriptor,
    site: SiteDescriptor,
    static_root: RootDescriptor,
};

/// Earlier producers fill these tables before code emission/body retirement.
/// Missing rows decline publication; they never authorize source reconstruction.
pub const Catalog = struct {
    /// An immutable producer can supply only the metadata emission requests,
    /// without duplicating full descriptors for every intermediate statement.
    pub const Provider = struct {
        context: *const anyopaque,
        source: *const fn (*const anyopaque, u32) ?SourceDescriptor,
        failure: *const fn (*const anyopaque, u32) ?FailureDescriptor,
        site: *const fn (*const anyopaque, u32) ?SiteDescriptor,
        root: *const fn (*const anyopaque, u32) ?RootDescriptor,
    };

    provider: ?Provider = null,
    sources: []const ?SourceDescriptor = &.{},
    failures: []const ?FailureDescriptor = &.{},
    sites: []const ?SiteDescriptor = &.{},
    roots: []const ?RootDescriptor = &.{},

    pub fn source(self: *const Catalog, statement: u32) ?SourceDescriptor {
        if (self.provider) |provider| return provider.source(provider.context, statement);
        return if (statement < self.sources.len) self.sources[statement] else null;
    }

    pub fn failure(self: *const Catalog, statement: u32) ?FailureDescriptor {
        if (self.provider) |provider| return provider.failure(provider.context, statement);
        return if (statement < self.failures.len) self.failures[statement] else null;
    }

    pub fn site(self: *const Catalog, index: u32) ?SiteDescriptor {
        if (self.provider) |provider| return provider.site(provider.context, index);
        return if (index < self.sites.len) self.sites[index] else null;
    }

    pub fn root(self: *const Catalog, index: u32) ?RootDescriptor {
        if (self.provider) |provider| return provider.root(provider.context, index);
        return if (index < self.roots.len) self.roots[index] else null;
    }
};

test "source-event catalog selects one immutable producer without row fallback" {
    const descriptor: SourceDescriptor = .{
        .checked_module = @as([32]u8, @splat(41)),
        .source_identity = @as([32]u8, @splat(42)),
        .region = .{ .start = .{ .offset = 1 }, .end = .{ .offset = 4 } },
        .line = 2,
        .column = 3,
        .has_location = true,
    };
    const Producer = struct {
        fn source(context: *const anyopaque, statement: u32) ?SourceDescriptor {
            const value: *const SourceDescriptor = @ptrCast(@alignCast(context));
            return if (statement == 7) value.* else null;
        }
        fn failure(_: *const anyopaque, _: u32) ?FailureDescriptor {
            return null;
        }
        fn site(_: *const anyopaque, _: u32) ?SiteDescriptor {
            return null;
        }
        fn root(_: *const anyopaque, _: u32) ?RootDescriptor {
            return null;
        }
    };
    const array_catalog: Catalog = .{ .sources = &.{descriptor} };
    try std.testing.expectEqualDeep(descriptor, array_catalog.source(0).?);
    try std.testing.expect(array_catalog.source(7) == null);
    const callback_catalog: Catalog = .{
        .sources = &.{descriptor},
        .provider = .{
            .context = &descriptor,
            .source = Producer.source,
            .failure = Producer.failure,
            .site = Producer.site,
            .root = Producer.root,
        },
    };
    try std.testing.expect(callback_catalog.source(0) == null);
    try std.testing.expectEqualDeep(descriptor, callback_catalog.source(7).?);
    try std.testing.expect(callback_catalog.failure(7) == null);
    try std.testing.expect(callback_catalog.site(7) == null);
    try std.testing.expect(callback_catalog.root(7) == null);
}
