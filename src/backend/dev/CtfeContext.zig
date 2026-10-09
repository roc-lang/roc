//! Stable producer-owned semantic context for CTFE machine-code artifacts.
//! Dense LIR IDs are binding inputs, never persisted semantic identities.

const std = @import("std");
const base = @import("base");

pub const Domain = enum(u8) { runtime, ctfe };
pub const SourceDescriptor = struct {
    checked_module: [32]u8,
    source_identity: [32]u8,
    region: base.Region,
    line: u32,
    column: u32,
    has_location: bool,
};
pub const LiteralKind = enum(u8) { numeral, quote };
pub const LiteralDescriptor = struct {
    checked_module: [32]u8,
    checked_expr: u32,
    kind: LiteralKind,
};
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
pub const FailureDescriptor = struct {
    source: SourceDescriptor,
    checked_error: bool,
    literal_rejection: ?LiteralDescriptor,
    guard_root: ?RootDescriptor,
};
pub const SiteDescriptor = struct {
    checked_module: [32]u8,
    checked_site: ?u32,
    procedure_identity: [32]u8,
    kind: enum(u8) { match, destructure, if_ },
    region: base.Region,
    branch_regions: []const base.Region,
};
pub const Binding = union(enum(u8)) {
    source_file: SourceDescriptor,
    failure: FailureDescriptor,
    site: SiteDescriptor,
    static_root: RootDescriptor,

    pub fn clone(self: Binding, allocator: std.mem.Allocator) std.mem.Allocator.Error!Binding {
        return switch (self) {
            .site => |site| blk: {
                var copy = site;
                copy.branch_regions = try allocator.dupe(base.Region, site.branch_regions);
                break :blk .{ .site = copy };
            },
            else => self,
        };
    }
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

test "CTFE context catalog selects one immutable producer without row fallback" {
    const descriptor: SourceDescriptor = .{
        .checked_module = [_]u8{41} ** 32,
        .source_identity = [_]u8{42} ** 32,
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
