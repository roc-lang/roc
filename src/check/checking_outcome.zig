//! Pre-evaluation checking diagnostics and every byte needed to render them.
//! Problems retain their order and snapshots; rendering still uses the current
//! filename and current source. Empirical/evaluation diagnostics are not cached.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const problem = @import("problem.zig");
const snapshot = @import("snapshot.zig");
const serde = @import("artifact_serialize.zig");
const dependencies = @import("checking_dependencies.zig");
const Self = @This();
const Allocator = std.mem.Allocator;

/// Whether the pre-evaluation outcome contains checking diagnostics.
pub const Status = enum(u32) { diagnostic_free, with_diagnostics };
/// A range in the stored diagnostic text pool.
pub const Text = extern struct { start: u32, len: u32 };
/// A snapshot and its preserved display text.
pub const Formatted = extern struct { snapshot: snapshot.SnapshotContentIdx, text: Text };
/// Pointer-free snapshot tag with its preserved display text.
pub const Tag = struct { name: base.Ident.Idx, args: @FieldType(snapshot.SnapshotTag, "args"), text: Text };
/// An explicit foreign source dependency for retained diagnostic regions.
pub const Source = extern struct { owner: u32, digest: [32]u8 };

status: Status = .diagnostic_free,
problems: []const problem.Problem = &.{},
pending: []const problem.Store.PendingStaticExhaustiveness = &.{},
extra_strings: []const u8 = &.{},
missing_patterns: []const problem.ExtraStringIdx = &.{},
regions: []const base.Region = &.{},
contents: []const snapshot.SnapshotContent = &.{},
content_indexes: []const snapshot.SnapshotContentIdx = &.{},
record_fields: []const snapshot.SnapshotRecordField = &.{},
tags: []const Tag = &.{},
constraints: []const snapshot.SnapshotStaticDispatchConstraint = &.{},
formatted: []const Formatted = &.{},
text: []const u8 = &.{},
sources: []const Source = &.{},

/// Portable diagnostic storage, independent of evaluated outcomes.
pub const Serialized = extern struct {
    status: Status = .diagnostic_free,
    problems: serde.SerializedSlice(problem.Problem) = .{},
    pending: serde.SerializedSlice(problem.Store.PendingStaticExhaustiveness) = .{},
    extra_strings: serde.SerializedSlice(u8) = .{},
    missing_patterns: serde.SerializedSlice(problem.ExtraStringIdx) = .{},
    regions: serde.SerializedSlice(base.Region) = .{},
    contents: serde.SerializedSlice(snapshot.SnapshotContent) = .{},
    content_indexes: serde.SerializedSlice(snapshot.SnapshotContentIdx) = .{},
    record_fields: serde.SerializedSlice(snapshot.SnapshotRecordField) = .{},
    tags: serde.SerializedSlice(Tag) = .{},
    constraints: serde.SerializedSlice(snapshot.SnapshotStaticDispatchConstraint) = .{},
    formatted: serde.SerializedSlice(Formatted) = .{},
    text: serde.SerializedSlice(u8) = .{},
    sources: serde.SerializedSlice(Source) = .{},

    pub const root_contract: collections.serde_validation.RootContract = .{ .Owner = Self };
    pub fn serialize(self: *@This(), value: *const Self, allocator: Allocator, writer: *collections.CompactWriter) Allocator.Error!void {
        self.status = value.status;
        inline for (@typeInfo(@This()).@"struct".field_names) |field| {
            if (comptime !serde.comptimeStrEq(field, "status")) try @field(self, field).serialize(@field(value, field), allocator, writer);
        }
    }
    pub fn deserialize(self: *const @This(), address: usize) Self {
        var result: Self = .{ .status = self.status };
        inline for (@typeInfo(@This()).@"struct".field_names) |field| {
            if (comptime !serde.comptimeStrEq(field, "status")) @field(result, field) = @field(self, field).deserialize(address);
        }
        return result;
    }
};

fn appendText(allocator: Allocator, text: *std.ArrayList(u8), bytes: []const u8) Allocator.Error!Text {
    return try serde.appendSpan(Text, u8, text, allocator, bytes);
}

/// Borrow checker storage until serialization; flatten pointer-bearing strings
/// and multi-lists into arena-owned arrays with deterministic index order.
pub fn capture(allocator: Allocator, checker: anytype, scope: *const dependencies.Scope) Allocator.Error!Self {
    const snapshots = &checker.snapshots;
    var text = std.ArrayList(u8).empty;
    var tags = std.ArrayList(Tag).empty;
    for (0..snapshots.tags.len()) |i| {
        const tag = snapshots.tags.get(@fromBackingInt(@as(u32, @intCast(i))));
        try tags.append(allocator, .{ .name = tag.name, .args = tag.args, .text = try appendText(allocator, &text, tag.formatted) });
    }
    var formatted = std.ArrayList(Formatted).empty;
    for (0..@as(usize, @intCast(snapshots.contents.len()))) |i| {
        const id: snapshot.SnapshotContentIdx = @fromBackingInt(@as(u32, @intCast(i)));
        if (snapshots.getFormattedString(id)) |bytes| try formatted.append(allocator, .{ .snapshot = id, .text = try appendText(allocator, &text, bytes) });
    }
    const fields = try allocator.alloc(snapshot.SnapshotRecordField, @intCast(snapshots.record_fields.len()));
    for (fields, 0..) |*field, i| field.* = snapshots.record_fields.get(@fromBackingInt(@as(u32, @intCast(i))));
    var sources = std.ArrayList(Source).empty;
    // Platform diagnostics can retain a platform-side source region. Pin that
    // declared provenance to the current platform source before replay.
    if (checker.problems.problems.items.len != 0) if (checker.platform_requirements) |requirements| {
        var hash: [32]u8 = undefined;
        base.Sha256.hash(requirements.env.common.source, &hash, .{});
        try sources.append(allocator, .{ .owner = scope.ordinal(requirements.env.contentIdentityHash().?.*), .digest = hash });
    };
    return .{
        .status = if (checker.problems.problems.items.len != 0 or checker.cir.diagnosticCount() != 0) .with_diagnostics else .diagnostic_free,
        .problems = checker.problems.problems.items,
        .pending = checker.problems.pending_static_exhaustiveness.items,
        .extra_strings = checker.problems.extra_strings_backing.items,
        .missing_patterns = checker.problems.missing_patterns_backing.items,
        .regions = checker.regions.items.items,
        .contents = snapshots.contents.items.items,
        .content_indexes = snapshots.content_indexes.items.items,
        .record_fields = fields,
        .tags = tags.items,
        .constraints = snapshots.static_dispatch_constraints.items.items,
        .formatted = formatted.items,
        .text = text.items,
        .sources = sources.items,
    };
}

/// Validate source origins before the capsule is installed. A source shift that
/// invalidates a stored foreign region is a miss, not stale diagnostic replay.
pub fn sourcesMatch(self: Self, scope: *const dependencies.Scope) bool {
    for (self.sources) |source| {
        if (source.owner >= scope.owners.items.len) return false;
        const env = scope.environments.get(scope.owners.items[source.owner]) orelse return false;
        var hash: [32]u8 = undefined;
        base.Sha256.hash(env.common.source, &hash, .{});
        if (!base.ModuleIdentity.eql(&source.digest, &hash)) return false;
    }
    return true;
}

/// Check diagnostic text ranges before restoring optional cache data.
pub fn validate(self: Self) error{CorruptArtifact}!void {
    for (self.tags) |tag| try self.validateText(tag.text);
    for (self.formatted) |formatted| {
        if (@backingInt(formatted.snapshot) >= self.contents.len) return error.CorruptArtifact;
        try self.validateText(formatted.text);
    }
    for (self.missing_patterns) |range| {
        if (range.start > self.extra_strings.len or range.count > self.extra_strings.len - range.start) return error.CorruptArtifact;
    }
}

fn validateText(self: Self, range: Text) error{CorruptArtifact}!void {
    if (@as(u64, range.start) + range.len > self.text.len) return error.CorruptArtifact;
}

/// Restore exactly the pre-publication problem store, including deferred
/// exhaustiveness recipes. Ordinary publication and reporting decide failures.
pub fn install(self: Self, checker: anytype) Allocator.Error!void {
    const allocator = checker.gpa;
    try checker.problems.problems.appendSlice(allocator, self.problems);
    try checker.problems.pending_static_exhaustiveness.appendSlice(allocator, self.pending);
    try checker.problems.extra_strings_backing.appendSlice(self.extra_strings);
    try checker.problems.missing_patterns_backing.appendSlice(self.missing_patterns);
    checker.regions.items.clearRetainingCapacity();
    _ = try checker.regions.appendSlice(allocator, self.regions);
    const snapshots = &checker.snapshots;
    _ = try snapshots.contents.appendSlice(allocator, self.contents);
    _ = try snapshots.content_indexes.appendSlice(allocator, self.content_indexes);
    _ = try snapshots.record_fields.appendSlice(allocator, self.record_fields);
    _ = try snapshots.static_dispatch_constraints.appendSlice(allocator, self.constraints);
    for (self.tags) |tag| {
        const formatted = try allocator.dupe(u8, self.text[tag.text.start..][0..tag.text.len]);
        errdefer allocator.free(formatted);
        _ = try snapshots.tags.append(allocator, .{ .name = tag.name, .args = tag.args, .formatted = formatted });
    }
    try snapshots.formatted_strings_backing.appendSlice(self.text);
    for (self.formatted) |formatted| try snapshots.formatted_strings.put(allocator, formatted.snapshot, .{ .start = formatted.text.start, .count = formatted.text.len });
}
