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
pub const Tag = struct {
    name: base.Ident.Idx,
    args: @FieldType(snapshot.SnapshotTag, "args"),
    render_root: ?snapshot.SnapshotContentIdx,
    text: Text,
};
/// An explicit foreign source dependency for retained diagnostic regions.
pub const Source = extern struct { owner: u32, digest: [32]u8 };

status: Status = .diagnostic_free,
problems: []const problem.Problem = &.{},
pending: []const problem.Store.PendingStaticExhaustiveness = &.{},
extra_strings: []const u8 = &.{},
missing_patterns: []const problem.ExtraStringIdx = &.{},
regions: []const base.Region = &.{},
contents: []const snapshot.SnapshotContent = &.{},
node_formats: []const snapshot.NodeFormat = &.{},
captures: []const snapshot.Capture = &.{},
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
    node_formats: serde.SerializedSlice(snapshot.NodeFormat) = .{},
    captures: serde.SerializedSlice(snapshot.Capture) = .{},
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
    // No diagnostic consumer means no snapshot/region publication input.
    if (checker.problems.problems.items.len == 0 and checker.problems.pending_static_exhaustiveness.items.len == 0) return .{};
    const snapshots = &checker.snapshots;
    var text = std.ArrayList(u8).empty;
    var tags = std.ArrayList(Tag).empty;
    for (0..snapshots.tags.len()) |i| {
        const tag = snapshots.tags.get(@fromBackingInt(@as(u32, @intCast(i))));
        try tags.append(allocator, .{ .name = tag.name, .args = tag.args, .render_root = tag.render_root, .text = try appendText(allocator, &text, tag.formatted) });
    }
    var formatted = std.ArrayList(Formatted).empty;
    for (0..@as(usize, @intCast(snapshots.contents.len()))) |i| {
        const id: snapshot.SnapshotContentIdx = @fromBackingInt(@as(u32, @intCast(i)));
        if (snapshots.formatted_strings.get(id)) |bytes| try formatted.append(allocator, .{ .snapshot = id, .text = try appendText(allocator, &text, bytes) });
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
    const outcome: Self = .{
        .status = if (checker.problems.problems.items.len != 0 or checker.cir.diagnosticCount() != 0) .with_diagnostics else .diagnostic_free,
        .problems = checker.problems.problems.items,
        .pending = checker.problems.pending_static_exhaustiveness.items,
        .extra_strings = checker.problems.extra_strings_backing.items,
        .missing_patterns = checker.problems.missing_patterns_backing.items,
        .regions = checker.regions.items.items,
        .contents = snapshots.contents.items.items,
        .node_formats = snapshots.node_formats.items,
        .captures = snapshots.captures.items,
        .content_indexes = snapshots.content_indexes.items.items,
        .record_fields = fields,
        .tags = tags.items,
        .constraints = snapshots.static_dispatch_constraints.items.items,
        .formatted = formatted.items,
        .text = text.items,
        .sources = sources.items,
    };
    if (std.debug.runtime_safety) outcome.validate() catch
        base.invariant("diagnostic capture produced invalid deferred rendering references", .{});
    return outcome;
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

/// Validate producer-owned diagnostic references in Debug builds and tests.
/// Cache admission trusts the checked entry's authenticated producer data.
pub fn validate(self: Self) error{CorruptArtifact}!void {
    if (self.node_formats.len != self.contents.len) return error.CorruptArtifact;
    var end: u64 = 0;
    for (self.captures, 0..) |capture_, i| {
        if (capture_.start != end or capture_.len == 0) return error.CorruptArtifact;
        end += capture_.len;
        if (end > self.contents.len) return error.CorruptArtifact;
        for (self.node_formats[capture_.start..@intCast(end)]) |info| {
            if (info.capture != i) return error.CorruptArtifact;
        }
    }
    if (end != self.contents.len) return error.CorruptArtifact;
    for (self.contents, self.node_formats) |content, info| {
        switch (content) {
            .err => {},
            .recursive => if (info.source_var == null) return error.CorruptArtifact,
            inline .flex, .rigid => |identity| {
                try validateRange(identity.constraints, self.constraints.len);
                if (identity.constraints.count != 0) {
                    for (self.constraints[@backingInt(identity.constraints.start)..][0..identity.constraints.count]) |constraint| {
                        try self.validateNode(constraint.fn_content, info.capture);
                    }
                }
            },
            .alias => |alias| {
                try self.validateNode(alias.backing, info.capture);
                try self.validateVars(alias.vars, info.capture);
            },
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union => {},
                .box => return error.CorruptArtifact,
                .tuple => |tuple| try self.validateVars(tuple.elems, info.capture),
                .nominal_type => |nominal| {
                    if (nominal.vars.count == 0) return error.CorruptArtifact;
                    try self.validateVars(nominal.vars, info.capture);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    try self.validateVars(func.args, info.capture);
                    try self.validateNode(func.ret, info.capture);
                },
                .record => |record| {
                    try self.validateNode(record.ext, info.capture);
                    try validateRange(record.fields, self.record_fields.len);
                    if (record.fields.count != 0) {
                        for (self.record_fields[@backingInt(record.fields.start)..][0..record.fields.count]) |field| {
                            try self.validateNode(field.content, info.capture);
                        }
                    }
                },
                .tag_union => |union_| {
                    try self.validateNode(union_.ext, info.capture);
                    try validateRange(union_.tags, self.tags.len);
                    if (union_.tags.count != 0) {
                        for (self.tags[@backingInt(union_.tags.start)..][0..union_.tags.count]) |tag| {
                            try self.validateVars(tag.args, info.capture);
                            if (tag.render_root) |root| try self.validateNode(root, info.capture);
                        }
                    }
                },
            },
        }
    }
    for (self.tags) |tag| {
        try self.validateText(tag.text);
        if (tag.render_root) |root| {
            if (@backingInt(root) >= self.contents.len) return error.CorruptArtifact;
        } else if (tag.text.len == 0) return error.CorruptArtifact;
    }
    var previous_formatted: ?u64 = null;
    for (self.formatted) |formatted| {
        if (@backingInt(formatted.snapshot) >= self.contents.len) return error.CorruptArtifact;
        if (previous_formatted) |previous| {
            if (@backingInt(formatted.snapshot) <= previous) return error.CorruptArtifact;
        }
        previous_formatted = @backingInt(formatted.snapshot);
        try self.validateText(formatted.text);
    }
    for (self.missing_patterns) |range| {
        if (range.start > self.extra_strings.len or range.count > self.extra_strings.len - range.start) return error.CorruptArtifact;
    }
}

test "checking outcomes reject invalid deferred diagnostic references" {
    const contents = [_]snapshot.SnapshotContent{
        .{ .structure = .empty_record },
        .{ .structure = .{ .fn_pure = .{ .args = .empty(), .ret = @fromBackingInt(0) } } },
    };
    const infos = [_]snapshot.NodeFormat{
        .{ .source_var = null, .capture = 0, .polarity = .pos },
        .{ .source_var = null, .capture = 0, .polarity = .pos },
    };
    const captures = [_]snapshot.Capture{.{
        .start = 0,
        .len = 2,
        .omit_literal_conversion_constraints = false,
        .omitted_dispatcher = null,
        .identifier_count = 0,
    }};
    var outcome = Self{ .contents = &contents, .node_formats = &infos, .captures = &captures };
    try outcome.validate();

    var invalid_contents = contents;
    invalid_contents[1].structure.fn_pure.ret = @fromBackingInt(2);
    outcome.contents = &invalid_contents;
    try std.testing.expectError(error.CorruptArtifact, outcome.validate());
    outcome.contents = &contents;

    const duplicate = Formatted{ .snapshot = @fromBackingInt(0), .text = .{ .start = 0, .len = 2 } };
    outcome.text = "{}";
    outcome.formatted = &.{ duplicate, duplicate };
    try std.testing.expectError(error.CorruptArtifact, outcome.validate());
}

fn validateRange(range: anytype, len: usize) error{CorruptArtifact}!void {
    if (range.count == 0) return;
    const start: u64 = @backingInt(range.start);
    if (start > len or range.count > len - start) return error.CorruptArtifact;
}

fn validateNode(self: Self, node: snapshot.SnapshotContentIdx, capture_idx: u32) error{CorruptArtifact}!void {
    if (@backingInt(node) >= self.contents.len) return error.CorruptArtifact;
    if (self.node_formats[@backingInt(node)].capture != capture_idx) return error.CorruptArtifact;
}

fn validateVars(self: Self, range: anytype, capture_idx: u32) error{CorruptArtifact}!void {
    try validateRange(range, self.content_indexes.len);
    if (range.count == 0) return;
    for (self.content_indexes[@backingInt(range.start)..][0..range.count]) |node| try self.validateNode(node, capture_idx);
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
    snapshots.setFormatContext(&checker.type_writer);
    _ = try snapshots.contents.appendSlice(allocator, self.contents);
    try snapshots.node_formats.appendSlice(allocator, self.node_formats);
    try snapshots.captures.appendSlice(allocator, self.captures);
    try snapshots.render_graphs.appendNTimes(allocator, null, self.captures.len);
    _ = try snapshots.content_indexes.appendSlice(allocator, self.content_indexes);
    _ = try snapshots.record_fields.appendSlice(allocator, self.record_fields);
    _ = try snapshots.static_dispatch_constraints.appendSlice(allocator, self.constraints);
    for (self.tags) |tag| {
        const formatted = try allocator.dupe(u8, self.text[tag.text.start..][0..tag.text.len]);
        errdefer allocator.free(formatted);
        _ = try snapshots.tags.append(allocator, .{ .name = tag.name, .args = tag.args, .formatted = formatted, .render_root = tag.render_root });
    }
    for (self.formatted) |formatted| {
        const text = try allocator.dupe(u8, self.text[formatted.text.start..][0..formatted.text.len]);
        errdefer allocator.free(text);
        try snapshots.formatted_log.ensureUnusedCapacity(allocator, 1);
        try snapshots.formatted_strings.put(allocator, formatted.snapshot, text);
        snapshots.formatted_log.appendAssumeCapacity(formatted.snapshot);
    }
}
