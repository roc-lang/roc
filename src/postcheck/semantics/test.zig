//! Unit tests for the shared value-semantics walkers over mock contexts.

const std = @import("std");
const inspect = @import("inspect.zig");
const inspect_override = @import("inspect_override.zig");

const testing = std.testing;
const text = inspect.text;

/// A tiny type universe: each type is an index into `shapes`.
const MockInspect = struct {
    shapes: []const inspect.InspectShape(MockInspect),
    records: []const []const ?inspect.RecordField(MockInspect),
    tuples: []const []const u32,
    unions: []const []const Variant,

    pub const Ty = u32;
    pub const Primitive = u8;
    pub const Record = u32;
    pub const Tuple = u32;
    pub const TagUnion = u32;
    pub const PresenceSlot = u32;
    pub const Override = u32;
    pub const Child = u32;
    pub const Label = []const u8;

    const Variant = struct { name: []const u8, payloads: []const u32 };

    pub fn inspectShape(self: MockInspect, ty: Ty) inspect.InspectShape(MockInspect) {
        return self.shapes[ty];
    }
    pub fn recordSlotCount(self: MockInspect, record: Record) u32 {
        return @intCast(self.records[record].len);
    }
    pub fn recordField(self: MockInspect, record: Record, slot: u32) ?inspect.RecordField(MockInspect) {
        return self.records[record][slot];
    }
    pub fn tupleElemCount(self: MockInspect, tuple: Tuple) u32 {
        return @intCast(self.tuples[tuple].len);
    }
    pub fn tupleElem(self: MockInspect, tuple: Tuple, index: u32) Child {
        return self.tuples[tuple][index];
    }
    pub fn variantCount(self: MockInspect, tag_union: TagUnion) u32 {
        return @intCast(self.unions[tag_union].len);
    }
    pub fn variantName(self: MockInspect, tag_union: TagUnion, variant: u32) []const u8 {
        return self.unions[tag_union][variant].name;
    }
    pub fn payloadCount(self: MockInspect, tag_union: TagUnion, variant: u32) u32 {
        return @intCast(self.unions[tag_union][variant].payloads.len);
    }
    pub fn payload(self: MockInspect, tag_union: TagUnion, variant: u32, index: u32) Child {
        return self.unions[tag_union][variant].payloads[index];
    }
};

/// Renders parts into text, rendering each child as `#<child>`.
const TextSink = struct {
    out: std.ArrayList(u8) = .empty,

    pub const PartsError = std.mem.Allocator.Error;

    pub fn literal(self: *TextSink, bytes: []const u8) PartsError!void {
        try self.out.appendSlice(testing.allocator, bytes);
    }

    pub fn label(self: *TextSink, bytes: []const u8) PartsError!void {
        try self.out.appendSlice(testing.allocator, bytes);
    }

    pub fn child(self: *TextSink, id: u32) PartsError!void {
        try self.out.print(testing.allocator, "#{d}", .{id});
    }
};

const Spec = inspect.InspectSpec(MockInspect);

test "inspect parts render records, tuples, and variants in order" {
    const ctx = MockInspect{
        .shapes = &.{ .{ .record = 0 }, .{ .tuple = 0 }, .{ .tag_union = 0 } },
        .records = &.{&.{ .{ .label = "a", .child = 7 }, null, .{ .label = "b", .child = 8 } }},
        .tuples = &.{&.{ 3, 4, 5 }},
        .unions = &.{&.{ .{ .name = "None", .payloads = &.{} }, .{ .name = "Some", .payloads = &.{ 1, 2 } } }},
    };

    var record: TextSink = .{};
    defer record.out.deinit(testing.allocator);
    try Spec.recordParts(ctx, 0, &record);
    try testing.expectEqualStrings("{ a: #7, b: #8 }", record.out.items);

    var tuple: TextSink = .{};
    defer tuple.out.deinit(testing.allocator);
    try Spec.tupleParts(ctx, 0, &tuple);
    try testing.expectEqualStrings("(#3, #4, #5)", tuple.out.items);

    var none: TextSink = .{};
    defer none.out.deinit(testing.allocator);
    try Spec.variantParts(ctx, 0, 0, &none);
    try testing.expectEqualStrings("None", none.out.items);

    var some: TextSink = .{};
    defer some.out.deinit(testing.allocator);
    try Spec.variantParts(ctx, 0, 1, &some);
    try testing.expectEqualStrings("Some(#1, #2)", some.out.items);
}

test "inspect actions decide empty aggregates, nominals, and uninhabited unions" {
    const ctx = MockInspect{
        .shapes = &.{
            .{ .record = 0 },
            .{ .tuple = 0 },
            .{ .tag_union = 0 },
            .{ .nominal = .{ .override = 9, .backing = 0 } },
            .{ .nominal = .{ .override = null, .backing = 1 } },
            .{ .nominal = .{ .override = null, .backing = null } },
            .function,
            .empty_record,
        },
        // Padding alone renders as an empty record.
        .records = &.{&.{null}},
        .tuples = &.{&.{}},
        .unions = &.{&.{}},
    };
    try testing.expectEqualStrings(text.empty_record, Spec.action(ctx, 0).literal);
    try testing.expectEqualStrings(text.empty_tuple, Spec.action(ctx, 1).literal);
    try testing.expect(Spec.action(ctx, 2) == .uninhabited);
    try testing.expectEqual(@as(u32, 9), Spec.action(ctx, 3).override);
    try testing.expectEqual(@as(u32, 1), Spec.action(ctx, 4).backing);
    try testing.expectEqualStrings(text.opaque_value, Spec.action(ctx, 5).literal);
    try testing.expectEqualStrings(text.function, Spec.action(ctx, 6).literal);
    try testing.expectEqualStrings(text.empty_record, Spec.action(ctx, 7).literal);
}

const MockRegistry = struct {
    declared: bool,
    override: ?u32,

    pub fn lookup(self: *const MockRegistry, _: u32) ?void {
        return if (self.declared) {} else null;
    }

    pub fn inspectOverride(self: *const MockRegistry, _: u32) ?u32 {
        return self.override;
    }
};

const MockViews = struct {
    registries: []const MockRegistry,
    /// Views whose names include the owner and `to_inspect`.
    names_owner: []const bool,

    pub const Found = struct { view: usize, override: u32 };

    pub fn viewCount(self: MockViews) usize {
        return self.registries.len;
    }
    pub fn view(_: MockViews, index: usize) usize {
        return index;
    }
    pub fn inspectKey(self: MockViews, view_index: usize) ?u32 {
        return if (self.names_owner[view_index]) 0 else null;
    }
    pub fn registry(self: MockViews, view_index: usize) *const MockRegistry {
        return &self.registries[view_index];
    }
    pub fn found(_: MockViews, view_index: usize, _: u32, override: u32) Found {
        return .{ .view = view_index, .override = override };
    }
};

test "the first view declaring to_inspect decides the override" {
    // The scope names the owner but declares no method; the next view
    // declares an eligible override.
    const selected = inspect_override.lookupInspectOverride(MockViews{
        .registries = &.{ .{ .declared = false, .override = null }, .{ .declared = true, .override = 4 }, .{ .declared = true, .override = 5 } },
        .names_owner = &.{ true, true, true },
    }).?;
    try testing.expectEqual(@as(usize, 1), selected.view);
    try testing.expectEqual(@as(u32, 4), selected.override);

    // An ineligible or rejected declaration decides too: the default form,
    // even when a later view would record an override.
    try testing.expect(inspect_override.lookupInspectOverride(MockViews{
        .registries = &.{ .{ .declared = true, .override = null }, .{ .declared = true, .override = 5 } },
        .names_owner = &.{ true, true },
    }) == null);

    // A view that cannot name the owner is skipped.
    try testing.expectEqual(@as(u32, 5), inspect_override.lookupInspectOverride(MockViews{
        .registries = &.{ .{ .declared = true, .override = 4 }, .{ .declared = true, .override = 5 } },
        .names_owner = &.{ false, true },
    }).?.override);
}
