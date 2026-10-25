//! Persistent, request-relative interface constraints. No solver or producer
//! type-store identity crosses this boundary.
const std = @import("std");
const check = @import("check");
const base = @import("base");
const Type = @import("type.zig");
const Solve = @import("solve.zig");
const names = check.CheckedNames;
const Allocator = std.mem.Allocator;
const Constraints = Solve.InterfaceConstraints;

/// A settled relation, independent of mutable solver state.
pub const Summary = union(enum) { unchanged, constraints: Constraints };
/// Allocation failure or a result outside the persistent summary contract.
pub const EncodeError = Allocator.Error || error{NotRepresentable};
/// Allocation failure or an invalid optional cache record.
pub const DecodeError = Allocator.Error || error{InvalidSummary};

/// Encode settled leaves relative to the producer's captured input.
pub fn encode(allocator: Allocator, types: *Type.Store, ns: *const names.NameStore, leaves: []const Type.TypeId, summary: Summary) EncodeError![]u8 {
    var writer: Writer = .{ .allocator = allocator, .types = types, .names = ns, .leaves = leaves };
    defer writer.bytes.deinit(allocator);
    const counts: [3]u32 = switch (summary) {
        .unchanged => .{ 0, 0, 0 },
        .constraints => |c| .{ @intCast(c.nodes.len), @intCast(c.open_nodes.len), @intCast(c.kinds.len) },
    };
    try writer.write([3]u32, counts);
    try writer.write(Summary, summary);
    return writer.bytes.toOwnedSlice(allocator);
}

/// `context.validNamedType` validates checked provenance against the current
/// exact module versions. Returned storage belongs to `arena`.
pub fn decode(arena: Allocator, types: *Type.Store, ns: *names.NameStore, leaves: []const Type.TypeId, bytes: []const u8, context: anytype) DecodeError!Summary {
    var reader: Reader(@TypeOf(context)) = .{ .arena = arena, .types = types, .names = ns, .leaves = leaves, .bytes = bytes, .context = context };
    reader.counts = try reader.read([3]u32);
    const summary = try reader.read(Summary);
    if (reader.bytes.len != 0) return error.InvalidSummary;
    switch (summary) {
        .unchanged => if (!std.meta.eql(reader.counts, [3]u32{ 0, 0, 0 })) return error.InvalidSummary,
        .constraints => |c| {
            if (c.nodes.len != reader.counts[0] or c.open_nodes.len != reader.counts[1] or c.kinds.len != reader.counts[2]) return error.InvalidSummary;
            for (c.nodes) |node| if (node == .open and node.open >= c.open_nodes.len) return error.InvalidSummary;
            for (c.open_nodes) |node| {
                if (node.content == .redirect or node.content == .leaf) return error.InvalidSummary;
                if (node.related_group != .none and @backingInt(node.related_group) >= c.open_nodes.len) return error.InvalidSummary;
            }
        },
    }
    return summary;
}

/// Procedure ordinals, captured input and context-qualified evidence identity.
/// Checked callable-type ordinals are provenance; canonical keys carry their
/// relation, as in `Ast.fnEvidenceDigest`.
pub fn requestKey(request: Constraints.Identity, evidence: [32]u8, procedure: [8]u8) [32]u8 {
    var hash = std.crypto.hash.sha2.Sha256.init(.{});
    hash.update("roc.interface-request.v2");
    hash.update(&procedure);
    hash.update(request.bytes);
    hash.update(&evidence);
    const result = hash.finalResult();
    if (base.dev_diagnostics.isSet(.pack_trace)) std.debug.print("interface request key={x} evidence={x} leaves={d}\n", .{ result, evidence, request.leaves.len });
    return result;
}

const Writer = struct {
    allocator: Allocator,
    types: *Type.Store,
    names: *const names.NameStore,
    leaves: []const Type.TypeId,
    bytes: std.ArrayList(u8) = .empty,

    fn write(self: *Writer, comptime T: type, value: T) EncodeError!void {
        if (T == Type.TypeId) {
            for (self.leaves, 0..) |leaf, index| if (leaf == value or
                (std.meta.eql(self.types.representationDigestCached(self.names, leaf, null), self.types.representationDigestCached(self.names, value, null)) and
                    try self.types.typeEql(self.names, leaf, value)))
            {
                try self.write(u8, 0);
                return self.write(u32, @intCast(index));
            };
            const content = self.types.get(value);
            if (content != .primitive) return error.NotRepresentable;
            try self.write(u8, 1);
            return self.write(Type.Primitive, content.primitive);
        }
        if (T == Type.MaybeTypeId) return self.write(?Type.TypeId, value.get());
        if (T == names.ModuleIdentityId) return self.write([32]u8, self.names.moduleIdentityBytes(value).*);
        if (T == names.TypeNameId) return self.write([]const u8, self.names.typeNameText(value));
        if (T == names.RecordFieldNameId) return self.write([]const u8, self.names.recordFieldLabelText(value));
        if (T == names.TagNameId) return self.write([]const u8, self.names.tagLabelText(value));
        // A default's expression address needs a separate relocatable producer
        // contract; it is not inferred from its current type or source spelling.
        if (T == Type.FieldDefault) return error.NotRepresentable;
        switch (@typeInfo(T)) {
            .@"struct" => |info| inline for (info.field_names, info.field_types) |field, F| try self.write(F, @field(value, field)),
            .@"union" => |info| {
                try self.write(info.tag_type.?, std.meta.activeTag(value));
                inline for (info.field_names, info.field_types) |field, F| if (std.meta.activeTag(value) == @field(info.tag_type.?, field)) return self.write(F, @field(value, field));
                unreachable; // activeTag(value) names a field handled by the loop above.
            },
            .optional => |info| {
                try self.write(bool, value != null);
                if (value) |v| try self.write(info.child, v);
            },
            .pointer => |info| switch (info.size) {
                .slice => {
                    try self.write(u32, std.math.cast(u32, value.len) orelse return error.NotRepresentable);
                    for (value) |v| try self.write(info.child, v);
                },
                .one => try self.write(info.child, value.*),
                .many, .c => @compileError("unbounded summary pointer"),
            },
            .array => |info| for (value) |v| try self.write(info.child, v),
            .@"enum" => |info| try self.write(info.tag_type, @backingInt(value)),
            .bool => try self.write(u8, @intFromBool(value)),
            .int => {
                const U = @Int(.unsigned, @bitSizeOf(T));
                const size = (@bitSizeOf(T) + 7) / 8;
                var bits: U = @bitCast(value);
                for (0..size) |_| {
                    try self.bytes.append(self.allocator, @truncate(@as(u64, bits)));
                    bits = std.math.shr(U, bits, 8);
                }
            },
            .void => {},
            .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported summary field: " ++ @typeName(T)),
        }
    }
};

fn Reader(comptime Context: type) type {
    return struct {
        const Self = @This();
        arena: Allocator,
        types: *Type.Store,
        names: *names.NameStore,
        leaves: []const Type.TypeId,
        bytes: []const u8,
        context: Context,
        counts: [3]u32 = .{ 0, 0, 0 },
        depth: usize = 0,

        fn read(self: *Self, comptime T: type) DecodeError!T {
            if (self.depth == 128) return error.InvalidSummary;
            self.depth += 1;
            defer self.depth -= 1;
            if (T == Type.TypeId) return switch (try self.read(u8)) {
                0 => blk: {
                    const index = try self.read(u32);
                    if (index >= self.leaves.len) return error.InvalidSummary;
                    break :blk self.leaves[index];
                },
                1 => try self.types.internPrimitive(self.names, try self.read(Type.Primitive)),
                else => error.InvalidSummary,
            };
            if (T == Type.MaybeTypeId) return .of(try self.read(?Type.TypeId));
            if (T == names.ModuleIdentityId) {
                const digest = try self.read([32]u8);
                if (!self.context.validModuleIdentity(digest)) return error.InvalidSummary;
                return self.names.internModuleIdentity(&digest);
            }
            if (T == names.TypeNameId) return self.names.internTypeName(try self.read([]const u8));
            if (T == names.RecordFieldNameId) return self.names.internRecordFieldLabel(try self.read([]const u8));
            if (T == names.TagNameId) return self.names.internTagLabel(try self.read([]const u8));
            if (T == Type.FieldDefault) return error.InvalidSummary;
            if (T == Solve.NodeId or T == Solve.MaybeNodeId or T == Solve.FieldKindId) {
                const index = try self.read(u32);
                if (T == Solve.MaybeNodeId and index == std.math.maxInt(u32)) return @fromBackingInt(index);
                if (index >= self.counts[if (T == Solve.FieldKindId) 2 else 0]) return error.InvalidSummary;
                return @fromBackingInt(index);
            }
            const result: T = switch (@typeInfo(T)) {
                .@"struct" => |info| blk: {
                    var value: T = undefined;
                    inline for (info.field_names, info.field_types) |field, F| @field(value, field) = try self.read(F);
                    break :blk value;
                },
                .@"union" => |info| blk: {
                    const tag = try self.read(info.tag_type.?);
                    inline for (info.field_names, info.field_types) |field, F| if (tag == @field(info.tag_type.?, field)) break :blk @unionInit(T, field, try self.read(F));
                    return error.InvalidSummary;
                },
                .optional => |info| if (try self.read(bool)) try self.read(info.child) else null,
                .pointer => |info| blk: {
                    if (info.size == .one) {
                        const value = try self.arena.create(info.child);
                        value.* = try self.read(info.child);
                        break :blk value;
                    }
                    if (info.size != .slice) @compileError("unbounded summary pointer");
                    const len = try self.read(u32);
                    // Every supported element consumes at least one byte.
                    if (len > self.bytes.len) return error.InvalidSummary;
                    const values = try self.arena.alloc(info.child, len);
                    for (values) |*value| value.* = try self.read(info.child);
                    break :blk values;
                },
                .array => |info| blk: {
                    var value: T = undefined;
                    for (&value) |*v| v.* = try self.read(info.child);
                    break :blk value;
                },
                .@"enum" => |info| blk: {
                    const value = try self.read(info.tag_type);
                    if (info.mode == .exhaustive) {
                        inline for (info.field_values) |v| if (value == v) break :blk @fromBackingInt(value);
                        return error.InvalidSummary;
                    }
                    break :blk @fromBackingInt(value);
                },
                .bool => switch (try self.read(u8)) {
                    0 => false,
                    1 => true,
                    else => return error.InvalidSummary,
                },
                .int => blk: {
                    const U = @Int(.unsigned, @bitSizeOf(T));
                    const size = (@bitSizeOf(T) + 7) / 8;
                    if (size > self.bytes.len) return error.InvalidSummary;
                    if (@bitSizeOf(T) % 8 != 0 and self.bytes[size - 1] >= (@as(u16, 1) << (@bitSizeOf(T) % 8))) return error.InvalidSummary;
                    var value: U = 0;
                    for (self.bytes[0..size], 0..) |b, i| value |= std.math.shl(U, @as(U, @truncate(b)), i * 8);
                    self.bytes = self.bytes[size..];
                    break :blk @bitCast(value);
                },
                .void => {},
                .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported summary field: " ++ @typeName(T)),
            };
            if (T == Type.NamedType) if (!self.context.validNamedType(result)) return error.InvalidSummary;
            return result;
        }
    };
}

test "persistent interface constraints relocate request leaves and preserve cycles" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var ns = names.NameStore.init(allocator);
    defer ns.deinit();
    var source = Type.Store.init(allocator);
    defer source.deinit();
    var destination = Type.Store.init(allocator);
    defer destination.deinit();
    const source_ty = try source.internPrimitive(&ns, .u64);
    _ = try destination.internPrimitive(&ns, .bool);
    const destination_ty = try destination.internPrimitive(&ns, .u64);
    const input: Summary = .{ .constraints = .{
        .roots = &.{@fromBackingInt(0)},
        .nodes = &.{ .{ .open = 0 }, .{ .mono = source_ty } },
        .open_nodes = &.{.{ .content = .{ .func = .{ .args = @constCast(&[_]Solve.NodeId{@fromBackingInt(1)}), .ret = @fromBackingInt(0) } } }},
        .kinds = &.{},
    } };
    const bytes = try encode(allocator, &source, &ns, &.{source_ty}, input);
    defer allocator.free(bytes);
    const Provenance = struct {
        pub fn validNamedType(_: @This(), _: Type.NamedType) bool {
            return false;
        }
        pub fn validModuleIdentity(_: @This(), _: [32]u8) bool {
            return false;
        }
    };
    const result = (try decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bytes, Provenance{})).constraints;
    try std.testing.expectEqual(destination_ty, result.nodes[1].mono);
    try std.testing.expectEqual(@as(u32, 0), @backingInt(result.open_nodes[0].content.func.ret));
    for (0..bytes.len) |len| try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bytes[0..len], Provenance{}));
    const bad = try allocator.dupe(u8, bytes);
    defer allocator.free(bad);
    // Header declares no node slots, but the root still references slot zero.
    @memset(bad[0..4], 0);
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bad, Provenance{}));
}

test "persistent interface leaves preserve representation beyond semantic equality" {
    const allocator = std.testing.allocator;
    var ns = names.NameStore.init(allocator);
    defer ns.deinit();
    var types = Type.Store.init(allocator);
    defer types.deinit();
    const u64_ty = try types.internPrimitive(&ns, .u64);
    const alias = try types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @fromBackingInt(0), .ty_module = .{} },
        .def = .{ .module = try ns.internModuleIdentity(&@as([32]u8, @splat(1))), .type_name = try ns.internTypeName("Alias") },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = .{ .ty = u64_ty, .use = .inspectable },
    } });
    try std.testing.expect(try types.typeEql(&ns, alias, u64_ty));
    const summary: Summary = .{ .constraints = .{
        .roots = &.{@fromBackingInt(0)},
        .nodes = &.{.{ .mono = alias }},
        .open_nodes = &.{},
        .kinds = &.{},
    } };
    try std.testing.expectError(error.NotRepresentable, encode(allocator, &types, &ns, &.{u64_ty}, summary));
}

test "persistent interface requests use canonical evidence instead of checked ordinals" {
    const Ast = @import("ast.zig");
    const nodes = [_]check.ConstStore.ConstFnEvidence{.{ .target = 0 }};
    var targets = [_]check.ConstStore.ConstFnTargetEvidence{.{
        .view = .{},
        .method = .{ .module_idx = 3, .def_idx = @fromBackingInt(4), .kind = .{ .structural = .parser }, .callable_ty = @fromBackingInt(5) },
        .method_callable_key = .{ .bytes = @splat(1) },
        .instantiation = .{ .view = .{}, .callable_key = .{ .bytes = @splat(2) }, .callable_ty = @fromBackingInt(6) },
        .nested = .from_callable,
    }};
    const evidence: check.ConstStore.ConstFnEvidenceVector = .{ .nodes = &nodes, .targets = &targets };
    const frames = [_]check.ConstStore.ConstFnEvidenceFrame{.init(.root, null, 0, 1)};
    const request: Constraints.Identity = .{ .bytes = &.{ 1, 2, 3 }, .leaves = &.{} };
    const original = requestKey(request, Ast.fnEvidenceDigest(evidence, &frames, 0).bytes, @splat(0));
    // Procedures in the same provider file must keep distinct request keys.
    try std.testing.expect(!std.meta.eql(original, requestKey(request, Ast.fnEvidenceDigest(evidence, &frames, 0).bytes, .{ 1, 0, 0, 0, 0, 0, 0, 0 })));
    try std.testing.expect(!std.meta.eql(original, requestKey(request, Ast.fnEvidenceDigest(evidence, &frames, 0).bytes, .{ 0, 0, 0, 0, 1, 0, 0, 0 })));
    targets[0].method.callable_ty = @fromBackingInt(7);
    targets[0].instantiation.?.callable_ty = @fromBackingInt(8);
    try std.testing.expectEqual(original, requestKey(request, Ast.fnEvidenceDigest(evidence, &frames, 0).bytes, @splat(0)));
    targets[0].method_callable_key.bytes[0] = 9;
    try std.testing.expect(!std.meta.eql(original, requestKey(request, Ast.fnEvidenceDigest(evidence, &frames, 0).bytes, @splat(0))));
}

test "persistent interface evidence contexts preserve types and exact procedure bodies" {
    const Ast = @import("ast.zig");
    const first: [32]u8 = @splat(1);
    const second: [32]u8 = @splat(2);
    var contexts = std.AutoHashMap([32]u8, [32]u8).init(std.testing.allocator);
    defer contexts.deinit();
    try contexts.put(first, @splat(3));
    try contexts.put(second, @splat(3));
    const nodes = [_]check.ConstStore.ConstFnEvidence{.{ .target = 0 }};
    var targets = [_]check.ConstStore.ConstFnTargetEvidence{.{
        .view = .{ .bytes = first },
        .method = .{ .module_idx = 0, .def_idx = @fromBackingInt(0), .kind = .{ .structural = .parser }, .callable_ty = @fromBackingInt(0) },
        .method_callable_key = .{ .bytes = @splat(4) },
        .instantiation = .{ .view = .{ .bytes = first }, .callable_key = .{ .bytes = @splat(5) }, .callable_ty = @fromBackingInt(0) },
        .nested = .from_callable,
    }};
    const evidence: check.ConstStore.ConstFnEvidenceVector = .{ .nodes = &nodes, .targets = &targets };
    const original = Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts);
    targets[0].view.bytes = second;
    targets[0].instantiation.?.view.bytes = second;
    try std.testing.expectEqual(original, Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts));
    try contexts.put(second, @splat(6));
    try std.testing.expect(!std.meta.eql(original, Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts)));
    try contexts.put(second, @splat(3));
    targets[0].method.kind = .{ .procedure = .{
        .proc = .{ .artifact = .{ .bytes = first }, .proc_base = @fromBackingInt(0) },
        .template = .{ .artifact = .{ .bytes = first }, .proc_base = @fromBackingInt(0), .template = @fromBackingInt(0) },
    } };
    const procedure = Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts);
    targets[0].method.kind.procedure.template.artifact.bytes = second;
    try std.testing.expect(!std.meta.eql(procedure, Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts)));
}
