//! Construction-slot bridge planning for executable MIR producers.

const std = @import("std");
const LambdaSolved = @import("../lambda_solved/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const repr = LambdaSolved.Representation;

/// Public bridge sink for executable AST stores.
pub const ExecutableSink = struct {
    store: *Ast.Store,

    pub const BridgeId = Ast.BridgeId;
    pub const BridgeSpan = Ast.Span(Ast.BridgeId);
    pub const BridgePlan = Ast.BridgePlan;

    pub fn addBridgePlan(self: *ExecutableSink, plan: BridgePlan) Allocator.Error!BridgeId {
        return try self.store.addBridgePlan(plan);
    }

    pub fn addBridgePlanSpan(self: *ExecutableSink, ids: []const BridgeId) Allocator.Error!BridgeSpan {
        return try self.store.addBridgePlanSpan(ids);
    }
};

/// Build a construction-slot bridge plan in an executable AST store.
pub fn executableBridge(
    allocator: Allocator,
    types: *const Type.Store,
    row_shapes: *const MonoRow.Store,
    ast: *Ast.Store,
    source_ty: Type.TypeId,
    target_ty: Type.TypeId,
) Allocator.Error!Ast.BridgeId {
    var sink = ExecutableSink{ .store = ast };
    var planner = Planner(ExecutableSink).init(allocator, types, row_shapes, &sink);
    return try planner.constructionSlotBridge(source_ty, target_ty);
}

/// Shared construction-slot bridge planner.
///
/// `Sink` must provide `BridgeId`, `BridgeSpan`, `BridgePlan`,
/// `addBridgePlan`, and `addBridgePlanSpan`. This keeps bridge storage owned by
/// the stage that emits it while centralizing the structural planning policy.
pub fn Planner(comptime Sink: type) type {
    return struct {
        allocator: Allocator,
        types: *const Type.Store,
        row_shapes: *const MonoRow.Store,
        sink: *Sink,

        const Self = @This();
        const BridgeId = Sink.BridgeId;
        const BridgeSpan = Sink.BridgeSpan;
        const BridgePlan = Sink.BridgePlan;

        pub fn init(
            allocator: Allocator,
            types: *const Type.Store,
            row_shapes: *const MonoRow.Store,
            sink: *Sink,
        ) Self {
            return .{
                .allocator = allocator,
                .types = types,
                .row_shapes = row_shapes,
                .sink = sink,
            };
        }

        pub fn constructionSlotBridge(
            self: *Self,
            source_ty: Type.TypeId,
            target_ty: Type.TypeId,
        ) Allocator.Error!BridgeId {
            if (source_ty == target_ty) {
                return switch (self.types.getType(source_ty)) {
                    .placeholder => constructionBridgeInvariant("construction bridge saw placeholder type"),
                    .link => constructionBridgeInvariant("construction bridge saw unresolved link type"),
                    .primitive => try self.sink.addBridgePlan(.direct),
                    .nominal, .box, .callable_set, .erased_fn => try self.sink.addBridgePlan(.nominal_reinterpret),
                    .list => try self.sink.addBridgePlan(.list_reinterpret),
                    .tuple => |items| try self.sink.addBridgePlan(.{ .struct_ = try self.structBridge(items, items) }),
                    .record => |record| try self.sink.addBridgePlan(.{ .struct_ = try self.recordBridge(record, record) }),
                    .tag_union => |tag_union| try self.sink.addBridgePlan(.{ .tag_union = try self.tagUnionBridge(tag_union, tag_union) }),
                    .vacant_callable_slot => try self.sink.addBridgePlan(.zst),
                };
            }

            const source = self.types.getType(source_ty);
            const target = self.types.getType(target_ty);
            const plan: BridgePlan = switch (source) {
                .placeholder => constructionBridgeInvariant("construction bridge saw placeholder source type"),
                .link => constructionBridgeInvariant("construction bridge saw unresolved source link"),
                .primitive => |source_prim| switch (target) {
                    .primitive => |target_prim| blk: {
                        if (source_prim != target_prim) constructionBridgeInvariant("construction bridge crossed primitive types");
                        break :blk .direct;
                    },
                    else => constructionBridgeInvariant("construction bridge crossed primitive/non-primitive types"),
                },
                .nominal => |source_nominal| switch (target) {
                    .nominal => |target_nominal| blk: {
                        if (source_nominal.nominal.module_name == target_nominal.nominal.module_name and
                            source_nominal.nominal.type_name == target_nominal.nominal.type_name)
                        {
                            break :blk .nominal_reinterpret;
                        }
                        constructionBridgeInvariant("construction bridge crossed distinct nominal types");
                    },
                    else => .nominal_reinterpret,
                },
                .list => switch (target) {
                    .list => .list_reinterpret,
                    .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed list/non-list types"),
                },
                .box => switch (target) {
                    .box, .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed box/non-box types"),
                },
                .tuple => |source_items| switch (target) {
                    .tuple => |target_items| .{ .struct_ = try self.structBridge(source_items, target_items) },
                    .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed tuple/non-tuple types"),
                },
                .record => |source_record| switch (target) {
                    .record => |target_record| .{ .struct_ = try self.recordBridge(source_record, target_record) },
                    .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed record/non-record types"),
                },
                .tag_union => |source_union| switch (target) {
                    .tag_union => |target_union| .{ .tag_union = try self.tagUnionBridge(source_union, target_union) },
                    .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed tag-union/non-tag-union types"),
                },
                .callable_set => |source_callable| switch (target) {
                    .callable_set => |target_callable| blk: {
                        if (!repr.callableSetKeyEql(source_callable.key, target_callable.key)) {
                            constructionBridgeInvariant("construction bridge crossed callable-set keys");
                        }
                        break :blk .nominal_reinterpret;
                    },
                    .nominal => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed callable-set/non-callable-set types"),
                },
                .erased_fn => switch (target) {
                    .erased_fn => .nominal_reinterpret,
                    else => constructionBridgeInvariant("construction bridge crossed erased-fn/non-erased-fn types"),
                },
                .vacant_callable_slot => switch (target) {
                    .vacant_callable_slot => .zst,
                    else => constructionBridgeInvariant("construction bridge crossed vacant/non-vacant callable-slot types"),
                },
            };
            return try self.sink.addBridgePlan(plan);
        }

        fn structBridge(
            self: *Self,
            source_items: []const Type.TypeId,
            target_items: []const Type.TypeId,
        ) Allocator.Error!BridgeSpan {
            if (source_items.len != target_items.len) constructionBridgeInvariant("construction struct bridge arity mismatch");
            if (source_items.len == 0) return BridgeSpan.empty();

            const children = try self.allocator.alloc(BridgeId, source_items.len);
            defer self.allocator.free(children);
            for (source_items, target_items, 0..) |source, target, i| {
                children[i] = try self.constructionSlotBridge(source, target);
            }
            return try self.sink.addBridgePlanSpan(children);
        }

        fn recordBridge(
            self: *Self,
            source: Type.RecordType,
            target: Type.RecordType,
        ) Allocator.Error!BridgeSpan {
            if (source.fields.len != target.fields.len) constructionBridgeInvariant("construction record bridge arity mismatch");
            if (source.fields.len == 0) return BridgeSpan.empty();

            const children = try self.allocator.alloc(BridgeId, source.fields.len);
            defer self.allocator.free(children);
            for (target.fields, 0..) |target_field, i| {
                const target_label = self.row_shapes.recordField(target_field.field).label;
                const source_field = self.recordFieldForLabel(source, target_label);
                children[i] = try self.constructionSlotBridge(source_field.ty, target_field.ty);
            }
            return try self.sink.addBridgePlanSpan(children);
        }

        fn tagUnionBridge(
            self: *Self,
            source: Type.TagUnionType,
            target: Type.TagUnionType,
        ) Allocator.Error!BridgeSpan {
            if (source.tags.len != target.tags.len) constructionBridgeInvariant("construction tag-union bridge arity mismatch");
            if (source.tags.len == 0) return BridgeSpan.empty();

            const children = try self.allocator.alloc(BridgeId, target.tags.len);
            defer self.allocator.free(children);
            for (target.tags, 0..) |target_tag, i| {
                const target_label = self.row_shapes.tag(target_tag.tag).label;
                const source_tag = self.tagTypeForLabel(source, target_label);
                children[i] = try self.tagPayloadBridge(source_tag, target_tag);
            }
            return try self.sink.addBridgePlanSpan(children);
        }

        fn tagPayloadBridge(
            self: *Self,
            source: Type.TagType,
            target: Type.TagType,
        ) Allocator.Error!BridgeId {
            if (source.payloads.len != target.payloads.len) constructionBridgeInvariant("construction tag payload bridge arity mismatch");
            if (source.payloads.len == 0) return try self.sink.addBridgePlan(.zst);
            if (source.payloads.len == 1) {
                return try self.constructionSlotBridge(source.payloads[0].ty, target.payloads[0].ty);
            }

            const source_payloads = try self.allocator.alloc(Type.TypeId, source.payloads.len);
            defer self.allocator.free(source_payloads);
            const target_payloads = try self.allocator.alloc(Type.TypeId, target.payloads.len);
            defer self.allocator.free(target_payloads);
            var source_seen = try self.allocator.alloc(bool, source.payloads.len);
            defer self.allocator.free(source_seen);
            var target_seen = try self.allocator.alloc(bool, target.payloads.len);
            defer self.allocator.free(target_seen);
            @memset(source_seen, false);
            @memset(target_seen, false);

            for (source.payloads) |payload| {
                const index: usize = @intCast(self.row_shapes.tagPayload(payload.payload).logical_index);
                if (index >= source_payloads.len) constructionBridgeInvariant("construction tag payload source index exceeded payload arity");
                if (source_seen[index]) constructionBridgeInvariant("construction tag payload bridge saw duplicate source payload");
                source_payloads[index] = payload.ty;
                source_seen[index] = true;
            }
            for (target.payloads) |payload| {
                const index: usize = @intCast(self.row_shapes.tagPayload(payload.payload).logical_index);
                if (index >= target_payloads.len) constructionBridgeInvariant("construction tag payload target index exceeded payload arity");
                if (target_seen[index]) constructionBridgeInvariant("construction tag payload bridge saw duplicate target payload");
                target_payloads[index] = payload.ty;
                target_seen[index] = true;
            }
            verifyAllSeen(source_seen, "construction tag payload bridge omitted source payload");
            verifyAllSeen(target_seen, "construction tag payload bridge omitted target payload");

            return try self.sink.addBridgePlan(.{ .struct_ = try self.structBridge(source_payloads, target_payloads) });
        }

        fn recordFieldForLabel(
            self: *Self,
            record: Type.RecordType,
            label: anytype,
        ) Type.RecordFieldType {
            for (record.fields) |field| {
                if (self.row_shapes.recordField(field.field).label == label) return field;
            }
            constructionBridgeInvariant("construction record bridge source field label is absent from source type");
        }

        fn tagTypeForLabel(
            self: *Self,
            tag_union: Type.TagUnionType,
            label: anytype,
        ) Type.TagType {
            for (tag_union.tags) |tag| {
                if (self.row_shapes.tag(tag.tag).label == label) return tag;
            }
            constructionBridgeInvariant("construction tag-union bridge source tag label is absent from source type");
        }
    };
}

fn verifyAllSeen(seen: []const bool, comptime message: []const u8) void {
    for (seen) |was_seen| {
        if (!was_seen) constructionBridgeInvariant(message);
    }
}

fn constructionBridgeInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}
