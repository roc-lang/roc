//! Persistent, request-relative interface constraints. No solver or producer
//! type-store identity crosses this boundary.
const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const base = @import("base");
const Type = @import("type.zig");
const Solve = @import("solve.zig");
const names = check.CheckedNames;
const Allocator = std.mem.Allocator;
const Constraints = Solve.InterfaceConstraints;
const GuardedList = @import("collections").GuardedList;
const invariant = base.invariant;
const magic = "RIFSUM04";
const frozen_magic = "RIFRZ001";

/// A checked occurrence either names its immutable checked module directly, or
/// names a closed checked type by normalized content. The latter has no
/// instantiation identity and can be bound to any current committed root
/// with that exact key; its nominal owner remains content-addressed.
pub const NamedOccurrence = union(enum) {
    exact: struct { owner: [32]u8, occurrence: [32]u8, ty: u32 },
    closed: struct { owner: [32]u8, key: [32]u8 },
    /// Context-defined ordinal in an exactly identified current input.
    input: u32,
};

/// Work performed by the compact codec, independent of wall-clock timing.
pub const Stats = struct { types: u64 = 0, composites: u64 = 0, references: u64 = 0, defaults: u64 = 0 };

/// A settled relation, independent of mutable solver state.
pub const Summary = union(enum) { unchanged, constraints: Constraints };
/// Allocation failure or a result outside the persistent summary contract.
pub const EncodeError = Allocator.Error || error{NotRepresentable};
/// Allocation failure or an invalid optional cache record.
pub const DecodeError = Allocator.Error || error{InvalidSummary};

fn invalidSummary(comptime line: u32) error{InvalidSummary} {
    if (base.dev_diagnostics.isSet(.pack_trace)) std.debug.print("interface invalid summary line={d}\n", .{line});
    return error.InvalidSummary;
}

/// Whether every placeholder open node is the slot of a field kind the
/// summary records. An undetermined field kind's slot stays a placeholder
/// until relation freeze selects its kind, and instantiation recreates it as
/// one, exactly as an in-memory summary does; any other placeholder is
/// unfinished solver state.
fn placeholdersAreFieldKindSlots(allocator: Allocator, c: Constraints) Allocator.Error!bool {
    var slots = try std.DynamicBitSetUnmanaged.initEmpty(allocator, c.open_nodes.len);
    defer slots.deinit(allocator);
    for (c.kinds) |kind| {
        const cells = kind.cells orelse continue;
        const index: usize = @backingInt(cells.slot);
        if (index >= c.nodes.len) continue;
        switch (c.nodes[index]) {
            .open => |open| if (open < c.open_nodes.len) slots.set(open),
            .mono => {},
        }
    }
    for (c.open_nodes, 0..) |node, index| {
        if (node.content == .unresolved and node.content.unresolved.origin == .placeholder and !slots.isSet(index)) return false;
    }
    return true;
}

/// Encode only the immutable graph reachable from the settled relation. The
/// context supplies exact checked module addresses for runtime defaults.
pub fn encode(allocator: Allocator, types: *Type.Store, ns: *const names.NameStore, leaves: []const Type.TypeId, summary: Summary, context: anytype) EncodeError![]u8 {
    return encodeWithStats(allocator, types, ns, leaves, summary, context, null);
}

/// Encoding with optional deterministic work counters.
pub fn encodeWithStats(allocator: Allocator, types: *Type.Store, ns: *const names.NameStore, leaves: []const Type.TypeId, summary: Summary, context: anytype, stats: ?*Stats) EncodeError![]u8 {
    var writer: Writer(@TypeOf(context)) = .{ .allocator = allocator, .types = types, .names = ns, .leaves = leaves, .context = context };
    defer writer.bytes.deinit(allocator);
    defer writer.references.deinit(allocator);
    defer writer.pending.deinit(allocator);
    defer writer.leaf_buckets.deinit(allocator);
    defer writer.leaf_next.deinit(allocator);
    if (summary != .unchanged) try writer.indexLeaves();
    if (summary == .constraints and !try placeholdersAreFieldKindSlots(allocator, summary.constraints)) {
        invariant("settled interface contains a placeholder that is no field kind's slot", .{});
    }
    try writer.bytes.appendSlice(allocator, magic);
    const counts: [3]u32 = switch (summary) {
        .unchanged => .{ 0, 0, 0 },
        .constraints => |c| .{ @intCast(c.nodes.len), @intCast(c.open_nodes.len), @intCast(c.kinds.len) },
    };
    try writer.write([3]u32, counts);
    const count_offset = writer.bytes.items.len;
    try writer.write(u32, 0);
    try writer.write(u32, std.math.cast(u32, leaves.len) orelse return error.NotRepresentable);
    try writer.write(Summary, summary);
    var index: usize = 0;
    while (index < writer.pending.items.len) : (index += 1) {
        const content = types.get(writer.pending.items[index]);
        writer.stats.types += 1;
        switch (content) {
            .named, .record, .tuple, .tag_union, .list, .box, .func => writer.stats.composites += 1,
            .primitive, .erased, .zst, .hole => {},
        }
        try writer.writeContent(content);
    }
    std.mem.writeInt(u32, writer.bytes.items[count_offset..][0..4], @intCast(writer.pending.items.len), .little);
    if (stats) |out| out.* = writer.stats;
    return writer.bytes.toOwnedSlice(allocator);
}

/// References in temporary decoded data name a request leaf or a table node,
/// never a live Type.Store slot. The complete record is validated before the
/// destination transaction starts. Its result becomes visible only after commit.
pub fn decode(arena: Allocator, types: *Type.Store, ns: *names.NameStore, leaves: []const Type.TypeId, bytes: []const u8, context: anytype) DecodeError!Summary {
    return decodeForRequest(arena, types, ns, leaves, bytes, context, null, true, null);
}

/// The request supplies its expected result arity and whether an unchanged
/// relation is meaningful; these are not inferred from persisted bytes.
pub fn decodeForRequest(arena: Allocator, types: *Type.Store, ns: *names.NameStore, leaves: []const Type.TypeId, bytes: []const u8, context: anytype, root_count: ?usize, allow_unchanged: bool, stats: ?*Stats) DecodeError!Summary {
    var scratch = std.heap.ArenaAllocator.init(types.allocator);
    defer scratch.deinit();
    var reader: Reader(@TypeOf(context)) = .{ .arena = scratch.allocator(), .names = ns, .leaves = leaves, .bytes = bytes, .context = context };
    if (!std.mem.startsWith(u8, reader.bytes, magic)) return invalidSummary(@src().line);
    reader.bytes = reader.bytes[magic.len..];
    reader.counts = try reader.read([3]u32);
    reader.type_count = try reader.read(u32);
    if (try reader.read(u32) != leaves.len) return invalidSummary(@src().line);
    if (reader.type_count > reader.bytes.len or leaves.len > std.math.maxInt(u32) - reader.type_count) return invalidSummary(@src().line);
    const summary = try reader.read(Summary);
    const nodes = try scratch.allocator().alloc(WireType, reader.type_count);
    for (nodes) |*node| {
        node.* = try reader.read(WireType);
        reader.stats.types += 1;
        switch (node.*) {
            .named, .record, .tuple, .tag_union, .list, .box, .func => reader.stats.composites += 1,
            .primitive, .erased, .zst, .hole => {},
        }
    }
    if (reader.bytes.len != 0) return invalidSummary(@src().line);
    switch (summary) {
        .unchanged => if (!allow_unchanged or !std.meta.eql(reader.counts, [3]u32{ 0, 0, 0 }) or nodes.len != 0) return invalidSummary(@src().line),
        .constraints => |c| {
            if (root_count) |expected| if (c.roots.len != expected) return invalidSummary(@src().line);
            if (c.nodes.len != reader.counts[0] or c.open_nodes.len != reader.counts[1] or c.kinds.len != reader.counts[2]) return invalidSummary(@src().line);
            for (c.nodes) |node| if (node == .open and node.open >= c.open_nodes.len) return invalidSummary(@src().line);
            for (c.open_nodes) |node| {
                if (node.content == .redirect or node.content == .leaf) return invalidSummary(@src().line);
                if (node.content == .unresolved and node.content.unresolved.origin == .interface_parameter and
                    (node.content.unresolved.numeric_default_phase != null or node.content.unresolved.row_default != null)) return invalidSummary(@src().line);
                if (node.related_group != .none and @backingInt(node.related_group) >= c.open_nodes.len) return invalidSummary(@src().line);
            }
            if (!try placeholdersAreFieldKindSlots(scratch.allocator(), c)) return invalidSummary(@src().line);
        },
    }
    try validateTypeGraph(scratch.allocator(), leaves.len, summary, nodes);
    const relocated = try scratch.allocator().alloc(Type.TypeId, leaves.len + nodes.len);
    @memcpy(relocated[0..leaves.len], leaves);
    const transaction = types.beginTransaction();
    errdefer transaction.abort(types);
    for (relocated[leaves.len..]) |*slot| slot.* = try transaction.reserve(types);
    var mapper = Relocation{ .types = types, .ids = relocated, .allocator = scratch.allocator() };
    for (nodes, relocated[leaves.len..]) |node, slot| transaction.fill(types, slot, try mapper.content(node));
    // Allocate every returned slice before commit. Only the two type-bearing
    // columns below need translating after the transaction interns its nodes.
    const result: Summary = switch (summary) {
        .unchanged => .unchanged,
        .constraints => |c| .{ .constraints = try c.copy(arena, mapper) },
    };
    if (nodes.len == 0) {
        transaction.abort(types);
        if (stats) |out| out.* = reader.stats;
        return result;
    }
    var committed = try types.commitTransaction(ns, transaction, relocated[leaves.len]);
    defer committed.deinit();
    if (result == .constraints) {
        for (@constCast(result.constraints.nodes)) |*node| if (node.* == .mono) {
            node.mono = committed.remapType(node.mono);
        };
        for (@constCast(result.constraints.open_nodes)) |*node| if (node.finished.get()) |ty| {
            node.finished = .of(committed.remapType(ty));
        };
    }
    if (stats) |out| out.* = reader.stats;
    return result;
}

/// Persist an explicitly declared immutable payload and its reachable type
/// closure. Payload slices are owned data, never references into a mutable
/// graph. The caller's schema tag versions all non-type fields. Unlike an
/// interface relation, this result has no request-relative type leaves.
pub fn encodeFrozen(comptime Payload: type, schema: [8]u8, allocator: Allocator, types: *Type.Store, ns: *const names.NameStore, payload: Payload, context: anytype, stats: ?*Stats) EncodeError![]u8 {
    var writer: Writer(@TypeOf(context)) = .{ .allocator = allocator, .types = types, .names = ns, .leaves = &.{}, .context = context };
    defer writer.bytes.deinit(allocator);
    defer writer.references.deinit(allocator);
    defer writer.pending.deinit(allocator);
    // No request leaves are indexed in this format.
    try writer.bytes.appendSlice(allocator, frozen_magic);
    try writer.write([8]u8, schema);
    const count_offset = writer.bytes.items.len;
    try writer.write(u32, 0);
    try writer.write(Payload, payload);
    var index: usize = 0;
    while (index < writer.pending.items.len) : (index += 1) {
        try writer.writeContent(types.get(writer.pending.items[index]));
    }
    writer.stats.types = writer.pending.items.len;
    std.mem.writeInt(u32, writer.bytes.items[count_offset..][0..4], @intCast(writer.pending.items.len), .little);
    if (stats) |out| out.* = writer.stats;
    return writer.bytes.toOwnedSlice(allocator);
}

/// Read completed immutable data. The context must validate all non-type
/// references through validateFrozenPayload before any type-store mutation.
/// That operation sees wire type references and must not dereference them in
/// the destination store. Returned slices belong to the supplied arena.
pub fn decodeFrozen(comptime Payload: type, schema: [8]u8, arena: Allocator, types: *Type.Store, ns: *names.NameStore, bytes: []const u8, context: anytype, stats: ?*Stats) DecodeError!Payload {
    var scratch = std.heap.ArenaAllocator.init(types.allocator);
    defer scratch.deinit();
    var reader: Reader(@TypeOf(context)) = .{ .arena = scratch.allocator(), .names = ns, .leaves = &.{}, .bytes = bytes, .context = context };
    if (!std.mem.startsWith(u8, reader.bytes, frozen_magic)) return invalidSummary(@src().line);
    reader.bytes = reader.bytes[frozen_magic.len..];
    if (!std.mem.eql(u8, &(try reader.read([8]u8)), &schema)) return invalidSummary(@src().line);
    reader.type_count = try reader.read(u32);
    if (reader.type_count > reader.bytes.len) return invalidSummary(@src().line);
    const payload = try reader.read(Payload);
    const nodes = try scratch.allocator().alloc(WireType, reader.type_count);
    for (nodes) |*node| node.* = try reader.read(WireType);
    if (reader.bytes.len != 0) return invalidSummary(@src().line);
    try context.validateFrozenPayload(payload);
    try validateTypeGraph(scratch.allocator(), 0, payload, nodes);
    // An annotation-only record has no Monotype closure to import. Its input
    // store may already be frozen; do not open a mutation transaction merely
    // to allocate the independent returned payload.
    if (nodes.len == 0) {
        if (stats) |out| out.* = reader.stats;
        return copyFrozen(Payload, arena, payload);
    }
    const relocated = try scratch.allocator().alloc(Type.TypeId, nodes.len);
    const transaction = types.beginTransaction();
    errdefer transaction.abort(types);
    for (relocated) |*slot| slot.* = try transaction.reserve(types);
    const mapper = Relocation{ .types = types, .ids = relocated, .allocator = scratch.allocator() };
    for (nodes, relocated) |node, slot| transaction.fill(types, slot, try mapper.content(node));
    var result = try copyFrozen(Payload, arena, payload);
    var committed = try types.commitTransaction(ns, transaction, relocated[0]);
    defer committed.deinit();
    // commitTransaction maps every reserved node, including roots not
    // reachable from the first root. This walk allocates nothing.
    for (relocated) |*slot| slot.* = committed.remapType(slot.*);
    relocateFrozen(Payload, &result, relocated);
    reader.stats.types = nodes.len;
    if (stats) |out| out.* = reader.stats;
    return result;
}

// These traversals follow only fixed-depth schema structure and owned slices;
// recursive user types are integer edges in the separately validated table.
fn copyFrozen(comptime T: type, arena: Allocator, value: T) Allocator.Error!T {
    return switch (@typeInfo(T)) {
        .@"struct" => |info| blk: {
            var out: T = undefined;
            inline for (info.field_names, info.field_types) |field, F| @field(out, field) = try copyFrozen(F, arena, @field(value, field));
            break :blk out;
        },
        .@"union" => |info| blk: {
            inline for (info.field_names, info.field_types) |field, F| if (std.meta.activeTag(value) == @field(info.tag_type.?, field)) break :blk @unionInit(T, field, try copyFrozen(F, arena, @field(value, field)));
            unreachable; // the loop above matches the active tag, which is one of the union's fields
        },
        .optional => |info| if (value) |v| try copyFrozen(info.child, arena, v) else null,
        .pointer => |info| blk: {
            if (info.size != .slice) @compileError("frozen payloads contain owned slices, not object pointers");
            const out = try arena.alloc(info.child, value.len);
            if (info.child == u8) {
                @memcpy(out, value);
            } else for (value, out) |v, *o| o.* = try copyFrozen(info.child, arena, v);
            break :blk out;
        },
        .array => |info| blk: {
            var out: T = undefined;
            for (value, &out) |v, *o| o.* = try copyFrozen(info.child, arena, v);
            break :blk out;
        },
        .@"enum", .int, .float, .bool, .void => value,
        .type,
        .noreturn,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("unsupported frozen payload field: " ++ @typeName(T)),
    };
}

fn relocateFrozen(comptime T: type, value: anytype, ids: []const Type.TypeId) void {
    if (T == Type.TypeId) {
        value.* = ids[@backingInt(value.*)];
        return;
    }
    if (T == Type.MaybeTypeId) {
        if (value.get()) |ty| value.* = .of(ids[@backingInt(ty)]);
        return;
    }
    switch (@typeInfo(T)) {
        .@"struct" => |info| inline for (info.field_names, info.field_types) |field, F| relocateFrozen(F, &@field(value, field), ids),
        .@"union" => |info| inline for (info.field_names, info.field_types) |field, F| {
            if (std.meta.activeTag(value.*) == @field(info.tag_type.?, field)) return relocateFrozen(F, &@field(value, field), ids);
        },
        .optional => |info| if (value.*) |*v| relocateFrozen(info.child, v, ids),
        .pointer => |info| {
            if (info.size != .slice) @compileError("frozen payloads contain owned slices, not object pointers");
            for (@constCast(value.*)) |*v| relocateFrozen(info.child, v, ids);
        },
        .array => |info| for (value) |*v| relocateFrozen(info.child, v, ids),
        .@"enum", .int, .float, .bool, .void => {},
        .type,
        .noreturn,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("unsupported frozen payload field: " ++ @typeName(T)),
    }
}

// This wire graph deliberately has no arena spans or producer indices. Its
// nesting has fixed schema depth; all user-controlled recursion uses TypeId.
const WireTag = struct { name: names.TagNameId, checked_name: names.TagNameId, payloads: []const Type.TypeId };
const WireNamed = struct {
    named_type: Type.NamedType,
    def: Type.TypeDef,
    kind: Type.NamedKind,
    builtin_owner: ?check.StaticDispatchRegistry.BuiltinOwner,
    args: []const Type.TypeId,
    backing: ?Type.NamedBacking,
    declared_order: []const Type.DeclaredField,
};
const WireType = union(enum(u8)) {
    primitive: Type.Primitive,
    named: WireNamed,
    record: []const Type.Field,
    tuple: []const Type.TypeId,
    tag_union: []const WireTag,
    list: Type.TypeId,
    box: Type.TypeId,
    func: struct { args: []const Type.TypeId, ret: Type.TypeId },
    erased: names.TypeDigest,
    zst,
    hole,
};

// All traversal here is over explicit integer edges. Schema reflection only
// visits the fixed-depth wire records; it never follows a type graph edge.
const TypeEdges = struct {
    allocator: Allocator,
    edges: std.ArrayList(Type.TypeId) = .empty,
    fn collect(self: *TypeEdges, comptime T: type, value: T) Allocator.Error!void {
        if (T == Type.TypeId) return self.edges.append(self.allocator, value);
        if (T == Type.MaybeTypeId) {
            if (value.get()) |ty| try self.edges.append(self.allocator, ty);
            return;
        }
        switch (@typeInfo(T)) {
            .@"struct" => |info| inline for (info.field_names, info.field_types) |field, F| try self.collect(F, @field(value, field)),
            .@"union" => |info| inline for (info.field_names, info.field_types) |field, F| {
                if (std.meta.activeTag(value) == @field(info.tag_type.?, field)) return self.collect(F, @field(value, field));
            },
            .optional => |info| if (value) |v| {
                try self.collect(info.child, v);
            },
            .pointer => |info| switch (info.size) {
                .one => try self.collect(info.child, value.*),
                .slice => for (value) |v| try self.collect(info.child, v),
                .many, .c => @compileError("unbounded summary pointer"),
            },
            .array => |info| for (value) |v| try self.collect(info.child, v),
            .@"enum", .int, .float, .bool, .void => {},
            .type,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .spirv,
            => @compileError("unsupported summary graph field"),
        }
    }
};

fn validateTypeGraph(allocator: Allocator, leaf_count: usize, summary: anytype, nodes: []const WireType) DecodeError!void {
    var edges: TypeEdges = .{ .allocator = allocator };
    defer edges.edges.deinit(allocator);
    try edges.collect(@TypeOf(summary), summary);
    const roots_end = edges.edges.items.len;
    const starts = try allocator.alloc(usize, nodes.len + 1);
    defer allocator.free(starts);
    for (nodes, 0..) |node, i| {
        starts[i] = edges.edges.items.len;
        try edges.collect(WireType, node);
    }
    starts[nodes.len] = edges.edges.items.len;
    const seen = try allocator.alloc(bool, nodes.len);
    defer allocator.free(seen);
    @memset(seen, false);
    var queue = std.ArrayList(Type.TypeId).empty;
    defer queue.deinit(allocator);
    try queue.appendSlice(allocator, edges.edges.items[0..roots_end]);
    var head: usize = 0;
    while (head < queue.items.len) : (head += 1) {
        const ref = @backingInt(queue.items[head]);
        if (ref < leaf_count) continue;
        const index = ref - leaf_count;
        if (seen[index]) continue;
        seen[index] = true;
        try queue.appendSlice(allocator, edges.edges.items[starts[index]..starts[index + 1]]);
    }
    for (seen) |reachable| if (!reachable) return invalidSummary(@src().line);

    // Checking rejects alias-only cycles. A recursive type must cross a real
    // constructor, so an unguarded alias cycle is corrupt external input.
    const colors = try allocator.alloc(enum { unseen, active, done }, nodes.len);
    defer allocator.free(colors);
    @memset(colors, .unseen);
    for (nodes, 0..) |_, start| {
        var index = start;
        while (colors[index] == .unseen) {
            colors[index] = .active;
            const node = nodes[index];
            if (node != .named or node.named.kind != .alias) break;
            const backing = node.named.backing orelse break;
            const ref = @backingInt(backing.ty);
            if (ref < leaf_count) break;
            index = ref - leaf_count;
            if (colors[index] == .active) return invalidSummary(@src().line);
        }
        index = start;
        while (colors[index] == .active) {
            colors[index] = .done;
            const node = nodes[index];
            if (node != .named or node.named.kind != .alias) break;
            const backing = node.named.backing orelse break;
            const ref = @backingInt(backing.ty);
            if (ref < leaf_count) break;
            index = ref - leaf_count;
        }
    }
}

const Relocation = struct {
    types: *Type.Store,
    ids: []const Type.TypeId,
    allocator: Allocator,

    pub fn mapScalar(self: Relocation, comptime T: type, value: T) Allocator.Error!T {
        if (T == Type.TypeId) return self.ids[@backingInt(value)];
        return value;
    }
    fn ty(self: Relocation, value: Type.TypeId) Type.TypeId {
        return self.ids[@backingInt(value)];
    }
    fn span(self: Relocation, values: []const Type.TypeId) Allocator.Error!Type.Span {
        const mapped = try self.allocator.alloc(Type.TypeId, values.len);
        for (values, mapped) |value, *out| out.* = self.ty(value);
        return self.types.addSpan(mapped);
    }
    fn content(self: Relocation, node: WireType) Allocator.Error!Type.Content {
        return switch (node) {
            .primitive => |v| .{ .primitive = v },
            .erased => |v| .{ .erased = v },
            .zst => .zst,
            .hole => .hole,
            .list => |v| .{ .list = self.ty(v) },
            .box => |v| .{ .box = self.ty(v) },
            .tuple => |v| .{ .tuple = try self.span(v) },
            .func => |v| .{ .func = .{ .args = try self.span(v.args), .ret = self.ty(v.ret) } },
            .record => |v| blk: {
                const fields = try self.allocator.dupe(Type.Field, v);
                for (fields) |*field| {
                    field.ty = self.ty(field.ty);
                    if (field.value_ty.get()) |value| field.value_ty = .of(self.ty(value));
                }
                break :blk .{ .record = try self.types.addFields(fields) };
            },
            .tag_union => |v| blk: {
                const tags = try self.allocator.alloc(Type.Tag, v.len);
                for (v, tags) |tag, *out| out.* = .{ .name = tag.name, .checked_name = tag.checked_name, .payloads = try self.span(tag.payloads) };
                break :blk .{ .tag_union = try self.types.addTags(tags) };
            },
            .named => |v| blk: {
                const declared = try self.allocator.dupe(Type.DeclaredField, v.declared_order);
                for (declared) |*field| if (field.* == .padding) {
                    field.padding = self.ty(field.padding);
                };
                var backing = v.backing;
                if (backing) |*b| b.ty = self.ty(b.ty);
                break :blk .{ .named = .{ .named_type = v.named_type, .def = v.def, .kind = v.kind, .builtin_owner = v.builtin_owner, .args = try self.span(v.args), .backing = backing, .declared_order = try self.types.addDeclaredFields(declared) } };
            },
        };
    }
};

/// Procedure ordinals, captured input and context-qualified evidence identity.
/// Checked callable-type ordinals are provenance; producer-issued callable keys
/// carry their relation, as in `Ast.fnEvidenceDigest`.
pub fn requestKey(request: Constraints.Identity, evidence: [32]u8, procedure: [8]u8) [32]u8 {
    var hash = base.Sha256.init(.{});
    hash.update("roc.interface-request.v3");
    hash.update(&procedure);
    hash.update(request.bytes);
    hash.update(&evidence);
    const result = hash.finalResult();
    if (base.dev_diagnostics.isSet(.pack_trace)) std.debug.print("interface request key={x} evidence={x} leaves={d}\n", .{ result, evidence, request.leaves.len });
    return result;
}

fn Writer(comptime Context: type) type {
    return struct {
        const Self = @This();
        context: Context,
        stats: Stats = .{},
        references: collections.DenseMapUnmanaged(Type.TypeId, u32) = .empty,
        pending: std.ArrayList(Type.TypeId) = .empty,
        leaf_buckets: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
        leaf_next: std.ArrayList(?u32) = .empty,
        allocator: Allocator,
        types: *Type.Store,
        names: *const names.NameStore,
        leaves: []const Type.TypeId,
        bytes: std.ArrayList(u8) = .empty,
        checked_names: bool = false,

        fn indexLeaves(self: *Self) EncodeError!void {
            try self.leaf_next.resize(self.allocator, self.leaves.len);
            @memset(self.leaf_next.items, null);
            // One representative per representation/equality class, always
            // the first request position, even when checked occurrences have
            // different process-local TypeIds.
            for (self.leaves, 0..) |leaf, index| {
                if (self.references.contains(leaf)) continue;
                const digest = try self.types.representationDigestFallible(self.names, leaf);
                var candidate = self.leaf_buckets.get(digest.bytes);
                var representative: ?u32 = null;
                while (candidate) |other| : (candidate = self.leaf_next.items[other]) {
                    if (try self.types.typeEql(self.names, self.leaves[other], leaf)) {
                        representative = other;
                        break;
                    }
                }
                if (representative) |other| {
                    try self.references.put(self.allocator, leaf, other);
                } else {
                    const entry = try self.leaf_buckets.getOrPut(self.allocator, digest.bytes);
                    self.leaf_next.items[index] = if (entry.found_existing) entry.value_ptr.* else null;
                    entry.value_ptr.* = @intCast(index);
                    try self.references.put(self.allocator, leaf, @intCast(index));
                }
            }
        }
        fn reference(self: *Self, value: Type.TypeId) EncodeError!u32 {
            if (self.references.get(value)) |ref| return ref;
            if (self.leaves.len != 0) {
                const digest = try self.types.representationDigestFallible(self.names, value);
                var candidate = self.leaf_buckets.get(digest.bytes);
                while (candidate) |index| : (candidate = self.leaf_next.items[index]) {
                    if (try self.types.typeEql(self.names, self.leaves[index], value)) {
                        try self.references.put(self.allocator, value, index);
                        return index;
                    }
                }
            }
            const ref = std.math.cast(u32, self.leaves.len + self.pending.items.len) orelse return error.NotRepresentable;
            try self.references.put(self.allocator, value, ref);
            try self.pending.append(self.allocator, value);
            return ref;
        }
        fn writeSpan(self: *Self, comptime T: type, borrow: anytype) EncodeError!void {
            try self.write(u32, @intCast(GuardedList.borrowLen(borrow)));
            for (0..GuardedList.borrowLen(borrow)) |index| try self.write(T, GuardedList.at(borrow, index));
        }
        fn writeContent(self: *Self, content: Type.Content) EncodeError!void {
            // The wire tag order is explicit and versioned with this payload.
            switch (content) {
                .primitive => |v| try self.write(WireType, .{ .primitive = v }),
                .erased => |v| try self.write(WireType, .{ .erased = v }),
                .zst => try self.write(WireType, .zst),
                .hole => try self.write(WireType, .hole),
                .list => |v| try self.write(WireType, .{ .list = v }),
                .box => |v| try self.write(WireType, .{ .box = v }),
                .tuple => |v| {
                    try self.write(std.meta.Tag(WireType), .tuple);
                    try self.writeSpan(Type.TypeId, self.types.span(v));
                },
                .func => |v| {
                    try self.write(std.meta.Tag(WireType), .func);
                    try self.writeSpan(Type.TypeId, self.types.span(v.args));
                    try self.write(Type.TypeId, v.ret);
                },
                .record => |v| {
                    try self.write(std.meta.Tag(WireType), .record);
                    try self.writeSpan(Type.Field, self.types.fieldSpan(v));
                },
                .tag_union => |v| {
                    try self.write(std.meta.Tag(WireType), .tag_union);
                    const tags = self.types.tagSpan(v);
                    try self.write(u32, @intCast(GuardedList.borrowLen(tags)));
                    for (0..GuardedList.borrowLen(tags)) |index| {
                        const tag = GuardedList.at(tags, index);
                        try self.write(names.TagNameId, tag.name);
                        try self.write(names.TagNameId, tag.checked_name);
                        try self.writeSpan(Type.TypeId, self.types.span(tag.payloads));
                    }
                },
                .named => |v| {
                    try self.write(std.meta.Tag(WireType), .named);
                    try self.write(Type.NamedType, v.named_type);
                    try self.write(Type.TypeDef, v.def);
                    try self.write(Type.NamedKind, v.kind);
                    try self.write(@TypeOf(v.builtin_owner), v.builtin_owner);
                    try self.writeSpan(Type.TypeId, self.types.span(v.args));
                    try self.write(?Type.NamedBacking, v.backing);
                    try self.writeSpan(Type.DeclaredField, self.types.declaredFieldSpan(v.declared_order));
                },
            }
        }

        fn write(self: *Self, comptime T: type, value: T) EncodeError!void {
            const previous = self.checked_names;
            if (T == check.ConstStore.ConstFnTargetEvidence or T == check.ConstStore.ConstFnStructuralEvidence) self.checked_names = true;
            defer self.checked_names = previous;
            return self.writeValue(T, value);
        }
        fn writeValue(self: *Self, comptime T: type, value: T) EncodeError!void {
            if (T == names.ExportNameId) if (!self.checked_names) return self.write([]const u8, self.names.exportNameText(value));
            if (T == names.ExternalSymbolNameId) if (!self.checked_names) return self.write([]const u8, self.context.builder.program.names.externalSymbolNameText(value));
            if (T == Type.NamedType) return self.write(NamedOccurrence, self.context.describeNamedType(value));
            if (T == Type.TypeId) {
                self.stats.references += 1;
                return self.write(u32, try self.reference(value));
            }
            if (T == Solve.InstNode) {
                // A placeholder is checked against the summary's field kinds
                // before writing (`placeholdersAreFieldKindSlots`).
                if (value == .redirect or value == .leaf) invariant("settled interface contains unfinished solver state", .{});
            }
            if (T == Type.MaybeTypeId) return self.write(?Type.TypeId, value.get());
            if (T == names.ModuleIdentityId) if (!self.checked_names) return self.write([32]u8, self.names.moduleIdentityBytes(value).*);
            if (T == names.TypeNameId) if (!self.checked_names) return self.write([]const u8, self.names.typeNameText(value));
            if (T == names.RecordFieldNameId) if (!self.checked_names) return self.write([]const u8, self.names.recordFieldLabelText(value));
            if (T == names.TagNameId) if (!self.checked_names) return self.write([]const u8, self.names.tagLabelText(value));
            if (T == Type.FieldDefault) {
                self.stats.defaults += 1;
                const identity = self.names.moduleIdentityBytes(value.module).*;
                const declaring_module = self.context.defaultDeclaringModule(identity, value.expr_node) orelse
                    invariant("committed field default has no declaring checked module or archived expression", .{});
                try self.write([32]u8, identity);
                try self.write([32]u8, declaring_module);
                return self.write(u32, value.expr_node);
            }
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
                        if (info.child == u8) {
                            try self.bytes.appendSlice(self.allocator, value);
                        } else for (value) |v| try self.write(info.child, v);
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
                        try self.bytes.append(self.allocator, @truncate(@as(@Int(.unsigned, @max(8, @bitSizeOf(T))), bits)));
                        bits = std.math.shr(U, bits, 8);
                    }
                },
                .float => try self.write(@Int(.unsigned, @bitSizeOf(T)), @bitCast(value)),
                .void => {},
                .type, .noreturn, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported summary field: " ++ @typeName(T)),
            }
        }
    };
}

fn Reader(comptime Context: type) type {
    return struct {
        const Self = @This();
        arena: Allocator,
        names: *names.NameStore,
        leaves: []const Type.TypeId,
        bytes: []const u8,
        context: Context,
        stats: Stats = .{},
        counts: [3]u32 = .{ 0, 0, 0 },
        type_count: u32 = 0,
        checked_names: bool = false,

        fn read(self: *Self, comptime T: type) DecodeError!T {
            const previous = self.checked_names;
            if (T == check.ConstStore.ConstFnTargetEvidence or T == check.ConstStore.ConstFnStructuralEvidence) self.checked_names = true;
            defer self.checked_names = previous;
            return self.readValue(T);
        }
        fn readValue(self: *Self, comptime T: type) DecodeError!T {
            if (T == names.ExportNameId) if (!self.checked_names) return self.names.internExportName(try self.read([]const u8));
            if (T == names.ExternalSymbolNameId) if (!self.checked_names) return self.names.internExternalSymbolName(try self.read([]const u8));

            if (T == Type.NamedType) {
                const named = (try self.context.resolveNamedType(try self.read(NamedOccurrence))) orelse return invalidSummary(@src().line);
                if (!self.context.validNamedType(named)) return invalidSummary(@src().line);
                return named;
            }
            if (T == Type.TypeId) {
                self.stats.references += 1;
                const index = try self.read(u32);
                if (index >= self.leaves.len + self.type_count) return invalidSummary(@src().line);
                return @fromBackingInt(index);
            }
            if (T == Type.MaybeTypeId) return .of(try self.read(?Type.TypeId));
            if (T == names.ModuleIdentityId and !self.checked_names) {
                const digest = try self.read([32]u8);
                if (!self.context.validModuleIdentity(digest)) return invalidSummary(@src().line);
                return self.names.internModuleIdentity(&digest);
            }
            if (T == names.TypeNameId) if (!self.checked_names) return self.names.internTypeName(try self.read([]const u8));
            if (T == names.RecordFieldNameId) if (!self.checked_names) return self.names.internRecordFieldLabel(try self.read([]const u8));
            if (T == names.TagNameId) if (!self.checked_names) return self.names.internTagLabel(try self.read([]const u8));
            if (T == Type.FieldDefault) {
                self.stats.defaults += 1;
                const identity = try self.read([32]u8);
                const declaring_module = try self.read([32]u8);
                const expr = try self.read(u32);
                const current = self.context.defaultDeclaringModule(identity, expr) orelse return invalidSummary(@src().line);
                if (!std.mem.eql(u8, &declaring_module, &current)) return invalidSummary(@src().line);
                return .{ .module = try self.names.internModuleIdentity(&identity), .expr_node = expr };
            }
            if (T == Solve.NodeId or T == Solve.MaybeNodeId or T == Solve.FieldKindId) {
                const index = try self.read(u32);
                if (T == Solve.MaybeNodeId and index == std.math.maxInt(u32)) return @fromBackingInt(index);
                if (index >= self.counts[if (T == Solve.FieldKindId) 2 else 0]) return invalidSummary(@src().line);
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
                    return invalidSummary(@src().line);
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
                    if (len > self.bytes.len) return invalidSummary(@src().line);
                    const values = try self.arena.alloc(info.child, len);
                    if (info.child == u8) {
                        @memcpy(values, self.bytes[0..len]);
                        self.bytes = self.bytes[len..];
                    } else for (values) |*value| value.* = try self.read(info.child);
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
                        return invalidSummary(@src().line);
                    }
                    break :blk @fromBackingInt(value);
                },
                .bool => switch (try self.read(u8)) {
                    0 => false,
                    1 => true,
                    else => return invalidSummary(@src().line),
                },
                .int => blk: {
                    const U = @Int(.unsigned, @bitSizeOf(T));
                    const size = (@bitSizeOf(T) + 7) / 8;
                    if (size > self.bytes.len) return invalidSummary(@src().line);
                    if (@bitSizeOf(T) % 8 != 0 and self.bytes[size - 1] >= (@as(u16, 1) << (@bitSizeOf(T) % 8))) return invalidSummary(@src().line);
                    var value: U = 0;
                    for (self.bytes[0..size], 0..) |b, i| value |= std.math.shl(U, @as(U, @truncate(b)), i * 8);
                    self.bytes = self.bytes[size..];
                    break :blk @bitCast(value);
                },
                .float => @bitCast(try self.read(@Int(.unsigned, @bitSizeOf(T)))),
                .void => {},
                .type, .noreturn, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported summary field: " ++ @typeName(T)),
            };
            if (T == Type.NamedType) if (!self.context.validNamedType(result)) return invalidSummary(@src().line);
            if (T == WireNamed or T == @typeInfo(@FieldType(Solve.InstNode, "named")).pointer.child or T == Solve.InstIteratorPublicSource) {
                if (!self.context.validNamedDefinition(self.names, result.named_type, result.def, result.kind, result.builtin_owner)) return invalidSummary(@src().line);
            }
            if (T == WireType) switch (result) {
                .record => |fields| {
                    for (fields, 0..) |field, i| {
                        if (i > 0 and std.mem.order(u8, self.names.recordFieldLabelText(fields[i - 1].name), self.names.recordFieldLabelText(field.name)) != .lt) return invalidSummary(@src().line);
                        if (field.kind_state == .undetermined and (field.value_ty.get() == null or field.ty != field.value_ty.get().? or field.default != null)) return invalidSummary(@src().line);
                    }
                },
                .tag_union => |tags| for (tags, 0..) |tag, i| {
                    if (i > 0 and std.mem.order(u8, self.names.tagLabelText(tags[i - 1].name), self.names.tagLabelText(tag.name)) != .lt) return invalidSummary(@src().line);
                },
                .primitive, .named, .tuple, .list, .box, .func, .erased, .zst, .hole => {},
            };
            return result;
        }
    };
}

const TestProvenance = struct {
    declaring_module: [32]u8 = @splat(7),
    available: bool = true,
    closed_key: ?[32]u8 = null,
    occurrence_version: u8 = 0,
    pub fn defaultDeclaringModule(self: @This(), identity: [32]u8, expr: u32) ?[32]u8 {
        if (!self.available or identity[0] != 1 or expr > 2) return null;
        return self.declaring_module;
    }
    pub fn validModuleIdentity(self: @This(), identity: [32]u8) bool {
        return self.available and identity[0] == 1;
    }
    pub fn describeNamedType(self: @This(), named: Type.NamedType) NamedOccurrence {
        if (self.closed_key) |key| return .{ .closed = .{ .owner = named.module.bytes, .key = key } };
        return .{ .exact = .{ .owner = named.module.bytes, .occurrence = named.ty_module.bytes, .ty = @backingInt(named.ty) } };
    }
    pub fn resolveNamedType(self: @This(), occurrence: NamedOccurrence) Allocator.Error!?Type.NamedType {
        return switch (occurrence) {
            .input => null,
            .exact => |exact| .{ .module = .{ .bytes = exact.owner }, .ty_module = .{ .bytes = exact.occurrence }, .ty = @fromBackingInt(exact.ty) },
            .closed => |closed| blk: {
                const key = self.closed_key orelse break :blk null;
                if (!std.mem.eql(u8, &key, &closed.key)) break :blk null;
                var module: [32]u8 = @splat(0);
                module[1] = self.occurrence_version;
                break :blk .{ .module = .{ .bytes = closed.owner }, .ty_module = .{ .bytes = module }, .ty = @fromBackingInt(1) };
            },
        };
    }
    pub fn validNamedType(self: @This(), named: Type.NamedType) bool {
        return self.available and named.module.bytes[0] == 0 and named.ty_module.bytes[0] == 0 and @backingInt(named.ty) == 1;
    }
    pub fn validNamedDefinition(self: @This(), _: *const names.NameStore, named: Type.NamedType, _: Type.TypeDef, _: Type.NamedKind, _: ?check.StaticDispatchRegistry.BuiltinOwner) bool {
        return self.validNamedType(named);
    }
    pub fn validateFrozenPayload(_: @This(), payload: FrozenTestPayload) DecodeError!void {
        for (payload.edges) |edge| if (edge >= payload.types.len) return error.InvalidSummary;
    }
};

const FrozenTestPayload = struct {
    types: []const Type.TypeId,
    edges: []const u32,
    name: names.RecordFieldNameId,
    data: []const u8,
    integer: i128,
    float: f64,
    optional: Type.MaybeTypeId,
};

fn frozenTestBytes(allocator: Allocator) error{ NotRepresentable, OutOfMemory }![]u8 {
    var ns = names.NameStore.init(allocator);
    defer ns.deinit();
    var source = Type.Store.init(allocator);
    defer source.deinit();
    const slot = try source.beginRecursive();
    const function = try source.add(.{ .func = .{ .args = .empty(), .ret = slot.ty } });
    const name = try ns.internRecordFieldLabel("step");
    source.finishRecursive(slot, .{ .record = try source.addFields(&.{.{ .name = name, .ty = function, .default = null }}) });
    // A disconnected root tests that committing the first root also retains
    // every other explicitly referenced result. The repeated root tests sharing.
    const primitive = try source.internPrimitive(&ns, .u64);
    return encodeFrozen(FrozenTestPayload, "TEST0001".*, allocator, &source, &ns, .{
        .types = &.{ slot.ty, slot.ty, primitive },
        .edges = &.{ 0, 1, 2 },
        .name = name,
        .data = "owned bytes\x00\xff",
        .integer = std.math.minInt(i128) + 17,
        .float = @bitCast(@as(u64, 0x7ff8000000000053)),
        .optional = .of(function),
    }, TestProvenance{}, null);
}

test "frozen specialization payload owns data and relocates cyclic shared type roots" {
    const gpa = std.testing.allocator;
    const bytes = try frozenTestBytes(gpa);
    defer gpa.free(bytes);
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    _ = try ns.internRecordFieldLabel("unrelated");
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    _ = try dest.internPrimitive(&ns, .bool);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const result = try decodeFrozen(FrozenTestPayload, "TEST0001".*, arena.allocator(), &dest, &ns, bytes, TestProvenance{}, null);
    try std.testing.expectEqual(result.types[0], result.types[1]);
    const field = GuardedList.at(dest.fieldSpan(dest.get(result.types[0]).record), 0);
    try std.testing.expectEqual(result.types[0], dest.get(field.ty).func.ret);
    try std.testing.expectEqual(field.ty, result.optional.get().?);
    try std.testing.expectEqual(Type.Primitive.u64, dest.get(result.types[2]).primitive);
    try std.testing.expectEqualStrings("step", ns.recordFieldLabelText(result.name));
    try std.testing.expectEqualStrings("owned bytes\x00\xff", result.data);
    try std.testing.expectEqual(std.math.minInt(i128) + 17, result.integer);
    try std.testing.expectEqual(@as(u64, 0x7ff8000000000053), @as(u64, @bitCast(result.float)));
}

test "frozen specialization payload rejects corrupt schema references and truncation before mutation" {
    const gpa = std.testing.allocator;
    const bytes = try frozenTestBytes(gpa);
    defer gpa.free(bytes);
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    _ = try dest.internPrimitive(&ns, .bool);
    const before = dest.types.len();
    for (0..bytes.len) |len| {
        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();
        try std.testing.expectError(error.InvalidSummary, decodeFrozen(FrozenTestPayload, "TEST0001".*, arena.allocator(), &dest, &ns, bytes[0..len], TestProvenance{}, null));
        try std.testing.expectEqual(before, dest.types.len());
        try std.testing.expect(!dest.hasSpeculativeConstruction());
    }
    const bad = try gpa.dupe(u8, bytes);
    defer gpa.free(bad);
    // Schema byte, type reference, and payload-local edge, respectively.
    for ([_]usize{ 8, 24, 40 }) |offset| {
        @memcpy(bad, bytes);
        bad[offset] = 255;
        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();
        try std.testing.expectError(error.InvalidSummary, decodeFrozen(FrozenTestPayload, "TEST0001".*, arena.allocator(), &dest, &ns, bad, TestProvenance{}, null));
        try std.testing.expectEqual(before, dest.types.len());
        try std.testing.expect(!dest.hasSpeculativeConstruction());
    }
}

fn frozenAllocationRoundTrip(gpa: Allocator) error{ InvalidSummary, NotRepresentable, OutOfMemory, TestExpectedEqual, TestUnexpectedResult }!void {
    const bytes = try frozenTestBytes(gpa);
    defer gpa.free(bytes);
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    _ = try dest.internPrimitive(&ns, .bool);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const before = dest.types.len();
    _ = decodeFrozen(FrozenTestPayload, "TEST0001".*, arena.allocator(), &dest, &ns, bytes, TestProvenance{}, null) catch |err| {
        try std.testing.expectEqual(before, dest.types.len());
        try std.testing.expect(!dest.hasSpeculativeConstruction());
        return err;
    };
}

test "frozen specialization payload allocation failures roll back construction" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, frozenAllocationRoundTrip, .{});
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
    // Node slot 0 is the open function, which returns itself; slot 1 is its
    // U64 argument leaf.
    const function_slot: u32 = 0;
    const argument_slot: u32 = 1;
    const input: Summary = .{ .constraints = .{
        .roots = &.{@fromBackingInt(function_slot)},
        .nodes = &.{ .{ .open = 0 }, .{ .mono = source_ty } },
        .open_nodes = &.{.{ .content = .{ .func = .{ .args = @constCast(&[_]Solve.NodeId{@fromBackingInt(argument_slot)}), .ret = @fromBackingInt(function_slot) } } }},
        .kinds = &.{},
    } };
    const bytes = try encode(allocator, &source, &ns, &.{source_ty}, input, TestProvenance{});
    defer allocator.free(bytes);
    const result = (try decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bytes, TestProvenance{})).constraints;
    try std.testing.expectEqual(destination_ty, result.nodes[1].mono);
    try std.testing.expectEqual(@as(u32, 0), @backingInt(result.open_nodes[0].content.func.ret));
    for (0..bytes.len) |len| try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bytes[0..len], TestProvenance{}));
    const bad = try allocator.dupe(u8, bytes);
    defer allocator.free(bad);
    // Header declares no node slots, but the root still references slot zero.
    @memset(bad[magic.len..][0..4], 0);
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &destination, &ns, &.{destination_ty}, bad, TestProvenance{}));
}

test "persistent interface leaves preserve representation beyond semantic equality" {
    const allocator = std.testing.allocator;
    var ns = names.NameStore.init(allocator);
    defer ns.deinit();
    var types = Type.Store.init(allocator);
    defer types.deinit();
    const u64_ty = try types.internPrimitive(&ns, .u64);
    const alias = try types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @fromBackingInt(1), .ty_module = .{} },
        .def = .{ .module = try ns.internModuleIdentity(&@as([32]u8, @splat(1))), .type_name = try ns.internTypeName("Alias") },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = .{ .ty = u64_ty, .use = .inspectable },
    } });
    try std.testing.expect(try types.typeEql(&ns, alias, u64_ty));
    // Node slot 0 is the alias leaf.
    const alias_slot: u32 = 0;
    const summary: Summary = .{ .constraints = .{
        .roots = &.{@fromBackingInt(alias_slot)},
        .nodes = &.{.{ .mono = alias }},
        .open_nodes = &.{},
        .kinds = &.{},
    } };
    const bytes = try encode(allocator, &types, &ns, &.{u64_ty}, summary, TestProvenance{});
    defer allocator.free(bytes);
    var destination = Type.Store.init(allocator);
    defer destination.deinit();
    var destination_names = names.NameStore.init(allocator);
    defer destination_names.deinit();
    _ = try destination_names.internTypeName("unrelated");
    _ = try destination.internPrimitive(&destination_names, .bool);
    const input = try destination.internPrimitive(&destination_names, .u64);
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const result = try decode(arena.allocator(), &destination, &destination_names, &.{input}, bytes, TestProvenance{});
    const restored = result.constraints.nodes[0].mono;
    try std.testing.expect(restored != input);
    try std.testing.expectEqual(types.representationDigestCached(&ns, alias, null), destination.representationDigestCached(&destination_names, restored, null));
    try std.testing.expectEqual(Type.NamedKind.alias, destination.get(restored).named.kind);
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
        .method = .{ .module_idx = 0, .def_idx = @fromBackingInt(1), .kind = .{ .structural = .parser }, .callable_ty = @fromBackingInt(2) },
        .method_callable_key = .{ .bytes = @splat(4) },
        .instantiation = .{ .view = .{ .bytes = first }, .callable_key = .{ .bytes = @splat(5) }, .callable_ty = @fromBackingInt(3) },
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
        .proc = .{ .artifact = .{ .bytes = first }, .proc_base = @fromBackingInt(1) },
        .template = .{ .artifact = .{ .bytes = first }, .proc_base = @fromBackingInt(1), .template = @fromBackingInt(2) },
    } };
    const procedure = Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts);
    targets[0].method.kind.procedure.template.artifact.bytes = second;
    try std.testing.expect(!std.meta.eql(procedure, Ast.fnEvidenceContextDigest(evidence, &.{}, null, &contexts)));
}

/// The root naming the node at `index` of a test summary's `nodes`.
fn testNodeRoot(index: u32) Solve.NodeId {
    return @fromBackingInt(index);
}

fn typeSummary(ty: Type.TypeId, nodes: *[1]Constraints.Node) Summary {
    nodes.* = .{.{ .mono = ty }};
    return .{ .constraints = .{ .roots = &.{comptime testNodeRoot(0)}, .nodes = nodes, .open_nodes = &.{}, .kinds = &.{} } };
}

test "persistent interface composite graph preserves sharing defaults and representation metadata" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var source = Type.Store.init(gpa);
    defer source.deinit();
    const module = try ns.internModuleIdentity(&@as([32]u8, @splat(1)));
    const label = try ns.internRecordFieldLabel("value");
    const primitive = try source.internPrimitive(&ns, .u64);
    const boxed = try source.add(.{ .box = primitive });
    const list = try source.add(.{ .list = boxed });
    const erased = try source.add(.{ .erased = .{ .bytes = @splat(3) } });
    const hole = try source.add(.hole);
    const zst = try source.add(.zst);
    const tuple = try source.add(.{ .tuple = try source.addSpan(&.{ list, list, erased, hole, zst }) });
    const tag = try source.add(.{ .tag_union = try source.addTags(&.{.{ .name = try ns.internTagLabel("Some"), .checked_name = try ns.internTagLabel("Checked.Some"), .payloads = try source.addSpan(&.{tuple}) }}) });
    const field: Type.Field = .{ .name = label, .ty = tag, .default = .{ .module = module, .expr_node = 1 } };
    const record = try source.add(.{ .record = try source.addFields(&.{field}) });
    const provisional = try source.add(.{ .record = try source.addFields(&.{.{ .name = label, .ty = primitive, .value_ty = .of(primitive), .kind_state = .undetermined, .default = null }}) });
    const definition: Type.TypeDef = .{
        .module = module,
        .type_name = try ns.internTypeName("Iter"),
        .source_decl = 12,
        .generated = .{ .bytes = @splat(4) },
        .iterator_representation = .minted,
        .iterator_kind = .list,
        .iterator_depth = 2,
        .iterator_topology = .{
            .len_field = label,
            .step_field = label,
            .known_tag = try ns.internTagLabel("Known"),
            .unknown_tag = try ns.internTagLabel("Unknown"),
            .done_tag = try ns.internTagLabel("Done"),
            .one_tag = try ns.internTagLabel("One"),
            .skip_tag = try ns.internTagLabel("Skip"),
            .item_field = label,
            .rest_field = label,
        },
    };
    var named_types: [3]Type.TypeId = undefined;
    for ([_]Type.NamedKind{ .nominal, .alias, .@"opaque" }, &named_types) |kind, *out| {
        out.* = try source.add(.{ .named = .{
            .named_type = .{ .module = .{}, .ty = @fromBackingInt(1), .ty_module = .{} },
            .def = definition,
            .kind = kind,
            .args = try source.addSpan(&.{primitive}),
            .backing = .{ .ty = record, .use = if (kind == .@"opaque") .runtime_layout_only else .inspectable, .authority = .generated_private },
            .declared_order = try source.addDeclaredFields(&.{ .{ .named = label }, .{ .padding = primitive } }),
        } });
    }
    const root = try source.add(.{ .func = .{ .args = try source.addSpan(&named_types), .ret = provisional } });
    var summary_nodes: [1]Constraints.Node = undefined;
    const bytes = try encode(gpa, &source, &ns, &.{primitive}, typeSummary(root, &summary_nodes), TestProvenance{});
    defer gpa.free(bytes);
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    var dest_names = names.NameStore.init(gpa);
    defer dest_names.deinit();
    _ = try dest_names.internRecordFieldLabel("unrelated");
    _ = try dest_names.internTagLabel("unrelated");
    _ = try dest_names.internModuleIdentity(&@as([32]u8, @splat(9)));
    _ = try dest.internPrimitive(&dest_names, .bool);
    const input = try dest.internPrimitive(&dest_names, .u64);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const restored = (try decode(arena.allocator(), &dest, &dest_names, &.{input}, bytes, TestProvenance{})).constraints.nodes[0].mono;
    try std.testing.expectEqual(source.representationDigestCached(&ns, root, null), dest.representationDigestCached(&dest_names, restored, null));
    const again = try encode(gpa, &dest, &dest_names, &.{input}, typeSummary(restored, &summary_nodes), TestProvenance{});
    defer gpa.free(again);
    try std.testing.expectEqualSlices(u8, bytes, again);
    const size = dest.types.len();
    for (0..3) |_| {
        const replay = try decode(arena.allocator(), &dest, &dest_names, &.{input}, bytes, TestProvenance{});
        try std.testing.expectEqual(restored, replay.constraints.nodes[0].mono);
        try std.testing.expectEqual(size, dest.types.len());
    }
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &dest, &dest_names, &.{input}, bytes, TestProvenance{ .declaring_module = @splat(8) }));
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &dest, &dest_names, &.{input}, bytes, TestProvenance{ .available = false }));
    try std.testing.expectEqual(size, dest.types.len());
    var other_field = field;
    other_field.default.?.expr_node = 2;
    const other_record = try source.add(.{ .record = try source.addFields(&.{other_field}) });
    const first_default = try encode(gpa, &source, &ns, &.{}, typeSummary(record, &summary_nodes), TestProvenance{});
    defer gpa.free(first_default);
    const second_default = try encode(gpa, &source, &ns, &.{}, typeSummary(other_record, &summary_nodes), TestProvenance{});
    defer gpa.free(second_default);
    try std.testing.expect(!std.mem.eql(u8, first_default, second_default));
}

test "persistent interface graph traversal is bounded by nodes rather than shared paths or depth" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var source = Type.Store.init(gpa);
    defer source.deinit();
    var root = try source.internPrimitive(&ns, .u64);
    // Every level shares its child twice; recursive serialization would have
    // exponential size. The long chain also exceeds native recursion budgets.
    const depth = 2048;
    for (0..depth) |_| root = try source.add(.{ .tuple = try source.addSpan(&.{ root, root }) });
    var nodes: [1]Constraints.Node = undefined;
    const bytes = try encode(gpa, &source, &ns, &.{}, typeSummary(root, &nodes), TestProvenance{});
    defer gpa.free(bytes);
    try std.testing.expect(bytes.len < 16 * depth);
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var restored = (try decode(arena.allocator(), &dest, &ns, &.{}, bytes, TestProvenance{})).constraints.nodes[0].mono;
    for (0..depth) |_| {
        const items = dest.span(dest.get(restored).tuple);
        try std.testing.expectEqual(GuardedList.at(items, 0), GuardedList.at(items, 1));
        restored = GuardedList.at(items, 0);
    }
    try std.testing.expectEqual(Type.Primitive.u64, dest.get(restored).primitive);
}

test "persistent interface recursive function record survives producer destruction" {
    const gpa = std.testing.allocator;
    const bytes = blk: {
        var ns = names.NameStore.init(gpa);
        defer ns.deinit();
        var source = Type.Store.init(gpa);
        defer source.deinit();
        // Recursive iterator-style record: step returns the same record.
        const slot = try source.beginRecursive();
        const function = try source.add(.{ .func = .{ .args = .empty(), .ret = slot.ty } });
        source.finishRecursive(slot, .{ .record = try source.addFields(&.{.{ .name = try ns.internRecordFieldLabel("step"), .ty = function, .default = null }}) });
        var nodes: [1]Constraints.Node = undefined;
        break :blk try encode(gpa, &source, &ns, &.{}, typeSummary(slot.ty, &nodes), TestProvenance{});
    };
    defer gpa.free(bytes);
    for (0..2) |_| {
        var ns = names.NameStore.init(gpa);
        defer ns.deinit();
        var dest = Type.Store.init(gpa);
        defer dest.deinit();
        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();
        const root = (try decode(arena.allocator(), &dest, &ns, &.{}, bytes, TestProvenance{})).constraints.nodes[0].mono;
        const field = GuardedList.at(dest.fieldSpan(dest.get(root).record), 0);
        try std.testing.expectEqual(root, dest.get(field.ty).func.ret);
    }
}

test "persistent interface open variables and defaulted field kinds remain relation data" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var store = Type.Store.init(gpa);
    defer store.deinit();
    const module = try ns.internModuleIdentity(&@as([32]u8, @splat(1)));
    const summary: Summary = .{ .constraints = .{
        .roots = &.{ comptime testNodeRoot(0), comptime testNodeRoot(0) },
        .nodes = &.{.{ .open = 0 }},
        .open_nodes = &.{.{ .content = .{ .unresolved = Solve.InstVariable.checkedVariable(null, null) } }},
        .kinds = &.{.{ .resolved = .{ .defaulted = .{ .module = module, .expr_node = 1 } }, .cells = null }},
    } };
    const bytes = try encode(gpa, &store, &ns, &.{}, summary, TestProvenance{});
    defer gpa.free(bytes);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const result = (try decode(arena.allocator(), &store, &ns, &.{}, bytes, TestProvenance{})).constraints;
    try std.testing.expectEqual(result.roots[0], result.roots[1]);
    try std.testing.expectEqual(Solve.InstVariableOrigin.checked_variable, result.open_nodes[0].content.unresolved.origin);
    try std.testing.expectEqual(@as(u32, 1), result.kinds[0].resolved.?.defaulted.expr_node);
}

fn allocationFailureRoundTrip(gpa: Allocator) error{ InvalidSummary, NotRepresentable, OutOfMemory, TestExpectedEqual, TestUnexpectedResult }!void {
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var source = Type.Store.init(gpa);
    defer source.deinit();
    const primitive = try source.internPrimitive(&ns, .u64);
    const record = try source.add(.{ .record = try source.addFields(&.{.{
        .name = try ns.internRecordFieldLabel("value"),
        .ty = primitive,
        .default = .{ .module = try ns.internModuleIdentity(&@as([32]u8, @splat(1))), .expr_node = 1 },
    }}) });
    const root = try source.add(.{ .tuple = try source.addSpan(&.{ record, record }) });
    var nodes: [1]Constraints.Node = undefined;
    const bytes = try encode(gpa, &source, &ns, &.{primitive}, typeSummary(root, &nodes), TestProvenance{});
    defer gpa.free(bytes);
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    var destination_names = names.NameStore.init(gpa);
    defer destination_names.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const input = try dest.internPrimitive(&destination_names, .u64);
    const before = dest.types.len();
    _ = decode(arena.allocator(), &dest, &destination_names, &.{input}, bytes, TestProvenance{}) catch |err| {
        try std.testing.expectEqual(before, dest.types.len());
        try std.testing.expect(!dest.hasSpeculativeConstruction());
        return err;
    };
}

test "persistent interface allocation failures roll back construction" {
    // In-place growth under the testing allocator depends on the heap's
    // layout, so the failure check runs over an allocator that refuses it.
    var deterministic = base.DeterministicAllocator.init(std.testing.allocator);
    try std.testing.checkAllAllocationFailures(deterministic.allocator(), allocationFailureRoundTrip, .{});
}

test "persistent interface rejects corrupt graph data before changing the receiving store" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var source = Type.Store.init(gpa);
    defer source.deinit();
    const primitive = try source.internPrimitive(&ns, .u64);
    const root = try source.internTuple(&ns, &.{ primitive, primitive });
    var summary_nodes: [1]Constraints.Node = undefined;
    const bytes = try encode(gpa, &source, &ns, &.{}, typeSummary(root, &summary_nodes), TestProvenance{});
    defer gpa.free(bytes);
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    _ = try dest.internPrimitive(&ns, .bool);
    const before = dest.types.len();
    for (0..bytes.len) |len| {
        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();
        try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &dest, &ns, &.{}, bytes[0..len], TestProvenance{}));
        try std.testing.expectEqual(before, dest.types.len());
        try std.testing.expect(!dest.hasSpeculativeConstruction());
    }
    const bad = try gpa.dupe(u8, bytes);
    defer gpa.free(bad);
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    // Payload version, enormous table count, invalid node tag, invalid type
    // reference, and an invalid table tag. These offsets are the v2 contract
    // for a single-root single-leaf relation with no open nodes or kinds.
    for ([_]usize{ 7, 20, 24, 41, 42, 54 }) |offset| {
        @memcpy(bad, bytes);
        bad[offset] = 255;
        try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &dest, &ns, &.{}, bad, TestProvenance{}));
        try std.testing.expectEqual(before, dest.types.len());
    }
    // Append a well-formed but unreachable primitive, then a tuple edge that
    // names it. Reachability, rather than allocation order, defines ownership.
    const extra = try gpa.alloc(u8, bytes.len + 2);
    defer gpa.free(extra);
    @memcpy(extra[0..bytes.len], bytes);
    @memcpy(extra[bytes.len..], bytes[bytes.len - 2 ..]);
    std.mem.writeInt(u32, extra[20..24], 3, .little);
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &dest, &ns, &.{}, extra, TestProvenance{}));
    try std.testing.expectEqual(before, dest.types.len());
    std.mem.writeInt(u32, extra[63..67], 2, .little);
    _ = try decode(arena.allocator(), &dest, &ns, &.{}, extra, TestProvenance{});
    try std.testing.expectError(error.InvalidSummary, decodeForRequest(arena.allocator(), &dest, &ns, &.{}, bytes, TestProvenance{}, 2, true, null));
    const restored = try decodeForRequest(arena.allocator(), &dest, &ns, &.{}, bytes, TestProvenance{}, 1, false, null);
    try std.testing.expectEqual(@as(usize, 1), restored.constraints.roots.len);
}

test "persistent interface captured open relations replay independently like memory summaries" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var store = Type.Store.init(gpa);
    defer store.deinit();
    const graph = try Solve.InstGraph.create(gpa, &store, &ns);
    defer graph.destroy();
    const variable = try graph.newNode(.{ .unresolved = Solve.InstVariable.checkedVariable(null, null) });
    const function = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(Solve.NodeId, &.{variable}), .ret = variable } });
    const captured = try Constraints.capture(graph, graph.arena(), &.{function});
    const memory = try captured.instantiate(graph);
    const bytes = try encode(gpa, &store, &ns, &.{}, .{ .constraints = captured }, TestProvenance{});
    defer gpa.free(bytes);
    const restored = (try decode(graph.arena(), &store, &ns, &.{}, bytes, TestProvenance{})).constraints;
    const first = try restored.instantiate(graph);
    const second = try restored.instantiate(graph);
    const original_identity = try captured.identity(graph);
    const persistent_identity = try restored.identity(graph);
    try std.testing.expectEqualSlices(u8, original_identity.bytes, persistent_identity.bytes);
    const first_fn = try graph.functionNodes(first[0]);
    const second_fn = try graph.functionNodes(second[0]);
    const memory_fn = try graph.functionNodes(memory[0]);
    try std.testing.expect(graph.sameClass(first_fn.args[0], first_fn.ret));
    try std.testing.expect(!graph.sameClass(first_fn.ret, second_fn.ret));
    try graph.unify(first_fn.ret, try graph.newNode(.{ .primitive = .u64 }));
    try std.testing.expect((try graph.content(second_fn.ret)) == .unresolved);
    try std.testing.expect((try graph.content(memory_fn.ret)) == .unresolved);
    try std.testing.expect((try graph.content(variable)) == .unresolved);
}

test "persistent interface equivalent input leaves use the first request position" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var source = Type.Store.init(gpa);
    defer source.deinit();
    const first = try source.add(.{ .primitive = .u64 });
    const second = try source.add(.{ .primitive = .u64 });
    var nodes: [1]Constraints.Node = undefined;
    var stats: Stats = .{};
    const bytes = try encodeWithStats(gpa, &source, &ns, &.{ first, second }, typeSummary(second, &nodes), TestProvenance{}, &stats);
    defer gpa.free(bytes);
    try std.testing.expectEqual(@as(u64, 0), stats.types);
    var dest = Type.Store.init(gpa);
    defer dest.deinit();
    _ = try dest.add(.{ .primitive = .bool });
    const first_input = try dest.add(.{ .primitive = .u64 });
    const second_input = try dest.add(.{ .primitive = .u64 });
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const result = try decode(arena.allocator(), &dest, &ns, &.{ first_input, second_input }, bytes, TestProvenance{});
    try std.testing.expectEqual(first_input, result.constraints.nodes[0].mono);
}

test "persistent parametric interfaces retain explicit holes and reject unfinished placeholders" {
    const gpa = std.testing.allocator;
    var ns = names.NameStore.init(gpa);
    defer ns.deinit();
    var types = Type.Store.init(gpa);
    defer types.deinit();
    const graph = try Solve.InstGraph.create(gpa, &types, &ns);
    defer graph.destroy();
    const scalar = try graph.newNode(.{ .primitive = .u8 });
    const list = try graph.newNode(.{ .list = scalar });
    var holes: [1]?Solve.NodeId = undefined;
    const captured = try Constraints.captureRequestWithHoles(graph, graph.arena(), &.{ list, scalar }, &.{scalar}, &holes);
    const bytes = try encode(gpa, &types, &ns, &.{}, .{ .constraints = captured }, TestProvenance{});
    defer gpa.free(bytes);
    const reopened = (try decode(graph.arena(), &types, &ns, &.{}, bytes, TestProvenance{})).constraints;
    for ([_]Constraints{ captured, reopened }) |summary| {
        const instance = try summary.instantiate(graph);
        try std.testing.expectEqual(Solve.InstVariableOrigin.interface_parameter, (try graph.content(instance[1])).unresolved.origin);
        try std.testing.expect(graph.sameClass((try graph.content(instance[0])).list, instance[1]));
        const actual = try graph.newNode(.{ .primitive = .str });
        try graph.unify(instance[1], actual);
        try std.testing.expectEqual(Type.Primitive.str, (try graph.content((try graph.content(instance[0])).list)).primitive);
    }
    const parameter: Summary = .{ .constraints = .{
        .roots = &.{comptime testNodeRoot(0)},
        .nodes = &.{.{ .open = 0 }},
        .open_nodes = &.{.{ .content = .{ .unresolved = Solve.InstVariable.interfaceParameter() } }},
        .kinds = &.{},
    } };
    var checked_variable = parameter;
    checked_variable.constraints.open_nodes = &.{.{ .content = .{ .unresolved = Solve.InstVariable.checkedVariable(null, null) } }};
    const malformed = try encode(gpa, &types, &ns, &.{}, parameter, TestProvenance{});
    defer gpa.free(malformed);
    const comparison = try encode(gpa, &types, &ns, &.{}, checked_variable, TestProvenance{});
    defer gpa.free(comparison);
    try std.testing.expectEqual(malformed.len, comparison.len);
    var differences: usize = 0;
    for (malformed, comparison) |*byte, other| if (byte.* != other) {
        try std.testing.expectEqual(@as(u8, @backingInt(Solve.InstVariableOrigin.interface_parameter)), byte.*);
        byte.* = @backingInt(Solve.InstVariableOrigin.placeholder);
        differences += 1;
    };
    try std.testing.expectEqual(@as(usize, 1), differences);
    try std.testing.expectError(error.InvalidSummary, decode(graph.arena(), &types, &ns, &.{}, malformed, TestProvenance{}));
}

test "persistent interface closed occurrences rebind by canonical key across artifacts" {
    const a = std.testing.allocator;
    var ns = names.NameStore.init(a);
    defer ns.deinit();
    var types = Type.Store.init(a);
    defer types.deinit();
    const scalar = try types.internPrimitive(&ns, .u64);
    var old_module: [32]u8 = @splat(0);
    old_module[1] = 3;
    const alias = try types.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @fromBackingInt(1), .ty_module = .{ .bytes = old_module } },
        .def = .{ .module = try ns.internModuleIdentity(&@as([32]u8, @splat(1))), .type_name = try ns.internTypeName("Alias") },
        .kind = .alias,
        .args = .empty(),
        .backing = .{ .ty = scalar, .use = .inspectable },
    } });
    var nodes: [1]Constraints.Node = undefined;
    const summary = typeSummary(alias, &nodes);
    const key: [32]u8 = @splat(42);
    const bytes = try encode(a, &types, &ns, &.{}, summary, TestProvenance{ .closed_key = key });
    defer a.free(bytes);
    var arena = std.heap.ArenaAllocator.init(a);
    defer arena.deinit();
    const decoded = (try decode(arena.allocator(), &types, &ns, &.{}, bytes, TestProvenance{ .closed_key = key, .occurrence_version = 9 })).constraints;
    const named = types.get(decoded.nodes[0].mono).named;
    try std.testing.expectEqual(@as(u8, 9), named.named_type.ty_module.bytes[1]);
    try std.testing.expectEqualSlices(u8, &@as([32]u8, @splat(0)), &named.named_type.module.bytes);
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &types, &ns, &.{}, bytes, TestProvenance{}));
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &types, &ns, &.{}, bytes, TestProvenance{ .closed_key = @splat(43) }));
    try std.testing.expectError(error.InvalidSummary, decode(arena.allocator(), &types, &ns, &.{}, bytes, TestProvenance{ .closed_key = key, .available = false }));
    // Identity-bearing occurrences keep their exact artifact; they are never
    // silently promoted to the canonical closed-root contract.
    const exact = try encode(a, &types, &ns, &.{}, summary, TestProvenance{});
    defer a.free(exact);
    const exact_decoded = (try decode(arena.allocator(), &types, &ns, &.{}, exact, TestProvenance{ .closed_key = key, .occurrence_version = 9 })).constraints;
    try std.testing.expectEqual(@as(u8, 3), types.get(exact_decoded.nodes[0].mono).named.named_type.ty_module.bytes[1]);
}
