//! One wasm function body generated apart from the module it joins.
//!
//! A body worker reads the module's fixed index spaces (procedures, imports,
//! tail drivers, Boxy thunks) but never creates a module entity. Each entity
//! that would otherwise be created the first time a body names it (an RC
//! helper, a table slot, a data segment, a data or PIC-base symbol) is
//! instead recorded in the body's entity list, in first-use order, and every
//! operand naming it is a `CodeBuilder.Deferred` operand or a relocation
//! against `CodeBuilder.deferredSymbol`. The module's owner commits bodies in
//! a fixed order, creating each listed entity that does not exist yet as it
//! goes, so every module index is a function of that order and the bodies'
//! contents, never of which worker generated a body or when.

const std = @import("std");
const base = @import("base");
const CodeBuilder = @import("CodeBuilder.zig");

const Allocator = std.mem.Allocator;

/// A PIC base global a relocatable object addresses data or table slots from.
pub const PicBase = enum { memory, table };

/// A module entity a body names before the module has it.
pub const Entity = union(enum) {
    /// An RC helper function by its cache key, compiled with its transitive
    /// child helpers on first reference.
    rc_helper: u64,
    /// An RC helper that a helper body calls. The closure reservation that
    /// placed the calling helper placed this one too.
    reserved_rc_helper: u64,
    /// The table slot of the RC helper with this cache key.
    rc_helper_table: u64,
    /// The table slot of the Boxy capture-drop callback with this key.
    capture_drop_table: u64,
    /// The data segment holding one LIR string backing.
    static_str: u32,
    /// The data symbol of one LIR static-data id.
    static_data: u32,
    /// A runtime message segment. Every emission site gets its own segment,
    /// so messages are never shared between references.
    message: []const u8,
    /// A PIC base global.
    pic_base: PicBase,

    const Key = struct { tag: std.meta.Tag(Entity), id: u64 };

    fn key(self: Entity) ?Key {
        return .{ .tag = self, .id = switch (self) {
            .rc_helper, .reserved_rc_helper, .rc_helper_table, .capture_drop_table => |id| id,
            .static_str, .static_data => |id| id,
            .pic_base => |which| @backingInt(which),
            .message => return null,
        } };
    }
};

/// Collects a body's entity list while a worker generates it.
pub const Recorder = struct {
    allocator: Allocator,
    entities: std.ArrayList(Entity) = .empty,
    refs: std.AutoHashMapUnmanaged(Entity.Key, u32) = .empty,

    pub fn init(allocator: Allocator) Recorder {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Recorder) void {
        for (self.entities.items) |entity| switch (entity) {
            .message => |bytes| self.allocator.free(bytes),
            .rc_helper, .reserved_rc_helper, .rc_helper_table, .capture_drop_table, .static_str, .static_data, .pic_base => {},
        };
        self.entities.deinit(self.allocator);
        self.refs.deinit(self.allocator);
    }

    /// The entity-list position naming `entity`, appending it on first use.
    /// A message's bytes are copied, since its source may be transient.
    pub fn ref(self: *Recorder, entity: Entity) Allocator.Error!u32 {
        const ref_index: u32 = @intCast(self.entities.items.len);
        if (entity.key()) |entity_key| {
            const entry = try self.refs.getOrPut(self.allocator, entity_key);
            if (entry.found_existing) return entry.value_ptr.*;
            entry.value_ptr.* = ref_index;
            errdefer _ = self.refs.remove(entity_key);
            try self.entities.append(self.allocator, entity);
        } else {
            const bytes = try self.allocator.dupe(u8, entity.message);
            errdefer self.allocator.free(bytes);
            try self.entities.append(self.allocator, .{ .message = bytes });
        }
        return ref_index;
    }

    /// A placeholder index or address operand value naming `entity`.
    pub fn placeholder(self: *Recorder, entity: Entity) Allocator.Error!u32 {
        return CodeBuilder.deferredSymbol(try self.ref(entity)).raw();
    }
};

/// A generated body and the entities it names, owned by its own arena so it
/// can move from the worker that generated it to the module's owner.
pub const Artifact = struct {
    arena: base.SingleThreadArena,
    body: CodeBuilder,
    entities: []const Entity,

    /// Copy `body` and `recorder`'s entities into an arena backed by `allocator`.
    pub fn capture(allocator: Allocator, body: *const CodeBuilder, recorder: *const Recorder) Allocator.Error!Artifact {
        var arena = base.SingleThreadArena.init(allocator);
        errdefer arena.deinit();
        const a = arena.allocator();
        var copy = CodeBuilder.init();
        try copy.code.appendSlice(a, body.code.items);
        try copy.preamble.appendSlice(a, body.preamble.items);
        try copy.import_relocations.appendSlice(a, body.import_relocations.items);
        try copy.deferred.appendSlice(a, body.deferred.items);
        const entities = try a.dupe(Entity, recorder.entities.items);
        for (entities) |*entity| switch (entity.*) {
            .message => |bytes| entity.* = .{ .message = try a.dupe(u8, bytes) },
            .rc_helper, .reserved_rc_helper, .rc_helper_table, .capture_drop_table, .static_str, .static_data, .pic_base => {},
        };
        return .{ .arena = arena, .body = copy, .entities = entities };
    }

    pub fn deinit(self: *Artifact) void {
        self.arena.deinit();
        self.* = undefined;
    }
};

const testing = std.testing;
const WasmModule = @import("WasmModule.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;

test "recorder lists each keyed entity once and every message separately" {
    var recorder = Recorder.init(testing.allocator);
    defer recorder.deinit();
    var message: [5]u8 = "first".*;
    try testing.expectEqual(@as(u32, 0), try recorder.ref(.{ .rc_helper = 7 }));
    try testing.expectEqual(@as(u32, 1), try recorder.ref(.{ .message = &message }));
    message = "other".*;
    try testing.expectEqual(@as(u32, 2), try recorder.ref(.{ .rc_helper_table = 7 }));
    try testing.expectEqual(@as(u32, 0), try recorder.ref(.{ .rc_helper = 7 }));
    try testing.expectEqual(@as(u32, 3), try recorder.ref(.{ .message = &message }));
    try testing.expectEqual(@as(u32, 4), try recorder.ref(.{ .pic_base = .table }));
    try testing.expectEqual(@as(u32, 4), try recorder.ref(.{ .pic_base = .table }));
    try testing.expectEqualStrings("first", recorder.entities.items[1].message);
    try testing.expectEqualStrings("other", recorder.entities.items[3].message);
}

test "resolving deferred operands patches padded operands in place and splices compact ones" {
    const a = testing.allocator;
    var body = CodeBuilder.init();
    defer body.deinit(a);
    // i32.const <compact ref 1>; call <padded ref 0 + relocation>; i32.const <compact ref 1 + 8>; end
    try body.code.append(a, WasmModule.Op.i32_const);
    try body.addDeferred(a, .{ .code_pos = @intCast(body.code.items.len), .ref = 1, .form = .compact_signed });
    const call_pos: u32 = @intCast(body.code.items.len + 1);
    try body.emitRelocatableCall(a, CodeBuilder.deferredSymbol(0), 0);
    try body.addDeferred(a, .{ .code_pos = call_pos, .ref = 0, .form = .padded_unsigned });
    try body.code.append(a, WasmModule.Op.i32_const);
    try body.addDeferred(a, .{ .code_pos = @intCast(body.code.items.len), .ref = 1, .addend = 8, .form = .compact_signed });
    try body.code.append(a, WasmModule.Op.end);
    try body.prependToCode(a, &.{WasmModule.Op.nop});

    const resolved = [_]CodeBuilder.Resolved{
        .{ .value = 300, .symbol = SymbolIndex.fromRaw(11) },
        .{ .value = 70, .symbol = SymbolIndex.fromRaw(12) },
    };
    var finished = try body.resolveDeferred(a, &resolved);
    defer finished.deinit(a);

    var expected: std.ArrayList(u8) = .empty;
    defer expected.deinit(a);
    try expected.appendSlice(a, &.{ WasmModule.Op.nop, WasmModule.Op.i32_const });
    try WasmModule.leb128WriteI32(a, &expected, 70);
    try expected.append(a, WasmModule.Op.call);
    const reloc_pos: u32 = @intCast(expected.items.len);
    try WasmModule.appendPaddedU32(a, &expected, 300);
    try expected.append(a, WasmModule.Op.i32_const);
    try WasmModule.leb128WriteI32(a, &expected, 78);
    try expected.append(a, WasmModule.Op.end);
    try testing.expectEqualSlices(u8, expected.items, finished.code.items);
    try testing.expectEqual(@as(usize, 0), finished.deferred.items.len);
    try testing.expectEqual(@as(usize, 1), finished.import_relocations.items.len);
    try testing.expectEqual(reloc_pos, finished.import_relocations.items[0].index.code_pos);
    try testing.expectEqual(SymbolIndex.fromRaw(11), finished.import_relocations.items[0].index.symbol_index);
}
