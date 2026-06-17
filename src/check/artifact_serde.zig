//! Generic, layout-independent (de)serializer used to cache the checked module
//! artifact (see `checked_artifact.zig`).
//!
//! Unlike the zero-copy `CompactWriter` / `Serialized` pattern used for
//! `ModuleEnv`, this serializer is layout-INDEPENDENT: it walks types via
//! comptime reflection and writes each scalar field in declaration order. This
//! matters because the artifact `.bin` is produced by the build-time
//! `builtin_compiler` (compiled `-ODebug` on the host) but consumed by `roc`
//! (which may be `ReleaseFast`). The in-memory byte layout of `auto`-layout
//! structs is not guaranteed equal across optimization levels, so we never rely
//! on it; we only rely on the comptime field order, which is stable.
//!
//! Deserialization deep-copies into freshly allocated memory, so the artifact's
//! existing `deinit` frees everything exactly as if it had been built by
//! `publishFromTypedModule`.
//!
//! Types that contain non-serializable members (allocators, hash maps, raw
//! pointers) opt out of the generic walk by providing `rocSerialize` /
//! `rocDeserialize` methods. Any type the generic walk cannot handle and that
//! lacks those hooks triggers a `@compileError`, so missing hooks are caught at
//! compile time rather than producing a corrupt cache.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Growable byte sink for serialization.
pub const Writer = struct {
    bytes: std.ArrayList(u8) = .empty,
    gpa: Allocator,

    pub fn init(gpa: Allocator) Writer {
        return .{ .bytes = .empty, .gpa = gpa };
    }

    pub fn deinit(self: *Writer) void {
        self.bytes.deinit(self.gpa);
    }

    pub fn toOwnedSlice(self: *Writer) Allocator.Error![]u8 {
        return self.bytes.toOwnedSlice(self.gpa);
    }

    pub fn writeRaw(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.gpa, data);
    }

    pub fn writeLen(self: *Writer, len: usize) Allocator.Error!void {
        const v: u64 = len;
        try self.writeRaw(std.mem.asBytes(&v));
    }

    /// Length-prefixed raw byte string. Matches the wire format produced by
    /// `serializeValue` for a `[]const u8`, so the two are interchangeable.
    pub fn writeStr(self: *Writer, s: []const u8) Allocator.Error!void {
        try self.writeLen(s.len);
        try self.writeRaw(s);
    }
};

/// Cursor over a serialized byte buffer.
pub const Reader = struct {
    bytes: []const u8,
    pos: usize = 0,
    gpa: Allocator,

    pub const Error = error{CorruptArtifact} || Allocator.Error;

    pub fn init(bytes: []const u8, gpa: Allocator) Reader {
        return .{ .bytes = bytes, .pos = 0, .gpa = gpa };
    }

    pub fn readRaw(self: *Reader, n: usize) error{CorruptArtifact}![]const u8 {
        if (self.pos + n > self.bytes.len) return error.CorruptArtifact;
        const out = self.bytes[self.pos..][0..n];
        self.pos += n;
        return out;
    }

    pub fn readLen(self: *Reader) error{CorruptArtifact}!usize {
        const raw = try self.readRaw(@sizeOf(u64));
        var v: u64 = undefined;
        @memcpy(std.mem.asBytes(&v), raw);
        return @intCast(v);
    }

    /// Read a length-prefixed byte string as a view INTO the underlying buffer
    /// (no allocation). The returned slice is only valid while `bytes` lives.
    pub fn readStr(self: *Reader) error{CorruptArtifact}![]const u8 {
        const n = try self.readLen();
        return self.readRaw(n);
    }
};

/// True when `T` provides custom (de)serialization hooks.
fn hasHooks(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum", .@"opaque" => @hasDecl(T, "rocSerialize") and @hasDecl(T, "rocDeserialize"),
        else => false,
    };
}

/// True when `T` provides a custom free hook (used by `freeValue`).
fn hasFreeHook(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum", .@"opaque" => @hasDecl(T, "rocFree"),
        else => false,
    };
}

/// True when `T` is a `std.ArrayList(E)` (unmanaged): exactly `items` (a slice)
/// and `capacity: usize`.
fn isArrayList(comptime T: type) bool {
    const info = @typeInfo(T);
    if (info != .@"struct") return false;
    if (info.@"struct".fields.len != 2) return false;
    if (!@hasField(T, "items") or !@hasField(T, "capacity")) return false;
    if (@FieldType(T, "capacity") != usize) return false;
    const items_info = @typeInfo(@FieldType(T, "items"));
    return items_info == .pointer and items_info.pointer.size == .slice;
}

/// Serialize a value of type `T` into `w`.
pub fn serializeValue(comptime T: type, w: *Writer, value: *const T) Allocator.Error!void {
    if (comptime hasHooks(T)) {
        return value.rocSerialize(w);
    }
    switch (@typeInfo(T)) {
        .int, .float, .bool, .@"enum" => try w.writeRaw(std.mem.asBytes(value)),
        .void => {},
        .@"struct" => |s| {
            if (comptime s.layout == .@"packed") {
                try w.writeRaw(std.mem.asBytes(value));
                return;
            }
            if (comptime isArrayList(T)) {
                try serializeSlice(@FieldType(T, "items"), w, value.items);
                return;
            }
            inline for (s.fields) |f| {
                try serializeValue(f.type, w, &@field(value, f.name));
            }
        },
        .optional => |o| {
            if (value.*) |payload| {
                try w.writeRaw(&[_]u8{1});
                var tmp = payload;
                try serializeValue(o.child, w, &tmp);
            } else {
                try w.writeRaw(&[_]u8{0});
            }
        },
        .@"union" => |u| {
            const Tag = u.tag_type orelse @compileError("artifact_serde: untagged union " ++ @typeName(T));
            var active = std.meta.activeTag(value.*);
            try serializeValue(Tag, w, &active);
            inline for (u.fields) |f| {
                if (active == @field(Tag, f.name)) {
                    if (f.type != void) {
                        try serializeValue(f.type, w, &@field(value, f.name));
                    }
                }
            }
        },
        .array => |a| {
            for (value) |*elem| try serializeValue(a.child, w, elem);
        },
        .pointer => |p| switch (p.size) {
            .slice => try serializeSlice(T, w, value.*),
            else => @compileError("artifact_serde: cannot serialize non-slice pointer " ++ @typeName(T)),
        },
        else => @compileError("artifact_serde: unsupported type " ++ @typeName(T)),
    }
}

fn serializeSlice(comptime SliceT: type, w: *Writer, slice: SliceT) Allocator.Error!void {
    const Elem = std.meta.Child(SliceT);
    try w.writeLen(slice.len);
    for (slice) |*elem| try serializeValue(Elem, w, elem);
}

/// Deserialize a value of type `T` from `r` into `out`. Allocates owned memory
/// for slices/lists via `r.gpa`.
pub fn deserializeValue(comptime T: type, r: *Reader, out: *T) Reader.Error!void {
    if (comptime hasHooks(T)) {
        out.* = try T.rocDeserialize(r);
        return;
    }
    switch (@typeInfo(T)) {
        .int, .float, .bool, .@"enum" => {
            const raw = try r.readRaw(@sizeOf(T));
            @memcpy(std.mem.asBytes(out), raw);
        },
        .void => out.* = {},
        .@"struct" => |s| {
            if (comptime s.layout == .@"packed") {
                const raw = try r.readRaw(@sizeOf(T));
                @memcpy(std.mem.asBytes(out), raw);
                return;
            }
            if (comptime isArrayList(T)) {
                const slice = try deserializeSlice(@FieldType(T, "items"), r);
                out.* = .{ .items = slice, .capacity = slice.len };
                return;
            }
            inline for (s.fields) |f| {
                try deserializeValue(f.type, r, &@field(out, f.name));
            }
        },
        .optional => |o| {
            const present = (try r.readRaw(1))[0];
            if (present == 1) {
                var payload: o.child = undefined;
                try deserializeValue(o.child, r, &payload);
                out.* = payload;
            } else {
                out.* = null;
            }
        },
        .@"union" => |u| {
            const Tag = u.tag_type orelse @compileError("artifact_serde: untagged union " ++ @typeName(T));
            var tag: Tag = undefined;
            try deserializeValue(Tag, r, &tag);
            inline for (u.fields) |f| {
                if (tag == @field(Tag, f.name)) {
                    if (f.type == void) {
                        out.* = @unionInit(T, f.name, {});
                    } else {
                        var payload: f.type = undefined;
                        try deserializeValue(f.type, r, &payload);
                        out.* = @unionInit(T, f.name, payload);
                    }
                }
            }
        },
        .array => |a| {
            for (out) |*elem| try deserializeValue(a.child, r, elem);
        },
        .pointer => |p| switch (p.size) {
            .slice => out.* = try deserializeSlice(T, r),
            else => @compileError("artifact_serde: cannot deserialize non-slice pointer " ++ @typeName(T)),
        },
        else => @compileError("artifact_serde: unsupported type " ++ @typeName(T)),
    }
}

fn deserializeSlice(comptime SliceT: type, r: *Reader) Reader.Error!SliceT {
    const Elem = std.meta.Child(SliceT);
    const len = try r.readLen();
    if (len == 0) return &.{};
    const buf = try r.gpa.alloc(Elem, len);
    for (buf) |*elem| try deserializeValue(Elem, r, elem);
    return buf;
}

/// Free everything `deserializeValue` allocated for a value of type `T`.
///
/// This mirrors `deserializeValue` exactly: it frees the same slices/lists that
/// deserialization allocated, regardless of what the type's own `deinit` does.
/// (The artifact's normal `deinit` intentionally does not free some slices that
/// are borrowed when built by `publishFromTypedModule`; deserialization owns
/// those copies, so it must free them here.) Types with a `rocFree` hook free
/// themselves.
pub fn freeValue(comptime T: type, gpa: Allocator, value: *const T) void {
    if (comptime hasFreeHook(T)) {
        @constCast(value).rocFree(gpa);
        return;
    }
    switch (@typeInfo(T)) {
        .int, .float, .bool, .@"enum", .void => {},
        .@"struct" => |s| {
            if (comptime s.layout == .@"packed") return;
            if (comptime isArrayList(T)) {
                freeSlice(@FieldType(T, "items"), gpa, value.items);
                return;
            }
            inline for (s.fields) |f| freeValue(f.type, gpa, &@field(value, f.name));
        },
        .optional => |o| {
            if (value.*) |*payload| freeValue(o.child, gpa, payload);
        },
        .@"union" => |u| {
            const Tag = u.tag_type.?;
            const active = std.meta.activeTag(value.*);
            inline for (u.fields) |f| {
                if (active == @field(Tag, f.name)) {
                    if (f.type != void) freeValue(f.type, gpa, &@field(value, f.name));
                }
            }
        },
        .array => |a| {
            for (value) |*elem| freeValue(a.child, gpa, elem);
        },
        .pointer => |p| switch (p.size) {
            .slice => freeSlice(T, gpa, value.*),
            else => {},
        },
        else => {},
    }
}

fn freeSlice(comptime SliceT: type, gpa: Allocator, slice: SliceT) void {
    const Elem = std.meta.Child(SliceT);
    for (slice) |*elem| freeValue(Elem, gpa, elem);
    if (slice.len > 0) gpa.free(@constCast(slice));
}

/// Helper for hooks: serialize a `std.AutoHashMapUnmanaged(K, V)` as a flat list
/// of `(key, value)` entries.
pub fn serializeAutoHashMap(comptime K: type, comptime V: type, w: *Writer, map: anytype) Allocator.Error!void {
    try w.writeLen(map.count());
    var it = map.iterator();
    while (it.next()) |entry| {
        var k = entry.key_ptr.*;
        var v = entry.value_ptr.*;
        try serializeValue(K, w, &k);
        try serializeValue(V, w, &v);
    }
}

/// Helper for hooks: load a `std.AutoHashMapUnmanaged(K, V)` from the flat
/// entry list written by `serializeAutoHashMap`.
pub fn deserializeAutoHashMap(comptime K: type, comptime V: type, r: *Reader, gpa: Allocator) Reader.Error!std.AutoHashMapUnmanaged(K, V) {
    var map: std.AutoHashMapUnmanaged(K, V) = .{};
    errdefer map.deinit(gpa);
    const count = try r.readLen();
    try map.ensureTotalCapacity(gpa, @intCast(count));
    var i: usize = 0;
    while (i < count) : (i += 1) {
        var k: K = undefined;
        var v: V = undefined;
        try deserializeValue(K, r, &k);
        try deserializeValue(V, r, &v);
        map.putAssumeCapacity(k, v);
    }
    return map;
}
