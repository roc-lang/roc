//! On-disk form of one module's pack: its procedure artifacts and the table
//! from specialization key to root artifact and ownership signature.
//!
//! The encoding is a pure function of its inputs (fixed-width little-endian
//! integers, length-prefixed bytes, artifacts in set order, specs in table
//! order), so two builds of the same module write identical files and the
//! store's rename-into-place protocol never has to compare contents.

const std = @import("std");
const lir = @import("lir");
const ProcArtifact = @import("ProcArtifact.zig");
const RelocationMod = @import("Relocation.zig");

const Allocator = std.mem.Allocator;

const magic = "RPCK";
/// Format version; bump whenever the encoding or artifact contents change.
pub const format_version: u32 = 2;

/// One specialization the pack can serve: its reservation-time key, the
/// artifact holding its procedure, and the ownership signature ARC solved
/// for that procedure, which the linking program adopts as fixed.
pub const SpecEntry = struct {
    key: [32]u8,
    artifact: u32,
    rc_borrowed_params: u64,
    rc_ret_borrowed: bool,
    rc_ret_lenders: u64,
};

/// A pack read back from its bytes.
pub const Pack = struct {
    set: ProcArtifact.Set,
    specs: []const SpecEntry,

    pub fn deinit(self: *Pack) void {
        self.set.deinit();
    }
};

/// Why bytes could not be read as a pack.
pub const ReadError = Allocator.Error || error{
    MalformedPack,
    UnsupportedPackVersion,
};

/// Encode an artifact set and its spec table.
pub fn write(allocator: Allocator, set: *const ProcArtifact.Set, specs: []const SpecEntry) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    var writer = Writer{ .allocator = allocator, .bytes = &bytes };

    try writer.raw(magic);
    try writer.word(format_version);
    try writer.word(@intCast(set.artifacts.len));
    try writer.word(@intCast(specs.len));

    for (set.artifacts) |artifact| {
        switch (artifact.kind) {
            .proc => |identity| {
                try writer.byte(0);
                try writer.raw(&identity.bytes);
            },
            .rc_helper => |name| {
                try writer.byte(1);
                try writer.str(name);
            },
            .boxy_thunk => |identity| {
                try writer.byte(2);
                try writer.raw(&identity.bytes);
            },
            .entrypoint => try writer.byte(3),
            .message_pool_run => try writer.byte(4),
            .branch_island => try writer.byte(5),
        }
        try writer.word(artifact.entry);
        if (artifact.frame) |frame| {
            try writer.byte(1);
            try writer.word(frame.prologue_size);
            try writer.word(frame.stack_alloc);
            try writer.word(frame.frame_size);
            try writer.word(frame.callee_saved_mask);
            try writer.word(frame.epilogue_offset);
            try writer.byte(@intFromBool(frame.uses_frame_pointer));
        } else {
            try writer.byte(0);
        }
        try writer.str(artifact.code);
        try writer.word(@intCast(artifact.refs.len));
        for (artifact.refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(switch (ref.form) {
                .call => 0,
                .addr => 1,
            });
            try writer.word(ref.target);
            try writer.word(ref.delta);
        }
        try writer.word(@intCast(artifact.relocations.len));
        for (artifact.relocations) |relocation| {
            try writer.word(relocation.offset);
            switch (relocation.kind) {
                .function => {
                    try writer.byte(0);
                    try writer.byte(0);
                },
                .data => |data_kind| {
                    try writer.byte(1);
                    try writer.byte(@intFromEnum(data_kind));
                },
            }
            try writer.str(relocation.name);
        }
        try writer.word(@intCast(artifact.data.len));
        for (artifact.data) |item| {
            try writer.str(item.name);
            try writer.str(item.bytes);
            try writer.word(item.alignment);
            try writer.word(item.symbol_offset);
        }
    }

    for (specs) |spec| {
        try writer.raw(&spec.key);
        try writer.word(spec.artifact);
        try writer.wide(spec.rc_borrowed_params);
        try writer.byte(@intFromBool(spec.rc_ret_borrowed));
        try writer.wide(spec.rc_ret_lenders);
    }

    return try bytes.toOwnedSlice(allocator);
}

/// Decode a pack. Every slice in the result is owned by the pack's arena.
pub fn read(allocator: Allocator, bytes: []const u8) ReadError!Pack {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();
    var reader = Reader{ .bytes = bytes };

    const header = try reader.raw(magic.len);
    if (!std.mem.eql(u8, header, magic)) return error.MalformedPack;
    if (try reader.word() != format_version) return error.UnsupportedPackVersion;
    const artifact_count = try reader.word();
    const spec_count = try reader.word();

    const artifacts = try arena_allocator.alloc(ProcArtifact.Artifact, artifact_count);
    for (artifacts) |*artifact| {
        const kind: ProcArtifact.Kind = switch (try reader.byte()) {
            0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
            1 => .{ .rc_helper = try reader.strOwned(arena_allocator) },
            2 => .{ .boxy_thunk = .{ .bytes = (try reader.raw(32))[0..32].* } },
            3 => .entrypoint,
            4 => .message_pool_run,
            5 => .branch_island,
            else => return error.MalformedPack,
        };
        const entry = try reader.word();
        const frame: ?ProcArtifact.Frame = switch (try reader.byte()) {
            0 => null,
            1 => .{
                .prologue_size = try reader.word(),
                .stack_alloc = try reader.word(),
                .frame_size = try reader.word(),
                .callee_saved_mask = try reader.word(),
                .epilogue_offset = try reader.word(),
                .uses_frame_pointer = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                },
            },
            else => return error.MalformedPack,
        };
        const code = try reader.strOwned(arena_allocator);
        const refs = try arena_allocator.alloc(ProcArtifact.Reference, try reader.word());
        for (refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = switch (try reader.byte()) {
                    0 => .call,
                    1 => .addr,
                    else => return error.MalformedPack,
                },
                .target = try reader.word(),
                .delta = try reader.word(),
            };
        }
        const relocations = try arena_allocator.alloc(ProcArtifact.NamedRelocation, try reader.word());
        for (relocations) |*relocation| {
            const offset = try reader.word();
            const kind_tag = try reader.byte();
            const data_kind_raw = try reader.byte();
            const name = try reader.strOwned(arena_allocator);
            relocation.* = .{
                .offset = offset,
                .name = name,
                .kind = switch (kind_tag) {
                    0 => .function,
                    1 => .{ .data = std.enums.fromInt(RelocationMod.DataRelocationKind, data_kind_raw) orelse return error.MalformedPack },
                    else => return error.MalformedPack,
                },
            };
        }
        const data = try arena_allocator.alloc(ProcArtifact.DataItem, try reader.word());
        for (data) |*item| {
            item.* = .{
                .name = try reader.strOwned(arena_allocator),
                .bytes = try reader.strOwned(arena_allocator),
                .alignment = try reader.word(),
                .symbol_offset = try reader.word(),
            };
        }
        artifact.* = .{
            .kind = kind,
            .code = code,
            .entry = entry,
            .frame = frame,
            .refs = refs,
            .relocations = relocations,
            .data = data,
        };
        for (refs) |ref| {
            if (ref.target >= artifact_count) return error.MalformedPack;
        }
    }

    const specs = try arena_allocator.alloc(SpecEntry, spec_count);
    for (specs) |*spec| {
        spec.* = .{
            .key = (try reader.raw(32))[0..32].*,
            .artifact = try reader.word(),
            .rc_borrowed_params = try reader.wide(),
            .rc_ret_borrowed = switch (try reader.byte()) {
                0 => false,
                1 => true,
                else => return error.MalformedPack,
            },
            .rc_ret_lenders = try reader.wide(),
        };
        if (spec.artifact >= artifact_count) return error.MalformedPack;
    }
    if (reader.offset != bytes.len) return error.MalformedPack;

    return .{
        .set = .{ .arena = arena, .artifacts = artifacts },
        .specs = specs,
    };
}

const Writer = struct {
    allocator: Allocator,
    bytes: *std.ArrayList(u8),

    fn raw(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, data);
    }

    fn byte(self: *Writer, value: u8) Allocator.Error!void {
        try self.bytes.append(self.allocator, value);
    }

    fn word(self: *Writer, value: u32) Allocator.Error!void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        try self.raw(&buffer);
    }

    fn wide(self: *Writer, value: u64) Allocator.Error!void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        try self.raw(&buffer);
    }

    fn str(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.word(@intCast(data.len));
        try self.raw(data);
    }
};

const Reader = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn raw(self: *Reader, len: usize) ReadError![]const u8 {
        if (self.bytes.len - self.offset < len) return error.MalformedPack;
        const slice = self.bytes[self.offset..][0..len];
        self.offset += len;
        return slice;
    }

    fn byte(self: *Reader) ReadError!u8 {
        return (try self.raw(1))[0];
    }

    fn word(self: *Reader) ReadError!u32 {
        return std.mem.readInt(u32, (try self.raw(4))[0..4], .little);
    }

    fn wide(self: *Reader) ReadError!u64 {
        return std.mem.readInt(u64, (try self.raw(8))[0..8], .little);
    }

    fn strOwned(self: *Reader, allocator: Allocator) ReadError![]u8 {
        const len = try self.word();
        return try allocator.dupe(u8, try self.raw(len));
    }
};

test "pack bytes round-trip every artifact field and spec entry" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const a = arena.allocator();
    const refs = try a.dupe(ProcArtifact.Reference, &.{
        .{ .site = 5, .form = .call, .target = 1, .delta = 8 },
        .{ .site = 12, .form = .addr, .target = 0, .delta = 0 },
    });
    const relocations = try a.dupe(ProcArtifact.NamedRelocation, &.{
        .{ .offset = 3, .name = try a.dupe(u8, "roc_builtins_str_concat"), .kind = .function },
        .{ .offset = 9, .name = try a.dupe(u8, "roc__static_1"), .kind = .{ .data = .rel32 } },
    });
    const artifacts = try a.dupe(ProcArtifact.Artifact, &.{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(7) },
            .code = try a.dupe(u8, &.{ 0x55, 0x48, 0x89, 0xe5, 0xc3 }),
            .entry = 0,
            .frame = .{ .prologue_size = 4, .stack_alloc = 16, .frame_size = 16, .callee_saved_mask = 0x1000, .epilogue_offset = 4, .uses_frame_pointer = true },
            .refs = refs,
            .relocations = relocations,
            .data = try a.dupe(ProcArtifact.DataItem, &.{.{ .name = try a.dupe(u8, "roc__static_str_ab"), .bytes = try a.dupe(u8, "\x00\x00hi"), .alignment = 8, .symbol_offset = 2 }}),
        },
        .{
            .kind = .{ .rc_helper = try a.dupe(u8, "roc__rc_decref_abc") },
            .code = try a.dupe(u8, &.{ 0xeb, 0x02, 0x90, 0xc3 }),
            .entry = 2,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
        .{ .kind = .message_pool_run, .code = try a.dupe(u8, "hello"), .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    var set = ProcArtifact.Set{ .arena = arena, .artifacts = artifacts };
    defer set.deinit();
    const specs = [_]SpecEntry{
        .{ .key = [_]u8{0xab} ** 32, .artifact = 0, .rc_borrowed_params = 0b101, .rc_ret_borrowed = true, .rc_ret_lenders = 1 },
    };

    const bytes = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(bytes);
    const again = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(again);
    try testing.expectEqualSlices(u8, bytes, again);

    var pack = try read(testing.allocator, bytes);
    defer pack.deinit();
    try testing.expectEqual(@as(usize, 3), pack.set.artifacts.len);
    try testing.expectEqual(@as(usize, 1), pack.specs.len);
    try testing.expectEqualSlices(u8, &specs[0].key, &pack.specs[0].key);
    try testing.expectEqual(specs[0].rc_borrowed_params, pack.specs[0].rc_borrowed_params);
    try testing.expect(pack.specs[0].rc_ret_borrowed);
    const proc = pack.set.artifacts[0];
    try testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(7).bytes, &proc.kind.proc.bytes);
    try testing.expectEqualSlices(u8, artifacts[0].code, proc.code);
    try testing.expectEqual(@as(usize, 2), proc.refs.len);
    try testing.expectEqual(ProcArtifact.Form.addr, proc.refs[1].form);
    try testing.expectEqual(@as(u32, 9), proc.relocations[1].offset);
    try testing.expectEqualStrings("roc__static_1", proc.relocations[1].name);
    try testing.expectEqual(RelocationMod.DataRelocationKind.rel32, proc.relocations[1].kind.data);
    try testing.expectEqual(@as(u32, 0x1000), proc.frame.?.callee_saved_mask);
    try testing.expectEqual(@as(usize, 1), proc.data.len);
    try testing.expectEqualStrings("roc__static_str_ab", proc.data[0].name);
    try testing.expectEqual(@as(u32, 2), proc.data[0].symbol_offset);
    try testing.expectEqualStrings("roc__rc_decref_abc", pack.set.artifacts[1].kind.rc_helper);
    try testing.expectEqual(@as(u32, 2), pack.set.artifacts[1].entry);
    try testing.expect(pack.set.artifacts[1].frame == null);

    const rewritten = try write(testing.allocator, &pack.set, pack.specs);
    defer testing.allocator.free(rewritten);
    try testing.expectEqualSlices(u8, bytes, rewritten);

    try testing.expectError(error.MalformedPack, read(testing.allocator, bytes[0 .. bytes.len - 1]));
}
