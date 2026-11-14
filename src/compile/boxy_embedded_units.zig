//! Boxy unit cache entries the compiler carries in its own binary (design.md
//! "Caching Under `.boxy`", "Embedded units"). The compiler's build lowers
//! and compiles the Builtin module's unit for the target the compiler runs
//! on (src/build/builtin_unit.zig) and links the result into the compiler,
//! where `boxy_unit.restoreOrBuild` serves a unit whose key is one of the
//! table's keys from it instead of the per-module cache.
//!
//! The entries are built by the compiler's own modules in a Debug host
//! tool. What the compiler emits is a function of its sources and its input,
//! never of how the compiler itself was built (design.md "Output
//! Determinism"), so the tool builds the entry any build of the same sources
//! would. The table records the tool's source identity
//! (`build_options.compiler_artifact_hash`), which `find` asserts is the
//! running compiler's. A cache key does fold in the running compiler's build,
//! so the table records each entry's key input
//! (`boxy_object_cache.KeyInput`), which `find` keys under the running
//! compiler.
//!
//! Layout, little-endian:
//!
//!     magic "RBXE", format version (u32),
//!     compiler source identity ([32]u8,
//!     `build_options.compiler_artifact_hash` of the executable that built
//!     the entries),
//!     module count (u32), then for each module whose digests the table
//!     holds: its checked artifact key, its link name, its interface digest
//!     and its object input (`boxy_interface.Digests`), [32]u8 each;
//!     unit count (u32), then for each unit
//!         object input ([32]u8), flags (u8: inline expects, run image,
//!         expect sites, wasm stack checks), target name and opt name (each
//!         a u8 length and its bytes), entry offset and length (u64 each);
//!     then each unit's entry (`boxy_object_cache.encode`) at an offset that
//!     places it as a cache file places its entry, so an image in it starts
//!     at an `image_alignment` offset of the table.

const std = @import("std");
const build_options = @import("build_options");
const object_cache = @import("boxy_object_cache.zig");

const Allocator = std.mem.Allocator;

/// The table format: bumped whenever `encode`'s layout changes.
pub const format_version: u32 = 3;

const magic = "RBXE";

/// The alignment the compiler's build gives the table.
pub const alignment: usize = object_cache.image_alignment;

/// Where an entry begins in a cache file, which `object_cache.encode`
/// aligns an image relative to.
const entry_file_offset = @import("artifact_store.zig").length_prefix_len;

/// The compiler's embedded unit entries.
pub const Units = struct {
    table: []align(alignment) const u8,

    /// No embedded entries: every unit comes from the cache or is built.
    pub const none: Units = .{ .table = &.{} };

    /// The entry of the unit keyed `key` under this compiler, when the table
    /// holds one: the bytes `boxy_object_cache.encode` wrote for it, with
    /// its image at an `object_cache.image_alignment` address.
    pub fn find(self: Units, key: object_cache.Key) ?[]const u8 {
        if (self.table.len == 0) return null;
        var reader = self.modules();
        for (0..reader.word32()) |_| _ = reader.take(module_record_len);
        const count = reader.word32();
        for (0..count) |_| {
            const object_input = reader.take(32)[0..32].*;
            const flags = reader.take(1)[0];
            const target = reader.take(reader.take(1)[0]);
            const opt = reader.take(reader.take(1)[0]);
            const offset: usize = @intCast(reader.word64());
            const len: usize = @intCast(reader.word64());
            const unit_key = object_cache.key(.{
                .object_input = object_input,
                .target = target,
                .opt = opt,
                .inline_expects = flags & flag_inline_expects != 0,
                .run_image = flags & flag_run_image != 0,
                .expect_sites = flags & flag_expect_sites != 0,
                .wasm_stack_checks = flags & flag_wasm_stack_checks != 0,
            });
            if (std.mem.eql(u8, &unit_key.bytes, &key.bytes)) return self.table[offset..][0..len];
        }
        return null;
    }

    /// The digests the table holds for the module whose checked artifact
    /// key is `artifact` when its link names it `link_name`.
    pub fn moduleDigests(self: Units, artifact: [32]u8, link_name: [32]u8) ?ModuleDigests {
        if (self.table.len == 0) return null;
        var reader = self.modules();
        for (0..reader.word32()) |_| {
            const record = reader.take(module_record_len);
            if (!std.mem.eql(u8, record[0..32], &artifact) or !std.mem.eql(u8, record[32..64], &link_name)) continue;
            return .{ .interface = record[64..96].*, .object_input = record[96..128].* };
        }
        return null;
    }

    /// A reader at the table's module count.
    fn modules(self: Units) Reader {
        var reader: Reader = .{ .bytes = self.table };
        std.debug.assert(std.mem.eql(u8, reader.take(magic.len), magic));
        std.debug.assert(reader.word32() == format_version);
        // The build makes the table from the sources this compiler was
        // built from.
        std.debug.assert(std.mem.eql(u8, reader.take(32), &build_options.compiler_artifact_hash));
        return reader;
    }
};

/// A module's digests (`boxy_interface.Digests`) under one link name, for
/// a module that imports nothing: its interface digest and its object
/// input, which a module with imports would extend with theirs.
pub const ModuleDigests = struct {
    interface: [32]u8,
    object_input: [32]u8,
};

/// One module whose digests the compiler's build embeds.
pub const Module = struct {
    artifact: [32]u8,
    link_name: [32]u8,
    digests: ModuleDigests,
};

const module_record_len = 4 * 32;

const flag_inline_expects: u8 = 1;
const flag_run_image: u8 = 2;
const flag_expect_sites: u8 = 4;
const flag_wasm_stack_checks: u8 = 8;

/// One unit the compiler's build embeds: its key input and its encoded
/// cache entry. An embedded unit binds no hosted functions.
pub const Unit = struct {
    input: object_cache.KeyInput,
    entry: []const u8,
};

/// The table of `modules`' digests and `units`, built by an executable
/// whose source identity is `compiler_artifact_hash`. The caller owns the
/// result.
pub fn encode(allocator: Allocator, compiler_artifact_hash: [32]u8, modules: []const Module, units: []const Unit) Allocator.Error![]u8 {
    var bytes: std.ArrayList(u8) = .empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, magic);
    try appendInt(&bytes, allocator, u32, format_version);
    try bytes.appendSlice(allocator, &compiler_artifact_hash);
    try appendInt(&bytes, allocator, u32, @intCast(modules.len));
    for (modules) |module| {
        for ([_]*const [32]u8{ &module.artifact, &module.link_name, &module.digests.interface, &module.digests.object_input }) |digest| {
            try bytes.appendSlice(allocator, digest);
        }
    }
    try appendInt(&bytes, allocator, u32, @intCast(units.len));
    var header_len = bytes.items.len;
    for (units) |unit| header_len += 32 + 1 + 1 + unit.input.target.len + 1 + unit.input.opt.len + 16;
    var offset = header_len;
    for (units) |unit| {
        std.debug.assert(unit.input.hosted_bindings == null);
        offset += placementPadding(offset);
        try bytes.appendSlice(allocator, &unit.input.object_input);
        var flags: u8 = 0;
        if (unit.input.inline_expects) flags |= flag_inline_expects;
        if (unit.input.run_image) flags |= flag_run_image;
        if (unit.input.expect_sites) flags |= flag_expect_sites;
        if (unit.input.wasm_stack_checks) flags |= flag_wasm_stack_checks;
        try bytes.append(allocator, flags);
        for ([_][]const u8{ unit.input.target, unit.input.opt }) |name| {
            try bytes.append(allocator, @intCast(name.len));
            try bytes.appendSlice(allocator, name);
        }
        try appendInt(&bytes, allocator, u64, offset);
        try appendInt(&bytes, allocator, u64, unit.entry.len);
        offset += unit.entry.len;
    }
    std.debug.assert(bytes.items.len == header_len);
    for (units) |unit| {
        try bytes.appendNTimes(allocator, 0, placementPadding(bytes.items.len));
        try bytes.appendSlice(allocator, unit.entry);
    }
    return try bytes.toOwnedSlice(allocator);
}

/// Zero bytes after `len` table bytes that place the next entry where a
/// cache file places its entry relative to an `alignment` boundary.
fn placementPadding(len: usize) usize {
    return std.mem.alignForward(usize, len + alignment - entry_file_offset, alignment) - (len + alignment - entry_file_offset);
}

fn appendInt(bytes: *std.ArrayList(u8), allocator: Allocator, comptime T: type, value: T) Allocator.Error!void {
    var buffer: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buffer, value, .little);
    try bytes.appendSlice(allocator, &buffer);
}

/// Reads the table the compiler's build wrote, which is well formed.
const Reader = struct {
    bytes: []const u8,
    at: usize = 0,

    fn take(self: *Reader, len: usize) []const u8 {
        std.debug.assert(len <= self.bytes.len - self.at);
        const slice = self.bytes[self.at..][0..len];
        self.at += len;
        return slice;
    }

    fn word32(self: *Reader) u32 {
        return std.mem.readInt(u32, self.take(4)[0..4], .little);
    }

    fn word64(self: *Reader) u64 {
        return std.mem.readInt(u64, self.take(8)[0..8], .little);
    }
};

test "an embedded entry is found by its key under this compiler, and placed as a cache file places it, and a module's digests by its key and link name" {
    const gpa = std.testing.allocator;
    const input: object_cache.KeyInput = .{ .object_input = @splat(7), .target = "x64musl", .opt = "dev", .inline_expects = true };
    var other = input;
    other.object_input = @splat(8);
    const first_entry = "first entry bytes";
    const second_entry = "second";
    const module: Module = .{ .artifact = @splat(1), .link_name = @splat(2), .digests = .{ .interface = @splat(3), .object_input = @splat(4) } };
    const encoded = try encode(gpa, build_options.compiler_artifact_hash, &.{module}, &.{
        .{ .input = input, .entry = first_entry },
        .{ .input = other, .entry = second_entry },
    });
    defer gpa.free(encoded);
    const table = try gpa.alignedAlloc(u8, .fromByteUnits(alignment), encoded.len);
    defer gpa.free(table);
    @memcpy(table, encoded);
    const units: Units = .{ .table = table };
    const found = units.find(object_cache.key(input)).?;
    try std.testing.expectEqualStrings(first_entry, found);
    try std.testing.expectEqual(entry_file_offset, (@intFromPtr(found.ptr) - @intFromPtr(table.ptr)) % alignment);
    try std.testing.expectEqualStrings(second_entry, units.find(object_cache.key(other)).?);
    var miss = input;
    miss.expect_sites = true;
    try std.testing.expectEqual(null, units.find(object_cache.key(miss)));
    try std.testing.expectEqual(null, Units.none.find(object_cache.key(input)));
    try std.testing.expectEqual(module.digests, units.moduleDigests(module.artifact, module.link_name).?);
    try std.testing.expectEqual(null, units.moduleDigests(module.artifact, @splat(5)));
}
