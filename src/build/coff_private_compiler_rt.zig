//! Build-time sealing of the compiler-rt a COFF shim archive carries.
//!
//! The interpreter shim is a static archive that `roc` links into a platform
//! executable, and Zig bundles its compiler-rt into that archive as one
//! `compiler_rt.obj` member. Every helper in it is a weak definition under its
//! public libcall name (`__udivti3`, `memcpy`, `__chkstk`, ...), which COFF
//! spells as a weak external `__udivti3` plus an ordinary external
//! `.weak.__udivti3.default`. A platform host carries the same helpers: a Rust
//! host has `compiler_builtins`, a Zig or C host has its own compiler-rt. ELF
//! and Mach-O let the two sets of weak definitions coexist. COFF does not: an
//! MSVC-ABI link rejects the second `.weak.<name>.default` as a duplicate
//! symbol, and rejects a second weak external whose default differs. Only
//! MinGW mode tolerates them.
//!
//! So the shim's compiler-rt must not take part in the platform's symbol
//! resolution at all. This tool gives every global the `compiler_rt.obj`
//! member defines a name private to the shim, in that member and in every
//! member that refers to it, and regenerates the archive's symbol indexes to
//! match. The shim keeps calling its own helpers, the platform keeps calling
//! its own, and neither can supply or interpose the other's.
//!
//! Nothing else changes: sections, relocations, and symbol indices are kept
//! byte for byte, so the renamed archive links exactly as before.
//!
//! Usage: coff_private_compiler_rt <input> <output>

const std = @import("std");
const archive = @import("archive_member_names.zig");

/// Prepended to every sealed symbol name.
pub const private_prefix = "roc_shim_private.";
/// The member Zig packs its bundled compiler-rt into.
const compiler_rt_member = "compiler_rt.obj";

const SealError = archive.RewriteError || error{
    MissingCompilerRtMember,
    DuplicateCompilerRtMember,
    MissingSymbolIndex,
    MalformedObject,
    UnsupportedCoff,
    MissingSymbolTable,
    TrailingObjectData,
};

const MainError = SealError || std.process.Args.ToSliceError ||
    std.Io.Dir.ReadFileAllocError || std.Io.Dir.WriteFileError || error{
    ExpectedInputAndOutputPaths,
};

/// Seal the archive at `args[1]`, writing the result to `args[2]`.
pub fn main(init: std.process.Init) MainError!void {
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) return error.ExpectedInputAndOutputPaths;
    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, args[1], arena, .unlimited);
    const sealed = seal(arena, bytes) catch |err| {
        std.debug.print("FAILED: {s}: {s}\n", .{ args[1], @errorName(err) });
        return err;
    };
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[2], .data = sealed });
}

const symbol_len = 18;
const storage_external = 2;
const storage_weak_external = 105;

/// The symbol and string tables of a COFF object. Every access is bounds
/// checked: a truncated or unfamiliar object must fail the build, never pass
/// through unsealed.
const Coff = struct {
    bytes: []const u8,
    symbols_offset: usize,
    symbol_count: usize,
    /// The string table, starting at its own 4-byte size field.
    strings_offset: usize,

    fn init(bytes: []const u8) SealError!Coff {
        if (bytes.len < 20) return error.MalformedObject;
        // Rejects the "big object" layout too, whose first field is zero.
        const machine = std.mem.readInt(u16, bytes[0..2], .little);
        if (machine != 0x8664 and machine != 0xaa64) return error.UnsupportedCoff;
        const symbols_offset: usize = std.mem.readInt(u32, bytes[8..12], .little);
        const symbol_count: usize = std.mem.readInt(u32, bytes[12..16], .little);
        if (symbols_offset == 0 or symbol_count == 0) return error.MissingSymbolTable;
        if (symbols_offset > bytes.len or symbol_count > (bytes.len - symbols_offset) / symbol_len) return error.MalformedObject;
        const strings_offset = symbols_offset + symbol_count * symbol_len;
        if (bytes.len - strings_offset < 4) return error.MalformedObject;
        const strings_len: usize = std.mem.readInt(u32, bytes[strings_offset..][0..4], .little);
        if (strings_len < 4 or strings_len > bytes.len - strings_offset) return error.MalformedObject;
        // Renamed symbols are appended to the string table, which is only
        // sound when nothing follows it.
        if (strings_offset + strings_len != bytes.len) return error.TrailingObjectData;
        return .{ .bytes = bytes, .symbols_offset = symbols_offset, .symbol_count = symbol_count, .strings_offset = strings_offset };
    }

    const Symbol = struct {
        /// Offset of the 18-byte record in the object.
        offset: usize,
        name: []const u8,
        /// Whether this object gives the name a definition a link can pick.
        defined: bool,
    };

    const Iterator = struct {
        coff: Coff,
        index: usize = 0,

        /// The next symbol a link resolves by name, skipping section, static,
        /// and auxiliary records.
        fn next(self: *Iterator) SealError!?Symbol {
            while (self.index < self.coff.symbol_count) {
                const offset = self.coff.symbols_offset + self.index * symbol_len;
                const record = self.coff.bytes[offset..][0..symbol_len];
                const aux: usize = record[17];
                if (aux > self.coff.symbol_count - self.index - 1) return error.MalformedObject;
                self.index += 1 + aux;
                const storage = record[16];
                if (storage != storage_external and storage != storage_weak_external) continue;
                const value = std.mem.readInt(u32, record[8..12], .little);
                const section = std.mem.readInt(i16, record[12..14], .little);
                return .{
                    .offset = offset,
                    .name = try self.coff.name(record),
                    // An external in section zero with no size is a plain
                    // reference; with a size it is a common definition.
                    .defined = storage == storage_weak_external or section != 0 or value != 0,
                };
            }
            return null;
        }
    };

    fn name(self: Coff, record: *const [symbol_len]u8) SealError![]const u8 {
        if (std.mem.readInt(u32, record[0..4], .little) != 0) return std.mem.sliceTo(record[0..8], 0);
        const strings = self.bytes[self.strings_offset..];
        const start: usize = std.mem.readInt(u32, record[4..8], .little);
        if (start < 4 or start >= strings.len) return error.MalformedObject;
        const len = std.mem.findScalar(u8, strings[start..], 0) orelse return error.MalformedObject;
        return strings[start..][0..len];
    }

    fn symbols(self: Coff) Iterator {
        return .{ .coff = self };
    }
};

const PrivateNames = std.StringHashMap(void);

/// Return `object` with every symbol named in `private` renamed. The new names
/// are appended to the string table; nothing before it moves.
fn sealObject(arena: std.mem.Allocator, object: []const u8, private: *const PrivateNames) SealError![]const u8 {
    const coff = try Coff.init(object);
    var out = std.ArrayList(u8).empty;
    try out.appendSlice(arena, object);
    var symbols = coff.symbols();
    while (try symbols.next()) |symbol| {
        if (!private.contains(symbol.name)) continue;
        const name_offset = out.items.len - coff.strings_offset;
        try out.appendSlice(arena, private_prefix);
        try out.appendSlice(arena, symbol.name);
        try out.append(arena, 0);
        const record = out.items[symbol.offset..][0..symbol_len];
        std.mem.writeInt(u32, record[0..4], 0, .little);
        std.mem.writeInt(u32, record[4..8], std.math.cast(u32, name_offset) orelse return error.Overflow, .little);
    }
    const strings_len = std.math.cast(u32, out.items.len - coff.strings_offset) orelse return error.Overflow;
    std.mem.writeInt(u32, out.items[coff.strings_offset..][0..4], strings_len, .little);
    return out.items;
}

const IndexEntry = struct {
    name: []const u8,
    /// Position in the parsed member list of the object defining `name`.
    member: usize,

    fn lessThan(_: void, left: IndexEntry, right: IndexEntry) bool {
        return std.mem.lessThan(u8, left.name, right.name);
    }
};

/// Read the first linker member: a big-endian symbol count, one big-endian
/// member offset per symbol, then that many NUL-terminated names.
fn readIndex(arena: std.mem.Allocator, body: []const u8, members: []const archive.Member, private: *const PrivateNames) SealError![]IndexEntry {
    if (body.len < 4) return error.MalformedArchive;
    const count: usize = std.mem.readInt(u32, body[0..4], .big);
    if (count > (body.len - 4) / 4) return error.MalformedArchive;
    const entries = try arena.alloc(IndexEntry, count);
    var names = body[4 + 4 * count ..];
    for (entries, 0..) |*entry, i| {
        const offset = std.mem.readInt(u32, body[4 + 4 * i ..][0..4], .big);
        const member = for (members, 0..) |member, position| {
            if (member.kind == .object and member.header_offset == offset) break position;
        } else return error.UnknownIndexOffset;
        const len = std.mem.findScalar(u8, names, 0) orelse return error.MalformedArchive;
        const name = names[0..len];
        names = names[len + 1 ..];
        entry.* = .{
            .name = if (private.contains(name)) try std.mem.concat(arena, u8, &.{ private_prefix, name }) else name,
            .member = member,
        };
    }
    return entries;
}

/// Give every global the archive's compiler-rt member defines a private name,
/// throughout the archive. `arena` owns the result and all scratch memory.
pub fn seal(arena: std.mem.Allocator, bytes: []const u8) SealError![]u8 {
    const parsed = try archive.parse(arena, bytes, .windows);
    if (parsed.bsd) return error.MalformedArchive;
    const members = parsed.members;

    var compiler_rt: ?[]const u8 = null;
    var first_index: ?[]const u8 = null;
    for (members) |member| switch (member.kind) {
        .object => if (std.mem.eql(u8, try archive.baseName(member.name), compiler_rt_member)) {
            if (compiler_rt != null) return error.DuplicateCompilerRtMember;
            compiler_rt = member.data;
        },
        .symbol_index => first_index = member.data,
        .symbol_index_64 => return error.MalformedArchive,
        .coff_second_index, .long_names => {},
    };

    var private = PrivateNames.init(arena);
    var defined = (try Coff.init(compiler_rt orelse return error.MissingCompilerRtMember)).symbols();
    while (try defined.next()) |symbol| {
        if (symbol.defined) try private.put(symbol.name, {});
    }

    const entries = try readIndex(arena, first_index orelse return error.MissingSymbolIndex, members, &private);
    const sorted = try arena.dupe(IndexEntry, entries);
    std.mem.sort(IndexEntry, sorted, {}, IndexEntry.lessThan);
    var names_len: usize = 0;
    for (entries) |entry| names_len += entry.name.len + 1;
    var object_count: usize = 0;
    for (members) |member| object_count += @intFromBool(member.kind == .object);

    // Lay out the new archive with every member in its original order.
    const bodies = try arena.alloc([]const u8, members.len);
    const offsets = try arena.alloc(usize, members.len);
    var total: usize = archive.magic.len;
    for (members, bodies, offsets) |member, *body, *offset| {
        body.* = switch (member.kind) {
            .object => try sealObject(arena, member.data, &private),
            .symbol_index => try arena.alloc(u8, 4 + 4 * entries.len + names_len),
            // Member count and offsets, symbol count and member numbers, names.
            .coff_second_index => try arena.alloc(u8, 4 + 4 * object_count + 4 + 2 * entries.len + names_len),
            .long_names => member.data,
            .symbol_index_64 => unreachable,
        };
        offset.* = total;
        total += archive.header_len + body.len + (body.len & 1);
    }

    // Both indexes name members by the offset of their header in this layout.
    // The second also numbers them, from one, in archive order.
    const numbers = try arena.alloc(u16, members.len);
    var next_number: usize = 0;
    for (members, numbers) |member, *number| {
        if (member.kind != .object) continue;
        next_number += 1;
        number.* = std.math.cast(u16, next_number) orelse return error.Overflow;
    }
    for (members, bodies) |member, body| switch (member.kind) {
        .symbol_index => {
            const index = @constCast(body);
            std.mem.writeInt(u32, index[0..4], std.math.cast(u32, entries.len) orelse return error.Overflow, .big);
            var names_at: usize = 4 + 4 * entries.len;
            for (entries, 0..) |entry, i| {
                std.mem.writeInt(u32, index[4 + 4 * i ..][0..4], std.math.cast(u32, offsets[entry.member]) orelse return error.Overflow, .big);
                names_at = writeName(index, names_at, entry.name);
            }
        },
        .coff_second_index => {
            const index = @constCast(body);
            std.mem.writeInt(u32, index[0..4], @intCast(object_count), .little);
            var at: usize = 4;
            for (members, offsets) |object, offset| {
                if (object.kind != .object) continue;
                std.mem.writeInt(u32, index[at..][0..4], std.math.cast(u32, offset) orelse return error.Overflow, .little);
                at += 4;
            }
            std.mem.writeInt(u32, index[at..][0..4], @intCast(sorted.len), .little);
            at += 4;
            for (sorted) |entry| {
                std.mem.writeInt(u16, index[at..][0..2], numbers[entry.member], .little);
                at += 2;
            }
            for (sorted) |entry| at = writeName(index, at, entry.name);
        },
        .object, .long_names => {},
        .symbol_index_64 => unreachable,
    };

    const out = try arena.alloc(u8, total);
    @memcpy(out[0..archive.magic.len], archive.magic);
    for (members, bodies, offsets) |member, body, offset| {
        const header = out[offset..][0..archive.header_len];
        header.* = member.header.*;
        try archive.setHeaderSize(header, body.len);
        @memcpy(out[offset + archive.header_len ..][0..body.len], body);
        if (body.len & 1 != 0) out[offset + archive.header_len + body.len] = '\n';
    }
    return out;
}

fn writeName(out: []u8, at: usize, name: []const u8) usize {
    @memcpy(out[at..][0..name.len], name);
    out[at + name.len] = 0;
    return at + name.len + 1;
}

const TestSymbol = struct {
    name: []const u8,
    /// Zero leaves the symbol undefined.
    section: i16 = 0,
    storage: u8 = storage_external,
    /// For a weak external: the symbol-table index of its default.
    weak_default: ?u32 = null,
};

/// Build a one-section x86_64 COFF object holding `symbols`, laid out the way
/// LLVM writes one: header, section table, symbol table, string table.
fn testObject(gpa: std.mem.Allocator, symbols: []const TestSymbol) ![]u8 {
    var records: u32 = 0;
    for (symbols) |symbol| records += if (symbol.weak_default != null) 2 else 1;
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(gpa);
    try out.appendNTimes(gpa, 0, 20 + 40);
    std.mem.writeInt(u16, out.items[0..2], 0x8664, .little);
    std.mem.writeInt(u16, out.items[2..4], 1, .little);
    std.mem.writeInt(u32, out.items[8..12], 20 + 40, .little);
    std.mem.writeInt(u32, out.items[12..16], records, .little);
    @memcpy(out.items[20..][0..5], ".text");
    var strings = std.ArrayList(u8).empty;
    defer strings.deinit(gpa);
    try strings.appendNTimes(gpa, 0, 4);
    for (symbols) |symbol| {
        var record: [symbol_len]u8 = @splat(0);
        if (symbol.name.len <= 8) {
            @memcpy(record[0..symbol.name.len], symbol.name);
        } else {
            std.mem.writeInt(u32, record[4..8], @intCast(strings.items.len), .little);
            try strings.appendSlice(gpa, symbol.name);
            try strings.append(gpa, 0);
        }
        std.mem.writeInt(i16, record[12..14], symbol.section, .little);
        record[16] = symbol.storage;
        try out.appendSlice(gpa, &record);
        if (symbol.weak_default) |default| {
            out.items[out.items.len - 1] = 1;
            var aux: [symbol_len]u8 = @splat(0);
            std.mem.writeInt(u32, aux[0..4], default, .little);
            // IMAGE_WEAK_EXTERN_SEARCH_ALIAS
            std.mem.writeInt(u32, aux[4..8], 3, .little);
            try out.appendSlice(gpa, &aux);
        }
    }
    std.mem.writeInt(u32, strings.items[0..4], @intCast(strings.items.len), .little);
    try out.appendSlice(gpa, strings.items);
    return out.toOwnedSlice(gpa);
}

fn expectGlobals(object: []const u8, expected: []const []const u8) !void {
    var symbols = (try Coff.init(object)).symbols();
    for (expected) |name| {
        const symbol = (try symbols.next()) orelse return error.TestExpectedEqual;
        try std.testing.expectEqualStrings(name, symbol.name);
    }
    try std.testing.expectEqual(null, try symbols.next());
}

const test_shim = [_]TestSymbol{
    .{ .name = "roc_entrypoint", .section = 1 },
    .{ .name = "memcpy" },
    .{ .name = "__udivti3" },
    .{ .name = "roc_alloc" },
};
const test_compiler_rt = [_]TestSymbol{
    .{ .name = ".text", .section = 1, .storage = 3 },
    .{ .name = ".weak.memcpy.default", .section = 1 },
    .{ .name = "memcpy", .storage = storage_weak_external, .weak_default = 1 },
    .{ .name = ".weak.__udivti3.default", .section = 1 },
    .{ .name = "__udivti3", .storage = storage_weak_external, .weak_default = 4 },
    .{ .name = "_tls_index" },
};

test "compiler-rt globals get private names in every member and both indexes" {
    var arena_impl = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();
    const shim = try testObject(arena, &test_shim);
    const compiler_rt = try testObject(arena, &test_compiler_rt);
    const input = try archive.testArchive(arena, .windows, &.{
        .{ .name = "roc_interpreter_shim_zcu.obj", .data = shim },
        .{ .name = compiler_rt_member, .data = compiler_rt },
    }, &.{
        .{ .name = "roc_entrypoint", .object = 0 },
        .{ .name = ".weak.memcpy.default", .object = 1 },
        .{ .name = "memcpy", .object = 1 },
        .{ .name = ".weak.__udivti3.default", .object = 1 },
        .{ .name = "__udivti3", .object = 1 },
    }, true);

    const output = try seal(arena, input);
    const parsed = try archive.parse(arena, output, .windows);
    const members = parsed.members;
    try std.testing.expectEqual(5, members.len);
    try std.testing.expectEqualStrings("roc_interpreter_shim_zcu.obj", members[3].name);
    try std.testing.expectEqualStrings(compiler_rt_member, members[4].name);

    // The shim's references follow the definitions they resolved to; what the
    // platform supplies keeps its name.
    try expectGlobals(members[3].data, &.{
        "roc_entrypoint",
        private_prefix ++ "memcpy",
        private_prefix ++ "__udivti3",
        "roc_alloc",
    });
    try expectGlobals(members[4].data, &.{
        private_prefix ++ ".weak.memcpy.default",
        private_prefix ++ "memcpy",
        private_prefix ++ ".weak.__udivti3.default",
        private_prefix ++ "__udivti3",
        "_tls_index",
    });
    // Everything ahead of the string table is untouched apart from the name
    // fields, so sections, relocations, and symbol indices still line up.
    try std.testing.expectEqualSlices(u8, compiler_rt[0 .. 20 + 40], members[4].data[0 .. 20 + 40]);
    try std.testing.expectEqualSlices(u8, compiler_rt[20 + 40 + 8 ..][0..10], members[4].data[20 + 40 + 8 ..][0..10]);

    const first = members[0].data;
    try std.testing.expectEqual(5, std.mem.readInt(u32, first[0..4], .big));
    const expected_first = [_]struct { []const u8, usize }{
        .{ "roc_entrypoint", 3 },
        .{ private_prefix ++ ".weak.memcpy.default", 4 },
        .{ private_prefix ++ "memcpy", 4 },
        .{ private_prefix ++ ".weak.__udivti3.default", 4 },
        .{ private_prefix ++ "__udivti3", 4 },
    };
    var names = first[4 + 4 * 5 ..];
    for (expected_first, 0..) |expected, i| {
        try std.testing.expectEqual(members[expected[1]].header_offset, std.mem.readInt(u32, first[4 + 4 * i ..][0..4], .big));
        try std.testing.expectEqualStrings(expected[0], std.mem.sliceTo(names, 0));
        names = names[expected[0].len + 1 ..];
    }
    try std.testing.expectEqual(0, names.len);

    // The second linker member lists the same symbols sorted by name.
    const second = members[1].data;
    try std.testing.expectEqual(2, std.mem.readInt(u32, second[0..4], .little));
    try std.testing.expectEqual(members[3].header_offset, std.mem.readInt(u32, second[4..8], .little));
    try std.testing.expectEqual(members[4].header_offset, std.mem.readInt(u32, second[8..12], .little));
    try std.testing.expectEqual(5, std.mem.readInt(u32, second[12..16], .little));
    const expected_second = [_]struct { []const u8, u16 }{
        .{ "roc_entrypoint", 1 },
        .{ private_prefix ++ ".weak.__udivti3.default", 2 },
        .{ private_prefix ++ ".weak.memcpy.default", 2 },
        .{ private_prefix ++ "__udivti3", 2 },
        .{ private_prefix ++ "memcpy", 2 },
    };
    names = second[16 + 2 * 5 ..];
    for (expected_second, 0..) |expected, i| {
        try std.testing.expectEqual(expected[1], std.mem.readInt(u16, second[16 + 2 * i ..][0..2], .little));
        try std.testing.expectEqualStrings(expected[0], std.mem.sliceTo(names, 0));
        names = names[expected[0].len + 1 ..];
    }
    try std.testing.expectEqual(0, names.len);
}

test "an archive that cannot be sealed fails closed" {
    var arena_impl = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();
    const shim = try testObject(arena, &test_shim);
    const compiler_rt = try testObject(arena, &test_compiler_rt);
    const index = [_]archive.TestSymbol{.{ .name = "roc_entrypoint", .object = 0 }};

    // Without the member to seal, the shim's compiler-rt would stay public.
    try std.testing.expectError(error.MissingCompilerRtMember, seal(arena, try archive.testArchive(arena, .windows, &.{
        .{ .name = "roc_interpreter_shim_zcu.obj", .data = shim },
    }, &index, true)));
    try std.testing.expectError(error.DuplicateCompilerRtMember, seal(arena, try archive.testArchive(arena, .windows, &.{
        .{ .name = "a\\" ++ compiler_rt_member, .data = compiler_rt },
        .{ .name = "b\\" ++ compiler_rt_member, .data = compiler_rt },
    }, &index, true)));

    // A big object starts with a zero machine field.
    const big = try arena.dupe(u8, shim);
    std.mem.writeInt(u16, big[0..2], 0, .little);
    try std.testing.expectError(error.UnsupportedCoff, seal(arena, try archive.testArchive(arena, .windows, &.{
        .{ .name = "roc_interpreter_shim_zcu.obj", .data = big },
        .{ .name = compiler_rt_member, .data = compiler_rt },
    }, &index, true)));

    // New names are appended to the string table, so it must come last.
    const trailing = try std.mem.concat(arena, u8, &.{ shim, "tail" });
    try std.testing.expectError(error.TrailingObjectData, seal(arena, try archive.testArchive(arena, .windows, &.{
        .{ .name = "roc_interpreter_shim_zcu.obj", .data = trailing },
        .{ .name = compiler_rt_member, .data = compiler_rt },
    }, &index, true)));
}
