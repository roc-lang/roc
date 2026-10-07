//! Verifies that a relocatable wasm object is position-independent.
//!
//! Every wasm32 output starts as a relocatable object that another linker
//! consumes afterwards. A shared link (`wasm-ld -shared`, emscripten
//! `-sSIDE_MODULE`) rejects absolute data and table relocations with
//! "relocation R_WASM_MEMORY_ADDR_SLEB cannot be used against symbol ...;
//! recompile with -fPIC", so an object carrying one cannot be built into a
//! side module. This check fails on any such relocation.
//!
//! It also requires at least one PIC relocation, so the check cannot pass
//! vacuously on an object whose relocations were never parsed.
//!
//! Usage: wasm_pic_check <path to .a or .o>

const std = @import("std");

const ar_magic = "!<arch>\n";
const wasm_magic = "\x00asm";

/// Relocation types from the wasm linking ABI.
const Reloc = enum(u8) {
    table_index_sleb = 1,
    memory_addr_leb = 3,
    memory_addr_sleb = 4,
    memory_addr_i32 = 5,
    function_offset_i32 = 8,
    section_offset_i32 = 9,
    memory_addr_rel_sleb = 11,
    table_index_rel_sleb = 12,
    _,

    /// Absolute forms that make the object unlinkable as a shared module.
    fn isAbsolute(self: Reloc) bool {
        return self == .memory_addr_sleb or self == .table_index_sleb;
    }

    fn isPic(self: Reloc) bool {
        return self == .memory_addr_rel_sleb or self == .table_index_rel_sleb;
    }

    fn hasAddend(self: Reloc) bool {
        return switch (self) {
            .memory_addr_leb,
            .memory_addr_sleb,
            .memory_addr_i32,
            .function_offset_i32,
            .section_offset_i32,
            .memory_addr_rel_sleb,
            => true,
            .table_index_sleb,
            .table_index_rel_sleb,
            => false,
            _ => false,
        };
    }

    fn name(self: Reloc) []const u8 {
        return switch (self) {
            .table_index_sleb => "R_WASM_TABLE_INDEX_SLEB",
            .memory_addr_leb => "R_WASM_MEMORY_ADDR_LEB",
            .memory_addr_sleb => "R_WASM_MEMORY_ADDR_SLEB",
            .memory_addr_i32 => "R_WASM_MEMORY_ADDR_I32",
            .function_offset_i32 => "R_WASM_FUNCTION_OFFSET_I32",
            .section_offset_i32 => "R_WASM_SECTION_OFFSET_I32",
            .memory_addr_rel_sleb => "R_WASM_MEMORY_ADDR_REL_SLEB",
            .table_index_rel_sleb => "R_WASM_TABLE_INDEX_REL_SLEB",
            _ => "unknown",
        };
    }
};

const ParseError = error{ Truncated, BadModule };

const Cursor = struct {
    bytes: []const u8,
    pos: usize = 0,

    fn byte(self: *Cursor) ParseError!u8 {
        if (self.pos >= self.bytes.len) return error.Truncated;
        defer self.pos += 1;
        return self.bytes[self.pos];
    }

    fn uleb(self: *Cursor) ParseError!u32 {
        var result: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            const b = try self.byte();
            result |= @as(u32, b & 0x7f) << shift;
            if (b & 0x80 == 0) return result;
            shift = std.math.add(u5, shift, 7) catch return error.BadModule;
        }
    }

    fn skipSleb(self: *Cursor) ParseError!void {
        while (true) {
            const b = try self.byte();
            if (b & 0x80 == 0) return;
        }
    }

    fn take(self: *Cursor, len: usize) ParseError![]const u8 {
        if (self.pos + len > self.bytes.len) return error.Truncated;
        defer self.pos += len;
        return self.bytes[self.pos..][0..len];
    }
};

const Counts = struct {
    absolute: usize = 0,
    pic: usize = 0,
    first_absolute: ?Reloc = null,
};

/// Walk the `reloc.*` custom sections, ignoring the DWARF ones: debug info is
/// not executed, so its absolute relocations do not affect linkability.
fn countRelocations(obj: []const u8, counts: *Counts) ParseError!void {
    if (obj.len < 8 or !std.mem.eql(u8, obj[0..4], wasm_magic)) return error.BadModule;

    var cursor = Cursor{ .bytes = obj, .pos = 8 };
    while (cursor.pos < obj.len) {
        const section_id = try cursor.byte();
        const section_len = try cursor.uleb();
        const body = try cursor.take(section_len);
        if (section_id != 0) continue;

        var section = Cursor{ .bytes = body };
        const name_len = try section.uleb();
        const section_name = try section.take(name_len);
        if (!std.mem.startsWith(u8, section_name, "reloc.")) continue;
        if (std.mem.startsWith(u8, section_name, "reloc..debug")) continue;

        _ = try section.uleb(); // target section index
        const count = try section.uleb();
        for (0..count) |_| {
            const reloc: Reloc = @enumFromInt(try section.byte());
            _ = try section.uleb(); // offset
            _ = try section.uleb(); // symbol index
            if (reloc.hasAddend()) try section.skipSleb();

            if (reloc.isAbsolute()) {
                counts.absolute += 1;
                if (counts.first_absolute == null) counts.first_absolute = reloc;
            } else if (reloc.isPic()) {
                counts.pic += 1;
            }
        }
    }
}

/// Count relocations in every wasm member of an ar archive, or in the file
/// itself when it is a bare wasm object.
fn countFile(bytes: []const u8, counts: *Counts) ParseError!usize {
    if (!std.mem.startsWith(u8, bytes, ar_magic)) {
        try countRelocations(bytes, counts);
        return 1;
    }

    var members: usize = 0;
    var pos: usize = ar_magic.len;
    while (pos + 60 <= bytes.len) {
        const size_field = std.mem.trimEnd(u8, bytes[pos + 48 ..][0..10], " ");
        const size = std.fmt.parseInt(usize, size_field, 10) catch return error.BadModule;
        const body_start = pos + 60;
        if (body_start + size > bytes.len) return error.Truncated;
        const body = bytes[body_start..][0..size];
        if (std.mem.startsWith(u8, body, wasm_magic)) {
            try countRelocations(body, counts);
            members += 1;
        }
        pos = body_start + size + (size & 1);
    }
    return members;
}

pub fn main(init: std.process.Init) anyerror!void {
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var arg_iter = try std.process.Args.Iterator.initAllocator(init.minimal.args, arena);
    defer arg_iter.deinit();
    _ = arg_iter.skip();

    const path = arg_iter.next() orelse {
        std.debug.print("Usage: wasm_pic_check <path to .a or .o>\n", .{});
        return error.MissingPath;
    };

    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, path, arena, .unlimited);

    var counts = Counts{};
    const members = try countFile(bytes, &counts);

    if (members == 0) {
        std.debug.print("FAILED: {s} contains no wasm object to check\n", .{path});
        return error.NoWasmObject;
    }

    if (counts.absolute != 0) {
        std.debug.print(
            "FAILED: {s} has {d} absolute relocation(s) (e.g. {s}); a shared/SIDE_MODULE link rejects these\n",
            .{ path, counts.absolute, counts.first_absolute.?.name() },
        );
        return error.NotPositionIndependent;
    }

    if (counts.pic == 0) {
        std.debug.print(
            "FAILED: {s} has no position-independent relocations, so this check proved nothing\n",
            .{path},
        );
        return error.NothingChecked;
    }

    std.debug.print(
        "SUCCESS: {s} is position-independent ({d} PIC relocation(s) across {d} wasm object(s))\n",
        .{ path, counts.pic, members },
    );
}
