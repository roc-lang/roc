//! Loads small hand-assembled objects and runs them: each format is written
//! by a minimal builder here, patched by the loader, and executed on the host.

const std = @import("std");
const builtin = @import("builtin");
const loader = @import("mod.zig");
const memory = @import("memory.zig");

const testing = std.testing;

fn hostAnswer() callconv(.c) i32 {
    return 41;
}

var host_value: u32 = 1234;

const HostSymbols = struct {
    far_answer: usize = 0,

    fn resolve(raw: ?*anyopaque, name: []const u8) ?usize {
        const self: *const HostSymbols = @ptrCast(@alignCast(raw.?));
        if (std.mem.eql(u8, name, "host_answer")) return @intFromPtr(&hostAnswer);
        if (std.mem.eql(u8, name, "host_value")) return @intFromPtr(&host_value);
        if (std.mem.eql(u8, name, "far_answer") and self.far_answer != 0) return self.far_answer;
        return null;
    }

    fn resolver(self: *HostSymbols) loader.Resolver {
        return .{ .context = self, .resolve = resolve };
    }
};

fn writeLe(list: *std.ArrayList(u8), allocator: std.mem.Allocator, comptime T: type, value: T) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, value, .little);
    try list.appendSlice(allocator, &buf);
}

fn padTo(list: *std.ArrayList(u8), allocator: std.mem.Allocator, alignment: usize) !void {
    while (list.items.len % alignment != 0) try list.append(allocator, 0);
}

/// A relocatable ELF object with `.text`, `.data`, and their relocations.
const ElfSpec = struct {
    const Symbol = struct { name: []const u8, section: u16, value: u64 };
    const Rela = struct { offset: u64, symbol: u32, kind: u32, addend: i64 };

    text: []const u8,
    data: []const u8 = &.{},
    /// Symbol 0 is the null symbol; these start at index 1.
    symbols: []const Symbol,
    text_relocs: []const Rela = &.{},
    data_relocs: []const Rela = &.{},

    fn build(spec: ElfSpec, allocator: std.mem.Allocator) ![]u8 {
        const machine: u16 = switch (builtin.cpu.arch) {
            .x86_64 => 62,
            .aarch64 => 183,
            else => return error.SkipZigTest,
        };
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(allocator);
        // Header, filled in at the end.
        try out.appendNTimes(allocator, 0, 64);

        var strtab: std.ArrayList(u8) = .empty;
        defer strtab.deinit(allocator);
        try strtab.append(allocator, 0);
        var symtab: std.ArrayList(u8) = .empty;
        defer symtab.deinit(allocator);
        try symtab.appendNTimes(allocator, 0, 24);
        for (spec.symbols) |symbol| {
            const name_offset: u32 = @intCast(strtab.items.len);
            try strtab.appendSlice(allocator, symbol.name);
            try strtab.append(allocator, 0);
            try writeLe(&symtab, allocator, u32, name_offset);
            try symtab.append(allocator, (1 << 4) | 0); // STB_GLOBAL, STT_NOTYPE
            try symtab.append(allocator, 0);
            try writeLe(&symtab, allocator, u16, symbol.section);
            try writeLe(&symtab, allocator, u64, symbol.value);
            try writeLe(&symtab, allocator, u64, 0);
        }
        const shstrtab = "\x00.text\x00.data\x00.symtab\x00.strtab\x00.shstrtab\x00.rela.text\x00.rela.data\x00";

        const Placed = struct { offset: usize, size: usize };
        var placed: [8]Placed = undefined;
        const bodies = [_][]const u8{ &.{}, spec.text, spec.data, symtab.items, strtab.items, shstrtab };
        for (bodies, 0..) |body, i| {
            try padTo(&out, allocator, 16);
            placed[i] = .{ .offset = out.items.len, .size = body.len };
            try out.appendSlice(allocator, body);
        }
        for ([_][]const ElfSpec.Rela{ spec.text_relocs, spec.data_relocs }, 6..) |relas, i| {
            try padTo(&out, allocator, 8);
            placed[i] = .{ .offset = out.items.len, .size = relas.len * 24 };
            for (relas) |rela| {
                try writeLe(&out, allocator, u64, rela.offset);
                try writeLe(&out, allocator, u64, (@as(u64, rela.symbol) << 32) | rela.kind);
                try writeLe(&out, allocator, u64, @bitCast(rela.addend));
            }
        }

        try padTo(&out, allocator, 8);
        const shoff = out.items.len;
        const names = [_]u32{ 0, 1, 7, 13, 21, 29, 39, 50 };
        const types = [_]u32{ 0, 1, 1, 2, 3, 3, 4, 4 };
        const flags = [_]u64{ 0, 0x6, 0x3, 0, 0, 0, 0, 0 };
        const links = [_]u32{ 0, 0, 0, 4, 0, 0, 3, 3 };
        const infos = [_]u32{ 0, 0, 0, 1, 0, 0, 1, 2 };
        const entsizes = [_]u64{ 0, 0, 0, 24, 0, 0, 24, 24 };
        for (0..8) |i| {
            try writeLe(&out, allocator, u32, names[i]);
            try writeLe(&out, allocator, u32, types[i]);
            try writeLe(&out, allocator, u64, flags[i]);
            try writeLe(&out, allocator, u64, 0);
            try writeLe(&out, allocator, u64, placed[i].offset);
            try writeLe(&out, allocator, u64, placed[i].size);
            try writeLe(&out, allocator, u32, links[i]);
            try writeLe(&out, allocator, u32, infos[i]);
            try writeLe(&out, allocator, u64, if (i == 0) 0 else 16);
            try writeLe(&out, allocator, u64, entsizes[i]);
        }

        const header = out.items[0..64];
        header[0..4].* = "\x7fELF".*;
        header[4] = 2;
        header[5] = 1;
        header[6] = 1;
        std.mem.writeInt(u16, header[16..18], 1, .little); // ET_REL
        std.mem.writeInt(u16, header[18..20], machine, .little);
        std.mem.writeInt(u32, header[20..24], 1, .little);
        std.mem.writeInt(u64, header[40..48], shoff, .little);
        std.mem.writeInt(u16, header[52..54], 64, .little);
        std.mem.writeInt(u16, header[58..60], 64, .little);
        std.mem.writeInt(u16, header[60..62], 8, .little);
        std.mem.writeInt(u16, header[62..64], 5, .little);
        return out.toOwnedSlice(allocator);
    }
};

const EntryFn = *const fn () callconv(.c) i32;

/// `entry` calls `host_answer` and adds one.
fn callAndAddOneSpec() ElfSpec {
    return switch (builtin.cpu.arch) {
        .x86_64 => .{
            // call rel32; add eax, 1; ret
            .text = &.{ 0xe8, 0, 0, 0, 0, 0x83, 0xc0, 0x01, 0xc3 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "host_answer", .section = 0, .value = 0 } },
            .text_relocs = &.{.{ .offset = 1, .symbol = 2, .kind = 4, .addend = -4 }}, // R_X86_64_PLT32
        },
        .aarch64 => .{
            // stp x29, x30, [sp, #-16]!; bl; add w0, w0, #1; ldp x29, x30, [sp], #16; ret
            .text = &.{ 0xfd, 0x7b, 0xbf, 0xa9, 0, 0, 0, 0x94, 0x00, 0x04, 0x00, 0x11, 0xfd, 0x7b, 0xc1, 0xa8, 0xc0, 0x03, 0x5f, 0xd6 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "host_answer", .section = 0, .value = 0 } },
            .text_relocs = &.{.{ .offset = 4, .symbol = 2, .kind = 283, .addend = 0 }}, // R_AARCH64_CALL26
        },
        else => unreachable,
    };
}

test "a call to a resolved host function" {
    if (builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    const object = try callAndAddOneSpec().build(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i32, 42), entry());
}

test "a far call goes through a stub" {
    if (builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    // Put a tiny function far away from this binary; if the address space
    // hands back something closer, the call is simply direct.
    const far_hint = @intFromPtr(&hostAnswer) + (1 << 33);
    var far = try memory.Mapping.map(std.heap.page_size_min, far_hint + (1 << 28));
    defer far.unmap();
    switch (builtin.cpu.arch) {
        .x86_64 => far.base[0..6].* = .{ 0xb8, 0x06, 0, 0, 0, 0xc3 }, // mov eax, 6; ret
        .aarch64 => {
            std.mem.writeInt(u32, far.base[0..4], 0x528000c0, .little); // mov w0, #6
            std.mem.writeInt(u32, far.base[4..8], 0xd65f03c0, .little); // ret
        },
        else => return error.SkipZigTest,
    }
    try far.protectCode(std.heap.page_size_min);

    var spec = callAndAddOneSpec();
    spec.symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "far_answer", .section = 0, .value = 0 } };
    const object = try spec.build(allocator);
    defer allocator.free(object);
    var host = HostSymbols{ .far_answer = @intFromPtr(far.base) };
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i32, 7), entry());
}

test "a GOT-relative load of a host variable" {
    if (builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    const spec: ElfSpec = switch (builtin.cpu.arch) {
        .x86_64 => .{
            // mov rax, [rip + host_value@GOTPCREL]; mov eax, [rax]; ret
            .text = &.{ 0x48, 0x8b, 0x05, 0, 0, 0, 0, 0x8b, 0x00, 0xc3 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "host_value", .section = 0, .value = 0 } },
            .text_relocs = &.{.{ .offset = 3, .symbol = 2, .kind = 42, .addend = -4 }}, // R_X86_64_REX_GOTPCRELX
        },
        .aarch64 => .{
            // adrp x8, :got:host_value; ldr x8, [x8, :got_lo12:host_value]; ldr w0, [x8]; ret
            .text = &.{ 0x08, 0x00, 0x00, 0x90, 0x08, 0x01, 0x40, 0xf9, 0x00, 0x01, 0x40, 0xb9, 0xc0, 0x03, 0x5f, 0xd6 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "host_value", .section = 0, .value = 0 } },
            .text_relocs = &.{
                .{ .offset = 0, .symbol = 2, .kind = 311, .addend = 0 }, // R_AARCH64_ADR_GOT_PAGE
                .{ .offset = 4, .symbol = 2, .kind = 312, .addend = 0 }, // R_AARCH64_LD64_GOT_LO12_NC
            },
        },
        else => return error.SkipZigTest,
    };
    const object = try spec.build(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    host_value = 5678;
    try testing.expectEqual(@as(i32, 5678), entry());
}

test "data holds an absolute pointer to code and code reads data" {
    if (builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    const spec: ElfSpec = switch (builtin.cpu.arch) {
        .x86_64 => .{
            // mov eax, [rip + value]; ret     (value lives in .data at offset 8)
            .text = &.{ 0x8b, 0x05, 0, 0, 0, 0, 0xc3 },
            .data = &.{ 0, 0, 0, 0, 0, 0, 0, 0, 0x2a, 0, 0, 0 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "value", .section = 2, .value = 8 }, .{ .name = "pointer", .section = 2, .value = 0 } },
            .text_relocs = &.{.{ .offset = 2, .symbol = 2, .kind = 2, .addend = -4 }}, // R_X86_64_PC32
            .data_relocs = &.{.{ .offset = 0, .symbol = 1, .kind = 1, .addend = 0 }}, // R_X86_64_64
        },
        .aarch64 => .{
            // adrp x8, value; ldr w0, [x8, :lo12:value]; ret
            .text = &.{ 0x08, 0x00, 0x00, 0x90, 0x00, 0x01, 0x40, 0xb9, 0xc0, 0x03, 0x5f, 0xd6 },
            .data = &.{ 0, 0, 0, 0, 0, 0, 0, 0, 0x2a, 0, 0, 0 },
            .symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "value", .section = 2, .value = 8 }, .{ .name = "pointer", .section = 2, .value = 0 } },
            .text_relocs = &.{
                .{ .offset = 0, .symbol = 2, .kind = 275, .addend = 0 }, // R_AARCH64_ADR_PREL_PG_HI21
                .{ .offset = 4, .symbol = 2, .kind = 285, .addend = 0 }, // R_AARCH64_LDST32_ABS_LO12_NC
            },
            .data_relocs = &.{.{ .offset = 0, .symbol = 1, .kind = 257, .addend = 0 }}, // R_AARCH64_ABS64
        },
        else => return error.SkipZigTest,
    };
    const object = try spec.build(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i32, 42), entry());
    const pointer = image.lookup(*const usize, "pointer") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@intFromPtr(entry), pointer.*);
}

test "an undefined symbol nothing resolves is an error naming it" {
    if (builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    var spec = callAndAddOneSpec();
    spec.symbols = &.{ .{ .name = "entry", .section = 1, .value = 0 }, .{ .name = "nobody_defines_this", .section = 0, .value = 0 } };
    const object = try spec.build(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    try testing.expectError(error.UndefinedSymbol, loader.Image.load(allocator, object, host.resolver()));
}

/// A minimal x64 COFF object: one `.text` section whose function calls
/// `host_answer` and adds one.
fn buildCoffObject(allocator: std.mem.Allocator) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    const text = [_]u8{ 0xe8, 0, 0, 0, 0, 0x83, 0xc0, 0x01, 0xc3 };
    const text_offset: u32 = 20 + 40;
    const reloc_offset: u32 = text_offset + text.len;
    const symtab_offset: u32 = reloc_offset + 10;
    // File header
    try writeLe(&out, allocator, u16, 0x8664);
    try writeLe(&out, allocator, u16, 1);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, symtab_offset);
    try writeLe(&out, allocator, u32, 2);
    try writeLe(&out, allocator, u16, 0);
    try writeLe(&out, allocator, u16, 0);
    // Section header: .text
    try out.appendSlice(allocator, ".text\x00\x00\x00");
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, text.len);
    try writeLe(&out, allocator, u32, text_offset);
    try writeLe(&out, allocator, u32, reloc_offset);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u16, 1);
    try writeLe(&out, allocator, u16, 0);
    try writeLe(&out, allocator, u32, 0x60500020); // code | execute | read | align 16
    try out.appendSlice(allocator, &text);
    // Relocation: REL32 at offset 1 against symbol 1
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u16, 4);
    // Symbols: entry (section 1), host_answer (undefined)
    try out.appendSlice(allocator, "entry\x00\x00\x00");
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u16, 1);
    try writeLe(&out, allocator, u16, 0x20);
    try out.append(allocator, 2);
    try out.append(allocator, 0);
    try out.appendSlice(allocator, "host_ans");
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u16, 0);
    try writeLe(&out, allocator, u16, 0x20);
    try out.append(allocator, 2);
    try out.append(allocator, 0);
    // The second name needs the string table: rewrite it as a long name.
    const second = out.items[symtab_offset + 18 ..][0..8];
    @memset(second, 0);
    std.mem.writeInt(u32, second[4..8], 4, .little);
    try writeLe(&out, allocator, u32, 4 + "host_answer".len + 1);
    try out.appendSlice(allocator, "host_answer\x00");
    return out.toOwnedSlice(allocator);
}

test "a COFF object's call is bound and run" {
    if (builtin.cpu.arch != .x86_64 or builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    const object = try buildCoffObject(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i32, 42), entry());
}

/// A minimal x86-64 Mach-O object with the same function.
fn buildMachOObject(allocator: std.mem.Allocator) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    const text = [_]u8{ 0xe8, 0, 0, 0, 0, 0x83, 0xc0, 0x01, 0xc3 };
    const header_size: u32 = 32;
    const segment_size: u32 = 72 + 80;
    const symtab_cmd_size: u32 = 24;
    const text_offset: u32 = header_size + segment_size + symtab_cmd_size;
    const reloc_offset: u32 = text_offset + text.len;
    const symoff: u32 = reloc_offset + 8;
    const stroff: u32 = symoff + 2 * 16;
    const strtab = "\x00_entry\x00_host_answer\x00";
    // Header
    try writeLe(&out, allocator, u32, 0xfeedfacf);
    try writeLe(&out, allocator, u32, 0x01000007);
    try writeLe(&out, allocator, u32, 3);
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u32, 2);
    try writeLe(&out, allocator, u32, segment_size + symtab_cmd_size);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, 0);
    // LC_SEGMENT_64 with one section
    try writeLe(&out, allocator, u32, 0x19);
    try writeLe(&out, allocator, u32, segment_size);
    try out.appendSlice(allocator, "\x00" ** 16);
    try writeLe(&out, allocator, u64, 0);
    try writeLe(&out, allocator, u64, text.len);
    try writeLe(&out, allocator, u64, text_offset);
    try writeLe(&out, allocator, u64, text.len);
    try writeLe(&out, allocator, u32, 7);
    try writeLe(&out, allocator, u32, 7);
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u32, 0);
    try out.appendSlice(allocator, "__text\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00");
    try out.appendSlice(allocator, "__TEXT\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00");
    try writeLe(&out, allocator, u64, 0);
    try writeLe(&out, allocator, u64, text.len);
    try writeLe(&out, allocator, u32, text_offset);
    try writeLe(&out, allocator, u32, 4);
    try writeLe(&out, allocator, u32, reloc_offset);
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u32, 0x80000400);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, 0);
    try writeLe(&out, allocator, u32, 0);
    // LC_SYMTAB
    try writeLe(&out, allocator, u32, 0x2);
    try writeLe(&out, allocator, u32, symtab_cmd_size);
    try writeLe(&out, allocator, u32, symoff);
    try writeLe(&out, allocator, u32, 2);
    try writeLe(&out, allocator, u32, stroff);
    try writeLe(&out, allocator, u32, strtab.len);
    try out.appendSlice(allocator, &text);
    // Relocation: X86_64_RELOC_BRANCH, pcrel, length 2, extern, symbol 1, at offset 1
    try writeLe(&out, allocator, u32, 1);
    try writeLe(&out, allocator, u32, 1 | (1 << 24) | (2 << 25) | (1 << 27) | (2 << 28));
    // Symbols
    try writeLe(&out, allocator, u32, 1); // _entry
    try out.append(allocator, 0x0f); // N_SECT | N_EXT
    try out.append(allocator, 1);
    try writeLe(&out, allocator, u16, 0);
    try writeLe(&out, allocator, u64, 0);
    try writeLe(&out, allocator, u32, 8); // _host_answer
    try out.append(allocator, 0x01); // N_UNDF | N_EXT
    try out.append(allocator, 0);
    try writeLe(&out, allocator, u16, 0);
    try writeLe(&out, allocator, u64, 0);
    try out.appendSlice(allocator, strtab);
    return out.toOwnedSlice(allocator);
}

test "a Mach-O object's call is bound and run" {
    if (builtin.cpu.arch != .x86_64 or builtin.os.tag == .freestanding) return error.SkipZigTest;
    const allocator = testing.allocator;
    const object = try buildMachOObject(allocator);
    defer allocator.free(object);
    var host = HostSymbols{};
    var image = try loader.Image.load(allocator, object, host.resolver());
    defer image.deinit();
    const entry = image.lookup(EntryFn, "entry") orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(i32, 42), entry());
}
