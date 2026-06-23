//! Apply an ELF shared object's own dynamic relocations after it has been
//! mapped by a relocation-free loader.
//!
//! Zig's `std.DynLib` resolves to `ElfDynLib` in a static, no-libc binary (the
//! eval-test runner is static-musl). `ElfDynLib.open` maps the `PT_LOAD`
//! segments and wires up symbol lookup, but its dynamic-vector loop ignores
//! every relocation entry (`DT_RELA`/`DT_JMPREL`/...): it applies **zero**
//! relocations. Any reference the linker left as a `RELATIVE`/`GLOB_DAT`/
//! `JUMP_SLOT` slot therefore stays whatever was baked into the file image
//! (usually `0`), so reading or calling through it faults.
//!
//! For an in-process eval object linked `-shared -Bsymbolic -z now`, the
//! remaining dynamic relocations are self-contained: `RELATIVE` entries that
//! resolve to `base + addend`, and (rarely) `GLOB_DAT`/`JUMP_SLOT` entries
//! against symbols defined in the same image. Applying them here makes the
//! mapped image self-consistent without reinventing the loader's mmap/symbol
//! work, and without relying on codegen never emitting a relocation (which is
//! what protected visibility only narrows, not guarantees).
//!
//! This is scoped to the eval/test in-process loader. The real `roc build`
//! pipeline links a normal executable that the OS dynamic loader relocates.

const std = @import("std");
const builtin = @import("builtin");
const elf = std.elf;

/// aarch64 dynamic relocation types.
const R_AARCH64_ABS64: u32 = 257;
const R_AARCH64_GLOB_DAT: u32 = 1025;
const R_AARCH64_JUMP_SLOT: u32 = 1026;
const R_AARCH64_RELATIVE: u32 = 1027;

/// x86_64 dynamic relocation types.
const R_X86_64_64: u32 = 1;
const R_X86_64_GLOB_DAT: u32 = 6;
const R_X86_64_JUMP_SLOT: u32 = 7;
const R_X86_64_RELATIVE: u32 = 8;

/// Resolves an undefined dynamic symbol (by name) to an absolute address in
/// the running host process, or null when it is not a symbol the caller can
/// provide. Used to bind the compiler-rt runtime libcalls that native codegen
/// emits but the self-contained eval image does not define (there is no dynamic
/// linker to bind them for the in-process loader).
pub const UndefinedSymbolResolver = *const fn (name: []const u8) ?usize;

const RelocKind = enum { relative, symbol, unsupported };

fn classifyReloc(r_type: u32) RelocKind {
    return switch (builtin.cpu.arch) {
        .aarch64 => switch (r_type) {
            R_AARCH64_RELATIVE => .relative,
            R_AARCH64_GLOB_DAT, R_AARCH64_JUMP_SLOT, R_AARCH64_ABS64 => .symbol,
            else => .unsupported,
        },
        .x86_64 => switch (r_type) {
            R_X86_64_RELATIVE => .relative,
            R_X86_64_GLOB_DAT, R_X86_64_JUMP_SLOT, R_X86_64_64 => .symbol,
            else => .unsupported,
        },
        else => .unsupported,
    };
}

/// Whether this build's `std.DynLib` is the relocation-free `ElfDynLib` (a
/// static, no-libc — i.e. static-musl — linux binary). For every other
/// configuration `std.DynLib` defers to the OS loader, which relocates for us.
pub const loader_skips_relocations = builtin.os.tag == .linux and
    (!builtin.link_libc or (builtin.abi == .musl and builtin.link_mode == .static)) and
    (builtin.cpu.arch == .aarch64 or builtin.cpu.arch == .x86_64);

/// Apply the dynamic relocations of an ELF image already mapped at `base`.
///
/// `base` is the load address of an `ET_DYN` image whose vaddr-0 maps to
/// `base` (i.e. the value of `ElfDynLib.memory.ptr`). `RELATIVE` and in-image
/// `GLOB_DAT`/`JUMP_SLOT`/absolute relocations are applied directly. Entries
/// against undefined symbols are handed to `resolver` (when one is supplied):
/// native codegen can emit compiler-rt runtime libcalls (`__divti3`, ...) that
/// the image itself does not define, and there is no dynamic linker to bind
/// them, so the resolver maps each to its host implementation. Symbols neither
/// in-image nor resolvable are left untouched rather than written with garbage.
///
/// The caller decides *whether* to invoke this (see `loader_skips_relocations`);
/// the work here is platform-independent so it can be unit-tested everywhere.
pub fn applyDynamicRelocations(base: usize, resolver: ?UndefinedSymbolResolver) void {
    const ehdr: *const elf.Ehdr = @ptrFromInt(base);

    // Locate PT_DYNAMIC and the symbol table from the program headers / dynamic
    // vector. The image's vaddrs are biased by `base`.
    var dyn: ?[*]const elf.Dyn = null;
    {
        var i: usize = 0;
        var ph_addr: usize = base + ehdr.e_phoff;
        while (i < ehdr.e_phnum) : ({
            i += 1;
            ph_addr += ehdr.e_phentsize;
        }) {
            const ph: *const elf.Phdr = @ptrFromInt(ph_addr);
            if (ph.p_type == elf.PT_DYNAMIC) {
                dyn = @ptrFromInt(base + ph.p_vaddr);
                break;
            }
        }
    }
    const dynv = dyn orelse return;

    var rela: usize = 0;
    var rela_size: usize = 0;
    var rela_ent: usize = @sizeOf(elf.Elf64_Rela);
    var jmprel: usize = 0;
    var pltrel_size: usize = 0;
    var symtab: usize = 0;
    var syment: usize = @sizeOf(elf.Elf64_Sym);
    var strtab: usize = 0;
    {
        var i: usize = 0;
        while (dynv[i].d_tag != elf.DT_NULL) : (i += 1) {
            const val = dynv[i].d_val;
            switch (dynv[i].d_tag) {
                elf.DT_RELA => rela = base + val,
                elf.DT_RELASZ => rela_size = val,
                elf.DT_RELAENT => rela_ent = val,
                elf.DT_JMPREL => jmprel = base + val,
                elf.DT_PLTRELSZ => pltrel_size = val,
                elf.DT_SYMTAB => symtab = base + val,
                elf.DT_SYMENT => syment = val,
                elf.DT_STRTAB => strtab = base + val,
                else => {},
            }
        }
    }

    applyRelaTable(base, rela, rela_size, rela_ent, symtab, syment, strtab, resolver);
    applyRelaTable(base, jmprel, pltrel_size, rela_ent, symtab, syment, strtab, resolver);
}

fn applyRelaTable(
    base: usize,
    table: usize,
    size: usize,
    ent: usize,
    symtab: usize,
    syment: usize,
    strtab: usize,
    resolver: ?UndefinedSymbolResolver,
) void {
    if (table == 0 or size == 0 or ent == 0) return;

    var off: usize = 0;
    while (off + @sizeOf(elf.Elf64_Rela) <= size) : (off += ent) {
        const r: *const elf.Elf64_Rela = @ptrFromInt(table + off);
        const r_type: u32 = @truncate(r.r_info);
        const r_sym: u32 = @truncate(r.r_info >> 32);
        const slot: *usize = @ptrFromInt(base + r.r_offset);

        switch (classifyReloc(r_type)) {
            .relative => slot.* = base +% @as(usize, @bitCast(@as(isize, @intCast(r.r_addend)))),
            .symbol => {
                if (symtab == 0 or r_sym == 0) continue;
                const sym: *const elf.Elf64_Sym = @ptrFromInt(symtab + @as(usize, r_sym) * syment);
                const addend: usize = @bitCast(@as(isize, @intCast(r.r_addend)));
                // SHN_UNDEF (0): no in-image definition. Ask the resolver to bind
                // it to a host implementation (compiler-rt libcalls emitted by
                // native codegen land here); if it cannot, leave the slot
                // untouched rather than write garbage.
                if (sym.st_shndx == elf.SHN_UNDEF) {
                    if (resolver != null and strtab != 0) {
                        const name_ptr: [*:0]const u8 = @ptrFromInt(strtab + sym.st_name);
                        const name = std.mem.sliceTo(name_ptr, 0);
                        if (resolver.?(name)) |addr| slot.* = addr +% addend;
                    }
                    continue;
                }
                slot.* = base +% sym.st_value +% addend;
            },
            .unsupported => {},
        }
    }
}

fn relativeRelocType() u32 {
    return switch (builtin.cpu.arch) {
        .aarch64 => R_AARCH64_RELATIVE,
        .x86_64 => R_X86_64_RELATIVE,
        else => 0,
    };
}

fn jumpSlotRelocType() u32 {
    return switch (builtin.cpu.arch) {
        .aarch64 => R_AARCH64_JUMP_SLOT,
        .x86_64 => R_X86_64_JUMP_SLOT,
        else => 0,
    };
}

const test_resolved_addr: usize = 0xABCD_0000;

fn testResolver(name: []const u8) ?usize {
    return if (std.mem.eql(u8, name, "__divti3")) test_resolved_addr else null;
}

test "applyDynamicRelocations applies a RELATIVE entry" {
    // The relocation logic is platform-independent; only the relocation-type
    // encoding is arch-specific, so skip arches we don't have a constant for.
    if (comptime relativeRelocType() == 0 or @bitSizeOf(usize) != 64) return error.SkipZigTest;

    // Build a minimal ET_DYN image in a buffer: an ELF header, one PT_DYNAMIC
    // program header, a dynamic vector with a single RELA table, one RELATIVE
    // relocation, and the target word it points at. The image's "vaddr 0" is
    // the buffer start, mirroring how ElfDynLib maps a real object.
    const ehdr_size = @sizeOf(elf.Ehdr);
    const phdr_size = @sizeOf(elf.Phdr);
    const dyn_off = std.mem.alignForward(usize, ehdr_size + phdr_size, 8);
    const dyn_count = 4;
    const rela_off = std.mem.alignForward(usize, dyn_off + dyn_count * @sizeOf(elf.Elf64_Dyn), 8);
    const target_off = std.mem.alignForward(usize, rela_off + @sizeOf(elf.Elf64_Rela), 8);
    const total = target_off + 8;

    const buf = try std.testing.allocator.alignedAlloc(u8, .of(elf.Ehdr), total);
    defer std.testing.allocator.free(buf);
    @memset(buf, 0);

    const base = @intFromPtr(buf.ptr);

    const ehdr: *elf.Ehdr = @ptrCast(buf.ptr);
    ehdr.e_ident[0..4].* = elf.MAGIC.*;
    ehdr.e_type = .DYN;
    ehdr.e_phoff = ehdr_size;
    ehdr.e_phentsize = phdr_size;
    ehdr.e_phnum = 1;

    const phdr: *elf.Phdr = @ptrCast(@alignCast(buf.ptr + ehdr_size));
    phdr.p_type = elf.PT_DYNAMIC;
    phdr.p_vaddr = dyn_off;
    phdr.p_offset = dyn_off;
    phdr.p_filesz = dyn_count * @sizeOf(elf.Elf64_Dyn);
    phdr.p_memsz = phdr.p_filesz;

    const dynv: [*]elf.Elf64_Dyn = @ptrCast(@alignCast(buf.ptr + dyn_off));
    dynv[0] = .{ .d_tag = elf.DT_RELA, .d_val = rela_off };
    dynv[1] = .{ .d_tag = elf.DT_RELASZ, .d_val = @sizeOf(elf.Elf64_Rela) };
    dynv[2] = .{ .d_tag = elf.DT_RELAENT, .d_val = @sizeOf(elf.Elf64_Rela) };
    dynv[3] = .{ .d_tag = elf.DT_NULL, .d_val = 0 };

    const addend: i64 = 0x1234;
    const rela: *elf.Elf64_Rela = @ptrCast(@alignCast(buf.ptr + rela_off));
    rela.r_offset = target_off;
    rela.r_info = relativeRelocType(); // symbol index 0, RELATIVE type
    rela.r_addend = addend;

    const target: *usize = @ptrCast(@alignCast(buf.ptr + target_off));
    target.* = 0;

    applyDynamicRelocations(base, null);

    try std.testing.expectEqual(base +% @as(usize, @intCast(addend)), target.*);
}

test "applyDynamicRelocations binds an undefined symbol through the resolver" {
    if (comptime jumpSlotRelocType() == 0 or @bitSizeOf(usize) != 64) return error.SkipZigTest;

    // Like the RELATIVE test above, but with a JUMP_SLOT entry against an
    // undefined symbol named "__divti3", plus the DT_SYMTAB/DT_STRTAB the
    // resolver path needs to read that name. The resolver binds the slot to a
    // sentinel address, mirroring how the eval loader binds compiler-rt libcalls.
    const ehdr_size = @sizeOf(elf.Ehdr);
    const phdr_size = @sizeOf(elf.Phdr);
    const dyn_off = std.mem.alignForward(usize, ehdr_size + phdr_size, 8);
    const dyn_count = 7;
    const rela_off = std.mem.alignForward(usize, dyn_off + dyn_count * @sizeOf(elf.Elf64_Dyn), 8);
    const sym_off = std.mem.alignForward(usize, rela_off + @sizeOf(elf.Elf64_Rela), 8);
    const str_off = std.mem.alignForward(usize, sym_off + 2 * @sizeOf(elf.Elf64_Sym), 8);
    const name_bytes = "\x00__divti3\x00"; // index 0 is the empty string
    const target_off = std.mem.alignForward(usize, str_off + name_bytes.len, 8);
    const total = target_off + 8;

    const buf = try std.testing.allocator.alignedAlloc(u8, .of(elf.Ehdr), total);
    defer std.testing.allocator.free(buf);
    @memset(buf, 0);

    const base = @intFromPtr(buf.ptr);

    const ehdr: *elf.Ehdr = @ptrCast(buf.ptr);
    ehdr.e_ident[0..4].* = elf.MAGIC.*;
    ehdr.e_type = .DYN;
    ehdr.e_phoff = ehdr_size;
    ehdr.e_phentsize = phdr_size;
    ehdr.e_phnum = 1;

    const phdr: *elf.Phdr = @ptrCast(@alignCast(buf.ptr + ehdr_size));
    phdr.p_type = elf.PT_DYNAMIC;
    phdr.p_vaddr = dyn_off;
    phdr.p_offset = dyn_off;
    phdr.p_filesz = dyn_count * @sizeOf(elf.Elf64_Dyn);
    phdr.p_memsz = phdr.p_filesz;

    const dynv: [*]elf.Elf64_Dyn = @ptrCast(@alignCast(buf.ptr + dyn_off));
    dynv[0] = .{ .d_tag = elf.DT_JMPREL, .d_val = rela_off };
    dynv[1] = .{ .d_tag = elf.DT_PLTRELSZ, .d_val = @sizeOf(elf.Elf64_Rela) };
    dynv[2] = .{ .d_tag = elf.DT_RELAENT, .d_val = @sizeOf(elf.Elf64_Rela) };
    dynv[3] = .{ .d_tag = elf.DT_SYMTAB, .d_val = sym_off };
    dynv[4] = .{ .d_tag = elf.DT_SYMENT, .d_val = @sizeOf(elf.Elf64_Sym) };
    dynv[5] = .{ .d_tag = elf.DT_STRTAB, .d_val = str_off };
    dynv[6] = .{ .d_tag = elf.DT_NULL, .d_val = 0 };

    @memcpy(buf[str_off..][0..name_bytes.len], name_bytes);

    // symtab[1] is an undefined reference to "__divti3" (st_name = 1).
    const symtab: [*]elf.Elf64_Sym = @ptrCast(@alignCast(buf.ptr + sym_off));
    symtab[1].st_name = 1;
    symtab[1].st_shndx = elf.SHN_UNDEF;

    const rela: *elf.Elf64_Rela = @ptrCast(@alignCast(buf.ptr + rela_off));
    rela.r_offset = target_off;
    rela.r_info = (@as(u64, 1) << 32) | jumpSlotRelocType(); // symbol index 1
    rela.r_addend = 0;

    const target: *usize = @ptrCast(@alignCast(buf.ptr + target_off));
    target.* = 0;

    applyDynamicRelocations(base, &testResolver);

    try std.testing.expectEqual(test_resolved_addr, target.*);
}
