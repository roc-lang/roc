//! A minimal, correct ELF `dlopen` for loading in-process eval objects in a
//! static, no-libc roc binary (where `std.DynLib` falls back to `ElfDynLib`).
//!
//! Adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!
//!
//! Zig 0.16's `ElfDynLib` is not a complete loader for our use:
//!
//!  1. It mishandles writable segments. For a writable `PT_LOAD` it does
//!     `@memcpy(sect_mem[0..p_filesz], file_bytes[0..p_filesz])` — copying from
//!     **file offset 0** (the ELF header) to the segment's page-aligned start,
//!     ignoring both `p_offset` (the real source) and the page-alignment gap
//!     (the real destination, `base + p_vaddr`). So `.data`/`.got`/`.dynamic`
//!     for that segment are left **zeroed**.
//!  2. It applies **no dynamic relocations** at all.
//!
//!  Together those are exactly why `roc test --opt=speed` crashed on
//!  arm64-linux: the generated code reaches an exported global through a GOT
//!  slot that needs an `R_AARCH64_RELATIVE`; the slot lived in a writable
//!  segment that was zeroed and never relocated, so the store went to address 0.
//!
//! This vendored loader maps every segment from the correct file bytes and
//! applies the object's dynamic relocations, so the loaded image is correct
//! regardless of what the linker emitted — rather than relying on codegen never
//! producing a relocation (which protected visibility narrows but cannot
//! guarantee). It is used only on the static/no-libc linux configuration; every
//! other configuration keeps using `std.DynLib`, whose `DlDynLib` defers to the
//! OS dynamic loader.
//!
//! (Upstream fix for the `ElfDynLib` writable-segment bug is worth reporting;
//! until a Zig release carries it, this keeps the eval path correct.)

const std = @import("std");
const builtin = @import("builtin");
const elf = std.elf;
const posix = std.posix;
const mem = std.mem;
const self_relocate = @import("base").elf_self_relocate;

/// True when `std.DynLib` resolves to the relocation-free, incomplete
/// `ElfDynLib` (a static, no-libc — i.e. static-musl — linux binary) and we
/// should use this vendored loader instead.
pub const active = self_relocate.loader_skips_relocations;

const SHT_DYNSYM: u32 = 11;

pub const ElfDynLib = struct {
    memory: []align(std.heap.page_size_min) u8,
    strtab: [*:0]const u8,
    symtab: [*]const elf.Elf64_Sym,
    sym_count: usize,

    /// Trusts the file. A malicious file can execute arbitrary code.
    ///
    /// `resolver` binds undefined symbols (compiler-rt libcalls that native
    /// codegen emits but the loaded image does not define) to host
    /// implementations; pass null to leave them unresolved.
    pub const OpenError = std.Io.File.OpenError || std.Io.File.StatError || posix.MMapError || error{
        NotElfFile,
        NotDynamicLibrary,
        MissingDynamicLinkingInformation,
        ElfStringSectionNotFound,
        ElfSymSectionNotFound,
    };

    pub fn open(path: [:0]const u8, resolver: ?self_relocate.UndefinedSymbolResolver) OpenError!ElfDynLib {
        const io = std.Options.debug_io;
        const file = try std.Io.Dir.cwd().openFile(io, path, .{});
        defer file.close(io);

        const stat = try file.stat(io);
        const size = std.math.cast(usize, stat.size) orelse return error.NotElfFile;
        const page_size = std.heap.pageSize();

        // Map the file read-only to read its headers and source bytes.
        const file_bytes = try posix.mmap(
            null,
            mem.alignForward(usize, size, page_size),
            .{ .READ = true },
            .{ .TYPE = .PRIVATE },
            file.handle,
            0,
        );
        defer posix.munmap(file_bytes);

        const eh: *elf.Ehdr = @ptrCast(file_bytes.ptr);
        if (!mem.eql(u8, eh.e_ident[0..4], elf.MAGIC)) return error.NotElfFile;
        if (eh.e_type != .DYN) return error.NotDynamicLibrary;
        const file_addr = @intFromPtr(file_bytes.ptr);

        // Pass 1: find the dynamic vector and the total virtual size.
        var maybe_dynv: ?[*]usize = null;
        var virt_addr_end: usize = 0;
        {
            var i: usize = 0;
            var ph_addr: usize = file_addr + eh.e_phoff;
            while (i < eh.e_phnum) : ({
                i += 1;
                ph_addr += eh.e_phentsize;
            }) {
                const ph: *elf.Phdr = @ptrFromInt(ph_addr);
                switch (ph.p_type) {
                    elf.PT_LOAD => virt_addr_end = @max(virt_addr_end, ph.p_vaddr + ph.p_memsz),
                    elf.PT_DYNAMIC => maybe_dynv = @ptrFromInt(file_addr + ph.p_offset),
                    else => {},
                }
            }
        }
        const dynv = maybe_dynv orelse return error.MissingDynamicLinkingInformation;

        // Reserve the whole range with no access so the FIXED maps below land
        // at a contiguous base.
        const loaded = try posix.mmap(
            null,
            virt_addr_end,
            .{},
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        errdefer posix.munmap(loaded);
        const base = @intFromPtr(loaded.ptr);

        // Pass 2: map each PT_LOAD from the correct file bytes.
        {
            var i: usize = 0;
            var ph_addr: usize = file_addr + eh.e_phoff;
            while (i < eh.e_phnum) : ({
                i += 1;
                ph_addr += eh.e_phentsize;
            }) {
                const ph: *elf.Phdr = @ptrFromInt(ph_addr);
                if (ph.p_type != elf.PT_LOAD) continue;

                const seg_addr = base + ph.p_vaddr;
                const aligned_addr = seg_addr & ~(@as(usize, page_size) - 1);
                const extra_bytes = seg_addr - aligned_addr;
                const extended_memsz = mem.alignForward(usize, ph.p_memsz + extra_bytes, page_size);
                const ptr: [*]align(std.heap.page_size_min) u8 = @ptrFromInt(aligned_addr);
                const prot = elfToProt(ph.p_flags);

                if ((ph.p_flags & elf.PF_W) == 0) {
                    // Read-only: map straight from the file.
                    _ = try posix.mmap(
                        ptr,
                        extended_memsz,
                        prot,
                        .{ .TYPE = .PRIVATE, .FIXED = true },
                        file.handle,
                        ph.p_offset - extra_bytes,
                    );
                } else {
                    // Writable: anonymous map, then copy the segment's real file
                    // bytes to its real destination (`base + p_vaddr`). Bytes in
                    // [p_filesz, p_memsz) stay zero (.bss). This is the part
                    // std's ElfDynLib gets wrong.
                    const seg_mem = try posix.mmap(
                        ptr,
                        extended_memsz,
                        prot,
                        .{ .TYPE = .PRIVATE, .FIXED = true, .ANONYMOUS = true },
                        -1,
                        0,
                    );
                    const dst = seg_mem[extra_bytes..][0..ph.p_filesz];
                    const src = file_bytes[ph.p_offset..][0..ph.p_filesz];
                    @memcpy(dst, src);
                }
            }
        }

        // Resolve the dynamic string/symbol tables (vaddrs biased by base).
        var strtab: ?[*:0]const u8 = null;
        var symtab: ?[*]const elf.Elf64_Sym = null;
        {
            var i: usize = 0;
            while (dynv[i] != 0) : (i += 2) {
                const p = base + dynv[i + 1];
                switch (dynv[i]) {
                    elf.DT_STRTAB => strtab = @ptrFromInt(p),
                    elf.DT_SYMTAB => symtab = @ptrFromInt(p),
                    else => {},
                }
            }
        }

        // Count dynamic symbols from the section headers (read from the file);
        // a linear scan over them is all `lookup` needs, avoiding the hash
        // table machinery for these small, short-lived eval objects.
        var sym_count: usize = 0;
        if (eh.e_shoff != 0) {
            var i: usize = 0;
            var sh_addr: usize = file_addr + eh.e_shoff;
            while (i < eh.e_shnum) : ({
                i += 1;
                sh_addr += eh.e_shentsize;
            }) {
                const sh: *elf.Elf64_Shdr = @ptrFromInt(sh_addr);
                if (sh.sh_type == SHT_DYNSYM and sh.sh_entsize != 0) {
                    sym_count = sh.sh_size / sh.sh_entsize;
                    break;
                }
            }
        }

        // Apply the object's dynamic relocations now that every segment holds
        // its correct bytes.
        self_relocate.applyDynamicRelocations(base, resolver);

        return .{
            .memory = loaded,
            .strtab = strtab orelse return error.ElfStringSectionNotFound,
            .symtab = symtab orelse return error.ElfSymSectionNotFound,
            .sym_count = sym_count,
        };
    }

    pub fn close(self: *ElfDynLib) void {
        posix.munmap(self.memory);
        self.* = undefined;
    }

    pub fn lookup(self: *const ElfDynLib, comptime T: type, name: [:0]const u8) ?T {
        const base = @intFromPtr(self.memory.ptr);
        var i: usize = 0;
        while (i < self.sym_count) : (i += 1) {
            const sym = self.symtab[i];
            if (sym.st_shndx == elf.SHN_UNDEF) continue;
            const sym_name = mem.sliceTo(self.strtab + sym.st_name, 0);
            if (mem.eql(u8, sym_name, name)) {
                return @ptrFromInt(base + sym.st_value);
            }
        }
        return null;
    }
};

fn elfToProt(elf_prot: u64) posix.PROT {
    return .{
        .READ = (elf_prot & elf.PF_R) != 0,
        .WRITE = (elf_prot & elf.PF_W) != 0,
        .EXEC = (elf_prot & elf.PF_X) != 0,
    };
}
