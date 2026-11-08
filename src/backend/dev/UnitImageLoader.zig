//! Loads a unit image (`UnitImage`) into the compiler's own process, where
//! compile-time evaluation calls into it (design.md "Fast Dev Link",
//! "Compile-Time Constants").
//!
//! The image's block (text, then read-only data) is copied into memory that
//! becomes read-only and executable once patched, its writable data into
//! writable memory after it, and its zero-fill is reserved there too. Each
//! import is bound once through the caller's resolver, then every patch
//! site is filled with `UnitImage.applySite`, exactly as a dev executable's
//! link fills it, against the addresses the regions were loaded at. A
//! reference to a function the image cannot reach directly goes through a
//! stub after the block, as a static link's PLT entry would: on x86_64 a
//! 32-bit displacement, on arm64 a branch or a page-relative address
//! computation. The whole block is loaded: a module's compile-time program
//! may call any of its exports.
//!
//! Unit images hold the host's own code, x86_64 for a Linux compiler, arm64
//! for a macOS one, either for a Windows one, so this loads them only on
//! those hosts (`backend.dev.UnitImageLoader` is `void` elsewhere). A
//! Windows image's unwind tables are loaded after the stubs and registered
//! with the process, so the unwinder knows the loaded functions. With the
//! developer diagnostic `ROC_PERF_MAP` set, each loaded image's functions
//! are appended to the process's perf map (`PerfMap`).

const std = @import("std");
const builtin = @import("builtin");
const dev_diagnostics = @import("base").dev_diagnostics;
const UnitImage = @import("UnitImage.zig");
const PerfMap = @import("PerfMap.zig");

const page_size = std.heap.page_size_min;
const stub_size: usize = 16;
const arm64 = builtin.cpu.arch == .aarch64;
const windows = builtin.os.tag == .windows;

const win32 = struct {
    const DWORD = u32;
    const BOOL = c_int;
    const MEM_COMMIT: DWORD = 0x1000;
    const MEM_RESERVE: DWORD = 0x2000;
    const MEM_RELEASE: DWORD = 0x8000;
    const PAGE_READWRITE: DWORD = 0x04;
    const PAGE_EXECUTE_READ: DWORD = 0x20;

    extern "kernel32" fn VirtualAlloc(address: ?*anyopaque, size: usize, allocation_type: DWORD, protect: DWORD) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn VirtualFree(address: *anyopaque, size: usize, free_type: DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn VirtualProtect(address: *anyopaque, size: usize, new_protect: DWORD, old_protect: *DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn GetCurrentProcess() callconv(.winapi) *anyopaque;
    extern "kernel32" fn FlushInstructionCache(process: *anyopaque, address: ?*const anyopaque, size: usize) callconv(.winapi) BOOL;
    extern "ntdll" fn RtlAddFunctionTable(table: *anyopaque, count: u32, base_address: u64) callconv(.winapi) u8;
    extern "ntdll" fn RtlDeleteFunctionTable(table: *anyopaque) callconv(.winapi) u8;
};

/// What an import binds to.
pub const Binding = struct {
    address: usize,
    /// Whether `address` is a function's entry: a call or a function's
    /// address may then go through a stub that jumps to it.
    function: bool,
};

/// Binds the image's imports by name; null for a name it does not define.
pub const Resolver = struct {
    context: *anyopaque,
    resolve: *const fn (context: *anyopaque, name: []const u8) ?Binding,
};

/// Why an image could not be loaded.
pub const LoadError = error{
    /// The process could not allocate, map or protect memory for the
    /// image.
    OutOfMemory,
    /// A strong import the resolver does not define; `Diagnostic.name`
    /// names it.
    UndefinedImport,
    /// A site's value does not fit its field even through a stub;
    /// `Diagnostic.name` names its import, or is empty for a local site.
    SiteOutOfRange,
};

/// What a failed load was about.
pub const Diagnostic = struct {
    name: []const u8 = "",
};

/// A loaded image.
pub const Loaded = struct {
    view: UnitImage.View,
    /// The block, its stubs and any unwind tables, then the writable data
    /// and the zero-fill.
    memory: []align(page_size) u8,
    data_offset: usize,
    bss_offset: usize,
    unwind_offset: usize,
    xdata_offset: usize,
    /// The function table registered with a Windows process, if any.
    function_table: ?*anyopaque = null,

    /// The address `region` was loaded at.
    pub fn regionAddress(self: *const Loaded, region: UnitImage.Region) usize {
        const base = @intFromPtr(self.memory.ptr);
        return switch (region) {
            .block => base,
            .data => base + self.data_offset,
            .bss => base + self.bss_offset,
            .pdata => base + self.unwind_offset,
            .xdata => base + self.xdata_offset,
            .debug_line, .debug_info, .debug_abbrev => unreachable, // debug regions are never loaded, and no allocated site or symbol lives in one
        };
    }

    /// The loaded text, where the unit's code locations count offsets from.
    pub fn text(self: *const Loaded) []const u8 {
        return self.memory[0..self.view.textSize()];
    }

    /// The address of `symbol`, one the image defines.
    pub fn symbolAddress(self: *const Loaded, symbol: UnitImage.Symbol) usize {
        return self.regionAddress(@fromBackingInt(symbol.region)) + symbol.value;
    }

    /// Release the image's memory.
    pub fn unload(self: *Loaded) void {
        if (windows) {
            if (self.function_table) |table| _ = win32.RtlDeleteFunctionTable(table);
            _ = win32.VirtualFree(self.memory.ptr, 0, win32.MEM_RELEASE);
        } else {
            std.posix.munmap(self.memory);
        }
        self.* = undefined;
    }
};

/// Load `view`, binding its imports through `resolver`. The image's bytes
/// are only read during the call.
pub fn load(gpa: std.mem.Allocator, view: UnitImage.View, resolver: Resolver, diagnostic: *Diagnostic) LoadError!Loaded {
    const imports = view.imports();
    const block_len: usize = view.block().len;
    const stubs_offset = std.mem.alignForward(usize, block_len, stub_size);
    const unwind_offset = std.mem.alignForward(usize, stubs_offset + imports.len * stub_size, 16);
    const xdata_offset = std.mem.alignForward(usize, unwind_offset + view.pdata().len, 16);
    const exec_len = std.mem.alignForward(usize, xdata_offset + view.xdata().len, page_size);
    const data_offset = exec_len;
    const bss_offset = std.mem.alignForward(usize, data_offset + view.data().len, @max(view.bssAlign(), 1));
    const total = @max(std.mem.alignForward(usize, bss_offset + view.bssSize(), page_size), page_size);

    const memory = try mapMemory(total);
    var loaded: Loaded = .{ .view = view, .memory = memory, .data_offset = data_offset, .bss_offset = bss_offset, .unwind_offset = unwind_offset, .xdata_offset = xdata_offset };
    errdefer loaded.unload();
    @memcpy(memory[0..block_len], view.block());
    @memcpy(memory[unwind_offset..][0..view.pdata().len], view.pdata());
    @memcpy(memory[xdata_offset..][0..view.xdata().len], view.xdata());
    @memcpy(memory[data_offset..][0..view.data().len], view.data());

    // Each import's binding, and its stub once a site needs one.
    const bindings = try gpa.alloc(Binding, imports.len);
    defer gpa.free(bindings);
    const stubs = try gpa.alloc(?usize, imports.len);
    defer gpa.free(stubs);
    @memset(stubs, null);
    for (imports, bindings) |import, *binding| {
        const name = view.name(import.name);
        binding.* = resolver.resolve(resolver.context, name) orelse binding: {
            if (import.flags & UnitImage.symbol_weak == 0) {
                diagnostic.name = name;
                return error.UndefinedImport;
            }
            break :binding .{ .address = 0, .function = false };
        };
    }

    const stubs_address = @intFromPtr(memory.ptr) + stubs_offset;
    const Stubs = struct {
        memory: []align(page_size) u8,
        stubs_offset: usize,
        stubs_address: usize,
        stubs: []?usize,

        fn of(self: @This(), import: u32, binding: Binding) usize {
            return self.stubs[import] orelse stub: {
                const address = self.stubs_address + import * stub_size;
                writeStub(self.memory[self.stubs_offset + import * stub_size ..][0..stub_size], binding.address);
                self.stubs[import] = address;
                break :stub address;
            };
        }
    };
    const stub_table: Stubs = .{ .memory = memory, .stubs_offset = stubs_offset, .stubs_address = stubs_address, .stubs = stubs };
    // On arm64 a page-relative pair names one address, so an import's
    // page sites all take the binding or all take its stub: the stub when
    // the binding is beyond their reach from anywhere in the mapping.
    for (view.sites()) |site| {
        const region: UnitImage.Region = @fromBackingInt(site.region);
        if (!region.isAllocated()) continue;
        const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
        const region_offset: usize = switch (region) {
            .block => 0,
            .data => data_offset,
            .pdata => unwind_offset,
            .xdata => xdata_offset,
            .bss, .debug_line, .debug_info, .debug_abbrev => unreachable, // a site is a field with bytes in an allocated region: the block, the data or the unwind tables
        };
        const field = memory[region_offset + site.offset ..][0..@intCast(UnitImage.kindWidth(kind))];
        const site_address = @intFromPtr(memory.ptr) + region_offset + site.offset;
        if (kind == .rva32) {
            // An image-relative field counts from the mapping's start, the
            // base the function table is registered with.
            const target = if (site.target_region != UnitImage.import_target) loaded.regionAddress(@fromBackingInt(site.target_region)) else bindings[site.target].address;
            const relative = @as(i64, @intCast(target)) - @as(i64, @intCast(@intFromPtr(memory.ptr))) + site.addend;
            if (!UnitImage.applySite(field, kind, relative, site_address)) {
                diagnostic.name = if (site.target_region != UnitImage.import_target) "" else view.name(imports[site.target].name);
                return error.SiteOutOfRange;
            }
            continue;
        }
        if (site.target_region != UnitImage.import_target) {
            const target = loaded.regionAddress(@fromBackingInt(site.target_region));
            if (!UnitImage.applySite(field, kind, @as(i64, @intCast(target)) + site.addend, site_address)) {
                diagnostic.name = "";
                return error.SiteOutOfRange;
            }
            continue;
        }
        const binding = bindings[site.target];
        const through_stub = switch (kind) {
            .abs64, .abs32, .abs32s => false,
            .pc32, .branch26 => !UnitImage.applySite(field, kind, @as(i64, @intCast(binding.address)) + site.addend, site_address),
            .page21, .pageoff12 => !withinPageReach(binding.address, @intFromPtr(memory.ptr), total),
            .rva32 => unreachable, // applied above
        };
        if (!through_stub) {
            if (UnitImage.applySite(field, kind, @as(i64, @intCast(binding.address)) + site.addend, site_address)) continue;
            diagnostic.name = view.name(imports[site.target].name);
            return error.SiteOutOfRange;
        }
        if (!binding.function) {
            diagnostic.name = view.name(imports[site.target].name);
            return error.SiteOutOfRange;
        }
        const stub = stub_table.of(site.target, binding);
        if (!UnitImage.applySite(field, kind, @as(i64, @intCast(stub)) + site.addend, site_address)) {
            diagnostic.name = view.name(imports[site.target].name);
            return error.SiteOutOfRange;
        }
    }

    if (windows) {
        var old: win32.DWORD = undefined;
        if (win32.VirtualProtect(memory.ptr, exec_len, win32.PAGE_EXECUTE_READ, &old) == 0) return error.OutOfMemory;
        _ = win32.FlushInstructionCache(win32.GetCurrentProcess(), memory.ptr, exec_len);
        if (view.pdata().len != 0) {
            const entry_size: usize = if (arm64) 8 else 12;
            const table: *anyopaque = @ptrCast(memory.ptr + unwind_offset);
            if (win32.RtlAddFunctionTable(table, @intCast(view.pdata().len / entry_size), @intFromPtr(memory.ptr)) == 0) return error.OutOfMemory;
            loaded.function_table = table;
        }
    } else {
        if (arm64) flushInstructionCache(memory[0..exec_len]);
        const executable: std.posix.PROT = .{ .READ = true, .EXEC = true };
        if (std.c.mprotect(@ptrCast(memory.ptr), exec_len, executable) != 0) return error.OutOfMemory;
    }
    if (dev_diagnostics.isSet(.perf_map)) PerfMap.appendImage(gpa, view, @as(*const Loaded, &loaded), Loaded.symbolAddress);
    return loaded;
}

/// Map `len` bytes of zeroed, writable memory, preferably below this
/// binary's code (`mappingHint`).
fn mapMemory(len: usize) error{OutOfMemory}![]align(page_size) u8 {
    if (windows) {
        const preferred: ?*anyopaque = if (mappingHint()) |hint| @ptrCast(hint) else null;
        const flags = win32.MEM_COMMIT | win32.MEM_RESERVE;
        const address = win32.VirtualAlloc(preferred, len, flags, win32.PAGE_READWRITE) orelse
            win32.VirtualAlloc(null, len, flags, win32.PAGE_READWRITE) orelse return error.OutOfMemory;
        const start: [*]align(page_size) u8 = @ptrCast(@alignCast(address));
        return start[0..len];
    }
    return std.posix.mmap(mappingHint(), len, .{ .READ = true, .WRITE = true }, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0) catch error.OutOfMemory;
}

/// Whether an arm64 page-relative pair anywhere in a mapping of `len`
/// bytes at `base` reaches `address`: 4 GiB either way.
fn withinPageReach(address: usize, base: usize, len: usize) bool {
    const reach: usize = 1 << 32;
    const low = base -| reach;
    const high = base + len + reach;
    return address >= low and address < high;
}

/// A stub jumping to `target`: on x86_64 `jmp qword ptr [rip + 0]`
/// followed by the address, on arm64 `ldr x16, #8; br x16` followed by it.
fn writeStub(bytes: *[stub_size]u8, target: usize) void {
    if (arm64) {
        std.mem.writeInt(u32, bytes[0..4], 0x5800_0050, .little);
        std.mem.writeInt(u32, bytes[4..8], 0xd61f_0200, .little);
        std.mem.writeInt(u64, bytes[8..16], target, .little);
        return;
    }
    bytes[0..6].* = .{ 0xff, 0x25, 0x00, 0x00, 0x00, 0x00 };
    std.mem.writeInt(u64, bytes[6..14], target, .little);
    bytes[14..16].* = .{ 0xcc, 0xcc };
}

/// Publish newly written instructions before they run, through compiler-rt's
/// cache maintenance for the host.
fn flushInstructionCache(memory: []const u8) void {
    __clear_cache(memory.ptr, memory.ptr + memory.len);
}

extern fn __clear_cache(start: [*]const u8, end: [*]const u8) callconv(.c) void;

/// An address below this binary's code, a preference for where the image
/// lands so its calls into the compiler reach it directly. The kernel may
/// place the image elsewhere; out-of-reach calls then use stubs.
fn mappingHint() ?[*]align(page_size) u8 {
    const anchor = @intFromPtr(&mappingHint);
    if (anchor <= (1 << 30)) return null;
    // Windows allocates at a 64 KiB granularity.
    return @ptrFromInt(std.mem.alignBackward(usize, anchor - (1 << 28), if (windows) 1 << 16 else page_size));
}

const TestObject = UnitImage.TestObject;
const TestMachObject = UnitImage.TestMachObject;
const TestCoffObject = UnitImage.TestCoffObject;
const SHF_WRITE = 0x1;
const SHF_ALLOC = 0x2;
const SHF_EXECINSTR = 0x4;
const SHT_NOBITS = 8;
const R_X86_64_64 = 1;
const R_X86_64_PC32 = 2;
const R_X86_64_PLT32 = 4;
const ARM64_RELOC_UNSIGNED = 0;
const ARM64_RELOC_BRANCH26 = 2;
const ARM64_RELOC_PAGE21 = 3;
const ARM64_RELOC_PAGEOFF12 = 4;
const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
const S_ZEROFILL = 1;

/// An object with a function returning 42, one calling the import `ext`,
/// one counting in its zero-fill, and a pointer to the first in its data,
/// in the host's own instruction set and object format.
fn testImage(allocator: std.mem.Allocator) UnitImage.BuildError![]align(UnitImage.alignment.toByteUnits()) u8 {
    if (windows) return testCoffImage(allocator);
    if (arm64) {
        // answer: mov w0, #42; ret
        // call_ext: stp x29, x30, [sp, #-16]!; bl ext; ldp x29, x30, [sp], #16; ret
        // bump: adrp x8, counter; ldr x9, [x8, counter]; add x9, x9, #1; str x9, [x8, counter]; mov x0, x9; ret
        const text = [_]u32{
            0x5280_0540, 0xd65f_03c0, 0,           0,
            0xa9bf_7bfd, 0x9400_0000, 0xa8c1_7bfd, 0xd65f_03c0,
            0x9000_0008, 0xf940_0109, 0x9100_0529, 0xf900_0109,
            0xaa09_03e0, 0xd65f_03c0, 0,           0,
        };
        var bytes: [text.len * 4]u8 = undefined;
        for (text, 0..) |word, index| std.mem.writeInt(u32, bytes[index * 4 ..][0..4], word, .little);
        const object = try TestMachObject.encode(allocator, &.{
            .{ .segname = "__TEXT", .sectname = "__text", .bytes = &bytes, .flags = S_ATTR_PURE_INSTRUCTIONS },
            .{ .segname = "__DATA", .sectname = "__data", .bytes = &@as([8]u8, @splat(0)), .@"align" = 3 },
            .{ .segname = "__DATA", .sectname = "__bss", .size = 8, .@"align" = 3, .flags = S_ZEROFILL },
        }, &.{
            .{ .name = "_answer", .section = 1, .value = 0, .external = true },
            .{ .name = "_call_ext", .section = 1, .value = 16, .external = true },
            .{ .name = "_bump", .section = 1, .value = 32, .external = true },
            .{ .name = "_counter", .section = 3, .value = 0 },
            .{ .name = "_ext", .section = 0, .external = true },
            .{ .name = "_answer_pointer", .section = 2, .value = 0, .external = true },
        }, &.{
            .{ .section = 1, .offset = 20, .symbol = 5, .type = ARM64_RELOC_BRANCH26, .pcrel = true },
            .{ .section = 1, .offset = 32, .symbol = 4, .type = ARM64_RELOC_PAGE21, .pcrel = true },
            .{ .section = 1, .offset = 36, .symbol = 4, .type = ARM64_RELOC_PAGEOFF12, .pcrel = false },
            .{ .section = 1, .offset = 44, .symbol = 4, .type = ARM64_RELOC_PAGEOFF12, .pcrel = false },
            .{ .section = 2, .offset = 0, .symbol = 1, .type = ARM64_RELOC_UNSIGNED, .pcrel = false, .length = 3 },
        });
        defer allocator.free(object);
        return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    }
    const text = [_]u8{
        0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3, 0x90, 0x90, // answer: mov eax, 42; ret
        0x48, 0x83, 0xec, 0x08, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x48, 0x83, 0xc4, 0x08, 0xc3, 0x90, 0x90, // call_ext: call ext
        0x8b, 0x05, 0x00, 0x00, 0x00, 0x00, 0xff, 0xc0, 0x89, 0x05, 0x00, 0x00, 0x00, 0x00, 0xc3, 0x90, // bump: ++counter
    };
    const object = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
        .{ .name = ".data", .flags = SHF_ALLOC | SHF_WRITE, .bytes = &@as([8]u8, @splat(0)), .addralign = 8 },
        .{ .name = ".bss", .type = SHT_NOBITS, .flags = SHF_ALLOC | SHF_WRITE, .size = 8, .addralign = 8 },
    }, &.{
        .{ .name = "answer", .section = 1, .value = 0, .size = 8, .global = true, .func = true },
        .{ .name = "call_ext", .section = 1, .value = 8, .size = 16, .global = true, .func = true },
        .{ .name = "bump", .section = 1, .value = 24, .size = 16, .global = true, .func = true },
        .{ .name = "counter", .section = 3, .value = 0, .size = 8 },
        .{ .name = "ext", .section = 0, .global = true },
        .{ .name = "answer_pointer", .section = 2, .value = 0, .size = 8, .global = true },
    }, &.{
        .{ .section = 1, .offset = 13, .symbol = 5, .type = R_X86_64_PLT32, .addend = -4 },
        .{ .section = 1, .offset = 26, .symbol = 4, .type = R_X86_64_PC32, .addend = -4 },
        .{ .section = 1, .offset = 34, .symbol = 4, .type = R_X86_64_PC32, .addend = -4 },
        .{ .section = 2, .offset = 0, .symbol = 1, .type = R_X86_64_64, .addend = 0 },
    });
    defer allocator.free(object);
    return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
}

/// The test image as a COFF object for a Windows host, with x86_64 unwind
/// information for the calling function.
fn testCoffImage(allocator: std.mem.Allocator) UnitImage.BuildError![]align(UnitImage.alignment.toByteUnits()) u8 {
    const text_flags: u32 = 0x6050_0020;
    const data_flags: u32 = 0xc040_0040;
    const bss_flags: u32 = 0xc050_0080;
    const unwind_flags: u32 = 0x4030_0040;
    if (arm64) {
        const text = [_]u32{
            0x5280_0540, 0xd65f_03c0, 0,           0,
            0xa9bf_7bfd, 0x9400_0000, 0xa8c1_7bfd, 0xd65f_03c0,
            0x9000_0008, 0xf940_0109, 0x9100_0529, 0xf900_0109,
            0xaa09_03e0, 0xd65f_03c0, 0,           0,
        };
        var bytes: [text.len * 4]u8 = undefined;
        for (text, 0..) |word, index| std.mem.writeInt(u32, bytes[index * 4 ..][0..4], word, .little);
        const object = try TestCoffObject.encode(allocator, 0xaa64, &.{
            .{ .name = ".text", .bytes = &bytes, .characteristics = text_flags },
            .{ .name = ".data", .bytes = &@as([8]u8, @splat(0)), .characteristics = data_flags },
            .{ .name = ".bss", .size = 8, .characteristics = bss_flags },
        }, &.{
            .{ .name = "answer", .section = 1, .value = 0, .function = true },
            .{ .name = "call_ext", .section = 1, .value = 16, .function = true },
            .{ .name = "bump", .section = 1, .value = 32, .function = true },
            .{ .name = "counter", .section = 3, .value = 0, .class = .static },
            .{ .name = "ext", .section = 0 },
            .{ .name = "answer_pointer", .section = 2, .value = 0 },
        }, &.{
            .{ .section = 1, .offset = 20, .symbol = 4, .type = 0x0003 },
            .{ .section = 1, .offset = 32, .symbol = 3, .type = 0x0004 },
            .{ .section = 1, .offset = 36, .symbol = 3, .type = 0x0007 },
            .{ .section = 1, .offset = 44, .symbol = 3, .type = 0x0007 },
            .{ .section = 2, .offset = 0, .symbol = 0, .type = 0x000e },
        });
        defer allocator.free(object);
        return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    }
    const text = [_]u8{
        0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3, 0x90, 0x90, // answer: mov eax, 42; ret
        0x48, 0x83, 0xec, 0x28, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x48, 0x83, 0xc4, 0x28, 0xc3, 0x90, 0x90, // call_ext: sub rsp, 40; call ext; add rsp, 40
        0x8b, 0x05, 0x00, 0x00, 0x00, 0x00, 0xff, 0xc0, 0x89, 0x05, 0x00, 0x00, 0x00, 0x00, 0xc3, 0x90, // bump: ++counter
    };
    // call_ext's unwind information: one small allocation of 40 bytes.
    const xdata = [_]u8{ 0x01, 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00 };
    var pdata: [12]u8 = @splat(0);
    std.mem.writeInt(u32, pdata[4..8], 14, .little);
    const object = try TestCoffObject.encode(allocator, 0x8664, &.{
        .{ .name = ".text", .bytes = &text, .characteristics = text_flags },
        .{ .name = ".data", .bytes = &@as([8]u8, @splat(0)), .characteristics = data_flags },
        .{ .name = ".bss", .size = 8, .characteristics = bss_flags },
        .{ .name = ".pdata", .bytes = &pdata, .characteristics = unwind_flags },
        .{ .name = ".xdata", .bytes = &xdata, .characteristics = unwind_flags },
    }, &.{
        .{ .name = "answer", .section = 1, .value = 0, .function = true },
        .{ .name = "call_ext", .section = 1, .value = 8, .function = true },
        .{ .name = "bump", .section = 1, .value = 24, .function = true },
        .{ .name = "counter", .section = 3, .value = 0, .class = .static },
        .{ .name = "ext", .section = 0 },
        .{ .name = "answer_pointer", .section = 2, .value = 0 },
        .{ .name = ".xdata", .section = 5, .class = .section },
    }, &.{
        .{ .section = 1, .offset = 13, .symbol = 4, .type = 0x0004 },
        .{ .section = 1, .offset = 26, .symbol = 3, .type = 0x0004 },
        .{ .section = 1, .offset = 34, .symbol = 3, .type = 0x0004 },
        .{ .section = 2, .offset = 0, .symbol = 0, .type = 0x0001 },
        .{ .section = 4, .offset = 0, .symbol = 1, .type = 0x0003 },
        .{ .section = 4, .offset = 4, .symbol = 1, .type = 0x0003 },
        .{ .section = 4, .offset = 8, .symbol = 6, .type = 0x0003 },
    });
    defer allocator.free(object);
    return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
}

fn testSeven() callconv(.c) i32 {
    return 7;
}

const TestResolver = struct {
    ext: ?Binding,

    fn resolve(context: *anyopaque, name: []const u8) ?Binding {
        const self: *TestResolver = @ptrCast(@alignCast(context));
        if (std.mem.eql(u8, name, "ext")) return self.ext;
        return null;
    }
};

fn exportAddress(loaded: *const Loaded, name: []const u8) usize {
    return loaded.symbolAddress(loaded.view.findExport(name).?);
}

test "a loaded image runs, calls its imports and resets its zero-fill" {
    const allocator = std.testing.allocator;
    const image = try testImage(allocator);
    defer allocator.free(image);
    const view = try UnitImage.View.init(image);
    var resolver = TestResolver{ .ext = .{ .address = @intFromPtr(&testSeven), .function = true } };
    var diagnostic: Diagnostic = .{};
    var loaded = try load(allocator, view, .{ .context = &resolver, .resolve = TestResolver.resolve }, &diagnostic);
    defer loaded.unload();
    const Fn = *const fn () callconv(.c) i32;
    const answer: Fn = @ptrFromInt(exportAddress(&loaded, "answer"));
    const call_ext: Fn = @ptrFromInt(exportAddress(&loaded, "call_ext"));
    const bump: Fn = @ptrFromInt(exportAddress(&loaded, "bump"));
    try std.testing.expectEqual(@as(i32, 42), answer());
    try std.testing.expectEqual(@as(i32, 7), call_ext());
    try std.testing.expectEqual(@as(i32, 1), bump());
    try std.testing.expectEqual(@as(i32, 2), bump());
    const pointer: *const Fn = @ptrFromInt(exportAddress(&loaded, "answer_pointer"));
    try std.testing.expectEqual(@as(i32, 42), pointer.*());
    try std.testing.expectEqual(@as(usize, if (arm64) 64 else 40), loaded.text().len);
}

test "a call out of reach goes through a stub, and a missing import is reported" {
    const allocator = std.testing.allocator;
    const image = try testImage(allocator);
    defer allocator.free(image);
    const view = try UnitImage.View.init(image);
    const far = @intFromPtr(&testSeven) +% (@as(usize, 1) << 44);
    var resolver = TestResolver{ .ext = .{ .address = far, .function = true } };
    var diagnostic: Diagnostic = .{};
    var loaded = try load(allocator, view, .{ .context = &resolver, .resolve = TestResolver.resolve }, &diagnostic);
    defer loaded.unload();
    const base = @intFromPtr(loaded.memory.ptr);
    if (arm64) {
        const call_site = exportAddress(&loaded, "call_ext") + 4;
        const instruction = std.mem.readInt(u32, loaded.memory[call_site - base ..][0..4], .little);
        const words: i26 = @bitCast(@as(u26, @truncate(instruction)));
        const stub = @as(usize, @intCast(@as(i64, @intCast(call_site)) + @as(i64, words) * 4));
        const stub_bytes = loaded.memory[stub - base ..][0..stub_size];
        try std.testing.expectEqual(@as(u32, 0x5800_0050), std.mem.readInt(u32, stub_bytes[0..4], .little));
        try std.testing.expectEqual(@as(u64, far), std.mem.readInt(u64, stub_bytes[8..16], .little));
    } else {
        const field_end = exportAddress(&loaded, "call_ext") + 9;
        const displacement = std.mem.readInt(i32, loaded.memory[field_end - 4 - base ..][0..4], .little);
        const stub = @as(usize, @intCast(@as(i64, @intCast(field_end)) + displacement));
        const stub_bytes = loaded.memory[stub - base ..][0..stub_size];
        try std.testing.expectEqualSlices(u8, &.{ 0xff, 0x25, 0, 0, 0, 0 }, stub_bytes[0..6]);
        try std.testing.expectEqual(@as(u64, far), std.mem.readInt(u64, stub_bytes[6..14], .little));
    }

    resolver.ext = null;
    try std.testing.expectError(error.UndefinedImport, load(allocator, view, .{ .context = &resolver, .resolve = TestResolver.resolve }, &diagnostic));
    try std.testing.expectEqualStrings("ext", diagnostic.name);

    resolver.ext = .{ .address = far, .function = false };
    try std.testing.expectError(error.SiteOutOfRange, load(allocator, view, .{ .context = &resolver, .resolve = TestResolver.resolve }, &diagnostic));
}
