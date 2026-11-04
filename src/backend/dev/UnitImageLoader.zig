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
//! Unit images hold the host's own code, x86_64 for a Linux compiler and
//! arm64 for a macOS one, so this loads them only on those hosts
//! (`backend.dev.UnitImageLoader` is `void` elsewhere).

const std = @import("std");
const builtin = @import("builtin");
const UnitImage = @import("UnitImage.zig");

const page_size = std.heap.page_size_min;
const stub_size: usize = 16;
const arm64 = builtin.cpu.arch == .aarch64;

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
    /// The block and its stubs, then the writable data and the zero-fill.
    memory: []align(page_size) u8,
    data_offset: usize,
    bss_offset: usize,

    /// The address `region` was loaded at.
    pub fn regionAddress(self: *const Loaded, region: UnitImage.Region) usize {
        const base = @intFromPtr(self.memory.ptr);
        return switch (region) {
            .block => base,
            .data => base + self.data_offset,
            .bss => base + self.bss_offset,
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

    /// Return the zero-fill to its loaded state: all zeros.
    pub fn resetZeroFill(self: *const Loaded) void {
        @memset(self.memory[self.bss_offset..][0..self.view.bssSize()], 0);
    }

    /// Release the image's memory.
    pub fn unload(self: *Loaded) void {
        std.posix.munmap(self.memory);
        self.* = undefined;
    }
};

/// Load `view`, binding its imports through `resolver`. The image's bytes
/// are only read during the call.
pub fn load(gpa: std.mem.Allocator, view: UnitImage.View, resolver: Resolver, diagnostic: *Diagnostic) LoadError!Loaded {
    const imports = view.imports();
    const block_len: usize = view.block().len;
    const stubs_offset = std.mem.alignForward(usize, block_len, stub_size);
    const exec_len = std.mem.alignForward(usize, stubs_offset + imports.len * stub_size, page_size);
    const data_offset = exec_len;
    const bss_offset = std.mem.alignForward(usize, data_offset + view.data().len, @max(view.bssAlign(), 1));
    const total = @max(std.mem.alignForward(usize, bss_offset + view.bssSize(), page_size), page_size);

    const memory = std.posix.mmap(
        mappingHint(),
        total,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch return error.OutOfMemory;
    errdefer std.posix.munmap(memory);
    var loaded: Loaded = .{ .view = view, .memory = memory, .data_offset = data_offset, .bss_offset = bss_offset };
    @memcpy(memory[0..block_len], view.block());
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
            .bss, .debug_line, .debug_info, .debug_abbrev => unreachable, // a site is a field with bytes in an allocated region: the block or the data
        };
        const field = memory[region_offset + site.offset ..][0..@intCast(UnitImage.kindWidth(kind))];
        const site_address = @intFromPtr(memory.ptr) + region_offset + site.offset;
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

    if (arm64) flushInstructionCache(memory[0..exec_len]);
    const executable: std.posix.PROT = .{ .READ = true, .EXEC = true };
    if (std.c.mprotect(@ptrCast(memory.ptr), exec_len, executable) != 0) return error.OutOfMemory;
    return loaded;
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

/// Publish newly written instructions before they run.
fn flushInstructionCache(memory: []const u8) void {
    if (comptime builtin.os.tag == .macos) {
        sys_icache_invalidate(memory.ptr, memory.len);
    } else {
        unreachable; // arm64 images are loaded only on a macOS compiler
    }
}

extern "c" fn sys_icache_invalidate(start: [*]const u8, len: usize) void;

/// An address below this binary's code, a preference for where the image
/// lands so its calls into the compiler reach it directly. The kernel may
/// place the image elsewhere; out-of-reach calls then use stubs.
fn mappingHint() ?[*]align(page_size) u8 {
    const anchor = @intFromPtr(&mappingHint);
    if (anchor <= (1 << 30)) return null;
    return @ptrFromInt(std.mem.alignBackward(usize, anchor - (1 << 28), page_size));
}

const TestObject = UnitImage.TestObject;
const TestMachObject = UnitImage.TestMachObject;
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
    if (arm64) {
        // answer: mov w0, #42; ret
        // call_ext: stp x29, x30, [sp, #-16]!; bl ext; ldp x29, x30, [sp], #16; ret
        // bump: adrp x8, counter; ldr x9, [x8, counter]; add x9, x9, #1; str x9, [x8, counter]; mov x0, x9; ret
        const text = [_]u32{
            0x5280_0540, 0xd65f_03c0, 0, 0,
            0xa9bf_7bfd, 0x9400_0000, 0xa8c1_7bfd, 0xd65f_03c0,
            0x9000_0008, 0xf940_0109, 0x9100_0529, 0xf900_0109, 0xaa09_03e0, 0xd65f_03c0, 0, 0,
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
    loaded.resetZeroFill();
    try std.testing.expectEqual(@as(i32, 1), bump());
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
