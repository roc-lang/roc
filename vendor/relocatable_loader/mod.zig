//! Loads a relocatable object file (ELF, Mach-O, or COFF) into this process
//! and binds it to the host the way a static link binds an application to
//! its platform: every symbol the object leaves undefined is resolved by name
//! once, at load time, and the object's own calls and data references are
//! patched to the final addresses. A call whose target is within reach of the
//! instruction becomes a direct call; one that is not goes through a small
//! stub next to the code, and a GOT-relative reference goes through a slot
//! the loader fills. Nothing is resolved lazily and nothing is looked up at
//! run time.
//!
//! The relocation arithmetic is adapted from the Zig compiler's linker at
//! https://codeberg.org/ziglang/zig (`src/link/Elf/Atom.zig`,
//! `src/link/MachO/Atom.zig`, `src/link/MachO/Object.zig`,
//! `src/link/Coff.zig`, and `src/link/aarch64.zig`), licensed under the MIT
//! license. Thanks, Zig team!
//!
//! The loader handles what an object compiled from a self-contained module
//! contains: code, read-only data, writable data, zero-filled data, unwind
//! tables, and the relocations LLVM emits for those on x86-64 and aarch64.
//! Thread-local storage, common symbols, and lazily bound references are not
//! part of that and are rejected by name rather than left unpatched.

const std = @import("std");
const builtin = @import("builtin");
const memory = @import("memory.zig");
const elf = @import("elf.zig");
const macho = @import("macho.zig");
const coff = @import("coff.zig");

const Allocator = std.mem.Allocator;

/// Binds an undefined symbol to an address in this process. Returns null for
/// a name it does not provide.
pub const Resolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, []const u8) ?usize,

    pub fn lookup(self: Resolver, name: []const u8) ?usize {
        return self.resolve(self.context, name);
    }
};

pub const LoadError = error{
    OutOfMemory,
    /// Not an object file this loader reads (wrong format, architecture, or
    /// file type).
    UnsupportedObject,
    /// The object's structure is inconsistent.
    MalformedObject,
    /// A relocation kind the loader does not implement; its name is printed.
    UnsupportedRelocation,
    /// A symbol neither the object nor the resolver defines; its name is
    /// printed.
    UndefinedSymbol,
    /// A relocation's value does not fit its field even through a stub.
    RelocationOutOfRange,
    /// The process could not map or protect memory for the image.
    MappingFailed,
};

/// How a section is mapped once loaded.
pub const SectionKind = enum {
    /// Executable; mapped read-only and executable.
    code,
    /// Read-only data; mapped with the code.
    readonly,
    /// Writable data, including zero-filled data.
    writable,
};

/// One allocatable section of the parsed object.
pub const Section = struct {
    name: []const u8,
    kind: SectionKind,
    /// The section's file bytes; empty for a zero-filled section.
    bytes: []const u8,
    size: u64,
    alignment: u64,
    /// The section's address within the object's own address space, for
    /// formats whose symbol values and relocations are expressed in it.
    object_addr: u64 = 0,
    /// Assigned by placement.
    address: usize = 0,
};

pub const SymbolKind = enum {
    /// Defined at `value` bytes into `section`.
    defined,
    /// Defined outside the object; bound through the resolver.
    undefined,
    /// A constant value with no section.
    absolute,
    /// Defined in a section the loader does not map (debug information);
    /// only a relocation from another unmapped section may name it.
    unmapped,
};

pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    section: u32 = 0,
    value: u64 = 0,
    weak: bool = false,
    global: bool = false,
    /// Assigned by resolution.
    address: usize = 0,
};

/// What a relocation refers to.
pub const Target = union(enum) {
    symbol: u32,
    section: struct { index: u32, offset: u64 },
};

pub const Relocation = struct {
    /// The section being patched and the offset within it.
    section: u32,
    offset: u64,
    target: Target,
    /// The format's relocation type.
    kind: u32,
    addend: i64,
    /// Mach-O: the paired subtractor's target, whose address is subtracted.
    subtractor: ?Target = null,
    /// Mach-O: the relocation's own flags.
    pcrel: bool = false,
    length: u2 = 0,
};

/// The parts of an object the loader works from, in a form shared by every
/// format. Slices point into the arena the parser was given or into the
/// object bytes.
pub const Parsed = struct {
    format: Format,
    arch: std.Target.Cpu.Arch,
    sections: []Section,
    symbols: []Symbol,
    relocations: []Relocation,
};

pub const Format = enum { elf, macho, coff };

/// A loaded object: its mapped image and the addresses of its defined symbols.
pub const Image = struct {
    mapping: memory.Mapping,
    symbols: std.StringHashMapUnmanaged(usize),
    arena: std.heap.ArenaAllocator,
    unwind: coff.UnwindRegistration = .{},

    /// Load `object_bytes` and bind its undefined symbols through `resolver`.
    /// The bytes are only read during the call.
    pub fn load(gpa: Allocator, object_bytes: []const u8, resolver: Resolver) LoadError!Image {
        var parse_arena = std.heap.ArenaAllocator.init(gpa);
        defer parse_arena.deinit();
        const parsed = try parse(parse_arena.allocator(), object_bytes);

        var loader = Loader{
            .gpa = gpa,
            .parsed = parsed,
            .resolver = resolver,
        };
        return loader.load();
    }

    /// The address of a symbol the object defines, or null.
    pub fn lookup(self: *const Image, comptime T: type, name: []const u8) ?T {
        const address = self.symbols.get(name) orelse return null;
        return @ptrFromInt(address);
    }

    /// The address of a symbol the object defines, or null.
    pub fn lookupAddress(self: *const Image, name: []const u8) ?usize {
        return self.symbols.get(name);
    }

    pub fn deinit(self: *Image) void {
        self.unwind.deinit();
        self.mapping.unmap();
        self.symbols.deinit(self.arena.child_allocator);
        self.arena.deinit();
        self.* = undefined;
    }
};

/// Read an object file's structure without loading it.
pub fn parse(arena: Allocator, object_bytes: []const u8) LoadError!Parsed {
    if (elf.matches(object_bytes)) return elf.parse(arena, object_bytes);
    if (macho.matches(object_bytes)) return macho.parse(arena, object_bytes);
    if (coff.matches(object_bytes)) return coff.parse(arena, object_bytes);
    return error.UnsupportedObject;
}

const stub_size: usize = 16;
const got_slot_size: usize = 8;

/// The context relocation arithmetic runs in: final addresses plus the
/// stub and GOT areas a relocation may need.
pub const Apply = struct {
    loader: *Loader,
    /// The relocation being applied.
    reloc: Relocation,
    /// The address of the field being patched.
    place: usize,
    /// The field's bytes in the image.
    code: []u8,
    /// The address the relocation targets, plus its addend.
    target: usize,
    addend: i64,

    /// `S`: the target's address.
    pub fn symbolAddress(self: *const Apply) i64 {
        return @intCast(self.target);
    }

    /// `A`.
    pub fn addendValue(self: *const Apply) i64 {
        return self.addend;
    }

    /// `P`: the address of the field.
    pub fn placeAddress(self: *const Apply) i64 {
        return @intCast(self.place);
    }

    /// The address the subtractor targets, or 0 without one.
    pub fn subtractorAddress(self: *const Apply) LoadError!i64 {
        const sub = self.reloc.subtractor orelse return 0;
        return @intCast(try self.loader.targetAddress(sub));
    }

    /// The address of the GOT slot holding the target's address.
    pub fn gotSlot(self: *const Apply) LoadError!usize {
        return self.loader.gotSlotFor(self.reloc.target, self.target);
    }

    /// The address of a stub that jumps to the target, for a branch whose
    /// target is out of reach.
    pub fn stub(self: *const Apply) LoadError!usize {
        return self.loader.stubFor(self.reloc.target, self.target);
    }

    /// The image base, for image-relative fields.
    pub fn imageBase(self: *const Apply) usize {
        return @intFromPtr(self.loader.mapping.base);
    }

    pub fn write(self: *const Apply, comptime T: type, value: T) void {
        std.mem.writeInt(T, self.code[0..@sizeOf(T)], value, .little);
    }

    pub fn read(self: *const Apply, comptime T: type) T {
        return std.mem.readInt(T, self.code[0..@sizeOf(T)], .little);
    }

    pub fn unsupported(self: *const Apply, name: []const u8) LoadError {
        std.debug.print("relocatable loader: unsupported relocation {s} in section {s}\n", .{ name, self.loader.parsed.sections[self.reloc.section].name });
        return error.UnsupportedRelocation;
    }
};

/// Placement, symbol resolution, and relocation of one parsed object.
pub const Loader = struct {
    gpa: Allocator,
    parsed: Parsed,
    resolver: Resolver,
    mapping: memory.Mapping = undefined,
    /// Byte length of the code and read-only region at the start of the
    /// mapping; the writable region follows.
    exec_len: usize = 0,
    got_base: usize = 0,
    got_count: usize = 0,
    got_capacity: usize = 0,
    got_slots: std.AutoHashMapUnmanaged(usize, usize) = .empty,
    stub_base: usize = 0,
    stub_count: usize = 0,
    stub_capacity: usize = 0,
    stubs: std.AutoHashMapUnmanaged(usize, usize) = .empty,

    fn load(self: *Loader) LoadError!Image {
        defer self.got_slots.deinit(self.gpa);
        defer self.stubs.deinit(self.gpa);

        try self.layOut();
        errdefer self.mapping.unmap();
        self.copySections();
        try self.resolveSymbols();
        try self.applyRelocations();
        self.mapping.protectCode(self.exec_len) catch return error.MappingFailed;

        var image = Image{
            .mapping = self.mapping,
            .symbols = .empty,
            .arena = std.heap.ArenaAllocator.init(self.gpa),
        };
        errdefer image.arena.deinit();
        errdefer image.symbols.deinit(self.gpa);
        const names = image.arena.allocator();
        for (self.parsed.symbols) |symbol| {
            if (symbol.kind != .defined or symbol.name.len == 0) continue;
            const gop = try image.symbols.getOrPut(self.gpa, symbol.name);
            if (gop.found_existing) {
                // A global definition wins over a local one of the same name.
                if (symbol.global) gop.value_ptr.* = symbol.address;
                continue;
            }
            gop.key_ptr.* = try names.dupe(u8, symbol.name);
            gop.value_ptr.* = symbol.address;
        }
        if (self.parsed.format == .coff) {
            image.unwind = coff.registerUnwind(self.parsed, &self.mapping);
        }
        return image;
    }

    /// Lay the sections out: code and read-only data first, then the GOT and
    /// stub areas, then writable data on its own pages.
    fn layOut(self: *Loader) LoadError!void {
        var exec_end: usize = 0;
        var data_end: usize = 0;
        for (self.parsed.sections) |*section| {
            const alignment: usize = @intCast(@max(section.alignment, 1));
            const size: usize = std.math.cast(usize, section.size) orelse return error.MalformedObject;
            switch (section.kind) {
                .code, .readonly => {
                    exec_end = std.mem.alignForward(usize, exec_end, alignment);
                    section.address = exec_end;
                    exec_end += size;
                },
                .writable => {
                    data_end = std.mem.alignForward(usize, data_end, alignment);
                    section.address = data_end;
                    data_end += size;
                },
            }
        }

        // Every symbol that a GOT-relative relocation names needs a slot, and
        // every symbol a branch might not reach needs a stub. Reserve one of
        // each per referenced symbol; the counts are small.
        var got_needed: usize = 0;
        var stubs_needed: usize = 0;
        for (self.parsed.relocations) |reloc| {
            if (needsGot(self.parsed, reloc)) got_needed += 1;
            if (mayNeedStub(self.parsed, reloc)) stubs_needed += 1;
        }
        exec_end = std.mem.alignForward(usize, exec_end, got_slot_size);
        self.got_base = exec_end;
        self.got_capacity = got_needed;
        exec_end += got_needed * got_slot_size;
        exec_end = std.mem.alignForward(usize, exec_end, stub_size);
        self.stub_base = exec_end;
        self.stub_capacity = stubs_needed;
        exec_end += stubs_needed * stub_size;

        const page = std.heap.page_size_min;
        self.exec_len = std.mem.alignForward(usize, exec_end, page);
        const total = self.exec_len + std.mem.alignForward(usize, data_end, page);
        self.mapping = memory.Mapping.map(@max(total, page), anchorAddress()) catch return error.MappingFailed;

        const base = @intFromPtr(self.mapping.base);
        for (self.parsed.sections) |*section| {
            section.address += switch (section.kind) {
                .code, .readonly => base,
                .writable => base + self.exec_len,
            };
        }
        self.got_base += base;
        self.stub_base += base;
    }

    fn copySections(self: *Loader) void {
        for (self.parsed.sections) |section| {
            if (section.bytes.len == 0) continue;
            const dest: [*]u8 = @ptrFromInt(section.address);
            @memcpy(dest[0..section.bytes.len], section.bytes);
        }
    }

    fn resolveSymbols(self: *Loader) LoadError!void {
        for (self.parsed.symbols) |*symbol| {
            switch (symbol.kind) {
                .defined => {
                    const section = self.parsed.sections[symbol.section];
                    symbol.address = section.address + @as(usize, @intCast(symbol.value));
                },
                .absolute => symbol.address = @intCast(symbol.value),
                .undefined => {
                    if (self.resolver.lookup(symbol.name)) |address| {
                        symbol.address = address;
                    } else if (symbol.weak) {
                        symbol.address = 0;
                    } else {
                        std.debug.print("relocatable loader: undefined symbol {s}\n", .{symbol.name});
                        return error.UndefinedSymbol;
                    }
                },
                .unmapped => symbol.address = 0,
            }
        }
    }

    fn targetAddress(self: *Loader, target: Target) LoadError!usize {
        return switch (target) {
            .symbol => |index| blk: {
                const symbol = self.parsed.symbols[index];
                if (symbol.kind == .unmapped) {
                    std.debug.print("relocatable loader: relocation names {s}, which lives in an unmapped section\n", .{symbol.name});
                    return error.MalformedObject;
                }
                break :blk symbol.address;
            },
            .section => |section| self.parsed.sections[section.index].address + @as(usize, @intCast(section.offset)),
        };
    }

    fn applyRelocations(self: *Loader) LoadError!void {
        for (self.parsed.relocations) |reloc| {
            const section = self.parsed.sections[reloc.section];
            const offset = std.math.cast(usize, reloc.offset) orelse return error.MalformedObject;
            if (offset >= section.size) return error.MalformedObject;
            const place = section.address + offset;
            const remaining: usize = @intCast(section.size - offset);
            const code: [*]u8 = @ptrFromInt(place);
            var apply = Apply{
                .loader = self,
                .reloc = reloc,
                .place = place,
                .code = code[0..remaining],
                .target = try self.targetAddress(reloc.target),
                .addend = reloc.addend,
            };
            switch (self.parsed.format) {
                .elf => try elf.apply(&apply, self.parsed.arch),
                .macho => try macho.apply(&apply, self.parsed.arch),
                .coff => try coff.apply(&apply, self.parsed.arch),
            }
        }
    }

    /// The GOT slot for `target`, created on first use.
    fn gotSlotFor(self: *Loader, target: Target, address: usize) LoadError!usize {
        const key = targetKey(target);
        if (self.got_slots.get(key)) |slot| return slot;
        if (self.got_count >= self.got_capacity) return error.MalformedObject;
        const slot = self.got_base + self.got_count * got_slot_size;
        self.got_count += 1;
        const slot_ptr: *align(1) usize = @ptrFromInt(slot);
        slot_ptr.* = address;
        try self.got_slots.put(self.gpa, key, slot);
        return slot;
    }

    /// The stub jumping to `target`, created on first use.
    fn stubFor(self: *Loader, target: Target, address: usize) LoadError!usize {
        const key = targetKey(target);
        if (self.stubs.get(key)) |stub| return stub;
        if (self.stub_count >= self.stub_capacity) return error.MalformedObject;
        const stub = self.stub_base + self.stub_count * stub_size;
        self.stub_count += 1;
        const bytes: *[stub_size]u8 = @ptrFromInt(stub);
        switch (self.parsed.arch) {
            .x86_64 => {
                // jmp qword ptr [rip + 0]; .quad target
                bytes[0..6].* = .{ 0xff, 0x25, 0x00, 0x00, 0x00, 0x00 };
                std.mem.writeInt(u64, bytes[6..14], address, .little);
                bytes[14..16].* = .{ 0xcc, 0xcc };
            },
            .aarch64 => {
                // ldr x16, [pc, #8]; br x16; .quad target
                std.mem.writeInt(u32, bytes[0..4], 0x58000050, .little);
                std.mem.writeInt(u32, bytes[4..8], 0xd61f0200, .little);
                std.mem.writeInt(u64, bytes[8..16], address, .little);
            },
            else => return error.UnsupportedObject,
        }
        try self.stubs.put(self.gpa, key, stub);
        return stub;
    }
};

fn targetKey(target: Target) usize {
    return switch (target) {
        .symbol => |index| index,
        .section => |section| (@as(usize, 1) << 62) | (@as(usize, section.index) << 40) | @as(usize, @intCast(section.offset & 0xff_ffff_ffff)),
    };
}

fn needsGot(parsed: Parsed, reloc: Relocation) bool {
    return switch (parsed.format) {
        .elf => elf.needsGot(parsed.arch, reloc.kind),
        .macho => macho.needsGot(parsed.arch, reloc.kind),
        .coff => false,
    };
}

fn mayNeedStub(parsed: Parsed, reloc: Relocation) bool {
    return switch (parsed.format) {
        .elf => elf.isBranch(parsed.arch, reloc.kind),
        .macho => macho.isBranch(parsed.arch, reloc.kind),
        .coff => coff.isBranch(parsed.arch, reloc.kind),
    };
}

/// An address inside this binary's code, used as a hint so the image lands
/// within direct-branch range of the host's functions when the address space
/// allows it. Out-of-range targets go through stubs either way.
fn anchorAddress() usize {
    return @intFromPtr(&anchorAddress);
}

test {
    std.testing.refAllDecls(@This());
    _ = @import("test.zig");
}
