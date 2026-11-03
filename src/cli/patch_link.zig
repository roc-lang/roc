//! The patch link of a fast dev link (Linux x86_64 executables): the
//! executable is the pre-linked host's bytes (`host_prelink.zig`) followed
//! by the app region, made of the unit images (`backend.dev.UnitImage`) the
//! program reaches and the support image, placed at addresses that are a
//! pure function of the images and their keys.
//!
//! The app region is two segments: one read-and-execute segment holding
//! the `provides` windows and every image's code block (text, then its
//! read-only data), and one read-write segment holding every image's data
//! and zero-fill. Every reference left in an image is a patch site the link
//! fills; an import resolves first inside the app region, then against the
//! host's kept symbols. Debug information and symbols are appended after
//! the segments, with the host's own, under a rebuilt section header table.
//!
//! The output file is written through a shared mapping, one task per image
//! on the compilation's post-check executor. Every write lands in the
//! writing task's own region: a fixup writes into the referring site,
//! never into the referent, and every region an image owns is disjoint from
//! every other image's. The host's words, the windows and the file tail
//! belong to the coordinating thread.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const host_prelink = @import("host_prelink.zig");

const Allocator = std.mem.Allocator;
const UnitImage = backend.dev.UnitImage;
const linux = std.os.linux;
const Executor = base.post_check_task_executor.Executor;

const page_size: u64 = 4096;
const sym_size = 24;
const PT_LOAD = 1;
const PF_X = 1;
const PF_W = 2;
const PF_R = 4;
const SHT_PROGBITS = 1;
const SHT_SYMTAB = 2;
const SHT_STRTAB = 3;
const SHT_NOBITS = 8;
const SHF_WRITE = 0x1;
const SHF_ALLOC = 0x2;
const SHF_EXECINSTR = 0x4;
const STB_LOCAL = 0;

/// One image to place.
pub const ImageInput = struct {
    view: UnitImage.View,
};

/// Everything a patch link reads.
pub const Inputs = struct {
    host: host_prelink.View,
    /// The builtins and the Boxy runtime.
    support: ImageInput,
    /// Every unit of the program; `root` indexes the root unit.
    units: []const ImageInput,
    root: usize,
    /// Symbols the host must provide when the app imports them: the
    /// platform's hosted symbols and the runtime set. Sorted.
    host_required: []const []const u8,
};

/// Where the time went, for `--timings`.
pub const Timing = struct {
    layout_ns: u64 = 0,
    fan_out_ns: u64 = 0,
    finalize_ns: u64 = 0,
    /// Releasing the output's mapping.
    unmap_ns: u64 = 0,
    images_placed: u64 = 0,
    bytes_written: u64 = 0,
    sites_patched: u64 = 0,
};

/// Why a patch link failed.
pub const LinkError = Allocator.Error || error{
    /// The app imports a symbol the host must provide and does not. The
    /// names are in `Outcome.missing`.
    MissingHostSymbols,
    /// A `provides` data value is larger than its window.
    ProvidedValueTooLarge,
    /// The app region is too large for 32-bit displacements to the host.
    ProgramTooLarge,
} || OutputFile.Error;

/// What a failed patch link reports beyond its error.
pub const Outcome = struct {
    missing: []const []const u8 = &.{},
};

/// Link `inputs` into `output`, whose first `host.host().len` bytes already
/// hold the host. `missing` lists the host symbols the app imports and the
/// host lacks when the link fails with `MissingHostSymbols`, allocated from
/// `arena`.
pub fn link(gpa: Allocator, arena: Allocator, std_io: std.Io, inputs: Inputs, output: *OutputFile, executor: ?Executor, timing: *Timing, outcome: *Outcome) LinkError!void {
    const layout_started = now(std_io);
    var images: std.ArrayList(ImageInput) = .empty;
    defer images.deinit(gpa);
    try images.append(gpa, inputs.support);
    try images.appendSlice(gpa, inputs.units);
    const root_image = inputs.root + 1;

    // Every export of every image, by name. A procedure is named by its
    // identity, which is a function of its content, so two units can each
    // define the same one (a conversion worker each memoizes for itself);
    // the name resolves to the definition in the image with the smallest
    // content digest, whatever order the images arrive in.
    var exports: std.StringHashMapUnmanaged(SymbolRef) = .empty;
    defer exports.deinit(gpa);
    var export_total: u32 = 0;
    for (images.items) |image| export_total += @intCast(image.view.exports().len);
    try exports.ensureTotalCapacity(gpa, export_total);
    for (images.items, 0..) |image, image_index| {
        const symbols = image.view.symbols();
        for (image.view.exports()) |symbol_index| {
            const gop = exports.getOrPutAssumeCapacity(image.view.name(symbols[symbol_index].name));
            if (gop.found_existing and std.mem.order(u8, &images.items[gop.value_ptr.image].view.header.digest, &image.view.header.digest) != .gt) continue;
            gop.value_ptr.* = .{ .image = @intCast(image_index), .symbol = symbol_index };
        }
    }

    // What the program reaches, atom by atom, from the `provides` symbols:
    // an image is placed when one of its atoms is reached, and within it
    // only the reached atoms' bytes are written. Every import of a reached
    // image is resolved, the app region first, then the host.
    var walk: Walk = .{ .gpa = gpa, .images = images.items, .exports = &exports, .host = inputs.host, .host_required = inputs.host_required };
    defer walk.deinit();
    try walk.init();
    for (0..inputs.host.provideCount()) |index| {
        const found = exports.get(inputs.host.provideName(index)) orelse continue;
        try walk.reachSymbol(found.image, found.symbol);
    }
    std.debug.assert(walk.states[root_image] != null);
    try walk.run();
    if (walk.missing.count() != 0) {
        const names_out = try arena.alloc([]const u8, walk.missing.count());
        for (walk.missing.keys(), names_out) |name, *out| out.* = try arena.dupe(u8, name);
        outcome.missing = names_out;
        return error.MissingHostSymbols;
    }
    const reached = try gpa.alloc(bool, images.items.len);
    defer gpa.free(reached);
    for (walk.states, reached) |state, *placed| placed.* = state != null;

    // Placement order: the support image, then the units by content digest.
    var order: std.ArrayList(u32) = .empty;
    defer order.deinit(gpa);
    if (reached[0]) try order.append(gpa, 0);
    for (1..images.items.len) |index| if (reached[index]) try order.append(gpa, @intCast(index));
    std.mem.sort(u32, order.items[@intFromBool(reached[0])..], images.items, struct {
        fn lessThan(all: []const ImageInput, a: u32, b: u32) bool {
            return std.mem.order(u8, &all[a].view.header.digest, &all[b].view.header.digest) == .lt;
        }
    }.lessThan);
    timing.images_placed = order.items.len;

    // Layout.
    const host = inputs.host;
    const host_size = host.host().len;
    const provide_count = host.provideCount();
    const host_symtab: ?host_prelink.SectionHeader = if (host.header.symtab_index != 0) host.section(host.header.symtab_index) else null;
    const host_strtab: ?host_prelink.SectionHeader = if (host.header.symtab_index != 0) host.section(host.header.strtab_index) else null;
    // A host with a symbol table ends with its string table, and the app's
    // names follow it in place; the app's segments come after them.
    var app_strings: u64 = 0;
    for (order.items) |index| app_strings += images.items[index].view.strings().len;
    if (host_strtab) |strtab| std.debug.assert(strtab.offset + strtab.size == host_size);
    const app_offset = std.mem.alignForward(u64, host_size + (if (host_strtab != null) app_strings else 0), page_size);
    const places = try gpa.alloc(Place, images.items.len);
    defer gpa.free(places);
    @memset(places, .{});
    var rx_size: u64 = std.mem.alignForward(u64, provide_count * host_prelink.provide_window, page_size);
    for (order.items) |index| {
        const view = images.items[index].view;
        rx_size = std.mem.alignForward(u64, rx_size, page_size);
        places[index].block = host_prelink.app_base + rx_size;
        rx_size += view.block().len;
    }
    const rw_offset = std.mem.alignForward(u64, app_offset + rx_size, page_size);
    const rw_base = host_prelink.app_base + (rw_offset - app_offset);
    var rw_file_size: u64 = 0;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.data().len == 0) continue;
        rw_file_size = std.mem.alignForward(u64, rw_file_size, page_size);
        places[index].data = rw_base + rw_file_size;
        rw_file_size += view.data().len;
    }
    var rw_mem_size = rw_file_size;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.bssSize() == 0) continue;
        rw_mem_size = std.mem.alignForward(u64, rw_mem_size, @max(view.bssAlign(), 16));
        places[index].bss = rw_base + rw_mem_size;
        rw_mem_size += view.bssSize();
    }
    if (rw_base + rw_mem_size - host_prelink.app_base > std.math.maxInt(i32) - host_prelink.app_base) return error.ProgramTooLarge;

    // The file tail: debug sections, symbols, section names, headers.
    var tail_at = rw_offset + rw_file_size;
    const debug_regions = [_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line };
    const debug_host_index = [_]u32{ host.header.debug_abbrev_index, host.header.debug_info_index, host.header.debug_line_index };
    var debug_offsets: [3]u64 = undefined;
    var debug_sizes: [3]u64 = undefined;
    for (debug_regions, debug_host_index, 0..) |region, host_index, slot| {
        debug_offsets[slot] = tail_at;
        var size: u64 = if (host_index != 0) host.section(host_index).size else 0;
        for (order.items) |index| {
            const fragment = images.items[index].view.regionBytes(region);
            places[index].debug[slot] = size;
            size += fragment.len;
        }
        debug_sizes[slot] = size;
        tail_at += size;
    }
    var app_symbol_count: u64 = 0;
    var strtab_size: u64 = if (host_strtab) |strtab| strtab.size else 1;
    for (order.items) |index| {
        const view = images.items[index].view;
        places[index].first_symbol = 1 + app_symbol_count;
        app_symbol_count += view.symbols().len;
        places[index].strings = strtab_size;
        strtab_size += view.strings().len;
    }
    const host_symbol_count: u64 = if (host_symtab) |symtab| symtab.size / sym_size else 0;
    const symtab_count = 1 + app_symbol_count + (if (host_symbol_count == 0) 0 else host_symbol_count - 1);
    tail_at = std.mem.alignForward(u64, tail_at, 8);
    const symtab_offset = tail_at;
    tail_at += symtab_count * sym_size;
    const strtab_offset = if (host_strtab) |strtab| strtab.offset else tail_at;
    if (host_strtab == null) tail_at += strtab_size;

    // Section headers: the host's, then the app's, then any the host lacks.
    const host_names = host.section(host.header.shstrndx);
    var names: std.ArrayList(u8) = .empty;
    defer names.deinit(gpa);
    try names.appendSlice(gpa, host.host()[@intCast(host_names.offset)..][0..@intCast(host_names.size)]);
    var headers: std.ArrayList(host_prelink.SectionHeader) = .empty;
    defer headers.deinit(gpa);
    for (0..host.header.shnum) |index| try headers.append(gpa, host.section(@intCast(index)));
    const text_index: u32 = @intCast(headers.items.len);
    try headers.append(gpa, .{ .name = try addName(gpa, &names, ".text.roc"), .type = SHT_PROGBITS, .flags = SHF_ALLOC | SHF_EXECINSTR, .addr = host_prelink.app_base, .offset = app_offset, .size = rx_size, .link = 0, .info = 0, .addralign = page_size, .entsize = 0 });
    const data_index: u32 = @intCast(headers.items.len);
    try headers.append(gpa, .{ .name = try addName(gpa, &names, ".data.roc"), .type = SHT_PROGBITS, .flags = SHF_ALLOC | SHF_WRITE, .addr = rw_base, .offset = rw_offset, .size = rw_file_size, .link = 0, .info = 0, .addralign = page_size, .entsize = 0 });
    const bss_index: u32 = @intCast(headers.items.len);
    try headers.append(gpa, .{ .name = try addName(gpa, &names, ".bss.roc"), .type = SHT_NOBITS, .flags = SHF_ALLOC | SHF_WRITE, .addr = rw_base + rw_file_size, .offset = rw_offset + rw_file_size, .size = rw_mem_size - rw_file_size, .link = 0, .info = 0, .addralign = 16, .entsize = 0 });
    const debug_names = [_][]const u8{ ".debug_abbrev", ".debug_info", ".debug_line" };
    for (debug_host_index, debug_names, 0..) |host_index, debug_name, slot| {
        const header: host_prelink.SectionHeader = .{ .name = 0, .type = SHT_PROGBITS, .flags = 0, .addr = 0, .offset = debug_offsets[slot], .size = debug_sizes[slot], .link = 0, .info = 0, .addralign = 1, .entsize = 0 };
        if (host_index != 0) {
            headers.items[host_index].offset = header.offset;
            headers.items[host_index].size = header.size;
        } else if (debug_sizes[slot] != 0) {
            var added = header;
            added.name = try addName(gpa, &names, debug_name);
            try headers.append(gpa, added);
        }
    }
    const first_global = 1 + app_symbol_count + (if (host_symtab) |symtab| symtab.info -| 1 else 0);
    if (host_symtab != null) {
        const symtab = &headers.items[host.header.symtab_index];
        symtab.offset = symtab_offset;
        symtab.size = symtab_count * sym_size;
        symtab.info = @intCast(first_global);
        const strtab = &headers.items[host.header.strtab_index];
        strtab.offset = strtab_offset;
        strtab.size = strtab_size;
    } else {
        const strtab_index: u32 = @intCast(headers.items.len + 1);
        try headers.append(gpa, .{ .name = try addName(gpa, &names, ".symtab"), .type = SHT_SYMTAB, .flags = 0, .addr = 0, .offset = symtab_offset, .size = symtab_count * sym_size, .link = strtab_index, .info = @intCast(first_global), .addralign = 8, .entsize = sym_size });
        try headers.append(gpa, .{ .name = try addName(gpa, &names, ".strtab"), .type = SHT_STRTAB, .flags = 0, .addr = 0, .offset = strtab_offset, .size = strtab_size, .link = 0, .info = 0, .addralign = 1, .entsize = 0 });
    }
    const shstrtab_offset = tail_at;
    tail_at += names.items.len;
    headers.items[host.header.shstrndx].offset = shstrtab_offset;
    headers.items[host.header.shstrndx].size = names.items.len;
    tail_at = std.mem.alignForward(u64, tail_at, 8);
    const shoff = tail_at;
    tail_at += headers.items.len * 64;
    const file_size = tail_at;

    // Import addresses, now that every image has its place.
    const resolved = try gpa.alloc([]u64, images.items.len);
    defer {
        for (order.items) |index| gpa.free(resolved[index]);
        gpa.free(resolved);
    }
    for (order.items, 0..) |index, placed| {
        const targets = walk.states[index].?.targets;
        resolved[index] = gpa.alloc(u64, targets.len) catch |err| {
            for (order.items[0..placed]) |earlier| gpa.free(resolved[earlier]);
            return err;
        };
        for (targets, resolved[index]) |target, *address| {
            address.* = switch (target) {
                .app => |found| app: {
                    const symbol = images.items[found.image].view.symbols()[found.symbol];
                    break :app places[found.image].base(@fromBackingInt(symbol.region)) + symbol.value;
                },
                .host => |vaddr| vaddr,
                .missing => unreachable, // a missing host symbol failed the link above
            };
        }
    }
    const reached_atoms = try gpa.alloc([]const bool, images.items.len);
    defer gpa.free(reached_atoms);
    for (walk.states, reached_atoms) |state, *atoms| atoms.* = if (state) |placed| placed.atoms else &.{};

    // Provided values must fit their windows.
    const provided = try gpa.alloc(?SymbolRef, provide_count);
    defer gpa.free(provided);
    for (provided, 0..) |*slot, index| {
        const name = host.provideName(index);
        slot.* = exports.get(name);
        const found = slot.* orelse continue;
        const symbol = images.items[found.image].view.symbols()[found.symbol];
        if (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type)) != .func and symbol.size > host_prelink.provide_window) return error.ProvidedValueTooLarge;
    }

    try output.resize(file_size);
    const mapped = try output.map(file_size);
    var unmapped = false;
    defer if (!unmapped) output.unmap(mapped);
    timing.layout_ns = elapsed(std_io, layout_started);

    // Fan-out, in two waves of bounded tasks: first every byte copy and
    // every symbol entry, then every patch site, which writes into bytes
    // the first wave copied.
    const fan_out_started = now(std_io);
    const context: Context = .{
        .out = mapped,
        .images = images.items,
        .places = places,
        .resolved = resolved,
        .app_offset = app_offset,
        .rw_offset = rw_offset,
        .rw_base = rw_base,
        .debug_offsets = debug_offsets,
        .symtab_offset = symtab_offset,
        .strtab_offset = strtab_offset,
        .section_index = .{ text_index, data_index, bss_index },
        .reached_atoms = reached_atoms,
    };
    var copy_list: std.ArrayList(Copy) = .empty;
    defer copy_list.deinit(gpa);
    var copies: std.ArrayList(Task) = .empty;
    defer copies.deinit(gpa);
    var patches: std.ArrayList(Task) = .empty;
    defer patches.deinit(gpa);
    for (order.items) |index| {
        const view = images.items[index].view;
        const place = places[index];
        // Only reached atoms are written; the rest of the block stays a
        // hole of zeros.
        const atoms_reached = reached_atoms[index];
        var atom: u32 = 0;
        while (atom < atoms_reached.len) {
            if (!atoms_reached[atom]) {
                atom += 1;
                continue;
            }
            const first = view.atomRange(atom).start;
            while (atom < atoms_reached.len and atoms_reached[atom]) atom += 1;
            const end = view.atomRange(atom - 1).end;
            try addCopies(gpa, &copy_list, view.block()[first..end], context.fileOffsetOf(place.block) + first);
        }
        if (view.data().len != 0) try addCopies(gpa, &copy_list, view.data(), context.fileOffsetOf(place.data));
        for ([_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line }, 0..) |region, slot| {
            try addCopies(gpa, &copy_list, view.regionBytes(region), debug_offsets[slot] + place.debug[slot]);
        }
        try addCopies(gpa, &copy_list, view.strings(), strtab_offset + place.strings);
        try addRanges(gpa, &copies, &context, .symbols, index, view.symbols().len);
        try addRanges(gpa, &patches, &context, .sites, index, view.sites().len);
    }
    if (host_symtab) |symtab| try addCopies(gpa, &copy_list, hostBytes(host, symtab.offset + sym_size, symtab.size -| sym_size), symtab_offset + (1 + app_symbol_count) * sym_size);
    for (debug_host_index, 0..) |host_index, slot| {
        if (host_index != 0) try addCopies(gpa, &copy_list, hostBytes(host, host.section(host_index).offset, host.section(host_index).size), debug_offsets[slot]);
    }
    // Copies in tasks of about `copy_chunk` bytes each, however many
    // pieces that takes.
    var batch_first: usize = 0;
    var batch_bytes: usize = 0;
    for (copy_list.items, 0..) |copy, at| {
        batch_bytes += copy.from.len;
        if (batch_bytes < copy_chunk and at + 1 < copy_list.items.len) continue;
        try copies.append(gpa, .{ .context = &context, .work = .{ .copies = copy_list.items[batch_first .. at + 1] } });
        batch_first = at + 1;
        batch_bytes = 0;
    }
    try runTasks(gpa, executor, copies.items);
    try runTasks(gpa, executor, patches.items);
    for (copies.items) |task| timing.bytes_written += task.bytes;
    for (patches.items) |task| timing.sites_patched += task.sites;
    timing.fan_out_ns = elapsed(std_io, fan_out_started);

    // The coordinator's writes: windows, program headers, build id, ELF
    // header, section names and headers.
    const finalize_started = now(std_io);
    for (provided, 0..) |found_or_null, index| {
        const found = found_or_null orelse continue;
        const window = host_prelink.provideAddress(index);
        const window_bytes = mapped[@intCast(app_offset + (window - host_prelink.app_base))..][0..@intCast(host_prelink.provide_window)];
        const symbol = images.items[found.image].view.symbols()[found.symbol];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        const address = places[found.image].base(region) + symbol.value;
        if (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type)) == .func) {
            window_bytes[0] = 0xe9;
            std.mem.writeInt(i32, window_bytes[1..5], @intCast(@as(i64, @intCast(address)) - @as(i64, @intCast(window + 5))), .little);
            continue;
        }
        switch (region) {
            .block, .data => {
                const from = context.fileOffsetOf(address);
                @memcpy(window_bytes[0..symbol.size], mapped[@intCast(from)..][0..symbol.size]);
            },
            // Zero-fill: the window is already zero.
            .bss => {},
            .debug_line, .debug_info, .debug_abbrev => unreachable, // image symbols live only in allocated regions
        }
    }
    const phdrs = mapped[@intCast(host.header.phdr_offset)..][0 .. host.header.phnum * 56];
    writeLoad(phdrs[host.header.first_spare * 56 ..][0..56], app_offset, host_prelink.app_base, rx_size, rx_size, PF_R | PF_X);
    if (rw_mem_size != 0) writeLoad(phdrs[(host.header.first_spare + 1) * 56 ..][0..56], rw_offset, rw_base, rw_file_size, rw_mem_size, PF_R | PF_W);
    var build_id = std.crypto.hash.sha2.Sha256.init(.{});
    build_id.update("roc-fast-dev-link-build-id-v1");
    build_id.update(&host.header.host_digest);
    for (order.items) |index| build_id.update(&images.items[index].view.header.digest);
    const digest = build_id.finalResult();
    const id_bytes = mapped[@intCast(host.header.build_id_offset)..][0..host.header.build_id_size];
    for (id_bytes, 0..) |*byte, at| byte.* = digest[at % digest.len];
    @memset(mapped[@intCast(symtab_offset)..][0..sym_size], 0);
    if (host_strtab == null) mapped[@intCast(strtab_offset)] = 0;
    @memcpy(mapped[@intCast(shstrtab_offset)..][0..names.items.len], names.items);
    for (headers.items, 0..) |header, index| header.write(mapped[@intCast(shoff + index * 64)..][0..64]);
    std.mem.writeInt(u64, mapped[40..48], shoff, .little);
    std.mem.writeInt(u16, mapped[60..62], @intCast(headers.items.len), .little);
    timing.finalize_ns = elapsed(std_io, finalize_started);
    const unmap_started = now(std_io);
    output.unmap(mapped);
    unmapped = true;
    timing.unmap_ns = elapsed(std_io, unmap_started);
}

const SymbolRef = struct { image: u32, symbol: u32 };

/// What an import resolves to.
const Target = union(enum) {
    app: SymbolRef,
    host: u64,
    missing,
};

/// The atom-level reachability walk over every image.
const Walk = struct {
    gpa: Allocator,
    images: []const ImageInput,
    exports: *const std.StringHashMapUnmanaged(SymbolRef),
    host: host_prelink.View,
    host_required: []const []const u8,
    /// Per image, once reached: its reached atoms and its import targets.
    states: []?State = &.{},
    stack: std.ArrayList(struct { image: u32, atom: u32 }) = .empty,
    missing: std.StringArrayHashMapUnmanaged(void) = .empty,

    const State = struct {
        atoms: []bool,
        targets: []Target,
    };

    fn init(self: *Walk) Allocator.Error!void {
        self.states = try self.gpa.alloc(?State, self.images.len);
        @memset(self.states, null);
    }

    fn deinit(self: *Walk) void {
        for (self.states) |state_or_null| if (state_or_null) |state| {
            self.gpa.free(state.atoms);
            self.gpa.free(state.targets);
        };
        self.gpa.free(self.states);
        self.stack.deinit(self.gpa);
        self.missing.deinit(self.gpa);
    }

    /// Reach `image`'s symbol `symbol`, placing the image if it was not.
    fn reachSymbol(self: *Walk, image: u32, symbol: u32) Allocator.Error!void {
        const state = try self.place(image);
        const atom = self.images[image].view.symbolAtoms()[symbol];
        if (atom == UnitImage.no_atom) return;
        try self.reachAtom(image, state, atom);
    }

    fn reachAtom(self: *Walk, image: u32, state: State, atom: u32) Allocator.Error!void {
        if (state.atoms[atom]) return;
        state.atoms[atom] = true;
        try self.stack.append(self.gpa, .{ .image = image, .atom = atom });
    }

    /// Follow an edge or root entry of `image`.
    fn follow(self: *Walk, image: u32, state: State, entry: u32) Allocator.Error!void {
        if (entry & UnitImage.edge_import == 0) return self.reachAtom(image, state, entry);
        switch (state.targets[entry & ~UnitImage.edge_import]) {
            .app => |found| try self.reachSymbol(found.image, found.symbol),
            .host, .missing => {},
        }
    }

    /// `image`'s state, created with its imports resolved and its roots
    /// reached the first time.
    fn place(self: *Walk, image: u32) Allocator.Error!State {
        if (self.states[image]) |state| return state;
        const view = self.images[image].view;
        const atoms = try self.gpa.alloc(bool, view.atoms().len);
        errdefer self.gpa.free(atoms);
        @memset(atoms, false);
        const targets = try self.gpa.alloc(Target, view.imports().len);
        errdefer self.gpa.free(targets);
        for (view.imports(), targets) |import, *target| {
            const name = view.name(import.name);
            target.* = if (self.exports.get(name)) |found|
                .{ .app = found }
            else if (self.host.findSymbol(name)) |vaddr|
                .{ .host = vaddr }
            else missing: {
                if (findSorted(self.host_required, name) == null) {
                    base.invariant("fast dev link invariant violated: the app imports {s}, which is neither a program symbol, a hosted symbol, a runtime symbol, nor a support import", .{name});
                }
                try self.missing.put(self.gpa, name, {});
                break :missing .missing;
            };
        }
        const state: State = .{ .atoms = atoms, .targets = targets };
        self.states[image] = state;
        for (view.roots()) |entry| try self.follow(image, state, entry);
        return state;
    }

    fn run(self: *Walk) Allocator.Error!void {
        while (self.stack.pop()) |item| {
            const state = self.states[item.image].?;
            for (self.images[item.image].view.atomEdges(item.atom)) |entry| try self.follow(item.image, state, entry);
        }
    }
};

const Place = struct {
    block: u64 = 0,
    data: u64 = 0,
    bss: u64 = 0,
    /// Offsets of the image's fragments in the output debug sections:
    /// abbrev, info, line.
    debug: [3]u64 = .{ 0, 0, 0 },
    first_symbol: u64 = 0,
    /// Offset of the image's names in the output string table.
    strings: u64 = 0,

    fn base(self: Place, region: UnitImage.Region) u64 {
        return switch (region) {
            .block => self.block,
            .data => self.data,
            .bss => self.bss,
            .debug_abbrev => self.debug[0],
            .debug_info => self.debug[1],
            .debug_line => self.debug[2],
        };
    }
};

const Context = struct {
    out: []u8,
    images: []const ImageInput,
    places: []const Place,
    resolved: []const []u64,
    app_offset: u64,
    rw_offset: u64,
    rw_base: u64,
    debug_offsets: [3]u64,
    symtab_offset: u64,
    strtab_offset: u64,
    /// Section indices of the app's text, data and zero-fill.
    section_index: [3]u32,
    /// For each placed image, which of its atoms are reached.
    reached_atoms: []const []const bool,

    /// The file offset of an address in the app region's file-backed bytes.
    fn fileOffsetOf(self: *const Context, address: u64) u64 {
        if (address >= self.rw_base) return self.rw_offset + (address - self.rw_base);
        return self.app_offset + (address - host_prelink.app_base);
    }

    /// The output bytes of `region` of the image placed at `place`.
    fn regionOut(self: *const Context, place: Place, view: UnitImage.View, region: UnitImage.Region) []u8 {
        const len = view.regionBytes(region).len;
        if (len == 0) return &.{};
        return switch (region) {
            .block => self.out[@intCast(self.fileOffsetOf(place.block))..][0..len],
            .data => self.out[@intCast(self.fileOffsetOf(place.data))..][0..len],
            .bss => &.{},
            .debug_abbrev => self.out[@intCast(self.debug_offsets[0] + place.debug[0])..][0..len],
            .debug_info => self.out[@intCast(self.debug_offsets[1] + place.debug[1])..][0..len],
            .debug_line => self.out[@intCast(self.debug_offsets[2] + place.debug[2])..][0..len],
        };
    }
};

/// The most bytes one copy task moves.
const copy_chunk: usize = 256 * 1024;
/// The most sites or symbols one task handles.
const entry_chunk: u32 = 8192;

/// One run of bytes to copy into the output.
const Copy = struct { from: []const u8, to: u64 };

/// Add the copy of `from` to output offset `to`, in pieces of at most
/// `copy_chunk` bytes.
fn addCopies(gpa: Allocator, list: *std.ArrayList(Copy), from: []const u8, to: u64) Allocator.Error!void {
    var at: usize = 0;
    while (at < from.len) : (at += copy_chunk) {
        const len = @min(copy_chunk, from.len - at);
        try list.append(gpa, .{ .from = from[at..][0..len], .to = to + at });
    }
}

fn addRanges(gpa: Allocator, tasks: *std.ArrayList(Task), context: *const Context, comptime kind: enum { symbols, sites }, image: u32, count: usize) Allocator.Error!void {
    var first: u32 = 0;
    while (first < count) : (first += entry_chunk) {
        const range: Range = .{ .image = image, .first = first, .count = @min(entry_chunk, @as(u32, @intCast(count)) - first) };
        try tasks.append(gpa, .{ .context = context, .work = switch (kind) {
            .symbols => .{ .symbols = range },
            .sites => .{ .sites = range },
        } });
    }
}

const Range = struct { image: u32, first: u32, count: u32 };

const Task = struct {
    context: *const Context,
    work: union(enum) {
        copies: []const Copy,
        symbols: Range,
        sites: Range,
    },
    sites: u64 = 0,
    bytes: u64 = 0,

    fn run(opaque_task: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
        const self: *Task = @ptrCast(@alignCast(opaque_task));
        switch (self.work) {
            .copies => |list| for (list) |copy| {
                @memcpy(self.context.out[@intCast(copy.to)..][0..copy.from.len], copy.from);
                self.bytes += copy.from.len;
            },
            .symbols => |range| self.writeSymbols(range),
            .sites => |range| self.applySites(range),
        }
        return opaque_task;
    }

    fn writeSymbols(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const symbols = view.symbols()[range.first..][0..range.count];
        const out = context.out[@intCast(context.symtab_offset + (place.first_symbol + range.first) * sym_size)..][0 .. symbols.len * sym_size];
        for (symbols, 0..) |symbol, at| {
            const entry = out[at * sym_size ..][0..sym_size];
            const region: UnitImage.Region = @fromBackingInt(symbol.region);
            const elf_type: u8 = switch (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type))) {
                .notype => 0,
                .object => 1,
                .func => 2,
            };
            std.mem.writeInt(u32, entry[0..4], @intCast(place.strings + symbol.name), .little);
            entry[4] = (STB_LOCAL << 4) | elf_type;
            entry[5] = 0;
            std.mem.writeInt(u16, entry[6..8], @intCast(context.section_index[
                switch (region) {
                    .block => 0,
                    .data => 1,
                    .bss => 2,
                    .debug_line, .debug_info, .debug_abbrev => unreachable, // image symbols live only in allocated regions
                }
            ]), .little);
            std.mem.writeInt(u64, entry[8..16], place.base(region) + symbol.value, .little);
            std.mem.writeInt(u64, entry[16..24], symbol.size, .little);
        }
        self.bytes = out.len;
    }

    fn applySites(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const addresses = context.resolved[range.image];
        const atoms_reached = context.reached_atoms[range.image];
        for (view.sites()[range.first..][0..range.count]) |site| {
            if (site.atom != UnitImage.no_atom and !atoms_reached[site.atom]) continue;
            const target = if (site.target_region == UnitImage.import_target) addresses[site.target] else place.base(@fromBackingInt(site.target_region));
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const out = context.regionOut(place, view, region);
            const field = out[site.offset..][0..@intCast(UnitImage.kindWidth(kind))];
            const value = @as(i64, @intCast(target)) + site.addend;
            // The layout keeps the app region within 32-bit reach of the
            // host and of itself, and debug sections within 32-bit offsets.
            if (!UnitImage.applySite(field, kind, value, place.base(region) + site.offset)) {
                base.invariant("fast dev link invariant violated: a patch site's value does not fit its field", .{});
            }
        }
        self.sites = range.count;
    }
};

fn runTasks(gpa: Allocator, executor: ?Executor, tasks: []Task) Allocator.Error!void {
    const pool = executor orelse {
        for (tasks) |*task| _ = Task.run(task, .{ .id = 0, .allocator = gpa, .scratch = gpa, .lane_state = undefined });
        return;
    };
    const work = try gpa.alloc(base.post_check_task_executor.Task, tasks.len);
    defer gpa.free(work);
    for (tasks, work, 0..) |*task, *item, index| item.* = .{ .id = index, .context = task, .run = Task.run };
    const completions = try gpa.alloc(base.post_check_task_executor.Completion, tasks.len);
    defer gpa.free(completions);
    try pool.run(work, completions);
}

fn writeLoad(entry: *[56]u8, offset: u64, vaddr: u64, filesz: u64, memsz: u64, flags: u32) void {
    std.mem.writeInt(u32, entry[0..4], PT_LOAD, .little);
    std.mem.writeInt(u32, entry[4..8], flags, .little);
    std.mem.writeInt(u64, entry[8..16], offset, .little);
    std.mem.writeInt(u64, entry[16..24], vaddr, .little);
    std.mem.writeInt(u64, entry[24..32], vaddr, .little);
    std.mem.writeInt(u64, entry[32..40], filesz, .little);
    std.mem.writeInt(u64, entry[40..48], memsz, .little);
    std.mem.writeInt(u64, entry[48..56], page_size, .little);
}

fn hostBytes(host: host_prelink.View, offset: u64, size: u64) []const u8 {
    return host.host()[@intCast(offset)..][0..@intCast(size)];
}

fn addName(gpa: Allocator, names: *std.ArrayList(u8), name: []const u8) Allocator.Error!u32 {
    const offset: u32 = @intCast(names.items.len);
    try names.appendSlice(gpa, name);
    try names.append(gpa, 0);
    return offset;
}

fn findSorted(list: []const []const u8, wanted: []const u8) ?usize {
    var low: usize = 0;
    var high: usize = list.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        switch (std.mem.order(u8, list[mid], wanted)) {
            .lt => low = mid + 1,
            .gt => high = mid,
            .eq => return mid,
        }
    }
    return null;
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

/// The output executable, created before the build's compile work as an
/// anonymous file in the destination directory and given its name only when
/// it is complete (plan "Output file lifecycle"). On a filesystem without
/// anonymous files, a named temporary file beside the destination takes its
/// place; the bytes written are the same.
pub const OutputFile = struct {
    fd: linux.fd_t,
    /// The destination path, NUL-terminated.
    dest: [:0]const u8,
    /// The temporary name the file is linked under before the rename.
    temp: [:0]const u8,
    /// Whether the file already has `temp` as its name.
    named: bool,
    size: u64 = 0,

    pub const Error = Allocator.Error || error{ AccessDenied, NoSpaceLeft, FileNotFound, Unexpected };

    fn failure(errno: linux.E) Error {
        if (errno == .ACCES or errno == .PERM or errno == .ROFS) return error.AccessDenied;
        if (errno == .NOSPC or errno == .DQUOT) return error.NoSpaceLeft;
        if (errno == .NOENT or errno == .NOTDIR) return error.FileNotFound;
        return error.Unexpected;
    }

    /// Create the output for `dest_path`. Fails when the destination
    /// directory cannot hold it.
    pub fn create(arena: Allocator, dest_path: []const u8) Error!OutputFile {
        const dir = std.fs.path.dirname(dest_path) orelse ".";
        const dir_z = try arena.dupeSentinel(u8, dir, 0);
        const dest = try arena.dupeSentinel(u8, dest_path, 0);
        const temp = try std.fmt.allocPrintSentinel(arena, "{s}{s}.{s}.{d}.roc-link", .{ if (std.fs.path.dirname(dest_path) == null) "" else dir, if (std.fs.path.dirname(dest_path) == null) "" else "/", std.fs.path.basename(dest_path), linux.getpid() }, 0);
        const anonymous = linux.openat(linux.AT.FDCWD, dir_z, .{ .ACCMODE = .RDWR, .TMPFILE = true, .DIRECTORY = true, .CLOEXEC = true }, 0o777);
        const anonymous_errno = linux.errno(anonymous);
        if (anonymous_errno == .SUCCESS) return .{ .fd = @intCast(anonymous), .dest = dest, .temp = temp, .named = false };
        // Without anonymous files on this filesystem, a named file stands in.
        if (anonymous_errno != .OPNOTSUPP and anonymous_errno != .ISDIR) return failure(anonymous_errno);
        const named = linux.openat(linux.AT.FDCWD, temp, .{ .ACCMODE = .RDWR, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, 0o777);
        if (linux.errno(named) != .SUCCESS) return failure(linux.errno(named));
        return .{ .fd = @intCast(named), .dest = dest, .temp = temp, .named = true };
    }

    /// Copy `len` bytes at `offset` of `source` to the start of the output,
    /// in the kernel.
    pub fn copyFrom(self: *OutputFile, source: linux.fd_t, offset: u64, len: u64) Error!void {
        var in_offset: i64 = @intCast(offset);
        var out_offset: i64 = 0;
        var remaining = len;
        while (remaining != 0) {
            const rc = linux.copy_file_range(source, &in_offset, self.fd, &out_offset, @intCast(@min(remaining, 1 << 30)), 0);
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno != .SUCCESS) return failure(errno);
            if (rc == 0) return error.Unexpected;
            remaining -= rc;
        }
        self.size = @max(self.size, len);
    }

    /// Write `bytes` at the start of the output.
    pub fn writeStart(self: *OutputFile, bytes: []const u8) Error!void {
        var written: usize = 0;
        while (written < bytes.len) {
            const rc = linux.pwrite(self.fd, bytes.ptr + written, bytes.len - written, @intCast(written));
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno != .SUCCESS) return failure(errno);
            written += rc;
        }
        self.size = @max(self.size, bytes.len);
    }

    fn resize(self: *OutputFile, size: u64) Error!void {
        const rc = linux.ftruncate(self.fd, @intCast(size));
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        self.size = size;
    }

    fn map(self: *OutputFile, size: u64) Error![]u8 {
        const rc = linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0);
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        const start: [*]u8 = @ptrFromInt(rc);
        return start[0..@intCast(size)];
    }

    fn unmap(_: *OutputFile, bytes: []u8) void {
        _ = linux.munmap(bytes.ptr, bytes.len);
    }

    /// Give the complete output its destination name.
    pub fn publish(self: *OutputFile, arena: Allocator) Error!void {
        if (!self.named) {
            const proc_path = try std.fmt.allocPrintSentinel(arena, "/proc/self/fd/{d}", .{self.fd}, 0);
            while (true) {
                const rc = linux.linkat(linux.AT.FDCWD, proc_path, linux.AT.FDCWD, self.temp, linux.AT.SYMLINK_FOLLOW);
                const errno = linux.errno(rc);
                if (errno == .SUCCESS) break;
                if (errno == .INTR) continue;
                if (errno != .EXIST) return failure(errno);
                // A temporary name a killed build left behind.
                const unlinked = linux.unlinkat(linux.AT.FDCWD, self.temp, 0);
                if (linux.errno(unlinked) != .SUCCESS) return failure(linux.errno(unlinked));
            }
            self.named = true;
        }
        const renamed = linux.renameat(linux.AT.FDCWD, self.temp, linux.AT.FDCWD, self.dest);
        if (linux.errno(renamed) != .SUCCESS) return failure(linux.errno(renamed));
        self.named = false;
        _ = linux.close(self.fd);
        self.fd = -1;
    }

    /// Release an output that will not be published.
    pub fn discard(self: *OutputFile) void {
        if (self.fd < 0) return;
        _ = linux.close(self.fd);
        self.fd = -1;
        if (self.named) _ = linux.unlinkat(linux.AT.FDCWD, self.temp, 0);
    }
};
