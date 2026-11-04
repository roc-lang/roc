//! The patch link of a fast dev link of an ELF executable (Linux or a BSD,
//! x86_64 or AArch64): the
//! executable is the pre-linked host's bytes (`host_prelink.zig`) followed
//! by the app region, made of the unit images (`backend.dev.UnitImage`) the
//! program reaches and the support image, placed at addresses that are a
//! pure function of the images and their keys.
//!
//! The app region is two segments: one read-and-execute segment holding
//! the `provides` windows and every image's code block (text, then its
//! read-only data, then on AArch64 a veneer per import, `jump_window`), and
//! one read-write segment holding every image's data and zero-fill. Every
//! reference left in an image is a patch site the link fills; an import
//! resolves first inside the app region, then against the host's kept
//! symbols, and an AArch64 branch that cannot reach its import takes the
//! import's veneer. Debug information and symbols are appended after
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
const unit_walk = @import("unit_walk.zig");
const fanout = @import("patch_fanout.zig");
const link_output = @import("link_output.zig");
const jump_window = @import("jump_window.zig");

const Allocator = std.mem.Allocator;
const UnitImage = backend.dev.UnitImage;
const Executor = base.post_check_task_executor.Executor;

const page_size: u64 = 4096;
/// An AArch64 kernel may run with 4, 16 or 64 KiB pages; a segment boundary
/// congruent modulo 64 KiB loads under any of them, as LLD's own AArch64
/// layout does.
const aarch64_segment_align: u64 = 65536;
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
pub const ImageInput = unit_walk.ImageInput;

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
    /// The provided data symbol larger than its window, for
    /// `ProvidedValueTooLarge`; allocated from the arena.
    too_large: ?struct { symbol: []const u8, size: u64 } = null,
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

    // Every export of every image, by name (`unit_walk.collectExports`).
    var exports = try unit_walk.collectExports(gpa, images.items);
    defer exports.deinit(gpa);

    // What the program reaches, atom by atom, from the `provides` symbols:
    // an image is placed when one of its atoms is reached, and within it
    // only the reached atoms' bytes are written. Every import of a reached
    // image is resolved, the app region first, then the host.
    const host_view = inputs.host;
    var walk: unit_walk.Walk = .{ .gpa = gpa, .images = images.items, .resolver = .{ .exports = &exports, .host = .{ .context = &host_view, .find = findHostSymbol }, .host_required = inputs.host_required } };
    defer walk.deinit();
    try walk.init();
    // The checked program defines every `provides` symbol (the checker
    // rejects an app that lacks one), so each resolves to an export.
    for (0..inputs.host.provideCount()) |index| {
        const found = exports.get(inputs.host.provideName(index)) orelse base.invariant("fast dev link invariant violated: no unit image defines the provided symbol {s}", .{inputs.host.provideName(index)});
        try walk.reachSymbol(found.image, found.symbol);
    }
    std.debug.assert(walk.states[root_image] != null);
    try walk.run();
    if (walk.resolver.missing.count() != 0) {
        const names_out = try arena.alloc([]const u8, walk.resolver.missing.count());
        for (walk.resolver.missing.keys(), names_out) |name, *out| out.* = try arena.dupe(u8, name);
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
    const arm64 = host.arm64();
    const segment_align: u64 = if (arm64) aarch64_segment_align else page_size;
    const host_size = host.host().len;
    const provide_count = host.provideCount();
    const host_symtab: ?host_prelink.SectionHeader = if (host.header.symtab_index != 0) host.section(host.header.symtab_index) else null;
    const host_strtab: ?host_prelink.SectionHeader = if (host.header.symtab_index != 0) host.section(host.header.strtab_index) else null;
    // A host with a symbol table ends with its string table, and the app's
    // names follow it in place; the app's segments come after them.
    var app_strings: u64 = 0;
    for (order.items) |index| app_strings += images.items[index].view.strings().len;
    if (host_strtab) |strtab| std.debug.assert(strtab.offset + strtab.size == host_size);
    const app_offset = std.mem.alignForward(u64, host_size + (if (host_strtab != null) app_strings else 0), segment_align);
    const places = try gpa.alloc(Place, images.items.len);
    defer gpa.free(places);
    @memset(places, .{});
    var rx_size: u64 = std.mem.alignForward(u64, provide_count * host_prelink.provide_window, page_size);
    for (order.items) |index| {
        const view = images.items[index].view;
        rx_size = std.mem.alignForward(u64, rx_size, page_size);
        places[index].block = host_prelink.app_base + rx_size;
        rx_size += view.block().len;
        if (arm64) {
            rx_size = std.mem.alignForward(u64, rx_size, 4);
            places[index].veneers = host_prelink.app_base + rx_size;
            rx_size += view.imports().len * jump_window.veneer_size;
        }
    }
    const rw_offset = std.mem.alignForward(u64, app_offset + rx_size, segment_align);
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
    // Symbols of code the link leaves as a hole get no entry: each placed
    // image's symbol table index maps to its entry's position among the
    // image's kept symbols, or `no_symbol`.
    const reached_atoms = try gpa.alloc([]const bool, images.items.len);
    defer gpa.free(reached_atoms);
    for (walk.states, reached_atoms) |state, *atoms| atoms.* = if (state) |placed| placed.atoms else &.{};
    const symbol_slots = try gpa.alloc([]u32, images.items.len);
    @memset(symbol_slots, &.{});
    defer {
        for (order.items) |index| gpa.free(symbol_slots[index]);
        gpa.free(symbol_slots);
    }
    var app_symbol_count: u64 = 0;
    var strtab_size: u64 = if (host_strtab) |strtab| strtab.size else 1;
    for (order.items) |index| {
        const view = images.items[index].view;
        const slots = try gpa.alloc(u32, view.symbols().len);
        symbol_slots[index] = slots;
        var kept: u32 = 0;
        for (view.symbolAtoms(), slots) |atom, *slot| {
            if (atom != UnitImage.no_atom and !reached_atoms[index][atom]) {
                slot.* = no_symbol;
                continue;
            }
            slot.* = kept;
            kept += 1;
        }
        places[index].first_symbol = 1 + app_symbol_count;
        app_symbol_count += kept;
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
    try headers.append(gpa, .{ .name = try addName(gpa, &names, ".text.roc"), .type = SHT_PROGBITS, .flags = SHF_ALLOC | SHF_EXECINSTR, .addr = host_prelink.app_base, .offset = app_offset, .size = rx_size, .link = 0, .info = 0, .addralign = segment_align, .entsize = 0 });
    const data_index: u32 = @intCast(headers.items.len);
    try headers.append(gpa, .{ .name = try addName(gpa, &names, ".data.roc"), .type = SHT_PROGBITS, .flags = SHF_ALLOC | SHF_WRITE, .addr = rw_base, .offset = rw_offset, .size = rw_file_size, .link = 0, .info = 0, .addralign = segment_align, .entsize = 0 });
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
    @memset(resolved, &.{});
    defer {
        for (order.items) |index| gpa.free(resolved[index]);
        gpa.free(resolved);
    }
    for (order.items) |index| {
        const targets = walk.states[index].?.targets;
        resolved[index] = try gpa.alloc(u64, targets.len);
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
    // Provided values must fit their windows.
    const provided = try gpa.alloc(SymbolRef, provide_count);
    defer gpa.free(provided);
    for (provided, 0..) |*slot, index| {
        const name = host.provideName(index);
        slot.* = exports.get(name) orelse unreachable; // the walk above resolved every provided symbol
        const symbol = images.items[slot.image].view.symbols()[slot.symbol];
        if (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type)) != .func and symbol.size > host_prelink.provide_window) {
            outcome.too_large = .{ .symbol = try arena.dupe(u8, name), .size = symbol.size };
            return error.ProvidedValueTooLarge;
        }
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
        .symbol_slots = symbol_slots,
    };
    var copy_list: std.ArrayList(fanout.Copy) = .empty;
    defer copy_list.deinit(gpa);
    var symbol_ranges: std.ArrayList(fanout.Range) = .empty;
    defer symbol_ranges.deinit(gpa);
    var site_ranges: std.ArrayList(fanout.Range) = .empty;
    defer site_ranges.deinit(gpa);
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
            try fanout.addCopies(gpa, &copy_list, view.block()[first..end], context.fileOffsetOf(place.block) + first);
        }
        if (view.data().len != 0) try fanout.addCopies(gpa, &copy_list, view.data(), context.fileOffsetOf(place.data));
        for ([_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line }, 0..) |region, slot| {
            try fanout.addCopies(gpa, &copy_list, view.regionBytes(region), debug_offsets[slot] + place.debug[slot]);
        }
        try fanout.addCopies(gpa, &copy_list, view.strings(), strtab_offset + place.strings);
        try fanout.addRanges(gpa, &symbol_ranges, index, view.symbols().len);
        try fanout.addRanges(gpa, &site_ranges, index, view.sites().len);
    }
    if (host_symtab) |symtab| try fanout.addCopies(gpa, &copy_list, hostBytes(host, symtab.offset + sym_size, symtab.size -| sym_size), symtab_offset + (1 + app_symbol_count) * sym_size);
    for (debug_host_index, 0..) |host_index, slot| {
        if (host_index != 0) try fanout.addCopies(gpa, &copy_list, hostBytes(host, host.section(host_index).offset, host.section(host_index).size), debug_offsets[slot]);
    }
    // Copies in tasks of about `copy_chunk` bytes each, however many
    // pieces that takes, with the symbol entries; then every patch site,
    // which writes into bytes the first wave copied.
    const copy_batches = try fanout.batches(gpa, copy_list.items);
    defer gpa.free(copy_batches);
    var copies: std.ArrayList(Task) = .empty;
    defer copies.deinit(gpa);
    for (copy_batches) |batch| try copies.append(gpa, .{ .context = &context, .work = .{ .copies = batch } });
    for (symbol_ranges.items) |range| try copies.append(gpa, .{ .context = &context, .work = .{ .symbols = range } });
    if (arm64) for (order.items) |index| try copies.append(gpa, .{ .context = &context, .work = .{ .veneers = index } });
    var patches: std.ArrayList(Task) = .empty;
    defer patches.deinit(gpa);
    for (site_ranges.items) |range| try patches.append(gpa, .{ .context = &context, .work = .{ .sites = range } });
    try fanout.runTasks(gpa, executor, Task, copies.items);
    try fanout.runTasks(gpa, executor, Task, patches.items);
    for (copies.items) |task| timing.bytes_written += task.bytes;
    for (patches.items) |task| timing.sites_patched += task.sites;
    timing.fan_out_ns = elapsed(std_io, fan_out_started);

    // The coordinator's writes: windows, program headers, build id, ELF
    // header, section names and headers.
    const finalize_started = now(std_io);
    for (provided, 0..) |found, index| {
        const window = host_prelink.provideAddress(index);
        const window_bytes = mapped[@intCast(app_offset + (window - host_prelink.app_base))..][0..@intCast(host_prelink.provide_window)];
        const symbol = images.items[found.image].view.symbols()[found.symbol];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        const address = places[found.image].base(region) + symbol.value;
        if (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type)) == .func) {
            jump_window.writeJump(arm64, window_bytes[0..16], window, address);
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
            .pdata, .xdata => unreachable, // an ELF image carries no unwind regions
        }
    }
    const phdrs = mapped[@intCast(host.header.phdr_offset)..][0 .. host.header.phnum * 56];
    writeLoad(phdrs[host.header.first_spare * 56 ..][0..56], app_offset, host_prelink.app_base, rx_size, rx_size, PF_R | PF_X, segment_align);
    if (rw_mem_size != 0) writeLoad(phdrs[(host.header.first_spare + 1) * 56 ..][0..56], rw_offset, rw_base, rw_file_size, rw_mem_size, PF_R | PF_W, segment_align);
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

const SymbolRef = unit_walk.SymbolRef;

fn findHostSymbol(context: *const anyopaque, name: []const u8) ?u64 {
    const host: *const host_prelink.View = @ptrCast(@alignCast(context));
    return host.findSymbol(name);
}

/// `Context.symbol_slots` entry of a symbol the output's symbol table omits.
const no_symbol: u32 = std.math.maxInt(u32);

const Place = struct {
    block: u64 = 0,
    /// On AArch64, the image's veneers, one per import in import order.
    veneers: u64 = 0,
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
            .pdata, .xdata => unreachable, // an ELF image carries no unwind regions
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
    /// For each placed image, each symbol's position among the image's
    /// entries in the output symbol table, or `no_symbol`.
    symbol_slots: []const []const u32,

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
            .pdata, .xdata => unreachable, // an ELF image carries no unwind regions
            .debug_abbrev => self.out[@intCast(self.debug_offsets[0] + place.debug[0])..][0..len],
            .debug_info => self.out[@intCast(self.debug_offsets[1] + place.debug[1])..][0..len],
            .debug_line => self.out[@intCast(self.debug_offsets[2] + place.debug[2])..][0..len],
        };
    }
};

const Range = fanout.Range;

const Task = struct {
    context: *const Context,
    work: union(enum) {
        copies: []const fanout.Copy,
        symbols: Range,
        sites: Range,
        /// The veneers of one placed image.
        veneers: u32,
    },
    sites: u64 = 0,
    bytes: u64 = 0,

    pub fn run(opaque_task: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
        const self: *Task = @ptrCast(@alignCast(opaque_task));
        switch (self.work) {
            .copies => |list| self.bytes += fanout.runCopies(self.context.out, list),
            .symbols => |range| self.writeSymbols(range),
            .sites => |range| self.applySites(range),
            .veneers => |image| self.writeVeneers(image),
        }
        return opaque_task;
    }

    fn writeVeneers(self: *Task, image: u32) void {
        const context = self.context;
        const place = context.places[image];
        for (context.resolved[image], 0..) |target, import| {
            const address = place.veneers + import * jump_window.veneer_size;
            jump_window.writeVeneer(context.out[@intCast(context.fileOffsetOf(address))..][0..jump_window.veneer_size], address, target);
        }
        self.bytes = context.resolved[image].len * jump_window.veneer_size;
    }

    fn writeSymbols(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const symbols = view.symbols()[range.first..][0..range.count];
        const slots = context.symbol_slots[range.image][range.first..][0..range.count];
        var written: u64 = 0;
        for (symbols, slots) |symbol, slot| {
            // A symbol of code the link left as a hole has no entry.
            if (slot == no_symbol) continue;
            const entry = context.out[@intCast(context.symtab_offset + (place.first_symbol + slot) * sym_size)..][0..sym_size];
            written += sym_size;
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
                    .pdata, .xdata => unreachable, // an ELF image carries no unwind regions
                }
            ]), .little);
            std.mem.writeInt(u64, entry[8..16], place.base(region) + symbol.value, .little);
            std.mem.writeInt(u64, entry[16..24], symbol.size, .little);
        }
        self.bytes = written;
    }

    fn applySites(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const addresses = context.resolved[range.image];
        const atoms_reached = context.reached_atoms[range.image];
        for (view.sites()[range.first..][0..range.count]) |site| {
            if (site.atom != UnitImage.no_atom and !atoms_reached[site.atom]) continue;
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const site_address = place.base(region) + site.offset;
            const out = context.regionOut(place, view, region);
            const field = out[site.offset..][0..@intCast(UnitImage.kindWidth(kind))];
            const imported = site.target_region == UnitImage.import_target;
            const target = if (imported) addresses[site.target] else place.base(@fromBackingInt(site.target_region));
            var value = @as(i64, @intCast(target)) + site.addend;
            // A branch to an import beyond its reach takes the import's
            // veneer; the app region is within reach of the host and of
            // itself for every other kind, and debug sections within 32-bit
            // offsets.
            if (kind == .branch26 and imported and !jump_window.branchReaches(site_address, @intCast(value))) {
                if (site.addend != 0) base.invariant("fast dev link invariant violated: a veneered branch has an addend", .{});
                value = @intCast(place.veneers + site.target * jump_window.veneer_size);
            }
            if (!UnitImage.applySite(field, kind, value, site_address)) {
                base.invariant("fast dev link invariant violated: a patch site's value does not fit its field", .{});
            }
        }
        self.sites = range.count;
    }
};

fn writeLoad(entry: *[56]u8, offset: u64, vaddr: u64, filesz: u64, memsz: u64, flags: u32, alignment: u64) void {
    std.mem.writeInt(u32, entry[0..4], PT_LOAD, .little);
    std.mem.writeInt(u32, entry[4..8], flags, .little);
    std.mem.writeInt(u64, entry[8..16], offset, .little);
    std.mem.writeInt(u64, entry[16..24], vaddr, .little);
    std.mem.writeInt(u64, entry[24..32], vaddr, .little);
    std.mem.writeInt(u64, entry[32..40], filesz, .little);
    std.mem.writeInt(u64, entry[40..48], memsz, .little);
    std.mem.writeInt(u64, entry[48..56], alignment, .little);
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

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

pub const OutputFile = link_output.OutputFile;
