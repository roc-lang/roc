//! The patch link of a fast dev link (Linux x86_64 executables): the
//! executable is the pre-linked host's bytes (`host_prelink.zig`) followed
//! by the app region, made of the unit images (`backend.dev.UnitImage`) the
//! program reaches and the support image, placed at addresses that are a
//! pure function of the images and their keys.
//!
//! The app region is two segments: one read-and-execute segment holding
//! the `provides` windows and every image's code block (text, then its
//! read-only data) without the atoms the program does not reach, and one
//! read-write segment holding every image's data and zero-fill. Every
//! reference left in an image is a patch site the link fills; an import
//! resolves first inside the app region, then against the host's kept
//! symbols. A reference an image resolved between two of its atoms is fixed
//! again only when dropping unreached atoms moved its two ends by different
//! amounts. Debug information and symbols are appended after
//! the segments, with the host's own, under a rebuilt section header table.
//!
//! The output file is written through a shared mapping of the file, whose
//! blocks are allocated before any write so running out of room is reported
//! rather than faulted, one task per image on the compilation's post-check
//! executor. Every write lands in the
//! writing task's own region: a fixup writes into the referring site,
//! never into the referent, and every region an image owns is disjoint from
//! every other image's. The host's words, the windows and the file tail
//! belong to the coordinating thread.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const host_prelink = @import("host_prelink.zig");
const unit_walk = @import("unit_walk.zig");

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
    /// Completing and releasing the output's mapping.
    unmap_ns: u64 = 0,
    images_placed: u64 = 0,
    bytes_written: u64 = 0,
    sites_patched: u64 = 0,
    /// Resolved references between atoms re-fixed after compaction.
    refs_fixed: u64 = 0,
    /// Bytes of unreached atoms the output does without.
    hole_bytes: u64 = 0,
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
    const reached_atoms = try gpa.alloc([]const bool, images.items.len);
    defer gpa.free(reached_atoms);
    for (walk.states, reached_atoms) |state, *atoms| atoms.* = if (state) |placed| placed.atoms else &.{};

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
    defer for (places) |place| if (place.atoms.len != 0) gpa.free(place.atoms);
    var rx_size: u64 = std.mem.alignForward(u64, provide_count * host_prelink.provide_window, page_size);
    for (order.items) |index| {
        const view = images.items[index].view;
        rx_size = std.mem.alignForward(u64, rx_size, page_size);
        places[index].block = host_prelink.app_base + rx_size;
        const compact = try compactAtoms(gpa, view, reached_atoms[index]);
        places[index].atoms = compact.offsets;
        places[index].block_size = compact.size;
        timing.hole_bytes += view.block().len - compact.size;
        rx_size += compact.size;
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
    // Symbols of code the link leaves out get no entry: each placed
    // image's symbol table index maps to its entry's position among the
    // image's kept symbols, or `no_symbol`.
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
    @memset(resolved, &.{});
    defer {
        for (order.items) |index| gpa.free(resolved[index]);
        gpa.free(resolved);
    }
    for (order.items) |index| {
        const state = walk.states[index].?;
        resolved[index] = try gpa.alloc(u64, state.targets.len);
        for (state.targets, resolved[index], 0..) |target, *address, import| {
            address.* = switch (target) {
                .app => |found| app: {
                    const target_view = images.items[found.image].view;
                    const atom = target_view.symbolAtoms()[found.symbol];
                    // An import no reached atom names resolves to a symbol
                    // of an image the link may not place, or an atom it may
                    // leave out; no placed field reads it.
                    const target_atoms = reached_atoms[found.image];
                    if (target_atoms.len == 0 or (atom != UnitImage.no_atom and !target_atoms[atom])) {
                        std.debug.assert(!state.imports_used[import]);
                        break :app 0;
                    }
                    break :app places[found.image].symbolAddress(target_view, found.symbol);
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
    var copy_list: std.ArrayList(Copy) = .empty;
    defer copy_list.deinit(gpa);
    var copies: std.ArrayList(Task) = .empty;
    defer copies.deinit(gpa);
    var patches: std.ArrayList(Task) = .empty;
    defer patches.deinit(gpa);
    for (order.items) |index| {
        const view = images.items[index].view;
        const place = places[index];
        // Only reached atoms are written, each run of them to where the
        // compacted block puts it.
        const atoms_reached = reached_atoms[index];
        var atom: u32 = 0;
        while (atom < atoms_reached.len) {
            if (!atoms_reached[atom]) {
                atom += 1;
                continue;
            }
            const first_atom = atom;
            const first = view.atomRange(atom).start;
            while (atom < atoms_reached.len and atoms_reached[atom]) atom += 1;
            const end = view.atomRange(atom - 1).end;
            try addCopies(gpa, &copy_list, view.block()[first..end], context.fileOffsetOf(place.block) + place.atoms[first_atom]);
        }
        if (view.data().len != 0) try addCopies(gpa, &copy_list, view.data(), context.fileOffsetOf(place.data));
        for ([_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line }, 0..) |region, slot| {
            try addCopies(gpa, &copy_list, view.regionBytes(region), debug_offsets[slot] + place.debug[slot]);
        }
        try addCopies(gpa, &copy_list, view.strings(), strtab_offset + place.strings);
        try addRanges(gpa, &copies, &context, .symbols, index, view.symbols().len);
        try addRanges(gpa, &patches, &context, .sites, index, view.sites().len);
        try addRanges(gpa, &patches, &context, .refs, index, view.atoms().len);
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
    for (patches.items) |task| {
        timing.sites_patched += task.sites;
        timing.refs_fixed += task.refs;
    }
    timing.fan_out_ns = elapsed(std_io, fan_out_started);

    // The coordinator's writes: windows, program headers, build id, ELF
    // header, section names and headers.
    const finalize_started = now(std_io);
    for (provided, 0..) |found, index| {
        const window = host_prelink.provideAddress(index);
        const window_bytes = mapped[@intCast(app_offset + (window - host_prelink.app_base))..][0..@intCast(host_prelink.provide_window)];
        const symbol = images.items[found.image].view.symbols()[found.symbol];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        const address = places[found.image].symbolAddress(images.items[found.image].view, found.symbol);
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
    unmapped = true;
    try output.finish(mapped);
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
    /// The size of the image's compacted block: its reached atoms, in
    /// order, without the unreached ones (`compactAtoms`).
    block_size: u64 = 0,
    /// For each atom, its offset in the compacted block, or `hole`.
    atoms: []u32 = &.{},
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

    /// How far back from its image offset the reached atom `atom` sits in
    /// the compacted block.
    fn shift(self: Place, view: UnitImage.View, atom: u32) u64 {
        std.debug.assert(self.atoms[atom] != hole);
        return view.atoms()[atom] - self.atoms[atom];
    }

    /// The address of symbol `symbol_index` of the image placed here; a
    /// block symbol's atom is reached.
    fn symbolAddress(self: Place, view: UnitImage.View, symbol_index: u32) u64 {
        const symbol = view.symbols()[symbol_index];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        if (region != .block) return self.base(region) + symbol.value;
        const atom = view.symbolAtoms()[symbol_index];
        return self.block + symbol.value - self.shift(view, atom);
    }
};

/// `Place.atoms` entry of an atom the link leaves out.
const hole: u32 = std.math.maxInt(u32);

/// Lay out the reached atoms of `view`'s block without the unreached ones.
/// Each run of consecutive reached atoms keeps its bytes together and
/// lands at the first place at or past the previous run where every atom
/// in it keeps the alignment its image offset has (the strongest the
/// object could have required of it, at most the block's), so a run moves
/// back by a multiple of that alignment. Returns each atom's offset in the
/// compacted block, `hole` for an unreached one, and the compacted size.
fn compactAtoms(gpa: Allocator, view: UnitImage.View, reached: []const bool) Allocator.Error!struct { offsets: []u32, size: u64 } {
    const starts = view.atoms();
    const offsets = try gpa.alloc(u32, starts.len);
    @memset(offsets, hole);
    const block_align: u64 = view.blockAlign();
    var cursor: u64 = 0;
    var atom: usize = 0;
    while (atom < starts.len) {
        if (!reached[atom]) {
            atom += 1;
            continue;
        }
        const first = atom;
        var run_align: u64 = 1;
        while (atom < starts.len and reached[atom]) : (atom += 1) run_align = @max(run_align, atomAlign(starts[atom], block_align));
        const old_start: u64 = starts[first];
        const old_end: u64 = view.atomRange(@intCast(atom - 1)).end;
        std.debug.assert(cursor <= old_start);
        const new_start = cursor + (old_start - cursor) % run_align;
        for (first..atom) |index| offsets[index] = @intCast(new_start + (starts[index] - old_start));
        cursor = new_start + (old_end - old_start);
    }
    return .{ .offsets = offsets, .size = cursor };
}

/// The alignment an atom at block offset `start` has: the largest power of
/// two dividing its offset, at most the block's alignment.
fn atomAlign(start: u64, block_align: u64) u64 {
    if (start == 0) return block_align;
    return @min(block_align, start & (~start +% 1));
}

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
            .block => self.out[@intCast(self.fileOffsetOf(place.block))..][0..@intCast(place.block_size)],
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

fn addRanges(gpa: Allocator, tasks: *std.ArrayList(Task), context: *const Context, comptime kind: enum { symbols, sites, refs }, image: u32, count: usize) Allocator.Error!void {
    var first: u32 = 0;
    while (first < count) : (first += entry_chunk) {
        const range: Range = .{ .image = image, .first = first, .count = @min(entry_chunk, @as(u32, @intCast(count)) - first) };
        try tasks.append(gpa, .{ .context = context, .work = switch (kind) {
            .symbols => .{ .symbols = range },
            .sites => .{ .sites = range },
            .refs => .{ .refs = range },
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
        /// A range of atoms whose resolved references are fixed.
        refs: Range,
    },
    sites: u64 = 0,
    refs: u64 = 0,
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
            .refs => |range| self.applyRefs(range),
        }
        return opaque_task;
    }

    fn writeSymbols(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const symbols = view.symbols()[range.first..][0..range.count];
        const slots = context.symbol_slots[range.image][range.first..][0..range.count];
        var written: u64 = 0;
        for (symbols, slots, 0..) |symbol, slot, at| {
            // A symbol of code the link left out has no entry.
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
                }
            ]), .little);
            std.mem.writeInt(u64, entry[8..16], place.symbolAddress(view, @intCast(range.first + at)), .little);
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
            // A field in the block sits where its atom moved to.
            const offset: u64 = if (site.atom != UnitImage.no_atom) site.offset - place.shift(view, site.atom) else site.offset;
            const out = context.regionOut(place, view, region);
            const field = out[@intCast(offset)..][0..@intCast(UnitImage.kindWidth(kind))];
            const target: i64 = if (site.target_region == UnitImage.import_target)
                @intCast(addresses[site.target])
            else if (site.target_region == @backingInt(UnitImage.Region.block)) block: {
                // Only a debug field can name an atom the link left out (a
                // reached atom's edges reach every atom its fields name, and
                // writable data's are roots): it gets the DWARF tombstone.
                if (!atoms_reached[site.target]) {
                    std.debug.assert(!region.isAllocated());
                    @memset(field, 0);
                    continue;
                }
                break :block @as(i64, @intCast(place.block)) - @as(i64, @intCast(place.shift(view, site.target)));
            } else @intCast(place.base(@fromBackingInt(site.target_region)));
            const value = target + site.addend;
            // The layout keeps the app region within 32-bit reach of the
            // host and of itself, and debug sections within 32-bit offsets.
            if (!UnitImage.applySite(field, kind, value, place.base(region) + offset)) {
                base.invariant("fast dev link invariant violated: a patch site's value does not fit its field", .{});
            }
        }
        self.sites = range.count;
    }

    /// Fix the resolved references of the reached atoms in `range` whose
    /// two ends the compaction moved by different amounts. Two atoms only
    /// ever come closer, so the displacement keeps fitting its field.
    fn applyRefs(self: *Task, range: Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const atoms_reached = context.reached_atoms[range.image];
        const out = context.regionOut(place, view, .block);
        var fixed: u64 = 0;
        for (range.first..range.first + range.count) |atom_index| {
            const atom: u32 = @intCast(atom_index);
            if (!atoms_reached[atom]) continue;
            const from_shift = place.shift(view, atom);
            for (view.atomRefs(atom)) |ref| {
                std.debug.assert(atoms_reached[ref.target]); // a reached atom's edges reach every atom its references name
                const to_shift = place.shift(view, ref.target);
                if (to_shift == from_shift) continue;
                const field = out[@intCast(ref.offset - from_shift)..][0..4];
                const displacement = std.mem.readInt(i32, field, .little);
                const moved = @as(i64, displacement) + @as(i64, @intCast(from_shift)) - @as(i64, @intCast(to_shift));
                std.mem.writeInt(i32, field, @intCast(moved), .little);
                fixed += 1;
            }
        }
        self.refs = fixed;
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
    /// How far from the start `copyFrom` and `writeStart` have written.
    written: u64 = 0,
    /// Whether `map` gave out a buffer instead of a mapping of the file,
    /// which `finish` writes back.
    buffered: bool = false,

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

    /// Copy `bytes`, which are `source`'s contents at `offset`, to the start
    /// of the output. Within one filesystem the kernel copies the file
    /// range itself (a reflink where the filesystem shares blocks); across
    /// filesystems, or on one without the operation, the bytes are written
    /// from `bytes`. The output holds the same bytes either way.
    pub fn copyFrom(self: *OutputFile, source: linux.fd_t, offset: u64, bytes: []const u8) Error!void {
        var in_offset: i64 = @intCast(offset);
        var out_offset: i64 = 0;
        var remaining = bytes.len;
        while (remaining != 0) {
            const rc = linux.copy_file_range(source, &in_offset, self.fd, &out_offset, @min(remaining, 1 << 30), 0);
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno == .XDEV or errno == .OPNOTSUPP or errno == .INVAL) {
                const done: usize = @intCast(out_offset);
                return self.writeAt(bytes[done..], done);
            }
            if (errno != .SUCCESS) return failure(errno);
            if (rc == 0) return error.Unexpected;
            remaining -= rc;
        }
        self.written = @max(self.written, bytes.len);
    }

    /// Write `bytes` at the start of the output.
    pub fn writeStart(self: *OutputFile, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    fn writeAt(self: *OutputFile, bytes: []const u8, at: usize) Error!void {
        var written: usize = 0;
        while (written < bytes.len) {
            const rc = linux.pwrite(self.fd, bytes.ptr + written, bytes.len - written, @intCast(at + written));
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno != .SUCCESS) return failure(errno);
            written += rc;
        }
        self.written = @max(self.written, at + bytes.len);
    }

    /// Set the output's size; bytes it grows by read as zero.
    pub fn resize(self: *OutputFile, size: u64) Error!void {
        const rc = linux.ftruncate(self.fd, @intCast(size));
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
    }

    /// Map the output's first `size` bytes for writing, with the blocks
    /// behind them allocated first, so a filesystem that runs out of room
    /// fails here instead of faulting a write into the mapping. On a
    /// filesystem without block allocation the bytes go to a buffer that
    /// `finish` writes out, which reports the same failure.
    pub fn map(self: *OutputFile, size: u64) Error![]u8 {
        const allocated = linux.fallocate(self.fd, 0, 0, @intCast(size));
        const allocated_errno = linux.errno(allocated);
        if (allocated_errno != .SUCCESS and allocated_errno != .OPNOTSUPP) return failure(allocated_errno);
        self.buffered = allocated_errno == .OPNOTSUPP;
        const rc = if (self.buffered)
            linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0)
        else
            linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0);
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        const start: [*]u8 = @ptrFromInt(rc);
        const bytes = start[0..@intCast(size)];
        if (self.buffered) {
            // The bytes already in the file belong in the buffer too.
            var got: usize = 0;
            while (got < self.written) {
                const read = linux.pread(self.fd, start + got, @intCast(self.written - got), @intCast(got));
                const read_errno = linux.errno(read);
                if (read_errno == .INTR) continue;
                if (read_errno != .SUCCESS) return failure(read_errno);
                if (read == 0) return error.Unexpected;
                got += read;
            }
        }
        return bytes;
    }

    /// Complete the bytes of a mapping `map` made and release it.
    pub fn finish(self: *OutputFile, bytes: []u8) Error!void {
        defer self.unmap(bytes);
        if (self.buffered) try self.writeAt(bytes, 0);
    }

    /// Release a mapping `map` made without completing the output.
    pub fn unmap(_: *OutputFile, bytes: []u8) void {
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

/// A one-object image whose 16-byte-aligned block holds four 8-byte
/// functions at 0, 8, 16 and 24, each its own atom.
fn fourAtomImage(allocator: Allocator) UnitImage.BuildError![]align(UnitImage.alignment.toByteUnits()) u8 {
    const shf_alloc: u64 = 0x2;
    const shf_execinstr: u64 = 0x4;
    const text: [32]u8 = @splat(0x90);
    const object = try UnitImage.TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = shf_alloc | shf_execinstr, .bytes = &text, .addralign = 16 },
    }, &.{
        .{ .name = "a", .section = 1, .value = 0, .size = 8, .global = true, .func = true },
        .{ .name = "b", .section = 1, .value = 8, .size = 8, .func = true },
        .{ .name = "c", .section = 1, .value = 16, .size = 8, .func = true },
        .{ .name = "d", .section = 1, .value = 24, .size = 8, .func = true },
    }, &.{});
    defer allocator.free(object);
    return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .{ .symbols = .{ .edges = &.{}, .data_starts = &.{} } } });
}

test "compaction packs runs of reached atoms and keeps each run's alignment" {
    const allocator = std.testing.allocator;
    const image = try fourAtomImage(allocator);
    defer allocator.free(image);
    const view = try UnitImage.View.init(image);
    try std.testing.expectEqual(@as(u32, 16), view.blockAlign());
    // c is 16-aligned, so the run c,d cannot move back by the 8 bytes b
    // leaves; the block keeps its size.
    const held = try compactAtoms(allocator, view, &.{ true, false, true, true });
    defer allocator.free(held.offsets);
    try std.testing.expectEqualSlices(u32, &.{ 0, hole, 16, 24 }, held.offsets);
    try std.testing.expectEqual(@as(u64, 32), held.size);
    // A run whose atoms are all 8-aligned moves back by 8.
    const moved = try compactAtoms(allocator, view, &.{ false, true, false, true });
    defer allocator.free(moved.offsets);
    try std.testing.expectEqualSlices(u32, &.{ hole, 0, hole, 8 }, moved.offsets);
    try std.testing.expectEqual(@as(u64, 16), moved.size);
    // The 16-aligned atom inside a run pins the whole run.
    const pinned = try compactAtoms(allocator, view, &.{ false, true, true, false });
    defer allocator.free(pinned.offsets);
    try std.testing.expectEqualSlices(u32, &.{ hole, 8, 16, hole }, pinned.offsets);
    try std.testing.expectEqual(@as(u64, 24), pinned.size);
    // A run moves back by a multiple of its alignment.
    const by_sixteen = try compactAtoms(allocator, view, &.{ false, false, true, true });
    defer allocator.free(by_sixteen.offsets);
    try std.testing.expectEqualSlices(u32, &.{ hole, hole, 0, 8 }, by_sixteen.offsets);
    try std.testing.expectEqual(@as(u64, 16), by_sixteen.size);
}
