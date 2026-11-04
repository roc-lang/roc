//! The patch link of a fast dev link of a macOS arm64 executable: the
//! executable is the pre-linked host's mapped segments (`HostPrelink.zig`)
//! followed by the app region, made of the unit images
//! (`backend.dev.UnitImage`) the program reaches and the support image,
//! then the debug information, then a rebuilt `__LINKEDIT`.
//!
//! The app region is two segments: `__ROC_TEXT`, read-and-execute, holding
//! every placed image's code block (text, then its read-only data), and
//! `__ROC_DATA`, read-and-write, holding every image's data and zero-fill.
//! The executable is position independent, so every absolute word the app
//! holds gets a rebase entry, and one holding the address of a symbol a
//! dylib defines gets a bind entry, both appended to the host's dyld
//! opcodes. A `provides` symbol's window in the host is filled too: a long
//! jump to the entry wrapper, or the value's bytes with their rebases.
//! Symbols are inserted among the host's local symbols, debug information
//! goes in a `__DWARF` segment the loader never maps, and the whole file
//! before its signature is signed ad hoc, reusing the host's page hashes
//! for every page the link leaves as it was.
//!
//! As on Linux, the output is written through a shared mapping in bounded
//! tasks on the compilation's post-check executor, every write landing in
//! the writing task's own region, with the host's words, the windows, the
//! load commands and the file tail belonging to the coordinating thread.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const macho = std.macho;
const HostPrelink = @import("HostPrelink.zig");
const unit_walk = @import("../unit_walk.zig");
const fanout = @import("../patch_fanout.zig");
const link_output = @import("../link_output.zig");
const patch_link = @import("../patch_link.zig");

const Allocator = std.mem.Allocator;
const UnitImage = backend.dev.UnitImage;
const Executor = base.post_check_task_executor.Executor;

const page_size: u64 = HostPrelink.page_size;
const block_align: u64 = UnitImage.page_size;
const nlist_size = 16;
const segment_command_size = 72;
const section_header_size = 80;

/// One image to place.
pub const ImageInput = unit_walk.ImageInput;
pub const Timing = patch_link.Timing;
pub const LinkError = patch_link.LinkError;
pub const Outcome = patch_link.Outcome;
pub const OutputFile = link_output.OutputFile;

/// Everything a patch link reads.
pub const Inputs = struct {
    host: HostPrelink.View,
    /// The builtins and the Boxy runtime.
    support: ImageInput,
    /// Every unit of the program; `root` indexes the root unit.
    units: []const ImageInput,
    root: usize,
    /// Symbols the host must provide when the app imports them. Sorted.
    host_required: []const []const u8,
};

/// Link `inputs` into `output`, whose first `host.segments().len` bytes
/// already hold the host's mapped segments.
pub fn link(gpa: Allocator, arena: Allocator, std_io: std.Io, inputs: Inputs, output: *OutputFile, executor: ?Executor, timing: *Timing, outcome: *Outcome) LinkError!void {
    const layout_started = now(std_io);
    var images: std.ArrayList(ImageInput) = .empty;
    defer images.deinit(gpa);
    try images.append(gpa, inputs.support);
    try images.appendSlice(gpa, inputs.units);
    const root_image = inputs.root + 1;

    var exports = try unit_walk.collectExports(gpa, images.items);
    defer exports.deinit(gpa);

    // What the program reaches from the `provides` symbols.
    const host = inputs.host;
    var walk: unit_walk.Walk = .{ .gpa = gpa, .images = images.items, .resolver = .{ .exports = &exports, .host = .{ .context = &host, .find = findHostSymbol }, .host_required = inputs.host_required } };
    defer walk.deinit();
    try walk.init();
    for (host.provides()) |provide| {
        const found = exports.get(host.name(provide.name)) orelse base.invariant("fast dev link invariant violated: no unit image defines the provided symbol {s}", .{host.name(provide.name)});
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

    // The host's load commands, which the record admitted.
    var parsed: HostPrelink.Executable.Parsed = undefined;
    const exe = parsed.parse(host.host()) catch unreachable; // the host record was admitted only after these load commands parsed
    const host_text = exe.segments[exe.text];
    const symtab = exe.symtab.?;
    const dysymtab = exe.dysymtab.?;
    const dyld_info = exe.dyld_info.?;

    // Layout: the app's text after the host's segments, its data after
    // that, the debug segment, then __LINKEDIT.
    const places = try gpa.alloc(Place, images.items.len);
    defer gpa.free(places);
    @memset(places, .{});
    const text_fileoff = host.trailer.linkedit_offset;
    const text_vmaddr = std.mem.alignForward(u64, host.trailer.host_end_vaddr, page_size);
    var rx_size: u64 = 0;
    for (order.items) |index| {
        const view = images.items[index].view;
        rx_size = std.mem.alignForward(u64, rx_size, block_align);
        places[index].block = text_vmaddr + rx_size;
        rx_size += view.block().len;
    }
    const text_segment_size = std.mem.alignForward(u64, rx_size, page_size);
    const data_fileoff = text_fileoff + text_segment_size;
    const data_vmaddr = text_vmaddr + text_segment_size;
    var rw_file_size: u64 = 0;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.data().len == 0) continue;
        rw_file_size = std.mem.alignForward(u64, rw_file_size, block_align);
        places[index].data = data_vmaddr + rw_file_size;
        rw_file_size += view.data().len;
    }
    var rw_mem_size = rw_file_size;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.bssSize() == 0) continue;
        rw_mem_size = std.mem.alignForward(u64, rw_mem_size, @max(view.bssAlign(), 16));
        places[index].bss = data_vmaddr + rw_mem_size;
        rw_mem_size += view.bssSize();
    }
    const data_filesize = std.mem.alignForward(u64, rw_file_size, page_size);
    const data_vmsize = std.mem.alignForward(u64, rw_mem_size, page_size);
    // Page-relative references reach 4 GiB.
    if (data_vmaddr + data_vmsize - host_text.vmaddr > (@as(u64, 1) << 32)) return error.ProgramTooLarge;

    // The debug segment: abbrev, info and line, each the images' fragments
    // in placement order.
    const dwarf_fileoff = data_fileoff + data_filesize;
    const debug_regions = [_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line };
    var debug_offsets: [3]u64 = undefined;
    var debug_sizes: [3]u64 = undefined;
    var dwarf_at = dwarf_fileoff;
    for (debug_regions, 0..) |region, slot| {
        debug_offsets[slot] = dwarf_at;
        var size: u64 = 0;
        for (order.items) |index| {
            const fragment = images.items[index].view.regionBytes(region);
            places[index].debug[slot] = size;
            size += fragment.len;
        }
        debug_sizes[slot] = size;
        dwarf_at = std.mem.alignForward(u64, dwarf_at + size, 8);
    }
    const dwarf_size = dwarf_at - dwarf_fileoff;

    // Symbols of code the link leaves as a hole get no entry.
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
    var app_strings: u64 = 0;
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
        places[index].first_symbol = dysymtab.nlocalsym + app_symbol_count;
        app_symbol_count += kept;
        places[index].strings = symtab.strsize + app_strings;
        app_strings += view.strings().len;
    }

    // Import addresses and dylib bindings, now that every image has its
    // place.
    const resolved = try gpa.alloc([]u64, images.items.len);
    @memset(resolved, &.{});
    const bindings = try gpa.alloc([]?HostPrelink.HostSymbol, images.items.len);
    @memset(bindings, &.{});
    defer {
        for (order.items) |index| {
            gpa.free(resolved[index]);
            gpa.free(bindings[index]);
        }
        gpa.free(resolved);
        gpa.free(bindings);
    }
    for (order.items) |index| {
        const view = images.items[index].view;
        const targets = walk.states[index].?.targets;
        resolved[index] = try gpa.alloc(u64, targets.len);
        bindings[index] = try gpa.alloc(?HostPrelink.HostSymbol, targets.len);
        for (targets, resolved[index], bindings[index], view.imports()) |target, *address, *binding, import| {
            binding.* = null;
            address.* = switch (target) {
                .app => |found| app: {
                    const symbol = images.items[found.image].view.symbols()[found.symbol];
                    break :app places[found.image].base(@fromBackingInt(symbol.region)) + symbol.value;
                },
                .host => |vaddr| host: {
                    const symbol = host.findSymbol(view.name(import.name)).?; // the walk found it there
                    if (symbol.ordinal != 0) binding.* = symbol;
                    break :host vaddr;
                },
                .missing => unreachable, // a missing host symbol failed the link above
            };
        }
    }

    // Every absolute word the app holds: a rebase, or a bind for one that
    // holds a dylib symbol's address. The host's data windows get theirs
    // from the value they copy.
    var rebases: std.ArrayList(Fixup) = .empty;
    defer rebases.deinit(gpa);
    var binds: std.ArrayList(Bind) = .empty;
    defer binds.deinit(gpa);
    const data_segment_index: u8 = @intCast(exe.linkedit + 1);
    for (order.items) |index| {
        const view = images.items[index].view;
        const place = places[index];
        for (view.sites()) |site| {
            if (@as(UnitImage.SiteKind, @fromBackingInt(site.kind)) != .abs64) continue;
            if (site.atom != UnitImage.no_atom and !reached_atoms[index][site.atom]) continue;
            const region: UnitImage.Region = @fromBackingInt(site.region);
            switch (region) {
                .data => {},
                // Debug words are not loaded.
                .block => unreachable, // the image builder admits no absolute words in code, which cannot be rebased
                .bss, .debug_line, .debug_info, .debug_abbrev => continue,
            }
            const fixup: Fixup = .{ .segment = data_segment_index, .offset = place.data - data_vmaddr + site.offset };
            if (site.target_region == UnitImage.import_target) {
                if (bindings[index][site.target]) |symbol| {
                    try binds.append(gpa, .{ .fixup = fixup, .symbol = symbol, .name = host.name(symbol.name), .addend = site.addend });
                    continue;
                }
            }
            try rebases.append(gpa, fixup);
        }
    }
    // The provides: their windows' contents and fixups.
    const host_data_segment: usize = for (exe.sections) |section| {
        if (std.mem.eql(u8, section.segname, "__DATA") and std.mem.eql(u8, section.sectname, "__roc_prov")) break section.segment;
    } else 0;
    const provided = try gpa.alloc(SymbolRef, host.provides().len);
    defer gpa.free(provided);
    for (host.provides(), provided) |provide, *slot| {
        const name = host.name(provide.name);
        slot.* = exports.get(name) orelse unreachable; // the walk above resolved every provided symbol
        const view = images.items[slot.image].view;
        const symbol = view.symbols()[slot.symbol];
        if (@as(HostPrelink.ProvideKind, @fromBackingInt(@intCast(provide.kind))) == .procedure) continue;
        // A value's window holds the value's bytes, so the words in it are
        // rebased and bound as the words in the value are.
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        if (region != .data) continue;
        const segment = exe.segments[host_data_segment];
        const window_offset = provide.vaddr - segment.vmaddr;
        for (view.sites()) |site| {
            if (@as(UnitImage.SiteKind, @fromBackingInt(site.kind)) != .abs64) continue;
            if (@as(UnitImage.Region, @fromBackingInt(site.region)) != .data) continue;
            if (site.offset < symbol.value or site.offset + 8 > symbol.value + @min(symbol.size, provide.size)) continue;
            const fixup: Fixup = .{ .segment = @intCast(host_data_segment), .offset = window_offset + (site.offset - symbol.value) };
            if (site.target_region == UnitImage.import_target) {
                if (bindings[slot.image][site.target]) |host_symbol| {
                    try binds.append(gpa, .{ .fixup = fixup, .symbol = host_symbol, .name = host.name(host_symbol.name), .addend = site.addend });
                    continue;
                }
            }
            try rebases.append(gpa, fixup);
        }
    }
    std.mem.sort(Fixup, rebases.items, {}, Fixup.lessThan);
    std.mem.sort(Bind, binds.items, {}, Bind.lessThan);
    var rebase_opcodes: std.ArrayList(u8) = .empty;
    defer rebase_opcodes.deinit(gpa);
    try encodeRebases(gpa, &rebase_opcodes, rebases.items);
    var bind_opcodes: std.ArrayList(u8) = .empty;
    defer bind_opcodes.deinit(gpa);
    try encodeBinds(gpa, &bind_opcodes, binds.items);

    // __LINKEDIT: the host's pieces with the app's appended, in the
    // host's order.
    const linkedit_fileoff = std.mem.alignForward(u64, dwarf_fileoff + dwarf_size, page_size);
    const linkedit_vmaddr = data_vmaddr + data_vmsize;
    const host_rebase = hostPiece(host, dyld_info.rebase_off, dyld_info.rebase_size);
    const host_bind = hostPiece(host, dyld_info.bind_off, dyld_info.bind_size);
    const rebase_prefix = withoutDone(host_rebase, .rebase);
    const bind_prefix = if (bind_opcodes.items.len == 0) host_bind else withoutDone(host_bind, .bind);
    var tail = Tail{ .at = linkedit_fileoff };
    const rebase_offset = tail.piece(rebase_prefix.len + rebase_opcodes.items.len + 1);
    const rebase_size = tail.at - rebase_offset;
    const bind_offset = tail.piece(bind_prefix.len + bind_opcodes.items.len + @as(u64, @intFromBool(bind_opcodes.items.len != 0)));
    const bind_size = tail.at - bind_offset;
    const weak_bind_offset = tail.piece(dyld_info.weak_bind_size);
    const lazy_bind_offset = tail.piece(dyld_info.lazy_bind_size);
    const export_offset = tail.piece(dyld_info.export_size);
    const function_starts_offset = tail.piece(if (exe.function_starts) |data| data.datasize else 0);
    const data_in_code_offset = tail.piece(if (exe.data_in_code) |data| data.datasize else 0);
    const symtab_count = symtab.nsyms + app_symbol_count;
    const symtab_offset = tail.piece(symtab_count * nlist_size);
    const indirect_offset = tail.piece(@as(u64, dysymtab.nindirectsyms) * 4);
    const strtab_size = symtab.strsize + app_strings;
    const strtab_offset = tail.piece(strtab_size);
    tail.at = std.mem.alignForward(u64, tail.at, 16);
    const signature_offset = tail.at;
    const page_count = std.math.divCeil(u64, signature_offset, page_size) catch unreachable; // page_size is nonzero
    const signature_size = signature_header_size + page_count * 32;
    const file_size = signature_offset + signature_size;
    const linkedit_filesize = file_size - linkedit_fileoff;
    if (file_size > std.math.maxInt(u32)) return error.ProgramTooLarge;

    try output.resize(file_size);
    const mapped = try output.map(file_size);
    var unmapped = false;
    defer if (!unmapped) output.unmap(mapped);
    timing.layout_ns = elapsed(std_io, layout_started);

    // Fan-out, in three waves of bounded tasks: every byte copy and every
    // symbol entry, then every patch site, which writes into bytes the
    // first wave copied, then the page hashes of the signature.
    const fan_out_started = now(std_io);
    const section_ordinal_base: u8 = @intCast(exe.sections.len);
    const context: Context = .{
        .out = mapped,
        .images = images.items,
        .places = places,
        .resolved = resolved,
        .bindings = bindings,
        .text_fileoff = text_fileoff,
        .text_vmaddr = text_vmaddr,
        .data_fileoff = data_fileoff,
        .data_vmaddr = data_vmaddr,
        .debug_offsets = debug_offsets,
        .symtab_offset = symtab_offset,
        .section_ordinals = .{ section_ordinal_base + 1, section_ordinal_base + 2, section_ordinal_base + 3 },
        .reached_atoms = reached_atoms,
        .symbol_slots = symbol_slots,
        .signature_offset = signature_offset,
        .hashes_offset = signature_offset + signature_header_size,
        .host_page_hashes = host.pageHashes(),
        .dirty_pages_end = exe.first_content,
        .dirty_ranges = &.{},
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
        for (debug_regions, 0..) |region, slot| {
            try fanout.addCopies(gpa, &copy_list, view.regionBytes(region), debug_offsets[slot] + place.debug[slot]);
        }
        try fanout.addCopies(gpa, &copy_list, view.strings(), strtab_offset + place.strings);
        try fanout.addRanges(gpa, &symbol_ranges, index, view.symbols().len);
        try fanout.addRanges(gpa, &site_ranges, index, view.sites().len);
    }
    // The host's __LINKEDIT pieces: the dyld opcode prefixes, the tables
    // kept whole, and the symbols around the app's.
    try fanout.addCopies(gpa, &copy_list, rebase_prefix, rebase_offset);
    try fanout.addCopies(gpa, &copy_list, rebase_opcodes.items, rebase_offset + rebase_prefix.len);
    try fanout.addCopies(gpa, &copy_list, bind_prefix, bind_offset);
    try fanout.addCopies(gpa, &copy_list, bind_opcodes.items, bind_offset + bind_prefix.len);
    try fanout.addCopies(gpa, &copy_list, hostPiece(host, dyld_info.weak_bind_off, dyld_info.weak_bind_size), weak_bind_offset);
    try fanout.addCopies(gpa, &copy_list, hostPiece(host, dyld_info.lazy_bind_off, dyld_info.lazy_bind_size), lazy_bind_offset);
    try fanout.addCopies(gpa, &copy_list, hostPiece(host, dyld_info.export_off, dyld_info.export_size), export_offset);
    if (exe.function_starts) |data| try fanout.addCopies(gpa, &copy_list, hostPiece(host, data.dataoff, data.datasize), function_starts_offset);
    if (exe.data_in_code) |data| try fanout.addCopies(gpa, &copy_list, hostPiece(host, data.dataoff, data.datasize), data_in_code_offset);
    const host_nlists = hostPiece(host, symtab.symoff, symtab.nsyms * nlist_size);
    const local_bytes = @as(u64, dysymtab.nlocalsym) * nlist_size;
    try fanout.addCopies(gpa, &copy_list, host_nlists[0..@intCast(local_bytes)], symtab_offset);
    try fanout.addCopies(gpa, &copy_list, host_nlists[@intCast(local_bytes)..], symtab_offset + local_bytes + app_symbol_count * nlist_size);
    try fanout.addCopies(gpa, &copy_list, hostPiece(host, dysymtab.indirectsymoff, @as(u64, dysymtab.nindirectsyms) * 4), indirect_offset);
    try fanout.addCopies(gpa, &copy_list, hostPiece(host, symtab.stroff, symtab.strsize), strtab_offset);
    const copy_batches = try fanout.batches(gpa, copy_list.items);
    defer gpa.free(copy_batches);
    var copies: std.ArrayList(Task) = .empty;
    defer copies.deinit(gpa);
    for (copy_batches) |batch| try copies.append(gpa, .{ .context = &context, .work = .{ .copies = batch } });
    for (symbol_ranges.items) |range| try copies.append(gpa, .{ .context = &context, .work = .{ .symbols = range } });
    var patches: std.ArrayList(Task) = .empty;
    defer patches.deinit(gpa);
    for (site_ranges.items) |range| try patches.append(gpa, .{ .context = &context, .work = .{ .sites = range } });
    try fanout.runTasks(gpa, executor, Task, copies.items);
    try fanout.runTasks(gpa, executor, Task, patches.items);
    for (copies.items) |task| timing.bytes_written += task.bytes;
    for (patches.items) |task| timing.sites_patched += task.sites;
    timing.fan_out_ns = elapsed(std_io, fan_out_started);

    // The coordinator's writes: the windows, the opcode terminators, the
    // indirect symbols, the load commands.
    const finalize_started = now(std_io);
    var dirty_ranges: std.ArrayList([2]u64) = .empty;
    defer dirty_ranges.deinit(gpa);
    for (host.provides(), provided) |provide, found| {
        const window = mapped[@intCast(provide.offset)..][0..@intCast(provide.size)];
        try dirty_ranges.append(gpa, .{ provide.offset, provide.offset + provide.size });
        const view = images.items[found.image].view;
        const symbol = view.symbols()[found.symbol];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        const address = places[found.image].base(region) + symbol.value;
        switch (@as(HostPrelink.ProvideKind, @fromBackingInt(@intCast(provide.kind)))) {
            .procedure => writeLongJump(window[0..16], provide.vaddr, address),
            .data => switch (region) {
                .block, .data => {
                    const size: usize = @intCast(@min(symbol.size, provide.size));
                    @memcpy(window[0..size], mapped[@intCast(context.fileOffsetOf(address))..][0..size]);
                },
                // Zero-fill: the window is already zero.
                .bss => {},
                .debug_line, .debug_info, .debug_abbrev => unreachable, // image symbols live only in allocated regions
            },
        }
    }
    mapped[@intCast(rebase_offset + rebase_size - 1)] = macho.REBASE_OPCODE_DONE;
    if (bind_opcodes.items.len != 0) mapped[@intCast(bind_offset + bind_size - 1)] = macho.BIND_OPCODE_DONE;
    // The app's symbols sit after the host's locals, so every index past
    // them moves.
    for (0..dysymtab.nindirectsyms) |index| {
        const entry = mapped[@intCast(indirect_offset + index * 4)..][0..4];
        const value = std.mem.readInt(u32, entry, .little);
        if (value & (macho.INDIRECT_SYMBOL_LOCAL | macho.INDIRECT_SYMBOL_ABS) != 0) continue;
        std.mem.writeInt(u32, entry, value + @as(u32, @intCast(app_symbol_count)), .little);
    }
    rewriteLoadCommands(mapped, exe, .{
        .text_vmaddr = text_vmaddr,
        .text_fileoff = text_fileoff,
        .text_size = text_segment_size,
        .text_content = rx_size,
        .data_vmaddr = data_vmaddr,
        .data_fileoff = data_fileoff,
        .data_filesize = data_filesize,
        .data_vmsize = data_vmsize,
        .data_content = rw_file_size,
        .bss_size = rw_mem_size - rw_file_size,
        .dwarf_fileoff = dwarf_fileoff,
        .dwarf_size = dwarf_size,
        .debug_offsets = debug_offsets,
        .debug_sizes = debug_sizes,
        .linkedit_vmaddr = linkedit_vmaddr,
        .linkedit_fileoff = linkedit_fileoff,
        .linkedit_filesize = linkedit_filesize,
        .rebase_offset = rebase_offset,
        .rebase_size = rebase_size,
        .bind_offset = bind_offset,
        .bind_size = bind_size,
        .weak_bind_offset = weak_bind_offset,
        .lazy_bind_offset = lazy_bind_offset,
        .export_offset = export_offset,
        .function_starts_offset = function_starts_offset,
        .data_in_code_offset = data_in_code_offset,
        .symtab_offset = symtab_offset,
        .symtab_count = symtab_count,
        .app_symbol_count = app_symbol_count,
        .indirect_offset = indirect_offset,
        .strtab_offset = strtab_offset,
        .strtab_size = strtab_size,
        .signature_offset = signature_offset,
        .signature_size = signature_size,
    });

    // The signature: the host's page hashes where the page is as it was,
    // fresh hashes elsewhere, under one code directory.
    var hash_context = context;
    hash_context.dirty_ranges = dirty_ranges.items;
    var hash_ranges: std.ArrayList(fanout.Range) = .empty;
    defer hash_ranges.deinit(gpa);
    try fanout.addRanges(gpa, &hash_ranges, 0, page_count);
    var hashes: std.ArrayList(Task) = .empty;
    defer hashes.deinit(gpa);
    for (hash_ranges.items) |range| try hashes.append(gpa, .{ .context = &hash_context, .work = .{ .hashes = range } });
    try fanout.runTasks(gpa, executor, Task, hashes.items);
    writeSignatureHeader(mapped[@intCast(signature_offset)..][0..signature_header_size], @intCast(signature_offset), page_count, host_text.fileoff, host_text.filesize);
    timing.finalize_ns = elapsed(std_io, finalize_started);
    const unmap_started = now(std_io);
    output.unmap(mapped);
    unmapped = true;
    timing.unmap_ns = elapsed(std_io, unmap_started);
}

const SymbolRef = unit_walk.SymbolRef;

fn findHostSymbol(context: *const anyopaque, name: []const u8) ?u64 {
    const host: *const HostPrelink.View = @ptrCast(@alignCast(context));
    const symbol = host.findSymbol(name) orelse return null;
    return symbol.vaddr;
}

/// `Context.symbol_slots` entry of a symbol the output's symbol table omits.
const no_symbol: u32 = std.math.maxInt(u32);

const Place = struct {
    block: u64 = 0,
    data: u64 = 0,
    bss: u64 = 0,
    /// Offsets of the image's fragments in the output debug sections:
    /// abbrev, info, line.
    debug: [3]u64 = .{ 0, 0, 0 },
    /// The index of the image's first symbol in the output symbol table.
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

/// One absolute word to rebase: a segment index and an offset in it.
const Fixup = struct {
    segment: u8,
    offset: u64,

    fn lessThan(_: void, a: Fixup, b: Fixup) bool {
        if (a.segment != b.segment) return a.segment < b.segment;
        return a.offset < b.offset;
    }
};

/// One absolute word dyld fills with a dylib symbol's address.
const Bind = struct {
    fixup: Fixup,
    symbol: HostPrelink.HostSymbol,
    name: []const u8,
    addend: i64,

    fn lessThan(_: void, a: Bind, b: Bind) bool {
        return Fixup.lessThan({}, a.fixup, b.fixup);
    }
};

fn encodeRebases(gpa: Allocator, out: *std.ArrayList(u8), fixups: []const Fixup) Allocator.Error!void {
    if (fixups.len == 0) return;
    try out.append(gpa, macho.REBASE_OPCODE_SET_TYPE_IMM | macho.REBASE_TYPE_POINTER);
    var current_segment: ?u8 = null;
    var current_offset: u64 = 0;
    for (fixups) |fixup| {
        if (current_segment == null or current_segment.? != fixup.segment or fixup.offset < current_offset) {
            try out.append(gpa, macho.REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB | fixup.segment);
            try appendUleb(gpa, out, fixup.offset);
            current_segment = fixup.segment;
        } else if (fixup.offset != current_offset) {
            try out.append(gpa, macho.REBASE_OPCODE_ADD_ADDR_ULEB);
            try appendUleb(gpa, out, fixup.offset - current_offset);
        }
        try out.append(gpa, macho.REBASE_OPCODE_DO_REBASE_IMM_TIMES | 1);
        current_offset = fixup.offset + 8;
    }
}

fn encodeBinds(gpa: Allocator, out: *std.ArrayList(u8), binds: []const Bind) Allocator.Error!void {
    if (binds.len == 0) return;
    try out.append(gpa, macho.BIND_OPCODE_SET_TYPE_IMM | macho.BIND_TYPE_POINTER);
    for (binds) |bind| {
        if (bind.symbol.ordinal < 16) {
            try out.append(gpa, macho.BIND_OPCODE_SET_DYLIB_ORDINAL_IMM | @as(u8, @intCast(bind.symbol.ordinal)));
        } else {
            try out.append(gpa, macho.BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB);
            try appendUleb(gpa, out, bind.symbol.ordinal);
        }
        const weak: u8 = if (bind.symbol.flags & HostPrelink.symbol_weak_import != 0) macho.BIND_SYMBOL_FLAGS_WEAK_IMPORT else 0;
        try out.append(gpa, macho.BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM | weak);
        try out.append(gpa, '_');
        try out.appendSlice(gpa, bind.name);
        try out.append(gpa, 0);
        try out.append(gpa, macho.BIND_OPCODE_SET_ADDEND_SLEB);
        try appendSleb(gpa, out, bind.addend);
        try out.append(gpa, macho.BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB | bind.fixup.segment);
        try appendUleb(gpa, out, bind.fixup.offset);
        try out.append(gpa, macho.BIND_OPCODE_DO_BIND);
    }
}

fn appendUleb(gpa: Allocator, out: *std.ArrayList(u8), value: u64) Allocator.Error!void {
    var remaining = value;
    while (true) {
        const byte: u8 = @intCast(remaining & 0x7f);
        remaining >>= 7;
        if (remaining == 0) return out.append(gpa, byte);
        try out.append(gpa, byte | 0x80);
    }
}

fn appendSleb(gpa: Allocator, out: *std.ArrayList(u8), value: i64) Allocator.Error!void {
    var remaining = value;
    while (true) {
        const byte: u8 = @intCast(remaining & 0x7f);
        remaining >>= 7;
        const done = (remaining == 0 and byte & 0x40 == 0) or (remaining == -1 and byte & 0x40 != 0);
        if (done) return out.append(gpa, byte);
        try out.append(gpa, byte | 0x80);
    }
}

fn hostPiece(host: HostPrelink.View, offset: u32, size: u64) []const u8 {
    return host.host()[offset..][0..@intCast(size)];
}

/// Which dyld opcode stream a prefix is taken from.
const OpcodeStream = enum { rebase, bind };

/// A dyld opcode stream up to its terminating DONE, which the linker pads
/// with zero bytes to its alignment: the stream is walked opcode by
/// opcode, since a ULEB argument may be a zero byte too.
fn withoutDone(stream: []const u8, kind: OpcodeStream) []const u8 {
    var at: usize = 0;
    while (at < stream.len) {
        const opcode = stream[at] & 0xf0;
        if (opcode == 0) return stream[0..at];
        at += 1;
        const ulebs: usize = switch (kind) {
            .rebase => if (opcode == macho.REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB or opcode == macho.REBASE_OPCODE_ADD_ADDR_ULEB or opcode == macho.REBASE_OPCODE_DO_REBASE_ULEB_TIMES or opcode == macho.REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB)
                1
            else if (opcode == macho.REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB)
                2
            else
                0,
            .bind => if (opcode == macho.BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB or opcode == macho.BIND_OPCODE_SET_ADDEND_SLEB or opcode == macho.BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB or opcode == macho.BIND_OPCODE_ADD_ADDR_ULEB or opcode == macho.BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB)
                1
            else if (opcode == macho.BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB)
                2
            else if (opcode == macho.BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM) name: {
                while (at < stream.len and stream[at] != 0) at += 1;
                at += 1;
                break :name 0;
            } else 0,
        };
        for (0..ulebs) |_| {
            while (at < stream.len and stream[at] & 0x80 != 0) at += 1;
            at += 1;
        }
    }
    return stream;
}

/// Pieces laid out one after another, 8-aligned.
const Tail = struct {
    at: u64,

    fn piece(self: *Tail, size: u64) u64 {
        self.at = std.mem.alignForward(u64, self.at, 8);
        const start = self.at;
        self.at += size;
        return start;
    }
};

/// `ADRP x16, target; ADD x16, x16, target; BR x16; BRK #0`.
fn writeLongJump(window: *[16]u8, window_address: u64, target: u64) void {
    std.mem.writeInt(u32, window[0..4], 0x9000_0010, .little);
    std.mem.writeInt(u32, window[4..8], 0x9100_0210, .little);
    std.mem.writeInt(u32, window[8..12], 0xd61f_0200, .little);
    std.mem.writeInt(u32, window[12..16], 0xd420_0000, .little);
    // The app region is within 4 GiB of the host.
    if (!UnitImage.applySite(window[0..4], .page21, @intCast(target), window_address)) base.invariant("fast dev link invariant violated: a provide window cannot reach its procedure", .{});
    if (!UnitImage.applySite(window[4..8], .pageoff12, @intCast(target), window_address + 4)) unreachable; // an ADD takes any low bits
}

const Context = struct {
    out: []u8,
    images: []const ImageInput,
    places: []const Place,
    resolved: []const []u64,
    bindings: []const []?HostPrelink.HostSymbol,
    text_fileoff: u64,
    text_vmaddr: u64,
    data_fileoff: u64,
    data_vmaddr: u64,
    debug_offsets: [3]u64,
    symtab_offset: u64,
    /// Section ordinals of the app's text, data and zero-fill.
    section_ordinals: [3]u8,
    reached_atoms: []const []const bool,
    symbol_slots: []const []const u32,
    signature_offset: u64,
    /// Where the code directory's page hashes start.
    hashes_offset: u64,
    host_page_hashes: []const [32]u8,
    /// The load commands end here; pages before it are rehashed.
    dirty_pages_end: u64,
    /// File ranges the link wrote inside the host's segments.
    dirty_ranges: []const [2]u64,

    /// The file offset of an address in the app region's file-backed bytes.
    fn fileOffsetOf(self: *const Context, address: u64) u64 {
        if (address >= self.data_vmaddr) return self.data_fileoff + (address - self.data_vmaddr);
        return self.text_fileoff + (address - self.text_vmaddr);
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

    /// Whether page `page` of the output holds bytes of the host's segments
    /// the link left as they were.
    fn pageUnchanged(self: *const Context, page: u64) bool {
        if (page >= self.host_page_hashes.len) return false;
        const start = page * page_size;
        const end = start + page_size;
        if (start < self.dirty_pages_end) return false;
        for (self.dirty_ranges) |range| {
            if (range[0] < end and range[1] > start) return false;
        }
        return true;
    }
};

const Task = struct {
    context: *const Context,
    work: union(enum) {
        copies: []const fanout.Copy,
        symbols: fanout.Range,
        sites: fanout.Range,
        /// A range of the output's pages to hash.
        hashes: fanout.Range,
    },
    sites: u64 = 0,
    bytes: u64 = 0,

    pub fn run(opaque_task: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
        const self: *Task = @ptrCast(@alignCast(opaque_task));
        switch (self.work) {
            .copies => |list| self.bytes += fanout.runCopies(self.context.out, list),
            .symbols => |range| self.writeSymbols(range),
            .sites => |range| self.applySites(range),
            .hashes => |range| self.hashPages(range),
        }
        return opaque_task;
    }

    fn writeSymbols(self: *Task, range: fanout.Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const symbols = view.symbols()[range.first..][0..range.count];
        const slots = context.symbol_slots[range.image][range.first..][0..range.count];
        var written: u64 = 0;
        for (symbols, slots) |symbol, slot| {
            // A symbol of code the link left as a hole has no entry.
            if (slot == no_symbol) continue;
            const entry = context.out[@intCast(context.symtab_offset + (place.first_symbol + slot) * nlist_size)..][0..nlist_size];
            written += nlist_size;
            const region: UnitImage.Region = @fromBackingInt(symbol.region);
            std.mem.writeInt(u32, entry[0..4], @intCast(place.strings + symbol.name), .little);
            entry[4] = macho.N_SECT;
            entry[5] = context.section_ordinals[
                switch (region) {
                    .block => 0,
                    .data => 1,
                    .bss => 2,
                    .debug_line, .debug_info, .debug_abbrev => unreachable, // image symbols live only in allocated regions
                }
            ];
            std.mem.writeInt(u16, entry[6..8], 0, .little);
            std.mem.writeInt(u64, entry[8..16], place.base(region) + symbol.value, .little);
        }
        self.bytes = written;
    }

    fn applySites(self: *Task, range: fanout.Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const addresses = context.resolved[range.image];
        const bindings = context.bindings[range.image];
        const atoms_reached = context.reached_atoms[range.image];
        for (view.sites()[range.first..][0..range.count]) |site| {
            if (site.atom != UnitImage.no_atom and !atoms_reached[site.atom]) continue;
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const out = context.regionOut(place, view, region);
            const field = out[site.offset..][0..@intCast(UnitImage.kindWidth(kind))];
            if (site.target_region == UnitImage.import_target) {
                // A word dyld binds holds nothing until it does.
                if (kind == .abs64 and bindings[site.target] != null) {
                    @memset(field, 0);
                    continue;
                }
            }
            const target = if (site.target_region == UnitImage.import_target) addresses[site.target] else place.base(@fromBackingInt(site.target_region));
            const value = @as(i64, @intCast(target)) + site.addend;
            // The layout keeps the app region within page reach of the
            // host and of itself, and debug sections within 32-bit offsets.
            if (!UnitImage.applySite(field, kind, value, place.base(region) + site.offset)) {
                base.invariant("fast dev link invariant violated: a patch site's value does not fit its field", .{});
            }
        }
        self.sites = range.count;
    }

    fn hashPages(self: *Task, range: fanout.Range) void {
        const context = self.context;
        for (range.first..range.first + range.count) |page| {
            const slot = context.out[@intCast(context.hashes_offset + page * 32)..][0..32];
            if (context.pageUnchanged(page)) {
                @memcpy(slot, &context.host_page_hashes[page]);
                continue;
            }
            const start = page * page_size;
            const end = @min(start + page_size, context.signature_offset);
            std.crypto.hash.sha2.Sha256.hash(context.out[@intCast(start)..@intCast(end)], slot, .{});
        }
        self.bytes = range.count * 32;
    }
};

/// The signature's blobs before the page hashes: the superblob, one blob
/// index, the code directory and the identifier.
const signature_identifier = "roc";
const code_directory_size = 88;
const signature_header_size: u64 = 12 + 8 + code_directory_size + signature_identifier.len + 1;

fn writeSignatureHeader(out: *[signature_header_size]u8, code_limit: u32, page_count: u64, text_fileoff: u64, text_filesize: u64) void {
    const total: u32 = @intCast(signature_header_size + page_count * 32);
    std.mem.writeInt(u32, out[0..4], macho.CSMAGIC_EMBEDDED_SIGNATURE, .big);
    std.mem.writeInt(u32, out[4..8], total, .big);
    std.mem.writeInt(u32, out[8..12], 1, .big);
    std.mem.writeInt(u32, out[12..16], macho.CSSLOT_CODEDIRECTORY, .big);
    std.mem.writeInt(u32, out[16..20], 20, .big);
    const cd = out[20..][0 .. code_directory_size + signature_identifier.len + 1];
    std.mem.writeInt(u32, cd[0..4], macho.CSMAGIC_CODEDIRECTORY, .big);
    std.mem.writeInt(u32, cd[4..8], total - 20, .big);
    std.mem.writeInt(u32, cd[8..12], macho.CS_SUPPORTSEXECSEG, .big);
    std.mem.writeInt(u32, cd[12..16], macho.CS_ADHOC | macho.CS_LINKER_SIGNED, .big);
    std.mem.writeInt(u32, cd[16..20], code_directory_size + signature_identifier.len + 1, .big);
    std.mem.writeInt(u32, cd[20..24], code_directory_size, .big);
    std.mem.writeInt(u32, cd[24..28], 0, .big);
    std.mem.writeInt(u32, cd[28..32], @intCast(page_count), .big);
    std.mem.writeInt(u32, cd[32..36], code_limit, .big);
    cd[36] = 32;
    cd[37] = macho.CS_HASHTYPE_SHA256;
    cd[38] = 0;
    cd[39] = std.math.log2_int(u64, page_size);
    std.mem.writeInt(u32, cd[40..44], 0, .big);
    std.mem.writeInt(u32, cd[44..48], 0, .big);
    std.mem.writeInt(u32, cd[48..52], 0, .big);
    std.mem.writeInt(u32, cd[52..56], 0, .big);
    std.mem.writeInt(u64, cd[56..64], 0, .big);
    std.mem.writeInt(u64, cd[64..72], text_fileoff, .big);
    std.mem.writeInt(u64, cd[72..80], text_filesize, .big);
    std.mem.writeInt(u64, cd[80..88], macho.CS_EXECSEG_MAIN_BINARY, .big);
    @memcpy(cd[88..][0..signature_identifier.len], signature_identifier);
    cd[88 + signature_identifier.len] = 0;
}

/// Where everything landed, for the load commands.
const Layout = struct {
    text_vmaddr: u64,
    text_fileoff: u64,
    text_size: u64,
    text_content: u64,
    data_vmaddr: u64,
    data_fileoff: u64,
    data_filesize: u64,
    data_vmsize: u64,
    data_content: u64,
    bss_size: u64,
    dwarf_fileoff: u64,
    dwarf_size: u64,
    debug_offsets: [3]u64,
    debug_sizes: [3]u64,
    linkedit_vmaddr: u64,
    linkedit_fileoff: u64,
    linkedit_filesize: u64,
    rebase_offset: u64,
    rebase_size: u64,
    bind_offset: u64,
    bind_size: u64,
    weak_bind_offset: u64,
    lazy_bind_offset: u64,
    export_offset: u64,
    function_starts_offset: u64,
    data_in_code_offset: u64,
    symtab_offset: u64,
    symtab_count: u64,
    app_symbol_count: u64,
    indirect_offset: u64,
    strtab_offset: u64,
    strtab_size: u64,
    signature_offset: u64,
    signature_size: u64,
};

/// The three segment commands the link adds before `__LINKEDIT`'s.
const inserted_commands: u32 = HostPrelink.app_load_commands;

/// Rewrite the host's load commands for the output: the app's segments
/// inserted before `__LINKEDIT`, and every `__LINKEDIT` reference moved.
fn rewriteLoadCommands(out: []u8, exe: HostPrelink.Executable, layout: Layout) void {
    const linkedit_command = exe.segments[exe.linkedit].command_offset;
    std.debug.assert(exe.commands_end + inserted_commands <= exe.first_content);
    const moved = out[linkedit_command..exe.commands_end];
    @memmove(out[linkedit_command + inserted_commands ..][0..moved.len], moved);
    var at: usize = linkedit_command;
    at = writeSegment(out, at, "__ROC_TEXT", layout.text_vmaddr, layout.text_size, layout.text_fileoff, layout.text_size, 5, &.{
        .{ .name = "__text", .addr = layout.text_vmaddr, .size = layout.text_content, .offset = layout.text_fileoff, .flags = macho.S_ATTR_PURE_INSTRUCTIONS | macho.S_ATTR_SOME_INSTRUCTIONS, .@"align" = 12 },
    });
    at = writeSegment(out, at, "__ROC_DATA", layout.data_vmaddr, layout.data_vmsize, layout.data_fileoff, layout.data_filesize, 3, &.{
        .{ .name = "__data", .addr = layout.data_vmaddr, .size = layout.data_content, .offset = layout.data_fileoff, .flags = macho.S_REGULAR, .@"align" = 12 },
        .{ .name = "__bss", .addr = layout.data_vmaddr + layout.data_content, .size = layout.bss_size, .offset = 0, .flags = macho.S_ZEROFILL, .@"align" = 4 },
    });
    at = writeSegment(out, at, "__DWARF", 0, 0, layout.dwarf_fileoff, layout.dwarf_size, 0, &.{
        .{ .name = "__debug_abbrev", .addr = layout.debug_offsets[0] - layout.dwarf_fileoff, .size = layout.debug_sizes[0], .offset = layout.debug_offsets[0], .flags = macho.S_ATTR_DEBUG, .@"align" = 0 },
        .{ .name = "__debug_info", .addr = layout.debug_offsets[1] - layout.dwarf_fileoff, .size = layout.debug_sizes[1], .offset = layout.debug_offsets[1], .flags = macho.S_ATTR_DEBUG, .@"align" = 0 },
        .{ .name = "__debug_line", .addr = layout.debug_offsets[2] - layout.dwarf_fileoff, .size = layout.debug_sizes[2], .offset = layout.debug_offsets[2], .flags = macho.S_ATTR_DEBUG, .@"align" = 0 },
    });
    std.debug.assert(at == linkedit_command + inserted_commands);
    const Shift = struct {
        insertion: u32,
        fn of(self: @This(), command_offset: u32) usize {
            return if (command_offset >= self.insertion) command_offset + inserted_commands else command_offset;
        }
    };
    const shift: Shift = .{ .insertion = linkedit_command };
    const linkedit = shift.of(linkedit_command);
    std.mem.writeInt(u64, out[linkedit + 24 ..][0..8], layout.linkedit_vmaddr, .little);
    std.mem.writeInt(u64, out[linkedit + 32 ..][0..8], std.mem.alignForward(u64, layout.linkedit_filesize, page_size), .little);
    std.mem.writeInt(u64, out[linkedit + 40 ..][0..8], layout.linkedit_fileoff, .little);
    std.mem.writeInt(u64, out[linkedit + 48 ..][0..8], layout.linkedit_filesize, .little);
    const symtab = shift.of(exe.symtab.?.command_offset);
    std.mem.writeInt(u32, out[symtab + 8 ..][0..4], @intCast(layout.symtab_offset), .little);
    std.mem.writeInt(u32, out[symtab + 12 ..][0..4], @intCast(layout.symtab_count), .little);
    std.mem.writeInt(u32, out[symtab + 16 ..][0..4], @intCast(layout.strtab_offset), .little);
    std.mem.writeInt(u32, out[symtab + 20 ..][0..4], @intCast(layout.strtab_size), .little);
    const dysymtab = shift.of(exe.dysymtab.?.command_offset);
    const added: u32 = @intCast(layout.app_symbol_count);
    std.mem.writeInt(u32, out[dysymtab + 12 ..][0..4], exe.dysymtab.?.nlocalsym + added, .little);
    std.mem.writeInt(u32, out[dysymtab + 16 ..][0..4], exe.dysymtab.?.iextdefsym + added, .little);
    std.mem.writeInt(u32, out[dysymtab + 24 ..][0..4], exe.dysymtab.?.iundefsym + added, .little);
    std.mem.writeInt(u32, out[dysymtab + 56 ..][0..4], @intCast(layout.indirect_offset), .little);
    const info = shift.of(exe.dyld_info.?.command_offset);
    std.mem.writeInt(u32, out[info + 8 ..][0..4], @intCast(layout.rebase_offset), .little);
    std.mem.writeInt(u32, out[info + 12 ..][0..4], @intCast(layout.rebase_size), .little);
    std.mem.writeInt(u32, out[info + 16 ..][0..4], @intCast(layout.bind_offset), .little);
    std.mem.writeInt(u32, out[info + 20 ..][0..4], @intCast(layout.bind_size), .little);
    std.mem.writeInt(u32, out[info + 24 ..][0..4], if (exe.dyld_info.?.weak_bind_size == 0) 0 else @intCast(layout.weak_bind_offset), .little);
    std.mem.writeInt(u32, out[info + 32 ..][0..4], if (exe.dyld_info.?.lazy_bind_size == 0) 0 else @intCast(layout.lazy_bind_offset), .little);
    std.mem.writeInt(u32, out[info + 40 ..][0..4], if (exe.dyld_info.?.export_size == 0) 0 else @intCast(layout.export_offset), .little);
    if (exe.function_starts) |data| {
        const command = shift.of(data.command_offset);
        std.mem.writeInt(u32, out[command + 8 ..][0..4], if (data.datasize == 0) 0 else @intCast(layout.function_starts_offset), .little);
    }
    if (exe.data_in_code) |data| {
        const command = shift.of(data.command_offset);
        std.mem.writeInt(u32, out[command + 8 ..][0..4], if (data.datasize == 0) 0 else @intCast(layout.data_in_code_offset), .little);
    }
    const signature = shift.of(exe.code_signature.?.command_offset);
    std.mem.writeInt(u32, out[signature + 8 ..][0..4], @intCast(layout.signature_offset), .little);
    std.mem.writeInt(u32, out[signature + 12 ..][0..4], @intCast(layout.signature_size), .little);
    std.mem.writeInt(u32, out[16..20], exe.header.ncmds + 3, .little);
    std.mem.writeInt(u32, out[20..24], exe.header.sizeofcmds + inserted_commands, .little);
}

const SectionSpec = struct { name: []const u8, addr: u64, size: u64, offset: u64, flags: u32, @"align": u32 };

fn writeSegment(out: []u8, at: usize, name: []const u8, vmaddr: u64, vmsize: u64, fileoff: u64, filesize: u64, prot: u32, sections: []const SectionSpec) usize {
    const command = out[at..][0..segment_command_size];
    @memset(command, 0);
    std.mem.writeInt(u32, command[0..4], @backingInt(macho.LC.SEGMENT_64), .little);
    std.mem.writeInt(u32, command[4..8], @intCast(segment_command_size + sections.len * section_header_size), .little);
    @memcpy(command[8..][0..name.len], name);
    std.mem.writeInt(u64, command[24..32], vmaddr, .little);
    std.mem.writeInt(u64, command[32..40], vmsize, .little);
    std.mem.writeInt(u64, command[40..48], fileoff, .little);
    std.mem.writeInt(u64, command[48..56], filesize, .little);
    std.mem.writeInt(u32, command[56..60], prot, .little);
    std.mem.writeInt(u32, command[60..64], prot, .little);
    std.mem.writeInt(u32, command[64..68], @intCast(sections.len), .little);
    var cursor = at + segment_command_size;
    for (sections) |section| {
        const header = out[cursor..][0..section_header_size];
        @memset(header, 0);
        @memcpy(header[0..section.name.len], section.name);
        @memcpy(header[16..][0..name.len], name);
        std.mem.writeInt(u64, header[32..40], section.addr, .little);
        std.mem.writeInt(u64, header[40..48], section.size, .little);
        std.mem.writeInt(u32, header[48..52], @intCast(section.offset), .little);
        std.mem.writeInt(u32, header[52..56], section.@"align", .little);
        std.mem.writeInt(u32, header[64..68], section.flags, .little);
        cursor += section_header_size;
    }
    return cursor;
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

test "dyld opcode streams encode rebases and binds" {
    const allocator = std.testing.allocator;
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    try encodeRebases(allocator, &out, &.{ .{ .segment = 4, .offset = 0x10 }, .{ .segment = 4, .offset = 0x18 }, .{ .segment = 4, .offset = 0x200 } });
    try std.testing.expectEqualSlices(u8, &.{ 0x11, 0x24, 0x10, 0x51, 0x51, 0x30, 0xe0, 0x03, 0x51 }, out.items);
    out.clearRetainingCapacity();
    try encodeBinds(allocator, &out, &.{.{ .fixup = .{ .segment = 4, .offset = 8 }, .symbol = .{ .name = 0, .ordinal = 1, .flags = 0, .vaddr = 0 }, .name = "memcpy", .addend = 0 }});
    const expected = [_]u8{ 0x51, 0x11, 0x40 } ++ "_memcpy\x00".* ++ [_]u8{ 0x60, 0x00, 0x74, 0x08, 0x90 };
    try std.testing.expectEqualSlices(u8, &expected, out.items);
    out.clearRetainingCapacity();
    try appendSleb(allocator, &out, -1);
    try appendSleb(allocator, &out, 128);
    try std.testing.expectEqualSlices(u8, &.{ 0x7f, 0x80, 0x01 }, out.items);
}

test "a dyld opcode stream ends at its DONE, not at its padding" {
    // SET_SEGMENT_AND_OFFSET_ULEB with a zero offset, one rebase, DONE, padding.
    try std.testing.expectEqualSlices(u8, &.{ 0x11, 0x24, 0x00, 0x51 }, withoutDone(&.{ 0x11, 0x24, 0x00, 0x51, 0x00, 0x00, 0x00 }, .rebase));
    const bind = [_]u8{ 0x51, 0x11, 0x40 } ++ "_memcpy\x00".* ++ [_]u8{ 0x74, 0x00, 0x90, 0x00, 0x00 };
    try std.testing.expectEqualSlices(u8, bind[0 .. bind.len - 2], withoutDone(&bind, .bind));
    try std.testing.expectEqualSlices(u8, &.{}, withoutDone(&.{}, .rebase));
}

test "a long jump window reaches its target" {
    var window: [16]u8 = undefined;
    writeLongJump(&window, 0x1000_4000, 0x1004_5678);
    // ADRP x16: 0x41 pages forward; ADD x16, x16, #0x678.
    try std.testing.expectEqual(@as(u32, 0x9000_0010 | ((0x41 & 3) << 29) | ((0x41 >> 2) << 5)), std.mem.readInt(u32, window[0..4], .little));
    try std.testing.expectEqual(@as(u32, 0x9100_0210 | (0x678 << 10)), std.mem.readInt(u32, window[4..8], .little));
}

test "the signature header describes its code directory" {
    var header: [signature_header_size]u8 = undefined;
    writeSignatureHeader(&header, 0x2_0000, 8, 0, 0x4000);
    try std.testing.expectEqual(macho.CSMAGIC_EMBEDDED_SIGNATURE, std.mem.readInt(u32, header[0..4], .big));
    try std.testing.expectEqual(@as(u32, @intCast(signature_header_size + 8 * 32)), std.mem.readInt(u32, header[4..8], .big));
    try std.testing.expectEqual(@as(u32, 8), std.mem.readInt(u32, header[48..52], .big));
    try std.testing.expectEqual(@as(u8, 14), header[59]);
    try std.testing.expectEqualStrings("roc", header[108..111]);
}
