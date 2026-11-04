//! The patch link of a fast dev link of a Windows executable: the
//! executable is the pre-linked host's headers and sections
//! (`HostPrelink.zig`) followed by the app region, made of the unit images
//! (`backend.dev.UnitImage`) the program reaches and the support image, the
//! exception and base relocation tables, the debug information, and a
//! rebuilt symbol table.
//!
//! The app region is `.roctext`, read-and-execute, holding every placed
//! image's code block (text, then its read-only data), and `.rocdata`,
//! read-and-write, holding every image's data with its zero-fill as the
//! section's virtual tail. `.rocpdat` is the exception directory: the host's
//! function table entries and those of the app's placed functions, sorted,
//! then the app's unwind information. `.rocrelo` is the base relocation
//! directory: the host's blocks and one entry for every absolute word the
//! app holds, so the executable keeps its dynamic base. A `provides`
//! symbol's window in the host is filled too: a jump to the entry wrapper,
//! or the value's bytes with their relocations. The host's debug sections,
//! which LLD places last, move after the app with the images' fragments
//! appended; the symbol table follows with the app's symbols appended and
//! the host's entries renumbered for the inserted sections. When the host's
//! header page has no room for the inserted section headers, its section
//! contents move one page down the file.
//!
//! As on Linux, the output is written through a shared mapping in bounded
//! tasks on the compilation's post-check executor, every write landing in
//! the writing task's own region, with the host's words, the windows, the
//! headers and the file tail belonging to the coordinating thread.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
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
const symbol_size = 18;
const section_header_size = 40;
const debug_entry_size = 28;

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

const text_section_name = ".roctext";
const data_section_name = ".rocdata";
const unwind_section_name = ".rocpdat";
const relocation_section_name = ".rocrelo";
const debug_section_names = [_][]const u8{ ".debug_abbrev", ".debug_info", ".debug_line" };
const debug_regions = [_]UnitImage.Region{ .debug_abbrev, .debug_info, .debug_line };

const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
const IMAGE_SCN_CNT_UNINITIALIZED_DATA: u32 = 0x0000_0080;
const IMAGE_SCN_MEM_DISCARDABLE: u32 = 0x0200_0000;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
const IMAGE_SCN_MEM_WRITE: u32 = 0x8000_0000;
const IMAGE_REL_BASED_DIR64: u16 = 10;
const IMAGE_DEBUG_TYPE_CODEVIEW: u32 = 2;
const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;
const IMAGE_SYM_CLASS_STATIC: u8 = 3;
const IMAGE_SYM_DTYPE_FUNCTION: u16 = 0x20;

/// Link `inputs` into `output`, whose first `host.sections().len` bytes
/// already hold the host's headers and section contents.
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

    // The host's headers, which the record admitted.
    var parsed: HostPrelink.Executable.Parsed = undefined;
    const exe = parsed.parse(host.host()) catch unreachable; // the host record was admitted only after these headers parsed
    const arm64 = exe.machine == HostPrelink.IMAGE_FILE_MACHINE_ARM64;
    const entry_size: u64 = if (arm64) 8 else 12;
    const reached_atoms = try gpa.alloc([]const bool, images.items.len);
    defer gpa.free(reached_atoms);
    for (walk.states, reached_atoms) |state, *atoms| atoms.* = if (state) |placed| placed.atoms else &.{};

    // The host's debug sections move after the app; every other host
    // section stays where it is.
    var debug_host_index = [_]?usize{ null, null, null };
    var app_rva: u64 = 0;
    for (exe.sections, 0..) |section, index| {
        const slot = for (debug_section_names, 0..) |name, slot| {
            if (std.mem.eql(u8, section.name, name)) break slot;
        } else null;
        if (slot) |found| {
            debug_host_index[found] = index;
            continue;
        }
        app_rva = @max(app_rva, @as(u64, section.rva) + @max(section.virtual_size, section.raw_size));
    }
    app_rva = std.mem.alignForward(u64, app_rva, page_size);

    // Layout: text, data, unwind, relocations, debug sections, symbols.
    const places = try gpa.alloc(Place, images.items.len);
    defer gpa.free(places);
    @memset(places, .{});
    var sections: std.ArrayList(OutSection) = .empty;
    defer sections.deinit(gpa);
    const text_rva = app_rva;
    var text_size: u64 = 0;
    for (order.items) |index| {
        const view = images.items[index].view;
        text_size = std.mem.alignForward(u64, text_size, block_align);
        places[index].block = text_rva + text_size;
        text_size += view.block().len;
    }
    const text_index = try addSection(gpa, &sections, text_section_name, text_rva, text_size, text_size, IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ);
    const data_rva = sections.items[text_index].end();
    var data_size: u64 = 0;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.data().len == 0) continue;
        data_size = std.mem.alignForward(u64, data_size, block_align);
        places[index].data = data_rva + data_size;
        data_size += view.data().len;
    }
    var data_virtual_size = data_size;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.bssSize() == 0) continue;
        data_virtual_size = std.mem.alignForward(u64, data_virtual_size, @max(view.bssAlign(), 16));
        places[index].bss = data_rva + data_virtual_size;
        data_virtual_size += view.bssSize();
    }
    const data_index = try addSection(gpa, &sections, data_section_name, data_rva, data_virtual_size, data_size, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE);
    // The exception directory: the host's entries, each placed image's
    // entries, then the unwind information the entries name.
    const unwind_rva = sections.items[data_index].end();
    const host_exceptions = exe.directories[HostPrelink.Executable.exception_directory];
    var unwind_size: u64 = host_exceptions.size;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.pdata().len == 0) continue;
        unwind_size = std.mem.alignForward(u64, unwind_size, 4);
        places[index].pdata = unwind_rva + unwind_size;
        unwind_size += view.pdata().len;
    }
    const entries_size = unwind_size;
    for (order.items) |index| {
        const view = images.items[index].view;
        if (view.xdata().len == 0) continue;
        unwind_size = std.mem.alignForward(u64, unwind_size, 16);
        places[index].xdata = unwind_rva + unwind_size;
        unwind_size += view.xdata().len;
    }
    const unwind_index = try addSection(gpa, &sections, unwind_section_name, unwind_rva, unwind_size, unwind_size, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ);

    // Which unwind entries the link keeps: those of placed functions.
    const kept_entries = try gpa.alloc([]bool, images.items.len);
    @memset(kept_entries, &.{});
    defer {
        for (order.items) |index| gpa.free(kept_entries[index]);
        gpa.free(kept_entries);
    }
    for (order.items) |index| {
        const view = images.items[index].view;
        const count = view.pdata().len / entry_size;
        kept_entries[index] = try gpa.alloc(bool, count);
        @memset(kept_entries[index], true);
        for (view.localSites()) |site| {
            if (@as(UnitImage.Region, @fromBackingInt(site.region)) != .pdata or site.offset % entry_size != 0) continue;
            if (@as(UnitImage.Region, @fromBackingInt(site.target_region)) != .block) continue;
            const atom = atomOf(view, @intCast(site.addend));
            kept_entries[index][site.offset / entry_size] = reached_atoms[index][atom];
        }
    }

    // Import addresses, now that every image has its place. Every address
    // in the link is image relative.
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
                .host => |rva| rva,
                .missing => unreachable, // a missing host symbol failed the link above
            };
        }
    }

    // Every absolute word the app holds gets a base relocation; a value
    // window's words get those of the value they copy.
    var fixups: std.ArrayList(u64) = .empty;
    defer fixups.deinit(gpa);
    for (order.items) |index| {
        const view = images.items[index].view;
        const place = places[index];
        for (view.sites()) |site| {
            if (@as(UnitImage.SiteKind, @fromBackingInt(site.kind)) != .abs64) continue;
            if (site.atom != UnitImage.no_atom and !reached_atoms[index][site.atom]) continue;
            const region: UnitImage.Region = @fromBackingInt(site.region);
            switch (region) {
                .block, .data, .pdata, .xdata => {},
                // Debug words are not loaded.
                .bss, .debug_line, .debug_info, .debug_abbrev => continue,
            }
            try fixups.append(gpa, place.base(region) + site.offset);
        }
    }
    const provided = try gpa.alloc(SymbolRef, host.provides().len);
    defer gpa.free(provided);
    for (host.provides(), provided) |provide, *slot| {
        const name = host.name(provide.name);
        slot.* = exports.get(name) orelse unreachable; // the walk above resolved every provided symbol
        const view = images.items[slot.image].view;
        const symbol = view.symbols()[slot.symbol];
        if (@as(HostPrelink.ProvideKind, @fromBackingInt(@intCast(provide.kind))) == .procedure) continue;
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        if (region != .data and region != .block) continue;
        for (view.sites()) |site| {
            if (@as(UnitImage.SiteKind, @fromBackingInt(site.kind)) != .abs64) continue;
            if (@as(UnitImage.Region, @fromBackingInt(site.region)) != region) continue;
            if (site.offset < symbol.value or site.offset + 8 > symbol.value + @min(symbol.size, provide.size)) continue;
            try fixups.append(gpa, provide.rva + (site.offset - symbol.value));
        }
    }
    std.mem.sort(u64, fixups.items, {}, std.sort.asc(u64));
    var relocation_blocks: std.ArrayList(u8) = .empty;
    defer relocation_blocks.deinit(gpa);
    const host_relocations = exe.directories[HostPrelink.Executable.base_relocation_directory];
    try relocation_blocks.appendSlice(gpa, hostDirectory(host, exe, host_relocations));
    try encodeRelocations(gpa, &relocation_blocks, fixups.items);
    const relocation_rva = sections.items[unwind_index].end();
    const relocation_index = try addSection(gpa, &sections, relocation_section_name, relocation_rva, relocation_blocks.items.len, relocation_blocks.items.len, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE);

    // The debug sections: the host's bytes, then the images' fragments.
    var debug_rva = sections.items[relocation_index].end();
    var debug_index = [_]?usize{ null, null, null };
    var debug_host_size = [_]u64{ 0, 0, 0 };
    for (debug_regions, debug_host_index, 0..) |region, host_index, slot| {
        var size: u64 = if (host_index) |index| exe.sections[index].virtual_size else 0;
        debug_host_size[slot] = size;
        for (order.items) |index| {
            const fragment = images.items[index].view.regionBytes(region);
            places[index].debug[slot] = size;
            size += fragment.len;
        }
        if (size == 0) continue;
        debug_index[slot] = try addSection(gpa, &sections, debug_section_names[slot], debug_rva, size, size, IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE);
        debug_rva = sections.items[debug_index[slot].?].end();
    }
    if (debug_rva > std.math.maxInt(u32)) return error.ProgramTooLarge;

    // Section numbers: the host's sections other than its debug ones keep
    // theirs, the app's follow, the debug sections come last.
    const host_number = try gpa.alloc(u16, exe.sections.len);
    defer gpa.free(host_number);
    var kept_host_sections: u16 = 0;
    for (exe.sections, host_number, 0..) |_, *number, index| {
        const is_debug = for (debug_host_index) |host_index| {
            if (host_index != null and host_index.? == index) break true;
        } else false;
        if (is_debug) continue;
        kept_host_sections += 1;
        number.* = kept_host_sections;
    }
    for (debug_host_index, debug_index) |host_index, out_index| {
        if (host_index) |index| host_number[index] = @intCast(kept_host_sections + 1 + (out_index orelse unreachable)); // a host debug section is kept with its contents
    }
    const section_count: u16 = kept_host_sections + @as(u16, @intCast(sections.items.len));
    const headers_needed: u64 = exe.section_headers_offset + @as(u64, section_count) * section_header_size;
    // When the header page cannot hold the headers, the host's section
    // contents move one page down the file.
    const file_shift: u64 = if (headers_needed <= exe.size_of_headers) 0 else page_size;
    const size_of_headers: u64 = exe.size_of_headers + file_shift;
    var file_at: u64 = host.trailer.sections_end + file_shift;
    for (sections.items) |*section| {
        section.raw_offset = file_at;
        file_at += section.rawSize();
    }

    // Symbols of code the link leaves as a hole get no entry.
    const symbol_slots = try gpa.alloc([]u32, images.items.len);
    @memset(symbol_slots, &.{});
    defer {
        for (order.items) |index| gpa.free(symbol_slots[index]);
        gpa.free(symbol_slots);
    }
    const host_strtab = host.host()[host.trailer.sections_end + exe.symbol_count * symbol_size ..];
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
        places[index].first_symbol = exe.symbol_count + app_symbol_count;
        app_symbol_count += kept;
        places[index].strings = host_strtab.len + app_strings;
        app_strings += view.strings().len;
    }
    const symtab_offset = file_at;
    const symbol_count = exe.symbol_count + app_symbol_count;
    const strtab_offset = symtab_offset + symbol_count * symbol_size;
    // Section names longer than the header's field: a host debug section
    // keeps its string table reference, a new one gets its name appended.
    var long_names: std.ArrayList(u8) = .empty;
    defer long_names.deinit(gpa);
    for (sections.items) |*section| {
        if (section.name.len <= 8) {
            @memcpy(section.header_name[0..section.name.len], section.name);
            continue;
        }
        const host_index = for (debug_section_names, debug_host_index) |name, host_index| {
            if (std.mem.eql(u8, section.name, name)) break host_index;
        } else null;
        if (host_index) |index| {
            @memcpy(&section.header_name, host.host()[exe.section_headers_offset + index * section_header_size ..][0..8]);
            continue;
        }
        const offset = host_strtab.len + app_strings + long_names.items.len;
        section.header_name[0] = '/';
        _ = std.fmt.printInt(section.header_name[1..], offset, 10, .lower, .{});
        try long_names.appendSlice(gpa, section.name);
        try long_names.append(gpa, 0);
    }
    const long_names_offset = host_strtab.len + app_strings;
    const strtab_size = long_names_offset + long_names.items.len;
    const file_size = strtab_offset + strtab_size;
    if (file_size > std.math.maxInt(u32)) return error.ProgramTooLarge;

    // Provided values must fit their windows.
    for (host.provides(), provided) |provide, found| {
        const symbol = images.items[found.image].view.symbols()[found.symbol];
        if (@as(HostPrelink.ProvideKind, @fromBackingInt(@intCast(provide.kind))) == .data and symbol.size > provide.size) {
            outcome.too_large = .{ .symbol = try arena.dupe(u8, host.name(provide.name)), .size = symbol.size };
            return error.ProvidedValueTooLarge;
        }
    }

    try output.resize(file_size);
    const mapped = try output.map(file_size);
    var unmapped = false;
    defer if (!unmapped) output.unmap(mapped);
    timing.layout_ns = elapsed(std_io, layout_started);

    // Fan-out, in two waves of bounded tasks: every byte copy and every
    // symbol entry, then every patch site, which writes into bytes the
    // first wave copied.
    const fan_out_started = now(std_io);
    var app_section_numbers: [3]u16 = undefined;
    app_section_numbers[0] = kept_host_sections + 1 + @as(u16, @intCast(text_index));
    app_section_numbers[1] = kept_host_sections + 1 + @as(u16, @intCast(data_index));
    app_section_numbers[2] = kept_host_sections + 1 + @as(u16, @intCast(unwind_index));
    const context: Context = .{
        .out = mapped,
        .images = images.items,
        .places = places,
        .resolved = resolved,
        .image_base = exe.image_base,
        .sections = sections.items,
        .text_index = text_index,
        .data_index = data_index,
        .unwind_index = unwind_index,
        .debug_index = debug_index,
        .symtab_offset = symtab_offset,
        .section_numbers = app_section_numbers,
        .reached_atoms = reached_atoms,
        .symbol_slots = symbol_slots,
    };
    var copy_list: std.ArrayList(fanout.Copy) = .empty;
    defer copy_list.deinit(gpa);
    var symbol_ranges: std.ArrayList(fanout.Range) = .empty;
    defer symbol_ranges.deinit(gpa);
    var site_ranges: std.ArrayList(fanout.Range) = .empty;
    defer site_ranges.deinit(gpa);
    if (file_shift != 0) {
        // The host's section contents, one page down.
        const contents = host.sections()[@intCast(exe.size_of_headers)..];
        try fanout.addCopies(gpa, &copy_list, contents, exe.size_of_headers + file_shift);
    }
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
        if (view.pdata().len != 0) try fanout.addCopies(gpa, &copy_list, view.pdata(), context.fileOffsetOf(place.pdata));
        if (view.xdata().len != 0) try fanout.addCopies(gpa, &copy_list, view.xdata(), context.fileOffsetOf(place.xdata));
        for (debug_regions, 0..) |region, slot| {
            const fragment = view.regionBytes(region);
            if (fragment.len != 0) try fanout.addCopies(gpa, &copy_list, fragment, sections.items[debug_index[slot].?].raw_offset + place.debug[slot]);
        }
        try fanout.addCopies(gpa, &copy_list, view.strings(), strtab_offset + place.strings);
        try fanout.addRanges(gpa, &symbol_ranges, index, view.symbols().len);
        try fanout.addRanges(gpa, &site_ranges, index, view.sites().len);
    }
    // The host's tables: its exception entries, relocation blocks, debug
    // sections, symbols and strings.
    try fanout.addCopies(gpa, &copy_list, hostDirectory(host, exe, host_exceptions), sections.items[unwind_index].raw_offset);
    try fanout.addCopies(gpa, &copy_list, relocation_blocks.items, sections.items[relocation_index].raw_offset);
    for (debug_host_index, debug_index) |host_index, out_index| {
        const index = host_index orelse continue;
        const section = exe.sections[index];
        const bytes = host.host()[section.raw_offset..][0..@min(section.virtual_size, section.raw_size)];
        try fanout.addCopies(gpa, &copy_list, bytes, sections.items[out_index.?].raw_offset);
    }
    try fanout.addCopies(gpa, &copy_list, host.host()[@intCast(host.trailer.sections_end)..][0 .. exe.symbol_count * symbol_size], symtab_offset);
    try fanout.addCopies(gpa, &copy_list, host_strtab, strtab_offset);
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

    // The coordinator's writes: the windows, the exception table's order,
    // the host's symbols' section numbers, the headers.
    const finalize_started = now(std_io);
    for (host.provides(), provided) |provide, found| {
        const window = mapped[@intCast(provide.offset + file_shift)..][0..@intCast(provide.size)];
        const view = images.items[found.image].view;
        const symbol = view.symbols()[found.symbol];
        const region: UnitImage.Region = @fromBackingInt(symbol.region);
        const address = places[found.image].base(region) + symbol.value;
        switch (@as(HostPrelink.ProvideKind, @fromBackingInt(@intCast(provide.kind)))) {
            .procedure => writeJump(arm64, window[0..16], exe.image_base + provide.rva, exe.image_base + address),
            .data => switch (region) {
                .block, .data => {
                    const size: usize = @intCast(@min(symbol.size, provide.size));
                    @memcpy(window[0..size], mapped[@intCast(context.fileOffsetOf(address))..][0..size]);
                },
                // Zero-fill: the window is already zero.
                .bss => {},
                .debug_line, .debug_info, .debug_abbrev, .pdata, .xdata => unreachable, // image symbols live only in the block, the data and the zero-fill
            },
        }
    }
    // The exception directory: the host's entries and the kept entries of
    // the app, in address order, then zeros up to the unwind information.
    const unwind_out = mapped[@intCast(sections.items[unwind_index].raw_offset)..][0..@intCast(entries_size)];
    var kept_total: u64 = host_exceptions.size;
    for (order.items) |index| {
        const place = places[index];
        for (kept_entries[index], 0..) |keep, entry| {
            if (!keep) continue;
            const from = place.pdata - unwind_rva + entry * entry_size;
            if (from != kept_total) std.mem.copyForwards(u8, unwind_out[@intCast(kept_total)..][0..@intCast(entry_size)], unwind_out[@intCast(from)..][0..@intCast(entry_size)]);
            kept_total += entry_size;
        }
    }
    @memset(unwind_out[@intCast(kept_total)..], 0);
    sortEntries(unwind_out[0..@intCast(kept_total)], entry_size);
    var index: usize = 0;
    while (index < exe.symbol_count) {
        const entry = mapped[@intCast(symtab_offset + index * symbol_size)..][0..symbol_size];
        const aux_count: usize = entry[17];
        defer index += 1 + aux_count;
        const section_number: i16 = @bitCast(std.mem.readInt(u16, entry[12..14], .little));
        if (section_number <= 0) continue;
        std.mem.writeInt(u16, entry[12..14], host_number[@intCast(section_number - 1)], .little);
    }
    var build_id = std.crypto.hash.sha2.Sha256.init(.{});
    build_id.update("roc-fast-dev-link-build-id-v1");
    build_id.update(&host.trailer.host_digest);
    for (order.items) |image| build_id.update(&images.items[image].view.header.digest);
    const digest = build_id.finalResult();
    writeBuildId(mapped, host, exe, file_shift, digest[0..16]);
    writeHeaders(mapped, exe, .{
        .host_section_headers = host.host()[exe.section_headers_offset..][0 .. exe.sections.len * section_header_size],
        .file_shift = file_shift,
        .size_of_headers = size_of_headers,
        .section_count = section_count,
        .host_number = host_number,
        .sections = sections.items,
        .size_of_image = std.mem.alignForward(u64, debug_rva, exe.section_alignment),
        .symtab_offset = symtab_offset,
        .symbol_count = symbol_count,
        .exception_rva = unwind_rva,
        .exception_size = kept_total,
        .relocation_rva = relocation_rva,
        .relocation_size = relocation_blocks.items.len,
    });
    @memcpy(mapped[@intCast(strtab_offset + long_names_offset)..][0..long_names.items.len], long_names.items);
    std.mem.writeInt(u32, mapped[@intCast(strtab_offset)..][0..4], @intCast(strtab_size), .little);
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
    return symbol.rva;
}

const no_symbol: u32 = std.math.maxInt(u32);

/// Where an image's regions land, as image-relative addresses.
const Place = struct {
    block: u64 = 0,
    data: u64 = 0,
    bss: u64 = 0,
    pdata: u64 = 0,
    xdata: u64 = 0,
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
            .pdata => self.pdata,
            .xdata => self.xdata,
            .debug_abbrev => self.debug[0],
            .debug_info => self.debug[1],
            .debug_line => self.debug[2],
        };
    }
};

/// A section the link appends.
const OutSection = struct {
    name: []const u8,
    /// The header's name field: the name inline when it fits, else a
    /// string table reference.
    header_name: [8]u8 = @splat(0),
    rva: u64,
    virtual_size: u64,
    /// The bytes the file holds; the rest of the virtual size is zero.
    content_size: u64,
    characteristics: u32,
    raw_offset: u64 = 0,

    fn rawSize(self: OutSection) u64 {
        return std.mem.alignForward(u64, self.content_size, page_size);
    }

    /// The image-relative address after the section's pages.
    fn end(self: OutSection) u64 {
        return std.mem.alignForward(u64, self.rva + self.virtual_size, page_size);
    }
};

fn addSection(gpa: Allocator, sections: *std.ArrayList(OutSection), name: []const u8, rva: u64, virtual_size: u64, content_size: u64, characteristics: u32) Allocator.Error!usize {
    try sections.append(gpa, .{ .name = name, .rva = rva, .virtual_size = virtual_size, .content_size = content_size, .characteristics = characteristics });
    return sections.items.len - 1;
}

/// The atom of `view` holding block offset `offset`.
fn atomOf(view: UnitImage.View, offset: u64) u32 {
    const starts = view.atoms();
    var low: usize = 0;
    var high: usize = starts.len;
    while (high - low > 1) {
        const mid = low + (high - low) / 2;
        if (starts[mid] <= offset) low = mid else high = mid;
    }
    return @intCast(low);
}

/// The bytes a data directory of the host names.
fn hostDirectory(host: HostPrelink.View, exe: HostPrelink.Executable, directory: HostPrelink.Executable.Directory) []const u8 {
    if (directory.size == 0) return &.{};
    const index = exe.sectionOf(directory.rva) orelse unreachable; // the host record was admitted with its directories inside its sections
    const section = exe.sections[index];
    return host.host()[section.raw_offset + (directory.rva - section.rva) ..][0..directory.size];
}

/// Append base relocation blocks for `rvas`, which must be sorted: one
/// block per page, each padded to a word.
fn encodeRelocations(gpa: Allocator, out: *std.ArrayList(u8), rvas: []const u64) Allocator.Error!void {
    var at: usize = 0;
    while (at < rvas.len) {
        const page = rvas[at] & ~(page_size - 1);
        var end = at;
        while (end < rvas.len and rvas[end] & ~(page_size - 1) == page) end += 1;
        const count = end - at;
        const padded = count + (count & 1);
        const block_size: u32 = @intCast(8 + padded * 2);
        var header: [8]u8 = undefined;
        std.mem.writeInt(u32, header[0..4], @intCast(page), .little);
        std.mem.writeInt(u32, header[4..8], block_size, .little);
        try out.appendSlice(gpa, &header);
        for (rvas[at..end]) |rva| {
            var entry: [2]u8 = undefined;
            std.mem.writeInt(u16, &entry, (IMAGE_REL_BASED_DIR64 << 12) | @as(u16, @intCast(rva & (page_size - 1))), .little);
            try out.appendSlice(gpa, &entry);
        }
        if (padded != count) try out.appendSlice(gpa, &.{ 0, 0 });
        at = end;
    }
}

/// Sort function table entries by their first word, the function's
/// image-relative address.
fn sortEntries(entries: []u8, entry_size: u64) void {
    const Sorter = struct {
        bytes: []u8,
        size: usize,

        pub fn lessThan(self: @This(), a: usize, b: usize) bool {
            return std.mem.readInt(u32, self.bytes[a * self.size ..][0..4], .little) < std.mem.readInt(u32, self.bytes[b * self.size ..][0..4], .little);
        }
        pub fn swap(self: @This(), a: usize, b: usize) void {
            var scratch: [12]u8 = undefined;
            @memcpy(scratch[0..self.size], self.bytes[a * self.size ..][0..self.size]);
            @memcpy(self.bytes[a * self.size ..][0..self.size], self.bytes[b * self.size ..][0..self.size]);
            @memcpy(self.bytes[b * self.size ..][0..self.size], scratch[0..self.size]);
        }
    };
    const size: usize = @intCast(entry_size);
    std.sort.pdqContext(0, entries.len / size, Sorter{ .bytes = entries, .size = size });
}

/// A procedure window: a jump from `window_address` to `target`, then
/// traps. On arm64 the window is ADRP, ADD, BR x16 and BRK.
fn writeJump(arm64: bool, window: *[16]u8, window_address: u64, target: u64) void {
    if (!arm64) {
        window[0] = 0xe9;
        std.mem.writeInt(i32, window[1..5], @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(window_address + 5))), .little);
        @memset(window[5..], 0xcc);
        return;
    }
    std.mem.writeInt(u32, window[0..4], 0x9000_0010, .little);
    std.mem.writeInt(u32, window[4..8], 0x9100_0210, .little);
    std.mem.writeInt(u32, window[8..12], 0xd61f_0200, .little);
    std.mem.writeInt(u32, window[12..16], 0xd420_0000, .little);
    // The app region is within 4 GiB of the host.
    if (!UnitImage.applySite(window[0..4], .page21, @intCast(target), window_address)) base.invariant("fast dev link invariant violated: a provide window cannot reach its procedure", .{});
    if (!UnitImage.applySite(window[4..8], .pageoff12, @intCast(target), window_address + 4)) unreachable; // an ADD takes any low bits
}

/// Write the link's build id into the host's CodeView debug entry, whose
/// signature identifies the executable.
fn writeBuildId(out: []u8, host: HostPrelink.View, exe: HostPrelink.Executable, file_shift: u64, id: *const [16]u8) void {
    const directory = exe.directories[HostPrelink.Executable.debug_directory];
    const entries = hostDirectory(host, exe, directory);
    var at: usize = 0;
    while (at + debug_entry_size <= entries.len) : (at += debug_entry_size) {
        const entry = entries[at..][0..debug_entry_size];
        if (std.mem.readInt(u32, entry[12..16], .little) != IMAGE_DEBUG_TYPE_CODEVIEW) continue;
        const size = std.mem.readInt(u32, entry[16..20], .little);
        const offset = std.mem.readInt(u32, entry[24..28], .little);
        if (size < 24 or @as(u64, offset) + size > host.trailer.sections_end) continue;
        const record = out[@intCast(offset + file_shift)..][0..size];
        if (!std.mem.eql(u8, record[0..4], "RSDS")) continue;
        @memcpy(record[4..20], id);
    }
}

const HeaderLayout = struct {
    /// The host's section header table, as it was.
    host_section_headers: []const u8,
    file_shift: u64,
    size_of_headers: u64,
    section_count: u16,
    host_number: []const u16,
    sections: []const OutSection,
    size_of_image: u64,
    symtab_offset: u64,
    symbol_count: u64,
    exception_rva: u64,
    exception_size: u64,
    relocation_rva: u64,
    relocation_size: u64,
};

/// Rewrite the headers: the section count and symbol table in the file
/// header; the sizes, directories and image size in the optional header;
/// the host's section headers in their new order, the app's after them.
fn writeHeaders(out: []u8, exe: HostPrelink.Executable, layout: HeaderLayout) void {
    const header = out[exe.coff_header_offset..][0..20];
    std.mem.writeInt(u16, header[2..4], layout.section_count, .little);
    std.mem.writeInt(u32, header[8..12], @intCast(layout.symtab_offset), .little);
    std.mem.writeInt(u32, header[12..16], @intCast(layout.symbol_count), .little);
    const optional = out[exe.coff_header_offset + 20 ..][0..240];
    var size_of_code: u64 = 0;
    var size_of_initialized: u64 = 0;
    var size_of_uninitialized: u64 = 0;
    const headers = out[exe.section_headers_offset..][0 .. @as(usize, layout.section_count) * section_header_size];
    @memset(headers, 0);
    for (exe.sections, layout.host_number, 0..) |section, number, index| {
        const entry = headers[(number - 1) * section_header_size ..][0..section_header_size];
        const kept_in_place = for (debug_section_names) |name| {
            if (std.mem.eql(u8, section.name, name)) break false;
        } else true;
        if (!kept_in_place) continue;
        const raw_offset = if (section.raw_size == 0) 0 else section.raw_offset + layout.file_shift;
        writeSectionHeader(entry, layout.host_section_headers[index * section_header_size ..][0..8], section.rva, section.virtual_size, section.raw_size, raw_offset, section.characteristics);
        countSection(section.characteristics, section.virtual_size, section.raw_size, &size_of_code, &size_of_initialized, &size_of_uninitialized);
    }
    _ = &size_of_code;
    for (layout.sections, 0..) |section, index| {
        const number = layout.section_count - layout.sections.len + index;
        const entry = headers[number * section_header_size ..][0..section_header_size];
        writeSectionHeader(entry, &section.header_name, section.rva, section.virtual_size, section.rawSize(), section.raw_offset, section.characteristics);
        countSection(section.characteristics, section.virtual_size, section.rawSize(), &size_of_code, &size_of_initialized, &size_of_uninitialized);
    }
    std.mem.writeInt(u32, optional[4..8], @intCast(size_of_code), .little);
    std.mem.writeInt(u32, optional[8..12], @intCast(size_of_initialized), .little);
    std.mem.writeInt(u32, optional[12..16], @intCast(size_of_uninitialized), .little);
    std.mem.writeInt(u32, optional[56..60], @intCast(layout.size_of_image), .little);
    std.mem.writeInt(u32, optional[60..64], @intCast(layout.size_of_headers), .little);
    const exception = optional[112 + HostPrelink.Executable.exception_directory * 8 ..][0..8];
    std.mem.writeInt(u32, exception[0..4], @intCast(if (layout.exception_size == 0) 0 else layout.exception_rva), .little);
    std.mem.writeInt(u32, exception[4..8], @intCast(layout.exception_size), .little);
    const relocation = optional[112 + HostPrelink.Executable.base_relocation_directory * 8 ..][0..8];
    std.mem.writeInt(u32, relocation[0..4], @intCast(if (layout.relocation_size == 0) 0 else layout.relocation_rva), .little);
    std.mem.writeInt(u32, relocation[4..8], @intCast(layout.relocation_size), .little);
}

fn writeSectionHeader(entry: *[section_header_size]u8, name: *const [8]u8, rva: u64, virtual_size: u64, raw_size: u64, raw_offset: u64, characteristics: u32) void {
    @memcpy(entry[0..8], name);
    std.mem.writeInt(u32, entry[8..12], @intCast(virtual_size), .little);
    std.mem.writeInt(u32, entry[12..16], @intCast(rva), .little);
    std.mem.writeInt(u32, entry[16..20], @intCast(raw_size), .little);
    std.mem.writeInt(u32, entry[20..24], @intCast(raw_offset), .little);
    std.mem.writeInt(u32, entry[36..40], characteristics, .little);
}

fn countSection(characteristics: u32, virtual_size: u64, raw_size: u64, code: *u64, initialized: *u64, uninitialized: *u64) void {
    if (characteristics & IMAGE_SCN_CNT_CODE != 0) code.* += raw_size;
    if (characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA != 0) initialized.* += raw_size;
    if (characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA != 0) uninitialized.* += virtual_size;
    if (virtual_size > raw_size and characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA != 0) uninitialized.* += virtual_size - raw_size;
}

const Context = struct {
    out: []u8,
    images: []const ImageInput,
    places: []const Place,
    resolved: []const []u64,
    image_base: u64,
    sections: []const OutSection,
    text_index: usize,
    data_index: usize,
    unwind_index: usize,
    debug_index: [3]?usize,
    symtab_offset: u64,
    /// Section numbers of the app's text, data and unwind sections.
    section_numbers: [3]u16,
    reached_atoms: []const []const bool,
    symbol_slots: []const []const u32,

    /// The file offset of an image-relative address in the app's sections.
    fn fileOffsetOf(self: *const Context, rva: u64) u64 {
        for ([_]usize{ self.text_index, self.data_index, self.unwind_index }) |index| {
            const section = self.sections[index];
            if (rva >= section.rva and rva < section.rva + section.content_size) return section.raw_offset + (rva - section.rva);
        }
        unreachable; // every placed region lies in an app section
    }

    /// The output bytes of `region` of the image placed at `place`.
    fn regionOut(self: *const Context, place: Place, view: UnitImage.View, region: UnitImage.Region) []u8 {
        const len = view.regionBytes(region).len;
        if (len == 0) return &.{};
        return switch (region) {
            .block, .data, .pdata, .xdata => self.out[@intCast(self.fileOffsetOf(place.base(region)))..][0..len],
            .bss => &.{},
            .debug_abbrev, .debug_info, .debug_line => self.out[@intCast(self.sections[self.debug_index[debugSlot(region)].?].raw_offset + place.base(region))..][0..len],
        };
    }
};

fn debugSlot(region: UnitImage.Region) usize {
    return switch (region) {
        .debug_abbrev => 0,
        .debug_info => 1,
        .debug_line => 2,
        .block, .data, .bss, .pdata, .xdata => unreachable, // only debug regions have fragment slots
    };
}

const Task = struct {
    context: *const Context,
    work: union(enum) {
        copies: []const fanout.Copy,
        symbols: fanout.Range,
        sites: fanout.Range,
    },
    sites: u64 = 0,
    bytes: u64 = 0,

    pub fn run(opaque_task: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
        const self: *Task = @ptrCast(@alignCast(opaque_task));
        switch (self.work) {
            .copies => |list| self.bytes += fanout.runCopies(self.context.out, list),
            .symbols => |range| self.writeSymbols(range),
            .sites => |range| self.applySites(range),
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
            const entry = context.out[@intCast(context.symtab_offset + (place.first_symbol + slot) * symbol_size)..][0..symbol_size];
            written += symbol_size;
            const region: UnitImage.Region = @fromBackingInt(symbol.region);
            const section_slot: usize = switch (region) {
                .block => 0,
                .data, .bss => 1,
                .pdata, .xdata => 2,
                .debug_line, .debug_info, .debug_abbrev => unreachable, // image symbols live only in allocated regions
            };
            const section = context.sections[
                switch (section_slot) {
                    0 => context.text_index,
                    1 => context.data_index,
                    else => context.unwind_index,
                }
            ];
            @memset(entry[0..4], 0);
            std.mem.writeInt(u32, entry[4..8], @intCast(place.strings + symbol.name), .little);
            std.mem.writeInt(u32, entry[8..12], @intCast(place.base(region) + symbol.value - section.rva), .little);
            std.mem.writeInt(u16, entry[12..14], context.section_numbers[section_slot], .little);
            std.mem.writeInt(u16, entry[14..16], if (@as(UnitImage.SymbolType, @fromBackingInt(symbol.type)) == .func) IMAGE_SYM_DTYPE_FUNCTION else 0, .little);
            entry[16] = if (symbol.flags & UnitImage.symbol_exported != 0) IMAGE_SYM_CLASS_EXTERNAL else IMAGE_SYM_CLASS_STATIC;
            entry[17] = 0;
        }
        self.bytes = written;
    }

    fn applySites(self: *Task, range: fanout.Range) void {
        const context = self.context;
        const view = context.images[range.image].view;
        const place = context.places[range.image];
        const addresses = context.resolved[range.image];
        const atoms_reached = context.reached_atoms[range.image];
        for (view.sites()[range.first..][0..range.count]) |site| {
            if (site.atom != UnitImage.no_atom and !atoms_reached[site.atom]) continue;
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const out = context.regionOut(place, view, region);
            const field = out[site.offset..][0..@intCast(UnitImage.kindWidth(kind))];
            const target = if (site.target_region == UnitImage.import_target) addresses[site.target] else place.base(@fromBackingInt(site.target_region));
            // Every address in the link is image relative: a relative
            // field or an image-relative one takes it as is, an absolute
            // field of an address adds the image base, and a field naming
            // an offset in a debug section holds the offset.
            const absolute = switch (kind) {
                .abs64, .abs32, .abs32s => targetIsAddress(site),
                .pc32, .branch26, .page21, .pageoff12, .rva32 => false,
            };
            const value = @as(i64, @intCast(target)) + site.addend + @as(i64, if (absolute) @intCast(context.image_base) else 0);
            // The layout keeps the app region within reach of the host and
            // of itself, and debug sections within 32-bit offsets.
            if (!UnitImage.applySite(field, kind, value, place.base(region) + site.offset)) {
                base.invariant("fast dev link invariant violated: a patch site's value does not fit its field", .{});
            }
        }
        self.sites = range.count;
    }
};

/// Whether a site's target is an address in the executable rather than an
/// offset in a debug section.
fn targetIsAddress(site: UnitImage.Site) bool {
    if (site.target_region == UnitImage.import_target) return true;
    return @as(UnitImage.Region, @fromBackingInt(site.target_region)).isAllocated();
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

test "base relocation blocks group words by page and pad to a word" {
    const allocator = std.testing.allocator;
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    try encodeRelocations(allocator, &out, &.{ 0x1008, 0x1010, 0x1ff8, 0x3000 });
    // Page 0x1000: three entries and a pad; page 0x3000: one and a pad.
    try std.testing.expectEqual(@as(usize, 8 + 8 + 8 + 4), out.items.len);
    try std.testing.expectEqual(@as(u32, 0x1000), std.mem.readInt(u32, out.items[0..4], .little));
    try std.testing.expectEqual(@as(u32, 16), std.mem.readInt(u32, out.items[4..8], .little));
    try std.testing.expectEqual(@as(u16, (IMAGE_REL_BASED_DIR64 << 12) | 0x008), std.mem.readInt(u16, out.items[8..10], .little));
    try std.testing.expectEqual(@as(u16, (IMAGE_REL_BASED_DIR64 << 12) | 0xff8), std.mem.readInt(u16, out.items[12..14], .little));
    try std.testing.expectEqual(@as(u16, 0), std.mem.readInt(u16, out.items[14..16], .little));
    try std.testing.expectEqual(@as(u32, 0x3000), std.mem.readInt(u32, out.items[16..20], .little));
    try std.testing.expectEqual(@as(u32, 12), std.mem.readInt(u32, out.items[20..24], .little));
}

test "function table entries sort by address" {
    var entries: [36]u8 = @splat(0);
    std.mem.writeInt(u32, entries[0..4], 0x3000, .little);
    std.mem.writeInt(u32, entries[12..16], 0x1000, .little);
    std.mem.writeInt(u32, entries[16..20], 7, .little);
    std.mem.writeInt(u32, entries[24..28], 0x2000, .little);
    sortEntries(&entries, 12);
    try std.testing.expectEqual(@as(u32, 0x1000), std.mem.readInt(u32, entries[0..4], .little));
    try std.testing.expectEqual(@as(u32, 7), std.mem.readInt(u32, entries[4..8], .little));
    try std.testing.expectEqual(@as(u32, 0x2000), std.mem.readInt(u32, entries[12..16], .little));
    try std.testing.expectEqual(@as(u32, 0x3000), std.mem.readInt(u32, entries[24..28], .little));
}

test "a procedure window jumps to its procedure" {
    var window: [16]u8 = undefined;
    writeJump(false, &window, 0x1_4000_2000, 0x1_4001_0000);
    try std.testing.expectEqual(@as(u8, 0xe9), window[0]);
    try std.testing.expectEqual(@as(i32, 0x1_0000 - 0x2000 - 5), std.mem.readInt(i32, window[1..5], .little));
    try std.testing.expectEqual(@as(u8, 0xcc), window[5]);
    writeJump(true, &window, 0x1_4000_2000, 0x1_4001_0010);
    // ADRP x16 to page 0x10000 from page 0x2000: 14 pages, immlo 2, immhi 3.
    try std.testing.expectEqual(@as(u32, 0x9000_0010 | (2 << 29) | (3 << 5)), std.mem.readInt(u32, window[0..4], .little));
    try std.testing.expectEqual(@as(u32, 0x9100_0210 | (0x10 << 10)), std.mem.readInt(u32, window[4..8], .little));
    try std.testing.expectEqual(@as(u32, 0xd61f_0200), std.mem.readInt(u32, window[8..12], .little));
}
