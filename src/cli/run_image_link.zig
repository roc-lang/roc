//! The run image of a dev run (`backend.RunImage`) composed from unit
//! images (`backend.dev.UnitImage`): the support image and the program's
//! units, placed into the image's code and writable regions at offsets the
//! compiler chooses (design.md "Hot Loading For Default Dev-Shim Runs").
//!
//! The image is used in place in the shared mapping, so every reference
//! whose distance the compiler decides is resolved here: references inside
//! one image were resolved when it was built, and 32-bit displacements
//! between images, or from code to writable data, are filled now. What is
//! left for the machine-code shim is what only the mapping decides: a host
//! symbol (a hosted function, the runtime set, or a builtin the shim
//! defines in place of the support image's), which it resolves once by name
//! per import, and a 64-bit absolute address into the image, which it
//! rebases by adding the image's code address. Its work per image is one
//! pass over those sites.
//!
//! Images built from COFF objects carry Windows unwind tables. The run
//! image holds one function table, the entries of every placed function
//! sorted by function start, and each placed image's unwind information,
//! both after the code in its executable pages. Their image-relative
//! fields count from the start of the code, the base the shim registers
//! the table with, so they are resolved here like any other distance
//! inside the image.
//!
//! Only the atoms the entrypoints reach are copied; the rest of each block
//! is left zero. Placement is a pure function of the images: the support
//! image first, then the units in content-digest order.
const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const backend = @import("backend");
const unit_walk = @import("unit_walk.zig");
const ipc = @import("ipc");

const Allocator = std.mem.Allocator;
const UnitImage = backend.dev.UnitImage;
const RunImage = backend.RunImage;

pub const ImageInput = unit_walk.ImageInput;

/// Everything a run image is composed from.
pub const Inputs = struct {
    /// The builtins and the Boxy runtime.
    support: ImageInput,
    /// Every unit of the program; `root` indexes the root unit.
    units: []const ImageInput,
    root: usize,
    /// The root image's export each entrypoint ordinal calls, by ordinal.
    entrypoints: []const []const u8,
    /// The symbols the shim resolves by name, sorted and distinct: the
    /// platform's hosted symbols, the runtime set and `shim_builtins`.
    host_symbols: []const []const u8,
    /// Builtins the shim defines in place of the support image's, sorted:
    /// its image-tracking builtins, which do nothing outside a hot-reload
    /// session.
    shim_builtins: []const []const u8,
    /// The size of one function table entry of the images' unwind tables:
    /// 12 bytes on x86_64, 8 on arm64.
    function_table_entry_size: u32,
};

/// Where the time went, for `--timings`.
pub const Timing = struct {
    layout_ns: u64 = 0,
    write_ns: u64 = 0,
    images_placed: u64 = 0,
    bytes_written: u64 = 0,
    sites_patched: u64 = 0,
    host_sites: u64 = 0,
    rebases: u64 = 0,
};

/// Why a run image could not be composed.
pub const PlanError = Allocator.Error || error{
    /// The image is too large for 32-bit displacements between its parts.
    ProgramTooLarge,
};

/// A planned composition: which images and atoms are placed where, and the
/// sizes of the image that holds them.
pub const Plan = struct {
    gpa: Allocator,
    images: []ImageInput,
    /// Per image, once placed: its reached atoms and its import targets.
    states: []?unit_walk.Walk.State,
    /// The placed images, in placement order.
    order: []u32,
    places: []Place,
    layout: RunImage.ComposedLayout,
    /// For each host symbol a placed site references, its import slot in
    /// the image. Written for exactly those symbols and read only through
    /// placed sites.
    host_slots: collections.WriteFirstSlice(u32),
    /// For each import slot, its host symbol and the start of its sites.
    slot_symbols: []u32,
    slot_site_starts: []u32,
    /// For each entrypoint ordinal, its wrapper's offset in the code.
    entry_offsets: []u64,
    /// Per image, for each entry of its function table, the entry's slot
    /// in the run image's function table, or `no_slot` for a function the
    /// composition leaves out. Empty for an image without unwind tables.
    table_slots: [][]u32,
    function_table_entry_size: u32,
    layout_ns: u64,

    pub fn deinit(self: *Plan) void {
        for (self.states) |state_or_null| if (state_or_null) |state| {
            self.gpa.free(state.atoms);
            self.gpa.free(state.targets);
            self.gpa.free(state.imports_used);
        };
        self.gpa.free(self.states);
        self.gpa.free(self.images);
        self.gpa.free(self.order);
        self.gpa.free(self.places);
        self.host_slots.deinit(self.gpa);
        self.gpa.free(self.slot_symbols);
        self.gpa.free(self.slot_site_starts);
        self.gpa.free(self.entry_offsets);
        for (self.table_slots) |slots| self.gpa.free(slots);
        self.gpa.free(self.table_slots);
        self.* = undefined;
    }
};

const no_slot: u32 = std.math.maxInt(u32);

/// Where one image's regions go: its block's offset in the code region,
/// its data's and zero-fill's offsets in the writable region, and its
/// unwind information's offset in the run image's unwind information.
const Place = struct {
    block: u64 = 0,
    data: u64 = 0,
    bss: u64 = 0,
    xdata: u64 = 0,
};

/// Where the run image's regions start, counted from the start of its code.
const Bases = struct {
    writable: u64,
    function_table: u64,
    unwind_info: u64,
};

/// Plan the composition of `inputs`: walk the atoms the entrypoints reach,
/// place the images that hold them, and count the sites left for the shim.
pub fn plan(gpa: Allocator, std_io: std.Io, inputs: Inputs) PlanError!Plan {
    const started = now(std_io);
    const images = try gpa.alloc(ImageInput, inputs.units.len + 1);
    var images_owned = true;
    defer if (images_owned) gpa.free(images);
    images[0] = inputs.support;
    @memcpy(images[1..], inputs.units);
    const root_image: u32 = @intCast(inputs.root + 1);

    // The shim's image-tracking builtins replace the support image's, so
    // their names resolve to the host.
    var exports = try unit_walk.collectExports(gpa, images);
    defer exports.deinit(gpa);
    for (inputs.shim_builtins) |name| _ = exports.remove(name);

    var walk: unit_walk.Walk = .{
        .gpa = gpa,
        .images = images,
        .resolver = .{
            .exports = &exports,
            .host = .{ .context = @ptrCast(&inputs.host_symbols), .find = findHostSymbol },
            .host_required = inputs.host_symbols,
        },
    };
    defer walk.deinit();
    try walk.init();

    // Every entrypoint wrapper is an export of the root image: the root
    // unit compiles one for each `provides` procedure.
    const entry_symbols = try gpa.alloc(unit_walk.SymbolRef, inputs.entrypoints.len);
    defer gpa.free(entry_symbols);
    for (inputs.entrypoints, entry_symbols) |name, *found| {
        found.* = exports.get(name) orelse base.invariant("run image invariant violated: the root unit image defines no entrypoint {s}", .{name});
        std.debug.assert(found.image == root_image);
        try walk.reachSymbol(found.image, found.symbol);
    }
    try walk.run();
    // Every import resolves to a program symbol or one the shim resolves.
    std.debug.assert(walk.resolver.missing.count() == 0);

    var order_list: std.ArrayList(u32) = .empty;
    defer order_list.deinit(gpa);
    const support_placed = walk.states[0] != null;
    if (support_placed) try order_list.append(gpa, 0);
    for (1..images.len) |index| if (walk.states[index] != null) try order_list.append(gpa, @intCast(index));
    std.mem.sort(u32, order_list.items[@intFromBool(support_placed)..], images, struct {
        fn lessThan(all: []const ImageInput, a: u32, b: u32) bool {
            return std.mem.order(u8, &all[a].view.header.digest, &all[b].view.header.digest) == .lt;
        }
    }.lessThan);
    const order = order_list.items;

    // Layout: the blocks in the code region, then each image's data and
    // each image's zero-fill in the writable region.
    const places = try gpa.alloc(Place, images.len);
    errdefer gpa.free(places);
    @memset(places, .{});
    var code_len: u64 = 0;
    for (order) |index| {
        const view = images[index].view;
        code_len = std.mem.alignForward(u64, code_len, @max(view.blockAlign(), 16));
        places[index].block = code_len;
        code_len += view.block().len;
    }
    var writable_len: u64 = 0;
    var writable_alignment: u64 = 16;
    for (order) |index| {
        const view = images[index].view;
        if (view.data().len == 0) continue;
        writable_alignment = @max(writable_alignment, view.dataAlign());
        writable_len = std.mem.alignForward(u64, writable_len, @max(view.dataAlign(), 1));
        places[index].data = writable_len;
        writable_len += view.data().len;
    }
    for (order) |index| {
        const view = images[index].view;
        if (view.bssSize() == 0) continue;
        const bss_alignment = @max(view.bssAlign(), 16);
        writable_alignment = @max(writable_alignment, bss_alignment);
        writable_len = std.mem.alignForward(u64, writable_len, bss_alignment);
        places[index].bss = writable_len;
        writable_len += view.bssSize();
    }

    // The unwind tables: one function table holding the entries of the
    // functions placed, and every placed image's unwind information whole.
    // An entry's first field names the start of its function, so the
    // atom that field's site targets decides whether it is kept.
    const entry_size = inputs.function_table_entry_size;
    const table_slots = try gpa.alloc([]u32, images.len);
    @memset(table_slots, &.{});
    errdefer {
        for (table_slots) |slots| gpa.free(slots);
        gpa.free(table_slots);
    }
    var table_entry_count: u32 = 0;
    var unwind_info_len: u64 = 0;
    for (order) |index| {
        const view = images[index].view;
        const state = walk.states[index].?;
        if (view.pdata().len != 0) {
            std.debug.assert(view.pdata().len % entry_size == 0);
            const slots = try gpa.alloc(u32, view.pdata().len / entry_size);
            table_slots[index] = slots;
            @memset(slots, 0);
            for (view.localSites()) |site| {
                if (@as(UnitImage.Region, @fromBackingInt(site.region)) != .pdata or site.offset % entry_size != 0) continue;
                if (@as(UnitImage.Region, @fromBackingInt(site.target_region)) != .block) continue;
                if (!state.atoms[site.target]) slots[site.offset / entry_size] = no_slot;
            }
            for (slots) |*slot| {
                if (slot.* == no_slot) continue;
                slot.* = table_entry_count;
                table_entry_count += 1;
            }
        }
        if (view.xdata().len != 0) {
            unwind_info_len = std.mem.alignForward(u64, unwind_info_len, 16);
            places[index].xdata = unwind_info_len;
            unwind_info_len += view.xdata().len;
        }
    }
    const function_table_len = @as(u64, table_entry_count) * entry_size;

    // The sites the shim fills: every host site, and every 64-bit address
    // into the image.
    var host_slots = try collections.WriteFirstSlice(u32).init(gpa, inputs.host_symbols.len);
    errdefer host_slots.deinit(gpa);
    const host_counts = try gpa.alloc(u32, inputs.host_symbols.len);
    defer gpa.free(host_counts);
    @memset(host_counts, 0);
    var rebase_count: usize = 0;
    for (order) |index| {
        const view = images[index].view;
        const state = walk.states[index].?;
        for (view.sites()) |site| {
            if (!placedSite(state, table_slots[index], entry_size, site)) continue;
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const region: UnitImage.Region = @fromBackingInt(site.region);
            // The table is sorted once written, so no field in it may be
            // left for the shim.
            if (region == .pdata and kind != .rva32) {
                base.invariant("run image invariant violated: a function table entry holds a field other than an image-relative address", .{});
            }
            if (hostTarget(state, site)) |symbol| {
                switch (kind) {
                    .abs64, .pc32, .branch26, .page21, .pageoff12 => host_counts[symbol] += 1,
                    .abs32, .abs32s => base.invariant("run image invariant violated: a unit image holds a 32-bit absolute host address", .{}),
                    // Image-relative fields live only in unwind tables,
                    // which name functions and unwind information of
                    // their own image and no exception handler: the dev
                    // backend writes unwind information without one, and
                    // the support objects are Zig code, which has no
                    // structured exception handling.
                    .rva32 => base.invariant("run image invariant violated: a unit image's unwind tables name a host symbol", .{}),
                }
                continue;
            }
            switch (kind) {
                .abs64 => rebase_count += 1,
                .pc32, .branch26, .page21, .pageoff12 => {},
                // An image-relative address counts from the base its table
                // is registered with, which only unwind tables have.
                .rva32 => if (region != .pdata and region != .xdata) {
                    base.invariant("run image invariant violated: a unit image holds an image-relative address outside its unwind tables", .{});
                },
                .abs32, .abs32s => base.invariant("run image invariant violated: a unit image holds a 32-bit absolute address, which the image's mapping cannot reach", .{}),
            }
        }
    }
    var slot_symbols: std.ArrayList(u32) = .empty;
    errdefer slot_symbols.deinit(gpa);
    var slot_site_starts: std.ArrayList(u32) = .empty;
    errdefer slot_site_starts.deinit(gpa);
    var host_site_count: u32 = 0;
    var symbol_names_len: usize = 0;
    for (host_counts, 0..) |count, symbol| {
        if (count == 0) continue;
        host_slots.set(symbol, @intCast(slot_symbols.items.len));
        try slot_symbols.append(gpa, @intCast(symbol));
        try slot_site_starts.append(gpa, host_site_count);
        host_site_count += count;
        symbol_names_len += inputs.host_symbols[symbol].len;
    }

    // Displacements between the code and the writable data are 32 bits,
    // and so are image-relative addresses. Each part after the code is
    // aligned to at most 16 bytes, and the writable region to a page.
    const span_bound = code_len + 16 + slot_symbols.items.len * RunImage.max_jump_stub_size + 16 + function_table_len + 16 + unwind_info_len + 2 * std.heap.page_size_max + writable_len;
    if (span_bound > std.math.maxInt(i32)) return error.ProgramTooLarge;

    const entry_offsets = try gpa.alloc(u64, inputs.entrypoints.len);
    errdefer gpa.free(entry_offsets);
    for (entry_symbols, entry_offsets) |found, *offset| {
        const symbol = images[found.image].view.symbols()[found.symbol];
        std.debug.assert(@as(UnitImage.Region, @fromBackingInt(symbol.region)) == .block);
        offset.* = places[found.image].block + symbol.value;
    }

    const owned_order = try gpa.dupe(u32, order);
    errdefer gpa.free(owned_order);
    const owned_slot_symbols = try slot_symbols.toOwnedSlice(gpa);
    errdefer gpa.free(owned_slot_symbols);
    const owned_slot_site_starts = try slot_site_starts.toOwnedSlice(gpa);
    // The walk's states move into the plan.
    const states = walk.states;
    walk.states = &.{};
    images_owned = false;
    return .{
        .gpa = gpa,
        .images = images,
        .states = states,
        .order = owned_order,
        .places = places,
        .layout = .{
            .code_len = @intCast(code_len),
            .function_table_len = @intCast(function_table_len),
            .unwind_info_len = @intCast(unwind_info_len),
            .writable_len = @intCast(writable_len),
            .writable_alignment = @intCast(writable_alignment),
            .entrypoint_count = inputs.entrypoints.len,
            .symbol_names_len = symbol_names_len,
            .host_import_count = owned_slot_symbols.len,
            .host_site_count = host_site_count,
            .rebase_count = rebase_count,
        },
        .host_slots = host_slots,
        .slot_symbols = owned_slot_symbols,
        .slot_site_starts = owned_slot_site_starts,
        .entry_offsets = entry_offsets,
        .table_slots = table_slots,
        .function_table_entry_size = entry_size,
        .layout_ns = elapsed(std_io, started),
    };
}

/// The host symbol a site's import resolves to, if it resolves to the host.
fn hostTarget(state: unit_walk.Walk.State, site: UnitImage.Site) ?usize {
    if (site.target_region != UnitImage.import_target) return null;
    return switch (state.targets[site.target]) {
        .host => |symbol| @intCast(symbol),
        .app => null,
        .missing => unreachable, // the host lookup finds every symbol the host may define
    };
}

/// Whether `site` is one the placed image keeps: a field in data, in its
/// unwind information, in a kept entry of its function table (`slots`),
/// or in a reached atom of the block. Sites in debug fragments are never
/// placed.
fn placedSite(state: unit_walk.Walk.State, slots: []const u32, entry_size: u32, site: UnitImage.Site) bool {
    const region: UnitImage.Region = @fromBackingInt(site.region);
    if (!region.isAllocated()) return false;
    if (region == .pdata) return slots[site.offset / entry_size] != no_slot;
    return site.atom == UnitImage.no_atom or state.atoms[site.atom];
}

fn findHostSymbol(context: *const anyopaque, name: []const u8) ?u64 {
    const symbols: *const []const []const u8 = @ptrCast(@alignCast(context));
    return unit_walk.findSorted(symbols.*, name);
}

/// Write the planned composition into `composed`, an image
/// `RunImage.allocComposed` allocated for `self.layout` in a shared
/// mapping. Its code and writable pages are cleared first, so what the
/// composition leaves unwritten reads as zero however the memory was used
/// before.
pub fn write(self: *const Plan, std_io: std.Io, inputs: Inputs, composed: RunImage.Composed, timing: *Timing) (Allocator.Error || error{ClearFailed})!void {
    const started = now(std_io);
    timing.layout_ns = self.layout_ns;
    timing.images_placed = self.order.len;
    try clearPages(composed);

    const code_start = @intFromPtr(composed.code.ptr);
    const bases: Bases = .{
        .writable = composed.writable_offset,
        .function_table = @intFromPtr(composed.function_table.ptr) - code_start,
        .unwind_info = @intFromPtr(composed.unwind_info.ptr) - code_start,
    };
    const entry_size = self.function_table_entry_size;
    for (self.order) |index| {
        const view = self.images[index].view;
        const place = self.places[index];
        const atoms = self.states[index].?.atoms;
        var atom: u32 = 0;
        while (atom < atoms.len) {
            if (!atoms[atom]) {
                atom += 1;
                continue;
            }
            const first = view.atomRange(atom).start;
            while (atom < atoms.len and atoms[atom]) atom += 1;
            const end = view.atomRange(atom - 1).end;
            @memcpy(composed.code[@intCast(place.block + first)..][0 .. end - first], view.block()[first..end]);
            timing.bytes_written += end - first;
        }
        if (view.data().len != 0) {
            @memcpy(composed.writable[@intCast(place.data)..][0..view.data().len], view.data());
            timing.bytes_written += view.data().len;
        }
        for (self.table_slots[index], 0..) |slot, entry| {
            if (slot == no_slot) continue;
            @memcpy(composed.function_table[slot * entry_size ..][0..entry_size], view.pdata()[entry * entry_size ..][0..entry_size]);
            timing.bytes_written += entry_size;
        }
        if (view.xdata().len != 0) {
            @memcpy(composed.unwind_info[@intCast(place.xdata)..][0..view.xdata().len], view.xdata());
            timing.bytes_written += view.xdata().len;
        }
    }

    const site_cursors = try self.gpa.dupe(u32, self.slot_site_starts);
    defer self.gpa.free(site_cursors);
    var rebase_cursor: usize = 0;
    for (self.order) |index| {
        const view = self.images[index].view;
        const place = self.places[index];
        const state = self.states[index].?;
        const slots = self.table_slots[index];
        for (view.sites()) |site| {
            if (!placedSite(state, slots, entry_size, site)) continue;
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const width: usize = @intCast(UnitImage.kindWidth(kind));
            // The field's offset in its part of the image, and that part.
            const in_part: u64, const part: []u8, const part_base: u64 = switch (region) {
                .block => .{ place.block + site.offset, composed.code, 0 },
                .data => .{ place.data + site.offset, composed.writable, bases.writable },
                .pdata => .{ @as(u64, slots[site.offset / entry_size]) * entry_size + site.offset % entry_size, composed.function_table, bases.function_table },
                .xdata => .{ place.xdata + site.offset, composed.unwind_info, bases.unwind_info },
                .bss, .debug_line, .debug_info, .debug_abbrev => unreachable, // a placed site is a field with bytes: in the block, the data or the unwind tables
            };
            const site_offset = part_base + in_part;
            const field = part[@intCast(in_part)..][0..width];
            timing.sites_patched += 1;
            const target_offset: u64 = if (site.target_region == UnitImage.import_target) switch (state.targets[site.target]) {
                .app => |found| app: {
                    const symbol = self.images[found.image].view.symbols()[found.symbol];
                    break :app regionBase(self.places[found.image], bases, @fromBackingInt(symbol.region)) + symbol.value;
                },
                .host => |symbol| {
                    const slot = self.host_slots.get(@intCast(symbol));
                    composed.host_sites[site_cursors[slot]] = .{
                        .offset = site_offset,
                        .addend = site.addend,
                        .kind = @backingInt(switch (kind) {
                            .pc32 => RunImage.HostSiteKind.pc32,
                            .abs64 => RunImage.HostSiteKind.abs64,
                            .branch26 => RunImage.HostSiteKind.branch26,
                            .page21 => RunImage.HostSiteKind.page21,
                            .pageoff12 => RunImage.HostSiteKind.pageoff12,
                            .abs32, .abs32s, .rva32 => unreachable, // `plan` rejected every other host site kind
                        }),
                    };
                    site_cursors[slot] += 1;
                    timing.host_sites += 1;
                    continue;
                },
                .missing => unreachable, // the host lookup finds every symbol the host may define
            } else regionBase(place, bases, @fromBackingInt(site.target_region));
            const value = @as(i64, @intCast(target_offset)) + site.addend;
            switch (kind) {
                // The image's code and data lie within 32 bits of each
                // other, and the code is page aligned, so offsets from its
                // start relocate like addresses.
                .pc32, .branch26, .page21, .pageoff12 => if (!UnitImage.applySite(field, kind, value, site_offset)) {
                    base.invariant("run image invariant violated: a displacement inside the image does not fit its field", .{});
                },
                // The function table is registered with the start of the
                // code as its base, so an offset from it is the field.
                .rva32 => if (!UnitImage.applySite(field, kind, value, site_offset)) {
                    base.invariant("run image invariant violated: an image-relative address does not fit its field", .{});
                },
                .abs64 => {
                    std.mem.writeInt(u64, field[0..8], @bitCast(value), .little);
                    composed.rebases[rebase_cursor] = .{ .offset = site_offset, .value = @bitCast(value) };
                    rebase_cursor += 1;
                },
                .abs32, .abs32s => unreachable, // `plan` rejected every other site kind
            }
        }
    }
    std.debug.assert(rebase_cursor == composed.rebases.len);
    // An image's table need not list its functions in address order, so
    // the run image's table is sorted once its addresses are filled.
    UnitImage.sortFunctionTable(composed.function_table, entry_size);
    timing.rebases = rebase_cursor;

    var name_cursor: usize = 0;
    for (self.slot_symbols, composed.host_imports, 0..) |symbol, *import, slot| {
        const name = inputs.host_symbols[symbol];
        @memcpy(composed.symbol_names[name_cursor..][0..name.len], name);
        const first = self.slot_site_starts[slot];
        import.* = .{
            .symbol = .{ .offset = name_cursor, .len = name.len },
            .first_site = first,
            .site_count = site_cursors[slot] - first,
        };
        name_cursor += name.len;
    }
    for (composed.entrypoints, self.entry_offsets, 0..) |*entrypoint, offset, ordinal| {
        entrypoint.* = .{ .ordinal = @intCast(ordinal), .code_offset = offset };
    }
    timing.write_ns = elapsed(std_io, started);
}

/// The offset from the start of the image's code of an image's region.
fn regionBase(place: Place, bases: Bases, region: UnitImage.Region) u64 {
    return switch (region) {
        .block => place.block,
        .data => bases.writable + place.data,
        .bss => bases.writable + place.bss,
        .xdata => bases.unwind_info + place.xdata,
        // Only the unwinder reads a function table: no symbol is defined in
        // one and no field refers into one.
        .pdata => base.invariant("run image invariant violated: a unit image refers into its function table", .{}),
        .debug_line, .debug_info, .debug_abbrev => unreachable, // debug fragments are never placed in a run image
    };
}

/// Give the image's code and writable pages back to the shared mapping's
/// file, which reads them as zero until written.
fn clearPages(composed: RunImage.Composed) error{ClearFailed}!void {
    const page = std.heap.pageSize();
    const start = std.mem.alignBackward(usize, @intFromPtr(composed.code.ptr), page);
    const end_address = if (composed.writable.len != 0)
        @intFromPtr(composed.writable.ptr) + composed.writable.len
    else
        @intFromPtr(composed.code.ptr) + composed.code.len;
    const end = std.mem.alignForward(usize, end_address, page);
    const pages: [*]align(std.heap.page_size_min) u8 = @ptrFromInt(start);
    // Linux punches the pages out of the mapping's file; elsewhere they are
    // written as zeros.
    if (comptime builtin.os.tag == .linux) {
        std.posix.madvise(pages, end - start, std.posix.MADV.REMOVE) catch return error.ClearFailed;
    } else {
        @memset(pages[0 .. end - start], 0);
    }
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

const TestCoffObject = UnitImage.TestCoffObject;
const coff_text_flags: u32 = 0x6050_0020;
const coff_unwind_flags: u32 = 0x4030_0040;
const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
const IMAGE_REL_AMD64_ADDR32NB: u16 = 0x0003;
const IMAGE_REL_AMD64_REL32: u16 = 0x0004;

/// A COFF x86_64 object of `text` with a function table of `functions`,
/// each a symbol's start, end and unwind information offset, all named
/// through the symbols at `function_symbols` and the `.xdata` section
/// symbol at `xdata_symbol`.
fn testCoffImage(
    allocator: Allocator,
    text: []const u8,
    xdata: []const u8,
    functions: []const struct { symbol: u32, end: u32, xdata: u32 },
    symbols: []const TestCoffObject.TestSymbol,
    calls: []const TestCoffObject.TestRelocation,
    xdata_symbol: u32,
) UnitImage.BuildError![]align(UnitImage.alignment.toByteUnits()) u8 {
    const pdata = try allocator.alloc(u8, functions.len * 12);
    defer allocator.free(pdata);
    @memset(pdata, 0);
    var relocations: std.ArrayList(TestCoffObject.TestRelocation) = .empty;
    defer relocations.deinit(allocator);
    try relocations.appendSlice(allocator, calls);
    for (functions, 0..) |function, entry| {
        const at: u32 = @intCast(entry * 12);
        std.mem.writeInt(u32, pdata[at + 4 ..][0..4], function.end, .little);
        std.mem.writeInt(u32, pdata[at + 8 ..][0..4], function.xdata, .little);
        try relocations.appendSlice(allocator, &.{
            .{ .section = 2, .offset = at, .symbol = function.symbol, .type = IMAGE_REL_AMD64_ADDR32NB },
            .{ .section = 2, .offset = at + 4, .symbol = function.symbol, .type = IMAGE_REL_AMD64_ADDR32NB },
            .{ .section = 2, .offset = at + 8, .symbol = xdata_symbol, .type = IMAGE_REL_AMD64_ADDR32NB },
        });
    }
    const object = try TestCoffObject.encode(allocator, IMAGE_FILE_MACHINE_AMD64, &.{
        .{ .name = ".text", .bytes = text, .characteristics = coff_text_flags },
        .{ .name = ".pdata", .bytes = pdata, .characteristics = coff_unwind_flags },
        .{ .name = ".xdata", .bytes = xdata, .characteristics = coff_unwind_flags },
    }, symbols, relocations.items);
    defer allocator.free(object);
    return UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .{ .symbols = .{ .edges = &.{}, .data_starts = &.{} } } });
}

test "a run image composed from COFF images carries the placed functions' unwind tables" {
    const allocator = std.testing.allocator;
    // The support image: `answer`, which nothing calls, `call_ext`, which
    // calls the host's `ext`, and `seven`. Its function table lists them
    // out of address order.
    const support_text = [_]u8{
        0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3, 0x90, 0x90, // answer: mov eax, 42; ret
        0x48, 0x83, 0xec, 0x28, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x48, 0x83, 0xc4, 0x28, 0xc3, 0x90, 0x90, // call_ext: sub rsp, 40; call ext; add rsp, 40; ret
        0xb8, 0x07, 0x00, 0x00, 0x00, 0xc3, 0x90, 0x90, // seven: mov eax, 7; ret
    };
    // A leaf's unwind information, then one small allocation of 40 bytes.
    const support_xdata = [_]u8{ 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00 };
    const support = try testCoffImage(allocator, &support_text, &support_xdata, &.{
        .{ .symbol = 2, .end = 6, .xdata = 0 },
        .{ .symbol = 0, .end = 6, .xdata = 0 },
        .{ .symbol = 1, .end = 14, .xdata = 4 },
    }, &.{
        .{ .name = "answer", .section = 1, .value = 0, .function = true },
        .{ .name = "call_ext", .section = 1, .value = 8, .function = true },
        .{ .name = "seven", .section = 1, .value = 24, .function = true },
        .{ .name = "ext", .section = 0 },
        .{ .name = ".xdata", .section = 3, .class = .section },
    }, &.{
        .{ .section = 1, .offset = 13, .symbol = 3, .type = IMAGE_REL_AMD64_REL32 },
    }, 4);
    defer allocator.free(support);
    // The root unit: `entry`, which calls `call_ext` and `seven`.
    const root_text = [_]u8{
        0x48, 0x83, 0xec, 0x28, // sub rsp, 40
        0xe8, 0x00, 0x00, 0x00, 0x00, // call call_ext
        0xe8, 0x00, 0x00, 0x00, 0x00, // call seven
        0x48, 0x83, 0xc4, 0x28, 0xc3, 0x90, // add rsp, 40; ret
    };
    const root_xdata = [_]u8{ 0x01, 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00 };
    const root = try testCoffImage(allocator, &root_text, &root_xdata, &.{
        .{ .symbol = 0, .end = 19, .xdata = 0 },
    }, &.{
        .{ .name = "entry", .section = 1, .value = 0, .function = true },
        .{ .name = "call_ext", .section = 0 },
        .{ .name = "seven", .section = 0 },
        .{ .name = ".xdata", .section = 3, .class = .section },
    }, &.{
        .{ .section = 1, .offset = 5, .symbol = 1, .type = IMAGE_REL_AMD64_REL32 },
        .{ .section = 1, .offset = 10, .symbol = 2, .type = IMAGE_REL_AMD64_REL32 },
    }, 3);
    defer allocator.free(root);

    const units = [_]ImageInput{.{ .view = try UnitImage.View.init(root) }};
    const inputs: Inputs = .{
        .support = .{ .view = try UnitImage.View.init(support) },
        .units = &units,
        .root = 0,
        .entrypoints = &.{"entry"},
        .host_symbols = &.{"ext"},
        .shim_builtins = &.{},
        .function_table_entry_size = 12,
    };
    var composition = try plan(allocator, std.testing.io, inputs);
    defer composition.deinit();
    // Two of the support image's three functions and the root's one.
    try std.testing.expectEqual(@as(usize, 3 * 12), composition.layout.function_table_len);
    try std.testing.expectEqual(@as(usize, 1), composition.layout.host_import_count);

    const page_size = std.heap.pageSize();
    var shm = try ipc.SharedMemoryAllocator.createExecutable(std.testing.io, 1 << 20, page_size);
    defer shm.deinit(allocator);
    const composed = try RunImage.allocComposed(shm.allocator(), shm.base_ptr, page_size, composition.layout);
    var timing: Timing = .{};
    try write(&composition, std.testing.io, inputs, composed, &timing);

    const view = try RunImage.viewMappedImage(composed.header, shm.base_ptr, @intCast(composed.header.image_size), shm.total_size);
    try std.testing.expectEqual(composed.function_table.ptr, view.function_table.ptr);
    try std.testing.expectEqual(composed.function_table.len, view.function_table.len);
    const code_start = @intFromPtr(composed.code.ptr);
    const unwind_info = @intFromPtr(composed.unwind_info.ptr) - code_start;
    const support_place = composition.places[0];
    const root_place = composition.places[1];
    try std.testing.expectEqualSlices(u8, &support_xdata, composed.unwind_info[@intCast(support_place.xdata)..][0..support_xdata.len]);
    try std.testing.expectEqualSlices(u8, &root_xdata, composed.unwind_info[@intCast(root_place.xdata)..][0..root_xdata.len]);
    // Sorted by function start, every address counted from the code.
    const Entry = struct { start: u64, end: u64, unwind: u64 };
    const expected = [_]Entry{
        .{ .start = support_place.block + 8, .end = support_place.block + 22, .unwind = unwind_info + support_place.xdata + 4 },
        .{ .start = support_place.block + 24, .end = support_place.block + 30, .unwind = unwind_info + support_place.xdata },
        .{ .start = root_place.block, .end = root_place.block + 19, .unwind = unwind_info + root_place.xdata },
    };
    try std.testing.expect(support_place.block < root_place.block);
    for (expected, 0..) |entry, index| {
        const fields = composed.function_table[index * 12 ..][0..12];
        try std.testing.expectEqual(entry.start, std.mem.readInt(u32, fields[0..4], .little));
        try std.testing.expectEqual(entry.end, std.mem.readInt(u32, fields[4..8], .little));
        try std.testing.expectEqual(entry.unwind, std.mem.readInt(u32, fields[8..12], .little));
    }
    // The table and the unwind information sit in the executable pages,
    // after the host jump stubs.
    try std.testing.expect(@intFromPtr(composed.function_table.ptr) >= @intFromPtr(view.function_stubs.ptr) + view.function_stubs.len);
    try std.testing.expect(@intFromPtr(composed.unwind_info.ptr) + composed.unwind_info.len <= @intFromPtr(view.executable.ptr) + view.executable.len);
    try std.testing.expectEqual(@as(usize, 1), composed.host_sites.len);
}
