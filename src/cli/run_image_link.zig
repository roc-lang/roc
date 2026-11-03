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
//! Only the atoms the entrypoints reach are copied; the rest of each block
//! is left zero. Placement is a pure function of the images: the support
//! image first, then the units in content-digest order.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const unit_walk = @import("unit_walk.zig");

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
    /// For each host symbol, its import slot in the image, or `no_slot`
    /// when no placed site references it.
    host_slots: []u32,
    /// For each import slot, its host symbol and the start of its sites.
    slot_symbols: []u32,
    slot_site_starts: []u32,
    /// For each entrypoint ordinal, its wrapper's offset in the code.
    entry_offsets: []u64,
    layout_ns: u64,

    pub fn deinit(self: *Plan) void {
        for (self.states) |state_or_null| if (state_or_null) |state| {
            self.gpa.free(state.atoms);
            self.gpa.free(state.targets);
        };
        self.gpa.free(self.states);
        self.gpa.free(self.images);
        self.gpa.free(self.order);
        self.gpa.free(self.places);
        self.gpa.free(self.host_slots);
        self.gpa.free(self.slot_symbols);
        self.gpa.free(self.slot_site_starts);
        self.gpa.free(self.entry_offsets);
        self.* = undefined;
    }
};

const no_slot: u32 = std.math.maxInt(u32);

/// Where one image's regions go: its block's offset in the code region,
/// and its data's and zero-fill's offsets in the writable region.
const Place = struct {
    block: u64 = 0,
    data: u64 = 0,
    bss: u64 = 0,
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
        .exports = &exports,
        .host = .{ .context = @ptrCast(&inputs.host_symbols), .find = findHostSymbol },
        .host_required = inputs.host_symbols,
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
    std.debug.assert(walk.missing.count() == 0);

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

    // The sites the shim fills: every host site, and every 64-bit address
    // into the image.
    const host_slots = try gpa.alloc(u32, inputs.host_symbols.len);
    errdefer gpa.free(host_slots);
    @memset(host_slots, no_slot);
    const host_counts = try gpa.alloc(u32, inputs.host_symbols.len);
    defer gpa.free(host_counts);
    @memset(host_counts, 0);
    var rebase_count: usize = 0;
    for (order) |index| {
        const view = images[index].view;
        const state = walk.states[index].?;
        for (view.sites()) |site| {
            if (!placedSite(state, site)) continue;
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            if (hostTarget(state, site)) |symbol| {
                switch (kind) {
                    .abs64, .pc32 => host_counts[symbol] += 1,
                    .abs32, .abs32s => base.invariant("run image invariant violated: a unit image holds a 32-bit absolute host address", .{}),
                }
                continue;
            }
            switch (kind) {
                .abs64 => rebase_count += 1,
                .pc32 => {},
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
        host_slots[symbol] = @intCast(slot_symbols.items.len);
        try slot_symbols.append(gpa, @intCast(symbol));
        try slot_site_starts.append(gpa, host_site_count);
        host_site_count += count;
        symbol_names_len += inputs.host_symbols[symbol].len;
    }

    // Displacements between the code and the writable data are 32 bits.
    const span_bound = code_len + slot_symbols.items.len * RunImage.max_jump_stub_size + 2 * std.heap.page_size_max + writable_len;
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

/// Whether `site` is one the placed image keeps: a field in data, or in a
/// reached atom of the block. Sites in debug fragments are never placed.
fn placedSite(state: unit_walk.Walk.State, site: UnitImage.Site) bool {
    const region: UnitImage.Region = @fromBackingInt(site.region);
    if (!region.isAllocated()) return false;
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

    const writable_base: u64 = composed.writable_offset;
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
    }

    const site_cursors = try self.gpa.dupe(u32, self.slot_site_starts);
    defer self.gpa.free(site_cursors);
    var rebase_cursor: usize = 0;
    for (self.order) |index| {
        const view = self.images[index].view;
        const place = self.places[index];
        const state = self.states[index].?;
        for (view.sites()) |site| {
            if (!placedSite(state, site)) continue;
            const kind: UnitImage.SiteKind = @fromBackingInt(site.kind);
            const region: UnitImage.Region = @fromBackingInt(site.region);
            const site_offset = regionBase(place, writable_base, region) + site.offset;
            const field = switch (region) {
                .block => composed.code[@intCast(place.block + site.offset)..][0..@intCast(UnitImage.kindWidth(kind))],
                .data => composed.writable[@intCast(place.data + site.offset)..][0..@intCast(UnitImage.kindWidth(kind))],
                .bss, .debug_line, .debug_info, .debug_abbrev => unreachable, // a placed site is a field with bytes: in the block or the data
            };
            timing.sites_patched += 1;
            const target_offset: u64 = if (site.target_region == UnitImage.import_target) switch (state.targets[site.target]) {
                .app => |found| app: {
                    const symbol = self.images[found.image].view.symbols()[found.symbol];
                    break :app regionBase(self.places[found.image], writable_base, @fromBackingInt(symbol.region)) + symbol.value;
                },
                .host => |symbol| {
                    const slot = self.host_slots[@intCast(symbol)];
                    composed.host_sites[site_cursors[slot]] = .{
                        .offset = site_offset,
                        .addend = site.addend,
                        .kind = @backingInt(switch (kind) {
                            .pc32 => RunImage.HostSiteKind.pc32,
                            .abs64 => RunImage.HostSiteKind.abs64,
                            .abs32, .abs32s => unreachable, // `plan` rejected 32-bit absolute host sites
                        }),
                    };
                    site_cursors[slot] += 1;
                    timing.host_sites += 1;
                    continue;
                },
                .missing => unreachable, // the host lookup finds every symbol the host may define
            } else regionBase(place, writable_base, @fromBackingInt(site.target_region));
            const value = @as(i64, @intCast(target_offset)) + site.addend;
            switch (kind) {
                .pc32 => if (!UnitImage.applySite(field, .pc32, value, site_offset)) {
                    base.invariant("run image invariant violated: a displacement inside the image does not fit 32 bits", .{});
                },
                .abs64 => {
                    std.mem.writeInt(u64, field[0..8], @bitCast(value), .little);
                    composed.rebases[rebase_cursor] = .{ .offset = site_offset, .value = @bitCast(value) };
                    rebase_cursor += 1;
                },
                .abs32, .abs32s => unreachable, // `plan` rejected 32-bit absolute sites
            }
        }
    }
    std.debug.assert(rebase_cursor == composed.rebases.len);
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
fn regionBase(place: Place, writable_base: u64, region: UnitImage.Region) u64 {
    return switch (region) {
        .block => place.block,
        .data => writable_base + place.data,
        .bss => writable_base + place.bss,
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
    std.posix.madvise(pages, end - start, std.posix.MADV.REMOVE) catch return error.ClearFailed;
}

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}
