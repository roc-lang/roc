//! What a program built from unit images (`backend.dev.UnitImage`) is made
//! of: every image's exports by name, and the atoms the program reaches
//! from its roots. Both consumers of unit images share it: the fast dev
//! link of an executable (`patch_link.zig`) and the run image of a dev run
//! (`run_image_link.zig`).
//!
//! An import resolves first to an export of an image, then to a symbol the
//! host provides. A name several images export resolves to a strong
//! definition over a weak one and, among equals, to the definition in the
//! image with the smallest content digest, whatever order the images
//! arrive in: a procedure's name is its content-derived identity, so two
//! units can each define the same one.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");

const Allocator = std.mem.Allocator;
const UnitImage = backend.dev.UnitImage;

/// One image of a program.
pub const ImageInput = struct {
    view: UnitImage.View,
};

/// A symbol of one image: the image's index and the symbol's index in it.
pub const SymbolRef = struct { image: u32, symbol: u32 };

/// Every export of a program's images, by name.
pub const Exports = std.StringHashMapUnmanaged(SymbolRef);

/// What an import resolves to.
pub const Target = union(enum) {
    app: SymbolRef,
    /// A symbol the host provides, as the host lookup identifies it.
    host: u64,
    /// A symbol the host must provide and does not.
    missing,
};

/// Every export of `images`, each name resolved to its one definition.
pub fn collectExports(gpa: Allocator, images: []const ImageInput) Allocator.Error!Exports {
    var exports: Exports = .empty;
    errdefer exports.deinit(gpa);
    var export_total: u32 = 0;
    for (images) |image| export_total += @intCast(image.view.exports().len);
    try exports.ensureTotalCapacity(gpa, export_total);
    for (images, 0..) |image, image_index| {
        const symbols = image.view.symbols();
        for (image.view.exports()) |symbol_index| {
            const gop = exports.getOrPutAssumeCapacity(image.view.name(symbols[symbol_index].name));
            if (gop.found_existing) {
                const existing = images[gop.value_ptr.image].view;
                const existing_weak = existing.symbols()[gop.value_ptr.symbol].flags & UnitImage.symbol_weak != 0;
                const candidate_weak = symbols[symbol_index].flags & UnitImage.symbol_weak != 0;
                if (existing_weak == candidate_weak) {
                    if (std.mem.order(u8, &existing.header.digest, &image.view.header.digest) != .gt) continue;
                } else if (candidate_weak) continue;
            }
            gop.value_ptr.* = .{ .image = @intCast(image_index), .symbol = symbol_index };
        }
    }
    return exports;
}

/// Finds the symbols the host provides.
pub const HostLookup = struct {
    context: *const anyopaque,
    /// The host's identification of `name`, or null when the host does not
    /// provide it.
    find: *const fn (context: *const anyopaque, name: []const u8) ?u64,
};

/// The atom-level reachability walk over every image: an image is placed
/// when one of its atoms is reached, and every import of a placed image is
/// resolved.
pub const Walk = struct {
    gpa: Allocator,
    images: []const ImageInput,
    exports: *const Exports,
    host: HostLookup,
    /// Symbols the host must provide when the app imports them, sorted: an
    /// import of one the host lacks is the user's missing-host-symbol error,
    /// and an import resolving to nothing else is a compiler invariant.
    host_required: []const []const u8,
    /// Per image, once reached: its reached atoms and its import targets.
    states: []?State = &.{},
    stack: std.ArrayList(struct { image: u32, atom: u32 }) = .empty,
    missing: std.StringArrayHashMapUnmanaged(void) = .empty,

    pub const State = struct {
        atoms: []bool,
        targets: []Target,
    };

    pub fn init(self: *Walk) Allocator.Error!void {
        self.states = try self.gpa.alloc(?State, self.images.len);
        @memset(self.states, null);
    }

    pub fn deinit(self: *Walk) void {
        for (self.states) |state_or_null| if (state_or_null) |state| {
            self.gpa.free(state.atoms);
            self.gpa.free(state.targets);
        };
        self.gpa.free(self.states);
        self.stack.deinit(self.gpa);
        self.missing.deinit(self.gpa);
    }

    /// Reach `image`'s symbol `symbol`, placing the image if it was not.
    pub fn reachSymbol(self: *Walk, image: u32, symbol: u32) Allocator.Error!void {
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
        // Once recorded, the state is the walk's to free; recording it
        // before following the roots lets a cycle of imports find it.
        const state = try self.resolve(view);
        self.states[image] = state;
        for (view.roots()) |entry| try self.follow(image, state, entry);
        return state;
    }

    /// A fresh state for an image: nothing reached, every import resolved.
    fn resolve(self: *Walk, view: UnitImage.View) Allocator.Error!State {
        const atoms = try self.gpa.alloc(bool, view.atoms().len);
        errdefer self.gpa.free(atoms);
        @memset(atoms, false);
        const targets = try self.gpa.alloc(Target, view.imports().len);
        errdefer self.gpa.free(targets);
        for (view.imports(), targets) |import, *target| {
            const name = view.name(import.name);
            target.* = if (self.exports.get(name)) |found|
                .{ .app = found }
            else if (self.host.find(self.host.context, name)) |host|
                .{ .host = host }
            else missing: {
                if (findSorted(self.host_required, name) == null) {
                    base.invariant("unit image link invariant violated: the app imports {s}, which is neither a program symbol nor a symbol the host may define", .{name});
                }
                try self.missing.put(self.gpa, name, {});
                break :missing .missing;
            };
        }
        return .{ .atoms = atoms, .targets = targets };
    }

    /// Follow every reached atom's edges until nothing new is reached.
    pub fn run(self: *Walk) Allocator.Error!void {
        while (self.stack.pop()) |item| {
            const state = self.states[item.image].?;
            for (self.images[item.image].view.atomEdges(item.atom)) |entry| try self.follow(item.image, state, entry);
        }
    }
};

/// The index of `wanted` in the sorted `list`, if it is there.
pub fn findSorted(list: []const []const u8, wanted: []const u8) ?usize {
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
