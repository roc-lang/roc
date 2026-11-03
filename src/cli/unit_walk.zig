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

/// One export of an image, as `collectExportsOf` reads it.
pub const ExportEntry = struct {
    name: []const u8,
    weak: bool,
    /// The symbol a `SymbolRef` names it by.
    symbol: u32,
};

/// Every export of `images`, each name resolved to its one definition.
pub fn collectExports(gpa: Allocator, images: []const ImageInput) Allocator.Error!Exports {
    return collectExportsOf(Native, gpa, images);
}

/// Every export of `images`, images of the kind `Kind` describes (see
/// `AtomWalk`), each name resolved to its one definition: a strong one over
/// a weak one, then the one in the image with the smallest digest.
pub fn collectExportsOf(comptime Kind: type, gpa: Allocator, images: []const Kind.Image) Allocator.Error!Exports {
    var exports: Exports = .empty;
    errdefer exports.deinit(gpa);
    var export_total: u32 = 0;
    for (images) |image| export_total += Kind.exportCount(image);
    try exports.ensureTotalCapacity(gpa, export_total);
    for (images, 0..) |image, image_index| {
        for (0..Kind.exportCount(image)) |at| {
            const entry = Kind.exportAt(image, @intCast(at));
            if (entry.name.len == 0) continue;
            const gop = exports.getOrPutAssumeCapacity(entry.name);
            if (gop.found_existing) {
                const existing = images[gop.value_ptr.image];
                const existing_weak = Kind.exportWeak(existing, gop.value_ptr.symbol);
                if (existing_weak == entry.weak) {
                    if (std.mem.order(u8, Kind.digest(existing), Kind.digest(image)) != .gt) continue;
                } else if (entry.weak) continue;
            }
            gop.value_ptr.* = .{ .image = @intCast(image_index), .symbol = entry.symbol };
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

/// What following an edge of an atom reaches.
pub const Edge = union(enum) {
    /// Another atom of the same image.
    atom: u32,
    /// An import of the same image, by index.
    import: u32,
    /// Nothing a walk follows.
    none,
};

/// The atom-level reachability walk over every image of a program, for
/// the image kind `Kind` describes: an image is placed when one of its
/// atoms is reached, every import of a placed image is resolved by
/// `Kind.Resolver`, and an import resolving to another image's symbol
/// reaches that symbol's atom. `Kind` declares:
///
/// - `Image`, `Target` and `Resolver` types, where `Resolver` has
///   `fn resolve(*Resolver, gpa, images, image_index, import_index) !Target`
///   and may have `fn deinit(*Resolver, gpa)`;
/// - `importCount(Image)`, `atomCount(Image)`, `roots(Image)` with
///   `rootEdge(Image, item) Edge` for its items, and `atomEdges(Image, atom)`
///   with `edge(Image, item) Edge` for its items;
/// - `symbolAtom(Image, symbol) ?u32` and `reference(Target) ?SymbolRef`;
/// - for `collectExportsOf`: `exportCount(Image)`, `exportAt(Image, at)
///   ExportEntry`, `exportWeak(Image, symbol)` and `digest(Image)`.
pub fn AtomWalk(comptime Kind: type) type {
    return struct {
        const Self = @This();

        gpa: Allocator,
        images: []const Kind.Image,
        resolver: Kind.Resolver,
        /// Per image, once reached: its reached atoms and its import targets.
        states: []?State = &.{},
        stack: std.ArrayList(struct { image: u32, atom: u32 }) = .empty,

        pub const State = struct {
            atoms: []bool,
            targets: []Kind.Target,
            /// Which imports an edge of a reached atom names.
            imports_used: []bool,
        };

        pub fn init(self: *Self) Allocator.Error!void {
            self.states = try self.gpa.alloc(?State, self.images.len);
            @memset(self.states, null);
        }

        pub fn deinit(self: *Self) void {
            for (self.states) |state_or_null| if (state_or_null) |state| {
                self.gpa.free(state.atoms);
                self.gpa.free(state.targets);
                self.gpa.free(state.imports_used);
            };
            self.gpa.free(self.states);
            self.stack.deinit(self.gpa);
            if (@hasDecl(Kind.Resolver, "deinit")) self.resolver.deinit(self.gpa);
        }

        /// Reach `image`'s symbol `symbol`, placing the image if it was not.
        pub fn reachSymbol(self: *Self, image: u32, symbol: u32) Allocator.Error!void {
            const state = try self.place(image);
            const atom = Kind.symbolAtom(self.images[image], symbol) orelse return;
            try self.reachAtom(image, state, atom);
        }

        /// Reach `image`'s atom `atom`, placing the image if it was not.
        pub fn reachImageAtom(self: *Self, image: u32, atom: u32) Allocator.Error!void {
            try self.reachAtom(image, try self.place(image), atom);
        }

        fn reachAtom(self: *Self, image: u32, state: State, atom: u32) Allocator.Error!void {
            if (state.atoms[atom]) return;
            state.atoms[atom] = true;
            try self.stack.append(self.gpa, .{ .image = image, .atom = atom });
        }

        fn follow(self: *Self, image: u32, state: State, edge: Edge) Allocator.Error!void {
            switch (edge) {
                .atom => |atom| try self.reachAtom(image, state, atom),
                .import => |import| {
                    state.imports_used[import] = true;
                    if (Kind.reference(state.targets[import])) |found| try self.reachSymbol(found.image, found.symbol);
                },
                .none => {},
            }
        }

        /// `image`'s state, created with its imports resolved and its roots
        /// reached the first time.
        pub fn place(self: *Self, image: u32) Allocator.Error!State {
            if (self.states[image]) |state| return state;
            const view = self.images[image];
            const atoms = try self.gpa.alloc(bool, Kind.atomCount(view));
            errdefer self.gpa.free(atoms);
            @memset(atoms, false);
            const targets = try self.gpa.alloc(Kind.Target, Kind.importCount(view));
            errdefer self.gpa.free(targets);
            const imports_used = try self.gpa.alloc(bool, targets.len);
            errdefer self.gpa.free(imports_used);
            @memset(imports_used, false);
            for (targets, 0..) |*target, import| target.* = try self.resolver.resolve(self.gpa, self.images, image, @intCast(import));
            // Once recorded, the state is the walk's to free; recording it
            // before following the roots lets a cycle of imports find it.
            const state: State = .{ .atoms = atoms, .targets = targets, .imports_used = imports_used };
            self.states[image] = state;
            for (Kind.roots(view)) |item| try self.follow(image, state, Kind.rootEdge(view, item));
            return state;
        }

        /// Follow every reached atom's edges until nothing new is reached.
        pub fn run(self: *Self) Allocator.Error!void {
            while (self.stack.pop()) |item| {
                const state = self.states[item.image].?;
                const view = self.images[item.image];
                for (Kind.atomEdges(view, item.atom)) |entry| try self.follow(item.image, state, Kind.edge(view, entry));
            }
        }
    };
}

/// How native unit images (`backend.dev.UnitImage`) are walked: an import
/// resolves first to an export of an image, then to a symbol the host
/// provides.
pub const Native = struct {
    pub const Image = ImageInput;
    pub const Target = unit_walk_Target;

    /// Resolves a native image's imports.
    pub const Resolver = struct {
        exports: *const Exports,
        host: HostLookup,
        /// Symbols the host must provide when the app imports them, sorted:
        /// an import of one the host lacks is the user's missing-host-symbol
        /// error, and an import resolving to nothing else is a compiler
        /// invariant.
        host_required: []const []const u8,
        missing: std.StringArrayHashMapUnmanaged(void) = .empty,

        pub fn resolve(self: *Resolver, gpa: Allocator, images: []const Image, image: u32, import_index: u32) Allocator.Error!unit_walk_Target {
            const view = images[image].view;
            const name = view.name(view.imports()[import_index].name);
            if (self.exports.get(name)) |found| return .{ .app = found };
            if (self.host.find(self.host.context, name)) |host| return .{ .host = host };
            if (findSorted(self.host_required, name) == null) {
                base.invariant("unit image link invariant violated: the app imports {s}, which is neither a program symbol nor a symbol the host may define", .{name});
            }
            try self.missing.put(gpa, name, {});
            return .missing;
        }

        pub fn deinit(self: *Resolver, gpa: Allocator) void {
            self.missing.deinit(gpa);
        }
    };

    pub fn importCount(image: Image) usize {
        return image.view.imports().len;
    }
    pub fn atomCount(image: Image) usize {
        return image.view.atoms().len;
    }
    pub fn roots(image: Image) []const u32 {
        return image.view.roots();
    }
    pub fn atomEdges(image: Image, atom: u32) []const u32 {
        return image.view.atomEdges(atom);
    }
    pub fn edge(_: Image, entry: u32) Edge {
        if (entry & UnitImage.edge_import == 0) return .{ .atom = entry };
        return .{ .import = entry & ~UnitImage.edge_import };
    }
    pub fn rootEdge(image: Image, entry: u32) Edge {
        return edge(image, entry);
    }
    pub fn symbolAtom(image: Image, symbol: u32) ?u32 {
        const atom = image.view.symbolAtoms()[symbol];
        return if (atom == UnitImage.no_atom) null else atom;
    }
    pub fn reference(target: unit_walk_Target) ?SymbolRef {
        return switch (target) {
            .app => |found| found,
            .host, .missing => null,
        };
    }
    pub fn exportCount(image: Image) u32 {
        return @intCast(image.view.exports().len);
    }
    pub fn exportAt(image: Image, at: u32) ExportEntry {
        const symbol_index = image.view.exports()[at];
        const symbol = image.view.symbols()[symbol_index];
        return .{ .name = image.view.name(symbol.name), .weak = symbol.flags & UnitImage.symbol_weak != 0, .symbol = symbol_index };
    }
    pub fn exportWeak(image: Image, symbol: u32) bool {
        return image.view.symbols()[symbol].flags & UnitImage.symbol_weak != 0;
    }
    pub fn digest(image: Image) []const u8 {
        return &image.view.header.digest;
    }
};

const unit_walk_Target = Target;

/// The walk over native unit images.
pub const Walk = AtomWalk(Native);

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
