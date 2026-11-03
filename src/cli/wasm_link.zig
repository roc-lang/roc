//! The wasm fast dev link: `roc build --opt=dev --specialize=no` of a
//! `wasm32` or `wasm32v1` program, linked by composing images
//! (`backend.wasm.WasmUnitImage`) instead of running LLD (plan "Wasm fast
//! dev link"). Its inputs are the program's unit images, the image of the
//! compiler's support code, and, for a finished module, the image of the
//! platform's host (`wasm_host_image.zig`).
//!
//! Two outputs are composed from the same walk:
//!
//! - A finished module (`composeModule`), for the `exe` and `shared`
//!   outputs: index spaces, memory, the table and every section are laid
//!   out before a byte is written, then every reached body and segment is
//!   copied into the output and its sites patched in place, in parallel
//!   tasks on the post-check executor. Every write lands in the writing
//!   task's own range of the output.
//! - A relocatable object (`composeObject`), for the `archive` output: the
//!   reached bodies and segments with every site a relocation, and a
//!   symbol table in which only the names the app gives the host are
//!   global.
//!
//! Names resolve in two scopes, as they do when LLD links the sealed Roc
//! object with the host: an import of a unit or the support code resolves
//! to the app's own definitions first (a strong one over a weak one, then
//! the one in the image with the smallest digest), then to a symbol the
//! link defines (`__stack_pointer`, `__stack_low`, ...), then to the host's
//! definitions, and is otherwise left to the embedder. An import of the
//! host resolves only to the names the app gives the host, then to the
//! link's own symbols, then to the embedder.
//!
//! Everything the composer decides is a function of the images and the
//! platform's configuration: placement order by content digest, index
//! spaces by walk order, memory by alignment and order. The worker count
//! only changes which task writes which range.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const layout = @import("layout");
const unit_walk = @import("unit_walk.zig");
const patch_link = @import("patch_link.zig");
const linker = @import("linker.zig");

const Allocator = std.mem.Allocator;
const WasmUnitImage = backend.wasm.WasmUnitImage;
const WasmModule = backend.wasm.WasmModule;
const StackRoom = backend.wasm.StackRoom;
const View = WasmUnitImage.View;
const Executor = base.post_check_task_executor.Executor;
const SymbolRef = unit_walk.SymbolRef;

/// The output a finished module is written to.
pub const OutputFile = patch_link.OutputFile;

/// What a link reads.
pub const Inputs = struct {
    /// The platform's host; null for a relocatable object, which the
    /// platform's own linker joins with its host.
    host: ?View,
    /// The compiler's support code.
    support: View,
    /// Every unit of the program; `root` indexes the root unit.
    units: []const View,
    root: usize,
    /// The names the app defines for the host, sorted and distinct: the
    /// entry wrappers of its provides, the data it exports, and the stack
    /// pointer accessors of a WebAssembly 1.0 module.
    app_public: []const []const u8,
    /// The platform's `provides` symbols.
    provides: []const []const u8,
    /// The platform's hosted symbols, sorted and distinct.
    hosted: []const []const u8,
};

/// What a finished module is configured with: the platform's link
/// configuration and the CLI's.
pub const ModuleConfig = struct {
    /// The platform's `exports:`.
    exports: []const []const u8,
    /// The names a host reads the stack bounds through (`linker.wasmStackExports`).
    stack_exports: []const []const u8,
    stack_size: u64,
    /// Null for a stack-first layout (`linker.wasmStackLayout`).
    global_base: ?u32,
    initial_memory: ?u64,
    maximum_memory: ?u64,
    import_memory: bool,
    /// Whether memory starts zeroed, so zero-filled data needs no bytes.
    zero_filled_memory: bool,
    /// Whether to emit a name section (`--debug`).
    names: bool,
};

/// Where the time went, for `--timings`.
pub const Timing = struct {
    walk_ns: u64 = 0,
    layout_ns: u64 = 0,
    write_ns: u64 = 0,
    finalize_ns: u64 = 0,
    images_placed: u64 = 0,
    functions: u64 = 0,
    bytes_written: u64 = 0,
    sites_patched: u64 = 0,
};

/// An engine limit a module exceeds.
pub const Limit = struct {
    what: []const u8,
    count: u64,
    limit: u64,
};

/// A platform boundary function with more parameters than an engine loads.
pub const ParameterProblem = struct {
    symbol: []const u8,
    hosted: bool,
    param_count: u32,
};

/// A memory configuration the program does not fit.
pub const MemoryProblem = struct {
    what: enum { initial_memory_too_small, maximum_memory_too_small, memory_not_page_aligned, stack_size_not_aligned, memory_too_large },
    needed: u64,
    configured: u64,
};

/// What a failed link reports beyond its error, allocated from the arena,
/// which outlives the images and the platform's configuration.
pub const Outcome = struct {
    missing: []const []const u8 = &.{},
    undefined_exports: []const []const u8 = &.{},
    conflict: ?[]const u8 = null,
    limit: ?Limit = null,
    parameters: []const ParameterProblem = &.{},
    memory: ?MemoryProblem = null,
};

/// Why a link failed.
pub const LinkError = Allocator.Error || OutputFile.Error || error{
    /// A data symbol the program uses is defined nowhere: `Outcome.missing`.
    MissingHostSymbols,
    /// The platform exports names nothing defines: `Outcome.undefined_exports`.
    UndefinedExports,
    /// Two images import one embedder function with different signatures:
    /// `Outcome.conflict`.
    ImportConflict,
    /// The module exceeds an engine limit: `Outcome.limit`.
    EngineLimit,
    /// A boundary function has too many parameters: `Outcome.parameters`.
    ParameterLimit,
    /// The memory configuration does not fit the program: `Outcome.memory`.
    MemoryLayout,
};

/// The engine limits a module must stay within (the JavaScript embedding's,
/// which every engine of note shares).
const max_functions = 1_000_000;
const max_imports = 100_000;
const max_exports = 100_000;
const max_types = 1_000_000;
const max_globals = 1_000_000;
const max_data_segments = 100_000;
const max_table_size = 10_000_000;
const max_body_size = 7_654_321;
const page_size: u64 = 64 * 1024;
const memory_limit: u64 = 1 << 32;
const stack_alignment: u64 = 16;

const none: u32 = std.math.maxInt(u32);

/// One image of the walk and the scope its imports resolve in.
const Item = struct {
    view: View,
    side: Side,
};

const Side = enum { host, app };

/// A symbol the link itself defines.
const Synthetic = enum {
    stack_pointer,
    memory_base,
    table_base,
    indirect_function_table,
    stack_low,
    stack_high,
    heap_base,
    heap_end,
    data_end,
    global_base,
    dso_handle,
    call_ctors,

    fn of(name: []const u8, kind: WasmUnitImage.ImportKind) ?Synthetic {
        const table = [_]struct { []const u8, WasmUnitImage.ImportKind, Synthetic }{
            .{ "__stack_pointer", .global, .stack_pointer },
            .{ "__memory_base", .global, .memory_base },
            .{ "__table_base", .global, .table_base },
            .{ "__indirect_function_table", .table, .indirect_function_table },
            .{ "__stack_low", .data, .stack_low },
            .{ "__stack_high", .data, .stack_high },
            .{ "__heap_base", .data, .heap_base },
            .{ "__heap_end", .data, .heap_end },
            .{ "__data_end", .data, .data_end },
            .{ "__global_base", .data, .global_base },
            .{ "__dso_handle", .data, .dso_handle },
            .{ "__wasm_call_ctors", .function, .call_ctors },
        };
        for (table) |entry| if (entry[1] == kind and std.mem.eql(u8, entry[0], name)) return entry[2];
        return null;
    }

    fn named(name: []const u8) ?Synthetic {
        inline for (.{ WasmUnitImage.ImportKind.function, .global, .table, .data }) |kind| {
            if (of(name, kind)) |found| return found;
        }
        return null;
    }
};

/// What an import resolves to.
const Resolved = union(enum) {
    /// A definition of an image.
    defined: SymbolRef,
    synthetic: Synthetic,
    /// Left to the embedder (a module) or undefined (an object).
    external,
    /// A data symbol nothing defines.
    missing,
};

/// How images are walked (`unit_walk.AtomWalk`). An image's symbols are
/// numbered: its function exports, then its data exports, then its globals.
const Kind = struct {
    pub const Image = Item;
    pub const Target = Resolved;
    pub const Resolver = Resolving;

    pub fn importCount(image: Image) usize {
        return image.view.imports().len;
    }
    pub fn atomCount(image: Image) usize {
        return image.view.functions().len + image.view.segments().len;
    }
    pub fn roots(image: Image) []const u32 {
        return image.view.roots();
    }
    pub fn rootEdge(_: Image, atom: u32) unit_walk.Edge {
        return .{ .atom = atom };
    }
    pub fn atomEdges(image: Image, atom: u32) []const WasmUnitImage.Site {
        const function_count: u32 = @intCast(image.view.functions().len);
        if (atom < function_count) return image.view.functionSites(image.view.functions()[atom]);
        return image.view.segmentSites(image.view.segments()[atom - function_count]);
    }
    pub fn edge(image: Image, site: WasmUnitImage.Site) unit_walk.Edge {
        return switch (@as(WasmUnitImage.TargetKind, @fromBackingInt(site.target_kind))) {
            .function => .{ .atom = site.target },
            .data => .{ .atom = @as(u32, @intCast(image.view.functions().len)) + site.target },
            .import => .{ .import = site.target },
            .type, .global => .none,
        };
    }
    pub fn symbolAtom(image: Image, symbol: u32) ?u32 {
        const view = image.view;
        const function_exports: u32 = @intCast(view.exports().len);
        if (symbol < function_exports) return view.exports()[symbol].function;
        const data_symbol = symbol - function_exports;
        if (data_symbol < view.dataExports().len) return @as(u32, @intCast(view.functions().len)) + view.dataExports()[data_symbol].segment;
        return null;
    }
    pub fn reference(target: Target) ?SymbolRef {
        return switch (target) {
            .defined => |found| found,
            .synthetic, .external, .missing => null,
        };
    }
    pub fn exportCount(image: Image) u32 {
        return @intCast(image.view.exports().len + image.view.dataExports().len + image.view.globals().len);
    }
    pub fn exportAt(image: Image, at: u32) unit_walk.ExportEntry {
        const view = image.view;
        const function_exports: u32 = @intCast(view.exports().len);
        const data_exports: u32 = @intCast(view.dataExports().len);
        if (at < function_exports) {
            const entry = view.exports()[at];
            return .{ .name = view.name(entry.name), .weak = entry.flags & WasmUnitImage.definition_weak != 0, .symbol = at };
        }
        if (at < function_exports + data_exports) {
            const entry = view.dataExports()[at - function_exports];
            return .{ .name = view.name(entry.name), .weak = entry.flags & WasmUnitImage.definition_weak != 0, .symbol = at };
        }
        const global = view.globals()[at - function_exports - data_exports];
        return .{ .name = view.name(global.name), .weak = global.flags & WasmUnitImage.definition_weak != 0, .symbol = at };
    }
    pub fn exportWeak(image: Image, symbol: u32) bool {
        return exportAt(image, symbol).weak;
    }
    pub fn digest(image: Image) []const u8 {
        return &image.view.header.digest;
    }
};

/// What a symbol of an image is.
const SymbolKind = enum { function, data, global };

fn symbolKind(view: View, symbol: u32) SymbolKind {
    const function_exports = view.exports().len;
    if (symbol < function_exports) return .function;
    if (symbol < function_exports + view.dataExports().len) return .data;
    return .global;
}

fn kindMatches(kind: WasmUnitImage.ImportKind, symbol_kind: SymbolKind) bool {
    return switch (kind) {
        .function => symbol_kind == .function,
        .data => symbol_kind == .data,
        .global => symbol_kind == .global,
        .table => false,
    };
}

const Mode = enum { module, object };

/// Resolves the imports of placed images in their scope.
const Resolving = struct {
    mode: Mode,
    app_exports: *const unit_walk.Exports,
    host_exports: *const unit_walk.Exports,
    app_public: []const []const u8,
    missing: std.StringArrayHashMapUnmanaged(void) = .empty,

    pub fn resolve(self: *Resolving, gpa: Allocator, images: []const Item, image: u32, import_index: u32) Allocator.Error!Resolved {
        const item = images[image];
        const import = item.view.imports()[import_index];
        const name = item.view.name(import.name);
        const kind: WasmUnitImage.ImportKind = @fromBackingInt(import.kind);
        switch (item.side) {
            .app => {
                if (self.app_exports.get(name)) |found| if (kindMatches(kind, symbolKind(images[found.image].view, found.symbol))) return .{ .defined = found };
                if (self.mode == .object) return .external;
                if (Synthetic.of(name, kind)) |synthetic| return .{ .synthetic = synthetic };
                if (self.host_exports.get(name)) |found| if (kindMatches(kind, symbolKind(images[found.image].view, found.symbol))) return .{ .defined = found };
            },
            .host => {
                if (unit_walk.findSorted(self.app_public, name) != null) if (self.app_exports.get(name)) |found| {
                    if (kindMatches(kind, symbolKind(images[found.image].view, found.symbol))) return .{ .defined = found };
                };
                if (Synthetic.of(name, kind)) |synthetic| return .{ .synthetic = synthetic };
            },
        }
        switch (kind) {
            .function, .global => return .external,
            .data, .table => {
                try self.missing.put(gpa, name, {});
                return .missing;
            },
        }
    }

    pub fn deinit(self: *Resolving, gpa: Allocator) void {
        self.missing.deinit(gpa);
    }
};

const Walk = unit_walk.AtomWalk(Kind);

/// The walk's result shared by both outputs: which images and atoms are
/// placed, and in what order.
const Program = struct {
    items: []Item,
    walk: Walk,
    app_exports: unit_walk.Exports,
    host_exports: unit_walk.Exports,
    /// Placed images, in placement order: the host, the support code, then
    /// the units by content digest.
    order: []u32,
    /// Index of the support image in `items`.
    support: u32,
    /// Index of the root unit in `items`.
    root: u32,
    ctors: bool,

    fn deinit(self: *Program, gpa: Allocator) void {
        self.walk.deinit();
        self.app_exports.deinit(gpa);
        self.host_exports.deinit(gpa);
        gpa.free(self.order);
        gpa.free(self.items);
    }

    fn state(self: *const Program, image: u32) Walk.State {
        return self.walk.states[image].?;
    }

    fn reached(self: *const Program, image: u32, atom: u32) bool {
        return self.state(image).atoms[atom];
    }
};

/// The program's images in walk order (the host, the support code, the
/// units) and the exports of each scope, ready to walk.
fn walkProgram(gpa: Allocator, mode: Mode, inputs: Inputs, program: *Program) Allocator.Error!void {
    const host_count: u32 = @intFromBool(inputs.host != null);
    const items = try gpa.alloc(Item, host_count + 1 + inputs.units.len);
    errdefer gpa.free(items);
    if (inputs.host) |host| items[0] = .{ .view = host, .side = .host };
    items[host_count] = .{ .view = inputs.support, .side = .app };
    for (inputs.units, items[host_count + 1 ..]) |view, *item| item.* = .{ .view = view, .side = .app };

    var app_exports = try unit_walk.collectExportsOf(Kind, gpa, items[host_count..]);
    errdefer app_exports.deinit(gpa);
    var app_values = app_exports.valueIterator();
    while (app_values.next()) |value| value.image += host_count;
    var host_exports = try unit_walk.collectExportsOf(Kind, gpa, items[0..host_count]);
    errdefer host_exports.deinit(gpa);

    program.* = .{
        .items = items,
        .walk = undefined,
        .app_exports = app_exports,
        .host_exports = host_exports,
        .order = &.{},
        .support = host_count,
        .root = host_count + 1 + @as(u32, @intCast(inputs.root)),
        .ctors = false,
    };
    program.walk = .{ .gpa = gpa, .images = items, .resolver = .{ .mode = mode, .app_exports = &program.app_exports, .host_exports = &program.host_exports, .app_public = inputs.app_public } };
}

/// Reach `name` as a root of the program, if anything defines it in the
/// scope an export resolves in; returns how it resolved.
fn reachExport(program: *Program, mode: Mode, name: []const u8) Allocator.Error!?Resolved {
    if (mode == .module) if (program.host_exports.get(name)) |found| {
        try program.walk.reachSymbol(found.image, found.symbol);
        return .{ .defined = found };
    };
    if (unit_walk.findSorted(program.walk.resolver.app_public, name) != null) if (program.app_exports.get(name)) |found| {
        try program.walk.reachSymbol(found.image, found.symbol);
        return .{ .defined = found };
    };
    if (mode == .module) if (Synthetic.named(name)) |synthetic| return .{ .synthetic = synthetic };
    return null;
}

/// Run the walk to completion, including the constructors once anything
/// names `__wasm_call_ctors`, and fix the placement order.
fn finishWalk(gpa: Allocator, program: *Program, ctors_exported: bool) Allocator.Error!void {
    try program.walk.run();
    program.ctors = ctors_exported or namesSynthetic(program, .call_ctors);
    if (program.ctors) {
        for (program.items, 0..) |item, index| {
            if (program.walk.states[index] == null) continue;
            for (item.view.initFuncs()) |init_func| try program.walk.reachImageAtom(@intCast(index), init_func.function);
        }
        try program.walk.run();
    }
    var order: std.ArrayList(u32) = .empty;
    errdefer order.deinit(gpa);
    for (0..program.support + 1) |index| if (program.walk.states[index] != null) try order.append(gpa, @intCast(index));
    const units_start = order.items.len;
    for (program.support + 1..program.items.len) |index| if (program.walk.states[index] != null) try order.append(gpa, @intCast(index));
    std.mem.sort(u32, order.items[units_start..], program.items, struct {
        fn lessThan(items: []const Item, a: u32, b: u32) bool {
            return std.mem.order(u8, &items[a].view.header.digest, &items[b].view.header.digest) == .lt;
        }
    }.lessThan);
    program.order = try order.toOwnedSlice(gpa);
}

/// Whether a reached site of any placed image names `synthetic`.
fn namesSynthetic(program: *const Program, synthetic: Synthetic) bool {
    for (program.walk.states) |state_or_null| {
        const state = state_or_null orelse continue;
        for (state.targets, state.imports_used) |target, used| {
            if (!used) continue;
            switch (target) {
                .synthetic => |found| if (found == synthetic) return true,
                .defined, .external, .missing => {},
            }
        }
    }
    return false;
}

/// The composed module's signatures: each distinct shape once, in the
/// order placed images first name them.
const Types = struct {
    arena: Allocator,
    by_shape: std.StringHashMapUnmanaged(u32) = .empty,
    shapes: std.ArrayList([]const u8) = .empty,
    /// Per image of the walk, its types' composed indices.
    maps: [][]u32,

    fn add(self: *Types, view: View, local: u32) Allocator.Error!u32 {
        const signature = view.types()[local];
        return self.addShape(view.params(signature), view.results(signature));
    }

    fn addShape(self: *Types, params: []const u8, results: []const u8) Allocator.Error!u32 {
        const shape = try self.arena.alloc(u8, params.len + 1 + results.len);
        @memcpy(shape[0..params.len], params);
        shape[params.len] = 0x40;
        @memcpy(shape[params.len + 1 ..], results);
        const gop = try self.by_shape.getOrPut(self.arena, shape);
        if (!gop.found_existing) {
            gop.value_ptr.* = @intCast(self.shapes.items.len);
            try self.shapes.append(self.arena, shape);
        }
        return gop.value_ptr.*;
    }

    fn encodedSize(self: *const Types) u64 {
        var size: u64 = lebSize(self.shapes.items.len);
        for (self.shapes.items) |shape| {
            const split = std.mem.indexOfScalar(u8, shape, 0x40).?;
            size += 1 + lebSize(split) + split + lebSize(shape.len - split - 1) + (shape.len - split - 1);
        }
        return size;
    }

    fn encode(self: *const Types, out: *Writer) void {
        out.leb(self.shapes.items.len);
        for (self.shapes.items) |shape| {
            const split = std.mem.indexOfScalar(u8, shape, 0x40).?;
            out.byte(0x60);
            out.leb(split);
            out.bytes(shape[0..split]);
            out.leb(shape.len - split - 1);
            out.bytes(shape[split + 1 ..]);
        }
    }

    /// The composed signature equal to `params -> results`.
    fn find(self: *const Types, params: []const u8, results: []const u8) ?u32 {
        var buffer: [512]u8 = undefined;
        if (params.len + 1 + results.len > buffer.len) {
            for (self.shapes.items, 0..) |shape, index| {
                const split = std.mem.indexOfScalar(u8, shape, 0x40).?;
                if (std.mem.eql(u8, shape[0..split], params) and std.mem.eql(u8, shape[split + 1 ..], results)) return @intCast(index);
            }
            return null;
        }
        @memcpy(buffer[0..params.len], params);
        buffer[params.len] = 0x40;
        @memcpy(buffer[params.len + 1 ..][0..results.len], results);
        return self.by_shape.get(buffer[0 .. params.len + 1 + results.len]);
    }
};

/// A bounded writer into a buffer whose size was computed in advance.
const Writer = struct {
    buffer: []u8,
    at: usize = 0,

    fn byte(self: *Writer, value: u8) void {
        self.buffer[self.at] = value;
        self.at += 1;
    }
    fn bytes(self: *Writer, values: []const u8) void {
        @memcpy(self.buffer[self.at..][0..values.len], values);
        self.at += values.len;
    }
    fn leb(self: *Writer, value: u64) void {
        var rest = value;
        while (true) {
            const low: u8 = @truncate(rest & 0x7f);
            rest >>= 7;
            if (rest == 0) {
                self.byte(low);
                return;
            }
            self.byte(low | 0x80);
        }
    }
    fn sleb(self: *Writer, value: i64) void {
        var rest = value;
        while (true) {
            const low: u8 = @truncate(@as(u64, @bitCast(rest)) & 0x7f);
            rest >>= 7;
            const done = (rest == 0 and low & 0x40 == 0) or (rest == -1 and low & 0x40 != 0);
            if (done) {
                self.byte(low);
                return;
            }
            self.byte(low | 0x80);
        }
    }
    fn name(self: *Writer, text: []const u8) void {
        self.leb(text.len);
        self.bytes(text);
    }
    fn padded(self: *Writer, value: u32) void {
        var rest = value;
        for (0..4) |_| {
            self.byte(@as(u8, @truncate(rest & 0x7f)) | 0x80);
            rest >>= 7;
        }
        self.byte(@truncate(rest));
    }
};

fn lebSize(value: u64) u64 {
    var size: u64 = 1;
    var rest = value >> 7;
    while (rest != 0) : (rest >>= 7) size += 1;
    return size;
}

fn slebSize(value: i64) u64 {
    var size: u64 = 1;
    var rest = value;
    while (true) {
        const low: u8 = @truncate(@as(u64, @bitCast(rest)) & 0x7f);
        rest >>= 7;
        if ((rest == 0 and low & 0x40 == 0) or (rest == -1 and low & 0x40 != 0)) return size;
        size += 1;
    }
}

fn nameSize(text: []const u8) u64 {
    return lebSize(text.len) + text.len;
}

fn sectionSize(payload: u64) u64 {
    return if (payload == 0) 0 else 1 + lebSize(payload) + payload;
}

/// An import of the composed module or object, kept for the embedder or
/// left undefined.
const External = struct {
    module: []const u8,
    field: []const u8,
    /// The symbol name, for an object's symbol table and a name section.
    name: []const u8,
    kind: WasmUnitImage.ImportKind,
    /// A function's composed signature.
    type: u32 = 0,
    valtype: u8 = 0,
    mutable: bool = false,
};

/// Every index a placed image's sites resolve to.
const Placement = struct {
    /// Composed function index of each local function, or `none`.
    functions: []u32,
    /// Address of each segment, or `none` when it is not placed.
    segments: []u32,
    /// Composed global index of each global the image defines.
    globals: []u32,
    /// What each import resolves to: a function or global index, a data
    /// address, or a table number; `none` when no reached site names it.
    imports: []u32,
};

/// The composed program: every decision of the layout.
const Layout = struct {
    gpa: Allocator,
    arena: Allocator,
    mode: Mode,
    program: *Program,
    types: Types,
    externals: std.ArrayList(External) = .empty,
    function_externals: u32 = 0,
    global_externals: u32 = 0,
    /// Per image of the walk; only placed images have one.
    placements: []Placement,
    /// The defined functions in index order: their image and local index;
    /// `none` image for the synthetic constructor caller.
    defined: std.ArrayList(DefinedRef) = .empty,
    /// Table slot of each composed function, or 0.
    slots: []u32 = &.{},
    table_size: u32 = 1,
    /// The functions in the table, in slot order from slot 1.
    elements: std.ArrayList(u32) = .empty,
    /// The defined globals, in index order after the imported ones.
    globals: std.ArrayList(GlobalDef) = .empty,
    stack_pointer_global: u32 = none,
    memory_base_global: u32 = none,
    table_base_global: u32 = none,
    memory: Memory = .{},
    ctors_function: u32 = none,
    /// Stack rooms of the defined functions, by defined index; empty
    /// without stack checks.
    rooms: []u64 = &.{},
    stack_checks: bool = false,

    const DefinedRef = struct { image: u32, local: u32 };

    const GlobalDef = struct {
        valtype: u8,
        mutable: bool,
        init: u64,
    };

    fn defineGlobal(self: *Layout, def: GlobalDef) Allocator.Error!u32 {
        const index = self.global_externals + @as(u32, @intCast(self.globals.items.len));
        try self.globals.append(self.arena, def);
        return index;
    }

    fn functionCount(self: *const Layout) u32 {
        return self.function_externals + @as(u32, @intCast(self.defined.items.len));
    }
};

const Memory = struct {
    data_start: u64 = 0,
    /// The end of the bytes data segments initialize.
    init_end: u64 = 0,
    data_end: u64 = 0,
    stack_low: u64 = 0,
    stack_high: u64 = 0,
    heap_base: u64 = 0,
    initial_pages: u64 = 0,
    maximum_pages: ?u64 = null,
    /// The range one active data segment covers, when any bytes need one.
    emit_start: u64 = 0,
    emit_end: u64 = 0,
};

/// Link a finished module into `output`, a fresh file.
pub fn composeModule(
    gpa: Allocator,
    arena: Allocator,
    std_io: std.Io,
    inputs: Inputs,
    config: ModuleConfig,
    output: *OutputFile,
    executor: ?Executor,
    timing: *Timing,
    outcome: *Outcome,
) LinkError!void {
    const walk_started = now(std_io);
    var program: Program = undefined;
    try walkProgram(gpa, .module, inputs, &program);
    defer program.deinit(gpa);
    try program.walk.init();
    _ = try program.walk.place(0);

    // Roots: the platform's exports, the stack exports, the host's retained
    // atoms (placing the host reaches them), and the provides.
    var undefined_exports: std.ArrayList([]const u8) = .empty;
    const export_targets = try arena.alloc(?Resolved, config.exports.len + config.stack_exports.len);
    for (config.exports, export_targets[0..config.exports.len]) |name, *target| {
        target.* = try reachExport(&program, .module, name);
        if (target.* == null) try undefined_exports.append(arena, try arena.dupe(u8, name));
    }
    for (config.stack_exports, export_targets[config.exports.len..]) |name, *target| {
        target.* = try reachExport(&program, .module, name);
        if (target.* == null) try undefined_exports.append(arena, try arena.dupe(u8, name));
    }
    if (undefined_exports.items.len != 0) {
        outcome.undefined_exports = undefined_exports.items;
        return error.UndefinedExports;
    }
    for (inputs.provides) |name| {
        // The checked program defines every provide (the checker rejects an
        // app that lacks one), and the root unit exports its wrapper.
        const found = program.app_exports.get(name) orelse base.invariant("wasm fast dev link invariant violated: no unit image defines the provided symbol {s}", .{name});
        try program.walk.reachSymbol(found.image, found.symbol);
    }
    var ctors_exported = false;
    for (export_targets) |target| switch (target.?) {
        .synthetic => |synthetic| if (synthetic == .call_ctors) {
            ctors_exported = true;
        },
        .defined, .external, .missing => {},
    };
    try finishWalk(gpa, &program, ctors_exported);
    if (program.walk.resolver.missing.count() != 0) {
        outcome.missing = try dupeNames(arena, program.walk.resolver.missing.keys());
        return error.MissingHostSymbols;
    }
    timing.walk_ns = elapsed(std_io, walk_started);

    const layout_started = now(std_io);
    var plan = try planLayout(arena, &program, .module, config, outcome);
    try checkParameters(arena, &program, inputs, &plan, outcome);

    // Exports: memory, the platform's exports, the stack exports, and what
    // the host's objects mark exported.
    var exports: std.ArrayList(ExportEntry) = .empty;
    if (!config.import_memory) try exports.append(arena, .{ .name = "memory", .kind = 2, .index = 0 });
    const export_names = try std.mem.concat(arena, []const u8, &.{ config.exports, config.stack_exports });
    var exported: std.StringHashMapUnmanaged(void) = .empty;
    for (export_names, export_targets) |name, target| {
        if ((try exported.getOrPut(arena, name)).found_existing) continue;
        try exports.append(arena, try exportEntry(&plan, name, target.?));
    }
    if (inputs.host != null and program.walk.states[0] != null) {
        const host = program.items[0].view;
        for (host.exports()) |entry| {
            if (entry.flags & WasmUnitImage.export_object_exported == 0) continue;
            const name = host.name(entry.name);
            if ((try exported.getOrPut(arena, name)).found_existing) continue;
            try exports.append(arena, .{ .name = name, .kind = 0, .index = plan.placements[0].functions[entry.function] });
        }
    }

    try checkLimits(&plan, exports.items.len, outcome);

    // The file's layout, section by section.
    const sections = try moduleSections(arena, &plan, exports.items, config);
    timing.layout_ns = elapsed(std_io, layout_started);

    const write_started = now(std_io);
    try output.resize(sections.total);
    const mapped = try output.map(sections.total);
    var unmapped = false;
    defer if (!unmapped) output.unmap(mapped);
    @memcpy(mapped[0..sections.head.len], sections.head);
    var tasks: std.ArrayList(Task) = .empty;
    defer tasks.deinit(gpa);
    const context: TaskContext = .{ .plan = &plan, .out = mapped, .function_offsets = sections.function_offsets, .data_payload = sections.data_payload };
    try addTasks(gpa, &tasks, &context);
    try runTasks(gpa, executor, tasks.items);
    for (tasks.items) |task| {
        timing.bytes_written += task.bytes;
        timing.sites_patched += task.sites;
    }
    timing.write_ns = elapsed(std_io, write_started);

    const finalize_started = now(std_io);
    // The coordinator's own writes: the constructor caller, the data
    // section's header and the name section.
    if (plan.ctors_function != none) writeCtorsBody(&plan, mapped[sections.ctors_offset..]);
    @memcpy(mapped[sections.data_header_offset..][0..sections.data_header.len], sections.data_header);
    @memcpy(mapped[sections.names_offset..][0..sections.names.len], sections.names);
    output.unmap(mapped);
    unmapped = true;
    timing.finalize_ns = elapsed(std_io, finalize_started);
    timing.images_placed = program.order.len;
    timing.functions = plan.functionCount();
}

fn dupeNames(arena: Allocator, names: []const []const u8) Allocator.Error![]const []const u8 {
    const out = try arena.alloc([]const u8, names.len);
    for (names, out) |name, *slot| slot.* = try arena.dupe(u8, name);
    return out;
}

const ExportEntry = struct {
    name: []const u8,
    /// 0 function, 2 memory, 3 global.
    kind: u8,
    index: u32,
};

/// The export of `name`, which resolved to `target`. A data symbol is
/// exported as an immutable global holding its address.
fn exportEntry(plan: *Layout, name: []const u8, target: Resolved) Allocator.Error!ExportEntry {
    switch (target) {
        .defined => |found| {
            const view = plan.program.items[found.image].view;
            const placement = plan.placements[found.image];
            const function_exports: u32 = @intCast(view.exports().len);
            const data_exports: u32 = @intCast(view.dataExports().len);
            if (found.symbol < function_exports) return .{ .name = name, .kind = 0, .index = placement.functions[view.exports()[found.symbol].function] };
            if (found.symbol < function_exports + data_exports) {
                const data_export = view.dataExports()[found.symbol - function_exports];
                const address = placement.segments[data_export.segment] + data_export.offset;
                return .{ .name = name, .kind = 3, .index = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = address }) };
            }
            return .{ .name = name, .kind = 3, .index = placement.globals[found.symbol - function_exports - data_exports] };
        },
        .synthetic => |synthetic| return switch (synthetic) {
            .stack_pointer => .{ .name = name, .kind = 3, .index = plan.stack_pointer_global },
            .memory_base => .{ .name = name, .kind = 3, .index = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = 0 }) },
            .table_base => .{ .name = name, .kind = 3, .index = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = 1 }) },
            .call_ctors => .{ .name = name, .kind = 0, .index = plan.ctors_function },
            .indirect_function_table => .{ .name = name, .kind = 1, .index = 0 },
            .stack_low, .stack_high, .heap_base, .heap_end, .data_end, .global_base, .dso_handle => .{ .name = name, .kind = 3, .index = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = syntheticAddress(plan, synthetic) }) },
        },
        .external, .missing => unreachable, // `reachExport` resolves an export to a definition or a synthetic symbol only
    }
}

fn syntheticAddress(plan: *const Layout, synthetic: Synthetic) u64 {
    const memory = plan.memory;
    return switch (synthetic) {
        .stack_low => memory.stack_low,
        .stack_high => memory.stack_high,
        .heap_base => memory.heap_base,
        .heap_end => memory.initial_pages * page_size,
        .data_end => memory.data_end,
        .global_base, .dso_handle => memory.data_start,
        .stack_pointer, .memory_base, .table_base, .indirect_function_table, .call_ctors => unreachable, // only data symbols have addresses
    };
}

/// Number every index space, lay out memory and the table, and resolve
/// every import of every placed image.
fn planLayout(arena: Allocator, program: *Program, mode: Mode, config: ?ModuleConfig, outcome: *Outcome) LinkError!Layout {
    const items = program.items;
    var plan: Layout = .{
        .gpa = arena,
        .arena = arena,
        .mode = mode,
        .program = program,
        .types = .{ .arena = arena, .maps = try arena.alloc([]u32, items.len) },
        .placements = try arena.alloc(Placement, items.len),
    };
    for (program.order) |image| {
        const view = items[image].view;
        const map = try arena.alloc(u32, view.types().len);
        for (map, 0..) |*slot, local| slot.* = try plan.types.add(view, @intCast(local));
        plan.types.maps[image] = map;
        plan.placements[image] = .{
            .functions = try arena.alloc(u32, view.functions().len),
            .segments = try arena.alloc(u32, view.segments().len),
            .globals = try arena.alloc(u32, view.globals().len),
            .imports = try arena.alloc(u32, view.imports().len),
        };
        @memset(plan.placements[image].functions, none);
        @memset(plan.placements[image].segments, none);
        @memset(plan.placements[image].imports, none);
        if (view.stackChecks()) plan.stack_checks = true;
    }

    // Imports: every external function a reached site names, then every
    // external global, each (module, field) once, in placement order.
    var external_index: std.StringHashMapUnmanaged(u32) = .empty;
    inline for (.{ WasmUnitImage.ImportKind.function, WasmUnitImage.ImportKind.global }) |wanted| {
        for (program.order) |image| {
            const view = items[image].view;
            const state = program.state(image);
            for (view.imports(), state.targets, state.imports_used, 0..) |import, target, used, index| {
                if (!used or target != .external or import.kind != @backingInt(wanted)) continue;
                const module_name = view.name(import.module);
                const field = view.name(import.field);
                const type_index: u32 = if (wanted == .function) blk: {
                    if (import.type == WasmUnitImage.no_type) base.invariant("wasm fast dev link invariant violated: the embedder import {s} has no signature", .{field});
                    break :blk plan.types.maps[image][import.type];
                } else 0;
                const key = try std.fmt.allocPrint(arena, "{d}\x00{s}\x00{s}", .{ @backingInt(wanted), module_name, field });
                const gop = try external_index.getOrPut(arena, key);
                if (gop.found_existing) {
                    const existing = plan.externals.items[gop.value_ptr.*];
                    if (existing.type != type_index or existing.valtype != import.valtype or existing.mutable != (import.mutable != 0)) {
                        outcome.conflict = try arena.dupe(u8, field);
                        return error.ImportConflict;
                    }
                } else {
                    gop.value_ptr.* = @intCast(plan.externals.items.len);
                    try plan.externals.append(arena, .{ .module = module_name, .field = field, .name = view.name(import.name), .kind = wanted, .type = type_index, .valtype = import.valtype, .mutable = import.mutable != 0 });
                }
                // Globals are numbered after functions; fixed below.
                plan.placements[image].imports[index] = gop.value_ptr.*;
            }
        }
        if (wanted == .function) plan.function_externals = @intCast(plan.externals.items.len);
    }
    plan.global_externals = @as(u32, @intCast(plan.externals.items.len)) - plan.function_externals;
    for (program.order) |image| {
        const view = items[image].view;
        const state = program.state(image);
        for (view.imports(), state.targets, state.imports_used, plan.placements[image].imports) |import, target, used, *slot| {
            if (!used or target != .external or import.kind != @backingInt(WasmUnitImage.ImportKind.global)) continue;
            slot.* -= plan.function_externals;
        }
    }

    // Functions: the externals, then each placed image's reached functions
    // in its order, then the constructor caller.
    for (program.order) |image| {
        const view = items[image].view;
        for (plan.placements[image].functions, 0..) |*slot, local| {
            if (!program.reached(image, @intCast(local))) continue;
            slot.* = plan.functionCount();
            try plan.defined.append(arena, .{ .image = image, .local = @intCast(local) });
        }
        _ = view;
    }
    if (program.ctors) {
        plan.ctors_function = plan.functionCount();
        try plan.defined.append(arena, .{ .image = none, .local = 0 });
        _ = try plan.types.addShape(&.{}, &.{});
    }

    // Globals: the stack pointer, the host's own, then the base globals a
    // reached site names. An object defines only what its images define.
    if (mode == .object) {
        for (program.order) |image| {
            const view = items[image].view;
            for (view.globals(), plan.placements[image].globals) |global, *slot| {
                slot.* = try plan.defineGlobal(.{ .valtype = global.valtype, .mutable = global.mutable != 0, .init = global.init });
            }
        }
    }
    if (mode == .module) {
        plan.stack_pointer_global = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = true, .init = 0 });
        for (program.order) |image| {
            const view = items[image].view;
            for (view.globals(), plan.placements[image].globals) |global, *slot| {
                slot.* = try plan.defineGlobal(.{ .valtype = global.valtype, .mutable = global.mutable != 0, .init = global.init });
            }
        }
        if (namesSynthetic(program, .memory_base)) plan.memory_base_global = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = 0 });
        if (namesSynthetic(program, .table_base)) plan.table_base_global = try plan.defineGlobal(.{ .valtype = 0x7f, .mutable = false, .init = 0 });
    }

    // Memory.
    if (config) |module_config| try planMemory(&plan, module_config, outcome) else planObjectMemory(&plan);
    if (mode == .module) plan.globals.items[plan.stack_pointer_global - plan.global_externals].init = plan.memory.stack_high;

    // Every reached import's value, and the table slots.
    const function_total = plan.functionCount();
    plan.slots = try arena.alloc(u32, function_total);
    @memset(plan.slots, 0);
    var requested = try arena.alloc(bool, function_total);
    @memset(requested, false);
    for (program.order) |image| {
        const view = items[image].view;
        const placement = plan.placements[image];
        for (view.tableRequests()) |local| if (placement.functions[local] != none) {
            requested[placement.functions[local]] = true;
        };
        const state = program.state(image);
        for (view.imports(), state.targets, state.imports_used, placement.imports) |import, target, used, *slot| {
            if (!used) continue;
            switch (target) {
                .defined => |found| slot.* = definedValue(&plan, found),
                .synthetic => |synthetic| slot.* = switch (synthetic) {
                    .stack_pointer => plan.stack_pointer_global,
                    .memory_base => plan.memory_base_global,
                    .table_base => plan.table_base_global,
                    .indirect_function_table => 0,
                    .call_ctors => plan.ctors_function,
                    .stack_low, .stack_high, .heap_base, .heap_end, .data_end, .global_base, .dso_handle => @intCast(syntheticAddress(&plan, synthetic)),
                },
                // Functions and globals were numbered with the externals
                // above; an object leaves data and its table undefined.
                .external => {},
                .missing => unreachable, // the link reports a missing symbol before it lays anything out
            }
            if (import.flags & WasmUnitImage.import_address_taken != 0 and import.kind == @backingInt(WasmUnitImage.ImportKind.function) and slot.* != none) requested[slot.*] = true;
        }
    }
    for (requested, 0..) |wanted, function| if (wanted) {
        plan.slots[function] = plan.table_size;
        plan.table_size += 1;
        try plan.elements.append(arena, @intCast(function));
    };

    if (plan.stack_checks) plan.rooms = try stackRooms(arena, &plan);
    return plan;
}

/// The composed index a definition resolves to: a function index, a data
/// address, or a global index.
fn definedValue(plan: *const Layout, found: SymbolRef) u32 {
    const view = plan.program.items[found.image].view;
    const placement = plan.placements[found.image];
    const function_exports: u32 = @intCast(view.exports().len);
    const data_exports: u32 = @intCast(view.dataExports().len);
    if (found.symbol < function_exports) return placement.functions[view.exports()[found.symbol].function];
    if (found.symbol < function_exports + data_exports) {
        const data_export = view.dataExports()[found.symbol - function_exports];
        return placement.segments[data_export.segment] + data_export.offset;
    }
    return placement.globals[found.symbol - function_exports - data_exports];
}

/// Lay out memory as `linker.wasmStackLayout` does: the stack first and
/// static data above it, or static data from the global base and the stack
/// above it. Initialized segments come first, in placement order, then the
/// zero-filled ones, each at its alignment.
fn planMemory(plan: *Layout, config: ModuleConfig, outcome: *Outcome) LinkError!void {
    const program = plan.program;
    var memory: Memory = .{};
    if (config.stack_size % stack_alignment != 0) {
        outcome.memory = .{ .what = .stack_size_not_aligned, .needed = stack_alignment, .configured = config.stack_size };
        return error.MemoryLayout;
    }
    var at: u64 = switch (linker.wasmStackLayout(config.global_base)) {
        .stack_first => blk: {
            memory.stack_low = 0;
            memory.stack_high = config.stack_size;
            break :blk config.stack_size;
        },
        .data_first => |global_base| global_base,
    };
    memory.data_start = at;
    var first_initialized: ?u64 = null;
    for ([_]bool{ false, true }) |zero_fill_pass| {
        for (program.order) |image| {
            const view = program.items[image].view;
            const function_count: u32 = @intCast(view.functions().len);
            for (view.segments(), plan.placements[image].segments, 0..) |segment, *slot, index| {
                if ((segment.flags & WasmUnitImage.segment_zero_fill != 0) != zero_fill_pass) continue;
                if (!program.reached(image, function_count + @as(u32, @intCast(index)))) continue;
                at = std.mem.alignForward(u64, at, @as(u64, 1) << @intCast(segment.alignment_log2));
                if (!zero_fill_pass and first_initialized == null) first_initialized = at;
                if (at + segment.size > memory_limit) {
                    outcome.memory = .{ .what = .memory_too_large, .needed = at + segment.size, .configured = memory_limit };
                    return error.MemoryLayout;
                }
                slot.* = @intCast(at);
                at += segment.size;
            }
        }
        if (!zero_fill_pass) memory.init_end = at;
    }
    memory.data_end = at;
    switch (linker.wasmStackLayout(config.global_base)) {
        .stack_first => {},
        .data_first => {
            memory.stack_low = std.mem.alignForward(u64, at, stack_alignment);
            memory.stack_high = memory.stack_low + config.stack_size;
            at = memory.stack_high;
        },
    }
    memory.heap_base = std.mem.alignForward(u64, at, stack_alignment);
    if (memory.heap_base > memory_limit) {
        outcome.memory = .{ .what = .memory_too_large, .needed = memory.heap_base, .configured = memory_limit };
        return error.MemoryLayout;
    }
    var initial = std.mem.alignForward(u64, memory.heap_base, page_size);
    if (config.initial_memory) |configured| {
        if (configured % page_size != 0) {
            outcome.memory = .{ .what = .memory_not_page_aligned, .needed = page_size, .configured = configured };
            return error.MemoryLayout;
        }
        if (configured < memory.heap_base) {
            outcome.memory = .{ .what = .initial_memory_too_small, .needed = memory.heap_base, .configured = configured };
            return error.MemoryLayout;
        }
        initial = configured;
    }
    memory.initial_pages = initial / page_size;
    const maximum: ?u64 = switch (linker.wasmStackLayout(config.global_base)) {
        .stack_first => config.maximum_memory orelse linker.stackFirstMaximumMemory(@intCast(config.stack_size)),
        .data_first => config.maximum_memory,
    };
    if (maximum) |bytes| {
        if (bytes % page_size != 0) {
            outcome.memory = .{ .what = .memory_not_page_aligned, .needed = page_size, .configured = bytes };
            return error.MemoryLayout;
        }
        if (bytes < initial) {
            outcome.memory = .{ .what = .maximum_memory_too_small, .needed = initial, .configured = bytes };
            return error.MemoryLayout;
        }
        memory.maximum_pages = bytes / page_size;
    }
    if (first_initialized) |start| {
        memory.emit_start = start;
        memory.emit_end = if (config.zero_filled_memory) memory.init_end else memory.data_end;
    } else if (!config.zero_filled_memory and memory.data_end > memory.data_start) {
        memory.emit_start = memory.data_start;
        memory.emit_end = memory.data_end;
    }
    plan.memory = memory;
}

/// An object's segments sit in a virtual address space from 0, each at its
/// alignment, in placement order; the platform's linker places them.
fn planObjectMemory(plan: *Layout) void {
    const program = plan.program;
    var at: u64 = 0;
    for (program.order) |image| {
        const view = program.items[image].view;
        const function_count: u32 = @intCast(view.functions().len);
        for (view.segments(), plan.placements[image].segments, 0..) |segment, *slot, index| {
            if (!program.reached(image, function_count + @as(u32, @intCast(index)))) continue;
            at = std.mem.alignForward(u64, at, @as(u64, 1) << @intCast(segment.alignment_log2));
            slot.* = @intCast(at);
            at += segment.size;
        }
    }
    plan.memory.data_end = at;
}

/// The room each defined function's stack checks need, by defined index:
/// the analysis `StackRoom.fillCheckRooms` runs on a composed object, over
/// the frame facts the images carry. Calls into the host need none.
fn stackRooms(arena: Allocator, plan: *const Layout) Allocator.Error![]u64 {
    const program = plan.program;
    const first = plan.function_externals;
    const count = plan.defined.items.len;
    const functions = try arena.alloc(StackRoom.ProgramFunction, count);
    var edges: std.ArrayList(StackRoom.ProgramEdge) = .empty;
    // Table functions of the app by composed signature: the targets an
    // indirect call may reach.
    var table_by_type: std.AutoHashMapUnmanaged(u32, std.ArrayList(u32)) = .empty;
    for (plan.elements.items) |function| {
        if (function < first) continue;
        const ref = plan.defined.items[function - first];
        if (ref.image == none or program.items[ref.image].side != .app) continue;
        const view = program.items[ref.image].view;
        const type_index = plan.types.maps[ref.image][view.functions()[ref.local].type];
        const gop = try table_by_type.getOrPut(arena, type_index);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(arena, function - first);
    }
    for (plan.defined.items, functions) |ref, *out| {
        out.* = .{ .own_depth = 0, .checked = true, .first_edge = @intCast(edges.items.len), .edge_count = 0 };
        if (ref.image == none) continue;
        const item = program.items[ref.image];
        if (item.side != .app or !item.view.stackChecks()) continue;
        const function = item.view.functions()[ref.local];
        const placement = plan.placements[ref.image];
        out.own_depth = function.frame_depth;
        out.checked = function.flags & WasmUnitImage.function_checked != 0;
        for (item.view.functionCalls(function)) |call| {
            switch (@as(WasmUnitImage.TargetKind, @fromBackingInt(call.target_kind))) {
                .function => try edges.append(arena, .{ .callee = placement.functions[call.target] - first, .depth = call.depth }),
                .import => switch (program.state(ref.image).targets[call.target]) {
                    .defined => |found| if (program.items[found.image].side == .app) {
                        try edges.append(arena, .{ .callee = definedValue(plan, found) - first, .depth = call.depth });
                    },
                    // The host's code is the host's own business.
                    .synthetic, .external, .missing => {},
                },
                .type => if (table_by_type.get(plan.types.maps[ref.image][call.target])) |targets| {
                    for (targets.items) |callee| try edges.append(arena, .{ .callee = callee, .depth = call.depth });
                },
                .data, .global => unreachable, // `View` admits only function, import and signature callees
            }
        }
        out.edge_count = @as(u32, @intCast(edges.items.len)) - out.first_edge;
    }
    return StackRoom.programRooms(arena, functions, edges.items);
}

/// Report every platform boundary function no engine would load: a
/// provided entry wrapper, or a hosted function the app calls.
fn checkParameters(arena: Allocator, program: *const Program, inputs: Inputs, plan: *const Layout, outcome: *Outcome) LinkError!void {
    const limit = layout.abi.wasm_max_function_params;
    var problems: std.ArrayList(ParameterProblem) = .empty;
    for (inputs.provides) |name| {
        const found = program.app_exports.get(name) orelse continue;
        const view = program.items[found.image].view;
        if (symbolKind(view, found.symbol) != .function) continue;
        const function = view.functions()[view.exports()[found.symbol].function];
        const params = view.types()[function.type].param_count;
        if (params > limit) try problems.append(arena, .{ .symbol = try arena.dupe(u8, name), .hosted = false, .param_count = params });
    }
    var seen: std.StringHashMapUnmanaged(void) = .empty;
    for (program.order) |image| {
        const item = program.items[image];
        if (item.side != .app) continue;
        const state = program.state(image);
        for (item.view.imports(), state.targets, state.imports_used) |import, target, used| {
            if (!used or import.kind != @backingInt(WasmUnitImage.ImportKind.function) or import.type == WasmUnitImage.no_type) continue;
            const name = item.view.name(import.name);
            if (unit_walk.findSorted(inputs.hosted, name) == null) continue;
            switch (target) {
                .defined => |found| if (program.items[found.image].side == .app) continue,
                .synthetic, .external, .missing => {},
            }
            if ((try seen.getOrPut(arena, name)).found_existing) continue;
            const params = item.view.types()[import.type].param_count;
            if (params > limit) try problems.append(arena, .{ .symbol = try arena.dupe(u8, name), .hosted = true, .param_count = params });
        }
    }
    _ = plan;
    if (problems.items.len != 0) {
        outcome.parameters = problems.items;
        return error.ParameterLimit;
    }
}

fn checkLimits(plan: *const Layout, export_count: usize, outcome: *Outcome) LinkError!void {
    const checks = [_]Limit{
        .{ .what = "functions", .count = plan.functionCount(), .limit = max_functions },
        .{ .what = "imports", .count = plan.externals.items.len + @intFromBool(false), .limit = max_imports },
        .{ .what = "exports", .count = export_count, .limit = max_exports },
        .{ .what = "function signatures", .count = plan.types.shapes.items.len, .limit = max_types },
        .{ .what = "globals", .count = plan.global_externals + plan.globals.items.len, .limit = max_globals },
        .{ .what = "table elements", .count = plan.table_size, .limit = max_table_size },
        .{ .what = "data segments", .count = @intFromBool(plan.memory.emit_end > plan.memory.emit_start), .limit = max_data_segments },
    };
    for (checks) |check| if (check.count > check.limit) {
        outcome.limit = check;
        return error.EngineLimit;
    };
    for (plan.defined.items) |ref| {
        if (ref.image == none) continue;
        const function = plan.program.items[ref.image].view.functions()[ref.local];
        if (function.body_size > max_body_size) {
            outcome.limit = .{ .what = "bytes in one function body", .count = function.body_size, .limit = max_body_size };
            return error.EngineLimit;
        }
    }
}

/// The module's bytes before the code section, the code and data sections'
/// places, and the bytes after them, all computed before anything is
/// written.
const ModuleSections = struct {
    /// Everything before the first function's size prefix.
    head: []const u8,
    /// Output offset of each defined function's size prefix.
    function_offsets: []u64,
    ctors_offset: usize,
    data_header: []const u8,
    data_header_offset: usize,
    /// Output offset of the data segment's payload.
    data_payload: u64,
    names: []const u8,
    names_offset: usize,
    total: usize,
};

fn moduleSections(arena: Allocator, plan: *Layout, exports: []const ExportEntry, config: ModuleConfig) Allocator.Error!ModuleSections {
    // Payload sizes of the small sections.
    const type_payload = plan.types.encodedSize();
    var import_payload: u64 = 0;
    const import_count = plan.externals.items.len + @intFromBool(config.import_memory);
    if (import_count != 0) {
        import_payload = lebSize(import_count);
        for (plan.externals.items) |external| {
            import_payload += nameSize(external.module) + nameSize(external.field) + 1;
            import_payload += switch (external.kind) {
                .function => lebSize(external.type),
                .global => 2,
                .table, .data => unreachable, // only functions and globals are left to the embedder
            };
        }
        if (config.import_memory) import_payload += nameSize("env") + nameSize("memory") + 1 + memoryLimitsSize(plan.memory);
    }
    const defined_count = plan.defined.items.len;
    var function_payload: u64 = if (defined_count == 0) 0 else lebSize(defined_count);
    for (plan.defined.items) |ref| function_payload += lebSize(definedType(plan, ref));
    const table_payload: u64 = 1 + 1 + 1 + lebSize(plan.table_size) + lebSize(plan.table_size);
    const memory_payload: u64 = if (config.import_memory) 0 else 1 + memoryLimitsSize(plan.memory);
    var global_payload: u64 = if (plan.globals.items.len == 0) 0 else lebSize(plan.globals.items.len);
    for (plan.globals.items) |global| global_payload += 2 + initExprSize(global);
    var export_payload: u64 = if (exports.len == 0) 0 else lebSize(exports.len);
    for (exports) |entry| export_payload += nameSize(entry.name) + 1 + lebSize(entry.index);
    var elem_payload: u64 = 0;
    if (plan.elements.items.len != 0) {
        elem_payload = lebSize(1) + lebSize(0) + 3 + lebSize(plan.elements.items.len);
        for (plan.elements.items) |function| elem_payload += lebSize(function);
    }

    var head_size: u64 = 8;
    head_size += sectionSize(type_payload) + sectionSize(import_payload) + sectionSize(function_payload) + sectionSize(table_payload) + sectionSize(memory_payload) + sectionSize(global_payload) + sectionSize(export_payload) + sectionSize(elem_payload);

    // The code section: each body after its size prefix.
    var code_payload: u64 = if (defined_count == 0) 0 else lebSize(defined_count);
    const ctors_body = ctorsBodySize(plan);
    for (plan.defined.items) |ref| {
        const size = definedBodySize(plan, ref, ctors_body);
        code_payload += lebSize(size) + size;
    }
    const code_start = head_size + if (code_payload == 0) 0 else 1 + lebSize(code_payload);
    const function_offsets = try arena.alloc(u64, defined_count);
    var at = code_start + if (defined_count == 0) 0 else lebSize(defined_count);
    var ctors_offset: u64 = 0;
    for (plan.defined.items, function_offsets) |ref, *offset| {
        offset.* = at;
        const size = definedBodySize(plan, ref, ctors_body);
        if (ref.image == none) ctors_offset = at;
        at += lebSize(size) + size;
    }
    std.debug.assert(at == code_start + code_payload);

    // The data section: one active segment.
    const memory = plan.memory;
    const payload_len = memory.emit_end - memory.emit_start;
    const data_payload_size: u64 = if (payload_len == 0) 0 else lebSize(1) + 1 + 1 + slebSize(@as(i32, @bitCast(@as(u32, @intCast(memory.emit_start))))) + 1 + lebSize(payload_len) + payload_len;
    const data_start = at;
    const data_payload_offset = data_start + if (data_payload_size == 0) 0 else 1 + lebSize(data_payload_size) + (data_payload_size - payload_len);

    // The name section, with `--debug`.
    var names_payload: u64 = 0;
    var function_names_size: u64 = 0;
    var named: u64 = 0;
    if (config.names) {
        for (plan.externals.items[0..plan.function_externals], 0..) |external, index| {
            function_names_size += lebSize(index) + nameSize(external.name);
            named += 1;
        }
        for (plan.defined.items, 0..) |ref, index| {
            const name = definedName(plan, ref);
            if (name.len == 0) continue;
            function_names_size += lebSize(plan.function_externals + index) + nameSize(name);
            named += 1;
        }
        function_names_size += lebSize(named);
        names_payload = nameSize("name") + 1 + lebSize(function_names_size) + function_names_size;
    }

    const code_header_size: u64 = if (code_payload == 0) 0 else 1 + lebSize(code_payload) + lebSize(defined_count);
    const data_header_size: u64 = if (data_payload_size == 0) 0 else data_payload_offset - data_start;

    // The head: everything before the first function's size prefix.
    const head = try arena.alloc(u8, head_size + code_header_size);
    var out: Writer = .{ .buffer = head };
    out.bytes("\x00asm\x01\x00\x00\x00");
    if (type_payload != 0) {
        out.byte(1);
        out.leb(type_payload);
        plan.types.encode(&out);
    }
    if (import_payload != 0) {
        out.byte(2);
        out.leb(import_payload);
        out.leb(import_count);
        for (plan.externals.items) |external| {
            out.name(external.module);
            out.name(external.field);
            switch (external.kind) {
                .function => {
                    out.byte(0);
                    out.leb(external.type);
                },
                .global => {
                    out.byte(3);
                    out.byte(external.valtype);
                    out.byte(@intFromBool(external.mutable));
                },
                .table, .data => unreachable, // only functions and globals are left to the embedder
            }
        }
        if (config.import_memory) {
            out.name("env");
            out.name("memory");
            out.byte(2);
            writeMemoryLimits(&out, memory);
        }
    }
    if (function_payload != 0) {
        out.byte(3);
        out.leb(function_payload);
        out.leb(defined_count);
        for (plan.defined.items) |ref| out.leb(definedType(plan, ref));
    }
    out.byte(4);
    out.leb(table_payload);
    out.leb(1);
    out.byte(0x70);
    out.byte(1);
    out.leb(plan.table_size);
    out.leb(plan.table_size);
    if (memory_payload != 0) {
        out.byte(5);
        out.leb(memory_payload);
        out.leb(1);
        writeMemoryLimits(&out, memory);
    }
    if (global_payload != 0) {
        out.byte(6);
        out.leb(global_payload);
        out.leb(plan.globals.items.len);
        for (plan.globals.items) |global| {
            out.byte(global.valtype);
            out.byte(@intFromBool(global.mutable));
            writeInitExpr(&out, global);
        }
    }
    if (export_payload != 0) {
        out.byte(7);
        out.leb(export_payload);
        out.leb(exports.len);
        for (exports) |entry| {
            out.name(entry.name);
            out.byte(entry.kind);
            out.leb(entry.index);
        }
    }
    if (elem_payload != 0) {
        out.byte(9);
        out.leb(elem_payload);
        out.leb(1);
        out.leb(0);
        out.byte(0x41);
        out.sleb(1);
        out.byte(0x0b);
        out.leb(plan.elements.items.len);
        for (plan.elements.items) |function| out.leb(function);
    }
    if (code_payload != 0) {
        out.byte(10);
        out.leb(code_payload);
        out.leb(defined_count);
    }
    std.debug.assert(out.at == head.len and head.len == code_start + (if (defined_count == 0) 0 else lebSize(defined_count)));

    // The data section's header, before the payload the tasks write.
    const data_header = try arena.alloc(u8, data_header_size);
    if (data_payload_size != 0) {
        var header: Writer = .{ .buffer = data_header };
        header.byte(11);
        header.leb(data_payload_size);
        header.leb(1);
        header.leb(0);
        header.byte(0x41);
        header.sleb(@as(i32, @bitCast(@as(u32, @intCast(memory.emit_start)))));
        header.byte(0x0b);
        header.leb(payload_len);
        std.debug.assert(header.at == data_header.len);
    }

    // The name section, last.
    const names = try arena.alloc(u8, sectionSize(names_payload));
    if (names_payload != 0) {
        var names_out: Writer = .{ .buffer = names };
        names_out.byte(0);
        names_out.leb(names_payload);
        names_out.name("name");
        names_out.byte(1);
        names_out.leb(function_names_size);
        names_out.leb(named);
        for (plan.externals.items[0..plan.function_externals], 0..) |external, index| {
            names_out.leb(index);
            names_out.name(external.name);
        }
        for (plan.defined.items, 0..) |ref, index| {
            const name = definedName(plan, ref);
            if (name.len == 0) continue;
            names_out.leb(plan.function_externals + index);
            names_out.name(name);
        }
        std.debug.assert(names_out.at == names.len);
    }
    const names_offset = data_payload_offset + (if (data_payload_size == 0) 0 else payload_len);
    return .{
        .head = head,
        .function_offsets = function_offsets,
        .ctors_offset = @intCast(ctors_offset),
        .data_header = data_header,
        .data_header_offset = @intCast(data_start),
        .data_payload = data_payload_offset,
        .names = names,
        .names_offset = @intCast(names_offset),
        .total = @intCast(names_offset + names.len),
    };
}

fn memoryLimitsSize(memory: Memory) u64 {
    return 1 + lebSize(memory.initial_pages) + if (memory.maximum_pages) |pages| lebSize(pages) else 0;
}

fn writeMemoryLimits(out: *Writer, memory: Memory) void {
    if (memory.maximum_pages) |pages| {
        out.byte(1);
        out.leb(memory.initial_pages);
        out.leb(pages);
    } else {
        out.byte(0);
        out.leb(memory.initial_pages);
    }
}

fn initExprSize(global: Layout.GlobalDef) u64 {
    return switch (global.valtype) {
        0x7f => 1 + slebSize(@as(i32, @truncate(@as(i64, @bitCast(global.init))))) + 1,
        0x7e => 1 + slebSize(@bitCast(global.init)) + 1,
        0x7d => 1 + 4 + 1,
        0x7c => 1 + 8 + 1,
        else => unreachable, // `View` admits only numeric global types
    };
}

fn writeInitExpr(out: *Writer, global: Layout.GlobalDef) void {
    switch (global.valtype) {
        0x7f => {
            out.byte(0x41);
            out.sleb(@as(i32, @truncate(@as(i64, @bitCast(global.init)))));
        },
        0x7e => {
            out.byte(0x42);
            out.sleb(@bitCast(global.init));
        },
        0x7d => {
            out.byte(0x43);
            var bits: [4]u8 = undefined;
            std.mem.writeInt(u32, &bits, @truncate(global.init), .little);
            out.bytes(&bits);
        },
        0x7c => {
            out.byte(0x44);
            var bits: [8]u8 = undefined;
            std.mem.writeInt(u64, &bits, global.init, .little);
            out.bytes(&bits);
        },
        else => unreachable, // `View` admits only numeric global types
    }
    out.byte(0x0b);
}

fn definedType(plan: *const Layout, ref: Layout.DefinedRef) u32 {
    if (ref.image == none) return plan.types.find(&.{}, &.{}).?;
    const view = plan.program.items[ref.image].view;
    return plan.types.maps[ref.image][view.functions()[ref.local].type];
}

fn definedName(plan: *const Layout, ref: Layout.DefinedRef) []const u8 {
    if (ref.image == none) return "__wasm_call_ctors";
    const view = plan.program.items[ref.image].view;
    return view.name(view.functions()[ref.local].name);
}

fn definedBodySize(plan: *const Layout, ref: Layout.DefinedRef, ctors_body: u64) u64 {
    if (ref.image == none) return ctors_body;
    return plan.program.items[ref.image].view.functions()[ref.local].body_size;
}

/// The constructors in the order they run: every placed image's, by
/// priority, in placement order among equals.
fn ctorsOrder(plan: *const Layout, out: *std.ArrayList(u32), arena: Allocator) Allocator.Error!void {
    const Entry = struct { priority: u32, function: u32 };
    var entries: std.ArrayList(Entry) = .empty;
    for (plan.program.order) |image| {
        const view = plan.program.items[image].view;
        for (view.initFuncs()) |init_func| try entries.append(arena, .{ .priority = init_func.priority, .function = plan.placements[image].functions[init_func.function] });
    }
    std.mem.sortUnstable(Entry, entries.items, {}, struct {
        fn lessThan(_: void, a: Entry, b: Entry) bool {
            if (a.priority != b.priority) return a.priority < b.priority;
            return a.function < b.function;
        }
    }.lessThan);
    for (entries.items) |entry| try out.append(arena, entry.function);
}

fn ctorsBodySize(plan: *const Layout) u64 {
    if (plan.ctors_function == none) return 0;
    var size: u64 = 1 + 1; // no locals, end
    for (plan.program.order) |image| {
        const view = plan.program.items[image].view;
        for (view.initFuncs()) |init_func| size += 1 + lebSize(plan.placements[image].functions[init_func.function]);
    }
    return size;
}

fn writeCtorsBody(plan: *const Layout, out_bytes: []u8) void {
    var order: std.ArrayList(u32) = .empty;
    ctorsOrder(plan, &order, plan.arena) catch base.invariant("wasm fast dev link invariant violated: the constructor order does not fit the link's arena", .{});
    const size = ctorsBodySize(plan);
    var out: Writer = .{ .buffer = out_bytes };
    out.leb(size);
    out.byte(0);
    for (order.items) |function| {
        out.byte(0x10);
        out.leb(function);
    }
    out.byte(0x0b);
}

/// What the write tasks share.
const TaskContext = struct {
    plan: *const Layout,
    out: []u8,
    function_offsets: []const u64,
    data_payload: u64,
};

/// Bytes a task copies, about.
const task_chunk: u64 = 1024 * 1024;

const Task = struct {
    context: *const TaskContext,
    work: union(enum) {
        /// Defined functions by defined index.
        functions: struct { first: u32, count: u32 },
        /// Segments of one image.
        segments: struct { image: u32, first: u32, count: u32 },
    },
    bytes: u64 = 0,
    sites: u64 = 0,

    fn run(opaque_task: *anyopaque, _: base.post_check_task_executor.Worker) ?*anyopaque {
        const self: *Task = @ptrCast(@alignCast(opaque_task));
        switch (self.work) {
            .functions => |range| self.writeFunctions(range.first, range.count),
            .segments => |range| self.writeSegments(range.image, range.first, range.count),
        }
        return opaque_task;
    }

    fn writeFunctions(self: *Task, first: u32, count: u32) void {
        const context = self.context;
        const plan = context.plan;
        for (plan.defined.items[first..][0..count], context.function_offsets[first..][0..count], first..) |ref, offset, defined_index| {
            if (ref.image == none) continue;
            const view = plan.program.items[ref.image].view;
            const function = view.functions()[ref.local];
            var prefix: Writer = .{ .buffer = context.out[@intCast(offset)..] };
            prefix.leb(function.body_size);
            const body_out = context.out[@intCast(offset + prefix.at)..][0..function.body_size];
            @memcpy(body_out, view.body(function));
            const room: u64 = if (plan.rooms.len != 0) plan.rooms[defined_index] else 0;
            for (view.functionSites(function)) |site| {
                const value = siteValue(plan, ref.image, site, room);
                const kind: WasmUnitImage.SiteKind = @fromBackingInt(site.kind);
                if (signedKind(kind)) {
                    WasmModule.overwritePaddedI32(body_out, site.offset, @bitCast(value));
                } else {
                    WasmModule.overwritePaddedU32(body_out, site.offset, value);
                }
            }
            self.bytes += prefix.at + function.body_size;
            self.sites += function.site_count;
        }
    }

    fn writeSegments(self: *Task, image: u32, first: u32, count: u32) void {
        const context = self.context;
        const plan = context.plan;
        const view = plan.program.items[image].view;
        const placement = plan.placements[image];
        for (view.segments()[first..][0..count], placement.segments[first..][0..count]) |segment, address| {
            if (address == none or segment.flags & WasmUnitImage.segment_zero_fill != 0) continue;
            const at = context.data_payload + (address - plan.memory.emit_start);
            const out = context.out[@intCast(at)..][0..segment.size];
            @memcpy(out, view.segmentBytes(segment));
            for (view.segmentSites(segment)) |site| {
                std.mem.writeInt(u32, out[site.offset..][0..4], siteValue(plan, image, site, 0), .little);
            }
            self.bytes += segment.size;
            self.sites += segment.site_count;
        }
    }
};

fn signedKind(kind: WasmUnitImage.SiteKind) bool {
    return switch (kind) {
        .table_index_sleb, .table_index_rel_sleb, .memory_addr_sleb, .memory_addr_rel_sleb, .stack_room_sleb => true,
        .function_index_leb, .type_index_leb, .memory_addr_leb, .global_index_leb, .table_number_leb, .memory_addr_i32, .table_index_i32 => false,
    };
}

/// The value a site's field holds in the composed module. Relative forms
/// are relative to the base globals, which are 0.
fn siteValue(plan: *const Layout, image: u32, site: WasmUnitImage.Site, room: u64) u32 {
    const placement = plan.placements[image];
    const target_kind: WasmUnitImage.TargetKind = @fromBackingInt(site.target_kind);
    const kind: WasmUnitImage.SiteKind = @fromBackingInt(site.kind);
    return switch (kind) {
        .function_index_leb => functionTarget(placement, target_kind, site.target),
        .table_index_sleb, .table_index_rel_sleb, .table_index_i32 => plan.slots[functionTarget(placement, target_kind, site.target)],
        .type_index_leb => plan.types.maps[image][site.target],
        .global_index_leb => switch (target_kind) {
            .global => placement.globals[site.target],
            .import => placement.imports[site.target],
            .function, .data, .type => unreachable, // `View` admits only global targets for global sites
        },
        .table_number_leb => 0,
        .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .memory_addr_i32 => blk: {
            const address: u32 = switch (target_kind) {
                .data => placement.segments[site.target],
                .import => placement.imports[site.target],
                .function, .type, .global => unreachable, // `View` admits only data targets for memory sites
            };
            break :blk address +% @as(u32, @bitCast(site.addend));
        },
        .stack_room_sleb => blk: {
            const room32 = std.math.cast(u32, room) orelse base.invariant("wasm stack room invariant violated: check room {d} exceeds the address space", .{room});
            break :blk placement.imports[site.target] +% room32 +% @as(u32, @bitCast(site.addend));
        },
    };
}

fn functionTarget(placement: Placement, target_kind: WasmUnitImage.TargetKind, target: u32) u32 {
    const index = switch (target_kind) {
        .function => placement.functions[target],
        .import => placement.imports[target],
        .data, .type, .global => unreachable, // `View` admits only function targets for function and table sites
    };
    std.debug.assert(index != none);
    return index;
}

fn addTasks(gpa: Allocator, tasks: *std.ArrayList(Task), context: *const TaskContext) Allocator.Error!void {
    const plan = context.plan;
    var first: u32 = 0;
    var bytes: u64 = 0;
    for (plan.defined.items, 0..) |ref, index| {
        if (ref.image != none) bytes += plan.program.items[ref.image].view.functions()[ref.local].body_size;
        if (bytes < task_chunk and index + 1 < plan.defined.items.len) continue;
        try tasks.append(gpa, .{ .context = context, .work = .{ .functions = .{ .first = first, .count = @as(u32, @intCast(index + 1)) - first } } });
        first = @intCast(index + 1);
        bytes = 0;
    }
    for (plan.program.order) |image| {
        const view = plan.program.items[image].view;
        var segment_first: u32 = 0;
        var segment_bytes: u64 = 0;
        const segments = view.segments();
        for (segments, 0..) |segment, index| {
            segment_bytes += segment.size;
            if (segment_bytes < task_chunk and index + 1 < segments.len) continue;
            try tasks.append(gpa, .{ .context = context, .work = .{ .segments = .{ .image = image, .first = segment_first, .count = @as(u32, @intCast(index + 1)) - segment_first } } });
            segment_first = @intCast(index + 1);
            segment_bytes = 0;
        }
    }
}

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

fn now(std_io: std.Io) i96 {
    return std.Io.Timestamp.now(std_io, .awake).nanoseconds;
}

fn elapsed(std_io: std.Io, started: i96) u64 {
    return @intCast(@max(0, now(std_io) - started));
}

/// Link a relocatable object holding every function and segment the app's
/// public names reach, for a platform's own linker. The caller owns the
/// result.
pub fn composeObject(gpa: Allocator, arena: Allocator, std_io: std.Io, inputs: Inputs, timing: *Timing, outcome: *Outcome) LinkError![]u8 {
    std.debug.assert(inputs.host == null);
    const walk_started = now(std_io);
    var program: Program = undefined;
    try walkProgram(gpa, .object, inputs, &program);
    defer program.deinit(gpa);
    try program.walk.init();
    for (inputs.app_public) |name| _ = try reachExport(&program, .object, name);
    for (inputs.provides) |name| {
        const found = program.app_exports.get(name) orelse base.invariant("wasm fast dev link invariant violated: no unit image defines the provided symbol {s}", .{name});
        try program.walk.reachSymbol(found.image, found.symbol);
    }
    try program.walk.run();
    // The platform's linker runs the constructors, so every placed image's
    // are kept.
    for (program.items, 0..) |item, index| {
        if (program.walk.states[index] == null) continue;
        for (item.view.initFuncs()) |init_func| try program.walk.reachImageAtom(@intCast(index), init_func.function);
    }
    try finishWalk(gpa, &program, false);
    timing.walk_ns = elapsed(std_io, walk_started);

    const layout_started = now(std_io);
    var plan = try planLayout(arena, &program, .object, null, outcome);
    try checkParameters(arena, &program, inputs, &plan, outcome);
    timing.layout_ns = elapsed(std_io, layout_started);

    const write_started = now(std_io);
    const bytes = try writeObject(gpa, arena, &plan, inputs);
    timing.write_ns = elapsed(std_io, write_started);
    timing.images_placed = program.order.len;
    timing.functions = plan.functionCount();
    timing.bytes_written = bytes.len;
    return bytes;
}

const SymFlag = backend.wasm.WasmLinking.SymFlag;

/// A symbol of the composed object's symbol table.
const ObjectSymbol = struct {
    kind: enum(u8) { function = 0, data = 1, global = 2, table = 5 },
    flags: u32,
    name: ?[]const u8,
    index: u32 = 0,
    offset: u32 = 0,
    size: u32 = 0,
};

/// An index or address relocation of the composed object.
const ObjectReloc = struct {
    type: u8,
    offset: u32,
    index: u32,
    addend: ?i32,
};

fn writeObject(gpa: Allocator, arena: Allocator, plan: *const Layout, inputs: Inputs) LinkError![]u8 {
    const program = plan.program;
    var symbols: std.ArrayList(ObjectSymbol) = .empty;

    // Undefined functions and globals, by import index.
    const external_symbols = try arena.alloc(u32, plan.externals.items.len);
    for (plan.externals.items, external_symbols, 0..) |external, *symbol, index| {
        symbol.* = @intCast(symbols.items.len);
        const explicit = !std.mem.eql(u8, external.name, external.field);
        try symbols.append(arena, .{
            .kind = if (external.kind == .function) .function else .global,
            .flags = SymFlag.UNDEFINED | (if (explicit) SymFlag.EXPLICIT_NAME else 0),
            .name = if (explicit) external.name else null,
            .index = if (external.kind == .function) @intCast(index) else @as(u32, @intCast(index)) - plan.function_externals,
        });
    }
    const table_symbol: u32 = @intCast(symbols.items.len);
    try symbols.append(arena, .{ .kind = .table, .flags = SymFlag.UNDEFINED | SymFlag.NO_STRIP, .name = null, .index = 0 });

    // Every defined function under a local name, then its public names.
    const function_symbols = try arena.alloc(u32, plan.defined.items.len);
    for (plan.defined.items, function_symbols, 0..) |ref, *symbol, index| {
        symbol.* = @intCast(symbols.items.len);
        const name = definedName(plan, ref);
        try symbols.append(arena, .{
            .kind = .function,
            .flags = SymFlag.BINDING_LOCAL,
            .name = if (name.len != 0) name else try std.fmt.allocPrint(arena, "roc_wasm_function_{d}", .{index}),
            .index = plan.function_externals + @as(u32, @intCast(index)),
        });
    }
    // Every placed segment under a local name.
    const segment_symbols = try arena.alloc([]u32, program.items.len);
    var segment_count: u32 = 0;
    for (program.order) |image| {
        const view = program.items[image].view;
        segment_symbols[image] = try arena.alloc(u32, view.segments().len);
        for (view.segments(), plan.placements[image].segments, segment_symbols[image], 0..) |segment, address, *symbol, index| {
            symbol.* = none;
            if (address == none) continue;
            symbol.* = @intCast(symbols.items.len);
            try symbols.append(arena, .{ .kind = .data, .flags = SymFlag.BINDING_LOCAL, .name = try std.fmt.allocPrint(arena, "roc_wasm_data_{d}_{d}", .{ image, index }), .index = segment_count, .offset = 0, .size = segment.size });
            segment_count += 1;
        }
    }
    // Defined globals under local names.
    const global_symbol_base: u32 = @intCast(symbols.items.len);
    for (plan.globals.items, 0..) |_, index| {
        try symbols.append(arena, .{ .kind = .global, .flags = SymFlag.BINDING_LOCAL, .name = try std.fmt.allocPrint(arena, "roc_wasm_global_{d}", .{index}), .index = plan.global_externals + @as(u32, @intCast(index)) });
    }
    // The public names: global, default visibility.
    for (inputs.app_public) |name| {
        const found = program.app_exports.get(name) orelse continue;
        if (program.walk.states[found.image] == null) continue;
        const view = program.items[found.image].view;
        switch (symbolKind(view, found.symbol)) {
            .function => try symbols.append(arena, .{ .kind = .function, .flags = 0, .name = name, .index = definedValue(plan, found) }),
            .data => {
                const function_exports: u32 = @intCast(view.exports().len);
                const data_export = view.dataExports()[found.symbol - function_exports];
                const segment_symbol = symbols.items[segment_symbols[found.image][data_export.segment]];
                try symbols.append(arena, .{ .kind = .data, .flags = 0, .name = name, .index = segment_symbol.index, .offset = data_export.offset, .size = data_export.size });
            },
            .global => try symbols.append(arena, .{ .kind = .global, .flags = 0, .name = name, .index = definedValue(plan, found) }),
        }
    }
    // Undefined data symbols, by name.
    var data_symbols: std.StringHashMapUnmanaged(u32) = .empty;
    const import_symbols = try arena.alloc([]u32, program.items.len);
    for (program.order) |image| {
        const view = program.items[image].view;
        const state = program.state(image);
        import_symbols[image] = try arena.alloc(u32, view.imports().len);
        for (view.imports(), state.targets, state.imports_used, plan.placements[image].imports, import_symbols[image]) |import, target, used, value, *symbol| {
            symbol.* = none;
            if (!used) continue;
            switch (target) {
                .defined => |found| symbol.* = switch (symbolKind(program.items[found.image].view, found.symbol)) {
                    .function => function_symbols[value - plan.function_externals],
                    .data => blk: {
                        const found_view = program.items[found.image].view;
                        const data_export = found_view.dataExports()[found.symbol - @as(u32, @intCast(found_view.exports().len))];
                        break :blk segment_symbols[found.image][data_export.segment];
                    },
                    .global => global_symbol_base + (value - plan.global_externals),
                },
                .external => switch (@as(WasmUnitImage.ImportKind, @fromBackingInt(import.kind))) {
                    .function, .global => symbol.* = external_symbols[if (import.kind == @backingInt(WasmUnitImage.ImportKind.function)) value else value + plan.function_externals],
                    .table => symbol.* = table_symbol,
                    .data => {
                        const name = view.name(import.name);
                        const gop = try data_symbols.getOrPut(arena, name);
                        if (!gop.found_existing) {
                            gop.value_ptr.* = @intCast(symbols.items.len);
                            try symbols.append(arena, .{ .kind = .data, .flags = SymFlag.UNDEFINED, .name = name });
                        }
                        symbol.* = gop.value_ptr.*;
                    },
                },
                .synthetic, .missing => unreachable, // an object resolves an import to a definition or leaves it undefined
            }
        }
    }

    // Sizes: type, import, function, global, code, data.
    var import_payload: u64 = lebSize(plan.externals.items.len + 2);
    for (plan.externals.items) |external| {
        import_payload += nameSize(external.module) + nameSize(external.field) + 1;
        import_payload += switch (external.kind) {
            .function => lebSize(external.type),
            .global => 2,
            .table, .data => unreachable, // only functions and globals are imported by index
        };
    }
    const memory_pages = std.math.divCeil(u64, plan.memory.data_end, page_size) catch unreachable; // the page size is nonzero
    import_payload += nameSize("env") + nameSize("__linear_memory") + 1 + 1 + lebSize(memory_pages);
    import_payload += nameSize("env") + nameSize("__indirect_function_table") + 1 + 1 + 1 + lebSize(0);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);
    try out.appendSlice(gpa, "\x00asm\x01\x00\x00\x00");
    var section_index: u32 = 0;

    // Type section.
    try appendSection(gpa, &out, 1, plan.types.encodedSize(), struct {
        fn write(w: *Writer, layout_plan: *const Layout) void {
            layout_plan.types.encode(w);
        }
    }.write, plan);
    section_index += 1;

    // Import section.
    {
        const start = try beginSection(gpa, &out, 2, import_payload);
        var w: Writer = .{ .buffer = out.items[start..] };
        w.leb(plan.externals.items.len + 2);
        w.name("env");
        w.name("__linear_memory");
        w.byte(2);
        w.byte(0);
        w.leb(memory_pages);
        w.name("env");
        w.name("__indirect_function_table");
        w.byte(1);
        w.byte(0x70);
        w.byte(0);
        w.leb(0);
        for (plan.externals.items) |external| {
            w.name(external.module);
            w.name(external.field);
            if (external.kind == .function) {
                w.byte(0);
                w.leb(external.type);
            } else {
                w.byte(3);
                w.byte(external.valtype);
                w.byte(@intFromBool(external.mutable));
            }
        }
        std.debug.assert(w.at == import_payload);
        section_index += 1;
    }

    // Function section.
    if (plan.defined.items.len != 0) {
        var payload: u64 = lebSize(plan.defined.items.len);
        for (plan.defined.items) |ref| payload += lebSize(definedType(plan, ref));
        const start = try beginSection(gpa, &out, 3, payload);
        var w: Writer = .{ .buffer = out.items[start..] };
        w.leb(plan.defined.items.len);
        for (plan.defined.items) |ref| w.leb(definedType(plan, ref));
        section_index += 1;
    }

    // Global section.
    if (plan.globals.items.len != 0) {
        var payload: u64 = lebSize(plan.globals.items.len);
        for (plan.globals.items) |global| payload += 2 + initExprSize(global);
        const start = try beginSection(gpa, &out, 6, payload);
        var w: Writer = .{ .buffer = out.items[start..] };
        w.leb(plan.globals.items.len);
        for (plan.globals.items) |global| {
            w.byte(global.valtype);
            w.byte(@intFromBool(global.mutable));
            writeInitExpr(&w, global);
        }
        section_index += 1;
    }

    // Code section, with every site a relocation.
    var code_relocs: std.ArrayList(ObjectReloc) = .empty;
    var code_section: ?u32 = null;
    if (plan.defined.items.len != 0) {
        var payload: u64 = lebSize(plan.defined.items.len);
        const ctors_body = ctorsBodySize(plan);
        for (plan.defined.items) |ref| {
            const size = definedBodySize(plan, ref, ctors_body);
            payload += lebSize(size) + size;
        }
        const start = try beginSection(gpa, &out, 10, payload);
        var w: Writer = .{ .buffer = out.items[start..] };
        w.leb(plan.defined.items.len);
        for (plan.defined.items, 0..) |ref, defined_index| {
            std.debug.assert(ref.image != none); // an object never synthesizes a constructor caller
            const view = program.items[ref.image].view;
            const function = view.functions()[ref.local];
            w.leb(function.body_size);
            const body_at = w.at;
            w.bytes(view.body(function));
            const body_out = w.buffer[body_at..][0..function.body_size];
            const room: u64 = if (plan.rooms.len != 0) plan.rooms[defined_index] else 0;
            for (view.functionSites(function)) |site| {
                const reloc = objectReloc(plan, ref.image, site, room, .{ .functions = function_symbols, .segments = segment_symbols, .imports = import_symbols, .global_base = global_symbol_base });
                const kind: WasmUnitImage.SiteKind = @fromBackingInt(site.kind);
                const value = objectFieldValue(plan, ref.image, site, room);
                if (signedKind(kind)) WasmModule.overwritePaddedI32(body_out, site.offset, @bitCast(value)) else WasmModule.overwritePaddedU32(body_out, site.offset, value);
                try code_relocs.append(arena, .{ .type = reloc.type, .offset = @intCast(body_at + site.offset), .index = reloc.index, .addend = reloc.addend });
            }
        }
        std.debug.assert(w.at == payload);
        code_section = section_index;
        section_index += 1;
    }

    // Data section: one active segment per placed segment, at its virtual
    // address; zero-filled ones hold their zeros.
    var data_relocs: std.ArrayList(ObjectReloc) = .empty;
    var data_section: ?u32 = null;
    var segment_infos: std.ArrayList(struct { name: []const u8, alignment: u32 }) = .empty;
    if (segment_count != 0) {
        var payload: u64 = lebSize(segment_count);
        for (program.order) |image| {
            const view = program.items[image].view;
            for (view.segments(), plan.placements[image].segments) |segment, address| {
                if (address == none) continue;
                payload += 1 + 1 + slebSize(@as(i32, @bitCast(address))) + 1 + lebSize(segment.size) + segment.size;
            }
        }
        const start = try beginSection(gpa, &out, 11, payload);
        var w: Writer = .{ .buffer = out.items[start..] };
        w.leb(segment_count);
        for (program.order) |image| {
            const view = program.items[image].view;
            for (view.segments(), plan.placements[image].segments, 0..) |segment, address, index| {
                if (address == none) continue;
                w.leb(0);
                w.byte(0x41);
                w.sleb(@as(i32, @bitCast(address)));
                w.byte(0x0b);
                w.leb(segment.size);
                const bytes_at = w.at;
                if (segment.flags & WasmUnitImage.segment_zero_fill != 0) {
                    @memset(w.buffer[w.at..][0..segment.size], 0);
                    w.at += segment.size;
                } else {
                    w.bytes(view.segmentBytes(segment));
                }
                const segment_out = w.buffer[bytes_at..][0..segment.size];
                for (view.segmentSites(segment)) |site| {
                    const reloc = objectReloc(plan, image, site, 0, .{ .functions = function_symbols, .segments = segment_symbols, .imports = import_symbols, .global_base = global_symbol_base });
                    std.mem.writeInt(u32, segment_out[site.offset..][0..4], objectFieldValue(plan, image, site, 0), .little);
                    try data_relocs.append(arena, .{ .type = reloc.type, .offset = @intCast(bytes_at + site.offset), .index = reloc.index, .addend = reloc.addend });
                }
                const zero_fill = segment.flags & WasmUnitImage.segment_zero_fill != 0;
                try segment_infos.append(arena, .{ .name = try std.fmt.allocPrint(arena, "{s}.roc_{d}_{d}", .{ if (zero_fill) ".bss" else ".data", image, index }), .alignment = segment.alignment_log2 });
            }
        }
        std.debug.assert(w.at == payload);
        data_section = section_index;
        section_index += 1;
    }

    // The linking section: symbols, segment info, constructors.
    var linking: std.ArrayList(u8) = .empty;
    try linking.append(arena, 2);
    {
        var table_bytes: std.ArrayList(u8) = .empty;
        try appendLeb(arena, &table_bytes, symbols.items.len);
        for (symbols.items) |symbol| {
            try table_bytes.append(arena, @backingInt(symbol.kind));
            try appendLeb(arena, &table_bytes, symbol.flags);
            switch (symbol.kind) {
                .function, .global, .table => {
                    try appendLeb(arena, &table_bytes, symbol.index);
                    if (symbol.name) |name| try appendName(arena, &table_bytes, name);
                },
                .data => {
                    try appendName(arena, &table_bytes, symbol.name.?);
                    if (symbol.flags & SymFlag.UNDEFINED == 0) {
                        try appendLeb(arena, &table_bytes, symbol.index);
                        try appendLeb(arena, &table_bytes, symbol.offset);
                        try appendLeb(arena, &table_bytes, symbol.size);
                    }
                },
            }
        }
        try linking.append(arena, 8);
        try appendLeb(arena, &linking, table_bytes.items.len);
        try linking.appendSlice(arena, table_bytes.items);
    }
    if (segment_infos.items.len != 0) {
        var info: std.ArrayList(u8) = .empty;
        try appendLeb(arena, &info, segment_infos.items.len);
        for (segment_infos.items) |segment| {
            try appendName(arena, &info, segment.name);
            try appendLeb(arena, &info, segment.alignment);
            try appendLeb(arena, &info, 0);
        }
        try linking.append(arena, 5);
        try appendLeb(arena, &linking, info.items.len);
        try linking.appendSlice(arena, info.items);
    }
    {
        var init: std.ArrayList(u8) = .empty;
        var count: u32 = 0;
        for (program.order) |image| {
            const view = program.items[image].view;
            for (view.initFuncs()) |init_func| {
                const function = plan.placements[image].functions[init_func.function];
                try appendLeb(arena, &init, init_func.priority);
                try appendLeb(arena, &init, function_symbols[function - plan.function_externals]);
                count += 1;
            }
        }
        if (count != 0) {
            var subsection: std.ArrayList(u8) = .empty;
            try appendLeb(arena, &subsection, count);
            try subsection.appendSlice(arena, init.items);
            try linking.append(arena, 6);
            try appendLeb(arena, &linking, subsection.items.len);
            try linking.appendSlice(arena, subsection.items);
        }
    }
    try appendCustom(gpa, &out, "linking", linking.items);
    if (code_section) |index| try appendRelocSection(gpa, arena, &out, "reloc.CODE", index, code_relocs.items);
    if (data_section) |index| try appendRelocSection(gpa, arena, &out, "reloc.DATA", index, data_relocs.items);
    return out.toOwnedSlice(gpa);
}

/// The relocation a site of `image` becomes in the object.
/// The object's symbol index of each thing a site names.
const ObjectSymbols = struct {
    /// By defined function index.
    functions: []const u32,
    /// By image, then segment.
    segments: []const []u32,
    /// By image, then import.
    imports: []const []u32,
    /// The symbol of the first defined global.
    global_base: u32,
};

fn objectReloc(plan: *const Layout, image: u32, site: WasmUnitImage.Site, room: u64, symbols: ObjectSymbols) ObjectReloc {
    const kind: WasmUnitImage.SiteKind = @fromBackingInt(site.kind);
    const target_kind: WasmUnitImage.TargetKind = @fromBackingInt(site.target_kind);
    const placement = plan.placements[image];
    const symbol: u32 = switch (target_kind) {
        .function => symbols.functions[placement.functions[site.target] - plan.function_externals],
        .data => symbols.segments[image][site.target],
        .import => symbols.imports[image][site.target],
        .global => symbols.global_base + (placement.globals[site.target] - plan.global_externals),
        .type => plan.types.maps[image][site.target],
    };
    return switch (kind) {
        .function_index_leb => .{ .type = 0, .offset = 0, .index = symbol, .addend = null },
        .table_index_sleb => .{ .type = 1, .offset = 0, .index = symbol, .addend = null },
        .table_index_i32 => .{ .type = 2, .offset = 0, .index = symbol, .addend = null },
        .memory_addr_leb => .{ .type = 3, .offset = 0, .index = symbol, .addend = site.addend },
        .memory_addr_sleb => .{ .type = 4, .offset = 0, .index = symbol, .addend = site.addend },
        .memory_addr_i32 => .{ .type = 5, .offset = 0, .index = symbol, .addend = site.addend },
        .type_index_leb => .{ .type = 6, .offset = 0, .index = symbol, .addend = null },
        .global_index_leb => .{ .type = 7, .offset = 0, .index = symbol, .addend = null },
        .memory_addr_rel_sleb => .{ .type = 11, .offset = 0, .index = symbol, .addend = site.addend },
        .table_index_rel_sleb => .{ .type = 12, .offset = 0, .index = symbol, .addend = null },
        .table_number_leb => .{ .type = 20, .offset = 0, .index = symbol, .addend = null },
        .stack_room_sleb => .{ .type = 4, .offset = 0, .index = symbol, .addend = site.addend +% (std.math.cast(i32, room) orelse base.invariant("wasm stack room invariant violated: check room {d} exceeds the address space", .{room})) },
    };
}

/// The value an object's field holds before its linker relocates it: the
/// object's own index or virtual address, as compilers write it.
fn objectFieldValue(plan: *const Layout, image: u32, site: WasmUnitImage.Site, room: u64) u32 {
    _ = room;
    const kind: WasmUnitImage.SiteKind = @fromBackingInt(site.kind);
    const target_kind: WasmUnitImage.TargetKind = @fromBackingInt(site.target_kind);
    const placement = plan.placements[image];
    return switch (kind) {
        .function_index_leb => switch (target_kind) {
            .function => placement.functions[site.target],
            .import => placement.imports[site.target],
            .data, .type, .global => unreachable, // `View` admits only function targets for function sites
        },
        .type_index_leb => plan.types.maps[image][site.target],
        .global_index_leb => switch (target_kind) {
            .global => placement.globals[site.target],
            .import => placement.imports[site.target],
            .function, .data, .type => unreachable, // `View` admits only global targets for global sites
        },
        .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .memory_addr_i32 => switch (target_kind) {
            .data => placement.segments[site.target] +% @as(u32, @bitCast(site.addend)),
            .import => 0,
            .function, .type, .global => unreachable, // `View` admits only data targets for memory sites
        },
        .table_index_sleb, .table_index_rel_sleb, .table_index_i32, .table_number_leb, .stack_room_sleb => 0,
    };
}

fn appendLeb(arena: Allocator, out: *std.ArrayList(u8), value: u64) Allocator.Error!void {
    var rest = value;
    while (true) {
        const low: u8 = @truncate(rest & 0x7f);
        rest >>= 7;
        if (rest == 0) return out.append(arena, low);
        try out.append(arena, low | 0x80);
    }
}

fn appendName(arena: Allocator, out: *std.ArrayList(u8), text: []const u8) Allocator.Error!void {
    try appendLeb(arena, out, text.len);
    try out.appendSlice(arena, text);
}

/// Append a section header for a payload of `payload` bytes and room for
/// the payload; returns where the payload starts.
fn beginSection(gpa: Allocator, out: *std.ArrayList(u8), id: u8, payload: u64) Allocator.Error!usize {
    try out.append(gpa, id);
    try appendLeb(gpa, out, payload);
    const start = out.items.len;
    try out.appendNTimes(gpa, 0, @intCast(payload));
    return start;
}

fn appendSection(gpa: Allocator, out: *std.ArrayList(u8), id: u8, payload: u64, comptime write: fn (*Writer, *const Layout) void, plan: *const Layout) Allocator.Error!void {
    const start = try beginSection(gpa, out, id, payload);
    var w: Writer = .{ .buffer = out.items[start..] };
    write(&w, plan);
    std.debug.assert(w.at == payload);
}

fn appendCustom(gpa: Allocator, out: *std.ArrayList(u8), name: []const u8, payload: []const u8) Allocator.Error!void {
    try out.append(gpa, 0);
    try appendLeb(gpa, out, nameSize(name) + payload.len);
    try appendName(gpa, out, name);
    try out.appendSlice(gpa, payload);
}

fn appendRelocSection(gpa: Allocator, arena: Allocator, out: *std.ArrayList(u8), name: []const u8, section: u32, relocs: []ObjectReloc) Allocator.Error!void {
    std.mem.sort(ObjectReloc, relocs, {}, struct {
        fn lessThan(_: void, a: ObjectReloc, b: ObjectReloc) bool {
            return a.offset < b.offset;
        }
    }.lessThan);
    var payload: std.ArrayList(u8) = .empty;
    try appendLeb(arena, &payload, section);
    try appendLeb(arena, &payload, relocs.len);
    for (relocs) |reloc| {
        try payload.append(arena, reloc.type);
        try appendLeb(arena, &payload, reloc.offset);
        try appendLeb(arena, &payload, reloc.index);
        if (reloc.addend) |addend| {
            var w_buffer: [10]u8 = undefined;
            var w: Writer = .{ .buffer = &w_buffer };
            w.sleb(addend);
            try payload.appendSlice(arena, w_buffer[0..w.at]);
        }
    }
    try appendCustom(gpa, out, name, payload.items);
}

/// A padded LEB128 zero, as every code site's field holds in an image.
const test_zero_leb = [5]u8{ 0x80, 0x80, 0x80, 0x80, 0x00 };

/// A body of `code` (whose sites hold padded zeros), after an empty locals
/// declaration.
fn testBody(allocator: Allocator, code: []const []const u8) Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    try out.append(allocator, 0);
    for (code) |piece| try out.appendSlice(allocator, piece);
    return out.toOwnedSlice(allocator);
}

test "a composed module places the reached code and data and patches every site" {
    if (comptime @import("builtin").os.tag != .linux) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // The host: `wasm_main` calls the app's `roc_main`, and a function
    // nothing reaches imports a function from the embedder.
    const host_main = try testBody(arena, &.{ &.{0x10}, &test_zero_leb, &.{0x0b} });
    const host_dead = try testBody(arena, &.{ &.{0x10}, &test_zero_leb, &.{0x0b} });
    const host = try WasmUnitImage.encode(allocator, .{
        .stack_checks = false,
        .types = &.{ .{ .params = &.{}, .results = &.{0x7f} }, .{ .params = &.{}, .results = &.{} } },
        .functions = &.{
            .{ .body = host_main, .type = 0, .export_name = "wasm_main", .sites = &.{.{ .offset = 2, .kind = .function_index_leb, .target_kind = .import, .target = 0, .addend = 0 }} },
            .{ .body = host_dead, .type = 1, .export_name = "dead", .sites = &.{.{ .offset = 2, .kind = .function_index_leb, .target_kind = .import, .target = 1, .addend = 0 }} },
        },
        .segments = &.{},
        .data_exports = &.{},
        .table_requests = &.{},
        .imports = &.{
            .{ .module = "env", .name = "roc_main", .kind = .function, .type = 0 },
            .{ .module = "env", .name = "canary", .kind = .function, .type = 1 },
        },
    });
    defer allocator.free(host);
    // The support code: nothing the program reaches.
    const support_body = try testBody(arena, &.{&.{0x0b}});
    const support = try WasmUnitImage.encode(allocator, .{
        .stack_checks = false,
        .types = &.{.{ .params = &.{}, .results = &.{} }},
        .functions = &.{.{ .body = support_body, .type = 0, .export_name = "roc_builtins_unused", .sites = &.{} }},
        .segments = &.{},
        .data_exports = &.{},
        .table_requests = &.{},
        .imports = &.{},
    });
    defer allocator.free(support);
    // The root unit: `roc_main` returns the address of the second word of
    // its data, through `__memory_base`.
    const root_body = try testBody(arena, &.{ &.{0x23}, &test_zero_leb, &.{0x41}, &test_zero_leb, &.{ 0x6a, 0x0b } });
    const root = try WasmUnitImage.encode(allocator, .{
        .stack_checks = false,
        .types = &.{.{ .params = &.{}, .results = &.{0x7f} }},
        .functions = &.{.{ .body = root_body, .type = 0, .export_name = "roc_main", .sites = &.{
            .{ .offset = 2, .kind = .global_index_leb, .target_kind = .import, .target = 0, .addend = 0 },
            .{ .offset = 8, .kind = .memory_addr_rel_sleb, .target_kind = .data, .target = 0, .addend = 4 },
        } }},
        .segments = &.{.{ .bytes = "abcdefgh", .size = 8, .alignment_log2 = 3, .sites = &.{} }},
        .data_exports = &.{},
        .table_requests = &.{},
        .imports = &.{.{ .module = "env", .name = "__memory_base", .kind = .global, .valtype = 0x7f }},
    });
    defer allocator.free(root);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", arena);
    const config: ModuleConfig = .{
        .exports = &.{"wasm_main"},
        .stack_exports = &.{ "__stack_pointer", "__stack_low", "__stack_high" },
        .stack_size = 64 * 1024,
        .global_base = null,
        .initial_memory = null,
        .maximum_memory = null,
        .import_memory = false,
        .zero_filled_memory = true,
        .names = false,
    };
    var outputs: [2][]u8 = undefined;
    for (&outputs, 0..) |*bytes, index| {
        const path = try std.fmt.allocPrint(arena, "{s}/out{d}.wasm", .{ dir, index });
        var output = try OutputFile.create(arena, path);
        var timing: Timing = .{};
        var outcome: Outcome = .{};
        const units = [_]View{try View.init(root)};
        try composeModule(allocator, arena, std.testing.io, .{
            .host = try View.init(host),
            .support = try View.init(support),
            .units = &units,
            .root = 0,
            .app_public = &.{"roc_main"},
            .provides = &.{"roc_main"},
            .hosted = &.{},
        }, config, &output, null, &timing, &outcome);
        try output.publish(arena);
        bytes.* = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, path, arena, .unlimited);
    }
    // The composition is a function of its inputs.
    try std.testing.expectEqualSlices(u8, outputs[0], outputs[1]);

    var module = try WasmModule.preload(allocator, outputs[0], .executable);
    defer module.deinit();
    // The canary only dead host code names is not imported, and the
    // support code nothing reaches is not placed.
    try std.testing.expectEqual(@as(usize, 0), module.imports.items.len);
    try std.testing.expectEqual(@as(usize, 2), module.function_offsets.items.len);
    var exported_main = false;
    for (module.exports.items) |entry| {
        if (std.mem.eql(u8, entry.name, "wasm_main")) exported_main = true;
        try std.testing.expect(!std.mem.eql(u8, entry.name, "dead"));
    }
    try std.testing.expect(exported_main);
    // `wasm_main` calls `roc_main`, the second function; `roc_main` adds 4
    // to its data's address, right above the stack.
    const code = module.code_bytes.items;
    var cursor: usize = module.function_offsets.items[0];
    _ = try WasmModule.readU32(code, &cursor);
    cursor += 2;
    try std.testing.expectEqual(@as(u32, 1), try WasmModule.readU32(code, &cursor));
    cursor = module.function_offsets.items[1];
    _ = try WasmModule.readU32(code, &cursor);
    cursor += 8;
    try std.testing.expectEqual(@as(i32, 64 * 1024 + 4), try WasmModule.readI32(code, &cursor));
    try std.testing.expectEqual(@as(usize, 1), module.data_segments.items.len);
    try std.testing.expectEqualStrings("abcdefgh", module.data_segments.items[0].data);
    try std.testing.expectEqual(@as(u32, 64 * 1024), module.data_segments.items[0].offset);
}
