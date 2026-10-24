//! Per-region artifacts of dev-backend machine code.
//!
//! The code generator emits one buffer for a whole program, but it keeps every
//! reference from that buffer into itself symbolic until a final patch pass
//! and logs each range it emits with its producer (`LirCodeGen.CodeRegion`,
//! `LirCodeGen.CodeRef`). An artifact is one such range lifted out of the
//! buffer: its bytes, set-local references (region index plus delta), stable
//! references to not-yet-emitted producers, named relocations, source lines,
//! and frame metadata. Procedures and refcount helpers are named by content
//! (`ProcIdentity`, `roc__rc_*`), so an artifact set from one program can be
//! placed into another program's buffer with `append`, then resolved after
//! all required producers have landed. `assemble` performs both steps.
//! Source-file indices are in the set's own file domain: `Set.source_files`
//! names each index by its module's content identity. A session's fragments
//! carry no table: they all index the session's one source-file table, and
//! session appends keep their indices as they are. `splice` binds a set's
//! indices to the receiving program's table, omitting the lines of module
//! versions that program does not have.
//!
//! `verifyRoundTrip` is the gate for the format: a program assembled from its
//! own artifacts must produce the same bytes, relocations, and unwind records
//! as compiling it directly.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const Sha256 = base.Sha256;
const lir = @import("lir");
const layout = @import("layout");
const RelocationMod = @import("Relocation.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const SymbolTable = @import("SymbolTable.zig");

/// Whether a symbol's name means the same thing in every program.
pub const SymbolScope = SymbolTable.Scope;

const Allocator = std.mem.Allocator;
const IndexedRelocation = RelocationMod.IndexedRelocation;

/// Producer-selected encoding: direct branch, inline address-plus-call, or address.
pub const Form = enum { call, inline_call, addr };

fn codeRefForm(form: Form) LirCodeGenMod.SymbolicRefForm {
    return switch (form) {
        .call => .call,
        .inline_call => .inline_call,
        .addr => .addr,
    };
}

/// A producer-identified target whose code need not have been emitted yet.
pub const SymbolicReference = struct {
    site: u32,
    form: Form,
    /// A producer-reserved in-body veneer, still repatched by final placement.
    veneer: ?u32 = null,
    target: union(enum) {
        proc: lir.ProcIdentity,
        rc_helper: []const u8,
        boxy_thunk: lir.ProcIdentity,
    },
};

/// Region-relative offsets, preserving producer order even at equal offsets.
/// SourceLoc.file belongs to the originating LIR source-file table.
pub const LineEntry = LirCodeGenMod.LineEntry;

/// Region-relative failure sites (`LirCodeGen.FailureSite`).
pub const FailureSite = LirCodeGenMod.FailureSite;

/// A reference from an artifact's bytes to a location inside another artifact.
pub const Reference = struct {
    /// Offset of the CALL/BL or ADR/LEA instruction within the artifact.
    site: u32,
    form: Form,
    /// Index of the target artifact within the set.
    target: u32,
    /// Offset within the target artifact the reference resolves to.
    delta: u32,
    veneer: ?u32 = null,
};

/// A runtime table number the artifact's code loads, with the offset of its
/// patchable load relative to the artifact.
pub const RuntimeReference = struct {
    site: u32,
    ref: RelocationMod.RuntimeRef,
};

/// A relocation against a named symbol, with its offset relative to the artifact.
pub const NamedRelocation = struct {
    offset: u32,
    name: []const u8,
    /// Whether `name` means the same thing in another program; declared by
    /// the code that made the reference.
    scope: SymbolScope,
    kind: union(enum) {
        function: RelocationMod.FunctionRelocationKind,
        data: RelocationMod.DataRelocationKind,
    },
};

/// Frame metadata of a procedure-shaped artifact, re-recorded as an unwind
/// entry wherever the artifact lands.
pub const Frame = struct {
    prologue_size: u32,
    stack_alloc: u32,
    frame_size: u32,
    callee_saved_mask: u32,
    epilogue_offset: u32,
    uses_frame_pointer: bool,
};

/// What an artifact holds. Procedures, helpers, and thunks are named by
/// content; the rest are program-local ranges that only ever travel with
/// their own program.
pub const Kind = union(enum) {
    proc: lir.ProcIdentity,
    /// Symbol name of the helper (`roc__rc_*`).
    rc_helper: []const u8,
    boxy_thunk: lir.ProcIdentity,
    entrypoint,
    message_pool_run,
    /// Never lifted as a standalone artifact: islands belong to placement.
    /// Reserved in-body island bytes travel with their owning procedure and
    /// are repatched there. The tag preserves pack numbering.
    branch_island,
};

/// A readonly datum the artifact's relocations name and its program
/// defined: carried so a program that lacks it can define it.
pub const DataItem = struct {
    name: []const u8,
    bytes: []const u8,
    alignment: u32,
    symbol_offset: u32,
    /// Pointers inside `bytes` to other symbols: constants, literal
    /// backings, procedures, or refcount helpers.
    relocations: []const DataRelocation = &.{},
    /// `name` is the producing program's own name for an internal constant
    /// (`roc__d{N}`, `roc__d{N}_{k}`), which another program
    /// can give to a different constant. A pack names such a datum by
    /// content instead; see `ContentNames`.
    program_local_name: bool = false,
};

/// One pointer-sized relocation inside a data item.
pub const DataRelocation = struct {
    offset: u32,
    name: []const u8,
    addend: i64,
    /// The target is code (a procedure or refcount helper) rather than data.
    function: bool,
    /// Exact external binding supplied by the linking image, not carried data.
    external: bool = false,
};

/// Prefix of a datum named by content, the same wherever it lands.
pub const content_data_prefix = lir.Program.content_data_symbol_prefix;

/// One lifted region of machine code.
pub const Artifact = struct {
    kind: Kind,
    code: []const u8,
    /// Offset within `code` that references to this artifact resolve to.
    entry: u32,
    frame: ?Frame,
    refs: []const Reference,
    symbolic_refs: []const SymbolicReference = &.{},
    lines: []const LineEntry = &.{},
    failure_sites: []const FailureSite = &.{},
    relocations: []const NamedRelocation,
    /// The Boxy runtime table numbers the code loads, in the producing
    /// program's numbering until a canonical image renumbers them.
    runtime_refs: []const RuntimeReference = &.{},
    data: []const DataItem,
    /// The source location a procedure's debug entry names: its procedure's
    /// location (`LirStore.procLoc`) in the set's source-file domain, the
    /// same wherever the procedure lands. `none` for every other kind.
    loc: base.SourceLoc = .none,
};

/// An ordered set of artifacts covering one code buffer. Order is emission
/// order, which `assemble` reproduces.
pub const Set = struct {
    arena: collections.SingleThreadArena,
    artifacts: []const Artifact,
    /// The module content identity of each source-file index the artifacts'
    /// lines use (`LirStore.sourceFileModuleIdentity`).
    source_files: []const [32]u8 = &.{},

    pub fn deinit(self: *Set) void {
        self.arena.deinit();
    }

    /// The index of the artifact defining `target`, a symbolic reference's
    /// target, or null when this set does not define it.
    pub fn definitionOf(self: *const Set, target: @FieldType(SymbolicReference, "target")) ?u32 {
        for (self.artifacts, 0..) |candidate, candidate_index| {
            const matches = switch (target) {
                .proc => |identity| candidate.kind == .proc and std.meta.eql(identity, candidate.kind.proc),
                .boxy_thunk => |identity| candidate.kind == .boxy_thunk and std.meta.eql(identity, candidate.kind.boxy_thunk),
                .rc_helper => |name| candidate.kind == .rc_helper and std.mem.eql(u8, name, candidate.kind.rc_helper),
            };
            if (matches) return @intCast(candidate_index);
        }
        return null;
    }
};

/// A fragment owns its requirement list in the same arena as its artifacts.
pub const Fragment = struct {
    set: Set,
    required_helpers: []const u64,
    context_dependencies: LirCodeGenMod.FragmentContextDependencies,

    pub fn clone(self: *const Fragment, allocator: Allocator) Allocator.Error!Fragment {
        var set = try copySet(allocator, &self.set);
        errdefer set.deinit();
        const required_helpers = try set.arena.allocator().dupe(u64, self.required_helpers);
        return .{
            .set = set,
            .required_helpers = required_helpers,
            .context_dependencies = self.context_dependencies,
        };
    }

    pub fn deinit(self: *Fragment) void {
        self.set.deinit();
    }
};

/// Emit one body against the producer's LIR metadata, then sever all workspace
/// ownership. No other procedure or helper body is needed to extract this set.
pub fn compileProcFragment(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_id: lir.LIR.LirProcSpecId,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
) ExtractError!Fragment {
    try codegen.emitProcFragment(proc_id);
    return captureFragment(CG, allocator, codegen, proc_specs, layout_store, string_exports, constant_exports);
}

fn captureFragment(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
) ExtractError!Fragment {
    var prepared = try PreparedData.init(allocator, string_exports, constant_exports, &.{});
    defer prepared.deinit();
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, &prepared);
}

/// Emit a procedure using the coordinator's shared immutable data catalog.
pub fn compileProcFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_id: lir.LIR.LirProcSpecId,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    try codegen.emitProcFragment(proc_id);
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
}

/// Emit a helper using the coordinator's shared immutable data catalog.
pub fn compileRcHelperFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    key: u64,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    try codegen.emitRcHelperFragment(key);
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
}

fn captureFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    var set = try extractPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
    errdefer set.deinit();
    const required_helpers = try codegen.getRequiredRcHelpers(set.arena.allocator());
    return .{
        .set = set,
        .required_helpers = required_helpers,
        .context_dependencies = codegen.getFragmentContextDependencies(),
    };
}

fn cloneData(a: Allocator, item: DataItem) Allocator.Error!DataItem {
    const relocations = try a.dupe(DataRelocation, item.relocations);
    var copier = NameCopier{};
    for (relocations) |*relocation| relocation.name = try copier.copy(a, relocation.name);
    return .{
        .name = try a.dupe(u8, item.name),
        .bytes = try a.dupe(u8, item.bytes),
        .alignment = item.alignment,
        .symbol_offset = item.symbol_offset,
        .relocations = relocations,
        .program_local_name = item.program_local_name,
    };
}

/// Copies relocation target names into an allocator, once for each run of
/// relocations in a row that name the same symbol, which share the copy: a
/// list of copies of one value names it once per item.
const NameCopier = struct {
    source: ?[]const u8 = null,
    copied: []const u8 = &.{},

    fn copy(self: *NameCopier, a: Allocator, name: []const u8) Allocator.Error![]const u8 {
        if (self.source) |source| if (std.mem.eql(u8, source, name)) return self.copied;
        self.copied = try a.dupe(u8, name);
        self.source = name;
        return self.copied;
    }
};

/// Deep-copy sets into one ownership domain, rebasing only set-local references.
/// Callers select cache entries using their full compilation provenance.
pub fn combine(allocator: Allocator, sets: []const *const Set) Allocator.Error!Set {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    var artifacts = std.ArrayList(Artifact).empty;
    var source_files = std.ArrayList([32]u8).empty;
    for (sets) |set| {
        const file_map = try a.alloc(u32, set.source_files.len);
        for (set.source_files, file_map) |identity, *mapped| {
            mapped.* = for (source_files.items, 0..) |existing, index| {
                if (std.mem.eql(u8, &existing, &identity)) break @intCast(index);
            } else index: {
                try source_files.append(a, identity);
                break :index @intCast(source_files.items.len - 1);
            };
        }
        try copyArtifacts(a, &artifacts, set, file_map);
    }
    const owned = try artifacts.toOwnedSlice(a);
    const owned_source_files = try source_files.toOwnedSlice(a);
    return .{ .arena = arena, .artifacts = owned, .source_files = owned_source_files };
}

/// Deep-copy sets compiled from programs of one Solved program, which name a
/// procedure, refcount helper, or Boxy thunk only by the code they hold, into
/// one ownership domain with one copy of each: a later set's copy of one is
/// dropped, and references to it resolve to the first copy. The sets share
/// one source-file table, as programs of one Solved program do.
pub fn combineDistinct(allocator: Allocator, sets: []const *const Set) Allocator.Error!Set {
    const source_files: []const [32]u8 = if (sets.len == 0) &.{} else sets[0].source_files;
    if (@import("builtin").mode == .debug) for (sets) |set| {
        if (set.source_files.len != source_files.len) base.invariant("{s}", .{"combined artifact sets index different source-file tables"});
        for (set.source_files, source_files) |left, right| {
            if (!std.mem.eql(u8, &left, &right)) base.invariant("{s}", .{"combined artifact sets index different source-file tables"});
        }
    };
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    const Named = union(enum) { proc: lir.ProcIdentity, thunk: lir.ProcIdentity, helper: []const u8 };
    const NamedContext = struct {
        pub fn hash(_: @This(), key: Named) u64 {
            var hasher = std.hash.Wyhash.init(0);
            switch (key) {
                .proc => |identity| {
                    hasher.update("p");
                    hasher.update(&identity.bytes);
                },
                .thunk => |identity| {
                    hasher.update("t");
                    hasher.update(&identity.bytes);
                },
                .helper => |name| {
                    hasher.update("h");
                    hasher.update(name);
                },
            }
            return hasher.final();
        }
        pub fn eql(_: @This(), left: Named, right: Named) bool {
            return switch (left) {
                .proc => |identity| right == .proc and std.meta.eql(identity, right.proc),
                .thunk => |identity| right == .thunk and std.meta.eql(identity, right.thunk),
                .helper => |name| right == .helper and std.mem.eql(u8, name, right.helper),
            };
        }
    };
    var placed = std.HashMap(Named, u32, NamedContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer placed.deinit();
    // Where each set's artifacts land, and which of them are copied.
    const maps = try allocator.alloc([]u32, sets.len);
    var mapped: usize = 0;
    defer {
        for (maps[0..mapped]) |map| allocator.free(map);
        allocator.free(maps);
    }
    const kept = try allocator.alloc([]bool, sets.len);
    var kept_count: usize = 0;
    defer {
        for (kept[0..kept_count]) |flags| allocator.free(flags);
        allocator.free(kept);
    }
    var next: u32 = 0;
    for (sets, maps, kept) |set, *map, *flags| {
        map.* = try allocator.alloc(u32, set.artifacts.len);
        mapped += 1;
        flags.* = try allocator.alloc(bool, set.artifacts.len);
        kept_count += 1;
        for (set.artifacts, map.*, flags.*) |artifact, *index, *keep| {
            const named: ?Named = switch (artifact.kind) {
                .proc => |identity| .{ .proc = identity },
                .boxy_thunk => |identity| .{ .thunk = identity },
                .rc_helper => |name| .{ .helper = name },
                .entrypoint, .message_pool_run, .branch_island => null,
            };
            if (named) |key| {
                const gop = try placed.getOrPut(key);
                if (gop.found_existing) {
                    index.* = gop.value_ptr.*;
                    keep.* = false;
                    continue;
                }
                gop.value_ptr.* = next;
            }
            index.* = next;
            keep.* = true;
            next += 1;
        }
    }
    var artifacts = try std.ArrayList(Artifact).initCapacity(a, next);
    for (sets, maps, kept) |set, map, flags| {
        // A constant's program-local name belongs to the program that made
        // the set, and another set's program can give the same name to a
        // different constant, so each set names its constants by content
        // before the sets meet.
        var names = try ContentNames.init(allocator, set);
        defer names.deinit();
        for (set.artifacts, flags) |source, keep| {
            if (!keep) continue;
            const first: u32 = @intCast(artifacts.items.len);
            try copyArtifacts(a, &artifacts, &.{ .arena = undefined, .artifacts = &.{source} }, null);
            const refs: []Reference = @constCast(artifacts.items[first].refs);
            for (refs, source.refs) |*ref, original| ref.target = map[original.target];
            try nameDataByContent(a, &artifacts.items[first], &names);
        }
    }
    // Both slices come from the arena before it is copied into the set.
    const owned_artifacts = try artifacts.toOwnedSlice(a);
    const owned_source_files = try a.dupe([32]u8, source_files);
    return .{ .arena = arena, .artifacts = owned_artifacts, .source_files = owned_source_files };
}

/// Replace every program-local constant name `artifact` carries or refers
/// to with its content name (`ContentNames`), as a pack names it.
fn nameDataByContent(a: Allocator, artifact: *Artifact, names: *const ContentNames) Allocator.Error!void {
    for (@constCast(artifact.relocations)) |*relocation| {
        if (!names.renames(relocation.name)) continue;
        relocation.name = try a.dupe(u8, names.of(relocation.name));
        relocation.scope = .shared;
    }
    for (@constCast(artifact.data)) |*item| {
        if (names.renames(item.name)) {
            item.name = try a.dupe(u8, names.of(item.name));
            item.program_local_name = false;
        }
        for (@constCast(item.relocations)) |*relocation| {
            if (relocation.external or !names.renames(relocation.name)) continue;
            relocation.name = try a.dupe(u8, names.of(relocation.name));
        }
    }
}

/// Deep-copy a set whose lines index a source-file table it does not carry
/// (`extractPrepared`), keeping its file indices.
fn copySet(allocator: Allocator, set: *const Set) Allocator.Error!Set {
    std.debug.assert(set.source_files.len == 0);
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    var artifacts = std.ArrayList(Artifact).empty;
    try copyArtifacts(a, &artifacts, set, null);
    const owned = try artifacts.toOwnedSlice(a);
    return .{ .arena = arena, .artifacts = owned };
}

/// Append deep copies of `set`'s artifacts, rebasing set-local references
/// and, given a `file_map`, carrying line file indices through it.
fn copyArtifacts(a: Allocator, artifacts: *std.ArrayList(Artifact), set: *const Set, file_map: ?[]const u32) Allocator.Error!void {
    const first: u32 = @intCast(artifacts.items.len);
    for (set.artifacts) |source| {
        var artifact = source;
        artifact.kind = switch (source.kind) {
            .rc_helper => |name| .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => source.kind,
        };
        artifact.code = try a.dupe(u8, source.code);
        const refs = try a.dupe(Reference, source.refs);
        for (refs) |*ref| ref.target += first;
        artifact.refs = refs;
        const symbolic_refs = try a.dupe(SymbolicReference, source.symbolic_refs);
        for (symbolic_refs) |*ref| switch (ref.target) {
            .rc_helper => |name| ref.target = .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk => {},
        };
        artifact.symbolic_refs = symbolic_refs;
        const lines = try a.dupe(LineEntry, source.lines);
        if (file_map) |map| for (lines) |*line| {
            line.loc = rebindLoc(line.loc, map);
        };
        artifact.lines = lines;
        if (file_map) |map| artifact.loc = rebindLoc(source.loc, map);
        artifact.failure_sites = try a.dupe(FailureSite, source.failure_sites);
        const relocations = try a.dupe(NamedRelocation, source.relocations);
        for (relocations) |*relocation| relocation.name = try a.dupe(u8, relocation.name);
        artifact.relocations = relocations;
        artifact.runtime_refs = try a.dupe(RuntimeReference, source.runtime_refs);
        const data = try a.alloc(DataItem, source.data.len);
        for (source.data, data) |item, *owned| owned.* = try cloneData(a, item);
        artifact.data = data;
        try artifacts.append(a, artifact);
    }
}

/// `loc` with its file index carried from one source-file domain to another
/// through `file_map`, indexed by the source domain's file index.
pub fn rebindLoc(loc: base.SourceLoc, file_map: []const u32) base.SourceLoc {
    if (!loc.hasLocation()) return loc;
    var rebound = loc;
    rebound.file = file_map[loc.file];
    return rebound;
}

/// Why a code buffer could not be lifted into artifacts.
pub const ExtractError = Allocator.Error || error{
    /// Two producer regions overlap. Embedded branch islands are not
    /// independent producers and are retained with their owning region.
    NestedCodeRegion,
    /// Bytes of the code buffer belong to no logged region.
    UncoveredCode,
    /// A reference resolves to an offset inside no region.
    DanglingReference,
    /// A relocation kind an artifact cannot carry.
    UnsupportedRelocation,
};

/// Content names for the internal constants an artifact set carries. A
/// constant is a node in a graph of readonly data: its bytes plus pointer
/// relocations to other constants, literal backings, procedures, or refcount
/// helpers. Two programs that hold the same constant must name it the same
/// way in their packs, and two programs that give one program-local name to
/// different constants must not, so every carried item with a
/// `program_local_name` is named by the digest of its own rendering: bytes,
/// alignment, symbol offset, and relocations, with program-local targets by
/// digest and every other target by its name. A constant can point back at
/// itself through a cycle; the walk writes a back-reference by relative
/// stack depth, as the procedure identity renderer does, and remembers every
/// node whose rendering refers to nothing above its own frame.
pub const ContentNames = struct {
    allocator: Allocator,
    arena: collections.SingleThreadArena,
    /// Every carried item, by the name its program gave it.
    items: std.StringHashMapUnmanaged(DataItem) = .empty,
    /// Content name of each program-local item.
    names: std.StringHashMapUnmanaged([]const u8) = .empty,
    digests: std.StringHashMapUnmanaged([32]u8) = .empty,
    active: std.StringHashMapUnmanaged(u32) = .empty,
    depth: u32 = 0,

    const domain = "roc.static.data.v2";
    const no_reference: u32 = std.math.maxInt(u32);

    pub fn init(allocator: Allocator, set: *const Set) Allocator.Error!ContentNames {
        var self = ContentNames{ .allocator = allocator, .arena = collections.SingleThreadArena.init(allocator) };
        errdefer self.deinit();
        for (set.artifacts) |artifact| {
            for (artifact.data) |item| try self.items.put(allocator, item.name, item);
        }
        for (set.artifacts) |artifact| {
            for (artifact.data) |item| try self.nameItem(item);
        }
        return self;
    }

    /// Content names for `items`, every one a carried constant together with
    /// every constant it names.
    pub fn initItems(allocator: Allocator, items: []const DataItem) Allocator.Error!ContentNames {
        var self = ContentNames{ .allocator = allocator, .arena = collections.SingleThreadArena.init(allocator) };
        errdefer self.deinit();
        for (items) |item| try self.items.put(allocator, item.name, item);
        for (items) |item| try self.nameItem(item);
        return self;
    }

    /// Content names for the constants an LLVM object pack's object defines
    /// as weak data (design.md "Object Cache Store"). Not every object format
    /// can place a weak symbol inside a datum, so each constant is the datum
    /// whose symbol marks its first byte: one whose symbol sits
    /// `symbol_offset` bytes in is named with that offset folded into every
    /// reference to it, and its code addresses it the same way.
    pub fn initObjectData(allocator: Allocator, exports: []const lir.Program.StaticDataExport) Allocator.Error!ContentNames {
        var self = ContentNames{ .allocator = allocator, .arena = collections.SingleThreadArena.init(allocator) };
        errdefer self.deinit();
        const a = self.arena.allocator();
        const items = try a.alloc(DataItem, exports.len);
        for (exports, items) |data_export, *item| {
            const relocations = try a.alloc(DataRelocation, data_export.relocations.len);
            for (data_export.relocations, relocations) |relocation, *out| {
                const name, const anchor: i64 = switch (relocation.target) {
                    .data_symbol => |target| .{ exports[@backingInt(target)].symbol_name, exports[@backingInt(target)].symbol_offset },
                    .named => .{ relocation.target_symbol_name, 0 },
                };
                out.* = .{
                    .offset = @intCast(relocation.offset),
                    .name = name,
                    .addend = anchor + relocation.addend,
                    .function = switch (relocation.kind) {
                        .address => false,
                        .function_pointer => true,
                    },
                };
            }
            item.* = .{
                .name = data_export.symbol_name,
                .bytes = data_export.bytes,
                .alignment = data_export.alignment,
                .symbol_offset = 0,
                .relocations = relocations,
                .program_local_name = !data_export.is_exported,
            };
        }
        for (items) |item| try self.items.put(allocator, item.name, item);
        for (items) |item| try self.nameItem(item);
        return self;
    }

    /// Give `item` its content name, when its program named it for itself.
    fn nameItem(self: *ContentNames, item: DataItem) Allocator.Error!void {
        if (!item.program_local_name or self.names.contains(item.name)) return;
        var hasher = Sha256.init(.{});
        _ = try self.write(&hasher, item);
        const digest = self.digests.get(item.name) orelse unreachable; // a top-level write has no enclosing active item, so it records the item's digest
        const content_name = try std.fmt.allocPrint(self.arena.allocator(), content_data_prefix ++ "{s}", .{&std.fmt.bytesToHex(digest[0..16].*, .lower)});
        try self.names.put(self.allocator, item.name, content_name);
    }

    pub fn deinit(self: *ContentNames) void {
        self.active.deinit(self.allocator);
        self.digests.deinit(self.allocator);
        self.names.deinit(self.allocator);
        self.items.deinit(self.allocator);
        self.arena.deinit();
    }

    /// The name a pack gives the symbol its program called `name`.
    pub fn of(self: *const ContentNames, name: []const u8) []const u8 {
        return self.names.get(name) orelse name;
    }

    /// Whether the pack names `name` by content instead.
    pub fn renames(self: *const ContentNames, name: []const u8) bool {
        return self.names.contains(name);
    }

    /// Writes the digest of `item`'s rendering and returns the shallowest
    /// stack depth that rendering referred back to.
    fn write(self: *ContentNames, hasher: *Sha256, item: DataItem) Allocator.Error!u32 {
        const rendered = try self.render(item);
        writeNaming(hasher, rendered.naming);
        return rendered.low;
    }

    /// How a rendering names a program-local constant: by the stack depth it
    /// is active at, or by its digest.
    const Naming = union(enum) {
        cycle: u32,
        data: [32]u8,
    };

    const Rendered = struct {
        naming: Naming,
        /// The shallowest stack depth the rendering referred back to.
        low: u32,
    };

    fn writeNaming(hasher: *Sha256, naming: Naming) void {
        switch (naming) {
            .cycle => |distance| {
                hasher.update("cycle");
                writeWord(hasher, distance);
            },
            .data => |digest| {
                hasher.update("data");
                hasher.update(&digest);
            },
        }
    }

    /// How `item` is named where a rendering refers to it.
    fn render(self: *ContentNames, item: DataItem) Allocator.Error!Rendered {
        if (self.active.get(item.name)) |depth| return .{ .naming = .{ .cycle = self.depth - depth }, .low = depth };
        if (self.digests.get(item.name)) |digest| return .{ .naming = .{ .data = digest }, .low = no_reference };
        const depth = self.depth;
        try self.active.putNoClobber(self.allocator, item.name, depth);
        self.depth += 1;
        defer {
            _ = self.active.remove(item.name);
            self.depth -= 1;
        }

        var sub = Sha256.init(.{});
        sub.update(domain);
        writeWord(&sub, @intCast(item.bytes.len));
        sub.update(item.bytes);
        writeWord(&sub, item.alignment);
        writeWord(&sub, item.symbol_offset);
        var low: u32 = no_reference;
        // Relocations are rendered in offset order; ones already in that
        // order are read in place.
        const in_order = std.sort.isSorted(DataRelocation, item.relocations, {}, relocationOffsetBefore);
        const relocations = if (in_order) item.relocations else blk: {
            const sorted = try self.allocator.dupe(DataRelocation, item.relocations);
            std.mem.sort(DataRelocation, sorted, {}, relocationOffsetBefore);
            break :blk sorted;
        };
        defer if (!in_order) self.allocator.free(relocations);
        writeWord(&sub, @intCast(relocations.len));
        // A relocation naming the same symbol, the same way, as the one before
        // it renders the same reference: rendering a target leaves the active
        // stack as it found it, and one already rendered renders again as it
        // did. A list of copies of one value names it once per item.
        var previous: ?struct { name: []const u8, code: bool, target: ?Rendered } = null;
        for (relocations) |relocation| {
            writeWord(&sub, relocation.offset);
            writeWide(&sub, @bitCast(relocation.addend));
            sub.update(&.{ @intFromBool(relocation.function), @intFromBool(relocation.external) });
            const code = relocation.function or relocation.external;
            const repeated = if (previous) |last| last.code == code and std.mem.eql(u8, last.name, relocation.name) else false;
            if (!repeated) {
                const target = if (code) null else self.items.get(relocation.name);
                previous = .{
                    .name = relocation.name,
                    .code = code,
                    .target = if (target != null and target.?.program_local_name) try self.render(target.?) else null,
                };
            }
            if (previous.?.target) |rendered| {
                writeNaming(&sub, rendered.naming);
                low = @min(low, rendered.low);
            } else {
                sub.update("named");
                writeWord(&sub, @intCast(relocation.name.len));
                sub.update(relocation.name);
            }
        }
        const digest = sub.finalResult();
        if (low >= depth) {
            try self.digests.put(self.allocator, item.name, digest);
            return .{ .naming = .{ .data = digest }, .low = no_reference };
        }
        return .{ .naming = .{ .data = digest }, .low = low };
    }

    fn relocationOffsetBefore(_: void, a: DataRelocation, b: DataRelocation) bool {
        return a.offset < b.offset;
    }

    fn writeWord(hasher: *Sha256, value: u32) void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        hasher.update(&buffer);
    }

    fn writeWide(hasher: *Sha256, value: u64) void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        hasher.update(&buffer);
    }
};

/// Immutable data catalog prepared once by the coordinator. Worker lookups
/// borrow it read-only; retained artifacts copy only their reachable closure.
pub const PreparedData = struct {
    arena: collections.SingleThreadArena,
    items: std.StringHashMapUnmanaged(DataItem),

    pub fn init(
        allocator: Allocator,
        string_exports: []const lir.Program.StaticDataExport,
        constant_exports: []const lir.Program.StaticDataExport,
        spliced_data: []const DataItem,
    ) Allocator.Error!PreparedData {
        var arena = collections.SingleThreadArena.init(allocator);
        errdefer arena.deinit();
        const a = arena.allocator();
        var items = std.StringHashMapUnmanaged(DataItem){};
        for (string_exports) |string_export| {
            const item = try cloneData(a, .{
                .name = string_export.symbol_name,
                .bytes = string_export.bytes,
                .alignment = string_export.alignment,
                .symbol_offset = string_export.symbol_offset,
            });
            try items.put(a, item.name, item);
        }
        for (constant_exports) |constant| {
            const relocations = try a.alloc(DataRelocation, constant.relocations.len);
            var copier = NameCopier{};
            for (constant.relocations, relocations) |relocation, *out| {
                const name = switch (relocation.target) {
                    .data_symbol => |target| constant_exports[@backingInt(target)].symbol_name,
                    .named => relocation.target_symbol_name,
                };
                out.* = .{
                    .offset = @intCast(relocation.offset),
                    .name = try copier.copy(a, name),
                    .addend = relocation.addend,
                    .function = switch (relocation.kind) {
                        .address => false,
                        .function_pointer => true,
                    },
                };
            }
            const item = DataItem{
                .name = try a.dupe(u8, constant.symbol_name),
                .bytes = try a.dupe(u8, constant.bytes),
                .alignment = constant.alignment,
                .symbol_offset = constant.symbol_offset,
                .relocations = relocations,
                .program_local_name = !constant.is_exported,
            };
            try items.put(a, item.name, item);
        }
        for (spliced_data) |item| {
            const owned = try cloneData(a, item);
            try items.put(a, owned.name, owned);
        }
        return .{ .arena = arena, .items = items };
    }

    pub fn deinit(self: *PreparedData) void {
        self.arena.deinit();
    }
};

/// Lift every region of a finished code generator's buffer into an artifact set.
pub fn extract(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
    spliced_data: []const DataItem,
) ExtractError!Set {
    var prepared = try PreparedData.init(allocator, string_exports, constant_exports, spliced_data);
    defer prepared.deinit();
    return try extractCataloged(CG, allocator, codegen, proc_specs, layout_store, &prepared);
}

/// `extract` against a data catalog the caller prepared and keeps.
pub fn extractCataloged(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Set {
    var set = try extractPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
    errdefer set.deinit();
    const source_files = try set.arena.allocator().alloc([32]u8, codegen.store.sourceFileCount());
    for (source_files, 0..) |*identity, file| identity.* = codegen.store.sourceFileModuleIdentity(@intCast(file));
    set.source_files = source_files;
    return set;
}

/// Capture against a shared immutable catalog without scanning module exports.
/// The set's lines index the code generator's own source-file table, which
/// the set does not copy: a session's fragments all share that table.
pub fn extractPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Set {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();

    const code = codegen.getGeneratedCode();
    const all_regions = try allocator.dupe(CG.CodeRegion, codegen.codeRegions());
    defer allocator.free(all_regions);
    std.mem.sort(CG.CodeRegion, all_regions, {}, regionStartsBefore(CG.CodeRegion));
    // An island inside a procedure is already carried by that procedure's
    // bytes. Top-level islands remain placement-owned, as before.
    var region_count: usize = 0;
    var containing_end: usize = 0;
    for (all_regions) |region| {
        if (region.kind == .branch_island and region.start < containing_end and region.end <= containing_end) continue;
        all_regions[region_count] = region;
        region_count += 1;
        containing_end = region.end;
    }
    const regions = all_regions[0..region_count];

    var covered: usize = 0;
    for (regions) |region| {
        if (region.start < covered) return error.NestedCodeRegion;
        if (region.start != covered) return error.UncoveredCode;
        covered = region.end;
    }
    if (covered != code.len) return error.UncoveredCode;

    const unwind = codegen.getUnwindFunctions();
    const relocations = try codegen.artifactRelocations(allocator);
    defer allocator.free(relocations);
    // Each symbol's name, copied into the set once however many relocations
    // name it, by symbol id.
    const owned_names = try allocator.alloc(?[]const u8, codegen.getSymbolNames().len);
    defer allocator.free(owned_names);
    @memset(owned_names, null);
    const refs = codegen.codeRefs();
    for (relocations) |relocation| switch (relocation) {
        .local_data => return error.UnsupportedRelocation,
        .retired, .linked_function, .linked_data, .runtime_ref => {},
    };

    // Standalone branch islands belong to this placement, not the artifact
    // set. Embedded reservations stay inside their owning bytes but every
    // reference still records its logical target for final placement.
    const artifact_of_region = try allocator.alloc(?u32, regions.len);
    defer allocator.free(artifact_of_region);
    var artifact_count: u32 = 0;
    for (regions, 0..) |region, index| {
        artifact_of_region[index] = if (regionHasArtifact(region.kind)) artifact_count else null;
        if (regionHasArtifact(region.kind)) artifact_count += 1;
    }

    // Each reference and relocation belongs to the one region its offset
    // falls in. Grouping them once keeps extraction linear in the program's
    // size; within a region they keep their emission order.
    var ref_groups = try RegionGroups.init(CG, allocator, regions, refs.len, refs, refSite);
    defer ref_groups.deinit(allocator);
    var relocation_groups = try RegionGroups.init(CG, allocator, regions, relocations.len, relocations, relocationOffset);
    defer relocation_groups.deinit(allocator);
    var frame_of_entry = std.AutoHashMap(usize, usize).init(allocator);
    defer frame_of_entry.deinit();
    for (unwind, 0..) |function, index| {
        const entry = try frame_of_entry.getOrPut(function.start_offset);
        if (!entry.found_existing) entry.value_ptr.* = index;
    }
    var message_runs = try MessageRuns.init(CG, allocator, regions);
    defer message_runs.deinit(allocator);

    const artifacts = try arena_allocator.alloc(Artifact, artifact_count);
    for (regions, 0..) |region, region_index| {
        const index = artifact_of_region[region_index] orelse continue;
        const kind: Kind = switch (region.kind) {
            .proc => |proc_id| .{ .proc = proc_specs[@backingInt(proc_id)].identity },
            .rc_helper => |key| .{ .rc_helper = try LirCodeGenMod.compiledRcHelperSymbolName(arena_allocator, layout_store, key) },
            .boxy_thunk => |proc_id| .{ .boxy_thunk = proc_specs[@backingInt(proc_id)].identity },
            .entrypoint => .entrypoint,
            .message_pool_run => .message_pool_run,
            .branch_island, .hosted_stub => unreachable, // regionHasArtifact is false for these, so artifact_of_region skipped them above
            .spliced_proc => |identity| .{ .proc = identity },
            .spliced_boxy_thunk => |identity| .{ .boxy_thunk = identity },
            .spliced_helper => .{ .rc_helper = try arena_allocator.dupe(u8, codegen.splicedHelperName(region.start + region.entry) orelse return error.DanglingReference) },
        };

        const frame: ?Frame = if (frame_of_entry.get(region.start + region.entry)) |function_index| frame: {
            const function = unwind[function_index];
            break :frame .{
                .prologue_size = function.prologue_size,
                .stack_alloc = function.stack_alloc,
                .frame_size = function.frame_size,
                .callee_saved_mask = function.callee_saved_mask,
                .epilogue_offset = function.epilogue_offset,
                .uses_frame_pointer = function.uses_frame_pointer,
            };
        } else null;

        // Code whose identity names it reaches a procedure whose identity
        // names its template by site (`LIR.LirProcSpec.code_key`) by that
        // identity, never by a copy of its code: a program the artifact is
        // spliced into calls its own code of the procedure.
        const region_names_code = switch (region.kind) {
            .proc => |proc_id| proc_specs[@backingInt(proc_id)].code_key == null,
            .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island, .hosted_stub, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => true,
        };
        var region_refs = std.ArrayList(Reference).empty;
        var symbolic_refs = std.ArrayList(SymbolicReference).empty;
        for (ref_groups.of(region_index)) |ref_index| {
            const ref = refs[ref_index];
            const symbolic_target: ?@FieldType(SymbolicReference, "target") = switch (ref.target) {
                .proc => |proc_id| if (codegen.compiledProcSymbol(proc_id) == null or
                    (region_names_code and proc_specs[@backingInt(proc_id)].code_key != null))
                    .{ .proc = proc_specs[@backingInt(proc_id)].identity }
                else
                    null,
                .rc_helper => |key| if (codegen.compiledRcHelperOffset(key) == null) .{ .rc_helper = try LirCodeGenMod.compiledRcHelperSymbolName(arena_allocator, layout_store, key) } else null,
                .boxy_thunk => |proc_id| if (codegen.boxyThunkOffset(proc_id) == null) .{ .boxy_thunk = proc_specs[@backingInt(proc_id)].identity } else null,
                .message, .offset => null,
            };
            if (symbolic_target) |target| {
                try symbolic_refs.append(arena_allocator, .{
                    .site = @intCast(ref.site - region.start),
                    .form = switch (ref.form) {
                        .call => .call,
                        .inline_call => .inline_call,
                        .addr => .addr,
                    },
                    .veneer = if (codegen.codeRefVeneer(ref.site)) |offset| if (offset >= region.start and offset < region.end) @intCast(offset - region.start) else null else null,
                    .target = target,
                });
                continue;
            }
            const target_offset: usize = switch (ref.target) {
                .proc => |proc_id| (codegen.compiledProcSymbol(proc_id) orelse return error.DanglingReference).code_start,
                .rc_helper => |key| codegen.compiledRcHelperOffset(key) orelse return error.DanglingReference,
                .message => |message_offset| message_runs.codeOffset(regions, message_offset) orelse return error.DanglingReference,
                .boxy_thunk => |proc_id| codegen.boxyThunkOffset(proc_id) orelse return error.DanglingReference,
                .offset => |offset| offset,
            };
            const target_region = regionContaining(CG, regions, target_offset) orelse return error.DanglingReference;
            const target_index = artifact_of_region[target_region] orelse return error.DanglingReference;
            if (region_names_code) site: {
                const target_proc = switch (regions[target_region].kind) {
                    .proc => |proc_id| proc_id,
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island, .hosted_stub, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => break :site,
                };
                if (proc_specs[@backingInt(target_proc)].code_key == null) break :site;
                if (target_offset != regions[target_region].start + regions[target_region].entry) return error.DanglingReference;
                try symbolic_refs.append(arena_allocator, .{
                    .site = @intCast(ref.site - region.start),
                    .form = switch (ref.form) {
                        .call => .call,
                        .inline_call => .inline_call,
                        .addr => .addr,
                    },
                    .veneer = if (codegen.codeRefVeneer(ref.site)) |offset| if (offset >= region.start and offset < region.end) @intCast(offset - region.start) else null else null,
                    .target = .{ .proc = proc_specs[@backingInt(target_proc)].identity },
                });
                continue;
            }
            try region_refs.append(arena_allocator, .{
                .site = @intCast(ref.site - region.start),
                .form = switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                .target = target_index,
                .delta = @intCast(target_offset - regions[target_region].start),
                .veneer = if (codegen.codeRefVeneer(ref.site)) |offset| if (offset >= region.start and offset < region.end) @intCast(offset - region.start) else null else null,
            });
        }

        var region_relocations = std.ArrayList(NamedRelocation).empty;
        var region_runtime_refs = std.ArrayList(RuntimeReference).empty;
        for (relocation_groups.of(region_index)) |relocation_index| {
            const relocation = relocations[relocation_index];
            const offset = relocation.getOffset();
            if (relocation == .runtime_ref) {
                try region_runtime_refs.append(arena_allocator, .{ .site = @intCast(offset - region.start), .ref = relocation.runtime_ref.ref });
                continue;
            }
            try region_relocations.append(arena_allocator, switch (relocation) {
                .linked_function => |function| .{
                    .offset = @intCast(offset - region.start),
                    .name = try ownedSymbolName(CG, arena_allocator, codegen, owned_names, function.symbol),
                    .scope = codegen.symbolScope(function.symbol),
                    .kind = .{ .function = function.kind },
                },
                .linked_data => |data| .{
                    .offset = @intCast(offset - region.start),
                    .name = try ownedSymbolName(CG, arena_allocator, codegen, owned_names, data.symbol),
                    .scope = codegen.symbolScope(data.symbol),
                    .kind = .{ .data = data.kind },
                },
                .retired, .local_data, .runtime_ref => unreachable, // the switch above skipped or rejected these kinds, and the loop took runtime references
            });
        }

        // Every data item the region names, then every item those name, so
        // an artifact carries the whole constant graph it points into.
        const region_data = try captureData(allocator, arena_allocator, region_relocations.items, prepared);

        // Lines and failure sites are in code offset order, so each region's
        // are one contiguous run found by search. A failing call's return
        // address may be the region's end.
        const all_lines = codegen.getLineEntries();
        var lines = std.ArrayList(LineEntry).empty;
        for (all_lines[firstAtOrAfter(LineEntry, all_lines, region.start)..]) |line| {
            if (line.offset >= region.end) break;
            try lines.append(arena_allocator, .{ .offset = @intCast(line.offset - region.start), .loc = line.loc, .region = line.region });
        }
        const all_failure_sites = codegen.getFailureSites();
        var failure_sites = std.ArrayList(FailureSite).empty;
        for (all_failure_sites[firstAtOrAfter(FailureSite, all_failure_sites, region.start + 1)..]) |site| {
            if (site.offset > region.end) break;
            try failure_sites.append(arena_allocator, .{ .offset = @intCast(site.offset - region.start), .meaning = site.meaning });
        }
        const artifact_code = try arena_allocator.dupe(u8, code[region.start..region.end]);
        codegen.normalizeArtifactCode(region.start, artifact_code);
        for (region_refs.items) |ref| CG.normalizeArtifactRefSite(artifact_code, ref.site, codeRefForm(ref.form), if (ref.veneer) |offset| offset else null);
        for (symbolic_refs.items) |ref| CG.normalizeArtifactRefSite(artifact_code, ref.site, codeRefForm(ref.form), if (ref.veneer) |offset| offset else null);
        artifacts[index] = .{
            .kind = kind,
            .code = artifact_code,
            .entry = @intCast(region.entry),
            .frame = frame,
            .refs = try region_refs.toOwnedSlice(arena_allocator),
            .symbolic_refs = try symbolic_refs.toOwnedSlice(arena_allocator),
            .lines = try lines.toOwnedSlice(arena_allocator),
            .failure_sites = try failure_sites.toOwnedSlice(arena_allocator),
            .relocations = try region_relocations.toOwnedSlice(arena_allocator),
            .runtime_refs = try region_runtime_refs.toOwnedSlice(arena_allocator),
            .data = region_data,
            .loc = switch (region.kind) {
                // A procedure placed from another set carries the location
                // that set recorded; any other has its own.
                .proc => |proc_id| codegen.assembledProcLoc(proc_specs[@backingInt(proc_id)].identity) orelse codegen.store.procLoc(proc_id),
                .spliced_proc => |identity| codegen.assembledProcLoc(identity) orelse .none,
                .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island, .hosted_stub, .spliced_boxy_thunk, .spliced_helper => .none,
            },
        };
    }

    return .{ .arena = arena, .artifacts = artifacts };
}

fn refSite(ref: anytype) ?usize {
    return ref.site;
}

fn relocationOffset(relocation: anytype) ?usize {
    return switch (relocation) {
        .retired => null,
        .linked_function, .linked_data, .runtime_ref => @intCast(relocation.getOffset()),
        .local_data => unreachable, // extraction rejected these kinds before grouping relocations
    };
}

/// The indices of a list's items grouped by the code region their offsets
/// fall in, each group in list order.
const RegionGroups = struct {
    /// `items[starts[r]..starts[r + 1]]` are region `r`'s item indices.
    starts: []u32,
    items: []u32,

    fn init(
        comptime CG: type,
        allocator: Allocator,
        regions: []const CG.CodeRegion,
        count: usize,
        list: anytype,
        comptime offsetOf: anytype,
    ) Allocator.Error!RegionGroups {
        const region_of = try allocator.alloc(?u32, count);
        defer allocator.free(region_of);
        const starts = try allocator.alloc(u32, regions.len + 1);
        errdefer allocator.free(starts);
        @memset(starts, 0);
        var grouped: usize = 0;
        for (list, region_of) |item, *region| {
            region.* = if (offsetOf(item)) |offset|
                if (regionContaining(CG, regions, offset)) |found| @intCast(found) else null
            else
                null;
            if (region.*) |found| {
                starts[found + 1] += 1;
                grouped += 1;
            }
        }
        for (1..starts.len) |index| starts[index] += starts[index - 1];
        const items = try allocator.alloc(u32, grouped);
        errdefer allocator.free(items);
        const next = try allocator.dupe(u32, starts[0..regions.len]);
        defer allocator.free(next);
        for (region_of, 0..) |maybe_region, item_index| {
            const region = maybe_region orelse continue;
            items[next[region]] = @intCast(item_index);
            next[region] += 1;
        }
        return .{ .starts = starts, .items = items };
    }

    fn of(self: *const RegionGroups, region: usize) []const u32 {
        return self.items[self.starts[region]..self.starts[region + 1]];
    }

    fn deinit(self: *RegionGroups, allocator: Allocator) void {
        allocator.free(self.starts);
        allocator.free(self.items);
    }
};

/// The message pool runs of a placement, for finding the run a message
/// byte was placed in: the last run in placement order that starts at or
/// before the message.
const MessageRuns = struct {
    /// Pool offset each run starts at, ascending.
    pool_from: []u32,
    /// For each entry of `pool_from`, the latest placement position among
    /// the runs starting at or before it.
    latest_region: []u32,

    const Run = struct { pool_from: u32, region: u32 };

    fn init(comptime CG: type, allocator: Allocator, regions: []const CG.CodeRegion) Allocator.Error!MessageRuns {
        var runs = std.ArrayList(Run).empty;
        defer runs.deinit(allocator);
        for (regions, 0..) |region, index| switch (region.kind) {
            .message_pool_run => |pool_from| try runs.append(allocator, .{ .pool_from = pool_from, .region = @intCast(index) }),
            .proc, .rc_helper, .boxy_thunk, .entrypoint, .branch_island, .hosted_stub, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => {},
        };
        std.mem.sort(Run, runs.items, {}, struct {
            fn lessThan(_: void, lhs: Run, rhs: Run) bool {
                return lhs.pool_from < rhs.pool_from;
            }
        }.lessThan);
        const pool_from = try allocator.alloc(u32, runs.items.len);
        errdefer allocator.free(pool_from);
        const latest_region = try allocator.alloc(u32, runs.items.len);
        var latest: u32 = 0;
        for (runs.items, pool_from, latest_region) |run, *from, *region| {
            latest = @max(latest, run.region);
            from.* = run.pool_from;
            region.* = latest;
        }
        return .{ .pool_from = pool_from, .latest_region = latest_region };
    }

    fn codeOffset(self: *const MessageRuns, regions: anytype, message_offset: u32) ?usize {
        // The number of runs starting at or before the message.
        var low: usize = 0;
        var high: usize = self.pool_from.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.pool_from[mid] <= message_offset) low = mid + 1 else high = mid;
        }
        if (low == 0) return null;
        const region = regions[self.latest_region[low - 1]];
        return region.start + (message_offset - region.kind.message_pool_run);
    }

    fn deinit(self: *MessageRuns, allocator: Allocator) void {
        allocator.free(self.pool_from);
        allocator.free(self.latest_region);
    }
};

/// Index of the first entry, in offset order, at or after `offset`.
fn firstAtOrAfter(comptime T: type, items: []const T, offset: usize) usize {
    var low: usize = 0;
    var high: usize = items.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (items[mid].offset < offset) low = mid + 1 else high = mid;
    }
    return low;
}

/// `symbol`'s name, copied into the set's arena the first time it is asked for.
fn ownedSymbolName(comptime CG: type, arena_allocator: Allocator, codegen: *const CG, owned_names: []?[]const u8, symbol: anytype) Allocator.Error![]const u8 {
    const slot = &owned_names[@backingInt(symbol)];
    if (slot.*) |name| return name;
    const name = try arena_allocator.dupe(u8, codegen.symbolName(symbol));
    slot.* = name;
    return name;
}

fn captureData(
    allocator: Allocator,
    arena_allocator: Allocator,
    relocations: []const NamedRelocation,
    prepared: *const PreparedData,
) ExtractError![]const DataItem {
    var data = std.ArrayList(DataItem).empty;
    var pending = std.ArrayList([]const u8).empty;
    defer pending.deinit(allocator);
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();
    for (relocations) |relocation| {
        const item = prepared.items.get(relocation.name) orelse continue;
        try pending.append(allocator, item.name);
    }
    while (pending.pop()) |name| {
        const visited = try seen.getOrPut(name);
        if (visited.found_existing) continue;
        const item = prepared.items.get(name) orelse return error.DanglingReference;
        try data.append(arena_allocator, try cloneData(arena_allocator, item));
        try appendDataTargets(allocator, &pending, item.relocations);
    }
    return try data.toOwnedSlice(arena_allocator);
}

/// Append to `out` the constants of `catalog` named `roots`, and every
/// constant they name, transitively, each once. The items stay borrowed from
/// the catalog.
pub fn dataClosure(
    allocator: Allocator,
    catalog: *const PreparedData,
    roots: []const []const u8,
    out: *std.ArrayList(DataItem),
) (Allocator.Error || error{DanglingReference})!void {
    var pending = std.ArrayList([]const u8).empty;
    defer pending.deinit(allocator);
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();
    var index = roots.len;
    while (index > 0) {
        index -= 1;
        try pending.append(allocator, roots[index]);
    }
    while (pending.pop()) |name| {
        if ((try seen.getOrPut(name)).found_existing) continue;
        const item = catalog.items.get(name) orelse return error.DanglingReference;
        try out.append(allocator, item);
        try appendDataTargets(allocator, &pending, item.relocations);
    }
}

/// Append to `pending` every symbol `relocations` name, code included, each
/// once per run of relocations that name it in a row.
fn appendRelocationTargets(allocator: Allocator, pending: *std.ArrayList([]const u8), relocations: []const DataRelocation) Allocator.Error!void {
    var previous: ?[]const u8 = null;
    for (relocations) |relocation| {
        if (previous) |name| if (std.mem.eql(u8, name, relocation.name)) continue;
        previous = relocation.name;
        try pending.append(allocator, relocation.name);
    }
}

/// Append to `pending` the constants `relocations` name, skipping code
/// targets. A target the relocation before it also names is the same
/// constant, already appended: a list of copies of one value names it once
/// per item.
fn appendDataTargets(allocator: Allocator, pending: *std.ArrayList([]const u8), relocations: []const DataRelocation) Allocator.Error!void {
    var previous: ?[]const u8 = null;
    for (relocations) |relocation| {
        if (relocation.function or relocation.external) continue;
        if (previous) |name| if (std.mem.eql(u8, name, relocation.name)) continue;
        previous = relocation.name;
        try pending.append(allocator, relocation.name);
    }
}

/// Whether a region lifts into an artifact. Branch islands belong to one
/// placement of the code, and a hosted stub exists only in the evaluator's
/// image.
fn regionHasArtifact(kind: anytype) bool {
    return switch (kind) {
        .branch_island, .hosted_stub => false,
        .proc, .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => true,
    };
}

fn regionStartsBefore(comptime Region: type) fn (void, Region, Region) bool {
    return struct {
        fn lessThan(_: void, lhs: Region, rhs: Region) bool {
            return lhs.start < rhs.start;
        }
    }.lessThan;
}

fn regionContaining(comptime CG: type, regions: []const CG.CodeRegion, offset: usize) ?usize {
    var low: usize = 0;
    var high: usize = regions.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        const region = regions[mid];
        if (offset < region.start) {
            high = mid;
        } else if (offset >= region.end) {
            low = mid + 1;
        } else {
            return mid;
        }
    }
    return null;
}

/// What a linked program's code is reached from: the procedures the host
/// calls through entry wrappers, and the constants the host binds by name.
pub const ImageRoots = struct {
    procs: []const lir.ProcIdentity,
    data: []const []const u8,
    /// Further names code and constants relocate to that name a helper
    /// artifact, by the helper's artifact name.
    helper_aliases: *const std.StringHashMap([]const u8),
    /// Every Boxy thunk is reached, from the program's descriptor sidecar.
    all_thunks: bool,
};

/// The artifacts of `set` that the image's roots reach: through
/// references, through relocations to a procedure's or helper's symbol, and
/// through the constants code names, whose relocations reach further code.
/// Code a program declares but nothing reaches (a lambda-set member lowered
/// only so a spliced entry could call it, which the entry never does) is
/// left out, so the image holds the same code however its procedures were
/// provided. The result keeps set order.
pub fn retainReachable(allocator: Allocator, set: *const Set, catalog: *const PreparedData, roots: ImageRoots) Allocator.Error!Set {
    var by_name = std.StringHashMap(u32).init(allocator);
    defer by_name.deinit();
    var names_arena = collections.SingleThreadArena.init(allocator);
    defer names_arena.deinit();
    const names_allocator = names_arena.allocator();
    var by_proc = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator);
    defer by_proc.deinit();
    var by_thunk = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator);
    defer by_thunk.deinit();
    for (set.artifacts, 0..) |artifact, index| switch (artifact.kind) {
        .proc => |identity| {
            try by_proc.put(identity, @intCast(index));
            try by_name.put(try identity.symbolName(names_allocator), @intCast(index));
        },
        .boxy_thunk => |identity| try by_thunk.put(identity, @intCast(index)),
        .rc_helper => |name| try by_name.put(name, @intCast(index)),
        .entrypoint, .message_pool_run, .branch_island => {},
    };
    var aliases = roots.helper_aliases.iterator();
    while (aliases.next()) |alias| {
        if (by_name.get(alias.value_ptr.*)) |index| try by_name.put(alias.key_ptr.*, index);
    }

    const reached = try allocator.alloc(bool, set.artifacts.len);
    defer allocator.free(reached);
    @memset(reached, false);
    var artifact_work = std.ArrayList(u32).empty;
    defer artifact_work.deinit(allocator);
    var data_work = std.ArrayList([]const u8).empty;
    defer data_work.deinit(allocator);
    var data_seen = std.StringHashMap(void).init(allocator);
    defer data_seen.deinit();

    for (roots.procs) |identity| if (by_proc.get(identity)) |index| try artifact_work.append(allocator, index);
    for (roots.data) |name| try data_work.append(allocator, name);
    if (roots.all_thunks) {
        var thunks = by_thunk.valueIterator();
        while (thunks.next()) |index| try artifact_work.append(allocator, index.*);
    }
    while (artifact_work.items.len != 0 or data_work.items.len != 0) {
        while (artifact_work.pop()) |index| {
            if (reached[index]) continue;
            reached[index] = true;
            const artifact = set.artifacts[index];
            for (artifact.refs) |ref| try artifact_work.append(allocator, ref.target);
            for (artifact.symbolic_refs) |ref| {
                const target = switch (ref.target) {
                    .proc => |identity| by_proc.get(identity),
                    .boxy_thunk => |identity| by_thunk.get(identity),
                    .rc_helper => |name| by_name.get(name),
                };
                if (target) |target_index| try artifact_work.append(allocator, target_index);
            }
            for (artifact.relocations) |relocation| try data_work.append(allocator, relocation.name);
            for (artifact.data) |item| try data_work.append(allocator, item.name);
        }
        while (data_work.pop()) |name| {
            if (by_name.get(name)) |index| {
                try artifact_work.append(allocator, index);
                continue;
            }
            if ((try data_seen.getOrPut(name)).found_existing) continue;
            const item = catalog.items.get(name) orelse continue;
            try appendRelocationTargets(allocator, &data_work, item.relocations);
        }
    }

    const new_index = try allocator.alloc(u32, set.artifacts.len);
    defer allocator.free(new_index);
    var kept: u32 = 0;
    for (reached, new_index) |is_reached, *index| {
        index.* = kept;
        if (is_reached) kept += 1;
    }
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    const artifacts = try a.alloc(Artifact, kept);
    var written: usize = 0;
    for (set.artifacts, reached) |source, is_reached| {
        if (!is_reached) continue;
        var artifact = source;
        artifact.kind = switch (source.kind) {
            .rc_helper => |name| .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => source.kind,
        };
        artifact.code = try a.dupe(u8, source.code);
        const refs = try a.dupe(Reference, source.refs);
        for (refs) |*ref| ref.target = new_index[ref.target];
        artifact.refs = refs;
        const symbolic_refs = try a.dupe(SymbolicReference, source.symbolic_refs);
        for (symbolic_refs) |*ref| switch (ref.target) {
            .rc_helper => |name| ref.target = .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk => {},
        };
        artifact.symbolic_refs = symbolic_refs;
        artifact.lines = try a.dupe(LineEntry, source.lines);
        artifact.failure_sites = try a.dupe(FailureSite, source.failure_sites);
        const relocations = try a.dupe(NamedRelocation, source.relocations);
        for (relocations) |*relocation| relocation.name = try a.dupe(u8, relocation.name);
        artifact.relocations = relocations;
        artifact.runtime_refs = try a.dupe(RuntimeReference, source.runtime_refs);
        const data = try a.alloc(DataItem, source.data.len);
        for (source.data, data) |item, *owned| owned.* = try cloneData(a, item);
        artifact.data = data;
        artifacts[written] = artifact;
        written += 1;
    }
    // Allocated before the arena is copied into the result, which must own
    // every node.
    const source_files = try a.dupe([32]u8, set.source_files);
    return .{ .arena = arena, .artifacts = artifacts, .source_files = source_files };
}

/// A program's machine code in its one layout: every artifact of the code
/// its procedures, helpers, and thunks are, compiled for this program or
/// spliced from an object-cache entry, in an order and under names that
/// depend only on what each artifact is (design.md "Canonical Dev Images").
/// `data` holds every constant the code names, and every constant a
/// host-visible constant names, under its content name, sorted by name.
pub const CanonicalImage = struct {
    set: Set,
    data: []const CanonicalDatum,

    pub fn deinit(self: *CanonicalImage) void {
        self.set.deinit();
    }
};

/// One constant of a canonical image, and whether the host sees it under its
/// own name.
pub const CanonicalDatum = struct {
    item: DataItem,
    host_visible: bool,
};

/// Lay `set` out canonically (`CanonicalImage`). `host_visible` are the
/// constants the host binds by name, with every constant they name; their
/// names are kept, and every other constant is renamed by content
/// (`ContentNames`), so a constant this program named for itself and the
/// same constant carried by a spliced entry are one symbol.
///
/// Artifacts are ordered by what they are: procedures by identity, then Boxy
/// thunks by identity, then refcount helpers by name. A message pool run
/// belongs to the one artifact whose code addresses it and follows that
/// artifact, in the order the artifact first addresses its runs.
pub fn canonicalize(allocator: Allocator, set: *const Set, host_visible: []const DataItem) Allocator.Error!CanonicalImage {
    var all_items = std.ArrayList(DataItem).empty;
    defer all_items.deinit(allocator);
    var collected = std.StringHashMap(void).init(allocator);
    defer collected.deinit();
    var visible = std.StringHashMap(void).init(allocator);
    defer visible.deinit();
    for (host_visible) |item| {
        // A constant the program named for itself is named by content, so
        // no host can name it.
        if (!item.program_local_name) try visible.put(item.name, {});
        if ((try collected.getOrPut(item.name)).found_existing) continue;
        try all_items.append(allocator, item);
    }
    for (set.artifacts) |artifact| for (artifact.data) |item| {
        if ((try collected.getOrPut(item.name)).found_existing) continue;
        try all_items.append(allocator, item);
    };
    var names = try ContentNames.initItems(allocator, all_items.items);
    defer names.deinit();

    // The owner of each message pool run: the first artifact whose code
    // addresses it.
    const owner_of = try allocator.alloc(?u32, set.artifacts.len);
    defer allocator.free(owner_of);
    @memset(owner_of, null);
    for (set.artifacts, 0..) |artifact, index| for (artifact.refs) |ref| {
        if (set.artifacts[ref.target].kind != .message_pool_run) continue;
        if (owner_of[ref.target] == null) owner_of[ref.target] = @intCast(index);
    };
    var heads = std.ArrayList(u32).empty;
    defer heads.deinit(allocator);
    var unowned_runs = std.ArrayList(u32).empty;
    defer unowned_runs.deinit(allocator);
    for (set.artifacts, 0..) |artifact, index| switch (artifact.kind) {
        .message_pool_run => if (owner_of[index] == null) try unowned_runs.append(allocator, @intCast(index)),
        .proc, .boxy_thunk, .rc_helper, .entrypoint, .branch_island => try heads.append(allocator, @intCast(index)),
    };
    std.mem.sort(u32, heads.items, set, canonicalBefore);
    std.mem.sort(u32, unowned_runs.items, set, struct {
        fn lessThan(owner: *const Set, a: u32, b: u32) bool {
            return std.mem.lessThan(u8, owner.artifacts[a].code, owner.artifacts[b].code);
        }
    }.lessThan);

    // `order[new] = old`.
    var order = std.ArrayList(u32).empty;
    defer order.deinit(allocator);
    const placed = try allocator.alloc(bool, set.artifacts.len);
    defer allocator.free(placed);
    @memset(placed, false);
    for (heads.items) |head| {
        try order.append(allocator, head);
        for (set.artifacts[head].refs) |ref| {
            if (set.artifacts[ref.target].kind != .message_pool_run or owner_of[ref.target] != head) continue;
            if (placed[ref.target]) continue;
            placed[ref.target] = true;
            try order.append(allocator, ref.target);
        }
    }
    try order.appendSlice(allocator, unowned_runs.items);
    std.debug.assert(order.items.len == set.artifacts.len);
    const position = try allocator.alloc(u32, set.artifacts.len);
    defer allocator.free(position);
    for (order.items, 0..) |old, new| position[old] = @intCast(new);

    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    // Every constant under its content name, renamed once: a constant one
    // name names is one constant, which each artifact carrying it and the
    // image's data share.
    var renamed_items = std.StringHashMap(DataItem).init(allocator);
    defer renamed_items.deinit();
    try renamed_items.ensureTotalCapacity(@intCast(all_items.items.len));
    for (all_items.items) |item| renamed_items.putAssumeCapacityNoClobber(item.name, try renamedData(a, item, &names));
    const artifacts = try a.alloc(Artifact, set.artifacts.len);
    for (order.items, artifacts) |old, *out| {
        const source = set.artifacts[old];
        out.* = source;
        out.kind = switch (source.kind) {
            .rc_helper => |name| .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => source.kind,
        };
        out.code = try a.dupe(u8, source.code);
        const refs = try a.dupe(Reference, source.refs);
        for (refs) |*ref| ref.target = position[ref.target];
        out.refs = refs;
        const symbolic_refs = try a.dupe(SymbolicReference, source.symbolic_refs);
        for (symbolic_refs) |*ref| switch (ref.target) {
            .rc_helper => |name| ref.target = .{ .rc_helper = try a.dupe(u8, name) },
            .proc, .boxy_thunk => {},
        };
        out.symbolic_refs = symbolic_refs;
        out.lines = try a.dupe(LineEntry, source.lines);
        out.failure_sites = try a.dupe(FailureSite, source.failure_sites);
        out.runtime_refs = try a.dupe(RuntimeReference, source.runtime_refs);
        const relocations = try a.dupe(NamedRelocation, source.relocations);
        for (relocations) |*relocation| {
            const renamed = names.renames(relocation.name);
            relocation.name = try a.dupe(u8, names.of(relocation.name));
            if (renamed) relocation.scope = .shared;
        }
        out.relocations = relocations;
        const data = try a.alloc(DataItem, source.data.len);
        for (source.data, data) |item, *owned| owned.* = renamed_items.get(item.name) orelse unreachable; // `all_items` holds every artifact's data by name
        out.data = data;
    }

    const data = try a.alloc(CanonicalDatum, all_items.items.len);
    var data_len: usize = 0;
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();
    for (all_items.items) |item| {
        const renamed = renamed_items.get(item.name) orelse unreachable; // every collected item was renamed above
        // Distinct program constants with the same content are one symbol.
        if ((try seen.getOrPut(renamed.name)).found_existing) continue;
        data[data_len] = .{ .item = renamed, .host_visible = visible.contains(item.name) };
        data_len += 1;
    }
    // The loop above keeps one datum per name, so ordering by name alone is
    // total and an unstable sort orders them exactly as a stable one would.
    std.mem.sortUnstable(CanonicalDatum, data[0..data_len], {}, struct {
        fn lessThan(_: void, lhs: CanonicalDatum, rhs: CanonicalDatum) bool {
            return std.mem.lessThan(u8, lhs.item.name, rhs.item.name);
        }
    }.lessThan);

    const source_files = try a.dupe([32]u8, set.source_files);
    return .{
        .set = .{ .arena = arena, .artifacts = artifacts, .source_files = source_files },
        .data = data[0..data_len],
    };
}

fn renamedData(a: Allocator, item: DataItem, names: *const ContentNames) Allocator.Error!DataItem {
    const relocations = try a.dupe(DataRelocation, item.relocations);
    var copier = NameCopier{};
    var previous: ?[]const u8 = null;
    var renamed: []const u8 = &.{};
    for (relocations) |*relocation| {
        const same = if (previous) |name| std.mem.eql(u8, name, relocation.name) else false;
        if (!same) {
            previous = relocation.name;
            renamed = names.of(relocation.name);
        }
        relocation.name = try copier.copy(a, renamed);
    }
    return .{
        .name = try a.dupe(u8, names.of(item.name)),
        .bytes = try a.dupe(u8, item.bytes),
        .alignment = item.alignment,
        .symbol_offset = item.symbol_offset,
        .relocations = relocations,
        .program_local_name = false,
    };
}

/// The canonical order of two head artifacts: by kind, then by what names
/// the artifact.
fn canonicalBefore(set: *const Set, a: u32, b: u32) bool {
    const lhs = set.artifacts[a].kind;
    const rhs = set.artifacts[b].kind;
    const lhs_rank = canonicalRank(lhs);
    const rhs_rank = canonicalRank(rhs);
    if (lhs_rank != rhs_rank) return lhs_rank < rhs_rank;
    return switch (lhs) {
        .proc => |identity| std.mem.lessThan(u8, &identity.bytes, &rhs.proc.bytes),
        .boxy_thunk => |identity| std.mem.lessThan(u8, &identity.bytes, &rhs.boxy_thunk.bytes),
        .rc_helper => |name| std.mem.lessThan(u8, name, rhs.rc_helper),
        // Only one placement's own code has these kinds, which name nothing;
        // the code bytes order them.
        .entrypoint, .branch_island, .message_pool_run => std.mem.lessThan(u8, set.artifacts[a].code, set.artifacts[b].code),
    };
}

fn canonicalRank(kind: Kind) u8 {
    return switch (kind) {
        .proc => 0,
        .boxy_thunk => 1,
        .rc_helper => 2,
        .entrypoint => 3,
        .branch_island => 4,
        .message_pool_run => 5,
    };
}

/// Place every artifact of a canonical image's set into an open code
/// generator, in set order, and resolve every reference. A procedure or thunk
/// the program declares lands as that procedure, and a helper whose key the
/// program knows as that helper; any other lands as spliced code, as a
/// splice places it. Returns each artifact's start; the caller owns it.
pub fn assembleCanonical(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    procs_by_identity: *const std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId),
    helper_keys: *const HelperKeys,
) Allocator.Error![]usize {
    const starts = try allocator.alloc(usize, set.artifacts.len);
    errdefer allocator.free(starts);
    for (set.artifacts, starts) |artifact, *start| {
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .spliced_proc = identity },
            .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .boxy_thunk = proc_id } else .{ .spliced_boxy_thunk = identity },
            .rc_helper => |name| if (helper_keys.get(name)) |key| .{ .rc_helper = key } else .spliced_helper,
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        start.* = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        try appendMetadata(CG, codegen, artifact, start.*, .same_domain);
        try appendRuntimeRefs(CG, codegen, artifact, start.*);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name, relocation.scope);
            const offset: u64 = start.* + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => |function_kind| .{ .linked_function = .{ .offset = offset, .symbol = symbol, .kind = function_kind } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }
    for (set.artifacts, starts) |artifact, start| {
        for (artifact.refs) |ref| {
            const target_start = starts[ref.target];
            const target: CG.CodeRefTarget = switch (set.artifacts[ref.target].kind) {
                .proc => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .offset = target_start + ref.delta },
                .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .boxy_thunk = proc_id } else .{ .offset = target_start + ref.delta },
                .rc_helper => |name| if (helper_keys.get(name)) |key| .{ .rc_helper = key } else .{ .offset = target_start + ref.delta },
                .entrypoint, .message_pool_run, .branch_island => .{ .offset = target_start + ref.delta },
            };
            try codegen.patchAssembledRef(
                start + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                target,
                target_start + ref.delta,
            );
        }
    }
    try codegen.resolveAssembledSymbolicRefs();
    return starts;
}

/// Why an artifact set could not be placed into a program.
pub const AssembleError = Allocator.Error || error{
    /// A procedure or thunk artifact names an identity the program has no proc for.
    UnknownProcIdentity,
    /// A helper artifact names a helper the program has no key for.
    UnknownRcHelper,
};

/// Keys of the refcount helpers a program can name, by symbol name. A program
/// assembling artifacts from its own compilation fills this from the original
/// code generator's helpers.
pub const HelperKeys = std.StringHashMap(u64);

/// Append every artifact of `set` to an open code generator in set order and
/// re-resolve every reference to where its target landed.
pub fn assemble(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    proc_specs: []const lir.LIR.LirProcSpec,
    helper_keys: *const HelperKeys,
) AssembleError!void {
    try append(CG, allocator, codegen, set, proc_specs, helper_keys);
    try codegen.resolveAssembledSymbolicRefs();
}

/// Append a set without requiring externally referenced code to exist yet.
/// Resolve queued references only after all module fragments and helpers land.
pub fn append(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    proc_specs: []const lir.LIR.LirProcSpec,
    helper_keys: *const HelperKeys,
) AssembleError!void {
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @fromBackingInt(@intCast(@as(u32, @intCast(index)))));
    }

    try appendPrepared(CG, allocator, codegen, set, &procs_by_identity, helper_keys);
}

/// Append using the coordinator's shared identity index. Reusing this index
/// avoids rescanning the module's procedures for every independent fragment.
pub fn appendPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    procs_by_identity: *const std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId),
    helper_keys: *const HelperKeys,
) AssembleError!void {
    const starts = try allocator.alloc(usize, set.artifacts.len);
    defer allocator.free(starts);

    for (set.artifacts, 0..) |artifact, index| {
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc => |identity| .{ .proc = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
            .rc_helper => |name| .{ .rc_helper = helper_keys.get(name) orelse return error.UnknownRcHelper },
            .boxy_thunk => |identity| .{ .boxy_thunk = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        starts[index] = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        try appendMetadata(CG, codegen, artifact, starts[index], .same_domain);
        try appendRuntimeRefs(CG, codegen, artifact, starts[index]);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name, relocation.scope);
            const offset: u64 = starts[index] + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => |function_kind| .{ .linked_function = .{ .offset = offset, .symbol = symbol, .kind = function_kind } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }

    for (set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| {
            const target_artifact = set.artifacts[ref.target];
            const target: CG.CodeRefTarget = switch (target_artifact.kind) {
                .proc => |identity| .{ .proc = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
                .rc_helper => |name| .{ .rc_helper = helper_keys.get(name) orelse return error.UnknownRcHelper },
                .boxy_thunk => |identity| .{ .boxy_thunk = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
                // Imported pools do not share the destination's message-ID
                // domain. Preserve this exact placed target for later extraction.
                .message_pool_run, .entrypoint, .branch_island => .{ .offset = starts[ref.target] + ref.delta },
            };
            try codegen.patchAssembledRef(
                starts[index] + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                target,
                starts[ref.target] + ref.delta,
            );
        }
    }
}

/// How an appended artifact's lines reach the receiving program: in the same
/// source-file domain, or carried through a file map, which omits the lines
/// of files the receiving program does not have.
const LineBinding = union(enum) {
    same_domain,
    file_map: []const ?u32,
};

/// Record the runtime table numbers an artifact placed at `start` loads.
fn appendRuntimeRefs(comptime CG: type, codegen: *CG, artifact: Artifact, start: usize) Allocator.Error!void {
    for (artifact.runtime_refs) |runtime_ref| {
        try codegen.appendAssembledRelocation(.{ .runtime_ref = .{ .offset = start + runtime_ref.site, .ref = runtime_ref.ref } });
    }
}

fn appendMetadata(comptime CG: type, codegen: *CG, artifact: Artifact, start: usize, lines: LineBinding) Allocator.Error!void {
    switch (artifact.kind) {
        .proc => |identity| {
            try codegen.registerAssembledProc(identity, start + artifact.entry);
            try codegen.registerAssembledProcLoc(identity, switch (lines) {
                .same_domain => artifact.loc,
                .file_map => |file_map| loc: {
                    if (!artifact.loc.hasLocation()) break :loc artifact.loc;
                    var bound = artifact.loc;
                    bound.file = file_map[artifact.loc.file] orelse
                        base.invariant("a spliced procedure's location names a module the program lacks", .{});
                    break :loc bound;
                },
            });
        },
        .rc_helper => |name| try codegen.registerSplicedHelper(name, start + artifact.entry),
        .boxy_thunk => |identity| try codegen.registerAssembledThunk(identity, start + artifact.entry),
        .entrypoint, .message_pool_run, .branch_island => {},
    }
    for (artifact.symbolic_refs) |ref| {
        var rebased = ref;
        rebased.site = @intCast(start + ref.site);
        rebased.veneer = if (ref.veneer) |offset| @intCast(start + offset) else null;
        try codegen.queueAssembledSymbolicRef(rebased);
    }
    for (artifact.refs) |ref| {
        if (ref.form == .call) {
            try codegen.registerAssembledRefVeneer(start + ref.site, if (ref.veneer) |veneer| start + veneer else null);
        }
    }
    for (artifact.lines) |line| {
        const loc = switch (lines) {
            .same_domain => line.loc,
            .file_map => |file_map| loc: {
                if (!line.loc.hasLocation()) break :loc line.loc;
                var bound = line.loc;
                // An entry whose lines name a module version the program
                // lacks is never served (`LoadedPacks`), so its lines would
                // differ from the procedure's own.
                bound.file = file_map[line.loc.file] orelse
                    base.invariant("a spliced line names a module the program lacks", .{});
                break :loc bound;
            },
        };
        try codegen.appendAssembledLineEntry(.{ .offset = @intCast(start + line.offset), .loc = loc, .region = line.region });
    }
    for (artifact.failure_sites) |site| {
        try codegen.appendAssembledFailureSite(.{ .offset = @intCast(start + site.offset), .meaning = site.meaning });
    }
}

/// Why a program and its reassembled artifacts disagree.
pub const RoundTripError = ExtractError || AssembleError || error{
    /// The assembled program differs from the compiled one.
    RoundTripMismatch,
};

/// Assemble `original`'s artifacts into `fresh` (an open code generator built
/// with the same inputs) and check that the two agree on code bytes,
/// relocations, unwind records, and region layout.
pub fn verifyRoundTrip(
    comptime CG: type,
    allocator: Allocator,
    original: *CG,
    fresh: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
) RoundTripError!void {
    var set = try extract(CG, allocator, original, proc_specs, layout_store, string_exports, &.{}, &.{});
    defer set.deinit();

    var helper_keys = HelperKeys.init(allocator);
    defer {
        var names = helper_keys.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        helper_keys.deinit();
    }
    const helpers = try original.compiledRcHelpers(allocator);
    defer allocator.free(helpers);
    for (helpers) |helper| {
        const name = try LirCodeGenMod.compiledRcHelperSymbolName(allocator, layout_store, helper.key);
        errdefer allocator.free(name);
        try helper_keys.putNoClobber(name, helper.key);
    }

    try assemble(CG, allocator, fresh, &set, proc_specs, &helper_keys);
    try fresh.finishImage();

    const original_code = original.getGeneratedCode();
    const fresh_code = fresh.getGeneratedCode();
    if (!std.mem.eql(u8, original_code, fresh_code)) {
        const common = @min(original_code.len, fresh_code.len);
        var first_diff: usize = common;
        for (0..common) |i| {
            if (original_code[i] != fresh_code[i]) {
                first_diff = i;
                break;
            }
        }
        std.debug.print("ROUNDTRIP code mismatch: original len {d}, fresh len {d}, first difference at {d}\n", .{ original_code.len, fresh_code.len, first_diff });
        for (original.codeRegions()) |region| {
            if (first_diff >= region.start and first_diff < region.end) std.debug.print("  original region {s} [{d}, {d}) entry {d}\n", .{ @tagName(region.kind), region.start, region.end, region.entry });
        }
        for (fresh.codeRegions()) |region| {
            if (first_diff >= region.start and first_diff < region.end) std.debug.print("  fresh region {s} [{d}, {d}) entry {d}\n", .{ @tagName(region.kind), region.start, region.end, region.entry });
        }
        if (first_diff < common) {
            const lo = if (first_diff >= 8) first_diff - 8 else 0;
            const hi = @min(common, first_diff + 8);
            std.debug.print("  original bytes {x}\n  fresh bytes    {x}\n", .{ original_code[lo..hi], fresh_code[lo..hi] });
        }
        return error.RoundTripMismatch;
    }
    if (!relocationsMatch(CG, original, fresh)) {
        std.debug.print("ROUNDTRIP relocation mismatch: original {d}, fresh {d}\n", .{ original.getRelocations().len, fresh.getRelocations().len });
        return error.RoundTripMismatch;
    }
    if (!unwindMatches(original.getUnwindFunctions(), fresh.getUnwindFunctions())) {
        std.debug.print("ROUNDTRIP unwind mismatch: original {d}, fresh {d}\n", .{ original.getUnwindFunctions().len, fresh.getUnwindFunctions().len });
        return error.RoundTripMismatch;
    }
    if (!unwindMatches(original.getLineEntries(), fresh.getLineEntries())) {
        std.debug.print("ROUNDTRIP source line mismatch\n", .{});
        return error.RoundTripMismatch;
    }
    if (artifactRegionCount(CG, original.codeRegions()) != artifactRegionCount(CG, fresh.codeRegions())) {
        std.debug.print("ROUNDTRIP region count mismatch: original {d}, fresh {d}\n", .{ original.codeRegions().len, fresh.codeRegions().len });
        return error.RoundTripMismatch;
    }
}

/// Regions that become artifacts: everything but branch islands, which each
/// placement lays out for itself.
fn artifactRegionCount(comptime CG: type, regions: []const CG.CodeRegion) usize {
    var count: usize = 0;
    for (regions) |region| count += @intFromBool(regionHasArtifact(region.kind));
    return count;
}

fn relocationsMatch(comptime CG: type, original: *CG, fresh: *CG) bool {
    var original_index: usize = 0;
    var fresh_index: usize = 0;
    const original_relocations = original.getRelocations();
    const fresh_relocations = fresh.getRelocations();
    while (true) {
        while (original_index < original_relocations.len and original_relocations[original_index] == .retired) original_index += 1;
        while (fresh_index < fresh_relocations.len and fresh_relocations[fresh_index] == .retired) fresh_index += 1;
        const original_done = original_index == original_relocations.len;
        const fresh_done = fresh_index == fresh_relocations.len;
        if (original_done or fresh_done) return original_done and fresh_done;
        const lhs = original_relocations[original_index];
        const rhs = fresh_relocations[fresh_index];
        if (!relocationEql(CG, original, fresh, lhs, rhs)) return false;
        original_index += 1;
        fresh_index += 1;
    }
}

fn relocationEql(comptime CG: type, original: *CG, fresh: *CG, lhs: IndexedRelocation, rhs: IndexedRelocation) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .linked_function => |function| function.offset == rhs.linked_function.offset and function.kind == rhs.linked_function.kind and
            std.mem.eql(u8, original.symbolName(function.symbol), fresh.symbolName(rhs.linked_function.symbol)),
        .linked_data => |data| data.offset == rhs.linked_data.offset and data.kind == rhs.linked_data.kind and
            std.mem.eql(u8, original.symbolName(data.symbol), fresh.symbolName(rhs.linked_data.symbol)),
        .runtime_ref => |ref| ref.offset == rhs.runtime_ref.offset and std.meta.eql(ref.ref, rhs.runtime_ref.ref),
        .local_data, .retired => false,
    };
}

fn unwindMatches(original: anytype, fresh: @TypeOf(original)) bool {
    if (original.len != fresh.len) return false;
    for (original, fresh) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}

/// Where an artifact from another program's pack lands in this program: on
/// a procedure this program declares (external or its own), or as code that
/// only other spliced artifacts reach.
pub const SpliceError = Allocator.Error;

/// Which stable references to procedures `markReferrers` follows.
pub const ProcSymbols = enum {
    /// Every one: a stable reference to a procedure the receiving program
    /// lowers itself is not followed by a splice, but whether a program
    /// lowers it is not known before lowering, so it counts.
    follow,
    /// None: the marked artifacts are procedures named by site, which every
    /// program that takes an entry referring to one lowers itself.
    skip,
};

/// Mark every artifact of `set` whose splice would place a marked one:
/// every artifact that reaches a marked artifact through references, or
/// through a stable reference to an artifact of the set (to a procedure
/// only as `proc_symbols` says), transitively.
pub fn markReferrers(allocator: Allocator, set: *const Set, marked: []bool, proc_symbols: ProcSymbols) Allocator.Error!void {
    std.debug.assert(marked.len == set.artifacts.len);
    var by_proc = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator);
    defer by_proc.deinit();
    var by_thunk = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator);
    defer by_thunk.deinit();
    var by_helper = std.StringHashMap(u32).init(allocator);
    defer by_helper.deinit();
    for (set.artifacts, 0..) |artifact, index| switch (artifact.kind) {
        .proc => |identity| _ = try by_proc.getOrPutValue(identity, @intCast(index)),
        .boxy_thunk => |identity| _ = try by_thunk.getOrPutValue(identity, @intCast(index)),
        .rc_helper => |name| _ = try by_helper.getOrPutValue(name, @intCast(index)),
        .entrypoint, .message_pool_run, .branch_island => {},
    };
    // Reverse edges: who places whom.
    var referrers = try allocator.alloc(std.ArrayList(u32), set.artifacts.len);
    defer {
        for (referrers) |*list| list.deinit(allocator);
        allocator.free(referrers);
    }
    for (referrers) |*list| list.* = .empty;
    for (set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| try referrers[ref.target].append(allocator, @intCast(index));
        for (artifact.symbolic_refs) |ref| {
            const target = switch (ref.target) {
                .proc => |identity| switch (proc_symbols) {
                    .follow => by_proc.get(identity),
                    .skip => null,
                },
                .boxy_thunk => |identity| by_thunk.get(identity),
                .rc_helper => |name| by_helper.get(name),
            } orelse continue;
            try referrers[target].append(allocator, @intCast(index));
        }
    }
    var work = std.ArrayList(u32).empty;
    defer work.deinit(allocator);
    for (marked, 0..) |is_marked, index| if (is_marked) try work.append(allocator, @intCast(index));
    while (work.pop()) |index| {
        for (referrers[index].items) |referrer| {
            if (marked[referrer]) continue;
            marked[referrer] = true;
            try work.append(allocator, referrer);
        }
    }
}

/// Per artifact of `set`, whether a splice of it would place code for a
/// procedure in `identities`: one of the artifacts it places is such a
/// procedure, or a stable reference of one names one. The caller owns the
/// result.
pub fn reachingIdentities(allocator: Allocator, set: *const Set, identities: anytype) Allocator.Error![]bool {
    const marked = try allocator.alloc(bool, set.artifacts.len);
    errdefer allocator.free(marked);
    for (set.artifacts, marked) |artifact, *is_marked| {
        is_marked.* = switch (artifact.kind) {
            .proc => |identity| identities.contains(identity),
            .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => false,
        };
        for (artifact.symbolic_refs) |ref| switch (ref.target) {
            .proc => |identity| if (identities.contains(identity)) {
                is_marked.* = true;
            },
            .boxy_thunk, .rc_helper => {},
        };
    }
    try markReferrers(allocator, set, marked, .follow);
    return marked;
}

/// Per artifact of `set`, whether a splice of it would place a line or a
/// location in a source file `present` does not mark, by the set's file
/// index. The caller owns the result. A procedure keeps its identity across
/// an edit of its module only when its identity names it by site, so a
/// procedure with lines in another version of a module is site-named code,
/// which a program taking an entry that refers to it lowers itself and a
/// splice never places through that reference.
pub fn reachingAbsentFiles(allocator: Allocator, set: *const Set, present: []const bool) Allocator.Error![]bool {
    const marked = try allocator.alloc(bool, set.artifacts.len);
    errdefer allocator.free(marked);
    for (set.artifacts, marked) |artifact, *is_marked| {
        is_marked.* = artifact.loc.hasLocation() and !present[artifact.loc.file];
        for (artifact.lines) |line| {
            if (line.loc.hasLocation() and !present[line.loc.file]) is_marked.* = true;
        }
    }
    try markReferrers(allocator, set, marked, .skip);
    return marked;
}

/// Place the closure of `roots` (every artifact they reach through
/// references, transitively) from `set` into an open code generator, before
/// any of the program's own procedures compile. A procedure artifact whose
/// identity this program declares registers as that procedure, so the
/// program's own compile skips it and every call to it is an ordinary
/// direct call; refcount helpers register by name so a later request for the
/// same helper reuses the spliced code. `placed` remembers which artifacts of
/// `set` are already in the buffer across calls.
/// Worker identities also name Boxy thunks, but the two ABIs have separate
/// registries and region kinds so neither can satisfy demand for the other.
pub fn splice(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    roots: []const u32,
    procs_by_identity: *const std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId),
    placed: *std.AutoHashMap(u32, usize),
    data_out: *std.ArrayList(DataItem),
) SpliceError!void {
    // Closure in first-discovery order: deterministic for a deterministic
    // root order, and every target is placed before its references resolve.
    var order = std.ArrayList(u32).empty;
    defer order.deinit(allocator);
    var stack = std.ArrayList(u32).empty;
    defer stack.deinit(allocator);
    var seen = std.AutoHashMap(u32, void).init(allocator);
    defer seen.deinit();
    // A line names a version of a module by its content identity. The
    // receiving program's table has that version only when the module is
    // unchanged since the pack was written; a line of any other version
    // describes source this program does not have, so it is not emitted.
    const file_map = try allocator.alloc(?u32, set.source_files.len);
    defer allocator.free(file_map);
    for (set.source_files, file_map) |identity, *mapped| {
        mapped.* = for (0..codegen.store.sourceFileCount()) |file| {
            const candidate = codegen.store.sourceFileModuleIdentity(@intCast(file));
            if (std.mem.eql(u8, &candidate, &identity)) break @intCast(file);
        } else null;
    }
    for (roots) |root| try stack.append(allocator, root);
    while (stack.pop()) |index| {
        if (placed.contains(index)) continue;
        const gop = try seen.getOrPut(index);
        if (gop.found_existing) continue;
        // Another pack already spliced this artifact: content names make
        // the code the same, so references resolve to the existing copy.
        const artifact = set.artifacts[index];
        const existing: ?usize = switch (artifact.kind) {
            .proc => |identity| codegen.splicedProcStart(identity),
            .boxy_thunk => |identity| if (codegen.assembledThunkEntry(identity)) |entry| entry - artifact.entry else null,
            .rc_helper => |name| if (codegen.splicedHelperEntry(name)) |entry| entry - artifact.entry else null,
            .entrypoint, .message_pool_run, .branch_island => null,
        };
        if (existing) |start| {
            try placed.putNoClobber(index, start);
            continue;
        }
        try order.append(allocator, index);
        const refs = set.artifacts[index].refs;
        var i = refs.len;
        while (i > 0) {
            i -= 1;
            try stack.append(allocator, refs[i].target);
        }
        // A pack may combine independent fragments. Stable references to
        // definitions in this pack participate in the same transitive closure.
        for (artifact.symbolic_refs) |ref| {
            // A procedure of this program has its own code, compiled or
            // spliced as an entry of its own under the key it was served by;
            // a copy from this pack never stands in for it.
            switch (ref.target) {
                .proc => |identity| if (procs_by_identity.contains(identity)) continue,
                .boxy_thunk, .rc_helper => {},
            }
            if (set.definitionOf(ref.target)) |definition| try stack.append(allocator, definition);
        }
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .spliced_proc = identity },
            .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .boxy_thunk = proc_id } else .{ .spliced_boxy_thunk = identity },
            .rc_helper => .spliced_helper,
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        const start = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        try appendMetadata(CG, codegen, artifact, start, .{ .file_map = file_map });
        // A program's own code loads its table numbers; code another
        // program compiled never reaches the runtime's tables.
        if (artifact.runtime_refs.len != 0)
            base.invariant("a spliced artifact loads another program's Boxy runtime table numbers", .{});
        switch (artifact.kind) {
            .rc_helper => |name| try codegen.registerSplicedHelper(name, start + artifact.entry),
            .proc => |identity| try codegen.registerSplicedProc(identity, start),
            .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
        }
        try placed.putNoClobber(index, start);
        for (artifact.data) |item| try data_out.append(allocator, item);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name, relocation.scope);
            const offset: u64 = start + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => |function_kind| .{ .linked_function = .{ .offset = offset, .symbol = symbol, .kind = function_kind } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const start = placed.get(index) orelse unreachable; // the placement loop above put every index in order
        for (artifact.refs) |ref| {
            const target_start = placed.get(ref.target) orelse unreachable; // the traversal pushed every ref target, so each was placed as existing or appended to order
            const target_artifact = set.artifacts[ref.target];
            const target: CG.CodeRefTarget = switch (target_artifact.kind) {
                .proc => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .offset = target_start + ref.delta },
                .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .boxy_thunk = proc_id } else .{ .offset = target_start + ref.delta },
                .rc_helper, .entrypoint, .message_pool_run, .branch_island => .{ .offset = target_start + ref.delta },
            };
            try codegen.patchAssembledRef(
                start + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                target,
                target_start + ref.delta,
            );
        }
    }
}

test "independent message pools preserve their targets across repeated artifact assembly" {
    const allocator = std.testing.allocator;
    inline for (.{ @import("roc_target").RocTarget.x64linux, @import("roc_target").RocTarget.arm64linux }) |target| {
        const CG = LirCodeGenMod.LirCodeGen(target);
        var store = lir.LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        var procs: [2]lir.LIR.LirProcSpecId = undefined; // Filled by addProcSpec before emission.
        for (&procs, [_][]const u8{ "first independent message", "second independent message" }, 0..) |*proc, message, index| {
            const text = try store.insertString(message);
            const body = try store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = text } } }, .test_fixture);
            proc.* = try store.addProcSpec(.{
                .name = store.freshSyntheticSymbol(),
                .identity = lir.ProcIdentity.forTest(@intCast(index)),
                .args = .empty(),
                .body = body,
                .ret_layout = .zst,
            }, .none);
        }
        var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer image.deinit();
        var helpers = HelperKeys.init(allocator);
        defer helpers.deinit();
        for (procs) |proc| {
            var producer = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
            defer producer.deinit();
            var fragment = try compileProcFragment(CG, allocator, &producer, proc, store.getProcSpecs(), &layouts, &.{}, &.{});
            defer fragment.deinit();
            try append(CG, allocator, &image, &fragment.set, store.getProcSpecs(), &helpers);
        }
        try image.finishImage();
        var pools: usize = 0;
        for (image.codeRegions()) |region| {
            if (region.kind == .message_pool_run) pools += 1;
        }
        try std.testing.expectEqual(@as(usize, 2), pools);
        var fresh = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer fresh.deinit();
        try verifyRoundTrip(CG, allocator, &image, &fresh, store.getProcSpecs(), &layouts, &.{});
    }
}

test "artifact local calls reserve veneers before later regions exceed reach" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.arm64linux);
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, .u64);
    defer layouts.deinit();
    var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer image.deinit();
    image.codegen.branch_reach_limit = 4096;
    const gap = @as([8192]u8, @splat(0));
    var set = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{
            .{
                .kind = .entrypoint,
                .code = "\x00\x00\x00\x94",
                .entry = 0,
                .frame = null,
                .refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }},
                .relocations = &.{},
                .data = &.{},
            },
            .{ .kind = .entrypoint, .code = &gap, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
            .{ .kind = .entrypoint, .code = "\xc0\x03\x5f\xd6", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        },
    };
    defer set.deinit();
    var helpers = HelperKeys.init(allocator);
    defer helpers.deinit();
    try assemble(CG, allocator, &image, &set, &.{}, &helpers);
    try image.finishImage();
    const veneer = image.codeRefVeneer(0) orelse return error.TestUnexpectedResult;
    try std.testing.expect(veneer < image.codegen.branch_reach_limit);
    const bytes = image.getGeneratedCode();
    const branch = std.mem.readInt(u32, bytes[0..4], .little);
    try std.testing.expectEqual(@as(u32, 0x94000000) | @as(u32, @intCast(veneer / 4)), branch);
    try std.testing.expectEqual(@as(?usize, bytes.len - 4), image.codegen.branch_sites.items[0].target);
    // The target is beyond the direct-call reach, not merely a nearby veneer.
    try std.testing.expect(bytes.len > gap.len);
}

test "artifact combination owns all fields and rebases only local references" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCombineOwnership, .{});
}

test "artifact prepared data captures only the reached closure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testPreparedData, .{});
}

fn testPreparedData(allocator: Allocator) (ExtractError || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var prepared = try PreparedData.init(allocator, &.{
        testExport("literal", "owned string", &.{}, false),
        testExport("unreferenced", "must not be carried", &.{}, false),
    }, &.{
        testExport("roc__d0", "placeholder", &.{}, false),
    }, &.{});
    var prepared_live = true;
    defer if (prepared_live) prepared.deinit();
    var arena = collections.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();
    // A slot the catalog does not carry is left for the image to bind.
    var refs = [_]NamedRelocation{
        .{ .offset = 0, .name = "literal", .scope = .shared, .kind = .{ .data = .rel32 } },
        .{ .offset = 4, .name = "roc__d1", .scope = .program, .kind = .{ .data = .rel32 } },
    };
    const items = try captureData(allocator, a, &refs, &prepared);
    prepared.deinit();
    prepared_live = false;
    try std.testing.expectEqual(@as(usize, 1), items.len);
    try std.testing.expectEqualStrings("literal", items[0].name);
    try std.testing.expectEqualStrings("owned string", items[0].bytes);
    try std.testing.expectEqualStrings("literal", refs[0].name);
}

test "artifact metadata queues unresolved targets, preserves same-offset line order, and binds file domains" {
    const Recorder = struct {
        const Self = @This();
        refs: [2]SymbolicReference = undefined,
        lines: [2]LineEntry = undefined,
        refs_len: usize = 0,
        lines_len: usize = 0,
        proc_entry: usize = 0,
        veneer_site: usize = 0,
        veneer_offset: usize = 0,

        pub fn registerAssembledProc(self: *Self, _: lir.ProcIdentity, entry: usize) Allocator.Error!void {
            self.proc_entry = entry;
        }
        pub fn registerAssembledProcLoc(_: *Self, _: lir.ProcIdentity, _: base.SourceLoc) Allocator.Error!void {}
        pub fn registerAssembledThunk(_: *Self, _: lir.ProcIdentity, _: usize) Allocator.Error!void {}
        pub fn registerSplicedHelper(_: *Self, _: []const u8, _: usize) Allocator.Error!void {}
        pub fn queueAssembledSymbolicRef(self: *Self, ref: SymbolicReference) Allocator.Error!void {
            self.refs[self.refs_len] = ref;
            self.refs_len += 1;
        }
        pub fn registerAssembledRefVeneer(self: *Self, site: usize, veneer: ?usize) Allocator.Error!void {
            self.veneer_site = site;
            self.veneer_offset = veneer orelse 0;
        }
        pub fn appendAssembledLineEntry(self: *Self, line: LineEntry) Allocator.Error!void {
            self.lines[self.lines_len] = line;
            self.lines_len += 1;
        }
        pub fn appendAssembledFailureSite(_: *Self, _: FailureSite) Allocator.Error!void {}
    };
    var recorder = Recorder{};
    const artifact = Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "code",
        .entry = 1,
        .frame = null,
        .refs = &.{.{ .site = 2, .form = .call, .target = 0, .delta = 1, .veneer = 3 }},
        .symbolic_refs = &.{
            .{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) }, .veneer = 3 },
            .{ .site = 1, .form = .addr, .target = .{ .rc_helper = "unemitted" } },
        },
        .lines = &.{
            .{ .offset = 0, .loc = .{ .file = 3, .line = 10, .column = 1 }, .region = base.Region.zero() },
            .{ .offset = 0, .loc = .{ .file = 3, .line = 11, .column = 2 }, .region = base.Region.zero() },
        },
        .relocations = &.{},
        .data = &.{},
    };
    try appendMetadata(Recorder, &recorder, artifact, 100, .same_domain);
    try std.testing.expectEqual(@as(usize, 101), recorder.proc_entry);
    try std.testing.expectEqual(@as(u32, 100), recorder.refs[0].site);
    try std.testing.expectEqual(@as(?u32, 103), recorder.refs[0].veneer);
    try std.testing.expectEqualDeep(artifact.symbolic_refs[0].target, recorder.refs[0].target);
    try std.testing.expectEqualStrings("unemitted", recorder.refs[1].target.rc_helper);
    try std.testing.expectEqual(@as(usize, 102), recorder.veneer_site);
    try std.testing.expectEqual(@as(usize, 103), recorder.veneer_offset);
    for (artifact.lines, recorder.lines[0..recorder.lines_len]) |original, rebased| {
        try std.testing.expectEqual(@as(u32, 100), rebased.offset);
        try std.testing.expectEqualDeep(original.loc, rebased.loc);
    }
    // A pack's file 3 is the receiving program's file 1.
    var persistent = Recorder{};
    try appendMetadata(Recorder, &persistent, artifact, 100, .{ .file_map = &.{ null, null, null, 1 } });
    try std.testing.expectEqual(@as(usize, 2), persistent.lines_len);
    for (artifact.lines, persistent.lines[0..persistent.lines_len]) |original, rebound| {
        try std.testing.expectEqual(@as(u32, 1), rebound.loc.file);
        try std.testing.expectEqual(original.loc.line, rebound.loc.line);
        try std.testing.expectEqual(original.loc.column, rebound.loc.column);
    }
    try std.testing.expectEqual(@as(usize, 2), persistent.refs_len);
}

fn testCombineOwnership(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    const source = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .rc_helper = "helper" },
            .code = "code",
            .entry = 1,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .addr, .target = 0, .delta = 1 }},
            .symbolic_refs = &.{
                .{ .site = 1, .form = .call, .target = .{ .rc_helper = "other" } },
                .{ .site = 2, .form = .addr, .target = .{ .proc = lir.ProcIdentity.forTest(4) } },
            },
            .lines = &.{
                .{ .offset = 0, .loc = .{ .file = 0, .line = 3, .column = 2 }, .region = base.Region.zero() },
                .{ .offset = 0, .loc = .{ .file = 0, .line = 4, .column = 5 }, .region = base.Region.zero() },
            },
            .relocations = &.{.{ .offset = 0, .name = "external", .scope = .shared, .kind = .{ .function = .x86_call_rel32 } }},
            .data = &.{.{
                .name = "data",
                .bytes = "contents",
                .alignment = 8,
                .symbol_offset = 0,
                .relocations = &.{.{ .offset = 0, .name = "pointer", .addend = 2, .function = true }},
            }},
        }},
        .source_files = &.{@as([32]u8, @splat(7))},
    };
    var copy = try combine(allocator, &.{&source});
    var copy_live = true;
    defer if (copy_live) copy.deinit();
    var combined = try combine(allocator, &.{ &copy, &copy });
    defer combined.deinit();
    copy.deinit();
    copy_live = false;
    try std.testing.expectEqualDeep(source.artifacts[0], combined.artifacts[0]);
    try std.testing.expectEqual(@as(u32, 1), combined.artifacts[1].refs[0].target);
    try std.testing.expectEqualDeep(source.artifacts[0].symbolic_refs, combined.artifacts[1].symbolic_refs);
    try std.testing.expectEqualDeep(source.artifacts[0].lines, combined.artifacts[1].lines);
    try std.testing.expectEqualDeep(source.artifacts[0].data, combined.artifacts[1].data);
    try std.testing.expectEqualDeep(source.source_files, combined.source_files);
}

fn testExport(name: []const u8, bytes: []const u8, relocations: []const lir.Program.StaticDataRelocation, is_exported: bool) lir.Program.StaticDataExport {
    return .{
        .symbol_name = name,
        .bytes = bytes,
        .alignment = 8,
        .is_global = false,
        .is_exported = is_exported,
        .relocations = relocations,
    };
}

fn testItem(name: []const u8, bytes: []const u8, relocations: []const DataRelocation, program_local_name: bool) DataItem {
    return .{ .name = name, .bytes = bytes, .alignment = 8, .symbol_offset = 0, .relocations = relocations, .program_local_name = program_local_name };
}

fn testReloc(offset: u32, name: []const u8, addend: i64) DataRelocation {
    return .{ .offset = offset, .name = name, .addend = addend, .function = false };
}

/// A set whose one artifact carries `data`; the set owns nothing.
fn testDataSet(storage: *[1]Artifact, data: []const DataItem) Set {
    storage[0] = .{ .kind = .entrypoint, .code = &.{}, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = data };
    return .{ .arena = collections.SingleThreadArena.init(std.testing.allocator), .artifacts = storage };
}

test "constants are named by content across programs, through cycles, and never when host-visible" {
    const testing = std.testing;

    // Program one: a cycle between the first two constants, a leaf, and a
    // host-visible export that points at the leaf.
    const one = [_]DataItem{
        testItem("roc__d0", &@as([16]u8, @splat('\x00')), &.{testReloc(8, "roc__d0_1", 0)}, true),
        testItem("roc__d0_1", &@as([16]u8, @splat('\x00')), &.{ testReloc(0, "roc__d0", 0), testReloc(8, "roc__d0_2", 4) }, true),
        testItem("roc__d0_2", "leaf", &.{}, true),
        testItem("roc__answer", &@as([8]u8, @splat('\x00')), &.{testReloc(0, "roc__d0_2", 0)}, false),
    };
    // Program two: the same graph under other names and another order.
    const two = [_]DataItem{
        testItem("roc__d7_2", "leaf", &.{}, true),
        testItem("roc__d7", &@as([16]u8, @splat('\x00')), &.{testReloc(8, "roc__d7_1", 0)}, true),
        testItem("roc__d7_1", &@as([16]u8, @splat('\x00')), &.{ testReloc(0, "roc__d7", 0), testReloc(8, "roc__d7_2", 4) }, true),
    };

    var storage_one: [1]Artifact = undefined;
    const set_one = testDataSet(&storage_one, &one);
    var names_one = try ContentNames.init(testing.allocator, &set_one);
    defer names_one.deinit();
    const one_root = names_one.of("roc__d0");
    const one_node = names_one.of("roc__d0_1");
    const one_leaf = names_one.of("roc__d0_2");
    var storage_two: [1]Artifact = undefined;
    const set_two = testDataSet(&storage_two, &two);
    var names_two = try ContentNames.init(testing.allocator, &set_two);
    defer names_two.deinit();

    try testing.expectEqualStrings("roc__answer", names_one.of("roc__answer"));
    try testing.expect(std.mem.startsWith(u8, one_root, content_data_prefix));
    try testing.expectEqualStrings(one_root, names_two.of("roc__d7"));
    try testing.expectEqualStrings(one_node, names_two.of("roc__d7_1"));
    try testing.expectEqualStrings(one_leaf, names_two.of("roc__d7_2"));
    try testing.expect(!std.mem.eql(u8, one_root, one_node));
    // Names outside the carried data keep their own spelling.
    try testing.expectEqualStrings("roc__pelsewhere", names_one.of("roc__pelsewhere"));

    // A different leaf changes every name that reaches it.
    const three = [_]DataItem{
        testItem("roc__d0", &@as([16]u8, @splat('\x00')), &.{testReloc(8, "roc__d0_1", 0)}, true),
        testItem("roc__d0_1", &@as([16]u8, @splat('\x00')), &.{ testReloc(0, "roc__d0", 0), testReloc(8, "roc__d0_2", 4) }, true),
        testItem("roc__d0_2", "LEAF", &.{}, true),
    };
    var storage_three: [1]Artifact = undefined;
    const set_three = testDataSet(&storage_three, &three);
    var names_three = try ContentNames.init(testing.allocator, &set_three);
    defer names_three.deinit();
    try testing.expect(!std.mem.eql(u8, one_root, names_three.of("roc__d0")));
    // The leaf's program-local name is the same in both programs, and its
    // content name is not.
    try testing.expect(!std.mem.eql(u8, one_leaf, names_three.of("roc__d0_2")));
}

test "combined sets keep the program-local constants of each set apart" {
    const allocator = std.testing.allocator;
    // Two programs each named a different constant `roc__d0`, and each
    // program's procedure reads its own.
    var first_relocations = [_]NamedRelocation{.{ .offset = 0, .name = "roc__d0", .scope = .program, .kind = .{ .data = .rel32 } }};
    var second_relocations = [_]NamedRelocation{.{ .offset = 0, .name = "roc__d0", .scope = .program, .kind = .{ .data = .rel32 } }};
    const first_data = [_]DataItem{testItem("roc__d0", "first constant", &.{}, true)};
    const second_data = [_]DataItem{testItem("roc__d0", "second constant", &.{}, true)};
    const first = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{.{ .kind = .{ .proc = lir.ProcIdentity.forTest(1) }, .code = "\x00\x00\x00\x00", .entry = 0, .frame = null, .refs = &.{}, .relocations = &first_relocations, .data = &first_data }},
    };
    const second = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{.{ .kind = .{ .proc = lir.ProcIdentity.forTest(2) }, .code = "\x00\x00\x00\x00", .entry = 0, .frame = null, .refs = &.{}, .relocations = &second_relocations, .data = &second_data }},
    };
    var combined = try combineDistinct(allocator, &.{ &first, &second });
    defer combined.deinit();
    try std.testing.expectEqual(@as(usize, 2), combined.artifacts.len);
    for (combined.artifacts, [_][]const u8{ "first constant", "second constant" }) |artifact, bytes| {
        try std.testing.expectEqualStrings(bytes, artifact.data[0].bytes);
        try std.testing.expect(!artifact.data[0].program_local_name);
        try std.testing.expectEqualStrings(artifact.data[0].name, artifact.relocations[0].name);
        try std.testing.expectEqual(SymbolScope.shared, artifact.relocations[0].scope);
    }
    try std.testing.expect(!std.mem.eql(u8, combined.artifacts[0].data[0].name, combined.artifacts[1].data[0].name));
}

test "an object pack names each constant as the datum anchored at its first byte" {
    const testing = std.testing;
    const Export = lir.Program.StaticDataExport;
    const Relocation = lir.Program.StaticDataRelocation;
    const zeros = @as([16]u8, @splat('\x00'));
    const leaf = "\x00\x00\x00\x00\x00\x00\x00\x00leaf!!!!";

    // A parent points at a leaf whose symbol sits 8 bytes in.
    const to_inner = [_]Relocation{.{ .offset = 0, .target_symbol_name = "roc__d0_1", .target = .{ .data_symbol = @fromBackingInt(1) } }};
    const inner = [_]Export{
        .{ .symbol_name = "roc__d0", .bytes = &zeros, .alignment = 8, .is_exported = false, .relocations = &to_inner },
        .{ .symbol_name = "roc__d0_1", .bytes = leaf, .symbol_offset = 8, .alignment = 8, .is_exported = false },
        .{ .symbol_name = "roc__answer", .bytes = &zeros, .alignment = 8, .relocations = &to_inner },
    };
    // The same memory, with the leaf's symbol at its first byte and the
    // parent's pointer 8 bytes past it.
    const to_start = [_]Relocation{.{ .offset = 0, .target_symbol_name = "roc__d3_1", .target = .{ .data_symbol = @fromBackingInt(1) }, .addend = 8 }};
    const start = [_]Export{
        .{ .symbol_name = "roc__d3", .bytes = &zeros, .alignment = 8, .is_exported = false, .relocations = &to_start },
        .{ .symbol_name = "roc__d3_1", .bytes = leaf, .alignment = 8, .is_exported = false },
    };
    // A parent pointing at the leaf's first byte instead.
    const to_leaf_start = [_]Relocation{.{ .offset = 0, .target_symbol_name = "roc__d5_1", .target = .{ .data_symbol = @fromBackingInt(1) } }};
    const other = [_]Export{
        .{ .symbol_name = "roc__d5", .bytes = &zeros, .alignment = 8, .is_exported = false, .relocations = &to_leaf_start },
        .{ .symbol_name = "roc__d5_1", .bytes = leaf, .alignment = 8, .is_exported = false },
    };

    var inner_names = try ContentNames.initObjectData(testing.allocator, &inner);
    defer inner_names.deinit();
    var start_names = try ContentNames.initObjectData(testing.allocator, &start);
    defer start_names.deinit();
    var other_names = try ContentNames.initObjectData(testing.allocator, &other);
    defer other_names.deinit();

    try testing.expect(std.mem.startsWith(u8, inner_names.of("roc__d0"), content_data_prefix));
    try testing.expectEqualStrings(inner_names.of("roc__d0"), start_names.of("roc__d3"));
    try testing.expectEqualStrings(inner_names.of("roc__d0_1"), start_names.of("roc__d3_1"));
    try testing.expect(!std.mem.eql(u8, inner_names.of("roc__d0"), other_names.of("roc__d5")));
    try testing.expectEqualStrings("roc__answer", inner_names.of("roc__answer"));

    // A constant anchored at its first byte is named as a dev pack names it.
    var storage: [1]Artifact = undefined;
    const items = [_]DataItem{
        testItem("roc__d5", &zeros, &.{testReloc(0, "roc__d5_1", 0)}, true),
        testItem("roc__d5_1", leaf, &.{}, true),
    };
    const set = testDataSet(&storage, &items);
    var dev_names = try ContentNames.init(testing.allocator, &set);
    defer dev_names.deinit();
    try testing.expectEqualStrings(dev_names.of("roc__d5"), other_names.of("roc__d5"));
}

fn testProc(id: u8, code: []const u8, refs: []const Reference, symbolic_refs: []const SymbolicReference, relocations: []const NamedRelocation, data: []const DataItem) Artifact {
    return .{
        .kind = .{ .proc = lir.ProcIdentity.forTest(id) },
        .code = code,
        .entry = 0,
        .frame = .{ .prologue_size = 1, .stack_alloc = 0, .frame_size = 0, .callee_saved_mask = 0, .epilogue_offset = 1, .uses_frame_pointer = true },
        .refs = refs,
        .symbolic_refs = symbolic_refs,
        .relocations = relocations,
        .data = data,
    };
}

test "a canonical image does not depend on artifact order or on what a program named its constants" {
    const allocator = std.testing.allocator;
    // One program placed the pool run and helper first and named its
    // constant `roc__d5`; another placed them last and named the same
    // constant `roc__d9`.
    const one_data = [_]DataItem{testItem("roc__d5", "constant", &.{}, true)};
    const two_data = [_]DataItem{testItem("roc__d9", "constant", &.{}, true)};
    const one_relocations = [_]NamedRelocation{.{ .offset = 1, .name = "roc__d5", .scope = .program, .kind = .{ .data = .rel32 } }};
    const two_relocations = [_]NamedRelocation{.{ .offset = 1, .name = "roc__d9", .scope = .program, .kind = .{ .data = .rel32 } }};
    const helper: Artifact = .{ .kind = .{ .rc_helper = "roc__rc_decref_x" }, .code = "\xeb\x00\xc3", .entry = 2, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    const pool: Artifact = .{ .kind = .message_pool_run, .code = "message", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    const one = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{
            pool,
            helper,
            testProc(2, "\x02\x02\x02\x02", &.{.{ .site = 0, .form = .addr, .target = 0, .delta = 3 }}, &.{}, &.{}, &.{}),
            testProc(1, "\x01\x01\x01\x01", &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }}, &.{.{ .site = 2, .form = .call, .target = .{ .rc_helper = "roc__rc_decref_x" } }}, &one_relocations, &one_data),
        },
    };
    const two = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{
            testProc(1, "\x01\x01\x01\x01", &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, &.{.{ .site = 2, .form = .call, .target = .{ .rc_helper = "roc__rc_decref_x" } }}, &two_relocations, &two_data),
            testProc(2, "\x02\x02\x02\x02", &.{.{ .site = 0, .form = .addr, .target = 3, .delta = 3 }}, &.{}, &.{}, &.{}),
            helper,
            pool,
        },
    };
    var first = try canonicalize(allocator, &one, &.{});
    defer first.deinit();
    var second = try canonicalize(allocator, &two, &.{});
    defer second.deinit();
    // Procedures by identity, the pool run right after the procedure
    // addressing it, then helpers.
    try std.testing.expectEqual(@as(usize, 4), first.set.artifacts.len);
    const proc_one: usize = if (std.meta.eql(first.set.artifacts[0].kind, Kind{ .proc = lir.ProcIdentity.forTest(1) })) 0 else 2;
    const proc_two: usize = if (proc_one == 0) 1 else 0;
    try std.testing.expectEqual(lir.ProcIdentity.forTest(1), first.set.artifacts[proc_one].kind.proc);
    try std.testing.expectEqual(lir.ProcIdentity.forTest(2), first.set.artifacts[proc_two].kind.proc);
    try std.testing.expect(first.set.artifacts[proc_two + 1].kind == .message_pool_run);
    try std.testing.expect(first.set.artifacts[3].kind == .rc_helper);
    try std.testing.expect(std.mem.lessThan(u8, &first.set.artifacts[@min(proc_one, proc_two)].kind.proc.bytes, &first.set.artifacts[@max(proc_one, proc_two)].kind.proc.bytes));
    for (first.set.artifacts, second.set.artifacts) |lhs, rhs| {
        try std.testing.expectEqualDeep(lhs.kind, rhs.kind);
        try std.testing.expectEqualSlices(u8, lhs.code, rhs.code);
        try std.testing.expectEqualDeep(lhs.refs, rhs.refs);
        try std.testing.expectEqual(lhs.relocations.len, rhs.relocations.len);
        for (lhs.relocations, rhs.relocations) |l, r| {
            try std.testing.expectEqualStrings(l.name, r.name);
            try std.testing.expectEqual(l.scope, r.scope);
        }
    }
    try std.testing.expectEqual(lir.ProcIdentity.forTest(2), first.set.artifacts[first.set.artifacts[proc_one].refs[0].target].kind.proc);
    try std.testing.expect(first.set.artifacts[first.set.artifacts[proc_two].refs[0].target].kind == .message_pool_run);
    // The constant is one symbol under its content name in both.
    try std.testing.expectEqual(@as(usize, 1), first.data.len);
    try std.testing.expectEqualStrings(first.data[0].item.name, second.data[0].item.name);
    try std.testing.expect(std.mem.startsWith(u8, first.data[0].item.name, content_data_prefix));
    try std.testing.expectEqualStrings(first.data[0].item.name, first.set.artifacts[proc_one].relocations[0].name);
    try std.testing.expectEqual(SymbolScope.shared, first.set.artifacts[proc_one].relocations[0].scope);
}

test "a host-visible constant keeps its name in a canonical image while the constants it names are named by content" {
    const allocator = std.testing.allocator;
    const exported = [_]DataItem{
        testItem("roc__answer", &@as([8]u8, @splat('\x00')), &.{testReloc(0, "roc__d3", 0)}, false),
        testItem("roc__d3", "leaf", &.{}, true),
    };
    const set = Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &.{} };
    var image = try canonicalize(allocator, &set, &exported);
    defer image.deinit();
    try std.testing.expectEqual(@as(usize, 2), image.data.len);
    var answer: ?CanonicalDatum = null;
    var leaf: ?CanonicalDatum = null;
    for (image.data) |datum| {
        if (std.mem.eql(u8, datum.item.name, "roc__answer")) answer = datum else leaf = datum;
    }
    try std.testing.expect(answer.?.host_visible);
    try std.testing.expect(!leaf.?.host_visible);
    try std.testing.expect(std.mem.startsWith(u8, leaf.?.item.name, content_data_prefix));
    try std.testing.expectEqualStrings(leaf.?.item.name, answer.?.item.relocations[0].name);
}

test "an image keeps only the code its roots reach" {
    const allocator = std.testing.allocator;
    // The root calls a procedure, names a constant whose function pointer
    // reaches another, and calls a helper by name; one procedure nothing
    // reaches is a lambda-set member lowered only for a spliced entry.
    const pointed = try lir.ProcIdentity.forTest(4).symbolName(allocator);
    defer allocator.free(pointed);
    const pointer = [_]DataRelocation{.{ .offset = 0, .name = pointed, .addend = 0, .function = true }};
    const constant = [_]DataItem{.{ .name = "roc__d1", .bytes = &@as([8]u8, @splat('\x00')), .alignment = 8, .symbol_offset = 0, .relocations = &pointer, .program_local_name = true }};
    const root_relocations = [_]NamedRelocation{.{ .offset = 1, .name = "roc__d1", .scope = .program, .kind = .{ .data = .rel32 } }};
    var catalog = try PreparedData.init(allocator, &.{}, &.{}, &constant);
    defer catalog.deinit();
    const set = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{
            testProc(1, "\x01\x01\x01\x01", &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, &.{.{ .site = 2, .form = .call, .target = .{ .rc_helper = "roc__rc_decref_x" } }}, &root_relocations, &constant),
            testProc(2, "\x02\x02\x02\x02", &.{}, &.{}, &.{}, &.{}),
            testProc(3, "\x03\x03\x03\x03", &.{}, &.{}, &.{}, &.{}),
            testProc(4, "\x04\x04\x04\x04", &.{}, &.{}, &.{}, &.{}),
            .{ .kind = .{ .rc_helper = "roc__rc_decref_x" }, .code = "\xeb\x00\xc3", .entry = 2, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        },
        // The kept set owns its copy of the file table too.
        .source_files = &.{@as([32]u8, @splat(1))},
    };
    var aliases = std.StringHashMap([]const u8).init(allocator);
    defer aliases.deinit();
    var kept = try retainReachable(allocator, &set, &catalog, .{
        .procs = &.{lir.ProcIdentity.forTest(1)},
        .data = &.{},
        .helper_aliases = &aliases,
        .all_thunks = false,
    });
    defer kept.deinit();
    try std.testing.expectEqual(@as(usize, 4), kept.artifacts.len);
    for (kept.artifacts) |artifact| switch (artifact.kind) {
        .proc => |identity| try std.testing.expect(!std.meta.eql(identity, lir.ProcIdentity.forTest(3))),
        .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
    };
    try std.testing.expectEqual(lir.ProcIdentity.forTest(2), kept.artifacts[kept.artifacts[0].refs[0].target].kind.proc);
}

test "a splice reaches a procedure through references and stable references" {
    const allocator = std.testing.allocator;
    const set = Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{
            testProc(1, "\x01\x01\x01\x01", &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, &.{}, &.{}, &.{}),
            testProc(2, "\x02\x02\x02\x02", &.{}, &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(3) } }}, &.{}, &.{}),
            testProc(3, "\x03\x03\x03\x03", &.{}, &.{}, &.{}, &.{}),
            testProc(4, "\x04\x04\x04\x04", &.{}, &.{}, &.{}, &.{}),
        },
    };
    var linked = std.AutoHashMapUnmanaged(lir.ProcIdentity, u32).empty;
    defer linked.deinit(allocator);
    try linked.put(allocator, lir.ProcIdentity.forTest(3), 0);
    const reaching = try reachingIdentities(allocator, &set, &linked);
    defer allocator.free(reaching);
    try std.testing.expectEqualSlices(bool, &.{ true, true, true, false }, reaching);

    // A procedure with lines in a file the program lacks is site-named code,
    // which the program lowers itself, so a stable reference to it places
    // nothing; a reference into the pack does.
    var lined = [_]Artifact{ set.artifacts[0], set.artifacts[1], set.artifacts[2], set.artifacts[3] };
    lined[2].lines = &.{.{ .offset = 0, .loc = .{ .file = 1, .line = 3, .column = 1 }, .region = base.Region.zero() }};
    lined[3].loc = .{ .file = 0, .line = 1, .column = 1 };
    const lined_set = Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &lined };
    const absent = try reachingAbsentFiles(allocator, &lined_set, &.{ true, false });
    defer allocator.free(absent);
    try std.testing.expectEqualSlices(bool, &.{ false, false, true, false }, absent);
    lined[1].lines = lined[2].lines;
    const reached = try reachingAbsentFiles(allocator, &lined_set, &.{ true, false });
    defer allocator.free(reached);
    try std.testing.expectEqualSlices(bool, &.{ true, true, true, false }, reached);
}
