//! On-disk form of one module's pack: a memory image of its procedure
//! artifacts and the offers that serve them.
//!
//! The image is read the way the checked-module cache is read: the file is
//! mapped, its header's entry-version hash is compared, and the columns are
//! the live tables themselves (fixed-width rows, every cross-reference a row
//! index, every byte string a range into one blob), relocated by one base
//! address per column. Nothing is decoded or checked to get at an artifact:
//! code, data and names are the mapped bytes, and the rows that describe an
//! artifact are read where they lie. The writer is this compiler, so the
//! shape of what it wrote is a compiler invariant, asserted in debug builds
//! when the pack is written and trusted after that; the only release-mode
//! checks a read performs are the header's version hash and the bounds of
//! each column and row range, which is what admitting any cache entry costs.
//!
//! The offer metadata that is not a flat row (storage-demand facts and
//! literal recipes, whose shapes nest) travels as opaque blobs in their
//! field-wise encoding and is decoded only for the offers a lookup admits.
//!
//! The encoding is a pure function of its inputs, so two builds of the same
//! module write identical files and the store's rename-into-place protocol
//! never has to compare contents. Line entries preserve exact SourceLoc
//! values and order; their file indices are meaningful only with the
//! originating LIR source-file domain.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const lir = @import("lir");
const ProcArtifact = @import("ProcArtifact.zig");
const ArtifactClosure = @import("ArtifactClosure.zig");
const SourceEvents = @import("builtins").source_events;

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;

/// Alignment of a pack image in memory: what the relocatable columns need.
pub const alignment: std.mem.Alignment = CompactWriter.SERIALIZATION_ALIGNMENT;

/// A pack image's bytes, aligned for viewing.
pub const Bytes = []align(alignment.toByteUnits()) u8;

const magic = "RPCK";

/// Version of the envelope: the header's layout and the placement of the
/// root after it. The columns' layout is folded into the entry-version hash
/// by `cache_versioning`, which composes the hash a writer stamps and a
/// reader compares.
pub const envelope_version: u32 = 1;

/// The fixed header every pack starts with; the root follows at `root_offset`.
pub const Header = extern struct {
    magic: [4]u8,
    reserved: [4]u8,
    /// Bytes from the root to the end of the image.
    data_size: u64,
    /// The entry-version hash of the compiler that wrote the pack.
    version_hash: [32]u8,
    /// Content identity of the artifact graph, over the artifact columns.
    physical_namespace: [32]u8,
};

/// Where the root lies: after the header, at the image's alignment.
pub const root_offset = std.mem.alignForward(usize, @sizeOf(Header), alignment.toByteUnits());

comptime {
    assertFullyDefined(Header);
}

/// One relocatable column: `len` rows of `T` starting `offset` bytes into
/// the image. Declares what the layout fingerprint and the bounds validation
/// look for, as `SerializedSlice` in the checked-module cache does.
pub fn Column(comptime T: type) type {
    comptime assertFullyDefined(T);
    return extern struct {
        offset: i64 = 0,
        len: u64 = 0,

        const Self = @This();

        /// The row type, read by the layout fingerprint.
        pub const SerializedElement = T;
        /// One relocatable base pointer, read by the relocation count.
        pub const serialized_relocatable_pointers: usize = 1;

        fn write(self: *Self, rows: []const T, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            if (rows.len == 0) {
                self.* = .{};
                return;
            }
            self.offset = @intCast(try writer.appendSlicePodZeroedOffset(gpa, rows));
            self.len = rows.len;
        }

        /// The rows, in the image whose root lies at `base`.
        pub fn view(self: *const Self, base_address: usize) []const T {
            if (self.len == 0) return &.{};
            const ptr: [*]const T = @ptrFromInt(base_address + @as(usize, @intCast(self.offset)));
            return ptr[0..@intCast(self.len)];
        }

        /// Bounds check against a `backing_len`-byte image.
        pub fn validateRelocations(self: *const Self, backing_len: u64) error{CorruptArtifact}!void {
            const bytes = std.math.mul(u64, self.len, @sizeOf(T)) catch return error.CorruptArtifact;
            return collections.validateRelocatedSpan(@alignOf(T), self.offset, bytes, backing_len);
        }
    };
}

/// `@compileError` unless every byte of `T` belongs to a declared field, so
/// the rows a pack writes are a function of their contents alone.
fn assertFullyDefined(comptime T: type) void {
    comptime {
        switch (@typeInfo(T)) {
            .int, .bool => {},
            .array => |info| assertFullyDefined(info.child),
            .@"struct" => |info| {
                if (info.layout != .@"extern") @compileError("pack row '" ++ @typeName(T) ++ "' must be an extern struct");
                var total: usize = 0;
                for (info.field_names, info.field_types) |field_name, field_type| {
                    if (@offsetOf(T, field_name) != total) @compileError("pack row '" ++ @typeName(T) ++ "' has padding before field '" ++ field_name ++ "'");
                    assertFullyDefined(field_type);
                    total += @sizeOf(field_type);
                }
                if (total != @sizeOf(T)) @compileError("pack row '" ++ @typeName(T) ++ "' has trailing padding");
            },
            .type,
            .void,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .optional,
            .error_union,
            .error_set,
            .@"enum",
            .@"union",
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .float,
            .pointer,
            .spirv,
            => @compileError("pack row '" ++ @typeName(T) ++ "' has an unsupported field type"),
        }
    }
}

/// A byte string in the image's name blob.
pub const NameRef = extern struct {
    offset: u32,
    len: u32,
};

/// A run of rows in a column.
pub const Range = extern struct {
    start: u32,
    len: u32,
};

/// A procedure's frame, when it has one.
pub const FrameRow = extern struct {
    prologue_size: u32,
    stack_alloc: u32,
    frame_size: u32,
    callee_saved_mask: u32,
    epilogue_offset: u32,
    uses_frame_pointer: u8,
    present: u8,
    reserved: [2]u8,
};

/// A root materialization (`lir.Program.RootMaterialization`), flattened.
pub const MaterializationRow = extern struct {
    module: [32]u8,
    representation_digest: [32]u8,
    layout_digest: [32]u8,
    wrapper_identity: [32]u8,
    /// The literal producer's conversion identity; zero for a checked root.
    conversion_identity: [32]u8,
    /// The checked root, or the literal producer's checked expression.
    checked: u32,
    /// 0 checked, 1 literal.
    producer: u8,
    /// The literal producer's rejection kind.
    kind: u8,
    abi: u8,
    present: u8,
};

/// One artifact the pack carries: where its code and rows lie, and its
/// contracts. The pack's rows are its artifacts followed by its references
/// (`Columns.references`), so an index at or past the artifact count names
/// an artifact another pack carries, by content address alone.
pub const ArtifactRow = extern struct {
    /// The procedure or Boxy thunk identity; zero for other kinds.
    identity: [32]u8,
    callable_contract: [32]u8,
    /// The artifact's content address (`ArtifactClosure.contentAddresses`).
    content: [32]u8,
    /// The refcount helper's name; empty for other kinds.
    helper_name: NameRef,
    code: Range,
    refs: Range,
    symbolic_refs: Range,
    lines: Range,
    relocations: Range,
    data: Range,
    source_events: Range,
    materializations: Range,
    /// The artifacts this one reaches directly: set-local references,
    /// symbolic references resolved to a definition in the pack, and the code
    /// targets of its data; ascending, without repeats.
    edges: Range,
    entry: u32,
    frame: FrameRow,
    root_materialization: MaterializationRow,
    /// `ProcArtifact.Kind`: 0 proc, 1 rc_helper, 2 boxy_thunk, 3 entrypoint,
    /// 4 message_pool_run, 5 branch_island.
    kind: u8,
    context_complete: u8,
    has_contract: u8,
    has_dependencies: u8,
    /// `FragmentContract`, when present.
    target: u8,
    cpu_level: u8,
    hot_reload: u8,
    default_platform_runtime: u8,
    /// `FragmentContextDependencies`, when present.
    static_data: u8,
    boxy_runtime_entry: u8,
    boxy_runtime: u8,
    dict_seed: u8,
    has_callable: u8,
    /// Whether a reference of this artifact resolves to nothing in the pack,
    /// which admission rejects along with every closure containing it.
    unresolved: u8,
    reserved: [2]u8,
};

/// A set-local reference (`ProcArtifact.Reference`).
pub const RefRow = extern struct {
    site: u32,
    target: u32,
    delta: u32,
    /// `no_veneer` when absent.
    veneer: u32,
    form: u8,
    reserved: [3]u8,
};

const no_veneer = std.math.maxInt(u32);

/// A symbolic reference (`ProcArtifact.SymbolicReference`).
pub const SymbolicRefRow = extern struct {
    identity: [32]u8,
    name: NameRef,
    site: u32,
    veneer: u32,
    form: u8,
    /// 0 proc, 1 rc_helper, 2 boxy_thunk.
    kind: u8,
    reserved: [2]u8,
};

/// A debug line entry (`LineEntry`).
pub const LineRow = extern struct {
    offset: u32,
    file: u32,
    line: u32,
    column: u32,
};

/// A named relocation of code (`ProcArtifact.NamedRelocation`).
pub const RelocationRow = extern struct {
    name: NameRef,
    offset: u32,
    /// 0 function, 1 data.
    kind: u8,
    data_kind: u8,
    scope: u8,
    reserved: u8,
};

/// A carried datum (`ProcArtifact.DataItem`).
pub const DataItemRow = extern struct {
    name: NameRef,
    bytes: Range,
    relocations: Range,
    alignment: u32,
    symbol_offset: u32,
    program_local_name: u8,
    reserved: [3]u8,
};

/// A relocation inside a carried datum (`ProcArtifact.DataRelocation`).
pub const DataRelocationRow = extern struct {
    addend: i64,
    name: NameRef,
    symbolic_name: NameRef,
    identity: [32]u8,
    offset: u32,
    local: u32,
    function: u8,
    external: u8,
    /// 0 none, 1 local, 2 symbolic.
    target: u8,
    /// The symbolic target's kind: 0 proc, 1 rc_helper, 2 boxy_thunk.
    symbolic_kind: u8,
    reserved: [4]u8,
};

/// A source-event certificate (`SourceEventCertificate`).
pub const CertificateRow = extern struct {
    descriptor: SourceEvents.Descriptor,
    data_name: NameRef,
    branch_regions: Range,
    site_origin: u32,
    /// 0 none, 1 checked_expr, 2 checked_pattern.
    site_kind: u8,
    reserved: [3]u8,
};

/// A source region (`base.Region`).
pub const RegionRow = extern struct {
    start: u32,
    end: u32,
};

/// A root materialization an artifact depends on (`RootMaterializationDependency`).
pub const MaterializationDependencyRow = extern struct {
    binding_name: NameRef,
    header_name: NameRef,
    materialization: MaterializationRow,
};

/// One offer (`SpecEntry`), with its nested demand as a blob.
pub const SpecRow = extern struct {
    key: [32]u8,
    relation: [32]u8,
    rc_borrowed_params: u64,
    rc_ret_lenders: u64,
    rc_read_only_params: u64,
    rc_ret_unique_fields: u64,
    /// The `CachedCtfeDemand` in its field-wise encoding, in the blob column.
    demand: Range,
    root_reads: Range,
    conditions: Range,
    /// The checked modules the offer's facts and serving closure name, in
    /// the modules column; a program lacking one of them never admits the
    /// closure.
    modules: Range,
    artifact: u32,
    has_relation: u8,
    has_demand: u8,
    rc_ret_borrowed: u8,
    rc_ret_unique: u8,
    /// `SpecEntry.by_identity`.
    by_identity: u8,
    reserved: [7]u8,
};

/// A compile-time root the served code reads (`ComptimeRootRead`).
pub const RootReadRow = extern struct {
    module: [32]u8,
    root: u32,
    reserved: u32,
};

/// A settled discovery outcome (`DiscoveryEntry`).
pub const DiscoveryRow = extern struct {
    key: [32]u8,
    relation: [32]u8,
    has_relation: u8,
    reserved: [3]u8,
};

/// The literal read behind a materializer (`MaterializerRead`), the read as a blob.
pub const MaterializerReadRow = extern struct {
    wrapper_identity: [32]u8,
    read: Range,
};

/// The columns of a pack, as the writer gathers them and the reader views
/// them.
pub const Columns = struct {
    code: []const u8 = &.{},
    names: []const u8 = &.{},
    data_bytes: []const u8 = &.{},
    blobs: []const u8 = &.{},
    artifacts: []const ArtifactRow = &.{},
    /// Content addresses of the artifacts the pack names without carrying
    /// them, as rows numbered after the artifacts.
    references: []const [32]u8 = &.{},
    refs: []const RefRow = &.{},
    symbolic_refs: []const SymbolicRefRow = &.{},
    lines: []const LineRow = &.{},
    relocations: []const RelocationRow = &.{},
    data_items: []const DataItemRow = &.{},
    data_relocations: []const DataRelocationRow = &.{},
    certificates: []const CertificateRow = &.{},
    regions: []const RegionRow = &.{},
    materializations: []const MaterializationDependencyRow = &.{},
    edges: []const u32 = &.{},
    specs: []const SpecRow = &.{},
    root_reads: []const RootReadRow = &.{},
    conditions: []const u32 = &.{},
    /// Checked module keys offers name (`SpecRow.modules`).
    modules: []const [32]u8 = &.{},
    discovery: []const DiscoveryRow = &.{},
    materializer_reads: []const MaterializerReadRow = &.{},
};

/// The root of the image: one column marker per column of `Columns`.
pub const Serialized = extern struct {
    code: Column(u8),
    names: Column(u8),
    data_bytes: Column(u8),
    blobs: Column(u8),
    artifacts: Column(ArtifactRow),
    references: Column([32]u8),
    refs: Column(RefRow),
    symbolic_refs: Column(SymbolicRefRow),
    lines: Column(LineRow),
    relocations: Column(RelocationRow),
    data_items: Column(DataItemRow),
    data_relocations: Column(DataRelocationRow),
    certificates: Column(CertificateRow),
    regions: Column(RegionRow),
    materializations: Column(MaterializationDependencyRow),
    edges: Column(u32),
    specs: Column(SpecRow),
    root_reads: Column(RootReadRow),
    conditions: Column(u32),
    modules: Column([32]u8),
    discovery: Column(DiscoveryRow),
    materializer_reads: Column(MaterializerReadRow),

    pub const root_contract: collections.serde_validation.RootContract = .{ .Owner = Columns };

    fn write(self: *Serialized, columns: *const Columns, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
        inline for (@typeInfo(Columns).@"struct".field_names) |field_name| {
            try @field(self, field_name).write(@field(columns, field_name), gpa, writer);
        }
    }

    fn view(self: *const Serialized, base_address: usize) Columns {
        var columns: Columns = .{};
        inline for (@typeInfo(Columns).@"struct".field_names) |field_name| {
            @field(columns, field_name) = @field(self, field_name).view(base_address);
        }
        return columns;
    }
};

/// One specialization the pack can serve: its reservation-time key, the
/// artifact holding its procedure, and the ownership signature and
/// uniqueness facts ARC solved for that procedure, which the linking program
/// adopts as fixed.
pub const SpecEntry = struct {
    /// The specialization key the offer serves, or, for an identity offer,
    /// the identity of the procedure it offers (design.md "Identity
    /// Offers").
    key: [32]u8,
    /// Whether `key` is a procedure identity rather than a specialization key.
    by_identity: bool = false,
    artifact: u32,
    /// Null is an authoritative independent summary, not missing metadata.
    platform_requirement_relation: ?[32]u8 = null,
    demand: ?lir.Program.CachedCtfeDemand = null,
    /// The compile-time roots the procedure's code reads, which a linking
    /// program demands (`Common.SpecCacheHit.comptime_root_reads`).
    comptime_root_reads: []const lir.CheckedPipeline.ComptimeRootRead = &.{},
    rc_borrowed_params: u64,
    rc_ret_borrowed: bool,
    rc_ret_lenders: u64,
    rc_read_only_params: u64,
    rc_ret_unique: bool,
    rc_ret_unique_fields: u64,
    /// Each entry an `lir.LIR.RcRetCondition`.
    rc_ret_conditions: []const u32,
    /// Every checked module the offer's facts and the artifacts of its
    /// serving closure name: demand reads, root reads, materializers and
    /// source-event certificates. A program that lacks one of them cannot be
    /// served the offer, and declines it before admitting its closure.
    modules: []const [32]u8 = &.{},
};

/// A specialization whose discovery outcome is settled, under the
/// specialization key it was reserved for (`lir.Program.DiscoverySummary`).
/// It names no artifact.
pub const DiscoveryEntry = struct {
    key: [32]u8,
    platform_requirement_relation: ?[32]u8 = null,
};

/// The literal read behind a literal materializer the pack's code calls,
/// under that materializer's wrapper identity. Serving the code that calls it
/// declares the literal from this recipe, wherever in the code the read is.
pub const MaterializerRead = struct {
    wrapper_identity: [32]u8,
    read: lir.Program.CachedLiteralRead,
};

/// A pack decoded whole: its artifact set, its offers, and the serving graph
/// of the set (each artifact's callees by set index, as the image records
/// them).
pub const Pack = struct {
    set: ProcArtifact.Set,
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry = &.{},
    materializer_reads: []const MaterializerRead = &.{},
    edges: []const []const u32,
    unresolved: []const bool,

    pub fn deinit(self: *Pack) void {
        self.set.deinit();
    }
};

/// Why bytes could not be read as a pack: not a pack image, truncated, or
/// written by another version of the compiler.
pub const ReadError = Allocator.Error || error{
    MalformedPack,
    UnsupportedPackVersion,
};

/// Why an artifact set could not be written as a pack.
pub const WriteError = Allocator.Error;

/// The entry-version hash tests stamp and compare; a build stamps the hash
/// `cache_versioning` composes for packs.
pub const test_version_hash: [32]u8 = @splat(0x50);

/// Content identity of a literal recipe: the digest of its persisted
/// encoding, which names exactly the declared fields and no in-memory layout.
pub fn recipeDigest(allocator: Allocator, recipe: lir.Program.CachedLiteralRecipe) Allocator.Error![32]u8 {
    var bytes = std.ArrayList(u8).empty;
    defer bytes.deinit(allocator);
    var writer = Writer{ .allocator = allocator, .bytes = &bytes };
    try writer.context(recipe);
    var digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(bytes.items, &digest, .{});
    return digest;
}

/// A set's artifacts gathered into the artifact columns of a pack, without
/// the offers. Packs offering different specs of one set share a body.
pub const Body = struct {
    arena: collections.SingleThreadArena,
    columns: Columns,
    physical_namespace: [32]u8,
    /// The content address of each artifact of the set, in the arena.
    addresses: []const [32]u8,
    /// The row of each artifact of the set: its index among the carried
    /// artifacts, or, past them, among the references.
    rows: []const u32,

    pub fn deinit(self: *Body) void {
        self.arena.deinit();
    }
};

/// What a body carries of its set.
pub const BodyOptions = struct {
    /// The content address of each artifact, when the producer has them;
    /// computed from the set otherwise.
    addresses: ?[]const [32]u8 = null,
    /// Which artifacts the pack carries. The rest are named by address
    /// alone, for a reader that finds them in the pack that carries them.
    /// Every artifact is carried when null.
    carried: ?[]const bool = null,
};

/// Write a pack of `set` offering `specs`, stamped with `version_hash`.
pub fn write(allocator: Allocator, set: *const ProcArtifact.Set, specs: []const SpecEntry, version_hash: [32]u8) WriteError!Bytes {
    return writeOffers(allocator, set, specs, &.{}, &.{}, version_hash);
}

/// `write`, with the discovery summaries the pack also offers and the literal
/// reads behind the literal materializers its code calls.
pub fn writeOffers(
    allocator: Allocator,
    set: *const ProcArtifact.Set,
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry,
    materializer_reads: []const MaterializerRead,
    version_hash: [32]u8,
) WriteError!Bytes {
    var body = try encodeBody(allocator, set, .{});
    defer body.deinit();
    return writeWithBody(allocator, &body, specs, discovery, materializer_reads, version_hash);
}

/// Gather a set's artifacts into pack columns. What the producer handed over
/// is checked in debug builds only: the shape of an artifact is a compiler
/// invariant, and a pack is read without re-checking it.
pub fn encodeBody(allocator: Allocator, set: *const ProcArtifact.Set, options: BodyOptions) WriteError!Body {
    if (builtin.mode == .debug) debugValidateArtifacts(allocator, set);
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    var builder = Builder.init(allocator, arena.allocator());
    defer builder.deinit();
    var names = try ProcArtifact.ContentNames.init(allocator, set);
    defer names.deinit();
    var graph = try ArtifactClosure.init(allocator, set);
    defer graph.deinit();
    const addresses = if (options.addresses) |given|
        try arena.allocator().dupe([32]u8, given)
    else blk: {
        const computed = try graph.contentAddresses(&names);
        defer allocator.free(computed);
        break :blk try arena.allocator().dupe([32]u8, computed);
    };
    std.debug.assert(addresses.len == set.artifacts.len);
    // Carried artifacts take the first rows, in set order; the rest are
    // references after them, so every edge and offer names a row.
    const rows_of = try arena.allocator().alloc(u32, set.artifacts.len);
    var carried_count: u32 = 0;
    for (set.artifacts, 0..) |_, index| {
        if (options.carried == null or options.carried.?[index]) {
            rows_of[index] = carried_count;
            carried_count += 1;
        }
    }
    var reference_count: u32 = 0;
    for (set.artifacts, 0..) |_, index| {
        if (options.carried != null and !options.carried.?[index]) {
            rows_of[index] = carried_count + reference_count;
            reference_count += 1;
            try builder.references.append(builder.lists, addresses[index]);
        }
    }
    var edges = std.ArrayList(u32).empty;
    defer edges.deinit(allocator);

    for (set.artifacts, 0..) |artifact, index| {
        if (options.carried) |carried| if (!carried[index]) continue;
        var row = std.mem.zeroes(ArtifactRow);
        row.content = addresses[index];
        switch (artifact.kind) {
            .proc => |identity| {
                row.kind = 0;
                row.identity = identity.bytes;
            },
            .rc_helper => |helper| {
                row.kind = 1;
                row.helper_name = try builder.name(helper);
            },
            .boxy_thunk => |identity| {
                row.kind = 2;
                row.identity = identity.bytes;
            },
            .entrypoint => row.kind = 3,
            .message_pool_run => row.kind = 4,
            .branch_island => row.kind = 5,
        }
        row.code = try builder.bytes(&builder.code, artifact.code);
        row.entry = artifact.entry;
        if (artifact.frame) |frame| {
            row.frame = .{
                .prologue_size = frame.prologue_size,
                .stack_alloc = frame.stack_alloc,
                .frame_size = frame.frame_size,
                .callee_saved_mask = frame.callee_saved_mask,
                .epilogue_offset = frame.epilogue_offset,
                .uses_frame_pointer = @intFromBool(frame.uses_frame_pointer),
                .present = 1,
                .reserved = @splat(0),
            };
        }
        row.refs.start = @intCast(builder.refs.items.len);
        for (artifact.refs) |ref| {
            try builder.refs.append(builder.lists, .{
                .site = ref.site,
                .target = rows_of[ref.target],
                .delta = ref.delta,
                .veneer = ref.veneer orelse no_veneer,
                .form = @backingInt(ref.form),
                .reserved = @splat(0),
            });
        }
        row.refs.len = @intCast(builder.refs.items.len - row.refs.start);
        row.symbolic_refs.start = @intCast(builder.symbolic_refs.items.len);
        for (artifact.symbolic_refs) |ref| {
            var symbolic = std.mem.zeroes(SymbolicRefRow);
            symbolic.site = ref.site;
            symbolic.veneer = ref.veneer orelse no_veneer;
            symbolic.form = @backingInt(ref.form);
            try builder.symbolicTarget(ref.target, &symbolic.kind, &symbolic.identity, &symbolic.name);
            try builder.symbolic_refs.append(builder.lists, symbolic);
        }
        row.symbolic_refs.len = @intCast(builder.symbolic_refs.items.len - row.symbolic_refs.start);
        row.lines.start = @intCast(builder.lines.items.len);
        for (artifact.lines) |line| {
            try builder.lines.append(builder.lists, .{ .offset = line.offset, .file = line.loc.file, .line = line.loc.line, .column = line.loc.column });
        }
        row.lines.len = @intCast(builder.lines.items.len - row.lines.start);
        row.relocations.start = @intCast(builder.relocations.items.len);
        for (artifact.relocations) |relocation| {
            // A reference to carried program data names it by content.
            const scope: ProcArtifact.SymbolScope = if (names.renames(relocation.name)) .shared else relocation.scope;
            try builder.relocations.append(builder.lists, .{
                .name = try builder.name(names.of(relocation.name)),
                .offset = relocation.offset,
                .kind = switch (relocation.kind) {
                    .function => 0,
                    .data => 1,
                },
                .data_kind = switch (relocation.kind) {
                    .function => 0,
                    .data => |data_kind| @backingInt(data_kind),
                },
                .scope = @backingInt(scope),
                .reserved = 0,
            });
        }
        row.relocations.len = @intCast(builder.relocations.items.len - row.relocations.start);
        row.data.start = @intCast(builder.data_items.items.len);
        for (artifact.data) |item| {
            var data_row = std.mem.zeroes(DataItemRow);
            data_row.name = try builder.name(names.of(item.name));
            data_row.bytes = try builder.dataBytes(data_row.name, item.bytes);
            data_row.alignment = item.alignment;
            data_row.symbol_offset = item.symbol_offset;
            data_row.program_local_name = @intFromBool(item.program_local_name);
            data_row.relocations.start = @intCast(builder.data_relocations.items.len);
            for (item.relocations) |relocation| {
                var relocation_row = std.mem.zeroes(DataRelocationRow);
                relocation_row.addend = relocation.addend;
                // An external binding names the linking image's symbol.
                relocation_row.name = try builder.name(if (relocation.external) relocation.name else names.of(relocation.name));
                relocation_row.offset = relocation.offset;
                relocation_row.function = @intFromBool(relocation.function);
                relocation_row.external = @intFromBool(relocation.external);
                if (relocation.code_target) |target| switch (target) {
                    .local => |node| {
                        relocation_row.target = 1;
                        relocation_row.local = rows_of[node];
                    },
                    .symbolic => |symbol| {
                        relocation_row.target = 2;
                        try builder.symbolicTarget(symbol, &relocation_row.symbolic_kind, &relocation_row.identity, &relocation_row.symbolic_name);
                    },
                    // Native-image offsets have no persistent representation.
                    .image_offset => unreachable,
                };
                try builder.data_relocations.append(builder.lists, relocation_row);
            }
            data_row.relocations.len = @intCast(builder.data_relocations.items.len - data_row.relocations.start);
            try builder.data_items.append(builder.lists, data_row);
        }
        row.data.len = @intCast(builder.data_items.items.len - row.data.start);
        row.source_events.start = @intCast(builder.certificates.items.len);
        for (artifact.source_events) |certificate| {
            var certificate_row = std.mem.zeroes(CertificateRow);
            certificate_row.descriptor = certificate.descriptor;
            certificate_row.data_name = try builder.name(names.of(certificate.data_name));
            certificate_row.branch_regions.start = @intCast(builder.regions.items.len);
            for (certificate.branch_regions) |region| {
                try builder.regions.append(builder.lists, .{ .start = region.start.offset, .end = region.end.offset });
            }
            certificate_row.branch_regions.len = @intCast(builder.regions.items.len - certificate_row.branch_regions.start);
            if (certificate.site_origin) |origin| switch (origin) {
                .checked_expr => |id| {
                    certificate_row.site_kind = 1;
                    certificate_row.site_origin = id;
                },
                .checked_pattern => |id| {
                    certificate_row.site_kind = 2;
                    certificate_row.site_origin = id;
                },
            };
            try builder.certificates.append(builder.lists, certificate_row);
        }
        row.source_events.len = @intCast(builder.certificates.items.len - row.source_events.start);
        row.materializations.start = @intCast(builder.materializations.items.len);
        for (artifact.materializations) |dependency| {
            try builder.materializations.append(builder.lists, .{
                .binding_name = try builder.name(names.of(dependency.binding_name)),
                .header_name = try builder.name(names.of(dependency.header_name)),
                .materialization = materializationRow(dependency.materialization),
            });
        }
        row.materializations.len = @intCast(builder.materializations.items.len - row.materializations.start);
        if (artifact.root_materialization) |declaration| row.root_materialization = materializationRow(declaration);
        row.context_complete = @intFromBool(artifact.context_complete);
        if (artifact.context_contract) |contract| {
            row.has_contract = 1;
            row.target = @intCast(@backingInt(contract.target));
            row.cpu_level = @intCast(@backingInt(contract.cpu_level));
            row.hot_reload = @intFromBool(contract.hot_reload);
            row.default_platform_runtime = @intFromBool(contract.default_platform_runtime);
        }
        if (artifact.context_dependencies) |dependencies| {
            row.has_dependencies = 1;
            row.static_data = @intFromBool(dependencies.static_data);
            row.boxy_runtime_entry = @intFromBool(dependencies.boxy_runtime_entry);
            row.boxy_runtime = @intFromBool(dependencies.boxy_runtime);
            row.dict_seed = @intFromBool(dependencies.dict_seed);
        }
        if (artifact.callable_contract) |contract| {
            row.has_callable = 1;
            row.callable_contract = contract;
        }

        // The artifact's edges, resolved here once so a reader selects a
        // closure from the rows alone.
        edges.clearRetainingCapacity();
        var unresolved = false;
        for (artifact.refs) |ref| try edges.append(allocator, rows_of[ref.target]);
        for (graph.symbolicTargets(@intCast(index))) |target| {
            if (target) |node| try edges.append(allocator, rows_of[node]) else unresolved = true;
        }
        for (graph.dataTargets(@intCast(index))) |target| {
            if (target) |node| try edges.append(allocator, rows_of[node]) else unresolved = true;
        }
        for (artifact.data) |item| for (item.relocations) |relocation| {
            if (relocation.function and !relocation.external and relocation.code_target == null) unresolved = true;
        };
        std.mem.sort(u32, edges.items, {}, std.sort.asc(u32));
        row.edges.start = @intCast(builder.edges.items.len);
        for (edges.items, 0..) |edge, i| {
            if (i == 0 or edge != edges.items[i - 1]) try builder.edges.append(builder.lists, edge);
        }
        row.edges.len = @intCast(builder.edges.items.len - row.edges.start);
        row.unresolved = @intFromBool(unresolved);
        try builder.artifacts.append(builder.lists, row);
    }

    const columns = try builder.columns();
    // The content identity of this exact artifact graph, over the artifact
    // columns, so no reader hashes the artifacts again.
    var namespace = std.crypto.hash.Blake3.init(.{});
    namespace.update("roc.native.physical.pack.v2");
    inline for (.{ "code", "names", "data_bytes", "artifacts", "references", "refs", "symbolic_refs", "lines", "relocations", "data_items", "data_relocations", "certificates", "regions", "materializations", "edges" }) |field| {
        namespace.update(std.mem.sliceAsBytes(@field(columns, field)));
    }
    var physical_namespace: [32]u8 = undefined;
    namespace.final(&physical_namespace);
    return .{ .arena = arena, .columns = columns, .physical_namespace = physical_namespace, .addresses = addresses, .rows = rows_of };
}

/// A body that carries nothing: it names the artifacts at `addresses`, each
/// a reference row, for a pack of offers whose code other packs carry.
pub fn encodeReferences(allocator: Allocator, addresses: []const [32]u8) WriteError!Body {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    const owned = try a.dupe([32]u8, addresses);
    const rows_of = try a.alloc(u32, addresses.len);
    for (rows_of, 0..) |*row, index| row.* = @intCast(index);
    var namespace = std.crypto.hash.Blake3.init(.{});
    namespace.update("roc.native.physical.pack.v2");
    namespace.update(std.mem.sliceAsBytes(owned));
    var physical_namespace: [32]u8 = undefined;
    namespace.final(&physical_namespace);
    return .{ .arena = arena, .columns = .{ .references = owned }, .physical_namespace = physical_namespace, .addresses = owned, .rows = rows_of };
}

fn materializationRow(declaration: lir.Program.RootMaterialization) MaterializationRow {
    var row = std.mem.zeroes(MaterializationRow);
    row.module = declaration.module;
    row.representation_digest = declaration.representation_digest;
    row.layout_digest = declaration.layout_digest;
    row.wrapper_identity = declaration.wrapper_identity;
    row.abi = @backingInt(declaration.abi);
    row.present = 1;
    switch (declaration.producer) {
        .checked => |root| {
            row.producer = 0;
            row.checked = root;
        },
        .literal => |literal| {
            row.producer = 1;
            row.checked = literal.checked_expr;
            row.kind = @backingInt(literal.kind);
            row.conversion_identity = literal.conversion_identity;
        },
    }
    return row;
}

fn materializationFromRow(row: MaterializationRow) lir.Program.RootMaterialization {
    return .{
        .module = row.module,
        .producer = switch (row.producer) {
            0 => .{ .checked = row.checked },
            1 => .{ .literal = .{
                .checked_expr = row.checked,
                .kind = @fromBackingInt(@intCast(row.kind)),
                .conversion_identity = row.conversion_identity,
            } },
            else => unreachable,
        },
        .representation_digest = row.representation_digest,
        .layout_digest = row.layout_digest,
        .wrapper_identity = row.wrapper_identity,
        .abi = @fromBackingInt(@intCast(row.abi)),
    };
}

/// Write a pack from a gathered body with these offers, each naming its
/// artifact by its index in the body's set.
pub fn writeWithBody(
    allocator: Allocator,
    body: *const Body,
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry,
    materializer_reads: []const MaterializerRead,
    version_hash: [32]u8,
) WriteError!Bytes {
    if (builtin.mode == .debug) debugValidateOffers(allocator, body, specs, materializer_reads);
    var arena = collections.SingleThreadArena.init(allocator);
    defer arena.deinit();
    var offers = Builder.init(allocator, arena.allocator());
    defer offers.deinit();
    for (specs) |spec| {
        var row = std.mem.zeroes(SpecRow);
        row.key = spec.key;
        row.by_identity = @intFromBool(spec.by_identity);
        row.artifact = body.rows[spec.artifact];
        if (spec.platform_requirement_relation) |relation| {
            row.has_relation = 1;
            row.relation = relation;
        }
        if (spec.demand) |demand| {
            row.has_demand = 1;
            row.demand = try offers.blob(demand);
        }
        row.root_reads = try offers.rootReads(spec.comptime_root_reads);
        row.modules = try offers.moduleKeys(spec.modules);
        row.conditions.start = @intCast(offers.conditions.items.len);
        try offers.conditions.appendSlice(offers.lists, spec.rc_ret_conditions);
        row.conditions.len = @intCast(spec.rc_ret_conditions.len);
        row.rc_borrowed_params = spec.rc_borrowed_params;
        row.rc_ret_lenders = spec.rc_ret_lenders;
        row.rc_read_only_params = spec.rc_read_only_params;
        row.rc_ret_unique_fields = spec.rc_ret_unique_fields;
        row.rc_ret_borrowed = @intFromBool(spec.rc_ret_borrowed);
        row.rc_ret_unique = @intFromBool(spec.rc_ret_unique);
        try offers.specs.append(offers.lists, row);
    }
    for (discovery) |entry| {
        var row = std.mem.zeroes(DiscoveryRow);
        row.key = entry.key;
        if (entry.platform_requirement_relation) |relation| {
            row.has_relation = 1;
            row.relation = relation;
        }
        try offers.discovery.append(offers.lists, row);
    }
    for (materializer_reads) |entry| {
        try offers.materializer_reads.append(offers.lists, .{ .wrapper_identity = entry.wrapper_identity, .read = try offers.blob(entry.read) });
    }
    var columns = body.columns;
    if (builtin.os.tag != .freestanding and std.c.getenv("ROC_PACK_TRACE") != null) {
        std.debug.print("pack-columns artifacts={d} code={d} names={d} data_bytes={d} data_items={d} data_relocations={d} blobs={d} lines={d} relocations={d} refs={d} symbolic_refs={d} certificates={d} regions={d} materializations={d} edges={d}\n", .{
            columns.artifacts.len, columns.code.len, columns.names.len, columns.data_bytes.len, columns.data_items.len, columns.data_relocations.len, columns.blobs.len, columns.lines.len, columns.relocations.len, columns.refs.len, columns.symbolic_refs.len, columns.certificates.len, columns.regions.len, columns.materializations.len, columns.edges.len,
        });
    }
    const offer_columns = try offers.columns();
    columns.blobs = offer_columns.blobs;
    columns.specs = offer_columns.specs;
    columns.root_reads = offer_columns.root_reads;
    columns.conditions = offer_columns.conditions;
    columns.modules = offer_columns.modules;
    columns.discovery = offer_columns.discovery;
    columns.materializer_reads = offer_columns.materializer_reads;
    if (builtin.os.tag != .freestanding and std.c.getenv("ROC_PACK_TRACE") != null) {
        std.debug.print("pack-offer-columns specs={d} references={d} blobs={d} root_reads={d} conditions={d} discovery={d} materializer_reads={d}\n", .{
            columns.specs.len, columns.references.len, columns.blobs.len, columns.root_reads.len, columns.conditions.len, columns.discovery.len, columns.materializer_reads.len,
        });
    }

    var plan = CompactWriter.init();
    defer plan.deinit(allocator);
    const root = try plan.appendAlloc(allocator, Serialized);
    root.* = std.mem.zeroes(Serialized);
    try root.write(&columns, allocator, &plan);
    const data_size = plan.total_bytes;
    const total = root_offset + data_size;
    const image = try allocator.alignedAlloc(u8, alignment, total);
    errdefer allocator.free(image);
    @memset(image[0..root_offset], 0);
    const header: *Header = @ptrCast(image.ptr);
    header.* = .{
        .magic = magic.*,
        .reserved = @splat(0),
        .data_size = data_size,
        .version_hash = version_hash,
        .physical_namespace = body.physical_namespace,
    };
    var cursor: usize = root_offset;
    for (plan.iovecs.items) |part| {
        const bytes = part.iov_base[0..part.iov_len];
        if (part.canonical) |canonical| {
            var fixed = std.Io.Writer.fixed(image[cursor .. cursor + part.iov_len]);
            canonical.write(bytes, &fixed) catch unreachable;
        } else {
            @memcpy(image[cursor .. cursor + part.iov_len], bytes);
        }
        cursor += part.iov_len;
    }
    return image;
}

/// Gathers the columns of a pack.
const Builder = struct {
    /// Owns the growing lists.
    lists: Allocator,
    /// Owns the finished columns.
    arena: Allocator,
    name_table: std.StringHashMap(NameRef),
    /// The data column range first appended for each data item name.
    data_ranges: std.AutoHashMapUnmanaged(NameRef, Range) = .empty,
    /// The range first appended for each distinct run of root reads, and
    /// for each distinct blob, by content digest: many offers of one pack
    /// read the same roots and carry the same demand, so each distinct run
    /// is stored once and every offer names its range.
    read_ranges: std.AutoHashMapUnmanaged([32]u8, Range) = .empty,
    blob_ranges: std.AutoHashMapUnmanaged([32]u8, Range) = .empty,
    module_ranges: std.AutoHashMapUnmanaged([32]u8, Range) = .empty,
    code: std.ArrayList(u8) = .empty,
    names: std.ArrayList(u8) = .empty,
    data_bytes: std.ArrayList(u8) = .empty,
    blobs: std.ArrayList(u8) = .empty,
    artifacts: std.ArrayList(ArtifactRow) = .empty,
    references: std.ArrayList([32]u8) = .empty,
    refs: std.ArrayList(RefRow) = .empty,
    symbolic_refs: std.ArrayList(SymbolicRefRow) = .empty,
    lines: std.ArrayList(LineRow) = .empty,
    relocations: std.ArrayList(RelocationRow) = .empty,
    data_items: std.ArrayList(DataItemRow) = .empty,
    data_relocations: std.ArrayList(DataRelocationRow) = .empty,
    certificates: std.ArrayList(CertificateRow) = .empty,
    regions: std.ArrayList(RegionRow) = .empty,
    materializations: std.ArrayList(MaterializationDependencyRow) = .empty,
    edges: std.ArrayList(u32) = .empty,
    specs: std.ArrayList(SpecRow) = .empty,
    root_reads: std.ArrayList(RootReadRow) = .empty,
    conditions: std.ArrayList(u32) = .empty,
    modules: std.ArrayList([32]u8) = .empty,
    discovery: std.ArrayList(DiscoveryRow) = .empty,
    materializer_reads: std.ArrayList(MaterializerReadRow) = .empty,

    fn init(lists: Allocator, arena: Allocator) Builder {
        return .{ .lists = lists, .arena = arena, .name_table = std.StringHashMap(NameRef).init(lists) };
    }

    fn deinit(self: *Builder) void {
        self.name_table.deinit();
        self.data_ranges.deinit(self.lists);
        self.read_ranges.deinit(self.lists);
        self.blob_ranges.deinit(self.lists);
        self.module_ranges.deinit(self.lists);
        inline for (@typeInfo(Columns).@"struct".field_names) |field_name| {
            @field(self, field_name).deinit(self.lists);
        }
    }

    /// Intern a name in the name blob.
    fn name(self: *Builder, text: []const u8) Allocator.Error!NameRef {
        const entry = try self.name_table.getOrPut(text);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{ .offset = @intCast(self.names.items.len), .len = @intCast(text.len) };
            try self.names.appendSlice(self.lists, text);
        }
        return entry.value_ptr.*;
    }

    /// Append bytes to a byte column, returning their range.
    fn bytes(self: *Builder, column: *std.ArrayList(u8), data: []const u8) Allocator.Error!Range {
        const start: u32 = @intCast(column.items.len);
        try column.appendSlice(self.lists, data);
        return .{ .start = start, .len = @intCast(data.len) };
    }

    /// The data column range holding `data` for the item named `name`. Every
    /// artifact lists the items it carries, and many carry the same frozen
    /// constant, so the bytes of one name are appended once and shared by
    /// every row that names it with those bytes.
    fn dataBytes(self: *Builder, item_name: NameRef, data: []const u8) Allocator.Error!Range {
        const entry = try self.data_ranges.getOrPut(self.lists, item_name);
        if (entry.found_existing) {
            const existing = entry.value_ptr.*;
            if (std.mem.eql(u8, self.data_bytes.items[existing.start .. existing.start + existing.len], data)) return existing;
            return try self.bytes(&self.data_bytes, data);
        }
        entry.value_ptr.* = try self.bytes(&self.data_bytes, data);
        return entry.value_ptr.*;
    }

    /// Encode a value field-wise into the blob column, returning its range;
    /// a value already encoded returns the range it was first encoded at.
    fn blob(self: *Builder, value: anytype) Allocator.Error!Range {
        const start: u32 = @intCast(self.blobs.items.len);
        var writer = Writer{ .allocator = self.lists, .bytes = &self.blobs };
        try writer.context(value);
        const encoded = self.blobs.items[start..];
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(encoded, &digest, .{});
        const entry = try self.blob_ranges.getOrPut(self.lists, digest);
        if (entry.found_existing) {
            const existing = entry.value_ptr.*;
            if (std.mem.eql(u8, self.blobs.items[existing.start .. existing.start + existing.len], encoded)) {
                self.blobs.shrinkRetainingCapacity(start);
                return existing;
            }
            return .{ .start = start, .len = @intCast(encoded.len) };
        }
        entry.value_ptr.* = .{ .start = start, .len = @intCast(encoded.len) };
        return entry.value_ptr.*;
    }

    /// The modules column range holding `modules`, appended once per
    /// distinct run.
    fn moduleKeys(self: *Builder, modules: []const [32]u8) Allocator.Error!Range {
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(std.mem.sliceAsBytes(modules), &digest, .{});
        const entry = try self.module_ranges.getOrPut(self.lists, digest);
        if (entry.found_existing) {
            const existing = entry.value_ptr.*;
            if (std.mem.eql(u8, std.mem.sliceAsBytes(self.modules.items[existing.start .. existing.start + existing.len]), std.mem.sliceAsBytes(modules))) return existing;
            const start: u32 = @intCast(self.modules.items.len);
            try self.modules.appendSlice(self.lists, modules);
            return .{ .start = start, .len = @intCast(modules.len) };
        }
        const start: u32 = @intCast(self.modules.items.len);
        try self.modules.appendSlice(self.lists, modules);
        entry.value_ptr.* = .{ .start = start, .len = @intCast(modules.len) };
        return entry.value_ptr.*;
    }

    /// The root-read column range holding `reads`, appended once per
    /// distinct run.
    fn rootReads(self: *Builder, reads: []const lir.CheckedPipeline.ComptimeRootRead) Allocator.Error!Range {
        const rows_in = try self.lists.alloc(RootReadRow, reads.len);
        defer self.lists.free(rows_in);
        for (reads, rows_in) |root_read, *row| row.* = .{ .module = root_read.module, .root = root_read.root, .reserved = 0 };
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(std.mem.sliceAsBytes(rows_in), &digest, .{});
        const entry = try self.read_ranges.getOrPut(self.lists, digest);
        if (entry.found_existing) {
            const existing = entry.value_ptr.*;
            if (std.mem.eql(u8, std.mem.sliceAsBytes(self.root_reads.items[existing.start .. existing.start + existing.len]), std.mem.sliceAsBytes(rows_in))) return existing;
            const start: u32 = @intCast(self.root_reads.items.len);
            try self.root_reads.appendSlice(self.lists, rows_in);
            return .{ .start = start, .len = @intCast(rows_in.len) };
        }
        const start: u32 = @intCast(self.root_reads.items.len);
        try self.root_reads.appendSlice(self.lists, rows_in);
        entry.value_ptr.* = .{ .start = start, .len = @intCast(rows_in.len) };
        return entry.value_ptr.*;
    }

    fn symbolicTarget(self: *Builder, target: @FieldType(ProcArtifact.SymbolicReference, "target"), kind: *u8, identity: *[32]u8, name_ref: *NameRef) Allocator.Error!void {
        switch (target) {
            .proc => |proc| {
                kind.* = 0;
                identity.* = proc.bytes;
            },
            .rc_helper => |helper| {
                kind.* = 1;
                name_ref.* = try self.name(helper);
            },
            .boxy_thunk => |thunk| {
                kind.* = 2;
                identity.* = thunk.bytes;
            },
        }
    }

    /// The gathered columns, owned by the builder's arena.
    fn columns(self: *Builder) Allocator.Error!Columns {
        var result: Columns = .{};
        inline for (@typeInfo(Columns).@"struct".field_names, @typeInfo(Columns).@"struct".field_types) |field_name, field_type| {
            @field(result, field_name) = try self.arena.dupe(std.meta.Child(field_type), @field(self, field_name).items);
        }
        return result;
    }
};

/// The debug-only check that a producer's artifacts have the shape a pack
/// records: every reference inside the set, no native-image offsets, and
/// every source-event certificate consistent with its code.
fn debugValidateArtifacts(allocator: Allocator, set: *const ProcArtifact.Set) void {
    for (set.artifacts) |artifact| {
        ProcArtifact.validateRootMaterializations(allocator, artifact) catch |err| std.debug.panic("pack producer emitted an invalid root materialization: {}", .{err});
        if (artifact.root_materialization == null) {
            for (artifact.refs) |reference| {
                std.debug.assert(reference.target < set.artifacts.len);
                std.debug.assert(set.artifacts[reference.target].root_materialization == null);
            }
        }
        if (artifact.source_events.len != 0) {
            const contract = artifact.context_contract orelse std.debug.panic("pack producer emitted source events without a fragment contract", .{});
            ProcArtifact.validateSourceEventCertificates(allocator, artifact, contract.target.toCpuArch().endian()) catch |err| std.debug.panic("pack producer emitted an invalid source event certificate: {}", .{err});
        } else {
            for (artifact.relocations) |relocation| {
                std.debug.assert(!(relocation.kind == .function and std.mem.eql(u8, relocation.name, SourceEvents.symbol_name)));
            }
        }
        for (artifact.data) |item| for (item.relocations) |relocation| {
            if (relocation.code_target) |target| {
                std.debug.assert(relocation.function and target != .image_offset);
                if (target == .local) std.debug.assert(target.local < set.artifacts.len);
            }
        };
        ProcArtifact.validateCodePatches(artifact) catch |err| std.debug.panic("pack producer emitted invalid code patches: {}", .{err});
    }
}

fn debugValidateOffers(allocator: Allocator, body: *const Body, specs: []const SpecEntry, materializer_reads: []const MaterializerRead) void {
    for (specs) |spec| {
        std.debug.assert(spec.artifact < body.rows.len);
        const row = body.rows[spec.artifact];
        if (row < body.columns.artifacts.len) {
            const root = body.columns.artifacts[row];
            std.debug.assert(root.root_materialization.present == 0);
            // An identity offer names the procedure it offers.
            if (spec.by_identity) std.debug.assert(root.kind == 0 and std.mem.eql(u8, &root.identity, &spec.key));
        }
        // An identity offer's code reads no literal roots.
        if (spec.by_identity) std.debug.assert(spec.demand != null and spec.demand.?.literal_reads.len == 0);
        if (spec.demand) |demand| for (demand.literal_reads) |literal| {
            const valid = literal.recipe.valid(allocator) catch |err| std.debug.panic("{}", .{err});
            std.debug.assert(valid);
        };
    }
    for (materializer_reads) |entry| {
        const valid = entry.read.recipe.valid(allocator) catch |err| std.debug.panic("{}", .{err});
        std.debug.assert(valid);
    }
}

/// A pack image in view: its header, its columns in place, and the means to
/// read artifacts and offers out of them without decoding the rest.
pub const View = struct {
    header: *const Header,
    columns: Columns,

    /// View `bytes` as a pack written by the compiler whose entry-version
    /// hash is `version_hash`. The header, the columns' bounds and the row
    /// ranges are checked; nothing else is.
    pub fn init(bytes: []align(alignment.toByteUnits()) const u8, version_hash: [32]u8) ReadError!View {
        if (bytes.len < root_offset) return error.MalformedPack;
        const header: *const Header = @ptrCast(bytes.ptr);
        if (!std.mem.eql(u8, &header.magic, magic)) return error.MalformedPack;
        if (!std.mem.eql(u8, &header.version_hash, &version_hash)) return error.UnsupportedPackVersion;
        if (header.data_size < @sizeOf(Serialized) or bytes.len - root_offset < header.data_size) return error.MalformedPack;
        const data = bytes[root_offset .. root_offset + @as(usize, @intCast(header.data_size))];
        const root: *const Serialized = @ptrCast(@alignCast(data.ptr));
        collections.validateSerializedRelocations(Serialized, root, data.len) catch return error.MalformedPack;
        const view: View = .{ .header = header, .columns = root.view(@intFromPtr(data.ptr)) };
        if (!view.rangesInBounds()) return error.MalformedPack;
        return view;
    }

    /// Whether every range a row holds lies within its column, so a reader
    /// indexes rows without checking each; a truncated or foreign image
    /// fails here rather than out of bounds.
    fn rangesInBounds(self: *const View) bool {
        const c = &self.columns;
        for (c.artifacts) |row| {
            if (!inBounds(row.code, c.code.len) or !inBounds(row.refs, c.refs.len) or !inBounds(row.symbolic_refs, c.symbolic_refs.len) or
                !inBounds(row.lines, c.lines.len) or !inBounds(row.relocations, c.relocations.len) or !inBounds(row.data, c.data_items.len) or
                !inBounds(row.source_events, c.certificates.len) or !inBounds(row.materializations, c.materializations.len) or
                !inBounds(row.edges, c.edges.len) or !nameInBounds(row.helper_name, c.names.len)) return false;
        }
        const row_count = c.artifacts.len + c.references.len;
        for (c.refs) |row| if (row.target >= row_count) return false;
        for (c.edges) |edge| if (edge >= row_count) return false;
        for (c.symbolic_refs) |row| if (!nameInBounds(row.name, c.names.len)) return false;
        for (c.relocations) |row| if (!nameInBounds(row.name, c.names.len)) return false;
        for (c.data_items) |row| if (!nameInBounds(row.name, c.names.len) or !inBounds(row.bytes, c.data_bytes.len) or !inBounds(row.relocations, c.data_relocations.len)) return false;
        for (c.data_relocations) |row| if (!nameInBounds(row.name, c.names.len) or !nameInBounds(row.symbolic_name, c.names.len) or (row.target == 1 and row.local >= row_count)) return false;
        for (c.certificates) |row| if (!nameInBounds(row.data_name, c.names.len) or !inBounds(row.branch_regions, c.regions.len)) return false;
        for (c.materializations) |row| if (!nameInBounds(row.binding_name, c.names.len) or !nameInBounds(row.header_name, c.names.len)) return false;
        for (c.specs) |row| if (row.artifact >= row_count or !inBounds(row.demand, c.blobs.len) or !inBounds(row.root_reads, c.root_reads.len) or !inBounds(row.conditions, c.conditions.len) or !inBounds(row.modules, c.modules.len)) return false;
        for (c.materializer_reads) |row| if (!inBounds(row.read, c.blobs.len)) return false;
        return true;
    }

    fn inBounds(range: Range, len: usize) bool {
        return @as(u64, range.start) + range.len <= len;
    }

    fn nameInBounds(ref: NameRef, len: usize) bool {
        return @as(u64, ref.offset) + ref.len <= len;
    }

    /// Content identity of the artifact graph.
    pub fn physicalNamespace(self: *const View) [32]u8 {
        return self.header.physical_namespace;
    }

    /// Content identity of the whole pack: its graph's identity, which the
    /// writer computed over the artifact columns, and the offer columns. Two
    /// packs with this digest are one pack, so a store names a pack by it
    /// without hashing the body again.
    pub fn contentDigest(self: *const View) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update("roc.native.pack.content.v1");
        hasher.update(&self.header.physical_namespace);
        const c = &self.columns;
        inline for (.{ "specs", "blobs", "root_reads", "conditions", "discovery", "materializer_reads" }) |field| {
            hasher.update(std.mem.sliceAsBytes(@field(c, field)));
        }
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return digest;
    }

    /// How many artifacts the pack holds.
    pub fn artifactCount(self: *const View) usize {
        return self.columns.artifacts.len;
    }

    /// How many offers the pack makes.
    pub fn specCount(self: *const View) usize {
        return self.columns.specs.len;
    }

    /// The key offer `index` serves: a specialization key, or the offered
    /// procedure's identity when `specByIdentity`.
    pub fn specKey(self: *const View, index: usize) [32]u8 {
        return self.columns.specs[index].key;
    }

    /// Whether offer `index` is an identity offer (`SpecEntry.by_identity`).
    pub fn specByIdentity(self: *const View, index: usize) bool {
        return self.columns.specs[index].by_identity != 0;
    }

    /// The artifact offer `index` names.
    pub fn specArtifact(self: *const View, index: usize) u32 {
        return self.columns.specs[index].artifact;
    }

    /// The platform/app relation offer `index` depends on, if any.
    pub fn specRelation(self: *const View, index: usize) ?[32]u8 {
        const row = self.columns.specs[index];
        return if (row.has_relation != 0) row.relation else null;
    }

    /// The checked modules offer `index` names (`SpecEntry.modules`).
    pub fn specModules(self: *const View, index: usize) []const [32]u8 {
        return rows([32]u8, self.columns.modules, self.columns.specs[index].modules);
    }

    /// The artifacts artifact `index` reaches directly.
    pub fn edges(self: *const View, index: usize) []const u32 {
        return rows(u32, self.columns.edges, self.columns.artifacts[index].edges);
    }

    /// Whether a reference of artifact `index` resolves to nothing in the pack.
    pub fn unresolved(self: *const View, index: usize) bool {
        return self.columns.artifacts[index].unresolved != 0;
    }

    /// How many rows the pack has: its artifacts, then its references.
    pub fn rowCount(self: *const View) usize {
        return self.columns.artifacts.len + self.columns.references.len;
    }

    /// The content address of row `index`: a carried artifact's or a
    /// reference's.
    pub fn contentAddress(self: *const View, index: usize) [32]u8 {
        const artifacts = self.columns.artifacts;
        return if (index < artifacts.len) artifacts[index].content else self.columns.references[index - artifacts.len];
    }

    /// Whether row `index` is an artifact the pack carries, rather than a
    /// reference to one another pack carries.
    pub fn carried(self: *const View, index: usize) bool {
        return index < self.columns.artifacts.len;
    }

    fn rows(comptime T: type, column: []const T, range: Range) []const T {
        return column[range.start .. range.start + range.len];
    }

    fn name(self: *const View, ref: NameRef) []const u8 {
        return self.columns.names[ref.offset .. ref.offset + ref.len];
    }

    fn symbolicTarget(self: *const View, kind: u8, identity: [32]u8, name_ref: NameRef) @FieldType(ProcArtifact.SymbolicReference, "target") {
        return switch (kind) {
            0 => .{ .proc = .{ .bytes = identity } },
            1 => .{ .rc_helper = self.name(name_ref) },
            2 => .{ .boxy_thunk = .{ .bytes = identity } },
            else => unreachable,
        };
    }

    /// Artifact `index`, which the pack carries, as the backend consumes it.
    /// Its code, data, names and descriptors are the image's bytes; the row
    /// arrays that describe it are built in `arena`.
    pub fn artifact(self: *const View, index: usize, arena: Allocator) Allocator.Error!ProcArtifact.Artifact {
        const c = &self.columns;
        const row = c.artifacts[index];
        const refs = try arena.alloc(ProcArtifact.Reference, row.refs.len);
        for (rows(RefRow, c.refs, row.refs), refs) |ref_row, *ref| {
            ref.* = .{
                .site = ref_row.site,
                .form = @fromBackingInt(@intCast(ref_row.form)),
                .target = ref_row.target,
                .delta = ref_row.delta,
                .veneer = if (ref_row.veneer == no_veneer) null else ref_row.veneer,
            };
        }
        const symbolic_refs = try arena.alloc(ProcArtifact.SymbolicReference, row.symbolic_refs.len);
        for (rows(SymbolicRefRow, c.symbolic_refs, row.symbolic_refs), symbolic_refs) |ref_row, *ref| {
            ref.* = .{
                .site = ref_row.site,
                .form = @fromBackingInt(@intCast(ref_row.form)),
                .veneer = if (ref_row.veneer == no_veneer) null else ref_row.veneer,
                .target = self.symbolicTarget(ref_row.kind, ref_row.identity, ref_row.name),
            };
        }
        const lines = try arena.alloc(ProcArtifact.LineEntry, row.lines.len);
        for (rows(LineRow, c.lines, row.lines), lines) |line_row, *line| {
            line.* = .{ .offset = line_row.offset, .loc = .{ .file = line_row.file, .line = line_row.line, .column = line_row.column } };
        }
        const relocations = try arena.alloc(ProcArtifact.NamedRelocation, row.relocations.len);
        for (rows(RelocationRow, c.relocations, row.relocations), relocations) |relocation_row, *relocation| {
            relocation.* = .{
                .offset = relocation_row.offset,
                .name = self.name(relocation_row.name),
                .scope = @fromBackingInt(@intCast(relocation_row.scope)),
                .kind = switch (relocation_row.kind) {
                    0 => .function,
                    1 => .{ .data = @fromBackingInt(@intCast(relocation_row.data_kind)) },
                    else => unreachable,
                },
            };
        }
        const data = try arena.alloc(ProcArtifact.DataItem, row.data.len);
        for (rows(DataItemRow, c.data_items, row.data), data) |item_row, *item| {
            const item_relocations = try arena.alloc(ProcArtifact.DataRelocation, item_row.relocations.len);
            for (rows(DataRelocationRow, c.data_relocations, item_row.relocations), item_relocations) |relocation_row, *relocation| {
                relocation.* = .{
                    .offset = relocation_row.offset,
                    .name = self.name(relocation_row.name),
                    .addend = relocation_row.addend,
                    .function = relocation_row.function != 0,
                    .external = relocation_row.external != 0,
                    .code_target = switch (relocation_row.target) {
                        0 => null,
                        1 => .{ .local = relocation_row.local },
                        2 => .{ .symbolic = self.symbolicTarget(relocation_row.symbolic_kind, relocation_row.identity, relocation_row.symbolic_name) },
                        else => unreachable,
                    },
                };
            }
            item.* = .{
                .name = self.name(item_row.name),
                .bytes = c.data_bytes[item_row.bytes.start .. item_row.bytes.start + item_row.bytes.len],
                .alignment = item_row.alignment,
                .symbol_offset = item_row.symbol_offset,
                .relocations = item_relocations,
                .program_local_name = item_row.program_local_name != 0,
            };
        }
        const source_events = try arena.alloc(ProcArtifact.SourceEventCertificate, row.source_events.len);
        for (rows(CertificateRow, c.certificates, row.source_events), source_events) |certificate_row, *certificate| {
            const regions = try arena.alloc(base.Region, certificate_row.branch_regions.len);
            for (rows(RegionRow, c.regions, certificate_row.branch_regions), regions) |region_row, *region| {
                region.* = base.Region.from_raw_offsets(region_row.start, region_row.end);
            }
            certificate.* = .{
                .data_name = self.name(certificate_row.data_name),
                .descriptor = certificate_row.descriptor,
                .branch_regions = regions,
                .site_origin = switch (certificate_row.site_kind) {
                    0 => null,
                    1 => .{ .checked_expr = certificate_row.site_origin },
                    2 => .{ .checked_pattern = certificate_row.site_origin },
                    else => unreachable,
                },
            };
        }
        const materializations = try arena.alloc(ProcArtifact.RootMaterializationDependency, row.materializations.len);
        for (rows(MaterializationDependencyRow, c.materializations, row.materializations), materializations) |dependency_row, *dependency| {
            dependency.* = .{
                .binding_name = self.name(dependency_row.binding_name),
                .header_name = self.name(dependency_row.header_name),
                .materialization = materializationFromRow(dependency_row.materialization),
            };
        }
        return .{
            .kind = switch (row.kind) {
                0 => .{ .proc = .{ .bytes = row.identity } },
                1 => .{ .rc_helper = self.name(row.helper_name) },
                2 => .{ .boxy_thunk = .{ .bytes = row.identity } },
                3 => .entrypoint,
                4 => .message_pool_run,
                5 => .branch_island,
                else => unreachable,
            },
            .root_materialization = if (row.root_materialization.present != 0) materializationFromRow(row.root_materialization) else null,
            .materializations = materializations,
            .code = c.code[row.code.start .. row.code.start + row.code.len],
            .entry = row.entry,
            .frame = if (row.frame.present != 0) .{
                .prologue_size = row.frame.prologue_size,
                .stack_alloc = row.frame.stack_alloc,
                .frame_size = row.frame.frame_size,
                .callee_saved_mask = row.frame.callee_saved_mask,
                .epilogue_offset = row.frame.epilogue_offset,
                .uses_frame_pointer = row.frame.uses_frame_pointer != 0,
            } else null,
            .refs = refs,
            .symbolic_refs = symbolic_refs,
            .lines = lines,
            .relocations = relocations,
            .data = data,
            .source_events = source_events,
            .context_contract = if (row.has_contract != 0) .{
                .target = @fromBackingInt(@intCast(row.target)),
                .cpu_level = @fromBackingInt(@intCast(row.cpu_level)),
                .hot_reload = row.hot_reload != 0,
                .default_platform_runtime = row.default_platform_runtime != 0,
            } else null,
            .context_dependencies = if (row.has_dependencies != 0) .{
                .static_data = row.static_data != 0,
                .boxy_runtime_entry = row.boxy_runtime_entry != 0,
                .boxy_runtime = row.boxy_runtime != 0,
                .dict_seed = row.dict_seed != 0,
            } else null,
            .context_complete = row.context_complete != 0,
            .callable_contract = if (row.has_callable != 0) row.callable_contract else null,
            .content_address = row.content,
        };
    }

    /// Offer `index`, with its demand decoded from its blob into `arena`.
    pub fn spec(self: *const View, index: usize, arena: Allocator) ReadError!SpecEntry {
        const c = &self.columns;
        const row = c.specs[index];
        const root_reads = try arena.alloc(lir.CheckedPipeline.ComptimeRootRead, row.root_reads.len);
        for (rows(RootReadRow, c.root_reads, row.root_reads), root_reads) |read_row, *out| out.* = .{ .module = read_row.module, .root = read_row.root };
        return .{
            .key = row.key,
            .by_identity = row.by_identity != 0,
            .artifact = row.artifact,
            .platform_requirement_relation = if (row.has_relation != 0) row.relation else null,
            .demand = if (row.has_demand != 0) try self.decode(lir.Program.CachedCtfeDemand, row.demand, arena) else null,
            .comptime_root_reads = root_reads,
            .rc_borrowed_params = row.rc_borrowed_params,
            .rc_ret_borrowed = row.rc_ret_borrowed != 0,
            .rc_ret_lenders = row.rc_ret_lenders,
            .rc_read_only_params = row.rc_read_only_params,
            .rc_ret_unique = row.rc_ret_unique != 0,
            .rc_ret_unique_fields = row.rc_ret_unique_fields,
            .rc_ret_conditions = rows(u32, c.conditions, row.conditions),
            .modules = rows([32]u8, c.modules, row.modules),
        };
    }

    /// The pack's settled discovery outcomes.
    pub fn discovery(self: *const View, arena: Allocator) Allocator.Error![]const DiscoveryEntry {
        const entries = try arena.alloc(DiscoveryEntry, self.columns.discovery.len);
        for (self.columns.discovery, entries) |row, *entry| {
            entry.* = .{ .key = row.key, .platform_requirement_relation = if (row.has_relation != 0) row.relation else null };
        }
        return entries;
    }

    /// The literal reads behind the pack's materializers, decoded into `arena`.
    pub fn materializerReads(self: *const View, arena: Allocator) ReadError![]const MaterializerRead {
        const entries = try arena.alloc(MaterializerRead, self.columns.materializer_reads.len);
        for (self.columns.materializer_reads, entries) |row, *entry| {
            entry.* = .{ .wrapper_identity = row.wrapper_identity, .read = try self.decode(lir.Program.CachedLiteralRead, row.read, arena) };
        }
        return entries;
    }

    fn decode(self: *const View, comptime T: type, range: Range, arena: Allocator) ReadError!T {
        var reader = Reader{ .bytes = self.columns.blobs[range.start .. range.start + range.len] };
        const value = try reader.context(T, arena);
        if (reader.offset != reader.bytes.len) return error.MalformedPack;
        return value;
    }

    /// Every artifact a set of roots reaches within the pack, ascending, or
    /// null when one of them has a reference that resolves to nothing in the
    /// pack: such a closure cannot be served. An artifact the pack only
    /// names is a leaf here; the reader continues in the pack carrying it.
    pub fn closure(self: *const View, allocator: Allocator, roots: []const u32) Allocator.Error!?[]u32 {
        const seen = try allocator.alloc(bool, self.rowCount());
        defer allocator.free(seen);
        @memset(seen, false);
        var stack = std.ArrayList(u32).empty;
        defer stack.deinit(allocator);
        var members = std.ArrayList(u32).empty;
        errdefer members.deinit(allocator);
        for (roots) |root| {
            if (seen[root]) continue;
            seen[root] = true;
            try stack.append(allocator, root);
        }
        while (stack.pop()) |node| {
            if (!self.carried(node)) {
                try members.append(allocator, node);
                continue;
            }
            if (self.unresolved(node)) {
                members.deinit(allocator);
                return null;
            }
            try members.append(allocator, node);
            for (self.edges(node)) |edge| {
                if (seen[edge]) continue;
                seen[edge] = true;
                try stack.append(allocator, edge);
            }
        }
        std.mem.sort(u32, members.items, {}, std.sort.asc(u32));
        return try members.toOwnedSlice(allocator);
    }

    /// Every artifact and offer of the pack, decoded into one pack whose
    /// arena is `arena`. Byte strings alias the viewed image.
    fn readAll(self: *const View, arena: *collections.SingleThreadArena) ReadError!Pack {
        const a = arena.allocator();
        const count = self.columns.artifacts.len;
        // A whole decode has no other pack to continue in, so a pack that
        // only names some of its artifacts is one it cannot place.
        if (self.columns.references.len != 0) return error.MalformedPack;
        const artifacts = try a.alloc(ProcArtifact.Artifact, count);
        const edges_out = try a.alloc([]const u32, count);
        const unresolved_flags = try a.alloc(bool, count);
        const addresses = try a.alloc([32]u8, count);
        for (artifacts, edges_out, unresolved_flags, addresses, 0..) |*out, *artifact_edges, *flag, *address, index| {
            out.* = try self.artifact(index, a);
            artifact_edges.* = try a.dupe(u32, self.edges(index));
            flag.* = self.unresolved(index);
            address.* = self.contentAddress(index);
        }
        const specs = try a.alloc(SpecEntry, self.columns.specs.len);
        for (specs, 0..) |*entry, index| entry.* = try self.spec(index, a);
        const discovery_entries = try self.discovery(a);
        const materializer_reads = try self.materializerReads(a);
        return .{
            .set = .{ .arena = arena.*, .artifacts = artifacts, .physical_namespace = self.header.physical_namespace, .addresses = addresses },
            .specs = specs,
            .discovery = discovery_entries,
            .materializer_reads = materializer_reads,
            .edges = edges_out,
            .unresolved = unresolved_flags,
        };
    }
};

/// Decode a pack whole into a pack that owns everything it holds, including
/// a copy of the image its byte strings point into.
pub fn read(allocator: Allocator, bytes: []align(alignment.toByteUnits()) const u8, version_hash: [32]u8) ReadError!Pack {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const owned = try arena.allocator().alignedAlloc(u8, alignment, bytes.len);
    @memcpy(owned, bytes);
    const view = try View.init(owned, version_hash);
    return view.readAll(&arena);
}

test "pack image round-trips every artifact field and offer" {
    const allocator = std.testing.allocator;
    const version: [32]u8 = @splat(9);
    const leaf = ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = @as([32]u8, @splat(1)),
        .context_contract = .{ .target = .x64musl, .cpu_level = .default, .hot_reload = false, .default_platform_runtime = false },
        .context_dependencies = .{ .static_data = true },
    };
    var caller = leaf;
    caller.kind = .{ .rc_helper = "roc__h_test_helper" };
    // call; mov rax, imm64; call; call; ret
    caller.code = "\xe8\x00\x00\x00\x00" ++ "\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00" ++ "\xe8\x00\x00\x00\x00" ++ "\xe8\x00\x00\x00\x00" ++ "\xc3";
    caller.frame = .{ .prologue_size = 4, .stack_alloc = 16, .frame_size = 32, .callee_saved_mask = 3, .epilogue_offset = 5, .uses_frame_pointer = true };
    caller.refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0, .veneer = null }};
    caller.symbolic_refs = &.{.{ .site = 20, .form = .call, .veneer = null, .target = .{ .proc = lir.ProcIdentity.forTest(1) } }};
    caller.lines = &.{.{ .offset = 0, .loc = .{ .file = 1, .line = 2, .column = 3 } }};
    caller.relocations = &.{ .{ .offset = 16, .name = "roc__psomething", .scope = .shared, .kind = .function }, .{ .offset = 7, .name = "roc__d7", .scope = .program, .kind = .{ .data = .abs64 } } };
    var relocation = [_]ProcArtifact.DataRelocation{.{ .offset = 0, .name = "roc__h_test_helper", .addend = -8, .function = true, .code_target = .{ .local = 0 } }};
    caller.data = &.{.{ .name = "roc__hdata", .bytes = "\x00\x00\x00\x00\x00\x00\x00\x00", .alignment = 8, .symbol_offset = 0, .relocations = &relocation }};
    var artifacts = [_]ProcArtifact.Artifact{ leaf, caller };
    const set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    const specs = [_]SpecEntry{.{
        .key = @splat(2),
        .artifact = 1,
        .platform_requirement_relation = @splat(3),
        .demand = .{ .complete = true },
        .comptime_root_reads = &.{.{ .module = @splat(4), .root = 7 }},
        .rc_borrowed_params = 1,
        .rc_ret_borrowed = true,
        .rc_ret_lenders = 2,
        .rc_read_only_params = 3,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 4,
        .rc_ret_conditions = &.{ 0x0001_02ff, 7 },
        .modules = &.{ @splat(6), @splat(7) },
    }};
    const discovery = [_]DiscoveryEntry{.{ .key = @splat(5), .platform_requirement_relation = null }};
    const bytes = try writeOffers(allocator, &set, &specs, &discovery, &.{}, version);
    defer allocator.free(bytes);

    var decoded = try read(allocator, bytes, version);
    defer decoded.deinit();
    // A decoded artifact knows its content address; the producer's did not.
    var expected_leaf = leaf;
    expected_leaf.content_address = decoded.set.addresses.?[0];
    var expected_caller = caller;
    expected_caller.content_address = decoded.set.addresses.?[1];
    try std.testing.expectEqualDeep(expected_leaf, decoded.set.artifacts[0]);
    try std.testing.expectEqualDeep(expected_caller, decoded.set.artifacts[1]);
    try std.testing.expect(!std.mem.eql(u8, &decoded.set.addresses.?[0], &decoded.set.addresses.?[1]));
    try std.testing.expectEqualDeep(specs[0], decoded.specs[0]);
    try std.testing.expectEqualDeep(discovery[0], decoded.discovery[0]);
    try std.testing.expectEqualSlices(u32, &.{0}, decoded.edges[1]);
    try std.testing.expectEqualSlices(bool, &.{ false, false }, decoded.unresolved);

    // Another compiler's pack is not admitted; a truncated image is malformed.
    try std.testing.expectError(error.UnsupportedPackVersion, View.init(bytes, @splat(10)));
    try std.testing.expectError(error.MalformedPack, View.init(bytes[0 .. bytes.len - 16], version));

    // The same set written twice is the same bytes.
    const again = try writeOffers(allocator, &set, &specs, &discovery, &.{}, version);
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, bytes, again);
}

test "pack image offers a procedure by identity" {
    const allocator = std.testing.allocator;
    const version: [32]u8 = @splat(9);
    const identity = lir.ProcIdentity.forTest(6);
    var artifacts = [_]ProcArtifact.Artifact{.{
        .kind = .{ .proc = identity },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = @as([32]u8, @splat(1)),
    }};
    const set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    const specs = [_]SpecEntry{.{
        .key = identity.bytes,
        .by_identity = true,
        .artifact = 0,
        .demand = .{ .complete = true },
        .comptime_root_reads = &.{.{ .module = @splat(4), .root = 2 }},
        .rc_borrowed_params = 1,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 1,
        .rc_ret_unique = true,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    }};
    const bytes = try writeOffers(allocator, &set, &specs, &.{}, &.{}, version);
    defer allocator.free(bytes);
    const view = try View.init(bytes, version);
    try std.testing.expect(view.specByIdentity(0));
    try std.testing.expectEqualSlices(u8, &identity.bytes, &view.specKey(0));
    var decoded = try read(allocator, bytes, version);
    defer decoded.deinit();
    try std.testing.expectEqualDeep(specs[0], decoded.specs[0]);
}

test "pack image selects closures and reads their artifacts alone" {
    const allocator = std.testing.allocator;
    const version: [32]u8 = @splat(9);
    const leaf = ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = @as([32]u8, @splat(1)),
        .context_contract = .{ .target = .x64musl, .cpu_level = .default, .hot_reload = false, .default_platform_runtime = false },
        .context_dependencies = .{},
    };
    var caller = leaf;
    caller.kind = .{ .proc = lir.ProcIdentity.forTest(2) };
    caller.code = "\xe8\x00\x00\x00\x00\xc3";
    caller.refs = &.{.{ .site = 1, .form = .call, .target = 0, .delta = 0, .veneer = null }};
    var symbolic = leaf;
    symbolic.kind = .{ .proc = lir.ProcIdentity.forTest(3) };
    symbolic.code = "\xe8\x00\x00\x00\x00\xc3";
    symbolic.symbolic_refs = &.{.{ .site = 1, .form = .call, .veneer = null, .target = .{ .proc = lir.ProcIdentity.forTest(1) } }};
    var dangling = leaf;
    dangling.kind = .{ .proc = lir.ProcIdentity.forTest(4) };
    dangling.code = "\xe8\x00\x00\x00\x00\xc3";
    dangling.symbolic_refs = &.{.{ .site = 1, .form = .call, .veneer = null, .target = .{ .proc = lir.ProcIdentity.forTest(99) } }};
    var artifacts = [_]ProcArtifact.Artifact{ leaf, caller, symbolic, dangling };
    const set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    const bytes = try write(allocator, &set, &.{}, version);
    defer allocator.free(bytes);

    const view = try View.init(bytes, version);
    try std.testing.expectEqual(@as(usize, 4), view.artifactCount());
    try std.testing.expectEqualSlices(u32, &.{}, view.edges(0));
    try std.testing.expectEqualSlices(u32, &.{0}, view.edges(1));
    try std.testing.expectEqualSlices(u32, &.{0}, view.edges(2));
    try std.testing.expect(!view.unresolved(2));
    try std.testing.expect(view.unresolved(3));

    const of_caller = (try view.closure(allocator, &.{1})) orelse return error.TestUnexpectedResult;
    defer allocator.free(of_caller);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1 }, of_caller);
    const of_both = (try view.closure(allocator, &.{ 2, 1 })) orelse return error.TestUnexpectedResult;
    defer allocator.free(of_both);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2 }, of_both);
    try std.testing.expect((try view.closure(allocator, &.{3})) == null);

    var arena = collections.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const decoded = try view.artifact(1, arena.allocator());
    var expected = caller;
    expected.content_address = view.contentAddress(1);
    try std.testing.expectEqualDeep(expected, decoded);
    // The decoded artifact's code is the image's bytes.
    try std.testing.expect(@intFromPtr(decoded.code.ptr) >= @intFromPtr(bytes.ptr) and @intFromPtr(decoded.code.ptr) < @intFromPtr(bytes.ptr) + bytes.len);
}

test "pack bodies are shared by packs with different offers" {
    const allocator = std.testing.allocator;
    const version: [32]u8 = @splat(9);
    const leaf = ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = @as([32]u8, @splat(1)),
    };
    var artifacts = [_]ProcArtifact.Artifact{leaf};
    const set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    var body = try encodeBody(allocator, &set, .{});
    defer body.deinit();
    const spec = SpecEntry{ .key = @splat(2), .artifact = 0, .demand = .{ .complete = true }, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
    const first = try writeWithBody(allocator, &body, &.{spec}, &.{}, &.{}, version);
    defer allocator.free(first);
    const second = try writeWithBody(allocator, &body, &.{}, &.{}, &.{}, version);
    defer allocator.free(second);
    const first_view = try View.init(first, version);
    const second_view = try View.init(second, version);
    try std.testing.expectEqual(@as(usize, 1), first_view.specCount());
    try std.testing.expectEqual(@as(usize, 0), second_view.specCount());
    try std.testing.expectEqualSlices(u8, &first_view.physicalNamespace(), &second_view.physicalNamespace());
    const whole = try write(allocator, &set, &.{spec}, version);
    defer allocator.free(whole);
    try std.testing.expectEqualSlices(u8, whole, first);
}

/// Field-wise encoder for the offer metadata whose shapes nest (storage
/// demand, literal recipes). Every declared field is written, never padding
/// or process memory.
const Writer = struct {
    allocator: Allocator,
    bytes: *std.ArrayList(u8),

    // Typed metadata is serialized fieldwise, never including padding or host pointers.
    fn context(self: *Writer, value: anytype) Allocator.Error!void {
        const T = @TypeOf(value);
        switch (@typeInfo(T)) {
            .void => {},
            .bool => try self.byte(@intFromBool(value)),
            .int => |info| {
                if (info.bits == 8) try self.byte(value) else if (info.bits == 32) try self.word(value) else @compileError("unsupported context integer");
            },
            // An enum keeps its full backing width: dense ids are enums too.
            .@"enum" => |info| switch (@typeInfo(info.tag_type).int.bits) {
                1...8 => try self.byte(@intCast(@backingInt(value))),
                9...32 => try self.word(@intCast(@backingInt(value))),
                else => @compileError("unsupported context enum width"),
            },
            .optional => {
                try self.byte(@intFromBool(value != null));
                if (value) |present| try self.context(present);
            },
            .array => for (value) |item| try self.context(item),
            .pointer => |info| {
                if (info.size != .slice) @compileError("context pointers must be slices");
                try self.word(@intCast(value.len));
                for (value) |item| try self.context(item);
            },
            .@"struct" => |info| inline for (info.field_names) |field_name| try self.context(@field(value, field_name)),
            .@"union" => |info| {
                const tag = std.meta.activeTag(value);
                try self.context(tag);
                inline for (info.field_names) |field_name| {
                    if (tag == @field(info.tag_type.?, field_name)) try self.context(@field(value, field_name));
                }
            },
            .type,
            .noreturn,
            .float,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .spirv,
            => @compileError("unsupported context metadata"),
        }
    }

    fn raw(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, data);
    }

    fn byte(self: *Writer, value: u8) Allocator.Error!void {
        try self.bytes.append(self.allocator, value);
    }

    fn word(self: *Writer, value: u32) Allocator.Error!void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        try self.raw(&buffer);
    }

    fn wide(self: *Writer, value: u64) Allocator.Error!void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        try self.raw(&buffer);
    }
};

/// Field-wise decoder for what `Writer` encodes.
const Reader = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn context(self: *Reader, comptime T: type, allocator: Allocator) ReadError!T {
        switch (@typeInfo(T)) {
            .void => return {},
            .bool => return switch (try self.byte()) {
                0 => false,
                1 => true,
                else => error.MalformedPack,
            },
            .int => |info| {
                if (info.bits == 8) return try self.byte();
                if (info.bits == 32) return try self.word();
                @compileError("unsupported context integer");
            },
            .@"enum" => |info| {
                const backing: u32 = switch (@typeInfo(info.tag_type).int.bits) {
                    1...8 => try self.byte(),
                    9...32 => try self.word(),
                    else => @compileError("unsupported context enum width"),
                };
                return std.enums.fromInt(T, backing) orelse error.MalformedPack;
            },
            .optional => |info| return switch (try self.byte()) {
                0 => null,
                1 => try self.context(info.child, allocator),
                else => error.MalformedPack,
            },
            .array => |info| {
                var result: T = undefined;
                for (&result) |*item| item.* = try self.context(info.child, allocator);
                return result;
            },
            .pointer => |info| {
                if (info.size != .slice) @compileError("context pointers must be slices");
                const len = try self.word();
                // Every supported element consumes at least one byte.
                if (len > self.bytes.len - self.offset) return error.MalformedPack;
                const items = try allocator.alloc(info.child, len);
                for (items) |*item| item.* = try self.context(info.child, allocator);
                return items;
            },
            .@"struct" => |info| {
                var result: T = undefined;
                inline for (info.field_names, info.field_types) |field_name, field_type| @field(result, field_name) = try self.context(field_type, allocator);
                return result;
            },
            .@"union" => |info| {
                const tag = try self.context(info.tag_type.?, allocator);
                inline for (info.field_names, info.field_types) |field_name, field_type| {
                    if (tag == @field(info.tag_type.?, field_name)) return @unionInit(T, field_name, try self.context(field_type, allocator));
                }
                return error.MalformedPack;
            },
            .type,
            .noreturn,
            .float,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .spirv,
            => @compileError("unsupported context metadata"),
        }
    }

    fn raw(self: *Reader, len: usize) ReadError![]const u8 {
        if (self.bytes.len - self.offset < len) return error.MalformedPack;
        const slice = self.bytes[self.offset..][0..len];
        self.offset += len;
        return slice;
    }

    fn byte(self: *Reader) ReadError!u8 {
        return (try self.raw(1))[0];
    }

    fn word(self: *Reader) ReadError!u32 {
        return std.mem.readInt(u32, (try self.raw(4))[0..4], .little);
    }

    fn wide(self: *Reader) ReadError!u64 {
        return std.mem.readInt(u64, (try self.raw(8))[0..8], .little);
    }
};
