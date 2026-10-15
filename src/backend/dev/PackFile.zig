//! On-disk form of one module's pack: its procedure artifacts and the table
//! from specialization key to root artifact and ownership signature.
//!
//! The encoding is a pure function of its inputs (fixed-width little-endian
//! integers, length-prefixed bytes, artifacts in set order, specs in table
//! order), so two builds of the same module write identical files and the
//! store's rename-into-place protocol never has to compare contents.
//!
//! Line entries preserve exact SourceLoc values and order. Their file indices
//! are meaningful only with the originating LIR source-file domain; a persistent
//! consumer must bind that domain explicitly or omit lines from debug emission.

const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const ProcArtifact = @import("ProcArtifact.zig");
const RelocationMod = @import("Relocation.zig");

const Allocator = std.mem.Allocator;

const magic = "RPCK";
/// Format version; bump whenever the encoding or artifact contents change.
pub const format_version: u32 = 25;

/// One specialization the pack can serve: its reservation-time key, the
/// artifact holding its procedure, and the ownership signature and
/// uniqueness facts ARC solved for that procedure, which the linking program
/// adopts as fixed.
pub const SpecEntry = struct {
    key: [32]u8,
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

/// A pack read back from its bytes.
pub const Pack = struct {
    set: ProcArtifact.Set,
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry = &.{},
    materializer_reads: []const MaterializerRead = &.{},

    pub fn deinit(self: *Pack) void {
        self.set.deinit();
    }
};

/// Why bytes could not be read as a pack.
pub const ReadError = Allocator.Error || error{
    MalformedPack,
    UnsupportedPackVersion,
};

/// Why an artifact set could not be written as a pack.
pub const WriteError = Allocator.Error || error{ InvalidCodeTarget, InvalidSourceEventCertificate, InvalidLiteralRecipe, InvalidRootMaterializationDependency };

/// Native-image offsets have no persistent representation.
const PersistentCodeTarget = union(enum) {
    local: u32,
    symbolic: @FieldType(ProcArtifact.SymbolicReference, "target"),
};

test "pack preserves physical callable targets and observation facts but rejects image offsets" {
    const allocator = std.testing.allocator;
    const leaf = ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(79) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    var relocation = [_]ProcArtifact.DataRelocation{.{
        .offset = 0,
        .name = "frozen-local-target",
        .addend = 0,
        .function = true,
        .code_target = .{ .local = 0 },
    }};
    var artifacts = [_]ProcArtifact.Artifact{ leaf, leaf };
    artifacts[1].data = &.{.{
        .name = "frozen",
        .bytes = "\x00\x00\x00\x00\x00\x00\x00\x00",
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &relocation,
    }};
    const set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    const bytes = try write(allocator, &set, &.{});
    defer allocator.free(bytes);
    var decoded = try read(allocator, bytes);
    defer decoded.deinit();
    try std.testing.expectEqualDeep(artifacts[1], decoded.set.artifacts[1]);
    const corrupt = try allocator.dupe(u8, bytes);
    defer allocator.free(corrupt);
    const name_offset = std.mem.find(u8, corrupt, relocation[0].name).?;
    const target_offset = name_offset - 8; // length prefix and the local target word
    std.mem.writeInt(u32, corrupt[target_offset..][0..4], 2, .little);
    try std.testing.expectError(error.MalformedPack, read(allocator, corrupt));
    @memcpy(corrupt, bytes);
    corrupt[target_offset - 1] = 2; // No persistent image_offset union member.
    try std.testing.expectError(error.MalformedPack, read(allocator, corrupt));
    relocation[0].code_target = .{ .image_offset = 123 };
    try std.testing.expectError(error.InvalidCodeTarget, write(allocator, &set, &.{}));
}

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

/// Encode an artifact set and its spec table. Every carried constant the
/// set's program named for itself is written under its content name, and so
/// is every relocation to it, so the pack links into any program. Explicit
/// context immediates are stored as zero: their descriptors, not the producer's
/// dense IDs, decide the persistent bytes.
pub fn write(allocator: Allocator, set: *const ProcArtifact.Set, specs: []const SpecEntry) WriteError![]u8 {
    return writeOffers(allocator, set, specs, &.{}, &.{});
}

/// `write`, with the discovery summaries the pack also offers and the literal
/// reads behind the literal materializers its code calls.
pub fn writeOffers(
    allocator: Allocator,
    set: *const ProcArtifact.Set,
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry,
    materializer_reads: []const MaterializerRead,
) WriteError![]u8 {
    for (specs) |spec| {
        if (spec.artifact >= set.artifacts.len or set.artifacts[spec.artifact].root_materialization != null)
            return error.InvalidRootMaterializationDependency;
    }
    for (specs) |spec| if (spec.demand) |demand| {
        for (demand.literal_reads) |literal| if (!try literal.recipe.valid(allocator)) return error.InvalidLiteralRecipe;
    };
    for (materializer_reads) |entry| if (!try entry.read.recipe.valid(allocator)) return error.InvalidLiteralRecipe;
    for (set.artifacts) |artifact| {
        try ProcArtifact.validateRootMaterializations(allocator, artifact);
        if (artifact.root_materialization == null) {
            for (artifact.refs) |reference| {
                if (reference.target < set.artifacts.len and set.artifacts[reference.target].root_materialization != null)
                    return error.InvalidRootMaterializationDependency;
            }
        }
        if (artifact.source_events.len != 0) {
            const contract = artifact.context_contract orelse return error.InvalidSourceEventCertificate;
            try ProcArtifact.validateSourceEventCertificates(allocator, artifact, contract.target.toCpuArch().endian());
        } else {
            for (artifact.relocations) |relocation| {
                if (relocation.kind == .function and std.mem.eql(u8, relocation.name, @import("builtins").source_events.symbol_name)) return error.InvalidSourceEventCertificate;
            }
        }
    }
    for (set.artifacts) |artifact| for (artifact.data) |item| for (item.relocations) |relocation| {
        if (relocation.code_target) |target| {
            if (!relocation.function or target == .image_offset) return error.InvalidCodeTarget;
            if (target == .local and target.local >= set.artifacts.len) return error.InvalidCodeTarget;
        }
    };
    var names = try ProcArtifact.ContentNames.init(allocator, set);
    defer names.deinit();
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    var writer = Writer{ .allocator = allocator, .bytes = &bytes };

    try writer.raw(magic);
    try writer.word(format_version);
    // The body's content hash names this exact physical graph for every
    // reader, so a reader never hashes the decoded artifacts again.
    const namespace_offset = bytes.items.len;
    try writer.raw(&(@as([32]u8, @splat(0))));
    const body_offset = bytes.items.len;
    try writer.word(@intCast(set.artifacts.len));
    try writer.word(@intCast(specs.len));
    // Where the spec table starts, so an index reads the offers without
    // decoding the artifacts before them (`readIndex`).
    const specs_offset_at = bytes.items.len;
    try writer.wide(0);

    for (set.artifacts) |artifact| {
        try writer.context(artifact.context_contract);
        try writer.context(artifact.context_dependencies);
        try writer.context(artifact.context_complete);
        try writer.context(artifact.callable_contract);
        try writer.context(artifact.root_materialization);
        try writer.word(@intCast(artifact.materializations.len));
        for (artifact.materializations) |dependency| {
            try writer.str(names.of(dependency.binding_name));
            try writer.str(names.of(dependency.header_name));
            try writer.context(dependency.materialization);
        }
        try writer.word(@intCast(artifact.source_events.len));
        for (artifact.source_events) |certificate| {
            try writer.str(names.of(certificate.data_name));
            // context encodes each declared integer/array/struct field, not
            // native extern-struct padding or process memory.
            try writer.context(certificate.descriptor);
            try writer.context(certificate.branch_regions);
            try writer.context(certificate.site_origin);
        }
        switch (artifact.kind) {
            .proc => |identity| {
                try writer.byte(0);
                try writer.raw(&identity.bytes);
            },
            .rc_helper => |name| {
                try writer.byte(1);
                try writer.str(name);
            },
            .boxy_thunk => |identity| {
                try writer.byte(2);
                try writer.raw(&identity.bytes);
            },
            .entrypoint => try writer.byte(3),
            .message_pool_run => try writer.byte(4),
            .branch_island => try writer.byte(5),
        }
        try writer.word(artifact.entry);
        if (artifact.frame) |frame| {
            try writer.byte(1);
            try writer.word(frame.prologue_size);
            try writer.word(frame.stack_alloc);
            try writer.word(frame.frame_size);
            try writer.word(frame.callee_saved_mask);
            try writer.word(frame.epilogue_offset);
            try writer.byte(@intFromBool(frame.uses_frame_pointer));
        } else {
            try writer.byte(0);
        }
        try writer.str(artifact.code);
        try writer.word(@intCast(artifact.refs.len));
        for (artifact.refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(switch (ref.form) {
                .call => 0,
                .addr => 1,
                .inline_call => 2,
            });
            try writer.word(ref.target);
            try writer.word(ref.delta);
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
        }
        try writer.word(@intCast(artifact.symbolic_refs.len));
        for (artifact.symbolic_refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(switch (ref.form) {
                .call => 0,
                .addr => 1,
                .inline_call => 2,
            });
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
            switch (ref.target) {
                .proc => |identity| {
                    try writer.byte(0);
                    try writer.raw(&identity.bytes);
                },
                .rc_helper => |name| {
                    try writer.byte(1);
                    try writer.str(name);
                },
                .boxy_thunk => |identity| {
                    try writer.byte(2);
                    try writer.raw(&identity.bytes);
                },
            }
        }
        try writer.word(@intCast(artifact.lines.len));
        for (artifact.lines) |line| {
            try writer.word(line.offset);
            try writer.word(line.loc.file);
            try writer.word(line.loc.line);
            try writer.word(line.loc.column);
        }
        try writer.word(@intCast(artifact.relocations.len));
        for (artifact.relocations) |relocation| {
            try writer.word(relocation.offset);
            switch (relocation.kind) {
                .function => {
                    try writer.byte(0);
                    try writer.byte(0);
                },
                .data => |data_kind| {
                    try writer.byte(1);
                    try writer.byte(@backingInt(data_kind));
                },
            }
            try writer.str(names.of(relocation.name));
            // A reference to carried program data now names it by content.
            const scope: ProcArtifact.SymbolScope = if (names.renames(relocation.name)) .shared else relocation.scope;
            try writer.byte(@backingInt(scope));
        }
        try writer.word(@intCast(artifact.data.len));
        for (artifact.data) |item| {
            try writer.str(names.of(item.name));
            try writer.str(item.bytes);
            try writer.word(item.alignment);
            try writer.word(item.symbol_offset);
            try writer.word(@intCast(item.relocations.len));
            for (item.relocations) |relocation| {
                try writer.word(relocation.offset);
                try writer.wide(@bitCast(relocation.addend));
                try writer.byte(@intFromBool(relocation.function));
                try writer.byte(@intFromBool(relocation.external));
                const persistent: ?PersistentCodeTarget = if (relocation.code_target) |target| switch (target) {
                    .local => |node| .{ .local = node },
                    .symbolic => |symbol| .{ .symbolic = symbol },
                    .image_offset => unreachable,
                } else null;
                try writer.context(persistent);
                // An external binding names the linking image's symbol.
                try writer.str(if (relocation.external) relocation.name else names.of(relocation.name));
            }
        }
    }

    std.mem.writeInt(u64, bytes.items[specs_offset_at..][0..8], bytes.items.len, .little);
    for (specs) |spec| {
        try writer.raw(&spec.key);
        try writer.word(spec.artifact);
        try writer.byte(@intFromBool(spec.platform_requirement_relation != null));
        if (spec.platform_requirement_relation) |relation| try writer.raw(&relation);
        try writer.context(spec.demand);
        try writer.word(@intCast(spec.comptime_root_reads.len));
        for (spec.comptime_root_reads) |root_read| {
            try writer.raw(&root_read.module);
            try writer.word(root_read.root);
        }
        try writer.wide(spec.rc_borrowed_params);
        try writer.byte(@intFromBool(spec.rc_ret_borrowed));
        try writer.wide(spec.rc_ret_lenders);
        try writer.wide(spec.rc_read_only_params);
        try writer.byte(@intFromBool(spec.rc_ret_unique));
        try writer.wide(spec.rc_ret_unique_fields);
        try writer.word(@intCast(spec.rc_ret_conditions.len));
        for (spec.rc_ret_conditions) |condition| try writer.word(condition);
    }

    try writer.word(@intCast(discovery.len));
    for (discovery) |entry| {
        try writer.raw(&entry.key);
        try writer.byte(@intFromBool(entry.platform_requirement_relation != null));
        if (entry.platform_requirement_relation) |relation| try writer.raw(&relation);
    }

    try writer.word(@intCast(materializer_reads.len));
    for (materializer_reads) |entry| {
        try writer.raw(&entry.wrapper_identity);
        try writer.context(entry.read);
    }

    var namespace = std.crypto.hash.Blake3.init(.{});
    namespace.update("roc.native.physical.pack.v1");
    namespace.update(bytes.items[body_offset..]);
    namespace.final(bytes.items[namespace_offset..][0..32]);
    return try bytes.toOwnedSlice(allocator);
}

/// Decode a pack. Every slice in the result is owned by the pack's arena.
pub fn read(allocator: Allocator, bytes: []const u8) ReadError!Pack {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();
    var reader = Reader{ .bytes = bytes };

    const header = try reader.raw(magic.len);
    if (!std.mem.eql(u8, header, magic)) return error.MalformedPack;
    if (try reader.word() != format_version) return error.UnsupportedPackVersion;
    const physical_namespace = (try reader.raw(32))[0..32].*;
    const artifact_count = try reader.word();
    const spec_count = try reader.word();
    const specs_offset = try reader.wide();

    const artifacts = try arena_allocator.alloc(ProcArtifact.Artifact, artifact_count);
    for (artifacts) |*artifact| {
        const context_contract = try reader.context(?@import("LirCodeGen.zig").FragmentContract, arena_allocator);
        const context_dependencies = try reader.context(?@import("LirCodeGen.zig").FragmentContextDependencies, arena_allocator);
        const context_complete = try reader.context(bool, arena_allocator);
        const callable_contract = try reader.context(?[32]u8, arena_allocator);
        const root_materialization = try reader.context(?lir.Program.RootMaterialization, arena_allocator);
        const materializations = try arena_allocator.alloc(ProcArtifact.RootMaterializationDependency, try reader.word());
        for (materializations) |*dependency| {
            dependency.* = .{
                .binding_name = try reader.strOwned(arena_allocator),
                .header_name = try reader.strOwned(arena_allocator),
                .materialization = try reader.context(lir.Program.RootMaterialization, arena_allocator),
            };
        }
        const source_events = try arena_allocator.alloc(ProcArtifact.SourceEventCertificate, try reader.word());
        for (source_events) |*certificate| {
            certificate.* = .{
                .data_name = try reader.strOwned(arena_allocator),
                .descriptor = try reader.context(@import("builtins").source_events.Descriptor, arena_allocator),
                .branch_regions = try reader.context([]const @import("base").Region, arena_allocator),
                .site_origin = try reader.context(?lir.LIR.CheckedSiteOrigin, arena_allocator),
            };
        }
        const kind: ProcArtifact.Kind = switch (try reader.byte()) {
            0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
            1 => .{ .rc_helper = try reader.strOwned(arena_allocator) },
            2 => .{ .boxy_thunk = .{ .bytes = (try reader.raw(32))[0..32].* } },
            3 => .entrypoint,
            4 => .message_pool_run,
            5 => .branch_island,
            else => return error.MalformedPack,
        };
        const entry = try reader.word();
        const frame: ?ProcArtifact.Frame = switch (try reader.byte()) {
            0 => null,
            1 => .{
                .prologue_size = try reader.word(),
                .stack_alloc = try reader.word(),
                .frame_size = try reader.word(),
                .callee_saved_mask = try reader.word(),
                .epilogue_offset = try reader.word(),
                .uses_frame_pointer = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                },
            },
            else => return error.MalformedPack,
        };
        const code = try reader.strOwned(arena_allocator);
        const refs = try arena_allocator.alloc(ProcArtifact.Reference, try reader.word());
        for (refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = switch (try reader.byte()) {
                    0 => .call,
                    1 => .addr,
                    2 => .inline_call,
                    else => return error.MalformedPack,
                },
                .target = try reader.word(),
                .delta = try reader.word(),
                .veneer = veneer: {
                    const value = try reader.word();
                    break :veneer if (value == std.math.maxInt(u32)) null else value;
                },
            };
        }
        const symbolic_refs = try arena_allocator.alloc(ProcArtifact.SymbolicReference, try reader.word());
        for (symbolic_refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = switch (try reader.byte()) {
                    0 => .call,
                    1 => .addr,
                    2 => .inline_call,
                    else => return error.MalformedPack,
                },
                .veneer = veneer: {
                    const value = try reader.word();
                    break :veneer if (value == std.math.maxInt(u32)) null else value;
                },
                .target = switch (try reader.byte()) {
                    0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
                    1 => .{ .rc_helper = try reader.strOwned(arena_allocator) },
                    2 => .{ .boxy_thunk = .{ .bytes = (try reader.raw(32))[0..32].* } },
                    else => return error.MalformedPack,
                },
            };
        }
        const lines = try arena_allocator.alloc(ProcArtifact.LineEntry, try reader.word());
        for (lines) |*line| {
            line.* = .{
                .offset = try reader.word(),
                .loc = .{ .file = try reader.word(), .line = try reader.word(), .column = try reader.word() },
            };
        }
        const relocations = try arena_allocator.alloc(ProcArtifact.NamedRelocation, try reader.word());
        for (relocations) |*relocation| {
            const offset = try reader.word();
            const kind_tag = try reader.byte();
            const data_kind_raw = try reader.byte();
            const name = try reader.strOwned(arena_allocator);
            const scope = std.enums.fromInt(ProcArtifact.SymbolScope, try reader.byte()) orelse return error.MalformedPack;
            relocation.* = .{
                .offset = offset,
                .name = name,
                .scope = scope,
                .kind = switch (kind_tag) {
                    0 => .function,
                    1 => .{ .data = std.enums.fromInt(RelocationMod.DataRelocationKind, data_kind_raw) orelse return error.MalformedPack },
                    else => return error.MalformedPack,
                },
            };
        }
        const data = try arena_allocator.alloc(ProcArtifact.DataItem, try reader.word());
        for (data) |*item| {
            const name = try reader.strOwned(arena_allocator);
            const item_bytes = try reader.strOwned(arena_allocator);
            const alignment = try reader.word();
            const symbol_offset = try reader.word();
            const data_relocations = try arena_allocator.alloc(ProcArtifact.DataRelocation, try reader.word());
            for (data_relocations) |*relocation| {
                const offset = try reader.word();
                const addend: i64 = @bitCast(try reader.wide());
                const function = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                };
                const external = switch (try reader.byte()) {
                    0 => false,
                    1 => true,
                    else => return error.MalformedPack,
                };
                const persistent = try reader.context(?PersistentCodeTarget, arena_allocator);
                const code_target: ?ProcArtifact.CodeTarget = if (persistent) |target| switch (target) {
                    .local => |node| .{ .local = node },
                    .symbolic => |symbol| .{ .symbolic = symbol },
                } else null;
                if (code_target) |target| {
                    if (!function or target == .image_offset) return error.MalformedPack;
                    if (target == .local and target.local >= artifact_count) return error.MalformedPack;
                }
                relocation.* = .{
                    .offset = offset,
                    .name = try reader.strOwned(arena_allocator),
                    .addend = addend,
                    .function = function,
                    .external = external,
                    .code_target = code_target,
                };
            }
            item.* = .{
                .name = name,
                .bytes = item_bytes,
                .alignment = alignment,
                .symbol_offset = symbol_offset,
                .relocations = data_relocations,
            };
        }
        artifact.* = .{
            .kind = kind,
            .source_events = source_events,
            .root_materialization = root_materialization,
            .materializations = materializations,
            .code = code,
            .entry = entry,
            .frame = frame,
            .refs = refs,
            .symbolic_refs = symbolic_refs,
            .lines = lines,
            .relocations = relocations,
            .data = data,
            .context_contract = context_contract,
            .context_dependencies = context_dependencies,
            .context_complete = context_complete,
            .callable_contract = callable_contract,
        };
        ProcArtifact.validateCodePatches(artifact.*) catch return error.MalformedPack;
        ProcArtifact.validateRootMaterializations(allocator, artifact.*) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidRootMaterializationDependency => return error.MalformedPack,
        };
        for (refs) |ref| {
            if (ref.target >= artifact_count) return error.MalformedPack;
        }
        if (artifact.source_events.len != 0) {
            const contract = artifact.context_contract orelse return error.MalformedPack;
            ProcArtifact.validateSourceEventCertificates(allocator, artifact.*, contract.target.toCpuArch().endian()) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidSourceEventCertificate => return error.MalformedPack,
            };
        } else {
            for (artifact.relocations) |relocation| {
                if (relocation.kind == .function and std.mem.eql(u8, relocation.name, @import("builtins").source_events.symbol_name)) return error.MalformedPack;
            }
        }
    }

    for (artifacts) |artifact| {
        if (artifact.root_materialization != null) continue;
        for (artifact.refs) |reference| {
            if (artifacts[reference.target].root_materialization != null) return error.MalformedPack;
        }
    }
    if (reader.offset != specs_offset) return error.MalformedPack;
    const offers = try readOffers(allocator, arena_allocator, &reader, artifact_count, spec_count);
    for (offers.specs) |spec| {
        if (artifacts[spec.artifact].root_materialization != null) return error.MalformedPack;
    }
    const specs = offers.specs;
    const discovery = offers.discovery;
    const materializer_reads = offers.materializer_reads;
    if (reader.offset != bytes.len) return error.MalformedPack;

    return .{
        .set = .{ .arena = arena, .artifacts = artifacts, .physical_namespace = physical_namespace },
        .specs = specs,
        .discovery = discovery,
        .materializer_reads = materializer_reads,
    };
}

/// The offers a pack makes, decoded without its artifacts.
pub const Offers = struct {
    specs: []const SpecEntry,
    discovery: []const DiscoveryEntry,
    materializer_reads: []const MaterializerRead,
};

/// A pack's offers, read without decoding its artifacts. Each spec names an
/// artifact by index; `read` checks the artifact itself.
pub const Index = struct {
    arena: std.heap.ArenaAllocator,
    physical_namespace: [32]u8,
    offers: Offers,

    pub fn deinit(self: *Index) void {
        self.arena.deinit();
    }
};

/// Read a pack's offers without decoding its artifacts.
pub fn readIndex(allocator: Allocator, bytes: []const u8) ReadError!Index {
    if (bytes.len < header_len) return error.MalformedPack;
    const location = try readIndexLocation(bytes[0..header_len]);
    if (location.specs_offset > bytes.len) return error.MalformedPack;
    return readIndexAt(allocator, location, bytes[@intCast(location.specs_offset)..]);
}

/// The length of the fixed header every pack starts with.
pub const header_len = magic.len + 4 + 32 + 4 + 4 + 8;

/// Where a pack's offers start, read from its header.
pub const IndexLocation = struct {
    physical_namespace: [32]u8,
    artifact_count: u32,
    spec_count: u32,
    specs_offset: u64,
};

/// Read the location of a pack's offers from its first `header_len` bytes.
pub fn readIndexLocation(header_bytes: []const u8) ReadError!IndexLocation {
    var reader = Reader{ .bytes = header_bytes };
    const header = try reader.raw(magic.len);
    if (!std.mem.eql(u8, header, magic)) return error.MalformedPack;
    if (try reader.word() != format_version) return error.UnsupportedPackVersion;
    const location: IndexLocation = .{
        .physical_namespace = (try reader.raw(32))[0..32].*,
        .artifact_count = try reader.word(),
        .spec_count = try reader.word(),
        .specs_offset = try reader.wide(),
    };
    if (location.specs_offset < header_len) return error.MalformedPack;
    return location;
}

/// Read a pack's offers from the bytes from its offers to its end, so a
/// reader need not read its artifacts.
pub fn readIndexAt(allocator: Allocator, location: IndexLocation, tail: []const u8) ReadError!Index {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    var reader = Reader{ .bytes = tail };
    const offers = try readOffers(allocator, arena.allocator(), &reader, location.artifact_count, location.spec_count);
    if (reader.offset != tail.len) return error.MalformedPack;
    return .{ .arena = arena, .physical_namespace = location.physical_namespace, .offers = offers };
}

fn readOffers(allocator: Allocator, arena_allocator: Allocator, reader: *Reader, artifact_count: u32, spec_count: u32) ReadError!Offers {
    const specs = try arena_allocator.alloc(SpecEntry, spec_count);
    for (specs) |*spec| {
        spec.* = .{
            .key = (try reader.raw(32))[0..32].*,
            .artifact = try reader.word(),
            .platform_requirement_relation = switch (try reader.byte()) {
                0 => null,
                1 => (try reader.raw(32))[0..32].*,
                else => return error.MalformedPack,
            },
            .demand = try reader.context(?lir.Program.CachedCtfeDemand, arena_allocator),
            .comptime_root_reads = reads: {
                const reads = try arena_allocator.alloc(lir.CheckedPipeline.ComptimeRootRead, try reader.word());
                for (reads) |*root_read| root_read.* = .{ .module = (try reader.raw(32))[0..32].*, .root = try reader.word() };
                break :reads reads;
            },
            .rc_borrowed_params = try reader.wide(),
            .rc_ret_borrowed = switch (try reader.byte()) {
                0 => false,
                1 => true,
                else => return error.MalformedPack,
            },
            .rc_ret_lenders = try reader.wide(),
            .rc_read_only_params = try reader.wide(),
            .rc_ret_unique = switch (try reader.byte()) {
                0 => false,
                1 => true,
                else => return error.MalformedPack,
            },
            .rc_ret_unique_fields = try reader.wide(),
            .rc_ret_conditions = &.{},
        };
        const conditions = try arena_allocator.alloc(u32, try reader.word());
        for (conditions) |*condition| condition.* = try reader.word();
        spec.rc_ret_conditions = conditions;
        if (spec.artifact >= artifact_count) return error.MalformedPack;
        if (spec.demand) |demand| {
            for (demand.literal_reads) |literal| if (!try literal.recipe.valid(allocator)) return error.MalformedPack;
        }
    }
    const discovery = try arena_allocator.alloc(DiscoveryEntry, try reader.word());
    for (discovery) |*entry| {
        entry.* = .{
            .key = (try reader.raw(32))[0..32].*,
            .platform_requirement_relation = switch (try reader.byte()) {
                0 => null,
                1 => (try reader.raw(32))[0..32].*,
                else => return error.MalformedPack,
            },
        };
    }
    const materializer_reads = try arena_allocator.alloc(MaterializerRead, try reader.word());
    for (materializer_reads) |*entry| {
        entry.* = .{
            .wrapper_identity = (try reader.raw(32))[0..32].*,
            .read = try reader.context(lir.Program.CachedLiteralRead, arena_allocator),
        };
        if (!try entry.read.recipe.valid(allocator)) return error.MalformedPack;
    }
    return .{ .specs = specs, .discovery = discovery, .materializer_reads = materializer_reads };
}

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

    fn str(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.word(@intCast(data.len));
        try self.raw(data);
    }
};

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

    fn strOwned(self: *Reader, allocator: Allocator) ReadError![]u8 {
        const len = try self.word();
        return try allocator.dupe(u8, try self.raw(len));
    }
};

test "pack ownership survives input destruction and allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testOwnedPack, .{});
}

test "pack ordinary code patches validate producer-declared spans" {
    const allocator = std.testing.allocator;
    var arena = collections.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const code = [_]u8{ 0x48, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0 } ++ @as([32]u8, @splat(0));
    var artifacts = [_]ProcArtifact.Artifact{.{
        .kind = .entrypoint,
        .code = &code,
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .context_dependencies = .{},
        .context_contract = .{
            .target = .x64glibc,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
        },
    }};
    var input = ProcArtifact.Set{ .arena = arena, .artifacts = &artifacts };
    var cloned = try ProcArtifact.combine(allocator, &.{&input});
    defer cloned.deinit();
    const bytes = try write(allocator, &cloned, &.{});
    defer allocator.free(bytes);
    var decoded = try read(allocator, bytes);
    defer decoded.set.deinit();
    try std.testing.expectEqualDeep(cloned.artifacts[0], decoded.set.artifacts[0]);
    artifacts[0].relocations = &.{.{ .offset = 40, .name = "ordinary", .scope = .shared, .kind = .{ .data = .abs64 } }};
    const overlap = try write(allocator, &input, &.{});
    defer allocator.free(overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, overlap));
    try std.testing.expectEqualSlices(u8, &code, input.artifacts[0].code);
    artifacts[0].relocations = &.{.{ .offset = 40, .name = "ordinary", .scope = .shared, .kind = .function }};
    const function_overlap = try write(allocator, &input, &.{});
    defer allocator.free(function_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, function_overlap));
    artifacts[0].relocations = &.{};
    artifacts[0].refs = &.{.{ .site = 40, .form = .addr, .target = 0, .delta = 0 }};
    const ref_overlap = try write(allocator, &input, &.{});
    defer allocator.free(ref_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, ref_overlap));
    artifacts[0].refs = &.{};
    artifacts[0].symbolic_refs = &.{.{ .site = 40, .form = .call, .target = .{ .rc_helper = "ordinary" } }};
    const symbolic_overlap = try write(allocator, &input, &.{});
    defer allocator.free(symbolic_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, symbolic_overlap));
    artifacts[0].symbolic_refs = &.{};
    // Code patches and same-offset data-section patches have distinct owners.
    artifacts[0].relocations = &.{.{ .offset = 10, .name = "ordinary", .scope = .shared, .kind = .{ .data = .abs64 } }};
    artifacts[0].refs = &.{.{ .site = 18, .form = .call, .target = 0, .delta = 0 }};
    artifacts[0].data = &.{.{
        .name = "datum",
        .bytes = &(@as([16]u8, @splat(0))),
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 2, .name = "ordinary", .addend = 0, .function = false }},
    }};
    const adjacent = try write(allocator, &input, &.{});
    defer allocator.free(adjacent);
    var accepted = try read(allocator, adjacent);
    defer accepted.set.deinit();

    var arm_code = @as([64]u8, @splat(0));
    for (0..4) |i| {
        const word: u32 = (if (i == 0) @as(u32, 0xd2800000) else 0xf2800000) |
            (@as(u32, @intCast(i)) << 21) | 7;
        std.mem.writeInt(u32, arm_code[16 + i * 4 ..][0..4], word, .little);
    }
    artifacts[0].code = &arm_code;
    artifacts[0].context_contract.?.target = .arm64glibc;
    artifacts[0].refs = &.{};
    for ([_]RelocationMod.DataRelocationKind{ .abs64, .rel32, .page21, .pageoff12 }) |kind| {
        const ordinary = [_]ProcArtifact.NamedRelocation{.{ .offset = 62, .name = "ordinary", .scope = .shared, .kind = .{ .data = kind } }};
        artifacts[0].relocations = &ordinary;
        const arm_overlap = try write(allocator, &input, &.{});
        defer allocator.free(arm_overlap);
        try std.testing.expectError(error.MalformedPack, read(allocator, arm_overlap));
    }
    artifacts[0].relocations = &.{};
    for ([_]ProcArtifact.Form{ .addr, .inline_call }) |form| {
        const references = [_]ProcArtifact.Reference{.{ .site = 52, .form = form, .target = 0, .delta = 0 }};
        artifacts[0].refs = &references;
        const arm_ref_overlap = try write(allocator, &input, &.{});
        defer allocator.free(arm_ref_overlap);
        try std.testing.expectError(error.MalformedPack, read(allocator, arm_ref_overlap));
    }
    artifacts[0].refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0, .veneer = 52 }};
    const veneer_overlap = try write(allocator, &input, &.{});
    defer allocator.free(veneer_overlap);
    try std.testing.expectError(error.MalformedPack, read(allocator, veneer_overlap));
    artifacts[0].refs = &.{
        .{ .site = 0, .form = .addr, .target = 0, .delta = 0 },
        .{ .site = 32, .form = .inline_call, .target = 0, .delta = 0 },
    };
    artifacts[0].relocations = &.{
        .{ .offset = 48, .name = "ordinary", .scope = .shared, .kind = .{ .data = .page21 } },
        .{ .offset = 52, .name = "ordinary", .scope = .shared, .kind = .{ .data = .pageoff12 } },
    };
    const arm_adjacent = try write(allocator, &input, &.{});
    defer allocator.free(arm_adjacent);
    var arm_accepted = try read(allocator, arm_adjacent);
    defer arm_accepted.set.deinit();
    try std.testing.expectEqualSlices(u8, &arm_code, input.artifacts[0].code);
}

fn testOwnedPack(allocator: Allocator) (ReadError || WriteError || error{TestExpectedEqual})!void {
    const set = ProcArtifact.Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .rc_helper = "helper" },
            .callable_contract = @as([32]u8, @splat(17)),
            .context_contract = .{
                .target = .arm64glibc,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
            },
            .code = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0, .veneer = 3 }},
            .symbolic_refs = &.{.{ .site = 1, .form = .call, .target = .{ .rc_helper = "external" } }},
            .lines = &.{.{ .offset = 0, .loc = .{ .file = 2, .line = 10, .column = 8 } }},
            .relocations = &.{.{ .offset = 0, .name = "builtin", .scope = .shared, .kind = .function }},
            .data = &.{.{
                .name = "datum",
                .bytes = "bytes",
                .alignment = 8,
                .symbol_offset = 1,
                .relocations = &.{.{ .offset = 0, .name = "target", .addend = -2, .function = true, .external = true }},
            }},
        }},
    };
    const bytes = try write(allocator, &set, &.{});
    var bytes_live = true;
    defer if (bytes_live) allocator.free(bytes);
    var pack = try read(allocator, bytes);
    defer pack.deinit();
    allocator.free(bytes);
    bytes_live = false;
    try std.testing.expectEqualDeep(set.artifacts, pack.set.artifacts);
}

test "literal producer representation survives namespace translation" {
    const allocator = std.testing.allocator;
    const Representation = lir.Program.LiteralRepresentation;
    const Names = @FieldType(Representation.Materialized, "names");
    const Store = @FieldType(@FieldType(Representation.Materialized, "store"), "type_store");
    var names = Names.init(allocator);
    defer names.deinit();
    _ = try names.internRecordFieldLabel("unrelated");
    const name = try names.internRecordFieldLabel("value");
    var store = Store.init(allocator);
    defer store.deinit();
    _ = try store.append(.{ .primitive = .u8 });
    const number = try store.append(.{ .primitive = .u64 });
    const record = try store.append(.{ .record = try store.appendFieldSpan(&.{.{ .name = name, .ty = number, .default = null }}) });
    const recipe = try Representation.capture(allocator, &store, &names, record);
    defer recipe.deinit(allocator);
    try std.testing.expect(try recipe.valid(std.testing.allocator));
    try std.testing.expectEqual(@as(usize, 2), recipe.types.len);
    try std.testing.expectEqual(@as(usize, 1), recipe.field_names.len);
    var restored = try recipe.materialize(allocator);
    defer restored.deinit();
    const field = restored.store.type_store.fieldSpan(restored.store.type_store.get(recipe.root).record)[0];
    try std.testing.expectEqualStrings("value", restored.names.recordFieldLabelText(field.name));
    try std.testing.expectEqualDeep(store.get(number), restored.store.type_store.get(field.ty));
}

test "pack literal producer declaration roundtrip and malformed recipe rejection" {
    const allocator = std.testing.allocator;
    const artifacts = [_]ProcArtifact.Artifact{.{
        .kind = .{ .proc = lir.ProcIdentity.forTest(991) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    }};
    const set: ProcArtifact.Set = .{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    const owner = @as([32]u8, @splat(73));
    const ConstType = @typeInfo(@FieldType(lir.Program.LiteralRepresentation, "types")).pointer.child;
    const ConstTypeId = @FieldType(lir.Program.LiteralRepresentation, "root");
    const result_types = [_]ConstType{.{ .primitive = .u64 }};
    const number: ConstTypeId = @fromBackingInt(@intCast(result_types.len - 1));
    const callable_types = result_types ++ [_]ConstType{.{ .func = .{
        .args = .{ .start = 0, .len = 0 },
        .ret = number,
    } }};
    const callable: ConstTypeId = @fromBackingInt(@intCast(callable_types.len - 1));
    const bindings = [_]lir.Program.CachedLiteralBinding{
        .{
            .kind = 0,
            .binder = 3,
            .type_digest = .{ .bytes = @as([32]u8, @splat(47)) },
            .ty = .{ .root = number, .types = &result_types },
        },
        .{
            .kind = 1,
            .binder = 4,
            .type_digest = .{ .bytes = @as([32]u8, @splat(49)) },
            .ty = .{ .root = number, .types = &result_types },
        },
    };
    const contexts = [_]lir.Program.CachedLiteralLocalContext{.{
        .module = owner,
        .binder = 5,
        .expr = 31,
        .context_anchor = 37,
        .owner_template = .{ .artifact = .{ .bytes = owner }, .proc_base = @fromBackingInt(@intCast(2)), .template = @fromBackingInt(@intCast(3)) },
        .base_key = .{ .bytes = @as([32]u8, @splat(51)) },
        .specialization_key = .{ .bytes = @as([32]u8, @splat(53)) },
        .bindings = &bindings,
        .evidence_frames = &.{.{ .scope_id = .root, .parent = null, .roots_start = 0, .roots_len = 0 }},
        .evidence_frame_head = 0,
    }};
    var literals = [_]lir.Program.CachedLiteralRead{.{
        .recipe = .{
            .module = owner,
            .checked_expr = 17,
            .kind = .numeral,
            .producer_key = @as([32]u8, @splat(39)),
            .object_identity = lir.ProcIdentity.forTest(991).bytes,
            .owner_template = .{ .artifact = .{ .bytes = owner }, .proc_base = @fromBackingInt(@intCast(2)), .template = @fromBackingInt(@intCast(3)) },
            .owner_context_fn_key = .{ .bytes = @as([32]u8, @splat(41)) },
            .current_fn_key = .{ .bytes = @as([32]u8, @splat(43)) },
            .method_scope = owner,
            .checked_callable_ty = @fromBackingInt(@intCast(8)),
            .callable = .{ .root = callable, .types = &callable_types },
            .result = .{ .root = number, .types = &result_types },
            .evidence_frames = &.{.{ .scope_id = .root, .parent = null, .roots_start = 0, .roots_len = 0 }},
            .evidence_frame_head = 0,
            .local_contexts = &contexts,
        },
        .cause = .{ .module = owner, .region = .{ .start = .{ .offset = 19 }, .end = .{ .offset = 21 } }, .line = 2, .column = 7 },
    }};
    const specs = [_]SpecEntry{.{
        .key = @as([32]u8, @splat(61)),
        .artifact = 0,
        .demand = .{ .complete = true, .literal_reads = &literals },
        .rc_borrowed_params = 0,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    }};
    try std.testing.expect(specs[0].demand.?.permitsRetirement());
    const bytes = try write(allocator, &set, &specs);
    defer allocator.free(bytes);
    const again = try write(allocator, &set, &specs);
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, bytes, again);
    var decoded = try read(allocator, bytes);
    defer decoded.deinit();
    try std.testing.expectEqualDeep(specs[0].demand.?, decoded.specs[0].demand.?);
    try std.testing.expect(decoded.specs[0].demand.?.permitsRetirement());
    const result = decoded.specs[0].demand.?.literal_reads[0].recipe.result;
    var restored = try result.materialize(allocator);
    defer restored.deinit();
    try std.testing.expectEqualDeep(result_types[0], restored.store.type_store.get(result.root));
    // The read behind a literal materializer the code calls travels in the
    // pack's own table, under the materializer's wrapper identity.
    const materializer_reads = [_]MaterializerRead{.{ .wrapper_identity = @as([32]u8, @splat(63)), .read = literals[0] }};
    const with_reads = try writeOffers(allocator, &set, &specs, &.{}, &materializer_reads);
    defer allocator.free(with_reads);
    var decoded_reads = try read(allocator, with_reads);
    defer decoded_reads.deinit();
    try std.testing.expectEqualDeep(@as([]const MaterializerRead, &materializer_reads), decoded_reads.materializer_reads);
    try std.testing.expectEqual(@as(usize, 0), decoded.materializer_reads.len);
    literals[0].recipe.result.root = @fromBackingInt(@intCast(100));
    try std.testing.expectError(error.InvalidLiteralRecipe, write(allocator, &set, &specs));
    const invalid_reads = [_]MaterializerRead{.{ .wrapper_identity = @as([32]u8, @splat(63)), .read = literals[0] }};
    try std.testing.expectError(error.InvalidLiteralRecipe, writeOffers(allocator, &set, &.{}, &.{}, &invalid_reads));
}

test "pack bytes round-trip every artifact field and spec entry" {
    const testing = std.testing;
    var arena = collections.SingleThreadArena.init(testing.allocator);
    const a = arena.allocator();
    const refs = try a.dupe(ProcArtifact.Reference, &.{
        .{ .site = 5, .form = .call, .target = 1, .delta = 8 },
        .{ .site = 12, .form = .addr, .target = 0, .delta = 0 },
    });
    const relocations = try a.dupe(ProcArtifact.NamedRelocation, &.{
        .{ .offset = 3, .name = try a.dupe(u8, "roc_builtins_str_concat"), .scope = .shared, .kind = .function },
        .{ .offset = 9, .name = try a.dupe(u8, "roc__d1"), .scope = .program, .kind = .{ .data = .rel32 } },
    });
    const artifacts = try a.dupe(ProcArtifact.Artifact, &.{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(7) },
            .context_contract = .{
                .target = .arm64glibc,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
            },
            .code = try a.dupe(u8, "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"),
            .entry = 0,
            .frame = .{ .prologue_size = 4, .stack_alloc = 16, .frame_size = 16, .callee_saved_mask = 0x1000, .epilogue_offset = 4, .uses_frame_pointer = true },
            .refs = refs,
            .symbolic_refs = &.{
                .{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(8) } },
                .{ .site = 1, .form = .call, .veneer = 4, .target = .{ .rc_helper = "roc__rc_later" } },
                .{ .site = 2, .form = .addr, .target = .{ .boxy_thunk = lir.ProcIdentity.forTest(9) } },
            },
            .lines = &.{
                .{ .offset = 0, .loc = .{ .file = 3, .line = 42, .column = 7 } },
                .{ .offset = 0, .loc = .{ .file = 3, .line = 43, .column = 9 } },
            },
            .relocations = relocations,
            .data = try a.dupe(ProcArtifact.DataItem, &.{
                .{ .name = try a.dupe(u8, "roc__hab"), .bytes = try a.dupe(u8, "\x00\x00hi"), .alignment = 8, .symbol_offset = 2 },
                .{
                    .name = try a.dupe(u8, "roc__hcd"),
                    .bytes = try a.dupe(u8, &@as([16]u8, @splat('\x00'))),
                    .alignment = 8,
                    .symbol_offset = 0,
                    .relocations = try a.dupe(ProcArtifact.DataRelocation, &.{
                        .{ .offset = 0, .name = try a.dupe(u8, "roc__hab"), .addend = 2, .function = false },
                        .{ .offset = 8, .name = try a.dupe(u8, "roc__rc_decref_abc"), .addend = -1, .function = true },
                    }),
                },
            }),
        },
        .{
            .kind = .{ .rc_helper = try a.dupe(u8, "roc__rc_decref_abc") },
            .code = try a.dupe(u8, &.{ 0xeb, 0x02, 0x90, 0xc3 }),
            .entry = 2,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
        .{ .kind = .message_pool_run, .code = try a.dupe(u8, "hello"), .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    });
    var set = ProcArtifact.Set{ .arena = arena, .artifacts = artifacts };
    defer set.deinit();
    const specs = [_]SpecEntry{
        .{
            .key = @as([32]u8, @splat(0xab)),
            .artifact = 0,
            .rc_borrowed_params = 0b101,
            .rc_ret_borrowed = true,
            .rc_ret_lenders = 1,
            .rc_read_only_params = 0b100,
            .rc_ret_unique = true,
            .rc_ret_unique_fields = 0b10,
            .rc_ret_conditions = &.{ 0x0001_02ff, 0x0000_0403 },
            .comptime_root_reads = &.{
                .{ .module = @as([32]u8, @splat(0x11)), .root = 3 },
                .{ .module = @as([32]u8, @splat(0x22)), .root = 0 },
            },
        },
    };

    const bytes = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(bytes);
    const again = try write(testing.allocator, &set, &specs);
    defer testing.allocator.free(again);
    try testing.expectEqualSlices(u8, bytes, again);

    var pack = try read(testing.allocator, bytes);
    defer pack.deinit();
    try testing.expectEqual(@as(usize, 3), pack.set.artifacts.len);
    try testing.expectEqual(@as(usize, 1), pack.specs.len);
    try testing.expectEqualSlices(u8, &specs[0].key, &pack.specs[0].key);
    try testing.expectEqual(specs[0].rc_borrowed_params, pack.specs[0].rc_borrowed_params);
    try testing.expect(pack.specs[0].rc_ret_borrowed);
    try testing.expectEqual(specs[0].rc_read_only_params, pack.specs[0].rc_read_only_params);
    try testing.expect(pack.specs[0].rc_ret_unique);
    try testing.expectEqual(specs[0].rc_ret_unique_fields, pack.specs[0].rc_ret_unique_fields);
    try testing.expectEqualSlices(u32, specs[0].rc_ret_conditions, pack.specs[0].rc_ret_conditions);
    try testing.expectEqualDeep(specs[0].comptime_root_reads, pack.specs[0].comptime_root_reads);
    const proc = pack.set.artifacts[0];
    try testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(7).bytes, &proc.kind.proc.bytes);
    try testing.expectEqualSlices(u8, artifacts[0].code, proc.code);
    try testing.expectEqual(@as(usize, 2), proc.refs.len);
    try testing.expectEqual(@as(usize, 3), proc.symbolic_refs.len);
    try testing.expectEqualDeep(artifacts[0].symbolic_refs, proc.symbolic_refs);
    try testing.expectEqualDeep(artifacts[0].lines, proc.lines);
    try testing.expectEqual(ProcArtifact.Form.addr, proc.refs[1].form);
    try testing.expectEqual(@as(u32, 9), proc.relocations[1].offset);
    try testing.expectEqualStrings("roc__d1", proc.relocations[1].name);
    try testing.expectEqual(ProcArtifact.SymbolScope.program, proc.relocations[1].scope);
    try testing.expectEqual(ProcArtifact.SymbolScope.shared, proc.relocations[0].scope);
    try testing.expectEqual(RelocationMod.DataRelocationKind.rel32, proc.relocations[1].kind.data);
    try testing.expectEqual(@as(u32, 0x1000), proc.frame.?.callee_saved_mask);
    try testing.expectEqual(@as(usize, 2), proc.data.len);
    try testing.expectEqualStrings("roc__hab", proc.data[0].name);
    try testing.expectEqual(@as(u32, 2), proc.data[0].symbol_offset);
    try testing.expectEqual(@as(usize, 0), proc.data[0].relocations.len);
    try testing.expectEqualStrings("roc__hcd", proc.data[1].name);
    try testing.expectEqual(@as(usize, 2), proc.data[1].relocations.len);
    try testing.expectEqual(@as(u32, 8), proc.data[1].relocations[1].offset);
    try testing.expectEqual(@as(i64, -1), proc.data[1].relocations[1].addend);
    try testing.expect(proc.data[1].relocations[1].function);
    try testing.expectEqualStrings("roc__rc_decref_abc", proc.data[1].relocations[1].name);
    try testing.expect(!proc.data[1].relocations[0].function);
    try testing.expectEqual(@as(i64, 2), proc.data[1].relocations[0].addend);
    try testing.expectEqualStrings("roc__rc_decref_abc", pack.set.artifacts[1].kind.rc_helper);
    try testing.expectEqual(@as(u32, 2), pack.set.artifacts[1].entry);
    try testing.expect(pack.set.artifacts[1].frame == null);

    const rewritten = try write(testing.allocator, &pack.set, pack.specs);
    defer testing.allocator.free(rewritten);
    try testing.expectEqualSlices(u8, bytes, rewritten);

    try testing.expectError(error.MalformedPack, read(testing.allocator, bytes[0 .. bytes.len - 1]));
}

test "pack preserves relation summaries and rejects old records" {
    const Attempt = struct {
        fn run(allocator: Allocator, relation: ?[32]u8) (ReadError || WriteError || error{ TestExpectedEqual, TestExpectedError, TestUnexpectedError })!void {
            const set = ProcArtifact.Set{
                .arena = collections.SingleThreadArena.init(allocator),
                .artifacts = &.{.{
                    .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
                    .code = "code",
                    .entry = 0,
                    .frame = null,
                    .refs = &.{},
                    .relocations = &.{},
                    .data = &.{},
                }},
            };
            const bytes = try write(allocator, &set, &.{.{
                .key = @as([32]u8, @splat(7)),
                .artifact = 0,
                .platform_requirement_relation = relation,
                .rc_borrowed_params = 1,
                .rc_ret_borrowed = true,
                .rc_ret_lenders = 1,
                .rc_read_only_params = 1,
                .rc_ret_unique = true,
                .rc_ret_unique_fields = 2,
                .rc_ret_conditions = &.{0x0001_02ff},
            }});
            defer allocator.free(bytes);
            var pack = try read(allocator, bytes);
            defer pack.deinit();
            try std.testing.expectEqualDeep(relation, pack.specs[0].platform_requirement_relation);
            const rewritten = try write(allocator, &pack.set, pack.specs);
            defer allocator.free(rewritten);
            try std.testing.expectEqualSlices(u8, bytes, rewritten);
            // A previous contract must decline before admitting any summary.
            const old = try allocator.dupe(u8, bytes);
            defer allocator.free(old);
            std.mem.writeInt(u32, old[4..8], format_version - 1, .little);
            try std.testing.expectError(error.UnsupportedPackVersion, read(allocator, old));
        }
    };
    for ([_]?[32]u8{ null, @as([32]u8, @splat(0)), @as([32]u8, @splat(9)) }) |relation| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, Attempt.run, .{relation});
    }
}

test "pack writes program-local constants and every reference to them under content names" {
    const testing = std.testing;
    // Two programs gave one program-local name to different constants.
    const names = [_][]const u8{ "first", "second" };
    var packed_names: [names.len][]const u8 = undefined;
    var arena = collections.SingleThreadArena.init(testing.allocator);
    defer arena.deinit();
    for (names, 0..) |bytes, index| {
        const set = ProcArtifact.Set{
            .arena = collections.SingleThreadArena.init(testing.allocator),
            .artifacts = &.{.{
                .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
                .code = "code",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .relocations = &.{.{ .offset = 0, .name = "roc__d1", .scope = .program, .kind = .{ .data = .rel32 } }},
                .data = &.{
                    .{
                        .name = "roc__d1",
                        .bytes = &@as([8]u8, @splat('\x00')),
                        .alignment = 8,
                        .symbol_offset = 0,
                        .relocations = &.{.{ .offset = 0, .name = "roc__d1_1", .addend = 16, .function = false }},
                        .program_local_name = true,
                    },
                    .{
                        .name = "roc__d1_1",
                        .bytes = bytes,
                        .alignment = 8,
                        .symbol_offset = 0,
                        .relocations = &.{.{ .offset = 0, .name = "roc__d1_1", .addend = 0, .function = false, .external = true }},
                        .program_local_name = true,
                    },
                },
            }},
        };
        const encoded = try write(testing.allocator, &set, &.{});
        defer testing.allocator.free(encoded);
        var pack = try read(testing.allocator, encoded);
        defer pack.deinit();
        const artifact = pack.set.artifacts[0];
        const root = artifact.data[0];
        const node = artifact.data[1];
        try testing.expect(std.mem.startsWith(u8, root.name, ProcArtifact.content_data_prefix));
        try testing.expect(std.mem.startsWith(u8, node.name, ProcArtifact.content_data_prefix));
        try testing.expectEqualStrings(root.name, artifact.relocations[0].name);
        // Named by content, the reference means the same in every program.
        try testing.expectEqual(ProcArtifact.SymbolScope.shared, artifact.relocations[0].scope);
        try testing.expectEqualStrings(node.name, root.relocations[0].name);
        // An external binding keeps the linking image's name, even one the
        // set also carries a datum under.
        try testing.expectEqualStrings("roc__d1_1", node.relocations[0].name);
        packed_names[index] = try arena.allocator().dupe(u8, node.name);
    }
    try testing.expect(!std.mem.eql(u8, packed_names[0], packed_names[1]));
}
