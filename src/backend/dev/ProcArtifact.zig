//! Per-region artifacts of dev-backend machine code.
//!
//! The code generator emits one buffer for a whole program, but it keeps every
//! reference from that buffer into itself symbolic until a final patch pass
//! and logs each range it emits with its producer (`LirCodeGen.CodeRegion`,
//! `LirCodeGen.CodeRef`). An artifact is one such range lifted out of the
//! buffer: its bytes, the references it makes to other regions (as region
//! index plus delta), its relocations against named symbols, and its frame
//! metadata. Procedures and refcount helpers are named by content
//! (`ProcIdentity`, `roc__rc_*`), so an artifact set from one program can be
//! placed into another program's buffer with `assemble`, which appends each
//! region and re-resolves every reference to where its target landed.
//!
//! `verifyRoundTrip` is the gate for the format: a program assembled from its
//! own artifacts must produce the same bytes, relocations, and unwind records
//! as compiling it directly.

const std = @import("std");
const lir = @import("lir");
const layout = @import("layout");
const RelocationMod = @import("Relocation.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");

const Allocator = std.mem.Allocator;
const IndexedRelocation = RelocationMod.IndexedRelocation;

/// Whether a reference is a call or an address literal.
pub const Form = enum { call, addr };

/// A reference from an artifact's bytes to a location inside another artifact.
pub const Reference = struct {
    /// Offset of the CALL/BL or ADR/LEA instruction within the artifact.
    site: u32,
    form: Form,
    /// Index of the target artifact within the set.
    target: u32,
    /// Offset within the target artifact the reference resolves to.
    delta: u32,
};

/// A relocation against a named symbol, with its offset relative to the artifact.
pub const NamedRelocation = struct {
    offset: u32,
    name: []const u8,
    kind: union(enum) {
        function,
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
    branch_island,
};

/// A readonly datum the artifact's relocations name and its program
/// defined: carried so a program that lacks it can define it.
pub const DataItem = struct {
    name: []const u8,
    bytes: []const u8,
    alignment: u32,
    symbol_offset: u32,
};

/// One lifted region of machine code.
pub const Artifact = struct {
    kind: Kind,
    code: []const u8,
    /// Offset within `code` that references to this artifact resolve to.
    entry: u32,
    frame: ?Frame,
    refs: []const Reference,
    relocations: []const NamedRelocation,
    data: []const DataItem,
};

/// An ordered set of artifacts covering one code buffer. Order is emission
/// order, which `assemble` reproduces.
pub const Set = struct {
    arena: std.heap.ArenaAllocator,
    artifacts: []const Artifact,

    pub fn deinit(self: *Set) void {
        self.arena.deinit();
    }
};

/// Why a code buffer could not be lifted into artifacts.
pub const ExtractError = Allocator.Error || error{
    /// Two logged regions overlap: a helper was emitted inside another
    /// region's bytes, which only Boxy programs do.
    NestedCodeRegion,
    /// Bytes of the code buffer belong to no logged region.
    UncoveredCode,
    /// A reference resolves to an offset inside no region.
    DanglingReference,
    /// A relocation kind an artifact cannot carry.
    UnsupportedRelocation,
};

/// Lift every region of a finished code generator's buffer into an artifact set.
pub fn extract(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
) ExtractError!Set {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();

    var strings_by_name = std.StringHashMap(*const lir.Program.StaticDataExport).init(allocator);
    defer strings_by_name.deinit();
    for (string_exports) |*string_export| try strings_by_name.put(string_export.symbol_name, string_export);

    const code = codegen.getGeneratedCode();
    const regions = try allocator.dupe(CG.CodeRegion, codegen.codeRegions());
    defer allocator.free(regions);
    std.mem.sort(CG.CodeRegion, regions, {}, regionStartsBefore(CG.CodeRegion));

    var covered: usize = 0;
    for (regions) |region| {
        if (region.start < covered) return error.NestedCodeRegion;
        if (region.start != covered) return error.UncoveredCode;
        covered = region.end;
    }
    if (covered != code.len) return error.UncoveredCode;

    const unwind = codegen.getUnwindFunctions();
    const relocations = codegen.getRelocations();
    const refs = codegen.codeRefs();

    const artifacts = try arena_allocator.alloc(Artifact, regions.len);
    for (regions, 0..) |region, index| {
        const kind: Kind = switch (region.kind) {
            .proc => |proc_id| .{ .proc = proc_specs[@intFromEnum(proc_id)].identity },
            .rc_helper => |key| .{ .rc_helper = try LirCodeGenMod.compiledRcHelperSymbolName(arena_allocator, layout_store, key) },
            .boxy_thunk => |proc_id| .{ .boxy_thunk = proc_specs[@intFromEnum(proc_id)].identity },
            .entrypoint => .entrypoint,
            .message_pool_run => .message_pool_run,
            .branch_island => .branch_island,
            .spliced_proc => |identity| .{ .proc = identity },
            .spliced_helper => .{ .rc_helper = try arena_allocator.dupe(u8, codegen.splicedHelperName(region.start + region.entry) orelse return error.DanglingReference) },
        };

        var frame: ?Frame = null;
        for (unwind) |function| {
            if (function.start_offset != region.start + region.entry) continue;
            frame = .{
                .prologue_size = function.prologue_size,
                .stack_alloc = function.stack_alloc,
                .frame_size = function.frame_size,
                .callee_saved_mask = function.callee_saved_mask,
                .epilogue_offset = function.epilogue_offset,
                .uses_frame_pointer = function.uses_frame_pointer,
            };
            break;
        }

        var region_refs = std.ArrayList(Reference).empty;
        for (refs) |ref| {
            if (ref.site < region.start or ref.site >= region.end) continue;
            const target_offset: usize = switch (ref.target) {
                .proc => |proc_id| (codegen.compiledProcSymbol(proc_id) orelse return error.DanglingReference).code_start,
                .rc_helper => |key| codegen.compiledRcHelperOffset(key) orelse return error.DanglingReference,
                .message => |message_offset| messageOffsetInCode(CG, regions, message_offset) orelse return error.DanglingReference,
                .boxy_thunk => |proc_id| codegen.boxyThunkOffset(proc_id) orelse return error.DanglingReference,
                .offset => |offset| offset,
            };
            const target_index = regionContaining(CG, regions, target_offset) orelse return error.DanglingReference;
            try region_refs.append(arena_allocator, .{
                .site = @intCast(ref.site - region.start),
                .form = switch (ref.form) {
                    .call => .call,
                    .addr => .addr,
                },
                .target = @intCast(target_index),
                .delta = @intCast(target_offset - regions[target_index].start),
            });
        }

        var region_relocations = std.ArrayList(NamedRelocation).empty;
        for (relocations) |relocation| {
            switch (relocation) {
                .retired => continue,
                .local_data, .jmp_to_return => return error.UnsupportedRelocation,
                .linked_function, .linked_data => {},
            }
            const offset = relocation.getOffset();
            if (offset < region.start or offset >= region.end) continue;
            try region_relocations.append(arena_allocator, switch (relocation) {
                .linked_function => |function| .{
                    .offset = @intCast(offset - region.start),
                    .name = try arena_allocator.dupe(u8, codegen.symbolName(function.symbol)),
                    .kind = .function,
                },
                .linked_data => |data| .{
                    .offset = @intCast(offset - region.start),
                    .name = try arena_allocator.dupe(u8, codegen.symbolName(data.symbol)),
                    .kind = .{ .data = data.kind },
                },
                .retired, .local_data, .jmp_to_return => unreachable,
            });
        }

        var region_data = std.ArrayList(DataItem).empty;
        for (region_relocations.items) |relocation| {
            const string_export = strings_by_name.get(relocation.name) orelse continue;
            var already = false;
            for (region_data.items) |item| {
                if (std.mem.eql(u8, item.name, relocation.name)) already = true;
            }
            if (already) continue;
            try region_data.append(arena_allocator, .{
                .name = relocation.name,
                .bytes = try arena_allocator.dupe(u8, string_export.bytes),
                .alignment = string_export.alignment,
                .symbol_offset = string_export.symbol_offset,
            });
        }

        artifacts[index] = .{
            .kind = kind,
            .code = try arena_allocator.dupe(u8, code[region.start..region.end]),
            .entry = @intCast(region.entry),
            .frame = frame,
            .refs = try region_refs.toOwnedSlice(arena_allocator),
            .relocations = try region_relocations.toOwnedSlice(arena_allocator),
            .data = try region_data.toOwnedSlice(arena_allocator),
        };
    }

    return .{ .arena = arena, .artifacts = artifacts };
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

/// Code offset of a message pool byte: the last pool run that starts at or
/// before the message, plus the message's distance into that run.
fn messageOffsetInCode(comptime CG: type, regions: []const CG.CodeRegion, message_offset: u32) ?usize {
    var found: ?usize = null;
    for (regions) |region| {
        switch (region.kind) {
            .message_pool_run => |pool_from| {
                if (pool_from <= message_offset) found = region.start + (message_offset - pool_from);
            },
            .proc, .rc_helper, .boxy_thunk, .entrypoint, .branch_island, .spliced_proc, .spliced_helper => {},
        }
    }
    return found;
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
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @enumFromInt(@as(u32, @intCast(index))));
    }

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
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name);
            const offset: u64 = starts[index] + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => .{ .linked_function = .{ .offset = offset, .symbol = symbol } },
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
                .message_pool_run => .{ .message = ref.delta },
                .entrypoint, .branch_island => .{ .message = 0 },
            };
            try codegen.patchAssembledRef(
                starts[index] + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .addr => .addr,
                },
                target,
                starts[ref.target] + ref.delta,
            );
        }
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
    var set = try extract(CG, allocator, original, proc_specs, layout_store, string_exports);
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
    if (original.codeRegions().len != fresh.codeRegions().len) {
        std.debug.print("ROUNDTRIP region count mismatch: original {d}, fresh {d}\n", .{ original.codeRegions().len, fresh.codeRegions().len });
        return error.RoundTripMismatch;
    }
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
        .linked_function => |function| function.offset == rhs.linked_function.offset and
            std.mem.eql(u8, original.symbolName(function.symbol), fresh.symbolName(rhs.linked_function.symbol)),
        .linked_data => |data| data.offset == rhs.linked_data.offset and data.kind == rhs.linked_data.kind and
            std.mem.eql(u8, original.symbolName(data.symbol), fresh.symbolName(rhs.linked_data.symbol)),
        .local_data, .jmp_to_return, .retired => false,
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

/// Place the closure of `roots` (every artifact they reach through
/// references, transitively) from `set` into an open code generator, before
/// any of the program's own procedures compile. A procedure artifact whose
/// identity this program declares registers as that procedure, so the
/// program's own compile skips it and every call to it is an ordinary
/// direct call; refcount helpers register by name so a later request for the
/// same helper reuses the spliced code. `placed` remembers which artifacts of
/// `set` are already in the buffer across calls.
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
    for (roots) |root| try stack.append(allocator, root);
    while (stack.pop()) |index| {
        if (placed.contains(index)) continue;
        const gop = try seen.getOrPut(index);
        if (gop.found_existing) continue;
        // Another pack already spliced this artifact: content names make
        // the code the same, so references resolve to the existing copy.
        const artifact = set.artifacts[index];
        const existing: ?usize = switch (artifact.kind) {
            .proc, .boxy_thunk => |identity| codegen.splicedProcStart(identity),
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
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc, .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .spliced_proc = identity },
            .rc_helper => .spliced_helper,
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        const start = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        switch (artifact.kind) {
            .rc_helper => |name| try codegen.registerSplicedHelper(name, start + artifact.entry),
            .proc, .boxy_thunk => |identity| try codegen.registerSplicedProc(identity, start),
            .entrypoint, .message_pool_run, .branch_island => {},
        }
        try placed.putNoClobber(index, start);
        for (artifact.data) |item| try data_out.append(allocator, item);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name);
            const offset: u64 = start + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => .{ .linked_function = .{ .offset = offset, .symbol = symbol } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const start = placed.get(index) orelse unreachable;
        for (artifact.refs) |ref| {
            const target_start = placed.get(ref.target) orelse unreachable;
            const target_artifact = set.artifacts[ref.target];
            const target: CG.CodeRefTarget = switch (target_artifact.kind) {
                .proc, .boxy_thunk => |identity| if (procs_by_identity.get(identity)) |proc_id| .{ .proc = proc_id } else .{ .offset = target_start + ref.delta },
                .rc_helper, .entrypoint, .message_pool_run, .branch_island => .{ .offset = target_start + ref.delta },
            };
            try codegen.patchAssembledRef(
                start + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .addr => .addr,
                },
                target,
                target_start + ref.delta,
            );
        }
    }
}
