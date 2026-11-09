//! One module's Boxy unit: its object restored from the per-module cache
//! (`boxy_object_cache`), or lowered, compiled, and filed there (design.md
//! "Caching Under `.boxy`"). `roc build` links these objects, and compile-time
//! evaluation loads them into the compiler's own process.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir = @import("lir");
const backend = @import("backend");
const eval = @import("eval");
const roc_target = @import("roc_target");
const static_data_exports = @import("static_data");
const object_cache = @import("boxy_object_cache.zig");
const boxy_interface = @import("boxy_interface.zig");
const cache_manager_mod = @import("cache_manager.zig");
const CacheData = @import("cache_module.zig").CacheModule.CacheData;

const Allocator = std.mem.Allocator;
const CacheManager = cache_manager_mod.CacheManager;
const Artifact = check.CheckedArtifact.CheckedModuleArtifact;
const ImportedView = check.CheckedArtifact.ImportedModuleView;
const ExportInterfaces = eval.CompileTimeFinalization.ExportInterfaces;
const dev_diagnostics = base.dev_diagnostics;

/// A library unit needs test instrumentation only if its producer recorded
/// expect nodes or explicit test roots. Imported workers belong to their own
/// units and select their own mode. Paired root units must keep their explicit
/// test mode, since they also lower the paired platform modules' bodies.
pub fn requiresTestUnit(artifact: *const Artifact) bool {
    if (artifact.has_expects) return true;
    for (artifact.root_requests.requests) |root| {
        if (root.kind == .test_expect) return true;
    }
    return false;
}

/// Everything one unit's object is built from.
pub const Unit = struct {
    gpa: Allocator,
    artifact: *const Artifact,
    /// The platform module whose hosted section binds the hosted functions
    /// the module declares, when it declares any.
    platform: ?ImportedView,
    /// The module's imports.
    views: []const ImportedView,
    target: roc_target.RocTarget,
    /// The lowering the object is compiled under, `boxy_unit` included.
    config: lir.CheckedPipeline.TargetConfig,
    interfaces: *ExportInterfaces,
    cache_manager: ?*CacheManager,
    key: object_cache.Key,
    /// Whether the caller reads the unit's pieces. A fast dev link, and
    /// compile-time evaluation on a host that loads unit images, read only
    /// the unit image, and a restored entry is then served from a mapping
    /// without decoding its pieces.
    want_pieces: bool = true,
    /// Whether this is a test unit (`roc test`, design.md "Unit-Image Test
    /// Execution"): `config.boxy_unit.expect_sites` is set, so every inline
    /// expect it lowers counts into a site of its expect-site table, and it
    /// exports a wrapper per top-level expect of its module.
    test_unit: bool = false,
    /// For a wasm target: whether the platform reserves low memory, so the
    /// unit's code checks its stack bounds (`object_cache.KeyInput.wasm_stack_checks`).
    wasm_stack_checks: bool = false,
};

/// The kind of unit image a unit compiled for a target carries.
pub const ImageKind = enum {
    /// None: the unit's code is its pieces alone.
    none,
    /// A `backend.dev.UnitImage` beside the unit's pieces.
    native,
    /// A `backend.wasm.WasmUnitImage`, which is all of the unit's code.
    wasm,
};

/// The kind of unit image a unit compiled for `target` carries. Every
/// entry for the target holds one, whichever command wrote it.
pub fn imageKind(target: roc_target.RocTarget) ImageKind {
    if (target.toCpuArch() == .wasm32) return .wasm;
    const arch = target.toCpuArch();
    if (arch != .x86_64 and arch != .aarch64) return .none;
    return if (backend.dev.imageOs(target.toOsTag())) .native else .none;
}

/// Whether a unit compiled for `target` carries a native unit image: the
/// targets a fast dev link serves and compile-time evaluation loads.
pub fn producesImage(target: roc_target.RocTarget) bool {
    return imageKind(target) == .native;
}

/// A unit image's bytes, its code locations, and what keeps them alive.
pub const Image = struct {
    bytes: []align(backend.dev.UnitImage.alignment.toByteUnits()) const u8,
    /// Where the image's code came from (`backend.dev.CodeLocations`), as
    /// the cache entry files it beside the image
    /// (`object_cache.Entry.code_locations`).
    code_locations: []const u8,
    owner: union(enum) {
        /// Both are allocated from the unit's allocator.
        owned,
        /// Both are slices of this mapped cache entry.
        mapped: CacheData,
    },

    pub fn deinit(self: Image, gpa: Allocator) void {
        switch (self.owner) {
            .owned => {
                gpa.free(self.bytes);
                gpa.free(self.code_locations);
            },
            .mapped => |data| data.deinit(gpa),
        }
    }
};

/// A unit's object.
pub const Built = struct {
    restored: bool,
    function_metadata: []const u8 = &.{},
    /// The unit's code in pieces, when the unit wanted them and its target
    /// has pieces (every target but wasm). Owned by the unit's `gpa`.
    pieces: ?*backend.dev.UnitPieces.Pieces,
    /// The unit's image, for a target with an `imageKind`.
    image: ?Image = null,
    hosted_symbols: []const []const u8,
    /// The ownership signatures the object exports. Owned by the unit's `gpa`.
    exports: []const object_cache.ExportRecord,
    /// A test unit's expect-site table and test-root table. Owned by the
    /// unit's `gpa`.
    expect_sites: []const object_cache.ExpectSiteRecord = &.{},
    test_roots: []const object_cache.TestRootRecord = &.{},

    /// Take the decoded entry's owned metadata after code restoration succeeds.
    /// On allocation failure, the caller still owns every input.
    fn fromRestoredEntry(gpa: Allocator, entry: *object_cache.Entry, pieces: ?*backend.dev.UnitPieces.Pieces, image: ?Image) Allocator.Error!Built {
        // Metadata borrows the mapped bytes and requires independent ownership.
        // Complete every fallible operation before transferring the owned data.
        const function_metadata = try gpa.dupe(u8, entry.function_metadata);
        const built: Built = .{
            .restored = true,
            .function_metadata = function_metadata,
            .pieces = pieces,
            .image = image,
            .exports = entry.exports,
            .hosted_symbols = entry.hosted_symbols,
            .expect_sites = entry.expect_sites,
            .test_roots = entry.test_roots,
        };
        entry.exports = &.{};
        entry.hosted_symbols = &.{};
        entry.expect_sites = &.{};
        entry.test_roots = &.{};
        return built;
    }

    pub fn deinit(self: Built, gpa: Allocator) void {
        if (self.pieces) |pieces| {
            pieces.deinit();
            gpa.destroy(pieces);
        }
        if (self.image) |image| image.deinit(gpa);
        self.deinitKeepingCode(gpa);
    }

    /// Release everything but the pieces and the image, which the caller
    /// now owns.
    pub fn deinitKeepingCode(self: Built, gpa: Allocator) void {
        gpa.free(self.function_metadata);
        for (self.exports) |record| {
            gpa.free(record.rc_ret_conditions);
            gpa.free(record.table_needs);
        }
        gpa.free(self.exports);
        for (self.hosted_symbols) |symbol| gpa.free(symbol);
        gpa.free(self.hosted_symbols);
        gpa.free(self.expect_sites);
        gpa.free(self.test_roots);
    }
};

/// The key of `artifact`'s object. A module declaring hosted functions is
/// lowered against the platform header's hosted section, which binds them to
/// their host symbols, so its key covers a digest of that section too.
/// `paired_platforms` are the code-generation keys of the root program's
/// platform modules, including their exact app bindings: the root unit lowers
/// their implementations at the app's types and calls its supplied workers.
/// A library module's object is a function of the module and its imports'
/// interfaces alone, whatever program pairs it.
/// `run_image` marks the root unit of a dev run image
/// (`object_cache.KeyInput.run_image`), and `expect_sites` a test unit
/// (`object_cache.KeyInput.expect_sites`).
/// `wasm_stack_checks` is the platform's stack-check mode on a wasm target.
pub fn objectKey(gpa: Allocator, digests: *boxy_interface.Digests, artifact: *const Artifact, platform: ?ImportedView, opt: []const u8, inline_expects: bool, target: roc_target.RocTarget, paired_platforms: []const [32]u8, run_image: bool, expect_sites: bool, wasm_stack_checks: bool) Allocator.Error!object_cache.Key {
    std.debug.assert(!wasm_stack_checks or imageKind(target) == .wasm);
    const hosted_bindings: ?[32]u8 = if (platform) |platform_view| blk: {
        const inputs = try gpa.alloc(object_cache.HostedBindingInput, platform_view.hosted_bindings.bindings.len);
        defer gpa.free(inputs);
        for (platform_view.hosted_bindings.bindings, inputs) |binding, *input| input.* = .{
            .target_module = binding.target_checked_module.bytes,
            .target_def = @backingInt(binding.target_def),
            .symbol = platform_view.canonical_names.externalSymbolNameText(binding.external_symbol_name),
        };
        break :blk object_cache.hostedBindingsDigest(inputs);
    } else null;
    var input = base.Sha256.init(.{});
    input.update(&try digests.objectInput(artifact));
    for (paired_platforms) |*paired| input.update(paired);
    return object_cache.key(.{
        .object_input = input.finalResult(),
        .hosted_bindings = hosted_bindings,
        .target = @tagName(target),
        .opt = opt,
        .inline_expects = inline_expects,
        .run_image = run_image,
        .expect_sites = expect_sites,
        .wasm_stack_checks = wasm_stack_checks,
    });
}

/// Restore `key`'s entry, a unit compiled for `target`, from the per-module
/// Boxy object cache, recording its exports in `interfaces`. Null for a
/// miss, or for bytes this compiler did not write.
pub fn restore(gpa: Allocator, cache_manager: ?*CacheManager, key: object_cache.Key, target: roc_target.RocTarget, interfaces: *ExportInterfaces) Allocator.Error!?object_cache.Entry {
    const manager = cache_manager orelse {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: no cache manager to restore key {s}\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        return null;
    };
    const bytes = manager.loadFamilyBytes(.boxy_object, key.bytes, object_cache.scope) orelse {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: no object cache entry for key {s}\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        return null;
    };
    defer gpa.free(bytes);
    const entry = object_cache.decode(gpa, bytes) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidEntry => {
            if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache entry for key {s} is not an entry ({d} bytes)\n", .{ &std.fmt.bytesToHex(key.bytes[0..8], .lower), bytes.len });
            return null;
        },
    };
    errdefer object_cache.deinitEntry(gpa, entry);
    if (!imageReads(entry, imageKind(target))) {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache entry for key {s} holds no image this compiler reads\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        object_cache.deinitEntry(gpa, entry);
        return null;
    }
    try recordInterfaces(interfaces, entry);
    return entry;
}

/// Whether `entry`'s image is an image of `kind` this compiler reads, and
/// its code locations, if it has them, a table this compiler reads. A
/// native library unit's entry carries both; a native root unit's, which no
/// compile-time evaluation loads, only the image; a wasm unit's only its
/// wasm image, which is all of its code. An entry that does not read is
/// bytes this compiler did not write (a miss), never a link-time or
/// compile-time evaluation failure.
fn imageReads(entry: object_cache.Entry, kind: ImageKind) bool {
    const function_metadata: ?lir.CheckedPipeline.BoxyFunctionMetadata.MetadataView = if (entry.function_metadata.len != 0) metadata: {
        if (kind != .native or entry.image.len == 0) return false;
        break :metadata lir.CheckedPipeline.BoxyFunctionMetadata.MetadataView.init(entry.function_metadata) catch return false;
    } else null;
    switch (kind) {
        .none => return entry.image.len == 0 and entry.code_locations.len == 0,
        .native => {
            if (entry.image.len == 0) return entry.code_locations.len == 0;
            const image: []align(backend.dev.UnitImage.alignment.toByteUnits()) const u8 = @alignCast(entry.image);
            const view = backend.dev.UnitImage.View.init(image) catch return false;
            if (function_metadata) |metadata| {
                var functions = metadata.functions();
                const symbols = view.symbols();
                while (functions.next()) |function| {
                    if (function.symbol >= symbols.len) return false;
                    const symbol = symbols[function.symbol];
                    if (symbol.type != @backingInt(backend.dev.UnitImage.SymbolType.func)) return false;
                    var name: [lir.LIR.ProcIdentity.symbol_name_len]u8 = undefined;
                    if (!std.mem.eql(u8, view.name(symbol.name), function.identity.writeSymbolName(&name))) return false;
                }
            }
            return entry.code_locations.len == 0 or backend.dev.CodeLocations.validate(entry.code_locations);
        },
        .wasm => {
            if (entry.pieces.len != 0 or entry.code_locations.len != 0) return false;
            const image: []align(backend.wasm.WasmUnitImage.alignment.toByteUnits()) const u8 = @alignCast(entry.image);
            _ = backend.wasm.WasmUnitImage.View.init(image) catch return false;
            return true;
        },
    }
}

/// Restore `key`'s entry like `restore`, serving it from a mapping of the
/// cache file: the pieces are left undecoded and the image is borrowed from
/// the mapping, which the returned `Image` keeps alive.
fn restoreMapped(gpa: Allocator, cache_manager: ?*CacheManager, key: object_cache.Key, target: roc_target.RocTarget, interfaces: *ExportInterfaces, function_metadata: bool) Allocator.Error!?struct { entry: object_cache.Entry, data: CacheData } {
    const manager = cache_manager orelse return null;
    const data = manager.loadFamilyMapped(gpa, .checked, .boxy_object, key.bytes, object_cache.scope) orelse {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: no object cache entry for key {s}\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        return null;
    };
    const entry = object_cache.decodeView(gpa, data.data()) catch |err| switch (err) {
        error.OutOfMemory => {
            data.deinit(gpa);
            return error.OutOfMemory;
        },
        error.InvalidEntry => {
            data.deinit(gpa);
            return null;
        },
    };
    errdefer {
        object_cache.deinitEntryView(gpa, entry);
        data.deinit(gpa);
    }
    if ((entry.function_metadata.len != 0) != function_metadata or !imageReads(entry, imageKind(target))) {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache entry for key {s} holds no image this compiler reads\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        object_cache.deinitEntryView(gpa, entry);
        data.deinit(gpa);
        return null;
    }
    try recordInterfaces(interfaces, entry);
    return .{ .entry = entry, .data = data };
}

fn recordInterfaces(interfaces: *ExportInterfaces, entry: object_cache.Entry) Allocator.Error!void {
    for (entry.exports) |record| try interfaces.put(.{ .bytes = record.identity }, .{
        .rc_borrowed_params = record.rc_borrowed_params,
        .rc_ret_borrowed = record.rc_ret_borrowed,
        .rc_ret_lenders = record.rc_ret_lenders,
        .rc_read_only_params = record.rc_read_only_params,
        .rc_ret_unique = record.rc_ret_unique,
        .rc_ret_unique_fields = record.rc_ret_unique_fields,
        .rc_ret_conditions = record.rc_ret_conditions,
        .may_leave_pending = record.may_leave_pending,
        .table_needs = record.table_needs,
    });
}

/// The export records of `lowered`'s exported procedures, recorded in
/// `interfaces`. The caller owns the result.
pub fn exportRecords(gpa: Allocator, lowered: *const lir.CheckedPipeline.LoweredProgram, interfaces: *ExportInterfaces) Allocator.Error![]object_cache.ExportRecord {
    const procs = &lowered.lir_result.store;
    const records = try gpa.alloc(object_cache.ExportRecord, lowered.lir_result.exported_procs.items.len);
    errdefer gpa.free(records);
    for (lowered.lir_result.exported_procs.items, records) |proc, *record| {
        const identity = procs.getProcSpec(proc).identity;
        const interface = try interfaces.putProc(procs, proc, lowered.lir_result.boxy_table_needs.get(identity) orelse &.{});
        record.* = .{
            .identity = procs.getProcSpec(proc).identity.bytes,
            .rc_borrowed_params = interface.rc_borrowed_params,
            .rc_ret_lenders = interface.rc_ret_lenders,
            .rc_read_only_params = interface.rc_read_only_params,
            .rc_ret_unique_fields = interface.rc_ret_unique_fields,
            .rc_ret_borrowed = interface.rc_ret_borrowed,
            .rc_ret_unique = interface.rc_ret_unique,
            .may_leave_pending = interface.may_leave_pending,
            .rc_ret_conditions = interface.rc_ret_conditions,
            .table_needs = interface.table_needs,
        };
    }
    return records;
}

/// File `entry` in the per-module Boxy object cache under `key`.
pub fn store(gpa: Allocator, cache_manager: ?*CacheManager, key: object_cache.Key, entry: object_cache.Entry) Allocator.Error!void {
    const manager = cache_manager orelse {
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: no cache manager to store key {s}\n", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        return;
    };
    const bytes = try object_cache.encode(gpa, entry);
    defer gpa.free(bytes);
    manager.storeFamilyBytes(.boxy_object, key.bytes, bytes, object_cache.scope, "boxy object");
    if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: stored object cache entry for key {s} ({d} bytes)\n", .{ &std.fmt.bytesToHex(key.bytes[0..8], .lower), bytes.len });
}

/// The distinct hosted symbols `store_` references. Each is duplicated into
/// `arena`: a diagnostic naming them renders after the program is gone.
pub fn hostedSymbols(arena: Allocator, store_: *const lir.LirStore) Allocator.Error![]const []const u8 {
    var seen = std.StringHashMap(void).init(arena);
    var symbols = std.ArrayList([]const u8).empty;
    for (store_.getProcSpecs()) |spec| {
        const hosted = spec.hosted orelse continue;
        const text = store_.getString(hosted.symbol);
        const gop = try seen.getOrPut(text);
        if (!gop.found_existing) try symbols.append(arena, try arena.dupe(u8, text));
    }
    return symbols.items;
}

fn freeSymbols(gpa: Allocator, symbols: []const []const u8) void {
    for (symbols) |symbol| gpa.free(symbol);
    gpa.free(symbols);
}

/// Copies of `symbols`, each and the slice allocated from `gpa`.
fn dupeSymbols(gpa: Allocator, symbols: []const []const u8) Allocator.Error![]const []const u8 {
    const copies = try gpa.alloc([]const u8, symbols.len);
    errdefer gpa.free(copies);
    var duped: usize = 0;
    errdefer for (copies[0..duped]) |copy| gpa.free(copy);
    for (symbols, copies) |symbol, *copy| {
        copy.* = try gpa.dupe(u8, symbol);
        duped += 1;
    }
    return copies;
}

/// Why a unit's object could not be built.
pub const BuildError = eval.CompileTimeFinalization.RuntimeMaterializationError || static_data_exports.MaterializationError || error{NativeCompilationFailed};

/// `unit`'s object, from the cache or built and filed there.
pub fn restoreOrBuild(unit: Unit) BuildError!Built {
    const gpa = unit.gpa;
    const module_name = unit.artifact.canonical_names.moduleNameText(unit.artifact.module_identity.module_name);
    var restore_scope = lir.CheckedPipeline.PipelineStageScope.begin(unit.config.timing, .boxy_unit_restore);
    defer restore_scope.end();
    if (try restoreMapped(gpa, unit.cache_manager, unit.key, unit.target, unit.interfaces, unit.config.boxy_function_unit != null)) |restored| {
        // The view's owned export records, symbols, and test tables move into
        // the returned `Built`; the view releases its remaining allocations.
        var entry = restored.entry;
        defer object_cache.deinitEntryView(gpa, entry);
        // The mapping stays alive as the image's owner; otherwise it is
        // released once the pieces are decoded.
        const keeps_mapping = imageKind(unit.target) != .none;
        defer if (!keeps_mapping) restored.data.deinit(gpa);
        errdefer if (keeps_mapping) restored.data.deinit(gpa);
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache hit for {s} key {s}\n", .{ module_name, &std.fmt.bytesToHex(unit.key.bytes[0..8], .lower) });
        std.debug.assert(entry.image.len != 0 or !keeps_mapping);
        if (dev_diagnostics.isSet(.verify_boxy_object_reuse)) {
            const fresh = try build(unit, module_name);
            defer fresh.deinit(gpa);
            if (fresh.pieces != null and unit.config.boxy_function_unit == null) {
                const fresh_pieces = fresh.pieces.?;
                const encoded = try backend.dev.UnitPieces.encode(gpa, fresh_pieces);
                defer gpa.free(encoded);
                if (!std.mem.eql(u8, encoded, entry.pieces)) {
                    base.invariant("the cached pieces of module {s} differ from a fresh build of them: its key misses an input", .{module_name});
                }
            }
            if (!std.mem.eql(u8, fresh.function_metadata, entry.function_metadata)) {
                base.invariant("the cached function metadata of module {s} differs from a fresh build: its key misses an input", .{module_name});
            }
            if (fresh.image) |image| if (!std.mem.eql(u8, image.bytes, entry.image)) {
                base.invariant("the cached image of module {s} differs from a fresh build of it: its key misses an input", .{module_name});
            };
        }
        const pieces: ?*backend.dev.UnitPieces.Pieces = if (unit.want_pieces and imageKind(unit.target) != .wasm) pieces: {
            const decoded = try gpa.create(backend.dev.UnitPieces.Pieces);
            errdefer gpa.destroy(decoded);
            decoded.* = backend.dev.UnitPieces.decode(gpa, entry.pieces) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.MalformedPieces, error.UnsupportedPiecesVersion => base.invariant("the cached pieces of module {s} were admitted but do not decode: {s}", .{ module_name, @errorName(err) }),
            };
            break :pieces decoded;
        } else null;
        errdefer if (pieces) |decoded| {
            decoded.deinit();
            gpa.destroy(decoded);
        };
        return try Built.fromRestoredEntry(
            gpa,
            &entry,
            pieces,
            if (keeps_mapping) .{ .bytes = @alignCast(entry.image), .code_locations = entry.code_locations, .owner = .{ .mapped = restored.data } } else null,
        );
    }
    restore_scope.end();
    if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache miss for {s} key {s}\n", .{ module_name, &std.fmt.bytesToHex(unit.key.bytes[0..8], .lower) });
    const built = try build(unit, module_name);
    errdefer built.deinit(gpa);
    var cache_scope = lir.CheckedPipeline.PipelineStageScope.begin(unit.config.timing, .boxy_unit_cache);
    defer cache_scope.end();
    // Compiler-only native units are consumed exclusively as images. Their
    // extraction pieces are needed while building, never while restoring.
    const encoded = if (built.pieces != null and unit.config.boxy_function_unit == null) try backend.dev.UnitPieces.encode(gpa, built.pieces.?) else &.{};
    defer gpa.free(encoded);
    try store(gpa, unit.cache_manager, unit.key, .{
        .exports = built.exports,
        .hosted_symbols = built.hosted_symbols,
        .pieces = encoded,
        .function_metadata = built.function_metadata,
        .code_locations = if (built.image) |image| image.code_locations else &.{},
        .image = if (built.image) |image| image.bytes else &.{},
        .expect_sites = built.expect_sites,
        .test_roots = built.test_roots,
    });
    if (unit.want_pieces or built.pieces == null) return built;
    var without_pieces = built;
    built.pieces.?.deinit();
    gpa.destroy(built.pieces.?);
    without_pieces.pieces = null;
    return without_pieces;
}

/// Lower and compile `unit`.
fn build(unit: Unit, module_name: []const u8) BuildError!Built {
    const gpa = unit.gpa;
    const trace_clock: ?std.Io = if (dev_diagnostics.isSet(.trace_boxy_work)) (if (unit.config.timing) |timing| timing.std_io else null) else null;
    const build_started_ns: i96 = if (trace_clock) |io| std.Io.Timestamp.now(io, .awake).nanoseconds else 0;
    var views: std.ArrayList(ImportedView) = .empty;
    defer views.deinit(gpa);
    try views.appendSlice(gpa, unit.views);
    if (unit.platform) |platform_view| {
        const present = for (unit.views) |view| {
            if (std.meta.eql(view.key, platform_view.key)) break true;
        } else false;
        if (!present) try views.append(gpa, platform_view);
    }
    const modules: lir.CheckedPipeline.CheckedModuleSet = .{
        .root = check.CheckedArtifact.loweringViewWithRelations(unit.artifact, &.{}),
        .imports = views.items,
    };
    std.debug.assert(unit.test_unit == (if (unit.config.boxy_unit) |module_unit| module_unit.expect_sites else false));
    // A test unit lowers its module's top-level expects as roots too.
    const test_requests = if (unit.test_unit) try testRootRequests(gpa, unit.artifact) else &.{};
    defer gpa.free(test_requests);
    const test_metadata = try testPlanMetadata(gpa, test_requests);
    defer gpa.free(test_metadata);
    var lowered = try eval.CompileTimeFinalization.lowerBoxyRuntime(gpa, modules, .{ .requests = test_requests, .test_plan_metadata = test_metadata }, unit.config);
    defer lowered.deinit();
    const module_digest = if (unit.test_unit) unit.config.boxy_unit.?.names.of(check.CheckedArtifact.importedView(unit.artifact)) else @as([32]u8, @splat(0));
    const test_modules = try gpa.alloc([32]u8, test_requests.len);
    defer gpa.free(test_modules);
    @memset(test_modules, module_digest);
    const test_root_indices = try gpa.alloc(u32, test_requests.len);
    defer gpa.free(test_root_indices);
    for (test_root_indices, 0..) |*index, position| index.* = @intCast(position);
    var wrappers = try testEntrypoints(gpa, &lowered, test_modules, test_root_indices);
    defer wrappers.deinit(gpa);
    if (trace_clock) |io| std.debug.print("boxy-work: lowered {s} ({d} procs, {d} ms)\n", .{ module_name, lowered.lir_result.store.procSpecCount(), @divTrunc(std.Io.Timestamp.now(io, .awake).nanoseconds - build_started_ns, std.time.ns_per_ms) }) else if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: lowered {s} ({d} procs)\n", .{ module_name, lowered.lir_result.store.procSpecCount() });

    const static_data = try static_data_exports.buildStaticData(gpa, .{ .root = modules.root, .imports = views.items }, &lowered, unit.target, .{});
    defer static_data_exports.deinitStaticData(gpa, static_data);

    var codegen_scope = lir.CheckedPipeline.PipelineStageScope.begin(unit.config.timing, .boxy_unit_codegen);
    defer codegen_scope.end();
    if (imageKind(unit.target) == .wasm) {
        std.debug.assert(!unit.test_unit);
        const bytes = backend.wasm.UnitCompile.compileUnitImage(gpa, &lowered.lir_result.store, &lowered.lir_result.layouts, &.{}, static_data, .{
            .cpu_level = unit.target.cpuLevel(),
            .stack_checks = unit.wasm_stack_checks,
            .executor = unit.config.post_check_executor,
        }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                std.log.err("Wasm compilation of module {s} failed: {}", .{ module_name, err });
                return error.NativeCompilationFailed;
            },
        };
        errdefer gpa.free(bytes);
        codegen_scope.end();
        const records = try exportRecords(gpa, &lowered, unit.interfaces);
        defer gpa.free(records);
        var hosted_arena = std.heap.ArenaAllocator.init(gpa);
        defer hosted_arena.deinit();
        const hosted_symbols = try dupeSymbols(gpa, try hostedSymbols(hosted_arena.allocator(), &lowered.lir_result.store));
        errdefer freeSymbols(gpa, hosted_symbols);
        if (trace_clock) |io| std.debug.print("boxy-work: built wasm unit {s} in {d} ms ({d} image bytes)\n", .{ module_name, @divTrunc(std.Io.Timestamp.now(io, .awake).nanoseconds - build_started_ns, std.time.ns_per_ms), bytes.len });
        return .{
            .restored = false,
            .pieces = null,
            .image = .{ .bytes = bytes, .code_locations = &.{}, .owner = .owned },
            .hosted_symbols = hosted_symbols,
            .exports = try dupeRecords(gpa, records),
        };
    }
    var object_compiler = backend.ObjectFileCompiler.init(gpa);
    var backend_timing: ?backend.ObjectFileCompiler.Timing = if (trace_clock) |io| .init(io) else null;
    if (backend_timing) |*timing| object_compiler.timing = timing;
    object_compiler.erased_capture_prefix = lowered.lir_result.erased_capture_prefix;
    object_compiler.link_unit = true;
    object_compiler.post_check_executor = unit.config.post_check_executor;
    // The command calls a test unit's wrappers in its own process.
    object_compiler.internal_entrypoints = unit.test_unit;
    object_compiler.relocatable_expect_sites = unit.test_unit;
    const pieces = try gpa.create(backend.dev.UnitPieces.Pieces);
    errdefer gpa.destroy(pieces);
    pieces.* = object_compiler.compileToPieces(
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        wrappers.entrypoints,
        static_data,
        lowered.lir_result.store.getProcSpecs(),
        unit.target,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            std.log.err("Native compilation of module {s} failed: {}", .{ module_name, err });
            return error.NativeCompilationFailed;
        },
    };
    errdefer pieces.deinit();
    // The image goes into the unit's cache entry, which later fast dev links
    // read, and to this build's own link when it wants images rather than
    // pieces; a build with neither has no use for it.
    const image: ?Image = if (producesImage(unit.target) and (unit.cache_manager != null or !unit.want_pieces)) image: {
        var code_locations: []const u8 = &.{};
        const bytes = object_compiler.buildUnitImage(pieces, unit.target, .every_procedure, &code_locations) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                std.log.err("Building the unit image of module {s} failed: {}", .{ module_name, err });
                return error.NativeCompilationFailed;
            },
        };
        break :image .{ .bytes = bytes, .code_locations = code_locations, .owner = .owned };
    } else null;
    errdefer if (image) |owned| owned.deinit(gpa);
    codegen_scope.end();
    if (backend_timing) |timing| {
        const snapshot = timing.snapshot();
        std.debug.print("boxy-work: compiled {s}: setup {d} ms, procedures {d} ms ({d} tasks, {d} in flight at most, {d} workers), extraction {d} ms, encoding {d} ms\n", .{
            module_name,
            snapshot.backend_setup_ns / std.time.ns_per_ms,
            snapshot.procedure_instructions_ns / std.time.ns_per_ms,
            snapshot.native_emission.tasks_submitted,
            snapshot.native_emission.peak_inflight_fragments,
            if (unit.config.post_check_executor) |executor| executor.worker_count else 0,
            snapshot.symbol_relocations_ns / std.time.ns_per_ms,
            snapshot.object_encoding_ns / std.time.ns_per_ms,
        });
    }

    const function_metadata: []const u8 = if (unit.config.boxy_function_unit != null) metadata: {
        var compiled: std.ArrayList(lir.CheckedPipeline.BoxyFunctionMetadata.CompiledFunction) = .empty;
        defer compiled.deinit(gpa);
        const view = backend.dev.UnitImage.View.init(image.?.bytes) catch unreachable; // buildUnitImage just emitted these bytes from the compiled pieces
        var symbols: std.StringHashMapUnmanaged(u32) = .empty;
        defer symbols.deinit(gpa);
        for (view.symbols(), 0..) |symbol, index| if (symbol.type == @backingInt(backend.dev.UnitImage.SymbolType.func)) {
            try symbols.put(gpa, view.name(symbol.name), @intCast(index));
        };
        for (pieces.set.excerpts) |excerpt| switch (excerpt.kind) {
            .proc => |identity| {
                var name: [lir.LIR.ProcIdentity.symbol_name_len]u8 = undefined;
                const symbol = symbols.get(identity.writeSymbolName(&name)) orelse unreachable; // every procedure excerpt is emitted as a function symbol by buildUnitImage
                try compiled.append(gpa, .{ .identity = identity, .symbol = symbol });
            },
            .rc_helper, .entrypoint, .message_pool_run, .branch_island => {},
        };
        break :metadata try lir.CheckedPipeline.BoxyFunctionMetadata.encodeMetadata(lowered.boxy_functions.?, &lowered.lir_result, compiled.items);
    } else &.{};
    errdefer gpa.free(function_metadata);
    const records = try exportRecords(gpa, &lowered, unit.interfaces);
    defer gpa.free(records);
    var hosted_arena = std.heap.ArenaAllocator.init(gpa);
    defer hosted_arena.deinit();
    const hosted = try hostedSymbols(hosted_arena.allocator(), &lowered.lir_result.store);
    const expect_sites = if (unit.test_unit) try expectSiteRecords(gpa, &lowered) else &.{};
    errdefer gpa.free(expect_sites);
    // A test unit runs only from its image, which a run with a cache has.
    if (unit.test_unit and image == null) base.invariant("a test unit of module {s} was built without its image", .{module_name});
    const test_roots = if (unit.test_unit) try testRootRecords(gpa, image.?.bytes, test_modules, test_root_indices) else &.{};
    errdefer gpa.free(test_roots);
    const hosted_symbols = try dupeSymbols(gpa, hosted);
    errdefer freeSymbols(gpa, hosted_symbols);
    if (trace_clock) |io| std.debug.print("boxy-work: built {s} in {d} ms\n", .{ module_name, @divTrunc(std.Io.Timestamp.now(io, .awake).nanoseconds - build_started_ns, std.time.ns_per_ms) });
    return .{
        .restored = false,
        .function_metadata = function_metadata,
        .pieces = pieces,
        .image = image,
        .hosted_symbols = hosted_symbols,
        .exports = try dupeRecords(gpa, records),
        .expect_sites = expect_sites,
        .test_roots = test_roots,
    };
}

/// A module's top-level expects, in source order: the roots a test unit
/// exports wrappers for, and the roots `roc test` plans for the module.
pub fn testRootRequests(gpa: Allocator, artifact: *const Artifact) Allocator.Error![]check.CheckedArtifact.RootRequest {
    var roots = std.ArrayList(check.CheckedArtifact.RootRequest).empty;
    errdefer roots.deinit(gpa);
    for (artifact.root_requests.requests) |root| {
        if (root.kind != .test_expect) continue;
        try roots.append(gpa, root);
    }
    return try roots.toOwnedSlice(gpa);
}

/// The test-plan metadata of a test unit's roots: each root's slot is its
/// own position, so the lowering is a function of the unit's sources alone.
pub fn testPlanMetadata(gpa: Allocator, requests: []const check.CheckedArtifact.RootRequest) Allocator.Error![]lir.CheckedPipeline.RootTestPlanMetadata {
    const metadata = try gpa.alloc(lir.CheckedPipeline.RootTestPlanMetadata, requests.len);
    for (requests, metadata, 0..) |request, *slot, index| slot.* = .{
        .request_index = @intCast(index),
        .root_order = request.order,
        .result_index = @intCast(index),
        .module_index = 0,
        .root_index = @intCast(index),
    };
    return metadata;
}

/// The symbol a test unit exports the wrapper of the root at `root_index`
/// among the test roots of the module `module` names under.
pub fn testWrapperName(gpa: Allocator, module: [32]u8, root_index: u32) Allocator.Error![]u8 {
    return std.fmt.allocPrint(gpa, "roc__test_{s}_{d}", .{ std.fmt.bytesToHex(module[0..16], .lower), root_index });
}

/// A test unit's root wrappers, one per lowered root in request order.
pub const TestWrappers = struct {
    entrypoints: []backend.Entrypoint = &.{},

    pub fn deinit(self: *TestWrappers, gpa: Allocator) void {
        for (self.entrypoints) |entrypoint| {
            gpa.free(entrypoint.symbol_name);
            gpa.free(entrypoint.arg_layouts);
        }
        gpa.free(self.entrypoints);
        self.* = .{};
    }
};

/// The wrapper of each of `lowered`'s roots, which are the test roots that
/// `modules` and `root_indices` name, in order.
pub fn testEntrypoints(gpa: Allocator, lowered: *const lir.CheckedPipeline.LoweredProgram, modules: []const [32]u8, root_indices: []const u32) Allocator.Error!TestWrappers {
    const root_procs = lowered.lir_result.root_procs.items;
    std.debug.assert(root_procs.len == modules.len and root_indices.len == modules.len);
    var wrappers: TestWrappers = .{ .entrypoints = try gpa.alloc(backend.Entrypoint, root_procs.len) };
    var filled: usize = 0;
    errdefer {
        wrappers.entrypoints = wrappers.entrypoints[0..filled];
        wrappers.deinit(gpa);
    }
    const store_ = &lowered.lir_result.store;
    for (root_procs, modules, root_indices, wrappers.entrypoints) |proc_id, module, root_index, *entrypoint| {
        const name = try testWrapperName(gpa, module, root_index);
        errdefer gpa.free(name);
        const proc = store_.getProcSpec(proc_id);
        const arg_ids = store_.getLocalSpan(proc.args);
        const arg_layouts = try gpa.alloc(@import("layout").Idx, arg_ids.len);
        for (arg_layouts, 0..) |*arg_layout, index| arg_layout.* = store_.getLocal(@import("collections").GuardedList.at(arg_ids, index)).layout_idx;
        entrypoint.* = .{ .symbol_name = name, .proc = proc_id, .arg_layouts = arg_layouts, .ret_layout = proc.ret_layout };
        filled += 1;
    }
    return wrappers;
}

/// `lowered`'s expect sites as a test unit files them, by site id.
pub fn expectSiteRecords(gpa: Allocator, lowered: *const lir.CheckedPipeline.LoweredProgram) Allocator.Error![]object_cache.ExpectSiteRecord {
    const sites = lowered.lir_result.expect_sites.items;
    const records = try gpa.alloc(object_cache.ExpectSiteRecord, sites.len);
    for (sites, records) |site, *record| {
        if (!site.loc.hasLocation()) base.invariant("a test unit's expect site has no source location", .{});
        record.* = .{
            .module = lowered.lir_result.store.sourceFileModuleIdentity(site.loc.file),
            .region_start = site.region.start.offset,
            .region_end = site.region.end.offset,
        };
    }
    return records;
}

/// The test-root table of a test unit whose image is `image`: where it
/// exports the wrapper of each root that `modules` and `root_indices` name.
pub fn testRootRecords(gpa: Allocator, image: []align(backend.dev.UnitImage.alignment.toByteUnits()) const u8, modules: []const [32]u8, root_indices: []const u32) Allocator.Error![]object_cache.TestRootRecord {
    const view = backend.dev.UnitImage.View.init(image) catch |err| base.invariant("a test unit's image does not read: {s}", .{@errorName(err)});
    const records = try gpa.alloc(object_cache.TestRootRecord, modules.len);
    errdefer gpa.free(records);
    for (modules, root_indices, records) |module, root_index, *record| {
        const name = try testWrapperName(gpa, module, root_index);
        defer gpa.free(name);
        const symbol = view.findExportIndex(name) orelse base.invariant("a test unit's image does not export its wrapper {s}", .{name});
        record.* = .{ .module = module, .root_index = root_index, .symbol = symbol };
    }
    return records;
}

/// Copies of `records`, each and the slice allocated from `gpa`.
fn dupeRecords(gpa: Allocator, records: []const object_cache.ExportRecord) Allocator.Error![]const object_cache.ExportRecord {
    const copies = try gpa.dupe(object_cache.ExportRecord, records);
    errdefer gpa.free(copies);
    var duped: usize = 0;
    errdefer for (copies[0..duped]) |copy| {
        gpa.free(copy.rc_ret_conditions);
        gpa.free(copy.table_needs);
    };
    for (records, copies) |record, *copy| {
        copy.rc_ret_conditions = try gpa.dupe(u32, record.rc_ret_conditions);
        errdefer gpa.free(copy.rc_ret_conditions);
        copy.table_needs = try gpa.dupe(u8, record.table_needs);
        duped += 1;
    }
    return copies;
}

test "Boxy object restoration transfers owned metadata and survives allocation failures" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testRestoredMetadata, .{});
}

fn testRestoredMetadata(gpa: Allocator) (object_cache.DecodeError || error{TestExpectedEqual})!void {
    const built = restored: {
        const bytes = try object_cache.encode(gpa, .{
            .exports = &.{.{
                .identity = @splat(3),
                .rc_borrowed_params = 1,
                .rc_ret_lenders = 0,
                .rc_read_only_params = 0,
                .rc_ret_unique_fields = 0,
                .rc_ret_borrowed = false,
                .rc_ret_unique = false,
                .may_leave_pending = false,
                .rc_ret_conditions = &.{ 7, 9 },
                .table_needs = "table inputs",
            }},
            .hosted_symbols = &.{"roc_fx_test"},
            .link_root_symbols = &.{"link-only"},
            .expect_sites = &.{.{ .module = @splat(4), .region_start = 5, .region_end = 8 }},
            .test_roots = &.{.{ .module = @splat(4), .root_index = 2, .symbol = 6 }},
            .pieces = &.{},
            .function_metadata = "function metadata",
        });
        defer gpa.free(bytes);
        var entry = try object_cache.decodeView(gpa, bytes);
        defer object_cache.deinitEntryView(gpa, entry);
        const exports = entry.exports;
        const symbols = entry.hosted_symbols;
        const sites = entry.expect_sites;
        const roots = entry.test_roots;
        const result = try Built.fromRestoredEntry(gpa, &entry, null, null);
        errdefer result.deinit(gpa);
        try std.testing.expectEqual(exports.ptr, result.exports.ptr);
        try std.testing.expectEqual(symbols.ptr, result.hosted_symbols.ptr);
        try std.testing.expectEqual(sites.ptr, result.expect_sites.ptr);
        try std.testing.expectEqual(roots.ptr, result.test_roots.ptr);
        break :restored result;
    };
    defer built.deinit(gpa);
    // Both the decoded view and its byte buffer are gone; Built owns its data.
    try std.testing.expectEqualStrings("table inputs", built.exports[0].table_needs);
    try std.testing.expectEqualSlices(u32, &.{ 7, 9 }, built.exports[0].rc_ret_conditions);
    try std.testing.expectEqualStrings("roc_fx_test", built.hosted_symbols[0]);
    try std.testing.expectEqual(@as(u32, 5), built.expect_sites[0].region_start);
    try std.testing.expectEqual(@as(u32, 6), built.test_roots[0].symbol);
    try std.testing.expectEqualStrings("function metadata", built.function_metadata);
}
