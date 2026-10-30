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

const Allocator = std.mem.Allocator;
const CacheManager = cache_manager_mod.CacheManager;
const Artifact = check.CheckedArtifact.CheckedModuleArtifact;
const ImportedView = check.CheckedArtifact.ImportedModuleView;
const ExportInterfaces = eval.CompileTimeFinalization.ExportInterfaces;
const dev_diagnostics = base.dev_diagnostics;

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
};

/// A unit's object.
pub const Built = struct {
    restored: bool,
    /// Owned by the unit's `gpa`.
    object: []const u8,
    /// The object's encoded `CodeLocations`. Owned by the unit's `gpa`.
    code_locations: []const u8,
    hosted_symbols: []const []const u8,
    /// The ownership signatures the object exports. Owned by the unit's `gpa`.
    exports: []const object_cache.ExportRecord,

    pub fn deinit(self: Built, gpa: Allocator) void {
        gpa.free(self.object);
        gpa.free(self.code_locations);
        for (self.exports) |record| gpa.free(record.rc_ret_conditions);
        gpa.free(self.exports);
        for (self.hosted_symbols) |symbol| gpa.free(symbol);
        gpa.free(self.hosted_symbols);
    }
};

/// The key of `artifact`'s object. A module declaring hosted functions is
/// lowered against the platform header's hosted section, which binds them to
/// their host symbols, so its key covers a digest of that section too.
/// `paired_platforms` are the checked keys of the root program's platform
/// modules, whose implementations the root unit lowers at the app's types.
/// A library module's object is a function of the module and its imports'
/// interfaces alone, whatever program pairs it.
pub fn objectKey(gpa: Allocator, digests: *boxy_interface.Digests, artifact: *const Artifact, platform: ?ImportedView, opt: []const u8, inline_expects: bool, target: roc_target.RocTarget, paired_platforms: []const [32]u8) Allocator.Error!object_cache.Key {
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
    });
}

/// Restore `key`'s entry from the per-module Boxy object cache, recording
/// its exports in `interfaces`. Null for a miss, or for bytes this compiler
/// did not write.
pub fn restore(gpa: Allocator, cache_manager: ?*CacheManager, key: object_cache.Key, interfaces: *ExportInterfaces) Allocator.Error!?object_cache.Entry {
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
    for (entry.exports) |record| try interfaces.put(.{ .bytes = record.identity }, .{
        .rc_borrowed_params = record.rc_borrowed_params,
        .rc_ret_borrowed = record.rc_ret_borrowed,
        .rc_ret_lenders = record.rc_ret_lenders,
        .rc_read_only_params = record.rc_read_only_params,
        .rc_ret_unique = record.rc_ret_unique,
        .rc_ret_unique_fields = record.rc_ret_unique_fields,
        .rc_ret_conditions = record.rc_ret_conditions,
        .may_leave_pending = record.may_leave_pending,
    });
    return entry;
}

/// The export records of `lowered`'s exported procedures, recorded in
/// `interfaces`. The caller owns the result.
pub fn exportRecords(gpa: Allocator, lowered: *const lir.CheckedPipeline.LoweredProgram, interfaces: *ExportInterfaces) Allocator.Error![]object_cache.ExportRecord {
    const procs = &lowered.lir_result.store;
    const records = try gpa.alloc(object_cache.ExportRecord, lowered.lir_result.exported_procs.items.len);
    errdefer gpa.free(records);
    for (lowered.lir_result.exported_procs.items, records) |proc, *record| {
        const interface = try interfaces.putProc(procs, proc);
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
    if (try restore(gpa, unit.cache_manager, unit.key, unit.interfaces)) |entry| {
        defer object_cache.deinitEntry(gpa, entry);
        if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache hit for {s} key {s}\n", .{ module_name, &std.fmt.bytesToHex(unit.key.bytes[0..8], .lower) });
        if (dev_diagnostics.isSet(.verify_boxy_object_reuse)) {
            const fresh = try build(unit, module_name);
            defer fresh.deinit(gpa);
            if (!std.mem.eql(u8, fresh.object, entry.object)) {
                base.invariant("the cached object of module {s} differs from a fresh build of it: its key misses an input", .{module_name});
            }
        }
        return .{
            .restored = true,
            .object = try gpa.dupe(u8, entry.object),
            .code_locations = try gpa.dupe(u8, entry.code_locations),
            .hosted_symbols = try dupeSymbols(gpa, entry.hosted_symbols),
            .exports = try dupeRecords(gpa, entry.exports),
        };
    }
    if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: object cache miss for {s} key {s}\n", .{ module_name, &std.fmt.bytesToHex(unit.key.bytes[0..8], .lower) });
    const built = try build(unit, module_name);
    errdefer built.deinit(gpa);
    var cache_scope = lir.CheckedPipeline.PipelineStageScope.begin(unit.config.timing, .boxy_unit_cache);
    defer cache_scope.end();
    try store(gpa, unit.cache_manager, unit.key, .{ .exports = built.exports, .hosted_symbols = built.hosted_symbols, .code_locations = built.code_locations, .object = built.object });
    return built;
}

/// Lower and compile `unit`.
fn build(unit: Unit, module_name: []const u8) BuildError!Built {
    const gpa = unit.gpa;
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
    var lowered = try eval.CompileTimeFinalization.lowerBoxyRuntime(gpa, modules, .{}, unit.config);
    defer lowered.deinit();
    if (dev_diagnostics.isSet(.trace_boxy_work)) std.debug.print("boxy-work: lowered {s} ({d} procs)\n", .{ module_name, lowered.lir_result.store.procSpecCount() });

    const static_data = try static_data_exports.buildStaticData(gpa, .{ .root = modules.root, .imports = views.items }, &lowered, unit.target, .{});
    defer static_data_exports.deinitStaticData(gpa, static_data);

    var codegen_scope = lir.CheckedPipeline.PipelineStageScope.begin(unit.config.timing, .boxy_unit_codegen);
    defer codegen_scope.end();
    var object_compiler = backend.ObjectFileCompiler.init(gpa);
    object_compiler.erased_capture_prefix = lowered.lir_result.erased_capture_prefix;
    object_compiler.link_unit = true;
    object_compiler.capture_code_locations = true;
    var written = object_compiler.compileToObjectFile(
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &.{},
        static_data,
        lowered.lir_result.store.getProcSpecs(),
        unit.target,
    ) catch |err| {
        std.log.err("Native compilation of module {s} failed: {}", .{ module_name, err });
        return error.NativeCompilationFailed;
    };
    defer written.deinit();
    codegen_scope.end();

    const records = try exportRecords(gpa, &lowered, unit.interfaces);
    defer gpa.free(records);
    var hosted_arena = std.heap.ArenaAllocator.init(gpa);
    defer hosted_arena.deinit();
    const hosted = try hostedSymbols(hosted_arena.allocator(), &lowered.lir_result.store);
    return .{
        .restored = false,
        .object = try gpa.dupe(u8, written.object_bytes),
        .code_locations = try gpa.dupe(u8, written.code_locations),
        .hosted_symbols = try dupeSymbols(gpa, hosted),
        .exports = try dupeRecords(gpa, records),
    };
}

/// Copies of `records`, each and the slice allocated from `gpa`.
fn dupeRecords(gpa: Allocator, records: []const object_cache.ExportRecord) Allocator.Error![]const object_cache.ExportRecord {
    const copies = try gpa.dupe(object_cache.ExportRecord, records);
    errdefer gpa.free(copies);
    var duped: usize = 0;
    errdefer for (copies[0..duped]) |copy| gpa.free(copy.rc_ret_conditions);
    for (records, copies) |record, *copy| {
        copy.rc_ret_conditions = try gpa.dupe(u32, record.rc_ret_conditions);
        duped += 1;
    }
    return copies;
}
