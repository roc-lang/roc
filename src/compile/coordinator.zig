//! Coordinator for the actor-based compilation model.
//!
//! The Coordinator is the single owner of all mutable state in the compilation pipeline.
//! This eliminates races by design:
//! - Single-threaded state mutations (no locks needed for state)
//! - Workers are pure: they receive tasks, return results
//! - Communication happens via bounded channels
//!
//! Architecture:
//! ```
//!                     ┌─────────────────────────────────────────────────────┐
//!                     │                    COORDINATOR                       │
//!                     │  - Owns all mutable state (modules, phases, deps)   │
//!                     │  - Single-threaded: no races by design              │
//!                     │  - Receives results, updates state, dispatches work │
//!                     └───────────────────────┬─────────────────────────────┘
//!                                             │
//!                          ┌──────────────────┼──────────────────┐
//!                          │                  │                  │
//!                     ┌────▼─────┐      ┌─────▼─────┐     ┌──────▼────────┐
//!                     │  Task    │      │  Result   │     │   Shared      │
//!                     │  Queue   │      │  Channel  │     │   Read-Only   │
//!                     │ (inject) │      │  (MPSC)   │     │   (Builtins)  │
//!                     └────┬─────┘      └─────▲─────┘     └───────────────┘
//!                          │                  │
//!         ┌────────────────┼──────────────────┼────────────────┐
//!         │                │                  │                │
//!    ┌────▼────┐      ┌────▼────┐        ┌────┴────┐      ┌────▼────┐
//!    │ Worker  │      │ Worker  │   ...  │ Worker  │      │ Worker  │
//!    │  Arena  │      │  Arena  │        │  Arena  │      │  Arena  │
//!    └─────────┘      └─────────┘        └─────────┘      └─────────┘
//! ```

const std = @import("std");
const builtin = @import("builtin");
const threading = @import("threading.zig");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const reporting = @import("reporting");
const eval = @import("eval");
const base = @import("base");
const collections = @import("collections");
const build_options = @import("build_options");

const CIR = can.CIR;

const messages = @import("messages.zig");
const channel = @import("channel.zig");
const compile_package = @import("compile_package.zig");
const compile_build = @import("compile_build.zig");
const module_discovery = @import("module_discovery.zig");
const cache_manager_mod = @import("cache_manager.zig");
const CacheManager = cache_manager_mod.CacheManager;
const CacheConfig = @import("cache_config.zig").CacheConfig;
const CacheStats = @import("cache_config.zig").CacheStats;
const app_header_mod = @import("app_header.zig");
const roc_target = @import("roc_target");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const Report = reporting.Report;
const AST = parse.AST;
const BuiltinModules = eval.BuiltinModules;
const CheckedModules = check.TypedCIR.Modules;

const WorkerTask = messages.WorkerTask;
const WorkerResult = messages.WorkerResult;
const ModuleId = messages.ModuleId;
const ParseTask = messages.ParseTask;
const CanonicalizeTask = messages.CanonicalizeTask;
const CanonicalizeImport = messages.CanonicalizeImport;
const TypeCheckTask = messages.TypeCheckTask;
const ParsedResult = messages.ParsedResult;
const CanonicalizedResult = messages.CanonicalizedResult;
const TypeCheckedResult = messages.TypeCheckedResult;
const CompileFailure = messages.CompileFailure;
const DiscoveredLocalImport = messages.DiscoveredLocalImport;
const DiscoveredExternalImport = messages.DiscoveredExternalImport;

const Channel = channel.Channel;
const CoreCtx = @import("ctx").CoreCtx;
const Mode = compile_package.Mode;

/// Allocators available while executing one worker task.
///
/// `module` and `result` allocations may outlive the task and cross back to the
/// coordinator. `scratch` allocations must not escape the task; worker threads
/// reset this arena after each task completes.
const WorkerTaskAllocators = struct {
    module: Allocator,
    result: Allocator,
    scratch: Allocator,
};

/// Threading features aren't available when targeting WebAssembly
const is_freestanding = threading.is_freestanding;
const threads_available = !is_freestanding;
const Thread = threading.Thread;

const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;
const CheckedArtifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

fn destroyCheckedArtifact(artifact: *CheckedModuleArtifact, retain_module_env: bool) void {
    const allocator = artifact.canonical_names.allocator;
    if (retain_module_env) {
        artifact.deinitRetainingModuleEnv(allocator);
    } else {
        artifact.deinit(allocator);
    }
    allocator.destroy(artifact);
}

const OwnedSemanticModuleData = struct {
    module_env: *ModuleEnv,
    checked_artifact: ?*CheckedModuleArtifact = null,

    fn deinit(self: *OwnedSemanticModuleData) void {
        if (self.checked_artifact) |artifact| {
            destroyCheckedArtifact(artifact, false);
            self.checked_artifact = null;
        }
    }
};

const RetiredCheckedArtifact = struct {
    artifact: *CheckedModuleArtifact,
    retain_module_env: bool,

    fn deinit(self: *RetiredCheckedArtifact) void {
        destroyCheckedArtifact(self.artifact, self.retain_module_env);
        self.artifact = undefined;
    }
};

/// Thread-safe allocator used for worker-thread allocations and shared
/// coordinator buffers (channels, per-package maps) when running multi-threaded.
///
/// On platforms with threads, `std.heap.smp_allocator` is used: it maintains
/// per-thread freelists backed by a shared global pool, avoiding the per-call
/// `mmap`/`munmap` serialization on the kernel VM lock that `page_allocator`
/// incurs (the prior cause of the multi-threaded performance cliff).
///
/// On wasm/freestanding (no threads), `smp_allocator` is unreachable at
/// runtime but the constant must still compile, so it resolves to
/// `page_allocator` (which on wasm is backed by `WasmAllocator`).
const thread_safe_allocator: Allocator = if (threads_available)
    std.heap.smp_allocator
else
    std.heap.page_allocator;

const StageTimer = if (threads_available) std.Io.Timestamp else void;

fn startStageTimer(io: std.Io) ?StageTimer {
    if (comptime !threads_available) return null;
    return std.Io.Timestamp.now(io, .awake);
}

fn readStageTimer(io: std.Io, timer: *?StageTimer) u64 {
    if (comptime !threads_available) return 0;
    if (timer.*) |active| {
        const elapsed = active.untilNow(io, .awake).nanoseconds;
        return if (elapsed > 0) @intCast(elapsed) else 0;
    }
    return 0;
}

const checked_module_cache_magic = "roc-mod-cache-v1";
const checked_module_cache_format_version: u64 = 1;
const checked_module_cache_header_len: usize = checked_module_cache_magic.len + 8 + 32 + 32;

fn writeU64Little(buf: []u8, value: u64) void {
    std.debug.assert(buf.len == 8);
    buf[0] = @truncate(value);
    buf[1] = @truncate(value >> 8);
    buf[2] = @truncate(value >> 16);
    buf[3] = @truncate(value >> 24);
    buf[4] = @truncate(value >> 32);
    buf[5] = @truncate(value >> 40);
    buf[6] = @truncate(value >> 48);
    buf[7] = @truncate(value >> 56);
}

fn readU64Little(buf: []const u8) u64 {
    std.debug.assert(buf.len == 8);
    return @as(u64, buf[0]) |
        (@as(u64, buf[1]) << 8) |
        (@as(u64, buf[2]) << 16) |
        (@as(u64, buf[3]) << 24) |
        (@as(u64, buf[4]) << 32) |
        (@as(u64, buf[5]) << 40) |
        (@as(u64, buf[6]) << 48) |
        (@as(u64, buf[7]) << 56);
}

fn hashCacheBody(body: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(body);
    return hasher.finalResult();
}

fn encodeCheckedModuleCacheEntry(
    allocator: Allocator,
    key: check.CheckedArtifact.CheckedModuleArtifactKey,
    body: []const u8,
) Allocator.Error![]u8 {
    const bytes = try allocator.alloc(u8, checked_module_cache_header_len + body.len);
    errdefer allocator.free(bytes);

    var offset: usize = 0;
    @memcpy(bytes[offset..][0..checked_module_cache_magic.len], checked_module_cache_magic);
    offset += checked_module_cache_magic.len;
    writeU64Little(bytes[offset..][0..8], checked_module_cache_format_version);
    offset += 8;
    @memcpy(bytes[offset..][0..32], &key.bytes);
    offset += 32;
    const body_hash = hashCacheBody(body);
    @memcpy(bytes[offset..][0..32], &body_hash);
    offset += 32;
    @memcpy(bytes[offset..], body);

    return bytes;
}

fn decodeCheckedModuleCacheEntry(
    key: check.CheckedArtifact.CheckedModuleArtifactKey,
    bytes: []const u8,
) ?[]const u8 {
    if (bytes.len < checked_module_cache_header_len) return null;
    var offset: usize = 0;
    if (!std.mem.eql(u8, bytes[offset..][0..checked_module_cache_magic.len], checked_module_cache_magic)) return null;
    offset += checked_module_cache_magic.len;
    if (readU64Little(bytes[offset..][0..8]) != checked_module_cache_format_version) return null;
    offset += 8;
    if (!std.mem.eql(u8, bytes[offset..][0..32], &key.bytes)) return null;
    offset += 32;
    const stored_hash = bytes[offset..][0..32];
    offset += 32;
    const body = bytes[offset..];
    const actual_hash = hashCacheBody(body);
    if (!std.mem.eql(u8, stored_hash, &actual_hash)) return null;
    if (body.len < @sizeOf(ModuleEnv.Serialized)) return null;
    return body;
}

/// Maximum scratch arena capacity retained by a worker after each task.
const worker_scratch_retain_limit = 64 * 1024 * 1024;

/// Allocators for a worker thread. Each worker has its own instance.
/// This ensures thread-safe allocations without contention.
///
/// Design rationale:
/// - `gpa`: For data that can outlive a task (ModuleEnv, source, ASTs, reports,
///   worker result payloads). In MT mode, this is smp_allocator for thread
///   safety. Data allocated here is freed during module/result cleanup.
/// - `arena_impl`: For temporary allocations within a task. Reset between tasks
///   to reduce allocation pressure.
pub const WorkerAllocators = struct {
    /// General purpose allocator for long-lived data and worker results.
    /// In multi-threaded mode, this is smp_allocator for thread safety.
    gpa: Allocator,

    /// Underlying arena implementation
    arena_impl: std.heap.ArenaAllocator,

    pub fn init(backing: Allocator) WorkerAllocators {
        return .{
            .gpa = backing,
            .arena_impl = std.heap.ArenaAllocator.init(backing),
        };
    }

    pub fn deinit(self: *WorkerAllocators) void {
        self.arena_impl.deinit();
    }

    /// Reset arena between tasks, retaining ordinary task capacity but trimming
    /// unusually large scratch spikes so one wide module does not pin worker RSS.
    pub fn resetArena(self: *WorkerAllocators) void {
        _ = self.arena_impl.reset(.{ .retain_with_limit = worker_scratch_retain_limit });
    }

    pub fn taskAllocators(self: *WorkerAllocators) WorkerTaskAllocators {
        return .{
            .module = self.gpa,
            .result = self.gpa,
            .scratch = self.arena_impl.allocator(),
        };
    }
};

/// Module compilation phase
pub const Phase = enum {
    /// Initial phase: needs parsing
    Parse,
    /// Queued for parsing (prevents double-enqueue)
    Parsing,
    /// Parsed, needs canonicalization
    Canonicalize,
    /// Canonicalized, waiting for imports to complete
    WaitingOnImports,
    /// Imports ready, needs type checking
    TypeCheck,
    /// Fully compiled
    Done,
};

/// State of a single module within a package
pub const ModuleState = struct {
    /// Module name (e.g., "Main", "Foo")
    name: []const u8,
    /// Filesystem path to the .roc file
    path: []const u8,
    /// Source-relative import base override for materialized modules.
    source_dir_override: ?[]const u8 = null,
    /// Compiler role assigned by the scheduler for this module.
    module_role: ModuleEnv.ModuleRole = .user,
    /// Owned semantic module payload. Earlier phases populate only `module_env`;
    /// type checking later fills in the checked artifact.
    semantic: ?OwnedSemanticModuleData = null,
    /// Cached AST from parsing (owned, null after canonicalization)
    cached_ast: ?*AST,
    /// Current compilation phase
    phase: Phase,
    /// Local imports (module IDs within same package)
    imports: std.ArrayList(ModuleId),
    /// External imports (qualified names like "pf.Stdout")
    external_imports: std.ArrayList([]const u8),
    /// Modules that depend on this one (for waking dependents)
    dependents: std.ArrayList(ModuleId),
    /// Transitive local imports known for this module.
    reachable_local_imports: std.bit_set.DynamicBitSetUnmanaged,
    /// Diagnostic reports
    reports: std.ArrayList(Report),
    /// Minimum dependency depth from root
    depth: u32,
    /// DFS visit color for cycle detection
    visit_color: VisitColor,
    /// Accumulated compile time for this module (parse + canonicalize + type-check)
    compile_time_ns: u64,

    /// DFS colors for cycle detection during import graph traversal
    pub const VisitColor = enum {
        /// Not yet visited
        white,
        /// Currently being visited (in the DFS stack)
        gray,
        /// Fully processed
        black,
    };

    pub fn init(name: []const u8, path: []const u8) ModuleState {
        return .{
            .name = name,
            .path = path,
            .cached_ast = null,
            .phase = .Parse,
            .imports = std.ArrayList(ModuleId).empty,
            .external_imports = std.ArrayList([]const u8).empty,
            .dependents = std.ArrayList(ModuleId).empty,
            .reachable_local_imports = .{},
            .reports = std.ArrayList(Report).empty,
            .depth = std.math.maxInt(u32),
            .visit_color = .white,
            .compile_time_ns = 0,
        };
    }

    fn moduleEnv(self: *ModuleState) ?*ModuleEnv {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |artifact| return artifact.moduleEnv();
            return semantic.module_env;
        }
        return null;
    }

    fn moduleEnvStorage(self: *ModuleState) ?check.CheckedArtifact.ModuleEnvStorage {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |artifact| return artifact.module_env;
            return .{ .checked_source = semantic.module_env };
        }
        return null;
    }

    fn checkedArtifact(self: *ModuleState) ?*check.CheckedArtifact.CheckedModuleArtifact {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |artifact| return artifact;
        }
        return null;
    }

    fn semanticData(self: *ModuleState) ?compile_package.SemanticModuleData {
        const env = self.moduleEnv() orelse return null;
        return .{
            .env = env,
            .checked_artifact = self.checkedArtifact(),
        };
    }

    fn replaceModuleEnv(self: *ModuleState, env: *ModuleEnv) void {
        if (self.semantic) |*semantic| {
            semantic.module_env = env;
        } else {
            self.semantic = .{
                .module_env = env,
                .checked_artifact = null,
            };
        }
    }

    fn canonicalSourceDir(self: *const ModuleState) []const u8 {
        return self.source_dir_override orelse (std.fs.path.dirname(self.path) orelse "");
    }

    fn replaceCheckedArtifact(
        self: *ModuleState,
        artifact: *CheckedModuleArtifact,
        retired_artifacts: *std.ArrayList(RetiredCheckedArtifact),
        allocator: Allocator,
    ) Allocator.Error!void {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |existing| {
                try retired_artifacts.append(allocator, .{
                    .artifact = existing,
                    .retain_module_env = @intFromPtr(existing.moduleEnv()) == @intFromPtr(artifact.moduleEnv()),
                });
            }
            semantic.checked_artifact = artifact;
            return;
        }
        std.debug.panic("compile.coordinator.ModuleState.replaceCheckedArtifact missing module env for {s}", .{self.name});
    }

    fn replaceRepublishedCheckedArtifact(
        self: *ModuleState,
        artifact: *CheckedModuleArtifact,
        retired_artifacts: *std.ArrayList(RetiredCheckedArtifact),
        allocator: Allocator,
    ) Allocator.Error!void {
        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact) |existing| {
                try retired_artifacts.append(allocator, .{
                    .artifact = existing,
                    .retain_module_env = true,
                });
            }
            semantic.checked_artifact = artifact;
            return;
        }
        std.debug.panic("compile.coordinator.ModuleState.replaceRepublishedCheckedArtifact missing semantic state for {s}", .{self.name});
    }

    pub fn deinit(self: *ModuleState, gpa: Allocator) void {
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: starting, semantic={}, ast={}\n", .{
                self.name,
                if (self.semantic != null) @as(u8, 1) else @as(u8, 0),
                if (self.cached_ast != null) @as(u8, 1) else @as(u8, 0),
            });
        }
        // Free cached AST if present.
        if (self.cached_ast) |ast| {
            if (comptime trace_build) {
                std.debug.print("[MOD DEINIT] {s}: freeing ast\n", .{self.name});
            }
            ast.deinit();
        }

        if (self.semantic) |*semantic| {
            if (semantic.checked_artifact != null) {
                // The checked artifact owns the ModuleEnv after publication.
                semantic.deinit();
            } else {
                const env = semantic.module_env;
                // IMPORTANT: env stores its own allocator (env.gpa) which was used to create it.
                // We must use env.gpa for cleanup, not the passed-in gpa, because in multi-threaded
                // mode, env.gpa is smp_allocator while gpa is the coordinator's allocator.
                const env_alloc = env.gpa;
                const source = env.common.source;
                if (comptime trace_build) {
                    std.debug.print("[MOD DEINIT] {s}: freeing env\n", .{self.name});
                }
                env.deinit();
                env_alloc.destroy(env);
                if (source.len > 0) env_alloc.free(@constCast(source));
            }
        }

        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: freeing imports\n", .{self.name});
        }
        self.imports.deinit(gpa);
        for (self.external_imports.items) |imp| {
            gpa.free(imp);
        }
        self.external_imports.deinit(gpa);
        self.dependents.deinit(gpa);
        self.reachable_local_imports.deinit(gpa);
        for (self.reports.items) |*rep| {
            rep.deinit();
        }
        self.reports.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[MOD DEINIT] {s}: freeing path and name\n", .{self.name});
        }
        if (self.source_dir_override) |source_dir| gpa.free(source_dir);
        gpa.free(self.path);
        gpa.free(self.name);
    }
};

/// State of a package in the workspace
pub const PackageState = struct {
    /// Package name (alias in workspace)
    name: []const u8,
    /// Root directory for the package
    root_dir: []const u8,
    /// All modules in this package
    modules: std.ArrayList(ModuleState),
    /// Module name -> module ID lookup
    module_names: std.StringHashMap(ModuleId),
    /// Number of modules not yet in Done phase
    remaining_modules: usize,
    /// Root module ID (the module that starts the build)
    root_module_id: ?ModuleId,
    /// Package shorthands (alias -> target package name)
    shorthands: std.StringHashMap([]const u8),

    pub fn init(_: Allocator, name: []const u8, root_dir: []const u8) PackageState {
        return .{
            .name = name,
            .root_dir = root_dir,
            .modules = std.ArrayList(ModuleState).empty,
            .module_names = std.StringHashMap(ModuleId).init(thread_safe_allocator),
            .remaining_modules = 0,
            .root_module_id = null,
            .shorthands = std.StringHashMap([]const u8).init(thread_safe_allocator),
        };
    }

    pub fn deinit(self: *PackageState, gpa: Allocator) void {
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: deiniting {} modules\n", .{ self.name, self.modules.items.len });
        }
        for (self.modules.items, 0..) |*mod, i| {
            if (comptime trace_build) {
                std.debug.print("[PKG DEINIT] {s}: deinit module {} ({s})\n", .{ self.name, i, mod.name });
            }
            mod.deinit(gpa);
        }
        self.modules.deinit(gpa);
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: modules done, deiniting names\n", .{self.name});
        }
        self.module_names.deinit();

        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: names done, deiniting shorthands\n", .{self.name});
        }
        var sh_it = self.shorthands.iterator();
        while (sh_it.next()) |entry| {
            gpa.free(entry.key_ptr.*);
            gpa.free(entry.value_ptr.*);
        }
        self.shorthands.deinit();

        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] {s}: freeing name and root_dir\n", .{self.name});
        }
        gpa.free(self.name);
        gpa.free(self.root_dir);
        if (comptime trace_build) {
            std.debug.print("[PKG DEINIT] done\n", .{});
        }
    }

    /// Ensure a module exists, creating it if necessary
    pub fn ensureModule(self: *PackageState, gpa: Allocator, name: []const u8, path: []const u8) Allocator.Error!ModuleId {
        if (self.module_names.get(name)) |id| {
            return id;
        }

        const id: ModuleId = @intCast(self.modules.items.len);
        const owned_name = try gpa.dupe(u8, name);
        const owned_path = try gpa.dupe(u8, path);

        try self.modules.append(gpa, ModuleState.init(owned_name, owned_path));
        try self.module_names.put(owned_name, id);

        return id;
    }

    /// Get module state by ID
    pub fn getModule(self: *PackageState, id: ModuleId) ?*ModuleState {
        if (id >= self.modules.items.len) return null;
        return &self.modules.items[id];
    }

    /// Get module ID by name
    pub fn getModuleId(self: *PackageState, name: []const u8) ?ModuleId {
        return self.module_names.get(name);
    }

    pub fn getSemanticDataIfDone(self: *PackageState, name: []const u8) ?compile_package.SemanticModuleData {
        const id = self.module_names.get(name) orelse return null;
        const mod = &self.modules.items[id];
        if (mod.phase != .Done) return null;
        return mod.semanticData();
    }

    /// Check if a module is done (regardless of whether it has an env).
    /// This is used to check if dependents can proceed - a module that failed
    /// to parse is still "done" and shouldn't block dependents.
    pub fn isDone(self: *PackageState, name: []const u8) bool {
        const id = self.module_names.get(name) orelse return false;
        const mod = &self.modules.items[id];
        return mod.phase == .Done;
    }

    pub fn moduleReaches(self: *const PackageState, from: ModuleId, target: ModuleId) bool {
        const reachable = self.modules.items[from].reachable_local_imports;
        return target < reachable.bit_length and reachable.isSet(target);
    }

    fn addReachableLocalImport(
        self: *PackageState,
        gpa: Allocator,
        module_id: ModuleId,
        reachable_id: ModuleId,
        delta: ?*std.ArrayList(ModuleId),
    ) Allocator.Error!bool {
        const reachable = &self.modules.items[module_id].reachable_local_imports;
        const needed_len = @as(usize, reachable_id) + 1;
        if (reachable.bit_length < needed_len) {
            try reachable.resize(gpa, needed_len, false);
        }
        if (reachable.isSet(reachable_id)) return false;

        reachable.set(reachable_id);
        if (delta) |delta_list| try delta_list.append(gpa, reachable_id);
        return true;
    }

    pub fn recordLocalImportReachability(
        self: *PackageState,
        gpa: Allocator,
        module_id: ModuleId,
        imported_id: ModuleId,
    ) Allocator.Error!void {
        var initial_delta = std.ArrayList(ModuleId).empty;
        defer initial_delta.deinit(gpa);

        _ = try self.addReachableLocalImport(gpa, module_id, imported_id, &initial_delta);

        const imported_reachable = &self.modules.items[imported_id].reachable_local_imports;
        if (imported_reachable.bit_length > 0) {
            var iter = imported_reachable.iterator(.{});
            while (iter.next()) |reachable| {
                _ = try self.addReachableLocalImport(gpa, module_id, @intCast(reachable), &initial_delta);
            }
        }

        if (initial_delta.items.len == 0) return;
        try self.propagateReachabilityDelta(gpa, module_id, initial_delta.items);
    }

    fn propagateReachabilityDelta(
        self: *PackageState,
        gpa: Allocator,
        module_id: ModuleId,
        initial_delta: []const ModuleId,
    ) Allocator.Error!void {
        const WorkItem = struct {
            module_id: ModuleId,
            delta_start: usize,
            delta_len: usize,
        };

        var all_delta = std.ArrayList(ModuleId).empty;
        defer all_delta.deinit(gpa);
        try all_delta.appendSlice(gpa, initial_delta);

        var work = std.ArrayList(WorkItem).empty;
        defer work.deinit(gpa);
        try work.append(gpa, .{
            .module_id = module_id,
            .delta_start = 0,
            .delta_len = initial_delta.len,
        });

        var work_index: usize = 0;
        while (work_index < work.items.len) : (work_index += 1) {
            const item = work.items[work_index];
            const item_delta_end = item.delta_start + item.delta_len;
            const dependents = self.modules.items[item.module_id].dependents.items;

            for (dependents) |dependent_id| {
                const new_delta_start = all_delta.items.len;
                for (item.delta_start..item_delta_end) |delta_index| {
                    const reachable_id = all_delta.items[delta_index];
                    _ = try self.addReachableLocalImport(gpa, dependent_id, reachable_id, &all_delta);
                }
                const new_delta_len = all_delta.items.len - new_delta_start;
                if (new_delta_len == 0) continue;

                try work.append(gpa, .{
                    .module_id = dependent_id,
                    .delta_start = new_delta_start,
                    .delta_len = new_delta_len,
                });
            }
        }
    }
};

/// A reference to a module in a specific package
pub const ModuleRef = struct {
    pkg_name: []const u8,
    module_id: ModuleId,
};

/// Coordinator manages all compilation state and coordinates workers
pub const Coordinator = struct {
    gpa: Allocator,
    mode: Mode,
    max_threads: usize,
    target: roc_target.RocTarget,

    /// All packages in the workspace
    packages: std.StringHashMap(*PackageState),

    /// Result channel from workers to coordinator
    result_channel: Channel(WorkerResult),

    /// Task channel for workers (ring buffer with proper synchronization)
    task_channel: Channel(WorkerTask),

    /// Worker threads
    workers: std.ArrayList(Thread),

    /// Number of tasks currently being processed by workers (atomic for thread safety)
    inflight: std.atomic.Value(usize),

    /// Set by shutdown() to tell workers to stop promptly instead of draining
    /// remaining tasks from the channel.
    shutting_down: std.atomic.Value(bool),

    /// Set by a worker thread when it runs out of memory while producing a
    /// result (e.g. constructing a failure report). Worker threads have a
    /// `void` signature and cannot return errors, so they record the OOM here;
    /// the coordinator loop observes it and aborts the build with
    /// `error.OutOfMemory` instead of hanging or silently dropping the failure.
    worker_oom: std.atomic.Value(bool),

    /// Total modules remaining across all packages
    total_remaining: usize,

    /// Shared read-only builtin modules
    builtin_modules: *const BuiltinModules,

    /// I/O abstraction for reading sources and other filesystem/stdio operations.
    roc_ctx: CoreCtx,

    /// Compiler version for cache keys
    compiler_version: []const u8,

    /// Optional cache manager for transparent checked-module disk caching.
    cache_manager: ?*CacheManager,

    /// Cross-package dependents: when module (pkg, id) completes, notify these modules
    /// Key is "pkg_name:module_id", value is list of dependent ModuleRefs
    cross_package_dependents: std.StringHashMap(std.ArrayList(ModuleRef)),

    /// Exact-key index for checked artifacts published during this build. This
    /// does not own artifacts; it points at the module storage that owns them.
    checked_artifact_index: std.AutoHashMap([32]u8, ModuleRef),

    /// Checked artifacts that have been replaced but may still be referenced by
    /// `ImportedModuleView`s already handed to worker threads.
    retired_checked_artifacts: std.ArrayList(RetiredCheckedArtifact),

    /// Whether to run hosted compiler transformation after canonicalization.
    /// Set to true for executable platform builds where platform modules need hosted lambdas.
    enable_hosted_transform: bool,

    /// Timing accumulators
    total_parse_ns: u64,
    total_canonicalize_ns: u64,
    total_canonicalize_diag_ns: u64,
    total_typecheck_ns: u64,
    total_typecheck_diag_ns: u64,

    /// Build statistics
    cache_hits: u32,
    cache_misses: u32,
    modules_compiled: u32,
    /// Module compile time tracking (min/max/sum for computing avg)
    module_time_min_ns: u64,
    module_time_max_ns: u64,
    module_time_sum_ns: u64,

    /// Get allocator for worker thread operations.
    /// In multi-threaded mode, returns smp_allocator (per-thread freelists
    /// backed by a shared global pool, designed for SMP workloads). In
    /// single-threaded mode, returns gpa for better performance.
    pub fn getWorkerAllocator(self: *const Coordinator) Allocator {
        return if (threads_available and self.mode == .multi_threaded)
            thread_safe_allocator
        else
            self.gpa;
    }

    /// Initialize a `Coordinator`.
    ///
    /// `compiler_version` is a cache-key discriminant — it is incorporated
    /// into the keys used by `cache_manager` so that artifacts from
    /// different compiler versions (or different consumer wrappers) stay
    /// segregated. Embedders should pass a stable string that changes
    /// whenever their consumer's compile semantics could change, e.g.
    /// `"my-embedder@1.2.3+roc@" ++ build_options.compiler_version`.
    /// Mismatching across runs causes cache misses, not corruption.
    pub fn init(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        builtin_modules: *const BuiltinModules,
        compiler_version: []const u8,
        cache_manager: ?*CacheManager,
        roc_ctx: CoreCtx,
    ) Allocator.Error!Coordinator {
        // Both channels use smp_allocator in multi-threaded mode because their
        // buffers may be grown (task_channel) or accessed from worker threads.
        // smp_allocator is thread-safe and avoids the per-allocation mmap/munmap
        // serialization on the kernel VM lock that page_allocator incurs.
        const channel_allocator = if (threads_available) thread_safe_allocator else gpa;
        const initial_task_capacity = 256;
        return .{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .packages = std.StringHashMap(*PackageState).init(gpa),
            .result_channel = try Channel(WorkerResult).init(channel_allocator, channel.DEFAULT_CAPACITY, roc_ctx.std_io),
            .task_channel = try Channel(WorkerTask).init(channel_allocator, initial_task_capacity, roc_ctx.std_io),
            .workers = std.ArrayList(Thread).empty,
            .inflight = std.atomic.Value(usize).init(0),
            .shutting_down = std.atomic.Value(bool).init(false),
            .worker_oom = std.atomic.Value(bool).init(false),
            .total_remaining = 0,
            .builtin_modules = builtin_modules,
            .roc_ctx = roc_ctx,
            .compiler_version = compiler_version,
            .cache_manager = cache_manager,
            .cross_package_dependents = std.StringHashMap(std.ArrayList(ModuleRef)).init(gpa),
            .checked_artifact_index = std.AutoHashMap([32]u8, ModuleRef).init(gpa),
            .retired_checked_artifacts = std.ArrayList(RetiredCheckedArtifact).empty,
            .enable_hosted_transform = false,
            .total_parse_ns = 0,
            .total_canonicalize_ns = 0,
            .total_canonicalize_diag_ns = 0,
            .total_typecheck_ns = 0,
            .total_typecheck_diag_ns = 0,
            .cache_hits = 0,
            .cache_misses = 0,
            .modules_compiled = 0,
            .module_time_min_ns = std.math.maxInt(u64),
            .module_time_max_ns = 0,
            .module_time_sum_ns = 0,
        };
    }

    pub fn deinit(self: *Coordinator) void {
        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] starting shutdown...\n", .{});
        }
        // Stop workers
        self.shutdown();

        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] shutdown done, freeing packages...\n", .{});
        }

        // Free packages
        // Note: ModuleEnv stores its own allocator (env.gpa), so deinit will use the correct
        // allocator for each env. In multi-threaded mode, this is smp_allocator.
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] deinit package {s}\n", .{entry.key_ptr.*});
            }
            entry.value_ptr.*.deinit(self.gpa);
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] package deinit done, now destroying\n", .{});
            }
            self.gpa.destroy(entry.value_ptr.*);
            if (comptime trace_build) {
                std.debug.print("[COORD DEINIT] package destroyed\n", .{});
            }
        }
        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] all packages done, calling packages.deinit()\n", .{});
        }
        self.packages.deinit();

        if (comptime trace_build) {
            std.debug.print("[COORD DEINIT] packages done\n", .{});
        }

        // Free task channel
        self.task_channel.deinit();

        // Free cross-package dependents
        var cpd_it = self.cross_package_dependents.iterator();
        while (cpd_it.next()) |entry| {
            self.gpa.free(entry.key_ptr.*); // Free the "pkg:id" key string
            entry.value_ptr.deinit(self.gpa);
        }
        self.cross_package_dependents.deinit();

        self.checked_artifact_index.deinit();

        for (self.retired_checked_artifacts.items) |*retired| {
            retired.deinit();
        }
        self.retired_checked_artifacts.deinit(self.gpa);

        self.result_channel.deinit();
        self.workers.deinit(self.gpa);
    }

    /// Set the I/O / core context implementation. Callers must supply a fully
    /// initialised `CoreCtx` — it must not create a replacement
    /// `CoreCtx.default(...)` here because the existing context may have been
    /// constructed via `CoreCtx.testing(undefined, undefined)` (see
    /// `cache_config.zig`), in which case snapshotting its fields into a
    /// `default()` vtable would invoke UB the first time the OS-backed
    /// vtable dereferenced `std_io`.
    pub fn setCoreCtx(self: *Coordinator, roc_ctx: CoreCtx) void {
        self.roc_ctx = roc_ctx;
    }

    /// Back-compat alias for the documented embedding contract
    /// (`coord.setIo(my_io)` in `src/compile/README.md`).
    pub const setIo = setCoreCtx;

    /// Get the allocator to use for module data.
    /// - In multi-threaded mode: smp_allocator (per-thread freelists)
    /// - In single-threaded mode: gpa (better performance)
    pub fn getModuleAllocator(self: *Coordinator) std.mem.Allocator {
        return if (threads_available and self.mode == .multi_threaded)
            thread_safe_allocator
        else
            self.gpa;
    }

    fn allocateCheckedArtifact(artifact: CheckedModuleArtifact) Allocator.Error!*CheckedModuleArtifact {
        const allocator = artifact.canonical_names.allocator;
        const owned = try allocator.create(CheckedModuleArtifact);
        owned.* = artifact;
        return owned;
    }

    /// Create or get a package
    pub fn ensurePackage(self: *Coordinator, name: []const u8, root_dir: []const u8) Allocator.Error!*PackageState {
        if (self.packages.get(name)) |pkg| {
            return pkg;
        }

        const pkg = try self.gpa.create(PackageState);
        errdefer self.gpa.destroy(pkg);

        const owned_name = try self.gpa.dupe(u8, name);
        const owned_root_dir = self.gpa.dupe(u8, root_dir) catch |err| {
            self.gpa.free(owned_name);
            return err;
        };

        pkg.* = PackageState.init(self.gpa, owned_name, owned_root_dir);
        errdefer pkg.deinit(self.gpa);

        try self.packages.put(pkg.name, pkg);
        return pkg;
    }

    /// Get a package by name
    pub fn getPackage(self: *Coordinator, name: []const u8) ?*PackageState {
        return self.packages.get(name);
    }

    /// Options for `discoverAppFromPath`.
    pub const AppDiscoveryOptions = struct {
        /// Path to the app `.roc` file, accessible via the configured Io.
        entry_path: []const u8,
        /// Optional override for the app module's `source_dir_override`.
        source_dir_override: ?[]const u8 = null,
    };

    pub const AppDiscoveryError = error{
        InvalidPlatformPath,
        AbsolutePlatformPath,
        UnsupportedPlatformSpec,
        UnsupportedPackageSpec,
    } || app_header_mod.Error || Allocator.Error;

    /// Read an app `.roc` file's header, register the app + platform +
    /// non-platform packages, and enqueue the app's parse task. After this
    /// returns, the caller should call `start()` and `coordinatorLoop()`.
    ///
    /// Only relative paths (`./...`, `../...`) are supported for the platform
    /// spec and non-platform packages — URL specs return
    /// `error.UnsupportedPlatformSpec` or `error.UnsupportedPackageSpec`.
    /// Embedders that need URL pre-resolution (downloading + caching) should
    /// call `compile.app_header.parseAppHeader` themselves, resolve URLs to
    /// local paths through their own policy, and register packages via
    /// `ensurePackage` / `registerInlinePackage`. See
    /// `buildLirImageWithCoordinator` in `src/cli/main.zig` for a
    /// worked example of the URL-aware path.
    ///
    /// `arena` holds strings extracted from the header (qualifiers, specs).
    pub fn discoverAppFromPath(
        self: *Coordinator,
        arena: Allocator,
        opts: AppDiscoveryOptions,
    ) AppDiscoveryError!void {
        const header_info = try app_header_mod.parseAppHeader(self.roc_ctx, self.gpa, arena, opts.entry_path);

        const app_dir = std.fs.path.dirname(opts.entry_path) orelse ".";

        const app_pkg = try self.ensurePackage("app", app_dir);
        const app_module_name = base.module_path.getModuleName(opts.entry_path);
        const app_module_id = try app_pkg.ensureModule(self.gpa, app_module_name, opts.entry_path);
        if (opts.source_dir_override) |source_dir| {
            app_pkg.modules.items[app_module_id].source_dir_override = try self.gpa.dupe(u8, source_dir);
        }
        app_pkg.root_module_id = app_module_id;
        app_pkg.modules.items[app_module_id].depth = 0;
        app_pkg.remaining_modules += 1;
        self.total_remaining += 1;

        if (header_info.platform_spec.len > 0) {
            if (std.fs.path.isAbsolute(header_info.platform_spec)) {
                return error.AbsolutePlatformPath;
            }
            if (!isRelativeSpec(header_info.platform_spec)) {
                return error.UnsupportedPlatformSpec;
            }
            const platform_main_path = try std.fs.path.join(arena, &.{ app_dir, header_info.platform_spec });
            const platform_dir = std.fs.path.dirname(platform_main_path) orelse return error.InvalidPlatformPath;

            try self.registerPlatformPackage(app_pkg, platform_dir, platform_main_path, header_info.platform_qualifier);
        }

        for (header_info.non_platform_packages) |entry| {
            if (!isRelativeSpec(entry.spec)) {
                return error.UnsupportedPackageSpec;
            }
            const pkg_main_path = try std.fs.path.join(arena, &.{ app_dir, entry.spec });
            const pkg_dir = std.fs.path.dirname(pkg_main_path) orelse ".";
            _ = try self.registerInlinePackage(entry.shorthand, pkg_dir, app_pkg, entry.shorthand);
        }

        try self.enqueueParseTask("app", app_module_id);
    }

    /// Register the platform package (named "pf"), wire the app's shorthand
    /// for it, ensure the platform's main module, and enqueue its parse task.
    pub fn registerPlatformPackage(
        self: *Coordinator,
        app_pkg: *PackageState,
        platform_dir: []const u8,
        platform_main_path: []const u8,
        qualifier: ?[]const u8,
    ) Allocator.Error!void {
        const pf_pkg = try self.ensurePackage("pf", platform_dir);

        if (qualifier) |qual| {
            try app_pkg.shorthands.put(
                try self.gpa.dupe(u8, qual),
                try self.gpa.dupe(u8, "pf"),
            );
        }

        const pf_module_id = try pf_pkg.ensureModule(self.gpa, "main", platform_main_path);
        pf_pkg.root_module_id = pf_module_id;
        pf_pkg.modules.items[pf_module_id].depth = 1;
        pf_pkg.remaining_modules += 1;
        self.total_remaining += 1;
        try self.enqueueParseTask("pf", pf_module_id);
    }

    /// Register a non-platform package and (optionally) wire a shorthand on
    /// the app package that resolves to it. Useful for embedders that have
    /// pre-resolved URL packages to local paths.
    ///
    /// Returns the new (or existing) package state.
    pub fn registerInlinePackage(
        self: *Coordinator,
        package_name: []const u8,
        package_root_dir: []const u8,
        app_pkg: ?*PackageState,
        shorthand_on_app: ?[]const u8,
    ) Allocator.Error!*PackageState {
        const pkg = try self.ensurePackage(package_name, package_root_dir);
        if (app_pkg) |a| {
            if (shorthand_on_app) |sh| {
                try a.shorthands.put(
                    try self.gpa.dupe(u8, sh),
                    try self.gpa.dupe(u8, package_name),
                );
            }
        }
        return pkg;
    }

    fn isRelativeSpec(spec: []const u8) bool {
        return std.mem.startsWith(u8, spec, "./") or std.mem.startsWith(u8, spec, "../");
    }

    /// Return the published checked artifact for a package root module.
    pub fn rootCheckedArtifact(self: *Coordinator, package_name: []const u8) *const check.CheckedArtifact.CheckedModuleArtifact {
        const pkg = self.packages.get(package_name) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.rootCheckedArtifact missing package {s}", .{package_name});
            }
            unreachable;
        };
        const root_id = pkg.root_module_id orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.rootCheckedArtifact missing root module for package {s}", .{package_name});
            }
            unreachable;
        };
        const root_mod = pkg.getModule(root_id) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.rootCheckedArtifact root id out of range for package {s}", .{package_name});
            }
            unreachable;
        };
        return root_mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.rootCheckedArtifact missing checked artifact for package {s}", .{package_name});
            }
            unreachable;
        };
    }

    /// Collect published checked artifacts available to post-check lowering.
    pub fn collectImportedArtifactViews(
        self: *Coordinator,
        allocator: Allocator,
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error![]check.CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(allocator);

        try appendImportedArtifactViewIfMissing(&views, allocator, root_artifact.key, &self.builtin_modules.checked_artifact);

        for (root_artifact.direct_import_artifact_keys) |key| {
            try self.appendPublicApiDependencyViewByKey(&views, allocator, root_artifact, key);
        }
        for (root_artifact.public_api_dependencies.artifacts) |key| {
            try self.appendPublicApiDependencyViewByKey(&views, allocator, root_artifact, key);
        }
        try self.appendRelationClosureDependencyViews(&views, allocator, root_artifact);

        return try views.toOwnedSlice(allocator);
    }

    fn collectTypecheckAvailableArtifactViews(
        self: *Coordinator,
        allocator: Allocator,
        imported_artifacts: []const check.CheckedArtifact.PublishImportArtifact,
    ) Allocator.Error![]check.CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(allocator);

        var pending = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        defer pending.deinit(allocator);

        var seen = std.AutoHashMap(check.CheckedArtifact.CheckedModuleArtifactKey, void).init(allocator);
        defer seen.deinit();

        for (imported_artifacts) |imported| {
            try pending.append(allocator, imported.view);
        }

        while (pending.pop()) |view| {
            const entry = try seen.getOrPut(view.key);
            if (entry.found_existing) continue;
            entry.value_ptr.* = {};

            try views.append(allocator, view);

            for (view.public_api_dependencies.type_owner_artifacts) |dependency_key| {
                const artifact = self.checkedArtifactByKey(dependency_key) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("compile.coordinator missing type-owner dependency checked artifact", .{});
                    }
                    unreachable;
                };
                try pending.append(allocator, check.CheckedArtifact.importedView(artifact));
            }
        }

        return try views.toOwnedSlice(allocator);
    }

    fn rootRelationContainsArtifact(
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) bool {
        for (root_artifact.platform_required_bindings.bindings) |binding| {
            if (std.mem.eql(u8, &binding.app_value.artifact.bytes, &key.bytes)) return true;
        }
        return false;
    }

    fn appendRelationClosureDependencyViews(
        self: *Coordinator,
        views: *std.ArrayList(CheckedArtifact.ImportedModuleView),
        allocator: Allocator,
        root_artifact: *const CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error!void {
        var relation_views = std.ArrayList(CheckedArtifact.ImportedModuleView).empty;
        defer relation_views.deinit(allocator);

        for (root_artifact.platform_required_bindings.bindings) |binding| {
            const relation_artifact = self.checkedArtifactByKey(binding.app_value.artifact) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("compile.coordinator invariant violated: platform relation references unavailable app artifact", .{});
                }
                unreachable;
            };
            try appendImportedArtifactViewIfMissing(&relation_views, allocator, root_artifact.key, relation_artifact);
        }

        var dependency_collector = RelationLoweringDependencyCollector.init(
            self,
            allocator,
            root_artifact.key,
            CheckedArtifact.importedView(root_artifact),
            relation_views.items,
            views,
        );
        defer dependency_collector.deinit();

        for (root_artifact.platform_required_bindings.bindings) |binding| {
            const relation_view = relationViewByKey(relation_views.items, binding.app_value.artifact) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("compile.coordinator invariant violated: platform relation view was not collected", .{});
                }
                unreachable;
            };
            try dependency_collector.appendPlatformRelationDependency(relation_view, binding);
        }
    }

    const RelationLoweringDependencyCollector = struct {
        coordinator: *Coordinator,
        allocator: Allocator,
        root_key: CheckedArtifact.CheckedModuleArtifactKey,
        root_view: ?CheckedArtifact.ImportedModuleView,
        relation_artifacts: []const CheckedArtifact.ImportedModuleView,
        views: *std.ArrayList(CheckedArtifact.ImportedModuleView),
        visited_public_api: std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, void),
        visited_templates: std.AutoHashMap(canonical.ProcedureTemplateRef, void),
        visited_consts: std.AutoHashMap(CheckedArtifact.ConstRef, void),
        visited_callable_eval_templates: std.AutoHashMap(CheckedArtifact.ArtifactCallableEvalTemplateRef, void),

        fn init(
            coordinator: *Coordinator,
            allocator: Allocator,
            root_key: CheckedArtifact.CheckedModuleArtifactKey,
            root_view: ?CheckedArtifact.ImportedModuleView,
            relation_artifacts: []const CheckedArtifact.ImportedModuleView,
            views: *std.ArrayList(CheckedArtifact.ImportedModuleView),
        ) RelationLoweringDependencyCollector {
            return .{
                .coordinator = coordinator,
                .allocator = allocator,
                .root_key = root_key,
                .root_view = root_view,
                .relation_artifacts = relation_artifacts,
                .views = views,
                .visited_public_api = std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, void).init(allocator),
                .visited_templates = std.AutoHashMap(canonical.ProcedureTemplateRef, void).init(allocator),
                .visited_consts = std.AutoHashMap(CheckedArtifact.ConstRef, void).init(allocator),
                .visited_callable_eval_templates = std.AutoHashMap(CheckedArtifact.ArtifactCallableEvalTemplateRef, void).init(allocator),
            };
        }

        fn deinit(self: *RelationLoweringDependencyCollector) void {
            self.visited_callable_eval_templates.deinit();
            self.visited_consts.deinit();
            self.visited_templates.deinit();
            self.visited_public_api.deinit();
        }

        fn appendPlatformRelationDependency(
            self: *RelationLoweringDependencyCollector,
            relation_artifact: CheckedArtifact.ImportedModuleView,
            binding: CheckedArtifact.PlatformRequiredBinding,
        ) Allocator.Error!void {
            switch (binding.value_use) {
                .const_value => |const_use| try self.appendClosure(const_use.relation_template_closure),
                .procedure_value => |procedure| try self.appendClosure(procedure.relation_template_closure),
            }
            try self.appendRelationArtifactExportedValueClosure(relation_artifact, binding);
        }

        fn appendClosure(
            self: *RelationLoweringDependencyCollector,
            closure: CheckedArtifact.ImportedTemplateClosureView,
        ) Allocator.Error!void {
            for (closure.checked_bodies) |value| try self.appendArtifactKey(value.artifact);
            for (closure.checked_type_roots) |value| try self.appendArtifactKey(value.artifact);
            for (closure.checked_type_schemes) |value| try self.appendArtifactKey(value.artifact);
            for (closure.checked_callable_bodies) |value| try self.appendArtifactKey(value.artifact);
            for (closure.checked_const_bodies) |value| try self.appendArtifactKey(value.artifact);
            for (closure.checked_procedure_templates) |value| try self.appendProcedureTemplateRef(value);
            for (closure.callable_eval_templates) |value| try self.appendCallableEvalTemplateRef(value);
            for (closure.const_templates) |value| try self.appendConstRef(value);
            for (closure.nested_proc_sites) |value| try self.appendArtifactKey(value.artifact);
            for (closure.resolved_value_refs) |value| try self.appendArtifactKey(value.artifact);
            for (closure.static_dispatch_plans) |value| try self.appendArtifactKey(value.artifact);
            for (closure.interface_capabilities) |value| try self.appendArtifactKey(value.artifact);
        }

        fn appendArtifactKey(
            self: *RelationLoweringDependencyCollector,
            key: CheckedArtifact.CheckedModuleArtifactKey,
        ) Allocator.Error!void {
            if (checkedArtifactKeyEql(key, self.root_key)) return;

            const view = self.viewForKey(key);
            if (!self.relationArtifactContainsKey(key)) {
                try self.appendViewIfMissing(view);
            }
            try self.appendPublicApiDependenciesForView(view);
        }

        fn appendViewIfMissing(
            self: *RelationLoweringDependencyCollector,
            view: CheckedArtifact.ImportedModuleView,
        ) Allocator.Error!void {
            if (checkedArtifactKeyEql(view.key, self.root_key)) return;
            if (self.relationArtifactContainsKey(view.key)) return;
            if (importedArtifactViewExists(self.views.items, view.key)) return;
            try self.views.append(self.allocator, view);
        }

        fn appendPublicApiDependenciesForView(
            self: *RelationLoweringDependencyCollector,
            view: CheckedArtifact.ImportedModuleView,
        ) Allocator.Error!void {
            const entry = try self.visited_public_api.getOrPut(view.key);
            if (entry.found_existing) return;
            entry.value_ptr.* = {};

            for (view.public_api_dependencies.artifacts) |dependency_key| {
                try self.appendArtifactKey(dependency_key);
            }
        }

        fn appendProcedureTemplateRef(
            self: *RelationLoweringDependencyCollector,
            template_ref: canonical.ProcedureTemplateRef,
        ) Allocator.Error!void {
            const key = checkedArtifactKeyFromArtifactRef(template_ref.artifact);
            try self.appendArtifactKey(key);

            const entry = try self.visited_templates.getOrPut(template_ref);
            if (entry.found_existing) return;
            entry.value_ptr.* = {};

            const view = self.viewForKey(key);
            const index: usize = @intFromEnum(template_ref.template);
            if (index >= view.checked_procedure_templates.templates.len) {
                coordinatorInvariant("relation lowering dependency referenced missing checked procedure template", .{});
            }
            const template = view.checked_procedure_templates.get(template_ref.template);
            if (template.proc_base != template_ref.proc_base) {
                coordinatorInvariant("relation lowering dependency procedure template ref disagreed with template row", .{});
            }
            try self.appendResolvedValueRefs(view, template.resolved_value_refs);
        }

        fn appendCallableEvalTemplateRef(
            self: *RelationLoweringDependencyCollector,
            template_ref: CheckedArtifact.ArtifactCallableEvalTemplateRef,
        ) Allocator.Error!void {
            try self.appendArtifactKey(template_ref.artifact);

            const entry = try self.visited_callable_eval_templates.getOrPut(template_ref);
            if (entry.found_existing) return;
            entry.value_ptr.* = {};

            const view = self.viewForKey(template_ref.artifact);
            const index: usize = @intFromEnum(template_ref.template);
            if (index >= view.callable_eval_templates.templates.len) {
                coordinatorInvariant("relation lowering dependency referenced missing callable-eval template", .{});
            }
            const template = view.callable_eval_templates.templates[index];
            const wrapper = view.entry_wrappers.lookupByRoot(template.root) orelse {
                coordinatorInvariant("relation lowering dependency callable-eval template had no entry wrapper", .{});
            };
            try self.appendProcedureTemplateRef(wrapper.template);
        }

        fn appendConstRef(
            self: *RelationLoweringDependencyCollector,
            const_ref: CheckedArtifact.ConstRef,
        ) Allocator.Error!void {
            try self.appendArtifactKey(const_ref.artifact);

            const entry = try self.visited_consts.getOrPut(const_ref);
            if (entry.found_existing) return;
            entry.value_ptr.* = {};

            const view = self.viewForKey(const_ref.artifact);
            const template = view.const_templates.get(const_ref);
            switch (template.state) {
                .eval_template => |eval_template| {
                    try self.appendResolvedValueRefs(view, eval_template.resolved_value_refs);
                    try self.appendProcedureTemplateRef(eval_template.entry_template);
                },
                .stored_const => {},
                .reserved => coordinatorInvariant("relation lowering dependency reached unsealed const template", .{}),
            }
        }

        fn appendResolvedValueRefs(
            self: *RelationLoweringDependencyCollector,
            view: CheckedArtifact.ImportedModuleView,
            table: CheckedArtifact.ResolvedValueRefTableRef,
        ) Allocator.Error!void {
            const end = table.start + table.len;
            if (end > view.resolved_value_refs.template_refs.len) {
                coordinatorInvariant("relation lowering dependency resolved-ref span was outside table", .{});
            }
            for (view.resolved_value_refs.template_refs[table.start..end]) |ref_id| {
                const raw = @intFromEnum(ref_id);
                if (raw >= view.resolved_value_refs.records.len) {
                    coordinatorInvariant("relation lowering dependency resolved-ref id was outside table", .{});
                }
                try self.appendResolvedValueRef(view, view.resolved_value_refs.records[raw].ref);
            }
        }

        fn appendResolvedValueRef(
            self: *RelationLoweringDependencyCollector,
            view: CheckedArtifact.ImportedModuleView,
            ref: CheckedArtifact.ResolvedValueRef,
        ) Allocator.Error!void {
            switch (ref) {
                .top_level_const,
                .imported_const,
                => |const_use| try self.appendConstRef(const_use.const_ref),
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .promoted_top_level_proc,
                => |procedure| try self.appendProcedureUse(procedure),
                .platform_required_const => |required| {
                    try self.appendConstRef(required.const_use.const_ref);
                    try self.appendPlatformRequiredBindingClosure(view, required.binding);
                },
                .platform_required_proc => |required| {
                    try self.appendProcedureUse(required.procedure);
                    try self.appendPlatformRequiredBindingClosure(view, required.binding);
                },
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                .local_proc,
                .platform_required_declaration,
                => {},
            }
        }

        fn appendProcedureUse(
            self: *RelationLoweringDependencyCollector,
            procedure: CheckedArtifact.ProcedureUseTemplate,
        ) Allocator.Error!void {
            switch (procedure.binding) {
                .top_level => |top_level| try self.appendTopLevelProcedureBinding(top_level),
                .imported => |imported| try self.appendImportedProcedureBinding(imported),
                .hosted => |hosted| try self.appendProcedureTemplateRef(hosted.template),
                .platform_required => |required| try self.appendArtifactKey(required.artifact),
            }
        }

        fn appendTopLevelProcedureBinding(
            self: *RelationLoweringDependencyCollector,
            binding_ref: CheckedArtifact.ArtifactTopLevelProcedureBindingRef,
        ) Allocator.Error!void {
            try self.appendArtifactKey(binding_ref.artifact);
            const view = self.viewForKey(binding_ref.artifact);
            const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
            try self.appendProcedureBindingBody(binding_ref.artifact, binding.body);
        }

        fn appendProcedureBindingBody(
            self: *RelationLoweringDependencyCollector,
            owner_key: CheckedArtifact.CheckedModuleArtifactKey,
            body: CheckedArtifact.ProcedureBindingBody,
        ) Allocator.Error!void {
            switch (body) {
                .direct_template => |direct| try self.appendCallableProcedureTemplateRef(direct.template),
                .callable_eval_template => |template| try self.appendCallableEvalTemplateRef(.{
                    .artifact = owner_key,
                    .template = template,
                }),
            }
        }

        fn appendCallableProcedureTemplateRef(
            self: *RelationLoweringDependencyCollector,
            template: canonical.CallableProcedureTemplateRef,
        ) Allocator.Error!void {
            switch (template) {
                .checked => |checked| try self.appendProcedureTemplateRef(checked),
                .synthetic => |synthetic| try self.appendProcedureTemplateRef(synthetic.template),
                .lifted => coordinatorInvariant("relation lowering dependency reached lifted procedure template before mono", .{}),
            }
        }

        fn appendImportedProcedureBinding(
            self: *RelationLoweringDependencyCollector,
            binding_ref: CheckedArtifact.ImportedProcedureBindingRef,
        ) Allocator.Error!void {
            try self.appendArtifactKey(binding_ref.artifact);
            const view = self.viewForKey(binding_ref.artifact);
            for (view.exported_procedure_bindings.bindings) |binding| {
                if (binding.binding.def != binding_ref.def or binding.binding.pattern != binding_ref.pattern) continue;
                try self.appendClosure(binding.template_closure);
                try self.appendImportedProcedureBindingBody(binding_ref.artifact, binding.body);
                return;
            }
            coordinatorInvariant("relation lowering dependency imported procedure had no exported binding closure", .{});
        }

        fn appendImportedProcedureBindingBody(
            self: *RelationLoweringDependencyCollector,
            owner_key: CheckedArtifact.CheckedModuleArtifactKey,
            body: CheckedArtifact.ImportedProcedureBindingBody,
        ) Allocator.Error!void {
            switch (body) {
                .direct_template => |direct| try self.appendCallableProcedureTemplateRef(direct.template),
                .callable_eval_template => |template| try self.appendCallableEvalTemplateRef(.{
                    .artifact = owner_key,
                    .template = template,
                }),
            }
        }

        fn appendPlatformRequiredBindingClosure(
            self: *RelationLoweringDependencyCollector,
            view: CheckedArtifact.ImportedModuleView,
            binding_id: CheckedArtifact.PlatformRequiredBindingId,
        ) Allocator.Error!void {
            const binding = view.platform_required_bindings.lookupByBindingId(@intFromEnum(binding_id)) orelse {
                coordinatorInvariant("relation lowering dependency referenced missing platform-required binding", .{});
            };
            switch (binding.value_use) {
                .const_value => |const_use| try self.appendClosure(const_use.relation_template_closure),
                .procedure_value => |procedure| try self.appendClosure(procedure.relation_template_closure),
            }
        }

        fn appendRelationArtifactExportedValueClosure(
            self: *RelationLoweringDependencyCollector,
            relation_artifact: CheckedArtifact.ImportedModuleView,
            binding: CheckedArtifact.PlatformRequiredBinding,
        ) Allocator.Error!void {
            switch (binding.value_use) {
                .procedure_value => {
                    var found = false;
                    for (relation_artifact.exported_procedure_bindings.bindings) |exported| {
                        if (exported.binding.pattern != binding.app_value.pattern) continue;
                        found = true;
                        try self.appendClosure(exported.template_closure);
                        try self.appendImportedProcedureBindingBody(relation_artifact.key, exported.body);
                        for (relation_artifact.exported_procedure_templates.templates) |template| {
                            if (template.def != exported.binding.def) continue;
                            try self.appendClosure(template.template_closure);
                            try self.appendProcedureTemplateRef(template.template);
                        }
                    }
                    if (!found) {
                        coordinatorInvariant("relation lowering dependency could not find exported app procedure binding", .{});
                    }
                },
                .const_value => |const_use| {
                    var found = false;
                    for (relation_artifact.exported_const_templates.templates) |template| {
                        if (template.pattern != binding.app_value.pattern) continue;
                        if (!std.meta.eql(template.const_ref, const_use.const_use.const_ref)) continue;
                        found = true;
                        try self.appendClosure(template.template_closure);
                        try self.appendConstRef(template.const_ref);
                    }
                    if (!found) {
                        coordinatorInvariant("relation lowering dependency could not find exported app const template", .{});
                    }
                },
            }
        }

        fn viewForKey(
            self: *RelationLoweringDependencyCollector,
            key: CheckedArtifact.CheckedModuleArtifactKey,
        ) CheckedArtifact.ImportedModuleView {
            if (self.root_view) |view| {
                if (checkedArtifactKeyEql(view.key, key)) return view;
            }
            if (relationViewByKey(self.relation_artifacts, key)) |view| return view;
            for (self.views.items) |view| {
                if (checkedArtifactKeyEql(view.key, key)) return view;
            }
            const artifact = self.coordinator.checkedArtifactByKey(key) orelse {
                coordinatorInvariant("relation lowering dependency referenced unavailable checked module", .{});
            };
            return CheckedArtifact.importedView(artifact);
        }

        fn relationArtifactContainsKey(
            self: *const RelationLoweringDependencyCollector,
            key: CheckedArtifact.CheckedModuleArtifactKey,
        ) bool {
            return relationViewByKey(self.relation_artifacts, key) != null;
        }
    };

    fn relationViewByKey(
        relation_views: []const CheckedArtifact.ImportedModuleView,
        key: CheckedArtifact.CheckedModuleArtifactKey,
    ) ?CheckedArtifact.ImportedModuleView {
        for (relation_views) |view| {
            if (checkedArtifactKeyEql(view.key, key)) return view;
        }
        return null;
    }

    fn platformRequiredBindingFromRelationInput(
        relation: CheckedArtifact.PlatformAppRelation,
        input: CheckedArtifact.PlatformRequiredBindingInput,
        index: usize,
    ) CheckedArtifact.PlatformRequiredBinding {
        return .{
            .id = @enumFromInt(@as(u32, @intCast(index))),
            .relation = relation.key,
            .module_idx = relation.platform_module_idx,
            .declaration = input.declaration,
            .requires_idx = input.requires_idx,
            .app_value = input.app_value,
            .requested_source_ty = input.requested_source_ty,
            .checked_relation = input.checked_relation,
            .value_use = input.value_use,
        };
    }

    fn checkedArtifactKeyFromArtifactRef(ref: canonical.ArtifactRef) CheckedArtifact.CheckedModuleArtifactKey {
        return .{ .bytes = ref.bytes };
    }

    fn checkedArtifactKeyEql(
        a: CheckedArtifact.CheckedModuleArtifactKey,
        b: CheckedArtifact.CheckedModuleArtifactKey,
    ) bool {
        return std.mem.eql(u8, &a.bytes, &b.bytes);
    }

    fn coordinatorInvariant(comptime message: []const u8, args: anytype) noreturn {
        if (builtin.mode == .Debug) {
            std.debug.panic("compile.coordinator invariant violated: " ++ message, args);
        }
        unreachable;
    }

    fn appendPublicApiDependencyViewByKey(
        self: *Coordinator,
        views: *std.ArrayList(check.CheckedArtifact.ImportedModuleView),
        allocator: Allocator,
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) Allocator.Error!void {
        if (std.mem.eql(u8, &key.bytes, &root_artifact.key.bytes)) return;
        if (rootRelationContainsArtifact(root_artifact, key)) return;
        if (importedArtifactViewExists(views.items, key)) return;
        const artifact = self.checkedArtifactByKey(key) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator invariant violated: public API dependency references unavailable checked artifact", .{});
            }
            unreachable;
        };
        try appendImportedArtifactViewIfMissing(views, allocator, root_artifact.key, artifact);
        for (artifact.public_api_dependencies.artifacts) |dependency_key| {
            try self.appendPublicApiDependencyViewByKey(views, allocator, root_artifact, dependency_key);
        }
    }

    fn importedArtifactViewExists(
        views: []const check.CheckedArtifact.ImportedModuleView,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) bool {
        for (views) |view| {
            if (std.mem.eql(u8, &view.key.bytes, &key.bytes)) return true;
        }
        return false;
    }

    /// Finalize the build's executable artifacts (link app + platform, build
    /// the platform-app relation, republish the root artifact).
    ///
    /// Must only be called after `coordinatorLoop` returns and after the
    /// caller has confirmed `hasUserErrors() == false`. Returns
    /// `error.HasUserErrors` if called while user-facing diagnostics exist.
    pub fn finalizeExecutableArtifacts(self: *Coordinator) anyerror!void {
        if (self.hasUserErrors()) return error.HasUserErrors;

        const app_root = self.findRootModule(.app) orelse self.findRootModule(.default_app) orelse {
            return;
        };
        const platform_root = self.findRootModule(.platform) orelse {
            return;
        };

        const platform_declaration_artifact = platform_root.mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.finalizeExecutableArtifacts missing platform declaration artifact", .{});
            }
            unreachable;
        };
        const requirement_context = check.CheckedArtifact.platformRequirementContextKey(platform_declaration_artifact);

        if (app_root.mod.checkedArtifact() == null) return;

        try self.republishCheckedArtifact(app_root.pkg, app_root.mod, .{
            .platform_requirement_context = requirement_context,
        });

        const app_artifact = app_root.mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.finalizeExecutableArtifacts missing app artifact after co-finalization", .{});
            }
            unreachable;
        };
        var relation_result = try check.CheckedArtifact.buildPlatformAppRelation(
            self.gpa,
            platform_declaration_artifact,
            platform_root.mod.moduleEnv().?,
            app_artifact,
        );
        defer relation_result.deinit(self.gpa);

        const relation = switch (relation_result) {
            .relation => |relation| relation,
            .type_mismatch => |mismatch| {
                try self.appendPlatformRequirementTypeMismatchReport(
                    app_root.mod,
                    platform_declaration_artifact,
                    app_artifact,
                    mismatch,
                );
                return;
            },
            .missing_value => |missing| {
                try self.appendPlatformRequirementMissingValueReport(
                    app_root.mod,
                    platform_declaration_artifact,
                    missing,
                );
                return;
            },
        };
        const relation_artifacts = [_]check.CheckedArtifact.ImportedModuleView{
            check.CheckedArtifact.importedView(app_artifact),
        };
        try self.republishCheckedArtifact(platform_root.pkg, platform_root.mod, .{
            .relation_artifacts = &relation_artifacts,
            .platform_app_relation = relation,
        });
    }

    pub fn validatePlatformAppRelationsForCheck(self: *Coordinator) anyerror!void {
        if (self.hasUserErrors()) {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.validatePlatformAppRelationsForCheck called after user-facing errors", .{});
            }
            unreachable;
        }

        const app_root = self.findRootModule(.app) orelse self.findRootModule(.default_app) orelse return;
        const platform_root = self.findRootModule(.platform) orelse return;

        const platform_declaration_artifact = platform_root.mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.validatePlatformAppRelationsForCheck missing platform declaration artifact", .{});
            }
            unreachable;
        };
        const requirement_context = check.CheckedArtifact.platformRequirementContextKey(platform_declaration_artifact);

        if (app_root.mod.checkedArtifact() == null) return;

        try self.republishCheckedArtifact(app_root.pkg, app_root.mod, .{
            .platform_requirement_context = requirement_context,
        });

        const app_artifact = app_root.mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.validatePlatformAppRelationsForCheck missing app artifact after co-finalization", .{});
            }
            unreachable;
        };

        var relation_result = try check.CheckedArtifact.buildPlatformAppRelation(
            self.gpa,
            platform_declaration_artifact,
            platform_root.mod.moduleEnv().?,
            app_artifact,
        );
        defer relation_result.deinit(self.gpa);

        switch (relation_result) {
            .relation => {},
            .type_mismatch => |mismatch| try self.appendPlatformRequirementTypeMismatchReport(
                app_root.mod,
                platform_declaration_artifact,
                app_artifact,
                mismatch,
            ),
            .missing_value => |missing| try self.appendPlatformRequirementMissingValueReport(
                app_root.mod,
                platform_declaration_artifact,
                missing,
            ),
        }
    }

    fn appendPlatformRequirementMissingValueReport(
        self: *Coordinator,
        app_mod: *ModuleState,
        platform_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        missing: check.CheckedArtifact.PlatformRequirementMissingValue,
    ) Allocator.Error!void {
        var report = Report.init(self.gpa, "MISSING REQUIRED VALUE", .runtime_error);
        errdefer report.deinit();

        const required_name = platform_artifact.canonical_names.exportNameText(missing.declaration.platform_name);
        try report.document.addText("The app does not provide ");
        try report.document.addAnnotated(required_name, .inline_code);
        try report.document.addText(", but the platform requires it.");

        try app_mod.reports.append(self.gpa, report);
    }

    fn appendPlatformRequirementTypeMismatchReport(
        self: *Coordinator,
        app_mod: *ModuleState,
        platform_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        app_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        mismatch: check.CheckedArtifact.PlatformRequirementTypeMismatch,
    ) Allocator.Error!void {
        var report = Report.init(self.gpa, "TYPE MISMATCH", .runtime_error);
        errdefer report.deinit();

        const required_name = platform_artifact.canonical_names.exportNameText(mismatch.declaration.platform_name);
        try report.document.addText("The app provides ");
        try report.document.addAnnotated(required_name, .inline_code);
        try report.document.addText(" with a type that does not match the platform's ");
        try report.document.addAnnotated("requires", .inline_code);
        try report.document.addText(" entry.");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        const actual = try check.CheckedArtifact.formatCheckedTypeAlloc(self.gpa, app_artifact, mismatch.actual);
        defer self.gpa.free(actual);
        try report.document.addText("The app provides:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(actual);
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        const expected = try check.CheckedArtifact.formatCheckedTypeAlloc(self.gpa, platform_artifact, mismatch.expected);
        defer self.gpa.free(expected);
        try report.document.addText("But the platform requires:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addCodeBlock(expected);

        try app_mod.reports.append(self.gpa, report);
    }

    pub fn hasUserErrors(self: *const Coordinator) bool {
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                for (mod.reports.items) |rep| {
                    switch (rep.severity) {
                        .info, .warning => {},
                        .runtime_error, .fatal => return true,
                    }
                }
            }
        }
        return false;
    }

    /// One entry yielded by `ReportIter` — a single diagnostic with the package
    /// and module it came from. Pointers borrow from the Coordinator's storage.
    pub const ReportEntry = struct {
        package_name: []const u8,
        module_name: []const u8,
        report: *const Report,
    };

    /// Iterator over every report from every module in every package.
    /// Borrows from `Coordinator` storage — do not mutate while iterating.
    pub const ReportIter = struct {
        pkg_it: std.StringHashMap(*PackageState).Iterator,
        cur_pkg: ?*PackageState = null,
        module_idx: usize = 0,
        report_idx: usize = 0,

        pub fn next(self: *ReportIter) ?ReportEntry {
            while (true) {
                if (self.cur_pkg) |pkg| {
                    if (self.module_idx < pkg.modules.items.len) {
                        const mod = &pkg.modules.items[self.module_idx];
                        if (self.report_idx < mod.reports.items.len) {
                            const report = &mod.reports.items[self.report_idx];
                            self.report_idx += 1;
                            return .{
                                .package_name = pkg.name,
                                .module_name = mod.name,
                                .report = report,
                            };
                        }
                        self.module_idx += 1;
                        self.report_idx = 0;
                        continue;
                    }
                }
                const entry = self.pkg_it.next() orelse return null;
                self.cur_pkg = entry.value_ptr.*;
                self.module_idx = 0;
                self.report_idx = 0;
            }
        }
    };

    /// Iterate over every diagnostic produced during compilation.
    pub fn iterReports(self: *const Coordinator) ReportIter {
        return .{ .pkg_it = self.packages.iterator() };
    }

    pub fn executableRootCheckedArtifact(self: *Coordinator) *const check.CheckedArtifact.CheckedModuleArtifact {
        if (self.findRootModule(.platform)) |platform_root| {
            if (platform_root.mod.checkedArtifact()) |artifact| {
                if (artifact.platform_required_bindings.bindings.len > 0 or
                    artifact.root_requests.requests.len > 0 or
                    artifact.provided_exports.exports.len > 0)
                {
                    return artifact;
                }
            }
        }
        return self.rootCheckedArtifact("app");
    }

    pub fn collectRelationArtifactViews(
        self: *Coordinator,
        allocator: Allocator,
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error![]check.CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(allocator);

        for (root_artifact.platform_required_bindings.bindings) |binding| {
            const artifact = self.checkedArtifactByKey(binding.app_value.artifact) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("compile.coordinator.collectRelationArtifactViews missing app artifact for platform relation", .{});
                }
                unreachable;
            };
            try appendImportedArtifactViewIfMissing(&views, allocator, root_artifact.key, artifact);
        }

        return try views.toOwnedSlice(allocator);
    }

    fn republishCheckedArtifact(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
        publication: compile_package.ArtifactPublicationInputs,
    ) anyerror!void {
        const env = mod.moduleEnv() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.republishCheckedArtifact missing module env for {s}", .{mod.name});
            }
            unreachable;
        };
        const module_env_storage = mod.moduleEnvStorage() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator.republishCheckedArtifact missing module env storage for {s}", .{mod.name});
            }
            unreachable;
        };
        const imported_envs = try self.buildTypecheckImportedEnvs(pkg, mod, self.gpa);
        defer self.gpa.free(imported_envs);
        const imported_artifacts = try self.buildTypecheckImportedArtifacts(pkg, mod, self.gpa);
        defer self.gpa.free(imported_artifacts);
        const available_artifacts = try self.collectTypecheckAvailableArtifactViews(self.gpa, imported_artifacts);
        defer self.gpa.free(available_artifacts);

        var publication_with_availability = publication;
        var relation_available_artifacts: []CheckedArtifact.ImportedModuleView = &.{};
        var relation_available_artifacts_owned = false;
        defer if (relation_available_artifacts_owned) self.gpa.free(relation_available_artifacts);

        if (publication.platform_app_relation) |relation| {
            var extended_available = std.ArrayList(CheckedArtifact.ImportedModuleView).empty;
            errdefer extended_available.deinit(self.gpa);
            try extended_available.appendSlice(self.gpa, available_artifacts);

            const root_key = if (mod.checkedArtifact()) |current| current.key else CheckedArtifact.CheckedModuleArtifactKey{};
            for (publication.relation_artifacts) |relation_artifact| {
                if (checkedArtifactKeyEql(relation_artifact.key, root_key)) continue;
                if (importedArtifactViewExists(extended_available.items, relation_artifact.key)) continue;
                try extended_available.append(self.gpa, relation_artifact);
            }

            var dependency_collector = RelationLoweringDependencyCollector.init(
                self,
                self.gpa,
                root_key,
                null,
                publication.relation_artifacts,
                &extended_available,
            );
            defer dependency_collector.deinit();

            for (relation.bindings, 0..) |binding_input, i| {
                const relation_view = relationViewByKey(publication.relation_artifacts, binding_input.app_value.artifact) orelse {
                    coordinatorInvariant("platform/app relation publication missing relation checked module view", .{});
                };
                const binding = platformRequiredBindingFromRelationInput(relation, binding_input, i);
                try dependency_collector.appendPlatformRelationDependency(relation_view, binding);
            }

            relation_available_artifacts = try extended_available.toOwnedSlice(self.gpa);
            relation_available_artifacts_owned = true;
            publication_with_availability.available_artifacts = relation_available_artifacts;
        } else {
            publication_with_availability.available_artifacts = available_artifacts;
        }

        var artifact = try compile_package.PackageEnv.publishCheckedArtifactFromCheckedModuleWithStorage(
            self.gpa,
            env,
            module_env_storage,
            imported_envs,
            imported_artifacts,
            publication_with_availability,
        );
        var artifact_owned = true;
        errdefer if (artifact_owned) artifact.deinit(self.gpa);
        const artifact_ptr = try allocateCheckedArtifact(artifact);
        var artifact_ptr_owned = true;
        errdefer if (artifact_ptr_owned) destroyCheckedArtifact(artifact_ptr, false);
        artifact_owned = false;
        self.unregisterCheckedArtifact(mod);
        try mod.replaceRepublishedCheckedArtifact(artifact_ptr, &self.retired_checked_artifacts, self.gpa);
        artifact_ptr_owned = false;
        try self.registerCheckedArtifact(pkg, mod);
    }

    const RootModuleRef = struct {
        pkg: *PackageState,
        mod: *ModuleState,
    };

    fn findRootModule(self: *Coordinator, kind: ModuleEnv.ModuleKind.Tag) ?RootModuleRef {
        var pkg_iter = self.packages.iterator();
        while (pkg_iter.next()) |entry| {
            const pkg = entry.value_ptr.*;
            const root_id = pkg.root_module_id orelse continue;
            const mod = pkg.getModule(root_id) orelse continue;
            const env = mod.moduleEnv() orelse continue;
            if (moduleKindTag(env.module_kind) == kind) return .{ .pkg = pkg, .mod = mod };
        }
        return null;
    }

    fn moduleKindTag(kind: ModuleEnv.ModuleKind) ModuleEnv.ModuleKind.Tag {
        return switch (kind) {
            .type_module => .type_module,
            .default_app => .default_app,
            .app => .app,
            .package => .package,
            .platform => .platform,
            .hosted => .hosted,
            .module => .module,
            .malformed => .malformed,
        };
    }

    fn checkedArtifactByKey(
        self: *Coordinator,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) ?*const check.CheckedArtifact.CheckedModuleArtifact {
        if (std.mem.eql(u8, &self.builtin_modules.checked_artifact.key.bytes, &key.bytes)) {
            return &self.builtin_modules.checked_artifact;
        }

        const location = self.checked_artifact_index.get(key.bytes) orelse return null;
        const pkg = self.packages.get(location.pkg_name) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator checked artifact registry points at missing package {s}", .{location.pkg_name});
            }
            unreachable;
        };
        const mod = pkg.getModule(location.module_id) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator checked artifact registry points at missing module {d} in package {s}", .{ location.module_id, location.pkg_name });
            }
            unreachable;
        };
        const artifact = mod.checkedArtifact() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator checked artifact registry points at unpublished module {s}:{d}", .{ location.pkg_name, location.module_id });
            }
            unreachable;
        };
        if (!std.mem.eql(u8, &artifact.key.bytes, &key.bytes)) {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator checked artifact registry returned stale key for {s}:{d}", .{ location.pkg_name, location.module_id });
            }
            unreachable;
        }
        return artifact;
    }

    fn unregisterCheckedArtifact(self: *Coordinator, mod: *ModuleState) void {
        if (mod.checkedArtifact()) |artifact| {
            _ = self.checked_artifact_index.remove(artifact.key.bytes);
        }
    }

    fn registerCheckedArtifact(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
    ) Allocator.Error!void {
        const artifact = mod.checkedArtifact() orelse return;
        const module_id = moduleIdForPtr(pkg, mod) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("compile.coordinator could not locate checked artifact module {s} in package {s}", .{ mod.name, pkg.name });
            }
            unreachable;
        };
        try self.checked_artifact_index.put(artifact.key.bytes, .{
            .pkg_name = pkg.name,
            .module_id = module_id,
        });
    }

    fn moduleIdForPtr(pkg: *PackageState, mod: *ModuleState) ?ModuleId {
        for (pkg.modules.items, 0..) |*candidate, raw| {
            if (@intFromPtr(candidate) == @intFromPtr(mod)) return @intCast(raw);
        }
        return null;
    }

    fn appendImportedArtifactViewIfMissing(
        views: *std.ArrayList(check.CheckedArtifact.ImportedModuleView),
        allocator: Allocator,
        root_key: check.CheckedArtifact.CheckedModuleArtifactKey,
        artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error!void {
        if (std.mem.eql(u8, &artifact.key.bytes, &root_key.bytes)) return;
        for (views.items) |view| {
            if (std.mem.eql(u8, &view.key.bytes, &artifact.key.bytes)) return;
        }
        try views.append(allocator, check.CheckedArtifact.importedView(artifact));
    }

    /// Start the coordinator and spawn worker threads (for multi-threaded mode).
    /// max_threads <= 1 is treated as single-threaded (inline execution); callers
    /// that want auto-detection should resolve 0 to the CPU count before init.
    pub fn start(self: *Coordinator) (Allocator.Error || std.Thread.SpawnError)!void {
        if (self.mode == .single_threaded or self.max_threads <= 1) return;
        if (comptime !is_freestanding) {
            const n = if (self.max_threads == 0) (std.Thread.getCpuCount() catch 1) else self.max_threads;

            try self.workers.ensureTotalCapacity(self.gpa, n);
            var i: usize = 0;
            while (i < n) : (i += 1) {
                const th = try std.Thread.spawn(.{}, workerThread, .{self});
                try self.workers.append(self.gpa, th);
            }
        }
    }

    /// Shutdown workers and wait for them to complete.
    /// Workers stop promptly: they finish their current task but do not
    /// pick up additional queued work.
    pub fn shutdown(self: *Coordinator) void {
        if (!threads_available) return;

        // Signal workers to stop before closing channels, so workers that
        // are between recv() calls see the flag and exit instead of
        // processing more buffered tasks.
        self.shutting_down.store(true, .release);

        // Close both channels to wake any workers blocked in recv()/send()
        self.result_channel.close();
        self.task_channel.close();

        // Wait for workers to finish
        for (self.workers.items) |w| {
            w.join();
        }
        self.workers.clearRetainingCapacity();
    }

    /// Enqueue a task for processing
    pub fn enqueueTask(self: *Coordinator, task: WorkerTask) Allocator.Error!void {
        if (comptime trace_build) {
            switch (task) {
                .parse => |t| std.debug.print("[COORD] ENQUEUE parse: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .canonicalize => |t| std.debug.print("[COORD] ENQUEUE canonicalize: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
                .type_check => |t| std.debug.print("[COORD] ENQUEUE type_check: pkg={s} module={s}\n", .{ t.package_name, t.module_name }),
            }
        }
        // Increment inflight BEFORE sending to the channel. This ensures there is
        // no window where a worker could recv, execute, and the coordinator could
        // process the result (decrementing inflight) before we increment here.
        // Only do this when workers are actually running (same guard as start() and
        // the multi-threaded branch in coordinatorLoop). When max_threads <= 1 the
        // coordinator processes tasks inline and the single-threaded path never
        // decrements inflight, so incrementing here would make isComplete() hang.
        const has_workers = threads_available and self.mode == .multi_threaded and self.max_threads > 1;
        if (has_workers) {
            _ = self.inflight.fetchAdd(1, .monotonic);
        }
        self.task_channel.sendGrowable(task) catch |err| switch (err) {
            error.Closed => {
                if (has_workers) {
                    _ = self.inflight.fetchSub(1, .monotonic);
                }
                return;
            },
            error.OutOfMemory => {
                if (has_workers) {
                    _ = self.inflight.fetchSub(1, .monotonic);
                }
                return error.OutOfMemory;
            },
        };
    }

    /// Enqueue a parse task for a module
    pub fn enqueueParseTask(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) Allocator.Error!void {
        const pkg = self.packages.get(pkg_name) orelse return;
        const mod = pkg.getModule(module_id) orelse return;

        mod.phase = .Parsing;

        try self.enqueueTask(.{
            .parse = .{
                .package_name = pkg.name, // Use pkg's owned name, not the passed-in reference
                .module_id = module_id,
                .module_name = mod.name,
                .path = mod.path,
                .module_role = mod.module_role,
                .depth = mod.depth,
            },
        });
    }

    /// Main coordinator loop - unified for single and multi-threaded modes
    pub fn coordinatorLoop(self: *Coordinator) (Allocator.Error || error{ UnsupportedBuiltinAnnotationOnly, BuiltinLowLevelAnnotationMustBeFunction, LowLevelOperationsNotFound })!void {
        var inline_worker_allocs = WorkerAllocators.init(self.gpa);
        defer inline_worker_allocs.deinit();
        var iterations_without_progress: u32 = 0;

        while (!self.isComplete()) {
            // A worker thread ran out of memory while producing a result. It
            // cannot return the error itself, so it recorded the OOM here.
            if (self.worker_oom.load(.acquire)) return error.OutOfMemory;

            var made_progress = false;

            if (!threads_available or self.mode == .single_threaded or self.max_threads <= 1) {
                // Single-threaded: process tasks inline
                if (self.task_channel.tryRecv()) |task| {
                    const result = try self.executeTaskInline(task, inline_worker_allocs.taskAllocators());
                    inline_worker_allocs.resetArena();
                    try self.handleResult(result);
                    made_progress = true;
                }
            } else {
                // Multi-threaded: receive from workers via channel
                // Use blocking recv with timeout to avoid busy spinning
                if (self.result_channel.recvTimeout(10_000_000)) |result| { // 10ms timeout
                    _ = self.inflight.fetchSub(1, .monotonic);
                    try self.handleResult(result);
                    made_progress = true;
                }
            }

            // If no progress was made, try to unblock modules waiting on external imports
            if (!made_progress) {
                if (try self.tryUnblockAllWaiting()) {
                    made_progress = true;
                }
            }

            if (made_progress) {
                iterations_without_progress = 0;
            } else if (self.inflight.load(.acquire) > 0) {
                // A worker is still processing a task or has one queued.
                // `inflight` is incremented in enqueueTask before the task is
                // sent to the channel, so it covers both "queued" and
                // "executing". The wall-clock stuck check below would
                // false-positive on overloaded CI when a worker is merely
                // slow, so don't advance the counter while inflight > 0.
                iterations_without_progress = 0;
            } else {
                iterations_without_progress += 1;
                if (iterations_without_progress > 1000) {
                    if (comptime !threading.is_freestanding) {
                        const task_count = self.task_channel.len();
                        std.debug.print("Coordinator stuck: remaining={}, tasks={}, inflight={}\n", .{
                            self.total_remaining,
                            task_count,
                            self.inflight.load(.acquire),
                        });
                        // Print package/module states with detailed diagnostics
                        var pkg_it = self.packages.iterator();
                        while (pkg_it.next()) |entry| {
                            const pkg = entry.value_ptr.*;
                            std.debug.print("  Package {s}: remaining={}, modules={}\n", .{
                                pkg.name,
                                pkg.remaining_modules,
                                pkg.modules.items.len,
                            });
                            for (pkg.modules.items, 0..) |mod, i| {
                                std.debug.print("    Module {}: {s} phase=.{s} ext_imports={}\n", .{
                                    i,
                                    mod.name,
                                    @tagName(mod.phase),
                                    mod.external_imports.items.len,
                                });
                                // For non-Done modules, print additional diagnostics
                                if (mod.phase != .Done) {
                                    // Print local imports and their status
                                    if (mod.imports.items.len > 0) {
                                        std.debug.print("      local_imports ({}):", .{mod.imports.items.len});
                                        for (mod.imports.items) |imp_id| {
                                            if (pkg.getModule(imp_id)) |imp_mod| {
                                                std.debug.print(" {s}(.{s})", .{ imp_mod.name, @tagName(imp_mod.phase) });
                                            } else {
                                                std.debug.print(" <invalid id={}>", .{imp_id});
                                            }
                                        }
                                        std.debug.print("\n", .{});
                                    }
                                    // Print external imports and their readiness
                                    if (mod.external_imports.items.len > 0) {
                                        std.debug.print("      ext_imports ({}):", .{mod.external_imports.items.len});
                                        for (mod.external_imports.items) |ext_name| {
                                            const ready = self.isExternalReady(pkg.name, ext_name);
                                            std.debug.print(" {s}(ready={})", .{ ext_name, ready });
                                        }
                                        std.debug.print("\n", .{});
                                    }
                                }
                            }
                        }
                        std.debug.print("\n", .{});
                    }
                    @panic("Coordinator stuck in infinite loop");
                }
            }
        }
    }

    /// Try to unblock all modules waiting on external imports
    /// Returns true if any module was unblocked
    fn tryUnblockAllWaiting(self: *Coordinator) Allocator.Error!bool {
        var any_unblocked = false;
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items, 0..) |*mod, idx| {
                if (mod.phase == .WaitingOnImports) {
                    const old_phase = mod.phase;
                    try self.tryUnblock(pkg, @intCast(idx));
                    if (mod.phase != old_phase) {
                        any_unblocked = true;
                    }
                }
            }
        }
        return any_unblocked;
    }

    /// Check if all work is complete.
    ///
    /// Thread-safety: This is only called from the coordinator thread.
    /// `total_remaining` is only mutated by the coordinator, so it's always fresh.
    /// `inflight` is incremented *before* a task enters the channel and decremented
    /// only when the coordinator processes the result, so there is no window where
    /// inflight is 0 while work is still pending. The `isEmpty` check is therefore
    /// redundant but kept as a defensive invariant.
    pub fn isComplete(self: *Coordinator) bool {
        return self.total_remaining == 0 and self.inflight.load(.acquire) == 0 and self.task_channel.isEmpty();
    }

    /// Execute a task inline with explicit worker allocators.
    fn executeTaskInline(self: *Coordinator, task: WorkerTask, allocators: WorkerTaskAllocators) Allocator.Error!WorkerResult {
        return switch (task) {
            .parse => |t| self.executeParse(t, allocators),
            .canonicalize => |t| self.executeCanonicalize(t, allocators),
            .type_check => |t| self.executeTypeCheck(t, allocators),
        };
    }

    fn createOwnedSemanticResult(
        allocator: Allocator,
        env: *ModuleEnv,
        checked_artifact: ?check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error!*messages.OwnedSemanticModuleData {
        const semantic = try allocator.create(messages.OwnedSemanticModuleData);
        semantic.* = .{
            .module_env = env,
            .checked_artifact = checked_artifact,
        };
        return semantic;
    }

    fn appendReportOwned(allocator: Allocator, reports: *std.ArrayList(Report), report: Report) Allocator.Error!void {
        var owned = report;
        errdefer owned.deinit();
        try reports.append(allocator, owned);
    }

    fn deinitReports(reports: *std.ArrayList(Report), allocator: Allocator) void {
        for (reports.items) |*rep| rep.deinit();
        reports.deinit(allocator);
    }

    fn destroyModuleEnvAndSource(env: *ModuleEnv) void {
        const env_alloc = env.gpa;
        const source = env.common.source;
        env.deinit();
        env_alloc.destroy(env);
        if (source.len > 0) env_alloc.free(@constCast(source));
    }

    fn appendWorkerFailureReport(
        self: *Coordinator,
        allocator: Allocator,
        reports: *std.ArrayList(Report),
        title: []const u8,
        path: []const u8,
        err: anyerror,
    ) void {
        var rep = Report.init(allocator, title, .fatal);
        const msg = std.fmt.allocPrint(allocator, "{s}: {s}", .{ path, @errorName(err) }) catch null;
        defer if (msg) |owned| allocator.free(owned);
        rep.addErrorMessage(msg orelse @errorName(err)) catch |report_err| {
            self.bugReport("BUG: failed to add worker failure report message for {s}: {s}\n", .{ path, @errorName(report_err) });
        };
        reports.append(allocator, rep) catch |append_err| {
            rep.deinit();
            self.bugReport("BUG: failed to append worker failure report for {s}: {s}\n", .{ path, @errorName(append_err) });
        };
    }

    fn workerFailureReports(
        self: *Coordinator,
        allocator: Allocator,
        title: []const u8,
        path: []const u8,
        err: anyerror,
    ) std.ArrayList(Report) {
        var reports = std.ArrayList(Report).empty;
        self.appendWorkerFailureReport(allocator, &reports, title, path, err);
        return reports;
    }

    fn serializeModuleEnvForCache(self: *Coordinator, env: *const ModuleEnv) Allocator.Error![]u8 {
        const allocator = self.cache_manager.?.allocator;
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        var writer = CompactWriter.init();
        defer writer.deinit(arena_alloc);

        const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
        try serialized.serialize(env, arena_alloc, &writer);

        const body = try allocator.alloc(u8, writer.total_bytes);
        errdefer allocator.free(body);
        _ = writer.writeToBuffer(body) catch unreachable;
        return body;
    }

    fn checkedModuleCacheKey(
        self: *Coordinator,
        env: *ModuleEnv,
        imported_envs: []const *ModuleEnv,
        imported_artifacts: []const check.CheckedArtifact.PublishImportArtifact,
    ) Allocator.Error!check.CheckedArtifact.CheckedModuleArtifactKey {
        var imported_source_count: usize = 0;
        for (imported_envs) |imported_env| {
            if (env.module_role == .builtin and imported_env.module_role == .builtin) continue;
            imported_source_count += 1;
        }

        var source_modules = try self.gpa.alloc(CheckedModules.SourceModule, imported_source_count + 1);
        defer self.gpa.free(source_modules);

        var source_index: usize = 0;
        for (imported_envs) |imported_env| {
            if (env.module_role == .builtin and imported_env.module_role == .builtin) continue;
            source_modules[source_index] = .{ .precompiled = imported_env };
            source_index += 1;
        }

        const module_idx: u32 = @intCast(imported_source_count);
        source_modules[imported_source_count] = .{ .precompiled = env };

        var typed_modules = try CheckedModules.init(self.gpa, source_modules);
        defer typed_modules.deinit();

        return check.CheckedArtifact.checkedModuleKeyFromTypedModule(
            self.gpa,
            &typed_modules,
            module_idx,
            .{ .imports = imported_artifacts },
        );
    }

    fn resolvedDirectImportsHaveCheckedOutput(
        env: *const ModuleEnv,
        checked_imports: []const check.CheckedArtifact.PublishImportArtifact,
    ) bool {
        for (env.imports.imports.items.items, 0..) |_, i| {
            const import_idx: CIR.Import.Idx = @enumFromInt(@as(u32, @intCast(i)));
            const resolved_module_idx = env.imports.getResolvedModule(import_idx) orelse continue;

            var found = false;
            for (checked_imports) |checked_import| {
                if (checked_import.module_idx == resolved_module_idx) {
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }

        return true;
    }

    fn storeCheckedModuleInCache(self: *Coordinator, artifact: *const check.CheckedArtifact.CheckedModuleArtifact) void {
        const manager = self.cache_manager orelse return;
        if (!manager.config.enabled) return;

        const entries_dir = manager.config.getCheckedArtifactCacheDir(manager.allocator) catch {
            manager.stats.recordStoreFailure();
            return;
        };
        defer manager.allocator.free(entries_dir);

        const body = self.serializeModuleEnvForCache(artifact.moduleEnvConst()) catch {
            manager.stats.recordStoreFailure();
            return;
        };
        defer manager.allocator.free(body);

        const entry = encodeCheckedModuleCacheEntry(manager.allocator, artifact.key, body) catch {
            manager.stats.recordStoreFailure();
            return;
        };
        defer manager.allocator.free(entry);

        manager.storeRawBytes(artifact.key.bytes, entry, entries_dir);
    }

    fn tryLoadCachedCheckedModule(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
        imported_envs: []const *ModuleEnv,
        imported_artifacts: []const check.CheckedArtifact.PublishImportArtifact,
        available_artifacts: []const check.CheckedArtifact.ImportedModuleView,
    ) bool {
        const manager = self.cache_manager orelse return false;
        if (!manager.config.enabled) return false;

        const current_env = mod.moduleEnv() orelse return false;
        if (!resolvedDirectImportsHaveCheckedOutput(current_env, imported_artifacts)) return false;
        const cache_key = self.checkedModuleCacheKey(current_env, imported_envs, imported_artifacts) catch return false;

        const entries_dir = manager.config.getCheckedArtifactCacheDir(manager.allocator) catch {
            manager.stats.recordMiss();
            return false;
        };
        defer manager.allocator.free(entries_dir);

        const entry = manager.loadRawBytes(cache_key.bytes, entries_dir) orelse return false;
        defer manager.allocator.free(entry);

        const body = decodeCheckedModuleCacheEntry(cache_key, entry) orelse {
            manager.stats.recordInvalidation();
            return false;
        };

        const module_alloc = self.getModuleAllocator();
        const buffer = module_alloc.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, body.len) catch {
            manager.stats.recordInvalidation();
            return false;
        };
        @memcpy(buffer, body);
        var buffer_owned = true;
        defer if (buffer_owned) module_alloc.free(buffer);

        const source = module_alloc.dupe(u8, current_env.common.source) catch {
            manager.stats.recordInvalidation();
            return false;
        };
        var source_owned = true;
        defer if (source_owned) module_alloc.free(source);

        const serialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
        const cached_env = serialized_ptr.deserializeWithMutableTypes(
            @intFromPtr(buffer.ptr),
            module_alloc,
            source,
            mod.name,
        ) catch {
            manager.stats.recordInvalidation();
            return false;
        };
        var cached_env_owned = true;
        defer if (cached_env_owned) {
            cached_env.deinitCachedModule();
            module_alloc.destroy(cached_env);
        };

        var artifact = compile_package.PackageEnv.publishCheckedArtifactFromCheckedModuleWithStorage(
            module_alloc,
            cached_env,
            .{ .cached_buffer = .{
                .env = cached_env,
                .buffer = buffer,
                .source = source,
            } },
            imported_envs,
            imported_artifacts,
            .{ .available_artifacts = available_artifacts },
        ) catch {
            manager.stats.recordInvalidation();
            return false;
        };
        cached_env_owned = false;
        buffer_owned = false;
        source_owned = false;
        var artifact_owned = true;
        defer if (artifact_owned) artifact.deinit(artifact.canonical_names.allocator);

        if (!std.mem.eql(u8, &artifact.key.bytes, &cache_key.bytes)) {
            manager.stats.recordInvalidation();
            return false;
        }
        const artifact_ptr = allocateCheckedArtifact(artifact) catch {
            manager.stats.recordInvalidation();
            return false;
        };
        artifact_owned = false;

        const old_env = current_env;
        const old_env_alloc = old_env.gpa;
        const old_source = old_env.common.source;
        const old_had_artifact = mod.checkedArtifact() != null;

        self.unregisterCheckedArtifact(mod);
        if (mod.semantic) |*semantic| {
            if (semantic.checked_artifact) |existing| {
                self.retired_checked_artifacts.append(self.gpa, .{
                    .artifact = existing,
                    .retain_module_env = @intFromPtr(existing.moduleEnv()) == @intFromPtr(artifact_ptr.moduleEnv()),
                }) catch {
                    destroyCheckedArtifact(artifact_ptr, false);
                    manager.stats.recordInvalidation();
                    return false;
                };
            }
            semantic.module_env = cached_env;
            semantic.checked_artifact = artifact_ptr;
        } else {
            destroyCheckedArtifact(artifact_ptr, false);
            return false;
        }

        self.registerCheckedArtifact(pkg, mod) catch {
            if (mod.semantic) |*semantic| {
                semantic.module_env = old_env;
                semantic.checked_artifact = null;
            }
            destroyCheckedArtifact(artifact_ptr, false);
            manager.stats.recordInvalidation();
            return false;
        };

        if (!old_had_artifact) {
            old_env.deinit();
            old_env_alloc.destroy(old_env);
            if (old_source.len > 0) old_env_alloc.free(@constCast(old_source));
        }

        return true;
    }

    fn finishCachedModule(self: *Coordinator, pkg: *PackageState, mod: *ModuleState) Allocator.Error!void {
        const module_time = mod.compile_time_ns;
        if (module_time < self.module_time_min_ns) self.module_time_min_ns = module_time;
        if (module_time > self.module_time_max_ns) self.module_time_max_ns = module_time;
        self.module_time_sum_ns += module_time;

        mod.phase = .Done;
        mod.visit_color = .black;

        self.cache_hits += 1;

        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        const module_id = pkg.getModuleId(mod.name) orelse return;
        try self.wakeCrossPackageDependents(pkg.name, module_id);
    }

    /// Write a BUG diagnostic to stderr via the injected Io. No-op in release builds.
    fn bugReport(self: *Coordinator, comptime fmt: []const u8, args: anytype) void {
        if (comptime builtin.mode == .Debug) {
            var buf: [2048]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, fmt, args) catch fmt;
            self.roc_ctx.writeStderr(msg) catch {};
        }
    }

    /// Handle a result from a worker
    fn handleResult(self: *Coordinator, result: WorkerResult) (Allocator.Error || error{ UnsupportedBuiltinAnnotationOnly, BuiltinLowLevelAnnotationMustBeFunction, LowLevelOperationsNotFound })!void {
        // Make a mutable copy so we can deinit after handling
        var res = result;
        // Use worker allocator to match what workers used for allocation
        defer res.deinit(self.getWorkerAllocator());

        // Capture by pointer so handlers can clear reports to transfer ownership
        switch (res) {
            .parsed => |*r| try self.handleParsed(r),
            .canonicalized => |*r| try self.handleCanonicalized(r),
            .type_checked => |*r| try self.handleTypeChecked(r),
            .parse_failed => |*r| try self.handleParseFailed(r),
            .compile_failed => |*r| try self.handleCompileFailed(r),
            .cycle_detected => |*r| try self.handleCycleDetected(r),
            .worker_oom => return error.OutOfMemory,
        }
    }

    /// Handle a successful parse result
    fn handleParsed(self: *Coordinator, result: *ParsedResult) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for parsed result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for parsed result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: mod.reports BEFORE: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of module env and AST
        mod.replaceModuleEnv(result.module_env);
        mod.cached_ast = result.cached_ast;

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] PARSED: mod.reports AFTER: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Update timing
        self.total_parse_ns += result.parse_ns;
        mod.compile_time_ns += result.parse_ns;

        for (result.discovered_local_imports.items) |imp| {
            const child_id = try pkg.ensureModule(self.gpa, imp.module_name, imp.path);
            const current_mod = pkg.getModule(result.module_id) orelse {
                self.bugReport("BUG: module id={} not found in package '{s}' after ensureModule in parsed handler (module={s})\n", .{
                    result.module_id, result.package_name, result.module_name,
                });
                unreachable;
            };

            if (child_id == result.module_id or pkg.moduleReaches(child_id, result.module_id)) {
                try self.handleCycleInline(pkg, result.module_id, child_id);
                return;
            }

            try current_mod.imports.append(self.gpa, child_id);

            const child = pkg.getModule(child_id).?;
            try child.dependents.append(self.gpa, result.module_id);
            const new_depth = current_mod.depth +| 1;
            if (new_depth < child.depth) {
                child.depth = new_depth;
            }

            try pkg.recordLocalImportReachability(self.gpa, result.module_id, child_id);

            if (child.phase == .Parse) {
                pkg.remaining_modules += 1;
                self.total_remaining += 1;
                try self.enqueueParseTask(result.package_name, child_id);
            }
        }

        const mod_after_imports = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' after local parse imports (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        for (result.discovered_external_imports.items) |ext_imp| {
            try mod_after_imports.external_imports.append(self.gpa, try self.gpa.dupe(u8, ext_imp.import_name));
            try self.scheduleExternalImport(result.package_name, ext_imp.import_name);

            const qualified = base.module_path.parseQualifiedImport(ext_imp.import_name) orelse continue;
            const target_pkg_name = pkg.shorthands.get(qualified.qualifier) orelse continue;
            const target_pkg = self.packages.get(target_pkg_name) orelse continue;
            const target_module_id = target_pkg.module_names.get(qualified.module) orelse continue;
            try self.registerCrossPackageDependent(
                target_pkg_name,
                target_module_id,
                result.package_name,
                result.module_id,
            );
        }

        mod_after_imports.phase = .WaitingOnImports;
        try self.tryUnblock(pkg, result.module_id);
    }

    /// Handle a successful canonicalization result
    fn handleCanonicalized(self: *Coordinator, result: *CanonicalizedResult) (Allocator.Error || error{ UnsupportedBuiltinAnnotationOnly, BuiltinLowLevelAnnotationMustBeFunction, LowLevelOperationsNotFound })!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: pkg={s} module={s} result_reports={}\n", .{
                result.package_name,
                result.module_name,
                result.reports.items.len,
            });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for canonicalized result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for canonicalized result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: mod.reports BEFORE: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of module env
        mod.replaceModuleEnv(result.module_env);
        mod.cached_ast = null; // AST was consumed during canonicalization

        if (mod.moduleEnv()) |env| {
            if (can.BuiltinLowLevel.isBuiltinModule(env)) {
                try can.BuiltinLowLevel.apply(env);
            } else if (self.enable_hosted_transform) {
                // Only run for platform modules (packages other than "app")
                // The app package doesn't need hosted lambdas
                if (!std.mem.eql(u8, result.package_name, "app")) {
                    if (can.HostedCompiler.replaceAnnoOnlyWithHosted(env)) |modified_defs| {
                        var defs = modified_defs;
                        defs.deinit(env.gpa);
                    } else |_| {}
                }
            }
        }

        // Append reports - we take ownership, so clear result.reports after copying.
        for (result.reports.items, 0..) |rep, ri| {
            if (comptime trace_build) {
                std.debug.print("[COORD] CANONICALIZED: result report {}: owned_strings.len={}\n", .{ ri, rep.owned_strings.items.len });
                if (rep.owned_strings.items.len > 0) {
                    std.debug.print("[COORD] CANONICALIZED: first owned_string ptr={} len={}\n", .{ @intFromPtr(rep.owned_strings.items[0].ptr), rep.owned_strings.items[0].len });
                }
            }
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] CANONICALIZED: mod ptr={} mod.reports AFTER: len={} cap={}\n", .{ @intFromPtr(mod), mod.reports.items.len, mod.reports.capacity });
            if (mod.reports.items.len > 0) {
                const mr = &mod.reports.items[0];
                std.debug.print("[COORD] CANONICALIZED: mod.reports.items.ptr={} owned_strings.len={}\n", .{ @intFromPtr(mod.reports.items.ptr), mr.owned_strings.items.len });
                if (mr.owned_strings.items.len > 0) {
                    std.debug.print("[COORD] CANONICALIZED: mod.reports[0] first owned_string ptr={} len={}\n", .{ @intFromPtr(mr.owned_strings.items[0].ptr), mr.owned_strings.items[0].len });
                }
            }
        }

        // Update timing
        self.total_canonicalize_ns += result.canonicalize_ns;
        self.total_canonicalize_diag_ns += result.canonicalize_diagnostics_ns;
        mod.compile_time_ns += result.canonicalize_ns + result.canonicalize_diagnostics_ns;

        const task_payload_alloc = self.getWorkerAllocator();
        const imported_envs = try self.buildTypecheckImportedEnvs(pkg, mod, task_payload_alloc);
        errdefer task_payload_alloc.free(imported_envs);
        const imported_artifacts = try self.buildTypecheckImportedArtifacts(pkg, mod, task_payload_alloc);
        errdefer task_payload_alloc.free(imported_artifacts);
        const available_artifacts = try self.collectTypecheckAvailableArtifactViews(task_payload_alloc, imported_artifacts);
        errdefer task_payload_alloc.free(available_artifacts);

        if (mod.reports.items.len == 0 and
            self.tryLoadCachedCheckedModule(pkg, mod, imported_envs, imported_artifacts, available_artifacts))
        {
            task_payload_alloc.free(imported_envs);
            task_payload_alloc.free(imported_artifacts);
            task_payload_alloc.free(available_artifacts);
            try self.finishCachedModule(pkg, mod);
            return;
        }

        mod.phase = .TypeCheck;
        mod.visit_color = .black;
        try self.enqueueTask(.{
            .type_check = .{
                .package_name = pkg.name,
                .module_id = result.module_id,
                .module_name = mod.name,
                .path = mod.path,
                .module_env = mod.moduleEnv().?,
                .imported_envs = imported_envs,
                .imported_artifacts = imported_artifacts,
                .available_artifacts = available_artifacts,
            },
        });
    }

    /// Handle a successful type-check result
    fn handleTypeChecked(self: *Coordinator, result: *TypeCheckedResult) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: pkg={s} module={s} result_reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for type_checked result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for type_checked result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: mod.reports BEFORE append: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Take ownership of semantic module data
        mod.replaceModuleEnv(result.semantic.module_env);
        if (result.semantic.checked_artifact) |artifact| {
            self.unregisterCheckedArtifact(mod);
            const artifact_ptr = try allocateCheckedArtifact(artifact);
            var artifact_ptr_owned = true;
            errdefer if (artifact_ptr_owned) destroyCheckedArtifact(artifact_ptr, false);
            result.semantic.checked_artifact = null;
            try mod.replaceCheckedArtifact(artifact_ptr, &self.retired_checked_artifacts, self.gpa);
            artifact_ptr_owned = false;
            try self.registerCheckedArtifact(pkg, mod);
            if (mod.checkedArtifact()) |cached_artifact| {
                self.storeCheckedModuleInCache(cached_artifact);
            }
        } else if (mod.semantic) |*semantic| {
            self.unregisterCheckedArtifact(mod);
            if (semantic.checked_artifact) |existing| {
                try self.retired_checked_artifacts.append(self.gpa, .{
                    .artifact = existing,
                    .retain_module_env = @intFromPtr(existing.moduleEnv()) == @intFromPtr(semantic.module_env),
                });
            }
            semantic.checked_artifact = null;
        }
        result.semantic.checked_artifact = null;

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        if (comptime trace_build) {
            std.debug.print("[COORD] TYPE_CHECKED: mod.reports AFTER append: len={} cap={}\n", .{ mod.reports.items.len, mod.reports.capacity });
        }

        // Update timing
        self.total_typecheck_ns += result.type_check_ns;
        self.total_typecheck_diag_ns += result.check_diagnostics_ns;
        mod.compile_time_ns += result.type_check_ns + result.check_diagnostics_ns;

        // Record per-module compile time for min/max/avg stats
        const module_time = mod.compile_time_ns;
        if (module_time < self.module_time_min_ns) self.module_time_min_ns = module_time;
        if (module_time > self.module_time_max_ns) self.module_time_max_ns = module_time;
        self.module_time_sum_ns += module_time;

        // Mark as done
        mod.phase = .Done;
        mod.visit_color = .black;

        // Update compile stats
        self.cache_misses += 1;
        self.modules_compiled += 1;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake local dependents (same package)
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        // Wake cross-package dependents (other packages that import this module)
        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Handle a parse failure
    fn handleParseFailed(self: *Coordinator, result: *messages.ParseFailure) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] PARSE FAILED: pkg={s} module={s} reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for parse_failed result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for parse_failed result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        // Store partial env if available
        if (result.partial_env) |env| {
            mod.replaceModuleEnv(env);
        }

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        // Mark as done (with errors)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        // Wake local dependents (they'll see this module as done with errors)
        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        // Wake cross-package dependents
        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Handle a non-parsing compilation failure.
    fn handleCompileFailed(self: *Coordinator, result: *CompileFailure) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] COMPILE FAILED: pkg={s} module={s} reports={}\n", .{ result.package_name, result.module_name, result.reports.items.len });
        }
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for compile_failed result (module={s}, id={})\n", .{
                result.package_name, result.module_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for compile_failed result (module={s})\n", .{
                result.module_id, result.package_name, result.module_name,
            });
            unreachable;
        };

        if (result.partial_env) |env| {
            mod.replaceModuleEnv(env);
        }
        mod.cached_ast = null;

        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        result.reports.clearRetainingCapacity();

        mod.phase = .Done;

        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;

        for (mod.dependents.items) |dep_id| {
            try self.tryUnblock(pkg, dep_id);
        }

        try self.wakeCrossPackageDependents(result.package_name, result.module_id);
    }

    /// Handle cycle detection
    fn handleCycleDetected(self: *Coordinator, result: *messages.CycleDetected) Allocator.Error!void {
        const pkg = self.packages.get(result.package_name) orelse {
            self.bugReport("BUG: package '{s}' not found for cycle_detected result (id={})\n", .{
                result.package_name, result.module_id,
            });
            unreachable;
        };
        const mod = pkg.getModule(result.module_id) orelse {
            self.bugReport("BUG: module id={} not found in package '{s}' for cycle_detected result\n", .{
                result.module_id, result.package_name,
            });
            unreachable;
        };

        // Take ownership of module env
        mod.replaceModuleEnv(result.module_env);

        // Append reports - we take ownership, so clear result.reports after copying
        for (result.reports.items) |rep| {
            try mod.reports.append(self.gpa, rep);
        }
        // Clear reports to transfer ownership - prevents double-free in WorkerResult.deinit
        result.reports.clearRetainingCapacity();

        // Mark as done (with cycle error)
        mod.phase = .Done;

        // Decrement counters
        if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
        if (self.total_remaining > 0) self.total_remaining -= 1;
    }

    /// Handle cycle detection inline during canonicalization result processing
    fn handleCycleInline(self: *Coordinator, pkg: *PackageState, module_id: ModuleId, child_id: ModuleId) Allocator.Error!void {
        const mod = pkg.getModule(module_id).?;
        const child = pkg.getModule(child_id).?;

        // Create cycle error report
        var rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
        const msg = try rep.addOwnedString("This module participates in an import cycle. Cycles between modules are not allowed.");
        try rep.addErrorMessage(msg);
        try mod.reports.append(self.gpa, rep);

        // Mark both as done
        if (mod.phase != .Done) {
            mod.phase = .Done;
            if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
            if (self.total_remaining > 0) self.total_remaining -= 1;
        }

        if (child.phase != .Done) {
            child.phase = .Done;
            if (pkg.remaining_modules > 0) pkg.remaining_modules -= 1;
            if (self.total_remaining > 0) self.total_remaining -= 1;

            // Add report to child too
            var child_rep = Report.init(self.gpa, "Import cycle detected", .runtime_error);
            const child_msg = try child_rep.addOwnedString("This module participates in an import cycle.");
            try child_rep.addErrorMessage(child_msg);
            try child.reports.append(self.gpa, child_rep);
        }
    }

    fn buildCanonicalizeImports(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
        allocator: Allocator,
    ) Allocator.Error![]const CanonicalizeImport {
        var imports = std.ArrayList(CanonicalizeImport).empty;
        errdefer imports.deinit(allocator);

        for (mod.imports.items) |imp_id| {
            const imp = pkg.getModule(imp_id).?;
            // Skip imports whose module env is missing (e.g. the source file
            // wasn't found during discovery). Canonicalize will emit a
            // `module_not_found` diagnostic for the unresolved name — see
            // src/canonicalize/can.zig where it checks `explicit_module_envs`.
            // Mirrors the `orelse continue` handling for external_imports below.
            const env = imp.moduleEnv() orelse continue;
            try imports.append(allocator, .{
                .import_name = imp.name,
                .module_env = env,
            });
        }
        for (mod.external_imports.items) |ext_name| {
            const ext_env = self.getExternalEnv(pkg.name, ext_name) orelse continue;
            try imports.append(allocator, .{
                .import_name = ext_name,
                .module_env = ext_env,
            });
        }

        return try imports.toOwnedSlice(allocator);
    }

    fn buildTypecheckImportedEnvs(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
        allocator: Allocator,
    ) Allocator.Error![]const *ModuleEnv {
        const expected_capacity = 1 + mod.imports.items.len + mod.external_imports.items.len;
        var imported_envs = try std.ArrayList(*ModuleEnv).initCapacity(allocator, expected_capacity);
        errdefer imported_envs.deinit(allocator);

        try imported_envs.append(allocator, self.builtin_modules.builtin_module.env);
        mod.moduleEnv().?.imports.clearResolvedModules();

        const direct_imports = mod.moduleEnv().?.imports.imports.items.items;
        for (direct_imports, 0..) |str_idx, i| {
            const import_idx: can.CIR.Import.Idx = @enumFromInt(i);
            const import_name = mod.moduleEnv().?.getString(str_idx);

            if (can.CIR.Import.isCompilerBuiltinImportName(import_name)) {
                mod.moduleEnv().?.imports.setResolvedModule(import_idx, 0);
                continue;
            }

            if (pkg.module_names.get(import_name)) |imp_id| {
                const imp = pkg.getModule(imp_id).?;
                if (imp.moduleEnv()) |env| {
                    const resolved_module_idx: u32 = @intCast(imported_envs.items.len);
                    try imported_envs.append(allocator, env);
                    mod.moduleEnv().?.imports.setResolvedModule(import_idx, resolved_module_idx);
                }
                continue;
            }

            if (self.getExternalEnv(pkg.name, import_name)) |ext_env| {
                const resolved_module_idx: u32 = @intCast(imported_envs.items.len);
                try imported_envs.append(allocator, ext_env);
                mod.moduleEnv().?.imports.setResolvedModule(import_idx, resolved_module_idx);
                continue;
            }
        }

        mod.moduleEnv().?.imports.markUnresolvedImportsFailedBeforeChecking();

        if (builtin.mode == .Debug) {
            for (mod.imports.items) |imp_id| {
                const imp = pkg.getModule(imp_id).?;
                std.debug.assert(imp.phase == .Done);
            }
        }

        return try imported_envs.toOwnedSlice(allocator);
    }

    fn buildTypecheckImportedArtifacts(
        self: *Coordinator,
        pkg: *PackageState,
        mod: *ModuleState,
        allocator: Allocator,
    ) Allocator.Error![]const check.CheckedArtifact.PublishImportArtifact {
        var imports = std.ArrayList(check.CheckedArtifact.PublishImportArtifact).empty;
        errdefer imports.deinit(allocator);

        const module_env = mod.moduleEnv().?;
        const direct_imports = module_env.imports.imports.items.items;
        for (direct_imports, 0..) |str_idx, i| {
            const import_idx: can.CIR.Import.Idx = @enumFromInt(i);
            const import_name = module_env.getString(str_idx);
            const resolved_module_idx = module_env.imports.getResolvedModule(import_idx) orelse continue;

            if (can.CIR.Import.isCompilerBuiltinImportName(import_name)) {
                try imports.append(allocator, .{
                    .module_idx = resolved_module_idx,
                    .key = self.builtin_modules.checked_artifact.key,
                    .view = check.CheckedArtifact.importedView(&self.builtin_modules.checked_artifact),
                });
                continue;
            }

            if (pkg.module_names.get(import_name)) |imp_id| {
                const imp = pkg.getModule(imp_id).?;
                if (imp.checkedArtifact()) |artifact| {
                    try imports.append(allocator, .{
                        .module_idx = resolved_module_idx,
                        .key = artifact.key,
                        .view = check.CheckedArtifact.importedView(artifact),
                    });
                }
                continue;
            }

            if (self.getExternalArtifact(pkg.name, import_name)) |artifact| {
                try imports.append(allocator, .{
                    .module_idx = resolved_module_idx,
                    .key = artifact.key,
                    .view = check.CheckedArtifact.importedView(artifact),
                });
            }
        }

        return try imports.toOwnedSlice(allocator);
    }

    /// Try to unblock a module waiting on imports
    fn tryUnblock(self: *Coordinator, pkg: *PackageState, module_id: ModuleId) Allocator.Error!void {
        const mod = pkg.getModule(module_id) orelse return;
        if (mod.phase != .WaitingOnImports) return;

        // Check local imports
        for (mod.imports.items) |imp_id| {
            const imp = pkg.getModule(imp_id) orelse continue;
            if (imp.phase != .Done) {
                if (comptime trace_build) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on local import {}\n", .{ pkg.name, mod.name, imp_id });
                }
                return; // Not ready yet
            }
        }

        // Check external imports
        for (mod.external_imports.items) |ext_name| {
            if (!self.isExternalReady(pkg.name, ext_name)) {
                if (comptime trace_build) {
                    std.debug.print("[COORD] UNBLOCK WAIT: pkg={s} module={s} waiting on external {s}\n", .{ pkg.name, mod.name, ext_name });
                }
                return;
            }
        }

        if (comptime trace_build) {
            std.debug.print("[COORD] UNBLOCK: pkg={s} module={s} -> Canonicalize\n", .{ pkg.name, mod.name });
        }

        mod.phase = .Canonicalize;
        mod.visit_color = .black;
        const task_payload_alloc = self.getWorkerAllocator();
        const imported_modules = try self.buildCanonicalizeImports(pkg, mod, task_payload_alloc);
        errdefer task_payload_alloc.free(imported_modules);
        try self.enqueueTask(.{
            .canonicalize = .{
                .package_name = pkg.name,
                .module_id = module_id,
                .module_name = mod.name,
                .path = mod.path,
                .source_dir = mod.canonicalSourceDir(),
                .module_env = mod.moduleEnv().?,
                .cached_ast = mod.cached_ast orelse
                    std.debug.panic("compile.coordinator.tryUnblock missing cached AST for {s}", .{mod.name}),
                .depth = mod.depth,
                .imported_modules = imported_modules,
            },
        });
    }

    /// Schedule an external import in its owning package
    /// Also registers the source module as a cross-package dependent of the target
    pub fn scheduleExternalImport(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] SCHEDULE EXT IMPORT: from {s} importing {s}\n", .{ source_pkg, import_name });
        }

        // Parse "pf.Stdout" -> { .qualifier = "pf", .module = "Stdout" }
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return;

        // Resolve shorthand to target package
        const source = self.packages.get(source_pkg) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: source pkg {s} not found\n", .{source_pkg});
            }
            return;
        };
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: shorthand {s} not found in {s}\n", .{ qualified.qualifier, source_pkg });
            }
            return;
        };

        // Get or create module in target package
        const target_pkg = self.packages.get(target_pkg_name) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] SCHEDULE EXT IMPORT: target pkg {s} not found\n", .{target_pkg_name});
            }
            return;
        };
        const path = try self.resolveModulePath(target_pkg.root_dir, qualified.module);
        defer self.gpa.free(path);

        const module_id = try target_pkg.ensureModule(self.gpa, qualified.module, path);
        const mod = target_pkg.getModule(module_id).?;

        if (comptime trace_build) {
            std.debug.print("[COORD] SCHEDULE EXT IMPORT: resolved to {s}:{} phase={}\n", .{ target_pkg_name, module_id, mod.phase });
        }

        // Queue parse if new
        if (mod.phase == .Parse) {
            target_pkg.remaining_modules += 1;
            self.total_remaining += 1;
            try self.enqueueParseTask(target_pkg_name, module_id);
        }
    }

    /// Register a cross-package dependent: when target module completes, wake source module
    fn registerCrossPackageDependent(
        self: *Coordinator,
        target_pkg: []const u8,
        target_module_id: ModuleId,
        source_pkg: []const u8,
        source_module_id: ModuleId,
    ) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[COORD] REGISTER CROSS-PKG DEP: {s}:{} depends on {s}:{}\n", .{ source_pkg, source_module_id, target_pkg, target_module_id });
        }

        // Build key: "pkg_name:module_id"
        const key = try std.fmt.allocPrint(self.gpa, "{s}:{d}", .{ target_pkg, target_module_id });
        errdefer self.gpa.free(key);

        const gop = try self.cross_package_dependents.getOrPut(key);
        if (!gop.found_existing) {
            gop.value_ptr.* = std.ArrayList(ModuleRef).empty;
        } else {
            // Key already existed, free our duplicate
            self.gpa.free(key);
        }

        // Add the source module as a dependent
        try gop.value_ptr.append(self.gpa, .{
            .pkg_name = source_pkg,
            .module_id = source_module_id,
        });
    }

    /// Wake all cross-package dependents of a completed module
    fn wakeCrossPackageDependents(self: *Coordinator, pkg_name: []const u8, module_id: ModuleId) Allocator.Error!void {
        // Build key
        var key_buf: [256]u8 = undefined;
        const key = std.fmt.bufPrint(&key_buf, "{s}:{d}", .{ pkg_name, module_id }) catch return;

        const dependents = self.cross_package_dependents.get(key) orelse {
            if (comptime trace_build) {
                std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} has no dependents\n", .{ pkg_name, module_id });
            }
            return;
        };

        if (comptime trace_build) {
            std.debug.print("[COORD] WAKE CROSS-PKG: {s}:{} waking {} dependents\n", .{ pkg_name, module_id, dependents.items.len });
        }

        for (dependents.items) |dep_ref| {
            const dep_pkg = self.packages.get(dep_ref.pkg_name) orelse continue;
            try self.tryUnblock(dep_pkg, dep_ref.module_id);
        }
    }

    /// Check if an external import is ready.
    /// Returns true if:
    /// - The target module is done (has completed compilation), OR
    /// - The import cannot be resolved (invalid shorthand, missing package, etc.)
    ///   In the latter case, we return true to allow the module to proceed to
    ///   type-checking, where the unresolved import will produce a proper error.
    ///   Returning false would cause the coordinator to wait forever for something
    ///   that will never be ready.
    pub fn isExternalReady(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) bool {
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return true;

        const source = self.packages.get(source_pkg) orelse return true;
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse return true;
        const target_pkg = self.packages.get(target_pkg_name) orelse return true;

        // Use isDone instead of getEnvIfDone - a module that failed to parse
        // is still "done" and shouldn't block dependents (even if it has no env)
        return target_pkg.isDone(qualified.module);
    }

    /// Get the ModuleEnv for an external import
    pub fn getExternalEnv(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) ?*ModuleEnv {
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return null;

        const source = self.packages.get(source_pkg) orelse return null;
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse return null;
        const target_pkg = self.packages.get(target_pkg_name) orelse return null;

        return if (target_pkg.getSemanticDataIfDone(qualified.module)) |semantic|
            semantic.env
        else
            null;
    }

    pub fn getExternalArtifact(self: *Coordinator, source_pkg: []const u8, import_name: []const u8) ?*const check.CheckedArtifact.CheckedModuleArtifact {
        const qualified = base.module_path.parseQualifiedImport(import_name) orelse return null;

        const source = self.packages.get(source_pkg) orelse return null;
        const target_pkg_name = source.shorthands.get(qualified.qualifier) orelse return null;
        const target_pkg = self.packages.get(target_pkg_name) orelse return null;

        return if (target_pkg.getSemanticDataIfDone(qualified.module)) |semantic|
            semantic.checked_artifact
        else
            null;
    }

    /// Get build statistics for this compilation
    pub fn getBuildStats(self: *Coordinator) compile_build.BuildEnv.BuildStats {
        return .{
            .modules_total = self.cache_hits + self.modules_compiled,
            .cache_hits = self.cache_hits,
            .cache_misses = self.cache_misses,
            .modules_compiled = self.modules_compiled,
            .module_time_min_ns = self.module_time_min_ns,
            .module_time_max_ns = self.module_time_max_ns,
            .module_time_sum_ns = self.module_time_sum_ns,
        };
    }

    /// Resolve a module name to a path
    fn resolveModulePath(self: *Coordinator, root_dir: []const u8, mod_name: []const u8) Allocator.Error![]const u8 {
        return self.resolveModulePathWithAllocator(root_dir, mod_name, self.gpa);
    }

    /// Resolve a module name to a path using a specific allocator
    fn resolveModulePathWithAllocator(_: *Coordinator, root_dir: []const u8, mod_name: []const u8, alloc: Allocator) Allocator.Error![]const u8 {
        var buffer = std.ArrayList(u8).empty;
        defer buffer.deinit(alloc);

        var it = std.mem.splitScalar(u8, mod_name, '.');
        var first = true;
        while (it.next()) |part| {
            if (!first) try buffer.appendSlice(alloc, std.fs.path.sep_str) else first = false;
            try buffer.appendSlice(alloc, part);
        }
        try buffer.appendSlice(alloc, ".roc");

        const rel = try buffer.toOwnedSlice(alloc);
        defer alloc.free(rel);

        return try std.fs.path.join(alloc, &.{ root_dir, rel });
    }

    /// Execute a parse task (pure function)
    fn executeParse(self: *Coordinator, task: ParseTask, allocators: WorkerTaskAllocators) WorkerResult {
        return self.executeParseFallible(task, allocators) catch |err| switch (err) {
            error.OutOfMemory => WorkerResult{ .worker_oom = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
            } },
            else => |e| blk: {
                const title = switch (e) {
                    error.FileNotFound => "FILE NOT FOUND",
                    else => "PARSING FAILED",
                };
                break :blk WorkerResult{ .parse_failed = .{
                    .package_name = task.package_name,
                    .module_id = task.module_id,
                    .module_name = task.module_name,
                    .path = task.path,
                    .reports = self.workerFailureReports(allocators.result, title, task.path, e),
                    .partial_env = null,
                } };
            },
        };
    }

    fn executeParseFallible(self: *Coordinator, task: ParseTask, task_allocs: WorkerTaskAllocators) (Allocator.Error || error{ AccessDenied, FileNotFound, IoError, StreamTooLong, TooNested })!WorkerResult {
        var parse_timer = startStageTimer(self.roc_ctx.std_io);

        const src = try self.readModuleSource(task.path, task_allocs.module);

        // Checked-module disk cache lookup happens after canonicalization, when
        // the coordinator has the exact module identity and import keys.

        // Create ModuleEnv using the long-lived module allocator.
        const module_alloc = task_allocs.module;
        const env = module_alloc.create(ModuleEnv) catch |err| {
            module_alloc.free(src);
            return err;
        };

        var env_initialized = false;
        errdefer {
            if (env_initialized) {
                destroyModuleEnvAndSource(env);
            } else {
                module_alloc.destroy(env);
                module_alloc.free(src);
            }
        }

        env.* = try ModuleEnv.init(module_alloc, src);
        env_initialized = true;
        try env.initCIRFields(task.module_name);
        env.module_role = task.module_role;

        // Set qualified_module_ident to a package-qualified identifier (e.g., "app.main", "pf.Stdout")
        // to ensure module identity is unique across packages. Without this, two modules with
        // the same filename in different packages (e.g., app's main.roc and platform's main.roc)
        // get the same identity, causing nominal type origin_module collisions.
        // display_module_name_idx stays as the bare name (for type module validation, error messages, etc.)
        {
            const qname = try std.fmt.allocPrint(task_allocs.scratch, "{s}.{s}", .{ task.package_name, task.module_name });
            env.qualified_module_ident = try env.insertIdent(base.Ident.for_text(qname));
        }
        try env.common.calcLineStarts(module_alloc);

        // The AST and result payloads outlive this task, so they use the result
        // allocator. Only import-discovery intermediates use task scratch below.
        const worker_alloc = task_allocs.result;
        var reports = try std.ArrayList(Report).initCapacity(worker_alloc, 8);
        errdefer deinitReports(&reports, worker_alloc);

        const parse_ast = try parse.parse(worker_alloc, &env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            const rep = try parse_ast.tokenizeDiagnosticToReport(diagnostic, worker_alloc, task.path);
            try appendReportOwned(worker_alloc, &reports, rep);
        }
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            const rep = try parse_ast.parseDiagnosticToReport(&env.common, diagnostic, worker_alloc, task.path);
            try appendReportOwned(worker_alloc, &reports, rep);
        }

        var discovered_local_imports = std.ArrayList(DiscoveredLocalImport).empty;
        errdefer {
            for (discovered_local_imports.items) |imp| {
                worker_alloc.free(imp.module_name);
                worker_alloc.free(imp.path);
            }
            discovered_local_imports.deinit(worker_alloc);
        }
        const local_import_names = try module_discovery.extractImportsFromDeclIndex(parse_ast, task_allocs.scratch);
        const module_dir = std.fs.path.dirname(task.path) orelse "";
        for (local_import_names) |module_name| {
            const path = try self.resolveModulePathWithAllocator(module_dir, module_name, worker_alloc);
            errdefer worker_alloc.free(path);
            const owned_name = try worker_alloc.dupe(u8, module_name);
            errdefer worker_alloc.free(owned_name);
            try discovered_local_imports.append(worker_alloc, .{
                .module_name = owned_name,
                .path = path,
            });
        }

        var discovered_external_imports = std.ArrayList(DiscoveredExternalImport).empty;
        errdefer {
            for (discovered_external_imports.items) |imp| worker_alloc.free(imp.import_name);
            discovered_external_imports.deinit(worker_alloc);
        }
        const qualified_import_names = try module_discovery.extractQualifiedImportsFromDeclIndex(parse_ast, task_allocs.scratch);
        for (qualified_import_names) |import_name| {
            const owned_name = try worker_alloc.dupe(u8, import_name);
            errdefer worker_alloc.free(owned_name);
            try discovered_external_imports.append(worker_alloc, .{
                .import_name = owned_name,
            });
        }

        return .{
            .parsed = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .cached_ast = parse_ast,
                .discovered_local_imports = discovered_local_imports,
                .discovered_external_imports = discovered_external_imports,
                .reports = reports,
                .parse_ns = readStageTimer(self.roc_ctx.std_io, &parse_timer),
            },
        };
    }

    /// Execute a canonicalize task (pure function)
    fn executeCanonicalize(self: *Coordinator, task: CanonicalizeTask, allocators: WorkerTaskAllocators) WorkerResult {
        return self.executeCanonicalizeFallible(task, allocators) catch |err| switch (err) {
            error.OutOfMemory => WorkerResult{ .worker_oom = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
            } },
        };
    }

    fn executeCanonicalizeFallible(self: *Coordinator, task: CanonicalizeTask, task_allocs: WorkerTaskAllocators) Allocator.Error!WorkerResult {
        var canonicalize_timer = startStageTimer(self.roc_ctx.std_io);

        const env = task.module_env;
        const ast = task.cached_ast;
        defer task_allocs.result.free(task.imported_modules);
        defer ast.deinit();

        // Build KnownModule entries for qualified imports (e.g. platform-exposed
        // `pf.Stdout`) so canonicalization has explicit module names.
        const qualified_imports = try module_discovery.extractQualifiedImportsFromDeclIndex(ast, task_allocs.scratch);
        defer {
            for (qualified_imports) |qi| task_allocs.scratch.free(qi);
            task_allocs.scratch.free(qualified_imports);
        }
        var known_modules = std.ArrayList(compile_package.PackageEnv.KnownModule).empty;
        defer known_modules.deinit(task_allocs.scratch);
        for (qualified_imports) |qi| {
            try known_modules.append(task_allocs.scratch, .{
                .qualified_name = qi,
                .import_name = qi,
            });
        }

        try compile_package.PackageEnv.canonicalizeModuleWithSiblings(
            self.roc_ctx,
            env,
            ast,
            self.builtin_modules.builtin_module.env,
            self.builtin_modules.builtin_indices,
            task.source_dir,
            task.package_name,
            null, // Coordinator handles import resolution separately
            known_modules.items,
            task.imported_modules,
        );

        const canonicalize_ns = readStageTimer(self.roc_ctx.std_io, &canonicalize_timer);

        var diagnostics_timer = startStageTimer(self.roc_ctx.std_io);
        const worker_alloc = task_allocs.result;
        var reports = try std.ArrayList(Report).initCapacity(worker_alloc, 8);
        errdefer deinitReports(&reports, worker_alloc);

        const diags = try env.getDiagnostics();
        defer env.gpa.free(diags);
        for (diags) |d| {
            const rep = try env.diagnosticToReport(d, worker_alloc, task.path);
            try appendReportOwned(worker_alloc, &reports, rep);
        }
        const diagnostics_ns = readStageTimer(self.roc_ctx.std_io, &diagnostics_timer);

        return .{
            .canonicalized = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .module_env = env,
                .discovered_local_imports = std.ArrayList(DiscoveredLocalImport).empty,
                .discovered_external_imports = std.ArrayList(DiscoveredExternalImport).empty,
                .reports = reports,
                .canonicalize_ns = canonicalize_ns,
                .canonicalize_diagnostics_ns = diagnostics_ns,
            },
        };
    }

    /// Execute a type-check task (pure function)
    fn executeTypeCheck(self: *Coordinator, task: TypeCheckTask, allocators: WorkerTaskAllocators) WorkerResult {
        return self.executeTypeCheckFallible(task, allocators) catch |err| switch (err) {
            error.OutOfMemory => WorkerResult{ .worker_oom = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
            } },
            else => |e| WorkerResult{ .compile_failed = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .reports = self.workerFailureReports(allocators.result, "TYPE CHECKING FAILED", task.path, e),
                .partial_env = task.module_env,
            } },
        };
    }

    fn executeTypeCheckFallible(self: *Coordinator, task: TypeCheckTask, task_allocs: WorkerTaskAllocators) anyerror!WorkerResult {
        var check_timer = startStageTimer(self.roc_ctx.std_io);

        const env = task.module_env;
        defer task_allocs.result.free(task.imported_envs);
        defer task_allocs.result.free(task.imported_artifacts);
        defer task_allocs.result.free(task.available_artifacts);

        const result_alloc = task_allocs.result;
        // The checked artifact can retain memory owned by the checker output.
        // Keep those allocations with the published result instead of the
        // task-local scratch arena, which is reset after the worker task.
        const check_alloc = result_alloc;
        var typecheck_output = try compile_package.PackageEnv.typeCheckModule(
            check_alloc,
            result_alloc,
            env,
            self.builtin_modules.builtin_module.env,
            task.imported_envs,
            task.imported_artifacts,
            task.available_artifacts,
        );
        defer typecheck_output.deinit();

        const type_check_ns = readStageTimer(self.roc_ctx.std_io, &check_timer);

        var diagnostics_timer = startStageTimer(self.roc_ctx.std_io);
        const worker_alloc = result_alloc;
        var reports = try std.ArrayList(Report).initCapacity(worker_alloc, 8);
        errdefer deinitReports(&reports, worker_alloc);

        var rb = try check.ReportBuilder.init(
            worker_alloc,
            env,
            env,
            &typecheck_output.checker.snapshots,
            &typecheck_output.checker.problems,
            task.path,
            task.imported_envs,
            &typecheck_output.checker.import_mapping,
            &typecheck_output.checker.regions,
        );
        defer rb.deinit();

        for (typecheck_output.checker.problems.problems.items) |prob| {
            const rep = try rb.build(prob);
            try appendReportOwned(worker_alloc, &reports, rep);
        }

        const diagnostics_ns = readStageTimer(self.roc_ctx.std_io, &diagnostics_timer);

        var checked_artifact: ?check.CheckedArtifact.CheckedModuleArtifact =
            if (typecheck_output.checked_artifact != null) typecheck_output.takeCheckedArtifact() else null;
        errdefer if (checked_artifact) |*artifact| artifact.deinit(artifact.canonical_names.allocator);

        const semantic = try createOwnedSemanticResult(result_alloc, env, checked_artifact);
        checked_artifact = null;

        return .{
            .type_checked = .{
                .package_name = task.package_name,
                .module_id = task.module_id,
                .module_name = task.module_name,
                .path = task.path,
                .semantic = semantic,
                .reports = reports,
                .type_check_ns = type_check_ns,
                .check_diagnostics_ns = diagnostics_ns,
            },
        };
    }

    /// Read module source using the Io abstraction.
    fn readModuleSource(self: *Coordinator, path: []const u8, module_alloc: Allocator) (Allocator.Error || error{ AccessDenied, FileNotFound, IoError, StreamTooLong })![]u8 {
        const data = try self.roc_ctx.readFile(path, module_alloc);
        errdefer module_alloc.free(data);

        // Normalize line endings
        return base.source_utils.normalizeLineEndingsRealloc(module_alloc, data);
    }

    /// Worker thread main function
    fn workerThread(self: *Coordinator) void {
        _ = base.stack_overflow.installForCurrentThread();

        // Each worker has its own allocators for thread safety.
        // - gpa: smp_allocator for long-lived data (ModuleEnv, source)
        // - arena: for temporary allocations, reset between tasks
        const backing = if (threads_available) thread_safe_allocator else self.gpa;
        var worker_allocs = WorkerAllocators.init(backing);
        defer worker_allocs.deinit();

        while (true) {
            // Check shutdown flag before blocking on the next task.
            // This ensures workers exit promptly instead of draining
            // remaining buffered tasks after shutdown() is called.
            if (self.shutting_down.load(.acquire)) break;

            // Block until a task is available. Returns null when the channel
            // is closed and drained.
            const t = self.task_channel.recv() orelse break;

            // Execute task. On OOM we cannot return an error from this `void`
            // thread entry point, so record it for the coordinator to observe
            // and stop pulling work — the coordinator aborts the build.
            const result = self.executeTaskInline(t, worker_allocs.taskAllocators()) catch |err| switch (err) {
                error.OutOfMemory => {
                    self.worker_oom.store(true, .release);
                    break;
                },
            };

            // Reset arena between tasks to reclaim temporary allocations
            worker_allocs.resetArena();

            // Send result
            self.result_channel.send(result) catch break;
        }
    }
};

const CheckedModuleCacheRunStats = struct {
    build: compile_build.BuildEnv.BuildStats,
    cache: CacheStats,
};

fn compileAppWithCheckedModuleCache(
    allocator: Allocator,
    cache_dir: []const u8,
    app_path: []const u8,
) anyerror!CheckedModuleCacheRunStats {
    const roc_ctx = CoreCtx.os(allocator, allocator, std.testing.io);
    var cache_manager = CacheManager.init(allocator, .{
        .enabled = true,
        .cache_dir = cache_dir,
    }, roc_ctx);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        &cache_manager,
        roc_ctx,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    return .{
        .build = coord.getBuildStats(),
        .cache = cache_manager.stats,
    };
}

fn overwriteFilesUnderDir(allocator: Allocator, absolute_dir: []const u8, contents: []const u8) anyerror!usize {
    const io = std.testing.io;
    var dir = try std.Io.Dir.openDirAbsolute(io, absolute_dir, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    var overwritten: usize = 0;
    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        try dir.writeFile(io, .{ .sub_path = entry.path, .data = contents });
        overwritten += 1;
    }
    return overwritten;
}

test "Coordinator checked cache key requires checked direct imports" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "import Host\n");
    defer env.deinit();
    try env.initCIRFields("W4");

    const import_idx = try env.imports.getOrPut(allocator, &env.common.strings, "Host");
    env.imports.setResolvedModule(import_idx, 1);

    try std.testing.expect(!Coordinator.resolvedDirectImportsHaveCheckedOutput(&env, &.{}));

    const checked_imports = [_]check.CheckedArtifact.PublishImportArtifact{.{
        .module_idx = 1,
        .key = .{},
        .view = undefined,
    }};
    try std.testing.expect(Coordinator.resolvedDirectImportsHaveCheckedOutput(&env, &checked_imports));

    env.imports.clearResolvedModules();
    try std.testing.expect(Coordinator.resolvedDirectImportsHaveCheckedOutput(&env, &.{}));
}

test "Coordinator checked module cache hits on second compile" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const cache_dir = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(cache_dir);

    const first = try compileAppWithCheckedModuleCache(allocator, cache_dir, "test/str/app_message.roc");
    try std.testing.expectEqual(@as(u32, 0), first.build.cache_hits);
    try std.testing.expect(first.build.modules_compiled > 0);
    try std.testing.expect(first.cache.stores > 0);
    try std.testing.expectEqual(@as(u64, 0), first.cache.store_failures);

    const second = try compileAppWithCheckedModuleCache(allocator, cache_dir, "test/str/app_message.roc");
    try std.testing.expect(second.build.cache_hits > 0);
    try std.testing.expect(second.build.modules_compiled < first.build.modules_compiled);
    try std.testing.expect(second.cache.hits > 0);
}

test "Coordinator corrupt checked module cache entries compile from source" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const cache_dir = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(cache_dir);

    const first = try compileAppWithCheckedModuleCache(allocator, cache_dir, "test/str/app_message.roc");
    try std.testing.expect(first.cache.stores > 0);

    const config = CacheConfig{ .cache_dir = cache_dir };
    const checked_module_cache_dir = try config.getCheckedArtifactCacheDir(allocator);
    defer allocator.free(checked_module_cache_dir);
    const overwritten = try overwriteFilesUnderDir(allocator, checked_module_cache_dir, "not a checked module cache entry");
    try std.testing.expect(overwritten > 0);

    const second = try compileAppWithCheckedModuleCache(allocator, cache_dir, "test/str/app_message.roc");
    try std.testing.expectEqual(@as(u32, 0), second.build.cache_hits);
    try std.testing.expect(second.build.modules_compiled > 0);
    try std.testing.expect(second.cache.invalidations > 0);
}

test "Coordinator basic initialization" {
    const allocator = std.testing.allocator;

    // Create minimal builtin modules mock
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined, // builtin_modules - not used in this test
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    try std.testing.expect(coord.total_remaining == 0);
    try std.testing.expect(coord.task_channel.isEmpty());
    try std.testing.expect(coord.isComplete());
}

test "Coordinator package creation" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create a package
    const pkg = try coord.ensurePackage("app", "/test/app");
    try std.testing.expectEqualStrings("app", pkg.name);
    try std.testing.expectEqualStrings("/test/app", pkg.root_dir);

    // Verify package is stored
    const retrieved = coord.packages.get("app");
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqualStrings("app", retrieved.?.name);
}

test "Coordinator module creation" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    try std.testing.expectEqual(@as(ModuleId, 0), module_id);

    const mod = pkg.getModule(module_id);
    try std.testing.expect(mod != null);
    try std.testing.expectEqualStrings("Main", mod.?.name);
    try std.testing.expectEqualStrings("/test/app/Main.roc", mod.?.path);
    try std.testing.expect(mod.?.phase == .Parse);
}

test "Coordinator task queue" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    _ = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Enqueue a task directly
    try coord.enqueueTask(.{
        .parse = .{
            .package_name = "app",
            .module_id = 0,
            .module_name = "Main",
            .path = "/test/app/Main.roc",
            .depth = 0,
            .module_role = .user,
        },
    });

    try std.testing.expectEqual(@as(usize, 1), coord.task_channel.len());

    // Receive the task
    const task = coord.task_channel.tryRecv();
    try std.testing.expect(task != null);
    try std.testing.expect(task.? == .parse);
    try std.testing.expectEqualStrings("app", task.?.parse.package_name);
}

test "Coordinator isComplete logic" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Initially complete (nothing to do)
    try std.testing.expect(coord.isComplete());

    // Add a remaining module
    coord.total_remaining = 1;
    try std.testing.expect(!coord.isComplete());

    // Clear remaining but add task
    coord.total_remaining = 0;
    try coord.enqueueTask(.{
        .parse = .{
            .package_name = "test",
            .module_id = 0,
            .module_name = "Test",
            .path = "/test.roc",
            .depth = 0,
            .module_role = .user,
        },
    });
    try std.testing.expect(!coord.isComplete());

    // Clear task but add inflight
    if (coord.task_channel.tryRecv()) |_| {} else {}
    coord.inflight.store(1, .release);
    try std.testing.expect(!coord.isComplete());

    // All clear - should be complete
    coord.inflight.store(0, .release);
    try std.testing.expect(coord.isComplete());
}

test "Coordinator isComplete with multi_threaded max_threads=0 (inline execution)" {
    // max_threads == 0 with multi_threaded mode should fall back to inline
    // execution (no workers). Inflight must NOT be incremented by enqueueTask
    // in this configuration, otherwise isComplete() would never return true.
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .multi_threaded,
        0, // auto — but <= 1, so no workers spawned
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    try std.testing.expect(coord.isComplete());

    // Enqueue a task — inflight must stay 0 since there are no workers
    try coord.enqueueTask(.{
        .parse = .{
            .package_name = "test",
            .module_id = 0,
            .module_name = "Test",
            .path = "/test.roc",
            .depth = 0,
            .module_role = .user,
        },
    });
    try std.testing.expectEqual(@as(usize, 0), coord.inflight.load(.monotonic));

    // Drain the task — should be complete again
    if (coord.task_channel.tryRecv()) |_| {} else {}
    try std.testing.expect(coord.isComplete());
}

test "Coordinator shutdown does not drain buffered tasks" {
    // When shutdown() is called with tasks still in the channel, workers
    // must exit promptly instead of processing the remaining work.
    if (is_freestanding) return error.SkipZigTest;

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .multi_threaded,
        2,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Buffer several tasks BEFORE starting workers, so they are sitting
    // in the channel when shutdown fires.
    for (0..4) |i| {
        try coord.enqueueTask(.{
            .parse = .{
                .package_name = "test",
                .module_id = @intCast(i),
                .module_name = "Mod",
                .path = "/mod.roc",
                .depth = 0,
                .module_role = .user,
            },
        });
    }

    // Verify tasks are buffered
    try std.testing.expectEqual(@as(usize, 4), coord.task_channel.len());

    // Shut down immediately — no workers were started, but exercise the
    // flag + close path so we can verify the channel is NOT drained.
    coord.shutdown();

    // The shutting_down flag must be set
    try std.testing.expect(coord.shutting_down.load(.acquire));

    // Tasks should still be in the channel (not consumed by workers)
    // because shutdown was called before any worker could run.
    // Some may have been popped by close() waking a blocked recv,
    // but with no workers started, all 4 must remain.
    try std.testing.expectEqual(@as(usize, 4), coord.task_channel.len());
}

test "Coordinator shutdown stops spawned workers promptly" {
    // With real workers running, shutdown must return cleanly and join all
    // spawned workers even when work is still buffered.
    if (is_freestanding) return error.SkipZigTest;

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .multi_threaded,
        2,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Buffer tasks before starting workers
    for (0..8) |i| {
        try coord.enqueueTask(.{
            .parse = .{
                .package_name = "test",
                .module_id = @intCast(i),
                .module_name = "Mod",
                .path = "/mod.roc",
                .depth = 0,
                .module_role = .user,
            },
        });
    }

    const buffered_before = coord.task_channel.len();
    try std.testing.expectEqual(@as(usize, 8), buffered_before);

    // Start workers then immediately shut down
    try coord.start();
    coord.shutdown(); // sets flag, closes channels, joins threads

    // The important property is that shutdown() returns promptly
    // (workers join instead of hanging). Workers may consume an
    // unpredictable number of tasks between start() and shutdown()
    // depending on scheduling, so we don't assert a specific count.
    // The deterministic drain test above covers the flag+close path.
    try std.testing.expect(coord.shutting_down.load(.acquire));
    try std.testing.expectEqual(@as(usize, 0), coord.workers.items.len);
}

test "Channel in coordinator context" {
    // Skip on wasm
    if (is_freestanding) return error.SkipZigTest;

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .multi_threaded,
        2,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Test that the result channel works
    const test_result = WorkerResult{
        .parse_failed = .{
            .package_name = "test",
            .module_id = 0,
            .module_name = "Test",
            .path = "/test.roc",
            .reports = std.ArrayList(reporting.Report).empty,
            .partial_env = null,
        },
    };

    try coord.result_channel.send(test_result);

    const received = coord.result_channel.tryRecv();
    try std.testing.expect(received != null);
    try std.testing.expect(received.? == .parse_failed);
}

test "Coordinator enqueueParseTask flow" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create package
    const pkg = try coord.ensurePackage("app", "/test/app");

    // Create module
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Set up remaining count (simulating buildWithCoordinator)
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Enqueue parse task
    try coord.enqueueParseTask("app", module_id);

    // Verify task was queued
    try std.testing.expectEqual(@as(usize, 1), coord.task_channel.len());

    // Verify it's a parse task for the right module
    const task = coord.task_channel.tryRecv().?;
    try std.testing.expect(task == .parse);
    try std.testing.expectEqualStrings("app", task.parse.package_name);
    try std.testing.expectEqual(@as(ModuleId, 0), task.parse.module_id);
    try std.testing.expectEqualStrings("Main", task.parse.module_name);
}

test "Coordinator single-threaded loop with mock result" {
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Main", "/test/app/Main.roc");

    // Set up remaining count
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Instead of actually parsing, let's manually process a "completed" result
    // This simulates what would happen after a module completes type-checking

    // Mark module as done directly
    const mod = pkg.getModule(module_id).?;
    mod.phase = .Done;
    pkg.remaining_modules = 0;
    coord.total_remaining = 0;

    // Now the coordinator should be complete
    try std.testing.expect(coord.isComplete());
}

test "Coordinator CI failure scenario - app with platform cross-package imports" {
    // This test mirrors the exact module graph from the CI failure:
    //   Package app: 1 module (expect_with_main) with 2 external imports (pf.Stdout, pf.Stderr)
    //   Package pf: 6 modules (main, Stdout, Stderr, Stdin, Builder, Host)
    //     where main imports Stdout, Stderr, Stdin, Builder, Host locally
    //
    // The bug was that result handlers silently dropped results when package/module
    // lookups failed (orelse return), causing modules to get stuck forever.
    // This test verifies the module graph setup and completion logic works
    // for this exact structure.

    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Set up "app" package
    const app_pkg = try coord.ensurePackage("app", "/test/app");

    // Set up shorthand: app's "pf" -> package "pf"
    const sh_key = try allocator.dupe(u8, "pf");
    const sh_val = try allocator.dupe(u8, "pf");
    try app_pkg.shorthands.put(sh_key, sh_val);

    const app_mod_id = try app_pkg.ensureModule(allocator, "expect_with_main", "/test/app/expect_with_main.roc");
    app_pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Set up "pf" package
    const pf_pkg = try coord.ensurePackage("pf", "/test/pf");
    const pf_main_id = try pf_pkg.ensureModule(allocator, "main", "/test/pf/main.roc");
    const pf_stdout_id = try pf_pkg.ensureModule(allocator, "Stdout", "/test/pf/Stdout.roc");
    const pf_stderr_id = try pf_pkg.ensureModule(allocator, "Stderr", "/test/pf/Stderr.roc");
    const pf_stdin_id = try pf_pkg.ensureModule(allocator, "Stdin", "/test/pf/Stdin.roc");
    const pf_builder_id = try pf_pkg.ensureModule(allocator, "Builder", "/test/pf/Builder.roc");
    const pf_host_id = try pf_pkg.ensureModule(allocator, "Host", "/test/pf/Host.roc");
    pf_pkg.remaining_modules = 6;
    coord.total_remaining += 6;

    // Set up local imports for pf.main -> Stdout, Stderr, Stdin, Builder, Host
    const pf_main = pf_pkg.getModule(pf_main_id).?;
    try pf_main.imports.append(allocator, pf_stdout_id);
    try pf_main.imports.append(allocator, pf_stderr_id);
    try pf_main.imports.append(allocator, pf_stdin_id);
    try pf_main.imports.append(allocator, pf_builder_id);
    try pf_main.imports.append(allocator, pf_host_id);
    pf_main.phase = .WaitingOnImports;

    // Set up external imports for app.expect_with_main -> pf.Stdout, pf.Stderr
    const app_mod = app_pkg.getModule(app_mod_id).?;
    try app_mod.external_imports.append(allocator, try allocator.dupe(u8, "pf.Stdout"));
    try app_mod.external_imports.append(allocator, try allocator.dupe(u8, "pf.Stderr"));
    app_mod.phase = .WaitingOnImports;

    // Register pf.main as dependent of its local imports
    const pf_stdout = pf_pkg.getModule(pf_stdout_id).?;
    try pf_stdout.dependents.append(allocator, pf_main_id);
    const pf_stderr = pf_pkg.getModule(pf_stderr_id).?;
    try pf_stderr.dependents.append(allocator, pf_main_id);
    const pf_stdin = pf_pkg.getModule(pf_stdin_id).?;
    try pf_stdin.dependents.append(allocator, pf_main_id);
    const pf_builder = pf_pkg.getModule(pf_builder_id).?;
    try pf_builder.dependents.append(allocator, pf_main_id);
    const pf_host = pf_pkg.getModule(pf_host_id).?;
    try pf_host.dependents.append(allocator, pf_main_id);

    // Verify initial state matches the CI failure scenario
    try std.testing.expectEqual(@as(usize, 7), coord.total_remaining);
    try std.testing.expect(!coord.isComplete());

    // Simulate: complete all pf leaf modules (Stdout, Stderr, Stdin, Builder, Host)
    // Mark them Done and decrement counters (as handleTypeChecked would do)
    const pf_leaf_ids = [_]ModuleId{ pf_stdout_id, pf_stderr_id, pf_stdin_id, pf_builder_id, pf_host_id };
    for (pf_leaf_ids) |leaf_id| {
        const leaf = pf_pkg.getModule(leaf_id).?;
        leaf.phase = .Done;
        pf_pkg.remaining_modules -= 1;
        coord.total_remaining -= 1;
    }

    // pf.main should still be WaitingOnImports (all imports are now Done but
    // nobody called tryUnblock yet - in real code handleTypeChecked wakes dependents)
    try std.testing.expect(pf_pkg.getModule(pf_main_id).?.phase == .WaitingOnImports);

    // Verify external import readiness via the public isExternalReady API
    // pf.Stdout and pf.Stderr should be ready (they're Done)
    try std.testing.expect(coord.isExternalReady("app", "pf.Stdout"));
    try std.testing.expect(coord.isExternalReady("app", "pf.Stderr"));

    // Now simulate completing pf.main
    pf_pkg.getModule(pf_main_id).?.phase = .Done;
    pf_pkg.remaining_modules -= 1;
    coord.total_remaining -= 1;

    // Verify pf is fully complete
    try std.testing.expectEqual(@as(usize, 0), pf_pkg.remaining_modules);

    // Now simulate completing app.expect_with_main
    app_pkg.getModule(app_mod_id).?.phase = .Done;
    app_pkg.remaining_modules -= 1;
    coord.total_remaining -= 1;

    // Verify everything is complete
    try std.testing.expectEqual(@as(usize, 0), coord.total_remaining);
    try std.testing.expectEqual(@as(usize, 0), app_pkg.remaining_modules);
    try std.testing.expectEqual(@as(usize, 0), pf_pkg.remaining_modules);

    // Verify all modules reached Done
    for (app_pkg.modules.items) |m| {
        try std.testing.expect(m.phase == .Done);
    }
    for (pf_pkg.modules.items) |m| {
        try std.testing.expect(m.phase == .Done);
    }

    try std.testing.expect(coord.isComplete());
}

test "Coordinator handleParseFailed advances module to Done" {
    // Verifies that handleParseFailed (which previously had orelse return)
    // correctly transitions a module to Done and decrements counters.
    // If the package/module lookup silently returned, the module would
    // stay in Parsing forever — exactly the bug from CI.
    const allocator = std.testing.allocator;

    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        undefined,
        "test",
        null, // cache_manager
        CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io),
    );
    defer coord.deinit();

    // Create package and module
    const pkg = try coord.ensurePackage("app", "/test/app");
    const module_id = try pkg.ensureModule(allocator, "Builder", "/test/app/Builder.roc");
    pkg.remaining_modules = 1;
    coord.total_remaining = 1;

    // Set module to Parsing (as enqueueParseTask would do)
    const mod = pkg.getModule(module_id).?;
    mod.phase = .Parsing;

    // Feed a parse_failed result through handleResult.
    // This exercises the package/module lookup that previously used orelse return.
    const result: WorkerResult = .{
        .parse_failed = .{
            .package_name = "app",
            .module_id = module_id,
            .module_name = "Builder",
            .path = "/test/app/Builder.roc",
            .reports = std.ArrayList(reporting.Report).empty,
            .partial_env = null,
        },
    };
    try coord.handleResult(result);

    // Verify module advanced to Done (not stuck in Parsing)
    const mod_after = pkg.getModule(module_id).?;
    try std.testing.expect(mod_after.phase == .Done);

    // Verify counters decremented
    try std.testing.expectEqual(@as(usize, 0), pkg.remaining_modules);
    try std.testing.expectEqual(@as(usize, 0), coord.total_remaining);
    try std.testing.expect(coord.isComplete());
}
