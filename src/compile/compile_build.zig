//! The state for an in-progress build. ("Builds" include `roc check` as well as `roc build`.)
//!
//! Modules are built in parallel unless targeting WebAssembly, which doesn't support threads.
//!
//! Errors are reported as soon as they're encountered, with the only exception being that
//! there is some buffering to make their output order determined by the dependency graph
//! rather than by parallelism races. In other words, if you build the same set of source files
//! repeatedly, you should always see the same errors be reported in the same order - but you
//! shouldn't have to wait until the end of the build to start seeing them appear.

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const can = @import("can");
const builtin = @import("builtin");
const build_options = @import("build_options");
const reporting = @import("reporting");
const eval = @import("eval");
const check = @import("check");
const unbundle = if (is_freestanding) struct {} else @import("unbundle");
const CoreCtx = @import("ctx").CoreCtx;

/// The underlying system I/O type, derived from CoreCtx to avoid
/// referencing the raw Zig I/O type directly (which is banned in core modules).
const Report = reporting.Report;
const BuiltinModules = eval.BuiltinModules;
const compile_package = @import("compile_package.zig");
const Mode = compile_package.Mode;
const Allocator = std.mem.Allocator;

/// The set of errors that can occur during a build (including `roc check`).
pub const BuildError = Allocator.Error || std.Thread.SpawnError || error{ ExpectedPlatformString, ExpectedString, FileNotFound, AccessDenied, StreamTooLong, IoError, InvalidNullByteInPath, PathOutsideWorkspace, UnsupportedHeader, Internal, DownloadFailed, FileError, InvalidUrl, NoCacheDir, NoPackageSource, UnsupportedBuiltinAnnotationOnly, BuiltinLowLevelAnnotationMustBeFunction, LowLevelOperationsNotFound, InvalidDependency, MissingFilesDirectory, MissingTargetFile };
/// Errors that can occur while initializing build inputs.
pub const InitError = Allocator.Error || BuiltinModules.InitError;
/// Errors that can occur while compiling discovered modules.
pub const CompileDiscoveredError = BuildError || compile_package.PublishError || error{ UnsupportedBuiltinAnnotationOnly, BuiltinLowLevelAnnotationMustBeFunction, LowLevelOperationsNotFound };
/// Errors that can occur while building a root module.
pub const BuildRootError = BuildError || CompileDiscoveredError;
/// Errors that can occur while building an app module.
pub const BuildAppError = BuildRootError || error{NotAnApp};
/// Errors that can occur while building with an explicit main module.
pub const BuildWithMainError = BuildError || CompileDiscoveredError;

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const SemanticModuleData = compile_package.SemanticModuleData;
const ModuleTimingInfo = compile_package.TimingInfo;
const CacheManager = @import("cache_manager.zig").CacheManager;
const package_source = @import("package_source.zig");
const compiler_platforms = @import("compiler_platforms.zig");
const package_resolution = @import("package_resolution.zig");
const package_identity = @import("package_identity.zig");
const watch_inputs = @import("watch_inputs.zig");
const module_discovery = @import("module_discovery.zig");

// Actor model components
const coordinator_mod = @import("coordinator.zig");
const Coordinator = coordinator_mod.Coordinator;
const roc_target = @import("roc_target");
const targets_config_mod = @import("targets_config.zig");

// Compile-time flag for build tracing - enabled via `zig build -Dtrace-build`
const trace_build = if (@hasDecl(build_options, "trace_build")) build_options.trace_build else false;

// Threading features aren't available when targeting WebAssembly,
// so we disable them at comptime to prevent builds from failing.
const threading = @import("threading.zig");
const is_freestanding = threading.is_freestanding;

const Mutex = threading.Mutex;
const ThreadCondition = threading.Condition;

/// Native fetchUrl implementation that downloads a tar.zst bundle via HTTP
/// and extracts it into the destination directory. Used by the CLI to wire up
/// real download support through the Filesystem vtable.
pub const nativeFetchUrl: ?*const fn (?*anyopaque, std.Io, Allocator, []const u8, []const u8, ?u64) CoreCtx.FetchUrlError!u64 = if (!is_freestanding)
    &nativeFetchUrlImpl
else
    null;

fn nativeFetchUrlImpl(_: ?*anyopaque, std_io: std.Io, allocator: Allocator, url: []const u8, dest_path: []const u8, max_expanded_bytes: ?u64) CoreCtx.FetchUrlError!u64 {
    var alloc = allocator;
    return unbundle.download.downloadAndExtract(&alloc, std_io, url, dest_path, .{
        .max_expanded_bytes = max_expanded_bytes,
    }) catch |err| switch (err) {
        error.ExpandedSizeLimitExceeded => error.ExpandedSizeLimitExceeded,
        error.OutOfMemory => error.OutOfMemory,
        error.AmbiguousVersion,
        error.ChecksumFailure,
        error.DecompressionFailed,
        error.DictionaryIdFlagUnsupported,
        error.DirectoryCreateFailed,
        error.EndOfStream,
        error.FileCreateFailed,
        error.FileError,
        error.FileTooLarge,
        error.FileWriteFailed,
        error.HashMismatch,
        error.HttpError,
        error.InvalidFilename,
        error.InvalidHash,
        error.InvalidPath,
        error.InvalidProxyUrl,
        error.InvalidTarHeader,
        error.InvalidUrl,
        error.InvalidVersion,
        error.LocalhostWasNotLoopback,
        error.MalformedBlock,
        error.MalformedFrame,
        error.NetworkError,
        error.NoDataExtracted,
        error.NoHashInUrl,
        error.ReadFailed,
        error.UnexpectedEndOfStream,
        error.WriteFailed,
        => error.DownloadFailed,
    };
}

fn freeSlice(gpa: Allocator, s: []u8) void {
    gpa.free(s);
}

fn freeConstSlice(gpa: Allocator, s: []const u8) void {
    gpa.free(@constCast(s));
}

// Rooted path + normalization helper
const PathUtils = struct {
    fn normalizeAndJoin(gpa: Allocator, root: []const u8, rel: []const u8) Allocator.Error![]const u8 {
        const joined = try std.fs.path.join(gpa, &.{ root, rel });
        errdefer gpa.free(joined);
        // Resolve .. and . components
        const canon = try std.fs.path.resolve(gpa, &.{joined});
        gpa.free(joined);
        return canon;
    }

    fn makeAbsolute(gpa: Allocator, base_dir: []const u8, path: []const u8) Allocator.Error![]const u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try std.fs.path.resolve(gpa, &.{path});
        } else {
            return try normalizeAndJoin(gpa, base_dir, path);
        }
    }

    fn isWithinRoot(candidate: []const u8, roots: []const []const u8) bool {
        for (roots) |root| {
            if (!std.mem.startsWith(u8, candidate, root)) continue;
            // Only match whole path segments so root "/x/app" does not capture
            // sibling "/x/app-secrets". An exact match counts, as does a match
            // where the next character (or the root's own trailing char) is a
            // path separator.
            if (candidate.len == root.len) return true;
            if (std.fs.path.isSep(candidate[root.len])) return true;
            if (root.len > 0 and std.fs.path.isSep(root[root.len - 1])) return true;
        }
        return false;
    }
};

/// Controls which post-check publication work runs after ordinary checking has completed.
pub const PostCheckPublicationMode = enum {
    /// No post-check work (diagnostics only).
    none,
    /// Publish the relation-bearing platform root once at finalization,
    /// including when checked source contains explicit runtime-error nodes.
    executable_artifacts,
};

// BuildEnv: workspace-level orchestrator for multi-package builds with local-only shorthands.
//
// Responsibilities:
// - Parse headers of app/package/platform modules (local paths only)
// - Build a package graph with shorthand alias maps
// - Enforce package rules: only apps may depend on platforms; no package may depend on apps
// - Coordinate all per-package and per-module build state
// - Aggregate reports deterministically across the workspace (depth then module name)
// - Keep ModuleEnv hermetic: the Coordinator passes each phase explicit dependency state
//
// This module is designed to be integrated at the compile layer to avoid base<->compile circular dependencies.
/// Main workspace-level build orchestrator that coordinates multiple packages
pub const BuildEnv = struct {
    gpa: Allocator,
    mode: Mode,
    max_threads: usize,
    target: roc_target.RocTarget,
    compiler_version: []const u8 = build_options.compiler_version,

    // Workspace roots for sandboxing (absolute, canonical)
    workspace_roots: std.array_list.Managed([]const u8),

    // Map of package identity -> Package
    packages: std.StringHashMapUnmanaged(Package) = .{},
    // Ordered sink over all packages (thread-safe, deterministic emission)
    sink: OrderedSink,

    // Actor model coordinator (owns all mutable compilation state)
    coordinator: ?*Coordinator = null,
    // Cache manager for compiled modules
    cache_manager: ?*CacheManager = null,
    // I/O abstraction for all OS operations (filesystem, stdio, env vars, etc.)
    filesystem: CoreCtx,
    // Explicit working directory for resolving relative paths
    cwd: []const u8,
    /// Whether to retain exact source byte states for watch-mode refreshes.
    track_watch_inputs: bool = false,

    /// Whether `compileDiscovered` validates that the selected platform target's
    /// declared link files exist before type checking.
    validate_target_files_for_selected_target: bool = false,

    /// Controls which checked-artifact publication work runs after ordinary
    /// checking has completed.
    ///
    /// Checking is not complete until the platform/app relation output completes,
    /// so `roc check` and `roc build` both finalize the relation-bearing platform
    /// root once (`.executable_artifacts`): finalization builds the platform/app
    /// relation and publishes the platform root, which also resolves the platform
    /// target config constants both flows depend on. `.none` runs no post-check
    /// work, for diagnostic-only embeddings that never link an executable.
    post_check_publication_mode: PostCheckPublicationMode = .executable_artifacts,

    /// Whether executable artifacts were published for this build. User
    /// diagnostics do not change this: checked recovery nodes remain valid
    /// lowering input and crash only if execution reaches them.
    executable_artifacts_finalized: bool = false,

    /// Compiler role to assign to the root module of this build.
    root_module_role: ModuleEnv.ModuleRole = .user,

    /// Post-canonicalization validation to apply to the root module of this
    /// build. `roc test` sets `.explicit_roots` because it runs the root file's
    /// top-level `expect`s, which are compile-time roots in their own right: a
    /// headerless root that is neither a type module nor a default app is a
    /// plain module there rather than a file missing its `main!`.
    root_validation: Can.Validation = .checking,

    /// Optional source directory used to resolve imports from the root module.
    root_source_dir_override: ?[]const u8 = null,

    /// Source mapping for a temporary synthetic root created by the CLI for a
    /// headerless default app. These slices are borrowed from CLI-owned storage
    /// that outlives this BuildEnv.
    synthetic_root_original_path: ?[]const u8 = null,
    synthetic_root_original_source: ?[]const u8 = null,
    synthetic_root_header_len: usize = 0,
    synthetic_root_header_lines: u32 = 0,
    synthetic_root_identity: bool = false,
    synthetic_root_platform_identity: bool = false,

    /// The bundle URL the root itself came from, when the build was launched
    /// from a URL or installed source. The URL—never the extracted path—
    /// is then the root's package identity, so direct-URL use and installed
    /// use of the same URL share one identity, and moving the extracted
    /// directory cannot change it.
    root_url: ?package_source.UrlSource = null,

    /// The bundle URL an explicitly supplied `--main` came from. Promoted to
    /// `root_url` if and only if that main file becomes the discovery root
    /// (see `buildResolvingMain`), since root identity must follow whichever
    /// file actually roots the build.
    main_url: ?package_source.UrlSource = null,

    /// Size limits applied during package version resolution.
    resolution_config: package_resolution.Config = .{},

    /// Identity -> note for packages compiled against a dependency version
    /// they did not declare; attached to error output from those packages.
    version_notes: std.StringHashMapUnmanaged([]const u8) = .{},

    /// The hash-addressed package cache directory used by resolution. Set
    /// before `build` to override the default location (used by tests);
    /// otherwise populated from the environment on first resolution.
    package_cache_dir: ?[]const u8 = null,

    /// Optional root for materialized compiler-owned platform sources.
    compiler_owned_source_dir: ?[]const u8 = null,

    // Builtin modules (Bool, Try, Str) shared across all packages (heap-allocated to prevent moves)
    builtin_modules: *BuiltinModules,
    owns_builtin_modules: bool,

    // Discovery state (populated by discoverDependencies, consumed by compileDiscovered)
    discovered_root_abs: ?[]const u8 = null,
    discovered_root_dir: ?[]const u8 = null,
    discovered_pkg_name: ?[]const u8 = null,
    entry_module_abs: ?[]const u8 = null,

    pub fn init(gpa: Allocator, mode: Mode, max_threads: usize, target: roc_target.RocTarget, cwd: []const u8, std_io: std.Io) InitError!BuildEnv {
        // Allocate builtin modules on heap to prevent moves that would invalidate internal pointers
        const builtin_modules = try gpa.create(BuiltinModules);
        errdefer gpa.destroy(builtin_modules);

        builtin_modules.* = try BuiltinModules.init(gpa);
        errdefer builtin_modules.deinit();

        return initWithBuiltinModules(gpa, mode, max_threads, target, cwd, std_io, builtin_modules, true);
    }

    pub fn initBorrowingBuiltinModules(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        cwd: []const u8,
        std_io: std.Io,
        builtin_modules: *BuiltinModules,
    ) BuildEnv {
        return initWithBuiltinModules(gpa, mode, max_threads, target, cwd, std_io, builtin_modules, false);
    }

    fn initWithBuiltinModules(
        gpa: Allocator,
        mode: Mode,
        max_threads: usize,
        target: roc_target.RocTarget,
        cwd: []const u8,
        std_io: std.Io,
        builtin_modules: *BuiltinModules,
        owns_builtin_modules: bool,
    ) BuildEnv {
        var env = BuildEnv{
            .gpa = gpa,
            .mode = mode,
            .max_threads = max_threads,
            .target = target,
            .cwd = cwd,
            .workspace_roots = std.array_list.Managed([]const u8).init(gpa),
            .sink = OrderedSink.init(gpa),
            .builtin_modules = builtin_modules,
            .owns_builtin_modules = owns_builtin_modules,
            .filesystem = CoreCtx.default(gpa, gpa, std_io),
        };

        // On native targets, enable HTTP downloads for URL packages.
        // On freestanding (WASM), fetchUrl remains the default stub (returns error.Unsupported).
        if (nativeFetchUrl) |fetch_fn| {
            env.filesystem.vtable.fetchUrl = fetch_fn;
        }
        // Note: `filesystem.vtable` is a value type so this mutation is safe.

        return env;
    }

    pub fn deinit(self: *BuildEnv) void {
        if (comptime trace_build) {
            std.debug.print("[DEINIT] BuildEnv.deinit starting\n", .{});
        }

        {
            var note_it = self.version_notes.iterator();
            while (note_it.next()) |entry| {
                self.gpa.free(@constCast(entry.key_ptr.*));
                self.gpa.free(@constCast(entry.value_ptr.*));
            }
            self.version_notes.deinit(self.gpa);
        }

        if (self.package_cache_dir) |dir| self.gpa.free(@constCast(dir));
        if (self.compiler_owned_source_dir) |dir| self.gpa.free(@constCast(dir));

        if (self.root_url) |*url| url.deinit(self.gpa);
        if (self.main_url) |*url| url.deinit(self.gpa);

        // Deinit and free owned builtin modules. Borrowed builtins outlive this
        // BuildEnv and are released by their owner.
        if (self.owns_builtin_modules) {
            self.builtin_modules.deinit();
            self.gpa.destroy(self.builtin_modules);
        }

        if (comptime trace_build) {
            std.debug.print("[DEINIT] builtin_modules done\n", .{});
        }

        // Deinit coordinator if present
        if (self.coordinator) |coord| {
            coord.deinit();
            self.gpa.destroy(coord);
        }
        if (comptime trace_build) {
            std.debug.print("[DEINIT] coordinator done\n", .{});
        }

        // Deinit cache manager if present
        if (self.cache_manager) |cm| {
            self.gpa.destroy(cm);
        }

        // Free discovery state
        if (self.discovered_root_abs) |ra| self.gpa.free(ra);
        if (self.discovered_root_dir) |rd| self.gpa.free(rd);
        if (self.entry_module_abs) |entry| self.gpa.free(@constCast(entry));
        // discovered_pkg_name is borrowed from the packages map key.

        if (comptime trace_build) {
            std.debug.print("[DEINIT] coordinator done, deinitializing packages...\n", .{});
        }

        // Deinit packages
        var pit = self.packages.iterator();
        while (pit.next()) |e| {
            var p = e.value_ptr.*;
            p.deinit(self.gpa);
            freeConstSlice(self.gpa, e.key_ptr.*);
        }
        self.packages.deinit(self.gpa);

        // Deinit roots and free the duplicated strings
        for (self.workspace_roots.items) |root| {
            self.gpa.free(root);
        }
        self.workspace_roots.deinit();

        self.sink.deinit();
    }

    /// Set the cache manager for this build environment
    pub fn setCacheManager(self: *BuildEnv, cache_manager: *CacheManager) void {
        self.cache_manager = cache_manager;
    }

    /// Create and attach the standard checked-module cache manager, owned by
    /// this BuildEnv (destroyed in deinit). Call before compilation starts so
    /// the Coordinator is constructed with it. Skipping cache attachment is
    /// an explicit `--no-cache` decision, never a default.
    pub fn enableDefaultCacheManager(self: *BuildEnv, verbose: bool) Allocator.Error!void {
        std.debug.assert(self.coordinator == null);
        std.debug.assert(self.cache_manager == null);
        const manager = try self.gpa.create(CacheManager);
        manager.* = CacheManager.init(self.gpa, .{
            .enabled = true,
            .verbose = verbose,
            .roc_ctx = self.filesystem,
        }, self.filesystem);
        self.cache_manager = manager;
    }

    /// Set the I/O implementation.
    pub fn setCoreCtx(self: *BuildEnv, roc_ctx: CoreCtx) void {
        self.filesystem = roc_ctx;
        self.sink.std_io = self.filesystem.std_io;
    }

    /// Get the TargetsConfig from the platform package, if any.
    pub fn getPlatformTargetsConfig(self: *const BuildEnv) ?targets_config_mod.TargetsConfig {
        var pit = self.packages.iterator();
        while (pit.next()) |entry| {
            if (entry.value_ptr.kind == .platform) {
                return entry.value_ptr.targets_config;
            }
        }
        return null;
    }

    /// Get the root_file of the platform package, if any.
    pub fn getPlatformRootFile(self: *const BuildEnv) ?[]const u8 {
        var pit = self.packages.iterator();
        while (pit.next()) |entry| {
            if (entry.value_ptr.kind == .platform) {
                return entry.value_ptr.root_file;
            }
        }
        return null;
    }

    /// Set the target for this build environment.
    /// Must be called before compileDiscovered() if target needs to change after discovery.
    pub fn setTarget(self: *BuildEnv, target: roc_target.RocTarget) void {
        self.target = target;
    }

    pub fn setValidateTargetFilesForSelectedTarget(self: *BuildEnv, enabled: bool) void {
        self.validate_target_files_for_selected_target = enabled;
    }

    pub fn setFinalizeExecutableArtifacts(self: *BuildEnv, enabled: bool) void {
        self.post_check_publication_mode = if (enabled) .executable_artifacts else .none;
        if (self.coordinator) |coord| coord.setExecutableFinalizationEnabled(enabled);
    }

    pub fn setPostCheckPublicationMode(self: *BuildEnv, mode: PostCheckPublicationMode) void {
        self.post_check_publication_mode = mode;
        if (self.coordinator) |coord| coord.setExecutableFinalizationEnabled(mode != .none);
    }

    pub fn setRootModuleRole(self: *BuildEnv, role: ModuleEnv.ModuleRole) void {
        self.root_module_role = role;
    }

    pub fn setRootValidation(self: *BuildEnv, validation: Can.Validation) void {
        self.root_validation = validation;
    }

    pub fn setRootSourceDirOverride(self: *BuildEnv, source_dir: []const u8) void {
        self.root_source_dir_override = source_dir;
    }

    pub fn setCompilerOwnedSourceDir(self: *BuildEnv, source_dir: []const u8) Allocator.Error!void {
        if (self.compiler_owned_source_dir) |old| {
            self.gpa.free(@constCast(old));
        }
        self.compiler_owned_source_dir = try self.gpa.dupe(u8, source_dir);
    }

    pub fn setSyntheticRootSourceMapping(
        self: *BuildEnv,
        original_path: []const u8,
        original_source: []const u8,
        header_len: usize,
    ) void {
        self.synthetic_root_original_path = original_path;
        self.synthetic_root_original_source = original_source;
        self.synthetic_root_header_len = header_len;
        self.synthetic_root_header_lines = 0;
        self.setSyntheticRootPackageIdentity();
        self.setSyntheticRootPlatformPackageIdentity();
    }

    pub fn setSyntheticRootSourceMappingWithLineOffset(
        self: *BuildEnv,
        original_path: []const u8,
        original_source: []const u8,
        header_len: usize,
        header_lines: u32,
    ) void {
        self.setSyntheticRootSourceMapping(original_path, original_source, header_len);
        self.synthetic_root_header_lines = header_lines;
    }

    pub fn setSyntheticRootPackageIdentity(self: *BuildEnv) void {
        self.synthetic_root_identity = true;
    }

    pub fn setSyntheticRootPlatformPackageIdentity(self: *BuildEnv) void {
        self.synthetic_root_platform_identity = true;
    }

    /// Declare that the root being compiled came from this bundle URL; see
    /// the `root_url` field. The URL must carry a valid trailing content
    /// hash, matching what download validation already enforced.
    pub fn setRootUrl(self: *BuildEnv, url: []const u8) error{ OutOfMemory, InvalidUrl }!void {
        const parsed = base.url.parseUrlPath(url) catch return error.InvalidUrl;
        if (self.root_url) |*existing| {
            existing.deinit(self.gpa);
            // Cleared before the fallible init so a failure cannot leave a
            // dangling pointer for deinit to double-free.
            self.root_url = null;
        }
        self.root_url = try package_source.UrlSource.init(self.gpa, .{ .url = url, .url_id = parsed.url_id });
    }

    /// Declare that an explicitly supplied `--main` came from this bundle
    /// URL; see the `main_url` field.
    pub fn setMainUrl(self: *BuildEnv, url: []const u8) error{ OutOfMemory, InvalidUrl }!void {
        const parsed = base.url.parseUrlPath(url) catch return error.InvalidUrl;
        if (self.main_url) |*existing| {
            existing.deinit(self.gpa);
            self.main_url = null;
        }
        self.main_url = try package_source.UrlSource.init(self.gpa, .{ .url = url, .url_id = parsed.url_id });
    }

    pub fn setWatchInputTracking(self: *BuildEnv, enabled: bool) void {
        self.track_watch_inputs = enabled;
        if (self.coordinator) |coord| coord.setWatchInputTracking(enabled);
    }

    /// Build an app file specifically (validates it's an app)
    pub fn buildApp(self: *BuildEnv, app_file: []const u8) BuildAppError!void {
        // Build and let the main function handle everything
        // The build function accepts both apps and modules
        try self.build(app_file);

        // After building, verify it was actually an app
        // Check the package we just created
        const pkg_name = self.discovered_pkg_name orelse return error.NotAnApp;
        const pkg = self.packages.get(pkg_name);
        if (pkg == null or pkg.?.kind != .app) {
            // If it wasn't an app, return an error
            return error.NotAnApp;
        }
    }

    // Build the workspace starting from an app root file path.
    // Assumptions:
    // - All header-declared paths are local filesystem paths (no URLs).
    // - Shorthand aliases uniquely identify packages within this workspace.
    //
    // Uses the actor model coordinator for both single-threaded and multi-threaded modes.
    // The coordinator uses message passing to eliminate race conditions.
    pub fn build(self: *BuildEnv, root_file: []const u8) BuildRootError!void {
        try self.discoverDependencies(root_file);
        try self.compileDiscovered();
    }

    pub fn buildWithMain(self: *BuildEnv, root_file: []const u8, main_file: []const u8) BuildWithMainError!void {
        try self.discoverDependencies(main_file);
        try self.setDiscoveredEntryModule(root_file);
        try self.compileDiscovered();
    }

    /// Walk `start_dir` and its parents for a file named `main.roc`.
    /// Returns an owned absolute path the caller must free, or null if none exists.
    pub fn findOwningMainRoc(gpa: Allocator, filesystem: CoreCtx, start_dir: []const u8) Allocator.Error!?[]u8 {
        var current = try std.fs.path.resolve(gpa, &.{start_dir});
        errdefer gpa.free(current);

        while (true) {
            const candidate = try std.fs.path.join(gpa, &.{ current, "main.roc" });
            defer gpa.free(candidate);

            if (filesystem.fileExists(candidate)) {
                const abs = try std.fs.path.resolve(gpa, &.{candidate});
                gpa.free(current);
                return abs;
            }

            const parent = std.fs.path.dirname(current) orelse {
                gpa.free(current);
                return null;
            };
            if (parent.len == 0 or std.mem.eql(u8, parent, current)) {
                gpa.free(current);
                return null;
            }
            const next = try gpa.dupe(u8, parent);
            gpa.free(current);
            current = next;
        }
    }

    /// Build `root_file`, using package aliases from an owning app/package/platform
    /// `main.roc` when the checked file is a child module.
    ///
    /// A root file that carries its own packages (app/default_app/package/platform)
    /// always uses its own header, regardless of `preferred_main`: `preferred_main`
    /// exists to supply package aliases for files that declare none of their own
    /// (module/type_module/hosted), not to override a root's own dependency graph.
    ///
    /// `preferred_main` (e.g. LSP workspace `{root}/main.roc` or CLI `--main`) wins
    /// when set and different from `root_file`. Otherwise module/type_module/hosted
    /// roots walk ancestor directories for `main.roc` (same convention as #6538).
    pub fn buildResolvingMain(self: *BuildEnv, root_file: []const u8, preferred_main: ?[]const u8) BuildWithMainError!void {
        const root_abs = try self.makeAbsolute(root_file);
        defer self.gpa.free(root_abs);

        const kind = self.peekPackageKind(root_abs) catch null;
        const carries_packages = kind == .app or kind == .default_app or kind == .package or kind == .platform;
        if (carries_packages) {
            try self.build(root_file);
            return;
        }

        if (preferred_main) |main_path| {
            const main_abs = try self.makeAbsolute(main_path);
            defer self.gpa.free(main_abs);
            if (std.mem.eql(u8, root_abs, main_abs)) {
                try self.build(root_file);
            } else {
                // The main file is the discovery root here, so its bundle
                // provenance—not the checked file's—is the root identity.
                if (self.main_url) |*main_url| {
                    if (self.root_url) |*existing| {
                        existing.deinit(self.gpa);
                        self.root_url = null;
                    }
                    self.root_url = try package_source.UrlSource.init(self.gpa, main_url.view());
                }
                try self.buildWithMain(root_file, main_path);
            }
            return;
        }

        const start_dir = std.fs.path.dirname(root_abs) orelse ".";
        if (try findOwningMainRoc(self.gpa, self.filesystem, start_dir)) |main_abs| {
            defer self.gpa.free(main_abs);
            if (!std.mem.eql(u8, root_abs, main_abs)) {
                try self.buildWithMain(root_file, main_abs);
                return;
            }
        }

        try self.build(root_file);
    }

    /// Silently read the package/header kind of `file_abs` without emitting reports.
    /// Returns null when the file cannot be read or parsed.
    fn peekPackageKind(self: *BuildEnv, file_abs: []const u8) Allocator.Error!?PackageKind {
        const src = self.readFile(file_abs) catch return null;
        defer self.gpa.free(src);

        var env = try ModuleEnv.init(self.gpa, src);
        defer env.deinit();

        try env.common.calcLineStarts(self.gpa);

        const ast = try parse.file(self.gpa, &env.common);
        defer ast.deinit();

        if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) {
            return null;
        }

        const file = ast.store.getFile();
        const header = ast.store.getHeader(file.header);
        return switch (header) {
            .app => |a| if (a.platform_idx == null) .default_app else .app,
            .package => .package,
            .platform => .platform,
            .module => .module,
            .hosted => .hosted,
            .type_module => if (ast.hasMainBangDecl()) .default_app else .type_module,
            .default_app => .default_app,
            .malformed => null,
        };
    }

    fn setDiscoveredEntryModule(self: *BuildEnv, root_file: []const u8) BuildError!void {
        _ = self.discovered_pkg_name orelse return error.Internal;
        const root_abs = try self.makeAbsolute(root_file);
        errdefer self.gpa.free(root_abs);
        if (self.entry_module_abs) |old| self.gpa.free(@constCast(old));
        self.entry_module_abs = root_abs;
    }

    /// Initialize the actor model coordinator.
    /// This must be called before compileDiscovered().
    pub fn initCoordinator(self: *BuildEnv) Allocator.Error!void {
        if (self.coordinator != null) return; // Already initialized

        // Propagate std_io to the ordered sink for its mutex operations
        self.sink.std_io = self.filesystem.std_io;

        const coord = try self.gpa.create(Coordinator);
        coord.* = try Coordinator.init(
            self.gpa,
            self.mode,
            self.max_threads,
            self.target,
            self.builtin_modules,
            self.compiler_version,
            self.cache_manager,
            self.filesystem,
        );
        // Enable hosted transform for platform modules - converts e_anno_only to e_hosted_lambda
        // This is required for roc build so that hosted functions can be called at runtime
        coord.enable_hosted_transform = true;
        coord.setWatchInputTracking(self.track_watch_inputs);
        coord.setExecutableFinalizationEnabled(self.post_check_publication_mode != .none);
        self.coordinator = coord;
    }

    /// Register a directory as a workspace root, skipping it if an existing root
    /// already contains it. Takes ownership of nothing; copies `dir` when stored.
    fn addWorkspaceRoot(self: *BuildEnv, dir: []const u8) Allocator.Error!void {
        if (PathUtils.isWithinRoot(dir, self.workspace_roots.items)) return;
        try self.workspace_roots.append(try self.gpa.dupe(u8, dir));
    }

    /// Phase 1: Parse headers, create package entries, extract TargetsConfig, and populate
    /// shorthands. Does NOT init the Coordinator, allowing the caller to inspect
    /// discovered state (e.g., TargetsConfig) and change the target before compilation.
    pub fn discoverDependencies(self: *BuildEnv, root_file: []const u8) BuildError!void {
        // Parse root file header
        const root_abs = try self.makeAbsolute(root_file);
        // Store immediately so deinit() frees on any subsequent error
        self.discovered_root_abs = root_abs;
        const root_dir = if (std.fs.path.dirname(root_abs)) |d| try std.fs.path.resolve(self.gpa, &.{d}) else try self.gpa.dupe(u8, ".");
        self.discovered_root_dir = root_dir;

        try self.addWorkspaceRoot(root_dir);

        var header_info = try self.parseHeaderDeps(root_abs);
        defer header_info.deinit(self.gpa);

        // Every header kind names a module this build can root at: apps and
        // default apps produce programs, and the rest are compiled for their own
        // definitions and `expect`s. A file whose header parsed into none of them
        // already failed in `parseHeaderDeps`.

        // Create package entry keyed by stable package identity. Real roots use
        // their canonical path; URL-launched roots use their bundle URL because
        // the extracted directory is a storage detail; synthetic default-app
        // roots keep the explicit synthetic identity because their temporary
        // paths are ephemeral.
        const root_identity = try package_identity.packageIdentityFor(
            self.gpa,
            self.filesystem,
            if (self.synthetic_root_identity)
                .synthetic_app
            else if (self.root_url) |*root_url|
                .{ .url = root_url.url }
            else
                .{ .local_path = root_abs },
        );
        defer self.gpa.free(root_identity);

        const key_pkg = try self.gpa.dupe(u8, root_identity);
        const pkg_root_file = try self.gpa.dupe(u8, root_abs);
        const pkg_root_dir = try self.gpa.dupe(u8, root_dir);

        try self.packages.put(self.gpa, key_pkg, .{
            .name = try self.gpa.dupe(u8, root_identity),
            .kind = header_info.kind,
            .root_file = pkg_root_file,
            .root_file_state = header_info.source_file_state,
            .root_dir = pkg_root_dir,
            .url = if (self.root_url) |*root_url| try package_source.UrlSource.init(self.gpa, root_url.view()) else null,
        });
        self.discovered_pkg_name = key_pkg;

        // Transfer provides entries from header to package for app or platform roots.
        // For platforms, also transfer targets_config.
        if (header_info.kind == .platform or header_info.kind == .app or header_info.kind == .default_app) {
            if (self.packages.getPtr(key_pkg)) |pkg| {
                pkg.provides_entries = header_info.provides_entries;
                header_info.provides_entries = .empty; // Prevent double-free in deinit
                pkg.targets_config = header_info.targets_config;
                header_info.targets_config = null; // Prevent double-free in deinit
            }
        }
        if (header_info.kind == .package or header_info.kind == .platform) {
            if (self.packages.getPtr(key_pkg)) |pkg| {
                self.moveHeaderPublicSurfaceToPackage(pkg, &header_info);
            }
        }

        // Resolve the full dependency graph (downloads plus version solving),
        // then materialize the resolved packages and their shorthands.
        if (header_info.kind == .app or header_info.kind == .default_app or header_info.kind == .package or header_info.kind == .platform) {
            try self.resolveAndMaterialize(key_pkg, header_info.resolver_root);
        }
    }

    /// Phase 2: Initialize the Coordinator, create coordinator packages from the
    /// discovered BuildEnv packages, and run compilation to completion.
    /// Must be called after discoverDependencies().
    pub fn compileDiscovered(self: *BuildEnv) CompileDiscoveredError!void {
        const pkg_name = self.discovered_pkg_name orelse unreachable; // Must call discoverDependencies() first

        if (self.validate_target_files_for_selected_target) {
            try self.validateDiscoveredPlatformTargetFilesForCurrentTarget();
        }

        // Initialize coordinator if not already done
        try self.initCoordinator();
        const coord = self.coordinator.?;

        // Look up the root file from the package entry (already stored by discoverDependencies)
        const root_pkg = self.packages.get(pkg_name) orelse unreachable; // Must call discoverDependencies() first
        const pkg_root_file = root_pkg.root_file;

        // Create coordinator packages mirroring BuildEnv packages
        // Skip .module kind packages - these are platform-exposed modules (e.g., Stdout)
        // that were incorrectly registered as packages. They should be modules within
        // the platform package, not separate packages.
        if (comptime trace_build) {
            std.debug.print("[BUILD] Creating coordinator packages from {} BuildEnv packages:\n", .{self.packages.count()});
        }
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;

            // Skip module-type packages - they're platform-exposed modules, not real packages
            // BUT don't skip the main package we're building, even if it's a module
            const is_main_pkg = std.mem.eql(u8, entry.key_ptr.*, pkg_name);
            if (!is_main_pkg and (pkg.kind == .module or pkg.kind == .type_module)) {
                if (comptime trace_build) {
                    std.debug.print("[BUILD]   Skipping module-as-package: {s}\n", .{entry.key_ptr.*});
                }
                continue;
            }

            if (comptime trace_build) {
                std.debug.print("[BUILD]   Package: {s} root_dir={s} kind={}\n", .{ entry.key_ptr.*, pkg.root_dir, pkg.kind });
            }
            const coord_pkg = if (pkg.url) |url|
                try coord.ensurePackageWithUrl(entry.key_ptr.*, pkg.root_dir, url.view())
            else
                try coord.ensurePackage(entry.key_ptr.*, pkg.root_dir);
            try coord_pkg.setRootInput(self.gpa, pkg.root_file, pkg.root_file_state);
            for (pkg.public_surface.modules.items) |public_module| {
                try coord_pkg.addPublicModule(
                    self.gpa,
                    public_module.name,
                    public_module.target,
                    public_module.nested_type,
                );
            }
            try coord_pkg.finishPublicModules(self.gpa);

            // The coordinator gates the hosted transform (and app-root artifact
            // lookups) on knowing which package is the app; app modules must
            // never have annotation-only defs rewritten into hosted lambdas,
            // and app-less builds (platform/package/module roots) record that
            // explicitly so every module takes the transform.
            if (is_main_pkg) {
                if (pkg.kind == .app or pkg.kind == .default_app) {
                    coord.markAppPackage(coord_pkg.name);
                } else {
                    coord.markNoAppPackage();
                }
            }

            // Copy shorthands to coordinator package
            // Only copy shorthands that map to real packages, not module-as-package entries
            var sh_it = pkg.shorthands.iterator();
            while (sh_it.next()) |sh_entry| {
                const target_name = sh_entry.value_ptr.name;
                // Check if the target is a real package or a module-as-package
                if (self.packages.get(target_name)) |target_pkg| {
                    if (target_pkg.kind == .module or target_pkg.kind == .type_module) {
                        if (comptime trace_build) {
                            std.debug.print("[BUILD]     Skipping shorthand to module: {s} -> {s}\n", .{ sh_entry.key_ptr.*, target_name });
                        }
                        continue;
                    }
                }

                if (comptime trace_build) {
                    std.debug.print("[BUILD]     Shorthand: {s} -> {s}\n", .{ sh_entry.key_ptr.*, target_name });
                }
                try coord_pkg.shorthands.put(
                    try self.gpa.dupe(u8, sh_entry.key_ptr.*),
                    try self.gpa.dupe(u8, target_name),
                );
            }
        }

        // Queue root module in coordinator
        const coord_pkg = coord.getPackage(pkg_name).?;
        const module_name = base.module_path.getModuleName(pkg_root_file);
        const root_id = try coord_pkg.ensureModule(self.gpa, module_name, pkg_root_file);
        coord_pkg.modules.items[root_id].module_role = self.root_module_role;
        coord_pkg.modules.items[root_id].validation = self.root_validation;
        if (self.root_source_dir_override) |source_dir| {
            coord_pkg.modules.items[root_id].source_dir_override = try self.gpa.dupe(u8, source_dir);
        }
        if (self.packages.get(pkg_name)) |queued_root_pkg| {
            if (queued_root_pkg.kind == .platform) {
                coord_pkg.modules.items[root_id].explicit_root_ident_names = try self.targetConfigRootIdentNames(queued_root_pkg.targets_config);
            }
        }
        coord_pkg.modules.items[root_id].depth = 0;
        coord_pkg.root_module_id = root_id;
        coord_pkg.remaining_modules += 1;
        coord.total_remaining += 1;

        // Start workers (for multi-threaded mode)
        try coord.start();

        // Queue initial parse task
        try coord.enqueueParseTask(pkg_name, root_id);

        if (self.entry_module_abs) |entry_file| {
            if (!std.mem.eql(u8, entry_file, pkg_root_file)) {
                const entry_module_name = base.module_path.getModuleName(entry_file);
                const entry_id = try coord_pkg.ensureModule(self.gpa, entry_module_name, entry_file);
                const entry_module = &coord_pkg.modules.items[entry_id];
                entry_module.validation = .explicit_roots;
                if (entry_module.phase == .Parse) {
                    entry_module.depth = 0;
                    coord_pkg.remaining_modules += 1;
                    coord.total_remaining += 1;
                    try coord.enqueueParseTask(pkg_name, entry_id);
                }
            }
        }

        // Also queue the platform's root module if this is an app
        // The platform's root module contains the `requires` clause which must be compiled
        // for type checking against the app's exports
        var pf_it = self.packages.iterator();
        while (pf_it.next()) |pf_entry| {
            const pf_pkg = pf_entry.value_ptr.*;
            if (pf_pkg.kind == .platform) {
                if (coord.getPackage(pf_entry.key_ptr.*)) |platform_coord_pkg| {
                    coord.markPlatformPackage(platform_coord_pkg.name);
                    const plat_module_name = base.module_path.getModuleName(pf_pkg.root_file);
                    const plat_root_id = try platform_coord_pkg.ensureModule(self.gpa, plat_module_name, pf_pkg.root_file);
                    if (platform_coord_pkg.modules.items[plat_root_id].phase == .Parse) {
                        platform_coord_pkg.modules.items[plat_root_id].explicit_root_ident_names = try self.targetConfigRootIdentNames(pf_pkg.targets_config);
                        platform_coord_pkg.modules.items[plat_root_id].depth = 1;
                        platform_coord_pkg.root_module_id = plat_root_id;
                        platform_coord_pkg.remaining_modules += 1;
                        coord.total_remaining += 1;
                        try coord.enqueueParseTask(pf_entry.key_ptr.*, plat_root_id);
                        if (comptime trace_build) {
                            std.debug.print("[BUILD] Queued platform root module: {s} in package {s}\n", .{ plat_module_name, pf_entry.key_ptr.* });
                        }
                    }
                }
            }
        }

        // Run coordinator loop. On failure, still move whatever reports have
        // accumulated into the ordered sink so callers can drain and render
        // them before propagating the error.
        self.executable_artifacts_finalized = false;
        coord.coordinatorLoop() catch |err| {
            self.emitAccumulatedReportsForError();
            return err;
        };
        var finalized_executable = false;
        switch (self.post_check_publication_mode) {
            .none => {},
            .executable_artifacts => {
                coord.finalizeExecutableArtifacts() catch |err| {
                    self.emitAccumulatedReportsForError();
                    return err;
                };
                finalized_executable = true;
            },
        }

        self.executable_artifacts_finalized = finalized_executable;

        try self.resolvePlatformTargetConfigConstants();

        try self.emitCoordinatorReports();

        // Deterministic emission
        try self.emitDeterministic();

        if (comptime trace_build) {
            std.debug.print("[BUILD] compileDiscovered complete\n", .{});
        }
    }

    fn targetConfigRootIdentNames(
        self: *BuildEnv,
        maybe_targets_config: ?targets_config_mod.TargetsConfig,
    ) Allocator.Error![]const []const u8 {
        var names = std.ArrayList([]const u8).empty;
        errdefer {
            for (names.items) |name| self.gpa.free(name);
            names.deinit(self.gpa);
        }

        const targets_config = maybe_targets_config orelse return try names.toOwnedSlice(self.gpa);
        var seen = std.StringHashMapUnmanaged(void){};
        defer seen.deinit(self.gpa);

        try self.appendTargetConfigRootIdentNames(&names, &seen, targets_config.targets);

        return try names.toOwnedSlice(self.gpa);
    }

    fn appendTargetConfigRootIdentNames(
        self: *BuildEnv,
        names: *std.ArrayList([]const u8),
        seen: *std.StringHashMapUnmanaged(void),
        specs: []const targets_config_mod.TargetLinkSpec,
    ) Allocator.Error!void {
        for (specs) |spec| {
            const wasm = spec.wasm orelse continue;
            try self.appendTargetConfigRootIdentName(names, seen, wasm.import_memory_ident);
            try self.appendTargetConfigRootIdentName(names, seen, wasm.minimum_memory_ident);
            try self.appendTargetConfigRootIdentName(names, seen, wasm.maximum_memory_ident);
            try self.appendTargetConfigRootIdentName(names, seen, wasm.initial_stack_size_ident);
            try self.appendTargetConfigRootIdentName(names, seen, wasm.global_base_ident);
        }
    }

    fn appendTargetConfigRootIdentName(
        self: *BuildEnv,
        names: *std.ArrayList([]const u8),
        seen: *std.StringHashMapUnmanaged(void),
        maybe_ident: ?[]const u8,
    ) Allocator.Error!void {
        const ident = maybe_ident orelse return;
        const entry = try seen.getOrPut(self.gpa, ident);
        if (entry.found_existing) return;
        entry.value_ptr.* = {};
        errdefer _ = seen.remove(ident);
        const owned = try self.gpa.dupe(u8, ident);
        errdefer self.gpa.free(owned);
        try names.append(self.gpa, owned);
    }

    const PackageKind = enum { app, package, platform, module, hosted, type_module, default_app };

    /// A mapping from a Roc identifier to an FFI symbol name, extracted from
    /// a platform's `provides { "roc_ffi_symbol": roc_ident }` clause.
    pub const ProvidesEntry = struct {
        roc_ident: []const u8,
        ffi_symbol: []const u8,
    };

    const PublicSurface = module_discovery.PublicSurface;

    const PackageRef = struct {
        name: []const u8, // Package name (alias in workspace)
        root_file: []const u8, // Absolute path to root module of the package
    };

    const Package = struct {
        name: []u8,
        kind: PackageKind,
        root_file: []u8,
        root_file_state: ?watch_inputs.State,
        root_dir: []u8,
        url: ?package_source.UrlSource = null,
        shorthands: std.StringHashMapUnmanaged(PackageRef) = .{},
        /// Source-local public declarations from a package or platform header.
        public_surface: PublicSurface = .{},
        provides_entries: std.ArrayListUnmanaged(ProvidesEntry) = .empty,
        targets_config: ?targets_config_mod.TargetsConfig = null,

        fn deinit(self: *Package, gpa: Allocator) void {
            if (self.url) |*url| url.deinit(gpa);
            if (self.targets_config) |tc| tc.deinit(gpa);
            self.public_surface.deinit(gpa);
            for (self.provides_entries.items) |entry| {
                freeConstSlice(gpa, entry.roc_ident);
                freeConstSlice(gpa, entry.ffi_symbol);
            }
            self.provides_entries.deinit(gpa);
            var it = self.shorthands.iterator();
            while (it.next()) |e| {
                freeConstSlice(gpa, e.key_ptr.*);
                freeConstSlice(gpa, e.value_ptr.name);
                freeConstSlice(gpa, e.value_ptr.root_file);
            }
            self.shorthands.deinit(gpa);
            freeSlice(gpa, self.name);
            freeSlice(gpa, self.root_file);
            freeSlice(gpa, self.root_dir);
        }
    };

    const HeaderInfo = struct {
        kind: PackageKind,
        source_file_state: ?watch_inputs.State,
        /// Source-local public declarations from a package or platform header.
        public_surface: PublicSurface = .{},
        resolver_root: package_resolution.FetchedPackage,
        /// Platform provides entries (roc_ident -> ffi_symbol mapping)
        provides_entries: std.ArrayListUnmanaged(ProvidesEntry) = .empty,
        /// Targets configuration extracted from platform header
        targets_config: ?targets_config_mod.TargetsConfig = null,

        fn deinit(self: *HeaderInfo, gpa: Allocator) void {
            if (self.targets_config) |tc| tc.deinit(gpa);
            self.resolver_root.deinit(gpa);
            self.public_surface.deinit(gpa);
            for (self.provides_entries.items) |entry| {
                freeConstSlice(gpa, entry.roc_ident);
                freeConstSlice(gpa, entry.ffi_symbol);
            }
            self.provides_entries.deinit(gpa);
        }
    };

    fn clearPackagePublicSurface(self: *BuildEnv, pkg: *Package) void {
        pkg.public_surface.deinit(self.gpa);
        pkg.public_surface = .{};
    }

    fn moveHeaderPublicSurfaceToPackage(self: *BuildEnv, pkg: *Package, header_info: *HeaderInfo) void {
        self.clearPackagePublicSurface(pkg);
        pkg.public_surface = header_info.public_surface;
        header_info.public_surface = .{};
    }

    fn setHeaderPublicSurface(
        self: *BuildEnv,
        info: *HeaderInfo,
        ast: *const parse.AST,
        exposes: parse.AST.Collection.Idx,
        kind: module_discovery.PublicSurfaceKind,
    ) (Allocator.Error || error{PathOutsideWorkspace})!void {
        var surface = module_discovery.extractPublicSurface(ast, exposes, kind, self.gpa) catch |err| switch (err) {
            error.ImportEscapesPackageRoot => return error.PathOutsideWorkspace,
            error.OutOfMemory => return error.OutOfMemory,
        };
        errdefer surface.deinit(self.gpa);
        info.public_surface.deinit(self.gpa);
        info.public_surface = surface;
    }

    fn parseHeaderDeps(self: *BuildEnv, file_path: []const u8) BuildError!HeaderInfo {
        // Read source
        const file_abs = try std.fs.path.resolve(self.gpa, &.{file_path});
        defer self.gpa.free(file_abs);
        const source_read = self.readHeaderSourceFile(file_abs) catch |err| {
            const report = blk: switch (err) {
                error.FileNotFound => {
                    const headline = try std.fmt.allocPrint(self.gpa, "I could not find the file {s}.", .{file_abs});
                    defer self.gpa.free(headline);
                    var report = try Report.init(self.gpa, "File Not Found", headline, .fatal);
                    try report.document.addText("Make sure the file exists and you do not have any typos in its name or path.");
                    break :blk report;
                },

                error.AccessDenied,
                error.IoError,
                error.OutOfMemory,
                error.StreamTooLong,
                => {
                    const headline = try std.fmt.allocPrint(self.gpa, "I could not read the file {s}.", .{file_abs});
                    defer self.gpa.free(headline);
                    var other_report = try Report.init(self.gpa, "Could Not Read File", headline, .fatal);
                    try other_report.document.addText("I did get the following error: ");
                    try other_report.addErrorMessage(@errorName(err));
                    try other_report.document.addText("Make sure the file can be read.");
                    break :blk other_report;
                },
            };
            try self.sink.emitReport("main", file_abs, report);
            try self.sink.buildOrder(&[_][]const u8{"main"}, &[_][]const u8{file_abs}, &[_]u32{0});
            self.sink.tryEmit();
            return err;
        };
        const src = source_read.source;
        defer self.gpa.free(src);

        var env = try ModuleEnv.init(self.gpa, src);
        defer env.deinit();

        try env.common.calcLineStarts(self.gpa);

        const ast = try parse.file(self.gpa, &env.common);
        defer ast.deinit();

        // Check for parse errors - if any exist, we cannot proceed
        if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) {
            // Convert diagnostics to reports and emit them
            // Use placeholder package name since we haven't determined it yet
            const pkg_name = "main";
            const module_name = file_abs;

            for (ast.tokenize_diagnostics.items) |diagnostic| {
                const report = try ast.tokenizeDiagnosticToReport(diagnostic, self.gpa, file_abs);
                try self.sink.emitReport(pkg_name, module_name, report);
            }
            for (ast.parse_diagnostics.items) |diagnostic| {
                const report = try ast.parseDiagnosticToReport(&env.common, diagnostic, self.gpa, file_abs);
                try self.sink.emitReport(pkg_name, module_name, report);
            }

            // Build the order so drainReports can find these reports
            try self.sink.buildOrder(&[_][]const u8{pkg_name}, &[_][]const u8{module_name}, &[_]u32{0});

            // Emit ready entries (marks them as emitted so they can be drained)
            self.sink.tryEmit();

            return error.UnsupportedHeader;
        }

        const file = ast.store.getFile();
        const header = ast.store.getHeader(file.header);

        const resolver_root = package_resolution.scanParsedHeader(self.gpa, file_abs, src, ast, header) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.HeaderParseFailed => return error.ExpectedString,
        };
        var info = HeaderInfo{
            .kind = .package,
            .source_file_state = source_read.file_state,
            .resolver_root = resolver_root,
        };
        errdefer info.deinit(self.gpa);

        switch (header) {
            .app => |a| {
                // An app that names no platform gets the built-in Echo
                // platform, which is what a default app is.
                info.kind = if (a.platform_idx == null) .default_app else .app;
            },
            .package => |p| {
                info.kind = .package;
                try self.setHeaderPublicSurface(&info, ast, p.exposes, .package);
            },
            .platform => |p| {
                info.kind = .platform;
                try self.setHeaderPublicSurface(&info, ast, p.exposes, .platform);

                // Extract provides entries (roc_ident -> linker symbol mapping)
                for (ast.store.symbolMapEntrySlice(p.provides)) |entry_idx| {
                    const entry = ast.store.getSymbolMapEntry(entry_idx);
                    const roc_ident = ast.resolve(entry.func);
                    const ffi_symbol = ast.resolve(entry.symbol);
                    try info.provides_entries.append(self.gpa, .{
                        .roc_ident = try self.gpa.dupe(u8, roc_ident),
                        .ffi_symbol = try self.gpa.dupe(u8, ffi_symbol),
                    });
                }

                // Extract targets config from the platform AST
                info.targets_config = try targets_config_mod.TargetsConfig.fromAST(self.gpa, ast);
            },
            .module => {
                info.kind = .module;
                // Module headers don't have package dependencies, just exports/imports
                // We'll handle imports during the module build process
            },
            .hosted => {
                info.kind = .hosted;
                // Hosted headers are like modules but for platform-specific code
            },
            .type_module => {
                // Check if file has a main! function, making it a default app
                info.kind = if (ast.hasMainBangDecl()) .default_app else .type_module;
            },
            .default_app => {
                info.kind = .default_app;
                // Default app headers are for REPL-style execution
            },
            .malformed => return error.UnsupportedHeader,
        }

        return info;
    }

    fn makeAbsolute(self: *BuildEnv, path: []const u8) Allocator.Error![]u8 {
        if (std.fs.path.isAbsolute(path)) return try std.fs.path.resolve(self.gpa, &.{path});
        return try std.fs.path.resolve(self.gpa, &.{ self.cwd, path });
    }

    fn readFile(self: *BuildEnv, path: []const u8) (Allocator.Error || error{FileNotFound})![]u8 {
        const data = self.filesystem.readFile(path, self.gpa) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.OutOfMemory => return error.OutOfMemory,
            error.AccessDenied,
            error.IoError,
            error.StreamTooLong,
            => return error.FileNotFound,
        };

        // Normalize line endings (CRLF -> LF) for consistent cross-platform behavior.
        return base.source_utils.normalizeLineEndingsRealloc(self.gpa, data);
    }

    const SourceRead = struct {
        source: []u8,
        file_state: ?watch_inputs.State,
    };

    fn readHeaderSourceFile(self: *BuildEnv, path: []const u8) (Allocator.Error || error{ FileNotFound, AccessDenied, StreamTooLong, IoError })!SourceRead {
        if (!self.track_watch_inputs) {
            return .{
                .source = try self.readFile(path),
                .file_state = null,
            };
        }

        return try self.readSourceFileWithState(path);
    }

    fn readSourceFileWithState(self: *BuildEnv, path: []const u8) (Allocator.Error || error{ FileNotFound, AccessDenied, StreamTooLong, IoError })!SourceRead {
        const data = self.filesystem.readFile(path, self.gpa) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.AccessDenied => return error.AccessDenied,
            error.StreamTooLong => return error.StreamTooLong,
            error.IoError => return error.IoError,
            error.OutOfMemory => return error.OutOfMemory,
        };
        const file_state: watch_inputs.State = .{ .hash = watch_inputs.hashBytes(data) };

        const source = base.source_utils.normalizeLineEndingsRealloc(self.gpa, data) catch |err| {
            self.gpa.free(data);
            return err;
        };

        return .{
            .source = source,
            .file_state = file_state,
        };
    }

    fn currentFileWatchState(self: *BuildEnv, path: []const u8) Allocator.Error!watch_inputs.State {
        const data = self.filesystem.readFile(path, self.gpa) catch |err| switch (err) {
            error.FileNotFound => return .missing,
            error.AccessDenied, error.StreamTooLong, error.IoError => return .unreadable,
            error.OutOfMemory => return error.OutOfMemory,
        };
        defer self.gpa.free(data);

        return .{ .hash = watch_inputs.hashBytes(data) };
    }

    /// Cross-platform environment variable lookup.
    /// Uses the filesystem vtable which works on both POSIX, Windows, and wasm
    /// (unlike std.posix.getenv which only works on POSIX systems).
    fn getEnvVar(self: *BuildEnv, allocator: Allocator, key: []const u8) Allocator.Error!?[]const u8 {
        return self.filesystem.getEnvVar(key, allocator) catch |err| switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.EnvironmentVariableMissing => null,
        };
    }

    /// Get the roc cache directory for downloaded packages.
    /// Standard cache locations by platform:
    /// - Linux/macOS: ~/.cache/roc/packages/ (respects XDG_CACHE_HOME if set)
    /// - Windows: %LOCALAPPDATA%\roc\packages\
    fn getRocCacheDir(self: *BuildEnv, allocator: Allocator) BuildError![]const u8 {
        // Check XDG_CACHE_HOME first (Linux/macOS)
        if (try self.getEnvVar(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
            defer allocator.free(xdg_cache);
            return std.fs.path.join(allocator, &.{ xdg_cache, "roc", "packages" });
        }

        // Fall back to %LOCALAPPDATA%\roc\packages (Windows)
        if (comptime builtin.os.tag == .windows) {
            if (try self.getEnvVar(allocator, "LOCALAPPDATA")) |local_app_data| {
                defer allocator.free(local_app_data);
                return std.fs.path.join(allocator, &.{ local_app_data, "roc", "packages" });
            }
        }

        // Fall back to ~/.cache/roc/packages (Unix)
        if (try self.getEnvVar(allocator, "HOME")) |home| {
            defer allocator.free(home);
            return std.fs.path.join(allocator, &.{ home, ".cache", "roc", "packages" });
        }

        return error.NoCacheDir;
    }

    fn logicalModuleToPath(self: *BuildEnv, root_dir: []const u8, logical_name: []const u8) (Allocator.Error || error{PathOutsideWorkspace})![]const u8 {
        var parts = std.mem.splitScalar(u8, logical_name, '/');
        var segs = std.ArrayList([]const u8).empty;
        defer segs.deinit(self.gpa);

        try segs.append(self.gpa, root_dir);
        while (parts.next()) |p| {
            if (p.len == 0) continue;
            try segs.append(self.gpa, p);
        }

        const joined = try std.fs.path.join(self.gpa, segs.items);
        defer self.gpa.free(joined);

        const with_ext = try std.fmt.allocPrint(self.gpa, "{s}.roc", .{joined});
        defer self.gpa.free(with_ext);

        // Canonicalize and sandbox
        const canon = try std.fs.path.resolve(self.gpa, &.{with_ext});
        // Enforce sandbox for dotted resolution: must be within the current package root (first workspace root).
        // If multiple roots are registered, we still require the resolved path to match at least one.
        if (!PathUtils.isWithinRoot(canon, self.workspace_roots.items)) {
            self.gpa.free(canon);
            return error.PathOutsideWorkspace;
        }
        return canon;
    }

    fn ensurePackage(
        self: *BuildEnv,
        name: []const u8,
        kind: PackageKind,
        root_file_abs: []const u8,
        root_file_state: ?watch_inputs.State,
        url: ?package_source.UrlSourceView,
    ) (Allocator.Error || error{PathOutsideWorkspace})!void {
        if (self.packages.getPtr(name)) |pkg| {
            if (pkg.url == null) {
                if (url) |url_view| {
                    pkg.url = try package_source.UrlSource.init(self.gpa, url_view);
                }
            }
            return;
        }

        const dir_raw = std.fs.path.dirname(root_file_abs) orelse ".";
        const dir = try std.fs.path.resolve(self.gpa, &.{dir_raw});
        const name_owned = try self.gpa.dupe(u8, name);
        const key_owned = try self.gpa.dupe(u8, name);
        const file_owned = try std.fs.path.resolve(self.gpa, &.{root_file_abs});
        var package_inserted = false;
        errdefer if (!package_inserted) {
            self.gpa.free(dir);
            self.gpa.free(name_owned);
            self.gpa.free(key_owned);
            self.gpa.free(file_owned);
        };
        // Package roots must be inside one of the explicitly discovered workspace roots.
        if (!PathUtils.isWithinRoot(file_owned, self.workspace_roots.items)) {
            return error.PathOutsideWorkspace;
        }

        var package_url = if (url) |url_view| try package_source.UrlSource.init(self.gpa, url_view) else null;
        errdefer if (package_url) |*url_source| url_source.deinit(self.gpa);

        try self.packages.put(self.gpa, key_owned, .{
            .name = name_owned,
            .kind = kind,
            .root_file = file_owned,
            .root_file_state = root_file_state,
            .root_dir = dir,
            .url = package_url,
        });
        package_inserted = true;
        package_url = null;
    }

    fn putPackageShorthand(self: *BuildEnv, pack: *Package, alias: []const u8, target_name: []const u8, root_file: []const u8) Allocator.Error![]const u8 {
        const key = try self.gpa.dupe(u8, alias);
        errdefer self.gpa.free(key);

        const name = try self.gpa.dupe(u8, target_name);
        errdefer self.gpa.free(name);

        const root_file_owned = try self.gpa.dupe(u8, root_file);
        errdefer self.gpa.free(root_file_owned);

        if (pack.shorthands.fetchRemove(key)) |old_entry| {
            freeConstSlice(self.gpa, old_entry.key);
            freeConstSlice(self.gpa, old_entry.value.name);
            freeConstSlice(self.gpa, old_entry.value.root_file);
        }
        try pack.shorthands.put(self.gpa, key, .{
            .name = name,
            .root_file = root_file_owned,
        });

        return name;
    }

    /// Run global package version resolution from the discovered root, then
    /// materialize the resolved graph: create a BuildEnv package per resolved
    /// package (named by its unique identity - full URL or absolute path),
    /// wire every package's shorthand aliases to the packages its specs
    /// resolved to, and transfer platform metadata.
    fn resolveAndMaterialize(
        self: *BuildEnv,
        root_pkg_name: []const u8,
        scanned_root: package_resolution.FetchedPackage,
    ) BuildError!void {
        // Without a cache directory, resolution still works for graphs with
        // no URL dependencies; URL specs report a download failure.
        if (self.package_cache_dir == null) {
            self.package_cache_dir = self.getRocCacheDir(self.gpa) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.BuiltinLowLevelAnnotationMustBeFunction,
                error.DownloadFailed,
                error.ExpectedPlatformString,
                error.ExpectedString,
                error.FileError,
                error.FileNotFound,
                error.Internal,
                error.InvalidDependency,
                error.InvalidNullByteInPath,
                error.InvalidUrl,
                error.IoError,
                error.LockedMemoryLimitExceeded,
                error.LowLevelOperationsNotFound,
                error.MissingFilesDirectory,
                error.MissingTargetFile,
                error.NoCacheDir,
                error.NoPackageSource,
                error.PathOutsideWorkspace,
                error.StreamTooLong,
                error.SystemResources,
                error.ThreadQuotaExceeded,
                error.Unexpected,
                error.UnsupportedBuiltinAnnotationOnly,
                error.UnsupportedHeader,
                => null,
            };
        }

        var ctx_fetcher = package_resolution.CtxFetcher{
            .fs = self.filesystem,
            .gpa = self.gpa,
            .cache_packages_dir = self.package_cache_dir,
            .compiler_owned_source_dir = self.compiler_owned_source_dir,
        };
        var resolver = package_resolution.Resolver.init(self.gpa, ctx_fetcher.fetcher(), self.resolution_config);
        defer resolver.deinit();
        if (self.root_url) |*root_url| resolver.setRootUrl(root_url.url);

        var resolved = resolver.resolveScannedRoot(scanned_root) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ResolutionFailed => {
                for (resolver.diagnostics.items) |diagnostic| {
                    try self.emitWorkspaceReport(diagnostic.title, diagnostic.message);
                }
                // Build the sink order so the reports above are drainable:
                // the build aborts here, so nothing else will order them.
                try self.sink.buildOrder(&[_][]const u8{"workspace"}, &[_][]const u8{"root"}, &[_]u32{0});
                self.sink.tryEmit();
                return error.InvalidDependency;
            },
        };
        defer resolved.deinit();

        try self.materializeResolved(root_pkg_name, &resolved);
    }

    fn materializeResolved(self: *BuildEnv, root_pkg_name: []const u8, resolved: *const package_resolution.Resolved) BuildError!void {
        const resolved_packages = resolved.packages;

        // Workspace roots must include every resolved package directory
        // before the sandbox checks in ensurePackage run.
        for (resolved_packages[1..]) |package| {
            try self.addWorkspaceRoot(package.root_dir);
        }

        var package_keys = try package_identity.buildPackageKeys(self.gpa, self.filesystem, resolved, .{
            .synthetic_root = self.synthetic_root_identity,
            .synthetic_platform = self.synthetic_root_platform_identity,
        });
        defer package_keys.deinit();
        if (!std.mem.eql(u8, package_keys.identity(package_resolution.Resolved.root_index), root_pkg_name)) {
            return error.Internal;
        }

        for (resolved_packages[1..], 1..) |package, i| {
            const kind: PackageKind = switch (package.kind) {
                .app => .app,
                .package => .package,
                .platform => .platform,
                .module => .module,
            };
            const url_view: ?package_source.UrlSourceView = if (package.url) |url| .{
                .url = url.url,
                .url_id = url.url_id,
            } else null;
            const root_file_state: ?watch_inputs.State = if (self.track_watch_inputs)
                if (url_view == null)
                    try self.currentFileWatchState(package.root_file)
                else
                    .{ .hash = package.root_source_hash }
            else
                null;
            try self.ensurePackage(package_keys.identity(i), kind, package.root_file, root_file_state, url_view);
        }

        // Wire every package's shorthand aliases.
        for (resolved_packages, 0..) |package, i| {
            for (package.deps) |dep| {
                const pack = self.packages.getPtr(package_keys.identity(i)) orelse return error.Internal;
                _ = try self.putPackageShorthand(pack, dep.alias, package_keys.identity(dep.target), resolved_packages[dep.target].root_file);
            }
        }

        // Transfer each dependency package's public-name-to-source-target map.
        // Platform headers additionally provide target metadata and expose
        // modules directly to their root application.
        for (resolved_packages, 0..) |package, i| {
            if (i == package_resolution.Resolved.root_index) continue;
            if (package.kind != .package and package.kind != .platform) continue;

            var child_info = try self.parseHeaderDeps(package.root_file);
            defer child_info.deinit(self.gpa);

            if (self.packages.getPtr(package_keys.identity(i))) |child_pkg| {
                if (package.kind == .platform and child_pkg.provides_entries.items.len == 0) {
                    child_pkg.provides_entries = child_info.provides_entries;
                    child_info.provides_entries = .empty; // Prevent double-free in deinit
                }
                if (package.kind == .platform and child_pkg.targets_config == null) {
                    child_pkg.targets_config = child_info.targets_config;
                    child_info.targets_config = null; // Prevent double-free in deinit
                }
            }

            if (package.kind == .platform) {
                // Find the root's platform alias for this package (if any).
                for (resolved_packages[package_resolution.Resolved.root_index].deps) |root_dep| {
                    if (!root_dep.is_platform) continue;
                    if (root_dep.target != i) continue;
                    try self.registerPlatformExposes(root_pkg_name, root_dep.alias, package.root_dir, &child_info);
                }
            }

            if (self.packages.getPtr(package_keys.identity(i))) |child_pkg| {
                self.moveHeaderPublicSurfaceToPackage(child_pkg, &child_info);
            }
        }

        // Record notes for packages whose declared dependency versions were
        // bumped by solving, so errors inside them can explain the bump.
        const bump_notes = try package_resolution.versionBumpNotes(resolved, package_keys.identities, self.gpa);
        defer self.gpa.free(bump_notes);
        for (bump_notes) |note| {
            const gop = try self.version_notes.getOrPut(self.gpa, note.package_identity);
            if (gop.found_existing) {
                // One note per package is enough; keep the first.
                self.gpa.free(@constCast(note.package_identity));
                self.gpa.free(@constCast(note.message));
            } else {
                gop.value_ptr.* = note.message;
            }
        }
    }

    /// Register a platform's exposed modules (e.g. Stdout, Stderr) as
    /// importable modules on the root app. This is necessary for URL
    /// platforms, whose directories live in the cache.
    fn registerPlatformExposes(
        self: *BuildEnv,
        root_pkg_name: []const u8,
        _: []const u8,
        platform_dir: []const u8,
        child_info: *const HeaderInfo,
    ) BuildError!void {
        for (child_info.public_surface.modules.items) |public_module| {
            const module_name = public_module.name;
            const module_path = try self.logicalModuleToPath(platform_dir, public_module.target);
            defer self.gpa.free(module_path);

            // Register this module as a package
            // Only allocate if package doesn't exist (ensurePackage makes its own copy)
            if (!self.packages.contains(module_name)) {
                const root_file_state: ?watch_inputs.State = if (self.track_watch_inputs)
                    try self.currentFileWatchState(module_path)
                else
                    null;
                try self.ensurePackage(module_name, .module, module_path, root_file_state, null);
            }

            const pack = self.packages.getPtr(root_pkg_name) orelse return error.Internal;

            // Also add to app's shorthands so imports resolve correctly
            _ = try self.putPackageShorthand(pack, module_name, module_name, module_path);
        }
    }

    /// Move Coordinator reports into the deterministic output sink while
    /// leaving all semantic ownership in the Coordinator.
    fn emitCoordinatorReports(self: *BuildEnv) Allocator.Error!void {
        const coord = self.coordinator orelse return;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                for (mod.reports.items) |report| {
                    try self.sink.emitReport(pkg.name, mod.name, report);
                }
                mod.reports.clearRetainingCapacity();
            }
        }
    }

    /// Move accumulated Coordinator reports into the ordered sink so callers
    /// can still drain and render them after a hard compilation error.
    /// Emission problems are ignored here - the original error propagates.
    fn emitAccumulatedReportsForError(self: *BuildEnv) void {
        self.emitCoordinatorReports() catch return;
        self.emitDeterministic() catch return;
    }

    fn emitWorkspaceReport(self: *BuildEnv, title: []const u8, msg: []const u8) Allocator.Error!void {
        const rep = try Report.init(self.gpa, title, msg, .runtime_error);
        // Route through OrderedSink with a stable fully-qualified identity so it participates in ordering.
        // We use "workspace:root" as the fq module identity.
        try self.sink.emitReport("workspace", "root", rep);
    }

    fn makeWorkspaceReportsDrainable(self: *BuildEnv) Allocator.Error!void {
        try self.sink.buildOrder(&[_][]const u8{"workspace"}, &[_][]const u8{"root"}, &[_]u32{0});
        self.sink.tryEmit();
    }

    fn validateDiscoveredPlatformTargetFiles(
        self: *BuildEnv,
        platform_source_path: []const u8,
        platform_dir: []const u8,
        maybe_targets_config: ?targets_config_mod.TargetsConfig,
    ) BuildError!void {
        const targets_config = maybe_targets_config orelse return;
        const validation = try targets_config.validateDeclaredTargetFilesExist(self.gpa, self.filesystem, platform_dir, self.target);
        defer validation.deinit(self.gpa);

        if (!validation.hasErrors()) return;

        for (validation.issues) |issue| {
            switch (issue) {
                .missing_inputs_directory => |info| try self.emitMissingTargetInputsDirectoryReport(platform_source_path, info),
                .missing_target_file => |info| try self.emitMissingTargetFileReport(platform_source_path, info),
            }
        }

        try self.makeWorkspaceReportsDrainable();
        if (validation.hasMissingInputsDirectory()) {
            return error.MissingFilesDirectory;
        }
        return error.MissingTargetFile;
    }

    fn validateDiscoveredPlatformTargetFilesForCurrentTarget(self: *BuildEnv) BuildError!void {
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr;
            if (pkg.kind != .platform) continue;
            try self.validateDiscoveredPlatformTargetFiles(pkg.root_file, pkg.root_dir, pkg.targets_config);
        }
    }

    fn emitMissingTargetInputsDirectoryReport(
        self: *BuildEnv,
        platform_source_path: []const u8,
        info: anytype,
    ) Allocator.Error!void {
        const headline = try std.fmt.allocPrint(
            self.gpa,
            "The platform targets configuration uses inputs directory `{s}`, but that directory does not exist.",
            .{info.inputs_dir},
        );
        defer self.gpa.free(headline);

        var report = try Report.init(self.gpa, "Missing Files Directory", headline, .runtime_error);
        try report.document.addText("Platform: ");
        try report.document.addAnnotated(platform_source_path, .path);
        try report.document.addLineBreak();
        try report.document.addText("Expected directory: ");
        try report.document.addAnnotated(info.expected_path, .path);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("Create this directory or update the platform `targets` section. Builds cannot proceed until declared target inputs exist.");

        try self.sink.emitReport("workspace", "root", report);
    }

    fn emitMissingTargetFileReport(
        self: *BuildEnv,
        platform_source_path: []const u8,
        info: anytype,
    ) Allocator.Error!void {
        const headline = try std.fmt.allocPrint(
            self.gpa,
            "The platform targets configuration declares `{s}` for {s}, but that file does not exist.",
            .{ info.file_path, @tagName(info.target) },
        );
        defer self.gpa.free(headline);

        var report = try Report.init(self.gpa, "Missing Target File", headline, .runtime_error);
        try report.document.addText("Platform: ");
        try report.document.addAnnotated(platform_source_path, .path);
        try report.document.addLineBreak();
        try report.document.addText("Target output: ");
        try report.document.addAnnotated(@tagName(info.output), .emphasized);
        try report.document.addLineBreak();
        try report.document.addText("Expected file: ");
        try report.document.addAnnotated(info.expected_full_path, .path);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("The selected target's platform entry lists this file. Add the file or remove it from that target config before building.");

        try self.sink.emitReport("workspace", "root", report);
    }

    fn resolvePlatformTargetConfigConstants(self: *BuildEnv) Allocator.Error!void {
        const semantic = self.getPlatformSemanticData() orelse return;
        const checked_module = semantic.checked_artifact orelse return;

        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (entry.value_ptr.kind != .platform) continue;
            const targets_config = entry.value_ptr.targets_config orelse continue;
            var diagnostic: targets_config_mod.TargetConfigResolveDiagnostic = undefined;
            targets_config.resolveCheckedConstants(self.gpa, checked_module, &diagnostic) catch |err| switch (err) {
                error.TargetConfigInvalid => {
                    try self.emitTargetConfigResolveError(diagnostic);
                    return;
                },
            };
        }
    }

    fn emitTargetConfigResolveError(
        self: *BuildEnv,
        diagnostic: targets_config_mod.TargetConfigResolveDiagnostic,
    ) Allocator.Error!void {
        const msg = try std.fmt.allocPrint(
            self.gpa,
            "The target configuration field `{s}` for {s}.{s} uses identifier `{s}`, but it {s}.",
            .{
                diagnostic.field_name,
                @tagName(diagnostic.target),
                @tagName(diagnostic.output),
                diagnostic.ident_name,
                diagnostic.reason.message(),
            },
        );
        defer self.gpa.free(msg);
        try self.emitWorkspaceReport("Invalid Target Configuration", msg);
    }

    // Compute global deterministic emission of accumulated reports:
    // sort by (min dependency depth from root app, then package and module names).
    fn emitDeterministic(self: *BuildEnv) Allocator.Error!void {
        // Build arrays of package names, module names, and depths
        var pkg_names = std.ArrayList([]const u8).empty;
        defer pkg_names.deinit(self.gpa);
        var module_names = std.ArrayList([]const u8).empty;
        defer module_names.deinit(self.gpa);
        var depths = std.ArrayList(u32).empty;
        defer depths.deinit(self.gpa);

        const coord = self.coordinator orelse return;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                try pkg_names.append(self.gpa, pkg.name);
                try module_names.append(self.gpa, mod.name);
                try depths.append(self.gpa, mod.depth);
            }
        }

        // Build the deterministic order
        try self.sink.buildOrder(pkg_names.items, module_names.items, depths.items);

        // Now that order is built, mark ready reports as emitted so they can be drained
        self.sink.lock.lockUncancelable(self.filesystem.std_io);
        defer self.sink.lock.unlock(self.filesystem.std_io);
        // Mark entries without reports as emitted BEFORE calling tryEmitLocked
        // so they don't block other entries from being emitted.
        for (self.sink.entries.items) |*e| {
            if (e.reports.items.len == 0) {
                e.ready = true;
                e.emitted = true;
            }
        }
        self.sink.tryEmitLocked();
    }

    fn moduleToPath(self: *BuildEnv, pkg_name: []const u8, module_name: []const u8) (Allocator.Error || error{ InvalidPackageName, PathOutsideWorkspace })![]const u8 {
        if (self.packages.get(pkg_name)) |pkg| {
            return try self.logicalModuleToPath(pkg.root_dir, module_name);
        }
        return error.InvalidPackageName;
    }

    fn syntheticRootDisplayPath(self: *BuildEnv, path: []const u8) ?[]const u8 {
        const original_path = self.synthetic_root_original_path orelse return null;
        const root_path = self.discovered_root_abs orelse return null;
        if (!std.mem.eql(u8, path, root_path)) return null;
        return original_path;
    }

    fn remapSyntheticRootSourceRegion(
        self: *BuildEnv,
        allocator: Allocator,
        region: *reporting.SourceCodeDisplayRegion,
        original_line_starts: []const u32,
    ) Allocator.Error!void {
        const original_path = self.synthetic_root_original_path orelse return;
        const original_source = self.synthetic_root_original_source orelse return;
        const filename = region.filename orelse return;
        if (self.syntheticRootDisplayPath(filename) == null) return;

        const header_lines = self.synthetic_root_header_lines;
        if (region.start_line <= header_lines or region.end_line <= header_lines) return;

        const original_start_line = region.start_line - header_lines;
        const original_end_line = region.end_line - header_lines;
        // A region past the last line of the user's source points at wiring
        // the staging appended, which is not the user's code; that region
        // keeps its own text and only its file name maps back.
        if (original_line_starts.len == 0 or original_end_line > original_line_starts.len) {
            const owned_filename = try allocator.dupe(u8, original_path);
            if (region.filename) |old_filename| allocator.free(old_filename);
            region.filename = owned_filename;
            return;
        }

        const region_info = base.RegionInfo{
            .start_line_idx = original_start_line - 1,
            .start_col_idx = region.start_column - 1,
            .end_line_idx = original_end_line - 1,
            .end_col_idx = region.end_column - 1,
        };

        const line_text = try allocator.dupe(u8, region_info.calculateLineText(original_source, original_line_starts));
        errdefer allocator.free(line_text);
        const owned_filename = try allocator.dupe(u8, original_path);
        errdefer allocator.free(owned_filename);

        allocator.free(region.line_text);
        if (region.filename) |old_filename| allocator.free(old_filename);

        region.line_text = line_text;
        region.filename = owned_filename;
        region.start_line = original_start_line;
        region.end_line = original_end_line;
    }

    fn remapSyntheticRootDocumentElement(
        self: *BuildEnv,
        allocator: Allocator,
        element: *reporting.DocumentElement,
        original_line_starts: []const u32,
    ) Allocator.Error!void {
        switch (element.*) {
            .source_code_region => |*region| try self.remapSyntheticRootSourceRegion(allocator, region, original_line_starts),
            .source_code_with_underlines => |*underlines| {
                const old_start_line = underlines.display_region.start_line;
                try self.remapSyntheticRootSourceRegion(allocator, &underlines.display_region, original_line_starts);
                const header_lines = self.synthetic_root_header_lines;
                if (header_lines == 0 or old_start_line <= header_lines) return;
                for (underlines.underline_regions) |*underline| {
                    if (underline.start_line > header_lines) {
                        underline.start_line -= header_lines;
                    }
                    if (underline.end_line > header_lines) {
                        underline.end_line -= header_lines;
                    }
                }
            },
            .source_location => |*location| {
                const original_path = self.synthetic_root_original_path orelse return;
                const filename = location.filename orelse return;
                if (self.syntheticRootDisplayPath(filename) == null) return;

                const header_lines = self.synthetic_root_header_lines;
                if (header_lines > 0 and location.line <= header_lines) return;

                const owned_filename = try allocator.dupe(u8, original_path);
                if (location.filename) |old_filename| allocator.free(old_filename);
                location.filename = owned_filename;
                if (location.line > header_lines) location.line -= header_lines;
            },
            .text,
            .annotated,
            .line_break,
            .indent,
            .space,
            .horizontal_rule,
            .annotation_start,
            .annotation_end,
            .raw,
            .reflowing_text,
            .link,
            .vertical_stack,
            .horizontal_concat,
            .source_code_multi_region,
            => {},
        }
    }

    fn remapSyntheticRootReport(self: *BuildEnv, report: *Report) Allocator.Error!void {
        if (self.synthetic_root_original_path == null or self.synthetic_root_original_source == null) return;

        var original_line_starts = try base.RegionInfo.findLineStarts(self.gpa, self.synthetic_root_original_source.?);
        defer original_line_starts.deinit(self.gpa);
        for (report.document.elements.items) |*element| {
            try self.remapSyntheticRootDocumentElement(
                report.document.allocator,
                element,
                original_line_starts.items.items,
            );
        }
    }

    pub const DrainedModuleReports = struct {
        abs_path: []const u8,
        reports: []Report,
    };

    pub fn drainReports(self: *BuildEnv) Allocator.Error![]DrainedModuleReports {
        const drained = try self.sink.drainEmitted(self.gpa);
        defer self.gpa.free(drained);

        var noted_pkgs = std.StringHashMapUnmanaged(void){};
        defer noted_pkgs.deinit(self.gpa);

        var out = try self.gpa.alloc(DrainedModuleReports, drained.len);
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            var path = self.moduleToPath(drained[i].pkg_name, drained[i].module_name) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidPackageName, error.PathOutsideWorkspace => try self.gpa.dupe(u8, ""),
            };

            if (self.syntheticRootDisplayPath(path)) |display_path| {
                self.gpa.free(path);
                path = try self.gpa.dupe(u8, display_path);
            }

            // When a package that was compiled against a bumped dependency
            // version has errors, attach its version note to the first error
            // (once per package) so the reader knows the bump may be the cause.
            if (self.version_notes.get(drained[i].pkg_name)) |note| {
                if (!noted_pkgs.contains(drained[i].pkg_name)) {
                    for (drained[i].reports) |*report| {
                        if (report.severity != .runtime_error and report.severity != .fatal) continue;
                        try noted_pkgs.put(self.gpa, drained[i].pkg_name, {});
                        const owned = try report.addOwnedString(note);
                        try report.addNote(owned);
                        break;
                    }
                }
            }

            for (drained[i].reports) |*report| {
                try self.remapSyntheticRootReport(report);
            }

            out[i] = .{
                .abs_path = path,
                .reports = drained[i].reports,
            };
        }
        return out;
    }

    /// Free memory allocated by drainReports.
    /// This frees the abs_path strings, deinits each report, frees the reports slices, and frees the outer slice.
    /// Safe to call with empty slices from catch handlers.
    pub fn freeDrainedReports(self: *BuildEnv, drained: []const DrainedModuleReports) void {
        // Skip if this is an empty slice (could be compile-time constant from catch handler)
        if (drained.len == 0) return;
        for (drained) |mod| {
            self.gpa.free(mod.abs_path);
            // Deinit each report and free the reports slice
            for (mod.reports) |*report| {
                @constCast(report).deinit();
            }
            self.gpa.free(mod.reports);
        }
        // Cast to non-const for freeing (safe since we allocated this ourselves)
        self.gpa.free(@constCast(drained));
    }

    /// Free memory from drainReports when reports ownership is transferred elsewhere.
    /// Only frees abs_path strings and outer slice, NOT the reports (caller now owns them).
    pub fn freeDrainedReportsPathsOnly(self: *BuildEnv, drained: []const DrainedModuleReports) void {
        if (drained.len == 0) return;
        for (drained) |mod| {
            self.gpa.free(mod.abs_path);
        }
        self.gpa.free(@constCast(drained));
    }

    /// Get accumulated timing information from the active coordinator.
    pub fn getTimingInfo(self: *BuildEnv) ModuleTimingInfo {
        const coord = self.coordinator orelse return .{};
        return coord.getTimingInfo();
    }

    /// Build statistics collected during compilation
    pub const BuildStats = struct {
        /// Total modules processed (cached + compiled)
        modules_total: u32 = 0,
        /// Modules loaded from cache
        cache_hits: u32 = 0,
        /// Modules that needed compilation (cache misses)
        cache_misses: u32 = 0,

        /// Number of modules that were compiled (not cached)
        modules_compiled: u32 = 0,

        /// Module compile time tracking (for non-cached modules)
        /// Time is for full module compilation: parse -> canonicalize -> type-check
        module_time_min_ns: u64 = std.math.maxInt(u64),
        module_time_max_ns: u64 = 0,
        module_time_sum_ns: u64 = 0,

        /// Record a module's compilation time
        pub fn recordModuleTime(self: *BuildStats, time_ns: u64) void {
            self.modules_compiled += 1;
            self.module_time_sum_ns += time_ns;
            if (time_ns < self.module_time_min_ns) self.module_time_min_ns = time_ns;
            if (time_ns > self.module_time_max_ns) self.module_time_max_ns = time_ns;
        }

        /// Get average module compile time in nanoseconds
        pub fn moduleTimeAvgNs(self: BuildStats) u64 {
            if (self.modules_compiled == 0) return 0;
            return self.module_time_sum_ns / self.modules_compiled;
        }

        /// Get module time min in milliseconds (rounded)
        pub fn moduleTimeMinMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.module_time_min_ns + 500_000) / 1_000_000);
        }

        /// Get module time max in milliseconds (rounded)
        pub fn moduleTimeMaxMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.module_time_max_ns + 500_000) / 1_000_000);
        }

        /// Get module time average in milliseconds (rounded)
        pub fn moduleTimeAvgMs(self: BuildStats) u32 {
            if (self.modules_compiled == 0) return 0;
            return @intCast((self.moduleTimeAvgNs() + 500_000) / 1_000_000);
        }

        /// Get cache hit rate as percentage (0-100)
        pub fn cacheHitPercent(self: BuildStats) u32 {
            const total = self.cache_hits + self.cache_misses;
            if (total == 0) return 0;
            return @intCast((@as(u64, self.cache_hits) * 100 + total / 2) / total);
        }
    };

    /// Get build statistics from the coordinator
    pub fn getBuildStats(self: *BuildEnv) BuildStats {
        if (self.coordinator) |coord| {
            return coord.getBuildStats();
        }
        return .{};
    }

    pub fn freeWatchInputs(self: *BuildEnv, inputs: []const []const u8) void {
        for (inputs) |path| self.gpa.free(path);
        self.gpa.free(inputs);
    }

    pub fn freeWatchInputStates(self: *BuildEnv, inputs: []const watch_inputs.Input) void {
        watch_inputs.deinit(self.gpa, inputs);
    }

    fn appendWatchInput(
        self: *BuildEnv,
        paths: *std.ArrayList([]const u8),
        seen: *std.StringHashMapUnmanaged(void),
        path: []const u8,
    ) Allocator.Error!void {
        if (seen.contains(path)) return;
        const owned = try self.gpa.dupe(u8, path);
        errdefer self.gpa.free(owned);
        try paths.append(self.gpa, owned);
        errdefer _ = paths.pop();
        try seen.put(self.gpa, owned, {});
    }

    fn appendWatchInputState(
        self: *BuildEnv,
        inputs: *std.ArrayList(watch_inputs.Input),
        seen: *std.StringHashMapUnmanaged(void),
        path: []const u8,
        state: watch_inputs.State,
    ) Allocator.Error!void {
        if (seen.contains(path)) return;
        const owned = try self.gpa.dupe(u8, path);
        errdefer self.gpa.free(owned);
        try inputs.append(self.gpa, .{ .path = owned, .state = state });
        errdefer _ = inputs.pop();
        try seen.put(self.gpa, owned, {});
    }

    fn appendFileDependencyWatchInputs(
        self: *BuildEnv,
        paths: *std.ArrayList([]const u8),
        seen: *std.StringHashMapUnmanaged(void),
        source_dir: []const u8,
        env: *const ModuleEnv,
    ) Allocator.Error!void {
        for (env.file_dependencies.items.items) |dep| {
            const relative_path = env.fileDependencyRelativePath(dep);
            const full_path = try std.fs.path.resolve(self.gpa, &.{ source_dir, relative_path });
            defer self.gpa.free(full_path);
            try self.appendWatchInput(paths, seen, full_path);
        }
    }

    fn appendFileDependencyWatchInputStates(
        self: *BuildEnv,
        inputs: *std.ArrayList(watch_inputs.Input),
        seen: *std.StringHashMapUnmanaged(void),
        source_dir: []const u8,
        env: *const ModuleEnv,
    ) Allocator.Error!void {
        for (env.file_dependencies.items.items) |dep| {
            const relative_path = env.fileDependencyRelativePath(dep);
            const full_path = try std.fs.path.resolve(self.gpa, &.{ source_dir, relative_path });
            defer self.gpa.free(full_path);
            const state: watch_inputs.State = switch (dep.state) {
                .present => .{ .hash = dep.content_hash },
                .missing => .missing,
                .unreadable => .unreadable,
                .pending => unreachable,
            };
            try self.appendWatchInputState(inputs, seen, full_path, state);
        }
    }

    /// Collect exact filesystem inputs read by the current build. Returned paths
    /// are owned by the BuildEnv allocator and must be released with
    /// `freeWatchInputs`.
    pub fn collectWatchInputs(self: *BuildEnv) Allocator.Error![]const []const u8 {
        var paths = std.ArrayList([]const u8).empty;
        errdefer {
            for (paths.items) |path| self.gpa.free(path);
            paths.deinit(self.gpa);
        }
        var seen = std.StringHashMapUnmanaged(void){};
        defer seen.deinit(self.gpa);

        var build_pkg_it = self.packages.iterator();
        while (build_pkg_it.next()) |entry| {
            if (entry.value_ptr.url == null) try self.appendWatchInput(&paths, &seen, entry.value_ptr.root_file);
        }

        if (self.coordinator) |coord| {
            var pkg_it = coord.packages.iterator();
            while (pkg_it.next()) |entry| {
                const pkg = entry.value_ptr.*;
                if (pkg.url != null) continue;
                for (pkg.modules.items) |*mod| {
                    try self.appendWatchInput(&paths, &seen, mod.path);
                    const module_env = mod.moduleEnv() orelse continue;
                    try self.appendFileDependencyWatchInputs(&paths, &seen, mod.canonicalSourceDir(), module_env);
                }
            }
        }

        return paths.toOwnedSlice(self.gpa);
    }

    /// Collect exact filesystem inputs read by the current build, paired with
    /// the state consumed by compilation. Returned paths are owned by the
    /// BuildEnv allocator and must be released with `freeWatchInputStates`.
    pub fn collectWatchInputStates(self: *BuildEnv) Allocator.Error![]const watch_inputs.Input {
        if (!self.track_watch_inputs) {
            if (builtin.mode == .Debug) std.debug.panic("collectWatchInputStates called without watch input tracking enabled", .{});
            unreachable;
        }

        var inputs = std.ArrayList(watch_inputs.Input).empty;
        errdefer {
            for (inputs.items) |input| self.gpa.free(input.path);
            inputs.deinit(self.gpa);
        }
        var seen = std.StringHashMapUnmanaged(void){};
        defer seen.deinit(self.gpa);

        var build_pkg_it = self.packages.iterator();
        while (build_pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            if (pkg.url != null) continue;
            const state = pkg.root_file_state orelse {
                if (builtin.mode == .Debug) std.debug.panic("build package {s} has root_file without root_file_state", .{entry.key_ptr.*});
                unreachable;
            };
            try self.appendWatchInputState(&inputs, &seen, pkg.root_file, state);
        }

        if (self.coordinator) |coord| {
            var pkg_it = coord.packages.iterator();
            while (pkg_it.next()) |entry| {
                const pkg = entry.value_ptr.*;
                if (pkg.url != null) continue;
                for (pkg.modules.items) |*mod| {
                    const state = mod.source_file_state orelse {
                        if (builtin.mode == .Debug) std.debug.panic("coordinator module {s} has no source_file_state", .{mod.name});
                        unreachable;
                    };
                    try self.appendWatchInputState(&inputs, &seen, mod.path, state);
                    const module_env = mod.moduleEnv() orelse continue;
                    try self.appendFileDependencyWatchInputStates(&inputs, &seen, mod.canonicalSourceDir(), module_env);
                }
            }
        }

        return inputs.toOwnedSlice(self.gpa);
    }

    pub fn hasCompiledModules(self: *BuildEnv) bool {
        const coord = self.coordinator orelse return false;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (entry.value_ptr.*.modules.items.len > 0) return true;
        }
        return false;
    }

    fn isPathInDir(path: []const u8, dir: []const u8) bool {
        if (dir.len == 0) return false;
        if (!std.mem.startsWith(u8, path, dir)) return false;
        if (path.len == dir.len) return true;
        if (std.fs.path.isSep(path[dir.len])) return true;
        return std.fs.path.isSep(dir[dir.len - 1]);
    }

    pub fn findPackageForModulePath(self: *BuildEnv, path: []const u8) ?*coordinator_mod.PackageState {
        const coord = self.coordinator orelse return null;

        // 1. Pass 1: exact module or root_file membership across all packages
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                if (std.mem.eql(u8, mod.path, path)) return pkg;
            }
            if (pkg.root_file) |rf| {
                if (std.mem.eql(u8, rf, path)) return pkg;
            }
        }

        // 2. Pass 2: find the package with the longest matching root_dir that contains `path`
        // on a directory boundary. This deterministically selects the most specific package when
        // package root directories nest or overlap (e.g. app workspace root vs child package root).
        var best_pkg: ?*coordinator_mod.PackageState = null;
        var longest_dir_len: usize = 0;

        pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            if (pkg.root_dir.len > longest_dir_len and isPathInDir(path, pkg.root_dir)) {
                best_pkg = pkg;
                longest_dir_len = pkg.root_dir.len;
            }
        }

        return best_pkg;
    }

    pub fn findModuleByQualifiedNameInPackage(
        self: *BuildEnv,
        importing_pkg_opt: ?*coordinator_mod.PackageState,
        qualified_name: []const u8,
    ) ?*coordinator_mod.ModuleState {
        const coord = self.coordinator orelse return null;
        const importing_pkg = importing_pkg_opt orelse return null;

        if (std.mem.find(u8, qualified_name, ".")) |dot_pos| {
            const pkg_alias = qualified_name[0..dot_pos];
            const mod_name = qualified_name[dot_pos + 1 ..];

            // 1. If pkg_alias is defined in the importing package's shorthands,
            // resolve it strictly in the target package. If the target package or module
            // cannot be found, return null immediately without falling through to other namespaces.
            if (importing_pkg.shorthands.get(pkg_alias)) |target_pkg_name| {
                const target_pkg = coord.packages.get(target_pkg_name) orelse return null;
                if (target_pkg.getPublicModuleTarget(mod_name)) |public_target| {
                    if (target_pkg.getModuleId(public_target.logicalModule())) |id| {
                        return target_pkg.getModule(id);
                    }
                }
                if (target_pkg.getPublicModuleId(mod_name) orelse target_pkg.getModuleId(mod_name)) |id| {
                    return target_pkg.getModule(id);
                }
                return null;
            }

            // 2. If pkg_alias is not a package shorthand, the qualifier must be part of a
            // hierarchical module defined within the importing package itself (e.g. `Foo.Bar`).
            if (importing_pkg.getModuleId(qualified_name) orelse importing_pkg.getPublicModuleId(qualified_name)) |id| {
                return importing_pkg.getModule(id);
            }

            return null;
        }

        return self.findModuleByNameInPackage(importing_pkg_opt, qualified_name);
    }

    pub fn findModuleByQualifiedName(self: *BuildEnv, qualified_name: []const u8) ?*coordinator_mod.ModuleState {
        return self.findModuleByQualifiedNameInPackage(null, qualified_name);
    }

    pub fn findModuleByNameInPackage(
        _: *BuildEnv,
        importing_pkg_opt: ?*coordinator_mod.PackageState,
        module_name: []const u8,
    ) ?*coordinator_mod.ModuleState {
        const importing_pkg = importing_pkg_opt orelse return null;
        if (importing_pkg.getModuleId(module_name) orelse importing_pkg.getPublicModuleId(module_name)) |id| {
            return importing_pkg.getModule(id);
        }
        return null;
    }

    pub fn findModuleByName(self: *BuildEnv, module_name: []const u8) ?*coordinator_mod.ModuleState {
        return self.findModuleByNameInPackage(null, module_name);
    }

    pub fn findModuleByPath(self: *BuildEnv, path: []const u8) ?*coordinator_mod.ModuleState {
        const coord = self.coordinator orelse return null;
        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            for (pkg.modules.items) |*mod| {
                if (std.mem.eql(u8, mod.path, path)) return mod;
            }
        }
        return null;
    }

    pub fn rootModule(self: *BuildEnv, package_name: []const u8) ?*coordinator_mod.ModuleState {
        const coord = self.coordinator orelse return null;
        const pkg = coord.packages.get(package_name) orelse return null;
        const root_id = pkg.root_module_id orelse return null;
        return pkg.getModule(root_id);
    }

    /// How the build reached the package a module belongs to. `roc test` runs
    /// the `expect`s the developer wrote and skips the ones that arrived with
    /// a dependency, so the origin has to travel alongside each module.
    pub const PackageOrigin = enum {
        /// The package the `roc test` source argument explicitly selected,
        /// including a bundle URL or installed shorthand.
        root,
        /// A dependency the root reaches through filesystem paths alone.
        local_path_dependency,
        /// Downloaded from a URL, or reached only by way of a fetched package.
        fetched,
        /// Embedded in the compiler, such as the default platform.
        compiler_owned,
    };

    /// Information about a compiled module, ready for serialization.
    /// All pointers reference data owned by the BuildEnv/Coordinator.
    pub const CompiledModuleInfo = struct {
        /// Module name (e.g., "Main", "Stdout")
        name: []const u8,
        /// Source file path for reporting and CLI diagnostics.
        path: []const u8,
        /// Paired semantic data retained after type checking
        semantic: SemanticModuleData,
        /// Source code of the module
        source: []const u8,
        /// Package name this module belongs to
        package_name: []const u8,
        /// How the build reached this module's package.
        package_origin: PackageOrigin,
        /// True if this is the platform's main.roc
        is_platform_main: bool,
        /// True if this is the app module
        is_app: bool,
        /// True if this is a platform sibling module (e.g., Stdout, Stderr)
        is_platform_sibling: bool,
        /// Dependency depth from root
        depth: u32,
        /// Platform provides entries (only populated for platform main modules)
        provides_entries: []const ProvidesEntry = &.{},
    };

    /// Compact borrowed view used only by public docs and API extraction.
    /// General compiled-module consumers do not pay for this routing metadata.
    pub const PublicModuleInfo = struct {
        name: []const u8,
        path: []const u8,
        semantic: SemanticModuleData,
        package_name: []const u8,
        /// Root-owned names to retain when extracting documentation.
        docs_exposed_names: ?[]const []const u8 = null,
        /// Exact type declaration represented by this public module view.
        /// This is transient post-build routing data, not retained per CIR node.
        public_type_decl: ?CIR.Statement.Idx = null,
        /// Declaration order within the owning package's public surface.
        public_order: u32 = 0,
    };

    /// Get all compiled modules from the Coordinator (after build completes).
    /// Returns modules in arbitrary order - use getModulesInSerializationOrder() for sorted order.
    /// The alias the root package uses for `pkg_name`, if any. Packages are
    /// named internally by their unique identity (full URL or absolute path);
    /// user-facing output like docs should show the root's alias instead.
    pub fn rootAliasForPackage(self: *BuildEnv, pkg_name: []const u8) ?[]const u8 {
        const root_name = self.discovered_pkg_name orelse return null;
        const root_pkg = self.packages.getPtr(root_name) orelse return null;
        var it = root_pkg.shorthands.iterator();
        while (it.next()) |entry| {
            if (std.mem.eql(u8, entry.value_ptr.name, pkg_name)) {
                return entry.key_ptr.*;
            }
        }
        return null;
    }

    /// User-facing display name for `pkg_name`: the alias the root package
    /// uses for it when one exists, the root's role label ("app" or "module")
    /// for the root package itself, and otherwise the internal identity.
    /// Identity strings (full URLs, canonical paths) are cache and nominal
    /// keys, not presentation; docs and other user output go through this.
    pub fn displayNameForPackage(self: *BuildEnv, pkg_name: []const u8) []const u8 {
        if (self.rootAliasForPackage(pkg_name)) |alias| return alias;
        if (self.discovered_pkg_name) |root_name| {
            if (std.mem.eql(u8, root_name, pkg_name)) {
                if (self.packages.getPtr(root_name)) |root_pkg| {
                    return switch (root_pkg.kind) {
                        .app, .default_app => "app",
                        .package, .platform, .module, .hosted, .type_module => "module",
                    };
                }
            }
        }
        return pkg_name;
    }

    pub fn rootIsPackage(self: *const BuildEnv) bool {
        const root_name = self.discovered_pkg_name orelse return false;
        const root_pkg = self.packages.get(root_name) orelse return false;
        return root_pkg.kind == .package;
    }

    /// Whether a module belongs in a bundle of this build's root package.
    /// URL dependencies are excluded: consumers resolve them from the URLs
    /// in the header, and their files live in the local package cache. The
    /// cache-directory check also excludes path dependencies that live
    /// inside an extracted bundle.
    pub fn isBundleableModule(self: *BuildEnv, pkg_name: []const u8, module_path: []const u8) bool {
        // Compiler-owned platforms are embedded in every compiler; their
        // materialized sources live outside the bundle root and must never
        // be bundled.
        if (compiler_platforms.fromIdentity(pkg_name) != null) return false;
        if (self.packages.getPtr(pkg_name)) |pkg| {
            if (pkg.url != null) return false;
        }
        if (self.package_cache_dir) |cache_dir| {
            if (PathUtils.isWithinRoot(module_path, &.{cache_dir})) return false;
        }
        return true;
    }

    /// The explicitly requested root package and the dependencies it reaches
    /// through filesystem paths alone, keyed by package identity. Keys borrow
    /// the names stored in `self.packages`.
    ///
    /// The requested root is included even when its identity is a bundle URL.
    /// After that root, the walk stops at every downloaded package and every
    /// compiler-owned platform, because a path dependency declared inside a
    /// fetched dependency arrived by download too and must not inherit the
    /// root's ownership.
    fn collectTestOwnedPackages(
        self: *BuildEnv,
        allocator: Allocator,
    ) Allocator.Error!std.StringHashMapUnmanaged(void) {
        var reached: std.StringHashMapUnmanaged(void) = .{};
        errdefer reached.deinit(allocator);

        const root_name = self.discovered_pkg_name orelse return reached;
        if (compiler_platforms.fromIdentity(root_name) != null) return reached;

        var frontier: std.ArrayList([]const u8) = .empty;
        defer frontier.deinit(allocator);

        try reached.put(allocator, root_name, {});
        try frontier.append(allocator, root_name);

        var next: usize = 0;
        while (next < frontier.items.len) : (next += 1) {
            const pkg = self.packages.getPtr(frontier.items[next]) orelse continue;
            var shorthand_it = pkg.shorthands.iterator();
            while (shorthand_it.next()) |shorthand| {
                const target = shorthand.value_ptr.name;
                if (reached.contains(target)) continue;
                if (!self.isLocalPathPackage(target)) continue;
                const key = (self.packages.getKeyPtr(target) orelse continue).*;
                try reached.put(allocator, key, {});
                try frontier.append(allocator, key);
            }
        }

        return reached;
    }

    /// Whether this package's own source is a filesystem path the developer
    /// could edit, ignoring how the build arrived at it.
    fn isLocalPathPackage(self: *BuildEnv, name: []const u8) bool {
        if (compiler_platforms.fromIdentity(name) != null) return false;
        const pkg = self.packages.getPtr(name) orelse return false;
        return pkg.url == null;
    }

    pub fn getCompiledModules(self: *BuildEnv, allocator: Allocator) Allocator.Error![]CompiledModuleInfo {
        const coord = self.coordinator orelse {
            std.debug.panic("build env invariant violated: compiled modules requested before coordinator initialization", .{});
        };

        var modules = std.ArrayList(CompiledModuleInfo).empty;
        errdefer modules.deinit(allocator);

        var test_owned_packages = try self.collectTestOwnedPackages(allocator);
        defer test_owned_packages.deinit(allocator);
        const root_name = self.discovered_pkg_name;

        var pkg_it = coord.packages.iterator();
        while (pkg_it.next()) |entry| {
            const pkg_name = entry.key_ptr.*;
            const coord_pkg = entry.value_ptr.*;

            // Determine package kind
            const pkg_ptr = self.packages.getPtr(pkg_name);
            const is_platform_pkg = pkg_ptr != null and pkg_ptr.?.kind == .platform;
            const is_app_pkg = pkg_ptr != null and (pkg_ptr.?.kind == .app or pkg_ptr.?.kind == .default_app);
            const package_origin: PackageOrigin = if (compiler_platforms.fromIdentity(pkg_name) != null)
                .compiler_owned
            else if (root_name != null and std.mem.eql(u8, pkg_name, root_name.?))
                .root
            else if (test_owned_packages.contains(pkg_name))
                .local_path_dependency
            else
                .fetched;

            for (coord_pkg.modules.items, 0..) |*mod, mod_idx| {
                const semantic = mod.semanticData() orelse continue;
                const is_root = if (coord_pkg.root_module_id) |root_id| @as(usize, root_id) == mod_idx else false;
                const is_platform_main = is_platform_pkg and is_root;
                const is_platform_sibling = is_platform_pkg and !is_root;
                const is_app = is_app_pkg and is_root;

                try modules.append(allocator, .{
                    .name = mod.name,
                    .path = mod.path,
                    .semantic = semantic,
                    .source = semantic.env.common.source,
                    .package_name = pkg_name,
                    .package_origin = package_origin,
                    .is_platform_main = is_platform_main,
                    .is_app = is_app,
                    .is_platform_sibling = is_platform_sibling,
                    .depth = mod.depth,
                    .provides_entries = if (is_platform_main or is_app)
                        if (pkg_ptr) |p| p.provides_entries.items else &.{}
                    else
                        &.{},
                });
            }
        }

        return modules.toOwnedSlice(allocator);
    }

    /// Resolve the root package or platform's explicit public module list to
    /// completed compiler outputs. Dependency modules are deliberately absent.
    pub fn getPublicRootModules(self: *BuildEnv, allocator: Allocator) Allocator.Error![]PublicModuleInfo {
        const root_name = self.discovered_pkg_name orelse {
            std.debug.panic("build env invariant violated: public modules requested before dependency discovery", .{});
        };
        const root_pkg = self.packages.getPtr(root_name) orelse {
            std.debug.panic("build env invariant violated: public-module root package is unavailable", .{});
        };
        if (root_pkg.kind != .package and root_pkg.kind != .platform) {
            std.debug.panic("build env invariant violated: public modules requested for a non-package root", .{});
        }
        const coord = self.coordinator orelse {
            std.debug.panic("build env invariant violated: public modules requested before coordinator initialization", .{});
        };
        const root_coord_pkg = coord.packages.get(root_name) orelse {
            std.debug.panic("build env invariant violated: public-module root coordinator package is unavailable", .{});
        };

        var public_modules = std.ArrayList(PublicModuleInfo).empty;
        errdefer public_modules.deinit(allocator);

        for (root_pkg.public_surface.modules.items, 0..) |public_module, public_index| {
            const module_name = public_module.name;
            const public_target = root_coord_pkg.getPublicModuleTarget(module_name) orelse {
                std.debug.panic(
                    "build env invariant violated: public module '{s}' has no source target",
                    .{module_name},
                );
            };
            const module_id = root_coord_pkg.getPublicModuleId(module_name) orelse {
                std.debug.panic(
                    "build env invariant violated: public module '{s}' was not compiled",
                    .{module_name},
                );
            };
            const module_state = root_coord_pkg.getModule(module_id).?;
            const module_data = module_state.semanticData() orelse {
                std.debug.panic(
                    "build env invariant violated: public module '{s}' has no completed compiler output",
                    .{module_name},
                );
            };
            try public_modules.append(allocator, .{
                .name = module_name,
                .path = module_state.path,
                .semantic = module_data,
                .package_name = root_name,
                .public_order = @intCast(public_index),
                .public_type_decl = switch (public_target.selection) {
                    .type_decl => |statement| statement,
                    .whole_module => null,
                    .unresolved_nested_type => std.debug.panic(
                        "build env invariant violated: public module '{s}' has an unresolved nested type",
                        .{module_name},
                    ),
                },
            });
        }

        return public_modules.toOwnedSlice(allocator);
    }

    /// Return every compiled public module view in the resolved package graph.
    /// Consumers use the public spelling here instead of exposing private
    /// source-module names; type views additionally carry their exact root.
    pub fn getCompiledPublicModules(self: *BuildEnv, allocator: Allocator) Allocator.Error![]PublicModuleInfo {
        const coord = self.coordinator orelse {
            std.debug.panic("build env invariant violated: public modules requested before coordinator initialization", .{});
        };

        var public_modules = std.ArrayList(PublicModuleInfo).empty;
        errdefer public_modules.deinit(allocator);

        var packages = self.packages.iterator();
        while (packages.next()) |package_entry| {
            const package_name = package_entry.key_ptr.*;
            const package = package_entry.value_ptr;
            const coord_package = coord.packages.get(package_name) orelse continue;

            for (package.public_surface.modules.items, 0..) |public_module, public_index| {
                const module_id = coord_package.getPublicModuleId(public_module.name) orelse continue;
                const module_state = coord_package.getModule(module_id) orelse continue;
                const semantic = module_state.semanticData() orelse continue;
                const public_target = coord_package.getPublicModuleTarget(public_module.name) orelse unreachable;
                const source_decl: ?CIR.Statement.Idx = switch (public_target.selection) {
                    .type_decl => |statement| statement,
                    .whole_module => null,
                    .unresolved_nested_type => std.debug.panic(
                        "build env invariant violated: compiled public module '{s}' has an unresolved nested type",
                        .{public_module.name},
                    ),
                };
                try public_modules.append(allocator, .{
                    .name = public_module.name,
                    .path = module_state.path,
                    .semantic = semantic,
                    .package_name = package_name,
                    .public_type_decl = source_decl,
                    .public_order = @intCast(public_index),
                });
            }
        }

        return public_modules.toOwnedSlice(allocator);
    }

    /// Return the compiled modules that belong in generated documentation.
    /// Package roots use their public modules. Platform roots additionally use
    /// their root module when the header exposes root-owned names.
    pub fn getDocumentationModules(self: *BuildEnv, allocator: Allocator) Allocator.Error![]PublicModuleInfo {
        const root_name = self.discovered_pkg_name orelse {
            std.debug.panic("build env invariant violated: documentation requested before dependency discovery", .{});
        };
        const root_pkg = self.packages.getPtr(root_name) orelse {
            std.debug.panic("build env invariant violated: documentation root package is unavailable", .{});
        };

        if (root_pkg.kind == .package) {
            return self.getPublicRootModules(allocator);
        }

        if (root_pkg.kind == .platform) {
            const public_modules = try self.getPublicRootModules(allocator);
            defer allocator.free(public_modules);

            var docs_modules = try std.ArrayList(PublicModuleInfo).initCapacity(
                allocator,
                public_modules.len + @intFromBool(root_pkg.public_surface.root_names.items.len > 0),
            );
            errdefer docs_modules.deinit(allocator);
            docs_modules.appendSliceAssumeCapacity(public_modules);

            if (root_pkg.public_surface.root_names.items.len > 0) {
                const root_module = self.rootModule(root_name) orelse {
                    std.debug.panic("build env invariant violated: platform documentation root module is unavailable", .{});
                };
                const root_data = root_module.semanticData() orelse {
                    std.debug.panic("build env invariant violated: platform documentation root module has no completed compiler output", .{});
                };
                docs_modules.appendAssumeCapacity(.{
                    .name = root_module.name,
                    .path = root_module.path,
                    .semantic = root_data,
                    .package_name = root_name,
                    .docs_exposed_names = root_pkg.public_surface.root_names.items,
                });
            }

            return docs_modules.toOwnedSlice(allocator);
        }

        const all_modules = try self.getCompiledModules(allocator);
        defer allocator.free(all_modules);
        const public_modules = try self.getCompiledPublicModules(allocator);
        defer allocator.free(public_modules);

        var docs_modules = std.ArrayList(PublicModuleInfo).empty;
        errdefer docs_modules.deinit(allocator);

        // The root's own compiled modules are its documentation surface.
        for (all_modules) |mod| {
            if (!std.mem.eql(u8, mod.package_name, root_name)) continue;
            switch (mod.semantic.env.module_kind) {
                .package, .platform => continue,
                .type_module, .default_app, .app, .hosted, .module, .malformed => {},
            }
            try docs_modules.append(allocator, .{
                .name = mod.name,
                .path = mod.path,
                .semantic = mod.semantic,
                .package_name = mod.package_name,
            });
        }

        // Dependencies contribute only explicit public views. Their private
        // source module names and transitive implementation helpers stay out.
        for (public_modules) |mod| {
            if (std.mem.eql(u8, mod.package_name, root_name)) continue;
            try docs_modules.append(allocator, mod);
        }

        return docs_modules.toOwnedSlice(allocator);
    }

    /// Get modules in serialization order: platform siblings → platform main → app siblings → app.
    /// This order ensures dependencies are serialized before dependents.
    pub fn getModulesInSerializationOrder(self: *BuildEnv, allocator: Allocator) Allocator.Error![]CompiledModuleInfo {
        const all_modules = try self.getCompiledModules(allocator);
        errdefer allocator.free(all_modules);

        if (all_modules.len == 0) return all_modules;

        // Separate into categories
        var platform_siblings = std.ArrayList(CompiledModuleInfo).empty;
        defer platform_siblings.deinit(allocator);
        var platform_main: ?CompiledModuleInfo = null;
        var app_siblings = std.ArrayList(CompiledModuleInfo).empty;
        defer app_siblings.deinit(allocator);
        var app_main: ?CompiledModuleInfo = null;

        for (all_modules) |mod| {
            if (mod.is_platform_sibling) {
                try platform_siblings.append(allocator, mod);
            } else if (mod.is_platform_main) {
                platform_main = mod;
            } else if (mod.is_app) {
                app_main = mod;
            } else {
                // App sibling module
                try app_siblings.append(allocator, mod);
            }
        }

        // Sort platform siblings by depth then name
        const SortContext = struct {
            pub fn lessThan(_: void, a: CompiledModuleInfo, b: CompiledModuleInfo) bool {
                if (a.depth != b.depth) return a.depth < b.depth;
                return std.mem.order(u8, a.name, b.name) == .lt;
            }
        };
        std.mem.sort(CompiledModuleInfo, platform_siblings.items, {}, SortContext.lessThan);
        std.mem.sort(CompiledModuleInfo, app_siblings.items, {}, SortContext.lessThan);

        // Build result in order: platform siblings → platform main → app siblings → app
        var result = std.ArrayList(CompiledModuleInfo).empty;
        errdefer result.deinit(allocator);

        for (platform_siblings.items) |mod| {
            try result.append(allocator, mod);
        }
        if (platform_main) |mod| {
            try result.append(allocator, mod);
        }
        for (app_siblings.items) |mod| {
            try result.append(allocator, mod);
        }
        if (app_main) |mod| {
            try result.append(allocator, mod);
        }

        allocator.free(all_modules);
        return result.toOwnedSlice(allocator);
    }

    /// Find the index of the primary module (platform main if present, otherwise app) in a module list.
    pub fn findPrimaryModuleIndex(modules: []const CompiledModuleInfo) ?usize {
        // First look for platform main
        for (modules, 0..) |mod, i| {
            if (mod.is_platform_main) return i;
        }
        // Fall back to app
        for (modules, 0..) |mod, i| {
            if (mod.is_app) return i;
        }
        return null;
    }

    /// Find the index of the app module in a module list.
    pub fn findAppModuleIndex(modules: []const CompiledModuleInfo) ?usize {
        for (modules, 0..) |mod, i| {
            if (mod.is_app) return i;
        }
        return null;
    }

    /// Get the root semantic data for the app package (convenience method).
    pub fn getAppSemanticData(self: *BuildEnv) ?SemanticModuleData {
        const root_name = self.discovered_pkg_name orelse return null;
        const root_pkg = self.packages.get(root_name) orelse return null;
        if (root_pkg.kind != .app and root_pkg.kind != .default_app) return null;
        const coord = self.coordinator orelse return null;
        const pkg = coord.packages.get(root_name) orelse return null;
        const root_id = pkg.root_module_id orelse return null;
        const root = pkg.getModule(root_id) orelse return null;
        return root.semanticData();
    }

    /// Get the root semantic data for the platform package (convenience method).
    pub fn getPlatformSemanticData(self: *BuildEnv) ?SemanticModuleData {
        // Find platform package name
        var pkg_it = self.packages.iterator();
        while (pkg_it.next()) |entry| {
            if (entry.value_ptr.kind == .platform) {
                const coord = self.coordinator orelse return null;
                const pkg = coord.packages.get(entry.key_ptr.*) orelse continue;
                const root_id = pkg.root_module_id orelse continue;
                const root = pkg.getModule(root_id) orelse continue;
                return root.semanticData();
            }
        }
        return null;
    }

    pub fn getExecutableRootSemanticData(self: *BuildEnv) ?SemanticModuleData {
        if (self.getPlatformSemanticData()) |platform| {
            if (platform.checked_artifact) |artifact| {
                if (artifact.checking_context_identity.platform_app_relation != null or
                    artifact.root_requests.requests.len > 0 or
                    artifact.provided_exports.exports.len > 0)
                {
                    return platform;
                }
            }
        }
        return self.getAppSemanticData();
    }

    pub fn executableRootCheckedArtifact(self: *BuildEnv) *const check.CheckedArtifact.CheckedModuleArtifact {
        const semantic = self.getExecutableRootSemanticData() orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("build env invariant violated: executable root semantic data is missing", .{});
            }
            unreachable;
        };
        return semantic.checked_artifact orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("build env invariant violated: executable root has no checked artifact", .{});
            }
            unreachable;
        };
    }

    pub fn collectImportedArtifactViews(
        self: *BuildEnv,
        allocator: Allocator,
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error![]check.CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(allocator);

        try appendImportedArtifactViewIfMissing(
            &views,
            allocator,
            root_artifact.key,
            &self.builtin_modules.checked_artifact,
        );

        for (root_artifact.lowering_visibility.module_ids) |key| {
            if (rootRelationContainsArtifact(root_artifact, key)) continue;
            const artifact = self.artifactByKey(key) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("build env invariant violated: missing lowering visibility artifact", .{});
                }
                unreachable;
            };
            try appendImportedArtifactViewIfMissing(&views, allocator, root_artifact.key, artifact);
        }

        return views.toOwnedSlice(allocator);
    }

    pub fn collectRelationArtifactViews(
        self: *BuildEnv,
        allocator: Allocator,
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error![]check.CheckedArtifact.ImportedModuleView {
        var views = std.ArrayList(check.CheckedArtifact.ImportedModuleView).empty;
        errdefer views.deinit(allocator);

        for (root_artifact.platform_required_bindings.bindings) |binding| {
            const artifact = self.artifactByKey(binding.app_value.artifact) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("build env invariant violated: missing relation artifact", .{});
                }
                unreachable;
            };
            var seen = false;
            for (views.items) |view| {
                if (checkedArtifactKeysEqual(view.key, artifact.key)) {
                    seen = true;
                    break;
                }
            }
            if (!seen) try views.append(allocator, check.CheckedArtifact.importedView(artifact));
        }

        return views.toOwnedSlice(allocator);
    }

    fn checkedArtifactKeysEqual(
        a: check.CheckedArtifact.CheckedModuleArtifactKey,
        b: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) bool {
        return std.mem.eql(u8, &a.bytes, &b.bytes);
    }

    fn appendImportedArtifactViewIfMissing(
        views: *std.ArrayList(check.CheckedArtifact.ImportedModuleView),
        allocator: Allocator,
        root_key: check.CheckedArtifact.CheckedModuleArtifactKey,
        artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    ) Allocator.Error!void {
        if (checkedArtifactKeysEqual(artifact.key, root_key)) return;
        for (views.items) |view| {
            if (checkedArtifactKeysEqual(view.key, artifact.key)) return;
        }
        try views.append(allocator, check.CheckedArtifact.importedView(artifact));
    }

    fn rootRelationContainsArtifact(
        root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) bool {
        for (root_artifact.platform_required_bindings.bindings) |binding| {
            if (checkedArtifactKeysEqual(binding.app_value.artifact, key)) return true;
        }
        return false;
    }

    fn artifactByKey(
        self: *const BuildEnv,
        key: check.CheckedArtifact.CheckedModuleArtifactKey,
    ) ?*const check.CheckedArtifact.CheckedModuleArtifact {
        const coord = self.coordinator orelse return null;
        return coord.checkedArtifactByKey(key);
    }

    /// Drain reports and render them to a writer. Returns error/warning counts.
    /// Replaces the repeated drain → iterate → render boilerplate pattern.
    pub fn renderDiagnostics(
        self: *BuildEnv,
        writer: anytype,
        config: reporting.ReportingConfig,
    ) Allocator.Error!RenderDiagnosticsResult {
        const drained = try self.drainReports();
        defer self.freeDrainedReports(drained);

        var total_error_count: usize = 0;
        var total_warning_count: usize = 0;
        const palette = reporting.ColorUtils.getPaletteForConfig(config);

        for (drained) |mod| {
            for (mod.reports) |*report| {
                switch (report.severity) {
                    .runtime_error, .fatal => total_error_count += 1,
                    .warning => total_warning_count += 1,
                }
                reporting.renderReportToTerminal(report, writer, palette, config) catch {};
            }
        }

        return .{
            .errors = total_error_count,
            .warnings = total_warning_count,
        };
    }

    /// Render one warning for each `dbg` that remains in an optimized build.
    /// Optimized builds intentionally keep `dbg` so users can debug performance
    /// problems, but release artifacts should make those call sites visible.
    pub fn renderOptimizedDbgWarnings(
        self: *BuildEnv,
        writer: anytype,
        opt_name: []const u8,
        config: reporting.ReportingConfig,
    ) Allocator.Error!usize {
        const modules = try self.getCompiledModules(self.gpa);
        defer self.gpa.free(modules);

        var total: usize = 0;
        for (modules) |mod| {
            var regions = std.ArrayList(base.Region).empty;
            defer regions.deinit(self.gpa);

            try collectDbgRegionsInModule(self.gpa, mod.semantic.env, &regions);
            for (regions.items) |region| {
                try self.renderOptimizedDbgWarning(writer, mod.semantic.env, mod.path, opt_name, region, config);
                total += 1;
            }
        }
        return total;
    }

    fn renderOptimizedDbgWarning(
        self: *BuildEnv,
        writer: anytype,
        env: *const ModuleEnv,
        path: []const u8,
        opt_name: []const u8,
        region: base.Region,
        config: reporting.ReportingConfig,
    ) Allocator.Error!void {
        var report = try Report.init(self.gpa, "`dbg` in Optimized Build", "", .warning);
        defer report.deinit();

        try report.headline.addReflowingText("This optimized build was compiled from a ");
        try report.headline.addInlineCode("dbg");
        try report.headline.addReflowingText(" statement.");

        try self.addOptimizedDbgSourceRegion(&report, env, path, region);

        try report.document.addLineBreak();
        try report.document.addReflowingText("Builds with ");
        const opt_arg = try std.fmt.allocPrint(self.gpa, "--opt={s}", .{opt_name});
        defer self.gpa.free(opt_arg);
        const owned_opt_arg = try report.addOwnedString(opt_arg);
        try report.document.addInlineCode(owned_opt_arg);
        try report.document.addReflowingText(" keep ");
        try report.document.addInlineCode("dbg");
        try report.document.addReflowingText(" output, but ");
        try report.document.addInlineCode("dbg");
        try report.document.addReflowingText(" is intended for debugging.");

        const palette = reporting.ColorUtils.getPaletteForConfig(config);
        reporting.renderReportToTerminal(&report, writer, palette, config) catch {};
    }

    fn addOptimizedDbgSourceRegion(
        self: *BuildEnv,
        report: *Report,
        env: *const ModuleEnv,
        path: []const u8,
        region: base.Region,
    ) Allocator.Error!void {
        if (self.synthetic_root_original_path) |original_path| {
            if (self.synthetic_root_original_source) |original_source| {
                if (self.discovered_root_abs) |root_path| {
                    const header_len: u32 = @intCast(self.synthetic_root_header_len);
                    if (std.mem.eql(u8, path, root_path) and region.start.offset >= header_len and region.end.offset >= header_len) {
                        var line_starts = try base.RegionInfo.findLineStarts(self.gpa, original_source);
                        defer line_starts.deinit(self.gpa);

                        const mapped_region = base.Region.from_raw_offsets(
                            region.start.offset - header_len,
                            region.end.offset - header_len,
                        );
                        const region_info = base.RegionInfo.position(
                            original_source,
                            line_starts.items.items,
                            mapped_region.start.offset,
                            mapped_region.end.offset,
                        ) catch unreachable;

                        const owned_path = try report.addOwnedString(original_path);
                        try report.document.addSourceRegion(
                            region_info,
                            .warning_highlight,
                            owned_path,
                            original_source,
                            line_starts.items.items,
                        );
                        return;
                    }
                }
            }
        }

        const owned_path = try report.addOwnedString(path);
        try report.document.addSourceRegion(
            env.calcRegionInfo(region),
            .warning_highlight,
            owned_path,
            env.getSourceAll(),
            env.getLineStartsAll(),
        );
    }

    fn collectDbgRegionsInModule(
        allocator: Allocator,
        env: *const ModuleEnv,
        regions: *std.ArrayList(base.Region),
    ) Allocator.Error!void {
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            const def = env.store.getDef(def_idx);
            try collectDbgRegionsInExpr(allocator, env, regions, def.expr);
        }

        var offset: usize = 0;
        while (offset < env.all_statements.span.len) : (offset += 1) {
            try collectDbgRegionsInStatement(allocator, env, regions, env.store.statementAt(env.all_statements, offset));
        }
    }

    fn collectDbgRegionsInStatement(
        allocator: Allocator,
        env: *const ModuleEnv,
        regions: *std.ArrayList(base.Region),
        statement_idx: CIR.Statement.Idx,
    ) Allocator.Error!void {
        switch (env.store.getStatement(statement_idx)) {
            .s_decl => |stmt| try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr),
            .s_var => |stmt| try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr),
            .s_reassign => |stmt| try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr),
            .s_expr => |stmt| try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr),
            .s_expect => {},
            .s_dbg => |stmt| {
                try regions.append(allocator, env.store.getStatementRegion(statement_idx));
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr);
            },
            .s_for => |stmt| {
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr);
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.body);
            },
            .s_while => |stmt| {
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.cond);
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.body);
            },
            .s_infinite_loop => |stmt| {
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.cond);
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.body);
            },
            .s_breakable_loop => |stmt| {
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.cond);
                try collectDbgRegionsInExpr(allocator, env, regions, stmt.body);
            },
            .s_return => |stmt| try collectDbgRegionsInExpr(allocator, env, regions, stmt.expr),
            .s_crash,
            .s_var_uninitialized,
            .s_break,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => {},
        }
    }

    fn collectDbgRegionsInExprSpan(
        allocator: Allocator,
        env: *const ModuleEnv,
        regions: *std.ArrayList(base.Region),
        span: CIR.Expr.Span,
    ) Allocator.Error!void {
        for (env.store.sliceExpr(span)) |expr_idx| {
            try collectDbgRegionsInExpr(allocator, env, regions, expr_idx);
        }
    }

    fn collectDbgRegionsInExpr(
        allocator: Allocator,
        env: *const ModuleEnv,
        regions: *std.ArrayList(base.Region),
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        switch (env.store.getExpr(expr_idx)) {
            .e_str => |str| try collectDbgRegionsInExprSpan(allocator, env, regions, str.span),
            .e_list => |list| try collectDbgRegionsInExprSpan(allocator, env, regions, list.elems),
            .e_tuple => |tuple| try collectDbgRegionsInExprSpan(allocator, env, regions, tuple.elems),
            .e_record => |record| {
                if (record.ext) |ext| try collectDbgRegionsInExpr(allocator, env, regions, ext);
                for (env.store.sliceRecordFields(record.fields)) |field_idx| {
                    try collectDbgRegionsInExpr(allocator, env, regions, env.store.getRecordField(field_idx).value);
                }
            },
            .e_block => |block| {
                var offset: usize = 0;
                while (offset < block.stmts.span.len) : (offset += 1) {
                    try collectDbgRegionsInStatement(allocator, env, regions, env.store.statementAt(block.stmts, offset));
                }
                try collectDbgRegionsInExpr(allocator, env, regions, block.final_expr);
            },
            .e_if => |if_expr| {
                for (env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = env.store.getIfBranch(branch_idx);
                    try collectDbgRegionsInExpr(allocator, env, regions, branch.cond);
                    try collectDbgRegionsInExpr(allocator, env, regions, branch.body);
                }
                try collectDbgRegionsInExpr(allocator, env, regions, if_expr.final_else);
            },
            .e_match => |match_expr| {
                try collectDbgRegionsInExpr(allocator, env, regions, match_expr.cond);
                for (env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = env.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard| try collectDbgRegionsInExpr(allocator, env, regions, guard);
                    try collectDbgRegionsInExpr(allocator, env, regions, branch.value);
                }
            },
            .e_call => |call| {
                try collectDbgRegionsInExpr(allocator, env, regions, call.func);
                try collectDbgRegionsInExprSpan(allocator, env, regions, call.args);
            },
            .e_lambda => |lambda| try collectDbgRegionsInExpr(allocator, env, regions, lambda.body),
            .e_closure => |closure| try collectDbgRegionsInExpr(allocator, env, regions, closure.lambda_idx),
            .e_nominal => |nominal| try collectDbgRegionsInExpr(allocator, env, regions, nominal.backing_expr),
            .e_nominal_external => |nominal| try collectDbgRegionsInExpr(allocator, env, regions, nominal.backing_expr),
            .e_binop => |binop| {
                try collectDbgRegionsInExpr(allocator, env, regions, binop.lhs);
                try collectDbgRegionsInExpr(allocator, env, regions, binop.rhs);
            },
            .e_unary_minus => |unary| try collectDbgRegionsInExpr(allocator, env, regions, unary.expr),
            .e_unary_not => |unary| try collectDbgRegionsInExpr(allocator, env, regions, unary.expr),
            .e_field_access => |field| try collectDbgRegionsInExpr(allocator, env, regions, field.receiver),
            .e_method_call => |call| {
                try collectDbgRegionsInExpr(allocator, env, regions, call.receiver);
                try collectDbgRegionsInExprSpan(allocator, env, regions, call.args);
            },
            .e_dispatch_call => |call| {
                try collectDbgRegionsInExpr(allocator, env, regions, call.receiver);
                try collectDbgRegionsInExprSpan(allocator, env, regions, call.args);
            },
            .e_interpolation => |interpolation| {
                try collectDbgRegionsInExpr(allocator, env, regions, interpolation.first);
                try collectDbgRegionsInExprSpan(allocator, env, regions, interpolation.parts);
            },
            .e_structural_eq => |eq| {
                try collectDbgRegionsInExpr(allocator, env, regions, eq.lhs);
                try collectDbgRegionsInExpr(allocator, env, regions, eq.rhs);
            },
            .e_structural_hash => |hash| {
                try collectDbgRegionsInExpr(allocator, env, regions, hash.value);
                try collectDbgRegionsInExpr(allocator, env, regions, hash.hasher);
            },
            .e_method_eq => |eq| {
                try collectDbgRegionsInExpr(allocator, env, regions, eq.lhs);
                try collectDbgRegionsInExpr(allocator, env, regions, eq.rhs);
            },
            .e_type_method_call => |call| try collectDbgRegionsInExprSpan(allocator, env, regions, call.args),
            .e_type_dispatch_call => |call| try collectDbgRegionsInExprSpan(allocator, env, regions, call.args),
            .e_tuple_access => |access| try collectDbgRegionsInExpr(allocator, env, regions, access.tuple),
            .e_dbg => |dbg| {
                try regions.append(allocator, env.store.getExprRegion(expr_idx));
                try collectDbgRegionsInExpr(allocator, env, regions, dbg.expr);
            },
            .e_expect_err => |expect_err| try collectDbgRegionsInExpr(allocator, env, regions, expect_err.expr),
            .e_expect => {},
            .e_return => |ret| try collectDbgRegionsInExpr(allocator, env, regions, ret.expr),
            .e_for => |for_expr| {
                try collectDbgRegionsInExpr(allocator, env, regions, for_expr.expr);
                try collectDbgRegionsInExpr(allocator, env, regions, for_expr.body);
            },
            .e_run_low_level => |run| try collectDbgRegionsInExprSpan(allocator, env, regions, run.args),
            .e_tag => |tag| try collectDbgRegionsInExprSpan(allocator, env, regions, tag.args),
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_bytes_literal,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_associated_resolved,
            .e_lookup_required,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_break,
            .e_hosted_lambda,
            => {},
        }
    }

    pub const RenderDiagnosticsResult = struct {
        errors: usize,
        warnings: usize,
    };

    /// Get compiled module envs ready for backend use: Builtin at [0], imports resolved.
    /// Replaces the repeated pattern of getCompiledModules + build array + resolveImports.
    pub fn getResolvedModuleEnvs(self: *BuildEnv, allocator: Allocator) Allocator.Error!ResolvedModules {
        const modules = try self.getCompiledModules(allocator);
        if (modules.len == 0) return error.NoModulesCompiled;

        const builtin_env = self.builtin_modules.builtin_module.env;
        var all_module_envs = try allocator.alloc(*ModuleEnv, modules.len + 1);
        all_module_envs[0] = builtin_env;
        for (modules, 0..) |mod, i| {
            all_module_envs[i + 1] = mod.semantic.env;
        }

        // Resolve imports directly from the assembled module env array.
        for (all_module_envs) |module| {
            module.imports.clearResolvedModules();
            for (module.imports.imports.items.items, 0..) |str_idx, i| {
                const import_name = module.getString(str_idx);
                for (all_module_envs, 0..) |candidate_env, module_idx| {
                    if (std.mem.eql(u8, candidate_env.module_name, import_name)) {
                        module.imports.setResolvedModule(@enumFromInt(i), @intCast(module_idx));
                        break;
                    }
                }
            }
        }

        return .{
            .all_module_envs = all_module_envs,
            .compiled_modules = modules,
        };
    }

    /// Result of getResolvedModuleEnvs: compiled modules with Builtin at [0] and imports resolved.
    pub const ResolvedModules = struct {
        /// Module envs array with Builtin at index 0, ready for backend use.
        all_module_envs: []*ModuleEnv,
        /// Metadata for each compiled module (indices correspond to all_module_envs[1..]).
        compiled_modules: []CompiledModuleInfo,

        /// Get module envs excluding Builtin (for closure pipeline, etc.)
        pub fn compiledModuleEnvs(self: *const ResolvedModules) []*ModuleEnv {
            return self.all_module_envs[1..];
        }
    };
};

test "BuildEnv collectWatchInputStates includes package root state" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const cwd = try std.fs.path.resolve(allocator, &.{"."});
    defer allocator.free(cwd);

    var env = try BuildEnv.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        cwd,
        testing.io,
    );
    defer env.deinit();
    env.setWatchInputTracking(true);

    const root_hash = [_]u8{7} ** 32;
    const key = try allocator.dupe(u8, "pkg");
    errdefer allocator.free(key);

    try env.packages.put(allocator, key, .{
        .name = try allocator.dupe(u8, "pkg"),
        .kind = .package,
        .root_file = try allocator.dupe(u8, "/test/pkg/main.roc"),
        .root_file_state = .{ .hash = root_hash },
        .root_dir = try allocator.dupe(u8, "/test/pkg"),
    });

    const inputs = try env.collectWatchInputStates();
    defer env.freeWatchInputStates(inputs);

    try testing.expectEqual(@as(usize, 1), inputs.len);
    try testing.expectEqualStrings("/test/pkg/main.roc", inputs[0].path);
    switch (inputs[0].state) {
        .hash => |hash| try testing.expectEqualSlices(u8, &root_hash, &hash),
        .missing, .unreadable => try testing.expect(false),
    }
}

test "BuildEnv collectWatchInputStates resolves file dependencies from module source dir override" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    try tmp.dir.createDir(testing.io, "real-src", .default_dir);
    try tmp.dir.createDir(testing.io, "generated", .default_dir);

    const real_src_dir = try std.fs.path.join(allocator, &.{ tmp_root, "real-src" });
    defer allocator.free(real_src_dir);
    const generated_dir = try std.fs.path.join(allocator, &.{ tmp_root, "generated" });
    defer allocator.free(generated_dir);
    const generated_app_path = try std.fs.path.join(allocator, &.{ generated_dir, "app.roc" });
    defer allocator.free(generated_app_path);
    const expected_file_dep_path = try std.fs.path.resolve(allocator, &.{ real_src_dir, "data.txt" });
    defer allocator.free(expected_file_dep_path);

    var env = try BuildEnv.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        tmp_root,
        testing.io,
    );
    defer env.deinit();
    env.setWatchInputTracking(true);

    const package_key = try allocator.dupe(u8, "pkg");
    try env.packages.put(allocator, package_key, .{
        .name = try allocator.dupe(u8, "pkg"),
        .kind = .package,
        .root_file = try allocator.dupe(u8, generated_app_path),
        .root_file_state = .{ .hash = [_]u8{1} ** 32 },
        .root_dir = try allocator.dupe(u8, generated_dir),
    });

    const coord = try allocator.create(Coordinator);
    coord.* = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        env.builtin_modules,
        build_options.compiler_version,
        null,
        env.filesystem,
    );
    coord.setWatchInputTracking(true);
    env.coordinator = coord;

    const coord_pkg = try coord.ensurePackage("pkg", generated_dir);
    try coord_pkg.setRootInput(allocator, generated_app_path, .{ .hash = [_]u8{1} ** 32 });
    const coord_module_id = try coord_pkg.ensureModule(allocator, "App", generated_app_path);
    const coord_mod = &coord_pkg.modules.items[coord_module_id];
    coord_mod.source_dir_override = try allocator.dupe(u8, real_src_dir);
    coord_mod.source_file_state = .{ .hash = [_]u8{2} ** 32 };
    try testing.expectEqualStrings(real_src_dir, coord_mod.canonicalSourceDir());

    const source = try allocator.dupe(u8, "main = 1\n");
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);
    try module_env.initCIRFields("App");
    const dep_idx = try module_env.recordFileDependency("data.txt", 0, 0);
    const dep_hash = [_]u8{3} ** 32;
    module_env.setFileDependencyContentHash(dep_idx, dep_hash);
    coord_mod.semantic = .{ .module_env = module_env, .checked_artifact = null };

    const inputs = try env.collectWatchInputStates();
    defer env.freeWatchInputStates(inputs);

    var found_file_dep = false;
    for (inputs) |input| {
        if (!std.mem.eql(u8, input.path, expected_file_dep_path)) continue;
        found_file_dep = true;
        switch (input.state) {
            .hash => |hash| try testing.expectEqualSlices(u8, &dep_hash, &hash),
            .missing, .unreadable => try testing.expect(false),
        }
    }
    try testing.expect(found_file_dep);
}

test "BuildEnv header watch state hashes raw CRLF source bytes" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const source = "main = 1\r\n";
    try tmp.dir.writeFile(testing.io, .{ .sub_path = "main.roc", .data = source });

    const main_path = try std.fs.path.join(allocator, &.{ tmp_root, "main.roc" });
    defer allocator.free(main_path);

    var env = try BuildEnv.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        tmp_root,
        testing.io,
    );
    defer env.deinit();
    env.setWatchInputTracking(true);

    var header_info = try env.parseHeaderDeps(main_path);
    defer header_info.deinit(allocator);

    const expected_hash = watch_inputs.hashBytes(source);
    switch (header_info.source_file_state orelse return error.ExpectedWatchInputState) {
        .hash => |hash| try testing.expectEqualSlices(u8, &expected_hash, &hash),
        .missing, .unreadable => try testing.expect(false),
    }
}

// OrderedSink buffers reports and emits them in a deterministic global order.
// - Sorting key: (min dependency depth from root app, then package and module names for internal sorting only)
// - Reporting: per-module report aggregation; file paths are provided by BuildEnv at sink time
/// OrderedSink provides deterministic, ordered emission of module reports across
/// the entire workspace. It ensures that reports are always emitted in a consistent
/// order based on (depth, module_name) regardless of the actual completion order.
///
/// Key design principles:
/// - Modules can complete in any order (due to parallelism)
/// - Reports are buffered until they can be emitted in the correct order
/// - Once a module's reports are drained, they cannot be modified
/// - Supports both streaming (during build) and batch (end of build) draining
///
/// Thread safety: All public methods are thread-safe via internal locking.
pub const OrderedSink = struct {
    const ModuleKey = struct {
        pkg: []const u8,
        module: []const u8,
    };

    const ModuleKeyContext = struct {
        pub fn hash(_: @This(), key: ModuleKey) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(key.pkg);
            h.update(key.module);
            return h.final();
        }

        pub fn eql(_: @This(), a: ModuleKey, b: ModuleKey) bool {
            return std.mem.eql(u8, a.pkg, b.pkg) and std.mem.eql(u8, a.module, b.module);
        }
    };
    const Entry = struct {
        pkg_name: []const u8, // borrowed from BuildEnv.packages
        module_name: []const u8, // borrowed from Coordinator module state
        depth: u32, // min dependency depth
        reports: std.array_list.Managed(Report), // zero or more reports for this module
        ready: bool,
        emitted: bool,
    };

    gpa: Allocator,
    std_io: std.Io,
    lock: Mutex = Mutex.init,
    cond: ThreadCondition = ThreadCondition.init,

    // Ordered buffer and index
    entries: std.array_list.Managed(Entry),
    // Precomputed global order - indices into entries array
    order: std.array_list.Managed(usize),
    // Map (pkg, module) -> index in entries
    index: std.HashMap(ModuleKey, usize, ModuleKeyContext, 80),
    // Drain cursor into order[] for emitted entries
    // INVARIANT: drain_cursor tracks the position in order[] up to which all modules
    // have been emitted. Modules at order[0..drain_cursor] are guaranteed to be emitted.
    // Once a module is drained, its reports cannot be modified - any reports added after
    // draining will not be seen by subsequent drain operations. This ensures deterministic
    // ordering but means modules must complete all reporting before being marked as ready.
    drain_cursor: usize = 0,

    // Lifecycle
    pub fn init(gpa: Allocator) OrderedSink {
        return .{
            .gpa = gpa,
            .std_io = undefined,
            .entries = std.array_list.Managed(Entry).init(gpa),
            .order = std.array_list.Managed(usize).init(gpa),
            .index = std.HashMap(ModuleKey, usize, ModuleKeyContext, 80).init(gpa),
        };
    }

    pub fn deinit(self: *OrderedSink) void {
        // Free entries
        for (self.entries.items) |*e| {
            // pkg_name and module_name are borrowed, don't free
            // Deinit all reports for this module
            for (e.reports.items) |*rep| {
                rep.deinit();
            }
            e.reports.deinit();
        }
        self.entries.deinit();
        self.order.deinit();
        self.index.deinit();
    }

    // Build deterministic order once: caller provides package names, module names, and depths
    pub fn buildOrder(self: *OrderedSink, pkg_names: []const []const u8, module_names: []const []const u8, depths: []const u32) Allocator.Error!void {
        if (comptime trace_build) {
            std.debug.print("[SINK] buildOrder: {} modules\n", .{pkg_names.len});
        }
        try self.order.ensureTotalCapacity(pkg_names.len);
        try self.entries.ensureTotalCapacity(pkg_names.len);
        try self.index.ensureTotalCapacity(@as(u32, @intCast(pkg_names.len)));

        // Refresh order; allow pre-registered entries (from early emits) and update their metadata
        self.order.items.len = 0;

        var i: usize = 0;
        while (i < pkg_names.len) : (i += 1) {
            if (comptime trace_build) {
                std.debug.print("[SINK] buildOrder: checking pkg=\"{s}\" module=\"{s}\" depth={}\n", .{ pkg_names[i], module_names[i], depths[i] });
            }
            const key = ModuleKey{ .pkg = pkg_names[i], .module = module_names[i] };
            var entry_index: usize = undefined;
            if (self.index.get(key)) |idx| {
                if (comptime trace_build) {
                    std.debug.print("[SINK] buildOrder: found entry at idx={}, ready={} depth={}\n", .{ idx, self.entries.items[idx].ready, depths[i] });
                }
                entry_index = idx;
                // Update depth
                self.entries.items[entry_index].depth = depths[i];
            } else {
                // New entry
                entry_index = self.entries.items.len;
                const reports_list = std.array_list.Managed(Report).init(self.gpa);
                try self.entries.append(.{
                    .pkg_name = pkg_names[i],
                    .module_name = module_names[i],
                    .depth = depths[i],
                    .reports = reports_list,
                    .ready = false,
                    .emitted = false,
                });
                try self.index.put(key, entry_index);
            }
            // Add index to order
            try self.order.append(entry_index);
        }

        // Sort order array by (depth, name) via a simple stable sort (normalize case on Windows)
        const self_ptr = self;
        std.sort.block(usize, self.order.items, self_ptr, struct {
            fn lessIgnoreCase(a: []const u8, b: []const u8) bool {
                var idx: usize = 0;
                const min_len = if (a.len < b.len) a.len else b.len;
                while (idx < min_len) : (idx += 1) {
                    const ca = std.ascii.toLower(a[idx]);
                    const cb = std.ascii.toLower(b[idx]);
                    if (ca < cb) return true;
                    if (ca > cb) return false;
                }
                return a.len < b.len;
            }

            fn lt(ctx: *OrderedSink, a_idx: usize, b_idx: usize) bool {
                const ea = ctx.entries.items[a_idx];
                const eb = ctx.entries.items[b_idx];
                if (ea.depth != eb.depth) {
                    return ea.depth < eb.depth;
                }
                // Case-insensitive compare to be safe on Windows-like filesystems
                // First compare packages
                const pkg_cmp = lessIgnoreCase(ea.pkg_name, eb.pkg_name);
                if (pkg_cmp != lessIgnoreCase(eb.pkg_name, ea.pkg_name)) {
                    return pkg_cmp;
                }
                // Then compare modules
                return lessIgnoreCase(ea.module_name, eb.module_name);
            }
        }.lt);
    }

    // Emit with package and module names
    pub fn emitReport(self: *OrderedSink, pkg_name: []const u8, module_name: []const u8, report: Report) Allocator.Error!void {
        self.lock.lockUncancelable(self.std_io);
        defer self.lock.unlock(self.std_io);

        if (comptime trace_build) {
            std.debug.print("[SINK] emitReport: pkg=\"{s}\" module=\"{s}\" title=\"{s}\"\n", .{ pkg_name, module_name, report.title });
        }

        // Lookup entry; auto-register if needed so we can buffer before order is built
        const key = ModuleKey{ .pkg = pkg_name, .module = module_name };
        var entry_index: usize = undefined;
        if (self.index.get(key)) |idx| {
            if (comptime trace_build) {
                std.debug.print("[SINK] emitReport: found existing entry at idx={}\n", .{idx});
            }
            entry_index = idx;
        } else {
            entry_index = self.entries.items.len;
            const reports_list = std.array_list.Managed(Report).init(self.gpa);
            try self.entries.append(.{
                .pkg_name = pkg_name,
                .module_name = module_name,
                .depth = std.math.maxInt(u32),
                .reports = reports_list,
                .ready = false,
                .emitted = false,
            });
            try self.index.put(key, entry_index);
            // Note: do not append to order here; buildOrder will populate and sort later
        }

        // Record report; take ownership by appending to per-module list
        try self.entries.items[entry_index].reports.append(report);
        self.entries.items[entry_index].ready = true;

        // Attempt ordered emission only if order has been built
        if (self.order.items.len > 0) self.tryEmitLocked();
    }

    // Attempt to emit entries in order prefix while next entries are ready (with locking).
    pub fn tryEmit(self: *OrderedSink) void {
        self.lock.lockUncancelable(self.std_io);
        defer self.lock.unlock(self.std_io);
        self.tryEmitLocked();
    }

    // Attempt to emit entries in order prefix while next entries are ready.
    fn tryEmitLocked(self: *OrderedSink) void {
        if (comptime trace_build) {
            std.debug.print("[SINK] tryEmitLocked: order.len={}\n", .{self.order.items.len});
        }
        var i: usize = 0;
        while (i < self.order.items.len) : (i += 1) {
            const entry_idx = self.order.items[i];
            const e = &self.entries.items[entry_idx];

            if (comptime trace_build) {
                std.debug.print("[SINK] tryEmitLocked: i={} entry_idx={} pkg={s} mod={s} ready={} emitted={}\n", .{ i, entry_idx, e.pkg_name, e.module_name, e.ready, e.emitted });
            }

            // Prefix gating: stop at first entry that is not yet ready and not yet emitted
            if (!e.ready and !e.emitted) {
                if (comptime trace_build) {
                    std.debug.print("[SINK] tryEmitLocked: breaking at i={} (not ready and not emitted)\n", .{i});
                }
                break;
            }

            // Skip already-emitted entries
            if (e.emitted) continue;

            // Emit this ready entry
            if (comptime trace_build) {
                std.debug.print("[SINK] tryEmitLocked: marking i={} as emitted\n", .{i});
            }
            e.emitted = true;
        }
        if (comptime trace_build) {
            std.debug.print("[SINK] tryEmitLocked: done, processed {} entries\n", .{i});
        }
    }
    pub const Drained = struct {
        pkg_name: []const u8,
        module_name: []const u8,
        reports: []Report,
    };

    pub fn drainEmitted(self: *OrderedSink, gpa: Allocator) Allocator.Error![]Drained {
        self.lock.lockUncancelable(self.std_io);
        defer self.lock.unlock(self.std_io);

        // Identify contiguous emitted prefix starting from current drain cursor
        var i: usize = self.drain_cursor;
        while (i < self.order.items.len) : (i += 1) {
            const entry_idx = self.order.items[i];
            const e = &self.entries.items[entry_idx];
            if (!e.emitted) break;
        }

        // Count only entries with reports (skip empty entries)
        var reports_count: usize = 0;
        {
            var k: usize = self.drain_cursor;
            while (k < i) : (k += 1) {
                const entry_idx = self.order.items[k];
                const e = &self.entries.items[entry_idx];
                if (e.reports.items.len > 0) {
                    reports_count += 1;
                }
            }
        }

        if (reports_count == 0) {
            self.drain_cursor = i;
            return try gpa.alloc(Drained, 0);
        }

        var out = try gpa.alloc(Drained, reports_count);
        var j: usize = 0;
        var k: usize = self.drain_cursor;
        while (k < i) : (k += 1) {
            const entry_idx = self.order.items[k];
            const e = &self.entries.items[entry_idx];

            // Skip entries with no reports
            if (e.reports.items.len == 0) {
                e.ready = false;
                e.emitted = false;
                continue;
            }

            // Move reports out; reset readiness for potential future appends
            const reps = e.reports.toOwnedSlice() catch {
                // Back out partially allocated results on failure
                var m: usize = 0;
                while (m < j) : (m += 1) {
                    for (out[m].reports) |*r| r.deinit();
                    gpa.free(out[m].reports);
                }
                gpa.free(out);
                return error.OutOfMemory;
            };

            out[j] = .{
                .pkg_name = e.pkg_name,
                .module_name = e.module_name,
                .reports = reps,
            };
            j += 1;

            // Reinitialize the reports ArrayList since toOwnedSlice() moved ownership
            e.reports = std.array_list.Managed(Report).init(self.gpa);
            e.ready = false;
            e.emitted = false;
        }

        self.drain_cursor = i;
        return out;
    }
};

test "issue 9737: logicalModuleToPath frees its scratch path exactly once on the PathOutsideWorkspace error path" {
    const gpa = std.testing.allocator;

    // logicalModuleToPath only reads `gpa` and `workspace_roots`, so a minimal BuildEnv
    // with just those fields set is enough to exercise it.
    var env: BuildEnv = undefined;
    env.gpa = gpa;
    env.workspace_roots = std.array_list.Managed([]const u8).init(gpa);
    defer env.workspace_roots.deinit();

    // No workspace roots are registered, so the resolved "<root>/Mod.roc" is
    // outside the workspace and logicalModuleToPath takes its error path. On that path
    // it must free its `with_ext` scratch allocation exactly once; freeing it a
    // second time is a double free that the testing allocator detects.
    try std.testing.expectError(error.PathOutsideWorkspace, env.logicalModuleToPath("/tmp/roc-issue-9737", "Mod"));
}

test "findPackageForModulePath deterministically selects nested package over outer package root_dir" {
    const gpa = std.testing.allocator;

    var coord: coordinator_mod.Coordinator = undefined;
    coord.packages = std.StringHashMap(*coordinator_mod.PackageState).init(gpa);
    defer coord.packages.deinit();
    coord.app_package_name = null;

    var pkg_app = coordinator_mod.PackageState.init(gpa, try gpa.dupe(u8, "app"), try gpa.dupe(u8, "/workspace"), null);
    defer pkg_app.deinit(gpa);
    var pkg_b = coordinator_mod.PackageState.init(gpa, try gpa.dupe(u8, "pkg_b"), try gpa.dupe(u8, "/workspace/pkg_b"), null);
    defer pkg_b.deinit(gpa);

    try coord.packages.put("app", &pkg_app);
    try coord.packages.put("pkg_b", &pkg_b);

    var env: BuildEnv = undefined;
    env.gpa = gpa;
    env.coordinator = &coord;
    env.discovered_pkg_name = null;

    // Exact nested file matches the more specific package, not the outer workspace package
    const nested_pkg = env.findPackageForModulePath("/workspace/pkg_b/ModB.roc");
    try std.testing.expect(nested_pkg != null);
    try std.testing.expectEqualStrings("pkg_b", nested_pkg.?.name);

    // Top-level workspace file matches the outer package
    const app_pkg = env.findPackageForModulePath("/workspace/Main.roc");
    try std.testing.expect(app_pkg != null);
    try std.testing.expectEqualStrings("app", app_pkg.?.name);

    // Partial directory name match on same prefix (e.g. /workspace/pkg_bad vs /workspace/pkg_b) matches /workspace, not pkg_b
    const sibling_pkg = env.findPackageForModulePath("/workspace/pkg_bad/Mod.roc");
    try std.testing.expect(sibling_pkg != null);
    try std.testing.expectEqualStrings("app", sibling_pkg.?.name);

    // Unowned document path returns null (never guesses or falls back to root/app package)
    env.discovered_pkg_name = "app";
    coord.app_package_name = "app";
    const unowned_pkg = env.findPackageForModulePath("/unrelated/path/Doc.roc");
    try std.testing.expect(unowned_pkg == null);
}

test "findModuleByQualifiedNameInPackage strictly preserves shorthand identity and does not cross namespaces" {
    const gpa = std.testing.allocator;

    var coord: coordinator_mod.Coordinator = undefined;
    coord.packages = std.StringHashMap(*coordinator_mod.PackageState).init(gpa);
    defer coord.packages.deinit();
    coord.app_package_name = null;

    var pkg_app = coordinator_mod.PackageState.init(gpa, try gpa.dupe(u8, "app"), try gpa.dupe(u8, "/workspace"), null);
    defer pkg_app.deinit(gpa);
    var pkg_dep = coordinator_mod.PackageState.init(gpa, try gpa.dupe(u8, "dep_pkg"), try gpa.dupe(u8, "/workspace/dep"), null);
    defer pkg_dep.deinit(gpa);

    // App has shorthand "dep" -> "dep_pkg"
    try pkg_app.shorthands.put(try gpa.dupe(u8, "dep"), try gpa.dupe(u8, "dep_pkg"));

    // App also has a hierarchical module named "dep.Missing"
    try pkg_app.modules.append(gpa, coordinator_mod.ModuleState.init(try gpa.dupe(u8, "dep.Missing"), try gpa.dupe(u8, "/workspace/dep_Missing.roc")));
    try pkg_app.module_names.put(pkg_app.modules.items[0].name, 0);

    // dep_pkg has module "RealModule" (but not "Missing")
    try pkg_dep.modules.append(gpa, coordinator_mod.ModuleState.init(try gpa.dupe(u8, "RealModule"), try gpa.dupe(u8, "/workspace/dep/RealModule.roc")));
    try pkg_dep.module_names.put(pkg_dep.modules.items[0].name, 0);

    try coord.packages.put("app", &pkg_app);
    try coord.packages.put("dep_pkg", &pkg_dep);

    var env: BuildEnv = undefined;
    env.gpa = gpa;
    env.coordinator = &coord;
    env.discovered_pkg_name = null;

    // 1. Dotted name with declared shorthand resolves in target package
    const mod_res = env.findModuleByQualifiedNameInPackage(&pkg_app, "dep.RealModule");
    try std.testing.expect(mod_res != null);
    try std.testing.expectEqualStrings("RealModule", mod_res.?.name);

    // 2. Dotted name with declared shorthand where target package misses module does NOT fall through to hierarchical module "dep.Missing"
    const missing_res = env.findModuleByQualifiedNameInPackage(&pkg_app, "dep.Missing");
    try std.testing.expect(missing_res == null);

    // 3. Dotted name with undeclared qualifier does NOT resolve against foreign canonical packages
    var pkg_other = coordinator_mod.PackageState.init(gpa, try gpa.dupe(u8, "foreign"), try gpa.dupe(u8, "/workspace/foreign"), null);
    defer pkg_other.deinit(gpa);
    try pkg_other.modules.append(gpa, coordinator_mod.ModuleState.init(try gpa.dupe(u8, "Mod"), try gpa.dupe(u8, "/workspace/foreign/Mod.roc")));
    try pkg_other.module_names.put(pkg_other.modules.items[0].name, 0);
    try coord.packages.put("foreign", &pkg_other);

    const foreign_res = env.findModuleByQualifiedNameInPackage(&pkg_app, "foreign.Mod");
    try std.testing.expect(foreign_res == null);

    // 4. Null importing package context returns null (never guesses or falls back to root/app package)
    env.discovered_pkg_name = "app";
    coord.app_package_name = "app";
    const null_pkg_res = env.findModuleByQualifiedNameInPackage(null, "dep.RealModule");
    try std.testing.expect(null_pkg_res == null);
}
