//! Global package version resolution.
//!
//! Packages downloaded from URLs are identified by their url id (the URL path
//! between the scheme and the version segment) plus their major version;
//! different major versions of the same url id are entirely separate packages
//! for solving purposes. Within one (url id, major) group, the whole build
//! uses the single highest minor.patch version that the final dependency
//! graph mentions. "Final" means the fixed point of: follow only chosen
//! winners' dependency edges, choosing each group's maximum mentioned
//! version. Versions mentioned only by packages that lost solving are
//! retracted, as though they had never been mentioned (matching what a
//! lockfile would record). Versionless URLs do not participate in solving;
//! each one is its own package keyed by its full URL.
//!
//! Apps pin exactly: if the final graph mentions a higher version of a group
//! than the app's own header declares for that group, that is an error (with
//! the full dependency chain reported) rather than a silent upgrade.
//!
//! Resolution is driven in rounds: walk the graph from the root following the
//! current choices, download whatever the walk needed but the knowledge base
//! lacks (in parallel), and repeat until the choices are stable. Downloads of
//! versions that end up retracted are expected and harmless. Because
//! retraction makes the fixed-point equation non-monotone, a (pathological)
//! graph can have no stable solution; the solver detects the resulting
//! oscillation and reports it as an error instead of looping.
//!
//! Each extracted bundle gets a sidecar file next to its extraction directory
//! in the cache recording its kind, declared dependencies, and content size.
//! Bundles are content-addressed and immutable, so sidecars never need
//! invalidation, and warm-cache resolution touches no network and parses no
//! Roc headers. Sidecars record only immutable facts about a bundle - never
//! resolution outcomes, which are global and recomputed every build.
//!
//! As a defense against decompression bombs, each package bundle's expanded
//! size is limited (platforms are exempt, since an app declares exactly one
//! platform on purpose), and the combined content size attributable to any
//! one of the root's direct dependencies is limited. Both limits count
//! already-cached packages, so deleting the cache never changes whether a
//! dependency graph is accepted.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const CoreCtx = @import("ctx").CoreCtx;
const threading = @import("threading.zig");

const Allocator = std.mem.Allocator;
const Version = base.url.Version;
const ModuleEnv = can.ModuleEnv;

const is_freestanding = threading.is_freestanding;

/// Maximum number of downloads in flight at once.
const MAX_CONCURRENT_DOWNLOADS: usize = 8;

/// Limits applied during resolution.
pub const Config = struct {
    /// Maximum decompressed size for any single package bundle, in bytes.
    /// Null means unlimited. Platform bundles are always exempt.
    max_package_expanded_bytes: ?u64 = default_max_package_expanded_bytes,
    /// Maximum combined content size attributable to any single direct
    /// dependency of the root, in bytes. Null means unlimited.
    max_transitive_expanded_bytes: ?u64 = default_max_transitive_expanded_bytes,

    pub const default_max_package_expanded_bytes: u64 = 10 * 1024 * 1024;
    pub const default_max_transitive_expanded_bytes: u64 = 100 * 1024 * 1024;
};

/// The kind of module header a package's root file has.
pub const HeaderKind = enum {
    app,
    package,
    platform,
    /// A plain module/hosted/type_module header: valid as a file, but carries
    /// no package dependencies.
    module,
};

/// One dependency declared in a package's header.
pub const ScannedDep = struct {
    alias: []const u8,
    spec: []const u8,
    /// True only for the platform entry of an app header.
    is_platform: bool,
};

/// Everything resolution needs to know about one package's contents.
pub const FetchedPackage = struct {
    kind: HeaderKind,
    /// Absolute path of the package's root .roc file.
    root_file: []const u8,
    /// Absolute path of the directory containing the root file.
    root_dir: []const u8,
    /// Hash of the normalized root source bytes consumed while scanning the header.
    root_source_hash: [32]u8,
    /// Combined size of the bundle's extracted files in bytes (0 for local
    /// packages, which are not downloaded and not size-limited).
    content_bytes: u64,
    deps: []const ScannedDep,
};

/// Errors a fetcher can produce for a single package.
pub const FetchError = error{
    DownloadFailed,
    ExpandedSizeLimitExceeded,
    FileNotFound,
    HeaderParseFailed,
    OutOfMemory,
    Unsupported,
};

/// Provides package contents to the resolver: downloading and scanning URL
/// bundles, and reading and scanning local package files. Injectable so the
/// solver can be tested without any filesystem or network.
pub const Fetcher = struct {
    ctx: ?*anyopaque,
    /// Fetch the bundle for `url` (whose trailing hash segment is `hash`),
    /// allocating all returned strings with `allocator`. Must be safe to call
    /// from multiple threads at once. `max_expanded_bytes` is the per-bundle
    /// decompression limit to enforce (null for exempt bundles).
    fetchUrlFn: *const fn (ctx: ?*anyopaque, allocator: Allocator, url: []const u8, hash: []const u8, max_expanded_bytes: ?u64) FetchError!FetchedPackage,
    /// Load and scan the local package rooted at `root_file_abs`.
    loadLocalFn: *const fn (ctx: ?*anyopaque, allocator: Allocator, root_file_abs: []const u8) FetchError!FetchedPackage,
};

/// One rendered resolution problem. Messages are owned by the resolver and
/// live until the resolver is deinitialized.
pub const Diagnostic = struct {
    title: []const u8,
    message: []const u8,
};

/// The result of successful resolution: every package in the final graph,
/// with each package's shorthand aliases resolved to package indices.
/// Owns all of its strings via an internal arena.
pub const Resolved = struct {
    arena: *std.heap.ArenaAllocator,
    /// Index 0 is always the root package.
    packages: []Package,

    pub const root_index: u32 = 0;

    pub const UrlInfo = struct {
        /// The full URL of the version this package resolved to.
        url: []const u8,
        url_id: base.url.UrlId,
        version: Version,
        hash: []const u8,
    };

    pub const Dep = struct {
        alias: []const u8,
        target: u32,
        /// True only for the root app's platform dependency.
        is_platform: bool,
        /// The version this package's header declared for the dependency,
        /// when it differs from what solving could pick. Compare with the
        /// target package's resolved version to detect bumps.
        declared_version: ?Version = null,
    };

    pub const Package = struct {
        kind: HeaderKind,
        /// Unique display identity: the full resolved URL for URL packages,
        /// or the absolute root file path for local packages.
        identity: []const u8,
        root_file: []const u8,
        root_dir: []const u8,
        root_source_hash: [32]u8,
        /// Present iff this package came from a URL.
        url: ?UrlInfo,
        deps: []Dep,
    };

    pub fn deinit(self: *Resolved) void {
        const child = self.arena.child_allocator;
        self.arena.deinit();
        child.destroy(self.arena);
    }
};

/// A note explaining that a package was compiled against a dependency
/// version it did not declare (because version solving picked a higher
/// minor.patch elsewhere in the graph).
pub const VersionBumpNote = struct {
    /// Identity of the package whose declared dependency was bumped.
    package_identity: []const u8,
    message: []const u8,
};

/// Collect a note for every package in the final graph that was compiled
/// against a dependency version it did not declare. In a well-behaved
/// ecosystem these bumps are compatible, so the notes are only worth showing
/// when a package fails to compile, attached to the error itself. All
/// strings are allocated with `allocator`.
pub fn versionBumpNotes(resolved: *const Resolved, allocator: Allocator) Allocator.Error![]VersionBumpNote {
    var notes = std.ArrayListUnmanaged(VersionBumpNote).empty;
    for (resolved.packages) |package| {
        for (package.deps) |dep| {
            const declared = dep.declared_version orelse continue;
            const target = resolved.packages[dep.target];
            const resolved_url = target.url orelse continue;
            if (declared.eql(resolved_url.version)) continue;
            try notes.append(allocator, .{
                .package_identity = try allocator.dupe(u8, package.identity),
                .message = try std.fmt.allocPrint(
                    allocator,
                    "the package this error is in declares its dependency {s} as version {d}.{d}.{d}, " ++
                        "but version solving resolved {s} to {s} because something else in the " ++
                        "dependency graph mentions that higher version. " ++
                        "Minor version bumps are supposed to be backwards-compatible, but that is " ++
                        "a guideline and not an enforced invariant, and this particular minor " ++
                        "version bump may not have been backwards-compatible in practice.",
                    .{
                        dep.alias,
                        declared.major,
                        declared.minor,
                        declared.patch,
                        dep.alias,
                        resolved_url.url,
                    },
                ),
            });
        }
    }
    return notes.toOwnedSlice(allocator);
}

/// Sidecar metadata recorded next to each extracted bundle in the cache.
/// `format` guards against layout changes; unknown formats are regenerated
/// from the extracted bundle, never trusted.
pub const Sidecar = struct {
    format: u32,
    kind: []const u8,
    /// Decompressed size of the bundle's tar stream, recorded at extraction
    /// time. The per-package size limit is enforced against this on warm
    /// cache reads too, so having a package cached never changes whether it
    /// is accepted.
    expanded_bytes: u64,
    /// Combined size of the extracted files, which drives the transitive
    /// tally. Derivable from the extracted bundle alone, so cache deletion
    /// can never change the tally.
    content_bytes: u64,
    /// Hash of the normalized root source bytes consumed while scanning this bundle.
    root_source_hash: [32]u8,
    deps: []const SidecarDep,

    pub const SidecarDep = struct {
        alias: []const u8,
        spec: []const u8,
        is_platform: bool,
    };

    pub const current_format: u32 = 2;
};

const GroupChoice = struct {
    version: Version,
    url: []const u8,
    hash: []const u8,
};

const UrlTarget = struct {
    group: []const u8,
    version: Version,
    url: []const u8,
    hash: []const u8,
};

const Edge = struct {
    /// Group key of the declaring package, or null for the root.
    parent: ?[]const u8,
    alias: []const u8,
    spec: []const u8,
    is_platform: bool,
    target: Target,

    const Target = union(enum) {
        url: UrlTarget,
        /// Group key ("l@" ++ absolute root file path).
        local: []const u8,
        invalid: InvalidSpec,
    };

    const InvalidSpec = enum {
        unparsable_url,
        insecure_url,
        reserved_version,
    };
};

const ParentLink = struct {
    parent: ?[]const u8,
    alias: []const u8,
    spec: []const u8,
};

const Missing = struct {
    group: []const u8,
    url: []const u8,
    hash: []const u8,
    /// True if any edge requiring this download is the app's platform edge,
    /// which exempts the bundle from the per-package size limit.
    platform_exempt: bool,
};

const WalkResult = struct {
    candidates: std.StringHashMapUnmanaged(GroupChoice),
    edges: std.ArrayListUnmanaged(Edge),
    parents: std.StringHashMapUnmanaged(ParentLink),
    missing_urls: std.ArrayListUnmanaged(Missing),
    missing_locals: std.ArrayListUnmanaged([]const u8),
    /// Group keys in the order they were first followed (BFS order).
    followed: std.ArrayListUnmanaged([]const u8),
};

/// Resolves a root package's full dependency graph. One-shot: create, call
/// `resolve` once, read diagnostics if it failed, then deinit.
pub const Resolver = struct {
    gpa: Allocator,
    arena_state: std.heap.ArenaAllocator,
    fetcher: Fetcher,
    config: Config,

    /// Content hash (or full URL for hashless specs) -> fetched bundle.
    url_nodes: std.StringHashMapUnmanaged(FetchedPackage),
    /// Absolute root file path -> scanned local package.
    local_nodes: std.StringHashMapUnmanaged(FetchedPackage),

    /// "group\x00major.minor.patch" -> hash, across every round (including
    /// retracted edges), to detect the same version being served with two
    /// different content hashes.
    seen_version_hashes: std.StringHashMapUnmanaged([]const u8),
    /// Group key -> every hash any edge has ever pointed it at, across every
    /// round. Drives the transitive size tally, which deliberately counts
    /// retracted intermediates: it is a safety mechanism, and counting
    /// everything that resolution caused to enter the graph is what
    /// guarantees the round loop terminates.
    seen_group_hashes: std.StringHashMapUnmanaged(std.ArrayListUnmanaged([]const u8)),
    /// Group key -> local node path, for traversing local packages in the
    /// transitive tally.
    seen_group_locals: std.StringHashMapUnmanaged([]const u8),

    diagnostics: std.ArrayListUnmanaged(Diagnostic),

    pub fn init(gpa: Allocator, fetcher: Fetcher, config: Config) Resolver {
        return .{
            .gpa = gpa,
            .arena_state = std.heap.ArenaAllocator.init(gpa),
            .fetcher = fetcher,
            .config = config,
            .url_nodes = .{},
            .local_nodes = .{},
            .seen_version_hashes = .{},
            .seen_group_hashes = .{},
            .seen_group_locals = .{},
            .diagnostics = .empty,
        };
    }

    pub fn deinit(self: *Resolver) void {
        self.arena_state.deinit();
    }

    fn arena(self: *Resolver) Allocator {
        return self.arena_state.allocator();
    }

    fn addDiagnostic(self: *Resolver, title: []const u8, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
        const message = try std.fmt.allocPrint(self.arena(), fmt, args);
        try self.diagnostics.append(self.arena(), .{ .title = title, .message = message });
    }

    /// Resolve the graph rooted at `root_file_abs`. On error.ResolutionFailed,
    /// `diagnostics` holds at least one problem to report.
    pub fn resolve(self: *Resolver, root_file_abs: []const u8) error{ OutOfMemory, ResolutionFailed }!Resolved {
        const root_path = try self.arena().dupe(u8, root_file_abs);
        const root_node = self.fetcher.loadLocalFn(self.fetcher.ctx, self.arena(), root_path) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                try self.addDiagnostic("Invalid Package Header", "Could not load the header of {s}: {s}.", .{ root_path, @errorName(err) });
                return error.ResolutionFailed;
            },
        };
        try self.local_nodes.put(self.arena(), root_path, root_node);

        var chosen: std.StringHashMapUnmanaged(GroupChoice) = .{};
        var states_seen: std.StringHashMapUnmanaged(void) = .{};

        while (true) {
            var walk_result = try self.walk(root_node, &chosen);

            if (self.diagnostics.items.len > 0) return error.ResolutionFailed;

            if (walk_result.missing_urls.items.len > 0 or walk_result.missing_locals.items.len > 0) {
                try self.fetchMissing(walk_result.missing_urls.items, walk_result.missing_locals.items);
                if (self.diagnostics.items.len > 0) return error.ResolutionFailed;
                try self.checkTransitiveLimits(root_node);
                if (self.diagnostics.items.len > 0) return error.ResolutionFailed;
                continue;
            }

            if (chosenEquals(&chosen, &walk_result.candidates)) {
                return try self.finish(root_path, root_node, &walk_result);
            }

            const state_key = try self.serializeChoices(&walk_result.candidates);
            if (states_seen.contains(state_key)) {
                try self.reportNonConvergence(&walk_result.candidates, &chosen);
                return error.ResolutionFailed;
            }
            try states_seen.put(self.arena(), state_key, {});
            chosen = walk_result.candidates;
        }
    }

    fn reportNonConvergence(
        self: *Resolver,
        a: *const std.StringHashMapUnmanaged(GroupChoice),
        b: *const std.StringHashMapUnmanaged(GroupChoice),
    ) Allocator.Error!void {
        var details = std.ArrayListUnmanaged(u8).empty;
        var it = a.iterator();
        while (it.next()) |entry| {
            const prev = b.get(entry.key_ptr.*) orelse {
                try details.appendSlice(self.arena(), entry.value_ptr.url);
                try details.appendSlice(self.arena(), "\n");
                continue;
            };
            if (!prev.version.eql(entry.value_ptr.version)) {
                try details.appendSlice(self.arena(), entry.value_ptr.url);
                try details.appendSlice(self.arena(), "\n");
            }
        }
        try self.addDiagnostic(
            "Dependency Versions Do Not Converge",
            "This dependency graph has no stable version resolution: lower versions of some packages " ++
                "pull in dependencies that demand higher versions of those same packages, and vice versa, " ++
                "so version solving oscillates forever.\n\n" ++
                "The packages whose versions oscillate include:\n{s}\n" ++
                "You can fix this by depending directly on a specific version of one of these packages " ++
                "in your own header, which pins it.",
            .{details.items},
        );
    }

    fn serializeChoices(self: *Resolver, choices: *const std.StringHashMapUnmanaged(GroupChoice)) Allocator.Error![]const u8 {
        var keys = std.ArrayListUnmanaged([]const u8).empty;
        var it = choices.iterator();
        while (it.next()) |entry| {
            try keys.append(self.arena(), entry.key_ptr.*);
        }
        std.mem.sort([]const u8, keys.items, {}, stringLessThan);

        var out = std.ArrayListUnmanaged(u8).empty;
        for (keys.items) |key| {
            const choice = choices.get(key).?;
            const line = try std.fmt.allocPrint(self.arena(), "{s}\x00{d}.{d}.{d}\x00{s}\x01", .{
                key, choice.version.major, choice.version.minor, choice.version.patch, choice.hash,
            });
            try out.appendSlice(self.arena(), line);
        }
        return out.items;
    }

    fn walk(
        self: *Resolver,
        root_node: FetchedPackage,
        chosen: *const std.StringHashMapUnmanaged(GroupChoice),
    ) Allocator.Error!WalkResult {
        var result = WalkResult{
            .candidates = .{},
            .edges = .empty,
            .parents = .{},
            .missing_urls = .empty,
            .missing_locals = .empty,
            .followed = .empty,
        };

        const QueueItem = struct {
            group: ?[]const u8,
            node: FetchedPackage,
        };

        var visited: std.StringHashMapUnmanaged(void) = .{};
        var missing_url_groups: std.StringHashMapUnmanaged(usize) = .{};
        var missing_local_seen: std.StringHashMapUnmanaged(void) = .{};
        var queue = std.ArrayListUnmanaged(QueueItem).empty;
        try queue.append(self.arena(), .{ .group = null, .node = root_node });

        var queue_index: usize = 0;
        while (queue_index < queue.items.len) : (queue_index += 1) {
            const item = queue.items[queue_index];

            for (item.node.deps) |dep| {
                if (specIsUrlLike(dep.spec)) {
                    if (!base.url.isSafeUrl(dep.spec)) {
                        try result.edges.append(self.arena(), .{
                            .parent = item.group,
                            .alias = dep.alias,
                            .spec = dep.spec,
                            .is_platform = dep.is_platform,
                            .target = .{ .invalid = .insecure_url },
                        });
                        continue;
                    }
                    const parsed = base.url.parseUrlPath(dep.spec) catch |err| {
                        try result.edges.append(self.arena(), .{
                            .parent = item.group,
                            .alias = dep.alias,
                            .spec = dep.spec,
                            .is_platform = dep.is_platform,
                            .target = .{ .invalid = switch (err) {
                                error.InvalidVersion => .reserved_version,
                                else => .unparsable_url,
                            } },
                        });
                        continue;
                    };

                    const group = if (parsed.version.isPresent())
                        try std.fmt.allocPrint(self.arena(), "v{d}@{s}", .{ parsed.version.major, parsed.urlId(dep.spec) })
                    else
                        try std.fmt.allocPrint(self.arena(), "u@{s}", .{dep.spec});

                    const target = UrlTarget{
                        .group = group,
                        .version = parsed.version,
                        .url = dep.spec,
                        .hash = parsed.hash,
                    };

                    try result.edges.append(self.arena(), .{
                        .parent = item.group,
                        .alias = dep.alias,
                        .spec = dep.spec,
                        .is_platform = dep.is_platform,
                        .target = .{ .url = target },
                    });

                    try self.recordSeenUrlEdge(group, parsed.version, dep.spec, parsed.hash);
                    if (self.diagnostics.items.len > 0) continue;

                    // Update this group's candidate to the max version seen.
                    const gop = try result.candidates.getOrPut(self.arena(), group);
                    if (!gop.found_existing) {
                        gop.value_ptr.* = .{ .version = parsed.version, .url = dep.spec, .hash = parsed.hash };
                    } else if (parsed.version.isPresent() and
                        gop.value_ptr.version.orderWithinMajor(parsed.version) == .lt)
                    {
                        gop.value_ptr.* = .{ .version = parsed.version, .url = dep.spec, .hash = parsed.hash };
                    }

                    // Follow only the currently-chosen version of this group.
                    const follow = chosen.get(group) orelse continue;
                    if (visited.contains(group)) continue;
                    try visited.put(self.arena(), group, {});
                    try result.parents.put(self.arena(), group, .{
                        .parent = item.group,
                        .alias = dep.alias,
                        .spec = dep.spec,
                    });

                    if (self.url_nodes.get(follow.hash)) |node| {
                        try result.followed.append(self.arena(), group);
                        try queue.append(self.arena(), .{ .group = group, .node = node });
                    } else {
                        const missing_gop = try missing_url_groups.getOrPut(self.arena(), follow.hash);
                        if (!missing_gop.found_existing) {
                            missing_gop.value_ptr.* = result.missing_urls.items.len;
                            try result.missing_urls.append(self.arena(), .{
                                .group = group,
                                .url = follow.url,
                                .hash = follow.hash,
                                .platform_exempt = dep.is_platform,
                            });
                        } else if (dep.is_platform) {
                            result.missing_urls.items[missing_gop.value_ptr.*].platform_exempt = true;
                        }
                    }
                } else {
                    const abs = try self.resolveLocalPath(item.node.root_dir, dep.spec);
                    const group = try std.fmt.allocPrint(self.arena(), "l@{s}", .{abs});

                    try result.edges.append(self.arena(), .{
                        .parent = item.group,
                        .alias = dep.alias,
                        .spec = dep.spec,
                        .is_platform = dep.is_platform,
                        .target = .{ .local = group },
                    });

                    try self.seen_group_locals.put(self.arena(), group, abs);

                    if (!result.candidates.contains(group)) {
                        try result.candidates.put(self.arena(), group, .{
                            .version = Version.none,
                            .url = abs,
                            .hash = abs,
                        });
                    }

                    if (visited.contains(group)) continue;
                    try visited.put(self.arena(), group, {});
                    try result.parents.put(self.arena(), group, .{
                        .parent = item.group,
                        .alias = dep.alias,
                        .spec = dep.spec,
                    });

                    if (self.local_nodes.get(abs)) |node| {
                        try result.followed.append(self.arena(), group);
                        try queue.append(self.arena(), .{ .group = group, .node = node });
                    } else if (!missing_local_seen.contains(abs)) {
                        try missing_local_seen.put(self.arena(), abs, {});
                        try result.missing_locals.append(self.arena(), abs);
                    }
                }
            }
        }

        return result;
    }

    /// Local groups are auto-followed even when absent from `chosen`, because
    /// they have no versions to solve. URL groups become followable one round
    /// after first discovery (once they have a chosen version). Record an edge
    /// in the persistent cross-round maps used for hash-conflict detection
    /// and the transitive size tally.
    fn recordSeenUrlEdge(self: *Resolver, group: []const u8, version: Version, url: []const u8, hash: []const u8) Allocator.Error!void {
        const version_key = try std.fmt.allocPrint(self.arena(), "{s}\x00{d}.{d}.{d}", .{
            group, version.major, version.minor, version.patch,
        });
        const gop = try self.seen_version_hashes.getOrPut(self.arena(), version_key);
        if (!gop.found_existing) {
            gop.value_ptr.* = hash;
        } else if (!std.mem.eql(u8, gop.value_ptr.*, hash)) {
            try self.addDiagnostic(
                "Conflicting Package Contents",
                "The same package version is being served with two different content hashes:\n\n" ++
                    "    {s}\n\n" ++
                    "appears with hash {s} and also with hash {s}.\n\n" ++
                    "This is very suspicious! A given version of a package should never change. " ++
                    "The host serving this package may not be trustworthy.",
                .{ url, gop.value_ptr.*, hash },
            );
            return;
        }

        const hashes_gop = try self.seen_group_hashes.getOrPut(self.arena(), group);
        if (!hashes_gop.found_existing) {
            hashes_gop.value_ptr.* = .empty;
        }
        for (hashes_gop.value_ptr.items) |existing| {
            if (std.mem.eql(u8, existing, hash)) return;
        }
        try hashes_gop.value_ptr.append(self.arena(), hash);
    }

    fn resolveLocalPath(self: *Resolver, parent_dir: []const u8, spec: []const u8) Allocator.Error![]const u8 {
        return std.fs.path.resolve(self.arena(), &.{ parent_dir, spec });
    }

    fn fetchMissing(self: *Resolver, missing_urls: []const Missing, missing_locals: []const []const u8) Allocator.Error!void {
        for (missing_locals) |abs| {
            const node = self.fetcher.loadLocalFn(self.fetcher.ctx, self.arena(), abs) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    try self.addDiagnostic("Invalid Package Dependency", "Could not load the package at {s}: {s}.", .{ abs, @errorName(err) });
                    continue;
                },
            };
            try self.local_nodes.put(self.arena(), abs, node);
        }

        if (missing_urls.len == 0) return;

        const Task = struct {
            fetcher: Fetcher,
            missing: Missing,
            max_expanded_bytes: ?u64,
            task_arena: std.heap.ArenaAllocator,
            result: FetchError!FetchedPackage,

            fn run(task: *@This()) void {
                task.result = task.fetcher.fetchUrlFn(
                    task.fetcher.ctx,
                    task.task_arena.allocator(),
                    task.missing.url,
                    task.missing.hash,
                    task.max_expanded_bytes,
                );
            }
        };

        var tasks = try self.gpa.alloc(Task, missing_urls.len);
        defer self.gpa.free(tasks);
        for (tasks, missing_urls) |*task, missing| {
            task.* = .{
                .fetcher = self.fetcher,
                .missing = missing,
                .max_expanded_bytes = if (missing.platform_exempt) null else self.config.max_package_expanded_bytes,
                .task_arena = std.heap.ArenaAllocator.init(self.gpa),
                .result = error.DownloadFailed,
            };
        }
        defer for (tasks) |*task| task.task_arena.deinit();

        if (comptime is_freestanding) {
            for (tasks) |*task| task.run();
        } else if (tasks.len == 1) {
            tasks[0].run();
        } else {
            var next: usize = 0;
            while (next < tasks.len) {
                const batch_len = @min(MAX_CONCURRENT_DOWNLOADS, tasks.len - next);
                var threads: [MAX_CONCURRENT_DOWNLOADS]std.Thread = undefined;
                var spawned: usize = 0;
                for (tasks[next..][0..batch_len]) |*task| {
                    threads[spawned] = std.Thread.spawn(.{}, Task.run, .{task}) catch {
                        // Could not spawn (resource limits); run on this thread.
                        task.run();
                        continue;
                    };
                    spawned += 1;
                }
                for (threads[0..spawned]) |thread| thread.join();
                next += batch_len;
            }
        }

        for (tasks) |*task| {
            const fetched = task.result catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ExpandedSizeLimitExceeded => {
                    try self.addDiagnostic(
                        "Package Too Large",
                        "The package at\n\n    {s}\n\nexpands to more than the per-package limit of {d} bytes.\n\n" ++
                            "You can raise the limit with the --max-package-bytes flag, or stop depending on this package.",
                        .{ task.missing.url, self.config.max_package_expanded_bytes orelse 0 },
                    );
                    continue;
                },
                else => {
                    try self.addDiagnostic(
                        "Package Download Failed",
                        "Failed to download and extract this package:\n\n    {s}\n\nError: {s}.",
                        .{ task.missing.url, @errorName(err) },
                    );
                    continue;
                },
            };
            const copied = try copyFetchedPackage(self.arena(), fetched);
            const hash = try self.arena().dupe(u8, task.missing.hash);
            try self.url_nodes.put(self.arena(), hash, copied);
        }
    }

    /// Enforce the per-direct-dependency transitive size limit over every
    /// package any round has ever pulled into the graph (including retracted
    /// ones, and including already-cached ones).
    fn checkTransitiveLimits(self: *Resolver, root_node: FetchedPackage) Allocator.Error!void {
        const limit = self.config.max_transitive_expanded_bytes orelse return;

        for (root_node.deps) |dep| {
            const start_group = (try self.groupKeyForSpec(root_node.root_dir, dep.spec)) orelse continue;

            var seen_groups: std.StringHashMapUnmanaged(void) = .{};
            var seen_hashes: std.StringHashMapUnmanaged(void) = .{};
            var total: u64 = 0;

            var frontier = std.ArrayListUnmanaged([]const u8).empty;
            try frontier.append(self.arena(), start_group);
            try seen_groups.put(self.arena(), start_group, {});

            var frontier_index: usize = 0;
            while (frontier_index < frontier.items.len) : (frontier_index += 1) {
                const group = frontier.items[frontier_index];

                var nodes = std.ArrayListUnmanaged(FetchedPackage).empty;
                if (self.seen_group_hashes.get(group)) |hashes| {
                    for (hashes.items) |hash| {
                        const node = self.url_nodes.get(hash) orelse continue;
                        const hash_gop = try seen_hashes.getOrPut(self.arena(), hash);
                        if (!hash_gop.found_existing) {
                            total += node.content_bytes;
                        }
                        try nodes.append(self.arena(), node);
                    }
                }
                if (self.seen_group_locals.get(group)) |abs| {
                    if (self.local_nodes.get(abs)) |node| {
                        try nodes.append(self.arena(), node);
                    }
                }

                for (nodes.items) |node| {
                    for (node.deps) |child_dep| {
                        const child_group = (try self.groupKeyForSpec(node.root_dir, child_dep.spec)) orelse continue;
                        const group_gop = try seen_groups.getOrPut(self.arena(), child_group);
                        if (!group_gop.found_existing) {
                            try frontier.append(self.arena(), child_group);
                        }
                    }
                }
            }

            if (total > limit) {
                try self.addDiagnostic(
                    "Dependency Tree Too Large",
                    "Depending on\n\n    {s}\n\nhas pulled more than {d} bytes of packages into the build ({d} bytes so far).\n\n" ++
                        "You can raise the limit with the --max-transitive-bytes flag, or stop depending on this package.",
                    .{ dep.spec, limit, total },
                );
            }
        }
    }

    fn groupKeyForSpec(self: *Resolver, parent_dir: []const u8, spec: []const u8) Allocator.Error!?[]const u8 {
        if (specIsUrlLike(spec)) {
            if (!base.url.isSafeUrl(spec)) return null;
            const parsed = base.url.parseUrlPath(spec) catch return null;
            if (parsed.version.isPresent()) {
                return try std.fmt.allocPrint(self.arena(), "v{d}@{s}", .{ parsed.version.major, parsed.urlId(spec) });
            }
            return try std.fmt.allocPrint(self.arena(), "u@{s}", .{spec});
        }
        const abs = try self.resolveLocalPath(parent_dir, spec);
        return try std.fmt.allocPrint(self.arena(), "l@{s}", .{abs});
    }

    fn finish(
        self: *Resolver,
        root_path: []const u8,
        root_node: FetchedPackage,
        walk_result: *const WalkResult,
    ) error{ OutOfMemory, ResolutionFailed }!Resolved {
        // Report invalid specs and kind violations over the final graph only:
        // edges that exist solely in retracted versions never get this far.
        for (walk_result.edges.items) |edge| {
            switch (edge.target) {
                .invalid => |reason| {
                    const owner = try self.describeOwner(walk_result, edge.parent);
                    switch (reason) {
                        .insecure_url => try self.addDiagnostic(
                            "Insecure Package URL",
                            "{s} depends on this URL, which does not use https:\n\n    {s}\n\n" ++
                                "Package URLs must use https (or http to localhost, for testing).",
                            .{ owner, edge.spec },
                        ),
                        .reserved_version => try self.addDiagnostic(
                            "Invalid Package Version",
                            "{s} depends on this URL, which uses the reserved version 0.0.0:\n\n    {s}\n\n" ++
                                "The lowest publishable package version is 0.0.1.",
                            .{ owner, edge.spec },
                        ),
                        .unparsable_url => try self.addDiagnostic(
                            "Invalid Package URL",
                            "{s} depends on this URL, which could not be parsed as a package URL:\n\n    {s}.",
                            .{ owner, edge.spec },
                        ),
                    }
                },
                .url => |target| {
                    const choice = walk_result.candidates.get(target.group).?;
                    const node = self.url_nodes.get(choice.hash).?;
                    try self.checkEdgeKind(walk_result, edge, node.kind);
                },
                .local => |group| {
                    const abs = group[2..];
                    const node = self.local_nodes.get(abs).?;
                    try self.checkEdgeKind(walk_result, edge, node.kind);
                },
            }
        }
        if (self.diagnostics.items.len > 0) return error.ResolutionFailed;

        // App pinning: the root app's versioned URL deps are exact. Any edge
        // in the final graph mentioning a higher version is an error.
        if (root_node.kind == .app) {
            var pins: std.StringHashMapUnmanaged(UrlTarget) = .{};
            for (walk_result.edges.items) |edge| {
                if (edge.parent != null) continue;
                const target = switch (edge.target) {
                    .url => |t| t,
                    else => continue,
                };
                if (!target.version.isPresent()) continue;
                const gop = try pins.getOrPut(self.arena(), target.group);
                if (gop.found_existing) {
                    if (!gop.value_ptr.version.eql(target.version)) {
                        try self.addDiagnostic(
                            "Conflicting Package Versions",
                            "This app's header depends on two different versions of the same package:\n\n" ++
                                "    {s}\n    {s}\n\n" ++
                                "An app's dependency versions are exact, so it can only declare one version per package.",
                            .{ gop.value_ptr.url, target.url },
                        );
                    }
                } else {
                    gop.value_ptr.* = target;
                }
            }

            for (walk_result.edges.items) |edge| {
                if (edge.parent == null) continue;
                const target = switch (edge.target) {
                    .url => |t| t,
                    else => continue,
                };
                const pin = pins.get(target.group) orelse continue;
                if (pin.version.orderWithinMajor(target.version) == .lt) {
                    const chain = try self.describeChain(walk_result, edge.parent);
                    try self.addDiagnostic(
                        "Package Version Conflict",
                        "This app's header depends on exactly this package version:\n\n    {s}\n\n" ++
                            "but a dependency needs version {d}.{d}.{d} of it:\n\n{s}    which depends on {s}\n\n" ++
                            "An app's dependency versions are exact. Either upgrade the app's header to " ++
                            "version {d}.{d}.{d}, or stop depending on the package that needs it.",
                        .{
                            pin.url,
                            target.version.major,
                            target.version.minor,
                            target.version.patch,
                            chain,
                            target.url,
                            target.version.major,
                            target.version.minor,
                            target.version.patch,
                        },
                    );
                }
            }
            if (self.diagnostics.items.len > 0) return error.ResolutionFailed;
        }

        try self.checkCycles(walk_result);
        if (self.diagnostics.items.len > 0) return error.ResolutionFailed;

        // Build the Resolved structure with its own arena so it can outlive
        // the resolver.
        const result_arena = try self.gpa.create(std.heap.ArenaAllocator);
        result_arena.* = std.heap.ArenaAllocator.init(self.gpa);
        errdefer {
            result_arena.deinit();
            self.gpa.destroy(result_arena);
        }
        const out = result_arena.allocator();

        var group_to_index: std.StringHashMapUnmanaged(u32) = .{};
        defer group_to_index.deinit(self.gpa);

        var packages = std.ArrayListUnmanaged(Resolved.Package).empty;

        // Index 0: the root.
        try packages.append(out, .{
            .kind = root_node.kind,
            .identity = try out.dupe(u8, root_path),
            .root_file = try out.dupe(u8, root_node.root_file),
            .root_dir = try out.dupe(u8, root_node.root_dir),
            .root_source_hash = root_node.root_source_hash,
            .url = null,
            .deps = &.{},
        });
        const root_local_group = try std.fmt.allocPrint(self.arena(), "l@{s}", .{root_path});
        try group_to_index.put(self.gpa, root_local_group, Resolved.root_index);

        for (walk_result.followed.items) |group| {
            if (group_to_index.contains(group)) continue;
            const choice = walk_result.candidates.get(group).?;

            if (group[0] == 'l') {
                const abs = group[2..];
                const node = self.local_nodes.get(abs).?;
                try group_to_index.put(self.gpa, try self.arena().dupe(u8, group), @intCast(packages.items.len));
                try packages.append(out, .{
                    .kind = node.kind,
                    .identity = try out.dupe(u8, abs),
                    .root_file = try out.dupe(u8, node.root_file),
                    .root_dir = try out.dupe(u8, node.root_dir),
                    .root_source_hash = node.root_source_hash,
                    .url = null,
                    .deps = &.{},
                });
            } else {
                const node = self.url_nodes.get(choice.hash).?;
                const parsed = base.url.parseUrlPath(choice.url) catch unreachable;
                try group_to_index.put(self.gpa, try self.arena().dupe(u8, group), @intCast(packages.items.len));
                try packages.append(out, .{
                    .kind = node.kind,
                    .identity = try out.dupe(u8, choice.url),
                    .root_file = try out.dupe(u8, node.root_file),
                    .root_dir = try out.dupe(u8, node.root_dir),
                    .root_source_hash = node.root_source_hash,
                    .url = .{
                        .url = try out.dupe(u8, choice.url),
                        .url_id = parsed.url_id,
                        .version = parsed.version,
                        .hash = try out.dupe(u8, parsed.hash),
                    },
                    .deps = &.{},
                });
            }
        }

        // Wire dependency lists.
        var dep_lists = try self.gpa.alloc(std.ArrayListUnmanaged(Resolved.Dep), packages.items.len);
        defer self.gpa.free(dep_lists);
        for (dep_lists) |*list| list.* = .empty;

        for (walk_result.edges.items) |edge| {
            const parent_index: u32 = if (edge.parent) |parent_group|
                group_to_index.get(parent_group).?
            else
                Resolved.root_index;

            const target_group = switch (edge.target) {
                .url => |t| t.group,
                .local => |g| g,
                .invalid => unreachable, // would have failed above
            };
            const target_index = group_to_index.get(target_group).?;

            try dep_lists[parent_index].append(out, .{
                .alias = try out.dupe(u8, edge.alias),
                .target = target_index,
                .is_platform = edge.is_platform,
                .declared_version = switch (edge.target) {
                    .url => |t| if (t.version.isPresent()) t.version else null,
                    else => null,
                },
            });
        }

        for (packages.items, dep_lists) |*package, *list| {
            package.deps = list.items;
        }

        return .{
            .arena = result_arena,
            .packages = packages.items,
        };
    }

    fn checkEdgeKind(self: *Resolver, walk_result: *const WalkResult, edge: Edge, target_kind: HeaderKind) Allocator.Error!void {
        const owner = try self.describeOwner(walk_result, edge.parent);
        if (target_kind == .app) {
            try self.addDiagnostic(
                "Invalid Package Dependency",
                "{s} depends on {s}, which is an app. Packages may not depend on apps.",
                .{ owner, edge.spec },
            );
            return;
        }
        if (edge.is_platform) {
            if (target_kind != .platform) {
                try self.addDiagnostic(
                    "Invalid Platform",
                    "{s} declares {s} as its platform, but that package does not have a platform header.",
                    .{ owner, edge.spec },
                );
            }
            return;
        }
        if (target_kind == .platform) {
            try self.addDiagnostic(
                "Invalid Package Dependency",
                "{s} depends on {s}, which is a platform. Only apps may depend on platforms.",
                .{ owner, edge.spec },
            );
        }
    }

    fn describeOwner(self: *Resolver, walk_result: *const WalkResult, parent: ?[]const u8) Allocator.Error![]const u8 {
        const parent_group = parent orelse return "This project's header";
        const choice = walk_result.candidates.get(parent_group) orelse return "A dependency";
        return try std.fmt.allocPrint(self.arena(), "The package at {s}", .{choice.url});
    }

    /// Render the chain of dependencies from the root down to (and including)
    /// `group`, one per line, for error messages.
    fn describeChain(self: *Resolver, walk_result: *const WalkResult, group: ?[]const u8) Allocator.Error![]const u8 {
        var links = std.ArrayListUnmanaged([]const u8).empty;
        var current = group;
        while (current) |g| {
            const choice = walk_result.candidates.get(g) orelse break;
            try links.append(self.arena(), choice.url);
            const link = walk_result.parents.get(g) orelse break;
            current = link.parent;
        }

        var out = std.ArrayListUnmanaged(u8).empty;
        try out.appendSlice(self.arena(), "    this app depends on ");
        var i = links.items.len;
        while (i > 0) {
            i -= 1;
            try out.appendSlice(self.arena(), links.items[i]);
            try out.appendSlice(self.arena(), "\n");
            if (i > 0) {
                try out.appendSlice(self.arena(), "    which depends on ");
            }
        }
        return out.items;
    }

    fn checkCycles(self: *Resolver, walk_result: *const WalkResult) Allocator.Error!void {
        // Build adjacency over final groups (root = "" sentinel).
        var adjacency: std.StringHashMapUnmanaged(std.ArrayListUnmanaged([]const u8)) = .{};
        for (walk_result.edges.items) |edge| {
            const from = edge.parent orelse "";
            const to = switch (edge.target) {
                .url => |t| t.group,
                .local => |g| g,
                .invalid => continue,
            };
            const gop = try adjacency.getOrPut(self.arena(), from);
            if (!gop.found_existing) gop.value_ptr.* = .empty;
            try gop.value_ptr.append(self.arena(), to);
        }

        const Color = enum { white, gray, black };
        var colors: std.StringHashMapUnmanaged(Color) = .{};

        const Frame = struct {
            group: []const u8,
            next_child: usize,
        };
        var stack = std.ArrayListUnmanaged(Frame).empty;
        try stack.append(self.arena(), .{ .group = "", .next_child = 0 });
        try colors.put(self.arena(), "", .gray);

        while (stack.items.len > 0) {
            const frame = &stack.items[stack.items.len - 1];
            const children: []const []const u8 = if (adjacency.get(frame.group)) |list| list.items else &.{};
            if (frame.next_child >= children.len) {
                try colors.put(self.arena(), frame.group, .black);
                stack.items.len -= 1;
                continue;
            }
            const child = children[frame.next_child];
            frame.next_child += 1;
            const color = colors.get(child) orelse .white;
            switch (color) {
                .white => {
                    try colors.put(self.arena(), child, .gray);
                    try stack.append(self.arena(), .{ .group = child, .next_child = 0 });
                },
                .gray => {
                    // Found a cycle: everything on the stack from `child` up.
                    var cycle = std.ArrayListUnmanaged(u8).empty;
                    var in_cycle = false;
                    for (stack.items) |entry| {
                        if (std.mem.eql(u8, entry.group, child)) in_cycle = true;
                        if (!in_cycle) continue;
                        const choice = walk_result.candidates.get(entry.group) orelse continue;
                        try cycle.appendSlice(self.arena(), "    ");
                        try cycle.appendSlice(self.arena(), choice.url);
                        try cycle.appendSlice(self.arena(), "\n");
                    }
                    if (walk_result.candidates.get(child)) |choice| {
                        try cycle.appendSlice(self.arena(), "    ");
                        try cycle.appendSlice(self.arena(), choice.url);
                        try cycle.appendSlice(self.arena(), "\n");
                    }
                    try self.addDiagnostic(
                        "Package Cycle",
                        "These packages depend on each other in a cycle:\n\n{s}\nPackages cannot depend on each other in cycles.",
                        .{cycle.items},
                    );
                    return;
                },
                .black => {},
            }
        }
    }
};

fn specIsUrlLike(spec: []const u8) bool {
    return std.mem.find(u8, spec, "://") != null;
}

fn stringLessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

fn chosenEquals(
    a: *const std.StringHashMapUnmanaged(GroupChoice),
    b: *const std.StringHashMapUnmanaged(GroupChoice),
) bool {
    if (a.count() != b.count()) return false;
    var it = a.iterator();
    while (it.next()) |entry| {
        const other = b.get(entry.key_ptr.*) orelse return false;
        if (!entry.value_ptr.version.eql(other.version)) return false;
        if (!std.mem.eql(u8, entry.value_ptr.hash, other.hash)) return false;
    }
    return true;
}

fn copyFetchedPackage(allocator: Allocator, fetched: FetchedPackage) Allocator.Error!FetchedPackage {
    const deps = try allocator.alloc(ScannedDep, fetched.deps.len);
    for (deps, fetched.deps) |*dep, source| {
        dep.* = .{
            .alias = try allocator.dupe(u8, source.alias),
            .spec = try allocator.dupe(u8, source.spec),
            .is_platform = source.is_platform,
        };
    }
    return .{
        .kind = fetched.kind,
        .root_file = try allocator.dupe(u8, fetched.root_file),
        .root_dir = try allocator.dupe(u8, fetched.root_dir),
        .root_source_hash = fetched.root_source_hash,
        .content_bytes = fetched.content_bytes,
        .deps = deps,
    };
}

/// Scan a Roc source file's header for its kind and declared dependencies.
/// Allocates returned strings with `allocator`; uses `gpa` for scratch.
pub fn scanHeaderSource(
    allocator: Allocator,
    gpa: Allocator,
    root_file_abs: []const u8,
    src: []const u8,
) error{ OutOfMemory, HeaderParseFailed }!FetchedPackage {
    var env = try ModuleEnv.init(gpa, src);
    defer env.deinit();
    try env.common.calcLineStarts(gpa);

    const ast = try parse.header(gpa, &env.common);
    defer ast.deinit();

    const header = ast.store.getHeader(@enumFromInt(ast.root_node_idx));

    if (ast.parse_diagnostics.items.len > 0) {
        return error.HeaderParseFailed;
    }
    if (header == .malformed) {
        return error.HeaderParseFailed;
    }

    const header_end_offset = headerEndOffset(ast, header);
    for (ast.tokenize_diagnostics.items) |diagnostic| {
        if (diagnostic.region.start.offset < header_end_offset) {
            return error.HeaderParseFailed;
        }
    }

    var deps = std.ArrayListUnmanaged(ScannedDep).empty;

    const root_file = try allocator.dupe(u8, root_file_abs);
    const root_dir = try allocator.dupe(u8, std.fs.path.dirname(root_file_abs) orelse ".");

    const kind: HeaderKind = switch (header) {
        .app => |a| blk: {
            const platform_field = ast.store.getRecordField(a.platform_idx);
            if (platform_field.value) |value_expr| {
                const spec = (try stringFromExpr(allocator, ast, value_expr)) orelse return error.HeaderParseFailed;
                try deps.append(allocator, .{
                    .alias = try allocator.dupe(u8, ast.resolve(platform_field.name)),
                    .spec = spec,
                    .is_platform = true,
                });
            } else {
                return error.HeaderParseFailed;
            }
            // The packages collection includes the platform field, which is
            // already recorded above as the platform edge.
            try appendPackagesCollection(allocator, ast, a.packages, a.platform_idx, &deps);
            break :blk .app;
        },
        .package => |p| blk: {
            try appendPackagesCollection(allocator, ast, p.packages, null, &deps);
            break :blk .package;
        },
        .platform => |p| blk: {
            try appendPackagesCollection(allocator, ast, p.packages, null, &deps);
            break :blk .platform;
        },
        .module, .hosted, .type_module, .default_app => .module,
        .malformed => unreachable,
    };

    return .{
        .kind = kind,
        .root_file = root_file,
        .root_dir = root_dir,
        .root_source_hash = sha256Bytes(src),
        .content_bytes = 0,
        .deps = deps.items,
    };
}

fn sha256Bytes(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

fn headerEndOffset(ast: *const parse.AST, header: parse.AST.Header) u32 {
    const region = header.to_tokenized_region();
    if (region.end <= region.start) return 0;

    const token_count: u32 = @intCast(ast.tokens.tokens.len);
    if (token_count == 0) return 0;

    const last_token = @min(region.end - 1, token_count - 1);
    return ast.tokens.resolve(last_token).end.offset;
}

fn appendPackagesCollection(
    allocator: Allocator,
    ast: *parse.AST,
    packages: parse.AST.Collection.Idx,
    skip_field: ?parse.AST.RecordField.Idx,
    deps: *std.ArrayListUnmanaged(ScannedDep),
) error{ OutOfMemory, HeaderParseFailed }!void {
    const coll = ast.store.getCollection(packages);
    const fields = ast.store.recordFieldSlice(.{ .span = coll.span });
    for (fields) |idx| {
        if (skip_field) |skip| {
            if (idx == skip) continue;
        }
        const field = ast.store.getRecordField(idx);
        const value_expr = field.value orelse continue;
        const spec = (try stringFromExpr(allocator, ast, value_expr)) orelse return error.HeaderParseFailed;
        try deps.append(allocator, .{
            .alias = try allocator.dupe(u8, ast.resolve(field.name)),
            .spec = spec,
            .is_platform = false,
        });
    }
}

fn stringFromExpr(allocator: Allocator, ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) Allocator.Error!?[]const u8 {
    const expr = ast.store.getExpr(expr_idx);
    switch (expr) {
        .string => |s| {
            var buf = std.ArrayListUnmanaged(u8).empty;
            for (ast.store.exprSlice(s.parts)) |part_idx| {
                const part = ast.store.getExpr(part_idx);
                if (part == .string_part) {
                    try buf.appendSlice(allocator, ast.resolve(part.string_part.token));
                }
            }
            // Null bytes are invalid in both file paths and URLs.
            if (std.mem.findScalar(u8, buf.items, 0) != null) return null;
            return buf.items;
        },
        else => return null,
    }
}

/// The production fetcher: downloads bundles through the CoreCtx vtable into
/// the hash-addressed cache, scans headers, and maintains sidecar files so
/// warm-cache resolution never re-parses bundle headers.
pub const CtxFetcher = struct {
    fs: CoreCtx,
    gpa: Allocator,
    /// Directory holding one subdirectory per bundle hash, or null when
    /// downloads are unsupported (freestanding targets).
    cache_packages_dir: ?[]const u8,

    pub fn fetcher(self: *CtxFetcher) Fetcher {
        return .{
            .ctx = self,
            .fetchUrlFn = fetchUrlImpl,
            .loadLocalFn = loadLocalImpl,
        };
    }

    fn fetchUrlImpl(ctx: ?*anyopaque, allocator: Allocator, url: []const u8, hash: []const u8, max_expanded_bytes: ?u64) FetchError!FetchedPackage {
        const self: *CtxFetcher = @ptrCast(@alignCast(ctx.?));
        const cache_dir = self.cache_packages_dir orelse return error.Unsupported;

        const package_dir = try std.fs.path.join(allocator, &.{ cache_dir, hash });
        const sidecar_path = try std.fmt.allocPrint(allocator, "{s}.deps.json", .{package_dir});
        const root_file = try std.fs.path.join(allocator, &.{ package_dir, "main.roc" });

        // Warm path: a sidecar means the bundle was fully extracted and
        // scanned before. The size limit applies to cached bundles too, so
        // having a package cached never changes whether it is accepted.
        if (self.fs.fileExists(sidecar_path)) {
            if (self.readSidecar(allocator, sidecar_path, package_dir, root_file)) |cached| {
                if (max_expanded_bytes) |max| {
                    if (cached.expanded_bytes > max) return error.ExpandedSizeLimitExceeded;
                }
                return cached.fetched;
            }
            // Unreadable or outdated sidecar: regenerate it from the
            // extracted bundle below.
        }

        var expanded_bytes: ?u64 = null;

        if (!self.fs.fileExists(root_file)) {
            if (!self.fs.fileExists(package_dir)) {
                self.fs.makePath(cache_dir) catch return error.DownloadFailed;
                self.fs.createDir(package_dir) catch |err| switch (err) {
                    // Possibly a concurrent process created it; extraction
                    // below will sort out whether it is usable.
                    error.IoError => {},
                    error.OutOfMemory => return error.OutOfMemory,
                    else => return error.DownloadFailed,
                };
                expanded_bytes = self.fs.fetchUrl(self.gpa, url, package_dir, max_expanded_bytes) catch |err| {
                    self.fs.deleteTree(package_dir) catch {};
                    return switch (err) {
                        error.OutOfMemory => error.OutOfMemory,
                        error.ExpandedSizeLimitExceeded => error.ExpandedSizeLimitExceeded,
                        else => error.DownloadFailed,
                    };
                };
            }
            if (!self.fs.fileExists(root_file)) {
                // Bundles must have a main.roc entry point.
                return error.FileNotFound;
            }
        }

        const src = try self.readNormalizedSource(allocator, root_file);
        var scanned = try scanHeaderSource(allocator, self.gpa, root_file, src);
        scanned.content_bytes = try self.measureContentBytes(allocator, package_dir, hash);

        // When the bundle was already extracted but had no sidecar, the tar
        // stream size is gone; the content size is the derivable equivalent.
        const recorded_expanded = expanded_bytes orelse scanned.content_bytes;
        if (max_expanded_bytes) |max| {
            if (recorded_expanded > max) return error.ExpandedSizeLimitExceeded;
        }

        self.writeSidecar(allocator, sidecar_path, scanned, recorded_expanded) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // A missing sidecar only costs a rescan next build.
            else => {},
        };

        return scanned;
    }

    fn loadLocalImpl(ctx: ?*anyopaque, allocator: Allocator, root_file_abs: []const u8) FetchError!FetchedPackage {
        const self: *CtxFetcher = @ptrCast(@alignCast(ctx.?));
        const src = try self.readNormalizedSource(allocator, root_file_abs);
        return try scanHeaderSource(allocator, self.gpa, root_file_abs, src);
    }

    fn readNormalizedSource(self: *CtxFetcher, allocator: Allocator, root_file_abs: []const u8) FetchError![]u8 {
        var src = self.fs.readFile(root_file_abs, allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.FileNotFound,
        };
        src = base.source_utils.normalizeLineEndingsRealloc(allocator, src) catch return error.OutOfMemory;
        return src;
    }

    /// Sum the sizes of the bundle's extracted files. The cached archive
    /// itself ("<hash>.tar.zst") is excluded: it is the compressed input,
    /// not extracted content. Using content bytes (rather than the tar
    /// stream size) keeps the tally derivable from the extracted bundle
    /// alone, so cache deletion can never change it.
    fn measureContentBytes(self: *CtxFetcher, allocator: Allocator, package_dir: []const u8, hash: []const u8) error{OutOfMemory}!u64 {
        const archive_name = try std.fmt.allocPrint(allocator, "{s}.tar.zst", .{hash});
        const entries = self.fs.listDir(package_dir, allocator) catch return 0;
        var total: u64 = 0;
        for (entries) |entry| {
            if (std.mem.endsWith(u8, entry.path, archive_name)) continue;
            const info = self.fs.stat(entry.path) catch continue;
            if (info.kind == .file) total += info.size;
        }
        return total;
    }

    const CachedPackage = struct {
        fetched: FetchedPackage,
        expanded_bytes: u64,
    };

    fn readSidecar(self: *CtxFetcher, allocator: Allocator, sidecar_path: []const u8, package_dir: []const u8, root_file: []const u8) ?CachedPackage {
        const bytes = self.fs.readFile(sidecar_path, allocator) catch return null;
        const parsed = std.json.parseFromSlice(Sidecar, self.gpa, bytes, .{}) catch return null;
        defer parsed.deinit();
        if (parsed.value.format != Sidecar.current_format) return null;

        const kind = std.meta.stringToEnum(HeaderKind, parsed.value.kind) orelse return null;

        const deps = allocator.alloc(ScannedDep, parsed.value.deps.len) catch return null;
        for (deps, parsed.value.deps) |*dep, source| {
            dep.* = .{
                .alias = allocator.dupe(u8, source.alias) catch return null,
                .spec = allocator.dupe(u8, source.spec) catch return null,
                .is_platform = source.is_platform,
            };
        }

        return .{
            .fetched = .{
                .kind = kind,
                .root_file = root_file,
                .root_dir = allocator.dupe(u8, package_dir) catch return null,
                .root_source_hash = parsed.value.root_source_hash,
                .content_bytes = parsed.value.content_bytes,
                .deps = deps,
            },
            .expanded_bytes = parsed.value.expanded_bytes,
        };
    }

    fn writeSidecar(
        self: *CtxFetcher,
        allocator: Allocator,
        sidecar_path: []const u8,
        scanned: FetchedPackage,
        expanded_bytes: u64,
    ) (Allocator.Error || error{WriteFailed} || CoreCtx.WriteError || CoreCtx.RenameError)!void {
        const deps = try allocator.alloc(Sidecar.SidecarDep, scanned.deps.len);
        for (deps, scanned.deps) |*dep, source| {
            dep.* = .{ .alias = source.alias, .spec = source.spec, .is_platform = source.is_platform };
        }
        const sidecar = Sidecar{
            .format = Sidecar.current_format,
            .kind = @tagName(scanned.kind),
            .expanded_bytes = expanded_bytes,
            .content_bytes = scanned.content_bytes,
            .root_source_hash = scanned.root_source_hash,
            .deps = deps,
        };

        var buffer: std.Io.Writer.Allocating = .init(allocator);
        defer buffer.deinit();
        try std.json.Stringify.value(sidecar, .{}, &buffer.writer);
        const json_bytes = try buffer.toOwnedSlice();

        // Write via a temp file and rename so readers never see a partial
        // sidecar. Concurrent writers race benignly: the content is a pure
        // function of the (content-addressed) bundle, so any writer's file
        // is correct.
        const tmp_path = try std.fmt.allocPrint(allocator, "{s}.tmp", .{sidecar_path});
        try self.fs.writeFile(tmp_path, json_bytes);
        try self.fs.rename(tmp_path, sidecar_path);
    }
};

// --- Tests ---

test "scanHeaderSource ignores tokenizer diagnostics after the header" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const src =
        "app [main!] { pf: platform \"../platform/main.roc\" }\r\n" ++
        "\r\n" ++
        "multiline_str = |number|\r\n" ++
        "    \\\\Line 1\r\n" ++
        "    \\\\Line ${number.to_str()}\r\n";

    const fetched = try scanHeaderSource(gpa, gpa, "/tmp/root.roc", src);

    try std.testing.expectEqual(HeaderKind.app, fetched.kind);
    try std.testing.expectEqual(@as(usize, 1), fetched.deps.len);
    try std.testing.expectEqualStrings("pf", fetched.deps[0].alias);
    try std.testing.expectEqualStrings("../platform/main.roc", fetched.deps[0].spec);
    try std.testing.expect(fetched.deps[0].is_platform);
}

test "scanHeaderSource rejects malformed package headers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const src =
        "app [main!] { pf: platform }\n" ++
        "main! = |_| Ok({})\n";

    try std.testing.expectError(error.HeaderParseFailed, scanHeaderSource(gpa, gpa, "/tmp/root.roc", src));
}

test "scanHeaderSource rejects tokenizer diagnostics inside the header" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const src =
        "app [main!] { pf: platform \"../bad\rpath\" }\n" ++
        "main! = |_| Ok({})\n";

    try std.testing.expectError(error.HeaderParseFailed, scanHeaderSource(gpa, gpa, "/tmp/root.roc", src));
}

const TestRegistry = struct {
    /// url -> package template. Specs inside templates are themselves
    /// registry keys (for URLs) or registry local keys (for paths).
    urls: std.StringHashMap(TestPackage),
    locals: std.StringHashMap(TestPackage),

    const TestPackage = struct {
        kind: HeaderKind = .package,
        content_bytes: u64 = 1000,
        deps: []const ScannedDep = &.{},
    };

    fn init(gpa: Allocator) TestRegistry {
        return .{
            .urls = std.StringHashMap(TestPackage).init(gpa),
            .locals = std.StringHashMap(TestPackage).init(gpa),
        };
    }

    fn deinit(self: *TestRegistry) void {
        self.urls.deinit();
        self.locals.deinit();
    }

    fn fetcher(self: *TestRegistry) Fetcher {
        return .{
            .ctx = self,
            .fetchUrlFn = fetchUrlImpl,
            .loadLocalFn = loadLocalImpl,
        };
    }

    fn toFetched(allocator: Allocator, package: TestPackage, root_file: []const u8, max_expanded_bytes: ?u64) FetchError!FetchedPackage {
        if (max_expanded_bytes) |max| {
            if (package.content_bytes > max) return error.ExpandedSizeLimitExceeded;
        }
        const deps = try allocator.alloc(ScannedDep, package.deps.len);
        for (deps, package.deps) |*dep, source| {
            dep.* = .{
                .alias = try allocator.dupe(u8, source.alias),
                .spec = try allocator.dupe(u8, source.spec),
                .is_platform = source.is_platform,
            };
        }
        return .{
            .kind = package.kind,
            .root_file = try allocator.dupe(u8, root_file),
            .root_dir = try allocator.dupe(u8, std.fs.path.dirname(root_file) orelse "/"),
            .root_source_hash = sha256Bytes(root_file),
            .content_bytes = package.content_bytes,
            .deps = deps,
        };
    }

    fn fetchUrlImpl(ctx: ?*anyopaque, allocator: Allocator, url: []const u8, hash: []const u8, max_expanded_bytes: ?u64) FetchError!FetchedPackage {
        const self: *TestRegistry = @ptrCast(@alignCast(ctx.?));
        const package = self.urls.get(url) orelse return error.DownloadFailed;
        var root_file_buf: [256]u8 = undefined;
        const root_file = std.fmt.bufPrint(&root_file_buf, "/cache/{s}/main.roc", .{hash}) catch return error.DownloadFailed;
        return toFetched(allocator, package, root_file, max_expanded_bytes);
    }

    /// Test paths in the registry are written Unix-style; on Windows the
    /// resolver hands back natively-resolved paths (backslashes, possibly a
    /// drive prefix), so lookups normalize before comparing.
    fn normalizedLookupKey(buf: []u8, path: []const u8) []const u8 {
        var trimmed = path;
        if (trimmed.len >= 2 and trimmed[1] == ':') {
            trimmed = trimmed[2..];
        }
        const len = @min(buf.len, trimmed.len);
        for (trimmed[0..len], 0..) |byte, i| {
            buf[i] = if (byte == '\\') '/' else byte;
        }
        return buf[0..len];
    }

    fn loadLocalImpl(ctx: ?*anyopaque, allocator: Allocator, root_file_abs: []const u8) FetchError!FetchedPackage {
        const self: *TestRegistry = @ptrCast(@alignCast(ctx.?));
        var key_buf: [512]u8 = undefined;
        const key = normalizedLookupKey(&key_buf, root_file_abs);
        const package = self.locals.get(key) orelse return error.FileNotFound;
        return toFetched(allocator, package, root_file_abs, null);
    }
};

fn testFindPackage(resolved: *const Resolved, identity: []const u8) ?*const Resolved.Package {
    var needle_buf: [512]u8 = undefined;
    const needle = TestRegistry.normalizedLookupKey(&needle_buf, identity);
    for (resolved.packages) |*package| {
        var identity_buf: [512]u8 = undefined;
        const package_identity = TestRegistry.normalizedLookupKey(&identity_buf, package.identity);
        if (std.mem.eql(u8, package_identity, needle)) return package;
    }
    return null;
}

test "resolves a simple URL dependency" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_url = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = a_url, .is_platform = false }},
    });
    try registry.urls.put(a_url, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 2), resolved.packages.len);
    const root = resolved.packages[Resolved.root_index];
    try std.testing.expectEqual(@as(usize, 1), root.deps.len);
    try std.testing.expectEqualStrings("a", root.deps[0].alias);
    const a = resolved.packages[root.deps[0].target];
    try std.testing.expectEqualStrings(a_url, a.identity);
    try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 3 }, a.url.?.version);
}

test "selects the highest minor.patch within a major version" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_123 = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    const a_131 = "https://example.com/foo/a/1.3.1/hashA131.tar.zst";
    const b_url = "https://example.com/foo/b/2.0.0/hashB200.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_123, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_131, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_131, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // Both the root's "a" alias and b's "a" alias point at 1.3.1.
    try std.testing.expectEqual(@as(usize, 3), resolved.packages.len);
    const winner = testFindPackage(&resolved, a_131).?;
    try std.testing.expect(testFindPackage(&resolved, a_123) == null);

    const root = resolved.packages[Resolved.root_index];
    for (root.deps) |dep| {
        if (std.mem.eql(u8, dep.alias, "a")) {
            try std.testing.expectEqualStrings(winner.identity, resolved.packages[dep.target].identity);
        }
    }
}

test "different major versions coexist as separate packages" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_v1 = "https://example.com/foo/a/1.2.3/hashA1.tar.zst";
    const a_v2 = "https://example.com/foo/a/2.4.5/hashA2.tar.zst";
    const b_url = "https://example.com/foo/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_v1, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_v1, .{});
    try registry.urls.put(a_v2, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_v2, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 4), resolved.packages.len);
    try std.testing.expect(testFindPackage(&resolved, a_v1) != null);
    try std.testing.expect(testFindPackage(&resolved, a_v2) != null);
}

test "retraction: versions mentioned only by losers do not count" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // root -> q 1.2.0 and s 1.0.0; q 1.2.0 -> r 1.5.0; s -> q 1.4.0;
    // q 1.4.0 -> r 1.2.0. Once q resolves to 1.4.0, the mention of r 1.5.0
    // is retracted and r resolves to 1.2.0.
    const q_120 = "https://example.com/q/1.2.0/hashQ120.tar.zst";
    const q_140 = "https://example.com/q/1.4.0/hashQ140.tar.zst";
    const r_150 = "https://example.com/r/1.5.0/hashR150.tar.zst";
    const r_120 = "https://example.com/r/1.2.0/hashR120.tar.zst";
    const s_100 = "https://example.com/s/1.0.0/hashS100.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "q", .spec = q_120, .is_platform = false },
            .{ .alias = "s", .spec = s_100, .is_platform = false },
        },
    });
    try registry.urls.put(q_120, .{ .deps = &.{.{ .alias = "r", .spec = r_150, .is_platform = false }} });
    try registry.urls.put(q_140, .{ .deps = &.{.{ .alias = "r", .spec = r_120, .is_platform = false }} });
    try registry.urls.put(r_150, .{});
    try registry.urls.put(r_120, .{});
    try registry.urls.put(s_100, .{ .deps = &.{.{ .alias = "q", .spec = q_140, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expect(testFindPackage(&resolved, q_140) != null);
    try std.testing.expect(testFindPackage(&resolved, r_120) != null);
    try std.testing.expect(testFindPackage(&resolved, q_120) == null);
    try std.testing.expect(testFindPackage(&resolved, r_150) == null);
}

test "app pin violation reports the dependency chain" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const platform_url = "https://example.com/pf/1.0.0/hashPf.tar.zst";
    const a_123 = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    const a_124 = "https://example.com/foo/a/1.2.4/hashA124.tar.zst";
    const b_url = "https://example.com/foo/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "pf", .spec = platform_url, .is_platform = true },
            .{ .alias = "a", .spec = a_123, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(platform_url, .{ .kind = .platform });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_124, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_124, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqual(@as(usize, 1), resolver.diagnostics.items.len);
    const diagnostic = resolver.diagnostics.items[0];
    try std.testing.expectEqualStrings("Package Version Conflict", diagnostic.title);
    try std.testing.expect(std.mem.find(u8, diagnostic.message, a_123) != null);
    try std.testing.expect(std.mem.find(u8, diagnostic.message, b_url) != null);
    try std.testing.expect(std.mem.find(u8, diagnostic.message, "1.2.4") != null);
}

test "app pin is satisfied by lower transitive versions" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const platform_url = "https://example.com/pf/1.0.0/hashPf.tar.zst";
    const a_123 = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    const a_122 = "https://example.com/foo/a/1.2.2/hashA122.tar.zst";
    const b_url = "https://example.com/foo/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "pf", .spec = platform_url, .is_platform = true },
            .{ .alias = "a", .spec = a_123, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(platform_url, .{ .kind = .platform });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_122, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_122, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // Everything (including b's "a" alias) resolves to the pinned 1.2.3.
    try std.testing.expect(testFindPackage(&resolved, a_123) != null);
    try std.testing.expect(testFindPackage(&resolved, a_122) == null);
}

test "app declaring two versions of the same package is an error" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const platform_url = "https://example.com/pf/1.0.0/hashPf.tar.zst";
    const a_123 = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    const a_140 = "https://example.com/foo/a/1.4.0/hashA140.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "pf", .spec = platform_url, .is_platform = true },
            .{ .alias = "a1", .spec = a_123, .is_platform = false },
            .{ .alias = "a2", .spec = a_140, .is_platform = false },
        },
    });
    try registry.urls.put(platform_url, .{ .kind = .platform });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_140, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Conflicting Package Versions", resolver.diagnostics.items[0].title);
}

test "same version with two different hashes is an error" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_via_one = "https://example.com/foo/a/1.3.1/hashOne.tar.zst";
    const a_via_two = "https://example.com/foo/a/1.3.1/hashTwo.tar.zst";
    const b_url = "https://example.com/foo/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_via_one, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_via_one, .{});
    try registry.urls.put(a_via_two, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_via_two, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Conflicting Package Contents", resolver.diagnostics.items[0].title);
}

test "versionless URLs do not participate in solving" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // The same url id, hashless-versionless in two different bundles, plus a
    // versioned release of the same url id: all three coexist.
    const a_plain_one = "https://example.com/foo/a/hashOldOne.tar.zst";
    const a_plain_two = "https://example.com/foo/a/hashOldTwo.tar.zst";
    const a_versioned = "https://example.com/foo/a/1.2.3/hashNew.tar.zst";
    const b_url = "https://example.com/foo/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_plain_one, .is_platform = false },
            .{ .alias = "a_new", .spec = a_versioned, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_plain_one, .{});
    try registry.urls.put(a_plain_two, .{});
    try registry.urls.put(a_versioned, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_plain_two, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expect(testFindPackage(&resolved, a_plain_one) != null);
    try std.testing.expect(testFindPackage(&resolved, a_plain_two) != null);
    try std.testing.expect(testFindPackage(&resolved, a_versioned) != null);
}

test "oscillating graphs are reported instead of looping" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // root -> b 1.0.0; b 1.0.0 -> c 1.0.0; c 1.0.0 -> b 1.1.0; b 1.1.0 has no
    // deps. No stable solution exists: choosing b 1.1.0 retracts c, which
    // retracts the b 1.1.0 mention, which restores b 1.0.0, which restores c.
    const b_100 = "https://example.com/b/1.0.0/hashB100.tar.zst";
    const b_110 = "https://example.com/b/1.1.0/hashB110.tar.zst";
    const c_100 = "https://example.com/c/1.0.0/hashC100.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "b", .spec = b_100, .is_platform = false }},
    });
    try registry.urls.put(b_100, .{ .deps = &.{.{ .alias = "c", .spec = c_100, .is_platform = false }} });
    try registry.urls.put(b_110, .{});
    try registry.urls.put(c_100, .{ .deps = &.{.{ .alias = "b", .spec = b_110, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Dependency Versions Do Not Converge", resolver.diagnostics.items[0].title);
}

test "transitive size limit counts each direct dependency's reachable packages" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_url = "https://example.com/a/1.0.0/hashA.tar.zst";
    const b_url = "https://example.com/b/1.0.0/hashB.tar.zst";
    const big_url = "https://example.com/big/1.0.0/hashBig.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = a_url, .is_platform = false }},
    });
    try registry.urls.put(a_url, .{
        .content_bytes = 100,
        .deps = &.{.{ .alias = "b", .spec = b_url, .is_platform = false }},
    });
    try registry.urls.put(b_url, .{
        .content_bytes = 100,
        .deps = &.{.{ .alias = "big", .spec = big_url, .is_platform = false }},
    });
    try registry.urls.put(big_url, .{ .content_bytes = 5000 });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{
        .max_transitive_expanded_bytes = 4000,
    });
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Dependency Tree Too Large", resolver.diagnostics.items[0].title);
    try std.testing.expect(std.mem.find(u8, resolver.diagnostics.items[0].message, a_url) != null);
}

test "per-package size limit is enforced for packages but not platforms" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const platform_url = "https://example.com/pf/1.0.0/hashPf.tar.zst";
    const a_url = "https://example.com/a/1.0.0/hashA.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "pf", .spec = platform_url, .is_platform = true },
            .{ .alias = "a", .spec = a_url, .is_platform = false },
        },
    });
    // Both are bigger than the per-package limit; only the package errors.
    try registry.urls.put(platform_url, .{ .kind = .platform, .content_bytes = 9000 });
    try registry.urls.put(a_url, .{ .content_bytes = 9000 });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{
        .max_package_expanded_bytes = 5000,
        .max_transitive_expanded_bytes = null,
    });
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqual(@as(usize, 1), resolver.diagnostics.items.len);
    const diagnostic = resolver.diagnostics.items[0];
    try std.testing.expectEqualStrings("Package Too Large", diagnostic.title);
    try std.testing.expect(std.mem.find(u8, diagnostic.message, a_url) != null);
}

test "packages may not depend on platforms or apps" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const platform_url = "https://example.com/pf/1.0.0/hashPf.tar.zst";
    const app_url = "https://example.com/someapp/1.0.0/hashApp.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "pf", .spec = platform_url, .is_platform = false },
            .{ .alias = "other", .spec = app_url, .is_platform = false },
        },
    });
    try registry.urls.put(platform_url, .{ .kind = .platform });
    try registry.urls.put(app_url, .{ .kind = .app });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqual(@as(usize, 2), resolver.diagnostics.items.len);
}

test "local package cycles are reported" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = "a/main.roc", .is_platform = false }},
    });
    try registry.locals.put("/app/a/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "b", .spec = "../b/main.roc", .is_platform = false }},
    });
    try registry.locals.put("/app/b/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = "../a/main.roc", .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Package Cycle", resolver.diagnostics.items[0].title);
}

test "local packages mix with URL packages" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_url = "https://example.com/a/2.0.1/hashA.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "helper", .spec = "vendored/helper/main.roc", .is_platform = false }},
    });
    try registry.locals.put("/app/vendored/helper/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = a_url, .is_platform = false }},
    });
    try registry.urls.put(a_url, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expectEqual(@as(usize, 3), resolved.packages.len);
    const helper = testFindPackage(&resolved, "/app/vendored/helper/main.roc").?;
    try std.testing.expectEqual(@as(usize, 1), helper.deps.len);
    try std.testing.expectEqualStrings(a_url, resolved.packages[helper.deps[0].target].identity);
}

test "pin violations report the full chain through deep indirect dependencies" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_100 = "https://example.com/a/1.0.0/hashA100.tar.zst";
    const a_101 = "https://example.com/a/1.0.1/hashA101.tar.zst";
    const b_url = "https://example.com/b/1.0.0/hashB.tar.zst";
    const c_url = "https://example.com/c/1.0.0/hashC.tar.zst";
    const d_url = "https://example.com/d/1.0.0/hashD.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "a", .spec = a_100, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_100, .{});
    try registry.urls.put(a_101, .{});
    try registry.urls.put(b_url, .{ .deps = &.{.{ .alias = "c", .spec = c_url, .is_platform = false }} });
    try registry.urls.put(c_url, .{ .deps = &.{.{ .alias = "d", .spec = d_url, .is_platform = false }} });
    try registry.urls.put(d_url, .{ .deps = &.{.{ .alias = "a", .spec = a_101, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    const diagnostic = resolver.diagnostics.items[0];
    try std.testing.expectEqualStrings("Package Version Conflict", diagnostic.title);
    // Every link of the indirect chain appears, in order, plus the
    // violating version itself.
    const b_at = std.mem.find(u8, diagnostic.message, b_url).?;
    const c_at = std.mem.find(u8, diagnostic.message, c_url).?;
    const d_at = std.mem.find(u8, diagnostic.message, d_url).?;
    try std.testing.expect(b_at < c_at);
    try std.testing.expect(c_at < d_at);
    try std.testing.expect(std.mem.find(u8, diagnostic.message, a_101) != null);
}

test "retraction prunes a loser's entire subtree without downloading it" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // q 1.2.0 pulls in r, which pulls in t — but s bumps q to 1.4.0 (which
    // has no deps) before q 1.2.0's subtree is ever followed, so r and t are
    // never even downloaded.
    const q_120 = "https://example.com/q/1.2.0/hashQ120.tar.zst";
    const q_140 = "https://example.com/q/1.4.0/hashQ140.tar.zst";
    const r_150 = "https://example.com/r/1.5.0/hashR150.tar.zst";
    const t_100 = "https://example.com/t/1.0.0/hashT100.tar.zst";
    const s_100 = "https://example.com/s/1.0.0/hashS100.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "q", .spec = q_120, .is_platform = false },
            .{ .alias = "s", .spec = s_100, .is_platform = false },
        },
    });
    try registry.urls.put(q_120, .{ .deps = &.{.{ .alias = "r", .spec = r_150, .is_platform = false }} });
    try registry.urls.put(q_140, .{});
    try registry.urls.put(r_150, .{ .deps = &.{.{ .alias = "t", .spec = t_100, .is_platform = false }} });
    try registry.urls.put(t_100, .{});
    try registry.urls.put(s_100, .{ .deps = &.{.{ .alias = "q", .spec = q_140, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expect(testFindPackage(&resolved, q_140) != null);
    try std.testing.expect(testFindPackage(&resolved, r_150) == null);
    try std.testing.expect(testFindPackage(&resolved, t_100) == null);
    // The subtree was retracted before ever being followed, so it was never
    // fetched at all.
    try std.testing.expect(!resolver.url_nodes.contains("hashR150"));
    try std.testing.expect(!resolver.url_nodes.contains("hashT100"));
}

test "an over-downloaded intermediate version is retracted and does not violate the app's pin" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // c 1.2.0 mentions a 1.3.0, briefly making it the chosen version of a
    // (so it gets downloaded). Then d bumps c to 1.5.0, which does not
    // mention a at all — retracting the 1.3.0 mention, restoring the app's
    // pinned 1.2.3, and reporting no conflict. The intermediate download is
    // intended waste.
    const a_123 = "https://example.com/a/1.2.3/hashA123.tar.zst";
    const a_130 = "https://example.com/a/1.3.0/hashA130.tar.zst";
    const b_url = "https://example.com/b/1.0.0/hashB.tar.zst";
    const c_120 = "https://example.com/c/1.2.0/hashC120.tar.zst";
    const c_150 = "https://example.com/c/1.5.0/hashC150.tar.zst";
    const d_url = "https://example.com/d/1.0.0/hashD.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "a", .spec = a_123, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_130, .{});
    try registry.urls.put(b_url, .{ .deps = &.{
        .{ .alias = "c", .spec = c_120, .is_platform = false },
        .{ .alias = "d", .spec = d_url, .is_platform = false },
    } });
    try registry.urls.put(c_120, .{ .deps = &.{.{ .alias = "a", .spec = a_130, .is_platform = false }} });
    try registry.urls.put(c_150, .{});
    try registry.urls.put(d_url, .{ .deps = &.{.{ .alias = "c", .spec = c_150, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expect(testFindPackage(&resolved, a_123) != null);
    try std.testing.expect(testFindPackage(&resolved, a_130) == null);
    try std.testing.expect(testFindPackage(&resolved, c_150) != null);
    try std.testing.expect(testFindPackage(&resolved, c_120) == null);
    // The losing version really was downloaded along the way.
    try std.testing.expect(resolver.url_nodes.contains("hashA130"));
}

test "a bumped winner's new dependencies join the graph" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // x 1.0.0 has no deps, but the winning x 1.1.0 introduces z.
    const x_100 = "https://example.com/x/1.0.0/hashX100.tar.zst";
    const x_110 = "https://example.com/x/1.1.0/hashX110.tar.zst";
    const y_url = "https://example.com/y/1.0.0/hashY.tar.zst";
    const z_url = "https://example.com/z/1.0.0/hashZ.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "x", .spec = x_100, .is_platform = false },
            .{ .alias = "y", .spec = y_url, .is_platform = false },
        },
    });
    try registry.urls.put(x_100, .{});
    try registry.urls.put(x_110, .{ .deps = &.{.{ .alias = "z", .spec = z_url, .is_platform = false }} });
    try registry.urls.put(y_url, .{ .deps = &.{.{ .alias = "x", .spec = x_110, .is_platform = false }} });
    try registry.urls.put(z_url, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    try std.testing.expect(testFindPackage(&resolved, x_110) != null);
    try std.testing.expect(testFindPackage(&resolved, z_url) != null);
    try std.testing.expect(testFindPackage(&resolved, x_100) == null);
}

test "diamond dependencies unify on a single package" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_url = "https://example.com/a/1.0.0/hashA.tar.zst";
    const b_url = "https://example.com/b/1.0.0/hashB.tar.zst";
    const c_11 = "https://example.com/c/1.1.0/hashC11.tar.zst";
    const c_12 = "https://example.com/c/1.2.0/hashC12.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_url, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_url, .{ .deps = &.{.{ .alias = "c", .spec = c_11, .is_platform = false }} });
    try registry.urls.put(b_url, .{ .deps = &.{.{ .alias = "c", .spec = c_12, .is_platform = false }} });
    try registry.urls.put(c_11, .{});
    try registry.urls.put(c_12, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // Exactly one c, and both sides of the diamond point at it.
    try std.testing.expectEqual(@as(usize, 4), resolved.packages.len);
    const a = testFindPackage(&resolved, a_url).?;
    const b = testFindPackage(&resolved, b_url).?;
    try std.testing.expectEqual(@as(usize, 1), a.deps.len);
    try std.testing.expectEqual(@as(usize, 1), b.deps.len);
    try std.testing.expectEqual(a.deps[0].target, b.deps[0].target);
    try std.testing.expectEqualStrings(c_12, resolved.packages[a.deps[0].target].identity);
}

test "app pins constrain only their own major version" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_v1 = "https://example.com/a/1.2.3/hashA1.tar.zst";
    const a_v2 = "https://example.com/a/2.5.0/hashA2.tar.zst";
    const b_url = "https://example.com/b/1.0.0/hashB.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "a", .spec = a_v1, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_v1, .{});
    try registry.urls.put(a_v2, .{});
    try registry.urls.put(b_url, .{ .deps = &.{.{ .alias = "a", .spec = a_v2, .is_platform = false }} });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // A different major version is a different package: no conflict, both
    // present.
    try std.testing.expect(testFindPackage(&resolved, a_v1) != null);
    try std.testing.expect(testFindPackage(&resolved, a_v2) != null);
}

test "an app may declare the identical version twice under different aliases" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_url = "https://example.com/a/1.2.3/hashA.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .app,
        .deps = &.{
            .{ .alias = "one", .spec = a_url, .is_platform = false },
            .{ .alias = "two", .spec = a_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_url, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // One package, two aliases pointing at it.
    try std.testing.expectEqual(@as(usize, 2), resolved.packages.len);
    const root = resolved.packages[Resolved.root_index];
    try std.testing.expectEqual(@as(usize, 2), root.deps.len);
    try std.testing.expectEqual(root.deps[0].target, root.deps[1].target);
}

test "transitive tally counts retracted downloads against every root that reached them" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    // big is pulled in by q 1.0.0 and downloaded before t bumps q to 1.1.0
    // (which would retract it). The tally deliberately keeps counting it —
    // it is a safety mechanism over everything resolution caused to enter
    // the graph — and counts it against both direct dependencies, since
    // both reach it.
    const q_100 = "https://example.com/q/1.0.0/hashQ100.tar.zst";
    const q_110 = "https://example.com/q/1.1.0/hashQ110.tar.zst";
    const s_url = "https://example.com/s/1.0.0/hashS.tar.zst";
    const t_url = "https://example.com/t/1.0.0/hashT.tar.zst";
    const big_url = "https://example.com/big/1.0.0/hashBig.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "q", .spec = q_100, .is_platform = false },
            .{ .alias = "s", .spec = s_url, .is_platform = false },
        },
    });
    try registry.urls.put(q_100, .{
        .content_bytes = 1000,
        .deps = &.{.{ .alias = "big", .spec = big_url, .is_platform = false }},
    });
    try registry.urls.put(q_110, .{ .content_bytes = 1000 });
    try registry.urls.put(s_url, .{
        .content_bytes = 1000,
        .deps = &.{.{ .alias = "t", .spec = t_url, .is_platform = false }},
    });
    try registry.urls.put(t_url, .{
        .content_bytes = 1000,
        .deps = &.{.{ .alias = "q", .spec = q_110, .is_platform = false }},
    });
    try registry.urls.put(big_url, .{ .content_bytes = 9000 });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{
        .max_transitive_expanded_bytes = 5000,
    });
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqual(@as(usize, 2), resolver.diagnostics.items.len);
    for (resolver.diagnostics.items) |diagnostic| {
        try std.testing.expectEqualStrings("Dependency Tree Too Large", diagnostic.title);
    }
    try std.testing.expect(std.mem.find(u8, resolver.diagnostics.items[0].message, q_100) != null);
    try std.testing.expect(std.mem.find(u8, resolver.diagnostics.items[1].message, s_url) != null);
}

test "mirrors of the same content are distinct packages sharing one download" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const mirror_one = "https://one.example.com/x/1.0.0/hashShared.tar.zst";
    const mirror_two = "https://two.example.com/x/1.0.0/hashShared.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "m1", .spec = mirror_one, .is_platform = false },
            .{ .alias = "m2", .spec = mirror_two, .is_platform = false },
        },
    });
    try registry.urls.put(mirror_one, .{});
    try registry.urls.put(mirror_two, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // Different url ids: two separate packages, even with identical hashes —
    // but the content was only fetched once and they share an extraction.
    try std.testing.expectEqual(@as(usize, 3), resolved.packages.len);
    const one = testFindPackage(&resolved, mirror_one).?;
    const two = testFindPackage(&resolved, mirror_two).?;
    try std.testing.expectEqualStrings(one.root_file, two.root_file);
}

test "version solving spans local packages bridging URL dependencies" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_10 = "https://example.com/a/1.0.0/hashA10.tar.zst";
    const a_11 = "https://example.com/a/1.1.0/hashA11.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_10, .is_platform = false },
            .{ .alias = "helper", .spec = "vendored/helper/main.roc", .is_platform = false },
        },
    });
    try registry.locals.put("/app/vendored/helper/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = a_11, .is_platform = false }},
    });
    try registry.urls.put(a_10, .{});
    try registry.urls.put(a_11, .{});

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // The local package's mention participates in solving like any other.
    try std.testing.expect(testFindPackage(&resolved, a_11) != null);
    try std.testing.expect(testFindPackage(&resolved, a_10) == null);
}

test "versionBumpNotes reports packages compiled against undeclared versions" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    const a_123 = "https://example.com/foo/a/1.2.3/hashA123.tar.zst";
    const a_131 = "https://example.com/foo/a/1.3.1/hashA131.tar.zst";
    const b_url = "https://example.com/foo/b/2.0.0/hashB200.tar.zst";

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{
            .{ .alias = "a", .spec = a_123, .is_platform = false },
            .{ .alias = "b", .spec = b_url, .is_platform = false },
        },
    });
    try registry.urls.put(a_123, .{});
    try registry.urls.put(a_131, .{});
    try registry.urls.put(b_url, .{
        .deps = &.{.{ .alias = "a", .spec = a_131, .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    var resolved = try resolver.resolve("/app/main.roc");
    defer resolved.deinit();

    // The root declared a 1.2.3 but the build uses 1.3.1; b declared the
    // winner, so only the root gets a note.
    var note_arena = std.heap.ArenaAllocator.init(gpa);
    defer note_arena.deinit();
    const notes = try versionBumpNotes(&resolved, note_arena.allocator());

    try std.testing.expectEqual(@as(usize, 1), notes.len);
    try std.testing.expectEqualStrings("/app/main.roc", notes[0].package_identity);
    try std.testing.expect(std.mem.find(u8, notes[0].message, "1.2.3") != null);
    try std.testing.expect(std.mem.find(u8, notes[0].message, a_131) != null);
}

test "insecure URLs are rejected" {
    const gpa = std.testing.allocator;
    var registry = TestRegistry.init(gpa);
    defer registry.deinit();

    try registry.locals.put("/app/main.roc", .{
        .kind = .package,
        .deps = &.{.{ .alias = "a", .spec = "http://example.com/a/1.0.0/hashA.tar.zst", .is_platform = false }},
    });

    var resolver = Resolver.init(gpa, registry.fetcher(), .{});
    defer resolver.deinit();

    try std.testing.expectError(error.ResolutionFailed, resolver.resolve("/app/main.roc"));
    try std.testing.expectEqualStrings("Insecure Package URL", resolver.diagnostics.items[0].title);
}
