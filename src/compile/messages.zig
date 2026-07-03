//! Message types for the actor-based compilation model.
//!
//! This module defines the messages passed between the coordinator and worker threads.
//! The actor model eliminates races by design:
//! - Workers receive immutable inputs via tasks
//! - Workers send results back via a channel
//! - Only the coordinator mutates state

const std = @import("std");
const can = @import("can");
const check = @import("check");
const parse = @import("parse");
const reporting = @import("reporting");
const watch_inputs = @import("watch_inputs.zig");

const ModuleEnv = can.ModuleEnv;
const CheckedArtifact = check.CheckedArtifact;
const Report = reporting.Report;
const AST = parse.AST;
const Allocator = std.mem.Allocator;

/// Module identifier - index into the modules list
pub const ModuleId = u32;

/// Information about a discovered local import during canonicalization
pub const DiscoveredLocalImport = struct {
    /// The module name (e.g., "Foo")
    module_name: []const u8,
    /// The resolved filesystem path
    path: []const u8,
};

/// Information about a discovered external import during canonicalization
pub const DiscoveredExternalImport = struct {
    /// The qualified import name (e.g., "pf.Stdout")
    import_name: []const u8,
};

/// Ready imported module data passed into canonicalization.
pub const CanonicalizeImport = struct {
    /// The direct import name for canonicalization lookup
    import_name: []const u8,
    /// The fully-ready semantic env for this import
    module_env: *const ModuleEnv,
};

/// Information about detected import cycles
pub const CycleInfo = struct {
    /// The module that caused the cycle
    module_id: ModuleId,
    /// The module name that was being imported when cycle was detected
    import_name: []const u8,
    /// Path of module IDs forming the cycle (if computed)
    cycle_path: ?[]const ModuleId,
};

/// Task to parse a module source file
pub const ParseTask = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier within the package
    module_id: ModuleId,
    /// Human-readable module name
    module_name: []const u8,
    /// Filesystem path to the .roc file
    path: []const u8,
    /// Source-relative import base directory. Distinct from `dirname(path)` when
    /// the module is staged elsewhere (e.g. a default app written to a temp dir),
    /// so sibling imports resolve against the user's original directory.
    source_dir: []const u8,
    /// Compiler role for this source module
    module_role: ModuleEnv.ModuleRole,
    /// Dependency depth from root
    depth: u32,
};

/// Task to canonicalize a parsed module
pub const CanonicalizeTask = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier within the package
    module_id: ModuleId,
    /// Human-readable module name
    module_name: []const u8,
    /// Filesystem path (for diagnostics)
    path: []const u8,
    /// Source-relative import base directory.
    source_dir: []const u8,
    /// Dependency depth
    depth: u32,
    /// Module environment (ownership transferred from coordinator)
    module_env: *ModuleEnv,
    /// Cached AST from parsing (ownership transferred)
    cached_ast: *AST,
    /// Real imported semantic envs available to canonicalization
    imported_modules: []const CanonicalizeImport,
};

/// Task to type-check a canonicalized module
pub const TypeCheckTask = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier within the package
    module_id: ModuleId,
    /// Human-readable module name
    module_name: []const u8,
    /// Filesystem path (for diagnostics)
    path: []const u8,
    /// Module environment (ownership transferred)
    module_env: *ModuleEnv,
    /// Imported module environments (read-only pointers to completed modules)
    imported_envs: []const *ModuleEnv,
    /// Published checked artifact keys for direct imports, keyed by typed-CIR module index
    imported_artifacts: []const CheckedArtifact.PublishImportArtifact,
    /// Published checked artifacts currently available for exact-key lookup during checking finalization
    available_artifacts: []const CheckedArtifact.ImportedModuleView,
    /// Platform requirement surface for app-root checking, carrying both the
    /// checker's input and the cache-identity component so the two can never
    /// be set independently.
    platform_requirements: ?PlatformRequirementSurface = null,
    /// Additional checked roots requested by package-level metadata.
    explicit_roots: []const CheckedArtifact.ExplicitRootRequestInput,
};

/// The platform root's requirement surface, borrowed from its completed
/// type check: the checked platform ModuleEnv (stable once the module is
/// Done), the cache-identity context derived from its published artifact,
/// and the platform path for diagnostics. Plain borrowed data — nothing here
/// is owned, so copies are free and there is nothing to deinit.
pub const PlatformRequirementSurface = struct {
    env: *const ModuleEnv,
    context: CheckedArtifact.PlatformRequirementContextKey,
    path: []const u8,

    pub fn checkerInput(self: *const PlatformRequirementSurface) check.Check.PlatformRequirementInput {
        return .{
            .env = self.env,
            .path = self.path,
        };
    }
};

/// Task sent to workers - contains ALL inputs needed for the operation
pub const WorkerTask = union(enum) {
    /// Parse a source file into an AST
    parse: ParseTask,
    /// Canonicalize a parsed module into CIR
    canonicalize: CanonicalizeTask,
    /// Type-check a canonicalized module
    type_check: TypeCheckTask,

    pub fn getPackageName(self: WorkerTask) ?[]const u8 {
        return switch (self) {
            .parse => |t| t.package_name,
            .canonicalize => |t| t.package_name,
            .type_check => |t| t.package_name,
        };
    }

    pub fn getModuleId(self: WorkerTask) ?ModuleId {
        return switch (self) {
            .parse => |t| t.module_id,
            .canonicalize => |t| t.module_id,
            .type_check => |t| t.module_id,
        };
    }

    pub fn getModuleName(self: WorkerTask) ?[]const u8 {
        return switch (self) {
            .parse => |t| t.module_name,
            .canonicalize => |t| t.module_name,
            .type_check => |t| t.module_name,
        };
    }
};

/// Result of successfully parsing a module
pub const ParsedResult = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// Raw source file state consumed before line-ending normalization, when requested.
    source_file_state: ?watch_inputs.State,
    /// The parsed module environment (ownership returned to coordinator)
    module_env: *ModuleEnv,
    /// Cached AST for reuse in canonicalization (ownership returned)
    cached_ast: *AST,
    /// Discovered local imports (within the same package)
    discovered_local_imports: std.ArrayList(DiscoveredLocalImport),
    /// Discovered external imports (cross-package qualified imports)
    discovered_external_imports: std.ArrayList(DiscoveredExternalImport),
    /// Any reports generated during parsing
    reports: std.ArrayList(Report),
    /// Timing: nanoseconds spent parsing
    parse_ns: u64,
};

/// Result of successfully canonicalizing a module
pub const CanonicalizedResult = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// The canonicalized module environment (ownership returned)
    module_env: *ModuleEnv,
    /// Discovered local imports (within the same package)
    discovered_local_imports: std.ArrayList(DiscoveredLocalImport),
    /// Discovered external imports (cross-package qualified imports)
    discovered_external_imports: std.ArrayList(DiscoveredExternalImport),
    /// Any reports generated during canonicalization
    reports: std.ArrayList(Report),
    /// Timing: nanoseconds spent canonicalizing
    canonicalize_ns: u64,
    /// Timing: nanoseconds spent on diagnostics
    canonicalize_diagnostics_ns: u64,
};

/// Result of successfully type-checking a module
pub const OwnedSemanticModuleData = struct {
    module_env: *ModuleEnv,
    checked_artifact: ?CheckedArtifact.CheckedModuleArtifact = null,
    user_errors_allow_lowering: bool = false,

    pub fn deinit(self: *OwnedSemanticModuleData) void {
        if (self.checked_artifact) |*artifact| artifact.deinit(artifact.canonical_names.allocator);
    }
};

/// Result of successfully type-checking a module
pub const TypeCheckedResult = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// The type-checked semantic module data (ownership returned)
    semantic: *OwnedSemanticModuleData,
    /// Any reports generated during type checking
    reports: std.ArrayList(Report),
    /// Timing: nanoseconds spent type checking
    type_check_ns: u64,
    /// Timing: nanoseconds spent on diagnostics
    check_diagnostics_ns: u64,
};

/// Result when parsing fails (but we still return partial info)
pub const ParseFailure = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// Raw source file state observed before parsing failed, when requested.
    source_file_state: ?watch_inputs.State,
    /// Error reports explaining the failure
    reports: std.ArrayList(Report),
    /// Partial module env if available (for error recovery)
    partial_env: ?*ModuleEnv,
};

/// Result when a non-parsing compilation stage cannot continue safely.
pub const CompileFailure = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// Error reports explaining the failure
    reports: std.ArrayList(Report),
    /// Partial module env if available. The coordinator takes ownership.
    partial_env: ?*ModuleEnv,
};

/// Result when an import cycle is detected during canonicalization
pub const CycleDetected = struct {
    /// Package where the cycle was detected
    package_name: []const u8,
    /// Module that detected the cycle
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// Information about the cycle
    cycle_info: CycleInfo,
    /// Error reports for the cycle
    reports: std.ArrayList(Report),
    /// Module environment (ownership returned even on cycle)
    module_env: *ModuleEnv,
};

/// Result when a worker stage ran out of memory. Carries just enough identity
/// to report which module was being processed; the coordinator turns this into
/// `error.OutOfMemory` and aborts the whole compilation rather than letting the
/// failure masquerade as an ordinary stage failure.
pub const WorkerOom = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
};

/// Result sent from workers - contains ALL outputs from the operation
pub const WorkerResult = union(enum) {
    /// Module was successfully parsed
    parsed: ParsedResult,
    /// Module was successfully canonicalized
    canonicalized: CanonicalizedResult,
    /// Module was successfully type-checked
    type_checked: TypeCheckedResult,
    /// Parsing failed
    parse_failed: ParseFailure,
    /// A later compilation stage failed before producing explicit facts
    compile_failed: CompileFailure,
    /// Import cycle was detected
    cycle_detected: CycleDetected,
    /// A worker stage ran out of memory
    worker_oom: WorkerOom,

    pub fn getPackageName(self: WorkerResult) []const u8 {
        return switch (self) {
            .parsed => |r| r.package_name,
            .canonicalized => |r| r.package_name,
            .type_checked => |r| r.package_name,
            .parse_failed => |r| r.package_name,
            .compile_failed => |r| r.package_name,
            .cycle_detected => |r| r.package_name,
            .worker_oom => |r| r.package_name,
        };
    }

    pub fn getModuleId(self: WorkerResult) ModuleId {
        return switch (self) {
            .parsed => |r| r.module_id,
            .canonicalized => |r| r.module_id,
            .type_checked => |r| r.module_id,
            .parse_failed => |r| r.module_id,
            .compile_failed => |r| r.module_id,
            .cycle_detected => |r| r.module_id,
            .worker_oom => |r| r.module_id,
        };
    }

    pub fn getModuleName(self: WorkerResult) []const u8 {
        return switch (self) {
            .parsed => |r| r.module_name,
            .canonicalized => |r| r.module_name,
            .type_checked => |r| r.module_name,
            .parse_failed => |r| r.module_name,
            .compile_failed => |r| r.module_name,
            .cycle_detected => |r| r.module_name,
            .worker_oom => |r| r.module_name,
        };
    }

    /// Free any owned memory in the result
    pub fn deinit(self: *WorkerResult, gpa: Allocator) void {
        switch (self.*) {
            .parsed => |*r| {
                for (r.discovered_local_imports.items) |imp| {
                    gpa.free(imp.module_name);
                    gpa.free(imp.path);
                }
                r.discovered_local_imports.deinit(gpa);
                for (r.discovered_external_imports.items) |imp| {
                    gpa.free(imp.import_name);
                }
                r.discovered_external_imports.deinit(gpa);
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .canonicalized => |*r| {
                for (r.discovered_local_imports.items) |imp| {
                    gpa.free(imp.module_name);
                    gpa.free(imp.path);
                }
                r.discovered_local_imports.deinit(gpa);
                for (r.discovered_external_imports.items) |imp| {
                    gpa.free(imp.import_name);
                }
                r.discovered_external_imports.deinit(gpa);
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .type_checked => |*r| {
                r.semantic.deinit();
                gpa.destroy(r.semantic);
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .parse_failed => |*r| {
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .compile_failed => |*r| {
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .cycle_detected => |*r| {
                if (r.cycle_info.cycle_path) |path| gpa.free(path);
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .worker_oom => {},
        }
    }
};

// Compile-time size assertions to catch unexpected growth of message types.
// These types are copied between threads, so keeping them small is important
// for performance. If you need to add fields that would exceed these limits,
// consider using pointers instead.
comptime {
    const max_message_size = 512;
    if (@sizeOf(WorkerTask) > max_message_size) {
        @compileError("WorkerTask too large for efficient thread-safe copy");
    }
    if (@sizeOf(WorkerResult) > max_message_size) {
        @compileError("WorkerResult too large for efficient thread-safe copy");
    }
}

test "WorkerTask accessors" {
    const task = WorkerTask{
        .parse = .{
            .package_name = "app",
            .module_id = 0,
            .module_name = "Main",
            .path = "/path/to/Main.roc",
            .source_dir = "/path/to",
            .depth = 0,
            .module_role = .user,
        },
    };

    try std.testing.expectEqualStrings("app", task.getPackageName().?);
    try std.testing.expectEqual(@as(ModuleId, 0), task.getModuleId().?);
    try std.testing.expectEqualStrings("Main", task.getModuleName().?);
}

test "WorkerResult accessors" {
    const reports = std.ArrayList(Report).empty;

    const result = WorkerResult{
        .parsed = .{
            .package_name = "app",
            .module_id = 1,
            .module_name = "Foo",
            .path = "/path/to/Foo.roc",
            .source_file_state = .{ .hash = [_]u8{0} ** 32 },
            .module_env = undefined,
            .cached_ast = undefined,
            .discovered_local_imports = std.ArrayList(DiscoveredLocalImport).empty,
            .discovered_external_imports = std.ArrayList(DiscoveredExternalImport).empty,
            .reports = reports,
            .parse_ns = 1000,
        },
    };

    try std.testing.expectEqualStrings("app", result.getPackageName());
    try std.testing.expectEqual(@as(ModuleId, 1), result.getModuleId());
    try std.testing.expectEqualStrings("Foo", result.getModuleName());
}
