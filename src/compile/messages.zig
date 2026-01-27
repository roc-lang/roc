//! Message types for the actor-based compilation model.
//!
//! This module defines the messages passed between the coordinator and worker threads.
//! The actor model eliminates races by design:
//! - Workers receive immutable inputs via tasks
//! - Workers send results back via a channel
//! - Only the coordinator mutates state

const std = @import("std");
const can = @import("can");
const parse = @import("parse");
const reporting = @import("reporting");
const cache_module = @import("cache_module.zig");
const cache_manager = @import("cache_manager.zig");

const ModuleEnv = can.ModuleEnv;
const CacheData = cache_module.CacheModule.CacheData;
const ImportInfo = cache_manager.ImportInfo;
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
    /// Dependency depth
    depth: u32,
    /// Module environment (ownership transferred from coordinator)
    module_env: *ModuleEnv,
    /// Cached AST from parsing (ownership transferred)
    cached_ast: *AST,
    /// Root directory for resolving local imports
    root_dir: []const u8,
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
};

/// Task sent to workers - contains ALL inputs needed for the operation
pub const WorkerTask = union(enum) {
    /// Parse a source file into an AST
    parse: ParseTask,
    /// Canonicalize a parsed module into CIR
    canonicalize: CanonicalizeTask,
    /// Type-check a canonicalized module
    type_check: TypeCheckTask,
    /// Signal worker to shut down
    shutdown: void,

    pub fn getPackageName(self: WorkerTask) ?[]const u8 {
        return switch (self) {
            .parse => |t| t.package_name,
            .canonicalize => |t| t.package_name,
            .type_check => |t| t.package_name,
            .shutdown => null,
        };
    }

    pub fn getModuleId(self: WorkerTask) ?ModuleId {
        return switch (self) {
            .parse => |t| t.module_id,
            .canonicalize => |t| t.module_id,
            .type_check => |t| t.module_id,
            .shutdown => null,
        };
    }

    pub fn getModuleName(self: WorkerTask) ?[]const u8 {
        return switch (self) {
            .parse => |t| t.module_name,
            .canonicalize => |t| t.module_name,
            .type_check => |t| t.module_name,
            .shutdown => null,
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
    /// The parsed module environment (ownership returned to coordinator)
    module_env: *ModuleEnv,
    /// Cached AST for reuse in canonicalization (ownership returned)
    cached_ast: *AST,
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
pub const TypeCheckedResult = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// The type-checked module environment (ownership returned)
    module_env: *ModuleEnv,
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
    /// Error reports explaining the failure
    reports: std.ArrayList(Report),
    /// Partial module env if available (for error recovery)
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

/// Result when a module is loaded from cache (fast path)
pub const CacheHitResult = struct {
    /// Package this module belongs to
    package_name: []const u8,
    /// Module identifier
    module_id: ModuleId,
    /// Module name
    module_name: []const u8,
    /// Path to the module file
    path: []const u8,
    /// The cached module environment (ownership returned)
    module_env: *ModuleEnv,
    /// Source code (kept for ModuleEnv)
    source: []const u8,
    /// Error count from cache
    error_count: u32,
    /// Warning count from cache
    warning_count: u32,
    /// Cache data buffer - must be kept alive for the lifetime of module_env
    cache_data: CacheData,
    /// Imports from cached metadata - these need to be recursively loaded (owned slice)
    imports: []ImportInfo,
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
    /// Import cycle was detected
    cycle_detected: CycleDetected,
    /// Module was loaded from cache (fast path)
    cache_hit: CacheHitResult,

    pub fn getPackageName(self: WorkerResult) []const u8 {
        return switch (self) {
            .parsed => |r| r.package_name,
            .canonicalized => |r| r.package_name,
            .type_checked => |r| r.package_name,
            .parse_failed => |r| r.package_name,
            .cycle_detected => |r| r.package_name,
            .cache_hit => |r| r.package_name,
        };
    }

    pub fn getModuleId(self: WorkerResult) ModuleId {
        return switch (self) {
            .parsed => |r| r.module_id,
            .canonicalized => |r| r.module_id,
            .type_checked => |r| r.module_id,
            .parse_failed => |r| r.module_id,
            .cycle_detected => |r| r.module_id,
            .cache_hit => |r| r.module_id,
        };
    }

    pub fn getModuleName(self: WorkerResult) []const u8 {
        return switch (self) {
            .parsed => |r| r.module_name,
            .canonicalized => |r| r.module_name,
            .type_checked => |r| r.module_name,
            .parse_failed => |r| r.module_name,
            .cycle_detected => |r| r.module_name,
            .cache_hit => |r| r.module_name,
        };
    }

    /// Free any owned memory in the result
    pub fn deinit(self: *WorkerResult, gpa: Allocator) void {
        switch (self.*) {
            .parsed => |*r| {
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
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .parse_failed => |*r| {
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .cycle_detected => |*r| {
                if (r.cycle_info.cycle_path) |path| gpa.free(path);
                for (r.reports.items) |*rep| rep.deinit();
                r.reports.deinit(gpa);
            },
            .cache_hit => |_| {
                // Module env ownership is transferred to ModuleState, nothing to free here
            },
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
            .depth = 0,
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
            .module_env = undefined,
            .cached_ast = undefined,
            .reports = reports,
            .parse_ns = 1000,
        },
    };

    try std.testing.expectEqualStrings("app", result.getPackageName());
    try std.testing.expectEqual(@as(ModuleId, 1), result.getModuleId());
    try std.testing.expectEqualStrings("Foo", result.getModuleName());
}

test "shutdown task" {
    const task = WorkerTask{ .shutdown = {} };
    try std.testing.expect(task.getPackageName() == null);
    try std.testing.expect(task.getModuleId() == null);
}
