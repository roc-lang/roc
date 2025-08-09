//! Package-level build wrapper for the Roc compiler.
//!
//! BuildPackage provides a convenient wrapper around BuildModule for managing
//! per-package builds. It owns the package name and root directory references
//! and delegates the actual module orchestration to BuildModule.

const std = @import("std");

// Delegate actual per-module orchestration to BuildModule
const BuildModuleMod = @import("BuildModule.zig");

/// Build execution mode - single or multi-threaded
pub const Mode = BuildModuleMod.Mode;
/// Destination for build reports (stdout, memory buffer, etc.)
pub const ReportSink = BuildModuleMod.ReportSink;
/// Resolver for cross-package imports
pub const ImportResolver = BuildModuleMod.ImportResolver;
/// Hook for module scheduling notifications
pub const ScheduleHook = BuildModuleMod.ScheduleHook;
/// Module-level build orchestrator
pub const ModuleBuild = BuildModuleMod.ModuleBuild;

const Allocator = std.mem.Allocator;

/// BuildPackage wraps per-package state and delegates module-level work to BuildModule.
/// Ownership:
/// - `name` and `root_dir` slices are borrowed; the caller owns/frees them.
/// - This wrapper owns only the embedded `builder` (ModuleBuild) and its internal state.
pub const BuildPackage = struct {
    gpa: Allocator,
    name: []const u8,
    root_dir: []const u8,

    builder: ModuleBuild,

    /// Initialize a BuildPackage wrapper. This borrows `name` and `root_dir` (the caller owns their memory).
    /// The resolver and schedule hook are optional and can be set/overridden later with setters.
    pub fn init(
        gpa: Allocator,
        name: []const u8,
        root_dir: []const u8,
        mode: Mode,
        max_threads: usize,
        sink: ReportSink,
        resolver: ?ImportResolver,
        schedule_hook: ?ScheduleHook,
    ) BuildPackage {
        return .{
            .gpa = gpa,
            .name = name,
            .root_dir = root_dir,
            .builder = ModuleBuild{
                .gpa = gpa,
                .package_name = name,
                .root_dir = root_dir,
                .mode = mode,
                .max_threads = max_threads,
                .sink = sink,
                .resolver = if (resolver) |r| r else null,
                .schedule_hook = if (schedule_hook) |h| h else null,
            },
        };
    }

    /// Deinitialize the package builder and its internal allocations.
    /// Does not free `name` or `root_dir`.
    pub fn deinit(self: *BuildPackage) void {
        self.builder.deinit();
    }

    /// Set/replace the report sink (stdout, memory buffer, etc.)
    pub fn setSink(self: *BuildPackage, sink: ReportSink) void {
        self.builder.sink = sink;
    }

    /// Set/replace the resolver for cross-package imports.
    pub fn setResolver(self: *BuildPackage, resolver: ImportResolver) void {
        self.builder.resolver = resolver;
    }

    /// Set/replace the optional schedule hook to integrate with a global queue.
    pub fn setScheduleHook(self: *BuildPackage, hook: ScheduleHook) void {
        self.builder.schedule_hook = hook;
    }

    /// Begin building from a root file path for this package.
    pub fn buildRoot(self: *BuildPackage, root_file_path: []const u8) !void {
        try self.builder.buildRoot(root_file_path);
    }

    /// Schedule a module for processing (external or newly discovered), with a preferred depth.
    pub fn scheduleModule(self: *BuildPackage, module_name: []const u8, path: []const u8, depth: u32) !void {
        try self.builder.scheduleModule(module_name, path, depth);
    }

    /// Process a single module by name (driven by a global dispatcher).
    pub fn processModule(self: *BuildPackage, module_name: []const u8) !void {
        try self.builder.process(.{ .module_name = module_name });
    }

    /// Read a module's current recorded dependency depth (if known).
    pub fn getModuleDepth(self: *BuildPackage, module_name: []const u8) ?u32 {
        return self.builder.getModuleDepth(module_name);
    }

    /// Get a pointer to the module's ModuleEnv if Done; null otherwise.
    pub fn getEnvIfDone(self: *BuildPackage, module_name: []const u8) ?*BuildModuleMod.ModuleEnv {
        return self.builder.getEnvIfDone(module_name);
    }

    /// Adjust a module's tracked depth if the provided depth is smaller.
    pub fn setModuleDepthIfSmaller(self: *BuildPackage, module_name: []const u8, depth: u32) !void {
        try self.builder.setModuleDepthIfSmaller(module_name, depth);
    }

    /// Access the underlying ModuleBuild (advanced uses).
    pub fn moduleBuilder(self: *BuildPackage) *ModuleBuild {
        return &self.builder;
    }
};

/// Convenience: construct a default BuildPackage with no resolver/hook (local-only).
pub fn initDefault(
    gpa: Allocator,
    name: []const u8,
    root_dir: []const u8,
    mode: Mode,
    max_threads: usize,
    sink: ReportSink,
) BuildPackage {
    return BuildPackage.init(gpa, name, root_dir, mode, max_threads, sink, null, null);
}
