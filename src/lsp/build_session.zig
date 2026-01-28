//! Build session management for LSP operations.
//!
//! This module encapsulates the repeated pattern of:
//! - Converting URIs to paths
//! - Creating a BuildEnv
//! - Setting up file overrides
//! - Changing directories
//! - Building and draining reports
//! - Finding the module environment
//!
//! This eliminates ~40 lines of duplication across 6 functions in syntax.zig.

const std = @import("std");
const compile = @import("compile");
const can = @import("can");
const uri_util = @import("uri.zig");

const BuildEnv = compile.BuildEnv;
const ModuleEnv = can.ModuleEnv;
const FileProvider = compile.package.FileProvider;
const Allocator = std.mem.Allocator;

/// A single build session with automatic cleanup.
/// Encapsulates URI conversion, BuildEnv setup, building, and module lookup.
pub const BuildSession = struct {
    allocator: Allocator,
    env: *BuildEnv,
    absolute_path: []const u8,
    build_succeeded: bool,
    drained_reports: ?[]BuildEnv.DrainedModuleReports = null,
    prev_cwd: ?[]const u8 = null,

    /// File provider state (if override text was provided)
    provider_state: ?OverrideProviderState = null,

    /// Module environment from successful build (null if build failed)
    cached_module_env: ?*ModuleEnv = null,

    /// Whether we've cached the module env yet
    module_env_cached: bool = false,

    const OverrideProviderState = struct {
        override_path: []const u8,
        override_text: []const u8,
    };

    /// Initialize a build session for the given URI.
    /// This handles:
    /// - URI to path conversion
    /// - BuildEnv creation
    /// - File override setup
    /// - Directory change
    /// - Building
    /// - Report draining
    pub fn init(
        allocator: Allocator,
        env: *BuildEnv,
        uri: []const u8,
        override_text: ?[]const u8,
    ) !BuildSession {
        // Convert URI to path
        const path = try uri_util.uriToPath(allocator, uri);
        defer allocator.free(path);

        const absolute_path = std.fs.cwd().realpathAlloc(allocator, path) catch
            try allocator.dupe(u8, path);
        errdefer allocator.free(absolute_path);

        // Save current directory
        const prev_cwd = std.process.getCwdAlloc(allocator) catch null;
        errdefer if (prev_cwd) |cwd| allocator.free(cwd);

        // Change to the directory containing the file
        const dir_slice = std.fs.path.dirname(absolute_path) orelse ".";
        const dir_owned = try allocator.dupe(u8, dir_slice);
        defer allocator.free(dir_owned);
        std.process.changeCurDir(dir_owned) catch {};

        // Set up file provider if override text provided
        var provider_state: ?OverrideProviderState = null;
        if (override_text) |text| {
            provider_state = .{
                .override_path = absolute_path,
                .override_text = text,
            };

            const provider = FileProvider{
                .ctx = &provider_state.?,
                .read = OverrideProvider.read,
            };
            env.setFileProvider(provider);
        }

        // Build
        const build_succeeded = blk: {
            env.build(absolute_path) catch |err| {
                std.debug.print("build_session: build FAILED for {s}: {s}\n", .{ absolute_path, @errorName(err) });
                break :blk false;
            };
            std.debug.print("build_session: build SUCCEEDED\n", .{});
            break :blk true;
        };

        // Drain reports (must be done even on build failure to avoid leaks)
        var drained_reports: ?[]BuildEnv.DrainedModuleReports = null;
        if (build_succeeded) {
            drained_reports = env.drainReports() catch |err| blk: {
                std.debug.print("build_session: drainReports FAILED for {s}: {s}\n", .{ absolute_path, @errorName(err) });
                break :blk null;
            };
        }

        return BuildSession{
            .allocator = allocator,
            .env = env,
            .absolute_path = absolute_path,
            .build_succeeded = build_succeeded,
            .drained_reports = drained_reports,
            .prev_cwd = prev_cwd,
            .provider_state = provider_state,
        };
    }

    /// Clean up the build session.
    /// Restores the working directory and frees allocated memory.
    pub fn deinit(self: *BuildSession) void {
        // Clear file provider
        if (self.provider_state != null) {
            self.env.setFileProvider(null);
        }

        // Restore previous directory
        if (self.prev_cwd) |cwd| {
            std.process.changeCurDir(cwd) catch {};
            self.allocator.free(cwd);
        }

        // Free drained reports
        if (self.drained_reports) |drained| {
            self.freeDrainedReports(drained);
        }

        // Free absolute path
        self.allocator.free(self.absolute_path);
    }

    /// Get the module environment from the current build.
    /// Returns null if the build failed or no module was found.
    pub fn getModuleEnv(self: *BuildSession) ?*ModuleEnv {
        if (self.module_env_cached) {
            return self.cached_module_env;
        }

        self.module_env_cached = true;

        if (!self.build_succeeded) {
            return null;
        }

        // Try to find the module by path across all schedulers
        var sched_it = self.env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            for (sched.modules.items) |*module_state| {
                if (std.mem.eql(u8, module_state.path, self.absolute_path)) {
                    if (module_state.env) |*mod_env| {
                        self.cached_module_env = mod_env;
                        return mod_env;
                    }
                }
            }
        }

        // Fallback: try to get root module from "app" scheduler
        if (self.env.schedulers.get("app")) |sched| {
            if (sched.getRootModule()) |rm| {
                if (rm.env) |*e| {
                    self.cached_module_env = e;
                    return e;
                }
            }
        }

        // Fallback: try to get root module from any scheduler
        sched_it = self.env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            if (sched.getRootModule()) |rm| {
                if (rm.env) |*e| {
                    self.cached_module_env = e;
                    return e;
                }
            }
        }

        return null;
    }

    /// Free drained reports with their contained data.
    fn freeDrainedReports(self: *BuildSession, drained: []BuildEnv.DrainedModuleReports) void {
        for (drained) |*entry| {
            self.allocator.free(entry.abs_path);
            // Free the reports themselves
            for (entry.reports) |*report| {
                @constCast(report).deinit();
            }
            self.allocator.free(entry.reports);
        }
        self.allocator.free(drained);
    }
};

/// File provider that overrides a specific file's content.
const OverrideProvider = struct {
    fn read(ctx: ?*anyopaque, path: []const u8, gpa: std.mem.Allocator) Allocator.Error!?[]u8 {
        const self: *BuildSession.OverrideProviderState = @ptrCast(@alignCast(ctx.?));
        if (std.mem.eql(u8, path, self.override_path)) {
            return try gpa.dupe(u8, self.override_text);
        }
        // Fall back to filesystem for all other files so the compiler can resolve
        // platform modules and package imports without failing on FileNotFound.
        return FileProvider.filesystem.read(null, path, gpa);
    }
};
