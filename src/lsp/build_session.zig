//! Build session management for LSP operations.
//!
//! This module encapsulates the repeated pattern of:
//! - Converting URIs to paths
//! - Creating a BuildEnv
//! - Setting up file overrides
//! - Building and draining reports
//! - Finding the module environment
//!
//! This eliminates ~40 lines of duplication across 6 functions in syntax.zig.

const std = @import("std");
const compile = @import("compile");
const can = @import("can");
const uri_util = @import("uri.zig");

const BuildEnv = compile.BuildEnv;
const CoreCtx = compile.CoreCtx;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;

/// A single build session with automatic cleanup.
/// Encapsulates URI conversion, BuildEnv setup, building, and module lookup.
pub const BuildSession = struct {
    allocator: Allocator,
    /// Borrowed pointer to the BuildEnv for this build. Ownership stays with the
    /// caller (typically SyntaxChecker via BuildEnvHandle); deinit does NOT free it.
    env: *BuildEnv,
    absolute_path: [:0]const u8,
    build_succeeded: bool,
    drained_reports: ?[]BuildEnv.DrainedModuleReports = null,

    /// Module environment from successful build (null if build failed)
    cached_module_env: ?*ModuleEnv = null,

    /// Whether we've cached the module env yet
    module_env_cached: bool = false,

    /// Initialize a build session for the given URI.
    /// This handles:
    /// - URI to path conversion
    /// - File override setup
    /// - Building (with owning main.roc / workspace package aliases when needed)
    /// - Report draining
    pub fn init(
        allocator: Allocator,
        std_io: std.Io,
        env: *BuildEnv,
        uri: []const u8,
        override_text: ?[]const u8,
        workspace_root: ?[]const u8,
    ) Allocator.Error!BuildSession {
        // Convert URI to path
        const path = try uri_util.uriToPath(allocator, uri);
        defer allocator.free(path);

        const absolute_path: [:0]u8 = std.Io.Dir.cwd().realPathFileAlloc(std_io, path, allocator) catch
            try allocator.dupeZ(u8, path);
        errdefer allocator.free(absolute_path);

        // Set up file override if override text provided.
        // SAFETY: override lives on the stack and its address is stored in env.filesystem.
        // This is safe because env.buildResolvingMain() is synchronous and we restore the Io before returning.
        var override: CoreCtx.ReadFileOverride = undefined;
        const saved_io = env.filesystem;
        if (override_text) |text| {
            override = .{ .path = absolute_path, .content = text, .base = env.filesystem };
            env.filesystem = override.io();
        }

        const preferred_main = try preferredMainFromWorkspace(allocator, env.filesystem, workspace_root);
        defer if (preferred_main) |main_path| allocator.free(main_path);

        // Build
        const build_succeeded = blk: {
            env.buildResolvingMain(absolute_path, preferred_main) catch {
                break :blk false;
            };
            break :blk true;
        };

        // Restore the original Io now that the build is complete.
        if (override_text != null) {
            env.filesystem = saved_io;
        }

        // Drain reports regardless of build success to capture parse errors
        // Parse errors are emitted to the sink even when build fails
        const drained_reports = try env.drainReports();

        return BuildSession{
            .allocator = allocator,
            .env = env,
            .absolute_path = absolute_path,
            .build_succeeded = build_succeeded,
            .drained_reports = drained_reports,
        };
    }

    /// If the LSP workspace root contains `main.roc`, prefer it as the package-alias root.
    fn preferredMainFromWorkspace(allocator: Allocator, filesystem: CoreCtx, workspace_root: ?[]const u8) Allocator.Error!?[]u8 {
        const root = workspace_root orelse return null;
        const candidate = try std.fs.path.join(allocator, &.{ root, "main.roc" });
        errdefer allocator.free(candidate);
        if (!filesystem.fileExists(candidate)) {
            allocator.free(candidate);
            return null;
        }
        const abs = try std.fs.path.resolve(allocator, &.{candidate});
        allocator.free(candidate);
        return abs;
    }

    /// Clean up the build session and free allocated memory.
    pub fn deinit(self: *BuildSession) void {
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
                    if (module_state.moduleEnv()) |mod_env| {
                        self.cached_module_env = mod_env;
                        return mod_env;
                    }
                }
            }
        }

        // Fallback: try to get the discovered root module by its package identity.
        if (self.env.discovered_pkg_name) |root_pkg_name| {
            if (self.env.schedulers.get(root_pkg_name)) |sched| {
                if (sched.getRootModule()) |rm| {
                    if (rm.moduleEnv()) |e| {
                        self.cached_module_env = e;
                        return e;
                    }
                }
            }
        }

        // Fallback: try to get root module from any scheduler
        sched_it = self.env.schedulers.iterator();
        while (sched_it.next()) |entry| {
            const sched = entry.value_ptr.*;
            if (sched.getRootModule()) |rm| {
                if (rm.moduleEnv()) |e| {
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
