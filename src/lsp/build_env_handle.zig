//! Reference-counted wrapper for BuildEnv used by the LSP.
//!
//! This is a small ownership layer that ensures a BuildEnv is only deinitialized
//! once all logical owners (build_env, previous_build_env, snapshot) have
//! released it. Debug tracking records who retains/releases to catch mismatches.

const std = @import("std");
const compile = @import("compile");

const Allocator = std.mem.Allocator;
const BuildEnv = compile.BuildEnv;

/// Reference-counted owner for a `BuildEnv` shared across LSP components.
pub const BuildEnvHandle = struct {
    allocator: Allocator,
    env: BuildEnv,
    ref_count: usize = 0,
    debug: bool = false,
    owners: std.StringHashMapUnmanaged(usize) = .{},

    /// Create a new handle with a single owner.
    pub fn create(allocator: Allocator, env: BuildEnv, owner: []const u8, debug: bool) !*BuildEnvHandle {
        const handle = try allocator.create(BuildEnvHandle);
        handle.* = .{
            .allocator = allocator,
            .env = env,
            .ref_count = 1,
            .debug = debug,
        };
        if (debug) {
            handle.addOwner(owner);
            handle.log("create", owner);
        }
        return handle;
    }

    /// Get a stable pointer to the underlying BuildEnv.
    pub fn envPtr(self: *BuildEnvHandle) *BuildEnv {
        return &self.env;
    }

    /// Retain the handle for an additional owner.
    pub fn retain(self: *BuildEnvHandle, owner: []const u8) void {
        self.ref_count += 1;
        if (self.debug) {
            self.addOwner(owner);
            self.log("retain", owner);
        }
    }

    /// Release the handle for a given owner.
    /// When ref_count reaches zero, the BuildEnv is deinitialized and freed.
    pub fn release(self: *BuildEnvHandle, owner: []const u8) void {
        std.debug.assert(self.ref_count > 0);
        if (self.debug) {
            self.removeOwner(owner);
            self.log("release", owner);
        }

        self.ref_count -= 1;
        if (self.ref_count == 0) {
            if (self.debug) {
                // In debug mode, all owner counts should be fully released.
                std.debug.assert(self.owners.count() == 0);
            }
            self.owners.deinit(self.allocator);
            self.env.deinit();
            self.allocator.destroy(self);
        }
    }

    fn addOwner(self: *BuildEnvHandle, owner: []const u8) void {
        if (!self.debug) return;
        const entry = self.owners.get(owner);
        const next = if (entry) |count| count + 1 else 1;
        _ = self.owners.put(self.allocator, owner, next) catch {};
    }

    fn removeOwner(self: *BuildEnvHandle, owner: []const u8) void {
        if (!self.debug) return;
        if (self.owners.get(owner)) |count| {
            if (count <= 1) {
                _ = self.owners.remove(owner);
            } else {
                _ = self.owners.put(self.allocator, owner, count - 1) catch {};
            }
        } else {
            std.debug.print("BuildEnvHandle: release without owner tracking: {s}\n", .{owner});
        }
    }

    fn log(self: *BuildEnvHandle, action: []const u8, owner: []const u8) void {
        if (!self.debug) return;
        std.debug.print("BuildEnvHandle {s}: owner={s} ref_count={d}\n", .{ action, owner, self.ref_count });
    }
};
