//! Packs loaded for one build: the specialization keys they can serve at
//! Monotype reservation and the artifacts the object writer splices in.

const std = @import("std");
const backend = @import("backend");
const lir = @import("lir");
const postcheck = @import("postcheck");

const Allocator = std.mem.Allocator;
const PackFile = backend.dev.PackFile;
const Common = postcheck.Common;

/// Every pack in a directory, indexed by specialization key and by procedure
/// identity. Files load in name order so the indexes are deterministic when
/// two packs carry the same entry.
pub const LoadedPacks = struct {
    allocator: Allocator,
    packs: std.ArrayList(PackFile.Pack),
    specs: std.AutoHashMap([32]u8, Common.SpecCacheHit),
    artifacts: std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact),
    /// Specializations served so far.
    hits: u64 = 0,

    pub const LoadError = Allocator.Error || PackFile.ReadError || error{PackDirectoryUnreadable};

    /// Load every `.rpk` file in `dir_path`.
    pub fn loadDir(allocator: Allocator, io: std.Io, dir_path: []const u8) LoadError!LoadedPacks {
        var self = LoadedPacks{
            .allocator = allocator,
            .packs = .empty,
            .specs = std.AutoHashMap([32]u8, Common.SpecCacheHit).init(allocator),
            .artifacts = std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact).init(allocator),
        };
        errdefer self.deinit();

        var names = std.ArrayList([]u8).empty;
        defer {
            for (names.items) |name| allocator.free(name);
            names.deinit(allocator);
        }
        {
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch return error.PackDirectoryUnreadable;
            defer dir.close(io);
            var entries = dir.iterate();
            while (true) {
                const entry = (entries.next(io) catch return error.PackDirectoryUnreadable) orelse break;
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.name, ".rpk")) continue;
                try names.append(allocator, try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);

        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024 * 1024)) catch return error.PackDirectoryUnreadable;
            defer allocator.free(bytes);
            const pack = try PackFile.read(allocator, bytes);
            try self.packs.append(allocator, pack);
        }
        // Index after every pack is in place: the artifact index points into
        // the packs list, which must not move again.
        for (self.packs.items) |*pack| {
            for (pack.set.artifacts, 0..) |artifact, index| {
                switch (artifact.kind) {
                    .proc => |identity| {
                        const gop = try self.artifacts.getOrPut(identity);
                        if (!gop.found_existing) gop.value_ptr.* = .{ .set = &pack.set, .index = @intCast(index) };
                    },
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
                }
            }
            for (pack.specs) |spec| {
                const root = pack.set.artifacts[spec.artifact];
                const identity = switch (root.kind) {
                    .proc => |identity| identity,
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
                };
                const gop = try self.specs.getOrPut(spec.key);
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .identity = identity.bytes,
                    .rc_borrowed_params = spec.rc_borrowed_params,
                    .rc_ret_borrowed = spec.rc_ret_borrowed,
                    .rc_ret_lenders = spec.rc_ret_lenders,
                };
            }
        }
        return self;
    }

    pub fn deinit(self: *LoadedPacks) void {
        self.artifacts.deinit();
        self.specs.deinit();
        for (self.packs.items) |*pack| pack.deinit();
        self.packs.deinit(self.allocator);
    }

    /// The lookup Monotype consults at reservation.
    pub fn specCacheLookup(self: *LoadedPacks) Common.SpecCacheLookup {
        return .{ .context = @ptrCast(self), .find = findSpec };
    }

    /// The artifact source the object compiler splices from.
    pub fn spliceSource(self: *LoadedPacks) backend.dev.SpliceSource {
        return .{ .context = @ptrCast(self), .find = findArtifact };
    }

    fn findSpec(context: *anyopaque, key: [32]u8) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        const hit = self.specs.get(key) orelse return null;
        self.hits += 1;
        return hit;
    }

    fn findArtifact(context: *anyopaque, identity: lir.ProcIdentity) ?backend.dev.LocatedArtifact {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        return self.artifacts.get(identity);
    }
};

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}
