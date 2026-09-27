//! Filesystem coverage for explicit compiler inputs. Never enumerates a directory.
//! Logical input identities stay unchanged; resolved paths are only notification
//! locations. Every traversed directory entry (including symlinks) remains an
//! interest so replacing an ancestor repairs coverage for its dependent inputs.
const std = @import("std");
const Inputs = @This();
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

allocator: Allocator,
io: std.Io,
nodes: std.StringArrayHashMapUnmanaged(Node) = .{},
windows_names: std.HashMapUnmanaged([]const u8, std.ArrayList(usize), WindowsNameContext, 80) = .{},

const WindowsNameContext = struct {
    pub fn hash(_: @This(), path: []const u8) u64 {
        var hasher = std.hash.Wyhash.init(0);
        var it = std.unicode.Wtf8View.initUnchecked(path).iterator();
        while (it.nextCodepoint()) |cp| {
            const upper: u32 = if (cp <= 0xffff) std.os.windows.toUpperWtf16(@intCast(cp)) else cp;
            hasher.update(std.mem.asBytes(&upper));
        }
        return hasher.final();
    }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return std.os.windows.eqlIgnoreCaseWtf8(a, b);
    }
};

pub const Error = Allocator.Error || error{WatchBackendFailed};

pub const Node = struct {
    inode: ?std.Io.File.INode = null,
    kind: std.Io.File.Kind = .unknown,
    permissions: ?std.Io.File.Permissions = null,
    mtime: i96 = 0,
    ctime: i96 = 0,
    link: ?[]const u8 = null,
    topology: bool = false,
    inputs: std.ArrayList(usize) = .empty,
    /// Used by vnode backends, whose directory events contain no child name.
    children: std.ArrayList(usize) = .empty,
};

pub fn init(allocator: Allocator, io: std.Io, paths: []const []const u8) Error!Inputs {
    var self = Inputs{ .allocator = allocator, .io = io };
    errdefer self.deinit();
    var links: std.StringHashMapUnmanaged(void) = .{};
    defer links.deinit(allocator);
    for (paths, 0..) |path, input| {
        std.debug.assert(std.fs.path.isAbsolute(path));
        if (try self.resolve(path, null, input, &links)) |resolved| allocator.free(resolved);
    }
    if (builtin.os.tag == .windows) {
        // Preserve distinct logical spellings, including on case-sensitive
        // directories, while routing native case-insensitive notifications to
        // every matching input. No scan of the full input set is needed.
        for (self.nodes.keys(), 0..) |path, index| {
            const entry = try self.windows_names.getOrPut(allocator, path);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.append(allocator, index);
        }
    }
    return self;
}

pub fn deinit(self: *Inputs) void {
    var names = self.windows_names.valueIterator();
    while (names.next()) |indices| indices.deinit(self.allocator);
    self.windows_names.deinit(self.allocator);
    for (self.nodes.keys(), self.nodes.values()) |path, *node| {
        self.allocator.free(path);
        if (node.link) |link| self.allocator.free(link);
        node.inputs.deinit(self.allocator);
        node.children.deinit(self.allocator);
    }
    self.nodes.deinit(self.allocator);
}

/// Compare coverage, including identities, to close plan/registration races.
/// Directory timestamps deliberately do not participate: unrelated children
/// changing must not invalidate coverage.
pub fn eql(self: *const Inputs, other: *const Inputs) bool {
    if (self.nodes.count() != other.nodes.count()) return false;
    for (self.nodes.keys(), self.nodes.values()) |path, node| {
        const b = other.nodes.get(path) orelse return false;
        if (node.inode != b.inode or node.kind != b.kind or node.topology != b.topology or node.permissions != b.permissions) return false;
        if (!std.mem.eql(usize, node.inputs.items, b.inputs.items)) return false;
        if (node.link) |link| {
            if (!std.mem.eql(u8, link, b.link orelse return false)) return false;
        } else if (b.link != null) return false;
    }
    return true;
}

/// Vnode backends cannot name the changed directory entry. Inspect only entries
/// in the input graph. Ignore access times so reading an input cannot cause a
/// notification/read loop on filesystems that update atime on every read.
pub fn entryChanged(self: *const Inputs, path: []const u8) bool {
    const node = self.nodes.get(path) orelse return false;
    const stat = std.Io.Dir.cwd().statFile(self.io, path, .{ .follow_symlinks = false }) catch return node.inode != null;
    if (node.inode != stat.inode or node.kind != stat.kind or node.permissions != stat.permissions) return true;
    return false;
}

fn nodeFor(self: *Inputs, path: []const u8, input: usize) Error!*Node {
    const entry = try self.nodes.getOrPut(self.allocator, path);
    if (!entry.found_existing) {
        // Roll back before exposing a partially initialized node on failure.
        errdefer _ = self.nodes.pop();
        const owned = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned);
        var node = Node{};
        errdefer if (node.link) |link| self.allocator.free(link);
        const stat = std.Io.Dir.cwd().statFile(self.io, path, .{ .follow_symlinks = false }) catch |err| switch (err) {
            error.FileNotFound, error.NotDir, error.AccessDenied, error.PermissionDenied => null,
            else => {
                std.log.warn("Failed to inspect watch input {s}: {}", .{ path, err });
                return error.WatchBackendFailed;
            },
        };
        if (stat) |s| {
            node.inode = s.inode;
            node.kind = s.kind;
            node.permissions = s.permissions;
            node.mtime = s.mtime.nanoseconds;
            node.ctime = s.ctime.nanoseconds;
            if (s.kind == .sym_link) {
                var buffer: [std.fs.max_path_bytes]u8 = undefined;
                const len = std.Io.Dir.readLinkAbsolute(self.io, path, &buffer) catch |err| switch (err) {
                    error.FileNotFound, error.NotDir, error.NotLink, error.AccessDenied, error.PermissionDenied => null,
                    else => {
                        std.log.warn("Failed to read watched symlink {s}: {}", .{ path, err });
                        return error.WatchBackendFailed;
                    },
                };
                if (len) |n| node.link = try self.allocator.dupe(u8, buffer[0..n]);
            }
        }
        entry.key_ptr.* = owned;
        entry.value_ptr.* = node;
        const index = self.nodes.count() - 1;
        if (std.fs.path.dirname(path)) |parent_path| {
            if (!std.mem.eql(u8, parent_path, path)) if (self.nodes.getPtr(parent_path)) |parent| {
                try parent.children.append(self.allocator, index);
            };
        }
    }
    const node = entry.value_ptr;
    // Inputs are visited in order, even when one visits an entry through several
    // aliases. Deduplication therefore needs no per-node hash table or scan.
    if (node.inputs.items.len == 0 or node.inputs.items[node.inputs.items.len - 1] != input) {
        try node.inputs.append(self.allocator, input);
    }
    return node;
}

fn resolve(self: *Inputs, path: []const u8, base: ?[]const u8, input: usize, links: *std.StringHashMapUnmanaged(void)) Error!?[]const u8 {
    var components = std.fs.path.componentIterator(path);
    var current: []const u8 = try self.allocator.dupe(u8, components.root() orelse base.?);
    errdefer self.allocator.free(current);
    const root = try self.nodeFor(current, input);
    root.topology = true;
    while (components.next()) |component| {
        const parent = try self.nodeFor(current, input);
        parent.topology = true;
        if (parent.kind != .directory) {
            self.allocator.free(current);
            return null;
        }
        if (std.mem.eql(u8, component.name, ".")) continue;
        if (std.mem.eql(u8, component.name, "..")) {
            const parent_path = try self.allocator.dupe(u8, std.fs.path.dirname(current) orelse current);
            self.allocator.free(current);
            current = parent_path;
            (try self.nodeFor(current, input)).topology = true;
            continue;
        }
        const candidate = try std.fs.path.join(self.allocator, &.{ current, component.name });
        errdefer self.allocator.free(candidate);
        const node = try self.nodeFor(candidate, input);
        if (node.kind == .sym_link) {
            node.topology = true;
            if (node.link == null or links.contains(candidate)) {
                self.allocator.free(candidate);
                self.allocator.free(current);
                return null;
            }
            try links.put(self.allocator, candidate, {});
            const target = self.resolve(node.link.?, current, input, links) catch |err| {
                _ = links.remove(candidate);
                return err;
            };
            _ = links.remove(candidate);
            self.allocator.free(candidate);
            self.allocator.free(current);
            current = target orelse return null;
        } else {
            self.allocator.free(current);
            current = candidate;
        }
    }
    return current;
}
