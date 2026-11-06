//! Checking dependencies and explicit owner binding, separate from implementation
//! identity. A scope's digest retains owner equality without retaining the old
//! implementations that supplied those owners.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Hash = base.ModuleIdentity.Hash;

/// Encode an interface scalar in fixed-width little-endian order.
pub fn hashU32(hasher: *base.Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

/// Length-prefix an interface byte string before hashing it.
pub fn hashBytes(hasher: *base.Sha256, bytes: []const u8) Allocator.Error!void {
    hashU32(hasher, std.math.cast(u32, bytes.len) orelse return error.OutOfMemory);
    hasher.update(bytes);
}

/// Ordered inputs supplied by the coordinator/checker, not a graph recovered
/// from a cached specialization. Repeated bindings retain their equality
/// relationship even when two interfaces happen to have the same digest.
pub const Scope = struct {
    allocator: Allocator,
    owners: std.ArrayList(Hash) = .empty,
    imported: []const *const ModuleEnv = &.{},
    environments: std.AutoHashMapUnmanaged(Hash, *const ModuleEnv) = .empty,
    ordinals: std.AutoHashMapUnmanaged(Hash, u32) = .empty,
    hasher: base.Sha256 = base.Sha256.init(.{}),

    pub fn init(allocator: Allocator, own_identity: Hash) Allocator.Error!Scope {
        var scope = Scope{ .allocator = allocator };
        errdefer scope.deinit();
        scope.hasher.update("roc-checking-owner-scope-v1");
        _ = try scope.appendOwner(own_identity);
        return scope;
    }

    pub fn deinit(self: *Scope) void {
        self.owners.deinit(self.allocator);
        self.ordinals.deinit(self.allocator);
        self.environments.deinit(self.allocator);
    }

    pub fn appendOwner(self: *Scope, identity: Hash) Allocator.Error!u32 {
        const next = std.math.cast(u32, self.owners.items.len) orelse return error.OutOfMemory;
        const entry = try self.ordinals.getOrPut(self.allocator, identity);
        if (!entry.found_existing) entry.value_ptr.* = next;
        try self.owners.append(self.allocator, identity);
        hashU32(&self.hasher, entry.value_ptr.*);
        return entry.value_ptr.*;
    }

    /// Add an already-published dependency at its explicit position in the
    /// checking scope. Missing producer data is an invariant, never a full-key
    /// substitute or an inferred public signature.
    pub fn appendDependency(self: *Scope, env: *const ModuleEnv) Allocator.Error!void {
        const identity = env.contentIdentityHash() orelse
            base.invariant("checking dependency owners have finalized content identities", .{});
        try self.environments.put(self.allocator, identity.*, env);
        const owner_ordinal = try self.appendOwner(identity.*);
        hashU32(&self.hasher, owner_ordinal);
        if (!env.checking_dependency.published) {
            base.invariant("checking consumes a dependency only after its checking interface is published", .{});
        }
        self.hasher.update(&env.checking_dependency.digest);
    }

    pub fn ordinal(self: *const Scope, identity: Hash) u32 {
        return self.ordinals.get(identity) orelse
            base.invariant("a checking reference's owner belongs to the producer's explicit scope", .{});
    }

    pub fn digest(self: *const Scope) Hash {
        var hasher = self.hasher;
        return hasher.finalResult();
    }

    pub fn copyOwners(self: *const Scope, allocator: Allocator) Allocator.Error!collections.SafeList(Hash) {
        var owners = try collections.SafeList(Hash).initCapacity(allocator, self.owners.items.len);
        errdefer owners.deinit(allocator);
        _ = try owners.appendSlice(allocator, self.owners.items);
        return owners;
    }
};

test "checking owner scopes retain nominal equality independently of implementation hashes" {
    const gpa = std.testing.allocator;
    var original = try Scope.init(gpa, @splat(1));
    defer original.deinit();
    _ = try original.appendOwner(@splat(2));
    _ = try original.appendOwner(@splat(2));

    var rebound = try Scope.init(gpa, @splat(3));
    defer rebound.deinit();
    _ = try rebound.appendOwner(@splat(4));
    _ = try rebound.appendOwner(@splat(4));
    try std.testing.expectEqualSlices(u8, &original.digest(), &rebound.digest());

    var split = try Scope.init(gpa, @splat(3));
    defer split.deinit();
    _ = try split.appendOwner(@splat(4));
    _ = try split.appendOwner(@splat(5));
    try std.testing.expect(!base.ModuleIdentity.eql(&original.digest(), &split.digest()));
}
