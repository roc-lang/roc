//! Owned, validated immutable cache records. This is an acquisition lifetime,
//! not a storage namespace: the catalog supplies the retained generation lease.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// The persisted format boundary validated by the owning native loader.
pub const Kind = enum { native_payload_v1, native_component_v1, native_offer_v3, native_offer_domain_v1 };

/// Validation work and retained storage, separate from publication work.
pub const Metrics = struct {
    acquisitions: u64 = 0,
    bytes_read: u64 = 0,
    bytes_hashed: u64 = 0,
    retained_bytes: u64 = 0,
    retained_records: u64 = 0,
};

/// A compilation's acquisitions, also retained by captured native artifact sets.
pub const Owner = struct {
    allocator: Allocator,
    io: std.Io,
    domain: [32]u8,
    metrics: Metrics = .{},
    references: std.atomic.Value(usize) = .init(1),
    records: std.ArrayList(*Record) = .empty,
    resources: std.ArrayList(Resource) = .empty,
    pub const Resource = struct { context: *anyopaque, deinit: *const fn (*anyopaque) void };

    pub fn adopt(self: *Owner, resource: Resource) Allocator.Error!void {
        try self.resources.append(self.allocator, resource);
    }

    pub fn create(allocator: Allocator, io: std.Io, domain: [32]u8) Allocator.Error!*Owner {
        const owner = try allocator.create(Owner);
        owner.* = .{ .allocator = allocator, .io = io, .domain = domain };
        return owner;
    }

    pub fn retain(self: *Owner) void {
        _ = self.references.fetchAdd(1, .monotonic);
    }

    pub fn release(self: *Owner) void {
        if (self.references.fetchSub(1, .acq_rel) != 1) return;
        for (self.resources.items) |resource| resource.deinit(resource.context);
        self.resources.deinit(self.allocator);
        for (self.records.items) |record| {
            record.retention.deinit(record.retention.context);
            self.allocator.free(record.bytes);
            self.allocator.destroy(record);
        }
        self.records.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    /// Take producer bytes and their storage lease only on successful admission.
    /// The caller owns both arguments on failure.
    pub fn take(self: *Owner, bytes: []const u8, expected: [32]u8, kind: Kind, storage_identity: [32]u8, retention: Resource) (Allocator.Error || error{Corrupt})!*const Record {
        self.metrics.acquisitions += 1;
        self.metrics.bytes_read += bytes.len;
        self.metrics.bytes_hashed += bytes.len;
        var hash: [32]u8 = undefined;
        @import("sha256.zig").Sha256.hash(bytes, &hash, .{});
        if (!std.mem.eql(u8, &hash, &expected)) return error.Corrupt;
        const record = try self.allocator.create(Record);
        errdefer self.allocator.destroy(record);
        record.* = .{ .owner = self, .retention = retention, .bytes = bytes, .digest = hash, .storage_identity = storage_identity, .kind = kind };
        try self.records.append(self.allocator, record);
        self.metrics.retained_bytes += bytes.len + @sizeOf(Record);
        self.metrics.retained_records += 1;
        return record;
    }
};

/// Validated bytes and their retained storage generation. Only Owner constructs
/// these references. Retain owner when a reference crosses an ownership boundary.
pub const Record = struct {
    kind: Kind,
    owner: *Owner,
    retention: Owner.Resource,
    bytes: []const u8,
    digest: [32]u8,
    storage_identity: [32]u8,
};
