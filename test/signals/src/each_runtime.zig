const std = @import("std");

pub const missing_row_index = std.math.maxInt(usize);

pub const SiteKey = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

pub const SiteKeyContext = struct {
    pub fn hash(_: @This(), key: SiteKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&key.parent_scope_id));
        hasher.update(std.mem.asBytes(&key.site_ordinal));
        return hasher.final();
    }

    pub fn eql(_: @This(), left: SiteKey, right: SiteKey) bool {
        return left.parent_scope_id == right.parent_scope_id and left.site_ordinal == right.site_ordinal;
    }
};

pub const SiteIndexMap = std.HashMapUnmanaged(SiteKey, usize, SiteKeyContext, std.hash_map.default_max_load_percentage);

pub const Membership = struct {
    site_index: usize,
    row_index: usize,
};

pub const Site = struct {
    key: SiteKey,
    scope_ids: std.ArrayListUnmanaged(u64) = .empty,
    hash_heads: std.AutoHashMapUnmanaged(u64, usize) = .empty,
    hash_links: std.ArrayListUnmanaged(usize) = .empty,

    pub fn deinit(self: *Site, allocator: std.mem.Allocator) void {
        self.scope_ids.deinit(allocator);
        self.hash_heads.deinit(allocator);
        self.hash_links.deinit(allocator);
        self.* = undefined;
    }
};

pub fn clearSites(allocator: std.mem.Allocator, sites: *std.ArrayListUnmanaged(Site), site_indexes: *SiteIndexMap, memberships: *std.ArrayListUnmanaged(?Membership)) void {
    for (sites.items) |*site| {
        site.deinit(allocator);
    }
    sites.deinit(allocator);
    site_indexes.deinit(allocator);
    memberships.deinit(allocator);
    sites.* = .empty;
    site_indexes.* = .empty;
    memberships.* = .empty;
}

pub fn ensureMembershipSlot(allocator: std.mem.Allocator, memberships: *std.ArrayListUnmanaged(?Membership), scope_id: u64) *?Membership {
    const index: usize = @intCast(scope_id);
    while (memberships.items.len <= index) {
        memberships.append(allocator, null) catch @panic("out of memory");
    }
    return &memberships.items[index];
}

pub fn ensureSiteIndex(allocator: std.mem.Allocator, sites: *std.ArrayListUnmanaged(Site), site_indexes: *SiteIndexMap, parent_scope_id: u64, site_ordinal: u64) usize {
    const key: SiteKey = .{
        .parent_scope_id = parent_scope_id,
        .site_ordinal = site_ordinal,
    };
    const entry = site_indexes.getOrPut(allocator, key) catch @panic("out of memory");
    if (entry.found_existing) return entry.value_ptr.*;

    const site_index = sites.items.len;
    sites.append(allocator, .{ .key = key }) catch @panic("out of memory");
    entry.value_ptr.* = site_index;
    return site_index;
}

pub fn activeSiteIndex(site_indexes: *const SiteIndexMap, parent_scope_id: u64, site_ordinal: u64) ?usize {
    return site_indexes.get(.{
        .parent_scope_id = parent_scope_id,
        .site_ordinal = site_ordinal,
    });
}

pub fn appendRowToSiteIndex(allocator: std.mem.Allocator, sites: *std.ArrayListUnmanaged(Site), memberships: *std.ArrayListUnmanaged(?Membership), site_index: usize, scope_id: u64, key_hash: u64) void {
    if (site_index >= sites.items.len) @panic("each row site index exceeded site table");
    const site = &sites.items[site_index];
    const row_index = site.scope_ids.items.len;

    site.scope_ids.append(allocator, scope_id) catch @panic("out of memory");
    site.hash_links.append(allocator, missing_row_index) catch @panic("out of memory");
    const hash_entry = site.hash_heads.getOrPut(allocator, key_hash) catch @panic("out of memory");
    if (hash_entry.found_existing) {
        site.hash_links.items[row_index] = hash_entry.value_ptr.*;
    }
    hash_entry.value_ptr.* = row_index;

    const membership = ensureMembershipSlot(allocator, memberships, scope_id);
    if (membership.* != null) @panic("each row scope already had an active row index");
    membership.* = .{
        .site_index = site_index,
        .row_index = row_index,
    };
}

pub fn removeRowFromSiteIndex(sites: *std.ArrayListUnmanaged(Site), memberships: *std.ArrayListUnmanaged(?Membership), scope_id: u64, key_hash: u64, row_keys: anytype) void {
    if (scope_id >= memberships.items.len) @panic("each row scope was missing its row index");
    const membership = memberships.items[@intCast(scope_id)] orelse @panic("each row scope was missing its row index");
    if (membership.site_index >= sites.items.len) @panic("each row membership pointed past site table");
    const site = &sites.items[membership.site_index];
    if (membership.row_index >= site.scope_ids.items.len) @panic("each row membership pointed past row table");
    if (site.scope_ids.items[membership.row_index] != scope_id) @panic("each row membership pointed at the wrong scope");

    const last_index = site.scope_ids.items.len - 1;
    const moved_scope_id = site.scope_ids.items[last_index];
    unlinkHashIndex(site, key_hash, membership.row_index);

    memberships.items[@intCast(scope_id)] = null;

    if (membership.row_index != last_index) {
        const moved_hash = rowKeysHash(row_keys, moved_scope_id);
        replaceHashIndex(site, moved_hash, last_index, membership.row_index);
        site.scope_ids.items[membership.row_index] = moved_scope_id;
        site.hash_links.items[membership.row_index] = site.hash_links.items[last_index];

        const moved_membership = &memberships.items[@intCast(moved_scope_id)];
        if (moved_membership.*) |*entry| {
            entry.row_index = membership.row_index;
        } else {
            @panic("moved each row scope was missing its row index");
        }
    }

    _ = site.scope_ids.pop();
    _ = site.hash_links.pop();
}

pub fn replaceSiteRows(allocator: std.mem.Allocator, sites: *std.ArrayListUnmanaged(Site), memberships: *std.ArrayListUnmanaged(?Membership), site_index: usize, scope_ids: []const u64, row_keys: anytype) void {
    if (site_index >= sites.items.len) @panic("each row site index exceeded site table");
    const site = &sites.items[site_index];

    for (site.scope_ids.items) |scope_id| {
        if (scope_id < memberships.items.len) {
            memberships.items[@intCast(scope_id)] = null;
        }
    }

    site.scope_ids.clearRetainingCapacity();
    site.scope_ids.appendSlice(allocator, scope_ids) catch @panic("out of memory");
    site.hash_links.resize(allocator, scope_ids.len) catch @panic("out of memory");
    @memset(site.hash_links.items, missing_row_index);
    site.hash_heads.clearRetainingCapacity();

    for (scope_ids, 0..) |scope_id, row_index| {
        const key_hash = rowKeysHash(row_keys, scope_id);

        const hash_entry = site.hash_heads.getOrPut(allocator, key_hash) catch @panic("out of memory");
        if (hash_entry.found_existing) {
            site.hash_links.items[row_index] = hash_entry.value_ptr.*;
        }
        hash_entry.value_ptr.* = row_index;

        const membership = ensureMembershipSlot(allocator, memberships, scope_id);
        if (membership.* != null) @panic("each row scope already had an active row index");
        membership.* = .{
            .site_index = site_index,
            .row_index = row_index,
        };
    }
}

fn rowKeysHash(row_keys: anytype, scope_id: u64) u64 {
    return row_keys.rowKeyHash(scope_id);
}

fn unlinkHashIndex(site: *Site, hash: u64, row_index: usize) void {
    const head = site.hash_heads.getPtr(hash) orelse @panic("each row hash bucket was missing");
    if (head.* == row_index) {
        const next = site.hash_links.items[row_index];
        if (next == missing_row_index) {
            _ = site.hash_heads.remove(hash);
        } else {
            head.* = next;
        }
        return;
    }

    var current = head.*;
    while (current != missing_row_index) {
        const next = &site.hash_links.items[current];
        if (next.* == row_index) {
            next.* = site.hash_links.items[row_index];
            return;
        }
        current = next.*;
    }
    @panic("each row hash bucket did not contain row index");
}

fn replaceHashIndex(site: *Site, hash: u64, old_index: usize, new_index: usize) void {
    if (old_index == new_index) return;

    const head = site.hash_heads.getPtr(hash) orelse @panic("each row hash bucket was missing");
    if (head.* == old_index) {
        head.* = new_index;
        return;
    }

    var current = head.*;
    while (current != missing_row_index) {
        const next = &site.hash_links.items[current];
        if (next.* == old_index) {
            next.* = new_index;
            return;
        }
        current = next.*;
    }
    @panic("each row hash bucket did not contain moved row index");
}

const TestRowKeys = struct {
    hashes: []const u64,

    pub fn rowKeyHash(self: *const TestRowKeys, scope_id: u64) u64 {
        if (scope_id >= self.hashes.len) @panic("test scope id exceeded row key table");
        return self.hashes[@intCast(scope_id)];
    }
};

test "each runtime appends rows and tracks memberships" {
    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var indexes: SiteIndexMap = .empty;
    var memberships: std.ArrayListUnmanaged(?Membership) = .empty;
    defer clearSites(std.testing.allocator, &sites, &indexes, &memberships);

    const site_index = ensureSiteIndex(std.testing.allocator, &sites, &indexes, 1, 2);
    try std.testing.expectEqual(site_index, activeSiteIndex(&indexes, 1, 2).?);

    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 10, 5);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 11, 5);

    try std.testing.expectEqualSlices(u64, &.{ 10, 11 }, sites.items[site_index].scope_ids.items);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 0 }, memberships.items[10].?);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 1 }, memberships.items[11].?);
    try std.testing.expectEqual(@as(usize, 1), sites.items[site_index].hash_heads.get(5).?);
    try std.testing.expectEqual(@as(usize, 0), sites.items[site_index].hash_links.items[1]);
}

test "each runtime removes rows and rewrites moved memberships" {
    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var indexes: SiteIndexMap = .empty;
    var memberships: std.ArrayListUnmanaged(?Membership) = .empty;
    defer clearSites(std.testing.allocator, &sites, &indexes, &memberships);

    const site_index = ensureSiteIndex(std.testing.allocator, &sites, &indexes, 1, 2);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 10, 5);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 11, 6);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 12, 7);

    const hashes = [_]u64{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 6, 7 };
    const row_keys = TestRowKeys{ .hashes = &hashes };
    removeRowFromSiteIndex(&sites, &memberships, 10, 5, &row_keys);

    try std.testing.expectEqualSlices(u64, &.{ 12, 11 }, sites.items[site_index].scope_ids.items);
    try std.testing.expectEqual(@as(?Membership, null), memberships.items[10]);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 0 }, memberships.items[12].?);
    try std.testing.expectEqual(@as(usize, 0), sites.items[site_index].hash_heads.get(7).?);
    try std.testing.expectEqual(@as(?usize, null), sites.items[site_index].hash_heads.get(5));
}

test "each runtime replaces row order and rebuilds indexes" {
    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var indexes: SiteIndexMap = .empty;
    var memberships: std.ArrayListUnmanaged(?Membership) = .empty;
    defer clearSites(std.testing.allocator, &sites, &indexes, &memberships);

    const site_index = ensureSiteIndex(std.testing.allocator, &sites, &indexes, 1, 2);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 10, 5);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 11, 6);

    const hashes = [_]u64{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 6, 5 };
    const row_keys = TestRowKeys{ .hashes = &hashes };
    replaceSiteRows(std.testing.allocator, &sites, &memberships, site_index, &.{ 12, 10 }, &row_keys);

    try std.testing.expectEqualSlices(u64, &.{ 12, 10 }, sites.items[site_index].scope_ids.items);
    try std.testing.expectEqual(@as(?Membership, null), memberships.items[11]);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 0 }, memberships.items[12].?);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 1 }, memberships.items[10].?);
    try std.testing.expectEqual(@as(usize, 1), sites.items[site_index].hash_heads.get(5).?);
    try std.testing.expectEqual(@as(usize, 0), sites.items[site_index].hash_links.items[1]);
}
