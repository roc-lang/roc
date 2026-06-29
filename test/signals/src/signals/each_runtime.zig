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

pub const DiffResult = struct {
    scope_ids: []u64,
    row_items_changed: []bool,
    scope_created: []bool,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,

    pub fn deinit(self: DiffResult, allocator: std.mem.Allocator) void {
        allocator.free(self.scope_ids);
        allocator.free(self.row_items_changed);
        allocator.free(self.scope_created);
        allocator.free(self.removed_scope_ids);
    }
};

pub const RenderSegment = struct {
    scope_id: u64,
    start: usize,
    len: usize,
};

pub const RenderMove = struct {
    old_start: usize,
    new_start: usize,
    len: usize,
};

const NextKeyIndexError = error{
    OutOfMemory,
    DuplicateKey,
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

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

pub fn diffPreservesSurvivorRenderOrder(old_render_rows: []const u64, next_scope_ids: []const u64) bool {
    var old_index: usize = 0;
    for (next_scope_ids) |next_scope_id| {
        if (!u64SliceContains(old_render_rows, next_scope_id)) continue;
        while (old_index < old_render_rows.len and !u64SliceContains(next_scope_ids, old_render_rows[old_index])) {
            old_index += 1;
        }
        if (old_index >= old_render_rows.len) return false;
        if (old_render_rows[old_index] != next_scope_id) return false;
        old_index += 1;
    }
    return true;
}

pub fn renderSegmentScopeIds(allocator: std.mem.Allocator, segments: []const RenderSegment) []u64 {
    const ids = allocator.alloc(u64, segments.len) catch @panic("out of memory");
    for (segments, ids) |segment, *id| {
        id.* = segment.scope_id;
    }
    return ids;
}

pub fn renderInsertIndexForRowRanges(site_render_insert_index: usize, row_ranges: *const std.AutoHashMapUnmanaged(u64, RenderSegment), next_scope_ids: []const u64, row_index: usize) usize {
    if (row_index >= next_scope_ids.len) @panic("each row insertion index was requested outside the next row order");

    if (row_ranges.get(next_scope_ids[row_index])) |existing| {
        return existing.start;
    }

    var next_index = row_index + 1;
    while (next_index < next_scope_ids.len) : (next_index += 1) {
        if (row_ranges.get(next_scope_ids[next_index])) |next| {
            return next.start;
        }
    }

    var previous_index = row_index;
    while (previous_index > 0) {
        previous_index -= 1;
        if (row_ranges.get(next_scope_ids[previous_index])) |previous| {
            return previous.start + previous.len;
        }
    }

    return site_render_insert_index;
}

fn adjustedRenderInsertIndex(old_index: usize, replace_index: usize, removed_count: usize, replacement_count: usize) usize {
    if (removed_count == 0) {
        if (old_index < replace_index) return old_index;
        return old_index + replacement_count;
    }
    if (old_index <= replace_index) return old_index;
    if (old_index < replace_index + removed_count) @panic("scope site inside replaced scope was not removed");
    return old_index - removed_count + replacement_count;
}

pub fn adjustRenderRanges(row_ranges: *std.AutoHashMapUnmanaged(u64, RenderSegment), replace_index: usize, removed_count: usize, replacement_count: usize) void {
    var range_iterator = row_ranges.iterator();
    while (range_iterator.next()) |entry| {
        entry.value_ptr.start = adjustedRenderInsertIndex(entry.value_ptr.start, replace_index, removed_count, replacement_count);
    }
}

pub fn updateRenderRange(row_ranges: *std.AutoHashMapUnmanaged(u64, RenderSegment), allocator: std.mem.Allocator, scope_id: u64, render_insert_index: usize, removed_count: usize, replacement_count: usize) void {
    const removed_range = row_ranges.fetchRemove(scope_id);
    const old_len = if (removed_range) |entry| entry.value.len else 0;
    if (old_len != removed_count) @panic("each row render range length did not match splice removal count");
    adjustRenderRanges(row_ranges, render_insert_index, old_len, replacement_count);
    if (replacement_count != 0) {
        row_ranges.put(allocator, scope_id, .{
            .scope_id = scope_id,
            .start = render_insert_index,
            .len = replacement_count,
        }) catch @panic("out of memory");
    }
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

pub fn syncRows(
    allocator: std.mem.Allocator,
    sites: *std.ArrayListUnmanaged(Site),
    memberships: *std.ArrayListUnmanaged(?Membership),
    site_index: usize,
    parent_scope_id: u64,
    site_ordinal: u64,
    keys: anytype,
    items: anytype,
    hooks: anytype,
) DiffResult {
    if (keys.len != items.len) @panic("Ui.each keyed scope received mismatched key and item lists");
    if (site_index >= sites.items.len) @panic("each row site index exceeded site table");

    const existing_len = sites.items[site_index].scope_ids.items.len;
    hooks.recordEachSync(keys.len, existing_len);

    const key_hashes = allocator.alloc(u64, keys.len) catch @panic("out of memory");
    defer allocator.free(key_hashes);
    for (keys, 0..) |key, key_index| {
        key_hashes[key_index] = hooks.hashKey(key);
    }

    var next_hash_heads: std.AutoHashMapUnmanaged(u64, usize) = .{};
    defer next_hash_heads.deinit(allocator);

    const next_hash_links = allocator.alloc(usize, keys.len) catch @panic("out of memory");
    defer allocator.free(next_hash_links);
    indexNextKeys(allocator, &next_hash_heads, next_hash_links, key_hashes, keys, hooks) catch |err| switch (err) {
        error.OutOfMemory => @panic("out of memory"),
        error.DuplicateKey => @panic("keyed row diff operation failed"),
    };

    const matched_existing = allocator.alloc(bool, existing_len) catch @panic("out of memory");
    defer allocator.free(matched_existing);
    @memset(matched_existing, false);

    var next_scope_ids = allocator.alloc(u64, keys.len) catch @panic("out of memory");
    errdefer allocator.free(next_scope_ids);
    var row_items_changed = allocator.alloc(bool, keys.len) catch @panic("out of memory");
    errdefer allocator.free(row_items_changed);
    var scope_created = allocator.alloc(bool, keys.len) catch @panic("out of memory");
    errdefer allocator.free(scope_created);
    var removed_scope_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer removed_scope_ids.deinit(allocator);

    var rows_reused: u64 = 0;
    var rows_created: u64 = 0;
    var row_items_unchanged: u64 = 0;
    var row_items_updated: u64 = 0;

    for (key_hashes, keys, items, 0..) |hash, key, item, key_index| {
        var matched_scope_id: ?u64 = null;
        const site = &sites.items[site_index];
        if (site.hash_heads.get(hash)) |head| {
            var existing_index = head;
            while (existing_index != missing_row_index) {
                if (existing_index < existing_len and !matched_existing[existing_index]) {
                    const scope_id = site.scope_ids.items[existing_index];
                    if (hooks.existingKeyEquals(scope_id, key)) {
                        matched_existing[existing_index] = true;
                        matched_scope_id = scope_id;
                        break;
                    }
                }
                existing_index = site.hash_links.items[existing_index];
            }
        }

        if (matched_scope_id) |scope_id| {
            next_scope_ids[key_index] = scope_id;
            scope_created[key_index] = false;
            rows_reused += 1;

            const row_item_equal = hooks.rowItemEquals(scope_id, item);
            hooks.replaceRowKey(scope_id, hash, key);
            hooks.replaceRowItem(scope_id, item);
            if (row_item_equal) {
                row_items_changed[key_index] = false;
                row_items_unchanged += 1;
            } else {
                row_items_changed[key_index] = true;
                row_items_updated += 1;
            }
        } else {
            next_scope_ids[key_index] = std.math.maxInt(u64);
            row_items_changed[key_index] = true;
            scope_created[key_index] = true;
            rows_created += 1;
        }
    }

    {
        const site = &sites.items[site_index];
        for (site.scope_ids.items[0..existing_len], 0..) |scope_id, existing_index| {
            if (matched_existing[existing_index]) continue;
            removed_scope_ids.append(allocator, scope_id) catch @panic("out of memory");
        }
    }

    for (scope_created, key_hashes, keys, items, 0..) |created, hash, key, item, key_index| {
        if (!created) continue;
        next_scope_ids[key_index] = hooks.createRow(parent_scope_id, site_ordinal, hash, key, item);
    }

    for (removed_scope_ids.items) |scope_id| {
        hooks.disposeScope(scope_id);
    }

    replaceSiteRows(allocator, sites, memberships, site_index, next_scope_ids, hooks);
    const removed = removed_scope_ids.toOwnedSlice(allocator) catch @panic("out of memory");
    errdefer allocator.free(removed);

    hooks.recordRows(rows_reused, rows_created, @intCast(removed.len));

    return .{
        .scope_ids = next_scope_ids,
        .row_items_changed = row_items_changed,
        .scope_created = scope_created,
        .removed_scope_ids = removed,
        .rows_reused = rows_reused,
        .rows_created = rows_created,
        .rows_removed = @intCast(removed.len),
        .row_items_unchanged = row_items_unchanged,
        .row_items_updated = row_items_updated,
    };
}

fn indexNextKeys(
    allocator: std.mem.Allocator,
    next_hash_heads: *std.AutoHashMapUnmanaged(u64, usize),
    next_hash_links: []usize,
    key_hashes: []const u64,
    keys: anytype,
    hooks: anytype,
) NextKeyIndexError!void {
    @memset(next_hash_links, missing_row_index);

    for (key_hashes, 0..) |hash, key_index| {
        if (next_hash_heads.get(hash)) |head| {
            var previous_index = head;
            while (previous_index != missing_row_index) {
                if (hooks.nextKeysEqual(keys[previous_index], keys[key_index])) {
                    return error.DuplicateKey;
                }
                previous_index = next_hash_links[previous_index];
            }
        }

        const entry = next_hash_heads.getOrPut(allocator, hash) catch return error.OutOfMemory;
        if (entry.found_existing) {
            next_hash_links[key_index] = entry.value_ptr.*;
        }
        entry.value_ptr.* = key_index;
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

const TestSyncHooks = struct {
    keys_by_scope: []u64,
    items_by_scope: []u64,
    next_scope_id: u64,
    forced_hash: ?u64 = null,
    disposed_scopes: std.ArrayListUnmanaged(u64) = .empty,
    sync_next_len: usize = 0,
    sync_existing_len: usize = 0,
    rows_reused: u64 = 0,
    rows_created: u64 = 0,
    rows_removed: u64 = 0,

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.disposed_scopes.deinit(allocator);
    }

    pub fn recordEachSync(self: *@This(), next_len: usize, existing_len: usize) void {
        self.sync_next_len = next_len;
        self.sync_existing_len = existing_len;
    }

    fn hashForKey(self: *@This(), key: u64) u64 {
        return self.forced_hash orelse key;
    }

    fn expectHash(self: *@This(), hash: u64, key: u64) void {
        if (hash != self.hashForKey(key)) @panic("test key hash must match key");
    }

    pub fn hashKey(self: *@This(), key: u64) u64 {
        return self.hashForKey(key);
    }

    pub fn nextKeysEqual(_: *@This(), left: u64, right: u64) bool {
        return left == right;
    }

    pub fn existingKeyEquals(self: *@This(), scope_id: u64, key: u64) bool {
        return self.keys_by_scope[@intCast(scope_id)] == key;
    }

    pub fn rowItemEquals(self: *@This(), scope_id: u64, item: u64) bool {
        return self.items_by_scope[@intCast(scope_id)] == item;
    }

    pub fn replaceRowKey(self: *@This(), scope_id: u64, hash: u64, key: u64) void {
        self.expectHash(hash, key);
        self.keys_by_scope[@intCast(scope_id)] = key;
    }

    pub fn replaceRowItem(self: *@This(), scope_id: u64, item: u64) void {
        self.items_by_scope[@intCast(scope_id)] = item;
    }

    pub fn createRow(self: *@This(), parent_scope_id: u64, site_ordinal: u64, hash: u64, key: u64, item: u64) u64 {
        if (parent_scope_id != 1 or site_ordinal != 2) @panic("test row was created for the wrong site");
        self.expectHash(hash, key);
        const scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        self.keys_by_scope[@intCast(scope_id)] = key;
        self.items_by_scope[@intCast(scope_id)] = item;
        return scope_id;
    }

    pub fn disposeScope(self: *@This(), scope_id: u64) void {
        self.disposed_scopes.append(std.testing.allocator, scope_id) catch @panic("out of memory");
    }

    pub fn rowKeyHash(self: *@This(), scope_id: u64) u64 {
        return self.hashForKey(self.keys_by_scope[@intCast(scope_id)]);
    }

    pub fn recordRows(self: *@This(), rows_reused: u64, rows_created: u64, rows_removed: u64) void {
        self.rows_reused = rows_reused;
        self.rows_created = rows_created;
        self.rows_removed = rows_removed;
    }
};

test "each runtime detects duplicate next keys through typed equality" {
    var next_hash_heads: std.AutoHashMapUnmanaged(u64, usize) = .{};
    defer next_hash_heads.deinit(std.testing.allocator);

    var next_hash_links = [_]usize{missing_row_index} ** 2;
    const key_hashes = [_]u64{ 7, 7 };
    const keys = [_]u64{ 1, 1 };

    var keys_by_scope = [_]u64{};
    var items_by_scope = [_]u64{};
    var hooks = TestSyncHooks{
        .keys_by_scope = &keys_by_scope,
        .items_by_scope = &items_by_scope,
        .next_scope_id = 0,
        .forced_hash = 7,
    };
    defer hooks.deinit(std.testing.allocator);

    try std.testing.expectError(
        error.DuplicateKey,
        indexNextKeys(std.testing.allocator, &next_hash_heads, &next_hash_links, &key_hashes, &keys, &hooks),
    );
}

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

test "each runtime sync reuses creates removes and rebuilds rows" {
    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var indexes: SiteIndexMap = .empty;
    var memberships: std.ArrayListUnmanaged(?Membership) = .empty;
    defer clearSites(std.testing.allocator, &sites, &indexes, &memberships);

    const site_index = ensureSiteIndex(std.testing.allocator, &sites, &indexes, 1, 2);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 10, 1);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 11, 2);

    var keys_by_scope = [_]u64{0} ** 16;
    var items_by_scope = [_]u64{0} ** 16;
    keys_by_scope[10] = 1;
    items_by_scope[10] = 100;
    keys_by_scope[11] = 2;
    items_by_scope[11] = 200;

    var hooks = TestSyncHooks{
        .keys_by_scope = &keys_by_scope,
        .items_by_scope = &items_by_scope,
        .next_scope_id = 12,
    };
    defer hooks.deinit(std.testing.allocator);

    const keys = [_]u64{ 2, 3 };
    const items = [_]u64{ 200, 300 };
    const diff = syncRows(std.testing.allocator, &sites, &memberships, site_index, 1, 2, &keys, &items, &hooks);
    defer diff.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(u64, &.{ 11, 12 }, diff.scope_ids);
    try std.testing.expectEqualSlices(bool, &.{ false, true }, diff.row_items_changed);
    try std.testing.expectEqualSlices(bool, &.{ false, true }, diff.scope_created);
    try std.testing.expectEqualSlices(u64, &.{10}, diff.removed_scope_ids);
    try std.testing.expectEqualSlices(u64, &.{10}, hooks.disposed_scopes.items);
    try std.testing.expectEqual(@as(u64, 1), diff.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), diff.rows_created);
    try std.testing.expectEqual(@as(u64, 1), diff.rows_removed);
    try std.testing.expectEqual(@as(u64, 1), diff.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), diff.row_items_updated);
    try std.testing.expectEqual(@as(usize, 2), hooks.sync_next_len);
    try std.testing.expectEqual(@as(usize, 2), hooks.sync_existing_len);
    try std.testing.expectEqualSlices(u64, &.{ 11, 12 }, sites.items[site_index].scope_ids.items);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 0 }, memberships.items[11].?);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 1 }, memberships.items[12].?);
    try std.testing.expectEqual(@as(?Membership, null), memberships.items[10]);
    try std.testing.expectEqual(@as(u64, 1), hooks.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), hooks.rows_created);
    try std.testing.expectEqual(@as(u64, 1), hooks.rows_removed);
}

test "each runtime sync resolves hash collisions with typed equality" {
    var sites: std.ArrayListUnmanaged(Site) = .empty;
    var indexes: SiteIndexMap = .empty;
    var memberships: std.ArrayListUnmanaged(?Membership) = .empty;
    defer clearSites(std.testing.allocator, &sites, &indexes, &memberships);

    const site_index = ensureSiteIndex(std.testing.allocator, &sites, &indexes, 1, 2);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 10, 0);
    appendRowToSiteIndex(std.testing.allocator, &sites, &memberships, site_index, 11, 0);

    var keys_by_scope = [_]u64{0} ** 16;
    var items_by_scope = [_]u64{0} ** 16;
    keys_by_scope[10] = 1;
    items_by_scope[10] = 100;
    keys_by_scope[11] = 2;
    items_by_scope[11] = 200;

    var hooks = TestSyncHooks{
        .keys_by_scope = &keys_by_scope,
        .items_by_scope = &items_by_scope,
        .next_scope_id = 12,
        .forced_hash = 0,
    };
    defer hooks.deinit(std.testing.allocator);

    const keys = [_]u64{ 2, 1 };
    const items = [_]u64{ 200, 100 };
    const diff = syncRows(std.testing.allocator, &sites, &memberships, site_index, 1, 2, &keys, &items, &hooks);
    defer diff.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(u64, &.{ 11, 10 }, diff.scope_ids);
    try std.testing.expectEqualSlices(bool, &.{ false, false }, diff.row_items_changed);
    try std.testing.expectEqualSlices(bool, &.{ false, false }, diff.scope_created);
    try std.testing.expectEqualSlices(u64, &.{}, diff.removed_scope_ids);
    try std.testing.expectEqual(@as(u64, 2), diff.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), diff.rows_created);
    try std.testing.expectEqual(@as(u64, 0), diff.rows_removed);
    try std.testing.expectEqualSlices(u64, &.{ 11, 10 }, sites.items[site_index].scope_ids.items);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 0 }, memberships.items[11].?);
    try std.testing.expectEqual(Membership{ .site_index = site_index, .row_index = 1 }, memberships.items[10].?);
    try std.testing.expectEqual(@as(usize, 1), sites.items[site_index].hash_heads.get(0).?);
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

test "each runtime render segments expose scope order" {
    const segments = [_]RenderSegment{
        .{ .scope_id = 10, .start = 2, .len = 3 },
        .{ .scope_id = 11, .start = 5, .len = 1 },
    };
    const scope_ids = renderSegmentScopeIds(std.testing.allocator, &segments);
    defer std.testing.allocator.free(scope_ids);

    try std.testing.expectEqualSlices(u64, &.{ 10, 11 }, scope_ids);
    try std.testing.expect(diffPreservesSurvivorRenderOrder(&.{ 10, 11, 12 }, &.{ 10, 12 }));
    try std.testing.expect(!diffPreservesSurvivorRenderOrder(&.{ 10, 11, 12 }, &.{ 12, 10 }));
}

test "each runtime render range helpers choose insertion points and adjust ranges" {
    var ranges: std.AutoHashMapUnmanaged(u64, RenderSegment) = .{};
    defer ranges.deinit(std.testing.allocator);

    ranges.put(std.testing.allocator, 10, .{ .scope_id = 10, .start = 4, .len = 2 }) catch @panic("out of memory");
    ranges.put(std.testing.allocator, 12, .{ .scope_id = 12, .start = 9, .len = 1 }) catch @panic("out of memory");

    try std.testing.expectEqual(@as(usize, 4), renderInsertIndexForRowRanges(3, &ranges, &.{ 10, 11, 12 }, 0));
    try std.testing.expectEqual(@as(usize, 9), renderInsertIndexForRowRanges(3, &ranges, &.{ 10, 11, 12 }, 1));
    try std.testing.expectEqual(@as(usize, 10), renderInsertIndexForRowRanges(3, &ranges, &.{ 10, 12, 11 }, 2));
    try std.testing.expectEqual(@as(usize, 4), renderInsertIndexForRowRanges(3, &ranges, &.{ 11, 10, 12 }, 0));
    try std.testing.expectEqual(@as(usize, 3), renderInsertIndexForRowRanges(3, &ranges, &.{11}, 0));

    updateRenderRange(&ranges, std.testing.allocator, 10, 4, 2, 3);
    try std.testing.expectEqual(RenderSegment{ .scope_id = 10, .start = 4, .len = 3 }, ranges.get(10).?);
    try std.testing.expectEqual(RenderSegment{ .scope_id = 12, .start = 10, .len = 1 }, ranges.get(12).?);

    updateRenderRange(&ranges, std.testing.allocator, 10, 4, 3, 0);
    try std.testing.expectEqual(@as(?RenderSegment, null), ranges.get(10));
    try std.testing.expectEqual(RenderSegment{ .scope_id = 12, .start = 7, .len = 1 }, ranges.get(12).?);
}
