//! The container of one module's cached `roc test` outcomes under
//! `--specialize=no` (design.md "Test outcome files").
//!
//! A file holds the outcomes of one module's tests. Its identity names what
//! every outcome in it shares: the module's own source, the platform/app
//! binding it was checked under, the compiler, the native target and how the
//! tests were lowered and executed. Inside the file, each entry is keyed by one
//! test's checked outcome identity and validates itself, so a damaged entry is
//! a miss for that test alone. The entry bodies are opaque to this file.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Sha256 = base.Sha256;

const magic = "ROC_TEST_OUTCOME_FILE_V1";

/// Everything a module's outcome file is a function of besides each test's
/// own identity. Paths, rendering settings and worker counts are not inputs.
pub const Inputs = struct {
    /// The module's own source inputs (`CheckedModuleArtifact.source_hash`).
    /// Imported modules are not part of it: each test's identity covers what it
    /// reads from them, so a dependency edit keeps the file.
    source: [32]u8,
    /// The module's qualified name.
    module: []const u8,
    /// The platform/app binding the module was checked under.
    relation: ?[32]u8,
    compiler: []const u8,
    target: []const u8,
    strategy: []const u8,
    opt: []const u8,

    pub fn key(self: Inputs) [32]u8 {
        var hasher = Sha256.init(.{});
        hasher.update(magic);
        hasher.update(&self.source);
        hashString(&hasher, self.module);
        if (self.relation) |relation| {
            hasher.update(&[_]u8{1});
            hasher.update(&relation);
        } else {
            hasher.update(&[_]u8{0});
        }
        hashString(&hasher, self.compiler);
        hashString(&hasher, self.target);
        hashString(&hasher, self.strategy);
        // An outcome records what one execution mode did with the tests,
        // including how long they ran and whether they exhausted its resources.
        hashString(&hasher, self.opt);
        return hasher.finalResult();
    }
};

fn hashString(hasher: *Sha256, bytes: []const u8) void {
    var len: [8]u8 = undefined;
    std.mem.writeInt(u64, &len, bytes.len, .little);
    hasher.update(&len);
    hasher.update(bytes);
}

fn entryDigest(identity: [32]u8, body: []const u8) [32]u8 {
    var hasher = Sha256.init(.{});
    hasher.update(&identity);
    var len: [4]u8 = undefined;
    std.mem.writeInt(u32, &len, @intCast(body.len), .little);
    hasher.update(&len);
    hasher.update(body);
    return hasher.finalResult();
}

/// The entries of one file, by test identity. Bodies borrow the file's bytes.
pub const Entries = std.AutoHashMapUnmanaged([32]u8, []const u8);

/// The valid entries of `bytes`, a file published under `key`. A file for a
/// different key has none. Parsing stops at the first entry that does not
/// validate, so a truncated or damaged file keeps only its intact prefix.
pub fn parse(allocator: Allocator, bytes: []const u8, file_key: [32]u8) Allocator.Error!Entries {
    var entries = Entries{};
    errdefer entries.deinit(allocator);
    if (bytes.len < magic.len + 32) return entries;
    if (!std.mem.eql(u8, bytes[0..magic.len], magic)) return entries;
    if (!std.mem.eql(u8, bytes[magic.len..][0..32], &file_key)) return entries;
    var offset: usize = magic.len + 32;
    while (bytes.len - offset >= 32 + 4) {
        const identity = bytes[offset..][0..32].*;
        const len = std.mem.readInt(u32, bytes[offset + 32 ..][0..4], .little);
        const body_start = offset + 32 + 4;
        if (len > bytes.len - body_start or bytes.len - body_start - len < 32) break;
        const body = bytes[body_start..][0..len];
        if (!std.mem.eql(u8, &entryDigest(identity, body), bytes[body_start + len ..][0..32])) break;
        const entry = try entries.getOrPut(allocator, identity);
        // A writer never repeats an identity; a file that does is damaged.
        if (entry.found_existing) break;
        entry.value_ptr.* = body;
        offset = body_start + len + 32;
    }
    return entries;
}

/// Builds the bytes of one file.
pub const Builder = struct {
    bytes: std.ArrayList(u8) = .empty,
    identities: std.AutoHashMapUnmanaged([32]u8, void) = .empty,

    pub fn init(allocator: Allocator, file_key: [32]u8) Allocator.Error!Builder {
        var builder = Builder{};
        try builder.bytes.appendSlice(allocator, magic);
        try builder.bytes.appendSlice(allocator, &file_key);
        return builder;
    }

    pub fn deinit(self: *Builder, allocator: Allocator) void {
        self.bytes.deinit(allocator);
        self.identities.deinit(allocator);
    }

    /// Whether the file already holds an entry for `identity`.
    pub fn contains(self: *const Builder, identity: [32]u8) bool {
        return self.identities.contains(identity);
    }

    /// The number of entries appended so far.
    pub fn count(self: *const Builder) usize {
        return self.identities.count();
    }

    /// Append one entry. Each identity appears at most once.
    pub fn append(self: *Builder, allocator: Allocator, identity: [32]u8, body: []const u8) Allocator.Error!void {
        std.debug.assert(!self.identities.contains(identity));
        const len = std.math.cast(u32, body.len) orelse return error.OutOfMemory;
        try self.identities.put(allocator, identity, {});
        try self.bytes.appendSlice(allocator, &identity);
        var len_bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &len_bytes, len, .little);
        try self.bytes.appendSlice(allocator, &len_bytes);
        try self.bytes.appendSlice(allocator, body);
        try self.bytes.appendSlice(allocator, &entryDigest(identity, body));
    }
};

test "the file key names every input it is a function of" {
    const inputs = Inputs{
        .source = @splat(0x5a),
        .module = "Main",
        .relation = null,
        .compiler = "compiler-a",
        .target = "x64glibc",
        .strategy = "boxy",
        .opt = "dev",
    };
    var changed: [9]Inputs = @splat(inputs);
    changed[0].source = @splat(0x5b);
    changed[1].module = "Other";
    changed[2].relation = @as([32]u8, @splat(1));
    changed[3].compiler = "compiler-b";
    changed[4].target = "arm64glibc";
    changed[5].strategy = "lss";
    changed[6].opt = "interpreter";
    changed[7].opt = "speed";
    changed[8].relation = @as([32]u8, @splat(2));
    for (changed) |other| try std.testing.expect(!std.mem.eql(u8, &inputs.key(), &other.key()));
    try std.testing.expect(!std.mem.eql(u8, &changed[2].key(), &changed[8].key()));
    try std.testing.expectEqualSlices(u8, &inputs.key(), &inputs.key());
}

test "entries validate themselves and a damaged file keeps its intact prefix" {
    const allocator = std.testing.allocator;
    const file_key: [32]u8 = @splat(7);
    const first: [32]u8 = @splat(1);
    const second: [32]u8 = @splat(2);
    var builder = try Builder.init(allocator, file_key);
    defer builder.deinit(allocator);
    try builder.append(allocator, first, "first body");
    const first_end = builder.bytes.items.len;
    try builder.append(allocator, second, "");
    const bytes = builder.bytes.items;

    {
        var entries = try parse(allocator, bytes, file_key);
        defer entries.deinit(allocator);
        try std.testing.expectEqual(@as(u32, 2), entries.count());
        try std.testing.expectEqualStrings("first body", entries.get(first).?);
        try std.testing.expectEqualStrings("", entries.get(second).?);
    }
    {
        var entries = try parse(allocator, bytes, @splat(8));
        defer entries.deinit(allocator);
        try std.testing.expectEqual(@as(u32, 0), entries.count());
    }
    for (0..bytes.len) |len| {
        var entries = try parse(allocator, bytes[0..len], file_key);
        defer entries.deinit(allocator);
        const expected: u32 = if (len >= first_end) 1 else 0;
        try std.testing.expectEqual(expected, entries.count());
    }
    const damaged = try allocator.dupe(u8, bytes);
    defer allocator.free(damaged);
    for (magic.len + 32..bytes.len) |index| {
        damaged[index] ^= 0x40;
        defer damaged[index] ^= 0x40;
        var entries = try parse(allocator, damaged, file_key);
        defer entries.deinit(allocator);
        const expected: u32 = if (index >= first_end) 1 else 0;
        try std.testing.expectEqual(expected, entries.count());
        if (entries.get(first)) |body| try std.testing.expectEqualStrings("first body", body);
    }
}
