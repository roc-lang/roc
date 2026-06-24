//! On-disk cache of source-pure canonicalized modules (CIR), keyed purely on the
//! module's source bytes.
//!
//! Because canonicalization is a pure function of a module's own source (cross-
//! module references are recorded symbolically as `CIR.ExternalRef` and resolved
//! later — see `src/canonicalize/resolve_externals.zig`), a module's CIR can be
//! looked up *before parsing*: read the source, hash it, and on a hit relocate the
//! cached `ModuleEnv` directly, skipping both parsing and canonicalization.
//!
//! This is distinct from the checked-module (type-info) cache, which is keyed on a
//! recursive hash that folds in every dependency's key and is consulted only once
//! a module's imports are available.
//!
//! Entry layout: a fixed header followed by the serialized `ModuleEnv` blob and a
//! scheduling tail (the parser's discovered imports + ingested-file fingerprints),
//! so a cache hit can schedule dependencies and revalidate ingested files exactly
//! as a cold parse would.

const std = @import("std");
const build_options = @import("build_options");
const cache_module = @import("cache_module.zig");

const Allocator = std.mem.Allocator;
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Content-addressed key for a module's CIR. Folds the compiler artifact hash in
/// so that a different compiler version (different builtins, different CIR shape)
/// never matches a stale entry.
pub const CirCacheKey = extern struct {
    bytes: [32]u8,

    /// Compute the key from the module's *normalized* source bytes — the same
    /// bytes `ModuleEnv.getSourceAll()` returns and that the cold store hashes.
    pub fn compute(normalized_source: []const u8) CirCacheKey {
        var source_hash: [32]u8 = undefined;
        Sha256.hash(normalized_source, &source_hash, .{});

        var hasher = Sha256.init(.{});
        hasher.update(key_domain_tag);
        hasher.update(&source_hash);
        hasher.update(&build_options.compiler_artifact_hash);
        return .{ .bytes = hasher.finalResult() };
    }
};

/// Domain-separation tag mixed into every CIR cache key.
const key_domain_tag = "roc-cir-key-v1";

/// Magic identifying a CIR cache entry on disk. Bumping it (or any layout change
/// reflected in `MODULE_ENV_VERSION_HASH`) makes old entries miss rather than
/// mis-deserialize.
pub const magic = "roc-cir-cache-v1";
pub const format_version: u64 = 1;

/// Header layout, in order:
///   magic (magic.len) | format_version u64 | ModuleEnv layout hash (32) |
///   key (32) | env_len u64 | tail_len u64
pub const header_len: usize =
    magic.len + 8 + 32 + 32 + 8 + 8;

/// The two variable-length bodies that follow the header.
pub const Bodies = struct {
    env_body: []const u8,
    tail_body: []const u8,
};

/// Write the entry header into `dest` (which must be at least `header_len`).
pub fn writeHeader(dest: []u8, key: CirCacheKey, env_len: usize, tail_len: usize) void {
    std.debug.assert(dest.len >= header_len);
    var offset: usize = 0;

    @memcpy(dest[offset..][0..magic.len], magic);
    offset += magic.len;

    std.mem.writeInt(u64, dest[offset..][0..8], format_version, .little);
    offset += 8;

    @memcpy(dest[offset..][0..32], &cache_module.MODULE_ENV_VERSION_HASH);
    offset += 32;

    @memcpy(dest[offset..][0..32], &key.bytes);
    offset += 32;

    std.mem.writeInt(u64, dest[offset..][0..8], env_len, .little);
    offset += 8;

    std.mem.writeInt(u64, dest[offset..][0..8], tail_len, .little);
}

/// Validate `entry` against the expected `key` and the running compiler's layout,
/// returning the two bodies on success or null on any mismatch (treated as a
/// cache miss, never a crash).
pub fn decodeEntry(key: CirCacheKey, entry: []const u8) ?Bodies {
    if (entry.len < header_len) return null;

    var offset: usize = 0;

    if (!std.mem.eql(u8, entry[offset..][0..magic.len], magic)) return null;
    offset += magic.len;

    if (std.mem.readInt(u64, entry[offset..][0..8], .little) != format_version) return null;
    offset += 8;

    if (!cache_module.expectModuleEnvVersion(entry[offset..][0..32])) return null;
    offset += 32;

    if (!std.mem.eql(u8, entry[offset..][0..32], &key.bytes)) return null;
    offset += 32;

    const env_len = std.math.cast(usize, std.mem.readInt(u64, entry[offset..][0..8], .little)) orelse return null;
    offset += 8;

    const tail_len = std.math.cast(usize, std.mem.readInt(u64, entry[offset..][0..8], .little)) orelse return null;
    offset += 8;

    // Bounds check: header + env + tail must fit exactly within the entry.
    const total = std.math.add(usize, header_len, env_len) catch return null;
    const total_with_tail = std.math.add(usize, total, tail_len) catch return null;
    if (total_with_tail > entry.len) return null;

    return .{
        .env_body = entry[header_len..][0..env_len],
        .tail_body = entry[total..][0..tail_len],
    };
}

/// A local import discovered during parsing: a module name and its resolved
/// filesystem path. Stored in the entry tail (authoritative) so a cache hit
/// schedules dependencies exactly as the cold parse did, without re-deriving
/// paths from the relocated env's import list.
pub const TailLocalImport = struct {
    module_name: []const u8,
    path: []const u8,
};

/// The scheduling lists recovered from an entry tail. Strings are owned by the
/// `gpa` passed to `decodeTail`; free via `deinitTail`.
pub const Tail = struct {
    local_imports: std.ArrayList(TailLocalImport),
    external_imports: std.ArrayList([]const u8),

    /// Free every string and both lists, using the same allocator passed to
    /// `decodeTail`.
    pub fn deinit(self: *Tail, gpa: Allocator) void {
        for (self.local_imports.items) |imp| {
            gpa.free(imp.module_name);
            gpa.free(imp.path);
        }
        self.local_imports.deinit(gpa);
        for (self.external_imports.items) |name| gpa.free(name);
        self.external_imports.deinit(gpa);
    }
};

fn appendLenPrefixed(gpa: Allocator, buf: *std.ArrayList(u8), bytes: []const u8) Allocator.Error!void {
    var len_le: [4]u8 = undefined;
    std.mem.writeInt(u32, &len_le, @intCast(bytes.len), .little);
    try buf.appendSlice(gpa, &len_le);
    try buf.appendSlice(gpa, bytes);
}

/// Encode the scheduling tail. Caller owns the returned buffer.
///
/// Format: `local_count u32` then per local `[name][path]` (each length-prefixed),
/// then `external_count u32` then per external `[name]` (length-prefixed).
pub fn encodeTail(
    gpa: Allocator,
    local_imports: []const TailLocalImport,
    external_imports: []const []const u8,
) Allocator.Error![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(gpa);

    var count_le: [4]u8 = undefined;
    std.mem.writeInt(u32, &count_le, @intCast(local_imports.len), .little);
    try buf.appendSlice(gpa, &count_le);
    for (local_imports) |imp| {
        try appendLenPrefixed(gpa, &buf, imp.module_name);
        try appendLenPrefixed(gpa, &buf, imp.path);
    }

    std.mem.writeInt(u32, &count_le, @intCast(external_imports.len), .little);
    try buf.appendSlice(gpa, &count_le);
    for (external_imports) |name| {
        try appendLenPrefixed(gpa, &buf, name);
    }

    return buf.toOwnedSlice(gpa);
}

const TailCursor = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn readU32(self: *TailCursor) ?u32 {
        if (self.offset + 4 > self.bytes.len) return null;
        const v = std.mem.readInt(u32, self.bytes[self.offset..][0..4], .little);
        self.offset += 4;
        return v;
    }

    fn readSlice(self: *TailCursor) ?[]const u8 {
        const len = self.readU32() orelse return null;
        if (self.offset + len > self.bytes.len) return null;
        const s = self.bytes[self.offset..][0..len];
        self.offset += len;
        return s;
    }
};

/// Decode a scheduling tail, duplicating all strings into `gpa`. Returns null on
/// any malformed/truncated tail (treated as a cache miss). On success the caller
/// owns the result and must call `Tail.deinit`.
pub fn decodeTail(gpa: Allocator, tail_body: []const u8) Allocator.Error!?Tail {
    return decodeTailInner(gpa, tail_body) catch |err| switch (err) {
        error.MalformedTail => null,
        else => |e| return e,
    };
}

/// Inner decoder that signals a malformed tail via `error.MalformedTail`, so a
/// single `errdefer` reliably frees any partial allocation (an optional `null`
/// return would not trigger `errdefer`).
fn decodeTailInner(gpa: Allocator, tail_body: []const u8) (Allocator.Error || error{MalformedTail})!Tail {
    var cursor = TailCursor{ .bytes = tail_body };

    var result = Tail{ .local_imports = .empty, .external_imports = .empty };
    errdefer result.deinit(gpa);

    const local_count = cursor.readU32() orelse return error.MalformedTail;
    try result.local_imports.ensureTotalCapacity(gpa, local_count);
    var i: u32 = 0;
    while (i < local_count) : (i += 1) {
        const name = cursor.readSlice() orelse return error.MalformedTail;
        const path = cursor.readSlice() orelse return error.MalformedTail;
        const name_copy = try gpa.dupe(u8, name);
        errdefer gpa.free(name_copy);
        const path_copy = try gpa.dupe(u8, path);
        result.local_imports.appendAssumeCapacity(.{ .module_name = name_copy, .path = path_copy });
    }

    const external_count = cursor.readU32() orelse return error.MalformedTail;
    try result.external_imports.ensureTotalCapacity(gpa, external_count);
    i = 0;
    while (i < external_count) : (i += 1) {
        const name = cursor.readSlice() orelse return error.MalformedTail;
        result.external_imports.appendAssumeCapacity(try gpa.dupe(u8, name));
    }

    return result;
}

test "CirCacheKey is deterministic and source-sensitive" {
    const a = CirCacheKey.compute("module [foo]\nfoo = 1\n");
    const a2 = CirCacheKey.compute("module [foo]\nfoo = 1\n");
    const b = CirCacheKey.compute("module [foo]\nfoo = 2\n");

    try std.testing.expectEqualSlices(u8, &a.bytes, &a2.bytes);
    try std.testing.expect(!std.mem.eql(u8, &a.bytes, &b.bytes));
}

test "writeHeader / decodeEntry round-trips and rejects mismatches" {
    const gpa = std.testing.allocator;
    const key = CirCacheKey.compute("hello");
    const env_body = "ENVENVENV";
    const tail_body = "TAIL";

    const entry = try gpa.alloc(u8, header_len + env_body.len + tail_body.len);
    defer gpa.free(entry);
    writeHeader(entry, key, env_body.len, tail_body.len);
    @memcpy(entry[header_len..][0..env_body.len], env_body);
    @memcpy(entry[header_len + env_body.len ..][0..tail_body.len], tail_body);

    const bodies = decodeEntry(key, entry) orelse return error.UnexpectedMiss;
    try std.testing.expectEqualSlices(u8, env_body, bodies.env_body);
    try std.testing.expectEqualSlices(u8, tail_body, bodies.tail_body);

    // Wrong key => miss.
    const other_key = CirCacheKey.compute("goodbye");
    try std.testing.expect(decodeEntry(other_key, entry) == null);

    // Corrupt magic => miss.
    const corrupt = try gpa.dupe(u8, entry);
    defer gpa.free(corrupt);
    corrupt[0] ^= 0xFF;
    try std.testing.expect(decodeEntry(key, corrupt) == null);

    // Truncated entry => miss, not a crash.
    try std.testing.expect(decodeEntry(key, entry[0 .. header_len - 1]) == null);
}

test "encodeTail / decodeTail round-trips" {
    const gpa = std.testing.allocator;
    const locals = [_]TailLocalImport{
        .{ .module_name = "Foo", .path = "/pkg/Foo.roc" },
        .{ .module_name = "Bar.Baz", .path = "/pkg/Bar/Baz.roc" },
    };
    const externals = [_][]const u8{ "pf.Stdout", "json.Json" };

    const tail = try encodeTail(gpa, &locals, &externals);
    defer gpa.free(tail);

    var decoded = (try decodeTail(gpa, tail)) orelse return error.UnexpectedMiss;
    defer decoded.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 2), decoded.local_imports.items.len);
    try std.testing.expectEqualStrings("Foo", decoded.local_imports.items[0].module_name);
    try std.testing.expectEqualStrings("/pkg/Foo.roc", decoded.local_imports.items[0].path);
    try std.testing.expectEqualStrings("Bar.Baz", decoded.local_imports.items[1].module_name);
    try std.testing.expectEqual(@as(usize, 2), decoded.external_imports.items.len);
    try std.testing.expectEqualStrings("pf.Stdout", decoded.external_imports.items[0]);
    try std.testing.expectEqualStrings("json.Json", decoded.external_imports.items[1]);
}

test "decodeTail rejects truncated tail" {
    const gpa = std.testing.allocator;
    const locals = [_]TailLocalImport{.{ .module_name = "Foo", .path = "/Foo.roc" }};
    const tail = try encodeTail(gpa, &locals, &.{});
    defer gpa.free(tail);

    // Any prefix shorter than the full tail must decode to a miss, never crash.
    var len: usize = 0;
    while (len < tail.len) : (len += 1) {
        var maybe = try decodeTail(gpa, tail[0..len]);
        if (maybe) |*t| t.deinit(gpa);
        try std.testing.expect(maybe == null);
    }
}
