//! Content-based module identity.
//!
//! A module's identity is a deep content hash: SHA-256 over the module's name,
//! its source bytes, and the identity hashes of its resolved imports (a Merkle
//! DAG over the transitive closure of module names and source bytes). It is a
//! pure function of that closure — no coordinator-assigned display strings, no
//! package names, no filesystem paths, no per-run state — so it satisfies the
//! caching constraint that any id serialized in a stage's artifact must be a
//! deterministic function of that stage's cache-key inputs.
//!
//! Two modules whose transitive closures are byte-identical share one identity:
//! nominal types declared by them unify across package versions, mirror URLs,
//! and vendored copies. Per design.md ("Identity provenance follows meaning
//! provenance"), this applies only to definitions whose entire meaning is
//! determined by module content; hosted functions and `provides` entrypoints
//! are identified by their platform-header symbol strings instead and must
//! never be merged by content.
//!
//! The module name participates in the hash because a type module's main type
//! takes its name from the module's file name (see design.md, Cache Boundary):
//! source bytes alone underdetermine meaning.
//!
//! In memory, 32-byte hashes exist only in per-store identity tables; every
//! use site holds a dense `Idx` into the owning store's table. Crossing a
//! store boundary is a rebase: read the 32-byte hash from the source table and
//! getOrInsert it into the destination table. Rebase is the single
//! cross-artifact identity resolution mechanism — no name matching.

const std = @import("std");

/// The deep content hash of a module: SHA-256 of the module name, source
/// bytes, and the sorted, deduplicated identity hashes of its resolved
/// imports.
pub const Hash = [32]u8;

/// Store-local dense index of a module identity hash in the owning store's
/// identity table. Only meaningful relative to that table; crossing stores
/// requires a rebase through the 32-byte hash.
pub const Idx = enum(u32) {
    _,

    /// Sentinel for "not yet assigned". Never valid as a table index.
    pub const NONE: Idx = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: Idx) bool {
        return self == NONE;
    }
};

/// Domain separator so module identity hashes can never collide with other
/// SHA-256 uses in the compiler (cache keys, type digests, ...).
const domain_tag = "roc-module-content-identity-v1";

/// Maximum direct-import count handled without heap allocation.
const inline_import_capacity = 32;

/// Compute the deep content identity of a module from its name, source bytes,
/// and the identity hashes of its resolved direct imports.
///
/// Import hashes are sorted and deduplicated internally, so the result is a
/// pure function of the SET of resolved import identities — independent of
/// resolution order, duplicate import entries, and data-structure iteration
/// order.
pub fn computeDeep(
    gpa: std.mem.Allocator,
    module_name: []const u8,
    source: []const u8,
    import_identities: []const Hash,
) std.mem.Allocator.Error!Hash {
    var inline_buf: [inline_import_capacity]Hash = undefined;
    const sorted: []Hash = if (import_identities.len <= inline_import_capacity)
        inline_buf[0..import_identities.len]
    else
        try gpa.alloc(Hash, import_identities.len);
    defer if (import_identities.len > inline_import_capacity) gpa.free(sorted);

    @memcpy(sorted, import_identities);
    std.mem.sortUnstable(Hash, sorted, {}, hashLessThan);

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(domain_tag);
    hashLenPrefixed(&hasher, module_name);
    hashLenPrefixed(&hasher, source);

    var distinct: u32 = 0;
    for (sorted, 0..) |import_hash, i| {
        if (i > 0 and std.mem.eql(u8, &import_hash, &sorted[i - 1])) continue;
        distinct += 1;
    }
    var count_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &count_bytes, distinct, .little);
    hasher.update(&count_bytes);
    for (sorted, 0..) |import_hash, i| {
        if (i > 0 and std.mem.eql(u8, &import_hash, &sorted[i - 1])) continue;
        hasher.update(&import_hash);
    }

    return hasher.finalResult();
}

/// Integer equality over two 32-byte identity hashes (a single 256-bit
/// compare — no byte-wise string comparison).
pub fn eql(a: *const Hash, b: *const Hash) bool {
    const a_bits: u256 = @bitCast(a.*);
    const b_bits: u256 = @bitCast(b.*);
    return a_bits == b_bits;
}

fn hashLessThan(_: void, a: Hash, b: Hash) bool {
    return std.mem.order(u8, &a, &b) == .lt;
}

fn hashLenPrefixed(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    var len_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &len_bytes, @intCast(bytes.len), .little);
    hasher.update(&len_bytes);
    hasher.update(bytes);
}

test "computeDeep is order- and duplicate-insensitive over imports" {
    const gpa = std.testing.allocator;
    const a: Hash = [_]u8{1} ** 32;
    const b: Hash = [_]u8{2} ** 32;

    const h1 = try computeDeep(gpa, "M", "x = 1", &.{ a, b });
    const h2 = try computeDeep(gpa, "M", "x = 1", &.{ b, a });
    const h3 = try computeDeep(gpa, "M", "x = 1", &.{ b, a, b, a });
    try std.testing.expectEqualSlices(u8, &h1, &h2);
    try std.testing.expectEqualSlices(u8, &h1, &h3);
}

test "computeDeep distinguishes name, source, and imports" {
    const gpa = std.testing.allocator;
    const a: Hash = [_]u8{1} ** 32;
    const base_hash = try computeDeep(gpa, "M", "x = 1", &.{a});

    try std.testing.expect(!std.mem.eql(u8, &base_hash, &(try computeDeep(gpa, "N", "x = 1", &.{a}))));
    try std.testing.expect(!std.mem.eql(u8, &base_hash, &(try computeDeep(gpa, "M", "x = 2", &.{a}))));
    try std.testing.expect(!std.mem.eql(u8, &base_hash, &(try computeDeep(gpa, "M", "x = 1", &.{}))));
}

test "computeDeep length-prefixes name and source (no concatenation ambiguity)" {
    const gpa = std.testing.allocator;
    const h1 = try computeDeep(gpa, "AB", "C", &.{});
    const h2 = try computeDeep(gpa, "A", "BC", &.{});
    try std.testing.expect(!std.mem.eql(u8, &h1, &h2));
}

test "computeDeep handles more imports than the inline buffer" {
    const gpa = std.testing.allocator;
    var many: [inline_import_capacity + 3]Hash = undefined;
    for (&many, 0..) |*h, i| {
        h.* = [_]u8{@intCast(i)} ** 32;
    }
    const h1 = try computeDeep(gpa, "M", "x = 1", &many);
    std.mem.reverse(Hash, &many);
    const h2 = try computeDeep(gpa, "M", "x = 1", &many);
    try std.testing.expectEqualSlices(u8, &h1, &h2);
}
