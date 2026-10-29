//! The per-module Boxy object cache (design.md "Caching Under `.boxy`").
//!
//! One entry per module, keyed by the module's checked-module cache key
//! extended with the target, the optimization level, whether expects are
//! compiled in, and the compiler's compatibility identity. An entry holds
//! everything a build needs of the module's object: the export interface
//! each importing module compiles its calls against, the hosted symbols the
//! object references, and the object code itself. Nothing in it refers to
//! another cache.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");

const Allocator = std.mem.Allocator;

/// The entry format: bumped whenever `encode`'s layout changes.
pub const format_version: u32 = 1;

const magic = "RBXO";

/// The cache scope the entries are filed under.
pub const scope = "boxy";

/// Names one module's object in the cache.
pub const Key = struct {
    bytes: [32]u8,
};

/// Every input a module's object is a function of, and nothing else
/// (design.md "Per-Module Output").
pub const KeyInput = struct {
    /// The module's checked-module cache key, which covers its content and
    /// every upstream module's checked key transitively.
    checked_key: [32]u8,
    /// For a module declaring hosted functions: the checked key of the
    /// platform module whose hosted section binds them to their host
    /// symbols, which the module's calls name.
    platform_key: ?[32]u8 = null,
    /// The target the object is compiled for.
    target: []const u8,
    /// The optimization level, which decides the LIR passes and backend.
    opt: []const u8,
    /// Whether inline expects are compiled in (`roc test`) or dropped.
    inline_expects: bool,
};

/// The key of a module's object for `input` under this compiler.
pub fn key(input: KeyInput) Key {
    var hasher = base.Sha256.init(.{});
    hasher.update("roc-boxy-object-key-v1");
    hasher.update(&build_options.compiler_compatibility_hash);
    hashU32(&hasher, format_version);
    hasher.update(&input.checked_key);
    if (input.platform_key) |platform_key| {
        hasher.update(&[_]u8{1});
        hasher.update(&platform_key);
    } else {
        hasher.update(&[_]u8{0});
    }
    hashLengthPrefixed(&hasher, input.target);
    hashLengthPrefixed(&hasher, input.opt);
    hasher.update(&[_]u8{@intFromBool(input.inline_expects)});
    return .{ .bytes = hasher.finalResult() };
}

/// The ownership signature and tail-drive summary ARC solved for one
/// exported worker (`LIR.LirProcSpec` `rc_*` fields), by the worker's
/// procedure identity.
pub const ExportRecord = struct {
    identity: [32]u8,
    rc_borrowed_params: u64,
    rc_ret_lenders: u64,
    rc_read_only_params: u64,
    rc_ret_unique_fields: u64,
    rc_ret_borrowed: bool,
    rc_ret_unique: bool,
    may_leave_pending: bool,
    rc_ret_conditions: []const u32,
};

/// One module's cached object.
pub const Entry = struct {
    exports: []const ExportRecord,
    /// The hosted function symbols the object references.
    hosted_symbols: []const []const u8,
    /// For the root program's object: the static data symbols the link
    /// keeps, and the symbols a shared library exports.
    link_root_symbols: []const []const u8 = &.{},
    app_export_symbols: []const []const u8 = &.{},
    object: []const u8,
};

/// Encode `entry` as the bytes filed in the cache. The caller owns the result.
pub fn encode(allocator: Allocator, entry: Entry) Allocator.Error![]u8 {
    var bytes: std.ArrayList(u8) = .empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, magic);
    try appendU32(&bytes, allocator, format_version);
    try appendU32(&bytes, allocator, @intCast(entry.exports.len));
    for (entry.exports) |record| {
        try bytes.appendSlice(allocator, &record.identity);
        try appendU64(&bytes, allocator, record.rc_borrowed_params);
        try appendU64(&bytes, allocator, record.rc_ret_lenders);
        try appendU64(&bytes, allocator, record.rc_read_only_params);
        try appendU64(&bytes, allocator, record.rc_ret_unique_fields);
        try bytes.append(allocator, @intFromBool(record.rc_ret_borrowed));
        try bytes.append(allocator, @intFromBool(record.rc_ret_unique));
        try bytes.append(allocator, @intFromBool(record.may_leave_pending));
        try appendU32(&bytes, allocator, @intCast(record.rc_ret_conditions.len));
        for (record.rc_ret_conditions) |word| try appendU32(&bytes, allocator, word);
    }
    for ([_][]const []const u8{ entry.hosted_symbols, entry.link_root_symbols, entry.app_export_symbols }) |symbols| {
        try appendU32(&bytes, allocator, @intCast(symbols.len));
        for (symbols) |symbol| {
            try appendU32(&bytes, allocator, @intCast(symbol.len));
            try bytes.appendSlice(allocator, symbol);
        }
    }
    try appendU64(&bytes, allocator, entry.object.len);
    try bytes.appendSlice(allocator, entry.object);
    return try bytes.toOwnedSlice(allocator);
}

/// Why cached bytes are not an entry: a file this compiler did not write
/// (another entry format, or a truncated or altered file). The cache
/// treats it as a miss.
pub const DecodeError = error{InvalidEntry} || Allocator.Error;

/// Decode an entry's bytes. Every slice of the result is allocated from
/// `allocator`; free them with `deinitEntry`.
pub fn decode(allocator: Allocator, bytes: []const u8) DecodeError!Entry {
    var reader = Reader{ .bytes = bytes };
    const found_magic = try reader.take(magic.len);
    if (!std.mem.eql(u8, found_magic, magic)) return error.InvalidEntry;
    if (try reader.word32() != format_version) return error.InvalidEntry;
    const export_count = try reader.word32();
    const exports = try allocator.alloc(ExportRecord, export_count);
    errdefer allocator.free(exports);
    var decoded_exports: usize = 0;
    errdefer for (exports[0..decoded_exports]) |record| allocator.free(record.rc_ret_conditions);
    for (exports) |*record| {
        const identity = try reader.take(32);
        record.* = .{
            .identity = identity[0..32].*,
            .rc_borrowed_params = try reader.word64(),
            .rc_ret_lenders = try reader.word64(),
            .rc_read_only_params = try reader.word64(),
            .rc_ret_unique_fields = try reader.word64(),
            .rc_ret_borrowed = try reader.flag(),
            .rc_ret_unique = try reader.flag(),
            .may_leave_pending = try reader.flag(),
            .rc_ret_conditions = &.{},
        };
        const condition_count = try reader.word32();
        const conditions = try allocator.alloc(u32, condition_count);
        errdefer allocator.free(conditions);
        for (conditions) |*word| word.* = try reader.word32();
        record.rc_ret_conditions = conditions;
        decoded_exports += 1;
    }
    const hosted_symbols = try decodeSymbols(allocator, &reader);
    errdefer freeSymbols(allocator, hosted_symbols);
    const link_root_symbols = try decodeSymbols(allocator, &reader);
    errdefer freeSymbols(allocator, link_root_symbols);
    const app_export_symbols = try decodeSymbols(allocator, &reader);
    errdefer freeSymbols(allocator, app_export_symbols);
    const object_len = try reader.word64();
    if (object_len > std.math.maxInt(usize)) return error.InvalidEntry;
    const object = try allocator.dupe(u8, try reader.take(@intCast(object_len)));
    errdefer allocator.free(object);
    if (reader.at != bytes.len) return error.InvalidEntry;
    return .{
        .exports = exports,
        .hosted_symbols = hosted_symbols,
        .link_root_symbols = link_root_symbols,
        .app_export_symbols = app_export_symbols,
        .object = object,
    };
}

fn decodeSymbols(allocator: Allocator, reader: *Reader) DecodeError![]const []const u8 {
    const count = try reader.word32();
    const symbols = try allocator.alloc([]const u8, count);
    errdefer allocator.free(symbols);
    var decoded: usize = 0;
    errdefer for (symbols[0..decoded]) |symbol| allocator.free(symbol);
    for (symbols) |*symbol| {
        const len = try reader.word32();
        symbol.* = try allocator.dupe(u8, try reader.take(len));
        decoded += 1;
    }
    return symbols;
}

fn freeSymbols(allocator: Allocator, symbols: []const []const u8) void {
    for (symbols) |symbol| allocator.free(symbol);
    allocator.free(symbols);
}

/// Free an entry `decode` returned.
pub fn deinitEntry(allocator: Allocator, entry: Entry) void {
    for (entry.exports) |record| allocator.free(record.rc_ret_conditions);
    allocator.free(entry.exports);
    freeSymbols(allocator, entry.hosted_symbols);
    freeSymbols(allocator, entry.link_root_symbols);
    freeSymbols(allocator, entry.app_export_symbols);
    allocator.free(entry.object);
}

const Reader = struct {
    bytes: []const u8,
    at: usize = 0,

    fn take(self: *Reader, len: usize) error{InvalidEntry}![]const u8 {
        if (self.bytes.len - self.at < len) return error.InvalidEntry;
        const slice = self.bytes[self.at..][0..len];
        self.at += len;
        return slice;
    }

    fn word32(self: *Reader) error{InvalidEntry}!u32 {
        return std.mem.readInt(u32, (try self.take(4))[0..4], .little);
    }

    fn word64(self: *Reader) error{InvalidEntry}!u64 {
        return std.mem.readInt(u64, (try self.take(8))[0..8], .little);
    }

    fn flag(self: *Reader) error{InvalidEntry}!bool {
        return switch ((try self.take(1))[0]) {
            0 => false,
            1 => true,
            else => error.InvalidEntry,
        };
    }
};

fn appendU32(bytes: *std.ArrayList(u8), allocator: Allocator, value: u32) Allocator.Error!void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    try bytes.appendSlice(allocator, &buffer);
}

fn appendU64(bytes: *std.ArrayList(u8), allocator: Allocator, value: u64) Allocator.Error!void {
    var buffer: [8]u8 = undefined;
    std.mem.writeInt(u64, &buffer, value, .little);
    try bytes.appendSlice(allocator, &buffer);
}

fn hashU32(hasher: *base.Sha256, value: u32) void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    hasher.update(&buffer);
}

/// Feed `bytes` with an explicit length, so no two different field splits
/// produce the same hash input.
fn hashLengthPrefixed(hasher: *base.Sha256, bytes: []const u8) void {
    var len_bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_bytes, bytes.len, .little);
    hasher.update(&len_bytes);
    hasher.update(bytes);
}

fn testInput() KeyInput {
    return .{ .checked_key = @splat(7), .target = "x64glibc", .opt = "dev", .inline_expects = false };
}

test "a Boxy object key is a function of exactly its declared inputs" {
    const baseline = key(testInput());
    try std.testing.expectEqual(baseline.bytes, key(testInput()).bytes);

    var changed_module = testInput();
    changed_module.checked_key = @splat(8);
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(changed_module).bytes));

    var with_platform = testInput();
    with_platform.platform_key = @splat(9);
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(with_platform).bytes));
    var other_platform = testInput();
    other_platform.platform_key = @splat(10);
    try std.testing.expect(!std.mem.eql(u8, &key(with_platform).bytes, &key(other_platform).bytes));

    var changed_target = testInput();
    changed_target.target = "arm64mac";
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(changed_target).bytes));

    var changed_opt = testInput();
    changed_opt.opt = "speed";
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(changed_opt).bytes));

    var changed_expects = testInput();
    changed_expects.inline_expects = true;
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(changed_expects).bytes));

    // A length prefix keeps one field's tail from reading as the next's head.
    var moved_boundary = testInput();
    moved_boundary.target = "x64glibcde";
    moved_boundary.opt = "v";
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(moved_boundary).bytes));
}

test "a Boxy object entry round-trips through its bytes" {
    const allocator = std.testing.allocator;
    const conditions = [_]u32{ 3, 9 };
    const entry: Entry = .{
        .exports = &.{
            .{ .identity = @splat(1), .rc_borrowed_params = 5, .rc_ret_lenders = 1, .rc_read_only_params = 4, .rc_ret_unique_fields = 0, .rc_ret_borrowed = true, .rc_ret_unique = false, .may_leave_pending = true, .rc_ret_conditions = &conditions },
            .{ .identity = @splat(2), .rc_borrowed_params = 0, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique_fields = 2, .rc_ret_borrowed = false, .rc_ret_unique = true, .may_leave_pending = false, .rc_ret_conditions = &.{} },
        },
        .hosted_symbols = &.{ "roc_fx_stdout_line", "roc_fx_stdin_line" },
        .link_root_symbols = &.{"roc__d3"},
        .app_export_symbols = &.{ "roc__main_for_host", "roc__d3" },
        .object = "not really an object",
    };
    const bytes = try encode(allocator, entry);
    defer allocator.free(bytes);
    const decoded = try decode(allocator, bytes);
    defer deinitEntry(allocator, decoded);
    try std.testing.expectEqualDeep(entry.exports, decoded.exports);
    try std.testing.expectEqualDeep(entry.hosted_symbols, decoded.hosted_symbols);
    try std.testing.expectEqualDeep(entry.link_root_symbols, decoded.link_root_symbols);
    try std.testing.expectEqualDeep(entry.app_export_symbols, decoded.app_export_symbols);
    try std.testing.expectEqualStrings(entry.object, decoded.object);

    // A truncated or altered file is no entry.
    try std.testing.expectError(error.InvalidEntry, decode(allocator, bytes[0 .. bytes.len - 1]));
    var altered = try allocator.dupe(u8, bytes);
    defer allocator.free(altered);
    altered[0] = 'X';
    try std.testing.expectError(error.InvalidEntry, decode(allocator, altered));
}
