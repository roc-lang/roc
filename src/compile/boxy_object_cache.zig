//! The per-module Boxy object cache (design.md "Caching Under `.boxy`").
//!
//! One entry per module, keyed by what the module's object is a function of
//! (`boxy_interface.Digests.objectInput`) extended with the target, the
//! optimization level, whether expects are compiled in, and the compiler's
//! compatibility identity. An entry holds
//! everything a build needs of the module's code: the export interface
//! each importing module compiles its calls against, the hosted symbols the
//! code references, and the code itself in pieces (`backend.dev.UnitPieces`,
//! design.md "Unit Pieces And Whole-Program Dead Code"), and for a Linux
//! x86_64 target the code laid out once more as a unit image
//! (`backend.dev.UnitImage`), which a fast dev link places directly and
//! compile-time evaluation loads, with the image's code locations. The
//! image starts at a page-aligned offset in the entry, so a mapped entry
//! serves it in place. Nothing in it refers to another cache.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");

const Allocator = std.mem.Allocator;

/// The entry format: bumped whenever `encode`'s layout changes.
pub const format_version: u32 = 12;

const magic = "RBXO";

/// The offset alignment of an entry's image, so a page-aligned mapping of
/// the entry holds the image at a page-aligned address.
pub const image_alignment: usize = 4096;

/// The cache scope the entries are filed under.
pub const scope = "boxy";

/// Names one module's object in the cache.
pub const Key = struct {
    bytes: [32]u8,
};

/// Every input a module's object is a function of, and nothing else
/// (design.md "Per-Module Output").
pub const KeyInput = struct {
    /// The module's own source and stored values and its imports' interfaces
    /// (`boxy_interface.Digests.objectInput`).
    object_input: [32]u8,
    /// For a module declaring hosted functions: a digest of the platform
    /// header's hosted section, which binds them to the host symbols the
    /// module's calls name (`hostedBindingsDigest`).
    hosted_bindings: ?[32]u8 = null,
    /// The target the object is compiled for.
    target: []const u8,
    /// The optimization level, which decides the LIR passes and backend.
    opt: []const u8,
    /// Whether inline expects are compiled in (`roc test`) or dropped.
    inline_expects: bool,
    /// Whether the object is the root unit of a dev run image: its
    /// entrypoint wrappers take the `(ret_ptr, args_ptr)` convention the
    /// machine-code shim calls, and it materializes no provided data, which
    /// the shim's host does not take from the image.
    run_image: bool = false,
    /// Whether the object is a test unit (`roc test`): every inline expect
    /// it compiles counts into a site of its expect-site table, and it
    /// exports a wrapper per top-level expect of its modules.
    expect_sites: bool = false,
    /// For a wasm target: whether the platform reserves low memory, so the
    /// code checks its stack bounds.
    wasm_stack_checks: bool = false,
};

/// The key of a module's object for `input` under this compiler.
pub fn key(input: KeyInput) Key {
    var hasher = base.Sha256.init(.{});
    hasher.update("roc-boxy-object-key-v2");
    hasher.update(&build_options.compiler_compatibility_hash);
    hashU32(&hasher, format_version);
    hasher.update(&input.object_input);
    if (input.hosted_bindings) |bindings| {
        hasher.update(&[_]u8{1});
        hasher.update(&bindings);
    } else {
        hasher.update(&[_]u8{0});
    }
    hashLengthPrefixed(&hasher, input.target);
    hashLengthPrefixed(&hasher, input.opt);
    hasher.update(&[_]u8{@intFromBool(input.inline_expects)});
    hasher.update(&[_]u8{@intFromBool(input.run_image)});
    hasher.update(&[_]u8{@intFromBool(input.expect_sites)});
    hasher.update(&[_]u8{@intFromBool(input.wasm_stack_checks)});
    return .{ .bytes = hasher.finalResult() };
}

/// One binding of a platform header's hosted section, as `hostedBindingsDigest`
/// digests it.
pub const HostedBindingInput = struct {
    target_module: [32]u8,
    target_def: u32,
    symbol: []const u8,
};

/// The digest of a platform header's hosted section: every binding, in the
/// section's order. Two platform headers binding the same hosted functions
/// to the same host symbols digest alike, whatever app they pair with.
pub fn hostedBindingsDigest(bindings: []const HostedBindingInput) [32]u8 {
    var hasher = base.Sha256.init(.{});
    hasher.update("roc-boxy-hosted-bindings-v1");
    hashU32(&hasher, @intCast(bindings.len));
    for (bindings) |binding| {
        hasher.update(&binding.target_module);
        hashU32(&hasher, binding.target_def);
        hashLengthPrefixed(&hasher, binding.symbol);
    }
    return hasher.finalResult();
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

/// One inline `expect` a test unit counts, at the unit-local site id that
/// is its index in `Entry.expect_sites`. The module is named by its
/// content-derived lowering identity, never a workspace name or path.
pub const ExpectSiteRecord = struct {
    module: [32]u8,
    region_start: u32,
    region_end: u32,
};

/// One top-level `expect` a test unit exports a wrapper for: the module's
/// content-derived link identity, the root's index among the module's
/// test roots, and the image symbol the wrapper is exported under.
pub const TestRootRecord = struct {
    module: [32]u8,
    root_index: u32,
    symbol: u32,
};

/// One module's cached object. A wasm target's entry holds no pieces and no
/// code locations: its code is its wasm unit image (`WasmUnitImage`).
pub const Entry = struct {
    exports: []const ExportRecord,
    /// The hosted function symbols the object references.
    hosted_symbols: []const []const u8,
    /// For the root program's object: the static data symbols the link
    /// keeps, and the symbols a shared library exports.
    link_root_symbols: []const []const u8 = &.{},
    app_export_symbols: []const []const u8 = &.{},
    /// For the root unit of a dev run image: each platform entrypoint's host
    /// boundary digest (`layout.host_boundary_digest`), in entrypoint
    /// order, which the run's checked host identity covers.
    host_boundary_digests: []const [32]u8 = &.{},
    /// For a test unit: the inline expects its code counts, by site id.
    expect_sites: []const ExpectSiteRecord = &.{},
    /// For a test unit: the wrapper of each top-level expect it exports.
    test_roots: []const TestRootRecord = &.{},
    /// The module's code in pieces, encoded (`UnitPieces.encode`).
    pieces: []const u8,
    /// Where the unit image's code came from (`CodeLocations`, over offsets
    /// from the start of the image's text), which compile-time evaluation
    /// reports a failure in the loaded image with; empty without an image.
    /// It stays beside the image so image placement depends only on code.
    /// Module names are semantic; module identities bind locations to the
    /// current compilation's source environments, independent of checkout.
    code_locations: []const u8 = &.{},
    /// The module's unit image: a `UnitImage` for a target whose
    /// `boxy_unit.imageKind` is native, a `WasmUnitImage` for a wasm
    /// target, and empty for any other target.
    image: []const u8 = &.{},
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
    try appendU32(&bytes, allocator, @intCast(entry.host_boundary_digests.len));
    for (entry.host_boundary_digests) |*digest| try bytes.appendSlice(allocator, digest);
    try appendU32(&bytes, allocator, @intCast(entry.expect_sites.len));
    for (entry.expect_sites) |site| {
        try bytes.appendSlice(allocator, &site.module);
        try appendU32(&bytes, allocator, site.region_start);
        try appendU32(&bytes, allocator, site.region_end);
    }
    try appendU32(&bytes, allocator, @intCast(entry.test_roots.len));
    for (entry.test_roots) |root| {
        try bytes.appendSlice(allocator, &root.module);
        try appendU32(&bytes, allocator, root.root_index);
        try appendU32(&bytes, allocator, root.symbol);
    }
    try appendU64(&bytes, allocator, entry.pieces.len);
    try bytes.appendSlice(allocator, entry.pieces);
    try appendU64(&bytes, allocator, entry.code_locations.len);
    try bytes.appendSlice(allocator, entry.code_locations);
    try appendU64(&bytes, allocator, entry.image.len);
    if (entry.image.len != 0) {
        try bytes.appendNTimes(allocator, 0, std.mem.alignForward(usize, bytes.items.len, image_alignment) - bytes.items.len);
        try bytes.appendSlice(allocator, entry.image);
    }
    return try bytes.toOwnedSlice(allocator);
}

/// The bytes of an export record with no return conditions: its identity,
/// four 64-bit masks, three flags, and its condition count.
const min_export_record_size = 32 + 4 * @sizeOf(u64) + 3 + @sizeOf(u32);

/// The bytes of an expect-site or test-root record: a digest and two words.
const table_record_size = 32 + 2 * @sizeOf(u32);

/// Why cached bytes are not an entry: a file this compiler did not write
/// (another entry format, or a truncated or altered file). The cache
/// treats it as a miss.
pub const DecodeError = error{InvalidEntry} || Allocator.Error;

/// Decode an entry's bytes. Every slice of the result is allocated from
/// `allocator`; free them with `deinitEntry`. The image is copied to memory
/// aligned for `UnitImage.View`.
pub fn decode(allocator: Allocator, bytes: []const u8) DecodeError!Entry {
    const view = try decodeView(allocator, bytes);
    errdefer deinitEntryView(allocator, view);
    const pieces = try allocator.dupe(u8, view.pieces);
    errdefer allocator.free(pieces);
    const code_locations = try allocator.dupe(u8, view.code_locations);
    errdefer allocator.free(code_locations);
    const image = try allocator.alignedAlloc(u8, .@"16", view.image.len);
    @memcpy(image, view.image);
    var entry = view;
    entry.pieces = pieces;
    entry.code_locations = code_locations;
    entry.image = image;
    return entry;
}

/// Decode an entry's bytes, borrowing its pieces, code locations and image
/// from `bytes`; every other slice is allocated from `allocator`. Free them
/// with `deinitEntryView`. A page-aligned `bytes` holds the image page-aligned.
pub fn decodeView(allocator: Allocator, bytes: []const u8) DecodeError!Entry {
    var reader = Reader{ .bytes = bytes };
    const found_magic = try reader.take(magic.len);
    if (!std.mem.eql(u8, found_magic, magic)) return error.InvalidEntry;
    if (try reader.word32() != format_version) return error.InvalidEntry;
    const export_count = try reader.word32();
    if (export_count > (bytes.len - reader.at) / min_export_record_size) return error.InvalidEntry;
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
        if (condition_count > (bytes.len - reader.at) / @sizeOf(u32)) return error.InvalidEntry;
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
    const digest_count = try reader.word32();
    if (digest_count > (bytes.len - reader.at) / 32) return error.InvalidEntry;
    const host_boundary_digests = try allocator.alloc([32]u8, digest_count);
    errdefer allocator.free(host_boundary_digests);
    for (host_boundary_digests) |*digest| digest.* = (try reader.take(32))[0..32].*;
    const site_count = try reader.word32();
    if (site_count > (bytes.len - reader.at) / table_record_size) return error.InvalidEntry;
    const expect_sites = try allocator.alloc(ExpectSiteRecord, site_count);
    errdefer allocator.free(expect_sites);
    for (expect_sites) |*site| site.* = .{
        .module = (try reader.take(32))[0..32].*,
        .region_start = try reader.word32(),
        .region_end = try reader.word32(),
    };
    const root_count = try reader.word32();
    if (root_count > (bytes.len - reader.at) / table_record_size) return error.InvalidEntry;
    const test_roots = try allocator.alloc(TestRootRecord, root_count);
    errdefer allocator.free(test_roots);
    for (test_roots) |*root| root.* = .{
        .module = (try reader.take(32))[0..32].*,
        .root_index = try reader.word32(),
        .symbol = try reader.word32(),
    };
    const pieces_len = try reader.word64();
    if (pieces_len > std.math.maxInt(usize)) return error.InvalidEntry;
    const pieces = try reader.take(@intCast(pieces_len));
    const code_locations_len = try reader.word64();
    if (code_locations_len > std.math.maxInt(usize)) return error.InvalidEntry;
    const code_locations = try reader.take(@intCast(code_locations_len));
    const image_len = try reader.word64();
    if (image_len > std.math.maxInt(usize)) return error.InvalidEntry;
    if (image_len != 0) _ = try reader.take(std.mem.alignForward(usize, reader.at, image_alignment) - reader.at);
    const image = try reader.take(@intCast(image_len));
    if (reader.at != bytes.len) return error.InvalidEntry;
    return .{
        .exports = exports,
        .hosted_symbols = hosted_symbols,
        .link_root_symbols = link_root_symbols,
        .app_export_symbols = app_export_symbols,
        .host_boundary_digests = host_boundary_digests,
        .expect_sites = expect_sites,
        .test_roots = test_roots,
        .pieces = pieces,
        .code_locations = code_locations,
        .image = image,
    };
}

fn decodeSymbols(allocator: Allocator, reader: *Reader) DecodeError![]const []const u8 {
    const count = try reader.word32();
    if (count > (reader.bytes.len - reader.at) / @sizeOf(u32)) return error.InvalidEntry;
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
    allocator.free(entry.pieces);
    allocator.free(entry.code_locations);
    const image: []align(16) const u8 = @alignCast(entry.image);
    allocator.free(image);
    deinitEntryView(allocator, entry);
}

/// Free an entry `decodeView` returned, leaving the bytes it borrows.
pub fn deinitEntryView(allocator: Allocator, entry: Entry) void {
    for (entry.exports) |record| allocator.free(record.rc_ret_conditions);
    allocator.free(entry.exports);
    freeSymbols(allocator, entry.hosted_symbols);
    freeSymbols(allocator, entry.link_root_symbols);
    freeSymbols(allocator, entry.app_export_symbols);
    allocator.free(entry.host_boundary_digests);
    allocator.free(entry.expect_sites);
    allocator.free(entry.test_roots);
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
    return .{ .object_input = @splat(7), .target = "x64glibc", .opt = "dev", .inline_expects = false };
}

test "a Boxy object key is a function of exactly its declared inputs" {
    const baseline = key(testInput());
    try std.testing.expectEqual(baseline.bytes, key(testInput()).bytes);

    var changed_module = testInput();
    changed_module.object_input = @splat(8);
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(changed_module).bytes));

    var with_platform = testInput();
    with_platform.hosted_bindings = @splat(9);
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(with_platform).bytes));
    var other_platform = testInput();
    other_platform.hosted_bindings = @splat(10);
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

    var run_root = testInput();
    run_root.run_image = true;
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(run_root).bytes));

    var test_unit = testInput();
    test_unit.expect_sites = true;
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(test_unit).bytes));

    var stack_checked = testInput();
    stack_checked.wasm_stack_checks = true;
    try std.testing.expect(!std.mem.eql(u8, &baseline.bytes, &key(stack_checked).bytes));

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
        .host_boundary_digests = &.{ @splat(7), @splat(8) },
        .expect_sites = &.{ .{ .module = @splat(4), .region_start = 10, .region_end = 30 }, .{ .module = @splat(5), .region_start = 2, .region_end = 3 } },
        .test_roots = &.{.{ .module = @splat(4), .root_index = 1, .symbol = 17 }},
        .pieces = "not really pieces",
        .code_locations = "where the code came from",
        .image = "an image",
    };
    const bytes = try encode(allocator, entry);
    defer allocator.free(bytes);
    const decoded = try decode(allocator, bytes);
    defer deinitEntry(allocator, decoded);
    try std.testing.expectEqualDeep(entry.exports, decoded.exports);
    try std.testing.expectEqualDeep(entry.hosted_symbols, decoded.hosted_symbols);
    try std.testing.expectEqualDeep(entry.link_root_symbols, decoded.link_root_symbols);
    try std.testing.expectEqualDeep(entry.app_export_symbols, decoded.app_export_symbols);
    try std.testing.expectEqualDeep(entry.host_boundary_digests, decoded.host_boundary_digests);
    try std.testing.expectEqualDeep(entry.expect_sites, decoded.expect_sites);
    try std.testing.expectEqualDeep(entry.test_roots, decoded.test_roots);
    try std.testing.expectEqualStrings(entry.pieces, decoded.pieces);
    try std.testing.expectEqualStrings(entry.code_locations, decoded.code_locations);
    try std.testing.expectEqualStrings(entry.image, decoded.image);
    try std.testing.expectEqual(@as(usize, 0), (bytes.len - entry.image.len) % image_alignment);

    // A truncated or altered file is no entry.
    try std.testing.expectError(error.InvalidEntry, decode(allocator, bytes[0 .. bytes.len - 1]));
    var altered = try allocator.dupe(u8, bytes);
    defer allocator.free(altered);
    altered[0] = 'X';
    try std.testing.expectError(error.InvalidEntry, decode(allocator, altered));
}

test "a hosted bindings digest is a function of the bindings in order" {
    const a: [32]u8 = @splat(1);
    const bindings = [_]HostedBindingInput{
        .{ .target_module = a, .target_def = 3, .symbol = "roc_fx_line" },
        .{ .target_module = a, .target_def = 4, .symbol = "roc_fx_read" },
    };
    const baseline = hostedBindingsDigest(&bindings);
    try std.testing.expectEqual(baseline, hostedBindingsDigest(&bindings));
    const reordered = [_]HostedBindingInput{ bindings[1], bindings[0] };
    try std.testing.expect(!std.mem.eql(u8, &baseline, &hostedBindingsDigest(&reordered)));
    var renamed = bindings;
    renamed[0].symbol = "roc_fx_line2";
    try std.testing.expect(!std.mem.eql(u8, &baseline, &hostedBindingsDigest(&renamed)));
    try std.testing.expect(!std.mem.eql(u8, &baseline, &hostedBindingsDigest(bindings[0..1])));
}

test "a count larger than the bytes behind it is a miss, not an allocation" {
    const gpa = std.testing.allocator;
    const encoded = try encode(gpa, .{ .exports = &.{}, .hosted_symbols = &.{}, .pieces = "pieces" });
    defer gpa.free(encoded);
    const count_offset = magic.len + @sizeOf(u32);
    for ([_]usize{ count_offset, count_offset + @sizeOf(u32) }) |offset| {
        const damaged = try gpa.dupe(u8, encoded);
        defer gpa.free(damaged);
        std.mem.writeInt(u32, damaged[offset..][0..4], std.math.maxInt(u32), .little);
        try std.testing.expectError(error.InvalidEntry, decode(gpa, damaged));
    }
}
