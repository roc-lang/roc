//! Source-only import facts shared by compilation and graph tools. Cache keys
//! depend on the digest of the normalized source bytes and compiler
//! compatibility, never paths.
const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const discovery = @import("module_discovery.zig");
const codec = @import("canonicalized_cache_entry.zig");
const CacheManager = @import("cache_manager.zig").CacheManager;
const collections = @import("collections");
const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const scope = "source-import-inventory-v4";

/// All slices belong to the allocator supplied to the producer or decoder.
/// Callers use an arena for the inventory's lifetime.
pub const Inventory = struct {
    is_application: bool = false,
    local: []const discovery.LocalImport,
    external: []const []const u8,
    public: []const discovery.PublicModule,
};

/// Name a source in the inventory format domain by the digest of its
/// normalized bytes. The caller took that digest where it read the source.
pub fn key(source_digest: base.SourceDigest) [32]u8 {
    var hash = base.Sha256.init(.{});
    hash.update(scope);
    hash.update(&source_digest.bytes);
    return hash.finalResult();
}

/// Extract source-local graph facts from a successful parser result.
pub fn fromAst(allocator: Allocator, ast: *const parse.AST) (Allocator.Error || error{ImportEscapesPackageRoot})!Inventory {
    const header = ast.store.getHeader(ast.store.getFile().header);
    const surface: discovery.PublicSurface = switch (header) {
        .package => |h| try discovery.extractPublicSurface(ast, h.exposes, .package, allocator),
        .platform => |h| try discovery.extractPublicSurface(ast, h.exposes, .platform, allocator),
        .app, .module, .hosted, .type_module, .default_app, .malformed => .{},
    };
    return .{
        .is_application = ast.rootAppKind() != .non_app,
        .local = try discovery.extractImportsFromDeclIndex(ast, allocator),
        .external = try discovery.extractQualifiedImportsFromDeclIndex(ast, allocator),
        .public = surface.modules.items,
    };
}

/// One public module alias: names in the record's `names` list.
const StoredPublic = extern struct {
    name: codec.StoredName,
    target: codec.StoredName,
};

/// The stored inventory, laid out like a canonicalized entry's parse-stage
/// record: fixed-size records in `SafeList` lists, with every name a range of
/// one byte list.
const Serialized = extern struct {
    local_imports: SafeList(codec.StoredLocalImport).Serialized,
    external_imports: SafeList(codec.StoredName).Serialized,
    public: SafeList(StoredPublic).Serialized,
    names: SafeList(u8).Serialized,
    is_application: u32,
    _padding: u32 = 0,
};

fn encode(allocator: Allocator, inventory: Inventory) Allocator.Error![]const u8 {
    var writer = CompactWriter.init();
    const header = try writer.appendAlloc(allocator, Serialized);

    var names = codec.NameTable{};
    var local = try SafeList(codec.StoredLocalImport).initCapacity(allocator, inventory.local.len);
    for (inventory.local) |item| _ = local.appendAssumeCapacity(.{
        .name = try names.add(allocator, item.import_name),
        .base = @backingInt(item.base),
        .parent_count = item.parent_count,
    });
    var external = try SafeList(codec.StoredName).initCapacity(allocator, inventory.external.len);
    for (inventory.external) |name| _ = external.appendAssumeCapacity(try names.add(allocator, name));
    var public = try SafeList(StoredPublic).initCapacity(allocator, inventory.public.len);
    for (inventory.public) |item| _ = public.appendAssumeCapacity(.{
        .name = try names.add(allocator, item.name),
        .target = try names.add(allocator, item.target),
    });

    try header.local_imports.serialize(&local, allocator, &writer);
    try header.external_imports.serialize(&external, allocator, &writer);
    try header.public.serialize(&public, allocator, &writer);
    try header.names.serialize(&names.bytes, allocator, &writer);
    header.is_application = @intFromBool(inventory.is_application);
    header._padding = 0;

    const bytes = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = writer.writeToBuffer(bytes) catch unreachable; // the buffer is exactly `total_bytes` long
    return bytes;
}

/// Read a stored inventory. `bytes` must be aligned to
/// `CompactWriter.SERIALIZATION_ALIGNMENT`; its names are borrowed, and the
/// slices of the result belong to `allocator`.
fn decode(allocator: Allocator, bytes: []const u8) (Allocator.Error || codec.DecodeError)!Inventory {
    if (bytes.len < @sizeOf(Serialized)) return error.CorruptParseStageRecord;
    if (@intFromPtr(bytes.ptr) % CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits() != 0) return error.CorruptParseStageRecord;
    const header: *const Serialized = @ptrCast(@alignCast(bytes.ptr));
    header.local_imports.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
    header.external_imports.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
    header.public.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
    header.names.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
    if (header.is_application > 1) return error.CorruptParseStageRecord;

    const base_addr = @intFromPtr(bytes.ptr);
    const record = codec.View{
        .diagnostics = &.{},
        .local_imports = header.local_imports.deserializeInto(base_addr).items(),
        .external_imports = header.external_imports.deserializeInto(base_addr).items(),
        .names = header.names.deserializeInto(base_addr).items(),
        .pin_validation = null,
    };
    const stored_public = header.public.deserializeInto(base_addr).items();

    const local = try allocator.alloc(discovery.LocalImport, record.local_imports.len);
    for (local, 0..) |*out, i| {
        const item = try record.localImport(i);
        out.* = .{ .import_name = item.import_name, .base = item.base, .parent_count = item.parent_count };
    }
    const external = try allocator.alloc([]const u8, record.external_imports.len);
    for (external, 0..) |*out, i| out.* = try record.externalImport(i);
    const public = try allocator.alloc(discovery.PublicModule, stored_public.len);
    for (public, stored_public) |*out, item| out.* = .{
        .name = try record.name(item.name),
        .target = try record.name(item.target),
        .nested_type = null, // Graph traversal needs the source module, not its selected type.
    };
    return .{ .is_application = header.is_application == 1, .local = local, .external = external, .public = public };
}

/// Load an inventory into the caller arena; stale or corrupt records miss.
pub fn load(allocator: Allocator, manager: *CacheManager, source_key: [32]u8) Allocator.Error!?Inventory {
    const loaded = manager.loadFamilyBytes(.canonicalized, source_key, scope) orelse return null;
    defer manager.allocator.free(loaded.bytes);
    const bytes = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, loaded.bytes.len);
    @memcpy(bytes, loaded.bytes);
    return decode(allocator, bytes) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.CorruptParseStageRecord => {
            manager.rejectEntry(.canonicalized, .canonicalized, source_key, scope, loaded.stamp, "it does not decode as a source import inventory", .{});
            return null;
        },
    };
}

/// Publish an inventory through the ordinary compiler artifact store.
pub fn store(allocator: Allocator, manager: *CacheManager, source_key: [32]u8, inventory: Inventory) Allocator.Error!void {
    if (!manager.config.enabled) return;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const bytes = try encode(arena.allocator(), inventory);
    manager.storeFamilyBytes(.canonicalized, source_key, bytes, scope, "source imports");
}

/// A successful compiler parse also warms graph-only tools. Invalid syntax
/// cannot certify a complete import inventory and is never cached here.
/// `source_digest` is the digest of the source `ast` was parsed from.
pub fn publishAst(allocator: Allocator, manager: *CacheManager, ast: *const parse.AST, source_digest: base.SourceDigest) Allocator.Error!void {
    if (!manager.config.enabled or ast.tokenize_diagnostics.items.len != 0 or ast.parse_diagnostics.items.len != 0) return;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const inventory = fromAst(arena.allocator(), ast) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ImportEscapesPackageRoot => return, // The compiler reports the invalid import; it cannot be cached.
    };
    try store(allocator, manager, key(source_digest), inventory);
}

test "loc inventory roundtrip preserves relative imports and public aliases and rejects truncation" {
    var arena = base.SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const inventory: Inventory = .{
        .local = &.{.{ .import_name = "../Foo", .base = .parent, .parent_count = 1 }},
        .external = &.{"dep.Foo"},
        .public = &.{.{ .name = "Alias", .target = "Internal/Foo", .nested_type = null }},
    };
    const bytes = try encode(a, inventory);
    const restored = try decode(a, bytes);
    try std.testing.expectEqualStrings("../Foo", restored.local[0].import_name);
    try std.testing.expectEqual(.parent, restored.local[0].base);
    try std.testing.expectEqual(@as(u16, 1), restored.local[0].parent_count);
    try std.testing.expectEqualStrings("dep.Foo", restored.external[0]);
    try std.testing.expectEqualStrings("Alias", restored.public[0].name);
    try std.testing.expectEqualStrings("Internal/Foo", restored.public[0].target);
    for (0..bytes.len) |len| try std.testing.expectError(error.CorruptParseStageRecord, decode(a, bytes[0..len]));
    const corrupt = try a.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bytes.len);
    @memcpy(corrupt, bytes);
    const header: *Serialized = @ptrCast(corrupt.ptr);
    header.is_application = 2;
    try std.testing.expectError(error.CorruptParseStageRecord, decode(a, corrupt));
    try std.testing.expect(!std.mem.eql(u8, &key(base.SourceDigest.of("import Foo")), &key(base.SourceDigest.of("import Bar"))));
}
