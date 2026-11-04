//! Source-only import facts shared by compilation and graph tools. Cache keys
//! depend on normalized source bytes and compiler compatibility, never paths.
const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const discovery = @import("module_discovery.zig");
const codec = @import("canonicalized_cache_entry.zig");
const CacheManager = @import("cache_manager.zig").CacheManager;
const Allocator = std.mem.Allocator;
const scope = "source-import-inventory-v2";

/// All slices belong to the allocator supplied to the producer or decoder.
/// Callers use an arena for the inventory's lifetime.
pub const Inventory = struct {
    is_application: bool = false,
    local: []const discovery.LocalImport,
    external: []const []const u8,
    public: []const discovery.PublicModule,
};

/// Hash normalized source bytes in the inventory format domain.
pub fn key(source: []const u8) [32]u8 {
    var hash = base.Sha256.init(.{});
    hash.update(scope);
    hash.update(source);
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

fn encode(allocator: Allocator, inventory: Inventory) Allocator.Error![]const u8 {
    const local = try allocator.alloc(codec.LocalImport, inventory.local.len);
    for (local, inventory.local) |*out, item| out.* = .{
        .import_name = item.import_name,
        .base = item.base,
        .parent_count = item.parent_count,
    };
    const record: codec.ParseStageRecord = .{ .diagnostics = &.{}, .local_imports = local, .external_imports = inventory.external };
    var bytes = std.ArrayList(u8).empty;
    try bytes.resize(allocator, codec.encodedLen(record));
    codec.encode(record, bytes.items);
    try appendCount(allocator, &bytes, @intFromBool(inventory.is_application));
    try appendCount(allocator, &bytes, inventory.public.len);
    for (inventory.public) |item| {
        try appendString(allocator, &bytes, item.name);
        try appendString(allocator, &bytes, item.target);
    }
    return bytes.items;
}

fn appendCount(allocator: Allocator, bytes: *std.ArrayList(u8), count: usize) Allocator.Error!void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, std.math.cast(u32, count) orelse return error.OutOfMemory, .little);
    try bytes.appendSlice(allocator, &buf);
}

fn appendString(allocator: Allocator, bytes: *std.ArrayList(u8), string: []const u8) Allocator.Error!void {
    try appendCount(allocator, bytes, string.len);
    try bytes.appendSlice(allocator, string);
}

fn decode(allocator: Allocator, bytes: []const u8) (Allocator.Error || codec.DecodeError)!Inventory {
    var reader = codec.Reader.init(bytes);
    if (try reader.readCount() != 0) return error.CorruptParseStageRecord;
    var local = std.ArrayList(discovery.LocalImport).empty;
    const local_count = try reader.readCount();
    for (0..local_count) |_| {
        const item = try reader.readLocalImport();
        try local.append(allocator, .{ .import_name = item.import_name, .base = item.base, .parent_count = item.parent_count });
    }
    var external = std.ArrayList([]const u8).empty;
    const external_count = try reader.readCount();
    for (0..external_count) |_| try external.append(allocator, try reader.readExternalImport());
    if (try reader.readPinValidation() != null) return error.CorruptParseStageRecord;
    const app_tag = try reader.readCount();
    if (app_tag > 1) return error.CorruptParseStageRecord;
    var public = std.ArrayList(discovery.PublicModule).empty;
    const public_count = try reader.readCount();
    for (0..public_count) |_| try public.append(allocator, .{
        .name = try reader.readExternalImport(),
        .target = try reader.readExternalImport(),
        .nested_type = null, // Graph traversal needs the source module, not its selected type.
    });
    if (!reader.atEnd()) return error.CorruptParseStageRecord;
    return .{ .is_application = app_tag == 1, .local = local.items, .external = external.items, .public = public.items };
}

/// Load an inventory into the caller arena; stale or corrupt records miss.
pub fn load(allocator: Allocator, manager: *CacheManager, source_key: [32]u8) Allocator.Error!?Inventory {
    const stored = manager.loadFamilyBytes(.canonicalized, source_key, scope) orelse return null;
    defer manager.allocator.free(stored);
    const bytes = try allocator.dupe(u8, stored);
    return decode(allocator, bytes) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.CorruptParseStageRecord => null,
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
pub fn publishAst(allocator: Allocator, manager: *CacheManager, ast: *const parse.AST) Allocator.Error!void {
    if (!manager.config.enabled or ast.tokenize_diagnostics.items.len != 0 or ast.parse_diagnostics.items.len != 0) return;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const inventory = fromAst(arena.allocator(), ast) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ImportEscapesPackageRoot => return, // The compiler reports the invalid import; it cannot be cached.
    };
    try store(allocator, manager, key(ast.env.source), inventory);
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
    try std.testing.expect(!std.mem.eql(u8, &key("import Foo"), &key("import Bar")));
}
