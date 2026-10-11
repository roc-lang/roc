//! Portable producer-owned representation evidence for cached literal roots.
//! ConstStore's type and name authority survives independently of erased bodies.

const std = @import("std");
const check = @import("check");
const ConstStore = check.ConstStore;
const Names = check.CheckedNames;
const Allocator = std.mem.Allocator;

pub const Representation = struct {
    root: ConstStore.ConstTypeId,
    types: []const ConstStore.ConstType = &.{},
    type_pool: []const ConstStore.ConstTypeId = &.{},
    field_pool: []const ConstStore.TypeField = &.{},
    tag_pool: []const ConstStore.TypeTag = &.{},
    declared_field_pool: []const ConstStore.TypeDeclaredField = &.{},
    modules: []const [32]u8 = &.{},
    type_names: []const []const u8 = &.{},
    field_names: []const []const u8 = &.{},
    tag_names: []const []const u8 = &.{},

    /// Clone only the declared closure through ConstStore's translation rules.
    pub fn capture(allocator: Allocator, source: *const ConstStore.ConstTypeStore, source_names: *const Names.NameStore, root: ConstStore.ConstTypeId) Allocator.Error!Representation {
        var store = ConstStore.ConstTypeStore.init(allocator);
        defer store.deinit();
        var names = Names.NameStore.init(allocator);
        defer names.deinit();
        var result: Representation = .{ .root = try store.cloneTypeFromTranslated(source, source_names, &names, root) };
        errdefer result.deinit(allocator);
        result.types = try store.types.toOwnedSlice(allocator);
        result.type_pool = try store.type_pool.toOwnedSlice(allocator);
        result.field_pool = try store.field_pool.toOwnedSlice(allocator);
        result.tag_pool = try store.tag_pool.toOwnedSlice(allocator);
        result.declared_field_pool = try store.declared_field_pool.toOwnedSlice(allocator);
        const modules = try allocator.alloc([32]u8, names.module_identities.count());
        result.modules = modules;
        for (modules, 0..) |*module, index| module.* = names.moduleIdentityBytes(@enumFromInt(index)).*;
        result.type_names = try captureNames(allocator, &names, .type_names);
        result.field_names = try captureNames(allocator, &names, .record_field_labels);
        result.tag_names = try captureNames(allocator, &names, .tag_labels);
        return result;
    }

    const NameKind = enum { type_names, record_field_labels, tag_labels };
    fn captureNames(allocator: Allocator, names: *const Names.NameStore, comptime kind: NameKind) Allocator.Error![]const []const u8 {
        const result = try allocator.alloc([]const u8, @field(names, @tagName(kind)).count());
        var initialized: usize = 0;
        errdefer {
            for (result[0..initialized]) |text| allocator.free(text);
            allocator.free(result);
        }
        for (result, 0..) |*text, index| {
            text.* = try allocator.dupe(u8, switch (kind) {
                .type_names => names.typeNameText(@enumFromInt(index)),
                .record_field_labels => names.recordFieldLabelText(@enumFromInt(index)),
                .tag_labels => names.tagLabelText(@enumFromInt(index)),
            });
            initialized += 1;
        }
        return result;
    }

    pub fn deinit(self: Representation, allocator: Allocator) void {
        allocator.free(self.types);
        allocator.free(self.type_pool);
        allocator.free(self.field_pool);
        allocator.free(self.tag_pool);
        allocator.free(self.declared_field_pool);
        allocator.free(self.modules);
        inline for (.{ self.type_names, self.field_names, self.tag_names }) |texts| {
            for (texts) |text| allocator.free(text);
            allocator.free(texts);
        }
    }

    pub const Materialized = struct {
        names: Names.NameStore,
        store: ConstStore.ConstStore,
        pub fn deinit(self: *Materialized) void {
            self.store.deinit();
            self.names.deinit();
        }
    };

    /// Restore declared types, never reverse a cached physical layout.
    pub fn materialize(self: Representation, allocator: Allocator) (Allocator.Error || error{InvalidLiteralRecipe})!Materialized {
        if (!self.valid()) return error.InvalidLiteralRecipe;
        var result: Materialized = .{ .names = Names.NameStore.init(allocator), .store = ConstStore.ConstStore.init(allocator) };
        errdefer result.deinit();
        for (self.modules, 0..) |module, index| {
            if (@intFromEnum(try result.names.internModuleIdentity(&module)) != index) return error.InvalidLiteralRecipe;
        }
        inline for (.{ "type_names", "field_names", "tag_names" }) |field| {
            for (@field(self, field), 0..) |text, index| {
                const id = if (comptime std.mem.eql(u8, field, "type_names"))
                    @intFromEnum(try result.names.internTypeName(text))
                else if (comptime std.mem.eql(u8, field, "field_names"))
                    @intFromEnum(try result.names.internRecordFieldLabel(text))
                else
                    @intFromEnum(try result.names.internTagLabel(text));
                if (id != index) return error.InvalidLiteralRecipe;
            }
        }
        try result.store.type_store.types.appendSlice(allocator, self.types);
        try result.store.type_store.type_pool.appendSlice(allocator, self.type_pool);
        try result.store.type_store.field_pool.appendSlice(allocator, self.field_pool);
        try result.store.type_store.tag_pool.appendSlice(allocator, self.tag_pool);
        try result.store.type_store.declared_field_pool.appendSlice(allocator, self.declared_field_pool);
        return result;
    }

    fn hasType(self: Representation, id: ConstStore.ConstTypeId) bool {
        return @intFromEnum(id) < self.types.len;
    }
    fn hasRange(range: ConstStore.ConstRange, len: usize) bool {
        return range.start <= len and range.len <= len - range.start;
    }
    /// All references are checked before entering ConstStore accessors.
    /// Recursive type edges are legitimate and need no recursive traversal.
    pub fn valid(self: Representation) bool {
        if (!self.hasType(self.root)) return false;
        for (self.modules, 0..) |module, index| {
            for (self.modules[0..index]) |earlier| if (std.mem.eql(u8, &module, &earlier)) return false;
        }
        inline for (.{ self.type_names, self.field_names, self.tag_names }) |texts| {
            for (texts, 0..) |text, index| {
                for (texts[0..index]) |earlier| if (std.mem.eql(u8, text, earlier)) return false;
            }
        }
        for (self.type_pool) |ty| if (!self.hasType(ty)) return false;
        for (self.field_pool) |field| {
            if (@intFromEnum(field.name) >= self.field_names.len or !self.hasType(field.ty)) return false;
            if (field.value_ty) |ty| if (!self.hasType(ty)) return false;
            if (field.default) |value| if (@intFromEnum(value.module) >= self.modules.len) return false;
        }
        for (self.tag_pool) |tag| {
            if (@intFromEnum(tag.name) >= self.tag_names.len or @intFromEnum(tag.checked_name) >= self.tag_names.len) return false;
            if (!hasRange(tag.payloads, self.type_pool.len)) return false;
        }
        for (self.declared_field_pool) |field| switch (field) {
            .named => |name| if (@intFromEnum(name) >= self.field_names.len) return false,
            .padding => |ty| if (!self.hasType(ty)) return false,
        };
        for (self.types) |ty| switch (ty) {
            .primitive, .erased, .zst => {},
            .list, .box => |child| if (!self.hasType(child)) return false,
            .tuple => |range| if (!hasRange(range, self.type_pool.len)) return false,
            .record => |range| if (!hasRange(range, self.field_pool.len)) return false,
            .tag_union => |range| if (!hasRange(range, self.tag_pool.len)) return false,
            .func => |func| {
                if (!hasRange(func.args, self.type_pool.len) or !self.hasType(func.ret)) return false;
            },
            .named => |named| {
                if (@intFromEnum(named.def.module) >= self.modules.len or @intFromEnum(named.def.type_name) >= self.type_names.len) return false;
                if (!hasRange(named.args, self.type_pool.len) or !hasRange(named.declared_order, self.declared_field_pool.len)) return false;
                if (named.backing) |backing| if (!self.hasType(backing.ty)) return false;
                if (named.kind == .alias) {
                    var current = named;
                    var remaining = self.types.len;
                    while (current.kind == .alias) {
                        if (remaining == 0) return false;
                        remaining -= 1;
                        const backing = current.backing orelse return false;
                        if (!self.hasType(backing.ty)) return false;
                        const next = self.types[@intFromEnum(backing.ty)];
                        if (next != .named) break;
                        current = next.named;
                    }
                }
                if (named.def.iterator_topology) |topology| {
                    if (@intFromEnum(topology.len_field) >= self.field_names.len or @intFromEnum(topology.step_field) >= self.field_names.len) return false;
                    if (@intFromEnum(topology.item_field) >= self.field_names.len or @intFromEnum(topology.rest_field) >= self.field_names.len) return false;
                    inline for (.{ "known_tag", "unknown_tag", "done_tag", "one_tag", "skip_tag" }) |field| {
                        if (@intFromEnum(@field(topology, field)) >= self.tag_names.len) return false;
                    }
                }
            },
        };
        return true;
    }
};

test "literal producer representation restores exact name and type closure" {
    const allocator = std.testing.allocator;
    var names = Names.NameStore.init(allocator);
    defer names.deinit();
    _ = try names.internRecordFieldLabel("unrelated");
    const name = try names.internRecordFieldLabel("value");
    var store = ConstStore.ConstTypeStore.init(allocator);
    defer store.deinit();
    _ = try store.append(.{ .primitive = .u8 });
    const number = try store.append(.{ .primitive = .u64 });
    const record = try store.append(.{ .record = try store.appendFieldSpan(&.{.{ .name = name, .ty = number, .default = null }}) });
    const recipe = try Representation.capture(allocator, &store, &names, record);
    defer recipe.deinit(allocator);
    try std.testing.expect(recipe.valid());
    try std.testing.expectEqual(@as(usize, 2), recipe.types.len);
    try std.testing.expectEqual(@as(usize, 1), recipe.field_names.len);
    var restored = try recipe.materialize(allocator);
    defer restored.deinit();
    const field = restored.store.type_store.fieldSpan(restored.store.type_store.get(recipe.root).record)[0];
    try std.testing.expectEqualStrings("value", restored.names.recordFieldLabelText(field.name));
    try std.testing.expectEqualDeep(ConstStore.ConstType{ .primitive = .u64 }, restored.store.type_store.get(field.ty));
}

test "literal producer representation rejects malformed references" {
    const types = [_]ConstStore.ConstType{.{ .list = @enumFromInt(1) }};
    const recipe: Representation = .{ .root = @enumFromInt(0), .types = &types };
    try std.testing.expect(!recipe.valid());
    try std.testing.expectError(error.InvalidLiteralRecipe, recipe.materialize(std.testing.allocator));
}
