//! Common IR types and utilities that were previously part of ModuleEnv.
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");

const ModuleEnv = @import("ModuleEnv.zig");
const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;

// Re-export these from other modules for convenience
pub const NodeStore = @import("NodeStore.zig");
pub const Node = @import("Node.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

// Type definitions for module compilation

/// Represents a definition (binding of a pattern to an expression) in the CIR
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pattern: Pattern.Idx,
    expr: Expr.Idx,
    annotation: ?Annotation.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A def that introduces identifiers
        let: void,
        /// A standalone statement with an fx variable
        stmt: TypeVar,
        /// Ignored result, must be effectful
        ignored: TypeVar,

        pub fn decode(encoded: [2]u32) Kind {
            if (encoded[0] == 0) {
                return .let;
            } else if (encoded[0] == 1) {
                return .{ .stmt = @as(TypeVar, @enumFromInt(encoded[1])) };
            } else {
                return .{ .ignored = @as(TypeVar, @enumFromInt(encoded[1])) };
            }
        }

        pub fn encode(self: Kind) [2]u32 {
            switch (self) {
                .let => return .{ 0, 0 },
                .stmt => |ty_var| return .{ 1, @intFromEnum(ty_var) },
                .ignored => |ty_var| return .{ 2, @intFromEnum(ty_var) },
            }
        }
    };

    pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        try tree.pushStaticAtom(name);

        const attrs = tree.beginNode();

        try cir.store.getPattern(self.pattern).pushToSExprTree(cir, tree, self.pattern);
        try cir.store.getExpr(self.expr).pushToSExprTree(cir, tree, self.expr);

        if (self.annotation) |annotation_idx| {
            try cir.store.getAnnotation(annotation_idx).pushToSExprTree(cir, tree, annotation_idx);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a type header (e.g., 'Maybe a' or 'Result err ok') in type annotations
pub const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { start: u32, len: u32 };

    name: base.Ident.Idx,
    args: TypeAnno.Span,

    pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, idx: TypeHeader.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ty-header");

        // Get the region for this TypeHeader
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("ty-args");
            const args_attrs = tree.beginNode();
            for (cir.store.sliceTypeAnnos(self.args)) |anno_idx| {
                try cir.store.getTypeAnno(anno_idx).pushToSExprTree(cir, tree, anno_idx);
            }
            try tree.endNode(args_begin, args_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a where clause constraint in type definitions
pub const WhereClause = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    mod_method: struct {
        var_name: base.Ident.Idx,
        method_name: base.Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Idx,
        external_decl: ExternalDecl.Idx,
    },
    mod_alias: struct {
        var_name: base.Ident.Idx,
        alias_name: base.Ident.Idx,
        external_decl: ExternalDecl.Idx,
    },
    malformed: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub fn pushToSExprTree(self: *const WhereClause, cir: anytype, tree: anytype, idx: WhereClause.Idx) !void {
        switch (self.*) {
            .mod_method => |method| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                // Add module-of and ident information
                const var_name_str = cir.getIdent(method.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const method_name_str = cir.getIdent(method.method_name);
                try tree.pushStringPair("ident", method_name_str);

                const attrs = tree.beginNode();

                // Add actual argument types
                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (cir.store.sliceTypeAnnos(method.args)) |arg_idx| {
                    try cir.store.getTypeAnno(arg_idx).pushToSExprTree(cir, tree, arg_idx);
                }
                try tree.endNode(args_begin, args_attrs);

                // Add actual return type
                try cir.store.getTypeAnno(method.ret_anno).pushToSExprTree(cir, tree, method.ret_anno);
                try tree.endNode(begin, attrs);
            },
            .mod_alias => |alias| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                const var_name_str = cir.getIdent(alias.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const alias_name_str = cir.getIdent(alias.alias_name);
                try tree.pushStringPair("ident", alias_name_str);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .malformed => |malformed| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                _ = malformed;
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// Represents a type annotation associated with definitions
pub const Annotation = struct {
    pub const Idx = enum(u32) { _ };

    type_anno: TypeAnno.Idx,
    signature: TypeVar,

    pub fn pushToSExprTree(self: *const Annotation, cir: anytype, tree: anytype, idx: Annotation.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");

        // Get the region for this Annotation
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const attrs = tree.beginNode();

        const type_anno_begin = tree.beginNode();
        try tree.pushStaticAtom("declared-type");
        const type_anno_attrs = tree.beginNode();
        try cir.store.getTypeAnno(self.type_anno).pushToSExprTree(cir, tree, self.type_anno);
        try tree.endNode(type_anno_begin, type_anno_attrs);

        try tree.endNode(begin, attrs);
    }
};

/// Represents an item exposed by a module's interface
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    name: base.Ident.Idx,
    alias: ?base.Ident.Idx,
    is_wildcard: bool,

    pub fn pushToSExprTree(self: *const ExposedItem, _: anytype, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("exposed");

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        if (self.alias) |alias_idx| {
            const alias_str = cir.getIdent(alias_idx);
            try tree.pushStringPair("alias", alias_str);
        }

        try tree.pushBoolPair("wildcard", self.is_wildcard);

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
    }
};

/// Represents a field in a record pattern for pattern matching
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { start: u32, len: u32 };
};

/// Represents an arbitrary precision integer value
pub const IntValue = struct {
    bytes: [16]u8,
    kind: enum {
        i64,
        u64,
        i128,
        u128,
    },

    pub fn toI128(self: IntValue) i128 {
        return @bitCast(self.bytes);
    }
};

// RocDec type definition (for missing export)
// Must match the structure of builtins.RocDec
pub const RocDec = builtins.dec.RocDec;

/// Converts a RocDec to an i128 integer
pub fn toI128(self: RocDec) i128 {
    return self.num;
}

/// Creates a RocDec from an f64 value, returns null if conversion fails
pub fn fromF64(f: f64) ?RocDec {
    // Simple conversion - the real implementation is in builtins/dec.zig
    const scaled = @as(i128, @intFromFloat(f * 1_000_000_000_000_000_000.0));
    return RocDec{ .num = scaled };
}

/// Represents an import statement in a module
pub const Import = struct {
    pub const Idx = enum(u32) { _ };

    pub const Store = struct {
        /// Map from interned string idx to Import.Idx for deduplication
        map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
        /// List of interned string IDs indexed by Import.Idx
        imports: collections.SafeList(base.StringLiteral.Idx) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            self.imports.deinit(allocator);
        }

        /// Get or create an Import.Idx for the given module name.
        /// The module name is first checked against existing imports by comparing strings.
        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, strings: *base.StringLiteral.Store, module_name: []const u8) !Import.Idx {
            // First check if we already have this module name
            for (self.imports.items.items, 0..) |existing_string_idx, i| {
                const existing_name = strings.get(existing_string_idx);
                if (std.mem.eql(u8, existing_name, module_name)) {
                    // Found existing import with same name
                    return @as(Import.Idx, @enumFromInt(i));
                }
            }

            // Not found - create new import
            const string_idx = try strings.insert(allocator, module_name);
            const idx = @as(Import.Idx, @enumFromInt(self.imports.len()));

            // Add to both the list and the map
            _ = try self.imports.append(allocator, string_idx);
            try self.map.put(allocator, string_idx, idx);

            return idx;
        }

        /// Serialize this Store to the given CompactWriter. The resulting Store
        /// in the writer's buffer will have offsets instead of pointers. Calling any
        /// methods on it or dereferencing its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!*const Store {
            // First, write the Store struct itself
            const offset_self = try writer.appendAlloc(allocator, Store);

            // Then serialize the sub-structures and update the struct
            offset_self.* = .{
                .map = .{}, // Map will be empty after deserialization (only used for deduplication during insertion)
                .imports = (try self.imports.serialize(allocator, writer)).*,
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *Store, offset: isize) void {
            self.imports.relocate(offset);
        }

        pub const Serialized = struct {
            // Placeholder to match Store size - not serialized
            map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
            imports: collections.SafeList(base.StringLiteral.Idx).Serialized,

            /// Serialize a Store into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                store: *const Store,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                // Serialize the imports SafeList
                try self.imports.serialize(&store.imports, allocator, writer);
                // Note: The map is not serialized as it's only used for deduplication during insertion
            }

            /// Deserialize this Serialized struct into a Store
            pub fn deserialize(self: *Serialized, offset: i64, allocator: std.mem.Allocator) *Store {
                // Overwrite ourself with the deserialized version, and return our pointer after casting it to Store.
                const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

                store.* = .{
                    .map = .{}, // Will be repopulated below
                    .imports = self.imports.deserialize(offset).*,
                };

                // Pre-allocate the exact capacity needed for the map
                const import_count = store.imports.items.items.len;
                store.map.ensureTotalCapacity(allocator, @intCast(import_count)) catch unreachable;

                // Repopulate the map - we know there's enough capacity since we
                // are deserializing from a Serialized struct
                for (store.imports.items.items, 0..) |string_idx, i| {
                    const import_idx = @as(Import.Idx, @enumFromInt(i));
                    store.map.putAssumeCapacityNoClobber(string_idx, import_idx);
                }

                return store;
            }
        };
    };
};

/// Represents a field in a record expression
pub const RecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    name: base.Ident.Idx,
    value: Expr.Idx,

    pub fn pushToSExprTree(self: *const RecordField, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("field");
        try tree.pushStringPair("name", cir.getIdent(self.name));
        const attrs = tree.beginNode();
        try cir.store.getExpr(self.value).pushToSExprTree(cir, tree, self.value);
        try tree.endNode(begin, attrs);
    }
};

/// Represents an external declaration from another module
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: base.Ident.Idx,
    /// Module this decl comes from (e.g., "json.Json")
    module_name: base.Ident.Idx,
    /// Local name within that module (e.g., "utf8")
    local_name: base.Ident.Idx,
    /// Type variable for this declaration
    type_var: TypeVar,
    /// Kind of external declaration
    kind: enum { value, type },
    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const ExternalDecl, cir: anytype, tree: anytype, region: Region) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
};

// Real Report type from the reporting module
pub const Report = reporting.Report;

/// Checks if a type is castable for index type conversions
pub fn isCastable(comptime T: type) bool {
    return switch (T) {
        Expr.Idx,
        Pattern.Idx,
        Statement.Idx,
        TypeAnno.Idx,
        Def.Idx,
        TypeHeader.Idx,
        RecordField.Idx,
        Pattern.RecordDestruct.Idx,
        Expr.IfBranch.Idx,
        Expr.Match.Branch.Idx,
        WhereClause.Idx,
        Annotation.Idx,
        TypeAnno.RecordField.Idx,
        ExposedItem.Idx,
        Expr.Match.BranchPattern.Idx,
        PatternRecordField.Idx,
        Node.Idx,
        TypeVar,
        => true,
        else => false,
    };
}

/// Safely casts between compatible index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}

test "Import.Store empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Create an empty Store
    var original = Import.Store.init();
    // No deinit needed, arena will handle it.

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Import.Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), arena_allocator);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.imports.len());
    try testing.expectEqual(@as(usize, 0), deserialized.map.count());
}

test "Import.Store basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Create a mock module env with string store
    var string_store = try base.StringLiteral.Store.initCapacityBytes(arena_allocator, 1024);

    const MockEnv = struct { strings: *base.StringLiteral.Store };
    const mock_env = MockEnv{ .strings = &string_store };

    // Create original store and add some imports
    var original = Import.Store.init();

    const idx1 = try original.getOrPut(arena_allocator, mock_env.strings, "json.Json");
    const idx2 = try original.getOrPut(arena_allocator, mock_env.strings, "core.List");
    const idx3 = try original.getOrPut(arena_allocator, mock_env.strings, "my.Module");

    // Verify indices
    try testing.expectEqual(@as(u32, 0), @intFromEnum(idx1));
    try testing.expectEqual(@as(u32, 1), @intFromEnum(idx2));
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx3));

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Import.Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), arena_allocator);

    // Verify the imports are accessible
    try testing.expectEqual(@as(usize, 3), deserialized.imports.len());

    // Verify the interned string IDs are stored correctly
    const str_idx1 = deserialized.imports.items.items[0];
    const str_idx2 = deserialized.imports.items.items[1];
    const str_idx3 = deserialized.imports.items.items[2];

    try testing.expectEqualStrings("json.Json", string_store.get(str_idx1));
    try testing.expectEqualStrings("core.List", string_store.get(str_idx2));
    try testing.expectEqualStrings("my.Module", string_store.get(str_idx3));

    // Verify the map is repopulated correctly
    try testing.expectEqual(@as(usize, 3), deserialized.map.count());
}

test "Import.Store duplicate imports CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Create a mock module env with string store
    var string_store = try base.StringLiteral.Store.initCapacityBytes(arena_allocator, 1024);

    const MockEnv = struct { strings: *base.StringLiteral.Store };
    const mock_env = MockEnv{ .strings = &string_store };

    // Create store with duplicate imports
    var original = Import.Store.init();

    const idx1 = try original.getOrPut(arena_allocator, mock_env.strings, "test.Module");
    const idx2 = try original.getOrPut(arena_allocator, mock_env.strings, "another.Module");
    const idx3 = try original.getOrPut(arena_allocator, mock_env.strings, "test.Module"); // duplicate

    // Verify deduplication worked
    try testing.expectEqual(idx1, idx3);
    try testing.expectEqual(@as(usize, 2), original.imports.len());

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_duplicate_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = collections.CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Import.Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), arena_allocator);

    // Verify correct number of imports
    try testing.expectEqual(@as(usize, 2), deserialized.imports.len());

    // Get the string IDs and verify the strings
    const str_idx1 = deserialized.imports.items.items[@intFromEnum(idx1)];
    const str_idx2 = deserialized.imports.items.items[@intFromEnum(idx2)];

    try testing.expectEqualStrings("test.Module", string_store.get(str_idx1));
    try testing.expectEqualStrings("another.Module", string_store.get(str_idx2));

    // Verify the map was repopulated correctly
    try testing.expectEqual(@as(usize, 2), deserialized.map.count());

    // Check that the map has correct entries for the string indices that were deserialized
    const str_idx_0 = deserialized.imports.items.items[0];
    const str_idx_1 = deserialized.imports.items.items[1];

    try testing.expect(deserialized.map.contains(str_idx_0));
    try testing.expect(deserialized.map.contains(str_idx_1));
    try testing.expectEqual(@as(Import.Idx, @enumFromInt(0)), deserialized.map.get(str_idx_0).?);
    try testing.expectEqual(@as(Import.Idx, @enumFromInt(1)), deserialized.map.get(str_idx_1).?);
}
