//! Common IR types and utilities that were previously part of ModuleEnv.
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");
const serialization = @import("serialization");

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
        diagnostic: ModuleEnv.Diagnostic.Idx,
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

// Diagnostic types for compilation errors
pub const Diagnostic = @import("Diagnostic.zig");

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
        /// The module name is first interned in the provided string store, then used for deduplication.
        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, strings: *base.StringLiteral.Store, module_name: []const u8) !Import.Idx {
            // First intern the string
            const string_idx = try strings.insert(allocator, module_name);

            // Then check if we already have this interned string in our map
            const result = try self.map.getOrPut(allocator, string_idx);
            if (!result.found_existing) {
                // New import - store it
                const idx = @as(Import.Idx, @enumFromInt(self.imports.len()));
                result.value_ptr.* = idx;
                _ = try self.imports.append(allocator, string_idx);
            }
            return result.value_ptr.*;
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
            // Note: self.map is a hash map that we don't serialize/deserialize
            // It's only used for deduplication during insertion
            self.imports.relocate(offset);
        }

        pub const Serialized = struct {
            imports: collections.SafeList(base.StringLiteral.Idx).Serialized,
            // Placeholder to match Store size - not serialized
            map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},

            /// Serialize a Store into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                store: *const Store,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                // Serialize the imports SafeList
                try self.imports.serialize(&store.imports, allocator, writer);
            }

            /// Deserialize this Serialized struct into a Store
            pub fn deserialize(self: *Serialized, offset: i64) *Store {
                // Overwrite ourself with the deserialized version, and return our pointer after casting it to Store.
                const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));
                store.map = .{}; // Map will be empty after deserialization (only used for deduplication during insertion)
                store.imports = self.imports.deserialize(offset).*;
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

    // Create an empty Store
    var original = Import.Store.init();
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_import_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Import.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Import.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.imports.len());
    try testing.expectEqual(@as(usize, 0), deserialized.map.count());
}

// TODO FIXME
// test "Import.Store basic CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     // Create a mock module env with string store
//     var string_store = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
//     defer string_store.deinit(gpa);

//     const MockEnv = struct {
//         strings: *base.StringLiteral.Store,
//     };
//     var mock_env = MockEnv{ .strings = &string_store };

//     // Create original store and add some imports
//     var original = Import.Store.init();
//     defer original.deinit(gpa);

//     const idx1 = try original.getOrPut(gpa, &mock_env, "json.Json");
//     const idx2 = try original.getOrPut(gpa, &mock_env, "core.List");
//     const idx3 = try original.getOrPut(gpa, &mock_env, "my.Module");

//     // Verify indices
//     try testing.expectEqual(@as(u32, 0), @intFromEnum(idx1));
//     try testing.expectEqual(@as(u32, 1), @intFromEnum(idx2));
//     try testing.expectEqual(@as(u32, 2), @intFromEnum(idx3));

//     // Create a temp file
//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_basic_import_store.dat", .{ .read = true });
//     defer file.close();

//     // Serialize using CompactWriter
//     var writer = collections.CompactWriter{
//         .iovecs = .{},
//         .total_bytes = 0,
//         .allocated_memory = .{},
//     };
//     defer writer.deinit(gpa);

//     _ = try original.serialize(gpa, &writer);

//     // Write to file
//     try writer.writeGather(gpa, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, 16, file_size);
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Cast and relocate
//     const deserialized = @as(*Import.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Import.Store))));
//     deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

//     // Verify the imports are accessible
//     try testing.expectEqual(@as(usize, 3), deserialized.imports.len());

//     // Verify the interned string IDs are stored correctly
//     const str_idx1 = deserialized.imports.items.items[0];
//     const str_idx2 = deserialized.imports.items.items[1];
//     const str_idx3 = deserialized.imports.items.items[2];

//     try testing.expectEqualStrings("json.Json", string_store.get(str_idx1));
//     try testing.expectEqualStrings("core.List", string_store.get(str_idx2));
//     try testing.expectEqualStrings("my.Module", string_store.get(str_idx3));

//     // Verify the map is empty after deserialization
//     try testing.expectEqual(@as(usize, 0), deserialized.map.count());
// }

// TODO FIXME
// test "Import.Store duplicate imports CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     // Create a mock module env with string store
//     var string_store = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
//     defer string_store.deinit(gpa);

//     const MockEnv = struct {
//         strings: *base.StringLiteral.Store,
//     };
//     var mock_env = MockEnv{ .strings = &string_store };

//     // Create store with duplicate imports
//     var original = Import.Store.init();
//     defer original.deinit(gpa);

//     const idx1 = try original.getOrPut(gpa, &mock_env, "test.Module");
//     const idx2 = try original.getOrPut(gpa, &mock_env, "another.Module");
//     const idx3 = try original.getOrPut(gpa, &mock_env, "test.Module"); // duplicate

//     // Verify deduplication worked
//     try testing.expectEqual(idx1, idx3);
//     try testing.expectEqual(@as(usize, 2), original.imports.len());

//     // Create a temp file
//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_duplicate_import_store.dat", .{ .read = true });
//     defer file.close();

//     // Serialize
//     var writer = collections.CompactWriter{
//         .iovecs = .{},
//         .total_bytes = 0,
//         .allocated_memory = .{},
//     };
//     defer writer.deinit(gpa);

//     _ = try original.serialize(gpa, &writer);

//     // Write to file
//     try writer.writeGather(gpa, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, 16, file_size);
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Cast and relocate
//     const deserialized = @as(*Import.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Import.Store))));
//     deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

//     // Verify correct number of imports
//     try testing.expectEqual(@as(usize, 2), deserialized.imports.len());

//     // Get the string IDs and verify the strings
//     const str_idx1 = deserialized.imports.items.items[@intFromEnum(idx1)];
//     const str_idx2 = deserialized.imports.items.items[@intFromEnum(idx2)];

//     try testing.expectEqualStrings("test.Module", string_store.get(str_idx1));
//     try testing.expectEqualStrings("another.Module", string_store.get(str_idx2));
// }

// TODO FIXME
// test "Import.Store uses interned string deduplication" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     // Create two separate string stores that will intern the same string differently
//     var string_store1 = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
//     defer string_store1.deinit(gpa);

//     var string_store2 = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
//     defer string_store2.deinit(gpa);

//     // Intern some strings in the first store
//     _ = try string_store1.insert(gpa, "foo");
//     _ = try string_store1.insert(gpa, "bar");

//     // Create import store
//     var store = Import.Store.init();
//     defer store.deinit(gpa);

//     // Add the same module name multiple times - should deduplicate based on interned string
//     const idx1 = try store.getOrPut(gpa, &string_store1, "test.Module");
//     const idx2 = try store.getOrPut(gpa, &string_store1, "test.Module");

//     // Should get the same index
//     try testing.expectEqual(idx1, idx2);
//     try testing.expectEqual(@as(usize, 1), store.imports.len());

//     // Now if we use a different string store, we might get a different interned string index
//     // but the Import.Store should still deduplicate based on the string content
//     const idx3 = try store.getOrPut(gpa, &string_store2, "test.Module");

//     // Should still get the same import index because the string content is the same
//     try testing.expectEqual(idx1, idx3);
//     try testing.expectEqual(@as(usize, 1), store.imports.len());

//     // But if we add a different module, it should create a new entry
//     const idx4 = try store.getOrPut(gpa, &string_store1, "other.Module");
//     try testing.expect(idx4 != idx1);
//     try testing.expectEqual(@as(usize, 2), store.imports.len());
// }

// TODO FIXME
// test "Import.Store.Serialized roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     // Create original store and add some imports
//     var original = Import.Store.init();
//     defer original.deinit(gpa);

//     // Create a string store for interning module names
//     var string_store = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
//     defer string_store.deinit(gpa);

//     // Add some imports
//     const idx1 = try original.getOrPut(gpa, &string_store, "Std.List");
//     const idx2 = try original.getOrPut(gpa, &string_store, "Std.Dict");
//     const idx3 = try original.getOrPut(gpa, &string_store, "App.Model");

//     // Create a CompactWriter and arena
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_alloc = arena.allocator();

//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();
//     const tmp_file = try tmp_dir.dir.createFile("test_import_store_serialized.dat", .{ .read = true });
//     defer tmp_file.close();

//     var writer = collections.CompactWriter{
//         .iovecs = .{},
//         .total_bytes = 0,
//         .allocated_memory = .{},
//     };
//     defer writer.deinit(arena_alloc);

//     // Allocate and serialize using the Serialized struct
//     const serialized_ptr = try writer.appendAlloc(arena_alloc, Import.Store.Serialized);
//     try serialized_ptr.serialize(&original, arena_alloc, &writer);

//     // Write to file
//     try writer.writeGather(arena_alloc, tmp_file);

//     // Read back
//     const file_size = try tmp_file.getEndPos();
//     const buffer = try gpa.alloc(u8, file_size);
//     defer gpa.free(buffer);
//     _ = try tmp_file.pread(buffer, 0);

//     // Deserialize
//     const deserialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
//     const store = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

//     // Verify the imports are accessible
//     try testing.expectEqual(@as(usize, 3), store.imports.len());

//     // The map should be empty after deserialization
//     try testing.expectEqual(@as(usize, 0), store.map.count());

//     // Verify the import indices match
//     try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx1))), string_store.get(store.imports.getAssume(@intFromEnum(idx1))));
//     try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx2))), string_store.get(store.imports.getAssume(@intFromEnum(idx2))));
//     try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx3))), string_store.get(store.imports.getAssume(@intFromEnum(idx3))));
// }
