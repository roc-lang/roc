//! Common IR types and utilities that were previously part of ModuleEnv.
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");

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

        const name_str = cir.idents.getText(self.name);
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
                const var_name_str = cir.idents.getText(method.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const method_name_str = cir.idents.getText(method.method_name);
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

                const var_name_str = cir.idents.getText(alias.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const alias_name_str = cir.idents.getText(alias.alias_name);
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

        const name_str = cir.idents.getText(self.name);
        try tree.pushStringPair("name", name_str);

        if (self.alias) |alias_idx| {
            const alias_str = cir.idents.getText(alias_idx);
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
pub const RocDec = builtins.RocDec;

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
pub const Diagnostic = @import("compile").Diagnostic;

/// Represents an import statement in a module
pub const Import = struct {
    pub const Idx = enum(u32) { _ };

    pub const Store = struct {
        /// Map from module name string to Import.Idx
        map: std.StringHashMapUnmanaged(Import.Idx) = .{},
        /// List of imports indexed by Import.Idx
        imports: std.ArrayListUnmanaged([]u8) = .{},
        /// Storage for module name strings
        strings: std.ArrayListUnmanaged(u8) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            for (self.imports.items) |import| {
                allocator.free(import);
            }
            self.imports.deinit(allocator);
            self.strings.deinit(allocator);
        }

        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, module_name: []const u8) !Import.Idx {
            const result = try self.map.getOrPut(allocator, module_name);
            if (!result.found_existing) {
                const idx = @as(Import.Idx, @enumFromInt(self.imports.items.len));
                result.value_ptr.* = idx;
                const owned_name = try allocator.dupe(u8, module_name);
                try self.imports.append(allocator, owned_name);
            }
            return result.value_ptr.*;
        }
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
        try tree.pushStringPair("name", cir.idents.getText(self.name));
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
        try tree.pushStringPair("ident", cir.idents.getText(self.qualified_name));

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
        try tree.pushStringPair("ident", cir.idents.getText(self.qualified_name));

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
