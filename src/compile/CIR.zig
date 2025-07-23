//! Common IR types and utilities that were previously part of ModuleEnv.
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("../reporting.zig");

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

// Def type definition
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

// TypeHeader type definition
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

// WhereClause type definition
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

// Annotation type definition
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

// ExposedItem type definition
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

// PatternRecordField type definition with proper Idx and Span types
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { start: u32, len: u32 };
};

// IntValue type definition (for missing export)
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
pub const RocDec = extern struct {
    num: i128,
    
    pub const decimal_places: u5 = 18;
    pub const whole_number_places: u5 = 21;
    
    pub fn toI128(self: RocDec) i128 {
        return self.num;
    }
    
    pub fn fromF64(f: f64) ?RocDec {
        // Simple conversion - the real implementation is in builtins/dec.zig
        const scaled = @as(i128, @intFromFloat(f * 1_000_000_000_000_000_000.0));
        return RocDec{ .num = scaled };
    }
    
    pub fn toF64(self: RocDec) f64 {
        // Simple conversion - the real implementation is in builtins/dec.zig
        return @as(f64, @floatFromInt(self.num)) / 1_000_000_000_000_000_000.0;
    }
    
    pub fn fromU64(n: u64) RocDec {
        // Simple conversion - the real implementation is in builtins/dec.zig
        return RocDec{ .num = @as(i128, n) * 1_000_000_000_000_000_000 };
    }
};

// Diagnostic types for compilation errors
pub const Diagnostic = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
    
    not_implemented: struct { feature: StringLiteral.Idx, region: Region },
    exposed_but_not_implemented: struct { ident: Ident.Idx, region: Region },
    redundant_exposed: struct { ident: Ident.Idx, region: Region, original_region: Region },
    invalid_num_literal: struct { region: Region },
    ident_already_in_scope: struct { ident: Ident.Idx, region: Region },
    ident_not_in_scope: struct { ident: Ident.Idx, region: Region },
    invalid_top_level_statement: struct { stmt: StringLiteral.Idx, region: Region },
    f64_pattern_literal: struct { region: Region },
    invalid_single_quote: struct { region: Region },
    crash_expects_string: struct { region: Region },
    empty_tuple: struct { region: Region },
    expr_not_canonicalized: struct { region: Region },
    invalid_string_interpolation: struct { region: Region },
    pattern_arg_invalid: struct { region: Region },
    pattern_not_canonicalized: struct { region: Region },
    can_lambda_not_implemented: struct { region: Region },
    lambda_body_not_canonicalized: struct { region: Region },
    if_condition_not_canonicalized: struct { region: Region },
    if_then_not_canonicalized: struct { region: Region },
    if_else_not_canonicalized: struct { region: Region },
    var_across_function_boundary: struct { region: Region },
    malformed_type_annotation: struct { region: Region },
    malformed_where_clause: struct { region: Region },
    shadowing_warning: struct { ident: Ident.Idx, region: Region, original_region: Region },
    type_redeclared: struct { name: Ident.Idx, original_region: Region, redeclared_region: Region },
    tuple_elem_not_canonicalized: struct { region: Region },
    module_not_found: struct { module_name: Ident.Idx, region: Region },
    value_not_exposed: struct { module_name: Ident.Idx, value_name: Ident.Idx, region: Region },
    type_not_exposed: struct { module_name: Ident.Idx, type_name: Ident.Idx, region: Region },
    module_not_imported: struct { module_name: Ident.Idx, region: Region },
    too_many_exports: struct { count: u32, region: Region },
    undeclared_type: struct { name: Ident.Idx, region: Region },
    undeclared_type_var: struct { name: Ident.Idx, region: Region },
    type_alias_redeclared: struct { name: Ident.Idx, original_region: Region, redeclared_region: Region },
    nominal_type_redeclared: struct { name: Ident.Idx, original_region: Region, redeclared_region: Region },
    type_shadowed_warning: struct { name: Ident.Idx, region: Region, original_region: Region, cross_scope: bool },
    type_parameter_conflict: struct { name: Ident.Idx, parameter_name: Ident.Idx, region: Region, original_region: Region },
    unused_variable: struct { region: Region, ident: Ident.Idx },
    used_underscore_variable: struct { region: Region, ident: Ident.Idx },
    duplicate_record_field: struct { field_name: Ident.Idx, duplicate_region: Region, original_region: Region },
    unused_type_var_name: struct { name: Ident.Idx, suggested_name: Ident.Idx, region: Region },
    type_var_marked_unused: struct { name: Ident.Idx, suggested_name: Ident.Idx, region: Region },
    type_var_ending_in_underscore: struct { name: Ident.Idx, suggested_name: Ident.Idx, region: Region },
    underscore_in_type_declaration: struct { is_alias: bool, region: Region },
    
    pub fn buildInvalidNumLiteralReport(
        allocator: std.mem.Allocator,
        region_info: RegionInfo,
        source: []const u8,
        filename: []const u8,
        _: []const u8,
        line_starts: []const u32,
    ) !Report {
        var report = Report.init(allocator, "Invalid number literal", .runtime_error);
        try report.addHeader("Invalid Number Literal");
        
        try report.document.addText("The number literal is invalid or too large to represent:");
        try report.document.addLineBreak();
        
        // Add source context with location
        const owned_filename = try report.addOwnedString(filename);
        try report.addSourceContext(region_info, owned_filename, source, line_starts);
        
        return report;
    }
};

// Import type definition
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

// RecordField type definition (for expression records)
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

// ExternalDecl type definition
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

// Helper function to check if a type is castable
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

// Helper function to cast between index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}