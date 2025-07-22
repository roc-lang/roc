//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const cir_types = @import("cir_types.zig");
const reporting = @import("reporting");
// const compile = @import("../compile.zig"); // Not needed for now

// Type definitions moved out of CIR_Types struct

// Def type definition
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
    
    pattern: Pattern.Idx,
    expr: Expr.Idx,
    annotation: ?Annotation.Idx,
    kind: Kind,
    
    pub const Kind = union(enum) {
        let: void,
        
        pub fn decode(encoded: [2]u32) Kind {
            _ = encoded;
            return Kind{ .let = {} };
        }
        
        pub fn encode(self: Kind) [2]u32 {
            _ = self;
            return [2]u32{ 0, 0 };
        }
    };
    
    pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("def");
        
        const attrs = tree.beginNode();
        
        const pattern_begin = tree.beginNode();
        try tree.pushStaticAtom("pattern");
        const pattern_attrs = tree.beginNode();
        try cir.store.getPattern(self.pattern).pushToSExprTree(cir, tree, self.pattern);
        try tree.endNode(pattern_begin, pattern_attrs);
        
        const expr_begin = tree.beginNode();
        try tree.pushStaticAtom("expr");
        const expr_attrs = tree.beginNode();
        try cir.store.getExpr(self.expr).pushToSExprTree(cir, tree, self.expr);
        try tree.endNode(expr_begin, expr_attrs);
        
        if (self.annotation) |annotation_idx| {
            const annotation_begin = tree.beginNode();
            try tree.pushStaticAtom("annotation");
            const annotation_attrs = tree.beginNode();
            try cir.store.getAnnotation(annotation_idx).pushToSExprTree(cir, tree, annotation_idx);
            try tree.endNode(annotation_begin, annotation_attrs);
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
    
    pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, _: TypeHeader.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("type-header");
        
        const name_str = cir.idents.getText(self.name);
        try tree.pushStringPair("name", name_str);
        
        const attrs = tree.beginNode();
        
        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("args");
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
    
    pub fn pushToSExprTree(self: *const WhereClause, cir: anytype, tree: anytype, _: WhereClause.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("where-clause");
        
        const attrs = tree.beginNode();
        
        switch (self.*) {
            .mod_method => |method| {
                try tree.pushStringPair("type", "mod-method");
                const var_name_str = cir.idents.getText(method.var_name);
                try tree.pushStringPair("var-name", var_name_str);
                
                const method_name_str = cir.idents.getText(method.method_name);
                try tree.pushStringPair("method-name", method_name_str);
            },
            .mod_alias => |alias| {
                try tree.pushStringPair("type", "mod-alias");
                const var_name_str = cir.idents.getText(alias.var_name);
                try tree.pushStringPair("var-name", var_name_str);
                
                const alias_name_str = cir.idents.getText(alias.alias_name);
                try tree.pushStringPair("alias-name", alias_name_str);
            },
            .malformed => |malformed| {
                try tree.pushStringPair("type", "malformed");
                // Could potentially add diagnostic information here
                _ = malformed;
            },
        }
        
        try tree.endNode(begin, attrs);
    }
};

// Annotation type definition
pub const Annotation = struct {
    pub const Idx = enum(u32) { _ };
    
    type_anno: TypeAnno.Idx,
    signature: TypeVar,
    
    pub fn pushToSExprTree(self: *const Annotation, cir: anytype, tree: anytype, _: Annotation.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");
        
        const attrs = tree.beginNode();
        
        const type_anno_begin = tree.beginNode();
        try tree.pushStaticAtom("type-anno");
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
        try tree.pushStaticAtom("exposed-item");
        
        const name_str = cir.idents.getText(self.name);
        try tree.pushStringPair("name", name_str);
        
        if (self.alias) |alias_idx| {
            const alias_str = cir.idents.getText(alias_idx);
            try tree.pushStringPair("alias", alias_str);
        }
        
        try tree.pushBoolPair("is_wildcard", self.is_wildcard);
        
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

// Diagnostic types from CIR
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
        
        const line_text = region_info.calculateLineText(source, line_starts);
        try report.document.addText("The number literal is invalid or too large to represent:");
        try report.document.addLineBreak();
        try report.addCodeSnippet(line_text, region_info.start_line_idx + 1);
        
        const context = try std.fmt.allocPrint(allocator, "in file {s}", .{filename});
        const owned_context = try report.addOwnedString(context);
        try report.addNote(owned_context);
        
        return report;
    }
};
pub const NodeStore = @import("NodeStore.zig");
pub const Node = @import("Node.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
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
        try tree.pushStaticAtom("record-field");
        
        const label_str = cir.idents.getText(self.name);
        try tree.pushStringPair("label", label_str);
        
        const attrs = tree.beginNode();
        
        const value_begin = tree.beginNode();
        try tree.pushStaticAtom("value");
        const value_attrs = tree.beginNode();
        try cir.store.getExpr(self.value).pushToSExprTree(cir, tree, self.value);
        try tree.endNode(value_begin, value_attrs);
        
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
        try tree.pushStaticAtom("external-decl");
        
        const qualified_name_str = cir.idents.getText(self.qualified_name);
        try tree.pushStringPair("qualified-name", qualified_name_str);
        
        const module_name_str = cir.idents.getText(self.module_name);
        try tree.pushStringPair("module-name", module_name_str);
        
        const local_name_str = cir.idents.getText(self.local_name);
        try tree.pushStringPair("local-name", local_name_str);
        
        const kind_str = switch (self.kind) {
            .value => "value",
            .type => "type",
        };
        try tree.pushStringPair("kind", kind_str);
        
        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
    
    pub fn pushToSExprTreeWithRegion(self: *const ExternalDecl, cir: anytype, tree: anytype, region: Region) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("external-decl");
        
        // Add region info
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);
        
        const qualified_name_str = cir.idents.getText(self.qualified_name);
        try tree.pushStringPair("qualified-name", qualified_name_str);
        
        const module_name_str = cir.idents.getText(self.module_name);
        try tree.pushStringPair("module-name", module_name_str);
        
        const local_name_str = cir.idents.getText(self.local_name);
        try tree.pushStringPair("local-name", local_name_str);
        
        const kind_str = switch (self.kind) {
            .value => "value",
            .type => "type",
        };
        try tree.pushStringPair("kind", kind_str);
        
        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
};

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;

const Self = @This();

// Backward compatibility field - allows code that expects ir.env.X to work
// This should be set to point to self after the ModuleEnv is allocated on the heap
env: *Self = undefined,
gpa: std.mem.Allocator,
idents: Ident.Store,
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
strings: StringLiteral.Store,
types: types_mod.Store,
/// Map of exposed items by their string representation (not interned)
/// This is built during canonicalization and preserved for later use
exposed_by_str: collections.SafeStringHashMap(void),
/// Map of exposed item names to their CIR node indices (stored as u16)
/// This is populated during canonicalization to allow cross-module lookups
exposed_nodes: collections.SafeStringHashMap(u16),

/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: collections.SafeList(u32),

/// The source code of this module.
source: []const u8,

// ===== CIR fields duplicated from CIR.zig =====
// NOTE: These fields are populated during canonicalization and preserved for later use

/// All the definitions in the module (populated by canonicalization)
all_defs: Def.Span,
/// All the top-level statements in the module (populated by canonicalization)
all_statements: Statement.Span,
/// All external declarations referenced in this module
external_decls: ExternalDecl.SafeList,
/// Store for interned module imports
imports: Import.Store,
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,
/// Diagnostics collected during canonicalization (optional)
diagnostics: Diagnostic.Span,
/// Stores the raw nodes which represent the intermediate representation
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
        .env = undefined, // Will be set by setSelfReference() after heap allocation
        .gpa = gpa,
        .idents = try Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = try collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = try StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .types = try types_mod.Store.initCapacity(gpa, 2048, 512),
        .exposed_by_str = try collections.SafeStringHashMap(void).initCapacity(gpa, 64),
        .exposed_nodes = try collections.SafeStringHashMap(u16).initCapacity(gpa, 64),
        .line_starts = try collections.SafeList(u32).initCapacity(gpa, 256),
        .source = source,
        // Initialize CIR fields with empty/default values
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = try ExternalDecl.SafeList.initCapacity(gpa, 16),
        .imports = Import.Store.init(),
        .module_name = "", // Will be set later during canonicalization
        .diagnostics = Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } },
        .store = try NodeStore.initCapacity(gpa, 10_000), // Using same capacity as CIR
    };
}

/// Set the self-reference after the ModuleEnv has been allocated on the heap.
/// This is needed for backward compatibility with code that expects ir.env.X
pub fn setSelfReference(self: *Self) void {
    self.env = self;
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.idents.deinit(self.gpa);
    self.ident_ids_for_slicing.deinit(self.gpa);
    self.strings.deinit(self.gpa);
    self.types.deinit();
    self.line_starts.deinit(self.gpa);
    self.exposed_by_str.deinit(self.gpa);
    self.exposed_nodes.deinit(self.gpa);
    // Clean up CIR fields
    self.external_decls.deinit(self.gpa);
    self.imports.deinit(self.gpa);
    // diagnostics are stored in the NodeStore, no need to free separately
    self.store.deinit();
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self) !void {
    // Reset line_starts by creating a new SafeList
    self.line_starts.deinit(self.gpa);
    self.line_starts = try collections.SafeList(u32).initCapacity(self.gpa, 256);

    // if the source is empty, we're done
    if (self.source.len == 0) {
        return;
    }

    // the first line starts at offset 0
    _ = try self.line_starts.append(self.gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (self.source) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            _ = try self.line_starts.append(self.gpa, pos + 1);
        }
        pos += 1;
    }
}

/// Get diagnostic position information for a given range
pub fn calcRegionInfo(self: *const Self, source: []const u8, begin: u32, end: u32) !RegionInfo {
    return RegionInfo.position(source, self.line_starts.items.items, begin, end);
}


/// Freeze all interners in this module environment, preventing any new entries from being added.
/// This should be called after canonicalization is complete, so that
/// we know it's safe to serialize/deserialize the part of the interner
/// that goes from ident to string, because we don't go from string to ident
/// (or add new entries) in any of the later stages of compilation.
pub fn freezeInterners(self: *Self) void {
    self.idents.freeze();
    self.strings.freeze();
}

// Temporary CIR struct for pushToSExprTree compatibility
const CIR = struct {
    env: *Self,
    store: NodeStore,
    diagnostics: Diagnostic.Span,
    all_defs: Def.Span,
    all_statements: Statement.Span,
    external_decls: ExternalDecl.SafeList,
    imports: Import.Store,
    module_name: []const u8,
};

// ===== CIR functionality duplicated from CIR.zig =====

fn literal_from_source(self: *Self, start_offset: u32, end_offset: u32) []const u8 {
    if (self.source.len > 0 and end_offset <= self.source.len and start_offset <= end_offset) {
        return self.source[start_offset..end_offset];
    } else {
        return "";
    }
}

/// Records a diagnostic error during canonicalization without blocking compilation.
pub fn pushDiagnostic(self: *Self, reason: Diagnostic) std.mem.Allocator.Error!void {
    _ = try self.addDiagnosticAndTypeVar(reason, .err);
}

/// Creates a malformed node that represents a runtime error in the IR.
pub fn pushMalformed(self: *Self, comptime RetIdx: type, reason: Diagnostic) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const diag_idx = try self.addDiagnosticAndTypeVar(reason, .err);
    const malformed_idx = try self.addMalformedAndTypeVar(diag_idx, .err, Region.zero());
    return castIdx(Node.Idx, RetIdx, malformed_idx);
}

/// Helper function to check if a type is castable
fn isCastable(comptime T: type) bool {
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

/// Helper function to cast between index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}

// ===== Functions duplicated from CIR.zig =====

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]Diagnostic {
    const diagnostic_indices = self.store.sliceDiagnostics(self.diagnostics);
    const diagnostics = try self.gpa.alloc(Diagnostic, diagnostic_indices.len);
    for (diagnostic_indices, 0..) |diagnostic_idx, i| {
        diagnostics[i] = self.store.getDiagnostic(diagnostic_idx);
    }
    return diagnostics;
}

// Real Report type from the reporting module
pub const Report = reporting.Report;

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: Diagnostic, allocator: std.mem.Allocator, _: []const u8) !Report {
    switch (diagnostic) {
        .invalid_num_literal => |invalid| {
            var report = Report.init(allocator, "Invalid number literal", .runtime_error);
            try report.addHeader("Invalid Number Literal");
            
            const region_info = try self.getRegionInfo(invalid.region);
            const source_line = try self.getSourceLine(invalid.region);
            
            try report.document.addText("The number literal is invalid or too large to represent:");
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .undefined_variable => |undef| {
            var report = Report.init(allocator, "Undefined variable", .runtime_error);
            try report.addHeader("Undefined Variable");
            
            const var_name = self.idents.getText(undef.name);
            const region_info = try self.getRegionInfo(undef.region);
            const source_line = try self.getSourceLine(undef.region);
            
            const message = try std.fmt.allocPrint(allocator, "The variable '{s}' is not defined:", .{var_name});
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .type_mismatch => |mismatch| {
            var report = Report.init(allocator, "Type mismatch", .runtime_error);
            try report.addHeader("Type Mismatch");
            
            const region_info = try self.getRegionInfo(mismatch.region);
            const source_line = try self.getSourceLine(mismatch.region);
            
            try report.document.addText("Expected and actual types do not match:");
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .duplicate_record_field => |dup| {
            var report = Report.init(allocator, "Duplicate record field", .runtime_error);
            try report.addHeader("Duplicate Record Field");
            
            const field_name = self.idents.getText(dup.field_name);
            const region_info = try self.getRegionInfo(dup.duplicate_region);
            const source_line = try self.getSourceLine(dup.duplicate_region);
            
            const message = try std.fmt.allocPrint(allocator, "The record field '{s}' is defined more than once:", .{field_name});
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .unused_type_var_name => |unused| {
            var report = Report.init(allocator, "Unused type variable", .warning);
            try report.addHeader("Unused Type Variable");
            
            const var_name = self.idents.getText(unused.name);
            const suggested_name = self.idents.getText(unused.suggested_name);
            const region_info = try self.getRegionInfo(unused.region);
            const source_line = try self.getSourceLine(unused.region);
            
            const message = try std.fmt.allocPrint(allocator, "Type variable '{s}' is not used. Consider renaming to '{s}':", .{ var_name, suggested_name });
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .type_var_marked_unused => |marked| {
            var report = Report.init(allocator, "Type variable marked unused", .runtime_error);
            try report.addHeader("Type Variable Marked Unused");
            
            const var_name = self.idents.getText(marked.name);
            const suggested_name = self.idents.getText(marked.suggested_name);
            const region_info = try self.getRegionInfo(marked.region);
            const source_line = try self.getSourceLine(marked.region);
            
            const message = try std.fmt.allocPrint(allocator, "Type variable '{s}' is marked as unused but is actually used. Consider renaming to '{s}':", .{ var_name, suggested_name });
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .type_var_ending_in_underscore => |underscore| {
            var report = Report.init(allocator, "Type variable ends with underscore", .warning);
            try report.addHeader("Type Variable Ending in Underscore");
            
            const var_name = self.idents.getText(underscore.name);
            const suggested_name = self.idents.getText(underscore.suggested_name);
            const region_info = try self.getRegionInfo(underscore.region);
            const source_line = try self.getSourceLine(underscore.region);
            
            const message = try std.fmt.allocPrint(allocator, "Type variable '{s}' ends with underscore. Consider renaming to '{s}':", .{ var_name, suggested_name });
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
        .underscore_in_type_declaration => |underscore_decl| {
            var report = Report.init(allocator, "Underscore in type declaration", .runtime_error);
            try report.addHeader("Underscore in Type Declaration");
            
            const region_info = try self.getRegionInfo(underscore_decl.region);
            const source_line = try self.getSourceLine(underscore_decl.region);
            
            const kind = if (underscore_decl.is_alias) "alias" else "opaque type";
            const message = try std.fmt.allocPrint(allocator, "Underscore cannot be used in a type {s} declaration:", .{kind});
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            try report.addCodeSnippet(source_line, region_info.start_line_idx);
            
            return report;
        },
    }
}

/// Get region info for a given region
pub fn getRegionInfo(self: *const Self, region: Region) !RegionInfo {
    return base.RegionInfo.position(self.source, self.line_starts.items.items, region.start.offset, region.end.offset);
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const Self, region: Region) ![]const u8 {
    const region_info = try self.getRegionInfo(region);
    const line_start = self.line_starts.items[region_info.start_line_idx];
    const line_end = if (region_info.start_line_idx + 1 < self.line_starts.items.len)
        self.line_starts.items[region_info.start_line_idx + 1]
    else
        self.source.len;
    
    return self.source[line_start..line_end];
}

/// Convert a type into a node index
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Convert a type into a type var
pub fn varFrom(idx: anytype) TypeVar {
    return @enumFromInt(@intFromEnum(idx));
}

/// Assert that CIR, regions and types are all in sync
pub inline fn debugAssertArraysInSync(self: *const Self) void {
    if (std.debug.runtime_safety) {
        const cir_nodes = self.store.nodes.items.len;
        const region_nodes = self.store.regions.len();
        const type_nodes = self.types.len();
        if (!(cir_nodes == region_nodes and region_nodes == type_nodes)) {
            std.debug.panic(
                "Arrays out of sync:\n  cir_nodes={}\n  region_nodes={}\n  type_nodes={}\n",
                .{ cir_nodes, region_nodes, type_nodes },
            );
        }
    }
}

/// Assert that CIR, regions and types are all in sync
inline fn debugAssertIdxsEql(comptime desc: []const u8, idx1: anytype, idx2: anytype) void {
    if (std.debug.runtime_safety) {
        if (@intFromEnum(idx1) != @intFromEnum(idx2)) {
            std.debug.panic(
                "{s} idxs out of sync: {} != {}\n",
                .{ desc, @intFromEnum(idx1), @intFromEnum(idx2) },
            );
        }
    }
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addDefAndTypeVar(self: *Self, expr: Def, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("self", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeHeaderAndTypeVar(self: *Self, expr: TypeHeader, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeHeader.Idx {
    const expr_idx = try self.store.addTypeHeader(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeHeaderAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addStatementAndTypeVar(self: *Self, expr: Statement, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Statement.Idx {
    const expr_idx = try self.store.addStatement(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addStatementAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addPatternAndTypeVar(self: *Self, expr: Pattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addExprAndTypeVar(self: *Self, expr: Expr, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExprAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addRecordFieldAndTypeVar(self: *Self, expr: RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addRecordDestructAndTypeVar(self: *Self, expr: Pattern.RecordDestruct, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordDestructorAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addIfBranchAndTypeVar(self: *Self, expr: Expr.IfBranch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addIfBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMatchBranchAndTypeVar(self: *Self, expr: Expr.Match.Branch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addWhereClauseAndTypeVar(self: *Self, expr: WhereClause, content: types_mod.Content, region: Region) std.mem.Allocator.Error!WhereClause.Idx {
    const expr_idx = try self.store.addWhereClause(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addWhereClauseAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeAnnoAndTypeVar(self: *Self, expr: TypeAnno, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeAnnoAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addAnnotationAndTypeVar(self: *Self, expr: Annotation, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVar(self: *Self, expr: TypeAnno.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addExposedItemAndTypeVar(self: *Self, expr: ExposedItem, content: types_mod.Content, region: Region) std.mem.Allocator.Error!ExposedItem.Idx {
    const expr_idx = try self.store.addExposedItem(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExposedItemAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addDiagnosticAndTypeVar(self: *Self, reason: Diagnostic, content: types_mod.Content) std.mem.Allocator.Error!Diagnostic.Idx {
    const expr_idx = try self.store.addDiagnostic(reason);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addDiagnosticAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMalformedAndTypeVar(self: *Self, diagnostic_idx: Diagnostic.Idx, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Node.Idx {
    const malformed_idx = try self.store.addMalformed(diagnostic_idx, region);
    const malformed_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMalformedAndTypeVar", malformed_idx, malformed_var);
    self.debugAssertArraysInSync();
    return malformed_idx;
}

/// Add a new match branch pattern and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMatchBranchPatternAndTypeVar(self: *Self, expr: Expr.Match.BranchPattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern record field and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addPatternRecordFieldAndTypeVar(self: *Self, expr: PatternRecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!PatternRecordField.Idx {
    const expr_idx = try self.store.addPatternRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeSlotAndTypeVar(
    self: *Self,
    parent_node: Node.Idx,
    content: types_mod.Content,
    region: Region,
    comptime RetIdx: type,
) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const node_idx = try self.store.addTypeVarSlot(parent_node, region);
    const node_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeSlotAndTypeVar", node_idx, node_var);
    self.debugAssertArraysInSync();
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeSlotAndTypeVarRedirect(
    self: *Self,
    parent_node: Node.Idx,
    redirect_to: TypeVar,
    region: Region,
    comptime RetIdx: type,
) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const node_idx = try self.store.addTypeVarSlot(parent_node, region);
    const node_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addTypeSlotAndTypeVarRedirect", node_idx, node_var);
    self.debugAssertArraysInSync();
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Function that redirects an existing node to the provided var.
/// Assert that the requested idx in in bounds
pub fn redirectTypeTo(
    self: *Self,
    comptime FromIdx: type,
    at_idx: FromIdx,
    redirect_to: types_mod.Var,
) std.mem.Allocator.Error!void {
    comptime if (!isCastable(FromIdx)) @compileError("Idx type " ++ @typeName(FromIdx) ++ " is not castable");
    self.debugAssertArraysInSync();
    std.debug.assert(@intFromEnum(at_idx) < self.types.len());

    const var_ = varFrom(at_idx);
    try self.types.setVarRedirect(var_, redirect_to);
}

/// Adds an external declaration to the CIR and returns its index
pub fn pushExternalDecl(self: *Self, decl: ExternalDecl) std.mem.Allocator.Error!ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = try self.external_decls.append(self.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const Self, idx: ExternalDecl.Idx) *const ExternalDecl {
    return self.external_decls.get(@as(ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *Self, decls: []const ExternalDecl) std.mem.Allocator.Error!ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = try self.external_decls.append(self.gpa, decl);
    }
    return ExternalDecl.Span{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const Self, span: ExternalDecl.Span) []const ExternalDecl {
    const range = ExternalDecl.SafeList.Range{ .start = @enumFromInt(span.span.start), .end = @enumFromInt(span.span.start + span.span.len) };
    return self.external_decls.rangeToSlice(range);
}

/// Retrieves the text of an identifier by its index
pub fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.idents.getText(idx);
}

/// Helper to format pattern index for s-expr output
fn formatPatternIdxNode(gpa: std.mem.Allocator, pattern_idx: Pattern.Idx) SExpr {
    var node = SExpr.init(gpa, "pid");
    node.appendUnsignedInt(gpa, @intFromEnum(pattern_idx));
    return node;
}

/// Helper function to generate the S-expression node for the entire Canonical IR.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(self: *Self, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        try self.store.getExpr(expr_idx).pushToSExprTree(self, tree, expr_idx);
    } else {
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression tree
        const defs_slice = self.store.sliceDefs(self.all_defs);
        const statements_slice = self.store.sliceStatements(self.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and self.external_decls.len() == 0) {
            try tree.pushBoolPair("empty", true);
        }
        const attrs = tree.beginNode();

        for (defs_slice) |def_idx| {
            try self.store.getDef(def_idx).pushToSExprTree(self, tree);
        }

        for (statements_slice) |stmt_idx| {
            try self.store.getStatement(stmt_idx).pushToSExprTree(self, tree, stmt_idx);
        }

        for (0..self.external_decls.len()) |i| {
            const external_decl = self.external_decls.get(@enumFromInt(i));
            try external_decl.pushToSExprTree(self, tree);
        }

        try tree.endNode(root_begin, attrs);
    }
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSexprNode(self: *const Self, node: *SExpr, idx: anytype) std.mem.Allocator.Error!void {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    try self.appendRegionInfoToSexprNodeFromRegion(node, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSexprNodeFromRegion(self: *const Self, node: *SExpr, region: Region) std.mem.Allocator.Error!void {
    const info = self.getRegionInfo(region);
    try node.appendByteRange(
        self.gpa,
        info,
        region.start.offset,
        region.end.offset,
    );
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSExprTree(self: *const Self, tree: *SExprTree, idx: anytype) std.mem.Allocator.Error!void {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    try self.appendRegionInfoToSExprTreeFromRegion(tree, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSExprTreeFromRegion(self: *const Self, tree: *SExprTree, region: Region) std.mem.Allocator.Error!void {
    const info = self.getRegionInfo(region) catch RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };
    try tree.pushBytesRange(
        region.start.offset,
        region.end.offset,
        info,
    );
}

/// Get region information for a node in the Canonical IR.
pub fn getNodeRegionInfo(self: *const Self, idx: anytype) RegionInfo {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    return self.getRegionInfo(region);
}

/// Helper function to convert type information from the Canonical IR to an SExpr node
/// in S-expression format for snapshot testing. Implements the definition-focused
/// format showing final types for defs, expressions, and builtins.
pub fn pushTypesToSExprTree(self: *Self, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    const gpa = self.gpa;

    // Create TypeWriter for converting types to strings
    var type_writer = try @import("type_writers.zig").TypeWriter.init(gpa, self);
    defer type_writer.deinit();

    if (maybe_expr_idx) |expr_idx| {
        const expr_var = @as(types_mod.Var, @enumFromInt(@intFromEnum(expr_idx)));

        const expr_begin = tree.beginNode();
        try tree.pushStaticAtom("expr");

        try self.appendRegionInfoToSExprTree(tree, expr_idx);

        if (@intFromEnum(expr_var) > self.types.slots.backing.len()) {
            const unknown_begin = tree.beginNode();
            try tree.pushStaticAtom("unknown");
            const unknown_attrs = tree.beginNode();
            try tree.endNode(unknown_begin, unknown_attrs);
        } else {
            try type_writer.write(expr_var);
            try tree.pushStringPair("type", type_writer.get());
        }

        const expr_attrs = tree.beginNode();
        try tree.endNode(expr_begin, expr_attrs);
    } else {
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("inferred-types");

        const attrs = tree.beginNode();

        // Collect definitions
        const defs_begin = tree.beginNode();
        try tree.pushStaticAtom("defs");
        const defs_attrs = tree.beginNode();

        const all_defs = self.store.sliceDefs(self.all_defs);

        for (all_defs) |def_idx| {
            const def = self.store.getDef(def_idx);

            // Extract identifier name from the pattern (assuming it's an assign pattern)
            const pattern = self.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |_| {
                    const patt_begin = tree.beginNode();
                    try tree.pushStaticAtom("patt");

                    // Get the pattern region instead of the whole def region
                    const pattern_region = self.store.getPatternRegion(def.pattern);
                    try self.appendRegionInfoToSExprTreeFromRegion(tree, pattern_region);

                    // Get the type variable for this definition
                    const def_var = castIdx(Def.Idx, TypeVar, def_idx);

                    // Clear the buffer and write the type
                    try type_writer.write(def_var);
                    try tree.pushStringPair("type", type_writer.get());

                    const patt_attrs = tree.beginNode();
                    try tree.endNode(patt_begin, patt_attrs);
                },
                else => {
                    // For non-assign patterns, we could handle destructuring, but for now skip
                    continue;
                },
            }
        }

        try tree.endNode(defs_begin, defs_attrs);

        const all_stmts = self.store.sliceStatements(self.all_statements);

        var has_type_decl = false;
        for (all_stmts) |stmt_idx| {
            const stmt = self.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_alias_decl => |_| {
                    has_type_decl = true;
                    break;
                },
                .s_nominal_decl => |_| {
                    has_type_decl = true;
                    break;
                },

                else => {
                    // For non-assign patterns, we could handle destructuring, but for now skip
                    continue;
                },
            }
        }

        // Collect statements
        if (has_type_decl) {
            const stmts_begin = tree.beginNode();
            try tree.pushStaticAtom("type_decls");
            const stmts_attrs = tree.beginNode();

            for (all_stmts) |stmt_idx| {
                const stmt = self.store.getStatement(stmt_idx);

                // Get the type variable for this definition
                const stmt_var = castIdx(Statement.Idx, TypeVar, stmt_idx);

                switch (stmt) {
                    .s_alias_decl => |alias| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        try tree.pushStaticAtom("alias");
                        const alias_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, alias_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        try tree.pushStringPair("type", type_writer.get());
                        const stmt_node_attrs = tree.beginNode();

                        const header = self.store.getTypeHeader(alias.header);
                        try header.pushToSExprTree(self, tree, alias.header);

                        try tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        try tree.pushStaticAtom("nominal");
                        const nominal_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, nominal_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        try tree.pushStringPair("type", type_writer.get());

                        const stmt_node_attrs = tree.beginNode();

                        const header = self.store.getTypeHeader(nominal.header);
                        try header.pushToSExprTree(self, tree, nominal.header);

                        try tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },

                    else => {
                        // For non-assign patterns, we could handle destructuring, but for now skip
                        continue;
                    },
                }
            }

            try tree.endNode(stmts_begin, stmts_attrs);
        }

        // Collect expression types (for significant expressions with regions)
        const exprs_begin = tree.beginNode();
        try tree.pushStaticAtom("expressions");
        const exprs_attrs = tree.beginNode();

        for (all_defs) |def_idx| {
            const def = self.store.getDef(def_idx);

            // Get the expression type
            const expr_var = @as(types_mod.Var, @enumFromInt(@intFromEnum(def.expr)));

            const expr_node_begin = tree.beginNode();
            try tree.pushStaticAtom("expr");

            // Add region info for the expression
            const expr_region = self.store.getExprRegion(def.expr);
            try self.appendRegionInfoToSExprTreeFromRegion(tree, expr_region);

            if (@intFromEnum(expr_var) > self.types.slots.backing.len()) {
                const unknown_begin = tree.beginNode();
                try tree.pushStaticAtom("unknown");
                const unknown_attrs = tree.beginNode();
                try tree.endNode(unknown_begin, unknown_attrs);
            } else {
                // Clear the buffer and write the type
                try type_writer.write(expr_var);
                try tree.pushStringPair("type", type_writer.get());
            }

            const expr_node_attrs = tree.beginNode();
            try tree.endNode(expr_node_begin, expr_node_attrs);
        }

        try tree.endNode(exprs_begin, exprs_attrs);

        try tree.endNode(root_begin, attrs);
    }
}
