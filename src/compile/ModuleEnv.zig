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
// const reporting = @import("../reporting.zig"); // Not needed for placeholder implementation
// const compile = @import("../compile.zig"); // Not needed for now

// Temporary placeholder types until we can properly move CIR types
// TODO: Eventually we'll want to move these types to a shared location
const CIR_Types = struct {
    // Forward declare Node type
    pub const Node = struct {
        pub const Idx = enum(u32) { _ };
    };
    
    pub const Expr = union(enum) {
        pub const Idx = enum(u32) { _ };
        
        pub const IfBranch = struct {
            pub const Idx = enum(u32) { _ };
        };
        pub const Match = struct {
            pub const Branch = struct {
                pub const Idx = enum(u32) { _ };
            };
            pub const BranchPattern = struct {
                pub const Idx = enum(u32) { _ };
            };
        };
        
        num: struct { val: union(enum) { i64: i64 } },
        
        pub fn pushToSExprTree(self: *const @This(), cir: anytype, tree: anytype, idx: @This().Idx) !void {
            _ = self;
            _ = cir;
            _ = idx;
            try tree.pushStaticAtom("expr-stub");
        }
    };

    pub const Pattern = union(enum) {
        pub const Idx = enum(u32) { _ };
        
        pub const RecordDestruct = struct {
            pub const Idx = enum(u32) { _ };
        };
        
        assign: struct { ident: base.Ident.Idx },
    };

    pub const Statement = union(enum) {
        pub const Idx = enum(u32) { _ };
        
        s_malformed: CIR_Types.Node.Idx,
        s_alias_decl: struct { header: TypeHeader.Idx },
        s_nominal_decl: struct { header: TypeHeader.Idx },
        
        pub fn pushToSExprTree(self: *const @This(), cir: anytype, tree: anytype, idx: @This().Idx) !void {
            _ = self;
            _ = cir;
            _ = idx;
            try tree.pushStaticAtom("stmt-stub");
        }
    };

    pub const TypeAnno = struct {
        pub const Idx = enum(u32) { _ };
        
        pub const RecordField = struct {
            pub const Idx = enum(u32) { _ };
        };
    };
    
    pub const Def = struct {
        pub const Idx = enum(u32) { _ };
        
        pattern: CIR_Types.Pattern.Idx,
        expr: CIR_Types.Expr.Idx,
        
        pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) !void {
            _ = self;
            _ = cir;
            try tree.pushStaticAtom("def-stub");
        }
    };
    pub const TypeHeader = struct {
        pub const Idx = enum(u32) { _ };
        
        name: base.Ident.Idx,
        params: struct { span: struct { start: u32, len: u32 } },
        
        pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, idx: TypeHeader.Idx) !void {
            _ = self;
            _ = cir;
            _ = idx;
            try tree.pushStaticAtom("type-header-stub");
        }
    };
    pub const RecordField = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const WhereClause = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const Annotation = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const ExposedItem = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const PatternRecordField = struct {
        pub const Idx = enum(u32) { _ };
    };
    pub const ExternalDecl = cir_types.ExternalDecl;
};

// Diagnostic types from CIR
const Diagnostic = union(enum) {
    pub const Idx = enum(u32) { _ };
    
    not_implemented: struct { feature: StringLiteral.Idx },
    exposed_but_not_implemented: struct { ident: Ident.Idx, region: Region },
    redundant_exposed: struct { ident: Ident.Idx, region: Region, original_region: Region },
    invalid_num_literal: struct { region: Region },
    ident_already_in_scope: struct { ident: Ident.Idx, region: Region },
    ident_not_in_scope: struct { ident: Ident.Idx, region: Region },
    invalid_top_level_statement: struct { stmt: StringLiteral.Idx, region: Region },
    f64_pattern_literal: struct { region: Region },
    invalid_single_quote: void,
    crash_expects_string: struct { region: Region },
    empty_tuple: struct { region: Region },
    expr_not_canonicalized: struct { region: Region },
    invalid_string_interpolation: void,
    pattern_arg_invalid: void,
    pattern_not_canonicalized: void,
    can_lambda_not_implemented: void,
    lambda_body_not_canonicalized: void,
    if_condition_not_canonicalized: void,
    if_then_not_canonicalized: void,
    if_else_not_canonicalized: void,
    var_across_function_boundary: void,
    malformed_type_annotation: void,
    malformed_where_clause: struct { region: Region },
    shadowing_warning: struct { ident: Ident.Idx, region: Region, original_region: Region },
    type_redeclared: struct { name: Ident.Idx, original_region: Region, redeclared_region: Region },
    tuple_elem_not_canonicalized: void,
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
};
const NodeStore = struct {
    gpa: std.mem.Allocator,
    nodes: std.ArrayList(u8),
    regions: std.ArrayList(Region),
    
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) !NodeStore {
        return .{ 
            .gpa = gpa,
            .nodes = try std.ArrayList(u8).initCapacity(gpa, capacity),
            .regions = try std.ArrayList(Region).initCapacity(gpa, capacity),
        };
    }
    
    pub fn deinit(self: *NodeStore) void {
        self.nodes.deinit();
        self.regions.deinit();
    }
    
    // Stub methods for adding nodes
    pub fn addDef(self: *NodeStore, def: CIR_Types.Def, region: Region) !CIR_Types.Def.Idx {
        _ = def;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addTypeHeader(self: *NodeStore, header: CIR_Types.TypeHeader, region: Region) !CIR_Types.TypeHeader.Idx {
        _ = header;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addStatement(self: *NodeStore, stmt: Statement, region: Region) !Statement.Idx {
        _ = stmt;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addPattern(self: *NodeStore, pattern: Pattern, region: Region) !Pattern.Idx {
        _ = pattern;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addExpr(self: *NodeStore, expr: Expr, region: Region) !Expr.Idx {
        _ = expr;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addRecordField(self: *NodeStore, field: CIR_Types.RecordField, region: Region) !CIR_Types.RecordField.Idx {
        _ = field;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addRecordDestruct(self: *NodeStore, destruct: CIR_Types.Pattern.RecordDestruct, region: Region) !CIR_Types.Pattern.RecordDestruct.Idx {
        _ = destruct;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addIfBranch(self: *NodeStore, branch: CIR_Types.Expr.IfBranch, region: Region) !CIR_Types.Expr.IfBranch.Idx {
        _ = branch;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addMatchBranch(self: *NodeStore, branch: CIR_Types.Expr.Match.Branch, region: Region) !CIR_Types.Expr.Match.Branch.Idx {
        _ = branch;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addWhereClause(self: *NodeStore, clause: CIR_Types.WhereClause, region: Region) !CIR_Types.WhereClause.Idx {
        _ = clause;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addTypeAnno(self: *NodeStore, anno: TypeAnno, region: Region) !TypeAnno.Idx {
        _ = anno;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addAnnotation(self: *NodeStore, annotation: CIR_Types.Annotation, region: Region) !CIR_Types.Annotation.Idx {
        _ = annotation;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addAnnoRecordField(self: *NodeStore, field: CIR_Types.TypeAnno.RecordField, region: Region) !CIR_Types.TypeAnno.RecordField.Idx {
        _ = field;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addExposedItem(self: *NodeStore, item: CIR_Types.ExposedItem, region: Region) !CIR_Types.ExposedItem.Idx {
        _ = item;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addDiagnostic(self: *NodeStore, diag: Diagnostic) !Diagnostic.Idx {
        _ = diag;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(Region.zero());
        return @enumFromInt(idx);
    }
    
    pub fn addMalformed(self: *NodeStore, diag_idx: Diagnostic.Idx, region: Region) !Node.Idx {
        _ = diag_idx;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addMatchBranchPattern(self: *NodeStore, pattern: CIR_Types.Expr.Match.BranchPattern, region: Region) !CIR_Types.Expr.Match.BranchPattern.Idx {
        _ = pattern;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addPatternRecordField(self: *NodeStore, field: CIR_Types.PatternRecordField, region: Region) !CIR_Types.PatternRecordField.Idx {
        _ = field;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    pub fn addTypeVarSlot(self: *NodeStore, parent: Node.Idx, region: Region) !Node.Idx {
        _ = parent;
        const idx = @as(u32, @intCast(self.nodes.items.len));
        try self.nodes.append(0);
        try self.regions.append(region);
        return @enumFromInt(idx);
    }
    
    // Stub getter methods
    pub fn getNodeRegion(self: *const NodeStore, idx: Node.Idx) Region {
        const i = @intFromEnum(idx);
        if (i < self.regions.items.len) {
            return self.regions.items[i];
        }
        return Region.zero();
    }
    
    pub fn getDef(self: *const NodeStore, idx: CIR_Types.Def.Idx) CIR_Types.Def {
        _ = self;
        _ = idx;
        return .{ .pattern = @enumFromInt(0), .expr = @enumFromInt(0) };
    }
    
    pub fn getStatement(self: *const NodeStore, idx: Statement.Idx) Statement {
        _ = self;
        _ = idx;
        return .{ .s_malformed = @enumFromInt(0) };
    }
    
    pub fn getPattern(self: *const NodeStore, idx: Pattern.Idx) Pattern {
        _ = self;
        _ = idx;
        return .{ .assign = .{ .ident = @enumFromInt(0) } };
    }
    
    pub fn getExpr(self: *const NodeStore, idx: Expr.Idx) Expr {
        _ = self;
        _ = idx;
        return .{ .num = .{ .val = .{ .i64 = 0 } } };
    }
    
    pub fn getTypeHeader(self: *const NodeStore, idx: CIR_Types.TypeHeader.Idx) CIR_Types.TypeHeader {
        _ = self;
        _ = idx;
        return .{ .name = @enumFromInt(0), .params = .{ .span = .{ .start = 0, .len = 0 } } };
    }
    
    pub fn getPatternRegion(self: *const NodeStore, idx: Pattern.Idx) Region {
        return self.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    }
    
    pub fn getExprRegion(self: *const NodeStore, idx: Expr.Idx) Region {
        return self.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    }
    
    pub fn getStatementRegion(self: *const NodeStore, idx: Statement.Idx) Region {
        return self.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    }
    
    // Stub slice methods
    pub fn sliceDefs(self: *const NodeStore, span: cir_types.DefSpan) []const CIR_Types.Def.Idx {
        _ = self;
        _ = span;
        return &[_]CIR_Types.Def.Idx{};
    }
    
    pub fn sliceStatements(self: *const NodeStore, span: cir_types.StatementSpan) []const Statement.Idx {
        _ = self;
        _ = span;
        return &[_]Statement.Idx{};
    }
};
const Node = CIR_Types.Node;
const Expr = CIR_Types.Expr;
const Pattern = CIR_Types.Pattern;
const Statement = CIR_Types.Statement;
const TypeAnno = CIR_Types.TypeAnno;
// const Diagnostic = CIR_Types.Diagnostic; // Using the Diagnostic type defined above

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;

const Self = @This();

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
all_defs: cir_types.DefSpan,
/// All the top-level statements in the module (populated by canonicalization)
all_statements: cir_types.StatementSpan,
/// All external declarations referenced in this module
external_decls: cir_types.ExternalDecl.SafeList,
/// Store for interned module imports
imports: cir_types.Import.Store,
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,
/// Diagnostics collected during canonicalization (optional)
diagnostics: ?[]const u8,
/// Stores the raw nodes which represent the intermediate representation
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
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
        .external_decls = try cir_types.ExternalDecl.SafeList.initCapacity(gpa, 16),
        .imports = cir_types.Import.Store.init(),
        .module_name = "", // Will be set later during canonicalization
        .diagnostics = null,
        .store = try NodeStore.initCapacity(gpa, 10_000), // Using same capacity as CIR
    };
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
    if (self.diagnostics) |diags| {
        self.gpa.free(diags);
    }
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

/// Get diagnostic position information for a given region
pub fn getRegionInfo(self: *const Self, region: Region) RegionInfo {
    const empty = RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };

    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const source = self.source;

    const info = RegionInfo.position(source, self.line_starts.items.items, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
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
    diagnostics: ?[]const u8,
    all_defs: cir_types.DefSpan,
    all_statements: cir_types.StatementSpan,
    external_decls: cir_types.ExternalDecl.SafeList,
    imports: cir_types.Import.Store,
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
        CIR_Types.Def.Idx,
        CIR_Types.TypeHeader.Idx,
        CIR_Types.RecordField.Idx,
        CIR_Types.Pattern.RecordDestruct.Idx,
        CIR_Types.Expr.IfBranch.Idx,
        CIR_Types.Expr.Match.Branch.Idx,
        CIR_Types.WhereClause.Idx,
        CIR_Types.Annotation.Idx,
        CIR_Types.TypeAnno.RecordField.Idx,
        CIR_Types.ExposedItem.Idx,
        CIR_Types.Expr.Match.BranchPattern.Idx,
        CIR_Types.PatternRecordField.Idx,
        Node.Idx,
        => true,
        else => false,
    };
}

/// Helper function to cast between index types
fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}

// ===== Functions duplicated from CIR.zig =====

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]Diagnostic {
    // TODO: Implement diagnostic retrieval from store
    // This is a stub implementation to get everything compiling
    _ = self;
    return &[_]Diagnostic{};
}

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !void {
    // TODO: Implement diagnostic to report conversion
    // This is a stub implementation to get everything compiling
    _ = self;
    _ = diagnostic;
    _ = allocator;
    _ = filename;
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
        const cir_nodes = self.store.nodes.len();
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
pub fn addDefAndTypeVar(self: *Self, expr: CIR_Types.Def, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("self", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeHeaderAndTypeVar(self: *Self, expr: CIR_Types.TypeHeader, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.TypeHeader.Idx {
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
pub fn addRecordFieldAndTypeVar(self: *Self, expr: CIR_Types.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addRecordDestructAndTypeVar(self: *Self, expr: CIR_Types.Pattern.RecordDestruct, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordDestructorAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addIfBranchAndTypeVar(self: *Self, expr: CIR_Types.Expr.IfBranch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addIfBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMatchBranchAndTypeVar(self: *Self, expr: CIR_Types.Expr.Match.Branch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addWhereClauseAndTypeVar(self: *Self, expr: CIR_Types.WhereClause, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.WhereClause.Idx {
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
pub fn addAnnotationAndTypeVar(self: *Self, expr: CIR_Types.Annotation, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVar(self: *Self, expr: CIR_Types.TypeAnno.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addExposedItemAndTypeVar(self: *Self, expr: CIR_Types.ExposedItem, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.ExposedItem.Idx {
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
pub fn addMatchBranchPatternAndTypeVar(self: *Self, expr: CIR_Types.Expr.Match.BranchPattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern record field and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addPatternRecordFieldAndTypeVar(self: *Self, expr: CIR_Types.PatternRecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR_Types.PatternRecordField.Idx {
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
pub fn pushExternalDecl(self: *Self, decl: cir_types.ExternalDecl) std.mem.Allocator.Error!cir_types.ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = try self.external_decls.append(self.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const Self, idx: cir_types.ExternalDecl.Idx) *const cir_types.ExternalDecl {
    return self.external_decls.get(@as(cir_types.ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *Self, decls: []const cir_types.ExternalDecl) std.mem.Allocator.Error!cir_types.ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = try self.external_decls.append(self.gpa, decl);
    }
    return cir_types.ExternalDecl.Span{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const Self, span: cir_types.ExternalDecl.Span) []const cir_types.ExternalDecl {
    const range = cir_types.ExternalDecl.SafeList.Range{ .start = @enumFromInt(span.span.start), .end = @enumFromInt(span.span.start + span.span.len) };
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
    // Create a temporary CIR wrapper to pass to the pushToSExprTree methods
    const temp_cir = CIR{
        .env = self,
        .store = self.store,
        .diagnostics = null,
        .all_defs = self.all_defs,
        .all_statements = self.all_statements,
        .external_decls = self.external_decls,
        .imports = self.imports,
        .module_name = self.module_name,
    };
    
    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        try self.store.getExpr(expr_idx).pushToSExprTree(&temp_cir, tree, expr_idx);
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
            try self.store.getDef(def_idx).pushToSExprTree(&temp_cir, tree);
        }

        for (statements_slice) |stmt_idx| {
            try self.store.getStatement(stmt_idx).pushToSExprTree(&temp_cir, tree, stmt_idx);
        }

        for (0..self.external_decls.len()) |i| {
            const external_decl = self.external_decls.get(@enumFromInt(i));
            try external_decl.pushToSExprTree(&temp_cir, tree);
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
    const info = self.getRegionInfo(region);
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
                    const def_var = castIdx(CIR_Types.Def.Idx, TypeVar, def_idx);

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

                        // Create a temporary CIR wrapper
                        const temp_cir = CIR{
                            .env = self,
                            .store = self.store,
                            .diagnostics = null,
                            .all_defs = self.all_defs,
                            .all_statements = self.all_statements,
                            .external_decls = self.external_decls,
                            .imports = self.imports,
                            .module_name = self.module_name,
                        };
                        const header = self.store.getTypeHeader(alias.header);
                        try header.pushToSExprTree(&temp_cir, tree, alias.header);

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

                        // Create a temporary CIR wrapper
                        const temp_cir2 = CIR{
                            .env = self,
                            .store = self.store,
                            .diagnostics = null,
                            .all_defs = self.all_defs,
                            .all_statements = self.all_statements,
                            .external_decls = self.external_decls,
                            .imports = self.imports,
                            .module_name = self.module_name,
                        };
                        const header = self.store.getTypeHeader(nominal.header);
                        try header.pushToSExprTree(&temp_cir2, tree, nominal.header);

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
