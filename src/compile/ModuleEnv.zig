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
const reporting = @import("reporting");
const CIR = @import("../check/CIR.zig");

// Re-export types from CIR module
pub const Def = CIR.Def;

pub const TypeHeader = CIR.TypeHeader;

pub const WhereClause = CIR.WhereClause;

pub const Annotation = CIR.Annotation;

pub const ExposedItem = CIR.ExposedItem;

pub const PatternRecordField = CIR.PatternRecordField;

pub const IntValue = CIR.IntValue;

pub const RocDec = CIR.RocDec;

pub const Diagnostic = CIR.Diagnostic;
pub const NodeStore = CIR.NodeStore;
pub const Node = CIR.Node;
pub const Expr = CIR.Expr;
pub const Pattern = CIR.Pattern;
pub const Statement = CIR.Statement;
pub const TypeAnno = CIR.TypeAnno;
pub const Import = CIR.Import;
pub const RecordField = CIR.RecordField;
pub const ExternalDecl = CIR.ExternalDecl;

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
/// Map of exposed item names to their node indices (stored as u16)
/// This is populated during canonicalization to allow cross-module lookups
exposed_nodes: collections.SafeStringHashMap(u16),

/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: collections.SafeList(u32),

/// The source code of this module.
source: []const u8,

// ===== Module compilation fields =====
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

/// Initialize the compilation fields in an existing ModuleEnv
pub fn initCIRFields(self: *Self, gpa: std.mem.Allocator, module_name: []const u8) !void {
    _ = gpa; // unused since we don't create new allocations
    self.all_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.all_statements = .{ .span = .{ .start = 0, .len = 0 } };
    // Note: external_decls already exists from ModuleEnv.init(), so we don't create a new one
    self.imports = Import.Store.init();
    self.module_name = module_name;
    self.diagnostics = Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
    // Note: self.store already exists from ModuleEnv.init(), so we don't create a new one
}

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
        // Initialize compilation fields with empty/default values
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = try ExternalDecl.SafeList.initCapacity(gpa, 16),
        .imports = Import.Store.init(),
        .module_name = "", // Will be set later during canonicalization
        .diagnostics = Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } },
        .store = try NodeStore.initCapacity(gpa, 10_000), // Default node store capacity
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
    // Clean up compilation fields
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


/// Freeze all interners in this module environment, preventing any new entries from being added.
/// This should be called after canonicalization is complete, so that
/// we know it's safe to serialize/deserialize the part of the interner
/// that goes from ident to string, because we don't go from string to ident
/// (or add new entries) in any of the later stages of compilation.
pub fn freezeInterners(self: *Self) void {
    self.idents.freeze();
    self.strings.freeze();
}


// ===== Module compilation functionality =====

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

/// Import helper functions from CIR
const isCastable = CIR.isCastable;
pub const castIdx = CIR.castIdx;

// ===== Module compilation functions =====

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]Diagnostic {
    // Get all diagnostics from the store, not just the ones in self.diagnostics span
    const all_diagnostics = try self.store.diagnosticSpanFrom(0);
    const diagnostic_indices = self.store.sliceDiagnostics(all_diagnostics);
    const diagnostics = try self.gpa.alloc(Diagnostic, diagnostic_indices.len);
    for (diagnostic_indices, 0..) |diagnostic_idx, i| {
        diagnostics[i] = self.store.getDiagnostic(diagnostic_idx);
    }
    return diagnostics;
}

// Real Report type from the reporting module
pub const Report = CIR.Report;

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !Report {
    
    return switch (diagnostic) {
        .invalid_num_literal => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            
            var report = Report.init(allocator, "INVALID NUMBER LITERAL", .runtime_error);
            try report.addHeader("Invalid Number Literal");
            
            try report.document.addText("The number literal is invalid or too large to represent:");
            try report.document.addLineBreak();
            
            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.source, self.line_starts.items.items);
            
            break :blk report;
        },
        .ident_not_in_scope => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            
            var report = Report.init(allocator, "UNDEFINED VARIABLE", .runtime_error);
            try report.addHeader("Undefined Variable");
            
            const ident_name = self.idents.getText(data.ident);
            
            const message = try std.fmt.allocPrint(allocator, "The variable '{s}' is not defined:", .{ident_name});
            defer allocator.free(message);
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            
            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.source, self.line_starts.items.items);
            
            break :blk report;
        },
        .exposed_but_not_implemented => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            
            var report = Report.init(allocator, "EXPOSED BUT NOT DEFINED", .runtime_error);
            try report.addHeader("Exposed but Not Defined");
            
            const ident_name = self.idents.getText(data.ident);
            
            const message = try std.fmt.allocPrint(allocator, "'{s}' is exposed in the module header but is not defined:", .{ident_name});
            defer allocator.free(message);
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            
            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.source, self.line_starts.items.items);
            
            break :blk report;
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.idents.getText(data.ident);
            
            var report = Report.init(allocator, "UNUSED VARIABLE", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            
            try report.document.addReflowingText("Variable ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" is not used anywhere in your code.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            
            const MAX_IDENT_FIXED_BUFFER = 100;
            if (owned_ident.len > MAX_IDENT_FIXED_BUFFER - 1) {
                try report.document.addReflowingText("If you don't need this variable, prefix it with an underscore to suppress this warning.");
            } else {
                // format the identifier with an underscore
                try report.document.addReflowingText("If you don't need this variable, prefix it with an underscore like ");
                var buf: [MAX_IDENT_FIXED_BUFFER]u8 = undefined;
                const owned_ident_with_underscore = try std.fmt.bufPrint(&buf, "_{s}", .{owned_ident});
                
                try report.document.addUnqualifiedSymbol(owned_ident_with_underscore);
                try report.document.addReflowingText(" to suppress this warning.");
            }
            
            try report.document.addLineBreak();
            try report.document.addReflowingText("The unused variable is declared here:");
            try report.document.addLineBreak();
            
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.source,
                self.line_starts.items.items,
            );
            
            break :blk report;
        },
        .underscore_in_type_declaration => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            
            var report = Report.init(allocator, "UNDERSCORE IN TYPE ALIAS", .runtime_error);
            try report.addHeader("Underscore in Type Alias");
            
            const kind = if (data.is_alias) "alias" else "opaque type";
            const message = try std.fmt.allocPrint(allocator, "Underscore cannot be used in a type {s} declaration:", .{kind});
            defer allocator.free(message);
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            
            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.source, self.line_starts.items.items);
            
            break :blk report;
        },
        .undeclared_type => |data| blk: {
            const type_name = self.idents.getText(data.name);
            const region_info = self.calcRegionInfo(data.region);
            
            var report = Report.init(allocator, "UNDECLARED TYPE", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.document.addReflowingText("The type ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" is not declared in this scope.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            
            try report.document.addReflowingText("This type is referenced here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.source,
                self.line_starts.items.items,
            );
            
            break :blk report;
        },
        else => {
            // For unhandled diagnostics, create a generic report
            const diagnostic_name = @tagName(diagnostic);
            
            var report = Report.init(allocator, "COMPILER DIAGNOSTIC", .runtime_error);
            try report.addHeader("Compiler Diagnostic");
            const message = try std.fmt.allocPrint(allocator, "Diagnostic type '{s}' is not yet handled in report generation.", .{diagnostic_name});
            defer allocator.free(message);
            const owned_message = try report.addOwnedString(message);
            try report.document.addText(owned_message);
            try report.document.addLineBreak();
            
            // Add location info even without specific region
            const location_msg = try std.fmt.allocPrint(allocator, "**{s}:0:0:0:0**", .{filename});
            defer allocator.free(location_msg);
            const owned_location = try report.addOwnedString(location_msg);
            try report.document.addText(owned_location);
            
            return report;
        }
    };
}

/// Get region info for a given region
pub fn getRegionInfo(self: *const Self, region: Region) !RegionInfo {
    return base.RegionInfo.position(self.source, self.line_starts.items.items, region.start.offset, region.end.offset);
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn calcRegionInfo(self: *const Self, region: Region) RegionInfo {
    const empty = RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };

    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const source = self.source;

    const info = base.RegionInfo.position(source, self.line_starts.items.items, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
}

/// Extract a literal from source code between given byte offsets
pub fn literal_from_source(self: *const Self, start_offset: u32, end_offset: u32) []const u8 {
    return self.source[start_offset..end_offset];
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const Self, region: Region) ![]const u8 {
    const region_info = try self.getRegionInfo(region);
    const line_start = self.line_starts.items.items[region_info.start_line_idx];
    const line_end = if (region_info.start_line_idx + 1 < self.line_starts.items.items.len)
        self.line_starts.items.items[region_info.start_line_idx + 1]
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

/// Assert that nodes, regions and types are all in sync
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

/// Assert that nodes, regions and types are all in sync
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
/// This function asserts that the types array and the nodes are in sync.
pub fn addDefAndTypeVar(self: *Self, expr: Def, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("self", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeHeaderAndTypeVar(self: *Self, expr: TypeHeader, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeHeader.Idx {
    const expr_idx = try self.store.addTypeHeader(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeHeaderAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addStatementAndTypeVar(self: *Self, expr: Statement, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Statement.Idx {
    const expr_idx = try self.store.addStatement(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addStatementAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addPatternAndTypeVar(self: *Self, expr: Pattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addExprAndTypeVar(self: *Self, expr: Expr, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExprAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addRecordFieldAndTypeVar(self: *Self, expr: RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addRecordDestructAndTypeVar(self: *Self, expr: Pattern.RecordDestruct, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordDestructorAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addIfBranchAndTypeVar(self: *Self, expr: Expr.IfBranch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addIfBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMatchBranchAndTypeVar(self: *Self, expr: Expr.Match.Branch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addWhereClauseAndTypeVar(self: *Self, expr: WhereClause, content: types_mod.Content, region: Region) std.mem.Allocator.Error!WhereClause.Idx {
    const expr_idx = try self.store.addWhereClause(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addWhereClauseAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeAnnoAndTypeVar(self: *Self, expr: TypeAnno, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeAnnoAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnotationAndTypeVar(self: *Self, expr: Annotation, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVar(self: *Self, expr: TypeAnno.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addExposedItemAndTypeVar(self: *Self, expr: ExposedItem, content: types_mod.Content, region: Region) std.mem.Allocator.Error!ExposedItem.Idx {
    const expr_idx = try self.store.addExposedItem(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExposedItemAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addDiagnosticAndTypeVar(self: *Self, reason: Diagnostic, content: types_mod.Content) std.mem.Allocator.Error!Diagnostic.Idx {
    const expr_idx = try self.store.addDiagnostic(reason);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addDiagnosticAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMalformedAndTypeVar(self: *Self, diagnostic_idx: Diagnostic.Idx, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Node.Idx {
    const malformed_idx = try self.store.addMalformed(diagnostic_idx, region);
    const malformed_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMalformedAndTypeVar", malformed_idx, malformed_var);
    self.debugAssertArraysInSync();
    return malformed_idx;
}

/// Add a new match branch pattern and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMatchBranchPatternAndTypeVar(self: *Self, expr: Expr.Match.BranchPattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern record field and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addPatternRecordFieldAndTypeVar(self: *Self, expr: PatternRecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!PatternRecordField.Idx {
    const expr_idx = try self.store.addPatternRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
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
/// This function asserts that the types array and the nodes are in sync.
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

/// Adds an external declaration and returns its index
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

/// Helper function to generate the S-expression node for the entire module.
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

/// Append region information to an S-expression node for a given index.
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

/// Append region information to an S-expression node for a given index.
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

/// Get region information for a node.
pub fn getNodeRegionInfo(self: *const Self, idx: anytype) RegionInfo {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    return self.getRegionInfo(region);
}

/// Helper function to convert type information to an SExpr node
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
