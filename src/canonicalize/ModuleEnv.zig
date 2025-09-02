//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const builtin = @import("builtin");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");

const CIR = @import("CIR.zig");
const reporting = @import("reporting");
const parse = @import("parse");

const TypeWriter = types_mod.TypeWriter;
const CompactWriter = collections.CompactWriter;
const CommonEnv = base.CommonEnv;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;
const TypeStore = types_mod.Store;

const Self = @This();

gpa: std.mem.Allocator,

common: CommonEnv,
types: TypeStore,

// ===== Module compilation fields =====
// NOTE: These fields are populated during canonicalization and preserved for later use

/// Reference to the CIR that canonicalized this module
cir: ?*CIR,
/// Imported modules
imported_modules: std.ArrayListUnmanaged(CIR.ImportedModule),
/// Import lookup table for tests
imports: std.HashMap([]const u8, void, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,
/// Diagnostics collected during canonicalization (optional)
diagnostics: std.ArrayListUnmanaged(CIR.CanDiagnostic),
/// Reference to the AST (the new CIR mutates AST nodes in place)
ast: ?*parse.AST,
/// Store interface for backward compatibility
store: Store,
/// All top-level definitions in this module
all_defs: DefSpan = .{ .start = 0, .len = 0 },
/// All statements in this module
all_statements: StatementSpan = .{ .start = 0, .len = 0 },

pub const Store = struct {
    regions: Region.List,
    scratch_defs: base.Scratch(u32),
    scratch_statements: base.Scratch(u32),
    type_headers: std.ArrayListUnmanaged(CIR.TypeHeader),
    nodes: std.ArrayListUnmanaged(u32),
    diagnostics: std.ArrayListUnmanaged(Diagnostic),

    const Diagnostic = struct {
        message: []const u8,
        severity: Severity,
        region: Region,

        const Severity = enum {
            @"error",
            warning,
            info,
        };
    };

    pub fn init(allocator: std.mem.Allocator) Store {
        return Store{
            .regions = Region.List{},
            .scratch_defs = base.Scratch(u32).init(allocator) catch base.Scratch(u32){ .items = .{} },
            .scratch_statements = base.Scratch(u32).init(allocator) catch base.Scratch(u32){ .items = .{} },
            .type_headers = .{},
            .nodes = .{},
            .diagnostics = .{},
        };
    }

    pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
        self.regions.deinit(allocator);
        self.scratch_defs.deinit(allocator);
        self.scratch_statements.deinit(allocator);
        self.type_headers.deinit(allocator);
        self.nodes.deinit(allocator);
        self.diagnostics.deinit(allocator);
    }

    pub fn scratchDefTop(self: *Store) u32 {
        return self.scratch_defs.top();
    }

    pub fn defSpanFrom(self: *Store, start: u32) std.mem.Allocator.Error!DefSpan {
        const end = self.scratch_defs.top();
        return DefSpan{ .start = start, .len = end - start };
    }

    pub fn statementSpanFrom(self: *Store, start: u32) std.mem.Allocator.Error!StatementSpan {
        const end = self.scratch_statements.top();
        return StatementSpan{ .start = start, .len = end - start };
    }

    pub fn getTypeHeader(self: Store, idx: CIR.TypeHeader.Idx) CIR.TypeHeader {
        const index = @intFromEnum(idx);
        if (index < self.type_headers.items.len) {
            return self.type_headers.items[index];
        } else {
            // Return default header if index is out of bounds
            return CIR.TypeHeader{
                .name = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
                .args = @enumFromInt(0),
            };
        }
    }

    pub fn addTypeHeader(self: *Store, allocator: std.mem.Allocator, header: CIR.TypeHeader) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
        try self.type_headers.append(allocator, header);
        return @enumFromInt(self.type_headers.items.len - 1);
    }

    pub fn addDiagnostic(self: *Store, allocator: std.mem.Allocator, diagnostic: anytype) std.mem.Allocator.Error!u32 {
        // Handle both old-style and CanDiagnostic
        const message = if (@hasField(@TypeOf(diagnostic), "message"))
            diagnostic.message orelse "Unknown diagnostic"
        else if (@hasField(@TypeOf(diagnostic), "tag"))
            "Diagnostic" // Simple message for CanDiagnostic
        else
            "Unknown diagnostic";

        const severity = if (@hasField(@TypeOf(diagnostic), "severity"))
            if (diagnostic.severity) |s| switch (s) {
                .@"error" => Diagnostic.Severity.@"error",
                .warning => .warning,
                .info => .info,
            } else Diagnostic.Severity.@"error"
        else
            Diagnostic.Severity.@"error";

        const region = if (@hasField(@TypeOf(diagnostic), "region"))
            diagnostic.region
        else
            Region{ .start = .{ .line = 0, .column = 0, .offset = 0 }, .end = .{ .line = 0, .column = 0, .offset = 0 } };

        const diag = Diagnostic{
            .message = message,
            .severity = severity,
            .region = region,
        };

        try self.diagnostics.append(allocator, diag);
        return @intCast(self.diagnostics.items.len - 1);
    }

    pub fn sliceDefs(self: *const Store, span: DefSpan) []const u32 {
        const start = span.start;
        const end = start + span.len;
        if (end <= self.nodes.items.len) {
            return self.nodes.items[start..end];
        } else {
            return &[_]u32{};
        }
    }

    pub fn getDef(self: *const Store, idx: u32) DefData {
        _ = self;
        return DefData{ .pattern = idx, .expr = idx };
    }

    pub fn getPattern(self: *const Store, idx: u32) PatternData {
        _ = self;
        return PatternData{ .assign = .{ .ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = @intCast(idx) } } };
    }

    const DefData = struct {
        pattern: u32,
        expr: u32,
    };

    const PatternData = union(enum) {
        assign: struct { ident: Ident.Idx },
        identifier: struct { data: u32 },
        literal: struct { data: u32 },
    };
};

pub const DefSpan = struct {
    start: u32,
    len: u32,
};

pub const StatementSpan = struct {
    start: u32,
    len: u32,
};

/// Initialize the compilation fields in an existing ModuleEnv
pub fn initCIRFields(self: *Self, gpa: std.mem.Allocator, module_name: []const u8) !void {
    _ = gpa; // unused since we don't create new allocations
    self.cir = null;
    // Note: imported_modules already exists from ModuleEnv.init(), so we don't create a new one
    self.module_name = module_name;
    self.diagnostics = .{};
    self.ast = null;
}

/// Alias for initCIRFields for backwards compatibility with tests
pub fn initModuleEnvFields(self: *Self, gpa: std.mem.Allocator, module_name: []const u8) !void {
    return self.initCIRFields(gpa, module_name);
}

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
        .gpa = gpa,
        .common = try CommonEnv.init(gpa, source),
        .types = try TypeStore.initCapacity(gpa, 2048, 512),
        .cir = null,
        .imported_modules = .{},
        .imports = std.HashMap([]const u8, void, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(gpa),
        .module_name = "", // Will be set later during canonicalization
        .diagnostics = .{},
        .ast = null, // Will be set when canonicalizing
        .store = Store.init(gpa),
        .all_defs = .{ .start = 0, .len = 0 },
        .all_statements = .{ .start = 0, .len = 0 },
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.common.deinit(self.gpa);
    self.types.deinit();
    for (self.imported_modules.items) |*module| {
        module.deinit(self.gpa);
    }
    self.imported_modules.deinit(self.gpa);
    self.imports.deinit();
    self.diagnostics.deinit(self.gpa);
    self.store.deinit(self.gpa);
    // ast and cir are not owned by ModuleEnv, don't deinit them
}

/// Freeze all interners in this module environment, preventing any new entries from being added.
/// This should be called after canonicalization is complete, so that
/// we know it's safe to serialize/deserialize the part of the interner
/// that goes from ident to string, because we don't go from string to ident
/// (or add new entries) in any of the later stages of compilation.
pub fn freezeInterners(self: *Self) void {
    self.common.freezeInterners();
}

// ===== Module compilation functionality =====

/// Records a diagnostic error during canonicalization without blocking compilation.
pub fn pushDiagnostic(self: *Self, reason: CIR.CanDiagnostic) std.mem.Allocator.Error!void {
    try self.diagnostics.append(self.gpa, reason);
}

/// Creates a malformed node that represents a runtime error in the IR.
pub fn pushMalformed(self: *Self, comptime RetIdx: type, reason: CIR.CanDiagnostic) std.mem.Allocator.Error!RetIdx {
    try self.diagnostics.append(self.gpa, reason);
    // Return a dummy index for now since the new CIR doesn't have malformed nodes
    return @enumFromInt(0);
}

/// Extract the region from any diagnostic variant
fn getDiagnosticRegion(diagnostic: CIR.CanDiagnostic) Region {
    return diagnostic.region;
}

// ===== Module compilation functions =====

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]CIR.CanDiagnostic {
    return self.diagnostics.items;
}

/// Compilation error report type for user-friendly error messages
pub const Report = reporting.Report;

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: CIR.CanDiagnostic, allocator: std.mem.Allocator, filename: []const u8) !Report {
    const region_info = self.calcRegionInfo(diagnostic.region);

    const title = switch (diagnostic.tag) {
        .pattern_in_expr_context => "PATTERN IN EXPRESSION CONTEXT",
        .expr_in_pattern_context => "EXPRESSION IN PATTERN CONTEXT",
        .stmt_in_expr_context => "STATEMENT IN EXPRESSION CONTEXT",
        .expr_in_stmt_context => "EXPRESSION IN STATEMENT CONTEXT",
        .type_in_expr_context => "TYPE IN EXPRESSION CONTEXT",
        .expr_in_type_context => "EXPRESSION IN TYPE CONTEXT",
        .ident_not_in_scope => "UNDEFINED VARIABLE",
        .ident_already_defined => "DUPLICATE DEFINITION",
        .unused_variable => "UNUSED VARIABLE",
        .type_not_in_scope => "UNDEFINED TYPE",
        .invalid_type_var_in_constraint => "INVALID TYPE CONSTRAINT",
        .invalid_ability_in_constraint => "INVALID ABILITY CONSTRAINT",
        .invalid_where_constraint => "INVALID WHERE CLAUSE",
        .exposed_but_not_implemented => "EXPOSED BUT NOT IMPLEMENTED",
        .redundant_exposed => "REDUNDANT EXPOSED",
        .shadowing_warning => "SHADOWING",
        .unsupported_node => "UNSUPPORTED FEATURE",
        .malformed_ast => "MALFORMED SYNTAX",
    };

    const severity: reporting.Severity = switch (diagnostic.tag) {
        .unused_variable, .redundant_exposed, .shadowing_warning => .warning,
        else => .runtime_error,
    };

    var report = Report.init(allocator, title, severity);

    // Add appropriate message based on the diagnostic
    switch (diagnostic.tag) {
        .pattern_in_expr_context => {
            try report.document.addReflowingText("I found a pattern where I was expecting an expression.");
        },
        .expr_in_pattern_context => {
            try report.document.addReflowingText("I found an expression where I was expecting a pattern.");
        },
        .stmt_in_expr_context => {
            try report.document.addReflowingText("I found a statement where I was expecting an expression.");
        },
        .expr_in_stmt_context => {
            try report.document.addReflowingText("I found an expression where I was expecting a statement. Did you forget a semicolon?");
        },
        .type_in_expr_context => {
            try report.document.addReflowingText("I found a type annotation where I was expecting an expression.");
        },
        .expr_in_type_context => {
            try report.document.addReflowingText("I found an expression where I was expecting a type annotation.");
        },
        .ident_not_in_scope => {
            try report.document.addReflowingText("Nothing is named with this identifier in this scope.");
        },
        .ident_already_defined => {
            try report.document.addReflowingText("This identifier is already defined in this scope.");
        },
        .unused_variable => {
            try report.document.addReflowingText("This variable is defined but never used.");
        },
        .type_not_in_scope => {
            try report.document.addReflowingText("This type is not defined in this scope.");
        },
        .invalid_type_var_in_constraint => {
            try report.document.addReflowingText("Invalid type variable in where clause constraint.");
        },
        .invalid_ability_in_constraint => {
            try report.document.addReflowingText("Invalid ability reference in where clause constraint.");
        },
        .invalid_where_constraint => {
            try report.document.addReflowingText("Invalid where clause constraint syntax.");
        },
        .exposed_but_not_implemented => {
            try report.document.addReflowingText("This value is exposed in the module header but not defined in the module.");
        },
        .redundant_exposed => {
            try report.document.addReflowingText("This value is exposed multiple times in the module header.");
        },
        .shadowing_warning => {
            try report.document.addReflowingText("This definition shadows an existing one.");
        },
        .unsupported_node => {
            try report.document.addReflowingText("This language feature is not yet supported.");
        },
        .malformed_ast => {
            try report.document.addReflowingText("The syntax tree was malformed at this location.");
        },
    }

    try report.document.addLineBreak();
    try report.document.addLineBreak();

    const owned_filename = try report.addOwnedString(filename);
    try report.document.addSourceRegion(
        region_info,
        if (severity == .warning) .warning_highlight else .error_highlight,
        owned_filename,
        self.getSourceAll(),
        self.getLineStartsAll(),
    );

    return report;
}

/// Get region info for a given region
/// Get region info for a given region
pub fn getRegionInfo(self: *const Self, region: Region) !RegionInfo {
    return self.common.getRegionInfo(region);
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn calcRegionInfo(self: *const Self, region: Region) RegionInfo {
    return self.common.calcRegionInfo(region);
}

/// Extract a literal from source code between given byte offsets
pub fn literal_from_source(self: *const Self, start_offset: u32, end_offset: u32) []const u8 {
    return self.common.source[start_offset..end_offset];
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const Self, region: Region) ![]const u8 {
    return self.common.getSourceLine(region);
}

/// Serialize this ModuleEnv to the given CompactWriter.
/// IMPORTANT: The returned pointer points to memory inside the writer!
/// Attempting to dereference this pointer or calling any methods on it
/// is illegal behavior!
pub fn serialize(
    self: *const Self,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const Self {
    // First, write the ModuleEnv struct itself
    const offset_self = try writer.appendAlloc(allocator, Self);

    // Then serialize the sub-structures and update the struct
    offset_self.* = .{
        .gpa = undefined, // Will be set when deserializing
        .common = (try self.common.serialize(allocator, writer)).*,
        .types = (try self.types.serialize(allocator, writer)).*,
        .cir = null, // Will be set when deserializing
        .imported_modules = self.imported_modules,
        .imports = std.HashMap([]const u8, void, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
        .module_name = "", // Will be set when deserializing
        .diagnostics = self.diagnostics,
        .ast = null, // Will be set when deserializing
        .store = .{
            .regions = .{},
            .scratch_defs = try base.Scratch(u32).init(allocator),
            .scratch_statements = try base.Scratch(u32).init(allocator),
            .type_headers = .{},
            .nodes = .{},
            .diagnostics = .{},
        },
        .all_defs = self.all_defs,
        .all_statements = self.all_statements,
    };

    // set gpa to all zeros, so that what we write to the file is deterministic
    @memset(@as([*]u8, @ptrCast(&offset_self.gpa))[0..@sizeOf(@TypeOf(offset_self.gpa))], 0);

    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
/// IMPORTANT: The gpa, source, and module_name fields must be manually set before calling this function.
pub fn relocate(self: *Self, offset: isize) void {
    // IMPORTANT: gpa, and module_name are not relocated - they should be set manually before calling relocate

    // Relocate all sub-structures
    self.common.relocate(offset);
    self.types.relocate(offset);
    // cir, imported_modules, and ast are not relocated - they are runtime references
}

/// Serialized representation of ModuleEnv
pub const Serialized = struct {
    gpa: std.mem.Allocator, // Serialized as zeros, provided during deserialization
    common: CommonEnv.Serialized,
    types: TypeStore.Serialized,
    cir: ?*CIR, // Not serialized, provided during deserialization
    imported_modules: std.ArrayListUnmanaged(CIR.ImportedModule),
    module_name: []const u8, // Serialized as zeros, provided during deserialization
    diagnostics: std.ArrayListUnmanaged(CIR.CanDiagnostic),
    ast: ?*parse.AST, // Not serialized, provided during deserialization

    /// Serialize a ModuleEnv into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        env: *const Self,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) !void {
        // Set fields that will be provided during deserialization to zeros
        self.gpa = undefined; // Will be set to zeros below

        try self.common.serialize(&env.common, allocator, writer);
        try self.types.serialize(&env.types, allocator, writer);

        // Copy simple values directly
        self.cir = null;
        self.imported_modules = env.imported_modules;
        self.module_name = "";
        self.diagnostics = env.diagnostics;
        self.ast = null;

        // Set gpa to all zeros; the space needs to be here,
        // but the value will be set separately during deserialization.
        @memset(@as([*]u8, @ptrCast(&self.gpa))[0..@sizeOf(@TypeOf(self.gpa))], 0);
    }

    /// Deserialize a ModuleEnv from the buffer, updating the ModuleEnv in place
    pub fn deserialize(
        self: *Serialized,
        offset: i64,
        gpa: std.mem.Allocator,
        source: []const u8,
        module_name: []const u8,
    ) *Self {
        // ModuleEnv.Serialized should be at least as big as ModuleEnv
        std.debug.assert(@sizeOf(Serialized) >= @sizeOf(Self));

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
        const env = @as(*Self, @ptrFromInt(@intFromPtr(self)));

        env.* = Self{
            .gpa = gpa,
            .common = self.common.deserialize(offset, source).*,
            .types = self.types.deserialize(offset).*,
            .cir = null,
            .imported_modules = self.imported_modules,
            .imports = std.HashMap([]const u8, void, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(gpa),
            .module_name = module_name,
            .diagnostics = self.diagnostics,
            .ast = null,
            .store = Store.init(gpa),
            .all_defs = .{ .start = 0, .len = 0 },
            .all_statements = .{ .start = 0, .len = 0 },
        };

        return env;
    }
};

/// Convert a type into a type var
pub fn varFrom(idx: anytype) TypeVar {
    return @enumFromInt(@intFromEnum(idx));
}

/// Adds an identifier to the list of exposed items by its identifier index.
pub fn addExposedById(self: *Self, ident_idx: Ident.Idx) !void {
    return try self.common.exposed_items.addExposedById(self.gpa, @bitCast(ident_idx));
}

/// Associates a node index with an exposed identifier.
pub fn setExposedNodeIndexById(self: *Self, ident_idx: Ident.Idx, node_idx: u16) !void {
    return try self.common.exposed_items.setNodeIndexById(self.gpa, @bitCast(ident_idx), node_idx);
}

/// Retrieves the node index associated with an exposed identifier, if any.
pub fn getExposedNodeIndexById(self: *const Self, ident_idx: Ident.Idx) ?u16 {
    return self.common.getNodeIndexById(self.gpa, ident_idx);
}

/// Ensures that the exposed items are sorted by identifier index.
pub fn ensureExposedSorted(self: *Self, allocator: std.mem.Allocator) void {
    self.common.exposed_items.ensureSorted(allocator);
}

/// Checks whether the given identifier is exposed by this module.
pub fn containsExposedById(self: *const Self, ident_idx: Ident.Idx) bool {
    return self.common.exposed_items.containsById(self.gpa, @bitCast(ident_idx));
}

/// Retrieves the text of an identifier by its index
pub fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.getIdent(idx);
}

/// Helper function to generate the S-expression node for the entire module.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (self.cir) |cir| {
        if (maybe_expr_idx) |expr_idx| {
            // Output just the single expression
            const expr = cir.getExpr(expr_idx);
            const node_begin = tree.beginNode();
            try tree.pushStaticAtom("expr");
            try tree.pushStringPair("tag", @tagName(expr.tag));
            try tree.endNode(node_begin, tree.beginNode());
        } else {
            // Output the entire module structure
            const root_begin = tree.beginNode();
            try tree.pushStaticAtom("cir-module");

            // Output statements
            const stmts = CIR.Stmts{ .cir = cir };
            if (stmts.len() > 0) {
                const stmts_begin = tree.beginNode();
                try tree.pushStaticAtom("statements");
                // try tree.pushIntPair("count", @intCast(stmts.len())); // Method doesn't exist in new SExprTree
                try tree.endNode(stmts_begin, tree.beginNode());
            }

            // Output expressions
            const exprs = CIR.Exprs{ .cir = cir };
            if (exprs.len() > 0) {
                const exprs_begin = tree.beginNode();
                try tree.pushStaticAtom("expressions");
                // try tree.pushIntPair("count", @intCast(exprs.len())); // Method doesn't exist
                try tree.endNode(exprs_begin, tree.beginNode());
            }

            try tree.endNode(root_begin, tree.beginNode());
        }
    }
}

/// Append region information to an S-expression node for a given index.
pub fn appendRegionInfoToSExprTree(self: *const Self, tree: *SExprTree, idx: anytype) std.mem.Allocator.Error!void {
    if (self.cir) |cir| {
        // Get the node from the AST
        const node_idx = @as(parse.AST.Node.Idx, @enumFromInt(@intFromEnum(idx)));
        const node = cir.ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
        const region = node.region;
        try self.appendRegionInfoToSExprTreeFromRegion(tree, region);
    }
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

/// Helper function to convert type information to an SExpr node
/// in S-expression format for snapshot testing.
pub fn pushTypesToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (maybe_expr_idx) |_| {
        // try self.pushExprTypesToSExprTree(expr_idx, tree); // Method doesn't exist - needs reimplementation for new CIR
    } else if (self.cir) |cir| {
        // Generate type information for the entire module
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("inferred-types");

        const root_attrs = tree.beginNode();

        // Output type information for all expressions
        const exprs = CIR.Exprs{ .cir = cir };
        if (exprs.len() > 0) {
            const exprs_begin = tree.beginNode();
            try tree.pushStaticAtom("expressions");

            // Create a TypeWriter to format types
            var type_writer = try self.initTypeWriter();
            defer type_writer.deinit();

            const exprs_attrs = tree.beginNode();
            // try tree.pushIntPair("count", @intCast(exprs.len())); // Method doesn't exist

            try tree.endNode(exprs_begin, exprs_attrs);
        }

        try tree.endNode(root_begin, root_attrs);
    }
}

/// Retrieves a string literal by its index from the common environment.
pub fn getString(self: *const Self, idx: StringLiteral.Idx) []const u8 {
    return self.common.getString(idx);
}

/// Inserts a string literal into the common environment and returns its index.
pub fn insertString(self: *Self, string: []const u8) std.mem.Allocator.Error!StringLiteral.Idx {
    return try self.common.insertString(self.gpa, string);
}

/// Returns a mutable reference to the identifier store.
pub fn getIdentStore(self: *Self) *Ident.Store {
    return &self.common.idents;
}

/// Retrieves the text of an identifier by its index.
pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.common.getIdent(idx);
}

/// Get the source text for a given region
pub fn getSource(self: *const Self, region: Region) []const u8 {
    return self.common.getSource(region);
}

/// TODO this is a code smell... we should track down the places using this
/// and replace with something more sensible -- need to refactor diagnostics a little.
pub fn getSourceAll(self: *const Self) []const u8 {
    return self.common.getSourceAll();
}

/// TODO this is a code smell... we should track down the places using this
/// and replace with something more sensible -- need to refactor diagnostics a little.
pub fn getLineStartsAll(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}

/// Initialize a TypeWriter with an immutable ModuleEnv reference.
pub fn initTypeWriter(self: *Self) std.mem.Allocator.Error!TypeWriter {
    return TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore());
}

/// Inserts an identifier into the common environment and returns its index.
pub fn insertIdent(self: *Self, ident: Ident) std.mem.Allocator.Error!Ident.Idx {
    return try self.common.insertIdent(self.gpa, ident);
}

/// Returns the line start positions for source code position mapping.
/// Each element represents the byte offset where a new line begins.
pub fn getLineStarts(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}
