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

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const CIR = @import("CIR.zig");

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

/// All the definitions in the module (populated by canonicalization)
all_defs: CIR.Def.Span,
/// All the top-level statements in the module (populated by canonicalization)
all_statements: CIR.Statement.Span,
/// Definitions that are exported by this module (populated by canonicalization)
exports: CIR.Def.Span,
/// All external declarations referenced in this module
external_decls: CIR.ExternalDecl.SafeList,
/// Store for interned module imports
imports: CIR.Import.Store,
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,
/// Diagnostics collected during canonicalization (optional)
diagnostics: CIR.Diagnostic.Span,
/// Stores the raw nodes which represent the intermediate representation
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,

/// Initialize the compilation fields in an existing ModuleEnv
pub fn initCIRFields(self: *Self, gpa: std.mem.Allocator, module_name: []const u8) !void {
    _ = gpa; // unused since we don't create new allocations
    self.all_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.all_statements = .{ .span = .{ .start = 0, .len = 0 } };
    self.exports = .{ .span = .{ .start = 0, .len = 0 } };
    // Note: external_decls already exists from ModuleEnv.init(), so we don't create a new one
    self.imports = CIR.Import.Store.init();
    self.module_name = module_name;
    self.diagnostics = CIR.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
    // Note: self.store already exists from ModuleEnv.init(), so we don't create a new one
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
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .exports = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = try CIR.ExternalDecl.SafeList.initCapacity(gpa, 16),
        .imports = CIR.Import.Store.init(),
        .module_name = "", // Will be set later during canonicalization
        .diagnostics = CIR.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } },
        .store = try NodeStore.initCapacity(gpa, 10_000), // Default node store capacity
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.common.deinit(self.gpa);
    self.types.deinit();
    self.external_decls.deinit(self.gpa);
    self.imports.deinit(self.gpa);
    // diagnostics are stored in the NodeStore, no need to free separately
    self.store.deinit();
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
pub fn pushDiagnostic(self: *Self, reason: CIR.Diagnostic) std.mem.Allocator.Error!void {
    _ = try self.addDiagnosticAndTypeVar(reason, .err);
}

/// Creates a malformed node that represents a runtime error in the IR.
pub fn pushMalformed(self: *Self, comptime RetIdx: type, reason: CIR.Diagnostic) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const diag_idx = try self.addDiagnosticAndTypeVar(reason, .err);
    const region = getDiagnosticRegion(reason);
    const malformed_idx = try self.addMalformedAndTypeVar(diag_idx, .err, region);
    return castIdx(Node.Idx, RetIdx, malformed_idx);
}

/// Extract the region from any diagnostic variant
fn getDiagnosticRegion(diagnostic: CIR.Diagnostic) Region {
    return switch (diagnostic) {
        .type_redeclared => |data| data.redeclared_region,
        .type_alias_redeclared => |data| data.redeclared_region,
        .nominal_type_redeclared => |data| data.redeclared_region,
        .duplicate_record_field => |data| data.duplicate_region,
        inline else => |data| data.region,
    };
}

/// Import helper functions from CIR
const isCastable = CIR.isCastable;
/// Cast function for safely converting between compatible index types
pub const castIdx = CIR.castIdx;

// ===== Module compilation functions =====

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]CIR.Diagnostic {
    // Get all diagnostics from the store, not just the ones in self.diagnostics span
    const all_diagnostics = try self.store.diagnosticSpanFrom(0);
    const diagnostic_indices = self.store.sliceDiagnostics(all_diagnostics);
    const diagnostics = try self.gpa.alloc(CIR.Diagnostic, diagnostic_indices.len);
    for (diagnostic_indices, 0..) |diagnostic_idx, i| {
        diagnostics[i] = self.store.getDiagnostic(diagnostic_idx);
    }
    return diagnostics;
}

/// Compilation error report type for user-friendly error messages
pub const Report = CIR.Report;

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: CIR.Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !Report {
    return switch (diagnostic) {
        .invalid_num_literal => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            // Extract the literal text from the source
            const literal_text = self.getSource(data.region);

            var report = Report.init(allocator, "INVALID NUMBER", .runtime_error);
            const owned_literal = try report.addOwnedString(literal_text);

            try report.document.addReflowingText("This number literal is not valid: ");
            try report.document.addInlineCode(owned_literal);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("Check that the number is correctly formatted. Valid examples include: ");
            try report.document.addInlineCode("42");
            try report.document.addReflowingText(", ");
            try report.document.addInlineCode("3.14");
            try report.document.addReflowingText(", ");
            try report.document.addInlineCode("0x1A");
            try report.document.addReflowingText(", or ");
            try report.document.addInlineCode("1_000_000");
            try report.document.addReflowingText(".");

            break :blk report;
        },
        .ident_not_in_scope => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = Report.init(allocator, "UNDEFINED VARIABLE", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.document.addReflowingText("Nothing is named ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" in this scope.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Is there an ");
            try report.document.addKeyword("import");
            try report.document.addReflowingText(" or ");
            try report.document.addKeyword("exposing");
            try report.document.addReflowingText(" missing up-top?");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .exposed_but_not_implemented => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "EXPOSED BUT NOT DEFINED", .runtime_error);

            const ident_name = self.getIdent(data.ident);
            const owned_ident = try report.addOwnedString(ident_name);

            try report.document.addReflowingText("The module header says that ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" is exposed, but it is not defined anywhere in this module.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.getSourceAll(), self.getLineStartsAll());

            try report.document.addReflowingText("You can fix this by either defining ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" in this module, or by removing it from the list of exposed values.");

            break :blk report;
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

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
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .underscore_in_type_declaration => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "UNDERSCORE IN TYPE ALIAS", .runtime_error);

            const kind = if (data.is_alias) "alias" else "opaque type";
            const message = try std.fmt.allocPrint(allocator, "Underscores are not allowed in type {s} declarations.", .{kind});
            defer allocator.free(message);
            const owned_message = try report.addOwnedString(message);
            try report.document.addReflowingText(owned_message);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.getSourceAll(), self.getLineStartsAll());

            try report.document.addLineBreak();
            const explanation = try std.fmt.allocPrint(allocator, "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.", .{});
            defer allocator.free(explanation);
            const owned_explanation = try report.addOwnedString(explanation);
            try report.document.addReflowingText(owned_explanation);

            break :blk report;
        },
        .undeclared_type => |data| blk: {
            const type_name = self.getIdent(data.name);
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
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_alias_but_needed_nominal => |data| blk: {
            const type_name = self.getIdent(data.name);
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "EXPECTED NOMINAL TYPE", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.document.addReflowingText("You are using the type ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" like a nominal type, but it is an alias.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("This type is referenced here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" You can declare this type with ");
            try report.document.addInlineCode(":=");
            try report.document.addReflowingText(" to make it nominal.");

            break :blk report;
        },
        .type_redeclared => |data| blk: {
            const type_name = self.getIdent(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);

            var report = Report.init(allocator, "TYPE REDECLARED", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.document.addReflowingText("The type ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" is being redeclared.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            // Show where the redeclaration is
            try report.document.addReflowingText("The redeclaration is here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                redeclared_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("But ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" was already declared here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .invalid_top_level_statement => |data| blk: {
            const stmt_name = self.getString(data.stmt);
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "INVALID STATEMENT", .runtime_error);
            const owned_stmt = try report.addOwnedString(stmt_name);
            try report.document.addReflowingText("The statement ");
            try report.document.addInlineCode(owned_stmt);
            try report.document.addReflowingText(" is not allowed at the top level.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Only definitions, type annotations, and imports are allowed at the top level.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .used_underscore_variable => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "UNDERSCORE VARIABLE USED", .warning);
            const owned_ident = try report.addOwnedString(ident_name);

            try report.document.addReflowingText("Variable ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" is prefixed with an underscore but is actually used.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Variables prefixed with ");
            try report.document.addUnqualifiedSymbol("_");
            try report.document.addReflowingText(" are intended to be unused. Remove the underscore prefix: ");

            // Create the suggested name without underscore
            const suggested_name = ident_name[1..]; // Remove first character (_)
            const owned_suggested = try report.addOwnedString(suggested_name);
            try report.document.addUnqualifiedSymbol(owned_suggested);
            try report.document.addReflowingText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .expr_not_canonicalized => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "UNRECOGNIZED SYNTAX", .runtime_error);
            try report.document.addReflowingText("I don't recognize this syntax.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("This might be a syntax error, an unsupported language feature, or a typo.");

            break :blk report;
        },
        .crash_expects_string => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "CRASH EXPECTS STRING", .runtime_error);
            try report.document.addReflowingText("The ");
            try report.document.addAnnotated("crash", .inline_code);
            try report.document.addReflowingText(" keyword expects a string literal as its argument.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("For example: ");
            try report.document.addAnnotated("crash \"Something went wrong\"", .inline_code);
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .duplicate_record_field => |data| blk: {
            const field_name = self.getIdent(data.field_name);
            const duplicate_region_info = self.calcRegionInfo(data.duplicate_region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            var report = Report.init(allocator, "DUPLICATE RECORD FIELD", .runtime_error);
            const owned_field_name = try report.addOwnedString(field_name);

            try report.document.addReflowingText("The record field ");
            try report.document.addRecordField(owned_field_name);
            try report.document.addReflowingText(" appears more than once in this record.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            // Show where the duplicate field is
            try report.document.addReflowingText("This field is duplicated here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                duplicate_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("The field ");
            try report.document.addRecordField(owned_field_name);
            try report.document.addReflowingText(" was first defined here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.");

            break :blk report;
        },
        .redundant_exposed => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            var report = Report.init(allocator, "REDUNDANT EXPOSED", .warning);
            const owned_ident = try report.addOwnedString(ident_name);

            try report.document.addReflowingText("The identifier ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" is exposed multiple times in the module header.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            // we don't need to display the original region info
            // as this header is in a single location
            _ = original_region_info;

            try report.document.addReflowingText("You can remove the duplicate entry to fix this warning.");

            break :blk report;
        },
        .undeclared_type_var => |data| blk: {
            const type_var_name = self.getIdent(data.name);
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "UNDECLARED TYPE VARIABLE", .runtime_error);
            const owned_type_var_name = try report.addOwnedString(type_var_name);
            try report.document.addReflowingText("The type variable ");
            try report.document.addType(owned_type_var_name);
            try report.document.addReflowingText(" is not declared in this scope.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Type variables must be introduced in a type annotation before they can be used.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("This type variable is referenced here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .not_implemented => |data| blk: {
            const feature = self.getString(data.feature);
            var report = Report.init(allocator, "NOT IMPLEMENTED", .fatal);
            const owned_feature = try report.addOwnedString(feature);
            try report.document.addReflowingText("This feature is not yet implemented: ");
            try report.document.addAnnotatedText(owned_feature, .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!");
            break :blk report;
        },
        .malformed_type_annotation => |data| blk: {
            var report = Report.init(allocator, "MALFORMED TYPE", .runtime_error);
            try report.document.addReflowingText("This type annotation is malformed or contains invalid syntax.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            const region_info = self.calcRegionInfo(data.region);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .if_condition_not_canonicalized => |_| blk: {
            var report = Report.init(allocator, "INVALID IF CONDITION", .runtime_error);
            try report.document.addReflowingText("The condition in this ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" expression could not be processed.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("The condition must be a valid expression that evaluates to a ");
            try report.document.addKeyword("Bool");
            try report.document.addReflowingText(" value (");
            try report.document.addKeyword("Bool.true");
            try report.document.addReflowingText(" or ");
            try report.document.addKeyword("Bool.false");
            try report.document.addReflowingText(").");
            break :blk report;
        },
        .if_else_not_canonicalized => |_| blk: {
            var report = Report.init(allocator, "INVALID IF BRANCH", .runtime_error);
            try report.document.addReflowingText("The ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" branch of this ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" expression could not be processed.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("The ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" branch must contain a valid expression. Check for syntax errors or missing values.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("Note: Every ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" expression in Roc must have an ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" branch, and both branches must have the same type.");
            break :blk report;
        },
        .pattern_not_canonicalized => |_| blk: {
            var report = Report.init(allocator, "INVALID PATTERN", .runtime_error);
            try report.document.addReflowingText("This pattern contains invalid syntax or uses unsupported features.");
            break :blk report;
        },
        .shadowing_warning => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            var report = Report.init(allocator, "DUPLICATE DEFINITION", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.document.addReflowingText("The name ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" is being redeclared in this scope.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            // Show where the new declaration is
            try report.document.addReflowingText("The redeclaration is here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                new_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("But ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" was already defined here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .empty_tuple => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "EMPTY TUPLE NOT ALLOWED", .runtime_error);
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addReflowingText("I am part way through parsing this tuple, but it is empty:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("If you want to represent nothing, try using an empty record: ");
            try report.document.addAnnotated("{}", .inline_code);
            try report.document.addReflowingText(".");

            break :blk report;
        },
        .lambda_body_not_canonicalized => |data| blk: {
            _ = data;

            var report = Report.init(allocator, "INVALID LAMBDA", .runtime_error);
            try report.document.addReflowingText("The body of this lambda expression is not valid.");

            break :blk report;
        },
        .malformed_where_clause => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "MALFORMED WHERE CLAUSE", .runtime_error);
            try report.document.addReflowingText("This where clause could not be parsed correctly.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("Check the syntax of your where clause.");

            break :blk report;
        },
        .var_across_function_boundary => |data| blk: {
            _ = data;

            var report = Report.init(allocator, "VAR REASSIGNMENT ERROR", .runtime_error);
            try report.document.addReflowingText("Cannot reassign a ");
            try report.document.addKeyword("var");
            try report.document.addReflowingText(" from outside the function where it was declared.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Variables declared with ");
            try report.document.addKeyword("var");
            try report.document.addReflowingText(" can only be reassigned within the same function scope.");

            break :blk report;
        },
        .tuple_elem_not_canonicalized => |data| blk: {
            _ = data;

            var report = Report.init(allocator, "INVALID TUPLE ELEMENT", .runtime_error);
            try report.document.addReflowingText("This tuple element is malformed or contains invalid syntax.");

            break :blk report;
        },
        .f64_pattern_literal => |data| blk: {
            // Extract the literal text from the source
            const literal_text = self.getSourceAll()[data.region.start.offset..data.region.end.offset];

            var report = Report.init(allocator, "F64 NOT ALLOWED IN PATTERN", .runtime_error);

            // Format the message to match origin/main
            try report.document.addText("This floating-point literal cannot be used in a pattern match: ");
            try report.document.addInlineCode(literal_text);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("This number exceeds the precision range of Roc's ");
            try report.document.addInlineCode("Dec");
            try report.document.addReflowingText(" type and would require F64 representation. ");
            try report.document.addReflowingText("Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Consider one of these alternatives:");
            try report.document.addLineBreak();
            try report.document.addText("• Use a guard condition with a range check");
            try report.document.addLineBreak();
            try report.document.addText("• Use a smaller number that fits in Dec's precision");
            try report.document.addLineBreak();
            try report.document.addText("• Restructure your code to avoid pattern matching on this value");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("For example, instead of:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("1e100 => ...");
            try report.document.addLineBreak();
            try report.document.addText("Use a guard:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("n if n > 1e99 => ...");

            break :blk report;
        },
        .type_not_exposed => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "TYPE NOT EXPOSED", .runtime_error);

            const type_name_bytes = self.getIdent(data.type_name);
            const type_name = try report.addOwnedString(type_name_bytes);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);

            // Format the message to match origin/main
            try report.document.addText("The type ");
            try report.document.addInlineCode(type_name);
            try report.document.addReflowingText(" is not an exposed by the module ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("You're attempting to use this type here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .module_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "MODULE NOT FOUND", .runtime_error);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);

            // Format the message to match origin/main
            try report.document.addText("The module ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(" was not found in this Roc project.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("You're attempting to use this module here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .module_not_imported => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "MODULE NOT IMPORTED", .runtime_error);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);

            // Format the message to match origin/main
            try report.document.addText("There is no module with the name ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(" imported into this Roc file.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("You're attempting to use this module here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .where_clause_not_allowed_in_type_decl => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = Report.init(allocator, "WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION", .warning);

            // Format the message to match origin/main
            try report.document.addText("You cannot define a ");
            try report.document.addInlineCode("where");
            try report.document.addReflowingText(" clause inside a type declaration.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("You're attempting do this here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
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
        },
    };
}

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
        .all_defs = self.all_defs,
        .all_statements = self.all_statements,
        .exports = self.exports,
        .external_decls = (try self.external_decls.serialize(allocator, writer)).*,
        .imports = (try self.imports.serialize(allocator, writer)).*,
        .module_name = "", // Will be set when deserializing
        .diagnostics = self.diagnostics,
        .store = (try self.store.serialize(allocator, writer)).*,
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

    // Note: all_defs and all_statements are just spans with numeric values, no pointers to relocate

    self.external_decls.relocate(offset);
    // self.imports is deserialized separately, so no need to relocate here

    // Note: module_name is not relocated - it should be set manually

    // Note: diagnostics is just a span with numeric values, no pointers to relocate

    self.store.relocate(offset);
}

/// Serialized representation of ModuleEnv
pub const Serialized = struct {
    gpa: std.mem.Allocator, // Serialized as zeros, provided during deserialization
    common: CommonEnv.Serialized,
    types: TypeStore.Serialized,
    all_defs: CIR.Def.Span,
    all_statements: CIR.Statement.Span,
    exports: CIR.Def.Span,
    external_decls: CIR.ExternalDecl.SafeList.Serialized,
    imports: CIR.Import.Store.Serialized,
    module_name: []const u8, // Serialized as zeros, provided during deserialization
    diagnostics: CIR.Diagnostic.Span,
    store: NodeStore.Serialized,

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
        self.all_defs = env.all_defs;
        self.all_statements = env.all_statements;
        self.exports = env.exports;

        try self.external_decls.serialize(&env.external_decls, allocator, writer);
        try self.imports.serialize(&env.imports, allocator, writer);

        self.diagnostics = env.diagnostics;

        // Serialize NodeStore
        try self.store.serialize(&env.store, allocator, writer);

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
            .all_defs = self.all_defs,
            .all_statements = self.all_statements,
            .exports = self.exports,
            .external_decls = self.external_decls.deserialize(offset).*,
            .imports = self.imports.deserialize(offset, gpa).*,
            .module_name = module_name,
            .diagnostics = self.diagnostics,
            .store = self.store.deserialize(offset, gpa).*,
        };

        return env;
    }
};

/// Convert a type into a node index
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

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

/// Assert that nodes, regions and types are all in sync
pub inline fn debugAssertArraysInSync(self: *const Self) void {
    if (builtin.mode == .Debug) {
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
    if (builtin.mode == .Debug) {
        const idx1_int = @intFromEnum(idx1);
        const idx2_int = @intFromEnum(idx2);

        if (idx1_int != idx2_int) {
            std.debug.panic(
                "{s} idxs out of sync: {} != {}\n",
                .{ desc, idx1_int, idx2_int },
            );
        }
    }
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addDefAndTypeVar(self: *Self, expr: CIR.Def, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("self", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeHeaderAndTypeVar(self: *Self, expr: CIR.TypeHeader, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const expr_idx = try self.store.addTypeHeader(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeHeaderAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addStatementAndTypeVar(self: *Self, expr: CIR.Statement, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Statement.Idx {
    const expr_idx = try self.store.addStatement(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addStatementAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addPatternAndTypeVar(self: *Self, expr: CIR.Pattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addPatternAndTypeVarRedirect(self: *Self, expr: CIR.Pattern, redirect_to: TypeVar, region: Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    const expr_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addExprAndTypeVar(self: *Self, expr: CIR.Expr, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExprAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addExprAndTypeVarRedirect(self: *Self, expr: CIR.Expr, redirect_to: TypeVar, region: Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    const expr_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addExprAndTypeVarRedirect", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new capture and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addCaptureAndTypeVar(self: *Self, capture: CIR.Expr.Capture, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Expr.Capture.Idx {
    const capture_idx = try self.store.addCapture(capture, region);
    const capture_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addCaptureAndTypeVar", capture_idx, capture_var);
    self.debugAssertArraysInSync();
    return capture_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addRecordFieldAndTypeVar(self: *Self, expr: CIR.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addRecordDestructAndTypeVar(self: *Self, expr: CIR.Pattern.RecordDestruct, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordDestructorAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addIfBranchAndTypeVar(self: *Self, expr: CIR.Expr.IfBranch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addIfBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMatchBranchAndTypeVar(self: *Self, expr: CIR.Expr.Match.Branch, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addWhereClauseAndTypeVar(self: *Self, expr: CIR.WhereClause, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.WhereClause.Idx {
    const expr_idx = try self.store.addWhereClause(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addWhereClauseAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeAnnoAndTypeVar(self: *Self, expr: CIR.TypeAnno, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeAnnoAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeAnnoAndTypeVarRedirect(self: *Self, expr: CIR.TypeAnno, redirect_to: TypeVar, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    const expr_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addTypeAnnoAndTypeVarRedirect", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnotationAndTypeVar(self: *Self, expr: CIR.Annotation, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnotationAndTypeVarRedirect(self: *Self, expr: CIR.Annotation, redirect_to: TypeVar, region: Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVar(self: *Self, expr: CIR.TypeAnno.RecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVarRedirect(self: *Self, expr: CIR.TypeAnno.RecordField, redirect_to: TypeVar, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addExposedItemAndTypeVar(self: *Self, expr: CIR.ExposedItem, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.ExposedItem.Idx {
    const expr_idx = try self.store.addExposedItem(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addExposedItemAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a diagnostic without creating a corresponding type variable.
pub fn addDiagnostic(self: *Self, reason: CIR.Diagnostic) std.mem.Allocator.Error!CIR.Diagnostic.Idx {
    return self.store.addDiagnostic(reason);
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addDiagnosticAndTypeVar(self: *Self, reason: CIR.Diagnostic, content: types_mod.Content) std.mem.Allocator.Error!CIR.Diagnostic.Idx {
    const expr_idx = try self.store.addDiagnostic(reason);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addDiagnosticAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMalformedAndTypeVar(self: *Self, diagnostic_idx: CIR.Diagnostic.Idx, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Node.Idx {
    const malformed_idx = try self.store.addMalformed(diagnostic_idx, region);
    const malformed_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMalformedAndTypeVar", malformed_idx, malformed_var);
    self.debugAssertArraysInSync();
    return malformed_idx;
}

/// Add a new match branch pattern and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addMatchBranchPatternAndTypeVar(self: *Self, expr: CIR.Expr.Match.BranchPattern, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern record field and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addPatternRecordFieldAndTypeVar(self: *Self, expr: CIR.PatternRecordField, content: types_mod.Content, region: Region) std.mem.Allocator.Error!CIR.PatternRecordField.Idx {
    _ = region;
    const expr_idx = try self.store.addPatternRecordField(expr);
    const expr_var = try self.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the nodes are in sync.
pub fn addTypeSlotAndTypeVar(
    self: *Self,
    parent_node: CIR.Node.Idx,
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
    parent_node: CIR.Node.Idx,
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
pub fn pushExternalDecl(self: *Self, decl: CIR.ExternalDecl) std.mem.Allocator.Error!CIR.ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = try self.external_decls.append(self.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const Self, idx: CIR.ExternalDecl.Idx) *const CIR.ExternalDecl {
    return self.external_decls.get(@as(CIR.ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *Self, decls: []const CIR.ExternalDecl) std.mem.Allocator.Error!CIR.ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = try self.external_decls.append(self.gpa, decl);
    }
    return CIR.ExternalDecl.Span{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const Self, span: CIR.ExternalDecl.Span) []const CIR.ExternalDecl {
    const range = CIR.ExternalDecl.SafeList.Range{ .start = @enumFromInt(span.span.start), .count = span.span.len };
    return self.external_decls.sliceRange(range);
}

/// Retrieves the text of an identifier by its index
pub fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.getIdent(idx);
}

/// Helper to format pattern index for s-expr output
fn formatPatternIdxNode(gpa: std.mem.Allocator, pattern_idx: CIR.Pattern.Idx) SExpr {
    var node = SExpr.init(gpa, "pid");
    node.appendUnsignedInt(gpa, @intFromEnum(pattern_idx));
    return node;
}

/// Helper function to generate the S-expression node for the entire module.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
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

        for (0..@intCast(self.external_decls.len())) |i| {
            const external_decl = self.external_decls.get(@enumFromInt(i));
            try external_decl.pushToSExprTree(self, tree);
        }

        try tree.endNode(root_begin, attrs);
    }
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
pub fn pushTypesToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (maybe_expr_idx) |expr_idx| {
        try self.pushExprTypesToSExprTree(expr_idx, tree);
    } else {
        // Generate full type information for all definitions and expressions
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("inferred-types");

        const root_attrs = tree.beginNode();

        // Create defs section
        const defs_begin = tree.beginNode();
        try tree.pushStaticAtom("defs");
        const defs_attrs = tree.beginNode();

        // Iterate through all definitions to extract pattern types
        const defs_slice = self.store.sliceDefs(self.all_defs);
        for (defs_slice) |def_idx| {
            const def = self.store.getDef(def_idx);

            // Only process assign patterns - skip destructuring patterns
            const pattern = self.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => {},
                else => continue, // Skip non-assign patterns (like destructuring)
            }

            const pattern_var = varFrom(def.pattern);

            // Get the region for this definition
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.pattern));
            const pattern_region = self.store.getRegionAt(pattern_node_idx);

            // Create a TypeWriter to format the type
            var type_writer = self.initTypeWriter() catch continue;
            defer type_writer.deinit();

            // Write the type to the buffer
            type_writer.write(pattern_var) catch continue;

            // Add the pattern type entry
            const patt_begin = tree.beginNode();
            try tree.pushStaticAtom("patt");
            try self.appendRegionInfoToSExprTreeFromRegion(tree, pattern_region);

            const type_str = type_writer.get();
            try tree.pushStringPair("type", type_str);

            try tree.endNode(patt_begin, tree.beginNode());
        }

        try tree.endNode(defs_begin, defs_attrs);

        // Check if we have any type declarations to output
        const all_stmts = self.store.sliceStatements(self.all_statements);
        var has_type_decl = false;
        for (all_stmts) |stmt_idx| {
            const stmt = self.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_alias_decl, .s_nominal_decl => {
                    has_type_decl = true;
                    break;
                },
                else => continue,
            }
        }

        // Create type_decls section if we have any type declarations
        if (has_type_decl) {
            const type_decls_begin = tree.beginNode();
            try tree.pushStaticAtom("type_decls");
            const type_decls_attrs = tree.beginNode();

            for (all_stmts) |stmt_idx| {
                const stmt = self.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_alias_decl => |alias| {
                        const stmt_begin = tree.beginNode();
                        try tree.pushStaticAtom("alias");

                        // Add region info for the statement
                        const stmt_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, stmt_region);

                        // Get the type variable for this statement
                        const stmt_var = varFrom(stmt_idx);

                        // Create a TypeWriter to format the type
                        var type_writer = self.initTypeWriter() catch continue;
                        defer type_writer.deinit();

                        // Write the type to the buffer
                        type_writer.write(stmt_var) catch continue;

                        const type_str = type_writer.get();
                        try tree.pushStringPair("type", type_str);

                        const stmt_attrs = tree.beginNode();

                        // Add the type header
                        const header = self.store.getTypeHeader(alias.header);
                        try header.pushToSExprTree(self, tree, alias.header);

                        try tree.endNode(stmt_begin, stmt_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        const stmt_begin = tree.beginNode();
                        try tree.pushStaticAtom("nominal");

                        // Add region info for the statement
                        const stmt_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, stmt_region);

                        // Get the type variable for this statement
                        const stmt_var = varFrom(stmt_idx);

                        // Create a TypeWriter to format the type
                        var type_writer = self.initTypeWriter() catch continue;
                        defer type_writer.deinit();

                        // Write the type to the buffer
                        type_writer.write(stmt_var) catch continue;

                        const type_str = type_writer.get();
                        try tree.pushStringPair("type", type_str);

                        const stmt_attrs = tree.beginNode();

                        // Add the type header
                        const header = self.store.getTypeHeader(nominal.header);
                        try header.pushToSExprTree(self, tree, nominal.header);

                        try tree.endNode(stmt_begin, stmt_attrs);
                    },
                    else => continue,
                }
            }

            try tree.endNode(type_decls_begin, type_decls_attrs);
        }

        // Create expressions section
        const exprs_begin = tree.beginNode();
        try tree.pushStaticAtom("expressions");
        const exprs_attrs = tree.beginNode();

        // Iterate through all definitions to extract expression types
        for (defs_slice) |def_idx| {
            const def = self.store.getDef(def_idx);
            const expr_var = varFrom(def.expr);

            // Get the region for this expression
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.expr));
            const expr_region = self.store.getRegionAt(expr_node_idx);

            // Create a TypeWriter to format the type
            var type_writer = self.initTypeWriter() catch continue;
            defer type_writer.deinit();

            // Write the type to the buffer
            type_writer.write(expr_var) catch continue;

            // Add the expression type entry
            const expr_begin = tree.beginNode();
            try tree.pushStaticAtom("expr");
            try self.appendRegionInfoToSExprTreeFromRegion(tree, expr_region);

            const type_str = type_writer.get();
            try tree.pushStringPair("type", type_str);

            try tree.endNode(expr_begin, tree.beginNode());
        }

        try tree.endNode(exprs_begin, exprs_attrs);
        try tree.endNode(root_begin, root_attrs);
    }
}

fn pushExprTypesToSExprTree(self: *Self, expr_idx: CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    const expr_begin = tree.beginNode();
    try tree.pushStaticAtom("expr");

    // Add region info for the expression
    try self.appendRegionInfoToSExprTree(tree, expr_idx);

    // Get the type variable for this expression
    const expr_var = varFrom(expr_idx);

    // Create a TypeWriter to format the type
    var type_writer = try self.initTypeWriter();
    defer type_writer.deinit();

    // Write the type to the buffer
    try type_writer.write(expr_var);

    // Add the formatted type to the S-expression tree
    const type_str = type_writer.get();
    try tree.pushStringPair("type", type_str);

    try tree.endNode(expr_begin, tree.beginNode());
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
