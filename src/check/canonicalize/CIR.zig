//! The canonical intermediate representation (CIR) is a representation of the
//! canonicalized abstract syntax tree (AST) that is used for interpreting code generation and type checking, and later compilation stages.

const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const tracy = @import("../../tracy.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const reporting = @import("../../reporting.zig");
const serialization = @import("../../serialization/mod.zig");
const exitOnOom = collections.utils.exitOnOom;
const SExpr = base.SExpr;
const Scratch = base.Scratch;
const DataSpan = base.DataSpan;
const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const ModuleEnv = base.ModuleEnv;
const StringLiteral = base.StringLiteral;
const CalledVia = base.CalledVia;
const SExprTree = base.SExprTree;
const TypeVar = types.Var;

pub const RocDec = @import("../../builtins/dec.zig").RocDec;
pub const Node = @import("Node.zig");
pub const NodeStore = @import("NodeStore.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Allocator = std.mem.Allocator;

// TODO what should this number be? build flag?
const NODE_STORE_CAPACITY = 10_000;

const CIR = @This();

/// Reference to data that persists between compiler stages
env: *ModuleEnv,
/// Stores the raw nodes which represent the intermediate representation
///
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,
/// Temporary source text used for generating SExpr and Reports, required to calculate region info.
///
/// This field exists because:
/// - CIR may be loaded from cache without access to the original source file
/// - Region info calculation requires the source text to convert byte offsets to line/column
/// - The source is only needed temporarily during diagnostic reporting or SExpr generation
///
/// Lifetime: The caller must ensure the source remains valid for the duration of the
/// operation (e.g., `toSExprStr` or `diagnosticToReport` calls).
temp_source_for_sexpr: ?[]const u8 = null,
/// All the definitions and in the module, populated by calling `canonicalize_file`
all_defs: Def.Span,
/// All the top-level statements in the module, populated by calling `canonicalize_file`
all_statements: Statement.Span,
/// All external declarations referenced in this module
external_decls: ExternalDecl.SafeList,
/// Store for interned module imports
imports: Import.Store,
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,

/// Initialize the IR for a module's canonicalization info.
///
/// When caching the can IR for a siloed module, we can avoid
/// manual deserialization of the cached data into IR by putting
/// the entirety of the IR into an arena that holds nothing besides
/// the IR. We can then load the cached binary data back into memory
/// with only 2 syscalls.
/// Initialize the IR for a module's canonicalization info.
///
/// Since the can IR holds indices into the `ModuleEnv`, we need
/// the `ModuleEnv` to also be owned by the can IR to cache it.
pub fn init(env: *ModuleEnv, module_name: []const u8) CIR {
    return CIR{
        .env = env,
        .store = NodeStore.initCapacity(env.gpa, NODE_STORE_CAPACITY),
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = ExternalDecl.SafeList.initCapacity(env.gpa, 16),
        .imports = Import.Store.init(),
        .module_name = module_name,
    };
}

/// Create a CIR from cached data, completely rehydrating from cache
pub fn fromCache(env: *ModuleEnv, cached_store: NodeStore, all_defs: Def.Span, all_statements: Statement.Span, module_name: []const u8) CIR {
    return CIR{
        .env = env,
        .store = cached_store,
        .temp_source_for_sexpr = null,
        .all_defs = all_defs,
        .all_statements = all_statements,
        .external_decls = ExternalDecl.SafeList.initCapacity(env.gpa, 16),
        .imports = Import.Store.init(),
        .module_name = module_name,
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *CIR) void {
    self.store.deinit();
    self.external_decls.deinit(self.env.gpa);
    self.imports.deinit(self.env.gpa);
}

/// Records a diagnostic error during canonicalization without blocking compilation.
///
/// This creates a diagnostic node that stores error information for later reporting.
/// The diagnostic is added to the diagnostic collection but does not create any
/// malformed nodes in the IR.
///
/// Use this when you want to record an error but don't need to replace a node
/// with a runtime error.
pub fn pushDiagnostic(self: *CIR, reason: CIR.Diagnostic) void {
    _ = self.store.addDiagnostic(reason);
}

/// Creates a malformed node that represents a runtime error in the IR. Returns and index of the requested type pointing to a malformed node.
///
/// This follows the "Inform Don't Block" principle: it allows compilation to continue
/// by creating a malformed node that will become a runtime_error in the CIR. If the
/// program execution reaches this node, it will crash with the associated diagnostic.
///
/// This function:
/// 1. Creates a diagnostic node to store the error details
/// 2. Creates a malformed node that references the diagnostic
/// 3. Creates an error type var this CIR index
/// 4. Returns an index of the requested type pointing to the malformed node
///
/// Use this when you need to replace a node (expression, pattern, etc.) with
/// something that represents a compilation error but allows the compiler to continue.
/// Creates a malformed CIR node with an error type variable.
///
/// This follows the "Inform Don't Block" principle - when compilation encounters
/// an error, it creates a placeholder node and continues rather than stopping.
/// The error will be reported to the user later.
///
/// **Usage**: When parsing or canonicalization encounters an error but needs
/// to continue compilation.
///
/// **Example**: Used when encountering invalid syntax that can't be parsed properly.
pub fn pushMalformed(self: *CIR, comptime t: type, reason: CIR.Diagnostic) t {
    const malformed_idx = self.store.addMalformed(t, reason);
    _ = self.setTypeVarAt(@enumFromInt(@intFromEnum(malformed_idx)), .err);
    return malformed_idx;
}

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *CIR) []CIR.Diagnostic {
    const all = self.store.diagnosticSpanFrom(0);

    var list = std.ArrayList(CIR.Diagnostic).init(self.store.gpa);

    for (self.store.sliceDiagnostics(all)) |idx| {
        list.append(self.store.getDiagnostic(idx)) catch |err| exitOnOom(err);
    }

    return list.toOwnedSlice() catch |err| exitOnOom(err);
}

/// Convert a canonicalization diagnostic to a Report for rendering.
///
/// The source parameter is not owned by this function - the caller must ensure it
/// remains valid for the duration of this call. The returned Report will contain
/// references to the source text but does not own it.
pub fn diagnosticToReport(self: *CIR, diagnostic: Diagnostic, allocator: std.mem.Allocator, source: []const u8, filename: []const u8) !reporting.Report {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Set temporary source for calcRegionInfo
    self.temp_source_for_sexpr = source;
    defer self.temp_source_for_sexpr = null;

    return switch (diagnostic) {
        .not_implemented => |data| blk: {
            const feature_text = self.env.strings.get(data.feature);
            break :blk Diagnostic.buildNotImplementedReport(allocator, feature_text);
        },
        .exposed_but_not_implemented => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildExposedButNotImplementedReport(
                allocator,
                ident_name,
                region_info,
                filename,
            );
        },
        .redundant_exposed => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk Diagnostic.buildRedundantExposedReport(
                allocator,
                ident_name,
                region_info,
                original_region_info,
                filename,
            );
        },
        .invalid_num_literal => |data| blk: {
            break :blk Diagnostic.buildInvalidNumLiteralReport(
                allocator,
                data.region,
                source,
            );
        },
        .ident_already_in_scope => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            break :blk Diagnostic.buildIdentAlreadyInScopeReport(
                allocator,
                ident_name,
            );
        },
        .ident_not_in_scope => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildIdentNotInScopeReport(
                allocator,
                ident_name,
                region_info,
                filename,
            );
        },
        .invalid_top_level_statement => |data| blk: {
            const stmt_name = self.env.strings.get(data.stmt);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildInvalidTopLevelStatementReport(
                allocator,
                stmt_name,
                region_info,
                filename,
            );
        },
        .f64_pattern_literal => |data| blk: {
            break :blk Diagnostic.buildF64PatternLiteralReport(
                allocator,
                data.region,
                source,
            );
        },
        .invalid_single_quote => Diagnostic.buildInvalidSingleQuoteReport(allocator),
        .too_long_single_quote => Diagnostic.buildTooLongSingleQuoteReport(allocator),
        .empty_single_quote => Diagnostic.buildEmptySingleQuoteReport(allocator),
        .crash_expects_string => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildCrashExpectsStringReport(allocator, region_info, filename);
        },
        .empty_tuple => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildEmptyTupleReport(allocator, region_info, filename);
        },
        .expr_not_canonicalized => Diagnostic.buildExprNotCanonicalizedReport(allocator),
        .invalid_string_interpolation => Diagnostic.buildInvalidStringInterpolationReport(allocator),
        .pattern_arg_invalid => Diagnostic.buildPatternArgInvalidReport(allocator),
        .pattern_not_canonicalized => Diagnostic.buildPatternNotCanonicalizedReport(allocator),
        .can_lambda_not_implemented => Diagnostic.buildCanLambdaNotImplementedReport(allocator),
        .lambda_body_not_canonicalized => Diagnostic.buildLambdaBodyNotCanonicalizedReport(allocator),
        .if_condition_not_canonicalized => Diagnostic.buildIfConditionNotCanonicalizedReport(allocator),
        .if_then_not_canonicalized => Diagnostic.buildIfThenNotCanonicalizedReport(allocator),
        .if_else_not_canonicalized => Diagnostic.buildIfElseNotCanonicalizedReport(allocator),
        .var_across_function_boundary => Diagnostic.buildVarAcrossFunctionBoundaryReport(allocator),
        .malformed_type_annotation => Diagnostic.buildMalformedTypeAnnotationReport(allocator),
        .malformed_where_clause => Diagnostic.buildMalformedWhereClauseReport(allocator),
        .shadowing_warning => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk Diagnostic.buildShadowingWarningReport(
                allocator,
                ident_name,
                new_region_info,
                original_region_info,
                filename,
            );
        },
        .type_redeclared => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);
            break :blk Diagnostic.buildTypeRedeclaredReport(
                allocator,
                type_name,
                original_region_info,
                redeclared_region_info,
                filename,
            );
        },
        .tuple_elem_not_canonicalized => Diagnostic.buildTupleElemNotCanonicalizedReport(allocator),
        .module_not_found => |data| blk: {
            const module_name = self.env.idents.getText(data.module_name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildModuleNotFoundReport(
                allocator,
                module_name,
                region_info,
                filename,
            );
        },
        .value_not_exposed => |data| blk: {
            const module_name = self.env.idents.getText(data.module_name);
            const value_name = self.env.idents.getText(data.value_name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildValueNotExposedReport(
                allocator,
                module_name,
                value_name,
                region_info,
                filename,
            );
        },
        .type_not_exposed => |data| blk: {
            const module_name = self.env.idents.getText(data.module_name);
            const type_name = self.env.idents.getText(data.type_name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildTypeNotExposedReport(
                allocator,
                module_name,
                type_name,
                region_info,
                filename,
            );
        },
        .module_not_imported => |data| blk: {
            const module_name = self.env.idents.getText(data.module_name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildModuleNotImportedReport(
                allocator,
                module_name,
                region_info,
                filename,
            );
        },
        .too_many_exports => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildTooManyExportsReport(
                allocator,
                data.count,
                region_info,
                filename,
            );
        },
        .undeclared_type => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildUndeclaredTypeReport(
                allocator,
                type_name,
                region_info,
                filename,
            );
        },
        .undeclared_type_var => |data| blk: {
            const type_var_name = self.env.idents.getText(data.name);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildUndeclaredTypeVarReport(
                allocator,
                type_var_name,
                region_info,
                filename,
            );
        },
        .type_alias_redeclared => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);
            break :blk Diagnostic.buildTypeAliasRedeclaredReport(
                allocator,
                type_name,
                original_region_info,
                redeclared_region_info,
                filename,
            );
        },
        .nominal_type_redeclared => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);
            break :blk Diagnostic.buildNominalTypeRedeclaredReport(
                allocator,
                type_name,
                original_region_info,
                redeclared_region_info,
                filename,
            );
        },
        .type_shadowed_warning => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk Diagnostic.buildTypeShadowedWarningReport(
                allocator,
                type_name,
                new_region_info,
                original_region_info,
                data.cross_scope,
                filename,
            );
        },
        .type_parameter_conflict => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const parameter_name = self.env.idents.getText(data.parameter_name);
            const region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk Diagnostic.buildTypeParameterConflictReport(
                allocator,
                type_name,
                parameter_name,
                region_info,
                original_region_info,
                filename,
            );
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk try Diagnostic.buildUnusedVariableReport(
                allocator,
                &self.env.idents,
                region_info,
                data,
                filename,
            );
        },
        .used_underscore_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk try Diagnostic.buildUsedUnderscoreVariableReport(
                allocator,
                &self.env.idents,
                region_info,
                data,
                filename,
            );
        },
        .duplicate_record_field => |data| blk: {
            const duplicate_region_info = self.calcRegionInfo(data.duplicate_region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const field_name = self.env.idents.getText(data.field_name);
            break :blk try Diagnostic.buildDuplicateRecordFieldReport(
                allocator,
                field_name,
                duplicate_region_info,
                original_region_info,
                filename,
            );
        },
    };
}

/// Convert a type into a node index
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Convert a type into a type var
pub fn varFrom(idx: anytype) TypeVar {
    return @enumFromInt(@intFromEnum(idx));
}

/// Creates a fresh flexible type variable for type inference.
///
/// This is a convenience wrapper around `pushTypeVar(.{ .flex_var = null }, ...)`.
/// Use this for expressions where the type needs to be inferred, like integer
/// literals before knowing their specific type (U64, I32, etc.).
///
pub fn pushFreshTypeVar(self: *CIR, parent_node_idx: Node.Idx, region: base.Region) Allocator.Error!types.Var {
    return self.pushTypeVar(.{ .flex_var = null }, parent_node_idx, region);
}

/// Creates a type variable with specific type content.
///
/// Use this to create type variables with predetermined structure or constraints,
/// unlike `pushFreshTypeVar` which creates unconstrained flexible variables.
///
/// **Common content types**:
/// - `.flex_var` - Flexible (same as pushFreshTypeVar)
/// - `.rigid_var` - Named type variable for generics
/// - `.structure` - Concrete types (nums, records, functions)
/// - `.err` - Error type for malformed code
pub fn pushTypeVar(self: *CIR, content: types.Content, parent_node_idx: Node.Idx, region: base.Region) Allocator.Error!types.Var {
    // insert a placeholder can node
    const var_slot = self.store.addTypeVarSlot(parent_node_idx, region);

    // if the new can node idx is greater than the types store length, backfill
    const var_: types.Var = @enumFromInt(@intFromEnum(var_slot));
    try self.env.types.fillInSlotsThru(var_);

    // set the type store slot based on the placeholder node idx
    try self.env.types.setVarContent(var_, content);

    return var_;
}

/// Creates a fresh flexible type var that redirects to another tyype var
pub fn pushRedirectTypeVar(self: *CIR, redirect_to: TypeVar, parent_node_idx: Node.Idx, region: base.Region) Allocator.Error!types.Var {
    const var_ = try self.pushTypeVar(.{ .flex_var = null }, parent_node_idx, region);
    try self.env.types.setVarRedirect(var_, redirect_to);
    return var_;
}

/// Associates a type with an existing definition node.
///
/// Use this to set the concrete type of a definition after type inference.
pub fn setTypeVarAtDef(self: *CIR, at_idx: Def.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Associates a type with an existing definition node.
///
/// Use this to set the concrete type of a definition after type inference.
pub fn setTypeVarAtStmt(self: *CIR, at_idx: Statement.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Convert any CIR index to a type variable, ensuring the slot exists.
///
/// This helper handles the conversion from CIR indices to type variables
/// and ensures the type store has allocated the necessary slots.
pub fn idxToTypeVar(_: *const CIR, types_store: *types.Store, idx: anytype) !types.Var {
    const var_: types.Var = @enumFromInt(@intFromEnum(idx));
    try types_store.fillInSlotsThru(var_);
    return var_;
}

/// Associates a type with an existing expression node.
///
/// Use this to set the final type of an expression after type inference.
pub fn setTypeVarAtExpr(self: *CIR, at_idx: Expr.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Associates a type with an existing pattern node.
///
/// Use this to set the type of a pattern after type inference or from context.
pub fn setTypeVarAtPat(self: *CIR, at_idx: Pattern.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Function that redirects an existing CIR node to the provided var.
pub fn setTypeRedirectAt(self: *CIR, at_idx: Node.Idx, redirect_to: types.Var) types.Var {
    // if the new can node idx is greater than the types store length, backfill
    const var_: types.Var = @enumFromInt(@intFromEnum(at_idx));
    self.env.types.fillInSlotsThru(var_) catch |err| exitOnOom(err);

    // set the type store slot based on the placeholder node idx
    self.env.types.setVarRedirect(var_, redirect_to) catch |err| exitOnOom(err);

    return var_;
}

/// Core function that associates a type with any existing CIR node.
///
/// This is used by all the `setTypeVarAt*` wrapper functions. Node indices
/// correspond directly to type variable indices, allowing direct conversion.
/// Usually called indirectly through the typed wrappers rather than directly.
pub fn setTypeVarAt(self: *CIR, at_idx: Node.Idx, content: types.Content) types.Var {
    // if the new can node idx is greater than the types store length, backfill
    const var_: types.Var = @enumFromInt(@intFromEnum(at_idx));
    self.env.types.fillInSlotsThru(var_) catch |err| exitOnOom(err);

    // set the type store slot based on the placeholder node idx
    self.env.types.setVarContent(var_, content) catch |err| exitOnOom(err);

    return var_;
}

/// Adds an external declaration to the CIR and returns its index
pub fn pushExternalDecl(self: *CIR, decl: ExternalDecl) ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = self.external_decls.append(self.env.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const CIR, idx: ExternalDecl.Idx) *const ExternalDecl {
    return self.external_decls.get(@as(ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *CIR, decls: []const ExternalDecl) ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = self.external_decls.append(self.env.gpa, decl);
    }
    return ExternalDecl.Span{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const CIR, span: ExternalDecl.Span) []const ExternalDecl {
    const range = ExternalDecl.SafeList.Range{ .start = @enumFromInt(span.span.start), .end = @enumFromInt(span.span.start + span.span.len) };
    return self.external_decls.rangeToSlice(range);
}

/// Retrieves the text of an identifier by its index
pub fn getIdentText(self: *const CIR, idx: Ident.Idx) []const u8 {
    return self.env.idents.getText(idx);
}

// Helper to format pattern index for s-expr output
fn formatPatternIdxNode(gpa: std.mem.Allocator, pattern_idx: Pattern.Idx) SExpr {
    var node = SExpr.init(gpa, "pid");
    node.appendUnsignedInt(gpa, @intFromEnum(pattern_idx));
    return node;
}

test "Node is 16 bytes" {
    try std.testing.expectEqual(16, @sizeOf(Node));
}

test "ExternalDecl serialization round-trip" {
    const gpa = std.testing.allocator;

    // Create original external declaration
    const original = ExternalDecl{
        .qualified_name = @bitCast(@as(u32, 123)),
        .module_name = @bitCast(@as(u32, 456)),
        .local_name = @bitCast(@as(u32, 789)),
        .type_var = @enumFromInt(999),
        .kind = .value,
        .region = Region{
            .start = .{ .offset = 10 },
            .end = .{ .offset = 20 },
        },
    };

    // Serialize
    const serialized_size = original.serializedSize();
    const buffer = try gpa.alloc(u8, serialized_size);
    defer gpa.free(buffer);

    const serialized = try original.serializeInto(buffer);
    try std.testing.expectEqual(serialized_size, serialized.len);

    // Deserialize
    const restored = try ExternalDecl.deserializeFrom(serialized);

    // Verify all fields are identical
    try std.testing.expectEqual(original.qualified_name, restored.qualified_name);
    try std.testing.expectEqual(original.module_name, restored.module_name);
    try std.testing.expectEqual(original.local_name, restored.local_name);
    try std.testing.expectEqual(original.type_var, restored.type_var);
    try std.testing.expectEqual(original.kind, restored.kind);
    try std.testing.expectEqual(original.region.start.offset, restored.region.start.offset);
    try std.testing.expectEqual(original.region.end.offset, restored.region.end.offset);
}

test "ExternalDecl serialization comprehensive" {
    const gpa = std.testing.allocator;

    // Test various external declarations including edge cases
    const decl1 = ExternalDecl{
        .qualified_name = @bitCast(@as(u32, 0)), // minimum value
        .module_name = @bitCast(@as(u32, 1)),
        .local_name = @bitCast(@as(u32, 2)),
        .type_var = @enumFromInt(0),
        .kind = .value,
        .region = Region{
            .start = .{ .offset = 0 },
            .end = .{ .offset = 1 },
        },
    };

    const decl2 = ExternalDecl{
        .qualified_name = @bitCast(@as(u32, 0xFFFFFFFF)), // maximum value
        .module_name = @bitCast(@as(u32, 0xFFFFFFFE)),
        .local_name = @bitCast(@as(u32, 0xFFFFFFFD)),
        .type_var = @enumFromInt(0xFFFFFFFF),
        .kind = .type,
        .region = Region{
            .start = .{ .offset = 0xFFFFFFFF },
            .end = .{ .offset = 0xFFFFFFFE },
        },
    };

    // Test serialization using the testing framework
    try serialization.testing.testSerialization(ExternalDecl, &decl1, gpa);
    try serialization.testing.testSerialization(ExternalDecl, &decl2, gpa);
}

test "ExternalDecl different kinds serialization" {
    const gpa = std.testing.allocator;

    const value_decl = ExternalDecl{
        .qualified_name = @bitCast(@as(u32, 100)),
        .module_name = @bitCast(@as(u32, 200)),
        .local_name = @bitCast(@as(u32, 300)),
        .type_var = @enumFromInt(400),
        .kind = .value,
        .region = Region{
            .start = .{ .offset = 50 },
            .end = .{ .offset = 75 },
        },
    };

    const type_decl = ExternalDecl{
        .qualified_name = @bitCast(@as(u32, 100)),
        .module_name = @bitCast(@as(u32, 200)),
        .local_name = @bitCast(@as(u32, 300)),
        .type_var = @enumFromInt(400),
        .kind = .type,
        .region = Region{
            .start = .{ .offset = 50 },
            .end = .{ .offset = 75 },
        },
    };

    try serialization.testing.testSerialization(ExternalDecl, &value_decl, gpa);
    try serialization.testing.testSerialization(ExternalDecl, &type_decl, gpa);
}

/// A working representation of a record field
pub const RecordField = struct {
    name: Ident.Idx,
    value: Expr.Idx,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("field");
        tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();
        ir.store.getExpr(self.value).pushToSExprTree(ir, tree, self.value);
        tree.endNode(begin, attrs);
    }
};

/// Canonical representation of where clauses in Roc.
///
/// Where clauses specify constraints on type variables, typically requiring that
/// a type comes from a module that provides specific methods or satisfies certain
/// type aliases.
pub const WhereClause = union(enum) {
    /// Module method constraint: `module(a).method : Args -> RetType`
    ///
    /// Specifies that type variable `a` must come from a module that provides
    /// a method with the given signature.
    mod_method: ModuleMethod,

    /// Module alias constraint: `module(a).AliasName`
    ///
    /// Specifies that type variable `a` must satisfy the constraints defined
    /// by the given type alias.
    mod_alias: ModuleAlias,

    /// Malformed where clause that couldn't be canonicalized correctly.
    ///
    /// Contains diagnostic information about what went wrong.
    malformed: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub const ModuleMethod = struct {
        var_name: Ident.Idx, // Type variable identifier (e.g., "a")
        method_name: Ident.Idx, // Method name without leading dot (e.g., "decode")
        args: TypeAnno.Span, // Method argument types
        ret_anno: TypeAnno.Idx, // Method return type
        external_decl: ExternalDecl.Idx, // External declaration for module lookup
    };

    pub const ModuleAlias = struct {
        var_name: Ident.Idx, // Type variable identifier (e.g., "elem")
        alias_name: Ident.Idx, // Alias name without leading dot (e.g., "Sort")
        external_decl: ExternalDecl.Idx, // External declaration for module lookup
    };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, where_idx: WhereClause.Idx) void {
        switch (self.*) {
            .mod_method => |mm| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("method");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                tree.pushStringPair("module-of", ir.env.idents.getText(mm.var_name));
                tree.pushStringPair("ident", ir.env.idents.getText(mm.method_name));
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                for (ir.store.sliceTypeAnnos(mm.args)) |arg_idx| {
                    ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
                }
                tree.endNode(args_begin, attrs2);

                ir.store.getTypeAnno(mm.ret_anno).pushToSExprTree(ir, tree, mm.ret_anno);
                tree.endNode(begin, attrs);
            },
            .mod_alias => |ma| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("alias");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                tree.pushStringPair("module-of", ir.env.idents.getText(ma.var_name));
                tree.pushStringPair("ident", ir.env.idents.getText(ma.alias_name));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .malformed => |_| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("malformed");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
        }
    }

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement PatternRecordField
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// Canonical representation of type declaration headers.
///
/// The type header is the left-hand side of a type declaration, specifying the type name
/// and its parameters. For example, in `Map(a, b) : List(a) -> List(b)`, the header is
/// `Map(a, b)` with name "Map" and type parameters `[a, b]`.
///
/// Examples:
/// - `Foo` - simple type with no parameters
/// - `List(a)` - generic type with one parameter
/// - `Dict(k, v)` - generic type with two parameters
/// - `Result(ok, err)` - generic type with named parameters
pub const TypeHeader = struct {
    name: Ident.Idx, // The type name (e.g., "Map", "List", "Dict")
    args: TypeAnno.Span, // Type parameters (e.g., [a, b] for generic types)

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, header_idx: TypeHeader.Idx) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ty-header");
        const region = ir.store.getRegionAt(@enumFromInt(@intFromEnum(header_idx)));
        ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
        tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            tree.pushStaticAtom("ty-args");
            const attrs2 = tree.beginNode();
            for (ir.store.sliceTypeAnnos(self.args)) |arg_idx| {
                ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
            }
            tree.endNode(args_begin, attrs2);
        }

        tree.endNode(begin, attrs);
    }
};

/// An item exposed from an imported module
/// Examples: `line!`, `Type as ValueCategory`, `Custom.*`
pub const ExposedItem = struct {
    /// The identifier being exposed
    name: Ident.Idx,
    /// Optional alias for the exposed item (e.g., `function` in `func as function`)
    alias: ?Ident.Idx,
    /// Whether this is a wildcard import (e.g., `Custom.*`)
    is_wildcard: bool,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn pushToSExprTree(self: ExposedItem, env: *base.ModuleEnv, ir: *const CIR, tree: *SExprTree) void {
        _ = ir; // Unused in this function, but could be used for more complex logic

        const begin = tree.beginNode();
        tree.pushStaticAtom("exposed");
        tree.pushStringPair("name", env.idents.getText(self.name));

        if (self.alias) |alias_idx| {
            tree.pushStringPair("alias", env.idents.getText(alias_idx));
        }

        tree.pushBoolPair("wildcard", self.is_wildcard);
        const attrs = tree.beginNode();
        tree.endNode(begin, attrs);
    }
};

/// An imported module
pub const Import = struct {
    pub const Idx = enum(u16) { _ };

    /// A store for interning imported module names
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

        pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
            self.map.deinit(gpa);
            self.imports.deinit(gpa);
            self.strings.deinit(gpa);
        }

        /// Get or create an Import.Idx for a module name
        pub fn getOrPut(self: *Store, gpa: std.mem.Allocator, module_name: []const u8) !Import.Idx {
            const gop = try self.map.getOrPut(gpa, module_name);
            if (!gop.found_existing) {
                // Store the string
                const start = self.strings.items.len;
                try self.strings.appendSlice(gpa, module_name);
                const stored_name = self.strings.items[start..];

                const import_idx: Import.Idx = @enumFromInt(self.imports.items.len);
                try self.imports.append(gpa, stored_name);
                gop.value_ptr.* = import_idx;
            }
            return gop.value_ptr.*;
        }
    };
};

/// A file of any type that has been ingested into a Roc module
/// as raw data, e.g. `import "lookups.txt" as lookups : Str`.
///
/// These ingestions aren't resolved until the import resolution
/// compiler stage.
pub const IngestedFile = struct {
    relative_path: StringLiteral.Idx,
    ident: Ident.Idx,
    type: Annotation,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;
    pub const NonEmptyRange = List.NonEmptyRange;

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ingested-file");
        tree.pushStringPair("path", ir.env.strings.get(self.relative_path));
        tree.pushStringPair("ident", ir.env.idents.getText(self.ident));

        const attrs = tree.beginNode();
        self.type.pushToSExprTree(ir, tree);
        tree.endNode(begin, attrs);
    }
};

/// A definition of a value (or destructured values) that
/// takes its value from an expression.
pub const Def = struct {
    pattern: Pattern.Idx,
    expr: Expr.Idx,
    // TODO:
    // pattern_vars: SendMap<Symbol, Variable>,
    annotation: ?Annotation.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A def that introduces identifiers
        let,
        /// A standalone statement with an fx variable
        stmt: TypeVar,
        /// Ignored result, must be effectful
        ignored: TypeVar,

        /// encode the kind of def into two u32 values
        pub fn encode(self: *const Kind) [2]u32 {
            switch (self.*) {
                .let => return .{ 0, 0 },
                .stmt => |ty_var| return .{ 1, @intFromEnum(ty_var) },
                .ignored => |ty_var| return .{ 2, @intFromEnum(ty_var) },
            }
        }

        /// decode the kind of def from two u32 values
        pub fn decode(data: [2]u32) Kind {
            if (data[0] == 0) {
                return .let;
            } else if (data[0] == 1) {
                return .{ .stmt = @as(TypeVar, @enumFromInt(data[1])) };
            } else if (data[0] == 2) {
                return .{ .ignored = @as(TypeVar, @enumFromInt(data[1])) };
            } else {
                @panic("invalid def kind");
            }
        }

        test "encode and decode def kind" {
            const kind: Kind = Kind.let;
            const encoded = kind.encode();
            const decoded = Kind.decode(encoded);
            try std.testing.expect(decoded == Kind.let);
        }

        test "encode and decode def kind with type var" {
            const kind: Kind = .{ .stmt = @as(TypeVar, @enumFromInt(42)) };
            const encoded = kind.encode();
            const decoded = Kind.decode(encoded);
            switch (decoded) {
                .stmt => |stmt| {
                    try std.testing.expect(stmt == @as(TypeVar, @enumFromInt(42)));
                },
                else => @panic("invalid def kind"),
            }
        }
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
    pub const Range = struct { start: u32, len: u32 };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        tree.pushStaticAtom(name);
        const attrs = tree.beginNode();

        ir.store.getPattern(self.pattern).pushToSExprTree(ir, tree, self.pattern);

        ir.store.getExpr(self.expr).pushToSExprTree(ir, tree, self.expr);

        if (self.annotation) |anno_idx| {
            const anno = ir.store.getAnnotation(anno_idx);
            anno.pushToSExprTree(ir, tree, anno_idx);
        }

        tree.endNode(begin, attrs);
    }
};

/// todo
/// An annotation represents a canonicalized type signature that connects
/// a type declaration to a value definition
pub const Annotation = struct {
    /// The canonicalized declared type structure (what the programmer wrote)
    type_anno: TypeAnno.Idx,
    /// The canonical type signature as a type variable (for type inference)
    signature: TypeVar,

    pub const Idx = enum(u32) { _ };

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, anno_idx: Annotation.Idx) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("annotation");
        const region = ir.store.getRegionAt(@enumFromInt(@intFromEnum(anno_idx)));
        ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
        const attrs = tree.beginNode();

        // Add the declared type annotation structure
        const type_begin = tree.beginNode();
        tree.pushStaticAtom("declared-type");
        const type_attrs = tree.beginNode();
        ir.store.getTypeAnno(self.type_anno).pushToSExprTree(ir, tree, self.type_anno);
        tree.endNode(type_begin, type_attrs);

        tree.endNode(begin, attrs);
    }
};

/// External declaration node for cross-module references
///
/// This node represents a reference to a declaration from another module
/// that hasn't been resolved yet. It serves as a placeholder during
/// canonicalization that will be populated with type information
/// later during dependency resolution.
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: Ident.Idx,

    /// Module this decl comes from (e.g., "json.Json")
    module_name: Ident.Idx,

    /// Local name within that module (e.g., "utf8")
    local_name: Ident.Idx,

    /// Type information (populated later after dependency resolution)
    type_var: TypeVar,

    /// Whether this is a value or type declaration
    kind: enum { value, type },

    /// Region where this external declaration was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ext-decl");
        ir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        tree.pushStringPair("ident", ir.env.idents.getText(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => tree.pushStringPair("kind", "value"),
            .type => tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        tree.endNode(begin, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const @This(), ir: *const CIR, tree: *SExprTree, region: Region) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ext-decl");
        ir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        tree.pushStringPair("ident", ir.env.idents.getText(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => tree.pushStringPair("kind", "value"),
            .type => tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        tree.endNode(begin, attrs);
    }

    /// Calculate the serialized size of this external declaration
    pub fn serializedSize(self: *const @This()) usize {
        _ = self;
        return @sizeOf(u32) + // qualified_name
            @sizeOf(u32) + // module_name
            @sizeOf(u32) + // local_name
            @sizeOf(u32) + // type_var
            @sizeOf(u8) + // kind (enum)
            @sizeOf(u32) + // region.start.offset
            @sizeOf(u32); // region.end.offset
    }

    /// Serialize this external declaration into the provided buffer
    pub fn serializeInto(self: *const @This(), buffer: []u8) ![]const u8 {
        const size = self.serializedSize();
        if (buffer.len < size) return error.BufferTooSmall;

        var offset: usize = 0;

        // Serialize qualified_name
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], @bitCast(self.qualified_name), .little);
        offset += 4;

        // Serialize module_name
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], @bitCast(self.module_name), .little);
        offset += 4;

        // Serialize local_name
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], @bitCast(self.local_name), .little);
        offset += 4;

        // Serialize type_var
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], @intFromEnum(self.type_var), .little);
        offset += 4;

        // Serialize kind
        buffer[offset] = switch (self.kind) {
            .value => 0,
            .type => 1,
        };
        offset += 1;

        // Serialize region
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], self.region.start.offset, .little);
        offset += 4;
        std.mem.writeInt(u32, buffer[offset .. offset + 4][0..4], self.region.end.offset, .little);
        offset += 4;

        return buffer[0..offset];
    }

    /// Deserialize an external declaration from the provided buffer
    pub fn deserializeFrom(buffer: []const u8) !@This() {
        var offset: usize = 0;
        const needed_size = @sizeOf(u32) * 6 + @sizeOf(u8);
        if (buffer.len < needed_size) return error.BufferTooSmall;

        const qualified_name: Ident.Idx = @bitCast(std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little));
        offset += 4;

        const module_name: Ident.Idx = @bitCast(std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little));
        offset += 4;

        const local_name: Ident.Idx = @bitCast(std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little));
        offset += 4;

        const type_var: TypeVar = @enumFromInt(std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little));
        offset += 4;

        const kind_byte = buffer[offset];
        offset += 1;
        const kind: @TypeOf(@as(@This(), undefined).kind) = switch (kind_byte) {
            0 => .value,
            1 => .type,
            else => return error.InvalidKind,
        };

        const start_offset = std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little);
        offset += 4;
        const end_offset = std.mem.readInt(u32, buffer[offset .. offset + 4][0..4], .little);
        offset += 4;

        const region = Region{
            .start = .{ .offset = start_offset },
            .end = .{ .offset = end_offset },
        };

        return @This(){
            .qualified_name = qualified_name,
            .module_name = module_name,
            .local_name = local_name,
            .type_var = type_var,
            .kind = kind,
            .region = region,
        };
    }
};

/// Tracks type variables introduced during annotation canonicalization
pub const IntroducedVariables = struct {
    /// Named type variables (e.g., 'a' in 'a -> a')
    named: std.ArrayListUnmanaged(NamedVariable),
    /// Wildcard type variables (e.g., '*' in some contexts)
    wildcards: std.ArrayListUnmanaged(TypeVar),
    /// Inferred type variables (e.g., '_')
    inferred: std.ArrayListUnmanaged(TypeVar),

    pub fn init() IntroducedVariables {
        return IntroducedVariables{
            .named = .{},
            .wildcards = .{},
            .inferred = .{},
        };
    }

    pub fn deinit(self: *IntroducedVariables, gpa: std.mem.Allocator) void {
        self.named.deinit(gpa);
        self.wildcards.deinit(gpa);
        self.inferred.deinit(gpa);
    }

    /// Insert a named type variable
    pub fn insertNamed(self: *IntroducedVariables, gpa: std.mem.Allocator, name: Ident.Idx, var_type: TypeVar, region: Region) void {
        const named_var = NamedVariable{
            .name = name,
            .variable = var_type,
            .first_seen = region,
        };
        self.named.append(gpa, named_var) catch |err| collections.exitOnOom(err);
    }

    /// Insert a wildcard type variable
    pub fn insertWildcard(self: *IntroducedVariables, gpa: std.mem.Allocator, var_type: TypeVar) void {
        self.wildcards.append(gpa, var_type) catch |err| collections.exitOnOom(err);
    }

    /// Insert an inferred type variable
    pub fn insertInferred(self: *IntroducedVariables, gpa: std.mem.Allocator, var_type: TypeVar) void {
        self.inferred.append(gpa, var_type) catch |err| collections.exitOnOom(err);
    }

    /// Find a type variable by name
    pub fn varByName(self: *const IntroducedVariables, name: Ident.Idx) ?TypeVar {
        // Check named variables
        for (self.named.items) |named_var| {
            if (named_var.name == name) {
                return named_var.variable;
            }
        }

        return null;
    }

    /// Union with another IntroducedVariables
    pub fn unionWith(self: *IntroducedVariables, other: *const IntroducedVariables) void {
        // This is a simplified union - in practice we'd want to avoid duplicates
        // For now, just append all items
        const gpa = std.heap.page_allocator; // TODO: pass proper allocator

        self.named.appendSlice(gpa, other.named.items) catch |err| collections.exitOnOom(err);
        self.wildcards.appendSlice(gpa, other.wildcards.items) catch |err| collections.exitOnOom(err);
        self.inferred.appendSlice(gpa, other.inferred.items) catch |err| collections.exitOnOom(err);
    }
};

/// A named type variable in an annotation
pub const NamedVariable = struct {
    variable: TypeVar,
    name: Ident.Idx,
    first_seen: Region,
};

/// Tracks references to symbols and modules made by an annotation
pub const References = struct {
    /// References to value symbols
    value_lookups: std.ArrayListUnmanaged(Ident.Idx),
    /// References to type symbols
    type_lookups: std.ArrayListUnmanaged(Ident.Idx),
    /// References to modules
    module_lookups: std.ArrayListUnmanaged(Ident.Idx),

    pub fn init() References {
        return .{
            .value_lookups = .{},
            .type_lookups = .{},
            .module_lookups = .{},
        };
    }

    pub fn deinit(self: *References, gpa: std.mem.Allocator) void {
        self.value_lookups.deinit(gpa);
        self.type_lookups.deinit(gpa);
        self.module_lookups.deinit(gpa);
    }

    /// Insert a value symbol reference
    pub fn insertValueLookup(self: *References, gpa: std.mem.Allocator, symbol: Ident.Idx) void {
        self.value_lookups.append(gpa, symbol) catch |err| exitOnOom(err);
    }
};

/// todo
pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    /// todo
    pub const Kind = enum { i128, u128 };

    pub fn placeholder() IntValue {
        return IntValue{
            .bytes = [16]u8{ 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
            .kind = .i128,
        };
    }
};

/// Helper function to generate the S-expression node for the entire Canonical IR.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree, source: []const u8) void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;

    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        ir.store.getExpr(expr_idx).pushToSExprTree(ir, tree, expr_idx);
    } else {
        const root_begin = tree.beginNode();
        tree.pushStaticAtom("can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression tree
        const defs_slice = ir.store.sliceDefs(ir.all_defs);
        const statements_slice = ir.store.sliceStatements(ir.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and ir.external_decls.len() == 0) {
            tree.pushBoolPair("empty", true);
        }
        const attrs = tree.beginNode();

        for (defs_slice) |def_idx| {
            ir.store.getDef(def_idx).pushToSExprTree(ir, tree);
        }

        for (statements_slice) |stmt_idx| {
            ir.store.getStatement(stmt_idx).pushToSExprTree(ir, tree, stmt_idx);
        }

        for (0..ir.external_decls.len()) |i| {
            const external_decl = ir.external_decls.get(@enumFromInt(i));
            external_decl.pushToSExprTree(ir, tree);
        }

        tree.endNode(root_begin, attrs);
    }
}

test "NodeStore - init and deinit" {
    var store = CIR.NodeStore.init(testing.allocator);
    defer store.deinit();

    try testing.expect(store.nodes.len() == 0);
    try testing.expect(store.extra_data.items.len == 0);
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn calcRegionInfo(self: *const CIR, region: Region) base.RegionInfo {
    const empty = base.RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
        .line_text = "",
    };

    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const source = self.temp_source_for_sexpr orelse {
        // No source available, return empty region info
        return empty;
    };

    const info = base.RegionInfo.position(source, self.env.line_starts.items.items, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSexprNode(ir: *const CIR, node: *SExpr, idx: anytype) void {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    ir.appendRegionInfoToSexprNodeFromRegion(node, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSexprNodeFromRegion(ir: *const CIR, node: *SExpr, region: Region) void {
    const info = ir.calcRegionInfo(region);
    node.appendByteRange(
        ir.env.gpa,
        info,
        region.start.offset,
        region.end.offset,
    );
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSExprTree(ir: *const CIR, tree: *SExprTree, idx: anytype) void {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSExprTreeFromRegion(ir: *const CIR, tree: *SExprTree, region: Region) void {
    const info = ir.calcRegionInfo(region);
    tree.pushBytesRange(
        region.start.offset,
        region.end.offset,
        info,
    );
}

/// Get region information for a node in the Canonical IR.
pub fn getNodeRegionInfo(ir: *const CIR, idx: anytype) base.RegionInfo {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    return ir.calcRegionInfo(region);
}

/// Helper function to convert type information from the Canonical IR to an SExpr node
/// in S-expression format for snapshot testing. Implements the definition-focused
/// format showing final types for defs, expressions, and builtins.
pub fn pushTypesToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree, source: []const u8) std.mem.Allocator.Error!void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;

    const gpa = ir.env.gpa;

    // Create TypeWriter for converting types to strings
    var type_writer = try types.writers.TypeWriter.init(gpa, ir.env);
    defer type_writer.deinit();

    if (maybe_expr_idx) |expr_idx| {
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

        const expr_begin = tree.beginNode();
        tree.pushStaticAtom("expr");

        ir.appendRegionInfoToSExprTree(tree, expr_idx);

        if (@intFromEnum(expr_var) > ir.env.types.slots.backing.len()) {
            const unknown_begin = tree.beginNode();
            tree.pushStaticAtom("unknown");
            const unknown_attrs = tree.beginNode();
            tree.endNode(unknown_begin, unknown_attrs);
        } else {
            if (type_writer.write(expr_var)) {
                tree.pushStringPair("type", type_writer.get());
            } else |err| {
                exitOnOom(err);
            }
        }

        const expr_attrs = tree.beginNode();
        tree.endNode(expr_begin, expr_attrs);
    } else {
        const root_begin = tree.beginNode();
        tree.pushStaticAtom("inferred-types");

        const attrs = tree.beginNode();

        // Collect definitions
        const defs_begin = tree.beginNode();
        tree.pushStaticAtom("defs");
        const defs_attrs = tree.beginNode();

        const all_defs = ir.store.sliceDefs(ir.all_defs);

        for (all_defs) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Extract identifier name from the pattern (assuming it's an assign pattern)
            const pattern = ir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |_| {
                    const patt_begin = tree.beginNode();
                    tree.pushStaticAtom("patt");

                    // Get the pattern region instead of the whole def region
                    const pattern_region = ir.store.getPatternRegion(def.pattern);
                    ir.appendRegionInfoToSExprTreeFromRegion(tree, pattern_region);

                    // Get the type variable for this definition
                    const def_var = try ir.idxToTypeVar(&ir.env.types, def_idx);

                    // Clear the buffer and write the type
                    try type_writer.write(def_var);
                    tree.pushStringPair("type", type_writer.get());

                    const patt_attrs = tree.beginNode();
                    tree.endNode(patt_begin, patt_attrs);
                },
                else => {
                    // For non-assign patterns, we could handle destructuring, but for now skip
                    continue;
                },
            }
        }

        tree.endNode(defs_begin, defs_attrs);

        const all_stmts = ir.store.sliceStatements(ir.all_statements);

        var has_type_decl = false;
        for (all_stmts) |stmt_idx| {
            const stmt = ir.store.getStatement(stmt_idx);
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
            tree.pushStaticAtom("type_decls");
            const stmts_attrs = tree.beginNode();

            for (all_stmts) |stmt_idx| {
                const stmt = ir.store.getStatement(stmt_idx);

                // Get the type variable for this definition
                const stmt_var = ir.idxToTypeVar(&ir.env.types, stmt_idx) catch |err| exitOnOom(err);

                switch (stmt) {
                    .s_alias_decl => |alias| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        tree.pushStaticAtom("alias");
                        const alias_region = ir.store.getStatementRegion(stmt_idx);
                        ir.appendRegionInfoToSExprTreeFromRegion(tree, alias_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        tree.pushStringPair("type", type_writer.get());
                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(alias.header);
                        header.pushToSExprTree(ir, tree, alias.header);

                        tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        tree.pushStaticAtom("nominal");
                        const nominal_region = ir.store.getStatementRegion(stmt_idx);
                        ir.appendRegionInfoToSExprTreeFromRegion(tree, nominal_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        tree.pushStringPair("type", type_writer.get());

                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(nominal.header);
                        header.pushToSExprTree(ir, tree, nominal.header);

                        tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },

                    else => {
                        // For non-assign patterns, we could handle destructuring, but for now skip
                        continue;
                    },
                }
            }

            tree.endNode(stmts_begin, stmts_attrs);
        }

        // Collect expression types (for significant expressions with regions)
        const exprs_begin = tree.beginNode();
        tree.pushStaticAtom("expressions");
        const exprs_attrs = tree.beginNode();

        for (all_defs) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Get the expression type
            const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(def.expr)));

            const expr_node_begin = tree.beginNode();
            tree.pushStaticAtom("expr");

            // Add region info for the expression
            const expr_region = ir.store.getExprRegion(def.expr);
            ir.appendRegionInfoToSExprTreeFromRegion(tree, expr_region);

            if (@intFromEnum(expr_var) > ir.env.types.slots.backing.len()) {
                const unknown_begin = tree.beginNode();
                tree.pushStaticAtom("unknown");
                const unknown_attrs = tree.beginNode();
                tree.endNode(unknown_begin, unknown_attrs);
            } else {
                // Clear the buffer and write the type
                try type_writer.write(expr_var);
                tree.pushStringPair("type", type_writer.get());
            }

            const expr_node_attrs = tree.beginNode();
            tree.endNode(expr_node_begin, expr_node_attrs);
        }

        tree.endNode(exprs_begin, exprs_attrs);

        tree.endNode(root_begin, attrs);
    }
}
