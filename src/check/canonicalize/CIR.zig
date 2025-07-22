//! The canonical intermediate representation (CIR) is a representation of the
//! canonicalized abstract syntax tree (AST) that is used for interpreting code generation and type checking, and later compilation stages.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const serialization = @import("serialization");
const types = @import("types");
const collections = @import("collections");
const tracy = @import("../../tracy.zig");
const reporting = @import("../../reporting.zig");
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

pub const RocDec = @import("builtins").RocDec;
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
/// Diagnostics extracted from the store (needed because getDiagnostics is destructive)
diagnostics: ?[]CIR.Diagnostic = null,
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
/// The allocator used for shared data (nodes, external_decls, etc)
/// This is needed for proper cleanup
shm: std.mem.Allocator,

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
pub fn init(env: *ModuleEnv, module_name: []const u8) std.mem.Allocator.Error!CIR {
    return initWithAllocator(env, module_name, env.gpa, env.gpa);
}

/// Initialize the IR with separate allocators for shared and scratch data.
///
/// Parameters:
/// - env: The module environment
/// - module_name: Name of the module
/// - shm: Allocator for persistent data (nodes, regions, extra_data)
/// - scratch_allocator: Allocator for temporary scratch arrays
pub fn initWithAllocator(
    env: *ModuleEnv,
    module_name: []const u8,
    shm: std.mem.Allocator,
    scratch_allocator: std.mem.Allocator,
) std.mem.Allocator.Error!CIR {
    return CIR{
        .env = env,
        .store = try NodeStore.initCapacityWithAllocators(
            shm,
            scratch_allocator,
            NODE_STORE_CAPACITY,
        ),
        .diagnostics = null,
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = try ExternalDecl.SafeList.initCapacity(shm, 16),
        .imports = Import.Store.init(),
        .module_name = module_name,
        .shm = shm,
    };
}

/// Create a CIR from cached data, completely rehydrating from cache
pub fn fromCache(env: *ModuleEnv, cached_store: NodeStore, all_defs: Def.Span, all_statements: Statement.Span, module_name: []const u8) CIR {
    return CIR{
        .env = env,
        .store = cached_store,
        .diagnostics = null,
        .all_defs = all_defs,
        .all_statements = all_statements,
        .external_decls = try ExternalDecl.SafeList.initCapacity(env.gpa, 16),
        .imports = Import.Store.init(),
        .module_name = module_name,
        .shm = env.gpa,
    };
}

fn literal_from_source(self: *CIR, start_offset: u32, end_offset: u32) []const u8 {
    if (self.env.source.len > 0 and end_offset <= self.env.source.len and start_offset <= end_offset) {
        return self.env.source[start_offset..end_offset];
    } else {
        return "";
    }
}

/// Deinit the IR's memory.
pub fn deinit(self: *CIR) void {
    self.store.deinit();
    self.external_decls.deinit(self.shm);
    self.imports.deinit(self.env.gpa);
    if (self.diagnostics) |diags| {
        self.env.gpa.free(diags);
    }
}

/// Records a diagnostic error during canonicalization without blocking compilation.
///
/// This creates a diagnostic node that stores error information for later reporting.
/// The diagnostic is added to the diagnostic collection but does not create any
/// malformed nodes in the IR.
///
/// Use this when you want to record an error but don't need to replace a node
/// with a runtime error.
pub fn pushDiagnostic(self: *CIR, reason: CIR.Diagnostic) std.mem.Allocator.Error!void {
    _ = try self.addDiagnosticAndTypeVar(reason, .err);
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
pub fn pushMalformed(self: *CIR, comptime RetIdx: type, reason: CIR.Diagnostic) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const diag_idx = try self.addDiagnosticAndTypeVar(reason, .err);
    const malformed_idx = try self.addMalformedAndTypeVar(diag_idx, .err, reason.toRegion());
    return castIdx(CIR.Node.Idx, RetIdx, malformed_idx);
}

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *CIR) std.mem.Allocator.Error![]CIR.Diagnostic {
    // Return diagnostics if already extracted
    if (self.diagnostics) |diags| {
        return diags;
    }

    // First time - compute and cache the diagnostics
    const all = try self.store.diagnosticSpanFrom(0);

    var list = std.ArrayList(CIR.Diagnostic).init(self.store.gpa);

    for (self.store.sliceDiagnostics(all)) |idx| {
        try list.append(self.store.getDiagnostic(idx));
    }

    self.diagnostics = try list.toOwnedSlice();
    return self.diagnostics.?;
}

/// Convert a canonicalization diagnostic to a Report for rendering.
///
/// The source parameter is not owned by this function - the caller must ensure it
/// remains valid for the duration of this call. The returned Report will contain
/// references to the source text but does not own it.
pub fn diagnosticToReport(self: *CIR, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    const trace = tracy.trace(@src());
    defer trace.end();

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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .invalid_num_literal => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const literal_text = self.literal_from_source(data.region.start.offset, data.region.end.offset);
            break :blk Diagnostic.buildInvalidNumLiteralReport(
                allocator,
                region_info,
                literal_text,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .ident_already_in_scope => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildIdentAlreadyInScopeReport(
                allocator,
                ident_name,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .f64_pattern_literal => |data| blk: {
            break :blk Diagnostic.buildF64PatternLiteralReport(
                allocator,
                data.region,
                self.env.source,
            );
        },
        .invalid_single_quote => Diagnostic.buildInvalidSingleQuoteReport(allocator),
        .crash_expects_string => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildCrashExpectsStringReport(
                allocator,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .empty_tuple => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildEmptyTupleReport(
                allocator,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .expr_not_canonicalized => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildExprNotCanonicalizedReport(
                allocator,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
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
        .malformed_where_clause => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildMalformedWhereClauseReport(
                allocator,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .too_many_exports => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildTooManyExportsReport(
                allocator,
                data.count,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk Diagnostic.buildUnusedVariableReport(
                allocator,
                &self.env.idents,
                region_info,
                data,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
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
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .unused_type_var_name => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const type_var_name = self.env.idents.getText(data.name);
            const suggested_name = self.env.idents.getText(data.suggested_name);
            break :blk try Diagnostic.buildUnusedTypeVarNameReport(
                allocator,
                type_var_name,
                suggested_name,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .type_var_marked_unused => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const type_var_name = self.env.idents.getText(data.name);
            const suggested_name = self.env.idents.getText(data.suggested_name);
            break :blk try Diagnostic.buildTypeVarMarkedUnusedReport(
                allocator,
                type_var_name,
                suggested_name,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .type_var_ending_in_underscore => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const type_var_name = self.env.idents.getText(data.name);
            const suggested_name = self.env.idents.getText(data.suggested_name);
            break :blk try Diagnostic.buildTypeVarEndingInUnderscoreReport(
                allocator,
                type_var_name,
                suggested_name,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
        .underscore_in_type_declaration => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            break :blk try Diagnostic.buildUnderscoreInTypeDeclarationReport(
                allocator,
                data.is_alias,
                region_info,
                filename,
                self.env.source,
                self.env.line_starts.items.items,
            );
        },
    };
}

// casting //

/// Convert a type into a node index
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Convert a type into a type var
pub fn varFrom(idx: anytype) TypeVar {
    return @enumFromInt(@intFromEnum(idx));
}

/// Cast between 2 idxs
/// Comptime asserts that these idxs are valid
pub inline fn castIdx(comptime FromIdx: type, comptime ToIdx: type, idx: FromIdx) ToIdx {
    comptime {
        if (!isCastable(FromIdx)) @compileError("FromIdx type " ++ @typeName(FromIdx) ++ " is not castable");
        if (!isCastable(ToIdx)) @compileError("ToIdx type " ++ @typeName(ToIdx) ++ " is not castable");
    }
    return @enumFromInt(@intFromEnum(idx));
}

/// Check if an Idx type is castable
pub inline fn isCastable(comptime Idx: type) bool {
    return Idx == CIR.Node.Idx or
        Idx == CIR.Def.Idx or
        Idx == CIR.Statement.Idx or
        Idx == CIR.Pattern.Idx or
        Idx == CIR.Expr.Idx or
        Idx == CIR.TypeAnno.Idx or
        Idx == CIR.RecordField.Idx or
        Idx == CIR.Expr.Match.Branch.Idx or
        Idx == CIR.WhereClause.Idx or
        Idx == CIR.TypeAnno.RecordField.Idx or
        Idx == CIR.ExposedItem.Idx or
        Idx == CIR.Annotation.Idx or
        Idx == Region.Idx or
        Idx == types.Var;
}

// debug assertions //

/// Assert that CIR, regions and types are all in sync
pub inline fn debugAssertArraysInSync(self: *const CIR) void {
    if (std.debug.runtime_safety) {
        const cir_nodes = self.store.nodes.len();
        const region_nodes = self.store.regions.len();
        const type_nodes = self.env.types.len();
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

// types //

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addDefAndTypeVar(self: *CIR, expr: CIR.Def, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("self", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeHeaderAndTypeVar(self: *CIR, expr: CIR.TypeHeader, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const expr_idx = try self.store.addTypeHeader(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeHeaderAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addStatementAndTypeVar(self: *CIR, expr: CIR.Statement, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Statement.Idx {
    const expr_idx = try self.store.addStatement(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addStatementAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addPatternAndTypeVar(self: *CIR, expr: CIR.Pattern, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addExprAndTypeVar(self: *CIR, expr: CIR.Expr, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addExprAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addRecordFieldAndTypeVar(self: *CIR, expr: CIR.RecordField, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addRecordDestructAndTypeVar(self: *CIR, expr: CIR.Pattern.RecordDestruct, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addRecordDestructorAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addIfBranchAndTypeVar(self: *CIR, expr: CIR.Expr.IfBranch, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addIfBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMatchBranchAndTypeVar(self: *CIR, expr: CIR.Expr.Match.Branch, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addWhereClauseAndTypeVar(self: *CIR, expr: CIR.WhereClause, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.WhereClause.Idx {
    const expr_idx = try self.store.addWhereClause(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addWhereClauseAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeAnnoAndTypeVar(self: *CIR, expr: CIR.TypeAnno, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeAnnoAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addAnnotationAndTypeVar(self: *CIR, expr: CIR.Annotation, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnotationAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addAnnoRecordFieldAndTypeVar(self: *CIR, expr: CIR.TypeAnno.RecordField, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addAnnoRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addExposedItemAndTypeVar(self: *CIR, expr: CIR.ExposedItem, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.ExposedItem.Idx {
    const expr_idx = try self.store.addExposedItem(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addExposedItemAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addDiagnosticAndTypeVar(self: *CIR, reason: CIR.Diagnostic, content: types.Content) std.mem.Allocator.Error!CIR.Diagnostic.Idx {
    const expr_idx = try self.store.addDiagnostic(reason);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addDiagnosticAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMalformedAndTypeVar(self: *CIR, diagnostic_idx: CIR.Diagnostic.Idx, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Node.Idx {
    const malformed_idx = try self.store.addMalformed(diagnostic_idx, region);
    const malformed_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addMalformedAndTypeVar", malformed_idx, malformed_var);
    self.debugAssertArraysInSync();
    return malformed_idx;
}

/// Add a new match branch pattern and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addMatchBranchPatternAndTypeVar(self: *CIR, expr: CIR.Expr.Match.BranchPattern, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addMatchBranchPatternAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern record field and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addPatternRecordFieldAndTypeVar(self: *CIR, expr: CIR.PatternRecordField, content: types.Content, region: base.Region) std.mem.Allocator.Error!CIR.PatternRecordField.Idx {
    const expr_idx = try self.store.addPatternRecordField(expr, region);
    const expr_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addPatternRecordFieldAndTypeVar", expr_idx, expr_var);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeSlotAndTypeVar(
    self: *CIR,
    parent_node: Node.Idx,
    content: types.Content,
    region: base.Region,
    comptime RetIdx: type,
) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const node_idx = try self.store.addTypeVarSlot(parent_node, region);
    const node_var = try self.env.types.freshFromContent(content);
    debugAssertIdxsEql("addTypeSlotAndTypeVar", node_idx, node_var);
    self.debugAssertArraysInSync();
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Add a new expression and type variable.
/// This function asserts that the types array and the CIR nodes are in sync.
pub fn addTypeSlotAndTypeVarRedirect(
    self: *CIR,
    parent_node: Node.Idx,
    redirect_to: TypeVar,
    region: base.Region,
    comptime RetIdx: type,
) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const node_idx = try self.store.addTypeVarSlot(parent_node, region);
    const node_var = try self.env.types.freshRedirect(redirect_to);
    debugAssertIdxsEql("addTypeSlotAndTypeVarRedirect", node_idx, node_var);
    self.debugAssertArraysInSync();
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Function that redirects an existing node to the provided var.
/// Assert that the requested idx in in bounds
pub fn redirectTypeTo(
    self: *CIR,
    comptime FromIdx: type,
    at_idx: FromIdx,
    redirect_to: types.Var,
) std.mem.Allocator.Error!void {
    comptime if (!isCastable(FromIdx)) @compileError("Idx type " ++ @typeName(FromIdx) ++ " is not castable");
    self.debugAssertArraysInSync();
    std.debug.assert(@intFromEnum(at_idx) < self.env.types.len());

    const var_ = varFrom(at_idx);
    try self.env.types.setVarRedirect(var_, redirect_to);
}

// external decls //

/// Adds an external declaration to the CIR and returns its index
pub fn pushExternalDecl(self: *CIR, decl: ExternalDecl) std.mem.Allocator.Error!ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = try self.external_decls.append(self.env.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const CIR, idx: ExternalDecl.Idx) *const ExternalDecl {
    return self.external_decls.get(@as(ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *CIR, decls: []const ExternalDecl) std.mem.Allocator.Error!ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = try self.external_decls.append(self.env.gpa, decl);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("field");
        try tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();
        try ir.store.getExpr(self.value).pushToSExprTree(ir, tree, self.value);
        try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, where_idx: WhereClause.Idx) std.mem.Allocator.Error!void {
        switch (self.*) {
            .mod_method => |mm| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("module-of", ir.env.idents.getText(mm.var_name));
                try tree.pushStringPair("ident", ir.env.idents.getText(mm.method_name));
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                for (ir.store.sliceTypeAnnos(mm.args)) |arg_idx| {
                    try ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
                }
                try tree.endNode(args_begin, attrs2);

                try ir.store.getTypeAnno(mm.ret_anno).pushToSExprTree(ir, tree, mm.ret_anno);
                try tree.endNode(begin, attrs);
            },
            .mod_alias => |ma| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                try tree.pushStringPair("module-of", ir.env.idents.getText(ma.var_name));
                try tree.pushStringPair("ident", ir.env.idents.getText(ma.alias_name));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .malformed => |_| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");
                const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(where_idx)));
                try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, header_idx: TypeHeader.Idx) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ty-header");
        const region = ir.store.getRegionAt(@enumFromInt(@intFromEnum(header_idx)));
        try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
        try tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("ty-args");
            const attrs2 = tree.beginNode();
            for (ir.store.sliceTypeAnnos(self.args)) |arg_idx| {
                try ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree, arg_idx);
            }
            try tree.endNode(args_begin, attrs2);
        }

        try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: ExposedItem, env: *base.ModuleEnv, ir: *const CIR, tree: *SExprTree) std.mem.Allocator.Error!void {
        _ = ir; // Unused in this function, but could be used for more complex logic

        const begin = tree.beginNode();
        try tree.pushStaticAtom("exposed");
        try tree.pushStringPair("name", env.idents.getText(self.name));

        if (self.alias) |alias_idx| {
            try tree.pushStringPair("alias", env.idents.getText(alias_idx));
        }

        try tree.pushBoolPair("wildcard", self.is_wildcard);
        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
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
            const gop = try self.map.getOrPutContext(gpa, module_name, std.hash_map.StringContext{});
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ingested-file");
        try tree.pushStringPair("path", ir.env.strings.get(self.relative_path));
        try tree.pushStringPair("ident", ir.env.idents.getText(self.ident));

        const attrs = tree.beginNode();
        try self.type.pushToSExprTree(ir, tree);
        try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        try tree.pushStaticAtom(name);
        const attrs = tree.beginNode();

        try ir.store.getPattern(self.pattern).pushToSExprTree(ir, tree, self.pattern);

        try ir.store.getExpr(self.expr).pushToSExprTree(ir, tree, self.expr);

        if (self.annotation) |anno_idx| {
            const anno = ir.store.getAnnotation(anno_idx);
            try anno.pushToSExprTree(ir, tree, anno_idx);
        }

        try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree, anno_idx: Annotation.Idx) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");
        const region = ir.store.getRegionAt(@enumFromInt(@intFromEnum(anno_idx)));
        try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
        const attrs = tree.beginNode();

        // Add the declared type annotation structure
        const type_begin = tree.beginNode();
        try tree.pushStaticAtom("declared-type");
        const type_attrs = tree.beginNode();
        try ir.store.getTypeAnno(self.type_anno).pushToSExprTree(ir, tree, self.type_anno);
        try tree.endNode(type_begin, type_attrs);

        try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try ir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        try tree.pushStringPair("ident", ir.env.idents.getText(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const @This(), ir: *const CIR, tree: *SExprTree, region: Region) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        try tree.pushStringPair("ident", ir.env.idents.getText(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
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
        try self.named.append(gpa, named_var);
    }

    /// Insert a wildcard type variable
    pub fn insertWildcard(self: *IntroducedVariables, gpa: std.mem.Allocator, var_type: TypeVar) std.mem.Allocator.Error!void {
        try self.wildcards.append(gpa, var_type);
    }

    /// Insert an inferred type variable
    pub fn insertInferred(self: *IntroducedVariables, gpa: std.mem.Allocator, var_type: TypeVar) std.mem.Allocator.Error!void {
        try self.inferred.append(gpa, var_type);
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
    pub fn unionWith(self: *IntroducedVariables, other: *const IntroducedVariables) std.mem.Allocator.Error!void {
        // This is a simplified union - in practice we'd want to avoid duplicates
        // For now, just append all items
        const gpa = std.heap.page_allocator; // TODO: pass proper allocator

        try self.named.appendSlice(gpa, other.named.items);
        try self.wildcards.appendSlice(gpa, other.wildcards.items);
        try self.inferred.appendSlice(gpa, other.inferred.items);
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
    pub fn insertValueLookup(self: *References, gpa: std.mem.Allocator, symbol: Ident.Idx) std.mem.Allocator.Error!void {
        try self.value_lookups.append(gpa, symbol);
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

    pub fn toI128(self: IntValue) i128 {
        switch (self.kind) {
            .i128 => {
                // Convert little-endian bytes to i128
                var result: i128 = 0;
                var i: usize = 0;
                while (i < 16) : (i += 1) {
                    result |= @as(i128, self.bytes[i]) << @intCast(i * 8);
                }
                return result;
            },
            .u128 => {
                // Convert little-endian bytes to u128, then cast to i128
                var result: u128 = 0;
                var i: usize = 0;
                while (i < 16) : (i += 1) {
                    result |= @as(u128, self.bytes[i]) << @intCast(i * 8);
                }
                return @intCast(result);
            },
        }
    }
};

/// Helper function to generate the S-expression node for the entire Canonical IR.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        try ir.store.getExpr(expr_idx).pushToSExprTree(ir, tree, expr_idx);
    } else {
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression tree
        const defs_slice = ir.store.sliceDefs(ir.all_defs);
        const statements_slice = ir.store.sliceStatements(ir.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and ir.external_decls.len() == 0) {
            try tree.pushBoolPair("empty", true);
        }
        const attrs = tree.beginNode();

        for (defs_slice) |def_idx| {
            try ir.store.getDef(def_idx).pushToSExprTree(ir, tree);
        }

        for (statements_slice) |stmt_idx| {
            try ir.store.getStatement(stmt_idx).pushToSExprTree(ir, tree, stmt_idx);
        }

        for (0..ir.external_decls.len()) |i| {
            const external_decl = ir.external_decls.get(@enumFromInt(i));
            try external_decl.pushToSExprTree(ir, tree);
        }

        try tree.endNode(root_begin, attrs);
    }
}

test "NodeStore - init and deinit" {
    var store = try CIR.NodeStore.init(testing.allocator);
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
    };

    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const source = self.env.source;

    const info = base.RegionInfo.position(source, self.env.line_starts.items.items, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSexprNode(ir: *const CIR, node: *SExpr, idx: anytype) std.mem.Allocator.Error!void {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    try ir.appendRegionInfoToSexprNodeFromRegion(node, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSexprNodeFromRegion(ir: *const CIR, node: *SExpr, region: Region) std.mem.Allocator.Error!void {
    const info = ir.calcRegionInfo(region);
    try node.appendByteRange(
        ir.env.gpa,
        info,
        region.start.offset,
        region.end.offset,
    );
}

/// Append region information to an S-expression node for a given index in the Canonical IR.
pub fn appendRegionInfoToSExprTree(ir: *const CIR, tree: *SExprTree, idx: anytype) std.mem.Allocator.Error!void {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    try ir.appendRegionInfoToSExprTreeFromRegion(tree, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSExprTreeFromRegion(ir: *const CIR, tree: *SExprTree, region: Region) std.mem.Allocator.Error!void {
    const info = ir.calcRegionInfo(region);
    try tree.pushBytesRange(
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
pub fn pushTypesToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    const gpa = ir.env.gpa;

    // Create TypeWriter for converting types to strings
    var type_writer = try types.writers.TypeWriter.init(gpa, ir.env);
    defer type_writer.deinit();

    if (maybe_expr_idx) |expr_idx| {
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

        const expr_begin = tree.beginNode();
        try tree.pushStaticAtom("expr");

        try ir.appendRegionInfoToSExprTree(tree, expr_idx);

        if (@intFromEnum(expr_var) > ir.env.types.slots.backing.len()) {
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

        const all_defs = ir.store.sliceDefs(ir.all_defs);

        for (all_defs) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Extract identifier name from the pattern (assuming it's an assign pattern)
            const pattern = ir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |_| {
                    const patt_begin = tree.beginNode();
                    try tree.pushStaticAtom("patt");

                    // Get the pattern region instead of the whole def region
                    const pattern_region = ir.store.getPatternRegion(def.pattern);
                    try ir.appendRegionInfoToSExprTreeFromRegion(tree, pattern_region);

                    // Get the type variable for this definition
                    const def_var = castIdx(CIR.Def.Idx, TypeVar, def_idx);

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
            try tree.pushStaticAtom("type_decls");
            const stmts_attrs = tree.beginNode();

            for (all_stmts) |stmt_idx| {
                const stmt = ir.store.getStatement(stmt_idx);

                // Get the type variable for this definition
                const stmt_var = castIdx(CIR.Statement.Idx, TypeVar, stmt_idx);

                switch (stmt) {
                    .s_alias_decl => |alias| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        try tree.pushStaticAtom("alias");
                        const alias_region = ir.store.getStatementRegion(stmt_idx);
                        try ir.appendRegionInfoToSExprTreeFromRegion(tree, alias_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        try tree.pushStringPair("type", type_writer.get());
                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(alias.header);
                        try header.pushToSExprTree(ir, tree, alias.header);

                        try tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        try tree.pushStaticAtom("nominal");
                        const nominal_region = ir.store.getStatementRegion(stmt_idx);
                        try ir.appendRegionInfoToSExprTreeFromRegion(tree, nominal_region);

                        // Clear the buffer and write the type
                        try type_writer.write(stmt_var);
                        try tree.pushStringPair("type", type_writer.get());

                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(nominal.header);
                        try header.pushToSExprTree(ir, tree, nominal.header);

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
            const def = ir.store.getDef(def_idx);

            // Get the expression type
            const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(def.expr)));

            const expr_node_begin = tree.beginNode();
            try tree.pushStaticAtom("expr");

            // Add region info for the expression
            const expr_region = ir.store.getExprRegion(def.expr);
            try ir.appendRegionInfoToSExprTreeFromRegion(tree, expr_region);

            if (@intFromEnum(expr_var) > ir.env.types.slots.backing.len()) {
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
