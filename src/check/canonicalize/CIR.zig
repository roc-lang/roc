//! The canonical intermediate representation (CIR) is a representation of the
//! canonicalized abstract syntax tree (AST) that is used for interpreting code generation and type checking, and later compilation stages.

const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const reporting = @import("../../reporting.zig");
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
const TypeVar = types.Var;
const RocDec = @import("../../builtins/dec.zig").RocDec;
const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");

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
external_decls: std.ArrayList(ExternalDecl),

/// Initialize the IR for a module's canonicalization info.
///
/// When caching the can IR for a siloed module, we can avoid
/// manual deserialization of the cached data into IR by putting
/// the entirety of the IR into an arena that holds nothing besides
/// the IR. We can then load the cached binary data back into memory
/// with only 2 syscalls.
///
/// Since the can IR holds indices into the `ModuleEnv`, we need
/// the `ModuleEnv` to also be owned by the can IR to cache it.
///
/// Takes ownership of the module_env
pub fn init(env: *ModuleEnv) CIR {
    return CIR{
        .env = env,
        .store = NodeStore.initCapacity(env.gpa, NODE_STORE_CAPACITY),
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = std.ArrayList(ExternalDecl).init(env.gpa),
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *CIR) void {
    self.store.deinit();
    self.external_decls.deinit();
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

    var list = std.ArrayList(CIR.Diagnostic).init(self.env.gpa);

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
    // Set temporary source for calcRegionInfo
    self.temp_source_for_sexpr = source;
    defer self.temp_source_for_sexpr = null;

    return switch (diagnostic) {
        .not_implemented => |data| blk: {
            const feature_text = self.env.strings.get(data.feature);
            break :blk Diagnostic.buildNotImplementedReport(allocator, feature_text);
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
            break :blk Diagnostic.buildIdentNotInScopeReport(
                allocator,
                ident_name,
            );
        },
        .invalid_top_level_statement => |data| blk: {
            const stmt_name = self.env.strings.get(data.stmt);
            break :blk Diagnostic.buildInvalidTopLevelStatementReport(
                allocator,
                stmt_name,
            );
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
        .shadowing_warning => |data| blk: {
            const ident_name = self.env.idents.getText(data.ident);
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk Diagnostic.buildShadowingWarningReport(
                allocator,
                ident_name,
                new_region_info,
                original_region_info,
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
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
                source,
                filename,
            );
        },
    };
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

/// Associates a type with an existing definition node.
///
/// Use this to set the concrete type of a definition after type inference.
pub fn setTypeVarAtDef(self: *CIR, at_idx: Def.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
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

/// Core function that associates a type with any existing CIR node.
///
/// This is used by all the `setTypeVarAt*` wrapper functions. Node indices
/// correspond directly to type variable indices, allowing direct conversion.
/// Usually called indirectly through the typed wrappers rather than directly.
///
/// **Note**: The node must already exist - this only sets types, not create nodes.
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
    const idx = @as(u32, @intCast(self.external_decls.items.len));
    self.external_decls.append(decl) catch |err| exitOnOom(err);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const CIR, idx: ExternalDecl.Idx) *const ExternalDecl {
    return &self.external_decls.items[@intFromEnum(idx)];
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *CIR, decls: []const ExternalDecl) ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.items.len));
    for (decls) |decl| {
        self.external_decls.append(decl) catch |err| exitOnOom(err);
    }
    return .{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const CIR, span: ExternalDecl.Span) []const ExternalDecl {
    return self.external_decls.items[span.span.start .. span.span.start + span.span.len];
}

fn getIdentText(self: *const CIR, idx: Ident.Idx) []const u8 {
    return self.env.idents.getText(idx);
}

// Helper to format pattern index for s-expr output
fn formatPatternIdxNode(gpa: std.mem.Allocator, pattern_idx: Pattern.Idx) SExpr {
    var node = SExpr.init(gpa, "pid");
    node.appendUnsignedInt(gpa, @intFromEnum(pattern_idx));
    return node;
}

test "Node is 24 bytes" {
    try testing.expectEqual(24, @sizeOf(Node));
}

/// A single statement - either at the top-level or within a block.
pub const Statement = union(enum) {
    /// A simple immutable declaration
    s_decl: struct {
        pattern: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// A rebindable declaration using the "var" keyword
    /// Not valid at the top level of a module
    s_var: struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// Reassignment of a previously declared var
    /// Not valid at the top level of a module
    s_reassign: struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// The "crash" keyword instruct a runtime crash with message
    ///
    /// Not valid at the top level of a module
    s_crash: struct {
        msg: StringLiteral.Idx,
        region: Region,
    },
    /// Just an expression - usually the return value for a block
    ///
    /// Not valid at the top level of a module
    s_expr: struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// An expression that will cause a panic (or some other error handling mechanism) if it evaluates to false
    s_expect: struct {
        body: Expr.Idx,
        region: Region,
    },
    /// A block of code that will be ran multiple times for each item in a list.
    ///
    /// Not valid at the top level of a module
    s_for: struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: Region,
    },
    /// A early return of the enclosing function.
    ///
    /// Not valid at the top level of a module
    s_return: struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// Brings in another module for use in the current module, optionally exposing only certain members of that module.
    ///
    /// Only valid at the top level of a module
    s_import: struct {
        module_name_tok: Ident.Idx,
        qualifier_tok: ?Ident.Idx,
        alias_tok: ?Ident.Idx,
        exposes: ExposedItem.Span,
        region: Region,
    },
    /// A declaration of a new type - whether an alias or a new nominal nominal type
    ///
    /// Only valid at the top level of a module
    s_type_decl: struct {
        header: TypeHeader.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    },
    /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
    s_type_anno: struct {
        name: Ident.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .s_decl => |d| {
                var node = SExpr.init(gpa, "s-let");
                node.appendRegion(gpa, ir.calcRegionInfo(d.region));

                var pattern_node = ir.store.getPattern(d.pattern).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(d.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_var => |v| {
                var node = SExpr.init(gpa, "s-var");
                node.appendRegion(gpa, ir.calcRegionInfo(v.region));

                var pattern_node = ir.store.getPattern(v.pattern_idx).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(v.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_reassign => |r| {
                var node = SExpr.init(gpa, "s-reassign");
                node.appendRegion(gpa, ir.calcRegionInfo(r.region));

                var pattern_node = ir.store.getPattern(r.pattern_idx).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(r.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_crash => |c| {
                var node = SExpr.init(gpa, "s-crash");
                node.appendRegion(gpa, ir.calcRegionInfo(c.region));
                node.appendStringAttr(gpa, "msg", ir.env.strings.get(c.msg));
                return node;
            },
            .s_expr => |s| {
                var node = SExpr.init(gpa, "s-expr");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_expect => |s| {
                var node = SExpr.init(gpa, "s-expect");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var body_node = ir.store.getExpr(s.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .s_for => |s| {
                var node = SExpr.init(gpa, "s-for");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var pattern_node = ir.store.getPattern(s.patt).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                var body_node = ir.store.getExpr(s.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .s_return => |s| {
                var node = SExpr.init(gpa, "s-return");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .s_import => |s| {
                var node = SExpr.init(gpa, "s-import");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "module", ir.env.idents.getText(s.module_name_tok));

                if (s.qualifier_tok) |qualifier| {
                    node.appendStringAttr(gpa, "qualifier", ir.getIdentText(qualifier));
                }

                if (s.alias_tok) |alias| {
                    node.appendStringAttr(gpa, "alias", ir.getIdentText(alias));
                }

                var exposes_node = SExpr.init(gpa, "exposes");
                const exposes_slice = ir.store.sliceExposedItems(s.exposes);
                for (exposes_slice) |exposed_idx| {
                    const exposed_sexpr = ir.store.getExposedItem(exposed_idx).toSExpr(ir);
                    exposes_node.appendNode(gpa, &exposed_sexpr);
                }
                node.appendNode(gpa, &exposes_node);

                return node;
            },
            .s_type_decl => |s| {
                var node = SExpr.init(gpa, "s-type-decl");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                // Add the type header
                var header_node = ir.store.getTypeHeader(s.header).toSExpr(ir);
                node.appendNode(gpa, &header_node);

                // Add the type annotation
                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir);
                node.appendNode(gpa, &anno_node);

                // TODO: Add where clause when implemented
                if (s.where) |_| {
                    node.appendStringAttr(gpa, "match", "TODO");
                }

                return node;
            },
            .s_type_anno => |s| {
                var node = SExpr.init(gpa, "s-type-anno");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "name", ir.getIdentText(s.name));

                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir);
                node.appendNode(gpa, &anno_node);

                if (s.where) |_| {
                    var where_node = SExpr.init(gpa, "match");
                    node.appendNode(gpa, &where_node);
                }

                return node;
            },
        }
    }

    /// Extract the region from any Statement variant
    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .s_decl => |s| return s.region,
            .s_var => |s| return s.region,
            .s_reassign => |s| return s.region,
            .s_crash => |s| return s.region,
            .s_expr => |s| return s.region,
            .s_expect => |s| return s.region,
            .s_for => |s| return s.region,
            .s_return => |s| return s.region,
            .s_import => |s| return s.region,
            .s_type_decl => |s| return s.region,
            .s_type_anno => |s| return s.region,
        }
    }
};

/// A working representation of a record field
pub const RecordField = struct {
    name: Ident.Idx,
    value: Expr.Idx,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "field");

        node.appendStringAttr(gpa, "name", ir.getIdentText(self.name));

        var value_node = ir.store.getExpr(self.value).toSExpr(ir);
        node.appendNode(gpa, &value_node);

        return node;
    }
};

/// TODO: implement WhereClause
pub const WhereClause = union(enum) {
    alias: WhereClause.Alias,
    method: Method,
    mod_method: ModuleMethod,

    pub const Alias = struct {
        var_tok: Ident.Idx,
        alias_tok: Ident.Idx,
        region: Region,
    };
    pub const Method = struct {
        var_tok: Ident.Idx,
        name_tok: Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Idx,
        region: Region,
    };
    pub const ModuleMethod = struct {
        var_tok: Ident.Idx,
        name_tok: Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Span,
        region: Region,
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement PatternRecordField
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// Canonical representation of type annotations in Roc.
///
/// Type annotations appear on the right-hand side of type declarations and in other
/// contexts where types are specified. For example, in `Map(a, b) : List(a) -> List(b)`,
/// the `List(a) -> List(b)` part is represented by these TypeAnno variants.
pub const TypeAnno = union(enum) {
    /// Type application: applying a type constructor to arguments.
    /// Examples: `List(Str)`, `Dict(String, Int)`, `Result(a, b)`
    apply: struct {
        symbol: Ident.Idx, // The type constructor being applied (e.g., "List", "Dict")
        args: TypeAnno.Span, // The type arguments (e.g., [Str], [String, Int])
        region: Region,
    },

    /// Type variable: a placeholder type that can be unified with other types.
    /// Examples: `a`, `b`, `elem` in generic type signatures
    ty_var: struct {
        name: Ident.Idx, // The variable name (e.g., "a", "b")
        region: Region,
    },

    /// Inferred type `_`
    underscore: struct {
        region: Region,
    },

    /// Basic type identifier: a concrete type name without arguments.
    /// Examples: `Str`, `U64`, `Bool`
    ty: struct {
        symbol: Ident.Idx, // The type name
        region: Region,
    },

    /// Module-qualified type: a type name prefixed with its module.
    /// Examples: `Shape.Rect`, `Json.Decoder`
    mod_ty: struct {
        mod_symbol: Ident.Idx, // The module name (e.g., "Json")
        ty_symbol: Ident.Idx, // The type name (e.g., "Decoder")
        region: Region,
    },

    /// Tag union type: a union of tags, possibly with payloads.
    /// Examples: `[Some(a), None]`, `[Red, Green, Blue]`, `[Cons(a, (List a)), Nil]`
    tag_union: struct {
        tags: TypeAnno.Span, // The individual tags in the union
        open_anno: ?TypeAnno.Idx, // Optional extension variable for open unions
        region: Region,
    },

    /// Tuple type: a fixed-size collection of heterogeneous types.
    /// Examples: `(Str, U64)`, `(a, b, c)`
    tuple: struct {
        annos: TypeAnno.Span, // The types of each tuple element
        region: Region,
    },

    /// Record type: a collection of named fields with their types.
    /// Examples: `{ name: Str, age: U64 }`, `{ x: F64, y: F64 }`
    record: struct {
        fields: AnnoRecordField.Span, // The field definitions
        region: Region,
    },

    /// Function type: represents function signatures.
    /// Examples: `a -> b`, `Str, U64 -> Str`, `{} => Str`
    @"fn": struct {
        args: TypeAnno.Span, // Argument types
        ret: TypeAnno.Idx, // Return type
        effectful: bool, // Whether the function can perform effects, i.e. uses fat arrow `=>`
        region: Region,
    },

    /// Parenthesized type: used for grouping and precedence.
    /// Examples: `(a -> b)` in `a, (a -> b) -> b`
    parens: struct {
        anno: TypeAnno.Idx, // The type inside the parentheses
        region: Region,
    },

    /// Malformed type annotation: represents a type that couldn't be parsed correctly.
    /// This follows the "Inform Don't Block" principle - compilation continues with
    /// an error marker that will be reported to the user.
    malformed: struct {
        diagnostic: Diagnostic.Idx, // The error that occurred
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .apply => |a| {
                var node = SExpr.init(gpa, "ty-apply");
                node.appendRegion(gpa, ir.calcRegionInfo(a.region));

                node.appendStringAttr(gpa, "symbol", ir.getIdentText(a.symbol));

                const args_slice = ir.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_node = arg.toSExpr(ir);
                    node.appendNode(gpa, &arg_node);
                }

                return node;
            },
            .ty_var => |tv| {
                var node = SExpr.init(gpa, "ty-var");
                node.appendRegion(gpa, ir.calcRegionInfo(tv.region));
                node.appendStringAttr(gpa, "name", ir.getIdentText(tv.name));
                return node;
            },
            .underscore => |u| {
                var node = SExpr.init(gpa, "ty-underscore");
                node.appendRegion(gpa, ir.calcRegionInfo(u.region));
                return node;
            },
            .ty => |t| {
                var node = SExpr.init(gpa, "ty");
                node.appendRegion(gpa, ir.calcRegionInfo(t.region));
                node.appendStringAttr(gpa, "name", ir.getIdentText(t.symbol));
                return node;
            },
            .mod_ty => |mt| {
                var node = SExpr.init(gpa, "ty-mod");
                node.appendRegion(gpa, ir.calcRegionInfo(mt.region));
                node.appendStringAttr(gpa, "module", ir.getIdentText(mt.mod_symbol));
                node.appendStringAttr(gpa, "type", ir.getIdentText(mt.ty_symbol));
                return node;
            },
            .tag_union => |tu| {
                var node = SExpr.init(gpa, "ty-tag-union");
                node.appendRegion(gpa, ir.calcRegionInfo(tu.region));

                const tags_slice = ir.store.sliceTypeAnnos(tu.tags);
                for (tags_slice) |tag_idx| {
                    const tag = ir.store.getTypeAnno(tag_idx);
                    var tag_node = tag.toSExpr(ir);
                    node.appendNode(gpa, &tag_node);
                }

                if (tu.open_anno) |open_idx| {
                    const open_anno = ir.store.getTypeAnno(open_idx);
                    var open_node = open_anno.toSExpr(ir);
                    node.appendNode(gpa, &open_node);
                }

                return node;
            },
            .tuple => |tup| {
                var node = SExpr.init(gpa, "ty-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(tup.region));

                const annos_slice = ir.store.sliceTypeAnnos(tup.annos);
                for (annos_slice) |anno_idx| {
                    const anno = ir.store.getTypeAnno(anno_idx);
                    var anno_node = anno.toSExpr(ir);
                    node.appendNode(gpa, &anno_node);
                }

                return node;
            },
            .record => |r| {
                var node = SExpr.init(gpa, "ty-record");
                node.appendRegion(gpa, ir.calcRegionInfo(r.region));

                const fields_slice = ir.store.sliceAnnoRecordFields(r.fields);
                for (fields_slice) |field_idx| {
                    const field = ir.store.getAnnoRecordField(field_idx);
                    var field_node = SExpr.init(gpa, "field");
                    field_node.appendStringAttr(gpa, "field", ir.getIdentText(field.name));

                    var type_node = ir.store.getTypeAnno(field.ty).toSExpr(ir);
                    field_node.appendNode(gpa, &type_node);

                    node.appendNode(gpa, &field_node);
                }

                return node;
            },
            .@"fn" => |f| {
                var node = SExpr.init(gpa, "ty-fn");
                node.appendRegion(gpa, ir.calcRegionInfo(f.region));

                const args_slice = ir.store.sliceTypeAnnos(f.args);
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_node = arg.toSExpr(ir);
                    node.appendNode(gpa, &arg_node);
                }

                var ret_node = ir.store.getTypeAnno(f.ret).toSExpr(ir);
                node.appendNode(gpa, &ret_node);

                node.appendBoolAttr(gpa, "effectful", f.effectful);

                return node;
            },
            .parens => |p| {
                var node = SExpr.init(gpa, "ty-parens");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const inner_anno = ir.store.getTypeAnno(p.anno);
                var inner_node = inner_anno.toSExpr(ir);
                node.appendNode(gpa, &inner_node);

                return node;
            },
            .malformed => |m| {
                var node = SExpr.init(gpa, "ty-malformed");
                node.appendRegion(gpa, ir.calcRegionInfo(m.region));
                return node;
            },
        }
    }

    /// Extract the region from any TypeAnno variant
    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .apply => |a| return a.region,
            .ty_var => |tv| return tv.region,
            .underscore => |u| return u.region,
            .ty => |t| return t.region,
            .mod_ty => |mt| return mt.region,
            .tuple => |t| return t.region,
            .tag_union => |tu| return tu.region,
            .record => |r| return r.region,
            .@"fn" => |f| return f.region,
            .parens => |p| return p.region,
            .malformed => |m| return m.region,
        }
    }
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
    region: Region, // Source location of the entire header

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "ty-header");
        node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        // Add the type name
        node.appendStringAttr(gpa, "name", ir.getIdentText(self.name));

        // Add the type arguments
        const args_slice = ir.store.sliceTypeAnnos(self.args);
        if (args_slice.len > 0) {
            var args_node = SExpr.init(gpa, "ty-args");
            for (args_slice) |arg_idx| {
                const arg = ir.store.getTypeAnno(arg_idx);
                var arg_node = arg.toSExpr(ir);
                args_node.appendNode(gpa, &arg_node);
            }
            node.appendNode(gpa, &args_node);
        }

        return node;
    }
};

/// Record field in a type annotation: `{ field_name: Type }`
pub const AnnoRecordField = struct {
    name: Ident.Idx,
    ty: TypeAnno.Idx,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
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

    pub fn toSExpr(self: ExposedItem, ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        var node = SExpr.init(gpa, "exposed");

        // Add the original name
        node.appendStringAttr(gpa, "name", ir.env.idents.getText(self.name));

        // Add the alias if present
        if (self.alias) |alias_idx| {
            node.appendStringAttr(gpa, "alias", ir.env.idents.getText(alias_idx));
        }

        // Add wildcard indicator if needed
        node.appendBoolAttr(gpa, "wildcard", self.is_wildcard);

        return node;
    }
};

/// An expression that has been canonicalized.
pub const Expr = union(enum) {
    e_num: struct {
        value: IntValue,
        region: Region,
    },
    e_int: struct {
        value: IntValue,
        region: Region,
    },
    e_frac_f64: struct {
        value: f64,
        region: Region,
    },
    e_frac_dec: struct {
        value: RocDec,
        region: Region,
    },
    e_dec_small: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        region: Region,
    },
    // A single segment of a string literal
    // a single string may be made up of a span sequential segments
    // for example if it was split across multiple lines
    e_str_segment: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    // A string is combined of one or more segments, some of which may be interpolated
    // An interpolated string contains one or more non-string_segment's in the span
    e_str: struct {
        span: Expr.Span,
        region: Region,
    },
    e_lookup: union(enum) {
        local: Lookup,
        external: ExternalDecl.Idx,
    },
    e_list: struct {
        elem_var: TypeVar,
        elems: Expr.Span,
        region: Region,
    },
    /// Empty list constant
    e_empty_list: struct {
        region: Region,
    },
    e_tuple: struct {
        elems: Expr.Span,
        region: Region,
    },
    e_match: Match,
    e_if: If,
    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    e_call: struct {
        args: Expr.Span,
        called_via: CalledVia,
        /// Tracks if this function is pure or effectful during type checking
        effect_var: TypeVar,
        region: Region,
    },
    e_record: struct {
        fields: RecordField.Span,
        region: Region,
    },
    /// Empty record constant
    e_empty_record: struct {
        region: Region,
    },
    e_block: struct {
        /// Statements executed in sequence
        stmts: Statement.Span,
        /// Final expression that produces the block's value
        final_expr: Expr.Idx,
        region: Region,
    },
    e_record_access: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: Expr.Idx,
        field: Ident.Idx,
        region: Region,
    },
    e_tag: struct {
        ext_var: TypeVar,
        name: Ident.Idx,
        args: Expr.Span,
        region: Region,
    },
    e_zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
        region: Region,
    },
    e_lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        /// Tracks if this function is pure or effectful during type checking
        effect_var: TypeVar,
        region: Region,
    },
    e_binop: Binop,
    /// Dot access that could be either record field access or static dispatch
    /// The decision is deferred until after type inference based on the receiver's type
    e_dot_access: struct {
        receiver: Expr.Idx, // Expression before the dot (e.g., `list` in `list.map`)
        field_name: Ident.Idx, // Identifier after the dot (e.g., `map` in `list.map`)
        args: ?Expr.Span, // Optional arguments for method calls (e.g., `fn` in `list.map(fn)`)
        region: Region,
    },
    /// Compiles, but will crash if reached
    e_runtime_error: struct {
        diagnostic: Diagnostic.Idx,
        region: Region,
    },

    pub const Lookup = struct {
        pattern_idx: Pattern.Idx,
        region: Region,
    };

    pub const Idx = enum(u32) { _ };

    pub const Span = struct { span: DataSpan };

    pub fn initStr(expr_span: Expr.Span, region: Region) Expr {
        return CIR.Expr{
            .e_str = .{
                .span = expr_span,
                .region = region,
            },
        };
    }

    pub fn initStrSegment(literal: StringLiteral.Idx, region: Region) Expr {
        return CIR.Expr{
            .e_str_segment = .{
                .literal = literal,
                .region = region,
            },
        };
    }

    pub const Binop = struct {
        op: Op,
        lhs: Expr.Idx,
        rhs: Expr.Idx,
        region: Region,

        pub const Op = enum {
            add,
            sub,
            mul,
            div,
            rem,
            lt,
            gt,
            le,
            ge,
            eq,
            ne,
            pow,
            div_trunc,
            @"and",
            @"or",
            pipe_forward,
            null_coalesce,
        };

        pub fn init(op: Op, lhs: Expr.Idx, rhs: Expr.Idx, region: Region) Binop {
            return .{ .lhs = lhs, .op = op, .rhs = rhs, .region = region };
        }
    };

    pub fn toRegion(self: *const @This()) ?Region {
        switch (self.*) {
            .e_num => |e| return e.region,
            .e_int => |e| return e.region,
            .e_frac_f64 => |e| return e.region,
            .e_frac_dec => |e| return e.region,
            .e_dec_small => |e| return e.region,
            .e_str_segment => |e| return e.region,
            .e_str => |e| return e.region,
            .e_lookup => |e| switch (e) {
                .local => |local| return local.region,
                .external => |_| {
                    // External lookups don't have a direct region access from Expr context
                    // The region should be handled where the CIR context is available
                    return null;
                },
            },
            .e_list => |e| return e.region,
            .e_tuple => |e| return e.region,
            .e_match => |e| return e.region,
            .e_if => |e| return e.region,
            .e_empty_list => |e| return e.region,
            .e_call => |e| return e.region,
            .e_record => |e| return e.region,
            .e_empty_record => |e| return e.region,
            .e_record_access => |e| return e.region,
            .e_dot_access => |e| return e.region,
            .e_tag => |e| return e.region,
            .e_zero_argument_tag => |e| return e.region,
            .e_binop => |e| return e.region,
            .e_block => |e| return e.region,
            .e_lambda => |e| return e.region,
            .e_runtime_error => |e| return e.region,
        }
    }

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .e_num => |num_expr| {
                var node = SExpr.init(gpa, "e-num");
                node.appendRegion(gpa, ir.calcRegionInfo(num_expr.region));

                // Add value info
                const value_str = std.fmt.allocPrint(gpa, "{}", .{num_expr.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_int => |int_expr| {
                var node = SExpr.init(gpa, "e-int");
                node.appendRegion(gpa, ir.calcRegionInfo(int_expr.region));

                // Add value
                const value_i128: i128 = @bitCast(int_expr.value.bytes);
                var value_buf: [40]u8 = undefined;
                const value_str = std.fmt.bufPrint(&value_buf, "{}", .{value_i128}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_frac_f64 => |e| {
                var node = SExpr.init(gpa, "e-frac-f64");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                // Add value
                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (e.value == 0)
                    "0.0"
                else if (@abs(e.value) < 1e-10 or @abs(e.value) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{e.value}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{e.value}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_frac_dec => |e| {
                var node = SExpr.init(gpa, "e-frac-dec");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                // Add value (convert RocDec to string)
                // RocDec has 18 decimal places, so divide by 10^18
                const dec_value_f64: f64 = @as(f64, @floatFromInt(e.value.num)) / std.math.pow(f64, 10, 18);
                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (dec_value_f64 == 0)
                    "0.0"
                else if (@abs(dec_value_f64) < 1e-10 or @abs(dec_value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{dec_value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{dec_value_f64}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_dec_small => |e| {
                var node = SExpr.init(gpa, "e-dec-small");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                // Add numerator and denominator_power_of_ten
                var num_buf: [32]u8 = undefined;
                const num_str = std.fmt.bufPrint(&num_buf, "{}", .{e.numerator}) catch "fmt_error";
                node.appendStringAttr(gpa, "numerator", num_str);

                var denom_buf: [32]u8 = undefined;
                const denom_str = std.fmt.bufPrint(&denom_buf, "{}", .{e.denominator_power_of_ten}) catch "fmt_error";
                node.appendStringAttr(gpa, "denominator-power-of-ten", denom_str);

                // Calculate and add the decimal value
                // Convert numerator to f64 and divide by 10^denominator_power_of_ten
                const numerator_f64: f64 = @floatFromInt(e.numerator);
                const denominator_f64: f64 = std.math.pow(f64, 10, @floatFromInt(e.denominator_power_of_ten));
                const value_f64 = numerator_f64 / denominator_f64;

                var value_buf: [512]u8 = undefined;
                // Use scientific notation for very large or very small numbers (but not zero)
                const value_str = if (value_f64 == 0)
                    "0.0"
                else if (@abs(value_f64) < 1e-10 or @abs(value_f64) > 1e10)
                    std.fmt.bufPrint(&value_buf, "{e}", .{value_f64}) catch "fmt_error"
                else
                    std.fmt.bufPrint(&value_buf, "{d}", .{value_f64}) catch "fmt_error";
                node.appendStringAttr(gpa, "value", value_str);

                return node;
            },
            .e_str_segment => |e| {
                var str_node = SExpr.init(gpa, "e-literal");
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                const value = ir.env.strings.get(e.literal);
                str_node.appendStringAttr(gpa, "string", value);

                return str_node;
            },
            .e_str => |e| {
                var str_node = SExpr.init(gpa, "e-string");
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                for (ir.store.sliceExpr(e.span)) |segment| {
                    var segment_node = ir.store.getExpr(segment).toSExpr(ir);
                    str_node.appendNode(gpa, &segment_node);
                }

                return str_node;
            },
            .e_list => |l| {
                var list_node = SExpr.init(gpa, "e-list");
                list_node.appendRegion(gpa, ir.calcRegionInfo(l.region));

                // Add list elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(l.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir);
                    elems_node.appendNode(gpa, &elem_node);
                }
                list_node.appendNode(gpa, &elems_node);

                return list_node;
            },
            .e_empty_list => |e| {
                var empty_list_node = SExpr.init(gpa, "e-empty_list");
                empty_list_node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                return empty_list_node;
            },
            .e_tuple => |t| {
                var node = SExpr.init(gpa, "e-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(t.region));

                // Add tuple elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(t.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir);
                    elems_node.appendNode(gpa, &elem_node);
                }
                node.appendNode(gpa, &elems_node);

                return node;
            },
            .e_lookup => |l| {
                switch (l) {
                    .local => |local| {
                        var lookup_node = SExpr.init(gpa, "e-lookup-local");
                        lookup_node.appendRegion(gpa, ir.calcRegionInfo(local.region));

                        var pattern_node = SExpr.init(gpa, "pattern");
                        pattern_node.appendRegion(gpa, ir.getNodeRegionInfo(local.pattern_idx));
                        lookup_node.appendNode(gpa, &pattern_node);

                        return lookup_node;
                    },
                    .external => |external_idx| {
                        var node = SExpr.init(gpa, "e-lookup-external");

                        var external_sexpr = ir.getExternalDecl(external_idx).toSExpr(ir);
                        node.appendNode(gpa, &external_sexpr);

                        return node;
                    },
                }
            },
            .e_match => |e| {
                var node = SExpr.init(gpa, "e-match");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                var match_sexpr = e.toSExpr(ir);
                node.appendNode(gpa, &match_sexpr);

                return node;
            },
            .e_if => |if_expr| {
                var node = SExpr.init(gpa, "e-if");
                node.appendRegion(gpa, ir.calcRegionInfo(if_expr.region));

                // Add branches
                var branches_node = SExpr.init(gpa, "if-branches");
                const branch_indices = ir.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = ir.store.getIfBranch(branch_idx);

                    var branch_node = SExpr.init(gpa, "if-branch");

                    // Add condition
                    const cond_expr = ir.store.getExpr(branch.cond);
                    var cond_node = cond_expr.toSExpr(ir);
                    branch_node.appendNode(gpa, &cond_node);

                    // Add body
                    const body_expr = ir.store.getExpr(branch.body);
                    var body_node = body_expr.toSExpr(ir);
                    branch_node.appendNode(gpa, &body_node);

                    branches_node.appendNode(gpa, &branch_node);
                }
                node.appendNode(gpa, &branches_node);

                // Add final_else
                var else_node = SExpr.init(gpa, "if-else");
                const else_expr = ir.store.getExpr(if_expr.final_else);
                var else_expr_node = else_expr.toSExpr(ir);
                else_node.appendNode(gpa, &else_expr_node);
                node.appendNode(gpa, &else_node);

                return node;
            },
            .e_call => |c| {
                var call_node = SExpr.init(gpa, "e-call");
                call_node.appendRegion(gpa, ir.calcRegionInfo(c.region));

                // Get all expressions from the args span
                const all_exprs = ir.store.exprSlice(c.args);

                // First element is the function being called
                if (all_exprs.len > 0) {
                    const fn_expr = ir.store.getExpr(all_exprs[0]);
                    var fn_node = fn_expr.toSExpr(ir);
                    call_node.appendNode(gpa, &fn_node);
                }

                // Remaining elements are the arguments
                if (all_exprs.len > 1) {
                    for (all_exprs[1..]) |arg_idx| {
                        const arg_expr = ir.store.getExpr(arg_idx);
                        var arg_node = arg_expr.toSExpr(ir);
                        call_node.appendNode(gpa, &arg_node);
                    }
                }

                return call_node;
            },
            .e_record => |record_expr| {
                var record_node = SExpr.init(gpa, "e-record");
                record_node.appendRegion(gpa, ir.calcRegionInfo(record_expr.region));

                // Add fields
                var fields_node = SExpr.init(gpa, "fields");
                for (ir.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    var field_node = ir.store.getRecordField(field_idx).toSExpr(ir);
                    fields_node.appendNode(gpa, &field_node);
                }
                record_node.appendNode(gpa, &fields_node);

                return record_node;
            },
            .e_empty_record => |e| {
                var empty_record_node = SExpr.init(gpa, "e-empty_record");
                empty_record_node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                return empty_record_node;
            },
            .e_block => |block_expr| {
                var block_node = SExpr.init(gpa, "e-block");
                block_node.appendRegion(gpa, ir.calcRegionInfo(block_expr.region));

                // Add statements
                for (ir.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    var stmt_node = ir.store.getStatement(stmt_idx).toSExpr(ir);
                    block_node.appendNode(gpa, &stmt_node);
                }

                // Add final expression
                var expr_node = ir.store.getExpr(block_expr.final_expr).toSExpr(ir);
                block_node.appendNode(gpa, &expr_node);

                return block_node;
            },
            .e_record_access => |access_expr| {
                var node = SExpr.init(gpa, "e-record_access");
                node.appendRegion(gpa, ir.calcRegionInfo(access_expr.region));

                // Add loc_expr
                var loc_expr_node = ir.store.getExpr(access_expr.loc_expr).toSExpr(ir);
                node.appendNode(gpa, &loc_expr_node);

                // Add field
                node.appendStringAttr(gpa, "field", ir.env.idents.getText(access_expr.field));

                return node;
            },
            .e_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add name
                node.appendStringAttr(gpa, "name", ir.env.idents.getText(tag_expr.name));

                // Add args
                node.appendStringAttr(gpa, "args", "TODO");

                return node;
            },
            .e_zero_argument_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-zero-argument-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add closure_name
                node.appendStringAttr(gpa, "closure", ir.getIdentText(tag_expr.closure_name));

                // Add name
                node.appendStringAttr(gpa, "name", ir.getIdentText(tag_expr.name));

                return node;
            },
            .e_lambda => |lambda_expr| {
                var node = SExpr.init(gpa, "e-lambda");
                node.appendRegion(gpa, ir.calcRegionInfo(lambda_expr.region));

                // Handle args span
                var args_node = SExpr.init(gpa, "args");
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    var pattern_node = ir.store.getPattern(arg_idx).toSExpr(ir);
                    args_node.appendNode(gpa, &pattern_node);
                }
                node.appendNode(gpa, &args_node);

                // Handle body
                var body_node = ir.store.getExpr(lambda_expr.body).toSExpr(ir);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .e_binop => |e| {
                var node = SExpr.init(gpa, "e-binop");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                node.appendStringAttr(gpa, "op", @tagName(e.op));

                var lhs_node = ir.store.getExpr(e.lhs).toSExpr(ir);
                node.appendNode(gpa, &lhs_node);

                var rhs_node = ir.store.getExpr(e.rhs).toSExpr(ir);
                node.appendNode(gpa, &rhs_node);

                return node;
            },
            .e_dot_access => |e| {
                var node = SExpr.init(gpa, "e-dot-access");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                var receiver_node = SExpr.init(gpa, "receiver");
                var expr_node = ir.store.getExpr(e.receiver).toSExpr(ir);
                receiver_node.appendNode(gpa, &expr_node);
                node.appendNode(gpa, &receiver_node);

                node.appendStringAttr(gpa, "field", ir.getIdentText(e.field_name));

                if (e.args) |args| {
                    var args_node = SExpr.init(gpa, "args");
                    for (ir.store.exprSlice(args)) |arg_idx| {
                        var arg_node = ir.store.getExpr(arg_idx).toSExpr(ir);
                        args_node.appendNode(gpa, &arg_node);
                    }
                    node.appendNode(gpa, &args_node);
                }

                return node;
            },
            .e_runtime_error => |e| {
                var node = SExpr.init(gpa, "e-runtime-error");

                // get our diagnostic
                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                // format just the tag
                const msg = std.fmt.allocPrint(gpa, "{s}", .{@tagName(diagnostic)}) catch |err| exitOnOom(err);
                defer gpa.free(msg);

                node.appendStringAttr(gpa, "tag", msg);

                return node;
            },
        }
    }
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        _ = line_starts;
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "ingested-file");
        node.appendStringAttr(gpa, "path", "TODO");

        const ident_text = ir.env.idents.getText(self.ident);
        node.appendStringAttr(gpa, "ident", ident_text);

        var type_node = self.type.toSExpr(ir);
        node.appendNode(gpa, &type_node);
        return node;
    }
};

/// A definition of a value (or destructured values) that
/// takes its value from an expression.
pub const Def = struct {
    pattern: Pattern.Idx,
    pattern_region: Region,
    expr: Expr.Idx,
    expr_region: Region,
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        const kind = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };

        var node = SExpr.init(gpa, kind);

        var pattern_node = ir.store.getPattern(self.pattern).toSExpr(ir);
        node.appendNode(gpa, &pattern_node);

        var expr_node = ir.store.getExpr(self.expr).toSExpr(ir);
        node.appendNode(gpa, &expr_node);

        if (self.annotation) |anno_idx| {
            var anno_node = ir.store.getAnnotation(anno_idx).toSExpr(ir, ir.env.line_starts);
            node.appendNode(gpa, &anno_node);
        }

        return node;
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
    /// Source region of the annotation
    region: Region,

    pub const Idx = enum(u32) { _ };

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        _ = line_starts;
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "annotation");
        node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        // Add the declared type annotation structure
        var type_anno_node = SExpr.init(gpa, "declared-type");
        const type_anno = ir.store.getTypeAnno(self.type_anno);
        var anno_sexpr = type_anno.toSExpr(ir);
        type_anno_node.appendNode(gpa, &anno_sexpr);
        node.appendNode(gpa, &type_anno_node);

        return node;
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

    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        var node = SExpr.init(gpa, "ext-decl");
        node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        // Add qualified name
        const qualified_name_str = ir.getIdentText(self.qualified_name);
        node.appendStringAttr(gpa, "qualified", qualified_name_str);

        // Add module name
        const module_name_str = ir.getIdentText(self.module_name);
        node.appendStringAttr(gpa, "module", module_name_str);

        // Add local name
        const local_name_str = ir.getIdentText(self.local_name);
        node.appendStringAttr(gpa, "local", local_name_str);

        // Add kind
        switch (self.kind) {
            .value => node.appendStringAttr(gpa, "kind", "value"),
            .type => node.appendStringAttr(gpa, "kind", "type"),
        }

        return node;
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

/// todo - evaluate if we need this?
pub const IfBranch = struct {
    cond: Expr.Idx,
    body: Expr.Idx,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    // Note: toSExpr is handled within Expr.if because the slice reference is there
};

/// TODO describe match expression
///
/// ```roc
/// match fruit_rank {
///     Apple => 1,
///     Banana => 2,
///     Orange => 3,
/// }
/// ```
pub const Match = struct {
    /// The condition of the match expression.
    loc_cond: Expr.Idx,
    /// The branches of the `match`
    branches: Branch.Span,
    /// Marks whether a match expression is exhaustive using a variable.
    exhaustive: TypeVar,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// TODO
    pub const Branch = struct {
        patterns: Match.BranchPattern.Span,
        value: Expr.Idx,
        guard: ?Expr.Idx,
        /// Marks whether a match branch is redundant using a variable.
        redundant: TypeVar,

        pub fn toSExpr(self: *const Match.Branch, ir: *const CIR) SExpr {
            const gpa = ir.env.gpa;
            var node = SExpr.init(gpa, "branch");

            var patterns_node = SExpr.init(gpa, "patterns");
            // Process each pattern in the span
            const patterns_data = ir.store.extra_data.items[self.patterns.span.start..(self.patterns.span.start + self.patterns.span.len * 2)];
            var i: usize = 0;
            while (i < patterns_data.len) : (i += 2) {
                const pattern_idx = @as(CIR.Pattern.Idx, @enumFromInt(patterns_data[i]));
                const degenerate = patterns_data[i + 1] != 0;
                const pattern = ir.store.getPattern(pattern_idx);
                var pattern_sexpr = pattern.toSExpr(ir);
                pattern_sexpr.appendBoolAttr(gpa, "degenerate", degenerate);
                patterns_node.appendNode(gpa, &pattern_sexpr);
            }
            node.appendNode(gpa, &patterns_node);

            var value_node = SExpr.init(gpa, "value");
            const value_expr = ir.store.getExpr(self.value);
            var value_sexpr = value_expr.toSExpr(ir);
            value_node.appendNode(gpa, &value_sexpr);
            node.appendNode(gpa, &value_node);

            if (self.guard) |guard_idx| {
                var guard_node = SExpr.init(gpa, "guard");
                const guard_expr = ir.store.getExpr(guard_idx);
                var guard_sexpr = guard_expr.toSExpr(ir);
                guard_node.appendNode(gpa, &guard_sexpr);
                node.appendNode(gpa, &guard_node);
            }

            return node;
        }

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: DataSpan };
    };

    /// TODO
    pub const BranchPattern = struct {
        pattern: Pattern.Idx,
        /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
        /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
        /// Degenerate patterns emit a runtime error if reached in a program.
        degenerate: bool,

        pub const Idx = enum(u32) { _ };
        pub const Span = struct { span: base.DataSpan };

        pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
            const gpa = ir.gpa;
            var node = self.pattern.toSExpr(ir);
            node.appendBoolAttr(gpa, "degenerate", self.degenerate);
            return node;
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "match");

        node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        var cond_node = SExpr.init(gpa, "cond");
        const cond_expr = ir.store.getExpr(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(ir);
        cond_node.appendNode(gpa, &cond_sexpr);

        node.appendNode(gpa, &cond_node);

        var branches_node = SExpr.init(gpa, "branches");
        for (ir.store.matchBranchSlice(self.branches)) |branch_idx| {
            const branch = ir.store.getMatchBranch(branch_idx);

            var branch_sexpr = branch.toSExpr(ir);
            branches_node.appendNode(gpa, &branch_sexpr);
        }
        node.appendNode(gpa, &branches_node);

        return node;
    }
};

/// A canonicalized if statement
pub const If = struct {
    branches: IfBranch.Span,
    final_else: Expr.Idx,
    region: Region,
};

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
pub const Pattern = union(enum) {
    /// An identifier in the assignment position, e.g. the `x` in `x = foo(1)`
    assign: struct {
        ident: Ident.Idx,
        region: Region,
    },
    as: struct {
        pattern: Pattern.Idx,
        ident: Ident.Idx,
        region: Region,
    },
    applied_tag: struct {
        ext_var: TypeVar,
        tag_name: Ident.Idx,
        arguments: Pattern.Span,
        region: Region,
    },
    record_destructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Span,
        region: Region,
    },
    list: struct {
        list_var: TypeVar,
        elem_var: TypeVar,
        patterns: Pattern.Span, // All non-rest patterns
        rest_info: ?struct {
            index: u32, // Where the rest appears (split point)
            pattern: ?Pattern.Idx, // None for `..`, Some(assign) for `.. as name`
        },
        region: Region,
    },
    tuple: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    int_literal: struct {
        value: IntValue,
        region: Region,
    },
    small_dec_literal: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        region: Region,
    },
    dec_literal: struct {
        value: RocDec,
        region: Region,
    },
    f64_literal: struct {
        value: f64,
        region: Region,
    },
    str_literal: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    char_literal: struct {
        num_var: TypeVar,
        requirements: types.Num.Int.Requirements,
        value: u32,
        region: Region,
    },
    underscore: struct {
        region: Region,
    },
    /// Compiles, but will crash if reached
    runtime_error: struct {
        diagnostic: Diagnostic.Idx,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toRegion(self: *const @This()) Region {
        switch (self.*) {
            .assign => |p| return p.region,
            .as => |p| return p.region,
            .applied_tag => |p| return p.region,
            .record_destructure => |p| return p.region,
            .list => |p| return p.region,
            .tuple => |p| return p.region,
            .int_literal => |p| return p.region,
            .small_dec_literal => |p| return p.region,
            .dec_literal => |p| return p.region,
            .f64_literal => |p| return p.region,
            .str_literal => |p| return p.region,
            .char_literal => |p| return p.region,
            .underscore => |p| return p.region,
            .runtime_error => |p| return p.region,
        }
    }

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .assign => |p| {
                var node = SExpr.init(gpa, "p-assign");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const ident = ir.getIdentText(p.ident);
                node.appendStringAttr(gpa, "ident", ident);

                return node;
            },
            .as => |a| {
                var node = SExpr.init(gpa, "p-as");
                node.appendRegion(gpa, ir.calcRegionInfo(a.region));

                const ident = ir.getIdentText(a.ident);
                node.appendStringAttr(gpa, "as", ident);

                var pattern_node = ir.store.getPattern(a.pattern).toSExpr(ir);
                node.appendNode(gpa, &pattern_node);

                return node;
            },
            .applied_tag => |p| {
                var node = SExpr.init(gpa, "p-applied-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .record_destructure => |p| {
                var node = SExpr.init(gpa, "p-record-destructure");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                var destructs_node = SExpr.init(gpa, "destructs");

                // Iterate through the destructs span and convert each to SExpr
                for (ir.store.sliceRecordDestructs(p.destructs)) |destruct_idx| {
                    var destruct_sexpr = ir.store.getRecordDestruct(destruct_idx).toSExpr(ir);
                    destructs_node.appendNode(gpa, &destruct_sexpr);
                }

                node.appendNode(gpa, &destructs_node);

                return node;
            },
            .list => |p| {
                var node = SExpr.init(gpa, "p-list");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                // Add rest information if present
                if (p.rest_info) |rest| {
                    var rest_node = SExpr.init(gpa, "rest-at");

                    // Add the index where rest appears
                    const index_str = std.fmt.allocPrint(gpa, "{d}", .{rest.index}) catch |err| exitOnOom(err);
                    defer gpa.free(index_str);
                    rest_node.appendRawAttr(gpa, "index", index_str);

                    // Add the rest pattern if it has a name
                    if (rest.pattern) |rest_pattern_idx| {
                        var rest_pattern_sexpr = ir.store.getPattern(rest_pattern_idx).toSExpr(ir);
                        rest_node.appendNode(gpa, &rest_pattern_sexpr);
                    }

                    node.appendNode(gpa, &rest_node);
                }

                return node;
            },
            .tuple => |p| {
                var node = SExpr.init(gpa, "p-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                return node;
            },
            .int_literal => |p| {
                var node = SExpr.init(gpa, "p-int");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .small_dec_literal => |p| {
                var node = SExpr.init(gpa, "p-small-dec");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .dec_literal => |p| {
                var node = SExpr.init(gpa, "p-dec");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .f64_literal => |p| {
                var node = SExpr.init(gpa, "p-f64");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                // TODO: add fields
                return node;
            },
            .str_literal => |p| {
                var node = SExpr.init(gpa, "p-str");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const text = ir.env.strings.get(p.literal);
                node.appendStringAttr(gpa, "text", text);

                return node;
            },
            .char_literal => |l| {
                var node = SExpr.init(gpa, "p-char");
                node.appendRegion(gpa, ir.calcRegionInfo(l.region));

                const char_str = std.fmt.allocPrint(gpa, "'\\u({d})'", .{l.value}) catch "<oom>";
                defer gpa.free(char_str);
                node.appendStringAttr(gpa, "byte", char_str);
                // TODO: add num_var and requirements
                return node;
            },
            .underscore => |p| {
                var node = SExpr.init(gpa, "p-underscore");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                return node;
            },
            .runtime_error => |e| {
                var node = SExpr.init(gpa, "p-runtime-error");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                const diagnostic = ir.store.getDiagnostic(e.diagnostic);

                node.appendStringAttr(gpa, "tag", @tagName(diagnostic));
                return node;
            },
        }
    }
};

/// todo
pub const RecordDestruct = struct {
    region: Region,
    label: Ident.Idx,
    ident: Ident.Idx,
    kind: Kind,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// todo
    pub const Kind = union(enum) {
        Required,
        Guard: Pattern.Idx,

        pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
            const gpa = ir.env.gpa;
            _ = line_starts;

            switch (self.*) {
                .Required => return SExpr.init(gpa, "required"),
                .Guard => |guard_idx| {
                    var guard_kind_node = SExpr.init(gpa, "guard");
                    _ = guard_idx; // TODO: implement guard pattern retrieval
                    guard_kind_node.appendStringAttr(gpa, "pattern", "TODO");
                    return guard_kind_node;
                },
            }
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        var node = SExpr.init(gpa, "record-destruct");

        node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        const label_text = ir.env.idents.getText(self.label);
        const ident_text = ir.env.idents.getText(self.ident);
        node.appendStringAttr(gpa, "label", label_text);
        node.appendStringAttr(gpa, "ident", ident_text);

        var kind_node = self.kind.toSExpr(ir, std.ArrayList(u32).init(ir.env.gpa));
        node.appendNode(gpa, &kind_node);

        return node;
    }
};

/// Helper function to convert the entire Canonical IR to a string in S-expression format
/// and write it to the given writer.
///
/// If a single expression is provided we only print that expression
pub fn toSExprStr(ir: *CIR, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;
    const gpa = ir.env.gpa;

    if (maybe_expr_idx) |expr_idx| {
        // Get the expression from the store
        var expr_node = ir.store.getExpr(expr_idx).toSExpr(ir);
        defer expr_node.deinit(gpa);

        expr_node.toStringPretty(writer);
    } else {
        var root_node = SExpr.init(gpa, "can-ir");
        defer root_node.deinit(gpa);

        // Iterate over all the definitions in the file and convert each to an S-expression
        const defs_slice = ir.store.sliceDefs(ir.all_defs);
        const statements_slice = ir.store.sliceStatements(ir.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0) {
            root_node.appendBoolAttr(gpa, "empty", true);
        }

        for (defs_slice) |def_idx| {
            var def_node = ir.store.getDef(def_idx).toSExpr(ir);
            root_node.appendNode(gpa, &def_node);
        }

        for (statements_slice) |stmt_idx| {
            var stmt_node = ir.store.getStatement(stmt_idx).toSExpr(ir);
            root_node.appendNode(gpa, &stmt_node);
        }

        root_node.toStringPretty(writer);
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

    const info = base.RegionInfo.position(source, self.env.line_starts.items, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
}

/// Get region information for a node in the Canonical IR.
pub fn getNodeRegionInfo(ir: *const CIR, idx: anytype) base.RegionInfo {
    const region = ir.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    return ir.calcRegionInfo(region);
}

/// Helper function to convert type information from the Canonical IR to a string
/// in S-expression format for snapshot testing. Implements the definition-focused
/// format showing final types for defs, expressions, and builtins.
pub fn toSexprTypesStr(ir: *CIR, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;

    const gpa = ir.env.gpa;

    // Create TypeWriter for converting types to strings
    var type_string_buf = std.ArrayList(u8).init(gpa);
    defer type_string_buf.deinit();

    var type_writer = types.writers.TypeWriter.init(type_string_buf.writer(), ir.env);

    if (maybe_expr_idx) |expr_idx| {
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

        var expr_node = SExpr.init(gpa, "expr");
        defer expr_node.deinit(gpa);

        expr_node.appendRegion(gpa, ir.getNodeRegionInfo(expr_idx));

        if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
            const unknown_node = SExpr.init(gpa, "unknown");
            expr_node.appendNode(gpa, &unknown_node);
        } else {
            if (type_writer.writeVar(expr_var)) {
                expr_node.appendStringAttr(gpa, "type", type_string_buf.items);
            } else |err| {
                exitOnOom(err);
            }
        }

        expr_node.toStringPretty(writer);
    } else {
        var root_node = SExpr.init(gpa, "inferred-types");
        defer root_node.deinit(gpa);

        // Collect definitions
        var defs_node = SExpr.init(gpa, "defs");
        const defs_slice = ir.store.sliceDefs(ir.all_defs);

        for (defs_slice) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Extract identifier name from the pattern (assuming it's an assign pattern)
            const pattern = ir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |assign_pat| {
                    var def_node = SExpr.init(gpa, "patt");

                    def_node.appendRegion(gpa, ir.calcRegionInfo(assign_pat.region));

                    // Get the type variable for this definition
                    // Each definition has a type_var at its node index which represents the type of the definition
                    const def_var = @as(types.Var, @enumFromInt(@intFromEnum(def_idx)));

                    // Clear the buffer and write the type
                    type_string_buf.clearRetainingCapacity();
                    try type_writer.writeVar(def_var);
                    def_node.appendStringAttr(gpa, "type", type_string_buf.items);

                    defs_node.appendNode(gpa, &def_node);
                },
                else => {
                    // For non-assign patterns, we could handle destructuring, but for now skip
                    continue;
                },
            }
        }

        root_node.appendNode(gpa, &defs_node);

        // Collect expression types (for significant expressions with regions)
        var expressions_node = SExpr.init(gpa, "expressions");

        // Walk through all expressions and collect those with meaningful types
        // We'll collect expressions that have regions and aren't just intermediate nodes
        for (defs_slice) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Get the expression type
            const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(def.expr)));

            var expr_node = SExpr.init(gpa, "expr");
            expr_node.appendRegion(gpa, ir.calcRegionInfo(def.expr_region));
            // expr_node.appendUnsignedInt(gpa, @intFromEnum(expr_var));

            if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
                const unknown_node = SExpr.init(gpa, "unknown");
                expr_node.appendNode(gpa, &unknown_node);
            } else {
                // Clear the buffer and write the type
                type_string_buf.clearRetainingCapacity();
                try type_writer.writeVar(expr_var);
                expr_node.appendStringAttr(gpa, "type", type_string_buf.items);
            }

            expressions_node.appendNode(gpa, &expr_node);
        }

        root_node.appendNode(gpa, &expressions_node);

        root_node.toStringPretty(writer);
    }
}
