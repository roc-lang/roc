//! The canonical intermediate representation (CIR) is a representation of the
//! canonicalized abstract syntax tree (AST) that is used for interpreting code generation and type checking, and later compilation stages.

const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const tracy = @import("../../tracy.zig");
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
const SExprTree = base.SExprTree;
const TypeVar = types.Var;
const NodeStore = @import("NodeStore.zig");

pub const RocDec = @import("../../builtins/dec.zig").RocDec;
pub const Node = @import("Node.zig");
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
            break :blk Diagnostic.buildInvalidTopLevelStatementReport(
                allocator,
                stmt_name,
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
    try testing.expectEqual(16, @sizeOf(Node));
}

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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("field");
        tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();
        ir.store.getExpr(self.value).pushToSExprTree(ir, tree);
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
        region: Region,
    },

    pub const ModuleMethod = struct {
        var_name: Ident.Idx, // Type variable identifier (e.g., "a")
        method_name: Ident.Idx, // Method name without leading dot (e.g., "decode")
        args: TypeAnno.Span, // Method argument types
        ret_anno: TypeAnno.Idx, // Method return type
        external_decl: ExternalDecl.Idx, // External declaration for module lookup
        region: Region,
    };

    pub const ModuleAlias = struct {
        var_name: Ident.Idx, // Type variable identifier (e.g., "elem")
        alias_name: Ident.Idx, // Alias name without leading dot (e.g., "Sort")
        external_decl: ExternalDecl.Idx, // External declaration for module lookup
        region: Region,
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .mod_method => |mm| {
                var node = SExpr.init(gpa, "method");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, mm.region);

                node.appendStringAttr(gpa, "module-of", ir.getIdentText(mm.var_name));
                node.appendStringAttr(gpa, "ident", ir.getIdentText(mm.method_name));

                // Add argument types
                const args_slice = ir.store.sliceTypeAnnos(mm.args);
                var args_node = SExpr.init(gpa, "args");
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_child = arg.toSExpr(ir);
                    args_node.appendNode(gpa, &arg_child);
                }
                node.appendNode(gpa, &args_node);

                // Add return type
                var ret_node = ir.store.getTypeAnno(mm.ret_anno).toSExpr(ir);
                node.appendNode(gpa, &ret_node);

                return node;
            },
            .mod_alias => |ma| {
                var node = SExpr.init(gpa, "alias");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, ma.region);

                node.appendStringAttr(gpa, "module-of", ir.getIdentText(ma.var_name));
                node.appendStringAttr(gpa, "ident", ir.getIdentText(ma.alias_name));

                return node;
            },
            .malformed => |m| {
                var node = SExpr.init(gpa, "malformed");
                ir.appendRegionInfoToSexprNodeFromRegion(&node, m.region);
                return node;
            },
        }
    }

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        switch (self.*) {
            .mod_method => |mm| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("method");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, mm.region);
                tree.pushStringPair("module-of", ir.env.idents.getText(mm.var_name));
                tree.pushStringPair("ident", ir.env.idents.getText(mm.method_name));
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                for (ir.store.sliceTypeAnnos(mm.args)) |arg_idx| {
                    ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree);
                }
                tree.endNode(args_begin, attrs2);

                ir.store.getTypeAnno(mm.ret_anno).pushToSExprTree(ir, tree);
                tree.endNode(begin, attrs);
            },
            .mod_alias => |ma| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("alias");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, ma.region);
                tree.pushStringPair("module-of", ir.env.idents.getText(ma.var_name));
                tree.pushStringPair("ident", ir.env.idents.getText(ma.alias_name));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("malformed");
                ir.appendRegionInfoToSExprTreeFromRegion(tree, m.region);
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
    region: Region, // Source location of the entire header

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "ty-header");
        ir.appendRegionInfoToSexprNodeFromRegion(&node, self.region);

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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ty-header");
        ir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);
        tree.pushStringPair("name", ir.env.idents.getText(self.name));
        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            tree.pushStaticAtom("ty-args");
            const attrs2 = tree.beginNode();
            for (ir.store.sliceTypeAnnos(self.args)) |arg_idx| {
                ir.store.getTypeAnno(arg_idx).pushToSExprTree(ir, tree);
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

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("ingested-file");
        tree.pushStringPair("path", "TODO");
        // tree.pushStringPair("path", env.strings.get(self.relative_path));
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

        var pattern_node = ir.store.getPattern(self.pattern).toSExpr(ir, self.pattern);
        node.appendNode(gpa, &pattern_node);

        var expr_node = ir.store.getExpr(self.expr).toSExpr(ir);
        node.appendNode(gpa, &expr_node);

        if (self.annotation) |anno_idx| {
            var anno_node = ir.store.getAnnotation(anno_idx).toSExpr(ir, ir.env.line_starts);
            node.appendNode(gpa, &anno_node);
        }

        return node;
    }

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        tree.pushStaticAtom(name);
        const attrs = tree.beginNode();

        ir.store.getPattern(self.pattern).pushToSExprTree(ir, tree, self.pattern, null);

        ir.store.getExpr(self.expr).pushToSExprTree(ir, tree);

        if (self.annotation) |anno_idx| {
            const anno = ir.store.getAnnotation(anno_idx);
            anno.pushToSExprTree(ir, tree);
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
    /// Source region of the annotation
    region: Region,

    pub const Idx = enum(u32) { _ };

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        _ = line_starts;
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "annotation");
        ir.appendRegionInfoToSexprNodeFromRegion(&node, self.region);

        // Add the declared type annotation structure
        var type_anno_node = SExpr.init(gpa, "declared-type");
        const type_anno = ir.store.getTypeAnno(self.type_anno);
        var anno_sexpr = type_anno.toSExpr(ir);
        type_anno_node.appendNode(gpa, &anno_sexpr);
        node.appendNode(gpa, &type_anno_node);

        return node;
    }

    pub fn pushToSExprTree(self: *const @This(), ir: *const CIR, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("annotation");
        ir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);
        const attrs = tree.beginNode();

        // Add the declared type annotation structure
        const type_begin = tree.beginNode();
        tree.pushStaticAtom("declared-type");
        const type_attrs = tree.beginNode();
        ir.store.getTypeAnno(self.type_anno).pushToSExprTree(ir, tree);
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

    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        var node = SExpr.init(gpa, "ext-decl");
        ir.appendRegionInfoToSexprNodeFromRegion(&node, self.region);

        // Add fully qualified name
        const qualified_name_str = ir.getIdentText(self.qualified_name);
        node.appendStringAttr(gpa, "ident", qualified_name_str);

        // Add kind
        switch (self.kind) {
            .value => node.appendStringAttr(gpa, "kind", "value"),
            .type => node.appendStringAttr(gpa, "kind", "type"),
        }

        return node;
    }

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
pub fn toSExpr(ir: *CIR, maybe_expr_idx: ?Expr.Idx, source: []const u8) SExpr {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;
    const gpa = ir.env.gpa;

    if (maybe_expr_idx) |expr_idx| {
        // Get the expression from the store
        return ir.store.getExpr(expr_idx).toSExpr(ir);
    } else {
        var root_node = SExpr.init(gpa, "can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression
        const defs_slice = ir.store.sliceDefs(ir.all_defs);
        const statements_slice = ir.store.sliceStatements(ir.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and ir.external_decls.items.len == 0) {
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

        for (ir.external_decls.items) |*external_decl| {
            var external_node = external_decl.toSExpr(ir);
            root_node.appendNode(gpa, &external_node);
        }

        return root_node;
    }
}

pub fn pushToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree, source: []const u8) void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;

    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        ir.store.getExpr(expr_idx).pushToSExprTree(ir, tree);
    } else {
        const root_begin = tree.beginNode();
        tree.pushStaticAtom("can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression tree
        const defs_slice = ir.store.sliceDefs(ir.all_defs);
        const statements_slice = ir.store.sliceStatements(ir.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and ir.external_decls.items.len == 0) {
            tree.pushBoolPair("empty", true);
        }
        const attrs = tree.beginNode();

        for (defs_slice) |def_idx| {
            ir.store.getDef(def_idx).pushToSExprTree(ir, tree);
        }

        for (statements_slice) |stmt_idx| {
            ir.store.getStatement(stmt_idx).pushToSExprTree(ir, tree);
        }

        for (ir.external_decls.items) |*external_decl| {
            external_decl.pushToSExprTree(ir, tree);
        }

        tree.endNode(root_begin, attrs);
    }
}

/// Helper function to convert the entire Canonical IR to a string in S-expression format
/// and write it to the given writer.
///
/// If a single expression is provided we only print that expression
pub fn toSExprStr(ir: *CIR, env: *ModuleEnv, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    const gpa = ir.env.gpa;
    var node = toSExpr(ir, env, maybe_expr_idx, source);
    defer node.deinit(gpa);
    node.toStringPretty(writer);
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
pub fn toSexprTypes(ir: *CIR, maybe_expr_idx: ?Expr.Idx, source: []const u8) SExpr {
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

        ir.appendRegionInfoToSexprNode(&expr_node, expr_idx);

        if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
            var unknown_node = SExpr.init(gpa, "unknown");
            expr_node.appendNode(gpa, &unknown_node);
        } else {
            if (type_writer.writeVar(expr_var)) {
                expr_node.appendStringAttr(gpa, "type", type_string_buf.items);
            } else |err| {
                exitOnOom(err);
            }
        }

        return expr_node;
    } else {
        var root_node = SExpr.init(gpa, "inferred-types");

        // Collect definitions
        var defs_node = SExpr.init(gpa, "defs");
        const all_defs = ir.store.sliceDefs(ir.all_defs);

        for (all_defs) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Extract identifier name from the pattern (assuming it's an assign pattern)
            const pattern = ir.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |_| {
                    var def_node = SExpr.init(gpa, "patt");

                    ir.appendRegionInfoToSexprNode(&def_node, def_idx);

                    // Get the type variable for this definition
                    // Each definition has a type_var at its node index which represents the type of the definition
                    const def_var = ir.idxToTypeVar(&ir.env.types, def_idx) catch |err| exitOnOom(err);

                    // Clear the buffer and write the type
                    type_string_buf.clearRetainingCapacity();
                    type_writer.writeVar(def_var) catch |err| exitOnOom(err);
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

        // Collect statements
        var stmts_node = SExpr.init(gpa, "type_decls");
        const all_stmts = ir.store.sliceStatements(ir.all_statements);

        var has_type_decl = false;
        for (all_stmts) |stmt_idx| {
            const stmt = ir.store.getStatement(stmt_idx);

            // Get the type variable for this definition
            // Each definition has a type_var at its node index which represents the type of the definition
            const stmt_var = ir.idxToTypeVar(&ir.env.types, stmt_idx) catch |err| exitOnOom(err);

            switch (stmt) {
                .s_alias_decl => |alias| {
                    has_type_decl = true;

                    var stmt_node = SExpr.init(gpa, "alias");
                    ir.appendRegionInfoToSexprNodeFromRegion(&stmt_node, alias.region);

                    const header = ir.store.getTypeHeader(alias.header);
                    const header_sexpr = header.toSExpr(ir);
                    stmt_node.appendNode(gpa, &header_sexpr);

                    // Clear the buffer and write the type
                    type_string_buf.clearRetainingCapacity();
                    type_writer.writeVar(stmt_var) catch |err| exitOnOom(err);
                    stmt_node.appendStringAttr(gpa, "type", type_string_buf.items);

                    stmts_node.appendNode(gpa, &stmt_node);
                },
                .s_nominal_decl => |nominal| {
                    has_type_decl = true;

                    var stmt_node = SExpr.init(gpa, "nominal");
                    ir.appendRegionInfoToSexprNodeFromRegion(&stmt_node, nominal.region);

                    const header = ir.store.getTypeHeader(nominal.header);
                    const header_sexpr = header.toSExpr(ir);
                    stmt_node.appendNode(gpa, &header_sexpr);

                    // Clear the buffer and write the type
                    type_string_buf.clearRetainingCapacity();
                    type_writer.writeVar(stmt_var) catch |err| exitOnOom(err);
                    stmt_node.appendStringAttr(gpa, "type", type_string_buf.items);

                    stmts_node.appendNode(gpa, &stmt_node);
                },

                else => {
                    // For non-assign patterns, we could handle destructuring, but for now skip
                    continue;
                },
            }
        }

        if (has_type_decl) {
            root_node.appendNode(gpa, &stmts_node);
        } else {
            stmts_node.deinit(gpa);
        }

        // Collect expression types (for significant expressions with regions)
        var expressions_node = SExpr.init(gpa, "expressions");

        // Walk through all expressions and collect those with meaningful types
        // We'll collect expressions that have regions and aren't just intermediate nodes
        for (all_defs) |def_idx| {
            const def = ir.store.getDef(def_idx);

            // Get the expression type
            const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(def.expr)));

            var expr_node = SExpr.init(gpa, "expr");
            ir.appendRegionInfoToSexprNodeFromRegion(&expr_node, def.expr_region);

            if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
                var unknown_node = SExpr.init(gpa, "unknown");
                expr_node.appendNode(gpa, &unknown_node);
            } else {
                // Clear the buffer and write the type
                type_string_buf.clearRetainingCapacity();
                type_writer.writeVar(expr_var) catch |err| exitOnOom(err);
                expr_node.appendStringAttr(gpa, "type", type_string_buf.items);
            }

            expressions_node.appendNode(gpa, &expr_node);
        }

        root_node.appendNode(gpa, &expressions_node);

        return root_node;
    }
}

pub fn pushTypesToSExprTree(ir: *CIR, maybe_expr_idx: ?Expr.Idx, tree: *SExprTree, source: []const u8) void {
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

        const expr_begin = tree.beginNode();
        tree.pushStaticAtom("expr");

        ir.appendRegionInfoToSExprTree(tree, expr_idx);

        if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
            const unknown_begin = tree.beginNode();
            tree.pushStaticAtom("unknown");
            const unknown_attrs = tree.beginNode();
            tree.endNode(unknown_begin, unknown_attrs);
        } else {
            if (type_writer.writeVar(expr_var)) {
                tree.pushStringPair("type", type_string_buf.items);
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

                    ir.appendRegionInfoToSExprTree(tree, def_idx);

                    // Get the type variable for this definition
                    const def_var = ir.idxToTypeVar(&ir.env.types, def_idx) catch |err| exitOnOom(err);

                    // Clear the buffer and write the type
                    type_string_buf.clearRetainingCapacity();
                    type_writer.writeVar(def_var) catch |err| exitOnOom(err);
                    tree.pushStringPair("type", type_string_buf.items);

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
                        ir.appendRegionInfoToSExprTreeFromRegion(tree, alias.region);

                        // Clear the buffer and write the type
                        type_string_buf.clearRetainingCapacity();
                        type_writer.writeVar(stmt_var) catch |err| exitOnOom(err);
                        tree.pushStringPair("type", type_string_buf.items);
                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(alias.header);
                        header.pushToSExprTree(ir, tree);

                        tree.endNode(stmt_node_begin, stmt_node_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        has_type_decl = true;

                        const stmt_node_begin = tree.beginNode();
                        tree.pushStaticAtom("nominal");
                        ir.appendRegionInfoToSExprTreeFromRegion(tree, nominal.region);

                        // Clear the buffer and write the type
                        type_string_buf.clearRetainingCapacity();
                        type_writer.writeVar(stmt_var) catch |err| exitOnOom(err);
                        tree.pushStringPair("type", type_string_buf.items);

                        const stmt_node_attrs = tree.beginNode();

                        const header = ir.store.getTypeHeader(nominal.header);
                        header.pushToSExprTree(ir, tree);

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
            ir.appendRegionInfoToSExprTreeFromRegion(tree, def.expr_region);

            if (@intFromEnum(expr_var) > ir.env.types.slots.backing.items.len) {
                const unknown_begin = tree.beginNode();
                tree.pushStaticAtom("unknown");
                const unknown_attrs = tree.beginNode();
                tree.endNode(unknown_begin, unknown_attrs);
            } else {
                // Clear the buffer and write the type
                type_string_buf.clearRetainingCapacity();
                type_writer.writeVar(expr_var) catch |err| exitOnOom(err);
                tree.pushStringPair("type", type_string_buf.items);
            }

            const expr_node_attrs = tree.beginNode();
            tree.endNode(expr_node_begin, expr_node_attrs);
        }

        tree.endNode(exprs_begin, exprs_attrs);

        tree.endNode(root_begin, attrs);
    }
}

/// Helper function to convert type information from the Canonical IR to a string
/// in S-expression format for snapshot testing. Calls `toSexprTypes` and writes the result.
pub fn toSexprTypesStr(ir: *CIR, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    const gpa = ir.env.gpa;
    var node = toSexprTypes(ir, maybe_expr_idx, source);
    defer node.deinit(gpa);
    node.toStringPretty(writer);
}
