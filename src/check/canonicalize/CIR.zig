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
const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");

pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

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
    const NODE_STORE_CAPACITY = 10_000;

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
            const literal_text = self.env.strings.get(data.literal);
            break :blk Diagnostic.buildInvalidNumLiteralReport(
                allocator,
                literal_text,
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
        .custom_type_redeclared => |data| blk: {
            const type_name = self.env.idents.getText(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);
            break :blk Diagnostic.buildCustomTypeRedeclaredReport(
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
    };
}

/// Inserts a placeholder CIR node and creates a fresh variable in the types store at that index
pub fn pushFreshTypeVar(self: *CIR, parent_node_idx: Node.Idx, region: base.Region) types.Var {
    return self.pushTypeVar(.{ .flex_var = null }, parent_node_idx, region);
}

/// Inserts a placeholder CIR node and creates a type variable with the
/// specified content in the types store at that index
pub fn pushTypeVar(self: *CIR, content: types.Content, parent_node_idx: Node.Idx, region: base.Region) types.Var {
    // insert a placeholder can node
    const var_slot = self.store.addTypeVarSlot(parent_node_idx, region);

    // if the new can node idx is greater than the types store length, backfill
    const var_: types.Var = @enumFromInt(@intFromEnum(var_slot));
    self.env.types.fillInSlotsThru(var_) catch |err| exitOnOom(err);

    // set the type store slot based on the placeholder node idx
    self.env.types.setVarContent(var_, content);

    return var_;
}

/// Set a type variable To the specified content at the specified CIR node index.
pub fn setTypeVarAtDef(self: *CIR, at_idx: Def.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Set a type variable To the specified content at the specified CIR node index.
pub fn setTypeVarAtExpr(self: *CIR, at_idx: Expr.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Set a type variable To the specified content at the specified CIR node index.
pub fn setTypeVarAtPat(self: *CIR, at_idx: Pattern.Idx, content: types.Content) types.Var {
    return self.setTypeVarAt(@enumFromInt(@intFromEnum(at_idx)), content);
}

/// Set a type variable To the specified content at the specified CIR node index.
pub fn setTypeVarAt(self: *CIR, at_idx: Node.Idx, content: types.Content) types.Var {
    // if the new can node idx is greater than the types store length, backfill
    const var_: types.Var = @enumFromInt(@intFromEnum(at_idx));
    self.env.types.fillInSlotsThru(var_) catch |err| exitOnOom(err);

    // set the type store slot based on the placeholder node idx
    self.env.types.setVarContent(var_, content);

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

// Helper to add identifier info to a s-expr node
fn appendIdent(node: *SExpr, gpa: std.mem.Allocator, ir: *const CIR, name: []const u8, ident_idx: Ident.Idx) void {
    const ident_text = ir.env.idents.getText(ident_idx);

    // Create a node with no pre-allocated children to avoid aliasing issues
    const ident_node = SExpr{
        .node = .{
            .value = gpa.dupe(u8, name) catch @panic("Failed to duplicate name"),
            .region = null,
            .node_idx = null,
            .attributes = .{},
            .children = .{},
        },
    };

    // Append the node to the parent first
    switch (node.*) {
        .node => |*n| {
            n.children.append(gpa, ident_node) catch @panic("Failed to append node");

            // Now add the string child directly to the node in its final location
            const last_idx = n.children.items.len - 1;
            n.children.items[last_idx].appendString(gpa, ident_text);
        },
        else => @panic("appendIdent called on non-node"),
    }
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
    decl: struct {
        pattern: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// A rebindable declaration using the "var" keyword
    /// Not valid at the top level of a module
    @"var": struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// Reassignment of a previously declared var
    /// Not valid at the top level of a module
    reassign: struct {
        pattern_idx: Pattern.Idx,
        expr: Expr.Idx,
        region: Region,
    },
    /// The "crash" keyword instruct a runtime crash with message
    ///
    /// Not valid at the top level of a module
    crash: struct {
        msg: StringLiteral.Idx,
        region: Region,
    },
    /// Just an expression - usually the return value for a block
    ///
    /// Not valid at the top level of a module
    expr: struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// An expression that will cause a panic (or some other error handling mechanism) if it evaluates to false
    expect: struct {
        body: Expr.Idx,
        region: Region,
    },
    /// A block of code that will be ran multiple times for each item in a list.
    ///
    /// Not valid at the top level of a module
    @"for": struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: Region,
    },
    /// A early return of the enclosing function.
    ///
    /// Not valid at the top level of a module
    @"return": struct {
        expr: Expr.Idx,
        region: Region,
    },
    /// Brings in another module for use in the current module, optionally exposing only certain members of that module.
    ///
    /// Only valid at the top level of a module
    import: struct {
        module_name_tok: Ident.Idx,
        qualifier_tok: ?Ident.Idx,
        alias_tok: ?Ident.Idx,
        exposes: ExposedItem.Span,
        region: Region,
    },
    /// A declaration of a new type - whether an alias or a new nominal custom type
    ///
    /// Only valid at the top level of a module
    type_decl: struct {
        header: TypeHeader.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    },
    /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
    type_anno: struct {
        name: Ident.Idx,
        anno: CIR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *CIR, env: *ModuleEnv) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .decl => |d| {
                var node = SExpr.init(gpa, "s-let");
                node.appendRegion(gpa, ir.calcRegionInfo(d.region));

                var pattern_node = ir.store.getPattern(d.pattern).toSExpr(ir);
                pattern_node.appendIdx(gpa, d.pattern);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(d.expr).toSExpr(ir, env);
                expr_node.appendIdx(gpa, d.expr);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .@"var" => |v| {
                var node = SExpr.init(gpa, "s-var");
                node.appendRegion(gpa, ir.calcRegionInfo(v.region));

                var pattern_node = ir.store.getPattern(v.pattern_idx).toSExpr(ir);
                pattern_node.appendIdx(gpa, v.pattern_idx);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(v.expr).toSExpr(ir, env);
                expr_node.appendIdx(gpa, v.expr);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .reassign => |r| {
                var node = SExpr.init(gpa, "s-reassign");
                node.appendRegion(gpa, ir.calcRegionInfo(r.region));

                var pattern_node = ir.store.getPattern(r.pattern_idx).toSExpr(ir);
                pattern_node.appendIdx(gpa, r.pattern_idx);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(r.expr).toSExpr(ir, env);
                expr_node.appendIdx(gpa, r.expr);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .crash => |c| {
                var node = SExpr.init(gpa, "s-crash");
                node.appendRegion(gpa, ir.calcRegionInfo(c.region));
                node.appendStringAttr(gpa, "msg", env.strings.get(c.msg));
                return node;
            },
            .expr => |s| {
                var node = SExpr.init(gpa, "s-expr");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir, env);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .expect => |s| {
                var node = SExpr.init(gpa, "s-expect");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var body_node = ir.store.getExpr(s.body).toSExpr(ir, env);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .@"for" => |s| {
                var node = SExpr.init(gpa, "s-for");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var pattern_node = ir.store.getPattern(s.patt).toSExpr(ir);
                pattern_node.appendIdx(gpa, s.patt);
                node.appendNode(gpa, &pattern_node);

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir, env);
                expr_node.appendIdx(gpa, s.expr);
                node.appendNode(gpa, &expr_node);

                var body_node = ir.store.getExpr(s.body).toSExpr(ir, env);
                body_node.appendIdx(gpa, s.body);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .@"return" => |s| {
                var node = SExpr.init(gpa, "s-return");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                var expr_node = ir.store.getExpr(s.expr).toSExpr(ir, env);
                node.appendNode(gpa, &expr_node);

                return node;
            },
            .import => |s| {
                var node = SExpr.init(gpa, "s-import");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "module", env.idents.getText(s.module_name_tok));

                if (s.qualifier_tok) |qualifier| {
                    node.appendStringAttr(gpa, "qualifier", ir.getIdentText(qualifier));
                }

                if (s.alias_tok) |alias| {
                    node.appendStringAttr(gpa, "alias", ir.getIdentText(alias));
                }

                var exposes_node = SExpr.init(gpa, "exposes");
                const exposes_slice = ir.store.sliceExposedItems(s.exposes);
                for (exposes_slice) |exposed_idx| {
                    const exposed_sexpr = ir.store.getExposedItem(exposed_idx).toSExpr(gpa, ir.env);
                    exposes_node.appendNode(gpa, &exposed_sexpr);
                }
                node.appendNode(gpa, &exposes_node);

                return node;
            },
            .type_decl => |s| {
                var node = SExpr.init(gpa, "s-type-decl");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                // Add the type header
                var header_node = ir.store.getTypeHeader(s.header).toSExpr(ir, env);
                node.appendNode(gpa, &header_node);

                // Add the type annotation
                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir, env);
                node.appendNode(gpa, &anno_node);

                // TODO: Add where clause when implemented
                if (s.where) |_| {
                    node.appendStringAttr(gpa, "match", "TODO");
                }

                return node;
            },
            .type_anno => |s| {
                var node = SExpr.init(gpa, "s-type-anno");
                node.appendRegion(gpa, ir.calcRegionInfo(s.region));

                node.appendStringAttr(gpa, "name", ir.getIdentText(s.name));

                var anno_node = ir.store.getTypeAnno(s.anno).toSExpr(ir, env);
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
            .decl => |s| return s.region,
            .@"var" => |s| return s.region,
            .reassign => |s| return s.region,
            .crash => |s| return s.region,
            .expr => |s| return s.region,
            .expect => |s| return s.region,
            .@"for" => |s| return s.region,
            .@"return" => |s| return s.region,
            .import => |s| return s.region,
            .type_decl => |s| return s.region,
            .type_anno => |s| return s.region,
        }
    }
};

/// A working representation of a record field
pub const RecordField = struct {
    name: Ident.Idx,
    value: Expr.Idx,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
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

    pub fn toSExpr(self: *const @This(), ir: *const CIR, env: *ModuleEnv) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .apply => |a| {
                var node = SExpr.init(gpa, "ty-apply");
                node.appendRegion(gpa, ir.calcRegionInfo(a.region));

                node.appendStringAttr(gpa, "symbol", ir.getIdentText(a.symbol));

                const args_slice = ir.store.sliceTypeAnnos(a.args);
                for (args_slice) |arg_idx| {
                    const arg = ir.store.getTypeAnno(arg_idx);
                    var arg_node = arg.toSExpr(ir, env);
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
                    var tag_node = tag.toSExpr(ir, env);
                    node.appendNode(gpa, &tag_node);
                }

                if (tu.open_anno) |open_idx| {
                    const open_anno = ir.store.getTypeAnno(open_idx);
                    var open_node = open_anno.toSExpr(ir, env);
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
                    var anno_node = anno.toSExpr(ir, env);
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

                    var type_node = ir.store.getTypeAnno(field.ty).toSExpr(ir, env);
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
                    var arg_node = arg.toSExpr(ir, env);
                    node.appendNode(gpa, &arg_node);
                }

                var ret_node = ir.store.getTypeAnno(f.ret).toSExpr(ir, env);
                node.appendNode(gpa, &ret_node);

                node.appendBoolAttr(gpa, "effectful", f.effectful);

                return node;
            },
            .parens => |p| {
                var node = SExpr.init(gpa, "ty-parens");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const inner_anno = ir.store.getTypeAnno(p.anno);
                var inner_node = inner_anno.toSExpr(ir, env);
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

    pub fn toSExpr(self: *const @This(), ir: *CIR, env: *ModuleEnv) SExpr {
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
                var arg_node = arg.toSExpr(ir, env);
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

    pub fn toSExpr(self: ExposedItem, gpa: std.mem.Allocator, env: *const ModuleEnv) SExpr {
        var node = SExpr.init(gpa, "exposed");

        // Add the original name
        node.appendStringAttr(gpa, "name", env.idents.getText(self.name));

        // Add the alias if present
        if (self.alias) |alias_idx| {
            node.appendStringAttr(gpa, "alias", env.idents.getText(alias_idx));
        }

        // Add wildcard indicator if needed
        node.appendBoolAttr(gpa, "wildcard", self.is_wildcard);

        return node;
    }
};

/// An expression that has been canonicalized.
pub const Expr = union(enum) {
    num: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int.Precision,
        region: Region,
    },
    int: struct {
        int_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int.Precision,
        region: Region,
    },
    float: struct {
        frac_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Frac.Precision,
        region: Region,
    },
    // A single segment of a string literal
    // a single string may be made up of a span sequential segments
    // for example if it was split across multiple lines
    str_segment: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    // A string is combined of one or more segments, some of which may be interpolated
    // An interpolated string contains one or more non-string_segment's in the span
    str: struct {
        span: Expr.Span,
        region: Region,
    },
    single_quote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.Num.Int.Precision,
        region: Region,
    },
    lookup: union(enum) {
        local: Lookup,
        external: ExternalDecl.Idx,
    },
    // TODO introduce a new node for re-assign here, used by Var instead of lookup
    list: struct {
        elem_var: TypeVar,
        elems: Expr.Span,
        region: Region,
    },
    tuple: struct {
        tuple_var: TypeVar,
        elems: Expr.Span,
        region: Region,
    },
    when: When,
    @"if": struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranch.Span,
        final_else: Expr.Idx,
        region: Region,
    },
    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    call: struct {
        args: Expr.Span,
        called_via: CalledVia,
        region: Region,
    },
    record: struct {
        ext_var: TypeVar,
        region: Region,
        // TODO:
        // fields: SendMap<Lowercase, Field>,
    },
    /// Empty record constant
    empty_record: struct {
        region: Region,
    },
    block: struct {
        /// Statements executed in sequence
        stmts: Statement.Span,
        /// Final expression that produces the block's value
        final_expr: Expr.Idx,
        region: Region,
    },
    record_access: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: Expr.Idx,
        field: Ident.Idx,
        region: Region,
    },
    tag: struct {
        ext_var: TypeVar,
        name: Ident.Idx,
        args: Expr.Span,
        region: Region,
    },
    zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
        region: Region,
    },
    lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: Region,
    },
    binop: Binop,
    /// Dot access that could be either record field access or static dispatch
    /// The decision is deferred until after type inference based on the receiver's type
    dot_access: struct {
        receiver: Expr.Idx, // Expression before the dot (e.g., `list` in `list.map`)
        field_name: Ident.Idx, // Identifier after the dot (e.g., `map` in `list.map`)
        args: ?Expr.Span, // Optional arguments for method calls (e.g., `fn` in `list.map(fn)`)
        region: Region,
    },
    /// Compiles, but will crash if reached
    runtime_error: struct {
        diagnostic: Diagnostic.Idx,
        region: Region,
    },

    pub const Lookup = struct {
        pattern_idx: Pattern.Idx,
        region: Region,
    };

    pub const Idx = enum(u32) { _ };

    pub const Span = struct { span: DataSpan };

    pub fn init_str(expr_span: Expr.Span, region: Region) Expr {
        return .{ .str = .{
            .span = expr_span,
            .region = region,
        } };
    }

    pub fn init_str_segment(literal: StringLiteral.Idx, region: Region) Expr {
        return .{ .str_segment = .{
            .literal = literal,
            .region = region,
        } };
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
        };

        pub fn init(op: Op, lhs: Expr.Idx, rhs: Expr.Idx, region: Region) Binop {
            return .{ .lhs = lhs, .op = op, .rhs = rhs, .region = region };
        }
    };

    pub fn toRegion(self: *const @This()) ?Region {
        switch (self.*) {
            .num => |e| return e.region,
            .int => |e| return e.region,
            .float => |e| return e.region,
            .str_segment => |e| return e.region,
            .str => |e| return e.region,
            .single_quote => |e| return e.region,
            .lookup => |e| switch (e) {
                .local => |local| return local.region,
                .external => |_| {
                    // External lookups don't have a direct region access from Expr context
                    // The region should be handled where the CIR context is available
                    return null;
                },
            },
            .list => |e| return e.region,
            .tuple => |e| return e.region,
            .when => |e| return e.region,
            .@"if" => |e| return e.region,
            .call => |e| return e.region,
            .record => |e| return e.region,
            .empty_record => |e| return e.region,
            .record_access => |e| return e.region,
            .dot_access => |e| return e.region,
            .tag => |e| return e.region,
            .zero_argument_tag => |e| return e.region,
            .binop => |e| return e.region,
            .block => |e| return e.region,
            .lambda => |e| return e.region,
            .runtime_error => |e| return e.region,
        }
    }

    pub fn toSExpr(self: *const @This(), ir: *CIR, env: *ModuleEnv) SExpr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .num => |num_expr| {
                var node = SExpr.init(gpa, "e-num");
                node.appendRegion(gpa, ir.calcRegionInfo(num_expr.region));

                // Add num_var
                node.appendTypeVar(gpa, "num-var", num_expr.num_var);

                // Add literal
                node.appendStringAttr(gpa, "literal", ir.env.strings.get(num_expr.literal));

                // Add value info
                // TODO: Format the actual integer value properly
                node.appendStringAttr(gpa, "value", "TODO");

                // Add bound info
                node.appendStringAttr(gpa, "bound", @tagName(num_expr.bound));

                return node;
            },
            .int => |int_expr| {
                var node = SExpr.init(gpa, "e-int");
                node.appendRegion(gpa, ir.calcRegionInfo(int_expr.region));

                // Add int_var
                node.appendTypeVar(gpa, "int-var", int_expr.int_var);

                // Add precision_var
                node.appendTypeVar(gpa, "precision-var", int_expr.precision_var);

                // Add literal
                node.appendStringAttr(gpa, "literal", ir.env.strings.get(int_expr.literal));

                // Add value info
                node.appendStringAttr(gpa, "value", "TODO");

                // Add bound info
                node.appendStringAttr(gpa, "bound", @tagName(int_expr.bound));

                return node;
            },
            .float => |float_expr| {
                var node = SExpr.init(gpa, "e-float");
                node.appendRegion(gpa, ir.calcRegionInfo(float_expr.region));

                // Add frac_var
                node.appendTypeVar(gpa, "frac_var", float_expr.frac_var);

                // Add precision_var
                node.appendTypeVar(gpa, "precision-var", float_expr.precision_var);

                // Add literal
                node.appendStringAttr(gpa, "literal", ir.env.strings.get(float_expr.literal));

                // Add value
                const value_str = std.fmt.allocPrint(gpa, "{d}", .{float_expr.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                node.appendStringAttr(gpa, "value", value_str);

                // Add bound info
                node.appendStringAttr(gpa, "bound", @tagName(float_expr.bound));

                return node;
            },
            .str_segment => |e| {
                var str_node = SExpr.init(gpa, "e-literal");
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                const value = ir.env.strings.get(e.literal);
                str_node.appendStringAttr(gpa, "string", value);

                return str_node;
            },
            .str => |e| {
                var str_node = SExpr.init(gpa, "e-string");
                str_node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                for (ir.store.sliceExpr(e.span)) |segment| {
                    var segment_node = ir.store.getExpr(segment).toSExpr(ir, env);
                    str_node.appendNode(gpa, &segment_node);
                }

                return str_node;
            },
            .single_quote => |e| {
                var node = SExpr.init(gpa, "e-single-quote");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                // Add num_var
                node.appendTypeVar(gpa, "num-var", e.num_var);

                // Add precision_var
                node.appendTypeVar(gpa, "precision-var", e.precision_var);

                // Add value
                const value_str = std.fmt.allocPrint(gpa, "'\\u{{{x}}}'", .{e.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                node.appendStringAttr(gpa, "value", value_str);

                // Add bound info
                node.appendStringAttr(gpa, "bound", @tagName(e.bound));

                return node;
            },
            .list => |l| {
                var list_node = SExpr.init(gpa, "e-list");
                list_node.appendRegion(gpa, ir.calcRegionInfo(l.region));

                // Add elem_var
                list_node.appendTypeVar(gpa, "elem-var", l.elem_var);

                // Add list elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(l.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir, env);
                    elems_node.appendNode(gpa, &elem_node);
                }
                list_node.appendNode(gpa, &elems_node);

                return list_node;
            },
            .tuple => |t| {
                var node = SExpr.init(gpa, "e-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(t.region));

                // Add tuple_var
                node.appendTypeVar(gpa, "tuple-var", t.tuple_var);

                // Add tuple elements
                var elems_node = SExpr.init(gpa, "elems");
                for (ir.store.sliceExpr(t.elems)) |elem_idx| {
                    var elem_node = ir.store.getExpr(elem_idx).toSExpr(ir, env);
                    elems_node.appendNode(gpa, &elem_node);
                }
                node.appendNode(gpa, &elems_node);

                return node;
            },
            .lookup => |l| {
                switch (l) {
                    .local => |local| {
                        var lookup_node = SExpr.init(gpa, "e-lookup-local");
                        lookup_node.appendRegion(gpa, ir.calcRegionInfo(local.region));

                        var pattern_node = SExpr.init(gpa, "pattern");
                        pattern_node.appendIdx(gpa, local.pattern_idx);
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
            .when => |e| {
                var node = SExpr.init(gpa, "e-match");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                node.appendStringAttr(gpa, "match", "TODO");

                return node;
            },
            .@"if" => |if_expr| {
                var node = SExpr.init(gpa, "e-if");
                node.appendRegion(gpa, ir.calcRegionInfo(if_expr.region));

                // Add cond_var
                node.appendTypeVar(gpa, "cond-var", if_expr.cond_var);

                // Add branch_var
                node.appendTypeVar(gpa, "branch-var", if_expr.branch_var);

                // Add branches
                var branches_node = SExpr.init(gpa, "if-branches");
                const branch_indices = ir.store.sliceIfBranches(if_expr.branches);
                for (branch_indices) |branch_idx| {
                    const branch = ir.store.getIfBranch(branch_idx);

                    var branch_node = SExpr.init(gpa, "if-branch");

                    // Add condition
                    const cond_expr = ir.store.getExpr(branch.cond);
                    var cond_node = cond_expr.toSExpr(ir, env);
                    branch_node.appendNode(gpa, &cond_node);

                    // Add body
                    const body_expr = ir.store.getExpr(branch.body);
                    var body_node = body_expr.toSExpr(ir, env);
                    branch_node.appendNode(gpa, &body_node);

                    branches_node.appendNode(gpa, &branch_node);
                }
                node.appendNode(gpa, &branches_node);

                // Add final_else
                var else_node = SExpr.init(gpa, "if-else");
                const else_expr = ir.store.getExpr(if_expr.final_else);
                var else_expr_node = else_expr.toSExpr(ir, env);
                else_node.appendNode(gpa, &else_expr_node);
                node.appendNode(gpa, &else_node);

                return node;
            },
            .call => |c| {
                var call_node = SExpr.init(gpa, "e-call");
                call_node.appendRegion(gpa, ir.calcRegionInfo(c.region));

                // Get all expressions from the args span
                const all_exprs = ir.store.exprSlice(c.args);

                // First element is the function being called
                if (all_exprs.len > 0) {
                    const fn_expr = ir.store.getExpr(all_exprs[0]);
                    var fn_node = fn_expr.toSExpr(ir, env);
                    call_node.appendNode(gpa, &fn_node);
                }

                // Remaining elements are the arguments
                if (all_exprs.len > 1) {
                    for (all_exprs[1..]) |arg_idx| {
                        const arg_expr = ir.store.getExpr(arg_idx);
                        var arg_node = arg_expr.toSExpr(ir, env);
                        call_node.appendNode(gpa, &arg_node);
                    }
                }

                return call_node;
            },
            .record => |record_expr| {
                var record_node = SExpr.init(gpa, "e-record");
                record_node.appendRegion(gpa, ir.calcRegionInfo(record_expr.region));

                // Add record_var
                record_node.appendTypeVar(gpa, "ext-var", record_expr.ext_var);

                // TODO: Add fields when implemented
                record_node.appendStringAttr(gpa, "fields", "TODO");

                return record_node;
            },
            .empty_record => |e| {
                var empty_record_node = SExpr.init(gpa, "e-empty_record");
                empty_record_node.appendRegion(gpa, ir.calcRegionInfo(e.region));
                return empty_record_node;
            },
            .block => |block_expr| {
                var block_node = SExpr.init(gpa, "e-block");
                block_node.appendRegion(gpa, ir.calcRegionInfo(block_expr.region));

                // Add statements
                for (ir.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    var stmt_node = ir.store.getStatement(stmt_idx).toSExpr(ir, env);
                    block_node.appendNode(gpa, &stmt_node);
                }

                // Add final expression
                var expr_node = ir.store.getExpr(block_expr.final_expr).toSExpr(ir, env);
                block_node.appendNode(gpa, &expr_node);

                return block_node;
            },
            .record_access => |access_expr| {
                var node = SExpr.init(gpa, "e-record_access");
                node.appendRegion(gpa, ir.calcRegionInfo(access_expr.region));

                // Add record_var
                node.appendTypeVar(gpa, "record_var", access_expr.record_var);

                // Add ext_var
                node.appendTypeVar(gpa, "ext-var", access_expr.ext_var);

                // Add field_var
                node.appendTypeVar(gpa, "field_var", access_expr.field_var);

                // Add loc_expr
                var loc_expr_node = ir.store.getExpr(access_expr.loc_expr).toSExpr(ir, env);
                node.appendNode(gpa, &loc_expr_node);

                // Add field
                node.appendStringAttr(gpa, "field", ir.env.idents.getText(access_expr.field));

                return node;
            },
            .tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add ext_var
                node.appendTypeVar(gpa, "ext-var", tag_expr.ext_var);

                // Add name
                node.appendStringAttr(gpa, "name", ir.env.idents.getText(tag_expr.name));

                // Add args
                node.appendStringAttr(gpa, "args", "TODO");

                return node;
            },
            .zero_argument_tag => |tag_expr| {
                var node = SExpr.init(gpa, "e-zero-argument-tag");
                node.appendRegion(gpa, ir.calcRegionInfo(tag_expr.region));

                // Add closure_name
                node.appendStringAttr(gpa, "closure", ir.getIdentText(tag_expr.closure_name));

                // Add variant_var
                node.appendTypeVar(gpa, "variant-var", tag_expr.variant_var);

                // Add ext_var
                node.appendTypeVar(gpa, "ext-var", tag_expr.ext_var);

                // Add name
                node.appendStringAttr(gpa, "name", ir.getIdentText(tag_expr.name));

                return node;
            },
            .lambda => |lambda_expr| {
                var node = SExpr.init(gpa, "e-lambda");
                node.appendRegion(gpa, ir.calcRegionInfo(lambda_expr.region));

                // Handle args span
                var args_node = SExpr.init(gpa, "args");
                for (ir.store.slicePatterns(lambda_expr.args)) |arg_idx| {
                    var pattern_node = ir.store.getPattern(arg_idx).toSExpr(ir);
                    pattern_node.appendIdx(gpa, arg_idx);
                    args_node.appendNode(gpa, &pattern_node);
                }
                node.appendNode(gpa, &args_node);

                // Handle body
                var body_node = ir.store.getExpr(lambda_expr.body).toSExpr(ir, env);
                node.appendNode(gpa, &body_node);

                return node;
            },
            .binop => |e| {
                var node = SExpr.init(gpa, "e-binop");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                node.appendStringAttr(gpa, "op", @tagName(e.op));

                var lhs_node = ir.store.getExpr(e.lhs).toSExpr(ir, env);
                node.appendNode(gpa, &lhs_node);

                var rhs_node = ir.store.getExpr(e.rhs).toSExpr(ir, env);
                node.appendNode(gpa, &rhs_node);

                return node;
            },
            .dot_access => |e| {
                var node = SExpr.init(gpa, "e-dot-access");
                node.appendRegion(gpa, ir.calcRegionInfo(e.region));

                var receiver_node = SExpr.init(gpa, "receiver");
                var expr_node = ir.store.getExpr(e.receiver).toSExpr(ir, env);
                receiver_node.appendNode(gpa, &expr_node);
                node.appendNode(gpa, &receiver_node);

                node.appendStringAttr(gpa, "field", ir.getIdentText(e.field_name));

                if (e.args) |args| {
                    var args_node = SExpr.init(gpa, "args");
                    for (ir.store.exprSlice(args)) |arg_idx| {
                        var arg_node = ir.store.getExpr(arg_idx).toSExpr(ir, env);
                        args_node.appendNode(gpa, &arg_node);
                    }
                    node.appendNode(gpa, &args_node);
                }

                return node;
            },
            .runtime_error => |e| {
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
        appendIdent(&node, gpa, ir.env, "ident", self.ident);
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

    pub fn toSExpr(self: *const @This(), ir: *CIR, env: *ModuleEnv) SExpr {
        const gpa = ir.env.gpa;

        const kind = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };

        var node = SExpr.init(gpa, kind);

        var pattern_node = ir.store.getPattern(self.pattern).toSExpr(ir);
        pattern_node.appendIdx(gpa, self.pattern);
        node.appendNode(gpa, &pattern_node);

        var expr_node = ir.store.getExpr(self.expr).toSExpr(ir, env);
        expr_node.appendIdx(gpa, self.expr);
        node.appendNode(gpa, &expr_node);

        if (self.annotation) |anno_idx| {
            var anno_node = ir.store.getAnnotation(anno_idx).toSExpr(ir, env.line_starts);
            anno_node.appendIdx(gpa, anno_idx);
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

        // Add the signature type variable info
        node.appendTypeVar(gpa, "signature", self.signature);

        // Add the declared type annotation structure
        var type_anno_node = SExpr.init(gpa, "declared-type");
        const type_anno = ir.store.getTypeAnno(self.type_anno);
        var anno_sexpr = type_anno.toSExpr(ir, ir.env);
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

        node.appendTypeVar(gpa, "type-var", self.type_var);

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

/// TODO
pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: Expr.Idx,
    cond_var: TypeVar,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeVar,
    region: Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Span,
    branches_cond_var: TypeVar,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "when");

        node.appendRegion(gpa, self.region);

        var cond_node = SExpr.init(gpa, "cond");
        const cond_expr = ir.store.getExpr(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(ir, line_starts);
        cond_node.appendNode(gpa, &cond_sexpr);

        node.appendNode(gpa, &cond_node);

        node.appendTypeVar(gpa, "cond-var", self.cond_var);
        node.appendTypeVar(gpa, "expr-var", self.expr_var);
        node.appendTypeVar(gpa, "branches-cond-var", self.branches_cond_var);
        node.appendTypeVar(gpa, "exhaustive-mark", self.exhaustive);

        var branches_node = SExpr.init(gpa, "branches");
        for (ir.store.whenBranchSlice(self.branches)) |branch_idx| {
            const branch = ir.store.getWhenBranch(branch_idx);

            var branch_sexpr = branch.toSExpr(ir);
            branches_node.appendNode(gpa, &branch_sexpr);
        }
        node.appendNode(gpa, &branches_node);

        return node;
    }
};

/// todo - evaluate if we need this?
pub const WhenBranchPattern = struct {
    pattern: Pattern.Idx,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        _ = line_starts;
        const gpa = ir.gpa;
        var node = SExpr.init(gpa, "match-branch-pattern");
        var pattern_sexpr = self.pattern.toSExpr(ir);
        node.appendNode(gpa, &pattern_sexpr);
        node.appendBoolAttr(gpa, "degenerate", self.degenerate);
        return node;
    }
};

/// todo - evaluate if we need this?
pub const WhenBranch = struct {
    patterns: WhenBranchPattern.Span,
    value: Expr.Idx,
    guard: ?Expr.Idx,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,

    pub fn toSExpr(self: *const @This(), ir: *const CIR, line_starts: std.ArrayList(u32)) SExpr {
        const gpa = ir.env.gpa;
        var node = SExpr.init(gpa, "match-branch");

        var patterns_node = SExpr.init(gpa, "patterns");
        patterns_node.appendStringAttr(gpa, "match", "TODO");
        node.appendNode(gpa, &patterns_node);

        var value_node = SExpr.init(gpa, "value");
        const value_expr = ir.exprs_at_regions.get(self.value);
        var value_sexpr = value_expr.toSExpr(ir, line_starts);
        value_node.appendNode(gpa, &value_sexpr);
        node.appendNode(gpa, &value_node);

        if (self.guard) |guard_idx| {
            var guard_node = SExpr.init(gpa, "guard");
            const guard_expr = ir.exprs_at_regions.get(guard_idx);
            var guard_sexpr = guard_expr.toSExpr(ir, line_starts);
            guard_node.appendNode(gpa, &guard_sexpr);
            node.appendNode(gpa, &guard_node);
        }

        return node;
    }

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
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
        patterns: Pattern.Span,
        region: Region,
    },
    tuple: struct {
        tuple_var: TypeVar,
        patterns: Pattern.Span,
        region: Region,
    },
    num_literal: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int.Precision,
        region: Region,
    },
    int_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int.Precision,
        region: Region,
    },
    float_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Frac.Precision,
        region: Region,
    },
    str_literal: struct {
        literal: StringLiteral.Idx,
        region: Region,
    },
    char_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.Num.Int.Precision,
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
            .num_literal => |p| return p.region,
            .int_literal => |p| return p.region,
            .float_literal => |p| return p.region,
            .str_literal => |p| return p.region,
            .char_literal => |p| return p.region,
            .underscore => |p| return p.region,
            .runtime_error => |p| return p.region,
        }
    }

    pub fn toSExpr(self: *const @This(), ir: *CIR) SExpr {
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
                destructs_node.appendStringAttr(gpa, "node", "TODO");
                node.appendNode(gpa, &destructs_node);

                return node;
            },
            .list => |p| {
                var node = SExpr.init(gpa, "p-list");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patt_sexpr.appendIdx(gpa, patt_idx);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                return node;
            },
            .tuple => |p| {
                var node = SExpr.init(gpa, "p-tuple");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                // var pattern_idx_node = formatPatternIdxNode(gpa, pattern_idx);
                // node.appendNode(gpa, &pattern_idx_node);

                // Add tuple_var
                node.appendTypeVar(gpa, "tuple-var", p.tuple_var);

                var patterns_node = SExpr.init(gpa, "patterns");

                for (ir.store.slicePatterns(p.patterns)) |patt_idx| {
                    var patt_sexpr = ir.store.getPattern(patt_idx).toSExpr(ir);
                    patt_sexpr.appendIdx(gpa, patt_idx);
                    patterns_node.appendNode(gpa, &patt_sexpr);
                }

                node.appendNode(gpa, &patterns_node);

                return node;
            },
            .num_literal => |p| {
                var node = SExpr.init(gpa, "p-num");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .int_literal => |p| {
                var node = SExpr.init(gpa, "p-int");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));
                return node;
            },
            .float_literal => |p| {
                var node = SExpr.init(gpa, "p-float");
                node.appendRegion(gpa, ir.calcRegionInfo(p.region));

                const val_str = std.fmt.allocPrint(gpa, "{d}", .{p.value}) catch "<oom>";
                defer gpa.free(val_str);

                node.appendStringAttr(gpa, "value", val_str);
                node.appendStringAttr(gpa, "bound", @tagName(p.bound));

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
                node.appendStringAttr(gpa, "bound", @tagName(l.bound));
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
    type_var: TypeVar,
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

            switch (self.*) {
                .Required => return SExpr.init(gpa, "required"),
                .Guard => |guard_idx| {
                    var guard_kind_node = SExpr.init(gpa, "guard");

                    const guard_patt = ir.typed_patterns_at_regions.get(guard_idx);
                    var guard_sexpr = guard_patt.toSExpr(ir.env, ir, line_starts);
                    guard_kind_node.appendNode(gpa, &guard_sexpr);

                    return guard_kind_node;
                },
            }
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const CIR) SExpr {
        const gpa = ir.env.gpa;

        var record_destruct_node = SExpr.init(gpa, "record-destruct");

        record_destruct_node.appendTypeVar(&record_destruct_node, gpa, "type-var", self.type_var);
        record_destruct_node.appendRegion(gpa, ir.calcRegionInfo(self.region));

        appendIdent(&record_destruct_node, gpa, ir, "label", self.label);
        appendIdent(&record_destruct_node, gpa, ir, "ident", self.ident);

        var kind_node = self.kind.toSExpr(ir);
        record_destruct_node.appendNode(gpa, &kind_node);

        return record_destruct_node;
    }
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = TypeVar;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeVar;

/// Helper function to convert the entire Canonical IR to a string in S-expression format
/// and write it to the given writer.
///
/// If a single expression is provided we only print that expression
pub fn toSExprStr(ir: *CIR, env: *ModuleEnv, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, source: []const u8) !void {
    // Set temporary source for region info calculation during SExpr generation
    ir.temp_source_for_sexpr = source;
    defer ir.temp_source_for_sexpr = null;
    const gpa = ir.env.gpa;

    if (maybe_expr_idx) |expr_idx| {
        // Get the expression from the store
        var expr_node = ir.store.getExpr(expr_idx).toSExpr(ir, env);
        expr_node.appendIdx(gpa, expr_idx);
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
            var def_node = ir.store.getDef(def_idx).toSExpr(ir, env);
            def_node.appendIdx(gpa, def_idx);
            root_node.appendNode(gpa, &def_node);
        }

        for (statements_slice) |stmt_idx| {
            var stmt_node = ir.store.getStatement(stmt_idx).toSExpr(ir, env);
            stmt_node.appendIdx(gpa, stmt_idx);
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

        expr_node.appendIdx(gpa, expr_idx);

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

                    // Get the type of the expression
                    const def_var = @as(types.Var, @enumFromInt(@intFromEnum(def_idx)));

                    var def_node = SExpr.init(gpa, "def");
                    def_node.appendStringAttr(gpa, "name", ir.env.idents.getText(assign_pat.ident));
                    // def_node.appendUnsignedInt(gpa, @intFromEnum(def_var));

                    if (@intFromEnum(def_var) > ir.env.types.slots.backing.items.len) {
                        const unknown_node = SExpr.init(gpa, "unknown");
                        def_node.appendNode(gpa, &unknown_node);
                    } else {

                        // Clear the buffer and write the type
                        type_string_buf.clearRetainingCapacity();
                        if (type_writer.writeVar(def_var)) {
                            def_node.appendStringAttr(gpa, "type", type_string_buf.items);
                        } else |err| {
                            exitOnOom(err);
                        }
                    }
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
                if (type_writer.writeVar(expr_var)) {
                    expr_node.appendStringAttr(gpa, "type", type_string_buf.items);
                } else |err| {
                    exitOnOom(err);
                }
            }

            expressions_node.appendNode(gpa, &expr_node);
        }

        root_node.appendNode(gpa, &expressions_node);

        root_node.toStringPretty(writer);
    }
}
