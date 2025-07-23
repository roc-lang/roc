//! Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.
//!
//! This module performs semantic analysis, resolves scoping, and transforms high-level language
//! constructs into a simplified, normalized form suitable for type inference.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const tracy = @import("../tracy.zig");
const parse = @import("parse.zig");
const tokenize = @import("parse/tokenize.zig");
const collections = @import("collections");
const types = @import("types");
const types_mod = types;
const RocDecBuiltins = @import("builtins").RocDec;

const Scope = @import("./canonicalize/Scope.zig");
// Import from compile module files directly
pub const ModuleEnv = @import("../compile/ModuleEnv.zig");
const CompileNodeStore = @import("../compile/NodeStore.zig");
// Node is already exported from ModuleEnv

const AST = parse.AST;
const Token = tokenize.Token;
// ModuleEnv is now imported from compile/ at the top

const TypeVarProblemKind = enum {
    unused_type_var,
    type_var_marked_unused,
    type_var_ending_in_underscore,
};

const TypeVarProblem = struct {
    ident: Ident.Idx,
    problem: TypeVarProblemKind,
    ast_anno: AST.TypeAnno.Idx,
};

env: *ModuleEnv,
parse_ir: *AST,
scopes: std.ArrayListUnmanaged(Scope) = .{},
/// Special scope for tracking exposed items from module header
exposed_scope: Scope = undefined,
/// Track exposed identifiers by text to handle changing indices
exposed_ident_texts: std.StringHashMapUnmanaged(Region) = .{},
/// Track exposed types by text to handle changing indices
exposed_type_texts: std.StringHashMapUnmanaged(Region) = .{},
/// Special scope for unqualified nominal tags (e.g., True, False)
unqualified_nominal_tags: std.StringHashMapUnmanaged(Statement.Idx) = .{},
/// Stack of function regions for tracking var reassignment across function boundaries
function_regions: std.ArrayListUnmanaged(Region),
/// Maps var patterns to the function region they were declared in
var_function_regions: std.AutoHashMapUnmanaged(Pattern.Idx, Region),
/// Set of pattern indices that are vars
var_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Tracks which pattern indices have been used/referenced
used_patterns: std.AutoHashMapUnmanaged(Pattern.Idx, void),
/// Map of module name strings to their ModuleEnv pointers for import validation
module_envs: ?*const std.StringHashMap(*ModuleEnv),
/// Map from module name string to Import.Idx for tracking unique imports
import_indices: std.StringHashMapUnmanaged(Import.Idx),
/// Scratch type variables
scratch_vars: base.Scratch(TypeVar),
/// Scratch ident
scratch_idents: base.Scratch(Ident.Idx),
/// Scratch type variable identifiers for underscore validation
scratch_type_var_validation: base.Scratch(Ident.Idx),
/// Scratch type variable problems
scratch_type_var_problems: base.Scratch(TypeVarProblem),
/// Scratch ident
scratch_record_fields: base.Scratch(types.RecordField),
/// Scratch ident
scratch_seen_record_fields: base.Scratch(SeenRecordField),
/// Scratch tags
scratch_tags: base.Scratch(types.Tag),

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
const CalledVia = base.CalledVia;

const TypeVar = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Num = types.Num;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

/// Struct to track fields that have been seen before during canonicalization
const SeenRecordField = struct { ident: base.Ident.Idx, region: base.Region };

/// The idx of the builtin Bool
pub const BUILTIN_BOOL: Pattern.Idx = @enumFromInt(0);
/// The idx of the builtin Box
pub const BUILTIN_BOX: Pattern.Idx = @enumFromInt(1);
/// The idx of the builtin Decode
pub const BUILTIN_DECODE: Pattern.Idx = @enumFromInt(2);
/// The idx of the builtin Dict
pub const BUILTIN_DICT: Pattern.Idx = @enumFromInt(3);
/// The idx of the builtin Encode
pub const BUILTIN_ENCODE: Pattern.Idx = @enumFromInt(4);
/// The idx of the builtin Hash
pub const BUILTIN_HASH: Pattern.Idx = @enumFromInt(5);
/// The idx of the builtin Inspect
pub const BUILTIN_INSPECT: Pattern.Idx = @enumFromInt(6);
/// The idx of the builtin List
pub const BUILTIN_LIST: Pattern.Idx = @enumFromInt(7);
/// The idx of the builtin Num
pub const BUILTIN_NUM: Pattern.Idx = @enumFromInt(8);
/// The idx of the builtin Result
pub const BUILTIN_RESULT: Pattern.Idx = @enumFromInt(9);
/// The idx of the builtin Set
pub const BUILTIN_SET: Pattern.Idx = @enumFromInt(10);
/// The idx of the builtin Str
pub const BUILTIN_STR: Pattern.Idx = @enumFromInt(11);

/// Deinitialize canonicalizer resources
pub fn deinit(
    self: *Self,
) void {
    const gpa = self.env.gpa;

    self.exposed_scope.deinit(gpa);
    self.exposed_ident_texts.deinit(gpa);
    self.exposed_type_texts.deinit(gpa);
    self.unqualified_nominal_tags.deinit(gpa);

    for (0..self.scopes.items.len) |i| {
        var scope = &self.scopes.items[i];
        scope.deinit(gpa);
    }

    self.scopes.deinit(gpa);
    self.function_regions.deinit(gpa);
    self.var_function_regions.deinit(gpa);
    self.var_patterns.deinit(gpa);
    self.used_patterns.deinit(gpa);
    self.scratch_vars.deinit(gpa);
    self.scratch_idents.deinit(gpa);
    self.scratch_type_var_validation.deinit(gpa);
    self.scratch_type_var_problems.deinit(gpa);
    self.scratch_record_fields.deinit(gpa);
    self.scratch_seen_record_fields.deinit(gpa);
    self.import_indices.deinit(gpa);
    self.scratch_tags.deinit(gpa);
}

pub fn init(env: *ModuleEnv, parse_ir: *AST, module_envs: ?*const std.StringHashMap(*ModuleEnv)) std.mem.Allocator.Error!Self {
    const gpa = env.gpa;

    // Create the canonicalizer with scopes
    var result = Self{
        .env = env,
        .parse_ir = parse_ir,
        .scopes = .{},
        .function_regions = std.ArrayListUnmanaged(Region){},
        .var_function_regions = std.AutoHashMapUnmanaged(Pattern.Idx, Region){},
        .var_patterns = std.AutoHashMapUnmanaged(Pattern.Idx, void){},
        .used_patterns = std.AutoHashMapUnmanaged(Pattern.Idx, void){},
        .module_envs = module_envs,
        .import_indices = std.StringHashMapUnmanaged(Import.Idx){},
        .scratch_vars = try base.Scratch(TypeVar).init(gpa),
        .scratch_idents = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_type_var_validation = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_type_var_problems = try base.Scratch(TypeVarProblem).init(gpa),
        .scratch_record_fields = try base.Scratch(types.RecordField).init(gpa),
        .scratch_seen_record_fields = try base.Scratch(SeenRecordField).init(gpa),
        .exposed_scope = Scope.init(false),
        .scratch_tags = try base.Scratch(types.Tag).init(gpa),
        .unqualified_nominal_tags = std.StringHashMapUnmanaged(Statement.Idx){},
    };

    // Top-level scope is not a function boundary
    try result.scopeEnter(gpa, false);

    // Simulate the builtins by adding to both the NodeStore and Scopes
    // Not sure if this is how we want to do it long term, but want something to
    // make a start on canonicalization.

    // Assert that the node store is completely empty
    // Arrays are now managed by ModuleEnv's NodeStore, no sync needed

    // Add builtinss (eventually will be gotten from builtins roc files)
    try result.addBuiltin(env, "Bool", BUILTIN_BOOL);
    try result.addBuiltin(env, "Box", BUILTIN_BOX);
    try result.addBuiltin(env, "Decode", BUILTIN_DECODE);
    try result.addBuiltin(env, "Dict", BUILTIN_DICT);
    try result.addBuiltin(env, "Encode", BUILTIN_ENCODE);
    try result.addBuiltin(env, "Hash", BUILTIN_HASH);
    try result.addBuiltin(env, "Inspect", BUILTIN_INSPECT);
    try result.addBuiltin(env, "List", BUILTIN_LIST);
    try result.addBuiltin(env, "Num", BUILTIN_NUM);
    try result.addBuiltin(env, "Result", BUILTIN_RESULT);
    try result.addBuiltin(env, "Set", BUILTIN_SET);
    try result.addBuiltin(env, "Str", BUILTIN_STR);

    // Assert that the node store has the 12 builtin types
    // Arrays are now managed by ModuleEnv's NodeStore, no sync needed

    // Add built-in types to the type scope
    // TODO: These should ultimately come from the platform/builtin files rather than being hardcoded
    try result.addBuiltinTypeBool(env);

    _ = try result.addBuiltinType(env, "Str", .{ .structure = .str });
    _ = try result.addBuiltinType(env, "U8", .{ .structure = .{ .num = types.Num.int_u8 } });
    _ = try result.addBuiltinType(env, "U16", .{ .structure = .{ .num = types.Num.int_u16 } });
    _ = try result.addBuiltinType(env, "U32", .{ .structure = .{ .num = types.Num.int_u32 } });
    _ = try result.addBuiltinType(env, "U64", .{ .structure = .{ .num = types.Num.int_u64 } });
    _ = try result.addBuiltinType(env, "U128", .{ .structure = .{ .num = types.Num.int_u128 } });
    _ = try result.addBuiltinType(env, "I8", .{ .structure = .{ .num = types.Num.int_i8 } });
    _ = try result.addBuiltinType(env, "I16", .{ .structure = .{ .num = types.Num.int_i16 } });
    _ = try result.addBuiltinType(env, "I32", .{ .structure = .{ .num = types.Num.int_i32 } });
    _ = try result.addBuiltinType(env, "I64", .{ .structure = .{ .num = types.Num.int_i64 } });
    _ = try result.addBuiltinType(env, "I128", .{ .structure = .{ .num = types.Num.int_i128 } });
    _ = try result.addBuiltinType(env, "F32", .{ .structure = .{ .num = types.Num.frac_f32 } });
    _ = try result.addBuiltinType(env, "F64", .{ .structure = .{ .num = types.Num.frac_f64 } });
    _ = try result.addBuiltinType(env, "Dec", .{ .structure = .{ .num = types.Num.frac_dec } });
    _ = try result.addBuiltinType(env, "Result", .{ .flex_var = null });
    _ = try result.addBuiltinType(env, "List", .{ .flex_var = null });
    _ = try result.addBuiltinType(env, "Dict", .{ .flex_var = null });
    _ = try result.addBuiltinType(env, "Set", .{ .flex_var = null });
    _ = try result.addBuiltinType(env, "Box", .{ .flex_var = null });

    return result;
}

fn addBuiltin(self: *Self, ir: *ModuleEnv, ident_text: []const u8, idx: Pattern.Idx) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const ident_store = &ir.idents;
    const ident_add = try ir.idents.insert(gpa, base.Ident.for_text(ident_text), Region.zero());
    const pattern_idx_add = try ir.addPatternAndTypeVar(
        Pattern{ .assign = .{ .ident = ident_add } },
        .{ .flex_var = null },
        Region.zero()
    );
    _ = try self.scopeIntroduceInternal(gpa, ident_store, .ident, ident_add, pattern_idx_add, false, true);
    std.debug.assert(idx == pattern_idx_add);
}

/// Stub builtin types. Currently sets every type to be a nominal type
/// This should be replaced by real builtins eventually
fn addBuiltinType(self: *Self, ir: *ModuleEnv, type_name: []const u8, _: types.Content) std.mem.Allocator.Error!Statement.Idx {
    const gpa = ir.gpa;
    const type_ident = try ir.idents.insert(gpa, base.Ident.for_text(type_name), Region.zero());

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());

    // Create a type annotation that refers to itself (built-in types are primitive)
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, .{ .flex_var = null }, Region.zero());
    const anno_var = castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_alias_decl = .{
            .header = header_idx,
            .anno = anno_idx,
            .anno_var = anno_var,
            .where = null,
        },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkAlias(types.TypeIdent{ .ident_idx = type_ident }, anno_var, &.{}),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    return type_decl_idx;
}

/// Stub builtin types. Currently sets every type to be a nominal type
/// This should be replaced by real builtins eventually
fn addBuiltinTypeBool(self: *Self, ir: *ModuleEnv) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const type_ident = try ir.idents.insert(gpa, base.Ident.for_text("Bool"), Region.zero());

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());
    // const header_node_idx = nodeIdxFrom(header_idx);

    // Create a type annotation that refers to itself (built-in types are primitive)
    // const ext_var = @as(TypeVar, @enumFromInt(0)); // TODO: Implement type slot properly
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, .{ .flex_var = null }, Region.zero());
    const anno_var = castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_nominal_decl = .{
            .header = header_idx,
            .anno = anno_idx,
            .anno_var = anno_var,
            .where = null,
        },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkNominal(
            types.TypeIdent{ .ident_idx = type_ident },
            anno_var,
            &.{},
            try ir.idents.insert(gpa, base.Ident.for_text(ir.module_name), Region.zero()),
        ),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    // TODO: Implement type redirection for built-in types

    // Add True and False to unqualified_nominal_tags
    // TODO: in the future, we should have hardcoded constants for these.
    try self.unqualified_nominal_tags.put(gpa, "True", type_decl_idx);
    try self.unqualified_nominal_tags.put(gpa, "False", type_decl_idx);
}

const Self = @This();

/// The intermediate representation of a canonicalized Roc program.
// Import CIR types from compile module
const CompileCIR = @import("../compile/zig");

fn getDiagnosticRegion(diag: Diagnostic) base.Region {
    return switch (diag) {
        .not_implemented => |d| d.region,
        .exposed_but_not_implemented => |d| d.region,
        .redundant_exposed => |d| d.region,
        .invalid_num_literal => |d| d.region,
        .ident_already_in_scope => |d| d.region,
        .ident_not_in_scope => |d| d.region,
        .invalid_top_level_statement => |d| d.region,
        .f64_pattern_literal => |d| d.region,
        .invalid_single_quote => |d| d.region,
        .crash_expects_string => |d| d.region,
        .empty_tuple => |d| d.region,
        .malformed_type_annotation => |d| d.region,
        .module_not_imported => |d| d.region,
        else => base.Region.zero(),
    };
}

// Re-export types from compile module for convenience
pub const CIR = ModuleEnv; // CIR is now just an alias for ModuleEnv
pub const Diagnostic = ModuleEnv.Diagnostic;
pub const Import = ModuleEnv.Import;
pub const Expr = ModuleEnv.Expr;
pub const Pattern = ModuleEnv.Pattern;
pub const Statement = ModuleEnv.Statement;
pub const Def = ModuleEnv.Def;
pub const TypeAnno = ModuleEnv.TypeAnno;
pub const TypeHeader = ModuleEnv.TypeHeader;
pub const WhereClause = ModuleEnv.WhereClause;
pub const Annotation = ModuleEnv.Annotation;
pub const ExposedItem = ModuleEnv.ExposedItem;
pub const PatternRecordField = ModuleEnv.PatternRecordField;
pub const IntValue = ModuleEnv.IntValue;
pub const RocDec = ModuleEnv.RocDec;
pub const RecordField = ModuleEnv.RecordField;
pub const ExternalDecl = ModuleEnv.ExternalDecl;
pub const Report = ModuleEnv.Report;
pub const NodeStore = ModuleEnv.NodeStore;
pub const Node = ModuleEnv.Node;

// Helper functions
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@as(u32, @intCast(@intFromEnum(idx))));
}

pub fn varFrom(idx: anytype) types_mod.Var {
    return @enumFromInt(@intFromEnum(idx));
}

pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @enumFromInt(@intFromEnum(idx));
}

/// After parsing a Roc program, the [ParseIR](src/check/parse/AST.zig) is transformed into a [canonical
/// form](src/check/canonicalize/ir.zig) called CanIR.
///
/// Canonicalization performs analysis to catch user errors, and sets up the state necessary to solve the types in a
/// program. Among other things, canonicalization;
/// - Uniquely identifies names (think variable and function names). Along the way,
///     canonicalization builds a graph of all variables' references, and catches
///     unused definitions, undefined definitions, and shadowed definitions.
/// - Resolves type signatures, including aliases, into a form suitable for type
///     solving.
/// - Determines the order definitions are used in, if they are defined
///     out-of-order.
/// - Eliminates syntax sugar (for example, renaming `+` to the function call `add`).
///
/// The canonicalization occurs on a single module (file) in isolation. This allows for this work to be easily parallelized and also cached. So where the source code for a module has not changed, the CanIR can simply be loaded from disk and used immediately.
pub fn canonicalizeFile(
    self: *Self,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    const file = self.parse_ir.store.getFile();

    // canonicalize_header_packages();

    // First, process the header to create exposed_scope
    const header = self.parse_ir.store.getHeader(file.header);
    switch (header) {
        .module => |h| try self.createExposedScope(h.exposes),
        .package => |h| try self.createExposedScope(h.exposes),
        .platform => |h| try self.createExposedScope(h.exposes),
        .hosted => |h| try self.createExposedScope(h.exposes),
        .app => {
            // App headers have 'provides' instead of 'exposes'
            // TODO: Handle app provides differently
        },
        .malformed => {
            // Skip malformed headers
        },
    }

    // Track the start of scratch defs and statements
    const scratch_defs_start = self.env.store.scratchDefTop();
    const scratch_statements_start = self.env.store.scratch_statements.top();

    // First pass: Process all type declarations to introduce them into scope
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .type_decl => |type_decl| {
                // Canonicalize the type declaration header first
                const header_idx = try self.canonicalizeTypeHeader(type_decl.header, .cannot_introduce_vars);
                const region = self.parse_ir.tokenizedRegionToRegion(type_decl.region);

                // Extract the type name from the header to introduce it into scope early
                const type_header = self.env.store.getTypeHeader(header_idx);

                // Create a placeholder type declaration statement to introduce the type name into scope
                // This allows recursive type references to work during annotation canonicalization
                const placeholder_cir_type_decl = switch (type_decl.kind) {
                    .alias => Statement{
                        .s_alias_decl = .{
                            .header = header_idx,
                            .anno = @enumFromInt(0), // placeholder - will be replaced
                            .anno_var = @enumFromInt(0), // placeholder - will be replaced
                            .where = null,
                        },
                    },
                    .nominal => Statement{
                        .s_nominal_decl = .{
                            .header = header_idx,
                            .anno = @enumFromInt(0), // placeholder - will be replaced
                            .anno_var = @enumFromInt(0), // placeholder - will be replaced
                            .where = null,
                        },
                    },
                };

                const placeholder_type_decl_idx = try self.env.addStatementAndTypeVar(placeholder_cir_type_decl, Content{ .flex_var = null }, region);

                // Introduce the type name into scope early to support recursive references
                try self.scopeIntroduceTypeDecl(type_header.name, placeholder_type_decl_idx, region);

                // Process type parameters and annotation in a separate scope
                const anno_idx = blk: {
                    // Enter a new scope for type parameters
                    try self.scopeEnter(self.env.gpa, false);
                    defer self.scopeExit(self.env.gpa) catch {};

                    // Introduce type parameters from the header into the scope
                    try self.introduceTypeParametersFromHeader(header_idx);

                    // Now canonicalize the type annotation with type parameters and type name in scope
                    break :blk try self.canonicalizeTypeAnno(type_decl.anno, .cannot_introduce_vars, true);
                };

                // Creat type variables to the backing type (rhs)
                const anno_var = blk: {
                    // Enter a new scope for backing annotation type
                    try self.scopeEnter(self.env.gpa, false);
                    defer self.scopeExit(self.env.gpa) catch {};

                    break :blk try self.canonicalizeTypeAnnoToTypeVar(anno_idx);
                };

                // Check if the backing type is already an error type
                const backing_resolved = self.env.types.resolveVar(anno_var);
                const backing_is_error = backing_resolved.desc.content == .err;

                // Create types for each arg annotation
                const scratch_anno_start = self.scratch_vars.top();
                for (self.env.store.sliceTypeAnnos(type_header.args)) |arg_anno_idx| {
                    const arg_anno_var = try self.canonicalizeTypeAnnoToTypeVar(arg_anno_idx);
                    try self.scratch_vars.append(self.env.gpa, arg_anno_var);
                }
                const arg_anno_slice = self.scratch_vars.slice(scratch_anno_start, self.scratch_vars.top());

                // The identified of the type
                const type_ident = types.TypeIdent{ .ident_idx = type_header.name };

                // Canonicalize where clauses if present
                const where_clauses = if (type_decl.where) |where_coll| blk: {
                    const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                    const where_start = self.env.store.scratchWhereClauseTop();

                    for (where_slice) |where_idx| {
                        const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .cannot_introduce_vars);
                        try self.env.store.addScratchWhereClause(canonicalized_where);
                    }

                    break :blk try self.env.store.whereClauseSpanFrom(where_start);
                } else null;

                // Create the real CIR type declaration statement with the canonicalized annotation
                const real_cir_type_decl, const type_decl_content = blk: {
                    switch (type_decl.kind) {
                        .alias => {
                            const alias_content = if (backing_is_error)
                                types.Content{ .err = {} }
                            else
                                try self.env.types.mkAlias(type_ident, anno_var, arg_anno_slice);

                            break :blk .{
                                Statement{
                                    .s_alias_decl = .{
                                        .header = header_idx,
                                        .anno = anno_idx,
                                        .anno_var = anno_var,
                                        .where = where_clauses,
                                    },
                                },
                                alias_content,
                            };
                        },
                        .nominal => {
                            const nominal_content = if (backing_is_error)
                                types.Content{ .err = {} }
                            else
                                try self.env.types.mkNominal(
                                    type_ident,
                                    anno_var,
                                    arg_anno_slice,
                                    try self.env.idents.insert(self.env.gpa, base.Ident.for_text(self.env.module_name), Region.zero()),
                                );

                            break :blk .{
                                Statement{
                                    .s_nominal_decl = .{
                                        .header = header_idx,
                                        .anno = anno_idx,
                                        .anno_var = anno_var,
                                        .where = where_clauses,
                                    },
                                },
                                nominal_content,
                            };
                        },
                    }
                };

                // Create the real statement and add it to scratch statements
                const type_decl_stmt_idx = try self.env.addStatementAndTypeVar(real_cir_type_decl, type_decl_content, region);
                try self.env.store.addScratchStatement(type_decl_stmt_idx);

                // Shrink the scratch var buffer now that our work is done
                self.scratch_vars.clearFrom(scratch_anno_start);

                // Update the scope to point to the real statement instead of the placeholder
                try self.scopeUpdateTypeDecl(type_header.name, type_decl_stmt_idx);

                // Remove from exposed_type_texts since the type is now fully defined
                const type_text = self.env.idents.getText(type_header.name);
                _ = self.exposed_type_texts.remove(type_text);
            },
            else => {
                // Skip non-type-declaration statements in first pass
            },
        }
    }

    // Second pass: Process all other statements
    var last_type_anno: ?struct {
        name: base.Ident.Idx,
        anno_idx: TypeAnno.Idx,
        type_vars: base.DataSpan,
        where_clauses: ?WhereClause.Span,
    } = null;

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        switch (stmt) {
            .import => |import_stmt| {
                _ = try self.canonicalizeImportStatement(import_stmt);
                last_type_anno = null; // Clear on non-annotation statement
            },
            .decl => |decl| {
                // Check if this declaration matches the last type annotation
                var annotation_idx: ?Annotation.Idx = null;
                if (last_type_anno) |anno_info| {
                    if (self.parse_ir.store.getPattern(decl.pattern) == .ident) {
                        const pattern_ident = self.parse_ir.store.getPattern(decl.pattern).ident;
                        if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                            if (self.env.idents.identsHaveSameText(anno_info.name, decl_ident)) {
                                // This declaration matches the type annotation
                                const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
                                const type_var = @as(TypeVar, @enumFromInt(0)); // TODO: Implement type slot properly
                                annotation_idx = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, type_var, pattern_region);

                                // Clear the annotation since we've used it
                                last_type_anno = null;
                            }
                        }
                    }
                }

                const def_idx = try self.canonicalizeDeclWithAnnotation(decl, annotation_idx);
                try self.env.store.addScratchDef(def_idx);
                last_type_anno = null; // Clear after successful use

                // If this declaration successfully defined an exposed value, remove it from exposed_ident_texts
                // and add it to exposed_nodes
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                if (pattern == .ident) {
                    const token_region = self.parse_ir.tokens.resolve(@intCast(pattern.ident.ident_tok));
                    const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                    // If this identifier is exposed, add it to exposed_nodes
                    if (self.exposed_ident_texts.contains(ident_text)) {
                        // Store the def index as u16 in exposed_nodes
                        const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                        try self.env.exposed_nodes.put(self.env.gpa, ident_text, def_idx_u16);
                    }

                    _ = self.exposed_ident_texts.remove(ident_text);
                }
            },
            .@"var" => |var_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "var");
                const region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .expr => |expr_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "expression");
                const region = self.parse_ir.tokenizedRegionToRegion(expr_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .crash => |crash_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "crash");
                const region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .dbg => |dbg_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "dbg");
                const region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .expect => |e| {
                // Top-level expect statement
                const region = self.parse_ir.tokenizedRegionToRegion(e.region);

                // Canonicalize the expect expression
                const expect_expr = try self.canonicalizeExpr(e.body) orelse {
                    // If canonicalization fails, create a malformed expression
                    const malformed = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                    const expect_stmt = Statement{ .s_expect = .{
                        .body = malformed,
                    } };
                    const expect_stmt_idx = try self.env.addStatementAndTypeVar(expect_stmt, Content{ .flex_var = null }, region);
                    try self.env.store.addScratchStatement(expect_stmt_idx);
                    last_type_anno = null; // Clear on non-annotation statement
                    continue;
                };

                // Create expect statement
                const expect_stmt = Statement{ .s_expect = .{
                    .body = expect_expr,
                } };
                const expect_stmt_idx = try self.env.addStatementAndTypeVar(expect_stmt, Content{ .flex_var = null }, region);
                try self.env.store.addScratchStatement(expect_stmt_idx);

                last_type_anno = null; // Clear on non-annotation statement
            },
            .@"for" => |for_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "for");
                const region = self.parse_ir.tokenizedRegionToRegion(for_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .@"return" => |return_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.strings.insert(self.env.gpa, "return");
                const region = self.parse_ir.tokenizedRegionToRegion(return_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .type_decl => {
                // Already processed in first pass, skip
                last_type_anno = null; // Clear on non-annotation statement
            },
            .type_anno => |ta| {
                const gpa = self.env.gpa;
                const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

                // Top-level type annotation - store for connection to next declaration
                const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                    // Malformed identifier - skip this annotation
                    const feature = try self.env.strings.insert(gpa, "handle malformed identifier for a type annotation");
                    try self.env.pushDiagnostic(Diagnostic{
                        .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        },
                    });
                    continue;
                };

                // First, make the top of our scratch list
                const type_vars_top: u32 = @intCast(self.scratch_idents.top());

                // Extract type variables from the AST annotation
                try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

                // Enter a new scope for type variables
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {};

                // Introduce type variables into scope (if we have any)
                if (self.scratch_idents.top() > type_vars_top) {
                    for (self.scratch_idents.sliceFromStart(type_vars_top)) |type_var| {
                        // Create a dummy type annotation for the type variable
                        const dummy_anno = try self.env.addTypeAnnoAndTypeVar(.{
                            .ty_var = .{
                                .name = type_var,
                            },
                        }, Content{ .flex_var = null }, region); // TODO we may want to use the region for the type_var instead of the whole annotation
                        try self.scopeIntroduceTypeVar(type_var, dummy_anno);
                    }
                    // Shrink the scratch vars list to the original size
                    self.scratch_idents.clearFrom(type_vars_top);
                }

                // Now canonicalize the annotation with type variables in scope
                const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .cannot_introduce_vars, false);

                // Canonicalize where clauses if present
                const where_clauses = if (ta.where) |where_coll| blk: {
                    const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                    const where_start = self.env.store.scratchWhereClauseTop();

                    for (where_slice) |where_idx| {
                        const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .cannot_introduce_vars);
                        try self.env.store.addScratchWhereClause(canonicalized_where);
                    }

                    break :blk try self.env.store.whereClauseSpanFrom(where_start);
                } else null;

                // If we have where clauses, create a separate s_type_anno statement
                if (where_clauses != null) {
                    const type_anno_stmt = Statement{
                        .s_type_anno = .{
                            .name = name_ident,
                            .anno = type_anno_idx,
                            .where = where_clauses,
                        },
                    };
                    const type_anno_stmt_idx = try self.env.addStatementAndTypeVar(type_anno_stmt, Content{ .flex_var = null }, region);
                    try self.env.store.addScratchStatement(type_anno_stmt_idx);
                }

                // Store this annotation for the next declaration
                last_type_anno = .{
                    .name = name_ident,
                    .anno_idx = type_anno_idx,
                    .type_vars = base.DataSpan.empty(), // TODO: store type vars if needed
                    .where_clauses = where_clauses,
                };
            },
            .malformed => |malformed| {
                // We won't touch this since it's already a parse error.
                _ = malformed;
                last_type_anno = null; // Clear on non-annotation statement
            },
        }
    }

    // Check for exposed but not implemented items
    try self.checkExposedButNotImplemented();

    // Create the span of all top-level defs and statements
    self.env.all_defs = try self.env.store.defSpanFrom(scratch_defs_start);
    self.env.all_statements = try self.env.store.statementSpanFrom(scratch_statements_start);

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    // Freeze the interners after canonicalization is complete
    self.env.freezeInterners();
}

fn createExposedScope(
    self: *Self,
    exposes: AST.Collection.Idx,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    // Reset exposed_scope (already initialized in init)
    self.exposed_scope.deinit(gpa);
    self.exposed_scope = Scope.init(false);

    const collection = self.parse_ir.store.getCollection(exposes);
    const exposed_items = self.parse_ir.store.exposedItemSlice(.{ .span = collection.span });

    // Check if we have too many exports (>= maxInt(u16) to reserve 0 as potential sentinel)
    if (exposed_items.len >= std.math.maxInt(u16)) {
        const region = self.parse_ir.tokenizedRegionToRegion(collection.region);
        try self.env.pushDiagnostic(Diagnostic{ .too_many_exports = .{
            .count = @intCast(exposed_items.len),
            .region = region,
        } });
        return;
    }

    for (exposed_items) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {
                // Get the text of the identifier token to use as key
                const token_region = self.parse_ir.tokens.resolve(@intCast(ident.ident));
                const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Add to exposed_by_str for permanent storage (unconditionally)
                try self.env.exposed_by_str.put(gpa, ident_text, {});

                // Also build exposed_scope with proper identifiers
                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                    // Use a dummy pattern index - we just need to track that it's exposed
                    const dummy_idx = @as(Pattern.Idx, @enumFromInt(0));
                    try self.exposed_scope.put(gpa, .ident, ident_idx, dummy_idx);
                }

                // Store by text in a temporary hash map, since indices may change
                const region = self.parse_ir.tokenizedRegionToRegion(ident.region);

                // Check if this identifier was already exposed
                if (self.exposed_ident_texts.get(ident_text)) |original_region| {
                    // Report redundant exposed entry error
                    if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                        const diag = Diagnostic{ .redundant_exposed = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } };
                        try self.env.pushDiagnostic(diag);
                    }
                } else {
                    try self.exposed_ident_texts.put(gpa, ident_text, region);
                }
            },
            .upper_ident => |type_name| {
                // Get the text of the identifier token to use as key
                const token_region = self.parse_ir.tokens.resolve(@intCast(type_name.ident));
                const type_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Add to exposed_by_str for permanent storage (unconditionally)
                try self.env.exposed_by_str.put(gpa, type_text, {});

                // Also build exposed_scope with proper identifiers
                if (self.parse_ir.tokens.resolveIdentifier(type_name.ident)) |ident_idx| {
                    // Use a dummy statement index - we just need to track that it's exposed
                    const dummy_idx = @as(Statement.Idx, @enumFromInt(0));
                    try self.exposed_scope.put(gpa, .type_decl, ident_idx, dummy_idx);
                }

                // Store by text in a temporary hash map, since indices may change
                const region = self.parse_ir.tokenizedRegionToRegion(type_name.region);

                // Check if this type was already exposed
                if (self.exposed_type_texts.get(type_text)) |original_region| {
                    // Report redundant exposed entry error
                    if (self.parse_ir.tokens.resolveIdentifier(type_name.ident)) |ident_idx| {
                        const diag = Diagnostic{ .redundant_exposed = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } };
                        try self.env.pushDiagnostic(diag);
                    }
                } else {
                    try self.exposed_type_texts.put(gpa, type_text, region);
                }
            },
            .upper_ident_star => |type_with_constructors| {
                // Get the text of the identifier token to use as key
                const token_region = self.parse_ir.tokens.resolve(@intCast(type_with_constructors.ident));
                const type_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Add to exposed_by_str for permanent storage (unconditionally)
                try self.env.exposed_by_str.put(gpa, type_text, {});

                // Also build exposed_scope with proper identifiers
                if (self.parse_ir.tokens.resolveIdentifier(type_with_constructors.ident)) |ident_idx| {
                    // Use a dummy statement index - we just need to track that it's exposed
                    const dummy_idx = @as(Statement.Idx, @enumFromInt(0));
                    try self.exposed_scope.put(gpa, .type_decl, ident_idx, dummy_idx);
                }

                // Store by text in a temporary hash map, since indices may change
                const region = self.parse_ir.tokenizedRegionToRegion(type_with_constructors.region);

                // Check if this type was already exposed
                if (self.exposed_type_texts.get(type_text)) |original_region| {
                    // Report redundant exposed entry error
                    if (self.parse_ir.tokens.resolveIdentifier(type_with_constructors.ident)) |ident_idx| {
                        const diag = Diagnostic{ .redundant_exposed = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } };
                        try self.env.pushDiagnostic(diag);
                    }
                } else {
                    try self.exposed_type_texts.put(gpa, type_text, region);
                }
            },
            .malformed => |malformed| {
                // Malformed exposed items are already captured as diagnostics during parsing
                _ = malformed;
            },
        }
    }
}

fn checkExposedButNotImplemented(self: *Self) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;

    // Check for remaining exposed identifiers
    var ident_iter = self.exposed_ident_texts.iterator();
    while (ident_iter.next()) |entry| {
        const ident_text = entry.key_ptr.*;
        const region = entry.value_ptr.*;
        // Create an identifier for error reporting
        const ident_idx = try self.env.idents.insert(gpa, base.Ident.for_text(ident_text), region);

        // Report error: exposed identifier but not implemented
        const diag = Diagnostic{ .exposed_but_not_implemented = .{
            .ident = ident_idx,
            .region = region,
        } };
        try self.env.pushDiagnostic(diag);
    }

    // Check for remaining exposed types
    var iter = self.exposed_type_texts.iterator();
    while (iter.next()) |entry| {
        const type_text = entry.key_ptr.*;
        const region = entry.value_ptr.*;
        // Create an identifier for error reporting
        const ident_idx = try self.env.idents.insert(gpa, base.Ident.for_text(type_text), region);

        // Report error: exposed type but not implemented
        try self.env.pushDiagnostic(Diagnostic{ .exposed_but_not_implemented = .{
            .ident = ident_idx,
            .region = region,
        } });
    }
}

fn bringImportIntoScope(
    self: *Self,
    import: *const AST.Statement,
) void {
    // const gpa = self.env.gpa;
    // const import_name: []u8 = &.{}; // import.module_name_tok;
    // const shorthand: []u8 = &.{}; // import.qualifier_tok;
    // const region = Region{
    //     .start = Region.Position.zero(),
    //     .end = Region.Position.zero(),
    // };

    // const res = self.env.imports.getOrInsert(gpa, import_name, shorthand);

    // if (res.was_present) {
    //     _ = self.env.problems.append(gpa, Problem.Canonicalize.make(.{ .DuplicateImport = .{
    //         .duplicate_import_region = region,
    //     } }));
    // }

    const exposesSlice = self.parse_ir.store.exposedItemSlice(import.exposes);
    for (exposesSlice) |exposed_idx| {
        const exposed = self.parse_ir.store.getExposedItem(exposed_idx);
        switch (exposed) {
            .lower_ident => |ident| {

                // TODO handle `as` here using an Alias

                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                    _ = ident_idx;

                    // TODO Introduce our import

                    // _ = self.scope.levels.introduce(gpa, &self.env.idents, .ident, .{ .scope_name = ident_idx, .ident = ident_idx });
                }
            },
            .upper_ident => |imported_type| {
                _ = imported_type;
                // const alias = Alias{
                //     .name = imported_type.name,
                //     .region = ir.tag_names.getRegion(imported_type.name),
                //     .is_builtin = false,
                //     .kind = .ImportedUnknown,
                // };
                // const alias_idx = ir.aliases.append(alias);
                //
                // _ = scope.levels.introduce(.alias, .{
                //     .scope_name = imported_type.name,
                //     .alias = alias_idx,
                // });
            },
            .upper_ident_star => |ident| {
                _ = ident;
            },
        }
    }
}

fn bringIngestedFileIntoScope(
    self: *Self,
    import: *const parse.AST.Stmt.Import,
) void {
    const res = self.env.modules.getOrInsert(
        import.name,
        import.package_shorthand,
    );

    if (res.was_present) {
        // _ = self.env.problems.append(Problem.Canonicalize.make(.DuplicateImport{
        //     .duplicate_import_region = import.name_region,
        // }));
    }

    // scope.introduce(self: *Scope, comptime item_kind: Level.ItemKind, ident: Ident.Idx)

    for (import.exposing.items.items) |exposed| {
        const exposed_ident = switch (exposed) {
            .Value => |ident| ident,
            .Type => |ident| ident,
            .CustomTagUnion => |custom| custom.name,
        };
        self.env.addExposedIdentForModule(exposed_ident, res.module_idx);
        // TODO: Implement scope introduction for exposed identifiers
    }
}

/// Canonicalize an import statement, handling both top-level file imports and statement imports
fn canonicalizeImportStatement(
    self: *Self,
    import_stmt: @TypeOf(@as(AST.Statement, undefined).import),
) std.mem.Allocator.Error!?Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // 1. Reconstruct the full module name (e.g., "json.Json")
    const module_name = blk: {
        if (self.parse_ir.tokens.resolveIdentifier(import_stmt.module_name_tok) == null) {
            const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
            const feature = try self.env.strings.insert(self.env.gpa, "resolve import module name token");
            try self.env.pushDiagnostic(Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return null;
        }

        if (import_stmt.qualifier_tok) |qualifier_tok| {
            if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok) == null) {
                const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
                const feature = try self.env.strings.insert(self.env.gpa, "resolve import qualifier token");
                try self.env.pushDiagnostic(Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return null;
            }

            // Slice from original source to get "qualifier.ModuleName"
            const qualifier_region = self.parse_ir.tokens.resolve(qualifier_tok);
            const module_region = self.parse_ir.tokens.resolve(import_stmt.module_name_tok);
            const full_name = self.parse_ir.env.source[qualifier_region.start.offset..module_region.end.offset];

            // Validate the full_name using Ident.from_bytes
            if (base.Ident.from_bytes(full_name)) |valid_ident| {
                break :blk try self.env.idents.insert(self.env.gpa, valid_ident, Region.zero());
            } else |err| {
                // Invalid identifier - create diagnostic and use placeholder
                const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
                const error_msg = switch (err) {
                    base.Ident.Error.EmptyText => "malformed import module name is empty",
                    base.Ident.Error.ContainsNullByte => "malformed import module name contains null bytes",
                    base.Ident.Error.ContainsControlCharacters => "malformed import module name contains invalid control characters",
                };
                const feature = try self.env.strings.insert(self.env.gpa, error_msg);
                try self.env.pushDiagnostic(Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });

                // Use a placeholder identifier instead
                const placeholder_text = "MALFORMED_IMPORT";
                break :blk try self.env.idents.insert(self.env.gpa, base.Ident.for_text(placeholder_text), Region.zero());
            }
        } else {
            // No qualifier, just use the module name directly
            break :blk self.parse_ir.tokens.resolveIdentifier(import_stmt.module_name_tok).?;
        }
    };

    // 2. Determine the alias (either explicit or default to last part)
    const alias = try self.resolveModuleAlias(import_stmt.alias_tok, module_name) orelse return null;

    // 3. Get or create Import.Idx for this module
    const module_name_text = self.env.idents.getText(module_name);
    const module_import_idx = try self.env.imports.getOrPut(self.env.gpa, module_name_text);

    // 4. Add to scope: alias -> module_name mapping
    try self.scopeIntroduceModuleAlias(alias, module_name);

    // Process type imports from this module
    try self.processTypeImports(module_name, alias);

    // 5. Convert exposed items and introduce them into scope
    const cir_exposes = try self.convertASTExposesToCIR(import_stmt.exposes);
    const import_region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
    try self.introduceExposedItemsIntoScope(cir_exposes, module_name, import_region);

    // 6. Store the mapping from module name to Import.Idx
    try self.import_indices.put(self.env.gpa, module_name_text, module_import_idx);

    // 7. Create CIR import statement
    const cir_import = Statement{
        .s_import = .{
            .module_name_tok = module_name,
            .qualifier_tok = if (import_stmt.qualifier_tok) |q_tok| self.parse_ir.tokens.resolveIdentifier(q_tok) else null,
            .alias_tok = if (import_stmt.alias_tok) |a_tok| self.parse_ir.tokens.resolveIdentifier(a_tok) else null,
            .exposes = cir_exposes,
        },
    };

    const import_idx = try self.env.addStatementAndTypeVar(cir_import, Content{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(import_stmt.region));
    try self.env.store.addScratchStatement(import_idx);

    // 8. Add the module to the current scope so it can be used in qualified lookups
    const current_scope = self.currentScope();
    _ = try current_scope.introduceImportedModule(self.env.gpa, module_name_text, module_import_idx);

    return import_idx;
}

/// Resolve the module alias name from either explicit alias or module name
fn resolveModuleAlias(
    self: *Self,
    alias_tok: ?Token.Idx,
    module_name: Ident.Idx,
) std.mem.Allocator.Error!?Ident.Idx {
    if (alias_tok) |alias_token| {
        return self.parse_ir.tokens.resolveIdentifier(alias_token);
    } else {
        // Extract last part from module name - e.g., "Json" from "json.Json"
        return try self.extractModuleName(module_name);
    }
}

/// Create a qualified name by combining module and field names (e.g., "json.Json.utf8")
fn createQualifiedName(
    self: *Self,
    module_name: Ident.Idx,
    field_name: Ident.Idx,
) Ident.Idx {
    const module_text = self.env.idents.getText(module_name);
    const field_text = self.env.idents.getText(field_name);

    // Allocate space for "module.field" - this case still needs allocation since we're combining
    // module name from import with field name from usage site
    const qualified_text = try std.fmt.allocPrint(self.env.gpa, "{s}.{s}", .{ module_text, field_text });
    defer self.env.gpa.free(qualified_text);

    return try self.env.idents.insert(self.env.gpa, base.Ident.for_text(qualified_text), Region.zero());
}

/// Create an external declaration for a qualified name
fn createExternalDeclaration(
    self: *Self,
    qualified_name: Ident.Idx,
    module_name: Ident.Idx,
    local_name: Ident.Idx,
    kind: @TypeOf(@as(ExternalDecl, undefined).kind),
    region: Region,
) std.mem.Allocator.Error!ExternalDecl.Idx {
    const external_type_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
    const external_decl = ExternalDecl{
        .qualified_name = qualified_name,
        .module_name = module_name,
        .local_name = local_name,
        .type_var = external_type_var,
        .kind = kind,
        .region = region,
    };

    return self.env.pushExternalDecl(external_decl);
}

/// Convert AST exposed items to CIR exposed items
fn convertASTExposesToCIR(
    self: *Self,
    ast_exposes: AST.ExposedItem.Span,
) std.mem.Allocator.Error!ExposedItem.Span {
    const scratch_start = self.env.store.scratchExposedItemTop();

    const ast_exposed_slice = self.parse_ir.store.exposedItemSlice(ast_exposes);
    for (ast_exposed_slice) |ast_exposed_idx| {
        const ast_exposed = self.parse_ir.store.getExposedItem(ast_exposed_idx);

        // Convert AST exposed item to CIR exposed item
        const cir_exposed = convert_item: {
            // Extract identifier token and alias token
            const ident_token, const alias_token, const is_wildcard = switch (ast_exposed) {
                .lower_ident => |ident| .{ ident.ident, ident.as, false },
                .upper_ident => |ident| .{ ident.ident, ident.as, false },
                .upper_ident_star => |star_ident| .{ star_ident.ident, null, true },
                .malformed => |_| continue, // Skip malformed exposed items
            };

            // Resolve the main identifier name
            const name = resolve_ident: {
                if (self.parse_ir.tokens.resolveIdentifier(ident_token)) |resolved| {
                    break :resolve_ident resolved;
                } else {
                    break :resolve_ident try self.env.idents.insert(self.env.gpa, base.Ident.for_text("unknown"), base.Region.zero());
                }
            };

            // Resolve the alias if present
            const alias = resolve_alias: {
                if (alias_token) |as_token| {
                    if (self.parse_ir.tokens.resolveIdentifier(as_token)) |resolved| {
                        break :resolve_alias resolved;
                    } else {
                        break :resolve_alias try self.env.idents.insert(self.env.gpa, base.Ident.for_text("unknown"), base.Region.zero());
                    }
                } else {
                    break :resolve_alias null;
                }
            };

            break :convert_item ExposedItem{
                .name = name,
                .alias = alias,
                .is_wildcard = is_wildcard,
            };
        };

        const tokenized_region = switch (ast_exposed) {
            inline else => |payload| payload.region,
        };
        const region = self.parse_ir.tokenizedRegionToRegion(tokenized_region);
        const cir_exposed_idx = try self.env.addExposedItemAndTypeVar(cir_exposed, .{ .flex_var = null }, region);
        try self.env.store.addScratchExposedItem(cir_exposed_idx);
    }

    return try self.env.store.exposedItemSpanFrom(scratch_start);
}

/// Introduce converted exposed items into scope for identifier resolution
fn introduceExposedItemsIntoScope(
    self: *Self,
    exposed_items_span: ExposedItem.Span,
    module_name: Ident.Idx,
    import_region: Region,
) std.mem.Allocator.Error!void {
    const exposed_items_slice = self.env.store.sliceExposedItems(exposed_items_span);

    // If we have module_envs, validate the imports
    if (self.module_envs) |envs_map| {
        const module_name_text = self.env.idents.getText(module_name);

        // Check if the module exists
        if (!envs_map.contains(module_name_text)) {
            // Module not found - create diagnostic
            try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
                .module_name = module_name,
                .region = import_region,
            } });
            return;
        }

        // Get the module's exposed_by_str map
        const module_env = envs_map.get(module_name_text).?;

        // Validate each exposed item
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const item_name_text = self.env.idents.getText(exposed_item.name);

            // Check if the item is exposed by the module
            if (!module_env.exposed_by_str.contains(item_name_text)) {
                // Determine if it's a type or value based on capitalization
                const first_char = item_name_text[0];

                if (first_char >= 'A' and first_char <= 'Z') {
                    // Type not exposed
                    try self.env.pushDiagnostic(Diagnostic{ .type_not_exposed = .{
                        .module_name = module_name,
                        .type_name = exposed_item.name,
                        .region = import_region,
                    } });
                } else {
                    // Value not exposed
                    try self.env.pushDiagnostic(Diagnostic{ .value_not_exposed = .{
                        .module_name = module_name,
                        .value_name = exposed_item.name,
                        .region = import_region,
                    } });
                }
                continue; // Skip introducing this item to scope
            }

            // Item is valid, introduce it to scope
            const item_name = exposed_item.alias orelse exposed_item.name;
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
            };
            try self.scopeIntroduceExposedItem(item_name, item_info);
        }
    } else {
        // No module_envs provided, introduce all items without validation
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const item_name = exposed_item.alias orelse exposed_item.name;
            const item_info = Scope.ExposedItemInfo{
                .module_name = module_name,
                .original_name = exposed_item.name,
            };
            try self.scopeIntroduceExposedItem(item_name, item_info);
        }
    }
}

fn canonicalizeDeclWithAnnotation(
    self: *Self,
    decl: AST.Statement.Decl,
    annotation: ?Annotation.Idx,
) std.mem.Allocator.Error!Def.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
    const expr_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getExpr(decl.body).to_tokenized_region());

    const pattern_idx = blk: {
        if (try self.canonicalizePattern(decl.pattern)) |idx| {
            break :blk idx;
        } else {
            const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                .region = pattern_region,
            } });
            break :blk malformed_idx;
        }
    };

    const expr_idx = blk: {
        if (try self.canonicalizeExpr(decl.body)) |idx| {
            break :blk idx;
        } else {
            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = expr_region,
            } });
            break :blk malformed_idx;
        }
    };

    // Create the def entry and set def type variable to a flex var
    //
    // We always use a flex variable for the definition, regardless of whether there's
    // an annotation. This is because:
    // 1. If there's no annotation, we need a flex var for normal type inference
    // 2. If there IS an annotation, we still use a flex var to avoid copying the
    //    annotation's type content. This is necessary because if the annotation contains
    //    an alias (e.g., `empty : ConsList(a)`), that alias expects its type arguments
    //    to live at specific memory offsets relative to the alias's own type variable.
    //    Copying the alias content to a different type variable would break this assumption.
    // 3. During type checking, the definition's flex var will be unified with the
    //    annotation's type (if present) or with the inferred type from the expression
    // 4. Type errors will be caught during unification if the implementation doesn't
    //    match the annotation
    const region = self.parse_ir.tokenizedRegionToRegion(decl.region);
    const def_idx = self.env.addDefAndTypeVar(.{
        .pattern = pattern_idx,
        .expr = expr_idx,
        .annotation = annotation,
        .kind = .let,
    }, Content{ .flex_var = null }, region);

    return def_idx;
}

fn parseSingleQuoteCodepoint(
    inner_text: []const u8,
) ?u21 {
    const escaped = inner_text[0] == '\\';

    if (escaped) {
        const c = inner_text[1];
        switch (c) {
            'u' => {
                const hex_code = inner_text[3 .. inner_text.len - 1];
                const codepoint = std.fmt.parseInt(u21, hex_code, 16) catch {
                    return null;
                };

                if (!std.unicode.utf8ValidCodepoint(codepoint)) {
                    return null;
                }

                return codepoint;
            },
            '\\', '"', '\'', '$' => {
                return c;
            },
            'n' => {
                return '\n';
            },
            'r' => {
                return '\r';
            },
            't' => {
                return '\t';
            },
            else => {
                return null;
            },
        }
    } else {
        const view = std.unicode.Utf8View.init(inner_text) catch |err| switch (err) {
            error.InvalidUtf8 => {
                return null;
            },
        };

        var iterator = view.iterator();

        if (iterator.nextCodepoint()) |codepoint| {
            std.debug.assert(iterator.nextCodepoint() == null);
            return codepoint;
        } else {
            // only single valid utf8 codepoint can be here after tokenization
            unreachable;
        }
    }
}

fn canonicalizeSingleQuote(
    self: *Self,
    token_region: AST.TokenizedRegion,
    token: Token.Idx,
    comptime Idx: type,
) std.mem.Allocator.Error!?Idx {
    const region = self.parse_ir.tokenizedRegionToRegion(token_region);

    // Resolve to a string slice from the source
    const token_text = self.parse_ir.resolve(token);

    if (parseSingleQuoteCodepoint(token_text[1 .. token_text.len - 1])) |codepoint| {
        const type_content = Content{ .structure = .{ .num = .{ .num_unbound = types.Num.IntRequirements{
            .sign_needed = false,
            .bits_needed = @intCast(@sizeOf(u21)),
        } } } };
        const value_content = IntValue{
            .bytes = @bitCast(@as(u128, @intCast(codepoint))),
            .kind = .u128,
        };
        if (Idx == Expr.Idx) {
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_int = .{
                    .value = value_content,
                },
            }, type_content, region);
            return expr_idx;
        } else if (Idx == Pattern.Idx) {
            const pat_idx = try self.env.addPatternAndTypeVar(Pattern{
                .int_literal = .{
                    .value = value_content,
                },
            }, type_content, region);
            return pat_idx;
        } else {
            @compileError("Unsupported Idx type");
        }
    }

    return try self.env.pushMalformed(Idx, Diagnostic{ .invalid_single_quote = .{
        .region = region,
    } });
}

fn canonicalizeRecordField(
    self: *Self,
    ast_field_idx: AST.RecordField.Idx,
) std.mem.Allocator.Error!?RecordField.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const field = self.parse_ir.store.getRecordField(ast_field_idx);

    // Canonicalize the field name
    const name = self.parse_ir.tokens.resolveIdentifier(field.name) orelse {
        return null;
    };

    // Canonicalize the field value
    const value = if (field.value) |v|
        try self.canonicalizeExpr(v) orelse return null
    else blk: {
        // Shorthand syntax: create implicit identifier expression
        // For { name, age }, this creates an implicit identifier lookup for "name" etc.
        const ident_expr = AST.Expr{
            .ident = .{
                .token = field.name,
                .qualifiers = .{ .span = .{ .start = 0, .len = 0 } },
                .region = field.region,
            },
        };
        const ident_expr_idx = try self.parse_ir.store.addExpr(ident_expr);
        break :blk try self.canonicalizeExpr(ident_expr_idx) orelse return null;
    };

    // Create the CIR record field
    const cir_field = RecordField{
        .name = name,
        .value = value,
    };

    return try self.env.addRecordFieldAndTypeVar(cir_field, Content{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(field.region));
}

/// Canonicalize an expression.
pub fn canonicalizeExpr(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!?Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    const expr = self.parse_ir.store.getExpr(ast_expr_idx);
    switch (expr) {
        .apply => |e| {
            // Check if the function being applied is a tag
            const ast_fn = self.parse_ir.store.getExpr(e.@"fn");
            if (ast_fn == .tag) {
                // This is a tag application, not a function call
                const tag_expr = ast_fn.tag;
                return self.canonicalizeTagExpr(tag_expr, e.args);
            }

            // Not a tag application, proceed with normal function call
            // Mark the start of scratch expressions
            const scratch_top = self.env.store.scratchExprTop();

            // Canonicalize the function being called and add as first element
            const fn_expr = try self.canonicalizeExpr(e.@"fn") orelse {
                self.env.store.clearScratchExprsFrom(scratch_top);
                return null;
            };
            try self.env.store.addScratchExpr(fn_expr);

            // Canonicalize and add all arguments
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                if (try self.canonicalizeExpr(arg)) |canonicalized_arg_expr_idx| {
                    try self.env.store.addScratchExpr(canonicalized_arg_expr_idx);
                }
            }

            // Create span from scratch expressions
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_call = .{
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, Content{ .flex_var = null }, region);

            return expr_idx;
        },
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.token)) |ident| {
                // Check if this is a module-qualified identifier
                const qualifier_tokens = self.parse_ir.store.tokenSlice(e.qualifiers);
                if (qualifier_tokens.len > 0) {
                    const qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
                    if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok)) |module_alias| {
                        // Check if this is a module alias
                        if (self.scopeLookupModule(module_alias)) |module_name| {
                            // This is a module-qualified lookup
                            const module_text = self.env.idents.getText(module_name);

                            // Check if this module is imported in the current scope
                            const import_idx = self.scopeLookupImportedModule(module_text) orelse {
                                // Module not imported in current scope
                                return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                                    .module_name = module_name,
                                    .region = region,
                                } });
                            };

                            // Look up the target node index in the module's exposed_nodes
                            const field_text = self.env.idents.getText(ident);
                            const target_node_idx = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(module_text)) |module_env| {
                                    break :blk module_env.exposed_nodes.get(field_text) orelse 0;
                                } else {
                                    break :blk 0;
                                }
                            } else 0;

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .region = region,
                            } }, Content{ .flex_var = null }, region);
                            return expr_idx;
                        }
                    }
                }

                // Not a module-qualified lookup, or qualifier not found, proceed with normal lookup
                switch (self.scopeLookup(&self.env.idents, .ident, ident)) {
                    .found => |pattern_idx| {
                        // Mark this pattern as used for unused variable checking
                        try self.used_patterns.put(self.env.gpa, pattern_idx, {});

                        // Check if this is a used underscore variable
                        try self.checkUsedUnderscoreVariable(ident, region);

                        // We found the ident in scope, lookup to reference the pattern
                        const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_lookup_local = .{
                            .pattern_idx = pattern_idx,
                        } }, Content{ .flex_var = null }, region);
                        return expr_idx;
                    },
                    .not_found => {
                        // Check if this identifier is an exposed item from an import
                        if (self.scopeLookupExposedItem(ident)) |exposed_info| {
                            // Get the Import.Idx for the module this item comes from
                            const module_text = self.env.idents.getText(exposed_info.module_name);
                            const import_idx = self.scopeLookupImportedModule(module_text) orelse {
                                // This shouldn't happen if imports are properly tracked, but handle it gracefully
                                return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                                    .module_name = exposed_info.module_name,
                                    .region = region,
                                } });
                            };

                            // Look up the target node index in the module's exposed_nodes
                            const field_text = self.env.idents.getText(exposed_info.original_name);
                            const target_node_idx = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(module_text)) |module_env| {
                                    break :blk module_env.exposed_nodes.get(field_text) orelse 0;
                                } else {
                                    break :blk 0;
                                }
                            } else 0;

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .region = region,
                            } }, Content{ .flex_var = null }, region);
                            return expr_idx;
                        }

                        // We did not find the ident in scope or as an exposed item
                        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                            .ident = ident,
                            .region = region,
                        } });
                    },
                }
            } else {
                const feature = try self.env.strings.insert(self.env.gpa, "report an error when unable to resolve identifier");
                return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
            }
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            // Parse the integer value
            const is_negated = token_text[0] == '-'; // Drop the negation for now, so all valid literals fit in u128
            const after_minus_sign = @as(usize, @intFromBool(is_negated));

            // The index the first *actual* digit (after minus sign, "0x" prefix, etc.) in the token
            var first_digit: usize = undefined;

            const DEFAULT_BASE: u8 = 10; // default to base-10, naturally
            var int_base: u8 = undefined;

            // If this begins with "0x" or "0b" or "Oo" then it's not base-10.
            // We don't bother storing this info anywhere else besides token text,
            // because we already have to look at the whole token to parse the digits
            // into a number, so it will be in cache. It's also trivial to parse.
            if (token_text[after_minus_sign] == '0' and token_text.len > after_minus_sign + 2) {
                switch (token_text[after_minus_sign + 1]) {
                    'x', 'X' => {
                        int_base = 16;
                        first_digit = after_minus_sign + 2;
                    },
                    'o', 'O' => {
                        int_base = 8;
                        first_digit = after_minus_sign + 2;
                    },
                    'b', 'B' => {
                        int_base = 2;
                        first_digit = after_minus_sign + 2;
                    },
                    else => {
                        int_base = DEFAULT_BASE;
                        first_digit = after_minus_sign;
                    },
                }
            } else {
                int_base = DEFAULT_BASE;
                first_digit = after_minus_sign;
            }

            const u128_val: u128 = std.fmt.parseInt(u128, token_text[first_digit..], int_base) catch {
                // Any number literal that is too large for u128 is invalid, regardless of whether it had a minus sign!
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return expr_idx;
            };

            // If this had a minus sign, but negating it would result in a negative number
            // that would be too low to fit in i128, then this int literal is also invalid.
            if (is_negated and u128_val > min_i128_negated) {
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return expr_idx;
            }

            // Now we've confirmed that our int literal is one of these:
            // * A signed integer that fits in i128
            // * An unsigned integer that fits in u128
            //
            // We'll happily bitcast a u128 to i128 for storage (and bitcast it back later
            // using its type information), but for negative numbers, we do need to actually
            // negate them (branchlessly) if we skipped its minus sign earlier.
            //
            // This operation should never overflow i128, because we already would have errored out
            // if the u128 portion was bigger than the lowest i128 without a minus sign.
            // Special case: exactly i128 min already has the correct bit pattern when bitcast from u128,
            // so if we try to negate it we'll get an overflow. We specifically *don't* negate that one.
            const sign: i128 = (@as(i128, @intFromBool(!is_negated or u128_val == min_i128_negated)) << 1) - 1;
            const i128_val: i128 = sign * @as(i128, @bitCast(u128_val));

            // Calculate requirements based on the value
            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            const is_negative_u1 = @as(u1, @intFromBool(is_negated));
            const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
            const is_minimum_signed = is_negative_u1 & is_power_of_2;
            const adjusted_val = u128_val - is_minimum_signed;

            const requirements = types.Num.Int.Requirements{
                .sign_needed = is_negated,
                .bits_needed = types.Num.Int.BitsNeeded.fromValue(adjusted_val),
            };

            const int_requirements = types.Num.IntRequirements{
                .sign_needed = requirements.sign_needed,
                .bits_needed = @intCast(@intFromEnum(requirements.bits_needed)),
            };

            // For non-decimal integers (hex, binary, octal), use int_poly directly
            // For decimal integers, use num_poly so they can be either Int or Frac
            const is_non_decimal = int_base != DEFAULT_BASE;

            // Insert concrete type variable
            const type_content = if (is_non_decimal)
                Content{ .structure = .{ .num = .{ .int_unbound = int_requirements } } }
            else
                Content{ .structure = .{ .num = .{ .num_unbound = int_requirements } } };

            // Add the expression and type variable atomically
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_int = .{
                    .value = .{ .bytes = @bitCast(i128_val), .kind = .i128 },
                },
            }, type_content, region);

            return expr_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumLiteral => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return expr_idx;
                },
            };

            // Parse the literal first to get requirements
            const requirements = switch (parsed) {
                .small => |small_info| small_info.requirements,
                .dec => |dec_info| dec_info.requirements,
                .f64 => |f64_info| f64_info.requirements,
            };

            const frac_requirements = types.Num.FracRequirements{
                .fits_in_f32 = requirements.fits_in_f32,
                .fits_in_dec = requirements.fits_in_dec,
            };

            const cir_expr = switch (parsed) {
                .small => |small_info| Expr{
                    .e_dec_small = .{
                        .numerator = small_info.numerator,
                        .denominator_power_of_ten = small_info.denominator_power_of_ten,
                    },
                },
                .dec => |dec_info| Expr{
                    .e_frac_dec = .{
                        .value = RocDec{ .num = dec_info.value.num },
                    },
                },
                .f64 => |f64_info| Expr{
                    .e_frac_f64 = .{
                        .value = f64_info.value,
                    },
                },
            };

            const expr_idx = try self.env.addExprAndTypeVar(cir_expr, Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } }, region);

            return expr_idx;
        },
        .single_quote => |e| {
            return self.canonicalizeSingleQuote(e.region, e.token, Expr.Idx);
        },
        .string => |e| {
            // Get all the string parts
            const parts = self.parse_ir.store.exprSlice(e.parts);

            // Extract segments from the string, inserting them into the string interner
            // For non-string interpolation segments, canonicalize them
            //
            // Returns a Expr.Span containing the canonicalized string segments
            // a string may consist of multiple string literal or expression segments
            const str_segments_span = try self.extractStringSegments(parts);

            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_str = .{
                .span = str_segments_span,
            } }, Content{ .structure = .str }, region);

            return expr_idx;
        },
        .list => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Empty lists get the .list_unbound type
            const items_slice = self.parse_ir.store.exprSlice(e.items);
            if (items_slice.len == 0) {
                // Empty list - use e_empty_list
                const expr_idx = try self.env.addExprAndTypeVar(Expr{
                    .e_empty_list = .{},
                }, Content{ .structure = .list_unbound }, region);

                return expr_idx;
            }

            // Mark the start of scratch expressions for the list
            const scratch_top = self.env.store.scratchExprTop();

            // Iterate over the list item, canonicalizing each one
            // Then append the result to the scratch list
            for (items_slice) |item| {
                if (try self.canonicalizeExpr(item)) |canonicalized| {
                    try self.env.store.addScratchExpr(canonicalized);
                }
            }

            // Create span of the new scratch expressions
            const elems_span = try self.env.store.exprSpanFrom(scratch_top);

            // If all elements failed to canonicalize, treat as empty list
            if (elems_span.span.len == 0) {
                // All elements failed to canonicalize - create empty list
                const expr_idx = try self.env.addExprAndTypeVar(Expr{
                    .e_empty_list = .{},
                }, Content{ .structure = .list_unbound }, region);

                return expr_idx;
            }

            // Initialize the list's type variable to its first element's CIR Index
            // (later steps will unify that type with the other elems' types)
            const first_elem_idx = self.env.store.sliceExpr(elems_span)[0];
            const elem_type_var = @as(TypeVar, @enumFromInt(@intFromEnum(first_elem_idx)));
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_list = .{
                    .elem_var = elem_type_var,
                    .elems = elems_span,
                },
            }, Content{ .structure = .{ .list = elem_type_var } }, region);

            return expr_idx;
        },
        .tag => |e| {
            return try self.canonicalizeTagExpr(e, null);
        },
        .string_part => |_| {
            const feature = try self.env.strings.insert(self.env.gpa, "canonicalize string_part expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .tuple => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Get the list of tuple elems
            const items_slice = self.parse_ir.store.exprSlice(e.items);

            if (items_slice.len == 0) {
                const ast_body = self.parse_ir.store.getExpr(ast_expr_idx);
                const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                return try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .empty_tuple = .{ .region = body_region },
                });
            } else if (items_slice.len == 1) {
                // 1-elem tuple == parenthesized expr

                // NOTE: Returning the sub-expr like this breaks 1-to-1 AST to
                // CIR node mapping. However, this is already broken due to how
                // we insert placeholder type var nodes in other places. So for
                // now, this is fine
                return blk: {
                    if (try self.canonicalizeExpr(items_slice[0])) |idx| {
                        break :blk idx;
                    } else {
                        const ast_body = self.parse_ir.store.getExpr(items_slice[0]);
                        const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                        break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                            .tuple_elem_not_canonicalized = .{ .region = body_region },
                        });
                    }
                };
            } else {
                // Mark the start of scratch expressions for the tuple
                const scratch_top = self.env.store.scratchExprTop();

                // Iterate over the tuple items, canonicalizing each one
                // Then append the resulting expr to the scratch list
                for (items_slice) |item| {
                    const item_expr_idx = blk: {
                        if (try self.canonicalizeExpr(item)) |idx| {
                            break :blk idx;
                        } else {
                            const ast_body = self.parse_ir.store.getExpr(item);
                            const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                            break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                                .tuple_elem_not_canonicalized = .{ .region = body_region },
                            });
                        }
                    };

                    try self.env.store.addScratchExpr(item_expr_idx);
                }

                // Since expr idx map 1-to-1 to variables, we can get cast the slice
                // of scratch expr idx and cast them to vars
                const elems_var_range = try self.env.types.appendVars(
                    @ptrCast(@alignCast(
                        self.env.store.scratch_exprs.slice(scratch_top, self.env.store.scratchExprTop()),
                    )),
                );

                // Create span of the new scratch expressions
                const elems_span = try self.env.store.exprSpanFrom(scratch_top);

                // Then insert the tuple expr
                const expr_idx = try self.env.addExprAndTypeVar(Expr{
                    .e_tuple = .{
                        .elems = elems_span,
                    },
                }, Content{ .structure = FlatType{
                    .tuple = types.Tuple{ .elems = elems_var_range },
                } }, region);

                return expr_idx;
            }
        },
        .record => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize extension if present
            var ext_expr: ?Expr.Idx = null;
            if (e.ext) |ext_ast_idx| {
                ext_expr = try self.canonicalizeExpr(ext_ast_idx);
            }

            const fields_slice = self.parse_ir.store.recordFieldSlice(e.fields);
            if (fields_slice.len == 0) {
                const expr_idx = try self.env.addExprAndTypeVar(Expr{
                    .e_empty_record = .{},
                }, Content{ .structure = .empty_record }, region);

                return expr_idx;
            }

            // Mark the start of scratch record fields for the record
            const scratch_top = self.env.store.scratch_record_fields.top();

            // Track field names to detect duplicates
            const seen_fields_top = self.scratch_seen_record_fields.top();

            // Iterate over the record fields, canonicalizing each one
            // Then append the result to the scratch list
            for (fields_slice) |field| {
                const ast_field = self.parse_ir.store.getRecordField(field);

                // Get the field name identifier
                if (self.parse_ir.tokens.resolveIdentifier(ast_field.name)) |field_name_ident| {
                    const field_name_region = self.parse_ir.tokens.resolve(ast_field.name);

                    // Check for duplicate field names
                    var found_duplicate = false;
                    for (self.scratch_seen_record_fields.sliceFromStart(seen_fields_top)) |seen_field| {
                        if (self.env.idents.identsHaveSameText(field_name_ident, seen_field.ident)) {
                            // Found a duplicate - add diagnostic
                            const diagnostic = Diagnostic{
                                .duplicate_record_field = .{
                                    .field_name = field_name_ident,
                                    .duplicate_region = field_name_region,
                                    .original_region = seen_field.region,
                                },
                            };
                            try self.env.pushDiagnostic(diagnostic);
                            found_duplicate = true;
                            break;
                        }
                    }

                    if (!found_duplicate) {
                        // First occurrence of this field name
                        try self.scratch_seen_record_fields.append(self.env.gpa, SeenRecordField{
                            .ident = field_name_ident,
                            .region = field_name_region,
                        });

                        // Only canonicalize and include non-duplicate fields
                        if (try self.canonicalizeRecordField(field)) |canonicalized| {
                            try self.env.store.scratch_record_fields.append(self.env.gpa, canonicalized);
                        }
                    } else {
                        // TODO: Add diagnostic on duplicate record field
                    }
                } else {
                    // Field name couldn't be resolved, still try to canonicalize
                    if (try self.canonicalizeRecordField(field)) |canonicalized| {
                        try self.env.store.scratch_record_fields.append(self.env.gpa, canonicalized);
                    }
                }
            }

            // Shink the scratch array to it's original size
            self.scratch_seen_record_fields.clearFrom(seen_fields_top);

            // Create span of the new scratch record fields
            const fields_span = try self.env.store.recordFieldSpanFrom(scratch_top);
            // Create fresh type variables for each record field
            // The type checker will unify these with the field expression types
            const cir_fields = self.env.store.sliceRecordFields(fields_span);

            // Create fresh type variables for each field
            const record_fields_top = self.scratch_record_fields.top();

            for (cir_fields) |cir_field_idx| {
                const cir_field = self.env.store.getRecordField(cir_field_idx);
                try self.scratch_record_fields.append(self.env.gpa, types.RecordField{
                    .name = cir_field.name,
                    .var_ = @enumFromInt(@intFromEnum(cir_field.value)),
                });
            }

            // Create the record type structure
            const type_fields_range = try self.env.types.appendRecordFields(
                self.scratch_record_fields.sliceFromStart(record_fields_top),
            );

            // Shink the scratch array to it's original size
            self.scratch_record_fields.clearFrom(record_fields_top);

            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_record = .{
                    .fields = fields_span,
                    .ext = ext_expr,
                },
            }, Content{ .structure = .{ .record_unbound = type_fields_range } }, region);

            return expr_idx;
        },
        .lambda => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Enter function boundary
            try self.enterFunction(region);
            defer self.exitFunction();

            // Enter new scope for function parameters and body
            try self.scopeEnter(self.env.gpa, true); // true = is_function_boundary
            defer self.scopeExit(self.env.gpa) catch {};

            // args
            const gpa = self.env.gpa;
            const args_start = self.env.store.scratch_patterns.top();
            for (self.parse_ir.store.patternSlice(e.args)) |arg_pattern_idx| {
                if (try self.canonicalizePattern(arg_pattern_idx)) |pattern_idx| {
                    try self.env.store.scratch_patterns.append(gpa, pattern_idx);
                } else {
                    const arg = self.parse_ir.store.getPattern(arg_pattern_idx);
                    const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    try self.env.store.scratch_patterns.append(gpa, malformed_idx);
                }
            }
            const args_span = try self.env.store.patternSpanFrom(args_start);

            // body
            const body_idx = blk: {
                if (try self.canonicalizeExpr(e.body)) |idx| {
                    break :blk idx;
                } else {
                    const ast_body = self.parse_ir.store.getExpr(e.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                    break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .lambda_body_not_canonicalized = .{ .region = body_region },
                    });
                }
            };

            // Create lambda expression with function type
            const lambda_type_content = try self.env.types.mkFuncUnbound(
                @ptrCast(self.env.store.slicePatterns(args_span)),
                varFrom(body_idx),
            );
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_lambda = .{
                    .args = args_span,
                    .body = body_idx,
                },
            }, lambda_type_content, region);

            return expr_idx;
        },
        .record_updater => |_| {
            const feature = try self.env.strings.insert(self.env.gpa, "canonicalize record_updater expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .field_access => |field_access| {
            // Try module-qualified lookup first (e.g., Json.utf8)
            if (try self.tryModuleQualifiedLookup(field_access)) |expr_idx| {
                return expr_idx;
            }

            // Regular field access canonicalization
            return try self.canonicalizeRegularFieldAccess(field_access);
        },
        .local_dispatch => |_| {
            const feature = try self.env.strings.insert(self.env.gpa, "canonicalize local_dispatch expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .bin_op => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize left and right operands
            const lhs = blk: {
                if (try self.canonicalizeExpr(e.left)) |left_expr_idx| {
                    break :blk left_expr_idx;
                } else {
                    // TODO should probably use LHS region here
                    const left_expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                    break :blk left_expr_idx;
                }
            };

            const rhs = blk: {
                if (try self.canonicalizeExpr(e.right)) |right_expr_idx| {
                    break :blk right_expr_idx;
                } else {
                    // TODO should probably use RHS region here
                    const right_expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = region,
                    } });
                    break :blk right_expr_idx;
                }
            };

            // Get the operator token
            const op_token = self.parse_ir.tokens.tokens.get(e.operator);

            const op: Expr.Binop.Op = switch (op_token.tag) {
                .OpPlus => .add,
                .OpBinaryMinus => .sub,
                .OpStar => .mul,
                .OpSlash => .div,
                .OpPercent => .rem,
                .OpLessThan => .lt,
                .OpGreaterThan => .gt,
                .OpLessThanOrEq => .le,
                .OpGreaterThanOrEq => .ge,
                .OpEquals => .eq,
                .OpNotEquals => .ne,
                .OpCaret => .pow,
                .OpDoubleSlash => .div_trunc,
                .OpAnd => .@"and",
                .OpOr => .@"or",
                .OpPizza => .pipe_forward,
                .OpDoubleQuestion => .null_coalesce,
                else => {
                    // Unknown operator
                    const feature = try self.env.strings.insert(self.env.gpa, "binop");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return expr_idx;
                },
            };

            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_binop = Expr.Binop.init(op, lhs, rhs),
            }, Content{ .flex_var = null }, region);

            return expr_idx;
        },
        .suffix_single_question => |_| {
            const feature = try self.env.strings.insert(self.env.gpa, "canonicalize suffix_single_question expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .unary_op => |unary| {
            const region = self.parse_ir.tokenizedRegionToRegion(unary.region);
            const operator_token = self.parse_ir.tokens.tokens.get(unary.operator);

            switch (operator_token.tag) {
                .OpUnaryMinus => {
                    // Canonicalize the operand expression
                    const operand_expr = (try self.canonicalizeExpr(unary.expr)) orelse {
                        const feature = try self.env.strings.insert(self.env.gpa, "canonicalize unary_minus operand");
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = region,
                        } });
                        return expr_idx;
                    };

                    // Create unary minus CIR expression
                    const expr_idx = try self.env.addExprAndTypeVar(Expr{
                        .e_unary_minus = Expr.UnaryMinus.init(operand_expr),
                    }, Content{ .flex_var = null }, region);

                    return expr_idx;
                },
                else => {
                    // Other operators not yet implemented or malformed
                    const feature = try self.env.strings.insert(self.env.gpa, "canonicalize unary_op expression (non-minus)");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return expr_idx;
                },
            }
        },
        .if_then_else => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Start collecting if-branches
            const scratch_top = self.env.store.scratchIfBranchTop();

            // Flatten the if-then-else chain
            const final_else = try self.flattenIfThenElseChainRecursive(e);
            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);

            // Get the first branch's body to redirect to it
            const branches = self.env.store.sliceIfBranches(branches_span);
            std.debug.assert(branches.len > 0);

            // Create the if expression with flex var initially
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_if = .{
                    .branches = branches_span,
                    .final_else = final_else,
                },
            }, Content{ .flex_var = null }, region);

            // Immediately redirect the if expression's type variable to the first branch's body
            const first_branch = self.env.store.getIfBranch(branches[0]);
            const first_branch_type_var = @as(TypeVar, @enumFromInt(@intFromEnum(first_branch.body)));
            const expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(expr_idx)));
            try self.env.types.setVarRedirect(expr_var, first_branch_type_var);

            return expr_idx;
        },
        .match => |m| {
            const region = self.parse_ir.tokenizedRegionToRegion(m.region);

            // Canonicalize the condition expression
            const cond_expr = try self.canonicalizeExpr(m.expr) orelse {
                return null;
            };

            // Mark the start of scratch match branches
            const scratch_top = self.env.store.scratchMatchBranchTop();

            // Process each branch
            var mb_branch_var: ?TypeVar = null;
            const branches_slice = self.parse_ir.store.matchBranchSlice(m.branches);
            for (branches_slice, 0..) |ast_branch_idx, index| {
                const ast_branch = self.parse_ir.store.getBranch(ast_branch_idx);

                // Enter a new scope for this branch so pattern variables are isolated
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {};

                // Mark the start of the scratch match branch patterns
                const branch_pat_scratch_top = self.env.store.scratchMatchBranchPatternTop();

                // Canonicalized the branch pattern(s)
                // Handle alternatives patterns by flattening them into multiple BranchPattern entries
                {
                    const pattern = self.parse_ir.store.getPattern(ast_branch.pattern);

                    switch (pattern) {
                        .alternatives => |alt| {
                            // Handle alternatives patterns by creating multiple BranchPattern entries
                            const alt_patterns = self.parse_ir.store.patternSlice(alt.patterns);
                            for (alt_patterns) |alt_pattern_idx| {
                                const alt_pattern = self.parse_ir.store.getPattern(alt_pattern_idx);
                                const alt_pattern_region = self.parse_ir.tokenizedRegionToRegion(alt_pattern.to_tokenized_region());

                                const pattern_idx = blk: {
                                    if (try self.canonicalizePattern(alt_pattern_idx)) |pattern_idx| {
                                        break :blk pattern_idx;
                                    } else {
                                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                            .region = alt_pattern_region,
                                        } });
                                        break :blk malformed_idx;
                                    }
                                };

                                const branch_pattern_idx = try self.env.addMatchBranchPatternAndTypeVar(Expr.Match.BranchPattern{
                                    .pattern = pattern_idx,
                                    .degenerate = false,
                                }, Content{ .flex_var = null }, alt_pattern_region);
                                try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
                            }
                        },
                        else => {
                            // Single pattern case
                            const pattern_region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region());
                            const pattern_idx = blk: {
                                if (try self.canonicalizePattern(ast_branch.pattern)) |pattern_idx| {
                                    break :blk pattern_idx;
                                } else {
                                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                        .region = pattern_region,
                                    } });
                                    break :blk malformed_idx;
                                }
                            };
                            const branch_pattern_idx = try self.env.addMatchBranchPatternAndTypeVar(Expr.Match.BranchPattern{
                                .pattern = pattern_idx,
                                .degenerate = false,
                            }, Content{ .flex_var = null }, pattern_region);
                            try self.env.store.addScratchMatchBranchPattern(branch_pattern_idx);
                        },
                    }
                }

                // Get the pattern span
                const branch_pat_span = try self.env.store.matchBranchPatternSpanFrom(branch_pat_scratch_top);

                // Canonicalize the branch's body
                const body = self.parse_ir.store.getExpr(ast_branch.body);
                const body_region = self.parse_ir.tokenizedRegionToRegion(body.to_tokenized_region());
                const value_idx = blk: {
                    if (try self.canonicalizeExpr(ast_branch.body)) |body_idx| {
                        break :blk body_idx;
                    } else {
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                            .region = body_region,
                        } });
                        break :blk malformed_idx;
                    }
                };

                const branch_idx = try self.env.addMatchBranchAndTypeVar(Expr.Match.Branch{
                    .patterns = branch_pat_span,
                    .value = value_idx,
                    .guard = null,
                    .redundant = @enumFromInt(0), // TODO
                }, Content{ .flex_var = null }, body_region);

                // Set the branch var
                if (index == 0) {
                    mb_branch_var = @enumFromInt(@intFromEnum(value_idx));
                }

                try self.env.store.addScratchMatchBranch(branch_idx);
            }

            // Create span from scratch branches
            const branches_span = try self.env.store.matchBranchSpanFrom(scratch_top);

            // Create the match expression
            const match_expr = Expr.Match{
                .cond = cond_expr,
                .branches = branches_span,
                .exhaustive = @enumFromInt(0), // Will be set during type checking
            };

            // Create initial content for the match expression
            const initial_content = if (mb_branch_var) |_| Content{ .flex_var = null } else Content{ .err = {} };
            const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_match = match_expr }, initial_content, region);

            // If there is at least 1 branch, then set the root expr to redirect
            // to the type of the match branch
            const expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(expr_idx)));
            if (mb_branch_var) |branch_var| {
                try self.env.types.setVarRedirect(expr_var, branch_var);
            }

            return expr_idx;
        },
        .dbg => |d| {
            // Debug expression - canonicalize the inner expression
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);
            const inner_expr = try self.canonicalizeExpr(d.expr) orelse return null;

            // Create debug expression
            const dbg_expr = try self.env.addExprAndTypeVar(Expr{ .e_dbg = .{
                .expr = inner_expr,
            } }, Content{ .flex_var = null }, region);

            return dbg_expr;
        },
        .record_builder => |_| {
            const feature = try self.env.strings.insert(self.env.gpa, "canonicalize record_builder expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return expr_idx;
        },
        .ellipsis => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const ellipsis_expr = try self.env.addExprAndTypeVar(Expr{ .e_ellipsis = .{} }, Content{ .flex_var = null }, region);
            return ellipsis_expr;
        },
        .block => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Blocks don't introduce function boundaries, but may contain var statements
            try self.scopeEnter(self.env.gpa, false); // false = not a function boundary
            defer self.scopeExit(self.env.gpa) catch {};

            // Keep track of the start position for statements
            const stmt_start = self.env.store.scratch_statements.top();

            // Canonicalize all statements in the block
            const statements = self.parse_ir.store.statementSlice(e.statements);
            var last_expr: ?Expr.Idx = null;

            for (statements, 0..) |stmt_idx, i| {
                // Check if this is the last statement and if it's an expression
                const is_last = (i == statements.len - 1);
                const stmt = self.parse_ir.store.getStatement(stmt_idx);

                if (is_last and (stmt == .expr or stmt == .dbg)) {
                    // For the last expression or debug statement, canonicalize it directly as the final expression
                    // without adding it as a statement
                    switch (stmt) {
                        .expr => |expr_stmt| last_expr = try self.canonicalizeExpr(expr_stmt.expr),
                        .dbg => |d| {
                            // For final debug statements, canonicalize as debug expression
                            const debug_region = self.parse_ir.tokenizedRegionToRegion(d.region);
                            const inner_expr = try self.canonicalizeExpr(d.expr) orelse return null;

                            // Create debug expression
                            const dbg_expr = try self.env.addExprAndTypeVar(Expr{ .e_dbg = .{
                                .expr = inner_expr,
                            } }, Content{ .flex_var = null }, debug_region);
                            last_expr = dbg_expr;
                        },
                        else => unreachable,
                    }
                } else {
                    // Regular statement processing
                    const result = try self.canonicalizeStatement(stmt_idx);
                    if (result) |expr_idx| {
                        last_expr = expr_idx;
                    }
                }
            }

            // Determine the final expression
            const final_expr = if (last_expr) |expr_idx| blk: {
                break :blk expr_idx;
            } else blk: {
                // Empty block - create empty record
                const expr_idx = try self.env.addExprAndTypeVar(Expr{
                    .e_empty_record = .{},
                }, Content{ .structure = .empty_record }, region);
                break :blk expr_idx;
            };
            const final_expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(final_expr)));

            // Create statement span
            const stmt_span = try self.env.store.statementSpanFrom(stmt_start);

            // Create and return block expression
            const block_expr = Expr{
                .e_block = .{
                    .stmts = stmt_span,
                    .final_expr = final_expr,
                },
            };
            const block_idx = try self.env.addExprAndTypeVar(block_expr, Content{ .flex_var = null }, region);
            const block_var = @as(TypeVar, @enumFromInt(@intFromEnum(block_idx)));

            // Set the root block expr to redirect to the final expr var
            try self.env.types.setVarRedirect(block_var, final_expr_var);

            return block_idx;
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
            return null;
        },
    }
}

// Canonicalize a tag expr
fn canonicalizeTagExpr(self: *Self, e: AST.TagExpr, mb_args: ?AST.Expr.Span) std.mem.Allocator.Error!?Expr.Idx {
    const region = self.parse_ir.tokenizedRegionToRegion(e.region);

    const parse_tag_name = self.parse_ir.tokens.resolveIdentifier(e.token) orelse return null;
    const tag_name_text = self.parse_ir.env.idents.getText(parse_tag_name);
    const tag_name = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(tag_name_text), region);

    var args_span = Expr.Span{ .span = base.DataSpan.empty() };

    if (mb_args) |args| {
        if (args.span.len > 0) {
            // Canonicalize all arguments
            const scratch_top = self.env.store.scratchExprTop();

            // Canonicalize all arguments
            const args_slice = self.parse_ir.store.exprSlice(args);
            for (args_slice) |arg| {
                if (try self.canonicalizeExpr(arg)) |canonicalized_arg_expr_idx| {
                    try self.env.store.addScratchExpr(canonicalized_arg_expr_idx);
                }
            }

            args_span = try self.env.store.exprSpanFrom(scratch_top);
        }
    }

    // Create a single tag, open tag union for this variable
    // Use a placeholder ext_var that will be handled during type checking
    // TODO(jared): Parent
    const ext_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
    const tag = try self.env.types.mkTag(tag_name, @ptrCast(self.env.store.sliceExpr(args_span)));
    const tag_union = try self.env.types.mkTagUnion(&[_]Tag{tag}, ext_var);

    // Create the tag expression with the tag union type
    const tag_expr_idx = try self.env.addExprAndTypeVar(Expr{
        .e_tag = .{
            .name = tag_name,
            .args = args_span,
        },
    }, tag_union, region);

    if (e.qualifiers.span.len == 0) {
        // Check if this is an unqualified nominal tag (e.g. True or False are in scope unqualified by default)
        if (self.unqualified_nominal_tags.get(tag_name_text)) |nominal_type_decl| {
            // Get the type variable for the nominal type declaration (e.g., Bool type)
            const nominal_type_var = castIdx(Statement.Idx, TypeVar, nominal_type_decl);
            const resolved = self.env.types.resolveVar(nominal_type_var);
            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal_type_decl,
                    .backing_expr = tag_expr_idx,
                    .backing_type = .tag,
                },
            }, resolved.desc.content, region);
            return expr_idx;
        }

        // If this is a tag without a prefix and not in unqualified_nominal_tags,
        // then it is an anonymous tag and we can just return it
        return tag_expr_idx;
    } else {
        // If this is a tag with a prefix, then is it a nominal tag.
        //
        // TODO: Currently this just get the last qualified, then
        // looks up the associated type. Is this right?

        // Get the last token of the qualifiers
        const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
        const last_tok_idx = qualifier_toks[e.qualifiers.span.len - 1];
        const last_tok_ident, const last_tok_region = self.parse_ir.tokens.resolveIdentifierAndRegion(last_tok_idx) orelse {
            const feature = try self.env.strings.insert(
                self.env.gpa,
                "tag qualifier token is not an ident",
            );
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
        };

        // Lookup last token (assumed to be a type decl) in scope
        const nominal_type_decl = self.scopeLookupTypeDecl(last_tok_ident) orelse
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                .ident = last_tok_ident,
                .region = last_tok_region,
            } });

        const nominal_type_var = castIdx(Statement.Idx, TypeVar, nominal_type_decl);
        const resolved = self.env.types.resolveVar(nominal_type_var);

        const expr_idx = try self.env.addExprAndTypeVar(Expr{
            .e_nominal = .{
                .nominal_type_decl = nominal_type_decl,
                .backing_expr = tag_expr_idx,
                .backing_type = .tag,
            },
        }, resolved.desc.content, last_tok_region);

        return expr_idx;
    }
}

/// Extract string segments from parsed string parts
fn extractStringSegments(self: *Self, parts: []const AST.Expr.Idx) std.mem.Allocator.Error!Expr.Span {
    const gpa = self.env.gpa;
    const start = self.env.store.scratchExprTop();

    for (parts) |part| {
        const part_node = self.parse_ir.store.getExpr(part);
        switch (part_node) {
            .string_part => |sp| {
                // get the raw text of the string part
                const part_text = self.parse_ir.resolve(sp.token);

                // intern the string in the ModuleEnv
                const string_idx = try self.env.strings.insert(gpa, part_text);

                // create a node for the string literal
                const str_expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_str_segment = .{
                    .literal = string_idx,
                } }, Content{ .structure = .str }, self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region()));

                // add the node idx to our scratch expr stack
                try self.env.store.addScratchExpr(str_expr_idx);
            },
            else => {

                // Any non-string-part is an interpolation
                if (try self.canonicalizeExpr(part)) |expr_idx| {
                    // append our interpolated expression
                    try self.env.store.addScratchExpr(expr_idx);
                } else {
                    // unable to canonicalize the interpolation, push a malformed node
                    const region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_string_interpolation = .{
                        .region = region,
                    } });
                    try self.env.store.addScratchExpr(malformed_idx);
                }
            },
        }
    }

    return try self.env.store.exprSpanFrom(start);
}

fn canonicalizePattern(
    self: *Self,
    ast_pattern_idx: AST.Pattern.Idx,
) std.mem.Allocator.Error!?Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const gpa = self.env.gpa;
    switch (self.parse_ir.store.getPattern(ast_pattern_idx)) {
        .ident => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            if (self.parse_ir.tokens.resolveIdentifier(e.ident_tok)) |ident_idx| {
                // Create a Pattern node for our identifier
                const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{ .assign = .{
                    .ident = ident_idx,
                } }, .{ .flex_var = null }, region);

                // Introduce the identifier into scope mapping to this pattern node
                switch (try self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, ident_idx, pattern_idx, false, true)) {
                    .success => {},
                    .shadowing_warning => |shadowed_pattern_idx| {
                        const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                        try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } });
                    },
                    .top_level_var_error => {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                            .invalid_top_level_statement = .{
                                .stmt = try self.env.strings.insert(self.env.gpa, "var"),
                                .region = region,
                            },
                        });
                    },
                    .var_across_function_boundary => {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_already_in_scope = .{
                            .ident = ident_idx,
                            .region = region,
                        } });
                    },
                }

                return pattern_idx;
            } else {
                const feature = try self.env.strings.insert(self.env.gpa, "report an error when unable to resolve identifier");
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
                return malformed_idx;
            }
        },
        .underscore => |p| {
            const region = self.parse_ir.tokenizedRegionToRegion(p.region);
            const underscore_pattern = Pattern{
                .underscore = {},
            };

            const pattern_idx = try self.env.addPatternAndTypeVar(underscore_pattern, Content{ .flex_var = null }, region);

            return pattern_idx;
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            // Parse as integer
            const value = std.fmt.parseInt(i128, token_text, 10) catch {
                // Invalid integer literal
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{
                    .region = region,
                } });
                return malformed_idx;
            };

            // Calculate requirements based on the value
            const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            // This is done branchlessly by checking if the value is negative and its
            // magnitude is a power of 2.
            const is_negative = @as(u1, @intFromBool(value < 0));
            const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
            const is_minimum_signed = is_negative & is_power_of_2;

            // If it's a minimum signed value, we subtract 1 from the magnitude before
            // calculating bits needed. This makes -128 report as needing 7 bits instead of 8.
            const adjusted_val = u128_val - is_minimum_signed;

            const requirements = types.Num.Int.Requirements{
                .sign_needed = value < 0,
                .bits_needed = types.Num.Int.BitsNeeded.fromValue(adjusted_val),
            };

            const int_requirements = types.Num.IntRequirements{
                .sign_needed = requirements.sign_needed,
                .bits_needed = @intCast(@intFromEnum(requirements.bits_needed)),
            };
            const int_pattern = Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value), .kind = .i128 },
                },
            };
            const pattern_idx = try self.env.addPatternAndTypeVar(int_pattern, Content{
                .structure = .{ .num = .{ .num_unbound = int_requirements } },
            }, region);

            return pattern_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumLiteral => {
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return malformed_idx;
                },
            };

            // Parse the literal first to get requirements
            const requirements = switch (parsed) {
                .small => |small_info| small_info.requirements,
                .dec => |dec_info| dec_info.requirements,
                .f64 => |f64_info| f64_info.requirements,
            };

            const frac_requirements = types.Num.FracRequirements{
                .fits_in_f32 = requirements.fits_in_f32,
                .fits_in_dec = requirements.fits_in_dec,
            };

            // Check for f64 literals which are not allowed in patterns
            if (parsed == .f64) {
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .f64_pattern_literal = .{
                    .region = region,
                } });
                return malformed_idx;
            }

            const cir_pattern = switch (parsed) {
                .small => |small_info| Pattern{
                    .small_dec_literal = .{
                        .numerator = small_info.numerator,
                        .denominator_power_of_ten = small_info.denominator_power_of_ten,
                    },
                },
                .dec => |dec_info| Pattern{
                    .dec_literal = .{
                        .value = RocDec{ .num = dec_info.value.num },
                    },
                },
                .f64 => unreachable, // Already handled above
            };

            const pattern_idx = try self.env.addPatternAndTypeVar(cir_pattern, Content{
                .structure = .{ .num = .{ .frac_unbound = frac_requirements } },
            }, region);

            return pattern_idx;
        },
        .string => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.string_tok);

            // TODO: Handle escape sequences
            // For now, just intern the raw string
            const literal = try self.env.strings.insert(gpa, token_text);

            const str_pattern = Pattern{
                .str_literal = .{
                    .literal = literal,
                },
            };
            const pattern_idx = try self.env.addPatternAndTypeVar(str_pattern, Content{ .structure = .str }, region);

            return pattern_idx;
        },
        .single_quote => |e| {
            return try self.canonicalizeSingleQuote(e.region, e.token, Pattern.Idx);
        },
        .tag => |e| {
            const tag_name = self.parse_ir.tokens.resolveIdentifier(e.tag_tok) orelse return null;

            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalized the tags args
            const patterns_start = self.env.store.scratch_patterns.top();
            for (self.parse_ir.store.patternSlice(e.args)) |sub_ast_pattern_idx| {
                if (try self.canonicalizePattern(sub_ast_pattern_idx)) |idx| {
                    try self.env.store.scratch_patterns.append(gpa, idx);
                } else {
                    const arg = self.parse_ir.store.getPattern(sub_ast_pattern_idx);
                    const arg_region = self.parse_ir.tokenizedRegionToRegion(arg.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_arg_invalid = .{
                        .region = arg_region,
                    } });
                    try self.env.store.scratch_patterns.append(gpa, malformed_idx);
                }
            }
            const args = try self.env.store.patternSpanFrom(patterns_start);

            // Create the pattern type var first
            const arg_vars: []TypeVar = @ptrCast(self.env.store.slicePatterns(args));
            // We need to create a temporary pattern idx to get the type var
            const ext_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
            const tag = try self.env.types.mkTag(tag_name, arg_vars);
            const tag_union_type = try self.env.types.mkTagUnion(&[_]Tag{tag}, ext_var);

            // Create the pattern node with type var
            const tag_pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                .applied_tag = .{
                    .name = tag_name,
                    .args = args,
                },
            }, tag_union_type, region);

            if (e.qualifiers.span.len == 0) {
                // If this is a tag without a prefix, then is it an
                // anonymous tag and we can just return it
                return tag_pattern_idx;
            } else {
                // If this is a tag with a prefix, then is it a nominal tag.
                //
                // TODO: Currently this just get the last qualified, then
                // looks up the associated type. Is this right?

                // Get the last token of the qualifiers
                const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
                const last_tok_idx = qualifier_toks[e.qualifiers.span.len - 1];
                const last_tok_ident, const last_tok_region = self.parse_ir.tokens.resolveIdentifierAndRegion(last_tok_idx) orelse {
                    const feature = try self.env.strings.insert(
                        self.env.gpa,
                        "tag qualifier token is not an ident",
                    );
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                };

                // Lookup last token (assumed to be a type decl) in scope
                const nominal_type_decl = self.scopeLookupTypeDecl(last_tok_ident) orelse
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_not_in_scope = .{
                        .ident = last_tok_ident,
                        .region = last_tok_region,
                    } });

                // Create the nominal pattern
                // In type checking, this will be unified with the nominal type if the `tag` is valid
                const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                    .nominal = .{
                        .nominal_type_decl = nominal_type_decl,
                        .backing_pattern = tag_pattern_idx,
                        .backing_type = .tag,
                    },
                }, Content{ .flex_var = null }, last_tok_region);

                return pattern_idx;
            }
        },
        .record => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch record destructs
            const scratch_top = self.env.store.scratchRecordDestructTop();

            // Process each field in the record pattern
            for (self.parse_ir.store.patternRecordFieldSlice(e.fields)) |field_idx| {
                const field = self.parse_ir.store.getPatternRecordField(field_idx);
                const field_region = self.parse_ir.tokenizedRegionToRegion(field.region);

                // Resolve the field name
                if (self.parse_ir.tokens.resolveIdentifier(field.name)) |field_name_ident| {
                    // For simple destructuring like `{ name, age }`, both label and ident are the same
                    if (field.value) |sub_pattern_idx| {
                        // Handle patterns like `{ name: x }` or `{ address: { city } }` where there's a sub-pattern
                        const canonicalized_sub_pattern = try self.canonicalizePattern(sub_pattern_idx) orelse {
                            // If sub-pattern canonicalization fails, return malformed pattern
                            const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                                .region = field_region,
                            } });
                            return malformed_idx;
                        };

                        // Create the RecordDestruct with sub-pattern
                        const record_destruct = Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .{ .SubPattern = canonicalized_sub_pattern },
                        };

                        const destruct_idx = try self.env.addRecordDestructAndTypeVar(record_destruct, .{ .flex_var = null }, field_region);
                        try self.env.store.addScratchRecordDestruct(destruct_idx);
                    } else {
                        // Simple case: Create the RecordDestruct for this field
                        const record_destruct = Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .Required,
                        };

                        const destruct_idx = try self.env.addRecordDestructAndTypeVar(record_destruct, .{ .flex_var = null }, field_region);
                        try self.env.store.addScratchRecordDestruct(destruct_idx);

                        // Create an assign pattern for this identifier and introduce it into scope
                        const assign_pattern_idx = try self.env.addPatternAndTypeVar(Pattern{ .assign = .{
                            .ident = field_name_ident,
                        } }, .{ .flex_var = null }, field_region);

                        // Introduce the identifier into scope
                        switch (try self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, field_name_ident, assign_pattern_idx, false, true)) {
                            .success => {},
                            .shadowing_warning => |shadowed_pattern_idx| {
                                const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                                try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                    .ident = field_name_ident,
                                    .region = field_region,
                                    .original_region = original_region,
                                } });
                            },
                            .top_level_var_error => {
                                const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                                    .invalid_top_level_statement = .{
                                        .stmt = try self.env.strings.insert(self.env.gpa, "var"),
                                        .region = field_region,
                                    },
                                });
                                return pattern_idx;
                            },
                            .var_across_function_boundary => {
                                const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_already_in_scope = .{
                                    .ident = field_name_ident,
                                    .region = field_region,
                                } });
                                return pattern_idx;
                            },
                        }
                    }
                } else {
                    const feature = try self.env.strings.insert(self.env.gpa, "report an error when unable to resolve field identifier");
                    const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = field_region,
                    } });
                    return pattern_idx;
                }
            }

            // Create span of the new scratch record destructs
            const destructs_span = try self.env.store.recordDestructSpanFrom(scratch_top);

            // Create type variables for the record
            // TODO: Remove `var`s from pattern node?
            const whole_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
            const ext_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);

            // Create the record destructure pattern
            const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                .record_destructure = .{
                    .whole_var = whole_var,
                    .ext_var = ext_var,
                    .destructs = destructs_span,
                },
            }, .{ .flex_var = null }, region);

            return pattern_idx;
        },
        .tuple => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch patterns for the tuple
            const scratch_top = self.env.store.scratchPatternTop();

            // Iterate over the tuple patterns, canonicalizing each one
            // Then append the result to the scratch list
            const patterns_slice = self.parse_ir.store.patternSlice(e.patterns);

            for (patterns_slice) |pattern| {
                if (try self.canonicalizePattern(pattern)) |canonicalized| {
                    try self.env.store.addScratchPattern(canonicalized);
                }
            }

            // Create span of the new scratch patterns
            const patterns_span = try self.env.store.patternSpanFrom(scratch_top);

            // Since pattern idx map 1-to-1 to variables, we can get cast the
            // slice of and cast them to vars
            const elems_var_range = try self.env.types.appendVars(
                @ptrCast(@alignCast(self.env.store.slicePatterns(patterns_span))),
            );

            const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                .tuple = .{
                    .patterns = patterns_span,
                },
            }, Content{ .structure = FlatType{
                .tuple = types.Tuple{ .elems = elems_var_range },
            } }, region);

            return pattern_idx;
        },
        .list => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Mark the start of scratch patterns for non-rest patterns only
            const scratch_top = self.env.store.scratchPatternTop();

            // Track rest pattern information
            var rest_index: ?u32 = null;
            var rest_pattern: ?Pattern.Idx = null;

            // Process all patterns, tracking rest position and canonicalizing non-rest patterns
            const patterns_slice = self.parse_ir.store.patternSlice(e.patterns);
            for (patterns_slice) |pattern_idx| {
                const ast_pattern = self.parse_ir.store.getPattern(pattern_idx);

                if (ast_pattern == .list_rest) {
                    // Check for multiple rest patterns (not allowed)
                    if (rest_index != null) {
                        const list_rest_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.list_rest.region);
                        try self.env.pushDiagnostic(Diagnostic{ .pattern_not_canonicalized = .{
                            .region = list_rest_region,
                        } });
                        continue;
                    }

                    const list_rest_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.list_rest.region);

                    // Handle named vs unnamed rest patterns
                    var current_rest_pattern: ?Pattern.Idx = null;
                    if (ast_pattern.list_rest.name) |name_tok| {
                        if (self.parse_ir.tokens.resolveIdentifier(name_tok)) |ident_idx| {
                            // Create an assign pattern for the rest variable
                            // Use the region of just the identifier token, not the full rest pattern
                            const name_region = self.parse_ir.tokenizedRegionToRegion(.{ .start = name_tok, .end = name_tok });
                            // Note: The rest variable's type will be set later when we know elem_var
                            // For now, just give it a flex var
                            const assign_idx = try self.env.addPatternAndTypeVar(Pattern{ .assign = .{
                                .ident = ident_idx,
                            } }, Content{ .flex_var = null }, name_region);

                            // Introduce the identifier into scope
                            switch (try self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, ident_idx, assign_idx, false, true)) {
                                .success => {},
                                .shadowing_warning => |shadowed_pattern_idx| {
                                    const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                                    try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                                        .ident = ident_idx,
                                        .region = name_region,
                                        .original_region = original_region,
                                    } });
                                },
                                .top_level_var_error => {},
                                .var_across_function_boundary => {
                                    try self.env.pushDiagnostic(Diagnostic{ .ident_already_in_scope = .{
                                        .ident = ident_idx,
                                        .region = list_rest_region,
                                    } });
                                },
                            }

                            current_rest_pattern = assign_idx;
                        } else {
                            const feature = try self.env.strings.insert(gpa, "list rest pattern with unresolvable name");
                            const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                                .feature = feature,
                                .region = list_rest_region,
                            } });
                            current_rest_pattern = malformed_idx;
                        }
                    }
                    // For unnamed rest patterns, current_rest_pattern remains null

                    // Store rest information
                    // The rest_index should be the number of patterns canonicalized so far
                    const patterns_so_far = self.env.store.scratch_patterns.top() - scratch_top;
                    rest_index = @intCast(patterns_so_far);
                    rest_pattern = current_rest_pattern;
                } else {
                    // Regular pattern - canonicalize it and add to scratch patterns
                    if (try self.canonicalizePattern(pattern_idx)) |canonicalized| {
                        try self.env.store.scratch_patterns.append(gpa, canonicalized);
                    } else {
                        const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.to_tokenized_region());
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .pattern_not_canonicalized = .{
                            .region = pattern_region,
                        } });
                        try self.env.store.scratch_patterns.append(gpa, malformed_idx);
                    }
                }
            }

            // Create span of the canonicalized non-rest patterns
            const patterns_span = try self.env.store.patternSpanFrom(scratch_top);

            // Handle empty list patterns specially
            if (patterns_span.span.len == 0 and rest_index == null) {
                // Empty list pattern - create a simple pattern without elem_var
                const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                    .list = .{
                        .list_var = @enumFromInt(0), // Will be set by addPatternAndTypeVar
                        .elem_var = @enumFromInt(0), // Not used for empty lists
                        .patterns = patterns_span,
                        .rest_info = null,
                    },
                }, Content{ .structure = .list_unbound }, region);

                return pattern_idx;
            }

            // For non-empty list patterns, use the first pattern's type variable as elem_var
            const elem_var: TypeVar = if (patterns_span.span.len > 0) blk: {
                const first_pattern_idx = self.env.store.slicePatterns(patterns_span)[0];
                break :blk @enumFromInt(@intFromEnum(first_pattern_idx));
            } else blk: {
                // Must be a rest-only pattern like [..] or [.. as rest]
                // Create a placeholder pattern for the element type
                const placeholder_idx = try self.env.addPatternAndTypeVar(Pattern{
                    .underscore = {},
                }, Content{ .flex_var = null }, region);
                break :blk @enumFromInt(@intFromEnum(placeholder_idx));
            };

            // Update rest pattern's type if it exists
            if (rest_pattern) |rest_pat| {
                // Update the rest pattern's type to be a list of elem_var
                const rest_list_type = Content{ .structure = .{ .list = elem_var } };
                _ = try self.env.types.setVarContent(@enumFromInt(@intFromEnum(rest_pat)), rest_list_type);
            }

            // Create the list pattern with rest info
            // Set type variable for the pattern - this should be the list type
            const list_type = Content{ .structure = .{ .list = elem_var } };
            const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{
                .list = .{
                    .list_var = @enumFromInt(0), // Will be set by addPatternAndTypeVar
                    .elem_var = elem_var,
                    .patterns = patterns_span,
                    .rest_info = if (rest_index) |idx| .{ .index = idx, .pattern = rest_pattern } else null,
                },
            }, list_type, region);

            return pattern_idx;
        },
        .list_rest => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const feature = try self.env.strings.insert(self.env.gpa, "standalone list rest pattern");
            const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return pattern_idx;
        },
        .alternatives => |_| {
            // Alternatives patterns should only appear in match expressions and are handled there
            // If we encounter one here, it's likely a parser error or misplaced pattern
            const feature = try self.env.strings.insert(self.env.gpa, "alternatives pattern outside match expression");
            const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return pattern_idx;
        },
        .as => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize the inner pattern
            const inner_pattern = try self.canonicalizePattern(e.pattern) orelse {
                const feature = try self.env.strings.insert(self.env.gpa, "canonicalize as pattern with malformed inner pattern");
                const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return pattern_idx;
            };

            // Resolve the identifier name
            if (self.parse_ir.tokens.resolveIdentifier(e.name)) |ident_idx| {
                // Create the as pattern
                const as_pattern = Pattern{
                    .as = .{
                        .pattern = inner_pattern,
                        .ident = ident_idx,
                    },
                };

                const pattern_idx = try self.env.addPatternAndTypeVar(as_pattern, .{ .flex_var = null }, region);

                // Introduce the identifier into scope
                switch (try self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, ident_idx, pattern_idx, false, true)) {
                    .success => {},
                    .shadowing_warning => |shadowed_pattern_idx| {
                        const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
                        try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                            .ident = ident_idx,
                            .region = region,
                            .original_region = original_region,
                        } });
                    },
                    .top_level_var_error => {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{
                            .invalid_top_level_statement = .{
                                .stmt = try self.env.strings.insert(self.env.gpa, "var"),
                                .region = region,
                            },
                        });
                    },
                    .var_across_function_boundary => {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .ident_already_in_scope = .{
                            .ident = ident_idx,
                            .region = region,
                        } });
                    },
                }

                return pattern_idx;
            } else {
                const feature = try self.env.strings.insert(self.env.gpa, "report an error when unable to resolve as pattern identifier");
                const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return pattern_idx;
            }
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
            return null;
        },
    }
}

/// Enter a function boundary by pushing its region onto the stack
fn enterFunction(self: *Self, region: Region) std.mem.Allocator.Error!void {
    try self.function_regions.append(self.env.gpa, region);
}

/// Exit a function boundary by popping from the stack
fn exitFunction(self: *Self) void {
    _ = self.function_regions.pop();
}

/// Get the current function region (the function we're currently in)
fn getCurrentFunctionRegion(self: *const Self) ?Region {
    if (self.function_regions.items.len > 0) {
        return self.function_regions.items[self.function_regions.items.len - 1];
    }
    return null;
}

/// Record which function a var pattern was declared in
fn recordVarFunction(self: *Self, pattern_idx: Pattern.Idx) std.mem.Allocator.Error!void {
    // Mark this pattern as a var
    try self.var_patterns.put(self.env.gpa, pattern_idx, {});

    if (self.getCurrentFunctionRegion()) |function_region| {
        try self.var_function_regions.put(self.env.gpa, pattern_idx, function_region);
    }
}

/// Check if a pattern is a var
fn isVarPattern(self: *const Self, pattern_idx: Pattern.Idx) bool {
    return self.var_patterns.contains(pattern_idx);
}

/// Check if a var reassignment crosses function boundaries
fn isVarReassignmentAcrossFunctionBoundary(self: *const Self, pattern_idx: Pattern.Idx) bool {
    if (self.var_function_regions.get(pattern_idx)) |var_function_region| {
        if (self.getCurrentFunctionRegion()) |current_function_region| {
            return !var_function_region.eq(current_function_region);
        }
    }
    return false;
}

// Check if the given f64 fits in f32 range (ignoring precision loss)
fn fitsInF32(f64_val: f64) bool {
    // Check if it's within the range that f32 can represent.
    // This includes normal, subnormal, and zero values.
    // (This is a magnitude check, so take the abs value to check
    // positive and negative at the same time.)
    const abs_val = @abs(f64_val);
    return abs_val == 0.0 or (abs_val >= std.math.floatTrueMin(f32) and abs_val <= std.math.floatMax(f32));
}

// Check if a float value can be represented accurately in RocDec
fn fitsInDec(value: f64) bool {
    // RocDec uses i128 with 18 decimal places
    const max_dec_value = 170141183460469231731.0;
    const min_dec_value = -170141183460469231731.0;

    return value >= min_dec_value and value <= max_dec_value;
}

// Result type for parsing fractional literals into small, Dec, or f64
const FracLiteralResult = union(enum) {
    small: struct {
        numerator: i16,
        denominator_power_of_ten: u8,
        requirements: types.Num.Frac.Requirements,
    },
    dec: struct {
        value: RocDec,
        requirements: types.Num.Frac.Requirements,
    },
    f64: struct {
        value: f64,
        requirements: types.Num.Frac.Requirements,
    },
};

// Try to parse a fractional literal as a small dec (numerator/10^power)
fn parseSmallDec(token_text: []const u8) ?struct { numerator: i16, denominator_power_of_ten: u8 } {
    // Return null if input is too long to fit in our 32-byte buffer
    if (token_text.len > 32) return null;

    // For negative zero, we'll return null to force f64 path
    if (token_text.len > 0 and token_text[0] == '-') {
        const rest = token_text[1..];
        // Check if it's -0, -0.0, -0.00, etc.
        var all_zeros = true;
        for (rest) |c| {
            if (c != '0' and c != '.') {
                all_zeros = false;
                break;
            }
        }
        if (all_zeros) return null;
    }

    // Parse as a whole number by removing the decimal point
    const dot_pos = std.mem.indexOf(u8, token_text, ".") orelse {
        // No decimal point, parse as integer
        const val = std.fmt.parseInt(i32, token_text, 10) catch return null;
        if (val < -32768 or val > 32767) return null;
        return .{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = 0 };
    };

    // Count digits after decimal point
    const after_decimal_len = token_text.len - dot_pos - 1;
    if (after_decimal_len > 255) return null; // Too many decimal places

    // Build the string without the decimal point
    var buf: [32]u8 = undefined;
    var len: usize = 0;

    // Copy part before decimal
    @memcpy(buf[0..dot_pos], token_text[0..dot_pos]);
    len = dot_pos;

    // Copy part after decimal
    if (after_decimal_len > 0) {
        @memcpy(buf[len..][0..after_decimal_len], token_text[dot_pos + 1 ..]);
        len += after_decimal_len;
    }

    // Parse the combined number
    const val = std.fmt.parseInt(i32, buf[0..len], 10) catch return null;
    if (val < -32768 or val > 32767) return null;

    return .{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = @as(u8, @intCast(after_decimal_len)) };
}

// Parse a fractional literal from text and return small, Dec, or F64 value
fn parseFracLiteral(token_text: []const u8) !FracLiteralResult {
    // First, always parse as f64 to get the numeric value
    const f64_val = std.fmt.parseFloat(f64, token_text) catch {
        // If it can't be parsed as F64, it's too big to fit in any of Roc's Frac types.
        return error.InvalidNumLiteral;
    };

    // Check if it has scientific notation
    const has_scientific_notation = blk: {
        for (token_text) |char| {
            if (char == 'e' or char == 'E') {
                break :blk true;
            }
        }
        break :blk false;
    };

    // For non-scientific notation, try the original parseSmallDec first to preserve behavior
    if (!has_scientific_notation) {
        if (parseSmallDec(token_text)) |small| {
            // Convert to f64 to check requirements
            const numerator_f64 = @as(f64, @floatFromInt(small.numerator));
            var divisor: f64 = 1.0;
            var i: u8 = 0;
            while (i < small.denominator_power_of_ten) : (i += 1) {
                divisor *= 10.0;
            }
            const small_f64_val = numerator_f64 / divisor;

            return FracLiteralResult{
                .small = .{
                    .numerator = small.numerator,
                    .denominator_power_of_ten = small.denominator_power_of_ten,
                    .requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fitsInF32(small_f64_val),
                        .fits_in_dec = true,
                    },
                },
            };
        }
    }

    // For scientific notation or when parseSmallDec fails, check if it's a whole number
    const rounded = @round(f64_val);
    if (f64_val == rounded and rounded >= -32768 and rounded <= 32767) {
        // It's a whole number in i16 range, can use small dec with denominator_power_of_ten = 0
        return FracLiteralResult{
            .small = .{
                .numerator = @as(i16, @intFromFloat(rounded)),
                .denominator_power_of_ten = 0,
                .requirements = types.Num.Frac.Requirements{
                    .fits_in_f32 = fitsInF32(f64_val),
                    .fits_in_dec = true,
                },
            },
        };
    }

    // Check if the value can fit in RocDec (whether or not it uses scientific notation)
    // RocDec uses i128 with 18 decimal places
    // We need to check if the value is within RocDec's range
    if (fitsInDec(f64_val)) {
        // Convert f64 to RocDec by multiplying by 10^18
        const dec_scale = std.math.pow(f64, 10, 18);
        const scaled_val = f64_val * dec_scale;

        // i128 max is 170141183460469231731687303715884105727
        // i128 min is -170141183460469231731687303715884105728
        // We need to be more conservative to avoid overflow during conversion
        const i128_max_f64 = 170141183460469231731687303715884105727.0;
        const i128_min_f64 = -170141183460469231731687303715884105728.0;

        if (scaled_val >= i128_min_f64 and scaled_val <= i128_max_f64) {
            // Safe to convert - but check for special cases
            const rounded_val = @round(scaled_val);

            // Extra safety check for boundary values
            if (rounded_val < i128_min_f64 or rounded_val > i128_max_f64) {
                // Would overflow, use f64 instead
                return FracLiteralResult{
                    .f64 = .{
                        .value = f64_val,
                        .requirements = types.Num.Frac.Requirements{
                            .fits_in_f32 = fitsInF32(f64_val),
                            .fits_in_dec = false,
                        },
                    },
                };
            }

            const dec_num = @as(i128, @intFromFloat(rounded_val));

            // Check if the value is too small (would round to 0 or near 0)
            // This prevents loss of precision for very small numbers like 1e-40
            const min_representable = 1e-18; // Smallest non-zero value Dec can represent
            if (@abs(f64_val) > 0 and @abs(f64_val) < min_representable) {
                // Too small for Dec precision, use f64
                return FracLiteralResult{
                    .f64 = .{
                        .value = f64_val,
                        .requirements = types.Num.Frac.Requirements{
                            .fits_in_f32 = fitsInF32(f64_val),
                            .fits_in_dec = false,
                        },
                    },
                };
            }

            return FracLiteralResult{
                .dec = .{
                    .value = RocDec{ .num = dec_num },
                    .requirements = types.Num.Frac.Requirements{
                        .fits_in_f32 = fitsInF32(f64_val),
                        .fits_in_dec = true,
                    },
                },
            };
        }
    }

    // If it doesn't fit in small dec or RocDec, use f64
    return FracLiteralResult{
        .f64 = .{
            .value = f64_val,
            .requirements = types.Num.Frac.Requirements{
                .fits_in_f32 = fitsInF32(f64_val),
                .fits_in_dec = false,
            },
        },
    };
}

test {
    // Test imports commented out - tests need to be run from test.zig
    // _ = @import("canonicalize/test/int_test.zig");
    // _ = @import("canonicalize/test/frac_test.zig");
    // _ = @import("canonicalize/test/node_store_test.zig");
    // _ = @import("canonicalize/test/exposed_shadowing_test.zig");
    _ = @import("let_polymorphism_integration_test.zig");
}

/// Flatten a chain of if-then-else expressions into multiple if-branches
/// Returns the final else expression that is not an if-then-else
fn flattenIfThenElseChainRecursive(self: *Self, if_expr: anytype) std.mem.Allocator.Error!Expr.Idx {
    // Canonicalize and add the current condition/then pair
    const cond_idx = blk: {
        if (try self.canonicalizeExpr(if_expr.condition)) |idx| {
            break :blk idx;
        } else {
            const ast_cond = self.parse_ir.store.getExpr(if_expr.condition);
            const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
            break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                .if_condition_not_canonicalized = .{ .region = cond_region },
            });
        }
    };

    const then_idx = blk: {
        if (try self.canonicalizeExpr(if_expr.then)) |idx| {
            break :blk idx;
        } else {
            const ast_then = self.parse_ir.store.getExpr(if_expr.then);
            const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
            break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{
                .if_then_not_canonicalized = .{ .region = then_region },
            });
        }
    };

    // Add this condition/then pair as an if-branch
    const if_branch = Expr.IfBranch{
        .cond = cond_idx,
        .body = then_idx,
    };
    const region = self.parse_ir.tokenizedRegionToRegion(if_expr.region);
    const if_branch_idx = try self.env.addIfBranchAndTypeVar(if_branch, Content{ .flex_var = null }, region);
    try self.env.store.addScratchIfBranch(if_branch_idx);

    // Check if the else clause is another if-then-else that we should flatten
    const else_expr = self.parse_ir.store.getExpr(if_expr.@"else");
    switch (else_expr) {
        .if_then_else => |nested_if| {
            // Recursively process the nested if-then-else
            return try self.flattenIfThenElseChainRecursive(nested_if);
        },
        else => {
            // This is the final else - canonicalize and return it
            if (try self.canonicalizeExpr(if_expr.@"else")) |else_idx| {
                return else_idx;
            } else {
                const else_region = self.parse_ir.tokenizedRegionToRegion(else_expr.to_tokenized_region());
                return try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .if_else_not_canonicalized = .{ .region = else_region },
                });
            }
        },
    }
}
/// Introduce a new identifier to the current scope, return an
/// index if
fn scopeIntroduceIdent(
    self: Self,
    ident_idx: Ident.Idx,
    pattern_idx: Pattern.Idx,
    region: Region,
    comptime T: type,
) T {
    const result = self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, ident_idx, pattern_idx, false, true);

    switch (result) {
        .success => {
            return pattern_idx;
        },
        .shadowing_warning => |shadowed_pattern_idx| {
            const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
            try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                .ident = ident_idx,
                .region = region,
                .original_region = original_region,
            } });
            return pattern_idx;
        },
        .top_level_var_error => {
            return try self.env.pushMalformed(T, Diagnostic{
                .invalid_top_level_statement = .{
                    .stmt = try self.env.strings.insert(self.env.gpa, "var"),
                    .region = region,
                },
            });
        },
        .var_across_function_boundary => |_| {
            // This shouldn't happen for regular identifiers
            return try self.env.pushMalformed(T, Diagnostic{ .not_implemented = .{
                .feature = try self.env.strings.insert(self.env.gpa, "var across function boundary for non-var identifier"),
                .region = region,
            } });
        },
    }
}

/// Introduce a var identifier to the current scope with function boundary tracking
fn scopeIntroduceVar(
    self: *Self,
    ident_idx: Ident.Idx,
    pattern_idx: Pattern.Idx,
    region: Region,
    is_declaration: bool,
    comptime T: type,
) std.mem.Allocator.Error!T {
    const result = try self.scopeIntroduceInternal(self.env.gpa, &self.env.idents, .ident, ident_idx, pattern_idx, true, is_declaration);

    switch (result) {
        .success => {
            // If this is a var declaration, record which function it belongs to
            if (is_declaration) {
                try self.recordVarFunction(pattern_idx);
            }
            return pattern_idx;
        },
        .shadowing_warning => |shadowed_pattern_idx| {
            const original_region = self.env.store.getPatternRegion(shadowed_pattern_idx);
            try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                .ident = ident_idx,
                .region = region,
                .original_region = original_region,
            } });
            if (is_declaration) {
                try self.recordVarFunction(pattern_idx);
            }
            return pattern_idx;
        },
        .top_level_var_error => {
            return try self.env.pushMalformed(T, Diagnostic{
                .invalid_top_level_statement = .{
                    .stmt = try self.env.strings.insert(self.env.gpa, "var"),
                    .region = region,
                },
            });
        },
        .var_across_function_boundary => |_| {
            // Generate crash expression for var reassignment across function boundary
            return try self.env.pushMalformed(T, Diagnostic{ .var_across_function_boundary = .{
                .region = region,
            } });
        },
    }
}

/// Canonicalize a tag variant within a tag union type annotation
/// Unlike general type canonicalization, this doesn't validate tag names against scope
/// since tags in tag unions are anonymous and defined by the union itself
fn canonicalizeTagVariant(self: *Self, anno_idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);
    switch (ast_anno) {
        .ty => |ty| {
            // For simple tags like `None`, just create the type annotation without scope validation
            const region = self.parse_ir.tokenizedRegionToRegion(ty.region);
            const ident_idx = if (self.parse_ir.tokens.resolveIdentifier(ty.token)) |ident|
                ident
            else
                // Create identifier from text if resolution fails
                try self.env.idents.insert(self.env.gpa, base.Ident.for_text(self.parse_ir.resolve(ty.token)), region);

            return try self.env.addTypeAnnoAndTypeVar(.{ .ty = .{
                .symbol = ident_idx,
            } }, Content{ .flex_var = null }, region);
        },
        .apply => |apply| {
            // For tags with arguments like `Some(Str)`, validate the arguments but not the tag name
            const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
            const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);

            if (args_slice.len == 0) {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            }

            // First argument is the tag name - don't validate it against scope
            const base_type = self.parse_ir.store.getTypeAnno(args_slice[0]);
            const type_name = switch (base_type) {
                .ty => |ty| if (self.parse_ir.tokens.resolveIdentifier(ty.token)) |ident|
                    ident
                else
                    try self.env.idents.insert(self.env.gpa, base.Ident.for_text(self.parse_ir.resolve(ty.token)), region),
                else => return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } }),
            };

            // Canonicalize type arguments (skip first which is the tag name)
            // These should be validated against scope since they're real types like `Str`, `Int`, etc.
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (args_slice[1..]) |arg_idx| {
                const canonicalized = try self.canonicalizeTypeAnno(arg_idx, .can_introduce_vars, false);
                try self.env.store.addScratchTypeAnno(canonicalized);
            }

            const args = try self.env.store.typeAnnoSpanFrom(scratch_top);
            return try self.env.addTypeAnnoAndTypeVar(.{ .apply = .{
                .symbol = type_name,
                .args = args,
            } }, Content{ .flex_var = null }, region);
        },
        else => {
            return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{
                .malformed_type_annotation = .{ .region = self.parse_ir.tokenizedRegionToRegion(ast_anno.toRegion()) },
            });
        },
    }
}

// Some type annotations, like function type annotations, can introduce variables.
// Others, however, like alias or nominal tag annotations, cannot.
const CanIntroduceVars = enum(u1) { can_introduce_vars, cannot_introduce_vars };

fn collectTypeVarProblems(ident: Ident.Idx, is_single_use: bool, ast_anno: AST.TypeAnno.Idx, gpa: std.mem.Allocator, scratch: *base.Scratch(TypeVarProblem)) void {
    // Warn for type variables with trailing underscores
    if (ident.attributes.reassignable) {
        scratch.append(gpa, .{ .ident = ident, .problem = .type_var_ending_in_underscore, .ast_anno = ast_anno });
    }

    // Should start with underscore but doesn't, or should not start with underscore but does.
    if (is_single_use != ident.attributes.ignored) {
        const problem_type: TypeVarProblemKind = if (is_single_use) .unused_type_var else .type_var_marked_unused;
        scratch.append(gpa, .{ .ident = ident, .problem = problem_type, .ast_anno = ast_anno });
    }
}

fn reportTypeVarProblems(self: *Self, problems: []const TypeVarProblem) std.mem.Allocator.Error!void {
    for (problems) |problem| {
        const region = self.getTypeVarRegionFromAST(problem.ast_anno, problem.ident) orelse Region.zero();
        const name_text = self.env.idents.getText(problem.ident);

        switch (problem.problem) {
            .type_var_ending_in_underscore => {
                const suggested_name_text = name_text[0 .. name_text.len - 1]; // Remove the trailing underscore
                const suggested_ident = self.env.idents.insert(self.env.gpa, base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_ending_in_underscore = .{
                    .name = problem.ident,
                    .suggested_name = suggested_ident,
                    .region = region,
                } });
            },
            .unused_type_var => {
                self.env.pushDiagnostic(Diagnostic{ .unused_type_var_name = .{
                    .name = problem.ident,
                    .suggested_name = problem.ident,
                    .region = region,
                } });
            },
            .type_var_marked_unused => {
                const suggested_name_text = name_text[1..]; // Remove the underscore
                const suggested_ident = self.env.idents.insert(self.env.gpa, base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_marked_unused = .{
                    .name = problem.ident,
                    .suggested_name = suggested_ident,
                    .region = region,
                } });
            },
        }
    }
}

fn processCollectedTypeVars(self: *Self) std.mem.Allocator.Error!void {
    // Process all type variables collected during type annotation parsing
    // and report any underscore convention violations
    const problems_start = self.scratch_type_var_problems.top();
    defer self.scratch_type_var_problems.clearFrom(problems_start);

    // Process from the end to avoid index shifting
    while (self.scratch_type_var_validation.items.items.len > 0) {
        // Pop the last item
        const last_idx = self.scratch_type_var_validation.items.items.len - 1;
        const first_ident = self.scratch_type_var_validation.items.items[last_idx];
        self.scratch_type_var_validation.items.shrinkRetainingCapacity(last_idx);
        var found_another = false;

        // Check if there are any other occurrences of this variable
        var i: usize = 0;
        while (i < self.scratch_type_var_validation.items.items.len) {
            if (self.scratch_type_var_validation.items.items[i].idx == first_ident.idx) {
                found_another = true;
                // Remove this occurrence by swapping with the last element and shrinking
                const last = self.scratch_type_var_validation.items.items.len - 1;
                self.scratch_type_var_validation.items.items[i] = self.scratch_type_var_validation.items.items[last];
                self.scratch_type_var_validation.items.shrinkRetainingCapacity(last);
            } else {
                i += 1;
            }
        }

        // Collect problems for this type variable
        const is_single_use = !found_another;
        // Use a dummy AST annotation index since we don't have the context
        collectTypeVarProblems(first_ident, is_single_use, @enumFromInt(0), self.env.gpa, &self.scratch_type_var_problems);
    }

    // Report any problems we found
    const problems = self.scratch_type_var_problems.slice(problems_start, self.scratch_type_var_problems.top());
    // Report problems with zero regions since we don't have AST context
    for (problems) |problem| {
        const name_text = self.env.idents.getText(problem.ident);

        switch (problem.problem) {
            .type_var_ending_in_underscore => {
                const suggested_name_text = name_text[0 .. name_text.len - 1]; // Remove the trailing underscore
                const suggested_ident = self.env.idents.insert(self.env.gpa, base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_ending_in_underscore = .{
                    .name = problem.ident,
                    .suggested_name = suggested_ident,
                    .region = Region.zero(),
                } });
            },
            .unused_type_var => {
                self.env.pushDiagnostic(Diagnostic{ .unused_type_var_name = .{
                    .name = problem.ident,
                    .suggested_name = problem.ident,
                    .region = Region.zero(),
                } });
            },
            .type_var_marked_unused => {
                const suggested_name_text = name_text[1..]; // Remove the underscore
                const suggested_ident = self.env.idents.insert(self.env.gpa, base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_marked_unused = .{
                    .name = problem.ident,
                    .suggested_name = suggested_ident,
                    .region = Region.zero(),
                } });
            },
        }
    }
}

/// Canonicalize a statement within a block
fn canonicalizeTypeAnno(self: *Self, anno_idx: AST.TypeAnno.Idx, can_intro_vars: CanIntroduceVars, in_type_declaration: bool) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    var found_underscore = false;
    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);
    switch (ast_anno) {
        .apply => |apply| {
            const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
            const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);

            // Validate we have arguments
            if (args_slice.len == 0) {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            }

            // Canonicalize the base type first
            const canonicalized_base = try self.canonicalizeTypeAnno(args_slice[0], can_intro_vars, in_type_declaration);
            const base_cir_type = self.env.store.getTypeAnno(canonicalized_base);

            // Check if base type is invalid
            if (in_type_declaration) {
                const base_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized_base)));
                const base_resolved = self.env.types.resolveVar(base_var);
                if (base_resolved.desc.content == .err) {
                    found_underscore = true;
                }
            }

            // Extract the symbol for the type application
            const type_symbol = switch (base_cir_type) {
                .ty => |ty| ty.symbol,
                .ty_lookup_external => |tle| {
                    // For external types, get the qualified name from the external declaration
                    const external_decl = self.env.getExternalDecl(tle.external_decl);

                    const scratch_top = self.env.store.scratchTypeAnnoTop();
                    defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

                    for (args_slice[1..]) |arg_idx| {
                        const canonicalized = try self.canonicalizeTypeAnno(arg_idx, can_intro_vars, in_type_declaration);
                        try self.env.store.addScratchTypeAnno(canonicalized);

                        // Check if this argument is invalid
                        if (in_type_declaration) {
                            const arg_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized)));
                            const arg_resolved = self.env.types.resolveVar(arg_var);
                            if (arg_resolved.desc.content == .err) {
                                found_underscore = true;
                            }
                        }
                    }

                    const args = try self.env.store.typeAnnoSpanFrom(scratch_top);
                    // Create type variable with error content if underscore in type declaration
                    const content = if (found_underscore and in_type_declaration)
                        types.Content{ .err = {} }
                    else
                        types.Content{ .flex_var = null };

                    return try self.env.addTypeAnnoAndTypeVar(.{ .apply = .{
                        .symbol = external_decl.qualified_name,
                        .args = args,
                    } }, content, region);
                },
                else => return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } }),
            };

            // Canonicalize type arguments (skip first which is the type name)
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (args_slice[1..]) |arg_idx| {
                const canonicalized = try self.canonicalizeTypeAnno(arg_idx, can_intro_vars, in_type_declaration);
                try self.env.store.addScratchTypeAnno(canonicalized);

                // Check if this argument is invalid
                if (in_type_declaration) {
                    const arg_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized)));
                    const arg_resolved = self.env.types.resolveVar(arg_var);
                    if (arg_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }
            }

            const args = try self.env.store.typeAnnoSpanFrom(scratch_top);

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .apply = .{
                .symbol = type_symbol,
                .args = args,
            } }, content, region);
        },
        .ty_var => |ty_var| {
            const region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = region,
                } });
            };

            // Check if this type variable is in scope
            const type_var_in_scope = self.scopeLookupTypeVar(name_ident);
            if (type_var_in_scope == null and can_intro_vars == .cannot_introduce_vars) {
                // Type variable not found in scope - issue diagnostic
                try self.env.pushDiagnostic(Diagnostic{ .undeclared_type_var = .{
                    .name = name_ident,
                    .region = region,
                } });
            }

            // Track this type variable for underscore validation
            try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                .name = name_ident,
            } }, content, region);
        },
        .underscore_type_var => |underscore_ty_var| {
            const region = self.parse_ir.tokenizedRegionToRegion(underscore_ty_var.region);

            // Underscore types aren't allowed in type declarations (aliases or nominal)
            if (in_type_declaration) {
                found_underscore = true;
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = region,
                } });
            }

            const name_ident = self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok) orelse {
                return self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = region,
                } });
            };

            // Check if this type variable is in scope
            const type_var_in_scope = self.scopeLookupTypeVar(name_ident);
            if (type_var_in_scope == null and can_intro_vars == .cannot_introduce_vars) {
                // Type variable not found in scope - issue diagnostic
                try self.env.pushDiagnostic(Diagnostic{ .undeclared_type_var = .{
                    .name = name_ident,
                    .region = region,
                } });
            }

            // Track this type variable for underscore validation
            try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                .name = name_ident,
            } }, content, region);
        },
        .ty => |ty| {
            const region = self.parse_ir.tokenizedRegionToRegion(ty.region);
            // Resolve the fully qualified type name
            const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};

            const type_name_text = self.parse_ir.resolveQualifiedName(ty.qualifiers, ty.token, &strip_tokens);

            // Check if this is a qualified type name (contains dots)
            if (std.mem.indexOf(u8, type_name_text, ".")) |_| {
                // This is a qualified type - create external declaration
                // Split the qualified name to get module and type parts
                const last_dot = std.mem.lastIndexOf(u8, type_name_text, ".") orelse unreachable;
                const module_text = type_name_text[0..last_dot];
                const local_type_text = type_name_text[last_dot + 1 ..];

                const module_name = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(module_text), Region.zero());
                const local_name = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(local_type_text), Region.zero());

                // Create qualified name identifier from the full type name
                const qualified_name_ident = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(type_name_text), region);

                // Create external declaration for the qualified type
                const external_type_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
                const external_decl = ExternalDecl{
                    .qualified_name = qualified_name_ident,
                    .module_name = module_name,
                    .local_name = local_name,
                    .type_var = external_type_var,
                    .kind = .type,
                    .region = region,
                };

                // TODO: Check that external decl is in scope
                // if (self.scopeLookupTypeDecl(external_decl.qualified_name) == null) {
                //     // Type not found in scope - issue diagnostic
                //     self.env.pushDiagnostic(Diagnostic{ .undeclared_type = .{
                //         .name = type_symbol,
                //         .region = region,
                //     } });
                // }

                const external_idx = try self.env.pushExternalDecl(external_decl);

                // Create type variable with error content if underscore in type declaration
                const content = if (found_underscore and in_type_declaration)
                    types.Content{ .err = {} }
                else
                    types.Content{ .flex_var = null };

                return try self.env.addTypeAnnoAndTypeVar(.{ .ty_lookup_external = .{
                    .external_decl = external_idx,
                } }, content, region);
            } else {
                // Unqualified type - check if this type is declared in scope
                const ident_idx = if (self.parse_ir.tokens.resolveIdentifier(ty.token)) |ident|
                    ident
                else
                    // Create identifier from text if resolution fails
                    try self.env.idents.insert(self.env.gpa, base.Ident.for_text(type_name_text), region);

                if (self.scopeLookupTypeDecl(ident_idx) == null) {
                    // Type not found in scope - issue diagnostic
                    try self.env.pushDiagnostic(Diagnostic{ .undeclared_type = .{
                        .name = ident_idx,
                        .region = region,
                    } });
                }

                // Create type variable with error content if underscore in type declaration
                const content = if (found_underscore and in_type_declaration)
                    types.Content{ .err = {} }
                else
                    types.Content{ .flex_var = null };

                return try self.env.addTypeAnnoAndTypeVar(.{ .ty = .{
                    .symbol = ident_idx,
                } }, content, region);
            }
        },
        .mod_ty => |mod_ty| {
            const region = self.parse_ir.tokenizedRegionToRegion(mod_ty.region);
            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .ty = .{
                .symbol = mod_ty.ty_ident,
            } }, content, region);
        },
        .underscore => |underscore| {
            const region = self.parse_ir.tokenizedRegionToRegion(underscore.region);

            // Underscore types aren't allowed in type declarations (aliases or nominal)
            if (in_type_declaration) {
                found_underscore = true;
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = region,
                } });
            }

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .underscore = {} }, content, region);
        },
        .tuple => |tuple| {
            const region = self.parse_ir.tokenizedRegionToRegion(tuple.region);
            // Canonicalize all tuple elements
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                const canonicalized = try self.canonicalizeTypeAnno(elem_idx, can_intro_vars, in_type_declaration);
                try self.env.store.addScratchTypeAnno(canonicalized);

                // Check if this element is invalid
                if (in_type_declaration) {
                    const elem_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized)));
                    const elem_resolved = self.env.types.resolveVar(elem_var);
                    if (elem_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }
            }

            const annos = try self.env.store.typeAnnoSpanFrom(scratch_top);
            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .tuple = .{
                .elems = annos,
            } }, content, region);
        },
        .record => |record| {
            const region = self.parse_ir.tokenizedRegionToRegion(record.region);

            // Canonicalize all record fields
            const scratch_top = self.env.store.scratchAnnoRecordFieldTop();
            defer self.env.store.clearScratchAnnoRecordFieldsFrom(scratch_top);

            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const ast_field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => {
                        // Skip malformed field entirely - it was already handled during parsing
                        continue;
                    },
                };

                // Resolve field name
                const field_name = self.parse_ir.tokens.resolveIdentifier(ast_field.name) orelse {
                    // Malformed field name - continue with placeholder
                    const malformed_field_ident = Ident.for_text("malformed_field");
                    const malformed_ident = try self.env.idents.insert(self.env.gpa, malformed_field_ident, Region.zero());
                    const canonicalized_ty = try self.canonicalizeTypeAnno(ast_field.ty, can_intro_vars, in_type_declaration);

                    const cir_field = TypeAnno.RecordField{
                        .name = malformed_ident,
                        .ty = canonicalized_ty,
                    };
                    const field_cir_idx = try self.env.addAnnoRecordFieldAndTypeVar(cir_field, .{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(ast_field.region));
                    try self.env.store.addScratchAnnoRecordField(field_cir_idx);
                    continue;
                };

                // Canonicalize field type
                const canonicalized_ty = try self.canonicalizeTypeAnno(ast_field.ty, can_intro_vars, in_type_declaration);

                // Check if this field type is invalid
                if (in_type_declaration) {
                    const field_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized_ty)));
                    const field_resolved = self.env.types.resolveVar(field_var);
                    if (field_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }

                // Create CIR field
                const cir_field = TypeAnno.RecordField{
                    .name = field_name,
                    .ty = canonicalized_ty,
                };
                const field_cir_idx = try self.env.addAnnoRecordFieldAndTypeVar(cir_field, .{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(ast_field.region));
                try self.env.store.addScratchAnnoRecordField(field_cir_idx);
            }

            const fields = try self.env.store.annoRecordFieldSpanFrom(scratch_top);
            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .record = .{
                .fields = fields,
            } }, content, region);
        },
        .tag_union => |tag_union| {
            const region = self.parse_ir.tokenizedRegionToRegion(tag_union.region);

            // Canonicalize all tags in the union using tag-specific canonicalization
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                const canonicalized = try self.canonicalizeTagVariant(tag_idx);
                try self.env.store.addScratchTypeAnno(canonicalized);

                // Check if this tag is invalid
                if (in_type_declaration) {
                    const tag_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized)));
                    const tag_resolved = self.env.types.resolveVar(tag_var);
                    if (tag_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }
            }

            const tags = try self.env.store.typeAnnoSpanFrom(scratch_top);

            // Handle optional open annotation (for extensible tag unions)
            const ext = if (tag_union.open_anno) |open_idx| blk: {
                const open_canonicalized = try self.canonicalizeTypeAnno(open_idx, can_intro_vars, in_type_declaration);

                // Check if the open annotation is invalid
                if (in_type_declaration) {
                    const open_var = @as(types.Var, @enumFromInt(@intFromEnum(open_canonicalized)));
                    const open_resolved = self.env.types.resolveVar(open_var);
                    if (open_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }

                break :blk open_canonicalized;
            } else null;

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .tag_union = .{
                .tags = tags,
                .ext = ext,
            } }, content, region);
        },
        .@"fn" => |fn_anno| {
            const region = self.parse_ir.tokenizedRegionToRegion(fn_anno.region);

            // Canonicalize argument types
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                const canonicalized = try self.canonicalizeTypeAnno(arg_idx, can_intro_vars, in_type_declaration);
                try self.env.store.addScratchTypeAnno(canonicalized);

                // Check if this argument is invalid
                if (in_type_declaration) {
                    const arg_var = @as(types.Var, @enumFromInt(@intFromEnum(canonicalized)));
                    const arg_resolved = self.env.types.resolveVar(arg_var);
                    if (arg_resolved.desc.content == .err) {
                        found_underscore = true;
                    }
                }
            }

            const args = try self.env.store.typeAnnoSpanFrom(scratch_top);

            // Canonicalize return type
            const ret = try self.canonicalizeTypeAnno(fn_anno.ret, can_intro_vars, in_type_declaration);

            // Check if the return type is invalid
            if (in_type_declaration) {
                const ret_var = @as(types.Var, @enumFromInt(@intFromEnum(ret)));
                const ret_resolved = self.env.types.resolveVar(ret_var);
                if (ret_resolved.desc.content == .err) {
                    found_underscore = true;
                }
            }

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .@"fn" = .{
                .args = args,
                .ret = ret,
                .effectful = fn_anno.effectful,
            } }, content, region);
        },
        .parens => |parens| {
            const region = self.parse_ir.tokenizedRegionToRegion(parens.region);
            const inner_anno = try self.canonicalizeTypeAnno(parens.anno, can_intro_vars, in_type_declaration);

            // Check if the inner annotation is invalid
            if (in_type_declaration) {
                const inner_var = @as(types.Var, @enumFromInt(@intFromEnum(inner_anno)));
                const inner_resolved = self.env.types.resolveVar(inner_var);
                if (inner_resolved.desc.content == .err) {
                    found_underscore = true;
                }
            }

            // Create type variable with error content if underscore in type declaration
            const content = if (found_underscore and in_type_declaration)
                types.Content{ .err = {} }
            else
                types.Content{ .flex_var = null };

            return try self.env.addTypeAnnoAndTypeVar(.{ .parens = .{
                .anno = inner_anno,
            } }, content, region);
        },
        .malformed => |malformed| {
            const region = self.parse_ir.tokenizedRegionToRegion(malformed.region);
            return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                .region = region,
            } });
        },
    }

    // Process any type variables collected during canonicalization
    try self.processCollectedTypeVars();
}

fn canonicalizeTypeHeader(self: *Self, header_idx: AST.TypeHeader.Idx, can_intro_vars: CanIntroduceVars) std.mem.Allocator.Error!TypeHeader.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if the node is malformed before calling getTypeHeader
    const node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    if (node.tag == .malformed) {
        // Create a malformed type header with an invalid identifier
        const region = self.parse_ir.tokenizedRegionToRegion(node.region);
        return try self.env.addTypeHeaderAndTypeVar(.{
            .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Invalid identifier
            .args = .{ .span = .{ .start = 0, .len = 0 } },
        }, Content{ .flex_var = null }, region);
    }

    const ast_header = self.parse_ir.store.getTypeHeader(header_idx);
    const region = self.parse_ir.tokenizedRegionToRegion(ast_header.region);

    // Get the type name identifier
    const name_ident = self.parse_ir.tokens.resolveIdentifier(ast_header.name) orelse {
        // If we can't resolve the identifier, create a malformed header with invalid identifier
        return try self.env.addTypeHeaderAndTypeVar(.{
            .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Invalid identifier
            .args = .{ .span = .{ .start = 0, .len = 0 } },
        }, Content{ .flex_var = null }, region);
    };

    // Canonicalize type arguments - these are parameter declarations, not references
    const scratch_top = self.env.store.scratchTypeAnnoTop();
    defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

    for (self.parse_ir.store.typeAnnoSlice(ast_header.args)) |arg_idx| {
        const ast_arg = self.parse_ir.store.getTypeAnno(arg_idx);
        // Type parameters should be treated as declarations, not lookups
        switch (ast_arg) {
            .ty_var => |ty_var| {
                const param_region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                const param_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                    const malformed = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                        .region = param_region,
                    } });
                    try self.env.store.addScratchTypeAnno(malformed);
                    continue;
                };

                // Create type variable annotation for this parameter
                // Check for underscore in type parameter
                const param_name = self.parse_ir.env.idents.getText(param_ident);
                if (param_name[0] == '_') {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                const param_anno = try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                    .name = param_ident,
                } }, Content{ .flex_var = null }, param_region);
                try self.env.store.addScratchTypeAnno(param_anno);
            },
            .underscore => |underscore_param| {
                // Handle underscore type parameters
                const param_region = self.parse_ir.tokenizedRegionToRegion(underscore_param.region);

                // Push underscore diagnostic for underscore type parameters
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = param_region,
                } });

                // Create underscore type annotation
                const underscore_anno = try self.env.addTypeAnnoAndTypeVar(.{ .underscore = {} }, Content{ .err = {} }, param_region);
                try self.env.store.addScratchTypeAnno(underscore_anno);
            },
            .malformed => |malformed_param| {
                // Handle malformed underscore type parameters
                const param_region = self.parse_ir.tokenizedRegionToRegion(malformed_param.region);

                // Push underscore diagnostic for malformed underscore type parameters
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = param_region,
                } });

                // Create malformed type annotation using pushMalformed for consistency
                const malformed_anno = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = param_region,
                } });
                try self.env.store.addScratchTypeAnno(malformed_anno);
            },
            else => {
                // Other types in parameter position - canonicalize normally but warn
                const canonicalized = try self.canonicalizeTypeAnno(arg_idx, can_intro_vars, false);
                try self.env.store.addScratchTypeAnno(canonicalized);
            },
        }
    }

    const args = try self.env.store.typeAnnoSpanFrom(scratch_top);

    return try self.env.addTypeHeaderAndTypeVar(.{
        .name = name_ident,
        .args = args,
    }, Content{ .flex_var = null }, region);
}

/// Canonicalize a statement in the canonical IR.
pub fn canonicalizeStatement(self: *Self, stmt_idx: AST.Statement.Idx) std.mem.Allocator.Error!?Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stmt = self.parse_ir.store.getStatement(stmt_idx);

    switch (stmt) {
        .decl => |d| {
            // Check if this is a var reassignment
            const pattern = self.parse_ir.store.getPattern(d.pattern);
            if (pattern == .ident) {
                const ident_tok = pattern.ident.ident_tok;
                if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                    const region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(d.pattern).to_tokenized_region());

                    // Check if this identifier exists and is a var
                    switch (self.scopeLookup(&self.env.idents, .ident, ident_idx)) {
                        .found => |existing_pattern_idx| {
                            // Check if this is a var reassignment across function boundaries
                            if (self.isVarReassignmentAcrossFunctionBoundary(existing_pattern_idx)) {
                                // Generate error for var reassignment across function boundary
                                const error_expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .var_across_function_boundary = .{
                                    .region = region,
                                } });

                                // Create a reassign statement with the error expression
                                const reassign_stmt = Statement{ .s_reassign = .{
                                    .pattern_idx = existing_pattern_idx,
                                    .expr = error_expr,
                                } };
                                const reassign_idx = try self.env.addStatementAndTypeVar(reassign_stmt, Content{ .flex_var = null }, region);
                                try self.env.store.addScratchStatement(reassign_idx);

                                return error_expr;
                            }

                            // Check if this was declared as a var
                            if (self.isVarPattern(existing_pattern_idx)) {
                                // This is a var reassignment - canonicalize the expression and create reassign statement
                                const expr_idx = try self.canonicalizeExpr(d.body) orelse return null;

                                // Create reassign statement
                                const reassign_stmt = Statement{ .s_reassign = .{
                                    .pattern_idx = existing_pattern_idx,
                                    .expr = expr_idx,
                                } };
                                const reassign_idx = try self.env.addStatementAndTypeVar(reassign_stmt, Content{ .flex_var = null }, region);
                                try self.env.store.addScratchStatement(reassign_idx);

                                return expr_idx;
                            }
                        },
                        .not_found => {
                            // Not found in scope, fall through to regular declaration
                        },
                    }
                }
            }

            // Regular declaration - canonicalize as usual
            const pattern_idx = try self.canonicalizePattern(d.pattern) orelse return null;
            const expr_idx = try self.canonicalizeExpr(d.body) orelse return null;

            // Create a declaration statement
            const var_stmt = Statement{ .s_decl = .{
                .pattern = pattern_idx,
                .expr = expr_idx,
            } };
            const region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getStatement(stmt_idx).decl.region);
            const var_stmt_idx = try self.env.addStatementAndTypeVar(var_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(var_stmt_idx);

            return expr_idx;
        },
        .@"var" => |v| {
            // Var declaration - handle specially with function boundary tracking
            const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse return null;
            const region = self.parse_ir.tokenizedRegionToRegion(v.region);

            // Canonicalize the initial value
            const init_expr_idx = try self.canonicalizeExpr(v.body) orelse return null;

            // Create pattern for the var
            const pattern_idx = try self.env.addPatternAndTypeVar(Pattern{ .assign = .{ .ident = var_name } }, Content{ .flex_var = null }, region);

            // Introduce the var with function boundary tracking
            _ = try self.scopeIntroduceVar(var_name, pattern_idx, region, true, Pattern.Idx);

            // Create var statement
            const var_stmt = Statement{ .s_var = .{
                .pattern_idx = pattern_idx,
                .expr = init_expr_idx,
            } };
            const var_idx = try self.env.addStatementAndTypeVar(var_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(var_idx);

            return init_expr_idx;
        },
        .expr => |e| {
            // Expression statement
            const expr_idx = try self.canonicalizeExpr(e.expr) orelse return null;

            // Create expression statement
            const expr_stmt = Statement{ .s_expr = .{
                .expr = expr_idx,
            } };
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const expr_stmt_idx = try self.env.addStatementAndTypeVar(expr_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(expr_stmt_idx);

            return expr_idx;
        },
        .crash => |c| {
            // Crash statement
            const region = self.parse_ir.tokenizedRegionToRegion(c.region);

            // Extract string content from the crash expression or create malformed if not string
            const msg_literal = blk: {
                const msg_expr = self.parse_ir.store.getExpr(c.expr);
                switch (msg_expr) {
                    .string => |s| {
                        // For string literals, we need to extract the actual string parts
                        const parts = self.parse_ir.store.exprSlice(s.parts);
                        if (parts.len > 0) {
                            const first_part = self.parse_ir.store.getExpr(parts[0]);
                            if (first_part == .string_part) {
                                const part_text = self.parse_ir.resolve(first_part.string_part.token);
                                break :blk try self.env.strings.insert(self.env.gpa, part_text);
                            }
                        }
                        // Fall back to default if we can't extract
                        break :blk try self.env.strings.insert(self.env.gpa, "crash");
                    },
                    else => {
                        // For non-string expressions, create a malformed expression
                        const malformed_expr = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .crash_expects_string = .{
                            .region = region,
                        } });
                        return malformed_expr;
                    },
                }
            };

            // Create crash statement
            const crash_stmt = Statement{ .s_crash = .{
                .msg = msg_literal,
            } };
            const crash_stmt_idx = try self.env.addStatementAndTypeVar(crash_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(crash_stmt_idx);

            // Create a crash expression that represents the runtime behavior
            const crash_expr = try self.env.addExprAndTypeVar(Expr{ .e_crash = .{
                .msg = msg_literal,
            } }, Content{ .flex_var = null }, region);
            return crash_expr;
        },
        .dbg => |d| {
            // Debug statement
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);

            // Canonicalize the debug expression
            const dbg_expr = try self.canonicalizeExpr(d.expr) orelse return null;

            // Create dbg statement
            const dbg_stmt = Statement{ .s_dbg = .{
                .expr = dbg_expr,
            } };
            const dbg_stmt_idx = try self.env.addStatementAndTypeVar(dbg_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(dbg_stmt_idx);

            // Return the debug expression value (dbg returns the value of its expression)
            return dbg_expr;
        },

        .expect => |e| {
            // Expect statement
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize the expect expression
            const expect_expr = try self.canonicalizeExpr(e.body) orelse return null;

            // Create expect statement
            const expect_stmt = Statement{ .s_expect = .{
                .body = expect_expr,
            } };
            const expect_stmt_idx = try self.env.addStatementAndTypeVar(expect_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(expect_stmt_idx);

            const expect_expr_node = try self.env.addExprAndTypeVar(Expr{ .e_expect = .{
                .body = expect_expr,
            } }, Content{ .flex_var = null }, region);
            return expect_expr_node;
        },
        .@"return" => |r| {
            // Return statement
            const region = self.parse_ir.tokenizedRegionToRegion(r.region);

            // Canonicalize the return expression
            const return_expr = try self.canonicalizeExpr(r.expr) orelse return null;

            // Create return statement
            const return_stmt = Statement{ .s_return = .{
                .expr = return_expr,
            } };
            const return_stmt_idx = try self.env.addStatementAndTypeVar(return_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(return_stmt_idx);

            // Return the return expression value
            return return_expr;
        },
        .type_decl => |s| {
            // TODO type declarations in statement context
            const feature = try self.env.strings.insert(self.env.gpa, "type_decl in statement context");
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = self.parse_ir.tokenizedRegionToRegion(s.region),
            } });
        },
        .type_anno => |ta| {
            // Type annotation statement
            const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

            // Resolve the identifier name
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                const feature = try self.env.strings.insert(self.env.gpa, "type annotation identifier resolution");
                return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
            };

            // Introduce type variables into scope
            const type_vars_top: u32 = @intCast(self.scratch_idents.top());

            // Extract type variables from the AST annotation
            try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);

            // Enter a new scope for type variables
            try self.scopeEnter(self.env.gpa, false);
            defer self.scopeExit(self.env.gpa) catch {};

            // Introduce type variables into scope (if we have any)
            if (self.scratch_idents.top() > type_vars_top) {
                for (self.scratch_idents.sliceFromStart(type_vars_top)) |type_var| {
                    // Get the proper region for this type variable from the AST
                    const type_var_anno = try self.env.addTypeAnnoAndTypeVar(
                        .{ .ty_var = .{ .name = type_var } },
                        .{ .flex_var = null },
                        self.getTypeVarRegionFromAST(ta.anno, type_var) orelse region,
                    );
                    try self.scopeIntroduceTypeVar(type_var, type_var_anno);
                }
                // Shrink the scratch vars list to the original size
                self.scratch_idents.clearFrom(type_vars_top);
            }

            // Now canonicalize the annotation with type variables in scope
            const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .cannot_introduce_vars, false);

            // Canonicalize where clauses if present
            const where_clauses = if (ta.where) |where_coll| blk: {
                const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                const where_start = self.env.store.scratchWhereClauseTop();

                for (where_slice) |where_idx| {
                    const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .cannot_introduce_vars);
                    try self.env.store.addScratchWhereClause(canonicalized_where);
                }

                break :blk try self.env.store.whereClauseSpanFrom(where_start);
            } else null;

            // Create a type annotation statement
            const type_anno_stmt = Statement{
                .s_type_anno = .{
                    .name = name_ident,
                    .anno = type_anno_idx,
                    .where = where_clauses,
                },
            };
            const type_anno_stmt_idx = try self.env.addStatementAndTypeVar(type_anno_stmt, Content{ .flex_var = null }, region);
            try self.env.store.addScratchStatement(type_anno_stmt_idx);

            // Type annotations don't produce runtime values, so return a unit expression
            // Create an empty tuple as a unit value
            const empty_span = Expr.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
            const unit_expr = try self.env.addExprAndTypeVar(Expr{ .e_tuple = .{
                .elems = empty_span,
            } }, Content{ .flex_var = null }, region);
            return unit_expr;
        },
        .import => |import_stmt| {
            _ = try self.canonicalizeImportStatement(import_stmt);

            // Import statements don't produce runtime values, so return a unit expression
            const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
            const empty_span = Expr.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
            const unit_expr = try self.env.addExprAndTypeVar(Expr{ .e_tuple = .{
                .elems = empty_span,
            } }, Content{ .flex_var = null }, region);
            return unit_expr;
        },
        else => {
            // Other statement types not yet implemented
            const feature = try self.env.strings.insert(self.env.gpa, "statement type in block");
            return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
        },
    }
}

/// Enter a new scope level
fn scopeEnter(self: *Self, gpa: std.mem.Allocator, is_function_boundary: bool) std.mem.Allocator.Error!void {
    const scope = Scope.init(is_function_boundary);
    try self.scopes.append(gpa, scope);
}

/// Exit the current scope level
fn scopeExit(self: *Self, gpa: std.mem.Allocator) Scope.Error!void {
    if (self.scopes.items.len <= 1) {
        return Scope.Error.ExitedTopScopeLevel;
    }

    // Check for unused variables in the scope we're about to exit
    const scope = &self.scopes.items[self.scopes.items.len - 1];
    try self.checkScopeForUnusedVariables(scope);

    var popped_scope: Scope = self.scopes.pop().?;
    popped_scope.deinit(gpa);
}

/// Get the current scope
fn currentScope(self: *Self) *Scope {
    std.debug.assert(self.scopes.items.len > 0);
    return &self.scopes.items[self.scopes.items.len - 1];
}

/// This will be used later for builtins like Num.nan, Num.infinity, etc.
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) Expr.Idx {
    // Dec doesn't have infinity, -infinity, or NaN
    const requirements = types.Num.Frac.Requirements{
        .fits_in_f32 = true,
        .fits_in_dec = false,
    };

    const frac_requirements = types.Num.FracRequirements{
        .fits_in_f32 = requirements.fits_in_f32,
        .fits_in_dec = requirements.fits_in_dec,
    };

    // then in the final slot the actual expr is inserted
    const expr_idx = try self.env.addExprAndTypeVar(Expr{
        .e_frac_f64 = .{
            .value = value,
            .region = region,
        },
    }, Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } }, region);

    return expr_idx;
}

/// Check if an identifier is in scope
fn scopeContains(
    self: *Self,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    name: base.Ident.Idx,
) ?Pattern.Idx {
    var scope_idx = self.scopes.items.len;
    while (scope_idx > 0) {
        scope_idx -= 1;
        const scope = &self.scopes.items[scope_idx];
        const map = scope.itemsConst(item_kind);

        var iter = map.iterator();
        while (iter.next()) |entry| {
            if (ident_store.identsHaveSameText(name, entry.key_ptr.*)) {
                return entry.value_ptr.*;
            }
        }
    }
    return null;
}

/// Look up an identifier in the scope
fn scopeLookup(
    self: *Self,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    name: base.Ident.Idx,
) Scope.LookupResult {
    if (self.scopeContains(ident_store, item_kind, name)) |pattern| {
        return Scope.LookupResult{ .found = pattern };
    }
    return Scope.LookupResult{ .not_found = {} };
}

/// Lookup a type variable in the scope hierarchy
fn scopeLookupTypeVar(self: *const Self, name_ident: Ident.Idx) ?TypeAnno.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupTypeVar(&self.env.idents, name_ident)) {
            .found => |type_var_idx| return type_var_idx,
            .not_found => continue,
        }
    }
    return null;
}

/// Introduce a type variable into the current scope
fn scopeIntroduceTypeVar(self: *Self, name: Ident.Idx, type_var_anno: TypeAnno.Idx) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Don't use parent lookup function for now - just introduce directly
    // Type variable shadowing is allowed in Roc
    const result = try current_scope.introduceTypeVar(gpa, &self.env.idents, name, type_var_anno, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_type_var_idx| {
            // Type variable shadowing is allowed but should produce warning
            const original_region = self.env.store.getTypeAnnoRegion(shadowed_type_var_idx);
            try self.env.pushDiagnostic(Diagnostic{ .shadowing_warning = .{
                .ident = name,
                .region = self.env.store.getTypeAnnoRegion(type_var_anno),
                .original_region = original_region,
            } });
        },
        .already_in_scope => |_| {
            // Type variable already exists in this scope - this is fine for repeated references
        },
    }
}

/// Extract type variables from a type annotation and introduce them into scope
fn extractTypeVarsFromAnno(self: *Self, type_anno_idx: TypeAnno.Idx) void {
    const type_anno = self.env.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .ty_var => |ty_var| {
            // This is a type variable - introduce it into scope
            self.scopeIntroduceTypeVar(ty_var.name, type_anno_idx);
        },
        .apply => |apply| {
            // Recursively extract from arguments
            for (self.env.store.sliceTypeAnnos(apply.args)) |arg_idx| {
                self.extractTypeVarsFromAnno(arg_idx);
            }
        },
        .@"fn" => |fn_anno| {
            // Extract type variables from function parameter types
            for (self.env.store.sliceTypeAnnos(fn_anno.args)) |param_idx| {
                self.extractTypeVarsFromAnno(param_idx);
            }
            // Extract type variables from return type
            self.extractTypeVarsFromAnno(fn_anno.ret);
        },
        .tuple => |tuple| {
            // Extract from tuple elements
            for (self.env.store.sliceTypeAnnos(tuple.annos)) |elem_idx| {
                self.extractTypeVarsFromAnno(elem_idx);
            }
        },
        .parens => |parens| {
            // Extract from inner annotation
            self.extractTypeVarsFromAnno(parens.anno);
        },
        .record => |record| {
            // Extract type variables from record field types
            for (self.env.store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                const field = self.env.store.getAnnoRecordField(field_idx);
                self.extractTypeVarsFromAnno(field.ty);
            }
        },
        .ty, .underscore, .mod_ty, .tag_union, .ty_lookup_external, .malformed => {
            // These don't contain type variables to extract
        },
    }
}

fn introduceTypeParametersFromHeader(self: *Self, header_idx: TypeHeader.Idx) std.mem.Allocator.Error!void {
    const header = self.env.store.getTypeHeader(header_idx);

    // Introduce each type parameter into the current scope
    for (self.env.store.sliceTypeAnnos(header.args)) |param_idx| {
        const param = self.env.store.getTypeAnno(param_idx);
        if (param == .ty_var) {
            try self.scopeIntroduceTypeVar(param.ty_var.name, param_idx);
        }
    }
}

// Recursively unwrap an annotation, getting all type var idents
fn extractTypeVarIdentsFromASTAnno(self: *Self, anno_idx: AST.TypeAnno.Idx, idents_start_idx: u32) std.mem.Allocator.Error!void {
    switch (self.parse_ir.store.getTypeAnno(anno_idx)) {
        .ty_var => |ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(ty_var.tok)) |ident| {
                // Check if we already have this type variable
                for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                    if (existing.idx == ident.idx) return; // Already added
                }
                _ = try self.scratch_idents.append(self.env.gpa, ident);
            }
        },
        .underscore_type_var => |underscore_ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                // Check if we already have this type variable
                for (self.scratch_idents.sliceFromStart(idents_start_idx)) |existing| {
                    if (existing.idx == ident.idx) return; // Already added
                }
                try self.scratch_idents.append(self.env.gpa, ident);
            }
        },
        .apply => |apply| {
            for (self.parse_ir.store.typeAnnoSlice(apply.args)) |arg_idx| {
                try self.extractTypeVarIdentsFromASTAnno(arg_idx, idents_start_idx);
            }
        },
        .@"fn" => |fn_anno| {
            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                try self.extractTypeVarIdentsFromASTAnno(arg_idx, idents_start_idx);
            }
            try self.extractTypeVarIdentsFromASTAnno(fn_anno.ret, idents_start_idx);
        },
        .tuple => |tuple| {
            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                try self.extractTypeVarIdentsFromASTAnno(elem_idx, idents_start_idx);
            }
        },
        .parens => |parens| {
            try self.extractTypeVarIdentsFromASTAnno(parens.anno, idents_start_idx);
        },
        .record => |record| {
            // Extract type variables from record field types
            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => continue,
                };
                try self.extractTypeVarIdentsFromASTAnno(field.ty, idents_start_idx);
            }
        },
        .ty, .underscore, .mod_ty, .tag_union, .malformed => {
            // These don't contain type variables to extract
        },
    }
}

/// Get the region of a specific type variable from an AST type annotation
fn getTypeVarRegionFromAST(self: *Self, anno_idx: AST.TypeAnno.Idx, target_ident: Ident.Idx) ?Region {
    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);

    switch (ast_anno) {
        .ty_var => |ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(ty_var.tok)) |ident| {
                if (ident.idx == target_ident.idx) {
                    return self.parse_ir.tokenizedRegionToRegion(ty_var.region);
                }
            }
            return null;
        },
        .underscore_type_var => |underscore_ty_var| {
            if (self.parse_ir.tokens.resolveIdentifier(underscore_ty_var.tok)) |ident| {
                if (ident.idx == target_ident.idx) {
                    return self.parse_ir.tokenizedRegionToRegion(underscore_ty_var.region);
                }
            }
            return null;
        },
        .apply => |apply| {
            for (self.parse_ir.store.typeAnnoSlice(apply.args)) |arg_idx| {
                if (self.getTypeVarRegionFromAST(arg_idx, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .@"fn" => |fn_anno| {
            for (self.parse_ir.store.typeAnnoSlice(fn_anno.args)) |arg_idx| {
                if (self.getTypeVarRegionFromAST(arg_idx, target_ident)) |region| {
                    return region;
                }
            }
            return self.getTypeVarRegionFromAST(fn_anno.ret, target_ident);
        },
        .tuple => |tuple| {
            for (self.parse_ir.store.typeAnnoSlice(tuple.annos)) |elem_idx| {
                if (self.getTypeVarRegionFromAST(elem_idx, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .parens => |parens| {
            return self.getTypeVarRegionFromAST(parens.anno, target_ident);
        },
        .record => |record| {
            for (self.parse_ir.store.annoRecordFieldSlice(record.fields)) |field_idx| {
                const field = self.parse_ir.store.getAnnoRecordField(field_idx) catch |err| switch (err) {
                    error.MalformedNode => continue,
                };
                if (self.getTypeVarRegionFromAST(field.ty, target_ident)) |region| {
                    return region;
                }
            }
            return null;
        },
        .tag_union => |tag_union| {
            for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
                if (self.getTypeVarRegionFromAST(tag_idx, target_ident)) |region| {
                    return region;
                }
            }
            if (tag_union.open_anno) |open_idx| {
                return self.getTypeVarRegionFromAST(open_idx, target_ident);
            }
            return null;
        },
        .ty, .underscore, .mod_ty, .malformed => {
            // These don't contain type variables
            return null;
        },
    }
}

/// Introduce a new identifier to the current scope level
fn scopeIntroduceInternal(
    self: *Self,
    gpa: std.mem.Allocator,
    ident_store: *const base.Ident.Store,
    comptime item_kind: Scope.ItemKind,
    ident_idx: base.Ident.Idx,
    pattern_idx: Pattern.Idx,
    is_var: bool,
    is_declaration: bool,
) std.mem.Allocator.Error!Scope.IntroduceResult {
    // Check if var is being used at top-level
    if (is_var and self.scopes.items.len == 1) {
        return Scope.IntroduceResult{ .top_level_var_error = {} };
    }

    // Check for existing identifier in any scope level for shadowing detection
    if (self.scopeContains(ident_store, item_kind, ident_idx)) |existing_pattern| {
        // If it's a var reassignment (not declaration), check function boundaries
        if (is_var and !is_declaration) {
            // Find the scope where the var was declared and check for function boundaries
            var declaration_scope_idx: ?usize = null;
            var scope_idx = self.scopes.items.len;

            // First, find where the identifier was declared
            while (scope_idx > 0) {
                scope_idx -= 1;
                const scope = &self.scopes.items[scope_idx];
                const map = scope.itemsConst(item_kind);

                var iter = map.iterator();
                while (iter.next()) |entry| {
                    if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
                        declaration_scope_idx = scope_idx;
                        break;
                    }
                }

                if (declaration_scope_idx != null) break;
            }

            // Now check if there are function boundaries between declaration and current scope
            if (declaration_scope_idx) |decl_idx| {
                var current_idx = decl_idx + 1;
                var found_function_boundary = false;

                while (current_idx < self.scopes.items.len) {
                    const scope = &self.scopes.items[current_idx];
                    if (scope.is_function_boundary) {
                        found_function_boundary = true;
                        break;
                    }
                    current_idx += 1;
                }

                if (found_function_boundary) {
                    // Different function, return error
                    return Scope.IntroduceResult{ .var_across_function_boundary = existing_pattern };
                } else {
                    // Same function, allow reassignment without warning
                    try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
                    return Scope.IntroduceResult{ .success = {} };
                }
            }

            // If we get here, the declaration scope was not found
            // This shouldn't happen in practice, but we need to handle it
            return Scope.IntroduceResult{ .success = {} };
        }

        // For non-var declarations, we should still report shadowing
        // Regular shadowing case - produce warning but still introduce
        try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
        return Scope.IntroduceResult{ .shadowing_warning = existing_pattern };
    }

    // Check the current level for duplicates
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    const map = current_scope.itemsConst(item_kind);

    var iter = map.iterator();
    while (iter.next()) |entry| {
        if (ident_store.identsHaveSameText(ident_idx, entry.key_ptr.*)) {
            // Duplicate in same scope - still introduce but return shadowing warning
            try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
            return Scope.IntroduceResult{ .shadowing_warning = entry.value_ptr.* };
        }
    }

    // No conflicts, introduce successfully
    try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
    return Scope.IntroduceResult{ .success = {} };
}

/// Get all identifiers in scope
/// TODO: Is this used? If so, we should update to use `self.scratch_idents`
fn scopeAllIdents(self: *const Self, gpa: std.mem.Allocator, comptime item_kind: Scope.ItemKind) std.mem.Allocator.Error![]base.Ident.Idx {
    var result = std.ArrayList(base.Ident.Idx).init(gpa);

    for (self.scopes.items) |scope| {
        const map = scope.itemsConst(item_kind);
        var iter = map.iterator();
        while (iter.next()) |entry| {
            try result.append(entry.key_ptr.*);
        }
    }

    return try result.toOwnedSlice();
}

/// Check if an identifier is marked as ignored (underscore prefix)
fn identIsIgnored(ident_idx: base.Ident.Idx) bool {
    return ident_idx.attributes.ignored;
}

/// Handle unused variable checking and diagnostics
fn checkUsedUnderscoreVariable(
    self: *Self,
    ident_idx: base.Ident.Idx,
    region: Region,
) std.mem.Allocator.Error!void {
    const is_ignored = identIsIgnored(ident_idx);

    if (is_ignored) {
        // Variable prefixed with _ but is actually used - warning
        try self.env.pushDiagnostic(Diagnostic{ .used_underscore_variable = .{
            .ident = ident_idx,
            .region = region,
        } });
    }
}

fn checkScopeForUnusedVariables(self: *Self, scope: *const Scope) std.mem.Allocator.Error!void {
    // Iterate through all identifiers in this scope
    var iterator = scope.idents.iterator();
    while (iterator.next()) |entry| {
        const ident_idx = entry.key_ptr.*;
        const pattern_idx = entry.value_ptr.*;

        // Skip if this variable was used
        if (self.used_patterns.contains(pattern_idx)) {
            continue;
        }

        // Skip if this is an ignored variable (starts with _)
        if (identIsIgnored(ident_idx)) {
            continue;
        }

        // Get the region for this pattern to provide good error location
        const region = self.env.store.getPatternRegion(pattern_idx);

        // Report unused variable
        try self.env.pushDiagnostic(Diagnostic{ .unused_variable = .{
            .ident = ident_idx,
            .region = region,
        } });
    }
}

/// Introduce a type declaration into the current scope
fn scopeIntroduceTypeDecl(
    self: *Self,
    name_ident: Ident.Idx,
    type_decl_stmt: Statement.Idx,
    region: Region,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Check for shadowing in parent scopes
    var shadowed_in_parent: ?Statement.Idx = null;
    if (self.scopes.items.len > 1) {
        var i = self.scopes.items.len - 1;
        while (i > 0) {
            i -= 1;
            const scope = &self.scopes.items[i];
            switch (scope.lookupTypeDecl(&self.env.idents, name_ident)) {
                .found => |type_decl_idx| {
                    shadowed_in_parent = type_decl_idx;
                    break;
                },
                .not_found => continue,
            }
        }
    }

    const result = try current_scope.introduceTypeDecl(gpa, &self.env.idents, name_ident, type_decl_stmt, null);

    switch (result) {
        .success => {
            // Check if we're shadowing a type in a parent scope
            if (shadowed_in_parent) |shadowed_stmt| {
                const original_region = self.env.store.getStatementRegion(shadowed_stmt);
                try self.env.pushDiagnostic(Diagnostic{
                    .shadowing_warning = .{
                        .ident = name_ident,
                        .region = region,
                        .original_region = original_region,
                    },
                });
            }
        },
        .shadowing_warning => |shadowed_stmt| {
            // This shouldn't happen since we're not passing a parent lookup function
            // but handle it just in case the Scope implementation changes
            const original_region = self.env.store.getStatementRegion(shadowed_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .shadowing_warning = .{
                    .ident = name_ident,
                    .region = region,
                    .original_region = original_region,
                },
            });
        },
        .redeclared_error => |original_stmt| {
            // Extract region information from the original statement
            const original_region = self.env.store.getStatementRegion(original_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .type_redeclared = .{
                    .original_region = original_region,
                    .redeclared_region = region,
                    .name = name_ident,
                },
            });
        },
        .type_alias_redeclared => |original_stmt| {
            const original_region = self.env.store.getStatementRegion(original_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .type_alias_redeclared = .{
                    .name = name_ident,
                    .original_region = original_region,
                    .redeclared_region = region,
                },
            });
        },
        .nominal_type_redeclared => |original_stmt| {
            const original_region = self.env.store.getStatementRegion(original_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .nominal_type_redeclared = .{
                    .name = name_ident,
                    .original_region = original_region,
                    .redeclared_region = region,
                },
            });
        },
        .cross_scope_shadowing => |shadowed_stmt| {
            const original_region = self.env.store.getStatementRegion(shadowed_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .type_shadowed_warning = .{
                    .name = name_ident,
                    .region = region,
                    .original_region = original_region,
                    .cross_scope = true,
                },
            });
        },
        .parameter_conflict => |conflict| {
            const original_region = self.env.store.getStatementRegion(conflict.original_stmt);
            try self.env.pushDiagnostic(Diagnostic{
                .type_parameter_conflict = .{
                    .name = name_ident,
                    .parameter_name = conflict.conflicting_parameter,
                    .region = region,
                    .original_region = original_region,
                },
            });
        },
    }
}

fn scopeUpdateTypeDecl(
    self: *Self,
    name_ident: Ident.Idx,
    new_type_decl_stmt: Statement.Idx,
) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.updateTypeDecl(gpa, &self.env.idents, name_ident, new_type_decl_stmt);
}

fn scopeLookupTypeDecl(self: *Self, ident_idx: Ident.Idx) ?Statement.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupTypeDecl(&self.env.idents, ident_idx)) {
            .found => |type_decl_idx| return type_decl_idx,
            .not_found => continue,
        }
    }

    return null;
}

/// Look up a module alias in the scope hierarchy
fn scopeLookupModule(self: *const Self, alias_name: Ident.Idx) ?Ident.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(&self.env.idents, alias_name)) {
            .found => |module_name| return module_name,
            .not_found => continue,
        }
    }

    return null;
}

/// Introduce a module alias into scope
fn scopeIntroduceModuleAlias(self: *Self, alias_name: Ident.Idx, module_name: Ident.Idx) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Simplified introduction without parent lookup for now
    const result = try current_scope.introduceModuleAlias(gpa, &self.env.idents, alias_name, module_name, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_module| {
            // Create diagnostic for module alias shadowing
            try self.env.pushDiagnostic(Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = Region.zero(), // TODO: get proper region
                    .original_region = Region.zero(), // TODO: get proper region
                },
            });
            _ = shadowed_module; // Suppress unused variable warning
        },
        .already_in_scope => |existing_module| {
            // Module alias already exists in current scope
            // For now, just issue a diagnostic
            try self.env.pushDiagnostic(Diagnostic{
                .shadowing_warning = .{
                    .ident = alias_name,
                    .region = Region.zero(), // TODO: get proper region
                    .original_region = Region.zero(), // TODO: get proper region
                },
            });
            _ = existing_module; // Suppress unused variable warning
        },
    }
}

/// Helper function to look up module aliases in parent scopes only
fn scopeLookupModuleInParentScopes(self: *const Self, alias_name: Ident.Idx) ?Ident.Idx {
    // Search from second-innermost to outermost scope (excluding current scope)
    if (self.scopes.items.len <= 1) return null;

    var i = self.scopes.items.len - 1;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupModuleAlias(&self.env.idents, alias_name)) {
            .found => |module_name| return module_name,
            .not_found => continue,
        }
    }

    return null;
}

/// Look up an exposed item across all scopes
fn scopeLookupExposedItem(self: *const Self, item_name: Ident.Idx) ?Scope.ExposedItemInfo {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupExposedItem(&self.env.idents, item_name)) {
            .found => |item_info| return item_info,
            .not_found => continue,
        }
    }

    return null;
}

/// Introduce an exposed item into the current scope
fn scopeIntroduceExposedItem(self: *Self, item_name: Ident.Idx, item_info: Scope.ExposedItemInfo) std.mem.Allocator.Error!void {
    const gpa = self.env.gpa;
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];

    // Simplified introduction without parent lookup for now
    const result = try current_scope.introduceExposedItem(gpa, &self.env.idents, item_name, item_info, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_info| {
            // Create diagnostic for exposed item shadowing
            const item_text = self.env.idents.getText(item_name);
            const shadowed_module_text = self.env.idents.getText(shadowed_info.module_name);
            const current_module_text = self.env.idents.getText(item_info.module_name);

            // For now, just add a simple diagnostic message
            const message = try std.fmt.allocPrint(gpa, "Exposed item '{s}' from module '{s}' shadows item from module '{s}'", .{ item_text, current_module_text, shadowed_module_text });
            const message_str = try self.env.strings.insert(gpa, message);
            gpa.free(message);

            try self.env.pushDiagnostic(Diagnostic{
                .not_implemented = .{
                    .feature = message_str,
                    .region = Region.zero(), // TODO: Get proper region from import statement
                },
            });
        },
        .already_in_scope => |existing_info| {
            // Create diagnostic for duplicate exposed item
            const item_text = self.env.idents.getText(item_name);
            const existing_module_text = self.env.idents.getText(existing_info.module_name);
            const new_module_text = self.env.idents.getText(item_info.module_name);

            const message = try std.fmt.allocPrint(gpa, "Exposed item '{s}' already imported from module '{s}', cannot import again from module '{s}'", .{ item_text, existing_module_text, new_module_text });
            const message_str = try self.env.strings.insert(gpa, message);
            gpa.free(message);

            try self.env.pushDiagnostic(Diagnostic{
                .not_implemented = .{
                    .feature = message_str,
                    .region = Region.zero(), // TODO: Get proper region from import statement
                },
            });
        },
    }
}

/// Look up an exposed item in parent scopes (for shadowing detection)
fn scopeLookupExposedItemInParentScopes(self: *const Self, item_name: Ident.Idx) ?Scope.ExposedItemInfo {
    // Search from second-innermost to outermost scope (excluding current scope)
    if (self.scopes.items.len <= 1) return null;

    var i = self.scopes.items.len - 1;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupExposedItem(&self.env.idents, item_name)) {
            .found => |item_info| return item_info,
            .not_found => continue,
        }
    }

    return null;
}

/// Look up an imported module in the scope hierarchy
fn scopeLookupImportedModule(self: *const Self, module_name: []const u8) ?Import.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupImportedModule(module_name)) {
            .found => |import_idx| return import_idx,
            .not_found => continue,
        }
    }

    return null;
}

/// Extract the module name from a full qualified name (e.g., "Json" from "json.Json")
fn extractModuleName(self: *Self, module_name_ident: Ident.Idx) std.mem.Allocator.Error!Ident.Idx {
    const module_text = self.env.idents.getText(module_name_ident);

    // Find the last dot and extract the part after it
    if (std.mem.lastIndexOf(u8, module_text, ".")) |last_dot_idx| {
        const extracted_name = module_text[last_dot_idx + 1 ..];
        return try self.env.idents.insert(self.env.gpa, base.Ident.for_text(extracted_name), Region.zero());
    } else {
        // No dot found, return the original name
        return module_name_ident;
    }
}

/// Convert a parsed TypeAnno into a canonical TypeVar with appropriate Content
fn canonicalizeTypeAnnoToTypeVar(self: *Self, type_anno_idx: TypeAnno.Idx) std.mem.Allocator.Error!TypeVar {
    const type_anno_node_idx = nodeIdxFrom(type_anno_idx);
    const type_anno = self.env.store.getTypeAnno(type_anno_idx);
    const region = self.env.store.getTypeAnnoRegion(type_anno_idx);

    switch (type_anno) {
        .ty_var => |tv| {
            // Check if this type variable is already in scope
            const scope = self.currentScope();
            const ident_store = &self.env.idents;

            switch (scope.lookupTypeVar(ident_store, tv.name)) {
                .found => |_| {
                    // Type variable already exists, create fresh var with same name
                    return try self.env.addTypeSlotAndTypeVar(
                        type_anno_node_idx,
                        .{ .flex_var = tv.name },
                        region,
                        TypeVar,
                    );
                },
                .not_found => {
                    // Create a basic type annotation for the scope
                    const ty_var_anno = try self.env.addTypeAnnoAndTypeVar(
                        .{ .ty_var = .{ .name = tv.name } },
                        .{ .flex_var = tv.name },
                        region,
                    );

                    // Add to scope (simplified - ignoring result for now)
                    // TODO: Handle scope result and possible error
                    _ = try scope.introduceTypeVar(self.env.gpa, ident_store, tv.name, ty_var_anno, null);

                    return castIdx(TypeAnno.Idx, TypeVar, ty_var_anno);
                },
            }
        },
        .underscore => {
            // Check if this underscore type annotation has error content
            const type_var = @as(types.Var, @enumFromInt(@intFromEnum(type_anno_idx)));
            const resolved = self.env.types.resolveVar(type_var);
            if (resolved.desc.content == .err) {
                // This underscore was in a type declaration - create error type variable
                return try self.env.addTypeSlotAndTypeVar(type_anno_node_idx, .err, region, TypeVar);
            } else {
                // Create anonymous flex var
                return try self.env.addTypeSlotAndTypeVar(type_anno_node_idx, .{ .flex_var = null }, region, TypeVar);
            }
        },
        .ty => |t| {
            // Look up built-in or user-defined type
            return try self.canonicalizeBasicType(t.symbol, type_anno_node_idx, region);
        },
        .apply => |apply| {
            // Handle type application like List(String), Dict(a, b)
            return try self.canonicalizeTypeApplication(apply, type_anno_node_idx, region);
        },
        .@"fn" => |func| {
            // Create function type
            return try self.canonicalizeFunctionType(func, type_anno_node_idx, region);
        },
        .tuple => |tuple| {
            if (tuple.elems.span.len == 1) {
                // Single element tuples are just parenthized exprs
                const type_annos = self.env.store.sliceTypeAnnos(tuple.elems);
                return try self.canonicalizeTypeAnnoToTypeVar(type_annos[0]);
            } else {
                // Create tuple type
                return self.canonicalizeTupleType(tuple, type_anno_node_idx, region);
            }
        },
        .record => |record| {
            // Create record type
            return try self.canonicalizeRecordType(record, type_anno_node_idx, region);
        },
        .tag_union => |tag_union| {
            // Create tag union type
            return try self.canonicalizeTagUnionType(tag_union, type_anno_node_idx, region);
        },
        .parens => |parens| {
            // Recursively canonicalize the inner type
            return try self.canonicalizeTypeAnnoToTypeVar(parens.anno);
        },

        .ty_lookup_external => |tle| {
            // For external type lookups, create a flexible type variable
            // This will be resolved later when dependencies are available
            const external_decl = self.env.getExternalDecl(tle.external_decl);
            return try self.env.addTypeSlotAndTypeVar(
                type_anno_node_idx,
                .{ .flex_var = external_decl.qualified_name },
                region,
                TypeVar,
            );
        },
        .malformed => {
            // Return error type for malformed annotations
            return try self.env.addTypeSlotAndTypeVar(
                type_anno_node_idx,
                .err,
                region,
                TypeVar,
            );
        },
    }
}

/// Canonicalize a where clause from AST to CIR
fn canonicalizeWhereClause(self: *Self, ast_where_idx: AST.WhereClause.Idx, can_intro_vars: CanIntroduceVars) std.mem.Allocator.Error!WhereClause.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const ast_where = self.parse_ir.store.getWhereClause(ast_where_idx);

    switch (ast_where) {
        .mod_method => |mm| {
            const region = self.parse_ir.tokenizedRegionToRegion(mm.region);

            // Resolve type variable name
            const var_name = self.parse_ir.resolve(mm.var_tok);

            // Resolve method name (remove leading dot)
            const method_name_text = self.parse_ir.resolve(mm.name_tok);

            // Remove leading dot from method name
            const method_name_clean = if (method_name_text.len > 0 and method_name_text[0] == '.')
                method_name_text[1..]
            else
                method_name_text;

            // Intern the variable and method names
            const var_ident = try self.env.idents.insert(self.env.gpa, Ident.for_text(var_name), region);
            const method_ident = try self.env.idents.insert(self.env.gpa, Ident.for_text(method_name_clean), region);

            // Canonicalize argument types
            const args_slice = self.parse_ir.store.typeAnnoSlice(.{ .span = self.parse_ir.store.getCollection(mm.args).span });
            const args_start = self.env.store.scratchTypeAnnoTop();
            for (args_slice) |arg_idx| {
                const canonicalized_arg = try self.canonicalizeTypeAnno(arg_idx, can_intro_vars, false);
                try self.env.store.addScratchTypeAnno(canonicalized_arg);
            }
            const args_span = try self.env.store.typeAnnoSpanFrom(args_start);

            // Canonicalize return type
            const ret_anno = try self.canonicalizeTypeAnno(mm.ret_anno, can_intro_vars, false);

            // Create external declaration for where clause method constraint
            // This represents the requirement that type variable must come from a module
            // that provides the specified method
            const var_name_text = self.env.idents.getText(var_ident);

            // Create qualified name: "module(a).method"
            const qualified_text = try std.fmt.allocPrint(self.env.gpa, "module({s}).{s}", .{ var_name_text, method_name_clean });
            defer self.env.gpa.free(qualified_text);
            const qualified_name = try self.env.idents.insert(self.env.gpa, Ident.for_text(qualified_text), region);

            // Create module name: "module(a)"
            const module_text = try std.fmt.allocPrint(self.env.gpa, "module({s})", .{var_name_text});
            defer self.env.gpa.free(module_text);
            const module_name = try self.env.idents.insert(self.env.gpa, Ident.for_text(module_text), region);

            const external_decl = try self.createExternalDeclaration(qualified_name, module_name, method_ident, .value, region);

            return try self.env.addWhereClauseAndTypeVar(WhereClause{ .mod_method = .{
                .var_name = var_ident,
                .method_name = method_ident,
                .args = args_span,
                .ret_anno = ret_anno,
                .external_decl = external_decl,
            } }, .{ .flex_var = null }, region);
        },
        .mod_alias => |ma| {
            const region = self.parse_ir.tokenizedRegionToRegion(ma.region);

            // Resolve type variable name
            const var_name = self.parse_ir.resolve(ma.var_tok);

            // Resolve alias name (remove leading dot)
            const alias_name_text = self.parse_ir.resolve(ma.name_tok);

            // Remove leading dot from alias name
            const alias_name_clean = if (alias_name_text.len > 0 and alias_name_text[0] == '.')
                alias_name_text[1..]
            else
                alias_name_text;

            // Intern the variable and alias names
            const var_ident = try self.env.idents.insert(self.env.gpa, Ident.for_text(var_name), region);
            const alias_ident = try self.env.idents.insert(self.env.gpa, Ident.for_text(alias_name_clean), region);

            // Create external declaration for where clause alias constraint
            // This represents the requirement that type variable must come from a module
            // that provides the specified type alias
            const var_name_text = self.env.idents.getText(var_ident);

            // Create qualified name: "module(a).Alias"
            const qualified_text = try std.fmt.allocPrint(self.env.gpa, "module({s}).{s}", .{ var_name_text, alias_name_clean });
            defer self.env.gpa.free(qualified_text);
            const qualified_name = try self.env.idents.insert(self.env.gpa, Ident.for_text(qualified_text), region);

            // Create module name: "module(a)"
            const module_text = try std.fmt.allocPrint(self.env.gpa, "module({s})", .{var_name_text});
            defer self.env.gpa.free(module_text);
            const module_name = try self.env.idents.insert(self.env.gpa, Ident.for_text(module_text), region);

            const external_decl = try self.createExternalDeclaration(qualified_name, module_name, alias_ident, .type, region);

            return try self.env.addWhereClauseAndTypeVar(WhereClause{ .mod_alias = .{
                .var_name = var_ident,
                .alias_name = alias_ident,
                .external_decl = external_decl,
            } }, .{ .flex_var = null }, region);
        },
        .malformed => |m| {
            const region = self.parse_ir.tokenizedRegionToRegion(m.region);
            const diagnostic = try self.env.addDiagnosticAndTypeVar(Diagnostic{ .malformed_where_clause = .{
                .region = region,
            } }, .{ .flex_var = null });
            return try self.env.addWhereClauseAndTypeVar(WhereClause{ .malformed = .{
                .diagnostic = diagnostic,
            } }, .{ .flex_var = null }, region);
        },
    }
}

/// Handle basic type lookup (Bool, Str, Num, etc.)
fn canonicalizeBasicType(self: *Self, symbol: Ident.Idx, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (self.scopeLookupTypeDecl(symbol)) |decl_idx| {
        const decl = self.env.store.getStatement(decl_idx);
        switch (decl) {
            .s_alias_decl => |_| {
                return try self.env.addTypeSlotAndTypeVarRedirect(parent_node_idx, varFrom(decl_idx), region, TypeVar);
            },
            .s_nominal_decl => |_| {
                return try self.env.addTypeSlotAndTypeVarRedirect(parent_node_idx, varFrom(decl_idx), region, TypeVar);
            },
            else => {
                // TODO: Add malformed node?
                // Unknown type - create error type
                return try self.env.addTypeSlotAndTypeVar(
                    parent_node_idx,
                    .err,
                    region,
                    TypeVar,
                );
            },
        }
    } else {
        // TODO: Add malformed node?
        // Unknown type - create error type
        return try self.env.addTypeSlotAndTypeVar(
            parent_node_idx,
            .err,
            region,
            TypeVar,
        );
    }
}

/// Handle type applications like List(Str), Dict(k, v)
fn canonicalizeTypeApplication(self: *Self, apply: TypeAnno.Apply, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Look up the type name in scope
    if (self.scopeLookupTypeDecl(apply.symbol)) |type_decl_idx| {
        const actual_args = self.env.store.sliceTypeAnnos(apply.args);

        const type_decl = self.env.store.getStatement(type_decl_idx);
        switch (type_decl) {
            .s_alias_decl => |d| {
                const header = self.env.store.getTypeHeader(d.header);
                const expected_args = self.env.store.sliceTypeAnnos(header.args).len;

                // Check arity
                if (expected_args != actual_args.len) {
                    return try self.env.addTypeSlotAndTypeVar(
                        parent_node_idx,
                        .err,
                        region,
                        TypeVar,
                    );
                }

                const scratch_anno_start = self.scratch_vars.top();
                for (actual_args) |arg_idx| {
                    const arg_anno_var = try self.canonicalizeTypeAnnoToTypeVar(arg_idx);
                    try self.scratch_vars.append(self.env.gpa, arg_anno_var);
                }
                const arg_anno_slice = self.scratch_vars.slice(scratch_anno_start, self.scratch_vars.top());

                // Now set the alias content on the main variable
                const alias = try self.env.types.mkAlias(.{ .ident_idx = apply.symbol }, d.anno_var, arg_anno_slice);
                const alias_var = try self.env.addTypeSlotAndTypeVar(
                    parent_node_idx,
                    alias,
                    region,
                    TypeVar,
                );

                // Ensure slots exist for the alias and all its arguments
                // Shrink the scratch var buffer now that our work is done
                self.scratch_vars.clearFrom(scratch_anno_start);

                return alias_var;
            },
            .s_nominal_decl => |d| {
                const header = self.env.store.getTypeHeader(d.header);
                const args = self.env.store.sliceTypeAnnos(header.args);

                // Check arity
                if (args.len != actual_args.len) {
                    return try self.env.addTypeSlotAndTypeVar(
                        parent_node_idx,
                        .err,
                        region,
                        TypeVar,
                    );
                }

                const scratch_anno_start = self.scratch_vars.top();
                for (actual_args) |arg_idx| {
                    const arg_anno_var = try self.canonicalizeTypeAnnoToTypeVar(arg_idx);
                    try self.scratch_vars.append(self.env.gpa, arg_anno_var);
                }
                const arg_anno_slice = self.scratch_vars.slice(scratch_anno_start, self.scratch_vars.top());

                const module_ident = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(self.env.module_name), Region.zero());
                const nominal = try self.env.types.mkNominal(types.TypeIdent{ .ident_idx = header.name }, d.anno_var, arg_anno_slice, module_ident);
                const nominal_var = try self.env.addTypeSlotAndTypeVar(
                    parent_node_idx,
                    nominal,
                    region,
                    TypeVar,
                );

                // Shrink the scratch var buffer now that our work is done
                self.scratch_vars.clearFrom(scratch_anno_start);

                return nominal_var;
            },
            else => @panic("scopeLookupTypeDecl only returns type declarations"),
        }
    } else {
        // TODO: This should be impossible
        return try self.env.addTypeSlotAndTypeVar(
            parent_node_idx,
            .err,
            region,
            TypeVar,
        );
    }
}

/// Handle function types like a -> b
fn canonicalizeFunctionType(self: *Self, func: TypeAnno.Func, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Canonicalize argument types and return type
    const args_slice = self.env.store.sliceTypeAnnos(func.args);

    // Collect canonicalized argument type variables

    // For each argument, canonicalize its type and collect the type var
    const arg_vars_top = self.scratch_vars.top();
    for (args_slice) |arg_anno_idx| {
        const arg_type_var = try self.canonicalizeTypeAnnoToTypeVar(arg_anno_idx);
        try self.scratch_vars.append(self.env.gpa, arg_type_var);
    }
    const arg_vars_end = self.scratch_vars.top();

    // Canonicalize return type
    const ret_type_var = try self.canonicalizeTypeAnnoToTypeVar(func.ret);

    // Create the appropriate function type based on effectfulness
    const arg_vars = self.scratch_vars.slice(arg_vars_top, arg_vars_end);
    const func_content = if (func.effectful)
        try self.env.types.mkFuncEffectful(arg_vars, ret_type_var)
    else
        try self.env.types.mkFuncPure(arg_vars, ret_type_var);

    // Shink the scratch array to it's original size
    self.scratch_vars.clearFrom(arg_vars_top);

    // Create and return the function type variable
    return try self.env.addTypeSlotAndTypeVar(
        parent_node_idx,
        func_content,
        region,
        TypeVar,
    );
}

/// Handle tuple types like (a, b, c)
fn canonicalizeTupleType(self: *Self, tuple: TypeAnno.Tuple, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Canonicalized each tuple element
    const scratch_elems_start = self.scratch_vars.top();
    for (self.env.store.sliceTypeAnnos(tuple.elems)) |tuple_elem_anno_idx| {
        const elem_var = try self.canonicalizeTypeAnnoToTypeVar(tuple_elem_anno_idx);
        _ = try self.scratch_vars.append(self.env.gpa, elem_var);
    }
    const elem_vars_range = try self.env.types.appendVars(
        self.scratch_vars.items.items[scratch_elems_start..],
    );

    // Shink the scratch array to it's original size
    self.scratch_vars.clearFrom(scratch_elems_start);

    const tuple_content = Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
    return try self.env.addTypeSlotAndTypeVar(
        parent_node_idx,
        tuple_content,
        region,
        TypeVar,
    );
}

/// Handle record types like { name: Str, age: Num }
fn canonicalizeRecordType(self: *Self, record: TypeAnno.Record, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create fresh type variables for each field
    const record_fields_top = self.scratch_record_fields.top();

    // Process each field in the record type annotation
    for (self.env.store.sliceAnnoRecordFields(record.fields)) |field_idx| {
        const field = self.env.store.getAnnoRecordField(field_idx);

        // Canonicalize the field's type annotation
        const field_type_var = try self.canonicalizeTypeAnnoToTypeVar(field.ty);

        try self.scratch_record_fields.append(self.env.gpa, types.RecordField{
            .name = field.name,
            .var_ = field_type_var,
        });
    }

    // Create the record type structure
    const type_fields_range = try self.env.types.appendRecordFields(self.scratch_record_fields.items.items[record_fields_top..]);
    const ext_var = try self.env.addTypeSlotAndTypeVar(
        parent_node_idx,
        .{ .structure = .empty_record },
        region,
        TypeVar,
    );

    // Shink the scratch array to it's original size
    self.scratch_record_fields.clearFrom(record_fields_top);

    const record_content = Content{ .structure = .{ .record = .{ .fields = type_fields_range, .ext = ext_var } } };
    return try self.env.addTypeSlotAndTypeVar(
        parent_node_idx,
        record_content,
        region,
        TypeVar,
    );
}

/// Handle tag union types like [Some(a), None]
fn canonicalizeTagUnionType(self: *Self, tag_union: TypeAnno.TagUnion, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!TypeVar {
    const trace = tracy.trace(@src());
    defer trace.end();

    // TODO: When

    // Iterate over the tags, adding them to the types store
    const tags_slice = self.env.store.sliceTypeAnnos(tag_union.tags);
    const tags_range = blk: {
        if (tags_slice.len == 0) {
            break :blk types.Tag.SafeMultiList.Range.empty();
        }

        // If we got here, then the following should be true:
        // 1. Each tag type should be either `ty` or `apply`
        // 2. Each `apply` should have at least 1 argument
        // If these are not true, it is a bug in `canonicalizeTagVariant`

        // First, add tags to the scratch buffer
        const tags_start = self.scratch_tags.top();
        for (tags_slice) |tag_anno_idx| {
            const tag_anno = self.env.store.getTypeAnno(tag_anno_idx);
            switch (tag_anno) {
                .ty => |ty| {
                    // For tags without args, just add them
                    _ = try self.scratch_tags.append(self.env.gpa, types.Tag{
                        .name = ty.symbol,
                        .args = TypeVar.SafeList.Range.empty(),
                    });
                },
                .apply => |apply| {
                    if (apply.args.span.len == 0) {
                        // This should be impossible. See above comment for details
                        // TODO: Is there a better way to do this?
                        std.debug.assert(false);
                    }

                    // Get a type var for every argument
                    const args_start = self.scratch_vars.top();
                    for (self.env.store.sliceTypeAnnos(apply.args)) |arg_anno_idx| {
                        const arg_var = try self.canonicalizeTypeAnnoToTypeVar(arg_anno_idx);
                        try self.scratch_vars.append(self.env.gpa, arg_var);
                    }
                    const args_end = self.scratch_vars.top();

                    // Append the tag args to the types store
                    const args_range = try self.env.types.appendVars(
                        self.scratch_vars.slice(args_start, args_end),
                    );

                    // Shrink the scratch var buffer now that our work is done
                    self.scratch_vars.clearFrom(args_start);

                    // Append the tag to scratch tags
                    _ = try self.scratch_tags.append(self.env.gpa, types.Tag{
                        .name = apply.symbol,
                        .args = args_range,
                    });
                },
                else => {
                    // This should be impossible. See above comment for details
                    // TODO: Is there a better way to do this?
                    std.debug.assert(false);
                },
            }
        }
        const tags_end = self.scratch_tags.top();

        // Append the tags to the types store
        const range = try self.env.types.appendTags(self.scratch_tags.slice(tags_start, tags_end));

        // Shrink the scratch var buffer now that our work is done
        self.scratch_tags.clearFrom(tags_start);

        break :blk range;
    };

    // Get the tag union ext
    const tag_union_ext = blk: {
        if (tag_union.ext) |ext_anno_idx| {
            break :blk try self.canonicalizeTypeAnnoToTypeVar(ext_anno_idx);
        } else {
            break :blk try self.env.addTypeSlotAndTypeVar(
                parent_node_idx,
                Content{ .structure = .empty_tag_union },
                region,
                TypeVar,
            );
        }
    };

    // Create the type content
    const tag_union_content = types.Content{ .structure = types.FlatType{
        .tag_union = types.TagUnion{ .tags = tags_range, .ext = tag_union_ext },
    } };

    // Create the type var
    return try self.env.addTypeSlotAndTypeVar(
        parent_node_idx,
        tag_union_content,
        region,
        TypeVar,
    );
}

/// Handle module-qualified types like Json.Decoder
/// Create an annotation from a type annotation
fn createAnnotationFromTypeAnno(self: *Self, type_anno_idx: TypeAnno.Idx, _: TypeVar, region: Region) std.mem.Allocator.Error!?Annotation.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Convert the type annotation to a type variable
    const signature = try self.canonicalizeTypeAnnoToTypeVar(type_anno_idx);

    // Create the annotation structure
    const annotation = Annotation{
        .type_anno = type_anno_idx,
        .signature = signature,
    };

    // Add to NodeStore and return the index
    const annotation_idx = try self.env.addAnnotationAndTypeVar(annotation, .{ .flex_var = null }, region);

    return annotation_idx;
}

/// Process type imports from a module
///
/// NOTE: When qualified types are encountered (e.g. `SomeModule.TypeName`)
/// we create external declarations that will be resolved later when
/// we have access to the other module's IR after it has been type checked.
fn processTypeImports(self: *Self, module_name: Ident.Idx, alias_name: Ident.Idx) std.mem.Allocator.Error!void {
    // Set up the module alias for qualified lookups
    const scope = self.currentScope();
    _ = try scope.introduceModuleAlias(
        self.env.gpa,
        &self.env.idents,
        alias_name,
        module_name,
        null, // No parent lookup function for now
    );
}

/// Try to handle field access as a module-qualified lookup.
///
/// Examples:
/// - `Json.utf8` where `Json` is a module alias and `utf8` is an exposed function
/// - `Http.get` where `Http` is imported and `get` is available in that module
///
/// Returns `null` if this is not a module-qualified lookup (e.g., regular field access like `user.name`)
fn tryModuleQualifiedLookup(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?Expr.Idx {
    const left_expr = self.parse_ir.store.getExpr(field_access.left);
    if (left_expr != .ident) return null;

    const left_ident = left_expr.ident;
    const module_alias = self.parse_ir.tokens.resolveIdentifier(left_ident.token) orelse return null;

    // Check if this is a module alias
    const module_name = self.scopeLookupModule(module_alias) orelse return null;
    const module_text = self.env.idents.getText(module_name);

    // Check if this module is imported in the current scope
    const import_idx = self.scopeLookupImportedModule(module_text) orelse {
        // Module not imported in current scope
        const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
        _ = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
            .module_name = module_name,
            .region = region,
        } });
        return null;
    };

    // This is a module-qualified lookup
    const right_expr = self.parse_ir.store.getExpr(field_access.right);
    if (right_expr != .ident) return null;

    const right_ident = right_expr.ident;
    const field_name = self.parse_ir.tokens.resolveIdentifier(right_ident.token) orelse return null;

    const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);

    // Look up the target node index in the module's exposed_nodes
    const field_text = self.env.idents.getText(field_name);
    const target_node_idx = if (self.module_envs) |envs_map| blk: {
        if (envs_map.get(module_text)) |module_env| {
            break :blk module_env.exposed_nodes.get(field_text) orelse 0;
        } else {
            break :blk 0;
        }
    } else 0;

    // Create the e_lookup_external expression with Import.Idx
    const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_lookup_external = .{
        .module_idx = import_idx,
        .target_node_idx = target_node_idx,
        .region = region,
    } }, Content{ .flex_var = null }, region);
    return expr_idx;
}

/// Canonicalize regular field access (not module-qualified).
///
/// Examples:
/// - `user.name` - accessing a field on a record
/// - `list.map(transform)` - calling a method with arguments
/// - `result.isOk` - accessing a field that might be a function
fn canonicalizeRegularFieldAccess(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Canonicalize the receiver (left side of the dot)
    const receiver_idx = try self.canonicalizeFieldAccessReceiver(field_access) orelse return null;

    // Parse the right side - this could be just a field name or a method call
    const field_name, const args = try self.parseFieldAccessRight(field_access);

    const dot_access_expr = Expr{
        .e_dot_access = .{
            .receiver = receiver_idx,
            .field_name = field_name,
            .args = args,
        },
    };

    const expr_idx = try self.env.addExprAndTypeVar(dot_access_expr, Content{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(field_access.region));
    return expr_idx;
}

/// Canonicalize the receiver (left side) of field access.
///
/// Examples:
/// - In `user.name`, canonicalizes `user`
/// - In `getUser().email`, canonicalizes `getUser()`
/// - In `[1,2,3].map(fn)`, canonicalizes `[1,2,3]`
fn canonicalizeFieldAccessReceiver(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!?Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (try self.canonicalizeExpr(field_access.left)) |idx| {
        return idx;
    } else {
        // Failed to canonicalize receiver, return malformed
        const region = self.parse_ir.tokenizedRegionToRegion(field_access.region);
        return try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
            .region = region,
        } });
    }
}

/// Parse the right side of field access, handling both plain fields and method calls.
///
/// Examples:
/// - `user.name` - returns `("name", null)` for plain field access
/// - `list.map(fn)` - returns `("map", args)` where args contains the canonicalized function
/// - `obj.method(a, b)` - returns `("method", args)` where args contains canonicalized a and b
fn parseFieldAccessRight(self: *Self, field_access: AST.BinOp) std.mem.Allocator.Error!struct { Ident.Idx, ?Expr.Span } {
    const right_expr = self.parse_ir.store.getExpr(field_access.right);

    return switch (right_expr) {
        .apply => |apply| try self.parseMethodCall(apply),
        .ident => |ident| .{ try self.resolveIdentOrFallback(ident.token), null },
        else => .{ try self.createUnknownIdent(), null },
    };
}

/// Parse a method call on the right side of field access.
///
/// Examples:
/// - `.map(transform)` - extracts "map" as method name and canonicalizes `transform` argument
/// - `.filter(predicate)` - extracts "filter" and canonicalizes `predicate`
/// - `.fold(0, combine)` - extracts "fold" and canonicalizes both `0` and `combine` arguments
fn parseMethodCall(self: *Self, apply: @TypeOf(@as(AST.Expr, undefined).apply)) std.mem.Allocator.Error!struct { Ident.Idx, ?Expr.Span } {
    const method_expr = self.parse_ir.store.getExpr(apply.@"fn");
    const field_name = switch (method_expr) {
        .ident => |ident| try self.resolveIdentOrFallback(ident.token),
        else => try self.createUnknownIdent(),
    };

    // Canonicalize the arguments using scratch system
    const scratch_top = self.env.store.scratchExprTop();
    for (self.parse_ir.store.exprSlice(apply.args)) |arg_idx| {
        if (try self.canonicalizeExpr(arg_idx)) |canonicalized| {
            try self.env.store.addScratchExpr(canonicalized);
        } else {
            self.env.store.clearScratchExprsFrom(scratch_top);
            return .{ field_name, null };
        }
    }
    const args = try self.env.store.exprSpanFrom(scratch_top);

    return .{ field_name, args };
}

/// Resolve an identifier token or return a fallback "unknown" identifier.
///
/// This helps maintain the "inform don't block" philosophy - even if we can't
/// resolve an identifier (due to malformed input), we continue compilation.
///
/// Examples:
/// - Valid token for "name" -> returns the interned identifier for "name"
/// - Malformed/missing token -> returns identifier for "unknown"
fn resolveIdentOrFallback(self: *Self, token: Token.Idx) std.mem.Allocator.Error!Ident.Idx {
    if (self.parse_ir.tokens.resolveIdentifier(token)) |ident_idx| {
        return ident_idx;
    } else {
        return try self.createUnknownIdent();
    }
}

/// Create an "unknown" identifier for fallback cases.
///
/// Used when we encounter malformed or unexpected syntax but want to continue
/// compilation instead of stopping. This supports the compiler's "inform don't block" approach.
fn createUnknownIdent(self: *Self) std.mem.Allocator.Error!Ident.Idx {
    return try self.env.idents.insert(self.env.gpa, base.Ident.for_text("unknown"), Region.zero());
}

/// Context helper for Scope tests
const ScopeTestContext = struct {
    self: Self,
    env: *ModuleEnv,
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!ScopeTestContext {
        // heap allocate env for testing
        const env = try gpa.create(ModuleEnv);
        env.* = try ModuleEnv.init(gpa, "");

        // Initialize CIR fields in env
        try env.initCIRFields(gpa, "Test");

        return ScopeTestContext{
            .self = try Self.init(env, undefined, null),
            .env = env,
            .gpa = gpa,
        };
    }

    fn deinit(ctx: *ScopeTestContext) void {
        ctx.self.deinit();
        ctx.env.deinit();
        ctx.gpa.destroy(ctx.env);
    }
};

// We write out this giant literal because it's actually annoying to try to
// take std.math.minInt(i128), drop the minus sign, and convert it to u128
// all at comptime. Instead we just have a test that verifies its correctness.
const min_i128_negated: u128 = 170141183460469231731687303715884105728;

test "min_i128_negated is actually the minimum i128, negated" {
    var min_i128_buf: [64]u8 = undefined;
    const min_i128_str = std.fmt.bufPrint(&min_i128_buf, "{}", .{std.math.minInt(i128)}) catch unreachable;

    var negated_buf: [64]u8 = undefined;
    const negated_str = std.fmt.bufPrint(&negated_buf, "-{}", .{min_i128_negated}) catch unreachable;

    try std.testing.expectEqualStrings(min_i128_str, negated_str);
}

test "basic scope initialization" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Test that we start with one scope (top-level)
    try std.testing.expect(ctx.self.scopes.items.len == 1);
}

test "empty scope has no items" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const result = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .not_found = {} }, result);
}

test "can add and lookup idents at top level" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("foo"), base.Region.zero());
    const bar_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("bar"), base.Region.zero());
    const foo_pattern: Pattern.Idx = @enumFromInt(1);
    const bar_pattern: Pattern.Idx = @enumFromInt(2);

    // Add identifiers
    const foo_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, foo_ident, foo_pattern, false, true);
    const bar_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, bar_ident, bar_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, foo_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, bar_result);

    // Lookup should find them
    const foo_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);
    const bar_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, bar_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = foo_pattern }, foo_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = bar_pattern }, bar_lookup);
}

test "nested scopes shadow outer scopes" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const x_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    const outer_pattern: Pattern.Idx = @enumFromInt(1);
    const inner_pattern: Pattern.Idx = @enumFromInt(2);

    // Add x to outer scope
    const outer_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, x_ident, outer_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, outer_result);

    // Enter new scope
    try ctx.self.scopeEnter(gpa, false);

    // x from outer scope should still be visible
    const outer_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, outer_lookup);

    // Add x to inner scope (shadows outer)
    const inner_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, x_ident, inner_pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .shadowing_warning = outer_pattern }, inner_result);

    // Now x should resolve to inner scope
    const inner_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = inner_pattern }, inner_lookup);

    // Exit inner scope
    try ctx.self.scopeExit(gpa);

    // x should resolve to outer scope again
    const after_exit_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, x_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = outer_pattern }, after_exit_lookup);
}

test "top level var error" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const var_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern: Pattern.Idx = @enumFromInt(1);

    // Should fail to introduce var at top level
    const result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, var_ident, pattern, true, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .top_level_var_error = {} }, result);
}

test "type variables are tracked separately from value identifiers" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Create identifiers for 'a' - one for value, one for type
    const a_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("a"), base.Region.zero());
    const pattern: Pattern.Idx = @enumFromInt(1);
    const type_anno: TypeAnno.Idx = @enumFromInt(1);

    // Introduce 'a' as a value identifier
    const value_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, a_ident, pattern, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, value_result);

    // Introduce 'a' as a type variable - should succeed because they're in separate namespaces
    const current_scope = &ctx.self.scopes.items[ctx.self.scopes.items.len - 1];
    const type_result = current_scope.introduceTypeVar(gpa, &ctx.env.idents, a_ident, type_anno, null);
    try std.testing.expectEqual(Scope.TypeVarIntroduceResult{ .success = {} }, type_result);

    // Lookup 'a' as value should find the pattern
    const value_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, a_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern }, value_lookup);

    // Lookup 'a' as type variable should find the type annotation
    const type_lookup = current_scope.lookupTypeVar(&ctx.env.idents, a_ident);
    try std.testing.expectEqual(Scope.TypeVarLookupResult{ .found = type_anno }, type_lookup);
}

test "var reassignment within same function" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter function scope
    try ctx.self.scopeEnter(gpa, true);

    const count_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Reassign var (not a declaration)
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, reassign_result);

    // Should resolve to the reassigned value
    const lookup_result = ctx.self.scopeLookup(&ctx.env.idents, .ident, count_ident);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, lookup_result);
}

test "var reassignment across function boundary fails" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    // Enter first function scope
    try ctx.self.scopeEnter(gpa, true);

    const count_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("count_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Declare var in first function
    const declare_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern1, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, declare_result);

    // Enter second function scope (function boundary)
    try ctx.self.scopeEnter(gpa, true);

    // Try to reassign var from different function - should fail
    const reassign_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, count_ident, pattern2, true, false);
    try std.testing.expectEqual(Scope.IntroduceResult{ .var_across_function_boundary = pattern1 }, reassign_result);
}

test "identifiers with and without underscores are different" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const sum_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("sum"), base.Region.zero());
    const sum_underscore_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("sum_"), base.Region.zero());
    const pattern1: Pattern.Idx = @enumFromInt(1);
    const pattern2: Pattern.Idx = @enumFromInt(2);

    // Enter function scope so we can use var
    try ctx.self.scopeEnter(gpa, true);

    // Introduce regular identifier
    const regular_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, sum_ident, pattern1, false, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, regular_result);

    // Introduce var with underscore - should not conflict
    const var_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, sum_underscore_ident, pattern2, true, true);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, var_result);

    // Both should be found independently
    const regular_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, sum_ident);
    const var_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, sum_underscore_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern1 }, regular_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = pattern2 }, var_lookup);
}

test "aliases work separately from idents" {
    const gpa = std.testing.allocator;

    var ctx = try ScopeTestContext.init(gpa);
    defer ctx.deinit();

    const foo_ident = try ctx.env.idents.insert(gpa, base.Ident.for_text("Foo"), base.Region.zero());
    const ident_pattern: Pattern.Idx = @enumFromInt(1);
    const alias_pattern: Pattern.Idx = @enumFromInt(2);

    // Add as both ident and alias (they're in separate namespaces)
    const ident_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .ident, foo_ident, ident_pattern, false, true);
    const alias_result = ctx.self.scopeIntroduceInternal(gpa, &ctx.env.idents, .alias, foo_ident, alias_pattern, false, true);

    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, ident_result);
    try std.testing.expectEqual(Scope.IntroduceResult{ .success = {} }, alias_result);

    // Both should be found in their respective namespaces
    const ident_lookup = ctx.self.scopeLookup(&ctx.env.idents, .ident, foo_ident);
    const alias_lookup = ctx.self.scopeLookup(&ctx.env.idents, .alias, foo_ident);

    try std.testing.expectEqual(Scope.LookupResult{ .found = ident_pattern }, ident_lookup);
    try std.testing.expectEqual(Scope.LookupResult{ .found = alias_pattern }, alias_lookup);
}

test "hexadecimal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic hex literals
        .{ .literal = "0x0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0x1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0xFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0x100", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFFFF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x10000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0xFFFFFFFF", .expected_value = 4294967295, .expected_sign_needed = false, .expected_bits_needed = 5 },
        .{ .literal = "0x100000000", .expected_value = 4294967296, .expected_sign_needed = false, .expected_bits_needed = 6 },
        .{ .literal = "0xFFFFFFFFFFFFFFFF", .expected_value = @as(i128, @bitCast(@as(u128, 18446744073709551615))), .expected_sign_needed = false, .expected_bits_needed = 7 },

        // Hex with underscores
        .{ .literal = "0x1_000", .expected_value = 4096, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0xFF_FF", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0x1234_5678_9ABC_DEF0", .expected_value = @as(i128, @bitCast(@as(u128, 0x123456789ABCDEF0))), .expected_sign_needed = false, .expected_bits_needed = 6 },

        // Negative hex literals
        .{ .literal = "-0x1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x80", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0x81", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0x8000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0x8001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
        .{ .literal = "-0x80000000", .expected_value = -2147483648, .expected_sign_needed = true, .expected_bits_needed = 4 },
        .{ .literal = "-0x80000001", .expected_value = -2147483649, .expected_sign_needed = true, .expected_bits_needed = 5 },
        .{ .literal = "-0x8000000000000000", .expected_value = -9223372036854775808, .expected_sign_needed = true, .expected_bits_needed = 6 },
        .{ .literal = "-0x8000000000000001", .expected_value = @as(i128, -9223372036854775809), .expected_sign_needed = true, .expected_bits_needed = 7 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "binary integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic binary literals
        .{ .literal = "0b0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b10", .expected_value = 2, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b100000000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1111111111111111", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0b10000000000000000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Binary with underscores
        .{ .literal = "0b11_11", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0b1111_1111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0b1_0000_0000", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0b1010_1010_1010_1010", .expected_value = 43690, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Negative binary
        .{ .literal = "-0b1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000000", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0b10000001", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0b1000000000000000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0b1000000000000001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "octal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Basic octal literals
        .{ .literal = "0o0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o10", .expected_value = 8, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0o377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0o400", .expected_value = 256, .expected_sign_needed = false, .expected_bits_needed = 2 },
        .{ .literal = "0o177777", .expected_value = 65535, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0o200000", .expected_value = 65536, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Octal with underscores
        .{ .literal = "0o377_377", .expected_value = 130815, .expected_sign_needed = false, .expected_bits_needed = 4 },
        .{ .literal = "0o1_234_567", .expected_value = 342391, .expected_sign_needed = false, .expected_bits_needed = 4 },

        // Negative octal literals
        .{ .literal = "-0o1", .expected_value = -1, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o100", .expected_value = -64, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o200", .expected_value = -128, .expected_sign_needed = true, .expected_bits_needed = 0 },
        .{ .literal = "-0o201", .expected_value = -129, .expected_sign_needed = true, .expected_bits_needed = 1 },
        .{ .literal = "-0o100000", .expected_value = -32768, .expected_sign_needed = true, .expected_bits_needed = 2 },
        .{ .literal = "-0o100001", .expected_value = -32769, .expected_sign_needed = true, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "integer literals with uppercase base prefixes" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
        expected_sign_needed: bool,
        expected_bits_needed: u8,
    }{
        // Uppercase hex prefix
        .{ .literal = "0X0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0X1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0XFF", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0XABCD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },

        // Uppercase binary prefix
        .{ .literal = "0B0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1", .expected_value = 1, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B1111", .expected_value = 15, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0B11111111", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },

        // Uppercase octal prefix
        .{ .literal = "0O0", .expected_value = 0, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O7", .expected_value = 7, .expected_sign_needed = false, .expected_bits_needed = 0 },
        .{ .literal = "0O377", .expected_value = 255, .expected_sign_needed = false, .expected_bits_needed = 1 },
        .{ .literal = "0O777", .expected_value = 511, .expected_sign_needed = false, .expected_bits_needed = 2 },

        // Mixed case in value (should still work)
        .{ .literal = "0xAbCd", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
        .{ .literal = "0XaBcD", .expected_value = 43981, .expected_sign_needed = false, .expected_bits_needed = 3 },
    };

    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx);
        try std.testing.expect(expr == .e_int);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_int.value.bytes)));

        const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(canonical_expr_idx));
        const resolved = env.types.resolveVar(expr_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .int_poly => |poly| {
                        try std.testing.expectEqual(tc.expected_sign_needed, poly.requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, poly.requirements.bits_needed);
                    },
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    .int_unbound => |requirements| {
                        try std.testing.expectEqual(tc.expected_sign_needed, requirements.sign_needed);
                        try std.testing.expectEqual(tc.expected_bits_needed, requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric literal patterns use pattern idx as type var" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that int literal patterns work and use the pattern index as the type variable
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Create an int literal pattern directly
        const int_pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(int_pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        // Verify the stored pattern
        const stored_pattern = env.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .int_literal);
        try std.testing.expectEqual(@as(i128, 42), @as(i128, @bitCast(stored_pattern.int_literal.value.bytes)));

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .num_unbound => |requirements| {
                        try std.testing.expectEqual(false, requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), requirements.bits_needed);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }

    // Test that f64 literal patterns work
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Create a dec literal pattern directly
        const dec_pattern = Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(3.14) orelse unreachable,
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(dec_pattern, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        }, base.Region.zero());

        // Verify the stored pattern
        const stored_pattern = env.store.getPattern(pattern_idx);
        try std.testing.expect(stored_pattern == .dec_literal);
        const expected_dec = RocDec.fromF64(3.14) orelse unreachable;
        try std.testing.expectEqual(expected_dec.num, stored_pattern.dec_literal.value.num);

        // Verify the pattern index can be used as a type variable
        const pattern_as_type_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_as_type_var);
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .frac_unbound => |requirements| {
                        try std.testing.expectEqual(true, requirements.fits_in_f32);
                        try std.testing.expectEqual(true, requirements.fits_in_dec);
                    },
                    else => return error.UnexpectedNumType,
                },
                else => return error.UnexpectedStructureType,
            },
            else => return error.UnexpectedContentType,
        }
    }
}

test "numeric pattern types: unbound vs polymorphic" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test int_unbound pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, -17)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = true,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        try std.testing.expectEqual(true, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test int_poly pattern (polymorphic integer that can be different int types)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 255)), .kind = .i128 },
            },
        };

        // Create a fresh type variable for polymorphic int
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{
                .num = .{
                    .int_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .sign_needed = false,
                            .bits_needed = 1, // Needs at least 8 bits for 255
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedIntPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_unbound pattern (can be int or frac)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 10)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 0,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_unbound => |req| {
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), req.bits_needed);
                    },
                    else => return error.ExpectedNumUnbound,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test num_poly pattern (polymorphic num that can be int or frac)
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 5)), .kind = .i128 },
            },
        };

        // Create a fresh type variable for polymorphic num
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{ .num = .{ .num_poly = .{
                .var_ = poly_var,
                .requirements = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                },
            } } },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_poly => |poly| {
                        try std.testing.expectEqual(false, poly.requirements.sign_needed);
                        try std.testing.expectEqual(@as(u8, 0), poly.requirements.bits_needed);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedNumPoly,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test frac_unbound pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(2.5) orelse unreachable,
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .frac_unbound = .{
                .fits_in_f32 = true,
                .fits_in_dec = true,
            } } },
        }, base.Region.zero());

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_unbound => |req| {
                        try std.testing.expectEqual(true, req.fits_in_f32);
                        try std.testing.expectEqual(true, req.fits_in_dec);
                    },
                    else => return error.ExpectedFracUnbound,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "record literal uses record_unbound" {
    const gpa = std.testing.allocator;

    // Test a simple record literal
    {
        const source1 = "{ x: 42, y: \"hello\" }";

        var env = try ModuleEnv.init(gpa, source1);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try Self.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        // Get the type of the expression
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
        const resolved = env.types.resolveVar(expr_var);

        // Check that it's a record_unbound
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .record_unbound => |fields| {
                    // Success! The record literal created a record_unbound type
                    try testing.expect(fields.len() == 2);
                },
                else => return error.ExpectedRecordUnbound,
            },
            else => return error.ExpectedStructure,
        }
    }

    // Test an empty record literal
    {
        const source2 = "{}";

        var env = try ModuleEnv.init(gpa, source2);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try Self.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        // Get the type of the expression
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
        const resolved = env.types.resolveVar(expr_var);

        // Check that it's an empty_record
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .empty_record => {
                    // Success! Empty record literal created empty_record type
                },
                else => return error.ExpectedEmptyRecord,
            },
            else => return error.ExpectedStructure,
        }
    }

    // Test a record with a single field
    // Test a nested record literal
    {
        const source3 = "{ value: 123 }";

        var env = try ModuleEnv.init(gpa, source3);
        defer env.deinit();

        var ast = try parse.parseExpr(&env);
        defer ast.deinit(gpa);

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        var can = try Self.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        // Get the type of the expression
        const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
        const resolved = env.types.resolveVar(expr_var);

        // Check that it's a record_unbound
        switch (resolved.desc.content) {
            .structure => |structure| switch (structure) {
                .record_unbound => |fields| {
                    // Success! The record literal created a record_unbound type
                    try testing.expect(fields.len() == 1);

                    // Check the field
                    const fields_slice = env.types.getRecordFieldsSlice(fields);
                    const field_name = env.idents.getText(fields_slice.get(0).name);
                    try testing.expectEqualStrings("value", field_name);
                },
                else => return error.ExpectedRecordUnbound,
            },
            else => return error.ExpectedStructure,
        }
    }
}

test "record_unbound basic functionality" {
    const gpa = std.testing.allocator;
    const source = "{ x: 42, y: 99 }";

    // Test that record literals create record_unbound types
    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    var ast = try parse.parseExpr(&env);
    defer ast.deinit(gpa);

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(gpa, "Test");

    var can = try Self.init(&env, &ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Get the type of the expression
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved = env.types.resolveVar(expr_var);

    // Verify it starts as record_unbound
    switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .record_unbound => |fields| {
                // Success! Record literal created record_unbound type
                try testing.expect(fields.len() == 2);

                // Check field names
                const field_slice = env.types.getRecordFieldsSlice(fields);
                try testing.expectEqualStrings("x", env.idents.getText(field_slice.get(0).name));
                try testing.expectEqualStrings("y", env.idents.getText(field_slice.get(1).name));
            },
            else => return error.ExpectedRecordUnbound,
        },
        else => return error.ExpectedStructure,
    }
}

test "record_unbound with multiple fields" {
    const gpa = std.testing.allocator;
    const source = "{ a: 123, b: 456, c: 789 }";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    // Create record_unbound with multiple fields
    var ast = try parse.parseExpr(&env);
    defer ast.deinit(gpa);

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(gpa, "Test");

    var can = try Self.init(&env, &ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved = env.types.resolveVar(expr_var);

    // Should be record_unbound
    switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .record_unbound => |fields| {
                try testing.expect(fields.len() == 3);

                // Check field names
                const field_slice = env.types.getRecordFieldsSlice(fields);
                try testing.expectEqualStrings("a", env.idents.getText(field_slice.get(0).name));
                try testing.expectEqualStrings("b", env.idents.getText(field_slice.get(1).name));
                try testing.expectEqualStrings("c", env.idents.getText(field_slice.get(2).name));
            },
            else => return error.ExpectedRecordUnbound,
        },
        else => return error.ExpectedStructure,
    }
}

test "record with extension variable" {
    const gpa = std.testing.allocator;

    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(gpa, "Test");

    // Test that regular records have extension variables
    // Create { x: 42, y: "hi" }* (open record)
    const num_var = try env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const str_var = try env.types.freshFromContent(Content{ .structure = .str });

    const fields = [_]types.RecordField{
        .{ .name = try env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero()), .var_ = num_var },
        .{ .name = try env.idents.insert(gpa, base.Ident.for_text("y"), base.Region.zero()), .var_ = str_var },
    };
    const fields_range = try env.types.appendRecordFields(&fields);
    const ext_var = try env.types.fresh(); // Open extension
    const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
    const record_var = try env.types.freshFromContent(record_content);

    // Verify the record has an extension variable
    const resolved = env.types.resolveVar(record_var);
    switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .record => |record| {
                try testing.expect(record.fields.len() == 2);

                // Check that extension is a flex var (open record)
                const ext_resolved = env.types.resolveVar(record.ext);
                switch (ext_resolved.desc.content) {
                    .flex_var => {
                        // Success! The record has an open extension
                    },
                    else => return error.ExpectedFlexVar,
                }
            },
            else => return error.ExpectedRecord,
        },
        else => return error.ExpectedStructure,
    }

    // Now test a closed record
    const closed_ext_var = try env.types.freshFromContent(Content{ .structure = .empty_record });
    const closed_record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = closed_ext_var } } };
    const closed_record_var = try env.types.freshFromContent(closed_record_content);

    // Verify the closed record has empty_record as extension
    const closed_resolved = env.types.resolveVar(closed_record_var);
    switch (closed_resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .record => |record| {
                try testing.expect(record.fields.len() == 2);

                // Check that extension is empty_record (closed record)
                const ext_resolved = env.types.resolveVar(record.ext);
                switch (ext_resolved.desc.content) {
                    .structure => |ext_structure| switch (ext_structure) {
                        .empty_record => {
                            // Success! The record is closed
                        },
                        else => return error.ExpectedEmptyRecord,
                    },
                    else => return error.ExpectedStructure,
                }
            },
            else => return error.ExpectedRecord,
        },
        else => return error.ExpectedStructure,
    }
}

test "numeric pattern types: unbound vs polymorphic - frac" {
    const gpa = std.testing.allocator;

    // Test frac_poly pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(1000000.0) orelse unreachable,
            },
        };

        // Create a fresh type variable for polymorphic frac
        const poly_var = try env.types.fresh();

        const pattern_idx = try env.store.addPattern(pattern, base.Region.zero());
        _ = try env.types.freshFromContent(Content{
            .structure = .{
                .num = .{
                    .frac_poly = .{
                        .var_ = poly_var,
                        .requirements = .{
                            .fits_in_f32 = true,
                            .fits_in_dec = false, // Infinity doesn't fit in Dec
                        },
                    },
                },
            },
        });

        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .frac_poly => |poly| {
                        try std.testing.expectEqual(true, poly.requirements.fits_in_f32);
                        try std.testing.expectEqual(false, poly.requirements.fits_in_dec);
                        try std.testing.expectEqual(poly_var, poly.var_);
                    },
                    else => return error.ExpectedFracPoly,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern numeric literal value edge cases" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test max/min integer values
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Test i128 max
        const max_pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.maxInt(i128))), .kind = .i128 },
            },
        };
        const max_idx = try env.store.addPattern(max_pattern, base.Region.zero());
        const stored_max = env.store.getPattern(max_idx);
        try std.testing.expectEqual(std.math.maxInt(i128), @as(i128, @bitCast(stored_max.int_literal.value.bytes)));

        // Test i128 min
        const min_pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, std.math.minInt(i128))), .kind = .i128 },
            },
        };
        const min_idx = try env.store.addPattern(min_pattern, base.Region.zero());
        const stored_min = env.store.getPattern(min_idx);
        try std.testing.expectEqual(std.math.minInt(i128), @as(i128, @bitCast(stored_min.int_literal.value.bytes)));
    }

    // Test small decimal pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const small_dec_pattern = Pattern{
            .small_dec_literal = .{
                .numerator = 1234,
                .denominator_power_of_ten = 2, // 12.34

            },
        };

        const pattern_idx = try env.store.addPattern(small_dec_pattern, base.Region.zero());
        const stored = env.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .small_dec_literal);
        try std.testing.expectEqual(@as(i16, 1234), stored.small_dec_literal.numerator);
        try std.testing.expectEqual(@as(u8, 2), stored.small_dec_literal.denominator_power_of_ten);
    }

    // Test dec literal pattern
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const dec_pattern = Pattern{
            .dec_literal = .{
                .value = RocDec{ .num = 314159265358979323 }, // π * 10^17

            },
        };

        const pattern_idx = try env.store.addPattern(dec_pattern, base.Region.zero());
        const stored = env.store.getPattern(pattern_idx);

        try std.testing.expect(stored == .dec_literal);
        try std.testing.expectEqual(@as(i128, 314159265358979323), stored.dec_literal.value.num);
    }

    // Test special float values
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Test negative zero (RocDec doesn't distinguish between +0 and -0)
        const neg_zero_pattern = Pattern{
            .dec_literal = .{
                .value = RocDec.fromF64(-0.0) orelse unreachable,
            },
        };
        const neg_zero_idx = try env.store.addPattern(neg_zero_pattern, base.Region.zero());
        const stored_neg_zero = env.store.getPattern(neg_zero_idx);
        try std.testing.expect(stored_neg_zero == .dec_literal);
        try std.testing.expectEqual(@as(i128, 0), stored_neg_zero.dec_literal.value.num);
    }
}

test "pattern literal type transitions" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test transitioning from unbound to concrete type
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        // Simulate type inference determining it's a U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = try env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it resolved to U8
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| switch (compact) {
                        .int => |int| {
                            try std.testing.expect(int == .u8);
                        },
                        else => return error.ExpectedInt,
                    },
                    else => return error.ExpectedNumCompact,
                },
                else => {},
            },
            else => {},
        }
    }

    // Test hex/binary/octal patterns must be integers
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Hex pattern (0xFF)
        const hex_pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 0xFF)), .kind = .i128 },
            },
        };

        const hex_idx = try env.addPatternAndTypeVar(hex_pattern, Content{
            .structure = .{ .num = .{ .int_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        const hex_var: types.Var = @enumFromInt(@intFromEnum(hex_idx));
        const resolved = env.types.resolveVar(hex_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_unbound => |req| {
                        // Verify it's constrained to integers only
                        try std.testing.expectEqual(false, req.sign_needed);
                        try std.testing.expectEqual(@as(u8, 1), req.bits_needed);
                    },
                    else => return error.ExpectedIntUnbound,
                },
                else => {},
            },
            else => {},
        }
    }
}

test "pattern type inference with numeric literals" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that pattern indices work correctly as type variables with type inference
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Create patterns representing different numeric literals
        const patterns = [_]struct {
            pattern: Pattern,
            expected_type: types.Content,
        }{
            // Small positive int - could be any unsigned type
            .{
                .pattern = Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = false,
                    .bits_needed = 0,
                } } } },
            },
            // Negative int - needs signed type
            .{
                .pattern = Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, -42)), .kind = .i128 },
                    },
                },
                .expected_type = Content{ .structure = .{ .num = .{ .num_unbound = .{
                    .sign_needed = true,
                    .bits_needed = 0,
                } } } },
            },
            // Large int requiring more bits
            .{
                .pattern = Pattern{
                    .int_literal = .{
                        .value = .{ .bytes = @bitCast(@as(i128, 65536)), .kind = .i128 },
                    },
                },
                .expected_type = Content{
                    .structure = .{
                        .num = .{
                            .num_unbound = .{
                                .sign_needed = false,
                                .bits_needed = 4, // Needs at least 17 bits
                            },
                        },
                    },
                },
            },
        };

        for (patterns) |test_case| {
            const pattern_idx = try env.addPatternAndTypeVar(test_case.pattern, test_case.expected_type, base.Region.zero());

            // Verify the pattern index works as a type variable
            const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
            const resolved = env.types.resolveVar(pattern_var);

            // Compare the resolved type with expected
            try std.testing.expectEqual(test_case.expected_type, resolved.desc.content);
        }
    }

    // Test patterns with type constraints from context
    {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();

        // Initialize CIR fields in ModuleEnv
        try env.initCIRFields(gpa, "Test");

        // Create a pattern that will be constrained by context
        const pattern = Pattern{
            .int_literal = .{
                .value = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 },
            },
        };

        const pattern_idx = try env.addPatternAndTypeVar(pattern, Content{
            .structure = .{ .num = .{ .num_unbound = .{
                .sign_needed = false,
                .bits_needed = 1,
            } } },
        }, base.Region.zero());

        // Simulate type inference constraining it to U8
        const pattern_var: types.Var = @enumFromInt(@intFromEnum(pattern_idx));
        _ = try env.types.setVarContent(pattern_var, Content{
            .structure = .{ .num = types.Num.int_u8 },
        });

        // Verify it was constrained correctly
        const resolved = env.types.resolveVar(pattern_var);
        switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .num_compact => |compact| {
                        try std.testing.expect(compact.int == .u8);
                    },
                    else => return error.ExpectedConcreteType,
                },
                else => {},
            },
            else => {},
        }
    }
}
