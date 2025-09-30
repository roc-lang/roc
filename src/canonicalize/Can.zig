//! Transforms Abstract Syntax Tree (AST) into Canonical Intermediate Representation (CIR) through desugaring and scope resolution.
//!
//! This module performs semantic analysis, resolves scoping, and transforms high-level language
//! constructs into a simplified, normalized form suitable for type inference.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const parse = @import("parse");
const collections = @import("collections");
const types = @import("types");
const builtins = @import("builtins");
const tracy = @import("tracy");

const CIR = @import("CIR.zig");
const Scope = @import("Scope.zig");

const tokenize = parse.tokenize;
const RocDec = builtins.dec.RocDec;
const CompileNodeStore = @import("NodeStore.zig");
const AST = parse.AST;
const Token = tokenize.Token;
const DataSpan = base.DataSpan;
const ModuleEnv = @import("ModuleEnv.zig");
const Node = @import("Node.zig");

/// Both the canonicalized expression and any free variables
///
/// We keep track of the free variables as we go so we can union these
/// in our Lambda's in a single forward pass during canonicalization.
pub const CanonicalizedExpr = struct {
    idx: Expr.Idx,
    free_vars: ?[]Pattern.Idx,

    pub fn get_idx(self: @This()) Expr.Idx {
        return self.idx;
    }

    pub fn maybe_expr_get_idx(self: ?@This()) ?Expr.Idx {
        if (self != null) {
            return self.?.idx;
        } else {
            return null;
        }
    }
};

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

/// Context for module validation - determines what kinds of modules are valid
pub const ValidationContext = enum {
    /// roc check - infer whether type module or default-app, allow both
    checking,
    /// roc foo.roc or roc build - require app or default-app only
    executing,
    /// File is being imported - require proper type module only
    importing,
    /// REPL - no module validation needed (evaluating expressions, not files)
    repl,
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
/// Context for validation - determines what module types are valid
validation_context: ValidationContext,
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
/// Scratch free variables
scratch_free_vars: base.Scratch(Pattern.Idx),

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;
// ModuleEnv is already imported at the top
const CalledVia = base.CalledVia;

const TypeVar = types.Var;
const Content = types.Content;

const FlatType = types.FlatType;
const Num = types.Num;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

// Type aliases for ModuleEnv types
const Pattern = CIR.Pattern;
const Statement = CIR.Statement;
const Expression = CIR.Expression;
const Expr = CIR.Expr;
const Import = CIR.Import;
const Type = CIR.Type;
const TypeAnno = CIR.TypeAnno;
const Annotation = CIR.Annotation;
const WhereClause = CIR.WhereClause;
const Diagnostic = CIR.Diagnostic;
const Closure = CIR.Closure;
const Ability = CIR.Ability;
const RecordField = CIR.RecordField;

/// Struct to track fields that have been seen before during canonicalization
const SeenRecordField = struct { ident: base.Ident.Idx, region: base.Region };

/// The idx of the builtin Bool
/// The idx of the builtin Bool pattern (not used for type checking - use BUILTIN_BOOL_TYPE instead)
pub const BUILTIN_BOOL: Pattern.Idx = @enumFromInt(0);
/// The idx of the builtin Bool type declaration (use this for type checking)
pub var BUILTIN_BOOL_TYPE: Statement.Idx = undefined;
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
    self.scratch_free_vars.deinit(gpa);
}

pub fn init(
    env: *ModuleEnv,
    parse_ir: *AST,
    module_envs: ?*const std.StringHashMap(*ModuleEnv),
    validation_context: ValidationContext,
) std.mem.Allocator.Error!Self {
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
        .validation_context = validation_context,
        .scratch_vars = try base.Scratch(TypeVar).init(gpa),
        .scratch_idents = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_type_var_validation = try base.Scratch(Ident.Idx).init(gpa),
        .scratch_type_var_problems = try base.Scratch(TypeVarProblem).init(gpa),
        .scratch_record_fields = try base.Scratch(types.RecordField).init(gpa),
        .scratch_seen_record_fields = try base.Scratch(SeenRecordField).init(gpa),
        .exposed_scope = Scope.init(false),
        .scratch_tags = try base.Scratch(types.Tag).init(gpa),
        .unqualified_nominal_tags = std.StringHashMapUnmanaged(Statement.Idx){},
        .scratch_free_vars = try base.Scratch(Pattern.Idx).init(gpa),
    };

    // Top-level scope is not a function boundary
    try result.scopeEnter(gpa, false);

    // Simulate the builtins by adding to both the NodeStore and Scopes
    // Not sure if this is how we want to do it long term, but want something to
    // make a start on canonicalization.

    // Assert that the node store is completely empty
    env.debugAssertArraysInSync();

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
    env.debugAssertArraysInSync();

    // Add built-in types to the type scope
    // TODO: These should ultimately come from the platform/builtin files rather than being hardcoded
    try result.addBuiltinTypeBool(env);
    try result.addBuiltinTypeList(env);
    try result.addBuiltinTypeBox(env);
    try result.addBuiltinTypeResult(env);

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
    _ = try result.addBuiltinType(env, "Dict", .{ .flex_var = null });
    _ = try result.addBuiltinType(env, "Set", .{ .flex_var = null });

    return result;
}

// builtins //

fn addBuiltin(self: *Self, ir: *ModuleEnv, ident_text: []const u8, idx: Pattern.Idx) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const ident_add = try ir.insertIdent(base.Ident.for_text(ident_text));
    const pattern_idx_add = try ir.addPatternAndTypeVar(Pattern{ .assign = .{ .ident = ident_add } }, Content{ .flex_var = null }, Region.zero());
    _ = try self.scopeIntroduceInternal(gpa, .ident, ident_add, pattern_idx_add, false, true);
    std.debug.assert(idx == pattern_idx_add);
}

/// Stub builtin types. Currently sets every type to be a nominal type
/// This should be replaced by real builtins eventually
fn addBuiltinType(self: *Self, ir: *ModuleEnv, type_name: []const u8, content: types.Content) std.mem.Allocator.Error!Statement.Idx {
    const gpa = ir.gpa;
    const type_ident = try ir.insertIdent(base.Ident.for_text(type_name));

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());

    // Create a type annotation that refers to itself (built-in types are primitive)
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, content, Region.zero());
    const anno_var = ModuleEnv.castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_nominal_decl = .{ .header = header_idx, .anno = anno_idx },
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

/// Creates `Result(ok, err) := [Ok(ok), Err(err)]`
fn addBuiltinTypeResult(self: *Self, ir: *ModuleEnv) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const type_ident = try ir.insertIdent(base.Ident.for_text("Result"));
    const a_ident = try ir.insertIdent(base.Ident.for_text("ok"));
    const b_ident = try ir.insertIdent(base.Ident.for_text("err"));

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());
    const header_node_idx = ModuleEnv.nodeIdxFrom(header_idx);

    // Create a type annotation that refers to itself (built-in types are primitive)
    const ext_var = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        Content{ .structure = .empty_tag_union },
        Region.zero(),
        TypeVar,
    );
    const a_rigid = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        .{ .rigid_var = a_ident },
        Region.zero(),
        TypeVar,
    );
    const b_rigid = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        .{ .rigid_var = b_ident },
        Region.zero(),
        TypeVar,
    );
    const anno_idx = try ir.addTypeAnnoAndTypeVar(
        .{ .ty = .{ .symbol = type_ident } },
        try ir.types.mkResult(gpa, ir.getIdentStore(), a_rigid, b_rigid, ext_var),
        Region.zero(),
    );
    const anno_var = ModuleEnv.castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_nominal_decl = .{ .header = header_idx, .anno = anno_idx },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkNominal(
            types.TypeIdent{ .ident_idx = type_ident },
            anno_var,
            &.{ a_rigid, b_rigid },
            try ir.insertIdent(base.Ident.for_text(ir.module_name)),
        ),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    try ir.redirectTypeTo(Pattern.Idx, BUILTIN_RESULT, ModuleEnv.varFrom(type_decl_idx));

    // Add True and False to unqualified_nominal_tags
    // TODO: in the future, we should have hardcoded constants for these.
    try self.unqualified_nominal_tags.put(gpa, "Ok", type_decl_idx);
    try self.unqualified_nominal_tags.put(gpa, "Err", type_decl_idx);
}

/// Creates `List(a) : <List Primitive>(a)`
fn addBuiltinTypeList(self: *Self, ir: *ModuleEnv) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const type_ident = try ir.insertIdent(base.Ident.for_text("List"));
    const elem_ident = try ir.insertIdent(base.Ident.for_text("item"));

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());
    const header_node_idx = ModuleEnv.nodeIdxFrom(header_idx);

    // Create a type annotation that refers to itself (built-in types are primitive)
    const elem_var = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        Content{ .rigid_var = elem_ident },
        Region.zero(),
        TypeVar,
    );
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, .{ .structure = .{ .list = elem_var } }, Region.zero());
    const anno_var = ModuleEnv.castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_alias_decl = .{ .header = header_idx, .anno = anno_idx },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkAlias(
            types.TypeIdent{ .ident_idx = type_ident },
            anno_var,
            &.{elem_var},
        ),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    try ir.redirectTypeTo(Pattern.Idx, BUILTIN_LIST, ModuleEnv.varFrom(type_decl_idx));
}

/// Creates `Box(a) : <Box Primitive>(a)`
fn addBuiltinTypeBox(self: *Self, ir: *ModuleEnv) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const type_ident = try ir.insertIdent(base.Ident.for_text("Box"));
    const elem_ident = try ir.insertIdent(base.Ident.for_text("item"));

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());
    const header_node_idx = ModuleEnv.nodeIdxFrom(header_idx);

    // Create a type annotation that refers to itself (built-in types are primitive)
    const elem_var = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        Content{ .rigid_var = elem_ident },
        Region.zero(),
        TypeVar,
    );
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, .{ .structure = .{ .box = elem_var } }, Region.zero());
    const anno_var = ModuleEnv.castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_alias_decl = .{ .header = header_idx, .anno = anno_idx },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkAlias(
            types.TypeIdent{ .ident_idx = type_ident },
            anno_var,
            &.{elem_var},
        ),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    try ir.redirectTypeTo(Pattern.Idx, BUILTIN_BOX, ModuleEnv.varFrom(type_decl_idx));
}

/// Creates `Bool := [True, False]`
fn addBuiltinTypeBool(self: *Self, ir: *ModuleEnv) std.mem.Allocator.Error!void {
    const gpa = ir.gpa;
    const type_ident = try ir.insertIdent(base.Ident.for_text("Bool"));

    // Create a type header for the built-in type
    const header_idx = try ir.addTypeHeaderAndTypeVar(.{
        .name = type_ident,
        .args = .{ .span = .{ .start = 0, .len = 0 } }, // No type parameters for built-ins
    }, .{ .flex_var = null }, Region.zero());
    const header_node_idx = ModuleEnv.nodeIdxFrom(header_idx);

    // Create a type annotation that refers to itself (built-in types are primitive)
    const ext_var = try ir.addTypeSlotAndTypeVar(
        header_node_idx,
        Content{ .structure = .empty_tag_union },
        Region.zero(),
        TypeVar,
    );
    const anno_idx = try ir.addTypeAnnoAndTypeVar(.{ .ty = .{
        .symbol = type_ident,
    } }, try ir.types.mkBool(gpa, ir.getIdentStore(), ext_var), Region.zero());
    const anno_var = ModuleEnv.castIdx(TypeAnno.Idx, TypeVar, anno_idx);

    // Create the type declaration statement
    const type_decl_stmt = Statement{
        .s_nominal_decl = .{ .header = header_idx, .anno = anno_idx },
    };

    const type_decl_idx = try ir.addStatementAndTypeVar(
        type_decl_stmt,
        try ir.types.mkNominal(
            types.TypeIdent{ .ident_idx = type_ident },
            anno_var,
            &.{},
            try ir.insertIdent(base.Ident.for_text(ir.module_name)),
        ),
        Region.zero(),
    );

    // Add to scope without any error checking (built-ins are always valid)
    const current_scope = &self.scopes.items[self.scopes.items.len - 1];
    try current_scope.put(gpa, .type_decl, type_ident, type_decl_idx);

    try ir.redirectTypeTo(Pattern.Idx, BUILTIN_BOOL, ModuleEnv.varFrom(type_decl_idx));

    // Add True and False to unqualified_nominal_tags
    // TODO: in the future, we should have hardcoded constants for these.
    try self.unqualified_nominal_tags.put(gpa, "True", type_decl_idx);
    try self.unqualified_nominal_tags.put(gpa, "False", type_decl_idx);
}

// canonicalize //

const Self = @This();

/// The intermediate representation of a canonicalized Roc program.
/// After parsing a Roc program, the [ParseIR](src/parse/AST.zig) is transformed into a [canonical
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
        .module => |h| {
            // Emit deprecation warning
            const header_region = self.parse_ir.tokenizedRegionToRegion(h.region);
            try self.env.pushDiagnostic(.{
                .module_header_deprecated = .{
                    .region = header_region,
                },
            });
            try self.createExposedScope(h.exposes);
        },
        .package => |h| try self.createExposedScope(h.exposes),
        .platform => |h| try self.createExposedScope(h.exposes),
        .hosted => |h| try self.createExposedScope(h.exposes),
        .app => |h| {
            // App headers have 'provides' instead of 'exposes'
            // but we need to track the provided functions for export
            try self.createExposedScope(h.provides);
        },
        .type_module => {
            // Type modules don't have an exposes list
            // We'll validate the type name matches the module name after processing types
        },
        .default_app => {
            // Default app modules don't have an exposes list
            // They have a main! function that will be validated
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
                const header_idx = try self.canonicalizeTypeHeader(type_decl.header);
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
                        },
                    },
                    .nominal => Statement{
                        .s_nominal_decl = .{
                            .header = header_idx,
                            .anno = @enumFromInt(0), // placeholder - will be replaced
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
                    break :blk try self.canonicalizeTypeAnno(type_decl.anno, .type_decl_anno);
                };

                // Get type variables to args (lhs)
                const header_arg_vars: []TypeVar = @ptrCast(self.env.store.sliceTypeAnnos(type_header.args));

                // Get type variable to the backing type (rhs)
                const anno_var = ModuleEnv.varFrom(anno_idx);

                // Check if the backing type is already an error type
                const backing_resolved = self.env.types.resolveVar(anno_var);
                const backing_is_error = backing_resolved.desc.content == .err;

                // The identified of the type
                const type_ident = types.TypeIdent{ .ident_idx = type_header.name };

                // Create the real CIR type declaration statement with the canonicalized annotation
                const real_cir_type_decl, const type_decl_content = blk: {
                    switch (type_decl.kind) {
                        .alias => {
                            const alias_content = if (backing_is_error)
                                types.Content{ .err = {} }
                            else
                                try self.env.types.mkAlias(type_ident, anno_var, header_arg_vars);

                            break :blk .{
                                Statement{
                                    .s_alias_decl = .{
                                        .header = header_idx,
                                        .anno = anno_idx,
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
                                    header_arg_vars,
                                    try self.env.insertIdent(base.Ident.for_text(self.env.module_name)),
                                );

                            break :blk .{
                                Statement{
                                    .s_nominal_decl = .{
                                        .header = header_idx,
                                        .anno = anno_idx,
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

                // Update the scope to point to the real statement instead of the placeholder
                try self.scopeUpdateTypeDecl(type_header.name, type_decl_stmt_idx);

                // Remove from exposed_type_texts since the type is now fully defined
                const type_text = self.env.getIdent(type_header.name);
                _ = self.exposed_type_texts.remove(type_text);
            },
            else => {
                // Skip non-type-declaration statements in first pass
            },
        }
    }

    // After processing all type declarations, handle type module validation
    // Validation rules depend on the context (checking, executing, or importing)
    if (header == .type_module) {
        switch (self.validation_context) {
            .checking => {
                // INFER mode - smart error messages, accept both type modules and default-app
                const main_result = self.findMainFunction();
                const has_matching_type_decl = self.hasMatchingType();

                if (main_result == .valid) {
                    // Valid default-app module
                    _ = main_result.valid; // Suppress unused warning
                } else if (main_result == .wrong_arity) {
                    // Report wrong arity error for default-app
                    try self.env.pushDiagnostic(.{
                        .default_app_wrong_arity = .{
                            .arity = main_result.wrong_arity.arity,
                            .region = main_result.wrong_arity.region,
                        },
                    });
                } else if (has_matching_type_decl) {
                    // Valid type module
                } else {
                    // Neither valid - use heuristics for helpful error
                    try self.reportTypeModuleOrDefaultAppError();
                }
            },
            .executing => {
                // EXECUTION mode - require default-app (app header checked elsewhere)
                const main_result = self.findMainFunction();
                if (main_result == .valid) {
                    // Valid default-app
                    _ = main_result.valid; // Suppress unused warning
                } else if (main_result == .wrong_arity) {
                    // Report wrong arity
                    try self.env.pushDiagnostic(.{
                        .default_app_wrong_arity = .{
                            .arity = main_result.wrong_arity.arity,
                            .region = main_result.wrong_arity.region,
                        },
                    });
                } else {
                    // No main! - error, can't execute type module
                    try self.reportExecutionRequiresAppOrDefaultApp();
                }
            },
            .importing => {
                // IMPORT mode - require type module ONLY, no default-app
                const main_result = self.findMainFunction();
                if (main_result != .not_found) {
                    // Has main! - can't import default-app
                    try self.reportCannotImportDefaultApp(main_result);
                } else {
                    // Validate as type module
                    try self.validateTypeModuleName();
                }
            },
            .repl => {
                // REPL mode - no module validation needed, just evaluating expressions
            },
        }
    }

    // Second pass: Process all other statements
    var last_type_anno: ?struct {
        name: base.Ident.Idx,
        anno_idx: TypeAnno.Idx,
        type_vars: DataSpan,
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
                            if (anno_info.name.idx == decl_ident.idx) {
                                // This declaration matches the type annotation
                                const pattern_region = self.parse_ir.tokenizedRegionToRegion(self.parse_ir.store.getPattern(decl.pattern).to_tokenized_region());
                                annotation_idx = try self.createAnnotationFromTypeAnno(anno_info.anno_idx, pattern_region);
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
                // and add the node index to exposed_items
                const pattern = self.parse_ir.store.getPattern(decl.pattern);
                if (pattern == .ident) {
                    const token_region = self.parse_ir.tokens.resolve(@intCast(pattern.ident.ident_tok));
                    const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                    // If this identifier is exposed, add it to exposed_items
                    if (self.exposed_ident_texts.contains(ident_text)) {
                        // Get the interned identifier - it should already exist from parsing
                        const ident = base.Ident.for_text(ident_text);
                        const idx = try self.env.insertIdent(ident);
                        // Store the def index as u16 in exposed_items
                        const def_idx_u16: u16 = @intCast(@intFromEnum(def_idx));
                        try self.env.setExposedNodeIndexById(idx, def_idx_u16);
                    }

                    _ = self.exposed_ident_texts.remove(ident_text);
                }
            },
            .@"var" => |var_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("var");
                const region = self.parse_ir.tokenizedRegionToRegion(var_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .expr => |expr_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("expression");
                const region = self.parse_ir.tokenizedRegionToRegion(expr_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .crash => |crash_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("crash");
                const region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
                last_type_anno = null; // Clear on non-annotation statement
            },
            .dbg => |dbg_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("dbg");
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
                const can_expect = try self.canonicalizeExpr(e.body) orelse {
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
                    .body = can_expect.idx,
                } };
                const expect_stmt_idx = try self.env.addStatementAndTypeVar(expect_stmt, Content{ .flex_var = null }, region);
                try self.env.store.addScratchStatement(expect_stmt_idx);

                last_type_anno = null; // Clear on non-annotation statement
            },
            .@"for" => |for_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("for");
                const region = self.parse_ir.tokenizedRegionToRegion(for_stmt.region);
                try self.env.pushDiagnostic(Diagnostic{ .invalid_top_level_statement = .{
                    .stmt = string_idx,
                    .region = region,
                } });
            },
            .@"return" => |return_stmt| {
                // Not valid at top-level
                const string_idx = try self.env.insertString("return");
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
                const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

                // Top-level type annotation - store for connection to next declaration
                const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                    // Malformed identifier - skip this annotation
                    const feature = try self.env.insertString("handle malformed identifier for a type annotation");
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

                // Now canonicalize the annotation with type variables in scope
                const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .inline_anno);

                // Canonicalize where clauses if present
                const where_clauses = if (ta.where) |where_coll| blk: {
                    const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                    const where_start = self.env.store.scratchWhereClauseTop();

                    for (where_slice) |where_idx| {
                        const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .inline_anno);
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
                    .type_vars = DataSpan.empty(),
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

    // Create the span of exported defs by finding definitions that correspond to exposed items
    try self.populateExports();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    // Freeze the interners after canonicalization is complete
    self.env.freezeInterners();
}

fn collectBoundVars(self: *Self, pattern_idx: Pattern.Idx, bound_vars: *std.AutoHashMapUnmanaged(Pattern.Idx, void)) !void {
    const pattern = self.env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => {
            try bound_vars.put(self.env.gpa, pattern_idx, {});
        },
        .record_destructure => |destructure| {
            for (self.env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectBoundVars(sub_pattern_idx, bound_vars),
                    .SubPattern => |sub_pattern_idx| try self.collectBoundVars(sub_pattern_idx, bound_vars),
                }
            }
        },
        .tuple => |tuple| {
            for (self.env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectBoundVars(elem_pattern_idx, bound_vars);
            }
        },
        .applied_tag => |tag| {
            for (self.env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectBoundVars(arg_pattern_idx, bound_vars);
            }
        },
        .as => |as_pat| {
            try bound_vars.put(self.env.gpa, pattern_idx, {});
            try self.collectBoundVars(as_pat.pattern, bound_vars);
        },
        .list => |list| {
            for (self.env.store.slicePatterns(list.patterns)) |elem_idx| {
                try self.collectBoundVars(elem_idx, bound_vars);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pat_idx| {
                    try self.collectBoundVars(rest_pat_idx, bound_vars);
                }
            }
        },
        .int_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .nominal,
        .nominal_external,
        .runtime_error,
        => {},
    }
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
                // Get the text for tracking redundant exposures
                const token_region = self.parse_ir.tokens.resolve(@intCast(ident.ident));
                const ident_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Get the interned identifier
                if (self.parse_ir.tokens.resolveIdentifier(ident.ident)) |ident_idx| {
                    // Add to exposed_items for permanent storage (unconditionally)
                    try self.env.addExposedById(ident_idx);

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
                // Get the text for tracking redundant exposures
                const token_region = self.parse_ir.tokens.resolve(@intCast(type_name.ident));
                const type_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Get the interned identifier
                if (self.parse_ir.tokens.resolveIdentifier(type_name.ident)) |ident_idx| {
                    // Add to exposed_items for permanent storage (unconditionally)
                    try self.env.addExposedById(ident_idx);

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
                // Get the text for tracking redundant exposures
                const token_region = self.parse_ir.tokens.resolve(@intCast(type_with_constructors.ident));
                const type_text = self.parse_ir.env.source[token_region.start.offset..token_region.end.offset];

                // Get the interned identifier
                if (self.parse_ir.tokens.resolveIdentifier(type_with_constructors.ident)) |ident_idx| {
                    // Add to exposed_items for permanent storage (unconditionally)
                    try self.env.addExposedById(ident_idx);

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

fn populateExports(self: *Self) std.mem.Allocator.Error!void {
    // Start a new scratch space for exports
    const scratch_exports_start = self.env.store.scratchDefTop();

    // Use the already-created all_defs span
    const defs_slice = self.env.store.sliceDefs(self.env.all_defs);

    // Check each definition to see if it corresponds to an exposed item
    for (defs_slice) |def_idx| {
        const def = self.env.store.getDef(def_idx);
        const pattern = self.env.store.getPattern(def.pattern);

        if (pattern == .assign) {
            // Check if this definition's identifier is in the exposed items
            if (self.env.common.exposed_items.containsById(self.env.gpa, @bitCast(pattern.assign.ident))) {
                // Add this definition to the exports scratch space
                try self.env.store.addScratchDef(def_idx);
            }
        }
    }

    // Create the exports span from the scratch space
    self.env.exports = try self.env.store.defSpanFrom(scratch_exports_start);
}

fn checkExposedButNotImplemented(self: *Self) std.mem.Allocator.Error!void {
    // Check for remaining exposed identifiers
    var ident_iter = self.exposed_ident_texts.iterator();
    while (ident_iter.next()) |entry| {
        const ident_text = entry.key_ptr.*;
        const region = entry.value_ptr.*;
        // Create an identifier for error reporting
        const ident_idx = try self.env.insertIdent(base.Ident.for_text(ident_text));

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
        const ident_idx = try self.env.insertIdent(base.Ident.for_text(type_text));

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
                //     .region = ir.env.tag_names.getRegion(imported_type.name),
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
            const feature = try self.env.insertString("resolve import module name token");
            try self.env.pushDiagnostic(Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return null;
        }

        if (import_stmt.qualifier_tok) |qualifier_tok| {
            if (self.parse_ir.tokens.resolveIdentifier(qualifier_tok) == null) {
                const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
                const feature = try self.env.insertString("resolve import qualifier token");
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
                break :blk try self.env.insertIdent(valid_ident);
            } else |err| {
                // Invalid identifier - create diagnostic and use placeholder
                const region = self.parse_ir.tokenizedRegionToRegion(import_stmt.region);
                const error_msg = switch (err) {
                    base.Ident.Error.EmptyText => "malformed import module name is empty",
                    base.Ident.Error.ContainsNullByte => "malformed import module name contains null bytes",
                    base.Ident.Error.ContainsControlCharacters => "malformed import module name contains invalid control characters",
                };
                const feature = try self.env.insertString(error_msg);
                try self.env.pushDiagnostic(Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });

                // Use a placeholder identifier instead
                const placeholder_text = "MALFORMED_IMPORT";
                break :blk try self.env.insertIdent(base.Ident.for_text(placeholder_text));
            }
        } else {
            // No qualifier, just use the module name directly
            break :blk self.parse_ir.tokens.resolveIdentifier(import_stmt.module_name_tok).?;
        }
    };

    // 2. Determine the alias (either explicit or default to last part)
    const alias = try self.resolveModuleAlias(import_stmt.alias_tok, module_name) orelse return null;

    // 3. Get or create Import.Idx for this module
    const module_name_text = self.env.getIdent(module_name);
    const module_import_idx = try self.env.imports.getOrPut(
        self.env.gpa,
        self.env.common.getStringStore(),
        module_name_text,
    );

    // 4. Add to scope: alias -> module_name mapping
    try self.scopeIntroduceModuleAlias(alias, module_name);

    // Process type imports from this module
    try self.processTypeImports(module_name, alias);

    // 4.5. Check if this is a type module and auto-expose main type
    // A module is a type module if it exports a type with the same name as the module
    const main_type_name = blk: {
        if (self.module_envs) |envs_map| {
            if (envs_map.get(module_name_text)) |target_env| {
                // Check if module exports a type with the same name (indicating type module)
                // Extract the expected type name from the module name
                const expected_type_text = if (std.mem.lastIndexOf(u8, module_name_text, ".")) |last_dot|
                    module_name_text[last_dot + 1 ..]
                else
                    module_name_text;

                // Look for this type name in the module's exports
                const target_ident = target_env.common.findIdent(expected_type_text);
                if (target_ident) |type_ident| {
                    if (target_env.containsExposedById(type_ident)) {
                        // This is a type module - use alias for the auto-exposed type
                        break :blk alias;
                    }
                }
            }
        }
        break :blk null;
    };

    // 5. Convert exposed items and introduce them into scope
    const cir_exposes = try self.convertASTExposesToCIR(import_stmt.exposes, main_type_name, module_name);
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

    // 9. Check that this module actually exists, and if not report an error
    if (self.module_envs) |envs_map| {
        // Check if the module exists
        if (!envs_map.contains(module_name_text)) {
            // Module not found - create diagnostic
            try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
                .module_name = module_name,
                .region = import_region,
            } });
        }
    } else {
        try self.env.pushDiagnostic(Diagnostic{ .module_not_found = .{
            .module_name = module_name,
            .region = import_region,
        } });
    }

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
    const module_text = self.env.getIdent(module_name);
    const field_text = self.env.getIdent(field_name);

    // Allocate space for "module.field" - this case still needs allocation since we're combining
    // module name from import with field name from usage site
    const qualified_text = try std.fmt.allocPrint(self.env.gpa, "{s}.{s}", .{ module_text, field_text });
    defer self.env.gpa.free(qualified_text);

    return try self.env.insertIdent(base.Ident.for_text(qualified_text), Region.zero());
}

/// Create an external declaration for a qualified name
fn createExternalDeclaration(
    self: *Self,
    qualified_name: Ident.Idx,
    module_name: Ident.Idx,
    local_name: Ident.Idx,
    kind: @TypeOf(@as(CIR.ExternalDecl, undefined).kind),
    type_var: TypeVar,
    region: Region,
) std.mem.Allocator.Error!CIR.ExternalDecl.Idx {
    const external_decl = CIR.ExternalDecl{
        .qualified_name = qualified_name,
        .module_name = module_name,
        .local_name = local_name,
        .type_var = type_var,
        .kind = kind,
        .region = region,
    };

    return self.env.pushExternalDecl(external_decl);
}

/// Convert AST exposed items to CIR exposed items
/// If main_type_name is provided, auto-inject it as an exposed item
fn convertASTExposesToCIR(
    self: *Self,
    ast_exposes: AST.ExposedItem.Span,
    main_type_name: ?Ident.Idx,
    module_name: Ident.Idx,
) std.mem.Allocator.Error!CIR.ExposedItem.Span {
    const scratch_start = self.env.store.scratchExposedItemTop();

    // Auto-inject main type if this is a type module import
    if (main_type_name) |type_name| {
        const cir_exposed = CIR.ExposedItem{
            .name = type_name,
            .alias = null,
            .is_wildcard = false,
        };
        const cir_exposed_idx = try self.env.addExposedItemAndTypeVar(cir_exposed, .{ .flex_var = null }, Region.zero());
        try self.env.store.addScratchExposedItem(cir_exposed_idx);
    }

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
                    break :resolve_ident try self.env.insertIdent(base.Ident.for_text("unknown"));
                }
            };

            // Check for redundant expose or invalid rename of main type
            if (main_type_name) |type_name| {
                const type_name_text = self.env.getIdent(type_name);
                const item_name_text = self.env.getIdent(name);

                if (std.mem.eql(u8, type_name_text, item_name_text)) {
                    const tokenized_region = switch (ast_exposed) {
                        inline else => |payload| payload.region,
                    };
                    const region = self.parse_ir.tokenizedRegionToRegion(tokenized_region);

                    // Check for invalid rename
                    if (alias_token != null) {
                        const alias = if (self.parse_ir.tokens.resolveIdentifier(alias_token.?)) |resolved|
                            resolved
                        else
                            try self.env.insertIdent(base.Ident.for_text("unknown"));

                        try self.env.pushDiagnostic(.{
                            .invalid_main_type_rename_in_exposing = .{
                                .type_name = name,
                                .alias = alias,
                                .region = region,
                            },
                        });
                        continue; // Skip adding this to exposed items
                    }

                    // Redundant expose - warn and skip adding duplicate
                    try self.env.pushDiagnostic(.{
                        .redundant_expose_main_type = .{
                            .type_name = name,
                            .module_name = module_name,
                            .region = region,
                        },
                    });
                    continue; // Skip adding duplicate
                }
            }

            // Resolve the alias if present
            const alias = resolve_alias: {
                if (alias_token) |as_token| {
                    if (self.parse_ir.tokens.resolveIdentifier(as_token)) |resolved| {
                        break :resolve_alias resolved;
                    } else {
                        break :resolve_alias try self.env.insertIdent(base.Ident.for_text("unknown"));
                    }
                } else {
                    break :resolve_alias null;
                }
            };

            break :convert_item CIR.ExposedItem{
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
    exposed_items_span: CIR.ExposedItem.Span,
    module_name: Ident.Idx,
    import_region: Region,
) std.mem.Allocator.Error!void {
    const exposed_items_slice = self.env.store.sliceExposedItems(exposed_items_span);

    // If we have module_envs, validate the imports
    if (self.module_envs) |envs_map| {
        const module_name_text = self.env.getIdent(module_name);

        // Check if the module exists
        if (!envs_map.contains(module_name_text)) {
            // Module not found - Module existence check is already done in canonicalizeImportStatement,
            // so there is no need to create another diagnostic here for module_not_found
            return;
        }

        // Get the module's exposed_items
        const module_env = envs_map.get(module_name_text).?;

        // Validate each exposed item
        for (exposed_items_slice) |exposed_item_idx| {
            const exposed_item = self.env.store.getExposedItem(exposed_item_idx);
            const item_name_text = self.env.getIdent(exposed_item.name);

            // Check if the item is exposed by the module
            // We need to look up by string because the identifiers are from different modules
            // First, try to find this identifier in the target module's ident store
            const is_exposed = if (module_env.common.findIdent(item_name_text)) |target_ident|
                module_env.containsExposedById(target_ident)
            else
                false;

            if (!is_exposed) {
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
) std.mem.Allocator.Error!CIR.Def.Idx {
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

    const can_expr = blk: {
        if (try self.canonicalizeExpr(decl.body)) |ce| {
            break :blk ce;
        } else {
            const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = expr_region,
            } });
            break :blk CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
        }
    };
    const expr_idx = can_expr.idx;

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

fn canonicalizeStringLike(
    self: *Self,
    e: anytype,
    is_multiline: bool,
) std.mem.Allocator.Error!CanonicalizedExpr {
    // Get all the string parts
    const parts = self.parse_ir.store.exprSlice(e.parts);

    // Extract segments from the string, inserting them into the string interner
    // For non-string interpolation segments, canonicalize them
    //
    // Returns a Expr.Span containing the canonicalized string segments
    // a string may consist of multiple string literal or expression segments
    const free_vars_start = self.scratch_free_vars.top();
    const can_str_span = if (is_multiline)
        try self.extractMultilineStringSegments(parts)
    else
        try self.extractStringSegments(parts);

    const region = self.parse_ir.tokenizedRegionToRegion(e.region);
    const expr_idx = try self.env.addExprAndTypeVar(Expr{ .e_str = .{
        .span = can_str_span,
    } }, Content{ .structure = .str }, region);

    const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
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
        const value_content = CIR.IntValue{
            .bytes = @bitCast(@as(u128, @intCast(codepoint))),
            .kind = .u128,
        };
        if (Idx == Expr.Idx) {
            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
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
    const can_value = if (field.value) |v|
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
        .value = can_value.idx,
    };

    return try self.env.addRecordFieldAndTypeVar(cir_field, Content{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(field.region));
}

/// Parse an integer with underscores.
pub fn parseIntWithUnderscores(comptime T: type, text: []const u8, int_base: u8) !T {
    var buf: [128]u8 = undefined;
    var len: usize = 0;
    for (text) |char| {
        if (char != '_') {
            if (len >= buf.len) return error.Overflow;
            buf[len] = char;
            len += 1;
        }
    }
    return std.fmt.parseInt(T, buf[0..len], int_base);
}

/// Canonicalize an expression.
pub fn canonicalizeExpr(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!?CanonicalizedExpr {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Assert that everything is in-sync
    self.env.debugAssertArraysInSync();

    const expr = self.parse_ir.store.getExpr(ast_expr_idx);
    switch (expr) {
        .apply => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Check if the function being applied is a tag
            const ast_fn = self.parse_ir.store.getExpr(e.@"fn");
            if (ast_fn == .tag) {
                // This is a tag application, not a function call
                const tag_expr = ast_fn.tag;
                const can_expr = try self.canonicalizeTagExpr(tag_expr, e.args, region);
                return can_expr;
            }

            // Not a tag application, proceed with normal function call
            // Mark the start of scratch expressions
            const free_vars_start = self.scratch_free_vars.top();
            const scratch_top = self.env.store.scratchExprTop();

            // Canonicalize the function being called and add as first element
            const can_fn_expr = try self.canonicalizeExpr(e.@"fn") orelse {
                self.env.store.clearScratchExprsFrom(scratch_top);
                return null;
            };
            try self.env.store.addScratchExpr(can_fn_expr.idx);

            // Canonicalize and add all arguments
            const args_slice = self.parse_ir.store.exprSlice(e.args);
            for (args_slice) |arg| {
                if (try self.canonicalizeExpr(arg)) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }

            // Create span from scratch expressions
            const args_span = try self.env.store.exprSpanFrom(scratch_top);

            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                .e_call = .{
                    .args = args_span,
                    .called_via = CalledVia.apply,
                },
            }, Content{ .flex_var = null }, region);

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
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
                            const module_text = self.env.getIdent(module_name);

                            // Check if this module is imported in the current scope
                            const import_idx = self.scopeLookupImportedModule(module_text) orelse {
                                // Module not imported in current scope
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                                        .module_name = module_name,
                                        .region = region,
                                    } }),
                                    .free_vars = null,
                                };
                            };

                            // Look up the target node index in the module's exposed_items
                            // Need to convert identifier from current module to target module
                            const field_text = self.env.getIdent(ident);
                            const target_node_idx = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(module_text)) |module_env| {
                                    if (module_env.common.findIdent(field_text)) |target_ident| {
                                        break :blk module_env.getExposedNodeIndexById(target_ident) orelse 0;
                                    } else {
                                        break :blk 0;
                                    }
                                } else {
                                    break :blk 0;
                                }
                            } else 0;

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .region = region,
                            } }, Content{ .flex_var = null }, region);
                            return CanonicalizedExpr{
                                .idx = expr_idx,
                                .free_vars = null,
                            };
                        }
                    }
                }

                // Not a module-qualified lookup, or qualifier not found, proceed with normal lookup
                switch (self.scopeLookup(.ident, ident)) {
                    .found => |pattern_idx| {
                        // Mark this pattern as used for unused variable checking
                        try self.used_patterns.put(self.env.gpa, pattern_idx, {});

                        // Check if this is a used underscore variable
                        try self.checkUsedUnderscoreVariable(ident, region);

                        // We found the ident in scope, lookup to reference the pattern
                        const expr_idx = try self.env.addExprAndTypeVarRedirect(CIR.Expr{ .e_lookup_local = .{
                            .pattern_idx = pattern_idx,
                        } }, ModuleEnv.varFrom(pattern_idx), region);

                        const free_vars_start = self.scratch_free_vars.top();
                        try self.scratch_free_vars.append(self.env.gpa, pattern_idx);
                        const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
                    },
                    .not_found => {
                        // Check if this identifier is an exposed item from an import
                        if (self.scopeLookupExposedItem(ident)) |exposed_info| {
                            // Get the Import.Idx for the module this item comes from
                            const module_text = self.env.getIdent(exposed_info.module_name);
                            const import_idx = self.scopeLookupImportedModule(module_text) orelse {
                                // This shouldn't happen if imports are properly tracked, but handle it gracefully
                                return CanonicalizedExpr{
                                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                                        .module_name = exposed_info.module_name,
                                        .region = region,
                                    } }),
                                    .free_vars = null,
                                };
                            };

                            // Look up the target node index in the module's exposed_items
                            // Need to convert identifier from current module to target module
                            const field_text = self.env.getIdent(exposed_info.original_name);
                            const target_node_idx = if (self.module_envs) |envs_map| blk: {
                                if (envs_map.get(module_text)) |module_env| {
                                    if (module_env.common.findIdent(field_text)) |target_ident| {
                                        break :blk module_env.getExposedNodeIndexById(target_ident) orelse 0;
                                    } else {
                                        break :blk 0;
                                    }
                                } else {
                                    break :blk 0;
                                }
                            } else 0;

                            // Create the e_lookup_external expression with Import.Idx
                            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{ .e_lookup_external = .{
                                .module_idx = import_idx,
                                .target_node_idx = target_node_idx,
                                .region = region,
                            } }, Content{ .flex_var = null }, region);
                            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                        }

                        // We did not find the ident in scope or as an exposed item
                        return CanonicalizedExpr{
                            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .ident_not_in_scope = .{
                                .ident = ident,
                                .region = region,
                            } }),
                            .free_vars = null,
                        };
                    },
                }
            } else {
                const feature = try self.env.insertString("report an error when unable to resolve identifier");
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } }),
                    .free_vars = null,
                };
            }
        },
        .int => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const token_text = self.parse_ir.resolve(e.token);
            const parsed = types.Num.parseNumLiteralWithSuffix(token_text);

            // Parse the integer value
            const is_negated = parsed.num_text[0] == '-';
            const after_minus_sign = @as(usize, @intFromBool(is_negated));

            var first_digit: usize = undefined;
            const DEFAULT_BASE = 10;
            var int_base: u8 = undefined;

            if (parsed.num_text[after_minus_sign] == '0' and parsed.num_text.len > after_minus_sign + 2) {
                switch (parsed.num_text[after_minus_sign + 1]) {
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

            const digit_part = parsed.num_text[first_digit..];

            const u128_val = parseIntWithUnderscores(u128, digit_part, int_base) catch {
                // Any number literal that is too large for u128 is invalid, regardless of whether it had a minus sign!
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            };

            // If this had a minus sign, but negating it would result in a negative number
            // that would be too low to fit in i128, then this int literal is also invalid.
            if (is_negated and u128_val > min_i128_negated) {
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
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
            const i128_val: i128 = if (is_negated) blk: {
                if (u128_val == min_i128_negated) {
                    break :blk @as(i128, @bitCast(u128_val));
                } else {
                    break :blk -@as(i128, @bitCast(u128_val));
                }
            } else @as(i128, @bitCast(u128_val));

            // Calculate requirements based on the value
            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            if (parsed.suffix) |suffix| {
                const type_content = blk: {
                    if (std.mem.eql(u8, suffix, "u8")) {
                        if (u128_val > std.math.maxInt(u8)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u8 } };
                    } else if (std.mem.eql(u8, suffix, "u16")) {
                        if (u128_val > std.math.maxInt(u16)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u16 } };
                    } else if (std.mem.eql(u8, suffix, "u32")) {
                        if (u128_val > std.math.maxInt(u32)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u32 } };
                    } else if (std.mem.eql(u8, suffix, "u64")) {
                        if (u128_val > std.math.maxInt(u64)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u64 } };
                    } else if (std.mem.eql(u8, suffix, "u128")) {
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u128 } };
                    } else if (std.mem.eql(u8, suffix, "i8")) {
                        if (i128_val < std.math.minInt(i8) or i128_val > std.math.maxInt(i8)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i8 } };
                    } else if (std.mem.eql(u8, suffix, "i16")) {
                        if (i128_val < std.math.minInt(i16) or i128_val > std.math.maxInt(i16)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i16 } };
                    } else if (std.mem.eql(u8, suffix, "i32")) {
                        if (i128_val < std.math.minInt(i32) or i128_val > std.math.maxInt(i32)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i32 } };
                    } else if (std.mem.eql(u8, suffix, "i64")) {
                        if (i128_val < std.math.minInt(i64) or i128_val > std.math.maxInt(i64)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i64 } };
                    } else if (std.mem.eql(u8, suffix, "i128")) {
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i128 } };
                    } else {
                        break :blk null;
                    }
                };

                if (type_content) |content| {
                    const expr_idx = try self.env.addExprAndTypeVar(
                        .{ .e_int = .{ .value = .{ .bytes = @bitCast(i128_val), .kind = .i128 } } },
                        content,
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                }
            }

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

            const expr_idx = try self.env.addExprAndTypeVar(
                CIR.Expr{ .e_int = .{ .value = CIR.IntValue{
                    .bytes = @bitCast(i128_val),
                    .kind = .i128,
                } } },
                type_content,
                region,
            );
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.token);
            const parsed_num = types.Num.parseNumLiteralWithSuffix(token_text);

            if (parsed_num.suffix) |suffix| {
                const f64_val = std.fmt.parseFloat(f64, parsed_num.num_text) catch {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                };

                if (std.mem.eql(u8, suffix, "f32")) {
                    if (!fitsInF32(f64_val)) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                    }
                    const expr_idx = try self.env.addExprAndTypeVar(
                        .{ .e_frac_f32 = .{ .value = @floatCast(f64_val) } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .f32 } } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                } else if (std.mem.eql(u8, suffix, "f64")) {
                    const expr_idx = try self.env.addExprAndTypeVar(
                        .{ .e_frac_f64 = .{ .value = f64_val } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .f64 } } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                } else if (std.mem.eql(u8, suffix, "dec")) {
                    if (!fitsInDec(f64_val)) {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                    }
                    const dec_val = RocDec.fromF64(f64_val) orelse {
                        const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                    };
                    const expr_idx = try self.env.addExprAndTypeVar(
                        .{ .e_frac_dec = .{ .value = dec_val } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .dec } } },
                        region,
                    );
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                }
            }

            const parsed = parseFracLiteral(token_text) catch |err| switch (err) {
                error.InvalidNumLiteral => {
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_num_literal = .{
                        .region = region,
                    } });
                    return CanonicalizedExpr{
                        .idx = expr_idx,
                        .free_vars = null,
                    };
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
                .small => |small_info| CIR.Expr{
                    .e_dec_small = .{
                        .numerator = small_info.numerator,
                        .denominator_power_of_ten = small_info.denominator_power_of_ten,
                    },
                },
                .dec => |dec_info| CIR.Expr{
                    .e_frac_dec = .{
                        .value = dec_info.value,
                    },
                },
                .f64 => |f64_info| CIR.Expr{
                    .e_frac_f64 = .{
                        .value = f64_info.value,
                    },
                },
            };

            const expr_idx = try self.env.addExprAndTypeVar(cir_expr, Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } }, region);

            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .single_quote => |e| {
            const expr_idx = try self.canonicalizeSingleQuote(e.region, e.token, Expr.Idx) orelse return null;
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .string => |e| {
            return try self.canonicalizeStringLike(e, false);
        },
        .multiline_string => |e| {
            return try self.canonicalizeStringLike(e, true);
        },
        .list => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Empty lists get the .list_unbound type
            const items_slice = self.parse_ir.store.exprSlice(e.items);
            if (items_slice.len == 0) {
                // Empty list - use e_empty_list
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_empty_list = .{},
                }, Content{ .structure = .list_unbound }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            }

            // Mark the start of scratch expressions for the list
            const free_vars_start = self.scratch_free_vars.top();
            const scratch_top = self.env.store.scratchExprTop();

            // Iterate over the list item, canonicalizing each one
            // Then append the result to the scratch list
            for (items_slice) |item| {
                if (try self.canonicalizeExpr(item)) |can_item| {
                    try self.env.store.addScratchExpr(can_item.idx);
                }
            }

            // Create span of the new scratch expressions
            const elems_span = try self.env.store.exprSpanFrom(scratch_top);

            // If all elements failed to canonicalize, treat as empty list
            if (elems_span.span.len == 0) {
                // All elements failed to canonicalize - create empty list
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_empty_list = .{},
                }, Content{ .structure = .list_unbound }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            }

            // Initialize the list's type variable to its first element's CIR Index
            // (later steps will unify that type with the other elems' types)
            const first_elem_idx = self.env.store.sliceExpr(elems_span)[0];
            const elem_type_var = @as(TypeVar, @enumFromInt(@intFromEnum(first_elem_idx)));
            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                .e_list = .{
                    .elem_var = elem_type_var,
                    .elems = elems_span,
                },
            }, Content{ .structure = .{ .list = elem_type_var } }, region);

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
        },
        .tag => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            return self.canonicalizeTagExpr(e, null, region);
        },
        .string_part => |_| {
            const feature = try self.env.insertString("canonicalize string_part expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .tuple => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Get the list of tuple elems
            const items_slice = self.parse_ir.store.exprSlice(e.items);

            if (items_slice.len == 0) {
                const ast_body = self.parse_ir.store.getExpr(ast_expr_idx);
                const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .empty_tuple = .{ .region = body_region },
                });
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            } else if (items_slice.len == 1) {
                // 1-elem tuple == parenthesized expr

                // NOTE: Returning the sub-expr like this breaks 1-to-1 AST to
                // CIR node mapping. However, this is already broken due to how
                // we insert placeholder type var nodes in other places. So for
                // now, this is fine
                return self.canonicalizeExpr(items_slice[0]);
            } else {
                // Mark the start of scratch expressions for the tuple
                const free_vars_start = self.scratch_free_vars.top();
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
                            break :blk CanonicalizedExpr{
                                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                                    .tuple_elem_not_canonicalized = .{ .region = body_region },
                                }),
                                .free_vars = null,
                            };
                        }
                    };

                    try self.env.store.addScratchExpr(item_expr_idx.get_idx());
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
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_tuple = .{
                        .elems = elems_span,
                    },
                }, Content{ .structure = FlatType{
                    .tuple = types.Tuple{ .elems = elems_var_range },
                } }, region);

                const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
            }
        },
        .record => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize extension if present
            const free_vars_start = self.scratch_free_vars.top();
            var ext_expr: ?Expr.Idx = null;
            if (e.ext) |ext_ast_idx| {
                if (try self.canonicalizeExpr(ext_ast_idx)) |can_ext| {
                    ext_expr = can_ext.idx;
                }
            }

            const fields_slice = self.parse_ir.store.recordFieldSlice(e.fields);
            if (fields_slice.len == 0) {
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_empty_record = .{},
                }, Content{ .structure = .empty_record }, region);

                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
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
                        if (field_name_ident.idx == seen_field.ident.idx) {
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
                        if (try self.canonicalizeRecordField(field)) |can_field_idx| {
                            try self.env.store.scratch_record_fields.append(self.env.gpa, can_field_idx);
                        }
                    } else {
                        // TODO: Add diagnostic on duplicate record field
                    }
                } else {
                    // Field name couldn't be resolved, still try to canonicalize
                    if (try self.canonicalizeRecordField(field)) |can_field_idx| {
                        try self.env.store.scratch_record_fields.append(self.env.gpa, can_field_idx);
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

            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                .e_record = .{
                    .fields = fields_span,
                    .ext = ext_expr,
                },
            }, Content{ .structure = .{ .record_unbound = type_fields_range } }, region);

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
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

            // body (this will detect and record captures)
            const body_free_vars_start = self.scratch_free_vars.top();
            const can_body = try self.canonicalizeExpr(e.body) orelse {
                self.scratch_free_vars.clearFrom(body_free_vars_start);
                const ast_body = self.parse_ir.store.getExpr(e.body);
                const body_region = self.parse_ir.tokenizedRegionToRegion(ast_body.to_tokenized_region());
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                    .lambda_body_not_canonicalized = .{ .region = body_region },
                });
                return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
            };

            // Determine captures: free variables in body minus variables bound by args
            var bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
            defer bound_vars.deinit(self.env.gpa);
            for (self.env.store.slicePatterns(args_span)) |arg_pat_idx| {
                try self.collectBoundVars(arg_pat_idx, &bound_vars);
            }

            var captures_set = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
            defer captures_set.deinit(self.env.gpa);

            const body_free_vars_slice = can_body.free_vars orelse &.{};
            for (body_free_vars_slice) |fv| {
                if (!bound_vars.contains(fv)) {
                    try captures_set.put(self.env.gpa, fv, {});
                }
            }

            // Now that we have the captures, we can clear the free variables from the body
            // from the scratch buffer.
            self.scratch_free_vars.clearFrom(body_free_vars_start);

            // Create the pure lambda expression first
            const lambda_expr = Expr{
                .e_lambda = .{
                    .args = args_span,
                    .body = can_body.idx,
                },
            };
            const lambda_type_content = try self.env.types.mkFuncUnbound(
                @ptrCast(self.env.store.slicePatterns(args_span)),
                ModuleEnv.varFrom(can_body.idx),
            );
            const lambda_idx = try self.env.addExprAndTypeVar(lambda_expr, lambda_type_content, region);

            // If there are no captures, this is a pure lambda.
            // Otherwise, it's a closure.
            if (captures_set.count() == 0) {
                // A pure lambda has no free variables.
                return CanonicalizedExpr{ .idx = lambda_idx, .free_vars = null };
            }

            const capture_info: Expr.Capture.Span = blk: {
                const scratch_start = self.env.store.scratch_captures.top();
                var cap_it = captures_set.iterator();
                while (cap_it.next()) |entry| {
                    const pattern_idx = entry.key_ptr.*;
                    const pattern = self.env.store.getPattern(pattern_idx);
                    const name = switch (pattern) {
                        .assign => |a| a.ident,
                        else => unreachable, // Should only capture simple idents
                    };
                    const capture = Expr.Capture{
                        .name = name,
                        .pattern_idx = pattern_idx,
                        .scope_depth = 0, // This is now unused, but kept for struct compatibility.
                    };
                    const capture_idx = try self.env.addCaptureAndTypeVar(capture, types.Content{ .flex_var = null }, region);
                    try self.env.store.addScratchCapture(capture_idx);
                }

                break :blk try self.env.store.capturesSpanFrom(scratch_start);
            };

            // Now, create the closure that captures the environment
            const closure_expr = Expr{
                .e_closure = .{
                    .lambda_idx = lambda_idx,
                    .captures = capture_info,
                },
            };

            // The type of the closure is the same as the type of the pure lambda
            const expr_idx = try self.env.addExprAndTypeVar(closure_expr, lambda_type_content, region);

            // The free variables of the lambda are its captures.
            // I need to add them to the global list and return a span.
            const lambda_free_vars_start = self.scratch_free_vars.top();
            var cap_it = captures_set.iterator();
            while (cap_it.next()) |entry| {
                try self.scratch_free_vars.append(self.env.gpa, entry.key_ptr.*);
            }
            const free_vars_slice = self.scratch_free_vars.slice(lambda_free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
        },
        .record_updater => |_| {
            const feature = try self.env.insertString("canonicalize record_updater expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .field_access => |field_access| {
            // Try module-qualified lookup first (e.g., Json.utf8)
            if (try self.tryModuleQualifiedLookup(field_access)) |expr_idx| {
                return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            }

            // Regular field access canonicalization
            return CanonicalizedExpr{
                .idx = (try self.canonicalizeRegularFieldAccess(field_access)) orelse return null,
                .free_vars = null,
            };
        },
        .local_dispatch => |_| {
            const feature = try self.env.insertString("canonicalize local_dispatch expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .bin_op => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            const free_vars_start = self.scratch_free_vars.top();
            // Canonicalize left and right operands
            const can_lhs = try self.canonicalizeExpr(e.left) orelse return null;
            const can_rhs = try self.canonicalizeExpr(e.right) orelse return null;

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
                    const feature = try self.env.insertString("binop");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                },
            };

            const expr_idx = try self.env.addExprAndTypeVar(Expr{
                .e_binop = Expr.Binop.init(op, can_lhs.idx, can_rhs.idx),
            }, Content{ .flex_var = null }, region);

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
        },
        .suffix_single_question => |_| {
            const feature = try self.env.insertString("canonicalize suffix_single_question expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .unary_op => |unary| {
            const region = self.parse_ir.tokenizedRegionToRegion(unary.region);
            const operator_token = self.parse_ir.tokens.tokens.get(unary.operator);

            switch (operator_token.tag) {
                .OpUnaryMinus => {
                    // Canonicalize the operand expression
                    const can_operand = (try self.canonicalizeExpr(unary.expr)) orelse return null;

                    // Create unary minus CIR expression
                    const expr_idx = try self.env.addExprAndTypeVar(Expr{
                        .e_unary_minus = Expr.UnaryMinus.init(can_operand.idx),
                    }, Content{ .flex_var = null }, region);

                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars };
                },
                .OpBang => {
                    // Canonicalize the operand expression
                    const can_operand = (try self.canonicalizeExpr(unary.expr)) orelse return null;

                    // Create unary not CIR expression
                    const expr_idx = try self.env.addExprAndTypeVar(Expr{
                        .e_unary_not = Expr.UnaryNot.init(can_operand.idx),
                    }, Content{ .flex_var = null }, region);

                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = can_operand.free_vars };
                },
                else => {
                    // Other operators not yet implemented or malformed
                    const feature = try self.env.insertString("canonicalize unary_op expression (non-minus)");
                    const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } });
                    return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
                },
            }
        },
        .if_then_else => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            const free_vars_start = self.scratch_free_vars.top();

            // Start collecting if-branches
            const scratch_top = self.env.store.scratchIfBranchTop();

            var current_if = e;
            var final_else: Expr.Idx = undefined;

            while (true) {
                // Canonicalize and add the current condition/then pair
                const can_cond = try self.canonicalizeExpr(current_if.condition) orelse {
                    const ast_cond = self.parse_ir.store.getExpr(current_if.condition);
                    const cond_region = self.parse_ir.tokenizedRegionToRegion(ast_cond.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .if_condition_not_canonicalized = .{ .region = cond_region },
                    });
                    // In case of error, we can't continue, so we just return a malformed expression for the whole if-else chain
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
                };

                const can_then = try self.canonicalizeExpr(current_if.then) orelse {
                    const ast_then = self.parse_ir.store.getExpr(current_if.then);
                    const then_region = self.parse_ir.tokenizedRegionToRegion(ast_then.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                        .if_then_not_canonicalized = .{ .region = then_region },
                    });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
                };

                // Add this condition/then pair as an if-branch
                const if_branch = Expr.IfBranch{
                    .cond = can_cond.idx,
                    .body = can_then.idx,
                };
                const if_branch_idx = try self.env.addIfBranchAndTypeVar(if_branch, Content{ .flex_var = null }, self.parse_ir.tokenizedRegionToRegion(current_if.region));
                try self.env.store.addScratchIfBranch(if_branch_idx);

                // Check if the else clause is another if-then-else
                const else_expr = self.parse_ir.store.getExpr(current_if.@"else");
                if (else_expr == .if_then_else) {
                    current_if = else_expr.if_then_else;
                } else {
                    // This is the final else
                    const can_else = try self.canonicalizeExpr(current_if.@"else") orelse {
                        const else_region = self.parse_ir.tokenizedRegionToRegion(else_expr.to_tokenized_region());
                        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{
                            .if_else_not_canonicalized = .{ .region = else_region },
                        });
                        return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
                    };
                    final_else = can_else.idx;
                    break;
                }
            }

            const branches_span = try self.env.store.ifBranchSpanFrom(scratch_top);

            // Get the first branch's body to redirect to it
            const branches = self.env.store.sliceIfBranches(branches_span);
            std.debug.assert(branches.len > 0);

            // Create the if expression with flex var initially
            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
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

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
        },
        .match => |m| {
            const region = self.parse_ir.tokenizedRegionToRegion(m.region);

            const free_vars_start = self.scratch_free_vars.top();
            // Canonicalize the condition expression
            const can_cond = try self.canonicalizeExpr(m.expr) orelse return null;

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
                const can_body = try self.canonicalizeExpr(ast_branch.body) orelse {
                    const body = self.parse_ir.store.getExpr(ast_branch.body);
                    const body_region = self.parse_ir.tokenizedRegionToRegion(body.to_tokenized_region());
                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                        .region = body_region,
                    } });
                    return CanonicalizedExpr{ .idx = malformed_idx, .free_vars = null };
                };
                const value_idx = can_body.idx;

                // Get the body region from the AST node
                const body = self.parse_ir.store.getExpr(ast_branch.body);
                const body_region = self.parse_ir.tokenizedRegionToRegion(body.to_tokenized_region());

                const branch_idx = try self.env.addMatchBranchAndTypeVar(
                    Expr.Match.Branch{
                        .patterns = branch_pat_span,
                        .value = value_idx,
                        .guard = null,
                        .redundant = @enumFromInt(0), // TODO
                    },
                    Content{ .flex_var = null },
                    body_region,
                );

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
                .cond = can_cond.idx,
                .branches = branches_span,
                .exhaustive = @enumFromInt(0), // Will be set during type checking
            };

            // Create initial content for the match expression
            const initial_content = if (mb_branch_var) |_| Content{ .flex_var = null } else Content{ .err = {} };
            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{ .e_match = match_expr }, initial_content, region);

            // If there is at least 1 branch, then set the root expr to redirect
            // to the type of the match branch
            const expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(expr_idx)));
            if (mb_branch_var) |branch_var| {
                try self.env.types.setVarRedirect(expr_var, branch_var);
            }

            const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null };
        },
        .dbg => |d| {
            // Debug expression - canonicalize the inner expression
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);
            const can_inner = try self.canonicalizeExpr(d.expr) orelse return null;

            // Create debug expression
            const dbg_expr = try self.env.addExprAndTypeVar(Expr{ .e_dbg = .{
                .expr = can_inner.idx,
            } }, Content{ .flex_var = null }, region);

            return CanonicalizedExpr{ .idx = dbg_expr, .free_vars = can_inner.free_vars };
        },
        .record_builder => |_| {
            const feature = try self.env.insertString("canonicalize record_builder expression");
            const expr_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        },
        .ellipsis => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);
            const ellipsis_expr = try self.env.addExprAndTypeVar(Expr{ .e_ellipsis = .{} }, Content{ .flex_var = null }, region);
            return CanonicalizedExpr{ .idx = ellipsis_expr, .free_vars = null };
        },
        .block => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            var last_type_anno: ?StmtTypeAnno = null;

            // Blocks don't introduce function boundaries, but may contain var statements
            try self.scopeEnter(self.env.gpa, false); // false = not a function boundary
            defer self.scopeExit(self.env.gpa) catch {};

            // Keep track of the start position for statements
            const stmt_start = self.env.store.scratch_statements.top();

            // TODO Use a temporary scratch space for the block's free variables
            //
            // I apologize for leaving these AutoHashMapUnmanaged's here ... but it's a workaround
            // to land a working closure capture implementation, and we can optimize this later. Forgive me.
            var bound_vars = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
            defer bound_vars.deinit(self.env.gpa);

            var captures = std.AutoHashMapUnmanaged(Pattern.Idx, void){};
            defer captures.deinit(self.env.gpa);

            // Canonicalize all statements in the block
            const statements = self.parse_ir.store.statementSlice(e.statements);
            var last_expr: ?CanonicalizedExpr = null;

            for (statements, 0..) |stmt_idx, i| {
                // Check if this is the last statement and if it's an expression
                const is_last = (i == statements.len - 1);
                const stmt = self.parse_ir.store.getStatement(stmt_idx);

                if (is_last and (stmt == .expr or stmt == .dbg or stmt == .@"return" or stmt == .crash)) {
                    // For the last expression or debug statement, canonicalize it directly as the final expression
                    // without adding it as a statement
                    switch (stmt) {
                        .expr => |expr_stmt| last_expr = try self.canonicalizeExprOrMalformed(expr_stmt.expr),
                        .dbg => |dbg_stmt| {
                            // For final debug statements, canonicalize as debug expression
                            const debug_region = self.parse_ir.tokenizedRegionToRegion(dbg_stmt.region);
                            const inner_expr = try self.canonicalizeExprOrMalformed(dbg_stmt.expr);

                            // Create debug expression
                            const dbg_expr = try self.env.addExprAndTypeVarRedirect(Expr{ .e_dbg = .{
                                .expr = inner_expr.idx,
                            } }, ModuleEnv.varFrom(inner_expr.idx), debug_region);
                            last_expr = CanonicalizedExpr{ .idx = dbg_expr, .free_vars = inner_expr.free_vars };
                        },
                        .@"return" => |return_stmt| last_expr = try self.canonicalizeExprOrMalformed(return_stmt.expr),
                        .crash => |crash_stmt| {
                            // For final debug statements, canonicalize as debug expression
                            const crash_region = self.parse_ir.tokenizedRegionToRegion(crash_stmt.region);

                            // Create crash expression
                            // Extract string content from the crash expression or create malformed if not string
                            const crash_expr = blk: {
                                const msg_expr = self.parse_ir.store.getExpr(crash_stmt.expr);
                                switch (msg_expr) {
                                    .string => |s| {
                                        // For string literals, we need to extract the actual string parts
                                        const parts = self.parse_ir.store.exprSlice(s.parts);
                                        if (parts.len > 0) {
                                            const first_part = self.parse_ir.store.getExpr(parts[0]);
                                            if (first_part == .string_part) {
                                                const part_text = self.parse_ir.resolve(first_part.string_part.token);
                                                break :blk try self.env.addExprAndTypeVar(Expr{ .e_crash = .{
                                                    .msg = try self.env.insertString(part_text),
                                                } }, .{ .flex_var = null }, crash_region);
                                            }
                                        }
                                        // Fall back to default if we can't extract
                                        break :blk try self.env.addExprAndTypeVar(Expr{ .e_crash = .{
                                            .msg = try self.env.insertString("crash"),
                                        } }, .{ .flex_var = null }, crash_region);
                                    },
                                    else => {
                                        // For non-string expressions, create a malformed expression
                                        break :blk try self.env.pushMalformed(Expr.Idx, Diagnostic{ .crash_expects_string = .{
                                            .region = region,
                                        } });
                                    },
                                }
                            };

                            last_expr = CanonicalizedExpr{ .idx = crash_expr, .free_vars = null };
                        },
                        else => unreachable,
                    }
                } else {
                    // This is a regular statement within the block
                    const can_stmt_result = try self.canonicalizeStatement(stmt_idx, &last_type_anno);
                    switch (can_stmt_result) {
                        .import_stmt => {
                            // After we process import statements, there's no
                            // need to include then in the canonicalize IR
                        },
                        .stmt => |can_stmt| {
                            try self.env.store.addScratchStatement(can_stmt.idx);

                            const cir_stmt = self.env.store.getStatement(can_stmt.idx);
                            switch (cir_stmt) {
                                .s_decl => |decl| try self.collectBoundVars(decl.pattern, &bound_vars),
                                .s_var => |var_stmt| try self.collectBoundVars(var_stmt.pattern_idx, &bound_vars),
                                else => {},
                            }

                            // Collect free vars from the statement into the block's scratch space
                            if (can_stmt.free_vars) |fvs| {
                                for (fvs) |fv| {
                                    if (!bound_vars.contains(fv)) {
                                        try captures.put(self.env.gpa, fv, {});
                                    }
                                }
                            }
                        },
                    }
                }
            }

            // Determine the final expression
            const final_expr = if (last_expr) |can_expr| can_expr else blk: {
                // Empty block - create empty record
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_empty_record = .{},
                }, Content{ .structure = .empty_record }, region);
                break :blk CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
            };
            const final_expr_var = @as(TypeVar, @enumFromInt(@intFromEnum(final_expr.idx)));

            // Add free vars from the final expression to the block's scratch space
            if (final_expr.free_vars) |fvs| {
                for (fvs) |fv| {
                    if (!bound_vars.contains(fv)) {
                        try captures.put(self.env.gpa, fv, {});
                    }
                }
            }

            // Add the actual free variables (captures) to the parent's scratch space
            const captures_start = self.scratch_free_vars.top();
            var cap_it = captures.iterator();
            while (cap_it.next()) |entry| {
                try self.scratch_free_vars.append(self.env.gpa, entry.key_ptr.*);
            }
            const captures_slice = self.scratch_free_vars.slice(captures_start, self.scratch_free_vars.top());

            // Create statement span
            const stmt_span = try self.env.store.statementSpanFrom(stmt_start);

            // Create and return block expression
            const block_expr = CIR.Expr{
                .e_block = .{
                    .stmts = stmt_span,
                    .final_expr = final_expr.idx,
                },
            };
            const block_idx = try self.env.addExprAndTypeVar(block_expr, Content{ .flex_var = null }, region);
            const block_var = @as(TypeVar, @enumFromInt(@intFromEnum(block_idx)));

            // Set the root block expr to redirect to the final expr var
            try self.env.types.setVarRedirect(block_var, final_expr_var);

            return CanonicalizedExpr{ .idx = block_idx, .free_vars = if (captures_slice.len > 0) captures_slice else null };
        },
        .malformed => |malformed| {
            // We won't touch this since it's already a parse error.
            _ = malformed;
            return null;
        },
    }
}

/// Canonicalize an expr. If it fails, convert it to a malormed expr node
fn canonicalizeExprOrMalformed(
    self: *Self,
    ast_expr_idx: AST.Expr.Idx,
) std.mem.Allocator.Error!CanonicalizedExpr {
    return try self.canonicalizeExpr(ast_expr_idx) orelse blk: {
        const ast_expr = self.parse_ir.store.getExpr(ast_expr_idx);
        break :blk CanonicalizedExpr{
            .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .expr_not_canonicalized = .{
                .region = self.parse_ir.tokenizedRegionToRegion(ast_expr.to_tokenized_region()),
            } }),
            .free_vars = null,
        };
    };
}

// Canonicalize a tag expr
fn canonicalizeTagExpr(self: *Self, e: AST.TagExpr, mb_args: ?AST.Expr.Span, region: base.Region) std.mem.Allocator.Error!?CanonicalizedExpr {
    const tag_name = self.parse_ir.tokens.resolveIdentifier(e.token) orelse @panic("tag token is not an ident");
    const tag_name_text = self.parse_ir.env.getIdent(tag_name);

    var args_span = Expr.Span{ .span = DataSpan.empty() };

    const free_vars_start = self.scratch_free_vars.top();

    if (mb_args) |args| {
        if (args.span.len > 0) {
            // Canonicalize all arguments
            const scratch_top = self.env.store.scratchExprTop();

            // Canonicalize all arguments
            const args_slice = self.parse_ir.store.exprSlice(args);
            for (args_slice) |arg| {
                if (try self.canonicalizeExpr(arg)) |can_arg| {
                    try self.env.store.addScratchExpr(can_arg.idx);
                }
            }

            args_span = try self.env.store.exprSpanFrom(scratch_top);
        }
    }

    // Create a single tag, open tag union for this variable
    // Use a placeholder ext_var that will be handled during type checking
    const ext_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
    const tag = try self.env.types.mkTag(tag_name, @ptrCast(self.env.store.sliceExpr(args_span)));
    const tag_union = try self.env.types.mkTagUnion(&[_]Tag{tag}, ext_var);

    // Create the tag expression with the tag union type
    const tag_expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
        .e_tag = .{
            .name = tag_name,
            .args = args_span,
        },
    }, tag_union, region);

    if (e.qualifiers.span.len == 0) {
        // Check if this is an unqualified nominal tag (e.g. True or False are in scope unqualified by default)
        if (self.unqualified_nominal_tags.get(tag_name_text)) |nominal_type_decl| {
            // Get the type variable for the nominal type declaration (e.g., Bool type)
            const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal_type_decl,
                    .backing_expr = tag_expr_idx,
                    .backing_type = .tag,
                },
            }, .err, region);
            return CanonicalizedExpr{ .idx = expr_idx, .free_vars = null };
        }

        // If this is a tag without a prefix and not in unqualified_nominal_tags,
        // then it is an anonymous tag and we can just return it
        return CanonicalizedExpr{ .idx = tag_expr_idx, .free_vars = null };
    } else if (e.qualifiers.span.len == 1) {
        // If this is a tag with a single, then is it a nominal tag and the qualifier
        // is the type

        // Get the last token of the qualifiers
        const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
        const type_tok_idx = qualifier_toks[0];
        const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

        // Lookup the type ident in scope
        const nominal_type_decl_stmt_idx = self.scopeLookupTypeDecl(type_tok_ident) orelse
            return CanonicalizedExpr{
                .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .undeclared_type = .{
                    .name = type_tok_ident,
                    .region = type_tok_region,
                } }),
                .free_vars = null,
            };
        switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
            .s_nominal_decl => {
                const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
                    .e_nominal = .{
                        .nominal_type_decl = nominal_type_decl_stmt_idx,
                        .backing_expr = tag_expr_idx,
                        .backing_type = .tag,
                    },
                }, .err, region);

                const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
                return CanonicalizedExpr{
                    .idx = expr_idx,
                    .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null,
                };
            },
            .s_alias_decl => {
                return CanonicalizedExpr{
                    .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                        .name = type_tok_ident,
                        .region = type_tok_region,
                    } }),
                    .free_vars = null,
                };
            },
            else => {
                const feature = try self.env.insertString("report an error resolved type decl in scope wasn't actually a type decl");
                const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = Region.zero(),
                } });
                return CanonicalizedExpr{
                    .idx = malformed_idx,
                    .free_vars = null,
                };
            },
        }
    } else {
        // If this is a tag with more than 1 qualifier, then it is an imported
        // nominal type where the last qualifier is the type name, then the other
        // are the module

        // Get the last token of the qualifiers
        const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);

        // Get the type from the last qualifier
        const type_tok_idx = qualifier_toks[qualifier_toks.len - 1];
        const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
        const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);
        const type_tok_text = self.env.getIdent(type_tok_ident);

        // Get the fully resolved module name from all but the last qualifier
        const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
        const module_alias_text = self.parse_ir.resolveQualifiedName(
            .{ .span = .{ .start = 0, .len = @intCast(qualifier_toks.len - 2) } },
            qualifier_toks[qualifier_toks.len - 2],
            &strip_tokens,
        );
        const module_alias = try self.env.insertIdent(base.Ident.for_text(module_alias_text));

        // Check if this is a module alias
        const module_name = self.scopeLookupModule(module_alias) orelse {
            // Module is not in current scope
            return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .module_not_imported = .{
                .module_name = module_alias,
                .region = region,
            } }), .free_vars = null };
        };
        const module_name_text = self.env.getIdent(module_name);

        // Check if this module is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
            return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } }), .free_vars = null };
        };

        // Look up the target node index in the module's exposed_nodes
        const target_node_idx, const type_content = blk: {
            const envs_map = self.module_envs orelse {
                break :blk .{ 0, Content.err };
            };

            const module_env = envs_map.get(module_name_text) orelse {
                break :blk .{ 0, Content.err };
            };

            const target_ident = module_env.common.findIdent(type_tok_text) orelse {
                // Type is not exposed by the module
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_tok_ident,
                    .region = type_tok_region,
                } }), .free_vars = null };
            };

            const other_module_node_id = module_env.getExposedNodeIndexById(target_ident) orelse {
                // Type is not exposed by the module
                return CanonicalizedExpr{ .idx = try self.env.pushMalformed(Expr.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_tok_ident,
                    .region = type_tok_region,
                } }), .free_vars = null };
            };

            // Successfully found the target node
            break :blk .{ other_module_node_id, Content{ .flex_var = null } };
        };

        const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{
            .e_nominal_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
                .backing_expr = tag_expr_idx,
                .backing_type = .tag,
            },
        }, type_content, region);

        const free_vars_slice = self.scratch_free_vars.slice(free_vars_start, self.scratch_free_vars.top());
        return CanonicalizedExpr{
            .idx = expr_idx,
            .free_vars = if (free_vars_slice.len > 0) free_vars_slice else null,
        };
    }
}

/// Helper function to create a string literal expression and add it to the scratch stack
fn addStringLiteralToScratch(self: *Self, text: []const u8, region: AST.TokenizedRegion) std.mem.Allocator.Error!void {
    // intern the string in the ModuleEnv
    const string_idx = try self.env.insertString(text);

    // create a node for the string literal
    const str_expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{ .e_str_segment = .{
        .literal = string_idx,
    } }, Content{ .structure = .str }, self.parse_ir.tokenizedRegionToRegion(region));

    // add the node idx to our scratch expr stack
    try self.env.store.addScratchExpr(str_expr_idx);
}

/// Helper function to handle interpolation (non-string-part) expressions inside string literals
fn addInterpolationToScratch(self: *Self, part: AST.Expr.Idx, part_node: AST.Expr) std.mem.Allocator.Error!void {
    if (try self.canonicalizeExpr(part)) |can_expr| {
        // append our interpolated expression
        try self.env.store.addScratchExpr(can_expr.idx);
    } else {
        // unable to canonicalize the interpolation, push a malformed node
        const region = self.parse_ir.tokenizedRegionToRegion(part_node.to_tokenized_region());
        const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .invalid_string_interpolation = .{
            .region = region,
        } });
        try self.env.store.addScratchExpr(malformed_idx);
    }
}

/// Extract string segments from parsed string parts
fn extractStringSegments(self: *Self, parts: []const AST.Expr.Idx) std.mem.Allocator.Error!Expr.Span {
    const start = self.env.store.scratchExprTop();

    for (parts) |part| {
        const part_node = self.parse_ir.store.getExpr(part);
        switch (part_node) {
            .string_part => |sp| {
                // get the raw text of the string part
                const part_text = self.parse_ir.resolve(sp.token);
                try self.addStringLiteralToScratch(part_text, part_node.to_tokenized_region());
            },
            else => {
                try self.addInterpolationToScratch(part, part_node);
            },
        }
    }

    return try self.env.store.exprSpanFrom(start);
}

/// Extract string segments from parsed multiline string parts, adding newlines between consecutive string parts
fn extractMultilineStringSegments(self: *Self, parts: []const AST.Expr.Idx) std.mem.Allocator.Error!Expr.Span {
    const start = self.env.store.scratchExprTop();
    var last_string_part_end: ?Token.Idx = null;

    for (parts) |part| {
        const part_node = self.parse_ir.store.getExpr(part);
        switch (part_node) {
            .string_part => |sp| {
                // Add newline between consecutive string parts
                if (last_string_part_end != null) {
                    try self.addStringLiteralToScratch("\\n", .{ .start = last_string_part_end.?, .end = part_node.to_tokenized_region().start });
                }

                // Get and process the raw text of the string part
                const part_text = self.parse_ir.resolve(sp.token);
                if (part_text.len != 0) {
                    try self.addStringLiteralToScratch(part_text, part_node.to_tokenized_region());
                }
                last_string_part_end = part_node.to_tokenized_region().end;
            },
            else => {
                last_string_part_end = null;
                try self.addInterpolationToScratch(part, part_node);
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
                switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, true)) {
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
                                .stmt = try self.env.insertString("var"),
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
                const feature = try self.env.insertString("report an error when unable to resolve identifier");
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
            const token_text = self.parse_ir.resolve(e.number_tok);
            const parsed = types.Num.parseNumLiteralWithSuffix(token_text);

            // Parse the integer value
            const is_negated = parsed.num_text[0] == '-';
            const after_minus_sign = @as(usize, @intFromBool(is_negated));

            var first_digit: usize = undefined;
            const DEFAULT_BASE = 10;
            var int_base: u8 = undefined;

            if (parsed.num_text[after_minus_sign] == '0' and parsed.num_text.len > after_minus_sign + 2) {
                switch (parsed.num_text[after_minus_sign + 1]) {
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

            const u128_val = parseIntWithUnderscores(u128, parsed.num_text[first_digit..], int_base) catch {
                // Any number literal that is too large for u128 is invalid, regardless of whether it had a minus sign!
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                return malformed_idx;
            };

            // If this had a minus sign, but negating it would result in a negative number
            // that would be too low to fit in i128, then this int literal is also invalid.
            if (is_negated and u128_val > min_i128_negated) {
                const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                return malformed_idx;
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
            const i128_val: i128 = if (is_negated) blk: {
                if (u128_val == min_i128_negated) {
                    break :blk @as(i128, @bitCast(u128_val));
                } else {
                    break :blk -@as(i128, @bitCast(u128_val));
                }
            } else @as(i128, @bitCast(u128_val));

            // Calculate requirements based on the value
            // Special handling for minimum signed values (-128, -32768, etc.)
            // These are special because they have a power-of-2 magnitude that fits exactly
            // in their signed type. We report them as needing one less bit to make the
            // standard "signed types have n-1 usable bits" logic work correctly.
            if (parsed.suffix) |suffix| {
                const type_content = blk: {
                    if (std.mem.eql(u8, suffix, "u8")) {
                        if (u128_val > std.math.maxInt(u8)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u8 } };
                    } else if (std.mem.eql(u8, suffix, "u16")) {
                        if (u128_val > std.math.maxInt(u16)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u16 } };
                    } else if (std.mem.eql(u8, suffix, "u32")) {
                        if (u128_val > std.math.maxInt(u32)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u32 } };
                    } else if (std.mem.eql(u8, suffix, "u64")) {
                        if (u128_val > std.math.maxInt(u64)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u64 } };
                    } else if (std.mem.eql(u8, suffix, "u128")) {
                        break :blk Content{ .structure = FlatType{ .num = Num.int_u128 } };
                    } else if (std.mem.eql(u8, suffix, "i8")) {
                        if (i128_val < std.math.minInt(i8) or i128_val > std.math.maxInt(i8)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i8 } };
                    } else if (std.mem.eql(u8, suffix, "i16")) {
                        if (i128_val < std.math.minInt(i16) or i128_val > std.math.maxInt(i16)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i16 } };
                    } else if (std.mem.eql(u8, suffix, "i32")) {
                        if (i128_val < std.math.minInt(i32) or i128_val > std.math.maxInt(i32)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i32 } };
                    } else if (std.mem.eql(u8, suffix, "i64")) {
                        if (i128_val < std.math.minInt(i64) or i128_val > std.math.maxInt(i64)) break :blk null;
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i64 } };
                    } else if (std.mem.eql(u8, suffix, "i128")) {
                        break :blk Content{ .structure = FlatType{ .num = Num.int_i128 } };
                    } else {
                        break :blk null;
                    }
                };

                if (type_content) |content| {
                    const pattern_idx = try self.env.addPatternAndTypeVar(
                        .{ .int_literal = .{ .value = .{ .bytes = @bitCast(i128_val), .kind = .i128 } } },
                        content,
                        region,
                    );
                    return pattern_idx;
                }
            }

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

            const pattern_idx = try self.env.addPatternAndTypeVar(
                Pattern{ .int_literal = .{ .value = CIR.IntValue{
                    .bytes = @bitCast(i128_val),
                    .kind = .i128,
                } } },
                type_content,
                region,
            );
            return pattern_idx;
        },
        .frac => |e| {
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Resolve to a string slice from the source
            const token_text = self.parse_ir.resolve(e.number_tok);
            const parsed_num = types.Num.parseNumLiteralWithSuffix(token_text);

            if (parsed_num.suffix) |suffix| {
                const f64_val = std.fmt.parseFloat(f64, parsed_num.num_text) catch {
                    const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                    return malformed_idx;
                };

                if (std.mem.eql(u8, suffix, "f32")) {
                    if (!fitsInF32(f64_val)) {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    }
                    const pattern_idx = try self.env.addPatternAndTypeVar(
                        .{ .frac_f32_literal = .{ .value = @floatCast(f64_val) } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .f32 } } },
                        region,
                    );
                    return pattern_idx;
                } else if (std.mem.eql(u8, suffix, "f64")) {
                    const pattern_idx = try self.env.addPatternAndTypeVar(
                        .{ .frac_f64_literal = .{ .value = f64_val } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .f64 } } },
                        region,
                    );
                    return pattern_idx;
                } else if (std.mem.eql(u8, suffix, "dec")) {
                    if (!fitsInDec(f64_val)) {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    }
                    const dec_val = RocDec.fromF64(f64_val) orelse {
                        const malformed_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .invalid_num_literal = .{ .region = region } });
                        return malformed_idx;
                    };
                    const pattern_idx = try self.env.addPatternAndTypeVar(
                        .{ .dec_literal = .{ .value = dec_val } },
                        .{ .structure = FlatType{ .num = .{ .frac_precision = .dec } } },
                        region,
                    );
                    return pattern_idx;
                }
            }

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
                        .value = dec_info.value,
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
            const literal = try self.env.insertString(token_text);

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
            const tag_name_text = self.parse_ir.env.getIdent(tag_name);

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
                // Check if this is an unqualified nominal tag (e.g. True or False are in scope unqualified by default)
                if (self.unqualified_nominal_tags.get(tag_name_text)) |nominal_type_decl| {
                    // Get the type variable for the nominal type declaration (e.g., Bool type)
                    const nominal_type_var = ModuleEnv.castIdx(Statement.Idx, TypeVar, nominal_type_decl);
                    const nominal_pattern_idx = try self.env.addPatternAndTypeVarRedirect(CIR.Pattern{
                        .nominal = .{
                            .nominal_type_decl = nominal_type_decl,
                            .backing_pattern = tag_pattern_idx,
                            .backing_type = .tag,
                        },
                    }, nominal_type_var, region);
                    return nominal_pattern_idx;
                }

                // If this is a tag without a prefix and not in unqualified_nominal_tags,
                // then it is an anonymous tag and we can just return it
                return tag_pattern_idx;
            } else if (e.qualifiers.span.len == 1) {
                // If this is a tag with a single, then is it a nominal tag and the qualifier is the type

                // Get the last token of the qualifiers
                const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);
                const type_tok_idx = qualifier_toks[0];
                const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
                const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);

                // Lookup the type ident in scope
                const nominal_type_decl_stmt_idx = self.scopeLookupTypeDecl(type_tok_ident) orelse
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .undeclared_type = .{
                        .name = type_tok_ident,
                        .region = type_tok_region,
                    } });

                switch (self.env.store.getStatement(nominal_type_decl_stmt_idx)) {
                    .s_nominal_decl => {
                        const nominal_type_var = ModuleEnv.castIdx(Statement.Idx, TypeVar, nominal_type_decl_stmt_idx);
                        const pattern_idx = try self.env.addPatternAndTypeVarRedirect(CIR.Pattern{
                            .nominal = .{
                                .nominal_type_decl = nominal_type_decl_stmt_idx,
                                .backing_pattern = tag_pattern_idx,
                                .backing_type = .tag,
                            },
                        }, nominal_type_var, region);

                        return pattern_idx;
                    },
                    .s_alias_decl => {
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .type_alias_but_needed_nominal = .{
                            .name = type_tok_ident,
                            .region = type_tok_region,
                        } });
                    },
                    else => {
                        const feature = try self.env.insertString("report an error resolved type decl in scope wasn't actually a type decl");
                        return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                            .feature = feature,
                            .region = Region.zero(),
                        } });
                    },
                }
            } else {
                // If this is a tag with more than 1 qualifier, then it is an imported
                // nominal type where the last qualifier is the type name, then the other
                // are the module

                // Get the last token of the qualifiers
                const qualifier_toks = self.parse_ir.store.tokenSlice(e.qualifiers);

                // Get the type from the last qualifier
                const type_tok_idx = qualifier_toks[qualifier_toks.len - 1];
                const type_tok_ident = self.parse_ir.tokens.resolveIdentifier(type_tok_idx) orelse unreachable;
                const type_tok_region = self.parse_ir.tokens.resolve(type_tok_idx);
                const type_tok_text = self.env.getIdent(type_tok_ident);

                // Get the fully resolved module name from all but the last qualifier
                const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
                const module_alias_text = self.parse_ir.resolveQualifiedName(
                    .{ .span = .{ .start = 0, .len = @intCast(qualifier_toks.len - 2) } },
                    qualifier_toks[qualifier_toks.len - 2],
                    &strip_tokens,
                );
                const module_alias = try self.env.insertIdent(base.Ident.for_text(module_alias_text));

                // Check if this is a module alias
                const module_name = self.scopeLookupModule(module_alias) orelse {
                    // Module is not in current scope
                    return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .module_not_imported = .{
                        .module_name = module_alias,
                        .region = region,
                    } });
                };
                const module_name_text = self.env.getIdent(module_name);

                // Check if this module is imported in the current scope
                const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
                    return try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .module_not_imported = .{
                        .module_name = module_name,
                        .region = region,
                    } });
                };

                // Look up the target node index in the module's exposed_nodes
                const target_node_idx, const type_content = blk: {
                    const envs_map = self.module_envs orelse {
                        break :blk .{ 0, Content.err };
                    };

                    const module_env = envs_map.get(module_name_text) orelse {
                        break :blk .{ 0, Content.err };
                    };

                    const target_ident = module_env.common.findIdent(type_tok_text) orelse {
                        // Type is not exposed by the module
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = type_tok_ident,
                            .region = type_tok_region,
                        } });
                    };

                    const other_module_node_id = module_env.getExposedNodeIndexById(target_ident) orelse {
                        // Type is not exposed by the module
                        return try self.env.pushMalformed(Pattern.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                            .module_name = module_name,
                            .type_name = type_tok_ident,
                            .region = type_tok_region,
                        } });
                    };

                    // Successfully found the target node
                    break :blk .{ other_module_node_id, Content{ .flex_var = null } };
                };

                const nominal_pattern_idx = try self.env.addPatternAndTypeVar(CIR.Pattern{
                    .nominal_external = .{
                        .module_idx = import_idx,
                        .target_node_idx = target_node_idx,
                        .backing_pattern = tag_pattern_idx,
                        .backing_type = .tag,
                    },
                }, type_content, region);

                return nominal_pattern_idx;
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
                        const record_destruct = CIR.Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .{ .SubPattern = canonicalized_sub_pattern },
                        };

                        const destruct_idx = try self.env.addRecordDestructAndTypeVar(record_destruct, .{ .flex_var = null }, field_region);
                        try self.env.store.addScratchRecordDestruct(destruct_idx);
                    } else {
                        // Simple case: Create the RecordDestruct for this field
                        const assign_pattern = Pattern{ .assign = .{ .ident = field_name_ident } };
                        const assign_pattern_idx = try self.env.addPatternAndTypeVar(assign_pattern, .{ .flex_var = null }, field_region);

                        const record_destruct = CIR.Pattern.RecordDestruct{
                            .label = field_name_ident,
                            .ident = field_name_ident,
                            .kind = .{ .Required = assign_pattern_idx },
                        };

                        const destruct_idx = try self.env.addRecordDestructAndTypeVar(record_destruct, .{ .flex_var = null }, field_region);
                        try self.env.store.addScratchRecordDestruct(destruct_idx);

                        // Introduce the identifier into scope
                        switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, field_name_ident, assign_pattern_idx, false, true)) {
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
                                        .stmt = try self.env.insertString("var"),
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
                    const feature = try self.env.insertString("report an error when unable to resolve field identifier");
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
                            switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, assign_idx, false, true)) {
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
                            const feature = try self.env.insertString("list rest pattern with unresolvable name");
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
            const feature = try self.env.insertString("standalone list rest pattern");
            const pattern_idx = try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = region,
            } });
            return pattern_idx;
        },
        .alternatives => |_| {
            // Alternatives patterns should only appear in match expressions and are handled there
            // If we encounter one here, it's likely a parser error or misplaced pattern
            const feature = try self.env.insertString("alternatives pattern outside match expression");
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
                const feature = try self.env.insertString("canonicalize as pattern with malformed inner pattern");
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
                switch (try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, true)) {
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
                                .stmt = try self.env.insertString("var"),
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
                const feature = try self.env.insertString("report an error when unable to resolve as pattern identifier");
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

/// Introduce a new identifier to the current scope, return an
/// index if
fn scopeIntroduceIdent(
    self: *Self,
    ident_idx: Ident.Idx,
    pattern_idx: Pattern.Idx,
    region: Region,
    comptime T: type,
) !T {
    const result = try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, false, true);

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
            return self.env.pushMalformed(T, Diagnostic{
                .invalid_top_level_statement = .{
                    .stmt = try self.env.insertString("var"),
                    .region = region,
                },
            });
        },
        .var_across_function_boundary => |_| {
            // This shouldn't happen for regular identifiers
            return self.env.pushMalformed(T, Diagnostic{ .not_implemented = .{
                .feature = try self.env.insertString("var across function boundary for non-var identifier"),
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
    const result = try self.scopeIntroduceInternal(self.env.gpa, .ident, ident_idx, pattern_idx, true, is_declaration);

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
                    .stmt = try self.env.insertString("var"),
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
        const name_text = self.env.getIdent(problem.ident);

        switch (problem.problem) {
            .type_var_ending_in_underscore => {
                const suggested_name_text = name_text[0 .. name_text.len - 1]; // Remove the trailing underscore
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

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
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

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
        const name_text = self.env.getIdent(problem.ident);

        switch (problem.problem) {
            .type_var_ending_in_underscore => {
                const suggested_name_text = name_text[0 .. name_text.len - 1]; // Remove the trailing underscore
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

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
                const suggested_ident = self.env.insertIdent(base.Ident.for_text(suggested_name_text), Region.zero());

                self.env.pushDiagnostic(Diagnostic{ .type_var_marked_unused = .{
                    .name = problem.ident,
                    .suggested_name = suggested_ident,
                    .region = Region.zero(),
                } });
            },
        }
    }
}

// ===== Canonicalize Type Annotations =====

// Some type annotations, like function type annotations, can introduce variables.
// Others, however, like alias or nominal tag annotations, cannot.
const TypeAnnoCtx = struct {
    type: TypeAnnoCtxType,
    found_underscore: bool,

    const TypeAnnoCtxType = enum(u1) { type_decl_anno, inline_anno };

    pub fn init(typ: TypeAnnoCtxType) TypeAnnoCtx {
        return .{ .type = typ, .found_underscore = false };
    }

    pub fn isTypeDeclAndHasUnderscore(self: TypeAnnoCtx) bool {
        return self.type == .type_decl_anno and self.found_underscore;
    }
};

fn canonicalizeTypeAnno(self: *Self, anno_idx: AST.TypeAnno.Idx, type_anno_ctx_type: TypeAnnoCtx.TypeAnnoCtxType) std.mem.Allocator.Error!TypeAnno.Idx {
    var ctx = TypeAnnoCtx.init(type_anno_ctx_type);
    return canonicalizeTypeAnnoHelp(self, anno_idx, &ctx);
}

fn canonicalizeTypeAnnoHelp(self: *Self, anno_idx: AST.TypeAnno.Idx, type_anno_ctx: *TypeAnnoCtx) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const ast_anno = self.parse_ir.store.getTypeAnno(anno_idx);
    switch (ast_anno) {
        .apply => |apply| {
            return try self.canonicalizeTypeAnnoTypeApplication(apply, type_anno_ctx);
        },
        .ty_var => |ty_var| {
            const region = self.parse_ir.tokenizedRegionToRegion(ty_var.region);
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ty_var.tok) orelse {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{
                    .region = region,
                } });
            };
            // Check if this type variable is in scope
            const scope = self.currentScope();
            switch (scope.lookupTypeVar(name_ident)) {
                .found => |found_anno_idx| {
                    // Track this type variable for underscore validation
                    try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

                    return try self.env.addTypeAnnoAndTypeVarRedirect(.{ .ty_var = .{
                        .name = name_ident,
                    } }, ModuleEnv.varFrom(found_anno_idx), region);
                },
                .not_found => {
                    switch (type_anno_ctx.type) {
                        // If this is an inline anno, then we can introduce the variable
                        // into the scope
                        .inline_anno => {
                            // Track this type variable for underscore validation
                            try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

                            const content = types.Content{ .rigid_var = name_ident };
                            const new_anno_idx = try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                                .name = name_ident,
                            } }, content, region);

                            // Add to scope
                            _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);

                            return new_anno_idx;
                        },
                        // Otherwise, this is malformed
                        .type_decl_anno => {
                            return self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                                .name = name_ident,
                                .region = region,
                            } });
                        },
                    }
                },
            }
        },
        .underscore_type_var => |underscore_ty_var| {
            type_anno_ctx.found_underscore = true;

            const region = self.parse_ir.tokenizedRegionToRegion(underscore_ty_var.region);

            // Underscore types aren't allowed in type declarations (aliases or nominal)
            if (type_anno_ctx.type == .type_decl_anno) {
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
            const scope = self.currentScope();
            switch (scope.lookupTypeVar(name_ident)) {
                .found => |found_anno_idx| {
                    // Track this type variable for underscore validation
                    try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

                    return try self.env.addTypeAnnoAndTypeVarRedirect(.{ .ty_var = .{
                        .name = name_ident,
                    } }, ModuleEnv.varFrom(found_anno_idx), region);
                },
                .not_found => {
                    switch (type_anno_ctx.type) {
                        // If this is an inline anno, then we can introduce the variable
                        // into the scope
                        .inline_anno => {
                            // Track this type variable for underscore validation
                            try self.scratch_type_var_validation.append(self.env.gpa, name_ident);

                            const content = types.Content{ .rigid_var = name_ident };
                            const new_anno_idx = try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                                .name = name_ident,
                            } }, content, region);

                            // Add to scope
                            _ = try self.scopeIntroduceTypeVar(name_ident, new_anno_idx);

                            return new_anno_idx;
                        },
                        // Otherwise, this is malformed
                        .type_decl_anno => {
                            return self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .undeclared_type_var = .{
                                .name = name_ident,
                                .region = region,
                            } });
                        },
                    }
                },
            }
        },
        .ty => |ty| {
            return (try self.canonicalizeTypeAnnoBasicType(ty)).anno_idx;
        },
        .underscore => |underscore| {
            type_anno_ctx.found_underscore = true;

            const region = self.parse_ir.tokenizedRegionToRegion(underscore.region);

            // Underscore types aren't allowed in type declarations (aliases or nominal)
            if (type_anno_ctx.type == .type_decl_anno) {
                try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                    .is_alias = true,
                    .region = region,
                } });
            }

            // Create type variable with error content if underscore in type declaration
            const content = blk: {
                if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                    break :blk types.Content{ .err = {} };
                } else {
                    break :blk types.Content{ .flex_var = null };
                }
            };

            return try self.env.addTypeAnnoAndTypeVar(.{ .underscore = {} }, content, region);
        },
        .tuple => |tuple| {
            return try self.canonicalizeTypeAnnoTuple(tuple, type_anno_ctx);
        },
        .record => |record| {
            return try self.canonicalizeTypeAnnoRecord(record, type_anno_ctx);
        },
        .tag_union => |tag_union| {
            return try self.canonicalizeTypeAnnoTagUnion(tag_union, type_anno_ctx);
        },
        .@"fn" => |func| {
            return try self.canonicalizeTypeAnnoFunc(func, type_anno_ctx);
        },
        .parens => |parens| {
            const region = self.parse_ir.tokenizedRegionToRegion(parens.region);
            const inner_anno = try self.canonicalizeTypeAnnoHelp(parens.anno, type_anno_ctx);

            // Create type variable with error content if underscore in type declaration
            if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                return try self.env.addTypeAnnoAndTypeVar(.{ .parens = .{
                    .anno = inner_anno,
                } }, .err, region);
            } else {
                return try self.env.addTypeAnnoAndTypeVarRedirect(.{ .parens = .{
                    .anno = inner_anno,
                } }, ModuleEnv.varFrom(inner_anno), region);
            }
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

const CanonicalizedTypeAnnoBasicType = struct {
    anno_idx: TypeAnno.Idx,
    mb_local_decl_idx: ?Statement.Idx,
};

/// Handle basic type lookup (Bool, Str, Num, etc.)
fn canonicalizeTypeAnnoBasicType(
    self: *Self,
    ty: @TypeOf(@as(AST.TypeAnno, undefined).ty),
) std.mem.Allocator.Error!CanonicalizedTypeAnnoBasicType {
    const trace = tracy.trace(@src());
    defer trace.end();

    const region = self.parse_ir.tokenizedRegionToRegion(ty.region);

    // Get the last token of the qualifiers
    const qualifier_toks = self.parse_ir.store.tokenSlice(ty.qualifiers);

    // Get the type ident
    const type_name_ident = self.parse_ir.tokens.resolveIdentifier(ty.token) orelse unreachable;
    const type_name_region = self.parse_ir.tokens.resolve(ty.token);

    if (qualifier_toks.len == 0) {
        // Unqualified type

        // TODO: Check for List, Box, and Str here (since they are primitives)

        const type_decl_idx = self.scopeLookupTypeDecl(type_name_ident) orelse {
            // Type not found in scope - issue diagnostic
            try self.env.pushDiagnostic(Diagnostic{ .undeclared_type = .{
                .name = type_name_ident,
                .region = type_name_region,
            } });
            return .{ .anno_idx = try self.env.addTypeAnnoAndTypeVar(.{ .ty = .{
                .symbol = type_name_ident,
            } }, .err, region), .mb_local_decl_idx = null };
        };

        const type_decl = self.env.store.getStatement(type_decl_idx);
        switch (type_decl) {
            .s_alias_decl => |_| {
                return .{
                    .anno_idx = try self.env.addTypeAnnoAndTypeVarRedirect(CIR.TypeAnno{ .ty = .{
                        .symbol = type_name_ident,
                    } }, ModuleEnv.varFrom(type_decl_idx), region),
                    .mb_local_decl_idx = type_decl_idx,
                };
            },
            .s_nominal_decl => |_| {
                return .{
                    .anno_idx = try self.env.addTypeAnnoAndTypeVarRedirect(CIR.TypeAnno{ .ty = .{
                        .symbol = type_name_ident,
                    } }, ModuleEnv.varFrom(type_decl_idx), region),
                    .mb_local_decl_idx = type_decl_idx,
                };
            },
            else => {
                // Since we looked up this type decl from `scopeLookupTypeDecl`
                // this state should be impossible
                unreachable;
            },
        }
    } else {
        // This is an external type

        // Get the fully resolved module name
        const strip_tokens = [_]tokenize.Token.Tag{.NoSpaceDotUpperIdent};
        const module_alias_text = self.parse_ir.resolveQualifiedName(
            .{ .span = .{ .start = 0, .len = @intCast(qualifier_toks.len - 1) } },
            qualifier_toks[qualifier_toks.len - 1],
            &strip_tokens,
        );
        const module_alias = try self.env.insertIdent(base.Ident.for_text(module_alias_text));

        // Check if this is a module alias
        const module_name = self.scopeLookupModule(module_alias) orelse {
            // Module is not in current scope
            return .{ .anno_idx = try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .module_not_imported = .{
                .module_name = module_alias,
                .region = region,
            } }), .mb_local_decl_idx = null };
        };
        const module_name_text = self.env.getIdent(module_name);

        // Check if this module is imported in the current scope
        const import_idx = self.scopeLookupImportedModule(module_name_text) orelse {
            return .{ .anno_idx = try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .module_not_imported = .{
                .module_name = module_name,
                .region = region,
            } }), .mb_local_decl_idx = null };
        };

        // Look up the target node index in the module's exposed_nodes
        const type_name_text = self.env.getIdent(type_name_ident);
        const target_node_idx, const type_content = blk: {
            const envs_map = self.module_envs orelse {
                break :blk .{ 0, Content.err };
            };

            const module_env = envs_map.get(module_name_text) orelse {
                break :blk .{ 0, Content.err };
            };

            const target_ident = module_env.common.findIdent(type_name_text) orelse {
                // Type is not exposed by the module
                return .{ .anno_idx = try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_name_region,
                } }), .mb_local_decl_idx = null };
            };

            const other_module_node_id = module_env.getExposedNodeIndexById(target_ident) orelse {
                // Type is not exposed by the module
                return .{ .anno_idx = try self.env.pushMalformed(TypeAnno.Idx, CIR.Diagnostic{ .type_not_exposed = .{
                    .module_name = module_name,
                    .type_name = type_name_ident,
                    .region = type_name_region,
                } }), .mb_local_decl_idx = null };
            };

            // Successfully found the target node
            break :blk .{ other_module_node_id, Content{ .flex_var = null } };
        };

        // Create the ty_lookup_external expression with Import.Idx
        // Type solving will copy this types from the origin type store into the
        // this module's type store
        return .{
            .anno_idx = try self.env.addTypeAnnoAndTypeVar(CIR.TypeAnno{ .ty_lookup_external = .{
                .module_idx = import_idx,
                .target_node_idx = target_node_idx,
            } }, type_content, region),
            .mb_local_decl_idx = null,
        };
    }
}

/// Handle type applications like List(Str), Dict(k, v)
fn canonicalizeTypeAnnoTypeApplication(
    self: *Self,
    apply: @TypeOf(@as(AST.TypeAnno, undefined).apply),
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const region = self.parse_ir.tokenizedRegionToRegion(apply.region);
    const args_slice = self.parse_ir.store.typeAnnoSlice(apply.args);

    // Validate we have arguments
    if (args_slice.len == 0) {
        return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
    }

    // Canonicalize the base type first
    const based_anno_ast = self.parse_ir.store.getTypeAnno(args_slice[0]);
    const base_canonicalized = blk: {
        switch (based_anno_ast) {
            .ty => |ty| {
                break :blk try self.canonicalizeTypeAnnoBasicType(ty);
            },
            else => {
                return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } });
            },
        }
    };
    const base_anno = self.env.store.getTypeAnno(base_canonicalized.anno_idx);

    // Canonicalize type arguments (skip first which is the type name)
    const scratch_top = self.env.store.scratchTypeAnnoTop();
    defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

    for (args_slice[1..]) |arg_idx| {
        const apply_arg_anno_idx = try self.canonicalizeTypeAnnoHelp(arg_idx, type_anno_ctx);
        try self.env.store.addScratchTypeAnno(apply_arg_anno_idx);
    }
    const args_span = try self.env.store.typeAnnoSpanFrom(scratch_top);

    // Extract the root type symbol for the type application
    // Then, we must instantiate the type from the base declaration *with* the
    // user-provided type arugmuments applied
    switch (base_anno) {
        .ty => |ty| {
            if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                return try self.env.addTypeAnnoAndTypeVar(.{ .apply = .{
                    .symbol = ty.symbol,
                    .args = args_span,
                } }, .err, region);
            }

            const local_decl_idx = base_canonicalized.mb_local_decl_idx orelse {
                return try self.env.addTypeAnnoAndTypeVar(.{ .apply = .{
                    .symbol = ty.symbol,
                    .args = args_span,
                } }, .err, region);
            };

            return try self.env.addTypeAnnoAndTypeVarRedirect(.{ .apply = .{
                .symbol = ty.symbol,
                .args = args_span,
            } }, ModuleEnv.varFrom(local_decl_idx), region);
        },
        .ty_lookup_external => |tle| {
            if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                return try self.env.addTypeAnnoAndTypeVar(.{ .apply_external = .{
                    .module_idx = tle.module_idx,
                    .target_node_idx = tle.target_node_idx,
                    .args = args_span,
                } }, .err, region);
            } else {
                // Set the type to be flex var for now. The type solving phase
                // will copy the type from the original module's type store into
                // this module's type store
                return try self.env.addTypeAnnoAndTypeVar(.{ .apply_external = .{
                    .module_idx = tle.module_idx,
                    .target_node_idx = tle.target_node_idx,
                    .args = args_span,
                } }, .{ .flex_var = null }, region);
            }
        },
        else => return base_canonicalized.anno_idx,
    }
}

/// Handle tuple types like (a, b, c)
fn canonicalizeTypeAnnoTuple(
    self: *Self,
    tuple: @TypeOf(@as(AST.TypeAnno, undefined).tuple),
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const region = self.parse_ir.tokenizedRegionToRegion(tuple.region);

    const tuple_elems_slice = self.parse_ir.store.typeAnnoSlice(tuple.annos);

    if (tuple_elems_slice.len == 1) {
        return try self.canonicalizeTypeAnnoHelp(tuple_elems_slice[0], type_anno_ctx);
    } else {

        // Canonicalize all tuple elements
        const scratch_top = self.env.store.scratchTypeAnnoTop();
        defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

        const scratch_vars_top = self.scratch_vars.top();
        defer self.scratch_vars.clearFrom(scratch_vars_top);

        for (tuple_elems_slice) |elem_idx| {
            const canonicalized_elem_idx = try self.canonicalizeTypeAnnoHelp(elem_idx, type_anno_ctx);
            try self.env.store.addScratchTypeAnno(canonicalized_elem_idx);

            const elem_var = ModuleEnv.varFrom(canonicalized_elem_idx);
            try self.scratch_vars.append(self.env.gpa, elem_var);
        }
        const annos = try self.env.store.typeAnnoSpanFrom(scratch_top);

        const content = blk: {
            if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
                break :blk types.Content{ .err = {} };
            } else {
                const elems_var_range = try self.env.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                break :blk types.Content{ .structure = FlatType{ .tuple = .{ .elems = elems_var_range } } };
            }
        };

        return try self.env.addTypeAnnoAndTypeVar(.{ .tuple = .{
            .elems = annos,
        } }, content, region);
    }
}

fn canonicalizeTypeAnnoRecord(
    self: *Self,
    record: @TypeOf(@as(AST.TypeAnno, undefined).record),
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
    const region = self.parse_ir.tokenizedRegionToRegion(record.region);

    // Canonicalize all record fields
    const scratch_top = self.env.store.scratchAnnoRecordFieldTop();
    defer self.env.store.clearScratchAnnoRecordFieldsFrom(scratch_top);

    const scratch_record_fields_top = self.scratch_record_fields.top();
    defer self.scratch_record_fields.clearFrom(scratch_record_fields_top);

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
            const malformed_ident = try self.env.insertIdent(malformed_field_ident);
            const canonicalized_ty = try self.canonicalizeTypeAnnoHelp(ast_field.ty, type_anno_ctx);

            const cir_field = CIR.TypeAnno.RecordField{
                .name = malformed_ident,
                .ty = canonicalized_ty,
            };
            const field_cir_idx = try self.env.addAnnoRecordFieldAndTypeVarRedirect(
                cir_field,
                ModuleEnv.varFrom(canonicalized_ty),
                self.parse_ir.tokenizedRegionToRegion(ast_field.region),
            );
            try self.env.store.addScratchAnnoRecordField(field_cir_idx);

            try self.scratch_record_fields.append(self.env.gpa, types.RecordField{
                .name = malformed_ident,
                .var_ = ModuleEnv.varFrom(field_cir_idx),
            });

            continue;
        };

        // Canonicalize field type
        const canonicalized_ty = try self.canonicalizeTypeAnnoHelp(ast_field.ty, type_anno_ctx);

        // Create CIR field
        const cir_field = CIR.TypeAnno.RecordField{
            .name = field_name,
            .ty = canonicalized_ty,
        };
        const field_cir_idx = try self.env.addAnnoRecordFieldAndTypeVarRedirect(
            cir_field,
            ModuleEnv.varFrom(canonicalized_ty),
            self.parse_ir.tokenizedRegionToRegion(ast_field.region),
        );
        try self.env.store.addScratchAnnoRecordField(field_cir_idx);

        try self.scratch_record_fields.append(self.env.gpa, types.RecordField{
            .name = field_name,
            .var_ = ModuleEnv.varFrom(field_cir_idx),
        });
    }

    const field_anno_idxs = try self.env.store.annoRecordFieldSpanFrom(scratch_top);

    // Should we be sorting here?
    const record_fields_scratch = self.scratch_record_fields.sliceFromStart(scratch_record_fields_top);
    std.mem.sort(types.RecordField, record_fields_scratch, self.env.common.getIdentStore(), comptime types.RecordField.sortByNameAsc);
    const fields_type_range = try self.env.types.appendRecordFields(record_fields_scratch);

    const content = blk: {
        if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
            break :blk types.Content{ .err = {} };
        } else {
            // TODO: Add parser support for extensible variables in
            // record then thread that through here
            const ext_var = try self.env.addTypeSlotAndTypeVar(
                @enumFromInt(0), // TODO
                .{ .structure = .empty_record },
                region,
                TypeVar,
            );
            break :blk Content{ .structure = .{ .record = .{ .fields = fields_type_range, .ext = ext_var } } };
        }
    };

    return try self.env.addTypeAnnoAndTypeVar(.{ .record = .{
        .fields = field_anno_idxs,
    } }, content, region);
}

/// Handle tag union types like [Some(a), None]
fn canonicalizeTypeAnnoTagUnion(
    self: *Self,
    tag_union: @TypeOf(@as(AST.TypeAnno, undefined).tag_union),
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const region = self.parse_ir.tokenizedRegionToRegion(tag_union.region);

    // Canonicalize all tags in the union using tag-specific canonicalization
    const scratch_annos_top = self.env.store.scratchTypeAnnoTop();
    defer self.env.store.clearScratchTypeAnnosFrom(scratch_annos_top);

    const scratch_tags_top = self.scratch_tags.top();
    defer self.scratch_tags.clearFrom(scratch_tags_top);

    for (self.parse_ir.store.typeAnnoSlice(tag_union.tags)) |tag_idx| {
        // First canonicalized the tag variant
        // This will always return a `ty` or an `apply`
        const canonicalized_tag_idx = try self.canonicalizeTypeAnnoTag(tag_idx, type_anno_ctx);
        try self.env.store.addScratchTypeAnno(canonicalized_tag_idx);

        // Then, create the type system tag and append to scratch tags
        const tag_cir_anno = self.env.store.getTypeAnno(canonicalized_tag_idx);
        const tag = blk: {
            switch (tag_cir_anno) {
                .ty => |ty| {
                    break :blk try self.env.types.mkTag(ty.symbol, &.{});
                },
                .apply => |apply| {
                    const args_slice: []TypeVar = @ptrCast(self.env.store.sliceTypeAnnos(apply.args));
                    break :blk try self.env.types.mkTag(apply.symbol, args_slice);
                },
                .malformed => {
                    continue;
                },
                else => unreachable,
            }
        };
        try self.scratch_tags.append(self.env.gpa, tag);
    }

    const tag_anno_idxs = try self.env.store.typeAnnoSpanFrom(scratch_annos_top);

    // Should we be sorting here?
    const tags_slice = self.scratch_tags.sliceFromStart(scratch_tags_top);
    std.mem.sort(types.Tag, tags_slice, self.env.common.getIdentStore(), comptime types.Tag.sortByNameAsc);

    // Canonicalize the ext, if it exists
    const mb_ext_anno = if (tag_union.open_anno) |open_idx| blk: {
        break :blk try self.canonicalizeTypeAnnoHelp(open_idx, type_anno_ctx);
    } else null;

    const content = blk: {
        if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
            break :blk types.Content{ .err = {} };
        } else {
            // Make the ext type variable
            const ext_var = inner_blk: {
                if (mb_ext_anno) |ext_anno| {
                    break :inner_blk ModuleEnv.varFrom(ext_anno);
                } else {
                    break :inner_blk try self.env.addTypeSlotAndTypeVar(
                        @enumFromInt(0),
                        .{ .structure = .empty_tag_union },
                        region,
                        TypeVar,
                    );
                }
            };

            // Make type system tag union
            break :blk try self.env.types.mkTagUnion(tags_slice, ext_var);
        }
    };

    return try self.env.addTypeAnnoAndTypeVar(.{ .tag_union = .{
        .tags = tag_anno_idxs,
        .ext = mb_ext_anno,
    } }, content, region);
}

/// Canonicalize a tag variant within a tag union type annotation
///
/// Unlike general type canonicalization, this doesn't validate tag names against scope
/// since tags in tag unions are anonymous and defined by the union itself
///
/// Note that these annotation node types are not used, so they're set to be flex vars
fn canonicalizeTypeAnnoTag(
    self: *Self,
    anno_idx: AST.TypeAnno.Idx,
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
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
                try self.env.insertIdent(base.Ident.for_text(self.parse_ir.resolve(ty.token)));

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
                    try self.env.insertIdent(base.Ident.for_text(self.parse_ir.resolve(ty.token))),
                else => return try self.env.pushMalformed(TypeAnno.Idx, Diagnostic{ .malformed_type_annotation = .{ .region = region } }),
            };

            // Canonicalize type arguments (skip first which is the tag name)
            // These should be validated against scope since they're real types like `Str`, `Int`, etc.
            const scratch_top = self.env.store.scratchTypeAnnoTop();
            defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);

            for (args_slice[1..]) |arg_idx| {
                const canonicalized = try self.canonicalizeTypeAnnoHelp(arg_idx, type_anno_ctx);
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
                .malformed_type_annotation = .{ .region = self.parse_ir.tokenizedRegionToRegion(ast_anno.to_tokenized_region()) },
            });
        },
    }
}

fn canonicalizeTypeAnnoFunc(
    self: *Self,
    func: @TypeOf(@as(AST.TypeAnno, undefined).@"fn"),
    type_anno_ctx: *TypeAnnoCtx,
) std.mem.Allocator.Error!TypeAnno.Idx {
    const region = self.parse_ir.tokenizedRegionToRegion(func.region);

    // Canonicalize argument types
    const scratch_top = self.env.store.scratchTypeAnnoTop();
    defer self.env.store.clearScratchTypeAnnosFrom(scratch_top);
    for (self.parse_ir.store.typeAnnoSlice(func.args)) |arg_idx| {
        const arg_anno_idx = try self.canonicalizeTypeAnnoHelp(arg_idx, type_anno_ctx);
        try self.env.store.addScratchTypeAnno(arg_anno_idx);
    }

    const args_span = try self.env.store.typeAnnoSpanFrom(scratch_top);

    const args_anno_idxs = self.env.store.sliceTypeAnnos(args_span);
    const args_vars: []TypeVar = @ptrCast(@alignCast(args_anno_idxs));

    // Canonicalize return type
    const ret_anno_idx = try self.canonicalizeTypeAnnoHelp(func.ret, type_anno_ctx);
    const ret_var = ModuleEnv.varFrom(ret_anno_idx);

    const content = blk: {
        if (type_anno_ctx.isTypeDeclAndHasUnderscore()) {
            break :blk types.Content{ .err = {} };
        } else {
            if (func.effectful) {
                break :blk try self.env.types.mkFuncEffectful(args_vars, ret_var);
            } else {
                break :blk try self.env.types.mkFuncPure(args_vars, ret_var);
            }
        }
    };

    return try self.env.addTypeAnnoAndTypeVar(.{ .@"fn" = .{
        .args = args_span,
        .ret = ret_anno_idx,
        .effectful = func.effectful,
    } }, content, region);
}

////////////////////////////////////////////////////////////////////////////////

fn canonicalizeTypeHeader(self: *Self, header_idx: AST.TypeHeader.Idx) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if the node is malformed before calling getTypeHeader
    const node = self.parse_ir.store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    const node_region = self.parse_ir.tokenizedRegionToRegion(node.region);
    if (node.tag == .malformed) {
        // Create a malformed type header with an invalid identifier
        return try self.env.addTypeHeaderAndTypeVar(.{
            .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Invalid identifier
            .args = .{ .span = .{ .start = 0, .len = 0 } },
        }, Content{ .flex_var = null }, node_region);
    }

    const ast_header = self.parse_ir.store.getTypeHeader(header_idx) catch unreachable; // Malformed handled above
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
                const param_name = self.parse_ir.env.getIdent(param_ident);
                if (param_name.len > 0 and param_name[0] == '_') {
                    try self.env.pushDiagnostic(Diagnostic{ .underscore_in_type_declaration = .{
                        .is_alias = true,
                        .region = param_region,
                    } });
                }

                const param_anno = try self.env.addTypeAnnoAndTypeVar(.{ .ty_var = .{
                    .name = param_ident,
                } }, Content{ .rigid_var = param_ident }, param_region);
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
                try self.env.pushDiagnostic(Diagnostic{ .malformed_type_annotation = .{
                    .region = node_region,
                } });
                const canonicalized = try self.canonicalizeTypeAnno(arg_idx, .type_decl_anno);
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

// expr statements //

// A canonicalized statement
const CanonicalizedStatement = struct {
    idx: Statement.Idx,
    free_vars: ?[]Pattern.Idx,
};

/// A statement type annotation
pub const StmtTypeAnno = struct {
    anno_idx: Statement.Idx,
    anno: std.meta.FieldType(Statement, .s_type_anno),
};

// The result of canonicalizing a statement
const CanonicalizedStatementResult = union(enum) {
    import_stmt,
    stmt: CanonicalizedStatement,
};

/// Canonicalize a statement in the canonical IR.
///
/// This always succeed, but the Statement.Idx returned may be null only if the
/// statement was an imported.
pub fn canonicalizeStatement(
    self: *Self,
    ast_stmt_idx: AST.Statement.Idx,
    last_type_anno: *?StmtTypeAnno,
) std.mem.Allocator.Error!CanonicalizedStatementResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // In many of these branches, we defer setting `last_anno_type` to ensure
    // that in the case of early returns, the value is reset properly.
    //
    // We can't have the `defer` outide the switch branches because not all
    // branches should reset `last_type_anno`

    const ast_stmt = self.parse_ir.store.getStatement(ast_stmt_idx);
    switch (ast_stmt) {
        .decl => |d| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);

            // Check if this is a var reassignment
            const ast_pattern = self.parse_ir.store.getPattern(d.pattern);
            switch (ast_pattern) {
                .ident => |pattern_ident| {
                    const ident_region = self.parse_ir.tokenizedRegionToRegion(pattern_ident.region);
                    const ident_tok = pattern_ident.ident_tok;

                    if (self.parse_ir.tokens.resolveIdentifier(ident_tok)) |ident_idx| {
                        // Check if this identifier exists and is a var
                        switch (self.scopeLookup(.ident, ident_idx)) {
                            .found => |existing_pattern_idx| {
                                // Check if this is a var reassignment across function boundaries
                                if (self.isVarReassignmentAcrossFunctionBoundary(existing_pattern_idx)) {
                                    // Generate error for var reassignment across function boundary
                                    const malformed_idx = try self.env.pushMalformed(Expr.Idx, Diagnostic{ .var_across_function_boundary = .{
                                        .region = ident_region,
                                    } });

                                    // Create a reassign statement with the error expression
                                    const reassign_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_reassign = .{
                                        .pattern_idx = existing_pattern_idx,
                                        .expr = malformed_idx,
                                    } }, ModuleEnv.varFrom(malformed_idx), ident_region);

                                    return .{
                                        .stmt = CanonicalizedStatement{ .idx = reassign_idx, .free_vars = null },
                                    };
                                }

                                // Check if this was declared as a var
                                if (self.isVarPattern(existing_pattern_idx)) {
                                    // This is a var reassignment - canonicalize the expression and create reassign statement
                                    const expr = try self.canonicalizeExprOrMalformed(d.body);

                                    // Create reassign statement
                                    const reassign_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_reassign = .{
                                        .pattern_idx = existing_pattern_idx,
                                        .expr = expr.idx,
                                    } }, ModuleEnv.varFrom(expr.idx), ident_region);

                                    return .{
                                        .stmt = CanonicalizedStatement{ .idx = reassign_idx, .free_vars = expr.free_vars },
                                    };
                                }
                            },
                            .not_found => {
                                // Not found in scope, fall through to regular declaration
                            },
                        }
                    }
                },
                else => {},
            }

            // check against last anno

            // Regular declaration - canonicalize as usual
            const pattern_idx = try self.canonicalizePattern(d.pattern) orelse blk: {
                const pattern = self.parse_ir.store.getPattern(d.pattern);
                break :blk try self.env.pushMalformed(Pattern.Idx, Diagnostic{ .expr_not_canonicalized = .{
                    .region = self.parse_ir.tokenizedRegionToRegion(pattern.to_tokenized_region()),
                } });
            };

            const expr = try self.canonicalizeExprOrMalformed(d.body);

            // Check if this declaration matches the last type annotation
            var annotation_idx: ?Annotation.Idx = null;
            if (last_type_anno.*) |anno_info| {
                if (ast_pattern == .ident) {
                    const pattern_ident = ast_pattern.ident;
                    if (self.parse_ir.tokens.resolveIdentifier(pattern_ident.ident_tok)) |decl_ident| {
                        if (anno_info.anno.name.idx == decl_ident.idx) {
                            // This declaration matches the type annotation
                            const pattern_region = self.parse_ir.tokenizedRegionToRegion(ast_pattern.ident.region);
                            annotation_idx = try self.createAnnotationFromTypeAnno(anno_info.anno.anno, pattern_region);

                            // Clear the annotation since we've used it
                            last_type_anno.* = null;
                        }
                    }
                }
            }

            // Create a declaration statement
            const stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_decl = .{
                .pattern = pattern_idx,
                .expr = expr.idx,
                .anno = annotation_idx,
            } }, ModuleEnv.varFrom(expr.idx), region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .@"var" => |v| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(v.region);

            // Var declaration - handle specially with function boundary tracking
            const var_name = self.parse_ir.tokens.resolveIdentifier(v.name) orelse {
                const feature = try self.env.insertString("resolve var name");
                return .{ .stmt = CanonicalizedStatement{
                    .idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                        .feature = feature,
                        .region = region,
                    } }),
                    .free_vars = null,
                } };
            };

            // Canonicalize the initial value
            const expr = try self.canonicalizeExprOrMalformed(v.body);

            // Create pattern for the var
            const pattern_idx = try self.env.addPatternAndTypeVarRedirect(
                Pattern{ .assign = .{ .ident = var_name } },
                ModuleEnv.varFrom(expr.idx),
                region,
            );

            // Introduce the var with function boundary tracking
            _ = try self.scopeIntroduceVar(var_name, pattern_idx, region, true, Pattern.Idx);

            // Create var statement
            const stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_var = .{
                .pattern_idx = pattern_idx,
                .expr = expr.idx,
            } }, ModuleEnv.varFrom(expr.idx), region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .expr => |e| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Expression statement
            const expr = try self.canonicalizeExprOrMalformed(e.expr);

            // Create expression statement
            const stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_expr = .{
                .expr = expr.idx,
            } }, ModuleEnv.varFrom(expr.idx), region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .crash => |c| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
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
                                break :blk try self.env.insertString(part_text);
                            }
                        }
                        // Fall back to default if we can't extract
                        break :blk try self.env.insertString("crash");
                    },
                    else => {
                        // For non-string expressions, create a malformed expression
                        const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .crash_expects_string = .{
                            .region = region,
                        } });
                        return .{
                            .stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = null },
                        };
                    },
                }
            };

            // Create crash statement
            const stmt_idx = try self.env.addStatementAndTypeVar(Statement{ .s_crash = .{
                .msg = msg_literal,
            } }, .err, region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = null },
            };
        },
        .dbg => |d| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(d.region);

            // Canonicalize the debug expression
            const expr = try self.canonicalizeExprOrMalformed(d.expr);

            // Create dbg statement

            const stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_dbg = .{
                .expr = expr.idx,
            } }, ModuleEnv.varFrom(expr.idx), region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .expect => |e| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(e.region);

            // Canonicalize the expect expression
            const expr = try self.canonicalizeExprOrMalformed(e.body);

            // Create expect statement
            const stmt_idx = try self.env.addStatementAndTypeVar(Statement{ .s_expect = .{
                .body = expr.idx,
            } }, Content{ .structure = .empty_record }, region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .@"return" => |r| {
            defer last_type_anno.* = null; // See above comment for why this is necessary
            const region = self.parse_ir.tokenizedRegionToRegion(r.region);

            // Canonicalize the return expression
            const expr = try self.canonicalizeExprOrMalformed(r.expr);

            // Create return statement
            const stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{ .s_return = .{
                .expr = expr.idx,
            } }, ModuleEnv.varFrom(expr.idx), region);

            return .{
                .stmt = CanonicalizedStatement{ .idx = stmt_idx, .free_vars = expr.free_vars },
            };
        },
        .type_decl => |s| {
            defer last_type_anno.* = null; // See above comment for why this is necessary

            // TODO type declarations in statement context
            const feature = try self.env.insertString("type_decl in statement context");
            const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = self.parse_ir.tokenizedRegionToRegion(s.region),
            } });
            return .{
                .stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = null },
            };
        },
        .type_anno => |ta| {
            // Note that we do _not_ defer resetting last_type_anno in this branch

            // Type annotation statement
            const region = self.parse_ir.tokenizedRegionToRegion(ta.region);

            // Resolve the identifier name
            const name_ident = self.parse_ir.tokens.resolveIdentifier(ta.name) orelse {
                const feature = try self.env.insertString("type annotation identifier resolution");
                const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                    .feature = feature,
                    .region = region,
                } });
                return .{
                    .stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = null },
                };
            };

            // Introduce type variables into scope
            const type_vars_top: u32 = @intCast(self.scratch_idents.top());
            // Extract type variables from the AST annotation
            try self.extractTypeVarIdentsFromASTAnno(ta.anno, type_vars_top);
            // Now canonicalize the annotation with type variables in scope
            const type_anno_idx = try self.canonicalizeTypeAnno(ta.anno, .inline_anno);
            // Canonicalize where clauses if present
            const where_clauses = if (ta.where) |where_coll| blk: {
                const where_slice = self.parse_ir.store.whereClauseSlice(.{ .span = self.parse_ir.store.getCollection(where_coll).span });
                const where_start = self.env.store.scratchWhereClauseTop();

                // Enter a new scope for where clause
                try self.scopeEnter(self.env.gpa, false);
                defer self.scopeExit(self.env.gpa) catch {}; // See above comment for why this is necessary

                for (where_slice) |where_idx| {
                    const canonicalized_where = try self.canonicalizeWhereClause(where_idx, .inline_anno);
                    try self.env.store.addScratchWhereClause(canonicalized_where);
                }
                break :blk try self.env.store.whereClauseSpanFrom(where_start);
            } else null;

            // Create a type annotation statement
            const type_anno_stmt: std.meta.FieldType(Statement, .s_type_anno) = .{
                .name = name_ident,
                .anno = type_anno_idx,
                .where = where_clauses,
            };
            const type_anno_stmt_idx = try self.env.addStatementAndTypeVarRedirect(Statement{
                .s_type_anno = type_anno_stmt,
            }, ModuleEnv.varFrom(type_anno_idx), region);

            last_type_anno.* = StmtTypeAnno{
                .anno_idx = type_anno_stmt_idx,
                .anno = type_anno_stmt,
            };

            return .{
                .stmt = CanonicalizedStatement{ .idx = type_anno_stmt_idx, .free_vars = null },
            };
        },
        .import => |import_stmt| {
            defer last_type_anno.* = null; // See above comment for why this is necessary

            // After we process import statements, there's no need to include
            // then in the canonicalize IR
            _ = try self.canonicalizeImportStatement(import_stmt);
            return .import_stmt;
        },
        else => {
            defer last_type_anno.* = null; // See above comment for why this is necessary

            // Other statement types not yet implemented
            const feature = try self.env.insertString("statement type in block");
            const malformed_idx = try self.env.pushMalformed(Statement.Idx, Diagnostic{ .not_implemented = .{
                .feature = feature,
                .region = Region.zero(),
            } });
            return .{
                .stmt = CanonicalizedStatement{ .idx = malformed_idx, .free_vars = null },
            };
        },
    }
}

// scope //

/// Enter a new scope level
pub fn scopeEnter(self: *Self, gpa: std.mem.Allocator, is_function_boundary: bool) std.mem.Allocator.Error!void {
    const scope = Scope.init(is_function_boundary);
    try self.scopes.append(gpa, scope);
}

/// Exit the current scope level
pub fn scopeExit(self: *Self, gpa: std.mem.Allocator) Scope.Error!void {
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
pub fn addNonFiniteFloat(self: *Self, value: f64, region: base.Region) !Expr.Idx {
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
    const expr_idx = try self.env.addExprAndTypeVar(
        CIR.Expr{
            .e_frac_f64 = .{
                .value = value,
            },
        },
        Content{ .structure = .{ .num = .{ .frac_unbound = frac_requirements } } },
        region,
    );

    return expr_idx;
}

/// Check if an identifier is in scope
fn scopeContains(
    self: *Self,
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
            if (name.idx == entry.key_ptr.idx) {
                return entry.value_ptr.*;
            }
        }
    }
    return null;
}

/// Look up an identifier in the scope
pub fn scopeLookup(
    self: *Self,
    comptime item_kind: Scope.ItemKind,
    name: base.Ident.Idx,
) Scope.LookupResult {
    if (self.scopeContains(item_kind, name)) |pattern| {
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

        switch (scope.lookupTypeVar(name_ident)) {
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
    const result = try current_scope.introduceTypeVar(gpa, name, type_var_anno, null);

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

fn introduceTypeParametersFromHeader(self: *Self, header_idx: CIR.TypeHeader.Idx) std.mem.Allocator.Error!void {
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
        .ty, .underscore, .tag_union, .malformed => {
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
        .ty, .underscore, .malformed => {
            // These don't contain type variables
            return null;
        },
    }
}

/// Introduce a new identifier to the current scope level
pub fn scopeIntroduceInternal(
    self: *Self,
    gpa: std.mem.Allocator,
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
    if (self.scopeContains(item_kind, ident_idx)) |existing_pattern| {
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
                    if (ident_idx.idx == entry.key_ptr.idx) {
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
        if (ident_idx.idx == entry.key_ptr.idx) {
            // Duplicate in same scope - still introduce but return shadowing warning
            try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
            return Scope.IntroduceResult{ .shadowing_warning = entry.value_ptr.* };
        }
    }

    // No conflicts, introduce successfully
    try self.scopes.items[self.scopes.items.len - 1].put(gpa, item_kind, ident_idx, pattern_idx);
    return Scope.IntroduceResult{ .success = {} };
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
    // Define the type for unused variables
    const UnusedVar = struct { ident: base.Ident.Idx, region: Region };

    // Collect all unused variables first so we can sort them
    var unused_vars = std.ArrayList(UnusedVar).init(self.env.gpa);
    defer unused_vars.deinit();

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

        // Collect unused variable for sorting
        try unused_vars.append(.{
            .ident = ident_idx,
            .region = region,
        });
    }

    // Sort unused variables by region (earlier in file first)
    std.mem.sort(UnusedVar, unused_vars.items, {}, struct {
        fn lessThan(_: void, a: UnusedVar, b: UnusedVar) bool {
            // Compare by start offset (position in file)
            return a.region.start.offset < b.region.start.offset;
        }
    }.lessThan);

    // Report unused variables in sorted order
    for (unused_vars.items) |unused| {
        try self.env.pushDiagnostic(Diagnostic{ .unused_variable = .{
            .ident = unused.ident,
            .region = unused.region,
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
            switch (scope.lookupTypeDecl(name_ident)) {
                .found => |type_decl_idx| {
                    shadowed_in_parent = type_decl_idx;
                    break;
                },
                .not_found => continue,
            }
        }
    }

    const result = try current_scope.introduceTypeDecl(gpa, name_ident, type_decl_stmt, null);

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
    try current_scope.updateTypeDecl(gpa, name_ident, new_type_decl_stmt);
}

fn scopeLookupTypeDecl(self: *Self, ident_idx: Ident.Idx) ?Statement.Idx {
    // Search from innermost to outermost scope
    var i = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        switch (scope.lookupTypeDecl(ident_idx)) {
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

        switch (scope.lookupModuleAlias(alias_name)) {
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
    const result = try current_scope.introduceModuleAlias(gpa, alias_name, module_name, null);

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

        switch (scope.lookupExposedItem(item_name)) {
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
    const result = try current_scope.introduceExposedItem(gpa, item_name, item_info, null);

    switch (result) {
        .success => {},
        .shadowing_warning => |shadowed_info| {
            // Create diagnostic for exposed item shadowing
            const item_text = self.env.getIdent(item_name);
            const shadowed_module_text = self.env.getIdent(shadowed_info.module_name);
            const current_module_text = self.env.getIdent(item_info.module_name);

            // For now, just add a simple diagnostic message
            const message = try std.fmt.allocPrint(gpa, "Exposed item '{s}' from module '{s}' shadows item from module '{s}'", .{ item_text, current_module_text, shadowed_module_text });
            const message_str = try self.env.insertString(message);
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
            const item_text = self.env.getIdent(item_name);
            const existing_module_text = self.env.getIdent(existing_info.module_name);
            const new_module_text = self.env.getIdent(item_info.module_name);

            const message = try std.fmt.allocPrint(gpa, "Exposed item '{s}' already imported from module '{s}', cannot import again from module '{s}'", .{ item_text, existing_module_text, new_module_text });
            const message_str = try self.env.insertString(message);
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
    const module_text = self.env.getIdent(module_name_ident);

    // Find the last dot and extract the part after it
    if (std.mem.lastIndexOf(u8, module_text, ".")) |last_dot_idx| {
        const extracted_name = module_text[last_dot_idx + 1 ..];
        return try self.env.insertIdent(base.Ident.for_text(extracted_name));
    } else {
        // No dot found, return the original name
        return module_name_ident;
    }
}

/// Canonicalize a where clause from AST to CIR
fn canonicalizeWhereClause(self: *Self, ast_where_idx: AST.WhereClause.Idx, type_anno_ctx: TypeAnnoCtx.TypeAnnoCtxType) std.mem.Allocator.Error!WhereClause.Idx {
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
            const var_ident = try self.env.insertIdent(Ident.for_text(var_name));
            const method_ident = try self.env.insertIdent(Ident.for_text(method_name_clean));

            // Canonicalize argument types
            const args_slice = self.parse_ir.store.typeAnnoSlice(.{ .span = self.parse_ir.store.getCollection(mm.args).span });
            const args_start = self.env.store.scratchTypeAnnoTop();
            for (args_slice) |arg_idx| {
                const canonicalized_arg = try self.canonicalizeTypeAnno(arg_idx, type_anno_ctx);
                try self.env.store.addScratchTypeAnno(canonicalized_arg);
            }
            const args_span = try self.env.store.typeAnnoSpanFrom(args_start);

            // Canonicalize return type
            const ret_anno = try self.canonicalizeTypeAnno(mm.ret_anno, type_anno_ctx);

            // Create external declaration for where clause method constraint
            // This represents the requirement that type variable must come from a module
            // that provides the specified method
            const var_name_text = self.env.getIdent(var_ident);

            // Create qualified name: "module(a).method"
            const qualified_text = try std.fmt.allocPrint(self.env.gpa, "module({s}).{s}", .{ var_name_text, method_name_clean });
            defer self.env.gpa.free(qualified_text);
            const qualified_name = try self.env.insertIdent(Ident.for_text(qualified_text));

            // Create module name: "module(a)"
            const module_text = try std.fmt.allocPrint(self.env.gpa, "module({s})", .{var_name_text});
            defer self.env.gpa.free(module_text);
            const module_name = try self.env.insertIdent(Ident.for_text(module_text));

            const external_type_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
            const external_decl = try self.createExternalDeclaration(qualified_name, module_name, method_ident, .value, external_type_var, region);

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
            const var_ident = try self.env.insertIdent(Ident.for_text(var_name));
            const alias_ident = try self.env.insertIdent(Ident.for_text(alias_name_clean));

            // Create external declaration for where clause alias constraint
            // This represents the requirement that type variable must come from a module
            // that provides the specified type alias
            const var_name_text = self.env.getIdent(var_ident);

            // Create qualified name: "module(a).Alias"
            const qualified_text = try std.fmt.allocPrint(self.env.gpa, "module({s}).{s}", .{ var_name_text, alias_name_clean });
            defer self.env.gpa.free(qualified_text);
            const qualified_name = try self.env.insertIdent(Ident.for_text(qualified_text));

            // Create module name: "module(a)"
            const module_text = try std.fmt.allocPrint(self.env.gpa, "module({s})", .{var_name_text});
            defer self.env.gpa.free(module_text);
            const module_name = try self.env.insertIdent(Ident.for_text(module_text));

            const external_type_var = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .{ .flex_var = null }, region, TypeVar);
            const external_decl = try self.createExternalDeclaration(qualified_name, module_name, alias_ident, .type, external_type_var, region);

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
            } }, .err);
            return try self.env.addWhereClauseAndTypeVar(WhereClause{ .malformed = .{
                .diagnostic = diagnostic,
            } }, .{ .flex_var = null }, region);
        },
    }
}

/// Handle module-qualified types like Json.Decoder
/// Create an annotation from a type annotation
fn createAnnotationFromTypeAnno(self: *Self, type_anno_idx: TypeAnno.Idx, region: Region) std.mem.Allocator.Error!?Annotation.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create the annotation structure
    // TODO: Remove signature field from Annotation
    const annotation = CIR.Annotation{
        .type_anno = type_anno_idx,
        .signature = try self.env.addTypeSlotAndTypeVar(@enumFromInt(0), .err, region, TypeVar),
    };

    // Add to NodeStore and return the index
    const annotation_idx = try self.env.addAnnotationAndTypeVarRedirect(annotation, ModuleEnv.varFrom(type_anno_idx), region);

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
    const module_text = self.env.getIdent(module_name);

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

    // Look up the target node index in the module's exposed_items
    // Need to convert identifier from current module to target module
    const field_text = self.env.getIdent(field_name);
    const target_node_idx = if (self.module_envs) |envs_map| blk: {
        if (envs_map.get(module_text)) |module_env| {
            if (module_env.common.findIdent(field_text)) |target_ident| {
                break :blk module_env.getExposedNodeIndexById(target_ident) orelse 0;
            } else {
                break :blk 0;
            }
        } else {
            break :blk 0;
        }
    } else 0;

    // Create the e_lookup_external expression with Import.Idx
    const expr_idx = try self.env.addExprAndTypeVar(CIR.Expr{ .e_lookup_external = .{
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

    const dot_access_expr = CIR.Expr{
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

    if (try self.canonicalizeExpr(field_access.left)) |can_expr| {
        return can_expr.idx;
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
            try self.env.store.addScratchExpr(canonicalized.get_idx());
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
    return try self.env.insertIdent(base.Ident.for_text("unknown"));
}

/// Check if this module has a main! function suitable for default_app
const MainFunctionResult = union(enum) {
    valid: struct { def_idx: CIR.Def.Idx, region: Region },
    wrong_arity: struct { arity: u32, region: Region },
    not_found,
};

fn findMainFunction(self: *Self) MainFunctionResult {
    const file = self.parse_ir.store.getFile();

    // Look through all statements for a definition of main!
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .decl) {
            const decl = stmt.decl;
            // Check if this is a definition with name "main!"
            const pattern = self.parse_ir.store.getPattern(decl.pattern);
            if (pattern == .ident) {
                const ident_token = pattern.ident.ident_tok;
                const ident_idx = self.parse_ir.tokens.resolveIdentifier(ident_token) orelse continue;
                const ident_text = self.env.getIdent(ident_idx);

                if (std.mem.eql(u8, ident_text, "main!")) {
                    // Found main! - now check if it's a lambda with exactly 1 parameter
                    const expr = self.parse_ir.store.getExpr(decl.body);
                    if (expr == .lambda) {
                        const lambda = expr.lambda;
                        const params = self.parse_ir.store.patternSlice(lambda.args);

                        const region = self.parse_ir.tokenizedRegionToRegion(decl.region);
                        if (params.len == 1) {
                            // Valid main! function found
                            return .{ .valid = .{ .def_idx = @enumFromInt(0), .region = region } }; // TODO: get actual CIR def idx
                        } else {
                            // main! found but with wrong arity
                            return .{ .wrong_arity = .{ .arity = @intCast(params.len), .region = region } };
                        }
                    }
                }
            }
        }
    }

    return .not_found;
}

/// Check if there's a type declaration matching the module name
fn hasMatchingType(self: *Self) bool {
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;

    // Look through all statements for a type declaration matching the module name
    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            // Get the type name from the header
            const header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
            const type_name_ident = self.parse_ir.tokens.resolveIdentifier(header.name) orelse continue;
            const type_name_text = self.env.getIdent(type_name_ident);

            if (std.mem.eql(u8, type_name_text, module_name_text)) {
                return true;
            }
        }
    }

    return false;
}

/// Check if any type declarations exist in the file
fn hasAnyTypeDeclarations(self: *Self) bool {
    const file = self.parse_ir.store.getFile();

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .type_decl) {
            return true;
        }
    }

    return false;
}

/// Check if there's a type with a case-insensitive match to the module name
fn hasCaseInsensitiveMatch(self: *Self) bool {
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;

    for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.parse_ir.store.getStatement(stmt_id);
        if (stmt == .type_decl) {
            const type_decl = stmt.type_decl;
            const header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
            const type_name_ident = self.parse_ir.tokens.resolveIdentifier(header.name) orelse continue;
            const type_name_text = self.env.getIdent(type_name_ident);

            // Check for case-insensitive match (but not exact match, since that's checked elsewhere)
            if (!std.mem.eql(u8, type_name_text, module_name_text) and
                std.ascii.eqlIgnoreCase(type_name_text, module_name_text))
            {
                return true;
            }
        }
    }

    return false;
}

/// Report smart error when neither type module nor default-app is valid (checking mode)
fn reportTypeModuleOrDefaultAppError(self: *Self) std.mem.Allocator.Error!void {
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;
    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);

    // Check for case mismatch
    if (self.hasCaseInsensitiveMatch()) {
        // Find the mismatched type name
        for (self.parse_ir.store.statementSlice(file.statements)) |stmt_id| {
            const stmt = self.parse_ir.store.getStatement(stmt_id);
            if (stmt == .type_decl) {
                const type_decl = stmt.type_decl;
                const header = self.parse_ir.store.getTypeHeader(type_decl.header) catch continue;
                const type_name_ident = self.parse_ir.tokens.resolveIdentifier(header.name) orelse continue;
                const type_name_text = self.env.getIdent(type_name_ident);

                if (std.ascii.eqlIgnoreCase(type_name_text, module_name_text)) {
                    try self.env.pushDiagnostic(.{
                        .type_name_case_mismatch = .{
                            .module_name = module_name_ident,
                            .type_name = type_name_ident,
                            .region = file_region,
                        },
                    });
                    return;
                }
            }
        }
    }

    // Use heuristic: if there are types declared, assume type module, else assume default-app
    if (self.hasAnyTypeDeclarations()) {
        // Assume user wanted type module
        try self.env.pushDiagnostic(.{
            .type_module_missing_matching_type = .{
                .module_name = module_name_ident,
                .region = file_region,
            },
        });
    } else {
        // Assume user wanted default-app
        try self.env.pushDiagnostic(.{
            .default_app_missing_main = .{
                .module_name = module_name_ident,
                .region = file_region,
            },
        });
    }
}

/// Report error when trying to execute a plain type module
fn reportExecutionRequiresAppOrDefaultApp(self: *Self) std.mem.Allocator.Error!void {
    const file = self.parse_ir.store.getFile();
    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);

    try self.env.pushDiagnostic(.{
        .execution_requires_app_or_default_app = .{
            .region = file_region,
        },
    });
}

/// Report error when trying to import a default-app module
fn reportCannotImportDefaultApp(self: *Self, main_result: MainFunctionResult) std.mem.Allocator.Error!void {
    const region = switch (main_result) {
        .valid => |v| v.region,
        .wrong_arity => |w| w.region,
        .not_found => unreachable, // Should never be called with not_found
    };

    const module_name_text = self.env.module_name;
    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));

    try self.env.pushDiagnostic(.{
        .cannot_import_default_app = .{
            .module_name = module_name_ident,
            .region = region,
        },
    });
}

fn validateTypeModuleName(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (self.hasMatchingType()) {
        return;
    }

    // No matching type declaration found - report error
    const file = self.parse_ir.store.getFile();
    const module_name_text = self.env.module_name;
    const module_name_ident = try self.env.insertIdent(base.Ident.for_text(module_name_text));
    const file_region = self.parse_ir.tokenizedRegionToRegion(file.region);
    try self.env.pushDiagnostic(Diagnostic{
        .type_module_missing_matching_type = .{
            .module_name = module_name_ident,
            .region = file_region,
        },
    });
}

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
